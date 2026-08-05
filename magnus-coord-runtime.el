;;; magnus-coord-runtime.el --- Bounded coordination orchestration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Cache the durable coordination snapshot and its reduced state.  Refreshes
;; use the store's metadata-only revision first, so an unchanged project never
;; rereads event contents.  This module returns effects but never delivers or
;; polls them; callers own exact retry and settlement.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magnus-coord-store)
(require 'magnus-coord-state)

(declare-function magnus-coord-state-sequence-anchor-event-ids
                  "magnus-coord-state" (state))

(defgroup magnus-coord-runtime nil
  "Low-overhead orchestration for Magnus coordination state."
  :group 'magnus
  :prefix "magnus-coord-runtime-")

(defcustom magnus-coord-runtime-gc-idle-delay 5
  "Idle seconds before pruning obsolete coordination events."
  :type 'number
  :group 'magnus-coord-runtime)

(cl-defstruct (magnus-coord-runtime-result
               (:constructor magnus-coord-runtime-result--create)
               (:copier nil))
  "Outcome of one coordination refresh."
  project-directory state changed-p new-logs unresolved-reviews
  revision-issues state-issues projection-written-p projection-dirty
  projection-error refresh-error)

(cl-defstruct (magnus-coord-runtime--cache
               (:constructor magnus-coord-runtime--cache-create)
               (:copier nil))
  "Private per-project coordination runtime state."
  project-directory initialized-p delivery-seeded-p revision-token revision-issues
  snapshot state seen-log-ids settled-review-ids
  projection-dirty projection-error refresh-error
  retryable-read-p gc-timer gc-result gc-error gc-deferred-reason)

(defconst magnus-coord-runtime--retryable-issue-codes
  '(changed-entry read-error scan-error)
  "Store issue codes that require another snapshot at the same revision.")

(defun magnus-coord-runtime--retryable-issues-p (revision-issues state)
  "Return non-nil when REVISION-ISSUES or STATE report a transient read."
  (or
   (cl-some
    (lambda (issue)
      (and (magnus-coord-store-issue-p issue)
           (memq (magnus-coord-store-issue-code issue)
                 magnus-coord-runtime--retryable-issue-codes)))
    revision-issues)
   (cl-some
    (lambda (issue)
      (and (magnus-coord-state-issue-p issue)
           (memq (magnus-coord-state-issue-code issue)
                 magnus-coord-runtime--retryable-issue-codes)))
    (magnus-coord-state-issues state))))

(defvar magnus-coord-runtime--projects (make-hash-table :test #'equal)
  "Runtime caches keyed by normalized project directory.")

(defun magnus-coord-runtime--project (project)
  "Return a stable local spelling for PROJECT."
  (unless (and (stringp project) (not (string-empty-p project)))
    (error "Project must be a nonempty directory name"))
  (file-name-as-directory (file-truename (expand-file-name project))))

(defun magnus-coord-runtime--new-cache (project)
  "Create and register a cache for normalized PROJECT."
  (let ((cache
         (magnus-coord-runtime--cache-create
          :project-directory project
          :seen-log-ids (make-hash-table :test #'equal)
          :settled-review-ids (make-hash-table :test #'equal))))
    (puthash project cache magnus-coord-runtime--projects)
    cache))

(defun magnus-coord-runtime--cache (project &optional create)
  "Return PROJECT's cache, creating it when CREATE is non-nil."
  (let* ((key (magnus-coord-runtime--project project))
         (cache (gethash key magnus-coord-runtime--projects)))
    (or cache (and create (magnus-coord-runtime--new-cache key)))))

(defun magnus-coord-runtime--unresolved-reviews (cache)
  "Return CACHE's current review effects that have not been settled."
  (let ((settled (magnus-coord-runtime--cache-settled-review-ids cache))
        (state (magnus-coord-runtime--cache-state cache)))
    (and state
         (cl-remove-if
          (lambda (effect)
            (gethash (magnus-coord-state-review-effect-event-id effect)
                     settled))
          (magnus-coord-state-review-ready state)))))

(defun magnus-coord-runtime--observe-logs
    (cache state startup-p &optional retain-missing)
  "Record STATE's log IDs in CACHE and return newly observed logs.
Return no logs when STARTUP-P is non-nil.  When RETAIN-MISSING is non-nil, a
partial snapshot cannot forget prior IDs and redeliver them on its retry."
  (let ((seen (magnus-coord-runtime--cache-seen-log-ids cache))
        (current (if retain-missing
                     (copy-hash-table
                      (magnus-coord-runtime--cache-seen-log-ids cache))
                   (make-hash-table :test #'equal)))
        new)
    ;; `log-effects' contains the whole just-read snapshot while `logs' is the
    ;; bounded human projection.  Observing the former prevents a burst larger
    ;; than the projection limit from silently losing conversational effects.
    ;; The fallback keeps hand-built third-party/test states compatible.
    (dolist (record (or (magnus-coord-state-log-effects state)
                        (magnus-coord-state-logs state)))
      (let ((id (magnus-coord-state-log-record-event-id record)))
        (unless (or startup-p (gethash id seen)) (push record new))
        (puthash id t current)))
    (setf (magnus-coord-runtime--cache-seen-log-ids cache) current)
    (nreverse new)))

(defun magnus-coord-runtime--trim-settled-reviews (cache state)
  "Bound CACHE's settled IDs to review effects still present in STATE."
  (let ((settled (magnus-coord-runtime--cache-settled-review-ids cache))
        (current (make-hash-table :test #'equal)))
    (dolist (effect (magnus-coord-state-review-ready state))
      (let ((id (magnus-coord-state-review-effect-event-id effect)))
        (when (gethash id settled) (puthash id t current))))
    (setf (magnus-coord-runtime--cache-settled-review-ids cache) current)))

(defun magnus-coord-runtime--result (cache changed new-logs written)
  "Build a refresh result from CACHE and transient outcome values."
  (let ((state (magnus-coord-runtime--cache-state cache)))
    (magnus-coord-runtime-result--create
     :project-directory (magnus-coord-runtime--cache-project-directory cache)
     :state state :changed-p changed :new-logs new-logs
     :unresolved-reviews (magnus-coord-runtime--unresolved-reviews cache)
     :revision-issues (magnus-coord-runtime--cache-revision-issues cache)
     :state-issues (and state (magnus-coord-state-issues state))
     :projection-written-p written
     :projection-dirty (magnus-coord-runtime--cache-projection-dirty cache)
     :projection-error (magnus-coord-runtime--cache-projection-error cache)
     :refresh-error (magnus-coord-runtime--cache-refresh-error cache))))

(defun magnus-coord-runtime--project-state (cache)
  "Try to project CACHE's committed state and return non-nil on success."
  (condition-case error-data
      (progn
        (magnus-coord-state-write-projection
         (magnus-coord-runtime--cache-state cache))
        (setf (magnus-coord-runtime--cache-projection-dirty cache) nil
              (magnus-coord-runtime--cache-projection-error cache) nil)
        (if (magnus-coord-runtime--cache-retryable-read-p cache)
            (setf (magnus-coord-runtime--cache-gc-deferred-reason cache)
                  "snapshot has a transient read issue")
          (condition-case timer-error
              (magnus-coord-runtime-schedule-gc
               (magnus-coord-runtime--cache-project-directory cache))
            (error
             (setf (magnus-coord-runtime--cache-gc-error cache)
                   (error-message-string timer-error)))))
        t)
    (error
     (setf (magnus-coord-runtime--cache-projection-dirty cache) t
           (magnus-coord-runtime--cache-projection-error cache)
           (error-message-string error-data))
     nil)))

;;;###autoload
(defun magnus-coord-runtime-refresh (project)
  "Refresh PROJECT once and return a `magnus-coord-runtime-result'.
An unchanged store revision performs no snapshot or event-content reads."
  (let* ((cache (magnus-coord-runtime--cache project t))
         (key (magnus-coord-runtime--cache-project-directory cache))
         revision token changed new-logs written)
    (condition-case error-data
        (progn
          (setq revision (magnus-coord-store-revision key)
                token (magnus-coord-store-revision-result-token revision)
                changed
                (or (not (magnus-coord-runtime--cache-initialized-p cache))
                    (not (equal token
                                (magnus-coord-runtime--cache-revision-token
                                 cache)))))
          (setf (magnus-coord-runtime--cache-revision-issues cache)
                (magnus-coord-store-revision-result-issues revision)
                (magnus-coord-runtime--cache-refresh-error cache) nil)
          (if changed
              (let* ((startup-p
                      (not
                       (magnus-coord-runtime--cache-delivery-seeded-p cache)))
                     (snapshot (magnus-coord-store-snapshot key))
                     (state (magnus-coord-state-reduce snapshot))
                     (retryable
                      (magnus-coord-runtime--retryable-issues-p
                       (magnus-coord-store-revision-result-issues revision)
                       state)))
                ;; Snapshot and reduction are the transaction boundary.
                (setf (magnus-coord-runtime--cache-snapshot cache) snapshot
                      (magnus-coord-runtime--cache-state cache) state
                      ;; A partial snapshot is useful immediately, but keeping
                      ;; the old token forces the next poll to retry the same
                      ;; metadata revision instead of waiting for another
                      ;; directory change that may never come.
                      (magnus-coord-runtime--cache-revision-token cache)
                      (if retryable
                          (magnus-coord-runtime--cache-revision-token cache)
                        token)
                      (magnus-coord-runtime--cache-initialized-p cache) t
                      (magnus-coord-runtime--cache-retryable-read-p cache)
                      retryable
                      (magnus-coord-runtime--cache-gc-deferred-reason cache)
                      (and retryable "snapshot has a transient read issue"))
                (setq new-logs
                      (magnus-coord-runtime--observe-logs
                       cache state startup-p retryable))
                (unless retryable
                  (setf (magnus-coord-runtime--cache-delivery-seeded-p cache) t)
                  (magnus-coord-runtime--trim-settled-reviews cache state))
                (setq written (magnus-coord-runtime--project-state cache)))
            (when (and (magnus-coord-runtime--cache-state cache)
                       (magnus-coord-runtime--cache-projection-dirty cache))
              (setq written (magnus-coord-runtime--project-state cache)))))
      (error
       ;; Keep the last committed token/snapshot/state so the next refresh
       ;; retries the failed stage.  A dirty cached projection can still heal.
       (setf (magnus-coord-runtime--cache-refresh-error cache)
             (error-message-string error-data))
       (when (and (magnus-coord-runtime--cache-state cache)
                  (magnus-coord-runtime--cache-projection-dirty cache))
         (setq written (magnus-coord-runtime--project-state cache)))
       (setq changed nil)))
    (magnus-coord-runtime--result cache changed new-logs written)))

(defun magnus-coord-runtime-reproject (project)
  "Rewrite PROJECT's cached human projection without rereading its store.
Return non-nil when a running cache had state and projection succeeded.  This
is used when Magnus lifecycle metadata changes the visibility overlay while
the immutable event revision itself remains unchanged."
  (let ((cache (magnus-coord-runtime--cache project)))
    (when (and cache (magnus-coord-runtime--cache-state cache))
      (setf (magnus-coord-runtime--cache-projection-dirty cache) t)
      (magnus-coord-runtime--project-state cache))))

;;;###autoload
(defun magnus-coord-runtime-start (project)
  "Start or refresh the bounded coordination runtime for PROJECT."
  (magnus-coord-runtime-refresh project))

(defun magnus-coord-runtime-current-state (project)
  "Return PROJECT's cached coordination state, without filesystem reads."
  (let ((cache (magnus-coord-runtime--cache project)))
    (and cache (magnus-coord-runtime--cache-state cache))))

(defun magnus-coord-runtime-settle-review (project event-id)
  "Mark review-ready EVENT-ID settled for the running PROJECT.
Return non-nil only for the first settlement."
  (let* ((cache (or (magnus-coord-runtime--cache project)
                    (error "Coordination runtime is not running")))
         (effect
          (cl-find event-id
                   (magnus-coord-state-review-ready
                    (or (magnus-coord-runtime--cache-state cache)
                        (error "Coordination state is not available")))
                   :key #'magnus-coord-state-review-effect-event-id
                   :test #'equal))
         (settled (magnus-coord-runtime--cache-settled-review-ids cache)))
    (unless effect (error "Unknown review-ready event: %s" event-id))
    (unless (gethash event-id settled)
      (puthash event-id t settled)
      (unless (magnus-coord-runtime--cache-projection-dirty cache)
        (magnus-coord-runtime-schedule-gc project))
      t)))

(defun magnus-coord-runtime-schedule-gc (project)
  "Debounce one idle garbage-collection pass for running PROJECT."
  (let ((cache (magnus-coord-runtime--cache project)))
    (when (and cache (magnus-coord-runtime--cache-state cache)
               (not (magnus-coord-runtime--cache-projection-dirty cache))
               (not (magnus-coord-runtime--cache-retryable-read-p cache)))
      (when (magnus-coord-runtime--cache-gc-timer cache)
        (cancel-timer (magnus-coord-runtime--cache-gc-timer cache))
        (setf (magnus-coord-runtime--cache-gc-timer cache) nil))
      (setf (magnus-coord-runtime--cache-gc-timer cache)
            (run-with-idle-timer
             magnus-coord-runtime-gc-idle-delay nil
             #'magnus-coord-runtime-run-gc
             (magnus-coord-runtime--cache-project-directory cache))))))

(defun magnus-coord-runtime-run-gc (project)
  "Prune obsolete evidence from PROJECT's last committed snapshot.
Return the store prune result, or nil when projection durability gates GC."
  (let ((cache (magnus-coord-runtime--cache project)))
    (when cache
      (when (magnus-coord-runtime--cache-gc-timer cache)
        (cancel-timer (magnus-coord-runtime--cache-gc-timer cache))
        (setf (magnus-coord-runtime--cache-gc-timer cache) nil))
      (if (or (not (magnus-coord-runtime--cache-state cache))
              (magnus-coord-runtime--cache-projection-dirty cache)
              (magnus-coord-runtime--cache-retryable-read-p cache))
          (progn
            (setf (magnus-coord-runtime--cache-gc-deferred-reason cache)
                  (if (magnus-coord-runtime--cache-retryable-read-p cache)
                      "snapshot has a transient read issue"
                    "projection is not durable"))
            nil)
        (let* ((state (magnus-coord-runtime--cache-state cache))
               (settled
                (magnus-coord-runtime--cache-settled-review-ids cache))
               (anchors
                (and
                 (fboundp 'magnus-coord-state-sequence-anchor-event-ids)
                 (magnus-coord-state-sequence-anchor-event-ids state)))
               (keep
                (cl-remove-if
                 (lambda (id)
                   (and (gethash id settled) (not (member id anchors))))
                 (magnus-coord-state-retained-event-ids state))))
          (condition-case error-data
              (let ((result
                     (magnus-coord-store-prune
                      (magnus-coord-runtime--cache-snapshot cache) keep)))
                (setf (magnus-coord-runtime--cache-gc-result cache) result
                      (magnus-coord-runtime--cache-gc-error cache) nil
                      (magnus-coord-runtime--cache-gc-deferred-reason cache) nil)
                result)
            (error
             (setf (magnus-coord-runtime--cache-gc-error cache)
                   (error-message-string error-data))
             nil)))))))

(defun magnus-coord-runtime-stop (project)
  "Stop and forget PROJECT's runtime cache."
  (let* ((key (magnus-coord-runtime--project project))
         (cache (gethash key magnus-coord-runtime--projects)))
    (when cache
      (when (magnus-coord-runtime--cache-gc-timer cache)
        (cancel-timer (magnus-coord-runtime--cache-gc-timer cache)))
      (remhash key magnus-coord-runtime--projects)
      t)))

(defun magnus-coord-runtime-stop-all ()
  "Stop and forget every coordination runtime cache."
  (maphash
   (lambda (_project cache)
     (when (magnus-coord-runtime--cache-gc-timer cache)
       (cancel-timer (magnus-coord-runtime--cache-gc-timer cache))))
   magnus-coord-runtime--projects)
  (clrhash magnus-coord-runtime--projects))

(defun magnus-coord-runtime-diagnostics (project)
  "Return a read-only diagnostic plist for PROJECT's runtime."
  (let* ((key (magnus-coord-runtime--project project))
         (cache (gethash key magnus-coord-runtime--projects)))
    (if (not cache)
        (list :running nil :project-directory key)
      (let ((state (magnus-coord-runtime--cache-state cache))
            (gc-result (magnus-coord-runtime--cache-gc-result cache)))
        (list
         :running t :project-directory key
         :revision-token (magnus-coord-runtime--cache-revision-token cache)
         :revision-issues (magnus-coord-runtime--cache-revision-issues cache)
         :state-issues (and state (magnus-coord-state-issues state))
         :projection-dirty
         (magnus-coord-runtime--cache-projection-dirty cache)
         :projection-error
         (magnus-coord-runtime--cache-projection-error cache)
         :refresh-error (magnus-coord-runtime--cache-refresh-error cache)
         :retrying-transient-read
         (magnus-coord-runtime--cache-retryable-read-p cache)
         :gc-scheduled (and (magnus-coord-runtime--cache-gc-timer cache) t)
         :gc-deferred
         (magnus-coord-runtime--cache-gc-deferred-reason cache)
         :gc-error (magnus-coord-runtime--cache-gc-error cache)
         :gc-issues
         (and gc-result (magnus-coord-store-prune-result-issues gc-result))
         :seen-log-count
         (hash-table-count (magnus-coord-runtime--cache-seen-log-ids cache))
         :settled-review-count
         (hash-table-count
          (magnus-coord-runtime--cache-settled-review-ids cache)))))))

(provide 'magnus-coord-runtime)
;;; magnus-coord-runtime.el ends here
