;;; magnus-background.el --- Bounded low-priority agent work -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Magnus has a few useful, non-interactive model tasks: indexing an archived
;; agent's expertise, writing session retrospectives, and optionally refreshing
;; dashboard fortunes.  They are deliberately less important than the agents a
;; user is steering.  This module gives all of those callers one FIFO queue and
;; owns at most one `magnus-headless' process at a time.
;;
;; Submit work with:
;;
;;   (magnus-background-submit KEY PROVIDER REQUEST CALLBACKS)
;;
;; KEY is any `equal' value and coalesces duplicate queued or running work.
;; CALLBACKS may contain :on-event, :on-error, and :on-complete functions, plus
;; a per-job :timeout in seconds.  Event callbacks receive one canonical event;
;; completion and error callbacks receive one result plist.  Every completion
;; contains bounded :output assembled from visible assistant text.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magnus-headless)

(defcustom magnus-background-queue-limit 32
  "Maximum number of background jobs waiting behind the active job.
The active job does not count toward this limit.  A submission beyond the
limit is rejected with a diagnostic rather than growing memory without bound."
  :type 'integer
  :group 'magnus)

(defcustom magnus-background-output-limit (* 256 1024)
  "Maximum visible assistant characters retained for one background job.
The newest characters are retained because text-only providers commonly put
their final answer at the end of the stream."
  :type 'integer
  :group 'magnus)

(defcustom magnus-background-timeout 90
  "Default maximum seconds for one background job."
  :type 'number
  :group 'magnus)

(cl-defstruct (magnus-background-job
               (:constructor magnus-background--make-job))
  "One queued or active background job."
  id key provider request callbacks timeout output-limit
  process timer output dropped state)

(defvar magnus-background--queue nil
  "FIFO list of waiting `magnus-background-job' objects.")

(defvar magnus-background--active nil
  "The one running `magnus-background-job', or nil.")

(defvar magnus-background--next-id 0
  "Monotonic identity source for background jobs.")

(defvar magnus-background--dispatching nil
  "Non-nil while a completed job's callbacks are running.
Submissions made by a callback join the tail before dispatch resumes.")

(defvar magnus-background--accepting-p nil
  "Non-nil while the shared queue accepts submissions.")

(defun magnus-background--callback (job key argument)
  "Safely invoke JOB callback KEY with ARGUMENT."
  (when-let ((function
              (plist-get (magnus-background-job-callbacks job) key)))
    (condition-case err
        (funcall function argument)
      (error
       (message "Magnus: background callback %s for %S failed: %s"
                key (magnus-background-job-key job)
                (error-message-string err))))))

(defun magnus-background--same-attempt-p (job process)
  "Return non-nil when JOB and PROCESS still own the active attempt."
  (and (eq magnus-background--active job)
       (eq (magnus-background-job-process job) process)
       (eq (magnus-background-job-state job) 'running)))

(defun magnus-background--append-output (job text)
  "Append visible assistant TEXT to JOB within its retained bound."
  (when (and (stringp text) (not (string-empty-p text)))
    (let* ((old (or (magnus-background-job-output job) ""))
           (combined (concat old text))
           (length (length combined))
           (limit (magnus-background-job-output-limit job)))
      (when (> length limit)
        (cl-incf (magnus-background-job-dropped job) (- length limit))
        (setq combined (substring combined (- length limit))))
      (setf (magnus-background-job-output job) combined))))

(defun magnus-background--result (job result &optional failure-kind message)
  "Enrich JOB completion RESULT with bounded output and FAILURE-KIND.
MESSAGE is the human-readable synthetic failure when FAILURE-KIND is non-nil."
  (let ((value (copy-sequence (or result nil))))
    (setq value (plist-put value :background-job-id
                           (magnus-background-job-id job)))
    (setq value (plist-put value :background-key
                           (magnus-background-job-key job)))
    (setq value (plist-put value :output
                           (or (magnus-background-job-output job) "")))
    (setq value (plist-put value :output-truncated-p
                           (> (magnus-background-job-dropped job) 0)))
    (setq value (plist-put value :output-dropped-chars
                           (magnus-background-job-dropped job)))
    (when failure-kind
      (setq value (plist-put value :success-p nil))
      (setq value (plist-put value :background-error failure-kind))
      (setq value (plist-put value :error-message message))
      (when (eq failure-kind 'timeout)
        (setq value (plist-put value :timed-out-p t))))
    value))

(defun magnus-background--failure-message (job result)
  "Return a concise diagnostic for failed JOB RESULT."
  (or (plist-get result :error-message)
      (let ((stderr (string-trim (or (plist-get result :stderr) ""))))
        (unless (string-empty-p stderr) stderr))
      (plist-get result :process-event)
      "provider did not complete successfully"))

(defun magnus-background--cancel-timer (job)
  "Cancel and forget JOB's timeout timer."
  (when-let ((timer (magnus-background-job-timer job)))
    (setf (magnus-background-job-timer job) nil)
    (condition-case err
        (cancel-timer timer)
      (error
       (message "Magnus: could not cancel background timer for %S: %s"
                (magnus-background-job-key job)
                (error-message-string err))))))

(defun magnus-background--finish (job process result
                                      &optional failure-kind message)
  "Finish exact JOB PROCESS with RESULT and dispatch its callbacks.
FAILURE-KIND and MESSAGE describe a queue-generated failure.  Late callbacks
from cancelled, timed-out, or otherwise superseded attempts are ignored."
  (when (magnus-background--same-attempt-p job process)
    (magnus-background--cancel-timer job)
    (setf (magnus-background-job-state job) 'complete)
    (setq magnus-background--active nil)
    (magnus-background--deliver job result failure-kind message)))

(defun magnus-background--deliver (job result &optional failure-kind message)
  "Deliver terminal JOB RESULT, then continue the queue.
FAILURE-KIND and MESSAGE have the same meaning as in
`magnus-background--finish'.  The caller must revoke active ownership first."
  (let* ((value (magnus-background--result
                 job result failure-kind message))
         (failed (not (plist-get value :success-p))))
    (when failed
      (message "Magnus: background job %S failed: %s"
               (magnus-background-job-key job)
               (magnus-background--failure-message job value)))
    ;; Prevent a completion callback from jumping a newly submitted job ahead
    ;; of work which was already queued.
    (let ((magnus-background--dispatching t))
      (when failed
        (magnus-background--callback job :on-error value))
      (magnus-background--callback job :on-complete value)))
  (magnus-background--pump))

(defun magnus-background--on-event (job process event)
  "Consume canonical EVENT from exact JOB PROCESS."
  (when (magnus-background--same-attempt-p job process)
    (magnus-background--append-output job (plist-get event :text))
    (magnus-background--callback job :on-event event)))

(defun magnus-background--on-complete (job process result)
  "Consume headless RESULT from exact JOB PROCESS."
  (magnus-background--finish job process result))

(defun magnus-background--kill-process (job)
  "Force-cancel JOB's owned process without signalling."
  (when-let ((process (magnus-background-job-process job)))
    (condition-case err
        (magnus-headless-cancel process t)
      (error
       (message "Magnus: could not cancel background job %S: %s"
                (magnus-background-job-key job)
                (error-message-string err))))))

(defun magnus-background--timeout (job process)
  "Time out JOB only when PROCESS still owns the active attempt."
  (when (magnus-background--same-attempt-p job process)
    ;; Revoke ownership before signalling the child.  Some process harnesses
    ;; invoke a completion sentinel synchronously from cancellation; that late
    ;; completion must not win over the timeout which caused it.
    (setf (magnus-background-job-timer job) nil
          (magnus-background-job-state job) 'timed-out)
    (setq magnus-background--active nil)
    (magnus-background--kill-process job)
    (magnus-background--deliver
     job nil 'timeout
     (format "timed out after %.1f seconds"
             (magnus-background-job-timeout job)))))

(defun magnus-background--start (job)
  "Start JOB through the provider-neutral headless boundary."
  (setq magnus-background--active job)
  (setf (magnus-background-job-state job) 'starting)
  (let (process)
    (condition-case err
        (progn
          (setq process
                (magnus-headless-start
                 (magnus-background-job-provider job)
                 (magnus-background-job-request job)
                 (list
                  :on-event
                  (lambda (process event)
                    (magnus-background--on-event job process event))
                  :on-complete
                  (lambda (process result)
                    (magnus-background--on-complete job process result)))))
          (setf (magnus-background-job-process job) process
                (magnus-background-job-state job) 'running)
          (setf (magnus-background-job-timer job)
                (run-at-time (magnus-background-job-timeout job) nil
                             #'magnus-background--timeout job process)))
      (error
       ;; `magnus-headless-start' can succeed before timeout allocation fails.
       ;; Preserve that exact process, revoke ownership, and cancel it before
       ;; delivering the launch failure; never replace a live child with a
       ;; synthetic token that shutdown can no longer reach.
       (let ((owned (or process
                        (list 'launch-failure
                              (magnus-background-job-id job)))))
         (setf (magnus-background-job-process job) owned
               (magnus-background-job-state job) 'launch-error)
         (setq magnus-background--active nil)
         (when process
           (magnus-background--kill-process job))
         (magnus-background--deliver
          job nil 'launch-error (error-message-string err)))))))

(defun magnus-background--pump ()
  "Start the oldest queued job when the runner is idle."
  (unless (or magnus-background--active magnus-background--dispatching)
    (when-let ((job (pop magnus-background--queue)))
      (magnus-background--start job))))

(defun magnus-background--find (key)
  "Return active or queued job whose key is `equal' to KEY."
  (or (and magnus-background--active
           (equal key (magnus-background-job-key
                       magnus-background--active))
           magnus-background--active)
      (seq-find (lambda (job)
                  (equal key (magnus-background-job-key job)))
                magnus-background--queue)))

(defun magnus-background-pending-p (key)
  "Return non-nil when KEY identifies active or queued background work."
  (not (null (magnus-background--find key))))

(defun magnus-background-queue-length ()
  "Return the number of waiting background jobs."
  (length magnus-background--queue))

(defun magnus-background-submit (key provider request &optional callbacks)
  "Submit PROVIDER REQUEST under coalescing KEY.
CALLBACKS accepts :on-event, :on-error, :on-complete, and :timeout.  Return the
new job, the existing coalesced job, or nil when the queue is stopped or full."
  (let ((timeout (or (plist-get callbacks :timeout)
                     magnus-background-timeout)))
    (cond
     ((not magnus-background--accepting-p)
      (message "Magnus: background queue is not running; rejected %S" key)
      nil)
     ((not (and (integerp magnus-background-queue-limit)
                (>= magnus-background-queue-limit 0)))
      (message "Magnus: background queue limit is invalid: %S"
               magnus-background-queue-limit)
      nil)
     ((not (and (integerp magnus-background-output-limit)
                (>= magnus-background-output-limit 0)))
      (message "Magnus: background output limit is invalid: %S"
               magnus-background-output-limit)
      nil)
     ((not (and (numberp timeout) (> timeout 0)))
      (message "Magnus: background timeout is invalid: %S" timeout)
      nil)
     ((magnus-background--find key))
     ((and (or magnus-background--active magnus-background--dispatching)
           (>= (length magnus-background--queue)
               magnus-background-queue-limit))
      (message "Magnus: background queue is full; rejected %S" key)
      nil)
     (t
      (let ((job
             (magnus-background--make-job
              :id (cl-incf magnus-background--next-id)
              :key key
              :provider provider
              :request (copy-sequence request)
              :callbacks (copy-sequence callbacks)
              :timeout timeout
              :output-limit magnus-background-output-limit
              :output ""
              :dropped 0
              :state 'queued)))
        (setq magnus-background--queue
              (nconc magnus-background--queue (list job)))
        (magnus-background--pump)
        job)))))

(defun magnus-background-cancel (key)
  "Cancel queued and active background work identified by KEY.
Cancellation is scoped: unrelated jobs retain their order and continue.  The
cancelled caller's callbacks are deliberately suppressed.  Return the number
of jobs removed."
  (let ((removed 0))
    (setq magnus-background--queue
          (cl-delete-if
           (lambda (job)
             (when (equal key (magnus-background-job-key job))
               (cl-incf removed)
               (setf (magnus-background-job-state job) 'cancelled)
               t))
           magnus-background--queue))
    (when (and magnus-background--active
               (equal key (magnus-background-job-key
                           magnus-background--active)))
      (let ((job magnus-background--active))
        (cl-incf removed)
        (magnus-background--cancel-timer job)
        (setf (magnus-background-job-state job) 'cancelled)
        (setq magnus-background--active nil)
        (magnus-background--kill-process job)))
    (magnus-background--pump)
    removed))

(defun magnus-background-shutdown ()
  "Cancel the owned process and timer, clear the queue, and reject new work."
  (setq magnus-background--accepting-p nil)
  (dolist (job magnus-background--queue)
    (setf (magnus-background-job-state job) 'cancelled))
  (setq magnus-background--queue nil)
  (when-let ((job magnus-background--active))
    (magnus-background--cancel-timer job)
    (setf (magnus-background-job-state job) 'cancelled)
    (setq magnus-background--active nil)
    (magnus-background--kill-process job)))

(defun magnus-background-setup ()
  "Initialize an empty shared background queue."
  (magnus-background-shutdown)
  (setq magnus-background--accepting-p t))

(provide 'magnus-background)
;;; magnus-background.el ends here
