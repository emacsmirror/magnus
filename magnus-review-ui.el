;;; magnus-review-ui.el --- Magit-style review reader for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module presents a completed Magnus review as a read-only, Magit-style
;; base..head diff.  Structured findings are attached to the lines they refer
;; to; findings that cannot be attached precisely fall back to their file or
;; to a general findings section.
;;
;; The durable review domain lives in magnus-review.el.  The small adapter
;; section below deliberately isolates that module's object and result-file
;; API from the reader and makes partially migrated persisted reviews usable.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'eieio)
(require 'json)
(require 'magit-section)
(require 'seq)
(require 'subr-x)

(defvar magnus-review-max-evidence-bytes)

;; Review-domain accessors.  Keeping these declarations together makes the
;; boundary between the durable model and this presentation module explicit.
(declare-function magnus-review-id "magnus-review")
(declare-function magnus-review-project-root "magnus-review")
(declare-function magnus-review-author-name "magnus-review")
(declare-function magnus-review-reviewer-name "magnus-review")
(declare-function magnus-review-reviewer-provider "magnus-review")
(declare-function magnus-review-model "magnus-review")
(declare-function magnus-review-effort "magnus-review")
(declare-function magnus-review-task "magnus-review")
(declare-function magnus-review-rounds "magnus-review")
(declare-function magnus-review-latest-round "magnus-review")
(declare-function magnus-review-round-number "magnus-review")
(declare-function magnus-review-round-base-oid "magnus-review")
(declare-function magnus-review-round-head-oid "magnus-review")
(declare-function magnus-review-round-verdict "magnus-review")
(declare-function magnus-review-round-result-path "magnus-review")
(declare-function magnus-review-round-patch-path "magnus-review")
(declare-function magnus-review-round-name-status-path "magnus-review")
(declare-function magnus-review-mark-read "magnus-review")

;;; Domain adapter

(defun magnus-review-ui--map-value (object keys)
  "Return the first value for KEYS found in map-like OBJECT."
  (seq-some
   (lambda (key)
     (cond
      ((hash-table-p object)
       (or (gethash key object)
           (gethash (substring (symbol-name key) 1) object)))
      ((and (listp object) (keywordp (car object)))
       (plist-get object key))
      ((listp object)
       (or (alist-get key object)
           (alist-get (intern (substring (symbol-name key) 1)) object)))))
   keys))

(defun magnus-review-ui--field (object accessors keys)
  "Read a field from OBJECT using ACCESSORS, then map-like KEYS.

Undefined accessors and accessors for a different persisted object shape are
ignored.  This tolerance is intentionally confined to this adapter block."
  (or (seq-some
       (lambda (accessor)
         (when (fboundp accessor)
           (condition-case err
               (funcall accessor object)
             (error
              (message "Magnus: review reader accessor %s failed: %s"
                       accessor (error-message-string err))
              nil))))
       accessors)
      (magnus-review-ui--map-value object keys)))

(defun magnus-review-ui--review-id (review)
  "Return REVIEW's stable identifier."
  (magnus-review-ui--field review
                           '(magnus-review-id)
                           '(:id :review_id)))

(defun magnus-review-ui--project-root (review)
  "Return REVIEW's source repository root."
  (magnus-review-ui--field review
                           '(magnus-review-project-root)
                           '(:project_root :project-root :directory)))

(defun magnus-review-ui--author-name (review)
  "Return REVIEW's author name."
  (magnus-review-ui--field review
                           '(magnus-review-author-name)
                           '(:author_name :author-name :author)))

(defun magnus-review-ui--reviewer-name (review)
  "Return REVIEW's reviewer name."
  (magnus-review-ui--field review
                           '(magnus-review-reviewer-name)
                           '(:reviewer_name :reviewer-name :reviewer)))

(defun magnus-review-ui--reviewer-provider (review)
  "Return REVIEW's reviewer provider."
  (magnus-review-ui--field review
                           '(magnus-review-reviewer-provider)
                           '(:reviewer_provider :reviewer-provider :provider)))

(defun magnus-review-ui--model (review)
  "Return REVIEW's model."
  (magnus-review-ui--field review '(magnus-review-model) '(:model)))

(defun magnus-review-ui--effort (review)
  "Return REVIEW's reasoning effort."
  (magnus-review-ui--field review '(magnus-review-effort) '(:effort)))

(defun magnus-review-ui--task (review)
  "Return REVIEW's task description."
  (magnus-review-ui--field review '(magnus-review-task) '(:task)))

(defun magnus-review-ui--rounds (review)
  "Return REVIEW's rounds in chronological order."
  (let ((rounds (magnus-review-ui--field review
                                         '(magnus-review-rounds)
                                         '(:rounds))))
    (cond ((vectorp rounds) (append rounds nil))
          ((listp rounds) rounds)
          (t nil))))

(defun magnus-review-ui--round-number (round)
  "Return ROUND's ordinal number."
  (magnus-review-ui--field round
                           '(magnus-review-round-number)
                           '(:number :round :round_number)))

(defun magnus-review-ui--round-base (round)
  "Return ROUND's base object ID."
  (magnus-review-ui--field round
                           '(magnus-review-round-base-oid)
                           '(:base_oid :base-oid :base)))

(defun magnus-review-ui--round-head (round)
  "Return ROUND's head object ID."
  (magnus-review-ui--field round
                           '(magnus-review-round-head-oid)
                           '(:head_oid :head-oid :head)))

(defun magnus-review-ui--latest-round (review)
  "Return the latest round in REVIEW."
  (or (when (fboundp 'magnus-review-latest-round)
        (condition-case err
            (magnus-review-latest-round review)
          (error
           (message "Magnus: review reader could not select latest round: %s"
                    (error-message-string err))
           nil)))
      (car (last (magnus-review-ui--rounds review)))))

(defun magnus-review-ui--result-path (review round)
  "Return the canonical structured-result path for REVIEW and ROUND."
  (when (fboundp 'magnus-review-round-result-path)
    ;; The domain helper takes REVIEW and ROUND.  The one-argument fallback
    ;; keeps persisted prototypes readable while the domain is being migrated.
    (or (condition-case err
            (magnus-review-round-result-path review round)
          (wrong-number-of-arguments nil)
          (error
           (message "Magnus: review result path lookup failed: %s"
                    (error-message-string err))
           nil))
        (condition-case err
            (funcall #'magnus-review-round-result-path round)
          (error
           (message "Magnus: legacy review result path lookup failed: %s"
                    (error-message-string err))
           nil)))))

(defun magnus-review-ui--read-result (review round)
  "Read REVIEW ROUND's canonical structured JSON result."
  (when-let ((path (magnus-review-ui--result-path review round)))
    (when (file-readable-p path)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents path)
            (json-parse-buffer :object-type 'plist
                               :array-type 'list
                               :null-object nil
                               :false-object nil))
        (error
         (list :magnus_review_ui_error (error-message-string err)))))))

(defun magnus-review-ui--result-body (result)
  "Return the review payload within RESULT."
  (or (magnus-review-ui--map-value result '(:result :review)) result))

(defun magnus-review-ui--result-field (result keys)
  "Return the first value for KEYS in structured RESULT."
  (magnus-review-ui--map-value (magnus-review-ui--result-body result) keys))

(defun magnus-review-ui--summary (result)
  "Return the summary from structured RESULT."
  (magnus-review-ui--result-field result '(:summary :overview)))

(defun magnus-review-ui--result-error (result)
  "Return a structured-result loading error from RESULT, if present."
  (magnus-review-ui--map-value result '(:magnus_review_ui_error)))

(defun magnus-review-ui--verdict (round result)
  "Return ROUND's verdict, falling back to structured RESULT."
  (or (magnus-review-ui--field round
                               '(magnus-review-round-verdict)
                               '(:verdict))
      (magnus-review-ui--result-field
       result '(:verdict :proposed_verdict :proposed-verdict))))

(defun magnus-review-ui--findings (result)
  "Return the findings list from structured RESULT."
  (let ((findings (magnus-review-ui--result-field
                   result '(:findings :comments :notes))))
    (cond ((vectorp findings) (append findings nil))
          ((listp findings) findings)
          (t nil))))

;;; Customization and faces

(defcustom magnus-review-ui-mark-read-function #'magnus-review-mark-read
  "Function called with REVIEW and ROUND when a round is displayed.

The function should be idempotent.  Set this to nil to manage unread state
outside the reader."
  :type '(choice (const :tag "Do not mark read" nil) function)
  :group 'magnus)

(defcustom magnus-review-ui-action-function nil
  "Function called with REVIEW and ROUND when `?' is pressed.

The review transient should set this to its dispatcher."
  :type '(choice (const :tag "No action dispatcher" nil) function)
  :group 'magnus)

(defface magnus-review-ui-title
  '((t :inherit magit-section-heading :weight bold :height 1.15))
  "Face used for the review title."
  :group 'magnus)

(defface magnus-review-ui-metadata
  '((t :inherit shadow))
  "Face used for review metadata."
  :group 'magnus)

(defface magnus-review-ui-approved
  '((t :inherit success :weight bold))
  "Face used for approving verdicts."
  :group 'magnus)

(defface magnus-review-ui-changes-requested
  '((t :inherit error :weight bold))
  "Face used for changes-requested verdicts."
  :group 'magnus)

(defface magnus-review-ui-comment-verdict
  '((t :inherit warning :weight bold))
  "Face used for non-blocking review verdicts."
  :group 'magnus)

(defface magnus-review-ui-critical
  '((t :inherit error :weight bold :underline t))
  "Face used for critical findings."
  :group 'magnus)

(defface magnus-review-ui-high
  '((t :inherit error :weight bold))
  "Face used for high-severity findings."
  :group 'magnus)

(defface magnus-review-ui-medium
  '((t :inherit warning :weight bold))
  "Face used for medium-severity findings."
  :group 'magnus)

(defface magnus-review-ui-low
  '((t :inherit success :weight bold))
  "Face used for low-severity findings."
  :group 'magnus)

(defface magnus-review-ui-line-number
  '((t :inherit shadow))
  "Face used for old and new line numbers."
  :group 'magnus)

(defface magnus-review-ui-finding-body
  '((t :inherit default))
  "Face used for finding explanations."
  :group 'magnus)

;;; Sections and mode

(defclass magnus-review-ui-root-section (magit-section) ())
(defclass magnus-review-ui-summary-section (magit-section) ())
(defclass magnus-review-ui-location-section (magit-section)
  ((path :initarg :path :initform nil)
   (current-path :initarg :current-path :initform nil)
   (line :initarg :line :initform nil)
   (side :initarg :side :initform 'head)))
(defclass magnus-review-ui-findings-section (magit-section) ())
(defclass magnus-review-ui-file-section
  (magnus-review-ui-location-section) ())
(defclass magnus-review-ui-hunk-section
  (magnus-review-ui-location-section) ())
(defclass magnus-review-ui-finding-section
  (magnus-review-ui-location-section) ())

(defvar-local magnus-review-ui--review nil)
(defvar-local magnus-review-ui--round nil)
(defvar-local magnus-review-ui--result nil)
(defvar-local magnus-review-ui--diff-error nil)
(defvar-local magnus-review-ui--evidence-source nil)
(defvar-local magnus-review-ui--marked-read-rounds nil)

(defvar magnus-review-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "n") #'magit-section-forward)
    (define-key map (kbd "p") #'magit-section-backward)
    (define-key map (kbd "N") #'magnus-review-ui-next-finding)
    (define-key map (kbd "P") #'magnus-review-ui-previous-finding)
    (define-key map (kbd "RET") #'magnus-review-ui-visit-snapshot)
    (define-key map (kbd "e") #'magnus-review-ui-visit-current-file)
    (define-key map (kbd "TAB") #'magit-section-cycle)
    (define-key map (kbd "[") #'magnus-review-ui-previous-round)
    (define-key map (kbd "]") #'magnus-review-ui-next-round)
    (define-key map (kbd "?") #'magnus-review-ui-actions)
    (define-key map (kbd "g") #'magnus-review-ui-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `magnus-review-ui-mode'.")

(define-derived-mode magnus-review-ui-mode magit-section-mode "Magnus-Review"
  "Read a structured Magnus review as a navigable base..head diff.

\\{magnus-review-ui-mode-map}"
  :group 'magnus
  (setq-local revert-buffer-function #'magnus-review-ui--revert)
  (setq-local truncate-lines t))

;;; Diff model and parsing

(cl-defstruct (magnus-review-ui--diff-line
               (:constructor magnus-review-ui--make-diff-line))
  kind text old-line new-line)

(cl-defstruct (magnus-review-ui--hunk
               (:constructor magnus-review-ui--make-hunk))
  header old-start old-count new-start new-count lines)

(cl-defstruct (magnus-review-ui--file
               (:constructor magnus-review-ui--make-file))
  status old-path new-path display-path headers hunks)

(cl-defstruct (magnus-review-ui--finding
               (:constructor magnus-review-ui--make-finding))
  raw id severity kind path line end-line side title evidence recommendation)

(defun magnus-review-ui--string (value &optional fallback)
  "Convert VALUE to a display string, or return FALLBACK."
  (cond ((stringp value) value)
        ((symbolp value) (symbol-name value))
        ((numberp value) (number-to-string value))
        (t fallback)))

(defun magnus-review-ui--short-oid (oid)
  "Return a short display form of OID."
  (let ((text (magnus-review-ui--string oid "????????")))
    (substring text 0 (min 8 (length text)))))

(defun magnus-review-ui--valid-oid-p (oid)
  "Return non-nil when OID is safe to pass as a resolved Git object ID."
  (and (stringp oid)
       (string-match-p
        "\\`\\(?:[[:xdigit:]]\\{40\\}\\|[[:xdigit:]]\\{64\\}\\)\\'"
        oid)))

(defun magnus-review-ui--git (root &rest args)
  "Run Git in ROOT with ARGS and return its output.

Signal an error containing Git's diagnostic when the command fails."
  (unless (and root (file-directory-p root))
    (error "Review project root is unavailable: %s" (or root "<none>")))
  (unless (executable-find "git")
    (error "Cannot render review diff because Git is unavailable"))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)
          (coding-system-for-write 'utf-8-unix)
          (status (apply #'process-file
                         "git" nil (current-buffer) nil
                         "-C" root "-c" "core.quotePath=false" args)))
      (if (zerop status)
          (buffer-string)
        (error "Git %s failed: %s"
               (car args) (string-trim (buffer-string)))))))

(defun magnus-review-ui--normalize-path (path)
  "Return a safe repository-relative form of PATH, or nil."
  (when-let ((text (magnus-review-ui--string path)))
    (setq text (string-remove-prefix "./" text))
    (when (or (string-prefix-p "a/" text)
              (string-prefix-p "b/" text))
      (setq text (substring text 2)))
    (unless (or (string-empty-p text)
                (string= text "/dev/null")
                (file-name-absolute-p text)
                (string-match-p "[\0\n\r]" text)
                (member ".." (split-string text "/" t)))
      text)))

(defun magnus-review-ui--parse-name-status (stream)
  "Parse NUL-delimited Git name-status STREAM into path records."
  (let ((tokens (split-string stream "\0" t)) records)
    (while tokens
      (let* ((status (pop tokens))
             (letter (and (not (string-empty-p status)) (aref status 0))))
        (unless (and letter (string-match-p "\\`[A-Z][0-9]*\\'" status))
          (error "Review evidence has an invalid Git status: %S" status))
        (if (memq letter '(?R ?C))
            (let ((old (pop tokens))
                  (new (pop tokens)))
              (unless (and old new)
                (error "Review evidence truncates a rename/copy record"))
              (let ((old-path (magnus-review-ui--normalize-path old))
                    (new-path (magnus-review-ui--normalize-path new)))
                (unless (and old-path new-path)
                  (error "Review evidence contains an unsafe changed path"))
                (push (list :status status
                            :old-path old-path
                            :new-path new-path)
                      records)))
          (let ((path (pop tokens)))
            (unless path
              (error "Review evidence truncates a changed-path record"))
            (let ((safe-path (magnus-review-ui--normalize-path path)))
              (unless safe-path
                (error "Review evidence contains an unsafe changed path"))
              (push (list :status status
                          :old-path (unless (eq letter ?A) safe-path)
                          :new-path (unless (eq letter ?D) safe-path))
                    records))))))
    (nreverse records)))

(defun magnus-review-ui--name-status-live (root base head)
  "Return live path metadata for BASE..HEAD in ROOT."
  (magnus-review-ui--parse-name-status
   (magnus-review-ui--git
    root "diff" "--no-ext-diff" "--no-color"
    "--find-renames" "--name-status" "-z" base head "--")))

(defun magnus-review-ui--artifact-path (accessor review round)
  "Call evidence path ACCESSOR for REVIEW and ROUND when available."
  (when (fboundp accessor)
    (funcall accessor review round)))

(defun magnus-review-ui--artifact-present-p (path)
  "Return non-nil when PATH exists, including as a broken symlink."
  (and path (or (file-exists-p path) (file-symlink-p path))))

(defun magnus-review-ui--artifact-bytes (path kind)
  "Read exact bytes from regular, non-symlink evidence PATH named KIND."
  (when (or (file-symlink-p path) (not (file-regular-p path)))
    (error "Review %s evidence is unavailable or unsafe: %s" kind path))
  (let* ((attributes (file-attributes path 'string))
         (size (file-attribute-size attributes))
         (limit (if (and (boundp 'magnus-review-max-evidence-bytes)
                         (integerp magnus-review-max-evidence-bytes))
                    magnus-review-max-evidence-bytes
                  (* 50 1024 1024))))
    (when (> size limit)
      (error "Review %s evidence exceeds the configured size limit" kind))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'no-conversion))
        (insert-file-contents-literally path))
      (buffer-string))))

(defun magnus-review-ui--persisted-evidence (review round)
  "Return persisted patch and name-status text for REVIEW ROUND.

Return nil when both artifacts are absent, as with reviews created before
durable evidence was introduced.  A partial or unsafe pair is corruption and
signals instead of silently substituting mutable repository state."
  (let* ((patch-path
          (magnus-review-ui--artifact-path
           'magnus-review-round-patch-path review round))
         (name-status-path
          (magnus-review-ui--artifact-path
           'magnus-review-round-name-status-path review round))
         (patch-present (magnus-review-ui--artifact-present-p patch-path))
         (name-status-present
          (magnus-review-ui--artifact-present-p name-status-path)))
    (cond
     ((and patch-present name-status-present)
      (list
       :patch
       (decode-coding-string
        (magnus-review-ui--artifact-bytes patch-path "patch") 'utf-8-unix)
       :name-status
       (decode-coding-string
        (magnus-review-ui--artifact-bytes name-status-path "name-status")
        'utf-8-unix)))
     ((or patch-present name-status-present)
      (error "Review has an incomplete persisted evidence pair"))
     (t nil))))

(defun magnus-review-ui--path-from-header (line prefix)
  "Extract and normalize a path from LINE beginning with PREFIX."
  (when (string-prefix-p prefix line)
    (let ((raw (substring line (length prefix))))
      (when (and (> (length raw) 1)
                 (eq (aref raw 0) ?\")
                 (eq (aref raw (1- (length raw))) ?\"))
        (setq raw (condition-case nil
                      (let ((decoded (car (read-from-string raw))))
                        ;; Git C-quotes UTF-8 path bytes using octal escapes.
                        ;; The Lisp reader returns those as a unibyte string.
                        (if (multibyte-string-p decoded)
                            decoded
                          (decode-coding-string decoded 'utf-8-unix)))
                    (error raw))))
      (magnus-review-ui--normalize-path raw))))

(defun magnus-review-ui--parse-hunk-header (line)
  "Parse unified diff hunk header LINE into an internal hunk."
  (when (string-match
         "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@\\(.*\\)\\'"
         line)
    (magnus-review-ui--make-hunk
     :header line
     :old-start (string-to-number (match-string 1 line))
     :old-count (string-to-number (or (match-string 2 line) "1"))
     :new-start (string-to-number (match-string 3 line))
     :new-count (string-to-number (or (match-string 4 line) "1"))
     :lines nil)))

(defun magnus-review-ui--parse-diff (patch statuses)
  "Parse unified PATCH, associating files with zero-safe STATUSES."
  (let ((lines (split-string patch "\n" nil))
        (status-queue statuses)
        files file hunk old-line new-line)
    (when (and lines (string-empty-p (car (last lines))))
      (setq lines (butlast lines)))
    (cl-labels
        ((finish-hunk
          ()
          (when hunk
            (setf (magnus-review-ui--hunk-lines hunk)
                  (nreverse (magnus-review-ui--hunk-lines hunk)))
            (push hunk (magnus-review-ui--file-hunks file))
            (setq hunk nil)))
         (finish-file
          ()
          (when file
            (finish-hunk)
            (setf (magnus-review-ui--file-headers file)
                  (nreverse (magnus-review-ui--file-headers file)))
            (setf (magnus-review-ui--file-hunks file)
                  (nreverse (magnus-review-ui--file-hunks file)))
            (setf (magnus-review-ui--file-display-path file)
                  (or (magnus-review-ui--file-new-path file)
                      (magnus-review-ui--file-old-path file)
                      "unknown"))
            (push file files)
            (setq file nil)))
         (start-file
          (line)
          (finish-file)
          (let ((record (pop status-queue)))
            (setq file
                  (magnus-review-ui--make-file
                   :status (or (plist-get record :status) "M")
                   :old-path (plist-get record :old-path)
                   :new-path (plist-get record :new-path)
                   :headers (list line)
                   :hunks nil))))
         (insert-hunk-line
          (line)
          (let (kind this-old this-new)
            (cond
             ((string-prefix-p "+" line)
              (setq kind 'added this-new new-line)
              (cl-incf new-line))
             ((string-prefix-p "-" line)
              (setq kind 'removed this-old old-line)
              (cl-incf old-line))
             ((string-prefix-p " " line)
              (setq kind 'context this-old old-line this-new new-line)
              (cl-incf old-line)
              (cl-incf new-line))
             ((string-prefix-p "\\" line)
              (setq kind 'marker))
             (t (setq kind 'metadata)))
            (push (magnus-review-ui--make-diff-line
                   :kind kind :text line
                   :old-line this-old :new-line this-new)
                  (magnus-review-ui--hunk-lines hunk))))
         (check-path-header
          (line prefix side)
          (when (string-prefix-p prefix line)
            (let* ((raw (substring line (length prefix)))
                   (null-path (string= raw "/dev/null"))
                   (path (unless null-path
                           (magnus-review-ui--path-from-header line prefix)))
                   (expected
                    (if (eq side 'old)
                        (magnus-review-ui--file-old-path file)
                      (magnus-review-ui--file-new-path file))))
              (cond
               (null-path
                (when expected
                  (error "Review patch path disagrees with name-status")))
               ((null path)
                (error "Review patch contains an unsafe path header"))
               ((and expected (not (equal expected path)))
                (error "Review patch path disagrees with name-status"))
               ((eq side 'old)
                (setf (magnus-review-ui--file-old-path file) path))
               (t
                (setf (magnus-review-ui--file-new-path file) path)))))))
      (dolist (line lines)
        (cond
         ((string-prefix-p "diff --git " line)
          (start-file line))
         ((and file (string-prefix-p "@@ " line))
          (finish-hunk)
          (setq hunk (magnus-review-ui--parse-hunk-header line))
          (when hunk
            (setq old-line (magnus-review-ui--hunk-old-start hunk)
                  new-line (magnus-review-ui--hunk-new-start hunk))))
         ((and file hunk)
          (insert-hunk-line line))
         (file
          (push line (magnus-review-ui--file-headers file))
          (check-path-header line "--- " 'old)
          (check-path-header line "+++ " 'new))))
      (finish-file))
    (nreverse files)))

(defun magnus-review-ui--parse-evidence (patch name-status)
  "Parse and cross-check persisted PATCH and NAME-STATUS evidence."
  (let* ((statuses (magnus-review-ui--parse-name-status name-status))
         (files (magnus-review-ui--parse-diff patch statuses)))
    (unless (= (length files) (length statuses))
      (error
       "Review evidence disagrees: patch has %d file%s, name-status has %d"
       (length files) (if (= (length files) 1) "" "s")
       (length statuses)))
    files))

(defun magnus-review-ui--validate-result-scope (round result)
  "Require any scope object IDs in RESULT to agree with ROUND."
  (unless (magnus-review-ui--result-error result)
    (dolist (spec `((,(magnus-review-ui--round-base round)
                      (:base_oid :base-oid :reviewed_base_oid
                       :reviewed-base-oid)
                      "base")
                    (,(magnus-review-ui--round-head round)
                      (:head_oid :head-oid :reviewed_head_oid
                       :reviewed-head-oid)
                      "head")))
      (let ((expected (nth 0 spec))
            (actual (magnus-review-ui--result-field result (nth 1 spec)))
            (kind (nth 2 spec)))
        (when (and actual expected
                   (not (and (stringp actual)
                             (string-equal-ignore-case actual expected))))
          (error "Review result %s object does not match its immutable round"
                 kind))))))

(defun magnus-review-ui--load-diff (review round)
  "Load and parse REVIEW ROUND's immutable base..head diff."
  (let ((root (magnus-review-ui--project-root review))
        (base (magnus-review-ui--round-base round))
        (head (magnus-review-ui--round-head round)))
    (unless (magnus-review-ui--valid-oid-p base)
      (error "Review round has no resolved base object ID"))
    (unless (magnus-review-ui--valid-oid-p head)
      (error "Review round has no resolved head object ID"))
    (magnus-review-ui--validate-result-scope round magnus-review-ui--result)
    (if-let ((evidence (magnus-review-ui--persisted-evidence review round)))
        (progn
          (setq magnus-review-ui--evidence-source 'persisted)
          (magnus-review-ui--parse-evidence
           (plist-get evidence :patch)
           (plist-get evidence :name-status)))
      ;; Compatibility path for reviews created before durable round evidence.
      ;; New reviews must never reach this branch.
      (setq magnus-review-ui--evidence-source 'git)
      (let ((statuses (magnus-review-ui--name-status-live root base head))
            (patch (magnus-review-ui--git
                    root "diff" "--binary" "--full-index"
                    "--no-ext-diff" "--no-color" "--find-renames"
                    base head "--")))
        (magnus-review-ui--parse-diff patch statuses)))))

;;; Finding assignment

(defun magnus-review-ui--integer (value)
  "Return VALUE as a positive integer, or nil."
  (let ((number (cond ((integerp value) value)
                      ((and (stringp value)
                            (string-match-p "\\`[0-9]+\\'" value))
                       (string-to-number value)))))
    (and number (> number 0) number)))

(defun magnus-review-ui--normalize-findings (findings)
  "Normalize structured FINDINGS for rendering."
  (cl-loop
   for raw in findings
   for index from 1
   collect
   (magnus-review-ui--make-finding
    :raw raw
    :id (or (magnus-review-ui--map-value raw '(:id :finding_id :finding-id))
            (format "F%d" index))
    :severity (or (magnus-review-ui--map-value raw '(:severity :priority))
                  "note")
    :kind (magnus-review-ui--map-value raw '(:kind :type))
    :path (magnus-review-ui--normalize-path
           (magnus-review-ui--map-value raw '(:path :file :filename)))
    :line (magnus-review-ui--integer
           (magnus-review-ui--map-value
            raw '(:line :head_line :head-line :start_line :start-line)))
    :end-line (magnus-review-ui--integer
               (magnus-review-ui--map-value
                raw '(:end_line :end-line :head_end_line :head-end-line)))
    :side (or (magnus-review-ui--map-value raw '(:side)) "head")
    :title (or (magnus-review-ui--map-value raw '(:title :subject))
               "Untitled finding")
    :evidence (magnus-review-ui--map-value
               raw '(:evidence :body :explanation :detail))
    :recommendation (magnus-review-ui--map-value
                     raw '(:recommendation :suggestion :fix)))))

(defun magnus-review-ui--file-aliases (file)
  "Return all paths identifying FILE."
  (delete-dups
   (delq nil (list (magnus-review-ui--file-display-path file)
                   (magnus-review-ui--file-old-path file)
                   (magnus-review-ui--file-new-path file)))))

(defun magnus-review-ui--head-line-present-p (file line)
  "Return non-nil when FILE's displayed hunks contain head LINE."
  (seq-some
   (lambda (hunk)
     (seq-some
      (lambda (diff-line)
        (equal line (magnus-review-ui--diff-line-new-line diff-line)))
      (magnus-review-ui--hunk-lines hunk)))
   (magnus-review-ui--file-hunks file)))

(defun magnus-review-ui--assign-findings (files findings)
  "Assign FINDINGS to inline, file, and general locations in FILES.

Return a plist containing equal-tested hash tables under `:inline' and
`:file', plus a list under `:general'."
  (let ((path-files (make-hash-table :test #'equal))
        (inline (make-hash-table :test #'equal))
        (file-findings (make-hash-table :test #'equal))
        general)
    (dolist (file files)
      (dolist (path (magnus-review-ui--file-aliases file))
        (puthash path file path-files)))
    (dolist (finding findings)
      (let* ((path (magnus-review-ui--finding-path finding))
             (file (and path (gethash path path-files)))
             (kind (downcase
                    (or (magnus-review-ui--string
                         (magnus-review-ui--finding-kind finding)) "")))
             (side (downcase
                    (or (magnus-review-ui--string
                         (magnus-review-ui--finding-side finding)) "head")))
             (line (magnus-review-ui--finding-line finding)))
        (cond
         ((or (null file) (string= kind "general"))
          (push finding general))
         ((and line
               (not (string= kind "file"))
               (member side '("head" "right" "new"))
               (magnus-review-ui--head-line-present-p file line))
          (let ((key (cons (magnus-review-ui--file-display-path file) line)))
            (puthash key (cons finding (gethash key inline)) inline)))
         (t
          (let ((key (magnus-review-ui--file-display-path file)))
            (puthash key (cons finding (gethash key file-findings))
                     file-findings))))))
    (list :inline inline :file file-findings :general (nreverse general))))

;;; Rendering

(defun magnus-review-ui--verdict-face (verdict)
  "Return an appropriate face for VERDICT."
  (pcase (downcase (or (magnus-review-ui--string verdict) "pending"))
    ((or "approve" "approved") 'magnus-review-ui-approved)
    ((or "changes-requested" "changes_requested" "request-changes"
         "request_changes")
     'magnus-review-ui-changes-requested)
    (_ 'magnus-review-ui-comment-verdict)))

(defun magnus-review-ui--severity-face (severity)
  "Return an appropriate face for SEVERITY."
  (pcase (downcase (or (magnus-review-ui--string severity) "note"))
    ((or "critical" "blocker" "p0") 'magnus-review-ui-critical)
    ((or "high" "major" "error" "p1") 'magnus-review-ui-high)
    ((or "medium" "minor" "warning" "p2") 'magnus-review-ui-medium)
    ((or "low" "nit" "suggestion" "p3") 'magnus-review-ui-low)
    (_ 'magnus-review-ui-metadata)))

(defun magnus-review-ui--display-value (value &optional fallback)
  "Return VALUE as non-empty display text, or FALLBACK."
  (let ((text (magnus-review-ui--string value)))
    (if (and text (not (string-empty-p text))) text fallback)))

(defun magnus-review-ui--insert-header (files)
  "Insert the current review header, including scope and FILES count."
  (let* ((review magnus-review-ui--review)
         (round magnus-review-ui--round)
         (rounds (magnus-review-ui--rounds review))
         (author (magnus-review-ui--display-value
                  (magnus-review-ui--author-name review) "unknown author"))
         (reviewer (magnus-review-ui--display-value
                    (magnus-review-ui--reviewer-name review) "unassigned"))
         (provider (magnus-review-ui--display-value
                    (magnus-review-ui--reviewer-provider review) "provider"))
         (model (magnus-review-ui--display-value
                 (magnus-review-ui--model review) "default model"))
         (effort (magnus-review-ui--display-value
                  (magnus-review-ui--effort review) "default effort"))
         (number (or (magnus-review-ui--round-number round) "?"))
         (verdict (magnus-review-ui--display-value
                   (magnus-review-ui--verdict round magnus-review-ui--result)
                   "pending")))
    (insert (propertize (format "Review of %s\n" author)
                        'face 'magnus-review-ui-title))
    (insert (propertize
             (format "Reviewer: %s [%s · %s · %s]    Round: %s of %d\n"
                     reviewer provider model effort number (length rounds))
             'face 'magnus-review-ui-metadata))
    (insert (propertize
             (format "Scope: %s..%s    Files: %d    Evidence: %s    Verdict: "
                     (magnus-review-ui--short-oid
                      (magnus-review-ui--round-base round))
                     (magnus-review-ui--short-oid
                      (magnus-review-ui--round-head round))
                     (length files)
                     (pcase magnus-review-ui--evidence-source
                       ('persisted "archived")
                       ('git "Git")
                       (_ "unavailable")))
             'face 'magnus-review-ui-metadata))
    (insert (propertize (upcase verdict)
                        'face (magnus-review-ui--verdict-face verdict)))
    (insert "\n")
    (when-let ((task (magnus-review-ui--task review)))
      (insert (propertize (format "Task: %s\n" task)
                          'face 'magnus-review-ui-metadata)))
    (insert "\n")))

(defun magnus-review-ui--insert-block (text indent &optional face)
  "Insert multiline TEXT with INDENT and optional FACE."
  (when-let ((value (magnus-review-ui--string text)))
    (dolist (line (split-string value "\n" nil))
      (insert indent)
      (insert (if face (propertize line 'face face) line))
      (insert "\n"))))

(defun magnus-review-ui--result-items (keys)
  "Return structured result value under KEYS as a list of items."
  (let ((value (magnus-review-ui--result-field
                magnus-review-ui--result keys)))
    (cond ((null value) nil)
          ((vectorp value) (append value nil))
          ;; A JSON object is a plist; a JSON array is already a list.
          ((and (listp value) (keywordp (car value))) (list value))
          ((listp value) value)
          (t (list value)))))

(defun magnus-review-ui--format-note-item (item)
  "Return compact human-readable text for structured note ITEM."
  (or (magnus-review-ui--string item)
      (when (or (hash-table-p item) (listp item))
        (let* ((id (magnus-review-ui--map-value
                    item '(:id :finding_id :finding-id)))
               (title (magnus-review-ui--map-value
                       item '(:title :name :area :command :test)))
               (status (magnus-review-ui--map-value
                        item '(:status :disposition :result)))
               (detail (magnus-review-ui--map-value
                        item '(:detail :reason :note :summary :explanation)))
               (parts (delq nil
                            (mapcar #'magnus-review-ui--string
                                    (list id title status)))))
          (concat (if parts (string-join parts ": ") "Review note")
                  (if detail
                      (format " — %s"
                              (magnus-review-ui--display-value detail ""))
                    ""))))
      "Review note"))

(defun magnus-review-ui--insert-result-notes ()
  "Insert compact strengths, verification, and continuity notes."
  (dolist (spec '(("Strengths" (:strengths))
                  ("Coverage" (:coverage))
                  ("Tests" (:tests :tests_run :tests-run))
                  ("Limitations" (:limitations))
                  ("Prior findings"
                   (:prior_findings :prior-findings
                    :prior_finding_dispositions
                    :prior-finding-dispositions))))
    (when-let ((items (magnus-review-ui--result-items (cadr spec))))
      (insert "  " (propertize (concat (car spec) ":") 'face 'bold) "\n")
      (dolist (item items)
        (insert "    • " (magnus-review-ui--format-note-item item) "\n")))))

(defun magnus-review-ui--insert-summary ()
  "Insert the structured review summary."
  (magit-insert-section
      (magnus-review-ui-summary-section 'summary nil)
    (magit-insert-heading "Summary")
    (cond
     ((magnus-review-ui--result-error magnus-review-ui--result)
      (insert (propertize "  Structured review result could not be read:\n"
                          'face 'error))
      (magnus-review-ui--insert-block
       (magnus-review-ui--result-error magnus-review-ui--result) "    "))
     ((magnus-review-ui--summary magnus-review-ui--result)
      (magnus-review-ui--insert-block
       (magnus-review-ui--summary magnus-review-ui--result) "  "))
     (t
      (insert (propertize "  No summary was returned.\n"
                          'face 'magnus-review-ui-metadata))))
    (magnus-review-ui--insert-result-notes)
    (insert "\n")))

(defun magnus-review-ui--finding-label (finding)
  "Return a one-line label for FINDING."
  (let ((severity (upcase
                   (magnus-review-ui--display-value
                    (magnus-review-ui--finding-severity finding) "note")))
        (id (magnus-review-ui--display-value
             (magnus-review-ui--finding-id finding) "F?"))
        (title (magnus-review-ui--display-value
                (magnus-review-ui--finding-title finding)
                "Untitled finding")))
    (concat (propertize severity 'face
                        (magnus-review-ui--severity-face severity))
            (format " %s  %s" id title))))

(defun magnus-review-ui--insert-finding (finding indent current-path)
  "Insert FINDING at INDENT, using CURRENT-PATH for worktree navigation."
  (let* ((path (magnus-review-ui--finding-path finding))
         (line (magnus-review-ui--finding-line finding))
         (side (intern
                (downcase (or (magnus-review-ui--string
                               (magnus-review-ui--finding-side finding))
                              "head"))))
         (id (magnus-review-ui--display-value
              (magnus-review-ui--finding-id finding) "F?")))
    (magit-insert-section
        (magnus-review-ui-finding-section id nil
          :path path :current-path (or current-path path)
          :line line :side side)
      (magit-insert-heading indent (magnus-review-ui--finding-label finding))
      (when (or path line)
        (insert indent "  ")
        (insert (propertize
                 (cond ((and path line) (format "%s:%d" path line))
                       (path path)
                       (t (format "line %d" line)))
                 'face 'magnus-review-ui-metadata))
        (insert "\n"))
      (when-let ((evidence (magnus-review-ui--finding-evidence finding)))
        (insert indent "  " (propertize "Evidence\n" 'face 'bold))
        (magnus-review-ui--insert-block
         evidence (concat indent "    ") 'magnus-review-ui-finding-body))
      (when-let ((recommendation
                  (magnus-review-ui--finding-recommendation finding)))
        (insert indent "  " (propertize "Recommendation\n" 'face 'bold))
        (magnus-review-ui--insert-block
         recommendation (concat indent "    ")
         'magnus-review-ui-finding-body)))))

(defun magnus-review-ui--insert-general-findings (findings)
  "Insert FINDINGS that cannot be associated with a changed file."
  (when findings
    (magit-insert-section
        (magnus-review-ui-findings-section 'general-findings nil)
      (magit-insert-heading (format "General findings (%d)" (length findings)))
      (dolist (finding findings)
        (magnus-review-ui--insert-finding finding "  " nil))
      (insert "\n"))))

(defun magnus-review-ui--diff-line-face (kind)
  "Return a face for a diff line of KIND."
  (pcase kind
    ('added 'diff-added)
    ('removed 'diff-removed)
    ('marker 'diff-header)
    (_ 'default)))

(defun magnus-review-ui--insert-diff-line (file line)
  "Insert LINE from FILE and attach exact-snapshot location properties."
  (let* ((kind (magnus-review-ui--diff-line-kind line))
         (old (magnus-review-ui--diff-line-old-line line))
         (new (magnus-review-ui--diff-line-new-line line))
         (removed (eq kind 'removed))
         (path (if removed
                   (magnus-review-ui--file-old-path file)
                 (or (magnus-review-ui--file-new-path file)
                     (magnus-review-ui--file-old-path file))))
         (current-path (magnus-review-ui--file-new-path file))
         (target-line (if removed old new))
         (side (if removed 'base 'head))
         (start (point)))
    (insert "    ")
    (insert (propertize (format "%5s %5s " (or old "") (or new ""))
                        'face 'magnus-review-ui-line-number))
    (insert (propertize (magnus-review-ui--diff-line-text line)
                        'face (magnus-review-ui--diff-line-face kind)))
    (insert "\n")
    (add-text-properties
     start (point)
     (list 'magnus-review-ui-path path
           'magnus-review-ui-current-path current-path
           'magnus-review-ui-line target-line
           'magnus-review-ui-side side
           'rear-nonsticky t))))

(defun magnus-review-ui--hunk-inline-findings (file hunk inline)
  "Return inline findings belonging to FILE HUNK from INLINE."
  (let (findings)
    (dolist (line (magnus-review-ui--hunk-lines hunk))
      (when-let ((new-line (magnus-review-ui--diff-line-new-line line)))
        (setq findings
              (nconc findings
                     (reverse
                      (gethash (cons (magnus-review-ui--file-display-path file)
                                     new-line)
                               inline))))))
    findings))

(defun magnus-review-ui--insert-hunk (file hunk inline)
  "Insert FILE HUNK and findings assigned through INLINE."
  (let* ((path (or (magnus-review-ui--file-new-path file)
                   (magnus-review-ui--file-old-path file)))
         (current-path (magnus-review-ui--file-new-path file))
         (side (if current-path 'head 'base))
         (line (if current-path
                   (magnus-review-ui--hunk-new-start hunk)
                 (magnus-review-ui--hunk-old-start hunk)))
         (findings (magnus-review-ui--hunk-inline-findings
                    file hunk inline))
         (hide (null findings)))
    (magit-insert-section
        (magnus-review-ui-hunk-section
          (list path
                (magnus-review-ui--hunk-old-start hunk)
                (magnus-review-ui--hunk-new-start hunk))
          hide :path path :current-path current-path :line line :side side)
      (magit-insert-heading
       "  " (propertize (magnus-review-ui--hunk-header hunk)
                         'face 'diff-hunk-header))
      (dolist (diff-line (magnus-review-ui--hunk-lines hunk))
        (magnus-review-ui--insert-diff-line file diff-line)
        (when-let ((new-line (magnus-review-ui--diff-line-new-line diff-line)))
          (dolist (finding
                   (reverse
                    (gethash
                     (cons (magnus-review-ui--file-display-path file) new-line)
                     inline)))
            (magnus-review-ui--insert-finding
             finding "      " current-path)))))))

(defun magnus-review-ui--file-status-label (status)
  "Return a human-readable label for Git STATUS."
  (pcase (and status (aref status 0))
    (?A "added") (?D "deleted") (?R "renamed") (?C "copied")
    (?T "type changed") (?U "unmerged") (_ "modified")))

(defun magnus-review-ui--insert-file (file inline file-findings)
  "Insert FILE, using INLINE and FILE-FINDINGS assignment tables."
  (let* ((path (magnus-review-ui--file-display-path file))
         (fallbacks (reverse (gethash path file-findings)))
         (hunk-findings
          (seq-some (lambda (hunk)
                      (magnus-review-ui--hunk-inline-findings file hunk inline))
                    (magnus-review-ui--file-hunks file)))
         (finding-count
          (+ (length fallbacks)
             (cl-loop for hunk in (magnus-review-ui--file-hunks file)
                      sum (length (magnus-review-ui--hunk-inline-findings
                                   file hunk inline)))))
         (current-path (magnus-review-ui--file-new-path file))
         (exact-path (or current-path (magnus-review-ui--file-old-path file)))
         (side (if current-path 'head 'base)))
    (magit-insert-section
        (magnus-review-ui-file-section path (and (null fallbacks)
                                                  (null hunk-findings))
          :path exact-path :current-path current-path :line 1 :side side)
      (magit-insert-heading
       (propertize
        (format "%s  %s%s"
                (magnus-review-ui--file-status-label
                 (magnus-review-ui--file-status file))
                path
                (if (> finding-count 0)
                    (format "  (%d finding%s)"
                            finding-count (if (= finding-count 1) "" "s"))
                  ""))
        'face 'diff-file-header))
      (dolist (header (magnus-review-ui--file-headers file))
        (insert "  " (propertize header 'face 'diff-header) "\n"))
      (when fallbacks
        (magit-insert-section
            (magnus-review-ui-findings-section
              (cons 'file-findings path) nil)
          (magit-insert-heading
           (format "  File findings (%d)" (length fallbacks)))
          (dolist (finding fallbacks)
            (magnus-review-ui--insert-finding
             finding "    " current-path))))
      (dolist (hunk (magnus-review-ui--file-hunks file))
        (magnus-review-ui--insert-hunk file hunk inline))
      (insert "\n"))))

(defun magnus-review-ui--insert-diff-error ()
  "Insert the current diff-loading error, if any."
  (when magnus-review-ui--diff-error
    (magit-insert-section
        (magnus-review-ui-summary-section 'diff-error nil)
      (magit-insert-heading
       (propertize "Diff unavailable" 'face 'error))
      (magnus-review-ui--insert-block magnus-review-ui--diff-error "  ")
      (insert "\n"))))

(defun magnus-review-ui--render (files)
  "Render the current review around parsed diff FILES."
  (let* ((findings (magnus-review-ui--normalize-findings
                    (magnus-review-ui--findings magnus-review-ui--result)))
         (assigned (magnus-review-ui--assign-findings files findings))
         (inline (plist-get assigned :inline))
         (file-findings (plist-get assigned :file))
         (general (plist-get assigned :general))
         (id (or (magnus-review-ui--review-id magnus-review-ui--review)
                 "review")))
    (magit-insert-section (magnus-review-ui-root-section id)
      (magnus-review-ui--insert-header files)
      (magnus-review-ui--insert-summary)
      (magnus-review-ui--insert-general-findings general)
      (magnus-review-ui--insert-diff-error)
      (if files
          (dolist (file files)
            (magnus-review-ui--insert-file file inline file-findings))
        (unless magnus-review-ui--diff-error
          (insert (propertize "No changes in the reviewed range.\n"
                              'face 'magnus-review-ui-metadata)))))))

;;; Public display API

(defun magnus-review-ui-current-review ()
  "Return the review displayed in the current reader buffer."
  magnus-review-ui--review)

(defun magnus-review-ui-current-round ()
  "Return the review round displayed in the current reader buffer."
  magnus-review-ui--round)

(defun magnus-review-ui--select-round (review round)
  "Resolve ROUND, which may be an object or number, within REVIEW."
  (cond
   ((null round) (magnus-review-ui--latest-round review))
   ((integerp round)
    (seq-find (lambda (candidate)
                (equal round (magnus-review-ui--round-number candidate)))
              (magnus-review-ui--rounds review)))
   (t round)))

(defun magnus-review-ui--buffer-name (review)
  "Return the reader buffer name for REVIEW."
  (let ((author (magnus-review-ui--display-value
                 (magnus-review-ui--author-name review) "unknown"))
        (id (magnus-review-ui--display-value
             (magnus-review-ui--review-id review) "review")))
    (format "*magnus-review: %s [%s]*"
            author (substring id 0 (min 8 (length id))))))

;;;###autoload
(defun magnus-review-ui-open (review &optional round)
  "Open REVIEW at ROUND in a Magit-style review reader.

ROUND may be a round object, a round number, or nil for the latest round.
Display uses `pop-to-buffer', so `display-buffer-alist' remains in control and
Magnus never manufactures a window layout."
  (let ((selected (magnus-review-ui--select-round review round)))
    (unless selected
      (user-error "Review has no completed rounds to display"))
    (let ((buffer (get-buffer-create (magnus-review-ui--buffer-name review))))
      (with-current-buffer buffer
        (unless (derived-mode-p 'magnus-review-ui-mode)
          (magnus-review-ui-mode))
        (setq-local magnus-review-ui--review review)
        (setq-local magnus-review-ui--round selected)
        (when-let ((root (magnus-review-ui--project-root review)))
          (setq default-directory (file-name-as-directory root)))
        (magnus-review-ui-refresh))
      (pop-to-buffer buffer))))

(defun magnus-review-ui--mark-read ()
  "Mark the displayed round read using the configured callback."
  (let ((number (magnus-review-ui--round-number magnus-review-ui--round)))
    ;; A missing or corrupt structured result is an error view, not a read
    ;; review.  Preserve the unread indicator so Hrishi can return after the
    ;; controller repairs or retries the round.
    (unless (or (null magnus-review-ui--result)
                (magnus-review-ui--result-error magnus-review-ui--result)
                (member number magnus-review-ui--marked-read-rounds))
      (when (and magnus-review-ui-mark-read-function
                 (functionp magnus-review-ui-mark-read-function))
        (let ((succeeded nil))
          (condition-case err
              (progn
                (funcall magnus-review-ui-mark-read-function
                         magnus-review-ui--review magnus-review-ui--round)
                (setq succeeded t))
            (error
             (message "Magnus could not mark review round read: %s"
                      (error-message-string err))))
          (when succeeded
            (push number magnus-review-ui--marked-read-rounds)))))))

(defun magnus-review-ui-refresh ()
  "Refresh the current review reader from its immutable Git objects."
  (interactive)
  (unless (and magnus-review-ui--review magnus-review-ui--round)
    (user-error "This buffer is not associated with a review round"))
  (setq magnus-review-ui--result
        (magnus-review-ui--read-result
         magnus-review-ui--review magnus-review-ui--round))
  (setq magnus-review-ui--evidence-source nil)
  (let ((files
         (condition-case err
             (prog1 (magnus-review-ui--load-diff
                     magnus-review-ui--review magnus-review-ui--round)
               (setq magnus-review-ui--diff-error nil))
           (error
            (setq magnus-review-ui--diff-error (error-message-string err))
            nil)))
        (inhibit-read-only t))
    (erase-buffer)
    (save-excursion
      (magnus-review-ui--render files))
    (goto-char (point-min))
    (set-buffer-modified-p nil))
  (magnus-review-ui--mark-read))

(defun magnus-review-ui--revert (_ignore-auto _noconfirm)
  "Revert the current review reader buffer."
  (magnus-review-ui-refresh))

;;; Navigation and actions

(defun magnus-review-ui--finding-sections ()
  "Return all finding sections in display order."
  (let (sections)
    (when magit-root-section
      (magit-map-sections
       (lambda (section)
         (when (cl-typep section 'magnus-review-ui-finding-section)
           (push section sections)))))
    (sort sections
          (lambda (left right)
            (< (oref left start) (oref right start))))))

(defun magnus-review-ui--reveal-section (section)
  "Expand SECTION's ancestors so SECTION becomes visible."
  (let ((parent (oref section parent)) ancestors)
    (while parent
      (unless (eq parent magit-root-section)
        (push parent ancestors))
      (setq parent (oref parent parent)))
    (dolist (ancestor ancestors)
      (magit-section-show ancestor))))

(defun magnus-review-ui--move-finding (direction)
  "Move to a finding in DIRECTION, where positive means forward."
  (let* ((origin (point))
         (sections (magnus-review-ui--finding-sections))
         (target
          (if (> direction 0)
              (seq-find (lambda (section) (> (oref section start) origin))
                        sections)
            (car (last
                  (seq-take-while
                   (lambda (section) (< (oref section start) origin))
                   sections))))))
    (unless target
      (user-error "No %s finding"
                  (if (> direction 0) "next" "previous")))
    (magnus-review-ui--reveal-section target)
    (magit-section-goto target)))

(defun magnus-review-ui-next-finding ()
  "Move directly to the next review finding."
  (interactive)
  (magnus-review-ui--move-finding 1))

(defun magnus-review-ui-previous-finding ()
  "Move directly to the previous review finding."
  (interactive)
  (magnus-review-ui--move-finding -1))

(defun magnus-review-ui--location-at-point ()
  "Return exact and current-worktree location data at point."
  (let* ((position (point))
         (path (or (get-text-property position 'magnus-review-ui-path)
                   (get-text-property (line-beginning-position)
                                      'magnus-review-ui-path))))
    (if path
        (list :path path
              :current-path
              (or (get-text-property position 'magnus-review-ui-current-path)
                  (get-text-property (line-beginning-position)
                                     'magnus-review-ui-current-path))
              :line
              (or (get-text-property position 'magnus-review-ui-line)
                  (get-text-property (line-beginning-position)
                                     'magnus-review-ui-line))
              :side
              (or (get-text-property position 'magnus-review-ui-side)
                  (get-text-property (line-beginning-position)
                                     'magnus-review-ui-side)))
      (let ((section (magit-current-section)) location)
        (while (and section (not location))
          (when (cl-typep section 'magnus-review-ui-location-section)
            (when-let ((section-path (oref section path)))
              (setq location
                    (list :path section-path
                          :current-path (oref section current-path)
                          :line (oref section line)
                          :side (oref section side)))))
          (setq section (oref section parent)))
        location))))

(defun magnus-review-ui--snapshot-contents (root oid path)
  "Return PATH's contents at OID in ROOT."
  (unless (magnus-review-ui--valid-oid-p oid)
    (user-error "Review snapshot has no resolved object ID"))
  (unless (magnus-review-ui--normalize-path path)
    (user-error "Review finding has no safe repository path"))
  (magnus-review-ui--git root "show" (format "%s:%s" oid path)))

(defun magnus-review-ui--show-source-buffer (root oid path line contents)
  "Display CONTENTS from ROOT at OID and PATH, moving to LINE."
  (let* ((name (format "*magnus-review-source: %s:%s*"
                       (magnus-review-ui--short-oid oid) path))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert contents)
        (setq default-directory (file-name-as-directory root))
        (let ((buffer-file-name (expand-file-name path root)))
          (set-auto-mode))
        (setq-local buffer-file-name nil)
        (setq-local buffer-read-only t)
        (setq-local view-read-only t)
        (goto-char (point-min))
        (forward-line (1- (max 1 (or line 1))))
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer)
    (recenter)))

(defun magnus-review-ui-visit-snapshot ()
  "Visit the exact reviewed snapshot at point.

Added and context lines open the head object.  Removed lines open the base
object, ensuring that the displayed source and line number always agree."
  (interactive)
  (if-let ((location (magnus-review-ui--location-at-point)))
      (condition-case err
          (let* ((root
                  (magnus-review-ui--project-root magnus-review-ui--review))
                 (side (plist-get location :side))
                 (oid (if (memq side '(base left old))
                          (magnus-review-ui--round-base
                           magnus-review-ui--round)
                        (magnus-review-ui--round-head
                         magnus-review-ui--round)))
                 (path (plist-get location :path))
                 (line (plist-get location :line))
                 (contents (magnus-review-ui--snapshot-contents
                            root oid path)))
            (magnus-review-ui--show-source-buffer
             root oid path line contents))
        (error
         (user-error
          "Exact source is unavailable; the archived diff remains readable: %s"
          (error-message-string err))))
    (user-error "No reviewed source location at point")))

(defun magnus-review-ui-visit-current-file ()
  "Visit the file at point in the current working tree."
  (interactive)
  (if-let* ((location (magnus-review-ui--location-at-point))
            (path (or (plist-get location :current-path)
                      (plist-get location :path))))
      (let* ((root (file-name-as-directory
                    (magnus-review-ui--project-root
                     magnus-review-ui--review)))
             (safe (magnus-review-ui--normalize-path path))
             (file (and safe (expand-file-name safe root))))
        (unless (and file (file-in-directory-p file root))
          (user-error "Review location escapes the project root"))
        (unless (file-exists-p file)
          (user-error "File no longer exists in the working tree: %s" safe))
        (let ((buffer (find-file-noselect file))
              (line (or (plist-get location :line) 1)))
          (pop-to-buffer buffer)
          (goto-char (point-min))
          (forward-line (1- (max 1 line)))))
    (user-error "No current-worktree source location at point")))

(defun magnus-review-ui--move-round (delta)
  "Move DELTA rounds from the currently displayed review round."
  (let* ((rounds (magnus-review-ui--rounds magnus-review-ui--review))
         (number (magnus-review-ui--round-number magnus-review-ui--round))
         (position
          (cl-position number rounds :test #'equal
                       :key #'magnus-review-ui--round-number))
         (target (and position (+ position delta))))
    (unless (and target (>= target 0) (< target (length rounds)))
      (user-error "No %s review round" (if (< delta 0) "previous" "next")))
    (setq magnus-review-ui--round (nth target rounds))
    (magnus-review-ui-refresh)
    (message "Review round %s"
             (magnus-review-ui--round-number magnus-review-ui--round))))

(defun magnus-review-ui-previous-round ()
  "Display the previous round of the current review."
  (interactive)
  (magnus-review-ui--move-round -1))

(defun magnus-review-ui-next-round ()
  "Display the next round of the current review."
  (interactive)
  (magnus-review-ui--move-round 1))

(defun magnus-review-ui-actions ()
  "Invoke the configured review action dispatcher."
  (interactive)
  (if (and magnus-review-ui-action-function
           (functionp magnus-review-ui-action-function))
      (funcall magnus-review-ui-action-function
               magnus-review-ui--review magnus-review-ui--round)
    (user-error "No Magnus review action dispatcher is configured")))

(provide 'magnus-review-ui)
;;; magnus-review-ui.el ends here
