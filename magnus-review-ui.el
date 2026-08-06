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
;; The durable review domain lives in magnus-review.el.  The reader accepts
;; only its canonical review records and the canonical structured result
;; envelope published by magnus-review-controller.el.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'eieio)
(require 'json)
(require 'magit-section)
(require 'seq)
(require 'subr-x)
(require 'magnus-review)

(declare-function magnus-review-actions "magnus-transient" (review round))

;;; Canonical result artifacts

(defun magnus-review-ui--decode-result-bytes (bytes)
  "Decode verified unibyte result BYTES for presentation."
  (json-parse-string
   (decode-coding-string bytes 'utf-8-unix)
   :object-type 'plist :array-type 'list
   :null-object nil :false-object nil))

(defun magnus-review-ui--load-artifacts (review round)
  "Return REVIEW ROUND's domain-verified presentation bundle."
  (condition-case err
      (let ((bundle (magnus-review-read-verified-artifacts review round)))
        (plist-put (copy-sequence bundle) :ui-result
                   (magnus-review-ui--decode-result-bytes
                    (plist-get bundle :result-bytes))))
    (error
     (list :magnus_review_ui_error (error-message-string err)))))

(defun magnus-review-ui--result-body (result)
  "Return the canonical review payload within RESULT's envelope."
  (plist-get result :result))

(defun magnus-review-ui--result-field (result key)
  "Return KEY from the canonical structured RESULT envelope."
  (plist-get (magnus-review-ui--result-body result) key))

(defun magnus-review-ui--summary (result)
  "Return the summary from structured RESULT."
  (magnus-review-ui--result-field result :summary))

(defun magnus-review-ui--result-error (result)
  "Return a structured-result loading error from RESULT, if present."
  (plist-get result :magnus_review_ui_error))

(defun magnus-review-ui--findings (result)
  "Return the findings list from structured RESULT."
  (magnus-review-ui--result-field result :findings))

;;; Customization and faces

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
(defvar-local magnus-review-ui--review-id nil)
(defvar-local magnus-review-ui--round-number nil)
(defvar-local magnus-review-ui--round-base-oid nil)
(defvar-local magnus-review-ui--round-head-oid nil)
(defvar-local magnus-review-ui--artifacts nil)
(defvar-local magnus-review-ui--result nil)
(defvar-local magnus-review-ui--diff-error nil)

(defvar magnus-review-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "n") #'magit-section-forward)
    (define-key map (kbd "p") #'magit-section-backward)
    (define-key map (kbd "N") #'magnus-review-ui-next-finding)
    (define-key map (kbd "P") #'magnus-review-ui-previous-finding)
    (define-key map (kbd "RET") #'magnus-review-ui-visit-snapshot)
    (define-key map (kbd "e") #'magnus-review-ui-visit-current-file)
    (define-key map (kbd "TAB") #'magit-section-toggle)
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
  (cond ((null value) fallback)
        ((stringp value) value)
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
  (magnus-review-normalize-repository-path
   (magnus-review-ui--string path)))

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
              ;; Git represents a blob/symlink type change as one `T' status
              ;; but emits adjacent deletion and addition patch sections.  Feed
              ;; those sections exact one-sided records, then recombine them
              ;; into one logical file after both headers have been checked.
              (if (eq letter ?T)
                  (progn
                    (push (list :status status
                                :old-path safe-path
                                :new-path nil)
                          records)
                    (push (list :status status
                                :old-path nil
                                :new-path safe-path)
                          records))
                (push (list :status status
                            :old-path (unless (eq letter ?A) safe-path)
                            :new-path (unless (eq letter ?D) safe-path))
                      records)))))))
    (nreverse records)))

(defun magnus-review-ui--collapse-type-changes (files)
  "Collapse adjacent one-sided type-change FILES into logical files."
  (let (collapsed)
    (while files
      (let ((file (pop files)))
        (if (and (string-prefix-p "T" (or (magnus-review-ui--file-status file)
                                           ""))
                 (magnus-review-ui--file-old-path file)
                 (null (magnus-review-ui--file-new-path file)))
            (let ((addition (pop files)))
              (unless (and addition
                           (string-prefix-p
                            "T" (or (magnus-review-ui--file-status addition)
                                    ""))
                           (null (magnus-review-ui--file-old-path addition))
                           (equal (magnus-review-ui--file-old-path file)
                                  (magnus-review-ui--file-new-path addition)))
                (error "Review patch has a malformed Git type change"))
              (setf (magnus-review-ui--file-new-path file)
                    (magnus-review-ui--file-new-path addition)
                    (magnus-review-ui--file-display-path file)
                    (magnus-review-ui--file-new-path addition)
                    (magnus-review-ui--file-headers file)
                    (append (magnus-review-ui--file-headers file)
                            (magnus-review-ui--file-headers addition))
                    (magnus-review-ui--file-hunks file)
                    (append (magnus-review-ui--file-hunks file)
                            (magnus-review-ui--file-hunks addition)))
              (push file collapsed))
          (when (string-prefix-p "T" (or (magnus-review-ui--file-status file)
                                          ""))
            (error "Review patch has a malformed Git type change"))
          (push file collapsed))))
    (nreverse collapsed)))

(defun magnus-review-ui--persisted-evidence (_review _round)
  "Return canonical evidence from the current verified artifact bundle."
  (when-let ((failure
              (plist-get magnus-review-ui--artifacts
                         :magnus_review_ui_error)))
    (error "%s" failure))
  (unless magnus-review-ui--artifacts
    (error "Review artifacts have not been verified"))
  (list
   :patch
   (decode-coding-string
    (plist-get magnus-review-ui--artifacts :patch)
    'utf-8-unix)
   :name-status
   (decode-coding-string
    (plist-get magnus-review-ui--artifacts :name-status)
    'utf-8-unix)))

(defun magnus-review-ui--path-from-header (line prefix)
  "Extract and normalize a path from LINE beginning with PREFIX."
  (when (string-prefix-p prefix line)
    (magnus-review-decode-diff-header-path
     (substring line (length prefix))
     (if (string= prefix "--- ") "a/" "b/"))))

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
            (unless record
              (error "Review patch contains a file absent from name-status"))
            (setq file
                  (magnus-review-ui--make-file
                   :status (plist-get record :status)
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
    (when status-queue
      (error "Review name-status contains a file absent from the patch"))
    (nreverse files)))

(defun magnus-review-ui--parse-evidence (patch name-status)
  "Parse and cross-check persisted PATCH and NAME-STATUS evidence."
  (let* ((statuses (magnus-review-ui--parse-name-status name-status))
         (files (magnus-review-ui--parse-diff patch statuses)))
    (magnus-review-ui--collapse-type-changes files)))

(defun magnus-review-ui--validate-result-scope (round result)
  "Require any scope object IDs in RESULT to agree with ROUND."
  (unless (magnus-review-ui--result-error result)
    (dolist (spec `((,(magnus-review-round-base-oid round) :base_oid "base")
                    (,(magnus-review-round-head-oid round) :head_oid "head")))
      (let ((expected (nth 0 spec))
            (actual (magnus-review-ui--result-field result (nth 1 spec)))
            (kind (nth 2 spec)))
        (when (and actual expected
                   (not (and (stringp actual)
                             (string= (downcase actual)
                                      (downcase expected)))))
          (error "Review result %s object does not match its immutable round"
                 kind))))))

(defun magnus-review-ui--load-diff (review round)
  "Load and parse REVIEW ROUND's immutable base..head diff."
  (let ((base (magnus-review-round-base-oid round))
        (head (magnus-review-round-head-oid round)))
    (unless (magnus-review-ui--valid-oid-p base)
      (error "Review round has no resolved base object ID"))
    (unless (magnus-review-ui--valid-oid-p head)
      (error "Review round has no resolved head object ID"))
    (magnus-review-ui--validate-result-scope round magnus-review-ui--result)
    (let ((evidence (magnus-review-ui--persisted-evidence review round)))
      (magnus-review-ui--parse-evidence
       (plist-get evidence :patch)
       (plist-get evidence :name-status)))))

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
    :id (or (plist-get raw :id)
            (format "F%d" index))
    :severity (or (plist-get raw :severity) "note")
    :kind (plist-get raw :kind)
    :path (magnus-review-ui--normalize-path
           (plist-get raw :path))
    :line (magnus-review-ui--integer (plist-get raw :head_line))
    :end-line (magnus-review-ui--integer
               (plist-get raw :end_line))
    :side "head"
    :title (or (plist-get raw :title)
               "Untitled finding")
    :evidence (plist-get raw :explanation)
    :recommendation (plist-get raw :suggestion))))

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
  "Return VALUE as non-empty single-line display text, or FALLBACK."
  (let* ((raw (magnus-review-ui--string value))
         (text (and raw
                    (string-trim
                     (replace-regexp-in-string "[[:cntrl:]]+" " " raw)))))
    (if (and text (not (string-empty-p text))) text fallback)))

(defun magnus-review-ui--insert-header (files)
  "Insert the current review header, including scope and FILES count."
  (let* ((review magnus-review-ui--review)
         (round magnus-review-ui--round)
         (rounds (magnus-review-rounds review))
         (author (magnus-review-ui--display-value
                  (magnus-review-author-name review) "unknown author"))
         (reviewer (magnus-review-ui--display-value
                    (magnus-review-reviewer-name review) "unassigned"))
         (provider (magnus-review-ui--display-value
                    (magnus-review-reviewer-provider review) "provider"))
         (model (magnus-review-ui--display-value
                 (magnus-review-model review) "default model"))
         (effort (magnus-review-ui--display-value
                  (magnus-review-effort review) "default effort"))
         (number (or (magnus-review-round-number round) "?"))
         (verdict (magnus-review-ui--display-value
                   (magnus-review-round-verdict round)
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
                      (magnus-review-round-base-oid round))
                     (magnus-review-ui--short-oid
                      (magnus-review-round-head-oid round))
                     (length files)
                     "archived")
             'face 'magnus-review-ui-metadata))
    (insert (propertize (upcase verdict)
                        'face (magnus-review-ui--verdict-face verdict)))
    (insert "\n")
    (when-let ((task (magnus-review-ui--display-value
                      (magnus-review-task review))))
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

(defun magnus-review-ui--format-note-item (item)
  "Return compact human-readable text for structured note ITEM."
  (or (magnus-review-ui--string item)
      (let* ((id (plist-get item :id))
             (status (plist-get item :disposition))
             (detail (plist-get item :explanation))
             (label (string-join (delq nil (list id status)) ": ")))
        (concat (if (string-empty-p label) "Review note" label)
                (if detail (format " — %s" detail) "")))
      "Review note"))

(defun magnus-review-ui--insert-result-notes ()
  "Insert compact strengths, verification, and continuity notes."
  (dolist (spec '(("Strengths" :strengths)
                  ("Tests" :tests)
                  ("Prior findings" :prior_findings)))
    (when-let ((items (magnus-review-ui--result-field
                       magnus-review-ui--result (cadr spec))))
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
         (id (or (magnus-review-id magnus-review-ui--review)
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
  (if magnus-review-ui--review-id
      (car (magnus-review-ui--resolve-identity))
    magnus-review-ui--review))

(defun magnus-review-ui-current-round ()
  "Return the review round displayed in the current reader buffer."
  (if magnus-review-ui--review-id
      (cdr (magnus-review-ui--resolve-identity))
    magnus-review-ui--round))

(defun magnus-review-ui--remember-identity (review round)
  "Remember stable identity for resolved REVIEW and ROUND."
  (setq-local magnus-review-ui--review review
              magnus-review-ui--round round
              magnus-review-ui--review-id (magnus-review-id review)
              magnus-review-ui--round-number
              (magnus-review-round-number round)
              magnus-review-ui--round-base-oid
              (magnus-review-round-base-oid round)
              magnus-review-ui--round-head-oid
              (magnus-review-round-head-oid round)))

(defun magnus-review-ui--resolve-identity (&optional refresh-from-disk)
  "Resolve the current buffer's review identity.
When REFRESH-FROM-DISK is non-nil, first accept a strictly newer manifest."
  (unless (and (stringp magnus-review-ui--review-id)
               (integerp magnus-review-ui--round-number))
    (user-error "This buffer is not associated with a review round"))
  (let ((review (magnus-review-get magnus-review-ui--review-id)))
    (unless review
      (user-error "Review is no longer loaded: %s"
                  magnus-review-ui--review-id))
    (when refresh-from-disk
      (setq review (magnus-review-refresh-from-disk review)))
    (pcase-let ((`(,review . ,round)
                 (magnus-review-resolve-round
                  review magnus-review-ui--round-number)))
      (unless (and (string= magnus-review-ui--round-base-oid
                            (magnus-review-round-base-oid round))
                   (string= magnus-review-ui--round-head-oid
                            (magnus-review-round-head-oid round)))
        (error "Completed review round identity changed"))
      (magnus-review-ui--remember-identity review round)
      (cons review round))))

(defun magnus-review-ui--select-round (review round)
  "Resolve ROUND, which may be an object or number, within REVIEW."
  (cond
   ((null round) (magnus-review-latest-round review))
   ((integerp round)
    (seq-find (lambda (candidate)
                (equal round (magnus-review-round-number candidate)))
              (magnus-review-rounds review)))
   (t round)))

(defun magnus-review-ui--buffer-name (review)
  "Return the reader buffer name for REVIEW."
  (let ((author (magnus-review-ui--display-value
                 (magnus-review-author-name review) "unknown"))
        (id (magnus-review-ui--display-value
             (magnus-review-id review) "review")))
    (format "*magnus-review: %s [%s]*"
            author (substring id 0 (min 8 (length id))))))

;;;###autoload
(defun magnus-review-ui-open (review &optional round)
  "Open REVIEW at ROUND in a Magit-style review reader.

ROUND may be a round object, a round number, or nil for the latest round.
Display uses `pop-to-buffer', so `display-buffer-alist' remains in control and
Magnus never manufactures a window layout."
  (setq review (magnus-review-resolve-current review))
  (let ((selected (magnus-review-ui--select-round review round)))
    (unless selected
      (user-error "Review has no completed rounds to display"))
    (let ((buffer (get-buffer-create (magnus-review-ui--buffer-name review))))
      (with-current-buffer buffer
        (unless (derived-mode-p 'magnus-review-ui-mode)
          (magnus-review-ui-mode))
        (magnus-review-ui--remember-identity review selected)
        (when-let ((root (magnus-review-project-root review)))
          (setq default-directory (file-name-as-directory root)))
        (magnus-review-ui-refresh))
      (pop-to-buffer buffer))))

(defun magnus-review-ui--mark-read ()
  "Mark the displayed round read after every artifact rendered successfully."
  ;; A missing, corrupt, or scope-inconsistent view is not a read review.
  (unless (or (null magnus-review-ui--result)
              (magnus-review-ui--result-error magnus-review-ui--result)
              magnus-review-ui--diff-error)
    (condition-case err
        (magnus-review-mark-read
         magnus-review-ui--review magnus-review-ui--round)
      (error
       (message "Magnus could not mark review round read: %s"
                (error-message-string err))))))

(defun magnus-review-ui-refresh (&optional refresh-from-disk)
  "Refresh the current review reader.
When REFRESH-FROM-DISK is non-nil, re-read a strictly newer manifest first."
  (interactive (list t))
  (let* ((old-section (and magit-root-section (magit-current-section)))
         (old-relative
          (and old-section
               (magit-section-get-relative-position old-section))))
    (magnus-review-ui--resolve-identity refresh-from-disk)
    (setq magnus-review-ui--artifacts
          (magnus-review-ui--load-artifacts
           magnus-review-ui--review magnus-review-ui--round)
          magnus-review-ui--result
          (or (plist-get magnus-review-ui--artifacts :ui-result)
              (and (plist-get magnus-review-ui--artifacts
                              :magnus_review_ui_error)
                   magnus-review-ui--artifacts)))
    (let* ((files
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
      (if old-section
          (apply #'magit-section-goto-successor old-section old-relative)
        (goto-char (point-min)))
      (set-buffer-modified-p nil)))
  (magnus-review-ui--mark-read))

(defun magnus-review-ui--revert (_ignore-auto _noconfirm)
  "Revert the current review reader buffer."
  (magnus-review-ui-refresh t))

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
                  (magnus-review-project-root magnus-review-ui--review))
                 (side (plist-get location :side))
                 (oid (if (memq side '(base left old))
                          (magnus-review-round-base-oid
                           magnus-review-ui--round)
                        (magnus-review-round-head-oid
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
                    (magnus-review-project-root
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
  (let* ((rounds (magnus-review-rounds magnus-review-ui--review))
         (number (magnus-review-round-number magnus-review-ui--round))
         (position
          (cl-position number rounds :test #'equal
                       :key #'magnus-review-round-number))
         (target (and position (+ position delta))))
    (unless (and target (>= target 0) (< target (length rounds)))
      (user-error "No %s review round" (if (< delta 0) "previous" "next")))
    (magnus-review-ui--remember-identity
     magnus-review-ui--review (nth target rounds))
    (magnus-review-ui-refresh)
    (message "Review round %s"
             (magnus-review-round-number magnus-review-ui--round))))

(defun magnus-review-ui-previous-round ()
  "Display the previous round of the current review."
  (interactive)
  (magnus-review-ui--move-round -1))

(defun magnus-review-ui-next-round ()
  "Display the next round of the current review."
  (interactive)
  (magnus-review-ui--move-round 1))

(defun magnus-review-ui-actions ()
  "Invoke review actions for the current resolved review and round."
  (interactive)
  (unless (fboundp 'magnus-review-actions)
    (require 'magnus-transient))
  (pcase-let ((`(,review . ,round) (magnus-review-ui--resolve-identity)))
    (magnus-review-actions review round)))

(provide 'magnus-review-ui)
;;; magnus-review-ui.el ends here
