;;; magnus-review.el --- Completed review lineages and Git evidence -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns the durable, provider-independent result of a Magnus
;; review.  A lineage remembers its author and reviewer identity, and contains
;; only successfully completed rounds.  Scope discovery, provider processes,
;; retries, and failures are deliberately ephemeral controller concerns.
;;
;; Each completed round pins an exact Git base and head, immutable patch and
;; path evidence, a structured result, and a rendered report.  The latest
;; successful provider session ID is retained so a later round can continue
;; the reviewer/reviewee conversation without making process recovery durable.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magnus-instances)

(defgroup magnus-review nil
  "Completed cross-provider review lineages managed by Magnus."
  :group 'magnus
  :prefix "magnus-review-")

(defcustom magnus-review-directory-root
  (expand-file-name ".magnus/reviews" (or (getenv "HOME") "~"))
  "Private directory containing completed Magnus review lineages."
  :type 'directory
  :group 'magnus-review)

(defcustom magnus-review-max-evidence-bytes (* 64 1024 1024)
  "Maximum bytes accepted for one immutable review patch."
  :type 'integer
  :group 'magnus-review)

(defconst magnus-review-schema-version 1
  "Manifest version for completed review lineages.")

(defconst magnus-review--lifecycle-states '(open archived))
(defconst magnus-review--verdict-states
  '(approve comment changes-requested))
(defconst magnus-review--read-states '(unread read))

(define-error 'magnus-review-error "Magnus review error")
(define-error 'magnus-review-git-error "Magnus review Git error"
  'magnus-review-error)

;;; Repository paths

(defun magnus-review-normalize-repository-path (value)
  "Return safe repository-relative VALUE, or nil.
This preserves legitimate top-level `a/' and `b/' directories.  Synthetic Git
side prefixes are handled by `magnus-review-decode-diff-header-path'."
  (when (stringp value)
    (let ((path (string-remove-prefix "./" value)))
      (when (and (not (string-empty-p path))
                 (not (string= path "/dev/null"))
                 (not (file-name-absolute-p path))
                 (not (member ".." (split-string path "/" t)))
                 (not (string-match-p "[\0\n\r]" path)))
        path))))

(defun magnus-review--decode-git-quoted-path (value)
  "Decode Git's optional C-quoted path VALUE, or return nil when malformed."
  (cond
   ((not (stringp value)) nil)
   ((and (> (length value) 1)
         (eq (aref value 0) ?\")
         (eq (aref value (1- (length value))) ?\"))
    (condition-case err
        (pcase-let ((`(,decoded . ,end) (read-from-string value)))
          (when (and (= end (length value)) (stringp decoded))
            (if (multibyte-string-p decoded)
                decoded
              (decode-coding-string decoded 'utf-8-unix))))
      (error
       (message "Magnus: malformed Git quoted path: %s"
                (error-message-string err))
       nil)))
   (t value)))

(defun magnus-review-decode-diff-header-path (value side-prefix)
  "Decode unified-diff VALUE and remove one synthetic SIDE-PREFIX."
  (let ((decoded (magnus-review--decode-git-quoted-path value)))
    (when (and (member side-prefix '("a/" "b/"))
               (stringp decoded)
               (string-prefix-p side-prefix decoded))
      (magnus-review-normalize-repository-path
       (substring decoded (length side-prefix))))))

(defun magnus-review-canonical-patch-arguments (base head)
  "Return Git arguments for canonical patch evidence from BASE to HEAD."
  (list "diff" "--binary" "--full-index" "--no-ext-diff" "--no-color"
        "--src-prefix=a/" "--dst-prefix=b/" "--find-renames"
        base head "--"))

;;; Completed lineage records

(cl-defstruct (magnus-review-round
               (:constructor magnus-review-round--create)
               (:copier nil))
  "One successfully completed immutable Git review round.
Before publication the same structure represents an ephemeral candidate, with
nil completion, verdict, and read state."
  number
  base-oid
  head-oid
  created-at
  completed-at
  verdict
  read-state
  metadata)

(cl-defstruct (magnus-review
               (:constructor magnus-review--create)
               (:copier nil))
  "A review lineage containing only successfully completed rounds."
  id
  project-root
  project-hash
  author-instance-id
  author-name
  reviewer-name
  reviewer-provider
  model
  effort
  task
  session-id
  metadata
  (lifecycle 'open)
  created-at
  updated-at
  archived-at
  rounds)

(defvar magnus-reviews nil
  "Completed lineages and ephemeral drafts known to this Emacs session.")

(defvar magnus-reviews-changed-hook nil
  "Hook run after the in-memory review registry changes.")

(defvar magnus-review-runtime-state-function nil
  "Optional function returning ephemeral execution state for one review.
The function receives REVIEW and should return a state symbol or nil.  Magnus
does not persist its answer.")

;;; Validation and managed paths

(defun magnus-review--signal (format-string &rest args)
  "Signal `magnus-review-error' with FORMAT-STRING and ARGS."
  (signal 'magnus-review-error (list (apply #'format format-string args))))

(defun magnus-review--valid-id-p (value)
  "Return non-nil when VALUE is a safe review identifier."
  (and (stringp value)
       (<= 1 (length value) 128)
       (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" value)))

(defun magnus-review--valid-hash-p (value)
  "Return non-nil when VALUE is a SHA-256 project hash."
  (and (stringp value)
       (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" value)))

(defun magnus-review--valid-oid-p (value)
  "Return non-nil when VALUE is a full SHA-1 or SHA-256 Git object ID."
  (and (stringp value)
       (string-match-p
        "\\`\\(?:[[:xdigit:]]\\{40\\}\\|[[:xdigit:]]\\{64\\}\\)\\'"
        value)))

(defun magnus-review--canonical-directory (directory)
  "Return a canonical local form of DIRECTORY."
  (unless (and (stringp directory) (file-name-absolute-p directory))
    (magnus-review--signal "Directory must be an absolute path: %S" directory))
  (when (file-remote-p directory)
    (magnus-review--signal "Remote review directories are not supported: %s"
                           directory))
  (directory-file-name (file-truename (expand-file-name directory))))

(defun magnus-review-compute-project-hash (project-root)
  "Return the stable storage hash for PROJECT-ROOT."
  (secure-hash 'sha256 (magnus-review--canonical-directory project-root)))

(defun magnus-review--safe-component (component)
  "Return COMPONENT or signal if it is unsafe in a managed path."
  (unless (magnus-review--valid-id-p component)
    (magnus-review--signal "Unsafe review path component: %S" component))
  component)

(defun magnus-review--child-path (parent component)
  "Return safe COMPONENT below PARENT."
  (magnus-review--safe-component component)
  (let* ((parent (file-name-as-directory (expand-file-name parent)))
         (child (expand-file-name component parent)))
    (unless (string-prefix-p parent child)
      (magnus-review--signal "Review path escapes managed directory: %s"
                             child))
    child))

(defun magnus-review-directory (review)
  "Return REVIEW's private directory without creating it."
  (unless (magnus-review-p review)
    (magnus-review--signal "Not a Magnus review: %S" review))
  (unless (magnus-review--valid-hash-p (magnus-review-project-hash review))
    (magnus-review--signal "Invalid project hash in review %s"
                           (magnus-review-id review)))
  (magnus-review--child-path
   (magnus-review--child-path magnus-review-directory-root
                              (magnus-review-project-hash review))
   (magnus-review-id review)))

(defun magnus-review-manifest-path (review)
  "Return REVIEW's completed-lineage manifest path."
  (expand-file-name "manifest.json" (magnus-review-directory review)))

(defun magnus-review--positive-number (value kind)
  "Return positive integer VALUE or signal, naming it KIND."
  (unless (and (integerp value) (> value 0))
    (magnus-review--signal "Invalid %s number: %S" kind value))
  value)

(defun magnus-review--round-component (round)
  "Return the collision-free managed path component for ROUND."
  (unless (magnus-review-round-p round)
    (magnus-review--signal "Not a Magnus review round: %S" round))
  (magnus-review--positive-number
   (magnus-review-round-number round) "round")
  (unless (and (magnus-review--valid-oid-p
                (magnus-review-round-base-oid round))
               (magnus-review--valid-oid-p
                (magnus-review-round-head-oid round)))
    (magnus-review--signal "Review round has invalid Git evidence"))
  (format "%03d-%s-%s"
          (magnus-review-round-number round)
          (downcase (magnus-review-round-base-oid round))
          (downcase (magnus-review-round-head-oid round))))

(defun magnus-review-round-directory (review round)
  "Return the artifact directory for REVIEW candidate or completed ROUND."
  (magnus-review--child-path
   (expand-file-name "rounds" (magnus-review-directory review))
   (magnus-review--round-component round)))

(defun magnus-review-round-checkout-path (review round)
  "Return REVIEW's isolated detached worktree path for ROUND."
  (magnus-review--child-path
   (expand-file-name "checkouts" (magnus-review-directory review))
   (magnus-review--round-component round)))

(defun magnus-review-round-result-path (review round)
  "Return the canonical structured result path for REVIEW ROUND."
  (expand-file-name "result.json"
                    (magnus-review-round-directory review round)))

(defun magnus-review-round-report-path (review round)
  "Return the rendered Markdown report path for REVIEW ROUND."
  (expand-file-name "report.md"
                    (magnus-review-round-directory review round)))

(defun magnus-review-round-patch-path (review round)
  "Return the immutable committed patch path for REVIEW ROUND."
  (expand-file-name "evidence.patch"
                    (magnus-review-round-directory review round)))

(defun magnus-review-round-name-status-path (review round)
  "Return the NUL-delimited changed-path evidence path for REVIEW ROUND."
  (expand-file-name "name-status.z"
                    (magnus-review-round-directory review round)))

(defun magnus-review--ensure-private-directory (directory)
  "Create DIRECTORY if needed and require a private real directory."
  (setq directory (directory-file-name (expand-file-name directory)))
  (when (file-remote-p directory)
    (magnus-review--signal "Managed review path may not be remote: %s"
                           directory))
  (when (file-symlink-p directory)
    (magnus-review--signal "Refusing symlinked review directory: %s"
                           directory))
  (if (file-exists-p directory)
      (unless (file-directory-p directory)
        (magnus-review--signal "Review path is not a directory: %s"
                               directory))
    (make-directory directory t))
  (set-file-modes directory #o700)
  directory)

(defun magnus-review--ensure-review-directories (review &optional round)
  "Create private directories for REVIEW and optional ROUND."
  (let* ((root (directory-file-name
                (expand-file-name magnus-review-directory-root)))
         (project (magnus-review--child-path
                   root (magnus-review-project-hash review)))
         (directory (magnus-review-directory review)))
    (dolist (path (list root project directory
                        (expand-file-name "rounds" directory)
                        (expand-file-name "checkouts" directory)))
      (magnus-review--ensure-private-directory path))
    (when round
      (magnus-review--ensure-private-directory
       (magnus-review-round-directory review round)))
    directory))

(defun magnus-review--atomic-write-string (file contents &optional coding)
  "Write CONTENTS atomically to FILE with mode 0600.
CODING defaults to `utf-8-unix'; use `no-conversion' for Git byte evidence."
  (let ((directory (file-name-directory file)) temporary write-error)
    (magnus-review--ensure-private-directory directory)
    (when (file-symlink-p file)
      (magnus-review--signal "Refusing to overwrite symlink: %s" file))
    (when (file-directory-p file)
      (magnus-review--signal "Refusing to overwrite directory: %s" file))
    (setq temporary
          (make-temp-file (expand-file-name ".magnus-review-tmp-" directory)))
    (unwind-protect
        (condition-case err
            (progn
              (let ((coding-system-for-write (or coding 'utf-8-unix)))
                (write-region contents nil temporary nil 'quiet))
              (set-file-modes temporary #o600)
              (rename-file temporary file t)
              (setq temporary nil))
          (error (setq write-error err)))
      (when (and temporary (file-exists-p temporary))
        (condition-case cleanup-error
            (delete-file temporary)
          (error
           (unless write-error
             (setq write-error cleanup-error))))))
    (when write-error
      (signal (car write-error) (cdr write-error)))))

(defun magnus-review-prepare-artifact-path (review path)
  "Prepare private directories for REVIEW artifact PATH and return PATH."
  (let* ((root (file-name-as-directory
                (expand-file-name "rounds" (magnus-review-directory review))))
         (path (expand-file-name path)))
    (unless (string-prefix-p root path)
      (magnus-review--signal "Artifact escapes review directory: %s" path))
    (magnus-review--ensure-review-directories review)
    (magnus-review--ensure-private-directory (file-name-directory path))
    path))

(defun magnus-review--read-artifact-bytes (file)
  "Return exact bytes from regular, non-symlink artifact FILE."
  (when (or (file-symlink-p file) (not (file-regular-p file)))
    (magnus-review--signal "Unsafe review artifact: %s" file))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally file))
    (buffer-string)))

(defun magnus-review--completed-artifact-path-p (review path)
  "Return non-nil when PATH belongs to a completed round of REVIEW."
  (let ((path (expand-file-name path)))
    (cl-some
     (lambda (round)
       (member path
               (mapcar #'expand-file-name
                       (list (magnus-review-round-result-path review round)
                             (magnus-review-round-report-path review round)
                             (magnus-review-round-patch-path review round)
                             (magnus-review-round-name-status-path
                              review round)))))
     (magnus-review-rounds review))))

(defun magnus-review-write-artifact
    (review path contents &optional coding replace)
  "Atomically write private REVIEW artifact CONTENTS to PATH.
An identical file is adopted.  REPLACE permits replacing an ephemeral
candidate artifact, but never an artifact belonging to a completed round."
  (setq path (magnus-review-prepare-artifact-path review path))
  (let* ((exists (file-exists-p path))
         (expected (if (eq coding 'no-conversion)
                       contents
                     (encode-coding-string contents
                                           (or coding 'utf-8-unix))))
         (identical (and exists
                         (equal (magnus-review--read-artifact-bytes path)
                                expected))))
    (when (and exists (not identical)
               (or (not replace)
                   (magnus-review--completed-artifact-path-p review path)))
      (magnus-review--signal
       "Refusing to overwrite durable review artifact: %s" path))
    (unless identical
      (magnus-review--atomic-write-string path contents coding)))
  (set-file-modes path #o600)
  path)

;;; Git scope and evidence

(defun magnus-review--git-output (directory &rest arguments)
  "Run Git in DIRECTORY with ARGUMENTS and return trimmed output."
  (unless (executable-find "git")
    (signal 'magnus-review-git-error (list "Git executable not found")))
  (let ((directory (magnus-review--canonical-directory directory))
        (process-environment (cons "LC_ALL=C" process-environment)))
    (unless (file-directory-p directory)
      (signal 'magnus-review-git-error
              (list (format "Git directory does not exist: %s" directory))))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil
                           "-C" directory arguments)))
        (let ((output (string-trim-right (buffer-string))))
          (unless (and (integerp status) (zerop status))
            (signal 'magnus-review-git-error
                    (list (if (string-empty-p output)
                              (format "Git failed (%s): git %s"
                                      status (string-join arguments " "))
                            output))))
          output)))))

(defun magnus-review--git-output-optional (directory &rest arguments)
  "Like `magnus-review--git-output', returning nil when Git fails."
  (condition-case nil
      (apply #'magnus-review--git-output directory arguments)
    (magnus-review-git-error nil)))

(defun magnus-review--git-output-raw (directory &rest arguments)
  "Run Git in DIRECTORY and return exact unibyte stdout."
  (unless (executable-find "git")
    (signal 'magnus-review-git-error (list "Git executable not found")))
  (let ((directory (magnus-review--canonical-directory directory))
        (process-environment (cons "LC_ALL=C" process-environment))
        (coding-system-for-read 'no-conversion))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((status (apply #'process-file "git" nil t nil
                           "-C" directory arguments)))
        (unless (and (integerp status) (zerop status))
          (signal 'magnus-review-git-error
                  (list (format "Git failed (%s): git %s"
                                status (string-join arguments " ")))))
        (buffer-string)))))

(defun magnus-review-git-root (directory)
  "Return the canonical Git worktree root containing DIRECTORY."
  (magnus-review--canonical-directory
   (magnus-review--git-output directory "rev-parse" "--show-toplevel")))

(defun magnus-review--git-common-directory (directory)
  "Return canonical common Git metadata directory for DIRECTORY."
  (let ((common (magnus-review--git-output
                 directory "rev-parse" "--git-common-dir")))
    (magnus-review--canonical-directory
     (if (file-name-absolute-p common)
         common
       (expand-file-name common directory)))))

(defun magnus-review--validate-revision (revision)
  "Reject unsafe or ambiguous Git REVISION strings."
  (unless (and (stringp revision)
               (<= 1 (length revision) 512)
               (not (string-prefix-p "-" revision))
               (not (string-match-p "[\0\n\r]" revision)))
    (magnus-review--signal "Invalid Git revision: %S" revision))
  revision)

(defun magnus-review-resolve-oid (project-root revision)
  "Resolve REVISION in PROJECT-ROOT to an exact commit object ID."
  (magnus-review--validate-revision revision)
  (let ((oid (downcase
              (magnus-review--git-output
               project-root "rev-parse" "--verify"
               (concat revision "^{commit}")))))
    (unless (magnus-review--valid-oid-p oid)
      (signal 'magnus-review-git-error
              (list (format "Git returned an invalid commit ID for %s: %S"
                            revision oid))))
    oid))

(defun magnus-review-base-ancestor-p (project-root base head)
  "Return non-nil when commit BASE is an ancestor of commit HEAD."
  (let ((base (magnus-review-resolve-oid project-root base))
        (head (magnus-review-resolve-oid project-root head)))
    (condition-case nil
        (progn
          (magnus-review--git-output project-root
                                     "merge-base" "--is-ancestor" base head)
          t)
      (magnus-review-git-error nil))))

(defvar magnus-coord-file)
(defvar magnus-coord-instructions-file)

(defun magnus-review--control-pathspecs (project-root)
  "Return literal Git exclusions for Magnus control files in PROJECT-ROOT."
  (let ((root (file-name-as-directory (expand-file-name project-root))) paths)
    (dolist (configured
             (list (if (boundp 'magnus-coord-file)
                       magnus-coord-file
                     ".magnus-coord.md")
                   (if (boundp 'magnus-coord-instructions-file)
                       magnus-coord-instructions-file
                     ".claude/magnus-instructions.md")))
      (when (and (stringp configured)
                 (not (string-empty-p configured))
                 (not (string-match-p "[\0\n\r]" configured)))
        (let* ((absolute (expand-file-name configured root))
               (relative (file-relative-name absolute root)))
          (when (and (not (file-name-absolute-p relative))
                     (not (string-match-p
                           "\\`\\.\\.?\\(?:/\\|\\'\\)" relative)))
            (push (concat ":(top,literal,exclude)" relative) paths)))))
    (delete-dups (nreverse paths))))

(defun magnus-review-worktree-dirty-status (project-root)
  "Return reviewable porcelain status for PROJECT-ROOT, or nil when clean."
  (let ((status
         (apply #'magnus-review--git-output
                project-root "status" "--porcelain=v1"
                "--untracked-files=normal" "--" "."
                (magnus-review--control-pathspecs project-root))))
    (unless (string-empty-p status) status)))

(defun magnus-review--managed-worktree-dirty-status (checkout)
  "Return every non-committed path in managed CHECKOUT, including ignored."
  (let ((status (magnus-review--git-output
                 checkout "status" "--porcelain=v1"
                 "--untracked-files=all" "--ignored")))
    (unless (string-empty-p status) status)))

(defconst magnus-review-uncommitted-message
  "work is uncommitted. Ask instance to commit first"
  "Message shown when exact committed review evidence cannot be captured.")

(defun magnus-review-inspect-scope (project-root base-revision head-revision)
  "Return validated Git evidence for BASE-REVISION..HEAD-REVISION."
  (let* ((project-root (magnus-review-git-root project-root))
         (base (magnus-review-resolve-oid project-root base-revision))
         (head (magnus-review-resolve-oid project-root head-revision)))
    (unless (magnus-review-base-ancestor-p project-root base head)
      (signal 'magnus-review-git-error
              (list (format "Review base %s is not an ancestor of head %s"
                            base head))))
    (let* ((dirty-status (magnus-review-worktree-dirty-status project-root))
           (name-output
            (magnus-review--git-output-raw
             project-root "diff" "--name-only" "-z" "--no-ext-diff"
             base head "--"))
           (changed-files
            (mapcar (lambda (path)
                      (decode-coding-string path 'utf-8-unix))
                    (split-string name-output "\0" t)))
           (diffstat (magnus-review--git-output
                      project-root "diff" "--stat" "--no-color"
                      "--no-ext-diff" base head "--"))
           (shortstat (magnus-review--git-output
                       project-root "diff" "--shortstat" "--no-color"
                       "--no-ext-diff" base head "--"))
           (commit-count
            (string-to-number
             (magnus-review--git-output
              project-root "rev-list" "--count" (concat base ".." head)))))
      (list :project-root project-root
            :base-oid base
            :head-oid head
            :ancestor-p t
            :commit-count commit-count
            :changed-file-count (length changed-files)
            :changed-files changed-files
            :diffstat diffstat
            :shortstat shortstat
            :dirty-p (and dirty-status t)
            :dirty-status dirty-status
            :dirty-warning (and dirty-status
                                magnus-review-uncommitted-message)))))

(defun magnus-review-capture-round-evidence (review round)
  "Persist immutable Git patch and changed-path evidence for REVIEW ROUND."
  (let* ((project-root (magnus-review-project-root review))
         (base (magnus-review-round-base-oid round))
         (head (magnus-review-round-head-oid round))
         (patch-path (magnus-review-round-patch-path review round))
         (name-status-path
          (magnus-review-round-name-status-path review round))
         (patch (apply #'magnus-review--git-output-raw project-root
                       (magnus-review-canonical-patch-arguments base head)))
         (name-status
          (magnus-review--git-output-raw
           project-root "diff" "--name-status" "-z" "--find-renames"
           "--no-ext-diff" base head "--")))
    (when (> (length patch) magnus-review-max-evidence-bytes)
      (magnus-review--signal
       "Review patch is %d bytes; configured limit is %d"
       (length patch) magnus-review-max-evidence-bytes))
    (magnus-review--ensure-review-directories review round)
    (cl-mapc
     (lambda (path contents)
       (if (file-exists-p path)
           (unless (equal (magnus-review--read-artifact-bytes path) contents)
             (magnus-review--signal
              "Existing round evidence does not match Git scope: %s" path))
         (magnus-review--atomic-write-string path contents 'no-conversion)))
     (list patch-path name-status-path)
     (list patch name-status))
    (list :patch patch-path :name-status name-status-path)))

;;; Detached candidate worktrees

(defun magnus-review--verified-clean-checkout-head (review checkout)
  "Return proven clean CHECKOUT head for REVIEW, or nil when absent."
  (cond
   ((file-symlink-p checkout)
    (magnus-review--signal "Refusing symlinked review checkout: %s" checkout))
   ((not (file-exists-p checkout)) nil)
   ((not (file-directory-p checkout))
    (magnus-review--signal "Review checkout is not a directory: %s" checkout))
   (t
    (condition-case error-data
        (progn
          (unless (string= (magnus-review-git-root checkout)
                           (magnus-review--canonical-directory checkout))
            (magnus-review--signal
             "Review checkout is not its exact Git top-level: %s" checkout))
          (unless (string=
                   (magnus-review--git-common-directory checkout)
                   (magnus-review--git-common-directory
                    (magnus-review-project-root review)))
            (magnus-review--signal
             "Review checkout belongs to a different repository: %s" checkout))
          (when (magnus-review--git-output-optional
                 checkout "symbolic-ref" "--quiet" "HEAD")
            (magnus-review--signal
             "Review checkout is not detached: %s" checkout))
          (when-let ((dirty
                      (magnus-review--managed-worktree-dirty-status checkout)))
            (magnus-review--signal
             "Refusing modified review checkout %s:\n%s" checkout dirty))
          (magnus-review-resolve-oid checkout "HEAD"))
      (magnus-review-git-error
       (magnus-review--signal
        "Cannot prove review checkout ownership at %s: %s"
        checkout (error-message-string error-data)))))))

(defun magnus-review-ensure-checkout (review head-revision round)
  "Ensure an isolated clean detached checkout for REVIEW ROUND at HEAD-REVISION."
  (unless (magnus-review-round-p round)
    (magnus-review--signal "A review round is required for isolated checkout"))
  (let* ((project-root (magnus-review-project-root review))
         (head (magnus-review-resolve-oid project-root head-revision))
         (checkout (magnus-review-round-checkout-path review round)))
    (unless (string= head (magnus-review-round-head-oid round))
      (magnus-review--signal "Checkout HEAD does not match candidate round"))
    (magnus-review--ensure-review-directories review)
    (magnus-review--ensure-private-directory (file-name-directory checkout))
    (let ((current (magnus-review--verified-clean-checkout-head
                    review checkout)))
      (cond
       ((and current (not (string= current head)))
        (magnus-review--signal
         "Immutable review checkout has unexpected HEAD %s" current))
       ((null current)
        (condition-case error-data
            (magnus-review--git-output
             project-root "worktree" "add" "--detach" checkout head)
          (error
           (unless
               (condition-case verification-error
                   (string=
                    (or (magnus-review--verified-clean-checkout-head
                         review checkout)
                        "")
                    head)
                 (error
                  (message
                   "Magnus: concurrent checkout validation failed: %s"
                   (error-message-string verification-error))
                  nil))
             (signal (car error-data) (cdr error-data))))))))
    (unless (string=
             (or (magnus-review--verified-clean-checkout-head review checkout)
                 "")
             head)
      (magnus-review--signal "Review checkout stopped at unexpected commit"))
    (set-file-modes checkout #o700)
    checkout))

(defun magnus-review--round-storage-chain (review round kind)
  "Return managed directory chain for REVIEW ROUND storage KIND.
KIND is either `round' or `checkout'."
  (let* ((root (directory-file-name
                (expand-file-name magnus-review-directory-root)))
         (project (magnus-review--child-path
                   root (magnus-review-project-hash review)))
         (directory (magnus-review-directory review))
         (bucket
          (expand-file-name
           (pcase kind
             ('round "rounds")
             ('checkout "checkouts")
             (_ (magnus-review--signal
                 "Invalid review storage kind: %S" kind)))
           directory))
         (leaf
          (pcase kind
            ('round (magnus-review-round-directory review round))
            ('checkout (magnus-review-round-checkout-path review round)))))
    (list root project directory bucket leaf)))

(defun magnus-review--assert-safe-directory-chain (paths)
  "Refuse symlinks and non-directories among existing managed PATHS."
  (dolist (path paths)
    (when (file-symlink-p path)
      (magnus-review--signal "Refusing symlinked review path: %s" path))
    (when (and (file-exists-p path) (not (file-directory-p path)))
      (magnus-review--signal "Review path is not a directory: %s" path))))

(defun magnus-review--assert-unpublished-candidate (review round)
  "Require ROUND to be an unpublished candidate belonging below REVIEW.
Path identity is checked as well as object identity so a copied or forged
round cannot target the artifacts of a completed lineage round."
  (unless (magnus-review-p review)
    (magnus-review--signal "Not a Magnus review: %S" review))
  (unless (magnus-review-round-p round)
    (magnus-review--signal "Not a Magnus review round: %S" round))
  ;; Computing the directory validates every path-forming round field before
  ;; any deletion is considered.
  (let ((candidate-directory
         (expand-file-name (magnus-review-round-directory review round))))
    (when (or (magnus-review-round-completed-at round)
              (magnus-review-round-verdict round)
              (magnus-review-round-read-state round)
              (cl-some
               (lambda (completed)
                 (string=
                  candidate-directory
                  (expand-file-name
                   (magnus-review-round-directory review completed))))
               (magnus-review-rounds review)))
      (magnus-review--signal "Refusing to discard a completed review round")))
  round)

(defun magnus-review--preflight-round-directory (review round)
  "Validate REVIEW ROUND's candidate artifact directory for safe deletion."
  (let* ((chain (magnus-review--round-storage-chain review round 'round))
         (directory (car (last chain))))
    (magnus-review--assert-safe-directory-chain chain)
    (when (file-directory-p directory)
      ;; Candidate artifacts are deliberately flat.  Refusing anything other
      ;; than ordinary files keeps recursive deletion from acquiring surprising
      ;; semantics later.
      (dolist (entry (directory-files directory t
                                      directory-files-no-dot-files-regexp))
        (when (file-symlink-p entry)
          (magnus-review--signal
           "Refusing symlinked candidate artifact: %s" entry))
        (unless (file-regular-p entry)
          (magnus-review--signal
           "Refusing unexpected candidate artifact: %s" entry))))
    directory))

(defun magnus-review--preflight-round-checkout (review round)
  "Validate REVIEW ROUND's detached checkout for safe removal."
  (let* ((chain (magnus-review--round-storage-chain review round 'checkout))
         (checkout (car (last chain))))
    (magnus-review--assert-safe-directory-chain chain)
    (when (file-directory-p checkout)
      (let ((head
             (magnus-review--verified-clean-checkout-head review checkout)))
        (unless (string= head (magnus-review-round-head-oid round))
          (magnus-review--signal
           "Candidate checkout has unexpected HEAD %s" head))))
    checkout))

(defun magnus-review--remove-round-checkout (review round)
  "Strictly and idempotently remove REVIEW ROUND's verified checkout."
  (let ((checkout (magnus-review--preflight-round-checkout review round)))
    (when (file-directory-p checkout)
      (magnus-review--git-output
       (magnus-review-project-root review) "worktree" "remove" checkout))
    (when (or (file-symlink-p checkout) (file-exists-p checkout))
      (magnus-review--signal
       "Review checkout remains after removal: %s" checkout))
    t))

(defun magnus-review--remove-candidate-round-directory (review round)
  "Strictly and idempotently remove REVIEW ROUND's candidate artifacts."
  (let ((directory (magnus-review--preflight-round-directory review round)))
    (when (file-directory-p directory)
      (delete-directory directory t))
    (when (or (file-symlink-p directory) (file-exists-p directory))
      (magnus-review--signal
       "Candidate artifact directory remains after removal: %s" directory))
    t))

(defun magnus-review-cleanup-round-checkout (review round)
  "Best-effort removal of REVIEW ROUND's reproducible detached checkout.
Return non-nil when the checkout is absent or was removed.  Unsafe or modified
paths are preserved and reported rather than forced away."
  (condition-case err
      (magnus-review--remove-round-checkout review round)
    (error
     (message "Magnus: could not remove review checkout %s: %s"
              (magnus-review-round-checkout-path review round)
              (error-message-string err))
     nil)))

(defun magnus-review-discard-candidate (review round)
  "Safely discard unpublished candidate ROUND and its checkout from REVIEW.
Completed rounds, escaped managed paths, symlinks, modified checkouts, and
unexpected candidate artifacts are refused.  Repeating a successful discard is
safe and returns non-nil."
  (magnus-review--assert-unpublished-candidate review round)
  ;; Preflight both trees before mutating either.  This makes a refusal leave
  ;; every candidate artifact available for inspection.
  (magnus-review--preflight-round-checkout review round)
  (magnus-review--preflight-round-directory review round)
  (magnus-review--remove-round-checkout review round)
  (magnus-review--remove-candidate-round-directory review round)
  t)

;;; Ephemeral preparation and successful publication

(defun magnus-review-list ()
  "Return a copy of the loaded review list."
  (copy-sequence magnus-reviews))

(defun magnus-review-get (id)
  "Return the known review whose ID is ID."
  (and (stringp id)
       (cl-find id magnus-reviews :key #'magnus-review-id :test #'string=)))

(defun magnus-review-latest-round (review)
  "Return REVIEW's latest successfully completed round, or nil."
  (car (last (magnus-review-rounds review))))

(defun magnus-review-execution (review)
  "Return REVIEW's ephemeral runtime state or durable fallback state."
  (or (and (functionp magnus-review-runtime-state-function)
           (funcall magnus-review-runtime-state-function review))
      (if (magnus-review-rounds review) 'complete 'idle)))

(defun magnus-review-read-state (review)
  "Return aggregate read state for REVIEW's completed rounds."
  (let ((rounds (magnus-review-rounds review)))
    (cond ((null rounds) 'not-ready)
          ((cl-some (lambda (round)
                      (eq (magnus-review-round-read-state round) 'unread))
                    rounds)
           'unread)
          (t 'read))))

(defun magnus-review-reserved-instance-names (project-root)
  "Return reviewer identities reserved by open lineages in PROJECT-ROOT."
  (let ((project (magnus-review--canonical-directory project-root)) names)
    (dolist (review magnus-reviews (delete-dups names))
      (when (and (eq (magnus-review-lifecycle review) 'open)
                 (stringp (magnus-review-reviewer-name review))
                 (string= project
                          (magnus-review--canonical-directory
                           (magnus-review-project-root review))))
        (push (magnus-review-reviewer-name review) names)))))

(add-hook 'magnus-instances-name-reservation-functions
          #'magnus-review-reserved-instance-names)

(defun magnus-review--run-changed-hooks ()
  "Run review registry observers without turning failures into transactions."
  (run-hook-wrapped
   'magnus-reviews-changed-hook
   (lambda (function)
     (condition-case err
         (funcall function)
       (error
        (message "Magnus: review observer %S failed: %s"
                 function (error-message-string err))))
     nil)))

(defun magnus-review--random-id-source ()
  "Return locally unique random material for a review identifier."
  (secure-hash
   'sha256
   (format "%s:%s:%s:%s:%s"
           (float-time) (emacs-pid) (user-uid)
           (random most-positive-fixnum) (current-time-string))))

(defun magnus-review--generate-id (project-hash)
  "Generate an unused review ID below PROJECT-HASH."
  (let (candidate)
    (while
        (progn
          (setq candidate (substring (magnus-review--random-id-source) 0 32))
          (or (magnus-review-get candidate)
              (file-exists-p
               (magnus-review--child-path
                (magnus-review--child-path
                 magnus-review-directory-root project-hash)
                candidate)))))
    candidate))

(defun magnus-review--optional-symbol-value (value kind)
  "Return VALUE as a bounded optional symbol, naming it KIND."
  (cond
   ((null value) nil)
   ((symbolp value) value)
   ((and (stringp value)
         (<= 1 (length value) 100)
         (string-match-p "\\`[[:alnum:]_.-]+\\'" value))
    (intern value))
   (t (magnus-review--signal "Invalid %s: %S" kind value))))

(cl-defun magnus-review-create
    (project-root author-instance-id author-name
                  &key id task reviewer-name reviewer-provider model effort
                  metadata)
  "Register an unsaved review draft for AUTHOR-NAME in PROJECT-ROOT.
The draft becomes durable only when `magnus-review-complete-round' publishes a
successful first round."
  (let* ((project-root (magnus-review-git-root project-root))
         (project-hash (magnus-review-compute-project-hash project-root))
         (id (or id (magnus-review--generate-id project-hash)))
         (now (float-time)))
    (unless (magnus-review--valid-id-p id)
      (magnus-review--signal "Invalid review ID: %S" id))
    (when (or (magnus-review-get id)
              (file-exists-p
               (magnus-review--child-path
                (magnus-review--child-path
                 magnus-review-directory-root project-hash)
                id)))
      (magnus-review--signal "Review ID is already in use: %s" id))
    (unless (and (stringp author-instance-id)
                 (not (string-empty-p author-instance-id))
                 (stringp author-name)
                 (not (string-empty-p author-name)))
      (magnus-review--signal "Review author identity is incomplete"))
    (unless (and (stringp reviewer-name)
                 (not (string-empty-p reviewer-name)))
      (magnus-review--signal "Review reviewer identity is incomplete"))
    (let ((review
           (magnus-review--create
            :id id
            :project-root project-root
            :project-hash project-hash
            :author-instance-id author-instance-id
            :author-name author-name
            :reviewer-name reviewer-name
            :reviewer-provider
            (magnus-review--optional-symbol-value reviewer-provider "provider")
            :model model
            :effort (magnus-review--optional-symbol-value effort "effort")
            :task task
            :session-id nil
            :metadata metadata
            :lifecycle 'open
            :created-at now
            :updated-at now
            :rounds nil)))
      (push review magnus-reviews)
      (magnus-review--run-changed-hooks)
      review)))

(cl-defun magnus-review-prepare-round
    (review base-oid head-oid &key metadata)
  "Prepare and return an unpersisted candidate for REVIEW at BASE-OID..HEAD-OID.
Both arguments must be full commit IDs supplied by the author agent.  Git
validates the claim; Magnus requires a clean source worktree, captures immutable
evidence, and creates an isolated detached checkout.  The lineage is unchanged
until `magnus-review-complete-round' succeeds."
  (unless (and (magnus-review-p review)
               (eq (magnus-review-lifecycle review) 'open))
    (magnus-review--signal "Cannot prepare a round for a closed review"))
  (unless (and (magnus-review--valid-oid-p base-oid)
               (magnus-review--valid-oid-p head-oid))
    (magnus-review--signal
     "Review scope requires exact full base and head commit IDs"))
  (let* ((base-oid (downcase base-oid))
         (head-oid (downcase head-oid))
         (scope (magnus-review-inspect-scope
                 (magnus-review-project-root review) base-oid head-oid))
         (base (plist-get scope :base-oid))
         (head (plist-get scope :head-oid))
         (latest (magnus-review-latest-round review)))
    (unless (and (string= base base-oid) (string= head head-oid))
      (magnus-review--signal "Review scope must use exact commit object IDs"))
    (when (plist-get scope :dirty-p)
      (magnus-review--signal "%s" magnus-review-uncommitted-message))
    (when (or (string= base head)
              (zerop (plist-get scope :changed-file-count)))
      (magnus-review--signal "Review scope contains no committed changes"))
    (when (and latest
               (string= base (magnus-review-round-base-oid latest))
               (string= head (magnus-review-round-head-oid latest)))
      (magnus-review--signal
       "Scope %s..%s is already the latest review round" base head))
    (let* ((summary
            `((commit_count . ,(plist-get scope :commit-count))
              (changed_file_count . ,(plist-get scope :changed-file-count))
              (shortstat . ,(plist-get scope :shortstat))))
           (round
            (magnus-review-round--create
             :number (1+ (length (magnus-review-rounds review)))
             :base-oid base
             :head-oid head
             :created-at (float-time)
             :completed-at nil
             :verdict nil
             :read-state nil
             :metadata `((scope . ,summary) (request . ,metadata)))))
      (condition-case original-error
          (progn
            (magnus-review-capture-round-evidence review round)
            (magnus-review-ensure-checkout review head round)
            round)
        (error
         ;; Preparation is not durable.  Remove each safely removable half of
         ;; the candidate independently, but never replace the useful original
         ;; failure with a cleanup failure.
         (dolist (remover '(magnus-review--remove-round-checkout
                            magnus-review--remove-candidate-round-directory))
           (condition-case cleanup-error
               (funcall remover review round)
             (error
              (message "Magnus: candidate cleanup after preparation failure: %s"
                       (error-message-string cleanup-error)))))
         (signal (car original-error) (cdr original-error)))))))

(defun magnus-review--safe-regular-file-p (path)
  "Return non-nil when PATH is a regular, non-symlink file."
  (and (file-regular-p path) (not (file-symlink-p path))))

(cl-defun magnus-review-complete-round
    (review round verdict &key session-id metadata)
  "Publish successful candidate ROUND with VERDICT into REVIEW.
SESSION-ID, when non-nil, becomes the lineage's last successful provider
session.  This single transition is the only way a prepared round becomes
durable.  METADATA replaces the candidate metadata when supplied."
  (unless (and (magnus-review-p review)
               (eq (magnus-review-lifecycle review) 'open))
    (magnus-review--signal "Cannot complete a round for a closed review"))
  (unless (magnus-review-round-p round)
    (magnus-review--signal "Not a Magnus review round: %S" round))
  (unless (memq verdict magnus-review--verdict-states)
    (magnus-review--signal "Invalid review verdict: %S" verdict))
  (unless (= (magnus-review-round-number round)
             (1+ (length (magnus-review-rounds review))))
    (magnus-review--signal "Review candidate is obsolete"))
  (when (or (magnus-review-round-completed-at round)
            (magnus-review-round-verdict round)
            (magnus-review-round-read-state round)
            (memq round (magnus-review-rounds review)))
    (magnus-review--signal "Review candidate is already published"))
  (when-let ((latest (magnus-review-latest-round review)))
    (when (and (string= (magnus-review-round-base-oid latest)
                        (magnus-review-round-base-oid round))
               (string= (magnus-review-round-head-oid latest)
                        (magnus-review-round-head-oid round)))
      (magnus-review--signal "Review candidate duplicates the latest round")))
  (dolist (path (list (magnus-review-round-patch-path review round)
                      (magnus-review-round-name-status-path review round)
                      (magnus-review-round-result-path review round)
                      (magnus-review-round-report-path review round)))
    (unless (magnus-review--safe-regular-file-p path)
      (magnus-review--signal
       "Successful review artifact is missing or unsafe: %s" path))
    (set-file-modes path #o600))
  (when (and session-id
             (not (and (stringp session-id)
                       (not (string-empty-p session-id)))))
    (magnus-review--signal "Provider returned an invalid session ID"))
  (let ((now (float-time))
        (old-rounds (magnus-review-rounds review))
        (old-updated-at (magnus-review-updated-at review))
        (old-session-id (magnus-review-session-id review))
        (old-completed-at (magnus-review-round-completed-at round))
        (old-verdict (magnus-review-round-verdict round))
        (old-read-state (magnus-review-round-read-state round))
        (old-metadata (magnus-review-round-metadata round)))
    (condition-case err
        (progn
          (when metadata
            (setf (magnus-review-round-metadata round) metadata))
          (setf (magnus-review-round-completed-at round) now
                (magnus-review-round-verdict round) verdict
                (magnus-review-round-read-state round) 'unread
                (magnus-review-rounds review) (append old-rounds (list round))
                (magnus-review-updated-at review) now)
          (when session-id
            (setf (magnus-review-session-id review) session-id))
          (magnus-review-save review))
      (error
       ;; Publication has one local rollback boundary.  An I/O failure restores
       ;; the in-memory lineage and leaves the exact candidate retryable.
       (setf (magnus-review-rounds review) old-rounds
             (magnus-review-updated-at review) old-updated-at
             (magnus-review-session-id review) old-session-id
             (magnus-review-round-completed-at round) old-completed-at
             (magnus-review-round-verdict round) old-verdict
             (magnus-review-round-read-state round) old-read-state
             (magnus-review-round-metadata round) old-metadata)
       (signal (car err) (cdr err))))
    round))

(defun magnus-review-mark-read (review &optional round)
  "Mark latest or supplied completed ROUND of REVIEW read."
  (setq round (or round (magnus-review-latest-round review)))
  (unless (and round (memq round (magnus-review-rounds review)))
    (magnus-review--signal "Only a completed review round can be read"))
  (unless (eq (magnus-review-round-read-state round) 'read)
    (setf (magnus-review-round-read-state round) 'read
          (magnus-review-updated-at review) (float-time))
    (magnus-review-save review))
  round)

(defun magnus-review-archive (review)
  "Archive REVIEW without deleting its completed reports."
  (let ((state (and (functionp magnus-review-runtime-state-function)
                    (funcall magnus-review-runtime-state-function review))))
    (when (memq state '(asking-scope queued running))
      (magnus-review--signal "Cannot archive a running review")))
  (let ((now (float-time)))
    (setf (magnus-review-lifecycle review) 'archived
          (magnus-review-archived-at review) now
          (magnus-review-updated-at review) now)
    ;; A failed first run has no report to archive.  Removing that unsaved
    ;; draft avoids an archived status row which cannot be opened or resumed.
    (if (null (magnus-review-rounds review))
        (progn
          (setq magnus-reviews (delq review magnus-reviews))
          (magnus-review--run-changed-hooks))
      (magnus-review-save review))
    review))

;;; Strict completed-lineage persistence

(defun magnus-review--symbol-name (value)
  "Return VALUE's symbol name, preserve strings, or nil."
  (cond ((symbolp value) (and value (symbol-name value)))
        ((stringp value) value)
        (t nil)))

(defun magnus-review--json-safe-value (value)
  "Return a recursively JSON-serializable representation of VALUE."
  (cond
   ((or (null value) (stringp value) (numberp value)
        (eq value t) (eq value :json-false))
    value)
   ((vectorp value)
    (vconcat (mapcar #'magnus-review--json-safe-value (append value nil))))
   ((hash-table-p value)
    (let (entries)
      (maphash
       (lambda (key item)
         (unless (or (stringp key) (symbolp key))
           (magnus-review--signal "Invalid JSON metadata key: %S" key))
         (push (cons key (magnus-review--json-safe-value item)) entries))
       value)
      entries))
   ((and (consp value) (keywordp (car value)))
    (let ((remaining value) result)
      (while remaining
        (let ((key (pop remaining)))
          (unless (and (keywordp key) remaining)
            (magnus-review--signal "Malformed metadata plist: %S" value))
          (setq result
                (append result
                        (list key
                              (magnus-review--json-safe-value
                               (pop remaining)))))))
      result))
   ((and (listp value) (cl-every #'consp value))
    (mapcar
     (lambda (entry)
       (unless (or (stringp (car entry)) (symbolp (car entry)))
         (magnus-review--signal "Invalid JSON metadata key: %S" (car entry)))
       (cons (car entry) (magnus-review--json-safe-value (cdr entry))))
     value))
   ((listp value)
    (vconcat (mapcar #'magnus-review--json-safe-value value)))
   ((symbolp value) (symbol-name value))
   (t (magnus-review--signal "Metadata value is not JSON-safe: %S" value))))

(defun magnus-review--round-to-json (round)
  "Return strict JSON-ready data for completed ROUND."
  `((number . ,(magnus-review-round-number round))
    (base_oid . ,(magnus-review-round-base-oid round))
    (head_oid . ,(magnus-review-round-head-oid round))
    (created_at . ,(magnus-review-round-created-at round))
    (completed_at . ,(magnus-review-round-completed-at round))
    (verdict . ,(magnus-review--symbol-name
                 (magnus-review-round-verdict round)))
    (read_state . ,(magnus-review--symbol-name
                    (magnus-review-round-read-state round)))
    (metadata . ,(magnus-review--json-safe-value
                  (magnus-review-round-metadata round)))))

(defun magnus-review--to-json (review)
  "Return strict JSON-ready completed-lineage data for REVIEW."
  `((schema_version . ,magnus-review-schema-version)
    (id . ,(magnus-review-id review))
    (project_root . ,(magnus-review-project-root review))
    (project_hash . ,(magnus-review-project-hash review))
    (author_instance_id . ,(magnus-review-author-instance-id review))
    (author_name . ,(magnus-review-author-name review))
    (reviewer_name . ,(magnus-review-reviewer-name review))
    (reviewer_provider . ,(magnus-review--symbol-name
                           (magnus-review-reviewer-provider review)))
    (model . ,(magnus-review--symbol-name (magnus-review-model review)))
    (effort . ,(magnus-review--symbol-name (magnus-review-effort review)))
    (task . ,(magnus-review-task review))
    (session_id . ,(magnus-review-session-id review))
    (metadata . ,(magnus-review--json-safe-value
                  (magnus-review-metadata review)))
    (lifecycle . ,(magnus-review--symbol-name
                   (magnus-review-lifecycle review)))
    (created_at . ,(magnus-review-created-at review))
    (updated_at . ,(magnus-review-updated-at review))
    (archived_at . ,(magnus-review-archived-at review))
    (rounds . ,(vconcat
                (mapcar #'magnus-review--round-to-json
                        (magnus-review-rounds review))))))

(defun magnus-review--require-object-shape (object expected kind)
  "Require OBJECT to contain exactly EXPECTED symbol keys, naming it KIND."
  (unless (and (listp object)
               (cl-every (lambda (entry)
                           (and (consp entry) (symbolp (car entry))))
                         object))
    (magnus-review--signal "Invalid %s object in manifest" kind))
  (let* ((actual (mapcar #'car object))
         (actual-names (sort (mapcar #'symbol-name actual) #'string<))
         (expected-names
          (sort (mapcar #'symbol-name (copy-sequence expected)) #'string<)))
    (unless (and (= (length actual) (length (delete-dups actual)))
                 (equal actual-names expected-names))
      (magnus-review--signal "Unexpected %s fields in manifest: %S"
                             kind actual)))
  object)

(defun magnus-review--require-string (value kind &optional allow-nil)
  "Return string VALUE or signal, naming it KIND.
When ALLOW-NIL is non-nil, nil is accepted."
  (unless (or (and (stringp value) (not (string-empty-p value)))
              (and allow-nil (null value)))
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  value)

(defun magnus-review--require-number (value kind &optional allow-nil)
  "Return numeric VALUE or signal, naming it KIND."
  (unless (or (numberp value) (and allow-nil (null value)))
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  value)

(defun magnus-review--require-oid (value kind)
  "Return lowercase full OID VALUE or signal, naming it KIND."
  (unless (magnus-review--valid-oid-p value)
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  (downcase value))

(defun magnus-review--validate-state (value states kind)
  "Parse string VALUE as one of STATES, naming it KIND."
  (let ((symbol (and (stringp value) (intern-soft value))))
    (unless (memq symbol states)
      (magnus-review--signal "Invalid %s in manifest: %S" kind value))
    symbol))

(defun magnus-review--optional-symbol (value kind)
  "Parse bounded optional symbol string VALUE, naming it KIND."
  (when value
    (magnus-review--require-string value kind)
    (unless (and (<= (length value) 100)
                 (string-match-p "\\`[[:alnum:]_.-]+\\'" value))
      (magnus-review--signal "Invalid %s in manifest: %S" kind value))
    (intern value)))

(defun magnus-review--round-from-json (object expected-number)
  "Deserialize completed round OBJECT, requiring EXPECTED-NUMBER."
  (magnus-review--require-object-shape
   object
   '(number base_oid head_oid created_at completed_at verdict read_state
            metadata)
   "round")
  (let ((number (alist-get 'number object)))
    (unless (eql number expected-number)
      (magnus-review--signal "Review rounds are not sequential at %S" number))
    (magnus-review-round--create
     :number number
     :base-oid (magnus-review--require-oid
                (alist-get 'base_oid object) "round base OID")
     :head-oid (magnus-review--require-oid
                (alist-get 'head_oid object) "round head OID")
     :created-at (magnus-review--require-number
                  (alist-get 'created_at object) "round creation time")
     :completed-at (magnus-review--require-number
                    (alist-get 'completed_at object) "round completion time")
     :verdict (magnus-review--validate-state
               (alist-get 'verdict object)
               magnus-review--verdict-states "round verdict")
     :read-state (magnus-review--validate-state
                  (alist-get 'read_state object)
                  magnus-review--read-states "round read state")
     :metadata (alist-get 'metadata object))))

(defun magnus-review--from-json (object &optional expected-id expected-hash)
  "Deserialize strict completed-lineage OBJECT.
EXPECTED-ID and EXPECTED-HASH validate its managed storage identity."
  (magnus-review--require-object-shape
   object
   '(schema_version id project_root project_hash author_instance_id author_name
                    reviewer_name reviewer_provider model effort task session_id
                    metadata lifecycle created_at updated_at archived_at rounds)
   "review")
  (unless (eql (alist-get 'schema_version object)
               magnus-review-schema-version)
    (magnus-review--signal
     "Unsupported review schema version: %S"
     (alist-get 'schema_version object)))
  (let* ((id (alist-get 'id object))
         (project-root (alist-get 'project_root object))
         (project-hash (alist-get 'project_hash object))
         (number 0))
    (unless (magnus-review--valid-id-p id)
      (magnus-review--signal "Invalid review ID in manifest: %S" id))
    (magnus-review--require-string project-root "project root")
    (unless (and (magnus-review--valid-hash-p project-hash)
                 (string= project-hash
                          (magnus-review-compute-project-hash project-root)))
      (magnus-review--signal "Project hash does not match manifest root"))
    (when (and expected-id (not (string= expected-id id)))
      (magnus-review--signal "Manifest ID does not match its directory"))
    (when (and expected-hash (not (string= expected-hash project-hash)))
      (magnus-review--signal
       "Manifest project hash does not match its directory"))
    (magnus-review--validate-invariants
     (magnus-review--create
      :id id
      :project-root (magnus-review--canonical-directory project-root)
      :project-hash project-hash
      :author-instance-id
      (magnus-review--require-string
       (alist-get 'author_instance_id object) "author instance ID")
      :author-name
      (magnus-review--require-string
       (alist-get 'author_name object) "author name")
      :reviewer-name
      (magnus-review--require-string
       (alist-get 'reviewer_name object) "reviewer name")
      :reviewer-provider
      (magnus-review--optional-symbol
       (alist-get 'reviewer_provider object) "reviewer provider")
      :model (magnus-review--require-string
              (alist-get 'model object) "model" t)
      :effort (magnus-review--optional-symbol
               (alist-get 'effort object) "effort")
      :task (magnus-review--require-string
             (alist-get 'task object) "task" t)
      :session-id (magnus-review--require-string
                   (alist-get 'session_id object) "session ID" t)
      :metadata (alist-get 'metadata object)
      :lifecycle (magnus-review--validate-state
                  (alist-get 'lifecycle object)
                  magnus-review--lifecycle-states "lifecycle")
      :created-at (magnus-review--require-number
                   (alist-get 'created_at object) "creation time")
      :updated-at (magnus-review--require-number
                   (alist-get 'updated_at object) "update time")
      :archived-at (magnus-review--require-number
                    (alist-get 'archived_at object) "archive time" t)
      :rounds
      (mapcar (lambda (round)
                (magnus-review--round-from-json round (cl-incf number)))
              (append (alist-get 'rounds object) nil))))))

(defun magnus-review--validate-invariants (review)
  "Validate completed lineage invariants for REVIEW."
  (unless (and (magnus-review-p review)
               (magnus-review--valid-id-p (magnus-review-id review))
               (magnus-review--valid-hash-p
                (magnus-review-project-hash review))
               (string= (magnus-review-project-hash review)
                        (magnus-review-compute-project-hash
                         (magnus-review-project-root review))))
    (magnus-review--signal "Review storage identity is inconsistent"))
  (dolist (value (list (magnus-review-author-instance-id review)
                       (magnus-review-author-name review)
                       (magnus-review-reviewer-name review)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (magnus-review--signal "Review participant identity is incomplete")))
  (unless (memq (magnus-review-lifecycle review)
                magnus-review--lifecycle-states)
    (magnus-review--signal "Invalid review lifecycle"))
  (unless (and (numberp (magnus-review-created-at review))
               (numberp (magnus-review-updated-at review))
               (<= (magnus-review-created-at review)
                   (magnus-review-updated-at review)))
    (magnus-review--signal "Invalid review timestamps"))
  (if (eq (magnus-review-lifecycle review) 'archived)
      (unless (numberp (magnus-review-archived-at review))
        (magnus-review--signal "Archived review lacks archive timestamp"))
    (when (magnus-review-archived-at review)
      (magnus-review--signal "Open review has archive timestamp")))
  (when (and (magnus-review-session-id review)
             (not (and (stringp (magnus-review-session-id review))
                       (not (string-empty-p
                             (magnus-review-session-id review))))))
    (magnus-review--signal "Invalid successful provider session ID"))
  (let ((number 0))
    (dolist (round (magnus-review-rounds review))
      (unless (= (magnus-review-round-number round) (cl-incf number))
        (magnus-review--signal "Review rounds are not sequential"))
      (unless (and (magnus-review--valid-oid-p
                    (magnus-review-round-base-oid round))
                   (magnus-review--valid-oid-p
                    (magnus-review-round-head-oid round))
                   (not (string= (magnus-review-round-base-oid round)
                                 (magnus-review-round-head-oid round))))
        (magnus-review--signal "Round %d has invalid Git scope" number))
      (unless (and (numberp (magnus-review-round-created-at round))
                   (numberp (magnus-review-round-completed-at round))
                   (<= (magnus-review-round-created-at round)
                       (magnus-review-round-completed-at round)))
        (magnus-review--signal "Round %d has invalid timestamps" number))
      (unless (and (memq (magnus-review-round-verdict round)
                         magnus-review--verdict-states)
                   (memq (magnus-review-round-read-state round)
                         magnus-review--read-states))
        (magnus-review--signal "Round %d is not successfully published"
                               number))))
  review)

(defun magnus-review-save (review)
  "Persist completed REVIEW atomically and privately.
Empty drafts are intentionally not persistable."
  (unless (magnus-review-rounds review)
    (magnus-review--signal "Cannot persist a review without a completed round"))
  (magnus-review--validate-invariants review)
  (magnus-review--ensure-review-directories review)
  (let ((json-encoding-pretty-print nil))
    (magnus-review--atomic-write-string
     (magnus-review-manifest-path review)
     (concat (json-serialize (magnus-review--to-json review)
                             :null-object nil
                             :false-object :json-false)
             "\n")))
  (unless (memq review magnus-reviews)
    (push review magnus-reviews))
  (magnus-review--run-changed-hooks)
  review)

(defun magnus-review--read-json-file (file)
  "Read a bounded JSON object from regular, non-symlink FILE."
  (when (file-symlink-p file)
    (magnus-review--signal "Refusing symlinked review manifest: %s" file))
  (unless (file-regular-p file)
    (magnus-review--signal "Review manifest is not a regular file: %s" file))
  (when (> (file-attribute-size (file-attributes file)) (* 10 1024 1024))
    (magnus-review--signal "Review manifest is unexpectedly large: %s" file))
  (set-file-modes file #o600)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil :false-object :json-false)))

(defun magnus-review-load-file (file &optional expected-id expected-hash)
  "Load one strict completed-lineage manifest FILE and register it."
  (let ((review
         (magnus-review--from-json
          (magnus-review--read-json-file file) expected-id expected-hash)))
    (unless (magnus-review-rounds review)
      (magnus-review--signal "Durable review has no completed rounds"))
    (when-let ((existing (magnus-review-get (magnus-review-id review))))
      (unless (string= (magnus-review-project-hash existing)
                       (magnus-review-project-hash review))
        (magnus-review--signal "Duplicate review ID across projects"))
      (setq magnus-reviews (delq existing magnus-reviews)))
    (push review magnus-reviews)
    review))

(defun magnus-review-load-all ()
  "Load strict completed review lineages.
Malformed or unsupported manifests are ignored with a warning.  Unfinished
execution is never restored."
  (interactive)
  (setq magnus-reviews nil)
  (let ((root (expand-file-name magnus-review-directory-root))
        (loaded 0))
    (when (file-symlink-p root)
      (magnus-review--signal "Refusing symlinked reviews root: %s" root))
    (when (file-directory-p root)
      (set-file-modes root #o700)
      (dolist (project-entry (directory-files root t "\\`[^.]" t))
        (let ((project-hash (file-name-nondirectory project-entry)))
          (when (and (file-directory-p project-entry)
                     (not (file-symlink-p project-entry))
                     (magnus-review--valid-hash-p project-hash))
            (set-file-modes project-entry #o700)
            (dolist (review-entry
                     (directory-files project-entry t "\\`[^.]" t))
              (let* ((review-id (file-name-nondirectory review-entry))
                     (manifest
                      (expand-file-name "manifest.json" review-entry)))
                (when (and (file-directory-p review-entry)
                           (not (file-symlink-p review-entry))
                           (magnus-review--valid-id-p review-id)
                           (file-exists-p manifest))
                  (set-file-modes review-entry #o700)
                  (condition-case error-data
                      (progn
                        (magnus-review-load-file
                         manifest review-id project-hash)
                        (cl-incf loaded))
                    (error
                     (display-warning
                      'magnus-review
                      (format "Ignoring review manifest %s: %s"
                              manifest (error-message-string error-data))
                      :warning))))))))))
    (setq magnus-reviews
          (sort magnus-reviews
                (lambda (left right)
                  (> (or (magnus-review-created-at left) 0)
                     (or (magnus-review-created-at right) 0)))))
    (magnus-review--run-changed-hooks)
    loaded))

(provide 'magnus-review)
;;; magnus-review.el ends here
