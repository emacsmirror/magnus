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

(defcustom magnus-review-max-result-bytes (* 10 1024 1024)
  "Maximum bytes accepted for one structured review result or report."
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
(define-error 'magnus-review-stale-error "Stale Magnus review state"
  'magnus-review-error)
(define-error 'magnus-review-busy-error "Magnus review is being written"
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

;;; Ephemeral candidates and completed lineage records

(cl-defstruct (magnus-review-scope
               (:constructor nil)
               (:copier nil))
  "Shared immutable Git identity for a candidate or completed round."
  number
  base-oid
  head-oid
  created-at)

(cl-defstruct (magnus-review-candidate
               (:include magnus-review-scope)
               (:constructor magnus-review-candidate--create)
               (:copier nil))
  "One unpublished, process-local review candidate.
Candidates never appear in a manifest.  Their evidence digests pin the exact
bytes captured before the reviewer starts."
  patch-sha256
  name-status-sha256)

(cl-defstruct (magnus-review-round
               (:include magnus-review-scope)
               (:constructor magnus-review-round--create)
               (:copier nil))
  "One successfully published review round."
  completed-at
  verdict
  read-state
  finding-count
  result-sha256
  patch-sha256
  name-status-sha256)

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
  reviewer-expertise
  session-id
  (revision 0)
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

(defun magnus-review--scope-component (scope)
  "Return a bounded collision-resistant managed path component for SCOPE."
  (unless (magnus-review-scope-p scope)
    (magnus-review--signal "Not a Magnus review scope: %S" scope))
  (magnus-review--positive-number
   (magnus-review-scope-number scope) "round")
  (unless (and (magnus-review--valid-oid-p
                (magnus-review-scope-base-oid scope))
               (magnus-review--valid-oid-p
                (magnus-review-scope-head-oid scope)))
    (magnus-review--signal "Review round has invalid Git evidence"))
  (let* ((base (downcase (magnus-review-scope-base-oid scope)))
         (head (downcase (magnus-review-scope-head-oid scope)))
         (digest (secure-hash 'sha256 (concat base ":" head))))
    (format "%03d-%s-%s-%s"
            (magnus-review-scope-number scope)
            (substring base 0 8)
            (substring head 0 8)
            digest)))

(defun magnus-review-round-directory (review round)
  "Return the artifact directory for REVIEW candidate or completed ROUND."
  (magnus-review--child-path
   (expand-file-name "rounds" (magnus-review-directory review))
   (magnus-review--scope-component round)))

(defun magnus-review-round-checkout-path (review round)
  "Return REVIEW's isolated detached worktree path for ROUND."
  (magnus-review--child-path
   (expand-file-name "checkouts" (magnus-review-directory review))
   (magnus-review--scope-component round)))

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

(defun magnus-review--read-artifact-bytes
    (file &optional kind limit)
  "Return exact bytes from regular, non-symlink artifact FILE.
KIND names the artifact in diagnostics.  LIMIT bounds its byte size."
  (when (or (file-symlink-p file) (not (file-regular-p file)))
    (magnus-review--signal "Unsafe review %s artifact: %s"
                           (or kind "stored") file))
  (let ((size (file-attribute-size (file-attributes file 'string))))
    (unless (and (integerp size) (>= size 0)
                 (or (null limit) (<= size limit)))
      (magnus-review--signal "Review %s artifact exceeds %d bytes: %s"
                             (or kind "stored") (or limit 0) file)))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally file))
    (buffer-string)))

(defun magnus-review--verify-bytes (bytes digest kind)
  "Return BYTES when its SHA-256 equals DIGEST, naming artifact KIND."
  (unless (and (magnus-review--valid-sha256-p digest)
               (string= (secure-hash 'sha256 bytes) digest))
    (magnus-review--signal "Review %s artifact digest disagrees with manifest"
                           kind))
  bytes)

(defun magnus-review--verify-candidate-evidence (review candidate)
  "Verify CANDIDATE's exact patch and name-status bytes for REVIEW."
  (unless (magnus-review-candidate-p candidate)
    (magnus-review--signal "Evidence verification requires a candidate"))
  (list
   :patch
   (magnus-review--verify-bytes
    (magnus-review--read-artifact-bytes
     (magnus-review-round-patch-path review candidate)
     "patch" magnus-review-max-evidence-bytes)
    (magnus-review-candidate-patch-sha256 candidate) "patch")
   :name-status
   (magnus-review--verify-bytes
    (magnus-review--read-artifact-bytes
     (magnus-review-round-name-status-path review candidate)
     "name-status" magnus-review-max-evidence-bytes)
    (magnus-review-candidate-name-status-sha256 candidate) "name-status")))

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
  (unless (magnus-review-candidate-p round)
    (magnus-review--signal "Review evidence requires an ephemeral candidate"))
  (let* ((project-root (magnus-review-project-root review))
         (base (magnus-review-scope-base-oid round))
         (head (magnus-review-scope-head-oid round))
         (patch-path (magnus-review-round-patch-path review round))
         (name-status-path
          (magnus-review-round-name-status-path review round))
         (patch (apply #'magnus-review--git-output-raw project-root
                       (magnus-review-canonical-patch-arguments base head)))
         (name-status
          (magnus-review--git-output-raw
           project-root "diff" "--name-status" "-z" "--find-renames"
           "--no-ext-diff" base head "--")))
    (dolist (entry `(("patch" ,patch) ("name-status" ,name-status)))
      (when (> (length (cadr entry)) magnus-review-max-evidence-bytes)
        (magnus-review--signal
         "Review %s is %d bytes; configured limit is %d"
         (car entry) (length (cadr entry)) magnus-review-max-evidence-bytes)))
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
    (setf (magnus-review-candidate-patch-sha256 round)
          (secure-hash 'sha256 patch)
          (magnus-review-candidate-name-status-sha256 round)
          (secure-hash 'sha256 name-status))
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
  (unless (magnus-review-scope-p round)
    (magnus-review--signal "A review scope is required for isolated checkout"))
  (let* ((project-root (magnus-review-project-root review))
         (head (magnus-review-resolve-oid project-root head-revision))
         (checkout (magnus-review-round-checkout-path review round)))
    (unless (string= head (magnus-review-scope-head-oid round))
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
  (unless (magnus-review-candidate-p round)
    (magnus-review--signal "Not a Magnus review candidate: %S" round))
  ;; Computing the directory validates every path-forming round field before
  ;; any deletion is considered.
  (let ((candidate-directory
         (expand-file-name (magnus-review-round-directory review round))))
    (when (cl-some
               (lambda (completed)
                 (string=
                  candidate-directory
                  (expand-file-name
                   (magnus-review-round-directory review completed))))
               (magnus-review-rounds review))
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
        (unless (string= head (magnus-review-scope-head-oid round))
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
  (setq review (magnus-review-resolve-current review))
  (magnus-review--call-with-lineage-lock
   review
   (lambda ()
     ;; A stale process must be rejected before it can remove a directory that
     ;; another Emacs has just made durable.
     (magnus-review--assert-current-revision-locked review)
     (magnus-review--assert-unpublished-candidate review round)
     ;; Preflight both trees before mutating either.  This makes a refusal leave
     ;; every candidate artifact available for inspection.
     (magnus-review--preflight-round-checkout review round)
     (magnus-review--preflight-round-directory review round)
     (magnus-review--remove-round-checkout review round)
     (magnus-review--remove-candidate-round-directory review round)))
  t)

;;; Ephemeral preparation and successful publication

(defun magnus-review-list ()
  "Return a copy of the loaded review list."
  (copy-sequence magnus-reviews))

(defun magnus-review-get (id)
  "Return the known review whose ID is ID."
  (and (stringp id)
       (cl-find id magnus-reviews :key #'magnus-review-id :test #'string=)))

(defun magnus-review--open-for-author-matches
    (project-root author-instance-id &optional excluded-id)
  "Return loaded open reviews for AUTHOR-INSTANCE-ID in PROJECT-ROOT.
EXCLUDED-ID, when non-nil, omits that lineage identity."
  (let ((project-root (magnus-review--canonical-directory project-root)))
    (cl-remove-if-not
     (lambda (review)
       (and (eq (magnus-review-lifecycle review) 'open)
            (string= (magnus-review-project-root review) project-root)
            (string= (magnus-review-author-instance-id review)
                     author-instance-id)
            (or (null excluded-id)
                (not (string= (magnus-review-id review) excluded-id)))))
     magnus-reviews)))

(defun magnus-review-open-for-author (project-root author-instance-id)
  "Return AUTHOR-INSTANCE-ID's open review in PROJECT-ROOT, if any.
One author instance has at most one open lineage per project.  A changed task
therefore requires explicitly archiving the previous lineage instead of
silently creating a second reviewer conversation."
  (let ((matches
         (magnus-review--open-for-author-matches
          project-root author-instance-id)))
    (when (cdr matches)
      (magnus-review--signal
       "Author %s has multiple loaded open reviews in this project"
       author-instance-id))
    (car matches)))

(defun magnus-review-resolve-current (review)
  "Return REVIEW's current registry object, or signal on stale identity."
  (unless (magnus-review-p review)
    (magnus-review--signal "Not a Magnus review: %S" review))
  (let ((current (magnus-review-get (magnus-review-id review))))
    (unless current
      (magnus-review--signal "Review is no longer loaded: %s"
                             (magnus-review-id review)))
    (unless (string= (magnus-review-project-hash current)
                     (magnus-review-project-hash review))
      (magnus-review--signal "Review identity changed across projects"))
    current))

(defun magnus-review-resolve-round (review round)
  "Resolve completed ROUND by stable identity within current REVIEW."
  (let* ((review (magnus-review-resolve-current review))
         (number (cond ((magnus-review-round-p round)
                        (magnus-review-scope-number round))
                       ((integerp round) round)
                       (t nil)))
         (current
          (and (integerp number) (> number 0)
               (nth (1- number) (magnus-review-rounds review)))))
    (unless (and current (= (magnus-review-scope-number current) number))
      (magnus-review--signal "Completed review round is no longer available"))
    (when (magnus-review-round-p round)
      (unless (and (string= (magnus-review-scope-base-oid current)
                            (magnus-review-scope-base-oid round))
                   (string= (magnus-review-scope-head-oid current)
                            (magnus-review-scope-head-oid round)))
        (magnus-review--signal "Completed review round identity changed")))
    (cons review current)))

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

(defun magnus-review--bounded-string-value
    (value kind limit &optional allow-nil)
  "Return bounded string VALUE, naming it KIND, or signal.
ALLOW-NIL permits nil but never an empty string."
  (unless (or (and (stringp value)
                   (<= 1 (length value) limit))
              (and allow-nil (null value)))
    (magnus-review--signal "Invalid %s: %S" kind value))
  value)

(defun magnus-review--model-value (value)
  "Return VALUE as a bounded optional model string."
  (cond ((null value) nil)
        ((symbolp value)
         (magnus-review--bounded-string-value
          (symbol-name value) "model" 200))
        (t (magnus-review--bounded-string-value value "model" 200))))

(cl-defun magnus-review-create
    (project-root author-instance-id author-name
                  &key id task reviewer-name reviewer-provider model effort
                  reviewer-expertise)
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
    (magnus-review--bounded-string-value
     author-instance-id "author instance ID" 256)
    (magnus-review--bounded-string-value author-name "author name" 256)
    (magnus-review--bounded-string-value reviewer-name "reviewer name" 256)
    (when-let ((existing
                (magnus-review-open-for-author
                 project-root author-instance-id)))
      (magnus-review--signal
       "Author %s already has open review %s in this project"
       author-name (magnus-review-id existing)))
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
            :model (magnus-review--model-value model)
            :effort (magnus-review--optional-symbol-value effort "effort")
            :task (magnus-review--bounded-string-value task "task" 4000 t)
            :reviewer-expertise
            (magnus-review--bounded-string-value
             reviewer-expertise "reviewer expertise" 1200 t)
            :session-id nil
            :revision 0
            :lifecycle 'open
            :created-at now
            :updated-at now
            :rounds nil)))
      (push review magnus-reviews)
      (magnus-review--run-changed-hooks)
      review)))

(defun magnus-review-prepare-round (review base-oid head-oid)
  "Prepare and return an unpersisted candidate for REVIEW at BASE-OID..HEAD-OID.
Both arguments must be full commit IDs supplied by the author agent.  Git
validates the claim; Magnus requires a clean source worktree, captures immutable
evidence, and creates an isolated detached checkout.  The lineage is unchanged
until `magnus-review-complete-round' succeeds."
  (setq review (magnus-review-resolve-current review))
  (unless (eq (magnus-review-lifecycle review) 'open)
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
    (let ((round
            (magnus-review-candidate--create
             :number (1+ (length (magnus-review-rounds review)))
             :base-oid base
             :head-oid head
             :created-at (float-time)
             :patch-sha256 nil
             :name-status-sha256 nil)))
      (condition-case original-error
          (progn
            (magnus-review-capture-round-evidence review round)
            (magnus-review-ensure-checkout review head round)
            round)
        (error
         ;; Preparation is not durable.  Cleanup still uses the cross-process
         ;; freshness gate so it cannot race another Emacs's publication.
         (condition-case cleanup-error
             (magnus-review-discard-candidate review round)
           (error
            (message "Magnus: candidate cleanup after preparation failure: %s"
                     (error-message-string cleanup-error))))
         (signal (car original-error) (cdr original-error)))))))

(defun magnus-review--safe-regular-file-p (path)
  "Return non-nil when PATH is a regular, non-symlink file."
  (and (file-regular-p path) (not (file-symlink-p path))))

(defun magnus-review--lineage-lock-target (review)
  "Return the file-lock target for REVIEW's durable manifest."
  (magnus-review-manifest-path review))

(defun magnus-review--call-with-file-lock (target description function)
  "Call FUNCTION while exclusively locking TARGET for DESCRIPTION.
Use Emacs's native lock-file protocol so dead-process locks are recognized and
reclaimed instead of permanently wedging a lineage after a crash."
  (let ((create-lockfiles t)
        acquired)
    (cl-labels
        ((busy (owner)
           (signal
            'magnus-review-busy-error
            (list
             (format "%s is being written by another Emacs process%s"
                     description
                     (if (stringp owner) (format " (%s)" owner) ""))))))
      ;; `file-locked-p' removes a stale native lock.  The subsequent
      ;; `lock-file' remains the atomic acquisition and closes the check/create
      ;; race against another live Emacs.
      (when-let ((owner (file-locked-p target)))
        (busy owner))
      (condition-case err
          (cl-letf (((symbol-function 'ask-user-about-lock)
                     (lambda (_file owner) (busy owner))))
            (lock-file target)
            (setq acquired t))
        (magnus-review-busy-error
         (signal (car err) (cdr err)))
        (file-locked (busy (nth 2 err)))
        (error (signal (car err) (cdr err))))
      (unwind-protect
          (funcall function)
        ;; Unlocking follows the manifest commit point and may not turn a
        ;; successful publication into a reported failure.
        (when acquired
          (condition-case cleanup-error
              (unlock-file target)
            (error
             (message "Magnus: could not release %s lock: %s"
                      description
                      (error-message-string cleanup-error)))))))))

(defun magnus-review--call-with-lineage-lock (review function)
  "Call FUNCTION under REVIEW's exclusive cross-process write lock."
  (magnus-review--ensure-review-directories review)
  (magnus-review--call-with-file-lock
   (magnus-review--lineage-lock-target review)
   (format "Review %s" (magnus-review-id review))
   function))

(defun magnus-review--project-storage-directory (review)
  "Return REVIEW's private project-level storage directory."
  (magnus-review--child-path
   (expand-file-name magnus-review-directory-root)
   (magnus-review-project-hash review)))

(defun magnus-review--author-lock-target (review)
  "Return REVIEW's project/author uniqueness lock target."
  (expand-file-name
   (format ".author-%s"
           (secure-hash
            'sha256 (magnus-review-author-instance-id review)))
   (magnus-review--project-storage-directory review)))

(defun magnus-review--disk-open-for-author-locked (review)
  "Return another durable open lineage for REVIEW's author, if any.
The caller must hold REVIEW's project/author lock.  Malformed unrelated
lineages are warned about and ignored exactly as they are during normal load."
  (let ((project-directory (magnus-review--project-storage-directory review))
        found)
    (dolist (entry (directory-files project-directory t "\\`[^.]" t))
      (let* ((id (file-name-nondirectory entry))
             (manifest (expand-file-name "manifest.json" entry)))
        (when (and (not (string= id (magnus-review-id review)))
                   (magnus-review--valid-id-p id)
                   (file-directory-p entry)
                   (not (file-symlink-p entry))
                   (file-exists-p manifest))
          (condition-case error-data
              (let ((candidate
                     (magnus-review--from-json
                      (magnus-review--read-json-file manifest)
                      id (magnus-review-project-hash review))))
                (when (and
                       (eq (magnus-review-lifecycle candidate) 'open)
                       (string=
                        (magnus-review-author-instance-id candidate)
                        (magnus-review-author-instance-id review)))
                  (if found
                      (magnus-review--signal
                       "Author %s has multiple durable open reviews"
                       (magnus-review-author-name review))
                    (setq found candidate))))
            (error
             (display-warning
              'magnus-review
              (format "Ignoring review manifest %s while checking author ownership: %s"
                      manifest (error-message-string error-data))
              :warning))))))
    found))

(defun magnus-review--call-with-publication-locks
    (review candidate function)
  "Call FUNCTION with locks required to publish REVIEW CANDIDATE.
The first round also serializes the project/author invariant across Emacs
processes; later rounds need only the lineage revision lock."
  (if (= (magnus-review-scope-number candidate) 1)
      (progn
        (magnus-review--ensure-review-directories review)
        (magnus-review--call-with-file-lock
         (magnus-review--author-lock-target review)
         (format "Review author %s" (magnus-review-author-name review))
         (lambda ()
           (when-let ((existing
                       (magnus-review--disk-open-for-author-locked review)))
             (magnus-review--signal
              "Author %s already has durable open review %s in this project"
              (magnus-review-author-name review)
              (magnus-review-id existing)))
           (magnus-review--call-with-lineage-lock review function))))
    (magnus-review--call-with-lineage-lock review function)))

(cl-defun magnus-review-complete-round
    (review candidate verdict
            &key session-id result-json report finding-count)
  "Publish successful CANDIDATE with VERDICT into REVIEW.
SESSION-ID, when non-nil, becomes the lineage's last successful provider
session.  RESULT-JSON and REPORT are final UTF-8 artifact contents.
This transition owns final artifact replacement and the manifest commit."
  (setq review (magnus-review-resolve-current review))
  (unless (eq (magnus-review-lifecycle review) 'open)
    (magnus-review--signal "Cannot complete a round for a closed review"))
  (unless (magnus-review-candidate-p candidate)
    (magnus-review--signal "Not a Magnus review candidate: %S" candidate))
  (unless (memq verdict magnus-review--verdict-states)
    (magnus-review--signal "Invalid review verdict: %S" verdict))
  (unless (and (integerp finding-count) (>= finding-count 0))
    (magnus-review--signal "Invalid review finding count: %S" finding-count))
  (dolist (entry `(("structured result" ,result-json)
                   ("rendered report" ,report)))
    (unless (stringp (cadr entry))
      (magnus-review--signal "Missing %s contents" (car entry)))
    (when (> (string-bytes (encode-coding-string (cadr entry) 'utf-8-unix))
             magnus-review-max-result-bytes)
      (magnus-review--signal "%s exceeds %d bytes"
                             (capitalize (car entry))
                             magnus-review-max-result-bytes)))
  (unless (= (magnus-review-scope-number candidate)
             (1+ (length (magnus-review-rounds review))))
    (magnus-review--signal "Review candidate is obsolete"))
  (when-let ((latest (magnus-review-latest-round review)))
    (when (and (string= (magnus-review-scope-base-oid latest)
                        (magnus-review-scope-base-oid candidate))
               (string= (magnus-review-scope-head-oid latest)
                        (magnus-review-scope-head-oid candidate)))
      (magnus-review--signal "Review candidate duplicates the latest round")))
  (when (and session-id
             (not (and (stringp session-id) (<= 1 (length session-id) 1000))))
    (magnus-review--signal "Provider returned an invalid session ID"))
  (let (completed)
    (magnus-review--call-with-publication-locks
     review candidate
     (lambda ()
       ;; This freshness gate is deliberately before replaceable result/report
       ;; writes.  Detecting a stale writer after those writes is too late.
       (magnus-review--assert-current-revision-locked review)
       (magnus-review--assert-unpublished-candidate review candidate)
       (magnus-review--verify-candidate-evidence review candidate)
       (magnus-review-write-artifact
        review (magnus-review-round-result-path review candidate)
        result-json 'utf-8-unix t)
       (magnus-review-write-artifact
        review (magnus-review-round-report-path review candidate)
        report 'utf-8-unix t)
       (let* ((now (float-time))
              (result-bytes
               (magnus-review--read-artifact-bytes
                (magnus-review-round-result-path review candidate)
                "result" magnus-review-max-result-bytes))
              (old-rounds (magnus-review-rounds review))
              (old-updated-at (magnus-review-updated-at review))
              (old-session-id (magnus-review-session-id review))
              (old-revision (magnus-review-revision review)))
         (setq completed
               (magnus-review-round--create
                :number (magnus-review-scope-number candidate)
                :base-oid (magnus-review-scope-base-oid candidate)
                :head-oid (magnus-review-scope-head-oid candidate)
                :created-at (magnus-review-scope-created-at candidate)
                :completed-at now
                :verdict verdict
                :read-state 'unread
                :finding-count finding-count
                :result-sha256 (secure-hash 'sha256 result-bytes)
                :patch-sha256
                (magnus-review-candidate-patch-sha256 candidate)
                :name-status-sha256
                (magnus-review-candidate-name-status-sha256 candidate)))
         ;; Validate the exact durable result bytes before they become visible
         ;; through the manifest.
         (magnus-review-read-verified-result review completed)
         (condition-case err
             (progn
               (setf (magnus-review-rounds review)
                     (append old-rounds (list completed))
                     (magnus-review-updated-at review) now)
               (when session-id
                 (setf (magnus-review-session-id review) session-id))
               (magnus-review--save-locked review))
           (error
            (setf (magnus-review-rounds review) old-rounds
                  (magnus-review-updated-at review) old-updated-at
                  (magnus-review-session-id review) old-session-id
                  (magnus-review-revision review) old-revision)
            (signal (car err) (cdr err)))))))
    (magnus-review--run-changed-hooks)
    completed))

(defun magnus-review-mark-read (review &optional round)
  "Mark latest or supplied completed ROUND of REVIEW read."
  (setq review (magnus-review-resolve-current review))
  (if round
      (pcase-let ((`(,current-review . ,current-round)
                   (magnus-review-resolve-round review round)))
        (setq review current-review round current-round))
    (setq round (magnus-review-latest-round review)))
  (unless round
    (magnus-review--signal "Only a completed review round can be read"))
  (unless (eq (magnus-review-round-read-state round) 'read)
    (let ((old-state (magnus-review-round-read-state round))
          (old-updated-at (magnus-review-updated-at review)))
      (condition-case err
          (progn
            (setf (magnus-review-round-read-state round) 'read
                  (magnus-review-updated-at review) (float-time))
            (magnus-review-save review))
        (error
         (setf (magnus-review-round-read-state round) old-state
               (magnus-review-updated-at review) old-updated-at)
         (signal (car err) (cdr err))))))
  round)

(defun magnus-review-archive (review)
  "Archive REVIEW without deleting its completed reports."
  (setq review (magnus-review-resolve-current review))
  (let ((state (and (functionp magnus-review-runtime-state-function)
                    (funcall magnus-review-runtime-state-function review))))
    (when (memq state '(asking-scope running))
      (magnus-review--signal "Cannot archive a running review")))
  (let ((now (float-time))
        (old-lifecycle (magnus-review-lifecycle review))
        (old-archived-at (magnus-review-archived-at review))
        (old-updated-at (magnus-review-updated-at review)))
    ;; A failed first run has no report to archive.  Removing that unsaved
    ;; draft avoids an archived status row which cannot be opened or resumed.
    (if (null (magnus-review-rounds review))
        (progn
          (setf (magnus-review-lifecycle review) 'archived
                (magnus-review-archived-at review) now
                (magnus-review-updated-at review) now)
          (setq magnus-reviews (delq review magnus-reviews))
          (magnus-review--run-changed-hooks))
      (condition-case err
          (progn
            (setf (magnus-review-lifecycle review) 'archived
                  (magnus-review-archived-at review) now
                  (magnus-review-updated-at review) now)
            (magnus-review-save review))
        (error
         (setf (magnus-review-lifecycle review) old-lifecycle
               (magnus-review-archived-at review) old-archived-at
               (magnus-review-updated-at review) old-updated-at)
         (signal (car err) (cdr err)))))
    review))

;;; Strict completed-lineage persistence

(defun magnus-review--symbol-name (value)
  "Return VALUE's symbol name, preserve strings, or nil."
  (cond ((symbolp value) (and value (symbol-name value)))
        ((stringp value) value)
        (t nil)))

(defun magnus-review--round-to-json (round)
  "Return strict JSON-ready data for completed ROUND."
  `((number . ,(magnus-review-scope-number round))
    (base_oid . ,(magnus-review-scope-base-oid round))
    (head_oid . ,(magnus-review-scope-head-oid round))
    (created_at . ,(magnus-review-scope-created-at round))
    (completed_at . ,(magnus-review-round-completed-at round))
    (verdict . ,(magnus-review--symbol-name
                 (magnus-review-round-verdict round)))
    (read_state . ,(magnus-review--symbol-name
                    (magnus-review-round-read-state round)))
    (finding_count . ,(magnus-review-round-finding-count round))
    (result_sha256 . ,(magnus-review-round-result-sha256 round))
    (patch_sha256 . ,(magnus-review-round-patch-sha256 round))
    (name_status_sha256
     . ,(magnus-review-round-name-status-sha256 round))))

(defun magnus-review--to-json (review &optional revision)
  "Return strict JSON-ready completed-lineage data for REVIEW."
  `((schema_version . ,magnus-review-schema-version)
    (revision . ,(or revision (magnus-review-revision review)))
    (id . ,(magnus-review-id review))
    (project_root . ,(magnus-review-project-root review))
    (project_hash . ,(magnus-review-project-hash review))
    (author_instance_id . ,(magnus-review-author-instance-id review))
    (author_name . ,(magnus-review-author-name review))
    (reviewer_name . ,(magnus-review-reviewer-name review))
    (reviewer_provider . ,(magnus-review--symbol-name
                           (magnus-review-reviewer-provider review)))
    (model . ,(magnus-review-model review))
    (effort . ,(magnus-review--symbol-name (magnus-review-effort review)))
    (task . ,(magnus-review-task review))
    (reviewer_expertise . ,(magnus-review-reviewer-expertise review))
    (session_id . ,(magnus-review-session-id review))
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

(defun magnus-review--require-string
    (value kind limit &optional allow-nil)
  "Return bounded string VALUE or signal, naming it KIND."
  (unless (or (and (stringp value) (<= 1 (length value) limit))
              (and allow-nil (null value)))
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  value)

(defun magnus-review--valid-timestamp-p (value)
  "Return non-nil when VALUE is a finite, plausible Unix timestamp."
  (and (numberp value) (>= value 0) (< value 1000000000000)))

(defun magnus-review--require-timestamp (value kind &optional allow-nil)
  "Return timestamp VALUE or signal, naming it KIND."
  (unless (or (magnus-review--valid-timestamp-p value)
              (and allow-nil (null value)))
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  value)

(defun magnus-review--require-integer
    (value kind &optional allow-zero)
  "Return integer VALUE or signal, naming it KIND."
  (unless (and (integerp value)
               (if allow-zero (>= value 0) (> value 0)))
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  value)

(defun magnus-review--canonical-oid-p (value)
  "Return non-nil when VALUE is a canonical lowercase full Git OID."
  (and (magnus-review--valid-oid-p value)
       (string= value (downcase value))))

(defun magnus-review--require-oid (value kind)
  "Return canonical lowercase full OID VALUE or signal, naming it KIND."
  (unless (magnus-review--canonical-oid-p value)
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  value)

(defun magnus-review--valid-sha256-p (value)
  "Return non-nil when VALUE is a lowercase SHA-256 digest."
  (and (stringp value)
       (string-match-p "\\`[0-9a-f]\\{64\\}\\'" value)))

(defun magnus-review--require-sha256 (value kind)
  "Return SHA-256 VALUE or signal, naming it KIND."
  (unless (magnus-review--valid-sha256-p value)
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  value)

(defun magnus-review--validate-state (value states kind)
  "Parse string VALUE as one of STATES, naming it KIND."
  (let ((symbol (and (stringp value) (intern-soft value))))
    (unless (memq symbol states)
      (magnus-review--signal "Invalid %s in manifest: %S" kind value))
    symbol))

(defun magnus-review--optional-symbol (value kind)
  "Parse bounded optional symbol string VALUE, naming it KIND."
  (when value
    (magnus-review--require-string value kind 100)
    (unless (and (<= (length value) 100)
                 (string-match-p "\\`[[:alnum:]_.-]+\\'" value))
      (magnus-review--signal "Invalid %s in manifest: %S" kind value))
    (intern value)))

(defun magnus-review--round-from-json (object expected-number)
  "Deserialize completed round OBJECT, requiring EXPECTED-NUMBER."
  (magnus-review--require-object-shape
   object
   '(number base_oid head_oid created_at completed_at verdict read_state
            finding_count result_sha256 patch_sha256 name_status_sha256)
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
     :created-at (magnus-review--require-timestamp
                  (alist-get 'created_at object) "round creation time")
     :completed-at (magnus-review--require-timestamp
                    (alist-get 'completed_at object) "round completion time")
     :verdict (magnus-review--validate-state
               (alist-get 'verdict object)
               magnus-review--verdict-states "round verdict")
     :read-state (magnus-review--validate-state
                  (alist-get 'read_state object)
                  magnus-review--read-states "round read state")
     :finding-count
     (magnus-review--require-integer
      (alist-get 'finding_count object) "round finding count" t)
     :result-sha256
     (magnus-review--require-sha256
      (alist-get 'result_sha256 object) "round result digest")
     :patch-sha256
     (magnus-review--require-sha256
      (alist-get 'patch_sha256 object) "round patch digest")
     :name-status-sha256
     (magnus-review--require-sha256
      (alist-get 'name_status_sha256 object) "round name-status digest"))))

(defun magnus-review--from-json (object &optional expected-id expected-hash)
  "Deserialize strict completed-lineage OBJECT.
EXPECTED-ID and EXPECTED-HASH validate its managed storage identity."
  (magnus-review--require-object-shape
   object
   '(schema_version revision id project_root project_hash author_instance_id
                    author_name reviewer_name reviewer_provider model effort task
                    reviewer_expertise session_id lifecycle created_at updated_at
                    archived_at rounds)
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
    (magnus-review--require-string project-root "project root" 4096)
    (unless (string= project-root
                     (magnus-review--canonical-directory project-root))
      (magnus-review--signal "Project root is not canonical in manifest"))
    (unless (and (magnus-review--valid-hash-p project-hash)
                 (string= project-hash (downcase project-hash))
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
      :project-root project-root
      :project-hash project-hash
      :author-instance-id
      (magnus-review--require-string
       (alist-get 'author_instance_id object) "author instance ID" 256)
      :author-name
      (magnus-review--require-string
       (alist-get 'author_name object) "author name" 256)
      :reviewer-name
      (magnus-review--require-string
       (alist-get 'reviewer_name object) "reviewer name" 256)
      :reviewer-provider
      (magnus-review--optional-symbol
       (alist-get 'reviewer_provider object) "reviewer provider")
      :model (magnus-review--require-string
              (alist-get 'model object) "model" 200 t)
      :effort (magnus-review--optional-symbol
               (alist-get 'effort object) "effort")
      :task (magnus-review--require-string
             (alist-get 'task object) "task" 4000 t)
      :reviewer-expertise
      (magnus-review--require-string
       (alist-get 'reviewer_expertise object) "reviewer expertise" 1200 t)
      :session-id (magnus-review--require-string
                   (alist-get 'session_id object) "session ID" 1000 t)
      :revision
      (magnus-review--require-integer
       (alist-get 'revision object) "manifest revision")
      :lifecycle (magnus-review--validate-state
                  (alist-get 'lifecycle object)
                  magnus-review--lifecycle-states "lifecycle")
      :created-at (magnus-review--require-timestamp
                   (alist-get 'created_at object) "creation time")
      :updated-at (magnus-review--require-timestamp
                   (alist-get 'updated_at object) "update time")
      :archived-at (magnus-review--require-timestamp
                    (alist-get 'archived_at object) "archive time" t)
      :rounds
      (let ((rounds (alist-get 'rounds object)))
        (unless (listp rounds)
          (magnus-review--signal "Invalid rounds array in manifest"))
        (mapcar (lambda (round)
                  (magnus-review--round-from-json round (cl-incf number)))
                rounds))))))

(defun magnus-review--validate-invariants (review)
  "Validate completed lineage invariants for REVIEW."
  (unless (and (magnus-review-p review)
               (magnus-review--valid-id-p (magnus-review-id review))
               (magnus-review--valid-hash-p
                (magnus-review-project-hash review))
               (string= (magnus-review-project-hash review)
                        (downcase (magnus-review-project-hash review)))
               (string= (magnus-review-project-root review)
                        (magnus-review--canonical-directory
                         (magnus-review-project-root review)))
               (string= (magnus-review-project-hash review)
                        (magnus-review-compute-project-hash
                         (magnus-review-project-root review))))
    (magnus-review--signal "Review storage identity is inconsistent"))
  (magnus-review--bounded-string-value
   (magnus-review-author-instance-id review) "author instance ID" 256)
  (magnus-review--bounded-string-value
   (magnus-review-author-name review) "author name" 256)
  (magnus-review--bounded-string-value
   (magnus-review-reviewer-name review) "reviewer name" 256)
  (magnus-review--optional-symbol-value
   (magnus-review-reviewer-provider review) "reviewer provider")
  (magnus-review--model-value (magnus-review-model review))
  (magnus-review--optional-symbol-value
   (magnus-review-effort review) "effort")
  (magnus-review--bounded-string-value
   (magnus-review-task review) "task" 4000 t)
  (magnus-review--bounded-string-value
   (magnus-review-reviewer-expertise review) "reviewer expertise" 1200 t)
  (unless (and (integerp (magnus-review-revision review))
               (>= (magnus-review-revision review) 0))
    (magnus-review--signal "Invalid manifest revision"))
  (unless (memq (magnus-review-lifecycle review)
                magnus-review--lifecycle-states)
    (magnus-review--signal "Invalid review lifecycle"))
  (unless (and (magnus-review--valid-timestamp-p
                (magnus-review-created-at review))
               (magnus-review--valid-timestamp-p
                (magnus-review-updated-at review))
               (<= (magnus-review-created-at review)
                   (magnus-review-updated-at review)))
    (magnus-review--signal "Invalid review timestamps"))
  (if (eq (magnus-review-lifecycle review) 'archived)
      (unless (and (magnus-review--valid-timestamp-p
                    (magnus-review-archived-at review))
                   (<= (magnus-review-created-at review)
                       (magnus-review-archived-at review))
                   (<= (magnus-review-archived-at review)
                       (magnus-review-updated-at review)))
        (magnus-review--signal "Archived review lacks archive timestamp"))
    (when (magnus-review-archived-at review)
      (magnus-review--signal "Open review has archive timestamp")))
  (when (and (magnus-review-session-id review)
             (not (and (stringp (magnus-review-session-id review))
                       (<= 1 (length (magnus-review-session-id review)) 1000))))
    (magnus-review--signal "Invalid successful provider session ID"))
  (unless (proper-list-p (magnus-review-rounds review))
    (magnus-review--signal "Review rounds must be a proper list"))
  (let ((number 0))
    (dolist (round (magnus-review-rounds review))
      (unless (and (magnus-review-round-p round)
                   (= (magnus-review-scope-number round) (cl-incf number)))
        (magnus-review--signal "Review rounds are not sequential"))
      (unless (and (magnus-review--canonical-oid-p
                    (magnus-review-scope-base-oid round))
                   (magnus-review--canonical-oid-p
                    (magnus-review-scope-head-oid round))
                   (not (string= (magnus-review-scope-base-oid round)
                                 (magnus-review-scope-head-oid round))))
        (magnus-review--signal "Round %d has invalid Git scope" number))
      (unless (and (magnus-review--valid-timestamp-p
                    (magnus-review-scope-created-at round))
                   (magnus-review--valid-timestamp-p
                    (magnus-review-round-completed-at round))
                   (<= (magnus-review-scope-created-at round)
                       (magnus-review-round-completed-at round))
                   (<= (magnus-review-round-completed-at round)
                       (magnus-review-updated-at review)))
        (magnus-review--signal "Round %d has invalid timestamps" number))
      (unless (and (memq (magnus-review-round-verdict round)
                         magnus-review--verdict-states)
                   (memq (magnus-review-round-read-state round)
                         magnus-review--read-states))
        (magnus-review--signal "Round %d is not successfully published"
                               number))
      (unless (and (integerp (magnus-review-round-finding-count round))
                   (>= (magnus-review-round-finding-count round) 0)
                   (magnus-review--valid-sha256-p
                    (magnus-review-round-result-sha256 round))
                   (magnus-review--valid-sha256-p
                    (magnus-review-round-patch-sha256 round))
                   (magnus-review--valid-sha256-p
                    (magnus-review-round-name-status-sha256 round)))
        (magnus-review--signal "Round %d has invalid artifact integrity" number))))
  review)

(defun magnus-review--disk-review-locked (review)
  "Return REVIEW's strict manifest object while its lineage lock is held."
  (let ((manifest (magnus-review-manifest-path review)))
    (when (file-exists-p manifest)
      (magnus-review--from-json
       (magnus-review--read-json-file manifest)
       (magnus-review-id review) (magnus-review-project-hash review)))))

(defun magnus-review--assert-current-revision-locked (review)
  "Require REVIEW to match its durable revision while write-locked."
  (let ((disk (magnus-review--disk-review-locked review))
        (expected (magnus-review-revision review)))
    (unless (if disk
                (= expected (magnus-review-revision disk))
              (= expected 0))
      (signal 'magnus-review-stale-error
              (list
               (format
                "Review %s changed in another Emacs; reload it before retrying"
                (magnus-review-id review)))))
    disk))

(defun magnus-review--save-locked (review)
  "Persist REVIEW while its lineage lock is held and return it.
No operation after the atomic manifest rename is allowed to signal."
  (unless (magnus-review-rounds review)
    (magnus-review--signal "Cannot persist a review without a completed round"))
  (magnus-review--validate-invariants review)
  (let ((next-revision (1+ (magnus-review-revision review))))
    (magnus-review--atomic-write-string
     (magnus-review-manifest-path review)
     (concat (json-serialize (magnus-review--to-json review next-revision)
                             :null-object nil
                             :false-object :json-false)
             "\n"))
    ;; This assignment is deliberately the only action after durable rename.
    (setf (magnus-review-revision review) next-revision))
  review)

(defun magnus-review-save (review)
  "Persist completed current REVIEW with stale-writer detection."
  (let ((current (magnus-review-resolve-current review)))
    (unless (eq current review)
      (signal 'magnus-review-stale-error
              (list "Review object was replaced; resolve it before saving"))))
  (magnus-review--call-with-lineage-lock
   review
   (lambda ()
     (magnus-review--assert-current-revision-locked review)
     (magnus-review--save-locked review)))
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

(defun magnus-review-read-verified-artifact (review round kind)
  "Return exact verified bytes for completed REVIEW ROUND artifact KIND.
KIND is one of `result', `patch', or `name-status'."
  (unless (and (magnus-review-p review) (magnus-review-round-p round))
    (magnus-review--signal "Verified artifacts require a completed round"))
  (pcase kind
    ('result
     (magnus-review--verify-bytes
      (magnus-review--read-artifact-bytes
       (magnus-review-round-result-path review round)
       "result" magnus-review-max-result-bytes)
      (magnus-review-round-result-sha256 round) "result"))
    ('patch
     (magnus-review--verify-bytes
      (magnus-review--read-artifact-bytes
       (magnus-review-round-patch-path review round)
       "patch" magnus-review-max-evidence-bytes)
      (magnus-review-round-patch-sha256 round) "patch"))
    ('name-status
     (magnus-review--verify-bytes
      (magnus-review--read-artifact-bytes
       (magnus-review-round-name-status-path review round)
       "name-status" magnus-review-max-evidence-bytes)
      (magnus-review-round-name-status-sha256 round) "name-status"))
    (_ (magnus-review--signal "Unknown review artifact kind: %S" kind))))

(defun magnus-review--result-from-verified-bytes (review round bytes)
  "Parse and validate verified result BYTES for REVIEW ROUND."
  (let ((envelope
         (with-temp-buffer
           (let ((coding-system-for-read 'utf-8-unix))
             (insert (decode-coding-string bytes 'utf-8-unix)))
           (goto-char (point-min))
           (json-parse-buffer :object-type 'alist :array-type 'array
                              :null-object nil :false-object :json-false))))
    (magnus-review--require-object-shape
     envelope
     '(artifact_schema_version review_id round_number base_oid head_oid
                               created_at result)
     "result envelope")
    (unless (eql (alist-get 'artifact_schema_version envelope) 1)
      (magnus-review--signal "Unsupported review result artifact schema"))
    (unless (equal (alist-get 'review_id envelope)
                   (magnus-review-id review))
      (magnus-review--signal "Review result belongs to another lineage"))
    (unless (eql (alist-get 'round_number envelope)
                 (magnus-review-scope-number round))
      (magnus-review--signal "Review result has the wrong round number"))
    (dolist (entry
             `((base_oid . ,(magnus-review-scope-base-oid round))
               (head_oid . ,(magnus-review-scope-head-oid round))))
      (unless (equal (alist-get (car entry) envelope) (cdr entry))
        (magnus-review--signal "Review result has the wrong %s" (car entry))))
    (magnus-review--require-timestamp
     (alist-get 'created_at envelope) "result creation time")
    (let* ((result (alist-get 'result envelope))
           (verdict (and (listp result) (alist-get 'verdict result)))
           (findings-entry (and (listp result) (assq 'findings result)))
           (findings (cdr findings-entry))
           (verdict-symbol
            (pcase verdict
              ("approve" 'approve)
              ("comment" 'comment)
              ("request_changes" 'changes-requested)
              (_ nil))))
      (unless (and (listp result) (assq 'verdict result)
                   findings-entry (vectorp findings))
        (magnus-review--signal "Review result has an invalid canonical body"))
      (unless (eq verdict-symbol (magnus-review-round-verdict round))
        (magnus-review--signal "Review result verdict disagrees with manifest"))
      (unless (= (length findings)
                 (magnus-review-round-finding-count round))
        (magnus-review--signal
         "Review result finding count disagrees with manifest"))
      result)))

(defun magnus-review-read-verified-result (review round)
  "Return REVIEW ROUND's canonical result after exact-byte verification."
  (let ((bytes (magnus-review-read-verified-artifact review round 'result)))
    (magnus-review--result-from-verified-bytes review round bytes)))

(defun magnus-review-read-verified-artifacts (review round)
  "Return one verified artifact bundle for current completed REVIEW ROUND.
The plist contains `:result-bytes', parsed `:result', `:patch', and
`:name-status'."
  (pcase-let* ((`(,review . ,round)
                (magnus-review-resolve-round review round))
               (result-bytes
                (magnus-review-read-verified-artifact review round 'result))
               (patch
                (magnus-review-read-verified-artifact review round 'patch))
               (name-status
                (magnus-review-read-verified-artifact
                 review round 'name-status)))
    (list :result-bytes result-bytes
          :result
          (magnus-review--result-from-verified-bytes
           review round result-bytes)
          :patch patch
          :name-status name-status)))

(defun magnus-review-load-file (file &optional expected-id expected-hash)
  "Load one strict completed-lineage manifest FILE and register it."
  (let ((review
         (magnus-review--from-json
          (magnus-review--read-json-file file) expected-id expected-hash)))
    (unless (magnus-review-rounds review)
      (magnus-review--signal "Durable review has no completed rounds"))
    (when (eq (magnus-review-lifecycle review) 'open)
      (when-let ((existing
                  (car
                   (magnus-review--open-for-author-matches
                    (magnus-review-project-root review)
                    (magnus-review-author-instance-id review)
                    (magnus-review-id review)))))
        (magnus-review--signal
         "Author %s already has loaded open review %s in this project"
         (magnus-review-author-name review) (magnus-review-id existing))))
    (when-let ((existing (magnus-review-get (magnus-review-id review))))
      (unless (string= (magnus-review-project-hash existing)
                       (magnus-review-project-hash review))
        (magnus-review--signal "Duplicate review ID across projects"))
      (setq magnus-reviews (delq existing magnus-reviews)))
    (push review magnus-reviews)
    review))

(defun magnus-review-refresh-from-disk (review)
  "Return current REVIEW, replacing it only when disk has a newer revision.
This is the narrow refresh operation for long-lived readers; it never resets
the complete review registry."
  (setq review (magnus-review-resolve-current review))
  (let ((manifest (magnus-review-manifest-path review)))
    (if (not (file-exists-p manifest))
        (if (= (magnus-review-revision review) 0)
            review
          (signal 'magnus-review-stale-error
                  (list "Durable review manifest disappeared")))
      (let ((disk
             (magnus-review--from-json
              (magnus-review--read-json-file manifest)
              (magnus-review-id review) (magnus-review-project-hash review))))
        (cond
         ((< (magnus-review-revision disk) (magnus-review-revision review))
          (signal 'magnus-review-stale-error
                  (list "Review manifest revision moved backwards")))
         ((= (magnus-review-revision disk) (magnus-review-revision review))
          review)
         (t
          (setq magnus-reviews (delq review magnus-reviews))
          (push disk magnus-reviews)
          (magnus-review--run-changed-hooks)
          disk))))))

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
