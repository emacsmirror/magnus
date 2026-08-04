;;; magnus-review.el --- Durable review records and Git snapshots -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns the durable, provider-independent part of Magnus reviews.
;; A review is not an interactive Magnus instance: it has its own lifecycle,
;; immutable Git rounds, resumable provider identity, and append-only attempts.
;; Provider processes and review presentation deliberately live elsewhere.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup magnus-review nil
  "Durable cross-provider reviews managed by Magnus."
  :group 'magnus
  :prefix "magnus-review-")

(defcustom magnus-review-directory-root
  (expand-file-name ".magnus/reviews" (or (getenv "HOME") "~"))
  "Private directory containing durable Magnus review records."
  :type 'directory
  :group 'magnus-review)

(defcustom magnus-review-max-evidence-bytes (* 64 1024 1024)
  "Maximum bytes accepted for one immutable review patch.
This bounds durable storage and accidental giant binary reviews."
  :type 'integer
  :group 'magnus-review)

(defconst magnus-review-schema-version 1
  "Current on-disk review manifest schema version.")

(defconst magnus-review--lifecycle-states '(open closed archived))
(defconst magnus-review--execution-states
  '(waiting-for-checkpoint queued starting running complete failed interrupted))
(defconst magnus-review--verdict-states '(approve comment changes-requested))
(defconst magnus-review--delivery-states '(not-ready pending sent failed))
(defconst magnus-review--read-states '(not-ready unread read))
(defconst magnus-review--terminal-attempt-states '(complete failed interrupted))
(defconst magnus-review--interruption-kinds '(manual shutdown crash))

(define-error 'magnus-review-error "Magnus review error")
(define-error 'magnus-review-git-error "Magnus review Git error"
  'magnus-review-error)

;;; Records

(cl-defstruct (magnus-review-attempt
               (:constructor magnus-review-attempt--create)
               (:copier nil))
  "One provider invocation within a review round."
  number
  token
  started-at
  finished-at
  (execution 'starting)
  error
  interruption-kind
  metadata)

(cl-defstruct (magnus-review-round
               (:constructor magnus-review-round--create)
               (:copier nil))
  "An immutable Git comparison and its append-only provider attempts."
  number
  base-oid
  head-oid
  previous-head-oid
  checkpoint-token
  created-at
  completed-at
  (execution 'queued)
  verdict
  (delivery-state 'not-ready)
  (delivery-attempts 0)
  delivery-error
  delivered-at
  (read-state 'not-ready)
  read-at
  attempts
  metadata)

(cl-defstruct (magnus-review
               (:constructor magnus-review--create)
               (:copier nil))
  "A durable review task independent of any provider process."
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
  (lifecycle 'open)
  (execution 'waiting-for-checkpoint)
  verdict
  (delivery-state 'not-ready)
  (read-state 'not-ready)
  session-id
  checkpoint-token
  created-at
  updated-at
  closed-at
  archived-at
  base-oid
  head-oid
  previous-head-oid
  rounds
  metadata
  checkpoint-acks)

(defvar magnus-reviews nil
  "All reviews loaded in this Emacs session.")

(defvar magnus-reviews-changed-hook nil
  "Hook run after the durable review registry changes.")

(defvar magnus-review-ready-hook nil
  "Hook run with REVIEW and ROUND after a checkpoint becomes reviewable.")

;;; Small validation and path helpers

(defun magnus-review--signal (format-string &rest args)
  "Signal `magnus-review-error' with FORMAT-STRING and ARGS."
  (signal 'magnus-review-error (list (apply #'format format-string args))))

(defun magnus-review--valid-id-p (value)
  "Return non-nil when VALUE is a safe review identifier."
  (and (stringp value)
       (<= 1 (length value) 128)
       (string-match-p
        "\\`[[:alnum:]][[:alnum:]_.-]*\\'" value)))

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

(defun magnus-review--valid-token-p (value)
  "Return non-nil when VALUE is a bounded opaque protocol token."
  (and (stringp value)
       (<= 16 (length value) 128)
       (string-match-p "\\`[[:alnum:]_.-]+\\'" value)))

(defun magnus-review--error-string (value)
  "Normalize arbitrary error VALUE to a bounded durable string."
  (when value
    (let ((text (if (stringp value) value (format "%S" value))))
      (if (> (length text) 10000)
          (substring text 0 10000)
        text))))

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
      (magnus-review--signal "Review path escapes its managed directory: %s"
                             child))
    child))

(defun magnus-review-directory (review)
  "Return REVIEW's private durable directory without creating it."
  (unless (magnus-review-p review)
    (magnus-review--signal "Not a Magnus review: %S" review))
  (unless (magnus-review--valid-hash-p (magnus-review-project-hash review))
    (magnus-review--signal "Invalid project hash in review %s"
                           (magnus-review-id review)))
  (let ((project-directory
         (magnus-review--child-path magnus-review-directory-root
                                    (magnus-review-project-hash review))))
    (magnus-review--child-path project-directory (magnus-review-id review))))

(defun magnus-review-manifest-path (review)
  "Return REVIEW's manifest path."
  (expand-file-name "manifest.json" (magnus-review-directory review)))

(defun magnus-review-checkout-path (review)
  "Return REVIEW's managed Git worktree path."
  (expand-file-name "checkout" (magnus-review-directory review)))

(defun magnus-review--worktree-marker-path (review)
  "Return REVIEW's private worktree ownership marker path."
  (expand-file-name "worktree-owner.json" (magnus-review-directory review)))

(defun magnus-review--positive-number (value kind)
  "Return positive integer VALUE or signal, naming it KIND."
  (unless (and (integerp value) (> value 0))
    (magnus-review--signal "Invalid %s number: %S" kind value))
  value)

(defun magnus-review-round-directory (review round)
  "Return the artifact directory for ROUND in REVIEW."
  (let ((number (if (magnus-review-round-p round)
                    (magnus-review-round-number round)
                  round)))
    (magnus-review--positive-number number "round")
    (expand-file-name (format "rounds/%03d" number)
                      (magnus-review-directory review))))

(defun magnus-review-attempt-raw-path (review round attempt)
  "Return the raw JSONL artifact path for ATTEMPT of ROUND in REVIEW."
  (let ((number (if (magnus-review-attempt-p attempt)
                    (magnus-review-attempt-number attempt)
                  attempt)))
    (magnus-review--positive-number number "attempt")
    (expand-file-name (format "attempt-%03d.jsonl" number)
                      (magnus-review-round-directory review round))))

(defun magnus-review-attempt-stderr-path (review round attempt)
  "Return the stderr artifact path for ATTEMPT of ROUND in REVIEW."
  (let ((number (if (magnus-review-attempt-p attempt)
                    (magnus-review-attempt-number attempt)
                  attempt)))
    (magnus-review--positive-number number "attempt")
    (expand-file-name (format "attempt-%03d.stderr.log" number)
                      (magnus-review-round-directory review round))))

(defun magnus-review-round-result-path (review round)
  "Return the canonical structured result path for ROUND in REVIEW."
  (expand-file-name "result.json"
                    (magnus-review-round-directory review round)))

(defun magnus-review-round-report-path (review round)
  "Return the rendered Markdown report path for ROUND in REVIEW."
  (expand-file-name "report.md"
                    (magnus-review-round-directory review round)))

(defun magnus-review-round-patch-path (review round)
  "Return the immutable committed patch evidence path for ROUND in REVIEW."
  (expand-file-name "evidence.patch"
                    (magnus-review-round-directory review round)))

(defun magnus-review-round-name-status-path (review round)
  "Return the NUL-delimited changed-path evidence path for ROUND in REVIEW."
  (expand-file-name "name-status.z"
                    (magnus-review-round-directory review round)))

(defun magnus-review--ensure-private-directory (directory)
  "Create DIRECTORY if needed and require it to be a private real directory."
  (when (file-remote-p directory)
    (magnus-review--signal "Managed review path may not be remote: %s" directory))
  (when (file-symlink-p directory)
    (magnus-review--signal "Refusing symlinked review directory: %s" directory))
  (if (file-exists-p directory)
      (unless (file-directory-p directory)
        (magnus-review--signal "Review path is not a directory: %s" directory))
    (make-directory directory t))
  (set-file-modes directory #o700)
  directory)

(defun magnus-review--ensure-review-directories (review &optional round)
  "Create REVIEW's private directories, and optional ROUND directory."
  (let* ((root (directory-file-name
                (expand-file-name magnus-review-directory-root)))
         (project (magnus-review--child-path
                   root (magnus-review-project-hash review)))
         (directory (magnus-review-directory review)))
    (magnus-review--ensure-private-directory root)
    (magnus-review--ensure-private-directory project)
    (magnus-review--ensure-private-directory directory)
    (magnus-review--ensure-private-directory
     (expand-file-name "rounds" directory))
    (when round
      (magnus-review--ensure-private-directory
       (magnus-review-round-directory review round)))
    directory))

(defun magnus-review--atomic-write-string (file contents &optional coding)
  "Write CONTENTS atomically to FILE with mode 0600.
CODING defaults to `utf-8-unix'; use `no-conversion' for Git byte evidence."
  (let ((directory (file-name-directory file))
        temporary)
    (magnus-review--ensure-private-directory directory)
    (when (file-symlink-p file)
      (magnus-review--signal "Refusing to overwrite symlink: %s" file))
    (when (file-directory-p file)
      (magnus-review--signal "Refusing to overwrite directory: %s" file))
    (setq temporary
          (make-temp-file (expand-file-name ".magnus-review-tmp-" directory)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write (or coding 'utf-8-unix)))
            (write-region contents nil temporary nil 'quiet))
          (set-file-modes temporary #o600)
          (rename-file temporary file t)
          (setq temporary nil)
          (set-file-modes file #o600))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun magnus-review-prepare-artifact-path (review path)
  "Prepare private directories for REVIEW artifact PATH and return PATH.
PATH must be lexically below REVIEW's managed rounds directory."
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

(defun magnus-review-write-artifact
    (review path contents &optional coding _replace)
  "Atomically write private REVIEW artifact CONTENTS to PATH.
An existing identical file is adopted idempotently; a divergent artifact is
never overwritten.  CODING defaults to UTF-8.  The final optional argument is
retained for controller compatibility but cannot weaken append-only safety."
  (setq path (magnus-review-prepare-artifact-path review path))
  (when (file-exists-p path)
    (unless (let ((existing (magnus-review--read-artifact-bytes path))
                  (expected
                   (if (eq coding 'no-conversion)
                       contents
                     (encode-coding-string contents
                                           (or coding 'utf-8-unix)))))
              (equal existing expected))
      (magnus-review--signal
       "Refusing to overwrite append-only review artifact: %s" path)))
  (unless (file-exists-p path)
    (magnus-review--atomic-write-string path contents coding))
  (set-file-modes path #o600)
  path)

(defun magnus-review-append-artifact-line
    (review path line &optional coding)
  "Append complete LINE to a private REVIEW artifact at PATH.
This is intended for raw JSONL and stderr streams; it never follows symlinks."
  (unless (stringp line)
    (magnus-review--signal "Review artifact line must be a string"))
  (setq path (magnus-review-prepare-artifact-path review path))
  (when (and (file-exists-p path)
             (or (file-symlink-p path) (not (file-regular-p path))))
    (magnus-review--signal "Unsafe append-only review artifact: %s" path))
  (let ((coding-system-for-write (or coding 'utf-8-unix)))
    (write-region (concat line "\n") nil path t 'quiet))
  (set-file-modes path #o600)
  path)

;;; Git scope inspection

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
                           "-C" directory arguments))
            (output nil))
        (setq output (string-trim-right (buffer-string)))
        (unless (and (integerp status) (zerop status))
          (signal 'magnus-review-git-error
                  (list (if (string-empty-p output)
                            (format "Git failed (%s): git %s"
                                    status (string-join arguments " "))
                          output))))
        output))))

(defun magnus-review--git-output-optional (directory &rest arguments)
  "Like `magnus-review--git-output', returning nil when Git fails."
  (condition-case nil
      (apply #'magnus-review--git-output directory arguments)
    (magnus-review-git-error nil)))

(defun magnus-review--git-output-raw (directory &rest arguments)
  "Run Git in DIRECTORY and return its exact unibyte stdout."
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
  (let ((root (magnus-review--git-output
               directory "rev-parse" "--show-toplevel")))
    (magnus-review--canonical-directory root)))

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
              (list (format "Git returned a non-full commit ID for %s: %S"
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

(defun magnus-review-worktree-dirty-status (project-root)
  "Return porcelain status for PROJECT-ROOT, or nil when it is clean."
  (let ((status (magnus-review--git-output
                 project-root "status" "--porcelain=v1"
                 "--untracked-files=normal")))
    (unless (string-empty-p status) status)))

(defun magnus-review-dirty-warning (project-root)
  "Return a user-facing warning when PROJECT-ROOT has uncommitted changes."
  (when (magnus-review-worktree-dirty-status project-root)
    "Working tree has uncommitted changes; they are excluded from this review."))

(defun magnus-review-inspect-scope (project-root base-revision head-revision)
  "Return validated Git evidence for BASE-REVISION..HEAD-REVISION.
The result is a plist suitable for status/transient presentation."
  (let* ((project-root (magnus-review-git-root project-root))
         (base (magnus-review-resolve-oid project-root base-revision))
         (head (magnus-review-resolve-oid project-root head-revision)))
    (unless (magnus-review-base-ancestor-p project-root base head)
      (signal 'magnus-review-git-error
              (list (format "Review base %s is not an ancestor of head %s"
                            base head))))
    (let* ((dirty-status (magnus-review-worktree-dirty-status project-root))
           (name-output (magnus-review--git-output
                         project-root "diff" "--name-only" "-z"
                         "--no-ext-diff" base head "--"))
           (changed-files (split-string name-output "\0" t))
           (diffstat (magnus-review--git-output
                      project-root "diff" "--stat" "--no-color"
                      "--no-ext-diff" base head "--"))
           (shortstat (magnus-review--git-output
                       project-root "diff" "--shortstat" "--no-color"
                       "--no-ext-diff" base head "--"))
           (commit-count-string
            (magnus-review--git-output project-root "rev-list" "--count"
                                       (concat base ".." head))))
      (list :project-root project-root
            :base-oid base
            :head-oid head
            :ancestor-p t
            :commit-count (string-to-number commit-count-string)
            :changed-file-count (length changed-files)
            :changed-files changed-files
            :diffstat diffstat
            :shortstat shortstat
            :dirty-p (and dirty-status t)
            :dirty-status dirty-status
            :dirty-warning
            (and dirty-status
                 "Working tree has uncommitted changes; they are excluded from this review.")))))

(defun magnus-review-suggest-upstream-scope (project-root &optional head-revision)
  "Suggest an upstream merge-base scope for PROJECT-ROOT.
Return nil when the current branch has no configured upstream.  Otherwise
return `magnus-review-inspect-scope' data plus :upstream and :upstream-oid."
  (let* ((project-root (magnus-review-git-root project-root))
         (head (magnus-review-resolve-oid project-root
                                          (or head-revision "HEAD")))
         (upstream (magnus-review--git-output-optional
                    project-root "rev-parse" "--abbrev-ref"
                    "--symbolic-full-name" "@{upstream}")))
    (when (and upstream (not (string-empty-p upstream)))
      (let* ((upstream-oid (magnus-review-resolve-oid project-root upstream))
             (base (downcase
                    (magnus-review--git-output
                     project-root "merge-base" head upstream-oid))))
        (unless (magnus-review--valid-oid-p base)
          (signal 'magnus-review-git-error
                  (list (format "Invalid merge-base returned by Git: %S" base))))
        (append (magnus-review-inspect-scope project-root base head)
                (list :upstream upstream :upstream-oid upstream-oid))))))

(defun magnus-review-capture-round-evidence (review round)
  "Persist immutable Git patch and path evidence for REVIEW ROUND.
Evidence files are append-only, private, and remain after worktree cleanup."
  (let* ((project-root (magnus-review-project-root review))
         (base (magnus-review-round-base-oid round))
         (head (magnus-review-round-head-oid round))
         (patch-path (magnus-review-round-patch-path review round))
         (name-status-path
          (magnus-review-round-name-status-path review round)))
    (magnus-review--ensure-review-directories review round)
    ;; Compute both byte streams before publishing either path.  The atomic
    ;; writers ensure readers never observe a partial Git stream.  Exact files
    ;; left by a crash before manifest publication are adopted, never replaced.
    (let ((patch (magnus-review--git-output-raw
                  project-root "diff" "--binary" "--full-index"
                  "--find-renames" "--no-ext-diff" base head "--"))
          (name-status
           (magnus-review--git-output-raw
            project-root "diff" "--name-status" "-z" "--find-renames"
            "--no-ext-diff" base head "--")))
      (when (> (length patch) magnus-review-max-evidence-bytes)
        (magnus-review--signal
         "Review patch is %d bytes; configured limit is %d"
         (length patch) magnus-review-max-evidence-bytes))
      (cl-mapc
       (lambda (path contents)
         (if (file-exists-p path)
             (progn
               (when (or (file-symlink-p path) (not (file-regular-p path)))
                 (magnus-review--signal
                  "Refusing unsafe existing review evidence: %s" path))
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (let ((coding-system-for-read 'no-conversion))
                   (insert-file-contents-literally path))
                 (unless (equal (buffer-string) contents)
                   (magnus-review--signal
                    "Existing round evidence does not match Git scope: %s"
                    path))))
           (magnus-review--atomic-write-string path contents 'no-conversion)))
       (list patch-path name-status-path)
       (list patch name-status)))
    (list :patch patch-path :name-status name-status-path)))

;;; Registry and append-only domain transitions

(defun magnus-review-list ()
  "Return a copy of the loaded review list."
  (copy-sequence magnus-reviews))

(defun magnus-review-get (id)
  "Return the loaded review whose ID is ID."
  (cl-find id magnus-reviews :key #'magnus-review-id :test #'string=))

(defun magnus-review-latest-round (review)
  "Return REVIEW's latest round, or nil."
  (car (last (magnus-review-rounds review))))

(defun magnus-review--checkpoint-ack-round-for-token (review token)
  "Return REVIEW's acknowledged immutable round for TOKEN, or nil."
  (when-let ((round-number
              (cdr (assoc token (magnus-review-checkpoint-acks review)))))
    (nth (1- round-number) (magnus-review-rounds review))))

(defun magnus-review-latest-attempt (round)
  "Return ROUND's latest attempt, or nil."
  (car (last (magnus-review-round-attempts round))))

(defun magnus-review--refresh-aggregate-notification-states (review)
  "Derive REVIEW delivery/read aggregates without hiding older rounds."
  (let* ((completed
          (cl-remove-if-not
           (lambda (round)
             (eq (magnus-review-round-execution round) 'complete))
           (magnus-review-rounds review)))
         (delivery-states
          (mapcar #'magnus-review-round-delivery-state completed))
         (read-states
          (mapcar #'magnus-review-round-read-state completed)))
    (setf (magnus-review-delivery-state review)
          (cond ((memq 'pending delivery-states) 'pending)
                ((memq 'failed delivery-states) 'failed)
                ((and completed
                      (cl-every (lambda (state) (eq state 'sent))
                                delivery-states))
                 'sent)
                (t 'not-ready))
          (magnus-review-read-state review)
          (cond ((memq 'unread read-states) 'unread)
                ((and completed
                      (cl-every (lambda (state) (eq state 'read)) read-states))
                 'read)
                (t 'not-ready)))))

(defun magnus-review--random-token ()
  "Return a locally unique opaque token."
  (secure-hash
   'sha256
   (format "%s:%s:%s:%s:%s"
           (float-time) (emacs-pid) (user-uid) (random most-positive-fixnum)
           (current-time-string))))

(defun magnus-review--generate-id (project-hash)
  "Generate an unused durable review ID below PROJECT-HASH."
  (let (candidate)
    (while
        (progn
          (setq candidate (substring (magnus-review--random-token) 0 32))
          (or (magnus-review-get candidate)
              (file-exists-p
               (magnus-review--child-path
                (magnus-review--child-path
                 magnus-review-directory-root project-hash)
                candidate)))))
    candidate))

(cl-defun magnus-review-create
    (project-root author-instance-id author-name
                  &key id task reviewer-name reviewer-provider model effort metadata)
  "Create and persist a review request for AUTHOR-NAME in PROJECT-ROOT.
The initial request waits for an exact committed checkpoint."
  (let* ((project-root (magnus-review-git-root project-root))
         (project-hash (magnus-review-compute-project-hash project-root))
         (id (or id (magnus-review--generate-id project-hash)))
         (now (float-time)))
    (unless (magnus-review--valid-id-p id)
      (magnus-review--signal "Invalid review ID: %S" id))
    (when (magnus-review-get id)
      (magnus-review--signal "Review ID is already loaded: %s" id))
    (let ((review
           (magnus-review--create
            :id id
            :project-root project-root
            :project-hash project-hash
            :author-instance-id author-instance-id
            :author-name author-name
            :reviewer-name reviewer-name
            :reviewer-provider reviewer-provider
            :model model
            :effort effort
            :task task
            :lifecycle 'open
            :execution 'waiting-for-checkpoint
            :delivery-state 'not-ready
            :read-state 'not-ready
            :checkpoint-token (magnus-review--random-token)
            :created-at now
            :updated-at now
            :rounds nil
            :metadata metadata)))
      (magnus-review-save review)
      review)))

(defun magnus-review-await-checkpoint (review)
  "Put open REVIEW into checkpoint-waiting state with a fresh token."
  (unless (eq (magnus-review-lifecycle review) 'open)
    (magnus-review--signal "Closed review cannot await a checkpoint: %s"
                           (magnus-review-id review)))
  (when (memq (magnus-review-execution review) '(starting running))
    (magnus-review--signal "Review already has a running attempt: %s"
                           (magnus-review-id review)))
  (when-let ((latest (magnus-review-latest-round review)))
    (unless (eq (magnus-review-round-execution latest) 'complete)
      (magnus-review--signal
       "Retry or finish the current round before requesting re-review")))
  (setf (magnus-review-checkpoint-token review) (magnus-review--random-token)
        (magnus-review-execution review) 'waiting-for-checkpoint
        (magnus-review-updated-at review) (float-time))
  (magnus-review-save review)
  (magnus-review-checkpoint-token review))

(defun magnus-review--acknowledge-unchanged-checkpoint
    (review checkpoint-token base head)
  "Durably acknowledge that REVIEW remained at exact BASE and HEAD.
CHECKPOINT-TOKEN identifies the request whose author reported no new committed
evidence.  The latest completed round and its verdict remain authoritative."
  (let ((latest (magnus-review-latest-round review)))
    (unless (and latest
                 (eq (magnus-review-lifecycle review) 'open)
                 (eq (magnus-review-execution review)
                     'waiting-for-checkpoint)
                 (eq (magnus-review-round-execution latest) 'complete)
                 (string= base (magnus-review-round-base-oid latest))
                 (string= head (magnus-review-round-head-oid latest)))
      (magnus-review--signal
       "Unchanged checkpoint does not match the latest completed round"))
    (let ((now (float-time)))
      (setf (magnus-review-checkpoint-acks review)
            (append
             (magnus-review-checkpoint-acks review)
             (list (cons checkpoint-token
                         (magnus-review-round-number latest))))
            (magnus-review-updated-at review) now)
      (magnus-review-save review)
      (message
       "Magnus: %s reported no new committed changes; review is still waiting after round %d (%s)"
       (magnus-review-author-name review)
       (magnus-review-round-number latest)
       (magnus-review-round-verdict latest))
      latest)))

(cl-defun magnus-review-append-round
    (review base-revision head-revision
            &key previous-head-revision checkpoint-token metadata)
  "Append an exact immutable Git round to REVIEW.
BASE-REVISION must remain fixed across rounds and be an ancestor of
HEAD-REVISION.  Older rounds and attempts are never removed or rewritten."
  (unless (eq (magnus-review-lifecycle review) 'open)
    (magnus-review--signal "Cannot append to non-open review: %s"
                           (magnus-review-id review)))
  (when (and checkpoint-token
             (not (magnus-review--valid-token-p checkpoint-token)))
    (magnus-review--signal "Invalid checkpoint token"))
  (unless (eq (magnus-review-execution review) 'waiting-for-checkpoint)
    (magnus-review--signal "Review is not waiting for a checkpoint: %s"
                           (magnus-review-id review)))
  (let* ((scope (magnus-review-inspect-scope
                 (magnus-review-project-root review)
                 base-revision head-revision))
         (base (plist-get scope :base-oid))
         (head (plist-get scope :head-oid))
         (latest (magnus-review-latest-round review))
         (previous (cond
                    (previous-head-revision
                     (magnus-review-resolve-oid
                      (magnus-review-project-root review)
                      previous-head-revision))
                    (latest (magnus-review-round-head-oid latest))))
         (expected-base (magnus-review-base-oid review))
         (scope-summary
          (list :commit-count (plist-get scope :commit-count)
                :changed-file-count (plist-get scope :changed-file-count)
                :shortstat (plist-get scope :shortstat)
                :dirty-p (plist-get scope :dirty-p)
                :dirty-warning (plist-get scope :dirty-warning))))
    (when (and (null latest) (string= base head))
      (magnus-review--signal "Refusing an empty review scope at %s" head))
    (when (and (null latest)
               (zerop (plist-get scope :changed-file-count)))
      (magnus-review--signal "Review scope contains no changed files"))
    (when (and latest
               (not (eq (magnus-review-round-execution latest) 'complete)))
      (magnus-review--signal "Previous review round is not complete"))
    (when (and expected-base (not (string= expected-base base)))
      (magnus-review--signal
       "Review base is immutable (%s, not %s)" expected-base base))
    (when (and latest
               previous
               (not (string= previous (magnus-review-round-head-oid latest))))
      (magnus-review--signal
       "Previous head must equal the latest reviewed head (%s)"
       (magnus-review-round-head-oid latest)))
    (when (and latest (string= (magnus-review-round-head-oid latest) head))
      (magnus-review--signal "Head %s is already the latest review round" head))
    (let* ((number (1+ (length (magnus-review-rounds review))))
           (now (float-time))
           (round
            (magnus-review-round--create
             :number number
             :base-oid base
             :head-oid head
             :previous-head-oid previous
             :checkpoint-token checkpoint-token
             :created-at now
             :execution 'queued
             :delivery-state 'not-ready
             :read-state 'not-ready
             :attempts nil
             :metadata `((scope . ,scope-summary)
                         (caller . ,metadata)))))
      (magnus-review-capture-round-evidence review round)
      (setf (magnus-review-rounds review)
            (append (magnus-review-rounds review) (list round))
            (magnus-review-base-oid review) (or expected-base base)
            (magnus-review-previous-head-oid review) previous
            (magnus-review-head-oid review) head
            (magnus-review-execution review) 'queued
            (magnus-review-updated-at review) now)
      (magnus-review-save review)
      round)))

(cl-defun magnus-review-append-attempt (review round &key token metadata)
  "Append and persist a new starting provider attempt to ROUND of REVIEW."
  (unless (eq (magnus-review-lifecycle review) 'open)
    (magnus-review--signal "Cannot launch an attempt for a non-open review"))
  (unless (eq round (magnus-review-latest-round review))
    (magnus-review--signal "Attempts may only be appended to the latest round"))
  (unless (memq (magnus-review-round-execution round)
                '(queued failed interrupted))
    (magnus-review--signal
     "Attempts cannot be appended to a round in %s state"
     (magnus-review-round-execution round)))
  (when (and token (not (magnus-review--valid-token-p token)))
    (magnus-review--signal "Invalid review attempt token"))
  (when-let ((latest (magnus-review-latest-attempt round)))
    (unless (memq (magnus-review-attempt-execution latest)
                  magnus-review--terminal-attempt-states)
      (magnus-review--signal "Latest attempt is still active")))
  (let* ((number (1+ (length (magnus-review-round-attempts round))))
         (now (float-time))
         (attempt
          (magnus-review-attempt--create
           :number number
           :token (or token (magnus-review--random-token))
           :started-at now
           :execution 'starting
           :metadata metadata)))
    (setf (magnus-review-round-attempts round)
          (append (magnus-review-round-attempts round) (list attempt))
          (magnus-review-round-execution round) 'starting
          (magnus-review-execution review) 'starting
          (magnus-review-updated-at review) now)
    (magnus-review--ensure-review-directories review round)
    (magnus-review-save review)
    attempt))

(defun magnus-review--verify-current-attempt
    (review round attempt &optional token allowed-states)
  "Verify current REVIEW ROUND ATTEMPT identity and optional TOKEN.
When ALLOWED-STATES is non-nil, ATTEMPT must be in one of them."
  (unless (and (eq round (magnus-review-latest-round review))
               (eq attempt (magnus-review-latest-attempt round)))
    (magnus-review--signal "Review callback belongs to an obsolete attempt"))
  (when (and token
             (not (and (magnus-review--valid-token-p token)
                       (string= token (magnus-review-attempt-token attempt)))))
    (magnus-review--signal "Review callback attempt token does not match"))
  (when (and allowed-states
             (not (memq (magnus-review-attempt-execution attempt)
                        allowed-states)))
    (magnus-review--signal "Attempt is in unexpected %s state"
                           (magnus-review-attempt-execution attempt)))
  attempt)

(defun magnus-review-mark-attempt-running (review round attempt &optional token)
  "Mark the latest starting ATTEMPT in ROUND of REVIEW as running."
  (unless (eq (magnus-review-lifecycle review) 'open)
    (magnus-review--signal "Cannot run an attempt for a non-open review"))
  (magnus-review--verify-current-attempt
   review round attempt token '(starting))
  (setf (magnus-review-attempt-execution attempt) 'running
        (magnus-review-round-execution round) 'running
        (magnus-review-execution review) 'running
        (magnus-review-updated-at review) (float-time))
  (magnus-review-save review)
  attempt)

(defun magnus-review--finish-attempt
    (review round attempt state
            &optional error defer-save token interruption-kind)
  "Finish ATTEMPT in ROUND of REVIEW with STATE and optional ERROR.
When DEFER-SAVE is non-nil, the caller must finish one larger atomic state
transition and call `magnus-review-save'."
  (unless (memq state magnus-review--terminal-attempt-states)
    (magnus-review--signal "Not a terminal attempt state: %S" state))
  (magnus-review--verify-current-attempt
   review round attempt token '(starting running))
  (unless (or (null interruption-kind)
              (and (eq state 'interrupted)
                   (memq interruption-kind
                         magnus-review--interruption-kinds)))
    (magnus-review--signal "Invalid review interruption kind: %S"
                           interruption-kind))
  (let ((now (float-time)))
    (setf (magnus-review-attempt-execution attempt) state
          (magnus-review-attempt-finished-at attempt) now
          (magnus-review-attempt-error attempt)
          (magnus-review--error-string error)
          (magnus-review-attempt-interruption-kind attempt) interruption-kind
          (magnus-review-round-execution round) state
          (magnus-review-execution review) state
          (magnus-review-updated-at review) now)
    (unless (eq state 'complete)
      ;; A failed retry does not erase the last valid verdict/report.
      (setf (magnus-review-round-completed-at round) nil))
    (unless defer-save
      (magnus-review-save review))
    attempt))

(defun magnus-review--completion-artifacts-ready-p (review round)
  "Return non-nil when ROUND's canonical result and report are private files."
  (let ((result (magnus-review-round-result-path review round))
        (report (magnus-review-round-report-path review round)))
    (and (file-regular-p result) (not (file-symlink-p result))
         (file-regular-p report) (not (file-symlink-p report)))))

(defun magnus-review--publish-completed-attempt
    (review round attempt verdict allowed-states &optional token)
  "Atomically publish ATTEMPT with VERDICT from ALLOWED-STATES."
  (unless (memq verdict magnus-review--verdict-states)
    (magnus-review--signal "Invalid review verdict: %S" verdict))
  (unless (eq (magnus-review-lifecycle review) 'open)
    (magnus-review--signal "Cannot publish a non-open review"))
  (magnus-review--verify-current-attempt
   review round attempt token allowed-states)
  (unless (magnus-review--completion-artifacts-ready-p review round)
    (magnus-review--signal
     "Canonical result and report must be durable before completion"))
  (let ((now (float-time)))
    (dolist (artifact (list (magnus-review-round-result-path review round)
                            (magnus-review-round-report-path review round)))
      (set-file-modes artifact #o600))
    ;; This is intentionally one manifest replacement: no terminal state is
    ;; observable before verdict/delivery/read publication is also durable.
    (setf (magnus-review-attempt-execution attempt) 'complete
          (magnus-review-attempt-finished-at attempt)
          (or (magnus-review-attempt-finished-at attempt) now)
          (magnus-review-attempt-error attempt) nil
          (magnus-review-attempt-interruption-kind attempt) nil
          (magnus-review-round-execution round) 'complete
          (magnus-review-round-verdict round) verdict
          (magnus-review-round-completed-at round) now
          (magnus-review-round-delivery-state round) 'pending
          (magnus-review-round-delivery-error round) nil
          (magnus-review-round-read-state round) 'unread
          (magnus-review-execution review) 'complete
          (magnus-review-verdict review) verdict
          (magnus-review-updated-at review) now)
    (magnus-review--refresh-aggregate-notification-states review)
    (magnus-review-save review)
    round))

(defun magnus-review-complete-attempt
    (review round attempt verdict &optional token)
  "Complete active ATTEMPT and publish VERDICT for ROUND of REVIEW."
  (magnus-review--publish-completed-attempt
   review round attempt verdict '(starting running) token))

(defun magnus-review-adopt-completed-attempt
    (review round attempt verdict attempt-token)
  "Publish recovered ATTEMPT whose validated artifacts survived interruption.
ATTEMPT-TOKEN is mandatory because this transition is normally initiated by
startup reconciliation rather than the original process callback."
  (unless (magnus-review--valid-token-p attempt-token)
    (magnus-review--signal "A valid attempt token is required for adoption"))
  (magnus-review--publish-completed-attempt
   review round attempt verdict '(failed interrupted) attempt-token))

(defun magnus-review-fail-attempt (review round attempt error &optional token)
  "Mark ATTEMPT in ROUND of REVIEW failed with ERROR."
  (magnus-review--finish-attempt review round attempt 'failed error nil token))

(defun magnus-review-interrupt-attempt
    (review round attempt &optional reason token interruption-kind)
  "Mark ATTEMPT in ROUND of REVIEW interrupted.
REASON is a durable diagnostic.  INTERRUPTION-KIND may be `manual',
`shutdown', or `crash'; a manual interruption is never auto-retried."
  (magnus-review--finish-attempt
   review round attempt 'interrupted
   (or reason "Review attempt was interrupted") nil token interruption-kind))

(defun magnus-review-record-session-id
    (review round attempt attempt-token session-id)
  "Persist SESSION-ID for the current token-guarded provider ATTEMPT."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (magnus-review--signal "Provider returned an invalid session ID"))
  (magnus-review--verify-current-attempt
   review round attempt attempt-token '(starting running))
  (when (and (magnus-review-session-id review)
             (not (string= (magnus-review-session-id review) session-id)))
    (magnus-review--signal "Provider changed review session identity"))
  (setf (magnus-review-session-id review) session-id
        (magnus-review-updated-at review) (float-time))
  (magnus-review-save review)
  session-id)

(defun magnus-review-mark-delivered (review &optional round)
  "Mark latest or supplied ROUND of REVIEW delivered to its author."
  (setq round (or round (magnus-review-latest-round review)))
  (unless (and round (eq (magnus-review-round-execution round) 'complete))
    (magnus-review--signal "Only a completed review round can be delivered"))
  (unless (eq (magnus-review-round-delivery-state round) 'sent)
    (let ((now (float-time)))
      (setf (magnus-review-round-delivery-state round) 'sent
            (magnus-review-round-delivery-attempts round)
            (1+ (magnus-review-round-delivery-attempts round))
            (magnus-review-round-delivery-error round) nil
            (magnus-review-round-delivered-at round) now
            (magnus-review-updated-at review) now))
    (magnus-review--refresh-aggregate-notification-states review)
    (magnus-review-save review))
  round)

(defun magnus-review-mark-delivery-failed (review error &optional round)
  "Record a failed author delivery for REVIEW ROUND with ERROR."
  (setq round (or round (magnus-review-latest-round review)))
  (unless (and round (eq (magnus-review-round-execution round) 'complete))
    (magnus-review--signal "Only a completed review round can be delivered"))
  (unless (eq (magnus-review-round-delivery-state round) 'sent)
    (setf (magnus-review-round-delivery-state round) 'failed
          (magnus-review-round-delivery-attempts round)
          (1+ (magnus-review-round-delivery-attempts round))
          (magnus-review-round-delivery-error round)
          (magnus-review--error-string error)
          (magnus-review-updated-at review) (float-time))
    (magnus-review--refresh-aggregate-notification-states review)
    (magnus-review-save review))
  round)

(defun magnus-review-mark-read (review &optional round)
  "Mark latest or supplied completed ROUND of REVIEW read by the user."
  (setq round (or round (magnus-review-latest-round review)))
  (unless (and round (eq (magnus-review-round-execution round) 'complete))
    (magnus-review--signal "Only a completed review round can be read"))
  (unless (eq (magnus-review-round-read-state round) 'read)
    (let ((now (float-time)))
      (setf (magnus-review-round-read-state round) 'read
            (magnus-review-round-read-at round) now
            (magnus-review-updated-at review) now))
    (magnus-review--refresh-aggregate-notification-states review)
    (magnus-review-save review))
  round)

(defun magnus-review-close (review)
  "Close REVIEW without deleting its durable reports or worktree."
  (when (memq (magnus-review-execution review) '(starting running))
    (magnus-review--signal "Cannot close a running review"))
  (let ((now (float-time)))
    (setf (magnus-review-lifecycle review) 'closed
          (magnus-review-closed-at review) now
          (magnus-review-updated-at review) now)
    (magnus-review-save review)
    review))

(defun magnus-review-archive (review)
  "Archive REVIEW without deleting its durable reports."
  (when (memq (magnus-review-execution review) '(starting running))
    (magnus-review--signal "Cannot archive a running review"))
  (let ((now (float-time)))
    (setf (magnus-review-lifecycle review) 'archived
          (magnus-review-closed-at review)
          (or (magnus-review-closed-at review) now)
          (magnus-review-archived-at review) now
          (magnus-review-updated-at review) now)
    (magnus-review-save review)
    review))

;;; JSON persistence

(defun magnus-review--symbol-name (value)
  "Return VALUE's symbol name, preserve strings, or nil."
  (cond ((symbolp value) (and value (symbol-name value)))
        ((stringp value) value)
        (t nil)))

(defun magnus-review--json-safe-value (value)
  "Return a recursively JSON-serializable representation of VALUE.
Metadata may use plists, alists, vectors, or ordinary lists.  Ordinary lists
become arrays, while object keys remain strings, symbols, or keywords."
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
    (let ((remaining value)
          result)
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

(defun magnus-review--attempt-to-json (attempt)
  "Return JSON-ready alist for ATTEMPT."
  `((number . ,(magnus-review-attempt-number attempt))
    (token . ,(magnus-review-attempt-token attempt))
    (started_at . ,(magnus-review-attempt-started-at attempt))
    (finished_at . ,(magnus-review-attempt-finished-at attempt))
    (execution . ,(magnus-review--symbol-name
                   (magnus-review-attempt-execution attempt)))
    (error . ,(magnus-review--error-string
               (magnus-review-attempt-error attempt)))
    (interruption_kind . ,(magnus-review--symbol-name
                           (magnus-review-attempt-interruption-kind attempt)))
    (metadata . ,(magnus-review--json-safe-value
                  (magnus-review-attempt-metadata attempt)))))

(defun magnus-review--round-to-json (round)
  "Return JSON-ready alist for ROUND."
  `((number . ,(magnus-review-round-number round))
    (base_oid . ,(magnus-review-round-base-oid round))
    (head_oid . ,(magnus-review-round-head-oid round))
    (previous_head_oid . ,(magnus-review-round-previous-head-oid round))
    (checkpoint_token . ,(magnus-review-round-checkpoint-token round))
    (created_at . ,(magnus-review-round-created-at round))
    (completed_at . ,(magnus-review-round-completed-at round))
    (execution . ,(magnus-review--symbol-name
                   (magnus-review-round-execution round)))
    (verdict . ,(magnus-review--symbol-name
                 (magnus-review-round-verdict round)))
    (delivery_state . ,(magnus-review--symbol-name
                        (magnus-review-round-delivery-state round)))
    (delivery_attempts . ,(magnus-review-round-delivery-attempts round))
    (delivery_error . ,(magnus-review--error-string
                        (magnus-review-round-delivery-error round)))
    (delivered_at . ,(magnus-review-round-delivered-at round))
    (read_state . ,(magnus-review--symbol-name
                    (magnus-review-round-read-state round)))
    (read_at . ,(magnus-review-round-read-at round))
    (attempts . ,(vconcat
                  (mapcar #'magnus-review--attempt-to-json
                          (magnus-review-round-attempts round))))
    (metadata . ,(magnus-review--json-safe-value
                  (magnus-review-round-metadata round)))))

(defun magnus-review--to-json (review)
  "Return JSON-ready manifest alist for REVIEW."
  `((schema_version . ,magnus-review-schema-version)
    (id . ,(magnus-review-id review))
    (project_root . ,(magnus-review-project-root review))
    (project_hash . ,(magnus-review-project-hash review))
    (author_instance_id . ,(magnus-review-author-instance-id review))
    (author_name . ,(magnus-review-author-name review))
    (reviewer_name . ,(magnus-review-reviewer-name review))
    (reviewer_provider . ,(magnus-review--symbol-name
                           (magnus-review-reviewer-provider review)))
    (model . ,(let ((model (magnus-review-model review)))
                (if (symbolp model)
                    (and model (symbol-name model))
                  model)))
    (effort . ,(magnus-review--symbol-name (magnus-review-effort review)))
    (task . ,(magnus-review-task review))
    (lifecycle . ,(magnus-review--symbol-name
                   (magnus-review-lifecycle review)))
    (execution . ,(magnus-review--symbol-name
                   (magnus-review-execution review)))
    (verdict . ,(magnus-review--symbol-name (magnus-review-verdict review)))
    (delivery_state . ,(magnus-review--symbol-name
                        (magnus-review-delivery-state review)))
    (read_state . ,(magnus-review--symbol-name
                    (magnus-review-read-state review)))
    (session_id . ,(magnus-review-session-id review))
    (checkpoint_token . ,(magnus-review-checkpoint-token review))
    (checkpoint_acks
     . ,(vconcat
         (mapcar (lambda (ack) (vector (car ack) (cdr ack)))
                 (magnus-review-checkpoint-acks review))))
    (created_at . ,(magnus-review-created-at review))
    (updated_at . ,(magnus-review-updated-at review))
    (closed_at . ,(magnus-review-closed-at review))
    (archived_at . ,(magnus-review-archived-at review))
    (base_oid . ,(magnus-review-base-oid review))
    (head_oid . ,(magnus-review-head-oid review))
    (previous_head_oid . ,(magnus-review-previous-head-oid review))
    (rounds . ,(vconcat
                (mapcar #'magnus-review--round-to-json
                        (magnus-review-rounds review))))
    (metadata . ,(magnus-review--json-safe-value
                  (magnus-review-metadata review)))))

(defun magnus-review--validate-state (value states kind &optional allow-nil)
  "Parse string VALUE as one of STATES, naming it KIND."
  (if (and allow-nil (null value))
      nil
    (let ((symbol (and (stringp value) (intern-soft value))))
      (unless (memq symbol states)
        (magnus-review--signal "Invalid %s state in manifest: %S" kind value))
      symbol)))

(defun magnus-review--optional-symbol (value kind)
  "Parse bounded symbol string VALUE, naming it KIND."
  (when value
    (unless (and (stringp value)
                 (<= 1 (length value) 100)
                 (string-match-p "\\`[[:alnum:]_.-]+\\'" value))
      (magnus-review--signal "Invalid %s in manifest: %S" kind value))
    (intern value)))

(defun magnus-review--require-number (value kind &optional allow-nil)
  "Return numeric VALUE or signal, naming it KIND."
  (unless (or (numberp value) (and allow-nil (null value)))
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  value)

(defun magnus-review--require-oid (value kind &optional allow-nil)
  "Return full OID VALUE or signal, naming it KIND."
  (unless (or (magnus-review--valid-oid-p value)
              (and allow-nil (null value)))
    (magnus-review--signal "Invalid %s in manifest: %S" kind value))
  (and value (downcase value)))

(defun magnus-review--checkpoint-acks-from-json (object)
  "Deserialize checkpoint token-to-round acknowledgement OBJECT."
  (mapcar
   (lambda (entry)
     (pcase entry
       (`(,token ,round-number)
        (unless (and (magnus-review--valid-token-p token)
                     (integerp round-number)
                     (> round-number 0))
          (magnus-review--signal
           "Invalid checkpoint acknowledgement: %S" entry))
        (cons token round-number))
       (_ (magnus-review--signal
           "Malformed checkpoint acknowledgement: %S" entry))))
   object))

(defun magnus-review--attempt-from-json (object expected-number)
  "Deserialize attempt OBJECT, requiring EXPECTED-NUMBER."
  (let ((number (alist-get 'number object)))
    (unless (eql number expected-number)
      (magnus-review--signal "Non-append-only attempt sequence at %S" number))
    (magnus-review-attempt--create
     :number number
     :token (alist-get 'token object)
     :started-at (magnus-review--require-number
                  (alist-get 'started_at object) "attempt start time")
     :finished-at (magnus-review--require-number
                   (alist-get 'finished_at object) "attempt finish time" t)
     :execution (magnus-review--validate-state
                 (alist-get 'execution object)
                 magnus-review--execution-states "attempt execution")
     :error (alist-get 'error object)
     :interruption-kind
     (magnus-review--validate-state
      (alist-get 'interruption_kind object)
      magnus-review--interruption-kinds "attempt interruption kind" t)
     :metadata (alist-get 'metadata object))))

(defun magnus-review--round-from-json (object expected-number)
  "Deserialize round OBJECT, requiring EXPECTED-NUMBER."
  (let ((number (alist-get 'number object)))
    (unless (eql number expected-number)
      (magnus-review--signal "Non-append-only round sequence at %S" number))
    (let ((attempt-number 0))
      (magnus-review-round--create
       :number number
       :base-oid (magnus-review--require-oid
                  (alist-get 'base_oid object) "round base OID")
       :head-oid (magnus-review--require-oid
                  (alist-get 'head_oid object) "round head OID")
       :previous-head-oid (magnus-review--require-oid
                           (alist-get 'previous_head_oid object)
                           "round previous head OID" t)
       :checkpoint-token (alist-get 'checkpoint_token object)
       :created-at (magnus-review--require-number
                    (alist-get 'created_at object) "round creation time")
       :completed-at (magnus-review--require-number
                      (alist-get 'completed_at object)
                      "round completion time" t)
       :execution (magnus-review--validate-state
                   (alist-get 'execution object)
                   magnus-review--execution-states "round execution")
       :verdict (magnus-review--validate-state
                 (alist-get 'verdict object)
                 magnus-review--verdict-states "round verdict" t)
       :delivery-state (magnus-review--validate-state
                        (alist-get 'delivery_state object)
                        magnus-review--delivery-states "round delivery")
       :delivery-attempts
       (let ((attempts (or (alist-get 'delivery_attempts object) 0)))
         (unless (and (integerp attempts) (>= attempts 0))
           (magnus-review--signal
            "Invalid delivery attempt count in manifest: %S" attempts))
         attempts)
       :delivery-error (alist-get 'delivery_error object)
       :delivered-at (magnus-review--require-number
                      (alist-get 'delivered_at object)
                      "delivery time" t)
       :read-state (magnus-review--validate-state
                    (alist-get 'read_state object)
                    magnus-review--read-states "round read")
       :read-at (magnus-review--require-number
                 (alist-get 'read_at object) "read time" t)
       :attempts
       (mapcar (lambda (attempt)
                 (magnus-review--attempt-from-json
                  attempt (cl-incf attempt-number)))
               (append (alist-get 'attempts object) nil))
       :metadata (alist-get 'metadata object)))))

(defun magnus-review--from-json (object &optional expected-id expected-hash)
  "Deserialize review OBJECT, validating optional storage identity."
  (unless (eql (alist-get 'schema_version object)
               magnus-review-schema-version)
    (magnus-review--signal
     "Unsupported review schema version: %S"
     (alist-get 'schema_version object)))
  (let* ((id (alist-get 'id object))
         (project-root (alist-get 'project_root object))
         (project-hash (alist-get 'project_hash object))
         (round-number 0))
    (unless (magnus-review--valid-id-p id)
      (magnus-review--signal "Invalid review ID in manifest: %S" id))
    (unless (and (magnus-review--valid-hash-p project-hash)
                 (string= project-hash
                          (magnus-review-compute-project-hash project-root)))
      (magnus-review--signal "Project hash does not match manifest root"))
    (when (and expected-id (not (string= expected-id id)))
      (magnus-review--signal "Manifest ID does not match its directory"))
    (when (and expected-hash (not (string= expected-hash project-hash)))
      (magnus-review--signal "Manifest project hash does not match its directory"))
    (let ((review
           (magnus-review--create
     :id id
     :project-root (magnus-review--canonical-directory project-root)
     :project-hash project-hash
     :author-instance-id (alist-get 'author_instance_id object)
     :author-name (alist-get 'author_name object)
     :reviewer-name (alist-get 'reviewer_name object)
     :reviewer-provider (magnus-review--optional-symbol
                         (alist-get 'reviewer_provider object) "provider")
     :model (alist-get 'model object)
     :effort (magnus-review--optional-symbol
              (alist-get 'effort object) "effort")
     :task (alist-get 'task object)
     :lifecycle (magnus-review--validate-state
                 (alist-get 'lifecycle object)
                 magnus-review--lifecycle-states "lifecycle")
     :execution (magnus-review--validate-state
                 (alist-get 'execution object)
                 magnus-review--execution-states "execution")
     :verdict (magnus-review--validate-state
               (alist-get 'verdict object)
               magnus-review--verdict-states "verdict" t)
     :delivery-state (magnus-review--validate-state
                      (alist-get 'delivery_state object)
                      magnus-review--delivery-states "delivery")
     :read-state (magnus-review--validate-state
                  (alist-get 'read_state object)
                  magnus-review--read-states "read")
     :session-id (alist-get 'session_id object)
     :checkpoint-token (alist-get 'checkpoint_token object)
     :checkpoint-acks
     (magnus-review--checkpoint-acks-from-json
      (alist-get 'checkpoint_acks object))
     :created-at (magnus-review--require-number
                  (alist-get 'created_at object) "creation time")
     :updated-at (magnus-review--require-number
                  (alist-get 'updated_at object) "update time")
     :closed-at (magnus-review--require-number
                 (alist-get 'closed_at object) "close time" t)
     :archived-at (magnus-review--require-number
                   (alist-get 'archived_at object) "archive time" t)
     :base-oid (magnus-review--require-oid
                (alist-get 'base_oid object) "base OID" t)
     :head-oid (magnus-review--require-oid
                (alist-get 'head_oid object) "head OID" t)
     :previous-head-oid (magnus-review--require-oid
                         (alist-get 'previous_head_oid object)
                         "previous head OID" t)
     :rounds
     (mapcar (lambda (round)
               (magnus-review--round-from-json round (cl-incf round-number)))
             (append (alist-get 'rounds object) nil))
            :metadata (alist-get 'metadata object))))
      ;; The top-level values are convenient cached aggregates, not independent
      ;; truth: older unread or pending rounds must remain visible.
      (magnus-review--refresh-aggregate-notification-states review)
      review)))

(defun magnus-review--validate-invariants (review)
  "Validate cross-field and append-only invariants for REVIEW."
  (unless (and (magnus-review--valid-id-p (magnus-review-id review))
               (magnus-review--valid-hash-p
                (magnus-review-project-hash review))
               (string= (magnus-review-project-hash review)
                        (magnus-review-compute-project-hash
                         (magnus-review-project-root review))))
    (magnus-review--signal "Review storage identity is inconsistent"))
  (unless (and (stringp (magnus-review-author-instance-id review))
               (stringp (magnus-review-author-name review)))
    (magnus-review--signal "Review author identity is incomplete"))
  (unless (magnus-review--valid-token-p
           (magnus-review-checkpoint-token review))
    (magnus-review--signal "Review checkpoint token is invalid"))
  (when (and (magnus-review-session-id review)
             (not (stringp (magnus-review-session-id review))))
    (magnus-review--signal "Review provider session ID is invalid"))
  (let ((round-number 0)
        previous-head
        latest-complete)
    (dolist (round (magnus-review-rounds review))
      (unless (= (magnus-review-round-number round) (cl-incf round-number))
        (magnus-review--signal "Review rounds are not append-only"))
      (unless (and (magnus-review--valid-oid-p
                    (magnus-review-round-base-oid round))
                   (magnus-review--valid-oid-p
                    (magnus-review-round-head-oid round))
                   (or (> round-number 1)
                       (not (string= (magnus-review-round-base-oid round)
                                     (magnus-review-round-head-oid round)))))
        (magnus-review--signal "Review round %d has invalid Git evidence"
                               round-number))
      (unless (or (null (magnus-review-base-oid review))
                  (string= (magnus-review-base-oid review)
                           (magnus-review-round-base-oid round)))
        (magnus-review--signal "Review base changed between rounds"))
      (unless (equal (magnus-review-round-previous-head-oid round)
                     previous-head)
        (magnus-review--signal "Review round %d has incorrect previous head"
                               round-number))
      (when (and (magnus-review-round-checkpoint-token round)
                 (not (magnus-review--valid-token-p
                       (magnus-review-round-checkpoint-token round))))
        (magnus-review--signal "Review round %d has an invalid checkpoint token"
                               round-number))
      (let ((attempt-number 0)
            latest-attempt)
        (dolist (attempt (magnus-review-round-attempts round))
          (unless (= (magnus-review-attempt-number attempt)
                     (cl-incf attempt-number))
            (magnus-review--signal "Round %d attempts are not append-only"
                                   round-number))
          (unless (magnus-review--valid-token-p
                   (magnus-review-attempt-token attempt))
            (magnus-review--signal "Round %d has an invalid attempt token"
                                   round-number))
          (unless
              (or (null (magnus-review-attempt-interruption-kind attempt))
                  (and
                   (eq (magnus-review-attempt-execution attempt) 'interrupted)
                   (memq (magnus-review-attempt-interruption-kind attempt)
                         magnus-review--interruption-kinds)))
            (magnus-review--signal
             "Round %d attempt %d has invalid interruption metadata"
             round-number attempt-number))
          (setq latest-attempt attempt))
        (when (and latest-attempt
                   (not (eq (magnus-review-attempt-execution latest-attempt)
                            (magnus-review-round-execution round))))
          (magnus-review--signal
           "Round %d execution disagrees with its latest attempt" round-number))
        (when (and (null latest-attempt)
                   (not (eq (magnus-review-round-execution round) 'queued)))
          (magnus-review--signal "Round %d has execution without an attempt"
                                 round-number)))
      (if (eq (magnus-review-round-execution round) 'complete)
          (progn
            (unless (and (memq (magnus-review-round-verdict round)
                               magnus-review--verdict-states)
                         (numberp (magnus-review-round-completed-at round))
                         (memq (magnus-review-round-delivery-state round)
                               '(pending sent failed))
                         (memq (magnus-review-round-read-state round)
                               '(unread read)))
              (magnus-review--signal
               "Completed round %d is only partially published" round-number))
            (setq latest-complete round))
        (when (or (magnus-review-round-verdict round)
                  (magnus-review-round-completed-at round)
                  (not (eq (magnus-review-round-delivery-state round)
                           'not-ready))
                  (not (eq (magnus-review-round-read-state round) 'not-ready)))
          (magnus-review--signal
           "Non-complete round %d contains published result state" round-number)))
      (setq previous-head (magnus-review-round-head-oid round)))
    (let (round-checkpoints acknowledged-tokens)
      (dolist (round (magnus-review-rounds review))
        (when-let ((token (magnus-review-round-checkpoint-token round)))
          (when (assoc token round-checkpoints)
            (magnus-review--signal
             "Checkpoint token identifies more than one review round"))
          (push (cons token round) round-checkpoints)))
      (dolist (ack (magnus-review-checkpoint-acks review))
        (let* ((token (car-safe ack))
               (round-number (cdr-safe ack))
               (round (and (integerp round-number)
                           (> round-number 0)
                           (nth (1- round-number)
                                (magnus-review-rounds review))))
               (eventual-round (cdr (assoc token round-checkpoints))))
          (unless (and (magnus-review--valid-token-p token)
                       round
                       (= round-number
                          (magnus-review-round-number round)))
            (magnus-review--signal
             "Checkpoint acknowledgement has an invalid round: %S" ack))
          (when (member token acknowledged-tokens)
            (magnus-review--signal
             "Checkpoint token has more than one acknowledgement"))
          (when (and eventual-round
                     (not
                      (and
                       (= (magnus-review-round-number eventual-round)
                          (1+ round-number))
                       (string=
                        (magnus-review-round-previous-head-oid eventual-round)
                        (magnus-review-round-head-oid round)))))
            (magnus-review--signal
             "Acknowledged checkpoint token advanced from the wrong round"))
          (push token acknowledged-tokens))))
    (let ((latest (magnus-review-latest-round review)))
      (if latest
          (unless (and (string= (magnus-review-base-oid review)
                                (magnus-review-round-base-oid latest))
                       (string= (magnus-review-head-oid review)
                                (magnus-review-round-head-oid latest))
                       (equal (magnus-review-previous-head-oid review)
                              (magnus-review-round-previous-head-oid latest)))
            (magnus-review--signal "Review scope cache disagrees with latest round"))
        (when (or (magnus-review-base-oid review)
                  (magnus-review-head-oid review)
                  (magnus-review-previous-head-oid review))
          (magnus-review--signal "Review without rounds contains Git scope")))
      (unless
          (or (and (eq (magnus-review-execution review)
                       'waiting-for-checkpoint)
                   (or (null latest)
                       (eq (magnus-review-round-execution latest) 'complete)))
              (and latest
                   (eq (magnus-review-execution review)
                       (magnus-review-round-execution latest))))
        (magnus-review--signal "Review execution disagrees with latest round")))
    (unless (equal (magnus-review-verdict review)
                   (and latest-complete
                        (magnus-review-round-verdict latest-complete)))
      (magnus-review--signal "Review verdict cache is inconsistent")))
  review)

(defun magnus-review--run-changed-hooks ()
  "Run review observers independently after a durable state transition.
Observer failures are diagnostics, never transaction failures: by the time
this runs the manifest has already been atomically replaced on disk."
  (run-hook-wrapped
   'magnus-reviews-changed-hook
   (lambda (function)
     (condition-case err
         (funcall function)
       (error
        (message "Magnus: review state observer %S failed: %s"
                 function (error-message-string err))))
     ;; `run-hook-wrapped' stops at the first non-nil wrapper result.
     nil)))

(defun magnus-review-save (review)
  "Persist REVIEW atomically and privately."
  (unless (magnus-review-p review)
    (magnus-review--signal "Not a Magnus review: %S" review))
  (setf (magnus-review-updated-at review) (float-time))
  (magnus-review--refresh-aggregate-notification-states review)
  (magnus-review--validate-invariants review)
  (magnus-review--ensure-review-directories review)
  (let ((json-encoding-pretty-print nil))
    (magnus-review--atomic-write-string
     (magnus-review-manifest-path review)
     (concat (json-serialize (magnus-review--to-json review)
                             :null-object nil :false-object :json-false)
             "\n")))
  (unless (memq review magnus-reviews)
    (push review magnus-reviews))
  (magnus-review--run-changed-hooks)
  review)

(defun magnus-review--read-json-file (file)
  "Read bounded JSON object from FILE without following a symlink."
  (when (file-symlink-p file)
    (magnus-review--signal "Refusing symlinked review manifest: %s" file))
  (unless (file-regular-p file)
    (magnus-review--signal "Review manifest is not a regular file: %s" file))
  (let ((size (file-attribute-size (file-attributes file))))
    (when (> size (* 10 1024 1024))
      (magnus-review--signal "Review manifest is unexpectedly large: %s" file)))
  (set-file-modes file #o600)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil :false-object :json-false)))

(defun magnus-review-load-file (file &optional expected-id expected-hash)
  "Load one review manifest FILE and register it.
EXPECTED-ID and EXPECTED-HASH validate its managed storage path."
  (let ((review
         (magnus-review--validate-invariants
          (magnus-review--from-json
           (magnus-review--read-json-file file)
           expected-id expected-hash))))
    (when-let ((existing (magnus-review-get (magnus-review-id review))))
      (unless (string= (magnus-review-project-hash existing)
                       (magnus-review-project-hash review))
        (magnus-review--signal "Duplicate review ID across project archives"))
      (setq magnus-reviews (delq existing magnus-reviews)))
    (push review magnus-reviews)
    review))

(defun magnus-review--recover-one (review)
  "Turn stale starting/running state in REVIEW into interrupted state."
  (let ((changed nil)
        (now (float-time)))
    (dolist (round (magnus-review-rounds review))
      (dolist (attempt (magnus-review-round-attempts round))
        (when (memq (magnus-review-attempt-execution attempt)
                    '(starting running))
          (setf (magnus-review-attempt-execution attempt) 'interrupted
                (magnus-review-attempt-finished-at attempt) now
                (magnus-review-attempt-error attempt)
                (or (magnus-review-attempt-error attempt)
                    "Emacs exited while this review attempt was active")
                (magnus-review-attempt-interruption-kind attempt) 'crash)
          (setq changed t)))
      (when (memq (magnus-review-round-execution round) '(starting running))
        (setf (magnus-review-round-execution round) 'interrupted)
        (setq changed t)))
    (when (memq (magnus-review-execution review) '(starting running))
      (setf (magnus-review-execution review) 'interrupted)
      (setq changed t))
    (when changed
      (setf (magnus-review-updated-at review) now)
      (magnus-review-save review))
    changed))

(defun magnus-review-load-all ()
  "Load all durable reviews and recover abandoned active attempts.
Malformed records are skipped with a warning; valid records remain available."
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
            (dolist (review-entry (directory-files project-entry t "\\`[^.]" t))
              (let* ((review-id (file-name-nondirectory review-entry))
                     (manifest (expand-file-name "manifest.json" review-entry)))
                (when (and (file-directory-p review-entry)
                           (not (file-symlink-p review-entry))
                           (magnus-review--valid-id-p review-id)
                           (file-exists-p manifest))
                  (set-file-modes review-entry #o700)
                  (condition-case error-data
                      (let ((review (magnus-review-load-file
                                     manifest review-id project-hash)))
                        (cl-incf loaded)
                        (magnus-review--recover-one review))
                    (error
                     (display-warning
                      'magnus-review
                      (format "Skipping review manifest %s: %s"
                              manifest (error-message-string error-data))
                      :warning))))))))))
    (setq magnus-reviews
          (sort magnus-reviews
                (lambda (left right)
                  (> (or (magnus-review-created-at left) 0)
                     (or (magnus-review-created-at right) 0)))))
    (magnus-review--run-changed-hooks)
    loaded))

;;; Managed immutable review worktree

(defun magnus-review--worktree-marker-object
    (review head state &optional previous-head)
  "Return ownership marker for REVIEW at HEAD in STATE.
PREVIOUS-HEAD records the safe rollback point of a preparing update."
  `((schema_version . 1)
    (state . ,state)
    (review_id . ,(magnus-review-id review))
    (project_hash . ,(magnus-review-project-hash review))
    (project_root . ,(magnus-review-project-root review))
    (checkout . ,(magnus-review-checkout-path review))
    (common_git_dir
     . ,(magnus-review--git-common-directory
         (magnus-review-project-root review)))
    (head_oid . ,head)
    (previous_head_oid . ,previous-head)
    (created_at . ,(float-time))))

(defun magnus-review--write-worktree-marker
    (review head state &optional previous-head)
  "Write REVIEW's ownership marker for HEAD in STATE and PREVIOUS-HEAD."
  (magnus-review--atomic-write-string
   (magnus-review--worktree-marker-path review)
   (concat (json-serialize
            (magnus-review--worktree-marker-object
             review head state previous-head)
                           :null-object nil :false-object :json-false)
           "\n")))

(defun magnus-review--worktree-marker-matches-p (review marker)
  "Return non-nil when MARKER identifies REVIEW's derived checkout and repo."
  (let ((checkout (magnus-review-checkout-path review)))
    (and (eql (alist-get 'schema_version marker) 1)
         (member (alist-get 'state marker) '("preparing" "ready"))
         (stringp (alist-get 'review_id marker))
         (string= (alist-get 'review_id marker) (magnus-review-id review))
         (stringp (alist-get 'project_hash marker))
         (string= (alist-get 'project_hash marker)
                  (magnus-review-project-hash review))
         (stringp (alist-get 'project_root marker))
         (string= (alist-get 'project_root marker)
                  (magnus-review-project-root review))
         (stringp (alist-get 'checkout marker))
         (string= (alist-get 'checkout marker) checkout)
         (stringp (alist-get 'common_git_dir marker))
         (or (null (alist-get 'previous_head_oid marker))
             (magnus-review--valid-oid-p
              (alist-get 'previous_head_oid marker)))
         (equal (file-truename (alist-get 'common_git_dir marker))
                (file-truename
                 (magnus-review--git-common-directory
                  (magnus-review-project-root review)))))))

(defun magnus-review--owned-worktree-p (review)
  "Return non-nil when REVIEW's checkout has a matching ownership marker."
  (let ((marker-path (magnus-review--worktree-marker-path review))
        (checkout (magnus-review-checkout-path review)))
    (and (file-exists-p marker-path)
         (not (file-symlink-p marker-path))
         (file-directory-p checkout)
         (not (file-symlink-p checkout))
         (condition-case err
             (let ((marker (magnus-review--read-json-file marker-path)))
               (and (magnus-review--worktree-marker-matches-p review marker)
                    (string= (alist-get 'state marker) "ready")
                    (equal (file-truename
                            (magnus-review--git-output
                             checkout "rev-parse" "--show-toplevel"))
                           (file-truename checkout))
                    (equal (file-truename
                            (magnus-review--git-common-directory checkout))
                           (file-truename
                            (alist-get 'common_git_dir marker)))
                    (string= (magnus-review-resolve-oid checkout "HEAD")
                             (alist-get 'head_oid marker))))
           (error
            (message "Magnus: could not verify review worktree ownership: %s"
                     (error-message-string err))
            nil)))))

(defun magnus-review-worktree-repair (review)
  "Adopt a checkout left between worktree creation and ready-marker commit.
Return non-nil when an owned worktree is ready.  A preparing marker without a
checkout is removed so a later create can safely retry."
  (let ((marker-path (magnus-review--worktree-marker-path review))
        (checkout (magnus-review-checkout-path review)))
    (when (file-exists-p marker-path)
      (let ((marker (magnus-review--read-json-file marker-path)))
        (unless (magnus-review--worktree-marker-matches-p review marker)
          (magnus-review--signal "Refusing mismatched worktree marker"))
        (cond
         ((not (file-exists-p checkout))
          (unless (string= (alist-get 'state marker) "preparing")
            (magnus-review--signal "Ready marker has lost its worktree"))
          (delete-file marker-path)
          nil)
         ((or (file-symlink-p checkout) (not (file-directory-p checkout)))
          (magnus-review--signal "Review checkout is not a real directory"))
         (t
          (let ((actual (magnus-review-resolve-oid checkout "HEAD")))
            (unless (and
                     (equal (file-truename
                             (magnus-review--git-output
                              checkout "rev-parse" "--show-toplevel"))
                            (file-truename checkout))
                     (equal (file-truename
                             (magnus-review--git-common-directory checkout))
                            (file-truename
                             (alist-get 'common_git_dir marker)))
                     (or (string= actual (alist-get 'head_oid marker))
                         (and (string= (alist-get 'state marker) "preparing")
                              (alist-get 'previous_head_oid marker)
                              (string= actual
                                       (alist-get 'previous_head_oid marker)))))
            (magnus-review--signal
             "Preparing checkout does not match its ownership marker"))
            (magnus-review--write-worktree-marker review actual "ready")
            t)))))))

(defun magnus-review-worktree-create (review &optional head-revision)
  "Create REVIEW's detached managed worktree at HEAD-REVISION.
If the owned worktree already exists, update it safely instead."
  (let* ((project-root (magnus-review-project-root review))
         (head (magnus-review-resolve-oid
                project-root (or head-revision (magnus-review-head-oid review))))
         (checkout (magnus-review-checkout-path review))
         (marker (magnus-review--worktree-marker-path review))
         (created nil))
    (magnus-review--ensure-review-directories review)
    (cond
     ((file-exists-p checkout)
      (unless (or (magnus-review--owned-worktree-p review)
                  (magnus-review-worktree-repair review))
        (magnus-review--signal
         "Refusing unowned checkout at managed path: %s" checkout))
      (magnus-review-worktree-update review head))
     ((file-exists-p marker)
      (magnus-review-worktree-repair review)
      (magnus-review-worktree-create review head))
     (t
      (condition-case error-data
          (progn
            ;; Persist intent first.  If Emacs dies after Git creates the
            ;; worktree, `magnus-review-worktree-repair' can prove and adopt it.
            (magnus-review--write-worktree-marker review head "preparing")
            (magnus-review--git-output project-root
                                       "worktree" "add" "--detach"
                                       checkout head)
            (setq created t)
            (set-file-modes checkout #o700)
            (magnus-review--write-worktree-marker review head "ready")
            checkout)
        (error
         (when (and created
                    (not (magnus-review--owned-worktree-p review)))
           ;; This path was proven absent above and created by this invocation.
           (ignore-errors
             (magnus-review--git-output project-root
                                        "worktree" "remove" "--force"
                                        checkout)))
         (when (and (file-exists-p marker)
                    (not (file-exists-p checkout)))
           (ignore-errors (delete-file marker)))
         (signal (car error-data) (cdr error-data))))))))

(defun magnus-review-worktree-update (review &optional head-revision)
  "Move REVIEW's owned, clean worktree to exact HEAD-REVISION."
  (unless (magnus-review--owned-worktree-p review)
    (magnus-review--signal "Review worktree is absent or not owned by Magnus"))
  (let* ((checkout (magnus-review-checkout-path review))
         (head (magnus-review-resolve-oid
                (magnus-review-project-root review)
                (or head-revision (magnus-review-head-oid review))))
         (previous-head (magnus-review-resolve-oid checkout "HEAD"))
         (dirty (magnus-review-worktree-dirty-status checkout)))
    (when dirty
      (magnus-review--signal
       "Refusing to overwrite modified review worktree %s:\n%s"
       checkout dirty))
    (if (string= previous-head head)
        checkout
      ;; A kill between checkout and the final marker can be reconciled to
      ;; either the target or the proven previous HEAD.
      (magnus-review--write-worktree-marker
       review head "preparing" previous-head)
      (condition-case error-data
          (progn
            (magnus-review--git-output checkout "checkout" "--detach" head)
            (let ((actual (magnus-review-resolve-oid checkout "HEAD")))
              (unless (string= actual head)
                (magnus-review--signal
                 "Review worktree stopped at unexpected commit %s" actual)))
            (magnus-review--write-worktree-marker review head "ready")
            checkout)
        (error
         ;; Repair settles a clean previous/target state; a third state remains
         ;; deliberately quarantined for manual inspection.
         (ignore-errors (magnus-review-worktree-repair review))
         (signal (car error-data) (cdr error-data)))))))

(defun magnus-review-worktree-cleanup (review &optional force)
  "Remove REVIEW's owned worktree, preserving reports and manifest.
Without FORCE, Git refuses cleanup if anything modified the review checkout.
Even with FORCE, cleanup never operates outside REVIEW's derived checkout."
  (let ((checkout (magnus-review-checkout-path review))
        (marker (magnus-review--worktree-marker-path review))
        (review-directory (file-name-as-directory
                           (magnus-review-directory review))))
    (unless (string-prefix-p review-directory (expand-file-name checkout))
      (magnus-review--signal "Derived checkout escaped the review directory"))
    (cond
     ((file-exists-p checkout)
      (unless (or (magnus-review--owned-worktree-p review)
                  (magnus-review-worktree-repair review))
        (magnus-review--signal
         "Refusing to remove unowned worktree: %s" checkout))
      (apply #'magnus-review--git-output
             (magnus-review-project-root review)
             (append (list "worktree" "remove")
                     (when force (list "--force"))
                     (list checkout)))
      (when (file-exists-p marker)
        (delete-file marker))
      t)
     ((file-exists-p marker)
      ;; The checkout is already gone; only delete a matching marker.
      (let ((object (magnus-review--read-json-file marker)))
        (unless (magnus-review--worktree-marker-matches-p review object)
          (magnus-review--signal "Refusing mismatched worktree marker"))
        (delete-file marker)
        t))
     (t nil))))

;;; Coordination checkpoint integration

(defun magnus-review-handle-ready-marker (directory marker)
  "Validate a coordination review-ready MARKER emitted in DIRECTORY.
MARKER is a plist with :request-id, :checkpoint-token, :base, and :head strings.
A new valid marker appends an immutable round and runs
`magnus-review-ready-hook'.  An unchanged re-review checkpoint is acknowledged
without duplicating the round or launching a model, and its request keeps
waiting for new committed evidence.  Replayed markers never append a duplicate
round, but a queued round re-runs the hook so startup recovery can continue
launching it idempotently."
  (let* ((request-id (plist-get marker :request-id))
         (token (plist-get marker :checkpoint-token))
         (base (plist-get marker :base))
         (head (plist-get marker :head))
         (review (and request-id (magnus-review-get request-id))))
    (when review
      (unless (stringp token)
        (magnus-review--signal "Review-ready checkpoint token is missing"))
      (unless (and (magnus-review--valid-oid-p base)
                   (magnus-review--valid-oid-p head))
        (magnus-review--signal
         "Review-ready checkpoints require full 40- or 64-hex object IDs"))
      (unless (string= (magnus-review-git-root directory)
                       (magnus-review-project-root review))
        (magnus-review--signal "Review-ready marker came from the wrong project"))
      (setq base (downcase base)
            head (downcase head))
      (let* ((latest (magnus-review-latest-round review))
             (persisted
              (seq-find
               (lambda (round)
                 (string= token
                          (or (magnus-review-round-checkpoint-token round) "")))
               (magnus-review-rounds review)))
             (acknowledged
              (magnus-review--checkpoint-ack-round-for-token review token))
             (persisted-match
              (and persisted
                   (string= base (magnus-review-round-base-oid persisted))
                   (string= head (magnus-review-round-head-oid persisted))))
             (acknowledged-match
              (and acknowledged
                   (string= base (magnus-review-round-base-oid acknowledged))
                   (string= head (magnus-review-round-head-oid acknowledged)))))
        (cond
         ;; Startup replays the full coordination log.  Historical checkpoint
         ;; tokens remain immutable identities even after the review advances to
         ;; a newer current token, and their Git objects may since have been GC'd.
         (persisted-match
          (when (and (eq persisted latest)
                     (eq (magnus-review-round-execution persisted) 'queued))
            (run-hook-with-args 'magnus-review-ready-hook review persisted))
          persisted)
         ;; A re-review request may legitimately produce no new commit.  Its
         ;; fresh token remains a durable event identity so startup replay can
         ;; accept it without manufacturing a duplicate round or model run.
         (acknowledged-match acknowledged)
         ;; Once the token binds to a real round, only that canonical scope or
         ;; its earlier acknowledged no-progress scope may replay successfully.
         (persisted
          (magnus-review--signal
           "Replayed review-ready marker changed its Git scope"))
         ((not (string= token
                        (or (magnus-review-checkpoint-token review) "")))
          (magnus-review--signal "Review-ready token does not match request %s"
                                 request-id))
         ((not (eq (magnus-review-execution review) 'waiting-for-checkpoint))
          (magnus-review--signal "Review %s is not waiting for a checkpoint"
                                 request-id))
         (t
          (let ((resolved-base (magnus-review-resolve-oid directory base))
                (resolved-head (magnus-review-resolve-oid directory head)))
            (unless (and (string= resolved-base base)
                         (string= resolved-head head))
              (magnus-review--signal
               "Review-ready values must be exact commit object IDs"))
            (if (and latest
                     (string= resolved-base
                              (magnus-review-round-base-oid latest))
                     (string= resolved-head
                              (magnus-review-round-head-oid latest)))
                (magnus-review--acknowledge-unchanged-checkpoint
                 review token resolved-base resolved-head)
              (let ((round (magnus-review-append-round
                            review resolved-base resolved-head
                            :checkpoint-token token)))
                (run-hook-with-args 'magnus-review-ready-hook review round)
                round)))))))))

(defun magnus-review-setup-coordination ()
  "Register review-ready marker handling when coordination is available."
  (add-hook 'magnus-coord-review-ready-hook
            #'magnus-review-handle-ready-marker))

(provide 'magnus-review)
;;; magnus-review.el ends here
