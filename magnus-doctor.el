;;; magnus-doctor.el --- Diagnose a Magnus installation -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `magnus-doctor' performs read-only checks for the editor libraries, command
;; line tools, storage paths, and active-instance directories Magnus relies on.
;; It intentionally reports optional provider CLIs as warnings: Claude-only and
;; Codex-only installations remain useful, while opposite-provider reviews need
;; both.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magnus-instances)

(declare-function magnus-coord-review-retry-diagnostics
                  "magnus-coord" (directory))
(declare-function magnus-coord-watched-directories "magnus-coord" ())
(declare-function magnus-coord-pending-review-directories "magnus-coord" ())

(defvar magnus-state-file)
(defvar magnus-review-directory-root)
(defvar magnus-claude-executable)
(defvar magnus-codex-executable)

(cl-defstruct (magnus-doctor-check
               (:constructor magnus-doctor-check--create))
  "One read-only installation diagnostic."
  id severity summary detail)

(defconst magnus-doctor-buffer-name "*Magnus Doctor*"
  "Name of the Magnus diagnostics buffer.")

(defun magnus-doctor--check (id severity summary &optional detail)
  "Construct a diagnostic named ID with SEVERITY, SUMMARY, and DETAIL."
  (magnus-doctor-check--create
   :id id :severity severity :summary summary :detail detail))

(defun magnus-doctor--library-check (library label)
  "Check that Emacs LIBRARY for LABEL is discoverable."
  (if-let ((path (locate-library library)))
      (magnus-doctor--check
       (intern library) 'ok (format "%s is available" label) path)
    (magnus-doctor--check
     (intern library) 'error (format "%s is missing" label)
     (format "Install the `%s' Emacs package, then restart Emacs." library))))

(defun magnus-doctor--executable-check (id executable label &optional detail)
  "Check configured EXECUTABLE for LABEL under ID.
DETAIL explains the missing command; provider-oriented guidance is the
default."
  (let ((configured (and (stringp executable)
                         (not (string-empty-p executable))
                         executable)))
    (if-let ((path (and configured (executable-find configured))))
        (magnus-doctor--check
         id 'ok (format "%s CLI is available" label) path)
      (magnus-doctor--check
       id 'warning (format "%s CLI is unavailable" label)
       (or detail
           (format
            (concat
             "Configured command: %s. Interactive %s agents and %s-backed "
             "reviews will be unavailable.")
            (or configured "<unset>") label label))))))

(defun magnus-doctor--existing-ancestor (path)
  "Return the nearest existing ancestor of PATH, or nil."
  (let ((candidate (expand-file-name path))
        parent)
    (while (and (not (file-exists-p candidate))
                (not (equal candidate
                            (setq parent
                                  (directory-file-name
                                   (file-name-directory candidate))))))
      (setq candidate parent))
    (and (file-exists-p candidate) candidate)))

(defun magnus-doctor--storage-check (id path label directory-p)
  "Check writable private PATH named LABEL.
When DIRECTORY-P is non-nil, PATH itself is the managed directory; otherwise
its parent is the location Magnus needs to create or replace a file in."
  (let* ((expanded (expand-file-name path))
         (target (if directory-p expanded (file-name-directory expanded)))
         (target-link-path (directory-file-name target))
         (ancestor (magnus-doctor--existing-ancestor target))
         ;; Atomic file replacement needs a writable parent.  That parent is
         ;; also Magnus-managed durable storage and must remain private even
         ;; before the state file itself has been created.
         (privacy-path (cond ((file-exists-p expanded) expanded)
                             ((file-exists-p target) target))))
    (cond
     ((file-remote-p expanded)
      (magnus-doctor--check
       id 'error (format "%s is remote" label)
       "Magnus durable state must use a local filesystem path."))
     ((or (file-symlink-p expanded)
          (and (not directory-p) (file-symlink-p target-link-path)))
      (magnus-doctor--check
       id 'error (format "%s uses a symlink" label)
       (format "Magnus refuses symlinked managed storage: %s"
               (if (file-symlink-p expanded) expanded target-link-path))))
     ((and (file-exists-p expanded)
           (if directory-p
               (not (file-directory-p expanded))
             (not (file-regular-p expanded))))
      (magnus-doctor--check
       id 'error (format "%s has the wrong path type" label)
       (format "%s must be %s."
               expanded (if directory-p "a directory" "a regular file"))))
     ((and (file-exists-p target) (not (file-directory-p target)))
      (magnus-doctor--check
       id 'error (format "%s parent is not a directory" label) target))
     ((null ancestor)
      (magnus-doctor--check
       id 'error (format "%s has no accessible parent" label) expanded))
     ((not (file-writable-p ancestor))
      (magnus-doctor--check
       id 'error (format "%s is not writable" label) ancestor))
     ((and privacy-path
           (let ((modes (file-modes privacy-path)))
             (and modes (not (zerop (logand modes #o077))))))
      (magnus-doctor--check
       id 'warning (format "%s permissions are broader than private" label)
       (format "%s has mode %o; Magnus-managed durable state should be 600/700."
               privacy-path (file-modes privacy-path))))
     (t
      (magnus-doctor--check
       id 'ok (format "%s is writable" label) expanded)))))

(defun magnus-doctor--instance-checks ()
  "Return diagnostics for every registered Magnus instance."
  (mapcar
   (lambda (instance)
     (let ((name (or (magnus-instance-name instance) "unnamed"))
           (directory (magnus-instance-directory instance)))
       (if (and (stringp directory) (file-directory-p directory))
           (magnus-doctor--check
            (intern (format "instance-%s" (magnus-instance-id instance)))
            'ok (format "Agent %s directory exists" name) directory)
         (magnus-doctor--check
          (intern (format "instance-%s" (magnus-instance-id instance)))
          'error (format "Agent %s directory is unavailable" name)
          (or directory "<unset>")))))
   (magnus-instances-list)))

(defun magnus-doctor--active-project-directories ()
  "Return roots owned by active agents, pending reviews, or live watchers."
  (let (directories)
    (cl-labels
        ((add-directory
          (directory)
          (when (and (stringp directory) (not (string-empty-p directory)))
            (cl-pushnew (file-name-as-directory
                         (file-truename (expand-file-name directory)))
                        directories :test #'equal))))
      (dolist (instance (magnus-instances-active-list))
        (add-directory (magnus-instance-directory instance)))
      ;; Discover pending reviews independently of their watcher. Otherwise a
      ;; failed watcher startup erases the very project Doctor must diagnose.
      (when (fboundp 'magnus-coord-pending-review-directories)
        (dolist (directory (magnus-coord-pending-review-directories))
          (add-directory directory)))
      (when (fboundp 'magnus-coord-watched-directories)
        (dolist (directory (magnus-coord-watched-directories))
          (add-directory directory))))
    (nreverse directories)))

(defun magnus-doctor--coordination-detail (diagnostics)
  "Summarize review checkpoint retry DIAGNOSTICS."
  (let (parts)
    (let ((pending (or (plist-get diagnostics :pending-review-retry-count) 0))
          (exhausted (or (plist-get diagnostics :exhausted-review-count) 0)))
      (when (> pending 0)
        (push (format "%d review checkpoint retr%s pending"
                      pending (if (= pending 1) "y" "ies"))
              parts))
      (when (> exhausted 0)
        (let* ((details (plist-get diagnostics :exhausted-review-details))
               (last-error (plist-get (car details) :last-error)))
          (push
           (concat
            (format "%d review checkpoint retr%s exhausted"
                    exhausted (if (= exhausted 1) "y" "ies"))
            (when last-error (format " (%s)" last-error))
            "; press g in *magnus* to re-arm")
           parts))))
    (if parts
        (string-join (nreverse parts) "; ")
      "No review checkpoint retries are pending.")))

(defun magnus-doctor--coordination-check (directory)
  "Return a read-only coordination watcher check for DIRECTORY."
  (condition-case error-data
      (let* ((retry
              (and (fboundp 'magnus-coord-review-retry-diagnostics)
                   (magnus-coord-review-retry-diagnostics directory)))
             (diagnostics (or retry '()))
             (watched
              (and (fboundp 'magnus-coord-watched-directories)
                   (member (directory-file-name directory)
                           (mapcar #'directory-file-name
                                   (magnus-coord-watched-directories)))))
             (pending
              (or (plist-get diagnostics :pending-review-retry-count) 0))
             (exhausted
              (or (plist-get diagnostics :exhausted-review-count) 0))
             (label (abbreviate-file-name directory))
             (id (intern (concat "coordination-"
                                 (secure-hash 'sha1 directory)))))
        (cond
         ((> exhausted 0)
          (magnus-doctor--check
           id 'error
           (format "Coordination checkpoint retries exhausted for %s" label)
           (magnus-doctor--coordination-detail diagnostics)))
         ((> pending 0)
          (magnus-doctor--check
           id 'warning
           (format "Coordination checkpoint retries pending for %s" label)
           (magnus-doctor--coordination-detail diagnostics)))
         (watched
          (magnus-doctor--check
           id 'ok (format "Coordination watcher is active for %s" label)
           (magnus-doctor--coordination-detail diagnostics)))
         (t
          (magnus-doctor--check
           id 'warning (format "Coordination watcher is idle for %s" label)
           (concat "No watcher is serving this active agent or review. "
                   "Magnus Doctor does not start one because diagnostics "
                   "are read-only.")))))
    (error
     (magnus-doctor--check
      (intern (concat "coordination-" (secure-hash 'sha1 directory)))
      'error
      (format "Coordination diagnostics failed for %s"
              (abbreviate-file-name directory))
      (error-message-string error-data)))))

(defun magnus-doctor--coordination-checks ()
  "Return read-only diagnostics for agent- and review-owned projects."
  (mapcar #'magnus-doctor--coordination-check
          (magnus-doctor--active-project-directories)))

(defun magnus-doctor-run ()
  "Return all current Magnus diagnostics without changing external state."
  (let* ((claude
          (magnus-doctor--executable-check
           'claude
           (and (boundp 'magnus-claude-executable) magnus-claude-executable)
           "Claude"))
         (codex
          (magnus-doctor--executable-check
           'codex
           (and (boundp 'magnus-codex-executable) magnus-codex-executable)
           "Codex"))
         (provider
          (if (or (eq (magnus-doctor-check-severity claude) 'ok)
                  (eq (magnus-doctor-check-severity codex) 'ok))
              (magnus-doctor--check
               'provider 'ok "At least one agent provider is available")
            (magnus-doctor--check
             'provider 'error "No agent provider is available"
             "Install or configure Claude Code, Codex, or both."))))
    (append
     (list
      (if (version<= "28.1" emacs-version)
          (magnus-doctor--check
           'emacs 'ok (format "Emacs %s is supported" emacs-version))
        (magnus-doctor--check
         'emacs 'error (format "Emacs %s is too old" emacs-version)
         "Magnus requires Emacs 28.1 or newer."))
      (magnus-doctor--library-check "vterm" "vterm")
      (magnus-doctor--library-check "transient" "transient")
      (magnus-doctor--library-check "magit-section" "magit-section")
      claude codex provider
      (magnus-doctor--executable-check
       'git "git" "Git"
       "Install Git to inspect immutable review scopes and project history.")
      (magnus-doctor--storage-check
       'state
       (if (and (boundp 'magnus-state-file)
                (stringp magnus-state-file))
           magnus-state-file
         (expand-file-name "~/.magnus/state.el"))
       "Instance state" nil)
      (magnus-doctor--storage-check
       'reviews
       (if (and (boundp 'magnus-review-directory-root)
                (stringp magnus-review-directory-root))
           magnus-review-directory-root
         (expand-file-name "~/.magnus/reviews"))
       "Review storage" t))
     (magnus-doctor--instance-checks)
     (magnus-doctor--coordination-checks))))

(defun magnus-doctor--severity-face (severity)
  "Return a display face for diagnostic SEVERITY."
  (pcase severity
    ('ok 'success)
    ('warning 'warning)
    (_ 'error)))

(defun magnus-doctor--severity-label (severity)
  "Return a compact label for diagnostic SEVERITY."
  (pcase severity
    ('ok "OK")
    ('warning "WARN")
    (_ "ERROR")))

(defvar magnus-doctor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'magnus-doctor-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `magnus-doctor-mode'.")

(define-derived-mode magnus-doctor-mode special-mode "Magnus-Doctor"
  "Major mode for read-only Magnus diagnostics.")

(defun magnus-doctor-refresh ()
  "Refresh the current Magnus diagnostics buffer."
  (interactive)
  (let ((checks (magnus-doctor-run))
        (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Magnus Doctor\n" 'face 'header-line))
    (insert "Read-only installation and runtime checks.  Press g to rerun.\n\n")
    (dolist (check checks)
      (let ((severity (magnus-doctor-check-severity check)))
        (insert
         (propertize
          (format "%-5s" (magnus-doctor--severity-label severity))
          'face (magnus-doctor--severity-face severity))
         "  " (magnus-doctor-check-summary check) "\n")
        (when-let ((detail (magnus-doctor-check-detail check)))
          (insert "       " detail "\n"))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

;;;###autoload
(defun magnus-doctor ()
  "Open a read-only report of Magnus installation and runtime health."
  (interactive)
  (let ((buffer (get-buffer-create magnus-doctor-buffer-name)))
    (with-current-buffer buffer
      (magnus-doctor-mode)
      (magnus-doctor-refresh))
    (pop-to-buffer buffer)))

(provide 'magnus-doctor)
;;; magnus-doctor.el ends here
