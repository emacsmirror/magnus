;;; package-lint-check.el --- Batch package metadata check -*- lexical-binding: t -*-

(require 'package)
(require 'package-lint)

;; Populate package descriptors without activating unrelated autoload files.
(package-load-all-descriptors)

(let ((file (expand-file-name "magnus.el" magnus-test-project-directory))
      issues)
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (setq issues (package-lint-buffer)))
  (dolist (issue issues)
    (pcase-let ((`(,line ,column ,kind ,message) issue))
      (message "magnus.el:%d:%d: %s: %s"
               line column kind message)))
  (when issues
    (kill-emacs 1)))

(provide 'package-lint-check)
;;; package-lint-check.el ends here
