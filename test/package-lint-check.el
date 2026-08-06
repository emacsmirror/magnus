;;; package-lint-check.el --- Batch package metadata check -*- lexical-binding: t -*-

(require 'package)
(require 'package-lint)

;; Package lint validates every declared dependency.  CI intentionally does not
;; compile vterm, so load cached archive descriptors as well as installed ones.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(package-read-all-archive-contents)
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
