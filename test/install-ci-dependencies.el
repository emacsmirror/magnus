;;; install-ci-dependencies.el --- Install Magnus CI packages -*- lexical-binding: t -*-

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(dolist (spec '((transient (0 4 0))
                (magit-section (3 3 0))
                (package-lint (0 24))))
  (let ((package (car spec))
        (minimum-version (cadr spec)))
    (unless (package-installed-p package minimum-version)
      (package-install package))))

(provide 'install-ci-dependencies)
;;; install-ci-dependencies.el ends here
