;;; test-helper.el --- Hermetic test load path for Magnus -*- lexical-binding: t -*-

;; `emacs -Q' intentionally skips package activation, but Magnus's public
;; review reader depends on magit-section.  Put installed package directories
;; ahead of Emacs's built-ins without evaluating every package's autoload file:
;; unrelated broken user packages must not make this repository's tests flaky,
;; and an upgraded Transient must win over an older built-in copy.

(require 'package)

(defconst magnus-test-project-directory
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Magnus checkout containing the tests currently running.")

(defun magnus-test--package-directories ()
  "Return installed package directories without activating their autoloads."
  (let (directories)
    (dolist (root (cons package-user-dir package-directory-list))
      (when (file-directory-p root)
        (dolist (entry (directory-files root t "\\`[^.]" t))
          (when (file-directory-p entry)
            (push entry directories)))))
    directories))

(setq load-path
      (append (magnus-test--package-directories) load-path))

;; An installed MELPA Magnus must never shadow the checkout under test.
(setq load-path
      (cons magnus-test-project-directory
            (delete magnus-test-project-directory load-path)))
(setq load-prefer-newer t)

(provide 'test-helper)
;;; test-helper.el ends here
