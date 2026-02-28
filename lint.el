;;; lint.el --- Lint checks for Magnus -*- lexical-binding: t -*-
;; Usage: emacs --batch -l lint.el -- file1.el file2.el ...
;; Pass --compile to also byte-compile files.

(require 'cl-lib)

(defvar lint-ok t "Set to nil on any failure.")
(defvar lint-do-compile nil "Non-nil to byte-compile files.")

(defun lint-files ()
  "Return the list of .el files from command-line args."
  (let ((files nil)
        (args command-line-args-left))
    (while args
      (let ((arg (pop args)))
        (cond
         ((string= arg "--compile") (setq lint-do-compile t))
         ((string-suffix-p ".el" arg) (push arg files)))))
    (setq command-line-args-left nil)
    (nreverse files)))

(defun lint-check-parens (file)
  "Check paren balance in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (condition-case err
        (check-parens)
      (error
       (message "FAIL parens: %s: %s" file (error-message-string err))
       (setq lint-ok nil)))))

(defun lint-check-nesting (file)
  "Check for top-level forms nested inside other forms in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (while (re-search-forward "^(def\\(un\\|var\\|custom\\|macro\\) " nil t)
      (goto-char (match-beginning 0))
      (let ((depth (car (syntax-ppss))))
        (unless (= depth 0)
          (message "FAIL nesting: %s:%d: depth %d"
                   file (line-number-at-pos) depth)
          (setq lint-ok nil)))
      (forward-line 1))))

(defun lint-check-patterns (file)
  "Check for banned patterns in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (re-search-forward "(error nil)" nil t)
      (message "FAIL pattern: %s:%d: (error nil) — log errors instead"
               file (line-number-at-pos))
      (setq lint-ok nil))))

(defun lint-byte-compile (file)
  "Byte-compile FILE, treating errors as failures."
  (unless (byte-compile-file file)
    (message "FAIL compile: %s" file)
    (setq lint-ok nil)))

(defun lint-run ()
  "Run all lint checks on files from command line."
  (let ((files (lint-files)))
    (setq files (cl-remove-if
                  (lambda (f) (string= (file-name-nondirectory f) "lint.el"))
                  files))
    (unless files
      (message "No files to lint (lint.el excluded).")
      (kill-emacs 0))
    (message "Linting %d file(s)..." (length files))
    (dolist (file files)
      (lint-check-parens file)
      (lint-check-nesting file)
      (lint-check-patterns file)
      (when lint-do-compile
        (lint-byte-compile file)))
    (if lint-ok
        (message "All checks passed.")
      (message "Lint FAILED.")
      (kill-emacs 1))))

(lint-run)
;;; lint.el ends here
