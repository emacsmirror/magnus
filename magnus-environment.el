;;; magnus-environment.el --- Isolated process environments for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral helpers for adding narrowly scoped environment bindings to
;; interactive terminals and headless subprocesses.  Every overlay returns a
;; new list; neither the caller's `process-environment' nor a provider-owned
;; filtered environment is modified.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun magnus-environment--binding-name (binding)
  "Validate BINDING and return its environment variable name."
  (unless (and (stringp binding)
               (not (string-match-p "\0" binding))
               (string-match
                "\\`\\([A-Za-z_][A-Za-z0-9_]*\\)=" binding))
    (error "Invalid Magnus environment binding: %S" binding))
  (match-string 1 binding))

(defun magnus-environment-validate-bindings (bindings)
  "Validate NAME=VALUE strings in BINDINGS and return BINDINGS."
  (unless (listp bindings)
    (signal 'wrong-type-argument (list 'listp bindings)))
  (dolist (binding bindings)
    (magnus-environment--binding-name binding))
  bindings)

(defun magnus-environment-overlay (environment bindings)
  "Return a copy of ENVIRONMENT with string BINDINGS applied.
Each binding must have the form NAME=VALUE.  Later bindings replace inherited
or earlier values with the same NAME.  Neither input list is modified."
  (unless (listp environment)
    (signal 'wrong-type-argument (list 'listp environment)))
  (magnus-environment-validate-bindings bindings)
  (let ((result (copy-sequence environment)))
    (dolist (binding bindings)
      (let ((prefix
             (concat (magnus-environment--binding-name binding) "=")))
        (setq result
              (cons binding
                    (cl-remove-if
                     (lambda (entry)
                       (and (stringp entry)
                            (string-prefix-p prefix entry)))
                     result)))))
    result))

(defun magnus-environment-without (environment names prefixes)
  "Return ENVIRONMENT without variables matching NAMES or PREFIXES.
NAMES contains exact variable names.  PREFIXES contains variable-name
prefixes.  The original environment is never modified."
  (unless (and (listp environment)
               (cl-every #'stringp names)
               (cl-every #'stringp prefixes))
    (signal 'wrong-type-argument
            (list 'list-of-strings-p (list environment names prefixes))))
  (cl-remove-if
   (lambda (entry)
     (when (and (stringp entry)
                (string-match "\\`\\([^=]+\\)=" entry))
       (let ((name (match-string 1 entry)))
         (or (member name names)
             (cl-some (lambda (prefix)
                        (string-prefix-p prefix name))
                      prefixes)))))
   environment))

(provide 'magnus-environment)
;;; magnus-environment.el ends here
