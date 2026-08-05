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

(defun magnus-environment-coordination-bindings (writer-id writer-name)
  "Return coordination bindings for WRITER-ID and WRITER-NAME.
WRITER-ID is a durable Magnus instance UUID.  WRITER-NAME is its display name."
  (dolist (value (list writer-id writer-name))
    (unless (and (stringp value)
                 (not (string-empty-p value))
                 (not (string-match-p "[\0\n\r]" value)))
      (error "Invalid Magnus coordination identity: %S" value)))
  (list (format "MAGNUS_COORD_WRITER_ID=%s" writer-id)
        (format "MAGNUS_COORD_WRITER_NAME=%s" writer-name)))

(provide 'magnus-environment)
;;; magnus-environment.el ends here
