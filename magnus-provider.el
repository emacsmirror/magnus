;;; magnus-provider.el --- Agent provider dispatch for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small, additive dispatch layer for non-Claude providers.  Claude Code is
;; intentionally not registered here: its established vterm implementation in
;; `magnus-process.el' remains the default and fallback path.

;;; Code:

(require 'magnus-instances)

(defvar magnus-provider--registry (make-hash-table :test #'eq)
  "Provider symbol to operation alist registry.")

(defun magnus-provider-register (provider operations)
  "Register PROVIDER with operation alist OPERATIONS.
Each entry has the form (OPERATION . FUNCTION)."
  (puthash provider operations magnus-provider--registry))

(defun magnus-provider--load (provider)
  "Load PROVIDER's optional implementation when available."
  (unless (or (eq provider 'claude)
              (gethash provider magnus-provider--registry))
    (require (intern (format "magnus-provider-%s" provider)) nil t)))

(defun magnus-provider-external-p (instance)
  "Return non-nil when INSTANCE names a non-Claude provider.
Unknown providers remain external so they fail clearly instead of silently
falling back to the legacy Claude process path."
  (let ((provider (or (magnus-instance-provider instance) 'claude)))
    (magnus-provider--load provider)
    (not (eq provider 'claude))))

(defun magnus-provider-call (instance operation &rest arguments)
  "Call INSTANCE provider's OPERATION with ARGUMENTS.
INSTANCE is prepended to ARGUMENTS.  Signal `user-error' when the provider
does not implement OPERATION."
  (let* ((provider (or (magnus-instance-provider instance) 'claude))
         (_loaded (magnus-provider--load provider))
         (operations (gethash provider magnus-provider--registry))
         (function (alist-get operation operations)))
    (unless function
      (user-error "Provider `%s' does not support `%s'" provider operation))
    (apply function instance arguments)))

(provide 'magnus-provider)
;;; magnus-provider.el ends here
