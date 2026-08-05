;;; magnus-terminal.el --- Shared vterm substrate for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared terminal creation and key handling for interactive Magnus providers.
;; Loading this library does not eagerly load vterm, so provider modules can
;; expose non-terminal capabilities in batch environments without requiring the
;; optional package.  Creating a terminal loads vterm before allocating a
;; buffer.

;;; Code:

(require 'magnus-environment)

(declare-function vterm-mode "vterm" ())
(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl))

(defun magnus-terminal-coordination-environment (writer-id writer-name)
  "Return terminal environment bindings for coordination identity.
WRITER-ID is the durable instance identity and WRITER-NAME is its display
name.  Keeping this provider-neutral prevents Claude and Codex terminals from
acquiring subtly different coordination identities."
  (magnus-environment-coordination-bindings writer-id writer-name))

(defun magnus-terminal--process-environment (bindings)
  "Return `process-environment' with string BINDINGS applied.
Each binding must have the form NAME=VALUE.  Later bindings replace inherited
values with the same NAME without mutating the caller's environment."
  (magnus-environment-overlay process-environment bindings))

(defun magnus-terminal--discard-buffer (buffer)
  "Discard partially initialized terminal BUFFER and its process."
  (when (buffer-live-p buffer)
    (let ((process (get-buffer-process buffer)))
      (when process
        (ignore-errors (set-process-query-on-exit-flag process nil))
        (when (process-live-p process)
          (ignore-errors (delete-process process)))))
    (ignore-errors (kill-buffer buffer))))

(defun magnus-terminal-create-buffer (buffer-name &optional environment)
  "Create and initialize a vterm buffer named BUFFER-NAME.
ENVIRONMENT is a list of NAME=VALUE bindings applied only while vterm starts.
Discard the buffer and any partially started process when initialization
fails."
  ;; Keep the optional dependency lazy: Codex's headless adapter is useful in
  ;; batch environments where vterm is not installed.
  (require 'vterm)
  (let ((buffer (generate-new-buffer buffer-name))
        initialized)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((process-environment
                   (magnus-terminal--process-environment environment)))
              (vterm-mode))
            (magnus-terminal-setup-keys))
          (setq initialized t)
          buffer)
      (unless initialized
        (magnus-terminal--discard-buffer buffer)))))

(defun magnus-terminal-send-escape ()
  "Send ESC to the terminal, for use in place of `keyboard-quit'."
  (interactive)
  (vterm-send-key "<escape>"))

(defun magnus-terminal-setup-keys ()
  "Set up Magnus key bindings in the current terminal buffer.
Map `keyboard-quit' to send ESC because Emacs intercepts the real key."
  (local-set-key (kbd "C-g") #'magnus-terminal-send-escape))

(provide 'magnus-terminal)
;;; magnus-terminal.el ends here
