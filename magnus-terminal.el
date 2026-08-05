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

(require 'cl-lib)
(require 'magnus-environment)
(require 'magnus-instances)

(declare-function vterm-mode "vterm" ())
(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl))
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-string "vterm" (string &optional paste-p))

(defcustom magnus-terminal-delivery-retry-delay 1.0
  "Seconds before retrying delivery to a user-owned or failed terminal."
  :type 'number
  :group 'magnus)

(defvar magnus-terminal--delivery-processes (make-hash-table :test #'eq)
  "Live processes with queued or settling terminal deliveries.")

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

(defun magnus-terminal--delivery-owner-p (entry process)
  "Return non-nil when ENTRY still belongs to exact PROCESS runtime."
  (let ((instance (plist-get entry :instance))
        (buffer (plist-get entry :buffer)))
    (and (magnus-instance-p instance)
         (eq process (plist-get entry :process))
         (buffer-live-p buffer)
         (eq buffer (magnus-instance-buffer instance))
         (eq process (get-buffer-process buffer))
         (process-live-p process))))

(defun magnus-terminal--cancel-property-timer (process property)
  "Cancel PROCESS timer in PROPERTY and clear the property."
  (when-let ((timer (process-get process property)))
    (when (timerp timer)
      (cancel-timer timer))
    (process-put process property nil)))

(defun magnus-terminal--schedule-delivery (process)
  "Schedule another safe attempt to drain PROCESS's delivery queue."
  (unless (process-get process 'magnus-terminal-delivery-retry-timer)
    (puthash process t magnus-terminal--delivery-processes)
    (process-put
     process 'magnus-terminal-delivery-retry-timer
     (run-with-timer
      magnus-terminal-delivery-retry-delay nil
      (lambda ()
        (when (processp process)
          (process-put process 'magnus-terminal-delivery-retry-timer nil))
        (magnus-terminal-drain process))))))

(defun magnus-terminal-delivery-idle-p (process)
  "Return non-nil when PROCESS has no queued or settling delivery."
  (and (processp process)
       (null (process-get process 'magnus-terminal-delivery-queue))
       (not (process-get process 'magnus-terminal-delivery-busy))))

(defun magnus-terminal--finish-delivery (process entry)
  "Release PROCESS after settling ENTRY, then continue its FIFO."
  (process-put process 'magnus-terminal-delivery-busy nil)
  (process-put process 'magnus-terminal-delivery-busy-timer nil)
  (magnus-terminal-drain process)
  (when (and (magnus-terminal-delivery-idle-p process)
             (plist-get entry :idle))
    (funcall (plist-get entry :idle) process)))

(defun magnus-terminal-drain (process)
  "Safely submit PROCESS's next queued terminal message.
Return `submitted', `queued', or nil.  A queue entry remains pending until
both bracketed paste and Return succeed."
  (let* ((queue (and (processp process)
                     (process-get process 'magnus-terminal-delivery-queue)))
         (entry (car queue))
         (buffer (plist-get entry :buffer)))
    (cond
     ((null queue)
      (unless (and (processp process)
                   (process-get process 'magnus-terminal-delivery-busy))
        (remhash process magnus-terminal--delivery-processes))
      nil)
     ((not (magnus-terminal--delivery-owner-p entry process))
      ;; Every entry is owned by this exact process.  A replaced or dead
      ;; runtime can acknowledge none of them; durable callers will replay.
      (magnus-terminal-release-process process)
      nil)
     ((or (process-get process 'magnus-terminal-delivery-busy)
          (and (plist-get entry :ready-p)
               (not (funcall (plist-get entry :ready-p) process))))
      'queued)
     ((eq buffer (window-buffer (selected-window)))
      ;; Never append to a composer while the user owns this TUI.
      (magnus-terminal--schedule-delivery process)
      'queued)
     (t
      (let ((accepted (plist-get entry :accepted)))
        (process-put process 'magnus-terminal-delivery-busy t)
        (condition-case err
            (progn
              ;; Both operations run in one Emacs event, so two automated
              ;; deliveries cannot interleave.  Pop before ACCEPTED to make a
              ;; callback unable to submit this exact entry twice.
              (with-current-buffer buffer
                (vterm-send-string (plist-get entry :text) t)
                (vterm-send-return))
              (process-put process 'magnus-terminal-delivery-queue (cdr queue))
              (when accepted
                (condition-case receipt-err
                    (funcall accepted)
                  (error
                   (message "Magnus: terminal delivery receipt failed: %s"
                            (error-message-string receipt-err)))))
              (let ((delay (or (plist-get entry :settle-delay) 0)))
                (if (> delay 0)
                    (process-put
                     process 'magnus-terminal-delivery-busy-timer
                     (run-with-timer delay nil
                                     #'magnus-terminal--finish-delivery
                                     process entry))
                  (magnus-terminal--finish-delivery process entry)))
              'submitted)
          (error
           (process-put process 'magnus-terminal-delivery-busy nil)
           (message "Magnus: terminal delivery deferred after error: %s"
                    (error-message-string err))
           (magnus-terminal--schedule-delivery process)
           'queued)))))))

(cl-defun magnus-terminal-submit
    (instance text &optional accepted
              &key ready-p settle-delay idle scope (deduplicate accepted))
  "Queue TEXT for exact INSTANCE terminal delivery.
Call ACCEPTED only after bracketed paste and Return succeed.  READY-P is a
process predicate.  SETTLE-DELAY serializes successive entries; IDLE is called
with the process after settling.  SCOPE supports selective cancellation.
DEDUPLICATE coalesces matching TEXT and defaults to non-nil for durable entries
with ACCEPTED.  Return `submitted' or `queued'."
  (let* ((buffer (magnus-instance-buffer instance))
         (process (and (buffer-live-p buffer) (get-buffer-process buffer))))
    (unless (and process (process-live-p process))
      (user-error "Magnus instance `%s' is not running"
                  (magnus-instance-name instance)))
    (let* ((queue (process-get process 'magnus-terminal-delivery-queue))
           (duplicate
            (and deduplicate
                 (cl-find-if
                  (lambda (entry)
                    (and (eq scope (plist-get entry :scope))
                         (string= text (plist-get entry :text))))
                  queue)))
           (entry
            (or duplicate
                (list :instance instance :buffer buffer :process process
                      :text text :accepted accepted
                      :ready-p ready-p :settle-delay settle-delay
                      :idle idle :scope scope))))
      (unless duplicate
        (process-put process 'magnus-terminal-delivery-queue
                     (append queue (list entry)))
        (puthash process t magnus-terminal--delivery-processes))
      (magnus-terminal-drain process)
      (if (memq entry
                (process-get process 'magnus-terminal-delivery-queue))
          'queued
        'submitted))))

(defun magnus-terminal-release-process (process)
  "Cancel and forget all delivery state owned by exact PROCESS."
  (when (processp process)
    (magnus-terminal--cancel-property-timer
     process 'magnus-terminal-delivery-retry-timer)
    (magnus-terminal--cancel-property-timer
     process 'magnus-terminal-delivery-busy-timer)
    (process-put process 'magnus-terminal-delivery-busy nil)
    (process-put process 'magnus-terminal-delivery-queue nil))
  (remhash process magnus-terminal--delivery-processes))

(defun magnus-terminal-cancel-scope (scope)
  "Cancel queued terminal deliveries belonging to SCOPE only."
  (let (processes)
    (maphash (lambda (process _value) (push process processes))
             magnus-terminal--delivery-processes)
    (dolist (process processes)
     (when (processp process)
       (process-put
        process 'magnus-terminal-delivery-queue
        (cl-delete-if
         (lambda (entry) (eq (plist-get entry :scope) scope))
         (process-get process 'magnus-terminal-delivery-queue)))
       (when (magnus-terminal-delivery-idle-p process)
         (magnus-terminal--cancel-property-timer
          process 'magnus-terminal-delivery-retry-timer)
         (remhash process magnus-terminal--delivery-processes))))))

(provide 'magnus-terminal)
;;; magnus-terminal.el ends here
