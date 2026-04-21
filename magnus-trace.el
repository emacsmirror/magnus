;;; magnus-trace.el --- Thinking trace viewer for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides a JSONL session viewer for Claude Code thinking
;; traces.  It reads the session JSONL files that Claude Code writes and
;; renders user messages, thinking blocks, and assistant responses in a
;; scrollable Emacs buffer with auto-refresh.

;;; Code:

(require 'magnus-instances)

(declare-function magnus-process--list-sessions "magnus-process")
(declare-function magnus-process--most-recent-session "magnus-process")
(declare-function magnus-process--session-jsonl-path "magnus-process")

;;; Faces

(defface magnus-trace-user
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for user messages in trace buffer."
  :group 'magnus)

(defface magnus-trace-thinking
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for thinking blocks in trace buffer."
  :group 'magnus)

(defface magnus-trace-assistant
  '((t :inherit default))
  "Face for assistant responses in trace buffer."
  :group 'magnus)

(defface magnus-trace-separator
  '((t :inherit font-lock-comment-face))
  "Face for separators in trace buffer."
  :group 'magnus)

;;; Variables

(defcustom magnus-trace-max-initial-entries 200
  "Maximum entries to render on initial trace open.
When a JSONL file has more entries than this, only the last N are
rendered.  Set to nil to render everything (may freeze on large files)."
  :type '(choice (integer :tag "Max entries")
                 (const :tag "No limit" nil))
  :group 'magnus)

(defvar-local magnus-trace--instance nil
  "The instance this trace buffer is following.")

(defvar-local magnus-trace--last-line-count 0
  "Number of JSONL lines already processed.")

(defvar-local magnus-trace--rendered-count 0
  "Number of entries currently rendered in the buffer.")

(defvar magnus-trace--timer nil
  "Timer for auto-refreshing trace buffers.")

;;; Major mode

(define-derived-mode magnus-trace-mode special-mode "Trace"
  "Major mode for viewing Claude Code thinking trace.
\\{magnus-trace-mode-map}"
  :group 'magnus
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(let ((map magnus-trace-mode-map))
  (define-key map (kbd "g") #'magnus-trace-refresh)
  (define-key map (kbd "G") #'magnus-trace-tail)
  (define-key map (kbd "TAB") #'magnus-trace-toggle-thinking)
  (define-key map (kbd "t") #'magnus-trace-toggle-all-thinking)
  (define-key map (kbd "n") #'magnus-trace-next-response)
  (define-key map (kbd "p") #'magnus-trace-prev-response)
  (define-key map (kbd "q") #'quit-window))

;;; Core functions

(defun magnus-trace-open (instance)
  "Open the trace buffer for INSTANCE showing thinking and messages."
  (let* ((name (magnus-instance-name instance))
         (trace-name (format "*trace:%s*" name))
         (trace-buf (get-buffer-create trace-name)))
    (with-current-buffer trace-buf
      (unless (derived-mode-p 'magnus-trace-mode)
        (magnus-trace-mode))
      (setq magnus-trace--instance instance)
      (setq magnus-trace--last-line-count 0)
      (setq magnus-trace--rendered-count 0)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (magnus-trace-refresh))
    (magnus-trace--ensure-timer)
    ;; Close any existing trace window before displaying the new one,
    ;; so the side window is freshly created with the correct buffer.
    (dolist (win (window-list))
      (when (and (window-live-p win)
                 (string-prefix-p "*trace:" (buffer-name (window-buffer win)))
                 (not (eq (window-buffer win) trace-buf)))
        (delete-window win)))
    (display-buffer trace-buf '(display-buffer-in-side-window
                                (side . bottom)
                                (slot . 1)
                                (window-height . 0.35)))
    trace-buf))

(defun magnus-trace-refresh ()
  "Refresh the trace buffer with new JSONL content."
  (interactive)
  (when magnus-trace--instance
    (let* ((instance magnus-trace--instance)
           (session-id (magnus-instance-session-id instance))
           (directory (magnus-instance-directory instance)))
      ;; Try to detect session-id if missing
      (unless session-id
        (let ((sessions (magnus-process--list-sessions directory)))
          (when sessions
            (setq session-id (if (= 1 (length sessions))
                                 (car sessions)
                               (magnus-process--most-recent-session
                                directory sessions)))
            (when session-id
              (magnus-instances-update instance :session-id session-id)
              ;; Clear the waiting message now that we have a session
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq magnus-trace--last-line-count 0))))))
      (let ((jsonl-file (when session-id
                          (magnus-process--session-jsonl-path directory session-id))))
        (if (and jsonl-file (file-exists-p jsonl-file))
            (magnus-trace--append-new-entries jsonl-file)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (> (buffer-size) 0)
              (insert (propertize "Waiting for session to start...\n"
                                 'face 'magnus-trace-separator)))))))))

(defun magnus-trace-tail ()
  "Refresh and jump to the end of the trace buffer."
  (interactive)
  (magnus-trace-refresh)
  (goto-char (point-max))
  (recenter -3))

;;; Thinking block collapse/expand

(defun magnus-trace-toggle-thinking ()
  "Toggle visibility of the thinking block at point."
  (interactive)
  (let ((found nil))
    (dolist (ov (overlays-at (point)))
      (when (and (not found) (overlay-get ov 'magnus-thinking))
        (overlay-put ov 'invisible
                     (not (overlay-get ov 'invisible)))
        (setq found t)))
    ;; If not on an overlay, try finding one nearby
    (unless found
      (let ((ov (magnus-trace--find-nearest-thinking)))
        (when ov
          (overlay-put ov 'invisible
                       (not (overlay-get ov 'invisible))))))))

(defun magnus-trace-toggle-all-thinking ()
  "Toggle visibility of all thinking blocks in the buffer."
  (interactive)
  (let ((ovs (overlays-in (point-min) (point-max)))
        (target-state nil)
        (first t))
    (dolist (ov ovs)
      (when (overlay-get ov 'magnus-thinking)
        ;; Determine target state from first overlay
        (when first
          (setq target-state (not (overlay-get ov 'invisible)))
          (setq first nil))
        (overlay-put ov 'invisible target-state)))))

(defun magnus-trace-next-response ()
  "Move point to the next assistant response block."
  (interactive)
  (let ((pos (next-single-property-change (point) 'face)))
    (while (and pos (< pos (point-max))
                (not (eq (get-text-property pos 'face) 'magnus-trace-assistant)))
      (setq pos (next-single-property-change pos 'face)))
    (when (and pos (< pos (point-max)))
      (goto-char pos))))

(defun magnus-trace-prev-response ()
  "Move point to the previous assistant response block."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'face)))
    (while (and pos (> pos (point-min))
                (not (eq (get-text-property pos 'face) 'magnus-trace-assistant)))
      (setq pos (previous-single-property-change pos 'face)))
    (when (and pos (> pos (point-min)))
      (goto-char pos))))

(defun magnus-trace--find-nearest-thinking ()
  "Find the nearest thinking overlay to point."
  (let ((best nil)
        (best-dist most-positive-fixnum))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'magnus-thinking)
        (let ((dist (min (abs (- (point) (overlay-start ov)))
                         (abs (- (point) (overlay-end ov))))))
          (when (< dist best-dist)
            (setq best ov best-dist dist)))))
    best))

;;; Content parsing

(defun magnus-trace-parse-content (text)
  "Parse TEXT for thinking/response markers.
Finds [thinking]...[end-thinking] and [response]...[end-response]
pairs.  Returns a list of plists (:type TYPE :text TEXT) where TYPE
is `thinking' or `response'.  Unmarked text is treated as response."
  (let ((segments nil)
        (pos 0)
        (len (length text)))
    (while (< pos len)
      (let ((think-start (string-match "\\[thinking\\]\n?" text pos))
            (resp-start (string-match "\\[response\\]\n?" text pos)))
        (cond
         ;; Thinking block comes first (or is only one)
         ((and think-start (or (null resp-start) (<= think-start resp-start)))
          ;; Capture any text before the marker as response
          (when (> think-start pos)
            (let ((pre (string-trim (substring text pos think-start))))
              (when (not (string-empty-p pre))
                (push (list :type 'response :text pre) segments))))
          (let* ((content-start (match-end 0))
                 (think-end (string-match "\\[end-thinking\\]\n?" text content-start)))
            (if think-end
                (progn
                  (let ((content (string-trim (substring text content-start think-end))))
                    (when (not (string-empty-p content))
                      (push (list :type 'thinking :text content) segments)))
                  (setq pos (match-end 0)))
              ;; No end marker — rest is thinking
              (let ((content (string-trim (substring text content-start))))
                (when (not (string-empty-p content))
                  (push (list :type 'thinking :text content) segments)))
              (setq pos len))))
         ;; Response block comes first (or is only one)
         ((and resp-start (or (null think-start) (< resp-start think-start)))
          ;; Capture any text before the marker as response
          (when (> resp-start pos)
            (let ((pre (string-trim (substring text pos resp-start))))
              (when (not (string-empty-p pre))
                (push (list :type 'response :text pre) segments))))
          (let* ((content-start (match-end 0))
                 (resp-end (string-match "\\[end-response\\]\n?" text content-start)))
            (if resp-end
                (progn
                  (let ((content (string-trim (substring text content-start resp-end))))
                    (when (not (string-empty-p content))
                      (push (list :type 'response :text content) segments)))
                  (setq pos (match-end 0)))
              ;; No end marker — rest is response
              (let ((content (string-trim (substring text content-start))))
                (when (not (string-empty-p content))
                  (push (list :type 'response :text content) segments)))
              (setq pos len))))
         ;; No markers found — rest is response
         (t
          (let ((rest (string-trim (substring text pos))))
            (when (not (string-empty-p rest))
              (push (list :type 'response :text rest) segments)))
          (setq pos len)))))
    (nreverse segments)))

(defun magnus-trace--text-has-markers-p (text)
  "Return non-nil if TEXT contains thinking/response markers."
  (string-match-p "\\[thinking\\]\\|\\[response\\]" text))

;;; Internal helpers

(defun magnus-trace--append-new-entries (jsonl-file)
  "Append new entries from JSONL-FILE to the current trace buffer."
  (let* ((all-lines (magnus-trace--read-lines jsonl-file))
         (total (length all-lines))
         (new-lines (nthcdr magnus-trace--last-line-count all-lines))
         (new-count (length new-lines))
         (at-end (eobp))
         ;; On initial load, cap to last N entries to avoid freezing
         (skip (if (and (zerop magnus-trace--last-line-count)
                        magnus-trace-max-initial-entries
                        (> new-count magnus-trace-max-initial-entries))
                   (- new-count magnus-trace-max-initial-entries)
                 0))
         (lines-to-render (nthcdr skip new-lines)))
    (when lines-to-render
      (let ((inhibit-read-only t)
            (parsed-count 0))
        (save-excursion
          (goto-char (point-max))
          ;; Show omission notice on initial load
          (when (> skip 0)
            (insert (propertize
                     (format "── %d earlier entries omitted (showing last %d) ──\n\n"
                             (+ magnus-trace--last-line-count skip)
                             magnus-trace-max-initial-entries)
                     'face 'magnus-trace-separator)))
          (catch 'partial-line
            (dolist (line lines-to-render)
              (condition-case nil
                  (let ((entry (json-parse-string line :object-type 'alist)))
                    (magnus-trace--render-entry entry)
                    (setq parsed-count (1+ parsed-count)))
                ;; Stop at first unparseable line (likely half-written).
                ;; We'll retry it on the next refresh.
                (error (throw 'partial-line nil))))))
        (setq magnus-trace--last-line-count
              (+ magnus-trace--last-line-count skip parsed-count))
        (setq magnus-trace--rendered-count
              (+ magnus-trace--rendered-count parsed-count)))
      ;; Trim buffer if it has grown too large (2x cap)
      (magnus-trace--maybe-trim jsonl-file)
      ;; Follow tail if user was at end
      (when at-end
        (goto-char (point-max))
        (let ((win (get-buffer-window (current-buffer))))
          (when win
            (set-window-point win (point-max))))))))

(defun magnus-trace--maybe-trim (jsonl-file)
  "Trim the current trace buffer if it exceeds 2x the entry cap.
Re-renders the last `magnus-trace-max-initial-entries' from JSONL-FILE."
  (when (and magnus-trace-max-initial-entries
             (> magnus-trace--rendered-count
                (* 2 magnus-trace-max-initial-entries)))
    (let* ((all-lines (magnus-trace--read-lines jsonl-file))
           (total (length all-lines))
           (keep magnus-trace-max-initial-entries)
           (skip (max 0 (- total keep)))
           (lines-to-render (nthcdr skip all-lines))
           (inhibit-read-only t))
      (erase-buffer)
      (remove-overlays)
      (when (> skip 0)
        (insert (propertize
                 (format "── %d earlier entries omitted (showing last %d) ──\n\n"
                         skip keep)
                 'face 'magnus-trace-separator)))
      (let ((parsed-count 0))
        (dolist (line lines-to-render)
          (condition-case nil
              (let ((entry (json-parse-string line :object-type 'alist)))
                (magnus-trace--render-entry entry)
                (setq parsed-count (1+ parsed-count)))
            (error (ignore))))
        (setq magnus-trace--last-line-count (+ skip parsed-count)
              magnus-trace--rendered-count parsed-count)))))

(defun magnus-trace--read-lines (file)
  "Read all lines from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun magnus-trace--render-entry (entry)
  "Render a JSONL ENTRY into the trace buffer."
  (let ((type (alist-get 'type entry))
        (message (alist-get 'message entry))
        (timestamp (alist-get 'timestamp entry)))
    (cond
     ((string= type "user")
      (let ((content (alist-get 'content message)))
        (when (and content (stringp content) (not (string-empty-p content)))
          (insert (propertize (format "── User [%s] ──\n"
                                     (magnus-trace--format-time timestamp))
                             'face 'magnus-trace-separator))
          (insert (propertize (concat content "\n\n")
                             'face 'magnus-trace-user)))))
     ((string= type "assistant")
      (let ((content (alist-get 'content message)))
        (when (vectorp content)
          (let ((has-output nil))
            (seq-doseq (block content)
              (let ((block-type (alist-get 'type block)))
                (cond
                 ((string= block-type "thinking")
                  (let ((thinking (alist-get 'thinking block)))
                    (when (and thinking (not (string-empty-p thinking)))
                      (unless has-output
                        (insert (propertize (format "── Thinking [%s] ──\n"
                                                   (magnus-trace--format-time timestamp))
                                           'face 'magnus-trace-separator))
                        (setq has-output t))
                      (let ((start (point)))
                        (insert (propertize (concat thinking "\n\n")
                                           'face 'magnus-trace-thinking))
                        (let ((ov (make-overlay start (point))))
                          (overlay-put ov 'magnus-thinking t)
                          (overlay-put ov 'evaporate t))))))
                 ((string= block-type "text")
                  (let ((text (alist-get 'text block)))
                    (when (and text (not (string-empty-p text)))
                      (if (magnus-trace--text-has-markers-p text)
                          ;; Parse [thinking]/[response] markers
                          (let ((segments (magnus-trace-parse-content text)))
                            (dolist (seg segments)
                              (let ((seg-type (plist-get seg :type))
                                    (seg-text (plist-get seg :text)))
                                (cond
                                 ((eq seg-type 'thinking)
                                  (unless has-output
                                    (insert (propertize
                                            (format "── Thinking [%s] ──\n"
                                                    (magnus-trace--format-time timestamp))
                                            'face 'magnus-trace-separator))
                                    (setq has-output t))
                                  (let ((start (point)))
                                    (insert (propertize (concat seg-text "\n\n")
                                                       'face 'magnus-trace-thinking))
                                    (let ((ov (make-overlay start (point))))
                                      (overlay-put ov 'magnus-thinking t)
                                      (overlay-put ov 'evaporate t))))
                                 ((eq seg-type 'response)
                                  (unless has-output
                                    (insert (propertize
                                            (format "── Assistant [%s] ──\n"
                                                    (magnus-trace--format-time timestamp))
                                            'face 'magnus-trace-separator))
                                    (setq has-output t))
                                  (insert (propertize (concat seg-text "\n\n")
                                                     'face 'magnus-trace-assistant)))))))
                        ;; No markers — render as plain assistant text
                        (unless has-output
                          (insert (propertize (format "── Assistant [%s] ──\n"
                                                     (magnus-trace--format-time timestamp))
                                             'face 'magnus-trace-separator))
                          (setq has-output t))
                        (insert (propertize (concat text "\n\n")
                                           'face 'magnus-trace-assistant))))))))))))))))

(defun magnus-trace--format-time (timestamp)
  "Format ISO TIMESTAMP to HH:MM:SS."
  (if (and timestamp (stringp timestamp))
      (if (string-match "T\\([0-9]+:[0-9]+:[0-9]+\\)" timestamp)
          (match-string 1 timestamp)
        "")
    ""))

;;; Timer management

(defun magnus-trace--ensure-timer ()
  "Ensure the trace auto-refresh timer is running."
  (unless magnus-trace--timer
    (setq magnus-trace--timer
          (run-with-timer 2 5 #'magnus-trace--sync-all))))

(defun magnus-trace--sync-all ()
  "Auto-refresh all open trace buffers.
The timer is kept alive even when no trace buffers exist, to avoid
a race where a buffer is opened between the check and the cancel.
The timer is cheap (no-op when nothing is open) and is only stopped
when magnus shuts down."
  (dolist (instance (magnus-instances-list))
    (let ((trace-buf (get-buffer (format "*trace:%s*" (magnus-instance-name instance)))))
      (when (and trace-buf (buffer-live-p trace-buf))
        (with-current-buffer trace-buf
          (condition-case err
              (magnus-trace-refresh)
            (error
             (message "Magnus: trace refresh error for %s: %s"
                      (magnus-instance-name instance)
                      (error-message-string err)))))))))

(defun magnus-trace-stop-timer ()
  "Stop the trace auto-refresh timer."
  (when magnus-trace--timer
    (cancel-timer magnus-trace--timer)
    (setq magnus-trace--timer nil)))

(provide 'magnus-trace)
;;; magnus-trace.el ends here
