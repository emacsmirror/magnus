;;; magnus-trace.el --- Thinking trace viewer for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides a provider-aware JSONL session viewer.  Providers
;; locate and normalize their records; this shared UI renders user messages,
;; thinking blocks, and assistant responses with pagination and auto-refresh.

;;; Code:

(require 'cl-lib)
(require 'magnus-instances)
(require 'magnus-provider)

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

(defcustom magnus-trace-max-buffer-lines 4000
  "Maximum buffer lines before refusing to load earlier entries.
Beyond this limit, users should use grep or less on the raw JSONL file."
  :type 'integer
  :group 'magnus)

(defcustom magnus-trace-read-chunk-bytes (* 64 1024)
  "Maximum number of JSONL bytes read from disk at once.
Initial tailing, earlier-page reads, and trimming all honor this bound."
  :type '(integer :tag "Bytes")
  :group 'magnus)

(defcustom magnus-trace-max-record-bytes (* 1024 1024)
  "Maximum bytes retained for one unfinished JSONL record.
When a writer exceeds this limit before writing a newline, Magnus discards
that record through its terminating newline rather than growing the trace
buffer's retained fragment without bound."
  :type '(integer :tag "Bytes")
  :group 'magnus)

(defvar-local magnus-trace--instance nil
  "The instance this trace buffer is following.")

(defvar-local magnus-trace--last-line-count 0
  "Number of JSONL lines already processed.")

(defvar-local magnus-trace--rendered-count 0
  "Number of entries currently rendered in the buffer.")

(defvar-local magnus-trace--file-offset 0
  "Byte offset into the JSONL file up to which we have read.")

(defvar-local magnus-trace--skip-count 0
  "Number of JSONL entries currently skipped (not rendered) at the top.")

(defvar-local magnus-trace--jsonl-file nil
  "Path to the JSONL file for this trace buffer.")

(defvar-local magnus-trace--session-id nil
  "Provider session ID associated with `magnus-trace--jsonl-file'.")

(defvar-local magnus-trace--pending-text ""
  "Incomplete trailing JSONL text retained for the next refresh.")

(defvar-local magnus-trace--discarding-incomplete-record-p nil
  "Non-nil while skipping an oversized unfinished JSONL record.")

(defvar-local magnus-trace--page-start-offset 0
  "Byte offset of the earliest JSONL record rendered in this buffer.")

(defvar magnus-trace--timer nil
  "Timer for auto-refreshing trace buffers.")

(define-error 'magnus-trace-cursor-error "Magnus trace cursor error")
(define-error 'magnus-trace-cursor-stale "Magnus trace cursor is stale"
  'magnus-trace-cursor-error)

(cl-defstruct (magnus-trace-cursor
               (:constructor magnus-trace-cursor--create))
  "An in-memory reader pinned to one exact instance trace.
The cursor deliberately carries no restart or replay semantics."
  instance
  instance-id
  provider
  session-id
  file
  file-identity
  offset
  pending
  discarding)

;;; Major mode

(define-derived-mode magnus-trace-mode special-mode "Trace"
  "Major mode for viewing an agent's thinking trace.
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
  (define-key map (kbd "[") #'magnus-trace-load-earlier)
  (define-key map (kbd "q") #'quit-window))

;;; Core functions

(defun magnus-trace-resolve-file (instance)
  "Return the exact current trace file for INSTANCE, or nil.
Claude traces are resolved from the captured session ID and project path.
External providers resolve their own exact trace through `trace-file'.  This
function never guesses from other sessions in the project."
  (when (magnus-instance-session-id instance)
    (let ((file
           (if (magnus-provider-external-p instance)
               (magnus-provider-call instance 'trace-file)
             (magnus-process--session-jsonl-path
              (magnus-instance-directory instance)
              (magnus-instance-session-id instance)))))
      (and file (expand-file-name file)))))

(defun magnus-trace--reset-read-state (&optional clear-buffer)
  "Reset JSONL reader state.
When CLEAR-BUFFER is non-nil, also erase rendered content and overlays."
  (setq magnus-trace--last-line-count 0
        magnus-trace--rendered-count 0
        magnus-trace--file-offset 0
        magnus-trace--skip-count 0
        magnus-trace--jsonl-file nil
        magnus-trace--session-id nil
        magnus-trace--pending-text ""
        magnus-trace--discarding-incomplete-record-p nil
        magnus-trace--page-start-offset 0)
  (when clear-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (remove-overlays))))

(defun magnus-trace-open (instance)
  "Open the trace buffer for INSTANCE showing thinking and messages."
  (let* ((name (magnus-instance-name instance))
         (trace-name (format "*trace:%s*" name))
         (trace-buf (get-buffer-create trace-name)))
    (with-current-buffer trace-buf
      (unless (derived-mode-p 'magnus-trace-mode)
        (magnus-trace-mode))
      (setq magnus-trace--instance instance)
      (magnus-trace--reset-read-state t)
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
           (session-id (magnus-instance-session-id instance)))
      ;; Session capture is owned by the exact provider launch.  Guessing from
      ;; a project-wide newest JSONL file can attach two concurrent agents to
      ;; the same conversation, so an unresolved trace simply keeps waiting.
      (let ((jsonl-file (magnus-trace-resolve-file instance)))
        (if (and jsonl-file (file-exists-p jsonl-file))
            (progn
              (unless (and (equal jsonl-file magnus-trace--jsonl-file)
                           (equal session-id magnus-trace--session-id))
                (magnus-trace--reset-read-state t))
              (setq magnus-trace--session-id session-id)
              (magnus-trace--append-new-entries jsonl-file))
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

;;; Backward pagination

(defun magnus-trace-load-earlier ()
  "Load the previous batch of earlier entries into the trace buffer.
Each press loads up to `magnus-trace-max-initial-entries' more entries
at the top.  Stops when all entries are loaded or the buffer exceeds
`magnus-trace-max-buffer-lines' lines."
  (interactive)
  (unless magnus-trace--jsonl-file
    (user-error "No JSONL file associated with this trace buffer"))
  (cond
   ((zerop magnus-trace--skip-count)
    (message "All entries already loaded"))
   ((and magnus-trace-max-buffer-lines
         (>= (count-lines (point-min) (point-max))
              magnus-trace-max-buffer-lines))
    (message "Buffer at %d lines — use grep/less on %s"
             magnus-trace-max-buffer-lines
             (abbreviate-file-name magnus-trace--jsonl-file)))
   (t
    (let* ((batch-size (or magnus-trace-max-initial-entries 200))
           (snapshot
            (magnus-trace--read-previous-records
             magnus-trace--jsonl-file
             magnus-trace--page-start-offset
             (min magnus-trace--skip-count batch-size)))
           (batch (plist-get snapshot :lines))
           (loaded (plist-get snapshot :count))
           (new-skip (- magnus-trace--skip-count loaded))
           (inhibit-read-only t)
           (parsed-count 0))
      (save-excursion
        (goto-char (point-min))
        ;; Remove old header
        (when (get-text-property (point) 'magnus-trace-header)
          (delete-region (point)
                         (or (next-single-property-change
                              (point) 'magnus-trace-header)
                             (point))))
        ;; Insert new header if still skipping
        (when (> new-skip 0)
          (insert (propertize
                   (format "── %d earlier entries (press [ to load more) ──\n\n"
                           new-skip)
                   'face 'magnus-trace-separator
                   'magnus-trace-header t)))
        ;; Render batch entries at point (before existing content)
        (dolist (line batch)
          (when (and line (magnus-trace--render-json-line line))
            (setq parsed-count (1+ parsed-count)))))
      (setq magnus-trace--skip-count new-skip
            magnus-trace--page-start-offset
            (plist-get snapshot :start)
            magnus-trace--rendered-count
            (+ magnus-trace--rendered-count parsed-count))
      (goto-char (point-min))
      (message "Loaded %d entries (%s)"
               parsed-count
               (if (zerop new-skip)
                   "all entries now visible"
                 (format "%d earlier remaining" new-skip)))))))

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

(defun magnus-trace--marker-match (regexp text start)
  "Return REGEXP's (START . END) match in TEXT at or after START."
  (save-match-data
    (when (string-match regexp text start)
      (cons (match-beginning 0) (match-end 0)))))

(defun magnus-trace-parse-content (text)
  "Parse TEXT for thinking/response markers.
Finds [thinking]...[end-thinking] and [response]...[end-response]
pairs.  Returns a list of plists (:type TYPE :text TEXT) where TYPE
is `thinking' or `response'.  Unmarked text is treated as response."
  (let ((segments nil)
        (pos 0)
        (len (length text)))
    (while (< pos len)
      (let* ((think-match
              (magnus-trace--marker-match "\\[thinking\\]\n?" text pos))
             (response-match
              (magnus-trace--marker-match "\\[response\\]\n?" text pos))
             (think-start (car-safe think-match))
             (response-start (car-safe response-match)))
        (cond
         ;; Thinking block comes first (or is only one)
         ((and think-start
               (or (null response-start) (<= think-start response-start)))
          ;; Capture any text before the marker as response
          (when (> think-start pos)
            (let ((pre (string-trim (substring text pos think-start))))
              (when (not (string-empty-p pre))
                (push (list :type 'response :text pre) segments))))
          (let* ((content-start (cdr think-match))
                 (end-match
                  (magnus-trace--marker-match
                   "\\[end-thinking\\]\n?" text content-start)))
            (if end-match
                (progn
                  (let ((content
                         (string-trim
                          (substring text content-start (car end-match)))))
                    (when (not (string-empty-p content))
                      (push (list :type 'thinking :text content) segments)))
                  (setq pos (cdr end-match)))
              ;; No end marker — rest is thinking
              (let ((content (string-trim (substring text content-start))))
                (when (not (string-empty-p content))
                  (push (list :type 'thinking :text content) segments)))
              (setq pos len))))
         ;; Response block comes first (or is only one)
         ((and response-start
               (or (null think-start) (< response-start think-start)))
          ;; Capture any text before the marker as response
          (when (> response-start pos)
            (let ((pre (string-trim (substring text pos response-start))))
              (when (not (string-empty-p pre))
                (push (list :type 'response :text pre) segments))))
          (let* ((content-start (cdr response-match))
                 (end-match
                  (magnus-trace--marker-match
                   "\\[end-response\\]\n?" text content-start)))
            (if end-match
                (progn
                  (let ((content
                         (string-trim
                          (substring text content-start (car end-match)))))
                    (when (not (string-empty-p content))
                      (push (list :type 'response :text content) segments)))
                  (setq pos (cdr end-match)))
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

(defun magnus-trace--complete-json-p (text)
  "Return non-nil when TEXT is one complete JSON value."
  (condition-case nil
      (progn
        (ignore (json-parse-string text :object-type 'alist))
        t)
    (error
     ;; A writer may not have finished the final JSONL record yet.
     nil)))

(defun magnus-trace--read-range (file start end)
  "Read FILE bytes from START through measured END."
  (let ((coding-system-for-read 'binary))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents file nil start end)
      (buffer-string))))

(defun magnus-trace--scan-forward
    (file start end pending discarding record-function)
  "Scan FILE from START to END in bounded chunks.
PENDING and DISCARDING describe an unfinished record before START.
Call RECORD-FUNCTION with the byte start and contents of each record.  An
oversized record is represented by nil contents.  Return reader state as a
plist containing `:pending' and `:discarding'."
  (let* ((chunk-size (max 1 magnus-trace-read-chunk-bytes))
         (record-limit (max 1 magnus-trace-max-record-bytes))
         (position start)
         (fragment (or pending ""))
         (discarding-p discarding)
         (record-start (max 0 (- start (string-bytes fragment)))))
    (when (> (string-bytes fragment) record-limit)
      (setq fragment ""
            discarding-p t)
      (message "Magnus: discarding a trace record larger than %d bytes"
               record-limit))
    (cl-labels
        ((append-piece
          (piece)
          (unless discarding-p
            (if (> (+ (string-bytes fragment) (string-bytes piece))
                   record-limit)
                (progn
                  (setq fragment ""
                        discarding-p t)
                  (message
                   "Magnus: discarding a trace record larger than %d bytes"
                   record-limit))
              (setq fragment (concat fragment piece)))))
         (finish-record
          ()
          (unless (and (not discarding-p) (string-empty-p fragment))
            (funcall record-function
                     record-start
                     (unless discarding-p fragment)))
          (setq fragment ""
                discarding-p nil)))
      (while (< position end)
        (let* ((chunk-end (min end (+ position chunk-size)))
               (text (magnus-trace--read-range file position chunk-end))
               (cursor 0))
          (while (string-match "\n" text cursor)
            ;; Rendering may change global match data, so capture both bounds
            ;; before invoking the callback.
            (let ((line-end (match-beginning 0))
                  (next-line (match-end 0)))
              (append-piece (substring text cursor line-end))
              (finish-record)
              (setq cursor next-line
                    record-start (+ position cursor))))
          (append-piece (substring text cursor))
          (setq position chunk-end)))
      ;; A final JSON value is usable before its newline is flushed.  Malformed
      ;; trailing text stays bounded and is retried when the writer appends.
      (when (and (not discarding-p)
                 (not (string-empty-p fragment))
                 (magnus-trace--complete-json-p fragment))
        (funcall record-function record-start fragment)
        (setq fragment ""))
      (list :pending fragment :discarding discarding-p))))

(defun magnus-trace--tail-snapshot (file keep)
  "Return a bounded tail snapshot of FILE containing at most KEEP records.
Every disk read is capped by `magnus-trace-read-chunk-bytes'.  The entire
prefix is counted so pagination retains its exact earlier-entry count, but
only the tail ring and one bounded unfinished record are held in memory."
  (let* ((size (file-attribute-size (file-attributes file)))
         (capacity (max 0 keep))
         (ring (make-vector capacity nil))
         (next 0)
         (retained 0)
         (total 0)
         state
         pairs)
    (setq state
          (magnus-trace--scan-forward
           file 0 size "" nil
           (lambda (start line)
             (setq total (1+ total))
             (when (> capacity 0)
               (aset ring next (cons start line))
               (setq next (mod (1+ next) capacity)
                     retained (min capacity (1+ retained)))))))
    (when (> retained 0)
      (let ((oldest (if (= retained capacity) next 0)))
        (dotimes (index retained)
          (push (aref ring (mod (+ oldest index) capacity)) pairs))))
    (setq pairs (nreverse pairs))
    (list :lines (mapcar #'cdr pairs)
          :start (if pairs (caar pairs) size)
          :total total
          :skip (- total retained)
          :pending (plist-get state :pending)
          :discarding (plist-get state :discarding)
          :size size)))

(defun magnus-trace--read-previous-records (file end count)
  "Read up to COUNT nonempty JSONL records preceding byte END in FILE.
Return `:lines', `:start', and `:count'.  Reads and retained fragments are
bounded even when a preceding record has no nearby newline."
  (let ((chunk-size (max 1 magnus-trace-read-chunk-bytes))
        (record-limit (max 1 magnus-trace-max-record-bytes))
        (cursor end)
        (fragment "")
        (oversized-p nil)
        (found 0)
        pairs)
    (cl-labels
        ((prepend-piece
          (piece)
          (unless oversized-p
            (if (> (+ (string-bytes piece) (string-bytes fragment))
                   record-limit)
                (progn
                  (setq fragment ""
                        oversized-p t)
                  (message
                   "Magnus: discarding a trace record larger than %d bytes"
                   record-limit))
              (setq fragment (concat piece fragment)))))
         (finish-record
          (start)
          (unless (and (not oversized-p) (string-empty-p fragment))
            (push (cons start (unless oversized-p fragment)) pairs)
            (setq found (1+ found)))
          (setq fragment ""
                oversized-p nil)))
      (while (and (> cursor 0) (< found count))
        (let* ((chunk-start (max 0 (- cursor chunk-size)))
               (text (magnus-trace--read-range file chunk-start cursor))
               (search-end (length text))
               newline)
          (while (and (< found count)
                      (setq newline
                            (cl-position ?\n text :from-end t
                                         :end search-end)))
            (prepend-piece (substring text (1+ newline) search-end))
            (finish-record (+ chunk-start newline 1))
            (setq search-end newline))
          (when (< found count)
            (prepend-piece (substring text 0 search-end))
            (setq cursor chunk-start))))
      (when (and (zerop cursor) (< found count))
        (finish-record 0))
      (list :lines (mapcar #'cdr pairs)
            :start (if pairs (caar pairs) end)
            :count found))))

(defun magnus-trace-normalize-entry (instance entry)
  "Normalize provider JSONL ENTRY for INSTANCE, or return nil.
The returned value uses Magnus's canonical Claude-style trace shape."
  (if (and instance
           (magnus-provider-external-p instance)
           (magnus-provider-operation-p instance 'trace-entry))
      (magnus-provider-call instance 'trace-entry entry)
    entry))

(defun magnus-trace--decode-line (instance line)
  "Decode and normalize one trace LINE for INSTANCE.
Return (t . ENTRY) after successful JSON parsing.  ENTRY may be nil when the
provider intentionally ignores the record.  Return nil for malformed JSON."
  (condition-case err
      (cons t
            (magnus-trace-normalize-entry
             instance
             (json-parse-string line :object-type 'alist)))
    (error
     (message "Magnus: skipped malformed trace record: %s"
              (error-message-string err))
     nil)))

(defun magnus-trace--response-texts (text)
  "Return assistant-visible response segments from TEXT.
Thinking marker segments are deliberately omitted."
  (when (and (stringp text) (not (string-empty-p text)))
    (if (magnus-trace--text-has-markers-p text)
        (let (responses)
          (dolist (segment (magnus-trace-parse-content text))
            (when (eq (plist-get segment :type) 'response)
              (push (plist-get segment :text) responses)))
          (nreverse responses))
      (list text))))

(defun magnus-trace-entry-assistant-texts (entry)
  "Return assistant-visible text strings from canonical trace ENTRY.
User, tool, and thinking-only entries return nil.  Both vector and list
content are accepted, and visible response markers are unwrapped."
  (when (and (listp entry)
             (equal (alist-get 'type entry) "assistant"))
    (let* ((message (alist-get 'message entry))
           (content (and (listp message) (alist-get 'content message)))
           texts)
      (cond
       ((stringp content)
        (setq texts (magnus-trace--response-texts content)))
       ((or (vectorp content) (listp content))
        (seq-doseq (block content)
          (when (and (listp block)
                     (equal (alist-get 'type block) "text"))
            (dolist (text
                     (magnus-trace--response-texts
                      (alist-get 'text block)))
              (push text texts))))
        (setq texts (nreverse texts))))
      texts)))

(defun magnus-trace--cursor-signal-stale (format-string &rest arguments)
  "Signal a stale trace cursor with FORMAT-STRING and ARGUMENTS."
  (signal 'magnus-trace-cursor-stale
          (list (apply #'format format-string arguments))))

(defun magnus-trace--file-identity (attributes)
  "Return an Emacs-28-compatible file identity from ATTRIBUTES."
  (cons (file-attribute-device-number attributes)
        (file-attribute-inode-number attributes)))

(defun magnus-trace-cursor-create (instance)
  "Create an in-memory cursor at the current end of INSTANCE's trace.
Signal `magnus-trace-cursor-error' until the instance has an exact captured
session and an existing trace file.  Existing records are intentionally not
replayed."
  (let* ((session-id (magnus-instance-session-id instance))
         (file (and session-id (magnus-trace-resolve-file instance))))
    (unless session-id
      (signal 'magnus-trace-cursor-error
              '("Instance does not have a captured provider session")))
    (unless (and file (file-regular-p file))
      (signal 'magnus-trace-cursor-error
              '("Instance trace file is not available yet")))
    (let* ((canonical-file (file-truename file))
           (attributes (file-attributes canonical-file))
           (identity (magnus-trace--file-identity attributes)))
      (magnus-trace-cursor--create
       :instance instance
       :instance-id (magnus-instance-id instance)
       :provider (or (magnus-instance-provider instance) 'claude)
       :session-id session-id
       :file canonical-file
       :file-identity identity
       :offset (file-attribute-size attributes)
       :pending ""
       :discarding nil))))

(defun magnus-trace--cursor-validate (cursor)
  "Return the current file size for CURSOR, or signal that it is stale."
  (unless (magnus-trace-cursor-p cursor)
    (signal 'wrong-type-argument (list 'magnus-trace-cursor-p cursor)))
  (let* ((instance (magnus-trace-cursor-instance cursor))
         (session-id (magnus-instance-session-id instance))
         (provider (or (magnus-instance-provider instance) 'claude)))
    (unless (and (equal (magnus-instance-id instance)
                        (magnus-trace-cursor-instance-id cursor))
                 (eq provider (magnus-trace-cursor-provider cursor)))
      (magnus-trace--cursor-signal-stale
       "The Magnus instance identity or provider changed"))
    (unless (equal session-id (magnus-trace-cursor-session-id cursor))
      (magnus-trace--cursor-signal-stale
       "The provider session changed from %s to %s"
       (magnus-trace-cursor-session-id cursor) session-id))
    (let ((file (magnus-trace-resolve-file instance)))
      (unless (and file (file-regular-p file))
        (magnus-trace--cursor-signal-stale
         "The trace file disappeared or is no longer resolvable"))
      (let* ((canonical-file (file-truename file))
             (attributes (file-attributes canonical-file))
             (identity (magnus-trace--file-identity attributes))
             (size (file-attribute-size attributes)))
        (unless (and (equal canonical-file (magnus-trace-cursor-file cursor))
                     (equal identity
                            (magnus-trace-cursor-file-identity cursor)))
          (magnus-trace--cursor-signal-stale
           "The provider replaced the trace file"))
        (when (< size (magnus-trace-cursor-offset cursor))
          (magnus-trace--cursor-signal-stale
           "The trace file was truncated"))
        size))))

(defun magnus-trace-cursor-read (cursor)
  "Read newly completed assistant response strings from CURSOR.
Advance CURSOR in place.  Partial and oversized records reuse the trace
viewer's bounded scanner.  Signal `magnus-trace-cursor-stale' rather than
silently following a replacement session, file, or truncated history."
  (let* ((size (magnus-trace--cursor-validate cursor))
         (start (magnus-trace-cursor-offset cursor))
         (file (magnus-trace-cursor-file cursor))
         texts)
    (when (> size start)
      (let ((state
             (magnus-trace--scan-forward
              file start size
              (magnus-trace-cursor-pending cursor)
              (magnus-trace-cursor-discarding cursor)
              (lambda (_record-start line)
                (when line
                  (when-let ((decoded
                              (magnus-trace--decode-line
                               (magnus-trace-cursor-instance cursor) line)))
                    (dolist (text
                             (magnus-trace-entry-assistant-texts
                              (cdr decoded)))
                      (push text texts))))))))
        (setf (magnus-trace-cursor-offset cursor) size
              (magnus-trace-cursor-pending cursor)
              (plist-get state :pending)
              (magnus-trace-cursor-discarding cursor)
              (plist-get state :discarding))))
    (nreverse texts)))

(defun magnus-trace--render-json-line (line)
  "Parse and render one provider JSONL LINE.
Return non-nil when LINE parsed, even when the provider ignores its record."
  (when-let ((decoded
              (magnus-trace--decode-line magnus-trace--instance line)))
    (when (cdr decoded)
      (magnus-trace--render-entry (cdr decoded)))
    t))

(defun magnus-trace--append-new-entries (jsonl-file)
  "Append new entries from JSONL-FILE to the current trace buffer.
On initial load, scans bounded byte chunks and retains only the last
`magnus-trace-max-initial-entries' entries.  Subsequent refreshes likewise
read new bytes in bounded chunks from the file offset."
  (let* ((file-size (file-attribute-size (file-attributes jsonl-file)))
         (at-end (eobp)))
    (when (and magnus-trace--jsonl-file
               (or (not (equal jsonl-file magnus-trace--jsonl-file))
                   (< file-size magnus-trace--file-offset)))
      (let ((session-id magnus-trace--session-id))
        (magnus-trace--reset-read-state t)
        (setq magnus-trace--session-id session-id)))
    (setq magnus-trace--jsonl-file jsonl-file)
    (cond
     ;; Initial load: scan a stable EOF and retain only the last N records.
     ((zerop magnus-trace--file-offset)
      (if magnus-trace-max-initial-entries
          (let* ((snapshot
                  (magnus-trace--tail-snapshot
                   jsonl-file magnus-trace-max-initial-entries))
                 (lines (plist-get snapshot :lines))
                 (skip (plist-get snapshot :skip)))
            (when lines
              (magnus-trace--render-lines lines skip))
            (setq magnus-trace--file-offset (plist-get snapshot :size)
                  magnus-trace--pending-text
                  (plist-get snapshot :pending)
                  magnus-trace--discarding-incomplete-record-p
                  (plist-get snapshot :discarding)
                  magnus-trace--last-line-count
                  (plist-get snapshot :total)
                  magnus-trace--skip-count skip
                  magnus-trace--page-start-offset
                  (plist-get snapshot :start)))
        ;; An explicit nil limit still renders every entry, but streams them
        ;; instead of materializing the whole JSONL file as one Lisp string.
        (let ((total 0)
              state)
          (setq state
                (magnus-trace--scan-forward
                 jsonl-file 0 file-size "" nil
                 (lambda (_start line)
                   (setq total (1+ total))
                   (when line
                     (magnus-trace--render-lines (list line) 0)))))
          (setq magnus-trace--file-offset file-size
                magnus-trace--pending-text (plist-get state :pending)
                magnus-trace--discarding-incomplete-record-p
                (plist-get state :discarding)
                magnus-trace--last-line-count total
                magnus-trace--skip-count 0
                magnus-trace--page-start-offset 0))))
     ;; Incremental: read only new bytes
     ((> file-size magnus-trace--file-offset)
      (let ((new-count 0)
            state)
        (setq state
              (magnus-trace--scan-forward
               jsonl-file magnus-trace--file-offset file-size
               magnus-trace--pending-text
               magnus-trace--discarding-incomplete-record-p
               (lambda (_start line)
                 (setq new-count (1+ new-count))
                 (when line
                   (magnus-trace--render-lines (list line) 0)))))
        (setq magnus-trace--file-offset file-size
              magnus-trace--pending-text (plist-get state :pending)
              magnus-trace--discarding-incomplete-record-p
              (plist-get state :discarding)
              magnus-trace--last-line-count
              (+ magnus-trace--last-line-count new-count)))))
    ;; Trim buffer if it has grown too large (2x cap)
    (when (and magnus-trace-max-initial-entries
               (> magnus-trace--rendered-count
                  (* 2 magnus-trace-max-initial-entries)))
      (magnus-trace--trim jsonl-file))
    ;; Follow tail if user was at end
    (when at-end
      (goto-char (point-max))
      (let ((win (get-buffer-window (current-buffer))))
        (when win
          (set-window-point win (point-max)))))))

(defun magnus-trace--render-lines (lines skip)
  "Render LINES into the current trace buffer.
SKIP is the number of earlier entries omitted (for the header)."
  (let ((inhibit-read-only t)
        (parsed-count 0))
    (save-excursion
      (goto-char (point-max))
      (when (> skip 0)
        (insert (propertize
                 (format "── %d earlier entries (press [ to load more) ──\n\n"
                         skip)
                 'face 'magnus-trace-separator
                 'magnus-trace-header t)))
      (dolist (line lines)
        (when (and line (magnus-trace--render-json-line line))
          (setq parsed-count (1+ parsed-count)))))
    (setq magnus-trace--rendered-count
          (+ magnus-trace--rendered-count parsed-count))))

(defun magnus-trace--trim (jsonl-file)
  "Trim the current trace buffer to last N entries from JSONL-FILE."
  (let* ((snapshot
          (magnus-trace--tail-snapshot
           jsonl-file magnus-trace-max-initial-entries))
         (lines-to-render (plist-get snapshot :lines))
         (skip (plist-get snapshot :skip))
         (inhibit-read-only t))
    (erase-buffer)
    (remove-overlays)
    (setq magnus-trace--rendered-count 0)
    (when lines-to-render
      (magnus-trace--render-lines lines-to-render skip))
    (setq magnus-trace--last-line-count (plist-get snapshot :total)
          magnus-trace--skip-count skip
          magnus-trace--page-start-offset (plist-get snapshot :start)
          magnus-trace--file-offset (plist-get snapshot :size)
          magnus-trace--pending-text (plist-get snapshot :pending)
          magnus-trace--discarding-incomplete-record-p
          (plist-get snapshot :discarding))))

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
