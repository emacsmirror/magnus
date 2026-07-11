;;; magnus-provider-codex.el --- Codex App Server provider for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Opt-in Codex support with a real Codex TUI in a vterm buffer.  A lightweight
;; JSON-RPC observer connects through `codex app-server proxy' to the managed
;; local daemon, while the TUI is the sole interactive writer for the thread.
;; This preserves native input, approvals, rendering, and session resurrection
;; without starting one App Server per Magnus agent.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magnus-instances)
(require 'magnus-provider)

(declare-function magnus-status-refresh "magnus-status")
(declare-function magnus-process--create-vterm-buffer "magnus-process"
                  (buffer-name))
(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl))
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-string "vterm" (string &optional paste-p))
(defvar magnus-buffer-name)

(defcustom magnus-codex-executable "codex"
  "Path to the Codex executable used for App Server sessions."
  :type 'string
  :group 'magnus)

(defcustom magnus-codex-approval-handler nil
  "Optional function called for each Codex approval request.
The function receives INSTANCE, REQUEST-ID, METHOD, and PARAMS.  It may call
`magnus-codex-respond-approval' immediately or later.  When nil, the request
remains pending and is displayed in the instance buffer."
  :type '(choice (const :tag "Queue for manual handling" nil)
                 (function :tag "Handler function"))
  :group 'magnus)

(defcustom magnus-codex-extra-developer-instructions nil
  "Optional extra developer instructions for Magnus-managed Codex threads.
Magnus always supplies a small provider-neutral coordination preamble."
  :type '(choice (const :tag "None" nil) string)
  :group 'magnus)

(defcustom magnus-codex-remote "unix://"
  "Remote endpoint passed to the Codex TUI.
The default selects the managed App Server daemon's standard Unix socket."
  :type 'string
  :group 'magnus)

(defvar magnus-codex-event-hook nil
  "Hook run after a Codex server event.
Functions receive INSTANCE, METHOD, and PARAMS.")

(defvar magnus-codex-approval-request-hook nil
  "Hook run when Codex requests semantic approval.
Functions receive INSTANCE, REQUEST-ID, METHOD, and PARAMS.  Use
`magnus-codex-respond-approval' to answer the request.")

(defvar magnus-codex--connections (make-hash-table :test #'equal)
  "Instance ID to App Server observer proxy map.")

(defvar magnus-codex--active-turns (make-hash-table :test #'equal)
  "Instance ID to active Codex turn ID map.")

(defvar magnus-codex--show-on-ready (make-hash-table :test #'equal)
  "Instance IDs whose TUI should be displayed after asynchronous startup.")

(defvar magnus-codex--new-thread-owner nil
  "Instance ID currently waiting for its TUI-created thread ID.")

(defvar magnus-codex--new-thread-queue nil
  "FIFO of (PROCESS INSTANCE INITIAL-MESSAGE) waiting to create TUI threads.")

(defvar-local magnus-codex--instance nil
  "Magnus instance represented by the current Codex TUI buffer.")

(defun magnus-codex--observer-buffer (instance)
  "Return INSTANCE's hidden semantic observer log buffer."
  (get-buffer-create
   (format " *magnus-codex-observer:%s*" (magnus-instance-name instance))))

(defun magnus-codex--insert (instance text &optional face)
  "Append TEXT to INSTANCE's hidden observer log, optionally using FACE.
Observer events must never be inserted into the live TUI buffer: doing so
would corrupt terminal rendering and violate the single-writer boundary."
  (with-current-buffer (magnus-codex--observer-buffer instance)
    (let ((inhibit-read-only t)
          (start (point-max)))
      (goto-char start)
      (insert text)
      (when face
        (add-text-properties start (point) (list 'face face))))))

(defun magnus-codex--tui-process (instance)
  "Return INSTANCE's live vterm process, or nil."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (and (buffer-live-p buffer)
         (let ((process (get-buffer-process buffer)))
           (and (process-live-p process) process)))))

(defun magnus-codex--connection (instance)
  "Return the live App Server process for INSTANCE, or nil."
  (let ((process (gethash (magnus-instance-id instance)
                          magnus-codex--connections)))
    (and (process-live-p process) process)))

(defun magnus-codex--approval-table (process)
  "Return PROCESS's private pending approval table."
  (or (process-get process 'magnus-codex-approvals)
      (let ((table (make-hash-table :test #'equal)))
        (process-put process 'magnus-codex-approvals table)
        table)))

(defun magnus-codex--json (object)
  "Serialize OBJECT as compact JSON."
  (json-serialize object :null-object nil :false-object :json-false))

(defun magnus-codex--byte-string (&rest bytes)
  "Return a unibyte string containing BYTES."
  (apply #'unibyte-string bytes))

(defun magnus-codex--integer-bytes (value count)
  "Encode non-negative integer VALUE as COUNT network-order bytes."
  (apply #'magnus-codex--byte-string
         (cl-loop for shift from (* 8 (1- count)) downto 0 by 8
                  collect (logand 255 (ash value (- shift))))))

(defun magnus-codex--websocket-frame (text &optional opcode)
  "Encode TEXT as a masked client WebSocket frame with OPCODE.
OPCODE defaults to 1, the text-frame opcode."
  (let* ((payload (if (multibyte-string-p text)
                      (encode-coding-string text 'utf-8 t)
                    (copy-sequence text)))
         (length (length payload))
         (mask (magnus-codex--byte-string
                (random 256) (random 256) (random 256) (random 256)))
         (header
          (cond
           ((< length 126)
            (magnus-codex--byte-string
             (logior 128 (or opcode 1)) (logior 128 length)))
           ((< length 65536)
            (concat (magnus-codex--byte-string
                     (logior 128 (or opcode 1)) 254)
                    (magnus-codex--integer-bytes length 2)))
           (t
            (concat (magnus-codex--byte-string
                     (logior 128 (or opcode 1)) 255)
                    (magnus-codex--integer-bytes length 8))))))
    (dotimes (index length)
      (aset payload index
            (logxor (aref payload index) (aref mask (% index 4)))))
    (concat header mask payload)))

(defun magnus-codex--send-frame (process text &optional opcode)
  "Send TEXT to PROCESS as a client WebSocket frame with OPCODE."
  (process-send-string process (magnus-codex--websocket-frame text opcode)))

(defun magnus-codex--send-object (process object)
  "Send JSON-RPC OBJECT as one WebSocket text frame through PROCESS."
  (unless (process-live-p process)
    (user-error "Codex App Server is not running"))
  (unless (process-get process 'magnus-codex-websocket-ready)
    (user-error "Codex App Server observer is not ready"))
  (magnus-codex--send-frame process (magnus-codex--json object)))

(defun magnus-codex--request (process method params callback)
  "Send METHOD request with PARAMS through PROCESS and register CALLBACK.
CALLBACK receives RESULT and ERROR, exactly one of which is non-nil."
  (let* ((next (1+ (or (process-get process 'magnus-codex-next-id) 0)))
         (pending (process-get process 'magnus-codex-pending)))
    (process-put process 'magnus-codex-next-id next)
    (puthash next callback pending)
    (magnus-codex--send-object
     process `((jsonrpc . "2.0") (id . ,next) (method . ,method)
               (params . ,params)))
    next))

(defun magnus-codex--notify (process method &optional params)
  "Send METHOD notification and optional PARAMS through PROCESS."
  (magnus-codex--send-object
   process (append `((jsonrpc . "2.0") (method . ,method))
                   (when params `((params . ,params))))))

(defun magnus-codex--parse-line (line)
  "Parse one JSON-RPC LINE into an alist."
  (json-parse-string line :object-type 'alist :array-type 'list
                     :null-object nil :false-object :json-false))

(defun magnus-codex--websocket-handshake (process)
  "Begin the HTTP Upgrade handshake through observer proxy PROCESS."
  (let ((key (base64-encode-string
              (apply #'magnus-codex--byte-string
                     (cl-loop repeat 16 collect (random 256)))
              t)))
    (process-put process 'magnus-codex-websocket-key key)
    (process-send-string
     process
     (encode-coding-string
      (concat "GET / HTTP/1.1\r\n"
              "Host: localhost\r\n"
              "Upgrade: websocket\r\n"
              "Connection: Upgrade\r\n"
              "Sec-WebSocket-Key: " key "\r\n"
              "Sec-WebSocket-Version: 13\r\n\r\n")
      'utf-8 t))))

(defun magnus-codex--websocket-accept (process)
  "Return the expected WebSocket accept value for PROCESS's handshake."
  (base64-encode-string
   (secure-hash
    'sha1
    (concat (process-get process 'magnus-codex-websocket-key)
            "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
    nil nil t)
   t))

(defun magnus-codex--filter (process output)
  "Safely consume App Server OUTPUT from observer PROCESS."
  (condition-case err
      (magnus-codex--filter-output process output)
    (error
     (when-let ((instance (process-get process 'magnus-codex-instance)))
       (magnus-codex--insert
        instance
        (format "\n[observer protocol failure] %s\n"
                (error-message-string err))
        'error))
     (when (process-live-p process)
       (delete-process process)))))

(defun magnus-codex--filter-output (process output)
  "Consume WebSocket handshake and framed App Server OUTPUT from PROCESS."
  (if (process-get process 'magnus-codex-websocket-ready)
      (magnus-codex--consume-frames process output)
    (let* ((partial (concat
                     (or (process-get process 'magnus-codex-handshake-buffer)
                         (magnus-codex--byte-string))
                     output))
           (end (string-match "\r\n\r\n" partial)))
      (if (not end)
          (process-put process 'magnus-codex-handshake-buffer partial)
        (let ((headers (decode-coding-string
                        (substring partial 0 (+ end 4)) 'utf-8 t))
              (remainder (substring partial (+ end 4))))
          (let ((case-fold-search t))
            (unless (string-match-p "\\`HTTP/1\\.[01] 101\\b" headers)
              (error "Codex App Server WebSocket upgrade failed: %s"
                     (string-trim headers)))
            (unless (and
                     (string-match
                      "^Sec-WebSocket-Accept:[ \t]*\\([^\r\n]+\\)" headers)
                     (equal (string-trim (match-string 1 headers))
                            (magnus-codex--websocket-accept process)))
              (error "Codex App Server returned an invalid WebSocket accept")))
          (process-put process 'magnus-codex-handshake-buffer nil)
          (process-put process 'magnus-codex-websocket-ready t)
          (when-let ((callback (process-get process 'magnus-codex-on-open)))
            (process-put process 'magnus-codex-on-open nil)
            (funcall callback))
          (unless (string-empty-p remainder)
            (magnus-codex--consume-frames process remainder)))))))

(defun magnus-codex--read-integer (data start count)
  "Read COUNT network-order bytes from DATA beginning at START."
  (cl-loop with value = 0
           for index from start below (+ start count)
           do (setq value (+ (ash value 8) (aref data index)))
           finally return value))

(defun magnus-codex--consume-frames (process output)
  "Consume zero or more WebSocket frames from PROCESS OUTPUT."
  (let ((data (concat (or (process-get process 'magnus-codex-frame-buffer)
                          (magnus-codex--byte-string))
                      output))
        (continue t))
    (while continue
      (if (< (length data) 2)
          (setq continue nil)
        (let* ((first (aref data 0))
               (second (aref data 1))
               (fin (not (zerop (logand first 128))))
               (opcode (logand first 15))
               (masked (not (zerop (logand second 128))))
               (short-length (logand second 127))
               (length-bytes (cond ((= short-length 126) 2)
                                   ((= short-length 127) 8)
                                   (t 0)))
               (base (+ 2 length-bytes))
               (mask-length (if masked 4 0)))
          (if (< (length data) (+ base mask-length))
              (setq continue nil)
            (let ((payload-length
                   (if (zerop length-bytes)
                       short-length
                     (magnus-codex--read-integer data 2 length-bytes))))
              (when (> payload-length (* 16 1024 1024))
                (error "Codex observer frame exceeds 16 MiB"))
              (let* ((payload-start (+ base mask-length))
                     (frame-end (+ payload-start payload-length)))
                (if (> frame-end (length data))
                    (setq continue nil)
                  (let ((payload (copy-sequence
                                  (substring data payload-start frame-end))))
                    (when masked
                      (let ((mask (substring data base (+ base 4))))
                        (dotimes (index payload-length)
                          (aset payload index
                                (logxor (aref payload index)
                                        (aref mask (% index 4)))))))
                    (setq data (substring data frame-end))
                    (magnus-codex--handle-frame
                     process fin opcode payload)))))))))
    (process-put process 'magnus-codex-frame-buffer data)))

(defun magnus-codex--handle-frame (process fin opcode payload)
  "Handle one WebSocket frame from PROCESS with FIN, OPCODE, and PAYLOAD."
  (pcase opcode
    (8
     (process-put process 'magnus-codex-intentional-stop t)
     (delete-process process))
    (9 (magnus-codex--send-frame process payload 10))
    (10 nil)
    ((or 0 1)
     (let ((fragments (append (process-get process 'magnus-codex-fragments)
                              (list payload))))
       (if (not fin)
           (process-put process 'magnus-codex-fragments fragments)
         (process-put process 'magnus-codex-fragments nil)
         (condition-case err
             (magnus-codex--dispatch-message
              process
              (magnus-codex--parse-line
               (decode-coding-string (apply #'concat fragments) 'utf-8 t)))
           (error
            (when-let ((instance
                        (process-get process 'magnus-codex-instance)))
              (magnus-codex--insert
               instance
               (format "\n[protocol error] %s\n" (error-message-string err))
               'error)))))))
    (_ nil)))

(defun magnus-codex--dispatch-message (process message)
  "Dispatch parsed JSON-RPC MESSAGE received from PROCESS."
  (let ((id (alist-get 'id message))
        (method (alist-get 'method message))
        (params (alist-get 'params message)))
    (cond
     ((and method id)
      (magnus-codex--handle-server-request process id method params))
     (method
      (magnus-codex--handle-notification process method params))
     (id
      (let* ((pending (process-get process 'magnus-codex-pending))
             (callback (gethash id pending)))
        (when callback
          (remhash id pending)
          (funcall callback (alist-get 'result message)
                   (alist-get 'error message))))))))

(defun magnus-codex--approval-method-p (method)
  "Return non-nil when METHOD is a supported approval request."
  (member method '("item/commandExecution/requestApproval"
                   "item/fileChange/requestApproval"
                   "item/permissions/requestApproval")))

(defun magnus-codex--handle-server-request (process id method params)
  "Handle server request ID METHOD PARAMS from PROCESS."
  (let ((instance (process-get process 'magnus-codex-instance)))
    (if (magnus-codex--approval-method-p method)
        (progn
          (puthash id (list :instance instance :process process
                            :method method :params params)
                   (magnus-codex--approval-table process))
          (magnus-codex--insert
           instance
           (format "\n[approval pending] %s\n%s\n"
                   method (magnus-codex--approval-summary params))
           'warning)
          (run-hook-with-args 'magnus-codex-approval-request-hook
                              instance id method params)
          (when magnus-codex-approval-handler
            (run-at-time 0 nil magnus-codex-approval-handler
                         instance id method params)))
      (magnus-codex--insert instance
                            (format "\n[unsupported server request] %s\n"
                                    method)
                            'warning)
      (magnus-codex--send-object
       process `((jsonrpc . "2.0") (id . ,id)
                 (error . ((code . -32601)
                           (message . "Unsupported server request"))))))))

(defun magnus-codex--approval-summary (params)
  "Return a readable summary of approval PARAMS."
  (string-join
   (delq nil
         (list (when-let ((command (alist-get 'command params)))
                 (cond
                  ((stringp command) command)
                  ((listp command)
                   (string-join (mapcar (lambda (arg) (format "%s" arg))
                                        command)
                                " "))
                  (t (format "%s" command))))
               (when-let ((cwd (alist-get 'cwd params)))
                 (format "cwd: %s" cwd))
               (when-let ((reason (alist-get 'reason params))) reason)))
   "\n"))

(defun magnus-codex-respond-approval (instance request-id decision)
  "Respond to INSTANCE approval REQUEST-ID with DECISION.
DECISION is normally accept, acceptForSession, decline, or cancel.  A
permission-profile request instead requires its complete response alist."
  (let* ((process (magnus-codex--connection instance))
         (table (and process (magnus-codex--approval-table process)))
         (entry (and table (gethash request-id table)))
         (owner (plist-get entry :instance))
         (method (plist-get entry :method))
         (result (if (equal method "item/permissions/requestApproval")
                     decision
                   `((decision . ,decision)))))
    (unless (and entry (eq owner instance))
      (user-error "No pending Codex approval %s for this instance" request-id))
    (when (and (equal method "item/permissions/requestApproval")
               (not (and (listp decision)
                         (alist-get 'permissions decision))))
      (user-error "Permission approval requires a response alist with permissions"))
    (when (and (not (equal method "item/permissions/requestApproval"))
               (not (member decision
                            '("accept" "acceptForSession" "decline" "cancel"))))
      (user-error "Unsupported Codex approval decision: %s" decision))
    (magnus-codex--send-object
     process `((jsonrpc . "2.0") (id . ,request-id)
               (result . ,result)))
    (remhash request-id table)
    (magnus-codex--insert instance
                          (format "[approval answered: %s]\n" decision)
                          'font-lock-comment-face)))

(defun magnus-codex--turn-id (turn)
  "Extract an ID from TURN."
  (and (listp turn) (alist-get 'id turn)))

(defun magnus-codex--handle-notification (process method params)
  "Handle METHOD notification with PARAMS from PROCESS."
  (let ((instance (process-get process 'magnus-codex-instance)))
    (pcase method
      ("item/agentMessage/delta"
       (magnus-codex--insert instance (or (alist-get 'delta params) "")))
      ("thread/started"
       (when (and (process-get process 'magnus-codex-awaiting-tui-thread)
                  (equal magnus-codex--new-thread-owner
                         (magnus-instance-id instance)))
         (when-let ((thread-id
                     (alist-get 'id (alist-get 'thread params))))
           (process-put process 'magnus-codex-awaiting-tui-thread nil)
           (magnus-instances-update instance :session-id thread-id)
           (magnus-codex--insert
            instance (format "[TUI thread started: %s]\n" thread-id)
            'font-lock-comment-face)
           (magnus-codex--release-new-thread-owner instance))))
      ("turn/plan/updated"
       (magnus-codex--insert instance
                             (magnus-codex--format-plan params)
                             'font-lock-comment-face))
      ("turn/started"
       (process-put process 'magnus-codex-turn-starting nil)
       (let ((turn-id (magnus-codex--turn-id (alist-get 'turn params))))
         (when turn-id
           (puthash (magnus-instance-id instance) turn-id
                    magnus-codex--active-turns)))
       (magnus-codex--insert instance "\n[turn started]\n"
                             'font-lock-comment-face))
      ("turn/completed"
       (remhash (magnus-instance-id instance) magnus-codex--active-turns)
       (process-put process 'magnus-codex-turn-starting nil)
       (magnus-codex--insert instance "\n[turn completed]\n\n"
                             'font-lock-comment-face))
      ("thread/status/changed"
       (magnus-codex--insert
        instance (format "\n[status: %s]\n"
                         (magnus-codex--thread-status-name
                          (alist-get 'status params)))
        'font-lock-comment-face))
      ("error"
       (magnus-codex--insert instance
                             (format "\n[Codex error] %s\n"
                                     (or (alist-get 'message
                                                    (alist-get 'error params))
                                         params))
                             'error)))
    (run-hook-with-args 'magnus-codex-event-hook instance method params)))

(defun magnus-codex--thread-status-name (status)
  "Return the readable type name from App Server STATUS."
  (if (listp status)
      (or (alist-get 'type status) "unknown")
    (or status "unknown")))

(defun magnus-codex--format-plan (params)
  "Format plan notification PARAMS for the output buffer."
  (concat
   "\n[plan]\n"
   (mapconcat
    (lambda (step)
      (format "  %-11s %s" (or (alist-get 'status step) "")
              (or (alist-get 'step step) "")))
    (alist-get 'plan params) "\n")
   "\n"))

(defun magnus-codex--sentinel (process event)
  "Handle observer PROCESS termination EVENT."
  (unless (process-live-p process)
    (when-let ((instance (process-get process 'magnus-codex-instance)))
      (when (process-get process 'magnus-codex-awaiting-tui-thread)
        (process-put process 'magnus-codex-awaiting-tui-thread nil)
        (when-let ((terminal (magnus-codex--tui-process instance)))
          (set-process-query-on-exit-flag terminal nil)
          (kill-process terminal))
        (magnus-codex--release-new-thread-owner instance))
      (when (eq process (gethash (magnus-instance-id instance)
                                 magnus-codex--connections))
        (remhash (magnus-instance-id instance) magnus-codex--connections))
      (remhash (magnus-instance-id instance) magnus-codex--active-turns)
      (magnus-codex--clear-approvals instance process)
      ;; Once the TUI owns the thread, observer failure must not terminate or
      ;; misreport the interactive session.  Before that handoff it is fatal.
      (unless (or (magnus-codex--tui-process instance)
                  (eq (magnus-instance-status instance) 'purged))
        (magnus-instances-update instance :status 'stopped))
      (unless (process-get process 'magnus-codex-intentional-stop)
        (magnus-codex--insert instance (format "\n[observer %s]" event)
                              'font-lock-comment-face))
      (when (and (boundp 'magnus-buffer-name)
                 (get-buffer magnus-buffer-name))
        (magnus-status-refresh)))))

(defun magnus-codex--clear-approvals (instance &optional process)
  "Remove pending approvals owned by INSTANCE."
  (when-let ((connection
              (or process
                  (gethash (magnus-instance-id instance)
                           magnus-codex--connections))))
    (clrhash (magnus-codex--approval-table connection))))

(defun magnus-codex--ensure-daemon ()
  "Ensure the managed local Codex App Server daemon is running."
  (unless (executable-find magnus-codex-executable)
    (user-error "Cannot find Codex executable: %s" magnus-codex-executable))
  (with-temp-buffer
    (let ((status (process-file magnus-codex-executable nil t nil
                                "app-server" "daemon" "start")))
      (unless (and (integerp status) (zerop status))
        (user-error "Could not start Codex App Server daemon: %s"
                    (string-trim (buffer-string)))))))

(defun magnus-codex--start-process (instance)
  "Start INSTANCE's semantic observer proxy to the managed daemon."
  (let* ((name (format "magnus-codex-%s" (magnus-instance-id instance)))
         (stderr (get-buffer-create (format " *%s-stderr*" name)))
         (default-directory (magnus-instance-directory instance))
         (process (make-process
                   :name name
                   :command (list magnus-codex-executable
                                  "app-server" "proxy")
                   :connection-type 'pipe
                   :coding 'binary
                   :noquery t
                   :buffer nil
                   :stderr stderr
                   :filter #'magnus-codex--filter
                   :sentinel #'magnus-codex--sentinel)))
    (process-put process 'magnus-codex-instance instance)
    (process-put process 'magnus-codex-pending (make-hash-table :test #'equal))
    (process-put process 'magnus-codex-approvals
                 (make-hash-table :test #'equal))
    (process-put process 'magnus-codex-next-id 0)
    (process-put process 'magnus-codex-handshake-buffer
                 (magnus-codex--byte-string))
    (process-put process 'magnus-codex-frame-buffer
                 (magnus-codex--byte-string))
    (process-put process 'magnus-codex-websocket-ready nil)
    (process-put process 'magnus-codex-input-queue nil)
    (process-put process 'magnus-codex-turn-starting nil)
    (puthash (magnus-instance-id instance) process magnus-codex--connections)
    process))

(defun magnus-codex-start (instance &optional initial-message)
  "Start or resume Codex INSTANCE in a TUI.
INITIAL-MESSAGE, when non-nil, is submitted by the TUI as its first prompt."
  (when (magnus-codex--connection instance)
    (user-error "Codex instance `%s' is already running"
                (magnus-instance-name instance)))
  (magnus-codex--ensure-daemon)
  (let ((process (magnus-codex--start-process instance)))
    (process-put process 'magnus-codex-on-open
                 (lambda ()
                   (magnus-codex--initialize
                    process instance initial-message)))
    (magnus-codex--websocket-handshake process)
    process))

(defun magnus-codex--initialize (process instance initial-message)
  "Initialize observer PROCESS, then open INSTANCE with INITIAL-MESSAGE."
  (magnus-codex--request
   process "initialize"
   '((clientInfo . ((name . "magnus") (title . "Magnus")
                    (version . "0.1.0")))
     (capabilities . ((experimentalApi . :json-false))))
   (lambda (_result error)
     (if error
         (progn
           (magnus-codex--insert instance
                                 (format "[initialize failed] %s\n" error)
                                 'error)
           (delete-process process))
       (magnus-codex--notify process "initialized")
       (if (magnus-instance-session-id instance)
           (magnus-codex--open-thread process instance initial-message)
         (magnus-codex--start-new-thread
          process instance initial-message))))))

(defun magnus-codex--start-new-thread (process instance initial-message)
  "Launch a new TUI-owned thread, serializing its identity handoff."
  (if magnus-codex--new-thread-owner
      (setq magnus-codex--new-thread-queue
            (append magnus-codex--new-thread-queue
                    (list (list process instance initial-message))))
    (setq magnus-codex--new-thread-owner (magnus-instance-id instance))
    (process-put process 'magnus-codex-awaiting-tui-thread t)
    (magnus-codex--spawn-tui
     instance (magnus-codex--onboarding-prompt instance initial-message))))

(defun magnus-codex--onboarding-prompt (instance &optional initial-message)
  "Return durable first-turn onboarding for INSTANCE and INITIAL-MESSAGE.
Remote TUI-created threads do not reliably honor CLI developer-instruction
overrides, so the den contract is made visible and persistent in history."
  (concat
   (magnus-codex--instructions instance)
   (if initial-message
       (concat "\n\nInitial task from the user:\n" initial-message)
     (concat "\n\nNo separate task was supplied. Complete your orientation, "
             "report that you are ready, and then wait for the user."))))

(defun magnus-codex--release-new-thread-owner (instance)
  "Release INSTANCE's new-thread handoff lock and start the next TUI."
  (when (equal magnus-codex--new-thread-owner
               (magnus-instance-id instance))
    (setq magnus-codex--new-thread-owner nil)
    (magnus-codex--start-next-new-thread)))

(defun magnus-codex--start-next-new-thread ()
  "Start the next valid queued new Codex TUI, if any."
  (let (entry)
    (while (and magnus-codex--new-thread-queue (not entry))
      (let ((candidate (pop magnus-codex--new-thread-queue)))
        (when (and (process-live-p (car candidate))
                   (not (eq (magnus-instance-status (cadr candidate))
                            'purged)))
          (setq entry candidate))))
    (when entry
      (apply #'magnus-codex--start-new-thread entry))))

(defun magnus-codex--open-thread (process instance initial-message)
  "Resume INSTANCE on observer PROCESS, then launch its TUI."
  (let* ((thread-id (magnus-instance-session-id instance))
         (method "thread/resume")
         (params `((threadId . ,thread-id)
                   (cwd . ,(magnus-instance-directory instance))
                   (developerInstructions . ,(magnus-codex--instructions instance)))))
    (magnus-codex--request
     process method params
     (lambda (result error)
       (if error
           (progn
             (magnus-codex--insert instance
                                   (format "[%s failed] %s\n" method error)
                                   'error)
             ;; A brand-new TUI session has no rollout until its first turn.
             ;; If it was archived before then, nothing can be lost: clear the
             ;; unusable ID and let the TUI create a fresh persisted identity.
             (magnus-instances-update instance :session-id nil)
             (magnus-codex--start-new-thread
              process instance initial-message))
         (let ((new-id (alist-get 'id (alist-get 'thread result))))
           (when new-id
             (magnus-instances-update instance :session-id new-id))
           (magnus-codex--insert instance
                                 (format "[thread %s: %s]\n\n"
                                         (if thread-id "resumed" "started")
                                         (or new-id thread-id))
                                 'font-lock-comment-face)
           (magnus-codex--spawn-tui instance initial-message)))))))

(defun magnus-codex--tui-command (instance &optional initial-message)
  "Return the shell command used to run INSTANCE's new or resumed TUI.
INITIAL-MESSAGE becomes the optional initial prompt."
  (let ((thread-id (magnus-instance-session-id instance)))
    (mapconcat
     #'shell-quote-argument
     (append (list "exec" magnus-codex-executable)
             (when thread-id (list "resume"))
             (list "--remote" magnus-codex-remote
                   "-C" (magnus-instance-directory instance))
             (when thread-id (list thread-id))
             (when initial-message (list initial-message)))
     " ")))

(defun magnus-codex--spawn-tui (instance &optional initial-message)
  "Launch INSTANCE's full Codex TUI, optionally with INITIAL-MESSAGE."
  (let* ((buffer-name (format "*codex:%s*" (magnus-instance-name instance)))
         (default-directory (magnus-instance-directory instance))
         (buffer (magnus-process--create-vterm-buffer buffer-name))
         (command (magnus-codex--tui-command instance initial-message)))
    (with-current-buffer buffer
      (setq-local magnus-codex--instance instance))
    ;; A freshly created vterm may still be initializing its login shell.  Give
    ;; it one tick before pasting, then another before submitting the command.
    (run-with-timer
     0.1 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (vterm-send-string command)))))
    (run-with-timer
     0.5 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (vterm-send-return)))))
    (magnus-instances-update instance :buffer buffer :status 'running)
    (magnus-codex--setup-tui-sentinel instance buffer)
    (when (gethash (magnus-instance-id instance) magnus-codex--show-on-ready)
      (remhash (magnus-instance-id instance) magnus-codex--show-on-ready)
      (pop-to-buffer buffer))
    (when (and (boundp 'magnus-buffer-name)
               (get-buffer magnus-buffer-name))
      (magnus-status-refresh))
    buffer))

(defun magnus-codex--setup-tui-sentinel (instance buffer)
  "Track the interactive Codex process for INSTANCE in BUFFER."
  (when-let ((process (get-buffer-process buffer)))
    (process-put process 'magnus-codex-observer
                 (magnus-codex--connection instance))
    (set-process-sentinel
     process
     (lambda (terminal _event)
       (unless (process-live-p terminal)
         (when-let ((observer
                     (process-get terminal 'magnus-codex-observer)))
           (process-put observer 'magnus-codex-intentional-stop t)
           (when (process-live-p observer)
             (delete-process observer)))
         ;; A stale vterm can report its exit after a replacement TUI exists.
         ;; Only the terminal still owned by INSTANCE may release or stop it.
         (when (and (buffer-live-p (magnus-instance-buffer instance))
                    (eq terminal
                        (get-buffer-process
                         (magnus-instance-buffer instance))))
           (when (and (null (magnus-instance-session-id instance))
                      (equal magnus-codex--new-thread-owner
                             (magnus-instance-id instance)))
             (magnus-codex--release-new-thread-owner instance))
           (unless (eq (magnus-instance-status instance) 'purged)
             (magnus-instances-update instance :status 'stopped))
           (when (and (boundp 'magnus-buffer-name)
                      (get-buffer magnus-buffer-name))
             (magnus-status-refresh))))))))

(defun magnus-codex--instructions (instance)
  "Build Magnus identity and coordination instructions for Codex INSTANCE."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (memory-relative (format ".claude/agents/%s/memory.md" name))
         (memory-file (expand-file-name memory-relative directory))
         (returning (file-exists-p memory-file)))
    (concat
     (format "You are %s, a Magnus-managed Codex agent. " name)
     (if returning
         (format
          (concat "You have been here before. Before anything else, read %s. "
                  "It is your own prior voice: decisions you made, patterns "
                  "you found, relationships, and unfinished work.\n\n")
          memory-relative)
       (format
        (concat "This is your first session under this identity. Your home is "
                ".claude/agents/%s/. Before signing off, create %s as a "
                "first-person letter to future-you, not a status report.\n\n")
        name memory-relative))

     "Magnus may be running several agents in this project. Get oriented before "
     "changing files:\n"
     "1. Read .magnus-coord.md when it exists. Review Active Work, Discoveries, "
     "Decisions, and the recent Log.\n"
     "2. Check for overlapping file claims. Discuss conflicts in the Log before "
     "editing, then announce your plan and claim the files you expect to touch.\n"
     "3. Read relevant project instructions and inspect existing work before "
     "choosing an implementation.\n\n"

     "While working:\n"
     "- Check .magnus-coord.md periodically. Use @mentions to reach teammates.\n"
     "- Put non-obvious facts and gotchas in Discoveries so peers do not have to "
     "rediscover them. Put shared architectural choices in Decisions.\n"
     "- Create .claude/agents/" name "/busy when you need uninterrupted focus; "
     "remove it when that focus period ends.\n"
     "- Other agents' first-person memories live under .claude/agents/. Read a "
     "relevant one when prior context or expertise would save work.\n"
     "- Before requesting user attention, log [ATTENTION] with the reason; log "
     "when it is resolved so agents can serialize interruptions.\n"
     "- Do not overwrite another agent's Active Work row or assume permission to "
     "commit, push, deploy, or perform unrelated changes.\n\n"

     "When finishing:\n"
     "1. Release or mark your Active Work row done and log what finished.\n"
     "2. Record useful discoveries and decisions.\n"
     "3. Update " memory-relative " in first person: what you learned, why you "
     "chose this approach, relationships, and what remains.\n"
     "4. When a commit is authorized, preserve the why, tradeoffs, and gotchas in "
     "its message; the coordination log is ephemeral.\n\n"

     "Thinking out loud:\n"
     "For every user-facing message, put a candid visible working notebook inside "
     "[thinking]...[end-thinking], followed by the answer inside "
     "[response]...[end-response]. The notebook is valuable before the conclusion "
     "is polished: state the current hypothesis, evidence, uncertainty, plausible "
     "branches, constraints, contradictions, dead ends, and corrections as they "
     "become relevant. If a new fact reverses the plan, say so and explain why. "
     "Avoid empty narration such as 'I will inspect the code.' Do not claim this "
     "reveals private hidden chain-of-thought; it is an explicit collaborative "
     "engineering journal written for the user.\n\n"

     "Begin by orienting and reading your memory when present, then handle the "
     "user's task."
     (when magnus-codex-extra-developer-instructions
       (concat "\n\n" magnus-codex-extra-developer-instructions)))))

(defun magnus-codex--input (text)
  "Build App Server user input for TEXT."
  `[((type . "text") (text . ,text))])

(defun magnus-codex-send (instance text)
  "Submit TEXT through INSTANCE's Codex TUI.
The semantic observer never starts or steers turns; this keeps the TUI as the
only interactive writer and lets Codex apply its native input semantics."
  (let ((buffer (magnus-instance-buffer instance)))
    (unless (and (buffer-live-p buffer) (magnus-codex--tui-process instance))
      (user-error "Codex instance `%s' is not running"
                  (magnus-instance-name instance)))
    (with-current-buffer buffer
      (vterm-send-string text))
    (run-with-timer
     0.1 nil
     (lambda ()
       (when (and (buffer-live-p buffer)
                  (process-live-p (get-buffer-process buffer)))
         (with-current-buffer buffer
           (vterm-send-return)))))))

(defun magnus-codex-steer (instance text &optional _expected-turn-id)
  "Submit TEXT through INSTANCE's TUI using Codex's native active-turn UI."
  (magnus-codex-send instance text))

(defun magnus-codex-interrupt (instance)
  "Interrupt the active turn through Codex INSTANCE's TUI."
  (let ((buffer (magnus-instance-buffer instance)))
    (unless (and (buffer-live-p buffer) (magnus-codex--tui-process instance))
      (user-error "Codex instance `%s' is not running"
                  (magnus-instance-name instance)))
    (with-current-buffer buffer
      (vterm-send-key "C-c"))))

(defun magnus-codex-stop (instance &optional force)
  "Stop Codex INSTANCE's TUI and observer, never the shared daemon.
When FORCE is non-nil, kill subprocesses immediately."
  (let ((buffer (magnus-instance-buffer instance)))
    (when-let ((observer (magnus-codex--connection instance)))
      (process-put observer 'magnus-codex-intentional-stop t)
      (if force (kill-process observer) (delete-process observer)))
    (when (buffer-live-p buffer)
      (when-let ((terminal (get-buffer-process buffer)))
        (set-process-query-on-exit-flag terminal nil)
        (when (process-live-p terminal)
          (if force (kill-process terminal) (delete-process terminal))))
      (kill-buffer buffer)))
  (remhash (magnus-instance-id instance) magnus-codex--connections)
  (remhash (magnus-instance-id instance) magnus-codex--active-turns)
  (remhash (magnus-instance-id instance) magnus-codex--show-on-ready)
  (setq magnus-codex--new-thread-queue
        (cl-remove-if (lambda (entry) (eq (cadr entry) instance))
                      magnus-codex--new-thread-queue))
  (magnus-codex--release-new-thread-owner instance)
  (magnus-instances-update instance :status 'stopped :buffer nil))

(defun magnus-codex-running-p (instance)
  "Return non-nil when Codex INSTANCE has a live interactive TUI."
  (and (magnus-codex--tui-process instance) t))

(defun magnus-codex-switch-to (instance)
  "Switch to Codex INSTANCE, resuming its TUI when necessary."
  (if (magnus-codex-running-p instance)
      (pop-to-buffer (magnus-instance-buffer instance))
    (puthash (magnus-instance-id instance) t magnus-codex--show-on-ready)
    (unless (magnus-codex--connection instance)
      (magnus-codex-start instance))
    (message "Magnus: starting Codex TUI for %s..."
             (magnus-instance-name instance))))

(defun magnus-codex-answer-approval ()
  "Answer a fallback observer approval in the current Codex TUI.
Normal interactive approvals are owned and rendered by the TUI itself."
  (interactive)
  (unless magnus-codex--instance
    (user-error "This is not a Magnus Codex buffer"))
  (let* ((process (magnus-codex--connection magnus-codex--instance))
         (table (and process (magnus-codex--approval-table process)))
         requests)
    (when table
      (maphash
       (lambda (request-id entry)
         (push (cons (format "%s: %s" request-id
                             (plist-get entry :method))
                     request-id)
               requests))
       table))
    (unless requests
      (user-error "This Codex instance has no pending approvals"))
    (let* ((label (if (= (length requests) 1)
                      (caar requests)
                    (completing-read "Approval: " requests nil t)))
           (request-id (cdr (assoc label requests)))
           (entry (gethash request-id table)))
      (when (equal (plist-get entry :method) "item/permissions/requestApproval")
        (user-error "Permission-profile approvals require a custom approval handler"))
      (magnus-codex-respond-approval
       magnus-codex--instance request-id
       (completing-read
        "Decision: "
        '("accept" "acceptForSession" "decline" "cancel") nil t)))))

(magnus-provider-register
 'codex
 '((start . magnus-codex-start)
   (resume . magnus-codex-start)
   (send . magnus-codex-send)
   (steer . magnus-codex-steer)
   (interrupt . magnus-codex-interrupt)
   (stop . magnus-codex-stop)
   (running-p . magnus-codex-running-p)
   (switch-to . magnus-codex-switch-to)))

(provide 'magnus-provider-codex)
;;; magnus-provider-codex.el ends here
