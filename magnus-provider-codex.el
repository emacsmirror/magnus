;;; magnus-provider-codex.el --- Codex App Server provider for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Opt-in Codex support using the versioned App Server JSON-RPC protocol over
;; stdio.  This module does not alter Magnus's default Claude Code/vterm path.
;; It provides thread start/resume, turn start/steer/interrupt, streamed agent
;; messages and plans, and a semantic approval callback surface.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magnus-instances)
(require 'magnus-provider)

(declare-function magnus-status-refresh "magnus-status")
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

(defcustom magnus-codex-active-turn-delivery 'queue
  "How `magnus-codex-send' handles text while a turn is active.
`queue' preserves Claude/vterm semantics by starting a new turn after the
current one completes.  `steer' injects the text into the active turn via
App Server's turn/steer method.  Calls to `magnus-codex-steer' always steer
regardless of this setting."
  :type '(choice (const :tag "Queue for the next turn" queue)
                 (const :tag "Steer the active turn" steer))
  :group 'magnus)

(defvar magnus-codex-event-hook nil
  "Hook run after a Codex server event.
Functions receive INSTANCE, METHOD, and PARAMS.")

(defvar magnus-codex-approval-request-hook nil
  "Hook run when Codex requests semantic approval.
Functions receive INSTANCE, REQUEST-ID, METHOD, and PARAMS.  Use
`magnus-codex-respond-approval' to answer the request.")

(defvar magnus-codex--connections (make-hash-table :test #'equal)
  "Instance ID to App Server process map.")

(defvar magnus-codex--active-turns (make-hash-table :test #'equal)
  "Instance ID to active Codex turn ID map.")

(defvar-local magnus-codex--instance nil
  "Codex instance represented by the current output buffer.")

(define-derived-mode magnus-codex-mode special-mode "Magnus-Codex"
  "Major mode for streamed Codex App Server output."
  :group 'magnus
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(let ((map magnus-codex-mode-map))
  (define-key map (kbd "m") #'magnus-codex-send-message)
  (define-key map (kbd "a") #'magnus-codex-answer-approval)
  (define-key map (kbd "C-c C-c") #'magnus-codex-send-message)
  (define-key map (kbd "C-c C-k") #'magnus-codex-interrupt-current))

(defun magnus-codex--buffer (instance)
  "Create or return the output buffer for INSTANCE."
  (let ((buffer (magnus-instance-buffer instance)))
    (if (buffer-live-p buffer)
        buffer
      (setq buffer (get-buffer-create
                    (format "*codex:%s*" (magnus-instance-name instance))))
      (with-current-buffer buffer
        (magnus-codex-mode)
        (setq-local magnus-codex--instance instance)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Codex agent: %s\nDirectory: %s\n\n"
                          (magnus-instance-name instance)
                          (magnus-instance-directory instance)))))
      (magnus-instances-update instance :buffer buffer)
      buffer)))

(defun magnus-codex--insert (instance text &optional face)
  "Append TEXT to INSTANCE's output buffer, optionally using FACE."
  (let ((buffer (magnus-codex--buffer instance)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (moving (= (point) (point-max))))
          (goto-char (point-max))
          (insert (if face (propertize text 'face face) text))
          (when moving (goto-char (point-max))))))))

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

(defun magnus-codex--send-object (process object)
  "Send JSON-RPC OBJECT followed by a newline to PROCESS."
  (unless (process-live-p process)
    (user-error "Codex App Server is not running"))
  (process-send-string process (concat (magnus-codex--json object) "\n")))

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

(defun magnus-codex--filter (process output)
  "Consume newline-delimited JSON-RPC OUTPUT from PROCESS."
  (let* ((partial (concat (or (process-get process 'magnus-codex-partial) "")
                          output))
         (lines (split-string partial "\n"))
         (complete (butlast lines))
         (remainder (car (last lines))))
    (process-put process 'magnus-codex-partial remainder)
    (dolist (line complete)
      (unless (string-empty-p line)
        (condition-case err
            (magnus-codex--dispatch-message
             process (magnus-codex--parse-line line))
          (error
           (when-let ((instance (process-get process 'magnus-codex-instance)))
             (magnus-codex--insert
              instance
              (format "\n[protocol error] %s\n" (error-message-string err))
              'error))))))))

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
                             'font-lock-comment-face)
       (magnus-codex--start-next-queued-turn process instance))
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
  "Handle PROCESS termination EVENT."
  (unless (process-live-p process)
    (when-let ((instance (process-get process 'magnus-codex-instance)))
      (remhash (magnus-instance-id instance) magnus-codex--connections)
      (remhash (magnus-instance-id instance) magnus-codex--active-turns)
      (magnus-codex--clear-approvals instance process)
      (unless (eq (magnus-instance-status instance) 'purged)
        (magnus-instances-update instance :status 'stopped))
      (unless (process-get process 'magnus-codex-intentional-stop)
        (magnus-codex--insert instance (format "\n[App Server %s]" event)
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

(defun magnus-codex--start-process (instance)
  "Start a Codex App Server process for INSTANCE."
  (let* ((name (format "magnus-codex-%s" (magnus-instance-id instance)))
         (stderr (get-buffer-create (format " *%s-stderr*" name)))
         (default-directory (magnus-instance-directory instance))
         (process (make-process
                   :name name
                   :command (list magnus-codex-executable
                                  "app-server" "--stdio")
                   :connection-type 'pipe
                   :coding 'utf-8-unix
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
    (process-put process 'magnus-codex-partial "")
    (process-put process 'magnus-codex-input-queue nil)
    (process-put process 'magnus-codex-turn-starting nil)
    (puthash (magnus-instance-id instance) process magnus-codex--connections)
    process))

(defun magnus-codex-start (instance &optional initial-message)
  "Start or resume Codex INSTANCE, then optionally send INITIAL-MESSAGE."
  (when (magnus-codex--connection instance)
    (user-error "Codex instance `%s' is already running"
                (magnus-instance-name instance)))
  (magnus-codex--buffer instance)
  (let ((process (magnus-codex--start-process instance)))
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
         (magnus-codex--open-thread process instance initial-message))))
    process))

(defun magnus-codex--open-thread (process instance initial-message)
  "Start or resume INSTANCE thread on PROCESS, then send INITIAL-MESSAGE."
  (let* ((thread-id (magnus-instance-session-id instance))
         (method (if thread-id "thread/resume" "thread/start"))
         (common `((cwd . ,(magnus-instance-directory instance))
                   (developerInstructions . ,(magnus-codex--instructions instance))))
         (params (if thread-id
                     (cons `(threadId . ,thread-id) common)
                   common)))
    (magnus-codex--request
     process method params
     (lambda (result error)
       (if error
           (progn
             (magnus-codex--insert instance
                                   (format "[%s failed] %s\n" method error)
                                   'error)
             (magnus-instances-update instance :status 'stopped)
             (delete-process process))
         (let ((new-id (alist-get 'id (alist-get 'thread result))))
           (when new-id
             (magnus-instances-update instance :session-id new-id))
           (magnus-instances-update instance :status 'running)
           (magnus-codex--insert instance
                                 (format "[thread %s: %s]\n\n"
                                         (if thread-id "resumed" "started")
                                         (or new-id thread-id))
                                 'font-lock-comment-face)
           (when initial-message
             (magnus-codex-send instance initial-message))))))))

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

(defun magnus-codex--queue-input (process text)
  "Append TEXT to PROCESS's next-turn input queue."
  (process-put process 'magnus-codex-input-queue
               (append (process-get process 'magnus-codex-input-queue)
                       (list text))))

(defun magnus-codex--start-next-queued-turn (process instance)
  "Start INSTANCE's next queued turn through PROCESS when idle."
  (unless (or (gethash (magnus-instance-id instance)
                       magnus-codex--active-turns)
              (process-get process 'magnus-codex-turn-starting))
    (when-let ((queue (process-get process 'magnus-codex-input-queue)))
      (let ((text (car queue))
            (thread-id (magnus-instance-session-id instance)))
        (unless thread-id
          (user-error "Codex thread is not ready yet"))
        (process-put process 'magnus-codex-input-queue (cdr queue))
        (process-put process 'magnus-codex-turn-starting t)
        (magnus-codex--insert instance (format "\nYou: %s\n\n" text)
                              'font-lock-keyword-face)
        (magnus-codex--request
         process "turn/start"
         `((threadId . ,thread-id) (input . ,(magnus-codex--input text)))
         (lambda (result error)
           (process-put process 'magnus-codex-turn-starting nil)
           (if error
               (progn
                 (process-put
                  process 'magnus-codex-input-queue
                  (cons text (process-get process
                                          'magnus-codex-input-queue)))
                 (magnus-codex--insert
                  instance
                  (format "[turn/start failed; message retained] %s\n" error)
                  'error))
             (when-let ((turn-id
                         (magnus-codex--turn-id (alist-get 'turn result))))
               (puthash (magnus-instance-id instance) turn-id
                        magnus-codex--active-turns)))))))))

(defun magnus-codex-send (instance text)
  "Send TEXT to Codex INSTANCE according to the active-turn delivery policy.
By default, text received during an active turn is queued for a new turn so
coordination nudges match Claude/vterm behavior.  See
`magnus-codex-active-turn-delivery' and `magnus-codex-steer'."
  (let* ((process (or (magnus-codex--connection instance)
                      (user-error "Codex instance is not running")))
         (_thread-id (or (magnus-instance-session-id instance)
                         (user-error "Codex thread is not ready yet")))
         (turn-id (gethash (magnus-instance-id instance)
                           magnus-codex--active-turns)))
    (if (and turn-id (eq magnus-codex-active-turn-delivery 'steer))
        (magnus-codex-steer instance text turn-id)
      (magnus-codex--queue-input process text)
      (if (or turn-id (process-get process 'magnus-codex-turn-starting))
          (magnus-codex--insert instance
                                "\n[message queued for next turn]\n"
                                'font-lock-comment-face)
        (magnus-codex--start-next-queued-turn process instance)))))

(defun magnus-codex-steer (instance text &optional expected-turn-id)
  "Steer active Codex INSTANCE turn with TEXT.
EXPECTED-TURN-ID defaults to INSTANCE's current active turn."
  (let ((process (or (magnus-codex--connection instance)
                     (user-error "Codex instance is not running")))
        (thread-id (or (magnus-instance-session-id instance)
                       (user-error "Codex thread is not ready yet")))
        (turn-id (or expected-turn-id
                     (gethash (magnus-instance-id instance)
                              magnus-codex--active-turns)
                     (user-error "Codex has no active turn to steer"))))
    (magnus-codex--insert instance (format "\nYou (steer): %s\n\n" text)
                          'font-lock-keyword-face)
    (magnus-codex--request
     process "turn/steer"
     `((threadId . ,thread-id) (expectedTurnId . ,turn-id)
       (input . ,(magnus-codex--input text)))
     (lambda (_result error)
       (when error
         (magnus-codex--insert instance
                               (format "[turn/steer failed] %s\n" error)
                               'error))))))

(defun magnus-codex-interrupt (instance)
  "Interrupt the active turn for Codex INSTANCE."
  (let ((process (or (magnus-codex--connection instance)
                     (user-error "Codex instance is not running")))
        (thread-id (or (magnus-instance-session-id instance)
                       (user-error "Codex thread is not ready yet")))
        (turn-id (or (gethash (magnus-instance-id instance)
                              magnus-codex--active-turns)
                     (user-error "Codex has no active turn"))))
    (magnus-codex--request
     process "turn/interrupt"
     `((threadId . ,thread-id) (turnId . ,turn-id))
     (lambda (_result error)
       (if error
           (magnus-codex--insert instance
                                 (format "[interrupt failed] %s\n" error)
                                 'error)
         (remhash (magnus-instance-id instance) magnus-codex--active-turns)
         (magnus-codex--insert instance "[turn interrupted]\n"
                               'font-lock-comment-face))))))

(defun magnus-codex-stop (instance &optional force)
  "Stop Codex INSTANCE App Server process.
When FORCE is non-nil, kill it immediately."
  (let ((buffer (magnus-instance-buffer instance)))
    (when-let ((process (magnus-codex--connection instance)))
      (process-put process 'magnus-codex-intentional-stop t)
      (if force (kill-process process) (delete-process process)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (remhash (magnus-instance-id instance) magnus-codex--connections)
  (remhash (magnus-instance-id instance) magnus-codex--active-turns)
  (magnus-instances-update instance :status 'stopped :buffer nil))

(defun magnus-codex-running-p (instance)
  "Return non-nil if Codex INSTANCE has a live App Server connection."
  (and (magnus-codex--connection instance) t))

(defun magnus-codex-switch-to (instance)
  "Switch to Codex INSTANCE, resuming its thread when necessary."
  (unless (magnus-codex-running-p instance)
    (magnus-codex-start instance))
  (pop-to-buffer (magnus-codex--buffer instance)))

(defun magnus-codex-send-message ()
  "Prompt for and send a message from a Codex output buffer."
  (interactive)
  (unless magnus-codex--instance
    (user-error "This is not a Magnus Codex buffer"))
  (let ((text (read-string (format "Message to %s: "
                                   (magnus-instance-name
                                    magnus-codex--instance)))))
    (unless (string-empty-p text)
      (magnus-codex-send magnus-codex--instance text))))

(defun magnus-codex-interrupt-current ()
  "Interrupt the Codex instance represented by the current buffer."
  (interactive)
  (unless magnus-codex--instance
    (user-error "This is not a Magnus Codex buffer"))
  (magnus-codex-interrupt magnus-codex--instance))

(defun magnus-codex-answer-approval ()
  "Answer a pending command or file approval in the current Codex buffer."
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
