;;; magnus-provider-codex.el --- Native Codex TUI provider for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Opt-in Codex support using the native Codex TUI as the single owner of each
;; session.  Magnus launches Codex directly in vterm, delivers coordination
;; messages through that terminal, and discovers new session IDs from Codex's
;; local rollout records so archived agents can be resumed later.

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
  "Path to the Codex executable used for native TUI sessions."
  :type 'string
  :group 'magnus)

(defcustom magnus-codex-extra-developer-instructions nil
  "Optional extra instructions for Magnus-managed Codex sessions."
  :type '(choice (const :tag "None" nil) string)
  :group 'magnus)

(defcustom magnus-codex-tui-ready-delay 1.0
  "Seconds after launch before queued input may be sent to Codex.
This protects the shell command and initial prompt from coordination messages
arriving during startup."
  :type 'number
  :group 'magnus)

(defcustom magnus-codex-session-capture-timeout 30
  "Seconds to wait for a new Codex TUI to record its session ID."
  :type 'number
  :group 'magnus)

(defconst magnus-codex--session-scan-limit (* 1024 1024)
  "Maximum rollout bytes inspected while identifying a new session.")

(defconst magnus-codex--metadata-scan-limit (* 64 1024)
  "Maximum rollout bytes inspected while reading session metadata.")

(defvar magnus-codex--session-file-cache (make-hash-table :test #'equal)
  "Codex session-root/session-ID keys to captured root rollout paths.")

(defvar magnus-codex--trace-file-cache (make-hash-table :test #'equal)
  "Codex session-root/session-ID keys to active trace rollout state.")

(defvar-local magnus-codex--instance nil
  "Magnus instance represented by the current Codex TUI buffer.")

(defun magnus-codex--tui-process (instance)
  "Return INSTANCE's live vterm process, or nil."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (and (buffer-live-p buffer)
         (let ((process (get-buffer-process buffer)))
           (and (process-live-p process) process)))))

(defun magnus-codex--current-process-p (process instance)
  "Return non-nil when PROCESS is INSTANCE's current live terminal."
  (eq process (magnus-codex--tui-process instance)))

(defun magnus-codex--cancel-timer (process property)
  "Cancel PROCESS timer stored under PROPERTY, if any."
  (when-let ((timer (process-get process property)))
    (cancel-timer timer)
    (process-put process property nil)))

(defun magnus-codex--session-root ()
  "Return the root containing Codex rollout records."
  (expand-file-name
   "sessions" (or (getenv "CODEX_HOME") (expand-file-name ".codex" "~"))))

(defun magnus-codex--session-directories ()
  "Return possible recent local and UTC Codex session directories."
  (let ((root (magnus-codex--session-root))
        (now (current-time)))
    (mapcar (lambda (date) (expand-file-name date root))
            (delete-dups
             (cl-loop for time in (list now (time-subtract now 86400))
                      append (list (format-time-string "%Y/%m/%d" time)
                                   (format-time-string "%Y/%m/%d" time t)))))))

(defun magnus-codex--session-files ()
  "Return current Codex rollout files that may belong to a new launch."
  (apply
   #'append
   (mapcar
    (lambda (directory)
      (when (file-directory-p directory)
        (directory-files directory t "\\`rollout-.*\\.jsonl\\'")))
    (magnus-codex--session-directories))))

(defun magnus-codex--read-json-line ()
  "Read the JSON object on the current line, returning nil when incomplete."
  (condition-case nil
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol)
            (json-false nil)
            (json-null nil))
        (json-read-from-string
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position))))
    (error
     ;; Rollout lines may be observed before Codex finishes writing them.
     nil)))

(defun magnus-codex--same-directory-p (first second)
  "Return non-nil when FIRST and SECOND name the same directory."
  (when (and (stringp first) (stringp second))
    (condition-case nil
        (equal (file-truename first) (file-truename second))
      (file-error
       (equal (directory-file-name (expand-file-name first))
              (directory-file-name (expand-file-name second)))))))

(defun magnus-codex--rollout-session-id (file)
  "Return the stable session ID from FILE's first metadata record."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 magnus-codex--metadata-scan-limit)
        (goto-char (point-min))
        (let* ((metadata (magnus-codex--read-json-line))
               (payload (and (equal (alist-get 'type metadata) "session_meta")
                             (alist-get 'payload metadata))))
          (alist-get 'session_id payload)))
    (error
     ;; A newly created continuation may not have a complete first line yet.
     nil)))

(defun magnus-codex--file-modification-time (file)
  "Return FILE's modification time, or the epoch when FILE vanished."
  (condition-case nil
      (or (file-attribute-modification-time (file-attributes file))
          (seconds-to-time 0))
    (file-error (seconds-to-time 0))))

(defun magnus-codex--scan-trace-rollouts (files session-id after)
  "Scan FILES for the newest SESSION-ID rollout modified at or after AFTER.
Return a cons of the matching file and the latest modification time seen."
  (let (matching matching-time (latest-time after))
    (dolist (file files)
      (let ((modified (magnus-codex--file-modification-time file)))
        (when (or (null latest-time) (time-less-p latest-time modified))
          (setq latest-time modified))
        (when (and (or (null after) (not (time-less-p modified after)))
                   (equal (magnus-codex--rollout-session-id file) session-id)
                   (or (null matching-time)
                       (time-less-p matching-time modified)
                       (and (not (time-less-p modified matching-time))
                            (string> file matching))))
          (setq matching file matching-time modified))))
    (cons matching latest-time)))

(defun magnus-codex--session-id-from-file (file directory prompt)
  "Return FILE's top-level Codex session ID for DIRECTORY and PROMPT.
Only the initial metadata and first user message are examined."
  (condition-case nil
      (with-temp-buffer
        ;; The initial prompt is recorded near the beginning of a rollout.
        (insert-file-contents file nil 0 magnus-codex--session-scan-limit)
        (goto-char (point-min))
        (let* ((metadata (magnus-codex--read-json-line))
               (payload (and (equal (alist-get 'type metadata) "session_meta")
                             (alist-get 'payload metadata)))
               (id (alist-get 'id payload))
               (session-id (alist-get 'session_id payload))
               user-message)
          (when (and id
                     (equal id session-id)
                     (null (alist-get 'parent_thread_id payload))
                     (magnus-codex--same-directory-p
                      directory (alist-get 'cwd payload)))
            (forward-line 1)
            (while (and (not user-message) (not (eobp)))
              (let* ((record (magnus-codex--read-json-line))
                     (event (and (equal (alist-get 'type record) "event_msg")
                                 (alist-get 'payload record))))
                (when (equal (alist-get 'type event) "user_message")
                  (setq user-message (alist-get 'message event))))
              (forward-line 1))
            (when (equal user-message prompt)
              (puthash (cons (magnus-codex--session-root) id) file
                       magnus-codex--session-file-cache)
              id))))
    (error
     ;; A concurrently written rollout is retried on the next bounded poll.
     nil)))

(defun magnus-codex--find-session-id (instance prompt files-before)
  "Find INSTANCE's new session for PROMPT, excluding FILES-BEFORE."
  (let ((candidates
         (cl-set-difference (magnus-codex--session-files) files-before
                            :test #'string=)))
    (cl-loop for file in candidates
             thereis (magnus-codex--session-id-from-file
                      file (magnus-instance-directory instance) prompt))))

(defun magnus-codex--finish-session-capture (process)
  "Release session-capture state owned by PROCESS."
  (magnus-codex--cancel-timer process 'magnus-codex-capture-timer)
  (process-put process 'magnus-codex-capture-prompt nil)
  (process-put process 'magnus-codex-files-before nil)
  (process-put process 'magnus-codex-capture-deadline nil))

(defun magnus-codex--poll-session (process)
  "Capture the new Codex session ID owned by PROCESS."
  (let ((instance (process-get process 'magnus-codex-instance)))
    (cond
     ((not (magnus-codex--current-process-p process instance))
      (magnus-codex--finish-session-capture process))
     ((when-let ((session-id
                  (magnus-codex--find-session-id
                   instance
                   (process-get process 'magnus-codex-capture-prompt)
                   (process-get process 'magnus-codex-files-before))))
        (magnus-instances-update instance :session-id session-id)
        (message "Magnus: captured Codex session %s for %s"
                 session-id (magnus-instance-name instance))
        (magnus-codex--finish-session-capture process)
        t))
     ((> (float-time) (process-get process 'magnus-codex-capture-deadline))
      (magnus-codex--finish-session-capture process)
      (message
       "Magnus: could not capture the Codex session for %s; the TUI remains usable"
       (magnus-instance-name instance))))))

(defun magnus-codex--watch-for-session (process instance prompt files-before)
  "Watch PROCESS for INSTANCE's session matching PROMPT.
FILES-BEFORE contains rollout files that predate this launch."
  (process-put process 'magnus-codex-instance instance)
  (process-put process 'magnus-codex-capture-prompt prompt)
  (process-put process 'magnus-codex-files-before files-before)
  (process-put process 'magnus-codex-capture-deadline
               (+ (float-time) magnus-codex-session-capture-timeout))
  (process-put process 'magnus-codex-capture-timer
               (run-at-time 0.5 0.5 #'magnus-codex--poll-session process)))

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

(defun magnus-codex--launch-marker (instance)
  "Return a unique session-correlation marker for INSTANCE."
  (concat "magnus-session-marker:"
          (secure-hash
           'sha256
           (format "%s:%s:%s" (magnus-instance-id instance)
                   (float-time) (random)))))

(defun magnus-codex--onboarding-prompt (instance initial-message marker)
  "Return first-turn onboarding for INSTANCE, INITIAL-MESSAGE, and MARKER."
  (concat
   (magnus-codex--instructions instance)
   (if initial-message
       (concat "\n\nInitial task from the user:\n" initial-message)
     (concat "\n\nNo separate task was supplied. Complete your orientation, "
             "report that you are ready, and then wait for the user."))
   "\n\nInternal Magnus session marker: " marker
   ". Leave this marker in the session history; do not repeat it in replies."))

(defun magnus-codex--tui-command (instance &optional initial-message)
  "Return the shell command for INSTANCE and optional INITIAL-MESSAGE."
  (let ((session-id (magnus-instance-session-id instance)))
    (mapconcat
     #'shell-quote-argument
     (append (list "exec" magnus-codex-executable)
             (when session-id (list "resume"))
             (list "-C" (magnus-instance-directory instance))
             (when session-id (list session-id))
             (when initial-message (list initial-message)))
     " ")))

(defun magnus-codex--setup-tui-sentinel (instance buffer)
  "Track INSTANCE's interactive Codex process in BUFFER."
  (when-let ((process (get-buffer-process buffer)))
    (process-put process 'magnus-codex-instance instance)
    (set-process-sentinel
     process
     (lambda (terminal _event)
       (unless (process-live-p terminal)
         (dolist (property '(magnus-codex-capture-timer
                             magnus-codex-ready-timer
                             magnus-codex-input-retry-timer
                             magnus-codex-input-busy-timer))
           (magnus-codex--cancel-timer terminal property))
         ;; A replaced vterm may report its exit after a new TUI is running.
         (when (and (buffer-live-p (magnus-instance-buffer instance))
                    (eq terminal (get-buffer-process
                                  (magnus-instance-buffer instance))))
           (unless (eq (magnus-instance-status instance) 'purged)
             (magnus-instances-update instance :status 'stopped))
           (when (and (boundp 'magnus-buffer-name)
                      (get-buffer magnus-buffer-name))
             (magnus-status-refresh))))))))

(defun magnus-codex--spawn-tui (instance prompt &optional marker files-before)
  "Launch INSTANCE's native TUI with PROMPT.
When MARKER is non-nil, capture its new session against FILES-BEFORE."
  (let* ((buffer-name (format "*codex:%s*" (magnus-instance-name instance)))
         (default-directory (magnus-instance-directory instance))
         (buffer (magnus-process--create-vterm-buffer buffer-name))
         (command (magnus-codex--tui-command instance prompt))
         (process (get-buffer-process buffer)))
    (unless (process-live-p process)
      (kill-buffer buffer)
      (user-error "Could not start a vterm for Codex instance `%s'"
                  (magnus-instance-name instance)))
    (with-current-buffer buffer
      (setq-local magnus-codex--instance instance))
    (magnus-instances-update instance :buffer buffer :status 'running)
    (magnus-codex--setup-tui-sentinel instance buffer)
    (when marker
      (magnus-codex--watch-for-session process instance prompt files-before))
    ;; Give vterm's login shell one tick before replacing it with Codex.
    (run-with-timer
     0.1 nil
     (lambda ()
       (when (magnus-codex--current-process-p process instance)
         (with-current-buffer buffer
           (vterm-send-string command)))))
    (run-with-timer
     0.5 nil
     (lambda ()
       (when (magnus-codex--current-process-p process instance)
         (with-current-buffer buffer
           (vterm-send-return))
         (process-put
          process 'magnus-codex-ready-timer
          (run-with-timer
           magnus-codex-tui-ready-delay nil
           (lambda ()
             (when (magnus-codex--current-process-p process instance)
               (process-put process 'magnus-codex-ready-timer nil)
               (process-put process 'magnus-codex-ready t)
               (magnus-codex--drain-input-queue process))))))))
    (when (and (boundp 'magnus-buffer-name)
               (get-buffer magnus-buffer-name))
      (magnus-status-refresh))
    buffer))

(defun magnus-codex-start (instance &optional initial-message)
  "Start or resume Codex INSTANCE with optional INITIAL-MESSAGE."
  (when (magnus-codex-running-p instance)
    (user-error "Codex instance `%s' is already running"
                (magnus-instance-name instance)))
  (unless (executable-find magnus-codex-executable)
    (user-error "Cannot find Codex executable: %s" magnus-codex-executable))
  (when-let ((old-buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p old-buffer)
      (kill-buffer old-buffer)))
  (if (magnus-instance-session-id instance)
      (magnus-codex--spawn-tui instance initial-message)
    (let* ((marker (magnus-codex--launch-marker instance))
           (prompt (magnus-codex--onboarding-prompt
                    instance initial-message marker))
           (files-before (magnus-codex--session-files)))
      (magnus-codex--spawn-tui instance prompt marker files-before))))

(defun magnus-codex-send (instance text)
  "Queue TEXT for serialized delivery through INSTANCE's native TUI."
  (let ((process (magnus-codex--tui-process instance)))
    (unless process
      (user-error "Codex instance `%s' is not running"
                  (magnus-instance-name instance)))
    (process-put process 'magnus-codex-input-queue
                 (append (process-get process 'magnus-codex-input-queue)
                         (list text)))
    (magnus-codex--drain-input-queue process)))

(defun magnus-codex--drain-input-queue (process)
  "Submit PROCESS's next queued TUI message when it is safe."
  (let* ((instance (process-get process 'magnus-codex-instance))
         (buffer (and instance (magnus-instance-buffer instance)))
         (queue (process-get process 'magnus-codex-input-queue)))
    (when (and queue
               (process-get process 'magnus-codex-ready)
               (not (process-get process 'magnus-codex-input-busy))
               (magnus-codex--current-process-p process instance))
      (if (eq buffer (window-buffer (selected-window)))
          ;; Do not append to a composer while the user owns this TUI.
          (unless (process-get process 'magnus-codex-input-retry-timer)
            (process-put
             process 'magnus-codex-input-retry-timer
             (run-with-timer
              1.0 nil
              (lambda ()
                (process-put process 'magnus-codex-input-retry-timer nil)
                (magnus-codex--drain-input-queue process)))))
        (process-put process 'magnus-codex-input-queue (cdr queue))
        (process-put process 'magnus-codex-input-busy t)
        ;; Bracketed paste and Return occur in one Emacs event, preventing two
        ;; automated deliveries from interleaving.
        (with-current-buffer buffer
          (vterm-send-string (car queue) t)
          (vterm-send-return))
        (process-put
         process 'magnus-codex-input-busy-timer
         (run-with-timer
          0.1 nil
          (lambda ()
            (process-put process 'magnus-codex-input-busy nil)
            (process-put process 'magnus-codex-input-busy-timer nil)
            (magnus-codex--drain-input-queue process))))))))

(defun magnus-codex-interrupt (instance)
  "Interrupt Codex INSTANCE through its native TUI."
  (let ((buffer (magnus-instance-buffer instance)))
    (unless (magnus-codex--tui-process instance)
      (user-error "Codex instance `%s' is not running"
                  (magnus-instance-name instance)))
    (with-current-buffer buffer
      (vterm-send-key "C-c"))))

(defun magnus-codex--finish-stop (instance buffer force)
  "Finish stopping INSTANCE's captured BUFFER.
FORCE selects immediate process killing."
  (when (buffer-live-p buffer)
    (when-let ((process (get-buffer-process buffer)))
      (set-process-query-on-exit-flag process nil)
      (when (process-live-p process)
        (if force (kill-process process) (delete-process process))))
    (kill-buffer buffer))
  (when (eq buffer (magnus-instance-buffer instance))
    (unless (eq (magnus-instance-status instance) 'purged)
      (magnus-instances-update instance :status 'stopped :buffer nil))))

(defun magnus-codex-stop (instance &optional force)
  "Stop Codex INSTANCE's native TUI.
FORCE kills the terminal immediately; otherwise interrupt before closing it."
  (let* ((buffer (magnus-instance-buffer instance))
         (process (magnus-codex--tui-process instance)))
    ;; Give a very short-lived session one final chance to become resumable.
    (when (and process
               (not (magnus-instance-session-id instance))
               (process-get process 'magnus-codex-capture-prompt))
      (magnus-codex--poll-session process))
    (unless (eq (magnus-instance-status instance) 'purged)
      (magnus-instances-update instance :status 'stopped))
    (if (and (buffer-live-p buffer) (process-live-p process))
        (if force
            (magnus-codex--finish-stop instance buffer t)
          (with-current-buffer buffer
            (vterm-send-key "C-c"))
          (run-with-timer 0.5 nil
                          #'magnus-codex--finish-stop instance buffer nil))
      (magnus-codex--finish-stop instance buffer force))))

(defun magnus-codex-running-p (instance)
  "Return non-nil when Codex INSTANCE has a live native TUI."
  (and (magnus-codex--tui-process instance) t))

(defun magnus-codex-switch-to (instance)
  "Switch to Codex INSTANCE, resuming its TUI when necessary."
  (unless (magnus-codex-running-p instance)
    (magnus-codex-start instance))
  (pop-to-buffer (magnus-instance-buffer instance)))

(defun magnus-codex-trace-file (instance)
  "Return the newest rollout JSONL file for Codex INSTANCE, or nil.
Root rollout filenames contain the stable session ID, but continuation
filenames contain a new per-launch ID.  Continuations are therefore matched
through their first `session_meta.session_id' record."
  (when-let ((session-id (magnus-instance-session-id instance)))
    (let* ((root (magnus-codex--session-root))
           (cache-key (cons root session-id))
           (state (gethash cache-key magnus-codex--trace-file-cache))
           (cached (plist-get state :file))
           (scan-time (plist-get state :scan-time)))
      (unless (and cached (file-exists-p cached))
        (setq cached nil scan-time nil)
        (remhash cache-key magnus-codex--trace-file-cache))
      (when (file-directory-p root)
        (let* ((files
                (condition-case nil
                    (if cached
                        ;; A continuation is always created in a current date
                        ;; directory.  Full traversal is only needed once.
                        (magnus-codex--session-files)
                      (directory-files-recursively
                       root "rollout-.*\\.jsonl\\'"))
                  (file-error nil)))
               (root-pattern
                (concat "-" (regexp-quote session-id) "\\.jsonl\\'"))
               (filename-root
                (and (not cached)
                     (cl-find-if
                      (lambda (file) (string-match-p root-pattern file))
                      files)))
               (fallback (or cached filename-root))
               (threshold
                (or scan-time
                    (and fallback
                         (magnus-codex--file-modification-time fallback))))
               (scan (magnus-codex--scan-trace-rollouts
                      files session-id threshold))
               (matching (car scan))
               (selected (or matching fallback))
               (latest-time (cdr scan)))
          (if selected
              (progn
                (puthash cache-key
                         (list :file selected :scan-time latest-time)
                         magnus-codex--trace-file-cache)
                selected)
            (remhash cache-key magnus-codex--trace-file-cache)
            nil))))))

(defun magnus-codex--canonical-trace-entry (role timestamp text)
  "Build a shared trace entry for ROLE, TIMESTAMP, and TEXT."
  (if (eq role 'user)
      `((type . "user")
        (timestamp . ,timestamp)
        (message . ((content . ,text))))
    `((type . "assistant")
      (timestamp . ,timestamp)
      (message . ((content . [((type . "text") (text . ,text))]))))))

(defun magnus-codex-trace-entry (_instance entry)
  "Normalize one visible Codex rollout ENTRY for the shared trace viewer.
Codex `response_item' records are deliberately ignored: assistant output is
duplicated in `event_msg' records, and raw reasoning content is encrypted."
  (when (equal (alist-get 'type entry) "event_msg")
    (let* ((payload (alist-get 'payload entry))
           (event-type (alist-get 'type payload))
           (text (alist-get 'message payload))
           (timestamp (alist-get 'timestamp entry)))
      (when (and (stringp text) (not (string-empty-p text)))
        (pcase event-type
          ("user_message"
           (magnus-codex--canonical-trace-entry 'user timestamp text))
          ("agent_message"
           (magnus-codex--canonical-trace-entry
            'assistant timestamp text)))))))

(magnus-provider-register
 'codex
 '((start . magnus-codex-start)
   (resume . magnus-codex-start)
   (send . magnus-codex-send)
   (interrupt . magnus-codex-interrupt)
   (stop . magnus-codex-stop)
   (running-p . magnus-codex-running-p)
   (switch-to . magnus-codex-switch-to)
   (trace-file . magnus-codex-trace-file)
   (trace-entry . magnus-codex-trace-entry)))

(provide 'magnus-provider-codex)
;;; magnus-provider-codex.el ends here
