;;; magnus-process.el --- Process management for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module handles spawning and managing Claude Code processes
;; in vterm buffers.

;;; Code:

(require 'cl-lib)
(require 'filenotify)
(require 'subr-x)
(require 'magnus-instances)
(require 'magnus-provider)
(require 'magnus-headless)
(require 'magnus-coord)
(require 'magnus-environment)
(require 'magnus-onboarding)
(require 'magnus-terminal)
(require 'magnus-trace)

(declare-function magnus-instances-create "magnus-instances"
                  (directory name &optional provider))

(declare-function magnus-status-refresh "magnus-status")
(declare-function magnus-project-root "magnus")
(declare-function magnus--agents-index-update "magnus")
(declare-function magnus-claude--fresh-session-id "magnus-provider-claude")
(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl))
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-string "vterm" (string &optional paste-p))

;; Variables defined in magnus.el
(defvar magnus-claude-executable)
(defvar magnus-default-directory)
(defvar magnus-instance-name-generator)
(defvar magnus-buffer-name)
(defvar magnus--summon-context)

(defvar magnus-process-ready-hook nil
  "Hook run when an interactive agent can receive automated input.
Functions are called with the ready `magnus-instance' as their sole argument.")

(defvar magnus-process--claude-session-id-support-cache
  (make-hash-table :test #'equal)
  "Claude executable path to cached `--session-id' support result.")

(defvar magnus-process--legacy-session-launches
  (make-hash-table :test #'equal)
  "Physical project roots with an unresolved legacy Claude launch.")

(defun magnus-process--run-ready-hook (instance)
  "Run `magnus-process-ready-hook' for INSTANCE when its terminal is live."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (and (buffer-live-p buffer)
               (get-buffer-process buffer)
               (process-live-p (get-buffer-process buffer)))
      (run-hook-with-args 'magnus-process-ready-hook instance))))

;;; Process creation

(defun magnus-process--claude-session-id-supported-p ()
  "Return non-nil when the configured Claude CLI supports `--session-id'.
Probe each resolved executable at most once per Emacs session.  A failed probe
conservatively selects the legacy unique-delta capture path."
  (let* ((executable (or (executable-find magnus-claude-executable)
                         magnus-claude-executable))
         (missing (make-symbol "not-probed"))
         (cached (gethash executable
                          magnus-process--claude-session-id-support-cache
                          missing)))
    (if (not (eq cached missing))
        (eq cached 'supported)
      (let ((supported
             (condition-case err
                 (with-temp-buffer
                   (and (eq 0 (call-process executable nil t nil "--help"))
                        (goto-char (point-min))
                        (re-search-forward
                         "\\(?:^\\|[[:space:]]\\)--session-id\\(?:[=[:space:]]\\|$\\)"
                         nil t)))
               (error
                (message "Magnus: could not probe Claude session-ID support: %s"
                         (error-message-string err))
                nil))))
        (puthash executable (if supported 'supported 'unsupported)
                 magnus-process--claude-session-id-support-cache)
        (and supported t)))))

(defun magnus-process--fresh-claude-session-id ()
  "Return a fresh UUID v4 candidate for one interactive Claude launch."
  (require 'magnus-provider-claude)
  (magnus-claude--fresh-session-id))

(defun magnus-process--shell-command (&rest arguments)
  "Return shell command string built from safely quoted ARGUMENTS."
  (mapconcat #'shell-quote-argument arguments " "))

(defun magnus-process--reserve-legacy-session-launch (directory token)
  "Reserve legacy session inference in DIRECTORY for launch TOKEN."
  (let* ((key (magnus-coord--normalized-directory directory))
         (existing (gethash key magnus-process--legacy-session-launches)))
    (when existing
      (user-error
       "A fresh legacy Claude launch is already resolving in %s; wait for its session capture"
       directory))
    (puthash key token magnus-process--legacy-session-launches)
    token))

(defun magnus-process--release-legacy-session-launch (directory token)
  "Release DIRECTORY's legacy launch reservation when owned by TOKEN."
  (when token
    (let ((key (magnus-coord--normalized-directory directory)))
      (when (eq (gethash key magnus-process--legacy-session-launches) token)
        (remhash key magnus-process--legacy-session-launches)))))

(defun magnus-process--discard-created-runtime (instance external)
  "Discard runtime resources acquired while creating INSTANCE.
When EXTERNAL is non-nil, first give the provider a chance to release its
own timers and transport state.  The buffer cleanup is an intentional
fallback: a provider start may fail after attaching a terminal but before its
normal `stop' operation is fully usable."
  ;; Capture the buffer before provider cleanup: a provider may clear the
  ;; instance slot even when its own partial-start cleanup cannot kill the
  ;; terminal it already created.
  (let ((buffer (magnus-instance-buffer instance)))
    (when external
      (condition-case err
          (magnus-provider-call instance 'stop t)
        (error
         (message "Magnus: provider creation rollback failed for %s: %s"
                  (magnus-instance-name instance)
                  (error-message-string err)))))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (ignore-errors (set-process-query-on-exit-flag process nil))
        (when (process-live-p process)
          (ignore-errors (delete-process process))))
      (ignore-errors (kill-buffer buffer))))
  ;; The object is about to leave the registry.  Set these slots directly so
  ;; a failing registry hook cannot strand a live-looking ghost while cleanup
  ;; is still in progress.
  (setf (magnus-instance-buffer instance) nil
        (magnus-instance-status instance) 'stopped))

(defun magnus-process--rollback-coordination
    (directory attempted watcher-existed
               session-start-existed session-start-value)
  "Roll back coordination state acquired for a failed creation.
ATTEMPTED says registration began.  WATCHER-EXISTED and the session-start
arguments snapshot project state from before this creation."
  (when attempted
    ;; Immutable-event registration never owns or rewrites legacy ingress.
    ;; Restore only runtime state this registration changed.  In particular,
    ;; never stop a watcher that was already serving another project agent.
    (condition-case err
        (if session-start-existed
            (puthash directory session-start-value
                     magnus-coord--session-start-times)
          (remhash directory magnus-coord--session-start-times))
      (error
       (message "Magnus: session-state rollback failed for %s: %s"
                directory (error-message-string err))))
    (when (and (not watcher-existed)
               (member directory magnus-coord--watched-dirs))
      (condition-case err
          (magnus-coord-stop-watching directory)
        (error
         (message "Magnus: watcher rollback failed for %s: %s"
                  directory (error-message-string err)))))))

(defun magnus-process--rollback-creation
    (instance directory coordination-attempted runtime-attempted external
              watcher-existed session-start-existed session-start-value)
  "Release resources acquired by one failed INSTANCE creation transaction."
  ;; Reverse acquisition order: runtime, coordination, registry.
  (when runtime-attempted
    (magnus-process--discard-created-runtime instance external))
  (magnus-process--rollback-coordination
   directory coordination-attempted watcher-existed
   session-start-existed session-start-value)
  (when (memq instance (magnus-instances-list))
    (condition-case err
        (magnus-instances-remove instance)
      (error
       ;; `magnus-instances-remove' mutates the registry before running its
       ;; hook, so an error here is diagnostic rather than a ghost instance.
       (message "Magnus: registry rollback hook failed for %s: %s"
                (magnus-instance-name instance)
                (error-message-string err))))))

(defun magnus-process--create-transaction (instance starter)
  "Register INSTANCE and call STARTER as one rollback-safe transaction.
STARTER receives INSTANCE and a non-nil external-provider flag.  On failure,
release only the runtime, coordination state, and registry entry acquired by
this call.  Return INSTANCE after successful startup."
  (let* ((directory (magnus-instance-directory instance))
         (project-key (magnus-coord--normalized-directory directory))
         (missing (make-symbol "missing-session-start"))
         (session-start-value
          (gethash project-key magnus-coord--session-start-times missing))
         (session-start-existed (not (eq session-start-value missing)))
         (watcher-existed
          (and (member project-key magnus-coord--watched-dirs) t))
         coordination-attempted
         runtime-attempted
         external
         committed)
    (unwind-protect
        (progn
          (magnus-instances-add instance)
          (setq coordination-attempted t)
          (magnus-coord-register-agent directory instance)
          (setq external (magnus-provider-external-p instance)
                runtime-attempted t)
          (funcall starter instance external)
          (setq committed t)
          instance)
      (unless committed
        (magnus-process--rollback-creation
         instance project-key coordination-attempted runtime-attempted external
         watcher-existed session-start-existed session-start-value)))))

(defun magnus-process-create (&optional directory name provider initial-message)
  "Create a new agent instance.
DIRECTORY is the working directory.  If nil, prompts for one.
NAME is the instance name.  If nil, auto-generates one.
PROVIDER defaults to `claude', preserving the original vterm behavior.
INITIAL-MESSAGE is passed to external providers when non-nil."
  (interactive)
  (let* ((dir (or directory
                  (magnus-process--get-directory)))
         (instance-name (or name
                            (funcall magnus-instance-name-generator dir)))
         (instance (magnus-instances-create dir instance-name provider)))
    (magnus-process--create-transaction
     instance
     (lambda (candidate external)
       (if external
           (if initial-message
               (magnus-provider-call candidate 'start initial-message)
             (magnus-provider-call candidate 'start))
         (magnus-process--spawn candidate))))))

(defun magnus-process-create-codex (&optional directory name initial-message)
  "Create an opt-in Codex instance in DIRECTORY with NAME.
When INITIAL-MESSAGE is non-nil, include it in the native TUI's first turn."
  (interactive)
  (magnus-process-create directory name 'codex initial-message))

(defun magnus-process--get-directory ()
  "Get directory for new instance, prompting user."
  (let ((default (or magnus-default-directory
                     (magnus-project-root)
                     default-directory)))
    (read-directory-name "Directory: " default nil t)))

(defun magnus-process--spawn (instance)
  "Spawn a Claude Code process for INSTANCE."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude:%s*" name))
         (default-directory directory)
         (explicit-session-p
          (magnus-process--claude-session-id-supported-p))
         (candidate-session-id
          (when explicit-session-p
            (magnus-process--fresh-claude-session-id)))
         (legacy-launch-token
          (unless explicit-session-p
            (cons instance (make-symbol "legacy-claude-launch"))))
         sessions-before
         return-timer
         onboarding-timer
         buffer
         spawned)
    (unwind-protect
        (progn
          (when legacy-launch-token
            (magnus-process--reserve-legacy-session-launch
             directory legacy-launch-token)
            (setq sessions-before
                  (magnus-process--list-sessions directory)))
          ;; Create vterm buffer.
          (setq buffer
                (magnus-terminal-create-buffer
                 buffer-name
                 (magnus-environment-coordination-bindings
                  (magnus-instance-id instance)
                  (magnus-instance-name instance))))
          (magnus-instances-update instance
                                   :buffer buffer
                                   :status 'running)
          ;; Send the claude command.
          (with-current-buffer buffer
            (vterm-send-string
             (if candidate-session-id
                 (magnus-process--shell-command
                  magnus-claude-executable "--session-id"
                  candidate-session-id)
               (magnus-process--shell-command magnus-claude-executable)))
            (setq return-timer
                  (run-with-timer
                   0.1 nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (vterm-send-return)))))))
          ;; Set up process sentinel.
          (magnus-process--setup-sentinel instance buffer)
          ;; Send onboarding after Claude starts.  Capture summon context now;
          ;; its dynamic binding unwinds before the timer fires.
          (let ((summon-ctx magnus--summon-context))
            (setq onboarding-timer
                  (run-with-timer 5 nil #'magnus-process--send-onboarding
                                  instance summon-ctx)))
          ;; Watch for a new session to appear.  This is deliberately last, so
          ;; no later synchronous failure can orphan a successful watcher.
          (magnus-process--watch-for-session
           instance directory sessions-before candidate-session-id buffer
           legacy-launch-token)
          (setq spawned t)
          buffer)
      (unless spawned
        (when (timerp return-timer)
          (cancel-timer return-timer))
        (when (timerp onboarding-timer)
          (cancel-timer onboarding-timer))
        (magnus-process--discard-created-runtime instance nil)
        (magnus-process--release-legacy-session-launch
         directory legacy-launch-token)))))

(defun magnus-process--send-onboarding (instance &optional summon-context)
  "Send onboarding message to INSTANCE.
SUMMON-CONTEXT, if non-nil, is a plist with :sender and :reason
from an agent-initiated summon.
Delays the Return keystroke so the terminal has time to process
the full message text before submitting."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (let ((msg (replace-regexp-in-string
                  "[\n\r]+" " "
                  (magnus-process--onboarding-message instance summon-context))))
        (with-current-buffer buffer
          (vterm-send-string msg))
        ;; Delay Return so the TUI can digest the pasted text
        (run-with-timer 0.5 nil
                        (lambda ()
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (vterm-send-return))
                            (magnus-process--run-ready-hook instance))))))))

(defun magnus-process--agent-memory-path (instance)
  "Return the memory file path for INSTANCE.
Path is <directory>/.claude/agents/<name>/memory.md."
  (magnus-onboarding-memory-path instance))

(defun magnus-process--agent-busy-path (instance)
  "Return the busy signal file path for INSTANCE.
Path is <directory>/.claude/agents/<name>/busy."
  (magnus-onboarding-busy-path instance))

(defun magnus-process--ensure-agent-dir (instance)
  "Ensure the agent directory exists for INSTANCE."
  (let ((dir (file-name-directory (magnus-process--agent-memory-path instance))))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun magnus-process--onboarding-message (instance &optional summon-context)
  "Generate onboarding message for INSTANCE.
SUMMON-CONTEXT, if non-nil, is a plist with :sender and :reason.
Tailors the message for new vs returning agents, and includes
summon context when the agent was brought back by another agent."
  (let* ((prev-session (magnus-instance-previous-session-id instance))
         (prev-trace (when prev-session
                       (magnus-process--session-jsonl-path
                        (magnus-instance-directory instance)
                        prev-session))))
    (magnus-onboarding-prompt instance prev-trace summon-context)))

(defun magnus-process--onboarding-returning
    (name prev-trace summon-context &optional instance)
  "Generate onboarding for a returning agent NAME.
PREV-TRACE is the path to the previous session trace, or nil.
SUMMON-CONTEXT is a plist with :sender and :reason, or nil.
INSTANCE is accepted for internal callers; legacy callers are resolved by NAME."
  (let ((candidate (or instance (magnus-instances-get-by-name name))))
    (if candidate
        (magnus-onboarding-build
         (magnus-instance-id candidate) name
         :returning t
         :previous-trace prev-trace
         :summon-context summon-context)
      (magnus-onboarding-build
       nil name
       :returning t
       :previous-trace prev-trace
       :summon-context summon-context))))

(defun magnus-process--onboarding-new (name summon-context &optional instance)
  "Generate onboarding for a new agent NAME.
SUMMON-CONTEXT is a plist with :sender and :reason, or nil.
INSTANCE is accepted for internal callers; legacy callers are resolved by NAME."
  (let ((candidate (or instance (magnus-instances-get-by-name name))))
    (magnus-onboarding-build
     (if candidate
         (magnus-instance-id candidate)
       nil)
     name
     :returning nil
     :summon-context summon-context)))

(defun magnus-process--list-sessions (directory)
  "List all session IDs for DIRECTORY.
Extracts IDs from .jsonl filenames in the project directory."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    (when (file-directory-p sessions-dir)
      (mapcar (lambda (f) (file-name-sans-extension f))
              (directory-files sessions-dir nil "\\.jsonl$")))))

(defun magnus-process--watch-for-session
    (instance directory sessions-before &optional candidate-session-id
              owner-buffer legacy-launch-token)
  "Watch for a new session to appear for INSTANCE in DIRECTORY.
SESSIONS-BEFORE is the list of sessions that existed before spawning.
When CANDIDATE-SESSION-ID is non-nil, watch only its exact JSONL file.
OWNER-BUFFER identifies the launch allowed to persist the captured session.
LEGACY-LAUNCH-TOKEN serializes old-CLI inference within one physical project.
Uses both filenotify and polling fallback for robustness."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    ;; Ensure directory exists before watching
    (unless (file-directory-p sessions-dir)
      (make-directory sessions-dir t))
    (let* ((descriptor nil)
           (poll-timer nil)
           (cleanup-timer nil)
           (watching nil)
           (detect-fn
            (lambda ()
              (magnus-process--detect-new-session
               instance directory sessions-before
               (list descriptor poll-timer cleanup-timer)
               candidate-session-id
               (or owner-buffer (magnus-instance-buffer instance))
               legacy-launch-token))))
      (unwind-protect
          (progn
            ;; Primary: file-notify watcher.
            (setq descriptor
                  (file-notify-add-watch
                   sessions-dir '(change)
                   (lambda (_event) (funcall detect-fn))))
            ;; Fallback: poll every 5 seconds.
            (setq poll-timer
                  (run-with-timer 5 5 detect-fn))
            ;; Final cleanup after 120 seconds.
            (setq cleanup-timer
                  (run-with-timer
                   120 nil
                   (lambda ()
                     (magnus-process--cleanup-session-watch
                      (list descriptor poll-timer nil))
                     (magnus-process--release-legacy-session-launch
                      directory legacy-launch-token))))
            (setq watching t)
            ;; Close the race where Claude creates its JSONL before the file
            ;; notification watcher is fully installed.
            (funcall detect-fn))
        (unless watching
          (when descriptor
            (ignore-errors (file-notify-rm-watch descriptor)))
          (when (timerp poll-timer)
            (cancel-timer poll-timer))
          (when (timerp cleanup-timer)
            (cancel-timer cleanup-timer))
          (magnus-process--release-legacy-session-launch
           directory legacy-launch-token))))))

(defun magnus-process--cleanup-session-watch (resources)
  "Release file notification and timer RESOURCES for a session watcher."
  (when-let ((descriptor (nth 0 resources)))
    (ignore-errors (file-notify-rm-watch descriptor)))
  (dolist (timer (cdr resources))
    (when (timerp timer)
      (cancel-timer timer))))

(defun magnus-process--session-watch-owner-p
    (instance directory owner-buffer)
  "Return non-nil when OWNER-BUFFER still owns INSTANCE's launch in DIRECTORY."
  (and (buffer-live-p owner-buffer)
       (eq owner-buffer (magnus-instance-buffer instance))
       (string=
        (directory-file-name (expand-file-name directory))
        (directory-file-name
         (expand-file-name (magnus-instance-directory instance))))))

(defun magnus-process--session-candidate-path (directory session-id)
  "Return the expected Claude JSONL path for SESSION-ID in DIRECTORY."
  (expand-file-name
   (format "projects/%s/%s.jsonl"
           (magnus-process--project-hash directory) session-id)
   (expand-file-name ".claude" (getenv "HOME"))))

(defun magnus-process--detect-new-session
    (instance directory sessions-before resources
              &optional candidate-session-id owner-buffer
              legacy-launch-token)
  "Try to detect a new session for INSTANCE in DIRECTORY.
SESSIONS-BEFORE is the pre-spawn session list.  RESOURCES is
a list of (descriptor poll-timer cleanup-timer) to clean up.
CANDIDATE-SESSION-ID selects exact-file capture for a supporting CLI.
OWNER-BUFFER must still be INSTANCE's buffer before any ID is persisted."
  (let ((owner (or owner-buffer (magnus-instance-buffer instance))))
    (cond
     ((not (magnus-process--session-watch-owner-p
            instance directory owner))
      (magnus-process--cleanup-session-watch resources)
      (magnus-process--release-legacy-session-launch
       directory legacy-launch-token)
      'stale)
     ((magnus-instance-session-id instance)
      (magnus-process--cleanup-session-watch resources)
      (magnus-process--release-legacy-session-launch
       directory legacy-launch-token)
      'settled)
     (candidate-session-id
      (when (file-exists-p
             (magnus-process--session-candidate-path
              directory candidate-session-id))
        (unwind-protect
            (progn
              (magnus-instances-update
               instance :session-id candidate-session-id)
              (message "Captured session %s for %s"
                       candidate-session-id
                       (magnus-instance-name instance))
              'captured)
          (magnus-process--cleanup-session-watch resources)
          (magnus-process--release-legacy-session-launch
           directory legacy-launch-token))))
     (t
      (let* ((sessions-after (magnus-process--list-sessions directory))
             (new-sessions
              (cl-set-difference sessions-after sessions-before
                                 :test #'string=)))
        (cond
         ((= 1 (length new-sessions))
          (let ((session-id (car new-sessions)))
            (unwind-protect
                (progn
                  (magnus-instances-update instance :session-id session-id)
                  (message "Captured legacy session %s for %s"
                           session-id (magnus-instance-name instance))
                  'captured)
              (magnus-process--cleanup-session-watch resources)
              (magnus-process--release-legacy-session-launch
               directory legacy-launch-token))))
         ((> (length new-sessions) 1)
          (message
           "Magnus: ambiguous Claude session capture for %s; leaving unresolved"
           (magnus-instance-name instance))
          (magnus-process--cleanup-session-watch resources)
          (magnus-process--release-legacy-session-launch
           directory legacy-launch-token)
          'ambiguous)))))))

(defun magnus-process--create-vterm-buffer (buffer-name)
  "Create a vterm buffer with BUFFER-NAME.
Compatibility wrapper for `magnus-terminal-create-buffer'."
  (magnus-terminal-create-buffer buffer-name))

(defun magnus-process-send-escape ()
  "Send ESC to Claude Code (mapped from \\`keyboard-quit')."
  (interactive)
  (magnus-terminal-send-escape))

(defun magnus-process--setup-keys ()
  "Set up keybindings for Claude Code in the current vterm buffer.
Maps \\`keyboard-quit' to send ESC, since Emacs intercepts the real ESC key."
  (magnus-terminal-setup-keys))

;;; Trace buffer entry point

(declare-function magnus-trace-open "magnus-trace")

(defun magnus-process-trace (instance)
  "Open the trace buffer for INSTANCE showing thinking and messages."
  (when (and (magnus-provider-external-p instance)
             (not (magnus-provider-operation-p instance 'trace-file)))
    (user-error "Provider `%s' does not support thinking traces"
                (magnus-instance-provider instance)))
  (require 'magnus-trace)
  (magnus-trace-open instance))

(defun magnus-process--session-jsonl-path (directory session-id)
  "Get the JSONL file path for SESSION-ID in DIRECTORY."
  (let* ((project-hash (magnus-process--project-hash directory))
         (jsonl-file (expand-file-name
                      (concat "projects/" project-hash "/" session-id ".jsonl")
                      (expand-file-name ".claude" (getenv "HOME")))))
    (when (file-exists-p jsonl-file)
      jsonl-file)))

(defun magnus-process--setup-sentinel (instance buffer)
  "Set up process monitoring for INSTANCE in BUFFER."
  (when-let ((process (get-buffer-process buffer)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (unless (process-live-p proc)
         ;; Don't overwrite 'purged status — archive sets it intentionally
         (unless (eq (magnus-instance-status instance) 'purged)
           (magnus-instances-update instance :status 'stopped))
         (when (get-buffer magnus-buffer-name)
           (magnus-status-refresh)))))))

;;; Process control

(defun magnus-process-kill (instance &optional force)
  "Kill the agent process for INSTANCE.
If FORCE is non-nil, forcefully terminate."
  (if (magnus-provider-external-p instance)
      (magnus-provider-call instance 'stop force)
    (when-let ((buffer (magnus-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (let ((process (get-buffer-process buffer)))
          (when (and process (process-live-p process))
            (if force
                (kill-process process)
              ;; Graceful exit: SIGINT for headless, C-c for vterm
              (if (magnus-process--headless-p instance)
                  (interrupt-process process)
                (with-current-buffer buffer
                  (vterm-send-key "C-c"))))))
        ;; Give process time to exit, then kill buffer
        (run-with-timer
         1 nil
         (lambda ()
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))))
    (magnus-instances-update instance :status 'stopped :buffer nil)))

(defun magnus-process-archive (instance)
  "Archive INSTANCE: stop its process but keep it in the registry.
The session ID is preserved so the agent can be resurrected later
  via `magnus-process-resurrect-purged'."
  (if (magnus-provider-external-p instance)
      (progn
        (magnus-provider-call instance 'stop)
        (magnus-instances-update instance
                                 :status 'purged
                                 :buffer nil
                                 :purged-at (float-time))
        ;; Lifecycle-aware coordination projection must see the archived
        ;; status before the last owner releases the project runtime.
        (magnus-coord-unregister-agent
         (magnus-instance-directory instance) instance))
    ;; Graceful stop (same as kill, but we keep the instance)
    (when-let ((buffer (magnus-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (let ((process (get-buffer-process buffer)))
          (when (and process (process-live-p process))
            (if (magnus-process--headless-p instance)
                (interrupt-process process)
              (with-current-buffer buffer
                (vterm-send-key "C-c")))))
        (run-with-timer
         1 nil
         (lambda ()
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))))
    (magnus-instances-update instance
                             :status 'purged
                             :buffer nil
                             :purged-at (float-time))
    ;; Keep projection ordering identical across local and external providers.
    (magnus-coord-unregister-agent
     (magnus-instance-directory instance) instance))
  ;; Index expertise tags asynchronously for every provider.
  (magnus--agents-index-update instance))

(defun magnus-process-resurrect-purged (instance)
  "Resurrect a purged INSTANCE by resuming its provider session.
If the instructions file was updated since the agent was archived,
nudge the agent to re-read it."
  (let* ((session-id (magnus-instance-session-id instance))
         (directory (magnus-instance-directory instance))
         (external (magnus-provider-external-p instance))
         (instructions-file (magnus-coord-instructions-path directory))
         (instructions-stale (or (not (file-exists-p instructions-file))
                                 (magnus-coord--instructions-stale-p
                                  instructions-file)))
         (coord-snapshot
          (magnus-process--coord-ownership-snapshot directory))
         (original-status (magnus-instance-status instance))
         (original-buffer (magnus-instance-buffer instance))
         (original-purged-at (magnus-instance-purged-at instance))
         coordination-attempted
         runtime-attempted
         committed)
    (unless session-id
      (user-error "No session ID for '%s' — cannot resume"
                  (magnus-instance-name instance)))
    (unwind-protect
        (progn
          ;; Acquire project ownership while the instance is still hidden from
          ;; lifecycle-aware projection, then expose it immediately before its
          ;; provider runtime starts.
          (setq coordination-attempted t)
          (magnus-coord-register-agent directory instance)
          (magnus-instances-update instance :status 'running :purged-at nil)
          (setq runtime-attempted t)
          (if external
              (magnus-provider-call instance 'resume)
            (magnus-process--spawn-with-session instance session-id))
          (setq committed t))
      (unless committed
        (when runtime-attempted
          (magnus-process--discard-created-runtime instance external))
        (when coordination-attempted
          (condition-case err
              (magnus-process--restore-coord-ownership
               directory coord-snapshot)
            (error
             (message "Magnus: resurrection ownership rollback failed for %s: %s"
                      directory (error-message-string err)))))
        (condition-case err
            (magnus-instances-update
             instance
             :status original-status
             :buffer (and (buffer-live-p original-buffer) original-buffer)
             :session-id session-id
             :purged-at original-purged-at)
          (error
           ;; As with directory moves, notification hooks run after mutation.
           ;; Complete rollback directly without replacing the startup error.
           (setf (magnus-instance-status instance) original-status
                 (magnus-instance-buffer instance)
                 (and (buffer-live-p original-buffer) original-buffer)
                 (magnus-instance-session-id instance) session-id
                 (magnus-instance-purged-at instance) original-purged-at)
           (message "Magnus: resurrection registry rollback failed for %s: %s"
                    (magnus-instance-name instance)
                    (error-message-string err))))))
    ;; If instructions were outdated, nudge agent to re-read after spawn.
    ;; External providers queue this until their resumed TUI is ready.
    (when instructions-stale
      (run-with-timer
       5 nil
       (lambda ()
         (when (magnus-process-running-p instance)
           (magnus-coord-nudge-agent
            instance
            (format
             "The coordination protocol has been updated. Please re-read %s for the latest event schema and engineering-journal guidance."
             magnus-coord-instructions-file)
            "Magnus")))))
    instance))

(defun magnus-process-suspend (instance)
  "Suspend the Claude Code process for INSTANCE.
Sends SIGTSTP to pause the process.  Use `magnus-process-resume' to continue."
  (when (magnus-provider-external-p instance)
    (user-error "Suspend is not supported by the `%s' provider"
                (magnus-instance-provider instance)))
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (when (process-live-p process)
          (signal-process process 'SIGTSTP)
          (magnus-instances-update instance :status 'suspended)
          (when (get-buffer magnus-buffer-name)
            (magnus-status-refresh))
          (message "Suspended %s" (magnus-instance-name instance)))))))

(defun magnus-process-resume (instance)
  "Resume a suspended Claude Code process for INSTANCE.
Sends SIGCONT to continue the process."
  (when (magnus-provider-external-p instance)
    (user-error "Process resume is not supported by the `%s' provider"
                (magnus-instance-provider instance)))
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (signal-process process 'SIGCONT)
        (magnus-instances-update instance :status 'running)
        (when (get-buffer magnus-buffer-name)
          (magnus-status-refresh))
        (message "Resumed %s" (magnus-instance-name instance))))))

(defun magnus-process-suspended-p (instance)
  "Return non-nil if INSTANCE is suspended."
  (eq (magnus-instance-status instance) 'suspended))

(defun magnus-process--coord-ownership-snapshot (directory)
  "Snapshot Magnus coordination ownership for DIRECTORY.
The returned plist is intentionally limited to process-owned runtime state;
durable writer directories and instructions are safe provisioning artifacts."
  (let* ((directory (magnus-coord--normalized-directory directory))
         (missing (make-symbol "missing-session-start"))
         (session-start
          (gethash directory magnus-coord--session-start-times missing)))
    (list :watching (and (member directory magnus-coord--watched-dirs) t)
          :session-start-present (not (eq session-start missing))
          :session-start session-start)))

(defun magnus-process--restore-coord-ownership (directory snapshot)
  "Restore DIRECTORY's coordination runtime ownership from SNAPSHOT.
This removes only a watcher acquired by the failed transaction and preserves
pre-existing ownership shared with another agent."
  (setq directory (magnus-coord--normalized-directory directory))
  (if (plist-get snapshot :session-start-present)
      (puthash directory (plist-get snapshot :session-start)
               magnus-coord--session-start-times)
    (remhash directory magnus-coord--session-start-times))
  (when (and (not (plist-get snapshot :watching))
             (member directory magnus-coord--watched-dirs))
    (magnus-coord-stop-watching directory)))

(defun magnus-process--stop-local-for-chdir (instance)
  "Synchronously stop INSTANCE's local terminal before a directory move."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (set-process-query-on-exit-flag process nil)
        (when (process-live-p process)
          (kill-process process)))
      (kill-buffer buffer)))
  (magnus-instances-update instance :status 'stopped :buffer nil))

(defun magnus-process--restore-chdir-instance
    (instance directory session-id previous-session-id purged-at)
  "Restore a failed move of INSTANCE to DIRECTORY as a stopped agent."
  (condition-case err
      (magnus-instances-update
       instance
       :directory directory
       :status 'stopped
       :buffer nil
       :session-id session-id
       :previous-session-id previous-session-id
       :purged-at purged-at)
    (error
     ;; `magnus-instances-update' mutates before running its notification hook.
     ;; Finish the restoration directly so a hook failure cannot replace the
     ;; startup error or leave a cross-project ghost behind.
     (setf (magnus-instance-directory instance) directory
           (magnus-instance-status instance) 'stopped
           (magnus-instance-buffer instance) nil
           (magnus-instance-session-id instance) session-id
           (magnus-instance-previous-session-id instance) previous-session-id
           (magnus-instance-purged-at instance) purged-at)
     (message "Magnus: instance rollback hook failed for %s: %s"
              (magnus-instance-name instance)
              (error-message-string err))
     instance)))

(defun magnus-process-chdir (instance directory)
  "Change INSTANCE's working directory to DIRECTORY.
Stop the old runtime, provision coordination and onboarding in the new root,
then synchronously start a fresh provider session.  The old root remains
owned until the new runtime is live.  If setup fails, restore the instance and
both roots' coordination ownership coherently; the old runtime remains
stopped so callers can retry or explicitly resurrect it."
  (let* ((old-dir (directory-file-name
                   (expand-file-name (magnus-instance-directory instance))))
         (new-dir (directory-file-name (expand-file-name directory)))
         (old-project-key (magnus-coord--normalized-directory old-dir))
         (new-project-key (magnus-coord--normalized-directory new-dir))
         (name (magnus-instance-name instance))
         (external (magnus-provider-external-p instance))
         (original-session-id (magnus-instance-session-id instance))
         (original-previous-session-id
          (magnus-instance-previous-session-id instance))
         (original-purged-at (magnus-instance-purged-at instance)))
    (if (string= old-project-key new-project-key)
        (progn
          (message "%s is already in %s" name new-dir)
          instance)
      ;; Stop first while INSTANCE still names the old root.  Provider cleanup
      ;; and process sentinels must never observe a partially moved identity.
      (if external
          (magnus-provider-call instance 'stop t)
        (magnus-process--stop-local-for-chdir instance))
      (let* ((old-session-id
              (or (magnus-instance-session-id instance) original-session-id))
             (new-coord-snapshot
              (magnus-process--coord-ownership-snapshot new-project-key))
             new-coord-attempted
             new-runtime-attempted
             old-release-attempted
             committed)
        (unwind-protect
            (progn
              ;; Updating the durable instance first makes every path derived
              ;; during provisioning point at the destination project.
              (magnus-instances-update
               instance
               :directory new-dir
               :status 'stopped
               :buffer nil
               :session-id nil
               :previous-session-id old-session-id
               :purged-at nil)
              (magnus-process--ensure-agent-dir instance)
              (setq new-coord-attempted t)
              ;; Registration creates current onboarding instructions, the
              ;; writer store, and the destination watcher before startup.
              (magnus-coord-register-agent new-dir instance)
              (setq new-runtime-attempted t)
              (if external
                  (magnus-provider-call instance 'start)
                ;; Synchronous spawn is deliberate: setup failures belong to
                ;; this transaction rather than an unreportable timer callback.
                (magnus-process--spawn instance))
              ;; There is now no observation gap: destination ownership and
              ;; its runtime are live before the source watcher is released.
              (setq old-release-attempted t)
              (magnus-coord-unregister-agent old-dir instance)
              (setq committed t)
              (message "Moved %s to %s (fresh %s thread)"
                       name new-dir
                       (if external
                           (magnus-instance-provider instance)
                         "Claude"))
              instance)
          (unless committed
            ;; Reverse destination acquisition before restoring the durable
            ;; instance.  Provider starts can fail after attaching a buffer.
            (when new-runtime-attempted
              (magnus-process--discard-created-runtime instance external))
            (when new-coord-attempted
              (condition-case err
                  (magnus-process--restore-coord-ownership
                   new-project-key new-coord-snapshot)
                (error
                 (message "Magnus: destination ownership rollback failed for %s: %s"
                          new-dir (error-message-string err)))))
            (magnus-process--restore-chdir-instance
             instance old-dir old-session-id original-previous-session-id
             original-purged-at)
            ;; A failing source release may have stopped its watcher before
            ;; signaling.  Re-register only in that ambiguous case; otherwise
            ;; the source ownership was deliberately kept throughout setup.
            (when old-release-attempted
              (condition-case err
                  (magnus-coord-register-agent old-dir instance)
                (error
                 (message "Magnus: source ownership rollback failed for %s: %s"
                          old-dir (error-message-string err)))))))))))

(defun magnus-process--project-hash (directory)
  "Convert DIRECTORY to Claude's project hash format.
Replaces slashes, spaces, tildes, and underscores with hyphens."
  (let ((path (directory-file-name (expand-file-name directory))))
    (replace-regexp-in-string "[/ ~_]+" "-" path)))

(defun magnus-process--spawn-with-session (instance &optional session-id)
  "Spawn a Claude Code process for INSTANCE, optionally resuming SESSION-ID."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude:%s*" name))
         (default-directory directory))
    ;; Create vterm buffer
    (let ((buffer
           (magnus-terminal-create-buffer
            buffer-name
            (magnus-environment-coordination-bindings
             (magnus-instance-id instance)
             (magnus-instance-name instance)))))
      (magnus-instances-update instance
                               :buffer buffer
                               :status 'running)
      ;; Send the claude command with optional --resume
      (with-current-buffer buffer
        (if session-id
            (vterm-send-string
             (magnus-process--shell-command
              magnus-claude-executable "--resume" session-id))
          (vterm-send-string
           (magnus-process--shell-command magnus-claude-executable)))
        (run-with-timer 0.1 nil
                        (lambda ()
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (vterm-send-return))))))
      ;; Set up process sentinel
      (magnus-process--setup-sentinel instance buffer)
      ;; Resumed Claude sessions have no onboarding callback to signal that
      ;; their composer is ready.  Use the same conservative startup window as
      ;; initial onboarding before releasing durable queued deliveries.
      (run-with-timer 5 nil #'magnus-process--run-ready-hook instance)
      buffer)))

;;; Instance interaction

(defun magnus-process-switch-to (instance)
  "Switch to the buffer for INSTANCE.
If the buffer is nil (e.g. after Emacs restart), resume the session
if a session ID exists, or spawn a fresh process."
  (if (magnus-provider-external-p instance)
      (magnus-provider-call instance 'switch-to)
    (let ((buffer (magnus-instance-buffer instance)))
      (cond
       ;; Buffer exists and is live — switch to it
       ((and buffer (buffer-live-p buffer))
        (switch-to-buffer buffer))
       ;; Buffer is dead or nil — need to (re)spawn
       (t
        (if-let ((session-id (magnus-instance-session-id instance)))
            ;; Has a session — resume it
            (progn
              (magnus-process--spawn-with-session instance session-id)
              (switch-to-buffer (magnus-instance-buffer instance)))
          ;; No session — spawn fresh
          (magnus-process--spawn instance)
          (switch-to-buffer (magnus-instance-buffer instance))))))))

(defun magnus-process-running-p (instance)
  "Return non-nil if INSTANCE has a running process."
  (if (magnus-provider-external-p instance)
      (magnus-provider-call instance 'running-p)
    (when-let ((buffer (magnus-instance-buffer instance)))
      (and (buffer-live-p buffer)
           (get-buffer-process buffer)
           (process-live-p (get-buffer-process buffer))))))

;;; Reconnection

(defun magnus-process-reconnect (instance)
  "Try to reconnect INSTANCE to an existing buffer/process."
  (if (magnus-provider-external-p instance)
      (progn
        ;; A provider's terminal cannot survive Emacs.  Preserve its session ID;
        ;; visiting the instance will resume it in a fresh display buffer.
        (when (eq (magnus-instance-status instance) 'running)
          (magnus-instances-update instance :status 'stopped :buffer nil))
        nil)
    (let* ((name (magnus-instance-name instance))
           (buffer-name (format "*claude:%s*" name))
           (buffer (get-buffer buffer-name)))
      (when (and buffer (buffer-live-p buffer))
        (magnus-instances-update instance
                                 :buffer buffer
                                 :status (if (get-buffer-process buffer)
                                             'running
                                           'stopped))))))

;;; Headless mode — fire-and-forget agents

(defvar magnus-headless-allowed-tools)

(defvar-local magnus-process--headless-instance nil
  "The instance associated with this headless buffer.")

(define-derived-mode magnus-process-headless-mode special-mode "Headless"
  "Major mode for headless Claude Code output buffers."
  :group 'magnus
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(let ((map magnus-process-headless-mode-map))
  (define-key map (kbd "q") #'quit-window))

(defun magnus-process--headless-p (instance)
  "Return non-nil if INSTANCE is a headless (non-interactive) agent."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (derived-mode-p 'magnus-process-headless-mode)))))

(defvar magnus--creation-task)

(defun magnus-process-create-headless (prompt &optional directory name)
  "Create a headless Claude Code instance with PROMPT.
DIRECTORY is the working directory.  NAME is optional.
Binds the prompt as `magnus--creation-task' for smart resurrection.
Returns the new instance."
  (interactive "sTask prompt: ")
  (let* ((dir (or directory (magnus-process--get-directory)))
         (magnus--creation-task prompt)
         (instance-name (or name
                            (concat "headless-"
                                    (funcall magnus-instance-name-generator dir))))
         (instance (magnus-instances-create dir instance-name)))
    (magnus-process--create-transaction
     instance
     (lambda (candidate _external)
       (magnus-process--spawn-headless candidate prompt)))))

(defun magnus-process--spawn-headless (instance prompt)
  "Spawn a headless Claude Code process for INSTANCE with PROMPT."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude-headless:%s*" name))
         (full-prompt (magnus-process--headless-prompt instance prompt))
         (buffer (generate-new-buffer buffer-name))
         started)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (magnus-process-headless-mode)
            (setq magnus-process--headless-instance instance)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize (format "Headless agent: %s\n" name)
                                  'face 'font-lock-keyword-face))
              (insert (propertize (format "Directory: %s\n" directory)
                                  'face 'font-lock-comment-face))
              (insert (propertize (format "Prompt: %s\n\n" prompt)
                                  'face 'font-lock-comment-face))
              (insert (propertize "--- Output ---\n\n"
                                  'face 'magnus-trace-separator))))
          (magnus-instances-update instance :buffer buffer :status 'running)
          (magnus-headless-start
           'claude
           (list :purpose 'agent
                 :directory directory
                 :prompt full-prompt
                 :allowed-tools magnus-headless-allowed-tools
                 :name name
                 :environment-bindings
                 (magnus-environment-coordination-bindings
                  (magnus-instance-id instance) name)
                 :buffer buffer)
           (list
            :on-event
            (lambda (process event)
              (magnus-process--headless-render-event
               instance process event))
            :on-complete
            (lambda (process result)
              (magnus-process--headless-complete
               instance process result))))
          (setq started t)
          buffer)
      (unless started
        (when (buffer-live-p buffer)
          (when-let ((process (get-buffer-process buffer)))
            (ignore-errors (set-process-query-on-exit-flag process nil))
            (when (process-live-p process)
              (ignore-errors (delete-process process))))
          (ignore-errors (kill-buffer buffer)))))))

(defun magnus-process--headless-prompt (instance prompt)
  "Build the full headless prompt for INSTANCE wrapping user PROMPT."
  (magnus-onboarding-task-prompt instance prompt))

(defun magnus-process--headless-render-event (instance process event)
  "Render canonical headless EVENT from PROCESS for INSTANCE."
  (ignore instance)
  (when-let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (pcase (plist-get event :type)
            ("assistant"
             (when-let ((text (plist-get event :text)))
               (insert text)))
            ("result"
             (insert (propertize "\n--- Task Complete ---\n"
                                 'face 'magnus-trace-separator))
             (when-let ((cost (plist-get event :cost-usd)))
               (insert (format "Cost: $%.4f\n" cost))))))))))

(defun magnus-process--headless-complete (instance process result)
  "Publish RESULT for headless INSTANCE completed by PROCESS."
  (let* ((process-status (plist-get result :status))
         (event-str (string-trim
                     (or (plist-get result :process-event)
                         (symbol-name (or process-status 'stopped)))))
         (new-status
          (cond
           ((plist-get result :success-p) 'finished)
           ((eq process-status 'exit) 'errored)
           (t 'stopped))))
    ;; Archiving intentionally wins a race with a late process sentinel.
    (unless (eq (magnus-instance-status instance) 'purged)
      (magnus-instances-update instance :status new-status))
    (pcase new-status
      ('finished
       (message "Magnus: headless agent '%s' completed"
                (magnus-instance-name instance)))
      ('errored
       (message "Magnus: headless agent '%s' failed: %s"
                (magnus-instance-name instance) event-str)))
    (when-let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (propertize (format "\n--- Process %s ---\n" event-str)
                                'face 'magnus-trace-separator))))))
    (when (and (boundp 'magnus-buffer-name)
               (get-buffer magnus-buffer-name))
      (magnus-status-refresh))))

(provide 'magnus-process)
;;; magnus-process.el ends here
