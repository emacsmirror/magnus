;;; magnus-coord.el --- Agent coordination for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module connects Magnus's append-only, per-writer coordination event
;; store to agent delivery and the status UI.  `.magnus-coord/current.md' is a
;; generated human view.  The historical `.magnus-coord.md' remains a
;; read-compatible legacy ingress and is never used as the event projection.
;; Agents use coordination to:
;; - Announce what they're working on
;; - Communicate with other agents
;; - Record decisions and agreements
;; - Avoid stepping on each other's work

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magnus-instances)
(require 'magnus-provider)
(require 'magnus-coord-runtime)
(require 'magnus-coord-state)
(require 'magnus-coord-store)
(require 'magnus-background)

(declare-function vterm-send-string "vterm")
(declare-function project-root "project")
(declare-function vterm-send-return "vterm")
(declare-function magnus-review-list "magnus-review" ())
(declare-function magnus-review-lifecycle "magnus-review" (review))
(declare-function magnus-review-project-root "magnus-review" (review))
(declare-function magnus-review-pending-checkpoint-request
                  "magnus-review" (review))

(defvar magnus-claude-executable)
(defvar magnus-headless-model)
(declare-function magnus--strip-thinking-markers "magnus")

;;; Vterm buffer activity tracking

(defvar magnus-coord--buffer-ticks (make-hash-table :test 'equal)
  "Hash: instance-id -> (TICK . FLOAT-TIME).
TICK is the `buffer-modified-tick' last observed, FLOAT-TIME is when it changed.")

(defcustom magnus-coord-quiescence-threshold 30
  "Seconds of vterm buffer inactivity before an agent is considered idle.
Only idle agents receive periodic nudges."
  :type 'integer
  :group 'magnus)

(defun magnus-coord--update-buffer-ticks ()
  "Update buffer activity timestamps for all running instances."
  (dolist (instance (magnus-instances-list))
    (when (eq (magnus-instance-status instance) 'running)
      (let* ((id (magnus-instance-id instance))
             (buffer (magnus-instance-buffer instance))
             (prev (gethash id magnus-coord--buffer-ticks))
             (prev-tick (car prev)))
        (when (and buffer (buffer-live-p buffer))
          (let ((tick (buffer-modified-tick buffer)))
            (if (and prev-tick (= tick prev-tick))
                ;; Tick unchanged — keep existing timestamp
                nil
              ;; Tick changed — record new tick + current time
              (puthash id (cons tick (float-time))
                       magnus-coord--buffer-ticks))))))))

(defun magnus-coord-agent-quiescent-p (instance)
  "Return non-nil if INSTANCE's vterm buffer has been quiet.
Quiet means no output for `magnus-coord-quiescence-threshold' seconds."
  (let* ((id (magnus-instance-id instance))
         (entry (gethash id magnus-coord--buffer-ticks))
         (last-change (cdr entry)))
    (or (null last-change)
        (> (- (float-time) last-change) magnus-coord-quiescence-threshold))))

;;; Customization

(defcustom magnus-coord-file ".magnus-coord.md"
  "Name of the legacy coordination ingress in project directories.
New Magnus agents publish immutable events below `.magnus-coord/writers/'."
  :type 'string
  :group 'magnus)

(defcustom magnus-coord-instructions-file ".claude/magnus-instructions.md"
  "Path to the instructions file for agents (relative to project)."
  :type 'string
  :group 'magnus)

(defconst magnus-coord--instructions-version 4
  "Version of the instructions template.
Bump this when the template content changes.  Files with an older
version marker are automatically regenerated.")

(defcustom magnus-coord-mention-notify t
  "If non-nil, automatically notify agents when they are @mentioned."
  :type 'boolean
  :group 'magnus)

(defcustom magnus-coord-skill-file ".claude/skills/coordinate/SKILL.md"
  "Path to the coordination skill file (relative to project)."
  :type 'string
  :group 'magnus)

(defcustom magnus-coord-reminder-interval 600
  "Seconds between coordination file reminders to agents.
Set to nil to disable.  Default is 600 (10 minutes)."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'magnus)

(defcustom magnus-coord-nudge-debounce 300
  "Minimum seconds between system nudges to the same agent.
Prevents rapid-fire reminders when multiple nudge paths fire
close together.  Only applies to Magnus system messages, not
agent-to-agent DMs or @mentions."
  :type 'integer
  :group 'magnus)

(defcustom magnus-coord-log-max-entries 25
  "Maximum number of log entries to keep in the coordination file.
Older entries are trimmed automatically.  Set to nil to disable."
  :type '(choice (integer :tag "Entries")
                 (const :tag "Unlimited" nil))
  :group 'magnus)

(defcustom magnus-coord-tidy-size-threshold 20480
  "File size in bytes above which an agent is asked to tidy the coord file.
When the coordination file exceeds this size, a random quiescent agent
is asked to consolidate the Discoveries and Decisions sections.
Set to nil to disable."
  :type '(choice (integer :tag "Bytes")
                 (const :tag "Disabled" nil))
  :group 'magnus)

(put 'magnus-coord-log-max-entries 'safe-local-variable
     (lambda (v) (or (null v) (integerp v))))
(put 'magnus-coord-tidy-size-threshold 'safe-local-variable
     (lambda (v) (or (null v) (integerp v))))

(defun magnus-coord--dir-local-value (var dir)
  "Return VAR's dir-local value in DIR, falling back to its global value.
Timers run outside any buffer visiting DIR, so dir-local settings in
DIR's .dir-locals.el are invisible to them unless resolved explicitly."
  (with-temp-buffer
    (setq default-directory (file-name-as-directory (expand-file-name dir)))
    (let ((enable-local-variables :safe))
      (hack-dir-local-variables-non-file-buffer))
    (symbol-value var)))

(defcustom magnus-coord-idle-threshold 300
  "Seconds of inactivity before telling agents to sleep.
When the user is idle for this long, a sleep message is sent to
all running agents and periodic nudges are suppressed.  When the
user returns, a wake-up message is sent.  Set to nil to disable."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'magnus)

(defcustom magnus-coord-context-warn-threshold 0.80
  "Context utilization ratio at which to warn an agent.
When an agent's context usage exceeds this fraction of the
maximum window, a memory consolidation warning is sent.
Set to nil to disable.  Default is 0.80 (80%)."
  :type '(choice (float :tag "Ratio (0.0-1.0)")
                 (const :tag "Disabled" nil))
  :group 'magnus)

(defcustom magnus-coord-context-max-tokens 200000
  "Maximum context window size in tokens.
Used to calculate context utilization percentage."
  :type 'integer
  :group 'magnus)

;;; Atomic file writes

(defun magnus-coord--write-file-atomic (file)
  "Write the current buffer to FILE atomically.
Writes to a temporary file in the same directory, then renames.
This prevents partial reads when agents write concurrently."
  (let ((tmp (make-temp-file
              (expand-file-name ".magnus-coord-tmp" (file-name-directory file)))))
    (write-region (point-min) (point-max) tmp nil 'quiet)
    (rename-file tmp file t)))

;;; Sending messages to agents

(defvar magnus-coord--last-nudge (make-hash-table :test 'equal)
  "Hash of instance-id → timestamp of last Magnus system nudge.")

(defun magnus-coord--log-undelivered-nudge (instance text reason)
  "Record in Emacs's message log that TEXT missed INSTANCE for REASON."
  (let* ((name (magnus-instance-name instance))
         (safe-text (replace-regexp-in-string
                     "@" "(at) "
                     (replace-regexp-in-string "[\n\r]+" " " text)))
         (entry (format "Undelivered nudge to %s (%s): %s"
                        name reason safe-text)))
    ;; `message' writes *Messages* without making Magnus another writer of the
    ;; shared legacy Markdown ingress.  That file cannot provide multiwriter
    ;; safety and is retained only for compatibility with old agents.
    (message "Magnus: %s" entry)
    nil))

(defun magnus-coord--instance-running-p (instance)
  "Return non-nil when INSTANCE can currently receive a nudge.
This intentionally avoids requiring `magnus-process', which itself requires
this module and would create a load-order cycle for standalone callers."
  (if (magnus-provider-external-p instance)
      (magnus-provider-call instance 'running-p)
    (when-let ((buffer (magnus-instance-buffer instance)))
      (and (buffer-live-p buffer)
           (get-buffer-process buffer)
           (process-live-p (get-buffer-process buffer))))))

(defun magnus-coord-nudge-agent (instance message &optional source)
  "Nudge INSTANCE by sending MESSAGE through its provider.
When SOURCE is non-nil, prepend \"[From SOURCE]:\" to distinguish
system messages from user-typed input.  System messages from Magnus
are debounced per `magnus-coord-nudge-debounce'."
  (catch 'magnus-debounced
    (let ((id (magnus-instance-id instance)))
      (when (and (string= source "Magnus")
                 magnus-coord-nudge-debounce)
        (let ((last (gethash id magnus-coord--last-nudge 0)))
          (when (< (- (float-time) last) magnus-coord-nudge-debounce)
            (throw 'magnus-debounced nil))))
      (let ((text (if source
                      (format "[From %s]: %s" source message)
                    message)))
        (condition-case err
            (if (not (magnus-coord--instance-running-p instance))
                (magnus-coord--log-undelivered-nudge
                 instance text "not running")
              (if (magnus-provider-external-p instance)
                  (magnus-provider-call instance 'send text)
                (let ((buffer (magnus-instance-buffer instance)))
                  (unless (buffer-live-p buffer)
                    (error "agent buffer is not live"))
                  (with-current-buffer buffer
                    (vterm-send-string text)
                    (run-with-timer
                     0.1 nil
                     (lambda ()
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (vterm-send-return))))))))
              (when (string= source "Magnus")
                (puthash id (float-time) magnus-coord--last-nudge)))
          (error
           (magnus-coord--log-undelivered-nudge
            instance text (error-message-string err))))))))


;;; Periodic reminders

(defvar magnus-coord--reminder-timer nil
  "Timer for periodic coordination reminders.")

(defun magnus-coord-start-reminders ()
  "Start periodic coordination reminders and AFK detection."
  (magnus-coord-stop-reminders)
  (when magnus-coord-reminder-interval
    (setq magnus-coord--reminder-timer
          (run-with-timer magnus-coord-reminder-interval
                         magnus-coord-reminder-interval
                         #'magnus-coord--send-reminders)))
  (magnus-coord--start-idle-watch))

(defun magnus-coord-stop-reminders ()
  "Stop periodic coordination reminders and AFK detection."
  (when magnus-coord--reminder-timer
    (cancel-timer magnus-coord--reminder-timer)
    (setq magnus-coord--reminder-timer nil))
  (magnus-coord--stop-idle-watch))

(defvar magnus-coord--reminder-templates
  '("Hey %s — take a quick look at .magnus-coord/current.md and the legacy .magnus-coord.md ingress when present. Publish useful updates with the protocol in .claude/magnus-instructions.md."
    "Coordination check, %s. Review .magnus-coord/current.md for messages, discoveries, decisions, and overlapping work; publish any update as an immutable event."
    "%s, heads up: revisit the generated coordination view. A teammate may have left context that saves you time; use your durable writer identity for anything you publish."
    "Quick sync, %s. Read the current and legacy coordination views, then publish status or non-obvious discoveries through .claude/magnus-instructions.md when needed.")
  "Rotating reminder messages. %s is replaced with the agent name.")

(defvar magnus-coord--reminder-index 0
  "Index into the rotating reminder templates.")

;;; Attention pattern learning

(defvar magnus-coord--attention-data (make-hash-table :test 'equal)
  "Hash: agent-name -> plist (:visits (FLOAT-TIME...) :messages INTEGER).
Visit timestamps are kept newest-first, max 50 entries.
Persists across sessions for adaptive nudge intervals.")

(defvar magnus-coord--attention-save-timer nil
  "Debounced timer for saving attention data.")

(defvar magnus-attention-data-file)

(defun magnus-coord--record-visit (agent-name)
  "Record a user visit to AGENT-NAME.
Debounced: only records if last visit was more than 30 seconds ago."
  (let* ((data (or (gethash agent-name magnus-coord--attention-data)
                   (list :visits nil :messages 0)))
         (visits (plist-get data :visits))
         (last-visit (car visits)))
    (when (or (null last-visit) (> (- (float-time) last-visit) 30))
      (puthash agent-name
               (plist-put data :visits
                          (seq-take (cons (float-time) visits) 50))
               magnus-coord--attention-data)
      (magnus-coord--schedule-attention-save))))

(defun magnus-coord--adaptive-interval (agent-name)
  "Return adaptive nudge interval for AGENT-NAME in seconds.
Matches the user's visiting rhythm.  Agents visited frequently
get shorter intervals (matching the pace).  Agents rarely visited
get longer intervals (up to the default)."
  (let* ((data (gethash agent-name magnus-coord--attention-data))
         (visits (plist-get data :visits))
         (default (or magnus-coord-reminder-interval 600)))
    (if (and visits (>= (length visits) 3))
        (let* ((recent (seq-take visits 10))
               (intervals (cl-mapcar (lambda (a b) (- a b))
                                     recent (cdr recent)))
               (avg (/ (apply #'+ intervals) (float (length intervals)))))
          (min default (max 300 (round (* avg 1.5)))))
      default)))

(defun magnus-coord--neglected-p (instance)
  "Return non-nil if INSTANCE is overdue for user attention.
Compares time since last visit against twice the adaptive interval."
  (when (eq (magnus-instance-status instance) 'running)
    (let* ((name (magnus-instance-name instance))
           (data (gethash name magnus-coord--attention-data))
           (visits (plist-get data :visits))
           (last-visit (car visits))
           (interval (magnus-coord--adaptive-interval name)))
      (and last-visit
           (> (- (float-time) last-visit) (* interval 2))))))

(defun magnus-coord--on-buffer-focus (_frame)
  "Record when the user switches to an agent's buffer."
  (let ((buf (window-buffer (selected-window))))
    (dolist (instance (magnus-instances-list))
      (when (eq buf (magnus-instance-buffer instance))
        (magnus-coord--record-visit (magnus-instance-name instance))))))

(defun magnus-coord-setup-attention-tracking ()
  "Start tracking user attention patterns."
  (add-hook 'window-buffer-change-functions
            #'magnus-coord--on-buffer-focus))

(defun magnus-coord-stop-attention-tracking ()
  "Stop tracking user attention patterns."
  (remove-hook 'window-buffer-change-functions
               #'magnus-coord--on-buffer-focus))

;;; Attention persistence

(defun magnus-coord--schedule-attention-save ()
  "Schedule a debounced save of attention data."
  (when magnus-coord--attention-save-timer
    (cancel-timer magnus-coord--attention-save-timer))
  (setq magnus-coord--attention-save-timer
        (run-with-idle-timer 5 nil #'magnus-coord-attention-save)))

(defun magnus-coord-attention-save ()
  "Save attention data to disk."
  (setq magnus-coord--attention-save-timer nil)
  (when (and (bound-and-true-p magnus-attention-data-file)
             (hash-table-count magnus-coord--attention-data))
    (let ((alist nil))
      (maphash (lambda (k v) (push (cons k v) alist))
               magnus-coord--attention-data)
      (with-temp-file magnus-attention-data-file
        (insert ";; Magnus attention data - do not edit manually\n")
        (pp alist (current-buffer))))))

(defun magnus-coord-attention-load ()
  "Load attention data from disk."
  (when (and (bound-and-true-p magnus-attention-data-file)
             (file-exists-p magnus-attention-data-file))
    (condition-case nil
        (let ((alist (with-temp-buffer
                       (insert-file-contents magnus-attention-data-file)
                       (goto-char (point-min))
                       (read (current-buffer)))))
          (clrhash magnus-coord--attention-data)
          (dolist (entry alist)
            (puthash (car entry) (cdr entry) magnus-coord--attention-data)))
      (error
       (message "Magnus: failed to load attention data")))))

(defvar magnus-coord--user-idle-p nil
  "Non-nil when the user has been detected as AFK.
Set by `magnus-coord--on-user-idle', cleared by `magnus-coord--on-user-return'.")

(defvar magnus-coord--do-not-disturb nil
  "Non-nil when do-not-disturb mode is active.
All periodic nudges are suppressed.  Toggle with `magnus-coord-toggle-dnd'.")

(defun magnus-coord-toggle-dnd ()
  "Toggle do-not-disturb mode.
When active, all periodic coordination nudges are suppressed.
Agents keep working but are not poked."
  (interactive)
  (setq magnus-coord--do-not-disturb (not magnus-coord--do-not-disturb))
  (message "Magnus: Do Not Disturb %s"
           (if magnus-coord--do-not-disturb "ON" "OFF"))
  (run-hooks 'magnus-instances-changed-hook))

(defun magnus-coord-agent-busy-p (instance)
  "Return non-nil if INSTANCE has signaled it is busy.
Agents create a busy file to tell Magnus to stop nudging them."
  (file-exists-p (magnus-process--agent-busy-path instance)))

(defun magnus-coord--send-reminders ()
  "Send a coordination reminder to idle instances.
Skips agents that are busy or whose vterm buffer is still active.
Suppressed entirely when AFK or DND is on."
  (unless (or magnus-coord--user-idle-p
              magnus-coord--do-not-disturb)
    (let ((template (nth (mod magnus-coord--reminder-index
                              (length magnus-coord--reminder-templates))
                         magnus-coord--reminder-templates)))
      (dolist (instance (magnus-instances-list))
        (when (and (eq (magnus-instance-status instance) 'running)
                   (not (magnus-coord-agent-busy-p instance))
                   (magnus-coord-agent-quiescent-p instance))
          (magnus-coord-nudge-agent
           instance
           (format template (magnus-instance-name instance))
           "Magnus")))
      (setq magnus-coord--reminder-index
            (1+ magnus-coord--reminder-index))))
  ;; Check context utilization (runs even when idle/active)
  (magnus-coord-check-context-all))

;;; Log trimming

(defun magnus-coord-trim-log (directory)
  "Trim the Log section in DIRECTORY's coordination file.
Keeps only the last `magnus-coord-log-max-entries' entries."
  (let ((max-entries (magnus-coord--dir-local-value
                      'magnus-coord-log-max-entries directory)))
    (when max-entries
      (let ((file (magnus-coord-file-path directory)))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (re-search-forward "^## Log" nil t)
            (let* ((log-start (match-beginning 0))
                   (section-end (save-excursion
                                  (if (re-search-forward "^## " nil t)
                                      (match-beginning 0)
                                    (point-max))))
                   (entries nil))
              ;; Collect log entries (timestamp lines)
              (goto-char log-start)
              (while (re-search-forward
                      "^\\[\\([^]]+\\)\\] .+$" section-end t)
                (push (match-beginning 0) entries))
              (setq entries (nreverse entries))
              ;; If over limit, delete everything before the Nth-from-last
              (when (> (length entries) max-entries)
                (let ((cut-point (nth (- (length entries)
                                         max-entries)
                                      entries)))
                  ;; Delete from after the Log header to the cut point
                  (goto-char log-start)
                  (forward-line 1)
                  ;; Skip blank lines and comments after header
                  (while (and (< (point) cut-point)
                              (or (looking-at "^$")
                                  (looking-at "^<!--")))
                    (if (looking-at "^<!--")
                        (if (re-search-forward "-->" nil t)
                            (forward-line 1)
                          (goto-char cut-point))
                      (forward-line 1)))
                  (when (< (point) cut-point)
                    (delete-region (point) cut-point))
                  (magnus-coord--write-file-atomic file)))))))))))

(defun magnus-coord-trim-all ()
  "Trim coordination file logs for all active project directories."
  (let ((dirs (delete-dups
               (mapcar
                (lambda (instance)
                  (magnus-coord--normalized-directory
                   (magnus-instance-directory instance)))
                (magnus-instances-list)))))
    (dolist (dir dirs)
      (magnus-coord-trim-log dir))))

;;; Intelligent coord file tidying

(defvar magnus-coord--last-tidy-time nil
  "Alist of (directory . timestamp) for last tidy request, for debouncing.")

(defun magnus-coord--maybe-tidy ()
  "Ask a random idle agent to tidy the coord file if it has grown too large.
Checks each project directory's coordination file against
`magnus-coord-tidy-size-threshold' and, when exceeded, picks a
random quiescent agent to consolidate the Discoveries and Decisions
sections.  Debounced to at most once per hour per directory."
  (let ((dirs (delete-dups
               (mapcar
                (lambda (instance)
                  (magnus-coord--normalized-directory
                   (magnus-instance-directory instance)))
                (magnus-instances-list)))))
    (dolist (dir dirs)
      (let ((file (magnus-coord-file-path dir))
            (threshold (magnus-coord--dir-local-value
                        'magnus-coord-tidy-size-threshold dir)))
        (when (and threshold
                   (file-exists-p file)
                   (> (file-attribute-size (file-attributes file))
                      threshold)
                     (let ((last (alist-get dir magnus-coord--last-tidy-time
                                            nil nil #'equal)))
                       (or (null last)
                           (> (- (float-time) last) 3600))))
            (let ((candidates
                   (cl-remove-if-not
                    (lambda (inst)
                      (and (magnus-coord--same-directory-p
                            (magnus-instance-directory inst) dir)
                           (eq (magnus-instance-status inst) 'running)
                           (not (magnus-coord-agent-busy-p inst))
                           (magnus-coord-agent-quiescent-p inst)))
                    (magnus-instances-list))))
              (when candidates
                (let ((chosen (nth (random (length candidates)) candidates)))
                  (setf (alist-get dir magnus-coord--last-tidy-time
                                   nil nil #'equal)
                        (float-time))
                  (magnus-coord-nudge-agent
                   chosen
                   (concat
                    "The coordination file is getting large. Please tidy it up: "
                    "read .magnus-coord.md, then in the Discoveries section "
                    "remove entries that are outdated or already captured in "
                    "code and commits, merge related entries, and keep only "
                    "what is still useful for agents working on this project. "
                    "In the Decisions section remove decisions that have been "
                    "fully implemented or are no longer relevant and keep "
                    "active architectural decisions. Do not touch the Active "
                    "Work or Log sections. Be aggressive about trimming — a "
                    "shorter file is more useful than a comprehensive one.")
                   "Magnus")))))))))

;;; AFK detection

(defvar magnus-coord--idle-timer nil
  "Idle timer that fires after `magnus-coord-idle-threshold' seconds.")

(defun magnus-coord--start-idle-watch ()
  "Start watching for user idleness."
  (magnus-coord--stop-idle-watch)
  (when magnus-coord-idle-threshold
    (setq magnus-coord--idle-timer
          (run-with-idle-timer magnus-coord-idle-threshold nil
                              #'magnus-coord--on-user-idle))))

(defun magnus-coord--stop-idle-watch ()
  "Stop watching for user idleness."
  (when magnus-coord--idle-timer
    (cancel-timer magnus-coord--idle-timer)
    (setq magnus-coord--idle-timer nil))
  (remove-hook 'pre-command-hook #'magnus-coord--on-user-return)
  (setq magnus-coord--user-idle-p nil))

(declare-function magnus-process--agent-memory-path "magnus-process")
(declare-function magnus-process--agent-busy-path "magnus-process")
(declare-function magnus-process--ensure-agent-dir "magnus-process")
(declare-function magnus-process--session-jsonl-path "magnus-process")
(declare-function magnus-process-create "magnus-process")

(defun magnus-coord--on-user-idle ()
  "Called when the user has been idle for `magnus-coord-idle-threshold'.
Tells agents to consolidate their memory and go to sleep."
  (setq magnus-coord--user-idle-p t)
  (dolist (instance (magnus-instances-list))
    (when (and (eq (magnus-instance-status instance) 'running)
               (not (magnus-coord-agent-busy-p instance)))
      (let* ((name (magnus-instance-name instance))
             (memory-rel (format ".claude/agents/%s/memory.md" name)))
        (magnus-process--ensure-agent-dir instance)
        (magnus-coord-nudge-agent
         instance
         (format "The user is away. Before you sleep, update your memory file at %s — write down what matters from this session: key decisions, things you learned, gotchas, unfinished work, your relationships with other agents. This file persists across restarts — it's how future-you remembers. Then go to sleep and wait quietly."
                 memory-rel)
         "Magnus"))))
  (add-hook 'pre-command-hook #'magnus-coord--on-user-return))

(defun magnus-coord--on-user-return ()
  "Called when the user presses a key after being idle.
Sends a wake-up message to running agents and re-arms the idle timer."
  (remove-hook 'pre-command-hook #'magnus-coord--on-user-return)
  (setq magnus-coord--user-idle-p nil)
  (dolist (instance (magnus-instances-list))
    (when (and (eq (magnus-instance-status instance) 'running)
               (not (magnus-coord-agent-busy-p instance)))
      (magnus-coord-nudge-agent
       instance
       (concat
        "The user is back! Resume normal operation — check "
        ".magnus-coord/current.md and the legacy ingress for updates.")
       "Magnus")))
  ;; Re-arm the idle timer for the next AFK period
  (magnus-coord--start-idle-watch))

;;; Context window monitoring

(defvar magnus-coord--context-warned (make-hash-table :test 'equal)
  "Hash table of instance-id to t for agents already warned about context.
Cleared when the agent's session changes (restart or compaction).")

(defun magnus-coord-check-context-all ()
  "Check context utilization for all running agents.
Warns agents approaching the context window limit."
  (when magnus-coord-context-warn-threshold
    (dolist (instance (magnus-instances-list))
      (when (eq (magnus-instance-status instance) 'running)
        (condition-case err
            (magnus-coord--check-context-one instance)
          (error (message "Magnus: context check error for %s: %s"
                          (magnus-instance-name instance)
                          (error-message-string err))))))))

(defun magnus-coord--check-context-one (instance)
  "Check context utilization for INSTANCE and warn if needed."
  (let ((id (magnus-instance-id instance)))
    (unless (gethash id magnus-coord--context-warned)
      (when-let ((usage (magnus-coord--read-context-usage instance)))
        (let ((ratio (/ (float usage) magnus-coord-context-max-tokens)))
          (when (>= ratio magnus-coord-context-warn-threshold)
            (puthash id t magnus-coord--context-warned)
            (let* ((name (magnus-instance-name instance))
                   (pct (round (* ratio 100)))
                   (memory-rel (format ".claude/agents/%s/memory.md" name)))
              (magnus-process--ensure-agent-dir instance)
              (magnus-coord-nudge-agent
               instance
               (format "Heads up: you're at %d%% context. Compaction is coming — write everything important to your memory file at %s NOW, while you still have full context. Key decisions, unfinished work, what you've learned, relationships with other agents. After compaction you'll lose detail."
                       pct memory-rel)
               "Magnus"))))))))

(defun magnus-coord--read-context-usage (instance)
  "Read the latest context token count from INSTANCE's session trace.
Returns total input tokens or nil if unavailable.  Only reads the
last 32KB of the file for efficiency."
  (when-let ((session-id (magnus-instance-session-id instance))
             (jsonl (magnus-process--session-jsonl-path
                     (magnus-instance-directory instance) session-id)))
    (let* ((attrs (file-attributes jsonl))
           (size (file-attribute-size attrs))
           (start (max 0 (- size 32768))))
      (with-temp-buffer
        (insert-file-contents jsonl nil start size)
        (magnus-coord--parse-last-usage)))))

(defun magnus-coord--parse-last-usage ()
  "Parse the last usage entry from the current buffer.
Looks for cache_read_input_tokens in the last complete JSON lines
and sums input_tokens + cache_creation_input_tokens +
cache_read_input_tokens."
  (goto-char (point-max))
  (let ((found nil))
    (while (and (not found)
                (re-search-backward "cache_read_input_tokens" nil t))
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        ;; Extract the three token counts via regex (avoid full JSON parse)
        (let ((input (magnus-coord--extract-number
                      "\"input_tokens\":[[:space:]]*\\([0-9]+\\)" line-start line-end))
              (cache-create (magnus-coord--extract-number
                             "\"cache_creation_input_tokens\":[[:space:]]*\\([0-9]+\\)" line-start line-end))
              (cache-read (magnus-coord--extract-number
                           "\"cache_read_input_tokens\":[[:space:]]*\\([0-9]+\\)" line-start line-end)))
          (when (and input cache-read)
            (setq found (+ input (or cache-create 0) cache-read))))))
    found))

(defun magnus-coord--extract-number (pattern start end)
  "Extract a number matching PATTERN between START and END."
  (save-excursion
    (goto-char start)
    (when (re-search-forward pattern end t)
      (string-to-number (match-string 1)))))

;;; @mention watching

(defvar magnus-coord--watched-dirs nil
  "List of directories being polled for coordination file changes.")

(defun magnus-coord-watched-directories ()
  "Return a copy of physical project roots with live coordination runtimes."
  (copy-sequence magnus-coord--watched-dirs))

(defvar magnus-coord--file-mtimes nil
  "Alist of (directory . modification-time) for polling dedup.")

(defvar magnus-coord--legacy-states nil
  "Alist of parsed legacy ingress state for watched project directories.")

(defvar magnus-coord--lifecycle-signatures (make-hash-table :test #'equal)
  "Visible instance-ID sets keyed by normalized project directory.")

(defun magnus-coord--normalized-directory (directory)
  "Return DIRECTORY's canonical physical project identity."
  (directory-file-name (file-truename (expand-file-name directory))))

(defun magnus-coord--same-directory-p (left right)
  "Return non-nil when LEFT and RIGHT have the same normalized spelling."
  (and (stringp left) (stringp right)
       (string= (magnus-coord--normalized-directory left)
                (magnus-coord--normalized-directory right))))

(defun magnus-coord--lifecycle-visible-p (instance)
  "Return non-nil when INSTANCE should appear as active coordination work."
  (memq (magnus-instance-status instance) '(running suspended)))

(defun magnus-coord--active-record-visible-p (record project-directory)
  "Apply Magnus lifecycle visibility to active RECORD in PROJECT-DIRECTORY.
Unmanaged durable writers remain event-derived.  A writer whose UUID belongs
to Magnus is visible only while that exact instance is live in this project."
  (let ((instance
         (magnus-instances-get
          (magnus-coord-state-active-record-writer-id record))))
    (or (null instance)
        (and (magnus-coord--lifecycle-visible-p instance)
             (magnus-coord--same-directory-p
              (magnus-instance-directory instance) project-directory)))))

;; Keep reduction pure and install the mutable registry only at projection.
(setq magnus-coord-state-active-record-visible-function
      #'magnus-coord--active-record-visible-p)

(defun magnus-coord--lifecycle-signature (directory)
  "Return sorted visible Magnus instance IDs for DIRECTORY."
  (sort
   (delq nil
         (mapcar
          (lambda (instance)
            (and (magnus-coord--lifecycle-visible-p instance)
                 (magnus-coord--same-directory-p
                  (magnus-instance-directory instance) directory)
                 (magnus-instance-id instance)))
          (magnus-instances-list)))
   #'string<))

(defun magnus-coord--known-lifecycle-directories ()
  "Return normalized directories whose active projection may have changed."
  (let ((directories
         (append (copy-sequence magnus-coord--watched-dirs)
                 (delq nil
                       (mapcar
                        (lambda (instance)
                          (let ((directory
                                 (magnus-instance-directory instance)))
                            (and (stringp directory)
                                 (not (string-empty-p directory))
                                 directory)))
                        (magnus-instances-list))))))
    (maphash (lambda (directory _signature) (push directory directories))
             magnus-coord--lifecycle-signatures)
    (delete-dups (mapcar #'magnus-coord--normalized-directory directories))))

(defun magnus-coord--instances-changed ()
  "Reproject cached event state whose live-instance visibility changed."
  (let ((missing (make-symbol "missing-lifecycle-signature")))
    (dolist (directory (magnus-coord--known-lifecycle-directories))
      (let ((before (gethash directory magnus-coord--lifecycle-signatures
                             missing))
            (after (magnus-coord--lifecycle-signature directory)))
        (unless (equal before after)
          (puthash directory after magnus-coord--lifecycle-signatures)
          (condition-case err
              (magnus-coord-runtime-reproject directory)
            (error
             (message "Magnus: lifecycle projection failed for %s: %s"
                      directory (error-message-string err)))))))))

(defun magnus-coord--pending-review-p (directory)
  "Return non-nil when DIRECTORY has a durable checkpoint still in flight."
  (and (fboundp 'magnus-review-list)
       (fboundp 'magnus-review-pending-checkpoint-request)
       (cl-some
        (lambda (review)
          (and (eq (magnus-review-lifecycle review) 'open)
               (magnus-coord--same-directory-p
                (magnus-review-project-root review) directory)
               (magnus-review-pending-checkpoint-request review)))
        (magnus-review-list))))

(defun magnus-coord--project-owned-p (directory)
  "Return non-nil when agents or pending reviews still own DIRECTORY's watch."
  (or (cl-some
       (lambda (instance)
         (magnus-coord--same-directory-p
          (magnus-instance-directory instance) directory))
       (magnus-instances-active-list))
      (magnus-coord--pending-review-p directory)))

(defun magnus-coord--maybe-stop-watching (directory)
  "Stop DIRECTORY only after every agent and pending-review owner releases it."
  (setq directory (magnus-coord--normalized-directory directory))
  (when (and (member directory magnus-coord--watched-dirs)
             (not (magnus-coord--project-owned-p directory)))
    (magnus-coord-stop-watching directory)))

(defvar magnus-coord--processed-mentions nil
  "Alist of (directory . list-of-processed-mention-hashes) to avoid duplicates.")

(defvar magnus-coord--processed-dms nil
  "Alist of (directory . list-of-processed-dm-hashes) to avoid duplicates.")

(defvar magnus-coord--processed-summons nil
  "Alist of (directory . list-of-processed-summon-hashes) to avoid duplicates.")

(defvar magnus-coord--processed-review-ready nil
  "Alist of processed review-checkpoint marker hashes by directory.")

(defvar magnus-coord-review-ready-hook nil
  "Hook run when an agent publishes a new REVIEW-READY marker.
Each function receives DIRECTORY and a plist containing `:request-id',
`:checkpoint-token', `:base', and `:head'.")

(defcustom magnus-coord-review-ready-retry-count 3
  "Maximum bounded retries after a review checkpoint handler errors."
  :type 'natnum
  :group 'magnus)

(defcustom magnus-coord-review-ready-retry-delay 2
  "Seconds between review checkpoint handler retries."
  :type 'number
  :group 'magnus)

(defvar magnus-coord--review-ready-retries (make-hash-table :test #'equal)
  "Legacy marker retry state keyed by (DIRECTORY . MARKER-HASH).")

(defvar magnus-coord--event-review-retries (make-hash-table :test #'equal)
  "Exact event-review retry state keyed by (DIRECTORY . EVENT-ID).")

(defvar magnus-coord--last-review-handler-error nil
  "Most recent review handler error captured during synchronous dispatch.")

(defun magnus-coord-review-retry-diagnostics (directory)
  "Return read-only retry diagnostics for durable reviews in DIRECTORY."
  (let (pending exhausted details)
    (maphash
     (lambda (key state)
       (when (magnus-coord--same-directory-p (car key) directory)
         (if (plist-get state :exhausted)
             (progn
               (push (cdr key) exhausted)
               (push (list :kind 'event :event-id (cdr key)
                           :last-error (plist-get state :last-error))
                     details))
           (push (cdr key) pending))))
     magnus-coord--event-review-retries)
    (maphash
     (lambda (key state)
       (when (magnus-coord--same-directory-p (car key) directory)
         (if (plist-get state :exhausted)
             (progn
               (push (concat "legacy:" (cdr key)) exhausted)
               (push (list :kind 'legacy :event-id (cdr key)
                           :last-error (plist-get state :last-error))
                     details))
           (push (concat "legacy:" (cdr key)) pending))))
     magnus-coord--review-ready-retries)
    (list :pending-review-retry-count (length pending)
          :exhausted-review-count (length exhausted)
          :exhausted-review-event-ids (sort exhausted #'string<)
          :exhausted-review-details
          (sort details
                (lambda (left right)
                  (string< (plist-get left :event-id)
                           (plist-get right :event-id)))))))

(defun magnus-coord--reset-exhausted-review-retries (directory)
  "Forget exhausted retry guards in DIRECTORY and return tagged IDs."
  (let (remove)
    (dolist (entry
             (list (cons 'event magnus-coord--event-review-retries)
                   (cons 'legacy magnus-coord--review-ready-retries)))
      (let (keys)
        (maphash
         (lambda (key state)
           (when (and (magnus-coord--same-directory-p (car key) directory)
                      (plist-get state :exhausted))
             (when-let ((timer (plist-get state :timer)))
               (cancel-timer timer))
             (push key keys)
             (push (cons (car entry) (cdr key)) remove)))
         (cdr entry))
        (dolist (key keys) (remhash key (cdr entry)))))
    remove))

(defvar magnus-coord--poll-timer nil
  "Timer for polling coordination files for messages and control markers.")

(defun magnus-coord-ensure-watchers ()
  "Start coordination runtimes for all directories with active instances.
Call this on startup so legacy messages and immutable events are observed for
instances restored from persistence."
  (add-hook 'magnus-instances-changed-hook #'magnus-coord--instances-changed)
  (let ((dirs
         (delete-dups
          (mapcar (lambda (instance)
                    (magnus-coord--normalized-directory
                     (magnus-instance-directory instance)))
                  (magnus-instances-active-list)))))
    (dolist (dir dirs)
      (unless (member dir magnus-coord--watched-dirs)
        (magnus-coord-start-watching dir))))
  (magnus-coord--start-poll-timer))

(defun magnus-coord--seed-legacy-content (directory content)
  "Seed legacy delivery deduplication for DIRECTORY from one CONTENT read."
  (magnus-coord--cache-legacy-content directory content)
  (magnus-coord--init-processed-mentions directory content)
  (magnus-coord--init-processed-dms directory content)
  (magnus-coord--init-processed-summons directory content)
  ;; Review checkpoints are durable/idempotent and intentionally replay.
  (magnus-coord--init-processed-review-ready directory content))

(defun magnus-coord--consume-legacy-content (directory content)
  "Consume every legacy effect for DIRECTORY from one CONTENT read."
  (when magnus-coord-mention-notify
    (magnus-coord--check-new-mentions directory content))
  (magnus-coord--check-new-dms directory content)
  (magnus-coord--check-new-summons directory content)
  (magnus-coord--check-new-review-ready directory content))

(defun magnus-coord-start-watching (directory)
  "Start observing legacy and event coordination in DIRECTORY.
Calling this again refreshes durable events without reseeding legacy delivery
state, so an in-flight mention cannot be silently reclassified as history."
  (setq directory (magnus-coord--normalized-directory directory))
  (unless (member directory magnus-coord--watched-dirs)
    (let* ((file (magnus-coord-file-path directory))
           (content (magnus-coord--read-legacy-content directory)))
      (cl-pushnew directory magnus-coord--watched-dirs :test #'equal)
      (setf (alist-get directory magnus-coord--file-mtimes nil nil #'equal)
            (and (file-exists-p file)
                 (file-attribute-modification-time (file-attributes file))))
      (magnus-coord--seed-legacy-content directory content)))
  (condition-case err
      (magnus-coord--consume-runtime-result
       directory (magnus-coord-runtime-start directory))
    (error
     (message "Magnus: event coordination startup failed for %s: %s"
              directory (error-message-string err))))
  (puthash (magnus-coord--normalized-directory directory)
           (magnus-coord--lifecycle-signature directory)
           magnus-coord--lifecycle-signatures)
  (magnus-coord--start-poll-timer))

(defun magnus-coord-stop-watching (directory)
  "Stop polling the coordination file in DIRECTORY."
  (setq directory (magnus-coord--normalized-directory directory))
  (setq magnus-coord--watched-dirs (delete directory magnus-coord--watched-dirs))
  (setq magnus-coord--file-mtimes
        (assoc-delete-all directory magnus-coord--file-mtimes))
  (setq magnus-coord--legacy-states
        (assoc-delete-all directory magnus-coord--legacy-states))
  (setq magnus-coord--processed-mentions
        (assoc-delete-all directory magnus-coord--processed-mentions))
  (setq magnus-coord--processed-dms
        (assoc-delete-all directory magnus-coord--processed-dms))
  (setq magnus-coord--processed-summons
        (assoc-delete-all directory magnus-coord--processed-summons))
  (setq magnus-coord--processed-review-ready
        (assoc-delete-all directory magnus-coord--processed-review-ready))
  (magnus-coord-runtime-stop directory)
  (let (remove)
    (maphash
     (lambda (key state)
       (when (equal (car key) directory)
         (when-let ((timer (plist-get state :timer)))
           (cancel-timer timer))
         (push key remove)))
     magnus-coord--review-ready-retries)
    (dolist (key remove)
      (remhash key magnus-coord--review-ready-retries)))
  (let (remove)
    (maphash
     (lambda (key state)
       (when (equal (car key) directory)
         (when-let ((timer (plist-get state :timer)))
           (cancel-timer timer))
         (push key remove)))
     magnus-coord--event-review-retries)
    (dolist (key remove)
      (remhash key magnus-coord--event-review-retries)))
  (when (and (null magnus-coord--watched-dirs)
             magnus-coord--poll-timer)
    (cancel-timer magnus-coord--poll-timer)
    (setq magnus-coord--poll-timer nil)))

(defun magnus-coord--start-poll-timer ()
  "Start the coordination file poll timer."
  (when (and magnus-coord--watched-dirs
             (null magnus-coord--poll-timer))
    (setq magnus-coord--poll-timer
          (run-with-timer 3 3 #'magnus-coord--poll-all))))

(defun magnus-coord--poll-all ()
  "Poll watched projects for legacy messages and immutable events.
Also update vterm buffer activity ticks for quiescence tracking."
  (magnus-coord--update-buffer-ticks)
  (dolist (directory magnus-coord--watched-dirs)
    (condition-case err
        (let* ((file (magnus-coord-file-path directory))
               (mtime (and (file-exists-p file)
                           (file-attribute-modification-time
                            (file-attributes file))))
               (last-mtime
                (alist-get directory magnus-coord--file-mtimes
                           nil nil #'equal)))
          (unless (equal mtime last-mtime)
            (let ((content (magnus-coord--read-legacy-content directory)))
              (magnus-coord--cache-legacy-content directory content)
              (magnus-coord--consume-legacy-content directory content)
              (setf (alist-get directory magnus-coord--file-mtimes
                               nil nil #'equal)
                    mtime))))
      (error
       (message "Magnus: legacy coord poll error for %s: %s"
                directory (error-message-string err))))
    (condition-case err
        (magnus-coord--consume-runtime-result
         directory (magnus-coord-runtime-refresh directory))
      (error
       (message "Magnus: event coord poll error for %s: %s"
                directory (error-message-string err))))))

(defun magnus-coord-stop-all-watchers ()
  "Stop all coordination file polling."
  (when magnus-coord--poll-timer
    (cancel-timer magnus-coord--poll-timer)
    (setq magnus-coord--poll-timer nil))
  (setq magnus-coord--watched-dirs nil)
  (setq magnus-coord--file-mtimes nil)
  (setq magnus-coord--legacy-states nil)
  (setq magnus-coord--processed-mentions nil)
  (setq magnus-coord--processed-dms nil)
  (setq magnus-coord--processed-summons nil)
  (setq magnus-coord--processed-review-ready nil)
  (clrhash magnus-coord--lifecycle-signatures)
  (maphash (lambda (_key state)
             (when-let ((timer (plist-get state :timer)))
               (cancel-timer timer)))
           magnus-coord--review-ready-retries)
  (clrhash magnus-coord--review-ready-retries)
  (maphash (lambda (_key state)
             (when-let ((timer (plist-get state :timer)))
               (cancel-timer timer)))
           magnus-coord--event-review-retries)
  (clrhash magnus-coord--event-review-retries)
  (magnus-coord-runtime-stop-all))

(defun magnus-coord-shutdown ()
  "Stop every long-lived coordination resource.
Safe to call after partial setup and safe to call more than once."
  (magnus-coord-stop-reminders)
  (magnus-coord-stop-all-watchers)
  (magnus-coord-stop-attention-tracking)
  (remove-hook 'magnus-instances-changed-hook
               #'magnus-coord--instances-changed)
  (remove-hook 'pre-command-hook #'magnus-coord--on-user-return)
  (when magnus-coord--attention-save-timer
    (cancel-timer magnus-coord--attention-save-timer)
    (setq magnus-coord--attention-save-timer nil)
    (condition-case err
        (magnus-coord-attention-save)
      (error
       (message "Magnus: could not flush attention data: %s"
                (error-message-string err))))))

(defun magnus-coord--read-legacy-content (directory)
  "Read DIRECTORY's legacy coordination ingress once, or return nil."
  (let ((file (magnus-coord-file-path directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun magnus-coord--parse-legacy-content (content)
  "Parse already-read legacy coordination CONTENT."
  (if content
      (with-temp-buffer
        (insert content)
        (magnus-coord--parse-buffer))
    (list :active nil :log nil :discoveries nil :decisions nil)))

(defun magnus-coord--cache-legacy-content (directory content)
  "Cache parsed legacy CONTENT for watched DIRECTORY."
  (setf (alist-get directory magnus-coord--legacy-states nil nil #'equal)
        (magnus-coord--parse-legacy-content content)))

(cl-defun magnus-coord--init-processed-mentions
    (directory &optional (content nil content-supplied-p))
  "Initialize processed mentions for DIRECTORY from legacy CONTENT.
Read the legacy ingress when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-legacy-content directory)))
  (when content
    (let ((mentions (magnus-coord--extract-mentions content)))
      (setf (alist-get directory magnus-coord--processed-mentions nil nil #'equal)
            (mapcar #'magnus-coord--mention-hash mentions)))))

(cl-defun magnus-coord--check-new-mentions
    (directory &optional (content nil content-supplied-p))
  "Check legacy CONTENT for new @mentions in DIRECTORY.
Read the legacy ingress when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-legacy-content directory)))
  (let* ((mentions (when content (magnus-coord--extract-mentions content)))
         (processed
          (alist-get directory magnus-coord--processed-mentions
                     nil nil #'equal)))
    (dolist (mention mentions)
      (let ((hash (magnus-coord--mention-hash mention)))
        (unless (member hash processed)
          ;; New mention - notify the agent
          (magnus-coord--notify-mention directory mention)
          (push hash processed))))
    (setf (alist-get directory magnus-coord--processed-mentions nil nil #'equal)
          processed)))

(defun magnus-coord--extract-mentions (content)
  "Extract all @mentions from CONTENT.
Returns list of (agent-name . context-line) pairs."
  (let (mentions)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward
              "@\\(?:{\\([^}\n]+\\)}\\|\\([a-zA-Z][-a-zA-Z0-9_]*\\)\\)"
              nil t)
        (let* ((prefix
                (buffer-substring-no-properties
                 (line-beginning-position) (match-beginning 0)))
               (agent (string-trim (or (match-string 1) (match-string 2))))
               (line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
          ;; Direct-message and summon envelopes have their own delivery
          ;; paths; do not also turn their target token into a plain mention.
          (unless (string-match-p "\\[\\(?:DM\\|SUMMON\\) \\'" prefix)
            (push (cons agent line) mentions)))))
    (nreverse mentions)))

(defun magnus-coord--mention-hash (mention)
  "Create a hash for MENTION to track duplicates."
  (secure-hash 'md5 (format "%s:%s" (car mention) (cdr mention))))

(defun magnus-coord--notify-mention (directory mention)
  "Notify the agent named in MENTION within DIRECTORY."
  (let* ((agent-name (car mention))
         (context-line (cdr mention))
         (instance (magnus-coord--find-instance-by-name agent-name directory)))
    (when instance
      (magnus-coord--send-mention-notification instance context-line))))

(defun magnus-coord--find-instance-by-name (name directory)
  "Find an instance with NAME working in DIRECTORY."
  (cl-find-if (lambda (inst)
                (and (string= (magnus-instance-name inst) name)
                     (magnus-coord--same-directory-p
                      (magnus-instance-directory inst) directory)))
              (magnus-instances-list)))

(defun magnus-coord--extract-sender-and-message (context-line)
  "Extract sender name and message from CONTEXT-LINE.
Returns (sender . message) or nil."
  (when (string-match
         (concat "\\[.*?\\] \\([^:]+\\): .*?"
                 "@\\(?:{[^}\n]+}\\|[^ ]+\\) \\(.*\\)")
         context-line)
    (let ((sender (match-string 1 context-line))
          (message (match-string 2 context-line)))
      (cons sender (string-trim message)))))

(defun magnus-coord--send-mention-notification (instance context-line)
  "Send a mention notification to INSTANCE with CONTEXT-LINE.
Delivers the message content without commanding the agent."
  (let* ((parsed (magnus-coord--extract-sender-and-message context-line))
         (msg (if parsed
                  (format "[From %s]: %s"
                          (car parsed) (cdr parsed))
                (format "[Mention in .magnus-coord.md]: %s"
                        context-line))))
    (magnus-coord-nudge-agent instance msg)))

;;; Agent-to-agent direct messages

(cl-defun magnus-coord--init-processed-dms
    (directory &optional (content nil content-supplied-p))
  "Initialize processed DMs for DIRECTORY from legacy CONTENT.
Read the legacy ingress when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-legacy-content directory)))
  (when content
    (let ((dms (magnus-coord--extract-dms content)))
      (setf (alist-get directory magnus-coord--processed-dms nil nil #'equal)
            (mapcar #'magnus-coord--dm-hash dms)))))

(defun magnus-coord--extract-dms (content)
  "Extract all [DM @name] patterns from CONTENT.
Returns list of (target sender message) tuples."
  (let (dms)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward
              "\\[DM @\\([^]\n]+\\)\\][[:space:]]*\\(.*\\)"
              nil t)
        (let* ((target (string-trim (match-string 1)))
               (message (string-trim (match-string 2)))
               (line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (sender (when (string-match "\\] \\([^:]+\\):" line)
                         (match-string 1 line))))
          (push (list target (or sender "unknown") message) dms))))
    (nreverse dms)))

(defun magnus-coord--dm-hash (dm)
  "Create a hash for DM to track duplicates."
  (secure-hash 'md5 (format "%s:%s:%s" (nth 0 dm) (nth 1 dm) (nth 2 dm))))

(cl-defun magnus-coord--check-new-dms
    (directory &optional (content nil content-supplied-p))
  "Check legacy CONTENT for new direct messages in DIRECTORY.
Read the legacy ingress when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-legacy-content directory)))
  (let* ((dms (when content (magnus-coord--extract-dms content)))
         (processed
          (alist-get directory magnus-coord--processed-dms
                     nil nil #'equal)))
    (dolist (dm dms)
      (let ((hash (magnus-coord--dm-hash dm)))
        (unless (member hash processed)
          (magnus-coord--deliver-dm directory dm)
          (push hash processed))))
    (setf (alist-get directory magnus-coord--processed-dms nil nil #'equal)
          processed)))

(defun magnus-coord--deliver-dm (directory dm)
  "Deliver DM to the target agent in DIRECTORY.
DM is (target sender message)."
  (let* ((target (nth 0 dm))
         (sender (nth 1 dm))
         (message (nth 2 dm))
         (instance (magnus-coord--find-instance-by-name target directory)))
    (when instance
      (magnus-coord-nudge-agent
       instance
       (format "[DM from %s]: %s" sender message)))))

;;; Agent-initiated summoning

(defvar magnus--summon-context)

(cl-defun magnus-coord--init-processed-summons
    (directory &optional (content nil content-supplied-p))
  "Initialize processed summons for DIRECTORY from legacy CONTENT.
Read the legacy ingress when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-legacy-content directory)))
  (when content
    (let ((summons (magnus-coord--extract-summons content)))
      (setf (alist-get directory magnus-coord--processed-summons nil nil #'equal)
            (mapcar #'magnus-coord--summon-hash summons)))))

(defun magnus-coord--extract-summons (content)
  "Extract all [SUMMON @name] patterns from CONTENT.
Returns list of (target sender reason) tuples."
  (let (summons)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward
              "\\[SUMMON @\\([^]\n]+\\)\\][[:space:]]*\\(.*\\)"
              nil t)
        (let* ((target (string-trim (match-string 1)))
               (reason (string-trim (match-string 2)))
               (line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (sender (when (string-match "\\] \\([^:]+\\):" line)
                         (match-string 1 line))))
          (push (list target (or sender "unknown") reason) summons))))
    (nreverse summons)))

(defun magnus-coord--summon-hash (summon)
  "Create a hash for SUMMON to track duplicates."
  (secure-hash 'md5 (format "%s:%s:%s"
                             (nth 0 summon) (nth 1 summon) (nth 2 summon))))

(cl-defun magnus-coord--check-new-summons
    (directory &optional (content nil content-supplied-p))
  "Check legacy CONTENT for new summon requests in DIRECTORY.
Read the legacy ingress when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-legacy-content directory)))
  (let* ((summons (when content (magnus-coord--extract-summons content)))
         (processed (alist-get directory magnus-coord--processed-summons
                               nil nil #'equal)))
    (dolist (summon summons)
      (let ((hash (magnus-coord--summon-hash summon)))
        (unless (member hash processed)
          ;; Wait for user to be idle before prompting
          (run-with-idle-timer 5 nil
                               #'magnus-coord--handle-summon directory summon)
          (push hash processed))))
    (setf (alist-get directory magnus-coord--processed-summons nil nil #'equal)
          processed)))

;;; Review checkpoint markers

(defun magnus-coord--review-ready-field (name fields)
  "Return NAME's value from whitespace-separated review marker FIELDS."
  (when (string-match
         (format "\\(?:\\`\\|[[:space:]]+\\)%s=\\([^[:space:]]+\\)"
                 (regexp-quote name))
         fields)
    (match-string 1 fields)))

(defun magnus-coord--extract-review-ready (content)
  "Extract machine-readable REVIEW-READY markers from CONTENT.
Return a list of plists.  A valid marker has this form:

  [REVIEW-READY request=ID checkpoint=TOKEN base=OID head=OID]

The review subsystem validates the request, attempt, and object IDs before
launching any work."
  (let (markers)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward
              "\\[REVIEW-READY[[:space:]]+\\([^]\n]+\\)\\]" nil t)
        (let* ((fields (match-string-no-properties 1))
               (request-id
                (magnus-coord--review-ready-field "request" fields))
               (checkpoint-token
                (magnus-coord--review-ready-field "checkpoint" fields))
               (base (magnus-coord--review-ready-field "base" fields))
               (head (magnus-coord--review-ready-field "head" fields)))
          (when (and request-id checkpoint-token base head
                     (string-match-p
                      "\\`[[:alnum:]_.:-]+\\'" request-id)
                     (string-match-p
                      "\\`[[:alnum:]_.:-]+\\'" checkpoint-token)
                     (string-match-p
                      "\\`[[:xdigit:]]\\{40,64\\}\\'" base)
                     (string-match-p
                      "\\`[[:xdigit:]]\\{40,64\\}\\'" head))
            (push (list :request-id request-id
                        :checkpoint-token checkpoint-token
                        :base (downcase base)
                        :head (downcase head))
                  markers)))))
    (nreverse markers)))

(defun magnus-coord--review-ready-hash (marker)
  "Return a stable deduplication hash for review checkpoint MARKER."
  (secure-hash 'sha256 (prin1-to-string marker)))

(defun magnus-coord--dispatch-review-ready (directory marker)
  "Safely dispatch MARKER from DIRECTORY to review handlers.
Return non-nil when all handlers complete or permanently reject the marker, so
an invalid immutable marker is consumed while a failed durable transition is
left unprocessed for bounded retry."
  (setq magnus-coord--last-review-handler-error nil)
  (when magnus-coord-review-ready-hook
    (condition-case err
        (progn
          (run-hook-with-args 'magnus-coord-review-ready-hook directory marker)
          t)
      (magnus-review-checkpoint-rejected
       (message "Magnus: ignored stale review checkpoint in %s — %s"
                (file-name-nondirectory (directory-file-name directory))
                (if (stringp (cadr err)) (cadr err)
                  (error-message-string err)))
       t)
      (error
       (setq magnus-coord--last-review-handler-error
             (error-message-string err))
       (message "Magnus: review checkpoint handler failed for %s: %s"
                directory magnus-coord--last-review-handler-error)
       nil))))

(defun magnus-coord--clear-review-ready-retry (directory hash)
  "Clear any pending retry for marker HASH in DIRECTORY."
  (let* ((key (cons directory hash))
         (state (gethash key magnus-coord--review-ready-retries)))
    (when-let ((timer (plist-get state :timer)))
      (cancel-timer timer))
    (remhash key magnus-coord--review-ready-retries)))

(defun magnus-coord--schedule-review-ready-retry (directory hash)
  "Schedule a bounded retry for marker HASH in DIRECTORY."
  (let* ((key (cons directory hash))
         (state (gethash key magnus-coord--review-ready-retries))
         (count (or (plist-get state :count) 0)))
    (when magnus-coord--last-review-handler-error
      (setq state
            (plist-put state :last-error
                       magnus-coord--last-review-handler-error)))
    (cond
     ((plist-get state :timer))
     ((>= count magnus-coord-review-ready-retry-count)
      (setq state (plist-put state :exhausted t))
      (message
       "Magnus: legacy review marker %s exhausted bounded retries; press g or diagnose"
       hash))
     (t
      (setq state (plist-put state :count (1+ count)))
      (setq state
            (plist-put
             state :timer
             (run-with-timer
              magnus-coord-review-ready-retry-delay nil
              (lambda ()
                (let ((current
                       (gethash key magnus-coord--review-ready-retries)))
                  (when current
                    (puthash key (plist-put current :timer nil)
                             magnus-coord--review-ready-retries))
                  (magnus-coord--check-new-review-ready directory))))))))
    (puthash key state magnus-coord--review-ready-retries)))

(cl-defun magnus-coord--init-processed-review-ready
    (directory &optional (content nil content-supplied-p))
  "Initialize checkpoint processing for DIRECTORY.
Unlike conversational messages, checkpoint markers are safe to replay because
the review lifecycle validates their request and attempt tokens.  Replaying
them on startup lets a persisted awaiting-checkpoint review recover after an
Emacs restart.  Use already-read legacy CONTENT when supplied."
  (setf (alist-get directory magnus-coord--processed-review-ready
                   nil nil #'equal)
        nil)
  (when magnus-coord-review-ready-hook
    (if content-supplied-p
        (magnus-coord--check-new-review-ready directory content)
      (magnus-coord--check-new-review-ready directory))))

(cl-defun magnus-coord--check-new-review-ready
    (directory &optional (content nil content-supplied-p))
  "Dispatch new REVIEW-READY markers from legacy CONTENT in DIRECTORY.
Read the legacy ingress when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-legacy-content directory)))
  (let* ((markers (when content
                    (magnus-coord--extract-review-ready content)))
         (processed
          (alist-get directory magnus-coord--processed-review-ready
                     nil nil #'equal))
         resolved)
    (dolist (marker markers)
      (let ((hash (magnus-coord--review-ready-hash marker)))
        (unless (member hash processed)
          (if (magnus-coord--dispatch-review-ready directory marker)
              (progn
                (magnus-coord--clear-review-ready-retry directory hash)
                (push hash processed)
                (setq resolved t))
            (magnus-coord--schedule-review-ready-retry directory hash)))))
    (setf (alist-get directory magnus-coord--processed-review-ready
                     nil nil #'equal)
          processed)
    (when resolved
      (magnus-coord--maybe-stop-watching directory))))

;;; Immutable event effects

(defun magnus-coord--event-log-content (record)
  "Render coordination log RECORD once for existing message extractors."
  (format "[%s] %s: %s"
          (magnus-coord-state-log-record-created-at record)
          (magnus-coord-state-log-record-writer-name record)
          (replace-regexp-in-string
           "[\r\n]+" " " (magnus-coord-state-log-record-message record))))

(defun magnus-coord--deliver-event-log (directory record)
  "Deliver conversational effects from immutable log RECORD in DIRECTORY."
  (let ((content (magnus-coord--event-log-content record)))
    (when magnus-coord-mention-notify
      (dolist (mention (magnus-coord--extract-mentions content))
        (magnus-coord--notify-mention directory mention)))
    (dolist (dm (magnus-coord--extract-dms content))
      (magnus-coord--deliver-dm directory dm))
    (dolist (summon (magnus-coord--extract-summons content))
      (run-with-idle-timer 5 nil
                           #'magnus-coord--handle-summon directory summon))))

(defun magnus-coord--event-review-marker (effect)
  "Convert durable review EFFECT to the review subsystem's marker plist."
  (list :request-id
        (magnus-coord-state-review-effect-request-id effect)
        :checkpoint-token
        (magnus-coord-state-review-effect-checkpoint-token effect)
        :base (magnus-coord-state-review-effect-base effect)
        :head (magnus-coord-state-review-effect-head effect)
        :writer-id (magnus-coord-state-review-effect-writer-id effect)
        :event-id (magnus-coord-state-review-effect-event-id effect)))

(defun magnus-coord--clear-event-review-retry (directory event-id)
  "Clear exact EVENT-ID retry state for DIRECTORY."
  (let* ((key (cons directory event-id))
         (state (gethash key magnus-coord--event-review-retries)))
    (when-let ((timer (plist-get state :timer)))
      (cancel-timer timer))
    (remhash key magnus-coord--event-review-retries)))

(defun magnus-coord--settle-event-review (directory event-id)
  "Settle EVENT-ID in DIRECTORY after its durable handler completes."
  (condition-case err
      (progn
        (magnus-coord-runtime-settle-review directory event-id)
        (magnus-coord--clear-event-review-retry directory event-id)
        t)
    (error
     (setq magnus-coord--last-review-handler-error
           (error-message-string err))
     (message "Magnus: could not settle review event %s: %s"
              event-id magnus-coord--last-review-handler-error)
     nil)))

(defun magnus-coord--schedule-event-review-retry
    (directory event-id marker)
  "Schedule a bounded exact retry for review EVENT-ID and MARKER."
  (let* ((key (cons directory event-id))
         (state (or (gethash key magnus-coord--event-review-retries)
                    (list :marker marker :count 0)))
         (count (or (plist-get state :count) 0)))
    (when magnus-coord--last-review-handler-error
      (setq state
            (plist-put state :last-error
                       magnus-coord--last-review-handler-error)))
    (cond
     ((plist-get state :timer))
     ((>= count magnus-coord-review-ready-retry-count)
      (setq state (plist-put state :exhausted t))
      (message
       "Magnus: review event %s exhausted bounded retries; press g or diagnose"
       event-id))
     (t
      (setq state (plist-put state :count (1+ count)))
      (setq state
            (plist-put
             state :timer
             (run-with-timer magnus-coord-review-ready-retry-delay nil
                             #'magnus-coord--retry-event-review
                             directory event-id)))))
    (puthash key state magnus-coord--event-review-retries)))

(defun magnus-coord--retry-event-review (directory event-id)
  "Retry the exact immutable review EVENT-ID in DIRECTORY."
  (let* ((key (cons directory event-id))
         (state (gethash key magnus-coord--event-review-retries))
         (marker (plist-get state :marker)))
    (when state
      (puthash key (plist-put state :timer nil)
               magnus-coord--event-review-retries)
      (if (and (magnus-coord--dispatch-review-ready directory marker)
               (magnus-coord--settle-event-review directory event-id))
          (progn
            (magnus-coord--maybe-stop-watching directory)
            t)
        (magnus-coord--schedule-event-review-retry
         directory event-id marker)))))

(defun magnus-coord--dispatch-event-review (directory effect)
  "Dispatch durable review EFFECT from DIRECTORY exactly once or retry it."
  (let* ((event-id (magnus-coord-state-review-effect-event-id effect))
         (key (cons directory event-id)))
    (when (and magnus-coord-review-ready-hook
               (not (gethash key magnus-coord--event-review-retries)))
      (let ((marker (magnus-coord--event-review-marker effect)))
        (if (and (magnus-coord--dispatch-review-ready directory marker)
                 (magnus-coord--settle-event-review directory event-id))
            t
          (magnus-coord--schedule-event-review-retry
           directory event-id marker))))))

(defun magnus-coord--consume-runtime-result (directory result)
  "Deliver bounded effects from coordination runtime RESULT in DIRECTORY."
  (dolist (record (magnus-coord-runtime-result-new-logs result))
    (magnus-coord--deliver-event-log directory record))
  (let ((reviews (magnus-coord-runtime-result-unresolved-reviews result)))
    (dolist (effect reviews)
      (magnus-coord--dispatch-event-review directory effect))
    ;; Do this only after the complete result has been settled: tearing down a
    ;; review-only runtime during the loop would strand sibling effects.
    (when reviews
      (magnus-coord--maybe-stop-watching directory)))
  result)

(defun magnus-coord--handle-summon (directory summon)
  "Handle a SUMMON request in DIRECTORY.
SUMMON is (target-name sender reason)."
  (let* ((target (nth 0 summon))
         (sender (nth 1 summon))
         (reason (nth 2 summon))
         (agents-dir (expand-file-name ".claude/agents/" directory))
         (memory (expand-file-name (concat target "/memory.md") agents-dir))
         (existing (mapcar #'magnus-instance-name (magnus-instances-list)))
         (is-active (member target existing))
         (is-dormant (and (file-exists-p memory) (not is-active))))
    (cond
     (is-dormant
      (when (y-or-n-p
             (format "Agent %s requests: summon %s (%s)? "
                     sender target
                     (if (string-empty-p reason) "no reason given" reason)))
        (magnus-coord--execute-summon directory target sender reason)))
     (is-active
      ;; Target is already running — notify the requesting agent
      (when-let ((requester (magnus-coord--find-instance-by-name sender directory)))
        (magnus-coord-nudge-agent
         requester
         (format "%s is already online — publish an @mention through the coordination protocol instead."
                 target)
         "Magnus"))))))

(defun magnus-coord--execute-summon (directory target sender reason)
  "Execute a summon of TARGET in DIRECTORY, requested by SENDER for REASON."
  (setq magnus--summon-context (list :sender sender :reason reason))
  (unwind-protect
      (magnus-process-create directory target)
    (setq magnus--summon-context nil))
  (message "Magnus: summoned %s for %s (%s)" target sender reason))

;;; Session retrospectives

(defvar magnus-coord--session-start-times (make-hash-table :test 'equal)
  "Hash: directory -> float-time when first agent joined this session.")

(defvar magnus-coord--latest-retro (make-hash-table :test 'equal)
  "Hash: directory -> path of the most recent retro file.")

(defun magnus-coord--git-log-since (directory since-time)
  "Get git log since SINCE-TIME in DIRECTORY."
  (let ((default-directory directory)
        (since (format-time-string "%Y-%m-%dT%H:%M:%S"
                                   (seconds-to-time since-time))))
    (condition-case err
        (with-temp-buffer
          (call-process "git" nil t nil
                        "log" "--oneline" "-n" "100"
                        (format "--since=%s" since))
          (let ((output (string-trim (buffer-string))))
            (if (string-empty-p output) "No commits" output)))
      (error
       (message "Magnus: git log failed: %s" (error-message-string err))
       "No git data"))))

(defun magnus-coord--collect-retro-data (directory)
  "Collect session data for a retrospective in DIRECTORY.
Returns a plist with :log, :discoveries, :decisions, :git, :start, :end."
  (let* ((parsed (magnus-coord-parse directory))
         (log-entries (plist-get parsed :log))
         (decisions (plist-get parsed :decisions))
         (discoveries (plist-get parsed :discoveries))
         (start-time (gethash directory magnus-coord--session-start-times))
         (git-log (when start-time
                    (magnus-coord--git-log-since directory start-time))))
    (list :log log-entries
          :discoveries discoveries
          :decisions decisions
          :git (or git-log "No git data")
          :start start-time
          :end (float-time))))

(defun magnus-coord--extract-discoveries (directory)
  "Extract the Discoveries section text from DIRECTORY's coord file."
  (let ((file (magnus-coord-file-path directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^## Discoveries" nil t)
          (forward-line 1)
          (let ((start (point))
                (end (if (re-search-forward "^## " nil t)
                         (match-beginning 0)
                       (point-max))))
            (string-trim (buffer-substring-no-properties start end))))))))

(defun magnus-coord--format-log-for-retro (entries)
  "Format log ENTRIES for the retro prompt."
  (if entries
      (mapconcat (lambda (e)
                   (format "[%s] %s: %s"
                           (plist-get e :time)
                           (plist-get e :agent)
                           (plist-get e :message)))
                 entries "\n")
    "No log entries"))

(defun magnus-coord--retro-prompt (data)
  "Build the Claude prompt for a session retro from DATA."
  (format "Summarize this multi-agent coding session. Be concise and useful.

## Coordination Log
%s

## Discoveries
%s

## Decisions
%s

## Git Commits This Session
%s

Write a session retrospective with these sections:
- **Accomplished**: What got done (2-4 bullets)
- **Decisions**: Key choices made and why
- **Discovered**: Important learnings or gotchas
- **Unfinished**: What's still pending
- **Next**: Suggested priorities for next session

Keep it under 250 words. No filler."
          (magnus-coord--format-log-for-retro (plist-get data :log))
          (if (plist-get data :discoveries)
              (mapconcat #'identity (plist-get data :discoveries) "\n")
            "None recorded")
          (if (plist-get data :decisions)
              (mapconcat #'identity (plist-get data :decisions) "\n")
            "None recorded")
          (plist-get data :git)))

(defun magnus-coord-generate-retro (directory)
  "Generate a session retrospective for DIRECTORY asynchronously."
  (setq directory (magnus-coord--normalized-directory directory))
  (when (and (bound-and-true-p magnus-claude-executable)
             (executable-find magnus-claude-executable))
    (let* ((data (magnus-coord--collect-retro-data directory))
           (key (list 'coord-retro directory)))
      (magnus-background-submit
       key 'claude
       (list :purpose 'agent
             :directory directory
             :prompt (magnus-coord--retro-prompt data)
             :allowed-tools ""
             :model (and (boundp 'magnus-headless-model)
                         magnus-headless-model)
             :name "session-retro")
       (list
        :on-complete
        (lambda (result)
          (when (plist-get result :success-p)
            (magnus-coord--save-retro
             directory
             (magnus--strip-thinking-markers (plist-get result :output))
             data))))))))

(defun magnus-coord--save-retro (directory content data)
  "Save retro CONTENT for DIRECTORY with session DATA metadata."
  (let* ((retros-dir (expand-file-name ".claude/retros/" directory))
         (timestamp (format-time-string "%Y-%m-%d-%H%M%S"))
         (file (expand-file-name (concat timestamp ".md") retros-dir))
         (start (plist-get data :start))
         (end-time (plist-get data :end)))
    (unless (file-directory-p retros-dir)
      (make-directory retros-dir t))
    (with-temp-file file
      (insert (format "# Session Retrospective — %s\n\n" timestamp))
      (when start
        (insert (format "**Session**: %s to %s\n\n"
                        (format-time-string "%H:%M" (seconds-to-time start))
                        (format-time-string "%H:%M" (seconds-to-time end-time)))))
      (insert content)
      (insert "\n"))
    (message "Magnus: session retro saved. Press F in magnus to view.")
    ;; Store the path for quick retrieval
    (puthash directory file magnus-coord--latest-retro)))

(defun magnus-coord--display-retro (file)
  "Display retro FILE in a buffer."
  (when (and file (file-exists-p file))
    (let ((buf (get-buffer-create "*magnus-retro*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents file)
          (goto-char (point-min)))
        (special-mode)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t))
      (switch-to-buffer buf))))

(defun magnus-coord--find-latest-retro (directory)
  "Find the most recent retro file in DIRECTORY."
  (or (gethash directory magnus-coord--latest-retro)
      (let ((retros-dir (expand-file-name ".claude/retros/" directory)))
        (when (file-directory-p retros-dir)
          (let ((files (directory-files retros-dir t "\\.md$")))
            (car (last (sort files #'string<))))))))

(defun magnus-retro ()
  "Show the latest session retrospective, or generate one.
If agents are running, generates a mid-session retro.
If no agents are running, shows the most recent saved retro."
  (interactive)
  (let* ((dir (or (when-let ((inst (car (magnus-instances-list))))
                    (magnus-instance-directory inst))
                  (magnus-coord--get-directory)))
         (has-agents (cl-some
                      (lambda (inst)
                        (and (magnus-coord--same-directory-p
                              (magnus-instance-directory inst) dir)
                             (eq (magnus-instance-status inst) 'running)))
                      (magnus-instances-list))))
    (if has-agents
        (progn
          (message "Generating mid-session retro...")
          (magnus-coord-generate-retro dir))
      ;; No agents — show latest saved retro
      (let ((file (magnus-coord--find-latest-retro dir)))
        (if file
            (magnus-coord--display-retro file)
          (user-error "No retrospectives found for this project"))))))

;;; Coordination file management

(defun magnus-coord-file-path (directory)
  "Get the coordination file path for DIRECTORY."
  (expand-file-name magnus-coord-file directory))

(defun magnus-coord-instructions-path (directory)
  "Get the instructions file path for DIRECTORY."
  (expand-file-name magnus-coord-instructions-file directory))

(defun magnus-coord-ensure-file (directory)
  "Ensure coordination file exists in DIRECTORY."
  (let ((file (magnus-coord-file-path directory)))
    (unless (file-exists-p file)
      (magnus-coord--create-file file))
    file))

(defun magnus-coord--create-file (file)
  "Create a new coordination FILE with initial template."
  (with-temp-file file
    (insert "# Agent Coordination\n\n")
    (insert "This file is used by Claude Code agents to coordinate their work.\n")
    (insert "Agents should check this file before starting and announce their plans.\n\n")
    (insert "## Active Work\n\n")
    (insert "<!-- Agents: Update this section when you start/finish work -->\n\n")
    (insert "| Agent | Area | Status | Files |\n")
    (insert "|-------|------|--------|-------|\n")
    (insert "\n## Discoveries\n\n")
    (insert "<!-- Share things you learned that other agents should know -->\n\n")
    (insert "## Decisions\n\n")
    (insert "<!-- Record agreed-upon decisions here -->\n\n")
    (insert "## Log\n\n")
    (insert "<!-- Agents: Append messages here to communicate -->\n\n")))

(defun magnus-coord-ensure-instructions (directory)
  "Ensure agent instructions file exists and is up-to-date in DIRECTORY."
  (let ((file (magnus-coord-instructions-path directory)))
    (if (file-exists-p file)
        (when (magnus-coord--instructions-stale-p file)
          (magnus-coord--create-instructions file directory))
      (magnus-coord--create-instructions file directory))
    file))

(defun magnus-coord--instructions-stale-p (file)
  "Return non-nil if FILE has an outdated instructions version."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((content (buffer-string)))
      (if (string-match "magnus-instructions-version: \\([0-9]+\\)" content)
          (< (string-to-number (match-string 1 content))
             magnus-coord--instructions-version)
        ;; No version marker — file predates versioning, regenerate
        t))))

(defun magnus-coord--create-instructions (file directory)
  "Create instructions FILE for agents in DIRECTORY."
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-file file
    (insert (magnus-coord--instructions-content directory))))

(defconst magnus-coord--git-exclude-patterns
  '("/.magnus-coord/" "/.claude/magnus-instructions.md")
  "Generated Magnus paths that should stay out of ordinary Git staging.")

(defun magnus-coord--git-path (directory path)
  "Return Git's resolved PATH for repository DIRECTORY, or nil."
  (when (executable-find "git")
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil
                                 "-C" directory "rev-parse" "--git-path" path))
        (let ((result (string-trim (buffer-string))))
          (unless (string-empty-p result)
            (if (file-name-absolute-p result)
                result
              (expand-file-name result directory))))))))

(defun magnus-coord--ensure-git-excludes (directory)
  "Locally exclude generated coordination paths in Git DIRECTORY.
This updates Git's repository-local `info/exclude', never the tracked root
`.gitignore'.  Return non-nil for a Git worktree, including when every pattern
was already present."
  (when-let ((file (magnus-coord--git-path directory "info/exclude")))
    (let ((parent (file-name-directory file))
          (existing ""))
      (when (or (file-symlink-p parent) (not (file-directory-p parent)))
        (error "Unsafe Git exclude directory: %s" parent))
      (when (or (file-symlink-p file) (file-directory-p file))
        (error "Unsafe Git exclude path: %s" file))
      (when (file-exists-p file)
        (when (> (file-attribute-size (file-attributes file)) (* 1024 1024))
          (error "Git exclude file is unexpectedly large: %s" file))
        (with-temp-buffer
          (insert-file-contents file)
          (setq existing (buffer-string))))
      (let* ((lines (split-string existing "\n" t))
             (missing
              (cl-remove-if (lambda (pattern) (member pattern lines))
                            magnus-coord--git-exclude-patterns)))
        (when missing
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region
             (concat
              (unless (or (string-empty-p existing)
                          (string-suffix-p "\n" existing))
                "\n")
              "# Magnus generated local state\n"
              (mapconcat #'identity missing "\n") "\n")
             nil file t 'quiet))))
    t)))

(defun magnus-coord--instructions-content (_directory)
  "Generate instructions content."
  (format "# Magnus Coordination Protocol

Magnus may run Claude and Codex agents together. Coordination is an immutable,
per-writer event stream; no agent rewrites a shared state file.

## Read First

1. Read applicable project guidance and inspect existing work.
2. Read `.magnus-coord/current.md` when present. It is generated and read-only.
3. Read legacy `.magnus-coord.md` when present. Older sessions may still write
   there, so it remains valid ingress during migration.
4. Check Active Work for overlap before editing. Resolve conflicts through a
   `log.append` event before touching another agent's claimed files.

The generated view may lag by a few seconds. Never edit `current.md`.

## Durable Identity

Your terminal exports:

- `MAGNUS_COORD_WRITER_ID`: your immutable Magnus instance UUID
- `MAGNUS_COORD_WRITER_NAME`: your human display name

Use the UUID as `writer_id` for every event, even if your display name changes.
Your inbox is `.magnus-coord/writers/$MAGNUS_COORD_WRITER_ID/`.

## Publishing One Event

Every final `*.json` file is immutable. Never edit, replace, or delete one.

1. Inspect every final event in your own inbox. Find the greatest positive
   `writer_sequence`; the next sequence is that value plus one (or 1 when the
   inbox is empty). If any existing final file cannot be safely parsed, stop and
   report it instead of guessing. Never run two writers with the same UUID.
2. Choose a globally unique, path-safe event ID using only letters, digits,
   `_`, `.`, `:`, and `-`. The final filename must be `<event-id>.json`.
3. Create a private temporary file named `.magnus-event-tmp-*` in that same
   writer inbox. Write exactly one JSON object, flush/close it, and use mode 600.
4. Atomically create the unique final name without replacement (a hard-link
   create from the temporary file has these semantics), then delete the
   temporary name. A collision requires a new ID and a newly encoded envelope.
5. Do not edit the generated view. Magnus validates, reduces, projects, and
   eventually garbage-collects superseded evidence.

The envelope has exactly these fields:

```json
{
  \"schema\": 1,
  \"id\": \"<event-id>\",
  \"writer_id\": \"<MAGNUS_COORD_WRITER_ID>\",
  \"writer_name\": \"<MAGNUS_COORD_WRITER_NAME>\",
  \"writer_sequence\": 1,
  \"created_at\": \"2026-08-04T12:34:56.123456Z\",
  \"kind\": \"log.append\",
  \"payload\": {\"message\": \"Starting work\"}
}
```

`created_at` is canonical UTC with exactly six fractional digits. It helps
display independent writers; only `writer_sequence` establishes your causal
order.

## Exact Event Payloads

- `log.append`: `{\"message\": \"...\"}`
- `active.set`: `{\"area\": \"...\", \"status\": \"in-progress\",
  \"files\": [\"path/one\", \"path/two\"]}`
- `active.clear`: `{}`
- `knowledge.put`: `{\"section\": \"discoveries\"|\"decisions\",
  \"entry_id\": \"stable-id\", \"text\": \"...\"}`
- `knowledge.remove`: `{\"section\": \"discoveries\"|\"decisions\",
  \"entry_id\": \"stable-id\"}`
- `review.ready`: `{\"request_id\": \"...\", \"checkpoint_token\": \"...\",
  \"base\": \"<full Git object ID>\", \"head\": \"<full Git object ID>\"}`

Payloads accept no extra fields. Put `@agent` (or `@{display name}` when the
name contains spaces), `[DM @display name]`, `[SUMMON @display name]`, and
`[ATTENTION]` conventions inside `log.append` messages when needed.

Publish `active.set` before substantive edits, with the files you expect to
touch. Publish `active.clear` when releasing them. Use stable knowledge
`entry_id` values so a later put updates that entry and a remove tombstones it.

An already-running legacy session without `MAGNUS_COORD_WRITER_ID` may continue
using `.magnus-coord.md`; do not invent a writer UUID. New sessions must use
events. No plugin or skill is required.

## Finishing and Authority

Publish completion and release events, preserve useful discoveries/decisions,
and update your first-person memory. Coordination context does not authorize
commits, pushes, deployments, destructive actions, external messages, or
unrelated changes.

## User-Visible Engineering Journal

For substantive user-facing messages, put a candid engineering decision journal
inside `[thinking]...[end-thinking]`, then the answer inside
`[response]...[end-response]`. State useful hypotheses, evidence, uncertainty,
constraints, alternatives, tradeoffs, contradictions, and corrections. This is
an explicit collaborative journal; never claim it is private or raw
chain-of-thought. Keep it proportional and omit empty narration.

<!-- magnus-instructions-version: %d -->
" magnus-coord--instructions-version))

;;; Coordination skill

(defun magnus-coord-skill-path (directory)
  "Get the coordination skill file path for DIRECTORY."
  (expand-file-name magnus-coord-skill-file directory))

(defun magnus-coord-ensure-skill (directory)
  "Ensure the coordinate skill file exists in DIRECTORY."
  (let ((file (magnus-coord-skill-path directory)))
    (unless (file-exists-p file)
      (magnus-coord--create-skill file))
    file))

(defun magnus-coord--create-skill (file)
  "Create the coordination skill FILE."
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-file file
    (insert (magnus-coord--skill-content))))

(defun magnus-coord--skill-content ()
  "Generate the coordination skill content."
  (format "# Coordination Check-in

When you run /coordinate, perform these steps in order:

## Steps

1. **Read the coordination file**: Open and read `%s` completely.
2. **Review active work**: Check the Active Work table. Note which agents are working on what files.
3. **Identify conflicts**: Compare your planned work against the table. Flag any file overlaps.
4. **Announce your claims**: Update the Active Work table with your row:
   - Your agent name
   - The area you are working on
   - Status: `in-progress`
   - Files you will touch (comma-separated)
5. **Log your check-in**: Append a message to the Log section:
   ```
   [HH:MM] your-name: Checked in. Working on <area>. Files: <list>.
   ```
6. **Resolve conflicts**: If you found conflicts in step 3, @mention the conflicting agent in the Log section and wait for acknowledgment before proceeding.

## After Completing a Task

1. Update your Active Work row: change status to `done` or remove it.
2. Log completion: `[HH:MM] your-name: Completed <task>. Files released: <list>.`
3. **Debrief**: Add anything you learned to the Discoveries section — gotchas, patterns, API quirks, things that surprised you. Your teammates will thank you.
4. If you made architectural decisions, add them to the Decisions section.
5. **Commit with context**: When committing, write a message that captures the *why* — what you learned, trade-offs you considered, gotchas you hit. This is the permanent record of your work.

## Important

- Always use the current time (HH:MM format) in log entries.
- Never modify another agent's Active Work row.
- If you are blocked by another agent, @mention them — they will be notified automatically.
- Read the Discoveries section when you check in — other agents may have learned something that helps you.
" magnus-coord-file))

;;; Parsing coordination file

(defun magnus-coord--parse-legacy (directory)
  "Parse DIRECTORY's legacy ingress without consulting event state."
  (setq directory (magnus-coord--normalized-directory directory))
  (let ((cached (assoc directory magnus-coord--legacy-states)))
    (if cached
        (cdr cached)
      (magnus-coord--parse-legacy-content
       (magnus-coord--read-legacy-content directory)))))

(defun magnus-coord--event-time (created-at)
  "Return a compact display time from canonical CREATED-AT."
  (if (and (stringp created-at)
           (string-match "T\\([0-9][0-9]:[0-9][0-9]\\):" created-at))
      (match-string 1 created-at)
    (or created-at "")))

(defun magnus-coord--event-active-entry (record)
  "Convert active state RECORD to the legacy display plist shape."
  (list :agent (magnus-coord-state-active-record-writer-name record)
        :writer-id (magnus-coord-state-active-record-writer-id record)
        :area (magnus-coord-state-active-record-area record)
        :status (magnus-coord-state-active-record-status record)
        :files (string-join
                (magnus-coord-state-active-record-files record) ", ")
        :event-id (magnus-coord-state-active-record-event-id record)))

(defun magnus-coord--event-log-entry (record)
  "Convert coordination log RECORD to the legacy display plist shape."
  (list :time
        (magnus-coord--event-time
         (magnus-coord-state-log-record-created-at record))
        :created-at (magnus-coord-state-log-record-created-at record)
        :agent (magnus-coord-state-log-record-writer-name record)
        :writer-id (magnus-coord-state-log-record-writer-id record)
        :message (magnus-coord-state-log-record-message record)
        :event-id (magnus-coord-state-log-record-event-id record)))

(defun magnus-coord-parse (directory)
  "Return merged event and legacy coordination state for DIRECTORY.
Event tombstones suppress a same-name legacy Active Work row.  The legacy file
remains read-compatible ingress but is never rewritten by this merge.  Log
entries are returned in chronological order.  Legacy entries precede event
entries because the immutable store is the migration successor to that
compatibility ingress."
  (setq directory (magnus-coord--normalized-directory directory))
  (let* ((legacy (magnus-coord--parse-legacy directory))
         (state (magnus-coord-runtime-current-state directory))
         (winners (and state (magnus-coord-state-active-winners state)))
         (managed-names
          (mapcar #'magnus-coord-state-active-record-writer-name winners))
         (legacy-active
          (cl-remove-if
           (lambda (entry)
             (member (plist-get entry :agent) managed-names))
           (plist-get legacy :active)))
         (event-active
          (mapcar #'magnus-coord--event-active-entry
                  (and state (magnus-coord-state-visible-active state))))
         (event-log
          (mapcar #'magnus-coord--event-log-entry
                  (and state (magnus-coord-state-logs state))))
         ;; `magnus-coord-add-log' inserts at the top of the legacy section,
         ;; whereas the event reducer is chronological.  Normalize once at
         ;; the read boundary so every consumer has the same contract.
         (legacy-log (reverse (copy-sequence (plist-get legacy :log))))
         (event-decisions
          (mapcar #'magnus-coord-state-knowledge-record-text
                  (and state (magnus-coord-state-decisions state))))
         (event-discoveries
          (mapcar #'magnus-coord-state-knowledge-record-text
                  (and state (magnus-coord-state-discoveries state)))))
    (list :active (append legacy-active event-active)
          :log (append legacy-log event-log)
          :decisions
          (delete-dups (append (plist-get legacy :decisions)
                               event-decisions))
          :discoveries
          (delete-dups (append (plist-get legacy :discoveries)
                               event-discoveries))
          :event-state state)))

(defun magnus-coord--parse-buffer ()
  "Parse the current buffer as a coordination file."
  (let ((active (magnus-coord--parse-active-table))
        (log (magnus-coord--parse-log))
        (discoveries (magnus-coord--parse-list-section "Discoveries"))
        (decisions (magnus-coord--parse-decisions)))
    (list :active active :log log
          :discoveries discoveries :decisions decisions)))

(defun magnus-coord--parse-active-table ()
  "Parse the Active Work table from current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (when (re-search-forward "^## Active Work" nil t)
        ;; Skip to table content (past header rows)
        (when (re-search-forward "^|[-|]+" nil t)
          (forward-line 1)
          (while (looking-at "^| *\\([^|]+\\) *| *\\([^|]+\\) *| *\\([^|]+\\) *| *\\([^|]*\\) *|")
            (let ((agent (string-trim (match-string 1)))
                  (area (string-trim (match-string 2)))
                  (status (string-trim (match-string 3)))
                  (files (string-trim (match-string 4))))
              (unless (string-empty-p agent)
                (push (list :agent agent :area area :status status :files files)
                      entries)))
            (forward-line 1))))
      (nreverse entries))))

(defun magnus-coord--parse-log ()
  "Parse the Log section from current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (when (re-search-forward "^## Log" nil t)
        (let ((section-end (save-excursion
                            (if (re-search-forward "^## " nil t)
                                (match-beginning 0)
                              (point-max)))))
          (while (re-search-forward "^\\[\\([0-9:]+\\)\\] \\([^:]+\\): \\(.+\\)$" section-end t)
            (push (list :time (match-string 1)
                       :agent (match-string 2)
                       :message (match-string 3))
                  entries))))
      (nreverse entries))))

(defun magnus-coord--parse-list-section (heading)
  "Parse Markdown list entries below section HEADING in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (when (re-search-forward
             (format "^## %s[[:space:]]*$" (regexp-quote heading)) nil t)
        (let ((section-end
               (save-excursion
                 (if (re-search-forward "^## " nil t)
                     (match-beginning 0)
                   (point-max)))))
          (while (re-search-forward "^- \\(.+\\)$" section-end t)
            (push (match-string 1) entries))))
      (nreverse entries))))

(defun magnus-coord--parse-decisions ()
  "Parse the Decisions section from current buffer."
  (magnus-coord--parse-list-section "Decisions"))

;;; Writing to coordination file

(defun magnus-coord-add-log (directory agent message)
  "Add a log MESSAGE from AGENT to coordination file in DIRECTORY."
  (let ((file (magnus-coord-ensure-file directory))
        (time (format-time-string "%H:%M")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward "^## Log\n+" nil t)
          (progn
            ;; Skip HTML comments (single or multi-line) and blank lines
            (while (and (not (eobp))
                        (or (looking-at "^$")
                            (looking-at "^<!--")))
              (if (looking-at "^<!--")
                  ;; Jump past closing -->, whether same line or later
                  (if (re-search-forward "-->" nil t)
                      (forward-line 1)
                    (goto-char (point-max)))
                (forward-line 1)))
            (insert (format "[%s] %s: %s\n\n" time agent message)))
        ;; No Log section, append at end
        (goto-char (point-max))
        (insert (format "\n[%s] %s: %s\n" time agent message)))
      (magnus-coord--write-file-atomic file))))

(defun magnus-coord-update-active (directory agent area status files)
  "Update AGENT's entry in the Active Work table in DIRECTORY.
AREA is what they're working on, STATUS is their status,
FILES is a list of files they're touching."
  (let ((file (magnus-coord-ensure-file directory))
        (files-str (if (listp files) (string-join files ", ") files)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward "^## Active Work" nil t)
          (let ((table-start (save-excursion
                              (re-search-forward "^|[-|]+" nil t)
                              (forward-line 1)
                              (point)))
                (table-end (save-excursion
                            (if (re-search-forward "^## " nil t)
                                (match-beginning 0)
                              (point-max))))
                (found nil))
            ;; Look for existing entry
            (goto-char table-start)
            (while (and (< (point) table-end)
                        (not found)
                        (looking-at "^| *\\([^|]+\\) *|"))
              (if (string= (string-trim (match-string 1)) agent)
                  (progn
                    (setq found t)
                    (delete-region (line-beginning-position)
                                  (1+ (line-end-position)))
                    (unless (string= status "done")
                      (insert (format "| %s | %s | %s | %s |\n"
                                     agent area status files-str))))
                (forward-line 1)))
            ;; Add new entry if not found
            (unless (or found (string= status "done"))
              (goto-char table-start)
              (insert (format "| %s | %s | %s | %s |\n"
                             agent area status files-str))))
        ;; No Active Work section, create one
        (goto-char (point-min))
        (when (re-search-forward "^# " nil t)
          (end-of-line)
          (insert "\n\n## Active Work\n\n")
          (insert "| Agent | Area | Status | Files |\n")
          (insert "|-------|------|--------|-------|\n")
          (insert (format "| %s | %s | %s | %s |\n"
                         agent area status files-str))))
      (magnus-coord--write-file-atomic file))))

(defun magnus-coord-clear-agent (directory agent)
  "Remove AGENT from the Active Work table in DIRECTORY."
  (magnus-coord-update-active directory agent "" "done" ""))

(defun magnus-coord-mark-session-end (directory)
  "Mark the end of a session in DIRECTORY's coordination file.
Appends a log entry instead of clearing sections, preserving
Discoveries and Decisions for future agents."
  (magnus-coord-add-log directory "Magnus" "Session ended"))

(defun magnus-coord-reconcile (directory)
  "Reconcile the Active Work table in DIRECTORY with live instances.
Removes entries for agents not in the Magnus registry, and entries
with stale statuses like done, died, finished, completed, stopped."
  (let* ((file (magnus-coord-file-path directory))
         (live-names (mapcar #'magnus-instance-name
                             (cl-remove-if-not
                              (lambda (inst)
                                (magnus-coord--same-directory-p
                                 (magnus-instance-directory inst) directory))
                              (magnus-instances-list)))))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^## Active Work" nil t)
          (when (re-search-forward "^|[-|]+" nil t)
            (forward-line 1)
            (while (looking-at "^| *\\([^|]+\\) *| *\\([^|]+\\) *| *\\([^|]+\\) *|")
              (let ((agent (string-trim (match-string 1)))
                    (status (string-trim (match-string 3))))
                (if (or (not (member agent live-names))
                        (string-match-p "done\\|died\\|finished\\|completed\\|stopped"
                                        status))
                    (delete-region (line-beginning-position)
                                  (min (1+ (line-end-position)) (point-max)))
                  (forward-line 1))))))
        (magnus-coord--write-file-atomic file)))))

(defun magnus-coord-reconcile-all ()
  "Reconcile coordination files for all project directories."
  (let ((dirs (delete-dups
               (mapcar
                (lambda (instance)
                  (magnus-coord--normalized-directory
                   (magnus-instance-directory instance)))
                (magnus-instances-list)))))
    (dolist (dir dirs)
      (magnus-coord-reconcile dir))))

(defun magnus-coord-refresh (directory)
  "Refresh canonical coordination state for DIRECTORY and deliver new effects.
The immutable event store is reduced once when its cheap revision changes.
The historical Markdown ingress is not rewritten."
  (setq directory (magnus-coord--normalized-directory directory))
  (let ((rearmed (magnus-coord--reset-exhausted-review-retries directory)))
    (when rearmed
      (message "Magnus: re-armed %d exhausted review checkpoint retr%s in %s"
               (length rearmed) (if (= (length rearmed) 1) "y" "ies")
               (file-name-nondirectory
                (directory-file-name directory))))
    (prog1
        (magnus-coord--consume-runtime-result
         directory (magnus-coord-runtime-refresh directory))
      (when (cl-find 'legacy rearmed :key #'car)
        (magnus-coord--check-new-review-ready directory)))))

(defun magnus-coord-refresh-all ()
  "Refresh canonical coordination state for every active or watched project.
Return an alist of project directories and refresh results.  A failure in one
project is reported without preventing healthy siblings from refreshing."
  (let ((directories
         (delete-dups
          (append (copy-sequence magnus-coord--watched-dirs)
                  (mapcar
                   (lambda (instance)
                     (magnus-coord--normalized-directory
                      (magnus-instance-directory instance)))
                   (magnus-instances-active-list)))))
        results)
    (dolist (directory directories)
      (condition-case err
          (push (cons directory (magnus-coord-refresh directory)) results)
        (error
         (message "Magnus: coordination refresh failed for %s: %s"
                  directory (error-message-string err)))))
    (nreverse results)))

(defun magnus-coord-current-path (directory)
  "Return DIRECTORY's generated coordination-view path without creating it."
  (expand-file-name "current.md" (magnus-coord-store-directory directory)))

(defun magnus-coord-has-state-p (directory)
  "Return non-nil when DIRECTORY has cached, generated, or legacy state.
This predicate performs no event scan, reduction, projection, or delivery."
  (or (magnus-coord-runtime-current-state directory)
      (file-exists-p (magnus-coord-file-path directory))
      (condition-case err
          (file-exists-p (magnus-coord-current-path directory))
        (error
         (message "Magnus: cannot inspect coordination state for %s: %s"
                  directory (error-message-string err))
         nil))))

;;; Agent registration

(defun magnus-coord-register-agent (directory instance)
  "Register INSTANCE's durable event identity in DIRECTORY."
  (setq directory (magnus-coord--normalized-directory directory))
  (let ((writer-id (magnus-instance-id instance)))
    (add-hook 'magnus-instances-changed-hook #'magnus-coord--instances-changed)
    (magnus-coord-ensure-instructions directory)
    (condition-case err
        (magnus-coord--ensure-git-excludes directory)
      (error
       (message "Magnus: could not install local Git excludes for %s: %s"
                directory (error-message-string err))))
    (magnus-coord-store-ensure-writer-directory directory writer-id)
    (unless (member directory magnus-coord--watched-dirs)
      (magnus-coord-start-watching directory))
    (unless (gethash directory magnus-coord--session-start-times)
      (puthash directory (float-time) magnus-coord--session-start-times))))

(defun magnus-coord-unregister-agent (directory instance)
  "Release Magnus's runtime ownership for INSTANCE in DIRECTORY.
The agent owns its append-only `active.clear' and completion events.  Magnus
does not forge a sequence on its behalf while its process may still be live."
  (setq directory (magnus-coord--normalized-directory directory))
  (let ((remaining
         (cl-count-if
          (lambda (other)
            (and (not (eq other instance))
                 (magnus-coord--same-directory-p
                  (magnus-instance-directory other) directory)))
          (magnus-instances-active-list))))
    (when (zerop remaining)
      (magnus-coord-generate-retro directory)
      (remhash directory magnus-coord--session-start-times)
      (magnus-coord--maybe-stop-watching directory))))

;;; Display

(defun magnus-coord-format-active (parsed)
  "Format the :active entries from PARSED for display."
  (let ((active (plist-get parsed :active)))
    (if active
        (mapconcat
         (lambda (entry)
           (format "  %s: %s [%s]"
                   (propertize (plist-get entry :agent) 'face 'magnus-status-instance-name)
                   (plist-get entry :area)
                   (propertize (plist-get entry :status)
                              'face (if (string= (plist-get entry :status) "in-progress")
                                       'magnus-status-running
                                     'magnus-status-instance-dir))))
         active
         "\n")
      (propertize "  No active work" 'face 'magnus-status-empty-hint))))

(defun magnus-coord-recent-log (entries limit)
  "Return the most recent LIMIT chronological log ENTRIES.
The returned entries retain chronological order."
  (let ((drop (- (length entries) (max 0 limit))))
    (if (> drop 0) (nthcdr drop entries) entries)))

(defun magnus-coord-format-log (parsed &optional limit)
  "Format the :log entries from PARSED for display.
Show at most LIMIT entries (default 5)."
  (let* ((log (plist-get parsed :log))
         (entries (if limit (magnus-coord-recent-log log limit) log)))
    (if entries
        (mapconcat
         (lambda (entry)
           (format "  [%s] %s: %s"
                   (propertize (plist-get entry :time) 'face 'magnus-status-instance-dir)
                   (propertize (plist-get entry :agent) 'face 'magnus-status-instance-name)
                   (plist-get entry :message)))
         entries
         "\n")
      (propertize "  No messages yet" 'face 'magnus-status-empty-hint))))

;;; Interactive commands

(defun magnus-coord-open (directory)
  "Open DIRECTORY's legacy coordination ingress, creating it if necessary."
  (interactive (list (magnus-coord--get-directory)))
  (find-file (magnus-coord-ensure-file directory)))

(defun magnus-coord-open-current (directory)
  "Refresh and open DIRECTORY's generated coordination view read-only."
  (interactive (list (magnus-coord--get-directory)))
  (magnus-coord-refresh directory)
  (let ((file (magnus-coord-current-path directory)))
    (unless (file-exists-p file)
      (user-error "No generated coordination view is available"))
    (find-file-read-only file)))

(defun magnus-coord-open-instructions (directory)
  "Open the instructions file for DIRECTORY."
  (interactive (list (magnus-coord--get-directory)))
  (find-file (magnus-coord-ensure-instructions directory)))

(defun magnus-coord--get-directory ()
  "Get directory for coordination, prompting if needed."
  (or (when (bound-and-true-p magnus-context--directory)
        magnus-context--directory)
      (when (fboundp 'project-current)
        (when-let ((project (project-current 'maybe)))
          (project-root project)))
      (read-directory-name "Project directory: ")))

(provide 'magnus-coord)
;;; magnus-coord.el ends here
