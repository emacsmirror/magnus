;;; magnus-provider-tests.el --- Provider compatibility tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'magnus)
(require 'magnus-instances)
(require 'magnus-provider)
(require 'magnus-provider-codex)
(require 'magnus-coord)
(require 'magnus-health)

(defun magnus-test--codex-tui (instance)
  "Create a live terminal-process stand-in for INSTANCE."
  (let* ((buffer (generate-new-buffer
                  (format " *magnus-test-tui:%s*"
                          (magnus-instance-id instance))))
         (process (make-pipe-process
                   :name (generate-new-buffer-name "magnus-test-tui")
                   :buffer buffer
                   :noquery t)))
    (process-put process 'magnus-codex-instance instance)
    (magnus-instances-update instance :buffer buffer :status 'running)
    (cons buffer process)))

(defun magnus-test--delete-terminal (terminal)
  "Delete TERMINAL's process and buffer."
  (when terminal
    (when (process-live-p (cdr terminal))
      (delete-process (cdr terminal)))
    (when (buffer-live-p (car terminal))
      (kill-buffer (car terminal)))))

(defun magnus-test--write-codex-rollout
    (root id directory prompt &optional parent-thread-id padding)
  "Write a test Codex rollout under ROOT for ID, DIRECTORY, and PROMPT.
PARENT-THREAD-ID marks the record as a subagent when non-nil.
PADDING adds an unrelated record before the first user message."
  (let* ((date-directory (expand-file-name "2026/07/11" root))
         (file (expand-file-name (format "rollout-test-%s.jsonl" id)
                                 date-directory)))
    (make-directory date-directory t)
    (with-temp-file file
      (insert
       (json-encode
        `((type . "session_meta")
          (payload . ((id . ,id) (session_id . ,id)
                      (parent_thread_id . ,parent-thread-id)
                      (cwd . ,directory)))))
       "\n"
       (json-encode
        '((type . "event_msg") (payload . ((type . "task_started")))))
       "\n"
       (if padding
           (concat (json-encode `((type . "response_item")
                                  (payload . ((text . ,padding)))))
                   "\n")
         "")
       (json-encode
        `((type . "event_msg")
          (payload . ((type . "user_message") (message . ,prompt)))))
       "\n"))
    file))

(ert-deftest magnus-provider-legacy-state-defaults-to-claude ()
  (let ((instance (magnus-instances-deserialize
                   '(:id "legacy" :name "old-user"
                     :directory "/tmp" :status stopped))))
    (should (eq (magnus-instance-provider instance) 'claude))
    (should-not (magnus-provider-external-p instance))))

(ert-deftest magnus-upgrade-requires-restart-before-reinitialization ()
  (let ((magnus--restart-required t)
        (magnus--initialized nil))
    (should-error (magnus--ensure-initialized) :type 'user-error)))

(ert-deftest magnus-provider-round-trips-codex-state ()
  (let* ((instance (magnus-instances-create "/tmp" "codex-user" 'codex))
         (restored (magnus-instances-deserialize
                    (magnus-instances-serialize instance))))
    (should (eq (magnus-instance-provider restored) 'codex))
    (should (magnus-provider-external-p restored))))

(ert-deftest magnus-provider-unknown-never-falls-back-to-claude ()
  (let ((instance (magnus-instances-create "/tmp" "unknown" 'not-a-provider)))
    (should (magnus-provider-external-p instance))
    (should-error (magnus-provider-call instance 'start) :type 'user-error)))

(ert-deftest magnus-codex-onboarding-keeps-identity-and-journal ()
  (let* ((instance (magnus-instances-create "/tmp" "solar-fox" 'codex))
         (prompt (magnus-codex--onboarding-prompt
                  instance "Fix the parser" "marker-1")))
    (should (string-match-p "You are solar-fox" prompt))
    (should (string-match-p "Fix the parser" prompt))
    (should (string-match-p "marker-1" prompt))
    (should (string-match-p "\\[thinking\\]" prompt))
    (should (string-match-p "visible working notebook" prompt))
    (should (string-match-p "Discoveries" prompt))
    (should (string-match-p "future-you" prompt))))

(ert-deftest magnus-codex-onboards-returning-identity-from-memory ()
  (let* ((directory (make-temp-file "magnus-codex-memory-" t))
         (memory-directory
          (expand-file-name ".claude/agents/solar-fox" directory))
         (memory-file (expand-file-name "memory.md" memory-directory)))
    (unwind-protect
        (progn
          (make-directory memory-directory t)
          (with-temp-file memory-file
            (insert "# Solar Fox\n\nI remember this project.\n"))
          (let* ((instance (magnus-instances-create
                            directory "solar-fox" 'codex))
                 (instructions (magnus-codex--instructions instance)))
            (should (string-match-p "been here before" instructions))
            (should (string-match-p "own prior voice" instructions))
            (should (string-match-p
                     (regexp-quote
                      ".claude/agents/solar-fox/memory.md")
                     instructions))))
      (delete-directory directory t))))

(ert-deftest magnus-codex-tui-command-is-standalone-and-resumable ()
  (let ((instance (magnus-instances-create
                   "/tmp/project with spaces" "tui-codex" 'codex)))
    (let ((fresh (magnus-codex--tui-command instance "First prompt")))
      (should (string-match-p "exec codex" fresh))
      (should-not (string-match-p "app-server\\|--remote\\|resume" fresh)))
    (magnus-instances-update instance :session-id "thread-1")
    (let ((resumed (magnus-codex--tui-command instance "read this next")))
      (should (string-match-p "exec codex resume" resumed))
      (should (string-match-p "thread-1" resumed))
      (should (string-match-p
               (regexp-quote (shell-quote-argument "read this next"))
               resumed)))))

(ert-deftest magnus-codex-session-capture-matches-exact-first-prompt ()
  (let* ((root (make-temp-file "magnus-codex-sessions-" t))
         (directory (make-temp-file "magnus-codex-project-" t))
         (prompt "Unique Magnus onboarding\nmarker:alpha")
         (file (magnus-test--write-codex-rollout
                root "session-alpha" directory prompt)))
    (unwind-protect
        (should (equal (magnus-codex--session-id-from-file
                        file directory prompt)
                       "session-alpha"))
      (delete-directory root t)
      (delete-directory directory t))))

(ert-deftest magnus-codex-session-capture-is-concurrent-launch-safe ()
  (let* ((root (make-temp-file "magnus-codex-sessions-" t))
         (directory (make-temp-file "magnus-codex-project-" t))
         (first (magnus-instances-create directory "first-codex" 'codex))
         (second (magnus-instances-create directory "second-codex" 'codex))
         (first-file (magnus-test--write-codex-rollout
                      root "session-first" directory "prompt:first"))
         (second-file (magnus-test--write-codex-rollout
                       root "session-second" directory "prompt:second")))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--session-files)
                   (lambda () (list second-file first-file))))
          (should (equal (magnus-codex--find-session-id
                          first "prompt:first" nil)
                         "session-first"))
          (should (equal (magnus-codex--find-session-id
                          second "prompt:second" nil)
                         "session-second")))
      (delete-directory root t)
      (delete-directory directory t))))

(ert-deftest magnus-codex-session-capture-rejects-subagent-rollout ()
  (let* ((root (make-temp-file "magnus-codex-sessions-" t))
         (directory (make-temp-file "magnus-codex-project-" t))
         (prompt "Inherited parent prompt")
         (file (magnus-test--write-codex-rollout
                root "session-child" directory prompt "session-parent")))
    (unwind-protect
        (should-not
         (magnus-codex--session-id-from-file file directory prompt))
      (delete-directory root t)
      (delete-directory directory t))))

(ert-deftest magnus-codex-session-capture-tolerates-large-preamble ()
  (let* ((root (make-temp-file "magnus-codex-sessions-" t))
         (directory (make-temp-file "magnus-codex-project-" t))
         (prompt "Onboarding after a large preamble")
         (file (magnus-test--write-codex-rollout
                root "session-large" directory prompt nil
                (make-string 150000 ?x))))
    (unwind-protect
        (should (equal (magnus-codex--session-id-from-file
                        file directory prompt)
                       "session-large"))
      (delete-directory root t)
      (delete-directory directory t))))

(ert-deftest magnus-codex-stale-capture-cannot-reassign-replacement ()
  (let* ((instance (magnus-instances-create "/tmp" "replacement" 'codex))
         (old-terminal (magnus-test--codex-tui instance))
         (old-process (cdr old-terminal))
         (new-terminal (magnus-test--codex-tui instance)))
    (unwind-protect
        (progn
          (process-put old-process 'magnus-codex-capture-prompt "old")
          (process-put old-process 'magnus-codex-files-before nil)
          (process-put old-process 'magnus-codex-capture-deadline
                       (+ (float-time) 30))
          (cl-letf (((symbol-function 'magnus-codex--find-session-id)
                     (lambda (&rest _arguments) "stale-session")))
            (magnus-codex--poll-session old-process))
          (should-not (magnus-instance-session-id instance))
          (should (eq (magnus-codex--tui-process instance)
                      (cdr new-terminal))))
      (magnus-test--delete-terminal old-terminal)
      (magnus-test--delete-terminal new-terminal))))

(ert-deftest magnus-codex-messages-use-tui-as-sole-writer ()
  (let* ((instance (magnus-instances-create "/tmp" "tui-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         sent return-sent timers)
    (process-put process 'magnus-codex-ready t)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p) (setq sent text)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-sent t)))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest arguments)
                     (push (cons function arguments) timers)
                     'test-timer)))
          (magnus-codex-send instance "read this next")
          (should (equal sent "read this next"))
          (should return-sent)
          (should-not (process-get process 'magnus-codex-input-queue)))
      (magnus-test--delete-terminal terminal))))

(ert-deftest magnus-codex-concurrent-tui-messages-are-serialized ()
  (let* ((instance (magnus-instances-create "/tmp" "queued-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         timers events)
    (process-put process 'magnus-codex-ready t)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p)
                     (setq events (append events (list text)))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq events (append events '(return)))))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq timers
                           (append timers (list (cons function arguments))))
                     'test-timer)))
          (magnus-codex-send instance "first")
          (magnus-codex-send instance "second")
          (should (equal events '("first" return)))
          (while timers
            (let ((timer (pop timers)))
              (apply (car timer) (cdr timer))))
          (should (equal events '("first" return "second" return))))
      (magnus-test--delete-terminal terminal))))

(ert-deftest magnus-codex-stop-interrupts-and-closes-native-tui ()
  (let* ((instance (magnus-instances-create "/tmp" "stoppable" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         sent-key)
    (process-put process 'magnus-codex-capture-prompt "short-lived")
    (process-put process 'magnus-codex-files-before nil)
    (process-put process 'magnus-codex-capture-deadline (+ (float-time) 30))
    (cl-letf (((symbol-function 'vterm-send-key)
               (lambda (key &rest _arguments) (setq sent-key key)))
              ((symbol-function 'magnus-codex--find-session-id)
               (lambda (&rest _arguments) "session-at-stop"))
              ((symbol-function 'run-with-timer)
               (lambda (_seconds _repeat function &rest arguments)
                 (apply function arguments))))
      (magnus-codex-stop instance)
      (should (equal sent-key "C-c"))
      (should (equal (magnus-instance-session-id instance) "session-at-stop"))
      (should (eq (magnus-instance-status instance) 'stopped))
      (should-not (magnus-instance-buffer instance)))
    (magnus-test--delete-terminal terminal)))

(ert-deftest magnus-codex-stale-tui-exit-preserves-replacement ()
  (let* ((instance (magnus-instances-create "/tmp" "replacement" 'codex))
         (old-terminal (magnus-test--codex-tui instance))
         (old-process (cdr old-terminal))
         (new-terminal nil)
         sentinel)
    (unwind-protect
        (progn
          (magnus-codex--setup-tui-sentinel instance (car old-terminal))
          (setq sentinel (process-sentinel old-process))
          (setq new-terminal (magnus-test--codex-tui instance))
          (set-process-sentinel old-process nil)
          (delete-process old-process)
          (funcall sentinel old-process "exited")
          (should (magnus-codex-running-p instance))
          (should (eq (magnus-instance-status instance) 'running)))
      (magnus-test--delete-terminal old-terminal)
      (magnus-test--delete-terminal new-terminal))))

(ert-deftest magnus-coord-stopped-agent-nudge-is-logged-not-signaled ()
  (let* ((directory (make-temp-file "magnus-stopped-nudge-" t))
         (instance (magnus-instances-create directory "stopped-codex" 'codex))
         (magnus-coord-nudge-debounce nil))
    (unwind-protect
        (progn
          (should-not
           (magnus-coord-nudge-agent
            instance "Please tell @bold-wren later" "Magnus"))
          (let ((log (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name ".magnus-coord.md" directory))
                       (buffer-string))))
            (should (string-match-p "Undelivered nudge" log))
            (should (string-match-p "(at) bold-wren" log))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-legacy-running-check-needs-no-process-module-call ()
  (let* ((instance (magnus-instances-create "/tmp" "legacy-live" 'claude))
         (buffer (generate-new-buffer " *magnus-legacy-live*"))
         (process (make-pipe-process
                   :name (generate-new-buffer-name "magnus-legacy-live")
                   :buffer buffer :noquery t)))
    (magnus-instances-update instance :buffer buffer :status 'running)
    (unwind-protect
        (should (magnus-coord--instance-running-p instance))
      (delete-process process)
      (kill-buffer buffer))))

(ert-deftest magnus-health-check-supports-codex-tui-buffers ()
  (let* ((magnus-health--state (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "healthy-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (buffer (car terminal)))
    (with-current-buffer buffer
      (insert "streamed Codex output"))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-process-running-p)
                   (lambda (candidate)
                     (magnus-provider-call candidate 'running-p))))
          (magnus-health--check-instance instance)
          (should (eq (magnus-health-get instance) 'ok)))
      (magnus-test--delete-terminal terminal))))

(provide 'magnus-provider-tests)
;;; magnus-provider-tests.el ends here
