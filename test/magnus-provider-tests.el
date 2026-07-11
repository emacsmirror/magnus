;;; magnus-provider-tests.el --- Provider compatibility tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus)
(require 'magnus-instances)
(require 'magnus-provider)
(require 'magnus-provider-codex)
(require 'magnus-coord)
(require 'magnus-health)

(defun magnus-test--codex-process (instance)
  "Create a live inert App Server stand-in for INSTANCE."
  (let ((process
         (make-pipe-process
          :name (generate-new-buffer-name
                 (format "magnus-test-%s" (magnus-instance-id instance)))
          :noquery t)))
    (process-put process 'magnus-codex-instance instance)
    (process-put process 'magnus-codex-pending
                 (make-hash-table :test #'equal))
    (process-put process 'magnus-codex-approvals
                 (make-hash-table :test #'equal))
    (process-put process 'magnus-codex-turn-starting nil)
    (magnus-codex--register-connection instance process)
    process))

(defun magnus-test--codex-tui (instance)
  "Create a live terminal-process stand-in for INSTANCE."
  (let* ((buffer (generate-new-buffer
                  (format " *magnus-test-tui:%s*"
                          (magnus-instance-id instance))))
         (process (make-pipe-process
                   :name (generate-new-buffer-name "magnus-test-tui")
                   :buffer buffer
                   :noquery t)))
    (magnus-instances-update instance :buffer buffer :status 'running)
    (cons buffer process)))

(defun magnus-test--server-websocket-frame (text)
  "Encode TEXT as an unmasked, final server WebSocket text frame."
  (let* ((payload (encode-coding-string text 'utf-8))
         (length (length payload)))
    (concat
     (cond
      ((< length 126)
       (magnus-codex--byte-string 129 length))
      ((< length 65536)
       (concat (magnus-codex--byte-string 129 126)
               (magnus-codex--integer-bytes length 2)))
      (t
       (concat (magnus-codex--byte-string 129 127)
               (magnus-codex--integer-bytes length 8))))
     payload)))

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

(ert-deftest magnus-codex-parses-current-app-server-events ()
  (let ((message
         (magnus-codex--parse-line
          "{\"jsonrpc\":\"2.0\",\"method\":\"item/agentMessage/delta\",\"params\":{\"delta\":\"hi\"}}")))
    (should (equal (alist-get 'method message) "item/agentMessage/delta"))
    (should (equal (alist-get 'delta (alist-get 'params message)) "hi")))
  (should (equal (magnus-codex--input "hello")
                 '[((type . "text") (text . "hello"))]))
  (should (equal (magnus-codex--thread-status-name
                  '((type . "active") (activeFlags . [])))
                 "active"))
  (let* ((instance (magnus-instances-create "/tmp" "solar-fox" 'codex))
         (instructions (magnus-codex--instructions instance)))
    (should (string-match-p "solar-fox" instructions))
    (should (string-match-p "\\[thinking\\]" instructions))
    (should (string-match-p "visible working notebook" instructions))
    (should (string-match-p "Discoveries" instructions))
    (should (string-match-p "future-you" instructions))))

(ert-deftest magnus-codex-parses-split-extended-websocket-frame ()
  (let* ((instance (magnus-instances-create "/tmp" "framed-codex" 'codex))
         (process (magnus-test--codex-process instance))
         (padding (make-string 180 ?x))
         (frame (magnus-test--server-websocket-frame
                 (magnus-codex--json
                  `((id . 1) (result . ((padding . ,padding)))))))
         result)
    (unwind-protect
        (progn
          (process-put process 'magnus-codex-frame-buffer
                       (magnus-codex--byte-string))
          (puthash 1
                   (lambda (value error)
                     (should-not error)
                     (setq result value))
                   (process-get process 'magnus-codex-pending))
          (magnus-codex--consume-frames process (substring frame 0 11))
          (should-not result)
          (magnus-codex--consume-frames process (substring frame 11))
          (should (equal (alist-get 'padding result) padding))
          (should (string-empty-p
                   (process-get process 'magnus-codex-frame-buffer))))
      (delete-process process))))

(ert-deftest magnus-codex-validates-websocket-upgrade-accept ()
  (let* ((instance (magnus-instances-create "/tmp" "upgrade-codex" 'codex))
         (process (magnus-test--codex-process instance)))
    (unwind-protect
        (progn
          (process-put process 'magnus-codex-websocket-key
                       "dGhlIHNhbXBsZSBub25jZQ==")
          (should (equal (magnus-codex--websocket-accept process)
                         "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")))
      (delete-process process))))

(ert-deftest magnus-codex-client-websocket-frames-are-unibyte ()
  (let ((frame (magnus-codex--websocket-frame
                "A deliberately ASCII JSON payload long enough to mask")))
    (should-not (multibyte-string-p frame))
    (should (= (aref frame 0) 129))
    (should-not (zerop (logand (aref frame 1) 128)))))

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

(ert-deftest magnus-codex-new-thread-onboarding-is-durable-user-input ()
  (let* ((instance (magnus-instances-create "/tmp" "named-codex" 'codex))
         (prompt (magnus-codex--onboarding-prompt instance "Fix the parser")))
    (should (string-match-p "You are named-codex" prompt))
    (should (string-match-p "Initial task from the user" prompt))
    (should (string-match-p "Fix the parser" prompt))
    (should (string-match-p "\\[thinking\\]" prompt))
    (magnus-instances-update instance :session-id "observer-owned-thread")
    (let ((command (magnus-codex--tui-command instance prompt)))
      (should (string-match-p "exec codex resume" command))
      (should (string-match-p "observer-owned-thread" command))
      (should-not (string-match-p "developer_instructions=" command)))))

(ert-deftest magnus-codex-command-approval-keeps-v2-response-shape ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "codex-user" 'codex))
         (request-id 7)
         (process (magnus-test--codex-process instance))
         sent)
    (unwind-protect
        (progn
          (puthash request-id
                   (list :instance instance :process process
                         :method "item/commandExecution/requestApproval")
                   (magnus-codex--approval-table process))
          (cl-letf (((symbol-function 'magnus-codex--send-object)
                     (lambda (_process object) (setq sent object)))
                    ((symbol-function 'magnus-codex--insert)
                     (lambda (&rest _arguments) nil)))
            (magnus-codex-respond-approval instance request-id "accept"))
          (should (equal (alist-get 'result sent)
                         '((decision . "accept")))))
      (delete-process process))))

(ert-deftest magnus-codex-command-approval-allows-policy-amendment ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "policy-codex" 'codex))
         (process (magnus-test--codex-process instance))
         (decision
          '((acceptWithExecpolicyAmendment
             . ((execpolicy_amendment . ("git" "status"))))))
         sent)
    (unwind-protect
        (progn
          (puthash 12
                   (list :instance instance :process process
                         :method "item/commandExecution/requestApproval")
                   (magnus-codex--approval-table process))
          (cl-letf (((symbol-function 'magnus-codex--send-object)
                     (lambda (_process object) (setq sent object)))
                    ((symbol-function 'magnus-codex--insert)
                     (lambda (&rest _arguments) nil)))
            (magnus-codex-respond-approval instance 12 decision))
          (should (equal (alist-get 'result sent)
                         `((decision . ,decision)))))
      (delete-process process))))

(ert-deftest magnus-codex-permission-approval-requires-profile ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "codex-user" 'codex))
         (request-id 8)
         (process (magnus-test--codex-process instance)))
    (unwind-protect
        (progn
          (puthash request-id
                   (list :instance instance :process process
                         :method "item/permissions/requestApproval")
                   (magnus-codex--approval-table process))
          (should-error
           (magnus-codex-respond-approval instance request-id "accept")
           :type 'user-error))
      (delete-process process))))

(ert-deftest magnus-codex-rejects-unknown-command-approval-decision ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "codex-user" 'codex))
         (request-id 9)
         (process (magnus-test--codex-process instance)))
    (unwind-protect
        (progn
          (puthash request-id
                   (list :instance instance :process process
                         :method "item/fileChange/requestApproval")
                   (magnus-codex--approval-table process))
          (should-error
           (magnus-codex-respond-approval
            instance request-id "approve-everything")
           :type 'user-error))
      (delete-process process))))

(ert-deftest magnus-codex-approval-ids-are-scoped-per-process ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (first (magnus-instances-create "/tmp" "first-codex" 'codex))
         (second (magnus-instances-create "/tmp" "second-codex" 'codex))
         (first-process (magnus-test--codex-process first))
         (second-process (magnus-test--codex-process second))
         sent)
    (magnus-instances-update first :session-id "thread-first")
    (magnus-instances-update second :session-id "thread-second")
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--insert)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'magnus-codex--send-object)
                   (lambda (process object)
                     (push (cons process object) sent))))
          (magnus-codex--handle-server-request
           first-process 3 "item/commandExecution/requestApproval"
           '((threadId . "thread-first") (command . "git status")))
          (magnus-codex--handle-server-request
           second-process 3 "item/fileChange/requestApproval"
           '((threadId . "thread-second") (reason . "edit file")))
          (should (= 1 (hash-table-count
                        (magnus-codex--approval-table first-process))))
          (should (= 1 (hash-table-count
                        (magnus-codex--approval-table second-process))))
          (magnus-codex-respond-approval first 3 "accept")
          (should (= 0 (hash-table-count
                        (magnus-codex--approval-table first-process))))
          (should (= 1 (hash-table-count
                        (magnus-codex--approval-table second-process))))
          (magnus-codex-respond-approval second 3 "decline")
          (should (= 0 (hash-table-count
                        (magnus-codex--approval-table second-process))))
          (should (= 2 (length sent))))
      (delete-process first-process)
      (delete-process second-process))))

(ert-deftest magnus-codex-stale-approval-cannot-answer-reused-id ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (magnus-codex--retired-approval-ids
          (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "reconnected" 'codex))
         (first-process (magnus-test--codex-process instance))
         second-process old-token current-token sent)
    (magnus-instances-update instance :session-id "thread-reconnected")
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--insert)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'magnus-codex--send-object)
                   (lambda (_process object) (setq sent object))))
          (magnus-codex--handle-server-request
           first-process 3 "item/commandExecution/requestApproval"
           '((threadId . "thread-reconnected") (command . "old")))
          (setq old-token (magnus-codex-approval-token instance 3))
          (delete-process first-process)
          (setq second-process (magnus-test--codex-process instance))
          (magnus-codex--handle-server-request
           second-process 3 "item/commandExecution/requestApproval"
           '((threadId . "thread-reconnected") (command . "new")))
          (setq current-token (magnus-codex-approval-token instance 3))
          (should-error
           (magnus-codex-respond-approval instance 3 "accept")
           :type 'user-error)
          (should-error
           (magnus-codex-respond-approval instance 3 "accept" old-token)
           :type 'user-error)
          (magnus-codex-respond-approval
           instance 3 "accept" current-token)
          (should (equal (alist-get 'result sent)
                         '((decision . "accept")))))
      (when (process-live-p first-process) (delete-process first-process))
      (when (and second-process (process-live-p second-process))
        (delete-process second-process)))))

(ert-deftest magnus-codex-approval-summary-accepts-argv-arrays ()
  (should
   (equal
    (magnus-codex--approval-summary
     '((command . ("git" "status" "--short")) (cwd . "/tmp")))
    "git status --short\ncwd: /tmp")))

(ert-deftest magnus-codex-tui-command-resumes-managed-thread ()
  (let* ((magnus-codex-remote "unix://")
         (instance (magnus-instances-create
                    "/tmp/project with spaces" "tui-codex" 'codex)))
    (magnus-instances-update instance :session-id "thread-1")
    (let ((command (magnus-codex--tui-command
                    instance "read this next")))
      (should (string-match-p "codex resume" command))
      (should (string-match-p
               (regexp-quote
                (format "--remote %s" (shell-quote-argument "unix://")))
               command))
      (should (string-match-p "thread-1" command))
      (should (string-match-p
               (regexp-quote (shell-quote-argument "read this next"))
               command)))))

(ert-deftest magnus-codex-messages-use-tui-as-sole-writer ()
  (let* ((magnus-codex--input-queues (make-hash-table :test #'equal))
         (magnus-codex--input-busy (make-hash-table :test #'equal))
         (magnus-codex--input-retry (make-hash-table :test #'equal))
         (magnus-codex--tui-ready (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "tui-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         sent return-sent)
    (puthash (magnus-instance-id instance) t magnus-codex--tui-ready)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--request)
                  (lambda (&rest _arguments)
                     (ert-fail "observer attempted an interactive request")))
                  ((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p) (setq sent text)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-sent t)))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest arguments)
                     (apply function arguments))))
          (magnus-codex-send instance "read this next")
          (should (equal sent "read this next"))
          (should return-sent))
      (delete-process (cdr terminal))
      (kill-buffer (car terminal)))))

(ert-deftest magnus-codex-concurrent-tui-messages-are-serialized ()
  (let* ((magnus-codex--input-queues (make-hash-table :test #'equal))
         (magnus-codex--input-busy (make-hash-table :test #'equal))
         (magnus-codex--input-retry (make-hash-table :test #'equal))
         (magnus-codex--tui-ready (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "queued-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         timers events)
    (puthash (magnus-instance-id instance) t magnus-codex--tui-ready)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p)
                     (setq events (append events (list text)))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq events (append events '(return)))))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq timers
                           (append timers (list (cons function arguments)))))))
          (magnus-codex-send instance "first")
          (magnus-codex-send instance "second")
          (should (equal events '("first" return)))
          (while timers
            (let ((timer (pop timers)))
              (apply (car timer) (cdr timer))))
          (should (equal events '("first" return "second" return))))
      (delete-process (cdr terminal))
      (kill-buffer (car terminal)))))

(ert-deftest magnus-codex-remote-endpoint-is-unix-only ()
  (let ((magnus-codex-remote "ws://127.0.0.1:9000"))
    (should-error (magnus-codex--remote-socket) :type 'user-error))
  (let ((magnus-codex-remote "unix:///tmp/custom-codex.sock"))
    (should (equal (magnus-codex--remote-socket)
                   "/tmp/custom-codex.sock"))))

(ert-deftest magnus-codex-stop-interrupts-active-daemon-turn ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (magnus-codex--active-turns (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "interruptible" 'codex))
         (observer (magnus-test--codex-process instance))
         (terminal (magnus-test--codex-tui instance))
         requested)
    (magnus-instances-update instance :session-id "thread-stop")
    (puthash (magnus-instance-id instance) "turn-stop"
             magnus-codex--active-turns)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--request)
                   (lambda (_process method params callback)
                     (setq requested (cons method params))
                     (funcall callback '() nil)))
                  ((symbol-function 'run-with-timer)
                   (lambda (&rest _arguments) nil)))
          (magnus-codex-stop instance)
          (should (equal (car requested) "turn/interrupt"))
          (should (equal (alist-get 'threadId (cdr requested)) "thread-stop"))
          (should (equal (alist-get 'turnId (cdr requested)) "turn-stop"))
          (should (eq (magnus-instance-status instance) 'stopped)))
      (when (process-live-p observer) (delete-process observer))
      (when (process-live-p (cdr terminal)) (delete-process (cdr terminal)))
      (when (buffer-live-p (car terminal)) (kill-buffer (car terminal))))))

(ert-deftest magnus-codex-stop-falls-back-to-tui-after-observer-loss ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "fallback-stop" 'codex))
         (terminal (magnus-test--codex-tui instance))
         sent-key)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-key)
                   (lambda (key &rest _arguments) (setq sent-key key)))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest arguments)
                     (apply function arguments))))
          (magnus-codex-stop instance)
          (should (equal sent-key "C-c"))
          (should (eq (magnus-instance-status instance) 'stopped))
          (should-not (magnus-instance-buffer instance)))
      (when (process-live-p (cdr terminal)) (delete-process (cdr terminal)))
      (when (buffer-live-p (car terminal)) (kill-buffer (car terminal))))))

(ert-deftest magnus-codex-stop-falls-back-before-turn-id-arrives ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (magnus-codex--active-turns (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "early-stop" 'codex))
         (observer (magnus-test--codex-process instance))
         (terminal (magnus-test--codex-tui instance))
         sent-key)
    (magnus-instances-update instance :session-id "thread-early")
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-key)
                   (lambda (key &rest _arguments) (setq sent-key key)))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest arguments)
                     (apply function arguments))))
          (magnus-codex-stop instance)
          (should (equal sent-key "C-c"))
          (should (eq (magnus-instance-status instance) 'stopped)))
      (when (process-live-p observer) (delete-process observer))
      (when (process-live-p (cdr terminal)) (delete-process (cdr terminal)))
      (when (buffer-live-p (car terminal)) (kill-buffer (car terminal))))))

(ert-deftest magnus-codex-running-state-follows-tui-not-observer ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "tui-codex" 'codex))
         (observer (magnus-test--codex-process instance))
         terminal)
    (unwind-protect
        (progn
          (should-not (magnus-codex-running-p instance))
          (setq terminal (magnus-test--codex-tui instance))
          (should (magnus-codex-running-p instance))
          (delete-process observer)
          (magnus-codex--sentinel observer "exited")
          (should (magnus-codex-running-p instance))
          (should (eq (magnus-instance-status instance) 'running)))
      (when terminal
        (delete-process (cdr terminal))
        (kill-buffer (car terminal)))
      (when (process-live-p observer)
        (delete-process observer)))))

(ert-deftest magnus-codex-stale-tui-exit-preserves-replacement ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "replacement" 'codex))
         (old-observer (magnus-test--codex-process instance))
         (old-terminal (magnus-test--codex-tui instance))
         new-observer new-terminal sentinel)
    (unwind-protect
        (progn
          (magnus-codex--setup-tui-sentinel instance (car old-terminal))
          (setq sentinel (process-sentinel (cdr old-terminal)))
          (setq new-observer (magnus-test--codex-process instance))
          (setq new-terminal (magnus-test--codex-tui instance))
          (set-process-sentinel (cdr old-terminal) nil)
          (delete-process (cdr old-terminal))
          (funcall sentinel (cdr old-terminal) "exited")
          (should (process-live-p new-observer))
          (should (magnus-codex-running-p instance))
          (should (eq (magnus-instance-status instance) 'running)))
      (dolist (process (delq nil
                             (list old-observer new-observer
                                   (cdr old-terminal)
                                   (and new-terminal (cdr new-terminal)))))
        (when (process-live-p process) (delete-process process)))
      (dolist (buffer (delq nil
                            (list (car old-terminal)
                                  (and new-terminal (car new-terminal)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest magnus-codex-stale-observer-exit-preserves-replacement-turn ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (magnus-codex--active-turns (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "observer-race" 'codex))
         (old-observer (magnus-test--codex-process instance))
         new-observer)
    (unwind-protect
        (progn
          (delete-process old-observer)
          (setq new-observer (magnus-test--codex-process instance))
          (puthash (magnus-instance-id instance) "replacement-turn"
                   magnus-codex--active-turns)
          (magnus-codex--sentinel old-observer "exited")
          (should (eq (magnus-codex--connection instance) new-observer))
          (should (equal
                   (gethash (magnus-instance-id instance)
                            magnus-codex--active-turns)
                   "replacement-turn")))
      (when (process-live-p old-observer) (delete-process old-observer))
      (when (and new-observer (process-live-p new-observer))
        (delete-process new-observer)))))

(ert-deftest magnus-codex-existing-thread-goes-straight-to-tui ()
  (let* ((instance (magnus-instances-create "/tmp" "handoff-codex" 'codex))
         (process (magnus-test--codex-process instance))
         spawned notified methods)
    (magnus-instances-update instance :session-id "thread-existing")
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--request)
                  (lambda (_process method params callback)
                     (push method methods)
                     (should params)
                     (funcall callback
                              (if (equal method "initialize")
                                  '((userAgent . "test"))
                                '((thread . ((id . "thread-existing")))))
                              nil)))
                  ((symbol-function 'magnus-codex--notify)
                   (lambda (_process method &optional _params)
                     (setq notified method)))
                  ((symbol-function 'magnus-codex--spawn-tui)
                   (lambda (candidate message)
                     (setq spawned (list candidate message)))))
          (magnus-codex--initialize process instance "first task")
          (should (equal (magnus-instance-session-id instance)
                         "thread-existing"))
          (should (equal notified "initialized"))
          (should (equal (nreverse methods)
                         '("initialize" "thread/resume")))
          (should (eq (car spawned) instance))
          (should (equal (cadr spawned) "first task")))
      (delete-process process))))

(ert-deftest magnus-codex-new-threads-are-owned-by-their-observers ()
  (let* ((first (magnus-instances-create "/tmp" "first-tui" 'codex))
         (second (magnus-instances-create "/tmp" "second-tui" 'codex))
         (first-process (magnus-test--codex-process first))
         (second-process (magnus-test--codex-process second))
         requests spawned)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--request)
                   (lambda (process method params callback)
                     (push (list process method params callback) requests)))
                  ((symbol-function 'magnus-codex--spawn-tui)
                   (lambda (instance message)
                     (setq spawned
                           (append spawned
                                   (list (list instance message)))))))
          (magnus-codex--create-observer-thread first-process first "first")
          (magnus-codex--create-observer-thread second-process second "second")
          (should (= (length requests) 2))
          (let* ((second-request (car requests))
                 (first-request (cadr requests)))
            (should (eq (car second-request) second-process))
            (should (equal (cadr second-request) "thread/start"))
            (funcall (nth 3 second-request)
                     '((thread . ((id . "thread-second")))) nil)
            (funcall (nth 3 first-request)
                     '((thread . ((id . "thread-first")))) nil))
          (should (equal (magnus-instance-session-id first) "thread-first"))
          (should (equal (magnus-instance-session-id second) "thread-second"))
          (should (equal (mapcar #'car spawned) (list second first))))
      (delete-process first-process)
      (delete-process second-process))))

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

(ert-deftest magnus-health-check-supports-codex-output-buffers ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (magnus-health--state (make-hash-table :test #'equal))
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
      (delete-process (cdr terminal))
      (kill-buffer buffer))))

(provide 'magnus-provider-tests)
;;; magnus-provider-tests.el ends here
