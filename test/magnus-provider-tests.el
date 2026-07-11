;;; magnus-provider-tests.el --- Provider compatibility tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
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
    (process-put process 'magnus-codex-input-queue nil)
    (process-put process 'magnus-codex-turn-starting nil)
    (puthash (magnus-instance-id instance) process
             magnus-codex--connections)
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
  (let* ((payload (encode-coding-string text 'utf-8 t))
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
         (prompt (magnus-codex--onboarding-prompt instance "Fix the parser"))
         (command (magnus-codex--tui-command instance prompt)))
    (should (string-match-p "You are named-codex" prompt))
    (should (string-match-p "Initial task from the user" prompt))
    (should (string-match-p "Fix the parser" prompt))
    (should (string-match-p "\\[thinking\\]" prompt))
    (should (string-match-p "exec codex --remote" command))
    (should-not (string-match-p "codex resume" command))
    (should-not (string-match-p "developer_instructions=" command))))

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
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--insert)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'magnus-codex--send-object)
                   (lambda (process object)
                     (push (cons process object) sent))))
          (magnus-codex--handle-server-request
           first-process 3 "item/commandExecution/requestApproval"
           '((command . "git status")))
          (magnus-codex--handle-server-request
           second-process 3 "item/fileChange/requestApproval"
           '((reason . "edit file")))
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
  (let* ((instance (magnus-instances-create "/tmp" "tui-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         sent return-sent)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--request)
                   (lambda (&rest _arguments)
                     (ert-fail "observer attempted an interactive request")))
                  ((symbol-function 'vterm-send-string)
                   (lambda (text) (setq sent text)))
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

(ert-deftest magnus-codex-open-thread-hands-session-to-tui ()
  (let* ((instance (magnus-instances-create "/tmp" "handoff-codex" 'codex))
         (process (magnus-test--codex-process instance))
         spawned)
    (magnus-instances-update instance :session-id "thread-existing")
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--request)
                   (lambda (_process method params callback)
                     (should (equal method "thread/resume"))
                     (should (equal (alist-get 'threadId params)
                                    "thread-existing"))
                     (funcall callback
                              '((thread . ((id . "thread-existing")))) nil)))
                  ((symbol-function 'magnus-codex--spawn-tui)
                   (lambda (candidate message)
                     (setq spawned (list candidate message)))))
          (magnus-codex--open-thread process instance "first task")
          (should (equal (magnus-instance-session-id instance)
                         "thread-existing"))
          (should (eq (car spawned) instance))
          (should (equal (cadr spawned) "first task")))
      (delete-process process))))

(ert-deftest magnus-codex-new-tui-handoffs-are-serialized ()
  (let* ((magnus-codex--new-thread-owner nil)
         (magnus-codex--new-thread-queue nil)
         (first (magnus-instances-create "/tmp" "first-tui" 'codex))
         (second (magnus-instances-create "/tmp" "second-tui" 'codex))
         (first-process (magnus-test--codex-process first))
         (second-process (magnus-test--codex-process second))
         spawned)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--spawn-tui)
                   (lambda (instance message)
                     (setq spawned
                           (append spawned
                                   (list (list instance message)))))))
          (magnus-codex--start-new-thread first-process first "first")
          (magnus-codex--start-new-thread second-process second "second")
          (should (equal (mapcar #'car spawned) (list first)))
          (should (= (length magnus-codex--new-thread-queue) 1))
          (magnus-codex--handle-notification
           first-process "thread/started"
           '((thread . ((id . "thread-first") (cwd . "/tmp")))))
          (should (equal (magnus-instance-session-id first) "thread-first"))
          (should (equal (mapcar #'car spawned) (list first second)))
          (should (equal magnus-codex--new-thread-owner
                         (magnus-instance-id second))))
      (delete-process first-process)
      (delete-process second-process))))

(ert-deftest magnus-coord-stopped-agent-nudge-is-logged-not-signaled ()
  (let* ((directory (make-temp-file "magnus-stopped-nudge-" t))
         (instance (magnus-instances-create directory "stopped-codex" 'codex))
         (magnus-coord-nudge-debounce nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-process-running-p)
                   (lambda (_instance) nil)))
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
