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

(ert-deftest magnus-codex-active-turn-messages-queue-by-default ()
  (let* ((magnus-codex--connections (make-hash-table :test #'equal))
         (magnus-codex--active-turns (make-hash-table :test #'equal))
         (magnus-codex-active-turn-delivery 'queue)
         (instance (magnus-instances-create "/tmp" "queued-codex" 'codex))
         (process (magnus-test--codex-process instance))
         requests)
    (magnus-instances-update instance :session-id "thread-1")
    (puthash (magnus-instance-id instance) "turn-1"
             magnus-codex--active-turns)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--insert)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'magnus-codex--request)
                   (lambda (_process method params _callback)
                     (push (cons method params) requests)
                     1)))
          (magnus-codex-send instance "read this next")
          (should-not requests)
          (should (equal (process-get process 'magnus-codex-input-queue)
                         '("read this next")))
          (magnus-codex--handle-notification
           process "turn/completed" '((turn . ((id . "turn-1")))))
          (should (equal (caar requests) "turn/start"))
          (should-not (process-get process 'magnus-codex-input-queue)))
      (delete-process process))))

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
         (process (magnus-test--codex-process instance))
         (buffer (generate-new-buffer " *healthy-codex*")))
    (magnus-instances-update instance :status 'running :buffer buffer)
    (with-current-buffer buffer
      (insert "streamed Codex output"))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-process-running-p)
                   (lambda (candidate)
                     (magnus-provider-call candidate 'running-p))))
          (magnus-health--check-instance instance)
          (should (eq (magnus-health-get instance) 'ok)))
      (kill-buffer buffer)
      (delete-process process))))

(provide 'magnus-provider-tests)
;;; magnus-provider-tests.el ends here
