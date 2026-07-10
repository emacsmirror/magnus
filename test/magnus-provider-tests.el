;;; magnus-provider-tests.el --- Provider compatibility tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-instances)
(require 'magnus-provider)
(require 'magnus-provider-codex)

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
  (let* ((instance (magnus-instances-create "/tmp" "codex-user" 'codex))
         (request-id 7)
         sent)
    (puthash request-id
             (list :instance instance :process 'fake-process
                   :method "item/commandExecution/requestApproval")
             magnus-codex--approvals)
    (cl-letf (((symbol-function 'magnus-codex--send-object)
               (lambda (_process object) (setq sent object)))
              ((symbol-function 'magnus-codex--insert)
               (lambda (&rest _arguments) nil)))
      (magnus-codex-respond-approval instance request-id "accept"))
    (should (equal (alist-get 'result sent)
                   '((decision . "accept"))))))

(ert-deftest magnus-codex-permission-approval-requires-profile ()
  (let* ((instance (magnus-instances-create "/tmp" "codex-user" 'codex))
         (request-id 8))
    (puthash request-id
             (list :instance instance :process 'fake-process
                   :method "item/permissions/requestApproval")
             magnus-codex--approvals)
    (should-error
     (magnus-codex-respond-approval instance request-id "accept")
     :type 'user-error)))

(ert-deftest magnus-codex-rejects-unknown-command-approval-decision ()
  (let* ((instance (magnus-instances-create "/tmp" "codex-user" 'codex))
         (request-id 9))
    (puthash request-id
             (list :instance instance :process 'fake-process
                   :method "item/fileChange/requestApproval")
             magnus-codex--approvals)
    (should-error
     (magnus-codex-respond-approval instance request-id "approve-everything")
     :type 'user-error)))

(provide 'magnus-provider-tests)
;;; magnus-provider-tests.el ends here
