;;; magnus-headless-tests.el --- Headless provider tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-headless)
(require 'magnus-provider-claude)
(require 'magnus-provider-codex)

(defun magnus-test-headless--decoder (event _request)
  "Normalize one fake provider EVENT."
  (let ((type (alist-get 'type event)))
    (pcase type
      ("session"
       (list :type type :session-id (alist-get 'session_id event)))
      ("result"
       (list :type type :structured-result (alist-get 'value event)
             :terminal t))
      ("terminal"
       (list :type type :terminal t))
      ("provider-error"
       (list :type type :error (list :message (alist-get 'message event))
             :terminal t))
      (_ (list :type (or type "unknown") :raw event)))))

(defun magnus-test-headless--spec (request)
  "Build a fake process specification from REQUEST."
  (let ((spec
         (list :command
               (list (or (executable-find "sh") shell-file-name)
                     shell-command-switch
                     (plist-get request :fixture-command))
               :decoder #'magnus-test-headless--decoder
               :session-id (plist-get request :fixture-session-id))))
    (when (plist-member request :fixture-success-requires)
      (setq spec
            (plist-put spec :success-requires
                       (plist-get request :fixture-success-requires))))
    (when (plist-member request :fixture-environment)
      (setq spec
            (plist-put spec :environment
                       (plist-get request :fixture-environment))))
    spec))

(magnus-provider-register
 'magnus-test-headless
 '((headless-review-spec . magnus-test-headless--spec)))

(magnus-provider-register
 'magnus-test-headless-generic
 '((headless-spec . magnus-test-headless--spec)))

(defun magnus-test-headless--request (command)
  "Return a fake provider request executing shell COMMAND."
  (list :directory default-directory
        :prompt "Exercise the fake provider"
        :fixture-command command))

(defun magnus-test-headless--emit (line &optional stderr no-newline)
  "Return shell source that emits LINE.
When STDERR is non-nil write to stderr.  NO-NEWLINE omits the trailing newline."
  (format "printf '%s' %s%s"
          (if no-newline "%s" "%s\\n")
          (shell-quote-argument line)
          (if stderr " >&2" "")))

(defun magnus-test-headless--wait (predicate)
  "Wait for PREDICATE with a bounded timeout."
  (let ((deadline (+ (float-time) 3.0)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.02)
      ;; `sleep-for' also gives zero-delay activation/finalization timers an
      ;; opportunity to run when the fixture exited before the first accept.
      (sleep-for 0.001))
    (unless (funcall predicate)
      (ert-fail "Timed out waiting for fake headless provider"))))

(ert-deftest magnus-headless-agent-never-uses-legacy-review-adapter ()
  (let ((request (magnus-test-headless--request "exit 0")))
    (setq request (plist-put request :purpose 'agent))
    (should-error
     (magnus-headless-start 'magnus-test-headless request)
     :type 'user-error)))

(ert-deftest magnus-headless-agent-can-require-terminal-only ()
  (let* ((terminal "{\"type\":\"terminal\"}")
         (request
          (append
           (magnus-test-headless--request
            (magnus-test-headless--emit terminal))
           '(:purpose agent :fixture-success-requires (terminal))))
         completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless-generic request
                 (list :on-complete
                       (lambda (_process result)
                         (setq completion result)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (plist-get completion :success-p))
          (should (equal (plist-get completion :success-requires)
                         '(terminal)))
          (should-not
           (plist-get completion :structured-result-present-p)))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-review-cannot-weaken-success-requirements ()
  (let ((request
         (append (magnus-test-headless--request "exit 0")
                 '(:purpose review
                   :fixture-success-requires (terminal)))))
    (should-error
     (magnus-headless-start 'magnus-test-headless-generic request))))

(ert-deftest magnus-headless-rejects-unknown-success-requirement ()
  (let ((request
         (append (magnus-test-headless--request "exit 0")
                 '(:purpose agent
                   :fixture-success-requires (terminal magic)))))
    (should-error
     (magnus-headless-start 'magnus-test-headless-generic request))))

(ert-deftest magnus-headless-associates-optional-display-buffer ()
  (let* ((line "{\"type\":\"result\",\"value\":{\"ok\":true}}")
         (buffer (generate-new-buffer " *magnus-headless-display*"))
         (request
          (append
           (magnus-test-headless--request
            (magnus-test-headless--emit line))
           (list :buffer buffer)))
         completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless-generic request
                 (list :on-complete
                       (lambda (_process result)
                         (setq completion result)))))
          (should (eq (process-buffer process) buffer))
          (should (eq (get-buffer-process buffer) process))
          (magnus-test-headless--wait (lambda () completion))
          (should (plist-get completion :success-p)))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magnus-headless-closes-stdin-after-launch ()
  (let* ((result "{\"type\":\"result\",\"value\":{\"ok\":true}}")
         ;; This fixture cannot produce its terminal event until the parent
         ;; closes stdin.  It models `codex exec PROMPT', which otherwise waits
         ;; forever to append Emacs's open input pipe as extra prompt context.
         (command
          (format "cat >/dev/null; %s"
                  (magnus-test-headless--emit result)))
         completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless
                 (magnus-test-headless--request command)
                 (list :on-complete
                       (lambda (_process value) (setq completion value)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (plist-get completion :success-p)))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-overlays-bindings-after-provider-filtering ()
  (let* ((original-environment
          '("CLAUDECODE=nested" "KEEP=yes"
            "MAGNUS_TEST_ID=old-value"))
         (process-environment (copy-sequence original-environment))
         (provider-environment
          (cl-remove-if
           (lambda (entry) (string-prefix-p "CLAUDECODE=" entry))
           process-environment))
         (command
          (concat
           "printf '{\"type\":\"result\",\"value\":"
           "\"%s|%s|%s\"}\\n' "
           "\"${CLAUDECODE-unset}\" "
           "\"$MAGNUS_TEST_ID\" "
           "\"$MAGNUS_TEST_NAME\""))
         (request
          (append
           (magnus-test-headless--request command)
           (list :purpose 'agent
                 :fixture-environment provider-environment
                 :environment-bindings
                 '("MAGNUS_TEST_ID=new-value"
                   "MAGNUS_TEST_NAME=fixture"))))
         completion
         process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless-generic request
                 (list :on-complete
                       (lambda (_process result)
                         (setq completion result)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (plist-get completion :success-p))
          (should (equal (plist-get completion :structured-result)
                         "unset|new-value|fixture"))
          (should (equal process-environment original-environment)))
      (when process
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-review-without-bindings-keeps-provider-scope ()
  (let* ((original-environment '("CALLER_ONLY=yes"))
         (process-environment (copy-sequence original-environment))
         (command
          (concat
           "printf '{\"type\":\"result\",\"value\":\"%s|%s\"}\\n' "
           "\"$REVIEW_ONLY\" "
           "\"${MAGNUS_TEST_ID-unset}\""))
         (request
          (append
           (magnus-test-headless--request command)
           '(:fixture-environment ("REVIEW_ONLY=yes"))))
         completion
         process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless request
                 (list :on-complete
                       (lambda (_process result)
                         (setq completion result)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (plist-get completion :success-p))
          (should (equal (plist-get completion :structured-result)
                         "yes|unset"))
          (should (equal process-environment original-environment)))
      (when process
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-frames-partial-jsonl ()
  (let* ((first "{\"type\":\"sess")
         (second "ion\",\"session_id\":\"thread-1\"}")
         (result "{\"type\":\"result\",\"value\":{\"ok\":true}}")
         (command
          (mapconcat
           #'identity
           (list (magnus-test-headless--emit first nil t)
                 "sleep 0.02"
                 (magnus-test-headless--emit second)
                 (magnus-test-headless--emit result))
           "; "))
         raw events completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless
                 (magnus-test-headless--request command)
                 (list :on-raw-event
                       (lambda (_process line) (push line raw))
                       :on-event
                       (lambda (_process event) (push event events))
                       :on-complete
                       (lambda (_process result) (setq completion result)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (equal (nreverse raw)
                         (list (concat first second) result)))
          (should (equal (mapcar (lambda (event) (plist-get event :type))
                                 (nreverse events))
                         '("session" "result")))
          (should (plist-get completion :success-p)))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-preserves-malformed-lines-and-continues ()
  (let* ((session "{\"type\":\"session\",\"session_id\":\"thread-2\"}")
         (malformed "{definitely-not-json")
         (result "{\"type\":\"result\",\"value\":{\"ok\":true}}")
         (command
          (mapconcat #'magnus-test-headless--emit
                     (list session malformed result) "; "))
         raw errors completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless
                 (magnus-test-headless--request command)
                 (list :on-raw-event
                       (lambda (_process line) (push line raw))
                       :on-error
                       (lambda (_process error) (push error errors))
                       :on-complete
                       (lambda (_process value) (setq completion value)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (equal (nreverse raw) (list session malformed result)))
          (should (equal (plist-get completion :session-id) "thread-2"))
          (should (plist-get completion :structured-result-present-p))
          (should-not (plist-get completion :success-p))
          (should (eq (plist-get (car errors) :kind) 'jsonl-decode-error)))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-captures-provider-session ()
  (let* ((session "{\"type\":\"session\",\"session_id\":\"thread-3\"}")
         (result "{\"type\":\"result\",\"value\":{\"ok\":true}}")
         (command
          (mapconcat #'magnus-test-headless--emit (list session result) "; "))
         sessions completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless
                 (magnus-test-headless--request command)
                 (list :on-session
                       (lambda (_process id) (push id sessions))
                       :on-complete
                       (lambda (_process value) (setq completion value)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (equal sessions '("thread-3")))
          (should (equal (magnus-headless-session-id process) "thread-3"))
          (should (equal (plist-get completion :session-id) "thread-3")))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-completes-with-structured-result ()
  (let* ((line "{\"type\":\"result\",\"value\":{\"verdict\":\"approve\"}}")
         (command (magnus-test-headless--emit line))
         completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless
                 (magnus-test-headless--request command)
                 (list :on-complete
                       (lambda (_process value) (setq completion value)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (plist-get completion :success-p))
          (should (equal
                   (alist-get 'verdict
                              (plist-get completion :structured-result))
                   "approve")))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-delivers-stderr-and-retains-tail ()
  (let* ((stderr "0123456789")
         (result "{\"type\":\"result\",\"value\":{\"ok\":true}}")
         (command
          (mapconcat
           #'identity
           (list (magnus-test-headless--emit stderr t t)
                 (magnus-test-headless--emit result))
           "; "))
         (magnus-headless-stderr-limit 5)
         chunks completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless
                 (magnus-test-headless--request command)
                 (list :on-stderr
                       (lambda (_process chunk) (push chunk chunks))
                       :on-complete
                       (lambda (_process value) (setq completion value)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (equal (apply #'concat (nreverse chunks)) stderr))
          (should (equal (plist-get completion :stderr) "56789")))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-callback-failure-does-not-break-decoding ()
  (let* ((result "{\"type\":\"result\",\"value\":{\"ok\":true}}")
         (command (magnus-test-headless--emit result))
         errors completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless
                 (magnus-test-headless--request command)
                 (list :on-raw-event
                       (lambda (_process _line) (error "persistence failed"))
                       :on-error
                       (lambda (_process value) (push value errors))
                       :on-complete
                       (lambda (_process value) (setq completion value)))))
          (magnus-test-headless--wait (lambda () completion))
          (should (plist-get completion :structured-result-present-p))
          (should-not (plist-get completion :success-p))
          (should (= (length (plist-get completion :callback-errors)) 1))
          (should (eq (plist-get (car errors) :kind) 'callback-error)))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-fast-process-cannot-outrun-startup-state ()
  (let* ((session "{\"type\":\"session\",\"session_id\":\"instant\"}")
         (result "{\"type\":\"result\",\"value\":{\"ok\":true}}")
         (command
          (mapconcat #'magnus-test-headless--emit (list session result) "; "))
         (returned nil)
         (callback-before-return nil)
         (missing-state nil)
         completion process)
    (unwind-protect
        (progn
          (setq process
                (magnus-headless-start
                 'magnus-test-headless
                 (magnus-test-headless--request command)
                 (list :on-event
                       (lambda (child _event)
                         (unless returned (setq callback-before-return t))
                         (unless (and (process-get child
                                                   'magnus-headless-request)
                                      (process-get child
                                                   'magnus-headless-decoder)
                                      (process-get child
                                                   'magnus-headless-callbacks))
                           (setq missing-state t)))
                       :on-complete
                       (lambda (_process value) (setq completion value)))))
          (setq returned t)
          (magnus-test-headless--wait (lambda () completion))
          (should-not callback-before-return)
          (should-not missing-state)
          (should (plist-get completion :success-p)))
      (when (and process (process-live-p process))
        (magnus-headless-cancel process t)))))

(ert-deftest magnus-headless-finalize-drains-late-terminal-output ()
  (let* ((process
          (make-process
           :name "magnus-headless-late-output"
           :command (list (or (executable-find "sh") shell-file-name)
                          shell-command-switch "exit 0")
           :buffer nil :noquery t :sentinel #'ignore))
         (line
          (concat "{\"type\":\"session\",\"session_id\":\"late-session\"}\n"
                  "{\"type\":\"result\",\"value\":{\"ok\":true}}\n"))
         delivered completion)
    (unwind-protect
        (progn
          (while (process-live-p process)
            (accept-process-output process 0.01))
          (process-put process 'magnus-headless-provider
                       'magnus-test-headless)
          (process-put process 'magnus-headless-request nil)
          (process-put process 'magnus-headless-callbacks
                       (list :on-complete
                             (lambda (_process result)
                               (setq completion result))))
          (process-put process 'magnus-headless-decoder
                       #'magnus-test-headless--decoder)
          (process-put process 'magnus-headless-partial-line "")
          ;; Force the terminal drain itself to deliver the tail.  This models
          ;; a sentinel becoming runnable one event-loop turn before its filter.
          (cl-letf (((symbol-function 'accept-process-output)
                     (lambda (candidate &rest _args)
                       (when (and (eq candidate process) (not delivered))
                         (setq delivered t)
                         (magnus-headless--filter process line)))))
            (magnus-headless--finalize process))
          (should delivered)
          (should (equal (plist-get completion :session-id) "late-session"))
          (should (plist-get completion :structured-result-present-p))
          (should (plist-get completion :success-p)))
      (when (process-live-p process) (delete-process process)))))

(ert-deftest magnus-claude-review-spec-is-resumable-and-read-only ()
  (let* ((process-environment
          (cons "CLAUDECODE=nested" process-environment))
         (request (list :prompt "Review it"
                        :name "wise-deer"
                        :model "claude-test"
                        :effort 'high
                        :schema-json "{\"type\":\"object\"}"))
         (fresh (magnus-claude-headless-review-spec request))
         (fresh-command (plist-get fresh :command))
         (session-id (plist-get fresh :candidate-session-id))
         (resumed
          (magnus-claude-headless-review-spec
           (plist-put (copy-sequence request) :session-id "resume-me")))
         (resumed-command (plist-get resumed :command)))
    (should (string-match-p
             "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-4[[:xdigit:]]\\{3\\}-a[[:xdigit:]]\\{3\\}-[[:xdigit:]]\\{12\\}\\'"
             session-id))
    (should (member "--safe-mode" fresh-command))
    (should (member "dontAsk" fresh-command))
    (should (member "--json-schema" fresh-command))
    (should (member "high" fresh-command))
    (should (member session-id fresh-command))
    (should-not (string-match-p "Write\\|Edit\\|Bash"
                                magnus-claude-review-tools))
    (should (equal (plist-get fresh :success-requires)
                   '(terminal structured-result)))
    (should-not (plist-get fresh :session-id))
    (should-not (cl-find-if
                 (lambda (entry) (string-prefix-p "CLAUDECODE=" entry))
                 (plist-get fresh :environment)))
    (should (member "--resume" resumed-command))
    (should (member "resume-me" resumed-command))
    (should-not (member "--session-id" resumed-command))))

(ert-deftest magnus-claude-agent-spec-is-explicit-and-separate-from-review ()
  (let* ((process-environment
          (cons "CLAUDECODE=nested" process-environment))
         (spec
          (magnus-claude-headless-spec
           '(:purpose agent
             :prompt "Implement it"
             :name "swift-hare"
             :allowed-tools "Read Write Edit Glob Grep Bash")))
         (command (plist-get spec :command)))
    (should
     (equal command
            (list magnus-claude-executable
                  "--print" "Implement it"
                  "--verbose"
                  "--output-format" "stream-json"
                  "--allowedTools" "Read Write Edit Glob Grep Bash")))
    (should (equal (plist-get spec :success-requires) '(terminal)))
    (should (equal (plist-get spec :name)
                   "magnus-claude-agent-swift-hare"))
    (should-not (member "--safe-mode" command))
    (should-not (member "--json-schema" command))
    (should-not
     (cl-find-if
      (lambda (entry) (string-prefix-p "CLAUDECODE=" entry))
      (plist-get spec :environment)))))

(ert-deftest magnus-claude-agent-spec-requires-explicit-tools ()
  (should-error
   (magnus-claude-headless-spec
    '(:purpose agent :prompt "Implement it"))
   :type 'user-error))

(ert-deftest magnus-claude-review-decoder-captures-schema-result ()
  (let* ((event '((type . "result")
                  (subtype . "success")
                  (session_id . "claude-session")
                  (structured_output . ((verdict . "approve")))))
         (decoded (magnus-claude-headless-decode-event event nil)))
    (should (plist-get decoded :terminal))
    (should (equal (plist-get decoded :session-id) "claude-session"))
    (should (equal (alist-get 'verdict
                              (plist-get decoded :structured-result))
                   "approve"))))

(ert-deftest magnus-claude-decoder-normalizes-visible-text-and-cost ()
  (let* ((assistant
          (magnus-claude-headless-decode-event
           '((type . "assistant")
             (message
              . ((content
                  . (((type . "text") (text . "Hello "))
                     ((type . "tool_use") (name . "Read"))
                     ((type . "text") (text . "world")))))))
           nil))
         (result
          (magnus-claude-headless-decode-event
           '((type . "result")
             (subtype . "success")
             (cost_usd . 0.125))
           nil)))
    (should (equal (plist-get assistant :text) "Hello world"))
    (should (= (plist-get result :cost-usd) 0.125))))

(ert-deftest magnus-codex-review-spec-uses-resumable-exec-session ()
  (let ((schema-file (make-temp-file "magnus-codex-schema-")))
    (unwind-protect
        (let* ((request (list :directory default-directory
                              :prompt "Review it"
                              :name "wise-deer"
                              :base "base-oid"
                              :model "codex-test"
                              :effort 'high
                              :title "Review title"
                              :schema-file schema-file))
               (fresh (magnus-codex-headless-review-spec request))
               (fresh-command (plist-get fresh :command))
               (resumed
                (magnus-codex-headless-review-spec
                 (plist-put (copy-sequence request)
                            :session-id "thread-resume")))
               (resumed-command (plist-get resumed :command))
               (common
                (list magnus-codex-executable "exec"
                      "--json" "--color" "never"
                      "--sandbox" "read-only"
                      "--cd" (expand-file-name default-directory)
                      "--output-schema" schema-file
                      "--model" "codex-test"
                      "--config" "model_reasoning_effort=\"high\"")))
          (should (equal fresh-command (append common '("Review it"))))
          (should (equal resumed-command
                         (append common
                                 '("resume" "thread-resume" "Review it"))))
          (should-not (member "review" fresh-command))
          (should-not (member "--base" fresh-command))
          (should-not (member "--commit" fresh-command))
          (should-not (member "--uncommitted" fresh-command))
          (should-not (member "--title" fresh-command))
          (should-not (member "--ephemeral" fresh-command))
          (should-not (member "review" resumed-command))
          (should-not (member "--base" resumed-command))
          (should-not (member "--commit" resumed-command))
          (should-not (member "--uncommitted" resumed-command))
          (should-not (member "--title" resumed-command))
          (should-not (member "--ephemeral" resumed-command))
          (should (equal (plist-get fresh :success-requires)
                         '(terminal structured-result)))
          (should (equal (plist-get resumed :session-id) "thread-resume")))
      (delete-file schema-file))))

(ert-deftest magnus-codex-headless-agent-purpose-fails-clearly ()
  (should-error
   (magnus-codex-headless-spec
    '(:purpose agent :prompt "Implement it"))
   :type 'user-error))

(ert-deftest magnus-codex-review-decoder-captures-thread-and-result ()
  (let* ((thread
          (magnus-codex-headless-decode-event
           '((type . "thread.started") (thread_id . "codex-thread")) nil))
         (message
          (magnus-codex-headless-decode-event
           '((type . "item.completed")
             (item . ((type . "agent_message")
                      (text . "{\"verdict\":\"approve\"}"))))
           '(:schema-file "/tmp/schema.json")))
         (terminal
          (magnus-codex-headless-decode-event
           '((type . "turn.completed")) nil)))
    (should (equal (plist-get thread :session-id) "codex-thread"))
    (should (equal (alist-get 'verdict
                              (plist-get message :structured-result))
                   "approve"))
    (should (equal (plist-get message :text)
                   "{\"verdict\":\"approve\"}"))
    (should (plist-get terminal :terminal))))

(provide 'magnus-headless-tests)
;;; magnus-headless-tests.el ends here
