;;; magnus-process-lifecycle-tests.el --- Creation transaction tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

;; CI does not install vterm.  These tests exercise lifecycle boundaries and
;; replace terminal creation before it can call into the package.
(unless (featurep 'vterm)
  (provide 'vterm))

(require 'magnus-process)

;; `magnus-process.el' declares these core-owned variables without defaults.
;; Bind them here so isolated loading (without magnus.el) can exercise spawn.
(defvar magnus-claude-executable nil)
(defvar magnus--summon-context nil)
(defvar magnus-headless-allowed-tools nil)

(defmacro magnus-test-process-lifecycle--isolated (&rest body)
  "Run BODY with isolated registry and coordination ownership state."
  (declare (indent 0) (debug t))
  `(let ((magnus-instances nil)
         (magnus-instances-changed-hook nil)
         (magnus-coord--watched-dirs nil)
         (magnus-coord--file-mtimes nil)
         (magnus-coord--processed-mentions nil)
         (magnus-coord--processed-dms nil)
         (magnus-coord--processed-summons nil)
         (magnus-coord--processed-review-ready nil)
         (magnus-coord--review-ready-retries
          (make-hash-table :test #'equal))
         (magnus-process--legacy-session-launches
          (make-hash-table :test #'equal))
         (magnus-coord--session-start-times
          (make-hash-table :test #'equal)))
     ,@body))

(defun magnus-test-process-lifecycle--coord-file (directory &optional agent)
  "Create and return a minimal coordination file in DIRECTORY.
When AGENT is non-nil, include its pre-existing Active Work row."
  (let ((file (magnus-coord-file-path directory)))
    (with-temp-file file
      (insert "# Agent Coordination\n\n## Active Work\n\n")
      (insert "| Agent | Area | Status | Files |\n")
      (insert "|-------|------|--------|-------|\n")
      (when agent
        (insert (format "| %s | existing | in-progress | old.el |\n"
                        agent))))
    file))

(defun magnus-test-process-lifecycle--coord-key (directory)
  "Return DIRECTORY's canonical coordination ownership key."
  (magnus-coord--normalized-directory directory))

(defun magnus-test-process-lifecycle--claim-coord (directory started-at)
  "Model coordination ownership of DIRECTORY since STARTED-AT."
  (let ((key (magnus-test-process-lifecycle--coord-key directory)))
    (cl-pushnew key magnus-coord--watched-dirs :test #'equal)
    (puthash key started-at magnus-coord--session-start-times)
    key))

(ert-deftest magnus-process-create-rolls-back-registry-add-failure ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-registry-" t))
          coord-called
          spawn-called)
      (unwind-protect
          (cl-letf
              (((symbol-function 'magnus-instances-add)
                (lambda (instance)
                  ;; Model a changed-hook failure after the registry mutation.
                  (push instance magnus-instances)
                  (error "registry hook failed")))
               ((symbol-function 'magnus-coord-register-agent)
                (lambda (&rest _arguments) (setq coord-called t)))
               ((symbol-function 'magnus-process--spawn)
                (lambda (&rest _arguments) (setq spawn-called t))))
            (let ((err (should-error
                        (magnus-process-create directory "broken-registry"))))
              (should (string-match-p "registry hook failed"
                                      (error-message-string err))))
            (should-not magnus-instances)
            (should-not coord-called)
            (should-not spawn-called))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-rolls-back-coordination-registration-failure ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-coord-" t))
          spawn-called)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file directory)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent)
                 (lambda (dir _instance)
                    (let ((key (magnus-coord--normalized-directory dir)))
                      (push key magnus-coord--watched-dirs)
                      (puthash key 42 magnus-coord--session-start-times))
                    (error "coord registration failed")))
                 ((symbol-function 'magnus-coord-clear-agent)
                  (lambda (&rest _arguments)
                    (error "rollback must not rewrite legacy ingress")))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (&rest _arguments) (setq spawn-called t))))
              (let ((err (should-error
                          (magnus-process-create directory "broken-coord"))))
                (should (string-match-p "coord registration failed"
                                        (error-message-string err))))
              (should-not magnus-instances)
              (should-not magnus-coord--watched-dirs)
              (should-not
               (gethash directory magnus-coord--session-start-times))
              (should-not spawn-called)))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-rolls-back-claude-spawn-failure ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-claude-" t))
          terminal)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file directory)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent)
                  (lambda (dir _instance)
                    (let ((key (magnus-coord--normalized-directory dir)))
                      (push key magnus-coord--watched-dirs)
                      (puthash key 42 magnus-coord--session-start-times))))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (instance)
                    (setq terminal (generate-new-buffer " *failed-claude*"))
                    (magnus-instances-update
                     instance :buffer terminal :status 'running)
                    (error "claude spawn failed"))))
              (let ((err (should-error
                          (magnus-process-create directory "broken-claude"))))
                (should (string-match-p "claude spawn failed"
                                        (error-message-string err))))
              (should-not magnus-instances)
              (should-not (buffer-live-p terminal))
              (should-not magnus-coord--watched-dirs)
              (should-not
               (gethash directory magnus-coord--session-start-times))))
        (when (buffer-live-p terminal)
          (kill-buffer terminal))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-external-failure-preserves-project-owners ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-create-external-" t))
           (existing (magnus-instances-create
                      directory "existing-agent" 'claude))
           terminal
           provider-calls)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file
             directory "broken-external")
            (magnus-instances-add existing)
            (let ((key (magnus-coord--normalized-directory directory)))
              (setq magnus-coord--watched-dirs (list key))
              (puthash key 17 magnus-coord--session-start-times))
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent) #'ignore)
                 ((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) t))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (instance operation &rest arguments)
                    (push (cons operation arguments) provider-calls)
                    (pcase operation
                      ('start
                       (setq terminal
                             (generate-new-buffer " *failed-provider*"))
                       (magnus-instances-update
                        instance :buffer terminal :status 'running)
                       (error "external start failed"))
                      ('stop nil)
                      (_ (error "unexpected provider operation"))))))
              (let ((err
                     (should-error
                      (magnus-process-create
                       directory "broken-external" 'fake "initial task"))))
                (should (string-match-p "external start failed"
                                        (error-message-string err))))
              (should (equal (magnus-instances-list) (list existing)))
              (should
               (equal magnus-coord--watched-dirs
                      (list (magnus-coord--normalized-directory directory))))
              (should (= (gethash (magnus-coord--normalized-directory directory)
                                  magnus-coord--session-start-times)
                         17))
              (should-not (buffer-live-p terminal))
              (should (member '(start "initial task") provider-calls))
              (should (member '(stop t) provider-calls))))
        (when (buffer-live-p terminal)
          (kill-buffer terminal))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-keeps-successful-acquisition-order ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-success-" t))
          calls)
      (unwind-protect
          (cl-letf
              (((symbol-function 'magnus-instances-add)
                (lambda (instance)
                  (push 'registry calls)
                  (push instance magnus-instances)
                  instance))
               ((symbol-function 'magnus-coord-register-agent)
                (lambda (&rest _arguments) (push 'coordination calls)))
               ((symbol-function 'magnus-provider-external-p)
                (lambda (_instance) t))
               ((symbol-function 'magnus-provider-call)
                (lambda (_instance operation &rest _arguments)
                  (push operation calls))))
            (let ((instance
                   (magnus-process-create directory "healthy" 'fake)))
              (should (eq instance (car magnus-instances)))
              (should (equal (nreverse calls)
                             '(registry coordination start)))))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-headless-rolls-back-launch-failure ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-headless-" t))
          (magnus-claude-executable "claude")
          (magnus-headless-allowed-tools "Read"))
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file directory)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent)
                  (lambda (dir _instance)
                    (let ((key (magnus-coord--normalized-directory dir)))
                      (push key magnus-coord--watched-dirs)
                      (puthash key 42 magnus-coord--session-start-times))))
                 ((symbol-function 'magnus-headless-start)
                  (lambda (&rest _arguments)
                    (error "headless launch failed"))))
              (let ((err
                     (should-error
                      (magnus-process-create-headless
                       "Review this" directory "broken-headless"))))
                (should (string-match-p "headless launch failed"
                                        (error-message-string err))))
              (should-not magnus-instances)
              (should-not magnus-coord--watched-dirs)
              (should-not
               (gethash directory magnus-coord--session-start-times))
              (should-not (get-buffer "*claude-headless:broken-headless*"))))
        (when-let ((buffer (get-buffer "*claude-headless:broken-headless*")))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest magnus-process-headless-spawn-uses-shared-runner ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-headless-runner-" t))
           (instance (magnus-instances-create directory "runner-agent"))
           (magnus-headless-allowed-tools "Read Write Edit")
           (magnus-buffer-name " *magnus-status-not-present*")
           provider request callbacks process buffer)
      (unwind-protect
          (let ((magnus-instances-changed-hook nil))
            (cl-letf
                (((symbol-function 'magnus-headless-start)
                  (lambda (selected-provider selected-request
                           selected-callbacks)
                    (setq provider selected-provider
                          request selected-request
                          callbacks selected-callbacks
                          process
                          (make-pipe-process
                           :name (generate-new-buffer-name
                                  "magnus-headless-runner")
                           :buffer (plist-get selected-request :buffer)
                           :noquery t))
                    process)))
              (setq buffer
                    (magnus-process--spawn-headless
                     instance "Implement it"))
              (should (eq provider 'claude))
              (should (eq (plist-get request :purpose) 'agent))
              (should (equal (plist-get request :allowed-tools)
                             "Read Write Edit"))
              (should
               (equal
                (plist-get request :environment-bindings)
                (list
                 (format "MAGNUS_COORD_WRITER_ID=%s"
                         (magnus-instance-id instance))
                 "MAGNUS_COORD_WRITER_NAME=runner-agent")))
              (should (string-match-p "Implement it"
                                      (plist-get request :prompt)))
              (should (eq (plist-get request :buffer) buffer))
              (should (eq (get-buffer-process buffer) process))
              (funcall (plist-get callbacks :on-event)
                       process '(:type "assistant" :text "Hello"))
              (funcall (plist-get callbacks :on-event)
                       process '(:type "result" :cost-usd 0.25))
              (with-current-buffer buffer
                (should (string-match-p "Hello" (buffer-string)))
                (should (string-match-p "Cost: \\$0\\.2500"
                                        (buffer-string))))
              (funcall (plist-get callbacks :on-complete)
                       process
                       '(:success-p t :status exit
                         :process-event "finished"))
              (should (eq (magnus-instance-status instance) 'finished))
              (with-current-buffer buffer
                (should (string-match-p "Process finished"
                                        (buffer-string))))))
        (when (and process (process-live-p process))
          (delete-process process))
        (when (and buffer (buffer-live-p buffer))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest magnus-process-headless-completion-preserves-purged-status ()
  (let* ((directory (make-temp-file "magnus-headless-purged-" t))
         (instance (magnus-instances-create directory "archived-agent"))
         (buffer (generate-new-buffer " *magnus-headless-purged*"))
         (process (make-pipe-process
                   :name (generate-new-buffer-name "magnus-headless-purged")
                   :buffer buffer :noquery t))
         (magnus-buffer-name " *magnus-status-not-present*"))
    (unwind-protect
        (let ((magnus-instances-changed-hook nil))
          (magnus-instances-update instance :buffer buffer :status 'purged)
          (magnus-process--headless-complete
           instance process
           '(:success-p t :status exit :process-event "finished"))
          (should (eq (magnus-instance-status instance) 'purged)))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest magnus-process-claude-spawn-cancels-prewatch-resources ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-spawn-cleanup-" t))
           (instance (magnus-instances-create directory "failed-spawn"))
           (terminal (generate-new-buffer " *failed-spawn-terminal*"))
           (first-timer (timer-create))
           (second-timer (timer-create))
           (timers (list first-timer second-timer))
           cancelled
           sent-command
           terminal-environment)
      (unwind-protect
          (cl-letf
              (((symbol-value 'magnus-claude-executable) "claude")
               ((symbol-value 'magnus--summon-context) nil)
               ((symbol-function
                 'magnus-process--claude-session-id-supported-p)
                (lambda () t))
               ((symbol-function 'magnus-process--fresh-claude-session-id)
                (lambda () "11111111-1111-4111-a111-111111111111"))
               ((symbol-function 'magnus-process--list-sessions)
                (lambda (_directory)
                  (error "explicit session launch must not scan")))
               ((symbol-function 'magnus-terminal-create-buffer)
                (lambda (_name &optional environment)
                  (setq terminal-environment environment)
                  terminal))
               ((symbol-function 'vterm-send-string)
                (lambda (command &optional _paste)
                  (setq sent-command command)))
               ((symbol-function 'magnus-process--setup-sentinel) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _arguments) (pop timers)))
               ((symbol-function 'cancel-timer)
                (lambda (timer) (push timer cancelled)))
               ((symbol-function 'magnus-process--watch-for-session)
                (lambda (&rest _arguments) (error "watch setup failed"))))
            (let ((err (should-error (magnus-process--spawn instance))))
            (should (string-match-p "watch setup failed"
                                      (error-message-string err))))
            (should
             (equal terminal-environment
                    (list
                     (format "MAGNUS_COORD_WRITER_ID=%s"
                             (magnus-instance-id instance))
                     "MAGNUS_COORD_WRITER_NAME=failed-spawn")))
            (should-not (buffer-live-p terminal))
            (should-not (magnus-instance-buffer instance))
            (should (eq (magnus-instance-status instance) 'stopped))
            (should-not (magnus-instance-session-id instance))
            (should
             (equal sent-command
                    "claude --session-id 11111111-1111-4111-a111-111111111111"))
            (should (memq first-timer cancelled))
            (should (memq second-timer cancelled)))
        (when (buffer-live-p terminal)
          (kill-buffer terminal))
        (delete-directory directory t)))))

(ert-deftest magnus-process-terminal-compatibility-wrappers-delegate ()
  (let (calls)
    (cl-letf (((symbol-function 'magnus-terminal-create-buffer)
               (lambda (name)
                 (push (list 'create name) calls)
                 'terminal-buffer))
              ((symbol-function 'magnus-terminal-send-escape)
               (lambda () (push '(escape) calls)))
              ((symbol-function 'magnus-terminal-setup-keys)
               (lambda () (push '(setup) calls))))
      (should (eq (magnus-process--create-vterm-buffer "compat")
                  'terminal-buffer))
      (magnus-process-send-escape)
      (magnus-process--setup-keys))
    (should (equal (nreverse calls)
                   '((create "compat") (escape) (setup))))))

(ert-deftest magnus-process-session-watch-setup-is-transactional ()
  (let* ((directory (make-temp-file "magnus-session-watch-" t))
         (instance (magnus-instances-create directory "failed-watch"))
         (poll-timer (timer-create))
         removed
         cancelled
         (timer-calls 0))
    (unwind-protect
        (cl-letf
            (((symbol-function 'magnus-process--project-hash)
              (lambda (_directory) "test-project"))
             ((symbol-function 'file-notify-add-watch)
              (lambda (&rest _arguments) 'watch-descriptor))
             ((symbol-function 'file-notify-rm-watch)
              (lambda (descriptor) (setq removed descriptor)))
             ((symbol-function 'run-with-timer)
              (lambda (&rest _arguments)
                (cl-incf timer-calls)
                (if (= timer-calls 1)
                    poll-timer
                  (error "cleanup timer setup failed"))))
             ((symbol-function 'cancel-timer)
              (lambda (timer) (setq cancelled timer))))
          (should-error
           (magnus-process--watch-for-session instance directory nil))
          (should (eq removed 'watch-descriptor))
          (should (eq cancelled poll-timer)))
      (delete-directory directory t))))

(ert-deftest magnus-process-claude-session-support-probe-is-cached ()
  (let ((magnus-claude-executable "/opt/Claude Code/claude")
        (magnus-process--claude-session-id-support-cache
         (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_executable) "/opt/Claude Code/claude"))
              ((symbol-function 'call-process)
               (lambda (&rest _arguments)
                 (cl-incf calls)
                 (insert "  --session-id <uuid>\n")
                 0)))
      (should (magnus-process--claude-session-id-supported-p))
      (should (magnus-process--claude-session-id-supported-p))
      (should (= calls 1)))))

(ert-deftest magnus-process-concurrent-claude-launches-capture-exact-sessions ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-concurrent-claude-" t))
           (home (make-temp-file "magnus-concurrent-home-" t))
           (process-environment (copy-sequence process-environment))
           (magnus-claude-executable "/opt/Claude Code/claude")
           (first (magnus-instances-create directory "first-claude"))
           (second (magnus-instances-create directory "second-claude"))
           (candidates
            '("11111111-1111-4111-a111-111111111111"
              "22222222-2222-4222-a222-222222222222"))
           (commands (make-hash-table :test #'eq))
           watches
           buffers)
      (setenv "HOME" home)
      (unwind-protect
          (cl-letf
              (((symbol-function
                 'magnus-process--claude-session-id-supported-p)
                (lambda () t))
               ((symbol-function 'magnus-process--fresh-claude-session-id)
                (lambda ()
                  (prog1 (car candidates)
                    (setq candidates (cdr candidates)))))
               ((symbol-function 'magnus-process--list-sessions)
                (lambda (_directory)
                  (error "exact session launches must not scan")))
               ((symbol-function 'magnus-terminal-create-buffer)
                (lambda (name &optional _environment)
                  (let ((buffer (generate-new-buffer (concat " *" name))))
                    (push buffer buffers)
                    buffer)))
               ((symbol-function 'vterm-send-string)
                (lambda (command &optional _paste)
                  (puthash (current-buffer) command commands)))
               ((symbol-function 'vterm-send-return) #'ignore)
               ((symbol-function 'magnus-process--setup-sentinel) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _arguments) nil))
               ((symbol-function 'magnus-process--watch-for-session)
                (lambda (instance project before &optional candidate owner
                                  _legacy-token)
                  (push (list :instance instance :directory project
                              :before before :candidate candidate
                              :owner owner)
                        watches))))
            (magnus-process--spawn first)
            (magnus-process--spawn second)
            (should-not candidates)
            (should-not (magnus-instance-session-id first))
            (should-not (magnus-instance-session-id second))
            (dolist (watch watches)
              (let* ((instance (plist-get watch :instance))
                     (candidate (plist-get watch :candidate))
                     (owner (plist-get watch :owner))
                     (path
                      (magnus-process--session-candidate-path
                       directory candidate)))
                (should-not (plist-get watch :before))
                (should (eq owner (magnus-instance-buffer instance)))
                (should
                 (equal
                  (gethash owner commands)
                  (magnus-process--shell-command
                   magnus-claude-executable "--session-id" candidate)))
                (make-directory (file-name-directory path) t)
                (with-temp-file path (insert "{}\n"))
                (should
                 (eq (magnus-process--detect-new-session
                      instance directory nil '(nil nil nil)
                      candidate owner)
                     'captured))))
            (should
             (equal (magnus-instance-session-id first)
                    "11111111-1111-4111-a111-111111111111"))
            (should
             (equal (magnus-instance-session-id second)
                    "22222222-2222-4222-a222-222222222222")))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
        (delete-directory directory t)
        (delete-directory home t)))))

(ert-deftest magnus-process-legacy-claude-launches-serialize-session-capture ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-legacy-serialize-" t))
           (magnus-claude-executable "claude")
           (first (magnus-instances-create directory "legacy-first"))
           (second (magnus-instances-create directory "legacy-second"))
           (buffers nil)
           first-token)
      (unwind-protect
          (cl-letf
              (((symbol-function
                 'magnus-process--claude-session-id-supported-p)
                (lambda () nil))
               ((symbol-function 'magnus-process--list-sessions)
                (lambda (_directory) nil))
               ((symbol-function 'magnus-terminal-create-buffer)
                (lambda (name &optional _environment)
                  (let ((buffer (generate-new-buffer (concat " *" name))))
                    (push buffer buffers)
                    buffer)))
               ((symbol-function 'vterm-send-string) #'ignore)
               ((symbol-function 'magnus-process--setup-sentinel) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _arguments) nil))
               ((symbol-function 'magnus-process--watch-for-session)
                (lambda (_instance _project _before &optional _candidate
                                  _owner legacy-token)
                  (setq first-token legacy-token))))
            (magnus-process--spawn first)
            (let ((err (should-error (magnus-process--spawn second)
                                     :type 'user-error)))
              (should (string-match-p "already resolving"
                                      (error-message-string err))))
            (should first-token)
            (should (= (length buffers) 1))
            (should-not (magnus-instance-buffer second))
            (should-not (magnus-instance-session-id second))
            (magnus-process--release-legacy-session-launch
             directory first-token)
            (should-not
             (gethash (magnus-coord--normalized-directory directory)
                      magnus-process--legacy-session-launches)))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
        (delete-directory directory t)))))

(ert-deftest magnus-process-legacy-session-capture-rejects-ambiguous-delta ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-ambiguous-session-" t))
           (instance (magnus-instances-create directory "legacy-claude"))
           (owner (generate-new-buffer " *legacy-claude-owner*"))
           (token (cons instance 'legacy-launch))
           cleaned)
      (unwind-protect
          (progn
            (magnus-instances-update instance :buffer owner :status 'running)
            (magnus-process--reserve-legacy-session-launch directory token)
            (cl-letf (((symbol-function 'magnus-process--list-sessions)
                       (lambda (_directory) '("old" "fresh-a" "fresh-b")))
                      ((symbol-function
                        'magnus-process--cleanup-session-watch)
                       (lambda (resources) (setq cleaned resources))))
              (should
               (eq (magnus-process--detect-new-session
                    instance directory '("old") '(watch poll cleanup)
                    nil owner token)
                   'ambiguous)))
            (should-not (magnus-instance-session-id instance))
            (should (equal cleaned '(watch poll cleanup)))
            (should-not
             (gethash (magnus-coord--normalized-directory directory)
                      magnus-process--legacy-session-launches)))
        (when (buffer-live-p owner)
          (kill-buffer owner))
        (delete-directory directory t)))))

(ert-deftest magnus-process-exact-session-watcher-rejects-stale-owner ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-stale-session-" t))
           (home (make-temp-file "magnus-stale-home-" t))
           (process-environment (copy-sequence process-environment))
           (instance (magnus-instances-create directory "relaunched-claude"))
           (stale-owner (generate-new-buffer " *stale-claude-owner*"))
           (current-owner (generate-new-buffer " *current-claude-owner*"))
           (candidate "33333333-3333-4333-a333-333333333333")
           cleaned)
      (setenv "HOME" home)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :buffer current-owner :status 'running)
            (let ((path
                   (magnus-process--session-candidate-path
                    directory candidate)))
              (make-directory (file-name-directory path) t)
              (with-temp-file path (insert "{}\n")))
            (cl-letf (((symbol-function
                        'magnus-process--cleanup-session-watch)
                       (lambda (resources) (setq cleaned resources))))
              (should
               (eq (magnus-process--detect-new-session
                    instance directory nil '(watch poll cleanup)
                    candidate stale-owner)
                   'stale)))
            (should-not (magnus-instance-session-id instance))
            (should (equal cleaned '(watch poll cleanup))))
        (when (buffer-live-p stale-owner)
          (kill-buffer stale-owner))
        (when (buffer-live-p current-owner)
          (kill-buffer current-owner))
        (delete-directory directory t)
        (delete-directory home t)))))

(ert-deftest magnus-process-chdir-transfers-external-coordination-ownership ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-old-" t))
           (new-directory (make-temp-file "magnus-move-new-" t))
           (instance
            (magnus-instances-create old-directory "moving-codex" 'codex))
           calls)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :session-id "old-session"
             :previous-session-id "older-session")
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) t))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (candidate operation &rest arguments)
                    (push (list operation
                                (magnus-instance-directory candidate)
                                arguments)
                          calls)
                    (pcase operation
                      ('stop
                       (magnus-instances-update
                        candidate :status 'stopped :buffer nil))
                      ('start
                       (magnus-instances-update candidate :status 'running)))))
                 ((symbol-function 'magnus-process--ensure-agent-dir)
                  (lambda (candidate)
                    (push (list 'onboarding
                                (magnus-instance-directory candidate))
                          calls)))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (directory candidate)
                    (should (string= directory
                                     (magnus-instance-directory candidate)))
                    (push (list 'register directory) calls)))
                 ((symbol-function 'magnus-coord-unregister-agent)
                  (lambda (directory candidate)
                    (should (eq (magnus-instance-status candidate) 'running))
                    (push (list 'unregister directory) calls))))
              (should (eq (magnus-process-chdir instance new-directory)
                          instance)))
            (should
             (equal
              (nreverse calls)
              (list (list 'stop old-directory '(t))
                    (list 'onboarding new-directory)
                    (list 'register new-directory)
                    (list 'start new-directory nil)
                    (list 'unregister old-directory))))
            (should (string= (magnus-instance-directory instance)
                             new-directory))
            (should (eq (magnus-instance-status instance) 'running))
            (should-not (magnus-instance-session-id instance))
            (should (equal (magnus-instance-previous-session-id instance)
                           "old-session")))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-chdir-transfers-local-coordination-ownership ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-local-old-" t))
           (new-directory (make-temp-file "magnus-move-local-new-" t))
           (instance (magnus-instances-create old-directory "moving-claude"))
           calls)
      (unwind-protect
          (cl-letf
              (((symbol-function 'magnus-provider-external-p)
                (lambda (_instance) nil))
               ((symbol-function 'magnus-process--stop-local-for-chdir)
                (lambda (candidate)
                  (push (list 'stop
                              (magnus-instance-directory candidate))
                        calls)
                  (magnus-instances-update
                   candidate :status 'stopped :buffer nil)))
               ((symbol-function 'magnus-process--ensure-agent-dir)
                (lambda (candidate)
                  (push (list 'onboarding
                              (magnus-instance-directory candidate))
                        calls)))
               ((symbol-function 'magnus-coord-register-agent)
                (lambda (directory _candidate)
                  (push (list 'register directory) calls)))
               ((symbol-function 'magnus-process--spawn)
                (lambda (candidate)
                  (push (list 'spawn
                              (magnus-instance-directory candidate))
                        calls)
                  (magnus-instances-update candidate :status 'running)))
               ((symbol-function 'magnus-coord-unregister-agent)
                (lambda (directory _candidate)
                  (push (list 'unregister directory) calls)))
               ((symbol-function 'run-with-timer)
                (lambda (&rest _arguments)
                  (error "directory move must not defer startup"))))
            (should (eq (magnus-process-chdir instance new-directory)
                        instance))
            (should
             (equal
              (nreverse calls)
              (list (list 'stop old-directory)
                    (list 'onboarding new-directory)
                    (list 'register new-directory)
                    (list 'spawn new-directory)
                    (list 'unregister old-directory))))
            (should (eq (magnus-instance-status instance) 'running)))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-chdir-symlink-alias-is-physical-no-op ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-move-real-" t))
           (alias (concat directory "-alias"))
           instance
           calls)
      (unwind-protect
          (progn
            (make-symbolic-link directory alias)
            (setq instance (magnus-instances-create alias "same-project"))
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) nil))
                 ((symbol-function 'magnus-process--stop-local-for-chdir)
                  (lambda (&rest _arguments) (push 'stop calls)))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (&rest _arguments) (push 'register calls)))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (&rest _arguments) (push 'spawn calls)))
                 ((symbol-function 'magnus-coord-unregister-agent)
                  (lambda (&rest _arguments) (push 'unregister calls))))
              (should (eq (magnus-process-chdir instance directory)
                          instance)))
            (should-not calls)
            ;; Keep the user's working-path spelling; only ownership identity
            ;; is canonicalized to the physical project root.
            (should (string= (magnus-instance-directory instance) alias)))
        (when (file-symlink-p alias)
          (delete-file alias))
        (delete-directory directory t)))))

(ert-deftest magnus-process-chdir-rolls-back-external-start-failure ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-fail-old-" t))
           (new-directory (make-temp-file "magnus-move-fail-new-" t))
           (instance
            (magnus-instances-create old-directory "broken-codex" 'codex))
           failed-buffer
           registrations
           stopped-watchers)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--claim-coord old-directory 17)
            (magnus-instances-update
             instance :status 'running :session-id "old-session"
             :previous-session-id "older-session")
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) t))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (candidate operation &rest _arguments)
                    (pcase operation
                      ('stop
                       (magnus-instances-update
                        candidate :status 'stopped :buffer nil))
                      ('start
                       (setq failed-buffer
                             (generate-new-buffer " *failed-move-codex*"))
                       (magnus-instances-update
                        candidate :status 'running :buffer failed-buffer)
                       (error "destination start failed")))))
                 ((symbol-function 'magnus-process--ensure-agent-dir) #'ignore)
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (directory _candidate)
                    (push directory registrations)
                    (magnus-test-process-lifecycle--claim-coord
                     directory 23)))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (directory)
                    (push directory stopped-watchers)
                    (setq magnus-coord--watched-dirs
                          (delete directory magnus-coord--watched-dirs)))))
              (let ((err (should-error
                          (magnus-process-chdir instance new-directory))))
                (should (string-match-p "destination start failed"
                                        (error-message-string err)))))
            (should (equal registrations (list new-directory)))
            (should
             (equal stopped-watchers
                    (list (magnus-test-process-lifecycle--coord-key
                           new-directory))))
            (should
             (equal magnus-coord--watched-dirs
                    (list (magnus-test-process-lifecycle--coord-key
                           old-directory))))
            (should (= (gethash
                        (magnus-test-process-lifecycle--coord-key old-directory)
                                magnus-coord--session-start-times)
                       17))
            (should-not
             (gethash (magnus-test-process-lifecycle--coord-key new-directory)
                      magnus-coord--session-start-times))
            (should (string= (magnus-instance-directory instance)
                             old-directory))
            (should (eq (magnus-instance-status instance) 'stopped))
            (should (equal (magnus-instance-session-id instance)
                           "old-session"))
            (should (equal (magnus-instance-previous-session-id instance)
                           "older-session"))
            (should-not (buffer-live-p failed-buffer)))
        (when (buffer-live-p failed-buffer)
          (kill-buffer failed-buffer))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-chdir-rolls-back-local-spawn-failure ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-local-fail-old-" t))
           (new-directory (make-temp-file "magnus-move-local-fail-new-" t))
           (instance (magnus-instances-create old-directory "broken-claude"))
           failed-buffer)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--claim-coord old-directory 31)
            (magnus-instances-update
             instance :status 'running :session-id "old-local-session")
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) nil))
                 ((symbol-function 'magnus-process--stop-local-for-chdir)
                  (lambda (candidate)
                    (magnus-instances-update
                     candidate :status 'stopped :buffer nil)))
                 ((symbol-function 'magnus-process--ensure-agent-dir) #'ignore)
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (directory _candidate)
                    (magnus-test-process-lifecycle--claim-coord
                     directory 37)))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (candidate)
                    (setq failed-buffer
                          (generate-new-buffer " *failed-move-claude*"))
                    (magnus-instances-update
                     candidate :status 'running :buffer failed-buffer)
                    (error "destination spawn failed")))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (directory)
                    (setq magnus-coord--watched-dirs
                          (delete directory magnus-coord--watched-dirs)))))
              (let ((err (should-error
                          (magnus-process-chdir instance new-directory))))
                (should (string-match-p "destination spawn failed"
                                        (error-message-string err)))))
            (should
             (equal magnus-coord--watched-dirs
                    (list (magnus-test-process-lifecycle--coord-key
                           old-directory))))
            (should (= (gethash
                        (magnus-test-process-lifecycle--coord-key old-directory)
                                magnus-coord--session-start-times)
                       31))
            (should-not
             (gethash (magnus-test-process-lifecycle--coord-key new-directory)
                      magnus-coord--session-start-times))
            (should (string= (magnus-instance-directory instance)
                             old-directory))
            (should (eq (magnus-instance-status instance) 'stopped))
            (should (equal (magnus-instance-session-id instance)
                           "old-local-session"))
            (should-not (buffer-live-p failed-buffer)))
        (when (buffer-live-p failed-buffer)
          (kill-buffer failed-buffer))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-chdir-restores-source-after-release-failure ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-release-old-" t))
           (new-directory (make-temp-file "magnus-move-release-new-" t))
           (instance (magnus-instances-create old-directory "release-fail"))
           new-buffer
           registrations)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--claim-coord old-directory 41)
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) nil))
                 ((symbol-function 'magnus-process--stop-local-for-chdir)
                  (lambda (candidate)
                    (magnus-instances-update
                     candidate :status 'stopped :buffer nil)))
                 ((symbol-function 'magnus-process--ensure-agent-dir) #'ignore)
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (directory _candidate)
                    (push directory registrations)
                    (magnus-test-process-lifecycle--claim-coord
                     directory 43)))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (candidate)
                    (setq new-buffer
                          (generate-new-buffer " *release-fail-runtime*"))
                    (magnus-instances-update
                     candidate :status 'running :buffer new-buffer)))
                 ((symbol-function 'magnus-coord-unregister-agent)
                  (lambda (directory _candidate)
                    (let ((key
                           (magnus-test-process-lifecycle--coord-key
                            directory)))
                      (setq magnus-coord--watched-dirs
                            (delete key magnus-coord--watched-dirs))
                      (remhash key magnus-coord--session-start-times))
                    (error "source release failed")))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (directory)
                    (setq magnus-coord--watched-dirs
                          (delete directory magnus-coord--watched-dirs)))))
              (let ((err (should-error
                          (magnus-process-chdir instance new-directory))))
                (should (string-match-p "source release failed"
                                        (error-message-string err)))))
            (should (equal registrations
                           (list old-directory new-directory)))
            (should
             (equal magnus-coord--watched-dirs
                    (list (magnus-test-process-lifecycle--coord-key
                           old-directory))))
            (should (= (gethash
                        (magnus-test-process-lifecycle--coord-key old-directory)
                                magnus-coord--session-start-times)
                       43))
            (should-not
             (gethash (magnus-test-process-lifecycle--coord-key new-directory)
                      magnus-coord--session-start-times))
            (should (string= (magnus-instance-directory instance)
                             old-directory))
            (should (eq (magnus-instance-status instance) 'stopped))
            (should-not (buffer-live-p new-buffer)))
        (when (buffer-live-p new-buffer)
          (kill-buffer new-buffer))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-resurrect-rolls-back-new-external-ownership ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-resurrect-codex-" t))
           (instance
            (magnus-instances-create directory "archived-codex" 'codex))
           failed-buffer
           stopped-watchers)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'purged :session-id "codex-session"
             :purged-at 123.0)
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) t))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (project candidate)
                    (should (eq (magnus-instance-status candidate) 'purged))
                    (magnus-test-process-lifecycle--claim-coord project 51)))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (candidate operation &rest _arguments)
                    (pcase operation
                      ('resume
                       (setq failed-buffer
                             (generate-new-buffer
                              " *failed-resurrect-codex*"))
                       (magnus-instances-update
                        candidate :status 'running :buffer failed-buffer)
                       (error "resume failed"))
                      ('stop
                       (magnus-instances-update
                        candidate :status 'stopped :buffer nil)))))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (project)
                    (push project stopped-watchers)
                    (setq magnus-coord--watched-dirs
                          (delete project magnus-coord--watched-dirs)))))
              (let ((err
                     (should-error
                      (magnus-process-resurrect-purged instance))))
                (should (string-match-p "resume failed"
                                        (error-message-string err)))))
            (should
             (equal stopped-watchers
                    (list (magnus-test-process-lifecycle--coord-key
                           directory))))
            (should-not magnus-coord--watched-dirs)
            (should-not
             (gethash (magnus-test-process-lifecycle--coord-key directory)
                      magnus-coord--session-start-times))
            (should (eq (magnus-instance-status instance) 'purged))
            (should-not (magnus-instance-buffer instance))
            (should (equal (magnus-instance-session-id instance)
                           "codex-session"))
            (should (= (magnus-instance-purged-at instance) 123.0))
            (should-not (buffer-live-p failed-buffer)))
        (when (buffer-live-p failed-buffer)
          (kill-buffer failed-buffer))
        (delete-directory directory t)))))

(ert-deftest magnus-process-resurrect-preserves-shared-ownership-on-failure ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-resurrect-claude-" t))
           (instance
            (magnus-instances-create directory "archived-claude"))
           failed-buffer
           stopped-watcher)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--claim-coord directory 59)
            (magnus-instances-update
             instance :status 'purged :session-id "claude-session"
             :purged-at 456.0)
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) nil))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (_project candidate)
                    (should (eq (magnus-instance-status candidate) 'purged))))
                 ((symbol-function 'magnus-process--spawn-with-session)
                  (lambda (candidate session-id)
                    (should (equal session-id "claude-session"))
                    (setq failed-buffer
                          (generate-new-buffer
                           " *failed-resurrect-claude*"))
                    (magnus-instances-update
                     candidate :status 'running :buffer failed-buffer)
                    (error "spawn resume failed")))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (_project) (setq stopped-watcher t))))
              (let ((err
                     (should-error
                      (magnus-process-resurrect-purged instance))))
                (should (string-match-p "spawn resume failed"
                                        (error-message-string err)))))
            (should-not stopped-watcher)
            (should
             (equal magnus-coord--watched-dirs
                    (list (magnus-test-process-lifecycle--coord-key
                           directory))))
            (should (= (gethash
                        (magnus-test-process-lifecycle--coord-key directory)
                                magnus-coord--session-start-times)
                       59))
            (should (eq (magnus-instance-status instance) 'purged))
            (should-not (magnus-instance-buffer instance))
            (should (equal (magnus-instance-session-id instance)
                           "claude-session"))
            (should (= (magnus-instance-purged-at instance) 456.0))
            (should-not (buffer-live-p failed-buffer)))
        (when (buffer-live-p failed-buffer)
          (kill-buffer failed-buffer))
        (delete-directory directory t)))))

(ert-deftest magnus-process-archive-purges-before-unregistering ()
  (dolist (provider '(claude codex))
    (magnus-test-process-lifecycle--isolated
      (let* ((directory (make-temp-file "magnus-archive-order-" t))
             (instance
              (magnus-instances-create directory "archive-me" provider))
             unregister-status)
        (unwind-protect
            (progn
              (magnus-instances-update instance :status 'running)
              (cl-letf
                  (((symbol-function 'magnus-provider-external-p)
                    (lambda (_instance) (eq provider 'codex)))
                   ((symbol-function 'magnus-provider-call)
                    (lambda (candidate operation &rest _arguments)
                      (should (eq operation 'stop))
                      (should-not
                       (eq (magnus-instance-status candidate) 'purged))
                      (magnus-instances-update candidate :status 'stopped)))
                   ((symbol-function 'magnus-coord-unregister-agent)
                    (lambda (_directory candidate)
                      (setq unregister-status
                            (magnus-instance-status candidate))))
                   ((symbol-function 'magnus--agents-index-update) #'ignore))
                (magnus-process-archive instance))
              (should (eq unregister-status 'purged))
              (should (eq (magnus-instance-status instance) 'purged)))
          (delete-directory directory t))))))

(provide 'magnus-process-lifecycle-tests)
;;; magnus-process-lifecycle-tests.el ends here
