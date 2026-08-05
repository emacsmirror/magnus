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
          cleared
          spawn-called)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file directory)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent)
                  (lambda (dir _instance)
                    (push dir magnus-coord--watched-dirs)
                    (puthash dir 42 magnus-coord--session-start-times)
                    (error "coord registration failed")))
                 ((symbol-function 'magnus-coord-clear-agent)
                  (lambda (_dir name) (setq cleared name)))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (&rest _arguments) (setq spawn-called t))))
              (let ((err (should-error
                          (magnus-process-create directory "broken-coord"))))
                (should (string-match-p "coord registration failed"
                                        (error-message-string err))))
              (should (equal cleared "broken-coord"))
              (should-not magnus-instances)
              (should-not magnus-coord--watched-dirs)
              (should-not
               (gethash directory magnus-coord--session-start-times))
              (should-not spawn-called)))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-rolls-back-claude-spawn-failure ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-claude-" t))
          terminal
          cleared)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file directory)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent)
                  (lambda (dir _instance)
                    (push dir magnus-coord--watched-dirs)
                    (puthash dir 42 magnus-coord--session-start-times)))
                 ((symbol-function 'magnus-coord-clear-agent)
                  (lambda (_dir name) (setq cleared name)))
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
              (should (equal cleared "broken-claude"))
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
           cleared
           provider-calls)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file
             directory "broken-external")
            (magnus-instances-add existing)
            (setq magnus-coord--watched-dirs (list directory))
            (puthash directory 17 magnus-coord--session-start-times)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent) #'ignore)
                 ((symbol-function 'magnus-coord-clear-agent)
                  (lambda (_dir name) (setq cleared name)))
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
              ;; A pre-existing row with the same display identity is not a
              ;; resource created by this failed call and must remain intact.
              (should-not cleared)
              (should (equal (magnus-instances-list) (list existing)))
              (should (equal magnus-coord--watched-dirs (list directory)))
              (should (= (gethash directory
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
          (magnus-headless-allowed-tools "Read")
          cleared)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file directory)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent)
                  (lambda (dir _instance)
                    (push dir magnus-coord--watched-dirs)
                    (puthash dir 42 magnus-coord--session-start-times)))
                 ((symbol-function 'magnus-coord-clear-agent)
                  (lambda (_dir name) (setq cleared name)))
                 ((symbol-function 'magnus-headless-start)
                  (lambda (&rest _arguments)
                    (error "headless launch failed"))))
              (let ((err
                     (should-error
                      (magnus-process-create-headless
                       "Review this" directory "broken-headless"))))
                (should (string-match-p "headless launch failed"
                                        (error-message-string err))))
              (should (equal cleared "broken-headless"))
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
           provider request callbacks process buffer logs)
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
                    process))
                 ((symbol-function 'magnus-coord-add-log)
                  (lambda (&rest arguments) (push arguments logs))))
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
              (should (equal (car logs)
                             (list directory "runner-agent"
                                   "Headless task finished")))
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
          (cl-letf (((symbol-function 'magnus-coord-add-log) #'ignore))
            (magnus-process--headless-complete
             instance process
             '(:success-p t :status exit :process-event "finished")))
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
           terminal-environment)
      (unwind-protect
          (cl-letf
              (((symbol-value 'magnus-claude-executable) "claude")
               ((symbol-value 'magnus--summon-context) nil)
               ((symbol-function 'magnus-process--list-sessions)
                (lambda (_directory) nil))
               ((symbol-function 'magnus-terminal-create-buffer)
                (lambda (_name &optional environment)
                  (setq terminal-environment environment)
                  terminal))
               ((symbol-function 'vterm-send-string) #'ignore)
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

(provide 'magnus-process-lifecycle-tests)
;;; magnus-process-lifecycle-tests.el ends here
