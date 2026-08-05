;;; magnus-coord-integration-tests.el --- Markdown coordination boundaries -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-coord)
(require 'magnus-review)

(ert-deftest magnus-coord-integration-poll-reads-once-per-change ()
  "One Markdown revision is read once and shared by every consumer."
  (let* ((directory (make-temp-file "magnus-coord-poll-" t))
         (file (expand-file-name magnus-coord-file directory))
         (magnus-coord--watched-dirs (list directory))
         (magnus-coord--file-mtimes (list (cons directory '(0 0 0 0))))
         (magnus-coord--states nil)
         (magnus-coord-mention-notify t)
         (reads 0)
         consumed)
    (unwind-protect
        (progn
          (with-temp-file file (insert "one shared Markdown read"))
          (cl-letf (((symbol-function 'magnus-coord--update-buffer-ticks)
                     #'ignore)
                    ((symbol-function 'magnus-coord--read-content)
                     (lambda (_directory)
                       (cl-incf reads)
                       "one shared Markdown read"))
                    ((symbol-function 'magnus-coord--check-new-mentions)
                     (lambda (root content)
                       (push (list 'mentions root content) consumed)))
                    ((symbol-function 'magnus-coord--check-new-dms)
                     (lambda (root content)
                       (push (list 'dms root content) consumed)))
                    ((symbol-function 'magnus-coord--check-new-summons)
                     (lambda (root content)
                       (push (list 'summons root content) consumed)))
                    ((symbol-function 'magnus-coord--check-new-review-ready)
                     (lambda (root content)
                       (push (list 'reviews root content) consumed))))
            (magnus-coord--poll-all)
            (should (= reads 1))
            (should (= (length consumed) 4))
            (dolist (entry consumed)
              (should (equal (nth 1 entry) directory))
              (should (equal (nth 2 entry) "one shared Markdown read")))
            (magnus-coord--poll-all)
            (should (= reads 1))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-status-parse-uses-polled-cache ()
  "Presentation refreshes never reread a watched Markdown file."
  (let* ((directory (magnus-coord--normalized-directory default-directory))
         (cached '(:active nil :log nil
                   :discoveries ("cached") :decisions nil))
         (magnus-coord--states (list (cons directory cached))))
    (cl-letf (((symbol-function 'magnus-coord--read-content)
               (lambda (&rest _arguments)
                 (ert-fail "status parse reread watched Markdown ingress"))))
      (should (equal (plist-get (magnus-coord-parse directory) :discoveries)
                     '("cached"))))))

(ert-deftest magnus-coord-integration-repeated-watch-does-not-reseed ()
  "Repeated setup cannot swallow a message that arrived after initial seed."
  (let* ((temporary-root (make-temp-file "magnus-coord-rewatch-" t))
         (directory (magnus-coord--normalized-directory temporary-root))
         (magnus-coord--watched-dirs (list directory))
         (magnus-coord--poll-timer 'existing-timer))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'magnus-coord--read-content)
                     (lambda (&rest _arguments)
                       (ert-fail "an existing watcher reread Markdown")))
                    ((symbol-function 'magnus-coord--start-poll-timer) #'ignore))
            (magnus-coord-start-watching directory))
          (should (equal magnus-coord--watched-dirs (list directory))))
      (delete-directory temporary-root t))))

(ert-deftest magnus-coord-integration-routing-addresses-display-names ()
  "Markdown routing supports generated and free-form display names."
  (let ((content
         (concat "[12:00] sender: hello @quick-wren and @{Wise Deer}\n"
                 "[12:01] sender: [DM @Wise Deer] check this\n"
                 "[12:02] sender: [SUMMON @Wise Deer] review UI\n")))
    (should (equal (mapcar #'car (magnus-coord--extract-mentions content))
                   '("quick-wren" "Wise Deer")))
    (should (equal (car (magnus-coord--extract-dms content))
                   '("Wise Deer" "sender" "check this")))
    (should (equal (car (magnus-coord--extract-summons content))
                   '("Wise Deer" "sender" "review UI")))))

(ert-deftest magnus-coord-integration-review-evidence-waits-for-a-handler ()
  "A temporarily detached review controller cannot consume checkpoint data."
  (let ((magnus-coord-review-ready-hook nil))
    (should-not
     (magnus-coord--dispatch-review-ready
      default-directory '(:request-id "review")))))

(ert-deftest magnus-coord-integration-refresh-acquires-orphan-review-once ()
  "Manual refresh consumes an unwatched review marker exactly once."
  (let* ((directory (make-temp-file "magnus-coord-orphan-review-" t))
         (canonical (magnus-coord--normalized-directory directory))
         (file (magnus-coord-file-path directory))
         (review 'pending-review)
         (pending t)
         (base "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
         (head "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
         (marker
          (format (concat "[REVIEW-READY request=orphan-review "
                          "checkpoint=orphan-token base=%s head=%s]")
                  base head))
         (magnus-instances nil)
         (magnus-coord--watched-dirs nil)
         (magnus-coord--file-mtimes nil)
         (magnus-coord--states nil)
         (magnus-coord--processed-mentions nil)
         (magnus-coord--processed-dms nil)
         (magnus-coord--processed-summons nil)
         (magnus-coord--processed-review-ready nil)
         (magnus-coord--poll-timer nil)
         calls)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# Agent Coordination\n\n## Log\n\n" marker "\n"))
          (let ((magnus-coord-review-ready-hook
                 (list
                  (lambda (root parsed-marker)
                    (push (list root parsed-marker) calls)
                    ;; Resolving the sole marker relinquishes the final owner
                    ;; while watcher acquisition is still on the stack.
                    (setq pending nil)))))
            (cl-letf (((symbol-function 'magnus-review-list)
                       (lambda () (list review)))
                      ((symbol-function 'magnus-review-lifecycle)
                       (lambda (_review) 'open))
                      ((symbol-function 'magnus-review-project-root)
                       (lambda (_review) directory))
                      ((symbol-function
                        'magnus-review-pending-checkpoint-request)
                       (lambda (_review) (and pending 'pending-request)))
                      ((symbol-function 'magnus-coord--start-poll-timer)
                       #'ignore))
              (magnus-coord-refresh-all)))
          (should (= (length calls) 1))
          (should (equal (caar calls) canonical))
          (should
           (equal (plist-get (cadar calls) :request-id) "orphan-review"))
          (should-not magnus-coord--watched-dirs)
          (should-not (assoc canonical magnus-coord--file-mtimes))
          (should-not (assoc canonical magnus-coord--states))
          (should-not
           (assoc canonical magnus-coord--processed-review-ready)))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-registration-and-unregistration-log ()
  "Agent lifecycle is represented in the shared Markdown file."
  (let* ((directory (make-temp-file "magnus-coord-register-" t))
         (canonical (magnus-coord--normalized-directory directory))
         (instance (magnus-instances-create directory "new-agent" 'codex))
         (magnus-instances (list instance))
         (magnus-coord--watched-dirs nil)
         (magnus-coord--file-mtimes nil)
         (magnus-coord--states nil)
         (magnus-coord--poll-timer nil)
         (magnus-coord--session-start-times (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-coord--start-poll-timer) #'ignore)
                  ((symbol-function 'magnus-coord-generate-retro) #'ignore))
          (magnus-coord-register-agent directory instance)
          (should (file-exists-p (magnus-coord-file-path directory)))
          (should (file-exists-p (magnus-coord-instructions-path directory)))
          (should (equal magnus-coord--watched-dirs (list canonical)))
          (should
           (equal (mapcar (lambda (entry) (plist-get entry :message))
                          (plist-get (magnus-coord-parse directory) :log))
                  '("Joined the session")))
          (setf (magnus-instance-status instance) 'purged)
          (magnus-coord-unregister-agent directory instance)
          (let ((messages
                 (mapcar (lambda (entry) (plist-get entry :message))
                         (plist-get (magnus-coord-parse directory) :log))))
            (should (member "Left the session" messages))
            (should (member "Session ended" messages)))
          (should-not magnus-coord--watched-dirs))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-reconcile-removes-ghost-work-rows ()
  "Only running and suspended owners retain in-progress Markdown rows."
  (let* ((directory (make-temp-file "magnus-coord-reconcile-" t))
         (running
          (magnus-instance--create
           :id "running" :name "running-agent" :directory directory
           :status 'running))
         (suspended
          (magnus-instance--create
           :id "suspended" :name "suspended-agent" :directory directory
           :status 'suspended))
         (stopped
          (magnus-instance--create
           :id "stopped" :name "stopped-agent" :directory directory
           :status 'stopped))
         (purged
          (magnus-instance--create
           :id "purged" :name "purged-agent" :directory directory
           :status 'purged))
         (magnus-instances (list running suspended stopped purged))
         (magnus-coord--states nil))
    (unwind-protect
        (progn
          (magnus-coord-ensure-file directory)
          (dolist (name '("running-agent" "suspended-agent" "stopped-agent"
                          "purged-agent" "unknown-agent"))
            (magnus-coord-update-active
             directory name "test area" "in-progress" '("sample.el")))
          (magnus-coord-reconcile directory)
          (let ((names
                 (sort
                  (mapcar
                   (lambda (entry) (plist-get entry :agent))
                   (plist-get (magnus-coord-parse directory) :active))
                  #'string<)))
            (should
             (equal names '("running-agent" "suspended-agent")))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-pending-review-retains-last-watcher ()
  "Archiving the last agent cannot strand an unresolved review checkpoint."
  (let* ((directory (magnus-coord--normalized-directory default-directory))
         (instance (magnus-instances-create directory "author" 'codex))
         (magnus-instances (list instance))
         (magnus-coord--watched-dirs (list directory))
         pending
         stopped)
    (setf (magnus-instance-status instance) 'purged)
    (cl-letf (((symbol-function 'magnus-coord--pending-review-p)
               (lambda (_root) pending))
              ((symbol-function 'magnus-coord-stop-watching)
               (lambda (root) (setq stopped root)))
              ((symbol-function 'magnus-coord-clear-agent) #'ignore)
              ((symbol-function 'magnus-coord-add-log) #'ignore)
              ((symbol-function 'magnus-coord-refresh) #'ignore)
              ((symbol-function 'magnus-coord-generate-retro) #'ignore)
              ((symbol-function 'magnus-coord-mark-session-end) #'ignore))
      (setq pending t)
      (magnus-coord-unregister-agent directory instance)
      (should-not stopped)
      (setq pending nil)
      (magnus-coord--maybe-stop-watching directory)
      (should (equal stopped directory)))))

(ert-deftest magnus-coord-integration-undelivered-nudge-is-durable-markdown ()
  "A failed nudge is discoverable after the transient echo-area message."
  (let* ((directory (make-temp-file "magnus-coord-undelivered-" t))
         (instance (magnus-instances-create directory "offline" 'codex)))
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore))
          (should-not
           (magnus-coord--log-undelivered-nudge
            instance "Please tell @peer later" "stopped"))
          (let* ((parsed (magnus-coord-parse directory))
                 (entry (car (plist-get parsed :log))))
            (should (string-match-p "Undelivered nudge"
                                    (plist-get entry :message)))
            (should (string-match-p "(at) peer"
                                    (plist-get entry :message)))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-symlink-and-real-root-share-identity ()
  "A symlink path and physical project root share one watcher and route."
  (let* ((parent (make-temp-file "magnus-coord-symlink-" t))
         (real (expand-file-name "real" parent))
         (link (expand-file-name "link" parent))
         (magnus-coord--watched-dirs nil)
         (magnus-coord--file-mtimes nil)
         (magnus-coord--states nil)
         (magnus-coord--poll-timer nil)
         (reads 0))
    (unwind-protect
        (progn
          (make-directory real)
          (make-symbolic-link real link)
          (let* ((canonical (magnus-coord--normalized-directory real))
                 (instance (magnus-instances-create link "linked-agent" 'codex))
                 (magnus-instances (list instance)))
            (cl-letf (((symbol-function 'magnus-coord--read-content)
                       (lambda (_root) (cl-incf reads) nil))
                      ((symbol-function 'magnus-coord--start-poll-timer)
                       #'ignore))
              (magnus-coord-start-watching link)
              (magnus-coord-start-watching real))
            (should (= reads 1))
            (should (equal magnus-coord--watched-dirs (list canonical)))
            (should (eq (magnus-coord--find-instance-by-name
                         "linked-agent" canonical)
                        instance))))
      (delete-directory parent t))))

(ert-deftest magnus-coord-integration-poll-timer-follows-watcher-lifetime ()
  "The poll timer exists exactly while at least one project is watched."
  (let* ((directory (make-temp-file "magnus-coord-timer-" t))
         (canonical (magnus-coord--normalized-directory directory))
         (magnus-coord--watched-dirs nil)
         (magnus-coord--poll-timer nil)
         (magnus-coord--file-mtimes nil)
         (magnus-coord--states nil)
         (magnus-coord--review-ready-retries
          (make-hash-table :test #'equal))
         started cancelled)
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _arguments)
                     (setq started t)
                     'poll-timer))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (setq cancelled timer))))
          (magnus-coord--start-poll-timer)
          (should-not started)
          (setq magnus-coord--watched-dirs (list canonical))
          (magnus-coord--start-poll-timer)
          (should started)
          (should (eq magnus-coord--poll-timer 'poll-timer))
          (magnus-coord-stop-watching directory)
          (should (eq cancelled 'poll-timer))
          (should-not magnus-coord--poll-timer))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-restored-stopped-agent-upgrades-guidance ()
  "Watcher recovery replaces stale event guidance for a stopped instance."
  (let* ((directory (make-temp-file "magnus-coord-stale-guidance-" t))
         (canonical (magnus-coord--normalized-directory directory))
         (instructions (magnus-coord-instructions-path directory))
         (instance
          (magnus-instance--create
           :id "restored-stopped" :name "restored-agent"
           :directory directory :status 'stopped))
         (magnus-instances (list instance))
         (magnus-reviews nil)
         (magnus-coord--watched-dirs nil)
         (magnus-coord--file-mtimes nil)
         (magnus-coord--states nil)
         (magnus-coord--processed-mentions nil)
         (magnus-coord--processed-dms nil)
         (magnus-coord--processed-summons nil)
         (magnus-coord--processed-review-ready nil)
         (magnus-coord--poll-timer nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory instructions) t)
          (with-temp-file instructions
            (insert "Publish immutable events under .magnus-coord/writers/.\n"
                    "Read .magnus-coord/current.md.\n"
                    "<!-- magnus-instructions-version: 4 -->\n"))
          (cl-letf (((symbol-function 'magnus-coord--start-poll-timer)
                     #'ignore))
            (magnus-coord-ensure-watchers))
          (let ((content
                 (with-temp-buffer
                   (insert-file-contents instructions)
                   (buffer-string))))
            (should (string-match-p "magnus-instructions-version: 8" content))
            (should (string-match-p "shared \"\\.magnus-coord\\.md\" file"
                                    content))
            (should-not (string-match-p "immutable events" content))
            (should-not (string-match-p "\\.magnus-coord/current\\.md"
                                        content))
            (should-not (string-match-p "\\.magnus-coord/writers/" content)))
          (should (equal magnus-coord--watched-dirs (list canonical))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-review-retries-are-diagnosable ()
  "Markdown checkpoint exhaustion is visible and manually re-armable."
  (let* ((directory (magnus-coord--normalized-directory default-directory))
         (magnus-coord--review-ready-retries
          (make-hash-table :test #'equal))
         (magnus-coord-review-ready-retry-count 0)
         (magnus-coord--last-review-handler-error "manifest unreadable"))
    (cl-letf (((symbol-function 'message) #'ignore))
      (magnus-coord--schedule-review-ready-retry directory "marker-hash"))
    (let ((diagnostics (magnus-coord-review-retry-diagnostics directory)))
      (should (= (plist-get diagnostics :exhausted-review-count) 1))
      (should
       (equal (plist-get diagnostics :exhausted-review-marker-hashes)
              '("marker-hash")))
      (should
       (equal (plist-get (car (plist-get diagnostics
                                          :exhausted-review-details))
                         :last-error)
              "manifest unreadable")))
    (should
     (equal (magnus-coord--reset-exhausted-review-retries directory)
            '("marker-hash")))
    (should (= (hash-table-count magnus-coord--review-ready-retries) 0))))

(ert-deftest magnus-coord-integration-agent-log-has-chronological-contract ()
  "Agent-written newest-first Markdown is normalized for presentation."
  (let* ((directory (make-temp-file "magnus-coord-log-order-" t))
         (file (expand-file-name magnus-coord-file directory)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# Agent Coordination\n\n## Log\n\n"
                    "<!-- Agents insert new entries below this comment. -->\n\n"
                    "[10:02] agent: new\n"
                    "[10:01] agent: old\n"))
          (let* ((log (plist-get (magnus-coord-parse directory) :log))
                 (messages
                  (mapcar (lambda (entry) (plist-get entry :message)) log)))
            (should (equal messages '("old" "new")))
            (should
             (equal
              (mapcar (lambda (entry) (plist-get entry :message))
                      (magnus-coord-recent-log log 1))
              '("new")))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-instructions-require-newest-first-log ()
  "Generated agent guidance agrees with Magnus's insertion and parser order."
  (let ((instructions (magnus-coord--instructions-content default-directory)))
    (should (string-match-p (regexp-quote "Log is stored newest-first")
                            instructions))
    (should (string-match-p (regexp-quote "immediately below") instructions))
    (should (string-match-p (regexp-quote "never append it at the bottom")
                            instructions))))

(ert-deftest magnus-coord-integration-instructions-use-custom-paths ()
  "Versioned guidance safely names both configured coordination paths."
  (let* ((directory (make-temp-file "magnus-coord-custom-guidance-" t))
         (magnus-coord-file "control/team journal.md")
         (magnus-coord-instructions-file
          ".agents/magnus shared instructions.md")
         (journal (magnus-coord-display-file directory))
         (instructions-file
          (magnus-coord-display-instructions-file directory))
         (content (magnus-coord--instructions-content directory)))
    (unwind-protect
        (progn
          (should (string-match-p (regexp-quote (prin1-to-string journal))
                                  content))
          (should
           (string-match-p (regexp-quote (prin1-to-string instructions-file))
                           content))
          (should (string-match-p "magnus-instructions-version: 8" content))
          (should-not (string-match-p (regexp-quote ".magnus-coord.md")
                                      content))
          (should-not
           (string-match-p
            (regexp-quote ".claude/magnus-instructions.md") content)))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-trim-preserves-review-checkpoints ()
  "Log bounds remove only old ordinary entries, never review evidence."
  (let* ((directory (make-temp-file "magnus-coord-trim-" t))
         (file (expand-file-name magnus-coord-file directory))
         (magnus-coord-log-max-entries 2)
         (bare
          (concat
           "[REVIEW-READY request=bare checkpoint=one "
           "base=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa "
           "head=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb]"))
         (timestamped
          (concat
           "[12:04] agent: [REVIEW-READY request=timestamped checkpoint=two "
           "base=cccccccccccccccccccccccccccccccccccccccc "
           "head=dddddddddddddddddddddddddddddddddddddddd]")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# Agent Coordination\n\n## Log\n\n"
                    "[12:05] agent: ordinary-newest\n\n"
                    bare "\n\n"
                    timestamped "\n\n"
                    "[12:03] agent: ordinary-second\n\n"
                    "[12:02] agent: ordinary-old\n\n"
                    "[12:01] agent: ordinary-oldest\n\n"))
          (magnus-coord-trim-log directory)
          (let ((content
                 (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
            (should (string-match-p "ordinary-newest" content))
            (should (string-match-p "ordinary-second" content))
            (should-not (string-match-p "ordinary-old\\(?:est\\)?" content))
            (should (string-match-p (regexp-quote bare) content))
            (should (string-match-p (regexp-quote timestamped) content))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-reminders-run-housekeeping-while-idle ()
  "Periodic maintenance is independent of whether nudges are suppressed."
  (let ((magnus-coord--user-idle-p t)
        trimmed tidied checked)
    (cl-letf (((symbol-function 'magnus-coord-trim-all)
               (lambda () (setq trimmed t)))
              ((symbol-function 'magnus-coord--maybe-tidy)
               (lambda () (setq tidied t)))
              ((symbol-function 'magnus-coord-check-context-all)
               (lambda () (setq checked t))))
      (magnus-coord--send-reminders))
    (should trimmed)
    (should tidied)
    (should checked)))

(ert-deftest magnus-coord-integration-reminder-uses-custom-control-paths ()
  "Reminder prompts safely name the configured coordination journal."
  (let* ((directory (make-temp-file "magnus-coord-reminder-paths-" t))
         (magnus-coord-file "control/team journal.md")
         (instance (magnus-instances-create directory "reminded-agent" 'codex))
         (magnus-instances (list instance))
         (magnus-coord--reminder-index 0)
         prompt)
    (unwind-protect
        (progn
          (setf (magnus-instance-status instance) 'running)
          (cl-letf (((symbol-function 'magnus-coord-agent-busy-p)
                     (lambda (_instance) nil))
                    ((symbol-function 'magnus-coord-agent-quiescent-p)
                     (lambda (_instance) t))
                    ((symbol-function 'magnus-coord-nudge-agent)
                     (lambda (_instance text _source) (setq prompt text)))
                    ((symbol-function 'magnus-coord-trim-all) #'ignore)
                    ((symbol-function 'magnus-coord-check-context-all) #'ignore)
                    ((symbol-function 'magnus-coord--maybe-tidy) #'ignore))
            (magnus-coord--send-reminders))
          (should prompt)
          (should
           (string-match-p
            (regexp-quote
             (prin1-to-string (magnus-coord-display-file directory)))
            prompt))
          (should-not (string-match-p (regexp-quote ".magnus-coord.md")
                                      prompt)))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-tidy-uses-custom-control-paths ()
  "Tidy prompts safely name the configured coordination journal."
  (let* ((directory (make-temp-file "magnus-coord-tidy-paths-" t))
         (magnus-coord-file "control/team journal.md")
         (file (magnus-coord-file-path directory))
         (instance (magnus-instances-create directory "tidy-agent" 'codex))
         (magnus-instances (list instance))
         (magnus-coord-tidy-size-threshold 0)
         (magnus-coord--last-tidy-time nil)
         prompt)
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "large enough"))
          (setf (magnus-instance-status instance) 'running)
          (cl-letf (((symbol-function 'magnus-coord-agent-busy-p)
                     (lambda (_instance) nil))
                    ((symbol-function 'magnus-coord-agent-quiescent-p)
                     (lambda (_instance) t))
                    ((symbol-function 'magnus-coord-nudge-agent)
                     (lambda (_instance text _source) (setq prompt text)))
                    ((symbol-function 'magnus-coord-add-log) #'ignore))
            (magnus-coord--maybe-tidy))
          (should prompt)
          (should
           (string-match-p
            (regexp-quote
             (prin1-to-string (magnus-coord-display-file directory)))
            prompt))
          (should-not (string-match-p (regexp-quote ".magnus-coord.md")
                                      prompt)))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-state-predicate-tolerates-missing-project ()
  "Status probes fail closed when an old instance directory disappeared."
  (let ((directory (expand-file-name
                    "missing-magnus-project" temporary-file-directory)))
    (should-not (magnus-coord-has-state-p directory))))

(provide 'magnus-coord-integration-tests)
;;; magnus-coord-integration-tests.el ends here
