;;; magnus-coord-integration-tests.el --- Coordination boundary tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-coord)
(require 'magnus-review)

(defun magnus-coord-integration-tests--review-effect (event-id)
  "Return a durable review effect named EVENT-ID."
  (magnus-coord-state-review-effect--create
   :writer-id "author-id" :writer-name "author" :writer-sequence 2
   :event-id event-id :created-at "2026-08-04T12:00:00.000000Z"
   :request-id "review-id" :checkpoint-token "checkpoint-token"
   :base "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   :head "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))

(ert-deftest magnus-coord-integration-legacy-poll-reads-once-per-change ()
  "One legacy revision is read once and shared by every ingress consumer."
  (let* ((directory (make-temp-file "magnus-coord-legacy-poll-" t))
         (file (expand-file-name magnus-coord-file directory))
         (magnus-coord--watched-dirs (list directory))
         (magnus-coord--file-mtimes (list (cons directory '(0 0 0 0))))
         (magnus-coord--legacy-states nil)
         (magnus-coord-mention-notify t)
         (reads 0)
         consumed)
    (unwind-protect
        (progn
          (with-temp-file file (insert "one immutable ingress read"))
          (cl-letf (((symbol-function 'magnus-coord--update-buffer-ticks)
                     #'ignore)
                    ((symbol-function 'magnus-coord--read-legacy-content)
                     (lambda (_directory)
                       (cl-incf reads)
                       "one immutable ingress read"))
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
                       (push (list 'reviews root content) consumed)))
                    ((symbol-function 'magnus-coord-runtime-refresh)
                     (lambda (_directory) 'unchanged-result))
                    ((symbol-function 'magnus-coord--consume-runtime-result)
                     (lambda (_directory result) result)))
            (magnus-coord--poll-all)
            (should (= reads 1))
            (should (= (length consumed) 4))
            (dolist (entry consumed)
              (should (equal (nth 1 entry) directory))
              (should (equal (nth 2 entry) "one immutable ingress read")))
            ;; The second poll observes the same mtime and performs no legacy
            ;; content read, while the cheap event revision still runs.
            (magnus-coord--poll-all)
            (should (= reads 1))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-status-parse-uses-polled-legacy-cache ()
  "Presentation refreshes never reread a watched legacy file."
  (let* ((directory (magnus-coord--normalized-directory default-directory))
         (cached '(:active nil :log nil
                   :discoveries ("cached") :decisions nil))
         (magnus-coord--legacy-states (list (cons directory cached))))
    (cl-letf (((symbol-function 'magnus-coord--read-legacy-content)
               (lambda (&rest _arguments)
                 (ert-fail "status parse reread watched legacy ingress")))
              ((symbol-function 'magnus-coord-runtime-current-state)
               (lambda (_directory) nil)))
      (should (equal (plist-get (magnus-coord-parse directory) :discoveries)
                     '("cached"))))))

(ert-deftest magnus-coord-integration-repeated-watch-does-not-reseed-legacy ()
  "Review setup may refresh events without swallowing an in-flight mention."
  (let ((directory (magnus-coord--normalized-directory default-directory))
        (magnus-coord--watched-dirs
         (list (magnus-coord--normalized-directory default-directory)))
        (magnus-coord--poll-timer 'existing-timer)
        consumed)
    (cl-letf (((symbol-function 'magnus-coord--read-legacy-content)
               (lambda (&rest _arguments)
                 (ert-fail "an existing watcher reread legacy ingress")))
              ((symbol-function 'magnus-coord-runtime-start)
               (lambda (_directory) 'runtime-result))
              ((symbol-function 'magnus-coord--consume-runtime-result)
               (lambda (root result) (setq consumed (cons root result))))
              ((symbol-function 'magnus-coord--start-poll-timer) #'ignore))
      (magnus-coord-start-watching directory))
    (should (equal consumed (cons directory 'runtime-result)))))

(ert-deftest magnus-coord-integration-routes-runtime-effects-by-record ()
  "Runtime results preserve exact log and review objects during delivery."
  (let* ((directory (magnus-coord--normalized-directory default-directory))
         (log
          (magnus-coord-state-log-record--create
           :writer-id "writer" :writer-name "Writer" :writer-sequence 1
           :event-id "log-id" :created-at "2026-08-04T12:00:00.000000Z"
           :message "hello"))
         (review (magnus-coord-integration-tests--review-effect "review-id"))
         (result
          (magnus-coord-runtime-result--create
           :project-directory directory :new-logs (list log)
           :unresolved-reviews (list review)))
         delivered)
    (cl-letf (((symbol-function 'magnus-coord--deliver-event-log)
               (lambda (root record) (push (list 'log root record) delivered)))
              ((symbol-function 'magnus-coord--dispatch-event-review)
               (lambda (root effect)
                 (push (list 'review root effect) delivered))))
      (should (eq (magnus-coord--consume-runtime-result directory result) result)))
    (should (equal (nreverse delivered)
                   (list (list 'log directory log)
                         (list 'review directory review))))))

(ert-deftest magnus-coord-integration-legacy-routing-addresses-display-names ()
  "Compatibility routing supports both generated and free-form display names."
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

(ert-deftest magnus-coord-integration-review-retry-keeps-exact-event ()
  "A transient handler retry reuses captured evidence without rescanning."
  (let* ((directory default-directory)
         (effect (magnus-coord-integration-tests--review-effect "retry-event"))
         (magnus-coord-review-ready-hook '(handler))
         (magnus-coord--event-review-retries (make-hash-table :test #'equal))
         (dispatches 0)
         markers
         settled)
    (cl-letf (((symbol-function 'magnus-coord--dispatch-review-ready)
               (lambda (_root marker)
                 (push marker markers)
                 (cl-incf dispatches)
                 (> dispatches 1)))
              ((symbol-function 'magnus-coord--settle-event-review)
               (lambda (_root event-id)
                 (setq settled event-id)
                 (magnus-coord--clear-event-review-retry directory event-id)
                 t))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _arguments) 'retry-timer))
              ((symbol-function 'magnus-coord-runtime-refresh)
               (lambda (&rest _arguments)
                 (ert-fail "an exact retry must not rescan the store"))))
      (magnus-coord--dispatch-event-review directory effect)
      (let* ((key (cons directory "retry-event"))
             (state (gethash key magnus-coord--event-review-retries)))
        (should state)
        (should (eq (plist-get state :timer) 'retry-timer))
        (puthash key (plist-put state :timer nil)
                 magnus-coord--event-review-retries)
        (magnus-coord--retry-event-review directory "retry-event"))
      (should (= dispatches 2))
      (should (equal settled "retry-event"))
      (should-not (gethash (cons directory "retry-event")
                           magnus-coord--event-review-retries))
      (should (equal (car markers) (cadr markers)))
      (should (equal (plist-get (car markers) :writer-id) "author-id")))))

(ert-deftest magnus-coord-integration-review-evidence-waits-for-a-handler ()
  "A temporarily detached controller cannot accidentally settle evidence."
  (let ((magnus-coord-review-ready-hook nil))
    (should-not
     (magnus-coord--dispatch-review-ready
     default-directory '(:request-id "review")))))

(ert-deftest magnus-coord-integration-unloaded-review-evidence-is-not-settled ()
  "An event whose manifest failed to load remains available for recovery."
  (let ((effect (magnus-coord-integration-tests--review-effect "unknown-event"))
        (magnus-reviews nil)
        (magnus-coord-review-ready-hook
         (list #'magnus-review-handle-ready-marker))
        (magnus-coord--event-review-retries (make-hash-table :test #'equal))
        settled)
    (cl-letf (((symbol-function 'magnus-coord-runtime-settle-review)
               (lambda (&rest _arguments) (setq settled t)))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _arguments) 'retry-timer)))
      (magnus-coord--dispatch-event-review default-directory effect))
    (should-not settled)
    (let ((retry
           (gethash (cons default-directory "unknown-event")
                    magnus-coord--event-review-retries)))
      (should retry)
      (should (= (plist-get retry :count) 1))
      (should (eq (plist-get retry :timer) 'retry-timer)))))

(ert-deftest magnus-coord-integration-event-tombstone-hides-legacy-active-row ()
  "A durable clear suppresses the same display identity in legacy ingress."
  (let* ((directory (make-temp-file "magnus-coord-merge-" t))
         (file (expand-file-name magnus-coord-file directory))
         (clear
          (magnus-coord-state-active-record--create
           :writer-id "writer-id" :writer-name "same-agent"
           :writer-sequence 3 :event-id "clear-id"
           :created-at "2026-08-04T12:00:00.000000Z" :operation 'clear))
         (state
          (magnus-coord-state--create
           :project-directory (file-name-as-directory directory)
           :active nil :active-winners (list clear)
           :discoveries nil :decisions nil :knowledge-winners nil
           :logs nil :review-ready nil :issues nil :retained-event-ids nil)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# Agent Coordination\n\n"
                    "## Active Work\n\n"
                    "| Agent | Area | Status | Files |\n"
                    "|-------|------|--------|-------|\n"
                    "| same-agent | stale | in-progress | stale.el |\n"
                    "| legacy-peer | docs | in-progress | README.md |\n\n"
                    "## Discoveries\n\n- Legacy discovery\n\n"
                    "## Decisions\n\n- Legacy decision\n\n"
                    "## Log\n\n[12:00] same-agent: legacy log\n"))
          (cl-letf (((symbol-function 'magnus-coord-runtime-current-state)
                     (lambda (_directory) state)))
            (let ((parsed (magnus-coord-parse directory)))
              (should (equal (mapcar (lambda (entry)
                                       (plist-get entry :agent))
                                     (plist-get parsed :active))
                             '("legacy-peer")))
              (should (equal (plist-get parsed :discoveries)
                             '("Legacy discovery")))
              (should (equal (plist-get parsed :decisions)
                             '("Legacy decision"))))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-merged-log-has-one-chronological-contract ()
  "Legacy and event logs merge chronologically across presentation consumers."
  (let* ((directory (make-temp-file "magnus-coord-log-order-" t))
         (file (expand-file-name magnus-coord-file directory))
         (old-event
          (magnus-coord-state-log-record--create
           :writer-id "event-writer" :writer-name "event-agent"
           :writer-sequence 1 :event-id "event-old"
           :created-at "2026-08-04T11:01:00.000000Z"
           :message "event old"))
         (new-event
          (magnus-coord-state-log-record--create
           :writer-id "event-writer" :writer-name "event-agent"
           :writer-sequence 2 :event-id "event-new"
           :created-at "2026-08-04T11:02:00.000000Z"
           :message "event new"))
         (state
          (magnus-coord-state--create
           :project-directory (file-name-as-directory (file-truename directory))
           :active nil :active-winners nil :discoveries nil :decisions nil
           :knowledge-winners nil :logs (list old-event new-event)
           :review-ready nil :issues nil :retained-event-ids nil)))
    (unwind-protect
        (progn
          ;; Legacy writes are newest-first on disk.
          (with-temp-file file
            (insert "# Agent Coordination\n\n## Log\n\n"
                    "[10:02] legacy-agent: legacy new\n"
                    "[10:01] legacy-agent: legacy old\n"))
          (cl-letf (((symbol-function 'magnus-coord-runtime-current-state)
                     (lambda (_directory) state)))
            (let* ((log (plist-get (magnus-coord-parse directory) :log))
                   (messages
                    (mapcar (lambda (entry) (plist-get entry :message)) log)))
              (should (equal messages
                             '("legacy old" "legacy new"
                               "event old" "event new")))
              (should
               (equal
                (mapcar (lambda (entry) (plist-get entry :message))
                        (magnus-coord-recent-log log 3))
                '("legacy new" "event old" "event new"))))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-registration-does-not-create-legacy-files ()
  "New registration creates identity infrastructure, not legacy/skill files."
  (let* ((directory (make-temp-file "magnus-coord-register-" t))
         (instance (magnus-instances-create directory "new-agent" 'codex))
         (magnus-coord--watched-dirs nil)
         (magnus-coord--session-start-times (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-coord-ensure-file)
                   (lambda (&rest _arguments)
                     (ert-fail "registration created legacy ingress")))
                  ((symbol-function 'magnus-coord-ensure-skill)
                   (lambda (&rest _arguments)
                     (ert-fail "registration created a legacy skill")))
                  ((symbol-function 'magnus-coord-start-watching)
                   (lambda (root) (push root magnus-coord--watched-dirs))))
          (magnus-coord-register-agent directory instance)
          (should
           (file-directory-p
            (magnus-coord-store-writer-directory
             directory (magnus-instance-id instance))))
          (should (file-exists-p (magnus-coord-instructions-path directory)))
          (should-not (file-exists-p (magnus-coord-file-path directory)))
          (should-not (file-exists-p (magnus-coord-skill-path directory))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-generated-state-is-locally-git-ignored ()
  "Registration keeps generated coordination state out of `git add -A'."
  (skip-unless (executable-find "git"))
  (let ((directory (make-temp-file "magnus-coord-git-exclude-" t)))
    (unwind-protect
        (progn
          (should (zerop (process-file "git" nil nil nil
                                       "-C" directory "init" "--quiet")))
          (should (magnus-coord--ensure-git-excludes directory))
          (let* ((file (expand-file-name ".git/info/exclude" directory))
                 (first
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
            (should (magnus-coord--ensure-git-excludes directory))
            (should
             (equal first
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
            (should (string-match-p (regexp-quote "/.magnus-coord/") first))
            (should (string-match-p
                     (regexp-quote "/.claude/magnus-instructions.md")
                     first))
            (make-directory (expand-file-name ".magnus-coord/writers/id"
                                               directory) t)
            (make-directory (expand-file-name ".claude" directory) t)
            (with-temp-file
                (expand-file-name ".magnus-coord/writers/id/event.json"
                                  directory)
              (insert "{}\n"))
            (with-temp-file
                (expand-file-name ".claude/magnus-instructions.md" directory)
              (insert "generated\n"))
            (with-temp-buffer
              (should (zerop (process-file "git" nil t nil
                                           "-C" directory "status"
                                           "--porcelain")))
              (should (string-empty-p (buffer-string))))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-integration-state-predicate-tolerates-missing-project ()
  "Status probes fail closed when an old instance directory disappeared."
  (let ((directory (expand-file-name
                    "missing-magnus-project" temporary-file-directory)))
    (cl-letf (((symbol-function 'magnus-coord-runtime-current-state)
               (lambda (_directory) nil)))
      (should-not (magnus-coord-has-state-p directory)))))

(ert-deftest magnus-coord-integration-lifecycle-hides-only-managed-stale-work ()
  "Archived and moved Magnus writers disappear; unmanaged writers remain."
  (let* ((directory (make-temp-file "magnus-coord-lifecycle-" t))
         (other (make-temp-file "magnus-coord-lifecycle-other-" t))
         (instance (magnus-instances-create directory "managed" 'codex))
         (managed
          (magnus-coord-state-active-record--create
           :writer-id (magnus-instance-id instance) :writer-name "managed"
           :writer-sequence 1 :event-id "managed-event"
           :created-at "2026-08-04T12:00:00.000000Z" :operation 'set
           :area "stale" :status "working" :files nil))
         (unmanaged
          (magnus-coord-state-active-record--create
           :writer-id "outside-magnus" :writer-name "outside"
           :writer-sequence 1 :event-id "outside-event"
           :created-at "2026-08-04T12:00:00.000000Z" :operation 'set
           :area "live" :status "working" :files nil))
         (state
          (magnus-coord-state--create
           :project-directory (file-name-as-directory directory)
           :active (list managed unmanaged)
           :active-winners (list managed unmanaged)
           :discoveries nil :decisions nil :knowledge-winners nil
           :logs nil :review-ready nil :issues nil :retained-event-ids nil))
         (magnus-instances (list instance)))
    (unwind-protect
        (progn
          (setf (magnus-instance-status instance) 'purged)
          (should
           (equal
            (mapcar #'magnus-coord-state-active-record-writer-id
                    (magnus-coord-state-visible-active state))
            '("outside-magnus")))
          (setf (magnus-instance-status instance) 'running
                (magnus-instance-directory instance) other)
          (should
           (equal
            (mapcar #'magnus-coord-state-active-record-writer-id
                    (magnus-coord-state-visible-active state))
            '("outside-magnus")))
          (setf (magnus-instance-directory instance) directory)
          (should (= (length (magnus-coord-state-visible-active state)) 2)))
      (delete-directory directory t)
      (delete-directory other t))))

(ert-deftest magnus-coord-integration-lifecycle-change-reprojects-cached-state ()
  "A process status transition rewrites presentation without a store scan."
  (let* ((directory (magnus-coord--normalized-directory default-directory))
         (instance (magnus-instances-create directory "managed" 'codex))
         (magnus-instances (list instance))
         (magnus-coord--watched-dirs (list directory))
         (magnus-coord--lifecycle-signatures
          (make-hash-table :test #'equal))
         reprojected)
    (setf (magnus-instance-status instance) 'running)
    (puthash directory (list (magnus-instance-id instance))
             magnus-coord--lifecycle-signatures)
    (setf (magnus-instance-status instance) 'stopped)
    (cl-letf (((symbol-function 'magnus-coord-runtime-reproject)
               (lambda (root) (push root reprojected))))
      (magnus-coord--instances-changed))
    (should (equal reprojected (list directory)))
    (should-not (gethash directory magnus-coord--lifecycle-signatures))))

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
              ((symbol-function 'magnus-coord-generate-retro) #'ignore))
      (setq pending t)
      (magnus-coord-unregister-agent directory instance)
      (should-not stopped)
      (setq pending nil)
      (magnus-coord--maybe-stop-watching directory)
      (should (equal stopped directory)))))

(ert-deftest magnus-coord-integration-undelivered-nudge-never-writes-legacy ()
  "System delivery failures stay in *Messages*, not shared Markdown."
  (let ((instance (magnus-instances-create default-directory "offline" 'codex)))
    (cl-letf (((symbol-function 'magnus-coord-add-log)
               (lambda (&rest _arguments)
                 (ert-fail "undelivered nudge wrote legacy ingress")))
              ((symbol-function 'message) #'ignore))
      (should-not
       (magnus-coord--log-undelivered-nudge instance "@peer hello" "stopped")))))

(ert-deftest magnus-coord-integration-symlink-and-real-root-share-one-identity ()
  "An agent's symlink path and a review's real root cannot split runtimes."
  (let* ((parent (make-temp-file "magnus-coord-symlink-" t))
         (real (expand-file-name "real" parent))
         (link (expand-file-name "link" parent))
         (magnus-coord--watched-dirs nil)
         (magnus-coord--file-mtimes nil)
         (magnus-coord--legacy-states nil)
         (magnus-coord--poll-timer nil)
         (magnus-coord--lifecycle-signatures
          (make-hash-table :test #'equal))
         starts)
    (unwind-protect
        (progn
          (make-directory real)
          (make-symbolic-link real link)
          (let* ((canonical (directory-file-name (file-truename real)))
                 (instance (magnus-instances-create link "linked-agent" 'codex))
                 (record
                  (magnus-coord-state-active-record--create
                   :writer-id (magnus-instance-id instance)
                   :writer-name "linked-agent" :writer-sequence 1
                   :event-id "linked-active"
                   :created-at "2026-08-04T12:00:00.000000Z"
                   :operation 'set :area "review" :status "working"
                   :files nil))
                 (state
                  (magnus-coord-state--create
                   :project-directory (file-name-as-directory canonical)
                   :active (list record) :active-winners (list record)))
                 (magnus-instances (list instance)))
            (setf (magnus-instance-status instance) 'running)
            (cl-letf (((symbol-function 'magnus-coord--read-legacy-content)
                       (lambda (_root) nil))
                      ((symbol-function 'magnus-coord-runtime-start)
                       (lambda (root) (push root starts) 'result))
                      ((symbol-function 'magnus-coord--consume-runtime-result)
                       (lambda (_root result) result))
                      ((symbol-function 'magnus-coord--start-poll-timer)
                       #'ignore))
              (magnus-coord-start-watching link)
              (magnus-coord-start-watching real))
            (should (equal magnus-coord--watched-dirs (list canonical)))
            (should (equal starts (list canonical canonical)))
            (should
             (equal (magnus-coord-runtime--project link)
                    (magnus-coord-runtime--project real)))
            (should
             (equal (magnus-coord-store-directory link)
                    (magnus-coord-store-directory real)))
            (should (equal (magnus-coord-state-visible-active state)
                           (list record)))
            (should (eq (magnus-coord--find-instance-by-name
                         "linked-agent" canonical)
                        instance))))
      (delete-directory parent t))))

(ert-deftest magnus-coord-integration-poll-timer-follows-watcher-lifetime ()
  "The global poll timer exists exactly while at least one project is watched."
  (let* ((directory (make-temp-file "magnus-coord-timer-" t))
         (canonical (magnus-coord--normalized-directory directory))
         (magnus-coord--watched-dirs nil)
         (magnus-coord--poll-timer nil)
         (magnus-coord--file-mtimes nil)
         (magnus-coord--legacy-states nil)
         started cancelled)
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _arguments)
                     (setq started t)
                     'poll-timer))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (setq cancelled timer)))
                  ((symbol-function 'magnus-coord-runtime-stop) #'ignore))
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

(ert-deftest magnus-coord-integration-review-retry-ledgers-are-diagnosable ()
  "Event and legacy exhaustion are visible and manually re-armable."
  (let* ((directory (magnus-coord--normalized-directory default-directory))
         (magnus-coord--event-review-retries
          (make-hash-table :test #'equal))
         (magnus-coord--review-ready-retries
          (make-hash-table :test #'equal))
         (magnus-coord-review-ready-retry-count 0)
         (magnus-coord--last-review-handler-error "manifest unreadable"))
    (puthash (cons directory "event-id")
             '(:marker marker :count 3 :exhausted t
               :last-error "settlement failed")
             magnus-coord--event-review-retries)
    (cl-letf (((symbol-function 'message) #'ignore))
      (magnus-coord--schedule-review-ready-retry directory "legacy-hash"))
    (let ((diagnostics (magnus-coord-review-retry-diagnostics directory)))
      (should (= (plist-get diagnostics :exhausted-review-count) 2))
      (should
       (equal (plist-get diagnostics :exhausted-review-event-ids)
              '("event-id" "legacy:legacy-hash")))
      (should
       (equal
        (sort
         (delq nil
               (mapcar (lambda (detail) (plist-get detail :last-error))
                       (plist-get diagnostics :exhausted-review-details)))
         #'string<)
        '("manifest unreadable" "settlement failed"))))
    (should
     (= (length (magnus-coord--reset-exhausted-review-retries directory)) 2))
    (should (= (hash-table-count magnus-coord--event-review-retries) 0))
    (should (= (hash-table-count magnus-coord--review-ready-retries) 0))))

(ert-deftest magnus-coord-integration-settlement-error-reaches-retry-ledger ()
  "A post-handler settlement failure remains actionable in diagnostics."
  (let ((directory (magnus-coord--normalized-directory default-directory))
        (magnus-coord--event-review-retries (make-hash-table :test #'equal))
        (magnus-coord--last-review-handler-error nil))
    (cl-letf (((symbol-function 'magnus-coord-runtime-settle-review)
               (lambda (&rest _arguments) (error "cache disappeared")))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'run-with-timer)
               (lambda (&rest _arguments) 'retry-timer)))
      (should-not (magnus-coord--settle-event-review directory "event-id"))
      (magnus-coord--schedule-event-review-retry
       directory "event-id" '(:request-id "review")))
    (should
     (equal
      (plist-get
       (gethash (cons directory "event-id")
                magnus-coord--event-review-retries)
       :last-error)
      "cache disappeared"))))

(provide 'magnus-coord-integration-tests)
;;; magnus-coord-integration-tests.el ends here
