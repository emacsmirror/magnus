;;; magnus-coord-runtime-tests.el --- Coordination runtime tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-coord-runtime)

(defun magnus-coord-runtime-tests--revision (token &optional issues)
  "Return a revision result containing TOKEN and ISSUES."
  (magnus-coord-store-revision-result--create
   :token token :issues issues))

(defun magnus-coord-runtime-tests--log (id)
  "Return a minimal log record named ID."
  (magnus-coord-state-log-record--create
   :writer-id "writer" :writer-name "Writer" :writer-sequence 1
   :event-id id :created-at "2026-08-04T00:00:00.000000Z"
   :message id))

(defun magnus-coord-runtime-tests--review (id)
  "Return a minimal review-ready effect named ID."
  (magnus-coord-state-review-effect--create
   :writer-id "writer" :writer-name "Writer" :writer-sequence 1
   :event-id id :created-at "2026-08-04T00:00:00.000000Z"
   :request-id id :checkpoint-token (concat "token-" id)
   :base "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   :head "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))

(defun magnus-coord-runtime-tests--state
    (project snapshot &optional logs reviews retained issues log-effects)
  "Return test state for PROJECT and SNAPSHOT with supplied records."
  (magnus-coord-state--create
   :project-directory (file-name-as-directory project)
   :snapshot snapshot :active nil :active-winners nil
   :discoveries nil :decisions nil :knowledge-winners nil
   :logs logs :log-effects (or log-effects logs)
   :review-ready reviews :issues issues
   :retained-event-ids retained))

(defmacro magnus-coord-runtime-tests--isolated (&rest body)
  "Run BODY with an empty runtime registry."
  (declare (indent 0) (debug t))
  `(let ((magnus-coord-runtime--projects (make-hash-table :test #'equal)))
     ,@body))

(ert-deftest magnus-coord-runtime-unchanged-revision-does-no-content-read ()
  "A stable token avoids snapshot, reduce, and projection work."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (snapshot 'snapshot-a)
           (state (magnus-coord-runtime-tests--state
                   project snapshot
                   (list (magnus-coord-runtime-tests--log "log-1"))
                   nil nil '(state-issue)))
           (revision-calls 0) (snapshot-calls 0) (reduce-calls 0)
           (projection-calls 0) (schedule-calls 0))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (cl-incf revision-calls)
                   (magnus-coord-runtime-tests--revision
                    "same" '(store-issue))))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project) (cl-incf snapshot-calls) snapshot))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (_snapshot) (cl-incf reduce-calls) state))
                ((symbol-function 'magnus-coord-state-write-projection)
                 (lambda (_state) (cl-incf projection-calls) "current.md"))
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 (lambda (_project) (cl-incf schedule-calls))))
        (let ((started (magnus-coord-runtime-start project))
              (refreshed (magnus-coord-runtime-refresh project)))
          (should (magnus-coord-runtime-result-changed-p started))
          (should-not (magnus-coord-runtime-result-new-logs started))
          (should (equal (magnus-coord-runtime-result-revision-issues started)
                         '(store-issue)))
          (should (equal (magnus-coord-runtime-result-state-issues started)
                         '(state-issue)))
          (should-not (magnus-coord-runtime-result-changed-p refreshed))
          (should-not (magnus-coord-runtime-result-new-logs refreshed))))
      (should (= revision-calls 2))
      (should (= snapshot-calls 1))
      (should (= reduce-calls 1))
      (should (= projection-calls 1))
      (should (= schedule-calls 1)))))

(ert-deftest magnus-coord-runtime-projection-recovers-from-cached-state ()
  "A projection crash leaves committed state and retries without rereading."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (snapshot 'snapshot-a)
           (state (magnus-coord-runtime-tests--state project snapshot))
           (snapshot-calls 0) (reduce-calls 0) (projection-calls 0)
           (schedule-calls 0))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (magnus-coord-runtime-tests--revision "same")))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project) (cl-incf snapshot-calls) snapshot))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (_snapshot) (cl-incf reduce-calls) state))
                ((symbol-function 'magnus-coord-state-write-projection)
                 (lambda (_state)
                   (cl-incf projection-calls)
                   (when (= projection-calls 1) (error "disk full"))))
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 (lambda (_project) (cl-incf schedule-calls))))
        (let ((failed (magnus-coord-runtime-start project)))
          (should (eq (magnus-coord-runtime-current-state project) state))
          (should (magnus-coord-runtime-result-projection-dirty failed))
          (should (string-match-p
                   "disk full"
                   (magnus-coord-runtime-result-projection-error failed))))
        (let ((recovered (magnus-coord-runtime-refresh project)))
          (should-not (magnus-coord-runtime-result-changed-p recovered))
          (should (magnus-coord-runtime-result-projection-written-p recovered))
          (should-not (magnus-coord-runtime-result-projection-dirty recovered))))
      (should (= snapshot-calls 1))
      (should (= reduce-calls 1))
      (should (= projection-calls 2))
      (should (= schedule-calls 1)))))

(ert-deftest magnus-coord-runtime-failed-snapshot-retries-same-revision ()
  "A pre-commit failure preserves the old token so the next pass retries."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (snapshot 'snapshot-a)
           (state (magnus-coord-runtime-tests--state project snapshot))
           (snapshot-calls 0))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (magnus-coord-runtime-tests--revision "same")))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project)
                   (cl-incf snapshot-calls)
                   (when (= snapshot-calls 1) (error "reader crashed"))
                   snapshot))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (_snapshot) state))
                ((symbol-function 'magnus-coord-state-write-projection)
                 (lambda (_state) "current.md"))
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 #'ignore))
        (let ((failed (magnus-coord-runtime-start project)))
          (should-not (magnus-coord-runtime-result-state failed))
          (should (string-match-p
                   "reader crashed"
                   (magnus-coord-runtime-result-refresh-error failed))))
        (let ((recovered (magnus-coord-runtime-refresh project)))
          (should (magnus-coord-runtime-result-changed-p recovered))
          (should (eq (magnus-coord-runtime-result-state recovered) state))))
      (should (= snapshot-calls 2)))))

(ert-deftest magnus-coord-runtime-transient-read-issue-retries-same-revision ()
  "A partial but reducible snapshot cannot make a transient omission sticky."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (snapshot-calls 0)
           (old-1 (magnus-coord-runtime-tests--log "old-1"))
           (old-2 (magnus-coord-runtime-tests--log "old-2"))
           (issue
            (magnus-coord-state-issue--create
             :code 'changed-entry :message "changed during bounded read"))
           (partial (magnus-coord-runtime-tests--state
                     project 'partial (list old-1) nil nil (list issue)))
           (complete (magnus-coord-runtime-tests--state
                      project 'complete (list old-1 old-2))))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (magnus-coord-runtime-tests--revision "same")))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project)
                   (cl-incf snapshot-calls)
                   (if (= snapshot-calls 1) 'partial 'complete)))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (snapshot)
                   (if (eq snapshot 'partial) partial complete)))
                ((symbol-function 'magnus-coord-state-write-projection)
                 #'ignore)
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 #'ignore))
        (let ((first (magnus-coord-runtime-start project))
              second third)
          (should (magnus-coord-runtime-result-changed-p first))
          (should
           (plist-get (magnus-coord-runtime-diagnostics project)
                      :retrying-transient-read))
          (setq second (magnus-coord-runtime-refresh project)
                third (magnus-coord-runtime-refresh project))
          (should (magnus-coord-runtime-result-changed-p second))
          (should-not (magnus-coord-runtime-result-new-logs second))
          (should-not (magnus-coord-runtime-result-changed-p third))
          (should-not
           (plist-get (magnus-coord-runtime-diagnostics project)
                      :retrying-transient-read))))
      (should (= snapshot-calls 2)))))

(ert-deftest magnus-coord-runtime-partial-retry-does-not-redeliver-old-logs ()
  "Retry preserves prior delivery IDs while recovering omitted evidence."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (token "one")
           (snapshot-calls 0)
           (old (magnus-coord-runtime-tests--log "old"))
           (new (magnus-coord-runtime-tests--log "new"))
           (issue
            (magnus-coord-state-issue--create
             :code 'read-error :message "temporarily unreadable"))
           (initial (magnus-coord-runtime-tests--state
                     project 'initial (list old)))
           (partial (magnus-coord-runtime-tests--state
                     project 'partial nil nil nil (list issue)))
           (complete (magnus-coord-runtime-tests--state
                      project 'complete (list old new))))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (magnus-coord-runtime-tests--revision token)))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project)
                   (cl-incf snapshot-calls)
                   (pcase snapshot-calls
                     (1 'initial) (2 'partial) (_ 'complete))))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (snapshot)
                   (pcase snapshot
                     ('initial initial) ('partial partial) (_ complete))))
                ((symbol-function 'magnus-coord-state-write-projection)
                 #'ignore)
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 #'ignore))
        (magnus-coord-runtime-start project)
        (setq token "two")
        (should-not
         (magnus-coord-runtime-result-new-logs
          (magnus-coord-runtime-refresh project)))
        (should
         (equal
          (mapcar
           #'magnus-coord-state-log-record-event-id
           (magnus-coord-runtime-result-new-logs
            (magnus-coord-runtime-refresh project)))
          '("new"))))
      (should (= snapshot-calls 3)))))

(ert-deftest magnus-coord-runtime-seeds-logs-then-returns-only-new-records ()
  "Startup history is silent while later unseen log records are returned."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (token "one")
           (snapshots 0)
           (log-1 (magnus-coord-runtime-tests--log "log-1"))
           (log-2 (magnus-coord-runtime-tests--log "log-2"))
           (state-1 (magnus-coord-runtime-tests--state
                     project 'snapshot-1 (list log-1)))
           (state-2 (magnus-coord-runtime-tests--state
                     project 'snapshot-2 (list log-1 log-2))))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (magnus-coord-runtime-tests--revision token)))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project)
                   (cl-incf snapshots)
                   (if (= snapshots 1) 'snapshot-1 'snapshot-2)))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (snapshot)
                   (if (eq snapshot 'snapshot-1) state-1 state-2)))
                ((symbol-function 'magnus-coord-state-write-projection)
                 #'ignore)
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 #'ignore))
        (should-not
         (magnus-coord-runtime-result-new-logs
          (magnus-coord-runtime-start project)))
        (setq token "two")
        (should
         (equal
          (mapcar
           #'magnus-coord-state-log-record-event-id
           (magnus-coord-runtime-result-new-logs
            (magnus-coord-runtime-refresh project)))
          '("log-2")))))))

(ert-deftest magnus-coord-runtime-delivers-a-burst-beyond-projection-limit ()
  "Every unseen log is delivered even when only 25 belong in current.md."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (token "one")
           (snapshots 0)
           (old (magnus-coord-runtime-tests--log "old"))
           (burst
            (cl-loop for number from 1 to 40
                     collect
                     (magnus-coord-runtime-tests--log
                      (format "new-%02d" number))))
           (state-1 (magnus-coord-runtime-tests--state
                     project 'snapshot-1 (list old)))
           (all (cons old burst))
           (state-2 (magnus-coord-runtime-tests--state
                     project 'snapshot-2 (last all 25) nil nil nil all)))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (magnus-coord-runtime-tests--revision token)))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project)
                   (cl-incf snapshots)
                   (if (= snapshots 1) 'snapshot-1 'snapshot-2)))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (snapshot)
                   (if (eq snapshot 'snapshot-1) state-1 state-2)))
                ((symbol-function 'magnus-coord-state-write-projection)
                 #'ignore)
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 #'ignore))
        (magnus-coord-runtime-start project)
        (setq token "two")
        (should
         (equal
          (mapcar
           #'magnus-coord-state-log-record-event-id
           (magnus-coord-runtime-result-new-logs
            (magnus-coord-runtime-refresh project)))
          (mapcar #'magnus-coord-state-log-record-event-id burst)))))))

(ert-deftest magnus-coord-runtime-reprojects-without-store-reads ()
  "A lifecycle-only projection change uses the committed cached state."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (state (magnus-coord-runtime-tests--state project 'snapshot))
           (revision-calls 0)
           (projection-calls 0))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (cl-incf revision-calls)
                   (magnus-coord-runtime-tests--revision "one")))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project) 'snapshot))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (_snapshot) state))
                ((symbol-function 'magnus-coord-state-write-projection)
                 (lambda (_state) (cl-incf projection-calls) "current.md"))
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 #'ignore))
        (magnus-coord-runtime-start project)
        (should (magnus-coord-runtime-reproject project))
        (should (= revision-calls 1))
        (should (= projection-calls 2))))))

(ert-deftest magnus-coord-runtime-review-settlement-controls-replay-and-gc ()
  "Reviews replay at startup until settled, then become pruneable."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (snapshot 'snapshot-a)
           (review-1 (magnus-coord-runtime-tests--review "review-1"))
           (review-2 (magnus-coord-runtime-tests--review "review-2"))
           (review-3 (magnus-coord-runtime-tests--review "review-3"))
           (state (magnus-coord-runtime-tests--state
                   project snapshot nil (list review-1 review-2 review-3)
                   '("active" "review-1" "review-2" "review-3")))
           prune-snapshot prune-keep)
      (setf (magnus-coord-state-sequence-anchor-event-ids state)
            '("review-1"))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (magnus-coord-runtime-tests--revision "same")))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project) snapshot))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (_snapshot) state))
                ((symbol-function 'magnus-coord-state-write-projection)
                 #'ignore)
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 #'ignore)
                ((symbol-function 'magnus-coord-store-prune)
                 (lambda (supplied keep)
                   (setq prune-snapshot supplied prune-keep keep)
                   (magnus-coord-store-prune-result--create
                    :deleted-events nil :kept-events nil :issues nil))))
        (let ((started (magnus-coord-runtime-start project)))
          (should
           (equal
            (mapcar #'magnus-coord-state-review-effect-event-id
                    (magnus-coord-runtime-result-unresolved-reviews started))
            '("review-1" "review-2" "review-3"))))
        (should (magnus-coord-runtime-settle-review project "review-1"))
        (should (magnus-coord-runtime-settle-review project "review-2"))
        (should-not (magnus-coord-runtime-settle-review project "review-1"))
        (should-error
         (magnus-coord-runtime-settle-review project "not-present"))
        (should
         (equal
          (mapcar
           #'magnus-coord-state-review-effect-event-id
           (magnus-coord-runtime-result-unresolved-reviews
            (magnus-coord-runtime-refresh project)))
          '("review-3")))
        (should (magnus-coord-runtime-run-gc project)))
      (should (eq prune-snapshot snapshot))
      ;; Settled review-1 remains because it is this writer's sequence anchor;
      ;; settled review-2 is obsolete and can be pruned.
      (should (equal prune-keep '("active" "review-1" "review-3"))))))

(ert-deftest magnus-coord-runtime-bounds-seen-and-settled-id-caches ()
  "A changed state retains only IDs that can still affect later results."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (token "one")
           (snapshot 'snapshot-1)
           (log-1 (magnus-coord-runtime-tests--log "log-1"))
           (log-2 (magnus-coord-runtime-tests--log "log-2"))
           (log-3 (magnus-coord-runtime-tests--log "log-3"))
           (review-1 (magnus-coord-runtime-tests--review "review-1"))
           (review-2 (magnus-coord-runtime-tests--review "review-2"))
           (review-3 (magnus-coord-runtime-tests--review "review-3"))
           (state-1 (magnus-coord-runtime-tests--state
                     project 'snapshot-1 (list log-1 log-2)
                     (list review-1 review-2)))
           (state-2 (magnus-coord-runtime-tests--state
                     project 'snapshot-2 (list log-2 log-3)
                     (list review-2 review-3))))
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (magnus-coord-runtime-tests--revision token)))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project) snapshot))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (value)
                   (if (eq value 'snapshot-1) state-1 state-2)))
                ((symbol-function 'magnus-coord-state-write-projection)
                 #'ignore)
                ((symbol-function 'magnus-coord-runtime-schedule-gc)
                 #'ignore))
        (magnus-coord-runtime-start project)
        (magnus-coord-runtime-settle-review project "review-1")
        (magnus-coord-runtime-settle-review project "review-2")
        (setq token "two" snapshot 'snapshot-2)
        (let ((result (magnus-coord-runtime-refresh project)))
          ;; log-2 survived the transition and is not redelivered.
          (should
           (equal
            (mapcar #'magnus-coord-state-log-record-event-id
                    (magnus-coord-runtime-result-new-logs result))
            '("log-3"))))
        (let ((diagnostics (magnus-coord-runtime-diagnostics project)))
          (should (= (plist-get diagnostics :seen-log-count) 2))
          ;; review-1 disappeared; review-2 is still settled and present.
          (should (= (plist-get diagnostics :settled-review-count) 1)))))))

(ert-deftest magnus-coord-runtime-gc-is-gated-debounced-and-diagnostic ()
  "Dirty projection blocks pruning; prune crashes only affect diagnostics."
  (magnus-coord-runtime-tests--isolated
    (let* ((project default-directory)
           (snapshot 'snapshot-a)
           (state (magnus-coord-runtime-tests--state
                   project snapshot nil nil '("active")))
           (write-ok nil) (prune-calls 0) (next-timer 0) cancelled)
      (cl-letf (((symbol-function 'magnus-coord-store-revision)
                 (lambda (_project)
                   (magnus-coord-runtime-tests--revision "same")))
                ((symbol-function 'magnus-coord-store-snapshot)
                 (lambda (_project) snapshot))
                ((symbol-function 'magnus-coord-state-reduce)
                 (lambda (_snapshot) state))
                ((symbol-function 'magnus-coord-state-write-projection)
                 (lambda (_state)
                   (unless write-ok (error "projection unavailable"))))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _arguments)
                   (intern (format "timer-%d" (cl-incf next-timer)))))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled)))
                ((symbol-function 'magnus-coord-store-prune)
                 (lambda (_snapshot _keep)
                   (cl-incf prune-calls)
                   (error "prune failed"))))
        (magnus-coord-runtime-start project)
        (should-not (magnus-coord-runtime-run-gc project))
        (should (= prune-calls 0))
        (should (plist-get (magnus-coord-runtime-diagnostics project)
                           :projection-dirty))
        (setq write-ok t)
        (magnus-coord-runtime-refresh project)
        (should (plist-get (magnus-coord-runtime-diagnostics project)
                           :gc-scheduled))
        (magnus-coord-runtime-schedule-gc project)
        (should (memq 'timer-1 cancelled))
        (should-not (magnus-coord-runtime-run-gc project))
        (should (= prune-calls 1))
        (should (string-match-p
                 "prune failed"
                 (plist-get (magnus-coord-runtime-diagnostics project)
                            :gc-error)))
        (should (eq (magnus-coord-runtime-current-state project) state))))))

(provide 'magnus-coord-runtime-tests)
;;; magnus-coord-runtime-tests.el ends here
