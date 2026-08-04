;;; magnus-review-tests.el --- Durable review tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-coord)
(require 'magnus-review)
(require 'magnus-review-controller)

(defconst magnus-test-review--base-oid
  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(defconst magnus-test-review--head-oid
  "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")

(ert-deftest magnus-review-worktree-update-defined-at-load-time ()
  "Guard against accidentally nesting the update defun inside create."
  (should (fboundp 'magnus-review-worktree-update)))

(ert-deftest magnus-review-ready-marker-parses-exact-checkpoint ()
  (let ((markers
         (magnus-coord--extract-review-ready
          (format
           (concat "ordinary log entry\n"
                   "[REVIEW-READY request=review-7 checkpoint=round.1:abc "
                   "base=%s head=%s]\n")
           magnus-test-review--base-oid
           (upcase magnus-test-review--head-oid)))))
    (should
     (equal markers
            (list
             (list :request-id "review-7"
                   :checkpoint-token "round.1:abc"
                   :base magnus-test-review--base-oid
                   :head magnus-test-review--head-oid))))))

(ert-deftest magnus-review-ready-marker-rejects-ambiguous-checkpoints ()
  (should-not
   (magnus-coord--extract-review-ready
    (format "[REVIEW-READY request=r base=%s head=%s]"
            magnus-test-review--base-oid magnus-test-review--head-oid)))
  (should-not
   (magnus-coord--extract-review-ready
    "[REVIEW-READY request=r checkpoint=a base=deadbeef head=cafebabe]")))

(ert-deftest magnus-review-ready-marker-replays-on-startup-once ()
  (let* ((directory (make-temp-file "magnus-review-marker-" t))
         (file (expand-file-name magnus-coord-file directory))
         (magnus-coord--processed-review-ready nil)
         seen
         (magnus-coord-review-ready-hook
          (list (lambda (root marker)
                  (push (cons root marker) seen)))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (format
              (concat "[REVIEW-READY request=review-recovery checkpoint=try-1 "
                      "base=%s head=%s]\n")
              magnus-test-review--base-oid magnus-test-review--head-oid)))
          (magnus-coord--init-processed-review-ready directory)
          (should (= (length seen) 1))
          ;; Ordinary polling remains deduplicated after startup replay.
          (magnus-coord--check-new-review-ready directory)
          (should (= (length seen) 1)))
      (delete-directory directory t))))

(ert-deftest magnus-review-ready-marker-retries-a-failed-handler ()
  (let* ((directory (make-temp-file "magnus-review-retry-" t))
         (file (expand-file-name magnus-coord-file directory))
         (magnus-coord--processed-review-ready nil)
         (magnus-coord--review-ready-retries
          (make-hash-table :test #'equal))
         (magnus-coord-review-ready-retry-count 3)
         (calls 0)
         scheduled
         (magnus-coord-review-ready-hook
          (list (lambda (_root _marker)
                  (cl-incf calls)
                  (when (= calls 1) (error "transient persistence error"))))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (format
              (concat "[REVIEW-READY request=review-retry checkpoint=try-1 "
                      "base=%s head=%s]\n")
              magnus-test-review--base-oid magnus-test-review--head-oid)))
          (cl-letf (((symbol-function 'run-with-timer)
                     (lambda (_delay _repeat callback &rest args)
                       (setq scheduled (cons callback args))
                       'fake-review-retry-timer))
                    ((symbol-function 'cancel-timer) #'ignore))
            (magnus-coord--check-new-review-ready directory)
            (should (= calls 1))
            (should scheduled)
            (should-not
             (alist-get directory magnus-coord--processed-review-ready
                        nil nil #'equal))
            (apply (car scheduled) (cdr scheduled))
            (should (= calls 2))
            (should (= (length
                        (alist-get
                         directory magnus-coord--processed-review-ready
                         nil nil #'equal))
                       1))
            (should (= (hash-table-count
                        magnus-coord--review-ready-retries)
                       0))))
      (delete-directory directory t))))

(ert-deftest magnus-review-ready-marker-replays-historical-round-tokens ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (round-one
                (magnus-review-round--create
                 :number 1 :base-oid base :head-oid head
                 :checkpoint-token "checkpoint-token-1"
                 :execution 'complete))
               (round-two
                (magnus-review-round--create
                 :number 2 :base-oid base :head-oid head
                 :checkpoint-token "checkpoint-token-2"
                 :execution 'queued))
               (review
                (magnus-review--create
                 :id "review-history" :project-root (file-truename root)
                 :checkpoint-token "checkpoint-token-2"
                 :lifecycle 'open :execution 'queued
                 :rounds (list round-one round-two)))
               (magnus-reviews (list review))
               (ready 0)
               (magnus-review-ready-hook
                (list (lambda (_review _round) (cl-incf ready)))))
    (unwind-protect
        (progn
          (should
           (eq (magnus-review-handle-ready-marker
                root (list :request-id "review-history"
                           :checkpoint-token "checkpoint-token-1"
                           :base base :head head))
               round-one))
          (should (= ready 0))
          (should
           (eq (magnus-review-handle-ready-marker
                root (list :request-id "review-history"
                           :checkpoint-token "checkpoint-token-2"
                           :base base :head head))
               round-two))
          (should (= ready 1)))
      (delete-directory root t))))

(defun magnus-test-review--git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY and return trimmed output."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil t nil
                         "-C" directory arguments)))
      (unless (and (integerp status) (zerop status))
        (error "Git fixture failed: %s" (buffer-string)))
      (string-trim (buffer-string)))))

(defun magnus-test-review--repository ()
  "Create a two-commit repository and return (ROOT BASE HEAD)."
  (let* ((root (make-temp-file "magnus-review-repo-" t))
         (source (expand-file-name "sample.el" root)))
    (magnus-test-review--git root "init" "--quiet")
    (magnus-test-review--git root "config" "user.name" "Magnus Test")
    (magnus-test-review--git root "config" "user.email" "test@example.com")
    (with-temp-file source
      (insert "(defun sample ()\n  1)\n"))
    (magnus-test-review--git root "add" "--" "sample.el")
    (magnus-test-review--git root "commit" "--quiet" "-m" "base")
    (let ((base (magnus-test-review--git root "rev-parse" "HEAD")))
      (with-temp-file source
        (insert "(defun sample ()\n  2)\n"))
      (magnus-test-review--git root "add" "--" "sample.el")
      (magnus-test-review--git root "commit" "--quiet" "-m" "head")
      (list root base (magnus-test-review--git root "rev-parse" "HEAD")))))

(defun magnus-test-review--raw-result (base head &optional path line)
  "Return valid structured output for BASE..HEAD, optionally anchored."
  `((schema_version . 1)
    (base_oid . ,base)
    (head_oid . ,head)
    (verdict . ,(if path "request_changes" "approve"))
    (summary . "Focused review fixture")
    (findings
     . ,(if path
            (vector
             `((severity . "major")
               (kind . "line")
               (title . "Fixture finding")
               (explanation . "The fixture exposes a concrete issue.")
               (path . ,path)
               (head_line . ,line)
               (end_line . ,line)
               (suggestion . "Correct the fixture.")
               (prior_id . nil)))
          []))
    (prior_findings . [])
    (strengths . ["Small committed scope"])
    (tests . ["Inspected the archived patch"])))

(ert-deftest magnus-review-recovers-tokened-result-before-model-retry ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-store-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil)
               (magnus-review-controller-changed-hook nil)
               (magnus-review-notify-on-completion nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "author-id" "quick-wolf"
                 :id "review-recovery"
                 :task "Exercise crash recovery"
                 :reviewer-name "wise-deer"
                 :reviewer-provider 'codex
                 :effort 'high))
               (round
                (magnus-review-append-round
                 review base head
                 :checkpoint-token (magnus-review-checkpoint-token review)))
               (attempt (magnus-review-append-attempt review round))
               (token (magnus-review-attempt-token attempt))
               (raw (magnus-test-review--raw-result
                     base head "not-in-the-diff.el" 99))
               (canonical
                (magnus-review-controller-anchor-result
                 review round
                 (magnus-review-controller-normalize-result review round raw)
                 (magnus-review-controller--patch review round)))
               (finding (aref (alist-get 'findings canonical) 0))
               (finding-id (alist-get 'id finding))
               (envelope
                (magnus-review-controller--result-envelope
                 review round attempt canonical)))
          (magnus-review-mark-attempt-running review round attempt token)
          ;; Simulate a crash boundary: result committed, report/manifest not.
          (magnus-review-write-artifact
           review (magnus-review-round-result-path review round)
           (magnus-review-controller--json envelope))
          (magnus-review-fail-attempt
           review round attempt "report publication interrupted" token)
          (should-not
           (file-exists-p (magnus-review-round-report-path review round)))
          (let ((persisted
                 (magnus-review-controller--read-json
                  (magnus-review-round-result-path review round))))
            (should persisted)
            (should (magnus-review-controller--envelope-valid-p
                     review round attempt persisted)))
          (cl-letf (((symbol-function
                      'magnus-review-controller--try-delivery)
                     (lambda (&rest _args) t)))
            (should
             (magnus-review-controller--adopt-artifacts review round)))
          (should (eq (magnus-review-round-execution round) 'complete))
          (should
           (file-regular-p (magnus-review-round-report-path review round)))
          ;; An anchor downgrade retains the ID derived from its original path.
          (let* ((persisted
                  (magnus-review-controller--read-json
                   (magnus-review-round-result-path review round)))
                 (body (magnus-review-controller--result-body persisted))
                 (recovered (aref (alist-get 'findings body) 0)))
            (should (equal (alist-get 'id recovered) finding-id))
            (should (equal (alist-get 'kind recovered) "general"))
            (should (equal (alist-get 'anchor_status recovered)
                           "path is outside the reviewed diff"))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-exact-owner-with-missing-context-releases-slot ()
  (let ((magnus-review-controller--processes
         (make-hash-table :test #'equal))
        (pumps 0)
        (process 'fake-review-process))
    (puthash "review-stale"
             (list :process process :round-number 2 :attempt-token "token-2")
             magnus-review-controller--processes)
    (cl-letf (((symbol-function 'magnus-review-controller--context)
               (lambda (&rest _args) nil))
              ((symbol-function 'magnus-review-controller--pump)
               (lambda () (cl-incf pumps))))
      (magnus-review-controller--complete
       process "review-stale" 2 "token-2" nil))
    (should (= pumps 1))
    (should-not (gethash "review-stale"
                         magnus-review-controller--processes))))

(ert-deftest magnus-review-success-promotes-provider-candidate-session ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-session-store-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil)
               (magnus-review-controller-changed-hook nil)
               (magnus-review-controller--queue nil)
               (magnus-review-controller--processes
                (make-hash-table :test #'equal))
               (magnus-review-notify-on-completion nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "author-id" "quick-wolf"
                 :id "review-session"
                 :task "Preserve reviewer continuity"
                 :reviewer-name "wise-deer"
                 :reviewer-provider 'claude))
               (round
                (magnus-review-append-round
                 review base head
                 :checkpoint-token (magnus-review-checkpoint-token review)))
               (attempt (magnus-review-append-attempt review round))
               (token (magnus-review-attempt-token attempt))
               (process 'successful-review-process))
          (magnus-review-mark-attempt-running review round attempt token)
          (puthash (magnus-review-id review)
                   (list :process process :round-number 1
                         :attempt-token token)
                   magnus-review-controller--processes)
          (cl-letf (((symbol-function
                      'magnus-review-controller--try-delivery)
                     (lambda (&rest _args) t)))
            (magnus-review-controller--complete
             process (magnus-review-id review) 1 token
             (list :success-p t
                   :candidate-session-id "claude-candidate-session"
                   :structured-result
                   (magnus-test-review--raw-result base head))))
          (should (equal (magnus-review-session-id review)
                         "claude-candidate-session"))
          (should (eq (magnus-review-round-execution round) 'complete))
          (should-not (gethash (magnus-review-id review)
                               magnus-review-controller--processes)))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-observer-error-cannot-skip-completion-effects ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-observer-store-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil)
               (magnus-review-controller-changed-hook nil)
               (magnus-review-controller--queue nil)
               (magnus-review-controller--processes
                (make-hash-table :test #'equal))
               (delivered nil)
               (notified nil)
               (observed nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "observer-author" "quick-wolf"
                 :id "observer-isolation"
                 :task "Keep post-commit effects independent"
                 :reviewer-name "wise-deer"
                 :reviewer-provider 'codex))
               (round
                (magnus-review-append-round
                 review base head
                 :checkpoint-token (magnus-review-checkpoint-token review)))
               (attempt (magnus-review-append-attempt review round))
               (token (magnus-review-attempt-token attempt))
               (process 'observer-review-process))
          (magnus-review-mark-attempt-running review round attempt token)
          (puthash (magnus-review-id review)
                   (list :process process :round-number 1
                         :attempt-token token)
                   magnus-review-controller--processes)
          ;; The first observer fails after the manifest replacement.  The
          ;; second observer and every completion side effect must still run.
          (setq magnus-reviews-changed-hook
                (list (lambda () (error "broken status observer"))
                      (lambda () (setq observed t))))
          (cl-letf (((symbol-function
                      'magnus-review-controller--try-delivery)
                     (lambda (&rest _args) (setq delivered t)))
                    ((symbol-function 'magnus-review-controller--notify)
                     (lambda (&rest _args) (setq notified t))))
            (magnus-review-controller--complete
             process (magnus-review-id review) 1 token
             (list :success-p t
                   :structured-result
                   (magnus-test-review--raw-result base head))))
          (should observed)
          (should delivered)
          (should notified)
          (should (eq (magnus-review-round-execution round) 'complete))
          (should
           (equal
            (alist-get
             'execution
             (magnus-review--read-json-file
              (magnus-review-manifest-path review)))
            "complete")))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-start-contains-attempt-allocation-errors ()
  (let* ((review (magnus-review--create
                  :id "closed-review" :reviewer-name "wise-deer"))
         (round (magnus-review-round--create :number 1))
         (magnus-review-controller--processes
          (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magnus-review-append-attempt)
               (lambda (&rest _args) (error "closed queue entry"))))
      (should-not (magnus-review-controller--start review round)))))

(ert-deftest magnus-review-local-delivery-defers-selected-tui ()
  (let* ((window (selected-window))
         (original-buffer (window-buffer window))
         (agent-buffer (generate-new-buffer " *magnus-review-agent*"))
         (other-buffer (generate-new-buffer " *magnus-review-other*"))
         (process
          (make-process :name "magnus-review-agent-fixture"
                        :buffer agent-buffer
                        :command (list (or (executable-find "cat") "cat"))
                        :noquery t))
         (instance (magnus-instance--create
                    :id "author-id" :name "quick-wolf"
                    :provider 'claude :buffer agent-buffer :status 'running))
         (magnus-review-controller--local-delivery-processes
          (make-hash-table :test #'eq))
         (magnus-review-controller--shutting-down nil)
         events
         (accepted 0))
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _args) 'fake-delivery-timer))
                  ((symbol-function 'vterm-send-string)
                   (lambda (text &optional paste-p)
                     (push (list 'paste text paste-p) events)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push 'return events))))
          (set-window-buffer window agent-buffer)
          (should (eq (magnus-review-controller--send
                       instance "durable feedback" (lambda () (cl-incf accepted)))
                      'queued))
          (should (= accepted 0))
          (should-not events)
          ;; Simulate the scheduled retry after Hrishi moves away from the TUI.
          (process-put process 'magnus-review-delivery-retry-timer nil)
          (set-window-buffer window other-buffer)
          (magnus-review-controller--drain-local-delivery process)
          (should (= accepted 1))
          (should (equal (nreverse events)
                         '((paste "durable feedback" t) return)))
          (should-not
           (process-get process 'magnus-review-delivery-queue)))
      (set-window-buffer window original-buffer)
      (when (process-live-p process) (delete-process process))
      (kill-buffer agent-buffer)
      (kill-buffer other-buffer))))

(ert-deftest magnus-review-local-delivery-retries-transient-vterm-error ()
  (let* ((window (selected-window))
         (original-buffer (window-buffer window))
         (agent-buffer (generate-new-buffer " *magnus-review-agent-retry*"))
         (other-buffer (generate-new-buffer " *magnus-review-away*"))
         (process
          (make-process :name "magnus-review-agent-retry-fixture"
                        :buffer agent-buffer
                        :command (list (or (executable-find "cat") "cat"))
                        :noquery t))
         (instance (magnus-instance--create
                    :id "author-retry-id" :name "quick-wolf"
                    :provider 'claude :buffer agent-buffer :status 'running))
         (magnus-review-controller--local-delivery-processes
          (make-hash-table :test #'eq))
         (magnus-review-controller--shutting-down nil)
         (send-attempts 0)
         (accepted 0))
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _args) 'fake-delivery-timer))
                  ((symbol-function 'vterm-send-string)
                   (lambda (&rest _args)
                     (when (= (cl-incf send-attempts) 1)
                       (error "transient vterm failure"))))
                  ((symbol-function 'vterm-send-return) #'ignore))
          (set-window-buffer window agent-buffer)
          (should (eq (magnus-review-controller--send
                       instance "durable retry" (lambda () (cl-incf accepted)))
                      'queued))
          (set-window-buffer window other-buffer)
          ;; The first drain fails before submission.  The entry and callback
          ;; must remain queued and a fresh retry must be scheduled.
          (process-put process 'magnus-review-delivery-retry-timer nil)
          (magnus-review-controller--drain-local-delivery process)
          (should (= accepted 0))
          (should (= send-attempts 1))
          (should (process-get process 'magnus-review-delivery-queue))
          (should (process-get process 'magnus-review-delivery-retry-timer))
          ;; Simulate that retry timer firing after vterm recovers.
          (process-put process 'magnus-review-delivery-retry-timer nil)
          (magnus-review-controller--drain-local-delivery process)
          (should (= accepted 1))
          (should (= send-attempts 2))
          (should-not
           (process-get process 'magnus-review-delivery-queue)))
      (set-window-buffer window original-buffer)
      (when (process-live-p process) (delete-process process))
      (kill-buffer agent-buffer)
      (kill-buffer other-buffer))))

(ert-deftest magnus-review-startup-orders-rounds-before-watcher-replay ()
  (let* ((older-review
          (magnus-review--create
           :id "older-logical-review" :project-root "/tmp/older"
           :lifecycle 'open :execution 'queued :created-at 1
           :rounds
           (list (magnus-review-round--create
                  :number 2 :created-at 30 :execution 'queued))))
         (newer-review
          (magnus-review--create
           :id "newer-logical-review" :project-root "/tmp/newer"
           :lifecycle 'open :execution 'queued :created-at 20
           :rounds
           (list (magnus-review-round--create
                  :number 1 :created-at 25 :execution 'queued))))
         (waiting-review
          (magnus-review--create
           :id "waiting-review" :project-root "/tmp/waiting"
           :lifecycle 'open :execution 'waiting-for-checkpoint
           :created-at 5 :rounds nil))
         (replayed-round
          (magnus-review-round--create
           :number 1 :created-at 35 :execution 'queued))
         (magnus-review-controller--queue nil)
         (magnus-review-controller--recovering nil)
         (magnus-review-controller--shutting-down nil)
         (magnus-review-controller--processes
          (make-hash-table :test #'equal))
         (magnus-review-ready-hook nil)
         (magnus-process-ready-hook nil)
         (magnus-reviews-changed-hook nil)
         (magnus-review-controller-changed-hook nil)
         (magnus-coord-review-ready-hook nil)
         (pump-calls 0)
         pump-recovery-states
         pumped-queue)
    (cl-letf (((symbol-function 'magnus-review-list)
               (lambda () (list older-review newer-review waiting-review)))
              ((symbol-function 'magnus-review-setup-coordination) #'ignore)
              ((symbol-function 'magnus-coord-ensure-file) #'ignore)
              ((symbol-function 'magnus-coord-start-watching)
               (lambda (_root)
                 (setf (magnus-review-rounds waiting-review)
                       (list replayed-round)
                       (magnus-review-execution waiting-review) 'queued)
                 (run-hook-with-args
                  'magnus-review-ready-hook waiting-review replayed-round)))
              ((symbol-function 'magnus-review-controller--pump)
               (lambda ()
                 (cl-incf pump-calls)
                 (push magnus-review-controller--recovering
                       pump-recovery-states)
                 (unless magnus-review-controller--recovering
                   (setq pumped-queue
                         (copy-tree magnus-review-controller--queue))))))
      (magnus-review-controller-setup))
    ;; Watcher replay asks the pump to run, but the recovering guard makes that
    ;; invocation inert; the final cleanup invocation owns actual startup work.
    (should (= pump-calls 2))
    (should (equal (nreverse pump-recovery-states) '(t nil)))
    (should
     (equal pumped-queue
            '(("newer-logical-review" . 1)
              ("older-logical-review" . 2)
              ("waiting-review" . 1))))))

(ert-deftest magnus-review-expertise-keeps-purged-identities-eligible ()
  (let* ((author (magnus-instance--create
                  :id "author" :name "quick-wolf" :status 'running))
         (expert (magnus-instance--create
                  :id "expert" :name "wise-deer" :status 'purged))
         (magnus-instances (list author expert))
         exclusions)
    (cl-letf (((symbol-function 'magnus-review-list) (lambda () nil))
              ((symbol-function 'magnus-expertise-match)
               (lambda (_root _task omitted)
                 (setq exclusions omitted)
                 (unless (member "wise-deer" omitted)
                   (list :name "wise-deer" :reason "domain expertise"))))
              ((symbol-function 'magnus--generate-random-name)
               (lambda (&rest _args) (ert-fail "expert match was skipped"))))
      (should (equal (magnus-review-controller--reviewer-name
                      "/tmp/project" "Review transport" author)
                     "wise-deer")))
    (should (member "quick-wolf" exclusions))
    (should-not (member "wise-deer" exclusions))))

(ert-deftest magnus-review-explicit-opposite-overrides-custom-default ()
  (let ((author (magnus-instance--create :provider 'codex))
        (magnus-review-default-provider 'codex))
    (should (eq (magnus-review-controller--provider author nil) 'codex))
    (should (eq (magnus-review-controller--provider author 'opposite)
                'claude))))

(ert-deftest magnus-review-process-ready-isolates-delivery-failures ()
  (let* ((instance (magnus-instance--create
                    :id "author-id" :name "quick-wolf"))
         (first-round (magnus-review-round--create
                       :number 1 :execution 'complete
                       :delivery-state 'pending))
         (second-round (magnus-review-round--create
                        :number 1 :execution 'complete
                        :delivery-state 'pending))
         (first (magnus-review--create
                 :id "first" :author-instance-id "author-id"
                 :lifecycle 'open :execution 'complete
                 :rounds (list first-round)))
         (second (magnus-review--create
                  :id "second" :author-instance-id "author-id"
                  :lifecycle 'open :execution 'complete
                  :rounds (list second-round)))
         attempted warnings)
    (cl-letf (((symbol-function 'magnus-review-list)
               ;; `--process-ready' reverses this list for oldest-first delivery.
               (lambda () (list second first)))
              ((symbol-function 'magnus-review-controller--try-delivery)
               (lambda (review &rest _args)
                 (push (magnus-review-id review) attempted)
                 (when (string= (magnus-review-id review) "first")
                   (error "broken first artifact"))))
              ((symbol-function 'magnus-review-controller--recovery-warning)
               (lambda (review operation _err)
                 (push (list (magnus-review-id review) operation) warnings))))
      (magnus-review-controller--process-ready instance))
    (should (equal (nreverse attempted) '("first" "second")))
    (should (equal warnings '(("first" "resurrection delivery"))))))

(ert-deftest magnus-review-completion-notification-survives-delivery-error ()
  (let* ((review (magnus-review--create :id "independent-effects"))
        (round (magnus-review-round--create :number 1))
        (result '((findings . []) (verdict . "approve")))
        (notified 0)
        (observed 0)
        (magnus-review-controller-changed-hook
         (list (lambda () (cl-incf observed)))))
    (cl-letf (((symbol-function 'magnus-review-controller--try-delivery)
               (lambda (&rest _args) (error "delivery disk error")))
              ((symbol-function 'magnus-review-controller--notify)
               (lambda (&rest _args) (cl-incf notified)))
              ((symbol-function 'display-warning) #'ignore))
      (magnus-review-controller--after-completion review round result))
    (should (= notified 1))
    (should (= observed 1))))

(ert-deftest magnus-review-queued-provider-delivery-stays-pending ()
  (let* ((round (magnus-review-round--create
                 :number 1 :execution 'complete :delivery-state 'pending))
         (review (magnus-review--create
                  :id "queued-delivery" :author-instance-id "author-id"
                  :rounds (list round)))
         (author (magnus-instance--create
                  :id "author-id" :name "wise-deer" :provider 'codex))
         accepted-callback)
    (cl-letf (((symbol-function 'magnus-review-controller--author-instance)
               (lambda (_review) author))
              ((symbol-function 'magnus-review-controller--send)
               (lambda (_instance _text &optional accepted)
                 (setq accepted-callback accepted)
                 'queued))
              ((symbol-function 'magnus-review-controller--delivery-message)
               (lambda (&rest _args) "durable review result")))
      (should
       (eq (magnus-review-controller--try-delivery
            review round
            '((verdict . "approve")
              (summary . "Ready")
              (findings . [])))
           'queued)))
    (should (functionp accepted-callback))
    (should (eq (magnus-review-round-delivery-state round) 'pending))))

(ert-deftest magnus-review-delivery-message-renders-durable-identity ()
  (let* ((round (magnus-review-round--create :number 3))
         (review
          (magnus-review--create
           :id "delivery-format" :project-hash (make-string 64 ?b)
           :author-name "quick-wolf" :reviewer-name "wise-deer"
           :reviewer-provider 'codex))
         (message
          (magnus-review-controller--delivery-message
           review round
           '((verdict . "request_changes")
             (summary . "Two concrete issues need attention.")
             (findings . [((id . "F-000000000001"))
                          ((id . "F-000000000002"))])))))
    (should (string-match-p
             "\\[MAGNUS-REVIEW-RESULT review=delivery-format round=3\\]"
             message))
    (should (string-match-p "Verdict: request_changes · findings: 2" message))
    (should (string-match-p "rounds/003/report\\.md" message))))

(ert-deftest magnus-review-retry-delivery-selects-newest-undelivered-round ()
  (let* ((older (magnus-review-round--create
                 :number 1 :execution 'complete :delivery-state 'failed))
         (newer (magnus-review-round--create
                 :number 2 :execution 'complete :delivery-state 'sent))
         (review (magnus-review--create
                  :id "historical-delivery" :reviewer-name "wise-deer"
                  :rounds (list older newer)))
         selected)
    (cl-letf (((symbol-function 'magnus-review-controller--try-delivery)
               (lambda (_review round &optional _result)
                 (setq selected round)
                 t)))
      (should (magnus-review-retry-delivery review)))
    (should (eq selected older))))

(ert-deftest magnus-review-start-cleanup-errors-do-not-escape ()
  (let* ((review (magnus-review--create
                  :id "start-cleanup" :reviewer-name "wise-deer"))
         (round (magnus-review-round--create :number 1))
         (attempt (magnus-review-attempt--create
                   :number 1 :token "0123456789abcdef"
                   :execution 'starting))
         (magnus-review-controller--processes
          (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magnus-review-append-attempt)
               (lambda (&rest _args) attempt))
              ((symbol-function 'magnus-review-worktree-create)
               (lambda (&rest _args) (error "worktree launch failed")))
              ((symbol-function 'magnus-review-fail-attempt)
               (lambda (&rest _args) (error "manifest is read-only"))))
      (should-not (magnus-review-controller--start review round)))
    (should (= (hash-table-count
                magnus-review-controller--processes)
               0))))

(ert-deftest magnus-review-start-recovers-mutate-then-save-failure ()
  (let* ((round (magnus-review-round--create
                 :number 1 :execution 'queued :attempts nil))
         (review (magnus-review--create
                  :id "mutate-then-throw" :reviewer-name "wise-deer"
                  :lifecycle 'open :execution 'queued :rounds (list round)))
         (attempt (magnus-review-attempt--create
                   :number 1 :token "0123456789abcdef"
                   :execution 'starting))
         failed-attempt)
    (cl-letf (((symbol-function 'magnus-review-append-attempt)
               (lambda (_review _round)
                 (setf (magnus-review-round-attempts round) (list attempt)
                       (magnus-review-round-execution round) 'starting
                       (magnus-review-execution review) 'starting)
                 (error "manifest replacement failed")))
              ((symbol-function 'magnus-review-fail-attempt)
               (lambda (_review _round candidate &rest _args)
                 (setq failed-attempt candidate)
                 (setf (magnus-review-attempt-execution candidate) 'failed))))
      (should-not (magnus-review-controller--start review round)))
    (should (eq failed-attempt attempt))
    (should (eq (magnus-review-attempt-execution attempt) 'failed))))

(ert-deftest magnus-review-pump-continues-after-unexpected-start-error ()
  (let* ((first-round (magnus-review-round--create
                       :number 1 :execution 'queued))
         (second-round (magnus-review-round--create
                        :number 1 :execution 'queued))
         (first (magnus-review--create
                 :id "first" :lifecycle 'open :execution 'queued
                 :rounds (list first-round)))
         (second (magnus-review--create
                  :id "second" :lifecycle 'open :execution 'queued
                  :rounds (list second-round)))
         (reviews `(("first" . ,first) ("second" . ,second)))
         (magnus-review-controller--queue
          '(("first" . 1) ("second" . 1)))
         (magnus-review-controller--processes
          (make-hash-table :test #'equal))
         (magnus-review-controller--shutting-down nil)
         (magnus-review-controller--recovering nil)
         attempted)
    (cl-letf (((symbol-function 'magnus-review-get)
               (lambda (id) (cdr (assoc id reviews))))
              ((symbol-function 'magnus-review-controller--start)
               (lambda (review _round)
                 (push (magnus-review-id review) attempted)
                 (when (string= (magnus-review-id review) "first")
                   (error "unexpected start bug"))))
              ((symbol-function 'display-warning) #'ignore))
      (magnus-review-controller--pump))
    (should (equal (nreverse attempted) '("first" "second")))
    (should-not magnus-review-controller--queue)))

(ert-deftest magnus-review-shutdown-cleans-every-owner-after-save-error ()
  (let* ((buffer-one (generate-new-buffer " *review-owner-one*"))
         (buffer-two (generate-new-buffer " *review-owner-two*"))
         (process-one
          (make-process :name "review-owner-one" :buffer buffer-one
                        :command (list (or (executable-find "cat") "cat"))
                        :noquery t))
         (process-two
          (make-process :name "review-owner-two" :buffer buffer-two
                        :command (list (or (executable-find "cat") "cat"))
                        :noquery t))
         (review-one (magnus-review--create :id "owner-one"))
         (review-two (magnus-review--create :id "owner-two"))
         (round-one (magnus-review-round--create :number 1))
         (round-two (magnus-review-round--create :number 1))
         (attempt-one (magnus-review-attempt--create :number 1))
         (attempt-two (magnus-review-attempt--create :number 1))
         (contexts
          `(("owner-one" . (,review-one ,round-one ,attempt-one))
            ("owner-two" . (,review-two ,round-two ,attempt-two))))
         (magnus-review-controller--processes
          (make-hash-table :test #'equal))
         (magnus-review-controller--local-delivery-processes
          (make-hash-table :test #'eq))
         (magnus-review-ready-hook
          (list #'magnus-review-controller--ready))
         (magnus-process-ready-hook
          (list #'magnus-review-controller--process-ready))
         (magnus-reviews-changed-hook
          (list #'magnus-review-controller--refresh-status))
         (magnus-review-controller-changed-hook
          (list #'magnus-review-controller--refresh-status))
         (magnus-coord-review-ready-hook
          (list #'magnus-review-handle-ready-marker))
         (magnus-review-ui-action-function #'magnus-review-actions)
         interrupted cancelled removed-hooks)
    (puthash "owner-one"
             (list :process process-one :round-number 1
                   :attempt-token "token-owner-one")
             magnus-review-controller--processes)
    (puthash "owner-two"
             (list :process process-two :round-number 1
                   :attempt-token "token-owner-two")
             magnus-review-controller--processes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-controller--context)
                   (lambda (review-id &rest _args)
                     (cdr (assoc review-id contexts))))
                  ((symbol-function 'magnus-review-interrupt-attempt)
                   (lambda (review &rest _args)
                     (push (magnus-review-id review) interrupted)
                     (when (string= (magnus-review-id review) "owner-one")
                       (error "manifest save failed"))))
                  ((symbol-function 'magnus-headless-cancel)
                   (lambda (process &optional _force)
                     (push process cancelled)))
                  ((symbol-function 'remove-hook)
                   (lambda (hook function &optional _local)
                     (push (cons hook function) removed-hooks))))
          (magnus-review-controller-shutdown)
          (should (= (length interrupted) 2))
          (should (= (length cancelled) 2))
          (should (= (hash-table-count
                      magnus-review-controller--processes)
                     0))
          (should (= (length removed-hooks) 5))
          (should (assoc 'magnus-review-ready-hook removed-hooks))
          (should (assoc 'magnus-process-ready-hook removed-hooks))
          (should (assoc 'magnus-reviews-changed-hook removed-hooks))
          (should (assoc 'magnus-review-controller-changed-hook removed-hooks))
          (should (assoc 'magnus-coord-review-ready-hook removed-hooks))
          (should-not magnus-review-ui-action-function))
      (when (process-live-p process-one) (delete-process process-one))
      (when (process-live-p process-two) (delete-process process-two))
      (kill-buffer buffer-one)
      (kill-buffer buffer-two))))

(ert-deftest magnus-review-release-starts-next-global-queue-item ()
  (let* ((first-round (magnus-review-round--create
                       :number 1 :execution 'queued))
         (second-round (magnus-review-round--create
                        :number 1 :execution 'queued))
         (first (magnus-review--create
                 :id "slot-first" :lifecycle 'open :execution 'queued
                 :rounds (list first-round)))
         (second (magnus-review--create
                  :id "slot-second" :lifecycle 'open :execution 'queued
                  :rounds (list second-round)))
         (reviews `(("slot-first" . ,first) ("slot-second" . ,second)))
         (magnus-review-controller--queue
          '(("slot-first" . 1) ("slot-second" . 1)))
         (magnus-review-controller--processes
          (make-hash-table :test #'equal))
         (magnus-review-controller--shutting-down nil)
         (magnus-review-controller--recovering nil)
         (magnus-review-max-concurrent 1)
         started)
    (cl-letf (((symbol-function 'magnus-review-get)
               (lambda (id) (cdr (assoc id reviews))))
              ((symbol-function 'magnus-review-controller--start)
               (lambda (review round)
                 (let* ((id (magnus-review-id review))
                        (process (intern (concat id "-process")))
                        (token (concat "token-" id)))
                   (push id started)
                   (setf (magnus-review-round-execution round) 'running)
                   (puthash id
                            (list :process process :round-number 1
                                  :attempt-token token)
                            magnus-review-controller--processes)
                   process))))
      (magnus-review-controller--pump)
      (should (equal started '("slot-first")))
      (should (equal magnus-review-controller--queue
                     '(("slot-second" . 1))))
      (magnus-review-controller--release
       'slot-first-process "slot-first" 1 "token-slot-first")
      (should (equal (nreverse started) '("slot-first" "slot-second")))
      (should-not magnus-review-controller--queue)
      (should (gethash "slot-second"
                       magnus-review-controller--processes)))))

(ert-deftest magnus-review-stale-callback-cannot-release-new-owner ()
  (let ((magnus-review-controller--processes
         (make-hash-table :test #'equal))
        (published 0)
        (pumped 0)
        (new-process 'new-owner-process))
    (puthash "review-cas"
             (list :process new-process :round-number 2
                   :attempt-token "new-attempt-token")
             magnus-review-controller--processes)
    (cl-letf (((symbol-function 'magnus-review-controller--publish-result)
               (lambda (&rest _args) (cl-incf published)))
              ((symbol-function 'magnus-review-controller--pump)
               (lambda () (cl-incf pumped))))
      (magnus-review-controller--complete
       'old-owner-process "review-cas" 2 "new-attempt-token"
       (list :success-p t))
      (magnus-review-controller--complete
       new-process "review-cas" 2 "old-attempt-token"
       (list :success-p t)))
    (should (= published 0))
    (should (= pumped 0))
    (should (eq (plist-get
                 (gethash "review-cas"
                          magnus-review-controller--processes)
                 :process)
                new-process))))

(ert-deftest magnus-review-interrupt-revokes-owner-before-cancel ()
  (let* ((attempt (magnus-review-attempt--create
                   :number 1 :token "interrupt-attempt-token"
                   :execution 'running))
         (round (magnus-review-round--create
                 :number 1 :execution 'running :attempts (list attempt)))
         (review (magnus-review--create
                  :id "interrupt-review" :reviewer-name "wise-deer"
                  :execution 'running :rounds (list round)))
         (process 'interrupt-review-process)
         (magnus-review-controller--processes
          (make-hash-table :test #'equal))
         interrupted
         cancelled
         (pumped 0))
    (puthash (magnus-review-id review)
             (list :process process :round-number 1
                   :attempt-token "interrupt-attempt-token")
             magnus-review-controller--processes)
    (cl-letf (((symbol-function 'magnus-review-controller--context)
               (lambda (&rest _args) (list review round attempt)))
              ((symbol-function 'magnus-review-interrupt-attempt)
               (lambda (&rest args) (setq interrupted args)))
              ((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'magnus-headless-cancel)
               (lambda (candidate &optional force)
                 (should-not
                  (gethash "interrupt-review"
                           magnus-review-controller--processes))
                 (setq cancelled (list candidate force))))
              ((symbol-function 'magnus-review-controller--pump)
               (lambda () (cl-incf pumped))))
      (should (eq (magnus-review-interrupt review) review)))
    (should (equal interrupted
                   (list review round attempt "Interrupted by user"
                         "interrupt-attempt-token" 'manual)))
    (should (equal cancelled (list process t)))
    (should (= pumped 1))
    (should-not
     (gethash "interrupt-review" magnus-review-controller--processes))))

(ert-deftest magnus-review-manual-interrupt-stays-stopped-after-reload ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-manual-stop-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil)
               (magnus-review-controller-changed-hook nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "manual-author-id" "quick-wolf"
                 :id "manual-interrupt"
                 :task "Keep an explicit stop durable"
                 :reviewer-name "wise-deer"
                 :reviewer-provider 'codex
                 :effort 'high))
               (round
                (magnus-review-append-round
                 review base head
                 :checkpoint-token (magnus-review-checkpoint-token review)))
               (attempt (magnus-review-append-attempt review round))
               loaded loaded-round loaded-attempt pump-snapshots)
          (magnus-review-mark-attempt-running
           review round attempt (magnus-review-attempt-token attempt))
          (magnus-review-interrupt-attempt
           review round attempt "Interrupted by user"
           (magnus-review-attempt-token attempt) 'manual)
          ;; Exercise the actual JSON manifest boundary, not only an in-memory
          ;; marker: restart recovery must preserve the user's stop intent.
          (setq magnus-reviews nil)
          (should (= (magnus-review-load-all) 1))
          (setq loaded (magnus-review-get "manual-interrupt")
                loaded-round (magnus-review-latest-round loaded)
                loaded-attempt (magnus-review-latest-attempt loaded-round))
          (should (eq (magnus-review-attempt-interruption-kind loaded-attempt)
                      'manual))
          (let ((magnus-review-controller--queue nil)
                (magnus-review-controller--processes
                 (make-hash-table :test #'equal))
                (magnus-review-controller--recovering nil)
                (magnus-review-controller--shutting-down nil)
                (magnus-review-ui-action-function nil))
            (cl-letf (((symbol-function 'magnus-review-list)
                       (lambda () (list loaded)))
                      ((symbol-function 'magnus-review-setup-coordination)
                       #'ignore)
                      ((symbol-function
                        'magnus-review-controller--adopt-artifacts)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'magnus-review-controller--pump)
                       (lambda ()
                         (push (copy-tree magnus-review-controller--queue)
                               pump-snapshots)))
                      ((symbol-function 'add-hook) #'ignore))
              (magnus-review-controller-setup)
              (should-not magnus-review-controller--queue)
              ;; An explicit retry remains available and queues this exact
              ;; immutable round without needing to manufacture a new task.
              (magnus-review-retry loaded)
              (should (equal magnus-review-controller--queue
                             '(("manual-interrupt" . 1))))))
          (should (equal (nreverse pump-snapshots)
                         '(nil (("manual-interrupt" . 1))))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-startup-retries-interruption-not-provider-failure ()
  (let* ((interrupted-round
          (magnus-review-round--create
           :number 1 :created-at 1 :execution 'interrupted))
         (failed-round
          (magnus-review-round--create
           :number 1 :created-at 2 :execution 'failed))
         (interrupted
          (magnus-review--create
           :id "interrupted" :project-root "/tmp/interrupted"
           :lifecycle 'open :execution 'interrupted
           :rounds (list interrupted-round)))
         (failed
          (magnus-review--create
           :id "failed" :project-root "/tmp/failed"
           :lifecycle 'open :execution 'failed
           :rounds (list failed-round)))
         (magnus-review-controller--queue nil)
         (magnus-review-controller--processes
          (make-hash-table :test #'equal))
         (magnus-review-controller--recovering nil)
         (magnus-review-controller--shutting-down nil)
         pumped)
    (cl-letf (((symbol-function 'magnus-review-list)
               (lambda () (list interrupted failed)))
              ((symbol-function 'magnus-review-setup-coordination) #'ignore)
              ((symbol-function 'magnus-review-controller--adopt-artifacts)
               (lambda (&rest _args) nil))
              ((symbol-function 'magnus-review-controller--pump)
               (lambda ()
                 (unless magnus-review-controller--recovering
                   (setq pumped
                         (copy-tree magnus-review-controller--queue)))))
              ((symbol-function 'add-hook) #'ignore))
      (magnus-review-controller-setup))
    (should (equal pumped '(("interrupted" . 1))))))

(ert-deftest magnus-review-refuses-mismatched-recovery-envelope ()
  (let* ((attempt (magnus-review-attempt--create
                   :number 1 :token "correct-attempt-token"
                   :execution 'failed))
         (round (magnus-review-round--create
                 :number 1
                 :base-oid magnus-test-review--base-oid
                 :head-oid magnus-test-review--head-oid
                 :execution 'failed :attempts (list attempt)))
         (review (magnus-review--create
                  :id "recovery-cas"
                  :project-hash (make-string 64 ?a)
                  :lifecycle 'open :execution 'failed
                  :rounds (list round)))
         (envelope
          `((artifact_schema_version . 1)
            (review_id . "recovery-cas")
            (round_number . 1)
            (attempt_token . "different-attempt-token")
            (base_oid . ,magnus-test-review--base-oid)
            (head_oid . ,magnus-test-review--head-oid)
            (result . ((verdict . "approve"))))))
    (cl-letf (((symbol-function 'magnus-review-controller--read-json)
               (lambda (_path) envelope)))
      (should-not
       (magnus-review-controller--adopt-artifacts review round)))
    (should (eq (magnus-review-round-execution round) 'failed))))

(provide 'magnus-review-tests)
;;; magnus-review-tests.el ends here
