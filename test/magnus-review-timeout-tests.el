;;; magnus-review-timeout-tests.el --- Ephemeral review timeout tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-review)
(require 'magnus-review-controller)

(ert-deftest magnus-review-scope-timeout-fails-only-in-memory-runtime ()
  (let* ((root (make-temp-file "magnus-review-scope-timeout-" t))
         (now (float-time))
         (review
          (magnus-review--create
           :id "scope-timeout"
           :project-root root
           :project-hash (magnus-review-compute-project-hash root)
           :author-instance-id "author-id"
           :author-name "quick-wren"
           :reviewer-name "keen-owl"
           :reviewer-provider 'codex
           :task "Review committed work"
           :lifecycle 'open
           :created-at now
           :updated-at now
           :rounds nil))
         (runtime
          (magnus-review-controller--make-runtime
           :phase 'asking-scope
           :nonce "expired-nonce"
           :cursor 'cursor
           :deadline (cons 'response (- now 1))))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review))
         cursor-read)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-trace-cursor-read)
                   (lambda (_cursor) (setq cursor-read t) nil)))
          (magnus-review-controller--poll-scope
           (magnus-review-id review) runtime)
          (should-not cursor-read)
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'failed))
          (should (string-match-p
                   "timeout"
                   (magnus-review-controller-runtime-error runtime)))
          (should-not (magnus-review-rounds review))
          (should-not (file-exists-p (magnus-review-manifest-path review))))
      (delete-directory root t))))

(ert-deftest magnus-review-direct-timeout-revokes-exact-process ()
  (let* ((root (make-temp-file "magnus-review-job-timeout-" t))
         (now (float-time))
         (review
          (magnus-review--create
           :id "job-timeout"
           :project-root root
           :project-hash (magnus-review-compute-project-hash root)
           :author-instance-id "author-id"
           :author-name "quick-wren"
           :reviewer-name "keen-owl"
           :reviewer-provider 'codex
           :task "Review committed work"
           :lifecycle 'open
           :created-at now
           :updated-at now
           :rounds nil))
         (round
          (magnus-review-round--create
           :number 1
           :base-oid (make-string 40 ?a)
           :head-oid (make-string 40 ?b)
           :created-at now))
         (runtime
          (magnus-review-controller--make-runtime
           :phase 'running
           :round round
           :process 'timed-process
           :timer 'review-timeout))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review))
         publish-called cancelled)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-controller--publish-result)
                   (lambda (&rest _arguments) (setq publish-called t)))
                  ((symbol-function 'magnus-headless-cancel)
                   (lambda (process &optional force)
                     (setq cancelled (list process force)))))
          (magnus-review-controller--review-timeout
           (magnus-review-id review) runtime 'timed-process)
          (should-not publish-called)
          (should (equal cancelled '(timed-process t)))
          (should-not (magnus-review-controller-runtime-process runtime))
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'failed))
          (should (stringp
                   (magnus-review-controller-runtime-error runtime)))
          (should (string-match-p
                   "timed out"
                   (magnus-review-controller-runtime-error runtime)))
          (should-not (magnus-review-rounds review))
          (should-not (magnus-review-session-id review)))
      (delete-directory root t))))

(ert-deftest magnus-review-direct-launch-failure-is-retryable-not-durable ()
  (let* ((root (make-temp-file "magnus-review-job-reject-" t))
         (now (float-time))
         (review
          (magnus-review--create
           :id "job-reject"
           :project-root root
           :project-hash (magnus-review-compute-project-hash root)
           :author-instance-id "author-id"
           :author-name "quick-wren"
           :reviewer-name "keen-owl"
           :reviewer-provider 'codex
           :task "Review committed work"
           :lifecycle 'open
           :created-at now
           :updated-at now
           :rounds nil))
         (round
          (magnus-review-round--create
           :number 1
           :base-oid (make-string 40 ?a)
           :head-oid (make-string 40 ?b)
           :created-at now))
         (runtime
          (magnus-review-controller--make-runtime
           :phase 'failed
           :round round))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal)))
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-ensure-checkout)
                   (lambda (&rest _arguments) root))
                  ((symbol-function 'magnus-headless-start)
                   (lambda (&rest _arguments) (error "launch rejected"))))
          (should-error
           (magnus-review-controller--start-round review runtime round))
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'failed))
          (should (string-match-p
                   "rejected"
                   (magnus-review-controller-runtime-error runtime)))
          (should-not (magnus-review-rounds review)))
      (delete-directory root t))))

(ert-deftest magnus-review-post-launch-timer-failure-cancels-real-child ()
  (let* ((root (make-temp-file "magnus-review-timer-failure-" t))
         (now (float-time))
         (review
          (magnus-review--create
           :id "timer-failure"
           :project-root root
           :project-hash (magnus-review-compute-project-hash root)
           :author-instance-id "author-id"
           :author-name "quick-wren"
           :reviewer-name "keen-owl"
           :reviewer-provider 'codex
           :task "Review committed work"
           :lifecycle 'open
           :created-at now
           :updated-at now
           :rounds nil))
         (round
          (magnus-review-round--create
           :number 1 :base-oid (make-string 40 ?a)
           :head-oid (make-string 40 ?b) :created-at now))
         (runtime
          (magnus-review-controller--make-runtime
           :phase 'failed :round round))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         cancelled)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-ensure-checkout)
                   (lambda (&rest _arguments) root))
                  ((symbol-function 'magnus-headless-start)
                   (lambda (&rest _arguments) 'launched-process))
                  ((symbol-function 'run-with-timer)
                   (lambda (&rest _arguments) (error "timer allocation failed")))
                  ((symbol-function 'magnus-headless-cancel)
                   (lambda (process &optional force)
                     (setq cancelled (list process force)))))
          (should-error
           (magnus-review-controller--start-round review runtime round))
          (should (equal cancelled '(launched-process t)))
          (should-not (magnus-review-controller-runtime-process runtime))
          (should (eq (magnus-review-controller-runtime-phase runtime) 'failed))
          (should (string-match-p
                   "timer allocation failed"
                   (magnus-review-controller-runtime-error runtime)))
          (should-not (magnus-review-rounds review)))
      (delete-directory root t))))

(ert-deftest magnus-review-direct-processes-run-concurrently-and-isolate-ownership ()
  (let* ((root (make-temp-file "magnus-review-concurrent-" t))
         (now (float-time))
         (make-review
          (lambda (id author reviewer)
            (magnus-review--create
             :id id :project-root root
             :project-hash (magnus-review-compute-project-hash root)
             :author-instance-id (concat id "-author-id")
             :author-name author :reviewer-name reviewer
             :reviewer-provider 'codex :task "Review committed work"
             :lifecycle 'open :created-at now :updated-at now :rounds nil)))
         (review-a (funcall make-review "concurrent-a" "quick-wren" "keen-owl"))
         (review-b (funcall make-review "concurrent-b" "bright-crow" "swift-hare"))
         (round-a
          (magnus-review-round--create
           :number 1 :base-oid (make-string 40 ?a)
           :head-oid (make-string 40 ?b) :created-at now))
         (round-b
          (magnus-review-round--create
           :number 1 :base-oid (make-string 40 ?c)
           :head-oid (make-string 40 ?d) :created-at now))
         (runtime-a (magnus-review-controller--make-runtime :round round-a))
         (runtime-b (magnus-review-controller--make-runtime :round round-b))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review-a review-b))
         (process-count 0)
         cancelled)
    (puthash (magnus-review-id review-a) runtime-a
             magnus-review-controller--runtimes)
    (puthash (magnus-review-id review-b) runtime-b
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-ensure-checkout)
                   (lambda (&rest _arguments) root))
                  ((symbol-function 'magnus-headless-start)
                   (lambda (&rest _arguments)
                     (intern (format "concurrent-process-%d"
                                     (cl-incf process-count)))))
                  ((symbol-function 'run-with-timer)
                   (lambda (&rest _arguments) (list 'review-timeout process-count)))
                  ((symbol-function 'magnus-headless-cancel)
                   (lambda (process &optional _force) (push process cancelled)))
                  ((symbol-function 'magnus-terminal-cancel-scope) #'ignore))
          (magnus-review-controller--start-round review-a runtime-a round-a)
          (magnus-review-controller--start-round review-b runtime-b round-b)
          (should (eq (magnus-review-controller-runtime-process runtime-a)
                      'concurrent-process-1))
          (should (eq (magnus-review-controller-runtime-process runtime-b)
                      'concurrent-process-2))
          (should (eq (magnus-review-controller-runtime-phase runtime-a) 'running))
          (should (eq (magnus-review-controller-runtime-phase runtime-b) 'running))

          (magnus-review-controller--complete-process
           (magnus-review-id review-a) runtime-a 'concurrent-process-1
           '(:success-p nil :error-message "review A failed"))
          (should (eq (magnus-review-controller-runtime-phase runtime-a) 'failed))
          (should (eq (magnus-review-controller-runtime-phase runtime-b) 'running))
          (should (eq (magnus-review-controller-runtime-process runtime-b)
                      'concurrent-process-2))

          ;; Neither a stale timeout nor interrupting B may touch A's replacement.
          (magnus-review-controller--review-timeout
           (magnus-review-id review-a) runtime-a 'concurrent-process-1)
          (magnus-review-interrupt review-b)
          (should (equal cancelled '(concurrent-process-2)))
          (should (eq (magnus-review-controller-runtime-phase runtime-a) 'failed))
          (should (eq (magnus-review-controller-runtime-phase runtime-b)
                      'interrupted)))
      (delete-directory root t))))

(provide 'magnus-review-timeout-tests)
;;; magnus-review-timeout-tests.el ends here
