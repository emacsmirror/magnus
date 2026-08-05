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
           :review-id (magnus-review-id review)
           :phase 'asking-scope
           :nonce "expired-nonce"
           :cursor 'cursor
           :deadline (- now 1)))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review))
         cursor-read)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-trace-cursor-read)
                   (lambda (_cursor) (setq cursor-read t) nil)))
          (magnus-review-controller--poll-scope
           (magnus-review-id review) "expired-nonce")
          (should-not cursor-read)
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'failed))
          (should (string-match-p
                   "timeout"
                   (magnus-review-controller-runtime-error runtime)))
          (should-not (magnus-review-rounds review))
          (should-not (file-exists-p (magnus-review-manifest-path review))))
      (delete-directory root t))))

(ert-deftest magnus-review-background-timeout-does-not-publish-candidate ()
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
           :review-id (magnus-review-id review)
           :phase 'running
           :round round
           :job-key '(timed-job)))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review))
         publish-called)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-controller--publish-result)
                   (lambda (&rest _arguments) (setq publish-called t))))
          (magnus-review-controller--complete-job
           (magnus-review-id review) '(timed-job)
           '(:success-p nil :timed-out-p t :background-error timeout
             :error-message "timed out after 30 seconds"))
          (should-not publish-called)
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'failed))
          (should (stringp
                   (magnus-review-controller-runtime-error runtime)))
          (should-not
           (string-empty-p
            (magnus-review-controller-runtime-error runtime)))
          (should-not (magnus-review-rounds review))
          (should-not (magnus-review-session-id review)))
      (delete-directory root t))))

(ert-deftest magnus-review-background-rejection-is-retryable-not-durable ()
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
           :review-id (magnus-review-id review)
           :phase 'queued
           :round round))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal)))
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-ensure-checkout)
                   (lambda (&rest _arguments) root))
                  ((symbol-function 'magnus-background-submit)
                   (lambda (&rest _arguments) nil)))
          (should-not
           (magnus-review-controller--start-round review runtime round))
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'failed))
          (should (string-match-p
                   "rejected"
                   (magnus-review-controller-runtime-error runtime)))
          (should-not (magnus-review-rounds review)))
      (delete-directory root t))))

(provide 'magnus-review-timeout-tests)
;;; magnus-review-timeout-tests.el ends here
