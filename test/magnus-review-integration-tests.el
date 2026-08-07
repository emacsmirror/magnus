;;; magnus-review-integration-tests.el --- Review pipeline integration test -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'magnus-review)
(require 'magnus-review-controller)

(defun magnus-test-review-integration--git (repository &rest arguments)
  "Run Git ARGUMENTS in REPOSITORY and return trimmed standard output."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil t nil
                         "-C" repository arguments)))
      (unless (and (integerp status) (zerop status))
        (error "Git fixture failed: %s" (buffer-string)))
      (string-trim-right (buffer-string)))))

(defun magnus-test-review-integration--commit
    (repository contents message)
  "Commit CONTENTS to sample.el in REPOSITORY with MESSAGE."
  (with-temp-file (expand-file-name "sample.el" repository)
    (insert contents))
  (magnus-test-review-integration--git repository "add" "--" "sample.el")
  (magnus-test-review-integration--git
   repository "commit" "--quiet" "-m" message)
  (magnus-test-review-integration--git repository "rev-parse" "HEAD"))

(ert-deftest magnus-review-integration-committed-scope-becomes-durable-round ()
  "Exercise Git evidence through controller completion and durable reload."
  (let* ((repository (make-temp-file "magnus-review-integration-repo-" t))
         (storage (make-temp-file "magnus-review-integration-store-" t))
         (magnus-review-directory-root storage)
         (magnus-reviews nil)
         (magnus-instances nil)
         (magnus-review-controller--runtimes
          (make-hash-table :test #'equal))
         (magnus-review-notify-on-completion nil)
         (provider-process (make-symbol "integration-reviewer-process"))
         provider-request provider-callbacks base head review candidate runtime)
    (unwind-protect
        (progn
          (magnus-test-review-integration--git repository "init" "--quiet")
          (magnus-test-review-integration--git
           repository "config" "user.name" "Magnus Integration Test")
          (magnus-test-review-integration--git
           repository "config" "user.email" "test@example.invalid")
          (setq base
                (magnus-test-review-integration--commit
                 repository "(defun answer () 41)\n" "base")
                head
                (magnus-test-review-integration--commit
                 repository "(defun answer () 42)\n" "return the answer")
                review
                (magnus-review-create
                 repository "author-integration" "quick-wren"
                 :id "integration-review"
                 :task "Review the committed answer change"
                 :reviewer-name "keen-owl"
                 :reviewer-provider 'codex
                 :model "integration-model"
                 :effort 'high)
                candidate (magnus-review-prepare-round review base head)
                runtime
                (magnus-review-controller--make-runtime
                 :phase 'running :round candidate))
          (puthash (magnus-review-id review) runtime
                   magnus-review-controller--runtimes)

          ;; The provider process is the only fake boundary.  Everything before
          ;; and after this callback is the production review pipeline.
          (cl-letf (((symbol-function 'magnus-headless-start)
                     (lambda (provider request &optional callbacks)
                       (setq provider-request (cons provider request)
                             provider-callbacks callbacks)
                       provider-process)))
            (should (eq (magnus-review-controller--start-round
                         review runtime candidate)
                        provider-process)))

          (should (eq (car provider-request) 'codex))
          (let ((request (cdr provider-request)))
            (should (equal (plist-get request :base) base))
            (should (equal (plist-get request :head) head))
            (should-not (plist-get request :session-id))
            (should (file-directory-p (plist-get request :directory)))
            (should (string-match-p
                     (regexp-quote (concat "Exact base object: " base))
                     (plist-get request :prompt)))
            (should (string-match-p
                     (regexp-quote (concat "Exact head object: " head))
                     (plist-get request :prompt))))

          (funcall
           (plist-get provider-callbacks :on-complete)
           provider-process
           (list
            :success-p t
            :session-id "integration-reviewer-session"
            :structured-result
            `((schema_version . 1)
              (base_oid . ,base)
              (head_oid . ,head)
              (verdict . "approve")
              (summary . "The committed change is focused and correct.")
              (findings . [])
              (prior_findings . [])
              (strengths . ["The patch changes only the intended return value."])
              (tests . ["Inspected the exact committed patch."]))))

          (should-not
           (gethash (magnus-review-id review)
                    magnus-review-controller--runtimes))
          (should (equal (magnus-review-session-id review)
                         "integration-reviewer-session"))
          (should (= (length (magnus-review-rounds review)) 1))
          (should (file-exists-p (magnus-review-manifest-path review)))

          ;; Reload from disk before reading artifacts so this assertion covers
          ;; the durable schema, not merely the object that published it.
          (setq magnus-reviews nil)
          (should (= (magnus-review-load-all) 1))
          (let* ((loaded (magnus-review-get "integration-review"))
                 (round (magnus-review-latest-round loaded))
                 (artifacts
                  (magnus-review-read-verified-artifacts loaded round))
                 (result (plist-get artifacts :result))
                 (handoff
                  (magnus-review-controller--author-message
                   loaded round result)))
            (should (magnus-review-round-p round))
            (should (equal (magnus-review-session-id loaded)
                           "integration-reviewer-session"))
            (should (equal (magnus-review-scope-base-oid round) base))
            (should (equal (magnus-review-scope-head-oid round) head))
            (should (eq (magnus-review-round-verdict round) 'approve))
            (should (equal (alist-get 'summary result)
                           "The committed change is focused and correct."))
            (should (string-match-p
                     "^diff --git a/sample\\.el b/sample\\.el"
                     (plist-get artifacts :patch)))
            (should (equal (plist-get artifacts :name-status)
                           "M\0sample.el\0"))
            (should (string-match-p
                     "MAGNUS-REVIEW-RESULT review=integration-review round=1"
                     handoff))
            (should (string-match-p
                     (regexp-quote (magnus-review-round-report-path loaded round))
                     handoff))))
      (ignore-errors (delete-directory repository t))
      (ignore-errors (delete-directory storage t)))))

(provide 'magnus-review-integration-tests)
;;; magnus-review-integration-tests.el ends here
