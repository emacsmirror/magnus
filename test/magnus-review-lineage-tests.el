;;; magnus-review-lineage-tests.el --- Completed lineage tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-review)

(defun magnus-test-lineage--git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY and return trimmed stdout."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil t nil
                         "-C" directory arguments)))
      (unless (zerop status)
        (error "Git failed: %s" (buffer-string)))
      (string-trim-right (buffer-string)))))

(defun magnus-test-lineage--write (path contents)
  "Write CONTENTS to fixture PATH."
  (with-temp-file path
    (insert contents)))

(defun magnus-test-lineage--commit (repository contents message)
  "Commit CONTENTS in REPOSITORY with MESSAGE and return its full OID."
  (magnus-test-lineage--write
   (expand-file-name "example.txt" repository) contents)
  (magnus-test-lineage--git repository "add" "example.txt")
  (magnus-test-lineage--git repository "commit" "-q" "-m" message)
  (magnus-test-lineage--git repository "rev-parse" "HEAD"))

(defun magnus-test-lineage--repository ()
  "Create a two-commit repository and return repository, base, and head."
  (let ((repository (make-temp-file "magnus-lineage-repository-" t)))
    (magnus-test-lineage--git repository "init" "-q")
    (magnus-test-lineage--git repository "config" "user.name" "Magnus Test")
    (magnus-test-lineage--git
     repository "config" "user.email" "magnus@example.invalid")
    (let ((base (magnus-test-lineage--commit repository "base\n" "base"))
          head)
      (setq head
            (magnus-test-lineage--commit repository "review me\n" "change"))
      (list repository base head))))

(defun magnus-test-lineage--draft (repository)
  "Create one canonical unsaved review draft in REPOSITORY."
  (magnus-review-create
   repository "author-id" "keen-owl"
   :id "lineage-review"
   :task "Review keen-owl's work"
   :reviewer-name "swift-hare"
   :reviewer-provider 'codex
   :model "gpt-test"
   :effort 'high))

(defun magnus-test-lineage--write-result (review round)
  "Write successful result artifacts for REVIEW candidate ROUND."
  (magnus-review-write-artifact
   review (magnus-review-round-result-path review round)
   "{\"verdict\":\"approve\"}\n" 'utf-8-unix t)
  (magnus-review-write-artifact
   review (magnus-review-round-report-path review round)
   "# Review\n\nApproved.\n" 'utf-8-unix t))

(ert-deftest magnus-review-lineage-create-is-an-unsaved-draft ()
  (pcase-let* ((`(,repository ,_base ,_head)
                 (magnus-test-lineage--repository))
                (storage (make-temp-file "magnus-lineage-storage-" t))
                (magnus-review-directory-root storage)
                (magnus-reviews nil))
    (unwind-protect
        (let ((review (magnus-test-lineage--draft repository)))
          (should (eq (magnus-review-get "lineage-review") review))
          (should (eq (magnus-review-execution review) 'idle))
          (should-not (magnus-review-rounds review))
          (should-not (file-exists-p (magnus-review-manifest-path review))))
      (delete-directory repository t)
      (delete-directory storage t))))

(ert-deftest magnus-review-lineage-preparation-is-ephemeral-and-exact ()
  (pcase-let* ((`(,repository ,base ,head)
                 (magnus-test-lineage--repository))
                (storage (make-temp-file "magnus-lineage-storage-" t))
                (magnus-review-directory-root storage)
                (magnus-reviews nil))
    (unwind-protect
        (let* ((review (magnus-test-lineage--draft repository))
               (round (magnus-review-prepare-round review base head)))
          (should-not (magnus-review-rounds review))
          (should-not (file-exists-p (magnus-review-manifest-path review)))
          (let ((scope (alist-get 'scope
                                  (magnus-review-round-metadata round))))
            (should (= (alist-get 'changed_file_count scope) 1))
            (should (stringp (alist-get 'shortstat scope)))
            (should-not (assq 'changed_files scope)))
          (should (file-regular-p
                   (magnus-review-round-patch-path review round)))
          (should (file-regular-p
                   (magnus-review-round-name-status-path review round)))
          (should
           (string=
            (magnus-test-lineage--git
             (magnus-review-round-checkout-path review round)
             "rev-parse" "HEAD")
            head))
          (should (magnus-review-discard-candidate review round)))
      (delete-directory repository t)
      (delete-directory storage t))))

(ert-deftest magnus-review-lineage-discard-candidate-is-safe-and-idempotent ()
  (pcase-let* ((`(,repository ,base ,head)
                 (magnus-test-lineage--repository))
                (storage (make-temp-file "magnus-lineage-storage-" t))
                (magnus-review-directory-root storage)
                (magnus-reviews nil))
    (unwind-protect
        (let* ((review (magnus-test-lineage--draft repository))
               (round (magnus-review-prepare-round review base head))
               (directory (magnus-review-round-directory review round))
               (checkout (magnus-review-round-checkout-path review round)))
          (should (file-directory-p directory))
          (should (file-directory-p checkout))
          (should (magnus-review-discard-candidate review round))
          (should-not (file-exists-p directory))
          (should-not (file-exists-p checkout))
          (should (magnus-review-discard-candidate review round))
          ;; Re-preparing the same unpublished scope is valid.  Once published,
          ;; neither object identity nor a repeated call may delete its evidence.
          (setq round (magnus-review-prepare-round review base head)
                directory (magnus-review-round-directory review round))
          (magnus-test-lineage--write-result review round)
          (magnus-review-complete-round review round 'approve)
          (should-error (magnus-review-discard-candidate review round)
                        :type 'magnus-review-error)
          (should (file-regular-p
                   (magnus-review-round-patch-path review round)))
          (should (file-regular-p
                   (magnus-review-round-result-path review round)))
          (should (file-directory-p directory)))
      (delete-directory repository t)
      (delete-directory storage t))))

(ert-deftest magnus-review-lineage-discard-refuses-escaped-and-symlinked-paths ()
  (pcase-let* ((`(,repository ,base ,head)
                 (magnus-test-lineage--repository))
                (storage (make-temp-file "magnus-lineage-storage-" t))
                (outside (make-temp-file "magnus-lineage-outside-" t))
                (magnus-review-directory-root storage)
                (magnus-reviews nil))
    (unwind-protect
        (let* ((review (magnus-test-lineage--draft repository))
               (round (magnus-review-prepare-round review base head))
               (directory (magnus-review-round-directory review round))
               (checkout (magnus-review-round-checkout-path review round))
               (original-base (magnus-review-round-base-oid round))
               (sentinel (expand-file-name "keep" outside)))
          (magnus-test-lineage--write sentinel "keep\n")
          (setf (magnus-review-round-base-oid round) "../escape")
          (should-error (magnus-review-discard-candidate review round)
                        :type 'magnus-review-error)
          (setf (magnus-review-round-base-oid round) original-base)
          (should (file-directory-p directory))
          (should (file-directory-p checkout))

          ;; Even a dangling or external checkout link is a refusal, not an
          ;; apparently successful no-op.
          (should (magnus-review-cleanup-round-checkout review round))
          (make-symbolic-link outside checkout)
          (should-error (magnus-review-discard-candidate review round)
                        :type 'magnus-review-error)
          (should (file-exists-p sentinel))
          (delete-file checkout)

          ;; The artifact directory itself receives the same treatment.
          (delete-directory directory t)
          (make-symbolic-link outside directory)
          (should-error (magnus-review-discard-candidate review round)
                        :type 'magnus-review-error)
          (should (file-exists-p sentinel))
          (delete-file directory))
      (delete-directory repository t)
      (delete-directory storage t)
      (delete-directory outside t))))

(ert-deftest magnus-review-lineage-preparation-error-cleans-candidate ()
  (pcase-let* ((`(,repository ,base ,head)
                 (magnus-test-lineage--repository))
                (storage (make-temp-file "magnus-lineage-storage-" t))
                (magnus-review-directory-root storage)
                (magnus-reviews nil))
    (unwind-protect
        (let* ((review (magnus-test-lineage--draft repository))
               (ensure (symbol-function 'magnus-review-ensure-checkout))
               candidate
               error-data)
          (cl-letf (((symbol-function 'magnus-review-ensure-checkout)
                     (lambda (candidate-review candidate-head round)
                       (setq candidate round)
                       (funcall ensure candidate-review candidate-head round)
                       (error "simulated preparation failure"))))
            (setq error-data
                  (should-error
                   (magnus-review-prepare-round review base head))))
          (should (string-match-p "simulated preparation failure"
                                  (error-message-string error-data)))
          (should candidate)
          (should-not
           (file-exists-p
            (magnus-review-round-directory review candidate)))
          (should-not
           (file-exists-p
            (magnus-review-round-checkout-path review candidate)))
          (should-not (magnus-review-rounds review)))
      (delete-directory repository t)
      (delete-directory storage t))))

(ert-deftest magnus-review-lineage-archive-blocks-actual-active-states ()
  (pcase-let* ((`(,repository ,_base ,_head)
                 (magnus-test-lineage--repository))
                (storage (make-temp-file "magnus-lineage-storage-" t))
                (magnus-review-directory-root storage)
                (magnus-reviews nil)
                (review (magnus-test-lineage--draft repository))
                (state nil)
                (magnus-review-runtime-state-function
                 (lambda (_review) state)))
    (unwind-protect
        (progn
          (dolist (active '(asking-scope queued running))
            (setq state active)
            (should-error (magnus-review-archive review)
                          :type 'magnus-review-error)
            (should (eq (magnus-review-lifecycle review) 'open)))
          (setq state 'failed)
          (should (eq (magnus-review-archive review) review))
          (should (eq (magnus-review-lifecycle review) 'archived)))
      (delete-directory repository t)
      (delete-directory storage t))))

(ert-deftest magnus-review-lineage-completes-multiple-changing-base-rounds ()
  (pcase-let* ((`(,repository ,base ,head)
                 (magnus-test-lineage--repository))
                (storage (make-temp-file "magnus-lineage-storage-" t))
                (magnus-review-directory-root storage)
                (magnus-reviews nil))
    (unwind-protect
        (let* ((review (magnus-test-lineage--draft repository))
               (round-one (magnus-review-prepare-round review base head)))
          (magnus-test-lineage--write-result review round-one)
          (magnus-review-complete-round
           review round-one 'comment :session-id "session-one")
          (let* ((new-head
                  (magnus-test-lineage--commit
                   repository "review me again\n" "followup"))
                 ;; A later author response may legitimately identify a new
                 ;; base as well as a new head.
                 (round-two
                  (magnus-review-prepare-round review head new-head)))
            (magnus-test-lineage--write-result review round-two)
            (magnus-review-complete-round
             review round-two 'approve :session-id "session-two")
            (should (= (length (magnus-review-rounds review)) 2))
            (should (string= (magnus-review-round-base-oid round-two) head))
            (should (string= (magnus-review-session-id review) "session-two"))
            (should (eq (magnus-review-read-state review) 'unread))
            (should (= magnus-review-schema-version 1))
            (setq magnus-reviews nil)
            (should (= (magnus-review-load-all) 1))
            (let ((loaded (magnus-review-get "lineage-review")))
              (should (= (length (magnus-review-rounds loaded)) 2))
              (should (string= (magnus-review-session-id loaded)
                               "session-two")))))
      (delete-directory repository t)
      (delete-directory storage t))))

(ert-deftest magnus-review-lineage-rejects-uncommitted-and-duplicate-scope ()
  (pcase-let* ((`(,repository ,base ,head)
                 (magnus-test-lineage--repository))
                (storage (make-temp-file "magnus-lineage-storage-" t))
                (magnus-review-directory-root storage)
                (magnus-reviews nil))
    (unwind-protect
        (let ((review (magnus-test-lineage--draft repository)))
          (magnus-test-lineage--write
           (expand-file-name "uncommitted.txt" repository) "dirty\n")
          (should-error (magnus-review-prepare-round review base head)
                        :type 'magnus-review-error)
          (delete-file (expand-file-name "uncommitted.txt" repository))
          (let ((round (magnus-review-prepare-round review base head)))
            (magnus-test-lineage--write-result review round)
            (magnus-review-complete-round review round 'approve)
            (should-error (magnus-review-prepare-round review base head)
                          :type 'magnus-review-error)))
      (delete-directory repository t)
      (delete-directory storage t))))

(ert-deftest magnus-review-lineage-publication-failure-is-retryable ()
  (pcase-let* ((`(,repository ,base ,head)
                 (magnus-test-lineage--repository))
                (storage (make-temp-file "magnus-lineage-storage-" t))
                (magnus-review-directory-root storage)
                (magnus-reviews nil))
    (unwind-protect
        (let* ((review (magnus-test-lineage--draft repository))
               (round (magnus-review-prepare-round review base head))
               (writer (symbol-function 'magnus-review--atomic-write-string)))
          (magnus-test-lineage--write-result review round)
          (cl-letf (((symbol-function 'magnus-review--atomic-write-string)
                     (lambda (file contents &optional coding)
                       (if (string= file (magnus-review-manifest-path review))
                           (error "simulated manifest failure")
                         (funcall writer file contents coding)))))
            (should-error
             (magnus-review-complete-round
              review round 'approve :session-id "retry-session")))
          (should-not (magnus-review-rounds review))
          (should-not (magnus-review-round-completed-at round))
          (should-not (magnus-review-round-verdict round))
          (should-not (magnus-review-session-id review))
          (magnus-review-complete-round
           review round 'approve :session-id "retry-session")
          (should (= (length (magnus-review-rounds review)) 1)))
      (delete-directory repository t)
      (delete-directory storage t))))

(provide 'magnus-review-lineage-tests)
;;; magnus-review-lineage-tests.el ends here
