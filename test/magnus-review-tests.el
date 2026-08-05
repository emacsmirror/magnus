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

(ert-deftest magnus-review-prompt-pins-the-exact-committed-range ()
  (let* ((review
          (magnus-review--create
           :id "prompt-scope"
           :reviewer-name "keen-owl"
           :task "Review the committed implementation"
           :metadata nil))
         (round
          (magnus-review-round--create
           :number 1
           :base-oid magnus-test-review--base-oid
           :head-oid magnus-test-review--head-oid))
         prompt)
    (cl-letf (((symbol-function 'magnus-review-controller--patch-path)
               (lambda (_review _round) "/tmp/canonical-evidence.patch")))
      (setq prompt
            (magnus-review-controller--review-prompt review round)))
    (should (string-match-p
             (regexp-quote
              (format "Exact base object: %s"
                      magnus-test-review--base-oid))
             prompt))
    (should (string-match-p
             (regexp-quote
              (format "Exact head object: %s"
                      magnus-test-review--head-oid))
             prompt))
    (should (string-match-p
             (regexp-quote
              (format "git diff --find-renames %s..%s --"
                      magnus-test-review--base-oid
                      magnus-test-review--head-oid))
             prompt))
    (should (string-match-p
             (regexp-quote "/tmp/canonical-evidence.patch") prompt))))

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
                 :rounds (list round-one round-two)
                 :checkpoint-requests
                 (list
                  (magnus-review-checkpoint-request--create
                   :number 1 :token "checkpoint-token-1"
                   :events
                   (list
                    (magnus-review-checkpoint-event--create
                     :kind 'round :round-number 1)))
                  (magnus-review-checkpoint-request--create
                   :number 2 :token "checkpoint-token-2"
                   :events
                   (list
                    (magnus-review-checkpoint-event--create
                     :kind 'round :round-number 2))))))
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

(defun magnus-test-review--publish-fixture-round (review round)
  "Make ROUND a structurally complete published result in REVIEW."
  (let* ((attempt (magnus-review-append-attempt review round))
         (now (float-time)))
    (setf (magnus-review-attempt-execution attempt) 'complete
          (magnus-review-attempt-finished-at attempt) now
          (magnus-review-round-execution round) 'complete
          (magnus-review-round-verdict round) 'changes-requested
          (magnus-review-round-completed-at round) now
          (magnus-review-round-delivery-state round) 'pending
          (magnus-review-round-read-state round) 'unread
          (magnus-review-execution review) 'complete
          (magnus-review-verdict review) 'changes-requested)
    (magnus-review-save review)
    round))

(defun magnus-test-review--schema-1-object (review)
  "Return a schema-1 manifest object preserving REVIEW's current history."
  (magnus-review--refresh-derived-fields review)
  (let* ((object (magnus-review--to-json review))
         (round-objects (append (alist-get 'rounds object) nil)))
    (setq object (assq-delete-all 'checkpoint_requests object))
    (setf (alist-get 'schema_version object) 1
          (alist-get 'execution object)
          (symbol-name (magnus-review-execution review))
          (alist-get 'verdict object)
          (and (magnus-review-verdict review)
               (symbol-name (magnus-review-verdict review)))
          (alist-get 'delivery_state object)
          (symbol-name (magnus-review-delivery-state review))
          (alist-get 'read_state object)
          (symbol-name (magnus-review-read-state review))
          (alist-get 'checkpoint_token object)
          (magnus-review-checkpoint-token review)
          (alist-get 'checkpoint_acks object)
          (vconcat
           (mapcar (lambda (ack) (vector (car ack) (cdr ack)))
                   (magnus-review-checkpoint-acks review)))
          (alist-get 'base_oid object) (magnus-review-base-oid review)
          (alist-get 'head_oid object) (magnus-review-head-oid review)
          (alist-get 'previous_head_oid object)
          (magnus-review-previous-head-oid review))
    (setq round-objects
          (cl-mapcar
           (lambda (round-object round)
             (cons
              (cons 'checkpoint_token
                    (magnus-review-round-checkpoint-token round))
              round-object))
           round-objects (magnus-review-rounds review)))
    (setf (alist-get 'rounds object) (vconcat round-objects))
    object))

(defun magnus-test-review--write-json-object (file object)
  "Write JSON OBJECT to FILE for a persistence-boundary fixture."
  (let ((json-encoding-pretty-print nil))
    (with-temp-file file
      (insert (json-serialize object :null-object nil
                              :false-object :json-false))
      (insert "\n"))))

(defun magnus-test-review--file-string (file)
  "Return FILE's exact textual contents."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest magnus-review-schema-2-serializes-ledger-not-aggregate-caches ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-schema-2-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "schema-author" "quick-wren"
                 :id "schema-two-ledger" :task "Exercise schema two"
                 :reviewer-name "keen-owl" :reviewer-provider 'codex))
               (token (magnus-review-checkpoint-token review))
               (same-token (magnus-review-await-checkpoint review))
               (round
                (magnus-review-append-round
                 review base head :checkpoint-token token))
               (manifest
                (magnus-review--read-json-file
                 (magnus-review-manifest-path review)))
               (request (car (alist-get 'checkpoint_requests manifest)))
               (event (car (alist-get 'events request)))
               (round-object (car (alist-get 'rounds manifest))))
          (should (equal same-token token))
          (should (= (length (magnus-review-checkpoint-requests review)) 1))
          (should (= (alist-get 'schema_version manifest) 2))
          (dolist (field '(execution verdict delivery_state read_state
                           checkpoint_token checkpoint_acks
                           base_oid head_oid previous_head_oid))
            (should-not (assq field manifest)))
          (should-not (assq 'checkpoint_token round-object))
          (should (= (alist-get 'number request) 1))
          (should (equal (alist-get 'token request) token))
          (should (equal (alist-get 'kind event) "round"))
          (should (= (alist-get 'round_number event) 1))
          (setq magnus-reviews nil)
          (let* ((loaded
                  (magnus-review-load-file
                   (magnus-review-manifest-path review)))
                 (loaded-round (magnus-review-latest-round loaded)))
            (should (eq (magnus-review-execution loaded) 'queued))
            (should (equal (magnus-review-checkpoint-token loaded) token))
            (should (equal (magnus-review-round-checkpoint-token loaded-round)
                           token))
            (should (= (magnus-review-round-number loaded-round)
                       (magnus-review-round-number round)))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-save-rolls-object-graph-back-in-place ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-rollback-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "rollback-author" "quick-wren"
                 :id "rollback-ledger" :task "Rollback failed saves"
                 :reviewer-name "keen-owl" :reviewer-provider 'codex))
               (request (magnus-review-current-checkpoint-request review))
               (round
                (magnus-review-append-round
                 review base head
                 :checkpoint-token
                 (magnus-review-checkpoint-request-token request)))
               (event (car (magnus-review-checkpoint-request-events request)))
               (attempt (magnus-review-append-attempt review round))
               (manifest (magnus-review-manifest-path review))
               (before (magnus-test-review--file-string manifest)))
          (cl-letf (((symbol-function 'magnus-review--atomic-write-string)
                     (lambda (&rest _args) (error "replacement failed"))))
            (should-error
             (magnus-review-mark-attempt-running
              review round attempt (magnus-review-attempt-token attempt))))
          (should (eq request (magnus-review-current-checkpoint-request review)))
          (should (eq event
                      (car (magnus-review-checkpoint-request-events request))))
          (should (eq round (magnus-review-latest-round review)))
          (should (eq attempt (magnus-review-latest-attempt round)))
          (should (eq (magnus-review-attempt-execution attempt) 'starting))
          (should (eq (magnus-review-round-execution round) 'starting))
          (should (eq (magnus-review-execution review) 'starting))
          (should (equal (magnus-test-review--file-string manifest) before))
          ;; Validation failures use the same durable snapshot and restore the
          ;; existing request object rather than replacing it with a clone.
          (let ((token (magnus-review-checkpoint-request-token request)))
            (setf (magnus-review-checkpoint-request-token request) "bad")
            (should-error (magnus-review-save review)
                          :type 'magnus-review-error)
            (should (eq request
                        (magnus-review-current-checkpoint-request review)))
            (should (equal
                     (magnus-review-checkpoint-request-token request) token))
            (should (equal (magnus-test-review--file-string manifest) before)))
          ;; Appending a brand-new request and then failing replacement restores
          ;; the old list; all pre-existing record identities remain reachable.
          (let ((now (float-time)))
            (setf (magnus-review-attempt-execution attempt) 'complete
                  (magnus-review-attempt-finished-at attempt) now
                  (magnus-review-round-execution round) 'complete
                  (magnus-review-round-verdict round) 'comment
                  (magnus-review-round-completed-at round) now
                  (magnus-review-round-delivery-state round) 'pending
                  (magnus-review-round-read-state round) 'unread)
            (magnus-review-save review))
          (let ((complete-bytes
                 (magnus-test-review--file-string manifest)))
            (cl-letf (((symbol-function 'magnus-review--atomic-write-string)
                       (lambda (&rest _args) (error "replacement failed"))))
              (should-error (magnus-review-await-checkpoint review)))
            (should (= (length (magnus-review-checkpoint-requests review)) 1))
            (should (eq request
                        (magnus-review-current-checkpoint-request review)))
            (should (eq event
                        (car (magnus-review-checkpoint-request-events request))))
            (should (eq round (magnus-review-latest-round review)))
            (should (eq attempt (magnus-review-latest-attempt round)))
            (should (eq (magnus-review-execution review) 'complete))
            (should (equal (magnus-test-review--file-string manifest)
                           complete-bytes))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-atomic-write-preserves-primary-error ()
  (let* ((directory (make-temp-file "magnus-review-atomic-error-" t))
         (target (expand-file-name "manifest.json" directory))
         observed)
    (unwind-protect
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest _args) (error "primary write failure")))
                  ((symbol-function 'delete-file)
                   (lambda (&rest _args) (error "cleanup failure"))))
          (condition-case err
              (magnus-review--atomic-write-string target "{}\n")
            (error (setq observed (error-message-string err))))
          (should (string-match-p "primary write failure" observed))
          (should-not (string-match-p "cleanup failure" observed)))
      (delete-directory directory t))))

(ert-deftest magnus-review-unchanged-checkpoint-settles-and-advances-durably ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-unchanged-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil)
               (ready 0)
               (magnus-review-ready-hook
                (list (lambda (_review _round) (cl-incf ready)))))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "unchanged-author-id" "bright-crow"
                 :id "unchanged-checkpoint"
                 :task "Review an already committed checkpoint"
                 :reviewer-name "swift-hare"
                 :reviewer-provider 'codex))
               (round
                (magnus-review-append-round
                 review base head
                 :checkpoint-token (magnus-review-checkpoint-token review))))
          (magnus-test-review--publish-fixture-round review round)
          (let* ((token (magnus-review-await-checkpoint review))
                 (marker (list :request-id (magnus-review-id review)
                               :checkpoint-token token
                               :base base :head head)))
            ;; A fresh re-review token can legitimately name the exact latest
            ;; completed evidence.  It spends no provider invocation and
            ;; settles this request; a later attempt requires a fresh token.
            (should (eq (magnus-review-handle-ready-marker root marker) round))
            (let ((settled-at (magnus-review-updated-at review)))
              (should
               (eq (magnus-review-handle-ready-marker root marker) round))
              (should (= (magnus-review-updated-at review) settled-at)))
            (should (eq (magnus-review-execution review) 'complete))
            (should (eq (magnus-review-verdict review) 'changes-requested))
            (should (equal (magnus-review-checkpoint-token review) token))
            (should (= (length (magnus-review-rounds review)) 1))
            (should (= (length (magnus-review-checkpoint-acks review)) 1))
            (should (= ready 0))
            (should (equal (magnus-review-checkpoint-acks review)
                           (list (cons token 1))))
            (with-temp-file (expand-file-name "sample.el" root)
              (insert "(defun sample ()\n  3)\n"))
            (magnus-test-review--git root "add" "--" "sample.el")
            (magnus-test-review--git root "commit" "--quiet" "-m" "advance")
            (let ((new-head
                   (magnus-test-review--git root "rev-parse" "HEAD")))
              ;; A no-progress acknowledgement also binds its fresh token to
              ;; the acknowledged round.  Reusing it for later commits is the
              ;; same permanent conflict as reusing a material round token.
              (let ((settled-at (magnus-review-updated-at review)))
                (should-error
                 (magnus-review-handle-ready-marker
                  root (plist-put (copy-sequence marker) :head new-head))
                 :type 'magnus-review-checkpoint-rejected)
                (should (= (magnus-review-updated-at review) settled-at))
                (should (eq (magnus-review-execution review) 'complete))
                (should (= (length (magnus-review-rounds review)) 1))
                (should (= (length (magnus-review-checkpoint-acks review)) 1))
                (should (= ready 0)))
              (let* ((next-token (magnus-review-await-checkpoint review))
                     (new-marker
                      (list :request-id (magnus-review-id review)
                            :checkpoint-token next-token
                            :base base :head new-head))
                     (round-two
                      (magnus-review-handle-ready-marker root new-marker)))
                (should (= (magnus-review-round-number round-two) 2))
                (should (equal (magnus-review-round-previous-head-oid round-two)
                               head))
                (should (= (length (magnus-review-rounds review)) 2))
                (should (= ready 1))
                (should-not (equal token next-token))
                ;; Once the token advances, neither its provisional nor
                ;; canonical scope permits an unrelated third object.  The
                ;; immutable bad marker is permanently rejected without changing
                ;; state; coordination consumes this specific condition so it
                ;; does not retry it forever.
                (should-error
                 (magnus-review-handle-ready-marker
                  root (plist-put (copy-sequence new-marker) :head
                                  "cccccccccccccccccccccccccccccccccccccccc"))
                 :type 'magnus-review-checkpoint-rejected)
                (should (= (length (magnus-review-rounds review)) 2))
                (should (eq (magnus-review-execution review) 'queued))
                (should (= ready 1))
                ;; Startup replays the whole coordination log.  Loading from
                ;; disk in file order: provisional first, canonical second.
                ;; Both are benign, and only queued canonical recovery runs the
                ;; hook.
                (setq magnus-reviews nil)
                (should (= (magnus-review-load-all) 1))
                (let* ((loaded (magnus-review-get "unchanged-checkpoint"))
                       (loaded-rounds (magnus-review-rounds loaded)))
                  (should (eq (magnus-review-handle-ready-marker root marker)
                              (nth 0 loaded-rounds)))
                  (should
                   (eq (magnus-review-handle-ready-marker root new-marker)
                       (nth 1 loaded-rounds)))
                  (should (eq (magnus-review-execution loaded) 'queued))
                  (should (= (length loaded-rounds) 2))
                  (should (= (length (magnus-review-checkpoint-acks loaded)) 1))
                  (should (= ready 2)))))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-conflicting-bound-token-is-a-terminal-rejection ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-conflict-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil)
               (magnus-coord--processed-review-ready nil)
               (magnus-coord--review-ready-retries
                (make-hash-table :test #'equal))
               (ready 0)
               (recovered nil)
               (magnus-review-ready-hook
                (list (lambda (_review _round) (cl-incf ready))))
               (magnus-review-checkpoint-mismatch-hook
                (list (lambda (review marker)
                        (setq recovered (list review marker))))))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "conflict-author" "quick-wren"
                 :id "bound-token-conflict"
                 :task "Reject a reused round token"
                 :reviewer-name "keen-owl"
                 :reviewer-provider 'codex))
               (bound-token (magnus-review-checkpoint-token review))
               (round
                (magnus-review-append-round
                 review base head :checkpoint-token bound-token))
               original-updated original-acks)
          (magnus-test-review--publish-fixture-round review round)
          (setq original-updated (magnus-review-updated-at review)
                original-acks
                (copy-tree (magnus-review-checkpoint-acks review)))
          (with-temp-file (expand-file-name "sample.el" root)
            (insert "(defun sample ()\n  3)\n"))
          (magnus-test-review--git root "add" "--" "sample.el")
          (magnus-test-review--git root "commit" "--quiet" "-m" "advance")
          (let* ((new-head (magnus-test-review--git root "rev-parse" "HEAD"))
                 (bad-marker
                  (list :request-id (magnus-review-id review)
                        :checkpoint-token bound-token
                        :base base :head new-head))
                 (file (expand-file-name magnus-coord-file root))
                 (magnus-coord-review-ready-hook
                  (list #'magnus-review-handle-ready-marker)))
            (with-temp-file file
              (insert
               (format
                "[REVIEW-READY request=%s checkpoint=%s base=%s head=%s]\n"
                (magnus-review-id review) bound-token base head))
              (insert
               (format
                "[REVIEW-READY request=%s checkpoint=%s base=%s head=%s]\n"
                (magnus-review-id review) bound-token base new-head)))
            ;; This exact production path must mark the permanent conflict as
            ;; processed instead of scheduling bounded retries and repeating
            ;; the user-facing handler error.
            (magnus-coord--check-new-review-ready root)
            (should (= (length
                        (alist-get
                         root magnus-coord--processed-review-ready
                         nil nil #'equal))
                       2))
            (should (= (hash-table-count
                        magnus-coord--review-ready-retries)
                       0))
            (should (= (length (magnus-review-rounds review)) 1))
            (should (eq (magnus-review-latest-round review) round))
            (should (eq (magnus-review-execution review) 'complete))
            (should (equal (magnus-review-head-oid review) head))
            (should (equal (magnus-review-checkpoint-token review)
                           bound-token))
            (should (equal (magnus-review-checkpoint-acks review)
                           original-acks))
            (should (= (magnus-review-updated-at review) original-updated))
            (should (= ready 0))
            (should-not recovered)
            ;; Once Hrishi intentionally requests another round, the same stale
            ;; marker remains rejected and recovers the new canonical request.
            (let ((fresh-token (magnus-review-await-checkpoint review)))
              (should-not (equal fresh-token bound-token))
              ;; Reproduce a restart: both historical markers replay through
              ;; coordination while the fresh request is waiting.  The stale
              ;; one is consumed and redelivers only the fresh current token.
              (setq magnus-coord--processed-review-ready nil
                    recovered nil)
              (magnus-coord--check-new-review-ready root)
              (should (= (length
                          (alist-get
                           root magnus-coord--processed-review-ready
                           nil nil #'equal))
                         2))
              (should (= (hash-table-count
                          magnus-coord--review-ready-retries)
                         0))
              (should (equal recovered (list review bad-marker)))
              (should (eq (magnus-review-execution review)
                          'waiting-for-checkpoint))
              (should (= (length (magnus-review-rounds review)) 1))
              (let ((round-two
                     (magnus-review-handle-ready-marker
                      root (list :request-id (magnus-review-id review)
                                 :checkpoint-token fresh-token
                                 :base base :head new-head))))
                (should (= (magnus-review-round-number round-two) 2))
                (should (= ready 1))
                (should (eq (magnus-review-execution review) 'queued))))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-load-settles-legacy-acknowledged-wait ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-acked-wait-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "legacy-author" "bright-crow"
                 :id "legacy-acknowledged-wait"
                 :task "Recover an acknowledged no-op"
                 :reviewer-name "swift-hare"
                 :reviewer-provider 'codex))
               (round
                (magnus-review-append-round
                 review base head
                 :checkpoint-token (magnus-review-checkpoint-token review))))
          (magnus-test-review--publish-fixture-round review round)
          (let ((token (magnus-review-await-checkpoint review)))
            (let ((object (magnus-test-review--schema-1-object review)))
              ;; Reproduce the durable shape written before unchanged
              ;; checkpoints became terminal no-op requests.
              (setf (alist-get 'execution object) "waiting-for-checkpoint"
                    (alist-get 'checkpoint_acks object)
                    (vector (vector token 1)))
              (magnus-test-review--write-json-object
               (magnus-review-manifest-path review) object)))
          (setq magnus-reviews nil)
          (should (= (magnus-review-load-all) 1))
          (let ((loaded (magnus-review-get "legacy-acknowledged-wait")))
            (should (eq (magnus-review-execution loaded) 'complete))
            (should (eq (magnus-review-verdict loaded) 'changes-requested))
            (should (= (length (magnus-review-checkpoint-acks loaded)) 1))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-migrates-two-round-schema-1-history-lazily ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-v1-two-rounds-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "fixture-author" "quick-wren"
                 :id "schema-one-two-rounds"
                 :task "Preserve every historical round"
                 :reviewer-name "keen-owl" :reviewer-provider 'codex))
               (token-one (magnus-review-checkpoint-token review))
               (round-one
                (magnus-review-append-round
                 review base head :checkpoint-token token-one)))
          ;; Mirror the live two-round manifest's first-round retry topology.
          (dotimes (_ 3)
            (let ((attempt (magnus-review-append-attempt review round-one)))
              (magnus-review-fail-attempt
               review round-one attempt "fixture retry")))
          (magnus-test-review--publish-fixture-round review round-one)
          (setf (magnus-review-session-id review) "stable-review-session")
          (magnus-review-save review)
          (with-temp-file (expand-file-name "sample.el" root)
            (insert "(defun sample ()\n  3)\n"))
          (magnus-test-review--git root "add" "--" "sample.el")
          (magnus-test-review--git root "commit" "--quiet" "-m" "advance")
          (let* ((new-head (magnus-test-review--git root "rev-parse" "HEAD"))
                 (token-two (magnus-review-await-checkpoint review))
                 (round-two
                  (magnus-review-append-round
                   review base new-head :checkpoint-token token-two)))
            (magnus-test-review--publish-fixture-round review round-two)
            (let* ((manifest (magnus-review-manifest-path review))
                   (patch-one (magnus-review-round-patch-path review round-one))
                   (patch-two (magnus-review-round-patch-path review round-two))
                   (patch-one-before
                    (magnus-test-review--file-string patch-one))
                   (patch-two-before
                    (magnus-test-review--file-string patch-two))
                   (object (magnus-test-review--schema-1-object review)))
              (magnus-test-review--write-json-object manifest object)
              (let ((schema-one-bytes
                     (magnus-test-review--file-string manifest)))
                (setq magnus-reviews nil)
                (should (= (magnus-review-load-all) 1))
                ;; Loading and migration are read-only until a real transition.
                (should (equal (magnus-test-review--file-string manifest)
                               schema-one-bytes))
                (let* ((loaded (magnus-review-get "schema-one-two-rounds"))
                       (requests
                        (magnus-review-checkpoint-requests loaded))
                       (loaded-rounds (magnus-review-rounds loaded)))
                  (should (= (length requests) 2))
                  (should (equal
                           (mapcar #'magnus-review-checkpoint-request-token
                                   requests)
                           (list token-one token-two)))
                  (should (equal
                           (mapcar
                            (lambda (request)
                              (mapcar
                               #'magnus-review-checkpoint-event-kind
                               (magnus-review-checkpoint-request-events
                                request)))
                            requests)
                           '((round) (round))))
                  (should (equal (magnus-review-session-id loaded)
                                 "stable-review-session"))
                  (should (= (length
                              (magnus-review-round-attempts
                               (nth 0 loaded-rounds)))
                             4))
                  (should (= (length
                              (magnus-review-round-attempts
                               (nth 1 loaded-rounds)))
                             1))
                  (should (equal (magnus-test-review--file-string patch-one)
                                 patch-one-before))
                  (should (equal (magnus-test-review--file-string patch-two)
                                 patch-two-before))
                  (magnus-review-mark-read loaded (nth 1 loaded-rounds))
                  (let ((schema-two
                         (magnus-review--read-json-file manifest)))
                    (should (= (alist-get 'schema_version schema-two) 2))
                    (should-not (assq 'execution schema-two))
                    (should-not
                     (assq 'checkpoint_token
                           (nth 1 (alist-get 'rounds schema-two))))))))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-migrates-legacy-unchanged-then-round-token ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-v1-dual-event-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "dual-author" "bright-crow"
                 :id "legacy-dual-event" :task "Preserve old replay history"
                 :reviewer-name "swift-hare" :reviewer-provider 'codex))
               (round-one
                (magnus-review-append-round
                 review base head
                 :checkpoint-token (magnus-review-checkpoint-token review))))
          (magnus-test-review--publish-fixture-round review round-one)
          (with-temp-file (expand-file-name "sample.el" root)
            (insert "(defun sample ()\n  3)\n"))
          (magnus-test-review--git root "add" "--" "sample.el")
          (magnus-test-review--git root "commit" "--quiet" "-m" "advance")
          (let* ((new-head (magnus-test-review--git root "rev-parse" "HEAD"))
                 (dual-token (magnus-review-await-checkpoint review))
                 (round-two
                  (magnus-review-append-round
                   review base new-head :checkpoint-token dual-token)))
            (magnus-test-review--publish-fixture-round review round-two)
            (let ((object (magnus-test-review--schema-1-object review)))
              ;; f909 briefly allowed this token to acknowledge round 1 and
              ;; later produce round 2.  Schema 2 preserves both replay scopes.
              (setf (alist-get 'checkpoint_acks object)
                    (vector (list dual-token 1)))
              (let* ((migrated
                      (magnus-review--from-json
                       object (magnus-review-id review)
                       (magnus-review-project-hash review)))
                     (request
                      (magnus-review-checkpoint-request-for-token
                       migrated dual-token)))
                (should (equal
                         (mapcar #'magnus-review-checkpoint-event-kind
                                 (magnus-review-checkpoint-request-events
                                  request))
                         '(unchanged round)))
                (setq magnus-reviews (list migrated))
                (should
                 (= (magnus-review-round-number
                     (magnus-review-handle-ready-marker
                      root (list :request-id (magnus-review-id migrated)
                                 :checkpoint-token dual-token
                                 :base base :head head)))
                    1))
                (should
                 (= (magnus-review-round-number
                     (magnus-review-handle-ready-marker
                      root (list :request-id (magnus-review-id migrated)
                                 :checkpoint-token dual-token
                                 :base base :head new-head)))
                    2))))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-schema-2-rejects-broken-checkpoint-timeline ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-bad-timeline-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "timeline-author" "quick-wren"
                 :id "broken-timeline" :task "Reject orphan rounds"
                 :reviewer-name "keen-owl" :reviewer-provider 'codex))
               (_round
                (magnus-review-append-round
                 review base head
                 :checkpoint-token (magnus-review-checkpoint-token review)))
               (object
                (magnus-review--read-json-file
                 (magnus-review-manifest-path review)))
               (request (car (alist-get 'checkpoint_requests object))))
          ;; The round remains present but no request produces it.
          (setf (alist-get 'events request) [])
          (should-error
           (magnus-review--from-json
            object (magnus-review-id review)
            (magnus-review-project-hash review))
           :type 'magnus-review-error))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-wrong-checkpoint-token-recovers-without-trusting-scope ()
  (pcase-let* ((`(,root ,base ,head) (magnus-test-review--repository))
               (storage (make-temp-file "magnus-review-wrong-token-" t))
               (magnus-review-directory-root storage)
               (magnus-reviews nil)
               (magnus-reviews-changed-hook nil)
               (ready 0)
               (recovered nil)
               (magnus-review-ready-hook
                (list (lambda (_review _round) (cl-incf ready))))
               (magnus-review-checkpoint-mismatch-hook
                (list (lambda (review marker)
                        (setq recovered (list review marker))))))
    (unwind-protect
        (let* ((review
                (magnus-review-create
                 root "compacted-author" "bright-crow"
                 :id "compacted-checkpoint"
                 :task "Recover after context compaction"
                 :reviewer-name "swift-hare"
                 :reviewer-provider 'codex))
               (bad-marker
                (list :request-id (magnus-review-id review)
                      :checkpoint-token
                      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      :base base :head head)))
          (should (eq (magnus-review-handle-ready-marker root bad-marker)
                      review))
          (should (equal recovered (list review bad-marker)))
          (should (eq (magnus-review-execution review)
                      'waiting-for-checkpoint))
          (should-not (magnus-review-rounds review))
          (should (= ready 0))
          ;; The durable token remains authoritative, and a later exact marker
          ;; proceeds normally after the rejected one.
          (let* ((good-marker
                  (plist-put
                   (copy-sequence bad-marker) :checkpoint-token
                   (magnus-review-checkpoint-token review)))
                 (round (magnus-review-handle-ready-marker root good-marker)))
            (should (= (magnus-review-round-number round) 1))
            (should (= ready 1))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-rereview-resends-waiting-checkpoint-token ()
  (let* ((token
          "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
         (request
          (magnus-review-checkpoint-request--create
           :number 1 :token token :requested-at 1 :events nil))
         (review
          (magnus-review--create
           :id "resend-checkpoint" :author-name "bright-crow"
           :reviewer-name "swift-hare" :lifecycle 'open
           :execution 'complete :checkpoint-token token
           :checkpoint-requests (list request)))
         delivered)
    (cl-letf (((symbol-function 'magnus-review-controller--deliver-checkpoint)
               (lambda (candidate exact-request)
                 (setq delivered
                       (list candidate exact-request
                             (magnus-review-checkpoint-request-token
                              exact-request)))
                 t)))
      (should (eq (magnus-review-rereview review) review)))
    (should (equal delivered (list review request token)))
    (should (equal (magnus-review-checkpoint-token review) token))))

(ert-deftest magnus-review-checkpoint-message-pins-the-exact-request ()
  (let* ((request
          (magnus-review-checkpoint-request--create
           :number 1 :token "exact-request-token"
           :requested-at 1 :events nil))
         (review
          (magnus-review--create
           :id "exact-message" :project-root "/tmp/project"
           :author-name "bright-crow" :task "Finish the controller"
           :checkpoint-token "stale-aggregate-token"))
         message)
    (cl-letf (((symbol-function 'magnus-review-suggest-upstream-scope)
               (lambda (_root)
                 (list :base-oid magnus-test-review--base-oid
                       :head-oid magnus-test-review--head-oid))))
      (setq message
            (magnus-review-controller--checkpoint-message review request)))
    (should (string-match-p
             (regexp-quote "checkpoint=exact-request-token") message))
    (should-not (string-match-p
                 (regexp-quote "stale-aggregate-token") message))))

(ert-deftest magnus-review-checkpoint-recovery-delivers-only-pending-request ()
  (let* ((request
          (magnus-review-checkpoint-request--create
           :number 1 :token "canonical-token" :requested-at 1 :events nil))
         (review
          (magnus-review--create
           :id "checkpoint-recovery" :lifecycle 'open
           ;; A deliberately stale aggregate must not suppress recovery.
           :execution 'complete :checkpoint-requests (list request)))
         delivered)
    (cl-letf (((symbol-function 'magnus-review-controller--deliver-checkpoint)
               (lambda (candidate exact-request)
                 (setq delivered (list candidate exact-request)))))
      (magnus-review-controller--recover-checkpoint-token review nil)
      (should (equal delivered (list review request)))
      (setq delivered nil)
      (setf (magnus-review-checkpoint-request-events request)
            (list
             (magnus-review-checkpoint-event--create
              :kind 'round :round-number 1 :recorded-at 2)))
      (magnus-review-controller--recover-checkpoint-token review nil)
      (should-not delivered))))

(ert-deftest magnus-review-checkpoint-delivery-rejects-stale-request-object ()
  (let* ((first
          (magnus-review-checkpoint-request--create
           :number 1 :token "old-token" :requested-at 1
           :events
           (list
            (magnus-review-checkpoint-event--create
             :kind 'round :round-number 1 :recorded-at 2))))
         (pending
          (magnus-review-checkpoint-request--create
           :number 2 :token "current-token" :requested-at 3 :events nil))
         (review
          (magnus-review--create
           :id "request-race"
           :checkpoint-requests (list first pending)))
         (author (magnus-instance--create :id "author" :name "bright-crow"))
         messaged)
    (cl-letf (((symbol-function 'magnus-review-controller--author-instance)
               (lambda (_review) author))
              ((symbol-function 'magnus-review-controller--checkpoint-message)
               (lambda (candidate request)
                 (setq messaged (list candidate request))
                 "checkpoint message"))
              ((symbol-function 'magnus-review-controller--send)
               (lambda (&rest _args) t)))
      (should-not
       (magnus-review-controller--deliver-checkpoint review first))
      (should-not messaged)
      (should
       (magnus-review-controller--deliver-checkpoint review pending))
      (should (equal messaged (list review pending))))))

(ert-deftest magnus-review-resend-refuses-resolved-ledger-despite-stale-cache ()
  (let* ((token
          "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")
         (round
          (magnus-review-round--create
           :number 1
           :base-oid "1111111111111111111111111111111111111111"
           :head-oid "2222222222222222222222222222222222222222"
           :execution 'complete))
         (request
          (magnus-review-checkpoint-request--create
           :number 1 :token token :requested-at 1
           :events
           (list
            (magnus-review-checkpoint-event--create
             :kind 'unchanged :round-number 1 :recorded-at 2))))
         (review
          (magnus-review--create
           :id "hot-loaded-checkpoint" :author-name "bright-crow"
           :reviewer-name "swift-hare" :lifecycle 'open
           :execution 'waiting-for-checkpoint :checkpoint-token token
           :checkpoint-acks (list (cons token 1)) :rounds (list round)
           :checkpoint-requests (list request))))
    (cl-letf (((symbol-function 'magnus-review-controller--deliver-checkpoint)
               (lambda (&rest _args)
                 (ert-fail "Resolved request was redelivered"))))
      (should-error (magnus-review-resend-checkpoint review)
                    :type 'user-error))))

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

(ert-deftest magnus-review-rejects-result-for-a-different-committed-range ()
  (let* ((review (magnus-review--create :id "result-scope"))
         (round
          (magnus-review-round--create
           :number 1
           :base-oid magnus-test-review--base-oid
           :head-oid magnus-test-review--head-oid))
         (other-oid "cccccccccccccccccccccccccccccccccccccccc"))
    (dolist (raw
             (list
              (magnus-test-review--raw-result
               other-oid magnus-test-review--head-oid)
              (magnus-test-review--raw-result
               magnus-test-review--base-oid other-oid)))
      (should-error
       (magnus-review-controller-normalize-result review round raw)
       :type 'error))))

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
          (let* ((manifest
                  (magnus-review--read-json-file
                   (magnus-review-manifest-path review)))
                 (round-object (car (alist-get 'rounds manifest))))
            (should (= (alist-get 'schema_version manifest) 2))
            (should-not (assq 'execution manifest))
            (should (equal (alist-get 'execution round-object)
                           "complete"))))
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
           :lifecycle 'open :execution 'complete
           :created-at 5 :rounds nil
           :checkpoint-requests
           (list
            (magnus-review-checkpoint-request--create
             :number 1 :token "waiting-token" :requested-at 5
             :events nil))))
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
                       (magnus-review-checkpoint-request-events
                        (magnus-review-current-checkpoint-request
                         waiting-review))
                       (list
                        (magnus-review-checkpoint-event--create
                         :kind 'round :round-number 1 :recorded-at 35))
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

(ert-deftest magnus-review-request-context-describes-the-exact-existing-action ()
  (let ((author
         (magnus-instance--create
          :id "author" :name "quick-wren" :directory "/tmp/project")))
    (cl-letf (((symbol-function 'magnus-review-git-root)
               (lambda (_directory) "/tmp/project"))
              ((symbol-function 'magnus-review-controller--task)
               (lambda (_author _root) "Review transport")))
      (dolist (case '((nil . new)
                      (complete . rereview)
                      (failed . retry)
                      (interrupted . retry)
                      (waiting-for-checkpoint . waiting)
                      (queued . queued)
                      (starting . running)
                      (running . running)))
        (let* ((execution (car case))
               (waiting (eq execution 'waiting-for-checkpoint))
               (review
                (and execution
                     (magnus-review--create
                      :id (symbol-name execution)
                      :author-instance-id "author"
                      :lifecycle 'open :execution execution
                      :rounds
                      (unless waiting
                        (list
                         (magnus-review-round--create
                          :number 1 :head-oid magnus-test-review--head-oid
                          :execution execution)))
                      :checkpoint-requests
                      (when waiting
                        (list
                         (magnus-review-checkpoint-request--create
                          :number 1 :token "waiting-token"
                          :requested-at 1 :events nil)))))))
          (cl-letf (((symbol-function
                      'magnus-review-controller--matching-open-review)
                     (lambda (_author _root _task) review)))
            (let ((context (magnus-review-request-context author)))
              (should (eq (plist-get context :action) (cdr case)))
              (should (plist-member context :state-key))
              (should (eq (plist-get context :review) review))
              (should (equal (plist-get context :root) "/tmp/project"))
              (should (equal (plist-get context :task)
                             "Review transport")))))))))

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

(ert-deftest magnus-review-process-ready-replays-canonical-pending-request ()
  (let* ((instance
          (magnus-instance--create
           :id "author-id" :name "quick-wolf"))
         (request
          (magnus-review-checkpoint-request--create
           :number 2 :token "canonical-token" :requested-at 2 :events nil))
         (review
          (magnus-review--create
           :id "pending" :author-instance-id "author-id"
           :lifecycle 'open
           ;; Deliberately contradict the ledger to prove the controller does
           ;; not use this compatibility cache as checkpoint authority.
           :execution 'complete :checkpoint-requests (list request)))
         delivered)
    (cl-letf (((symbol-function 'magnus-review-list)
               (lambda () (list review)))
              ((symbol-function 'magnus-review-controller--deliver-checkpoint)
               (lambda (candidate exact-request)
                 (setq delivered (list candidate exact-request)))))
      (magnus-review-controller--process-ready instance))
    (should (equal delivered (list review request)))))

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
         (magnus-review-checkpoint-mismatch-hook
          (list #'magnus-review-controller--recover-checkpoint-token))
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
          (should (= (length removed-hooks) 6))
          (should (assoc 'magnus-review-ready-hook removed-hooks))
          (should (assoc 'magnus-review-checkpoint-mismatch-hook
                         removed-hooks))
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
