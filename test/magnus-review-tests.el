;;; magnus-review-tests.el --- Ephemeral review controller tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'magnus-background)
(require 'magnus-review)
(require 'magnus-review-controller)

(defconst magnus-test-review--oid-a (make-string 40 ?a))
(defconst magnus-test-review--oid-b (make-string 40 ?b))
(defconst magnus-test-review--oid-c (make-string 40 ?c))
(defconst magnus-test-review--oid-d (make-string 40 ?d))
(defconst magnus-test-review--oid-e (make-string 40 ?e))

(defun magnus-test-review--git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY and return trimmed stdout."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil t nil
                         "-C" directory arguments)))
      (unless (and (integerp status) (zerop status))
        (error "Git fixture failed: %s" (buffer-string)))
      (string-trim-right (buffer-string)))))

(defun magnus-test-review--commit (repository contents message)
  "Commit CONTENTS in REPOSITORY with MESSAGE and return the full OID."
  (with-temp-file (expand-file-name "sample.el" repository)
    (insert contents))
  (magnus-test-review--git repository "add" "--" "sample.el")
  (magnus-test-review--git repository "commit" "--quiet" "-m" message)
  (magnus-test-review--git repository "rev-parse" "HEAD"))

(defun magnus-test-review--repository ()
  "Create a three-commit repository and return (ROOT BASE HEAD TIP)."
  (let ((root (make-temp-file "magnus-review-controller-repo-" t)))
    (magnus-test-review--git root "init" "--quiet")
    (magnus-test-review--git root "config" "user.name" "Magnus Test")
    (magnus-test-review--git root "config" "user.email" "test@example.invalid")
    (let ((base (magnus-test-review--commit root "base\n" "base"))
          head tip)
      (setq head (magnus-test-review--commit root "head\n" "reviewed change")
            tip (magnus-test-review--commit root "tip\n" "later change"))
      (list root base head tip))))

(defun magnus-test-review--review (root &optional rounds id)
  "Return a complete controller review fixture in ROOT with ROUNDS and ID."
  (let ((now (float-time)))
    (magnus-review--create
     :id (or id "controller-review")
     :project-root (directory-file-name (file-truename root))
     :project-hash (magnus-review-compute-project-hash root)
     :author-instance-id "author-id"
     :author-name "quick-wren"
     :reviewer-name "keen-owl"
     :reviewer-provider 'codex
     :model "review-model"
     :effort 'high
     :task "Review the committed implementation"
     :metadata nil
     :lifecycle 'open
     :created-at now
     :updated-at now
     :rounds rounds)))

(defun magnus-test-review--round (number base head &optional completed)
  "Return round NUMBER for BASE..HEAD, completed when COMPLETED is non-nil."
  (magnus-review-round--create
   :number number
   :base-oid base
   :head-oid head
   :created-at (float-time)
   :completed-at (and completed (float-time))
   :verdict (and completed 'comment)
   :read-state (and completed 'unread)
   :metadata nil))

(cl-defun magnus-test-review--raw-result
    (base head &key (verdict "comment") (summary "Careful review")
          findings prior-findings strengths tests)
  "Return one raw structured result for BASE..HEAD."
  `((schema_version . 1)
    (base_oid . ,base)
    (head_oid . ,head)
    (verdict . ,verdict)
    (summary . ,summary)
    (findings . ,(vconcat findings))
    (prior_findings . ,(vconcat prior-findings))
    (strengths . ,(vconcat (or strengths '("Focused change"))))
    (tests . ,(vconcat (or tests '("Inspected the patch"))))))

(cl-defun magnus-test-review--finding
    (title &key (severity "minor") (kind "file") (path "sample.el")
           line end-line suggestion prior-id)
  "Return one raw review finding titled TITLE."
  `((severity . ,severity)
    (kind . ,kind)
    (title . ,title)
    (explanation . ,(format "Evidence for %s" title))
    (path . ,(and (not (string= kind "general")) path))
    (head_line . ,(and (string= kind "line") line))
    (end_line . ,(and (string= kind "line") end-line))
    (suggestion . ,suggestion)
    (prior_id . ,prior-id)))

(defun magnus-test-review--disposition (id state)
  "Return a prior finding disposition for ID in STATE."
  `((id . ,id)
    (disposition . ,state)
    (explanation . ,(format "%s is %s" id state))))

(defun magnus-test-review--store-result (review round result)
  "Store canonical RESULT as ROUND's controller envelope."
  (let ((metadata
         (copy-tree (magnus-review-round-metadata round))))
    (setf (alist-get 'finding_count metadata)
          (length (append (alist-get 'findings result) nil))
          (alist-get 'result_sha256 metadata)
          (magnus-review-controller--result-digest result)
          (magnus-review-round-metadata round) metadata))
  (make-directory (magnus-review-round-directory review round) t)
  (with-temp-file (magnus-review-round-result-path review round)
    (insert
     (magnus-review-controller--json
      (magnus-review-controller--result-envelope review round result)))))

(defun magnus-test-review--prepare-publication-evidence (review round)
  "Write the two immutable evidence files required to publish ROUND."
  (magnus-review-write-artifact
   review (magnus-review-round-patch-path review round) "" 'utf-8-unix t)
  (magnus-review-write-artifact
   review (magnus-review-round-name-status-path review round) ""
   'utf-8-unix t))

(ert-deftest magnus-review-result-normalization-assigns-stable-finding-ids ()
  (let* ((root (make-temp-file "magnus-review-normalize-" t))
         (review (magnus-test-review--review root))
         (round (magnus-test-review--round
                 1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (finding (magnus-test-review--finding
                   "Guard the empty input" :kind "line" :line 7 :end-line 7))
         (raw (magnus-test-review--raw-result
               magnus-test-review--oid-a magnus-test-review--oid-b
               :findings (list finding)))
         first second)
    (unwind-protect
        (progn
          (setq first
                (magnus-review-controller-normalize-result review round raw)
                second
                (magnus-review-controller-normalize-result review round raw))
          (let ((first-id
                 (alist-get 'id (aref (alist-get 'findings first) 0)))
                (second-id
                 (alist-get 'id (aref (alist-get 'findings second) 0))))
            (should (string-match-p "\\`F-[[:xdigit:]]\\{12\\}\\'" first-id))
            (should (equal first-id second-id)))
          (should-error
           (magnus-review-controller-normalize-result
            review round (assq-delete-all 'tests (copy-tree raw)))))
      (delete-directory root t))))

(ert-deftest magnus-review-result-normalization-enforces-prior-round-trip ()
  (let* ((root (make-temp-file "magnus-review-prior-" t))
         (review (magnus-test-review--review root))
         (round-one (magnus-test-review--round
                     1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (round-two (magnus-test-review--round
                     2 magnus-test-review--oid-b magnus-test-review--oid-c))
         (prior
          (magnus-review-controller-normalize-result
           review round-one
           (magnus-test-review--raw-result
            magnus-test-review--oid-a magnus-test-review--oid-b
            :findings
            (list (magnus-test-review--finding "Still unsafe")))))
         (id (alist-get 'id (aref (alist-get 'findings prior) 0))))
    (unwind-protect
        (let* ((current
                (magnus-review-controller-normalize-result
                 review round-two
                 (magnus-test-review--raw-result
                  magnus-test-review--oid-b magnus-test-review--oid-c
                  :findings
                  (list (magnus-test-review--finding
                         "Still unsafe" :prior-id id))
                  :prior-findings
                  (list (magnus-test-review--disposition id "still_present")))
                 prior))
               (current-finding (aref (alist-get 'findings current) 0)))
          (should (equal (alist-get 'id current-finding) id))
          (should (equal (alist-get 'prior_id current-finding) id))
          (should-error
           (magnus-review-controller-normalize-result
            review round-two
            (magnus-test-review--raw-result
             magnus-test-review--oid-b magnus-test-review--oid-c
             :findings nil :prior-findings nil)
            prior)))
      (delete-directory root t))))

(ert-deftest magnus-review-history-fails-closed-on-invalid-durable-evidence ()
  (let* ((root (make-temp-file "magnus-review-history-root-" t))
         (storage (make-temp-file "magnus-review-history-store-" t))
         (magnus-review-directory-root storage)
         (round-one (magnus-test-review--round
                     1 magnus-test-review--oid-a
                     magnus-test-review--oid-b t))
         (round-two (magnus-test-review--round
                     2 magnus-test-review--oid-b
                     magnus-test-review--oid-c))
         (review (magnus-test-review--review root (list round-one)))
         (result
          (magnus-review-controller-normalize-result
           review round-one
           (magnus-test-review--raw-result
            magnus-test-review--oid-a magnus-test-review--oid-b
            :findings
            (list (magnus-test-review--finding "Keep this identity")))))
         (path (magnus-review-round-result-path review round-one)))
    (unwind-protect
        (progn
          (let ((err (should-error
                      (magnus-review-controller--history review round-two))))
            (should (string-match-p "Cannot read completed review round 1"
                                    (error-message-string err))))
          (make-directory (file-name-directory path) t)
          (with-temp-file path (insert "{"))
          (let ((err (should-error
                      (magnus-review-controller--history review round-two))))
            (should (string-match-p "Cannot read completed review round 1"
                                    (error-message-string err))))
          (let ((envelope
                 (magnus-review-controller--result-envelope
                  review round-one result)))
            (setf (alist-get 'review_id envelope) "another-review")
            (with-temp-file path
              (insert (magnus-review-controller--json envelope))))
          (let ((err (should-error
                      (magnus-review-controller--history review round-two))))
            (should (string-match-p "different review"
                                    (error-message-string err))))
          (magnus-test-review--store-result review round-one result)
          (should (equal (magnus-review-controller--history review round-two)
                         (list result)))
          (let ((tampered (copy-tree result)))
            (setf (alist-get 'summary tampered) "Altered after publication")
            (with-temp-file path
              (insert
               (magnus-review-controller--json
                (magnus-review-controller--result-envelope
                 review round-one tampered)))))
          (let ((err (should-error
                      (magnus-review-controller--history review round-two))))
            (should (string-match-p "digest disagrees"
                                    (error-message-string err))))
          (let ((tampered (copy-tree result)))
            (setf (alist-get 'findings tampered) [])
            (with-temp-file path
              (insert
               (magnus-review-controller--json
                (magnus-review-controller--result-envelope
                 review round-one tampered)))))
          (let ((err (should-error
                      (magnus-review-controller--history review round-two))))
            (should (string-match-p "finding count disagrees"
                                    (error-message-string err))))
          (let ((tampered (copy-tree result)))
            (setf (alist-get 'verdict tampered) "approve")
            (with-temp-file path
              (insert
               (magnus-review-controller--json
                (magnus-review-controller--result-envelope
                 review round-one tampered)))))
          (let ((err (should-error
                      (magnus-review-controller--history review round-two))))
            (should (string-match-p "verdict disagrees"
                                    (error-message-string err)))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-complete-lineage-reserves-and-resurrects-old-ids ()
  (let* ((root (make-temp-file "magnus-review-old-id-" t))
         (review (magnus-test-review--review root))
         (round-one (magnus-test-review--round
                     1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (round-two (magnus-test-review--round
                     2 magnus-test-review--oid-b magnus-test-review--oid-c))
         (round-three (magnus-test-review--round
                       3 magnus-test-review--oid-c magnus-test-review--oid-d))
         (round-four (magnus-test-review--round
                      4 magnus-test-review--oid-d magnus-test-review--oid-e))
         (result-one
          (magnus-review-controller-normalize-result
           review round-one
           (magnus-test-review--raw-result
            magnus-test-review--oid-a magnus-test-review--oid-b
            :findings
            (list (magnus-test-review--finding "Historical issue")))))
         (old-id (alist-get 'id (aref (alist-get 'findings result-one) 0)))
         (result-two
          (magnus-review-controller-normalize-result
           review round-two
           (magnus-test-review--raw-result
            magnus-test-review--oid-b magnus-test-review--oid-c
            :prior-findings
            (list (magnus-test-review--disposition old-id "resolved")))
           result-one))
         (result-three
          (magnus-review-controller-normalize-result
           review round-three
           (magnus-test-review--raw-result
            magnus-test-review--oid-c magnus-test-review--oid-d)
           result-two nil (list result-one)))
         (history (list result-one result-two result-three))
         (new-result
          (magnus-review-controller-normalize-result
           review round-four
           (magnus-test-review--raw-result
            magnus-test-review--oid-d magnus-test-review--oid-e
            :findings
            (list (magnus-test-review--finding "Historical issue")))
           result-three nil (list result-one result-two)))
         (resurfaced
          (magnus-review-controller-normalize-result
           review round-four
           (magnus-test-review--raw-result
            magnus-test-review--oid-d magnus-test-review--oid-e
            :findings
            (list (magnus-test-review--finding
                   "Historical issue" :prior-id old-id)))
           result-three nil (list result-one result-two)))
         (prompt (magnus-review-controller--review-prompt
                  review round-four history)))
    (unwind-protect
        (progn
          (should (equal
                   (alist-get 'id (aref (alist-get 'findings new-result) 0))
                   (concat old-id "-2")))
          (should (equal
                   (alist-get 'id (aref (alist-get 'findings resurfaced) 0))
                   old-id))
          (dolist (number '(1 2 3))
            (should
             (string-match-p
              (regexp-quote (format "\"round_number\":%d" number))
              prompt)))
          (should (string-match-p (regexp-quote old-id) prompt))
          (let ((magnus-review-lineage-prompt-limit 10))
            (should-error
             (magnus-review-controller--review-prompt
              review round-four history))))
      (delete-directory root t))))

(ert-deftest magnus-review-result-anchors-only-visible-head-lines ()
  (let* ((path "café.el")
         (patch
          (concat
           "diff --git a/café.el b/café.el\n"
           "+++ b/café.el\n"
           "@@ -0,0 +5,2 @@\n"
           "+visible\n"
           "+also visible\n"))
         (visible `((kind . "line") (path . ,path)
                    (head_line . 5) (end_line . 6)))
         (stale `((kind . "line") (path . ,path)
                  (head_line . 99) (end_line . 99)))
         (outside '((kind . "file") (path . "other.el")
                    (head_line . nil) (end_line . nil)))
         (result `((findings . ,(vector visible stale outside)))))
    (cl-letf (((symbol-function 'magnus-review-controller--changed-paths)
               (lambda (_review _round) (list path))))
      (magnus-review-controller-anchor-result nil nil result patch))
    (should (equal (alist-get 'kind visible) "line"))
    (should-not (alist-get 'anchor_status visible))
    (should (equal (alist-get 'kind stale) "file"))
    (should (string-match-p "not visible" (alist-get 'anchor_status stale)))
    (should (equal (alist-get 'kind outside) "general"))
    (should (string-match-p "outside" (alist-get 'anchor_status outside)))))

(ert-deftest magnus-review-scope-response-is-correlated-by-exact-nonce ()
  (let ((text
         (format
          (concat
           "[MAGNUS-REVIEW-SCOPE request=wrong status=ready base=%s head=%s]\n"
           "ordinary prose\n"
           "[MAGNUS-REVIEW-SCOPE request=right status=ready base=%s head=%s]")
          magnus-test-review--oid-c magnus-test-review--oid-d
          magnus-test-review--oid-a (upcase magnus-test-review--oid-b))))
    (should
     (equal (magnus-review-controller--parse-scope-response text "right")
            (list :status "ready"
                  :base magnus-test-review--oid-a
                  :head (upcase magnus-test-review--oid-b))))
    (should-not
     (magnus-review-controller--parse-scope-response text "absent"))))

(ert-deftest magnus-review-canonical-scope-accepts-reachable-commits-only ()
  (pcase-let* ((`(,root ,base ,head ,_tip)
                 (magnus-test-review--repository))
                (review (magnus-test-review--review root)))
    (unwind-protect
        (progn
          (should
           (equal (magnus-review-controller--canonical-scope
                   review base head)
                  (cons base head)))
          (should-error
           (magnus-review-controller--canonical-scope
            review (substring base 0 12) head)
           :type 'magnus-review-git-error)
          (with-temp-file (expand-file-name "dirty.el" root)
            (insert "dirty\n"))
          (should-error
           (magnus-review-controller--canonical-scope review base head)
           :type 'user-error))
      (delete-directory root t))))

(ert-deftest magnus-review-scope-poll-prepares-and-submits-background-candidate ()
  (let* ((root (make-temp-file "magnus-review-poll-" t))
         (review (magnus-test-review--review root))
         (round (magnus-test-review--round
                 1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (nonce "scope-nonce")
         (runtime
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id review)
           :phase 'asking-scope
           :nonce nonce
           :cursor 'fixture-cursor
           :deadline (+ (float-time) 60)))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review))
         prepared submission)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf
            (((symbol-function 'magnus-trace-cursor-read)
              (lambda (_cursor)
                (list
                 (format
                  "[MAGNUS-REVIEW-SCOPE request=%s status=ready base=%s head=%s]"
                  nonce magnus-test-review--oid-a magnus-test-review--oid-b))))
             ((symbol-function 'magnus-review-controller--canonical-scope)
              (lambda (_review base head) (cons base head)))
             ((symbol-function 'magnus-review-prepare-round)
              (lambda (candidate base head &rest _keys)
                (setq prepared (list candidate base head))
                round))
             ((symbol-function 'magnus-review-ensure-checkout)
              (lambda (&rest _arguments) root))
             ((symbol-function 'magnus-background-submit)
              (lambda (key provider request &optional callbacks)
                (setq submission (list key provider request callbacks))
                (magnus-background--make-job :key key :state 'queued))))
          (magnus-review-controller--poll-scope
           (magnus-review-id review) nonce)
          (should (equal prepared
                         (list review magnus-test-review--oid-a
                               magnus-test-review--oid-b)))
          (should (eq (nth 1 submission) 'codex))
          (let ((request (nth 2 submission)))
            (should (equal (plist-get request :base)
                           magnus-test-review--oid-a))
            (should (equal (plist-get request :head)
                           magnus-test-review--oid-b))
            (should (equal (plist-get request :name) "keen-owl"))
            (should-not (plist-get request :session-id)))
          (should-not (magnus-review-rounds review))
          (should (eq (magnus-review-controller-runtime-round runtime) round))
          (should (eq (magnus-review-controller-runtime-phase runtime) 'queued)))
      (delete-directory root t))))

(ert-deftest magnus-review-scope-timeout-starts-only-after-queued-delivery ()
  (let* ((root (make-temp-file "magnus-review-delivery-" t))
         (review (magnus-test-review--review root))
         (author
          (magnus-instance--create
           :id "author-id" :name "quick-wren" :directory root
           :provider 'claude :status 'running))
         (magnus-reviews (list review))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         receipt delivery-scope runtime cancelled-timers cancelled-scopes)
    (unwind-protect
        (cl-letf
            (((symbol-function
               'magnus-review-controller--require-committed-work)
              #'ignore)
             ((symbol-function
               'magnus-review-controller--instance-running-p)
              (lambda (_instance) t))
             ((symbol-function 'magnus-trace-cursor-create)
              (lambda (_instance) 'cursor))
             ((symbol-function 'magnus-review-controller--send)
              (lambda (_instance _text accepted scope)
                (setq receipt accepted delivery-scope scope)
                'queued))
             ((symbol-function 'run-with-timer)
              (lambda (_seconds repeat &rest _arguments)
                (if repeat 'scope-poll-timer 'scope-delivery-timer)))
             ((symbol-function 'timerp)
              (lambda (timer)
                (memq timer '(scope-delivery-timer scope-poll-timer))))
             ((symbol-function 'cancel-timer)
              (lambda (timer) (push timer cancelled-timers)))
             ((symbol-function 'magnus-terminal-cancel-scope)
              (lambda (scope) (push scope cancelled-scopes)))
             ((symbol-function 'magnus-review-controller--changed) #'ignore))
          (magnus-review-controller--begin-scope-query review author)
          (setq runtime (magnus-review-controller--runtime review))
          (should (eq delivery-scope runtime))
          (should (functionp receipt))
          (should-not (magnus-review-controller-runtime-deadline runtime))
          (should-not (magnus-review-controller-runtime-timer runtime))
          (should (eq
                   (magnus-review-controller-runtime-delivery-timer runtime)
                   'scope-delivery-timer))
          (funcall receipt)
          (should-not
           (magnus-review-controller-runtime-delivery-timer runtime))
          (should (numberp
                   (magnus-review-controller-runtime-deadline runtime)))
          (should (eq (magnus-review-controller-runtime-timer runtime)
                      'scope-poll-timer))
          (magnus-review-interrupt review)
          (should (memq 'scope-delivery-timer cancelled-timers))
          (should (memq 'scope-poll-timer cancelled-timers))
          (should (memq runtime cancelled-scopes))
          ;; A delayed receipt cannot revive interrupted ownership.
          (funcall receipt)
          (should-not (magnus-review-controller-runtime-timer runtime))
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'interrupted)))
      (delete-directory root t))))

(ert-deftest magnus-review-undelivered-scope-question-times-out-in-memory ()
  (let* ((root (make-temp-file "magnus-review-undelivered-" t))
         (review (magnus-test-review--review root))
         (runtime
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id review)
           :phase 'asking-scope :nonce "delivery-nonce" :cursor 'cursor
           :delivery-timer 'delivery-watchdog))
         (magnus-reviews (list review))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         cancelled-scope)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-terminal-cancel-scope)
                   (lambda (scope) (setq cancelled-scope scope)))
                  ((symbol-function 'magnus-review-controller--changed)
                   #'ignore))
          (magnus-review-controller--scope-delivery-timeout
           (magnus-review-id review) "delivery-nonce" runtime)
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'failed))
          (should (eq cancelled-scope runtime))
          (should-not
           (magnus-review-controller-runtime-delivery-timer runtime))
          (should (string-match-p
                   "did not accept"
                   (magnus-review-controller-runtime-error runtime))))
      (delete-directory root t))))

(ert-deftest magnus-review-scope-poll-scheduler-failure-is-retryable ()
  (let* ((root (make-temp-file "magnus-review-poll-failure-" t))
         (review (magnus-test-review--review root))
         (runtime
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id review)
           :phase 'asking-scope :nonce "poll-nonce" :cursor 'cursor))
         (magnus-reviews (list review))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal)))
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _arguments) (error "timer unavailable")))
                  ((symbol-function 'magnus-terminal-cancel-scope) #'ignore)
                  ((symbol-function 'magnus-review-controller--changed)
                   #'ignore))
          (should-not
           (magnus-review-controller--scope-delivery-accepted
            (magnus-review-id review) "poll-nonce" runtime))
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'failed))
          (should-not (magnus-review-controller-runtime-deadline runtime))
          (should (string-match-p
                   "timer unavailable"
                   (magnus-review-controller-runtime-error runtime))))
      (delete-directory root t))))

(ert-deftest magnus-review-failure-retry-and-interrupt-stay-ephemeral ()
  (let* ((root (make-temp-file "magnus-review-retry-" t))
         (review (magnus-test-review--review root))
         (round (magnus-test-review--round
                 1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (runtime
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id review)
           :phase 'failed :round round :error "failed"))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         started cancelled)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-controller--start-round)
                   (lambda (candidate candidate-runtime candidate-round)
                     (setq started
                           (list candidate candidate-runtime candidate-round))
                     (setf (magnus-review-controller-runtime-job-key
                            candidate-runtime)
                           '(fixture-job)))))
          (magnus-review-retry review)
          (should (equal started (list review runtime round)))
          (should (eq (magnus-review-controller-runtime-phase runtime) 'queued))
          (should-not (magnus-review-rounds review))
          (cl-letf (((symbol-function 'magnus-background-cancel)
                     (lambda (key) (setq cancelled key) 1)))
            (magnus-review-interrupt review))
          (should (equal cancelled '(fixture-job)))
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'interrupted))
          (should-not (magnus-review-controller-runtime-job runtime))
          (should-not (magnus-review-rounds review)))
      (delete-directory root t))))

(ert-deftest magnus-review-fresh-session-retry-keeps-lineage-and-identity ()
  (let* ((root (make-temp-file "magnus-review-fresh-root-" t))
         (storage (make-temp-file "magnus-review-fresh-store-" t))
         (magnus-review-directory-root storage)
         (round-one (magnus-test-review--round
                     1 magnus-test-review--oid-a
                     magnus-test-review--oid-b t))
         (round-two (magnus-test-review--round
                     2 magnus-test-review--oid-b
                     magnus-test-review--oid-c))
         (review (magnus-test-review--review root (list round-one)))
         (result-one
          (magnus-review-controller-normalize-result
           review round-one
           (magnus-test-review--raw-result
            magnus-test-review--oid-a magnus-test-review--oid-b
            :findings
            (list (magnus-test-review--finding "Preserve this finding")))))
         (finding-id
          (alist-get 'id (aref (alist-get 'findings result-one) 0)))
         (runtime
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id review)
           :phase 'failed :round round-two :error "resume failed"))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review))
         submission)
    (setf (magnus-review-session-id review) "old-reviewer-session")
    (magnus-test-review--store-result review round-one result-one)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf
            (((symbol-function 'magnus-review-ensure-checkout)
              (lambda (&rest _arguments) root))
             ((symbol-function 'magnus-background-submit)
              (lambda (key provider request &optional callbacks)
                (setq submission (list key provider request callbacks))
                (magnus-background--make-job :key key :state 'queued))))
          (magnus-review-restart-session review)
          (let ((request (nth 2 submission)))
            (should-not (plist-get request :session-id))
            (should (equal (plist-get request :name) "keen-owl"))
            (should (string-match-p (regexp-quote finding-id)
                                    (plist-get request :prompt))))
          (should (magnus-review-controller-runtime-fresh-session-p runtime))
          (should (eq (magnus-review-controller-runtime-phase runtime)
                      'queued))
          (should (equal (magnus-review-session-id review)
                         "old-reviewer-session")))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-completion-publishes-only-successful-structured-result ()
  (let* ((root (make-temp-file "magnus-review-complete-" t))
         (storage (make-temp-file "magnus-review-complete-store-" t))
         (magnus-review-directory-root storage)
         (review (magnus-test-review--review root))
         (round (magnus-test-review--round
                 1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (runtime
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id review) :phase 'running
           :round round :job-key '(success-job)))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review)))
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (progn
          (magnus-test-review--prepare-publication-evidence review round)
          (cl-letf (((symbol-function 'magnus-review-controller--cleanup-round)
                     #'ignore)
                    ((symbol-function 'magnus-review-controller--handoff)
                     #'ignore)
                    ((symbol-function 'magnus-review-controller--notify)
                     #'ignore))
            (magnus-review-controller--complete-job
             (magnus-review-id review) '(success-job)
             (list
              :success-p t
              :session-id "reviewer-session-one"
              :structured-result
              (magnus-test-review--raw-result
               magnus-test-review--oid-a magnus-test-review--oid-b
               :verdict "approve" :findings nil))))
          (should (= (length (magnus-review-rounds review)) 1))
          (should (eq (magnus-review-latest-round review) round))
          (should (equal (magnus-review-session-id review)
                         "reviewer-session-one"))
          (should-not (gethash (magnus-review-id review)
                               magnus-review-controller--runtimes)))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-post-publication-errors-cannot-relabel-success ()
  (let* ((root (make-temp-file "magnus-review-post-publish-" t))
         (storage (make-temp-file "magnus-review-post-store-" t))
         (magnus-review-directory-root storage)
         (review (magnus-test-review--review root))
         (round (magnus-test-review--round
                 1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (runtime
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id review) :phase 'running
           :round round :job-key '(post-publish-job)))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review)))
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (progn
          (magnus-test-review--prepare-publication-evidence review round)
          (cl-letf (((symbol-function 'magnus-review-controller--cleanup-round)
                     #'ignore)
                    ((symbol-function 'magnus-review-controller--handoff)
                     #'ignore)
                    ((symbol-function 'magnus-review-controller--notify)
                     (lambda (&rest _arguments) (error "bell failed")))
                    ((symbol-function 'magnus-review-controller--changed)
                     (lambda () (error "refresh failed"))))
            (magnus-review-controller--complete-job
             (magnus-review-id review) '(post-publish-job)
             (list
              :success-p t :session-id "durable-reviewer-session"
              :structured-result
              (magnus-test-review--raw-result
               magnus-test-review--oid-a magnus-test-review--oid-b
               :verdict "approve"))))
          (should (= (length (magnus-review-rounds review)) 1))
          (should (equal (magnus-review-session-id review)
                         "durable-reviewer-session"))
          (should-not (gethash (magnus-review-id review)
                               magnus-review-controller--runtimes))
          (should (eq (magnus-review-execution review) 'complete)))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-publication-retry-replaces-inert-candidate-artifacts ()
  "A manifest failure leaves artifacts replaceable by the exact candidate."
  (let* ((root (make-temp-file "magnus-review-publish-retry-" t))
         (storage (make-temp-file "magnus-review-publish-retry-store-" t))
         (magnus-review-directory-root storage)
         (review (magnus-test-review--review root))
         (round (magnus-test-review--round
                 1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (raw-one
          (magnus-test-review--raw-result
           magnus-test-review--oid-a magnus-test-review--oid-b
           :verdict "approve" :summary "First candidate result"))
         (raw-two
          (magnus-test-review--raw-result
           magnus-test-review--oid-a magnus-test-review--oid-b
           :verdict "approve" :summary "Corrected candidate result"))
         (real-save (symbol-function 'magnus-review-save))
         (save-count 0))
    (unwind-protect
        (progn
          (magnus-test-review--prepare-publication-evidence review round)
          (cl-letf (((symbol-function 'magnus-review-save)
                     (lambda (candidate)
                       (cl-incf save-count)
                       (if (= save-count 1)
                           (error "simulated manifest save failure")
                         (funcall real-save candidate)))))
            (should-error
             (magnus-review-controller--publish-result
              review round raw-one "reviewer-session"))
            (should-not (magnus-review-rounds review))
            (should-not (magnus-review-session-id review))
            (magnus-review-controller--publish-result
             review round raw-two "reviewer-session"))
          (should (= (length (magnus-review-rounds review)) 1))
          (should (equal (magnus-review-session-id review)
                         "reviewer-session"))
          (let* ((envelope
                  (magnus-review-controller--read-json
                   (magnus-review-round-result-path review round)))
                 (result
                  (magnus-review-controller--result-body envelope)))
            (should (equal (alist-get 'summary result)
                           "Corrected candidate result")))
          (with-temp-buffer
            (insert-file-contents
             (magnus-review-round-report-path review round))
            (should (search-forward "Corrected candidate result" nil t))
            (should-not (search-forward "First candidate result" nil t))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-first-success-requires-resumable-session-id ()
  (dolist (session-id '(nil ""))
    (let* ((root (make-temp-file "magnus-review-no-session-" t))
           (review (magnus-test-review--review root))
           (round (magnus-test-review--round
                   1 magnus-test-review--oid-a magnus-test-review--oid-b))
           (runtime
            (magnus-review-controller--make-runtime
             :review-id (magnus-review-id review) :phase 'running
             :round round :job-key '(candidate-job)))
           (magnus-review-controller--runtimes
            (make-hash-table :test #'equal))
           (magnus-reviews (list review))
           publish-called)
      (puthash (magnus-review-id review) runtime
               magnus-review-controller--runtimes)
      (unwind-protect
          (cl-letf (((symbol-function 'magnus-review-controller--publish-result)
                     (lambda (&rest _arguments) (setq publish-called t))))
            (magnus-review-controller--complete-job
             (magnus-review-id review) '(candidate-job)
             (list
              :success-p t
              :session-id session-id
              :structured-result
              (magnus-test-review--raw-result
               magnus-test-review--oid-a magnus-test-review--oid-b
               :verdict "approve")))
            (should-not publish-called)
            (should-not (magnus-review-rounds review))
            (should-not (magnus-review-session-id review))
            (should (eq (magnus-review-controller-runtime-phase runtime)
                        'failed))
            (should (string-match-p
                     "resumable session ID"
                     (magnus-review-controller-runtime-error runtime))))
        (delete-directory root t)))))

(ert-deftest magnus-review-failed-or-missing-result-never-enters-lineage ()
  (dolist (result
           (list '(:success-p nil :timed-out-p t :error-message "timed out")
                 '(:success-p t :session-id "reviewer-session"
                   :structured-result nil)))
    (let* ((root (make-temp-file "magnus-review-no-publish-" t))
           (review (magnus-test-review--review root))
           (round (magnus-test-review--round
                   1 magnus-test-review--oid-a magnus-test-review--oid-b))
           (runtime
            (magnus-review-controller--make-runtime
             :review-id (magnus-review-id review) :phase 'running
             :round round :job-key '(candidate-job)))
           (magnus-review-controller--runtimes
            (make-hash-table :test #'equal))
           (magnus-reviews (list review)))
      (puthash (magnus-review-id review) runtime
               magnus-review-controller--runtimes)
      (unwind-protect
          (cl-letf (((symbol-function 'magnus-review-controller--patch)
                     (lambda (&rest _arguments) "")))
            (magnus-review-controller--complete-job
             (magnus-review-id review) '(candidate-job) result)
            (should-not (magnus-review-rounds review))
            (should-not (magnus-review-session-id review))
            (should (eq (magnus-review-controller-runtime-phase runtime)
                        'failed)))
        (delete-directory root t)))))

(ert-deftest magnus-review-three-rounds-resume-reviewer-and-carry-ledger ()
  (let* ((root (make-temp-file "magnus-review-three-rounds-" t))
         (storage (make-temp-file "magnus-review-three-store-" t))
         (magnus-review-directory-root storage)
         (review (magnus-test-review--review root))
         (round-one (magnus-test-review--round
                     1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (round-two (magnus-test-review--round
                     2 magnus-test-review--oid-b magnus-test-review--oid-c))
         (round-three (magnus-test-review--round
                       3 magnus-test-review--oid-c magnus-test-review--oid-d))
         (runtime-one (magnus-review-controller--make-runtime
                       :review-id (magnus-review-id review) :round round-one))
         (runtime-two (magnus-review-controller--make-runtime
                       :review-id (magnus-review-id review) :round round-two))
         (runtime-three (magnus-review-controller--make-runtime
                         :review-id (magnus-review-id review) :round round-three))
         submissions)
    (unwind-protect
        (cl-letf
            (((symbol-function 'magnus-review-ensure-checkout)
              (lambda (&rest _arguments) root))
             ((symbol-function 'magnus-background-submit)
              (lambda (key provider request &optional callbacks)
                (push (list key provider request callbacks) submissions)
                (magnus-background--make-job :key key :state 'queued))))
          ;; Round one has no session or prior ledger.
          (magnus-review-controller--start-round review runtime-one round-one)
          (let ((request (nth 2 (car submissions))))
            (should (eq (plist-get request :purpose) 'review))
            (should (equal (plist-get request :name) "keen-owl"))
            (should-not (plist-get request :session-id))
            (should (string-match-p "first review round"
                                    (plist-get request :prompt))))
          (let* ((raw-one
                  (magnus-test-review--raw-result
                   magnus-test-review--oid-a magnus-test-review--oid-b
                   :findings
                   (list
                    (magnus-test-review--finding "Close the race")
                    (magnus-test-review--finding "Clarify the error"))))
                 (result-one
                  (magnus-review-controller-normalize-result
                   review round-one raw-one))
                 (findings-one (alist-get 'findings result-one))
                 (race-id (alist-get 'id (aref findings-one 0)))
                 (error-id (alist-get 'id (aref findings-one 1))))
            (setf (magnus-review-round-completed-at round-one) (float-time)
                  (magnus-review-round-verdict round-one) 'comment
                  (magnus-review-round-read-state round-one) 'unread
                  (magnus-review-rounds review) (list round-one)
                  (magnus-review-session-id review) "reviewer-session-one")
            (magnus-test-review--store-result review round-one result-one)

            ;; Round two resumes the same provider session and receives both
            ;; findings from round one.
            (magnus-review-controller--start-round review runtime-two round-two)
            (let* ((submission (car submissions))
                   (request (nth 2 submission))
                   (prompt (plist-get request :prompt)))
              (should (eq (nth 1 submission) 'codex))
              (should (equal (plist-get request :name) "keen-owl"))
              (should (equal (plist-get request :session-id)
                             "reviewer-session-one"))
              (should (string-match-p (regexp-quote race-id) prompt))
              (should (string-match-p (regexp-quote error-id) prompt)))
            (let* ((raw-two
                    (magnus-test-review--raw-result
                     magnus-test-review--oid-b magnus-test-review--oid-c
                     :findings
                     (list (magnus-test-review--finding
                            "Close the race" :prior-id race-id))
                     :prior-findings
                     (list
                      (magnus-test-review--disposition race-id "still_present")
                      (magnus-test-review--disposition error-id "resolved"))))
                   (result-two
                    (magnus-review-controller-normalize-result
                     review round-two raw-two result-one)))
              (should
               (equal (alist-get 'id (aref (alist-get 'findings result-two) 0))
                      race-id))
              (setf (magnus-review-round-completed-at round-two) (float-time)
                    (magnus-review-round-verdict round-two) 'comment
                    (magnus-review-round-read-state round-two) 'unread
                    (magnus-review-rounds review) (list round-one round-two)
                    (magnus-review-session-id review) "reviewer-session-two")
              (magnus-test-review--store-result review round-two result-two)

              ;; Round three resumes the second successful session and gets
              ;; the current finding plus both prior dispositions.
              (magnus-review-controller--start-round
               review runtime-three round-three)
              (let* ((submission (car submissions))
                     (request (nth 2 submission))
                     (prompt (plist-get request :prompt)))
                (should (eq (nth 1 submission) 'codex))
                (should (equal (plist-get request :name) "keen-owl"))
                (should (equal (plist-get request :session-id)
                               "reviewer-session-two"))
                (should (string-match-p (regexp-quote race-id) prompt))
                (should (string-match-p
                         "\\\"disposition\\\":\\\"still_present\\\""
                         prompt))
                (should (string-match-p
                         "\\\"disposition\\\":\\\"resolved\\\""
                         prompt))))))
      (delete-directory root t)
      (delete-directory storage t))))

(ert-deftest magnus-review-stale-completion-cannot-publish ()
  (let* ((root (make-temp-file "magnus-review-stale-" t))
         (review (magnus-test-review--review root))
         (round (magnus-test-review--round
                 1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (runtime
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id review) :phase 'running
           :round round :job-key '(replacement-job)))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-reviews (list review))
         published)
    (puthash (magnus-review-id review) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-controller--publish-result)
                   (lambda (&rest _arguments) (setq published t))))
          (magnus-review-controller--complete-job
           (magnus-review-id review) '(stale-job)
           '(:success-p t :structured-result ((schema_version . 1))))
          (should-not published)
          (should (eq (gethash (magnus-review-id review)
                               magnus-review-controller--runtimes)
                      runtime))
          (should-not (magnus-review-rounds review)))
      (delete-directory root t))))

(ert-deftest magnus-review-archive-discards-empty-draft-but-keeps-lineage ()
  (let* ((root (make-temp-file "magnus-review-archive-" t))
         (draft (magnus-test-review--review root))
         (completed-round
          (magnus-test-review--round
           1 magnus-test-review--oid-a magnus-test-review--oid-b t))
         (lineage
          (magnus-test-review--review
           root (list completed-round) "completed-review"))
         (runtime
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id draft)
           :phase 'failed :error "scope failed"))
         (magnus-reviews (list draft lineage))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         saved)
    (puthash (magnus-review-id draft) runtime
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-review-save)
                   (lambda (review) (push review saved) review)))
          (magnus-review-controller-archive draft)
          (should-not (memq draft magnus-reviews))
          (should-not (gethash (magnus-review-id draft)
                               magnus-review-controller--runtimes))
          (magnus-review-controller-archive lineage)
          (should (memq lineage magnus-reviews))
          (should (eq (magnus-review-lifecycle lineage) 'archived))
          (should (memq lineage saved)))
      (delete-directory root t))))

(ert-deftest magnus-review-controller-shutdown-drops-all-runtime-ownership ()
  (let* ((root (make-temp-file "magnus-review-shutdown-" t))
         (review (magnus-test-review--review root))
         (round (magnus-test-review--round
                 1 magnus-test-review--oid-a magnus-test-review--oid-b))
         (scope
          (magnus-review-controller--make-runtime
           :review-id "scope" :phase 'asking-scope :timer 'scope-timer))
         (job
          (magnus-review-controller--make-runtime
           :review-id (magnus-review-id review) :phase 'running
           :round round :job-key '(job-key)))
         (magnus-review-controller--runtimes (make-hash-table :test #'equal))
         (magnus-review-runtime-state-function
          #'magnus-review-controller--runtime-state)
         (magnus-review-controller--shutting-down nil)
         (magnus-reviews (list review))
         cancelled-timer cancelled-job cleaned terminal-scopes)
    (puthash "scope" scope magnus-review-controller--runtimes)
    (puthash (magnus-review-id review) job
             magnus-review-controller--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'timerp)
                   (lambda (value) (eq value 'scope-timer)))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (setq cancelled-timer timer)))
                  ((symbol-function 'magnus-background-cancel)
                   (lambda (key) (setq cancelled-job key) 1))
                  ((symbol-function 'magnus-review-controller--discard-candidate)
                   (lambda (candidate candidate-round)
                     (setq cleaned (list candidate candidate-round))))
                  ((symbol-function 'magnus-terminal-cancel-scope)
                   (lambda (scope-name) (push scope-name terminal-scopes))))
          (magnus-review-controller-shutdown)
          (should (= (hash-table-count
                      magnus-review-controller--runtimes) 0))
          (should (eq cancelled-timer 'scope-timer))
          (should (equal cancelled-job '(job-key)))
          (should (equal cleaned (list review round)))
          (should (memq scope terminal-scopes))
          (should (memq job terminal-scopes))
          (should (memq 'magnus-review-controller terminal-scopes))
          (should-not magnus-review-runtime-state-function))
      (delete-directory root t))))

(provide 'magnus-review-tests)
;;; magnus-review-tests.el ends here
