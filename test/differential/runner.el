;;; runner.el --- Old/new Magnus semantic gate -*- lexical-binding: t -*-

;; Loaded from the candidate checkout while Magnus itself comes from
;; MAGNUS_DIFF_CHECKOUT.  Keep projections behavioral: never compare marker
;; JSON, timestamps, opaque tokens, paths, or record layout.

(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'subr-x)
(setq load-prefer-newer t)

(defun magnus-differential--env (name)
  "Return required environment variable NAME."
  (or (getenv name) (error "Missing %s" name)))

(defun magnus-differential--write (value)
  "Write canonical VALUE to the configured output."
  (with-temp-file (magnus-differential--env "MAGNUS_DIFF_OUTPUT")
    (let ((print-length nil) (print-level nil))
      (pp value (current-buffer)))))

(defun magnus-differential--git (directory &rest arguments)
  "Run Git ARGUMENTS in DIRECTORY and return trimmed stdout."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (zerop (apply #'process-file "git" nil t nil arguments))
        (error "git %s failed: %s"
               (string-join arguments " ") (string-trim (buffer-string))))
      (string-trim-right (buffer-string)))))

(defun magnus-differential--commit (directory date message contents)
  "Commit CONTENTS in DIRECTORY at fixed DATE with MESSAGE."
  (with-temp-file (expand-file-name "fixture.txt" directory) (insert contents))
  (magnus-differential--git directory "add" "fixture.txt")
  (let ((process-environment
         (append (list (concat "GIT_AUTHOR_DATE=" date)
                       (concat "GIT_COMMITTER_DATE=" date))
                 process-environment)))
    (magnus-differential--git
     directory "-c" "user.name=Magnus Differential"
     "-c" "user.email=magnus@example.invalid" "-c" "commit.gpgsign=false"
     "commit" "-q" "-m" message))
  (magnus-differential--git directory "rev-parse" "HEAD"))

(defun magnus-differential--repository (state)
  "Create a deterministic two-commit repository beneath STATE."
  (let ((directory (expand-file-name "project" state)))
    (make-directory directory t)
    (magnus-differential--git directory "init" "-q")
    (let ((base (magnus-differential--commit
                 directory "2001-01-01T00:00:00Z" "base" "base\n")))
      (list directory base
            (magnus-differential--commit
             directory "2001-01-02T00:00:00Z" "head"
             "base\nreviewed change\n")))))

(defun magnus-differential--isolated-review (state function)
  "Call FUNCTION with review persistence isolated beneath STATE."
  (require 'magnus-review)
  (let ((magnus-review-directory-root (expand-file-name "reviews" state))
        (magnus-reviews nil)
        (magnus-reviews-changed-hook nil)
        (magnus-review-ready-hook nil)
        (magnus-review-checkpoint-mismatch-hook nil)
        (magnus-review--durable-snapshots
         (make-hash-table :test #'eq :weakness 'key)))
    (funcall function)))

(defun magnus-differential--ensure-checkout (review head)
  "Reach REVIEW checkout at HEAD across the old/new API boundary."
  (if (fboundp 'magnus-review-ensure-checkout)
      (magnus-review-ensure-checkout review head)
    (magnus-review-worktree-create review head)))

(defun magnus-differential--checkout (review base head)
  "Project REVIEW checkout semantics without reading its marker."
  (let* ((checkout (magnus-review-checkout-path review))
         (actual (magnus-review-resolve-oid checkout "HEAD"))
         (worktrees (magnus-differential--git
                     (magnus-review-project-root review)
                     "worktree" "list" "--porcelain")))
    (list :head (cond ((string= actual base) 'base)
                      ((string= actual head) 'head)
                      (t 'other))
          :dirty (and (magnus-review-worktree-dirty-status checkout) t)
          :contents (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "fixture.txt" checkout))
                      (buffer-string))
          :mode (logand (file-modes checkout) #o777)
          :registrations
          (cl-count-if (lambda (line) (string-prefix-p "worktree " line))
                       (split-string worktrees "\n" t)))))

(defun magnus-differential--worktree (state)
  "Characterize fresh, repeat, update, and dirty checkout behavior."
  (magnus-differential--isolated-review
   state
   (lambda ()
     (pcase-let* ((`(,project ,base ,head)
                   (magnus-differential--repository state))
                  (counter 0))
       (cl-letf (((symbol-function 'magnus-review--random-token)
                  (lambda () (format "token-%016d" (cl-incf counter)))))
         (let* ((review (magnus-review-create
                         project "author" "quick-wolf" :id "worktree-review"
                         :task "Checkout semantics" :reviewer-name "wise-deer"
                         :reviewer-provider 'codex))
                (fresh-path (magnus-differential--ensure-checkout review base))
                (fresh (magnus-differential--checkout review base head))
                (repeat-path (magnus-differential--ensure-checkout review base))
                (repeat (magnus-differential--checkout review base head))
                (update-path (magnus-differential--ensure-checkout review head))
                (updated (magnus-differential--checkout review base head)))
           (with-temp-file (expand-file-name "fixture.txt" update-path)
             (insert "local reviewer note\n"))
           (list :fresh fresh :repeat repeat
                 :same-path (and (string= fresh-path repeat-path)
                                 (string= repeat-path update-path))
                 :updated updated
                 :dirty-refusal
                 (condition-case nil
                     (progn (magnus-differential--ensure-checkout review base)
                            'accepted)
                   (error 'refused))
                 :preserved
                 (magnus-differential--checkout review base head))))))))

(defun magnus-differential--schema (review)
  "Return REVIEW manifest schema version."
  (with-temp-buffer
    (insert-file-contents (magnus-review-manifest-path review))
    (alist-get 'schema_version
               (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun magnus-differential--review (review)
  "Project canonical review ledger semantics."
  (list
   :schema (magnus-differential--schema review)
   :lifecycle (magnus-review-lifecycle review)
   :execution (magnus-review-execution review)
   :delivery (magnus-review-delivery-state review)
   :read (magnus-review-read-state review)
   :checkpoints
   (mapcar (lambda (request)
             (mapcar (lambda (event)
                       (list (magnus-review-checkpoint-event-kind event)
                             (magnus-review-checkpoint-event-round-number event)))
                     (magnus-review-checkpoint-request-events request)))
           (magnus-review-checkpoint-requests review))
   :rounds
   (mapcar
    (lambda (round)
      (list :execution (magnus-review-round-execution round)
            :verdict (magnus-review-round-verdict round)
            :delivery (list (magnus-review-round-delivery-state round)
                            (magnus-review-round-delivery-attempts round))
            :read (magnus-review-round-read-state round)
            :attempts
            (mapcar (lambda (attempt)
                      (magnus-review-attempt-execution attempt))
                    (magnus-review-round-attempts round))))
    (magnus-review-rounds review))))

(defun magnus-differential--review-ledger (state)
  "Create, reload, and project a durable schema-2 review in STATE."
  (magnus-differential--isolated-review
   state
   (lambda ()
     (pcase-let* ((`(,project ,base ,head)
                   (magnus-differential--repository state))
                  (counter 0))
       (cl-letf (((symbol-function 'magnus-review--random-token)
                  (lambda () (format "token-%016d" (cl-incf counter)))))
         (let* ((review (magnus-review-create
                         project "author" "quick-wolf"
                         :id "differential-review" :task "Ledger semantics"
                         :reviewer-name "wise-deer" :reviewer-provider 'codex))
                (round (magnus-review-append-round
                        review base head
                        :checkpoint-token (magnus-review-checkpoint-token review)))
                (attempt (magnus-review-append-attempt review round)))
           (magnus-review-mark-attempt-running
            review round attempt (magnus-review-attempt-token attempt))
           (magnus-review-write-artifact
            review (magnus-review-round-result-path review round) "{}\n")
           (magnus-review-write-artifact
            review (magnus-review-round-report-path review round) "# Review\n")
           (magnus-review-complete-attempt
            review round attempt 'changes-requested
            (magnus-review-attempt-token attempt))
           (magnus-review-archive review)
           (setq magnus-reviews nil)
           (unless (= (magnus-review-load-all) 1) (error "Reload failed"))
           (magnus-differential--review
            (magnus-review-get "differential-review"))))))))

(defun magnus-differential--handoff (state)
  "Load, save, reload, and project copied baseline schema-2 state."
  (magnus-differential--isolated-review
   state
   (lambda ()
     (unless (= (magnus-review-load-all) 1) (error "Baseline load failed"))
     (let ((before (magnus-differential--review (car (magnus-review-list)))))
       (magnus-review-save (car (magnus-review-list)))
       (setq magnus-reviews nil)
       (unless (= (magnus-review-load-all) 1) (error "Candidate reload failed"))
       (let ((after (magnus-differential--review (car (magnus-review-list)))))
         (unless (equal before after) (error "Save changed review semantics"))
         after)))))

;;;###autoload
(defun magnus-differential-main ()
  "Run the configured scenario and write its semantic projection."
  (let* ((checkout (file-name-as-directory
                    (file-truename (magnus-differential--env
                                    "MAGNUS_DIFF_CHECKOUT"))))
         (state (file-name-as-directory
                 (expand-file-name (magnus-differential--env
                                    "MAGNUS_DIFF_STATE"))))
         (scenario (magnus-differential--env "MAGNUS_DIFF_SCENARIO")))
    (setq load-path (cons checkout (delete checkout load-path)))
    (make-directory state t)
    (let ((process-environment
           (append '("GIT_CONFIG_NOSYSTEM=1" "GIT_CONFIG_GLOBAL=/dev/null"
                     "LC_ALL=C") process-environment)))
      (magnus-differential--write
       (pcase scenario
         ("worktree-semantics" (magnus-differential--worktree state))
         ("review-ledger" (magnus-differential--review-ledger state))
         ("review-handoff" (magnus-differential--handoff state))
         (_ (error "Unknown scenario: %s" scenario)))))))

(provide 'magnus-differential-runner)
;;; runner.el ends here
