;;; magnus-transient-tests.el --- Magnus command-menu tests -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)
(require 'magnus-transient)

(defconst magnus-test-transient--base-oid
  "1111111111111111111111111111111111111111")

(defconst magnus-test-transient--head-oid
  "2222222222222222222222222222222222222222")

(defun magnus-test-transient--suffix-runs-command-p (prefix key command)
  "Return non-nil when PREFIX maps KEY to COMMAND.
Use Transient's public lookup contract instead of depending on the private
layout representation, which differs between supported Transient releases."
  (equal (transient-get-suffix prefix key)
         (transient-get-suffix prefix command)))

(defun magnus-test-transient--suffix-object (prefix command)
  "Return PREFIX's initialized suffix object for COMMAND."
  (cl-find-if (lambda (suffix)
                (eq (oref suffix command) command))
              (transient-suffixes prefix)))

(defun magnus-test-transient--round (&optional number)
  "Return a completed review round numbered NUMBER."
  (magnus-review-round--create
   :number (or number 1)
   :base-oid magnus-test-transient--base-oid
   :head-oid magnus-test-transient--head-oid
   :created-at 1 :completed-at 2
   :verdict 'comment :read-state 'read))

(defun magnus-test-transient--review (&optional id rounds)
  "Return a completed review lineage with ID and ROUNDS."
  (magnus-review--create
   :id (or id "review")
   :project-root "/tmp/project"
   :project-hash (make-string 64 ?a)
   :author-instance-id "author"
   :author-name "quick-wren"
   :reviewer-name "keen-owl"
   :reviewer-provider 'codex
   :effort 'high
   :task "Implement the feature"
   :lifecycle 'open
   :created-at 1 :updated-at 2
   :rounds (or rounds (list (magnus-test-transient--round)))))

(ert-deftest magnus-transient-creation-directory-prefers-status-context ()
  (let ((point-instance
         (magnus-instance--create :id "point" :name "point"
                                  :directory "/point"))
        (first-instance
         (magnus-instance--create :id "first" :name "first"
                                  :directory "/first"))
        (default-directory "/default/"))
    (cl-letf (((symbol-function 'magnus-status--get-instance-at-point)
               (lambda () point-instance))
              ((symbol-function 'magnus-instances-list)
               (lambda () (list first-instance))))
      (should (equal (magnus-transient--creation-directory) "/point")))
    (cl-letf (((symbol-function 'magnus-status--get-instance-at-point)
               (lambda () nil))
              ((symbol-function 'magnus-instances-list)
               (lambda () (list first-instance))))
      (should (equal (magnus-transient--creation-directory) "/first")))
    (cl-letf (((symbol-function 'magnus-status--get-instance-at-point)
               (lambda () nil))
              ((symbol-function 'magnus-instances-list) (lambda () nil)))
      (should (equal (magnus-transient--creation-directory) "/default/")))))

(ert-deftest magnus-transient-create-codex-forwards-task-and-provider ()
  (let (arguments creation-task refreshed)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _arguments) "Inspect this"))
              ((symbol-function 'magnus-transient--creation-directory)
               (lambda () "/project"))
              ((symbol-function 'magnus-process-create)
               (lambda (&rest values)
                 (setq arguments values
                       creation-task magnus--creation-task)))
              ((symbol-function 'magnus-status-refresh)
               (lambda () (setq refreshed t))))
      (magnus-transient-create-codex))
    (should (equal arguments
                   '("/project" nil codex "Inspect this")))
    (should (equal creation-task "Inspect this"))
    (should refreshed)))

(ert-deftest magnus-transient-create-codex-allows-an-empty-first-turn ()
  (let (arguments)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _arguments) ""))
              ((symbol-function 'magnus-transient--creation-directory)
               (lambda () "/project"))
              ((symbol-function 'magnus-process-create)
               (lambda (&rest values) (setq arguments values)))
              ((symbol-function 'magnus-status-refresh) #'ignore))
      (magnus-transient-create-codex))
    (should (equal arguments '("/project" nil codex nil)))))

(ert-deftest magnus-transient-review-menus-expose-the-complete-workflow ()
  (should
   (magnus-test-transient--suffix-runs-command-p
    'magnus-dispatch "v" 'magnus-review-request-dispatch))
  (should
   (magnus-test-transient--suffix-runs-command-p
    'magnus-review-request-menu "RET" 'magnus-transient-request-review))
  (dolist
      (entry
       '(("RET" magnus-transient-review-open
          "Open completed report" magnus-transient--review-open-inapt-p)
         ("r" magnus-transient-review-rereview
          nil magnus-transient--review-rereview-inapt-p)
         ("t" magnus-transient-review-retry
          "Retry failed work" magnus-transient--review-retry-inapt-p)
         ("f" magnus-transient-review-fresh-session
          "Retry with fresh reviewer session"
          magnus-transient--review-fresh-session-inapt-p)
         ("i" magnus-transient-review-interrupt
          "Interrupt active work" magnus-transient--review-interrupt-inapt-p)
         ("k" magnus-transient-review-archive
          "Archive" magnus-transient--review-archive-inapt-p)))
    (should
     (magnus-test-transient--suffix-runs-command-p
      'magnus-review-actions-menu (car entry) (cadr entry)))
    (let ((suffix
           (magnus-test-transient--suffix-object
            'magnus-review-actions-menu (cadr entry))))
      (should suffix)
      (when (nth 2 entry)
        (should (equal (oref suffix description) (nth 2 entry))))
      (should (eq (oref suffix inapt-if) (nth 3 entry)))))
  (should (equal (magnus-transient--review-rereview-description)
                 "Ask author for the next committed round")))

(ert-deftest magnus-transient-review-action-availability-follows-state ()
  (let* ((completed (magnus-test-transient--review "completed"))
         (draft (magnus-test-transient--review "draft" nil))
         (selected completed)
         candidate
         state
         (magnus-transient--review-action-context
          (list :review-id "completed")))
    ;; The helper defaults nil ROUNDS to one round; make the draft explicit.
    (setf (magnus-review-rounds draft) nil)
    (cl-letf (((symbol-function 'magnus-review-get)
               (lambda (_id) selected))
              ((symbol-function 'magnus-review-execution)
               (lambda (_review) state))
              ((symbol-function
                'magnus-review-controller-candidate-round)
               (lambda (_review) candidate)))
      ;; EXPECTED is (open next-round retry fresh-session interrupt archive),
      ;; where non-nil means Transient should render that action inapt.
      (dolist (case '((complete completed nil (nil nil t t t nil))
                      (asking-scope draft nil (t t t t nil nil))
                      (queued completed candidate (nil t t t nil nil))
                      (running completed candidate (nil t t t nil nil))
                      (failed completed candidate (nil t nil nil t nil))
                      (interrupted completed candidate (nil t nil nil t nil))
                      (failed completed nil (nil t nil t t nil))))
        (setq state (nth 0 case)
              selected (if (eq (nth 1 case) 'draft) draft completed)
              candidate (and (nth 2 case)
                             (magnus-test-transient--round 2)))
        (setf (magnus-review-lifecycle selected) 'open)
        (let ((actual
               (list (not (null (magnus-transient--review-open-inapt-p)))
                     (not (null (magnus-transient--review-rereview-inapt-p)))
                     (not (null (magnus-transient--review-retry-inapt-p)))
                     (not (null
                           (magnus-transient--review-fresh-session-inapt-p)))
                     (not (null (magnus-transient--review-interrupt-inapt-p)))
                     (not (null (magnus-transient--review-archive-inapt-p))))))
          (should (equal actual (nth 3 case)))))
      (setq state 'complete
            selected completed)
      (setf (magnus-review-lifecycle completed) 'archived)
      (should-not (magnus-transient--review-open-inapt-p))
      (should (magnus-transient--review-rereview-inapt-p))
      (should (magnus-transient--review-archive-inapt-p)))))

(ert-deftest magnus-transient-review-request-describes-each-ephemeral-state ()
  (let* ((author
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"))
         (review (magnus-test-transient--review))
         (cases
          '((new "Independent review of quick-wren"
                 "Start independent review" nil t)
            (rereview
             "Re-review quick-wren with keen-owl (same reviewer session)"
             "Request the next review round" nil nil)
            (asking "Asking quick-wren which commits belong to its work"
                    "Waiting for the author to identify its committed range"
                    t nil)
            (queued "keen-owl's review of quick-wren — queued"
                    "Review is queued" t nil)
            (running "keen-owl is reviewing quick-wren now"
                     "Review is in progress" t nil)
            (retry "Retry keen-owl's review of quick-wren"
                   "Retry failed review" nil nil))))
    (unwind-protect
        (dolist (case cases)
          (let ((action (nth 0 case)))
            (setq magnus-transient--review-request-context
                  (list :author author
                        :review (unless (eq action 'new) review)
                        :action action
                        :execution (and (eq action 'retry) 'failed)))
            (should (equal (magnus-transient--review-request-heading)
                           (nth 1 case)))
            (should (equal
                     (magnus-transient--review-request-action-description)
                     (nth 2 case)))
            (should (eq (not (null (magnus-transient--review-request-busy-p)))
                        (nth 3 case)))
            (should (eq (magnus-transient--review-request-new-p)
                        (nth 4 case)))))
      (setq magnus-transient--review-request-context nil))))

(ert-deftest magnus-transient-review-request-menu-renders-the-primary-action ()
  (let* ((author
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"))
         (magnus-transient--review-request-context
          (list :author author :action 'new)))
    (unwind-protect
        (progn
          (transient-setup #'magnus-review-request-menu)
          (let ((rendered
                 (with-current-buffer " *transient*"
                   (buffer-substring-no-properties (point-min) (point-max)))))
            (should (string-match-p "Independent review of quick-wren"
                                    rendered))
            (should (string-match-p "RET Start independent review" rendered))
            (should (string-match-p "defaults: opposite provider" rendered))))
      (when transient--prefix
        (transient-quit-one)))))

(ert-deftest magnus-transient-direct-review-key-follows-point-context ()
  (let ((review (magnus-test-transient--review))
        (author (magnus-instance--create :id "author" :name "quick-wren"))
        selected opened)
    (cl-letf (((symbol-function 'magnus-status--get-review-at-point)
               (lambda () review))
              ((symbol-function 'magnus-review-actions)
               (lambda (&optional candidate _round)
                 (setq selected candidate))))
      (magnus-review-request-dispatch))
    (should (eq selected review))
    (let ((magnus-transient--review-request-context nil))
      (cl-letf (((symbol-function 'magnus-status--get-review-at-point)
                 (lambda () nil))
                ((symbol-function 'magnus-status--get-instance-at-point)
                 (lambda () author))
                ((symbol-function 'magnus-review-request-context)
                 (lambda (candidate)
                   (list :author candidate :action 'new :state-key '(new))))
                ((symbol-function 'transient-setup)
                 (lambda (prefix &rest _arguments) (setq opened prefix))))
        (magnus-review-request-dispatch))
      (should (eq opened #'magnus-review-request-menu))
      (should (eq (plist-get magnus-transient--review-request-context :author)
                  author)))))

(ert-deftest magnus-transient-review-request-rejects-stale-runtime-context ()
  (let* ((author
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"))
         (cached (list :author author :root "/tmp/project" :task "Task"
                       :action 'queued :state-key '(queued old)))
         (fresh (list :author author :root "/tmp/project" :task "Task"
                      :action 'running :state-key '(running new))))
    (let ((magnus-transient--review-request-context cached))
      (cl-letf (((symbol-function 'magnus-review-request-context)
                 (lambda (_author) fresh)))
        (should-error
         (magnus-transient--validated-review-request-context cached)
         :type 'user-error)
        (should (eq magnus-transient--review-request-context fresh))))))

(ert-deftest magnus-transient-review-actions-retain-lineage-and-round-context ()
  (let* ((old-round (magnus-test-transient--round 1))
         (old-review
          (magnus-test-transient--review "review-a" (list old-round)))
         (fresh-round (magnus-test-transient--round 1))
         (fresh-review
          (magnus-test-transient--review "review-a" (list fresh-round)))
         (other-review (magnus-test-transient--review "review-b"))
         (magnus-transient--review-action-context
          (list :review-id "review-a" :round-number 1
                :state-key '(rereview stable)
                :reviewer-name "keen-owl" :author-name "quick-wren"))
         (transient-current-command 'magnus-review-actions-menu)
         opened rereviewed retried restarted interrupted archived
         (refreshes 0))
    (ignore old-review)
    (cl-letf (((symbol-function 'magnus-review-get)
               (lambda (id) (and (string= id "review-a") fresh-review)))
              ((symbol-function 'magnus-status--get-review-at-point)
               (lambda () other-review))
              ((symbol-function 'magnus-review-controller--operation)
               (lambda (_review)
                 '(:action rereview :state-key (rereview stable))))
              ((symbol-function 'magnus-review-ui-open)
               (lambda (review round) (setq opened (list review round))))
              ((symbol-function 'magnus-review-rereview)
               (lambda (review) (setq rereviewed review)))
              ((symbol-function 'magnus-review-retry)
               (lambda (review) (setq retried review)))
              ((symbol-function 'magnus-review-restart-session)
               (lambda (review) (setq restarted review)))
              ((symbol-function 'magnus-review-interrupt)
               (lambda (review) (setq interrupted review)))
              ((symbol-function 'magnus-review-controller-archive)
               (lambda (review) (setq archived review)))
              ((symbol-function 'magnus-status-refresh)
               (lambda () (cl-incf refreshes)))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _arguments) t)))
      (magnus-transient-review-open)
      (magnus-transient-review-rereview)
      (magnus-transient-review-retry)
      (magnus-transient-review-fresh-session)
      (magnus-transient-review-interrupt)
      (magnus-transient-review-archive))
    (should (equal opened (list fresh-review fresh-round)))
    (should (eq rereviewed fresh-review))
    (should (eq retried fresh-review))
    (should (eq restarted fresh-review))
    (should (eq interrupted fresh-review))
    (should (eq archived fresh-review))
    (should (= refreshes 5))
    (should-not (eq rereviewed other-review))))

(ert-deftest magnus-transient-review-action-rejects-an-ephemeral-aba-change ()
  (let* ((review (magnus-test-transient--review "review-a"))
         (magnus-transient--review-action-context
          (list :review-id "review-a" :state-key '(queued first)))
         (transient-current-command 'magnus-review-actions-menu)
         called)
    (cl-letf (((symbol-function 'magnus-review-get)
               (lambda (_id) review))
              ((symbol-function 'magnus-review-controller--operation)
               (lambda (_review)
                 '(:action queued :state-key (queued replacement))))
              ((symbol-function 'magnus-review-retry)
               (lambda (_review) (setq called t))))
      (should-error (magnus-transient-review-retry) :type 'user-error))
    (should-not called)))

(provide 'magnus-transient-tests)
;;; magnus-transient-tests.el ends here
