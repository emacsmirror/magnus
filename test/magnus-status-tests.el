;;; magnus-status-tests.el --- Status UX tests for Magnus -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
;; CI intentionally tests with `emacs -Q' and does not install vterm.  Status
;; rendering only needs the process function boundaries, not vterm itself.
(unless (featurep 'vterm)
  (provide 'vterm))
(require 'magnus-status)
(require 'magnus-transient)

(defun magnus-test-status--attempts-for-state (state)
  "Return canonical attempt history deriving review round STATE."
  (unless (eq state 'queued)
    (list
     (magnus-review-attempt--create
      :number 1 :token (format "status-fixture-%s" state)
      :started-at 1
      :finished-at (and (memq state '(complete failed interrupted)) 2)
      :execution state))))

(defun magnus-test-status--render-instance (instance reviews)
  "Render INSTANCE with REVIEWS and return its text plus slot position."
  (with-temp-buffer
    (cl-letf (((symbol-function 'magnus-review-list) (lambda () reviews))
              ((symbol-function 'magnus-process-running-p) (lambda (_instance) t))
              ((symbol-function 'magnus-coord-agent-busy-p) (lambda (_instance) nil))
              ((symbol-function 'magnus-coord--neglected-p) (lambda (_instance) nil))
              ((symbol-function 'magnus-health-indicator) (lambda (_instance) "+"))
              ((symbol-function 'magnus--agents-index-get)
               (lambda (_directory _name) nil)))
      (magnus-status--insert-instance instance)
      (list (buffer-string)
            (text-property-any (point-min) (point-max)
                               'magnus-review-animation-slot t)))))

(defun magnus-test-status--review-in-state (execution &rest arguments)
  "Return a review whose canonical history derives EXECUTION.
ARGUMENTS are passed to `magnus-review--create'."
  (apply
   #'magnus-review--create
   (append
    arguments
    (if (eq execution 'waiting-for-checkpoint)
        (list
         :checkpoint-requests
         (list
          (magnus-review-checkpoint-request--create
           :number 1 :token "status-fixture-checkpoint"
           :requested-at 1 :events nil)))
      (list
       :rounds
       (list
        (magnus-review-round--create
         :number 1
         :attempts (magnus-test-status--attempts-for-state execution))))))))

(ert-deftest magnus-status-review-keys-are-directly-discoverable ()
  (should (eq (lookup-key magnus-status-mode-map (kbd "v"))
              'magnus-review-request-dispatch))
  (should (eq (lookup-key magnus-status-mode-map (kbd "V"))
              'magnus-review-actions)))

(ert-deftest magnus-status-coordination-command-is-explicitly-discoverable ()
  (should-not (lookup-key magnus-status-mode-map (kbd "J")))
  (should (eq (lookup-key magnus-status-mode-map (kbd "C"))
              'magnus-status-coordination))
  (let ((coordination (transient-get-suffix 'magnus-dispatch "C")))
    (should-error (transient-get-suffix 'magnus-dispatch "J"))
    (should (eq (plist-get (nth 2 coordination) :command)
                'magnus-status-coordination))))

(ert-deftest magnus-status-manual-refresh-polls-markdown-coordination ()
  (let ((magnus-buffer-name " *magnus-manual-refresh-test*")
        calls)
    (cl-letf (((symbol-function 'magnus-coord-refresh-all)
               (lambda () (push 'refresh calls)))
              ((symbol-function 'magnus-coord-reconcile-all)
               (lambda () (push 'reconcile calls)))
              ((symbol-function 'called-interactively-p)
               (lambda (&rest _arguments) t)))
      (magnus-status-refresh))
    (should (equal (nreverse calls) '(reconcile refresh)))))

(ert-deftest magnus-status-coordination-section-uses-markdown-state ()
  (let ((directories '("/first/" "/second/" "/empty/")))
    (with-temp-buffer
      (cl-letf (((symbol-function 'magnus-status--get-project-directories)
                 (lambda () directories))
                ((symbol-function 'magnus-coord-has-state-p)
                 (lambda (directory)
                   (member directory '("/first/" "/second/"))))
                ((symbol-function 'magnus-coord-parse)
                 (lambda (_directory) '(:active nil :log nil))))
        (magnus-status--insert-coordination))
      (should (string-match-p "Coordination" (buffer-string)))
      (should (string-match-p "/first/" (buffer-string)))
      (should (string-match-p "/second/" (buffer-string)))
      (should-not (string-match-p "/empty/" (buffer-string))))))

(ert-deftest magnus-status-projects-deduplicate-physical-directory-aliases ()
  (let* ((directory (make-temp-file "magnus-status-project-" t))
         (link (concat directory "-link"))
         (real-instance
          (magnus-instance--create
           :id "real" :name "real" :directory directory :status 'running))
         (link-instance
          (magnus-instance--create
           :id "link" :name "link" :directory link :status 'running))
         (magnus-instances (list real-instance link-instance)))
    (unwind-protect
        (progn
          (make-symbolic-link directory link)
          (should
           (equal (magnus-status--get-project-directories)
                  (list (magnus-coord--normalized-directory directory)))))
      (when (file-symlink-p link) (delete-file link))
      (delete-directory directory t))))

(ert-deftest magnus-status-recent-log-shows-the-chronological-tail ()
  (let ((log
         (mapcar (lambda (message)
                   (list :time "12:00" :agent "agent" :message message))
                 '("one" "two" "three" "four"))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'magnus-coord-parse)
                 (lambda (_directory) (list :active nil :log log))))
        (magnus-status--insert-coordination-for-dir default-directory))
      (let ((rendered (buffer-string)))
        (should-not (string-match-p "one" rendered))
        (should (< (string-match "two" rendered)
                   (string-match "three" rendered)))
        (should (< (string-match "three" rendered)
                   (string-match "four" rendered)))))))

(ert-deftest magnus-status-coordination-row-retains-its-project-context ()
  (let ((first
         (magnus-instance--create
          :id "first" :name "first" :directory "/first/" :status 'running)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'magnus-coord-parse)
                 (lambda (_directory) '(:active nil :log nil))))
        (magnus-status--insert-coordination-for-dir "/selected/"))
      (goto-char (point-min))
      (search-forward "selected")
      (let ((magnus-instances (list first)))
        (should (equal (magnus-status--coordination-directory)
                       "/selected/"))))))

(ert-deftest magnus-status-opens-shared-coordination-file ()
  (let ((instance (magnus-instance--create
                   :id "author" :name "quick-wren"
                   :directory "/project/"))
        opened)
    (cl-letf (((symbol-function 'magnus-status--get-instance-at-point)
               (lambda () instance))
              ((symbol-function 'magnus-coord-open)
               (lambda (directory) (setq opened directory))))
      (magnus-status-coordination))
    (should (equal opened "/project/"))))

(ert-deftest magnus-status-context-hints-use-buffer-local-eldoc ()
  (let ((magnus-status-show-context-hints t))
    (with-temp-buffer
      (magnus-status-mode)
      (should eldoc-mode)
      (should (eq eldoc-message-function
                  #'magnus-status--context-hint-message))
      (should (memq #'magnus-status--context-hint
                    eldoc-documentation-functions))
      (should (intern-soft "magnus-status-next" eldoc-message-commands))
      (should (intern-soft "magnus-status-previous"
                           eldoc-message-commands))))
  (let ((magnus-status-show-context-hints nil))
    (with-temp-buffer
      (magnus-status-mode)
      (should-not (memq #'magnus-status--context-hint
                        eldoc-documentation-functions)))))

(ert-deftest magnus-status-context-hints-preserve-unrelated-messages ()
  (let ((echo "Review completed for quick-wren")
        (eldoc-last-message "stale ElDoc hint")
        (magnus-status--last-context-hint "old Magnus hint"))
    (cl-letf (((symbol-function 'current-message) (lambda () echo))
              ((symbol-function 'eldoc-minibuffer-message)
               (lambda (format-string &rest args)
                 (setq echo
                       (and format-string
                            (apply #'format-message format-string args))))))
      ;; A timer or process notification owns the echo area, so a delayed hint
      ;; neither replaces it nor leaves a stale ElDoc message to resurrect.
      (magnus-status--context-hint-message "%s" "new Magnus hint")
      (should (equal echo "Review completed for quick-wren"))
      (should-not magnus-status--last-context-hint)
      (should-not eldoc-last-message)
      ;; Silence is available, and the exact rendered message becomes Magnus's
      ;; ownership token for the next contextual update.
      (setq echo nil)
      (magnus-status--context-hint-message "%s" "new Magnus hint")
      (should (equal echo "new Magnus hint"))
      (should (equal magnus-status--last-context-hint "new Magnus hint"))
      (magnus-status--context-hint-message "%s" "next Magnus hint")
      (should (equal echo "next Magnus hint"))
      (magnus-status--context-hint-message nil)
      (should-not echo)
      (should-not magnus-status--last-context-hint))))

(ert-deftest magnus-status-context-hints-follow-review-lines-and-bindings ()
  (let* ((round
          (magnus-review-round--create
           :number 1 :verdict 'approve
           :attempts (magnus-test-status--attempts-for-state 'complete)
           :read-state 'unread))
         (review
          (magnus-review--create
           :id "review-hint" :reviewer-name "keen-owl"
           :author-name "quick-wren" :lifecycle 'open
           :created-at (float-time)
           :updated-at (float-time) :rounds (list round)))
         (waiting
          (magnus-test-status--review-in-state
           'waiting-for-checkpoint
           :id "review-waiting" :reviewer-name "swift-hare"
           :author-name "bright-crow" :lifecycle 'open
           :created-at (float-time)
           :updated-at (float-time))))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (magnus-status--insert-review review))
      (cl-letf (((symbol-function 'magnus-review-get)
                 (lambda (id)
                   (and (string= id "review-hint") review))))
        (goto-char (point-min))
        (search-forward "keen-owl")
        (let ((reviewer-hint
               (substring-no-properties
                (magnus-status--context-hint nil))))
          (search-forward "round 1")
          (let ((round-hint
                 (substring-no-properties
                  (magnus-status--context-hint nil))))
            (should (equal reviewer-hint round-hint))
            (should (equal round-hint
                           (concat "keen-owl · round 1 — RET open · "
                                   "v review actions · ? all actions")))))
        ;; Hints resolve command keys at display time, so user rebinding remains
        ;; truthful instead of baking Magnus's default keys into prose.
        (let ((map (copy-keymap magnus-status-mode-map)))
          (define-key map (kbd "v") nil)
          (define-key map (kbd "u") #'magnus-review-request-dispatch)
          (use-local-map map)
          (should
           (string-match-p
            "u review actions"
            (substring-no-properties
             (magnus-status--context-hint nil))))
          (use-local-map magnus-status-mode-map)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "waiting" 'magnus-review-id "review-waiting")))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'magnus-review-get)
                 (lambda (id)
                   (and (string= id "review-waiting") waiting))))
        (let ((hint (substring-no-properties
                     (magnus-status--context-hint nil))))
          (should (string-match-p "v review actions" hint))
          (should-not (string-match-p "RET open" hint)))))))

(ert-deftest magnus-status-context-hints-distinguish-agent-lifecycle ()
  (let ((active
         (magnus-instance--create
          :id "active" :name "quick-wren" :status 'running))
        (purged
         (magnus-instance--create
          :id "purged" :name "wise-deer" :status 'purged)))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "active" 'magnus-instance-id "active") "\n")
        (insert (propertize "purged" 'magnus-instance-id "purged") "\n")
        (insert "heading"))
      (cl-letf (((symbol-function 'magnus-instances-get)
                 (lambda (id)
                   (cond ((string= id "active") active)
                         ((string= id "purged") purged)))))
        (goto-char (point-min))
        (should
         (equal
          (substring-no-properties (magnus-status--context-hint nil))
          (concat "quick-wren — RET visit · m message · v request review · "
                  "t thinking trace · ? all actions")))
        (forward-line 1)
        (should
         (equal
          (substring-no-properties (magnus-status--context-hint nil))
          "wise-deer (archived) — R resurrect · r rename · ? all actions"))
        (forward-line 1)
        (should
         (equal
          (substring-no-properties (magnus-status--context-hint nil))
          "Magnus — n/p navigate · c create agent · ? all actions"))))))

(ert-deftest magnus-status-rename-requires-an-archived-agent ()
  (let ((instance
         (magnus-instance--create
          :id "live" :name "quick-wren" :directory default-directory
          :status 'running)))
    (should-error
     (magnus-status--rename-archived-instance instance "wise-deer")
     :type 'user-error)
    (should (equal (magnus-instance-name instance) "quick-wren"))))

(ert-deftest magnus-status-rename-migrates-memory-and-preserves-continuity ()
  (let* ((directory (make-temp-file "magnus-status-rename-" t))
         (instance
          (magnus-instance--create
           :id "archived" :name "quick-wren" :directory directory
           :status 'purged))
         (magnus-instances (list instance))
         (magnus-instances-changed-hook nil)
         (old-memory (magnus-onboarding-memory-path instance)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory old-memory) t)
          (with-temp-file old-memory (insert "I remember."))
          (magnus-status--rename-archived-instance instance "wise-deer")
          (should (equal (magnus-instance-name instance) "wise-deer"))
          (should-not (file-exists-p old-memory))
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents (magnus-onboarding-memory-path instance))
              (buffer-string))
            "I remember."))
          (should (string-match-p
                   "You have been here before"
                   (magnus-onboarding-prompt instance))))
      (delete-directory directory t))))

(ert-deftest magnus-status-rename-rejects-project-name-and-home-collisions ()
  (let* ((directory (make-temp-file "magnus-status-collision-" t))
         (instance
          (magnus-instance--create
           :id "old" :name "quick-wren" :directory directory :status 'purged))
         (other
          (magnus-instance--create
           :id "other" :name "wise-deer" :directory directory :status 'purged))
         (magnus-instances (list instance other)))
    (unwind-protect
        (progn
          (should-error
           (magnus-status--rename-archived-instance instance "wise-deer")
           :type 'user-error)
          (setq magnus-instances (list instance))
          (make-directory
           (file-name-directory
            (expand-file-name
             (magnus-onboarding-memory-relative-path "wise-deer") directory))
           t)
          (should-error
           (magnus-status--rename-archived-instance instance "wise-deer")
           :type 'user-error)
          (should (equal (magnus-instance-name instance) "quick-wren")))
      (delete-directory directory t))))

(ert-deftest magnus-status-rename-rolls-home-back-after-observer-failure ()
  (let* ((directory (make-temp-file "magnus-status-rollback-" t))
         (instance
          (magnus-instance--create
           :id "rollback" :name "quick-wren" :directory directory
           :status 'purged))
         (magnus-instances (list instance))
         (old-memory (magnus-onboarding-memory-path instance))
         (magnus-instances-changed-hook
          (list (lambda () (error "simulated observer failure")))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory old-memory) t)
          (with-temp-file old-memory (insert "still here"))
          (should-error
           (magnus-status--rename-archived-instance instance "wise-deer"))
          (should (equal (magnus-instance-name instance) "quick-wren"))
          (should (file-exists-p old-memory))
          (should-not
           (file-exists-p
            (expand-file-name
             (magnus-onboarding-memory-relative-path "wise-deer")
             directory))))
      (delete-directory directory t))))

(ert-deftest magnus-status-rename-rolls-back-after-persistence-failure ()
  (let* ((directory (make-temp-file "magnus-status-persist-rollback-" t))
         (instance
          (magnus-instance--create
           :id "persist-rollback" :name "quick-wren" :directory directory
           :status 'purged))
         (magnus-instances (list instance))
         (magnus-instances-changed-hook nil)
         (magnus-persistence--autosave-active t)
         (old-memory (magnus-onboarding-memory-path instance)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory old-memory) t)
          (with-temp-file old-memory (insert "durable old home"))
          (cl-letf (((symbol-function 'magnus-persistence-save)
                     (lambda () (error "simulated durable write failure"))))
            (should-error
             (magnus-status--rename-archived-instance instance "wise-deer")))
          (should (equal (magnus-instance-name instance) "quick-wren"))
          (should (file-exists-p old-memory)))
      (delete-directory directory t))))

(ert-deftest magnus-instances-create-rejects-a-workspace-name-collision ()
  (let* ((directory (make-temp-file "magnus-instance-name-" t))
         (link (concat directory "-link"))
         (existing
          (magnus-instance--create
           :id "existing" :name "quick-wren" :directory directory
           :status 'purged))
         (magnus-instances (list existing)))
    (unwind-protect
        (progn
          (make-symbolic-link directory link)
          (should-error
           (magnus-instances-create link "quick-wren" 'codex)
           :type 'user-error))
      (when (file-symlink-p link) (delete-file link))
      (delete-directory directory t))))

(ert-deftest magnus-instances-create-honors-adjacent-identity-reservations ()
  (let ((magnus-instances nil)
        (magnus-instances-name-reservation-functions
         (list (lambda (_project) '("api-reviewer")))))
    (should-error
     (magnus-instances-create default-directory "api-reviewer" 'codex)
     :type 'user-error)
    (should
     (magnus-instance-p
      (magnus-instances-create default-directory "implementation-agent"
                               'codex)))))

(ert-deftest magnus-review-transient-names-the-selected-operation ()
  (let* ((author
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"))
         (review
          (magnus-test-status--review-in-state
           'running
           :id "active" :author-instance-id "author"
           :author-name "quick-wren" :reviewer-name "keen-owl"
           :lifecycle 'open)))
    (unwind-protect
        (progn
          (setq magnus-transient--review-request-context
                (list :author author :action 'new))
          (should (equal (magnus-transient--review-request-heading)
                         "Independent review of quick-wren"))
          (should (equal (magnus-transient--review-request-action-description)
                         "Start independent review"))
          (should (magnus-transient--review-request-new-p))
          (should-not (magnus-transient--review-request-busy-p))
          (setq magnus-transient--review-request-context
                (list :author author :review review :action 'running))
          (should (equal (magnus-transient--review-request-heading)
                         "keen-owl is reviewing quick-wren now"))
          (should (equal (magnus-transient--review-request-action-description)
                         "Review is in progress"))
          (should-not (magnus-transient--review-request-new-p))
          (should (magnus-transient--review-request-busy-p))
          (setf (magnus-review-checkpoint-requests review)
                (list
                 (magnus-review-checkpoint-request--create
                  :number 1 :token "waiting-token" :requested-at 1
                  :events nil)))
          (setq magnus-transient--review-request-context
                (list :author author :review review :action 'waiting))
          (should
           (equal (magnus-transient--review-request-action-description)
                  "Resend current checkpoint request"))
          (should-not (magnus-transient--review-request-busy-p))
          (setq magnus-transient--review-action-context
                (magnus-transient--make-review-action-context review))
          (should
           (equal (magnus-transient--review-rereview-description)
                  "Resend checkpoint request")))
      (setq magnus-transient--review-request-context nil
            magnus-transient--review-action-context nil))))

(ert-deftest magnus-review-transient-wires-the-direct-action ()
  (let ((main (transient-get-suffix 'magnus-dispatch "v"))
        (request (transient-get-suffix 'magnus-review-request-menu "RET")))
    (should (eq (plist-get (nth 2 main) :command)
                'magnus-review-request-dispatch))
    (should (eq (plist-get (nth 2 request) :command)
                'magnus-transient-request-review))))

(ert-deftest magnus-review-direct-key-is-contextual-on-review-rows ()
  (let ((review (magnus-review--create
                 :id "contextual-review" :reviewer-name "keen-owl"))
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
                   (list :author candidate :action 'new)))
                ((symbol-function 'transient-setup)
                 (lambda (prefix &rest _args) (setq opened prefix))))
        (magnus-review-request-dispatch))
      (should (eq opened #'magnus-review-request-menu))
      (should (eq (plist-get magnus-transient--review-request-context :author)
                  author)))))

(ert-deftest magnus-review-transient-setup-renders-the-request-menu ()
  (let* ((author
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"))
         (magnus-transient--review-request-context
          (list :author author :action 'new)))
    (unwind-protect
        (progn
          ;; Construct the real popup so unsupported Transient group properties
          ;; fail here instead of only on a user's older installation.
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

(ert-deftest magnus-review-transient-rejects-a-stale-request-context ()
  (let* ((author
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"))
         (review
          (magnus-test-status--review-in-state
           'running
           :id "now-running" :author-instance-id "author"
           :reviewer-name "keen-owl" :lifecycle 'open))
         (cached (list :author author :root "/tmp/project" :task "Task"
                       :review nil :action 'new))
         (fresh (list :author author :root "/tmp/project" :task "Task"
                      :review review :action 'running)))
    (cl-letf (((symbol-function 'magnus-review-request-context)
               (lambda (_author) fresh)))
      (should-error
       (magnus-transient--validated-review-request-context cached)
       :type 'user-error)
      (should (eq magnus-transient--review-request-context fresh)))
    (setq magnus-transient--review-request-context nil)))

(ert-deftest magnus-review-transient-rejects-same-action-checkpoint-aba ()
  (let* ((author
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"))
         (first
          (magnus-review-checkpoint-request--create
           :number 1 :token "checkpoint-one" :requested-at 1 :events nil))
         (review
          (magnus-review--create
           :id "waiting-review" :author-instance-id "author"
           :lifecycle 'open
           :checkpoint-requests (list first)))
         cached fresh)
    (cl-letf (((symbol-function 'magnus-review-git-root)
               (lambda (_directory) "/tmp/project"))
              ((symbol-function 'magnus-review-controller--task)
               (lambda (_author _root) "Task"))
              ((symbol-function
                'magnus-review-controller--matching-open-review)
               (lambda (_author _root _task) review)))
      (setq cached (magnus-review-request-context author))
      ;; Request one resolves and request two replaces it.  The broad action
      ;; remains `waiting', so only the immutable operation key exposes ABA.
      (setf (magnus-review-checkpoint-request-events first)
            (list
             (magnus-review-checkpoint-event--create
              :kind 'round :round-number 1 :recorded-at 2))
            (magnus-review-checkpoint-requests review)
            (append
             (magnus-review-checkpoint-requests review)
             (list
              (magnus-review-checkpoint-request--create
               :number 2 :token "checkpoint-two"
               :requested-at 3 :events nil))))
      (setq fresh (magnus-review-request-context author)))
    (should (eq (plist-get cached :action) 'waiting))
    (should (eq (plist-get fresh :action) 'waiting))
    (let ((magnus-transient--review-request-context cached))
      (cl-letf (((symbol-function 'magnus-review-request-context)
                 (lambda (_author) fresh)))
        (should-error
         (magnus-transient--validated-review-request-context cached)
         :type 'user-error)
        (should (eq magnus-transient--review-request-context fresh))))))

(ert-deftest magnus-review-action-transient-rejects-resolved-checkpoint ()
  (let* ((request
          (magnus-review-checkpoint-request--create
           :number 1 :token "checkpoint-one" :requested-at 1 :events nil))
         (review
          (magnus-review--create
           :id "review-a" :lifecycle 'open
           :checkpoint-requests (list request)))
         (magnus-transient--review-action-context
          (magnus-transient--make-review-action-context review))
         (transient-current-command 'magnus-review-actions-menu)
         called)
    (setf (magnus-review-checkpoint-request-events request)
          (list
           (magnus-review-checkpoint-event--create
            :kind 'round :round-number 1 :recorded-at 2))
          (magnus-review-rounds review)
          (list
           (magnus-review-round--create
            :number 1 :head-oid "head-one"
            :attempts (magnus-test-status--attempts-for-state 'complete))))
    (cl-letf (((symbol-function 'magnus-review-get)
               (lambda (_id) review))
              ((symbol-function 'magnus-review-rereview)
               (lambda (_review) (setq called t)))
              ((symbol-function 'magnus-status-refresh) #'ignore))
      (should-error (magnus-transient-review-rereview) :type 'user-error))
    (should-not called)))

(ert-deftest magnus-review-action-transient-rejects-checkpoint-aba ()
  (let* ((first
          (magnus-review-checkpoint-request--create
           :number 1 :token "checkpoint-one" :requested-at 1 :events nil))
         (review
          (magnus-review--create
           :id "review-a" :lifecycle 'open
           :checkpoint-requests (list first)))
         (magnus-transient--review-action-context
          (magnus-transient--make-review-action-context review))
         (transient-current-command 'magnus-review-actions-menu)
         called)
    (setf (magnus-review-checkpoint-request-events first)
          (list
           (magnus-review-checkpoint-event--create
            :kind 'round :round-number 1 :recorded-at 2))
          (magnus-review-checkpoint-requests review)
          (append
           (magnus-review-checkpoint-requests review)
           (list
            (magnus-review-checkpoint-request--create
             :number 2 :token "checkpoint-two"
             :requested-at 3 :events nil))))
    (cl-letf (((symbol-function 'magnus-review-get)
               (lambda (_id) review))
              ((symbol-function 'magnus-review-rereview)
               (lambda (_review) (setq called t)))
              ((symbol-function 'magnus-status-refresh) #'ignore))
      (should-error (magnus-transient-review-rereview) :type 'user-error))
    (should-not called)))

(ert-deftest magnus-review-action-transient-does-not-retarget-at-point ()
  (let* ((old-round
          (magnus-review-round--create
           :number 1 :head-oid "head-a"
           :attempts (magnus-test-status--attempts-for-state 'complete)))
         (old-a
          (magnus-review--create
           :id "review-a" :lifecycle 'open :rounds (list old-round)))
         (fresh-round
          (magnus-review-round--create
           :number 1 :head-oid "head-a"
           :attempts (magnus-test-status--attempts-for-state 'complete)))
         (fresh-a
          (magnus-review--create
           :id "review-a" :lifecycle 'open :rounds (list fresh-round)))
         (review-b
          (magnus-review--create
           :id "review-b" :lifecycle 'open
           :rounds
           (list
            (magnus-review-round--create
             :number 1 :head-oid "head-b"
             :attempts
             (magnus-test-status--attempts-for-state 'complete)))))
         (magnus-transient--review-action-context
          (magnus-transient--make-review-action-context old-a))
         (transient-current-command 'magnus-review-actions-menu)
         targeted)
    (cl-letf (((symbol-function 'magnus-review-get)
               (lambda (id) (and (string= id "review-a") fresh-a)))
              ((symbol-function 'magnus-status--get-review-at-point)
               (lambda () review-b))
              ((symbol-function 'magnus-review-rereview)
               (lambda (review) (setq targeted review)))
              ((symbol-function 'magnus-status-refresh) #'ignore))
      (magnus-transient-review-rereview))
    (should (eq targeted fresh-a))
    (should-not (eq targeted review-b))))

(ert-deftest magnus-review-action-transient-resolves-fresh-historical-round ()
  (let* ((old-round
          (magnus-review-round--create
           :number 1 :head-oid "head-a"
           :attempts (magnus-test-status--attempts-for-state 'complete)))
         (old-review
          (magnus-review--create
           :id "review-a" :lifecycle 'open :rounds (list old-round)))
         (fresh-round
          (magnus-review-round--create
           :number 1 :head-oid "head-a"
           :attempts (magnus-test-status--attempts-for-state 'complete)))
         (fresh-review
          (magnus-review--create
           :id "review-a" :lifecycle 'open :rounds (list fresh-round)))
         (magnus-transient--review-action-context
          (magnus-transient--make-review-action-context old-review old-round))
         (transient-current-command 'magnus-review-actions-menu)
         delivered)
    (cl-letf (((symbol-function 'magnus-review-get)
               (lambda (_id) fresh-review))
              ((symbol-function 'magnus-review-retry-delivery)
               (lambda (review round)
                 (setq delivered (list review round))))
              ((symbol-function 'magnus-status-refresh) #'ignore))
      (magnus-transient-review-delivery))
    (should (equal delivered (list fresh-review fresh-round)))))

(ert-deftest magnus-status-animates-only-active-review-execution ()
  (let* ((instance
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"
           :created-at (current-time) :status 'running))
         (magnus-status-review-animation-interval 0.4))
    (dolist (state '(starting running))
      (let* ((review
              (magnus-test-status--review-in-state
               state
               :id (symbol-name state) :author-instance-id "author"
               :reviewer-name "keen-owl" :lifecycle 'open))
             (rendered (magnus-test-status--render-instance
                        instance (list review))))
        (should (string-match-p (regexp-quote "[review |]") (car rendered)))
        (should (number-or-marker-p (cadr rendered)))))
    (dolist (state '(waiting-for-checkpoint queued complete failed interrupted))
      (let* ((review
              (magnus-test-status--review-in-state
               state
               :id (symbol-name state) :author-instance-id "author"
               :reviewer-name "keen-owl" :lifecycle 'open))
             (rendered (magnus-test-status--render-instance
                        instance (list review))))
        (should-not (string-match-p "\\[review" (car rendered)))
        (should-not (cadr rendered))))
    (let* ((review
            (magnus-test-status--review-in-state
             'running
             :id "archived" :author-instance-id "author"
             :reviewer-name "keen-owl" :lifecycle 'archived))
           (rendered (magnus-test-status--render-instance
                      instance (list review))))
      (should-not (cadr rendered)))))

(ert-deftest magnus-status-review-badge-matches-durable-author-id ()
  (let* ((instance
          (magnus-instance--create
           :id "author-a" :name "quick-wren" :directory "/tmp/project"
           :created-at (current-time) :status 'running))
         (review
          (magnus-test-status--review-in-state
           'running
           :id "other-author" :author-instance-id "author-b"
           :reviewer-name "keen-owl" :lifecycle 'open))
         (rendered (magnus-test-status--render-instance instance (list review))))
    (should-not (cadr rendered))))

(ert-deftest magnus-status-review-badge-handles-multiple-and-static-reviews ()
  (let* ((instance
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"
           :created-at (current-time) :status 'running))
         (reviews
          (list
           (magnus-test-status--review-in-state
            'running
            :id "first" :author-instance-id "author"
            :reviewer-name "keen-owl" :lifecycle 'open)
           (magnus-test-status--review-in-state
            'starting
            :id "second" :author-instance-id "author"
            :reviewer-name "swift-hare" :lifecycle 'open))))
    (let* ((magnus-status-review-animation-interval 0.4)
           (rendered (magnus-test-status--render-instance instance reviews)))
      (should (string-match-p (regexp-quote "[2 reviews |]") (car rendered)))
      (should (cadr rendered)))
    (let* ((magnus-status-review-animation-interval nil)
           (rendered (magnus-test-status--render-instance
                      instance (list (car reviews)))))
      (should (string-match-p (regexp-quote "[review]") (car rendered)))
      (should-not (cadr rendered)))))

(ert-deftest magnus-status-review-animation-tick-is-presentation-only ()
  (let* ((magnus-buffer-name " *magnus-status-animation-test*")
         (magnus-status-review-animation-interval 0.4)
         (magnus-status--review-animation-frame 0)
         (buffer (get-buffer-create magnus-buffer-name)))
    (unwind-protect
        (with-current-buffer buffer
          (insert (propertize "|" 'magnus-review-animation-slot t
                             'magnus-instance-id "author"))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (&rest _args) t)))
            (magnus-status--review-animation-tick))
          (should (equal (buffer-string) "|"))
          (should (equal (get-text-property (point-min) 'display) "/"))
          (should (equal (get-text-property (point-min) 'magnus-instance-id)
                         "author"))
          (should (= (point) (point-min)))
          (should-not (buffer-modified-p)))
      (magnus-status-stop-review-animation)
      (kill-buffer buffer))))

(ert-deftest magnus-status-review-animation-reuses-and-stops-one-timer ()
  (let ((magnus-status-review-animation-interval 0.4)
        (magnus-status--review-animation-timer nil)
        (started 0)
        (cancelled 0)
        (fake-timer (list 'fake-timer)))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "|" 'magnus-review-animation-slot t)))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _args) t))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _args)
                   (cl-incf started)
                   fake-timer))
                ((symbol-function 'timerp)
                 (lambda (value) (eq value fake-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_timer) (cl-incf cancelled))))
        (magnus-status--sync-review-animation)
        (magnus-status--sync-review-animation)
        (should (= started 1))
        (let ((inhibit-read-only t))
          (remove-text-properties (point-min) (point-max)
                                  '(magnus-review-animation-slot nil)))
        (magnus-status--sync-review-animation)
        (should (= cancelled 1))
        (should-not magnus-status--review-animation-timer)))))

(ert-deftest magnus-status-hidden-buffer-stops-review-animation ()
  (let ((magnus-status-review-animation-interval 0.4)
        (cancelled 0)
        (started 0)
        (visible nil)
        (fake-timer (list 'fake-timer)))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "|" 'magnus-review-animation-slot t)))
      (setq magnus-status--review-animation-timer fake-timer)
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _args) visible))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _args)
                   (cl-incf started)
                   fake-timer))
                ((symbol-function 'timerp)
                 (lambda (value) (eq value fake-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_timer) (cl-incf cancelled))))
        (magnus-status--sync-review-animation)
        (should (= cancelled 1))
        (should-not magnus-status--review-animation-timer)
        ;; Ordinary window history/buffer switching reaches this buffer-local
        ;; visibility hook without going through `magnus-status'.
        (setq visible t)
        (magnus-status--window-state-change nil)
        (should (= started 1))
        (should (eq magnus-status--review-animation-timer fake-timer))
        (magnus-status-stop-review-animation)))))

(ert-deftest magnus-status-motion-opt-out-normalizes-a-live-badge ()
  (let ((magnus-status-review-animation-interval nil)
        (magnus-status--review-animation-timer nil)
        (refreshes 0))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "|" 'magnus-review-animation-slot t)))
      (cl-letf (((symbol-function 'magnus-status-refresh)
                 (lambda () (cl-incf refreshes))))
        (magnus-status--sync-review-animation))
      (should (= refreshes 1)))))

(ert-deftest magnus-status-review-refresh-sees-other-frames ()
  (let* ((magnus-buffer-name " *magnus-other-frame-test*")
         (buffer (get-buffer-create magnus-buffer-name))
         (all-frames nil)
         (refreshes 0))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (_buffer all)
                     (setq all-frames all)
                     t))
                  ((symbol-function 'magnus-status-refresh)
                   (lambda () (cl-incf refreshes))))
          (magnus-status--maybe-refresh)
          (should all-frames)
          (should (= refreshes 1)))
      (kill-buffer buffer))))

(provide 'magnus-status-tests)
;;; magnus-status-tests.el ends here
