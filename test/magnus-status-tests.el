;;; magnus-status-tests.el --- Status UX tests for Magnus -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
;; CI intentionally tests with `emacs -Q' and does not install vterm.  Status
;; rendering only needs the process function boundaries, not vterm itself.
(unless (featurep 'vterm)
  (provide 'vterm))
(require 'magnus-status)
(require 'magnus-transient)

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

(ert-deftest magnus-status-review-keys-are-directly-discoverable ()
  (should (eq (lookup-key magnus-status-mode-map (kbd "v"))
              'magnus-review-request-dispatch))
  (should (eq (lookup-key magnus-status-mode-map (kbd "V"))
              'magnus-review-actions)))

(ert-deftest magnus-review-transient-names-the-selected-operation ()
  (let* ((author
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"))
         (review
          (magnus-review--create
           :id "active" :author-instance-id "author"
           :author-name "quick-wren" :reviewer-name "keen-owl"
           :lifecycle 'open :execution 'running)))
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
          (should (magnus-transient--review-request-busy-p)))
      (setq magnus-transient--review-request-context nil))))

(ert-deftest magnus-review-transient-wires-the-direct-action ()
  (let ((main (transient-get-suffix 'magnus-dispatch "v"))
        (request (transient-get-suffix 'magnus-review-request-menu "RET")))
    (should (eq (plist-get (nth 2 main) :command)
                'magnus-review-request-dispatch))
    (should (eq (plist-get (nth 2 request) :command)
                'magnus-transient-request-review))))

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
          (magnus-review--create
           :id "now-running" :author-instance-id "author"
           :reviewer-name "keen-owl" :lifecycle 'open :execution 'running))
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

(ert-deftest magnus-status-animates-only-active-review-execution ()
  (let* ((instance
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"
           :created-at (current-time) :status 'running))
         (magnus-status-review-animation-interval 0.4))
    (dolist (state '(starting running))
      (let* ((review
              (magnus-review--create
               :id (symbol-name state) :author-instance-id "author"
               :reviewer-name "keen-owl" :lifecycle 'open :execution state))
             (rendered (magnus-test-status--render-instance
                        instance (list review))))
        (should (string-match-p (regexp-quote "[review |]") (car rendered)))
        (should (number-or-marker-p (cadr rendered)))))
    (dolist (state '(waiting-for-checkpoint queued complete failed interrupted))
      (let* ((review
              (magnus-review--create
               :id (symbol-name state) :author-instance-id "author"
               :reviewer-name "keen-owl" :lifecycle 'open :execution state))
             (rendered (magnus-test-status--render-instance
                        instance (list review))))
        (should-not (string-match-p "\\[review" (car rendered)))
        (should-not (cadr rendered))))
    (let* ((review
            (magnus-review--create
             :id "archived" :author-instance-id "author"
             :reviewer-name "keen-owl" :lifecycle 'archived
             :execution 'running))
           (rendered (magnus-test-status--render-instance
                      instance (list review))))
      (should-not (cadr rendered)))))

(ert-deftest magnus-status-review-badge-matches-durable-author-id ()
  (let* ((instance
          (magnus-instance--create
           :id "author-a" :name "quick-wren" :directory "/tmp/project"
           :created-at (current-time) :status 'running))
         (review
          (magnus-review--create
           :id "other-author" :author-instance-id "author-b"
           :reviewer-name "keen-owl" :lifecycle 'open :execution 'running))
         (rendered (magnus-test-status--render-instance instance (list review))))
    (should-not (cadr rendered))))

(ert-deftest magnus-status-review-badge-handles-multiple-and-static-reviews ()
  (let* ((instance
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"
           :created-at (current-time) :status 'running))
         (reviews
          (list
           (magnus-review--create
            :id "first" :author-instance-id "author"
            :reviewer-name "keen-owl" :lifecycle 'open :execution 'running)
           (magnus-review--create
            :id "second" :author-instance-id "author"
            :reviewer-name "swift-hare" :lifecycle 'open :execution 'starting))))
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
