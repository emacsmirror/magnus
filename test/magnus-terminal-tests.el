;;; magnus-terminal-tests.el --- Shared terminal substrate tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

;; CI does not install vterm.  The unit tests replace its entry points.
(unless (featurep 'vterm)
  (provide 'vterm))

(require 'magnus-terminal)

(ert-deftest magnus-terminal-init-failure-does-not-leak-buffer ()
  (let ((name " *magnus-failed-vterm*"))
    (when-let ((old (get-buffer name)))
      (kill-buffer old))
    (cl-letf (((symbol-function 'vterm-mode)
               (lambda () (error "vterm initialization failed"))))
      (should-error (magnus-terminal-create-buffer name)))
    (should-not (get-buffer name))))

(ert-deftest magnus-terminal-setup-failure-discards-partial-process ()
  (let ((name " *magnus-partial-vterm*")
        process)
    (when-let ((old (get-buffer name)))
      (kill-buffer old))
    (cl-letf (((symbol-function 'vterm-mode)
               (lambda ()
                 (setq process
                       (make-pipe-process
                        :name (generate-new-buffer-name
                               "magnus-partial-vterm")
                        :buffer (current-buffer)))))
              ((symbol-function 'magnus-terminal-setup-keys)
               (lambda () (error "terminal key setup failed"))))
      (should-error (magnus-terminal-create-buffer name)))
    (should process)
    (should-not (process-live-p process))
    (should-not (process-query-on-exit-flag process))
    (should-not (get-buffer name))))

(ert-deftest magnus-terminal-setup-keys-binds-quit-to-escape ()
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (magnus-terminal-setup-keys)
    (should (eq (local-key-binding (kbd "C-g"))
                #'magnus-terminal-send-escape))))

(ert-deftest magnus-terminal-send-escape-forwards-to-vterm ()
  (let (sent)
    (cl-letf (((symbol-function 'vterm-send-key)
               (lambda (&rest arguments) (setq sent arguments))))
      (magnus-terminal-send-escape))
    (should (equal sent '("<escape>")))))

(ert-deftest magnus-terminal-applies-isolated-process-environment ()
  (let ((process-environment '("KEEP=yes" "MAGNUS_TEST_ID=old"))
        observed
        buffer)
    (cl-letf (((symbol-function 'vterm-mode)
               (lambda () (setq observed process-environment)))
              ((symbol-function 'magnus-terminal-setup-keys) #'ignore))
      (setq buffer
            (magnus-terminal-create-buffer
             " *magnus-environment*"
             '("MAGNUS_TEST_ID=new"
               "MAGNUS_TEST_NAME=fixture"))))
    (unwind-protect
        (progn
          (should (member "KEEP=yes" observed))
          (should (member "MAGNUS_TEST_ID=new" observed))
          (should (member "MAGNUS_TEST_NAME=fixture" observed))
          (should-not (member "MAGNUS_TEST_ID=old" observed))
          (should (equal process-environment
                         '("KEEP=yes" "MAGNUS_TEST_ID=old"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magnus-terminal-cancels-only-the-requested-delivery-scope ()
  (let* ((buffer (generate-new-buffer " *magnus-terminal-scopes*"))
         (process
          (make-pipe-process :name "magnus-terminal-scopes"
                             :buffer buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "scoped" :name "scoped" :buffer buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         timer events)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p) (push text events)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push 'return events)))
                  ((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat function &rest arguments)
                     (setq timer (cons function arguments))
                     'settle-timer)))
          (should (eq (magnus-terminal-submit
                       instance "review" nil :settle-delay 1
                       :scope 'review-controller)
                      'submitted))
          (should (eq (magnus-terminal-submit
                       instance "codex" nil :scope 'codex)
                      'queued))
          (magnus-terminal-cancel-scope 'review-controller)
          ;; The review entry was already submitted and is only settling;
          ;; cancelling its scope must retain both its timer and later Codex
          ;; work on the same exact process.
          (should timer)
          (should
           (equal (mapcar
                   (lambda (entry) (plist-get entry :text))
                   (process-get process 'magnus-terminal-delivery-queue))
                  '("codex")))
          (apply (car timer) (cdr timer))
          (should (equal (nreverse events)
                         '("review" return "codex" return)))
          (should (magnus-terminal-delivery-idle-p process)))
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(ert-deftest magnus-terminal-deduplication-is-scoped ()
  (let* ((buffer (generate-new-buffer " *magnus-terminal-dedup-scope*"))
         (process
          (make-pipe-process :name "magnus-terminal-dedup-scope"
                             :buffer buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "dedup" :name "dedup" :buffer buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         (not-ready (lambda (_process) nil)))
    (unwind-protect
        (progn
          (magnus-terminal-submit
           instance "same text" nil :ready-p not-ready
           :scope 'review-controller :deduplicate t)
          (magnus-terminal-submit
           instance "same text" nil :scope 'codex :deduplicate t)
          (should
           (equal (mapcar
                   (lambda (entry) (plist-get entry :scope))
                   (process-get process 'magnus-terminal-delivery-queue))
                  '(review-controller codex))))
      (magnus-terminal-release-process process)
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(provide 'magnus-terminal-tests)
;;; magnus-terminal-tests.el ends here
