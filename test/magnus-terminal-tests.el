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

(provide 'magnus-terminal-tests)
;;; magnus-terminal-tests.el ends here
