;;; magnus-attention-tests.el --- Attention delivery boundary tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-attention)

(ert-deftest magnus-attention-delivery-state-fails-closed-when-inactive ()
  (let ((magnus-attention--monitoring-active nil)
        (magnus-attention-queue nil))
    (should (eq (magnus-attention-delivery-state "agent") 'inactive)))
  (let ((magnus-attention--monitoring-active t)
        (magnus-attention-queue '("flagged")))
    (should (eq (magnus-attention-delivery-state "flagged") 'flagged))
    (should (eq (magnus-attention-delivery-state "clear") 'clear))))

(ert-deftest magnus-attention-auto-approval-does-not-queue-stale-input ()
  (let* ((buffer (generate-new-buffer " *magnus-attention-auto*"))
         (instance
          (magnus-instance--create
           :id "auto" :name "auto" :buffer buffer :status 'running))
         (magnus-attention-auto-approve-patterns '("Read"))
         call)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-attention--tail-text)
                   (lambda () "Allow Read? [y/n]"))
                  ((symbol-function 'magnus-terminal-try-submit)
                   (lambda (given text &rest arguments)
                     (setq call (list given text arguments))
                     nil)))
          (should-not (magnus-attention--try-auto-approve instance))
          (should (eq (car call) instance))
          (should (equal (cadr call) "y")))
      (kill-buffer buffer))))

(ert-deftest magnus-attention-auto-approval-accepts-owned-pending-return ()
  (let* ((buffer (generate-new-buffer " *magnus-attention-pending*"))
         (instance
          (magnus-instance--create
           :id "pending" :name "pending" :buffer buffer :status 'running))
         (magnus-attention-auto-approve-patterns '("Read"))
         accepted)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-attention--tail-text)
                   (lambda () "Allow Read? [y/n]"))
                  ((symbol-function 'magnus-terminal-try-submit)
                   (lambda (_instance _text &rest arguments)
                     (setq accepted (plist-get arguments :accepted))
                     'queued)))
          (should (magnus-attention--try-auto-approve instance))
          (should (functionp accepted)))
      (kill-buffer buffer))))

(ert-deftest magnus-attention-start-failure-remains-inactive ()
  (let ((magnus-attention--monitoring-active t)
        (magnus-attention--timer nil)
        (magnus-attention--focus-timer nil))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest _arguments) (error "timer unavailable")))
              ((symbol-function 'message) #'ignore))
      (should-error (magnus-attention-start))
      (should-not magnus-attention--monitoring-active)
      (should-not magnus-attention--timer))))

(provide 'magnus-attention-tests)
;;; magnus-attention-tests.el ends here
