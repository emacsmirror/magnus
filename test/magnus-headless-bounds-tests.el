;;; magnus-headless-bounds-tests.el --- Headless retention bounds -*- lexical-binding: t -*-

(require 'ert)
(require 'magnus-headless)

(defun magnus-test-headless-bounds--process ()
  "Return an inert pipe process suitable for aggregate-state tests."
  (make-pipe-process
   :name (generate-new-buffer-name "magnus-headless-bounds")
   :buffer nil :noquery t))

(defmacro magnus-test-headless-bounds--with-process (binding &rest body)
  "Bind BINDING to a temporary process while evaluating BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,binding (magnus-test-headless-bounds--process)))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p ,binding)
         (delete-process ,binding)))))

(ert-deftest magnus-headless-overlong-unterminated-line-is-bounded ()
  (magnus-test-headless-bounds--with-process process
    (let ((magnus-headless-jsonl-line-limit 8)
          errors
          raw)
      (process-put process 'magnus-headless-partial-line "")
      (process-put process 'magnus-headless-callbacks
                   (list :on-error
                         (lambda (_process failure) (push failure errors))
                         :on-raw-event
                         (lambda (_process line) (push line raw))))
      (process-put process 'magnus-headless-request nil)
      (process-put process 'magnus-headless-decoder
                   (lambda (_event _request) (list :type "event")))
      ;; No newline: the runner must stop retaining this record immediately.
      (magnus-headless--filter process "123456789")
      (should (equal (process-get process 'magnus-headless-partial-line) ""))
      (should (process-get process 'magnus-headless-discarding-line-p))
      ;; The first newline ends the discarded record; a later valid record is
      ;; decoded normally in the same stream.
      (magnus-headless--filter process "discarded-tail\n{\"x\":1}\n")
      (should-not (process-get process 'magnus-headless-discarding-line-p))
      (should (equal raw '("{\"x\":1}")))
      (should (= (process-get process
                              'magnus-headless-discarded-jsonl-lines)
                 1))
      (should (= (length errors) 1))
      (should (eq (plist-get (car errors) :kind) 'jsonl-line-too-long)))))

(ert-deftest magnus-headless-error-retention-reports-omissions ()
  (magnus-test-headless-bounds--with-process process
    (let ((magnus-headless-error-limit 2)
          (magnus-headless-error-detail-limit 5)
          delivered)
      (process-put process 'magnus-headless-callbacks
                   (list :on-error
                         (lambda (_process failure)
                           (push failure delivered))))
      (dotimes (index 5)
        (magnus-headless--record-error
         process 'magnus-headless-decode-errors 'fixture-error
         "fixture" :detail (format "detail-%d-long" index)))
      (let* ((result (magnus-headless--result process))
             (retained (plist-get result :decode-errors)))
        (should (= (length delivered) 5))
        (should (= (length retained) 2))
        (should (equal (plist-get (car retained) :detail) "detai…"))
        (should (plist-get result :errors-truncated-p))
        (should (= (plist-get (plist-get result :dropped-errors)
                              :decode-errors)
                   3))
        (should-not (plist-get result :success-p))))))

(ert-deftest magnus-headless-stderr-tail-reports-truncation ()
  (magnus-test-headless-bounds--with-process process
    (magnus-test-headless-bounds--with-process stderr-process
      (let ((magnus-headless-stderr-limit 5)
            chunks)
        (process-put stderr-process 'magnus-headless-process process)
        (process-put process 'magnus-headless-callbacks
                     (list :on-stderr
                           (lambda (_process chunk) (push chunk chunks))))
        (magnus-headless--stderr-filter stderr-process "0123")
        (magnus-headless--stderr-filter stderr-process "456789")
        (let ((result (magnus-headless--result process)))
          (should (equal (apply #'concat (nreverse chunks)) "0123456789"))
          (should (equal (plist-get result :stderr) "56789"))
          (should (plist-get result :stderr-truncated-p))
          (should (= (plist-get result :stderr-dropped-chars) 5)))))))

(provide 'magnus-headless-bounds-tests)
;;; magnus-headless-bounds-tests.el ends here
