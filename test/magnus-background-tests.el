;;; magnus-background-tests.el --- Bounded background queue tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-background)

(defvar magnus-background-test--callbacks nil)
(defvar magnus-background-test--starts nil)
(defvar magnus-background-test--cancelled nil)
(defvar magnus-background-test--cancel-completes nil)
(defvar magnus-background-test--live 0)
(defvar magnus-background-test--max-live 0)
(defvar magnus-background-test--next-process 0)

(defun magnus-background-test--start (_provider request callbacks)
  "Record a fake headless launch for REQUEST and CALLBACKS."
  (when (equal (plist-get request :prompt) "launch-error")
    (error "simulated launch failure"))
  (let ((process (list 'fake-process
                       (cl-incf magnus-background-test--next-process))))
    (push (cons process callbacks) magnus-background-test--callbacks)
    (setq magnus-background-test--starts
          (append magnus-background-test--starts
                  (list (cons process (plist-get request :prompt)))))
    (cl-incf magnus-background-test--live)
    (setq magnus-background-test--max-live
          (max magnus-background-test--max-live
               magnus-background-test--live))
    process))

(defun magnus-background-test--callbacks (process)
  "Return callbacks captured for fake PROCESS."
  (cdr (assq process magnus-background-test--callbacks)))

(defun magnus-background-test--event (process text)
  "Emit visible assistant TEXT from fake PROCESS."
  (funcall (plist-get (magnus-background-test--callbacks process) :on-event)
           process (list :type "assistant" :text text)))

(defun magnus-background-test--complete (process &optional success)
  "Complete fake PROCESS, with SUCCESS defaulting to non-nil."
  (setq magnus-background-test--live
        (max 0 (1- magnus-background-test--live)))
  (funcall (plist-get (magnus-background-test--callbacks process) :on-complete)
           process (list :success-p (if (null success) t success))))

(defun magnus-background-test--cancel (process &optional _force)
  "Record cancellation of fake PROCESS.
Optionally simulate a harness which completes synchronously from cancellation."
  (push process magnus-background-test--cancelled)
  (setq magnus-background-test--live
        (max 0 (1- magnus-background-test--live)))
  (when magnus-background-test--cancel-completes
    (funcall
     (plist-get (magnus-background-test--callbacks process) :on-complete)
     process '(:success-p nil :process-event "killed"))))

(defmacro magnus-background-test--with-runner (&rest body)
  "Run BODY with a fresh fake background runner."
  (declare (indent 0) (debug t))
  `(let ((magnus-background--queue nil)
         (magnus-background--active nil)
         (magnus-background--next-id 0)
         (magnus-background--dispatching nil)
         (magnus-background--accepting-p nil)
         (magnus-background-queue-limit 32)
         (magnus-background-output-limit 1024)
         (magnus-background-timeout 90)
         (magnus-background-test--callbacks nil)
         (magnus-background-test--starts nil)
         (magnus-background-test--cancelled nil)
         (magnus-background-test--cancel-completes nil)
         (magnus-background-test--live 0)
         (magnus-background-test--max-live 0)
         (magnus-background-test--next-process 0)
         (fake-timers nil))
     (cl-letf (((symbol-function 'magnus-headless-start)
                #'magnus-background-test--start)
               ((symbol-function 'magnus-headless-cancel)
                #'magnus-background-test--cancel)
               ((symbol-function 'run-at-time)
                (lambda (&rest _arguments)
                  (let ((timer (list 'fake-timer)))
                    (push timer fake-timers)
                    timer)))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (setq fake-timers (delq timer fake-timers))))
               ((symbol-function 'message) (lambda (&rest _arguments))))
       (unwind-protect
           (progn
             (magnus-background-setup)
             ,@body)
         (magnus-background-shutdown)))))

(defun magnus-background-test--request (prompt)
  "Return a minimal fake headless request containing PROMPT."
  (list :purpose 'agent :directory default-directory :prompt prompt))

(ert-deftest magnus-background-serializes-jobs-in-fifo-order ()
  (magnus-background-test--with-runner
    (magnus-background-submit 'a 'fake
                              (magnus-background-test--request "a"))
    (magnus-background-submit 'b 'fake
                              (magnus-background-test--request "b"))
    (magnus-background-submit 'c 'fake
                              (magnus-background-test--request "c"))
    (should (equal (mapcar #'cdr magnus-background-test--starts) '("a")))
    (let ((a (caar magnus-background-test--starts)))
      (magnus-background-test--complete a))
    (should (equal (mapcar #'cdr magnus-background-test--starts)
                   '("a" "b")))
    (let ((b (car (nth 1 magnus-background-test--starts))))
      (magnus-background-test--complete b))
    (should (equal (mapcar #'cdr magnus-background-test--starts)
                   '("a" "b" "c")))
    (should (= magnus-background-test--max-live 1))))

(ert-deftest magnus-background-failure-does-not-stall-successor ()
  (magnus-background-test--with-runner
    (magnus-background-submit 'first 'fake
                              (magnus-background-test--request "first"))
    (magnus-background-submit 'bad 'fake
                              (magnus-background-test--request "launch-error"))
    (magnus-background-submit 'last 'fake
                              (magnus-background-test--request "last"))
    (magnus-background-test--complete
     (caar magnus-background-test--starts))
    (should (equal (mapcar #'cdr magnus-background-test--starts)
                   '("first" "last")))
    (should (equal (magnus-background-job-key magnus-background--active)
                   'last))))

(ert-deftest magnus-background-timeout-revokes-before-synchronous-completion ()
  (magnus-background-test--with-runner
    (let (result)
      (magnus-background-submit
       'slow 'fake (magnus-background-test--request "slow")
       (list :on-complete (lambda (value) (setq result value))))
      (magnus-background-submit 'next 'fake
                                (magnus-background-test--request "next"))
      (let* ((job magnus-background--active)
             (process (magnus-background-job-process job)))
        (setq magnus-background-test--cancel-completes t)
        (magnus-background--timeout job process)
        (should (plist-get result :timed-out-p))
        (should (eq (plist-get result :background-error) 'timeout))
        (should (equal (magnus-background-job-key
                        magnus-background--active)
                       'next))
        (should (= (length magnus-background-test--starts) 2))))))

(ert-deftest magnus-background-stale-timeout-and-callback-cannot-touch-successor ()
  (magnus-background-test--with-runner
    (let ((completions 0))
      (magnus-background-submit
       'old 'fake (magnus-background-test--request "old")
       (list :on-complete (lambda (_result) (cl-incf completions))))
      (magnus-background-submit 'new 'fake
                                (magnus-background-test--request "new"))
      (let* ((old-job magnus-background--active)
             (old-process (magnus-background-job-process old-job)))
        (magnus-background-test--complete old-process)
        (should (= completions 1))
        (magnus-background--timeout old-job old-process)
        (magnus-background-test--complete old-process)
        (should (= completions 1))
        (should (equal (magnus-background-job-key
                        magnus-background--active)
                       'new))))))

(ert-deftest magnus-background-bounds-visible-output ()
  (magnus-background-test--with-runner
    (let ((magnus-background-output-limit 5)
          result)
      (magnus-background-submit
       'bounded 'fake (magnus-background-test--request "bounded")
       (list :on-complete (lambda (value) (setq result value))))
      (let ((process (magnus-background-job-process
                      magnus-background--active)))
        (magnus-background-test--event process "abc")
        (magnus-background-test--event process "defgh")
        (magnus-background-test--complete process))
      (should (equal (plist-get result :output) "defgh"))
      (should (plist-get result :output-truncated-p))
      (should (= (plist-get result :output-dropped-chars) 3)))))

(ert-deftest magnus-background-coalesces-keys-and-bounds-waiters ()
  (magnus-background-test--with-runner
    (let ((magnus-background-queue-limit 1))
      (magnus-background-submit 'active 'fake
                                (magnus-background-test--request "active"))
      (let ((queued
             (magnus-background-submit
              'same 'fake (magnus-background-test--request "same"))))
        (should (eq queued
                    (magnus-background-submit
                     'same 'fake
                     (magnus-background-test--request "replacement"))))
        (should (= (length magnus-background--queue) 1))
        (should-not
         (magnus-background-submit
          'overflow 'fake
          (magnus-background-test--request "overflow")))))))

(ert-deftest magnus-background-cancel-is-key-scoped ()
  (magnus-background-test--with-runner
    (magnus-background-submit 'a 'fake
                              (magnus-background-test--request "a"))
    (magnus-background-submit 'b 'fake
                              (magnus-background-test--request "b"))
    (magnus-background-submit 'c 'fake
                              (magnus-background-test--request "c"))
    (should (= (magnus-background-cancel 'b) 1))
    (should (= (magnus-background-cancel 'a) 1))
    (should (equal (mapcar #'cdr magnus-background-test--starts)
                   '("a" "c")))
    (should (= (length magnus-background-test--cancelled) 1))
    (should (equal (magnus-background-job-key magnus-background--active)
                   'c))))

(ert-deftest magnus-background-shutdown-releases-every-owned-resource ()
  (magnus-background-test--with-runner
    (let ((callbacks 0))
      (magnus-background-submit
       'active 'fake (magnus-background-test--request "active")
       (list :on-complete (lambda (_result) (cl-incf callbacks))))
      (magnus-background-submit 'waiting 'fake
                                (magnus-background-test--request "waiting"))
      (let ((process (magnus-background-job-process
                      magnus-background--active)))
        (magnus-background-shutdown)
        (should-not magnus-background--active)
        (should-not magnus-background--queue)
        (should-not magnus-background--accepting-p)
        (should (member process magnus-background-test--cancelled))
        (magnus-background-test--complete process)
        (should (= callbacks 0))
        (should-not
         (magnus-background-submit
          'rejected 'fake
          (magnus-background-test--request "rejected")))))))

(ert-deftest magnus-background-post-launch-timer-failure-cancels-real-child ()
  (magnus-background-test--with-runner
    (let (result)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _arguments)
                   (error "timer allocation failed"))))
        (magnus-background-submit
         'timer-failure 'fake
         (magnus-background-test--request "timer-failure")
         (list :on-complete (lambda (value) (setq result value)))))
      (should (eq (plist-get result :background-error) 'launch-error))
      (should (= (length magnus-background-test--cancelled) 1))
      (should-not magnus-background--active))))

(ert-deftest magnus-background-rejects-invalid-resource-bounds ()
  (magnus-background-test--with-runner
    (let ((magnus-background-output-limit "unbounded"))
      (should-not
       (magnus-background-submit
        'bad-output 'fake
        (magnus-background-test--request "bad-output"))))
    (let ((magnus-background-timeout nil))
      (should-not
       (magnus-background-submit
        'bad-timeout 'fake
        (magnus-background-test--request "bad-timeout"))))))

(provide 'magnus-background-tests)
;;; magnus-background-tests.el ends here
