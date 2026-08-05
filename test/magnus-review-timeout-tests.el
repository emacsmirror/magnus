;;; magnus-review-timeout-tests.el --- Exact-owner review watchdog -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-review-controller)

(ert-deftest magnus-review-controller-active-p-tracks-owned-attempts ()
  (let ((magnus-review-controller--processes
         (make-hash-table :test #'equal)))
    (should-not (magnus-review-controller-active-p))
    (puthash "review-1"
             '(:process startup-process :round-number 1
               :attempt-token "token")
             magnus-review-controller--processes)
    (should (magnus-review-controller-active-p))
    (remhash "review-1" magnus-review-controller--processes)
    (should-not (magnus-review-controller-active-p))))

(ert-deftest magnus-review-watchdog-times-out-only-its-exact-owner ()
  (let ((magnus-review-controller--processes
         (make-hash-table :test #'equal))
        (process 'timed-out-process)
        (review 'review)
        (round 'round)
        (attempt 'attempt)
        cancelled
        failed
        (pumps 0))
    (puthash "review-1"
             (list :process process :round-number 2 :attempt-token "token-2")
             magnus-review-controller--processes)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (candidate) (eq candidate process)))
              ((symbol-function 'magnus-review-controller--context)
               (lambda (&rest _args) (list review round attempt)))
              ((symbol-function 'magnus-headless-cancel)
               (lambda (candidate &optional force)
                 ;; Ownership is revoked before a synchronous sentinel can run.
                 (should-not
                  (gethash "review-1" magnus-review-controller--processes))
                 (setq cancelled (list candidate force))))
              ((symbol-function 'magnus-review-fail-attempt)
               (lambda (&rest args) (setq failed args)))
              ((symbol-function 'magnus-review-controller--pump)
               (lambda () (cl-incf pumps))))
      (magnus-review-controller--watchdog-fired
       process "review-1" 2 "token-2" 12.5))
    (should (equal cancelled (list process t)))
    (should (equal (seq-take failed 3) (list review round attempt)))
    (should (equal (nth 3 failed)
                   "Review attempt timed out after 12.5 seconds"))
    (should (equal (nth 4 failed) "token-2"))
    (should (= pumps 1))
    (should-not (gethash "review-1" magnus-review-controller--processes))))

(ert-deftest magnus-review-late-watchdog-cannot-cancel-replacement-owner ()
  (let ((magnus-review-controller--processes
         (make-hash-table :test #'equal))
        (old-process 'old-process)
        (replacement-process 'replacement-process)
        touched)
    (puthash "review-1"
             (list :process replacement-process
                   :round-number 2
                   :attempt-token "replacement-token")
             magnus-review-controller--processes)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (_process) (setq touched t) t))
              ((symbol-function 'magnus-headless-cancel)
               (lambda (&rest _args) (setq touched t)))
              ((symbol-function 'magnus-review-controller--pump)
               (lambda () (setq touched t))))
      (magnus-review-controller--watchdog-fired
       old-process "review-1" 2 "old-token" 12.5))
    (should-not touched)
    (should (eq (plist-get
                 (gethash "review-1" magnus-review-controller--processes)
                 :process)
                replacement-process))))

(ert-deftest magnus-review-watchdog-lets-terminal-finalizer-win ()
  (let ((magnus-review-controller--processes
         (make-hash-table :test #'equal))
        (process 'terminal-process)
        touched)
    (puthash "review-1"
             (list :process process :round-number 1 :attempt-token "token")
             magnus-review-controller--processes)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) nil))
              ((symbol-function 'magnus-headless-cancel)
               (lambda (&rest _args) (setq touched t)))
              ((symbol-function 'magnus-review-fail-attempt)
               (lambda (&rest _args) (setq touched t)))
              ((symbol-function 'magnus-review-controller--pump)
               (lambda () (setq touched t))))
      (magnus-review-controller--watchdog-fired
       process "review-1" 1 "token" 60))
    (should-not touched)
    (should (gethash "review-1" magnus-review-controller--processes))))

(ert-deftest magnus-review-release-cancels-armed-watchdog ()
  (let ((magnus-review-controller--processes
         (make-hash-table :test #'equal))
        (magnus-review-attempt-timeout 30)
        (process 'review-process)
        cancelled
        scheduled
        (pumps 0))
    (puthash "review-1"
             (list :process process :round-number 1 :attempt-token "token")
             magnus-review-controller--processes)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest args) (setq scheduled args) 'fixture-timer))
              ((symbol-function 'timerp)
               (lambda (value) (eq value 'fixture-timer)))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (setq cancelled timer)))
              ((symbol-function 'magnus-review-controller--pump)
               (lambda () (cl-incf pumps))))
      (magnus-review-controller--arm-watchdog
       process "review-1" 1 "token")
      (should scheduled)
      (should (eq
               (plist-get
                (gethash "review-1" magnus-review-controller--processes)
                :watchdog-timer)
               'fixture-timer))
      (magnus-review-controller--release
       process "review-1" 1 "token"))
    (should (eq cancelled 'fixture-timer))
    (should (= pumps 1))
    (should-not (gethash "review-1" magnus-review-controller--processes))))

(provide 'magnus-review-timeout-tests)
;;; magnus-review-timeout-tests.el ends here
