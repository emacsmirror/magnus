;;; magnus-transient-tests.el --- Magnus command-menu tests -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)
(require 'magnus-transient)

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

(provide 'magnus-transient-tests)
;;; magnus-transient-tests.el ends here
