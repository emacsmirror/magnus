;;; magnus-doctor-tests.el --- Magnus diagnostics tests -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)
(require 'magnus-doctor)

(ert-deftest magnus-doctor-reports-missing-provider-without-failing-run ()
  (let ((magnus-claude-executable "missing-claude")
        (magnus-codex-executable "codex"))
    (cl-letf (((symbol-function 'locate-library)
               (lambda (library) (format "/packages/%s.el" library)))
              ((symbol-function 'executable-find)
               (lambda (command)
                 (unless (string= command "missing-claude")
                   (format "/bin/%s" command))))
              ((symbol-function 'magnus-doctor--storage-check)
               (lambda (id _path label _directory-p)
                 (magnus-doctor--check id 'ok label)))
              ((symbol-function 'magnus-instances-list) (lambda () nil)))
      (let* ((checks (magnus-doctor-run))
             (claude (cl-find 'claude checks
                              :key #'magnus-doctor-check-id)))
        (should (eq (magnus-doctor-check-severity claude) 'warning))
        (should (string-match-p "unavailable"
                                (magnus-doctor-check-summary claude)))))))

(ert-deftest magnus-doctor-reports-no-usable-provider-as-an-error ()
  (let ((magnus-claude-executable "missing-claude")
        (magnus-codex-executable "missing-codex"))
    (cl-letf (((symbol-function 'locate-library)
               (lambda (library) (format "/packages/%s.el" library)))
              ((symbol-function 'executable-find) (lambda (_command) nil))
              ((symbol-function 'magnus-doctor--storage-check)
               (lambda (id _path label _directory-p)
                 (magnus-doctor--check id 'ok label)))
              ((symbol-function 'magnus-instances-list) (lambda () nil)))
      (let* ((checks (magnus-doctor-run))
             (provider (cl-find 'provider checks
                                :key #'magnus-doctor-check-id)))
        (should (eq (magnus-doctor-check-severity provider) 'error))
        (should (string-match-p "No agent provider"
                                (magnus-doctor-check-summary provider)))))))

(ert-deftest magnus-doctor-detects-overbroad-durable-permissions ()
  (let* ((directory (make-temp-file "magnus-doctor-storage-" t))
         (file (expand-file-name "state.el" directory)))
    (unwind-protect
        (progn
          (write-region "state" nil file nil 'quiet)
          (set-file-modes file #o644)
          (let ((check (magnus-doctor--storage-check
                        'state file "Instance state" nil)))
            (should (eq (magnus-doctor-check-severity check) 'warning))
            (should (string-match-p "broader"
                                    (magnus-doctor-check-summary check)))))
      (delete-directory directory t))))

(ert-deftest magnus-doctor-checks-state-parent-before-file-exists ()
  (let* ((directory (make-temp-file "magnus-doctor-parent-" t))
         (file (expand-file-name "state.el" directory)))
    (unwind-protect
        (progn
          (set-file-modes directory #o755)
          (let ((check (magnus-doctor--storage-check
                        'state file "Instance state" nil)))
            (should (eq (magnus-doctor-check-severity check) 'warning))
            (should (equal (magnus-doctor-check-detail check)
                           (format
                            (concat "%s has mode 755; Magnus-managed durable "
                                    "state should be 600/700.")
                            (file-name-as-directory directory))))))
      (delete-directory directory t))))

(ert-deftest magnus-doctor-reports-missing-instance-directory ()
  (let* ((missing (expand-file-name
                   (format "magnus-missing-%s" (random most-positive-fixnum))
                   temporary-file-directory))
         (instance (magnus-instance--create
                    :id "doctor-agent" :name "keen-owl"
                    :directory missing)))
    (cl-letf (((symbol-function 'magnus-instances-list)
               (lambda () (list instance))))
      (let ((check (car (magnus-doctor--instance-checks))))
        (should (eq (magnus-doctor-check-severity check) 'error))
        (should (string-match-p "keen-owl"
                                (magnus-doctor-check-summary check)))))))

(ert-deftest magnus-doctor-buffer-renders-severity-and-remediation ()
  (with-temp-buffer
    (magnus-doctor-mode)
    (cl-letf (((symbol-function 'magnus-doctor-run)
               (lambda ()
                 (list
                  (magnus-doctor--check 'ok 'ok "Everything works" "/bin/tool")
                  (magnus-doctor--check
                   'bad 'error "Something failed" "Install it")))))
      (magnus-doctor-refresh))
    (should (string-match-p "OK[ ]+Everything works" (buffer-string)))
    (should (string-match-p "ERROR[ ]+Something failed" (buffer-string)))
    (should (string-match-p "Install it" (buffer-string)))))

(provide 'magnus-doctor-tests)
;;; magnus-doctor-tests.el ends here
