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

(ert-deftest magnus-doctor-rejects-storage-path-type-mismatches ()
  (let* ((root (make-temp-file "magnus-doctor-types-" t))
         (state-directory (expand-file-name "state.el" root))
         (review-file (expand-file-name "reviews" root)))
    (unwind-protect
        (progn
          (make-directory state-directory)
          (write-region "not a directory" nil review-file nil 'quiet)
          (let ((state (magnus-doctor--storage-check
                        'state state-directory "Instance state" nil))
                (reviews (magnus-doctor--storage-check
                          'reviews review-file "Review storage" t)))
            (should (eq (magnus-doctor-check-severity state) 'error))
            (should (eq (magnus-doctor-check-severity reviews) 'error))
            (should (string-match-p "wrong path type"
                                    (magnus-doctor-check-summary state)))
            (should (string-match-p "wrong path type"
                                    (magnus-doctor-check-summary reviews)))))
      (delete-directory root t))))

(ert-deftest magnus-doctor-rejects-symlinked-storage-paths ()
  (let* ((root (make-temp-file "magnus-doctor-links-" t))
         (real-state (expand-file-name "real-state.el" root))
         (state-link (expand-file-name "state.el" root))
         (real-reviews (expand-file-name "real-reviews" root))
         (review-link (expand-file-name "reviews" root)))
    (unwind-protect
        (progn
          (write-region "state" nil real-state nil 'quiet)
          (make-directory real-reviews)
          (make-symbolic-link real-state state-link)
          (make-symbolic-link real-reviews review-link)
          (dolist (check
                   (list
                    (magnus-doctor--storage-check
                     'state state-link "Instance state" nil)
                    (magnus-doctor--storage-check
                     'reviews review-link "Review storage" t)))
            (should (eq (magnus-doctor-check-severity check) 'error))
            (should (string-match-p "symlink"
                                    (magnus-doctor-check-summary check)))))
      (delete-directory root t))))

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

(ert-deftest magnus-doctor-coordination-checks-each-active-project-once ()
  (let* ((first (magnus-instance--create
                 :id "first" :name "quick-wren" :directory "/project"))
         (second (magnus-instance--create
                  :id "second" :name "keen-owl" :directory "/project/"))
         (third (magnus-instance--create
                 :id "third" :name "swift-hare" :directory "/other"))
         calls)
    (cl-letf (((symbol-function 'magnus-instances-active-list)
               (lambda () (list first second third)))
              ((symbol-function 'magnus-coord-watched-directories)
               (lambda () '("/project/" "/review-only/")))
              ((symbol-function 'magnus-coord-review-retry-diagnostics)
               (lambda (directory)
                 (push directory calls)
                 '(:pending-review-retry-count 0
                   :exhausted-review-count 0))))
      (let ((checks (magnus-doctor--coordination-checks)))
        (should (= (length checks) 3))
        (should (= (cl-count 'ok checks
                             :key #'magnus-doctor-check-severity)
                   2))
        (should (= (cl-count 'warning checks
                             :key #'magnus-doctor-check-severity)
                   1))))
    (should (equal (sort calls #'string<)
                   '("/other/" "/project/" "/review-only/")))))

(ert-deftest magnus-doctor-finds-pending-review-without-a-watcher ()
  "A missing watcher cannot hide the pending review that needs diagnosis."
  (let* ((directory (make-temp-file "magnus-doctor-pending-review-" t))
         (canonical (file-name-as-directory (file-truename directory)))
         (review 'pending-review))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-instances-active-list)
                   (lambda () nil))
                  ((symbol-function 'magnus-review-list)
                   (lambda () (list review)))
                  ((symbol-function 'magnus-review-lifecycle)
                   (lambda (candidate)
                     (and (eq candidate review) 'open)))
                  ((symbol-function 'magnus-review-pending-checkpoint-request)
                   (lambda (candidate)
                     (and (eq candidate review) 'pending-request)))
                  ((symbol-function 'magnus-review-project-root)
                   (lambda (candidate)
                     (and (eq candidate review) directory)))
                  ((symbol-function 'magnus-coord-watched-directories)
                   (lambda () nil))
                  ((symbol-function 'magnus-coord-review-retry-diagnostics)
                   (lambda (_root)
                     '(:pending-review-retry-count 0
                       :exhausted-review-count 0))))
          (let ((checks (magnus-doctor--coordination-checks)))
            (should (= (length checks) 1))
            (let ((check (car checks)))
              (should (eq (magnus-doctor-check-severity check) 'warning))
              (should (string-match-p "watcher is idle"
                                      (magnus-doctor-check-summary check)))
              (should (string-match-p (regexp-quote canonical)
                                      (magnus-doctor-check-summary check))))))
      (delete-directory directory t))))

(ert-deftest magnus-doctor-classifies-watcher-and-retry-diagnostics ()
  (cl-letf (((symbol-function 'magnus-coord-watched-directories)
             (lambda () '("/project/")))
            ((symbol-function 'magnus-coord-review-retry-diagnostics)
             (lambda (_directory)
               '(:pending-review-retry-count 0
                 :exhausted-review-count 0))))
    (let ((check (magnus-doctor--coordination-check "/project/")))
      (should (eq (magnus-doctor-check-severity check) 'ok))
      (should (string-match-p "No review checkpoint retries"
                              (magnus-doctor-check-detail check)))))
  (cl-letf (((symbol-function 'magnus-coord-watched-directories)
             (lambda () '("/project/")))
            ((symbol-function 'magnus-coord-review-retry-diagnostics)
             (lambda (_directory)
               '(:pending-review-retry-count 2
                 :exhausted-review-count 0))))
    (let ((check (magnus-doctor--coordination-check "/project/")))
      (should (eq (magnus-doctor-check-severity check) 'warning))
      (should (string-match-p "2 review checkpoint retries pending"
                              (magnus-doctor-check-detail check)))))
  (cl-letf (((symbol-function 'magnus-coord-watched-directories)
             (lambda () nil))
            ((symbol-function 'magnus-coord-review-retry-diagnostics)
             (lambda (_directory)
               '(:pending-review-retry-count 0
                 :exhausted-review-count 0))))
    (let ((check (magnus-doctor--coordination-check "/project/")))
      (should (eq (magnus-doctor-check-severity check) 'warning))
      (should (string-match-p "No watcher"
                              (magnus-doctor-check-detail check))))))

(ert-deftest magnus-doctor-exposes-exhausted-review-evidence ()
  (cl-letf (((symbol-function 'magnus-coord-watched-directories)
             (lambda () '("/project/")))
            ((symbol-function 'magnus-coord-review-retry-diagnostics)
             (lambda (_directory)
               '(:pending-review-retry-count 0
                 :exhausted-review-count 1
                 :exhausted-review-marker-hashes ("marker-1")
                 :exhausted-review-details
                 ((:marker-hash "marker-1"
                   :last-error "manifest unreadable"))))))
    (let ((check (magnus-doctor--coordination-check "/project/")))
      (should (eq (magnus-doctor-check-severity check) 'error))
      (should (string-match-p "manifest unreadable"
                              (magnus-doctor-check-detail check)))
      (should (string-match-p "press g"
                              (magnus-doctor-check-detail check))))))

(provide 'magnus-doctor-tests)
;;; magnus-doctor-tests.el ends here
