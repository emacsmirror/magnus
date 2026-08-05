;;; magnus-review-ui-tests.el --- Review reader tests -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)
(require 'magnus-review-ui)

(defconst magnus-test-review-ui--base-oid
  "1111111111111111111111111111111111111111")

(defconst magnus-test-review-ui--head-oid
  "2222222222222222222222222222222222222222")

(defun magnus-test-review-ui--round (number)
  "Return a map-shaped review round numbered NUMBER."
  (list :number number
        :base_oid magnus-test-review-ui--base-oid
        :head_oid magnus-test-review-ui--head-oid
        :verdict 'comment))

(defun magnus-test-review-ui--patch (&optional patch-path)
  "Return a one-file unified patch for PATCH-PATH."
  (let ((path (or patch-path "lib/sample.el")))
    (format (concat
             "diff --git a/%s b/%s\n"
             "index 1111111..2222222 100644\n"
             "--- a/%s\n"
             "+++ b/%s\n"
             "@@ -1,2 +1,2 @@\n"
             "-old value\n"
             "+new value\n"
             " shared value\n")
            path path path path)))

(ert-deftest magnus-review-ui-normalizes-only-safe-repository-paths ()
  (should (equal (magnus-review-ui--normalize-path "./a/lib.el") "a/lib.el"))
  (should (equal (magnus-review-ui--normalize-path "b/lib.el") "b/lib.el"))
  (dolist (path '(nil "" "/tmp/escape" "../escape" "a/../../escape"
                      "/dev/null" "bad\npath"))
    (should-not (magnus-review-ui--normalize-path path))))

(ert-deftest magnus-review-ui-preserves-real-a-and-b-top-level-directories ()
  (dolist (path '("a/sample.el" "b/sample.el"))
    (let* ((files
            (magnus-review-ui--parse-evidence
             (magnus-test-review-ui--patch path)
             (concat "M\0" path "\0")))
           (file (car files)))
      (should (= (length files) 1))
      (should (equal (magnus-review-ui--file-display-path file) path))
      (should (equal (magnus-review-ui--file-old-path file) path))
      (should (equal (magnus-review-ui--file-new-path file) path)))))

(ert-deftest magnus-review-ui-decodes-c-quoted-unicode-header-paths ()
  (should
   (equal
    (magnus-review-ui--path-from-header
     "+++ \"b/caf\\303\\251.el\"" "+++ ")
    "café.el")))

(ert-deftest magnus-review-ui-parses-and-cross-checks-immutable-evidence ()
  (let* ((files
          (magnus-review-ui--parse-evidence
           (magnus-test-review-ui--patch)
           "M\0lib/sample.el\0"))
         (file (car files))
         (hunk (car (magnus-review-ui--file-hunks file)))
         (lines (magnus-review-ui--hunk-lines hunk)))
    (should (= (length files) 1))
    (should (equal (magnus-review-ui--file-display-path file) "lib/sample.el"))
    (should (equal (magnus-review-ui--file-status file) "M"))
    (should (equal (mapcar #'magnus-review-ui--diff-line-kind lines)
                   '(removed added context)))
    (should (equal (mapcar #'magnus-review-ui--diff-line-old-line lines)
                   '(1 nil 2)))
    (should (equal (mapcar #'magnus-review-ui--diff-line-new-line lines)
                   '(nil 1 2)))))

(ert-deftest magnus-review-ui-rejects-disagreeing-evidence-paths ()
  (should-error
   (magnus-review-ui--parse-evidence
    (magnus-test-review-ui--patch "lib/patch.el")
    "M\0lib/status.el\0")
   :type 'error)
  (should-error
   (magnus-review-ui--parse-name-status "R100\0only-old.el\0")
   :type 'error))

(ert-deftest magnus-review-ui-assigns-inline-file-and-general-findings ()
  (let* ((files
          (magnus-review-ui--parse-evidence
           (magnus-test-review-ui--patch)
           "M\0lib/sample.el\0"))
         (findings
          (magnus-review-ui--normalize-findings
           (list
            '(:id "F-inline" :path "lib/sample.el" :line 1
              :side "head" :title "Inline")
            '(:id "F-file" :path "lib/sample.el" :line 99
              :side "head" :title "File fallback")
            '(:id "F-general" :kind "general" :title "General"))))
         (assigned (magnus-review-ui--assign-findings files findings))
         (inline (plist-get assigned :inline))
         (file-findings (plist-get assigned :file))
         (general (plist-get assigned :general)))
    (should (equal
             (mapcar #'magnus-review-ui--finding-id
                     (gethash '("lib/sample.el" . 1) inline))
             '("F-inline")))
    (should (equal
             (mapcar #'magnus-review-ui--finding-id
                     (gethash "lib/sample.el" file-findings))
             '("F-file")))
    (should (equal (mapcar #'magnus-review-ui--finding-id general)
                   '("F-general")))))

(ert-deftest magnus-review-ui-validates-result-against-immutable-scope ()
  (let ((round (magnus-test-review-ui--round 1)))
    (should-not
     (magnus-review-ui--validate-result-scope
      round
      (list :result
            (list :base_oid magnus-test-review-ui--base-oid
                  :head_oid magnus-test-review-ui--head-oid))))
    (should-error
     (magnus-review-ui--validate-result-scope
      round
      (list :result
            (list :base_oid magnus-test-review-ui--base-oid
                  :head_oid "3333333333333333333333333333333333333333")))
     :type 'error)))

(ert-deftest magnus-review-ui-marks-a-valid-round-read-exactly-once ()
  (with-temp-buffer
    (magnus-review-ui-mode)
    (let* ((round (magnus-test-review-ui--round 1))
           (review (list :id "review" :rounds (list round)))
           calls)
      (setq-local magnus-review-ui--review review)
      (setq-local magnus-review-ui--round round)
      (setq-local magnus-review-ui--result '(:result (:summary "Reviewed")))
      (let ((magnus-review-ui-mark-read-function
             (lambda (candidate candidate-round)
               (push (list candidate candidate-round) calls))))
        (magnus-review-ui--mark-read)
        (magnus-review-ui--mark-read))
      (should (= (length calls) 1))
      (should (equal magnus-review-ui--marked-read-rounds '(1))))))

(ert-deftest magnus-review-ui-keeps-corrupt-result-unread ()
  (with-temp-buffer
    (magnus-review-ui-mode)
    (let ((round (magnus-test-review-ui--round 1))
          called)
      (setq-local magnus-review-ui--round round)
      (setq-local magnus-review-ui--result
                  '(:magnus_review_ui_error "invalid JSON"))
      (let ((magnus-review-ui-mark-read-function
             (lambda (&rest _arguments) (setq called t))))
        (magnus-review-ui--mark-read))
      (should-not called)
      (should-not magnus-review-ui--marked-read-rounds))))

(ert-deftest magnus-review-ui-round-navigation-is-bounded-and-stable ()
  (with-temp-buffer
    (magnus-review-ui-mode)
    (let* ((first (magnus-test-review-ui--round 1))
           (second (magnus-test-review-ui--round 2))
           (review (list :id "review" :rounds (list first second)))
           (refreshed 0))
      (setq-local magnus-review-ui--review review)
      (setq-local magnus-review-ui--round first)
      (cl-letf (((symbol-function 'magnus-review-ui-refresh)
                 (lambda () (cl-incf refreshed))))
        (magnus-review-ui-next-round)
        (should (eq magnus-review-ui--round second))
        (should (= refreshed 1))
        (should-error (magnus-review-ui-next-round) :type 'user-error)
        (magnus-review-ui-previous-round)
        (should (eq magnus-review-ui--round first))
        (should (= refreshed 2))
        (should-error (magnus-review-ui-previous-round) :type 'user-error)))))

(ert-deftest magnus-review-ui-actions-preserve-review-and-round-context ()
  (with-temp-buffer
    (magnus-review-ui-mode)
    (let ((review '(:id "review"))
          (round (magnus-test-review-ui--round 1))
          received)
      (setq-local magnus-review-ui--review review)
      (setq-local magnus-review-ui--round round)
      (let ((magnus-review-ui-action-function
             (lambda (candidate candidate-round)
               (setq received (list candidate candidate-round)))))
        (magnus-review-ui-actions))
      (should (equal received (list review round))))))

(provide 'magnus-review-ui-tests)
;;; magnus-review-ui-tests.el ends here
