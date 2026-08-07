;;; magnus-trace-cursor-tests.el --- Exact trace cursor tests -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'json)
(require 'magnus-instances)
(require 'magnus-trace)

(defun magnus-trace-cursor-test--claude-line (type text)
  "Encode one canonical Claude trace record of TYPE containing TEXT."
  (json-encode
   (if (equal type "assistant")
       `((type . "assistant")
         (message . ((content . [((type . "text") (text . ,text))]))))
     `((type . ,type) (message . ((content . ,text)))))))

(ert-deftest magnus-trace-cursor-starts-at-current-eof ()
  (let* ((file (make-temp-file "magnus-trace-cursor-eof-"))
         (instance (magnus-instances-create "/tmp" "cursor-eof" 'claude)))
    (unwind-protect
        (progn
          (magnus-instances-update instance :session-id "session-eof")
          (with-temp-file file
            (insert (magnus-trace-cursor-test--claude-line
                     "assistant" "before-cursor")
                    "\n"))
          (cl-letf (((symbol-function 'magnus-trace-resolve-file)
                     (lambda (_instance) file)))
            (let ((cursor (magnus-trace-cursor-create instance)))
              (should-not (magnus-trace-cursor-read cursor))
              (write-region
               (concat
                (magnus-trace-cursor-test--claude-line
                 "assistant" "after-cursor")
                "\n")
               nil file t 'silent)
              (should (equal (magnus-trace-cursor-read cursor)
                             '("after-cursor")))
              (should-not (magnus-trace-cursor-read cursor)))))
      (delete-file file))))

(ert-deftest magnus-trace-cursor-retains-partial-new-record ()
  (let* ((file (make-temp-file "magnus-trace-cursor-partial-"))
         (instance
          (magnus-instances-create "/tmp" "cursor-partial" 'claude))
         (line (magnus-trace-cursor-test--claude-line
                "assistant" "completed-after-split"))
         (split (/ (length line) 2)))
    (unwind-protect
        (progn
          (magnus-instances-update instance :session-id "session-partial")
          (cl-letf (((symbol-function 'magnus-trace-resolve-file)
                     (lambda (_instance) file)))
            (let ((cursor (magnus-trace-cursor-create instance)))
              (write-region (substring line 0 split) nil file t 'silent)
              (should-not (magnus-trace-cursor-read cursor))
              (should-not
               (string-empty-p (magnus-trace-cursor-pending cursor)))
              (write-region (concat (substring line split) "\n")
                            nil file t 'silent)
              (should (equal (magnus-trace-cursor-read cursor)
                             '("completed-after-split")))
              (should (string-empty-p
                       (magnus-trace-cursor-pending cursor))))))
      (delete-file file))))

(ert-deftest magnus-trace-cursor-returns-only-visible-assistant-text ()
  (let* ((file (make-temp-file "magnus-trace-cursor-visible-"))
         (instance
          (magnus-instances-create "/tmp" "cursor-visible" 'claude)))
    (unwind-protect
        (progn
          (magnus-instances-update instance :session-id "session-visible")
          (cl-letf (((symbol-function 'magnus-trace-resolve-file)
                     (lambda (_instance) file)))
            (let ((cursor (magnus-trace-cursor-create instance)))
              (write-region
               (concat
                (magnus-trace-cursor-test--claude-line "user" "question")
                "\n"
                (json-encode
                 '((type . "assistant")
                   (message
                    . ((content
                        . [((type . "thinking") (thinking . "private"))
                           ((type . "text") (text . "public"))])))))
                "\n")
               nil file t 'silent)
              (should (equal (magnus-trace-cursor-read cursor)
                             '("public"))))))
      (delete-file file))))

(ert-deftest magnus-trace-cursor-unwraps-marked-nonce-response ()
  (let* ((file (make-temp-file "magnus-trace-cursor-nonce-"))
         (instance
          (magnus-instances-create "/tmp" "cursor-nonce" 'claude))
         (nonce "review-scope-9f2a"))
    (unwind-protect
        (progn
          (magnus-instances-update instance :session-id "session-nonce")
          (cl-letf (((symbol-function 'magnus-trace-resolve-file)
                     (lambda (_instance) file)))
            (let ((cursor (magnus-trace-cursor-create instance)))
              (write-region
               (concat
                (magnus-trace-cursor-test--claude-line
                 "assistant"
                 (format
                  (concat "[thinking]\ninternal\n[end-thinking]\n"
                          "[response]\n[MAGNUS-REVIEW-SCOPE request=%s]"
                          "\n[end-response]")
                  nonce))
                "\n")
               nil file t 'silent)
              (should
               (equal
                (magnus-trace-cursor-read cursor)
                (list (format "[MAGNUS-REVIEW-SCOPE request=%s]" nonce)))))))
      (delete-file file))))

(ert-deftest magnus-trace-cursor-rejects-session-replacement ()
  (let* ((file (make-temp-file "magnus-trace-cursor-session-"))
         (instance
          (magnus-instances-create "/tmp" "cursor-session" 'claude)))
    (unwind-protect
        (progn
          (magnus-instances-update instance :session-id "original-session")
          (cl-letf (((symbol-function 'magnus-trace-resolve-file)
                     (lambda (_instance) file)))
            (let ((cursor (magnus-trace-cursor-create instance)))
              (magnus-instances-update instance :session-id "new-session")
              (should-error (magnus-trace-cursor-read cursor)
                            :type 'magnus-trace-cursor-stale))))
      (delete-file file))))

(ert-deftest magnus-trace-cursor-rejects-file-replacement ()
  (let* ((first (make-temp-file "magnus-trace-cursor-first-"))
         (second (make-temp-file "magnus-trace-cursor-second-"))
         (current first)
         (instance
          (magnus-instances-create "/tmp" "cursor-file" 'claude)))
    (unwind-protect
        (progn
          (magnus-instances-update instance :session-id "same-session")
          (cl-letf (((symbol-function 'magnus-trace-resolve-file)
                     (lambda (_instance) current)))
            (let ((cursor (magnus-trace-cursor-create instance)))
              (setq current second)
              (should-error (magnus-trace-cursor-read cursor)
                            :type 'magnus-trace-cursor-stale))))
      (delete-file first)
      (delete-file second))))

(ert-deftest magnus-trace-file-identity-uses-emacs-28-accessors ()
  (let* ((file (make-temp-file "magnus-trace-file-identity-"))
         (attributes (file-attributes file)))
    (unwind-protect
        (should
         (equal (magnus-trace--file-identity attributes)
                (cons (file-attribute-device-number attributes)
                      (file-attribute-inode-number attributes))))
      (delete-file file))))

(ert-deftest magnus-trace-load-earlier-skips-oversized-placeholder ()
  "A nil scanner placeholder never reaches the JSON renderer."
  (with-temp-buffer
    (setq magnus-trace--jsonl-file "/tmp/ignored.jsonl"
          magnus-trace--skip-count 2
          magnus-trace--page-start-offset 10
          magnus-trace--rendered-count 0)
    (let ((magnus-trace-max-initial-entries 10)
          (magnus-trace-max-buffer-lines nil)
          rendered)
      (cl-letf (((symbol-function 'magnus-trace--read-previous-records)
                 (lambda (&rest _arguments)
                   '(:lines (nil "valid") :count 2 :start 0)))
                ((symbol-function 'magnus-trace--render-json-line)
                 (lambda (line)
                   (push line rendered)
                   t))
                ((symbol-function 'message) #'ignore))
        (magnus-trace-load-earlier))
      (should (equal rendered '("valid")))
      (should (zerop magnus-trace--skip-count))
      (should (= magnus-trace--rendered-count 1)))))

(provide 'magnus-trace-cursor-tests)
;;; magnus-trace-cursor-tests.el ends here
