;;; magnus-provider-claude.el --- Claude headless provider for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Additive Claude provider capabilities that do not replace Magnus's
;; established Claude vterm implementation.  This adapter builds both strict,
;; schema-constrained reviews and explicitly requested fire-and-forget agents,
;; then normalizes their stream-json events for `magnus-headless'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magnus-provider)

(defvar magnus-claude-executable "claude"
  "Path to the Claude Code executable.")

(defconst magnus-claude-review-tools "Read,Glob,Grep"
  "Claude tools visible to a non-interactive reviewer.")

(defconst magnus-claude-review-allowed-tools
  (mapconcat
   #'identity
   '("Read" "Glob" "Grep")
   ",")
  "Read/search operations preapproved for a Claude reviewer.")

(defvar magnus-claude--uuid-counter 0
  "Local entropy for fresh Claude review session IDs.")

(defun magnus-claude--fresh-session-id ()
  "Return a locally generated UUID v4 string for a fresh review session."
  (let ((hex
         (secure-hash
          'sha256
          (format "%s:%s:%s:%s:%s"
                  (float-time) (emacs-pid) (user-uid)
                  (cl-incf magnus-claude--uuid-counter) (random)))))
    (format "%s-%s-4%s-a%s-%s"
            (substring hex 0 8)
            (substring hex 8 12)
            (substring hex 13 16)
            (substring hex 17 20)
            (substring hex 20 32))))

(defun magnus-claude--option-string (value)
  "Return optional CLI VALUE as a string."
  (cond
   ((null value) nil)
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t (format "%s" value))))

(defun magnus-claude--headless-environment ()
  "Return an environment suitable for a nested Claude headless process."
  (cl-remove-if (lambda (entry) (string-prefix-p "CLAUDECODE=" entry))
                process-environment))

(defun magnus-claude-headless-review-spec (request)
  "Return a Claude headless launch specification for REQUEST."
  (let* ((resumed-session (plist-get request :session-id))
         (session-id (or resumed-session
                         (magnus-claude--fresh-session-id)))
         (schema (plist-get request :schema-json))
         (model (magnus-claude--option-string (plist-get request :model)))
         (effort (magnus-claude--option-string (plist-get request :effort)))
         (evidence-directory (plist-get request :evidence-directory))
         (name (magnus-claude--option-string (plist-get request :name)))
         (prompt (plist-get request :prompt)))
    (unless (and (stringp schema) (not (string-empty-p schema)))
      (user-error "Claude headless reviews require a JSON schema"))
    (when (and resumed-session
               (not (and (stringp resumed-session)
                         (not (string-empty-p resumed-session)))))
      (user-error "Claude review session ID is invalid"))
    (list
     :command
     (append
      (list magnus-claude-executable
            "--safe-mode"
            "--print"
            "--verbose"
            "--output-format" "stream-json"
            "--permission-mode" "dontAsk"
            "--tools" magnus-claude-review-tools
            "--allowedTools" magnus-claude-review-allowed-tools
            "--json-schema" schema)
      (when model (list "--model" model))
      (when effort (list "--effort" effort))
      (when name (list "--name" name))
      (when evidence-directory
        (list "--add-dir" (expand-file-name evidence-directory)))
      (if resumed-session
          (list "--resume" resumed-session)
        (list "--session-id" session-id))
      (list prompt))
     :environment
     (magnus-claude--headless-environment)
     :decoder #'magnus-claude-headless-decode-event
     :success-requires '(terminal structured-result)
     ;; A fresh UUID is only a candidate until Claude emits it in stream-json.
     ;; Persisting it before that point makes an early CLI/auth failure poison
     ;; every retry with --resume of a session that never existed.
     :session-id resumed-session
     :candidate-session-id (unless resumed-session session-id)
     :name (and name (format "magnus-claude-review-%s" name)))))

(defun magnus-claude-headless-agent-spec (request)
  "Return a Claude fire-and-forget agent specification for REQUEST."
  (let ((allowed-tools (plist-get request :allowed-tools))
        (model (magnus-claude--option-string (plist-get request :model)))
        (name (magnus-claude--option-string (plist-get request :name)))
        (prompt (plist-get request :prompt)))
    (unless (and (plist-member request :allowed-tools)
                 (stringp allowed-tools))
      (user-error "Claude headless agents require configured allowed tools"))
    (list
     :command
     (append
      (list magnus-claude-executable
            "--print" prompt
            "--verbose"
            "--output-format" "stream-json")
      (when model (list "--model" model))
      ;; An explicitly empty tool set is the narrow capability needed by
      ;; background summarizers.  Omitting the field remains an error, so a
      ;; caller can never gain the CLI's default tools by accident.
      (if (string-empty-p allowed-tools)
          (list "--tools" "")
        (list "--allowedTools" allowed-tools)))
     :environment (magnus-claude--headless-environment)
     :decoder #'magnus-claude-headless-decode-event
     :success-requires '(terminal)
     :name (and name (format "magnus-claude-agent-%s" name)))))

(defun magnus-claude-headless-spec (request)
  "Return a Claude headless launch specification for REQUEST's purpose."
  (pcase (plist-get request :purpose)
    ('review (magnus-claude-headless-review-spec request))
    ('agent (magnus-claude-headless-agent-spec request))
    (purpose (user-error "Claude does not support headless purpose `%s'"
                         purpose))))

(defun magnus-claude--parse-structured-result (text)
  "Parse Claude structured result TEXT into alists and lists."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun magnus-claude--assistant-text (event)
  "Return concatenated visible assistant text from stream-json EVENT."
  (let* ((message (alist-get 'message event))
         (content (and (listp message) (alist-get 'content message)))
         (blocks (cond
                  ((vectorp content) (append content nil))
                  ((listp content) content))))
    (mapconcat
     #'identity
     (delq nil
           (mapcar
            (lambda (block)
              (when (and (listp block)
                         (equal (alist-get 'type block) "text")
                         (stringp (alist-get 'text block)))
                (alist-get 'text block)))
            blocks))
     "")))

(defun magnus-claude-headless-decode-event (event request)
  "Normalize one Claude stream-json EVENT for REQUEST."
  (let* ((type (alist-get 'type event))
         (subtype (alist-get 'subtype event))
         (session-entry (assq 'session_id event))
         (structured-entry (assq 'structured_output event))
         (result-text (alist-get 'result event))
         (assistant-text (and (equal type "assistant")
                              (magnus-claude--assistant-text event)))
         (cost-usd (alist-get 'cost_usd event))
         (canonical (list :type (or type "unknown")
                          :provider 'claude
                          :raw event)))
    (when (and (stringp assistant-text)
               (not (string-empty-p assistant-text)))
      (setq canonical (plist-put canonical :text assistant-text)))
    (when (numberp cost-usd)
      (setq canonical (plist-put canonical :cost-usd cost-usd)))
    (when session-entry
      (setq canonical
            (plist-put canonical :session-id (cdr session-entry))))
    (when structured-entry
      (setq canonical
            (plist-put canonical :structured-result
                       (cdr structured-entry))))
    ;; Current Claude builds expose schema output as `structured_output'.  The
    ;; fallback keeps older compatible builds useful while treating malformed
    ;; schema output as an explicit decode failure.
    (when (and (equal type "result")
               (plist-get request :schema-json)
               (not structured-entry)
               (stringp result-text))
      (condition-case err
          (setq canonical
                (plist-put canonical :structured-result
                           (magnus-claude--parse-structured-result result-text)))
        (error
         (setq canonical
               (plist-put canonical :decode-error
                          (format "Claude result is not schema JSON: %s"
                                  (error-message-string err)))))))
    (when (equal type "result")
      (setq canonical (plist-put canonical :terminal t))
      (when (or (alist-get 'is_error event)
                (and subtype (not (equal subtype "success"))))
        (setq canonical
              (plist-put
               canonical :error
               (list :subtype subtype
                     :message (or result-text
                                  (format "Claude result ended as %s"
                                          (or subtype "an error"))))))))
    canonical))

(magnus-provider-register
 'claude
 '((headless-spec . magnus-claude-headless-spec)))

(provide 'magnus-provider-claude)
;;; magnus-provider-claude.el ends here
