;;; magnus-headless.el --- Provider-neutral JSONL subprocesses -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A narrow asynchronous runner for resumable, non-interactive provider work.
;; It deliberately does not create a `magnus-instance', own review attempts, or
;; persist lifecycle state.  Its caller owns those concerns and may reject a
;; late callback from an obsolete attempt.
;;
;; Start a process with:
;;
;;   (magnus-headless-start PROVIDER REQUEST CALLBACKS)
;;
;; REQUEST is a plist containing:
;;
;;   :directory   Existing working directory (required)
;;   :prompt      Provider prompt (required)
;;   :purpose     `review' (the default) or `agent'
;;   :session-id  Session to resume, or nil for a fresh session
;;   :model       Optional provider model name
;;   :effort      Optional provider effort symbol or string
;;   :schema      Optional JSON Schema as JSON text or an encodable Lisp value
;;   :schema-file Optional existing JSON Schema file
;;   :buffer      Optional live display buffer to associate with the process
;;   :environment-bindings Optional NAME=VALUE bindings over the provider's
;;                         filtered launch environment
;;
;; Provider adapters may define additional keys.  Review adapters currently use
;; :base, :head, :title, and :name.  When :schema is supplied without
;; :schema-file, the runner creates a private temporary schema file and removes
;; it after completion.
;;
;; Headless commands receive their prompt through REQUEST/provider argv and do
;; not receive streaming stdin.  The runner closes the subprocess input pipe
;; after launch so CLIs that inspect piped stdin cannot wait forever for EOF.
;;
;; A provider launch spec may return :candidate-session-id for a locally chosen
;; fresh ID.  Unlike :session-id, a candidate is diagnostic only and never
;; triggers :on-session until the provider confirms it in its event stream.
;; Specs may also return :success-requires, a non-empty list containing
;; `terminal' and/or `structured-result'.  The strict default requires both;
;; review work may never weaken that default.
;;
;; CALLBACKS is a plist of optional functions:
;;
;;   :on-raw-event (PROCESS LINE)
;;   :on-event     (PROCESS CANONICAL-EVENT)
;;   :on-session   (PROCESS SESSION-ID)
;;   :on-stderr    (PROCESS CHUNK)
;;   :on-error     (PROCESS ERROR-PLIST)
;;   :on-complete  (PROCESS RESULT-PLIST)
;;
;; Raw complete JSONL lines are delivered before parsing, so callers can persist
;; malformed provider evidence.  Records beyond `magnus-headless-jsonl-line-limit'
;; are instead discarded with an explicit error.  All callbacks occur from
;; process filters, sentinels, or a zero-delay timer after this function returns.
;; Provider and callback failures are reported explicitly rather than aborting
;; the remaining JSONL stream.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magnus-environment)
(require 'magnus-provider)

(defcustom magnus-headless-stderr-limit (* 256 1024)
  "Maximum trailing stderr characters retained in a headless result.
The complete stream remains available through the :on-stderr callback."
  :type 'integer
  :group 'magnus)

(defcustom magnus-headless-jsonl-line-limit (* 4 1024 1024)
  "Maximum characters retained for one headless JSONL record.
This bounds both a complete record and stdout waiting for its terminating
newline.  An overlong record is discarded through its next newline and becomes
an explicit `jsonl-line-too-long' decode error; later records are still read."
  :type 'integer
  :group 'magnus)

(defcustom magnus-headless-error-limit 64
  "Maximum headless errors retained in one completion result.
The limit is shared by decode, provider, and callback errors.  Every error is
still delivered to :on-error, while the result reports how many were omitted."
  :type 'integer
  :group 'magnus)

(defcustom magnus-headless-error-detail-limit (* 16 1024)
  "Maximum characters retained for one headless error detail value."
  :type 'integer
  :group 'magnus)

(defvar magnus-headless--process-counter 0
  "Counter used to make provider subprocess names distinct.")

(defconst magnus-headless--purposes '(review agent)
  "Purposes accepted by the generic headless runner.")

(defconst magnus-headless--success-requirements
  '(terminal structured-result)
  "Canonical facts a provider spec may require for success.")

(defconst magnus-headless--default-success-requires
  '(terminal structured-result)
  "Success requirements used when a provider spec does not override them.")

(defconst magnus-headless--callback-keys
  '(:on-raw-event :on-event :on-session :on-stderr :on-error :on-complete)
  "Recognized keys in a headless CALLBACKS plist.")

(defun magnus-headless--bounded-error-value (value)
  "Return diagnostic VALUE in a bounded, printable form."
  (let ((limit (max 0 magnus-headless-error-detail-limit)))
    (cond
     ((stringp value)
      (cond
       ((zerop limit) "")
       ((> (length value) limit)
        (concat (substring value 0 limit) "…"))
       (t value)))
     ((or (null value) (numberp value) (symbolp value)) value)
     (t
      ;; Error details are diagnostic only.  Keeping an arbitrary provider
      ;; object here could retain an entire oversized decoded event.
      (let* ((print-circle t)
             (print-length 50)
             (print-level 8)
             (printed (prin1-to-string value)))
        (cond
         ((zerop limit) "")
         ((> (length printed) limit)
          (concat (substring printed 0 limit) "…"))
         (t printed)))))))

(defun magnus-headless--bounded-error-data (data)
  "Return error plist DATA with every value bounded."
  (let (bounded)
    (while data
      (let ((key (pop data))
            (value (pop data)))
        (setq bounded
              (append bounded
                      (list key
                            (magnus-headless--bounded-error-value value))))))
    bounded))

(defun magnus-headless--error-result-key (property)
  "Return public result key corresponding to error PROPERTY."
  (pcase property
    ('magnus-headless-decode-errors :decode-errors)
    ('magnus-headless-provider-errors :provider-errors)
    ('magnus-headless-callback-errors :callback-errors)
    (_ :other-errors)))

(defun magnus-headless--store-error (process property failure)
  "Retain bounded FAILURE under PROCESS PROPERTY, or count its omission."
  (let ((count (or (process-get process 'magnus-headless-error-count) 0))
        (limit (max 0 magnus-headless-error-limit)))
    (process-put process 'magnus-headless-error-count (1+ count))
    (if (< count limit)
        (process-put process property
                     (cons failure (process-get process property)))
      (let* ((key (magnus-headless--error-result-key property))
             (dropped
              (copy-sequence
               (or (process-get process 'magnus-headless-dropped-errors) nil))))
        (setq dropped
              (plist-put dropped key (1+ (or (plist-get dropped key) 0))))
        (process-put process 'magnus-headless-dropped-errors dropped)))))

(defun magnus-headless--validate-request (provider request)
  "Validate PROVIDER and common fields in REQUEST."
  (unless (symbolp provider)
    (signal 'wrong-type-argument (list 'symbolp provider)))
  (unless (listp request)
    (signal 'wrong-type-argument (list 'listp request)))
  (let ((directory (plist-get request :directory))
        (prompt (plist-get request :prompt))
        (purpose (or (plist-get request :purpose) 'review))
        (buffer (plist-get request :buffer)))
    (unless (and (stringp directory) (file-directory-p directory))
      (user-error "Headless directory does not exist: %s" directory))
    (unless (and (stringp prompt) (not (string-empty-p prompt)))
      (user-error "A non-empty headless prompt is required"))
    (unless (memq purpose magnus-headless--purposes)
      (user-error "Unknown headless purpose: %s" purpose))
    (magnus-environment-validate-bindings
     (plist-get request :environment-bindings))
    (when (and buffer (not (buffer-live-p buffer)))
      (user-error "Headless display buffer is not live"))))

(defun magnus-headless--validate-callbacks (callbacks)
  "Validate functions in CALLBACKS."
  (unless (listp callbacks)
    (signal 'wrong-type-argument (list 'listp callbacks)))
  (dolist (key magnus-headless--callback-keys)
    (when-let ((function (plist-get callbacks key)))
      (unless (functionp function)
        (user-error "Headless callback %s is not callable" key)))))

(defun magnus-headless--schema-json (schema)
  "Return SCHEMA as JSON text."
  (cond
   ((null schema) nil)
   ((stringp schema) schema)
   (t (json-encode schema))))

(defun magnus-headless--read-file (file)
  "Read FILE literally and return its contents."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun magnus-headless--prepare-request (request)
  "Return REQUEST enriched with normalized schema fields.
The returned plist contains :owned-schema-file when this function created the
file and the caller is responsible for deleting it."
  (let* ((prepared (plist-put (copy-sequence request) :purpose
                              (or (plist-get request :purpose) 'review)))
         (schema-file (plist-get prepared :schema-file))
         (schema-json (magnus-headless--schema-json
                       (plist-get prepared :schema))))
    (when schema-file
      (unless (and (stringp schema-file) (file-readable-p schema-file))
        (user-error "Headless schema file is not readable: %s" schema-file))
      (unless schema-json
        (setq schema-json (magnus-headless--read-file schema-file))))
    (when (and schema-json (not schema-file))
      (setq schema-file
            (make-temp-file "magnus-headless-schema-" nil ".json" schema-json))
      (setq prepared (plist-put prepared :owned-schema-file schema-file)))
    (when schema-json
      (setq prepared (plist-put prepared :schema-json schema-json)))
    (when schema-file
      (setq prepared (plist-put prepared :schema-file
                                (expand-file-name schema-file))))
    prepared))

(defun magnus-headless--delete-owned-schema (request)
  "Delete the temporary schema owned by REQUEST, if any."
  (when-let ((file (plist-get request :owned-schema-file)))
    (when (file-exists-p file)
      (delete-file file))))

(defun magnus-headless--callback (process key &rest arguments)
  "Invoke PROCESS callback KEY with ARGUMENTS without breaking its stream."
  (when-let ((function (plist-get (process-get process 'magnus-headless-callbacks)
                                  key)))
    (condition-case err
        (apply function (cons process arguments))
      (error
       (let ((failure
              (list :kind 'callback-error
                    :callback key
                    :message
                    (magnus-headless--bounded-error-value
                     (error-message-string err))
                    :error (magnus-headless--bounded-error-value err))))
         (magnus-headless--store-error
          process 'magnus-headless-callback-errors failure)
         (message "Magnus: headless callback %s failed: %s"
                  key (error-message-string err))
         (unless (eq key :on-error)
           (magnus-headless--callback process :on-error failure)))))))

(defun magnus-headless--record-error (process property kind message &rest data)
  "Record a PROCESS error under PROPERTY with KIND, MESSAGE, and DATA."
  (let ((failure
         (append
          (list :kind kind
                :message (magnus-headless--bounded-error-value message))
          (magnus-headless--bounded-error-data data))))
    (magnus-headless--store-error process property failure)
    (magnus-headless--callback process :on-error failure)
    failure))

(defun magnus-headless--json-value (text)
  "Parse one JSON value from TEXT into alists and lists."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun magnus-headless--notify-session (process session-id)
  "Record and notify PROCESS of SESSION-ID exactly once per distinct ID."
  (if (not (and (stringp session-id) (not (string-empty-p session-id))))
      (magnus-headless--record-error
       process 'magnus-headless-decode-errors 'session-decode-error
       "Provider emitted an invalid session ID" :value session-id)
    (let ((previous (process-get process 'magnus-headless-session-id))
          (notified (process-get process 'magnus-headless-session-notified-p)))
      (process-put process 'magnus-headless-session-id session-id)
      (unless (and notified (equal previous session-id))
        (process-put process 'magnus-headless-session-notified-p t)
        (magnus-headless--callback process :on-session session-id)))))

(defun magnus-headless--notify-initial-session (process)
  "Asynchronously notify the preassigned or resumed session for PROCESS."
  (when-let ((session-id (process-get process 'magnus-headless-session-id)))
    (magnus-headless--notify-session process session-id)))

(defun magnus-headless--handle-canonical-event (process canonical)
  "Apply CANONICAL provider event to PROCESS aggregate state."
  (unless (and (listp canonical) (plist-member canonical :type))
    (error "Provider decoder returned no canonical event"))
  (when (plist-member canonical :session-id)
    (magnus-headless--notify-session
     process (plist-get canonical :session-id)))
  (when (plist-member canonical :structured-result)
    (process-put process 'magnus-headless-structured-result
                 (plist-get canonical :structured-result))
    (process-put process 'magnus-headless-structured-result-p t))
  (when-let ((decode-error (plist-get canonical :decode-error)))
    (magnus-headless--record-error
     process 'magnus-headless-decode-errors 'provider-decode-error
     (if (stringp decode-error)
         decode-error
       "Provider could not decode its structured result")
     :detail decode-error))
  (when-let ((provider-error (plist-get canonical :error)))
    (magnus-headless--record-error
     process 'magnus-headless-provider-errors 'provider-error
     (or (and (listp provider-error) (plist-get provider-error :message))
         (and (stringp provider-error) provider-error)
         "Provider reported an error")
     :detail provider-error))
  (when (plist-get canonical :terminal)
    (process-put process 'magnus-headless-terminal-event canonical))
  (magnus-headless--callback process :on-event canonical))

(defun magnus-headless--consume-line (process line)
  "Persist and decode one complete JSONL LINE from PROCESS."
  (unless (string-empty-p (string-trim line))
    (setq line (string-remove-suffix "\r" line))
    ;; Persistence runs before parsing by design: malformed provider evidence is
    ;; still available for diagnosis and retry decisions.
    (magnus-headless--callback process :on-raw-event line)
    (condition-case err
        (let* ((event (magnus-headless--json-value line))
               (decoder (process-get process 'magnus-headless-decoder))
               (request (process-get process 'magnus-headless-request))
               (canonical (funcall decoder event request)))
          (magnus-headless--handle-canonical-event process canonical))
      (error
       (magnus-headless--record-error
        process 'magnus-headless-decode-errors 'jsonl-decode-error
        (error-message-string err) :line line :error err)))))

(defun magnus-headless--filter (process output)
  "Consume bounded JSONL OUTPUT from PROCESS.
An overlong record is discarded through its next newline without preventing
subsequent records in OUTPUT from being decoded."
  (let* ((segments (split-string output "\n" nil))
         (last-index (1- (length segments)))
         (partial (or (process-get process 'magnus-headless-partial-line) ""))
         (discarding
          (process-get process 'magnus-headless-discarding-line-p))
         (limit (max 0 magnus-headless-jsonl-line-limit)))
    (cl-loop
     for segment in segments
     for index from 0
     for final-p = (= index last-index)
     do
     (if discarding
         ;; A non-final segment is followed by a newline, which terminates the
         ;; record currently being discarded.  The next segment starts clean.
         (unless final-p
           (setq discarding nil
                 partial ""))
       (let ((combined-length (+ (length partial) (length segment))))
         (cond
          ((> combined-length limit)
           (process-put
            process 'magnus-headless-discarded-jsonl-lines
            (1+ (or (process-get
                     process 'magnus-headless-discarded-jsonl-lines)
                    0)))
           (magnus-headless--record-error
            process 'magnus-headless-decode-errors 'jsonl-line-too-long
            (format "Provider JSONL record exceeded %d characters" limit)
            :limit limit :observed-at-least combined-length)
           (setq partial ""
                 ;; Without a newline this same record continues in the next
                 ;; filter invocation and must be dropped without another error.
                 discarding final-p))
          (t
           (let ((combined (concat partial segment)))
             (if final-p
                 (setq partial combined)
               (setq partial "")
               (magnus-headless--consume-line process combined))))))))
    (process-put process 'magnus-headless-partial-line partial)
    (process-put process 'magnus-headless-discarding-line-p discarding)))

(defun magnus-headless--stderr-filter (stderr-process output)
  "Deliver and retain OUTPUT from STDERR-PROCESS."
  (when-let ((process (process-get stderr-process 'magnus-headless-process)))
    (let* ((existing (or (process-get process 'magnus-headless-stderr) ""))
           (combined (concat existing output))
           (length (length combined))
           (limit (max 0 magnus-headless-stderr-limit)))
      (when (> length limit)
        (process-put
         process 'magnus-headless-stderr-dropped-chars
         (+ (- length limit)
            (or (process-get
                 process 'magnus-headless-stderr-dropped-chars)
                0)))
        (setq combined (substring combined
                                  (- length limit))))
      (process-put process 'magnus-headless-stderr combined)
      (magnus-headless--callback process :on-stderr output))))

(defun magnus-headless--result (process)
  "Build the canonical completion result for PROCESS."
  (let* ((status (process-status process))
         (exit-status (process-exit-status process))
         (terminal (process-get process 'magnus-headless-terminal-event))
         (structured-p
          (process-get process 'magnus-headless-structured-result-p))
         (success-requires
          (or (process-get process 'magnus-headless-success-requires)
              magnus-headless--default-success-requires))
         (decode-errors
          (nreverse (process-get process 'magnus-headless-decode-errors)))
         (provider-errors
          (nreverse (process-get process 'magnus-headless-provider-errors)))
         (callback-errors
          (nreverse (process-get process 'magnus-headless-callback-errors)))
         (dropped-errors
          (process-get process 'magnus-headless-dropped-errors))
         (stderr-dropped
          (or (process-get process 'magnus-headless-stderr-dropped-chars) 0)))
    (list
     :provider (process-get process 'magnus-headless-provider)
     :success-p (and (eq status 'exit)
                     (zerop exit-status)
                     (cl-every
                      (lambda (requirement)
                        (pcase requirement
                          ('terminal terminal)
                          ('structured-result structured-p)))
                      success-requires)
                     (zerop
                      (or (process-get process 'magnus-headless-error-count) 0)))
     :status status
     :exit-status exit-status
     :process-event (process-get process 'magnus-headless-process-event)
     :session-id (process-get process 'magnus-headless-session-id)
     :candidate-session-id
     (process-get process 'magnus-headless-candidate-session-id)
     :success-requires success-requires
     :structured-result-present-p structured-p
     :structured-result
     (process-get process 'magnus-headless-structured-result)
     :terminal-event terminal
     :stderr (or (process-get process 'magnus-headless-stderr) "")
     :stderr-truncated-p (> stderr-dropped 0)
     :stderr-dropped-chars stderr-dropped
     :jsonl-line-limit (max 0 magnus-headless-jsonl-line-limit)
     :discarded-jsonl-lines
     (or (process-get process 'magnus-headless-discarded-jsonl-lines) 0)
     :errors-truncated-p (not (null dropped-errors))
     :dropped-errors dropped-errors
     :decode-errors decode-errors
     :provider-errors provider-errors
     :callback-errors callback-errors)))

(defun magnus-headless--drain-terminal-output (process)
  "Give PROCESS and its stderr pipe one bounded final drain.
A subprocess state change can become visible before Emacs has dispatched the
last readable pipe bytes to its filters.  Final result construction must happen
after that tail or it can publish a completion missing an earlier session or
structured-result event."
  (dolist (candidate
           (delq nil
                 (list process
                       (process-get process
                                    'magnus-headless-stderr-process))))
    (condition-case err
        ;; Closed connections return immediately; 50ms is only a ceiling for a
        ;; stderr pipe whose EOF notification is a fraction behind its parent.
        ;; Integer JUST-THIS-ONE also suppresses unrelated timer callbacks while
        ;; finalization is deliberately dispatching these last process filters.
        (accept-process-output candidate 0.05 nil 1)
      (error
       (message "Magnus: final headless output drain failed: %s"
                (error-message-string err))))))

(defun magnus-headless--finalize (process)
  "Flush, report, and clean up completed PROCESS."
  (unless (process-get process 'magnus-headless-completed-p)
    (magnus-headless--drain-terminal-output process)
    (process-put process 'magnus-headless-completed-p t)
    (unless (process-get process 'magnus-headless-discarding-line-p)
      (when-let ((partial (process-get process 'magnus-headless-partial-line)))
        (unless (string-empty-p partial)
          (process-put process 'magnus-headless-partial-line "")
          (magnus-headless--consume-line process partial))))
    (process-put process 'magnus-headless-discarding-line-p nil)
    (let ((request (process-get process 'magnus-headless-request)))
      (unwind-protect
          (magnus-headless--callback
           process :on-complete (magnus-headless--result process))
        (magnus-headless--delete-owned-schema request)
        (when-let ((stderr-process
                    (process-get process 'magnus-headless-stderr-process)))
          (when (process-live-p stderr-process)
            (delete-process stderr-process)))))))

(defun magnus-headless--sentinel (process event)
  "Schedule finalization of PROCESS after terminal EVENT."
  (when (memq (process-status process) '(exit signal failed closed))
    (process-put process 'magnus-headless-process-event (string-trim event))
    (unless (process-get process 'magnus-headless-finalizer)
      ;; Give the separate stderr pipe one event-loop turn to deliver its tail.
      (process-put process 'magnus-headless-finalizer
                   (run-at-time 0 nil #'magnus-headless--finalize process)))))

(defun magnus-headless--provider-spec (provider request)
  "Return PROVIDER's launch specification for REQUEST.
Generic adapters are preferred.  The former review-only operation remains a
compatibility fallback exclusively for review requests."
  (let ((purpose (plist-get request :purpose)))
    (cond
     ((magnus-provider-symbol-operation-p provider 'headless-spec)
      (magnus-provider-call-symbol provider 'headless-spec request))
     ((and (eq purpose 'review)
           (magnus-provider-symbol-operation-p
            provider 'headless-review-spec))
      (magnus-provider-call-symbol provider 'headless-review-spec request))
     ((eq purpose 'review)
      (user-error "Provider `%s' does not support headless reviews" provider))
     (t
      (user-error "Provider `%s' does not support headless agent work"
                  provider)))))

(defun magnus-headless--validate-spec (provider spec purpose)
  "Validate PROVIDER launch SPEC for PURPOSE and return it."
  (let* ((command (plist-get spec :command))
         (decoder (plist-get spec :decoder))
         (success-requires
          (if (plist-member spec :success-requires)
              (plist-get spec :success-requires)
            magnus-headless--default-success-requires)))
    (unless (and (consp command) (cl-every #'stringp command))
      (error "Provider `%s' returned an invalid headless command" provider))
    (unless (functionp decoder)
      (error "Provider `%s' returned no headless event decoder" provider))
    (unless (and (consp success-requires)
                 (cl-every
                  (lambda (requirement)
                    (memq requirement
                          magnus-headless--success-requirements))
                  success-requires))
      (error "Provider `%s' returned invalid success requirements" provider))
    (when (and (eq purpose 'review)
               (not (and (memq 'terminal success-requires)
                         (memq 'structured-result success-requires))))
      (error "Provider `%s' weakened headless review completion" provider))
    (plist-put (copy-sequence spec) :success-requires success-requires)))

;;;###autoload
(defun magnus-headless-start (provider request &optional callbacks)
  "Asynchronously launch PROVIDER for REQUEST with CALLBACKS.
Return the subprocess immediately.  See this library's Commentary for the
request, callback, and completion contracts.  Attempt ownership deliberately
remains with the caller."
  (magnus-headless--validate-request provider request)
  (magnus-headless--validate-callbacks callbacks)
  (let* ((prepared (magnus-headless--prepare-request request))
         (spec nil)
         (stderr-process nil)
         (process nil)
         (active nil)
         (pending-output nil)
         (pending-stderr nil)
         (pending-event nil))
    (condition-case err
        (progn
          (setq spec
                (magnus-headless--validate-spec
                 provider
                 (magnus-headless--provider-spec provider prepared)
                 (plist-get prepared :purpose)))
          (let* ((directory (file-name-as-directory
                             (expand-file-name
                              (plist-get prepared :directory))))
                 (default-directory directory)
                 ;; Provider filtering is authoritative.  Apply request-local
                 ;; identity only after it, and never mutate either source.
                 (process-environment
                  (magnus-environment-overlay
                   (if (plist-member spec :environment)
                       (plist-get spec :environment)
                     process-environment)
                   (plist-get prepared :environment-bindings)))
                 (name (or (plist-get spec :name)
                           (format "magnus-headless-%s-%d"
                                   provider
                                   (cl-incf magnus-headless--process-counter)))))
            (setq stderr-process
                  (make-pipe-process
                   :name (generate-new-buffer-name (concat name "-stderr"))
                   :buffer nil
                   :noquery t
                   :coding 'utf-8-unix
                   :filter
                   (lambda (stderr output)
                     (if active
                         (magnus-headless--stderr-filter stderr output)
                       (push output pending-stderr)))))
            (setq process
                  (make-process
                   :name (generate-new-buffer-name name)
                   :buffer (plist-get prepared :buffer)
                   :command (plist-get spec :command)
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t
                   :stderr stderr-process
                   :filter
                   (lambda (child output)
                     (if active
                         (magnus-headless--filter child output)
                       (push output pending-output)))
                   :sentinel
                   (lambda (child event)
                     (if active
                         (magnus-headless--sentinel child event)
                       (setq pending-event event)))))
            (process-put stderr-process 'magnus-headless-process process)
            (process-put process 'magnus-headless-stderr-process stderr-process)
            (process-put process 'magnus-headless-provider provider)
            (process-put process 'magnus-headless-request prepared)
            (process-put process 'magnus-headless-callbacks callbacks)
            (process-put process 'magnus-headless-decoder
                         (plist-get spec :decoder))
            (process-put process 'magnus-headless-success-requires
                         (plist-get spec :success-requires))
            (process-put process 'magnus-headless-partial-line "")
            (process-put process 'magnus-headless-discarding-line-p nil)
            (when-let ((session-id (plist-get spec :session-id)))
              (process-put process 'magnus-headless-session-id session-id))
            (when-let ((candidate (plist-get spec :candidate-session-id)))
              (process-put process 'magnus-headless-candidate-session-id
                           candidate))
            ;; `codex exec PROMPT' appends piped stdin as extra context.  An
            ;; Emacs pipe remains writable until explicitly closed, so without
            ;; this EOF Codex waits indefinitely after printing "Reading
            ;; additional input from stdin...".  No headless provider accepts
            ;; interactive input through this API; prompts are already in the
            ;; provider command.
            (when (process-live-p process)
              (process-send-eof process))
            ;; Activation occurs on the next event-loop turn.  Until then the
            ;; bootstrap filters above retain output, which guarantees that a
            ;; very short-lived subprocess cannot outrun property installation
            ;; and that no user callback fires synchronously from this API.
            (run-at-time
             0 nil
             (lambda ()
               (setq active t)
               (magnus-headless--notify-initial-session process)
               (dolist (chunk (nreverse pending-output))
                 (magnus-headless--filter process chunk))
               (dolist (chunk (nreverse pending-stderr))
                 (magnus-headless--stderr-filter stderr-process chunk))
               (when (or pending-event
                         (memq (process-status process)
                               '(exit signal failed closed)))
                 (magnus-headless--sentinel
                  process (or pending-event "process finished")))))
            process))
      (error
       (when (and process (process-live-p process))
         (delete-process process))
       (when (and stderr-process (process-live-p stderr-process))
         (delete-process stderr-process))
       (magnus-headless--delete-owned-schema prepared)
       (signal (car err) (cdr err))))))

(defun magnus-headless-session-id (process)
  "Return the captured, preassigned, or resumed session ID for PROCESS."
  (process-get process 'magnus-headless-session-id))

(defun magnus-headless-cancel (process &optional force)
  "Cancel live headless PROCESS.
With FORCE, kill the process immediately; otherwise send an interrupt."
  (when (process-live-p process)
    (if force
        (kill-process process)
      (interrupt-process process))))

(provide 'magnus-headless)
;;; magnus-headless.el ends here
