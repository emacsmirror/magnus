;;; magnus-coord-store.el --- Durable coordination event inbox -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module is the storage boundary for Magnus coordination events.  Each
;; writer owns an append-only inbox beneath:
;;
;;   PROJECT/.magnus-coord/writers/WRITER-ID/EVENT-ID.json
;;
;; An event file is an immutable schema-1 JSON object containing `schema',
;; `id', `writer_id', `writer_name', `writer_sequence', `created_at', `kind',
;; and an object-valued `payload'.  Events are an unordered cross-writer
;; evidence set with causal order only within each writer.  This module
;; deliberately does not interpret event kinds, render the human coordination
;; document, deliver effects, or compact old events.  Those policies belong to
;; the coordination layer above it.
;;
;; The public boundary is intentionally small:
;;
;; - `magnus-coord-store-directory' returns the validated store root.
;; - `magnus-coord-store-writer-directory' returns a validated inbox path.
;; - `magnus-coord-store-ensure-writer-directory' creates a hardened inbox.
;; - `magnus-coord-store-publish' atomically publishes one immutable event.
;; - `magnus-coord-store-revision' derives a cheap metadata-only change token.
;; - `magnus-coord-store-snapshot' reads one deterministic filesystem snapshot.
;; - `magnus-coord-store-prune' removes revalidated evidence from that snapshot.
;;
;; Snapshot failures are returned as `magnus-coord-store-issue' values.  One
;; malformed or unsafe entry therefore cannot hide valid sibling events.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup magnus-coord-store nil
  "Durable event storage for Magnus coordination."
  :group 'magnus
  :prefix "magnus-coord-store-")

(defcustom magnus-coord-store-directory-name ".magnus-coord"
  "Directory beneath a project that contains durable coordination events."
  :type 'string
  :group 'magnus-coord-store)

(defcustom magnus-coord-store-max-event-bytes (* 64 1024)
  "Maximum encoded size of one coordination event file."
  :type 'integer
  :group 'magnus-coord-store)

(defconst magnus-coord-store-schema-version 1
  "Current durable coordination event schema.")

(defconst magnus-coord-store--event-fields
  '("created_at" "id" "kind" "payload" "schema" "writer_id" "writer_name"
    "writer_sequence")
  "Exact set of fields accepted in a schema-1 event envelope.")

(defvar magnus-coord-store--id-counter 0
  "Process-local input used to make generated event identifiers unique.")

(define-error 'magnus-coord-store-error
  "Magnus coordination event-store error")
(define-error 'magnus-coord-store-invalid-event
  "Invalid Magnus coordination event" 'magnus-coord-store-error)
(define-error 'magnus-coord-store-conflict
  "Conflicting Magnus coordination event" 'magnus-coord-store-error)
(define-error 'magnus-coord-store-unsafe-entry
  "Unsafe Magnus coordination event entry" 'magnus-coord-store-error)
(define-error 'magnus-coord-store-oversized-entry
  "Oversized Magnus coordination event entry" 'magnus-coord-store-error)
(define-error 'magnus-coord-store-changed-entry
  "Magnus coordination event changed while being read"
  'magnus-coord-store-error)

(cl-defstruct (magnus-coord-store-event
               (:constructor magnus-coord-store-event--create)
               (:copier nil))
  "One validated immutable coordination event."
  schema
  id
  writer-id
  writer-name
  writer-sequence
  created-at
  kind
  payload
  path
  file-identifier
  content-hash
  bytes)

(cl-defstruct (magnus-coord-store-issue
               (:constructor magnus-coord-store-issue--create)
               (:copier nil))
  "A problem with one entry observed during a store snapshot."
  path
  code
  message
  writer-id
  event-id
  related-path
  writer-sequence
  related-event-id)

(cl-defstruct (magnus-coord-store-snapshot
               (:constructor magnus-coord-store-snapshot--create)
               (:copier nil))
  "A deterministic, isolated view of the event inbox."
  project-directory
  captured-at
  candidate-paths
  events
  issues)

(cl-defstruct (magnus-coord-store-revision-result
               (:constructor magnus-coord-store-revision-result--create)
               (:copier nil))
  "Cheap metadata revision and any unsafe entries observed while deriving it."
  token
  issues)

(cl-defstruct (magnus-coord-store-prune-result
               (:constructor magnus-coord-store-prune-result--create)
               (:copier nil))
  "Structured outcome of pruning one supplied snapshot."
  deleted-events
  kept-events
  issues)

(defun magnus-coord-store--safe-segment-p (value &optional max-bytes)
  "Return non-nil when VALUE is a safe single pathname segment.
MAX-BYTES defaults to 160."
  (and (stringp value)
       (<= (string-bytes value) (or max-bytes 160))
       (string-match-p "\\`[A-Za-z0-9_][A-Za-z0-9_.-]*\\'" value)))

(defun magnus-coord-store--safe-directory-name-p (value)
  "Return non-nil when VALUE is one safe store directory name."
  (and (stringp value)
       (not (member value '("" "." "..")))
       ;; `expand-file-name' treats a leading tilde specially even when the
       ;; spelling contains no slash.  Reject it before composing a child path.
       (not (string-prefix-p "~" value))
       (not (file-name-absolute-p value))
       (<= (string-bytes value) 160)
       (string= value (file-name-nondirectory value))))

(defun magnus-coord-store--max-event-bytes ()
  "Return the validated configured event-size bound."
  (unless (and (integerp magnus-coord-store-max-event-bytes)
               (> magnus-coord-store-max-event-bytes 0))
    (signal 'magnus-coord-store-error
            '("Event-size bound must be a positive integer")))
  magnus-coord-store-max-event-bytes)

(defun magnus-coord-store--valid-writer-name-p (value)
  "Return non-nil when VALUE is a bounded printable writer name."
  (and (stringp value)
       (not (string-empty-p value))
       (<= (string-bytes value) 256)
       (not (cl-some (lambda (character)
                       (or (< character 32) (= character 127)))
                     (string-to-list value)))))

(defun magnus-coord-store--valid-timestamp-p (value)
  "Return non-nil when VALUE is a real canonical UTC timestamp.
Schema 1 uses six fractional-second digits so lexical order is time order."
  (when (and
         (stringp value)
         (string-match
          (concat
           "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-"
           "\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):"
           "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\."
           "[0-9]\\{6\\}Z\\'")
          value))
    (let ((year (string-to-number (match-string 1 value)))
          (month (string-to-number (match-string 2 value)))
          (day (string-to-number (match-string 3 value)))
          (hour (string-to-number (match-string 4 value)))
          (minute (string-to-number (match-string 5 value)))
          (second (string-to-number (match-string 6 value))))
      (condition-case timestamp-error
          (let ((decoded
                 (decode-time
                  (encode-time second minute hour day month year t) t)))
            (and (= second (nth 0 decoded))
                 (= minute (nth 1 decoded))
                 (= hour (nth 2 decoded))
                 (= day (nth 3 decoded))
                 (= month (nth 4 decoded))
                 (= year (nth 5 decoded))))
        (error (ignore timestamp-error))))))

(defun magnus-coord-store--timestamp ()
  "Return the current time in schema-1 canonical UTC form."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ" nil t))

(defun magnus-coord-store--new-event-id (writer-id created-at)
  "Return a collision-resistant event identifier for WRITER-ID at CREATED-AT."
  (secure-hash
   'sha256
   (format "%s\0%s\0%s\0%s\0%s"
           writer-id created-at (emacs-pid)
           (cl-incf magnus-coord-store--id-counter)
           (random most-positive-fixnum))))

(defun magnus-coord-store--project-directory (project-directory)
  "Return validated canonical spelling of PROJECT-DIRECTORY."
  (unless (and (stringp project-directory)
               (not (string-empty-p project-directory)))
    (signal 'magnus-coord-store-error
            '("Project directory must be a nonempty string")))
  (let ((directory
         (file-name-as-directory (expand-file-name project-directory))))
    (when (file-remote-p directory)
      (signal 'magnus-coord-store-error
              (list (format "Coordination store may not be remote: %s"
                            directory))))
    (unless (file-directory-p directory)
      (signal 'magnus-coord-store-error
              (list (format "Project directory does not exist: %s"
                            directory))))
    directory))

(defun magnus-coord-store--root (project-directory)
  "Return the coordination store root for PROJECT-DIRECTORY."
  (unless (magnus-coord-store--safe-directory-name-p
           magnus-coord-store-directory-name)
    (signal 'magnus-coord-store-error
            (list (format "Unsafe coordination store directory name: %S"
                          magnus-coord-store-directory-name))))
  (expand-file-name magnus-coord-store-directory-name
                    (magnus-coord-store--project-directory project-directory)))

(defun magnus-coord-store-directory (project-directory)
  "Return PROJECT-DIRECTORY's validated coordination store path.
This read-only path helper does not create the store."
  (magnus-coord-store--root project-directory))

(defun magnus-coord-store-writer-directory (project-directory writer-id)
  "Return WRITER-ID's validated inbox path beneath PROJECT-DIRECTORY.
This function does not create the directory."
  (unless (magnus-coord-store--safe-segment-p writer-id)
    (signal 'magnus-coord-store-invalid-event
            (list (format "Unsafe writer id: %S" writer-id))))
  (expand-file-name
   writer-id
   (expand-file-name "writers" (magnus-coord-store--root project-directory))))

(defun magnus-coord-store--ensure-directory (directory)
  "Ensure owned DIRECTORY exists without traversing an owned symlink."
  (when (file-symlink-p directory)
    (signal 'magnus-coord-store-unsafe-entry
            (list (format "Refusing symlinked store directory: %s"
                          directory))))
  (unless (file-exists-p directory)
    (condition-case error-data
        (make-directory directory nil)
      (file-already-exists nil)
      (file-error
       (if (file-exists-p directory)
           nil
         (signal (car error-data) (cdr error-data))))))
  (when (file-symlink-p directory)
    (signal 'magnus-coord-store-unsafe-entry
            (list (format "Refusing symlinked store directory: %s"
                          directory))))
  (unless (file-directory-p directory)
    (signal 'magnus-coord-store-unsafe-entry
            (list (format "Store path is not a directory: %s" directory))))
  ;; These directories are owned by Magnus, including when a prior partial
  ;; run created them.  Do not silently retain group/world-writable inboxes.
  (set-file-modes directory #o700)
  directory)

(defun magnus-coord-store-ensure-writer-directory
    (project-directory writer-id)
  "Create, harden, and return WRITER-ID's inbox in PROJECT-DIRECTORY."
  (let* ((root (magnus-coord-store--root project-directory))
         (writers (expand-file-name "writers" root))
         (writer (magnus-coord-store-writer-directory
                  project-directory writer-id)))
    (magnus-coord-store--ensure-directory root)
    (magnus-coord-store--ensure-directory writers)
    (magnus-coord-store--ensure-directory writer)))

(defun magnus-coord-store--invalid-event (format-string &rest arguments)
  "Signal an invalid-event error using FORMAT-STRING and ARGUMENTS."
  (signal 'magnus-coord-store-invalid-event
          (list (apply #'format format-string arguments))))

(defun magnus-coord-store--object-fields (object)
  "Return sorted string keys from JSON OBJECT."
  (let (fields)
    (maphash (lambda (key _value) (push key fields)) object)
    (sort fields #'string<)))

(defun magnus-coord-store--decode-event
    (bytes path expected-writer-id expected-event-id)
  "Decode and validate one event from BYTES read at PATH.
EXPECTED-WRITER-ID and EXPECTED-EVENT-ID bind the envelope to its pathname."
  (let ((object
         (json-parse-string bytes :object-type 'hash-table :array-type 'array
                            :null-object nil :false-object :json-false)))
    (unless (hash-table-p object)
      (magnus-coord-store--invalid-event "Event envelope is not an object"))
    (unless (equal (magnus-coord-store--object-fields object)
                   magnus-coord-store--event-fields)
      (magnus-coord-store--invalid-event
       "Event envelope has missing or unknown fields"))
    (let ((schema (gethash "schema" object))
          (event-id (gethash "id" object))
          (writer-id (gethash "writer_id" object))
          (writer-name (gethash "writer_name" object))
          (writer-sequence (gethash "writer_sequence" object))
          (created-at (gethash "created_at" object))
          (kind (gethash "kind" object))
          (payload (gethash "payload" object)))
      (unless (and (integerp schema)
                   (= schema magnus-coord-store-schema-version))
        (magnus-coord-store--invalid-event
         "Unsupported event schema: %S" schema))
      (unless (magnus-coord-store--safe-segment-p event-id)
        (magnus-coord-store--invalid-event
         "Unsafe event id: %S" event-id))
      (unless (magnus-coord-store--safe-segment-p writer-id)
        (magnus-coord-store--invalid-event
         "Unsafe writer id: %S" writer-id))
      (unless (string-equal writer-id expected-writer-id)
        (magnus-coord-store--invalid-event
         "Envelope writer id %S does not match inbox %S"
         writer-id expected-writer-id))
      (unless (string-equal event-id expected-event-id)
        (magnus-coord-store--invalid-event
         "Envelope event id %S does not match filename %S"
         event-id expected-event-id))
      (unless (magnus-coord-store--valid-writer-name-p writer-name)
        (magnus-coord-store--invalid-event
         "Invalid writer name: %S" writer-name))
      (unless (and (integerp writer-sequence) (> writer-sequence 0))
        (magnus-coord-store--invalid-event
         "Writer sequence must be a positive integer: %S" writer-sequence))
      (unless (magnus-coord-store--valid-timestamp-p created-at)
        (magnus-coord-store--invalid-event
         "Invalid created_at timestamp: %S" created-at))
      (unless (magnus-coord-store--safe-segment-p kind 128)
        (magnus-coord-store--invalid-event
         "Invalid event kind: %S" kind))
      (unless (hash-table-p payload)
        (magnus-coord-store--invalid-event
         "Event payload must be a JSON object"))
      (magnus-coord-store-event--create
       :schema schema
       :id event-id
       :writer-id writer-id
       :writer-name writer-name
       :writer-sequence writer-sequence
       :created-at created-at
       :kind kind
       :payload payload
       :path path
       :content-hash (secure-hash 'sha256 bytes)
       :bytes bytes))))

(defun magnus-coord-store--encode-event
    (path event-id writer-id writer-name writer-sequence created-at kind payload)
  "Encode and validate an event intended for PATH.
The remaining arguments are the schema-1 envelope values."
  (let ((object (make-hash-table :test #'equal))
        text)
    (puthash "schema" magnus-coord-store-schema-version object)
    (puthash "id" event-id object)
    (puthash "writer_id" writer-id object)
    (puthash "writer_name" writer-name object)
    (puthash "writer_sequence" writer-sequence object)
    (puthash "created_at" created-at object)
    (puthash "kind" kind object)
    (puthash "payload" payload object)
    (setq text
          (condition-case error-data
              (json-serialize object :null-object nil
                              :false-object :json-false)
            (error
             (magnus-coord-store--invalid-event
              "Payload is not JSON serializable: %s"
              (error-message-string error-data)))))
    (let* ((bytes (encode-coding-string (concat text "\n") 'utf-8-unix t))
           (maximum (magnus-coord-store--max-event-bytes)))
      (when (> (string-bytes bytes) maximum)
        (signal 'magnus-coord-store-oversized-entry
                (list (format "Encoded event exceeds %d bytes" maximum))))
      (condition-case error-data
          (magnus-coord-store--decode-event
           bytes path writer-id event-id)
        (json-error
       (magnus-coord-store--invalid-event
          "Serialized event is not valid JSON: %s"
          (error-message-string error-data)))))))

(defun magnus-coord-store--read-stable-entry (path)
  "Read bounded regular non-symlink PATH exactly once.
Return (BYTES . ATTRIBUTES) from the same stable read.  Signal when PATH is
unsafe, too large, or replaced during the read."
  (when (file-symlink-p path)
    (signal 'magnus-coord-store-unsafe-entry
            (list (format "Event entry is a symlink: %s" path))))
  (let ((before (file-attributes path 'string))
        (maximum (magnus-coord-store--max-event-bytes)))
    (unless (and before (null (file-attribute-type before))
                 (file-regular-p path))
      (signal 'magnus-coord-store-unsafe-entry
              (list (format "Event entry is not a regular file: %s" path))))
    (when (> (file-attribute-size before) maximum)
      (signal 'magnus-coord-store-oversized-entry
              (list
               (format "Event entry exceeds %d bytes: %s"
                       maximum path))))
    (let ((bytes
           (with-temp-buffer
             (set-buffer-multibyte nil)
             ;; END bounds a file that grows after the size check.  This is the
             ;; sole content read performed for this candidate.
             (insert-file-contents-literally
              path nil 0 (1+ maximum))
             (buffer-string)))
          (after (file-attributes path 'string)))
      (when (> (string-bytes bytes) maximum)
        (signal 'magnus-coord-store-oversized-entry
                (list
                 (format "Event entry exceeds %d bytes: %s"
                         maximum path))))
      (when (or (file-symlink-p path)
                (not (and after (null (file-attribute-type after))
                          (file-regular-p path)))
                (not (equal (file-attribute-file-identifier before)
                            (file-attribute-file-identifier after)))
                (not (= (file-attribute-size before)
                        (file-attribute-size after)))
                (not (= (file-attribute-size after) (string-bytes bytes)))
                (not (equal (file-attribute-modification-time before)
                            (file-attribute-modification-time after))))
        (signal 'magnus-coord-store-changed-entry
                (list (format "Event entry changed while being read: %s"
                              path))))
      (cons bytes after))))

(defun magnus-coord-store--read-stable-bytes (path)
  "Return bytes from one stable bounded read of PATH."
  (car (magnus-coord-store--read-stable-entry path)))

(defun magnus-coord-store--existing-content-matches-p (path bytes)
  "Return non-nil when immutable PATH contains exactly BYTES."
  (string= (magnus-coord-store--read-stable-bytes path) bytes))

(cl-defun magnus-coord-store-publish
    (project-directory writer-id writer-name kind payload
                       &key event-id created-at writer-sequence)
  "Publish one immutable coordination event and return it.

PROJECT-DIRECTORY owns the store.  WRITER-ID is the stable instance identity,
WRITER-NAME is its display name, KIND is a bounded event-kind string, and
PAYLOAD must serialize as a JSON object.  WRITER-SEQUENCE is a required
positive integer allocated durably by that writer.  EVENT-ID and CREATED-AT
may be supplied when replaying deterministic evidence; otherwise Magnus
generates them.

Publication writes a private temporary file beside the final event and commits
it with an atomic no-replace hard link.  Re-publishing the same ID with
byte-identical content is idempotent.  Reusing an ID with different content
signals `magnus-coord-store-conflict'."
  (let* ((created-at (or created-at (magnus-coord-store--timestamp)))
         (event-id (or event-id
                       (magnus-coord-store--new-event-id writer-id created-at)))
         (writer-directory
          (magnus-coord-store-writer-directory project-directory writer-id))
         (path (expand-file-name (concat event-id ".json") writer-directory))
         ;; Encode before creating directories: invalid caller data must not
         ;; leave a partial store hierarchy behind.
         (event (magnus-coord-store--encode-event
                 path event-id writer-id writer-name writer-sequence
                 created-at kind payload))
         (bytes (magnus-coord-store-event-bytes event))
         temporary)
    (magnus-coord-store-ensure-writer-directory project-directory writer-id)
    (when (or (file-exists-p path) (file-symlink-p path))
      (if (magnus-coord-store--existing-content-matches-p path bytes)
          (cl-return-from magnus-coord-store-publish event)
        (signal 'magnus-coord-store-conflict
                (list (format "Event id already has different content: %s"
                              event-id)))))
    (setq temporary
          (make-temp-file
           (expand-file-name ".magnus-event-tmp-" writer-directory)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil temporary nil 'quiet))
          (set-file-modes temporary #o600)
          (condition-case publish-error
              (progn
                ;; A hard-link create is an atomic no-replace operation on the
                ;; local filesystems supported by Magnus.  This avoids relying
                ;; on the implementation of `rename-file' in older Emacsen.
                (add-name-to-file temporary path nil)
                ;; The final name is now committed.  Cleanup must not turn a
                ;; successful publication into an apparent failure.
                (let ((published-temporary temporary))
                  (setq temporary nil)
                  (ignore-errors (delete-file published-temporary)))
                event)
            (file-already-exists
             (if (magnus-coord-store--existing-content-matches-p path bytes)
                 event
               (signal 'magnus-coord-store-conflict
                       (list
                        (format "Event id concurrently acquired: %s"
                                event-id)))))
            (file-error
             ;; Some filesystems report an existing target as a generic
             ;; `file-error'.  Resolve that race without hiding other errors.
             (if (or (file-exists-p path) (file-symlink-p path))
                 (if (magnus-coord-store--existing-content-matches-p
                      path bytes)
                     event
                   (signal 'magnus-coord-store-conflict
                           (list
                            (format "Event id concurrently acquired: %s"
                                    event-id))))
               (signal (car publish-error) (cdr publish-error))))))
      ;; Keep both the predicate and deletion below error precedence: a parent
      ;; disappearing while unwinding must not replace the original failure.
      (when temporary
        (ignore-errors (delete-file temporary))))))

(defun magnus-coord-store--issue
    (path code message &optional writer-id event-id related-path
          writer-sequence related-event-id)
  "Construct an issue for PATH with CODE and MESSAGE.
The optional identity fields preserve all evidence needed to diagnose a
conflict without reopening PATH."
  (magnus-coord-store-issue--create
   :path path
   :code code
   :message message
   :writer-id writer-id
   :event-id event-id
   :related-path related-path
   :writer-sequence writer-sequence
   :related-event-id related-event-id))

(defun magnus-coord-store--directory-entries (directory)
  "Return a sorted snapshot of DIRECTORY's non-dot entries."
  (sort (directory-files directory t directory-files-no-dot-files-regexp t)
        #'string<))

(defun magnus-coord-store--discover-candidates (project-directory)
  "Return (CANDIDATES . ISSUES) for PROJECT-DIRECTORY.
Each candidate is (WRITER-ID . PATH).  No candidate content is read here."
  (let* ((root (magnus-coord-store--root project-directory))
         (writers (expand-file-name "writers" root))
         candidates
         issues)
    (cond
     ((not (or (file-exists-p root) (file-symlink-p root))))
     ((or (file-symlink-p root) (not (file-directory-p root)))
      (push (magnus-coord-store--issue
             root 'unsafe-store "Store root is not a safe directory")
            issues))
     ((not (or (file-exists-p writers) (file-symlink-p writers))))
     ((or (file-symlink-p writers) (not (file-directory-p writers)))
      (push (magnus-coord-store--issue
             writers 'unsafe-store "Writers root is not a safe directory")
            issues))
     (t
      (condition-case scan-error
          (dolist (writer-path
                   (magnus-coord-store--directory-entries writers))
            (let ((writer-id
                   (file-name-nondirectory
                    (directory-file-name writer-path))))
              (cond
               ((not (magnus-coord-store--safe-segment-p writer-id))
                (push (magnus-coord-store--issue
                       writer-path 'invalid-writer-path
                       "Writer directory has an unsafe identifier" writer-id)
                      issues))
               ((or (file-symlink-p writer-path)
                    (not (file-directory-p writer-path)))
                (push (magnus-coord-store--issue
                       writer-path 'unsafe-writer-path
                       "Writer inbox is not a safe directory" writer-id)
                      issues))
               (t
                (condition-case writer-error
                    (dolist (path
                             (magnus-coord-store--directory-entries writer-path))
                      (let* ((filename (file-name-nondirectory path))
                             (event-id
                              (and (string-suffix-p ".json" filename)
                                   (substring filename 0 -5))))
                        (cond
                         ;; A publisher may coexist with this snapshot.  Its
                         ;; reserved same-directory temporary is not an event.
                         ((string-prefix-p ".magnus-event-tmp-" filename))
                         ((and event-id
                               (magnus-coord-store--safe-segment-p event-id))
                          (push (cons writer-id path) candidates))
                         (t
                          (push (magnus-coord-store--issue
                                 path 'invalid-event-path
                                 "Event filename must be a safe ID plus .json"
                                 writer-id event-id)
                                issues)))))
                  (file-error
                   (push (magnus-coord-store--issue
                          writer-path 'scan-error
                          (error-message-string writer-error) writer-id)
                         issues)))))))
        (file-error
         (push (magnus-coord-store--issue
                writers 'scan-error (error-message-string scan-error))
               issues)))))
    (cons (delete-dups (sort candidates
                              (lambda (left right)
                                (string< (cdr left) (cdr right)))))
          issues)))

(defun magnus-coord-store--read-candidate (candidate)
  "Read and validate CANDIDATE, returning (EVENT . ISSUE)."
  (let* ((writer-id (car candidate))
         (path (cdr candidate))
         (filename (file-name-nondirectory path))
         (event-id (substring filename 0 -5)))
    (condition-case error-data
        (let* ((entry (magnus-coord-store--read-stable-entry path))
               (event (magnus-coord-store--decode-event
                       (car entry) path writer-id event-id)))
          (setf (magnus-coord-store-event-file-identifier event)
                (file-attribute-file-identifier (cdr entry)))
          (cons event nil))
      (magnus-coord-store-invalid-event
       (cons nil (magnus-coord-store--issue
                  path 'invalid-event (error-message-string error-data)
                  writer-id event-id)))
      (json-parse-error
       (cons nil (magnus-coord-store--issue
                  path 'invalid-json (error-message-string error-data)
                  writer-id event-id)))
      (magnus-coord-store-unsafe-entry
       (cons nil (magnus-coord-store--issue
                  path 'unsafe-entry (error-message-string error-data)
                  writer-id event-id)))
      (magnus-coord-store-oversized-entry
       (cons nil (magnus-coord-store--issue
                  path 'oversized-entry (error-message-string error-data)
                  writer-id event-id)))
      (magnus-coord-store-changed-entry
       (cons nil (magnus-coord-store--issue
                  path 'changed-entry (error-message-string error-data)
                  writer-id event-id)))
      (error
       (cons nil (magnus-coord-store--issue
                  path 'read-error (error-message-string error-data)
                  writer-id event-id))))))

(defun magnus-coord-store--deduplicate-events (events)
  "Resolve identity and writer-sequence conflicts in path-ordered EVENTS.
Return (EVENTS . ISSUES).  Identical bytes for one global ID are idempotent.
Differing bytes for an ID, or different IDs claiming the same writer sequence,
produce a structured issue and the deterministic first event wins."
  (let ((seen-ids (make-hash-table :test #'equal))
        (seen-sequences (make-hash-table :test #'equal))
        (seen-variants (make-hash-table :test #'equal))
        accepted
        issues)
    (dolist (event events)
      (let* ((id (magnus-coord-store-event-id event))
             (sequence-key
              (cons (magnus-coord-store-event-writer-id event)
                    (magnus-coord-store-event-writer-sequence event)))
             (prior-id (gethash id seen-ids))
             (prior-sequence (gethash sequence-key seen-sequences))
             (variant-key
              (cons id (magnus-coord-store-event-bytes event)))
             (exact-duplicate
              (gethash variant-key seen-variants))
             (id-conflict (and prior-id (not exact-duplicate)))
             (sequence-conflict (and prior-sequence
                                     (not exact-duplicate))))
        ;; Each dimension is an independent claim.  Even rejected evidence
        ;; reserves any previously unseen dimension so a later event cannot
        ;; slip through a conflict chain and acquire the same causal slot.
        (unless prior-id
          (puthash id event seen-ids))
        (unless prior-sequence
          (puthash sequence-key event seen-sequences))
        (puthash variant-key event seen-variants)
        (when id-conflict
          (push
           (magnus-coord-store--issue
            (magnus-coord-store-event-path event)
            'duplicate-conflict
            (format "Event id %s has different immutable content" id)
            (magnus-coord-store-event-writer-id event)
            id
            (magnus-coord-store-event-path prior-id)
            (magnus-coord-store-event-writer-sequence event)
            (magnus-coord-store-event-id prior-id))
           issues))
        (when sequence-conflict
          (push
           (magnus-coord-store--issue
            (magnus-coord-store-event-path event)
            'writer-sequence-conflict
            (format "Writer %s reused sequence %d for event %s"
                    (magnus-coord-store-event-writer-id event)
                    (magnus-coord-store-event-writer-sequence event)
                    id)
            (magnus-coord-store-event-writer-id event)
            id
            (magnus-coord-store-event-path prior-sequence)
            (magnus-coord-store-event-writer-sequence event)
            (magnus-coord-store-event-id prior-sequence))
           issues))
        (unless (or exact-duplicate id-conflict sequence-conflict)
          (push event accepted))))
    (cons (nreverse accepted) (nreverse issues))))

(defun magnus-coord-store--event-less-p (left right)
  "Return non-nil when event LEFT sorts before event RIGHT."
  (let ((left-key
         (list (magnus-coord-store-event-writer-id left)
               (magnus-coord-store-event-writer-sequence left)
               (magnus-coord-store-event-id left)
               (magnus-coord-store-event-path left)))
        (right-key
         (list (magnus-coord-store-event-writer-id right)
               (magnus-coord-store-event-writer-sequence right)
               (magnus-coord-store-event-id right)
               (magnus-coord-store-event-path right))))
    (catch 'less
      (while left-key
        (let ((left-part (pop left-key))
              (right-part (pop right-key)))
          (cond
           ((and (numberp left-part) (numberp right-part))
            (cond
             ((< left-part right-part) (throw 'less t))
             ((< right-part left-part) (throw 'less nil))))
           ((string< left-part right-part) (throw 'less t))
           ((string< right-part left-part) (throw 'less nil)))))
      nil)))

(defun magnus-coord-store--issue-less-p (left right)
  "Return non-nil when issue LEFT sorts before issue RIGHT."
  (let ((left-key (format "%s\0%s\0%s"
                          (or (magnus-coord-store-issue-path left) "")
                          (magnus-coord-store-issue-code left)
                          (magnus-coord-store-issue-message left)))
        (right-key (format "%s\0%s\0%s"
                           (or (magnus-coord-store-issue-path right) "")
                           (magnus-coord-store-issue-code right)
                           (magnus-coord-store-issue-message right))))
    (string< left-key right-key)))

(defun magnus-coord-store--revision-metadata (path &optional include-mtime)
  "Return deterministic no-content metadata for PATH.
Include modification time only when INCLUDE-MTIME is non-nil."
  (condition-case metadata-error
      (let ((attributes (file-attributes path 'string))
            (link (file-symlink-p path)))
        (list
         (cond
          (link 'symlink)
          ((and attributes (eq (file-attribute-type attributes) t)) 'directory)
          ((and attributes (null (file-attribute-type attributes))) 'file)
          (t 'missing))
         link
         (and attributes (file-attribute-file-identifier attributes))
         (and include-mtime attributes
              (file-attribute-modification-time attributes))))
    (file-error
     (list 'metadata-error (error-message-string metadata-error)))))

(defun magnus-coord-store--revision-token (evidence)
  "Return a deterministic token for metadata EVIDENCE."
  (secure-hash
   'sha256
   (encode-coding-string (prin1-to-string evidence) 'utf-8-unix t)))

(defun magnus-coord-store-revision (project-directory)
  "Return a cheap metadata revision result for PROJECT-DIRECTORY.

The token covers the store identity plus writers-root and writer-directory
identities and modification times.  Only `writers/' itself is listed; writer
inboxes and event files are never listed, opened, or statted.  Consequently
writer creation/deletion and atomic event addition/removal change the token
without imposing a full snapshot read.  Store-root mtime is deliberately
excluded so replacing the `current.md' projection cannot self-trigger.  Unsafe
root and writer entries are encoded into the token and returned as structured
issues; one bad writer cannot hide healthy siblings."
  (let* ((project (magnus-coord-store--project-directory project-directory))
         (root (magnus-coord-store-directory project))
         (writers (expand-file-name "writers" root))
         evidence
         issues)
    (cond
     ((not (or (file-exists-p root) (file-symlink-p root)))
      (push '(root missing) evidence))
     ((or (file-symlink-p root) (not (file-directory-p root)))
      (push (list 'root 'unsafe
                  (magnus-coord-store--revision-metadata root))
            evidence)
      (push (magnus-coord-store--issue
             root 'unsafe-store "Store root is not a safe directory")
            issues))
     (t
      (push (list 'root (magnus-coord-store--revision-metadata root))
            evidence)
      (cond
       ((not (or (file-exists-p writers) (file-symlink-p writers)))
        (push '(writers missing) evidence))
       ((or (file-symlink-p writers) (not (file-directory-p writers)))
        (push (list 'writers 'unsafe
                    (magnus-coord-store--revision-metadata writers t))
              evidence)
        (push (magnus-coord-store--issue
               writers 'unsafe-store "Writers root is not a safe directory")
              issues))
       (t
        (push (list 'writers
                    (magnus-coord-store--revision-metadata writers t))
              evidence)
        (condition-case scan-error
            (dolist (writer-path
                     (magnus-coord-store--directory-entries writers))
              (let* ((writer-id
                      (file-name-nondirectory
                       (directory-file-name writer-path)))
                     (metadata
                      (magnus-coord-store--revision-metadata writer-path t)))
                (cond
                 ((not (magnus-coord-store--safe-segment-p writer-id))
                  (push (list 'writer writer-id 'invalid metadata) evidence)
                  (push (magnus-coord-store--issue
                         writer-path 'invalid-writer-path
                         "Writer directory has an unsafe identifier" writer-id)
                        issues))
                 ((or (file-symlink-p writer-path)
                      (not (file-directory-p writer-path)))
                  (push (list 'writer writer-id 'unsafe metadata) evidence)
                  (push (magnus-coord-store--issue
                         writer-path 'unsafe-writer-path
                         "Writer inbox is not a safe directory" writer-id)
                        issues))
                 (t
                  (push (list 'writer writer-id metadata) evidence)))))
          (file-error
           (push (list 'writers 'scan-error
                       (error-message-string scan-error)) evidence)
           (push (magnus-coord-store--issue
                  writers 'scan-error (error-message-string scan-error))
                 issues)))))))
    (setq evidence (nreverse evidence)
          issues (sort issues #'magnus-coord-store--issue-less-p))
    (magnus-coord-store-revision-result--create
     :token (magnus-coord-store--revision-token evidence)
     :issues issues)))

(defun magnus-coord-store-snapshot (project-directory)
  "Return one isolated event-store snapshot for PROJECT-DIRECTORY.

Candidate paths are captured and sorted before any content read.  Every
candidate is read at most once, with a byte bound, and every failure becomes a
structured issue.  Valid events are globally deduplicated by ID and sorted by
writer ID, writer sequence, event ID, and path.  Wall time is metadata, never a
causal ordering input."
  (let* ((project (magnus-coord-store--project-directory project-directory))
         (discovery (magnus-coord-store--discover-candidates project))
         (candidates (car discovery))
         (issues (cdr discovery))
         parsed)
    (dolist (candidate candidates)
      (let ((result (magnus-coord-store--read-candidate candidate)))
        (if (car result)
            (push (car result) parsed)
          (push (cdr result) issues))))
    ;; PARSED was pushed while candidates were in deterministic path order.
    (let* ((deduplicated
            (magnus-coord-store--deduplicate-events (nreverse parsed)))
           (events (sort (car deduplicated)
                         #'magnus-coord-store--event-less-p))
           (all-issues (sort (append (cdr deduplicated) issues)
                             #'magnus-coord-store--issue-less-p)))
      (magnus-coord-store-snapshot--create
       :project-directory project
       :captured-at (current-time)
       :candidate-paths (mapcar #'cdr candidates)
       :events events
       :issues all-issues))))

(defun magnus-coord-store--id-set (ids)
  "Return an equal-tested set copied from list or hash-table IDS."
  (let ((result (make-hash-table :test #'equal)))
    (cond
     ((hash-table-p ids)
      (maphash
       (lambda (id present)
         (when present
           (unless (magnus-coord-store--safe-segment-p id)
             (signal 'magnus-coord-store-error
                     (list (format "Invalid kept event id: %S" id))))
           (puthash id t result)))
       ids))
     ((proper-list-p ids)
      (dolist (id ids)
        (unless (magnus-coord-store--safe-segment-p id)
          (signal 'magnus-coord-store-error
                  (list (format "Invalid kept event id: %S" id))))
        (puthash id t result)))
     (t
      (signal 'magnus-coord-store-error
              '("Keep IDs must be a list or hash table"))))
    result))

(defun magnus-coord-store--conflicted-id-set (snapshot)
  "Return event IDs protected by identity conflicts in SNAPSHOT."
  (let ((result (make-hash-table :test #'equal)))
    (dolist (issue (magnus-coord-store-snapshot-issues snapshot))
      (when (memq (magnus-coord-store-issue-code issue)
                  '(duplicate-conflict writer-sequence-conflict))
        (when (magnus-coord-store-issue-event-id issue)
          (puthash (magnus-coord-store-issue-event-id issue) t result))
        (when (magnus-coord-store-issue-related-event-id issue)
          (puthash (magnus-coord-store-issue-related-event-id issue) t result))))
    result))

(defun magnus-coord-store--prune-issue (event code message)
  "Return a pruning issue for EVENT using CODE and MESSAGE."
  (magnus-coord-store--issue
   (magnus-coord-store-event-path event) code message
   (magnus-coord-store-event-writer-id event)
   (magnus-coord-store-event-id event) nil
   (magnus-coord-store-event-writer-sequence event)))

(defun magnus-coord-store--prune-deferred-issue (event failed-event)
  "Return an issue deferring EVENT after FAILED-EVENT stopped pruning."
  (magnus-coord-store--issue
   (magnus-coord-store-event-path event)
   'prune-deferred
   (format "Pruning stopped after event %s failed"
           (magnus-coord-store-event-id failed-event))
   (magnus-coord-store-event-writer-id event)
   (magnus-coord-store-event-id event)
   (magnus-coord-store-event-path failed-event)
   (magnus-coord-store-event-writer-sequence event)
   (magnus-coord-store-event-id failed-event)))

(defun magnus-coord-store--prune-event (snapshot event)
  "Delete revalidated EVENT from SNAPSHOT, returning nil or an issue."
  (let* ((project (magnus-coord-store-snapshot-project-directory snapshot))
         (writer-id (magnus-coord-store-event-writer-id event))
         (event-id (magnus-coord-store-event-id event))
         (path (magnus-coord-store-event-path event))
         (root (magnus-coord-store-directory project))
         (writers (expand-file-name "writers" root))
         (writer-directory
          (magnus-coord-store-writer-directory project writer-id))
         (expected (expand-file-name (concat event-id ".json")
                                     writer-directory)))
    (cond
     ((not (string-equal path expected))
      (magnus-coord-store--prune-issue
       event 'prune-path-mismatch
       "Snapshot event path is outside its canonical writer inbox"))
     ((cl-some (lambda (directory)
                 (or (file-symlink-p directory)
                     (not (file-directory-p directory))))
               (list root writers writer-directory))
      (magnus-coord-store--prune-issue
       event 'prune-unsafe-entry
       "Store ancestry is missing, non-directory, or symlinked"))
     ((not (or (file-exists-p path) (file-symlink-p path)))
      (magnus-coord-store--prune-issue
       event 'prune-missing-entry "Snapshot event no longer exists"))
     ((null (magnus-coord-store-event-file-identifier event))
      (magnus-coord-store--prune-issue
       event 'prune-missing-identity
       "Snapshot event has no captured file identity"))
     (t
      (condition-case prune-error
          (let* ((entry (magnus-coord-store--read-stable-entry path))
                 (bytes (car entry))
                 (attributes (cdr entry))
                 (identifier
                  (file-attribute-file-identifier attributes)))
            (cond
             ((not (equal identifier
                          (magnus-coord-store-event-file-identifier event)))
              (magnus-coord-store--prune-issue
               event 'prune-identity-mismatch
               "Event path now names a different file"))
             ((or (not (equal (secure-hash 'sha256 bytes)
                              (magnus-coord-store-event-content-hash event)))
                  (not (string= bytes
                                (magnus-coord-store-event-bytes event))))
              (magnus-coord-store--prune-issue
               event 'prune-content-mismatch
               "Event content changed after the supplied snapshot"))
             (t
              ;; Narrow the validation/unlink race once more.  Under the store
              ;; contract event IDs are immutable and never reused; this check
              ;; also refuses a cooperative delete/recreate before unlink.
              (let ((final (file-attributes path 'string)))
                (if (or (file-symlink-p path)
                        (not (and final
                                  (null (file-attribute-type final))
                                  (file-regular-p path)))
                        (not (equal
                              (file-attribute-file-identifier final)
                              identifier))
                        (not (= (file-attribute-size final)
                                (file-attribute-size attributes)))
                        (not (equal
                              (file-attribute-modification-time final)
                              (file-attribute-modification-time attributes))))
                    (magnus-coord-store--prune-issue
                     event 'prune-changed-entry
                     "Event changed immediately before deletion")
                  (delete-file path)
                  nil)))))
        (magnus-coord-store-unsafe-entry
         (magnus-coord-store--prune-issue
          event 'prune-unsafe-entry (error-message-string prune-error)))
        (magnus-coord-store-oversized-entry
         (magnus-coord-store--prune-issue
          event 'prune-oversized-entry (error-message-string prune-error)))
        (magnus-coord-store-changed-entry
         (magnus-coord-store--prune-issue
          event 'prune-changed-entry (error-message-string prune-error)))
        (file-missing
         (magnus-coord-store--prune-issue
          event 'prune-missing-entry (error-message-string prune-error)))
        (error
         (magnus-coord-store--prune-issue
          event 'prune-error (error-message-string prune-error))))))))

(defun magnus-coord-store-prune (snapshot keep-ids)
  "Prune revalidated events from SNAPSHOT except KEEP-IDS.

KEEP-IDS is a list or hash-table set of globally unique event IDs.  Only valid
events present in SNAPSHOT are candidates: malformed paths, unknown files, and
events added afterward are never enumerated.  Every deletion revalidates path
confinement, regular/non-symlink status, captured file identity, content hash,
and exact bytes.  Events involved in snapshot identity conflicts are retained.
After the first deletion or revalidation failure, all remaining events are
retained with `prune-deferred' issues; no later filesystem operation is tried.

Return a `magnus-coord-store-prune-result'.  Its issues include the snapshot's
existing issues so callers retain evidence about paths deliberately untouched."
  (unless (magnus-coord-store-snapshot-p snapshot)
    (signal 'magnus-coord-store-error '("Expected an event-store snapshot")))
  (let ((kept-ids (magnus-coord-store--id-set keep-ids))
        (conflicted-ids (magnus-coord-store--conflicted-id-set snapshot))
        (issues (copy-sequence
                 (magnus-coord-store-snapshot-issues snapshot)))
        deleted
        kept
        failed-event)
    (dolist (event (magnus-coord-store-snapshot-events snapshot))
      (cond
       (failed-event
        (push event kept)
        (push (magnus-coord-store--prune-deferred-issue event failed-event)
              issues))
       ((or (gethash (magnus-coord-store-event-id event) kept-ids)
            (gethash (magnus-coord-store-event-id event) conflicted-ids))
        (push event kept))
       (t
        (let ((issue
               (condition-case prune-error
                   (magnus-coord-store--prune-event snapshot event)
                 (error
                  (magnus-coord-store--prune-issue
                   event 'prune-error
                   (error-message-string prune-error))))))
          (if issue
              (progn
                (push event kept)
                (push issue issues)
                (setq failed-event event))
            (push event deleted))))))
    (magnus-coord-store-prune-result--create
     :deleted-events (nreverse deleted)
     :kept-events (nreverse kept)
     :issues (sort issues #'magnus-coord-store--issue-less-p))))

(provide 'magnus-coord-store)
;;; magnus-coord-store.el ends here
