;;; magnus-coord-state.el --- Coordination event policy and projection -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Reduce a durable coordination-store snapshot into current coordination
;; state.  This layer owns event payload schemas, causal ordering, retention
;; policy, and the generated human projection.  It does not deliver effects or
;; modify the legacy `.magnus-coord.md' ingress file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magnus-coord-store)

(declare-function magnus-coord-store-directory "magnus-coord-store"
                  (project-directory))

(defgroup magnus-coord-state nil
  "Policy for durable Magnus coordination events."
  :group 'magnus
  :prefix "magnus-coord-state-")

(defcustom magnus-coord-state-log-limit 25
  "Maximum number of recent log entries retained in current state."
  :type 'natnum
  :group 'magnus-coord-state)

(defcustom magnus-coord-state-knowledge-limit 100
  "Maximum number of winning knowledge keys retained in current state.
Both visible entries and removal tombstones count toward this bound."
  :type 'natnum
  :group 'magnus-coord-state)

(defvar magnus-coord-state-active-record-visible-function nil
  "Optional predicate used when projecting active-work records.
The function receives an active record and its project directory.  A nil
value keeps the reducer completely event-derived.  Magnus binds this boundary
to its live instance registry so a crashed, archived, or moved agent does not
remain visible merely because its last immutable event was `active.set'.")

(define-error 'magnus-coord-state-invalid-payload
  "Invalid Magnus coordination event payload")

(cl-defstruct (magnus-coord-state-active-record
               (:constructor magnus-coord-state-active-record--create)
               (:copier nil))
  "Winning active-work event for one durable writer identity."
  writer-id writer-name writer-sequence event-id created-at operation
  area status files)

(cl-defstruct (magnus-coord-state-knowledge-record
               (:constructor magnus-coord-state-knowledge-record--create)
               (:copier nil))
  "Winning knowledge event for one writer, section, and entry identity."
  writer-id writer-name writer-sequence event-id created-at operation
  section entry-id text)

(cl-defstruct (magnus-coord-state-log-record
               (:constructor magnus-coord-state-log-record--create)
               (:copier nil))
  "One causally ordered coordination log entry."
  writer-id writer-name writer-sequence event-id created-at message)

(cl-defstruct (magnus-coord-state-review-effect
               (:constructor magnus-coord-state-review-effect--create)
               (:copier nil))
  "One unresolved review-ready effect with durable source identity."
  writer-id writer-name writer-sequence event-id created-at
  request-id checkpoint-token base head)

(cl-defstruct (magnus-coord-state-issue
               (:constructor magnus-coord-state-issue--create)
               (:copier nil))
  "One isolated storage or policy problem observed during reduction."
  code message event-id writer-id writer-sequence kind store-issue)

(cl-defstruct (magnus-coord-state
               (:constructor magnus-coord-state--create)
               (:copier nil))
  "Current state derived from one immutable store snapshot."
  project-directory snapshot active active-winners discoveries decisions
  knowledge-winners logs log-effects review-ready issues retained-event-ids
  sequence-anchor-event-ids)

(defun magnus-coord-state-visible-active (state)
  "Return active records from STATE that belong in the human projection.
The durable winning records remain available through `active-winners' even
when Magnus's lifecycle overlay temporarily hides one from `current.md'."
  (let ((records (magnus-coord-state-active state))
        (predicate magnus-coord-state-active-record-visible-function)
        (project (magnus-coord-state-project-directory state)))
    (if (functionp predicate)
        (cl-remove-if-not
         (lambda (record) (funcall predicate record project)) records)
      records)))

(defun magnus-coord-state--payload-error (format-string &rest arguments)
  "Signal a payload error formatted with FORMAT-STRING and ARGUMENTS."
  (signal 'magnus-coord-state-invalid-payload
          (list (apply #'format format-string arguments))))

(defun magnus-coord-state--fields (payload)
  "Return sorted string keys from object PAYLOAD."
  (let (fields)
    (when (hash-table-p payload)
      (maphash (lambda (key _value) (push key fields)) payload))
    (sort fields #'string<)))

(defun magnus-coord-state--expect-fields (payload expected)
  "Require PAYLOAD to contain exactly EXPECTED string keys."
  (unless (and (hash-table-p payload)
               (equal (magnus-coord-state--fields payload)
                      (sort (copy-sequence expected) #'string<)))
    (magnus-coord-state--payload-error
     "payload must contain exactly: %s" (string-join expected ", "))))

(defun magnus-coord-state--string (payload key maximum &optional empty-ok)
  "Read bounded string KEY from PAYLOAD, with byte bound MAXIMUM.
Permit the empty string only when EMPTY-OK is non-nil."
  (let ((value (gethash key payload)))
    (unless (and (stringp value)
                 (<= (string-bytes value) maximum)
                 (or empty-ok (not (string-empty-p value))))
      (magnus-coord-state--payload-error "invalid %s" key))
    value))

(defun magnus-coord-state--identity (payload key)
  "Read safe bounded identity KEY from PAYLOAD."
  (let ((value (magnus-coord-state--string payload key 256)))
    (unless (string-match-p "\\`[[:alnum:]_.:-]+\\'" value)
      (magnus-coord-state--payload-error "invalid %s" key))
    value))

(defun magnus-coord-state--files (payload)
  "Read a bounded JSON string array from PAYLOAD."
  (let ((value (gethash "files" payload)))
    (unless (and (vectorp value) (<= (length value) 256))
      (magnus-coord-state--payload-error "invalid files"))
    (mapcar
     (lambda (file)
       (unless (and (stringp file) (not (string-empty-p file))
                    (<= (string-bytes file) 1024))
         (magnus-coord-state--payload-error "invalid file name"))
       file)
     (append value nil))))

(defun magnus-coord-state--parse-payload (event)
  "Validate EVENT's exact kind-specific payload and return a plist."
  (let ((kind (magnus-coord-store-event-kind event))
        (payload (magnus-coord-store-event-payload event)))
    (pcase kind
      ("log.append"
       (magnus-coord-state--expect-fields payload '("message"))
       (list :message (magnus-coord-state--string payload "message" 8192)))
      ("active.set"
       (magnus-coord-state--expect-fields payload '("area" "status" "files"))
       (list :area (magnus-coord-state--string payload "area" 2048)
             :status (magnus-coord-state--string payload "status" 128)
             :files (magnus-coord-state--files payload)))
      ("active.clear"
       (magnus-coord-state--expect-fields payload nil)
       nil)
      ("knowledge.put"
       (magnus-coord-state--expect-fields
        payload '("section" "entry_id" "text"))
       (let ((section (magnus-coord-state--string payload "section" 16)))
         (unless (member section '("discoveries" "decisions"))
           (magnus-coord-state--payload-error "invalid section"))
         (list :section section
               :entry-id (magnus-coord-state--identity payload "entry_id")
               :text (magnus-coord-state--string payload "text" 16384))))
      ("knowledge.remove"
       (magnus-coord-state--expect-fields payload '("section" "entry_id"))
       (let ((section (magnus-coord-state--string payload "section" 16)))
         (unless (member section '("discoveries" "decisions"))
           (magnus-coord-state--payload-error "invalid section"))
         (list :section section
               :entry-id (magnus-coord-state--identity payload "entry_id"))))
      ("review.ready"
       (magnus-coord-state--expect-fields
        payload '("request_id" "checkpoint_token" "base" "head"))
       (let ((base (magnus-coord-state--string payload "base" 64))
             (head (magnus-coord-state--string payload "head" 64)))
         (unless (and (string-match-p "\\`[[:xdigit:]]\\{40,64\\}\\'" base)
                      (string-match-p "\\`[[:xdigit:]]\\{40,64\\}\\'" head))
           (magnus-coord-state--payload-error "invalid Git object id"))
         (list :request-id (magnus-coord-state--identity payload "request_id")
               :checkpoint-token
               (magnus-coord-state--identity payload "checkpoint_token")
               :base (downcase base) :head (downcase head))))
      (_ :unknown))))

(defun magnus-coord-state--event-key (event)
  "Return EVENT's writer-sequence identity."
  (cons (magnus-coord-store-event-writer-id event)
        (magnus-coord-store-event-writer-sequence event)))

(defun magnus-coord-state--store-issue-field (issue accessor)
  "Read ACCESSOR from store ISSUE when that accessor exists."
  (when (fboundp accessor) (funcall accessor issue)))

(defun magnus-coord-state--store-state-issue (issue)
  "Convert store ISSUE to a state issue."
  (magnus-coord-state-issue--create
   :code (magnus-coord-store-issue-code issue)
   :message (magnus-coord-store-issue-message issue)
   :event-id (magnus-coord-store-issue-event-id issue)
   :writer-id (magnus-coord-store-issue-writer-id issue)
   :writer-sequence
   (magnus-coord-state--store-issue-field
    issue 'magnus-coord-store-issue-writer-sequence)
   :store-issue issue))

(defun magnus-coord-state--record-common (event)
  "Return common constructor keyword arguments for EVENT."
  (list :writer-id (magnus-coord-store-event-writer-id event)
        :writer-name (magnus-coord-store-event-writer-name event)
        :writer-sequence (magnus-coord-store-event-writer-sequence event)
        :event-id (magnus-coord-store-event-id event)
        :created-at (magnus-coord-store-event-created-at event)))

(defun magnus-coord-state--values (table)
  "Return values from hash TABLE."
  (let (values)
    (maphash (lambda (_key value) (push value values)) table)
    values))

(defun magnus-coord-state--keys (table)
  "Return keys from hash TABLE."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) table)
    keys))

(defun magnus-coord-state--record-less-p (left right)
  "Order state records LEFT and RIGHT deterministically by identity."
  (let ((left-key (format "%s\0%s\0%s\0%s"
                          (magnus-coord-state-knowledge-record-writer-id left)
                          (magnus-coord-state-knowledge-record-section left)
                          (magnus-coord-state-knowledge-record-entry-id left)
                          (magnus-coord-state-knowledge-record-event-id left)))
        (right-key (format "%s\0%s\0%s\0%s"
                           (magnus-coord-state-knowledge-record-writer-id right)
                           (magnus-coord-state-knowledge-record-section right)
                           (magnus-coord-state-knowledge-record-entry-id right)
                           (magnus-coord-state-knowledge-record-event-id right))))
    (string< left-key right-key)))

(defun magnus-coord-state--knowledge-time-less-p (left right)
  "Order knowledge winners LEFT and RIGHT from oldest to newest."
  (let ((left-key
         (format "%s\0%s\0%020d\0%s"
                 (magnus-coord-state-knowledge-record-created-at left)
                 (magnus-coord-state-knowledge-record-writer-id left)
                 (magnus-coord-state-knowledge-record-writer-sequence left)
                 (magnus-coord-state-knowledge-record-event-id left)))
        (right-key
         (format "%s\0%s\0%020d\0%s"
                 (magnus-coord-state-knowledge-record-created-at right)
                 (magnus-coord-state-knowledge-record-writer-id right)
                 (magnus-coord-state-knowledge-record-writer-sequence right)
                 (magnus-coord-state-knowledge-record-event-id right))))
    (string< left-key right-key)))

(defun magnus-coord-state--log-head-less-p (left right)
  "Order currently eligible causal log heads LEFT and RIGHT."
  (let ((left-key (format "%s\0%s\0%020d\0%s"
                          (magnus-coord-state-log-record-created-at left)
                          (magnus-coord-state-log-record-writer-id left)
                          (magnus-coord-state-log-record-writer-sequence left)
                          (magnus-coord-state-log-record-event-id left)))
        (right-key (format "%s\0%s\0%020d\0%s"
                           (magnus-coord-state-log-record-created-at right)
                           (magnus-coord-state-log-record-writer-id right)
                           (magnus-coord-state-log-record-writer-sequence right)
                           (magnus-coord-state-log-record-event-id right))))
    (string< left-key right-key)))

(defun magnus-coord-state--merge-logs (logs)
  "K-way merge LOGS while preserving every writer's sequence order."
  (let ((groups (make-hash-table :test #'equal)) queues merged)
    (dolist (entry logs)
      (push entry (gethash (magnus-coord-state-log-record-writer-id entry)
                           groups)))
    (maphash
     (lambda (writer entries)
       (push (cons writer
                   (sort entries
                         (lambda (left right)
                           (< (magnus-coord-state-log-record-writer-sequence left)
                              (magnus-coord-state-log-record-writer-sequence right)))))
             queues))
     groups)
    (setq queues (sort queues (lambda (left right) (string< (car left) (car right)))))
    (while queues
      (let* ((winner
              (car (sort (copy-sequence queues)
                         (lambda (left right)
                           (magnus-coord-state--log-head-less-p
                            (cadr left) (cadr right))))))
             (entry (cadr winner)))
        (push entry merged)
        (setcdr winner (cddr winner))
        (unless (cdr winner) (setq queues (delq winner queues)))))
    (nreverse merged)))

(defun magnus-coord-state--issue-less-p (left right)
  "Order state issues LEFT and RIGHT deterministically."
  (string<
   (format "%s\0%020d\0%s\0%s\0%s"
           (or (magnus-coord-state-issue-writer-id left) "")
           (or (magnus-coord-state-issue-writer-sequence left) 0)
           (or (magnus-coord-state-issue-event-id left) "")
           (magnus-coord-state-issue-code left)
           (magnus-coord-state-issue-message left))
   (format "%s\0%020d\0%s\0%s\0%s"
           (or (magnus-coord-state-issue-writer-id right) "")
           (or (magnus-coord-state-issue-writer-sequence right) 0)
           (or (magnus-coord-state-issue-event-id right) "")
           (magnus-coord-state-issue-code right)
           (magnus-coord-state-issue-message right))))

;;;###autoload
(defun magnus-coord-state-reduce (snapshot)
  "Strictly reduce coordination-store SNAPSHOT into current state."
  (let ((events
         (sort
          (copy-sequence (magnus-coord-store-snapshot-events snapshot))
          (lambda (left right)
            (let ((left-writer (magnus-coord-store-event-writer-id left))
                  (right-writer (magnus-coord-store-event-writer-id right)))
              (if (string= left-writer right-writer)
                  (let ((left-sequence
                         (magnus-coord-store-event-writer-sequence left))
                        (right-sequence
                         (magnus-coord-store-event-writer-sequence right)))
                    (if (= left-sequence right-sequence)
                        (string< (magnus-coord-store-event-id left)
                                 (magnus-coord-store-event-id right))
                      (< left-sequence right-sequence)))
                (string< left-writer right-writer))))))
        (store-issues (magnus-coord-store-snapshot-issues snapshot))
        (sequence-groups (make-hash-table :test #'equal))
        (ambiguous (make-hash-table :test #'equal))
        (ambiguous-ids (make-hash-table :test #'equal))
        (retain (make-hash-table :test #'equal))
        (anchors (make-hash-table :test #'equal))
        (active (make-hash-table :test #'equal))
        (knowledge (make-hash-table :test #'equal))
        logs reviews issues)
    (dolist (event events)
      (push event (gethash (magnus-coord-state--event-key event)
                           sequence-groups)))
    (maphash
     (lambda (key grouped)
       (when (> (length grouped) 1)
         (puthash key t ambiguous)
         (dolist (event grouped)
           (puthash (magnus-coord-store-event-id event) t retain))))
     sequence-groups)
    ;; Store rejects the deterministic second event.  Its structured conflict
    ;; issue restores enough identity to suppress the retained first as well.
    (dolist (issue store-issues)
      (push (magnus-coord-state--store-state-issue issue) issues)
      (when (eq (magnus-coord-store-issue-code issue)
                'writer-sequence-conflict)
        (let ((key (cons (magnus-coord-store-issue-writer-id issue)
                         (magnus-coord-state--store-issue-field
                          issue 'magnus-coord-store-issue-writer-sequence)))
              (related (magnus-coord-state--store-issue-field
                        issue 'magnus-coord-store-issue-related-event-id)))
          (puthash key t ambiguous)
          (when (magnus-coord-store-issue-event-id issue)
            (puthash (magnus-coord-store-issue-event-id issue) t retain))
          (when related (puthash related t retain))))
      (when (eq (magnus-coord-store-issue-code issue) 'duplicate-conflict)
        (let ((id (magnus-coord-store-issue-event-id issue)))
          (when id
            (puthash id t ambiguous-ids)
            (puthash id t retain)))))
    (dolist (event events)
      (let* ((kind (magnus-coord-store-event-kind event))
             (id (magnus-coord-store-event-id event))
             (key (magnus-coord-state--event-key event))
             (sequence-ambiguous (gethash key ambiguous))
             (id-ambiguous (gethash id ambiguous-ids))
             (is-ambiguous (or sequence-ambiguous id-ambiguous))
             parsed valid)
        (when is-ambiguous
          (puthash id t retain)
          (push (magnus-coord-state-issue--create
                 :code (if sequence-ambiguous
                           'ambiguous-writer-sequence
                         'ambiguous-event-id)
                 :message (if sequence-ambiguous
                              "writer sequence is used by multiple event IDs"
                            "event ID has conflicting immutable content")
                 :event-id id
                 :writer-id (car key) :writer-sequence (cdr key) :kind kind)
                issues))
        (unless is-ambiguous
          ;; EVENTS are sorted by writer sequence, so replacement records the
          ;; highest surviving sequence independent of wall-clock movement.
          (puthash (car key) id anchors))
        (condition-case error-data
            (progn
              (setq parsed (magnus-coord-state--parse-payload event))
              (setq valid t))
          (magnus-coord-state-invalid-payload
           (puthash id t retain)
           (push (magnus-coord-state-issue--create
                  :code 'invalid-payload :message (error-message-string error-data)
                  :event-id id :writer-id (car key) :writer-sequence (cdr key)
                  :kind kind)
                 issues)))
        (cond
         ((not valid))
         ((eq parsed :unknown)
          (puthash id t retain)
          (push (magnus-coord-state-issue--create
                 :code 'unknown-kind :message (format "unknown event kind: %s" kind)
                 :event-id id :writer-id (car key) :writer-sequence (cdr key)
                 :kind kind)
                issues))
         ((not is-ambiguous)
            (pcase kind
              ((or "active.set" "active.clear")
               (puthash
                (car key)
                (apply #'magnus-coord-state-active-record--create
                       (append (magnus-coord-state--record-common event)
                               (list :operation (if (string= kind "active.set")
                                                    'set 'clear)
                                     :area (plist-get parsed :area)
                                     :status (plist-get parsed :status)
                                     :files (plist-get parsed :files))))
                active))
              ((or "knowledge.put" "knowledge.remove")
               (let ((knowledge-key
                      (list (car key) (plist-get parsed :section)
                            (plist-get parsed :entry-id))))
                 (puthash
                  knowledge-key
                  (apply #'magnus-coord-state-knowledge-record--create
                         (append (magnus-coord-state--record-common event)
                                 (list :operation
                                       (if (string= kind "knowledge.put") 'put 'remove)
                                       :section (plist-get parsed :section)
                                       :entry-id (plist-get parsed :entry-id)
                                       :text (plist-get parsed :text))))
                  knowledge)))
              ("log.append"
               (push (apply #'magnus-coord-state-log-record--create
                            (append (magnus-coord-state--record-common event)
                                    (list :message (plist-get parsed :message))))
                     logs))
              ("review.ready"
               (puthash id t retain)
               (push (apply #'magnus-coord-state-review-effect--create
                            (append (magnus-coord-state--record-common event)
                                    (list :request-id (plist-get parsed :request-id)
                                          :checkpoint-token
                                          (plist-get parsed :checkpoint-token)
                                          :base (plist-get parsed :base)
                                          :head (plist-get parsed :head))))
                     reviews)))))))
    (let* ((active-winners
            (sort (magnus-coord-state--values active)
                  (lambda (left right)
                    (string< (magnus-coord-state-active-record-writer-id left)
                             (magnus-coord-state-active-record-writer-id right)))))
           (visible-active
            (cl-remove-if-not
             (lambda (entry)
               (eq (magnus-coord-state-active-record-operation entry) 'set))
             active-winners))
           (all-knowledge-winners
            (sort (magnus-coord-state--values knowledge)
                  #'magnus-coord-state--knowledge-time-less-p))
           (knowledge-limit (max 0 magnus-coord-state-knowledge-limit))
           (knowledge-dropped
            (max 0 (- (length all-knowledge-winners) knowledge-limit)))
           (knowledge-winners
            (sort (if (> knowledge-dropped 0)
                      (nthcdr knowledge-dropped all-knowledge-winners)
                    all-knowledge-winners)
                  #'magnus-coord-state--record-less-p))
           (discoveries
            (cl-remove-if-not
             (lambda (entry)
               (and (eq (magnus-coord-state-knowledge-record-operation entry) 'put)
                    (string= (magnus-coord-state-knowledge-record-section entry)
                             "discoveries")))
             knowledge-winners))
           (decisions
            (cl-remove-if-not
             (lambda (entry)
               (and (eq (magnus-coord-state-knowledge-record-operation entry) 'put)
                    (string= (magnus-coord-state-knowledge-record-section entry)
                             "decisions")))
             knowledge-winners))
           (merged (magnus-coord-state--merge-logs logs))
           (limit (max 0 magnus-coord-state-log-limit))
           (recent (if (> (length merged) limit)
                       (nthcdr (- (length merged) limit) merged)
                     merged))
           (anchor-ids (sort (magnus-coord-state--values anchors) #'string<)))
      (when (> knowledge-dropped 0)
        (push (magnus-coord-state-issue--create
               :code 'knowledge-truncated
               :message (format "dropped %d older knowledge keys at limit %d"
                                knowledge-dropped knowledge-limit))
              issues))
      (dolist (entry active-winners)
        (puthash (magnus-coord-state-active-record-event-id entry) t retain))
      (dolist (entry knowledge-winners)
        (puthash (magnus-coord-state-knowledge-record-event-id entry) t retain))
      (dolist (entry recent)
        (puthash (magnus-coord-state-log-record-event-id entry) t retain))
      (dolist (id anchor-ids) (puthash id t retain))
      (magnus-coord-state--create
       :project-directory
       (magnus-coord-store-snapshot-project-directory snapshot)
       :snapshot snapshot
       :active visible-active :active-winners active-winners
       :discoveries discoveries :decisions decisions
       :knowledge-winners knowledge-winners :logs recent :log-effects merged
       :review-ready (nreverse reviews)
       :issues (sort issues #'magnus-coord-state--issue-less-p)
       :sequence-anchor-event-ids anchor-ids
       :retained-event-ids
       (sort (magnus-coord-state--keys retain) #'string<)))))

(defun magnus-coord-state--markdown (value)
  "Flatten and escape VALUE for generated Markdown."
  (replace-regexp-in-string
   "|" "\\|"
   (replace-regexp-in-string "[\r\n\t]+" " " (or value "")) t t))

(defun magnus-coord-state--projection-text (state)
  "Return deterministic generated Markdown for STATE."
  (with-temp-buffer
    (insert "# Magnus Coordination (Generated)\n\n")
    (insert "<!-- Generated from immutable Magnus events. DO NOT EDIT. -->\n\n")
    (insert "## Active Work\n\n")
    (insert "| Agent | Area | Status | Files |\n")
    (insert "|-------|------|--------|-------|\n")
    (dolist (entry (magnus-coord-state-visible-active state))
      (insert (format "| %s | %s | %s | %s |\n"
                      (magnus-coord-state--markdown
                       (magnus-coord-state-active-record-writer-name entry))
                      (magnus-coord-state--markdown
                       (magnus-coord-state-active-record-area entry))
                      (magnus-coord-state--markdown
                       (magnus-coord-state-active-record-status entry))
                      (mapconcat #'magnus-coord-state--markdown
                                 (magnus-coord-state-active-record-files entry)
                                 ", "))))
    (dolist (section
             (list (cons "Discoveries" (magnus-coord-state-discoveries state))
                   (cons "Decisions" (magnus-coord-state-decisions state))))
      (insert (format "\n## %s\n\n" (car section)))
      (dolist (entry (cdr section))
        (insert (format "- %s (%s)\n"
                        (magnus-coord-state--markdown
                         (magnus-coord-state-knowledge-record-text entry))
                        (magnus-coord-state--markdown
                         (magnus-coord-state-knowledge-record-writer-name entry))))))
    (insert "\n## Log\n\n")
    (dolist (entry (magnus-coord-state-logs state))
      (insert (format "[%s] %s: %s\n"
                      (magnus-coord-state-log-record-created-at entry)
                      (magnus-coord-state--markdown
                       (magnus-coord-state-log-record-writer-name entry))
                      (magnus-coord-state--markdown
                       (magnus-coord-state-log-record-message entry)))))
    (buffer-string)))

;;;###autoload
(defun magnus-coord-state-write-projection (state)
  "Atomically write a private `.magnus-coord/current.md' for STATE.
Signal all validation and filesystem failures to the orchestration caller."
  (let* ((root (magnus-coord-store-directory
                (magnus-coord-state-project-directory state)))
         (target (expand-file-name "current.md" root))
         temporary)
    (when (file-symlink-p root)
      (error "Refusing symlinked coordination store: %s" root))
    (unless (file-exists-p root) (make-directory root nil))
    (when (or (file-symlink-p root) (not (file-directory-p root)))
      (error "Coordination store is not a safe directory: %s" root))
    (when (or (file-symlink-p target)
              (and (file-exists-p target) (not (file-regular-p target))))
      (error "Refusing unsafe coordination projection target: %s" target))
    (set-file-modes root #o700)
    (setq temporary (make-temp-file (expand-file-name ".current-tmp-" root)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (magnus-coord-state--projection-text state)
                          nil temporary nil 'quiet))
          (set-file-modes temporary #o600)
          (rename-file temporary target t)
          (setq temporary nil)
          target)
      (when temporary (ignore-errors (delete-file temporary))))))

(provide 'magnus-coord-state)
;;; magnus-coord-state.el ends here
