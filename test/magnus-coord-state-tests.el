;;; magnus-coord-state-tests.el --- Coordination state tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-coord-state)

(defconst magnus-coord-state-tests--base
  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(defconst magnus-coord-state-tests--head
  "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")

(defun magnus-coord-state-tests--payload (&rest entries)
  "Return a JSON object containing alternating key/value ENTRIES."
  (let ((payload (make-hash-table :test #'equal)))
    (while entries
      (puthash (pop entries) (pop entries) payload))
    payload))

(cl-defun magnus-coord-state-tests--event
    (writer sequence kind payload &key id name created-at)
  "Construct a store event for reducer tests."
  (let ((event-id (or id (format "%s-%d" writer sequence))))
    (magnus-coord-store-event--create
     :schema 1 :id event-id :writer-id writer
     :writer-name (or name writer) :writer-sequence sequence
     :created-at (or created-at
                     (format "2026-08-04T00:00:%02d.000000Z" sequence))
     :kind kind :payload payload :path (concat "/events/" event-id ".json")
     :content-hash event-id :bytes event-id)))

(defun magnus-coord-state-tests--snapshot (project events &optional issues)
  "Construct a store snapshot for PROJECT containing EVENTS and ISSUES."
  (magnus-coord-store-snapshot--create
   :project-directory (file-name-as-directory project)
   :captured-at (current-time) :candidate-paths nil
   :events events :issues issues))

(defun magnus-coord-state-tests--issue-codes (state)
  "Return policy and storage issue codes from STATE."
  (mapcar #'magnus-coord-state-issue-code
          (magnus-coord-state-issues state)))

(ert-deftest magnus-coord-state-invalid-payload-cannot-replace-valid-state ()
  "Exact schema failures are retained and never become implicit clears."
  (let* ((project default-directory)
         (valid
          (magnus-coord-state-tests--event
           "writer-a" 1 "active.set"
           (magnus-coord-state-tests--payload
            "area" "parser" "status" "working" "files" ["parse.el"])
           :id "valid-active"))
         (invalid
          (magnus-coord-state-tests--event
           "writer-a" 2 "active.set"
           (magnus-coord-state-tests--payload
            "area" "bad" "status" "working" "files" [] "extra" t)
           :id "invalid-active"))
         (state (magnus-coord-state-reduce
                 (magnus-coord-state-tests--snapshot
                  project (list invalid valid)))))
    (should (= (length (magnus-coord-state-active state)) 1))
    (should (equal
             (magnus-coord-state-active-record-event-id
              (car (magnus-coord-state-active state)))
             "valid-active"))
    (should (member 'invalid-payload
                    (magnus-coord-state-tests--issue-codes state)))
    (should (equal (magnus-coord-state-retained-event-ids state)
                   '("invalid-active" "valid-active")))))

(ert-deftest magnus-coord-state-highest-sequence-wins-with-visible-tombstones ()
  "Active and knowledge clears win by sequence and remain exposed for merge."
  (let* ((project default-directory)
         (events
          (list
           (magnus-coord-state-tests--event
            "writer-a" 4 "knowledge.remove"
            (magnus-coord-state-tests--payload
             "section" "discoveries" "entry_id" "parser-note")
            :id "knowledge-clear")
           (magnus-coord-state-tests--event
            "writer-a" 1 "active.set"
            (magnus-coord-state-tests--payload
             "area" "parser" "status" "working" "files" [])
            :id "active-set")
           (magnus-coord-state-tests--event
            "writer-a" 2 "active.clear"
            (magnus-coord-state-tests--payload) :id "active-clear")
           (magnus-coord-state-tests--event
            "writer-a" 3 "knowledge.put"
            (magnus-coord-state-tests--payload
             "section" "discoveries" "entry_id" "parser-note"
             "text" "Old note")
            :id "knowledge-put")))
         (state (magnus-coord-state-reduce
                 (magnus-coord-state-tests--snapshot project events)))
         (active-winner (car (magnus-coord-state-active-winners state)))
         (knowledge-winner
          (car (magnus-coord-state-knowledge-winners state))))
    (should-not (magnus-coord-state-active state))
    (should (eq (magnus-coord-state-active-record-operation active-winner)
                'clear))
    (should (equal (magnus-coord-state-active-record-writer-name active-winner)
                   "writer-a"))
    (should-not (magnus-coord-state-discoveries state))
    (should (eq (magnus-coord-state-knowledge-record-operation knowledge-winner)
                'remove))
    (should (equal (magnus-coord-state-retained-event-ids state)
                   '("active-clear" "knowledge-clear")))))

(ert-deftest magnus-coord-state-sequence-conflict-suppresses-first-winner ()
  "A store-level rejected sequence reuse also taints the accepted event."
  (let* ((project default-directory)
         (accepted
          (magnus-coord-state-tests--event
           "writer-a" 1 "active.set"
           (magnus-coord-state-tests--payload
            "area" "unsafe winner" "status" "working" "files" [])
           :id "accepted"))
         (issue
          (magnus-coord-store-issue--create
           :path "/events/rejected.json" :code 'writer-sequence-conflict
           :message "sequence reused" :writer-id "writer-a"
           :event-id "rejected" :related-path "/events/accepted.json"
           :writer-sequence 1 :related-event-id "accepted"))
         (state (magnus-coord-state-reduce
                 (magnus-coord-state-tests--snapshot
                  project (list accepted) (list issue)))))
    (should-not (magnus-coord-state-active state))
    (should (member 'ambiguous-writer-sequence
                    (magnus-coord-state-tests--issue-codes state)))
    (should (equal (magnus-coord-state-retained-event-ids state)
                   '("accepted" "rejected")))))

(ert-deftest magnus-coord-state-duplicate-id-conflict-suppresses-log ()
  "A conflicting duplicate global ID cannot silently drive policy."
  (let* ((project default-directory)
         (accepted
          (magnus-coord-state-tests--event
           "writer-a" 1 "log.append"
           (magnus-coord-state-tests--payload "message" "maybe corrupt")
           :id "same-id"))
         (issue
          (magnus-coord-store-issue--create
           :path "/events/other.json" :code 'duplicate-conflict
           :message "id reused" :writer-id "writer-b" :event-id "same-id"
           :related-path "/events/same-id.json"))
         (state (magnus-coord-state-reduce
                 (magnus-coord-state-tests--snapshot
                  project (list accepted) (list issue)))))
    (should-not (magnus-coord-state-logs state))
    (should (member 'ambiguous-event-id
                    (magnus-coord-state-tests--issue-codes state)))
    (should (equal (magnus-coord-state-retained-event-ids state)
                   '("same-id")))))

(ert-deftest magnus-coord-state-log-merge-preserves-writer-causality ()
  "K-way merge survives clock rollback and retains only the bounded suffix."
  (let* ((magnus-coord-state-log-limit 2)
         (project default-directory)
         (events
          (list
           (magnus-coord-state-tests--event
            "a" 2 "log.append"
            (magnus-coord-state-tests--payload "message" "a2")
            :id "a2" :created-at "2026-08-04T00:00:01.000000Z")
           (magnus-coord-state-tests--event
            "b" 1 "log.append"
            (magnus-coord-state-tests--payload "message" "b1")
            :id "b1" :created-at "2026-08-04T00:00:05.000000Z")
           (magnus-coord-state-tests--event
            "a" 1 "log.append"
            (magnus-coord-state-tests--payload "message" "a1")
            :id "a1" :created-at "2026-08-04T00:00:10.000000Z")))
         (state (magnus-coord-state-reduce
                 (magnus-coord-state-tests--snapshot project events))))
    (should (equal
             (mapcar #'magnus-coord-state-log-record-event-id
                     (magnus-coord-state-logs state))
             '("a1" "a2")))
    (should (equal (magnus-coord-state-retained-event-ids state)
                   '("a1" "a2" "b1")))))

(ert-deftest magnus-coord-state-knowledge-cap-counts-tombstones ()
  "The newest bounded winners include removals and permit old-key collection."
  (let* ((magnus-coord-state-knowledge-limit 2)
         (project default-directory)
         (put
          (lambda (sequence entry time &optional operation)
            (magnus-coord-state-tests--event
             "writer-a" sequence (or operation "knowledge.put")
             (apply #'magnus-coord-state-tests--payload
                    (append (list "section" "discoveries" "entry_id" entry)
                            (unless operation (list "text" entry))))
             :id (format "%s-%d" entry sequence) :created-at time)))
         (events
          (list
           (funcall put 1 "key-1" "2026-08-04T00:00:01.000000Z")
           (funcall put 2 "key-2" "2026-08-04T00:00:02.000000Z")
           (funcall put 3 "key-3" "2026-08-04T00:00:03.000000Z")
           (funcall put 4 "key-1" "2026-08-04T00:00:04.000000Z"
                    "knowledge.remove")))
         (state (magnus-coord-state-reduce
                 (magnus-coord-state-tests--snapshot project events))))
    (should (equal
             (mapcar #'magnus-coord-state-knowledge-record-entry-id
                     (magnus-coord-state-knowledge-winners state))
             '("key-1" "key-3")))
    (should (eq
             (magnus-coord-state-knowledge-record-operation
              (car (magnus-coord-state-knowledge-winners state)))
             'remove))
    (should (equal
             (mapcar #'magnus-coord-state-knowledge-record-entry-id
                     (magnus-coord-state-discoveries state))
             '("key-3")))
    (should (= (cl-count 'knowledge-truncated
                         (magnus-coord-state-tests--issue-codes state))
               1))
    (should (equal (magnus-coord-state-retained-event-ids state)
                   '("key-1-4" "key-3-3")))))

(ert-deftest magnus-coord-state-sequence-anchors-ignore-time-and-bounds ()
  "Every writer keeps its maximum sequence even when its effect is omitted."
  (let* ((magnus-coord-state-log-limit 0)
         (project default-directory)
         (events
          (list
           (magnus-coord-state-tests--event
            "writer-a" 1 "log.append"
            (magnus-coord-state-tests--payload "message" "new clock")
            :id "a1" :created-at "2026-08-04T00:00:20.000000Z")
           (magnus-coord-state-tests--event
            "writer-a" 2 "log.append"
            (magnus-coord-state-tests--payload "message" "old clock")
            :id "a2" :created-at "2026-08-04T00:00:01.000000Z")
           (magnus-coord-state-tests--event
            "reviewer" 5 "review.ready"
            (magnus-coord-state-tests--payload
             "request_id" "review-1" "checkpoint_token" "round.1:token"
             "base" magnus-coord-state-tests--base
             "head" magnus-coord-state-tests--head)
            :id "review5")))
         (state (magnus-coord-state-reduce
                 (magnus-coord-state-tests--snapshot project events))))
    (should-not (magnus-coord-state-logs state))
    (should (equal (magnus-coord-state-sequence-anchor-event-ids state)
                   '("a2" "review5")))
    (should (equal (magnus-coord-state-retained-event-ids state)
                   '("a2" "review5")))))

(ert-deftest magnus-coord-state-review-and-unknown-events-remain-durable ()
  "Review effects expose source identity; unknown events become retained issues."
  (let* ((project default-directory)
         (review
          (magnus-coord-state-tests--event
           "reviewer-7" 9 "review.ready"
           (magnus-coord-state-tests--payload
            "request_id" "review-2" "checkpoint_token" "round.1:abc"
            "base" magnus-coord-state-tests--base
            "head" magnus-coord-state-tests--head)
           :id "ready-9" :name "Keen Owl"))
         (unknown
          (magnus-coord-state-tests--event
           "writer-z" 1 "future.event"
           (magnus-coord-state-tests--payload "anything" t)
           :id "future-1"))
         (state (magnus-coord-state-reduce
                 (magnus-coord-state-tests--snapshot
                  project (list unknown review))))
         (effect (car (magnus-coord-state-review-ready state))))
    (should (equal (magnus-coord-state-review-effect-event-id effect) "ready-9"))
    (should (equal (magnus-coord-state-review-effect-writer-id effect)
                   "reviewer-7"))
    (should (= (magnus-coord-state-review-effect-writer-sequence effect) 9))
    (should (member 'unknown-kind
                    (magnus-coord-state-tests--issue-codes state)))
    (should (equal (magnus-coord-state-retained-event-ids state)
                   '("future-1" "ready-9")))))

(ert-deftest magnus-coord-state-projection-is-private-generated-and-isolated ()
  "Projection is atomic/private, escapes Markdown, and leaves legacy ingress alone."
  (let ((project (make-temp-file "magnus-coord-state-" t)))
    (unwind-protect
        (let* ((legacy (expand-file-name ".magnus-coord.md" project))
               (event
                (magnus-coord-state-tests--event
                 "writer-a" 1 "active.set"
                 (magnus-coord-state-tests--payload
                  "area" "table|safe" "status" "working" "files" ["a.el"])
                 :id "active-1" :name "Swift Hare"))
               (state (magnus-coord-state-reduce
                       (magnus-coord-state-tests--snapshot project (list event))))
               path text)
          (with-temp-file legacy (insert "legacy sentinel\n"))
          (setq path (magnus-coord-state-write-projection state))
          (setq text (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string)))
          (should (string-match-p "DO NOT EDIT" text))
          (dolist (heading '("## Active Work" "## Discoveries"
                             "## Decisions" "## Log"))
            (should (string-match-p (regexp-quote heading) text)))
          (should (string-match-p (regexp-quote "table\\|safe") text))
          (should (= (logand (file-modes (file-name-directory path)) #o777)
                     #o700))
          (should (= (logand (file-modes path) #o777) #o600))
          (should (equal (with-temp-buffer
                           (insert-file-contents legacy)
                           (buffer-string))
                         "legacy sentinel\n"))
          ;; An unsafe existing target is never followed or replaced.
          (delete-file path)
          (make-symbolic-link legacy path)
          (should-error (magnus-coord-state-write-projection state))
          (delete-file path)
          (make-directory path)
          (should-error (magnus-coord-state-write-projection state)))
      (delete-directory project t))))

(provide 'magnus-coord-state-tests)
;;; magnus-coord-state-tests.el ends here
