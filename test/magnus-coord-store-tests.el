;;; magnus-coord-store-tests.el --- Coordination event-store tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'magnus-coord-store)

(defmacro magnus-coord-store-tests--with-project (&rest body)
  "Run BODY with `project' bound to a temporary project directory."
  (declare (indent 0) (debug t))
  `(let ((project (make-temp-file "magnus-coord-store-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory project t))))

(defun magnus-coord-store-tests--payload (text)
  "Return an object payload containing TEXT."
  `((text . ,text)))

(defun magnus-coord-store-tests--write-bytes (path bytes)
  "Write exact BYTES to PATH, creating its parent directory."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'no-conversion))
    (write-region bytes nil path nil 'quiet)))

(defun magnus-coord-store-tests--event-bytes
    (id writer-id writer-name writer-sequence created-at kind payload)
  "Return schema-1 JSON bytes for the supplied event fields."
  (let ((object (make-hash-table :test #'equal)))
    (puthash "schema" 1 object)
    (puthash "id" id object)
    (puthash "writer_id" writer-id object)
    (puthash "writer_name" writer-name object)
    (puthash "writer_sequence" writer-sequence object)
    (puthash "created_at" created-at object)
    (puthash "kind" kind object)
    (puthash "payload" payload object)
    (encode-coding-string
     (concat (json-serialize object :null-object nil
                             :false-object :json-false)
             "\n")
     'utf-8-unix t)))

(defun magnus-coord-store-tests--event-path (project writer-id event-id)
  "Return EVENT-ID's canonical path for WRITER-ID in PROJECT."
  (expand-file-name
   (concat event-id ".json")
   (magnus-coord-store-writer-directory project writer-id)))

(cl-defun magnus-coord-store-tests--write-event
    (project writer-id writer-name kind payload
             &key event-id created-at writer-sequence)
  "Write one schema-1 fixture event beneath PROJECT and return its path."
  (let* ((path (magnus-coord-store-tests--event-path
                project writer-id event-id))
         (bytes (magnus-coord-store-tests--event-bytes
                 event-id writer-id writer-name writer-sequence
                 created-at kind payload)))
    (magnus-coord-store-ensure-writer-directory project writer-id)
    (magnus-coord-store-tests--write-bytes path bytes)
    (set-file-modes path #o600)
    path))

(ert-deftest magnus-coord-store-reads-schema-1-event ()
  "A valid event is path-bound and survives a snapshot."
  (magnus-coord-store-tests--with-project
    (magnus-coord-store-tests--write-event
     project "writer-1" "Swift Hare" "log.append"
     (magnus-coord-store-tests--payload "héllo")
     :event-id "event-1"
     :writer-sequence 1
     :created-at "2026-08-04T01:02:03.000001Z")
    (let* ((path (magnus-coord-store-tests--event-path
                  project "writer-1" "event-1"))
           (snapshot (magnus-coord-store-snapshot project))
           (loaded (car (magnus-coord-store-snapshot-events snapshot))))
      (should (file-regular-p path))
      (should (string-suffix-p
               ".magnus-coord/writers/writer-1/event-1.json" path))
      (should (equal (magnus-coord-store-snapshot-candidate-paths snapshot)
                     (list path)))
      (should-not (magnus-coord-store-snapshot-issues snapshot))
      (should (= (magnus-coord-store-event-schema loaded) 1))
      (should (equal (magnus-coord-store-event-id loaded) "event-1"))
      (should (equal (magnus-coord-store-event-writer-id loaded) "writer-1"))
      (should (equal (magnus-coord-store-event-writer-name loaded) "Swift Hare"))
      (should (= (magnus-coord-store-event-writer-sequence loaded) 1))
      (should (equal (gethash "text"
                              (magnus-coord-store-event-payload loaded))
                     "héllo"))
      (should (equal (magnus-coord-store-event-content-hash loaded)
                     (secure-hash
                      'sha256 (magnus-coord-store-event-bytes loaded)))))))

(ert-deftest magnus-coord-store-snapshot-is-deterministic-and-read-once ()
  "A snapshot captures sorted paths, reads each once, and orders by metadata."
  (magnus-coord-store-tests--with-project
    (magnus-coord-store-tests--write-event
     project "writer-z" "Zed" "log.append"
     (magnus-coord-store-tests--payload "later")
     :event-id "event-z" :writer-sequence 1
     :created-at "2026-08-04T02:00:00.000000Z")
    (magnus-coord-store-tests--write-event
     project "writer-b" "Bee" "log.append"
     (magnus-coord-store-tests--payload "tie b")
     :event-id "event-b" :writer-sequence 1
     :created-at "2026-08-04T01:00:00.000000Z")
    (magnus-coord-store-tests--write-event
     project "writer-a" "Aye" "log.append"
     (magnus-coord-store-tests--payload "tie a")
     :event-id "event-a" :writer-sequence 1
     :created-at "2026-08-04T01:00:00.000000Z")
    (let ((original-read (symbol-function 'insert-file-contents-literally))
          (reads (make-hash-table :test #'equal))
          snapshot)
      (cl-letf (((symbol-function 'insert-file-contents-literally)
                 (lambda (file &rest arguments)
                   (puthash file (1+ (gethash file reads 0)) reads)
                   (apply original-read file arguments))))
        (setq snapshot (magnus-coord-store-snapshot project)))
      (should
       (equal (magnus-coord-store-snapshot-candidate-paths snapshot)
              (sort (copy-sequence
                     (magnus-coord-store-snapshot-candidate-paths snapshot))
                    #'string<)))
      (dolist (path (magnus-coord-store-snapshot-candidate-paths snapshot))
        (should (= (gethash path reads 0) 1)))
      (should
       (equal
        (mapcar #'magnus-coord-store-event-id
                (magnus-coord-store-snapshot-events snapshot))
        '("event-a" "event-b" "event-z")))
      (should-not (magnus-coord-store-snapshot-issues snapshot)))))

(ert-deftest magnus-coord-store-malformed-entry-does-not-hide-siblings ()
  "Malformed JSON and path debris are isolated from valid sibling events."
  (magnus-coord-store-tests--with-project
    (magnus-coord-store-tests--write-event
     project "writer" "Writer" "log.append"
     (magnus-coord-store-tests--payload "first")
     :event-id "good-a" :writer-sequence 1
     :created-at "2026-08-04T01:00:00.000000Z")
    (magnus-coord-store-tests--write-event
     project "writer" "Writer" "log.append"
     (magnus-coord-store-tests--payload "second")
     :event-id "good-b" :writer-sequence 2
     :created-at "2026-08-04T02:00:00.000000Z")
    (let* ((writer-directory
            (magnus-coord-store-writer-directory project "writer"))
           (bad-json (expand-file-name "bad.json" writer-directory))
           (bad-name (expand-file-name "not-an-event.txt" writer-directory)))
      (magnus-coord-store-tests--write-bytes bad-json "{ definitely-not-json")
      (magnus-coord-store-tests--write-bytes bad-name "ignored bytes")
      (let ((snapshot (magnus-coord-store-snapshot project)))
        (should
         (equal
          (mapcar #'magnus-coord-store-event-id
                  (magnus-coord-store-snapshot-events snapshot))
          '("good-a" "good-b")))
        (should
         (equal (mapcar #'magnus-coord-store-issue-code
                        (magnus-coord-store-snapshot-issues snapshot))
                '(invalid-json invalid-event-path)))))))

(ert-deftest magnus-coord-store-rejects-unsafe-and-oversized-entries ()
  "Symlinks, directories, and oversized files are issues, not content reads."
  (magnus-coord-store-tests--with-project
    (let ((magnus-coord-store-max-event-bytes 512))
      (magnus-coord-store-tests--write-event
       project "writer" "Writer" "log.append"
       (magnus-coord-store-tests--payload "good")
       :event-id "good" :writer-sequence 1
       :created-at "2026-08-04T01:00:00.000000Z")
      (let* ((writer-directory
              (magnus-coord-store-writer-directory project "writer"))
             (outside (expand-file-name "outside.json" project))
             (link (expand-file-name "linked.json" writer-directory))
             (nested (expand-file-name "nested.json" writer-directory))
             (oversized (expand-file-name "oversized.json" writer-directory)))
        (magnus-coord-store-tests--write-bytes outside "{}")
        (make-symbolic-link outside link)
        (make-directory nested)
        (magnus-coord-store-tests--write-bytes
         oversized (make-string 513 ?x))
        (let ((snapshot (magnus-coord-store-snapshot project)))
          (should
           (equal (mapcar #'magnus-coord-store-event-id
                          (magnus-coord-store-snapshot-events snapshot))
                  '("good")))
          (should
           (equal
            (sort (mapcar #'magnus-coord-store-issue-code
                          (magnus-coord-store-snapshot-issues snapshot))
                  (lambda (left right)
                    (string< (symbol-name left) (symbol-name right))))
            '(oversized-entry unsafe-entry unsafe-entry))))))))

(ert-deftest magnus-coord-store-binds-envelope-identity-to-path ()
  "Envelope writer and event IDs must agree with their containing path."
  (magnus-coord-store-tests--with-project
    (let* ((writer-directory
            (magnus-coord-store-writer-directory project "writer-a"))
           (wrong-id (expand-file-name "expected.json" writer-directory))
           (wrong-writer (expand-file-name "writer-event.json"
                                           writer-directory)))
      (magnus-coord-store-tests--write-bytes
       wrong-id
       (magnus-coord-store-tests--event-bytes
        "other" "writer-a" "Aye" 1 "2026-08-04T01:00:00.000000Z"
        "log.append" (magnus-coord-store-tests--payload "x")))
      (magnus-coord-store-tests--write-bytes
       wrong-writer
       (magnus-coord-store-tests--event-bytes
        "writer-event" "writer-b" "Bee" 1
        "2026-08-04T01:00:00.000000Z"
        "log.append" (magnus-coord-store-tests--payload "x")))
      (let ((snapshot (magnus-coord-store-snapshot project)))
        (should-not (magnus-coord-store-snapshot-events snapshot))
        (should (= (length (magnus-coord-store-snapshot-issues snapshot)) 2))
        (should
         (cl-every
          (lambda (issue)
            (eq (magnus-coord-store-issue-code issue) 'invalid-event))
          (magnus-coord-store-snapshot-issues snapshot)))))))

(ert-deftest magnus-coord-store-surfaces-global-id-conflicts ()
  "The deterministic first valid event wins when two writers reuse one ID."
  (magnus-coord-store-tests--with-project
    (dolist (description
             '(("writer-a" "Aye" "first")
               ("writer-b" "Bee" "second")))
      (pcase-let ((`(,writer-id ,writer-name ,text) description))
        (let ((path (magnus-coord-store-tests--event-path
                     project writer-id "shared-id")))
          (magnus-coord-store-tests--write-bytes
           path
           (magnus-coord-store-tests--event-bytes
            "shared-id" writer-id writer-name 1
            "2026-08-04T01:00:00.000000Z" "log.append"
            (magnus-coord-store-tests--payload text))))))
    (let* ((snapshot (magnus-coord-store-snapshot project))
           (event (car (magnus-coord-store-snapshot-events snapshot)))
           (issue (car (magnus-coord-store-snapshot-issues snapshot))))
      (should (= (length (magnus-coord-store-snapshot-events snapshot)) 1))
      (should (equal (magnus-coord-store-event-writer-id event) "writer-a"))
      (should (= (length (magnus-coord-store-snapshot-issues snapshot)) 1))
      (should (eq (magnus-coord-store-issue-code issue)
                  'duplicate-conflict))
      (should (string-suffix-p
               "/writer-a/shared-id.json"
               (magnus-coord-store-issue-related-path issue))))))

(ert-deftest magnus-coord-store-byte-identical-duplicate-is-idempotent ()
  "Exact duplicate evidence is accepted once without an issue."
  (let* ((first
          (magnus-coord-store-event--create
           :id "same" :writer-id "writer" :writer-sequence 1
           :path "/a/same.json"
           :bytes "same bytes"))
         (second
          (magnus-coord-store-event--create
           :id "same" :writer-id "writer" :writer-sequence 1
           :path "/b/same.json"
           :bytes "same bytes"))
         (result (magnus-coord-store--deduplicate-events
                  (list first second))))
    (should (equal (car result) (list first)))
    (should-not (cdr result))))

(ert-deftest magnus-coord-store-rejected-variant-is-also-idempotent ()
  "Repeating identical rejected evidence does not duplicate conflict issues."
  (let* ((accepted
          (magnus-coord-store-event--create
           :id "shared" :writer-id "writer-a" :writer-sequence 1
           :path "/a/shared.json" :bytes "accepted bytes"))
         (rejected
          (magnus-coord-store-event--create
           :id "shared" :writer-id "writer-b" :writer-sequence 7
           :path "/b/shared.json" :bytes "rejected bytes"))
         (repeated
          (magnus-coord-store-event--create
           :id "shared" :writer-id "writer-b" :writer-sequence 7
           :path "/copy/shared.json" :bytes "rejected bytes"))
         (result (magnus-coord-store--deduplicate-events
                  (list accepted rejected repeated))))
    (should (equal (car result) (list accepted)))
    (should (= (length (cdr result)) 1))
    (should (eq (magnus-coord-store-issue-code (car (cdr result)))
                'duplicate-conflict))))

(ert-deftest magnus-coord-store-empty-project-has-empty-snapshot ()
  "A project without a store produces a clean empty snapshot."
  (magnus-coord-store-tests--with-project
    (let ((snapshot (magnus-coord-store-snapshot project)))
      (should-not (magnus-coord-store-snapshot-candidate-paths snapshot))
      (should-not (magnus-coord-store-snapshot-events snapshot))
      (should-not (magnus-coord-store-snapshot-issues snapshot)))))

(ert-deftest magnus-coord-store-reserves-temporaries-and-confines-root-name ()
  "Publisher temporaries are invisible and a custom store name cannot escape."
  (magnus-coord-store-tests--with-project
    (let* ((writer-directory
            (magnus-coord-store-writer-directory project "writer"))
           (temporary (expand-file-name ".magnus-event-tmp-stale"
                                        writer-directory)))
      (magnus-coord-store-tests--write-bytes temporary "partial")
      (let ((snapshot (magnus-coord-store-snapshot project)))
        (should-not (magnus-coord-store-snapshot-candidate-paths snapshot))
        (should-not (magnus-coord-store-snapshot-events snapshot))
        (should-not (magnus-coord-store-snapshot-issues snapshot))))
    (let ((magnus-coord-store-directory-name "../escape"))
      (should-error
       (magnus-coord-store-writer-directory project "writer")
       :type 'magnus-coord-store-error)
      (should-not (file-exists-p
                   (expand-file-name "escape" (file-name-directory project)))))
    (dolist (unsafe '("~" "~root" "/tmp/escape"))
      (let ((magnus-coord-store-directory-name unsafe))
        (should-error
         (magnus-coord-store-writer-directory project "writer")
         :type 'magnus-coord-store-error)))))

(ert-deftest magnus-coord-store-hardens-preexisting-owned-directories ()
  "A partial prior hierarchy cannot leave writable store directories."
  (magnus-coord-store-tests--with-project
    (let* ((root (expand-file-name ".magnus-coord" project))
           (writers (expand-file-name "writers" root))
           (writer (expand-file-name "writer" writers)))
      (make-directory writer t)
      (dolist (directory (list root writers writer))
        (set-file-modes directory #o777))
      (magnus-coord-store-tests--write-event
       project "writer" "Writer" "log.append"
       (magnus-coord-store-tests--payload "private")
       :event-id "private-event"
       :writer-sequence 1
       :created-at "2026-08-04T01:00:00.000000Z")
      (dolist (directory (list root writers writer))
        (should (= (logand (file-modes directory) #o777) #o700))))))

(ert-deftest magnus-coord-store-public-directory-primitives-do-not-need-events ()
  "Policy can discover and provision an inbox without publishing fake data."
  (magnus-coord-store-tests--with-project
    (let* ((root (magnus-coord-store-directory project))
           (writer (magnus-coord-store-writer-directory project "writer")))
      (should (equal root
                     (expand-file-name ".magnus-coord"
                                       (file-truename project))))
      (should-not (file-exists-p root))
      (should (equal (magnus-coord-store-ensure-writer-directory
                      project "writer")
                     writer))
      (should (file-directory-p writer))
      (should (= (logand (file-modes root) #o777) #o700))
      (should (= (logand (file-modes
                          (expand-file-name "writers" root)) #o777)
                 #o700))
      (should (= (logand (file-modes writer) #o777) #o700))
      (should-not (directory-files writer nil "\\.json\\'")))))

(ert-deftest magnus-coord-store-orders-by-writer-sequence-not-wall-clock ()
  "Backward wall-clock movement cannot reverse one writer's causal order."
  (magnus-coord-store-tests--with-project
    (magnus-coord-store-tests--write-event
     project "writer-a" "Aye" "active.set"
     (magnus-coord-store-tests--payload "first")
     :event-id "first" :writer-sequence 1
     :created-at "2026-08-04T03:00:00.000000Z")
    (magnus-coord-store-tests--write-event
     project "writer-a" "Aye" "active.clear"
     (magnus-coord-store-tests--payload "second")
     :event-id "second" :writer-sequence 2
     :created-at "2026-08-04T01:00:00.000000Z")
    (magnus-coord-store-tests--write-event
     project "writer-b" "Bee" "log.append"
     (magnus-coord-store-tests--payload "other writer")
     :event-id "other" :writer-sequence 1
     :created-at "2026-08-04T02:00:00.000000Z")
    (let ((events (magnus-coord-store-snapshot-events
                   (magnus-coord-store-snapshot project))))
      (should (equal (mapcar #'magnus-coord-store-event-id events)
                     '("first" "second" "other")))
      (should (equal (mapcar #'magnus-coord-store-event-writer-sequence events)
                     '(1 2 1))))))

(ert-deftest magnus-coord-store-surfaces-writer-sequence-ambiguity ()
  "Two IDs cannot silently claim the same writer-local causal position."
  (magnus-coord-store-tests--with-project
    (magnus-coord-store-tests--write-event
     project "writer" "Writer" "active.set"
     (magnus-coord-store-tests--payload "first by path")
     :event-id "event-a" :writer-sequence 7
     :created-at "2026-08-04T03:00:00.000000Z")
    (magnus-coord-store-tests--write-event
     project "writer" "Writer" "active.clear"
     (magnus-coord-store-tests--payload "ambiguous")
     :event-id "event-b" :writer-sequence 7
     :created-at "2026-08-04T01:00:00.000000Z")
    (let* ((snapshot (magnus-coord-store-snapshot project))
           (event (car (magnus-coord-store-snapshot-events snapshot)))
           (issue (car (magnus-coord-store-snapshot-issues snapshot))))
      (should (= (length (magnus-coord-store-snapshot-events snapshot)) 1))
      (should (equal (magnus-coord-store-event-id event) "event-a"))
      (should (eq (magnus-coord-store-issue-code issue)
                  'writer-sequence-conflict))
      (should (equal (magnus-coord-store-issue-writer-id issue) "writer"))
      (should (= (magnus-coord-store-issue-writer-sequence issue) 7))
      (should (equal (magnus-coord-store-issue-event-id issue) "event-b"))
      (should (equal (magnus-coord-store-issue-related-event-id issue)
                     "event-a")))))

(ert-deftest magnus-coord-store-revision-is-stable-and-does-no-event-io ()
  "An unchanged revision lists only writers/ and performs no event operations."
  (magnus-coord-store-tests--with-project
    (magnus-coord-store-tests--write-event
     project "writer-a" "Aye" "log.append"
     (magnus-coord-store-tests--payload "a")
     :event-id "event-a" :writer-sequence 1
     :created-at "2026-08-04T01:00:00.000000Z")
    (magnus-coord-store-ensure-writer-directory project "writer-b")
    (let* ((writers (expand-file-name
                     "writers" (magnus-coord-store-directory project)))
           (original-entries
            (symbol-function 'magnus-coord-store--directory-entries))
           (original-attributes (symbol-function 'file-attributes))
           listings
           attributes
           first
           second)
      (cl-letf (((symbol-function 'magnus-coord-store--directory-entries)
                 (lambda (directory)
                   (push directory listings)
                   (when (string-prefix-p
                          (file-name-as-directory
                           (magnus-coord-store-writer-directory
                            project "writer-a"))
                          (file-name-as-directory directory))
                     (ert-fail "revision listed inside a writer inbox"))
                   (funcall original-entries directory)))
                ((symbol-function 'insert-file-contents-literally)
                 (lambda (&rest _arguments)
                   (ert-fail "revision opened an event file")))
                ((symbol-function 'file-attributes)
                 (lambda (path &rest arguments)
                   (push path attributes)
                   (when (string-suffix-p ".json" path)
                     (ert-fail "revision statted an event file"))
                   (apply original-attributes path arguments))))
        (setq first (magnus-coord-store-revision project)
              second (magnus-coord-store-revision project)))
      (should (equal (magnus-coord-store-revision-result-token first)
                     (magnus-coord-store-revision-result-token second)))
      (should-not (magnus-coord-store-revision-result-issues first))
      (should (equal listings (list writers writers)))
      (should-not (cl-find-if (lambda (path)
                                (string-suffix-p ".json" path))
                              attributes)))))

(ert-deftest magnus-coord-store-revision-tracks-inbox-shape-not-projection ()
  "Writer/event changes alter revision; replacing current.md does not."
  (magnus-coord-store-tests--with-project
    (let* ((empty (magnus-coord-store-revision-result-token
                   (magnus-coord-store-revision project)))
           (_writer
            (magnus-coord-store-ensure-writer-directory project "writer-a"))
           (with-writer (magnus-coord-store-revision-result-token
                         (magnus-coord-store-revision project))))
      (should-not (equal empty with-writer))
      (let* ((event
              (magnus-coord-store-tests--write-event
               project "writer-a" "Aye" "log.append"
               (magnus-coord-store-tests--payload "event")
               :event-id "event" :writer-sequence 1
               :created-at "2026-08-04T01:00:00.000000Z"))
             (with-event (magnus-coord-store-revision-result-token
                          (magnus-coord-store-revision project))))
        (should-not (equal with-writer with-event))
        (let* ((root (magnus-coord-store-directory project))
               (projection (expand-file-name "current.md" root))
               (temporary (make-temp-file
                           (expand-file-name ".current-tmp-" root))))
          (magnus-coord-store-tests--write-bytes temporary "projection")
          (rename-file temporary projection t)
          (should (equal with-event
                         (magnus-coord-store-revision-result-token
                          (magnus-coord-store-revision project)))))
        (delete-file event)
        (should-not
         (equal with-event
                (magnus-coord-store-revision-result-token
                 (magnus-coord-store-revision project))))
        (let ((before-second-writer
               (magnus-coord-store-revision-result-token
                (magnus-coord-store-revision project))))
          (magnus-coord-store-ensure-writer-directory project "writer-b")
          (should-not
           (equal before-second-writer
                  (magnus-coord-store-revision-result-token
                   (magnus-coord-store-revision project)))))))))

(ert-deftest magnus-coord-store-revision-isolates-unsafe-writer-entries ()
  "An unsafe writer is encoded and reported without hiding valid siblings."
  (magnus-coord-store-tests--with-project
    (let* ((valid (magnus-coord-store-ensure-writer-directory
                   project "valid-writer"))
           (writers (file-name-directory (directory-file-name valid)))
           (target (expand-file-name "target" project))
           (unsafe (expand-file-name "unsafe-writer" writers)))
      (make-directory target)
      (make-symbolic-link target unsafe)
      (let* ((revision (magnus-coord-store-revision project))
             (issues (magnus-coord-store-revision-result-issues revision)))
        (should (= (length issues) 1))
        (should (eq (magnus-coord-store-issue-code (car issues))
                    'unsafe-writer-path))
        (should (stringp (magnus-coord-store-revision-result-token revision)))
        (should (file-directory-p valid))))))

(ert-deftest magnus-coord-store-prune-is-snapshot-scoped ()
  "Pruning deletes selected snapshot evidence but no later or malformed file."
  (magnus-coord-store-tests--with-project
    (let* ((keep
            (magnus-coord-store-tests--write-event
             project "writer" "Writer" "log.append"
             (magnus-coord-store-tests--payload "keep")
             :event-id "keep" :writer-sequence 1
             :created-at "2026-08-04T01:00:00.000000Z"))
           (remove
            (magnus-coord-store-tests--write-event
             project "writer" "Writer" "log.append"
             (magnus-coord-store-tests--payload "remove")
             :event-id "remove" :writer-sequence 2
             :created-at "2026-08-04T02:00:00.000000Z"))
           (bad (magnus-coord-store-tests--event-path
                 project "writer" "malformed")))
      (magnus-coord-store-tests--write-bytes bad "not json")
      (let ((snapshot (magnus-coord-store-snapshot project)))
        (let ((later
               (magnus-coord-store-tests--write-event
                project "writer" "Writer" "log.append"
                (magnus-coord-store-tests--payload "later")
                :event-id "later" :writer-sequence 3
                :created-at "2026-08-04T03:00:00.000000Z")))
          (let ((result (magnus-coord-store-prune snapshot '("keep"))))
            (should
             (equal (mapcar #'magnus-coord-store-event-id
                            (magnus-coord-store-prune-result-deleted-events
                             result))
                    '("remove")))
            (should
             (equal (mapcar #'magnus-coord-store-event-id
                            (magnus-coord-store-prune-result-kept-events result))
                    '("keep")))
            (should (equal (mapcar #'magnus-coord-store-issue-code
                                   (magnus-coord-store-prune-result-issues result))
                           '(invalid-json)))
            (should (file-exists-p keep))
            (should-not (file-exists-p remove))
            (should (file-exists-p later))
            (should (file-exists-p bad))))))))

(ert-deftest magnus-coord-store-prune-refuses-replaced-or-mutated-events ()
  "Identity and content checks preserve evidence changed since the snapshot."
  (magnus-coord-store-tests--with-project
    (let* ((original
            (magnus-coord-store-tests--write-event
             project "writer" "Writer" "log.append"
             (magnus-coord-store-tests--payload "original")
             :event-id "replaced" :writer-sequence 1
             :created-at "2026-08-04T01:00:00.000000Z"))
           (snapshot (magnus-coord-store-snapshot project))
           (path original))
      (delete-file path)
      (magnus-coord-store-tests--write-event
       project "writer" "Writer" "log.append"
       (magnus-coord-store-tests--payload "original")
       :event-id "replaced" :writer-sequence 1
       :created-at "2026-08-04T01:00:00.000000Z")
      (let ((result (magnus-coord-store-prune snapshot nil)))
        (should-not (magnus-coord-store-prune-result-deleted-events result))
        (should (eq (magnus-coord-store-issue-code
                     (car (magnus-coord-store-prune-result-issues result)))
                    'prune-identity-mismatch))
        (should (file-exists-p path)))
      (let* ((mutated
              (magnus-coord-store-tests--write-event
               project "writer" "Writer" "log.append"
               (magnus-coord-store-tests--payload "before")
               :event-id "mutated" :writer-sequence 2
               :created-at "2026-08-04T02:00:00.000000Z"))
             (mutated-snapshot (magnus-coord-store-snapshot project))
             (mutated-path mutated))
        (magnus-coord-store-tests--write-bytes mutated-path "changed in place")
        (let ((result
               (magnus-coord-store-prune
                mutated-snapshot '("replaced"))))
          (should (eq (magnus-coord-store-issue-code
                       (car (magnus-coord-store-prune-result-issues result)))
                      'prune-content-mismatch))
          (should (file-exists-p mutated-path)))))))

(ert-deftest magnus-coord-store-prune-refuses-symlink-and-forged-path ()
  "Pruning never follows a replacement symlink or trusts a forged event path."
  (magnus-coord-store-tests--with-project
    (let* ((event
            (magnus-coord-store-tests--write-event
             project "writer" "Writer" "log.append"
             (magnus-coord-store-tests--payload "event")
             :event-id "event" :writer-sequence 1
             :created-at "2026-08-04T01:00:00.000000Z"))
           (snapshot (magnus-coord-store-snapshot project))
           (path event)
           (outside (expand-file-name "outside" project)))
      (magnus-coord-store-tests--write-bytes outside "outside")
      (delete-file path)
      (make-symbolic-link outside path)
      (let ((result (magnus-coord-store-prune snapshot nil)))
        (should (eq (magnus-coord-store-issue-code
                     (car (magnus-coord-store-prune-result-issues result)))
                    'prune-unsafe-entry))
        (should (file-symlink-p path))
        (should (file-exists-p outside))))
    (let* ((event
            (magnus-coord-store-tests--write-event
             project "other-writer" "Other" "log.append"
             (magnus-coord-store-tests--payload "other")
             :event-id "other" :writer-sequence 1
             :created-at "2026-08-04T01:00:00.000000Z"))
           (snapshot (magnus-coord-store-snapshot project))
           (outside (expand-file-name "forged-target" project)))
      (magnus-coord-store-tests--write-bytes outside "do not delete")
      (setf (magnus-coord-store-event-path
             (car (magnus-coord-store-snapshot-events snapshot)))
            outside)
      (let ((result (magnus-coord-store-prune snapshot nil)))
        (should (memq 'prune-path-mismatch
                      (mapcar #'magnus-coord-store-issue-code
                              (magnus-coord-store-prune-result-issues result))))
        (should (file-exists-p outside))
        (should (file-exists-p event))))))

(ert-deftest magnus-coord-store-prune-protects-conflicted-evidence ()
  "Pruning cannot discard the deterministic representative of an ambiguity."
  (magnus-coord-store-tests--with-project
    (dolist (id '("event-a" "event-b"))
      (magnus-coord-store-tests--write-event
       project "writer" "Writer" "log.append"
       (magnus-coord-store-tests--payload id)
       :event-id id :writer-sequence 1
       :created-at "2026-08-04T01:00:00.000000Z"))
    (let* ((snapshot (magnus-coord-store-snapshot project))
           (result (magnus-coord-store-prune snapshot nil)))
      (should-not (magnus-coord-store-prune-result-deleted-events result))
      (should
       (equal (mapcar #'magnus-coord-store-event-id
                      (magnus-coord-store-prune-result-kept-events result))
              '("event-a")))
      (should (eq (magnus-coord-store-issue-code
                   (car (magnus-coord-store-prune-result-issues result)))
                  'writer-sequence-conflict))
      (should (file-exists-p
               (magnus-coord-store-tests--event-path
                project "writer" "event-a")))
      (should (file-exists-p
               (magnus-coord-store-tests--event-path
                project "writer" "event-b"))))))

(ert-deftest magnus-coord-store-prune-deletes-predecessors-before-winners ()
  "Pruning preserves ascending snapshot order for crash-safe state removal."
  (magnus-coord-store-tests--with-project
    (dolist (description '(("one" 1) ("two" 2) ("three" 3)))
      (pcase-let ((`(,id ,sequence) description))
        (magnus-coord-store-tests--write-event
         project "writer" "Writer" "knowledge.put"
         (magnus-coord-store-tests--payload id)
         :event-id id :writer-sequence sequence
         :created-at "2026-08-04T01:00:00.000000Z")))
    (let ((snapshot (magnus-coord-store-snapshot project))
          (original-delete (symbol-function 'delete-file))
          deleted-paths)
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (path &optional trash)
                   (when (string-suffix-p ".json" path)
                     (push path deleted-paths))
                   (funcall original-delete path trash))))
        (magnus-coord-store-prune snapshot nil))
      (should
       (equal (mapcar #'file-name-base (nreverse deleted-paths))
              '("one" "two" "three"))))))

(ert-deftest magnus-coord-store-conflict-claims-close-transitively ()
  "A rejected duplicate ID still reserves its independent causal slot."
  (magnus-coord-store-tests--with-project
    (magnus-coord-store-tests--write-event
     project "writer-a" "Aye" "log.append"
     (magnus-coord-store-tests--payload "accepted")
     :event-id "shared-id" :writer-sequence 1
     :created-at "2026-08-04T01:00:00.000000Z")
    ;; This loses the global-ID claim but must still reserve writer-b/7.
    (magnus-coord-store-tests--write-event
     project "writer-b" "Bee" "log.append"
     (magnus-coord-store-tests--payload "id conflict")
     :event-id "shared-id" :writer-sequence 7
     :created-at "2026-08-04T02:00:00.000000Z")
    (magnus-coord-store-tests--write-event
     project "writer-b" "Bee" "log.append"
     (magnus-coord-store-tests--payload "sequence conflict")
     :event-id "third-id" :writer-sequence 7
     :created-at "2026-08-04T03:00:00.000000Z")
    (let* ((snapshot (magnus-coord-store-snapshot project))
           (issues (magnus-coord-store-snapshot-issues snapshot))
           (sequence-issue
            (cl-find 'writer-sequence-conflict issues
                     :key #'magnus-coord-store-issue-code)))
      (should
       (equal (mapcar #'magnus-coord-store-event-id
                      (magnus-coord-store-snapshot-events snapshot))
              '("shared-id")))
      (should
       (equal (mapcar #'magnus-coord-store-issue-code issues)
              '(duplicate-conflict writer-sequence-conflict)))
      (should (equal (magnus-coord-store-issue-event-id sequence-issue)
                     "third-id"))
      (should (= (magnus-coord-store-issue-writer-sequence sequence-issue) 7))
      (should
       (string-suffix-p
        "/writer-b/shared-id.json"
        (magnus-coord-store-issue-related-path sequence-issue))))))

(ert-deftest magnus-coord-store-emits-every-applicable-conflict-dimension ()
  "One event conflicting by global ID and writer sequence reports both."
  (magnus-coord-store-tests--with-project
    (magnus-coord-store-tests--write-event
     project "writer-a" "Aye" "log.append"
     (magnus-coord-store-tests--payload "id owner")
     :event-id "shared-z" :writer-sequence 1
     :created-at "2026-08-04T01:00:00.000000Z")
    (magnus-coord-store-tests--write-event
     project "writer-b" "Bee" "log.append"
     (magnus-coord-store-tests--payload "sequence owner")
     :event-id "a-sequence-owner" :writer-sequence 9
     :created-at "2026-08-04T02:00:00.000000Z")
    ;; This path sorts after writer-b/a-sequence-owner, so both dimensions are
    ;; already claimed when it is examined.
    (magnus-coord-store-tests--write-event
     project "writer-b" "Bee" "log.append"
     (magnus-coord-store-tests--payload "both conflicts")
     :event-id "shared-z" :writer-sequence 9
     :created-at "2026-08-04T03:00:00.000000Z")
    (let* ((snapshot (magnus-coord-store-snapshot project))
           (issues (magnus-coord-store-snapshot-issues snapshot)))
      (should
       (equal (mapcar #'magnus-coord-store-event-id
                      (magnus-coord-store-snapshot-events snapshot))
              '("shared-z" "a-sequence-owner")))
      (should
       (equal (mapcar #'magnus-coord-store-issue-code issues)
              '(duplicate-conflict writer-sequence-conflict)))
      (should (cl-every
               (lambda (issue)
                 (equal (magnus-coord-store-issue-event-id issue) "shared-z"))
               issues)))))

(ert-deftest magnus-coord-store-prune-failure-defers-all-later-events ()
  "A sequence-1 failure leaves sequence 2 untouched and explicitly deferred."
  (magnus-coord-store-tests--with-project
    (let* ((first
            (magnus-coord-store-tests--write-event
             project "writer" "Writer" "knowledge.put"
             (magnus-coord-store-tests--payload "first")
             :event-id "first" :writer-sequence 1
             :created-at "2026-08-04T01:00:00.000000Z"))
           (second
            (magnus-coord-store-tests--write-event
             project "writer" "Writer" "knowledge.put"
             (magnus-coord-store-tests--payload "second")
             :event-id "second" :writer-sequence 2
             :created-at "2026-08-04T02:00:00.000000Z"))
           (snapshot (magnus-coord-store-snapshot project))
           (first-path first)
           (second-path second)
           (original-read (symbol-function 'insert-file-contents-literally))
           reads
           result)
      ;; Recreate identical bytes at the first path with a new inode.
      (delete-file first-path)
      (magnus-coord-store-tests--write-event
       project "writer" "Writer" "knowledge.put"
       (magnus-coord-store-tests--payload "first")
       :event-id "first" :writer-sequence 1
       :created-at "2026-08-04T01:00:00.000000Z")
      (cl-letf (((symbol-function 'insert-file-contents-literally)
                 (lambda (path &rest arguments)
                   (push path reads)
                   (apply original-read path arguments))))
        (setq result (magnus-coord-store-prune snapshot nil)))
      (should-not (magnus-coord-store-prune-result-deleted-events result))
      (should
       (equal (mapcar #'magnus-coord-store-event-id
                      (magnus-coord-store-prune-result-kept-events result))
              '("first" "second")))
      (should (equal reads (list first-path)))
      (should (file-exists-p first-path))
      (should (file-exists-p second-path))
      (let* ((issues (magnus-coord-store-prune-result-issues result))
             (deferred (cl-find 'prune-deferred issues
                                :key #'magnus-coord-store-issue-code)))
        (should
         (equal (mapcar #'magnus-coord-store-issue-code issues)
                '(prune-identity-mismatch prune-deferred)))
        (should (equal (magnus-coord-store-issue-event-id deferred) "second"))
        (should (equal (magnus-coord-store-issue-related-event-id deferred)
                       "first"))))))

(provide 'magnus-coord-store-tests)
;;; magnus-coord-store-tests.el ends here
