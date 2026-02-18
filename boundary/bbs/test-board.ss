;;; boundary/bbs/test-board.ss — Multi-Board BBS Test Suite
;;;
;;; Tests: board record, registry, multi-board isolation, forum threading,
;;; migration from legacy layout, backward compatibility.
;;;
;;; Run: scheme --script boundary/bbs/test-board.ss

;; Load BBS from project root first (load paths are relative to CWD)
(load "boundary/bbs/bbs.ss")

(doc 'module 'test-board)
(doc 'description "Multi-board BBS architecture test suite")
(doc 'layer 'boundary)

;; ---- Test harness ----

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (printf "  [PASS] ~a~n" name))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (printf "  [FAIL] ~a~n" name)
        (printf "      Expected: ~s~n" expected)
        (printf "      Got:      ~s~n" actual))))

(define (test-true name actual)
  (test name #t (and actual #t)))

(define (test-false name actual)
  (test name #f actual))

(define (test-pred name pred actual)
  (if (pred actual)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (printf "  [PASS] ~a~n" name))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (printf "  [FAIL] ~a~n" name)
        (printf "      Value ~s did not satisfy predicate~n" actual))))

(define (test-error name thunk)
  (guard (e [else
             (set! tests-passed (+ tests-passed 1))
             (printf "  [PASS] ~a (raised error)~n" name)])
    (thunk)
    (set! tests-failed (+ tests-failed 1))
    (printf "  [FAIL] ~a (no error raised)~n" name)))

;; ---- Setup: isolate from real BBS data ----
;; cd to temp dir so all relative .bbs/ and .store/ paths are isolated

(define project-root (current-directory))
(define test-dir "/tmp/fold-bbs-test")

(printf "~n====~n")
(printf "  BBS MULTI-BOARD TEST SUITE~n")
(printf "====~n~n")
(printf "Test directory: ~a~n~n" test-dir)

;; Clean up any previous test run
(when (file-exists? test-dir)
  (system (format "rm -rf ~a" test-dir)))
(mkdir test-dir)
(cd test-dir)

;; Create minimal CAS structure for block storage
(mkdir ".store")
(mkdir ".store/objects")

;; ---- Group 1: Board Record ----

(printf "---- Board Record ----~n")

(let ([b (make-tracker-board "test-tracker" "tt-" "A test tracker")])
  (test "tracker slug" "test-tracker" (board-slug b))
  (test "tracker type" 'tracker (board-board-type b))
  (test "tracker prefix" "tt-" (board-id-prefix b))
  (test "tracker description" "A test tracker" (board-description b))
  (test-true "tracker items-ht is hashtable" (hashtable? (board-items-ht b)))
  (test-true "tracker by-status-ht is hashtable" (hashtable? (board-by-status-ht b)))
  (test-true "tracker by-priority-ht is hashtable" (hashtable? (board-by-priority-ht b)))
  (test-false "tracker by-type-ht is #f" (board-by-type-ht b))
  (test-false "tracker reply-map-ht is #f" (board-reply-map-ht b))
  (test "tracker initial deps" '() (board-deps b))
  (test "tracker initial counter" 0 (board-counter b))
  (test-false "tracker not initialized" (board-initialized? b)))

(let ([b (make-feed-board "test-feed" "pf-" "A test feed")])
  (test "feed type" 'feed (board-board-type b))
  (test-false "feed by-status-ht is #f" (board-by-status-ht b))
  (test-true "feed by-type-ht is hashtable" (hashtable? (board-by-type-ht b))))

(let ([b (make-forum-board "test-forum" "tf-" "A test forum")])
  (test "forum type" 'forum (board-board-type b))
  (test-true "forum reply-map-ht is hashtable" (hashtable? (board-reply-map-ht b)))
  (test-true "forum by-status-ht is hashtable" (hashtable? (board-by-status-ht b))))

;; ---- Group 2: Board Paths ----

(printf "~n---- Board Paths ----~n")

(let ([b (make-tracker-board "myboard" "mb-" "Test")])
  (test "board-dir" ".bbs/boards/myboard" (board-dir b))
  (test "board-heads-dir" ".store/heads/bbs/myboard" (board-heads-dir b))
  (test "board-counter-file" ".bbs/boards/myboard/counter" (board-counter-file b))
  (test "board-deps-file" ".bbs/boards/myboard/deps" (board-deps-file b))
  (test "board-index-cache-file" ".bbs/boards/myboard/index.cache" (board-index-cache-file b))
  (test "board-descriptor-file" ".bbs/boards/myboard/board.sexp" (board-descriptor-file b)))

;; ---- Group 3: Board Context ----

(printf "~n---- Board Context ----~n")

(test-false "no board context by default" (board-context?))

(test-error "current-board errors without context"
  (lambda () (current-board)))

(let ([b (make-tracker-board "ctx-test" "ct-" "Context test")])
  (with-board b
    (test-true "board context active inside with-board" (board-context?))
    (test "current-board-slug" "ctx-test" (current-board-slug))
    (test "current-board-type" 'tracker (current-board-type))
    (test "current-board-id-prefix" "ct-" (current-board-id-prefix))))

(test-false "board context gone after with-board" (board-context?))

;; ---- Group 4: Board Registry ----

(printf "~n---- Board Registry ----~n")

;; Reset registry
(set! *board-registry* (make-hashtable string-hash string=?))

(let ([b (make-tracker-board "reg-test" "rt-" "Registry test")])
  (test-false "board not registered yet" (board-exists? "reg-test"))
  (board-register! b)
  (test-true "board registered" (board-exists? "reg-test"))
  (test "board-ref returns board" "reg-test" (board-slug (board-ref "reg-test")))
  (test-false "unknown board returns #f" (board-ref "nonexistent"))
  (test-pred "board-list has one entry" (lambda (lst) (= (length lst) 1)) (board-list))
  (test-pred "board-slugs" (lambda (lst) (member "reg-test" lst)) (board-slugs)))

;; ---- Group 5: Board Persistence (Registry + Descriptor) ----

(printf "~n---- Board Persistence ----~n")

;; Clean slate
(set! *board-registry* (make-hashtable string-hash string=?))

;; board-create! handles dirs, descriptor, register, and save
(let ([b1 (board-create! "persist-tracker" 'tracker "pt-" "Persistence test tracker")]
      [b2 (board-create! "persist-feed" 'feed "pf-" "Persistence test feed")])
  (test-true "persist-tracker created" (board-exists? "persist-tracker"))
  (test-true "persist-feed created" (board-exists? "persist-feed"))
  (test-true "boards.sexp exists" (file-exists? ".bbs/boards.sexp"))
  (test-true "tracker board.sexp exists" (file-exists? ".bbs/boards/persist-tracker/board.sexp"))
  (test-true "tracker heads dir exists" (file-exists? ".store/heads/bbs/persist-tracker"))

  ;; Reload registry from disk
  (set! *board-registry* (make-hashtable string-hash string=?))
  (test-true "registry loaded from disk" (board-load-registry!))
  (test-true "tracker survived persistence" (board-exists? "persist-tracker"))
  (test-true "feed survived persistence" (board-exists? "persist-feed"))
  (test "reloaded tracker type" 'tracker (board-board-type (board-ref "persist-tracker")))
  (test "reloaded feed type" 'feed (board-board-type (board-ref "persist-feed")))
  (test "reloaded tracker prefix" "pt-" (board-id-prefix (board-ref "persist-tracker"))))

;; ---- Group 6: Multi-Board Isolation ----

(printf "~n---- Multi-Board Isolation ----~n")

;; Fresh registry
(set! *board-registry* (make-hashtable string-hash string=?))

(let* ([board-a (board-create! "alpha" 'tracker "a-" "Board Alpha")]
       [board-b (board-create! "beta" 'tracker "b-" "Board Beta")])

  ;; Create items on board-a
  (with-board board-a
    (let ([id (item-create! "Alpha issue" 'description "On board A")])
      (test-pred "alpha issue has a- prefix" (lambda (s) (string=? "a-" (substring s 0 2))) id)
      (test "alpha item count" 1 (item-count))))

  ;; Board-b should be empty
  (with-board board-b
    (test "beta item count" 0 (item-count)))

  ;; Create items on board-b
  (with-board board-b
    (let ([id (item-create! "Beta issue" 'description "On board B")])
      (test-pred "beta issue has b- prefix" (lambda (s) (string=? "b-" (substring s 0 2))) id)
      (test "beta item count" 1 (item-count))))

  ;; Verify alpha is still 1
  (with-board board-a
    (test "alpha still has 1 item" 1 (item-count))))

;; ---- Group 7: Forum Topic + Reply Threading ----

(printf "~n---- Forum Threading ----~n")

(set! *board-registry* (make-hashtable string-hash string=?))

(let ([forum-board (board-create! "discuss" 'forum "disc-" "Discussion forum")])
  (with-board forum-board
    ;; Create a topic
    (let ([topic-id (topic-create! "First topic" "Hello world" 'author "andy")])
      (test-pred "topic has disc- prefix"
        (lambda (s) (string=? "disc-" (substring s 0 5))) topic-id)
      (test "topic count" 1 (item-count))

      ;; Verify topic data
      (let ([data (item-fetch-data topic-id)])
        (test-true "topic data is alist" (pair? data))
        (test "topic title" "First topic" (cdr (assq 'title data)))
        (test "topic body" "Hello world" (cdr (assq 'body data)))
        (test "topic author" "andy" (cdr (assq 'author data)))
        (test "topic status" 'open (cdr (assq 'status data))))

      ;; Add replies (let* to ensure left-to-right evaluation order)
      (let* ([r1 (topic-reply! topic-id "Nice topic!" 'author "claude")]
             [r2 (topic-reply! topic-id "I agree!" 'author "sonnet")])
        (test-pred "reply starts with reply-"
          (lambda (s) (string=? "reply-" (substring s 0 6))) r1)
        (test "reply count" 2 (topic-reply-count topic-id))

        ;; Verify replies are in chronological order
        (let ([replies (topic-replies topic-id)])
          (test "replies length" 2 (length replies))
          (test "first reply author" "claude" (cdr (assq 'author (car replies))))
          (test "second reply author" "sonnet" (cdr (assq 'author (cadr replies))))))

      ;; Lock the topic
      (let ([new-hash (topic-lock! topic-id)])
        (test-true "lock returns hash" (bytevector? new-hash))
        (let ([data (item-fetch-data topic-id)])
          (test "topic status after lock" 'locked (cdr (assq 'status data)))))

      ;; Locking again returns #f (already locked)
      (test-false "double lock returns #f" (topic-lock! topic-id))

      ;; Replies to locked topics should error (Codex P2 fix)
      (test-error "reply to locked topic errors"
        (lambda () (topic-reply! topic-id "Should fail" 'author "hacker"))))))

;; ---- Group 8: Reply ID Parsing ----

(printf "~n---- Reply ID Parsing ----~n")

(test "reply-id->topic-id basic"
  "disc-001" (reply-id->topic-id "reply-disc-001-001"))
(test "reply-id->topic-id triple digit"
  "disc-001" (reply-id->topic-id "reply-disc-001-042"))
(test-false "reply-id->topic-id invalid prefix"
  (reply-id->topic-id "notreply-disc-001-001"))
(test-false "reply-id->topic-id too short"
  (reply-id->topic-id "reply"))

;; ---- Group 9: Migration (Legacy -> Multi-Board) ----

(printf "~n---- Migration ----~n")

;; Create a fresh temp dir simulating old layout
(let ([migration-dir "/tmp/fold-bbs-migrate-test"])
  (when (file-exists? migration-dir)
    (system (format "rm -rf ~a" migration-dir)))
  (mkdir migration-dir)
  (cd migration-dir)

  ;; Simulate old flat layout
  (mkdir ".bbs")
  (mkdir ".store")
  (mkdir ".store/heads")
  (mkdir ".store/heads/bbs")

  ;; Create old counter files
  (call-with-output-file ".bbs/counter"
    (lambda (p) (write 5 p) (newline p)))
  (call-with-output-file ".bbs/post-counter"
    (lambda (p) (write 2 p) (newline p)))

  ;; Create some old head files
  (call-with-output-file ".store/heads/bbs/fold-001.head"
    (lambda (p) (display "fakehash1" p)))
  (call-with-output-file ".store/heads/bbs/fold-002.head"
    (lambda (p) (display "fakehash2" p)))
  (call-with-output-file ".store/heads/bbs/post-1.head"
    (lambda (p) (display "fakehash3" p)))

  ;; Should need migration (no boards.sexp, but counter exists)
  (test-true "needs migration" (bbs-needs-migration?))

  ;; Run migration
  (bbs-migrate-to-boards!)

  ;; Verify new layout
  (test-true "boards.sexp created" (file-exists? ".bbs/boards.sexp"))
  (test-true "issues board dir created" (file-exists? ".bbs/boards/issues"))
  (test-true "changelog board dir created" (file-exists? ".bbs/boards/changelog"))
  (test-true "issues counter migrated" (file-exists? ".bbs/boards/issues/counter"))
  (test-true "changelog counter migrated" (file-exists? ".bbs/boards/changelog/counter"))
  (test-true "issues heads dir created" (file-exists? ".store/heads/bbs/issues"))
  (test-true "changelog heads dir created" (file-exists? ".store/heads/bbs/changelog"))

  ;; Verify head files moved
  (test-true "fold-001.head moved to issues/"
    (file-exists? ".store/heads/bbs/issues/fold-001.head"))
  (test-true "fold-002.head moved to issues/"
    (file-exists? ".store/heads/bbs/issues/fold-002.head"))
  (test-true "post-1.head moved to changelog/"
    (file-exists? ".store/heads/bbs/changelog/post-1.head"))

  ;; Old files should be gone
  (test-false "old counter removed" (file-exists? ".bbs/counter"))
  (test-false "old post-counter removed" (file-exists? ".bbs/post-counter"))
  (test-false "old fold-001.head removed" (file-exists? ".store/heads/bbs/fold-001.head"))
  (test-false "old post-1.head removed" (file-exists? ".store/heads/bbs/post-1.head"))

  ;; Should NOT need migration again (idempotent)
  (test-false "no migration needed after migrate" (bbs-needs-migration?))

  ;; Board descriptors
  (test-true "issues board.sexp created" (file-exists? ".bbs/boards/issues/board.sexp"))
  (test-true "changelog board.sexp created" (file-exists? ".bbs/boards/changelog/board.sexp"))

  ;; Cleanup
  (cd test-dir)
  (system (format "rm -rf ~a" migration-dir)))

;; ---- Group 10: Backward Compatibility (existing bbs-* functions) ----

(printf "~n---- Backward Compatibility ----~n")

;; Reset everything for a clean bbs-init!
(set! *board-registry* (make-hashtable string-hash string=?))
(set! *bbs-initialized* #f)
(set! *bbs-posts-initialized* #f)

;; Create fresh test dir for compat test
(let ([compat-dir "/tmp/fold-bbs-compat-test"])
  (when (file-exists? compat-dir)
    (system (format "rm -rf ~a" compat-dir)))
  (mkdir compat-dir)
  (cd compat-dir)

  ;; Create minimal CAS structure
  (mkdir ".store")
  (mkdir ".store/objects")

  ;; init creates default boards
  (let ([count (bbs-init!)])
    (test "bbs-init! returns issue count" 0 count)
    (test-true "issues board exists" (board-exists? "issues"))
    (test-true "changelog board exists" (board-exists? "changelog"))

    ;; Test that globals are wired
    (test-true "*bbs-issues* is hashtable" (hashtable? *bbs-issues*))
    (test-true "*bbs-by-status* is hashtable" (hashtable? *bbs-by-status*))
    (test-true "*bbs-by-priority* is hashtable" (hashtable? *bbs-by-priority*))

    ;; Create an issue via old API
    (let ([id (bbs-create "Backward compat issue")])
      (test-pred "bbs-create returns fold- ID"
        (lambda (s) (string=? "fold-" (substring s 0 5))) id)
      (test "issue count via old API" 1 (bbs-issue-count))
      (test-true "issue exists via old API" (bbs-issue-exists? id))

      ;; The same issue should be visible through new board API
      (with-board (board-ref "issues")
        (test "issue visible via board context" 1 (item-count)))

      ;; Test deps sync between global and board record (Codex P2 fix)
      (let ([id2 (bbs-create "Second issue")])
        (bbs-dep id id2)
        ;; Global API sees dependency
        (test-true "deps visible via global API" (bbs-is-blocked? id2))
        ;; Board-scoped API should also see it
        (with-board (board-ref "issues")
          (test-true "deps visible via board context" (bbs-is-blocked? id2)))))

    ;; Create a post via old API
    (let ([pid (post-create "Compat post" "Test body" 'changelog)])
      (test-pred "post-create returns post- ID"
        (lambda (s) (string=? "post-" (substring s 0 5))) pid)))

  ;; Cleanup
  (cd test-dir)
  (system (format "rm -rf ~a" compat-dir)))

;; ---- Cleanup ----

(cd project-root)
(system (format "rm -rf ~a" test-dir))

;; ---- Summary ----

(printf "~n====~n")
(printf "  Results: ~a passed, ~a failed~n" tests-passed tests-failed)
(printf "====~n")

(when (> tests-failed 0)
  (exit 1))
