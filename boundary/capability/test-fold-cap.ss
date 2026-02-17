;;; boundary/capability/test-fold-cap.ss — Tests for fold capability layer
;;; Covers: env store/retrieve, session isolation, map-chunks error handling,
;;; ingest/peek/grep round-trips, env-keys, memory persistence.

(load "core/testing/test-framework.ss")
(load "boundary/capability/fold-cap.ss")

;;; ====
;;; Test Helpers
;;; ====

;; Use unique session IDs per test group to avoid cross-contamination
(define (fresh-session) (format "test-~a" (random 1000000)))

;; Temporary directory for memory persistence tests
(define *test-memory-dir* #f)
(define *saved-cwd* #f)

(define (setup-temp-dir!)
  (let ([dir (format "/tmp/fold-cap-test-~a" (random 1000000))])
    (mkdir dir)
    (set! *test-memory-dir* dir)
    (set! *saved-cwd* (current-directory))
    (current-directory dir)))

(define (teardown-temp-dir!)
  (when *saved-cwd*
    (current-directory *saved-cwd*))
  (when *test-memory-dir*
    (system (format "rm -rf ~a" *test-memory-dir*))
    (set! *test-memory-dir* #f)))

;;; ====
;;; Env Store/Retrieve Consistency
;;; ====

(test-group env-store-retrieve
  (define-test "store and fetch round-trips a string"
    (let* ([sid (fresh-session)]
           [result (fold-cap-env-store! sid 'greeting "hello world" 'text)]
           [msg (car result)]
           [summary (cdr result)]
           [fetched (fold-cap-env-fetch sid 'greeting)])
      (assert-equal fetched "hello world")
      (assert-true (string? msg))))

  (define-test "store and fetch round-trips a list"
    (let* ([sid (fresh-session)]
           [_  (fold-cap-env-store! sid 'data '(1 2 3) 'sexpr)]
           [fetched (fold-cap-env-fetch sid 'data)])
      (assert-equal fetched '(1 2 3))))

  (define-test "store and fetch round-trips a number"
    (let* ([sid (fresh-session)]
           [_  (fold-cap-env-store! sid 'count 42 'result)]
           [fetched (fold-cap-env-fetch sid 'count)])
      (assert-equal fetched 42)))

  (define-test "fetch returns #f for missing key"
    (let ([sid (fresh-session)])
      (assert-false (fold-cap-env-fetch sid 'nonexistent))))

  (define-test "overwrite replaces previous value"
    (let* ([sid (fresh-session)]
           [_  (fold-cap-env-store! sid 'x 1 'result)]
           [_  (fold-cap-env-store! sid 'x 2 'result)]
           [fetched (fold-cap-env-fetch sid 'x)])
      (assert-equal fetched 2))))

;;; ====
;;; Per-Session Isolation
;;; ====

(test-group session-isolation
  (define-test "different sessions don't share state"
    (let* ([s1 (fresh-session)]
           [s2 (fresh-session)]
           [_  (fold-cap-env-store! s1 'secret "session-1-data" 'text)]
           [_  (fold-cap-env-store! s2 'secret "session-2-data" 'text)])
      (assert-equal (fold-cap-env-fetch s1 'secret) "session-1-data")
      (assert-equal (fold-cap-env-fetch s2 'secret) "session-2-data")))

  (define-test "keys from one session invisible in another"
    (let* ([s1 (fresh-session)]
           [s2 (fresh-session)]
           [_  (fold-cap-env-store! s1 'only-here "yes" 'text)])
      (assert-false (fold-cap-env-fetch s2 'only-here))))

  (define-test "env-keys reflects only session's own entries"
    (let* ([s1 (fresh-session)]
           [s2 (fresh-session)]
           [_  (fold-cap-env-store! s1 'a 1 'result)]
           [_  (fold-cap-env-store! s1 'b 2 'result)]
           [_  (fold-cap-env-store! s2 'c 3 'result)])
      (assert-equal (length (fold-cap-env-keys s1)) 2)
      (assert-equal (length (fold-cap-env-keys s2)) 1))))

;;; ====
;;; Env Summary
;;; ====

(test-group env-summary
  (define-test "summary returns list of (key type size) triples"
    (let* ([sid (fresh-session)]
           [result (fold-cap-env-store! sid 'item "test" 'text)]
           [summary (cdr result)])
      (assert-equal (length summary) 1)
      (let ([entry (car summary)])
        (assert-equal (car entry) 'item)
        (assert-equal (cadr entry) 'text))))

  (define-test "summary grows with each store"
    (let* ([sid (fresh-session)]
           [r1 (fold-cap-env-store! sid 'a 1 'result)]
           [r2 (fold-cap-env-store! sid 'b 2 'result)])
      (assert-equal (length (cdr r1)) 1)
      (assert-equal (length (cdr r2)) 2))))

;;; ====
;;; Ingest, Peek, Grep Round-Trips
;;; ====

(test-group ingest-peek-grep
  (define-test "ingest stores text and peek retrieves prefix"
    (let* ([sid (fresh-session)]
           [text "The quick brown fox jumps over the lazy dog. This is a test of chunked storage."]
           [_    (fold-cap-env-ingest! sid 'doc text 40)]
           [peeked (fold-cap-env-peek sid 'doc 20)])
      (assert-true (string? peeked))
      (assert-equal (string-length peeked) 20)
      (assert-equal peeked "The quick brown fox ")))

  (define-test "ingest creates chunks entry in env-keys"
    (let* ([sid (fresh-session)]
           [_   (fold-cap-env-ingest! sid 'bigtext "hello world" 5)]
           [keys (fold-cap-env-keys sid)])
      (assert-equal (length keys) 1)
      (let ([entry (car keys)])
        (assert-equal (car entry) 'bigtext)
        (assert-equal (cadr entry) 'chunks))))

  (define-test "grep finds relevant chunks"
    (let* ([sid (fresh-session)]
           [text (string-append
                   "Chapter 1: Linear Algebra basics. Vectors and matrices.\n"
                   (make-string 200 #\.)  ; padding to force a second chunk
                   "\nChapter 2: Calculus fundamentals. Derivatives and integrals.\n")]
           [_    (fold-cap-env-ingest! sid 'book text 150)]
           [results (fold-cap-env-grep sid 'book "linear algebra vectors" 3)])
      (assert-true (pair? results))))

  (define-test "grep returns #f for non-chunked keys"
    (let* ([sid (fresh-session)]
           [_ (fold-cap-env-store! sid 'plain "not chunked" 'text)])
      (assert-false (fold-cap-env-grep sid 'plain "anything" 3))))

  (define-test "grep returns #f for missing keys"
    (let ([sid (fresh-session)])
      (assert-false (fold-cap-env-grep sid 'nope "query" 3))))

  (define-test "peek returns #f for missing key"
    (let ([sid (fresh-session)])
      (assert-false (fold-cap-env-peek sid 'nope 100))))

  (define-test "peek on non-chunked value truncates"
    (let* ([sid (fresh-session)]
           [_ (fold-cap-env-store! sid 'msg "a long message" 'text)]
           [peeked (fold-cap-env-peek sid 'msg 5)])
      (assert-true (string? peeked))
      ;; Non-chunked peek returns format "~s" of value, so includes quotes
      (assert-true (<= (string-length peeked) 5)))))

;;; ====
;;; Slice
;;; ====

(test-group env-slice
  (define-test "slice retrieves contiguous chunk range"
    (let* ([sid (fresh-session)]
           [text "aaaa\nbbbb\ncccc\ndddd\n"]
           [_    (fold-cap-env-ingest! sid 'parts text 5)]
           [sliced (fold-cap-env-slice sid 'parts 0 2)])
      (assert-true (string? sliced))
      (assert-true (> (string-length sliced) 0)))))

;;; ====
;;; Map-Chunks Error Handling
;;; ====

(test-group map-chunks
  (define-test "map-chunks applies expression to each chunk"
    (let* ([sid (fresh-session)]
           [_ (fold-cap-env-ingest! sid 'nums "111\n222\n333" 4)]
           [results (fold-cap-env-map-chunks sid 'nums "(string-length *chunk*)")])
      (assert-true (pair? results))
      (assert-true (for-all number? results))))

  (define-test "map-chunks returns empty list for missing key"
    (let* ([sid (fresh-session)]
           [results (fold-cap-env-map-chunks sid 'nope "anything")])
      (assert-equal results '())))

  (define-test "map-chunks returns empty for non-chunked key"
    (let* ([sid (fresh-session)]
           [_ (fold-cap-env-store! sid 'plain 42 'result)]
           [results (fold-cap-env-map-chunks sid 'plain "(+ 1 1)")])
      (assert-equal results '())))

  (define-test "map-chunks handles eval errors gracefully"
    (let* ([sid (fresh-session)]
           [_ (fold-cap-env-ingest! sid 'data "hello" 100)]
           [results (fold-cap-env-map-chunks sid 'data "(/ 1 0)")])
      ;; Should return error strings, not crash
      (assert-true (pair? results))
      (assert-true (string? (car results)))
      (assert-true (string-contains? (car results) "error"))))

  (define-test "map-chunks catches parse errors upfront"
    (let* ([sid (fresh-session)]
           [_ (fold-cap-env-ingest! sid 'data "hello" 100)]
           [results (fold-cap-env-map-chunks sid 'data "(unclosed paren")])
      ;; Malformed expression — every result should be an error
      (assert-true (pair? results))
      (assert-true (string? (car results)))
      (assert-true (string-contains? (car results) "error")))))

;;; ====
;;; Memory Persistence
;;; ====

(test-group memory-persistence
  (define-test "memorize and remember round-trip"
    (dynamic-wind
      setup-temp-dir!
      (lambda ()
        ;; Reset caches
        (set! *fold-cap-memory-cache* #f)
        (set! *fold-cap-memory-index* #f)
        (fold-cap-memorize! 'test-key "The quick brown fox")
        ;; Force re-read from disk
        (set! *fold-cap-memory-cache* #f)
        (set! *fold-cap-memory-index* #f)
        (let ([results (fold-cap-remember "quick brown fox" 5)])
          (assert-true (pair? results))
          (let ([entry (car results)])
            (assert-equal (car entry) 'test-key)
            (assert-equal (cadr entry) "The quick brown fox"))))
      teardown-temp-dir!))

  (define-test "remember returns empty for no memories"
    (dynamic-wind
      setup-temp-dir!
      (lambda ()
        (set! *fold-cap-memory-cache* #f)
        (set! *fold-cap-memory-index* #f)
        (let ([results (fold-cap-remember "anything" 5)])
          (assert-equal results '())))
      teardown-temp-dir!))

  (define-test "multiple memories accumulate on disk"
    (dynamic-wind
      setup-temp-dir!
      (lambda ()
        (set! *fold-cap-memory-cache* #f)
        (set! *fold-cap-memory-index* #f)
        (fold-cap-memorize! 'first "alpha bravo charlie")
        (fold-cap-memorize! 'second "delta echo foxtrot")
        ;; Force re-read
        (set! *fold-cap-memory-cache* #f)
        (set! *fold-cap-memory-index* #f)
        (let ([all (fold-cap-load-memory!)])
          (assert-equal (length all) 2)))
      teardown-temp-dir!)))

;;; ====
;;; Run
;;; ====

(run-all-tests)
