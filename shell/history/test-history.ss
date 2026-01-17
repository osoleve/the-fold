;;; shell/history/test-history.ss — Test Suite for REPL History Module
;;;
;;; Tests command classification, block creation, history operations,
;;; and branching functionality.
;;;
;;; Run with: scheme --script shell/history/test-history.ss

(load "core/testing/test-framework.ss")

;;; ====
;;; Test Setup
;;; ====

;;; Load the history module
(load "shell/history/history.ss")

;;; Use a unique test session to avoid conflicts
(define *test-session* (format "test-history-~a" (time-nanosecond (current-time))))

;;; Clean up test artifacts
(define (cleanup-test-session!)
  (let ([dir (history-session-dir *test-session*)])
    (when (file-exists? dir)
      (guard (e [else #f])
        ;; Clean up head files
        (for-each
          (lambda (f)
            (let ([path (string-append dir "/" f)])
              (when (file-exists? path)
                (delete-file path))))
          (directory-list dir))
        (delete-directory dir)))))

;;; ====
;;; Command Classification Tests
;;; ====

(test-group "command-classification"
  (define-test "classifies define as definition"
    (assert-equal 'definition (classify-command "(define x 10)")))

  (define-test "classifies define with function as definition"
    (assert-equal 'definition (classify-command "(define (foo x) x)")))

  (define-test "classifies define-syntax as definition"
    (assert-equal 'definition (classify-command "(define-syntax my-mac (syntax-rules () ((_) 1)))")))

  (define-test "classifies load as effect"
    (assert-equal 'effect (classify-command "(load \"foo.ss\")")))

  (define-test "classifies display as effect"
    (assert-equal 'effect (classify-command "(display \"hello\")")))

  (define-test "classifies set! as effect"
    (assert-equal 'effect (classify-command "(set! x 20)")))

  (define-test "classifies arithmetic as expression"
    (assert-equal 'expression (classify-command "(+ 1 2)")))

  (define-test "classifies lambda as expression"
    (assert-equal 'expression (classify-command "(lambda (x) x)")))

  (define-test "classifies let as expression"
    (assert-equal 'expression (classify-command "(let ((x 1)) x)")))

  (define-test "classifies begin with define as definition"
    (assert-equal 'definition (classify-command "(begin (define y 5) y)")))

  (define-test "classifies begin with effect as effect"
    (assert-equal 'effect (classify-command "(begin (display 1) 2)")))

  (define-test "handles parse errors gracefully"
    (assert-equal 'expression (classify-command "(incomplete"))))

;;; ====
;;; Defined Name Extraction Tests
;;; ====

(test-group "defined-name-extraction"
  (define-test "extracts name from simple define"
    (assert-equal 'x (extract-defined-name "(define x 10)")))

  (define-test "extracts name from function define"
    (assert-equal 'foo (extract-defined-name "(define (foo x) x)")))

  (define-test "returns #f for non-definition"
    (assert-equal #f (extract-defined-name "(+ 1 2)")))

  (define-test "extracts first name from begin"
    (assert-equal 'a (extract-defined-name "(begin (define a 1) (define b 2))"))))

;;; ====
;;; Block Creation Tests
;;; ====

(test-group "block-creation"
  (define-test "creates history entry block"
    (let ([blk (make-history-entry-block
                 "test-session"
                 0
                 "(define x 10)"
                 'definition
                 'success
                 "abc123"
                 'x
                 "2026-01-17T12:00:00Z"
                 1
                 #f)])
      (assert-true (block? blk))
      (assert-equal 'history/entry (block-tag blk))))

  (define-test "entry data extraction works"
    (let* ([blk (make-history-entry-block
                  "test-session"
                  5
                  "(+ 1 2)"
                  'expression
                  'success
                  "def456"
                  #f
                  "2026-01-17T12:00:00Z"
                  1
                  #f)]
           [data (history-entry-data blk)])
      (assert-equal "test-session" (cdr (assq 'session-id data)))
      (assert-equal 5 (cdr (assq 'index data)))
      (assert-equal "(+ 1 2)" (cdr (assq 'command data)))
      (assert-equal 'expression (cdr (assq 'cmd-type data)))))

  (define-test "entry with prev ref has correct refs"
    (let* ([prev-hash (sha256 (string->utf8 "previous"))]
           [blk (make-history-entry-block
                  "test-session"
                  1
                  "(define y 20)"
                  'definition
                  'success
                  "ghi789"
                  'y
                  "2026-01-17T12:00:00Z"
                  1
                  prev-hash)])
      (assert-equal prev-hash (history-entry-prev blk)))))

;;; ====
;;; Head Management Tests
;;; ====

(test-group "head-management"
  (define-test "session directory creation"
    (history-ensure-session-dir! *test-session*)
    (assert-true (file-exists? (history-session-dir *test-session*))))

  (define-test "branch write and read"
    (let ([hash (sha256 (string->utf8 "test-hash"))])
      (history-write-branch! *test-session* "test-branch" hash)
      (let ([read-hash (history-read-branch *test-session* "test-branch")])
        (assert-true (bytevector=? hash read-hash)))))

  (define-test "current branch default"
    (assert-equal "main" (history-read-current-branch *test-session*)))

  (define-test "current branch write and read"
    (history-write-current-branch! *test-session* "experiment")
    (assert-equal "experiment" (history-read-current-branch *test-session*))
    ;; Reset for other tests
    (history-write-current-branch! *test-session* "main"))

  (define-test "redo stack operations"
    (let ([h1 (sha256 (string->utf8 "entry1"))]
          [h2 (sha256 (string->utf8 "entry2"))])
      (history-clear-redo! *test-session*)
      (history-push-redo! *test-session* h1)
      (history-push-redo! *test-session* h2)
      (let ([popped (history-pop-redo! *test-session*)])
        (assert-true (bytevector=? h2 popped)))
      (let ([popped2 (history-pop-redo! *test-session*)])
        (assert-true (bytevector=? h1 popped2)))
      (assert-equal #f (history-pop-redo! *test-session*))))

  (define-test "branch listing"
    (history-write-branch! *test-session* "branch1" (sha256 (string->utf8 "b1")))
    (history-write-branch! *test-session* "branch2" (sha256 (string->utf8 "b2")))
    (let ([branches (history-list-branches *test-session*)])
      (assert-true (pair? (member "branch1" branches)))
      (assert-true (pair? (member "branch2" branches))))))

;;; ====
;;; Storage Tests
;;; ====

(test-group "storage"
  (define-test "block store and fetch"
    (let* ([blk (make-history-entry-block
                  *test-session*
                  0
                  "(define test-var 42)"
                  'definition
                  'success
                  "abc"
                  'test-var
                  "2026-01-17T12:00:00Z"
                  1
                  #f)]
           [hash (history-store! blk)]
           [fetched (history-fetch hash)])
      (assert-true (block? fetched))
      (assert-equal 'history/entry (block-tag fetched))))

  (define-test "history count"
    ;; Initialize fresh branch for counting
    (history-write-current-branch! *test-session* "count-test")
    (let* ([blk1 (make-history-entry-block *test-session* 0 "(+ 1)" 'expression 'success "" #f "t1" 1 #f)]
           [h1 (history-store! blk1)]
           [blk2 (make-history-entry-block *test-session* 1 "(+ 2)" 'expression 'success "" #f "t2" 1 h1)]
           [h2 (history-store! blk2)])
      (history-write-branch! *test-session* "count-test" h2)
      (assert-equal 2 (history-count *test-session* "count-test")))))

;;; ====
;;; Recording Tests
;;; ====

(test-group "recording"
  (define-test "record success"
    ;; Use a fresh branch
    (history-write-current-branch! *test-session* "record-test")
    (let ([hash (history-record-success! *test-session* "(define rec-test 1)" 'void 'rec-test)])
      (assert-true (bytevector? hash))
      (let ([entry (history-fetch hash)])
        (assert-true (block? entry))
        (let ([data (history-entry-data entry)])
          (assert-equal "(define rec-test 1)" (cdr (assq 'command data)))
          (assert-equal 'definition (cdr (assq 'cmd-type data)))
          (assert-equal 'success (cdr (assq 'result-type data)))))))

  (define-test "record error"
    (let ([hash (history-record-error! *test-session* "(undefined-fn)" "not defined")])
      (let* ([entry (history-fetch hash)]
             [data (history-entry-data entry)])
        (assert-equal 'error (cdr (assq 'result-type data))))))

  (define-test "redo stack cleared on new record"
    (history-push-redo! *test-session* (sha256 (string->utf8 "old")))
    (history-record-success! *test-session* "(+ 1 1)" 2 #f)
    (assert-equal '() (history-read-redo-stack *test-session*))))

;;; ====
;;; History List Tests
;;; ====

(test-group "history-list"
  (define-test "history list returns entries"
    (history-write-current-branch! *test-session* "list-test")
    ;; Record a few commands
    (history-record-success! *test-session* "(define a 1)" 'void 'a)
    (history-record-success! *test-session* "(define b 2)" 'void 'b)
    (history-record-success! *test-session* "(+ a b)" 3 #f)
    (let ([entries (history-list *test-session* 10)])
      (assert-true (>= (length entries) 3)))))

;;; ====
;;; Branch Operations Tests
;;; ====

(test-group "branching"
  (define-test "create branch"
    (history-write-current-branch! *test-session* "main")
    (history-record-success! *test-session* "(define main-x 1)" 'void 'main-x)
    (let ([result (history-create-branch! *test-session* 'feature)])
      (assert-equal '(ok) result)
      (assert-equal "feature" (history-read-current-branch *test-session*))))

  (define-test "cannot create duplicate branch"
    (let ([result (history-create-branch! *test-session* 'feature)])
      (assert-true (and (pair? result) (eq? (car result) 'error)))))

  (define-test "checkout branch"
    (let ([result (history-checkout! *test-session* 'main)])
      (assert-equal '(ok) result)
      (assert-equal "main" (history-read-current-branch *test-session*))))

  (define-test "cannot checkout nonexistent branch"
    (let ([result (history-checkout! *test-session* 'nonexistent)])
      (assert-true (and (pair? result) (eq? (car result) 'error)))))

  (define-test "delete branch"
    ;; Make sure we're on main first
    (history-checkout! *test-session* 'main)
    (let ([result (history-delete-branch-op! *test-session* 'feature)])
      (assert-equal '(ok) result)
      (assert-false (history-branch-exists? *test-session* "feature"))))

  (define-test "cannot delete current branch"
    (let ([result (history-delete-branch-op! *test-session* 'main)])
      (assert-true (and (pair? result) (eq? (car result) 'error))))))

;;; ====
;;; Undo/Redo Tests
;;; ====

(test-group "undo-redo"
  (define-test "undo to empty then redo works"
    ;; Use a fresh branch for this test
    (history-write-current-branch! *test-session* "undo-test")
    ;; Record a single command
    (let ([h1 (history-record-success! *test-session* "(define undo-z 99)" 'void 'undo-z)])
      ;; Verify we have one entry
      (assert-true (bytevector? (history-read-branch *test-session* "undo-test")))
      ;; Undo to empty state
      (let ([undo-result (history-undo! *test-session*)])
        (assert-equal 'ok (car undo-result))
        ;; Branch should still exist (not deleted)
        (assert-true (history-branch-exists? *test-session* "undo-test"))
        ;; But branch head should return #f (empty)
        (assert-false (history-read-branch *test-session* "undo-test"))
        ;; Redo should work
        (let ([redo-result (history-redo! *test-session*)])
          (assert-equal 'ok (car redo-result))
          ;; Branch head should be restored
          (assert-true (bytevector? (history-read-branch *test-session* "undo-test"))))))))

;;; ====
;;; Cleanup
;;; ====

(cleanup-test-session!)

;;; ====
;;; Run Tests
;;; ====

(display "\n")
(display "═══════════════════════════════════════════════════════════\n")
(display "              REPL History Module Test Suite                \n")
(display "═══════════════════════════════════════════════════════════\n")
(display "\n")

(run-all-tests)
