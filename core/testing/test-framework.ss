;;; core/testing/test-framework.ss — Unified Test Harness
;;;
;;; Provides a standard testing API for all core/ tests.
;;; Tests self-register when defined and can be run as groups.
;;;
;;; API:
;;;   (define-test name body ...)      — register a named test
;;;   (test-group name tests ...)      — group related tests
;;;   (assert-equal expected actual [msg])  — equality assertion
;;;   (assert-true expr [msg])         — truth assertion
;;;   (assert-false expr [msg])        — falseness assertion
;;;   (assert-error expr)              — verify expr throws error
;;;   (assert-ok expr [msg])           — verify expr is (ok ...)
;;;   (run-all-tests)                  — run all registered tests
;;;   (run-tests 'group-name)          — run specific group
;;;
;;; All assertions accept an optional message string that appears on failure.
;;;
;;; Dependencies:
;;;   - prelude.ss (for result types)

(load "core/base/prelude.ss")

;;; ====
;;; Test Registry
;;; ====

;;; Global test registry: ((group-name . tests) ...)
;;; Each test is: (name . thunk)
(define *test-registry* '())

;;; Current group being defined (thread-local parameter)
(define current-group (make-parameter 'default))

;;; ====
;;; Test Context (State)
;;; ====

;;; We use a vector for mutable state: #(run passed failed name)
(define (make-test-context) (vector 0 0 0 #f))

;;; Current test context (thread-local parameter)
(define current-context (make-parameter (make-test-context)))

;;; Accessors
(define (ctx-run) (vector-ref (current-context) 0))
(define (ctx-passed) (vector-ref (current-context) 1))
(define (ctx-failed) (vector-ref (current-context) 2))
(define (ctx-name) (vector-ref (current-context) 3))

;;; Mutators
(define (inc-run!) (vector-set! (current-context) 0 (+ (ctx-run) 1)))
(define (inc-passed!) (vector-set! (current-context) 1 (+ (ctx-passed) 1)))
(define (inc-failed!) (vector-set! (current-context) 2 (+ (ctx-failed) 1)))
(define (set-name! name) (vector-set! (current-context) 3 name))

;;; Reset
(define (reset-statistics!)
  (let ([ctx (current-context)])
    (vector-set! ctx 0 0)
    (vector-set! ctx 1 0)
    (vector-set! ctx 2 0)
    (vector-set! ctx 3 #f)))

;;; Backwards compatibility globals (mapped to context)
;;; These are macros to redirect access to the context
(define-syntax *tests-run*
  (identifier-syntax
    [id (vector-ref (current-context) 0)]
    [(set! id val) (vector-set! (current-context) 0 val)]))

(define-syntax *tests-passed*
  (identifier-syntax
    [id (vector-ref (current-context) 1)]
    [(set! id val) (vector-set! (current-context) 1 val)]))

(define-syntax *tests-failed*
  (identifier-syntax
    [id (vector-ref (current-context) 2)]
    [(set! id val) (vector-set! (current-context) 2 val)]))

(define-syntax *current-test-name*
  (identifier-syntax
    [id (vector-ref (current-context) 3)]
    [(set! id val) (vector-set! (current-context) 3 val)]))

;;; ====
;;; Test Registration
;;; ====

;;; register-test : Symbol × (Unit → Unit) → Unit
;;; Register a test in the current group.
(define (register-test name test-thunk)
  (let* ([group (current-group)]
         [group-entry (assq group *test-registry*)]
         [tests (if group-entry (cdr group-entry) '())]
         [new-tests (cons (cons name test-thunk) tests)])
        (if group-entry
            (set-cdr! group-entry new-tests)
            (set! *test-registry* (cons (cons group '()) *test-registry*)))
        (let ([updated-entry (assq group *test-registry*)])
             (set-cdr! updated-entry new-tests))))

;;; ====
;;; Assertion Helpers
;;; ====

;;; assert-equal : Any × Any [× String] → Unit
;;; Check that expected equals actual, fail with message if not.
;;; Optional third argument is a custom failure message.
(define (assert-equal expected actual . msg)
  (unless (equal? expected actual)
          (inc-failed!)
          (display "    ✗ ")
          (display (ctx-name))
          (when (pair? msg)
            (display " — ")
            (display (car msg)))
          (newline)
          (display "      Expected: ")
          (write expected)
          (newline)
          (display "      Got:      ")
          (write actual)
          (newline)))

;;; assert-true : Any [× String] → Unit
;;; Check that expression is true.
;;; Optional second argument is a custom failure message.
(define (assert-true expr . msg)
  (unless (eq? #t expr)
          (inc-failed!)
          (display "    ✗ ")
          (display (ctx-name))
          (when (pair? msg)
            (display " — ")
            (display (car msg)))
          (newline)
          (display "      Expected: #t")
          (newline)
          (display "      Got:      ")
          (write expr)
          (newline)))

;;; assert-false : Any [× String] → Unit
;;; Check that expression is false.
;;; Optional second argument is a custom failure message.
(define (assert-false expr . msg)
  (unless (eq? #f expr)
          (inc-failed!)
          (display "    ✗ ")
          (display (ctx-name))
          (when (pair? msg)
            (display " — ")
            (display (car msg)))
          (newline)
          (display "      Expected: #f")
          (newline)
          (display "      Got:      ")
          (write expr)
          (newline)))

;;; assert-error : (Unit → Any) → Unit
;;; Check that thunk raises an error when called.
(define (assert-error thunk)
  (guard (exn [else #t])  ; Error raised as expected
         (thunk)
         ;; If we get here, no error was raised
         (inc-failed!)
         (display "    ✗ ")
         (display (ctx-name))
         (newline)
         (display "      Expected error, but none was raised")
         (newline)))

;;; assert-ok : Result [× String] → Unit
;;; Check that result is (ok ...).
;;; Optional second argument is a custom failure message.
(define (assert-ok result . msg)
  (unless (ok? result)
          (inc-failed!)
          (display "    ✗ ")
          (display (ctx-name))
          (when (pair? msg)
            (display " — ")
            (display (car msg)))
          (newline)
          (display "      Expected (ok ...), got: ")
          (write result)
          (newline)))

;;; ====
;;; Test Definition Macros
;;; ====

;;; define-test : Symbol × Expr ... → Unit
;;; Define and register a test, then run it immediately.
(define-syntax define-test
  (syntax-rules ()
                [(_ name body ...)
                 (let ([test-thunk (lambda ()
                                           (set-name! 'name)
                                           (inc-run!)
                                           body ...)])
                      (register-test 'name test-thunk)
                      (run-test 'name test-thunk))]))

;;; test-group : Symbol × Expr ... → Unit
;;; Execute tests within a named group.
(define-syntax test-group
  (syntax-rules ()
                [(_ name tests ...)
                 (parameterize ([current-group 'name])
                   tests ...)]))

;;; ====
;;; Test Runners
;;; ====

;;; run-test : Symbol × (Unit → Unit) → Unit
;;; Run a single test, catching any errors.
(define (run-test name test-thunk)
  (set-name! name)
  (let ([initial-fail-count (ctx-failed)])
       (guard (exn [else
                    (inc-failed!)
                    (display "    ✗ ")
                    (display name)
                    (display " — EXCEPTION: ")
                    (display (if (condition? exn)
                                 (condition-message exn)
                                 exn))
                    (newline)])
              (test-thunk))
       ;; If no new failures, the test passed
       (when (= initial-fail-count (ctx-failed))
             (inc-passed!)
             (display "    ✓ ")
             (display name)
             (newline))))

;;; run-group : Symbol → Unit
;;; Run all tests in a specific group.
(define (run-group group-name)
  (let ([group-entry (assq group-name *test-registry*)])
       (if group-entry
           (let ([tests (reverse (cdr group-entry))])
                (display "  Group: ")
                (display group-name)
                (newline)
                (for-each (lambda (test)
                                  (run-test (car test) (cdr test)))
                          tests))
           (begin
            (display "  Warning: No tests found for group ")
            (display group-name)
            (newline)))))

;;; run-tests : Symbol → Unit
;;; Run tests in a specific named group.
(define (run-tests group-name)
  ;; We parameterize with a FRESH context for this run
  ;; so stats are isolated to this call.
  (parameterize ([current-context (make-test-context)])
    (display "
╔══════════════════════════════════════════════════════════╗
")
    (display "║              Running Test Group                          ║
")
    (display "╚══════════════════════════════════════════════════════════╝
")
    (run-group group-name)
    (print-summary)))

;;; run-all-tests : Unit → Unit
;;; Run all registered tests, grouped by category.
(define (run-all-tests)
  ;; Isolate stats
  (parameterize ([current-context (make-test-context)])
    (display "
╔══════════════════════════════════════════════════════════╗
")
    (display "║           Running All Registered Tests                   ║
")
    (display "╚══════════════════════════════════════════════════════════╝
")
    (newline)
    (for-each (lambda (group-entry)
                      (run-group (car group-entry)))
              (reverse *test-registry*))
    (print-summary)))

;;; ====
;;; Statistics & Reporting
;;; ====

;;; print-summary : Unit → Unit
;;; Print test run summary.
(define (print-summary)
  (newline)
  (display "╔══════════════════════════════════════════════════════════╗
")
  (display "║                    TEST SUMMARY                          ║
")
  (display "╚══════════════════════════════════════════════════════════╝
")
  (display "  Total:  ")
  (display (ctx-run))
  (display " tests
")
  (display "  Passed: ")
  (display (ctx-passed))
  (newline)
  (display "  Failed: ")
  (display (ctx-failed))
  (newline)
  (newline)
  (if (= (ctx-failed) 0)
      (display "✓ All tests passed!
")
      (display "✗ Some tests failed!
")))

;;; exit-with-summary : Unit → Unit
;;; Print summary and exit with appropriate code.
(define (exit-with-summary)
  (print-summary)
  (when (> (ctx-failed) 0)
        (exit 1)))

;;; ====
;;; Legacy Compatibility
;;; ====

;;; test : String × Any × Any → Unit
;;; Legacy test function for backward compatibility.
;;; Use assert-equal in new code.
(define (test name expected actual)
  (set-name! name)
  (inc-run!)
  (if (equal? expected actual)
      (begin
       (inc-passed!)
       (display "  ✓ ")
       (display name)
       (newline))
      (begin
       (inc-failed!)
       (display "  ✗ ")
       (display name)
       (display " — expected ")
       (write expected)
       (display ", got ")
       (write actual)
       (newline))))