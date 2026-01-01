;;; fabric/stitches/test-framework.ss — Unified Test Harness
;;;
;;; Provides a standard testing API for all core/ tests.
;;; Tests self-register when defined and can be run as groups.
;;;
;;; API:
;;;   (define-test name body ...)      — register a named test
;;;   (test-group name tests ...)      — group related tests
;;;   (assert-equal expected actual)   — equality assertion
;;;   (assert-true expr)               — truth assertion
;;;   (assert-false expr)              — falseness assertion
;;;   (assert-error expr)              — verify expr throws error
;;;   (assert-ok expr)                 — verify expr is (ok ...)
;;;   (run-all-tests)                  — run all registered tests
;;;   (run-tests 'group-name)          — run specific group
;;;
;;; Dependencies:
;;;   - prelude.ss (for result types)

(load "core/prelude.ss")

;;; ============================================================
;;; Test Registry
;;; ============================================================

;;; Global test registry: ((group-name . tests) ...)
;;; Each test is: (name . thunk)
(define *test-registry* '())

;;; Current group being defined
(define *current-group* 'default)

;;; Test statistics
(define *tests-run* 0)
(define *tests-passed* 0)
(define *tests-failed* 0)
(define *current-test-name* #f)

;;; ============================================================
;;; Test Registration
;;; ============================================================

;;; register-test : Symbol × (Unit → Unit) → Unit
;;; Register a test in the current group.
(define (register-test name test-thunk)
  (let* ([group *current-group*]
         [group-entry (assq group *test-registry*)]
         [tests (if group-entry (cdr group-entry) '())]
         [new-tests (cons (cons name test-thunk) tests)])
        (if group-entry
            (set-cdr! group-entry new-tests)
            (set! *test-registry* (cons (cons group '()) *test-registry*)))
        (let ([updated-entry (assq group *test-registry*)])
             (set-cdr! updated-entry new-tests))))

;;; ============================================================
;;; Assertion Helpers
;;; ============================================================

;;; assert-equal : Any × Any → Unit
;;; Check that expected equals actual, fail with message if not.
(define (assert-equal expected actual)
  (unless (equal? expected actual)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (write expected)
          (newline)
          (display "      Got:      ")
          (write actual)
          (newline)))

;;; assert-true : Any → Unit
;;; Check that expression is true.
(define (assert-true expr)
  (unless (eq? #t expr)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: #t")
          (newline)
          (display "      Got:      ")
          (write expr)
          (newline)))

;;; assert-false : Any → Unit
;;; Check that expression is false.
(define (assert-false expr)
  (unless (eq? #f expr)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
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
         (set! *tests-failed* (+ *tests-failed* 1))
         (display "    ✗ ")
         (display *current-test-name*)
         (newline)
         (display "      Expected error, but none was raised")
         (newline)))

;;; assert-ok : Result → Unit
;;; Check that result is (ok ...).
(define (assert-ok result)
  (unless (ok? result)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected (ok ...), got: ")
          (write result)
          (newline)))

;;; ============================================================
;;; Test Definition Macros
;;; ============================================================

;;; define-test : Symbol × Expr ... → Unit
;;; Define and register a test, then run it immediately.
(define-syntax define-test
  (syntax-rules ()
                [(_ name body ...)
                 (let ([test-thunk (lambda ()
                                           (set! *current-test-name* 'name)
                                           (set! *tests-run* (+ *tests-run* 1))
                                           body ...)])
                      (register-test 'name test-thunk)
                      (run-test 'name test-thunk))]))

;;; test-group : Symbol × Expr ... → Unit
;;; Execute tests within a named group.
(define-syntax test-group
  (syntax-rules ()
                [(_ name tests ...)
                 (begin
                  (set! *current-group* 'name)
                  tests ...
                  (set! *current-group* 'default))]))

;;; ============================================================
;;; Test Runners
;;; ============================================================

;;; run-test : Symbol × (Unit → Unit) → Unit
;;; Run a single test, catching any errors.
(define (run-test name test-thunk)
  (set! *current-test-name* name)
  (let ([initial-fail-count *tests-failed*])
       (guard (exn [else
                    (set! *tests-failed* (+ *tests-failed* 1))
                    (display "    ✗ ")
                    (display name)
                    (display " — EXCEPTION: ")
                    (display (if (condition? exn)
                                 (condition-message exn)
                                 exn))
                    (newline)])
              (test-thunk))
       ;; If no new failures, the test passed
       (when (= initial-fail-count *tests-failed*)
             (set! *tests-passed* (+ *tests-passed* 1))
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
  (reset-statistics!)
  (display "
╔══════════════════════════════════════════════════════════╗
")
  (display "║              Running Test Group                          ║
")
  (display "╚══════════════════════════════════════════════════════════╝
")
  (run-group group-name)
  (print-summary))

;;; run-all-tests : Unit → Unit
;;; Run all registered tests, grouped by category.
(define (run-all-tests)
  (reset-statistics!)
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
  (print-summary))

;;; ============================================================
;;; Statistics & Reporting
;;; ============================================================

;;; reset-statistics! : Unit → Unit
;;; Reset test counters to zero.
(define (reset-statistics!)
  (set! *tests-run* 0)
  (set! *tests-passed* 0)
  (set! *tests-failed* 0))

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
  (display *tests-run*)
  (display " tests
")
  (display "  Passed: ")
  (display *tests-passed*)
  (newline)
  (display "  Failed: ")
  (display *tests-failed*)
  (newline)
  (newline)
  (if (= *tests-failed* 0)
      (display "✓ All tests passed!
")
      (display "✗ Some tests failed!
")))

;;; exit-with-summary : Unit → Unit
;;; Print summary and exit with appropriate code.
(define (exit-with-summary)
  (print-summary)
  (when (> *tests-failed* 0)
        (exit 1)))

;;; ============================================================
;;; Legacy Compatibility
;;; ============================================================

;;; test : String × Any × Any → Unit
;;; Legacy test function for backward compatibility.
;;; Use assert-equal in new code.
(define (test name expected actual)
  (set! *current-test-name* name)
  (set! *tests-run* (+ *tests-run* 1))
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "  ✓ ")
       (display name)
       (newline))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "  ✗ ")
       (display name)
       (display " — expected ")
       (write expected)
       (display ", got ")
       (write actual)
       (newline))))
