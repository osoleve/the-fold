(load "core/base/prelude.ss")

(doc 'module 'test-framework)
(doc 'description "Unified test harness providing standard testing API for all core/ tests. Tests self-register when defined and can be run as groups.")
(doc 'layer 'core)

(doc 'section 'test-registry)

(doc 'description "Global test registry: ((group-name . tests) ...). Each test is: (name . thunk)")
(define *test-registry* '())

(doc 'description "Current group being defined (thread-local parameter)")
(define current-group (make-parameter 'default))

(doc 'section 'test-context)

(doc 'description "Test context vector for mutable state: #(run passed failed name)")
(define (make-test-context)
  (doc 'type (-> TestContext))
  (doc 'description "Create a new test context vector.")
  (doc 'export #t)
  (vector 0 0 0 #f))

(doc 'description "Current test context (thread-local parameter)")
(define current-context (make-parameter (make-test-context)))

(doc 'section 'context-accessors)

(define (ctx-run)
  (doc 'type (-> Nat))
  (doc 'description "Get number of tests run from current context.")
  (doc 'export #t)
  (vector-ref (current-context) 0))

(define (ctx-passed)
  (doc 'type (-> Nat))
  (doc 'description "Get number of tests passed from current context.")
  (doc 'export #t)
  (vector-ref (current-context) 1))

(define (ctx-failed)
  (doc 'type (-> Nat))
  (doc 'description "Get number of tests failed from current context.")
  (doc 'export #t)
  (vector-ref (current-context) 2))

(define (ctx-name)
  (doc 'type (-> (Maybe Symbol)))
  (doc 'description "Get current test name from context.")
  (doc 'export #t)
  (vector-ref (current-context) 3))

(doc 'section 'context-mutators)

(define (inc-run!)
  (doc 'type (-> Unit))
  (doc 'description "Increment tests run counter in current context.")
  (doc 'export #t)
  (vector-set! (current-context) 0 (+ (ctx-run) 1)))

(define (inc-passed!)
  (doc 'type (-> Unit))
  (doc 'description "Increment tests passed counter in current context.")
  (doc 'export #t)
  (vector-set! (current-context) 1 (+ (ctx-passed) 1)))

(define (inc-failed!)
  (doc 'type (-> Unit))
  (doc 'description "Increment tests failed counter in current context.")
  (doc 'export #t)
  (vector-set! (current-context) 2 (+ (ctx-failed) 1)))

(define (set-name! name)
  (doc 'type (-> Symbol Unit))
  (doc 'description "Set current test name in context.")
  (doc 'export #t)
  (vector-set! (current-context) 3 name))

(define (reset-statistics!)
  (doc 'type (-> Unit))
  (doc 'description "Reset all test statistics in current context to zero.")
  (doc 'export #t)
  (let ([ctx (current-context)])
    (vector-set! ctx 0 0)
    (vector-set! ctx 1 0)
    (vector-set! ctx 2 0)
    (vector-set! ctx 3 #f)))

(doc 'section 'backwards-compatibility)

(doc 'description "Backwards compatibility globals mapped to context via macros.")
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

(doc 'section 'test-registration)

(define (register-test name test-thunk)
  (doc 'type (-> Symbol (-> Unit) Unit))
  (doc 'description "Register a test in the current group.")
  (doc 'export #t)
  (let* ([group (current-group)]
         [group-entry (assq group *test-registry*)]
         [tests (if group-entry (cdr group-entry) '())]
         [new-tests (cons (cons name test-thunk) tests)])
        (if group-entry
            (set-cdr! group-entry new-tests)
            (set! *test-registry* (cons (cons group '()) *test-registry*)))
        (let ([updated-entry (assq group *test-registry*)])
             (set-cdr! updated-entry new-tests))))

(doc 'section 'assertion-helpers)

(define (assert-equal expected actual . msg)
  (doc 'type (-> Any Any (List String) Unit))
  (doc 'description "Check that expected equals actual, fail with message if not. Optional third argument is a custom failure message.")
  (doc 'export #t)
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

(define (assert-true expr . msg)
  (doc 'type (-> Any (List String) Unit))
  (doc 'description "Check that expression is exactly #t. Optional second argument is a custom failure message.")
  (doc 'export #t)
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

(define (assert-false expr . msg)
  (doc 'type (-> Any (List String) Unit))
  (doc 'description "Check that expression is exactly #f. Optional second argument is a custom failure message.")
  (doc 'export #t)
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

(define (assert-error thunk)
  (doc 'type (-> (-> Any) Unit))
  (doc 'description "Check that thunk raises an error when called.")
  (doc 'export #t)
  (guard (exn [else #t])
         (thunk)
         (inc-failed!)
         (display "    ✗ ")
         (display (ctx-name))
         (newline)
         (display "      Expected error, but none was raised")
         (newline)))

(define (assert-ok result . msg)
  (doc 'type (-> Result (List String) Unit))
  (doc 'description "Check that result is (ok ...). Optional second argument is a custom failure message.")
  (doc 'export #t)
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

(doc 'section 'test-definition-macros)

(doc 'description "Define and register a test, then run it immediately.")
(define-syntax define-test
  (syntax-rules ()
                [(_ name body ...)
                 (let ([test-thunk (lambda ()
                                           (set-name! 'name)
                                           (inc-run!)
                                           body ...)])
                      (register-test 'name test-thunk)
                      (run-test 'name test-thunk))]))

(doc 'description "Execute tests within a named group.")
(define-syntax test-group
  (syntax-rules ()
                [(_ name tests ...)
                 (parameterize ([current-group 'name])
                   tests ...)]))

(doc 'section 'test-runners)

(define (run-test name test-thunk)
  (doc 'type (-> Symbol (-> Unit) Unit))
  (doc 'description "Run a single test, catching any errors.")
  (doc 'export #t)
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
       (when (= initial-fail-count (ctx-failed))
             (inc-passed!)
             (display "    ✓ ")
             (display name)
             (newline))))

(define (run-group group-name)
  (doc 'type (-> Symbol Unit))
  (doc 'description "Run all tests in a specific group.")
  (doc 'export #t)
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

(define (run-tests group-name)
  (doc 'type (-> Symbol Unit))
  (doc 'description "Run tests in a specific named group.")
  (doc 'export #t)
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

(define (run-all-tests)
  (doc 'type (-> Unit))
  (doc 'description "Run all registered tests, grouped by category.")
  (doc 'export #t)
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

(doc 'section 'statistics-and-reporting)

(define (print-summary)
  (doc 'type (-> Unit))
  (doc 'description "Print test run summary with machine-parseable result line.")
  (doc 'export #t)
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
"))
  ;; Machine-parseable result line for structured test detection
  (printf "[TEST-RESULT total=~a passed=~a failed=~a]~n"
          (ctx-run) (ctx-passed) (ctx-failed)))

(define (exit-with-summary)
  (doc 'type (-> Unit))
  (doc 'description "Print summary and exit with appropriate code. Must be called within the same parameterize context as run-all-tests.")
  (doc 'export #t)
  (print-summary)
  (when (> (ctx-failed) 0)
        (exit 1)))

(define (run-all-tests-and-exit)
  (doc 'type (-> Never))
  (doc 'description "Run all tests and exit with appropriate code (0=all passed, 1=some failed). Use this for standalone test scripts run via scheme --script.")
  (doc 'export #t)
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
    (exit-with-summary)))

(doc 'section 'legacy-compatibility)

(doc test 'type (-> String Any Any Unit))
(doc test 'description "Legacy test function for backward compatibility. Use assert-equal in new code.")
(doc test 'export #t)
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
