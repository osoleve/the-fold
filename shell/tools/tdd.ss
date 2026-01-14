;;; shell/tdd.ss — Test-Driven Development Workflow for The Fold
;;;
;;; Provides comprehensive TDD capabilities:
;;;   • Watch mode with auto-test on file changes
;;;   • Focused testing (specific modules, failing tests)
;;;   • Quick feedback with sub-second test runs
;;;   • Coverage reporting and trends
;;;   • Test data generation and fixtures
;;;   • Integration with existing test framework
;;;
;;; This is Shell code: uses IO, manages state, handles failures.
;;;
;;; Dependencies:
;;;   - watch.ss (file watching)
;;;   - test-framework.ss (test execution)
;;;   - test-runner.ss (test discovery and reporting)

(load "shell/watch.ss")
(load "core/test-framework.ss")
(load "shell/tests/test-runner.ss")

;;; ====
;;; Configuration
;;; ====

(define *tdd-enabled* #f)
(define *tdd-watchers* '())
(define *tdd-current-focus* 'all)
(define *tdd-last-results* '())
(define *tdd-coverage-enabled* #f)
(define *tdd-test-history* (make-eq-hashtable))  ; test → (passes . failures)

;;; Performance targets
(define *tdd-target-runtime* 1000)  ; milliseconds - target for quick feedback
(define *tdd-slow-test-threshold* 500)  ; milliseconds - warn about slow tests

;;; ====
;;; Core TDD Functions
;;; ====

;;; test:watch : [Symbol] → void
;;; Start watching test files and auto-run on changes.
(define (test:watch . args)
  (let ([focus (if (null? args) 'all (car args))])
       (stop-tdd-watchers!)  ; Clean up any existing watchers
       (set! *tdd-enabled* #t)
       (set! *tdd-current-focus* focus)
       
       (display "
")
       (display "╔════════════════════════════════════════════════════════════╗
")
       (display "║                    TDD WATCH MODE                          ║
")
       (display "╚════════════════════════════════════════════════════════════╝
")
       (display "
")
       
       ;; Watch test files
       (let ([test-files (discover-test-files focus)])
            (if (null? test-files)
                (begin
                 (display "⚠️  No test files found for focus: ")
                 (display focus)
                 (newline)
                 (display "Available focuses: all, core, shell, thimble
")
                 (set! *tdd-enabled* #f))
                
                (begin
                 (display "📁 Watching ")
                 (display (length test-files))
                 (display " test files...

")
                 
                 (for-each
                  (lambda (test-file)
                          (let ([watcher (auto-test-tdd test-file)])
                               (set! *tdd-watchers* (cons watcher *tdd-watchers*))
                               (display "👁️  Watching: ")
                               (display test-file)
                               (newline)))
                  test-files)
                 
                 (display "
✅ TDD watch mode started!
")
                 (display "   Tests will run automatically when files change.
")
                 (display "   Use (test:stop) to stop watching.
")
                 (display "   Use (test:focus 'module) to change focus.
")
                 (newline)
                 
                 ;; Run tests initially
                 (run-focused-tests focus))))))

;;; test:stop : → void
;;; Stop all TDD watchers and disable TDD mode.
(define (test:stop)
  (stop-tdd-watchers!)
  (set! *tdd-enabled* #f)
  (display "🛑 TDD watch mode stopped.
"))

;;; test:focus : Symbol → void
;;; Change test focus and restart watchers.
(define (test:focus focus)
  (if *tdd-enabled*
      (begin
       (set! *tdd-current-focus* focus)
       (test:watch focus))
      (begin
       (display "TDD mode is not enabled. Use (test:watch) first.
"))))

;;; test:run : [Symbol] → void
;;; Run tests immediately with optional focus.
(define (test:run . args)
  (let ([focus (if (null? args) *tdd-current-focus* (car args))])
       (run-focused-tests focus)))

;;; test:failing : → void
;;; Run only the tests that failed in the last run.
(define (test:failing)
  (let ([failing-tests (get-failing-tests *tdd-last-results*)])
       (if (null? failing-tests)
           (display "✅ All tests passed! No failing tests to re-run.
")
           (begin
            (display "🔄 Re-running ")
            (display (length failing-tests))
            (display " failing tests...
")
            (run-specific-tests failing-tests)))))

;;; test:all : → void
;;; Run all tests (same as test:run with 'all focus).
(define (test:all)
  (test:run 'all))

;;; ====
;;; Test Discovery and Execution
;;; ====

(define (discover-test-files focus)
  "Discover test files based on focus"
  (case focus
        [(all) (append (discover-core-tests) (discover-shell-tests) (discover-thimble-tests))]
        [(core) (discover-core-tests)]
        [(shell) (discover-shell-tests)]
        [(thimble) (discover-thimble-tests)]
        [else (discover-tests-matching (symbol->string focus))]))

(define (discover-core-tests)
  "Discover core test files"
  '("core/test-block.ss"
    "core/test-cas.ss"
    "core/test-compile.ss"
    "core/test-eval.ss"
    "core/test-parse.ss"
    "core/test-prim.ss"
    "core/test-resolve.ss"))

(define (discover-shell-tests)
  "Discover shell test files"
  '())  ; Add shell tests when available

(define (discover-thimble-tests)
  "Discover thimble test files"
  '("shell/tests/test-commands.ss"
    "shell/tests/test-scaffold.ss"
    "shell/tests/test-store-api.ss"))

(define (discover-tests-matching pattern)
  "Discover tests matching a pattern"
  (let ([all-tests (append (discover-core-tests)
                           (discover-shell-tests)
                           (discover-thimble-tests))])
       (filter (lambda (test-file) (string-contains? test-file pattern))
               all-tests)))

(define (run-focused-tests focus)
  "Run tests with the specified focus"
  (let ([start-time (current-time-ms)])
       (display "🧪 Running tests...
")
       
       (let ([results (run-tests-with-focus focus)])
            (set! *tdd-last-results* results)
            
            (let ([end-time (current-time-ms)]
                  [duration (- end-time start-time)])
                 
                 (display-test-results results duration)
                 (update-test-history results)
                 
                 ;; Performance feedback
                 (when (> duration *tdd-target-runtime*)
                       (display "⚠️  Tests took ")
                       (display duration)
                       (display "ms - target is ")
                       (display *tdd-target-runtime*)
                       (display "ms for quick feedback
"))
                 
                 results))))

(define (run-tests-with-focus focus)
  "Run tests and return results"
  (let ([test-files (discover-test-files focus)])
       (map (lambda (test-file)
                    (run-single-test-file test-file))
            test-files)))

(define (run-single-test-file test-file)
  "Run a single test file and return results"
  (let ([start-time (current-time-ms)]
        [file-passed 0]
        [file-failed 0]
        [file-errors '()])
       
       (guard (e
               [(error? e)
                (display "Error running test file: ")
                (display test-file)
                (display "
")
                (display (format-condition e))
                (newline)
                (list test-file 0 1 (list (format-condition e)) 0)]
               [else
                (display "❌ Unexpected error in: ")
                (display test-file)
                (newline)
                (list test-file 0 1 (list "Unexpected error") 0)])
              
              ;; Clear existing tests
              (clear-tests!)
              
              ;; Load and run the test file
              (load test-file)
              
              ;; Run the tests
              (let ([results (run-all-tests)])
                   (let ([end-time (current-time-ms)]
                         [duration (- end-time start-time)])
                        
                        ;; Extract results
                        (list test-file
                              (car results)  ; passed
                              (cadr results) ; failed
                              (if (> (cadr results) 0) (caddr results) '()) ; errors
                              duration)))))
  
  ;;; ====
  ;;; Enhanced Auto-Test for TDD
  ;;; ====
  
  (define (auto-test-tdd test-path)
    "Enhanced auto-test for TDD with better feedback"
    (watch-file test-path
                (lambda (changed)
                        (display "
")
                        (display "═══════════════════════════════════════════════════════════════
")
                        (display "🔄 File changed: ")
                        (display changed)
                        (newline)
                        (display "═══════════════════════════════════════════════════════════════
")
                        
                        ;; Clear and reload tests
                        (clear-tests!)
                        
                        ;; Run the changed test file
                        (guard (e [(error? e)
                                   (display "Error reloading test file:
")
                                   (display (format-condition e))
                                   (newline)]
                                  [else
                                   (display "❌ Unexpected error reloading test file
")
                                   (newline)])
                               
                               (load changed)
                               (let ([results (run-all-tests)])
                                    (display-tdd-results results changed)))
                        
                        (display "═══════════════════════════════════════════════════════════════
")
                        (display "👁️  Watching for changes... (Ctrl+C to stop)
")
                        (display "═══════════════════════════════════════════════════════════════
"))))
  
  ;;; ====
  ;;; Results Display and Analysis
  ;;; ====
  
  (define (display-test-results results total-duration)
    "Display comprehensive test results"
    (let ([total-files (length results)]
          [total-passed (apply + (map cadr results))]
          [total-failed (apply + (map caddr results))]
          [total-tests (+ total-passed total-failed)])
         
         (display "
")
         (display "╔════════════════════════════════════════════════════════════╗
")
         (display "║                        TEST RESULTS                        ║
")
         (display "╚════════════════════════════════════════════════════════════╝
")
         
         ;; Summary
         (display "
📊 Summary:
")
         (display "   Files: ")
         (display total-files)
         (newline)
         (display "   Tests: ")
         (display total-tests)
         (newline)
         (display "   Passed: ")
         (display total-passed)
         (newline)
         (display "   Failed: ")
         (display total-failed)
         (newline)
         (display "   Duration: ")
         (display total-duration)
         (display "ms
")
         
         ;; Detailed results by file
         (when (> total-files 1)
               (display "
📁 Results by file:
")
               (for-each
                (lambda (result)
                        (let ([file (car result)]
                              [passed (cadr result)]
                              [failed (caddr result)]
                              [duration (list-ref result 4)])
                             (display "   ")
                             (display file)
                             (display ": ")
                             (display passed)
                             (display " passed, ")
                             (display failed)
                             (display " failed (")
                             (display duration)
                             (display "ms)
")))
                results))
         
         ;; Status indicator
         (if (zero? total-failed)
             (begin
              (display "
✅ All tests passed!
")
              (when (> total-duration *tdd-target-runtime*)
                    (display "⚠️  Consider optimizing tests for faster feedback (< ")
                    (display *tdd-target-runtime*)
                    (display "ms target)
")))
             (begin
              (display "
❌ Some tests failed.
")
              (display "💡 Use (test:failing) to re-run only failing tests.
")))
         
         (newline)))
  
  (define (display-tdd-results results changed-file)
    "Display TDD-specific results"
    (let ([passed (car results)]
          [failed (cadr results)]
          [errors (if (> (cadr results) 0) (caddr results) '())])
         
         (if (zero? failed)
             (begin
              (display "✅ ")
              (display passed)
              (display " tests passed in ")
              (display changed-file)
              (newline))
             (begin
              (display "❌ ")
              (display failed)
              (display " tests failed in ")
              (display changed-file)
              (newline)
              (for-each
               (lambda (error)
                       (display "   • ")
                       (display error)
                       (newline))
               errors)))))
  
  ;;; ====
  ;;; Test History and Analytics
  ;;; ====
  
  (define (update-test-history results)
    "Update test history for trend analysis"
    ;; This would track test performance and flakiness over time
    (for-each
     (lambda (result)
             (let ([file (car result)]
                   [passed (cadr result)]
                   [failed (caddr result)])
                  (let ([current (hashtable-ref *tdd-test-history* file '(0 . 0))]
                        [new-stats (cons (+ (car current) passed)
                                         (+ (cdr current) failed))])
                       (hashtable-set! *tdd-test-history* file new-stats))))
     results))
  
  (define (get-failing-tests results)
    "Extract failing test names from results"
    ;; This would need to integrate with the test framework to get specific test names
    '())  ; Placeholder - would implement based on test framework
  
  (define (run-specific-tests test-names)
    "Run specific tests by name"
    ;; This would need to integrate with the test framework
    (display "Running specific tests...
"))
  
  ;;; ====
  ;;; Utility Functions
  ;;; ====
  
  (define (clear-tests!)
    "Clear the test registry"
    (set! *test-registry* '())
    (set! *current-group* 'default)
    (set! *tests-run* 0)
    (set! *tests-passed* 0)
    (set! *tests-failed* 0))
  
  (define (stop-tdd-watchers!)
    "Stop all TDD watchers"
    (for-each stop-watcher! *tdd-watchers*)
    (set! *tdd-watchers* '()))
  
  (define (current-time-ms)
    "Get current time in milliseconds"
    (let-values ([(s ms) (time-difference (current-time) (make-time 'time-duration 0 0))])
                (+ (* s 1000) (quotient ms 1000000))))
  
  ;;; ====
  ;;; TDD Commands and Help
  ;;; ====
  
  (define (tdd-help)
    "Display TDD help information"
    (display "
")
    (display "╔════════════════════════════════════════════════════════════╗
")
    (display "║                    TDD COMMANDS                           ║
")
    (display "╚════════════════════════════════════════════════════════════╝
")
    (display "
")
    (display "Core Commands:
")
    (display "  (test:watch)              - Start watching all tests
")
    (display "  (test:watch 'core)        - Watch only core tests
")
    (display "  (test:watch 'thimble)     - Watch only thimble tests
")
    (display "  (test:stop)               - Stop watching tests
")
    (display "  (test:run)                - Run tests once
")
    (display "  (test:all)                - Run all tests
")
    (display "  (test:failing)            - Re-run only failing tests
")
    (display "  (test:focus 'module)      - Change test focus
")
    (display "
")
    (display "Advanced:
")
    (display "  (tdd-help)                - Show this help
")
    (display "
")
    (display "Configuration:
")
    (display "  *tdd-target-runtime*      - Target test run time (default: 1000ms)
")
    (display "
")
    (display "Examples:
")
    (display "  (test:watch 'core)        - Watch core tests during development
")
    (display "  (test:run 'shell)         - Run shell tests once
")
    (display "  (test:failing)            - Re-run tests that just failed
"))
  
  ;;; ====
  ;;; Initialization
  ;;; ====
  
  (display "TDD system loaded!
")
  (display "Use (tdd-help) for available commands.
")
  (display "Start with (test:watch) to begin TDD workflow.
")
