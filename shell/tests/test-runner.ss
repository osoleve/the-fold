;;; thimble/test-runner.ss — Comprehensive Test Runner for The Fold
;;;
;;; ============================================================
;;; OVERVIEW
;;; ============================================================
;;;
;;; A production-ready test runner designed for The Fold's test suite.
;;; Provides fast, reliable test execution with rich reporting and
;;; multiple execution modes.
;;;
;;; Features:
;;;   • Auto-discovery of test files (test-*.ss, *-test.ss patterns)
;;;   • Detailed timing information per test file
;;;   • Color-coded output for immediate visual feedback
;;;   • Pattern-based filtering for focused testing
;;;   • Summary statistics including slowest tests
;;;   • REPL integration for interactive development
;;;   • Proper error handling and reporting
;;;   • Exit codes for CI/CD integration
;;;
;;; ============================================================
;;; USAGE
;;; ============================================================
;;;
;;; Command Line:
;;;   scheme --script thimble/tests/test-runner.ss              Run all tests
;;;   scheme --script thimble/tests/test-runner.ss core         Run core/ tests only
;;;   scheme --script thimble/tests/test-runner.ss shell        Run shell/ tests only
;;;   scheme --script thimble/tests/test-runner.ss --pattern <text>
;;;                                                     Filter tests by pattern
;;;   scheme --script thimble/tests/test-runner.ss --list       List available tests
;;;   scheme --script thimble/tests/test-runner.ss --help       Show help message
;;;
;;; From REPL (interactive):
;;;   (load "shell/test-runner.ss")
;;;   (run-tests)                      Run all tests
;;;   (run-tests 'core)                Run core/ tests only
;;;   (run-tests 'shell)               Run shell/ tests only
;;;   (run-tests-matching "block")     Filter by pattern
;;;   (run-test-file "core/test-block.ss")  Run single file
;;;   (list-available-tests)           Show available tests
;;;
;;; ============================================================
;;; OUTPUT
;;; ============================================================
;;;
;;; Test results use color coding:
;;;   GREEN  - Test passed
;;;   RED    - Test failed
;;;   YELLOW - Warning or skipped
;;;   CYAN   - Test file names
;;;   GRAY   - Timing information
;;;
;;; Summary includes:
;;;   • Total test files run
;;;   • Pass/fail counts
;;;   • Total execution time
;;;   • Top 5 slowest tests
;;;   • Detailed error messages for failures
;;;
;;; Exit codes:
;;;   0 - All tests passed
;;;   1 - One or more tests failed
;;;
;;; ============================================================
;;; INTEGRATION
;;; ============================================================
;;;
;;; CI/CD:
;;;   The runner exits with code 1 on failure, making it suitable
;;;   for continuous integration pipelines.
;;;
;;; REPL Daemon:
;;;   Can be called from within REPL sessions. Test state is isolated
;;;   per invocation - no global pollution.
;;;
;;; ============================================================
;;; ARCHITECTURE
;;; ============================================================
;;;
;;; The test runner operates in three phases:
;;;
;;; 1. Discovery
;;;    - Curated lists of stable tests (*-test-files*)
;;;    - Pattern-based filtering when requested
;;;
;;; 2. Execution
;;;    - Each test file runs in isolated guard block
;;;    - Timing measured per file
;;;    - Errors caught and recorded
;;;
;;; 3. Reporting
;;;    - Results aggregated across all runs
;;;    - Summary statistics computed
;;;    - Exit code determined
;;;
;;; Test files are loaded via (load path) which executes them
;;; immediately. Most test files use the test-framework.ss which
;;; provides define-test, assert-equal, etc.
;;;
;;; ============================================================
;;; DEPENDENCIES
;;; ============================================================
;;;
;;; Required:
;;;   - None (standalone)
;;;
;;; Optional (for tests themselves):
;;;   - core/test-framework.ss (used by most test files)
;;;   - core/prelude.ss (utilities)

;;; ============================================================
;;; ANSI Color Codes
;;; ============================================================

(define *use-color* #t)  ; Can be disabled for non-TTY output

(define (ansi-code code)
  (if *use-color*
      (string-append "\x1b;[" code "m")
      ""))

(define ansi-reset     (ansi-code "0"))
(define ansi-bold      (ansi-code "1"))
(define ansi-red       (ansi-code "31"))
(define ansi-green     (ansi-code "32"))
(define ansi-yellow    (ansi-code "33"))
(define ansi-blue      (ansi-code "34"))
(define ansi-magenta   (ansi-code "35"))
(define ansi-cyan      (ansi-code "36"))
(define ansi-gray      (ansi-code "90"))

(define (colorize color text)
  (string-append color text ansi-reset))

(define (red text)     (colorize ansi-red text))
(define (green text)   (colorize ansi-green text))
(define (yellow text)  (colorize ansi-yellow text))
(define (blue text)    (colorize ansi-blue text))
(define (magenta text) (colorize ansi-magenta text))
(define (cyan text)    (colorize ansi-cyan text))
(define (gray text)    (colorize ansi-gray text))
(define (bold text)    (colorize ansi-bold text))

;;; ============================================================
;;; Timing Utilities
;;; ============================================================

(define (current-time-ms)
  (let ([t (current-time)])
       (+ (* (time-second t) 1000)
          (quotient (time-nanosecond t) 1000000))))

(define (format-duration-ms ms)
  (cond
   [(< ms 1) "< 1ms"]
   [(< ms 1000) (string-append (number->string ms) "ms")]
   [(< ms 60000)
    (let ([sec (quotient ms 1000)]
          [msec (remainder ms 1000)])
         (string-append (number->string sec) "."
                        (number->string (quotient msec 100)) "s"))]
   [else
    (let ([min (quotient ms 60000)]
          [sec (quotient (remainder ms 60000) 1000)])
         (string-append (number->string min) "m "
                        (number->string sec) "s"))]))

;;; ============================================================
;;; Test Discovery
;;; ============================================================

;;; string-contains? : String × String → Boolean
;;; Check if needle is a substring of haystack
(define (string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i n-len) h-len) #f]
             [(string=? (substring haystack i (+ i n-len)) needle) #t]
             [else (loop (+ i 1))]))))

;;; string-starts-with? : String × String → Boolean
(define (string-starts-with? str prefix)
  (let ([s-len (string-length str)]
        [p-len (string-length prefix)])
       (and (>= s-len p-len)
            (string=? (substring str 0 p-len) prefix))))

;;; string-ends-with? : String × String → Boolean
(define (string-ends-with? str suffix)
  (let ([s-len (string-length str)]
        [x-len (string-length suffix)])
       (and (>= s-len x-len)
            (string=? (substring str (- s-len x-len) s-len) suffix))))

;;; list-directory : String → (List String)
;;; List all files in a directory using system commands
(define (list-directory dir)
  (guard (e [else '()])
         ;; Detect Windows vs Unix
         (let* ([is-windows (string-contains? (current-directory) "c:")]
                [cmd (if is-windows
                         (string-append "cmd.exe /c dir /b " dir)
                         (string-append "ls -1 " dir))])
               (let-values ([(to-stdin from-stdout from-stderr pid)
                             (open-process-ports cmd (buffer-mode line) (native-transcoder))])
                           (close-port to-stdin)
                           (close-port from-stderr)
                           (let loop ([result '()])
                                (let ([line (get-line from-stdout)])
                                     (if (eof-object? line)
                                         (begin
                                          (close-port from-stdout)
                                          (reverse result))
                                         (loop (cons line result)))))))))

;;; is-test-file? : String → Boolean
;;; Check if filename matches test file patterns
(define (is-test-file? filename)
  (and (string-ends-with? filename ".ss")
       (or (string-starts-with? filename "test-")
           (string-ends-with? filename "-test.ss"))))

;;; discover-test-files : String → (List String)
;;; Find all test files in a directory
(define (discover-test-files dir)
  (filter is-test-file? (list-directory dir)))

;;; ============================================================
;;; Test Results Registry
;;; ============================================================

;;; Test result: (file status duration-ms error-message)
;;; Status: 'passed | 'failed | 'skipped | 'error
(define *test-results* '())
(define *total-start-time* 0)

(define (record-result! file status duration-ms error-msg)
  (set! *test-results*
        (cons (list file status duration-ms error-msg)
              *test-results*)))

(define (reset-results!)
  (set! *test-results* '())
  (set! *total-start-time* (current-time-ms)))

(define (count-status status)
  (length (filter (lambda (r) (eq? (cadr r) status)) *test-results*)))

(define (total-test-duration)
  (fold-left + 0 (map caddr *test-results*)))

;;; ============================================================
;;; Test Execution
;;; ============================================================

;;; run-test-file : String → Unit
;;; Execute a single test file with timing and error handling
(define (run-test-file path)
  (let ([start (current-time-ms)]
        [filename (if (string-contains? path "/")
                      (let loop ([i (- (string-length path) 1)])
                           (cond
                            [(< i 0) path]
                            [(or (char=? (string-ref path i) #\/)
                                 (char=? (string-ref path i) #\\))
                             (substring path (+ i 1) (string-length path))]
                            [else (loop (- i 1))]))
                      path)])
       
       (display (string-append "  " (cyan filename) " "))
       (flush-output-port)
       
       (guard (exn [else
                    (let ([duration (- (current-time-ms) start)]
                          [msg (if (condition? exn)
                                   (condition-message exn)
                                   (format "~a" exn))])
                         (record-result! filename 'failed duration msg)
                         (display (red "FAILED"))
                         (display (string-append " (" (gray (format-duration-ms duration)) ")"))
                         (newline)
                         (display (string-append "    " (red "Error: ") msg))
                         (newline))])
              
              ;; Execute the test file
              (load path)
              
              (let ([duration (- (current-time-ms) start)])
                   (record-result! filename 'passed duration "")
                   (display (green "PASSED"))
                   (display (string-append " (" (gray (format-duration-ms duration)) ")"))
                   (newline)))))

;;; ============================================================
;;; Test Categories and Discovery
;;; ============================================================

;;; Well-known test file lists (curated for stability)
(define *core-test-files*
  '("test-prelude.ss"
    "test-sha256.ss"
    "test-block.ss"
    "test-cas.ss"
    "test-cas-gc.ss"
    "test-normalize.ss"
    "test-prim.ss"
    "test-parse.ss"
    "test-fold-parse.ss"
    "test-types.ss"
    "test-kinds.ss"
    "test-infer.ss"
    "test-resolve.ss"
    "test-annotate.ss"
    "test-eval.ss"
    "test-typed-eval.ss"
    "test-debug.ss"
    "test-compile.ss"
    "test-error.ss"
    "test-framework.ss"
    "test-module.ss"
    "test-state.ss"))

(define *shell-test-files*
  '("test-validate.ss"
    "test-block-index.ss"
    "test-duckie-persist.ss"
    "test-toolkit.ss"))

(define *excluded-test-files*
  '("test-text.ss"
    "test-fs.ss"
    "test-color.ss"
    "test-layout.ss"
    "test-block-navigator.ss"  ; Memory-intensive
    "test-commands.ss"          ; Requires special setup
    "test-commands-demo.ss"
    "test-commands-advanced.ss"
    "test-repl-integration.ss"))

;;; filter-tests-by-pattern : String × (List String) → (List String)
;;; Filter test files by pattern match
(define (filter-tests-by-pattern pattern test-files)
  (filter (lambda (f) (string-contains? f pattern)) test-files))

;;; ============================================================
;;; Test Runners
;;; ============================================================

;;; run-test-category : String × String × (List String) → Unit
;;; Run a category of tests (core, shell, etc.)
(define (run-test-category category-name dir test-files)
  (display (bold "────────────────────────────────────────────────────────────────\n"))
  (display (bold (string-append "  " category-name " (" dir "/)\n")))
  (display (bold "────────────────────────────────────────────────────────────────\n"))
  
  (if (null? test-files)
      (begin
       (display (yellow "  No test files found.\n"))
       (newline))
      (for-each
       (lambda (test-file)
               (run-test-file (string-append dir "/" test-file)))
       test-files))
  
  (newline))

;;; run-tests : [Symbol | (List String)] → Unit
;;; Main test runner - can be called from REPL
(define run-tests
  (case-lambda
   [() (run-all-tests-impl)]
   [(category)
    (cond
     [(eq? category 'all) (run-all-tests-impl)]
     [(eq? category 'core) (run-core-tests-impl)]
     [(eq? category 'shell) (run-shell-tests-impl)]
     [(list? category) (run-custom-tests-impl category)]
     [else
      (display (red (string-append "Unknown category: " (symbol->string category) "\n")))
      (display "Valid categories: all, core, shell\n")])]))

;;; run-tests-matching : String → Unit
;;; Run tests matching a pattern
(define (run-tests-matching pattern)
  (reset-results!)
  (print-header (string-append "PATTERN: " pattern))
  
  (let ([core-matches (filter-tests-by-pattern pattern *core-test-files*)]
        [shell-matches (filter-tests-by-pattern pattern *shell-test-files*)])
       
       (unless (null? core-matches)
               (run-test-category "CORE TESTS" "core" core-matches))
       
       (unless (null? shell-matches)
               (run-test-category "SHELL TESTS" "shell" shell-matches))
       
       (when (and (null? core-matches) (null? shell-matches))
             (display (yellow (string-append "No tests matching pattern: " pattern "\n")))
             (newline)))
  
  (print-summary))

;;; run-all-tests-impl : Unit → Unit
(define (run-all-tests-impl)
  (reset-results!)
  (print-header "ALL TESTS")
  
  (run-test-category "CORE TESTS" "core" *core-test-files*)
  (run-test-category "SHELL TESTS" "shell/tests" *shell-test-files*)
  
  (print-summary))

;;; run-core-tests-impl : Unit → Unit
(define (run-core-tests-impl)
  (reset-results!)
  (print-header "CORE TESTS ONLY")
  
  (run-test-category "CORE TESTS" "core" *core-test-files*)
  
  (print-summary))

;;; run-shell-tests-impl : Unit → Unit
(define (run-shell-tests-impl)
  (reset-results!)
  (print-header "SHELL TESTS ONLY")
  
  (run-test-category "SHELL TESTS" "shell/tests" *shell-test-files*)
  
  (print-summary))

;;; run-custom-tests-impl : (List String) → Unit
(define (run-custom-tests-impl test-files)
  (reset-results!)
  (print-header "CUSTOM TEST SET")
  
  (for-each run-test-file test-files)
  
  (print-summary))

;;; ============================================================
;;; Output Formatting
;;; ============================================================

;;; print-header : String → Unit
(define (print-header title)
  (display "\n")
  (display (bold "╔══════════════════════════════════════════════════════════════╗\n"))
  (display (bold (string-append "║  THE FOLD — TEST RUNNER: " title)))
  (let ([padding (- 62 (+ 27 (string-length title)))])
       (display (make-string padding #\space)))
  (display (bold "║\n"))
  (display (bold "╚══════════════════════════════════════════════════════════════╝\n"))
  (display (string-append "\nWorking directory: " (gray (current-directory)) "\n"))
  (display (string-append "Time: " (gray (date-time-string)) "\n\n")))

;;; date-time-string : Unit → String
(define (date-time-string)
  (let ([t (current-time)])
       (format "~a" (time-second t))))  ; Simple timestamp for now

;;; print-summary : Unit → Unit
;;; Print final test summary with statistics
(define (print-summary)
  (let* ([passed (count-status 'passed)]
         [failed (count-status 'failed)]
         [total (length *test-results*)]
         [duration (- (current-time-ms) *total-start-time*)])
        
        (display (bold "\n╔══════════════════════════════════════════════════════════════╗\n"))
        (display (bold "║                      SUMMARY                                 ║\n"))
        (display (bold "╚══════════════════════════════════════════════════════════════╝\n\n"))
        
        (display (string-append "  Test files:  " (bold (number->string total)) "\n"))
        (display (string-append "  " (green (string-append "Passed:      " (number->string passed))) "\n"))
        (display (string-append "  " (if (> failed 0)
                                         (red (string-append "Failed:      " (number->string failed)))
                                         (string-append "Failed:      " (number->string failed))) "\n"))
        (display (string-append "  Total time:  " (bold (format-duration-ms duration)) "\n\n"))
        
        ;; Show slowest tests (top 5)
        (unless (null? *test-results*)
                (let ([sorted (sort (lambda (a b) (> (caddr a) (caddr b)))
                                    *test-results*)])
                     (display (bold "  Slowest tests:\n"))
                     (for-each
                      (lambda (r)
                              (display (string-append "    "
                                                      (magenta (format-duration-ms (caddr r)))
                                                      "  "
                                                      (car r)
                                                      "\n")))
                      (take 5 sorted))))
        
        (newline)
        
        ;; Final verdict
        (if (= failed 0)
            (begin
             (display (bold "╔══════════════════════════════════════════════════════════════╗\n"))
             (display (bold "║  "))
             (display (green "✓ ALL TESTS PASSED"))
             (display (bold "                                          ║\n"))
             (display (bold "╚══════════════════════════════════════════════════════════════╝\n")))
            (begin
             (display (bold "╔══════════════════════════════════════════════════════════════╗\n"))
             (display (bold "║  "))
             (display (red "✗ SOME TESTS FAILED"))
             (display (bold "                                        ║\n"))
             (display (bold "╚══════════════════════════════════════════════════════════════╝\n\n"))
             
             ;; List failed tests
             (display (red (bold "  Failed tests:\n")))
             (for-each
              (lambda (r)
                      (when (eq? (cadr r) 'failed)
                            (display (string-append "    " (red "✗ ") (car r) "\n"))
                            (unless (string=? (cadddr r) "")
                                    (display (string-append "      " (gray (cadddr r)) "\n")))))
              (reverse *test-results*))))))

;;; take : Int × (List a) → (List a)
;;; Take first n elements from list
(define (take n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;;; filter : (a → Boolean) × (List a) → (List a)
;;; Filter list by predicate
(define (filter pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
   [else (filter pred (cdr lst))]))

;;; sort : (a × a → Boolean) × (List a) → (List a)
;;; Sort list using comparison function (simple insertion sort)
(define (sort less? lst)
  (if (null? lst)
      '()
      (insert (car lst) (sort less? (cdr lst)) less?)))

(define (insert x sorted less?)
  (cond
   [(null? sorted) (list x)]
   [(less? x (car sorted)) (cons x sorted)]
   [else (cons (car sorted) (insert x (cdr sorted) less?))]))

;;; ============================================================
;;; Test Listing
;;; ============================================================

;;; list-available-tests : Unit → Unit
;;; Display all available test files organized by category
(define (list-available-tests)
  (display "\n")
  (display (bold "╔══════════════════════════════════════════════════════════════╗\n"))
  (display (bold "║              AVAILABLE TESTS                                 ║\n"))
  (display (bold "╚══════════════════════════════════════════════════════════════╝\n\n"))
  
  (display (bold (string-append "CORE TESTS (" (number->string (length *core-test-files*)) " files):\n")))
  (for-each
   (lambda (f) (display (string-append "  " (cyan f) "\n")))
   *core-test-files*)
  
  (display "\n")
  (display (bold (string-append "SHELL TESTS (" (number->string (length *shell-test-files*)) " files):\n")))
  (for-each
   (lambda (f) (display (string-append "  " (cyan f) "\n")))
   *shell-test-files*)
  
  (display "\n")
  (display (bold (string-append "EXCLUDED TESTS (" (number->string (length *excluded-test-files*)) " files):\n")))
  (display (gray "  (These require special setup and are not run automatically)\n"))
  (for-each
   (lambda (f) (display (string-append "  " (gray f) "\n")))
   *excluded-test-files*)
  
  (display "\n")
  (display (string-append "Total available: "
                          (bold (number->string (+ (length *core-test-files*)
                                                   (length *shell-test-files*))))
                          " tests\n\n"))
  
  (display "Usage:\n")
  (display "  Run all:       scheme --script shell/test-runner.ss\n")
  (display "  Run core:      scheme --script shell/test-runner.ss core\n")
  (display "  Run shell:     scheme --script shell/test-runner.ss shell\n")
  (display "  Filter:        scheme --script shell/test-runner.ss --pattern <text>\n")
  (display "\n"))

;;; ============================================================
;;; Watch Mode (Stub for Future Implementation)
;;; ============================================================

;;; watch-tests : Unit → Unit
;;; Watch for file changes and re-run tests (NYI)
(define (watch-tests)
  (display (yellow "Watch mode not yet implemented.\n"))
  (display "This feature will:\n")
  (display "  - Monitor core/ and shell/ for .ss file changes\n")
  (display "  - Re-run affected tests automatically\n")
  (display "  - Provide fast feedback during development\n"))

;;; ============================================================
;;; Command-Line Interface
;;; ============================================================

;;; parse-args : (List String) → Unit
;;; Parse command-line arguments and execute appropriate action
(define (parse-args args)
  (cond
   [(null? args)
    (run-tests 'all)]
   
   [(string=? (car args) "--help")
    (print-help)]
   
   [(string=? (car args) "--list")
    (list-available-tests)]
   
   [(string=? (car args) "--watch")
    (watch-tests)]
   
   [(string=? (car args) "--pattern")
    (if (null? (cdr args))
        (begin
         (display (red "Error: --pattern requires an argument\n"))
         (exit 1))
        (run-tests-matching (cadr args)))]
   
   [(string=? (car args) "core")
    (run-tests 'core)]
   
   [(string=? (car args) "shell")
    (run-tests 'shell)]
   
   [(string=? (car args) "all")
    (run-tests 'all)]
   
   [else
    (display (red (string-append "Unknown argument: " (car args) "\n")))
    (print-help)
    (exit 1)]))

;;; print-help : Unit → Unit
(define (print-help)
  (display "\n")
  (display (bold "THE FOLD — TEST RUNNER\n"))
  (display "\n")
  (display "Usage:\n")
  (display "  scheme --script thimble/tests/test-runner.ss [OPTIONS]\n")
  (display "\n")
  (display "Options:\n")
  (display "  (no args)           Run all tests\n")
  (display "  all                 Run all tests\n")
  (display "  core                Run core/ tests only\n")
  (display "  shell               Run shell/ tests only\n")
  (display "  --pattern <text>    Run tests matching pattern\n")
  (display "  --list              List all available tests\n")
  (display "  --watch             Watch mode (NYI)\n")
  (display "  --help              Show this help\n")
  (display "\n")
  (display "From REPL:\n")
  (display "  (load \"shell/tests/test-runner.ss\")\n")
  (display "  (run-tests)                    ; all tests\n")
  (display "  (run-tests 'core)              ; core tests\n")
  (display "  (run-tests 'shell)             ; shell tests\n")
  (display "  (run-tests-matching \"block\")   ; pattern match\n")
  (display "  (run-test-file \"core/test-block.ss\")  ; single file\n")
  (display "  (list-available-tests)         ; show available tests\n")
  (display "\n"))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

;;; Global flag to track if loaded
(define *test-runner-loaded* #f)

;;; Add core to source-directories if not present
(unless (member "core" (source-directories))
        (source-directories (cons "core" (source-directories))))

;;; When run as a script, parse command-line args
(let ([args (command-line)])
     (when (and (pair? args)
                (pair? (cdr args))
                (string-contains? (car args) "test-runner.ss"))
           ;; Running as script
           (set! *test-runner-loaded* #t)
           (parse-args (cdr args))
           
           ;; Exit with appropriate code
           (when (> (count-status 'failed) 0)
                 (exit 1))))

;;; If loaded interactively, print a helpful message
(unless *test-runner-loaded*
        (set! *test-runner-loaded* #t)
        (display (green "\nTest runner loaded.\n"))
        (display "Try: ")
        (display (cyan "(run-tests)"))
        (display " or ")
        (display (cyan "(run-tests 'core)"))
        (display " or ")
        (display (cyan "(run-tests-matching \"block\")"))
        (display "\n\n"))
