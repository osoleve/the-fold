;;; test-all.ss — Unified Test Suite Runner for The Fold
;;;
;;; Runs both core/ and shell/ test suites with timing and metrics.
;;; Invoke from ccverse root directory.
;;;
;;; Usage:
;;;   scheme --script test-all.ss           ; run all tests
;;;   scheme --script test-all.ss quick     ; skip slow tests
;;;   scheme --script test-all.ss core      ; core tests only
;;;   scheme --script test-all.ss shell     ; shell tests only

;;; ============================================================
;;; Setup
;;; ============================================================

(source-directories (cons "core" (source-directories)))
(load "core/test-framework.ss")

;;; format-condition : Condition → String
;;; Format a Chez Scheme condition with its irritants properly filled in.
;;; Fixes the ~s placeholder bug in error messages.
(define (format-condition e)
  (if (condition? e)
      (guard (e2 [else (condition-message e)])
             (let ([template (condition-message e)]
                   [irritants (if (irritants-condition? e)
                                  (condition-irritants e)
                                  '())])
                  (if (null? irritants)
                      template
                      (apply format template irritants))))
      (format "~a" e)))

;;; ============================================================
;;; Timing Utilities
;;; ============================================================

(define (current-time-ms)
  (let ([t (current-time)])
       (+ (* (time-second t) 1000)
          (quotient (time-nanosecond t) 1000000))))

(define (format-duration-ms ms)
  (cond
   [(< ms 1000) (string-append (number->string ms) "ms")]
   [(< ms 60000)
    (string-append (number->string (quotient ms 1000)) "."
                   (number->string (quotient (remainder ms 1000) 100)) "s")]
   [else
    (string-append (number->string (quotient ms 60000)) "m "
                   (number->string (quotient (remainder ms 60000) 1000)) "s")]))

;;; ============================================================
;;; Test Registry
;;; ============================================================

(define *test-results* '())  ; ((file status duration-ms message) ...)
(define total-start-time 0)

(define (record-result! file status duration-ms . message)
  (set! *test-results*
        (cons (list file status duration-ms (if (null? message) "" (car message)))
              *test-results*)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(define (run-test-file base-dir filename)
  (let ([start (current-time-ms)]
        [path (string-append base-dir "/" filename)])
       (display (string-append "  " filename " "))
       (flush-output-port)
       (guard (exn [else
                    (let ([duration (- (current-time-ms) start)]
                          [msg (if (condition? exn) (format-condition exn) "Unknown error")])
                         (record-result! filename 'failed duration msg)
                         (display "FAILED")
                         (display (string-append " (" (format-duration-ms duration) ")"))
                         (newline)
                         (display (string-append "    Error: " msg))
                         (newline))])
              (load path)
              (let ([duration (- (current-time-ms) start)])
                   (record-result! filename 'passed duration)
                   (display (string-append " (" (format-duration-ms duration) ") ok"))
                   (newline)))))

;;; ============================================================
;;; Test Categories
;;; ============================================================

;;; Core tests in dependency order
(define core-tests
  '(;; Layer 0: Foundation
    "test-prelude.ss"
    "test-sha256.ss"
    ;; Layer 1: Block System
    "test-block.ss"
    "test-cas.ss"
    "test-cas-gc.ss"
    ;; Layer 2: Language Core
    "test-normalize.ss"
    "test-prim.ss"
    "test-parse.ss"
    "test-fold-parse.ss"
    ;; Layer 3: Type System
    "test-types.ss"
    "test-kinds.ss"
    ;; Layer 4: Type Inference
    "test-infer.ss"
    "test-resolve.ss"
    "test-annotate.ss"
    ;; Layer 5: Evaluation
    "test-eval.ss"
    "test-typed-eval.ss"
    "test-debug.ss"
    ;; Layer 6: Compilation Pipeline
    "test-compile.ss"
    ;; Layer 7: Error System
    "test-error.ss"))

;;; Shell tests (validated, stable)
(define shell-tests
  '("test-validate.ss"
    "test-block-index.ss"
    "test-duckie-persist.ss"))

;;; Slow tests (excluded from 'quick' mode)
(define slow-tests
  '("test-block-navigator.ss"))

;;; Excluded tests (require special setup)
(define excluded-tests
  '("test-text.ss"
    "test-fs.ss"
    "test-color.ss"
    "test-layout.ss"
    "test-block-navigator.ss"
    "test-commands.ss"
    "test-commands-demo.ss"
    "test-commands-advanced.ss"
    "test-repl-integration.ss"))

;;; ============================================================
;;; Main Test Runner
;;; ============================================================

(define (run-test-category category-name dir tests)
  (display "────────────────────────────────────────────────────────────────
")
  (display (string-append "  " category-name " (" dir "/)
"))
  (display "────────────────────────────────────────────────────────────────
")
  (for-each (lambda (test) (run-test-file dir test)) tests))

(define (count-status status)
  (length (filter (lambda (r) (eq? (cadr r) status)) *test-results*)))

(define (total-duration)
  (fold-left + 0 (map caddr *test-results*)))

(define (print-final-summary)
  (let* ([passed (count-status 'passed)]
         [failed (count-status 'failed)]
         [total (length *test-results*)]
         [duration (- (current-time-ms) total-start-time)])
        
        (display "
╔══════════════════════════════════════════════════════════════╗
")
        (display "║                      FINAL SUMMARY                           ║
")
        (display "╚══════════════════════════════════════════════════════════════╝

")
        
        (display (string-append "  Test files:  " (number->string total) "
"))
        (display (string-append "  Passed:      " (number->string passed) "
"))
        (display (string-append "  Failed:      " (number->string failed) "
"))
        (display (string-append "  Duration:    " (format-duration-ms duration) "

"))
        
        ;; Show slowest tests
        (let ([sorted (sort (lambda (a b) (> (caddr a) (caddr b))) *test-results*)])
             (display "  Slowest tests:
")
             (for-each
              (lambda (r)
                      (display (string-append "    " (format-duration-ms (caddr r)) "  " (car r) "
")))
              (take 5 sorted)))
        
        (newline)
        (if (= failed 0)
            (begin
             (display "╔══════════════════════════════════════════════════════════════╗
")
             (display "║              ✓ ALL TESTS PASSED                              ║
")
             (display "╚══════════════════════════════════════════════════════════════╝
"))
            (begin
             (display "╔══════════════════════════════════════════════════════════════╗
")
             (display "║              ✗ SOME TESTS FAILED                             ║
")
             (display "╚══════════════════════════════════════════════════════════════╝
")
             (newline)
             (display "  Failed tests:
")
             (for-each
              (lambda (r)
                      (when (eq? (cadr r) 'failed)
                            (display (string-append "    ✗ " (car r) ": " (cadddr r) "
"))))
              *test-results*)
             (exit 1)))))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

(define (main args)
  (let ([mode (if (null? args) 'all (string->symbol (car args)))])
       
       (display "
")
       (display "╔══════════════════════════════════════════════════════════════╗
")
       (display "║         THE FOLD — UNIFIED TEST SUITE                        ║
")
       (display "╚══════════════════════════════════════════════════════════════╝
")
       (display (string-append "
Working directory: " (current-directory) "
"))
       (display (string-append "Mode: " (symbol->string mode) "

"))
       
       (set! total-start-time (current-time-ms))
       
       (case mode
             [(all)
              (run-test-category "CORE TESTS" "core" core-tests)
              (display "
")
              (run-test-category "SHELL TESTS" "shell/tests" shell-tests)]
             
             [(core)
              (run-test-category "CORE TESTS" "core" core-tests)]
             
             [(shell)
              (run-test-category "SHELL TESTS" "shell/tests" shell-tests)]
             
             [(quick)
              (let ([quick-core (filter (lambda (t) (not (member t slow-tests))) core-tests)])
                   (run-test-category "CORE TESTS (quick)" "core" quick-core))
              (display "
")
              (run-test-category "SHELL TESTS" "shell/tests" shell-tests)]
             
             [else
              (display (string-append "Unknown mode: " (symbol->string mode) "
"))
              (display "Valid modes: all, quick, core, shell
")
              (exit 1)])
       
       (print-final-summary)))

;;; Run with command line args
(main (cdr (command-line)))
