;;; core/run-tests.ss — Test Runner for Core Modules
;;;
;;; Runs all core/ tests in dependency order.
;;; Can be invoked from ANY directory.
;;;
;;; Usage:
;;;   scheme --script core/run-tests.ss        (from ccverse root)
;;;   scheme --script run-tests.ss             (from core/)
;;;
;;; Dependencies: None (this is the test infrastructure)

;;; ============================================================
;;; Setup: Ensure we can find core/ modules
;;; ============================================================

;;; Get the directory containing this script
(define script-dir
  (let ([args (command-line-arguments)])
    (if (and (pair? args) (string? (car args)))
        (let* ([script-path (car args)]
               [slash-pos (let loop ([i (- (string-length script-path) 1)])
                            (cond
                              [(< i 0) #f]
                              [(or (char=? (string-ref script-path i) #\\)
                                   (char=? (string-ref script-path i) #\/)) i]
                              [else (loop (- i 1))]))])
          (if slash-pos
              (substring script-path 0 (+ slash-pos 1))
              "./"))
        "./")))

;;; Change to core directory if we're not already there
(when (or (string=? script-dir "core/")
          (string=? script-dir "core\\"))
  (current-directory (path-parent (current-directory))))

;;; Ensure core/ is in source-directories
(let ([core-path (string-append (current-directory) "/core")])
  (unless (member core-path (source-directories))
    (source-directories (cons core-path (source-directories)))))

;;; ============================================================
;;; Load Test Framework
;;; ============================================================

;;; Load the unified test framework first so all test files can use it
(load "core/test-framework.ss")

;;; ============================================================
;;; Test Runner
;;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (run-test-file filename)
  (display (string-append "\n=== Running " filename " ===\n"))
  (set! test-count (+ test-count 1))
  (guard (exn [else
               (set! fail-count (+ fail-count 1))
               (display "FAILED: ")
               (display (condition-message exn))
               (newline)])
    (load (string-append "core/" filename))
    (set! pass-count (+ pass-count 1))))

;;; ============================================================
;;; Run Tests in Dependency Order
;;; ============================================================

(display "╔══════════════════════════════════════════════════════════╗\n")
(display "║           The Fold — Core Test Suite                     ║\n")
(display "╚══════════════════════════════════════════════════════════╝\n")
(display (string-append "Working directory: " (current-directory) "\n"))

;;; Layer 0: Foundation
(run-test-file "test-prelude.ss")
(run-test-file "test-sha256.ss")

;;; Layer 1: Block System
(run-test-file "test-block.ss")
(run-test-file "test-cas.ss")

;;; Layer 2: Language Core
(run-test-file "test-normalize.ss")
(run-test-file "test-prim.ss")
(run-test-file "test-parse.ss")

;;; Layer 3: Type System
(run-test-file "test-types.ss")
(run-test-file "test-kinds.ss")

;;; Layer 4: Type Inference
(run-test-file "test-infer.ss")
(run-test-file "test-resolve.ss")
(run-test-file "test-annotate.ss")

;;; Layer 5: Evaluation
(run-test-file "test-eval.ss")
(run-test-file "test-typed-eval.ss")

;;; Layer 6: Compilation Pipeline
(run-test-file "test-compile.ss")

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "╔══════════════════════════════════════════════════════════╗\n")
(display "║                    TEST SUMMARY                          ║\n")
(display "╚══════════════════════════════════════════════════════════╝\n")
(display (string-append "  Total:  " (number->string test-count) " test files\n"))
(display (string-append "  Passed: " (number->string pass-count) "\n"))
(display (string-append "  Failed: " (number->string fail-count) "\n"))

(if (= fail-count 0)
    (display "\n✓ All core tests passed!\n")
    (begin
      (display "\n✗ Some tests failed!\n")
      (exit 1)))
