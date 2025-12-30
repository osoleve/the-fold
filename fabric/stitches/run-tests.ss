;;; fabric/stitches/run-tests.ss — Test Runner for Core Modules
;;;
;;; Runs all fabric/stitches/ tests in dependency order.
;;; Can be invoked from ANY directory.
;;;
;;; Usage:
;;;   scheme --script fabric/stitches/run-tests.ss        (from ccverse root)
;;;   scheme --script run-tests.ss                        (from fabric/stitches/)
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
                                   [(or (char=? (string-ref script-path i) #\/)
                                        (char=? (string-ref script-path i) #\\)) i]
                                   [else (loop (- i 1))]))])
                 (if slash-pos
                     (substring script-path 0 (+ slash-pos 1))
                     "./"))
           "./")))

;;; Change to fabric/stitches directory if we're not already there
(when (or (string=? script-dir "fabric/stitches/")
          (string=? script-dir "fabric/stitches\\")
          (string=? script-dir "fabric\\stitches\\"))
      (current-directory (path-parent (path-parent (current-directory)))))

;;; Ensure fabric/stitches/ is in source-directories
(let ([core-path (string-append (current-directory) "/fabric/stitches")])
     (unless (member core-path (source-directories))
             (source-directories (cons core-path (source-directories)))))

;;; ============================================================
;;; Load Test Framework
;;; ============================================================

;;; Load the unified test framework first so all test files can use it
(load "fabric/stitches/test-framework.ss")

;;; ============================================================
;;; Test Runner
;;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

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

(define (run-test-file filename)
  (display (string-append "
=== Running " filename " ===
"))
  (set! test-count (+ test-count 1))
  (guard (exn [else
               (set! fail-count (+ fail-count 1))
               (display "FAILED: ")
               (display (format-condition exn))
               (newline)])
         (load (string-append "fabric/stitches/" filename))
         (set! pass-count (+ pass-count 1))))

;;; ============================================================
;;; Run Tests in Dependency Order
;;; ============================================================

(display "╔══════════════════════════════════════════════════════════╗
")
(display "║           The Fold — Core Test Suite                     ║
")
(display "╚══════════════════════════════════════════════════════════╝
")
(display (string-append "Working directory: " (current-directory) "
"))

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
(run-test-file "test-infer-constraints.ss")
(run-test-file "test-annotate.ss")

;;; Layer 4.5: Automatic Differentiation Type Class
(run-test-file "test-differentiable.ss")

;;; Layer 5: Evaluation
(run-test-file "test-eval.ss")
(run-test-file "test-typed-eval.ss")

;;; Layer 6: Compilation Pipeline
(run-test-file "test-compile.ss")

;;; Layer 7: Error System
(run-test-file "test-error.ss")

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "╔══════════════════════════════════════════════════════════╗
")
(display "║                    TEST SUMMARY                          ║
")
(display "╚══════════════════════════════════════════════════════════╝
")
(display (string-append "  Total:  " (number->string test-count) " test files
"))
(display (string-append "  Passed: " (number->string pass-count) "
"))
(display (string-append "  Failed: " (number->string fail-count) "
"))

(if (= fail-count 0)
    (display "
✓ All core tests passed!
")
    (begin
     (display "
✗ Some tests failed!
")
     (exit 1)))
