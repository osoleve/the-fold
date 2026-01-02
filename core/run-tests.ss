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
(when (or (string=? script-dir "core/")
          (string=? script-dir "core\\")
          (string=? script-dir "core\\"))
      (current-directory (path-parent (path-parent (current-directory)))))

;;; Ensure fabric/stitches/ is in source-directories
(let ([core-path (string-append (current-directory) "/fabric/stitches")])
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

(define (run-test-file subdir filename)
  (let ([path (string-append "core/" subdir "/" filename)])
       (display (string-append "
=== Running " path " ===
"))
       (set! test-count (+ test-count 1))
       (guard (exn [else
                    (set! fail-count (+ fail-count 1))
                    (display "FAILED: ")
                    (display (format-condition exn))
                    (newline)])
              (load path)
              (set! pass-count (+ pass-count 1)))))

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

;;; Layer 0: Foundation (base/)
(run-test-file "base" "test-prelude.ss")
(run-test-file "base" "test-sha256.ss")
(run-test-file "base" "test-error.ss")

;;; Layer 1: Block System (blocks/)
(run-test-file "blocks" "test-block.ss")
(run-test-file "blocks" "test-cas.ss")
(run-test-file "blocks" "test-normalize.ss")
(run-test-file "blocks" "test-expand.ss")

;;; Layer 2: Language Core (lang/)
(run-test-file "lang" "test-prim.ss")
(run-test-file "lang" "test-parse.ss")
(run-test-file "lang" "test-span.ss")
(run-test-file "lang" "test-fold-parse.ss")
(run-test-file "lang" "test-eval.ss")

;;; Layer 3: Type System (types/)
(run-test-file "types" "test-types.ss")
(run-test-file "types" "test-kinds.ss")
(run-test-file "types" "test-infer.ss")
(run-test-file "types" "test-resolve.ss")
(run-test-file "types" "test-infer-constraints.ss")
(run-test-file "types" "test-annotate.ss")

;;; Layer 4: Linear Algebra (linalg/)
(run-test-file "linalg" "test-vec.ss")
(run-test-file "linalg" "test-vec2.ss")
(run-test-file "linalg" "test-matrix.ss")
(run-test-file "linalg" "test-matrix-decomp.ss")
(run-test-file "linalg" "test-matrix-solvers.ss")
(run-test-file "linalg" "test-dep-linalg.ss")

;;; Layer 5: Numeric (numeric/)
(run-test-file "numeric" "test-complex.ss")
(run-test-file "numeric" "test-dft.ss")
(run-test-file "numeric" "test-convolution.ss")

;;; Layer 6: Automatic Differentiation (autodiff/)
(run-test-file "autodiff" "test-comp-graph.ss")
(run-test-file "autodiff" "test-reverse-diff.ss")
(run-test-file "autodiff" "test-differentiable.ss")
(run-test-file "autodiff" "test-higher-order-diff.ss")

;;; Layer 7: Data Structures (data/)
(run-test-file "data" "test-data-structures.ss")
(run-test-file "data" "test-collection-utils.ss")
(run-test-file "data" "test-graph-algorithms.ss")

;;; Layer 8: Query System (query/)
(run-test-file "query" "test-aho-corasick.ss")
(run-test-file "query" "test-patterns-parse.ss")
(run-test-file "query" "test-query-dsl.ss")
(run-test-file "query" "test-query-patterns.ss")

;;; Layer 9: Utilities (util/)
(run-test-file "util" "test-debug.ss")
(run-test-file "util" "test-pretty.ss")
(run-test-file "util" "test-help.ss")
(run-test-file "util" "test-state.ss")

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
