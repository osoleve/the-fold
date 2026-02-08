;;; test-all.ss — Unified Test Suite Runner for The Fold
;;;
;;; Runs both core/ and boundary/ test suites with timing and metrics.
;;; Invoke from ccverse root directory.
;;;
;;; Usage:
;;;   scheme --script test-all.ss           ; run all tests
;;;   scheme --script test-all.ss quick     ; skip slow tests
;;;   scheme --script test-all.ss core      ; core tests only
;;;   scheme --script test-all.ss boundary  ; boundary tests only

;;; ====
;;; Setup
;;; ====

(source-directories (cons "core" (cons "lattice" (source-directories))))
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

;;; ====
;;; Timing Utilities
;;; ====

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

;;; ====
;;; Test Registry
;;; ====

(define *test-results* '())  ; ((file status duration-ms message) ...)
(define total-start-time 0)

(define (record-result! file status duration-ms . message)
  (set! *test-results*
        (cons (list file status duration-ms (if (null? message) "" (car message)))
              *test-results*)))

;;; ====
;;; Test Runner
;;; ====

;;; run-test-file : String x String -> Void
;;; Run a test file as a subprocess (isolates exit calls).
;;; Uses scheme --script which properly handles test file exits.
(define (run-test-file base-dir filename)
  (let* ([start (current-time-ms)]
         [path (string-append base-dir "/" filename)]
         [cmd (string-append "scheme --script " path " 2>&1")])
    (display (string-append "  " filename " "))
    (flush-output-port)
    (let* ([exit-code (system cmd)]
           [duration (- (current-time-ms) start)])
      (if (= exit-code 0)
          (begin
            (record-result! filename 'passed duration)
            (display (string-append "(" (format-duration-ms duration) ") ok"))
            (newline))
          (begin
            (record-result! filename 'failed duration (string-append "exit code " (number->string exit-code)))
            (display (string-append "FAILED (" (format-duration-ms duration) ")"))
            (newline))))))

;;; ====
;;; Test Categories
;;; ====

;;; Core tests in dependency order (language kernel only)
(define core-tests
  '(;; Layer 0: Foundation
    "base/test-prelude.ss"
    "base/test-sha256.ss"
    ;; Layer 1: Block System
    "blocks/test-block.ss"
    "blocks/test-cas.ss"
    "blocks/test-cas-gc.ss"
    ;; Layer 2: Language Core
    "blocks/test-normalize.ss"
    "lang/test-prim.ss"
    "lang/test-parse.ss"
    "lang/test-fold-parse.ss"
    ;; Layer 3: Type System
    "types/test-types.ss"
    "types/test-kinds.ss"
    ;; Layer 4: Type Inference
    "types/test-infer.ss"
    "types/test-resolve.ss"
    "types/test-annotate.ss"
    ;; Layer 5: Evaluation
    "lang/test-eval.ss"
    "lang/test-typed-eval.ss"
    "util/test-debug.ss"
    ;; Layer 6: Compilation Pipeline
    "lang/test-compile.ss"
    ;; Layer 7: Error System
    "base/test-error.ss"))

;;; Lattice tests (skill tree - organized by tier)
(define lattice-tests
  '(;; === Tier 0: Foundational ===
    ;; Linear Algebra
    "linalg/test-vec.ss"
    "linalg/test-matrix.ss"
    "linalg/test-matrix-decomp.ss"
    "linalg/test-matrix-solvers.ss"
    ;; Data Structures
    "data/test-data-structures.ss"
    "data/test-heap.ss"
    ;; Algebra
    "algebra/test-polynomial.ss"
    "algebra/test-ring-field.ss"
    "algebra/test-multivariate-groebner.ss"
    "algebra/test-galois.ss"
    "algebra/test-field-ext.ss"
    "algebra/test-module.ss"

    ;; === Tier 1: Intermediate ===
    ;; Numeric
    "numeric/test-interval.ss"
    "numeric/test-affine.ss"
    "numeric/test-complex.ss"
    "numeric/test-interpolate.ss"
    "numeric/test-complex-bridge.ss"
    ;; Info Theory
    "info/test-entropy.ss"
    ;; FP Core
    "fp/optics/test-optics.ss"
    "fp/category/test-comonad.ss"
    "fp/category/test-natural-transform.ss"
    "fp/category/test-common-monads.ss"
    "fp/clp/test-clp.ss"
    "fp/game/test-voting-games.ss"
    "fp/game/test-coop-games.ss"
    ;; Statistics
    "statistics/test-statistics.ss"
    ;; Optimization
    "optimization/test-optimize.ss"
    "optimization/test-interval-global.ss"
    ;; Query
    "query/test-optic-query.ss"
    "query/test-query-macro.ss"
    "query/sql/test-ast-zipper.ss"
    ;; Autodiff
    "autodiff/test-traced-optics.ss"

    ;; === Tier 2: Advanced ===
    ;; Physics (2D)
    "physics/diff/test-rollout.ss"
    ;; Topology
    "topology/test-homology.ss"
    ;; Tiles
    "tiles/test-tiles.ss"
    "tiles/test-triangle.ss"
    "tiles/test-topology-analysis.ss"
    ;; Simulation
    "sim/test-simulation-stream.ss"
    ;; Meta
    "meta/test-meta.ss"
    ;; Pipeline
    "pipeline/test-stage-context.ss"
    "pipeline/test-rlm2.ss"
    "pipeline/test-council-voting.ss"
    ;; Dataset
    "dataset/test-dataset.ss"
    ))

;;; Boundary tests (validated, stable)
(define boundary-tests
  '("test-validate.ss"
    "test-block-index.ss"
    "test-duckie-persist.ss"
    "test-string-utils.ss"))

;;; Boundary pipeline tests (RLM harness, no live infra required)
(define boundary-pipeline-tests
  '("test-rlm-client.ss"
    "test-rlm2-drive.ss"))

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
    "test-repl-integration.ss"
    "test-rlm2-integration.ss"))  ; requires live vLLM + daemon

;;; ====
;;; Main Test Runner
;;; ====

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

;;; ====
;;; Main Entry Point
;;; ====

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
              (run-test-category "LATTICE TESTS" "lattice" lattice-tests)
              (display "
")
              (run-test-category "BOUNDARY TESTS" "boundary/tests" boundary-tests)
              (display "
")
              (run-test-category "BOUNDARY PIPELINE TESTS" "boundary/pipeline" boundary-pipeline-tests)]
             
             [(core)
              (run-test-category "CORE TESTS" "core" core-tests)]
             
             [(lattice)
              (run-test-category "LATTICE TESTS" "lattice" lattice-tests)]
             
             [(boundary)
              (run-test-category "BOUNDARY TESTS" "boundary/tests" boundary-tests)
              (display "
")
              (run-test-category "BOUNDARY PIPELINE TESTS" "boundary/pipeline" boundary-pipeline-tests)]
             
             [(quick)
              (let ([quick-core (filter (lambda (t) (not (member t slow-tests))) core-tests)])
                   (run-test-category "CORE TESTS (quick)" "core" quick-core))
              (display "
")
              (run-test-category "LATTICE TESTS" "lattice" lattice-tests)
              (display "
")
              (run-test-category "BOUNDARY TESTS" "boundary/tests" boundary-tests)]
             
             [else
              (display (string-append "Unknown mode: " (symbol->string mode) "
"))
              (display "Valid modes: all, quick, core, lattice, boundary
")
              (exit 1)])
       
       (print-final-summary)))

;;; Run with command line args
(main (cdr (command-line)))
