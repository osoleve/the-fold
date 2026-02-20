(load "core/test-framework.ss")

(doc 'module 'run-tests)
(doc 'description "Test runner for core modules. Runs all core/ and lattice/ tests in dependency order. Can be invoked from any directory.")
(doc 'layer 'core)

(doc 'section 'load-test-framework)

(doc 'section 'test-runner)

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (format-condition e)
  (doc 'type (-> Condition String))
  (doc 'description "Format a Chez Scheme condition with its irritants properly filled in. Fixes the ~s placeholder bug in error messages.")
  (doc 'export #t)
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

(define (run-test-file base subdir filename)
  (doc 'type (-> String String String Unit))
  (doc 'description "Run a single test file, catching errors and updating global counters.")
  (doc 'export #t)
  (let ([path (string-append base "/" subdir "/" filename)])
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

(doc 'section 'run-tests-in-dependency-order)

(display "=============== The Fold -- Core Test Suite ================
")
(display (string-append "Working directory: " (current-directory) "
"))

(doc 'description "Layer 0: Foundation (core/base/)")
(run-test-file "core" "base" "test-prelude.ss")
(run-test-file "core" "base" "test-sha256.ss")
(run-test-file "core" "base" "test-error.ss")

(doc 'description "Layer 1: Block System (core/blocks/)")
(run-test-file "core" "blocks" "test-block.ss")
(run-test-file "core" "blocks" "test-cas.ss")
(run-test-file "core" "blocks" "test-normalize.ss")
(run-test-file "core" "blocks" "test-expand.ss")

(doc 'description "Layer 1.5: Autodiff Foundation (core/autodiff/)")
(run-test-file "core" "autodiff" "test-comp-graph.ss")
(run-test-file "core" "autodiff" "test-reverse-diff.ss")

(doc 'description "Layer 2: Language Core (core/lang/)")
(run-test-file "core" "lang" "test-prim.ss")
(run-test-file "core" "lang" "test-parse.ss")
(run-test-file "core" "lang" "test-span.ss")
(run-test-file "core" "lang" "test-fold-parse.ss")
(run-test-file "core" "lang" "test-eval.ss")

(doc 'description "Layer 3: Type System (core/types/)")
(run-test-file "core" "types" "test-types.ss")
(run-test-file "core" "types" "test-kinds.ss")
(run-test-file "core" "types" "test-infer.ss")
(run-test-file "core" "types" "test-resolve.ss")
(run-test-file "core" "types" "test-infer-constraints.ss")
(run-test-file "core" "types" "test-annotate.ss")

(doc 'description "Layer 4: Linear Algebra (lattice/linalg/)")
(run-test-file "lattice" "linalg" "test-vec.ss")
(run-test-file "lattice" "linalg" "test-vec2.ss")
(run-test-file "lattice" "linalg" "test-matrix.ss")
(run-test-file "lattice" "linalg" "test-matrix-decomp.ss")
(run-test-file "lattice" "linalg" "test-matrix-solvers.ss")
(run-test-file "lattice" "linalg" "test-dep-linalg.ss")

(doc 'description "Layer 5: Numeric (lattice/numeric/)")
(run-test-file "lattice" "numeric" "test-complex.ss")
(run-test-file "lattice" "numeric" "test-dft.ss")
(run-test-file "lattice" "numeric" "test-convolution.ss")

(doc 'description "Layer 6: Automatic Differentiation (lattice/autodiff/)")
(run-test-file "lattice" "autodiff" "test-differentiable.ss")
(run-test-file "lattice" "autodiff" "test-higher-order-diff.ss")

(doc 'description "Layer 7: Data Structures (lattice/data/)")
(run-test-file "lattice" "data" "test-data-structures.ss")
(run-test-file "lattice" "data" "test-collection-utils.ss")
(run-test-file "lattice" "data" "test-graph-primitives.ss")

(doc 'description "Layer 8: Query System (lattice/query/)")
(run-test-file "lattice" "query" "test-aho-corasick.ss")
(run-test-file "lattice" "query" "test-patterns-parse.ss")
(run-test-file "lattice" "query" "test-query-dsl.ss")
(run-test-file "lattice" "query" "test-query-patterns.ss")

(doc 'description "Layer 9: Utilities (core/util/)")
(run-test-file "core" "util" "test-debug.ss")
(run-test-file "core" "util" "test-pretty.ss")
(run-test-file "core" "util" "test-help.ss")
(run-test-file "core" "util" "test-state.ss")

(doc 'description "Layer 10: Simulation (lattice/sim/)")
(run-test-file "lattice" "sim" "test-simulation-stream.ss")

(doc 'section 'summary)

(newline)
(display "===================== TEST SUMMARY =========================
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
