;;; test-all.ss — Unified Test Suite Runner for The Fold
;;;
;;; Runs both core/ and boundary/ test suites with timing and metrics.
;;; Invoke from ccverse root directory.
;;;
;;; Usage:
;;;   scheme --script test-all.ss               ; run all tests
;;;   scheme --script test-all.ss quick         ; skip slow tests
;;;   scheme --script test-all.ss core          ; core tests only
;;;   scheme --script test-all.ss boundary      ; boundary tests only
;;;   scheme --script test-all.ss incremental   ; skip tests unchanged since last pass
;;;   FOLD_FULL_TESTS=1 git commit              ; force full run from pre-commit

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
;;; Incremental Test Cache
;;; ====

(define *cache-version* 1)
(define *cache-dir* ".test-cache")
(define *cache-file* ".test-cache/manifest.sexp")
(define *incremental-mode* #f)
(define *cached-tests* '())    ; alist: ((full-path . (fingerprint deps ...)) ...)
(define *dep-memo* (make-hashtable string-hash string=?))   ; path → dep-list
(define *hash-memo* (make-hashtable string-hash string=?))  ; path → content-hash
(define *skipped-count* 0)

;;; extract-load-paths : String → (List String)
;;; Parse (load "...") from file contents. Handles guarded loads too.
(define (extract-load-paths file)
  (guard (e [else '()])
    (let ([text (call-with-input-file file
                  (lambda (p)
                    (let loop ([chars '()])
                      (let ([c (read-char p)])
                        (if (eof-object? c)
                            (list->string (reverse chars))
                            (loop (cons c chars)))))))])
      (let loop ([pos 0] [results '()])
        (let ([idx (string-search "(load \"" text pos)])
          (if (not idx)
              (reverse results)
              (let* ([start (+ idx 7)]
                     [end (string-search "\"" text start)])
                (if (not end)
                    (reverse results)
                    (loop (+ end 1)
                          (cons (substring text start end) results))))))))))

;;; string-search : String × String × Int → Int|#f
;;; Find needle in haystack starting at pos. Returns index or #f.
(define (string-search needle haystack pos)
  (let ([nlen (string-length needle)]
        [hlen (string-length haystack)])
    (let loop ([i pos])
      (cond
       [(> (+ i nlen) hlen) #f]
       [(string=? needle (substring haystack i (+ i nlen))) i]
       [else (loop (+ i 1))]))))

;;; resolve-load-path : String → String|#f
;;; Resolve a load path, trying as-is then with source-directories prefixes.
(define (resolve-load-path path)
  (cond
   [(file-exists? path) path]
   [(file-exists? (string-append "core/" path)) (string-append "core/" path)]
   [(file-exists? (string-append "lattice/" path)) (string-append "lattice/" path)]
   [else #f]))

;;; scan-deps : String → (List String)
;;; Recursive transitive closure of (load ...) dependencies. Memoized.
(define (scan-deps file)
  (cond
   [(hashtable-ref *dep-memo* file #f) => values]
   [else
    ;; Mark in-progress to avoid cycles
    (hashtable-set! *dep-memo* file '())
    (let* ([loads (extract-load-paths file)]
           [resolved (filter values (map resolve-load-path loads))]
           [transitive (apply append (map scan-deps resolved))]
           [all-deps (delete-duplicates (append resolved transitive))])
      (hashtable-set! *dep-memo* file all-deps)
      all-deps)]))

;;; delete-duplicates : (List String) → (List String)
(define (delete-duplicates lst)
  (let ([seen (make-hashtable string-hash string=?)])
    (filter (lambda (x)
              (if (hashtable-ref seen x #f)
                  #f
                  (begin (hashtable-set! seen x #t) #t)))
            lst)))

;;; file-content-hash : String → String
;;; Content hash via git hash-object. Memoized.
;;; Uses temp file for subprocess output capture (Chez has no popen).
(define (file-content-hash file)
  (cond
   [(hashtable-ref *hash-memo* file #f) => values]
   [else
    (let ([hash
           (if (file-exists? file)
               (let* ([tmp (string-append *cache-dir* "/.hash-tmp")]
                      [cmd (string-append "git hash-object "
                                          file " > " tmp " 2>/dev/null")]
                      [_ (system cmd)])
                 (guard (e [else "missing"])
                   (call-with-input-file tmp
                     (lambda (p)
                       (let ([line (get-line p)])
                         (if (eof-object? line) "missing" line))))))
               "missing")])
      (hashtable-set! *hash-memo* file hash)
      hash)]))

;;; compute-fingerprint : (List String) → String
;;; Sort dep files, concatenate their content hashes, produce compact key.
(define (compute-fingerprint files)
  (let* ([sorted (sort string<? files)]
         [hashes (map file-content-hash sorted)]
         [combined (apply string-append hashes)])
    (number->string (equal-hash combined))))

;;; current-branch : → String
(define (current-branch)
  (let* ([tmp (string-append *cache-dir* "/.branch-tmp")]
         [_ (system (string-append "git rev-parse --abbrev-ref HEAD > " tmp " 2>/dev/null"))]
         [branch (guard (e [else "unknown"])
                   (call-with-input-file tmp
                     (lambda (p)
                       (let ([line (get-line p)])
                         (if (eof-object? line) "unknown" line)))))])
    branch))

;;; load-test-cache : → Alist
;;; Returns cached test data or '() on any failure.
(define (load-test-cache)
  (guard (e [else '()])
    (if (file-exists? *cache-file*)
        (let ([data (call-with-input-file *cache-file* read)])
          (if (and (pair? data)
                   (assq 'version data)
                   (= (cdr (assq 'version data)) *cache-version*)
                   (assq 'branch data)
                   (string=? (cdr (assq 'branch data)) (current-branch))
                   (assq 'tests data))
              (cdr (assq 'tests data))
              '()))
        '())))

;;; save-test-cache! : Alist → Void
;;; Write cache manifest to disk.
(define (save-test-cache! tests)
  (unless (file-exists? *cache-dir*)
    (system (string-append "mkdir -p " *cache-dir*)))
  (call-with-output-file *cache-file*
    (lambda (p)
      (write `((version . ,*cache-version*)
               (branch . ,(current-branch))
               (tests . ,tests))
             p)
      (newline p))
    'replace))

;;; test-cached? : String → Boolean
;;; Check if test's fingerprint matches cached value.
(define (test-cached? full-path)
  (let ([entry (assoc full-path *cached-tests*)])
    (if entry
        (let* ([cached-fp (cdr (assq 'fingerprint (cdr entry)))]
               [cached-deps (cdr (assq 'deps (cdr entry)))]
               ;; Check all deps still exist
               [deps-exist (for-all file-exists? cached-deps)]
               [current-fp (if deps-exist
                               (compute-fingerprint (cons full-path cached-deps))
                               "invalid")])
          (string=? cached-fp current-fp))
        #f)))

;;; cache-test-result! : String × Symbol → Void
;;; On pass: scan deps, compute fingerprint, store.
;;; On fail: remove from cache.
(define (cache-test-result! full-path status)
  (cond
   [(eq? status 'passed)
    (let* ([deps (scan-deps full-path)]
           [fp (compute-fingerprint (cons full-path deps))])
      (set! *cached-tests*
            (cons (cons full-path
                        `((fingerprint . ,fp)
                          (deps . ,deps)))
                  (remove (lambda (e) (string=? (car e) full-path))
                          *cached-tests*))))]
   [else
    (set! *cached-tests*
          (remove (lambda (e) (string=? (car e) full-path))
                  *cached-tests*))]))

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
;;; In incremental mode, skips tests whose fingerprint matches cache.
(define (run-test-file base-dir filename)
  (let* ([path (string-append base-dir "/" filename)])
    (if (and *incremental-mode* (test-cached? path))
        (begin
          (set! *skipped-count* (+ *skipped-count* 1))
          (record-result! filename 'passed 0)
          (display (string-append "  " filename " (cached) ok"))
          (newline))
        (let* ([start (current-time-ms)]
               [cmd (string-append "scheme --script " path " 2>&1")])
          (display (string-append "  " filename " "))
          (flush-output-port)
          (let* ([exit-code (system cmd)]
                 [duration (- (current-time-ms) start)])
            (if (= exit-code 0)
                (begin
                  (record-result! filename 'passed duration)
                  (when *incremental-mode*
                    (cache-test-result! path 'passed))
                  (display (string-append "(" (format-duration-ms duration) ") ok"))
                  (newline))
                (begin
                  (record-result! filename 'failed duration
                                  (string-append "exit code " (number->string exit-code)))
                  (when *incremental-mode*
                    (cache-test-result! path 'failed))
                  (display (string-append "FAILED (" (format-duration-ms duration) ")"))
                  (newline))))))))

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
    "linalg/test-vec2.ss"
    "linalg/test-vec3.ss"
    "linalg/test-matrix.ss"
    "linalg/test-matrix-decomp.ss"
    "linalg/test-matrix-solvers.ss"
    "linalg/test-matrix-eigen.ss"
    "linalg/test-matrix-optics.ss"
    "linalg/test-sparse.ss"
    "linalg/test-svd.ss"
    "linalg/test-quaternion.ss"
    "linalg/test-iteration.ss"
    "linalg/test-iterative-solvers.ss"
    "linalg/test-integer-matrix.ss"
    "linalg/test-graph-laplacian.ss"
    "linalg/test-dep-linalg.ss"
    "linalg/test-numeric-instances.ss"
    ;; Data Structures
    "data/test-data-structures.ss"
    "data/test-heap.ss"
    "data/test-avl-tree.ss"
    "data/test-kdtree.ss"
    "data/test-quadtree.ss"
    "data/test-sort.ss"
    "data/test-chase-lev-deque.ss"
    "data/test-collection-protocol.ss"
    "data/test-collection-utils.ss"
    "data/graph/test-centrality.ss"
    "data/graph/test-pagerank.ss"
    "data/graph/test-graph-algorithms.ss"
    "data/graph/test-graph-community.ss"
    "data/graph/test-graph-homology.ss"
    "data/graph/test-graph-layout.ss"
    "data/graph/test-graph-matrix.ss"
    "data/test-community-homology.ss"
    ;; Algebra
    "algebra/test-polynomial.ss"
    "algebra/test-ring-field.ss"
    "algebra/test-multivariate-groebner.ss"
    "algebra/test-galois.ss"
    "algebra/test-field-ext.ss"
    "algebra/test-module.ss"
    "algebra/test-group.ss"
    "algebra/test-poly-bridge.ss"
    ;; Crypto
    "crypto/test-crypto.ss"
    ;; Number Theory
    "number-theory/test-primality.ss"
    "number-theory/test-modular.ss"
    "number-theory/test-fast-multiply.ss"
    ;; IPC
    "ipc/test-protocol.ss"

    ;; === Tier 1: Intermediate ===
    ;; Numeric
    "numeric/test-interval.ss"
    "numeric/test-affine.ss"
    "numeric/test-complex.ss"
    "numeric/test-interpolate.ss"
    "numeric/test-complex-bridge.ss"
    "numeric/test-spectral-analysis.ss"
    "numeric/test-dft.ss"
    "numeric/test-convolution.ss"
    "numeric/test-window-functions.ss"
    "numeric/test-digital-filters.ss"
    "numeric/test-polynomial.ss"
    "numeric/test-signal-poly.ss"
    "numeric/test-finite-diff.ss"
    "numeric/test-wavelet.ss"
    "numeric/test-fem.ss"
    "numeric/test-interval-integrate.ss"
    "numeric/test-pde-time.ss"
    "numeric/test-spectral-pde.ss"
    ;; Info Theory
    "info/test-entropy.ss"
    "info/test-coding.ss"
    "info/test-channel-capacity.ss"
    "info/test-epiplexity.ss"
    "info/test-statistical-measures.ss"
    "info/test-rate-distortion.ss"
    ;; Random
    "random/test-prng.ss"
    "random/test-random.ss"
    "random/test-distributions.ss"
    "random/test-probability.ss"
    "random/test-monte-carlo.ss"
    "random/test-bayesian.ss"
    "random/test-variational-inference.ss"
    ;; FP Core
    "fp/test-protocol.ss"
    "fp/test-protocol-bundle.ss"
    "fp/test-protocol-introspect.ss"
    "fp/test-templates.ss"
    "optics/test-optics.ss"
    "optics/test-bidirectional.ss"
    "optics/test-block-optics.ss"
    "optics/test-profunctor-optics.ss"
    "fp/category/test-comonad.ss"
    "fp/category/test-natural-transform.ss"
    "fp/category/test-common-monads.ss"
    "fp/category/test-adjunction.ss"
    "fp/category/test-kleisli.ss"
    "fp/category/test-limits.ss"
    "fp/category/test-free-algebra.ss"
    "fp/category/test-kan-extension.ss"
    "fp/category/test-effect-category.ss"
    "fp/category/test-abstract-interp.ss"
    "fp/category/test-monad-derivation.ss"
    "fp/category/test-state-store-adjunction.ss"
    "fp/category/test-logic-adjunction.ss"
    "fp/category/multi/test-multi-category.ss"
    "fp/clp/test-clp.ss"
    "fp/clp/test-domain.ss"
    "fp/clp/test-store.ss"
    "fp/parsing/test-parser.ss"
    "fp/parsing/test-regex.ss"
    "fp/parsing/test-regex-extensions.ss"
    "fp/parsing/test-fsm.ss"
    "fp/parsing/test-parser-examples.ss"
    "fp/parsing/test-parser-infinite-loop.ss"
    "fp/sat/test-sat.ss"
    "fp/sat/test-maxsat.ss"
    "fp/control/test-free.ss"
    "fp/control/test-state.ss"
    "fp/control/test-effects.ss"
    "fp/control/test-continuation.ss"
    "fp/control/test-random-effect.ss"
    "fp/data/test-zipper.ss"
    "fp/data/test-stream.ss"
    "fp/data/test-tree-zipper.ss"
    "fp/data/test-generic-zipper.ss"
    "fp/data/test-fused-ops.ss"
    "fp/data/test-zipper-lens.ss"
    "fp/symbolic/test-expr.ss"
    "fp/symbolic/test-diff.ss"
    "fp/symbolic/test-simplify.ss"
    "fp/symbolic/test-integrate.ss"
    "fp/symbolic/test-solve.ss"
    "fp/symbolic/test-poly-canonical.ss"
    "fp/rewrite/test-rewrite.ss"
    "fp/rewrite/test-sexp-zipper.ss"
    "fp/rewrite/test-proof-tactics.ss"
    "fp/rewrite/test-sketch.ss"
    "fp/measure/test-units.ss"
    "fp/measure/test-qvec.ss"
    "fp/meta/test-combinators.ss"
    "fp/meta/test-dsl.ss"
    "fp/meta/test-logic.ss"
    "fp/meta/test-result.ss"
    "fp/numeric/test-special-functions.ss"
    "fp/numeric/test-transcendental.ss"
    "fp/analysis/test-cost-analysis.ss"
    "fp/analysis/test-fusion-detect.ss"
    "fp/analysis/test-parallel-detect.ss"
    "control-systems/test-transfer-function.ss"
    "control-systems/test-state-space.ss"
    "control-systems/test-stability.ss"
    "control-systems/test-controller-design.ss"
    "control-systems/test-tf-convert.ss"
    "control-systems/test-z-transform.ss"
    "control-systems/test-discrete-control.ss"
    "control-systems/test-digital-pid.ss"
    "control-systems/test-poly-algebra.ss"
    "control-systems/test-kalman.ss"
    "control-systems/test-hinf-synthesis.ss"
    "game-theory/test-fair-division.ss"
    "game-theory/test-voting-games.ss"
    "game-theory/test-coop-games.ss"
    "game-theory/test-normal-form.ss"
    "game-theory/test-extensive-form.ss"
    "game-theory/test-matching.ss"
    "game-theory/test-mechanism.ss"
    "game-theory/test-evolutionary.ss"
    "game-theory/test-mcdm.ss"
    "game-theory/test-multi-winner.ss"
    "game-theory/test-strategic-voting.ss"
    "game-theory/test-voting.ss"
    "game-theory/test-physics-dsl.ss"
    ;; Statistics
    "statistics/test-statistics.ss"
    "statistics/core/test-model-protocol.ss"
    "statistics/regression/test-traced-regression.ss"
    "statistics/timeseries/test-ar-poly.ss"
    ;; Optimization
    "optimization/test-convergence.ss"
    "optimization/test-line-search.ss"
    "optimization/test-optimize.ss"
    "optimization/test-interval-global.ss"
    "optimization/test-lp.ss"
    "optimization/test-ilp.ss"
    "optimization/test-metaheuristic.ss"
    "optimization/test-poly-optimize.ss"
    "optimization/test-interval-contract.ss"
    "optimization/test-interval-newton.ss"
    ;; Query
    "query/test-optic-query.ss"
    "query/test-query-macro.ss"
    "query/test-query-dsl.ss"
    "query/test-aho-corasick.ss"
    "query/test-patterns-parse.ss"
    "query/test-query-patterns.ss"
    "query/sql/test-sql.ss"
    "query/sql/test-dialect.ss"
    "query/sql/test-ast-zipper.ss"
    ;; Autodiff
    "autodiff/test-traced-optics.ss"
    "autodiff/test-differentiable.ss"
    "autodiff/test-higher-order-diff.ss"
    "autodiff/test-symbolic-diff.ss"
    "autodiff/test-interval-autodiff.ss"
    "autodiff/test-sparse-autodiff.ss"
    "autodiff/test-typed-gradients.ss"
    "autodiff/test-eval-traced.ss"
    "autodiff/test-eval-and-grad-simple.ss"
    "autodiff/test-differentiable-signal.ss"
    "autodiff/test-profiling.ss"
    ;; E-Graphs
    "egraph/test-union-find.ss"
    "egraph/test-eclass.ss"
    "egraph/test-egraph.ss"
    "egraph/test-match.ss"
    "egraph/test-extract.ss"
    "egraph/test-cost.ss"
    "egraph/test-saturation.ss"
    "egraph/test-scheduler.ss"
    ;; Differential Geometry
    "diffgeo/test-charts.ss"
    "diffgeo/test-tangent.ss"
    "diffgeo/test-forms.ss"
    "diffgeo/test-lie-groups.ss"
    "diffgeo/test-curvature.ss"
    "diffgeo/test-geodesics.ss"
    "diffgeo/test-symbolic-metrics.ss"

    ;; === Tier 2: Advanced ===
    ;; Geometry
    "geometry/test-geometry.ss"
    "geometry/test-convex-hull.ss"
    "geometry/test-bvh.ss"
    "geometry/test-bvh-accel.ss"
    "geometry/test-mesh-topology.ss"
    "geometry/test-voronoi.ss"
    "geometry/test-raymarch.ss"
    "geometry/test-shape-protocol.ss"
    "geometry/test-mesh-gen.ss"
    "geometry/test-mesh-sdf.ss"
    "geometry/test-geometry-optics.ss"
    "geometry/test-ascii-render.ss"
    "geometry/test-obj-loader.ss"
    "geometry/test-octree.ss"
    "geometry/test-marching-cubes.ss"
    ;; DSL
    "dsl/test-tagless.ss"
    "dsl/test-staging.ss"
    "dsl/test-match.ss"
    "dsl/test-quasi.ss"
    "dsl/test-reader.ss"
    "dsl/test-compose.ss"
    "dsl/test-partial-eval.ss"
    "dsl/template/test-template.ss"
    "dsl/template/test-template-zipper.ss"
    "dsl/chronicle/test-chronicle.ss"
    "dsl/chronicle/test-chronicle-parser.ss"
    "dsl/chronicle/test-tagless-chronicle.ss"
    "dsl/markdown/test-markdown.ss"
    ;; Physics (2D diff)
    "physics/diff/test-rollout.ss"
    "physics/diff/test-diff-physics.ss"
    "physics/diff/test-traced-body.ss"
    "physics/diff/test-traced-vec2.ss"
    "physics/diff/test-diff-collision.ss"
    "physics/diff/test-smooth-collision.ss"
    "physics/diff/test-diff-constraints.ss"
    "physics/diff/test-optimize.ss"
    "physics/diff/test-optic-optimize.ss"
    ;; Physics (3D diff)
    "physics/diff3d/test-traced-vec3.ss"
    "physics/diff3d/test-traced-quaternion.ss"
    "physics/diff3d/test-traced-body3d.ss"
    "physics/diff3d/test-traced-integrators3d.ss"
    "physics/diff3d/test-diff-collision3d.ss"
    "physics/diff3d/test-smooth-collision3d.ss"
    "physics/diff3d/test-diff-constraints3d.ss"
    "physics/diff3d/test-rollout3d.ss"
    ;; Physics (classical 2D)
    "physics/classical/test-physics.ss"
    "physics/classical/test-particles.ss"
    "physics/classical/test-constraints.ss"
    "physics/classical/test-collision-detection.ss"
    "physics/classical/test-collision-response.ss"
    "physics/classical/test-collision-protocol.ss"
    "physics/classical/test-constraint-graph.ss"
    "physics/classical/test-integrators.ss"
    "physics/classical/test-raycasting.ss"
    "physics/classical/test-world.ss"
    "physics/classical/test-ascii-renderer.ss"
    "physics/classical/test-ode-integrators.ss"
    ;; Physics (classical 3D)
    "physics/classical3d/test-shapes3d.ss"
    "physics/classical3d/test-rigid-body3d.ss"
    "physics/classical3d/test-collision-detection3d.ss"
    "physics/classical3d/test-constraints3d.ss"
    "physics/classical3d/test-world3d.ss"
    ;; Physics (lenses + problems)
    "physics/lenses/test-lenses.ss"
    "physics/lenses/test-lenses3d.ss"
    "physics/lenses/test-optics-integration.ss"
    "physics/problems/test-physics-problems.ss"
    ;; Topology
    "topology/test-homology.ss"
    "topology/test-simplicial-complex.ss"
    "topology/test-persistent.ss"
    ;; Automata
    "automata/test-statechart.ss"
    "automata/test-statechart-zipper.ss"
    ;; Tiles
    "tiles/test-tiles.ss"
    "tiles/test-triangle.ss"
    "tiles/test-pathfinding.ss"
    "tiles/test-visibility.ss"
    "tiles/test-units.ss"
    "tiles/test-turns.ss"
    "tiles/test-topology-analysis.ss"
    "tiles/test-board-size.ss"
    ;; Simulation
    "sim/test-simulation-stream.ss"
    "sim/dynamics/test-ode-system.ss"
    "sim/dynamics/test-chaos.ss"
    "sim/dynamics/test-bifurcation.ss"
    "sim/dynamics/test-stability.ss"
    "sim/dynamics/test-discrete.ss"
    ;; Meta
    "meta/test-meta.ss"
    "meta/test-docs.ss"
    "meta/test-source-loc.ss"
    "meta/test-manifest-optics.ss"
    "meta/test-cache-migration.ss"
    ;; Pipeline
    "pipeline/test-stage-context.ss"
    "pipeline/test-rlm2.ss"
    "pipeline/test-council-voting.ss"
    "pipeline/tests/test-stage.ss"
    "pipeline/tests/test-effects.ss"
    "pipeline/tests/test-context.ss"
    "pipeline/tests/test-council.ss"
    "pipeline/tests/test-dsl.ss"
    ;; Dataset
    "dataset/test-dataset.ss"
    ))

;;; Boundary tests (validated, stable)
(define boundary-tests
  '("test-validate.ss"
    "test-block-index.ss"
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
        (when (> *skipped-count* 0)
          (display (string-append "  Cached:      " (number->string *skipped-count*) "
")))
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

(define (run-all-categories)
  (run-test-category "CORE TESTS" "core" core-tests)
  (display "
")
  (run-test-category "LATTICE TESTS" "lattice" lattice-tests)
  (display "
")
  (run-test-category "BOUNDARY TESTS" "boundary/tests" boundary-tests)
  (display "
")
  (run-test-category "BOUNDARY PIPELINE TESTS" "boundary/pipeline" boundary-pipeline-tests))

(define (main args)
  ;; Parse args: support "incremental", "incremental quick", etc.
  (let* ([has-incremental (and (member "incremental" args) #t)]
         [other-args (filter (lambda (a) (not (string=? a "incremental"))) args)]
         [mode (if (null? other-args) 'all (string->symbol (car other-args)))])

       ;; Enable incremental mode
       (when has-incremental
         (set! *incremental-mode* #t)
         (unless (file-exists? *cache-dir*)
           (system (string-append "mkdir -p " *cache-dir*)))
         (set! *cached-tests* (load-test-cache)))

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
       (display (string-append "Mode: " (symbol->string mode)
                               (if *incremental-mode* " (incremental)" "") "

"))

       (set! total-start-time (current-time-ms))

       (case mode
             [(all)
              (run-all-categories)]

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
              (display "Valid modes: all, quick, core, lattice, boundary, incremental
")
              (exit 1)])

       ;; Save cache after all tests complete
       (when *incremental-mode*
         (save-test-cache! *cached-tests*))

       (print-final-summary)))

;;; Run with command line args
(main (cdr (command-line)))
