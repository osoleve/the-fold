;;; @module module-paths
;;; @description Module path registration table. Maps module names to file paths.
;;; Loaded by module.ss — requires register-module-path! to be defined first.
;;;
;;; Structure:
;;;   1. Auto-discovery: scan core/, lattice/, boundary/ for ;;; @module annotations
;;;   2. Manual overrides: modules without annotations, or needing different names
;;;   3. Namespaced aliases: collision-avoidance names (e.g., 'tiles/core for core.ss)

;;; ====
;;; Phase 1: Auto-discovery from @module annotations
;;; ====

;;; discover-module-paths! : -> Integer
;;; Scan source directories for files with ;;; @module annotations.
;;; Registers each as name -> path. Returns count of discovered modules.
;;; Only registers if the name isn't already registered (manual overrides win).
(define (discover-module-paths!)
  (let ([count 0])
    (define (scan-dir dir)
      (when (file-directory? dir)
        (for-each
         (lambda (entry)
           (let ([path (string-append dir "/" entry)])
             (cond
               [(and (not (char=? (string-ref entry 0) #\.))
                     (file-directory? path))
                (scan-dir path)]
               [(and (> (string-length entry) 3)
                     (string=? (substring entry (- (string-length entry) 3)
                                          (string-length entry)) ".ss")
                     ;; Skip test files — they're not modules
                     (not (and (> (string-length entry) 5)
                               (string=? (substring entry 0 5) "test-"))))
                (guard (e [else (void)])
                  (let ([p (open-input-file path)])
                    (let loop ([i 0])
                      (when (< i 8)
                        (let ([line (get-line p)])
                          (when (string? line)
                            (let* ([trimmed (string-trim line)]
                                   [mod-name
                                    (and (> (string-length trimmed) 3)
                                         (string-starts-with? trimmed ";;;")
                                         (let ([after (string-trim (substring trimmed 3 (string-length trimmed)))])
                                           (and (string-starts-with? after "@module ")
                                                (let ([val (string-trim (substring after 8 (string-length after)))])
                                                  (and (> (string-length val) 0) val)))))])
                              (if mod-name
                                  ;; Found annotation — extract name and register
                                  (let ([name (string->symbol mod-name)])
                                    (unless (hashtable-contains? *module-paths* name)
                                      (hashtable-set! *module-paths* name path)
                                      (set! count (+ count 1))))
                                  ;; Not this line — keep scanning
                                  (loop (+ i 1))))))))
                    (close-input-port p)))])))
         (directory-list dir))))
    (scan-dir "core")
    (scan-dir "lattice")
    (scan-dir "boundary")
    count))

;;; ====
;;; Phase 2: Manual overrides (modules without @module annotations)
;;; ====
;;; These modules predate the annotation convention or have names that
;;; differ from their @module annotation. Listed explicitly so they
;;; appear in (modules) and resolve correctly via require.

(begin
 ;; Core modules without @module annotations
 (register-module-path! 'prelude "core/base/prelude.ss")
 (register-module-path! 'sha256 "core/base/sha256.ss")
 (register-module-path! 'error "core/base/error.ss")
 (register-module-path! 'block "core/blocks/block.ss")
 (register-module-path! 'cas "core/blocks/cas.ss")
 (register-module-path! 'normalize "core/blocks/normalize.ss")
 (register-module-path! 'expand "core/blocks/expand.ss")
 (register-module-path! 'parse "core/lang/parse.ss")
 (register-module-path! 'span "core/lang/span.ss")
 (register-module-path! 'fold-parse "core/lang/fold-parse.ss")
 (register-module-path! 'prim "core/lang/prim.ss")
 (register-module-path! 'eval "core/lang/eval.ss")
 (register-module-path! 'compile "core/lang/compile.ss")
 (register-module-path! 'module "core/lang/module.ss")
 (register-module-path! 'nbe "core/lang/nbe.ss")
 (register-module-path! 'types "core/types/types.ss")
 (register-module-path! 'kinds "core/types/kinds.ss")
 (register-module-path! 'infer "core/types/infer.ss")
 (register-module-path! 'resolve "core/types/resolve.ss")
 (register-module-path! 'annotate "core/types/annotate.ss")
 (register-module-path! 'dep-types "core/types/dep-types.ss")
 (register-module-path! 'pretty-class "core/util/pretty-class.ss")
 (register-module-path! 'comp-graph "core/autodiff/comp-graph.ss")
 (register-module-path! 'reverse-diff "core/autodiff/reverse-diff.ss")

 ;; Lattice modules without @module annotations
 (register-module-path! 'tangent "lattice/diffgeo/tangent.ss")
 (register-module-path! 'geodesics "lattice/diffgeo/geodesics.ss")
 (register-module-path! 'charts "lattice/diffgeo/charts.ss")
 (register-module-path! 'curvature "lattice/diffgeo/curvature.ss")
 (register-module-path! 'forms "lattice/diffgeo/forms.ss")
 (register-module-path! 'symbolic-metrics "lattice/diffgeo/symbolic-metrics.ss")
 (register-module-path! 'lie-groups "lattice/diffgeo/lie-groups.ss")
 (register-module-path! 'optic-query "lattice/query/optic-query.ss")
 (register-module-path! 'query-macro "lattice/query/query-macro.ss")
 (register-module-path! 'ascii-render "lattice/geometry/ascii-render.ss")
 (register-module-path! 'raymarch "lattice/geometry/raymarch.ss")
 (register-module-path! 'mesh-sdf "lattice/geometry/mesh-sdf.ss")
 (register-module-path! 'mesh-topology "lattice/geometry/mesh-topology.ss")
 (register-module-path! 'marching-cubes "lattice/geometry/marching-cubes.ss")
 (register-module-path! 'octree "lattice/geometry/octree.ss")
 (register-module-path! 'obj-loader "lattice/geometry/obj-loader.ss")
 (register-module-path! 'fair-division "lattice/game-theory/fair-division.ss")
 (register-module-path! 'coop-games "lattice/game-theory/coop-games.ss")
 (register-module-path! 'matching "lattice/game-theory/matching.ss")
 (register-module-path! 'strategic-voting "lattice/game-theory/strategic-voting.ss")
 (register-module-path! 'voting-games "lattice/game-theory/voting-games.ss")
 (register-module-path! 'mcdm "lattice/game-theory/mcdm.ss")
 (register-module-path! 'multi-winner "lattice/game-theory/multi-winner.ss")
 (register-module-path! 'evolutionary "lattice/game-theory/evolutionary.ss")
 (register-module-path! 'physics-dsl "lattice/game-theory/physics-dsl.ss")
 (register-module-path! 'council "lattice/pipeline/council.ss")
 (register-module-path! 'council-voting "lattice/pipeline/council-voting.ss")
 (register-module-path! 'curriculum "lattice/pipeline/curriculum.ss")
 (register-module-path! 'bifurcation "lattice/sim/dynamics/bifurcation.ss")
 (register-module-path! 'attractor-render "lattice/sim/dynamics/attractor-render.ss")
 (register-module-path! 'simulation-stream "lattice/sim/simulation-stream.ss")
 (register-module-path! 'difficulty "lattice/dataset/difficulty.ss")
 (register-module-path! 'parameter "lattice/dataset/parameter.ss")
 (register-module-path! 'distractor "lattice/dataset/distractor.ss")
 (register-module-path! 'sample "lattice/dataset/sample.ss")
 (register-module-path! 'presentation "lattice/dataset/presentation.ss")
 (register-module-path! 'collision-protocol "lattice/physics/classical/collision-protocol.ss")
 (register-module-path! 'collision-detection "lattice/physics/classical/collision-detection.ss")
 (register-module-path! 'collision-response "lattice/physics/classical/collision-response.ss")
 (register-module-path! 'collision-impl "lattice/physics/classical/collision-impl.ss")
 (register-module-path! 'ode-integrators "lattice/physics/classical/ode-integrators.ss")
 (register-module-path! 'integrators "lattice/physics/classical/integrators.ss")
 (register-module-path! 'particles "lattice/physics/classical/particles.ss")
 (register-module-path! 'constraints "lattice/physics/classical/constraints.ss")
 (register-module-path! 'constraint-solver "lattice/physics/classical/constraint-solver.ss")
 (register-module-path! 'constraint-graph "lattice/physics/classical/constraint-graph.ss")
 (register-module-path! 'raycasting "lattice/physics/classical/raycasting.ss")
 (register-module-path! 'world "lattice/physics/classical/world.ss")
 (register-module-path! 'ascii-renderer "lattice/physics/classical/ascii-renderer.ss")
 (register-module-path! 'rigid-body3d "lattice/physics/classical3d/rigid-body3d.ss")
 (register-module-path! 'shapes3d "lattice/physics/classical3d/shapes3d.ss")
 (register-module-path! 'collision-detection3d "lattice/physics/classical3d/collision-detection3d.ss")
 (register-module-path! 'constraints3d "lattice/physics/classical3d/constraints3d.ss")
 (register-module-path! 'collision-protocol3d "lattice/physics/classical3d/collision-protocol3d.ss")
 (register-module-path! 'collision-impl3d "lattice/physics/classical3d/collision-impl3d.ss")
 (register-module-path! 'constraint-solver3d "lattice/physics/classical3d/constraint-solver3d.ss")
 (register-module-path! 'world3d "lattice/physics/classical3d/world3d.ss")
 (register-module-path! 'dynamics-ops "lattice/physics/diff/dynamics-ops.ss")
 (register-module-path! 'traced-vec2 "lattice/physics/diff/traced-vec2.ss")
 (register-module-path! 'traced-body "lattice/physics/diff/traced-body.ss")
 (register-module-path! 'traced-body-protocols "lattice/physics/diff/traced-body-protocols.ss")
 (register-module-path! 'traced-integrators "lattice/physics/diff/traced-integrators.ss")
 (register-module-path! 'smooth-collision "lattice/physics/diff/smooth-collision.ss")
 (register-module-path! 'diff-collision "lattice/physics/diff/diff-collision.ss")
 (register-module-path! 'diff-constraints "lattice/physics/diff/diff-constraints.ss")
 (register-module-path! 'rollout "lattice/physics/diff/rollout.ss")
 (register-module-path! 'optic-optimize "lattice/physics/diff/optic-optimize.ss")
 (register-module-path! 'traced-vec3 "lattice/physics/diff3d/traced-vec3.ss")
 (register-module-path! 'traced-quaternion "lattice/physics/diff3d/traced-quaternion.ss")
 (register-module-path! 'traced-body3d "lattice/physics/diff3d/traced-body3d.ss")
 (register-module-path! 'traced-integrators3d "lattice/physics/diff3d/traced-integrators3d.ss")
 (register-module-path! 'smooth-collision3d "lattice/physics/diff3d/smooth-collision3d.ss")
 (register-module-path! 'diff-collision3d "lattice/physics/diff3d/diff-collision3d.ss")
 (register-module-path! 'diff-constraints3d "lattice/physics/diff3d/diff-constraints3d.ss")
 (register-module-path! 'rollout3d "lattice/physics/diff3d/rollout3d.ss")
 (register-module-path! 'optimize3d "lattice/physics/diff3d/optimize3d.ss")
 (register-module-path! 'lenses "lattice/physics/lenses/lenses.ss")
 (register-module-path! 'lenses3d "lattice/physics/lenses/lenses3d.ss")
 (register-module-path! 'optics-integration "lattice/physics/lenses/optics-integration.ss")
 (register-module-path! 'simulation "lattice/physics/problems/simulation.ss")
 (register-module-path! 'physics-problem "lattice/physics/problems/physics-problem.ss")
 (register-module-path! 'spatial "lattice/physics/problems/templates/spatial.ss")

 ;; Tiles (no @module annotations)
 (register-module-path! 'pathfinding "lattice/tiles/pathfinding.ss")
 (register-module-path! 'visibility "lattice/tiles/visibility.ss")
 (register-module-path! 'turns "lattice/tiles/turns.ss")
 (register-module-path! 'topology-analysis "lattice/tiles/topology-analysis.ss")
 (register-module-path! 'boardcraft "lattice/tiles/boardcraft.ss")

 ;; Sim dynamics (no @module annotations)
 (register-module-path! 'ode-system "lattice/sim/dynamics/ode-system.ss")
 (register-module-path! 'chaos "lattice/sim/dynamics/chaos.ss")

 ;; Game theory (no @module annotations)
 (register-module-path! 'normal-form "lattice/game-theory/normal-form.ss")
 (register-module-path! 'extensive-form "lattice/game-theory/extensive-form.ss")
 (register-module-path! 'voting "lattice/game-theory/voting.ss")
 (register-module-path! 'mechanism "lattice/game-theory/mechanism.ss")

 ;; Query (no @module annotations)
 (register-module-path! 'query-dsl "lattice/query/query-dsl.ss")
 (register-module-path! 'aho-corasick "lattice/query/aho-corasick.ss")
 (register-module-path! 'ast-zipper "lattice/query/sql/ast-zipper.ss")

 ;; Optimization (no @module annotations)
 (register-module-path! 'convergence "lattice/optimization/convergence.ss")
 (register-module-path! 'line-search "lattice/optimization/line-search.ss")
 (register-module-path! 'first-order "lattice/optimization/first-order.ss")
 (register-module-path! 'newton "lattice/optimization/newton.ss")
 (register-module-path! 'lbfgs "lattice/optimization/lbfgs.ss")
 (register-module-path! 'optimize "lattice/optimization/optimize.ss")
 (register-module-path! 'optic-first-order "lattice/optimization/optic-first-order.ss")
 (register-module-path! 'poly-optimize "lattice/optimization/poly-optimize.ss")
 (register-module-path! 'metaheuristic "lattice/optimization/metaheuristic.ss")
 (register-module-path! 'interval-global "lattice/optimization/interval-global.ss")
 (register-module-path! 'interval-newton "lattice/optimization/interval-newton.ss")
 (register-module-path! 'interval-contract "lattice/optimization/interval-contract.ss")
 (register-module-path! 'maxsat-bridge "lattice/optimization/maxsat-bridge.ss")

 ;; Geometry (no @module annotations)
 (register-module-path! 'geometry "lattice/geometry/geometry.ss")
 (register-module-path! 'bvh "lattice/geometry/bvh.ss")
 (register-module-path! 'shape-protocol "lattice/geometry/shape-protocol.ss")

 ;; FP (no @module annotations)
 (register-module-path! 'regex "lattice/fp/parsing/regex.ss")
 (register-module-path! 'engine "lattice/rewrite/engine.ss")

 ;; Interval (no @module annotation)
 (register-module-path! 'interval-integrate "lattice/interval/interval-integrate.ss")

 ;; Statistics (no @module annotations)
 (register-module-path! 'model-protocol "lattice/statistics/core/model-protocol.ss")
 (register-module-path! 'traced-regression "lattice/statistics/regression/traced-regression.ss")

 ;; Pipeline (no @module annotations)
 (register-module-path! 'rlm2 "lattice/pipeline/rlm2.ss")
 (register-module-path! 'rlm2-hud "lattice/pipeline/rlm2-hud.ss")
 (register-module-path! 'rlm2-parse "lattice/pipeline/rlm2-parse.ss")

 ;; Name mismatches: registered name differs from @module annotation
 (register-module-path! 'parser-combinators "lattice/fp/parsing/parser.ss")  ; @module parser
 (register-module-path! 'module-theory "lattice/algebra/module.ss")          ; @module module (name clash)
)

;;; ====
;;; Phase 3: Auto-discover from source annotations
;;; ====
;;; Scans core/, lattice/, boundary/ for ;;; @module headers.
;;; Only registers modules not already in the table (Phase 2 overrides win).

(discover-module-paths!)

;;; ====
;;; Phase 4: Namespaced aliases (collision avoidance)
;;; ====
;;; These register a namespaced name alongside (or replacing) the bare name.
;;; Use (require 'dir/module) when multiple modules share the same bare name.

(begin
 ;; Numeric — collision with algebra/polynomial
 (register-module-path! 'numeric/polynomial "lattice/numeric/polynomial.ss")

 ;; Algebra — collision with numeric/polynomial
 (register-module-path! 'algebra/polynomial "lattice/algebra/polynomial.ss")

 ;; Data
 (register-module-path! 'data/alist "lattice/data/alist.ss")

 ;; Statistics — collision with random/distributions
 (register-module-path! 'stat/distributions "lattice/statistics/hypothesis/distributions.ss")

 ;; Info-stat bridge
 (register-module-path! 'info-stat/mi-bridge "lattice/statistics/mi-bridge.ss")

 ;; FP protocol (alias)
 (register-module-path! 'fp/protocol "lattice/fp/protocol.ss")

 ;; Tiles — collision with other core/square/hex/etc names
 (register-module-path! 'tiles/core "lattice/tiles/core.ss")
 (register-module-path! 'tiles/square "lattice/tiles/square.ss")
 (register-module-path! 'tiles/hex "lattice/tiles/hex.ss")
 (register-module-path! 'tiles/triangle "lattice/tiles/triangle.ss")
 (register-module-path! 'tiles/render "lattice/tiles/render.ss")
 (register-module-path! 'tiles/units "lattice/tiles/units.ss")

 ;; Pipeline — collision with fp/pipeline concepts
 (register-module-path! 'pipeline/context "lattice/pipeline/context.ss")
 (register-module-path! 'pipeline/stage "lattice/pipeline/stage.ss")
 (register-module-path! 'pipeline/effects "lattice/pipeline/effects.ss")
 (register-module-path! 'pipeline/dsl "lattice/pipeline/dsl.ss")
 (register-module-path! 'pipeline/rlm2-metrics "lattice/pipeline/rlm2-metrics.ss")

 ;; DSL — collision with fp/dsl
 (register-module-path! 'dsl/reader "lattice/dsl/reader.ss")
 (register-module-path! 'dsl/match "lattice/dsl/match.ss")
 (register-module-path! 'dsl/compose "lattice/dsl/compose.ss")
 (register-module-path! 'dsl/template "lattice/dsl/template/template.ss")

 ;; E-graph (namespaced for clarity)
 (register-module-path! 'egraph/union-find "lattice/egraph/union-find.ss")
 (register-module-path! 'egraph/eclass "lattice/egraph/eclass.ss")
 (register-module-path! 'egraph/egraph "lattice/egraph/egraph.ss")
 (register-module-path! 'egraph/match "lattice/egraph/match.ss")
 (register-module-path! 'egraph/saturation "lattice/egraph/saturation.ss")
 (register-module-path! 'egraph/cost "lattice/egraph/cost.ss")
 (register-module-path! 'egraph/extract "lattice/egraph/extract.ss")
 (register-module-path! 'egraph/scheduler "lattice/egraph/scheduler.ss")

 ;; Control systems
 (register-module-path! 'control/state-space "lattice/control-systems/state-space.ss")
 (register-module-path! 'control/transfer-function "lattice/control-systems/transfer-function.ss")
 (register-module-path! 'control/stability "lattice/control-systems/stability.ss")
 (register-module-path! 'control/controller-design "lattice/control-systems/controller-design.ss")
 (register-module-path! 'control/discrete-control "lattice/control-systems/discrete-control.ss")
 (register-module-path! 'control/hinf-synthesis "lattice/control-systems/hinf-synthesis.ss")
 (register-module-path! 'control/kalman "lattice/control-systems/kalman.ss")
 (register-module-path! 'control/digital-pid "lattice/control-systems/digital-pid.ss")
 (register-module-path! 'control/z-transform "lattice/control-systems/z-transform.ss")
 (register-module-path! 'control/tf-convert "lattice/control-systems/tf-convert.ss")
 (register-module-path! 'control/poly-algebra "lattice/control-systems/poly-algebra.ss")

 ;; SAT (under fp/)
 (register-module-path! 'sat/literal "lattice/fp/sat/literal.ss")
 (register-module-path! 'sat/clause "lattice/fp/sat/clause.ss")
 (register-module-path! 'sat/assignment "lattice/fp/sat/assignment.ss")
 (register-module-path! 'sat/watches "lattice/fp/sat/watches.ss")
 (register-module-path! 'sat/cnf "lattice/fp/sat/cnf.ss")
 (register-module-path! 'sat/solver "lattice/fp/sat/solver.ss")

 ;; DES (under sim/)
 (register-module-path! 'des/event "lattice/sim/des/event.ss")
 (register-module-path! 'des/world "lattice/sim/des/world.ss")
 (register-module-path! 'des/engine "lattice/sim/des/engine.ss")
 (register-module-path! 'des/schedule "lattice/sim/des/schedule.ss")
 (register-module-path! 'des/replicate "lattice/sim/des/replicate.ss")

 ;; Sim namespaced
 (register-module-path! 'sim/stability "lattice/sim/dynamics/stability.ss")
 (register-module-path! 'sim/discrete "lattice/sim/dynamics/discrete.ss")

 ;; Rewrite namespaced
 (register-module-path! 'rewrite/sexp-zipper "lattice/rewrite/sexp-zipper.ss")
 (register-module-path! 'rewrite/laws "lattice/rewrite/laws.ss")
 (register-module-path! 'rewrite/fusion-rules "lattice/rewrite/fusion-rules.ss")
 (register-module-path! 'rewrite/proof-tactics "lattice/rewrite/proof-tactics.ss")
 (register-module-path! 'rewrite/sketch "lattice/rewrite/sketch.ss")

 ;; Physics namespaced
 (register-module-path! 'physics/optimize "lattice/physics/diff/optimize.ss")

 ;; Autodiff namespaced
 (register-module-path! 'autodiff/ode-jacobian "lattice/autodiff/ode-jacobian.ss")

 ;; Game theory namespaced
 (register-module-path! 'game-theory/clp-equilibrium "lattice/game-theory/clp-equilibrium.ss")

 ;; FP control (namespaced to avoid collision with bare 'state)
 (register-module-path! 'fp/control/state "lattice/fp/control/state.ss")

 ;; Pipeline (namespaced rlm2 for require 'pipeline/rlm2)
 (register-module-path! 'pipeline/rlm2 "lattice/pipeline/rlm2.ss")

 ;; Meta modules (namespaced to avoid collisions with lattice concepts)
 (register-module-path! 'meta/fuel-vocab "lattice/meta/fuel-vocab.ss")
 (register-module-path! 'meta/purity-audit "lattice/meta/purity-audit.ss")
 (register-module-path! 'meta/bridge-discovery "lattice/meta/bridge-discovery.ss")
 (register-module-path! 'meta/readme-audit "lattice/meta/readme-audit.ss")
 (register-module-path! 'meta/fuel-report "lattice/meta/fuel-report.ss")
 (register-module-path! 'meta/module-manifest "lattice/meta/module-manifest.ss")
 (register-module-path! 'meta/serendipity "lattice/meta/serendipity.ss")
 (register-module-path! 'meta/promotion "lattice/meta/promotion.ss")
 (register-module-path! 'meta/bbs-xref "lattice/meta/bbs-xref.ss")
 (register-module-path! 'meta/concept-normalize "lattice/meta/concept-normalize.ss")
 (register-module-path! 'meta/unused-requires "lattice/meta/unused-requires.ss")
 (register-module-path! 'meta/skill-map "lattice/meta/skill-map.ss")
 (register-module-path! 'meta/derive-deps "lattice/meta/derive-deps.ss")
 (register-module-path! 'meta/fuel-normalize "lattice/meta/fuel-normalize.ss")
 (register-module-path! 'meta/bm25 "lattice/meta/bm25.ss")
 (register-module-path! 'meta/cache-migration "lattice/meta/cache-migration.ss")
 (register-module-path! 'meta/dag "lattice/meta/dag.ss")
 (register-module-path! 'meta/docs "lattice/meta/docs.ss")
 (register-module-path! 'meta/docstrings "lattice/meta/docstrings.ss")
 (register-module-path! 'meta/kg "lattice/meta/kg.ss")
 (register-module-path! 'meta/persist "lattice/meta/persist.ss")
 (register-module-path! 'meta/search "lattice/meta/search.ss")
 (register-module-path! 'meta/source-loc "lattice/meta/source-loc.ss")
 (register-module-path! 'meta/xref "lattice/meta/xref.ss")
 (register-module-path! 'meta/lattice-walk "lattice/meta/lattice-walk.ss")
 (register-module-path! 'meta/analyzer-exports "lattice/meta/analyzer-exports.ss")
 (register-module-path! 'meta/analyzer-effects "lattice/meta/analyzer-effects.ss")
 (register-module-path! 'meta/analyzer-unused "lattice/meta/analyzer-unused.ss")
 (register-module-path! 'meta/analyzer-patterns "lattice/meta/analyzer-patterns.ss")
 (register-module-path! 'meta/analyzer-coverage "lattice/meta/analyzer-coverage.ss")
 (register-module-path! 'meta/analyzer-fuel "lattice/meta/analyzer-fuel.ss")

 ;; Boundary
 (register-module-path! 'boundary/bbs "boundary/bbs/bbs.ss")
)
