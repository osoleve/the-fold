(load "core/base/prelude.ss")

(doc 'module 'module)
(doc 'description "Module System for The Fold. Provides a module loader that tracks loaded modules, automatically loads dependencies, parses @module/@requires annotations from file headers, records load times, and provides discovery functions.")
(doc 'layer 'core)
(doc 'requires '(prelude))

(doc 'note "Usage:
  (require 'compile)        ; Load module and its dependencies
  (require 'eval 'infer)    ; Load multiple modules
  (modules)                 ; List all registered modules
  (module-info 'eval)       ; Show module details (deps, path, status)
  (module-stats)            ; Show load times
  (module-deps 'eval)       ; Show dependencies of a module

Session Introspection:
  (loaded-modules)          ; List of loaded module symbols
  (session-info)            ; Alist with modules, counts, timing
  (session-summary)         ; Print brief session summary

Module Header Format (at top of .ss files):
  ;;; @module eval
  ;;; @requires prelude block prim

Dependencies:
  - prelude.ss (must be loaded first manually)")

(doc 'section 'registry)

(doc *module-registry* 'type (Hashtable Symbol (Pair Boolean Nat)))
(doc *module-registry* 'description "Maps module names to (loaded? . load-time-ms).")
(define *module-registry* (make-eq-hashtable))

(doc *module-deps* 'type (Hashtable Symbol (List Symbol)))
(doc *module-deps* 'description "Declared dependencies for each module.")
(define *module-deps* (make-eq-hashtable))

(doc *load-order* 'type (List Symbol))
(doc *load-order* 'description "Order in which modules were loaded (for diagnostics).")
(define *load-order* '(prelude))

(doc *loading-stack* 'type (List Symbol))
(doc *loading-stack* 'description "Stack of modules currently being loaded (for circular dependency detection).")
(define *loading-stack* '())

(doc 'note "Register prelude as already loaded (we loaded it above)")
(hashtable-set! *module-registry* 'prelude (cons #t 0))

(doc 'section 'paths)

(doc *module-paths* 'type (Hashtable Symbol String))
(doc *module-paths* 'description "Maps module names to file paths (includes both pre-registered and discovered).")
(define *module-paths* (make-eq-hashtable))

(doc *header-cache* 'type (Hashtable String (Maybe (Pair Symbol (List Symbol)))))
(doc *header-cache* 'description "Cache for parsed headers (keyed by file path).")
(define *header-cache* (make-hashtable string-hash string=?))

(doc 'section 'manifest)

(doc *manifest-module-index* 'type (Maybe (Hashtable Symbol String)))
(doc *manifest-module-index* 'description "Simple module names → file paths (populated by boundary at startup).")
(define *manifest-module-index* #f)

(doc *manifest-namespaced-index* 'type (Maybe (Hashtable Symbol String)))
(doc *manifest-namespaced-index* 'description "Namespaced module names (skill/module) → file paths (always unambiguous).")
(define *manifest-namespaced-index* #f)

(define (register-manifest-index! simple-index namespaced-index)
  (doc 'type (-> (Hashtable Symbol String) (Hashtable Symbol String) Void))
  (doc 'description "Boundary calls this at startup to inject the manifest-derived index.")
  (doc 'export #t)
  (set! *manifest-module-index* simple-index)
  (set! *manifest-namespaced-index* namespaced-index))

;;; ====
;;; Legacy Search Directories (Deprecated)
;;; ====

;;; *module-search-dirs* : (List String)
;;; DEPRECATED: Only used as fallback when manifest index is not initialized.
;;; The manifest index (populated at startup) is the primary lookup mechanism.
(define *module-search-dirs*
  '(;; Core directories (language kernel)
    "core/base" "core/blocks" "core/lang" "core/types" "core/util"
    ;; Lattice directories (skill tree)
    "lattice/linalg" "lattice/data" "lattice/data/graph" "lattice/algebra" "lattice/random"
    "lattice/numeric" "lattice/geometry" "lattice/autodiff" "lattice/diffgeo"
    "lattice/query" "lattice/dsl" "lattice/info" "lattice/info-stat"
    "lattice/physics/diff" "lattice/physics/diff3d"
    "lattice/physics/classical" "lattice/physics/classical3d"
    "lattice/sim" "lattice/automata" "lattice/number-theory" "lattice/pipeline"
    ;; FP subdirectories
    "lattice/fp" "lattice/fp/control" "lattice/fp/numeric" "lattice/fp/parsing"
    "lattice/fp/meta" "lattice/fp/data" "lattice/fp/symbolic"
    "lattice/fp/measure" "lattice/fp/rewrite"
    "lattice/fp/analysis" "lattice/fp/clp" "lattice/statistics"
    ;; Extracted from fp/ to top-level
    "lattice/optics" "lattice/game-theory" "lattice/control-systems"
    "lattice/meta" "lattice/crypto" "lattice/topology" "lattice/optimization"
    "lattice/physics/lenses"
    ;; Boundary directories
    "boundary" "boundary/tests" "boundary/lsp"))

;;; *header-scan-limit* : Nat
;;; Number of lines to scan for header annotations
(define *header-scan-limit* 60)

(define (register-module-path! name path)
  (doc 'type (-> Symbol String Void))
  (doc 'description "Register a module's file path.")
  (doc 'export #t)
  (hashtable-set! *module-paths* name path))

;;; Initialize known module paths (core modules)
(begin
 ;; BASE layer
 (register-module-path! 'prelude "core/base/prelude.ss")
 (register-module-path! 'sha256 "core/base/sha256.ss")
 (register-module-path! 'error "core/base/error.ss")
 
 ;; Block layer
 (register-module-path! 'block "core/blocks/block.ss")
 (register-module-path! 'cas "core/blocks/cas.ss")
 (register-module-path! 'normalize "core/blocks/normalize.ss")
 (register-module-path! 'expand "core/blocks/expand.ss")
 
 ;; Lang layer
 (register-module-path! 'parse "core/lang/parse.ss")
 (register-module-path! 'span "core/lang/span.ss")
 (register-module-path! 'fold-parse "core/lang/fold-parse.ss")
 (register-module-path! 'prim "core/lang/prim.ss")
 (register-module-path! 'eval "core/lang/eval.ss")
 (register-module-path! 'compile "core/lang/compile.ss")
 (register-module-path! 'module "core/lang/module.ss")
 (register-module-path! 'nbe "core/lang/nbe.ss")
 
 ;; Types layer
 (register-module-path! 'types "core/types/types.ss")
 (register-module-path! 'kinds "core/types/kinds.ss")
 (register-module-path! 'infer "core/types/infer.ss")
 (register-module-path! 'resolve "core/types/resolve.ss")
 (register-module-path! 'annotate "core/types/annotate.ss")
 (register-module-path! 'dep-types "core/types/dep-types.ss")
 
 ;; Query layer
 (register-module-path! 'query "lattice/query/query.ss")
 (register-module-path! 'query-dsl "lattice/query/query-dsl.ss")
 (register-module-path! 'aho-corasick "lattice/query/aho-corasick.ss")
 (register-module-path! 'optic-query "lattice/query/optic-query.ss")
 (register-module-path! 'query-macro "lattice/query/query-macro.ss")
 (register-module-path! 'ast-zipper "lattice/query/sql/ast-zipper.ss")
 
 ;; Data layer
 (register-module-path! 'stack "lattice/data/stack.ss")
 (register-module-path! 'queue "lattice/data/queue.ss")
 (register-module-path! 'set "lattice/data/set.ss")
 (register-module-path! 'dict "lattice/data/dict.ss")
 (register-module-path! 'data/alist "lattice/data/alist.ss")
 (register-module-path! 'collection-utils "lattice/data/collection-utils.ss")
 (register-module-path! 'graph-primitives "lattice/data/graph/graph-primitives.ss")
 (register-module-path! 'heap "lattice/data/heap.ss")
 (register-module-path! 'avl-tree "lattice/data/avl-tree.ss")
 (register-module-path! 'chase-lev-deque "lattice/data/chase-lev-deque.ss")
 (register-module-path! 'collection-protocol "lattice/data/collection-protocol.ss")
 (register-module-path! 'collection-impl "lattice/data/collection-impl.ss")
 (register-module-path! 'kdtree "lattice/data/kdtree.ss")
 (register-module-path! 'quadtree "lattice/data/quadtree.ss")
 (register-module-path! 'graph-matrix "lattice/data/graph/graph-matrix.ss")
 (register-module-path! 'graph-homology "lattice/data/graph/graph-homology.ss")
 (register-module-path! 'graph-filtration "lattice/data/graph/graph-filtration.ss")
 (register-module-path! 'graph-community "lattice/data/graph/graph-community.ss")
 (register-module-path! 'graph-layout "lattice/data/graph/graph-layout.ss")
 (register-module-path! 'pagerank "lattice/data/graph/pagerank.ss")
 (register-module-path! 'centrality "lattice/data/graph/centrality.ss")
 (register-module-path! 'random-graphs "lattice/data/graph/random-graphs.ss")
 (register-module-path! 'graph-bridge "lattice/data/graph/graph-bridge.ss")
 (register-module-path! 'spectral-community "lattice/data/graph/spectral-community.ss")
 (register-module-path! 'shortest-path "lattice/data/graph/shortest-path.ss")
 (register-module-path! 'max-flow "lattice/data/graph/max-flow.ss")

 ;; Optics layer
 (register-module-path! 'optics "lattice/optics/optics.ss")
 (register-module-path! 'profunctor-optics "lattice/optics/profunctor-optics.ss")
 (register-module-path! 'bidirectional "lattice/optics/bidirectional.ss")
 (register-module-path! 'format-iso "lattice/optics/format-iso.ss")
 (register-module-path! 'schema "lattice/optics/schema.ss")
 (register-module-path! 'block-migration "lattice/optics/block-migration.ss")
 (register-module-path! 'block-optics "lattice/optics/block-optics.ss")

 ;; Parallel layer
 (register-module-path! 'strategies "lattice/parallel/strategies.ss")
 (register-module-path! 'patterns "lattice/parallel/patterns.ss")

 ;; Linalg layer
 (register-module-path! 'iteration "lattice/linalg/iteration.ss")
 (register-module-path! 'vec-common "lattice/linalg/vec-common.ss")
 (register-module-path! 'vec "lattice/linalg/vec.ss")
 (register-module-path! 'vec2 "lattice/linalg/vec2.ss")
 (register-module-path! 'vec3 "lattice/linalg/vec3.ss")
 (register-module-path! 'matrix "lattice/linalg/matrix.ss")
 (register-module-path! 'matrix-decomp "lattice/linalg/matrix-decomp.ss")
 (register-module-path! 'matrix-solvers "lattice/linalg/matrix-solvers.ss")
 (register-module-path! 'matrix-eigen "lattice/linalg/matrix-eigen.ss")
 (register-module-path! 'svd "lattice/linalg/svd.ss")
 (register-module-path! 'sparse "lattice/linalg/sparse.ss")
 (register-module-path! 'matrix-blocked "lattice/linalg/matrix-blocked.ss")
 (register-module-path! 'matrix-blocked-decomp "lattice/linalg/matrix-blocked-decomp.ss")
 (register-module-path! 'iterative-solvers "lattice/linalg/iterative-solvers.ss")
 (register-module-path! 'integer-matrix "lattice/linalg/integer-matrix.ss")
 (register-module-path! 'numeric-instances "lattice/linalg/numeric-instances.ss")
 (register-module-path! 'dep-linalg "lattice/linalg/dep-linalg.ss")
 (register-module-path! 'graph-laplacian "lattice/linalg/graph-laplacian.ss")
 (register-module-path! 'quaternion "lattice/linalg/quaternion.ss")
 (register-module-path! 'matrix-optics "lattice/linalg/matrix-optics.ss")
 
 ;; Numeric layer
 (register-module-path! 'complex "lattice/numeric/complex.ss")
 (register-module-path! 'dft "lattice/numeric/dft.ss")
 (register-module-path! 'convolution "lattice/numeric/convolution.ss")
 (register-module-path! 'fft-convolve "lattice/numeric/fft-convolve.ss")
 (register-module-path! 'spectral-analysis "lattice/numeric/spectral-analysis.ss")
 (register-module-path! 'numeric/polynomial "lattice/numeric/polynomial.ss")
 (register-module-path! 'interpolate "lattice/numeric/interpolate.ss")
 (register-module-path! 'signal-poly "lattice/numeric/signal-poly.ss")
 (register-module-path! 'digital-filters "lattice/numeric/digital-filters.ss")
 (register-module-path! 'wavelet "lattice/numeric/wavelet.ss")
 (register-module-path! 'window-functions "lattice/numeric/window-functions.ss")
 (register-module-path! 'pde-time "lattice/numeric/pde-time.ss")
 (register-module-path! 'finite-diff "lattice/numeric/finite-diff.ss")
 (register-module-path! 'spectral-pde "lattice/numeric/spectral-pde.ss")
 (register-module-path! 'fem "lattice/numeric/fem.ss")
 (register-module-path! 'complex-bridge "lattice/numeric/complex-bridge.ss")
 (register-module-path! 'interval "lattice/numeric/interval.ss")
 (register-module-path! 'affine "lattice/numeric/affine.ss")
 (register-module-path! 'interval-integrate "lattice/numeric/interval-integrate.ss")
 
 ;; Number theory layer
 (register-module-path! 'modular "lattice/number-theory/modular.ss")
 (register-module-path! 'primality "lattice/number-theory/primality.ss")
 (register-module-path! 'fast-multiply "lattice/number-theory/fast-multiply.ss")

 ;; Crypto layer
 (register-module-path! 'sha512 "lattice/crypto/sha512.ss")
 (register-module-path! 'blake2b "lattice/crypto/blake2b.ss")
 (register-module-path! 'hmac "lattice/crypto/hmac.ss")
 (register-module-path! 'elliptic-curve "lattice/crypto/elliptic-curve.ss")

 ;; Info layer
 (register-module-path! 'entropy "lattice/info/entropy.ss")
 (register-module-path! 'coding "lattice/info/coding.ss")
 (register-module-path! 'channel-capacity "lattice/info/channel-capacity.ss")
 (register-module-path! 'statistical-measures "lattice/info/statistical-measures.ss")
 (register-module-path! 'epiplexity "lattice/info/epiplexity.ss")
 (register-module-path! 'rate-distortion "lattice/info/rate-distortion.ss")
 (register-module-path! 'model-selection-info "lattice/info/model-selection.ss")
 (register-module-path! 'empirical-info "lattice/info/empirical-info.ss")
 (register-module-path! 'partition-info "lattice/info/partition-info.ss")

 ;; Info-stat bridge layer
 (register-module-path! 'info-stat/mi-bridge "lattice/info-stat/mi-bridge.ss")

 ;; Topology layer
 (register-module-path! 'simplicial-complex "lattice/topology/simplicial-complex.ss")
 (register-module-path! 'homology "lattice/topology/homology.ss")
 (register-module-path! 'persistent "lattice/topology/persistent.ss")

 ;; Automata layer
 (register-module-path! 'statechart "lattice/automata/statechart.ss")
 (register-module-path! 'statechart-zipper "lattice/automata/statechart-zipper.ss")

 ;; Diffgeo layer
 (register-module-path! 'charts "lattice/diffgeo/charts.ss")
 (register-module-path! 'tangent "lattice/diffgeo/tangent.ss")
 (register-module-path! 'curvature "lattice/diffgeo/curvature.ss")
 (register-module-path! 'forms "lattice/diffgeo/forms.ss")
 (register-module-path! 'symbolic-metrics "lattice/diffgeo/symbolic-metrics.ss")
 (register-module-path! 'geodesics "lattice/diffgeo/geodesics.ss")
 (register-module-path! 'lie-groups "lattice/diffgeo/lie-groups.ss")

 ;; Sim layer
 (register-module-path! 'ode-system "lattice/sim/dynamics/ode-system.ss")
 (register-module-path! 'ode-state-space "lattice/sim/dynamics/ode-state-space.ss")
 (register-module-path! 'chaos "lattice/sim/dynamics/chaos.ss")
 (register-module-path! 'sim/stability "lattice/sim/dynamics/stability.ss")
 (register-module-path! 'sim/discrete "lattice/sim/dynamics/discrete.ss")
 (register-module-path! 'bifurcation "lattice/sim/dynamics/bifurcation.ss")
 (register-module-path! 'attractor-render "lattice/sim/dynamics/attractor-render.ss")
 (register-module-path! 'simulation-stream "lattice/sim/simulation-stream.ss")
 (register-module-path! 'des/event "lattice/sim/des/event.ss")
 (register-module-path! 'des/world "lattice/sim/des/world.ss")
 (register-module-path! 'des/engine "lattice/sim/des/engine.ss")
 (register-module-path! 'des/schedule "lattice/sim/des/schedule.ss")
 (register-module-path! 'des/replicate "lattice/sim/des/replicate.ss")

 ;; Dataset layer
 (register-module-path! 'difficulty "lattice/dataset/difficulty.ss")
 (register-module-path! 'parameter "lattice/dataset/parameter.ss")
 (register-module-path! 'distractor "lattice/dataset/distractor.ss")
 (register-module-path! 'sample "lattice/dataset/sample.ss")

 ;; Game theory layer
 (register-module-path! 'normal-form "lattice/game-theory/normal-form.ss")
 (register-module-path! 'extensive-form "lattice/game-theory/extensive-form.ss")
 (register-module-path! 'voting "lattice/game-theory/voting.ss")
 (register-module-path! 'mechanism "lattice/game-theory/mechanism.ss")
 (register-module-path! 'fair-division "lattice/game-theory/fair-division.ss")
 (register-module-path! 'coop-games "lattice/game-theory/coop-games.ss")
 (register-module-path! 'matching "lattice/game-theory/matching.ss")
 (register-module-path! 'strategic-voting "lattice/game-theory/strategic-voting.ss")
 (register-module-path! 'voting-games "lattice/game-theory/voting-games.ss")
 (register-module-path! 'mcdm "lattice/game-theory/mcdm.ss")
 (register-module-path! 'multi-winner "lattice/game-theory/multi-winner.ss")
 (register-module-path! 'evolutionary "lattice/game-theory/evolutionary.ss")
 (register-module-path! 'physics-dsl "lattice/game-theory/physics-dsl.ss")
 (register-module-path! 'game-theory/clp-equilibrium "lattice/game-theory/clp-equilibrium.ss")

 ;; Control systems layer
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

 ;; Optimization layer
 (register-module-path! 'convergence "lattice/optimization/convergence.ss")
 (register-module-path! 'line-search "lattice/optimization/line-search.ss")
 (register-module-path! 'lp "lattice/optimization/lp.ss")
 (register-module-path! 'ilp "lattice/optimization/ilp.ss")
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

 ;; Geometry layer
 (register-module-path! 'geometry "lattice/geometry/geometry.ss")
 (register-module-path! 'bvh "lattice/geometry/bvh.ss")
 (register-module-path! 'mesh-sdf "lattice/geometry/mesh-sdf.ss")
 (register-module-path! 'mesh-topology "lattice/geometry/mesh-topology.ss")
 (register-module-path! 'mesh-gen "lattice/geometry/mesh-gen.ss")
 (register-module-path! 'convex-hull "lattice/geometry/convex-hull.ss")
 (register-module-path! 'voronoi "lattice/geometry/voronoi.ss")
 (register-module-path! 'obj-loader "lattice/geometry/obj-loader.ss")
 (register-module-path! 'ascii-render "lattice/geometry/ascii-render.ss")
 (register-module-path! 'raymarch "lattice/geometry/raymarch.ss")
 (register-module-path! 'marching-cubes "lattice/geometry/marching-cubes.ss")
 (register-module-path! 'octree "lattice/geometry/octree.ss")
 (register-module-path! 'shape-protocol "lattice/geometry/shape-protocol.ss")
 (register-module-path! 'geometry-optics "lattice/geometry/geometry-optics.ss")

 ;; Algebra
 (register-module-path! 'group "lattice/algebra/group.ss")
 (register-module-path! 'ring "lattice/algebra/ring.ss")
 (register-module-path! 'field "lattice/algebra/field.ss")
 (register-module-path! 'algebra/polynomial "lattice/algebra/polynomial.ss")
 (register-module-path! 'multivariate "lattice/algebra/multivariate.ss")
 (register-module-path! 'groebner "lattice/algebra/groebner.ss")
 (register-module-path! 'module-theory "lattice/algebra/module.ss")
 (register-module-path! 'galois "lattice/algebra/galois.ss")
 (register-module-path! 'field-ext "lattice/algebra/field-ext.ss")
 (register-module-path! 'poly-bridge "lattice/algebra/poly-bridge.ss")
 (register-module-path! 'tropical "lattice/algebra/tropical.ss")
 (register-module-path! 'tropical-graph "lattice/algebra/tropical-graph.ss")

 ;; IPC
 (register-module-path! 'ipc-protocol "lattice/ipc/protocol.ss")

 ;; UI
 (register-module-path! 'layout "lattice/ui/layout.ss")
 (register-module-path! 'presentation "lattice/dataset/presentation.ss")

 ;; E-graph
 (register-module-path! 'egraph/union-find "lattice/egraph/union-find.ss")
 (register-module-path! 'egraph/eclass "lattice/egraph/eclass.ss")
 (register-module-path! 'egraph/egraph "lattice/egraph/egraph.ss")
 (register-module-path! 'egraph/match "lattice/egraph/match.ss")
 (register-module-path! 'egraph/saturation "lattice/egraph/saturation.ss")
 (register-module-path! 'egraph/cost "lattice/egraph/cost.ss")
 (register-module-path! 'egraph/extract "lattice/egraph/extract.ss")
 (register-module-path! 'egraph/scheduler "lattice/egraph/scheduler.ss")

 ;; FP layers
 (register-module-path! 'sort "lattice/data/sort.ss")
 (register-module-path! 'combinators "lattice/fp/meta/combinators.ss")
 (register-module-path! 'tree-zipper "lattice/fp/data/tree-zipper.ss")
 (register-module-path! 'transcendental "lattice/fp/numeric/transcendental.ss")
 (register-module-path! 'parser-combinators "lattice/fp/parsing/parser.ss")
 (register-module-path! 'state "lattice/fp/control/state.ss")
 (register-module-path! 'protocol "lattice/fp/protocol.ss")
 (register-module-path! 'special-functions "lattice/fp/numeric/special-functions.ss")
 (register-module-path! 'units "lattice/fp/measure/units.ss")
 (register-module-path! 'pretty-class "core/util/pretty-class.ss")
 (register-module-path! 'pretty-instances "lattice/fp/pretty-instances.ss")
 (register-module-path! 'cost-analysis "lattice/fp/analysis/cost-analysis.ss")
 (register-module-path! 'fusion-detect "lattice/fp/analysis/fusion-detect.ss")
 (register-module-path! 'parallel-detect "lattice/fp/analysis/parallel-detect.ss")
 (register-module-path! 'multi-category "lattice/fp/category/multi/category.ss")
 (register-module-path! 'domain "lattice/fp/clp/domain.ss")
 (register-module-path! 'templates "lattice/fp/templates.ss")
 (register-module-path! 'continuation "lattice/fp/control/continuation.ss")
 (register-module-path! 'free "lattice/fp/control/free.ss")
 (register-module-path! 'stream "lattice/fp/data/stream.ss")
 (register-module-path! 'zipper "lattice/fp/data/zipper.ss")
 (register-module-path! 'generic-zipper "lattice/fp/data/generic-zipper.ss")
 (register-module-path! 'fsm "lattice/fp/parsing/fsm.ss")
 (register-module-path! 'parser "lattice/fp/parsing/parser.ss")
 (register-module-path! 'result "lattice/fp/meta/result.ss")
 (register-module-path! 'protocol-bundle "lattice/fp/protocol-bundle.ss")
 (register-module-path! 'protocol-introspect "lattice/fp/protocol-introspect.ss")
 (register-module-path! 'natural-transform "lattice/fp/category/natural-transform.ss")
 (register-module-path! 'kan-extension "lattice/fp/category/kan-extension.ss")
 (register-module-path! 'qvec "lattice/fp/measure/qvec.ss")
 (register-module-path! 'markov "lattice/fp/markov.ss")

 ;; FP layer 2: depth-1 dependents
 (register-module-path! 'effects "lattice/fp/control/effects.ss")
 (register-module-path! 'fused-ops "lattice/fp/data/fused-ops.ss")
 (register-module-path! 'zipper-lens "lattice/fp/data/zipper-lens.ss")
 (register-module-path! 'logic "lattice/fp/meta/logic.ss")
 (register-module-path! 'parser-examples "lattice/fp/parsing/parser-examples.ss")
 (register-module-path! 'regex "lattice/fp/parsing/regex.ss")
 (register-module-path! 'parser-compile "lattice/fp/parsing/parser-compile.ss")
 (register-module-path! 'functor-general "lattice/fp/category/multi/functor-general.ss")
 (register-module-path! 'expr "lattice/fp/symbolic/expr.ss")
 (register-module-path! 'rule "lattice/fp/rewrite/rule.ss")
 (register-module-path! 'trace "lattice/fp/rewrite/trace.ss")
 (register-module-path! 'literal "lattice/fp/sat/literal.ss")
 (register-module-path! 'adjunction "lattice/fp/category/adjunction.ss")

 ;; FP layer 3: deeper dependents
 (register-module-path! 'random-effect "lattice/fp/control/random-effect.ss")
 (register-module-path! 'dsl "lattice/fp/meta/dsl.ss")
 (register-module-path! 'rewrite/sexp-zipper "lattice/fp/rewrite/sexp-zipper.ss")
 (register-module-path! 'verify "lattice/fp/rewrite/verify.ss")
 (register-module-path! 'goals "lattice/fp/rewrite/goals.ss")
 (register-module-path! 'simplify "lattice/fp/symbolic/simplify.ss")
 (register-module-path! 'egraph-simplify "lattice/fp/symbolic/egraph-simplify.ss")
 (register-module-path! 'diff "lattice/fp/symbolic/diff.ss")
 (register-module-path! 'clause "lattice/fp/sat/clause.ss")
 (register-module-path! 'assignment "lattice/fp/sat/assignment.ss")
 (register-module-path! 'watches "lattice/fp/sat/watches.ss")
 (register-module-path! 'monad-derivation "lattice/fp/category/monad-derivation.ss")
 (register-module-path! 'logic-adjunction "lattice/fp/category/logic-adjunction.ss")
 (register-module-path! 'comonad "lattice/fp/category/comonad.ss")
 (register-module-path! 'abstract-interp "lattice/fp/category/abstract-interp.ss")
 (register-module-path! 'nat-transform-indexed "lattice/fp/category/multi/nat-transform-indexed.ss")
 (register-module-path! 'store "lattice/fp/clp/store.ss")

 ;; FP layer 4: engine, solver, category
 (register-module-path! 'engine "lattice/fp/rewrite/engine.ss")
 (register-module-path! 'solve "lattice/fp/symbolic/solve.ss")
 (register-module-path! 'integrate "lattice/fp/symbolic/integrate.ss")
 (register-module-path! 'poly-canonical "lattice/fp/symbolic/poly-canonical.ss")
 (register-module-path! 'cnf "lattice/fp/sat/cnf.ss")
 (register-module-path! 'solver "lattice/fp/sat/solver.ss")
 (register-module-path! 'kleisli "lattice/fp/category/kleisli.ss")
 (register-module-path! 'common-monads "lattice/fp/category/common-monads.ss")
 (register-module-path! 'limits "lattice/fp/category/limits.ss")
 (register-module-path! 'state-store-adjunction "lattice/fp/category/state-store-adjunction.ss")
 (register-module-path! 'free-algebra "lattice/fp/category/free-algebra.ss")
 (register-module-path! 'adjunction-inter "lattice/fp/category/multi/adjunction-inter.ss")
 (register-module-path! 'fd-constraints "lattice/fp/clp/fd-constraints.ss")

 ;; FP layer 5-7: laws, SAT, CLP, effects
 (register-module-path! 'rewrite/laws "lattice/fp/rewrite/laws.ss")
 (register-module-path! 'sat "lattice/fp/sat/sat.ss")
 (register-module-path! 'propagate "lattice/fp/clp/propagate.ss")
 (register-module-path! 'effect-adjunction "lattice/fp/category/multi/effect-adjunction.ss")
 (register-module-path! 'effect-category "lattice/fp/category/effect-category.ss")
 (register-module-path! 'rewrite/fusion-rules "lattice/fp/rewrite/fusion-rules.ss")
 (register-module-path! 'rewrite/proof-tactics "lattice/fp/rewrite/proof-tactics.ss")
 (register-module-path! 'rewrite/sketch "lattice/fp/rewrite/sketch.ss")
 (register-module-path! 'maxsat "lattice/fp/sat/maxsat.ss")
 (register-module-path! 'label "lattice/fp/clp/label.ss")
 (register-module-path! 'global-constraints "lattice/fp/clp/global-constraints.ss")
 (register-module-path! 'clp "lattice/fp/clp/clp.ss")

 ;; Autodiff layer — core
 (register-module-path! 'comp-graph "core/autodiff/comp-graph.ss")
 (register-module-path! 'reverse-diff "core/autodiff/reverse-diff.ss")
 ;; Autodiff layer — lattice
 (register-module-path! 'higher-order-diff "lattice/autodiff/higher-order-diff.ss")
 (register-module-path! 'sparse-autodiff "lattice/autodiff/sparse-autodiff.ss")
 (register-module-path! 'differentiable "lattice/autodiff/differentiable.ss")
 (register-module-path! 'differentiable-signal "lattice/autodiff/differentiable-signal.ss")
 (register-module-path! 'profiling "lattice/autodiff/profiling.ss")
 (register-module-path! 'traced-optics "lattice/autodiff/traced-optics.ss")
 (register-module-path! 'optic-functors "lattice/autodiff/optic-functors.ss")
 (register-module-path! 'symbolic-diff "lattice/autodiff/symbolic-diff.ss")

 ;; Random layer
 (register-module-path! 'prng "lattice/random/prng.ss")
 (register-module-path! 'distributions "lattice/random/distributions.ss")
 (register-module-path! 'probability "lattice/random/probability.ss")
 (register-module-path! 'monte-carlo "lattice/random/monte-carlo.ss")
 (register-module-path! 'bayesian "lattice/random/bayesian.ss")
 (register-module-path! 'variational-inference "lattice/random/variational-inference.ss")

 ;; QuickCheck layer
 (register-module-path! 'qc-generators "lattice/quickcheck/generators.ss")
 (register-module-path! 'qc-shrink "lattice/quickcheck/shrink.ss")
 (register-module-path! 'quickcheck "lattice/quickcheck/quickcheck.ss")
 (register-module-path! 'gen-linalg "lattice/quickcheck/gen-linalg.ss")
 (register-module-path! 'qc-laws "lattice/quickcheck/laws.ss")

 ;; Statistics core
 (register-module-path! 'summary-stats "lattice/statistics/core/summary-stats.ss")
 (register-module-path! 'result-types "lattice/statistics/core/result-types.ss")
 (register-module-path! 'diagnostics "lattice/statistics/core/diagnostics.ss")
 (register-module-path! 'design-matrix "lattice/statistics/core/design-matrix.ss")
 (register-module-path! 'model-protocol "lattice/statistics/core/model-protocol.ss")

 ;; Statistics hypothesis
 (register-module-path! 'stat/distributions "lattice/statistics/hypothesis/distributions.ss")
 (register-module-path! 't-test "lattice/statistics/hypothesis/t-test.ss")
 (register-module-path! 'f-test "lattice/statistics/hypothesis/f-test.ss")
 (register-module-path! 'chi-squared "lattice/statistics/hypothesis/chi-squared.ss")
 (register-module-path! 'anova "lattice/statistics/hypothesis/anova.ss")

 ;; Statistics regression
 (register-module-path! 'families "lattice/statistics/regression/families.ss")
 (register-module-path! 'link-functions "lattice/statistics/regression/link-functions.ss")
 (register-module-path! 'linear "lattice/statistics/regression/linear.ss")
 (register-module-path! 'regularized "lattice/statistics/regression/regularized.ss")
 (register-module-path! 'glm "lattice/statistics/regression/glm.ss")
 (register-module-path! 'traced-regression "lattice/statistics/regression/traced-regression.ss")
 (register-module-path! 'autodiff-fit "lattice/statistics/regression/autodiff-fit.ss")

 ;; Statistics timeseries
 (register-module-path! 'differencing "lattice/statistics/timeseries/differencing.ss")
 (register-module-path! 'exponential "lattice/statistics/timeseries/exponential.ss")
 (register-module-path! 'forecast "lattice/statistics/timeseries/forecast.ss")
 (register-module-path! 'acf-pacf "lattice/statistics/timeseries/acf-pacf.ss")
 (register-module-path! 'ar "lattice/statistics/timeseries/ar.ss")
 (register-module-path! 'ma "lattice/statistics/timeseries/ma.ss")
 (register-module-path! 'ar-poly "lattice/statistics/timeseries/ar-poly.ss")
 (register-module-path! 'spectral-ts "lattice/statistics/spectral-ts.ss")

 ;; DSL layer
 (register-module-path! 'tagless "lattice/dsl/tagless.ss")
 (register-module-path! 'quasi "lattice/dsl/quasi.ss")
 (register-module-path! 'staging "lattice/dsl/staging.ss")
 (register-module-path! 'dsl/reader "lattice/dsl/reader.ss")
 (register-module-path! 'dsl/match "lattice/dsl/match.ss")
 (register-module-path! 'dsl/compose "lattice/dsl/compose.ss")
 (register-module-path! 'partial-eval "lattice/dsl/partial-eval.ss")
 (register-module-path! 'chronicle "lattice/dsl/chronicle/chronicle.ss")
 (register-module-path! 'chronicle-parser "lattice/dsl/chronicle/chronicle-parser.ss")
 (register-module-path! 'tagless-chronicle "lattice/dsl/chronicle/tagless-chronicle.ss")
 (register-module-path! 'dsl/template "lattice/dsl/template/template.ss")
 (register-module-path! 'template-zipper "lattice/dsl/template/template-zipper.ss")
 (register-module-path! 'markdown-ast "lattice/dsl/markdown/ast.ss")
 (register-module-path! 'inline-parser "lattice/dsl/markdown/inline-parser.ss")
 (register-module-path! 'block-parser "lattice/dsl/markdown/block-parser.ss")
 (register-module-path! 'markdown-html "lattice/dsl/markdown/html.ss")

 ;; Tiles layer
 (register-module-path! 'tiles/core "lattice/tiles/core.ss")
 (register-module-path! 'tiles/square "lattice/tiles/square.ss")
 (register-module-path! 'tiles/hex "lattice/tiles/hex.ss")
 (register-module-path! 'tiles/triangle "lattice/tiles/triangle.ss")
 (register-module-path! 'pathfinding "lattice/tiles/pathfinding.ss")
 (register-module-path! 'visibility "lattice/tiles/visibility.ss")
 (register-module-path! 'tiles/render "lattice/tiles/render.ss")
 (register-module-path! 'tiles/units "lattice/tiles/units.ss")
 (register-module-path! 'turns "lattice/tiles/turns.ss")
 (register-module-path! 'topology-analysis "lattice/tiles/topology-analysis.ss")
 (register-module-path! 'boardcraft "lattice/tiles/boardcraft.ss")

 ;; Pipeline layer
 (register-module-path! 'pipeline/context "lattice/pipeline/context.ss")
 (register-module-path! 'pipeline/stage "lattice/pipeline/stage.ss")
 (register-module-path! 'pipeline/effects "lattice/pipeline/effects.ss")
 (register-module-path! 'council "lattice/pipeline/council.ss")
 (register-module-path! 'council-voting "lattice/pipeline/council-voting.ss")
 (register-module-path! 'curriculum "lattice/pipeline/curriculum.ss")
 (register-module-path! 'pipeline/dsl "lattice/pipeline/dsl.ss")
 (register-module-path! 'rlm2 "lattice/pipeline/rlm2.ss")
 (register-module-path! 'rlm2-hud "lattice/pipeline/rlm2-hud.ss")
 (register-module-path! 'rlm2-parse "lattice/pipeline/rlm2-parse.ss")

 ;; Physics classical (2D)
 (register-module-path! 'rigid-body "lattice/physics/classical/rigid-body.ss")
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

 ;; Physics classical3d (3D)
 (register-module-path! 'rigid-body3d "lattice/physics/classical3d/rigid-body3d.ss")
 (register-module-path! 'shapes3d "lattice/physics/classical3d/shapes3d.ss")
 (register-module-path! 'collision-detection3d "lattice/physics/classical3d/collision-detection3d.ss")
 (register-module-path! 'constraints3d "lattice/physics/classical3d/constraints3d.ss")
 (register-module-path! 'constraint-solver3d "lattice/physics/classical3d/constraint-solver3d.ss")
 (register-module-path! 'world3d "lattice/physics/classical3d/world3d.ss")

 ;; Physics diff (2D differentiable)
 (register-module-path! 'traced-vec2 "lattice/physics/diff/traced-vec2.ss")
 (register-module-path! 'traced-body "lattice/physics/diff/traced-body.ss")
 (register-module-path! 'traced-body-protocols "lattice/physics/diff/traced-body-protocols.ss")
 (register-module-path! 'traced-integrators "lattice/physics/diff/traced-integrators.ss")
 (register-module-path! 'smooth-collision "lattice/physics/diff/smooth-collision.ss")
 (register-module-path! 'diff-collision "lattice/physics/diff/diff-collision.ss")
 (register-module-path! 'diff-constraints "lattice/physics/diff/diff-constraints.ss")
 (register-module-path! 'rollout "lattice/physics/diff/rollout.ss")
 (register-module-path! 'physics/optimize "lattice/physics/diff/optimize.ss")
 (register-module-path! 'optic-optimize "lattice/physics/diff/optic-optimize.ss")

 ;; Physics diff3d (3D differentiable)
 (register-module-path! 'traced-vec3 "lattice/physics/diff3d/traced-vec3.ss")
 (register-module-path! 'traced-quaternion "lattice/physics/diff3d/traced-quaternion.ss")
 (register-module-path! 'traced-body3d "lattice/physics/diff3d/traced-body3d.ss")
 (register-module-path! 'traced-integrators3d "lattice/physics/diff3d/traced-integrators3d.ss")
 (register-module-path! 'smooth-collision3d "lattice/physics/diff3d/smooth-collision3d.ss")
 (register-module-path! 'diff-collision3d "lattice/physics/diff3d/diff-collision3d.ss")
 (register-module-path! 'diff-constraints3d "lattice/physics/diff3d/diff-constraints3d.ss")
 (register-module-path! 'rollout3d "lattice/physics/diff3d/rollout3d.ss")
 (register-module-path! 'optimize3d "lattice/physics/diff3d/optimize3d.ss")

 ;; Physics lenses
 (register-module-path! 'lenses "lattice/physics/lenses/lenses.ss")
 (register-module-path! 'lenses3d "lattice/physics/lenses/lenses3d.ss")
 (register-module-path! 'optics-integration "lattice/physics/lenses/optics-integration.ss")

 ;; Physics problems
 (register-module-path! 'simulation "lattice/physics/problems/simulation.ss")
 (register-module-path! 'physics-problem "lattice/physics/problems/physics-problem.ss")
 (register-module-path! 'spatial "lattice/physics/problems/templates/spatial.ss")

 ;; Meta modules
 (register-module-path! 'meta/fuel-vocab "lattice/meta/fuel-vocab.ss")
 (register-module-path! 'meta/purity-audit "lattice/meta/purity-audit.ss")
 (register-module-path! 'meta/bridge-discovery "lattice/meta/bridge-discovery.ss")
 (register-module-path! 'meta/readme-audit "lattice/meta/readme-audit.ss")
 (register-module-path! 'meta/fuel-report "lattice/meta/fuel-report.ss")
 (register-module-path! 'meta/module-manifest "lattice/meta/module-manifest.ss")
 (register-module-path! 'meta/serendipity "lattice/meta/serendipity.ss")
 (register-module-path! 'meta/promotion "lattice/meta/promotion.ss")
 (register-module-path! 'meta/bbs-xref "lattice/meta/bbs-xref.ss")
 (register-module-path! 'ode-adaptive "lattice/sim/dynamics/ode-adaptive.ss")
 (register-module-path! 'autodiff/ode-jacobian "lattice/autodiff/ode-jacobian.ss")
 (register-module-path! 'contract "lattice/validation/contract.ss")
 (register-module-path! 'pipeline/rlm2-metrics "lattice/pipeline/rlm2-metrics.ss")

 ;; Boundary modules
 (register-module-path! 'boundary/bbs "boundary/bbs/bbs.ss"))

;;; clear-module-caches! : → Void
;;; Clear header cache (useful after file modifications).
(define (clear-module-caches!)
  (hashtable-clear! *header-cache*))

;;; ====
;;; Header Parsing
;;; ====

;;; read-header-lines : String × Nat → (Option (List String))
;;; Read first n lines from file for header parsing. Returns #f if file doesn't exist.
(define (read-header-lines filepath n)
  (guard (exn [else #f])
         (call-with-input-file filepath
                               (lambda (port)
                                       (let loop ([i 0] [lines '()])
                                            (if (>= i n)
                                                (reverse lines)
                                                (let ([line (get-line port)])
                                                     (if (eof-object? line)
                                                         (reverse lines)
                                                         (loop (+ i 1) (cons line lines))))))))))

;;; extract-annotation : String × String → (Option String)
;;; Extract value from ";;; @key value" line.
(define (extract-annotation line prefix)
  (let ([trimmed (string-trim line)])
       (and (string-starts-with? trimmed ";;;")
            (let ([after-comment (string-trim (substring trimmed 3 (string-length trimmed)))])
                 (and (string-starts-with? after-comment prefix)
                      (string-trim (substring after-comment
                                              (string-length prefix)
                                              (string-length after-comment))))))))

;;; parse-module-name : (List String) → (Option Symbol)
;;; Extract module name from @module annotation.
(define (parse-module-name lines)
  (let loop ([lines lines])
       (cond
        [(null? lines) #f]
        [else
         (let ([name (extract-annotation (car lines) "@module ")])
              (if (and name (> (string-length name) 0))
                  (string->symbol name)
                  (loop (cdr lines))))])))

;;; parse-requires : (List String) → (List Symbol)
;;; Extract dependencies from ALL @requires annotations.
;;; Supports multiple @requires lines and accumulates all deps.
(define (parse-requires lines)
  (let loop ([lines lines] [acc '()])
       (cond
        [(null? lines) (reverse acc)]
        [else
         (let ([deps (extract-annotation (car lines) "@requires ")])
              (if deps
                  (let ([parsed (filter (lambda (s) (> (string-length (symbol->string s)) 0))
                                        (map string->symbol
                                             (filter (lambda (s) (> (string-length s) 0))
                                                     (string-split deps #\space))))])
                       (loop (cdr lines) (append (reverse parsed) acc)))
                  (loop (cdr lines) acc)))])))

;;; parse-module-header : String → (Option (Pair Symbol (List Symbol)))
;;; Parse @module/@requires from file. Returns (name . deps) or #f.
;;; Only caches successful parses (not file-not-found failures).
(define (parse-module-header filepath)
  ;; Check cache first
  (let ([cached (hashtable-ref *header-cache* filepath 'not-found)])
       (if (not (eq? cached 'not-found))
           cached
           (let* ([lines (read-header-lines filepath *header-scan-limit*)]
                  [result (and lines
                               (let ([name (parse-module-name lines)])
                                    (and name
                                         (cons name (parse-requires lines)))))])
                 ;; Only cache successful parses (don't cache failures)
                 (when result
                       (hashtable-set! *header-cache* filepath result))
                 result))))

;;; ====
;;; Path Resolution
;;; ====

;;; *module-base-dirs* : (List String)
;;; Base directories for namespaced module resolution (e.g., 'diffgeo/charts → lattice/diffgeo/charts.ss)
(define *module-base-dirs*
  '("lattice" "core" "boundary"))

;;; find-module-path : Symbol → (Option String)
;;; Find file path for a module using manifest-based index.
;;;
;;; Supports two forms:
;;;   - Simple:     (require 'vec)              → O(1) lookup in manifest index
;;;   - Namespaced: (require 'linalg/vec)       → O(1) lookup, always unambiguous
;;;
;;; Priority:
;;;   1. Manifest namespaced index (for 'skill/module form)
;;;   2. Manifest simple index (for 'module form)
;;;   3. Legacy fallback (for base directories if index not initialized)
(define (find-module-path name)
  (let ([name-str (symbol->string name)])
       (if (string-contains? name-str "/")
           ;; Namespaced: check namespaced index, then legacy fallback
           (or (and *manifest-namespaced-index*
                    (hashtable-ref *manifest-namespaced-index* name #f))
               (find-namespaced-module name-str))
           ;; Simple: check simple index, then namespaced index, then legacy fallback
           (or (and *manifest-module-index*
                    (hashtable-ref *manifest-module-index* name #f))
               (and *manifest-namespaced-index*
                    (hashtable-ref *manifest-namespaced-index* name #f))
               (find-simple-module name-str)))))

;;; string-contains? : String × String → Boolean
;;; Check if haystack contains needle.
(define (string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i n-len) h-len) #f]
             [(string=? (substring haystack i (+ i n-len)) needle) #t]
             [else (loop (+ i 1))]))))

;;; find-namespaced-module : String → (Option String)
;;; Find module by namespaced path (e.g., "diffgeo/charts" → "lattice/diffgeo/charts.ss")
(define (find-namespaced-module name-str)
  (let loop ([bases *module-base-dirs*])
       (if (null? bases)
           #f
           (let ([path (string-append (car bases) "/" name-str ".ss")])
                (if (file-exists? path)
                    path
                    (loop (cdr bases)))))))

;;; find-simple-module : String → (Option String)
;;; Find module by simple name using first-match-wins from search directories.
(define (find-simple-module name-str)
  (let loop ([dirs *module-search-dirs*])
       (if (null? dirs)
           #f
           (let ([path (string-append (car dirs) "/" name-str ".ss")])
                (if (file-exists? path)
                    path
                    (loop (cdr dirs)))))))

;;; find-all-module-paths : String → (List String)
;;; Find ALL paths matching a simple module name (for collision detection).
(define (find-all-module-paths name-str)
  (filter file-exists?
          (map (lambda (dir) (string-append dir "/" name-str ".ss"))
               *module-search-dirs*)))

;;; check-module-collision : Symbol → Void
;;; Warn if a simple module name has multiple matches in search directories.
;;; Only fires once per module (require-one short-circuits on already-loaded).
(define (check-module-collision name)
  (let* ([name-str (symbol->string name)]
         [all-paths (find-all-module-paths name-str)])
        (when (> (length all-paths) 1)
              (let ([resolved (module-name->path name)])
                (display (format "  Warning: '~a' matches ~a files:~%" name (length all-paths)))
                (for-each (lambda (p)
                            (display (format "      ~a ~a~%"
                                             (if (and resolved (string=? p resolved)) "*" " ")
                                             p)))
                          all-paths)
                (display (format "    Consider: (require '~a)~%"
                                 (if resolved
                                     (path->namespace resolved)
                                     (path->namespace (car all-paths)))))))))

;;; path->namespace : String → String
;;; Convert path like "lattice/diffgeo/charts.ss" to namespace "diffgeo/charts"
;;; Dynamically matches against *module-base-dirs* for extensibility.
(define (path->namespace path)
  (let* ([without-ext (if (string-ends-with? path ".ss")
                          (substring path 0 (- (string-length path) 3))
                          path)])
        ;; Match against base directories dynamically
        (let loop ([bases *module-base-dirs*])
             (if (null? bases)
                 without-ext  ; No match, return as-is
                 (let ([base-prefix (string-append (car bases) "/")])
                      (if (string-starts-with? without-ext base-prefix)
                          (substring without-ext
                                     (string-length base-prefix)
                                     (string-length without-ext))
                          (loop (cdr bases))))))))

;;; string-ends-with? : String × String → Boolean
(define (string-ends-with? str suffix)
  (let ([s-len (string-length str)]
        [x-len (string-length suffix)])
       (and (>= s-len x-len)
            (string=? (substring str (- s-len x-len) s-len) suffix))))

;;; module-name->path : Symbol → (Option String)
;;; Get file path for module, using registry or searching.
;;; Discovered modules are registered in *module-paths* so they appear in (modules).
(define (module-name->path name)
  ;; Check explicit registry first
  (or (hashtable-ref *module-paths* name #f)
      ;; Then search and register if found
      (let ([found (find-module-path name)])
           (when found
                 ;; Register in *module-paths* so it appears in (modules) listing
                 (hashtable-set! *module-paths* name found))
           found)))

;;; ====
;;; Auto-Registration from Headers
;;; ====

;;; auto-register-module! : Symbol → Void
;;; Parse module header and register dependencies if not already known.
;;; Uses the requested name (not the header's @module name) as the key.
(define (auto-register-module! name)
  (unless (hashtable-contains? *module-deps* name)
          (let ([path (module-name->path name)])
               (when path
                     (let ([header (parse-module-header path)])
                          (when header
                                ;; Register under the requested name, using deps from header
                                (hashtable-set! *module-deps* name (cdr header))))))))

;;; ====
;;; Bootstrap Dependencies
;;; ====

;;; prelude is the foundation - it has no dependencies
;;; All other modules now declare deps via @requires headers
(hashtable-set! *module-deps* 'prelude '())

;;; ====
;;; Module Loading
;;; ====

;;; current-time-ms : Unit → Nat
(define (current-time-ms)
  (let ([t (current-time)])
       (+ (* (time-second t) 1000)
          (quotient (time-nanosecond t) 1000000))))

;;; module-loaded? : Symbol → Boolean
(define (module-loaded? name)
  (let ([entry (hashtable-ref *module-registry* name #f)])
       (and entry (car entry))))

;;; module-load-time : Symbol → (Option Nat)
(define (module-load-time name)
  (let ([entry (hashtable-ref *module-registry* name #f)])
       (and entry (cdr entry))))

;;; load-module! : Symbol → Void
;;; Load a single module (without dependencies).
(define (load-module! name)
  (unless (module-loaded? name)
          (let* ([path (or (module-name->path name)
                           (string-append (symbol->string name) ".ss"))]
                 [start (current-time-ms)])
                (load path)
                (let ([duration (- (current-time-ms) start)])
                     (hashtable-set! *module-registry* name (cons #t duration))
                     (set! *load-order* (cons name *load-order*))))))

;;; format-cycle : Symbol × (List Symbol) → String
;;; Format a circular dependency chain for error message.
(define (format-cycle name stack)
  (let* ([cycle-start (member name stack)]
         [cycle (if cycle-start
                    (reverse (cons name cycle-start))
                    (reverse (cons name stack)))])
        (apply string-append
               (cons (symbol->string (car cycle))
                     (map (lambda (m) (string-append " -> " (symbol->string m)))
                          (cdr cycle))))))

;;; require-one : Symbol → Void
;;; Load a module and all its dependencies.
;;; Auto-registers dependencies from file headers if not already known.
;;; Detects circular dependencies and raises an error with the cycle path.
(define (require-one name)
  (cond
   ;; Already loaded - nothing to do
   [(module-loaded? name) (void)]
   ;; Currently loading - circular dependency detected!
   [(memq name *loading-stack*)
    (error 'require-one
           (string-append "Circular dependency detected: "
                          (format-cycle name *loading-stack*)))]
   ;; Not loaded yet - auto-register, load dependencies, then this module
   [else
    ;; Try to discover dependencies from header if not known
    (auto-register-module! name)
    ;; Warn about collisions for simple (non-namespaced) module names
    (unless (string-contains? (symbol->string name) "/")
            (check-module-collision name))
    (let ([deps (hashtable-ref *module-deps* name '())])
         ;; Push/pop loading stack with dynamic-wind so failed loads
         ;; don't poison the stack with stale "in-progress" entries
         ;; (which would cause false circular-dependency errors on retry)
         (dynamic-wind
           (lambda () (set! *loading-stack* (cons name *loading-stack*)))
           (lambda ()
             ;; Load dependencies first (may raise circular dependency error)
             (for-each require-one deps)
             ;; Then load this module
             (load-module! name))
           (lambda () (set! *loading-stack* (cdr *loading-stack*)))))]))

;;; require : Symbol ... → Void
;;; Load one or more modules with their dependencies.
;;; Prints a one-liner for each freshly loaded module.
(define (require . names)
  (for-each
   (lambda (name)
     (let ([was-loaded (module-loaded? name)]
           [loaded-before (length *load-order*)])
       (require-one name)
       (unless was-loaded
         (let* ([time (or (module-load-time name) 0)]
                [new-modules (- (length *load-order*) loaded-before)]
                [new-deps (- new-modules 1)])
           (if (> new-deps 0)
               (display (format "  ~a loaded (~a deps, ~ams)\n" name new-deps time))
               (display (format "  ~a loaded (~ams)\n" name time)))))))
   names))

;;; ====
;;; Module Information
;;; ====

;;; module-deps : Symbol → (List Symbol)
;;; Get declared dependencies for a module.
(define (module-deps name)
  (hashtable-ref *module-deps* name '()))

;;; loading-stack : Unit → (List Symbol)
;;; Get the current loading stack (modules in progress).
(define (loading-stack)
  *loading-stack*)

;;; detect-cycle : Symbol → (Option (List Symbol))
;;; Check if loading a module would create a circular dependency.
;;; Returns the cycle path if a cycle exists, #f otherwise.
(define (detect-cycle name)
  (detect-cycle-helper name '()))

;;; detect-cycle-helper : Symbol × (List Symbol) → (Option (List Symbol))
(define (detect-cycle-helper name visited)
  (cond
   [(memq name visited)
    ;; Found a cycle - return the path
    (reverse (cons name (member name (reverse visited))))]
   [(module-loaded? name) #f]
   [else
    (let ([deps (module-deps name)])
         (let loop ([remaining deps])
              (cond
               [(null? remaining) #f]
               [else
                (let ([result (detect-cycle-helper (car remaining)
                                                   (cons name visited))])
                     (if result
                         result
                         (loop (cdr remaining))))])))]))

;;; validate-deps : Unit → (List (Pair Symbol (List Symbol)))
;;; Check all registered modules for circular dependencies.
;;; Returns a list of (module . cycle-path) pairs for any cycles found.
(define (validate-deps)
  (let ([modules (vector->list (hashtable-keys *module-deps*))])
       (filter (lambda (x) x)
               (map (lambda (m)
                            (let ([cycle (detect-cycle m)])
                                 (if cycle
                                     (cons m cycle)
                                     #f)))
                    modules))))

;;; all-deps : Symbol → (List Symbol)
;;; Get all transitive dependencies for a module.
(define (all-deps name)
  (let ([direct (module-deps name)])
       (if (null? direct)
           '()
           (unique (append direct (append-map all-deps direct))))))

;;; module-stats : Unit → Void
;;; Display loading statistics.
(define (module-stats)
  (display "
==================== MODULE STATISTICS =====================

")
  
  (let* ([loaded (reverse *load-order*)]
         [total-time (fold-left + 0 (map (lambda (m) (or (module-load-time m) 0)) loaded))])
        
        (display "  Loaded modules:
")
        (for-each
         (lambda (m)
                 (let ([time (module-load-time m)])
                      (display (format "    ~a~a~ams
"
                                       m
                                       (make-string (max 1 (- 20 (string-length (symbol->string m)))) #\space)
                                       (or time 0)))))
         loaded)
        
        (newline)
        (display (format "  Total: ~a modules, ~ams

" (length loaded) total-time))))

;;; module-graph : Unit → Void
;;; Display dependency graph.
(define (module-graph)
  (display "
==================== DEPENDENCY GRAPH ======================

")
  
  (let ([modules (vector->list (hashtable-keys *module-deps*))])
       (for-each
        (lambda (m)
                (let ([deps (module-deps m)])
                     (display (format "  ~a → ~a
" m
                                      (if (null? deps) "(none)" (apply string-append
                                                                       (map (lambda (d) (string-append (symbol->string d) " ")) deps)))))))
        (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) modules))))

;;; ====
;;; Programmatic Introspection
;;; ====

(doc loaded-modules 'type '(-> (List Symbol)))
(doc loaded-modules 'description "Return list of currently loaded module names in load order.")
(define (loaded-modules)
  (reverse *load-order*))

(doc loaded-module-count 'type '(-> Int))
(doc loaded-module-count 'description "Return number of loaded modules.")
(define (loaded-module-count)
  (length *load-order*))

(doc total-load-time 'type '(-> Int))
(doc total-load-time 'description "Return total module load time in milliseconds.")
(define (total-load-time)
  (fold-left + 0 (map (lambda (m) (or (module-load-time m) 0)) *load-order*)))

(doc session-info 'type '(-> Alist))
(doc session-info 'description "Return session state as an alist with modules, load-order, counts, and timing info.")
(define (session-info)
  (let* ([loaded (loaded-modules)]
         [total-time (total-load-time)]
         [load-times (map (lambda (m)
                            (cons m (or (module-load-time m) 0)))
                          loaded)])
    `((modules . ,loaded)
      (module-count . ,(length loaded))
      (load-order . ,*load-order*)
      (total-load-time-ms . ,total-time)
      (load-times . ,load-times)
      (cached-headers . ,(hashtable-size *header-cache*))
      (registered-paths . ,(hashtable-size *module-paths*))
      (registered-deps . ,(hashtable-size *module-deps*)))))

(doc session-summary 'type '(-> Void))
(doc session-summary 'description "Print a brief session summary.")
(define (session-summary)
  (let ([info (session-info)])
    (printf "\nSession Summary\n")
    (printf "---------------------------------------\n")
    (printf "  Loaded modules:    ~a\n" (cdr (assq 'module-count info)))
    (printf "  Total load time:   ~ams\n" (cdr (assq 'total-load-time-ms info)))
    (printf "  Cached headers:    ~a\n" (cdr (assq 'cached-headers info)))
    (printf "  Registered paths:  ~a\n" (cdr (assq 'registered-paths info)))
    (printf "---------------------------------------\n\n")))

;;; ====
;;; Manifest Integration
;;; ====

(doc 'section 'manifest-integration)

(doc module-paths-snapshot 'type '(-> (List (Pair Symbol String))))
(doc module-paths-snapshot 'description "Export current *module-paths* as a sorted alist. Useful for building content-addressed manifests from the live registry.")
(doc module-paths-snapshot 'export #t)
(define (module-paths-snapshot)
  (let-values ([(keys vals) (hashtable-entries *module-paths*)])
    (list-sort
      (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
      (let loop ([i 0] [acc '()])
        (if (>= i (vector-length keys))
            acc
            (loop (+ i 1)
                  (cons (cons (vector-ref keys i) (vector-ref vals i)) acc)))))))

(doc register-module-paths! 'type '(-> (List (Pair Symbol String)) Void))
(doc register-module-paths! 'description "Register multiple module paths from a list of (name . path) pairs.
More explicit than individual register-module-path! calls — manifests bulk-loading their
module entries through this makes the source of truth clear.")
(doc register-module-paths! 'export #t)
(define (register-module-paths! entries)
  (for-each
    (lambda (e)
      (hashtable-set! *module-paths* (car e) (cdr e)))
    entries))

;;; ====
;;; Convenience
;;; ====

;;; require-core : Unit → Void
;;; Load all core modules.
(define (require-core)
  (require 'compile 'error 'annotate))

;;; require-all : Unit → Void
;;; Load everything.
(define (require-all)
  (let ([all-modules (vector->list (hashtable-keys *module-deps*))])
       (for-each require-one all-modules)))

;;; ====
;;; Module Discovery (LLM-Friendly)
;;; ====

;;; extract-category : String → String
;;; Extract category from path like "core/base/foo.ss" → "BASE"
(define (extract-category path)
  (cond
   [(string-starts-with? path "core/base/") "BASE"]
   [(string-starts-with? path "core/blocks/") "BLOCKS"]
   [(string-starts-with? path "core/lang/") "LANG"]
   [(string-starts-with? path "core/types/") "TYPES"]
   [(string-starts-with? path "lattice/query/") "QUERY"]
   [(string-starts-with? path "lattice/data/") "DATA"]
   [(string-starts-with? path "lattice/linalg/") "LINALG"]
   [(string-starts-with? path "lattice/numeric/") "NUMERIC"]
   [(string-starts-with? path "lattice/autodiff/") "AUTODIFF"]
   [(string-starts-with? path "lattice/random/") "RANDOM"]
   [(string-starts-with? path "lattice/pipeline/") "PIPELINE"]
   [(string-starts-with? path "lattice/info/") "INFO-THEORY"]
   [(string-starts-with? path "core/util/") "UTIL"]
   [(string-starts-with? path "lattice/fp/") "FP"]
   [(string-starts-with? path "boundary/") "BOUNDARY"]
   [else "OTHER"]))

;;; category-description : String → String
;;; Return description for category.
(define (category-description cat)
  (cond
   [(string=? cat "BASE") "foundation, no dependencies"]
   [(string=? cat "BLOCKS") "content-addressed storage"]
   [(string=? cat "LANG") "evaluation, compilation"]
   [(string=? cat "TYPES") "type system"]
   [(string=? cat "QUERY") "pattern matching, search"]
   [(string=? cat "DATA") "data structures"]
   [(string=? cat "LINALG") "linear algebra"]
   [(string=? cat "NUMERIC") "numerical computing"]
   [(string=? cat "AUTODIFF") "automatic differentiation"]
   [(string=? cat "RANDOM") "randomness, probability"]
   [(string=? cat "PIPELINE") "agent pipelines"]
   [(string=? cat "INFO-THEORY") "information theory"]
   [(string=? cat "UTIL") "utilities"]
   [(string=? cat "FP") "functional programming toolkit"]
   [(string=? cat "BOUNDARY") "boundary, REPL, IO"]
   [else ""]))

;;; group-modules-by-category : Unit → (Hashtable String (List Symbol))
;;; Group all registered modules by their category.
(define (group-modules-by-category)
  (let ([groups (make-hashtable string-hash string=?)]
        [modules (vector->list (hashtable-keys *module-paths*))])
       (for-each
        (lambda (mod)
                (let* ([path (hashtable-ref *module-paths* mod "")]
                       [cat (extract-category path)]
                       [existing (hashtable-ref groups cat '())])
                      (hashtable-set! groups cat (cons mod existing))))
        modules)
       groups))

;;; modules : Unit → Void
;;; List all registered modules grouped by category.
;;; Dynamically builds listing from *module-paths* registry.
(define (modules)
  (display "\n")
  (display "  --------------- AVAILABLE MODULES ---------------\n")
  (display "\n")
  
  (let* ([groups (group-modules-by-category)]
         [categories '("BASE" "BLOCKS" "LANG" "TYPES" "QUERY" "DATA"
                       "LINALG" "NUMERIC" "AUTODIFF" "RANDOM" "PIPELINE"
                       "INFO-THEORY" "UTIL" "FP" "BOUNDARY" "OTHER")])
        
        (for-each
         (lambda (cat)
                 (let ([mods (hashtable-ref groups cat '())])
                      (unless (null? mods)
                              (let ([desc (category-description cat)]
                                    [sorted-mods (sort (lambda (a b)
                                                               (string<? (symbol->string a)
                                                                         (symbol->string b)))
                                                       mods)])
                                   (display (format "  ~a~a:\n"
                                                    cat
                                                    (if (string=? desc "")
                                                        ""
                                                        (string-append " (" desc ")"))))
                                   (display "    ")
                                   (display (apply string-append
                                                   (map (lambda (m)
                                                                (string-append (symbol->string m) " "))
                                                        sorted-mods)))
                                   (display "\n\n")))))
         categories)
        
        (display "  Usage: (require 'module-name) to load a module\n")
        (display "         (require 'dir/module) for namespaced (avoids collisions)\n")
        (display "         (module-info 'module-name) for details\n")
        (display "         (module-stats) for load times\n")
        (display "\n  Full lattice discovery (36 skills, 284 modules, 3000+ exports):\n")
        (display "         (load \"lattice/meta/meta.ss\") then (lattice-init!)\n")
        (display "         (lf \"query\")  search functions   (li 'skill)  skill info\n")
        (display "         (le 'skill)  list exports       (lh)  health check\n\n")))

;;; module-info : Symbol → Void
;;; Show detailed information about a module.
;;; Useful for understanding dependencies before loading.
(define (module-info name)
  (display "\n")
  (display (format "  Module: ~a\n" name))
  (display "  --------------------------------------------------------\n")

  ;; Path
  (let ([path (module-name->path name)])
       (if path
           (display (format "  Path: ~a\n" path))
           (display "  Path: (not registered)\n")))
  
  ;; Load status
  (display (format "  Status: ~a\n" (if (module-loaded? name) "LOADED" "not loaded")))
  
  ;; Load time
  (let ([time (module-load-time name)])
       (when time
             (display (format "  Load time: ~ams\n" time))))
  
  ;; Dependencies
  (let ([deps (hashtable-ref *module-deps* name 'not-found)])
       (cond
        [(eq? deps 'not-found)
         ;; Try to discover from header
         (let ([path (module-name->path name)])
              (if path
                  (let ([header (parse-module-header path)])
                       (if header
                           (begin
                            (display (format "  Dependencies: ~a (from header)\n"
                                             (if (null? (cdr header)) "(none)" (cdr header)))))
                           (display "  Dependencies: (unknown - no header annotations)\n")))
                  (display "  Dependencies: (unknown - module not found)\n")))]
        [(null? deps)
         (display "  Dependencies: (none)\n")]
        [else
         (display (format "  Dependencies: ~a\n" deps))]))
  
  ;; Transitive dependencies
  (let ([all (all-deps name)])
       (unless (null? all)
               (display (format "  Transitive: ~a\n" all))))
  
  (display "\n"))

;;; module-runtime-syms : Symbol → (List Symbol)
;;; Get all runtime symbols matching module prefix from oblist.
(define (module-runtime-syms name)
  (let* ([prefix (string-append (symbol->string name) "-")]
         [prefix-len (string-length prefix)])
    (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
          (filter (lambda (s)
                    (let ([str (symbol->string s)])
                      (and (>= (string-length str) prefix-len)
                           (string=? (substring str 0 prefix-len) prefix)
                           (top-level-bound? s))))
                  (oblist)))))

;;; module-runtime-extras : Symbol × (List Symbol) → (List Symbol)
;;; Get runtime symbols not in the source-level set.
(define (module-runtime-extras name source-names)
  (let ([runtime (module-runtime-syms name)])
    (filter (lambda (s) (not (memq s source-names))) runtime)))

;;; module-exports-runtime : Symbol → Void
;;; Show only runtime symbols for a module.
(define (module-exports-runtime name)
  (if (not (module-loaded? name))
      (display (format "  Module '~a' not loaded. Use (require '~a) first.\n" name name))
      (let ([syms (module-runtime-syms name)])
        (display (format "\n  Runtime exports of '~a' (~a symbols):\n" name (length syms)))
        (display "  --------------------------------------------------------\n")
        (for-each (lambda (s) (display (format "    ~a\n" s))) syms)
        (display "  --------------------------------------------------------\n")
        (display "\n"))))

;;; module-exports : Symbol [Symbol] → Void
;;; Show top-level definitions exported by a module.
;;; Scans source for define, define-syntax, and define-record-type forms.
;;; Optional mode argument:
;;;   'all     — include macro-generated runtime symbols
;;;   'runtime — show only runtime symbols (requires module to be loaded)
;;; Default shows source-level exports with a count of runtime symbols.
(define (module-exports name . mode-arg)
  (let ([mode (if (null? mode-arg) 'source (car mode-arg))]
        [path (module-name->path name)])
    (if (not path)
        (display (format "  Module '~a' not found.\n" name))
        (if (eq? mode 'runtime)
            ;; Runtime-only mode: just list symbols from oblist
            (module-exports-runtime name)
            (let ([port (open-input-file path)])
              (display (format "\n  Exports of '~a' (~a):\n" name path))
              (display "  --------------------------------------------------------\n")
              (let loop ([count 0] [has-macros #f] [source-names '()])
                (let ([form (guard (e [#t #!eof]) (read port))])
                  (cond
                    [(eof-object? form)
                     (close-input-port port)
                     ;; Show macro-generated symbols when mode is 'all
                     (let ([extra-count 0])
                       (when (and (eq? mode 'all) has-macros (module-loaded? name))
                         (let ([extra (module-runtime-extras name source-names)])
                           (unless (null? extra)
                             (display "  --- macro-generated ---\n")
                             (for-each (lambda (s)
                                         (display (format "    ~a\n" s)))
                                       extra)
                             (set! extra-count (length extra)))))
                       (display "  --------------------------------------------------------\n")
                       (display (format "  ~a source-level exports\n" count))
                       (when (> extra-count 0)
                         (display (format "  ~a macro-generated exports\n" extra-count))))
                     ;; Show dependencies
                     (let ([deps (hashtable-ref *module-deps* name '())])
                       (unless (null? deps)
                         (display (format "  Dependencies: ~a\n" deps))))
                     ;; Runtime symbol count hint (source mode only)
                     (when (and (eq? mode 'source) (module-loaded? name))
                       (let* ([prefix (string-append (symbol->string name) "-")]
                              [prefix-len (string-length prefix)]
                              [runtime-syms
                               (filter (lambda (s)
                                         (let ([str (symbol->string s)])
                                           (and (>= (string-length str) prefix-len)
                                                (string=? (substring str 0 prefix-len) prefix)
                                                (top-level-bound? s))))
                                       (oblist))])
                         (when (> (length runtime-syms) count)
                           (display (format "  + ~a macro-generated runtime symbols (use 'all to see all ~a* bindings)\n"
                                            (- (length runtime-syms) count) name)))))
                     (when (and (eq? mode 'source) has-macros (not (module-loaded? name)))
                       (display "  Note: load module first to see macro-generated exports\n"))
                     (display "\n")]
                ;; (define (name args ...) body)
                [(and (pair? form) (eq? (car form) 'define)
                      (pair? (cdr form)) (pair? (cadr form)))
                 (let ([fn-name (caadr form)]
                       [args (cdadr form)])
                   (display (format "    (~a ~a)\n" fn-name
                                    (let fmt ([a args])
                                      (cond
                                        [(null? a) ""]
                                        [(symbol? a) (format ". ~a" a)]
                                        [(pair? a) (string-append
                                                    (symbol->string (car a))
                                                    (if (or (null? (cdr a)) (symbol? (cdr a)))
                                                        (fmt (cdr a))
                                                        (string-append " " (fmt (cdr a)))))]
                                        [else ""])))))
                 (loop (+ count 1) has-macros (cons (caadr form) source-names))]
                ;; (define name value)
                [(and (pair? form) (eq? (car form) 'define)
                      (pair? (cdr form)) (symbol? (cadr form)))
                 (display (format "    ~a\n" (cadr form)))
                 (loop (+ count 1) has-macros (cons (cadr form) source-names))]
                ;; (define-syntax name ...)
                [(and (pair? form) (eq? (car form) 'define-syntax)
                      (pair? (cdr form)) (symbol? (cadr form)))
                 (display (format "    ~a [syntax]\n" (cadr form)))
                 (loop (+ count 1) has-macros (cons (cadr form) source-names))]
                ;; (define-record-type name ...)
                [(and (pair? form) (eq? (car form) 'define-record-type)
                      (pair? (cdr form)) (symbol? (cadr form)))
                 (display (format "    ~a [record]\n" (cadr form)))
                 (loop (+ count 1) has-macros (cons (cadr form) source-names))]
                ;; Detect macro invocations that likely generate defines
                ;; Pattern: bare (symbol args...) at top level where symbol
                ;; contains "generate" or starts with known macro prefixes
                [(and (pair? form) (symbol? (car form))
                      (let ([s (symbol->string (car form))])
                        (or (and (>= (string-length s) 8)
                                 (string=? (substring s 0 8) "generate"))
                            (and (>= (string-length s) 7)
                                 (string=? (substring s 0 7) "define-")
                                 (not (memq (car form)
                                            '(define-syntax define-record-type)))))))
                 (loop count #t source-names)]
                ;; Skip other forms
                [else (loop count has-macros source-names)]))))))))

;;; list-registered-modules : Unit → (List Symbol)
;;; Return a list of all registered module names.
(define (list-registered-modules)
  (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
        (vector->list (hashtable-keys *module-paths*))))

;;; module-collisions : Unit → Void
;;; List all module names that have multiple files (collision candidates).
;;; Useful for auditing the codebase and knowing when to use namespaced requires.
;;;
;;; Usage: (module-collisions) to see all colliding names
;;;        Use namespaced form (require 'dir/module) to avoid ambiguity
(define (module-collisions)
  (display "\n")
  (display "  --------------- MODULE COLLISIONS ---------------\n")
  (display "\n")
  (display "  Known collisions (use namespaced form to disambiguate):\n\n")

  ;; Check known collision-prone names
  (let ([collision-count 0]
        [names-to-check '("types" "state" "effects" "polynomial" "parser"
                          "distributions" "dsl" "integrators" "optimize"
                          "span" "stability" "units" "numeric-instances")])
       (for-each
        (lambda (name)
                (let ([paths (find-all-module-paths name)])
                     (when (> (length paths) 1)
                           (set! collision-count (+ collision-count 1))
                           (display (format "  ~a (~a files):~%" name (length paths)))
                           (for-each
                            (lambda (p)
                                    (display (format "    → (require '~a)~%" (path->namespace p))))
                            paths)
                           (display "\n"))))
        names-to-check)

       (if (= collision-count 0)
           (display "  No collisions found.\n\n")
           (display (format "  Total: ~a module names with collisions~%~%" collision-count)))))

;;; ====
;;; Module Reloading
;;; ====

(doc 'section 'reloading)

(doc *module-dependents* 'type (Hashtable Symbol (List Symbol)))
(doc *module-dependents* 'description "Reverse dependency index: maps module to list of modules that depend on it.")
(define *module-dependents* (make-eq-hashtable))

(doc build-dependents-index! 'type (-> Void))
(doc build-dependents-index! 'description "Rebuild the reverse dependency index from *module-deps*.")
(define (build-dependents-index!)
  (hashtable-clear! *module-dependents*)
  ;; For each module, add it to the dependents list of each of its deps
  (let ([modules (vector->list (hashtable-keys *module-deps*))])
    (for-each
     (lambda (mod)
       (let ([deps (hashtable-ref *module-deps* mod '())])
         (for-each
          (lambda (dep)
            (let ([existing (hashtable-ref *module-dependents* dep '())])
              (unless (memq mod existing)
                (hashtable-set! *module-dependents* dep (cons mod existing)))))
          deps)))
     modules)))

(doc module-dependents 'type (-> Symbol (List Symbol)))
(doc module-dependents 'description "Get direct dependents of a module (modules that depend on it).")
(define (module-dependents name)
  ;; Ensure index is built
  (when (zero? (hashtable-size *module-dependents*))
    (build-dependents-index!))
  (hashtable-ref *module-dependents* name '()))

(doc all-dependents 'type (-> Symbol (List Symbol)))
(doc all-dependents 'description "Get all transitive dependents of a module.")
(define (all-dependents name)
  (let ([direct (module-dependents name)])
    (if (null? direct)
        '()
        (unique (append direct (append-map all-dependents direct))))))

(doc topo-sort-reload 'type (-> (List Symbol) (List Symbol)))
(doc topo-sort-reload 'description "Topologically sort modules for reload: dependencies before dependents.")
(define (topo-sort-reload modules)
  ;; Kahn's algorithm: sort so deps come before dependents
  (let* ([mod-set (make-eq-hashtable)]
         [in-degree (make-eq-hashtable)]
         [graph (make-eq-hashtable)])
    ;; Initialize
    (for-each (lambda (m)
                (hashtable-set! mod-set m #t)
                (hashtable-set! in-degree m 0)
                (hashtable-set! graph m '()))
              modules)
    ;; Build graph edges within our module set
    (for-each
     (lambda (m)
       (let ([deps (hashtable-ref *module-deps* m '())])
         (for-each
          (lambda (dep)
            (when (hashtable-ref mod-set dep #f)
              ;; dep -> m edge (dep must load before m)
              (hashtable-set! graph dep (cons m (hashtable-ref graph dep '())))
              (hashtable-set! in-degree m (+ 1 (hashtable-ref in-degree m 0)))))
          deps)))
     modules)
    ;; Kahn's algorithm
    (let ([queue (filter (lambda (m) (zero? (hashtable-ref in-degree m 0))) modules)]
          [result '()])
      (let loop ([q queue] [res result])
        (if (null? q)
            (reverse res)
            (let* ([node (car q)]
                   [rest (cdr q)]
                   [neighbors (hashtable-ref graph node '())])
              (let ([new-queue
                     (fold-left
                      (lambda (acc neighbor)
                        (let ([new-deg (- (hashtable-ref in-degree neighbor 0) 1)])
                          (hashtable-set! in-degree neighbor new-deg)
                          (if (zero? new-deg)
                              (cons neighbor acc)
                              acc)))
                      rest
                      neighbors)])
                (loop new-queue (cons node res)))))))))

(doc reload! 'type (-> Symbol Void))
(doc reload! 'description "Reload a single module (re-execute its file). Preserves session state except for redefined bindings.")
(define (reload! name)
  (let ([path (module-name->path name)])
    (if path
        (let ([start (current-time-ms)])
          ;; Clear loaded status
          (hashtable-set! *module-registry* name (cons #f 0))
          ;; Re-load the file
          (load path)
          ;; Update registry with new load time
          (let ([duration (- (current-time-ms) start)])
            (hashtable-set! *module-registry* name (cons #t duration))
            (display (format "  Reloaded ~a (~ams)\n" name duration))))
        (display (format "  Error: Module ~a not found\n" name)))))

(doc reload-with-dependents! 'type (-> Symbol Void))
(doc reload-with-dependents! 'description "Reload a module and all its dependents in topological order.")
(define (reload-with-dependents! name)
  (build-dependents-index!)  ; Rebuild for accuracy
  (let* ([dependents (all-dependents name)]
         [to-reload (cons name dependents)]
         [sorted (topo-sort-reload to-reload)])
    (display (format "\n  Reloading ~a and ~a dependent(s):\n" name (length dependents)))
    (display (format "  Order: ~a\n\n" sorted))
    (for-each reload! sorted)
    (display "\n  Done.\n\n")))

;;; Convenience aliases
(doc rel! 'type (-> Symbol Void))
(doc rel! 'description "Alias for reload! (reload single module).")
(define rel! reload!)

(doc rel+! 'type (-> Symbol Void))
(doc rel+! 'description "Alias for reload-with-dependents! (reload module and dependents).")
(define rel+! reload-with-dependents!)
