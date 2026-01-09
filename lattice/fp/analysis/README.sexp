(directory core/fp/analysis
  (purpose "Static program analysis tools for functional programs")
  (authority shepherd)
  (modules
    ((name "fusion-detect")
     (file "fusion-detect.ss")
     (description "Static fusion pattern detection - finds opportunities to fuse
                   consecutive map/filter/fold operations into single traversals")
     (exports
       (detect-fusion-static "Expr -> (List FusionOpportunity)")
       (fusion-opportunity? "Any -> Boolean")
       (fusion-type "FusionOpportunity -> Symbol")
       (fusion-location "FusionOpportunity -> Position")
       (fusion-before "FusionOpportunity -> Expr")
       (fusion-after "FusionOpportunity -> Expr")
       (fusion-savings "FusionOpportunity -> Nat")
       (fusion-confidence "FusionOpportunity -> Symbol")
       (fusion-summary "List -> Summary")
       (safe-opportunities "(List FusionOpportunity) -> (List FusionOpportunity)")
       (opportunities-by-type "(List FusionOpportunity) -> Grouped")
       (display-opportunities "(List FusionOpportunity) -> Void")
       (format-opportunity "FusionOpportunity -> String"))
     (fusion-rules
       ((map-map-fuse "(map f (map g xs))" "(map (compose f g) xs)")
        (filter-map-fuse "(map f (filter p xs))" "(filter-map p f xs)")
        (map-filter-fuse "(filter p (map f xs))" "(filter-map (compose p f) f xs)")
        (fold-map-fuse "(foldl f z (map g xs))" "(foldl (lambda (acc x) (f acc (g x))) z xs)")
        (concat-map-fuse "(flatten (map f xs))" "(flatMap f xs)")
        (stream-map-map-fuse "(stream-map f (stream-map g s))" "(stream-map (compose f g) s)")))
     (tests "test-fusion-detect.ss"))

    ((name "cost-analysis")
     (file "cost-analysis.ss")
     (description "Cost estimation for parallelization heuristics - determines when
                   parallel execution is beneficial by comparing computation cost
                   against parallelization overhead")
     (exports
       (estimate-work "Expr -> Nat - estimate computational cost of expression")
       (parallel-beneficial? "Expr x Expr -> Bool - should these be parallelized?")
       (parallel-beneficial-n? "(List Expr) -> Bool - should list be parallelized?")
       (min-parallel-threshold "-> Nat - minimum work for parallelization")
       (set-parallel-threshold! "Nat -> Void - set minimum work threshold")
       (get-parallel-overhead "-> Nat - get parallel overhead estimate")
       (set-parallel-overhead! "Nat -> Void - set parallel overhead")
       (operation-cost "Symbol -> Nat - cost of primitive operation")
       (expr-complexity-class "Expr -> Symbol - classify complexity class")
       (work-analysis "Expr -> WorkAnalysis - detailed analysis report")
       (work-analysis? "Any -> Boolean")
       (analysis-total-cost "WorkAnalysis -> Nat")
       (analysis-complexity-class "WorkAnalysis -> Symbol")
       (analysis-hotspots "WorkAnalysis -> (List Hotspot)")
       (analysis-parallelizable? "WorkAnalysis -> Boolean")
       (analysis-suggested-splits "WorkAnalysis -> Nat")
       (suggested-parallelism "(List Expr) -> Nat - optimal parallel task count")
       (total-work "(List Expr) -> Nat - sum of work estimates")
       (work-ratio "Expr x Expr -> Real - ratio of work between expressions")
       (balanced-split? "(List Expr) -> Bool - is work evenly distributed?")
       (format-work-analysis "WorkAnalysis -> String")
       (display-work-analysis "WorkAnalysis -> Void"))
     (complexity-classes
       (constant "O(1) - atoms, arithmetic")
       (linear "O(n) - single HOF traversal")
       (quadratic "O(n^2) - nested HOF traversals")
       (recursive "Unknown - contains recursion")
       (unknown "Cannot determine"))
     (tests "test-cost-analysis.ss"))

    ((name "parallel-detect")
     (file "parallel-detect.ss")
     (description "Parallelization opportunity detection - finds code patterns
                   where independent computations can be executed in parallel")
     (exports
       (detect-parallel-regions "Expr -> (List ParallelOpportunity)")
       (parallel-opportunity? "Any -> Boolean")
       (par-type "ParallelOpportunity -> Symbol")
       (par-branches "ParallelOpportunity -> (List Expr)")
       (par-location "ParallelOpportunity -> Position")
       (par-estimated-speedup "ParallelOpportunity -> Rational")
       (par-dependencies "ParallelOpportunity -> DependencyGraph")
       (parallel-summary "(List ParallelOpportunity) -> Summary")
       (free-vars "Expr -> (List Symbol)")
       (make-dep-graph "(List (Pair Symbol (List Symbol))) -> DependencyGraph")
       (dep-graph-lookup "DependencyGraph x Symbol -> (List Symbol)")
       (high-value-opportunities "(List ParallelOpportunity) -> (List ParallelOpportunity)")
       (opportunities-by-pattern "(List ParallelOpportunity) -> Grouped")
       (suggest-par-transform "ParallelOpportunity -> Expr")
       (format-parallel-opportunity "ParallelOpportunity -> String")
       (display-parallel-opportunities "(List ParallelOpportunity) -> Void"))
     (parallel-patterns
       ((independent-let "let bindings with no inter-dependencies"
                         "(let ([a (f x)] [b (g y)]) ...)")
        (independent-map "multiple maps on different data"
                         "(begin (map f xs) (map g ys))")
        (par-eligible "function calls with independent argument computations"
                      "(func (expensive-a) (expensive-b))")
        (fanout "single input feeding multiple independent computations"
                "(let ([x input]) (list (f x) (g x) (h x)))")))
     (tests "test-parallel-detect.ss")))

  (dependencies
    (("prelude" "core/base/prelude.ss")))

  (notes "This directory contains static program analysis tools for functional programs.
          All modules are pure Core code - no IO, no side effects, assumes perfect input.

          Analysis tools:
          - fusion-detect: Finds opportunities to fuse consecutive traversals
          - cost-analysis: Estimates computation cost for parallelization decisions
          - parallel-detect: Finds code patterns amenable to parallel execution

          The parallel-detect module detects:
          - Independent let bindings that can be evaluated in parallel
          - Independent map operations on different data
          - Function calls with independent argument computations (par-eligible)
          - Fanout patterns where one input feeds multiple independent computations

          Speedup estimation uses Amdahl's law approximation with overhead adjustment.
          High-value opportunities are those with estimated speedup >= 1.5x.

          The cost analysis module uses weighted costs for different operations:
          - Primitive ops: 1-5 work units (trivial to cheap)
          - Complex ops: 10-25 work units (sqrt, transcendentals)
          - HOF base cost: 15-25 work units plus iteration costs
          - Default parallel threshold: 200 work units
          - Default parallel overhead: 100 work units"))
