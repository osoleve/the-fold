;;; lattice/parallel/manifest.sexp — Parallel Evaluation Strategies Manifest

(skill parallel
  (version "0.1.0")
  (path "lattice/parallel")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n) for flat strategies, O(n log n) for tree-based patterns")
  (deps (fp))

  (description
   "Evaluation strategies and parallel algorithm skeletons.
    Strategies describe task decomposition as pure S-expression plans.
    Patterns provide higher-level skeletons: divide-and-conquer,
    parallel reduction, prefix scan, pipeline, and merge sort.")

  (keywords (parallel strategy plan thunk divide-and-conquer reduce scan
             pipeline sort chunking speculation race fuel-budget combinator))
  (aliases (par strategies))

  (concepts
    (concept parallel-computation
      (description "Divide-and-conquer parallelism, parallel reduction, and scan/prefix operations.")
      (parent computation)
      (synonyms parallel par strategies divide-and-conquer parallel-reduction scan-prefix)))

  (exports
   ;; strategies.ss
   (strategies
     make-thunk thunk? thunk-id thunk-work
     plan:seq plan:par plan:chunk plan:tree plan:race plan:pipe
     plan:seq? plan:par? plan:chunk? plan:tree? plan:race? plan:pipe?
     plan-thunks plan-chunk-size plan-combiner plan-sub-plans plan-stages
     plan? plan-type collect-plan-thunks
     make-strategy strategy? strategy-name strategy-transform
     strategy/seq strategy/par strategy/par-chunk strategy/par-buffer
     strategy/speculate strategy/par-list
     strategy-compose strategy-when-beneficial using
     fuel-budget-full fuel-budget-proportional)
   ;; patterns.ss
   (patterns
     par-divide-and-conquer par-reduce par-scan par-pipeline par-sort
     plan-thunks-deep))

  (modules
   (strategies "strategies.ss" "Strategy types, core strategies, and combinators for parallel plan generation")
   (patterns "patterns.ss" "Parallel algorithm skeletons: D&C, reduce, scan, pipeline, sort")))
