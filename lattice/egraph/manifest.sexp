;;; lattice/egraph/manifest.sexp — E-Graph Skill Manifest
;;;
;;; Equality saturation and e-graph optimization for finding
;;; equivalent program forms. Integrates with CUDA codegen to
;;; find optimal GPU-friendly expressions.

(skill egraph
  (version "0.1.0")
  (tier 1)
  (path "lattice/egraph")
  (purity partial)  ; Union-find uses mutation for efficiency
  (stability experimental)
  (fuel-bound 100000)

  (deps ())  ; Self-contained foundation; will add fp dependency for rewrite rules later

  (description
   "E-graph (equality graph) implementation for equality saturation.
    Represents multiple equivalent program forms simultaneously,
    enabling optimal extraction based on cost models.

    Key components:
    - Union-find for e-class equivalence tracking
    - E-nodes and e-classes for term representation
    - Equality saturation loop with fuel bounding
    - Cost-based extraction for optimization

    Primary use case: CUDA codegen optimization - find algebraically
    equivalent forms that minimize memory bandwidth and kernel launches.")

  (keywords (egraph equality-saturation optimization rewriting
             term-rewriting cuda-optimization))

  (aliases (egraph e-graph))

  (exports
   (union-find
    make-uf uf? uf-make-set! uf-find uf-union! uf-same-set?
    uf-count uf-size uf-roots uf-set-members uf-set-size uf-all-sets
    uf-debug))

  (modules
   (("union-find.ss" "Disjoint set with path compression and union by rank")))

  (future-work
   "eclass.ss - E-class representation"
   "egraph.ss - E-graph with hashcons and rebuilding"
   "match.ss - Pattern matching on e-classes"
   "scheduler.ss - Rule scheduling with backoff"
   "saturation.ss - Equality saturation loop"
   "cost.ss - CUDA cost model"
   "extract.ss - Cost-based extraction"))
