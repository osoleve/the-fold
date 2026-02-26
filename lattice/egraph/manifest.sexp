;;; lattice/egraph/manifest.sexp — E-Graph Skill Manifest
;;;
;;; Equality saturation and e-graph optimization for finding
;;; equivalent program forms. Integrates with CUDA codegen to
;;; find optimal GPU-friendly expressions.

(skill egraph
  (version "0.1.0")
  (path "lattice/egraph")
  (purity partial)  ; Union-find uses mutation for efficiency
  (stability experimental)
  (fuel-bound 100000)

  (deps (algebra))  ; egraph-groebner bridge uses algebra's polynomial/Gröbner infrastructure

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

  (concepts
    (concept equality-saturation
      (description "E-graphs that represent multiple equivalent program forms simultaneously for cost-based optimal extraction.")
      (parent computation)
      (synonyms egraph e-graph)))

  (exports
   (union-find
    make-uf uf? uf-make-set! uf-find uf-union! uf-same-set?
    uf-count uf-size uf-roots uf-set-members uf-set-size uf-all-sets
    uf-debug)
   (eclass
    make-enode enode? enode-op enode-children enode-arity
    enode-canonicalize enode-equal? enode-hash enode->string
    make-eclass-store eclass-store?
    eclass-get eclass-get-or-create!
    eclass-add-node! eclass-add-parent!
    eclass-get-nodes eclass-get-parents
    eclass-merge!
    eclass-node-count eclass-all-nodes eclass-debug)
   (egraph
    make-egraph egraph?
    egraph-find egraph-lookup
    egraph-add-enode! egraph-add-term!
    egraph-merge! egraph-rebuild! egraph-saturate-rebuild!
    egraph-class-count egraph-node-count egraph-size
    egraph-class-nodes egraph-stats-report egraph-debug)
   (match
    pattern-var? empty-subst subst-lookup subst-extend
    subst-try-extend subst-merge subst-compatible?
    ematch ematch-pattern ematch-all
    pattern-apply eclass-ref? eclass-ref-id
    make-rule rule? rule-lhs rule-rhs
    apply-rule apply-rules
    subst->string pattern->string)
   (saturation
    make-saturation-config saturation-config? default-saturation-config
    config-fuel config-node-limit config-iter-limit
    saturation-result? result-status result-iterations result-rules-applied
    result-final-classes result-final-nodes result-saturated?
    saturate saturate-simple saturate-with-fuel
    arith-identity-rules arith-comm-rules arith-assoc-rules
    arith-distrib-rules basic-arith-rules full-arith-rules
    result->string saturation-debug)
   (cost
    make-cost-model cost-model? cost-model-name
    compute-costs class-cost compute-node-cost
    ast-size-cost ast-depth-cost leaf-count-cost
    make-weighted-cost cuda-cost cpu-cost code-size-cost
    combine-costs analyze-costs
    cost-model->string costs-debug)
   (extract
    make-extraction-state extraction-state?
    extract extract-term extract-all
    optimize optimize-with-config
    extraction-report compare-extractions
    extraction-debug optimize-debug)
   (scheduler
    make-rule-stats rule-stats? stats-rule stats-priority
    update-stats! should-run?
    make-scheduler scheduler? scheduler-name
    simple-scheduler make-backoff-scheduler
    make-worklist-scheduler make-priority-scheduler
    saturate-scheduled scheduled-iteration
    optimize-scheduled
    scheduler->string rule-stats->string scheduler-stats-report)
   (egraph-groebner
    eterm->mpoly mpoly->eterm eterm-variables
    poly-equiv? poly-equiv-over? poly-equiv-modulo?
    groebner-rewrite-rules groebner-rewrite-rules-over
    poly-identity-rules poly-degree-cost
    poly-optimize poly-optimize-over
    poly-reduce poly-reduce-over poly-reduce-modulo poly-reduce-modulo-over))

  (modules
   (union-find "union-find.ss" "Disjoint set with path compression and union by rank")
   (eclass "eclass.ss" "E-node and e-class representation for e-graphs")
   (egraph "egraph.ss" "E-graph with hashconsing, merging, and rebuild")
   (match "match.ss" "Pattern matching and rewrite rules for e-graphs")
   (saturation "saturation.ss" "Equality saturation loop with resource limits")
   (cost "cost.ss" "Cost models for CUDA, CPU, and code size optimization")
   (extract "extract.ss" "Cost-based extraction from e-graphs")
   (scheduler "scheduler.ss" "Rule scheduling with backoff and priority strategies")
   (egraph-groebner "egraph-groebner.ss" "Bridge e-graph rewriting to Gröbner basis polynomial identity proving")))
