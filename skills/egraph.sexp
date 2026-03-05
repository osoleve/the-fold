;;; skills/egraph.sexp — Skill curation file

(skill egraph
  (description
    "E-graph (equality graph) implementation for equality saturation.\n    Represents multiple equivalent program forms simultaneously,\n    enabling optimal extraction based on cost models.\n\n    Key components:\n    - Union-find for e-class equivalence tracking\n    - E-nodes and e-classes for term representation\n    - Equality saturation loop with fuel bounding\n    - Cost-based extraction for optimization\n\n    Primary use case: CUDA codegen optimization - find algebraically\n    equivalent forms that minimize memory bandwidth and kernel launches.")

  (keywords (egraph equality-saturation optimization rewriting term-rewriting cuda-optimization))

  (aliases (egraph e-graph))

  (concepts (equality-saturation))

  (modules
    union-find
    eclass
    egraph
    match
    saturation
    cost
    extract
    scheduler
    egraph-groebner
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
