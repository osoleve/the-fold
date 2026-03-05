;;; skills/parallel.sexp — Skill curation file

(skill parallel
  (description
    "Evaluation strategies and parallel algorithm skeletons.\n    Strategies describe task decomposition as pure S-expression plans.\n    Patterns provide higher-level skeletons: divide-and-conquer,\n    parallel reduction, prefix scan, pipeline, and merge sort.")

  (keywords (parallel strategy plan thunk divide-and-conquer reduce scan pipeline sort chunking speculation race fuel-budget combinator))

  (aliases (par strategies))

  (concepts (parallel-computation))

  (modules
    strategies
    patterns
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
