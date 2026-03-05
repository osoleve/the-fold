;;; skills/dataset.sexp — Skill curation file

(skill dataset
  (description
    "Generic dataset SDK for visual reasoning problems. Provides reusable framework for presentation, sampling, distractor generation, and export.")

  (keywords (dataset ml benchmark visual-reasoning sampling export))

  (aliases (sample-gen))

  (concepts (dataset-engineering))

  (modules
    sample
    parameter
    presentation
    distractor
    difficulty
    sexp
    jsonl
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
