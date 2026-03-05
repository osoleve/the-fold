;;; skills/physics-problems.sexp — Skill curation file

(skill physics-problems
  (description
    "Physics word problem generation using the Dataset SDK and classical physics engine. Provides declarative templates for spatial, kinematic, and dynamic problems.")

  (keywords (physics problems dataset ml benchmark visual-reasoning))

  (concepts (physics-problem-generation))

  (modules
    physics-problem
    simulation
    spatial
    kinematic
    dynamic
    counterfactual
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
