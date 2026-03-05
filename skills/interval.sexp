;;; skills/interval.sexp — Skill curation file

(skill interval
  (description
    "Verified numerical computation with rigorous bounds. Interval arithmetic\n    represents sets of real numbers with guaranteed enclosure. Affine arithmetic\n    provides tighter bounds via correlation tracking, solving the dependency\n    problem where x - x yields [0, 0] instead of [-r, r]. Includes verified\n    numerical integration via adaptive interval quadrature.")

  (keywords (interval-arithmetic affine-arithmetic verified-computation bounds correlation-tracking dependency-problem noise-symbols directed-rounding rigorous-enclosure three-valued-logic interval-integration adaptive-quadrature))

  (aliases (interval affine))

  (concepts (verified-computation))

  (modules
    interval
    affine
    interval-integrate
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
