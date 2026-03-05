;;; skills/symbolic.sexp — Skill curation file

(skill symbolic
  (description
    "Symbolic computation and computer algebra. Provides symbolic expression\nrepresentation, differentiation, algebraic simplification, integration,\nequation solving, and polynomial canonical form conversion. The egraph-simplify\nmodule bridges to the e-graph subsystem for equality-saturation-based optimization.")

  (keywords (symbolic-computation computer-algebra differentiation integration simplification equation-solving polynomial))

  (aliases (symbolic cas))

  (concepts (symbolic-computation))

  (modules
    expr
    diff
    simplify
    integrate
    solve
    poly-canonical
    egraph-simplify
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
