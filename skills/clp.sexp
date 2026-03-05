;;; skills/clp.sexp — Skill curation file

(skill clp
  (description
    "cKanren-style constraint logic programming with finite domains.\n    Extends miniKanren with arithmetic constraints, global constraints like\n    all-different, and intelligent search strategies. Supports classic problems\n    like N-Queens, Sudoku, cryptarithmetic, and scheduling.")

  (keywords (clp constraint-logic-programming finite-domain cKanren constraint-propagation arc-consistency n-queens sudoku cryptarithmetic scheduling combinatorial))

  (aliases (clp fd ckanren constraint-logic))

  (concepts (constraint-logic-programming))

  (modules
    domain
    store
    fd-constraints
    global-constraints
    propagate
    label
    clp
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
