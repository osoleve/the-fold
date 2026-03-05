;;; skills/sat.sexp — Skill curation file

(skill sat
  (description
    "Boolean Satisfiability solver with CDCL (Conflict-Driven Clause Learning).\n    Implements DPLL algorithm with Two-Watched Literals (2WL) for efficient unit propagation,\n    conflict analysis, clause learning, non-chronological backtracking, and VSIDS branching\n    heuristic. Includes MaxSAT extension for optimization (minimize unsatisfied soft clauses).\n    CNF builders for common patterns: cardinality constraints, graph coloring, N-Queens,\n    minimum vertex cover, maximum independent set.")

  (keywords (sat satisfiability cnf dpll cdcl clause-learning boolean constraint np-complete maxsat optimization vertex-cover independent-set diagnosis))

  (aliases (sat boolean-sat cdcl maxsat))

  (concepts (satisfiability sat-solving maxsat))

  (modules
    sat/literal
    sat/clause
    sat/cnf
    sat/assignment
    sat/watches
    sat/solver
    sat
    maxsat
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
