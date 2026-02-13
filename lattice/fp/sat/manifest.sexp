(skill sat
  (version "0.3.1")
  (tier 1)
  (path "lattice/fp/sat")
  (purity partial)
  (stability experimental)
  (fuel-bound "O(2^n) worst case, typically much better with CDCL")
  (deps (fp))
  (description "Boolean Satisfiability solver with CDCL (Conflict-Driven Clause Learning).
    Implements DPLL algorithm with Two-Watched Literals (2WL) for efficient unit propagation,
    conflict analysis, clause learning, non-chronological backtracking, and VSIDS branching
    heuristic. Includes MaxSAT extension for optimization (minimize unsatisfied soft clauses).
    CNF builders for common patterns: cardinality constraints, graph coloring, N-Queens,
    minimum vertex cover, maximum independent set.")
  (keywords (sat satisfiability cnf dpll cdcl clause-learning boolean constraint np-complete
             maxsat optimization vertex-cover independent-set diagnosis))
  (aliases (sat boolean-sat cdcl maxsat))
  (exports
   (sat sat-solve sat-satisfiable? sat-model sat-help)
   (sat var neg implies iff)
   (sat at-most-one at-least-one exactly-one at-most-k at-least-k exactly-k sequential-at-most-k)
   (sat graph-coloring n-queens-clauses n-queens-sat n-queens-solve graph-coloring-solve)
   (cnf make-cnf cnf-from-lists cnf-empty cnf-clauses cnf-vars cnf->string cnf->dimacs)
   (clause make-clause clause-from-list clause-empty? clause-unit? clause->string)
   (literal pos-lit neg-lit lit-var lit-positive? lit-negative? lit-negate lit->string)
   (solver solve solve-with-model make-solver-state)
   (maxsat make-maxsat maxsat-solve maxsat-solve-linear maxsat-solve-binary)
   (maxsat partial-maxsat maxsat-add-hard maxsat-add-soft)
   (maxsat maxsat-result-cost maxsat-result-model)
   (maxsat min-vertex-cover max-independent-set min-correction-set maxsat-help))
  (modules
   (literal "literal.ss" "SAT literal representation")
   (clause "clause.ss" "Disjunctive clause representation")
   (cnf "cnf.ss" "CNF formula representation and DIMACS I/O")
   (assignment "assignment.ss" "Partial assignment with decision levels")
   (watches "watches.ss" "Two-Watched Literals (2WL) for efficient unit propagation")
   (solver "solver.ss" "CDCL solver with clause learning and 2WL")
   (sat "sat.ss" "High-level API and common encodings")
   (maxsat "maxsat.ss" "MaxSAT optimization - minimize unsatisfied soft clauses")))
