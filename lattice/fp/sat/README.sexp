(readme sat
  (title "SAT Solver - Boolean Satisfiability with CDCL")

  (overview
   "A complete SAT solver implementing the CDCL (Conflict-Driven Clause Learning)
    algorithm. Solves Boolean satisfiability problems in CNF (Conjunctive Normal Form).
    Includes builders for common constraint patterns and problem encodings.")

  (architecture
   (literal "Signed integers: positive = true, negative = negated")
   (clause "Disjunction of literals (OR). Empty clause = FALSE.")
   (cnf "Conjunction of clauses (AND). Formula to be satisfied.")
   (assignment "Partial truth assignment with decision levels and antecedent tracking")
   (solver "CDCL loop: propagate -> decide -> conflict-analyze -> backtrack"))

  (algorithm
   (unit-propagation "BCP: if clause has one unassigned literal, force it true")
   (conflict-analysis "1-UIP: resolve conflict clause until single literal at current level")
   (clause-learning "Add learned clause to prune future search")
   (backtracking "Non-chronological: jump to second-highest level in learned clause")
   (branching "VSIDS: score variables by conflict participation, decay over time"))

  (usage
   (basic
    "(load \"lattice/fp/sat/sat.ss\")"
    "(sat-solve '((1 2) (-1 3)))        ; => 'sat or 'unsat"
    "(sat-satisfiable? '((1) (-1)))     ; => #f"
    "(sat-model '((1 2) (-1 2)))        ; => ((1 . #f) (2 . #t))")

   (building-clauses
    "(var 1)                            ; x1 (positive literal)"
    "(neg 1)                            ; ~x1 (negative literal)"
    "(implies (var 1) (var 2))          ; x1 => x2 as clause"
    "(iff (var 1) (var 2))              ; x1 <=> x2 as clause pair")

   (cardinality
    "(at-most-one '(1 2 3))             ; At most one of x1,x2,x3"
    "(exactly-one '(1 2 3))             ; Exactly one"
    "(at-most-k '(1 2 3 4) 2)           ; At most 2 of 4 vars"))

  (examples
   (n-queens
    ";; Solve 8-Queens (convenience)"
    "(n-queens-solve 8)                    ; => satisfying assignment or #f"
    ";; Or two-step: generate clauses then solve"
    "(sat-satisfiable? (n-queens-clauses 8))  ; => #t"
    "(sat-satisfiable? (n-queens-clauses 3))  ; => #f (no solution)")

   (graph-coloring
    ";; 3-color a triangle (needs 3 colors)"
    "(sat-satisfiable? (graph-coloring '((0 . 1) (1 . 2) (0 . 2)) 3 2))  ; => #f"
    "(sat-satisfiable? (graph-coloring '((0 . 1) (1 . 2) (0 . 2)) 3 3))  ; => #t")

   (custom-problem
    ";; Encode: (x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2 OR NOT x3)"
    "(sat-model '((1 2) (-1 3) (-2 -3)))"
    ";; => satisfying assignment"))

  (complexity
   (worst-case "O(2^n) - NP-complete problem")
   (typical "Much better with clause learning pruning the search space")
   (space "O(n + m) where n=variables, m=clauses (plus learned clauses)"))

  (limitations
   (fuel "Solver has fuel limit (10000 iterations) to prevent infinite loops")
   (incremental "No incremental solving support yet")
   (preprocessing "No preprocessing (variable elimination, subsumption, etc.)"))

  (see-also
   (clp "CLP(FD) for finite domain constraint satisfaction")
   (optimization "For optimization variants like MAX-SAT")))
