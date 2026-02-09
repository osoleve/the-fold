;;; lattice/fp/clp/README.sexp — CLP(FD) Documentation
;;;
;;; Constraint Logic Programming over Finite Domains

((name . "CLP(FD)")
 (tagline . "cKanren-style constraint logic programming")
 (status . experimental)

 (overview . "
CLP(FD) extends the miniKanren logic programming system with finite domain
constraints. Variables can be constrained to integer ranges, and arithmetic
and global constraints propagate to narrow domains automatically.

Key features:
- Finite domains with interval representation
- Arithmetic constraints (=, <, <=, >, >=, ≠, +, -, *)
- Global constraints (all-different, sum, element)
- Arc consistency propagation
- Intelligent labeling (first-fail, most-constrained)
- Goal-based API compatible with miniKanren
")

 (quick-start . "
;; Load the CLP system
(load \"lattice/fp/clp/clp.ss\")

;; Create variables and constrain them
(let* ([cs (make-cstore)]
       [x (make-lvar 'x)]
       [y (make-lvar 'y)])
  ;; Set domains
  (let* ([cs1 ((in-range x 1 10) cs)]
         [cs2 ((in-range y 1 10) cs1)]
         ;; Add constraints
         [cs3 ((<fd x y) cs2)]
         [cs4 ((all-different (list x y)) cs3)])
    ;; Find solutions
    (label-first cs4 (list x y))))
")

 (modules . (
   ((name . "domain.ss")
    (purpose . "Finite domain representation using intervals")
    (key-functions . (make-domain domain-singleton domain-from-list
                      domain-intersect domain-union domain-subtract-value)))

   ((name . "store.ss")
    (purpose . "Constraint store with domains and propagators")
    (key-functions . (make-cstore cstore-get-domain cstore-set-domain
                      cstore-narrow-domain cstore-unify)))

   ((name . "fd-constraints.ss")
    (purpose . "Arithmetic FD constraints")
    (key-functions . (in-range in-domain =fd <fd <=fd =/=fd +fd *fd)))

   ((name . "global-constraints.ss")
    (purpose . "Global constraints over multiple variables")
    (key-functions . (all-different element sum-fd count-fd)))

   ((name . "propagate.ss")
    (purpose . "Arc consistency propagation engine")
    (key-functions . (propagate post-constraint)))

   ((name . "label.ss")
    (purpose . "Variable and value selection strategies")
    (key-functions . (label label-with first-fail most-constrained
                      min-value max-value)))

   ((name . "clp.ss")
    (purpose . "Unified goal API and examples")
    (key-functions . (clp-conj clp-disj goal-in-range goal-label
                      clp-solve n-queens send-more-money)))))

 (usage-patterns . (
   ((pattern . "Basic constraint solving")
    (example . "
(let* ([cs (make-cstore)]
       [x (make-lvar 'x)]
       [cs1 ((in-range x 1 10) cs)]
       [cs2 ((=fd x 5) cs1)])
  (cstore-get-value cs2 x))  ; => 5
"))

   ((pattern . "Finding all solutions")
    (example . "
(let* ([cs (make-cstore)]
       [x (make-lvar 'x)]
       [y (make-lvar 'y)]
       [cs1 ((in-range x 1 3) cs)]
       [cs2 ((in-range y 1 3) cs1)]
       [cs3 ((<fd x y) cs2)])
  (label-all cs3 (list x y)))  ; All (x,y) pairs where x < y
"))

   ((pattern . "Goal-based programming")
    (example . "
(let ([goal (clp-conj*
              (list (goal-in-range x 1 10)
                    (goal-in-range y 1 10)
                    (goal-<fd x y)
                    (goal-label (list x y))))])
  (clp-solve (list x y) goal))
"))))

 (constraints-reference . (
   ((constraint . "in-range")
    (signature . "(LVar × Int × Int) → Propagator")
    (description . "Constrain variable to integer range [lo, hi]"))

   ((constraint . "=fd")
    (signature . "(Term × Term) → Propagator")
    (description . "Equality constraint on FD variables"))

   ((constraint . "<fd")
    (signature . "(Term × Term) → Propagator")
    (description . "Strict less-than constraint"))

   ((constraint . "<=fd")
    (signature . "(Term × Term) → Propagator")
    (description . "Less-than-or-equal constraint"))

   ((constraint . "=/=fd")
    (signature . "(Term × Term) → Propagator")
    (description . "Disequality constraint"))

   ((constraint . "+fd")
    (signature . "(Term × Term × Term) → Propagator")
    (description . "Addition: x + y = z"))

   ((constraint . "*fd")
    (signature . "(Term × Term × Term) → Propagator")
    (description . "Multiplication: x * y = z"))

   ((constraint . "all-different")
    (signature . "((List LVar)) → Propagator")
    (description . "All variables must have distinct values"))

   ((constraint . "sum-fd")
    (signature . "((List LVar) × Term) → Propagator")
    (description . "Sum of variables equals result"))))

 (labeling-strategies . (
   ((strategy . "first-fail")
    (description . "Select variable with smallest domain (fail-first heuristic)"))

   ((strategy . "most-constrained")
    (description . "Select by smallest domain/constraint-degree ratio"))

   ((strategy . "input-order")
    (description . "Select first unbound variable"))

   ((strategy . "min-value")
    (description . "Try smallest value in domain first"))

   ((strategy . "max-value")
    (description . "Try largest value in domain first"))

   ((strategy . "mid-value")
    (description . "Try middle value in domain first"))))

 (testing . "
;; Run all CLP tests
scheme --script lattice/fp/clp/test-domain.ss
scheme --script lattice/fp/clp/test-store.ss
scheme --script lattice/fp/clp/test-clp.ss
")

 (dependencies . (
   "lattice/fp/meta/logic.ss"
   "lattice/fp/data/stream.ss"
   "lattice/fp/meta/combinators.ss"))

 (see-also . (
   "lattice/fp/meta/logic.ss"
   "lattice/optimization/manifest.sexp"
   "lattice/data/graph/graph-algorithms.ss")))
