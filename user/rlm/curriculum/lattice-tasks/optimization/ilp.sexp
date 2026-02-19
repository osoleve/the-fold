;;; ilp.sexp — Development tasks for lattice/optimization/ilp.ss
;;;
;;; Module: ilp (tier 0, depends on lp)
;;; 728 lines, ~20 exported functions
;;; Integer Linear Programming with branch-and-bound and Gomory cutting planes.
;;; Extends LP with integer constraints: mixed integer, pure integer, and binary
;;; programs. Applications: knapsack, set cover.
;;;
;;; Git history:
;;;   852c2c60 fix(ilp): Detect infeasible set cover via Big-M threshold check
;;;   97556a64 feat(optimization): Implement integer linear programming (fold-zxkq)

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement is-integer?
  ;; ──────────────────────────────────────────────
  ;; Tolerance-based integer check.

  (task
    (id "optimization/ilp/is-integer-impl")
    (module ilp)
    (tier 0)
    (type implement)
    (description "Implement is-integer?: check if a number is approximately an integer, within a tolerance of 1e-6. Compute the difference between x and (round x), take the absolute value, and check if it's less than the tolerance. This is the foundational predicate for the ILP solver — branch-and-bound needs to know when a solution satisfies integrality constraints. Floating-point LP solutions rarely land exactly on integers, so the tolerance allows for numerical imprecision.")
    (context "Available: abs, -, round, <. Single arithmetic expression.")
    (before "(define (is-integer? x)
  ;; TODO: is x approximately integer?
  #f)")
    (test-file "lattice/optimization/test-ilp.ss")
    (test-forms (";; Exact integers
(assert-true (is-integer? 5))
(assert-true (is-integer? 0))
(assert-true (is-integer? -3))"
                 ";; Near-integers (within tolerance)
(assert-true (is-integer? 4.9999999))
(assert-true (is-integer? 5.0000001))"
                 ";; Non-integers
(assert-false (is-integer? 5.5))
(assert-false (is-integer? 0.3))
(assert-false (is-integer? 2.999))"))
    (available-modules (lp))
    (hints ("Round to nearest integer: (round x)"
            "Distance: (abs (- x (round x)))"
            "Compare with tolerance: (< distance 1e-6)"))
    (reference-solution "(define (is-integer? x)
  (< (abs (- x (round x))) 1e-6))")
    (alternatives ())
    (source-commit "97556a64")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement solution-is-integer?
  ;; ──────────────────────────────────────────────
  ;; Check if solution satisfies integer constraints.

  (task
    (id "optimization/ilp/solution-is-integer-impl")
    (module ilp)
    (tier 0)
    (type implement)
    (description "Implement solution-is-integer?: check if a solution vector satisfies all integer variable constraints. Takes a vector x and a list of variable indices (integer-vars) that must be integer. Use a named let loop over the list: base case (null) returns #t, otherwise check if x[car vars] is integer via is-integer? — if yes, continue to cdr; if no, return #f immediately. This short-circuit design avoids checking all variables when the first fractional one is found.")
    (context "Available: null?, car, cdr, vector-ref, is-integer?, or, and, let loop. Named let with short-circuit.")
    (before "(define (solution-is-integer? x integer-vars)
  ;; TODO: are all integer-constrained variables integral?
  #f)")
    (test-file "lattice/optimization/test-ilp.ss")
    (test-forms (";; All checked variables are integer
(assert-true (solution-is-integer? '#(1 2 3.5 4) '(0 1 3)))"
                 ";; One checked variable is fractional
(assert-false (solution-is-integer? '#(1 2 3.5 4) '(0 2)))"
                 ";; Empty constraints: trivially true
(assert-true (solution-is-integer? '#(1.5 2.5) '()))"
                 ";; All integer
(assert-true (solution-is-integer? '#(1 2 3) '(0 1 2)))"))
    (available-modules (lp))
    (hints ("Named let: (let loop ([vars integer-vars]) ...)"
            "Base: (null? vars) -> #t (all checked)"
            "Check: (is-integer? (vector-ref x (car vars)))"
            "Short-circuit with or/and: (or (null? vars) (and (is-integer? ...) (loop (cdr vars))))"))
    (reference-solution "(define (solution-is-integer? x integer-vars)
  (let loop ([vars integer-vars])
    (or (null? vars)
        (and (is-integer? (vector-ref x (car vars)))
             (loop (cdr vars))))))")
    (alternatives ())
    (source-commit "97556a64")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement find-fractional-var
  ;; ──────────────────────────────────────────────
  ;; Find the branching variable for B&B.

  (task
    (id "optimization/ilp/find-fractional-var-impl")
    (module ilp)
    (tier 0)
    (type implement)
    (description "Implement find-fractional-var: find the first integer-constrained variable with a fractional value. Returns the variable index, or #f if all integer variables are integral. This is the branching variable selection step in branch-and-bound: the algorithm branches on x_j <= floor(val) and x_j >= ceil(val) for the returned variable j. Use a cond-based loop: null -> #f, not integer -> return index, else recurse.")
    (context "Available: null?, car, cdr, vector-ref, is-integer?, not, cond, let loop. Named let with three-way cond.")
    (before "(define (find-fractional-var x integer-vars)
  ;; TODO: first fractional integer variable, or #f
  #f)")
    (test-file "lattice/optimization/test-ilp.ss")
    (test-forms (";; First fractional is index 1
(assert-equal 1 (find-fractional-var '#(1 2.5 3 4.7) '(0 1 2 3)))"
                 ";; Skip integral variables, find index 3
(assert-equal 3 (find-fractional-var '#(1 2.5 3 4.7) '(0 2 3)))"
                 ";; All integer: return #f
(assert-equal #f (find-fractional-var '#(1 2.5 3 4.7) '(0 2)))"))
    (available-modules (lp))
    (hints ("Named let: (let loop ([vars integer-vars]) ...)"
            "Base: (null? vars) -> #f"
            "Check: (not (is-integer? (vector-ref x (car vars)))) -> (car vars)"
            "else -> (loop (cdr vars))"))
    (reference-solution "(define (find-fractional-var x integer-vars)
  (let loop ([vars integer-vars])
    (cond
      [(null? vars) #f]
      [(not (is-integer? (vector-ref x (car vars)))) (car vars)]
      [else (loop (cdr vars))])))")
    (alternatives ())
    (source-commit "97556a64")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement frac
  ;; ──────────────────────────────────────────────
  ;; Fractional part for Gomory cuts.

  (task
    (id "optimization/ilp/frac-impl")
    (module ilp)
    (tier 0)
    (type implement)
    (description "Implement frac: compute the fractional part of a number, defined as x - floor(x). This always returns a value in [0, 1). The fractional part is the core building block of Gomory cutting planes: given a basic row x_i + sum(a_ij * x_j) = b_i where b_i is fractional, the Gomory cut uses frac(a_ij) and frac(b_i) to derive a valid inequality that cuts off the fractional LP solution while preserving all integer-feasible points.")
    (context "Available: -, floor. Single arithmetic expression.")
    (before "(define (frac x)
  ;; TODO: fractional part x - floor(x)
  0)")
    (test-file "lattice/optimization/test-ilp.ss")
    (test-forms (";; Standard fractional part
(assert-true (< (abs (- (frac 3.7) 0.7)) 1e-10))"
                 ";; Integer has zero fractional part
(assert-true (< (abs (frac 5.0)) 1e-10))"
                 ";; Negative number: frac(-0.3) = -0.3 - floor(-0.3) = -0.3 - (-1) = 0.7
(assert-true (< (abs (- (frac -0.3) 0.7)) 1e-10))"
                 ";; Zero
(assert-true (< (abs (frac 0)) 1e-10))"))
    (available-modules (lp))
    (hints ("floor(x) gives the largest integer <= x"
            "frac(x) = x - floor(x)"
            "This is always in [0, 1)"
            "For negative: floor(-0.3) = -1, so frac(-0.3) = 0.7"))
    (reference-solution "(define (frac x)
  (- x (floor x)))")
    (alternatives ())
    (source-commit "97556a64")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement vector-take
  ;; ──────────────────────────────────────────────
  ;; Extract first n elements of a vector.

  (task
    (id "optimization/ilp/vector-take-impl")
    (module ilp)
    (tier 0)
    (type implement)
    (description "Implement vector-take: extract the first n elements of a vector as a new vector. Allocate a result vector of size n, then copy elements from positions 0 through n-1 using a do loop. This is essential for the ILP solver because the LP relaxation adds slack/surplus variables, producing a longer solution vector. After solving, we need to extract just the original n decision variables from the full solution.")
    (context "Available: make-vector, vector-ref, vector-set!, do, =, +. Allocation + copy loop.")
    (before "(define (vector-take v n)
  ;; TODO: first n elements of vector
  (make-vector n 0))")
    (test-file "lattice/optimization/test-ilp.ss")
    (test-forms (";; Take first 3 of 5
(assert-equal '#(10 20 30) (vector-take '#(10 20 30 40 50) 3))"
                 ";; Take all
(assert-equal '#(1 2 3) (vector-take '#(1 2 3) 3))"
                 ";; Take none
(assert-equal '#() (vector-take '#(1 2 3) 0))"
                 ";; Take 1
(assert-equal '#(42) (vector-take '#(42 99) 1))"))
    (available-modules (lp))
    (hints ("Allocate: (let ([result (make-vector n 0)]) ...)"
            "Copy loop: (do ([i 0 (+ i 1)]) ((= i n) result) ...)"
            "Each step: (vector-set! result i (vector-ref v i))"))
    (reference-solution "(define (vector-take v n)
  (let ([result (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        [(= i n) result]
      (vector-set! result i (vector-ref v i)))))")
    (alternatives ())
    (source-commit "97556a64")
    (difficulty 1))

) ;; end tasks
