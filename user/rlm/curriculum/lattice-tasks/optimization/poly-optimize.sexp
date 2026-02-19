;;; poly-optimize.sexp — Development tasks for lattice/optimization/poly-optimize.ss
;;;
;;; Module: poly-optimize (tier 0, depends on prelude, field, algebra/polynomial, multivariate, convergence)
;;; 509 lines, ~25 exported functions
;;; Polynomial optimization with exact derivatives: univariate Newton's method,
;;; polynomial root-finding, multivariate gradients/Hessians, polynomial constraints
;;; (equality/inequality), penalty-method constrained optimization, SOS preparation.
;;;
;;; Git history:
;;;   45a06521 feat(optimization): Add polynomial optimization constraints
;;;   dab80631 feat(module): migrate optimization/, geometry/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement sign
  ;; ──────────────────────────────────────────────
  ;; Three-way sign function.

  (task
    (id "optimization/poly-optimize/sign-impl")
    (module poly-optimize)
    (tier 0)
    (type implement)
    (description "Implement sign: return -1 for negative numbers, 0 for zero, and 1 for positive numbers. This is the signum function, used as a gradient descent fallback direction when the Hessian is near-zero in Newton's method. Use a cond with three branches checking <, >, and else.")
    (context "Available: <, >, cond, else. Three-way conditional.")
    (before "(define (sign x)
  ;; TODO: signum function
  0)")
    (test-file "lattice/optimization/test-poly-optimize.ss")
    (test-forms (";; Negative
(assert-equal -1 (sign -5))"
                 ";; Zero
(assert-equal 0 (sign 0))"
                 ";; Positive
(assert-equal 1 (sign 3.7))"
                 ";; Small negative
(assert-equal -1 (sign -0.001))"))
    (available-modules (prelude field algebra/polynomial multivariate convergence))
    (hints ("Three cases: negative, positive, zero"
            "(cond [(< x 0) -1] [(> x 0) 1] [else 0])"))
    (reference-solution "(define (sign x)
  (cond [(< x 0) -1]
        [(> x 0) 1]
        [else 0]))")
    (alternatives ())
    (source-commit "45a06521")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement constraint-satisfied?
  ;; ──────────────────────────────────────────────
  ;; Check if a polynomial constraint is satisfied.

  (task
    (id "optimization/poly-optimize/constraint-satisfied-impl")
    (module poly-optimize)
    (tier 0)
    (type implement)
    (description "Implement constraint-satisfied?: check if a polynomial constraint (poly op 0) is satisfied at a given value with tolerance. A constraint has an operator (=, <=, >=) accessible via pc-op. For equality (=): satisfied if |val| < tol. For less-or-equal (<=): satisfied if val <= tol. For greater-or-equal (>=): satisfied if val >= -tol. The tolerance allows for numerical imprecision near constraint boundaries. Use case to dispatch on the operator.")
    (context "Available: pc-op, case, abs, <, <=, >=, -. Case dispatch with three branches.")
    (before "(define (constraint-satisfied? c val tol)
  ;; TODO: is constraint satisfied within tolerance?
  #f)")
    (test-file "lattice/optimization/test-poly-optimize.ss")
    (test-forms (";; Equality: within tolerance
(let ([c (make-poly-constraint (make-polynomial Q-field '(1)) '=)])
  (assert-true (constraint-satisfied? c 0.001 0.01)))"
                 ";; Equality: outside tolerance
(let ([c (make-poly-constraint (make-polynomial Q-field '(1)) '=)])
  (assert-false (constraint-satisfied? c 0.5 0.01)))"
                 ";; Less-or-equal: satisfied
(let ([c (make-poly-constraint (make-polynomial Q-field '(1)) '<=)])
  (assert-true (constraint-satisfied? c -1.0 0.01)))"
                 ";; Greater-or-equal: satisfied
(let ([c (make-poly-constraint (make-polynomial Q-field '(1)) '>=)])
  (assert-true (constraint-satisfied? c 1.0 0.01)))"))
    (available-modules (prelude field algebra/polynomial multivariate convergence))
    (hints ("Get operator: (pc-op c)"
            "case dispatch: (case (pc-op c) [(=) ...] [(<= ) ...] [(>=) ...] [else #f])"
            "Equality: (< (abs val) tol)"
            "Less-or-equal: (<= val tol)"
            "Greater-or-equal: (>= val (- tol))"))
    (reference-solution "(define (constraint-satisfied? c val tol)
  (case (pc-op c)
    [(=) (< (abs val) tol)]
    [(<=) (<= val tol)]
    [(>=) (>= val (- tol))]
    [else #f]))")
    (alternatives ())
    (source-commit "45a06521")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement constraint-violation
  ;; ──────────────────────────────────────────────
  ;; Compute how much a constraint is violated.

  (task
    (id "optimization/poly-optimize/constraint-violation-impl")
    (module poly-optimize)
    (tier 0)
    (type implement)
    (description "Implement constraint-violation: compute how much a polynomial constraint is violated (returns 0 if satisfied). For equality (=): violation is |val| (distance from zero). For less-or-equal (<=): violation is max(0, val) — positive when val exceeds 0. For greater-or-equal (>=): violation is max(0, -val) — positive when val is below 0. This is squared in the penalty function to create smooth gradients for constrained optimization.")
    (context "Available: pc-op, case, abs, max, -. Case dispatch returning non-negative violation.")
    (before "(define (constraint-violation c val)
  ;; TODO: how much is constraint violated? (0 if satisfied)
  0)")
    (test-file "lattice/optimization/test-poly-optimize.ss")
    (test-forms (";; Equality violation
(let ([c (make-poly-constraint (make-polynomial Q-field '(1)) '=)])
  (assert-equal 0.5 (constraint-violation c 0.5))
  (assert-equal 0.3 (constraint-violation c -0.3)))"
                 ";; Less-or-equal: violated when positive
(let ([c (make-poly-constraint (make-polynomial Q-field '(1)) '<=)])
  (assert-equal 2.0 (constraint-violation c 2.0))
  (assert-equal 0.0 (constraint-violation c -1.0)))"
                 ";; Greater-or-equal: violated when negative
(let ([c (make-poly-constraint (make-polynomial Q-field '(1)) '>=)])
  (assert-equal 0.0 (constraint-violation c 1.0))
  (assert-equal 2.0 (constraint-violation c -2.0)))"))
    (available-modules (prelude field algebra/polynomial multivariate convergence))
    (hints ("case dispatch on (pc-op c)"
            "Equality: (abs val)"
            "Less-or-equal: (max 0 val)"
            "Greater-or-equal: (max 0 (- val))"
            "else: 0"))
    (reference-solution "(define (constraint-violation c val)
  (case (pc-op c)
    [(=) (abs val)]
    [(<=) (max 0 val)]
    [(>=) (max 0 (- val))]
    [else 0]))")
    (alternatives ())
    (source-commit "45a06521")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement sos-gram-matrix-size
  ;; ──────────────────────────────────────────────
  ;; Size of Gram matrix for SOS decomposition.

  (task
    (id "optimization/poly-optimize/sos-gram-matrix-size-impl")
    (module poly-optimize)
    (tier 0)
    (type implement)
    (description "Implement sos-gram-matrix-size: compute the size of the Gram matrix needed for sum-of-squares (SOS) decomposition of a degree-d polynomial. A polynomial p(x) of degree 2d is SOS if p = v^T Q v where v = [1, x, x^2, ..., x^d] and Q is positive semidefinite. The monomial basis vector v has d/2 + 1 entries, so the Gram matrix is (d/2 + 1) x (d/2 + 1). Use integer division (quotient) to compute d/2, then add 1.")
    (context "Available: quotient, +. Single arithmetic expression.")
    (before "(define (sos-gram-matrix-size d)
  ;; TODO: Gram matrix size for degree-d polynomial
  1)")
    (test-file "lattice/optimization/test-poly-optimize.ss")
    (test-forms (";; Degree 0: size 1 (just a scalar)
(assert-equal 1 (sos-gram-matrix-size 0))"
                 ";; Degree 2: size 2 (2x2 Gram matrix)
(assert-equal 2 (sos-gram-matrix-size 2))"
                 ";; Degree 4: size 3
(assert-equal 3 (sos-gram-matrix-size 4))"
                 ";; Degree 6: size 4
(assert-equal 4 (sos-gram-matrix-size 6))"))
    (available-modules (prelude field algebra/polynomial multivariate convergence))
    (hints ("For degree 2d polynomial, monomial basis is [1, x, ..., x^d]"
            "That's d/2 + 1 monomials"
            "(+ (quotient d 2) 1)"))
    (reference-solution "(define (sos-gram-matrix-size d)
  (+ (quotient d 2) 1))")
    (alternatives ())
    (source-commit "45a06521")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement filter-map
  ;; ──────────────────────────────────────────────
  ;; Combined filter and map in one pass.

  (task
    (id "optimization/poly-optimize/filter-map-impl")
    (module poly-optimize)
    (tier 0)
    (type implement)
    (description "Implement filter-map: apply a function f to each element of a list, keeping only the non-#f results. This combines filter and map into a single pass. For each element, apply f: if the result is #f, skip it; otherwise, collect it. Use a named let loop with an accumulator, building the result in reverse (cons onto acc), then reverse at the end. This is used in multivariate partial derivative computation to drop terms where the variable doesn't appear.")
    (context "Available: null?, car, cdr, cons, reverse, if, let. Named let with accumulator.")
    (before "(define (filter-map f lst)
  ;; TODO: map f, keeping non-#f results
  '())")
    (test-file "lattice/optimization/test-poly-optimize.ss")
    (test-forms (";; Keep and transform positives
(assert-equal '(20 40 50)
  (filter-map (lambda (x) (if (> x 0) (* x 10) #f)) '(-1 2 -3 4 0 5)))"
                 ";; All filtered out
(assert-equal '()
  (filter-map (lambda (x) (if (> x 100) x #f)) '(1 2 3)))"
                 ";; None filtered out
(assert-equal '(1 2 3)
  (filter-map (lambda (x) x) '(1 2 3)))"
                 ";; Empty input
(assert-equal '()
  (filter-map (lambda (x) x) '()))"))
    (available-modules (prelude field algebra/polynomial multivariate convergence))
    (hints ("Named let: (let loop ([xs lst] [acc '()]) ...)"
            "Base case: (null? xs) -> (reverse acc)"
            "Apply f: (let ([result (f (car xs))]) ...)"
            "If result is truthy: (loop (cdr xs) (cons result acc))"
            "If result is #f: (loop (cdr xs) acc)"))
    (reference-solution "(define (filter-map f lst)
  (let loop ([xs lst] [acc '()])
    (if (null? xs)
        (reverse acc)
        (let ([result (f (car xs))])
          (if result
              (loop (cdr xs) (cons result acc))
              (loop (cdr xs) acc))))))")
    (alternatives ())
    (source-commit "45a06521")
    (difficulty 1))

) ;; end tasks
