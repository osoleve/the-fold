;;; lp.sexp — Development tasks for lattice/optimization/lp.ss
;;;
;;; Module: lp (tier 0, depends on prelude, vec, matrix, matrix-decomp, matrix-solvers)
;;; 640 lines, ~25 exported functions
;;; Linear programming using the revised simplex method. Standard form LP,
;;; inequality conversion, basis operations, Bland's rule, Phase I/II,
;;; dual LP, sensitivity analysis, duality verification.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/optimization/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement lp?
  ;; ──────────────────────────────────────────────
  ;; Structure predicate for LP problems.

  (task
    (id "optimization/lp/lp-predicate-impl")
    (module lp)
    (tier 0)
    (type implement)
    (description "Implement lp?: check if a value is a well-formed LP problem in standard form. An LP is represented as the list (lp c A b) where c is a cost vector, A is a constraint matrix, and b is a right-hand side vector. Check five conditions with and: (1) pair?, (2) car is the symbol 'lp, (3) length is exactly 4, (4) cadr (c) is a vector?, (5) caddr (A) is a matrix?, (6) cadddr (b) is a vector?.")
    (context "Available: pair?, eq?, car, cadr, caddr, cadddr, length, =, vector?, matrix?, and. Chain of boolean tests.")
    (before "(define (lp? x)
  ;; TODO: check if x is (lp c A b)
  #f)")
    (test-file "lattice/optimization/test-lp.ss")
    (test-forms (";; Valid LP
(assert-true (lp? (make-lp (vector 1 2) (make-matrix 1 2 '((3 4))) (vector 5))))"
                 ";; Not a pair
(assert-false (lp? 42))"
                 ";; Wrong tag
(assert-false (lp? (list 'not-lp (vector 1) (make-matrix 1 1 '((1))) (vector 1))))"
                 ";; Missing field
(assert-false (lp? (list 'lp (vector 1))))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-solvers))
    (hints ("Start with (pair? x) to avoid errors on non-pairs"
            "Check tag: (eq? (car x) 'lp)"
            "Check length: (= (length x) 4)"
            "Check types: (vector? (cadr x)) for c, (matrix? (caddr x)) for A, (vector? (cadddr x)) for b"
            "Combine all with and"))
    (reference-solution "(define (lp? x)
  (and (pair? x)
       (eq? (car x) 'lp)
       (= (length x) 4)
       (vector? (cadr x))
       (matrix? (caddr x))
       (vector? (cadddr x))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement extract-basis-costs
  ;; ──────────────────────────────────────────────
  ;; Gather cost coefficients for basis variables.

  (task
    (id "optimization/lp/extract-basis-costs-impl")
    (module lp)
    (tier 0)
    (type implement)
    (description "Implement extract-basis-costs: extract the cost coefficients corresponding to basis variables. Given the full cost vector c and a basis vector containing indices of basic variables, build a new vector cb where cb[i] = c[basis[i]]. This is used in the simplex method to compute reduced costs. Create a result vector of length m (= length of basis), then loop from 0 to m-1, setting cb[i] = c[basis[i]] using nested vector-ref.")
    (context "Available: vector-length, vector-ref, vector-set!, make-vector, >=, +, do. Imperative loop with double indexing.")
    (before "(define (extract-basis-costs c basis)
  ;; TODO: cb[i] = c[basis[i]]
  (make-vector (vector-length basis) 0))")
    (test-file "lattice/optimization/test-lp.ss")
    (test-forms (";; Extract costs at indices 1,3 from (10 20 30 40) → (20 40)
(let ([c (vector 10 20 30 40)]
      [basis (vector 1 3)])
  (assert-equal '#(20 40) (extract-basis-costs c basis)))"
                 ";; Single basis variable
(let ([c (vector 5 10 15)]
      [basis (vector 2)])
  (assert-equal '#(15) (extract-basis-costs c basis)))"
                 ";; All variables in basis
(let ([c (vector 1 2 3)]
      [basis (vector 0 1 2)])
  (assert-equal '#(1 2 3) (extract-basis-costs c basis)))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-solvers))
    (hints ("m = (vector-length basis)"
            "Allocate: (let ([cb (make-vector m 0)]) ...)"
            "Loop: (do ([i 0 (+ i 1)]) [(= i m) cb] ...)"
            "Each step: (vector-set! cb i (vector-ref c (vector-ref basis i)))"))
    (reference-solution "(define (extract-basis-costs c basis)
  (let* ([m (vector-length basis)]
         [cb (make-vector m 0)])
    (do ([i 0 (+ i 1)])
        [(= i m) cb]
      (vector-set! cb i (vector-ref c (vector-ref basis i))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement vec-contains?
  ;; ──────────────────────────────────────────────
  ;; Linear scan membership test for vectors.

  (task
    (id "optimization/lp/vec-contains-impl")
    (module lp)
    (tier 0)
    (type implement)
    (description "Implement vec-contains?: check if a vector contains a given value. Scan the vector from index 0 to n-1. If any element equals x (using =), return #t. If the loop completes without finding x, return #f. Use a named let loop with index i.")
    (context "Available: vector-length, vector-ref, =, let. Named let with cond for three-way branch.")
    (before "(define (vec-contains? v x)
  ;; TODO: linear scan
  #f)")
    (test-file "lattice/optimization/test-lp.ss")
    (test-forms (";; Present element
(assert-true (vec-contains? (vector 3 1 4 1 5) 4))"
                 ";; Absent element
(assert-false (vec-contains? (vector 3 1 4 1 5) 2))"
                 ";; Empty vector
(assert-false (vec-contains? (vector) 1))"
                 ";; First element
(assert-true (vec-contains? (vector 7 8 9) 7))"
                 ";; Last element
(assert-true (vec-contains? (vector 7 8 9) 9))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-solvers))
    (hints ("n = (vector-length v)"
            "Named let: (let loop ([i 0]) ...)"
            "Base: (= i n) -> #f"
            "Match: (= (vector-ref v i) x) -> #t"
            "Step: (loop (+ i 1))"))
    (reference-solution "(define (vec-contains? v x)
  (let ([n (vector-length v)])
    (let loop ([i 0])
      (cond
        [(= i n) #f]
        [(= (vector-ref v i) x) #t]
        [else (loop (+ i 1))]))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement find-entering-variable
  ;; ──────────────────────────────────────────────
  ;; Bland's anti-cycling rule for simplex.

  (task
    (id "optimization/lp/find-entering-variable-impl")
    (module lp)
    (tier 0)
    (type implement)
    (description "Implement find-entering-variable: select the entering variable for a simplex pivot using Bland's rule. Bland's rule prevents cycling by always choosing the variable with the smallest index that has a negative reduced cost and is not currently in the basis. Scan reduced-costs from index 0 to n-1. For each index j, check two conditions: (1) the reduced cost is sufficiently negative (< -tolerance, where tolerance is *lp-tolerance* = 1e-10), AND (2) j is not in the current basis (using not and vec-contains?). Return the first j that satisfies both conditions. If no such j exists, return #f (the current solution is optimal).")
    (context "Available: vector-length, vector-ref, <, -, not, and, vec-contains?, *lp-tolerance*, cond, let. Named let loop with index j.")
    (before "(define (find-entering-variable reduced-costs basis)
  ;; TODO: smallest index with negative reduced cost, not in basis
  #f)")
    (test-file "lattice/optimization/test-lp.ss")
    (test-forms (";; Index 1 has negative reduced cost and is not in basis
(let ([rc (vector 0.0 -2.0 1.0 -3.0)]
      [basis (vector 0 2)])
  (assert-equal 1 (find-entering-variable rc basis)))"
                 ";; All non-negative: optimal (return #f)
(let ([rc (vector 0.0 1.0 2.0)]
      [basis (vector 0)])
  (assert-false (find-entering-variable rc basis)))"
                 ";; Skip basis variables: index 0 is negative but in basis, pick 3
(let ([rc (vector -5.0 0.0 0.0 -1.0)]
      [basis (vector 0 1)])
  (assert-equal 3 (find-entering-variable rc basis)))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-solvers))
    (hints ("n = (vector-length reduced-costs)"
            "Named let: (let loop ([j 0]) ...)"
            "Base: (= j n) -> #f  (optimal)"
            "Check: (and (< (vector-ref reduced-costs j) (- *lp-tolerance*)) (not (vec-contains? basis j)))"
            "If check passes, return j. Otherwise (loop (+ j 1))"))
    (reference-solution "(define (find-entering-variable reduced-costs basis)
  (let ([n (vector-length reduced-costs)])
    (let loop ([j 0])
      (cond
        [(= j n) #f]
        [(and (< (vector-ref reduced-costs j) (- *lp-tolerance*))
              (not (vec-contains? basis j)))
         j]
        [else (loop (+ j 1))]))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement construct-solution
  ;; ──────────────────────────────────────────────
  ;; Build full solution vector from basis solution.

  (task
    (id "optimization/lp/construct-solution-impl")
    (module lp)
    (tier 0)
    (type implement)
    (description "Implement construct-solution: build a full n-dimensional solution vector from the basis solution. In the simplex method, only m basic variables have non-zero values (stored in xB). The remaining n-m non-basic variables are zero. Create a zero vector of length n, then for each basis variable at position i, set x[basis[i]] = xB[i]. Guard against out-of-range indices: only set when basis[i] < n (artificial variables from Phase I may have indices >= n).")
    (context "Available: vec-zeros, vector-length, vector-ref, vector-set!, <, when, do, let. One allocation + one loop with conditional assignment.")
    (before "(define (construct-solution basis xB n)
  ;; TODO: x[basis[i]] = xB[i] for each i
  (vec-zeros n))")
    (test-file "lattice/optimization/test-lp.ss")
    (test-forms (";; Basis {1,3}: x = [0, xB[0], 0, xB[1]]
(let ([basis (vector 1 3)]
      [xB (vector 5.0 7.0)])
  (assert-equal '#(0 5.0 0 7.0) (construct-solution basis xB 4)))"
                 ";; Full basis: identity mapping
(let ([basis (vector 0 1 2)]
      [xB (vector 1.0 2.0 3.0)])
  (assert-equal '#(1.0 2.0 3.0) (construct-solution basis xB 3)))"
                 ";; Skip artificial variables (index >= n)
(let ([basis (vector 0 3)]  ;; index 3 is artificial if n=3
      [xB (vector 1.0 2.0)])
  (assert-equal '#(1.0 0 0) (construct-solution basis xB 3)))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-solvers))
    (hints ("Allocate: (let ([x (vec-zeros n)]) ...)"
            "Loop: (do ([i 0 (+ i 1)]) [(= i (vector-length basis)) x] ...)"
            "Get basis index: (let ([j (vector-ref basis i)]) ...)"
            "Guard: (when (< j n) (vector-set! x j (vector-ref xB i)))"))
    (reference-solution "(define (construct-solution basis xB n)
  (let ([x (vec-zeros n)])
    (do ([i 0 (+ i 1)])
        [(= i (vector-length basis)) x]
      (let ([j (vector-ref basis i)])
        (when (< j n)
          (vector-set! x j (vector-ref xB i)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
