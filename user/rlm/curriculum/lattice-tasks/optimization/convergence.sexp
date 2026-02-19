;;; convergence.sexp — Development tasks for lattice/optimization/convergence.ss
;;;
;;; Module: convergence (tier 0, depends on prelude, vec)
;;; 187 lines, ~15 exported functions
;;; Multi-criterion stopping conditions for optimization: gradient norm,
;;; function value change, parameter change, iteration limits.
;;; Includes scaled vector norms (overflow-safe) and progress reporting.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement vec-norm
  ;; ──────────────────────────────────────────────
  ;; Overflow-safe L2 norm via max-element scaling.

  (task
    (id "optimization/convergence/vec-norm-impl")
    (module convergence)
    (tier 0)
    (type implement)
    (description "Implement vec-norm: compute the L2 (Euclidean) norm of a list-vector using scaling by the maximum absolute element to prevent floating-point overflow. Direct computation of sqrt(sum(x^2)) overflows for large values. The scaling trick: ||v|| = max_abs * sqrt(sum((x/max_abs)^2)). Steps: (1) Empty list → 0. (2) Find max-abs = max of absolute values via fold-left. (3) If max-abs < 1e-300 (near zero), return 0. (4) Scale each element by max-abs, square, sum, sqrt, multiply back by max-abs.")
    (context "Available: null?, fold-left, map, abs, max, <, /, *, +, sqrt, lambda. Two passes: find max, then compute scaled sum.")
    (before "(define (vec-norm v)
  ;; TODO: scaled L2 norm to avoid overflow
  0)")
    (test-file "lattice/optimization/test-convergence.ss")
    (test-forms (";; Pythagorean triple: ||(3,4)|| = 5
(assert-equal 5 (vec-norm '(3 4)))"
                 ";; Empty vector: 0
(assert-equal 0 (vec-norm '()))"
                 ";; Zero vector: 0
(assert-equal 0 (vec-norm '(0 0 0)))"
                 ";; Unit vector: 1
(assert-true (< (abs (- (vec-norm '(1 0 0)) 1.0)) 0.001))"))
    (available-modules (prelude vec))
    (hints ("Guard: (null? v) → 0"
            "max-abs = (fold-left max 0 (map abs v))"
            "Guard: (< max-abs 1e-300) → 0"
            "Scale: (map (lambda (x) (let ([s (/ x max-abs)]) (* s s))) v)"
            "Sum: (fold-left + 0 scaled-squares)"
            "Return: (* max-abs (sqrt sum))"))
    (reference-solution "(define (vec-norm v)
  (if (null? v)
      0
      (let ([max-abs (fold-left max 0 (map abs v))])
           (if (< max-abs 1e-300)
               0
               (let ([scaled-sq-sum (fold-left + 0
                                               (map (lambda (x)
                                                            (let ([s (/ x max-abs)])
                                                                 (* s s)))
                                                    v))])
                    (* max-abs (sqrt scaled-sq-sum)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement converged?
  ;; ──────────────────────────────────────────────
  ;; Multi-criterion stopping check — the heart of the module.

  (task
    (id "optimization/convergence/converged-impl")
    (module convergence)
    (tier 0)
    (type implement)
    (description "Implement converged?: check if optimization has converged using multiple criteria. Returns #f if not converged, or a symbol indicating the reason: 'grad-converged (gradient norm < grad-tol), 'f-converged (function change < f-tol, after iter 0), 'x-converged (parameter change < x-tol, after iter 0), 'max-iter (iterations >= max-iter). Check in priority order: gradient first, then f-change, then x-change, then max-iter. The f-change and x-change checks skip iteration 0 (no previous values to compare).")
    (context "Available: cs-grad-norm, cs-f-change, cs-x-change, cs-iter, cc-grad-tol, cc-f-tol, cc-x-tol, cc-max-iter, <, >, >=, and, cond, else. Four-case cond dispatch.")
    (before "(define (converged? criteria state)
  ;; TODO: multi-criterion convergence check
  #f)")
    (test-file "lattice/optimization/test-convergence.ss")
    (test-forms (";; Not converged: all values above tolerance
(let ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 1000)])
  (assert-false (converged? cc (make-convergence-state 5 1.0 0.01 0.001 0.001))))"
                 ";; Gradient converged
(let ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 1000)])
  (assert-equal 'grad-converged
    (converged? cc (make-convergence-state 5 1.0 1e-7 0.001 0.001))))"
                 ";; Max iterations reached
(let ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 1000)])
  (assert-equal 'max-iter
    (converged? cc (make-convergence-state 1000 1.0 0.01 0.001 0.001))))"
                 ";; f-change converged (after iter 0)
(let ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 1000)])
  (assert-equal 'f-converged
    (converged? cc (make-convergence-state 5 1.0 0.01 1e-9 0.001))))"))
    (available-modules (prelude vec))
    (hints ("Cond with four clauses + else"
            "1: (< (cs-grad-norm state) (cc-grad-tol criteria)) → 'grad-converged"
            "2: (and (> iter 0) (< f-change f-tol)) → 'f-converged"
            "3: (and (> iter 0) (< x-change x-tol)) → 'x-converged"
            "4: (>= iter max-iter) → 'max-iter"
            "else → #f"))
    (reference-solution "(define (converged? criteria state)
  (cond
   [(< (cs-grad-norm state) (cc-grad-tol criteria))
    'grad-converged]
   [(and (> (cs-iter state) 0)
         (< (cs-f-change state) (cc-f-tol criteria)))
    'f-converged]
   [(and (> (cs-iter state) 0)
         (< (cs-x-change state) (cc-x-tol criteria)))
    'x-converged]
   [(>= (cs-iter state) (cc-max-iter criteria))
    'max-iter]
   [else #f]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement update-convergence-state
  ;; ──────────────────────────────────────────────
  ;; Tracking optimization progress across iterations.

  (task
    (id "optimization/convergence/update-state-impl")
    (module convergence)
    (tier 0)
    (type implement)
    (description "Implement update-convergence-state: compute the new convergence state from new iteration data. Given the old state, new function value, new gradient, old parameter vector, and new parameter vector, compute: new-iter = old iter + 1, grad-norm = L2 norm of new gradient (via vec-norm), f-change = |new-f - old-f|, x-change = ||new-x - old-x|| (via vec-diff-norm). Return a new convergence state with these values.")
    (context "Available: cs-iter, cs-f-val, vec-norm, vec-diff-norm, make-convergence-state, +, -, abs, let*. Five computations into one constructor.")
    (before "(define (update-convergence-state old-state new-f new-grad old-x new-x)
  ;; TODO: compute new iter, grad-norm, f-change, x-change
  old-state)")
    (test-file "lattice/optimization/test-convergence.ss")
    (test-forms (";; Iteration increments
(let* ([s0 (make-convergence-state 0 10.0 1.0 1.0 1.0)]
       [s1 (update-convergence-state s0 9.0 '(0.1 0.1) '(1.0 0.0) '(1.1 0.1))])
  (assert-equal 1 (cs-iter s1)))"
                 ";; f-change computed correctly
(let* ([s0 (make-convergence-state 0 10.0 1.0 1.0 1.0)]
       [s1 (update-convergence-state s0 8.0 '(0.1) '(1.0) '(1.1))])
  (assert-true (< (abs (- (cs-f-change s1) 2.0)) 0.001)))"
                 ";; Gradient norm computed
(let* ([s0 (make-convergence-state 0 10.0 1.0 1.0 1.0)]
       [s1 (update-convergence-state s0 9.0 '(3 4) '(0 0) '(1 1))])
  (assert-true (< (abs (- (cs-grad-norm s1) 5.0)) 0.001)))"))
    (available-modules (prelude vec))
    (hints ("new-iter = (+ (cs-iter old-state) 1)"
            "grad-norm = (vec-norm new-grad)"
            "f-change = (abs (- new-f (cs-f-val old-state)))"
            "x-change = (vec-diff-norm new-x old-x)"
            "Return (make-convergence-state new-iter new-f grad-norm f-change x-change)"))
    (reference-solution "(define (update-convergence-state old-state new-f new-grad old-x new-x)
  (let* ([new-iter (+ (cs-iter old-state) 1)]
         [grad-norm (vec-norm new-grad)]
         [f-change (abs (- new-f (cs-f-val old-state)))]
         [x-change (vec-diff-norm new-x old-x)])
        (make-convergence-state new-iter new-f grad-norm f-change x-change)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement vec-diff-norm
  ;; ──────────────────────────────────────────────
  ;; Distance between parameter vectors.

  (task
    (id "optimization/convergence/vec-diff-norm-impl")
    (module convergence)
    (tier 0)
    (type implement)
    (description "Implement vec-diff-norm: compute the L2 distance between two list-vectors, ||v1 - v2||. Compute the element-wise difference using map, then take the L2 norm of the result using vec-norm. This measures how much the parameter vector has changed between iterations — a small vec-diff-norm means the optimizer has nearly stopped moving.")
    (context "Available: vec-norm, map, -. One map and one norm call.")
    (before "(define (vec-diff-norm v1 v2)
  ;; TODO: ||v1 - v2||
  0)")
    (test-file "lattice/optimization/test-convergence.ss")
    (test-forms (";; Identical vectors: distance 0
(assert-equal 0 (vec-diff-norm '(1 2 3) '(1 2 3)))"
                 ";; Known distance: ||(4,0) - (0,3)|| = 5
(assert-equal 5 (vec-diff-norm '(4 0) '(0 -3)))"
                 ";; Symmetric: d(a,b) = d(b,a)
(assert-equal (vec-diff-norm '(1 2) '(3 4))
              (vec-diff-norm '(3 4) '(1 2)))"))
    (available-modules (prelude vec))
    (hints ("Difference: (map - v1 v2)"
            "Return: (vec-norm (map - v1 v2))"))
    (reference-solution "(define (vec-diff-norm v1 v2)
  (vec-norm (map - v1 v2)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement initial-convergence-state
  ;; ──────────────────────────────────────────────
  ;; Starting state for an optimization run.

  (task
    (id "optimization/convergence/initial-state-impl")
    (module convergence)
    (tier 0)
    (type implement)
    (description "Implement initial-convergence-state: create the initial convergence state at the start of optimization. Set iteration to 0, function value to the given f-val, gradient norm to the L2 norm of the given gradient, and both f-change and x-change to +inf.0 (infinity). The infinite changes ensure that the f-change and x-change criteria won't trigger at iteration 0 — only the gradient norm or max-iter criteria can stop optimization before any updates have occurred.")
    (context "Available: make-convergence-state, vec-norm, +inf.0. One constructor call.")
    (before "(define (initial-convergence-state f-val grad)
  ;; TODO: iter=0, f-change=+inf, x-change=+inf
  (make-convergence-state 0 0 0 0 0))")
    (test-file "lattice/optimization/test-convergence.ss")
    (test-forms (";; Iteration starts at 0
(let ([s (initial-convergence-state 10.0 '(3 4))])
  (assert-equal 0 (cs-iter s)))"
                 ";; Function value stored
(let ([s (initial-convergence-state 42.0 '(1))])
  (assert-equal 42.0 (cs-f-val s)))"
                 ";; Gradient norm computed
(let ([s (initial-convergence-state 10.0 '(3 4))])
  (assert-true (< (abs (- (cs-grad-norm s) 5.0)) 0.001)))"
                 ";; Changes are infinite initially
(let ([s (initial-convergence-state 10.0 '(1))])
  (assert-equal +inf.0 (cs-f-change s))
  (assert-equal +inf.0 (cs-x-change s)))"))
    (available-modules (prelude vec))
    (hints ("grad-norm = (vec-norm grad)"
            "Return (make-convergence-state 0 f-val grad-norm +inf.0 +inf.0)"))
    (reference-solution "(define (initial-convergence-state f-val grad)
  (make-convergence-state 0 f-val (vec-norm grad) +inf.0 +inf.0))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
