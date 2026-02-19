;;; iterative-solvers.sexp — Development tasks for lattice/linalg/iterative-solvers.ss
;;;
;;; Module: iterative-solvers (tier 0, depends on prelude, vec, matrix)
;;; 742 lines, ~15 exported functions
;;; Iterative linear system solvers: Jacobi, Gauss-Seidel, SOR,
;;; Conjugate Gradient, PCG, GMRES. Includes convergence checking,
;;; diagonal dominance testing, and Jacobi preconditioner.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   9dd7f0b3 feat(module): migrate lattice/linalg/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement residual
  ;; ──────────────────────────────────────────────
  ;; Compute ||Ax - b|| for convergence checking.

  (task
    (id "linalg/iterative-solvers/residual-impl")
    (module iterative-solvers)
    (tier 0)
    (type implement)
    (description "Implement residual: compute the residual norm ||Ax - b|| for an approximate solution x of the linear system Ax = b. This is the fundamental convergence measure for all iterative solvers — when the residual is below a tolerance, the solution is good enough. Three steps: (1) compute Ax via matrix-vector multiplication, (2) compute Ax - b via vector subtraction, (3) compute the Euclidean norm of the difference. Uses matrix-vec-mul, vec-sub, and vec-norm from the vec and matrix modules.")
    (context "Available: matrix-vec-mul, vec-sub, vec-norm. Three composed function calls.")
    (before "(define (residual a x b)
  ;; TODO: compute ||Ax - b||
  0)")
    (test-file "lattice/linalg/test-iterative-solvers.ss")
    (test-forms (";; Exact solution has zero residual
(let* ([A (matrix-from-lists '((2 0) (0 3)))]
       [x '#(1.0 2.0)]
       [b '#(2.0 6.0)])
  (assert-true (< (residual A x b) 1e-10)))"
                 ";; Known residual
(let* ([A (matrix-from-lists '((1 0) (0 1)))]
       [x '#(1.0 1.0)]
       [b '#(0.0 0.0)])
  ;; Ax = (1,1), Ax - b = (1,1), ||Ax-b|| = sqrt(2)
  (assert-true (< (abs (- (residual A x b) (sqrt 2))) 1e-10)))"
                 ";; Zero vector
(let* ([A (matrix-from-lists '((4 1) (1 3)))]
       [x '#(0.0 0.0)]
       [b '#(1.0 2.0)])
  ;; Ax = (0,0), Ax - b = (-1,-2), ||...|| = sqrt(5)
  (assert-true (< (abs (- (residual A x b) (sqrt 5))) 1e-10)))"))
    (available-modules (prelude vec matrix))
    (hints ("Step 1: (matrix-vec-mul a x) gives Ax"
            "Step 2: (vec-sub ax b) gives Ax - b"
            "Step 3: (vec-norm diff) gives ||Ax - b||"
            "Can be written as a single nested expression"))
    (reference-solution "(define (residual a x b)
  (let ([ax (matrix-vec-mul a x)])
    (vec-norm (vec-sub ax b))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement converged?
  ;; ──────────────────────────────────────────────
  ;; Simple convergence check predicate.

  (task
    (id "linalg/iterative-solvers/converged-impl")
    (module iterative-solvers)
    (tier 0)
    (type implement)
    (description "Implement converged?: check if a residual value is below a given tolerance. Returns #t if res < tol, #f otherwise. This is the simplest possible predicate — a single comparison — but abstracting it as a named function makes the solver loop logic clearer and allows easy swapping of convergence criteria later. Every iterative solver (Jacobi, Gauss-Seidel, SOR, CG, GMRES) calls this at the top of each iteration to decide whether to stop.")
    (context "Available: <. Single comparison.")
    (before "(define (converged? res tol)
  ;; TODO: is residual below tolerance?
  #f)")
    (test-file "lattice/linalg/test-iterative-solvers.ss")
    (test-forms (";; Below tolerance
(assert-true (converged? 1e-12 1e-10))"
                 ";; Above tolerance
(assert-false (converged? 1e-5 1e-10))"
                 ";; Exactly at tolerance (not converged, need strictly less)
(assert-false (converged? 1e-10 1e-10))"
                 ";; Zero residual
(assert-true (converged? 0.0 1e-10))"))
    (available-modules (prelude vec matrix))
    (hints ("(< res tol)"
            "This is a one-expression function"))
    (reference-solution "(define (converged? res tol)
  (< res tol))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement is-diagonally-dominant?
  ;; ──────────────────────────────────────────────
  ;; Check the convergence condition for Jacobi/GS.

  (task
    (id "linalg/iterative-solvers/diag-dominant-impl")
    (module iterative-solvers)
    (tier 0)
    (type implement)
    (description "Implement is-diagonally-dominant?: check if a matrix is strictly diagonally dominant, meaning for every row i, |a_ii| > sum_{j != i} |a_ij|. This is the key convergence guarantee for Jacobi and Gauss-Seidel iterations. Use a named let loop over rows (i from 0 to n-1): for each row, compute |a_ii| (the diagonal) and the sum of |a_ij| for j != i (the off-diagonal sum). If diagonal > off-diagonal sum for all rows, return #t. Short-circuit: if any row fails, return #f immediately. The inner off-diagonal sum uses a nested named let loop over columns.")
    (context "Available: matrix-rows, matrix-ref, abs, >, =, +, let loop, if, not. Nested loops with short-circuit.")
    (before "(define (is-diagonally-dominant? a)
  ;; TODO: |a_ii| > sum_{j!=i} |a_ij| for all i?
  #f)")
    (test-file "lattice/linalg/test-iterative-solvers.ss")
    (test-forms (";; DD matrix
(assert-true (is-diagonally-dominant? (matrix-from-lists '((4 -1 0) (-1 4 -1) (0 -1 4)))))"
                 ";; Non-DD matrix
(assert-false (is-diagonally-dominant? (matrix-from-lists '((1 2) (3 1)))))"
                 ";; Identity matrix (trivially DD)
(assert-true (is-diagonally-dominant? (matrix-from-lists '((1 0) (0 1)))))"
                 ";; Borderline: equal (not strictly dominant)
(assert-false (is-diagonally-dominant? (matrix-from-lists '((2 1 1) (0 2 1) (0 0 2)))))"))
    (available-modules (prelude vec matrix))
    (hints ("Outer loop: (let loop ([i 0]) ... (= i n) -> #t)"
            "Diagonal: (abs (matrix-ref a i i))"
            "Inner loop for off-diagonal sum: (let sloop ([j 0] [s 0.0]) ...)"
            "Skip diagonal: (if (= j i) (sloop (+ j 1) s) ...)"
            "Accumulate: (sloop (+ j 1) (+ s (abs (matrix-ref a i j))))"
            "Check: (if (> diag sum) (loop (+ i 1)) #f)"))
    (reference-solution "(define (is-diagonally-dominant? a)
  (let ([n (matrix-rows a)])
    (let loop ([i 0])
      (if (= i n)
          #t
          (let* ([diag (abs (matrix-ref a i i))]
                 [sum (let sloop ([j 0] [s 0.0])
                        (if (= j n)
                            s
                            (if (= j i)
                                (sloop (+ j 1) s)
                                (sloop (+ j 1)
                                       (+ s (abs (matrix-ref a i j)))))))])
            (if (> diag sum)
                (loop (+ i 1))
                #f))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement vector-copy
  ;; ──────────────────────────────────────────────
  ;; Deep copy for in-place iterative updates.

  (task
    (id "linalg/iterative-solvers/vector-copy-impl")
    (module iterative-solvers)
    (tier 0)
    (type implement)
    (description "Implement vector-copy: create a fresh copy of a vector. Allocate a new vector of the same length, then copy each element via a do loop. This is essential for iterative solvers that update the solution vector in place (Gauss-Seidel, SOR) — the initial guess must be copied so the caller's vector isn't mutated. The do loop iterates i from 0 to n-1, copying each element with vector-ref and vector-set!, then returns the result vector when i reaches n.")
    (context "Available: vector-length, make-vector, vector-ref, vector-set!, do, =, +. Allocation + copy loop.")
    (before "(define (vector-copy v)
  ;; TODO: create a fresh copy of vector v
  v)")
    (test-file "lattice/linalg/test-iterative-solvers.ss")
    (test-forms (";; Copy has same contents
(assert-equal '#(1 2 3) (vector-copy '#(1 2 3)))"
                 ";; Copy is independent (mutation doesn't affect original)
(let* ([v '#(10 20 30)]
       [vc (vector-copy v)])
  (vector-set! vc 0 99)
  (assert-equal '#(10 20 30) v)
  (assert-equal '#(99 20 30) vc))"
                 ";; Empty vector
(assert-equal '#() (vector-copy '#()))"
                 ";; Single element
(assert-equal '#(42) (vector-copy '#(42)))"))
    (available-modules (prelude vec matrix))
    (hints ("Get length: (vector-length v)"
            "Allocate: (make-vector n 0)"
            "Copy loop: (do ([i 0 (+ i 1)]) ((= i n) result) (vector-set! result i (vector-ref v i)))"
            "Return result from the do termination clause"))
    (reference-solution "(define (vector-copy v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
        (vector-set! result i (vector-ref v i)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement compute-givens
  ;; ──────────────────────────────────────────────
  ;; Givens rotation for GMRES.

  (task
    (id "linalg/iterative-solvers/compute-givens-impl")
    (module iterative-solvers)
    (tier 0)
    (type implement)
    (description "Implement compute-givens: compute Givens rotation parameters (c, s) such that the rotation matrix [[c, s], [-s, c]] applied to [a, b]^T produces [r, 0]^T. Returns a pair (c . s). Three cases: (1) b = 0: no rotation needed, return (1.0 . 0.0). (2) |b| > |a|: compute t = a/b, then s = 1/sqrt(1 + t^2), c = s*t. (3) Otherwise: compute t = b/a, then c = 1/sqrt(1 + t^2), s = c*t. The two non-trivial cases avoid computing sqrt(a^2 + b^2) directly, which could overflow for large values. Givens rotations are the core of GMRES's QR factorization of the Hessenberg matrix.")
    (context "Available: =, >, abs, /, *, +, sqrt, cons, cond, let*. Three-branch cond returning a pair.")
    (before "(define (compute-givens a-val b-val)
  ;; TODO: Givens rotation (c . s) to zero out b
  (cons 1.0 0.0))")
    (test-file "lattice/linalg/test-iterative-solvers.ss")
    (test-forms (";; b = 0: identity rotation
(let ([r (compute-givens 3.0 0.0)])
  (assert-equal 1.0 (car r))
  (assert-equal 0.0 (cdr r)))"
                 ";; a = 0: swap
(let* ([r (compute-givens 0.0 5.0)]
       [c (car r)] [s (cdr r)])
  ;; Should zero out b: c*0 + s*5 = r, -s*0 + c*5 = 0
  (assert-true (< (abs (+ (* (- s) 0.0) (* c 5.0))) 1e-10)))"
                 ";; General case: verify rotation zeros out b
(let* ([a 3.0] [b 4.0]
       [r (compute-givens a b)]
       [c (car r)] [s (cdr r)]
       [result-b (+ (* (- s) a) (* c b))])
  (assert-true (< (abs result-b) 1e-10)))"
                 ";; Verify c^2 + s^2 = 1 (orthogonality)
(let* ([r (compute-givens 3.0 4.0)]
       [c (car r)] [s (cdr r)])
  (assert-true (< (abs (- (+ (* c c) (* s s)) 1.0)) 1e-10)))"))
    (available-modules (prelude vec matrix))
    (hints ("Case 1: (= b-val 0) -> (cons 1.0 0.0)"
            "Case 2: (> (abs b-val) (abs a-val)): t = a/b, s = 1/sqrt(1+t^2), c = s*t"
            "Case 3: else: t = b/a, c = 1/sqrt(1+t^2), s = c*t"
            "Return (cons c s) in all cases"
            "The two cases avoid overflow by dividing the smaller by the larger"))
    (reference-solution "(define (compute-givens a-val b-val)
  (cond
   [(= b-val 0)
    (cons 1.0 0.0)]
   [(> (abs b-val) (abs a-val))
    (let* ([t (/ a-val b-val)]
           [s (/ 1.0 (sqrt (+ 1 (* t t))))]
           [c (* s t)])
      (cons c s))]
   [else
    (let* ([t (/ b-val a-val)]
           [c (/ 1.0 (sqrt (+ 1 (* t t))))]
           [s (* c t)])
      (cons c s))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
