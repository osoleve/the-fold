;;; lattice/linalg/iterative-solvers.ss — Iterative Linear System Solvers
;;; @module iterative-solvers
;;; @requires prelude vec matrix iteration
;;; @description CG, GMRES, BiCGSTAB
;;; @purity total
;;; @stability stable

(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'iteration)

(doc 'module 'iterative-solvers)
(doc 'purity 'partial)
(doc 'description "Iterative Linear System Solvers

Implements iterative methods for solving Ax = b:
  - jacobi : Jacobi iteration (parallelizable, slow convergence)
  - gauss-seidel : Gauss-Seidel iteration (faster convergence)
  - sor : Successive Over-Relaxation (accelerated Gauss-Seidel)
  - conjugate-gradient : CG method (for symmetric positive-definite)
  - gmres : GMRES (for general systems)

Best for large sparse systems where direct methods are expensive.

This is Core code: pure (except where noted), total, assumes reasonable input.

Dependencies (must be loaded by client in correct order):
  - prelude.ss
  - vec.ss
  - matrix.ss

Do NOT load dependencies here to avoid redefinition issues.")

(doc 'module 'constants
     'description "Default tolerance for convergence and maximum iterations")
(define *iterative-tolerance* 1e-10)
(define *iterative-max-iterations* 10000)

(doc 'section 'convergence-checking
     'description "Residual computation and convergence tests")

(doc residual
     'type (-> Matrix Vec Vec Num)
     'description "Compute ||Ax - b||")
(define (residual a x b)
  (doc 'export #t)
  (let ([ax (matrix-vec-mul a x)])
       (vec-norm (vec-sub ax b))))

(doc converged?
     'type (-> Num Num Boolean))
(define (converged? res tol)
  (doc 'export #t)
  (< res tol))

(doc 'section 'jacobi-iteration
     'description "Jacobi iteration for solving Ax = b")

(doc jacobi
     'type (-> Matrix Vec [Vec] [Nat] [Num] (or (list Vec Nat Num) Error))
     'description "Solve Ax = b using Jacobi iteration.

Algorithm:
  x^{k+1}_i = (b_i - Σ_{j≠i} a_ij * x^k_j) / a_ii

Properties:
  - Each component updated independently (parallelizable)
  - Converges if A is strictly diagonally dominant
  - Slower convergence than Gauss-Seidel"
     'param '(a "n×n coefficient matrix")
     'param '(b "n-element right-hand side vector")
     'param '(x0 "Initial guess (default: zero vector)")
     'param '(max-iter "Maximum iterations")
     'param '(tol "Convergence tolerance")
     'returns "(list solution iterations final-residual) or error")
(define (jacobi a b . opts)
  (doc 'export #t)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (cond
         [(not (= n m))
          `(error not-square ,n ,m)]
         [(not (= n (vector-length b)))
          `(error dimension-mismatch ,n ,(vector-length b))]
         [else
          (let* ([x0 (if (and (pair? opts) (vector? (car opts)))
                         (car opts)
                         (make-vector n 0.0))]
                 [rest1 (if (and (pair? opts) (vector? (car opts)))
                            (cdr opts)
                            opts)]
                 [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                               (car rest1)
                               *iterative-max-iterations*)]
                 [rest2 (if (and (pair? rest1) (integer? (car rest1)))
                            (cdr rest1)
                            rest1)]
                 [tol (if (and (pair? rest2) (number? (car rest2)))
                          (car rest2)
                          *iterative-tolerance*)])
                (jacobi-loop a b x0 0 max-iter tol n))])))

(doc jacobi-loop
     'type (-> Matrix Vec Vec Nat Nat Num Nat (or (list Vec Nat Num) Error)))
(define (jacobi-loop a b x iter max-iter tol n)
  (let ([res (residual a x b)])
       (cond
        [(converged? res tol)
         (list x iter res)]
        [(>= iter max-iter)
         `(error no-convergence ,iter ,res)]
        [else
         (let ([x-new (vec-tabulate n i
                        (let ([a-ii (matrix-ref a i i)])
                             (if (< (abs a-ii) *iterative-tolerance*)
                                 ;; Zero diagonal element - can't divide
                                 0.0
                                 ;; x_i = (b_i - Σ_{j≠i} a_ij * x_j) / a_ii
                                 (let ([sum (let loop ([j 0] [s 0.0])
                                                 (if (= j n)
                                                     s
                                                     (if (= j i)
                                                         (loop (+ j 1) s)
                                                         (loop (+ j 1)
                                                               (+ s (* (matrix-ref a i j)
                                                                       (vector-ref x j)))))))])
                                      (/ (- (vector-ref b i) sum) a-ii)))))])
              (jacobi-loop a b x-new (+ iter 1) max-iter tol n))])))

(doc 'section 'gauss-seidel-iteration
     'description "Gauss-Seidel iteration for solving Ax = b")

(doc gauss-seidel
     'type (-> Matrix Vec [Vec] [Nat] [Num] (or (list Vec Nat Num) Error))
     'description "Solve Ax = b using Gauss-Seidel iteration.

Algorithm:
  x^{k+1}_i = (b_i - Σ_{j<i} a_ij * x^{k+1}_j - Σ_{j>i} a_ij * x^k_j) / a_ii

Properties:
  - Uses latest values immediately (sequential)
  - Converges faster than Jacobi for most problems
  - Converges if A is symmetric positive-definite or strictly diagonally dominant"
     'note "Arguments same as jacobi")
(define (gauss-seidel a b . opts)
  (doc 'export #t)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (cond
         [(not (= n m))
          `(error not-square ,n ,m)]
         [(not (= n (vector-length b)))
          `(error dimension-mismatch ,n ,(vector-length b))]
         [else
          (let* ([x0 (if (and (pair? opts) (vector? (car opts)))
                         (car opts)
                         (make-vector n 0.0))]
                 [rest1 (if (and (pair? opts) (vector? (car opts)))
                            (cdr opts)
                            opts)]
                 [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                               (car rest1)
                               *iterative-max-iterations*)]
                 [rest2 (if (and (pair? rest1) (integer? (car rest1)))
                            (cdr rest1)
                            rest1)]
                 [tol (if (and (pair? rest2) (number? (car rest2)))
                          (car rest2)
                          *iterative-tolerance*)])
                (gauss-seidel-loop a b (vector-copy x0) 0 max-iter tol n))])))

(doc gauss-seidel-loop
     'type (-> Matrix Vec Vec Nat Nat Num Nat (or (list Vec Nat Num) Error)))
(define (gauss-seidel-loop a b x iter max-iter tol n)
  (doc 'export #t)
  (let ([res (residual a x b)])
       (cond
        [(converged? res tol)
         (list x iter res)]
        [(>= iter max-iter)
         `(error no-convergence ,iter ,res)]
        [else
         ;; Update x in place
         (do ([i 0 (+ i 1)])
             ((= i n))
             (let ([a-ii (matrix-ref a i i)])
                  (when (>= (abs a-ii) *iterative-tolerance*)
                        ;; sum1 = Σ_{j<i} a_ij * x^{k+1}_j (already updated)
                        ;; sum2 = Σ_{j>i} a_ij * x^k_j (not yet updated)
                        (let ([sum (let loop ([j 0] [s 0.0])
                                        (if (= j n)
                                            s
                                            (if (= j i)
                                                (loop (+ j 1) s)
                                                (loop (+ j 1)
                                                      (+ s (* (matrix-ref a i j)
                                                              (vector-ref x j)))))))])
                             (vector-set! x i (/ (- (vector-ref b i) sum) a-ii))))))
         (gauss-seidel-loop a b x (+ iter 1) max-iter tol n)])))

(doc 'section 'sor
     'description "Successive Over-Relaxation (SOR)")

(doc sor
     'type (-> Matrix Vec Num [Vec] [Nat] [Num] (or (list Vec Nat Num) Error))
     'description "Solve Ax = b using SOR (Successive Over-Relaxation).

Algorithm:
  x^{k+1}_i = (1-ω)x^k_i + ω * GS_update_i

where GS_update is the Gauss-Seidel update.

Properties:
  - ω ∈ (0, 2) for convergence
  - ω = 1 gives Gauss-Seidel
  - ω > 1 is over-relaxation (acceleration)
  - ω < 1 is under-relaxation (damping for stability)
  - Optimal ω depends on spectrum of A"
     'param '(a "n×n coefficient matrix")
     'param '(b "n-element right-hand side vector")
     'param '(omega "Relaxation parameter (typically 1.0 to 1.9)")
     'param '(x0 "Initial guess")
     'param '(max-iter "Maximum iterations")
     'param '(tol "Convergence tolerance"))
(define (sor a b omega . opts)
  (doc 'export #t)
  (cond
   [(or (<= omega 0) (>= omega 2))
    `(error invalid-omega ,omega)]
   [else
    (let* ([n (matrix-rows a)]
           [m (matrix-cols a)])
          (cond
           [(not (= n m))
            `(error not-square ,n ,m)]
           [(not (= n (vector-length b)))
            `(error dimension-mismatch ,n ,(vector-length b))]
           [else
            (let* ([x0 (if (and (pair? opts) (vector? (car opts)))
                           (car opts)
                           (make-vector n 0.0))]
                   [rest1 (if (and (pair? opts) (vector? (car opts)))
                              (cdr opts)
                              opts)]
                   [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                                 (car rest1)
                                 *iterative-max-iterations*)]
                   [rest2 (if (and (pair? rest1) (integer? (car rest1)))
                              (cdr rest1)
                              rest1)]
                   [tol (if (and (pair? rest2) (number? (car rest2)))
                            (car rest2)
                            *iterative-tolerance*)])
                  (sor-loop a b omega (vector-copy x0) 0 max-iter tol n))]))]))

(doc sor-loop
     'type (-> Matrix Vec Num Vec Nat Nat Num Nat (or (list Vec Nat Num) Error)))
(define (sor-loop a b omega x iter max-iter tol n)
  (doc 'export #t)
  (let ([res (residual a x b)])
       (cond
        [(converged? res tol)
         (list x iter res)]
        [(>= iter max-iter)
         `(error no-convergence ,iter ,res)]
        [else
         ;; Update x in place with relaxation
         (do ([i 0 (+ i 1)])
             ((= i n))
             (let ([a-ii (matrix-ref a i i)])
                  (when (>= (abs a-ii) *iterative-tolerance*)
                        (let* ([x-old (vector-ref x i)]
                               [sum (let loop ([j 0] [s 0.0])
                                         (if (= j n)
                                             s
                                             (if (= j i)
                                                 (loop (+ j 1) s)
                                                 (loop (+ j 1)
                                                       (+ s (* (matrix-ref a i j)
                                                               (vector-ref x j)))))))]
                               [x-gs (/ (- (vector-ref b i) sum) a-ii)]
                               ;; SOR update: x_new = (1-ω)x_old + ω*x_gs
                               [x-new (+ (* (- 1 omega) x-old) (* omega x-gs))])
                              (vector-set! x i x-new)))))
         (sor-loop a b omega x (+ iter 1) max-iter tol n)])))

(doc 'section 'conjugate-gradient
     'description "Conjugate Gradient method for symmetric positive-definite systems")

(doc conjugate-gradient
     'type (-> Matrix Vec [Vec] [Nat] [Num] (or (list Vec Nat Num) Error))
     'description "Solve Ax = b using the Conjugate Gradient method.

Requirements:
  - A must be symmetric positive-definite

Properties:
  - Guaranteed to converge in at most n iterations (exact arithmetic)
  - Optimal for SPD systems
  - Much faster than stationary methods for well-conditioned systems

Algorithm:
  r_0 = b - Ax_0
  p_0 = r_0
  For k = 0, 1, ...
    α_k = (r_k · r_k) / (p_k · Ap_k)
    x_{k+1} = x_k + α_k * p_k
    r_{k+1} = r_k - α_k * Ap_k
    β_k = (r_{k+1} · r_{k+1}) / (r_k · r_k)
    p_{k+1} = r_{k+1} + β_k * p_k")
(define (conjugate-gradient a b . opts)
  (doc 'export #t)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (cond
         [(not (= n m))
          `(error not-square ,n ,m)]
         [(not (= n (vector-length b)))
          `(error dimension-mismatch ,n ,(vector-length b))]
         [else
          (let* ([x0 (if (and (pair? opts) (vector? (car opts)))
                         (car opts)
                         (make-vector n 0.0))]
                 [rest1 (if (and (pair? opts) (vector? (car opts)))
                            (cdr opts)
                            opts)]
                 [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                               (car rest1)
                               n)]  ; CG converges in at most n iterations
                 [rest2 (if (and (pair? rest1) (integer? (car rest1)))
                            (cdr rest1)
                            rest1)]
                 [tol (if (and (pair? rest2) (number? (car rest2)))
                          (car rest2)
                          *iterative-tolerance*)]
                 ;; Initialize: r = b - Ax, p = r
                 [ax0 (matrix-vec-mul a x0)]
                 [r0 (vec-sub b ax0)]
                 [r0-norm (vec-norm r0)])
                (if (< r0-norm tol)
                    ;; Already converged
                    (list x0 0 r0-norm)
                    (cg-loop a b (vector-copy x0) r0 (vector-copy r0)
                             (vec-dot r0 r0) 0 max-iter tol n)))])))

(doc cg-loop
     'type (-> Matrix Vec Vec Vec Vec Num Nat Nat Num Nat (or (list Vec Nat Num) Error))
     'description "CG loop: a, b=system; x=current solution; r=residual; p=search direction; rr=r·r")
(define (cg-loop a b x r p rr iter max-iter tol n)
  (doc 'export #t)
  (let ([r-norm (sqrt rr)])
       (cond
        [(< r-norm tol)
         (list x iter r-norm)]
        [(>= iter max-iter)
         `(error no-convergence ,iter ,r-norm)]
        [else
         (let* ([ap (matrix-vec-mul a p)]
                [pap (vec-dot p ap)])
               (if (< (abs pap) *iterative-tolerance*)
                   ;; p is nearly in the null space or A is not SPD
                   `(error breakdown-pap-zero ,iter ,pap)
                   (let* ([alpha (/ rr pap)]
                          ;; x = x + α*p
                          [x-new (vec-add x (vec-scale alpha p))]
                          ;; r = r - α*Ap
                          [r-new (vec-sub r (vec-scale alpha ap))]
                          [rr-new (vec-dot r-new r-new)]
                          [beta (/ rr-new rr)]
                          ;; p = r + β*p
                          [p-new (vec-add r-new (vec-scale beta p))])
                         (cg-loop a b x-new r-new p-new rr-new
                                  (+ iter 1) max-iter tol n))))])))

(doc 'section 'preconditioned-cg
     'description "Preconditioned Conjugate Gradient")

(doc pcg
     'type (-> Matrix Vec Preconditioner [Vec] [Nat] [Num] (or (list Vec Nat Num) Error))
     'description "Solve Ax = b using Preconditioned Conjugate Gradient.

The preconditioner M is represented as a function that solves Mz = r.
Common preconditioners:
  - Jacobi: M = diag(A), solve-precond = (lambda (r) (vec-div r (diag a)))
  - ILU: Incomplete LU factorization
  - SSOR: Symmetric SOR

Algorithm modification:
  z_k = M^{-1} r_k
  Use z instead of r for direction updates")
(define (pcg a b solve-precond . opts)
  (doc 'export #t)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (cond
         [(not (= n m))
          `(error not-square ,n ,m)]
         [(not (= n (vector-length b)))
          `(error dimension-mismatch ,n ,(vector-length b))]
         [else
          (let* ([x0 (if (and (pair? opts) (vector? (car opts)))
                         (car opts)
                         (make-vector n 0.0))]
                 [rest1 (if (and (pair? opts) (vector? (car opts)))
                            (cdr opts)
                            opts)]
                 [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                               (car rest1)
                               n)]
                 [rest2 (if (and (pair? rest1) (integer? (car rest1)))
                            (cdr rest1)
                            rest1)]
                 [tol (if (and (pair? rest2) (number? (car rest2)))
                          (car rest2)
                          *iterative-tolerance*)]
                 ;; Initialize
                 [ax0 (matrix-vec-mul a x0)]
                 [r0 (vec-sub b ax0)]
                 [z0 (solve-precond r0)]
                 [p0 (vector-copy z0)]
                 [rz0 (vec-dot r0 z0)]
                 [r0-norm (vec-norm r0)])
                (if (< r0-norm tol)
                    (list x0 0 r0-norm)
                    (pcg-loop a b solve-precond (vector-copy x0) r0 z0 p0
                              rz0 0 max-iter tol n)))])))

(doc pcg-loop
     'type (-> Matrix Vec Proc Vec Vec Vec Vec Num Nat Nat Num Nat ...))
(define (pcg-loop a b solve-precond x r z p rz iter max-iter tol n)
  (doc 'export #t)
  (let ([r-norm (vec-norm r)])
       (cond
        [(< r-norm tol)
         (list x iter r-norm)]
        [(>= iter max-iter)
         `(error no-convergence ,iter ,r-norm)]
        [else
         (let* ([ap (matrix-vec-mul a p)]
                [pap (vec-dot p ap)])
               (if (< (abs pap) *iterative-tolerance*)
                   `(error breakdown-pap-zero ,iter ,pap)
                   (let* ([alpha (/ rz pap)]
                          [x-new (vec-add x (vec-scale alpha p))]
                          [r-new (vec-sub r (vec-scale alpha ap))]
                          [z-new (solve-precond r-new)]
                          [rz-new (vec-dot r-new z-new)]
                          [beta (/ rz-new rz)]
                          [p-new (vec-add z-new (vec-scale beta p))])
                         (pcg-loop a b solve-precond x-new r-new z-new p-new
                                   rz-new (+ iter 1) max-iter tol n))))])))

(doc 'section 'gmres
     'description "GMRES (Generalized Minimum Residual)")

(doc gmres
     'type (-> Matrix Vec [Nat] [Vec] [Nat] [Num] (or (list Vec Nat Num) Error))
     'description "Solve Ax = b using GMRES (restarted).

Properties:
  - Works for general (non-symmetric) matrices
  - Minimizes residual over Krylov subspace
  - Memory grows with iterations; restart controls memory"
     'param '(a "n×n coefficient matrix")
     'param '(b "n-element right-hand side vector")
     'param '(restart "Restart after this many iterations (default: 30)")
     'param '(x0 "Initial guess")
     'param '(max-iter "Maximum total iterations")
     'param '(tol "Convergence tolerance"))
(define (gmres a b . opts)
  (doc 'export #t)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (cond
         [(not (= n m))
          `(error not-square ,n ,m)]
         [(not (= n (vector-length b)))
          `(error dimension-mismatch ,n ,(vector-length b))]
         [else
          (let* ([restart (if (and (pair? opts) (integer? (car opts)))
                              (car opts)
                              (min 30 n))]
                 [rest1 (if (and (pair? opts) (integer? (car opts)))
                            (cdr opts)
                            opts)]
                 [x0 (if (and (pair? rest1) (vector? (car rest1)))
                         (car rest1)
                         (make-vector n 0.0))]
                 [rest2 (if (and (pair? rest1) (vector? (car rest1)))
                            (cdr rest1)
                            rest1)]
                 [max-iter (if (and (pair? rest2) (integer? (car rest2)))
                               (car rest2)
                               *iterative-max-iterations*)]
                 [rest3 (if (and (pair? rest2) (integer? (car rest2)))
                            (cdr rest2)
                            rest2)]
                 [tol (if (and (pair? rest3) (number? (car rest3)))
                          (car rest3)
                          *iterative-tolerance*)])
                (gmres-outer a b restart (vector-copy x0) 0 max-iter tol n))])))

(doc gmres-outer
     'type (-> Matrix Vec Nat Vec Nat Nat Num Nat (or (list Vec Nat Num) Error))
     'description "Outer loop for restarted GMRES")
(define (gmres-outer a b restart x total-iter max-iter tol n)
  (doc 'export #t)
  (let* ([ax (matrix-vec-mul a x)]
         [r (vec-sub b ax)]
         [r-norm (vec-norm r)])
        (cond
         [(< r-norm tol)
          (list x total-iter r-norm)]
         [(>= total-iter max-iter)
          `(error no-convergence ,total-iter ,r-norm)]
         [else
          (let ([result (gmres-arnoldi a b x r r-norm restart tol n)])
               (if (and (pair? result) (eq? (car result) 'error))
                   result
                   (let* ([x-new (car result)]
                          [iters (cadr result)]
                          [final-res (caddr result)]
                          [new-total (+ total-iter iters)])
                         (if (< final-res tol)
                             (list x-new new-total final-res)
                             (gmres-outer a b restart x-new new-total max-iter tol n)))))])))

(doc gmres-arnoldi
     'type (-> Matrix Vec Vec Vec Num Nat Num Nat (or (list Vec Nat Num) Error))
     'description "Inner GMRES using Arnoldi process")
(define (gmres-arnoldi a b x0 r0 beta restart tol n)
  (doc 'export #t)
  ;; V: matrix of Arnoldi vectors (n × (restart+1))
  ;; H: upper Hessenberg matrix ((restart+1) × restart)
  (let* ([k restart]
         [v-list '()]  ; Will store vectors as a list
         [h-data (make-vector (* (+ k 1) k) 0.0)]  ; (k+1) × k Hessenberg matrix
         ;; v_1 = r_0 / ||r_0||
         [v1 (vec-scale (/ 1.0 beta) r0)]
         [_ (set! v-list (list v1))]
         ;; g = [beta, 0, 0, ..., 0]^T (right-hand side for least squares)
         [g (vec-tabulate (+ k 1) i (if (= i 0) beta 0.0))]
         ;; Givens rotation parameters
         [cs (make-vector k 0.0)]
         [sn (make-vector k 0.0)])
        (gmres-arnoldi-loop a h-data v-list g cs sn 0 k tol n x0 r0)))

(doc gmres-arnoldi-loop
     'type (-> ... (or (list Vec Nat Num) Error)))
(define (gmres-arnoldi-loop a h-data v-list g cs sn j k tol n x0 r0)
  (doc 'export #t)
  (if (>= j k)
      ;; Reached restart limit - solve least squares and return
      (gmres-solve-update h-data v-list g j n x0)
      (let* ([vj (list-ref (reverse v-list) j)]
             [w (matrix-vec-mul a vj)])
            ;; Modified Gram-Schmidt orthogonalization
            (let orth-loop ([i 0] [w-curr w])
                 (if (> i j)
                     ;; Store h_{j+1,j} = ||w||
                     (let ([w-norm (vec-norm w-curr)])
                          (let ([h-idx (+ (* (+ j 1) k) j)])  ; h[j+1, j]
                               (when (< h-idx (vector-length h-data))
                                     (vector-set! h-data h-idx w-norm)))
                          (if (< w-norm *iterative-tolerance*)
                              ;; Lucky breakdown - w is zero, Krylov subspace is invariant
                              (gmres-solve-update h-data v-list g (+ j 1) n x0)
                              ;; Normalize and continue
                              (let* ([vj+1 (vec-scale (/ 1.0 w-norm) w-curr)]
                                     [new-v-list (cons vj+1 v-list)]
                                     ;; Apply previous Givens rotations to column j of H
                                     [_ (apply-givens-rotations h-data cs sn j k)]
                                     ;; Compute new Givens rotation for h[j,j] and h[j+1,j]
                                     [h-jj-idx (+ (* j k) j)]
                                     [h-j1j-idx (+ (* (+ j 1) k) j)]
                                     [h-jj (vector-ref h-data h-jj-idx)]
                                     [h-j1j (vector-ref h-data h-j1j-idx)]
                                     [givens (compute-givens h-jj h-j1j)]
                                     [c (car givens)]
                                     [s (cdr givens)]
                                     [_ (vector-set! cs j c)]
                                     [_ (vector-set! sn j s)]
                                     ;; Apply to H
                                     [new-hjj (+ (* c h-jj) (* s h-j1j))]
                                     [_ (vector-set! h-data h-jj-idx new-hjj)]
                                     [_ (vector-set! h-data h-j1j-idx 0.0)]
                                     ;; Apply to g
                                     [g-j (vector-ref g j)]
                                     [g-j1 (vector-ref g (+ j 1))]
                                     [new-gj (+ (* c g-j) (* s g-j1))]
                                     [new-gj1 (+ (* (- s) g-j) (* c g-j1))]
                                     [_ (vector-set! g j new-gj)]
                                     [_ (vector-set! g (+ j 1) new-gj1)]
                                     [res (abs new-gj1)])
                                    (if (< res tol)
                                        ;; Converged
                                        (gmres-solve-update h-data new-v-list g (+ j 1) n x0)
                                        ;; Continue
                                        (gmres-arnoldi-loop a h-data new-v-list g cs sn
                                                            (+ j 1) k tol n x0 r0)))))
                     ;; Orthogonalize against v_i
                     (let* ([vi (list-ref (reverse v-list) i)]
                            [h-ij (vec-dot w-curr vi)]
                            ;; Store h[i,j]
                            [h-idx (+ (* i k) j)])
                           (when (< h-idx (vector-length h-data))
                                 (vector-set! h-data h-idx h-ij))
                           (orth-loop (+ i 1)
                                      (vec-sub w-curr (vec-scale h-ij vi)))))))))

(doc apply-givens-rotations
     'type (-> Vec Vec Vec Nat Nat Void)
     'description "Apply stored Givens rotations to column j of H")
(define (apply-givens-rotations h-data cs sn j k)
  (doc 'export #t)
  (do ([i 0 (+ i 1)])
      ((= i j))
      (let* ([h-ij-idx (+ (* i k) j)]
             [h-i1j-idx (+ (* (+ i 1) k) j)]
             [h-ij (vector-ref h-data h-ij-idx)]
             [h-i1j (vector-ref h-data h-i1j-idx)]
             [c (vector-ref cs i)]
             [s (vector-ref sn i)]
             [new-hij (+ (* c h-ij) (* s h-i1j))]
             [new-hi1j (+ (* (- s) h-ij) (* c h-i1j))])
            (vector-set! h-data h-ij-idx new-hij)
            (vector-set! h-data h-i1j-idx new-hi1j))))

(doc compute-givens
     'type (-> Num Num (cons Num Num))
     'description "Compute Givens rotation (c, s) such that [c s; -s c] * [a; b] = [r; 0]")
(define (compute-givens a-val b-val)
  (doc 'export #t)
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
          (cons c s))]))

(doc gmres-solve-update
     'type (-> Vec List Vec Nat Nat Vec (list Vec Nat Num))
     'description "Solve triangular system and update x")
(define (gmres-solve-update h-data v-list g m n x0)
  (doc 'export #t)
  (let ([y (make-vector m 0.0)]
        [k (if (= m 0) 1 m)])  ; Prevent division by zero
       ;; Back-substitution: H[:m,:m] is upper triangular
       (do ([i (- m 1) (- i 1)])
           ((< i 0))
           (let* ([sum (let loop ([j (+ i 1)] [s 0.0])
                            (if (>= j m)
                                s
                                (let ([h-idx (+ (* i k) j)])
                                     (loop (+ j 1)
                                           (+ s (* (vector-ref h-data h-idx)
                                                   (vector-ref y j)))))))]
                  [h-ii-idx (+ (* i k) i)]
                  [h-ii (if (< h-ii-idx (vector-length h-data))
                            (vector-ref h-data h-ii-idx)
                            1.0)])
                 (when (>= (abs h-ii) *iterative-tolerance*)
                       (vector-set! y i (/ (- (vector-ref g i) sum) h-ii)))))
       ;; x = x0 + V[:,:m] * y
       (let ([x-new (vector-copy x0)]
             [v-rev (reverse v-list)])
            (do ([i 0 (+ i 1)])
                ((>= i (min m (length v-rev))))
                (let ([vi (list-ref v-rev i)]
                      [yi (vector-ref y i)])
                     (do ([j 0 (+ j 1)])
                         ((= j n))
                         (vector-set! x-new j
                                      (+ (vector-ref x-new j)
                                         (* yi (vector-ref vi j)))))))
            ;; The residual norm is |g[m]| in the GMRES algorithm
            (let ([res-norm (if (< m (vector-length g))
                                (abs (vector-ref g m))
                                0.0)])
                 (list x-new m res-norm)))))

(doc 'section 'utilities
     'description "Utility functions for iterative solvers")

(doc vector-copy
     'type (-> Vec Vec))
(define (vector-copy v)
  (doc 'export #t)
  (vec-tabulate (vector-length v) i (vector-ref v i)))

(doc make-jacobi-preconditioner
     'type (-> Matrix (-> Vec Vec))
     'description "Create Jacobi (diagonal) preconditioner")
(define (make-jacobi-preconditioner a)
  (doc 'export #t)
  (let* ([n (matrix-rows a)]
         [diag-inv (vec-tabulate n i
                     (let ([d (matrix-ref a i i)])
                          (if (> (abs d) *iterative-tolerance*)
                              (/ 1.0 d)
                              0.0)))])
        (lambda (r)
                (vec-tabulate n i (* (vector-ref r i)
                                     (vector-ref diag-inv i))))))

(doc is-diagonally-dominant?
     'type (-> Matrix Boolean)
     'description "Check if matrix is strictly diagonally dominant")
(define (is-diagonally-dominant? a)
  (doc 'export #t)
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
                          #f))))))

(doc spectral-radius-estimate
     'type (-> Matrix [Nat] (or Num Error))
     'description "Estimate spectral radius using power iteration")
(define (spectral-radius-estimate a . opts)
  (doc 'export #t)
  (let* ([max-iter (if (pair? opts) (car opts) 100)]
         [n (matrix-rows a)]
         [v (make-vector n 1.0)]
         [norm-v (vec-norm v)]
         [v (vec-scale (/ 1.0 norm-v) v)])
        (let loop ([iter 0] [v-curr v] [lambda-prev 0.0])
             (if (>= iter max-iter)
                 lambda-prev
                 (let* ([av (matrix-vec-mul a v-curr)]
                        [lambda-new (vec-dot v-curr av)]
                        [av-norm (vec-norm av)])
                       (if (< av-norm *iterative-tolerance*)
                           0.0  ; Matrix is likely nilpotent or nearly so
                           (let ([v-new (vec-scale (/ 1.0 av-norm) av)])
                                (if (< (abs (- lambda-new lambda-prev)) *iterative-tolerance*)
                                    lambda-new
                                    (loop (+ iter 1) v-new lambda-new)))))))))
