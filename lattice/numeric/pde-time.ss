;;; lattice/numeric/pde-time.ss — Time stepping schemes for PDEs
;;; @module pde-time
;;; @requires prelude linalg/vec linalg/matrix linalg/sparse linalg/iterative-solvers

(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'sparse)
(require 'iterative-solvers)

(doc 'module 'pde-time)
(doc 'description "Time stepping schemes for PDEs: explicit, implicit, and adaptive methods")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Section 1: Time Stepping Overview
;;; ============================================================
;;;
;;; For time-dependent PDEs like:
;;;   ∂u/∂t = L(u)  where L is a spatial operator
;;;
;;; We discretize space (FEM, finite difference) to get:
;;;   M du/dt = K u + F  (or just du/dt = A u + b)
;;;
;;; Time stepping advances the solution from t^n to t^{n+1}.
;;;
;;; Methods:
;;; - Forward Euler (explicit): Fast but conditionally stable
;;; - Backward Euler (implicit): Unconditionally stable, requires solve
;;; - Crank-Nicolson: 2nd order, unconditionally stable, requires solve
;;; - Method of Lines: Use ODE integrators on spatial discretization
;;; - RK4 for PDEs: High accuracy explicit method

(doc 'section 'overview)

;;; Module-level constants
(define *pde-epsilon* 1e-30)  ; Minimum denominator to prevent division by zero

;;; ============================================================
;;; Section 2: Vector Operations
;;; ============================================================

(doc 'section 'vector-ops)

(doc pde-vec-add 'type '(-> Vector Vector Vector))
(define (pde-vec-add v1 v2)
  (let* ([n (vector-length v1)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (+ (vector-ref v1 i) (vector-ref v2 i))))))

(doc pde-vec-sub 'type '(-> Vector Vector Vector))
(define (pde-vec-sub v1 v2)
  (let* ([n (vector-length v1)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (- (vector-ref v1 i) (vector-ref v2 i))))))

(doc pde-vec-scale 'type '(-> Number Vector Vector))
(define (pde-vec-scale s v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (* s (vector-ref v i))))))

(doc pde-vec-madd 'type '(-> Vector Number Vector Vector))
(doc pde-vec-madd 'description "Multiply-add: v1 + s * v2")
(define (pde-vec-madd v1 s v2)
  (let* ([n (vector-length v1)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (+ (vector-ref v1 i) (* s (vector-ref v2 i)))))))

(doc pde-vec-dot 'type '(-> Vector Vector Number))
(define (pde-vec-dot v1 v2)
  (let ([n (vector-length v1)])
    (let loop ([i 0] [sum 0.0])
      (if (= i n)
          sum
          (loop (+ i 1) (+ sum (* (vector-ref v1 i) (vector-ref v2 i))))))))

(doc pde-vec-norm 'type '(-> Vector Number))
(define (pde-vec-norm v)
  (sqrt (pde-vec-dot v v)))

;;; ============================================================
;;; Section 3: Forward Euler (Explicit)
;;; ============================================================
;;;
;;; For du/dt = f(u, t):
;;;   u^{n+1} = u^n + dt * f(u^n, t^n)
;;;
;;; For M du/dt = A u + b:
;;;   u^{n+1} = u^n + dt * M^{-1} (A u^n + b)
;;;
;;; Stability: dt < dt_crit (CFL condition)

(doc 'section 'forward-euler)

(doc forward-euler-step 'type '(-> (-> Vector Number Vector) Vector Number Number Vector))
(doc forward-euler-step 'description "One step of forward Euler: u^{n+1} = u^n + dt * f(u^n, t^n)")
(define (forward-euler-step f u t dt)
  (let ([du (f u t)])
    (pde-vec-madd u dt du)))

(doc forward-euler-matrix-step 'type '(-> SparseCSR Vector Vector Number Vector))
(doc forward-euler-matrix-step 'description "Forward Euler for du/dt = A*u + b")
(define (forward-euler-matrix-step A u b dt)
  ;; u^{n+1} = u^n + dt * (A*u^n + b)
  (let* ([Au (sparse-csr-vec-mul A u)]
         [rhs (pde-vec-add Au b)])
    (pde-vec-madd u dt rhs)))

(doc forward-euler-mass-step 'type '(-> SparseCSR SparseCSR Vector Vector Number Number Number Vector))
(doc forward-euler-mass-step 'description "Forward Euler for M*du/dt = A*u + b with lumped mass approximation")
(define (forward-euler-mass-step M-diag A u b dt tol max-iter)
  ;; Lumped mass: use diagonal of M for efficiency
  ;; u^{n+1} = u^n + dt * M_diag^{-1} * (A*u^n + b)
  (let* ([Au (sparse-csr-vec-mul A u)]
         [rhs (pde-vec-add Au b)]
         [n (vector-length u)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (let ([m-i (vector-ref M-diag i)]
            [r-i (vector-ref rhs i)]
            [u-i (vector-ref u i)])
        (vector-set! result i (+ u-i (/ (* dt r-i) (max m-i *pde-epsilon*))))))))

;;; ============================================================
;;; Section 4: Backward Euler (Implicit)
;;; ============================================================
;;;
;;; For du/dt = f(u, t):
;;;   u^{n+1} = u^n + dt * f(u^{n+1}, t^{n+1})
;;;   Requires solving nonlinear system
;;;
;;; For M du/dt = A u + b (linear):
;;;   (M - dt*A) u^{n+1} = M u^n + dt*b
;;;   Requires solving linear system
;;;
;;; Stability: Unconditionally stable (A-stable)

(doc 'section 'backward-euler)

(doc backward-euler-linear-step 'type '(-> SparseCSR SparseCSR Vector Vector Number Number Number (Values Vector Number Nat)))
(doc backward-euler-linear-step 'description "Backward Euler for M*du/dt = A*u + b (returns solution, residual, iterations)")
(doc backward-euler-linear-step 'note "Uses CG solver - requires (M - dt*A) to be symmetric positive definite (e.g., pure diffusion). Will fail or diverge for non-SPD systems like advection-diffusion.")
(define (backward-euler-linear-step M A u b dt tol max-iter)
  ;; Solve: (M - dt*A) u^{n+1} = M*u^n + dt*b
  ;; Build LHS: M - dt*A
  (let* ([n (sparse-csr-rows M)]
         [LHS (sparse-csr-add M (sparse-csr-scale (- dt) A))]
         ;; Build RHS: M*u^n + dt*b
         [Mu (sparse-csr-vec-mul M u)]
         [RHS (pde-vec-madd Mu dt b)]
         ;; Solve using CG
         [result (sparse-cg-solve LHS RHS u tol max-iter)])
    (values (car result) (cadr result) (caddr result))))

(doc backward-euler-identity-step 'type '(-> SparseCSR Vector Vector Number Number Number (Values Vector Number Nat)))
(doc backward-euler-identity-step 'description "Backward Euler for du/dt = A*u + b (M = I)")
(define (backward-euler-identity-step A u b dt tol max-iter)
  ;; Solve: (I - dt*A) u^{n+1} = u^n + dt*b
  (let* ([n (sparse-csr-rows A)]
         ;; I - dt*A
         [LHS (sparse-csr-add (sparse-csr-identity n) (sparse-csr-scale (- dt) A))]
         ;; u^n + dt*b
         [RHS (pde-vec-madd u dt b)]
         [result (sparse-cg-solve LHS RHS u tol max-iter)])
    (values (car result) (cadr result) (caddr result))))

;;; ============================================================
;;; Section 5: Crank-Nicolson (Implicit, 2nd Order)
;;; ============================================================
;;;
;;; Average of forward and backward Euler:
;;;   u^{n+1} = u^n + dt/2 * (f(u^n, t^n) + f(u^{n+1}, t^{n+1}))
;;;
;;; For M du/dt = A u + b:
;;;   (M - dt/2*A) u^{n+1} = (M + dt/2*A) u^n + dt*b
;;;
;;; Stability: Unconditionally stable, 2nd order accurate in time

(doc 'section 'crank-nicolson)

(doc crank-nicolson-step 'type '(-> SparseCSR SparseCSR Vector Vector Number Number Number (Values Vector Number Nat)))
(doc crank-nicolson-step 'description "Crank-Nicolson for M*du/dt = A*u + b")
(doc crank-nicolson-step 'note "Uses CG solver - requires (M - dt/2*A) to be symmetric positive definite (e.g., pure diffusion). Will fail or diverge for non-SPD systems.")
(define (crank-nicolson-step M A u b dt tol max-iter)
  ;; Solve: (M - dt/2*A) u^{n+1} = (M + dt/2*A) u^n + dt*b
  (let* ([half-dt (/ dt 2)]
         [n (sparse-csr-rows M)]
         ;; LHS: M - dt/2*A
         [LHS (sparse-csr-add M (sparse-csr-scale (- half-dt) A))]
         ;; RHS: (M + dt/2*A) u^n + dt*b
         [M-plus (sparse-csr-add M (sparse-csr-scale half-dt A))]
         [Mu (sparse-csr-vec-mul M-plus u)]
         [RHS (pde-vec-madd Mu dt b)]
         [result (sparse-cg-solve LHS RHS u tol max-iter)])
    (values (car result) (cadr result) (caddr result))))

(doc crank-nicolson-identity-step 'type '(-> SparseCSR Vector Vector Number Number Number (Values Vector Number Nat)))
(doc crank-nicolson-identity-step 'description "Crank-Nicolson for du/dt = A*u + b (M = I)")
(define (crank-nicolson-identity-step A u b dt tol max-iter)
  (let* ([half-dt (/ dt 2)]
         [n (sparse-csr-rows A)]
         [I (sparse-csr-identity n)]
         ;; LHS: I - dt/2*A
         [LHS (sparse-csr-add I (sparse-csr-scale (- half-dt) A))]
         ;; RHS: (I + dt/2*A) u^n + dt*b
         [I-plus (sparse-csr-add I (sparse-csr-scale half-dt A))]
         [Iu (sparse-csr-vec-mul I-plus u)]
         [RHS (pde-vec-madd Iu dt b)]
         [result (sparse-cg-solve LHS RHS u tol max-iter)])
    (values (car result) (cadr result) (caddr result))))

;;; ============================================================
;;; Section 6: Method of Lines
;;; ============================================================
;;;
;;; Discretize space first to get ODE system:
;;;   du/dt = f(u, t)
;;; Then use any ODE solver (Euler, RK4, etc.)
;;;
;;; This connects the spatial discretization to our existing ODE integrators.

(doc 'section 'method-of-lines)

(doc mol-rhs 'type '(-> SparseCSR Vector (-> Vector Number Vector)))
(doc mol-rhs 'description "Create MOL RHS function du/dt = A*u + b")
(define (mol-rhs A b)
  (lambda (u t)
    (let ([Au (sparse-csr-vec-mul A u)])
      (pde-vec-add Au b))))

(doc mol-euler-step 'type '(-> SparseCSR Vector Vector Number Number Vector))
(doc mol-euler-step 'description "MOL with forward Euler")
(define (mol-euler-step A b u t dt)
  (let ([f (mol-rhs A b)])
    (forward-euler-step f u t dt)))

(doc mol-rk4-step 'type '(-> SparseCSR Vector Vector Number Number Vector))
(doc mol-rk4-step 'description "MOL with RK4 (4th order accurate)")
(define (mol-rk4-step A b u t dt)
  (let* ([f (mol-rhs A b)]
         [half-dt (/ dt 2)]
         [k1 (f u t)]
         [k2 (f (pde-vec-madd u half-dt k1) (+ t half-dt))]
         [k3 (f (pde-vec-madd u half-dt k2) (+ t half-dt))]
         [k4 (f (pde-vec-madd u dt k3) (+ t dt))]
         ;; u + dt/6 * (k1 + 2*k2 + 2*k3 + k4)
         [weighted (pde-vec-add k1
                                (pde-vec-add (pde-vec-scale 2 k2)
                                             (pde-vec-add (pde-vec-scale 2 k3) k4)))])
    (pde-vec-madd u (/ dt 6) weighted)))

;;; ============================================================
;;; Section 7: Stability Analysis
;;; ============================================================
;;;
;;; CFL Condition: dt < C * h^2 / alpha (for parabolic)
;;;                dt < C * h / c (for hyperbolic)
;;; where h = mesh spacing, alpha = diffusivity, c = wave speed

(doc 'section 'stability)

(doc cfl-parabolic 'type '(-> Number Number Number Number))
(doc cfl-parabolic 'description "Compute stable dt for parabolic PDE (heat equation)")
(doc cfl-parabolic 'note "dt < safety * h² / (2 * dim * alpha) for explicit methods")
(define (cfl-parabolic h alpha dim)
  ;; CFL for forward Euler on heat equation in dim dimensions
  ;; dt < h² / (2 * dim * alpha)
  (let ([safety 0.9])  ; Safety factor
    (* safety (/ (* h h) (* 2 dim alpha)))))

(doc cfl-hyperbolic 'type '(-> Number Number Number))
(doc cfl-hyperbolic 'description "Compute stable dt for hyperbolic PDE (wave equation)")
(doc cfl-hyperbolic 'note "dt < safety * h / c for explicit methods")
(define (cfl-hyperbolic h c)
  ;; CFL for wave equation: dt < h / c
  (let ([safety 0.9])
    (* safety (/ h c))))

(doc estimate-mesh-spacing 'type '(-> Vector Number))
(doc estimate-mesh-spacing 'description "Estimate characteristic mesh spacing from node coordinates")
(doc estimate-mesh-spacing 'note "Assumes nodes are sorted in linear 1D order (adjacent indices = spatial neighbors). For 2D/3D unstructured meshes, use proper spatial queries instead.")
(define (estimate-mesh-spacing nodes)
  ;; Use minimum distance between adjacent nodes as estimate
  ;; For regular grids, this gives h directly
  (let* ([n (vector-length nodes)]
         [min-dist +inf.0])
    (when (>= n 2)
      ;; Sample a few pairs to estimate (not exhaustive for large meshes)
      (let ([samples (min 100 (- n 1))])
        (do ([i 0 (+ i 1)])
            ((= i samples))
          (let* ([p1 (vector-ref nodes i)]
                 [p2 (vector-ref nodes (+ i 1))]
                 ;; Assuming p1, p2 are (x . y) pairs or similar
                 [dx (- (if (pair? p1) (car p1) p1)
                        (if (pair? p2) (car p2) p2))]
                 [dist (abs dx)])
            (when (and (> dist 0) (< dist min-dist))
              (set! min-dist dist))))))
    min-dist))

;;; ============================================================
;;; Section 8: Adaptive Time Stepping
;;; ============================================================
;;;
;;; Adjust dt based on error estimate:
;;; - Take two steps with dt and one with 2*dt
;;; - Compare solutions to estimate local error
;;; - Adjust dt to maintain target accuracy

(doc 'section 'adaptive)

(doc adaptive-euler-step 'type '(-> (-> Vector Number Vector) Vector Number Number Number Number (Values Vector Number Number)))
(doc adaptive-euler-step 'description "Adaptive forward Euler with error control")
(doc adaptive-euler-step 'returns "(values u_new dt_new error)")
(define (adaptive-euler-step f u t dt tol safety)
  ;; Richardson extrapolation: compare full step with two half steps
  (let* ([half-dt (/ dt 2)]
         ;; One full step
         [u-full (forward-euler-step f u t dt)]
         ;; Two half steps
         [u-half1 (forward-euler-step f u t half-dt)]
         [u-half2 (forward-euler-step f u-half1 (+ t half-dt) half-dt)]
         ;; Error estimate (difference between methods)
         [error-vec (pde-vec-sub u-half2 u-full)]
         [error (pde-vec-norm error-vec)]
         ;; New dt estimate: dt_new = safety * dt * (tol / error)^(1/2)
         [dt-new (if (< error 1e-30)
                     (* 2 dt)  ; Error too small, can increase dt
                     (* dt safety (expt (/ tol error) 0.5)))])
    ;; Accept step if error < tol, return half-step result (more accurate)
    (values (if (< error tol) u-half2 u)
            (min dt-new (* 2 dt))  ; Don't grow too fast
            error)))

(doc integrate-adaptive 'type '(-> (-> Vector Number Vector) Vector Number Number Number Number Number Number (List (Pair Number Vector))))
(doc integrate-adaptive 'description "Integrate PDE with adaptive time stepping")
(define (integrate-adaptive f u0 t0 t-end dt0 tol safety max-steps)
  ;; Returns list of (t . u) pairs
  (let loop ([t t0] [u u0] [dt dt0] [steps 0] [results (list (cons t0 (vector-copy u0)))])
    (cond
      [(>= t t-end) (reverse results)]
      [(>= steps max-steps) (reverse results)]
      [else
       (let ([dt-clamped (min dt (- t-end t))])
         (let-values ([(u-new dt-new error)
                       (adaptive-euler-step f u t dt-clamped tol safety)])
           (if (< error tol)
               ;; Accept step
               (loop (+ t dt-clamped) u-new dt-new (+ steps 1)
                     (cons (cons (+ t dt-clamped) (vector-copy u-new)) results))
               ;; Reject step, try again with smaller dt
               (loop t u (/ dt 2) steps results))))])))

;;; ============================================================
;;; Section 9: Sparse Matrix Utilities
;;; ============================================================

(doc 'section 'sparse-utils)

(doc sparse-csr-identity 'type '(-> Nat SparseCSR))
(doc sparse-csr-identity 'description "Create sparse identity matrix in CSR format")
(define (sparse-csr-identity n)
  (let* ([row-ptrs (make-vector (+ n 1) 0)]
         [col-indices (make-vector n 0)]
         [values (make-vector n 1.0)])
    ;; Fill row pointers and column indices
    (do ([i 0 (+ i 1)])
        ((= i n))
      (vector-set! row-ptrs i i)
      (vector-set! col-indices i i))
    (vector-set! row-ptrs n n)
    ;; Return CSR structure: (sparse-csr rows cols row-ptrs col-indices values)
    (list 'sparse-csr n n row-ptrs col-indices values)))

;; Note: sparse-csr-scale is provided by lattice/linalg/sparse.ss

;; Note: sparse-csr-add is provided by lattice/linalg/sparse.ss

(doc sparse-cg-solve 'type '(-> SparseCSR Vector Vector Number Number (List Vector Number Nat)))
(doc sparse-cg-solve 'description "Solve Ax = b using conjugate gradient")
(define (sparse-cg-solve A b x0 tol max-iter)
  ;; CG for Ax = b where A is sparse CSR
  (let* ([n (sparse-csr-rows A)]
         ;; r = b - Ax
         [Ax0 (sparse-csr-vec-mul A x0)]
         [r (pde-vec-sub b Ax0)]
         [p (vector-copy r)]
         [rr (pde-vec-dot r r)])
    (if (< (sqrt rr) tol)
        (list x0 (sqrt rr) 0)
        (cg-loop A b (vector-copy x0) r p rr 0 max-iter tol n))))

(define (cg-loop A b x r p rr iter max-iter tol n)
  (let ([r-norm (sqrt rr)])
    (cond
      [(< r-norm tol)
       (list x r-norm iter)]
      [(>= iter max-iter)
       (list x r-norm iter)]
      [else
       (let* ([Ap (sparse-csr-vec-mul A p)]
              [pAp (pde-vec-dot p Ap)])
         (if (< (abs pAp) *pde-epsilon*)
             (list x r-norm iter)
             (let* ([alpha (/ rr pAp)]
                    [x-new (pde-vec-madd x alpha p)]
                    [r-new (pde-vec-madd r (- alpha) Ap)]
                    [rr-new (pde-vec-dot r-new r-new)]
                    [beta (/ rr-new rr)]
                    [p-new (pde-vec-madd r-new beta p)])
               (cg-loop A b x-new r-new p-new rr-new
                        (+ iter 1) max-iter tol n))))])))

;;; ============================================================
;;; Section 10: Time Stepper Factory
;;; ============================================================

(doc 'section 'factory)

(doc make-time-stepper 'type '(-> Symbol SparseCSR SparseCSR Vector (-> Vector Number Number Vector)))
(doc make-time-stepper 'description "Create time stepper function for given method")
(doc make-time-stepper 'note "Methods: forward-euler, backward-euler, crank-nicolson, mol-rk4")
(define (make-time-stepper method M A b)
  (case method
    [(forward-euler)
     (let ([M-diag (sparse-csr-diagonal-vec M)])
       (lambda (u t dt)
         (forward-euler-mass-step M-diag A u b dt 1e-10 1000)))]
    [(mol-euler)
     (lambda (u t dt)
       (mol-euler-step A b u t dt))]
    [(mol-rk4)
     (lambda (u t dt)
       (mol-rk4-step A b u t dt))]
    [(backward-euler)
     (lambda (u t dt)
       (let-values ([(u-new residual iters)
                     (backward-euler-linear-step M A u b dt 1e-10 1000)])
         u-new))]
    [(crank-nicolson)
     (lambda (u t dt)
       (let-values ([(u-new residual iters)
                     (crank-nicolson-step M A u b dt 1e-10 1000)])
         u-new))]
    [else
     (error 'make-time-stepper "Unknown method" method)]))

(doc sparse-csr-diagonal-vec 'type '(-> SparseCSR Vector))
(doc sparse-csr-diagonal-vec 'description "Extract diagonal elements of sparse CSR matrix")
(define (sparse-csr-diagonal-vec A)
  (let* ([n (sparse-csr-rows A)]
         [row-ptrs (sparse-csr-row-ptrs A)]
         [col-indices (sparse-csr-col-indices A)]
         [vals (sparse-csr-values A)]
         [diag (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) diag)
      (let ([start (vector-ref row-ptrs i)]
            [end (vector-ref row-ptrs (+ i 1))])
        (do ([k start (+ k 1)])
            ((= k end))
          (when (= (vector-ref col-indices k) i)
            (vector-set! diag i (vector-ref vals k))))))))

;;; ============================================================
;;; Section 11: Integration Driver
;;; ============================================================

(doc 'section 'driver)

(doc integrate-pde 'type '(-> (-> Vector Number Number Vector) Vector Number Number Number Nat (List (Pair Number Vector))))
(doc integrate-pde 'description "Integrate PDE over time interval [t0, t-end] with fixed time step")
(define (integrate-pde stepper u0 t0 t-end dt max-steps)
  (let loop ([t t0] [u (vector-copy u0)] [steps 0] [results (list (cons t0 (vector-copy u0)))])
    (cond
      [(>= t t-end) (reverse results)]
      [(>= steps max-steps) (reverse results)]
      [else
       (let* ([dt-clamped (min dt (- t-end t))]
              [u-new (stepper u t dt-clamped)])
         (loop (+ t dt-clamped) u-new (+ steps 1)
               (cons (cons (+ t dt-clamped) (vector-copy u-new)) results)))])))
