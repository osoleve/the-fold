;;; lattice/numeric/finite-diff.ss — Finite Difference Methods
;;; @module finite-diff
;;; @requires prelude linalg/vec linalg/matrix linalg/sparse

(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'sparse)

(doc 'module 'finite-diff)
(doc 'description "Finite difference operators and PDE solvers: stencils for spatial derivatives, boundary conditions, and solvers for heat/wave/Laplace equations")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Section 1: Difference Operators
;;; ============================================================
;;;
;;; For a function u sampled at points x_i with spacing h:
;;;   Forward:  (u_{i+1} - u_i) / h
;;;   Backward: (u_i - u_{i-1}) / h
;;;   Central:  (u_{i+1} - u_{i-1}) / (2h)
;;;
;;; Second derivative (central):
;;;   (u_{i+1} - 2*u_i + u_{i-1}) / h²

(doc 'section 'difference-operators)

(doc diff-forward 'export #t)
(doc diff-forward 'type '(-> Vector Number Vector))
(doc diff-forward 'description "Forward difference: (u_{i+1} - u_i) / h. Result has n-1 elements.")
(define (diff-forward u h)
  (let* ([n (vector-length u)]
         [result (make-vector (- n 1) 0.0)]
         [inv-h (/ 1.0 h)])
    (do ([i 0 (+ i 1)])
        ((= i (- n 1)) result)
      (vector-set! result i
                   (* inv-h (- (vector-ref u (+ i 1))
                               (vector-ref u i)))))))

(doc diff-backward 'export #t)
(doc diff-backward 'type '(-> Vector Number Vector))
(doc diff-backward 'description "Backward difference: (u_i - u_{i-1}) / h. Result has n-1 elements.")
(define (diff-backward u h)
  (let* ([n (vector-length u)]
         [result (make-vector (- n 1) 0.0)]
         [inv-h (/ 1.0 h)])
    (do ([i 1 (+ i 1)])
        ((= i n) result)
      (vector-set! result (- i 1)
                   (* inv-h (- (vector-ref u i)
                               (vector-ref u (- i 1))))))))

(doc diff-central 'export #t)
(doc diff-central 'type '(-> Vector Number Vector))
(doc diff-central 'description "Central difference: (u_{i+1} - u_{i-1}) / (2h). Result has n-2 elements (interior only).")
(define (diff-central u h)
  (let* ([n (vector-length u)]
         [result (make-vector (- n 2) 0.0)]
         [inv-2h (/ 1.0 (* 2.0 h))])
    (do ([i 1 (+ i 1)])
        ((= i (- n 1)) result)
      (vector-set! result (- i 1)
                   (* inv-2h (- (vector-ref u (+ i 1))
                                (vector-ref u (- i 1))))))))

(doc diff2-central 'export #t)
(doc diff2-central 'type '(-> Vector Number Vector))
(doc diff2-central 'description "Central second difference: (u_{i+1} - 2*u_i + u_{i-1}) / h². Result has n-2 elements (interior only).")
(define (diff2-central u h)
  (let* ([n (vector-length u)]
         [result (make-vector (- n 2) 0.0)]
         [inv-h2 (/ 1.0 (* h h))])
    (do ([i 1 (+ i 1)])
        ((= i (- n 1)) result)
      (vector-set! result (- i 1)
                   (* inv-h2 (+ (vector-ref u (+ i 1))
                                (- (* 2.0 (vector-ref u i)))
                                (vector-ref u (- i 1))))))))

;;; ============================================================
;;; Section 2: Stencil Application
;;; ============================================================
;;;
;;; A stencil is a set of weights applied to neighboring points.
;;; 1D stencils: vectors of coefficients
;;; 2D stencils: 3x3 or 5x5 matrices of coefficients

(doc 'section 'stencils)

(doc apply-stencil-1d 'export #t)
(doc apply-stencil-1d 'type '(-> Vector Vector Nat Number Vector))
(doc apply-stencil-1d 'description "Apply 1D stencil to vector. Stencil is centered at index 'center'. Returns interior points only (no boundary handling).")
(define (apply-stencil-1d stencil u center h)
  (let* ([n (vector-length u)]
         [s-len (vector-length stencil)]
         [half (quotient s-len 2)]
         [start half]
         [end (- n half)]
         [result-len (- end start)]
         [result (make-vector result-len 0.0)])
    (do ([i start (+ i 1)])
        ((= i end) result)
      (let ([sum 0.0])
        (do ([j 0 (+ j 1)])
            ((= j s-len))
          (let ([idx (+ i (- j half))])
            (set! sum (+ sum (* (vector-ref stencil j)
                                (vector-ref u idx))))))
        (vector-set! result (- i start) sum)))))

;;; Common 1D stencils
(doc stencil-d1-forward 'export #t)
(doc stencil-d1-forward 'description "Forward difference stencil: [-1, 1] / h")
(define stencil-d1-forward '#(-1.0 1.0))

(doc stencil-d1-backward 'export #t)
(doc stencil-d1-backward 'description "Backward difference stencil: [-1, 1] / h")
(define stencil-d1-backward '#(-1.0 1.0))

(doc stencil-d1-central 'export #t)
(doc stencil-d1-central 'description "Central difference stencil: [-1, 0, 1] / (2h)")
(define stencil-d1-central '#(-0.5 0.0 0.5))

(doc stencil-d2-central 'export #t)
(doc stencil-d2-central 'description "Second derivative central stencil: [1, -2, 1] / h²")
(define stencil-d2-central '#(1.0 -2.0 1.0))

;;; ============================================================
;;; Section 3: 2D Grid Operations
;;; ============================================================
;;;
;;; For 2D grids stored as row-major vectors:
;;;   index(i, j) = i * ny + j
;;; where i is row (y-direction), j is column (x-direction)

(doc 'section '2d-grids)

(doc grid-index 'export #t)
(doc grid-index 'type '(-> Nat Nat Nat Nat))
(doc grid-index 'description "Convert 2D indices to linear index: i * ny + j")
(define (grid-index i j ny)
  (+ (* i ny) j))

(doc laplacian-2d 'export #t)
(doc laplacian-2d 'type '(-> Vector Nat Nat Number Vector))
(doc laplacian-2d 'description "Apply 2D Laplacian (∇²u) using 5-point stencil. Returns interior points only.")
(define (laplacian-2d u nx ny h)
  (let* ([inv-h2 (/ 1.0 (* h h))]
         [interior-nx (- nx 2)]
         [interior-ny (- ny 2)]
         [result (make-vector (* interior-nx interior-ny) 0.0)])
    ;; Loop over interior points
    (do ([i 1 (+ i 1)])
        ((= i (- nx 1)) result)
      (do ([j 1 (+ j 1)])
          ((= j (- ny 1)))
        (let* ([center (grid-index i j ny)]
               [left (grid-index i (- j 1) ny)]
               [right (grid-index i (+ j 1) ny)]
               [up (grid-index (- i 1) j ny)]
               [down (grid-index (+ i 1) j ny)]
               [lap (* inv-h2
                       (+ (vector-ref u left)
                          (vector-ref u right)
                          (vector-ref u up)
                          (vector-ref u down)
                          (* -4.0 (vector-ref u center))))]
               [result-idx (grid-index (- i 1) (- j 1) interior-ny)])
          (vector-set! result result-idx lap))))))

;;; ============================================================
;;; Section 4: Boundary Conditions
;;; ============================================================
;;;
;;; Dirichlet: u = g on boundary
;;; Neumann: ∂u/∂n = g on boundary
;;; Periodic: u(0) = u(n-1)

(doc 'section 'boundary-conditions)

(doc apply-dirichlet-1d 'export #t)
(doc apply-dirichlet-1d 'type '(-> Vector Number Number Vector))
(doc apply-dirichlet-1d 'description "Apply Dirichlet BC: set boundary values to left-val and right-val")
(define (apply-dirichlet-1d u left-val right-val)
  (let* ([n (vector-length u)]
         [result (vector-copy u)])
    (vector-set! result 0 left-val)
    (vector-set! result (- n 1) right-val)
    result))

(doc apply-neumann-1d 'export #t)
(doc apply-neumann-1d 'type '(-> Vector Number Number Number Vector))
(doc apply-neumann-1d 'description "Apply Neumann BC: set ghost points so ∂u/∂x = left-deriv at left, right-deriv at right. Uses one-sided differences.")
(define (apply-neumann-1d u h left-deriv right-deriv)
  (let* ([n (vector-length u)]
         [result (vector-copy u)])
    ;; Left: (u[1] - u[0]) / h = left-deriv  =>  u[0] = u[1] - h * left-deriv
    (vector-set! result 0 (- (vector-ref u 1) (* h left-deriv)))
    ;; Right: (u[n-1] - u[n-2]) / h = right-deriv  =>  u[n-1] = u[n-2] + h * right-deriv
    (vector-set! result (- n 1) (+ (vector-ref u (- n 2)) (* h right-deriv)))
    result))

(doc apply-periodic-1d 'export #t)
(doc apply-periodic-1d 'type '(-> Vector Vector))
(doc apply-periodic-1d 'description "Apply periodic BC: u[0] = u[n-2], u[n-1] = u[1]")
(define (apply-periodic-1d u)
  (let* ([n (vector-length u)]
         [result (vector-copy u)])
    (vector-set! result 0 (vector-ref u (- n 2)))
    (vector-set! result (- n 1) (vector-ref u 1))
    result))

;;; ============================================================
;;; Section 5: Laplacian Matrix Assembly
;;; ============================================================
;;;
;;; Build the sparse matrix for the Laplacian operator.
;;; This is used for implicit methods and steady-state problems.

(doc 'section 'matrix-assembly)

(doc laplacian-matrix-1d 'export #t)
(doc laplacian-matrix-1d 'type '(-> Nat Number SparseCSR))
(doc laplacian-matrix-1d 'description "Build 1D Laplacian matrix (tridiagonal): [1, -2, 1] / h² for n interior points. Dirichlet BCs assumed (boundary points eliminated).")
(define (laplacian-matrix-1d n h)
  (let* ([inv-h2 (/ 1.0 (* h h))]
         [diag (* -2.0 inv-h2)]
         [off-diag (* 1.0 inv-h2)]
         ;; CSR format: count non-zeros
         [nnz (- (* 3 n) 2)]  ; tridiagonal
         [row-ptrs (make-vector (+ n 1) 0)]
         [col-indices (make-vector nnz 0)]
         [values (make-vector nnz 0.0)]
         [ptr 0])
    ;; Build row by row
    (do ([i 0 (+ i 1)])
        ((= i n))
      (vector-set! row-ptrs i ptr)
      ;; Left neighbor
      (when (> i 0)
        (vector-set! col-indices ptr (- i 1))
        (vector-set! values ptr off-diag)
        (set! ptr (+ ptr 1)))
      ;; Diagonal
      (vector-set! col-indices ptr i)
      (vector-set! values ptr diag)
      (set! ptr (+ ptr 1))
      ;; Right neighbor
      (when (< i (- n 1))
        (vector-set! col-indices ptr (+ i 1))
        (vector-set! values ptr off-diag)
        (set! ptr (+ ptr 1))))
    (vector-set! row-ptrs n ptr)
    (list 'sparse-csr n n row-ptrs col-indices values)))

(doc laplacian-matrix-2d 'export #t)
(doc laplacian-matrix-2d 'type '(-> Nat Nat Number SparseCSR))
(doc laplacian-matrix-2d 'description "Build 2D Laplacian matrix for nx × ny interior grid using 5-point stencil.")
(define (laplacian-matrix-2d nx ny h)
  (let* ([n (* nx ny)]
         [inv-h2 (/ 1.0 (* h h))]
         [diag (* -4.0 inv-h2)]
         [off-diag (* 1.0 inv-h2)]
         ;; Max 5 entries per row
         [max-nnz (* 5 n)]
         [row-ptrs (make-vector (+ n 1) 0)]
         [col-indices (make-vector max-nnz 0)]
         [values (make-vector max-nnz 0.0)]
         [ptr 0])
    ;; Build row by row
    (do ([i 0 (+ i 1)])
        ((= i nx))
      (do ([j 0 (+ j 1)])
          ((= j ny))
        (let ([row (grid-index i j ny)])
          (vector-set! row-ptrs row ptr)
          ;; Up neighbor (i-1, j)
          (when (> i 0)
            (vector-set! col-indices ptr (grid-index (- i 1) j ny))
            (vector-set! values ptr off-diag)
            (set! ptr (+ ptr 1)))
          ;; Left neighbor (i, j-1)
          (when (> j 0)
            (vector-set! col-indices ptr (grid-index i (- j 1) ny))
            (vector-set! values ptr off-diag)
            (set! ptr (+ ptr 1)))
          ;; Diagonal (i, j)
          (vector-set! col-indices ptr row)
          (vector-set! values ptr diag)
          (set! ptr (+ ptr 1))
          ;; Right neighbor (i, j+1)
          (when (< j (- ny 1))
            (vector-set! col-indices ptr (grid-index i (+ j 1) ny))
            (vector-set! values ptr off-diag)
            (set! ptr (+ ptr 1)))
          ;; Down neighbor (i+1, j)
          (when (< i (- nx 1))
            (vector-set! col-indices ptr (grid-index (+ i 1) j ny))
            (vector-set! values ptr off-diag)
            (set! ptr (+ ptr 1))))))
    (vector-set! row-ptrs n ptr)
    ;; Trim arrays to actual size
    (let ([actual-col (make-vector ptr 0)]
          [actual-val (make-vector ptr 0.0)])
      (do ([k 0 (+ k 1)])
          ((= k ptr))
        (vector-set! actual-col k (vector-ref col-indices k))
        (vector-set! actual-val k (vector-ref values k)))
      (list 'sparse-csr n n row-ptrs actual-col actual-val))))

;;; ============================================================
;;; Section 6: Heat Equation Solver
;;; ============================================================
;;;
;;; ∂u/∂t = α ∇²u
;;; Explicit: u^{n+1} = u^n + α*dt*∇²u^n
;;; Stable when dt < h²/(2*α*dim)

(doc 'section 'heat-equation)

(doc heat-step-explicit-1d 'export #t)
(doc heat-step-explicit-1d 'type '(-> Vector Number Number Number Number Number Vector))
(doc heat-step-explicit-1d 'description "One explicit step of 1D heat equation with Dirichlet BCs")
(define (heat-step-explicit-1d u h dt alpha left-bc right-bc)
  (let* ([n (vector-length u)]
         [inv-h2 (/ 1.0 (* h h))]
         [coeff (* alpha dt inv-h2)]
         [result (make-vector n 0.0)])
    ;; Boundary conditions
    (vector-set! result 0 left-bc)
    (vector-set! result (- n 1) right-bc)
    ;; Interior points: u_new = u + coeff * (u_{i+1} - 2*u_i + u_{i-1})
    (do ([i 1 (+ i 1)])
        ((= i (- n 1)) result)
      (let ([lap (+ (vector-ref u (+ i 1))
                    (* -2.0 (vector-ref u i))
                    (vector-ref u (- i 1)))])
        (vector-set! result i (+ (vector-ref u i) (* coeff lap)))))))

(doc solve-heat-1d 'export #t)
(doc solve-heat-1d 'type '(-> Vector Number Number Number Number Number Nat (List (Pair Number Vector))))
(doc solve-heat-1d 'description "Solve 1D heat equation from t=0 to t=t-end using explicit method")
(define (solve-heat-1d u0 h alpha t-end left-bc right-bc max-steps)
  (let* ([dt-cfl (* 0.4 (/ (* h h) (* 2.0 alpha)))]  ; Safe CFL
         [n-steps (min max-steps (ceiling (/ t-end dt-cfl)))]
         [dt (/ t-end n-steps)])
    (let loop ([t 0.0]
               [u (vector-copy u0)]
               [step 0]
               [results (list (cons 0.0 (vector-copy u0)))])
      (if (>= step n-steps)
          (reverse results)
          (let ([u-new (heat-step-explicit-1d u h dt alpha left-bc right-bc)]
                [t-new (+ t dt)])
            (loop t-new u-new (+ step 1)
                  (cons (cons t-new (vector-copy u-new)) results)))))))

;;; ============================================================
;;; Section 7: Wave Equation Solver
;;; ============================================================
;;;
;;; ∂²u/∂t² = c² ∇²u
;;; Requires tracking both u and ∂u/∂t (or u at two time levels)
;;; Stable when dt < h/c

(doc 'section 'wave-equation)

(doc wave-step-explicit-1d 'export #t)
(doc wave-step-explicit-1d 'type '(-> Vector Vector Number Number Number Number Number Vector))
(doc wave-step-explicit-1d 'description "One explicit step of 1D wave equation. u-prev is u^{n-1}, u is u^n, returns u^{n+1}")
(define (wave-step-explicit-1d u-prev u h dt c left-bc right-bc)
  (let* ([n (vector-length u)]
         [r2 (expt (/ (* c dt) h) 2)]  ; (c*dt/h)²
         [result (make-vector n 0.0)])
    ;; Boundary conditions
    (vector-set! result 0 left-bc)
    (vector-set! result (- n 1) right-bc)
    ;; Interior: u^{n+1} = 2*u^n - u^{n-1} + r²*(u_{i+1} - 2*u_i + u_{i-1})
    (do ([i 1 (+ i 1)])
        ((= i (- n 1)) result)
      (let ([lap (+ (vector-ref u (+ i 1))
                    (* -2.0 (vector-ref u i))
                    (vector-ref u (- i 1)))])
        (vector-set! result i
                     (+ (* 2.0 (vector-ref u i))
                        (- (vector-ref u-prev i))
                        (* r2 lap)))))))

(doc solve-wave-1d 'export #t)
(doc solve-wave-1d 'type '(-> Vector Vector Number Number Number Number Number Nat (List (Pair Number Vector))))
(doc solve-wave-1d 'description "Solve 1D wave equation. u0 is initial displacement, v0 is initial velocity.")
(define (solve-wave-1d u0 v0 h c t-end left-bc right-bc max-steps)
  (let* ([dt-cfl (* 0.9 (/ h c))]  ; Safe CFL
         [n-steps (min max-steps (ceiling (/ t-end dt-cfl)))]
         [dt (/ t-end n-steps)]
         ;; Initialize u^{-1} from velocity: u^{-1} = u^0 - dt * v^0
         [n (vector-length u0)]
         [u-prev (make-vector n 0.0)])
    ;; Set u-prev = u0 - dt * v0
    (do ([i 0 (+ i 1)])
        ((= i n))
      (vector-set! u-prev i (- (vector-ref u0 i) (* dt (vector-ref v0 i)))))
    ;; Apply BCs to u-prev
    (vector-set! u-prev 0 left-bc)
    (vector-set! u-prev (- n 1) right-bc)

    (let loop ([t 0.0]
               [u-p (vector-copy u-prev)]
               [u (vector-copy u0)]
               [step 0]
               [results (list (cons 0.0 (vector-copy u0)))])
      (if (>= step n-steps)
          (reverse results)
          (let ([u-new (wave-step-explicit-1d u-p u h dt c left-bc right-bc)]
                [t-new (+ t dt)])
            (loop t-new u u-new (+ step 1)
                  (cons (cons t-new (vector-copy u-new)) results)))))))

;;; ============================================================
;;; Section 8: Poisson/Laplace Solver
;;; ============================================================
;;;
;;; ∇²u = f (Poisson)
;;; ∇²u = 0 (Laplace)
;;; Solved as linear system: A u = b

(doc 'section 'poisson-laplace)

;;; Simple vector utilities (avoid circular deps)
;;; Defined before jacobi-solve which uses them
(define (vec-sub-simple v1 v2)
  (let* ([n (vector-length v1)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (- (vector-ref v1 i) (vector-ref v2 i))))))

(define (vec-norm-simple v)
  (let ([n (vector-length v)])
    (sqrt (let loop ([i 0] [sum 0.0])
            (if (= i n)
                sum
                (loop (+ i 1) (+ sum (expt (vector-ref v i) 2))))))))

;;; Jacobi iteration for Poisson (avoids dependency on pde-time)
;;; Defined before solve-poisson-1d which calls it
(define (jacobi-solve A b x0 tol max-iter)
  (let* ([n (vector-length b)]
         [row-ptrs (list-ref A 3)]
         [col-indices (list-ref A 4)]
         [vals (list-ref A 5)]  ; 'vals' not 'values' to avoid shadowing built-in
         [x (vector-copy x0)])
    (let loop ([iter 0])
      ;; Compute residual
      (let* ([Ax (sparse-csr-vec-mul A x)]
             [r (vec-sub-simple b Ax)]
             [r-norm (vec-norm-simple r)])
        (if (or (< r-norm tol) (>= iter max-iter))
            (values x r-norm iter)
            ;; Jacobi update: x_new[i] = (b[i] - sum_{j≠i} A[i,j]*x[j]) / A[i,i]
            (let ([x-new (make-vector n 0.0)])
              (do ([i 0 (+ i 1)])
                  ((= i n))
                (let ([start (vector-ref row-ptrs i)]
                      [end (vector-ref row-ptrs (+ i 1))]
                      [sum 0.0]
                      [diag 1.0])
                  (do ([k start (+ k 1)])
                      ((= k end))
                    (let ([j (vector-ref col-indices k)]
                          [v (vector-ref vals k)])
                      (if (= j i)
                          (set! diag v)
                          (set! sum (+ sum (* v (vector-ref x j)))))))
                  (vector-set! x-new i (/ (- (vector-ref b i) sum) diag))))
              (do ([i 0 (+ i 1)])
                  ((= i n))
                (vector-set! x i (vector-ref x-new i)))
              (loop (+ iter 1))))))))

(doc solve-poisson-1d 'export #t)
(doc solve-poisson-1d 'type '(-> Vector Number Number Number Number Number (Values Vector Number Nat)))
(doc solve-poisson-1d 'description "Solve 1D Poisson: u'' = f with Dirichlet BCs. Returns (solution, residual, iterations)")
(define (solve-poisson-1d f h left-bc right-bc tol max-iter)
  (let* ([n (vector-length f)]
         ;; Build Laplacian matrix for interior points
         [n-interior (- n 2)]
         [A (laplacian-matrix-1d n-interior h)]
         ;; Build RHS: f at interior points, adjusted for BCs
         [h2 (/ 1.0 (* h h))]
         [rhs (make-vector n-interior 0.0)])
    ;; Copy f to RHS and adjust for boundary conditions
    (do ([i 0 (+ i 1)])
        ((= i n-interior))
      (let ([f-val (vector-ref f (+ i 1))])  ; f at interior point
        ;; Adjust first and last for Dirichlet BCs
        (vector-set! rhs i
                     (- f-val
                        (if (= i 0) (* h2 left-bc) 0.0)
                        (if (= i (- n-interior 1)) (* h2 right-bc) 0.0)))))
    ;; Initial guess
    (let ([x0 (make-vector n-interior 0.0)])
      ;; Solve using CG (from pde-time or we implement simple version here)
      (let-values ([(sol residual iters) (jacobi-solve A rhs x0 tol max-iter)])
        ;; Assemble full solution with BCs
        (let ([result (make-vector n 0.0)])
          (vector-set! result 0 left-bc)
          (vector-set! result (- n 1) right-bc)
          (do ([i 0 (+ i 1)])
              ((= i n-interior))
            (vector-set! result (+ i 1) (vector-ref sol i)))
          (values result residual iters))))))


(printf "finite-diff.ss loaded\n")
(printf "  Operators: diff-forward, diff-backward, diff-central, diff2-central\n")
(printf "  Stencils: stencil-d1-*, stencil-d2-*, apply-stencil-1d\n")
(printf "  2D: laplacian-2d, grid-index\n")
(printf "  BCs: apply-dirichlet-1d, apply-neumann-1d, apply-periodic-1d\n")
(printf "  Matrices: laplacian-matrix-1d, laplacian-matrix-2d\n")
(printf "  Solvers: solve-heat-1d, solve-wave-1d, solve-poisson-1d\n")
