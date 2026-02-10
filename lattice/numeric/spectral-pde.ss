;;; lattice/numeric/spectral-pde.ss — Spectral Methods for PDEs
;;; @module spectral-pde
;;; @requires prelude linalg/vec linalg/matrix numeric/complex numeric/dft

(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'complex)
(require 'dft)

(doc 'module 'spectral-pde)
(doc 'description "Spectral methods for PDEs: Fourier spectral (periodic), Chebyshev spectral (non-periodic), pseudospectral collocation")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'see-also '(finite-diff fem pde-time dft))

;;; ============================================================
;;; Section 1: Fourier Spectral Differentiation
;;; ============================================================
;;;
;;; For periodic functions on [0, L], the Fourier series is:
;;;   u(x) = Σ û_k e^{ikx·2π/L}
;;;
;;; Differentiation in Fourier space:
;;;   d/dx → ik·2π/L  (multiply by wavenumber)
;;;   d²/dx² → -k²·(2π/L)²
;;;
;;; This gives spectral accuracy: errors decay exponentially for smooth functions.

(doc 'section 'fourier-spectral)

(doc fourier-wavenumbers 'export #t)
(doc fourier-wavenumbers 'type '(-> Nat Number Vector))
(doc fourier-wavenumbers 'description "Compute wavenumbers for Fourier spectral differentiation. For N points on [0, L), returns k = [0, 1, ..., N/2-1, -N/2, ..., -1] × 2π/L")
(define (fourier-wavenumbers n L)
  (let* ([k (make-vector n 0.0)]
         [factor (/ (* 2.0 (pi-value)) L)]
         [half (quotient n 2)])
    ;; Positive frequencies: 0, 1, 2, ..., N/2-1
    (do ([i 0 (+ i 1)])
        ((= i half))
      (vector-set! k i (* i factor)))
    ;; Negative frequencies: -N/2, ..., -1
    (do ([i half (+ i 1)])
        ((= i n) k)
      (vector-set! k i (* (- i n) factor)))))

(doc fourier-diff 'export #t)
(doc fourier-diff 'type '(-> Vector Number Vector))
(doc fourier-diff 'description "Compute first derivative using Fourier spectral method. u is real-valued periodic on [0, L)")
(define (fourier-diff u L)
  (let* ([n (vector-length u)]
         [k (fourier-wavenumbers n L)]
         [u-hat (dft-real u)]
         [du-hat (make-vector n (complex-zero))])
    ;; Multiply by ik in Fourier space
    (do ([j 0 (+ j 1)])
        ((= j n))
      (let* ([kj (vector-ref k j)]
             [uj (vector-ref u-hat j)]
             ;; i * k * û = (0 + i·k) * (a + ib) = -kb + i·ka
             [re (complex-real uj)]
             [im (complex-imag uj)])
        (vector-set! du-hat j (make-complex (* (- kj) im) (* kj re)))))
    ;; Transform back
    (complex->real-vec (idft du-hat))))

(doc fourier-diff2 'export #t)
(doc fourier-diff2 'type '(-> Vector Number Vector))
(doc fourier-diff2 'description "Compute second derivative using Fourier spectral method")
(define (fourier-diff2 u L)
  (let* ([n (vector-length u)]
         [k (fourier-wavenumbers n L)]
         [u-hat (dft-real u)]
         [d2u-hat (make-vector n (complex-zero))])
    ;; Multiply by -k² in Fourier space
    (do ([j 0 (+ j 1)])
        ((= j n))
      (let* ([kj (vector-ref k j)]
             [k2 (* kj kj)]
             [uj (vector-ref u-hat j)])
        (vector-set! d2u-hat j (complex-scale (- k2) uj))))
    ;; Transform back
    (complex->real-vec (idft d2u-hat))))

(doc fourier-laplacian 'export #t)
(doc fourier-laplacian 'type '(-> Vector Number Vector))
(doc fourier-laplacian 'description "Alias for fourier-diff2 (Laplacian in 1D)")
(define fourier-laplacian fourier-diff2)

;;; ============================================================
;;; Section 2: Fourier Spectral PDE Solvers
;;; ============================================================
;;;
;;; Heat equation: ∂u/∂t = α·∂²u/∂x²
;;; Advection equation: ∂u/∂t + c·∂u/∂x = 0
;;;
;;; In Fourier space, these become ODEs:
;;;   dû_k/dt = -α·k²·û_k  (heat)
;;;   dû_k/dt = -i·c·k·û_k  (advection)

(doc 'section 'fourier-pde-solvers)

(doc fourier-heat-rhs 'export #t)
(doc fourier-heat-rhs 'type '(-> Vector Number Number Vector))
(doc fourier-heat-rhs 'description "Compute RHS of heat equation: α·∂²u/∂x². For use with time steppers.")
(define (fourier-heat-rhs u L alpha)
  (let ([d2u (fourier-diff2 u L)])
    (let* ([n (vector-length d2u)]
           [result (make-vector n 0.0)])
      (do ([i 0 (+ i 1)])
          ((= i n) result)
        (vector-set! result i (* alpha (vector-ref d2u i)))))))

(doc fourier-advection-rhs 'export #t)
(doc fourier-advection-rhs 'type '(-> Vector Number Number Vector))
(doc fourier-advection-rhs 'description "Compute RHS of advection equation: -c·∂u/∂x. For use with time steppers.")
(define (fourier-advection-rhs u L c)
  (let ([du (fourier-diff u L)])
    (let* ([n (vector-length du)]
           [result (make-vector n 0.0)])
      (do ([i 0 (+ i 1)])
          ((= i n) result)
        (vector-set! result i (* (- c) (vector-ref du i)))))))

;;; Exponential integrator for heat equation (exact in Fourier space)
(doc fourier-heat-exact-step 'export #t)
(doc fourier-heat-exact-step 'type '(-> Vector Number Number Number Vector))
(doc fourier-heat-exact-step 'description "Exact time step for periodic heat equation using exponential integrator. û(t+dt) = û(t)·e^{-α·k²·dt}")
(define (fourier-heat-exact-step u L alpha dt)
  (let* ([n (vector-length u)]
         [k (fourier-wavenumbers n L)]
         [u-hat (dft-real u)]
         [u-hat-new (make-vector n (complex-zero))])
    ;; Apply exponential decay in Fourier space
    (do ([j 0 (+ j 1)])
        ((= j n))
      (let* ([kj (vector-ref k j)]
             [decay (exp (* (- alpha) (* kj kj) dt))]
             [uj (vector-ref u-hat j)])
        (vector-set! u-hat-new j (complex-scale decay uj))))
    (complex->real-vec (idft u-hat-new))))

;;; Exact solver for advection (phase shift in Fourier space)
(doc fourier-advection-exact-step 'export #t)
(doc fourier-advection-exact-step 'type '(-> Vector Number Number Number Vector))
(doc fourier-advection-exact-step 'description "Exact time step for periodic advection equation. Solution translates by c·dt.")
(define (fourier-advection-exact-step u L c dt)
  (let* ([n (vector-length u)]
         [k (fourier-wavenumbers n L)]
         [u-hat (dft-real u)]
         [u-hat-new (make-vector n (complex-zero))])
    ;; Apply phase shift: e^{-i·c·k·dt}
    (do ([j 0 (+ j 1)])
        ((= j n))
      (let* ([kj (vector-ref k j)]
             [phase (- (* c kj dt))]
             [shift (make-polar 1.0 phase)]
             [uj (vector-ref u-hat j)])
        (vector-set! u-hat-new j (complex-mul uj shift))))
    (complex->real-vec (idft u-hat-new))))

;;; Integration driver
(doc fourier-integrate 'export #t)
(doc fourier-integrate 'type '(-> (-> Vector Vector) Vector Number Number Nat (List (Pair Number Vector))))
(doc fourier-integrate 'description "Integrate Fourier spectral PDE. rhs-fn takes u and returns du/dt.")
(define (fourier-integrate rhs-fn u0 t-end dt max-steps)
  (let ([n-steps (min max-steps (inexact->exact (ceiling (/ t-end dt))))])
    (let loop ([t 0.0]
               [u (vector-copy u0)]
               [step 0]
               [results (list (cons 0.0 (vector-copy u0)))])
      (if (>= step n-steps)
          (reverse results)
          (let* ([dt-use (min dt (- t-end t))]
                 ;; RK4 step
                 [k1 (rhs-fn u)]
                 [u1 (vec-madd-simple u 0.5 dt-use k1)]
                 [k2 (rhs-fn u1)]
                 [u2 (vec-madd-simple u 0.5 dt-use k2)]
                 [k3 (rhs-fn u2)]
                 [u3 (vec-madd-simple u 1.0 dt-use k3)]
                 [k4 (rhs-fn u3)]
                 ;; u_new = u + dt/6 * (k1 + 2*k2 + 2*k3 + k4)
                 [u-new (rk4-combine u k1 k2 k3 k4 dt-use)]
                 [t-new (+ t dt-use)])
            (loop t-new u-new (+ step 1)
                  (cons (cons t-new (vector-copy u-new)) results)))))))

;;; Helper functions for integration
(define (vec-madd-simple u s dt k)
  (let* ([n (vector-length u)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (+ (vector-ref u i)
                               (* s dt (vector-ref k i)))))))

(define (rk4-combine u k1 k2 k3 k4 dt)
  (let* ([n (vector-length u)]
         [result (make-vector n 0.0)]
         [dt6 (/ dt 6.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i
                   (+ (vector-ref u i)
                      (* dt6 (+ (vector-ref k1 i)
                                (* 2.0 (vector-ref k2 i))
                                (* 2.0 (vector-ref k3 i))
                                (vector-ref k4 i))))))))

;;; ============================================================
;;; Section 3: Chebyshev Polynomials and Nodes
;;; ============================================================
;;;
;;; Chebyshev polynomials T_n(x) = cos(n·arccos(x)) for x ∈ [-1, 1]
;;; They cluster near boundaries, which is ideal for non-periodic BCs.
;;;
;;; Gauss-Lobatto nodes: x_j = cos(πj/N) for j = 0, 1, ..., N
;;; These include the endpoints ±1.

(doc 'section 'chebyshev)

(doc chebyshev-nodes 'export #t)
(doc chebyshev-nodes 'type '(-> Nat Vector))
(doc chebyshev-nodes 'description "Compute Chebyshev-Gauss-Lobatto nodes: x_j = cos(πj/N) for j = 0, ..., N. Returns N+1 points on [-1, 1] including endpoints.")
(define (chebyshev-nodes n)
  (let* ([nodes (make-vector (+ n 1) 0.0)]
         [pi-val (pi-value)])
    (do ([j 0 (+ j 1)])
        ((> j n) nodes)
      (vector-set! nodes j (cos (/ (* pi-val j) n))))))

(doc chebyshev-nodes-interval 'export #t)
(doc chebyshev-nodes-interval 'type '(-> Nat Number Number Vector))
(doc chebyshev-nodes-interval 'description "Chebyshev nodes mapped to interval [a, b]")
(define (chebyshev-nodes-interval n a b)
  (let* ([std-nodes (chebyshev-nodes n)]
         [mapped (make-vector (+ n 1) 0.0)]
         [mid (/ (+ a b) 2.0)]
         [half-width (/ (- b a) 2.0)])
    (do ([j 0 (+ j 1)])
        ((> j n) mapped)
      (let ([x-std (vector-ref std-nodes j)])
        (vector-set! mapped j (+ mid (* half-width x-std)))))))

(doc chebyshev-poly 'export #t)
(doc chebyshev-poly 'type '(-> Nat Number Number))
(doc chebyshev-poly 'description "Evaluate Chebyshev polynomial T_n(x)")
(define (chebyshev-poly n x)
  (cond
    [(= n 0) 1.0]
    [(= n 1) x]
    [else
     ;; Recurrence: T_{n+1}(x) = 2x·T_n(x) - T_{n-1}(x)
     (let loop ([k 2] [t-prev 1.0] [t-curr x])
       (if (> k n)
           t-curr
           (let ([t-next (- (* 2.0 x t-curr) t-prev)])
             (loop (+ k 1) t-curr t-next))))]))

;;; ============================================================
;;; Section 4: Chebyshev Differentiation Matrix
;;; ============================================================
;;;
;;; The Chebyshev differentiation matrix D maps function values at
;;; Gauss-Lobatto nodes to derivative values at the same nodes.
;;;
;;; D[i,j] = c_i/c_j · (-1)^{i+j} / (x_i - x_j)  for i ≠ j
;;; D[i,i] = -x_i / (2(1-x_i²))  for interior points
;;; D[0,0] = (2N² + 1)/6,  D[N,N] = -(2N² + 1)/6

(doc 'section 'chebyshev-diff)

(doc chebyshev-diff-matrix 'export #t)
(doc chebyshev-diff-matrix 'type '(-> Nat Matrix))
(doc chebyshev-diff-matrix 'description "Compute (N+1)×(N+1) Chebyshev differentiation matrix for N+1 Gauss-Lobatto nodes")
(define (chebyshev-diff-matrix n)
  (let* ([size (+ n 1)]
         [x (chebyshev-nodes n)]
         ;; Coefficient c_j: c_0 = c_N = 2, c_j = 1 otherwise
         [c (make-vector size 1.0)]
         ;; Build data as flat vector (row-major)
         [data (make-vector (* size size) 0.0)])
    (vector-set! c 0 2.0)
    (vector-set! c n 2.0)

    ;; Fill all entries
    (do ([i 0 (+ i 1)])
        ((= i size))
      (do ([j 0 (+ j 1)])
          ((= j size))
        (let ([idx (+ (* i size) j)])
          (cond
            ;; Diagonal corner: D[0,0] = (2N² + 1)/6
            [(and (= i 0) (= j 0))
             (vector-set! data idx (/ (+ (* 2.0 n n) 1.0) 6.0))]
            ;; Diagonal corner: D[N,N] = -(2N² + 1)/6
            [(and (= i n) (= j n))
             (vector-set! data idx (- (/ (+ (* 2.0 n n) 1.0) 6.0)))]
            ;; Interior diagonal: D[i,i] = -x_i / (2(1-x_i²))
            [(= i j)
             (let* ([xi (vector-ref x i)]
                    [denom (* 2.0 (- 1.0 (* xi xi)))])
               (vector-set! data idx (- (/ xi denom))))]
            ;; Off-diagonal: D[i,j] = c_i/c_j · (-1)^{i+j} / (x_i - x_j)
            [else
             (let* ([xi (vector-ref x i)]
                    [xj (vector-ref x j)]
                    [ci (vector-ref c i)]
                    [cj (vector-ref c j)]
                    [sign (if (even? (+ i j)) 1.0 -1.0)]
                    [val (/ (* sign ci) (* cj (- xi xj)))])
               (vector-set! data idx val))]))))

    ;; Return matrix structure
    (list 'matrix size size data)))

(doc chebyshev-diff-matrix-interval 'export #t)
(doc chebyshev-diff-matrix-interval 'type '(-> Nat Number Number Matrix))
(doc chebyshev-diff-matrix-interval 'description "Chebyshev diff matrix for interval [a, b]. Scales by 2/(b-a).")
(define (chebyshev-diff-matrix-interval n a b)
  (let ([D (chebyshev-diff-matrix n)]
        [scale (/ 2.0 (- b a))])
    (matrix-scale scale D)))

(doc chebyshev-diff2-matrix 'export #t)
(doc chebyshev-diff2-matrix 'type '(-> Nat Matrix))
(doc chebyshev-diff2-matrix 'description "Second derivative matrix: D² = D·D")
(define (chebyshev-diff2-matrix n)
  (let ([D (chebyshev-diff-matrix n)])
    (matrix-mul D D)))

;;; ============================================================
;;; Section 5: Chebyshev Spectral Differentiation
;;; ============================================================
;;;
;;; Apply the differentiation matrix to compute derivatives.

(doc 'section 'chebyshev-spectral-diff)

(doc chebyshev-diff 'export #t)
(doc chebyshev-diff 'type '(-> Vector Matrix Vector))
(doc chebyshev-diff 'description "Compute derivative using Chebyshev differentiation matrix: du = D·u")
(define (chebyshev-diff u D)
  (matrix-vec-mul D u))

(doc chebyshev-diff2 'export #t)
(doc chebyshev-diff2 'type '(-> Vector Matrix Vector))
(doc chebyshev-diff2 'description "Compute second derivative using D²·u")
(define (chebyshev-diff2 u D2)
  (matrix-vec-mul D2 u))

;;; ============================================================
;;; Section 6: Chebyshev Collocation for BVPs
;;; ============================================================
;;;
;;; For boundary value problems like u'' = f with u(±1) = 0:
;;; 1. Discretize at Gauss-Lobatto nodes
;;; 2. Replace differential equation with D² u = f
;;; 3. Apply boundary conditions
;;; 4. Solve linear system

(doc 'section 'chebyshev-bvp)

(doc chebyshev-poisson-1d 'export #t)
(doc chebyshev-poisson-1d 'type '(-> (-> Number Number) Number Number Nat Vector))
(doc chebyshev-poisson-1d 'description "Solve u'' = f(x) on [-1, 1] with Dirichlet BCs u(-1) = ua, u(1) = ub using Chebyshev collocation")
(define (chebyshev-poisson-1d f ua ub n)
  (let* ([x (chebyshev-nodes n)]
         [D2 (chebyshev-diff2-matrix n)]
         [size (+ n 1)]
         ;; RHS: f evaluated at interior nodes
         [rhs (make-vector size 0.0)])

    ;; Set up RHS from source term at interior points
    (do ([i 1 (+ i 1)])
        ((= i n))
      (vector-set! rhs i (f (vector-ref x i))))

    ;; Modify for boundary conditions
    ;; Row 0: u(x_0) = ub (x_0 = 1 for standard ordering)
    ;; Row N: u(x_N) = ua (x_N = -1)

    ;; Extract interior subproblem
    ;; D2[1:N-1, 1:N-1] u[1:N-1] = rhs[1:N-1] - D2[1:N-1, 0]*ub - D2[1:N-1, N]*ua
    (let* ([interior-size (- n 1)]
           ;; Build interior matrix as flat data vector
           [A-data (make-vector (* interior-size interior-size) 0.0)]
           [b-vec (make-vector interior-size 0.0)])

      ;; Extract interior block of D2
      (do ([i 1 (+ i 1)])
          ((= i n))
        (do ([j 1 (+ j 1)])
            ((= j n))
          (let ([idx (+ (* (- i 1) interior-size) (- j 1))])
            (vector-set! A-data idx (matrix-ref D2 i j)))))

      ;; Create matrix from data
      (let ([A (list 'matrix interior-size interior-size A-data)])

        ;; Adjust RHS for boundary conditions
        (do ([i 1 (+ i 1)])
            ((= i n))
          (let ([rhs-i (- (vector-ref rhs i)
                          (* (matrix-ref D2 i 0) ub)    ; x_0 = +1
                          (* (matrix-ref D2 i n) ua))]) ; x_N = -1
            (vector-set! b-vec (- i 1) rhs-i)))

        ;; Solve using simple Gaussian elimination
        (let ([u-interior (matrix-solve A b-vec)])
          ;; Assemble full solution
          (let ([u (make-vector size 0.0)])
            (vector-set! u 0 ub)   ; x_0 = +1
            (vector-set! u n ua)   ; x_N = -1
            (do ([i 1 (+ i 1)])
                ((= i n) u)
              (vector-set! u i (vector-ref u-interior (- i 1))))))))))

;;; Simple Gaussian elimination for small systems
(define (matrix-solve A b)
  (let* ([n (vector-length b)]
         [augmented (make-vector n)]
         ;; Copy A and b into working storage
         [M (make-vector n)])
    ;; Initialize rows
    (do ([i 0 (+ i 1)])
        ((= i n))
      (let ([row (make-vector (+ n 1) 0.0)])
        (do ([j 0 (+ j 1)])
            ((= j n))
          (vector-set! row j (matrix-ref A i j)))
        (vector-set! row n (vector-ref b i))
        (vector-set! M i row)))

    ;; Forward elimination
    (do ([k 0 (+ k 1)])
        ((= k n))
      ;; Find pivot
      (let ([max-val (abs (vector-ref (vector-ref M k) k))]
            [max-row k])
        (do ([i (+ k 1) (+ i 1)])
            ((= i n))
          (let ([val (abs (vector-ref (vector-ref M i) k))])
            (when (> val max-val)
              (set! max-val val)
              (set! max-row i))))
        ;; Swap rows
        (unless (= max-row k)
          (let ([tmp (vector-ref M k)])
            (vector-set! M k (vector-ref M max-row))
            (vector-set! M max-row tmp)))
        ;; Eliminate
        (let ([pivot (vector-ref (vector-ref M k) k)])
          (when (> (abs pivot) 1e-15)
            (do ([i (+ k 1) (+ i 1)])
                ((= i n))
              (let* ([row-i (vector-ref M i)]
                     [row-k (vector-ref M k)]
                     [factor (/ (vector-ref row-i k) pivot)])
                (do ([j k (+ j 1)])
                    ((> j n))
                  (vector-set! row-i j
                               (- (vector-ref row-i j)
                                  (* factor (vector-ref row-k j)))))))))))

    ;; Back substitution
    (let ([x (make-vector n 0.0)])
      (do ([i (- n 1) (- i 1)])
          ((< i 0) x)
        (let* ([row (vector-ref M i)]
               [sum (vector-ref row n)])
          (do ([j (+ i 1) (+ j 1)])
              ((= j n))
            (set! sum (- sum (* (vector-ref row j) (vector-ref x j)))))
          (let ([diag (vector-ref row i)])
            (vector-set! x i (if (> (abs diag) 1e-15)
                                 (/ sum diag)
                                 0.0))))))))

;;; ============================================================
;;; Section 7: Grid and Convenience Functions
;;; ============================================================

(doc 'section 'convenience)

(doc periodic-grid 'export #t)
(doc periodic-grid 'type '(-> Nat Number Vector))
(doc periodic-grid 'description "Create uniform grid on [0, L) for periodic problems")
(define (periodic-grid n L)
  (let* ([h (/ L n)]
         [grid (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) grid)
      (vector-set! grid i (* i h)))))

(doc sample-function 'export #t)
(doc sample-function 'type '(-> (-> Number Number) Vector Vector))
(doc sample-function 'description "Sample function f at grid points")
(define (sample-function f grid)
  (let* ([n (vector-length grid)]
         [values (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) values)
      (vector-set! values i (f (vector-ref grid i))))))

(doc spectral-error-estimate 'export #t)
(doc spectral-error-estimate 'type '(-> Vector Vector Number))
(doc spectral-error-estimate 'description "Compute max-norm error between computed and exact solution")
(define (spectral-error-estimate computed exact)
  (let ([n (vector-length computed)]
        [max-err 0.0])
    (do ([i 0 (+ i 1)])
        ((= i n) max-err)
      (let ([err (abs (- (vector-ref computed i) (vector-ref exact i)))])
        (when (> err max-err)
          (set! max-err err))))))

(printf "spectral-pde.ss loaded\n")
(printf "  Fourier: fourier-diff, fourier-diff2, fourier-wavenumbers\n")
(printf "  Fourier PDE: fourier-heat-exact-step, fourier-advection-exact-step\n")
(printf "  Chebyshev: chebyshev-nodes, chebyshev-poly, chebyshev-diff-matrix\n")
(printf "  Collocation: chebyshev-poisson-1d\n")
(printf "  Utilities: periodic-grid, sample-function, spectral-error-estimate\n")
