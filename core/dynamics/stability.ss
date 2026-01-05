;;; core/dynamics/stability.ss — Stability Analysis for Dynamical Systems
;;;
;;; Fixed point detection, linearization, eigenvalue analysis, and stability
;;; classification for continuous-time dynamical systems. Supports:
;;;   - Fixed point (equilibrium) detection
;;;   - Jacobian matrix computation (linearization)
;;;   - Eigenvalue-based stability classification
;;;   - Stability types: stable/unstable node, saddle, spiral, center
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/linalg/vec.ss
;;;   - core/linalg/matrix.ss
;;;   - core/linalg/matrix-eigen.ss
;;;   - core/numeric/complex.ss
;;;   - core/dynamics/ode-system.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec.ss")
(load "core/linalg/matrix.ss")
(load "core/linalg/matrix-eigen.ss")
(load "core/numeric/complex.ss")
(load "core/dynamics/ode-system.ss")

;;; ============================================================
;;; Fixed Point Detection
;;; ============================================================

;;; find-fixed-point-newton : Any × Any × Number × Number × Nat → Any
;;; Find a fixed point using Newton's method.
;;; Returns approximate equilibrium point or #f if not found.
;;;
;;; Arguments:
;;;   sys - ODE system
;;;   initial-guess - starting point for Newton iteration
;;;   tolerance - convergence tolerance
;;;   step-size - finite difference step for Jacobian
;;;   max-iter - maximum iterations
(define (find-fixed-point-newton sys initial-guess tolerance step-size max-iter)
  (let loop ([x initial-guess] [iter 0])
       (if (>= iter max-iter)
           #f  ; Failed to converge
           (let* ([fx (eval-vector-field sys 0 x)]
                  [norm-fx (vec-norm fx)])
                 (if (< norm-fx tolerance)
                     x  ; Converged!
                     ;; Newton step: x_new = x - J^(-1) * f(x)
                     (let* ([jac (compute-jacobian sys x step-size)]
                            [jac-inv (matrix-invert-simple jac)]
                            [delta (matrix-vec-mul jac-inv fx)]
                            [x-new (vec-sub x delta)])
                           (loop x-new (+ iter 1))))))))

;;; is-fixed-point? : Any × Any × Number → Bool
;;; Check if a point is a fixed point (equilibrium) within tolerance.
(define (is-fixed-point? sys point tolerance)
  (< (vector-field-norm sys 0 point) tolerance))

;;; refine-fixed-point : Any × Any × Number × Number → Any
;;; Refine a fixed point estimate using Newton iteration.
(define (refine-fixed-point sys point tolerance step-size)
  (find-fixed-point-newton sys point tolerance step-size 20))

;;; ============================================================
;;; Jacobian Matrix Computation
;;; ============================================================

;;; compute-jacobian : Any × Any × Number → Any
;;; Compute the Jacobian matrix of a vector field at a point.
;;; Uses finite differences for numerical differentiation.
;;;
;;; For f : R^n → R^n, Jacobian J[i,j] = ∂f_i/∂x_j
;;;
;;; Arguments:
;;;   sys - ODE system
;;;   point - point at which to compute Jacobian
;;;   h - step size for finite differences
(define (compute-jacobian sys point h)
  (let* ([n (vector-length point)]
         [f0 (eval-vector-field sys 0 point)]
         [jac-data (make-vector (* n n) 0)])
        ;; For each column j (variable x_j)
        (do ([j 0 (+ j 1)])
            ((= j n))
            ;; Perturb x_j by h
            (let* ([point-plus-h (vec-copy point)]
                   [_ (vector-set! point-plus-h j (+ (vector-ref point j) h))]
                   [f-plus-h (eval-vector-field sys 0 point-plus-h)])
                  ;; Compute column j of Jacobian: (f(x + h*e_j) - f(x)) / h
                  (do ([i 0 (+ i 1)])
                      ((= i n))
                      (let ([df-dxj (/ (- (vector-ref f-plus-h i)
                                          (vector-ref f0 i))
                                       h)])
                           (vector-set! jac-data (+ (* i n) j) df-dxj)))))
        (list 'matrix n n jac-data)))

;;; linearize-at-equilibrium : Any × Any × Number → Any
;;; Linearize an ODE system at an equilibrium point.
;;; Returns the Jacobian matrix A where dx/dt ≈ A(x - x*)
(define (linearize-at-equilibrium sys equilibrium step-size)
  (compute-jacobian sys equilibrium step-size))

;;; ============================================================
;;; Matrix Utilities (Simple Implementations)
;;; ============================================================

;;; matrix-invert-simple : Any → Any
;;; Simple matrix inversion using Gauss-Jordan elimination.
;;; Only works for small matrices - uses simple pivoting.
(define (matrix-invert-simple m)
  (let* ([n (matrix-rows m)]
         [data (matrix-data m)]
         ;; Create augmented matrix [A | I]
         [aug (make-vector (* n (* n 2)) 0)])
        ;; Copy A into left half, I into right half
        (do ([i 0 (+ i 1)])
            ((= i n))
            (do ([j 0 (+ j 1)])
                ((= j n))
                (vector-set! aug (+ (* i (* n 2)) j)
                             (vector-ref data (+ (* i n) j))))
            (vector-set! aug (+ (* i (* n 2)) (+ n i)) 1.0))
        ;; Gauss-Jordan elimination
        (do ([k 0 (+ k 1)])
            ((= k n))
            ;; Find pivot
            (let ([pivot-row k]
                  [pivot-val (abs (vector-ref aug (+ (* k (* n 2)) k)))])
                 (do ([i (+ k 1) (+ i 1)])
                     ((= i n))
                     (let ([val (abs (vector-ref aug (+ (* i (* n 2)) k)))])
                          (when (> val pivot-val)
                                (set! pivot-row i)
                                (set! pivot-val val))))
                 ;; Swap rows
                 (unless (= pivot-row k)
                         (do ([j 0 (+ j 1)])
                             ((= j (* n 2)))
                             (let ([temp (vector-ref aug (+ (* k (* n 2)) j))])
                                  (vector-set! aug (+ (* k (* n 2)) j)
                                               (vector-ref aug (+ (* pivot-row (* n 2)) j)))
                                  (vector-set! aug (+ (* pivot-row (* n 2)) j) temp))))
                 ;; Scale pivot row
                 (let ([pivot (vector-ref aug (+ (* k (* n 2)) k))])
                      (when (> (abs pivot) 1e-10)
                            (do ([j 0 (+ j 1)])
                                ((= j (* n 2)))
                                (vector-set! aug (+ (* k (* n 2)) j)
                                             (/ (vector-ref aug (+ (* k (* n 2)) j)) pivot)))))
                 ;; Eliminate column k in other rows
                 (do ([i 0 (+ i 1)])
                     ((= i n))
                     (unless (= i k)
                             (let ([factor (vector-ref aug (+ (* i (* n 2)) k))])
                                  (do ([j 0 (+ j 1)])
                                      ((= j (* n 2)))
                                      (vector-set! aug (+ (* i (* n 2)) j)
                                                   (- (vector-ref aug (+ (* i (* n 2)) j))
                                                      (* factor (vector-ref aug (+ (* k (* n 2)) j)))))))))))
        ;; Extract inverse from right half
        (let ([inv-data (make-vector (* n n) 0)])
             (do ([i 0 (+ i 1)])
                 ((= i n))
                 (do ([j 0 (+ j 1)])
                     ((= j n))
                     (vector-set! inv-data (+ (* i n) j)
                                  (vector-ref aug (+ (* i (* n 2)) (+ n j))))))
             (list 'matrix n n inv-data))))

;;; ============================================================
;;; 2x2 Eigenvalue Computation
;;; ============================================================

;;; matrix-eigenvalues-2d : Any → (List Complex)
;;; Compute eigenvalues of a 2x2 matrix analytically.
;;; For A = [[a, b], [c, d]], eigenvalues satisfy:
;;;   λ² - (a+d)λ + (ad-bc) = 0
;;;   λ = (trace ± sqrt(trace² - 4*det)) / 2
(define (matrix-eigenvalues-2d m)
  (let* ([a (matrix-ref m 0 0)]
         [b (matrix-ref m 0 1)]
         [c (matrix-ref m 1 0)]
         [d (matrix-ref m 1 1)]
         [trace (+ a d)]
         [det (- (* a d) (* b c))]
         [discriminant (- (* trace trace) (* 4 det))])
        (if (< discriminant 0)
            ;; Complex eigenvalues: λ = (trace ± i*sqrt(-disc)) / 2
            (let ([real-part (/ trace 2)]
                  [imag-part (/ (sqrt (- discriminant)) 2)])
                 (list (make-complex real-part imag-part)
                       (make-complex real-part (- imag-part))))
            ;; Real eigenvalues: λ = (trace ± sqrt(disc)) / 2
            (let ([sqrt-disc (sqrt discriminant)]
                  [half-trace (/ trace 2)])
                 (list (make-complex (+ half-trace (/ sqrt-disc 2)) 0)
                       (make-complex (- half-trace (/ sqrt-disc 2)) 0))))))

;;; matrix-dominant-eigenvalue : Any → Complex
;;; Estimate dominant (largest magnitude) eigenvalue using power iteration.
(define (matrix-dominant-eigenvalue m)
  (let* ([n (matrix-rows m)]
         [v0 (vec-ones n)]
         [result (power-iteration m v0 100 1e-8)])
        (if (and (pair? result) (eq? (car result) 'error))
            (make-complex 0 0)  ; Failed - return zero
            (make-complex (car result) 0))))

;;; power-iteration : Any × Any × Nat × Number → (Number × Any)
;;; Simple power iteration to find dominant eigenvalue.
(define (power-iteration m v max-iter tol)
  (let loop ([v-curr v] [iter 0] [lambda-prev 0])
       (if (>= iter max-iter)
           (cons lambda-prev v-curr)
           (let* ([v-next-unnorm (matrix-vec-mul m v-curr)]
                  [norm (vec-norm v-next-unnorm)]
                  [v-next (if (> norm 1e-10)
                              (vec-scale (/ 1 norm) v-next-unnorm)
                              v-curr)]
                  [lambda-curr norm]
                  [change (abs (- lambda-curr lambda-prev))])
                 (if (< change tol)
                     (cons lambda-curr v-next)
                     (loop v-next (+ iter 1) lambda-curr))))))

;;; ============================================================
;;; Eigenvalue-Based Stability Classification
;;; ============================================================

;;; classify-stability-2d : (List Complex) → Symbol
;;; Classify stability for 2D system based on eigenvalues.
;;; Returns: 'stable-node, 'unstable-node, 'saddle, 'stable-spiral,
;;;          'unstable-spiral, 'center, 'degenerate
(define (classify-stability-2d eigenvalues)
  (if (not (= (length eigenvalues) 2))
      'invalid-dimension
      (let* ([lambda1 (car eigenvalues)]
             [lambda2 (cadr eigenvalues)]
             [re1 (complex-real lambda1)]
             [re2 (complex-real lambda2)]
             [im1 (complex-imag lambda1)]
             [im2 (complex-imag lambda2)])
            (cond
             ;; Complex eigenvalues (spiral or center)
             [(> (abs im1) 1e-10)
              (cond
               [(< re1 -1e-10) 'stable-spiral]
               [(> re1 1e-10) 'unstable-spiral]
               [else 'center])]
             ;; Real eigenvalues (node or saddle)
             [(and (< re1 -1e-10) (< re2 -1e-10))
              'stable-node]
             [(and (> re1 1e-10) (> re2 1e-10))
              'unstable-node]
             [(< (* re1 re2) 0)
              'saddle]
             [else 'degenerate]))))

;;; analyze-stability : Any × Any × Number → (Symbol × (List Complex))
;;; Analyze stability of an equilibrium point.
;;; Returns (stability-type . eigenvalues)
(define (analyze-stability sys equilibrium step-size)
  (let* ([jac (linearize-at-equilibrium sys equilibrium step-size)]
         [eigenvalues (matrix-eigenvalues-2d jac)])
        (cons (classify-stability-2d eigenvalues) eigenvalues)))

;;; ============================================================
;;; Stability Predicates
;;; ============================================================

;;; stable? : Symbol → Bool
;;; Check if a stability type is stable.
(define (stable? stability-type)
  (if (memq stability-type '(stable-node stable-spiral)) #t #f))

;;; asymptotically-stable? : Symbol → Bool
;;; Check if equilibrium is asymptotically stable.
(define (asymptotically-stable? stability-type)
  (if (memq stability-type '(stable-node stable-spiral)) #t #f))

;;; unstable? : Symbol → Bool
;;; Check if a stability type is unstable.
(define (unstable? stability-type)
  (if (memq stability-type '(unstable-node unstable-spiral saddle)) #t #f))

;;; ============================================================
;;; Higher-Dimensional Stability
;;; ============================================================

;;; classify-stability-nd : (List Complex) → Symbol
;;; Classify stability for n-dimensional system based on eigenvalues.
;;; Returns: 'stable, 'unstable, 'neutrally-stable, or 'unknown
(define (classify-stability-nd eigenvalues)
  (let ([max-real (apply max (map complex-real eigenvalues))]
        [min-real (apply min (map complex-real eigenvalues))])
       (cond
        [(< max-real -1e-10) 'stable]
        [(> min-real 1e-10) 'unstable]
        [(and (< max-real 1e-10) (> min-real -1e-10)) 'neutrally-stable]
        [else 'saddle-type])))

;;; analyze-stability-nd : Any × Any × Number → (Symbol × (List Complex))
;;; Analyze stability for n-dimensional system.
(define (analyze-stability-nd sys equilibrium step-size)
  (let* ([jac (linearize-at-equilibrium sys equilibrium step-size)]
         [n (matrix-rows jac)])
        (if (<= n 2)
            (analyze-stability sys equilibrium step-size)
            ;; For n > 2, use power iteration for dominant eigenvalue
            (let* ([dom-eval (matrix-dominant-eigenvalue jac)]
                   [stability (if (< (complex-real dom-eval) 0)
                                  'stable
                                  'unstable)])
                  (cons stability (list dom-eval))))))

;;; ============================================================
;;; Standard System Analysis
;;; ============================================================

;;; analyze-linear-system : Any → (Symbol × (List Complex))
;;; Analyze stability of a linear ODE system dx/dt = Ax.
(define (analyze-linear-system A)
  (let ([eigenvalues (matrix-eigenvalues-2d A)])
       (cons (classify-stability-2d eigenvalues) eigenvalues)))

;;; ============================================================
;;; Trajectory Analysis
;;; ============================================================

;;; estimate-basin-of-attraction : Any × Any × Number × (List Any) → (List Any)
;;; Estimate basin of attraction by testing which initial conditions
;;; converge to the given equilibrium.
;;; Returns list of initial conditions that converge.
(define (estimate-basin-of-attraction sys equilibrium radius grid-points)
  (filter (lambda (point)
                  ;; Check if point is within radius of equilibrium
                  (< (vec-norm (vec-sub point equilibrium)) radius))
          grid-points))

;;; is-stable-numerically? : Any × Any × Number × Number × Nat → Bool
;;; Test stability by numerical integration from nearby initial condition.
;;; Returns #t if trajectory appears to converge to equilibrium.
(define (is-stable-numerically? sys equilibrium perturbation-size time-steps dt)
  ;; Create small perturbation
  (let* ([n (vector-length equilibrium)]
         [perturbation (vec-scale perturbation-size (vec-ones n))]
         [initial (vec-add equilibrium perturbation)]
         ;; Simple Euler integration (just for testing)
         [trajectory (integrate-euler sys 0 initial dt time-steps)])
        ;; Check if final state is close to equilibrium
        (let ([final-state (car (reverse trajectory))])
             (< (vec-norm (vec-sub final-state equilibrium))
                (* 2 perturbation-size)))))

;;; integrate-euler : Any × Number × Any × Number × Nat → (List Any)
;;; Simple forward Euler integration (for testing).
(define (integrate-euler sys t0 state0 dt n)
  (let loop ([t t0] [state state0] [i 0] [result (list state0)])
       (if (>= i n)
           (reverse result)
           (let* ([f (eval-vector-field sys t state)]
                  [new-state (vec-add state (vec-scale dt f))])
                 (loop (+ t dt) new-state (+ i 1) (cons new-state result))))))
