(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/sim/dynamics/ode-system.ss")

(doc 'module 'chaos)
(doc 'description "Chaos detection and analysis: Lyapunov exponents, fractal dimension, strange attractors, Poincaré sections")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'features "RK4 integration, Lyapunov spectrum computation, Kaplan-Yorke dimension, Poincaré section extraction, strange attractor generation")

;;; ============================================================
;;; Section: RK4 Integration
;;; ============================================================

(doc 'section 'rk4-integration)
(doc 'note "Fourth-order Runge-Kutta method for accurate integration of chaotic systems")

(define (rk4-step sys t state dt)
  (doc 'type '(-> ODE Number Vec Number Vec))
  (doc 'description "Single RK4 integration step")
  (let* ([k1 (eval-vector-field sys t state)]
         [k2 (eval-vector-field sys
                                (+ t (/ dt 2))
                                (vec-add state (vec-scale (/ dt 2) k1)))]
         [k3 (eval-vector-field sys
                                (+ t (/ dt 2))
                                (vec-add state (vec-scale (/ dt 2) k2)))]
         [k4 (eval-vector-field sys
                                (+ t dt)
                                (vec-add state (vec-scale dt k3)))])
        (vec-add state
                 (vec-scale (/ dt 6)
                            (vec-add k1
                                     (vec-add (vec-scale 2 k2)
                                              (vec-add (vec-scale 2 k3) k4)))))))

(define (integrate-rk4 sys t0 state0 dt n)
  (doc 'export #t)
  (doc 'type '(-> ODE Number Vec Number Nat (List Vec)))
  (doc 'description "Integrate ODE system using RK4, returning trajectory as list of states")
  (let loop ([t t0] [state state0] [i 0] [result (list state0)])
       (if (>= i n)
           (reverse result)
           (let ([new-state (rk4-step sys t state dt)])
                (loop (+ t dt) new-state (+ i 1) (cons new-state result))))))

(define (integrate-rk4-transient sys t0 state0 dt n-transient n-keep)
  (doc 'export #t)
  (doc 'type '(-> ODE Number Vec Number Nat Nat (List Vec)))
  (doc 'description "Integrate with transient removal: skip first n-transient steps, keep n-keep")
  (let loop-transient ([t t0] [state state0] [i 0])
       (if (>= i n-transient)
           ;; Now collect trajectory
           (let loop-collect ([t t] [state state] [j 0] [result '()])
                (if (>= j n-keep)
                    (reverse result)
                    (let ([new-state (rk4-step sys t state dt)])
                         (loop-collect (+ t dt) new-state (+ j 1) (cons new-state result)))))
           ;; Still in transient
           (let ([new-state (rk4-step sys t state dt)])
                (loop-transient (+ t dt) new-state (+ i 1))))))

;;; ============================================================
;;; Section: Classic Strange Attractors
;;; ============================================================

(doc 'section 'strange-attractors)

(define (rossler-system a b c)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number ODE))
  (doc 'description "Rössler system: a simple chaotic attractor. Classic params: a=0.2, b=0.2, c=5.7")
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)]
                 [z (vector-ref state 2)])
                (vector (- (- y) z)
                        (+ x (* a y))
                        (+ b (* z (- x c))))))
   3))

(define (chen-system a b c)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number ODE))
  (doc 'description "Chen system: another chaotic attractor. Classic params: a=40, b=3, c=28")
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)]
                 [z (vector-ref state 2)])
                (vector (* a (- y x))
                        (+ (* (- c a) x) (* c y) (- (* x z)))
                        (- (* x y) (* b z)))))
   3))

(define (chua-circuit alpha beta m0 m1)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number ODE))
  (doc 'description "Chua's circuit: electronic chaos. Classic: alpha=15.6, beta=28, m0=-1.143, m1=-0.714")
  (make-autonomous-ode
   (lambda (state)
           (let* ([x (vector-ref state 0)]
                  [y (vector-ref state 1)]
                  [z (vector-ref state 2)]
                  ;; Chua's diode nonlinearity
                  [h (+ (* m1 x)
                        (* 0.5 (- m0 m1) (- (abs (+ x 1)) (abs (- x 1)))))])
                 ;; Standard Chua equations: dx/dt = α(y - x - h(x))
                ;;                         dy/dt = x - y + z
                ;;                         dz/dt = -βy
                (vector (* alpha (- y x h))
                        (+ (- x y) z)
                        (* (- beta) y))))
   3))

(define (thomas-attractor b)
  (doc 'export #t)
  (doc 'type '(-> Number ODE))
  (doc 'description "Thomas' cyclically symmetric attractor. Classic: b=0.208186")
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)]
                 [z (vector-ref state 2)])
                (vector (- (sin y) (* b x))
                        (- (sin z) (* b y))
                        (- (sin x) (* b z)))))
   3))

(define (aizawa-system a b c d e f)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number Number Number ODE))
  (doc 'description "Aizawa attractor. Classic: a=0.95, b=0.7, c=0.6, d=3.5, e=0.25, f=0.1")
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)]
                 [z (vector-ref state 2)])
                (vector (- (* (- z b) x) (* d y))
                        (+ (* d x) (* (- z b) y))
                        (+ c (* a z) (- (/ (* z z z) 3))
                           (- (* (+ (* x x) (* y y)) (+ 1 (* e z))))
                           (* f z (* x x x))))))
   3))

;;; ============================================================
;;; Section: Lyapunov Exponent Computation
;;; ============================================================

(doc 'section 'lyapunov-exponents)
(doc 'note "Lyapunov exponents measure the rate of separation of infinitesimally close trajectories. Positive exponents indicate chaos.")

(define (make-tangent-vector n index)
  (doc 'type '(-> Nat Nat Vec))
  (doc 'description "Create a unit vector along axis 'index' in n-dimensional space")
  (let ([v (make-vector n 0.0)])
       (vector-set! v index 1.0)
       v))

;;; Jacobian computation helper (finite differences)
(define (compute-jacobian-at sys t state h)
  (doc 'type '(-> ODE Number Vec Number Matrix))
  (doc 'description "Compute Jacobian of vector field at (t, state) using finite differences")
  (let* ([n (vector-length state)]
         [f0 (eval-vector-field sys t state)]
         [jac-data (make-vector (* n n) 0.0)])
        (do ([j 0 (+ j 1)])
            ((= j n))
            (let* ([state-plus-h (vec-copy state)]
                   [_ (vector-set! state-plus-h j
                                   (+ (vector-ref state j) h))]
                   [f-plus-h (eval-vector-field sys t state-plus-h)])
                  (do ([i 0 (+ i 1)])
                      ((= i n))
                      (vector-set! jac-data (+ (* i n) j)
                                   (/ (- (vector-ref f-plus-h i)
                                         (vector-ref f0 i))
                                      h)))))
        (list 'matrix n n jac-data)))

;;; RK4 step for coupled state + tangent evolution (variational equations)
(define (rk4-variational-step sys t state tangent dt)
  (doc 'type '(-> ODE Number Vec Vec Number (Pair Vec Vec)))
  (doc 'description "Single RK4 step for state and one tangent vector as coupled system")
  (doc 'note "Integrates dx/dt = f(x) and dξ/dt = J(x)·ξ together using RK4")
  (let* ([h 1e-6]  ; finite difference step for Jacobian
         ;; k1 at (t, state)
         [k1-state (eval-vector-field sys t state)]
         [j1 (compute-jacobian-at sys t state h)]
         [k1-tangent (matrix-vec-mul j1 tangent)]
         ;; k2 at (t + dt/2, state + dt/2 * k1)
         [t-mid (+ t (/ dt 2))]
         [state-mid1 (vec-add state (vec-scale (/ dt 2) k1-state))]
         [tangent-mid1 (vec-add tangent (vec-scale (/ dt 2) k1-tangent))]
         [k2-state (eval-vector-field sys t-mid state-mid1)]
         [j2 (compute-jacobian-at sys t-mid state-mid1 h)]
         [k2-tangent (matrix-vec-mul j2 tangent-mid1)]
         ;; k3 at (t + dt/2, state + dt/2 * k2)
         [state-mid2 (vec-add state (vec-scale (/ dt 2) k2-state))]
         [tangent-mid2 (vec-add tangent (vec-scale (/ dt 2) k2-tangent))]
         [k3-state (eval-vector-field sys t-mid state-mid2)]
         [j3 (compute-jacobian-at sys t-mid state-mid2 h)]
         [k3-tangent (matrix-vec-mul j3 tangent-mid2)]
         ;; k4 at (t + dt, state + dt * k3)
         [t-end (+ t dt)]
         [state-end (vec-add state (vec-scale dt k3-state))]
         [tangent-end (vec-add tangent (vec-scale dt k3-tangent))]
         [k4-state (eval-vector-field sys t-end state-end)]
         [j4 (compute-jacobian-at sys t-end state-end h)]
         [k4-tangent (matrix-vec-mul j4 tangent-end)]
         ;; Combine: new = old + dt/6 * (k1 + 2*k2 + 2*k3 + k4)
         [new-state (vec-add state
                             (vec-scale (/ dt 6)
                                        (vec-add k1-state
                                                 (vec-add (vec-scale 2 k2-state)
                                                          (vec-add (vec-scale 2 k3-state)
                                                                   k4-state)))))]
         [new-tangent (vec-add tangent
                               (vec-scale (/ dt 6)
                                          (vec-add k1-tangent
                                                   (vec-add (vec-scale 2 k2-tangent)
                                                            (vec-add (vec-scale 2 k3-tangent)
                                                                     k4-tangent)))))])
        (cons new-state new-tangent)))

;;; RK4 step for state + multiple tangent vectors
(define (rk4-variational-step-multi sys t state tangents dt)
  (doc 'type '(-> ODE Number Vec (List Vec) Number (Pair Vec (List Vec))))
  (doc 'description "Single RK4 step for state and multiple tangent vectors")
  (doc 'note "More efficient than calling rk4-variational-step multiple times - shares Jacobian computations")
  (let* ([h 1e-6]
         ;; k1 at (t, state)
         [k1-state (eval-vector-field sys t state)]
         [j1 (compute-jacobian-at sys t state h)]
         [k1-tangents (map (lambda (tv) (matrix-vec-mul j1 tv)) tangents)]
         ;; k2 at (t + dt/2, state + dt/2 * k1)
         [t-mid (+ t (/ dt 2))]
         [state-mid1 (vec-add state (vec-scale (/ dt 2) k1-state))]
         [tangents-mid1 (map (lambda (tv k1t) (vec-add tv (vec-scale (/ dt 2) k1t)))
                             tangents k1-tangents)]
         [k2-state (eval-vector-field sys t-mid state-mid1)]
         [j2 (compute-jacobian-at sys t-mid state-mid1 h)]
         [k2-tangents (map (lambda (tv) (matrix-vec-mul j2 tv)) tangents-mid1)]
         ;; k3 at (t + dt/2, state + dt/2 * k2)
         [state-mid2 (vec-add state (vec-scale (/ dt 2) k2-state))]
         [tangents-mid2 (map (lambda (tv k2t) (vec-add tv (vec-scale (/ dt 2) k2t)))
                             tangents k2-tangents)]
         [k3-state (eval-vector-field sys t-mid state-mid2)]
         [j3 (compute-jacobian-at sys t-mid state-mid2 h)]
         [k3-tangents (map (lambda (tv) (matrix-vec-mul j3 tv)) tangents-mid2)]
         ;; k4 at (t + dt, state + dt * k3)
         [t-end (+ t dt)]
         [state-end (vec-add state (vec-scale dt k3-state))]
         [tangents-end (map (lambda (tv k3t) (vec-add tv (vec-scale dt k3t)))
                            tangents k3-tangents)]
         [k4-state (eval-vector-field sys t-end state-end)]
         [j4 (compute-jacobian-at sys t-end state-end h)]
         [k4-tangents (map (lambda (tv) (matrix-vec-mul j4 tv)) tangents-end)]
         ;; Combine state
         [new-state (vec-add state
                             (vec-scale (/ dt 6)
                                        (vec-add k1-state
                                                 (vec-add (vec-scale 2 k2-state)
                                                          (vec-add (vec-scale 2 k3-state)
                                                                   k4-state)))))]
         ;; Combine tangents
         [new-tangents (map (lambda (tv k1t k2t k3t k4t)
                                    (vec-add tv
                                             (vec-scale (/ dt 6)
                                                        (vec-add k1t
                                                                 (vec-add (vec-scale 2 k2t)
                                                                          (vec-add (vec-scale 2 k3t)
                                                                                   k4t))))))
                            tangents k1-tangents k2-tangents k3-tangents k4-tangents)])
        (cons new-state new-tangents)))

(define (gram-schmidt-orthonormalize vectors)
  (doc 'type '(-> (List Vec) (Pair (List Vec) (List Number))))
  (doc 'description "Gram-Schmidt orthonormalization, returns (orthonormal-vecs . norms)")
  (let loop ([vecs vectors] [ortho '()] [norms '()])
       (if (null? vecs)
           (cons (reverse ortho) (reverse norms))
           (let* ([v (car vecs)]
                  ;; Subtract projections onto previous orthonormal vectors
                  [v-ortho (fold-left
                            (lambda (acc u)
                                    (vec-sub acc
                                             (vec-scale (vec-dot acc u) u)))
                            v
                            ortho)]
                  [norm (vec-norm v-ortho)]
                  [v-normed (if (> norm 1e-10)
                                (vec-scale (/ 1 norm) v-ortho)
                                v-ortho)])
                 (loop (cdr vecs)
                       (cons v-normed ortho)
                       (cons (max norm 1e-10) norms))))))

(define (lyapunov-exponents sys state0 dt n-steps n-transient reorth-interval)
  (doc 'export #t)
  (doc 'type '(-> ODE Vec Number Nat Nat Nat (List Number)))
  (doc 'description "Compute all Lyapunov exponents using RK4-integrated tangent space evolution")
  (doc 'param 'reorth-interval "steps between Gram-Schmidt reorthonormalizations")
  (doc 'note "Uses RK4 for both state and tangent vectors (variational equations) for O(dt^4) accuracy")
  (let* ([dim (ode-dimension sys)]
         ;; Skip transient
         [state-after-transient
          (let loop ([t 0] [state state0] [i 0])
               (if (>= i n-transient)
                   state
                   (loop (+ t dt) (rk4-step sys t state dt) (+ i 1))))]
         ;; Initialize tangent vectors (identity matrix columns)
         [tangent-vecs (map (lambda (i) (make-tangent-vector dim i))
                            (iota dim))])
        ;; Main loop: evolve trajectory and tangent vectors using RK4 variational integration
        (let loop ([t 0]
                   [state state-after-transient]
                   [tangents tangent-vecs]
                   [lyap-sums (make-list dim 0.0)]
                   [step 0]
                   [reorth-count 0])
             (if (>= step n-steps)
                 ;; Compute final Lyapunov exponents
                 (map (lambda (sum) (/ sum (* n-steps dt))) lyap-sums)
                 ;; Evolve state and tangents together using RK4
                 (let* ([result (rk4-variational-step-multi sys t state tangents dt)]
                        [new-state (car result)]
                        [new-tangents (cdr result)])
                       ;; Check if we need to reorthonormalize
                       (if (>= reorth-count reorth-interval)
                           (let* ([gs-result (gram-schmidt-orthonormalize new-tangents)]
                                  [ortho-tangents (car gs-result)]
                                  [norms (cdr gs-result)]
                                  ;; Add log(norms) to running sums
                                  [new-sums (map (lambda (sum norm)
                                                         (+ sum (log norm)))
                                                 lyap-sums norms)])
                                 (loop (+ t dt) new-state ortho-tangents
                                       new-sums (+ step 1) 0))
                           ;; No reorthonormalization needed
                           (loop (+ t dt) new-state new-tangents
                                 lyap-sums (+ step 1) (+ reorth-count 1))))))))

(define (largest-lyapunov-exponent sys state0 dt n-steps n-transient)
  (doc 'export #t)
  (doc 'type '(-> ODE Vec Number Nat Nat Number))
  (doc 'description "Compute largest Lyapunov exponent using RK4 variational integration")
  (doc 'note "Uses RK4 for both state and tangent vector for O(dt^4) accuracy")
  (let* ([dim (ode-dimension sys)]
         ;; Skip transient
         [state-after-transient
          (let loop ([t 0] [state state0] [i 0])
               (if (>= i n-transient)
                   state
                   (loop (+ t dt) (rk4-step sys t state dt) (+ i 1))))])
        ;; Main loop using RK4 variational integration
        (let loop ([t 0]
                   [state state-after-transient]
                   [tangent (make-tangent-vector dim 0)]
                   [lyap-sum 0.0]
                   [step 0])
             (if (>= step n-steps)
                 (/ lyap-sum (* n-steps dt))
                 (let* ([result (rk4-variational-step sys t state tangent dt)]
                        [new-state (car result)]
                        [new-tangent-raw (cdr result)]
                        ;; Renormalize tangent vector (avoid exponential growth)
                        [norm (vec-norm new-tangent-raw)]
                        [new-tangent (vec-scale (/ 1 (max norm 1e-10)) new-tangent-raw)])
                       (loop (+ t dt) new-state new-tangent
                             (+ lyap-sum (log (max norm 1e-10)))
                             (+ step 1)))))))

;;; ============================================================
;;; Section: Chaos Detection
;;; ============================================================

(doc 'section 'chaos-detection)

(define (is-chaotic? sys state0 dt n-steps n-transient threshold)
  (doc 'export #t)
  (doc 'type '(-> ODE Vec Number Nat Nat Number Boolean))
  (doc 'description "Test if system exhibits chaos (positive largest Lyapunov exponent)")
  (> (largest-lyapunov-exponent sys state0 dt n-steps n-transient)
     threshold))

(define (kaplan-yorke-dimension lyap-exponents)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Compute Kaplan-Yorke (Lyapunov) dimension from exponents")
  (doc 'note "Sorted exponents λ₁ ≥ λ₂ ≥ ... → D_KY = j + Σᵢ₌₁ʲλᵢ/|λⱼ₊₁| where j is largest index with Σλᵢ ≥ 0")
  (let* ([sorted (list-sort > lyap-exponents)]
         [n (length sorted)])
        (let loop ([j 0] [sum 0.0])
             (if (>= j n)
                 (inexact n)  ; All exponents positive - full dimension
                 (let ([new-sum (+ sum (list-ref sorted j))])
                      (if (< new-sum 0)
                          ;; Found j where sum goes negative
                          (let ([prev-sum (- new-sum (list-ref sorted j))]
                                [lambda-j (abs (list-ref sorted j))])
                               (if (> lambda-j 1e-10)
                                   (+ j (/ prev-sum lambda-j))
                                   (inexact j)))
                          (loop (+ j 1) new-sum)))))))

;;; ============================================================
;;; Section: Poincaré Sections
;;; ============================================================

(doc 'section 'poincare-sections)
(doc 'note "Reduce 3D attractor to 2D by recording intersections with a plane")

(define (poincare-section sys state0 dt n-steps n-transient plane-axis plane-value direction)
  (doc 'export #t)
  (doc 'type '(-> ODE Vec Number Nat Nat Nat Number Symbol (List Vec)))
  (doc 'description "Compute Poincaré section by recording crossings of a coordinate plane")
  (doc 'param 'plane-axis "which axis defines the plane (0=x, 1=y, 2=z)")
  (doc 'param 'plane-value "value where the plane crosses that axis")
  (doc 'param 'direction "'positive or 'negative for crossing direction")
  (let* (;; Skip transient
         [state-after-transient
          (let loop ([t 0] [state state0] [i 0])
               (if (>= i n-transient)
                   state
                   (loop (+ t dt) (rk4-step sys t state dt) (+ i 1))))]
         ;; Check crossing direction
         [check-crossing
          (lambda (prev-val curr-val)
                  (case direction
                    [(positive) (and (< prev-val plane-value)
                                     (>= curr-val plane-value))]
                    [(negative) (and (> prev-val plane-value)
                                     (<= curr-val plane-value))]
                    [else (or (and (< prev-val plane-value)
                                   (>= curr-val plane-value))
                              (and (> prev-val plane-value)
                                   (<= curr-val plane-value)))]))]
         ;; Linear interpolation to find crossing point
         [interpolate-crossing
          (lambda (state1 state2)
                  (let* ([v1 (vector-ref state1 plane-axis)]
                         [v2 (vector-ref state2 plane-axis)]
                         [t-frac (/ (- plane-value v1) (- v2 v1))])
                        (vec-add state1
                                 (vec-scale t-frac (vec-sub state2 state1)))))])
        ;; Collect crossings
        (let loop ([t 0]
                   [state state-after-transient]
                   [step 0]
                   [crossings '()])
             (if (>= step n-steps)
                 (reverse crossings)
                 (let* ([new-state (rk4-step sys t state dt)]
                        [prev-val (vector-ref state plane-axis)]
                        [curr-val (vector-ref new-state plane-axis)])
                       (if (check-crossing prev-val curr-val)
                           (let ([crossing-point (interpolate-crossing state new-state)])
                                (loop (+ t dt) new-state (+ step 1)
                                      (cons crossing-point crossings)))
                           (loop (+ t dt) new-state (+ step 1) crossings)))))))

(define (poincare-section-2d section-points keep-axes)
  (doc 'export #t)
  (doc 'type '(-> (List Vec) (List Nat) (List (Pair Number Number))))
  (doc 'description "Project Poincaré section points onto 2D for plotting")
  (doc 'param 'keep-axes "list of two indices to keep, e.g., '(0 2) for x-z projection")
  (map (lambda (pt)
               (cons (vector-ref pt (car keep-axes))
                     (vector-ref pt (cadr keep-axes))))
       section-points))

;;; ============================================================
;;; Section: Attractor Generation
;;; ============================================================

(doc 'section 'attractor-generation)

(define (generate-attractor sys state0 dt n-steps n-transient)
  (doc 'export #t)
  (doc 'type '(-> ODE Vec Number Nat Nat (List Vec)))
  (doc 'description "Generate attractor trajectory after removing transient")
  (integrate-rk4-transient sys 0 state0 dt n-transient n-steps))

(define (attractor-bounds trajectory)
  (doc 'export #t)
  (doc 'type '(-> (List Vec) (List (Pair Number Number))))
  (doc 'description "Compute bounding box of attractor trajectory")
  (doc 'returns "list of (min . max) pairs for each dimension")
  (if (null? trajectory)
      '()
      (let* ([dim (vector-length (car trajectory))]
             [initial-bounds (map (lambda (i)
                                          (let ([v (vector-ref (car trajectory) i)])
                                               (cons v v)))
                                  (iota dim))])
            (fold-left
             (lambda (bounds pt)
                     (map (lambda (bound i)
                                  (let ([v (vector-ref pt i)])
                                       (cons (min (car bound) v)
                                             (max (cdr bound) v))))
                          bounds
                          (iota dim)))
             initial-bounds
             (cdr trajectory)))))

(define (normalize-trajectory trajectory bounds target-range)
  (doc 'export #t)
  (doc 'type '(-> (List Vec) (List (Pair Number Number)) Number (List Vec)))
  (doc 'description "Normalize trajectory to fit within target range [-r, r]")
  (if (null? trajectory)
      '()
      (let* ([dim (length bounds)]
             [scales (map (lambda (bound)
                                  (let ([range (- (cdr bound) (car bound))])
                                       (if (> range 1e-10)
                                           (/ (* 2 target-range) range)
                                           1.0)))
                         bounds)]
             [offsets (map (lambda (bound)
                                   (/ (+ (car bound) (cdr bound)) 2))
                          bounds)])
            (map (lambda (pt)
                         (list->vector
                          (map (lambda (i)
                                       (* (list-ref scales i)
                                          (- (vector-ref pt i)
                                             (list-ref offsets i))))
                               (iota dim))))
                 trajectory))))

;;; ============================================================
;;; Section: Standard Attractor Parameters
;;; ============================================================

(doc 'section 'standard-parameters)

(doc lorenz-classic 'export #t)
(doc lorenz-classic 'description "Lorenz system with classic chaotic parameters")
(define lorenz-classic (lorenz-system 10 28 (/ 8 3)))

(doc rossler-classic 'export #t)
(doc rossler-classic 'description "Rössler system with classic chaotic parameters")
(define rossler-classic (rossler-system 0.2 0.2 5.7))

(doc chen-classic 'export #t)
(doc chen-classic 'description "Chen system with classic chaotic parameters")
(define chen-classic (chen-system 40 3 28))

(doc thomas-classic 'export #t)
(doc thomas-classic 'description "Thomas attractor with classic parameters")
(define thomas-classic (thomas-attractor 0.208186))

(doc aizawa-classic 'export #t)
(doc aizawa-classic 'description "Aizawa attractor with classic parameters")
(define aizawa-classic (aizawa-system 0.95 0.7 0.6 3.5 0.25 0.1))

;;; ============================================================
;;; Section: Diagnostic Helpers
;;; ============================================================

(doc 'section 'diagnostics)

(define (chaos-summary sys state0 dt n-steps n-transient)
  (doc 'export #t)
  (doc 'type '(-> ODE Vec Number Nat Nat List))
  (doc 'description "Compute summary of chaotic properties")
  (let* ([lyaps (lyapunov-exponents sys state0 dt n-steps n-transient 10)]
         [ky-dim (kaplan-yorke-dimension lyaps)]
         [trajectory (generate-attractor sys state0 dt (min 1000 n-steps) n-transient)]
         [bounds (attractor-bounds trajectory)])
        (list (cons 'lyapunov-exponents lyaps)
              (cons 'kaplan-yorke-dimension ky-dim)
              (cons 'is-chaotic (> (car lyaps) 0.01))
              (cons 'bounds bounds)
              (cons 'trajectory-length (length trajectory)))))
