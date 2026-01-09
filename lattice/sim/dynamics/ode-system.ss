;;; core/dynamics/ode-system.ss — Ordinary Differential Equation Systems
;;;
;;; Pure implementation of continuous-time dynamical systems represented
;;; as systems of ordinary differential equations (ODEs). Supports autonomous
;;; and non-autonomous systems, vector field computation, phase portraits,
;;; and flow analysis.
;;;
;;; An ODE system is: dx/dt = f(t, x) where
;;;   x ∈ ℝⁿ is the state vector
;;;   f : Number × Numberⁿ → Numberⁿ is the vector field
;;;   For autonomous systems: dx/dt = f(x) (time-independent)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/linalg/vec.ss
;;;   - core/linalg/matrix.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")

;;; ============================================================
;;; ODE System Representation
;;; ============================================================

;;; An ODE system is represented as:
;;;   (ode-system vector-field dimension autonomous?)
;;;
;;; where:
;;;   vector-field : (Number × Any) → Any  (non-autonomous)
;;;              or  Vec → Any             (autonomous)
;;;   dimension : Nat (dimension of state space)
;;;   autonomous? : Boolean (time-independent?)

;;; ode-system? : Any → Bool
;;; Check if value is an ODE system.
(define (ode-system? sys)
  (and (pair? sys)
       (eq? (car sys) 'ode-system)
       (= (length sys) 4)
       (procedure? (cadr sys))
       (integer? (caddr sys))
       (boolean? (cadddr sys))))

;;; Accessors
;;; ode-vector-field : Any → Any
(define (ode-vector-field sys) (cadr sys))

;;; ode-dimension : Any → Nat
(define (ode-dimension sys) (caddr sys))

;;; ode-autonomous? : Any → Bool
(define (ode-autonomous? sys) (cadddr sys))

;;; make-ode-system : (Number × Any → Any) × Nat × Bool → Any
;;; Create an ODE system with explicit autonomy flag.
(define (make-ode-system vector-field dimension autonomous?)
  (list 'ode-system vector-field dimension autonomous?))

;;; make-autonomous-ode : (Any → Any) × Nat → Any
;;; Create an autonomous ODE system: dx/dt = f(x).
;;; Wraps the vector field to accept (but ignore) time parameter.
(define (make-autonomous-ode vector-field dimension)
  (make-ode-system
   (lambda (t state) (vector-field state))
   dimension
   #t))

;;; make-nonautonomous-ode : (Number × Any → Any) × Nat → Any
;;; Create a non-autonomous ODE system: dx/dt = f(t, x).
(define (make-nonautonomous-ode vector-field dimension)
  (make-ode-system
   vector-field
   dimension
   #f))

;;; ============================================================
;;; Vector Field Evaluation
;;; ============================================================

;;; eval-vector-field : Any × Number × Any → Any
;;; Evaluate the vector field at a given time and state.
;;; Returns the derivative dx/dt at the point (t, x).
(define (eval-vector-field sys t state)
  ((ode-vector-field sys) t state))

;;; eval-vector-field-batch : Any × Number × (List Any) → (List Any)
;;; Evaluate the vector field at multiple states at the same time.
;;; Useful for phase portrait computation.
(define (eval-vector-field-batch sys t states)
  (map (lambda (state) (eval-vector-field sys t state)) states))

;;; ============================================================
;;; Flow and Trajectory
;;; ============================================================

;;; make-flow-function : Any → (Number × Number × Any → Any)
;;; Create a flow function φ(t, t0, x0) that represents the solution
;;; at time t starting from state x0 at time t0.
;;; Note: This is a symbolic/lazy representation - actual computation
;;; requires a numerical integrator.
(define (make-flow-function sys)
  (lambda (t t0 x0)
          ;; This is a placeholder - actual flow requires numerical integration
          ;; The integrator should be provided separately
          `(flow ,sys ,t ,t0 ,x0)))

;;; vector-field-norm : Any × Number × Any → Number
;;; Compute the magnitude of the vector field at a point.
;;; Useful for identifying equilibrium points (where ||f(x)|| ≈ 0).
(define (vector-field-norm sys t state)
  (let ([field (eval-vector-field sys t state)])
       (if (vector? field)
           (vec-norm field)
           (abs field))))

;;; ============================================================
;;; Phase Space Utilities
;;; ============================================================

;;; make-phase-space-grid : Number × Number × Nat × Number × Number × Nat → (List Any)
;;; Create a rectangular grid of points in 2D phase space.
;;; For higher dimensions, use make-phase-space-grid-nd.
;;;
;;; Arguments:
;;;   x-min, x-max, x-steps : x-axis range and steps
;;;   y-min, y-max, y-steps : y-axis range and steps
;;;
;;; Returns: List of 2D state vectors (as Scheme vectors)
(define (make-phase-space-grid x-min x-max x-steps y-min y-max y-steps)
  (let ([x-delta (/ (- x-max x-min) (max 1 (- x-steps 1)))]
        [y-delta (/ (- y-max y-min) (max 1 (- y-steps 1)))])
       (let loop-y ([j 0] [result '()])
            (if (>= j y-steps)
                (reverse result)
                (let ([y (+ y-min (* j y-delta))])
                     (loop-y (+ j 1)
                             (let loop-x ([i 0] [row-result result])
                                  (if (>= i x-steps)
                                      row-result
                                      (let ([x (+ x-min (* i x-delta))])
                                           (loop-x (+ i 1)
                                                   (cons (vector x y) row-result)))))))))))

;;; compute-vector-field-grid : Any × Number × (List Any) → (List Any)
;;; Compute vector field values at a grid of points.
;;; Returns list of derivative vectors at each grid point.
(define (compute-vector-field-grid sys t grid-points)
  (map (lambda (point) (eval-vector-field sys t point)) grid-points))

;;; ============================================================
;;; Standard ODE Systems (Examples)
;;; ============================================================

;;; exponential-growth : Number → Any
;;; Simple exponential growth: dx/dt = r*x
;;; Solution: x(t) = x₀ * exp(r*t)
(define (exponential-growth r)
  (make-autonomous-ode
   (lambda (state)
           (if (vector? state)
               (vec-scale r state)
               (* r state)))
   1))

;;; harmonic-oscillator : Number → Any
;;; Simple harmonic oscillator: d²x/dt² = -ω²x
;;; As first-order system: dx/dt = v, dv/dt = -ω²x
;;; State: [position, velocity]
(define (harmonic-oscillator omega)
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [v (vector-ref state 1)])
                (vector v (- (* omega omega x)))))
   2))

;;; damped-oscillator : Number × Number → Any
;;; Damped harmonic oscillator: d²x/dt² = -2ζω dx/dt - ω²x
;;; State: [position, velocity]
(define (damped-oscillator omega zeta)
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [v (vector-ref state 1)])
                (vector v
                        (- (- (* 2 zeta omega v))
                           (* omega omega x)))))
   2))

;;; lotka-volterra : Number × Number × Number × Number → Any
;;; Lotka-Volterra predator-prey model:
;;;   dx/dt = αx - βxy  (prey)
;;;   dy/dt = δxy - γy  (predator)
;;; State: [prey, predator]
(define (lotka-volterra alpha beta gamma delta)
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]  ; prey
                 [y (vector-ref state 1)]) ; predator
                (vector (- (* alpha x) (* beta x y))
                        (- (* delta x y) (* gamma y)))))
   2))

;;; lorenz-system : Number × Number × Number → Any
;;; Lorenz system (chaotic attractor):
;;;   dx/dt = σ(y - x)
;;;   dy/dt = x(ρ - z) - y
;;;   dz/dt = xy - βz
;;; Classic parameters: σ=10, ρ=28, β=8/3
(define (lorenz-system sigma rho beta)
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)]
                 [z (vector-ref state 2)])
                (vector (* sigma (- y x))
                        (- (* x (- rho z)) y)
                        (- (* x y) (* beta z)))))
   3))

;;; van-der-pol : Number → Any
;;; Van der Pol oscillator (limit cycle):
;;;   d²x/dt² = μ(1 - x²)dx/dt - x
;;; State: [position, velocity]
(define (van-der-pol mu)
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [v (vector-ref state 1)])
                (vector v
                        (- (* mu (- 1 (* x x)) v) x))))
   2))

;;; pendulum : Number → Any
;;; Simple pendulum (nonlinear): d²θ/dt² = -g/L sin(θ)
;;; State: [angle, angular velocity]
;;; Normalized with g/L = 1
(define (pendulum g-over-l)
  (make-autonomous-ode
   (lambda (state)
           (let ([theta (vector-ref state 0)]
                 [omega (vector-ref state 1)])
                (vector omega
                        (- (* g-over-l (sin theta))))))
   2))

;;; forced-oscillator : Number × Number × Number → Any
;;; Forced oscillator: d²x/dt² = -ω₀²x + A·cos(ωt)
;;; Non-autonomous system (time-dependent forcing)
;;; State: [position, velocity]
(define (forced-oscillator omega0 omega-drive amplitude)
  (make-nonautonomous-ode
   (lambda (t state)
           (let ([x (vector-ref state 0)]
                 [v (vector-ref state 1)])
                (vector v
                        (+ (- (* omega0 omega0 x))
                           (* amplitude (cos (* omega-drive t)))))))
   2))

;;; linear-ode : Any → Any
;;; Linear ODE system: dx/dt = Ax
;;; where A is an n×n matrix
(define (linear-ode A)
  (let ([n (matrix-rows A)])
       (make-autonomous-ode
        (lambda (state)
                (matrix-vec-mul A state))
        n)))

;;; ============================================================
;;; Equilibrium Points
;;; ============================================================

;;; is-equilibrium? : Any × Any × Number → Bool
;;; Check if a point is an equilibrium (fixed point) within tolerance.
;;; An equilibrium satisfies f(x) = 0.
(define (is-equilibrium? sys state tolerance)
  (let ([field-norm (vector-field-norm sys 0 state)])
       (< field-norm tolerance)))

;;; find-equilibria-grid : Any × (List Any) × Number → (List Any)
;;; Find approximate equilibrium points from a grid search.
;;; Returns points where ||f(x)|| < tolerance.
(define (find-equilibria-grid sys grid-points tolerance)
  (filter (lambda (point) (is-equilibrium? sys point tolerance))
          grid-points))

;;; ============================================================
;;; System Composition
;;; ============================================================

;;; couple-ode-systems : Any × Any → Any
;;; Couple two ODE systems into a single higher-dimensional system.
;;; The state vector is the concatenation [x1, x2].
(define (couple-ode-systems sys1 sys2)
  (let ([dim1 (ode-dimension sys1)]
        [dim2 (ode-dimension sys2)]
        [f1 (ode-vector-field sys1)]
        [f2 (ode-vector-field sys2)]
        [auto? (and (ode-autonomous? sys1) (ode-autonomous? sys2))])
       (make-ode-system
        (lambda (t state)
                ;; Split state into two parts
                (let* ([state1 (vec-take state dim1)]
                       [state2 (vec-drop state dim1)]
                       [deriv1 (f1 t state1)]
                       [deriv2 (f2 t state2)])
                      ;; Concatenate derivatives
                      (vec-append deriv1 deriv2)))
        (+ dim1 dim2)
        auto?)))

;;; ============================================================
;;; Time Reversal
;;; ============================================================

;;; reverse-time-ode : Any → Any
;;; Create the time-reversed ODE system: dx/dt = -f(t, x)
;;; Useful for backward integration and analyzing reversibility.
(define (reverse-time-ode sys)
  (let ([f (ode-vector-field sys)]
        [dim (ode-dimension sys)]
        [auto? (ode-autonomous? sys)])
       (make-ode-system
        (lambda (t state)
                (let ([field (f t state)])
                     (if (vector? field)
                         (vec-negate field)
                         (- field))))
        dim
        auto?)))

;;; ============================================================
;;; Coordinate Transformations
;;; ============================================================

;;; transform-ode-system : Any × (Any → Any) × (Any → Any) → Any
;;; Transform an ODE system via a coordinate change.
;;; If y = T(x), then dy/dt = (DT)(x) · f(x)
;;; This is a simplified version - full implementation needs Jacobian.
;;;
;;; Arguments:
;;;   sys : Original ODE system
;;;   transform : x → y coordinate transformation
;;;   inverse : y → x inverse transformation
;;;
;;; Returns: Transformed ODE system in y coordinates
(define (transform-ode-system sys transform inverse)
  (let ([f (ode-vector-field sys)]
        [dim (ode-dimension sys)]
        [auto? (ode-autonomous? sys)])
       (make-ode-system
        (lambda (t y)
                ;; Convert y back to x, evaluate f(x), transform result
                ;; This is approximate - proper transformation needs Jacobian
                (let* ([x (inverse y)]
                       [fx (f t x)])
                      ;; Approximate: assume linear transformation
                      (transform fx)))
        dim
        auto?)))
