(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'vec)
(require 'matrix)

(doc 'module 'ode-system)
(doc 'description "Pure implementation of continuous-time dynamical systems represented as systems of ordinary differential equations (ODEs). Supports autonomous and non-autonomous systems, vector field computation, phase portraits, and flow analysis")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "An ODE system is: dx/dt = f(t, x) where x ∈ ℝⁿ is the state vector, f : Number × Numberⁿ → Numberⁿ is the vector field. For autonomous systems: dx/dt = f(x) (time-independent)")

(doc 'section 'ode-system-representation)
(doc 'note "An ODE system is represented as: (ode-system vector-field dimension autonomous?)")
(doc 'note "vector-field : (Number × Any) → Any (non-autonomous) or Vec → Any (autonomous)")
(doc 'note "dimension : Nat (dimension of state space)")
(doc 'note "autonomous? : Boolean (time-independent?)")

(define (ode-system? sys)
  (doc 'type '(-> Any Bool))
  (doc 'description "Check if value is an ODE system")
  (and (pair? sys)
       (eq? (car sys) 'ode-system)
       (= (length sys) 4)
       (procedure? (cadr sys))
       (integer? (caddr sys))
       (boolean? (cadddr sys))))

(define (ode-vector-field sys)
  (doc 'type '(-> Any Any))
  (cadr sys))

(define (ode-dimension sys)
  (doc 'type '(-> Any Nat))
  (caddr sys))

(define (ode-autonomous? sys)
  (doc 'type '(-> Any Bool))
  (cadddr sys))

(define (make-ode-system vector-field dimension autonomous?)
  (doc 'type '(-> (-> Number Any Any) Nat Bool Any))
  (doc 'description "Create an ODE system with explicit autonomy flag")
  (list 'ode-system vector-field dimension autonomous?))

(define (make-autonomous-ode vector-field dimension)
  (doc 'type '(-> (-> Any Any) Nat Any))
  (doc 'description "Create an autonomous ODE system: dx/dt = f(x). Wraps the vector field to accept (but ignore) time parameter")
  (make-ode-system
   (lambda (t state) (vector-field state))
   dimension
   #t))

(define (make-nonautonomous-ode vector-field dimension)
  (doc 'type '(-> (-> Number Any Any) Nat Any))
  (doc 'description "Create a non-autonomous ODE system: dx/dt = f(t, x)")
  (make-ode-system
   vector-field
   dimension
   #f))

(doc 'section 'vector-field-evaluation)

(define (eval-vector-field sys t state)
  (doc 'type '(-> Any Number Any Any))
  (doc 'description "Evaluate the vector field at a given time and state")
  (doc 'returns "the derivative dx/dt at the point (t, x)")
  ((ode-vector-field sys) t state))

(define (eval-vector-field-batch sys t states)
  (doc 'type '(-> Any Number (List Any) (List Any)))
  (doc 'description "Evaluate the vector field at multiple states at the same time. Useful for phase portrait computation")
  (map (lambda (state) (eval-vector-field sys t state)) states))

(doc 'section 'flow-and-trajectory)

(define (make-flow-function sys)
  (doc 'type '(-> Any (-> Number Number Any Any)))
  (doc 'description "Create a flow function φ(t, t0, x0) that represents the solution at time t starting from state x0 at time t0")
  (doc 'note "This is a symbolic/lazy representation - actual computation requires a numerical integrator")
  (lambda (t t0 x0)
          `(flow ,sys ,t ,t0 ,x0)))

(define (vector-field-norm sys t state)
  (doc 'type '(-> Any Number Any Number))
  (doc 'description "Compute the magnitude of the vector field at a point. Useful for identifying equilibrium points (where ||f(x)|| ≈ 0)")
  (let ([field (eval-vector-field sys t state)])
       (if (vector? field)
           (vec-norm field)
           (abs field))))

(doc 'section 'phase-space-utilities)

(define (make-phase-space-grid x-min x-max x-steps y-min y-max y-steps)
  (doc 'type '(-> Number Number Nat Number Number Nat (List Any)))
  (doc 'description "Create a rectangular grid of points in 2D phase space. For higher dimensions, use make-phase-space-grid-nd")
  (doc 'param 'x-min "x-axis minimum")
  (doc 'param 'x-max "x-axis maximum")
  (doc 'param 'x-steps "x-axis steps")
  (doc 'param 'y-min "y-axis minimum")
  (doc 'param 'y-max "y-axis maximum")
  (doc 'param 'y-steps "y-axis steps")
  (doc 'returns "List of 2D state vectors (as Scheme vectors)")
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

(define (compute-vector-field-grid sys t grid-points)
  (doc 'type '(-> Any Number (List Any) (List Any)))
  (doc 'description "Compute vector field values at a grid of points")
  (doc 'returns "list of derivative vectors at each grid point")
  (map (lambda (point) (eval-vector-field sys t point)) grid-points))

(doc 'section 'standard-ode-systems)

(define (exponential-growth r)
  (doc 'type '(-> Number Any))
  (doc 'description "Simple exponential growth: dx/dt = r*x. Solution: x(t) = x₀ * exp(r*t)")
  (make-autonomous-ode
   (lambda (state)
           (if (vector? state)
               (vec-scale r state)
               (* r state)))
   1))

(define (harmonic-oscillator omega)
  (doc 'type '(-> Number Any))
  (doc 'description "Simple harmonic oscillator: d²x/dt² = -ω²x. As first-order system: dx/dt = v, dv/dt = -ω²x. State: [position, velocity]")
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [v (vector-ref state 1)])
                (vector v (- (* omega omega x)))))
   2))

(define (damped-oscillator omega zeta)
  (doc 'type '(-> Number Number Any))
  (doc 'description "Damped harmonic oscillator: d²x/dt² = -2ζω dx/dt - ω²x. State: [position, velocity]")
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [v (vector-ref state 1)])
                (vector v
                        (- (- (* 2 zeta omega v))
                           (* omega omega x)))))
   2))

(define (lotka-volterra alpha beta gamma delta)
  (doc 'type '(-> Number Number Number Number Any))
  (doc 'description "Lotka-Volterra predator-prey model: dx/dt = αx - βxy (prey), dy/dt = δxy - γy (predator). State: [prey, predator]")
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]  ; prey
                 [y (vector-ref state 1)]) ; predator
                (vector (- (* alpha x) (* beta x y))
                        (- (* delta x y) (* gamma y)))))
   2))

(define (lorenz-system sigma rho beta)
  (doc 'type '(-> Number Number Number Any))
  (doc 'description "Lorenz system (chaotic attractor): dx/dt = σ(y - x), dy/dt = x(ρ - z) - y, dz/dt = xy - βz. Classic parameters: σ=10, ρ=28, β=8/3")
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)]
                 [z (vector-ref state 2)])
                (vector (* sigma (- y x))
                        (- (* x (- rho z)) y)
                        (- (* x y) (* beta z)))))
   3))

(define (van-der-pol mu)
  (doc 'type '(-> Number Any))
  (doc 'description "Van der Pol oscillator (limit cycle): d²x/dt² = μ(1 - x²)dx/dt - x. State: [position, velocity]")
  (make-autonomous-ode
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [v (vector-ref state 1)])
                (vector v
                        (- (* mu (- 1 (* x x)) v) x))))
   2))

(define (pendulum g-over-l)
  (doc 'type '(-> Number Any))
  (doc 'description "Simple pendulum (nonlinear): d²θ/dt² = -g/L sin(θ). State: [angle, angular velocity]. Normalized with g/L = 1")
  (make-autonomous-ode
   (lambda (state)
           (let ([theta (vector-ref state 0)]
                 [omega (vector-ref state 1)])
                (vector omega
                        (- (* g-over-l (sin theta))))))
   2))

(define (forced-oscillator omega0 omega-drive amplitude)
  (doc 'type '(-> Number Number Number Any))
  (doc 'description "Forced oscillator: d²x/dt² = -ω₀²x + A·cos(ωt). Non-autonomous system (time-dependent forcing). State: [position, velocity]")
  (make-nonautonomous-ode
   (lambda (t state)
           (let ([x (vector-ref state 0)]
                 [v (vector-ref state 1)])
                (vector v
                        (+ (- (* omega0 omega0 x))
                           (* amplitude (cos (* omega-drive t)))))))
   2))

(define (linear-ode A)
  (doc 'type '(-> Any Any))
  (doc 'description "Linear ODE system: dx/dt = Ax where A is an n×n matrix")
  (let ([n (matrix-rows A)])
       (make-autonomous-ode
        (lambda (state)
                (matrix-vec-mul A state))
        n)))

(doc 'section 'equilibrium-points)

(define (is-equilibrium? sys state tolerance)
  (doc 'type '(-> Any Any Number Bool))
  (doc 'description "Check if a point is an equilibrium (fixed point) within tolerance. An equilibrium satisfies f(x) = 0")
  (let ([field-norm (vector-field-norm sys 0 state)])
       (< field-norm tolerance)))

(define (find-equilibria-grid sys grid-points tolerance)
  (doc 'type '(-> Any (List Any) Number (List Any)))
  (doc 'description "Find approximate equilibrium points from a grid search")
  (doc 'returns "points where ||f(x)|| < tolerance")
  (filter (lambda (point) (is-equilibrium? sys point tolerance))
          grid-points))

(doc 'section 'system-composition)

(define (couple-ode-systems sys1 sys2)
  (doc 'type '(-> Any Any Any))
  (doc 'description "Couple two ODE systems into a single higher-dimensional system. The state vector is the concatenation [x1, x2]")
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

(doc 'section 'time-reversal)

(define (reverse-time-ode sys)
  (doc 'type '(-> Any Any))
  (doc 'description "Create the time-reversed ODE system: dx/dt = -f(t, x). Useful for backward integration and analyzing reversibility")
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

(doc 'section 'coordinate-transformations)

(define (transform-ode-system sys transform inverse)
  (doc 'type '(-> Any (-> Any Any) (-> Any Any) Any))
  (doc 'description "Transform an ODE system via a coordinate change. If y = T(x), then dy/dt = (DT)(x) · f(x). This is a simplified version - full implementation needs Jacobian")
  (doc 'param 'sys "Original ODE system")
  (doc 'param 'transform "x → y coordinate transformation")
  (doc 'param 'inverse "y → x inverse transformation")
  (doc 'returns "Transformed ODE system in y coordinates")
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
