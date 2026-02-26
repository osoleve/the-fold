(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module ode-integrators
;;; @requires prelude ode-state-vec ode-adaptive
(require 'prelude)
(require 'ode-state-vec)
(require 'ode-adaptive)

(doc 'module 'ode-integrators)
(doc 'description "Numerical integration (ODE solving) methods for physics and scientific computing")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Explicit methods: euler-step, midpoint-step, rk4-step. Symplectic methods: verlet-step, velocity-verlet-step, leapfrog-step")
;;;   - state, pos, vel are vectors (lists of numbers)
;;;   - t is time (number)
;;;   - dt is timestep (number)

;;; ====
;;; Vector Operations (backward-compat aliases for ode-state-vec)
;;; ====

(define state-add sv-add)
(define state-sub sv-sub)
(define state-scale sv-scale)
(define state-madd sv-madd)

;;; ====
;;; Explicit Integration Methods
;;; ====

;;; euler-step : ((Number × State) → State) × Number × State × Number → State
;;; One step of forward Euler method.
;;; y_{n+1} = y_n + dt * f(t_n, y_n)
;;;
;;; First-order accurate, simple but unstable for stiff problems.
(define (euler-step f t state dt)
  (let ([derivative (f t state)])
       (state-madd state dt derivative)))

;;; midpoint-step : ((Number × State) → State) × Number × State × Number → State
;;; One step of midpoint method (RK2).
;;; k1 = f(t_n, y_n)
;;; k2 = f(t_n + dt/2, y_n + dt/2 * k1)
;;; y_{n+1} = y_n + dt * k2
;;;
;;; Second-order accurate, more stable than Euler.
(define (midpoint-step f t state dt)
  (let* ([k1 (f t state)]
         [half-dt (/ dt 2)]
         [mid-state (state-madd state half-dt k1)]
         [k2 (f (+ t half-dt) mid-state)])
        (state-madd state dt k2)))

;;; heun-step : ((Number × State) → State) × Number × State × Number → State
;;; One step of Heun's method (improved Euler, RK2 variant).
;;; k1 = f(t_n, y_n)
;;; k2 = f(t_n + dt, y_n + dt * k1)
;;; y_{n+1} = y_n + dt/2 * (k1 + k2)
;;;
;;; Second-order accurate, uses trapezoid rule.
(define (heun-step f t state dt)
  (let* ([k1 (f t state)]
         [euler-state (state-madd state dt k1)]
         [k2 (f (+ t dt) euler-state)]
         [avg (state-add k1 k2)])
        (state-madd state (/ dt 2) avg)))

;;; rk4-step : ((Number × State) → State) × Number × State × Number → State
;;; One step of classic 4th-order Runge-Kutta.
;;; k1 = f(t_n, y_n)
;;; k2 = f(t_n + dt/2, y_n + dt/2 * k1)
;;; k3 = f(t_n + dt/2, y_n + dt/2 * k2)
;;; k4 = f(t_n + dt, y_n + dt * k3)
;;; y_{n+1} = y_n + dt/6 * (k1 + 2*k2 + 2*k3 + k4)
;;;
;;; Fourth-order accurate, excellent for most non-stiff problems.
(define (rk4-step f t state dt)
  (let* ([half-dt (/ dt 2)]
         [k1 (f t state)]
         [k2 (f (+ t half-dt) (state-madd state half-dt k1))]
         [k3 (f (+ t half-dt) (state-madd state half-dt k2))]
         [k4 (f (+ t dt) (state-madd state dt k3))]
         ;; Weighted combination: (k1 + 2*k2 + 2*k3 + k4) / 6
         [weighted (state-add k1
                              (state-add (state-scale 2 k2)
                                         (state-add (state-scale 2 k3)
                                                    k4)))])
        (state-madd state (/ dt 6) weighted)))

;;; ====
;;; Symplectic Integration Methods
;;; ====

;;; These methods are designed for Hamiltonian systems and preserve
;;; the symplectic structure, leading to better energy conservation
;;; over long integration times.
;;;
;;; For Newtonian mechanics: m * a = F(x)
;;; We split state into position (x) and velocity (v).

;;; symplectic-euler-step : ((Number × Pos) → Accel) × Number × Pos × Vel × Number → (Pos × Vel)
;;; Symplectic Euler method.
;;; v_{n+1} = v_n + dt * a(t_n, x_n)
;;; x_{n+1} = x_n + dt * v_{n+1}
;;;
;;; First-order but symplectic. Updates velocity first, then position.
(define (symplectic-euler-step a t pos vel dt)
  (let* ([accel (a t pos)]
         [new-vel (state-madd vel dt accel)]
         [new-pos (state-madd pos dt new-vel)])
        (list new-pos new-vel)))

;;; symplectic-euler-step-b : ((Number × Pos) → Accel) × Number × Pos × Vel × Number → (Pos × Vel)
;;; Alternative symplectic Euler (updates position first).
;;; x_{n+1} = x_n + dt * v_n
;;; v_{n+1} = v_n + dt * a(t_n, x_{n+1})
(define (symplectic-euler-step-b a t pos vel dt)
  (let* ([new-pos (state-madd pos dt vel)]
         [accel (a (+ t dt) new-pos)]
         [new-vel (state-madd vel dt accel)])
        (list new-pos new-vel)))

;;; verlet-step : ((Number × Pos) → Accel) × Pos × Pos-Prev × Number → (Pos × Pos-Prev)
;;; Störmer-Verlet method (position Verlet).
;;; x_{n+1} = 2*x_n - x_{n-1} + dt² * a(t_n, x_n)
;;;
;;; Second-order, symplectic. Requires previous position.
;;; Returns new position and current position (which becomes prev).
(define (verlet-step a pos pos-prev dt)
  (let* ([t 0]  ; Verlet is autonomous, time not used directly
         [accel (a t pos)]
         [dt-sq (* dt dt)]
         [new-pos (state-add (state-sub (state-scale 2 pos) pos-prev)
                             (state-scale dt-sq accel))])
        (list new-pos pos)))

;;; velocity-verlet-step : ((Number × Pos) → Accel) × Number × Pos × Vel × Number → (Pos × Vel)
;;; Velocity Verlet method.
;;; x_{n+1} = x_n + dt * v_n + dt²/2 * a(t_n, x_n)
;;; v_{n+1} = v_n + dt/2 * (a(t_n, x_n) + a(t_{n+1}, x_{n+1}))
;;;
;;; Second-order, symplectic, gives both position and velocity.
;;; Most commonly used for molecular dynamics and physics engines.
(define (velocity-verlet-step a t pos vel dt)
  (let* ([accel-n (a t pos)]
         [half-dt (/ dt 2)]
         [dt-sq-half (* half-dt dt)]
         ;; x_{n+1} = x_n + dt*v_n + dt²/2 * a_n
         [new-pos (state-madd (state-madd pos dt vel)
                              dt-sq-half accel-n)]
         ;; a_{n+1} from new position
         [accel-n1 (a (+ t dt) new-pos)]
         ;; v_{n+1} = v_n + dt/2 * (a_n + a_{n+1})
         [new-vel (state-madd vel half-dt (state-add accel-n accel-n1))])
        (list new-pos new-vel)))

;;; leapfrog-step : ((Number × Pos) → Accel) × Number × Pos × Vel × Number → (Pos × Vel)
;;; Leapfrog integration (kick-drift-kick variant).
;;; v_{n+1/2} = v_n + dt/2 * a(t_n, x_n)
;;; x_{n+1} = x_n + dt * v_{n+1/2}
;;; v_{n+1} = v_{n+1/2} + dt/2 * a(t_{n+1}, x_{n+1})
;;;
;;; Second-order, symplectic, equivalent to velocity Verlet
;;; but sometimes more convenient for implementation.
(define (leapfrog-step a t pos vel dt)
  (let* ([half-dt (/ dt 2)]
         [accel-n (a t pos)]
         ;; Half-step velocity
         [vel-half (state-madd vel half-dt accel-n)]
         ;; Full-step position
         [new-pos (state-madd pos dt vel-half)]
         ;; Acceleration at new position
         [accel-n1 (a (+ t dt) new-pos)]
         ;; Complete velocity step
         [new-vel (state-madd vel-half half-dt accel-n1)])
        (list new-pos new-vel)))

;;; ====
;;; Multi-Step Integration
;;; ====

;;; integrate : Step × f × Number × State × Number × Nat → (List State)
;;; Integrate an ODE over n steps, returning list of states.
;;; step is one of euler-step, midpoint-step, rk4-step, etc.
(define (integrate step f t0 state0 dt n)
  (let loop ([t t0] [state state0] [i 0] [results (list state0)])
       (if (>= i n)
           (reverse results)
           (let ([new-state (step f t state dt)])
                (loop (+ t dt) new-state (+ i 1) (cons new-state results))))))

;;; integrate-symplectic : Step × a × Number × Pos × Vel × Number × Nat → (List (Pos × Vel))
;;; Integrate a Newtonian system over n steps.
;;; step must have signature (a t pos vel dt) → (pos vel).
;;; Compatible with: symplectic-euler-step, velocity-verlet-step, leapfrog-step.
;;; NOT compatible with verlet-step (different state structure) — use integrate-verlet instead.
(define (integrate-symplectic step a t0 pos0 vel0 dt n)
  (let loop ([t t0] [pos pos0] [vel vel0] [i 0] [results (list (list pos0 vel0))])
       (if (>= i n)
           (reverse results)
           (let* ([result (step a t pos vel dt)]
                  [new-pos (car result)]
                  [new-vel (cadr result)])
                 (loop (+ t dt) new-pos new-vel (+ i 1) (cons result results))))))

;;; integrate-verlet : ((Number × Pos) → Accel) × Number × Pos × Pos-Prev × Number × Nat → (List (Pos × Pos-Prev))
;;; Integrate using Störmer-Verlet, which tracks (pos, pos-prev) instead of (pos, vel).
;;; To bootstrap from (pos, vel), compute pos-prev = pos - vel*dt.
(define (integrate-verlet a t0 pos0 pos-prev0 dt n)
  (let loop ([t t0] [pos pos0] [prev pos-prev0] [i 0] [results (list (list pos0 pos-prev0))])
       (if (>= i n)
           (reverse results)
           (let* ([result (verlet-step a pos prev dt)]
                  [new-pos (car result)]
                  [new-prev (cadr result)])
                 (loop (+ t dt) new-pos new-prev (+ i 1) (cons result results))))))

;;; ====
;;; Adaptive Step Size Control (delegates to ode-adaptive)
;;; ====

;;; rk45-step : backward-compat alias for dp45-step from ode-adaptive
(define rk45-step dp45-step)

;;; integrate-adaptive : f × Number × State × Number × Number × Number × Number → (List (Number × State))
;;; Wrapper around ode-adaptive-integrate with the original API.
;;; Returns list of (time state) pairs (not dotted pairs).
(define (integrate-adaptive f t0 state0 t-end dt-initial tol max-steps)
  (let* ([opts (list (cons 'tolerance tol)
                     (cons 'max-steps max-steps)
                     (cons 'initial-dt dt-initial))]
         [result (ode-adaptive-integrate f t0 state0 t-end opts)]
         [traj (ode-result-trajectory result)])
    ;; Convert (time . state) dotted pairs to (time state) lists
    (map (lambda (pair) (list (car pair) (cdr pair))) traj)))

;;; ====
;;; Energy and Conservation Metrics
;;; ====

;;; kinetic-energy : Vel × Number → Number
;;; Compute kinetic energy: 1/2 * m * v²
(define (kinetic-energy vel mass)
  (* 0.5 mass (apply + (map (lambda (v) (* v v)) vel))))

;;; total-energy : Pos × Vel × Number × (Pos → Number) → Number
;;; Compute total energy: KE + PE
;;; potential-fn: (Pos → Number) gives potential energy at position
(define (total-energy pos vel mass potential-fn)
  (+ (kinetic-energy vel mass)
     (potential-fn pos)))

;;; energy-drift : (List (Pos × Vel)) × Number × (Pos → Number) → (List Number)
;;; Compute energy at each step to measure drift.
(define (energy-drift trajectory mass potential-fn)
  (map (lambda (state)
               (total-energy (car state) (cadr state) mass potential-fn))
       trajectory))

;;; max-energy-drift : (List Number) → Number
;;; Maximum deviation from initial energy.
(define (max-energy-drift energies)
  (let ([e0 (car energies)])
       (apply max (map (lambda (e) (abs (- e e0))) energies))))

;;; relative-energy-error : (List Number) → Number
;;; Maximum relative energy error.
(define (relative-energy-error energies)
  (let ([e0 (car energies)])
       (if (= e0 0)
           0
           (apply max (map (lambda (e) (abs (/ (- e e0) e0))) energies)))))
