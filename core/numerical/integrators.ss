;;; fabric/stitches/numerical/integrators.ss — Numerical Integration Methods
;;;
;;; Provides numerical integration (ODE solving) methods for physics
;;; and scientific computing.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Methods implemented:
;;;   Explicit methods:
;;;     - euler-step: First-order Euler method
;;;     - midpoint-step: Second-order midpoint (RK2)
;;;     - rk4-step: Fourth-order Runge-Kutta
;;;   Symplectic methods (energy-conserving):
;;;     - verlet-step: Störmer-Verlet (position Verlet)
;;;     - velocity-verlet-step: Velocity Verlet
;;;     - leapfrog-step: Leapfrog (equivalent to Verlet)
;;;
;;; Interface conventions:
;;;   - f: (t, state) → derivative (for general ODEs)
;;;   - a: (t, pos) → acceleration (for Newtonian mechanics)
;;;   - state, pos, vel are vectors (lists of numbers)
;;;   - t is time (number)
;;;   - dt is timestep (number)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec.ss

(load "core/prelude.ss")
(load "core/vec.ss")

;;; ============================================================
;;; Vector Operations for Integrators
;;; ============================================================

;;; These work on state vectors represented as lists of numbers
;;; for simplicity and compatibility with the rest of the codebase.

;;; state-add : (List Number) × (List Number) → (List Number)
;;; Add two state vectors.
(define (state-add a b)
  (map + a b))

;;; state-sub : (List Number) × (List Number) → (List Number)
;;; Subtract two state vectors.
(define (state-sub a b)
  (map - a b))

;;; state-scale : Number × (List Number) → (List Number)
;;; Scale a state vector by a scalar.
(define (state-scale k v)
  (map (lambda (x) (* k x)) v))

;;; state-madd : (List Number) × Number × (List Number) → (List Number)
;;; Multiply-add: a + k*b (fused for efficiency)
(define (state-madd a k b)
  (map (lambda (ai bi) (+ ai (* k bi))) a b))

;;; ============================================================
;;; Explicit Integration Methods
;;; ============================================================

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

;;; ============================================================
;;; Symplectic Integration Methods
;;; ============================================================

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

;;; ============================================================
;;; Multi-Step Integration
;;; ============================================================

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
(define (integrate-symplectic step a t0 pos0 vel0 dt n)
  (let loop ([t t0] [pos pos0] [vel vel0] [i 0] [results (list (list pos0 vel0))])
       (if (>= i n)
           (reverse results)
           (let* ([result (step a t pos vel dt)]
                  [new-pos (car result)]
                  [new-vel (cadr result)])
                 (loop (+ t dt) new-pos new-vel (+ i 1) (cons result results))))))

;;; ============================================================
;;; Adaptive Step Size Control
;;; ============================================================

;;; rk45-step : ((Number × State) → State) × Number × State × Number × Number → (State × Number × Number)
;;; Embedded RK4(5) method (Dormand-Prince coefficients).
;;; Returns (new-state, error-estimate, suggested-dt).
;;;
;;; Uses 4th order result and 5th order error estimate for adaptive stepping.
(define (rk45-step f t state dt tol)
  ;; Dormand-Prince coefficients (simplified version)
  (let* ([k1 (f t state)]
         [k2 (f (+ t (* 0.2 dt))
                (state-madd state (* 0.2 dt) k1))]
         [k3 (f (+ t (* 0.3 dt))
                (state-add state
                           (state-add (state-scale (* 3/40 dt) k1)
                                      (state-scale (* 9/40 dt) k2))))]
         [k4 (f (+ t (* 0.8 dt))
                (state-add state
                           (state-add (state-scale (* 44/45 dt) k1)
                                      (state-add (state-scale (* -56/15 dt) k2)
                                                 (state-scale (* 32/9 dt) k3)))))]
         [k5 (f (+ t (* 8/9 dt))
                (state-add state
                           (state-add (state-scale (* 19372/6561 dt) k1)
                                      (state-add (state-scale (* -25360/2187 dt) k2)
                                                 (state-add (state-scale (* 64448/6561 dt) k3)
                                                            (state-scale (* -212/729 dt) k4))))))]
         [k6 (f (+ t dt)
                (state-add state
                           (state-add (state-scale (* 9017/3168 dt) k1)
                                      (state-add (state-scale (* -355/33 dt) k2)
                                                 (state-add (state-scale (* 46732/5247 dt) k3)
                                                            (state-add (state-scale (* 49/176 dt) k4)
                                                                       (state-scale (* -5103/18656 dt) k5)))))))]
         ;; 5th order solution
         [y5 (state-add state
                        (state-scale dt
                                     (state-add (state-scale 35/384 k1)
                                                (state-add (state-scale 500/1113 k3)
                                                           (state-add (state-scale 125/192 k4)
                                                                      (state-add (state-scale -2187/6784 k5)
                                                                                 (state-scale 11/84 k6)))))))]
         ;; 4th order solution (for error estimate)
         [k7 (f (+ t dt) y5)]
         [y4 (state-add state
                        (state-scale dt
                                     (state-add (state-scale 5179/57600 k1)
                                                (state-add (state-scale 7571/16695 k3)
                                                           (state-add (state-scale 393/640 k4)
                                                                      (state-add (state-scale -92097/339200 k5)
                                                                                 (state-add (state-scale 187/2100 k6)
                                                                                            (state-scale 1/40 k7))))))))]
         ;; Error estimate
         [error-vec (state-sub y5 y4)]
         [error (sqrt (/ (apply + (map (lambda (e) (* e e)) error-vec))
                         (length error-vec)))]
         ;; Suggested new step size (PI controller)
         [safety 0.9]
         [new-dt (* dt safety (expt (/ tol (max error 1e-10)) 0.2))])
        (list y5 error new-dt)))

;;; integrate-adaptive : f × Number × State × Number × Number × Number × Number → (List (Number × State))
;;; Integrate with adaptive step size control.
;;; Returns list of (time, state) pairs.
(define (integrate-adaptive f t0 state0 t-end dt-initial tol max-steps)
  (let loop ([t t0] [state state0] [dt dt-initial] [steps 0] [results (list (list t0 state0))])
       (cond
        [(>= t t-end) (reverse results)]
        [(>= steps max-steps) (reverse results)]
        [else
         (let* ([dt-clamped (min dt (- t-end t))]
                [result (rk45-step f t state dt-clamped tol)]
                [new-state (car result)]
                [error (cadr result)]
                [suggested-dt (caddr result)])
               (if (< error tol)
                   ;; Accept step
                   (loop (+ t dt-clamped) new-state
                         (min suggested-dt (* 2 dt))  ; Don't grow too fast
                         (+ steps 1)
                         (cons (list (+ t dt-clamped) new-state) results))
                   ;; Reject step, try again with smaller dt
                   (loop t state (/ dt 2) steps results)))])))

;;; ============================================================
;;; Energy and Conservation Metrics
;;; ============================================================

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
