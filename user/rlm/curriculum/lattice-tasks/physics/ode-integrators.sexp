;;; ode-integrators.sexp — Development tasks for lattice/physics/classical/ode-integrators.ss
;;;
;;; Module: ode-integrators (tier 0, depends on prelude)
;;; 339 lines, ~20 exported functions
;;; Numerical integration (ODE solving): state vector operations,
;;; explicit methods (Euler, midpoint, Heun, RK4), symplectic methods
;;; (Verlet, velocity-Verlet, leapfrog), adaptive step size (RK4(5)),
;;; energy conservation metrics.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement euler-step
  ;; ──────────────────────────────────────────────
  ;; The simplest ODE integrator — first-order forward Euler.

  (task
    (id "physics/ode-integrators/euler-step-impl")
    (module ode-integrators)
    (tier 0)
    (type implement)
    (description "Implement euler-step: one step of the forward Euler method for solving ordinary differential equations. Given a derivative function f(t, y), current time t, current state y, and timestep dt, compute the next state: y_{n+1} = y_n + dt * f(t_n, y_n). First evaluate the derivative f at the current time and state, then use state-madd to compute the fused multiply-add: state + dt * derivative. State vectors are lists of numbers. This is the simplest integrator — first-order accurate, O(dt) error per step.")
    (context "Available: state-madd (computes a + k*b for vectors). Two operations: evaluate derivative, then state-madd.")
    (before "(define (euler-step f t state dt)
  ;; TODO: y_{n+1} = y_n + dt * f(t_n, y_n)
  state)")
    (test-file "lattice/physics/classical/test-ode-integrators.ss")
    (test-forms (";; y' = y, y(0) = 1, dt = 0.1 → y(0.1) ≈ 1.1
(let ([f (lambda (t y) y)])
  (assert-equal '(1.1) (euler-step f 0 '(1.0) 0.1)))"
                 ";; y' = -y, y(0) = 1, dt = 0.1 → y(0.1) = 0.9
(let ([f (lambda (t y) (map - y))])
  (assert-equal '(0.9) (euler-step f 0 '(1.0) 0.1)))"
                 ";; 2D system: constant derivative (1, 2)
(let ([f (lambda (t y) '(1 2))])
  (assert-equal '(1.1 2.2) (euler-step f 0 '(1.0 2.0) 0.1)))"
                 ";; Zero derivative: state unchanged
(let ([f (lambda (t y) '(0 0))])
  (assert-equal '(5 3) (euler-step f 0 '(5 3) 0.1)))"))
    (available-modules (prelude))
    (hints ("Evaluate: (f t state) to get the derivative vector"
            "Return: (state-madd state dt derivative)"))
    (reference-solution "(define (euler-step f t state dt)
  (let ([derivative (f t state)])
       (state-madd state dt derivative)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement midpoint-step
  ;; ──────────────────────────────────────────────
  ;; Second-order Runge-Kutta — evaluate at the midpoint.

  (task
    (id "physics/ode-integrators/midpoint-step-impl")
    (module ode-integrators)
    (tier 0)
    (type implement)
    (description "Implement midpoint-step: one step of the midpoint method (RK2). This is a second-order method that evaluates the derivative at the midpoint of the interval for better accuracy. Steps: (1) k1 = f(t, state) — derivative at current point. (2) Compute half-dt = dt/2. (3) mid-state = state + half-dt * k1 — Euler half-step to midpoint. (4) k2 = f(t + half-dt, mid-state) — derivative at midpoint. (5) Return state + dt * k2 — full step using midpoint derivative. The midpoint derivative is a better estimate of the average derivative over the interval, giving O(dt^2) accuracy.")
    (context "Available: state-madd, /, +, let*. Five bindings: k1, half-dt, mid-state, k2, result.")
    (before "(define (midpoint-step f t state dt)
  ;; TODO: evaluate derivative at midpoint for better accuracy
  state)")
    (test-file "lattice/physics/classical/test-ode-integrators.ss")
    (test-forms (";; y' = y, y(0) = 1, dt = 0.1 → exact: e^0.1 ≈ 1.10517
;; Midpoint gives 1.105 (much better than Euler's 1.1)
(let* ([f (lambda (t y) y)]
       [result (midpoint-step f 0 '(1.0) 0.1)])
  (assert-true (< (abs (- (car result) 1.105)) 0.001)))"
                 ";; Constant derivative: midpoint = Euler (both exact)
(let ([f (lambda (t y) '(1 2))])
  (assert-equal '(1.1 2.2) (midpoint-step f 0 '(1.0 2.0) 0.1)))"
                 ";; y' = t, y(0) = 0, dt = 1 → exact: 0.5
;; Midpoint: k1=f(0,0)=0, mid=0, k2=f(0.5,0)=0.5, result=0.5 (exact!)
(let* ([f (lambda (t y) (list t))]
       [result (midpoint-step f 0 '(0) 1.0)])
  (assert-true (< (abs (- (car result) 0.5)) 0.001)))"))
    (available-modules (prelude))
    (hints ("k1 = (f t state)"
            "half-dt = (/ dt 2)"
            "mid-state = (state-madd state half-dt k1)"
            "k2 = (f (+ t half-dt) mid-state)"
            "Return: (state-madd state dt k2)"))
    (reference-solution "(define (midpoint-step f t state dt)
  (let* ([k1 (f t state)]
         [half-dt (/ dt 2)]
         [mid-state (state-madd state half-dt k1)]
         [k2 (f (+ t half-dt) mid-state)])
        (state-madd state dt k2)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement rk4-step
  ;; ──────────────────────────────────────────────
  ;; The classic fourth-order Runge-Kutta — workhorse of numerical ODE solving.

  (task
    (id "physics/ode-integrators/rk4-step-impl")
    (module ode-integrators)
    (tier 0)
    (type implement)
    (description "Implement rk4-step: one step of the classic 4th-order Runge-Kutta method. This is the most widely used explicit ODE integrator. It evaluates the derivative at four points and takes a weighted average. Steps: (1) half-dt = dt/2. (2) k1 = f(t, state). (3) k2 = f(t + half-dt, state + half-dt * k1). (4) k3 = f(t + half-dt, state + half-dt * k2). (5) k4 = f(t + dt, state + dt * k3). (6) weighted = k1 + 2*k2 + 2*k3 + k4 (using state-add and state-scale). (7) Return state + (dt/6) * weighted. The 1:2:2:1 weighting gives fourth-order accuracy O(dt^4).")
    (context "Available: state-madd, state-add, state-scale, /, +, let*. Seven bindings forming the RK4 tableau.")
    (before "(define (rk4-step f t state dt)
  ;; TODO: classic 4th-order Runge-Kutta
  state)")
    (test-file "lattice/physics/classical/test-ode-integrators.ss")
    (test-forms (";; y' = y, y(0) = 1, dt = 0.1 → exact: e^0.1 ≈ 1.105170918
;; RK4 gives 1.10517083... (very close!)
(let* ([f (lambda (t y) y)]
       [result (rk4-step f 0 '(1.0) 0.1)])
  (assert-true (< (abs (- (car result) 1.10517091808)) 0.0001)))"
                 ";; Constant derivative: all methods agree
(let ([f (lambda (t y) '(1 2))])
  (assert-equal '(1.1 2.2) (rk4-step f 0 '(1.0 2.0) 0.1)))"
                 ";; y' = -y, y(0) = 1, dt = 0.1 → exact: e^{-0.1} ≈ 0.904837
(let* ([f (lambda (t y) (map - y))]
       [result (rk4-step f 0 '(1.0) 0.1)])
  (assert-true (< (abs (- (car result) 0.904837)) 0.001)))"))
    (available-modules (prelude))
    (hints ("half-dt = (/ dt 2)"
            "k1 = (f t state)"
            "k2 = (f (+ t half-dt) (state-madd state half-dt k1))"
            "k3 = (f (+ t half-dt) (state-madd state half-dt k2))"
            "k4 = (f (+ t dt) (state-madd state dt k3))"
            "weighted = (state-add k1 (state-add (state-scale 2 k2) (state-add (state-scale 2 k3) k4)))"
            "Return: (state-madd state (/ dt 6) weighted)"))
    (reference-solution "(define (rk4-step f t state dt)
  (let* ([half-dt (/ dt 2)]
         [k1 (f t state)]
         [k2 (f (+ t half-dt) (state-madd state half-dt k1))]
         [k3 (f (+ t half-dt) (state-madd state half-dt k2))]
         [k4 (f (+ t dt) (state-madd state dt k3))]
         [weighted (state-add k1
                              (state-add (state-scale 2 k2)
                                         (state-add (state-scale 2 k3)
                                                    k4)))])
        (state-madd state (/ dt 6) weighted)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement velocity-verlet-step
  ;; ──────────────────────────────────────────────
  ;; Symplectic integrator for Hamiltonian systems.

  (task
    (id "physics/ode-integrators/velocity-verlet-impl")
    (module ode-integrators)
    (tier 0)
    (type implement)
    (description "Implement velocity-verlet-step: one step of the velocity Verlet method for Newtonian mechanics. This is the most common integrator for molecular dynamics and physics engines because it is symplectic (preserves phase space volume) and gives excellent energy conservation over long integrations. Given acceleration function a(t, pos), current time, position, velocity, and timestep: (1) accel-n = a(t, pos) — acceleration at current position. (2) half-dt = dt/2, dt-sq-half = half-dt * dt. (3) new-pos = pos + dt*vel + dt^2/2 * accel-n — position update. (4) accel-n1 = a(t+dt, new-pos) — acceleration at new position. (5) new-vel = vel + dt/2 * (accel-n + accel-n1) — velocity from average acceleration. Return (list new-pos new-vel).")
    (context "Available: state-madd, state-add, /, *, +, list, let*. Six bindings for the two-phase update.")
    (before "(define (velocity-verlet-step a t pos vel dt)
  ;; TODO: symplectic position+velocity update
  (list pos vel))")
    (test-file "lattice/physics/classical/test-ode-integrators.ss")
    (test-forms (";; Free fall: a=(0,-9.8), pos=(0,10), vel=(5,0), dt=0.1
(let* ([a (lambda (t pos) '(0 -9.8))]
       [result (velocity-verlet-step a 0 '(0 10) '(5 0) 0.1)]
       [new-pos (car result)]
       [new-vel (cadr result)])
  ;; pos: (0+0.5+0, 10+0-0.049) = (0.5, 9.951)
  (assert-true (< (abs (- (car new-pos) 0.5)) 0.001))
  (assert-true (< (abs (- (cadr new-pos) 9.951)) 0.001))
  ;; vel: (5+0, 0-0.98) = (5, -0.98)
  (assert-true (< (abs (- (car new-vel) 5)) 0.001))
  (assert-true (< (abs (- (cadr new-vel) -0.98)) 0.001)))"
                 ";; Zero acceleration: uniform motion
(let* ([a (lambda (t pos) '(0 0))]
       [result (velocity-verlet-step a 0 '(0 0) '(1 2) 0.5)]
       [new-pos (car result)]
       [new-vel (cadr result)])
  (assert-equal '(0.5 1.0) new-pos)
  (assert-equal '(1 2) new-vel))"
                 ";; Returns (pos vel) pair
(let* ([a (lambda (t pos) '(1))]
       [result (velocity-verlet-step a 0 '(0) '(0) 1.0)])
  (assert-equal 2 (length result)))"))
    (available-modules (prelude))
    (hints ("accel-n = (a t pos)"
            "half-dt = (/ dt 2), dt-sq-half = (* half-dt dt)"
            "new-pos = (state-madd (state-madd pos dt vel) dt-sq-half accel-n)"
            "accel-n1 = (a (+ t dt) new-pos)"
            "new-vel = (state-madd vel half-dt (state-add accel-n accel-n1))"
            "Return: (list new-pos new-vel)"))
    (reference-solution "(define (velocity-verlet-step a t pos vel dt)
  (let* ([accel-n (a t pos)]
         [half-dt (/ dt 2)]
         [dt-sq-half (* half-dt dt)]
         [new-pos (state-madd (state-madd pos dt vel)
                              dt-sq-half accel-n)]
         [accel-n1 (a (+ t dt) new-pos)]
         [new-vel (state-madd vel half-dt (state-add accel-n accel-n1))])
        (list new-pos new-vel)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement kinetic-energy
  ;; ──────────────────────────────────────────────
  ;; Energy computation for conservation checking.

  (task
    (id "physics/ode-integrators/kinetic-energy-impl")
    (module ode-integrators)
    (tier 0)
    (type implement)
    (description "Implement kinetic-energy: compute the kinetic energy KE = (1/2) * m * |v|^2 from a velocity vector and mass. The velocity magnitude squared is the sum of squares of all components: |v|^2 = v_x^2 + v_y^2 + ... Map each velocity component to its square, sum with apply +, then multiply by 0.5 * mass. This is used for energy conservation checks — symplectic integrators like velocity-Verlet preserve total energy much better than non-symplectic methods.")
    (context "Available: apply, +, map, lambda, *, 0.5. One map, one apply +, one multiplication.")
    (before "(define (kinetic-energy vel mass)
  ;; TODO: KE = 0.5 * m * |v|^2
  0)")
    (test-file "lattice/physics/classical/test-ode-integrators.ss")
    (test-forms (";; 3-4-5: v=(3,4), m=2 → 0.5*2*25 = 25
(assert-equal 25.0 (kinetic-energy '(3 4) 2))"
                 ";; Zero velocity: KE = 0
(assert-equal 0.0 (kinetic-energy '(0 0 0) 5))"
                 ";; 1D: v=(6), m=1 → 0.5*1*36 = 18
(assert-equal 18.0 (kinetic-energy '(6) 1))"
                 ";; Unit mass, unit velocity: KE = 0.5
(assert-equal 0.5 (kinetic-energy '(1) 1))"))
    (available-modules (prelude))
    (hints ("|v|^2 = (apply + (map (lambda (v) (* v v)) vel))"
            "Return: (* 0.5 mass |v|^2)"))
    (reference-solution "(define (kinetic-energy vel mass)
  (* 0.5 mass (apply + (map (lambda (v) (* v v)) vel))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
