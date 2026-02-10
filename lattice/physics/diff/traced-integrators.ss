(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module traced-integrators
;;; @requires prelude vec2 reverse-diff traced-vec2 traced-body
(require 'prelude)
(require 'vec2)
(require 'reverse-diff)
(require 'traced-vec2)
(require 'traced-body)

(doc 'module 'traced-integrators)
(doc 'description "Differentiable Physics Integration

Differentiable numerical integration for physics simulation.
Supports gradient computation through time-stepping for trajectory
optimization and inverse problems.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'symplectic-euler-integration)

;;; The symplectic Euler method is:
;;;   v_{n+1} = v_n + a_n * dt
;;;   x_{n+1} = x_n + v_{n+1} * dt
;;;
;;; This is more stable than standard Euler for Hamiltonian systems
;;; and better preserves energy over long simulations.

;;; traced-euler-step : TracedBody × TracedVec2 × TracedValue × Number → TracedBody
;;; Perform one symplectic Euler integration step.
;;;   body: current body state
;;;   accel: linear acceleration (force / mass)
;;;   torque-over-I: angular acceleration (torque / inertia)
;;;   dt: time step (constant, not traced)
(define (traced-euler-step body accel torque-over-I dt)
  (if (traced-body-static? body)
      body  ; Static bodies don't move
      (let* (;; Update velocities first (symplectic)
             [new-vel (traced-vec2-add (traced-body-vel body)
                                       (traced-vec2-scale accel dt))]
             [new-omega (traced-add (traced-body-angular-vel body)
                                    (traced-mul torque-over-I dt))]
             ;; Then update positions using new velocities
             [new-pos (traced-vec2-add (traced-body-pos body)
                                       (traced-vec2-scale new-vel dt))]
             [new-angle (traced-add (traced-body-angle body)
                                    (traced-mul new-omega dt))])
            (traced-body-with-state body new-pos new-vel new-angle new-omega))))

;;; traced-euler-step-with-force : TracedBody × TracedVec2 × TracedValue × Number → TracedBody
;;; Euler step taking force and torque directly (computes acceleration internally).
(define (traced-euler-step-with-force body force torque dt)
  (if (traced-body-static? body)
      body
      (let* ([accel (traced-vec2-scale force (traced-body-inv-mass body))]
             [torque-over-I (traced-mul torque (traced-body-inv-inertia body))])
            (traced-euler-step body accel torque-over-I dt))))

;;; ====
;;; Standard Euler (for comparison)
;;; ====

;;; traced-euler-step-explicit : TracedBody × TracedVec2 × TracedValue × Number → TracedBody
;;; Standard (explicit) Euler integration.
;;;   x_{n+1} = x_n + v_n * dt
;;;   v_{n+1} = v_n + a_n * dt
;;; Less stable than symplectic but sometimes useful.
(define (traced-euler-step-explicit body accel torque-over-I dt)
  (if (traced-body-static? body)
      body
      (let* (;; Update positions using old velocities
             [new-pos (traced-vec2-add (traced-body-pos body)
                                       (traced-vec2-scale (traced-body-vel body) dt))]
             [new-angle (traced-add (traced-body-angle body)
                                    (traced-mul (traced-body-angular-vel body) dt))]
             ;; Then update velocities
             [new-vel (traced-vec2-add (traced-body-vel body)
                                       (traced-vec2-scale accel dt))]
             [new-omega (traced-add (traced-body-angular-vel body)
                                    (traced-mul torque-over-I dt))])
            (traced-body-with-state body new-pos new-vel new-angle new-omega))))

;;; ====
;;; Velocity Verlet Integration
;;; ====

;;; Velocity Verlet is second-order accurate and symplectic.
;;; Requires computing acceleration at two time points:
;;;   x_{n+1} = x_n + v_n * dt + 0.5 * a_n * dt^2
;;;   a_{n+1} = compute_accel(x_{n+1})
;;;   v_{n+1} = v_n + 0.5 * (a_n + a_{n+1}) * dt

;;; traced-verlet-step : TracedBody × (TracedBody → (Values TracedVec2 TracedValue)) × Number → TracedBody
;;; Perform one Velocity Verlet integration step.
;;;   body: current body state
;;;   compute-accel: function returning (values linear-accel angular-accel) given body state
;;;   dt: time step
(define (traced-verlet-step body compute-accel dt)
  (if (traced-body-static? body)
      body
      (call-with-values
       (lambda () (compute-accel body))
       (lambda (accel-n torque-n)
               (let* ([half-vel (traced-vec2-add (traced-body-vel body)
                                                 (traced-vec2-scale accel-n (* 0.5 dt)))]
                      [half-omega (traced-add (traced-body-angular-vel body)
                                              (traced-mul torque-n (* 0.5 dt)))]
                      [new-pos (traced-vec2-add (traced-body-pos body)
                                                (traced-vec2-add
                                                 (traced-vec2-scale (traced-body-vel body) dt)
                                                 (traced-vec2-scale accel-n (* 0.5 dt dt))))]
                      [new-angle (traced-add (traced-body-angle body)
                                             (traced-add
                                              (traced-mul (traced-body-angular-vel body) dt)
                                              (traced-mul torque-n (* 0.5 dt dt))))]
                      [temp-body (traced-body-with-state body new-pos half-vel new-angle half-omega)])
                     (call-with-values
                      (lambda () (compute-accel temp-body))
                      (lambda (accel-n1 torque-n1)
                              (let* ([new-vel (traced-vec2-add (traced-body-vel body)
                                                               (traced-vec2-scale
                                                                (traced-vec2-add accel-n accel-n1)
                                                                (* 0.5 dt)))]
                                     [new-omega (traced-add (traced-body-angular-vel body)
                                                            (traced-mul
                                                             (traced-add torque-n torque-n1)
                                                             (* 0.5 dt)))])
                                    (traced-body-with-state body new-pos new-vel new-angle new-omega)))))))))

;;; ====
;;; Semi-Implicit Euler (Common in Games)
;;; ====

;;; Also known as symplectic Euler, but sometimes implementations
;;; vary in whether they include damping, clamping, etc.

;;; traced-semi-implicit-step : TracedBody × TracedVec2 × TracedValue × Number × Number → TracedBody
;;; Semi-implicit Euler with optional velocity damping.
;;;   damping: velocity multiplier per step (0.99 typical)
(define (traced-semi-implicit-step body accel torque-over-I dt damping)
  (if (traced-body-static? body)
      body
      (let* (;; Update and damp velocities
             [new-vel-raw (traced-vec2-add (traced-body-vel body)
                                           (traced-vec2-scale accel dt))]
             [new-omega-raw (traced-add (traced-body-angular-vel body)
                                        (traced-mul torque-over-I dt))]
             [new-vel (traced-vec2-scale new-vel-raw damping)]
             [new-omega (traced-mul new-omega-raw damping)]
             ;; Update positions
             [new-pos (traced-vec2-add (traced-body-pos body)
                                       (traced-vec2-scale new-vel dt))]
             [new-angle (traced-add (traced-body-angle body)
                                    (traced-mul new-omega dt))])
            (traced-body-with-state body new-pos new-vel new-angle new-omega))))

;;; ====
;;; Gravity and Common Forces
;;; ====

;;; traced-gravity-accel : TracedVec2 → TracedVec2
;;; Standard gravity acceleration (constant, independent of mass).
;;; gravity is typically (vec2 0 9.8) or (vec2 0 -9.8) depending on coordinate system.
(define (traced-gravity-accel gravity)
  gravity)  ; Gravity is acceleration, not force

;;; traced-gravity-step : TracedBody × TracedVec2 × Number → TracedBody
;;; Apply gravity and integrate for one step.
(define (traced-gravity-step body gravity dt)
  (traced-euler-step body gravity 0 dt))

;;; ====
;;; Multi-Step Integration
;;; ====

;;; traced-integrate-n-steps : TracedBody × (TracedBody → (Values TracedVec2 TracedValue)) × Number × Nat → TracedBody
;;; Integrate for n steps using symplectic Euler.
;;;   compute-accel: returns (values linear-accel angular-accel) for body state
(define (traced-integrate-n-steps body compute-accel dt n)
  (if (= n 0)
      body
      (call-with-values
       (lambda () (compute-accel body))
       (lambda (accel torque)
               (let ([new-body (traced-euler-step body accel torque dt)])
                    (traced-integrate-n-steps new-body compute-accel dt (- n 1)))))))

;;; traced-integrate-until : TracedBody × (TracedBody → TracedVec2 × TracedValue) × Number × Number → TracedBody
;;; Integrate for total time t using fixed timestep dt.
(define (traced-integrate-until body compute-accel dt total-time)
  (let ([n (exact (floor (/ total-time dt)))])
       (traced-integrate-n-steps body compute-accel dt n)))

;;; ====
;;; Point Mass Integration (simplified)
;;; ====

;;; For simple point masses without rotation.

;;; traced-point-step : TracedVec2 × TracedVec2 × TracedVec2 × Number → (TracedVec2 × TracedVec2)
;;; Integrate a point mass: returns (new-pos, new-vel).
(define (traced-point-step pos vel accel dt)
  (let* ([new-vel (traced-vec2-add vel (traced-vec2-scale accel dt))]
         [new-pos (traced-vec2-add pos (traced-vec2-scale new-vel dt))])
        (values new-pos new-vel)))

;;; traced-projectile-step : TracedVec2 × TracedVec2 × TracedVec2 × Number → (TracedVec2 × TracedVec2)
;;; Integrate projectile motion under constant gravity.
(define (traced-projectile-step pos vel gravity dt)
  (traced-point-step pos vel gravity dt))

;;; ====
;;; Energy-Preserving Integration Check
;;; ====

;;; For Hamiltonian systems, symplectic integrators preserve energy
;;; (up to bounded oscillations). This utility helps validate simulations.

;;; traced-total-energy : TracedBody × (TracedBody → TracedValue) → TracedValue
;;; Compute total energy: kinetic + potential.
;;;   potential-fn: function computing potential energy from body state
(define (traced-total-energy body potential-fn)
  (traced-add (traced-body-kinetic-energy body)
              (potential-fn body)))

;;; traced-gravitational-potential : TracedBody × TracedVec2 × Number → TracedValue
;;; Gravitational potential energy: mgh (in direction of gravity).
;;;   ref-height: reference height where PE = 0
(define (traced-gravitational-potential body gravity ref-height)
  (let* ([m (traced-body-mass body)]
         [h (traced-sub (traced-vec2-y (traced-body-pos body)) ref-height)]
         ;; PE = mgh, where g is magnitude of gravity in -y direction
         [g (traced-vec2-magnitude gravity)])
        (traced-mul m (traced-mul g h))))
