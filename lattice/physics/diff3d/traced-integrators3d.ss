;;; core/diff-physics-3d/traced-integrators3d.ss --- Differentiable 3D Physics Integration
;;;
;;; Differentiable numerical integration for 3D physics simulation.
;;; Supports gradient computation through time-stepping for trajectory
;;; optimization and inverse problems.
;;;
;;; Key difference from 2D: quaternion integration requires special care
;;; to maintain unit length and avoid singularities.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/vec3.ss
;;;   - autodiff/reverse-diff.ss
;;;   - diff-physics-3d/traced-vec3.ss
;;;   - diff-physics-3d/traced-quaternion.ss
;;;   - diff-physics-3d/traced-body3d.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec3.ss")
(load "lattice/autodiff/reverse-diff.ss")
(load "lattice/physics/diff3d/traced-vec3.ss")
(load "lattice/physics/diff3d/traced-quaternion.ss")
(load "lattice/physics/diff3d/traced-body3d.ss")

;;; ============================================================
;;; Symplectic Euler Integration for 3D Rigid Bodies
;;; ============================================================

;;; The symplectic Euler method is:
;;;   v_{n+1} = v_n + a_n * dt
;;;   x_{n+1} = x_n + v_{n+1} * dt
;;;   omega_{n+1} = omega_n + alpha_n * dt
;;;   q_{n+1} = normalize(q_n + 0.5 * dt * omega_quat * q_n)
;;;
;;; For quaternion integration, we use the derivative:
;;;   dq/dt = 0.5 * omega_quat * q
;;; where omega_quat = (0, omega_x, omega_y, omega_z)

;;; traced-euler-step-3d : TracedBody3D × TracedVec3 × TracedVec3 × Number → TracedBody3D
;;; Perform one symplectic Euler integration step.
;;;   body: current body state
;;;   accel: linear acceleration (force / mass)
;;;   angular-accel: angular acceleration (I^-1 * torque)
;;;   dt: time step (constant, not traced)
(define (traced-euler-step-3d body accel angular-accel dt)
  (if (traced-body-3d-static? body)
      body  ; Static bodies don't move
      (let* (;; Update velocities first (symplectic)
             [new-vel (traced-vec3-add (traced-body-3d-vel body)
                                       (traced-vec3-scale accel dt))]
             [new-omega (traced-vec3-add (traced-body-3d-angular-vel body)
                                         (traced-vec3-scale angular-accel dt))]
             ;; Update position using new velocity
             [new-pos (traced-vec3-add (traced-body-3d-pos body)
                                       (traced-vec3-scale new-vel dt))]
             ;; Update quaternion using new angular velocity
             ;; q_new = normalize(q + 0.5 * dt * omega_quat * q)
             [new-orientation (traced-quat-integrate (traced-body-3d-orientation body)
                                                     new-omega dt)])
            (traced-body-3d-with-state body new-pos new-vel new-orientation new-omega))))

;;; traced-euler-step-3d-with-force : TracedBody3D × TracedVec3 × TracedVec3 × Number → TracedBody3D
;;; Euler step taking force and torque directly (computes acceleration internally).
(define (traced-euler-step-3d-with-force body force torque dt)
  (if (traced-body-3d-static? body)
      body
      (let* ([accel (traced-vec3-scale force (traced-body-3d-inv-mass body))]
             ;; Angular acceleration: alpha = I_world^-1 * torque
             [I-world-inv (traced-body-3d-world-inv-inertia body)]
             [angular-accel (traced-mat3-mul-vec3 I-world-inv torque)])
            (traced-euler-step-3d body accel angular-accel dt))))

;;; ============================================================
;;; Standard Euler (for comparison)
;;; ============================================================

;;; traced-euler-step-3d-explicit : TracedBody3D × TracedVec3 × TracedVec3 × Number → TracedBody3D
;;; Standard (explicit) Euler integration.
;;;   x_{n+1} = x_n + v_n * dt
;;;   v_{n+1} = v_n + a_n * dt
;;; Less stable than symplectic but sometimes useful.
(define (traced-euler-step-3d-explicit body accel angular-accel dt)
  (if (traced-body-3d-static? body)
      body
      (let* (;; Update positions using old velocities
             [new-pos (traced-vec3-add (traced-body-3d-pos body)
                                       (traced-vec3-scale (traced-body-3d-vel body) dt))]
             [new-orientation (traced-quat-integrate (traced-body-3d-orientation body)
                                                     (traced-body-3d-angular-vel body) dt)]
             ;; Then update velocities
             [new-vel (traced-vec3-add (traced-body-3d-vel body)
                                       (traced-vec3-scale accel dt))]
             [new-omega (traced-vec3-add (traced-body-3d-angular-vel body)
                                         (traced-vec3-scale angular-accel dt))])
            (traced-body-3d-with-state body new-pos new-vel new-orientation new-omega))))

;;; ============================================================
;;; Semi-Implicit Euler with Damping
;;; ============================================================

;;; traced-semi-implicit-step-3d : TracedBody3D × TracedVec3 × TracedVec3 × Number × Number → TracedBody3D
;;; Semi-implicit Euler with optional velocity damping.
;;;   damping: velocity multiplier per step (0.99 typical)
(define (traced-semi-implicit-step-3d body accel angular-accel dt damping)
  (if (traced-body-3d-static? body)
      body
      (let* (;; Update and damp velocities
             [new-vel-raw (traced-vec3-add (traced-body-3d-vel body)
                                           (traced-vec3-scale accel dt))]
             [new-omega-raw (traced-vec3-add (traced-body-3d-angular-vel body)
                                             (traced-vec3-scale angular-accel dt))]
             [new-vel (traced-vec3-scale new-vel-raw damping)]
             [new-omega (traced-vec3-scale new-omega-raw damping)]
             ;; Update positions
             [new-pos (traced-vec3-add (traced-body-3d-pos body)
                                       (traced-vec3-scale new-vel dt))]
             [new-orientation (traced-quat-integrate (traced-body-3d-orientation body)
                                                     new-omega dt)])
            (traced-body-3d-with-state body new-pos new-vel new-orientation new-omega))))

;;; ============================================================
;;; Velocity Verlet Integration
;;; ============================================================

;;; traced-verlet-step-3d : TracedBody3D × (TracedBody3D → (Values TracedVec3 TracedVec3)) × Number → TracedBody3D
;;; Perform one Velocity Verlet integration step.
;;;   body: current body state
;;;   compute-accel: function returning (values linear-accel angular-accel) given body state
;;;   dt: time step
(define (traced-verlet-step-3d body compute-accel dt)
  (if (traced-body-3d-static? body)
      body
      (call-with-values
       (lambda () (compute-accel body))
       (lambda (accel-n angular-accel-n)
               (let* ([half-vel (traced-vec3-add (traced-body-3d-vel body)
                                                 (traced-vec3-scale accel-n (* 0.5 dt)))]
                      [half-omega (traced-vec3-add (traced-body-3d-angular-vel body)
                                                   (traced-vec3-scale angular-accel-n (* 0.5 dt)))]
                      [new-pos (traced-vec3-add (traced-body-3d-pos body)
                                                (traced-vec3-add
                                                 (traced-vec3-scale (traced-body-3d-vel body) dt)
                                                 (traced-vec3-scale accel-n (* 0.5 dt dt))))]
                      [new-orientation (traced-quat-integrate
                                        (traced-body-3d-orientation body)
                                        (traced-body-3d-angular-vel body) dt)]
                      [temp-body (traced-body-3d-with-state body new-pos half-vel
                                                            new-orientation half-omega)])
                     (call-with-values
                      (lambda () (compute-accel temp-body))
                      (lambda (accel-n1 angular-accel-n1)
                              (let* ([new-vel (traced-vec3-add (traced-body-3d-vel body)
                                                               (traced-vec3-scale
                                                                (traced-vec3-add accel-n accel-n1)
                                                                (* 0.5 dt)))]
                                     [new-omega (traced-vec3-add (traced-body-3d-angular-vel body)
                                                                 (traced-vec3-scale
                                                                  (traced-vec3-add angular-accel-n angular-accel-n1)
                                                                  (* 0.5 dt)))])
                                    (traced-body-3d-with-state body new-pos new-vel new-orientation new-omega)))))))))

;;; ============================================================
;;; Gravity and Common Forces
;;; ============================================================

;;; traced-gravity-accel-3d : TracedVec3 → TracedVec3
;;; Standard gravity acceleration (constant, independent of mass).
;;; gravity is typically (vec3 0 -9.8 0) or (vec3 0 0 -9.8) depending on convention.
(define (traced-gravity-accel-3d gravity)
  gravity)  ; Gravity is acceleration, not force

;;; traced-gravity-step-3d : TracedBody3D × TracedVec3 × Number → TracedBody3D
;;; Apply gravity and integrate for one step.
(define (traced-gravity-step-3d body gravity dt)
  (let ([zero-angular-accel (traced-vec3-zero (traced-tape (traced-vec3-x (traced-body-3d-pos body))))])
       (traced-euler-step-3d body gravity zero-angular-accel dt)))

;;; traced-gravity-step-3d-const : TracedBody3D × Vec3 × Number → TracedBody3D
;;; Apply constant (non-traced) gravity and integrate for one step.
(define (traced-gravity-step-3d-const body gravity-vec dt)
  (let ([tape (traced-tape (traced-vec3-x (traced-body-3d-pos body)))])
       (if tape
           (traced-gravity-step-3d body (lift-vec3 gravity-vec tape) dt)
           (traced-gravity-step-3d body (lift-vec3-const gravity-vec) dt))))

;;; ============================================================
;;; Multi-Step Integration
;;; ============================================================

;;; traced-integrate-n-steps-3d : TracedBody3D × (TracedBody3D → (Values TracedVec3 TracedVec3)) × Number × Nat → TracedBody3D
;;; Integrate for n steps using symplectic Euler.
;;;   compute-accel: returns (values linear-accel angular-accel) for body state
(define (traced-integrate-n-steps-3d body compute-accel dt n)
  (if (= n 0)
      body
      (call-with-values
       (lambda () (compute-accel body))
       (lambda (accel angular-accel)
               (let ([new-body (traced-euler-step-3d body accel angular-accel dt)])
                    (traced-integrate-n-steps-3d new-body compute-accel dt (- n 1)))))))

;;; traced-integrate-until-3d : TracedBody3D × (TracedBody3D → (Values TracedVec3 TracedVec3)) × Number × Number → TracedBody3D
;;; Integrate for total time t using fixed timestep dt.
(define (traced-integrate-until-3d body compute-accel dt total-time)
  (let ([n (exact (floor (/ total-time dt)))])
       (traced-integrate-n-steps-3d body compute-accel dt n)))

;;; ============================================================
;;; Integration with Trajectory Recording
;;; ============================================================

;;; traced-integrate-with-trajectory-3d : TracedBody3D × (TracedBody3D → (Values TracedVec3 TracedVec3)) × Number × Nat → (List TracedBody3D)
;;; Integrate for n steps and return all intermediate states.
(define (traced-integrate-with-trajectory-3d body compute-accel dt n)
  (let loop ([b body] [steps n] [traj (list body)])
       (if (= steps 0)
           (reverse traj)
           (call-with-values
            (lambda () (compute-accel b))
            (lambda (accel angular-accel)
                    (let ([new-body (traced-euler-step-3d b accel angular-accel dt)])
                         (loop new-body (- steps 1) (cons new-body traj))))))))

;;; ============================================================
;;; Point Mass Integration (simplified, 3D)
;;; ============================================================

;;; traced-point-step-3d : TracedVec3 × TracedVec3 × TracedVec3 × Number → (Values TracedVec3 TracedVec3)
;;; Integrate a point mass: returns (new-pos, new-vel).
(define (traced-point-step-3d pos vel accel dt)
  (let* ([new-vel (traced-vec3-add vel (traced-vec3-scale accel dt))]
         [new-pos (traced-vec3-add pos (traced-vec3-scale new-vel dt))])
        (values new-pos new-vel)))

;;; traced-projectile-step-3d : TracedVec3 × TracedVec3 × TracedVec3 × Number → (Values TracedVec3 TracedVec3)
;;; Integrate projectile motion under constant gravity.
(define (traced-projectile-step-3d pos vel gravity dt)
  (traced-point-step-3d pos vel gravity dt))

;;; ============================================================
;;; Energy-Preserving Integration Check
;;; ============================================================

;;; traced-total-energy-3d : TracedBody3D × (TracedBody3D → TracedValue) → TracedValue
;;; Compute total energy: kinetic + potential.
;;;   potential-fn: function computing potential energy from body state
(define (traced-total-energy-3d body potential-fn)
  (traced-add (traced-body-3d-kinetic-energy body)
              (potential-fn body)))

;;; traced-gravitational-potential-3d : TracedBody3D × TracedVec3 × Number → TracedValue
;;; Gravitational potential energy: mgh (height in gravity direction).
;;;   gravity: gravity vector (e.g., (0, -9.8, 0))
;;;   ref-height: reference height where PE = 0
(define (traced-gravitational-potential-3d body gravity ref-height)
  (let* ([m (traced-body-3d-mass body)]
         [g-mag (traced-vec3-magnitude gravity)]
         ;; Height is negative dot product with normalized gravity direction
         [g-dir (traced-vec3-smooth-normalize gravity 1e-10)]
         [pos (traced-body-3d-pos body)]
         [h (traced-sub (traced-neg (traced-vec3-dot pos g-dir)) ref-height)])
        (traced-mul m (traced-mul g-mag h))))

;;; ============================================================
;;; Spring Forces (useful for testing)
;;; ============================================================

;;; traced-spring-force-3d : TracedVec3 × TracedVec3 × Number × Number → TracedVec3
;;; Compute spring force between two points.
;;;   anchor: fixed anchor point
;;;   point: current position
;;;   k: spring constant
;;;   rest-length: natural length of spring
(define (traced-spring-force-3d anchor point k rest-length)
  (let* ([diff (traced-vec3-sub anchor point)]
         [dist (traced-vec3-smooth-magnitude diff 1e-10)]
         [stretch (traced-sub dist rest-length)]
         [direction (traced-vec3-smooth-normalize diff 1e-10)])
        (traced-vec3-scale direction (traced-mul k stretch))))

;;; traced-damped-spring-force-3d : TracedVec3 × TracedVec3 × TracedVec3 × Number × Number × Number → TracedVec3
;;; Compute damped spring force.
;;;   anchor: fixed anchor point
;;;   point: current position
;;;   velocity: current velocity
;;;   k: spring constant
;;;   rest-length: natural length
;;;   damping: damping coefficient
(define (traced-damped-spring-force-3d anchor point velocity k rest-length damping)
  (let* ([spring-f (traced-spring-force-3d anchor point k rest-length)]
         [damp-f (traced-vec3-scale velocity (- damping))])
        (traced-vec3-add spring-f damp-f)))
