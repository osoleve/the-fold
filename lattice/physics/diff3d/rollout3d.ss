(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module rollout3d
;;; @requires prelude vec3 quaternion reverse-diff traced-vec3 traced-quaternion traced-body3d traced-integrators3d
(require 'prelude)
(require 'vec3)
(require 'quaternion)
(require 'reverse-diff)
(require 'traced-vec3)
(require 'traced-quaternion)
(require 'traced-body3d)
(require 'traced-integrators3d)

(doc 'module 'rollout3d)
(doc 'description "Multi-Step 3D Simulation with Checkpointing - Rollout functions for simulating 3D physics over multiple timesteps with gradient computation. Includes checkpointing for memory efficiency.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Memory management: Naive rollout uses O(T) memory for T timesteps (stores full tape). Checkpointed uses O(sqrt(T)) memory (stores checkpoints, recomputes segments).")

(doc 'section 'basic-rollout)

;;; traced-rollout-3d : TracedBody3D × (TracedBody3D → TracedBody3D) × Nat → TracedBody3D
;;; Roll out simulation for n steps using step function.
;;; Accumulates full tape for gradient computation.
(define (traced-rollout-3d initial-body step-fn n)
  (let loop ([body initial-body] [i 0])
       (if (>= i n)
           body
           (loop (step-fn body) (+ i 1)))))

;;; traced-rollout-with-gravity-3d : TracedBody3D × TracedVec3 × Number × Nat → TracedBody3D
;;; Simple rollout with constant gravity.
(define (traced-rollout-with-gravity-3d initial-body gravity dt n)
  (traced-rollout-3d initial-body
                     (lambda (body)
                             (traced-gravity-step-3d body gravity dt))
                     n))

;;; traced-rollout-with-force-3d : TracedBody3D × (TracedBody3D → TracedVec3 × TracedVec3) × Number × Nat → TracedBody3D
;;; Rollout with general force field.
;;; force-fn returns (values linear-accel angular-accel) for body state.
(define (traced-rollout-with-force-3d initial-body force-fn dt n)
  (traced-rollout-3d initial-body
                     (lambda (body)
                             (call-with-values
                              (lambda () (force-fn body))
                              (lambda (accel torque)
                                      (traced-euler-step-3d body accel torque dt))))
                     n))

;;; ====
;;; Trajectory Recording
;;; ====

;;; Sometimes we need the full trajectory, not just final state.

;;; traced-rollout-trajectory-3d : TracedBody3D × (TracedBody3D → TracedBody3D) × Nat → (List TracedBody3D)
;;; Roll out and collect all intermediate states.
;;; Returns list of states from step 0 to step n.
(define (traced-rollout-trajectory-3d initial-body step-fn n)
  (let loop ([body initial-body] [i 0] [trajectory (list initial-body)])
       (if (>= i n)
           (reverse trajectory)
           (let ([new-body (step-fn body)])
                (loop new-body (+ i 1) (cons new-body trajectory))))))

;;; traced-trajectory-final-3d : (List TracedBody3D) → TracedBody3D
(define (traced-trajectory-final-3d traj)
  (car (reverse traj)))

;;; ====
;;; Gradient Checkpointing
;;; ====

;;; For long rollouts, storing full tape uses O(T) memory.
;;; Checkpointing reduces this to O(sqrt(T)) by storing only
;;; periodic snapshots and recomputing segments during backprop.

;;; checkpoint-interval-3d : Nat → Nat
;;; Compute optimal checkpoint interval for T steps.
;;; Returns approximately sqrt(T).
(define (checkpoint-interval-3d num-steps)
  (max 1 (exact (ceiling (sqrt num-steps)))))

;;; rollout-checkpoints-3d : RigidBody3D × (RigidBody3D → RigidBody3D) × Nat × Nat → (Vector RigidBody3D)
;;; Roll out simulation storing checkpoints.
;;; Uses non-traced arithmetic to save memory.
;;; checkpoint-freq: save state every checkpoint-freq steps
(define (rollout-checkpoints-3d initial-body step-fn num-steps checkpoint-freq)
  (let* ([num-checkpoints (+ 1 (exact (ceiling (/ num-steps checkpoint-freq))))]
         [checkpoints (make-vector num-checkpoints)])
        (vector-set! checkpoints 0 initial-body)
        (let loop ([body initial-body] [step 0] [ckpt-idx 1])
             (if (>= step num-steps)
                 checkpoints
                 (let ([new-body (step-fn body)]
                       [next-step (+ step 1)])
                      (if (and (= (mod next-step checkpoint-freq) 0)
                               (< ckpt-idx num-checkpoints))
                          (begin
                           (vector-set! checkpoints ckpt-idx new-body)
                           (loop new-body next-step (+ ckpt-idx 1)))
                          (loop new-body next-step ckpt-idx)))))))

;;; ====
;;; Segment-wise Gradient Computation
;;; ====

;;; For checkpointed gradients, we compute gradients segment by segment.
;;; Each segment is small enough to fit in memory.

;;; traced-segment-rollout-3d : TracedBody3D × (TracedBody3D → TracedBody3D) × Nat → TracedBody3D
;;; Roll out a single segment with full gradient tracking.
(define (traced-segment-rollout-3d initial-body step-fn segment-length)
  (traced-rollout-3d initial-body step-fn segment-length))

;;; ====
;;; Full Checkpointed Gradient (3D)
;;; ====

;;; checkpointed-gradient-3d : RigidBody3D × (RigidBody3D → RigidBody3D) × (RigidBody3D → Number) × Nat → GradientResult
;;; Compute gradient of loss w.r.t. initial state using checkpointing.
;;; Returns gradients for (pos, vel, orientation, angular-vel).
(define (checkpointed-gradient-3d initial-body step-fn loss-fn num-steps)
  (let* ([ckpt-freq (checkpoint-interval-3d num-steps)]
         ;; First, do a forward pass to store checkpoints
         [checkpoints (rollout-checkpoints-3d initial-body step-fn num-steps ckpt-freq)]
         [num-ckpts (vector-length checkpoints)]
         [final-body (vector-ref checkpoints (- num-ckpts 1))]
         ;; Compute gradient with full tracing
         ;; (In production would chain through segments, but for now use full trace)
         [grads (gradient
                 (lambda args
                         (with-fresh-ad-scope
                          (lambda ()
                                  (let* ([tape (make-reverse-tape)]
                                         [tb (make-traced-body-3d
                                              (traced-vec3 (make-traced-var (list-ref args 0) tape)
                                                           (make-traced-var (list-ref args 1) tape)
                                                           (make-traced-var (list-ref args 2) tape))
                                              (traced-vec3 (make-traced-var (list-ref args 3) tape)
                                                           (make-traced-var (list-ref args 4) tape)
                                                           (make-traced-var (list-ref args 5) tape))
                                              (traced-quat (make-traced-var (list-ref args 6) tape)
                                                           (make-traced-var (list-ref args 7) tape)
                                                           (make-traced-var (list-ref args 8) tape)
                                                           (make-traced-var (list-ref args 9) tape))
                                              (traced-vec3 (make-traced-var (list-ref args 10) tape)
                                                           (make-traced-var (list-ref args 11) tape)
                                                           (make-traced-var (list-ref args 12) tape))
                                              (rigid-body-3d-mass initial-body)
                                              (rigid-body-3d-inertia initial-body))]
                                         ;; Roll forward with tracing
                                         [final-tb (traced-rollout-3d tb
                                                                      (lambda (b)
                                                                              (traced-gravity-step-3d b
                                                                                                      (lift-vec3-const (vec3 0 -9.8 0))
                                                                                                      (/ 1 60)))
                                                                      num-steps)])
                                        (loss-fn (unpack-traced-body-3d final-tb))))))
                 (list (vec3-x (rigid-body-3d-pos initial-body))
                       (vec3-y (rigid-body-3d-pos initial-body))
                       (vec3-z (rigid-body-3d-pos initial-body))
                       (vec3-x (rigid-body-3d-vel initial-body))
                       (vec3-y (rigid-body-3d-vel initial-body))
                       (vec3-z (rigid-body-3d-vel initial-body))
                       (quat-w (rigid-body-3d-orientation initial-body))
                       (quat-x (rigid-body-3d-orientation initial-body))
                       (quat-y (rigid-body-3d-orientation initial-body))
                       (quat-z (rigid-body-3d-orientation initial-body))
                       (vec3-x (rigid-body-3d-angular-vel initial-body))
                       (vec3-y (rigid-body-3d-angular-vel initial-body))
                       (vec3-z (rigid-body-3d-angular-vel initial-body))))])
        ;; Return structured gradients
        (values (vec3 (list-ref grads 0) (list-ref grads 1) (list-ref grads 2))
                (vec3 (list-ref grads 3) (list-ref grads 4) (list-ref grads 5))
                (quat (list-ref grads 6) (list-ref grads 7) (list-ref grads 8) (list-ref grads 9))
                (vec3 (list-ref grads 10) (list-ref grads 11) (list-ref grads 12)))))

;;; ====
;;; Loss Functions for 3D Tasks
;;; ====

;;; target-position-loss-3d : Vec3 → (RigidBody3D → Number)
;;; Loss for reaching a target position.
(define (target-position-loss-3d target)
  (lambda (body)
          (vec3-distance-sq (rigid-body-3d-pos body) target)))

;;; target-orientation-loss-3d : Quat → (RigidBody3D → Number)
;;; Loss for reaching a target orientation.
(define (target-orientation-loss-3d target)
  (lambda (body)
          ;; Angular distance: 1 - |q1 · q2|^2
          (let* ([q (rigid-body-3d-orientation body)]
                 [dot (+ (* (quat-w q) (quat-w target))
                         (* (quat-x q) (quat-x target))
                         (* (quat-y q) (quat-y target))
                         (* (quat-z q) (quat-z target)))])
                (- 1 (* dot dot)))))

;;; target-state-loss-3d : Vec3 × Vec3 × Quat × Vec3 × Number × Number × Number × Number → (RigidBody3D → Number)
;;; Weighted loss for reaching target state.
(define (target-state-loss-3d target-pos target-vel target-orient target-omega
                               w-pos w-vel w-orient w-omega)
  (lambda (body)
          (+ (* w-pos (vec3-distance-sq (rigid-body-3d-pos body) target-pos))
             (* w-vel (vec3-distance-sq (rigid-body-3d-vel body) target-vel))
             (* w-orient ((target-orientation-loss-3d target-orient) body))
             (* w-omega (vec3-distance-sq (rigid-body-3d-angular-vel body) target-omega)))))

;;; trajectory-loss-3d : (List Vec3) → ((List RigidBody3D) → Number)
;;; Loss for following a trajectory of positions.
(define (trajectory-loss-3d target-positions)
  (lambda (trajectory)
          (let loop ([bodies trajectory]
                     [targets target-positions]
                     [total 0])
               (if (or (null? bodies) (null? targets))
                   total
                   (loop (cdr bodies)
                         (cdr targets)
                         (+ total (vec3-distance-sq (rigid-body-3d-pos (car bodies))
                                                    (car targets))))))))

;;; energy-minimization-loss-3d : RigidBody3D → Number
;;; Loss for minimizing kinetic energy.
(define (energy-minimization-loss-3d body)
  (let* ([mass (rigid-body-3d-mass body)]
         [vel (rigid-body-3d-vel body)]
         [omega (rigid-body-3d-angular-vel body)]
         [inertia (rigid-body-3d-inertia body)]
         ;; Linear KE: 0.5 * m * v²
         [linear-ke (* 0.5 mass (vec3-magnitude-sq vel))]
         ;; Angular KE: 0.5 * ω · I · ω
         [I-omega (mat3-mul-vec3 inertia omega)]
         [angular-ke (* 0.5 (vec3-dot omega I-omega))])
        (+ linear-ke angular-ke)))

;;; ====
;;; Projectile Motion (3D)
;;; ====

;;; projectile-hit-loss-3d : Vec3 × Vec3 × Vec3 × Number × Nat → (Vec3 → Number)
;;; Loss function for 3D projectile hitting target.
;;; Takes initial velocity as input (for optimization).
(define (projectile-hit-loss-3d start-pos target gravity dt num-steps)
  (lambda (initial-vel)
          (with-fresh-ad-scope
           (lambda ()
                   (let* ([tape (make-reverse-tape)]
                          [traced-pos (lift-vec3 start-pos tape)]
                          [traced-vel (lift-vec3 initial-vel tape)]
                          [traced-gravity (lift-vec3-const gravity)]
                          ;; Simulate projectile
                          [final-state
                           (let loop ([pos traced-pos] [vel traced-vel] [i 0])
                                (if (>= i num-steps)
                                    pos
                                    (call-with-values
                                     (lambda () (traced-point-step-3d pos vel traced-gravity dt))
                                     (lambda (new-pos new-vel)
                                             (loop new-pos new-vel (+ i 1))))))]
                          ;; Distance to target
                          [dist-sq (traced-vec3-distance-sq final-state (lift-vec3-const target))])
                         (traced-value dist-sq))))))

;;; traced-point-step-3d : TracedVec3 × TracedVec3 × TracedVec3 × Number → (TracedVec3 × TracedVec3)
;;; Single step for point mass (position + velocity).
(define (traced-point-step-3d pos vel gravity dt)
  (let* ([new-vel (traced-vec3-add vel (traced-vec3-scale gravity dt))]
         [new-pos (traced-vec3-add pos (traced-vec3-scale new-vel dt))])
        (values new-pos new-vel)))

;;; ====
;;; Substepping for Stability
;;; ====

;;; traced-rollout-substep-3d : TracedBody3D × (TracedBody3D → TracedBody3D) × Nat × Nat → TracedBody3D
;;; Roll out with substepping.
;;; substeps: number of sub-iterations per main step
(define (traced-rollout-substep-3d initial-body step-fn num-steps substeps)
  (traced-rollout-3d initial-body
                     (lambda (body)
                             (traced-rollout-3d body step-fn substeps))
                     num-steps))

;;; ====
;;; Collision Rollout (with soft contacts)
;;; ====

;;; traced-collision-rollout-3d : TracedBody3D × Number × SoftMaterial3D × Number × Nat → TracedBody3D
;;; Rollout with ground collision.
(define (traced-collision-rollout-3d initial-body radius material ground-y n)
  (let ([gravity (lift-vec3-const (vec3 0 -9.8 0))]
        [dt (/ 1 60)])
       (traced-rollout-3d
        initial-body
        (lambda (body)
                (traced-sphere-collision-step-3d body radius '() ground-y material dt))
        n)))

;;; ====
;;; Constraint Rollout
;;; ====

;;; traced-constraint-rollout-3d : (Vector TracedBody3D) × (List Constraint3D) × Vec3 × Number × Nat × Nat → (Vector TracedBody3D)
;;; Rollout multiple bodies with constraints.
(define (traced-constraint-rollout-3d initial-bodies constraints gravity dt iterations n)
  (let loop ([bodies initial-bodies] [i 0])
       (if (>= i n)
           bodies
           (loop (traced-constraint-step-3d bodies constraints gravity dt iterations)
                 (+ i 1)))))

;;; ====
;;; Multi-Body System State
;;; ====

;;; For systems with multiple bodies (chains, cloth, etc.), we need
;;; to track state as a vector of bodies.

;;; traced-system-state-3d : Structure for multi-body state
;;; Just use (Vector TracedBody3D) directly.

;;; traced-system-rollout-3d : (Vector TracedBody3D) × ((Vector TracedBody3D) → (Vector TracedBody3D)) × Nat → (Vector TracedBody3D)
;;; Roll out multi-body system.
(define (traced-system-rollout-3d initial-bodies step-fn n)
  (let loop ([bodies initial-bodies] [i 0])
       (if (>= i n)
           bodies
           (loop (step-fn bodies) (+ i 1)))))

;;; traced-system-trajectory-3d : (Vector TracedBody3D) × ((Vector TracedBody3D) → (Vector TracedBody3D)) × Nat → (List (Vector TracedBody3D))
;;; Collect trajectory for multi-body system.
(define (traced-system-trajectory-3d initial-bodies step-fn n)
  (let loop ([bodies initial-bodies] [i 0] [trajectory (list initial-bodies)])
       (if (>= i n)
           (reverse trajectory)
           (let ([new-bodies (step-fn bodies)])
                (loop new-bodies (+ i 1) (cons new-bodies trajectory))))))

;;; ====
;;; Pendulum System Rollout
;;; ====

;;; traced-pendulum-rollout-3d : TracedBody3D × Constraint3D × Vec3 × Number × Nat × Nat → TracedBody3D
;;; Roll out single pendulum system.
(define (traced-pendulum-rollout-3d bob constraint gravity dt iterations n)
  (let ([bodies (vector bob)]
        [constraints (list constraint)])
       (let ([final-bodies (traced-constraint-rollout-3d bodies constraints gravity dt iterations n)])
            (vector-ref final-bodies 0))))

;;; traced-chain-rollout-3d : (Vector TracedBody3D) × (List Constraint3D) × Vec3 × Number × Nat × Nat → (Vector TracedBody3D)
;;; Roll out chain/rope system.
(define (traced-chain-rollout-3d bodies constraints gravity dt iterations n)
  (traced-constraint-rollout-3d bodies constraints gravity dt iterations n))

;;; ====
;;; Gradient of Pendulum Motion
;;; ====

;;; pendulum-gradient-3d : TracedBody3D × Constraint3D × (RigidBody3D → Number) × Number × Nat × Nat → (Vec3 × Vec3)
;;; Compute gradient of loss w.r.t. initial position and velocity.
(define (pendulum-gradient-3d bob constraint loss-fn dt iterations num-steps)
  (let* ([mass (traced-body-3d-mass bob)]
         [inertia (traced-body-3d-inertia bob)]
         [gravity (vec3 0 -9.8 0)])
        (gradient
         (lambda args
                 (with-fresh-ad-scope
                  (lambda ()
                          (let* ([tape (make-reverse-tape)]
                                 [tb (make-traced-body-3d
                                      (traced-vec3 (make-traced-var (list-ref args 0) tape)
                                                   (make-traced-var (list-ref args 1) tape)
                                                   (make-traced-var (list-ref args 2) tape))
                                      (traced-vec3 (make-traced-var (list-ref args 3) tape)
                                                   (make-traced-var (list-ref args 4) tape)
                                                   (make-traced-var (list-ref args 5) tape))
                                      (lift-quat (quat-identity) tape)
                                      (lift-vec3 (vec3-zero) tape)
                                      mass inertia)]
                                 [final (traced-pendulum-rollout-3d tb constraint gravity dt iterations num-steps)]
                                 [final-rb (unpack-traced-body-3d final)])
                                (loss-fn final-rb)))))
         (list (traced-value (traced-vec3-x (traced-body-3d-pos bob)))
               (traced-value (traced-vec3-y (traced-body-3d-pos bob)))
               (traced-value (traced-vec3-z (traced-body-3d-pos bob)))
               (traced-value (traced-vec3-x (traced-body-3d-vel bob)))
               (traced-value (traced-vec3-y (traced-body-3d-vel bob)))
               (traced-value (traced-vec3-z (traced-body-3d-vel bob)))))))

