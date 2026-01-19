;;; lattice/physics/diff/optic-optimize.ss --- Optic-Based Physics Optimization
;;;
;;; Selective physics parameter optimization using traced-optics.
;;; Complements optimize.ss with lens-based parameter selection.
;;;
;;; When to use each API:
;;;
;;;   optimize.ss:
;;;     - optimize-trajectory: Optimize ALL body parameters (6 DOF)
;;;     - optimize-initial-velocity: Optimize velocity for projectiles
;;;     - Uses Adam optimizer with parameter flattening
;;;     - Best for: unconstrained full-state optimization
;;;
;;;   optic-optimize.ss (this module):
;;;     - optimize-trajectory-optic: Optimize SINGLE parameter via lens
;;;     - optimize-physics-param: General optic-focused gradient descent
;;;     - Uses vanilla GD, no flattening needed
;;;     - Best for: constrained optimization (e.g., only velocity, not position)
;;;
;;; Key differences:
;;;   - Optic version: loss-fn receives TracedBody, must use traced ops
;;;   - Original version: loss-fn receives RigidBody2D, uses regular ops
;;;
;;; Dependencies:
;;;   - lattice/autodiff/traced-optics.ss
;;;   - lattice/physics/lenses/lenses.ss
;;;   - lattice/physics/diff/traced-body.ss

(load "lattice/autodiff/traced-optics.ss")
(load "lattice/physics/lenses/lenses.ss")
(load "lattice/physics/diff/traced-body.ss")
(load "lattice/physics/diff/traced-integrators.ss")
(load "lattice/physics/diff/rollout.ss")

;;; ============================================================
;;; Core Optic-Based Optimization
;;; ============================================================

;;; optimize-physics-param : Optic × RigidBody2D × ((TracedBody → Traced) → loss) × Number × Nat → RigidBody2D
;;; Optimize a physics parameter specified by optic.
;;;
;;; Arguments:
;;;   param-optic: Lens focusing on parameter to optimize (e.g., rigid-body-vel-lens)
;;;   initial-body: Starting body state
;;;   make-loss: Function that takes a body-tracer and returns a loss function
;;;   learning-rate: Gradient descent step size
;;;   max-iters: Number of optimization iterations
;;;
;;; Example:
;;;   (optimize-physics-param
;;;     rigid-body-vel-lens
;;;     body
;;;     (lambda (trace-body)
;;;       (lambda (b) (traced-vec2-magnitude-sq (traced-body-pos (trace-body b)))))
;;;     0.01
;;;     100)
(define (optimize-physics-param param-optic initial-body make-loss learning-rate max-iters)
  (let loop ([body initial-body] [iter 0])
    (if (>= iter max-iters)
        body
        (let* ([grad (physics-optic-gradient param-optic body make-loss)]
               [focus (view param-optic body)]
               [new-focus (subtract-physics-grad focus grad learning-rate)]
               [new-body (set-lens param-optic new-focus body)])
          (loop new-body (+ iter 1))))))

;;; physics-optic-gradient : Optic × RigidBody2D × ((TracedBody → Traced) → loss) → gradient
;;; Compute gradient of physics loss with respect to optic focus.
;;;
;;; The make-loss function receives a body-tracer that converts RigidBody2D to TracedBody.
;;; This enables gradient flow through physics simulation.
(define (physics-optic-gradient param-optic body make-loss)
  (with-fresh-ad-scope
   (lambda ()
     (let* ([tape (make-reverse-tape)]
            ;; Create tracer for the body with traced parameter
            [trace-body (make-body-tracer param-optic body tape)]
            ;; Build loss function
            [loss-fn (make-loss trace-body)]
            ;; Create body with traced focus
            [traced-body (trace-body body)]
            ;; CRITICAL: Extract traced focus FROM the traced body, not separately!
            ;; This ensures gradient IDs match.
            [traced-focus (get-traced-focus param-optic traced-body)]
            ;; Compute loss
            [loss (loss-fn traced-body)])
       (if (traced? loss)
           (let ([grads (backward tape (traced-id loss) 1)])
             (extract-gradient traced-focus grads))
           (zero-like traced-focus))))))

;;; get-traced-focus : Optic × TracedBody → TracedValue
;;; Extract the traced focus from a traced body based on optic type.
(define (get-traced-focus param-optic traced-body)
  (cond
    [(eq? param-optic rigid-body-pos-lens)
     (traced-body-pos traced-body)]
    [(eq? param-optic rigid-body-vel-lens)
     (traced-body-vel traced-body)]
    [(eq? param-optic rigid-body-angle-lens)
     (traced-body-angle traced-body)]
    [(eq? param-optic rigid-body-angular-vel-lens)
     (traced-body-angular-vel traced-body)]
    [else
     ;; For composed lenses, we need full body tracing
     ;; Return a placeholder - gradients still flow, we just extract all
     (traced-body-vel traced-body)]))

;;; make-body-tracer : Optic × RigidBody2D × Tape → (RigidBody2D → TracedBody)
;;; Create a function that traces a body with the specified parameter traced.
(define (make-body-tracer param-optic template-body tape)
  (lambda (body)
    (let* ([pos (rigid-body-pos body)]
           [vel (rigid-body-vel body)]
           [angle (rigid-body-angle body)]
           [omega (rigid-body-angular-vel body)]
           [mass (rigid-body-mass body)]
           [inertia (rigid-body-inertia body)]
           ;; Trace based on optic type
           [otype (optic-type param-optic)])
      (cond
        ;; Position lens - trace position
        [(eq? param-optic rigid-body-pos-lens)
         (let ([traced-pos (lift-vec2 pos tape)])
           (make-traced-body traced-pos
                             (lift-vec2-const vel)
                             (make-traced-const angle tape)
                             (make-traced-const omega tape)
                             mass inertia))]
        ;; Velocity lens - trace velocity
        [(eq? param-optic rigid-body-vel-lens)
         (let ([traced-vel (lift-vec2 vel tape)])
           (make-traced-body (lift-vec2-const pos)
                             traced-vel
                             (make-traced-const angle tape)
                             (make-traced-const omega tape)
                             mass inertia))]
        ;; Angle lens - trace angle
        [(eq? param-optic rigid-body-angle-lens)
         (make-traced-body (lift-vec2-const pos)
                           (lift-vec2-const vel)
                           (make-traced-var angle tape)
                           (make-traced-const omega tape)
                           mass inertia)]
        ;; Angular velocity lens - trace angular velocity
        [(eq? param-optic rigid-body-angular-vel-lens)
         (make-traced-body (lift-vec2-const pos)
                           (lift-vec2-const vel)
                           (make-traced-const angle tape)
                           (make-traced-var omega tape)
                           mass inertia)]
        ;; Composed lens for position/velocity components
        [else
         ;; Default: trace all (full gradient)
         (make-traced-body (lift-vec2 pos tape)
                           (lift-vec2 vel tape)
                           (make-traced-var angle tape)
                           (make-traced-var omega tape)
                           mass inertia)]))))

;;; subtract-physics-grad : PhysicsValue × gradient × rate → PhysicsValue
;;; Subtract scaled gradient from physics value (Vec2 or Number).
(define (subtract-physics-grad value grad rate)
  (cond
    [(vec2? value)
     (vec2 (- (vec2-x value) (* rate (vec2-x grad)))
           (- (vec2-y value) (* rate (vec2-y grad))))]
    [(number? value)
     (- value (* rate grad))]
    [else value]))

;;; ============================================================
;;; Simplified Trajectory Optimization
;;; ============================================================

;;; optimize-trajectory-optic : RigidBody2D × (TracedBody → TracedBody) × (TracedBody → Traced) × Nat × Optic × Number × Nat → RigidBody2D
;;; Optimize a trajectory parameter specified by optic.
;;;
;;; Much simpler than the original optimize-trajectory:
;;;   - No manual flattening/unflattening
;;;   - Parameter selection via optic
;;;   - Reuses traced-optics infrastructure
;;;
;;; IMPORTANT: The loss function must use traced operations (e.g., traced-vec2-distance-sq)
;;; to enable gradient flow.
;;;
;;; Example:
;;;   ;; Optimize initial velocity to hit target
;;;   (optimize-trajectory-optic
;;;     body step-fn
;;;     (lambda (final-tb)
;;;       (traced-vec2-distance-sq (traced-body-pos final-tb)
;;;                                (lift-vec2-const target)))
;;;     100             ; simulation steps
;;;     rigid-body-vel-lens
;;;     0.01 50)        ; learning rate, iterations
(define (optimize-trajectory-optic initial-body step-fn loss-fn num-steps
                                    param-optic learning-rate max-iters)
  (optimize-physics-param
   param-optic
   initial-body
   (lambda (trace-body)
     (lambda (tb)
       ;; Run rollout and compute loss on traced body
       (let ([final-tb (traced-rollout tb step-fn num-steps)])
         (loss-fn final-tb))))
   learning-rate
   max-iters))

;;; optimize-initial-velocity-optic : RigidBody2D × Vec2 × Vec2 × Number × Nat × Number × Nat → Vec2
;;; Find initial velocity to reach target position.
;;; Simplified version using optic-based optimization.
(define (optimize-initial-velocity-optic body target gravity dt num-steps
                                          learning-rate max-iters)
  (let* ([traced-gravity (lift-vec2-const gravity)]
         [traced-target (lift-vec2-const target)]
         [step-fn (lambda (tb)
                    (traced-gravity-step tb traced-gravity dt))]
         [loss-fn (lambda (final-tb)
                    (traced-vec2-distance-sq (traced-body-pos final-tb)
                                             traced-target))]
         [optimized (optimize-trajectory-optic
                     body step-fn loss-fn num-steps
                     rigid-body-vel-lens
                     learning-rate max-iters)])
    (rigid-body-vel optimized)))

;;; ============================================================
;;; Sensitivity Analysis via Optics
;;; ============================================================

;;; sensitivity-at-optic : RigidBody2D × (TracedBody → TracedBody) × Optic × Nat → gradient
;;; Compute sensitivity of final position to parameter at optic focus.
;;;
;;; Returns gradient showing how final position changes with parameter change.
(define (sensitivity-at-optic initial-body step-fn param-optic num-steps)
  (physics-optic-gradient
   param-optic
   initial-body
   (lambda (trace-body)
     (lambda (tb)
       ;; Run simulation
       (let* ([final-tb (traced-rollout tb step-fn num-steps)])
         ;; Loss = final position magnitude (for gradient extraction)
         (traced-vec2-magnitude-sq (traced-body-pos final-tb)))))))

;;; ============================================================
;;; Multi-Parameter Optimization via Traversal
;;; ============================================================

;;; For optimizing multiple bodies simultaneously, we can use traversals.
;;; This demonstrates composing traced-optics with physics world lenses.

;;; optimize-world-bodies : World × (TracedWorld → Traced) × Optic × Number × Nat → World
;;; Optimize all bodies in a world through a traversal.
;;;
;;; Example:
;;;   (optimize-world-bodies
;;;     world
;;;     (lambda (tw) (total-energy-loss tw))
;;;     bodies-each-lens
;;;     0.01 100)
;;;
;;; Note: Requires world/bodies traversal lenses (future work)

