(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module optimize3d
;;; @requires prelude vec3 quaternion reverse-diff traced-vec3 traced-quaternion traced-body3d traced-integrators3d rollout3d
(require 'prelude)
(require 'vec3)
(require 'quaternion)
(require 'reverse-diff)
(require 'traced-vec3)
(require 'traced-quaternion)
(require 'traced-body3d)
(require 'traced-integrators3d)
(require 'rollout3d)

(doc 'module 'optimize3d)
(doc 'description "3D Trajectory Optimization API - High-level API for differentiable 3D physics optimization: gradient descent and variants (Adam, momentum), trajectory optimization, inverse physics (parameter estimation), sensitivity analysis.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'gradient-descent)

;;; gradient-descent-step-3d : (List Number) × (List Number) × Number → (List Number)
;;; Single gradient descent update: x = x - lr * grad
(define (gradient-descent-step-3d params grads learning-rate)
  (map (lambda (p g) (- p (* learning-rate g)))
       params grads))

;;; gradient-descent-3d : ((List Number) → Number) × (List Number) × Number × Nat → (List Number)
;;; Vanilla gradient descent optimization.
(define (gradient-descent-3d loss-fn initial-params learning-rate max-iters)
  (let loop ([params initial-params] [iter 0])
       (if (>= iter max-iters)
           params
           (let* ([grads (gradient (lambda args (loss-fn args))
                                   params)]
                  [new-params (gradient-descent-step-3d params grads learning-rate)])
                 (loop new-params (+ iter 1))))))

;;; ====
;;; Gradient Descent with Momentum
;;; ====

;;; momentum-step-3d : (List Number) × (List Number) × (List Number) × Number × Number → ((List Number) × (List Number))
(define (momentum-step-3d params grads velocity learning-rate beta)
  (let* ([new-velocity (map (lambda (v g) (+ (* beta v) g))
                            velocity grads)]
         [new-params (map (lambda (p v) (- p (* learning-rate v)))
                          params new-velocity)])
        (values new-params new-velocity)))

;;; gradient-descent-momentum-3d : ((List Number) → Number) × (List Number) × Number × Number × Nat → (List Number)
(define (gradient-descent-momentum-3d loss-fn initial-params learning-rate beta max-iters)
  (let ([initial-velocity (map (lambda (_) 0) initial-params)])
       (let loop ([params initial-params] [velocity initial-velocity] [iter 0])
            (if (>= iter max-iters)
                params
                (let* ([grads (gradient (lambda args (loss-fn args))
                                        params)])
                      (call-with-values
                       (lambda () (momentum-step-3d params grads velocity learning-rate beta))
                       (lambda (new-params new-velocity)
                               (loop new-params new-velocity (+ iter 1)))))))))

;;; ====
;;; Adam Optimizer
;;; ====

;;; adam-step-3d : (List Number) × (List Number) × (List Number) × (List Number) × Nat × Number × Number × Number × Number → Values
(define (adam-step-3d params grads m-prev v-prev t lr beta1 beta2 epsilon)
  (let* (;; Update biased first moment estimate
         [m (map (lambda (m-i g) (+ (* beta1 m-i) (* (- 1 beta1) g)))
                 m-prev grads)]
         ;; Update biased second moment estimate
         [v (map (lambda (v-i g) (+ (* beta2 v-i) (* (- 1 beta2) (* g g))))
                 v-prev grads)]
         ;; Bias correction
         [m-hat (map (lambda (m-i) (/ m-i (- 1 (expt beta1 t)))) m)]
         [v-hat (map (lambda (v-i) (/ v-i (- 1 (expt beta2 t)))) v)]
         ;; Update parameters
         [new-params (map (lambda (p mh vh)
                                  (- p (* lr (/ mh (+ (sqrt vh) epsilon)))))
                          params m-hat v-hat)])
        (values new-params m v)))

;;; adam-3d : ((List Number) → Number) × (List Number) × Number × Nat → (List Number)
;;; Adam optimization with default hyperparameters.
(define (adam-3d loss-fn initial-params learning-rate max-iters)
  (let ([beta1 0.9]
        [beta2 0.999]
        [epsilon 1e-8]
        [m-init (map (lambda (_) 0) initial-params)]
        [v-init (map (lambda (_) 0) initial-params)])
       (let loop ([params initial-params] [m m-init] [v v-init] [t 1])
            (if (> t max-iters)
                params
                (let* ([grads (gradient (lambda args (loss-fn args))
                                        params)])
                      (call-with-values
                       (lambda () (adam-step-3d params grads m v t learning-rate beta1 beta2 epsilon))
                       (lambda (new-params new-m new-v)
                               (loop new-params new-m new-v (+ t 1)))))))))

;;; ====
;;; 3D Trajectory Optimization
;;; ====

;;; optimize-initial-velocity-3d : Vec3 × Vec3 × Vec3 × Number × Nat × Number × Nat → Vec3
;;; Find initial velocity to hit target position.
(define (optimize-initial-velocity-3d start-pos target-pos gravity dt num-steps
                                       learning-rate max-iters)
  (let* ([loss-fn (projectile-hit-loss-3d start-pos target-pos gravity dt num-steps)]
         ;; Wrap for gradient: takes list, returns scalar
         [opt-loss (lambda (vel-list)
                           (loss-fn (vec3 (car vel-list) (cadr vel-list) (caddr vel-list))))]
         ;; Initial guess: direct line to target (ignoring gravity)
         [delta (vec3-sub target-pos start-pos)]
         [time (* num-steps dt)]
         [initial-vel (vec3-scale delta (/ 1 time))]
         ;; Optimize
         [result (adam-3d opt-loss
                          (list (vec3-x initial-vel) (vec3-y initial-vel) (vec3-z initial-vel))
                          learning-rate
                          max-iters)])
        (vec3 (car result) (cadr result) (caddr result))))

;;; optimize-trajectory-3d : RigidBody3D × (TracedBody3D → TracedBody3D) × (RigidBody3D → Number) × Nat × Number × Nat → RigidBody3D
;;; Optimize initial body state to minimize final loss.
(define (optimize-trajectory-3d initial-body step-fn loss-fn num-steps
                                 learning-rate max-iters)
  (let* ([mass (rigid-body-3d-mass initial-body)]
         [inertia (rigid-body-3d-inertia initial-body)]
         ;; Flatten initial state (13D: 3 pos + 3 vel + 4 quat + 3 omega)
         [initial-params (list (vec3-x (rigid-body-3d-pos initial-body))
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
                               (vec3-z (rigid-body-3d-angular-vel initial-body)))]
         ;; Loss wrapper
         [opt-loss
          (lambda (params)
                  (with-fresh-ad-scope
                   (lambda ()
                           (let* ([tape (make-reverse-tape)]
                                  [tb (make-traced-body-3d
                                       (traced-vec3 (make-traced-var (list-ref params 0) tape)
                                                    (make-traced-var (list-ref params 1) tape)
                                                    (make-traced-var (list-ref params 2) tape))
                                       (traced-vec3 (make-traced-var (list-ref params 3) tape)
                                                    (make-traced-var (list-ref params 4) tape)
                                                    (make-traced-var (list-ref params 5) tape))
                                       (traced-quat (make-traced-var (list-ref params 6) tape)
                                                    (make-traced-var (list-ref params 7) tape)
                                                    (make-traced-var (list-ref params 8) tape)
                                                    (make-traced-var (list-ref params 9) tape))
                                       (traced-vec3 (make-traced-var (list-ref params 10) tape)
                                                    (make-traced-var (list-ref params 11) tape)
                                                    (make-traced-var (list-ref params 12) tape))
                                       mass inertia)]
                                  [final-tb (traced-rollout-3d tb step-fn num-steps)]
                                  [final-rb (unpack-traced-body-3d final-tb)])
                                 (loss-fn final-rb)))))]
         ;; Optimize
         [result (adam-3d opt-loss initial-params learning-rate max-iters)])
        ;; Reconstruct body
        (make-rigid-body-3d (vec3 (list-ref result 0) (list-ref result 1) (list-ref result 2))
                            (vec3 (list-ref result 3) (list-ref result 4) (list-ref result 5))
                            (quat-normalize (quat (list-ref result 6) (list-ref result 7)
                                                  (list-ref result 8) (list-ref result 9)))
                            (vec3 (list-ref result 10) (list-ref result 11) (list-ref result 12))
                            mass inertia)))

;;; optimize-velocity-only-3d : RigidBody3D × (TracedBody3D → TracedBody3D) × (RigidBody3D → Number) × Nat × Number × Nat → RigidBody3D
;;; Optimize only initial velocity (keep position fixed).
(define (optimize-velocity-only-3d initial-body step-fn loss-fn num-steps
                                    learning-rate max-iters)
  (let* ([mass (rigid-body-3d-mass initial-body)]
         [inertia (rigid-body-3d-inertia initial-body)]
         [pos (rigid-body-3d-pos initial-body)]
         [orient (rigid-body-3d-orientation initial-body)]
         [omega (rigid-body-3d-angular-vel initial-body)]
         ;; Optimize only velocity (6D: 3 linear + 3 angular)
         [initial-params (list (vec3-x (rigid-body-3d-vel initial-body))
                               (vec3-y (rigid-body-3d-vel initial-body))
                               (vec3-z (rigid-body-3d-vel initial-body))
                               (vec3-x omega)
                               (vec3-y omega)
                               (vec3-z omega))]
         [opt-loss
          (lambda (params)
                  (with-fresh-ad-scope
                   (lambda ()
                           (let* ([tape (make-reverse-tape)]
                                  [tb (make-traced-body-3d
                                       (lift-vec3 pos tape)
                                       (traced-vec3 (make-traced-var (list-ref params 0) tape)
                                                    (make-traced-var (list-ref params 1) tape)
                                                    (make-traced-var (list-ref params 2) tape))
                                       (lift-quat orient tape)
                                       (traced-vec3 (make-traced-var (list-ref params 3) tape)
                                                    (make-traced-var (list-ref params 4) tape)
                                                    (make-traced-var (list-ref params 5) tape))
                                       mass inertia)]
                                  [final-tb (traced-rollout-3d tb step-fn num-steps)]
                                  [final-rb (unpack-traced-body-3d final-tb)])
                                 (loss-fn final-rb)))))]
         [result (adam-3d opt-loss initial-params learning-rate max-iters)])
        (make-rigid-body-3d pos
                            (vec3 (list-ref result 0) (list-ref result 1) (list-ref result 2))
                            orient
                            (vec3 (list-ref result 3) (list-ref result 4) (list-ref result 5))
                            mass inertia)))

;;; ====
;;; Inverse Physics (Parameter Estimation)
;;; ====

;;; estimate-gravity-3d : (List (Vec3 × Vec3 × Vec3 × Vec3)) × Number × Vec3 × Nat → Vec3
;;; Estimate gravity from observed trajectories.
;;;   observations: list of (pos0, vel0, pos1, vel1) tuples
;;;   dt: timestep between observations
(define (estimate-gravity-3d observations dt initial-guess max-iters)
  (let* ([loss-fn
          (lambda (g-list)
                  (let ([gx (car g-list)]
                        [gy (cadr g-list)]
                        [gz (caddr g-list)])
                       (fold-left
                        +
                        0
                        (map (lambda (obs)
                                     (let* ([pos0 (list-ref obs 0)]
                                            [vel0 (list-ref obs 1)]
                                            [pos1 (list-ref obs 2)]
                                            [vel1 (list-ref obs 3)]
                                            ;; Predicted state
                                            [pred-vel (vec3 (+ (vec3-x vel0) (* gx dt))
                                                            (+ (vec3-y vel0) (* gy dt))
                                                            (+ (vec3-z vel0) (* gz dt)))]
                                            [pred-pos (vec3 (+ (vec3-x pos0) (* (vec3-x pred-vel) dt))
                                                            (+ (vec3-y pos0) (* (vec3-y pred-vel) dt))
                                                            (+ (vec3-z pos0) (* (vec3-z pred-vel) dt)))]
                                            ;; Error
                                            [error (+ (vec3-distance-sq pred-pos pos1)
                                                      (vec3-distance-sq pred-vel vel1))])
                                           error))
                             observations))))]
         [result (adam-3d loss-fn
                          (list (vec3-x initial-guess) (vec3-y initial-guess) (vec3-z initial-guess))
                          0.1 max-iters)])
        (vec3 (car result) (cadr result) (caddr result))))

;;; estimate-mass-3d : (List (RigidBody3D × RigidBody3D)) × Vec3 × Number × Number × Nat → Number
;;; Estimate mass from observed (initial, final) state pairs.
(define (estimate-mass-3d observations gravity dt initial-guess max-iters)
  (let* ([loss-fn
          (lambda (mass-list)
                  (let ([mass (car mass-list)])
                       (fold-left
                        +
                        0
                        (map (lambda (obs)
                                     (let* ([start (car obs)]
                                            [observed-end (cdr obs)]
                                            ;; Simulate with estimated mass
                                            [new-vel (vec3-add (rigid-body-3d-vel start)
                                                               (vec3-scale gravity dt))]
                                            [new-pos (vec3-add (rigid-body-3d-pos start)
                                                               (vec3-scale new-vel dt))]
                                            ;; Error
                                            [pos-error (vec3-distance-sq new-pos (rigid-body-3d-pos observed-end))]
                                            [vel-error (vec3-distance-sq new-vel (rigid-body-3d-vel observed-end))])
                                           (+ pos-error vel-error)))
                             observations))))]
         [result (adam-3d loss-fn (list initial-guess) 0.01 max-iters)])
        (car result)))

;;; ====
;;; Sensitivity Analysis
;;; ====

;;; sensitivity-3d : ((List Number) → Number) × (List Number) → (List Number)
;;; Compute gradient of loss w.r.t. parameters.
(define (sensitivity-3d loss-fn params)
  (gradient (lambda args (apply loss-fn (list args))) params))

;;; hessian-diagonal-3d : ((List Number) → Number) × (List Number) × Number → (List Number)
;;; Approximate diagonal of Hessian using finite differences of gradient.
(define (hessian-diagonal-3d loss-fn params epsilon)
  (let ([grads (sensitivity-3d loss-fn params)])
       (map (lambda (i)
                    (let* ([params+ (list-update-3d params i (lambda (x) (+ x epsilon)))]
                           [params- (list-update-3d params i (lambda (x) (- x epsilon)))]
                           [grad+ (sensitivity-3d loss-fn params+)]
                           [grad- (sensitivity-3d loss-fn params-)]
                           [g+ (list-ref grad+ i)]
                           [g- (list-ref grad- i)])
                          (/ (- g+ g-) (* 2 epsilon))))
            (iota-3d (length params)))))

;;; position-sensitivity-3d : RigidBody3D × (TracedBody3D → TracedBody3D) × Nat → (Vec3 × Vec3 × Vec3)
;;; How much does initial position affect final position?
(define (position-sensitivity-3d body step-fn num-steps)
  (let* ([grads-x (gradient
                   (lambda (px py pz)
                           (with-fresh-ad-scope
                            (lambda ()
                                    (let* ([tape (make-reverse-tape)]
                                           [tb (make-traced-body-3d
                                                (traced-vec3 (make-traced-var px tape)
                                                             (make-traced-var py tape)
                                                             (make-traced-var pz tape))
                                                (lift-vec3 (rigid-body-3d-vel body) tape)
                                                (lift-quat (rigid-body-3d-orientation body) tape)
                                                (lift-vec3 (rigid-body-3d-angular-vel body) tape)
                                                (rigid-body-3d-mass body)
                                                (rigid-body-3d-inertia body))]
                                           [final (traced-rollout-3d tb step-fn num-steps)])
                                          (traced-value (traced-vec3-x (traced-body-3d-pos final)))))))
                   (list (vec3-x (rigid-body-3d-pos body))
                         (vec3-y (rigid-body-3d-pos body))
                         (vec3-z (rigid-body-3d-pos body))))]
         [grads-y (gradient
                   (lambda (px py pz)
                           (with-fresh-ad-scope
                            (lambda ()
                                    (let* ([tape (make-reverse-tape)]
                                           [tb (make-traced-body-3d
                                                (traced-vec3 (make-traced-var px tape)
                                                             (make-traced-var py tape)
                                                             (make-traced-var pz tape))
                                                (lift-vec3 (rigid-body-3d-vel body) tape)
                                                (lift-quat (rigid-body-3d-orientation body) tape)
                                                (lift-vec3 (rigid-body-3d-angular-vel body) tape)
                                                (rigid-body-3d-mass body)
                                                (rigid-body-3d-inertia body))]
                                           [final (traced-rollout-3d tb step-fn num-steps)])
                                          (traced-value (traced-vec3-y (traced-body-3d-pos final)))))))
                   (list (vec3-x (rigid-body-3d-pos body))
                         (vec3-y (rigid-body-3d-pos body))
                         (vec3-z (rigid-body-3d-pos body))))]
         [grads-z (gradient
                   (lambda (px py pz)
                           (with-fresh-ad-scope
                            (lambda ()
                                    (let* ([tape (make-reverse-tape)]
                                           [tb (make-traced-body-3d
                                                (traced-vec3 (make-traced-var px tape)
                                                             (make-traced-var py tape)
                                                             (make-traced-var pz tape))
                                                (lift-vec3 (rigid-body-3d-vel body) tape)
                                                (lift-quat (rigid-body-3d-orientation body) tape)
                                                (lift-vec3 (rigid-body-3d-angular-vel body) tape)
                                                (rigid-body-3d-mass body)
                                                (rigid-body-3d-inertia body))]
                                           [final (traced-rollout-3d tb step-fn num-steps)])
                                          (traced-value (traced-vec3-z (traced-body-3d-pos final)))))))
                   (list (vec3-x (rigid-body-3d-pos body))
                         (vec3-y (rigid-body-3d-pos body))
                         (vec3-z (rigid-body-3d-pos body))))])
        (values (vec3 (car grads-x) (cadr grads-x) (caddr grads-x))
                (vec3 (car grads-y) (cadr grads-y) (caddr grads-y))
                (vec3 (car grads-z) (cadr grads-z) (caddr grads-z)))))

;;; ====
;;; Utility Functions
;;; ====

;;; list-update-3d : (List α) × Nat × (α → α) → (List α)
(define (list-update-3d lst idx f)
  (if (= idx 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (list-update-3d (cdr lst) (- idx 1) f))))

;;; iota-3d : Nat → (List Nat)
(define (iota-3d n)
  (let loop ([i 0])
       (if (= i n)
           '()
           (cons i (loop (+ i 1))))))

;;; ====
;;; Complete Optimization Examples
;;; ====

;;; projectile-optimization-example-3d : Vec3 × Vec3 × Vec3 → Vec3
;;; Find initial velocity to hit target from start position under gravity.
(define (projectile-optimization-example-3d start target gravity)
  (let* ([dt (/ 1 60)]
         [num-steps 100]
         [learning-rate 0.1]
         [max-iters 100])
        (optimize-initial-velocity-3d start target gravity dt num-steps
                                       learning-rate max-iters)))

;;; basketball-shot-example : Vec3 × Vec3 × Number → Vec3
;;; Find optimal velocity to get ball in hoop.
(define (basketball-shot-example start-pos hoop-pos hoop-radius)
  (let* ([gravity (vec3 0 -9.8 0)]
         [dt (/ 1 60)]
         [num-steps 60]  ; ~1 second flight
         [learning-rate 0.1]
         [max-iters 200])
        (optimize-initial-velocity-3d start-pos hoop-pos gravity dt num-steps
                                       learning-rate max-iters)))

;;; pendulum-optimization-example-3d : Vec3 × Vec3 × Vec3 × Number × Nat → Vec3
;;; Optimize pendulum swing to reach target.
(define (pendulum-optimization-example-3d pivot target initial-bob-pos dt num-steps)
  ;; This would optimize initial conditions to reach target
  ;; For now, just show the API pattern
  (let* ([gravity (vec3 0 -9.8 0)]
         [learning-rate 0.01]
         [max-iters 50])
        ;; Return optimized initial velocity
        (optimize-initial-velocity-3d initial-bob-pos target gravity dt num-steps
                                       learning-rate max-iters)))

