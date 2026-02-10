(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module physics/optimize
;;; @requires prelude vec2 reverse-diff traced-vec2 traced-body traced-integrators rollout
(require 'prelude)
(require 'vec2)
(require 'reverse-diff)
(require 'traced-vec2)
(require 'traced-body)
(require 'traced-integrators)
(require 'rollout)

(doc 'module 'optimize)
(doc 'description "Trajectory Optimization API

Provides general-purpose optimization algorithms and physics-specific utilities:
- Gradient descent and variants (Adam, momentum)
- Inverse physics (parameter estimation: mass, gravity)
- Sensitivity analysis (gradients, Hessian diagonal)

For trajectory optimization, prefer optic-optimize.ss which provides:
  optimize-trajectory-optic        Optimize initial body state
  optimize-trajectory-all          Optimize all state variables
  optimize-initial-velocity-optic  Find velocity to hit target")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'gradient-descent)

;;; gradient-descent-step : (List Number) × (List Number) × Number → (List Number)
;;; Single gradient descent update: x = x - lr * grad
(define (gradient-descent-step params grads learning-rate)
  (map (lambda (p g) (- p (* learning-rate g)))
       params grads))

;;; gradient-descent : ((List Number) → Number) × (List Number) × Number × Nat → (List Number)
;;; Vanilla gradient descent optimization.
;;;   loss-fn: function to minimize
;;;   initial-params: starting point
;;;   learning-rate: step size
;;;   max-iters: maximum iterations
;;; Returns optimized parameters.
(define (gradient-descent loss-fn initial-params learning-rate max-iters)
  (let loop ([params initial-params] [iter 0])
       (if (>= iter max-iters)
           params
           (let* ([grads (gradient (lambda args (loss-fn args))
                                   params)]
                  [new-params (gradient-descent-step params grads learning-rate)])
                 (loop new-params (+ iter 1))))))

;;; ====
;;; Gradient Descent with Momentum
;;; ====

;;; momentum-step : (List Number) × (List Number) × (List Number) × Number × Number → ((List Number) × (List Number))
;;; Momentum update: v = beta*v + grad, x = x - lr*v
(define (momentum-step params grads velocity learning-rate beta)
  (let* ([new-velocity (map (lambda (v g) (+ (* beta v) g))
                            velocity grads)]
         [new-params (map (lambda (p v) (- p (* learning-rate v)))
                          params new-velocity)])
        (values new-params new-velocity)))

;;; gradient-descent-momentum : ((List Number) → Number) × (List Number) × Number × Number × Nat → (List Number)
;;; Gradient descent with momentum.
(define (gradient-descent-momentum loss-fn initial-params learning-rate beta max-iters)
  (let ([initial-velocity (map (lambda (_) 0) initial-params)])
       (let loop ([params initial-params] [velocity initial-velocity] [iter 0])
            (if (>= iter max-iters)
                params
                (let* ([grads (gradient (lambda args (loss-fn args))
                                        params)])
                      (call-with-values
                       (lambda () (momentum-step params grads velocity learning-rate beta))
                       (lambda (new-params new-velocity)
                               (loop new-params new-velocity (+ iter 1)))))))))

;;; ====
;;; Adam Optimizer
;;; ====

;;; adam-step : (List Number) × (List Number) × (List Number) × (List Number) × Nat × Number × Number × Number × Number → ((List Number) × (List Number) × (List Number))
;;; Adam optimizer update.
(define (adam-step params grads m-prev v-prev t lr beta1 beta2 epsilon)
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

;;; adam : ((List Number) → Number) × (List Number) × Number × Nat → (List Number)
;;; Adam optimization with default hyperparameters.
(define (adam loss-fn initial-params learning-rate max-iters)
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
                       (lambda () (adam-step params grads m v t learning-rate beta1 beta2 epsilon))
                       (lambda (new-params new-m new-v)
                               (loop new-params new-m new-v (+ t 1)))))))))

;;; ====
;;; Inverse Physics (Parameter Estimation)
;;; ====

;;; estimate-mass : (List (RigidBody2D × RigidBody2D)) × Vec2 × Number × Number × Nat → Number
;;; Estimate mass from observed (initial, final) state pairs.
;;;   observations: list of (start-state, end-state) pairs
;;;   gravity: gravity vector
;;;   dt: timestep per observation
;;;   initial-guess: initial mass estimate
(define (estimate-mass observations gravity dt initial-guess max-iters)
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
                                            [inertia (circle-inertia mass 1)]  ; assume radius 1
                                            [body (make-rigid-body (rigid-body-pos start)
                                                                   (rigid-body-vel start)
                                                                   (rigid-body-angle start)
                                                                   (rigid-body-angular-vel start)
                                                                   mass inertia)]
                                            ;; Simple gravity step
                                            [new-vel (vec2-add (rigid-body-vel body)
                                                               (vec2-scale gravity dt))]
                                            [new-pos (vec2-add (rigid-body-pos body)
                                                               (vec2-scale new-vel dt))]
                                            ;; Error
                                            [pos-error (vec2-distance-sq new-pos (rigid-body-pos observed-end))]
                                            [vel-error (vec2-distance-sq new-vel (rigid-body-vel observed-end))])
                                           (+ pos-error vel-error)))
                             observations))))]
         [result (adam loss-fn (list initial-guess) 0.01 max-iters)])
        (car result)))

;;; estimate-gravity : (List (Vec2 × Vec2 × Vec2 × Vec2)) × Number × Number × Nat → Vec2
;;; Estimate gravity from observed trajectories.
;;;   observations: list of (pos0, vel0, pos1, vel1) tuples
;;;   dt: timestep between observations
(define (estimate-gravity observations dt initial-guess max-iters)
  (let* ([loss-fn
          (lambda (g-list)
                  (let ([gx (car g-list)]
                        [gy (cadr g-list)])
                       (fold-left
                        +
                        0
                        (map (lambda (obs)
                                     (let* ([pos0 (list-ref obs 0)]
                                            [vel0 (list-ref obs 1)]
                                            [pos1 (list-ref obs 2)]
                                            [vel1 (list-ref obs 3)]
                                            ;; Predicted state
                                            [pred-vel (vec2 (+ (vec2-x vel0) (* gx dt))
                                                            (+ (vec2-y vel0) (* gy dt)))]
                                            [pred-pos (vec2 (+ (vec2-x pos0) (* (vec2-x pred-vel) dt))
                                                            (+ (vec2-y pos0) (* (vec2-y pred-vel) dt)))]
                                            ;; Error
                                            [error (+ (vec2-distance-sq pred-pos pos1)
                                                      (vec2-distance-sq pred-vel vel1))])
                                           error))
                             observations))))]
         [result (adam loss-fn
                       (list (vec2-x initial-guess) (vec2-y initial-guess))
                       0.1 max-iters)])
        (vec2 (car result) (cadr result))))

;;; ====
;;; Sensitivity Analysis
;;; ====

;;; sensitivity : ((List Number) → Number) × (List Number) → (List Number)
;;; Compute gradient of loss w.r.t. parameters (same as gradient, but semantic).
(define (sensitivity loss-fn params)
  (gradient (lambda args (apply loss-fn (list args))) params))

;;; hessian-diagonal : ((List Number) → Number) × (List Number) × Number → (List Number)
;;; Approximate diagonal of Hessian using finite differences of gradient.
;;; Shows curvature along each parameter dimension.
(define (hessian-diagonal loss-fn params epsilon)
  (let ([grads (sensitivity loss-fn params)])
       (map (lambda (i)
                    (let* ([params+ (list-update params i (lambda (x) (+ x epsilon)))]
                           [params- (list-update params i (lambda (x) (- x epsilon)))]
                           [grad+ (sensitivity loss-fn params+)]
                           [grad- (sensitivity loss-fn params-)]
                           [g+ (list-ref grad+ i)]
                           [g- (list-ref grad- i)])
                          (/ (- g+ g-) (* 2 epsilon))))
            (iota (length params)))))

;;; position-sensitivity : RigidBody2D × (RigidBody2D → RigidBody2D) × Nat → (Vec2 × Vec2)
;;; How much does initial position affect final position?
;;; Returns (d(final-x)/d(init-x,y), d(final-y)/d(init-x,y)).
(define (position-sensitivity body step-fn num-steps)
  (let* ([grads-x (gradient
                   (lambda (px py)
                           (with-fresh-ad-scope
                            (lambda ()
                                    (let* ([tape (make-reverse-tape)]
                                           [tb (make-traced-body
                                                (traced-vec2 (make-traced-var px tape)
                                                             (make-traced-var py tape))
                                                (lift-vec2 (rigid-body-vel body) tape)
                                                (make-traced-var (rigid-body-angle body) tape)
                                                (make-traced-var (rigid-body-angular-vel body) tape)
                                                (rigid-body-mass body)
                                                (rigid-body-inertia body))]
                                           [final (traced-rollout tb step-fn num-steps)])
                                          (traced-value (traced-vec2-x (traced-body-pos final)))))))
                   (list (vec2-x (rigid-body-pos body))
                         (vec2-y (rigid-body-pos body))))]
         [grads-y (gradient
                   (lambda (px py)
                           (with-fresh-ad-scope
                            (lambda ()
                                    (let* ([tape (make-reverse-tape)]
                                           [tb (make-traced-body
                                                (traced-vec2 (make-traced-var px tape)
                                                             (make-traced-var py tape))
                                                (lift-vec2 (rigid-body-vel body) tape)
                                                (make-traced-var (rigid-body-angle body) tape)
                                                (make-traced-var (rigid-body-angular-vel body) tape)
                                                (rigid-body-mass body)
                                                (rigid-body-inertia body))]
                                           [final (traced-rollout tb step-fn num-steps)])
                                          (traced-value (traced-vec2-y (traced-body-pos final)))))))
                   (list (vec2-x (rigid-body-pos body))
                         (vec2-y (rigid-body-pos body))))])
        (values (vec2 (car grads-x) (cadr grads-x))
                (vec2 (car grads-y) (cadr grads-y)))))

;;; ====
;;; Utility Functions
;;; ====

;;; list-update : (List α) × Nat × (α → α) → (List α)
;;; Update element at index with function.
(define (list-update lst idx f)
  (if (= idx 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- idx 1) f))))

;;; print-optimization-progress : Nat × Number × (List Number) → Unit
;;; Print optimization iteration info (for debugging).
(define (print-optimization-progress iter loss params)
  (printf "Iter ~a: loss=~a params=~a~n" iter loss params))
