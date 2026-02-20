(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'traced-optics)
(require 'first-order)

(doc 'module 'optic-first-order)
(doc 'bridges '(optimization optics autodiff))
(doc 'description "Optic-based first-order optimization for structured parameters")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'overview)
(doc 'note "Wrappers around first-order optimizers that use optics to specify which
parameters to optimize within a structure.

Instead of flattening structures to lists, these functions use optics to focus on
specific parameters, enabling:
  - Direct optimization of nested structure fields
  - Type-safe parameter access via lens composition
  - No manual flattening/unflattening

Example:
  ;; Optimize velocity to minimize distance to target
  (adam-at rigid-body-vel-lens body
           (lambda (b) (vec2-dist-sq (body-final-pos b) target))
           0.01 100)")

(doc 'section 'sgd)

(define (sgd-at optic structure loss-fn lr max-iters)
  (doc 'type '(-> Optic s (-> s-traced Traced) Number Nat s))
  (doc 'description "SGD optimization at optic focus")
  (doc 'param 'optic "Lens/Affine focusing on parameter to optimize")
  (doc 'param 'structure "The structure containing the parameter")
  (doc 'param 'loss-fn "Loss function from traced structure to traced scalar")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'max-iters "Maximum iterations")
  (doc 'returns "Structure with optimized parameter at focus")
  (optimize-steps optic structure loss-fn lr max-iters))

(doc 'section 'momentum)

(define (momentum-at optic structure loss-fn lr beta max-iters)
  (doc 'type '(-> Optic s (-> s-traced Traced) Number Number Nat s))
  (doc 'description "Gradient descent with momentum at optic focus")
  (doc 'param 'beta "Momentum coefficient (typically 0.9)")
  (let* ([focus (view optic structure)]
         [velocity (zero-like-value focus)])
    (let loop ([s structure] [v velocity] [iter 0])
      (if (>= iter max-iters)
          s
          (let* ([grad (optic-gradient loss-fn optic s)]
                 [new-v (momentum-velocity-update v grad beta)]
                 [focus (view optic s)]
                 [new-focus (subtract-scaled-value focus new-v lr)]
                 [new-s (set-lens optic new-focus s)])
            (loop new-s new-v (+ iter 1)))))))

(doc 'section 'adam)

(define (adam-at optic structure loss-fn lr max-iters)
  (doc 'type '(-> Optic s (-> s-traced Traced) Number Nat s))
  (doc 'description "Adam optimization at optic focus with default hyperparameters")
  (doc 'note "Uses default Adam hyperparameters:
  beta1 = 0.9 (first moment decay)
  beta2 = 0.999 (second moment decay)
  epsilon = 1e-8 (numerical stability)")
  (adam-at-full optic structure loss-fn lr 0.9 0.999 1e-8 max-iters))

;;; adam-at-full : Optic × s × (s-traced → Traced) × Number × Number × Number × Number × Nat → s
;;; Adam optimization with full hyperparameter control.
(define (adam-at-full optic structure loss-fn lr beta1 beta2 epsilon max-iters)
  (let* ([focus (view optic structure)]
         [m (zero-like-value focus)]    ; First moment estimate
         [v (zero-like-value focus)])   ; Second moment estimate
    (let loop ([s structure] [m m] [v v] [t 1])
      (if (> t max-iters)
          s
          (let* ([grad (optic-gradient loss-fn optic s)]
                 ;; Update biased first moment: m = beta1 * m + (1 - beta1) * grad
                 [new-m (adam-moment-update m grad beta1)]
                 ;; Update biased second moment: v = beta2 * v + (1 - beta2) * grad^2
                 [new-v (adam-second-moment-update v grad beta2)]
                 ;; Bias correction
                 [m-hat (scale-value new-m (/ 1 (- 1 (expt beta1 t))))]
                 [v-hat (scale-value new-v (/ 1 (- 1 (expt beta2 t))))]
                 ;; Update: focus = focus - lr * m-hat / (sqrt(v-hat) + epsilon)
                 [focus (view optic s)]
                 [new-focus (adam-update focus m-hat v-hat lr epsilon)]
                 [new-s (set-lens optic new-focus s)])
            (loop new-s new-m new-v (+ t 1)))))))

(doc 'section 'rmsprop)

(define (rmsprop-at optic structure loss-fn lr gamma max-iters)
  (doc 'type '(-> Optic s (-> s-traced Traced) Number Number Nat s))
  (doc 'description "RMSprop optimization at optic focus")
  (doc 'param 'gamma "Decay rate (typically 0.9)")
  (let* ([focus (view optic structure)]
         [sq-avg (zero-like-value focus)])  ; Running average of squared gradients
    (let loop ([s structure] [sq-avg sq-avg] [iter 0])
      (if (>= iter max-iters)
          s
          (let* ([grad (optic-gradient loss-fn optic s)]
                 ;; sq_avg = gamma * sq_avg + (1 - gamma) * grad^2
                 [new-sq-avg (rmsprop-update-sq-avg sq-avg grad gamma)]
                 ;; focus = focus - lr * grad / sqrt(sq_avg + epsilon)
                 [focus (view optic s)]
                 [new-focus (rmsprop-update focus grad new-sq-avg lr 1e-8)]
                 [new-s (set-lens optic new-focus s)])
            (loop new-s new-sq-avg (+ iter 1)))))))

;;; ============================================================
;;; Helper Functions for Structured Values
;;; ============================================================

;;; zero-like-value : α → α
;;; Create zero value matching structure.
(define (zero-like-value val)
  (cond
    [(number? val) 0.0]
    [(vec2? val) (vec2 0.0 0.0)]
    [(list? val) (map zero-like-value val)]
    [(pair? val) (cons (zero-like-value (car val))
                       (zero-like-value (cdr val)))]
    [else 0.0]))

;;; scale-value : α × Number → α
;;; Scale a structured value by a scalar.
(define (scale-value val factor)
  (cond
    [(number? val) (* val factor)]
    [(vec2? val) (vec2 (* (vec2-x val) factor)
                       (* (vec2-y val) factor))]
    [(list? val) (map (lambda (x) (scale-value x factor)) val)]
    [(pair? val) (cons (scale-value (car val) factor)
                       (scale-value (cdr val) factor))]
    [else val]))

;;; subtract-scaled-value : α × α × Number → α
;;; Compute val - scale * gradient.
(define (subtract-scaled-value val grad scale)
  (cond
    [(number? val) (- val (* scale grad))]
    [(vec2? val) (vec2 (- (vec2-x val) (* scale (vec2-x grad)))
                       (- (vec2-y val) (* scale (vec2-y grad))))]
    [(list? val)
     (map (lambda (v g) (subtract-scaled-value v g scale)) val grad)]
    [(pair? val)
     (cons (subtract-scaled-value (car val) (car grad) scale)
           (subtract-scaled-value (cdr val) (cdr grad) scale))]
    [else val]))

;;; square-value : α → α
;;; Element-wise square.
(define (square-value val)
  (cond
    [(number? val) (* val val)]
    [(vec2? val) (vec2 (* (vec2-x val) (vec2-x val))
                       (* (vec2-y val) (vec2-y val)))]
    [(list? val) (map square-value val)]
    [(pair? val) (cons (square-value (car val))
                       (square-value (cdr val)))]
    [else val]))

;;; sqrt-value : α → α
;;; Element-wise square root.
(define (sqrt-value val)
  (cond
    [(number? val) (sqrt val)]
    [(vec2? val) (vec2 (sqrt (vec2-x val))
                       (sqrt (vec2-y val)))]
    [(list? val) (map sqrt-value val)]
    [(pair? val) (cons (sqrt-value (car val))
                       (sqrt-value (cdr val)))]
    [else val]))

;;; add-values : α × α → α
;;; Element-wise addition.
(define (add-values a b)
  (cond
    [(number? a) (+ a b)]
    [(vec2? a) (vec2 (+ (vec2-x a) (vec2-x b))
                     (+ (vec2-y a) (vec2-y b)))]
    [(list? a) (map add-values a b)]
    [(pair? a) (cons (add-values (car a) (car b))
                     (add-values (cdr a) (cdr b)))]
    [else a]))

;;; ============================================================
;;; Optimizer-Specific Update Functions
;;; ============================================================

;;; momentum-velocity-update : v × grad × beta → v'
;;; v' = beta * v + grad
(define (momentum-velocity-update v grad beta)
  (add-values (scale-value v beta) grad))

;;; adam-moment-update : m × grad × beta → m'
;;; m' = beta * m + (1 - beta) * grad
(define (adam-moment-update m grad beta)
  (add-values (scale-value m beta)
              (scale-value grad (- 1 beta))))

;;; adam-second-moment-update : v × grad × beta → v'
;;; v' = beta * v + (1 - beta) * grad^2
(define (adam-second-moment-update v grad beta)
  (add-values (scale-value v beta)
              (scale-value (square-value grad) (- 1 beta))))

;;; adam-update : focus × m-hat × v-hat × lr × epsilon → focus'
;;; focus' = focus - lr * m-hat / (sqrt(v-hat) + epsilon)
(define (adam-update focus m-hat v-hat lr epsilon)
  (let ([denom (add-values (sqrt-value v-hat)
                           (if (number? v-hat) epsilon (scale-value (zero-like-value v-hat) 0)))])
    ;; Add epsilon element-wise (simplified: just add to sqrt result)
    (subtract-scaled-value focus
                           (divide-values m-hat
                                          (add-scalar-to-value (sqrt-value v-hat) epsilon))
                           lr)))

;;; divide-values : α × α → α
;;; Element-wise division.
(define (divide-values a b)
  (cond
    [(number? a) (/ a b)]
    [(vec2? a) (vec2 (/ (vec2-x a) (vec2-x b))
                     (/ (vec2-y a) (vec2-y b)))]
    [(list? a) (map divide-values a b)]
    [(pair? a) (cons (divide-values (car a) (car b))
                     (divide-values (cdr a) (cdr b)))]
    [else a]))

;;; add-scalar-to-value : α × Number → α
;;; Add scalar to each element.
(define (add-scalar-to-value val scalar)
  (cond
    [(number? val) (+ val scalar)]
    [(vec2? val) (vec2 (+ (vec2-x val) scalar)
                       (+ (vec2-y val) scalar))]
    [(list? val) (map (lambda (x) (add-scalar-to-value x scalar)) val)]
    [(pair? val) (cons (add-scalar-to-value (car val) scalar)
                       (add-scalar-to-value (cdr val) scalar))]
    [else val]))

;;; rmsprop-update-sq-avg : sq-avg × grad × gamma → sq-avg'
;;; sq-avg' = gamma * sq-avg + (1 - gamma) * grad^2
(define (rmsprop-update-sq-avg sq-avg grad gamma)
  (add-values (scale-value sq-avg gamma)
              (scale-value (square-value grad) (- 1 gamma))))

;;; rmsprop-update : focus × grad × sq-avg × lr × epsilon → focus'
;;; focus' = focus - lr * grad / sqrt(sq_avg + epsilon)
(define (rmsprop-update focus grad sq-avg lr epsilon)
  (subtract-scaled-value focus
                         (divide-values grad
                                        (sqrt-value (add-scalar-to-value sq-avg epsilon)))
                         lr))

