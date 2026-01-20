(load "core/base/prelude.ss")
(load "core/autodiff/reverse-diff.ss")
(load "lattice/optimization/convergence.ss")
(load "lattice/optimization/line-search.ss")

(doc 'module 'first-order)
(doc 'description "First-order optimization algorithms using gradient information")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'overview)
(doc 'note "Gradient-based optimization methods that use only first derivatives.
Algorithms:
  - SGD (Stochastic Gradient Descent)
  - Momentum
  - Adam
  - RMSprop
  - Adagrad
  - Nesterov Accelerated Gradient
  - AdamW (Adam with decoupled weight decay)")

(doc 'section 'gradient-descent)

(define (gradient-descent f x0 lr criteria)
  (doc 'export #t)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number ConvergenceCriteria OptResult))
  (doc 'description "Vanilla gradient descent with fixed learning rate")
  (doc 'param 'f "Objective function (uses traced operations)")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'criteria "Convergence criteria")
  (let* ([init-grad (gradient f x0)]
         [init-f (apply f x0)]
         [init-state (initial-convergence-state init-f init-grad)])
        (let loop ([x x0]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      ;; Converged
                      (make-opt-result x (cs-f-val state) (gradient f x)
                                       (cs-iter state) reason)
                      ;; Take gradient step
                      (let* ([grad (gradient f x)]
                             [x-new (map (lambda (xi gi) (- xi (* lr gi))) x grad)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new new-state)))))))

(define (sgd f x0 lr max-iters)
  (doc 'export #t)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number Nat (List Number)))
  (doc 'description "Simple SGD interface (no convergence tracking)")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'max-iters "Maximum iterations")
  (let loop ([x x0] [iter 0])
       (if (>= iter max-iters)
           x
           (let* ([grad (gradient f x)]
                  [x-new (map (lambda (xi gi) (- xi (* lr gi))) x grad)])
                 (loop x-new (+ iter 1))))))

(doc 'section 'momentum)

(define (momentum f x0 lr beta criteria)
  (doc 'export #t)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number Number ConvergenceCriteria OptResult))
  (doc 'description "Gradient descent with momentum (heavy ball method)")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'beta "Momentum coefficient (typically 0.9)")
  (doc 'param 'criteria "Convergence criteria")
  (let* ([n (length x0)]
         [init-v (make-list n 0)]  ; Initial velocity
         [init-grad (gradient f x0)]
         [init-f (apply f x0)]
         [init-state (initial-convergence-state init-f init-grad)])
        (let loop ([x x0]
                   [v init-v]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      (make-opt-result x (cs-f-val state) (gradient f x)
                                       (cs-iter state) reason)
                      (let* ([grad (gradient f x)]
                             ;; v = beta * v + grad
                             [v-new (map (lambda (vi gi) (+ (* beta vi) gi)) v grad)]
                             ;; x = x - lr * v
                             [x-new (map (lambda (xi vi) (- xi (* lr vi))) x v-new)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new v-new new-state)))))))

(doc 'section 'adam)

(define (adam f x0 lr criteria)
  (doc 'export #t)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number ConvergenceCriteria OptResult))
  (doc 'description "Adam optimizer with default hyperparameters")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate (default: 0.001)")
  (doc 'param 'criteria "Convergence criteria")
  (adam-full f x0 lr 0.9 0.999 1e-8 criteria))

(define (adam-full f x0 lr beta1 beta2 epsilon criteria)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number Number Number Number ConvergenceCriteria OptResult))
  (doc 'description "Adam optimizer with all hyperparameters")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'beta1 "First moment decay (typically 0.9)")
  (doc 'param 'beta2 "Second moment decay (typically 0.999)")
  (doc 'param 'epsilon "Numerical stability constant")
  (doc 'param 'criteria "Convergence criteria")
  (let* ([n (length x0)]
         [init-m (make-list n 0)]   ; First moment estimate
         [init-v (make-list n 0)]   ; Second moment estimate
         [init-grad (gradient f x0)]
         [init-f (apply f x0)]
         [init-state (initial-convergence-state init-f init-grad)])
        (let loop ([x x0]
                   [m init-m]
                   [v init-v]
                   [t 1]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      (make-opt-result x (cs-f-val state) (gradient f x)
                                       (cs-iter state) reason)
                      (let* ([grad (gradient f x)]
                             ;; Update biased first moment: m = beta1*m + (1-beta1)*g
                             [m-new (map (lambda (mi gi)
                                                 (+ (* beta1 mi) (* (- 1 beta1) gi)))
                                         m grad)]
                             ;; Update biased second moment: v = beta2*v + (1-beta2)*g^2
                             [v-new (map (lambda (vi gi)
                                                 (+ (* beta2 vi) (* (- 1 beta2) (* gi gi))))
                                         v grad)]
                             ;; Bias correction
                             [m-hat (map (lambda (mi) (/ mi (- 1 (expt beta1 t)))) m-new)]
                             [v-hat (map (lambda (vi) (/ vi (- 1 (expt beta2 t)))) v-new)]
                             ;; Update parameters
                             [x-new (map (lambda (xi mhi vhi)
                                                 (- xi (* lr (/ mhi (+ (sqrt vhi) epsilon)))))
                                         x m-hat v-hat)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new m-new v-new (+ t 1) new-state)))))))

(doc 'section 'rmsprop)

(define (rmsprop f x0 lr criteria)
  (doc 'export #t)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number ConvergenceCriteria OptResult))
  (doc 'description "RMSprop optimizer")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'criteria "Convergence criteria")
  (rmsprop-full f x0 lr 0.99 1e-8 criteria))

(define (rmsprop-full f x0 lr decay epsilon criteria)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number Number Number ConvergenceCriteria OptResult))
  (doc 'description "RMSprop with all hyperparameters")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'decay "Decay rate for running average (typically 0.99)")
  (doc 'param 'epsilon "Numerical stability constant")
  (doc 'param 'criteria "Convergence criteria")
  (let* ([n (length x0)]
         [init-v (make-list n 0)]  ; Running average of squared gradients
         [init-grad (gradient f x0)]
         [init-f (apply f x0)]
         [init-state (initial-convergence-state init-f init-grad)])
        (let loop ([x x0]
                   [v init-v]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      (make-opt-result x (cs-f-val state) (gradient f x)
                                       (cs-iter state) reason)
                      (let* ([grad (gradient f x)]
                             ;; Update running average: v = decay*v + (1-decay)*g^2
                             [v-new (map (lambda (vi gi)
                                                 (+ (* decay vi) (* (- 1 decay) (* gi gi))))
                                         v grad)]
                             ;; Update parameters
                             [x-new (map (lambda (xi gi vi)
                                                 (- xi (* lr (/ gi (+ (sqrt vi) epsilon)))))
                                         x grad v-new)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new v-new new-state)))))))

(doc 'section 'adagrad)

(define (adagrad f x0 lr criteria)
  (doc 'export #t)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number ConvergenceCriteria OptResult))
  (doc 'description "Adagrad optimizer (adaptive gradient)")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'criteria "Convergence criteria")
  (adagrad-full f x0 lr 1e-8 criteria))

(define (adagrad-full f x0 lr epsilon criteria)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number Number ConvergenceCriteria OptResult))
  (doc 'description "Adagrad with all hyperparameters")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'epsilon "Numerical stability constant")
  (doc 'param 'criteria "Convergence criteria")
  (let* ([n (length x0)]
         [init-g (make-list n 0)]  ; Sum of squared gradients
         [init-grad (gradient f x0)]
         [init-f (apply f x0)]
         [init-state (initial-convergence-state init-f init-grad)])
        (let loop ([x x0]
                   [g init-g]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      (make-opt-result x (cs-f-val state) (gradient f x)
                                       (cs-iter state) reason)
                      (let* ([grad (gradient f x)]
                             ;; Accumulate squared gradients: g = g + grad^2
                             [g-new (map (lambda (gi gradi) (+ gi (* gradi gradi))) g grad)]
                             ;; Update parameters
                             [x-new (map (lambda (xi gradi gi)
                                                 (- xi (* lr (/ gradi (+ (sqrt gi) epsilon)))))
                                         x grad g-new)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new g-new new-state)))))))

(doc 'section 'line-search-variants)

(define (gradient-descent-ls f x0 criteria)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) ConvergenceCriteria OptResult))
  (doc 'description "Gradient descent with Armijo line search")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'criteria "Convergence criteria")
  (let* ([init-grad (gradient f x0)]
         [init-f (apply f x0)]
         [init-state (initial-convergence-state init-f init-grad)])
        (let loop ([x x0]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      (make-opt-result x (cs-f-val state) (gradient f x)
                                       (cs-iter state) reason)
                      (let* ([grad (gradient f x)]
                             ;; Search direction = negative gradient
                             [direction (map - grad)]
                             ;; Line search
                             [ls-result (armijo-backtrack f x grad direction)]
                             [x-new (car ls-result)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new new-state)))))))

(doc 'section 'nesterov)

(define (nesterov f x0 lr beta criteria)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number Number ConvergenceCriteria OptResult))
  (doc 'description "Nesterov accelerated gradient descent")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'beta "Momentum coefficient (typically 0.9)")
  (doc 'param 'criteria "Convergence criteria")
  (let* ([n (length x0)]
         [init-v (make-list n 0)]
         [init-grad (gradient f x0)]
         [init-f (apply f x0)]
         [init-state (initial-convergence-state init-f init-grad)])
        (let loop ([x x0]
                   [v init-v]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      (make-opt-result x (cs-f-val state) (gradient f x)
                                       (cs-iter state) reason)
                      ;; Look ahead: compute gradient at x - lr*beta*v (lookahead position)
                      (let* ([x-lookahead (map (lambda (xi vi) (- xi (* lr beta vi))) x v)]
                             [grad (gradient f x-lookahead)]
                             ;; Update velocity: v = beta*v + grad (same as momentum)
                             [v-new (map (lambda (vi gi) (+ (* beta vi) gi)) v grad)]
                             ;; Update position: x = x - lr*v (same as momentum)
                             [x-new (map (lambda (xi vi) (- xi (* lr vi))) x v-new)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new v-new new-state)))))))

(doc 'section 'adamw)

(define (adamw f x0 lr weight-decay criteria)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number Number ConvergenceCriteria OptResult))
  (doc 'description "AdamW optimizer (Adam with decoupled weight decay regularization)")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'weight-decay "Weight decay coefficient (typically 0.01)")
  (doc 'param 'criteria "Convergence criteria")
  (adamw-full f x0 lr 0.9 0.999 1e-8 weight-decay criteria))

(define (adamw-full f x0 lr beta1 beta2 epsilon weight-decay criteria)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number Number Number Number Number ConvergenceCriteria OptResult))
  (doc 'description "AdamW optimizer with all hyperparameters")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lr "Learning rate")
  (doc 'param 'beta1 "First moment decay (typically 0.9)")
  (doc 'param 'beta2 "Second moment decay (typically 0.999)")
  (doc 'param 'epsilon "Numerical stability constant")
  (doc 'param 'weight-decay "Weight decay coefficient")
  (doc 'param 'criteria "Convergence criteria")
  (let* ([n (length x0)]
         [init-m (make-list n 0)]
         [init-v (make-list n 0)]
         [init-grad (gradient f x0)]
         [init-f (apply f x0)]
         [init-state (initial-convergence-state init-f init-grad)])
        (let loop ([x x0]
                   [m init-m]
                   [v init-v]
                   [t 1]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      (make-opt-result x (cs-f-val state) (gradient f x)
                                       (cs-iter state) reason)
                      (let* ([grad (gradient f x)]
                             ;; Adam moment updates
                             [m-new (map (lambda (mi gi)
                                                 (+ (* beta1 mi) (* (- 1 beta1) gi)))
                                         m grad)]
                             [v-new (map (lambda (vi gi)
                                                 (+ (* beta2 vi) (* (- 1 beta2) (* gi gi))))
                                         v grad)]
                             [m-hat (map (lambda (mi) (/ mi (- 1 (expt beta1 t)))) m-new)]
                             [v-hat (map (lambda (vi) (/ vi (- 1 (expt beta2 t)))) v-new)]
                             ;; Decoupled weight decay: x = x - lr*weight_decay*x
                             [x-decayed (map (lambda (xi) (* xi (- 1 (* lr weight-decay)))) x)]
                             ;; Adam update on decayed parameters
                             [x-new (map (lambda (xi mhi vhi)
                                                 (- xi (* lr (/ mhi (+ (sqrt vhi) epsilon)))))
                                         x-decayed m-hat v-hat)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new m-new v-new (+ t 1) new-state)))))))
