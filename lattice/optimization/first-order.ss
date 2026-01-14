;;; lattice/optimization/first-order.ss --- First-Order Optimization Algorithms
;;;
;;; Gradient-based optimization methods that use only first derivatives.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Algorithms:
;;;   - SGD (Stochastic Gradient Descent)
;;;   - Momentum
;;;   - Adam
;;;   - RMSprop
;;;   - Adagrad
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - autodiff/reverse-diff.ss
;;;   - optimization/convergence.ss
;;;   - optimization/line-search.ss

(load "core/base/prelude.ss")
(load "lattice/autodiff/reverse-diff.ss")
(load "lattice/optimization/convergence.ss")
(load "lattice/optimization/line-search.ss")

;;; ====
;;; Gradient Descent (Vanilla)
;;; ====

;;; gradient-descent : ((List TracedValue) → TracedValue) × (List Number) × Number × ConvergenceCriteria → OptResult
;;; Vanilla gradient descent with fixed learning rate.
;;;   f: objective function (uses traced operations)
;;;   x0: initial point
;;;   lr: learning rate
;;;   criteria: convergence criteria
(define (gradient-descent f x0 lr criteria)
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

;;; sgd : ((List TracedValue) → TracedValue) × (List Number) × Number × Nat → (List Number)
;;; Simple SGD interface (no convergence tracking).
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate
;;;   max-iters: maximum iterations
(define (sgd f x0 lr max-iters)
  (let loop ([x x0] [iter 0])
       (if (>= iter max-iters)
           x
           (let* ([grad (gradient f x)]
                  [x-new (map (lambda (xi gi) (- xi (* lr gi))) x grad)])
                 (loop x-new (+ iter 1))))))

;;; ====
;;; Gradient Descent with Momentum
;;; ====

;;; momentum : ((List TracedValue) → TracedValue) × (List Number) × Number × Number × ConvergenceCriteria → OptResult
;;; Gradient descent with momentum (heavy ball method).
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate
;;;   beta: momentum coefficient (typically 0.9)
;;;   criteria: convergence criteria
(define (momentum f x0 lr beta criteria)
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

;;; ====
;;; Adam Optimizer
;;; ====

;;; adam : ((List TracedValue) → TracedValue) × (List Number) × Number × ConvergenceCriteria → OptResult
;;; Adam optimizer with default hyperparameters.
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate (default: 0.001)
;;;   criteria: convergence criteria
(define (adam f x0 lr criteria)
  (adam-full f x0 lr 0.9 0.999 1e-8 criteria))

;;; adam-full : ((List TracedValue) → TracedValue) × (List Number) × Number × Number × Number × Number × ConvergenceCriteria → OptResult
;;; Adam optimizer with all hyperparameters.
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate
;;;   beta1: first moment decay (typically 0.9)
;;;   beta2: second moment decay (typically 0.999)
;;;   epsilon: numerical stability constant
;;;   criteria: convergence criteria
(define (adam-full f x0 lr beta1 beta2 epsilon criteria)
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

;;; ====
;;; RMSprop Optimizer
;;; ====

;;; rmsprop : ((List TracedValue) → TracedValue) × (List Number) × Number × ConvergenceCriteria → OptResult
;;; RMSprop optimizer.
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate
;;;   criteria: convergence criteria
(define (rmsprop f x0 lr criteria)
  (rmsprop-full f x0 lr 0.99 1e-8 criteria))

;;; rmsprop-full : ((List TracedValue) → TracedValue) × (List Number) × Number × Number × Number × ConvergenceCriteria → OptResult
;;; RMSprop with all hyperparameters.
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate
;;;   decay: decay rate for running average (typically 0.99)
;;;   epsilon: numerical stability constant
;;;   criteria: convergence criteria
(define (rmsprop-full f x0 lr decay epsilon criteria)
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

;;; ====
;;; Adagrad Optimizer
;;; ====

;;; adagrad : ((List TracedValue) → TracedValue) × (List Number) × Number × ConvergenceCriteria → OptResult
;;; Adagrad optimizer (adaptive gradient).
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate
;;;   criteria: convergence criteria
(define (adagrad f x0 lr criteria)
  (adagrad-full f x0 lr 1e-8 criteria))

;;; adagrad-full : ((List TracedValue) → TracedValue) × (List Number) × Number × Number × ConvergenceCriteria → OptResult
;;; Adagrad with all hyperparameters.
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate
;;;   epsilon: numerical stability constant
;;;   criteria: convergence criteria
(define (adagrad-full f x0 lr epsilon criteria)
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

;;; ====
;;; Gradient Descent with Line Search
;;; ====

;;; gradient-descent-ls : ((List TracedValue) → TracedValue) × (List Number) × ConvergenceCriteria → OptResult
;;; Gradient descent with Armijo line search.
;;;   f: objective function
;;;   x0: initial point
;;;   criteria: convergence criteria
(define (gradient-descent-ls f x0 criteria)
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

;;; ====
;;; Nesterov Accelerated Gradient (NAG)
;;; ====

;;; nesterov : ((List TracedValue) → TracedValue) × (List Number) × Number × Number × ConvergenceCriteria → OptResult
;;; Nesterov accelerated gradient descent.
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate
;;;   beta: momentum coefficient (typically 0.9)
;;;   criteria: convergence criteria
(define (nesterov f x0 lr beta criteria)
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

;;; ====
;;; AdamW (Adam with Decoupled Weight Decay)
;;; ====

;;; adamw : ((List TracedValue) → TracedValue) × (List Number) × Number × Number × ConvergenceCriteria → OptResult
;;; AdamW optimizer (Adam with decoupled weight decay regularization).
;;;   f: objective function
;;;   x0: initial point
;;;   lr: learning rate
;;;   weight-decay: weight decay coefficient (typically 0.01)
;;;   criteria: convergence criteria
(define (adamw f x0 lr weight-decay criteria)
  (adamw-full f x0 lr 0.9 0.999 1e-8 weight-decay criteria))

;;; adamw-full : ((List TracedValue) → TracedValue) × (List Number) × Number × Number × Number × Number × Number × ConvergenceCriteria → OptResult
(define (adamw-full f x0 lr beta1 beta2 epsilon weight-decay criteria)
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
