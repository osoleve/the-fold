(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'convergence)
(require 'line-search)
(require 'first-order)
(require 'newton)
(require 'lbfgs)

(doc 'module 'optimize)
(doc 'description "Unified optimization API - main entry point for all optimizers")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'overview)
(doc 'note "Main entry point for optimization. Loads all optimizers and provides a unified interface.

Usage:
  (minimize f x0)                    ; L-BFGS with defaults
  (minimize f x0 'adam)              ; Adam optimizer
  (minimize f x0 'newton)            ; Newton's method
  (minimize-bounded f x0 lo hi)      ; L-BFGS-B with bounds")

(doc 'section 'unified-interface)

(define (minimize f x0 . opts)
  (doc 'export #t)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) OptResult))
  (doc 'description "Minimize a function starting from x0")
  (doc 'param 'method "Optional: 'sgd, 'momentum, 'adam, 'adamw, 'muon, 'rmsprop, 'adagrad, 'lbfgs, 'newton, 'newton-cg")
  (doc 'param 'criteria "Optional: ConvergenceCriteria (or use defaults)")
  (doc 'note "Examples:
  (minimize f x0)                    ; L-BFGS with defaults
  (minimize f x0 'adam)              ; Adam with defaults
  (minimize f x0 'lbfgs criteria)    ; L-BFGS with custom criteria")
  (let* ([method (if (and (pair? opts) (symbol? (car opts)))
                     (car opts)
                     'lbfgs)]
         [rest (if (and (pair? opts) (symbol? (car opts)))
                   (cdr opts)
                   opts)]
         [criteria (if (and (pair? rest) (convergence-criteria? (car rest)))
                       (car rest)
                       *default-convergence*)])
        (case method
              ;; First-order methods
              [(sgd gradient-descent)
               (gradient-descent f x0 0.01 criteria)]
              [(momentum)
               (momentum f x0 0.01 0.9 criteria)]
              [(adam)
               (adam f x0 0.001 criteria)]
              [(rmsprop)
               (rmsprop f x0 0.001 criteria)]
              [(adagrad)
               (adagrad f x0 0.01 criteria)]
              [(nesterov nag)
               (nesterov f x0 0.01 0.9 criteria)]
              [(adamw)
               (adamw f x0 0.001 0.01 criteria)]
              [(muon)
               ;; Note: For matrix-shaped parameters with Newton-Schulz orthogonalization,
               ;; use muon-matrix directly (different signature: takes Matrix, not List)
               (muon f x0 0.02 criteria)]
              ;; Second-order methods
              [(newton)
               (newton-method f x0 criteria)]
              [(modified-newton)
               (modified-newton f x0 criteria)]
              [(newton-cg truncated-newton)
               (newton-cg f x0 criteria)]
              ;; Quasi-Newton methods
              [(lbfgs bfgs)
               (lbfgs f x0 criteria)]
              ;; Default
              [else
               (lbfgs f x0 criteria)])))

(define (minimize-bounded f x0 lower upper . opts)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) (List Number) (List Number) OptResult))
  (doc 'description "Minimize with box constraints")
  (doc 'param 'f "Objective function")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lower "Lower bounds (use -inf.0 for no lower bound)")
  (doc 'param 'upper "Upper bounds (use +inf.0 for no upper bound)")
  (let ([criteria (if (and (pair? opts) (convergence-criteria? (car opts)))
                      (car opts)
                      *default-convergence*)])
       (lbfgs-b f x0 lower upper criteria)))

(doc 'section 'learning-rate-finder)

(define (find-learning-rate f x0 lr-min lr-max num-steps)
  (doc 'type '(-> (-> (List TracedValue) TracedValue) (List Number) Number Number Nat (List (Pair Number Number))))
  (doc 'description "Find good learning rate by exponentially increasing lr and tracking loss")
  (doc 'returns "List of (learning-rate, loss) pairs")
  (doc 'note "Look for the steepest descent region before loss explodes")
  (let* ([lr-mult (expt (/ lr-max lr-min) (/ 1 (- num-steps 1)))])
        (let loop ([x x0]
                   [lr lr-min]
                   [step 0]
                   [results '()])
             (if (>= step num-steps)
                 (reverse results)
                 (let* ([loss (apply f x)]
                        [grad (gradient f x)]
                        [x-new (map (lambda (xi gi) (- xi (* lr gi))) x grad)]
                        [new-lr (* lr lr-mult)])
                       (loop x-new new-lr (+ step 1)
                             (cons (list lr loss) results)))))))

;;; ====
;;; Optimization with Callbacks
;;; ====

;;; minimize-callback : ((List TracedValue) → TracedValue) × (List Number) × Symbol × ConvergenceCriteria × (ConvergenceState → Unit) → OptResult
;;; Minimize with iteration callback for monitoring/logging.
;;; Note: callback is impure but doesn't affect optimization result.
(define (minimize-callback f x0 method criteria callback)
  ;; For now, just use standard minimize
  ;; Callback support would require modifying individual optimizers
  (minimize f x0 method criteria))

(doc 'section 'test-functions)

(define (rosenbrock args)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Rosenbrock function: f(x,y) = (1-x)^2 + 100*(y-x^2)^2")
  (doc 'note "Minimum at (1, 1) with f = 0")
  (let ([x (car args)]
        [y (cadr args)])
       (+ (* (- 1 x) (- 1 x))
          (* 100 (- y (* x x)) (- y (* x x))))))

;;; rosenbrock-traced : (List TracedValue) → TracedValue
;;; Rosenbrock using traced operations for autodiff.
(define (rosenbrock-traced x y)
  (traced-add (traced-sq (traced-sub 1 x))
              (traced-mul 100 (traced-sq (traced-sub y (traced-sq x))))))

;;; sphere : (List Number) → Number
;;; Sphere function: f(x) = sum(x_i^2)
;;; Minimum at origin with f = 0
(define (sphere args)
  (fold-left + 0 (map (lambda (x) (* x x)) args)))

;;; rastrigin : (List Number) → Number
;;; Rastrigin function (multimodal): f(x) = 10n + sum(x_i^2 - 10*cos(2*pi*x_i))
;;; Global minimum at origin with f = 0
(define (rastrigin args)
  (let ([n (length args)]
        [pi 3.141592653589793])
       (+ (* 10 n)
          (fold-left + 0
                     (map (lambda (x)
                                  (- (* x x) (* 10 (cos (* 2 pi x)))))
                          args)))))

;;; beale : (List Number) → Number
;;; Beale function: common benchmark with minimum at (3, 0.5)
(define (beale args)
  (let ([x (car args)]
        [y (cadr args)])
       (+ (expt (- 1.5 (+ x (* (- 1 y) x))) 2)
          (expt (- 2.25 (+ x (* (- 1 (* y y)) x))) 2)
          (expt (- 2.625 (+ x (* (- 1 (* y y y)) x))) 2))))

(doc 'section 'method-selection)

(define (suggest-method n is-smooth is-convex)
  (doc 'type '(-> Nat Boolean Boolean Symbol))
  (doc 'description "Suggest optimization method based on problem characteristics")
  (doc 'param 'n "Number of variables")
  (doc 'param 'is-smooth "Whether function is smooth (continuous second derivatives)")
  (doc 'param 'is-convex "Whether function is convex")
  (cond
   ;; Small convex smooth problems: Newton is fastest
   [(and is-smooth is-convex (< n 100))
    'newton]
   ;; Large smooth problems: L-BFGS
   [(and is-smooth (>= n 100))
    'lbfgs]
   ;; Medium smooth problems: L-BFGS or Newton-CG
   [is-smooth
    'lbfgs]
   ;; Non-smooth or unknown: Adam is robust
   [else
    'adam]))

(doc 'section 'summary)
(doc 'note "Available optimizers:

First-order (gradient only):
  - gradient-descent : Basic gradient descent with fixed learning rate
  - sgd             : Stochastic gradient descent (alias)
  - momentum        : Gradient descent with momentum
  - nesterov        : Nesterov accelerated gradient
  - adam            : Adaptive moment estimation
  - adamw           : Adam with decoupled weight decay
  - muon            : Momentum orthogonalized update (normalized momentum)
  - rmsprop         : RMSprop adaptive learning rate
  - adagrad         : Adagrad adaptive learning rate

Second-order (uses Hessian):
  - newton-method   : Newton's method with line search
  - modified-newton : Newton with positive-definite Hessian enforcement
  - newton-cg       : Truncated Newton using conjugate gradient
  - gauss-newton    : For nonlinear least squares

Quasi-Newton (approximates Hessian):
  - lbfgs           : Limited-memory BFGS (default)
  - lbfgs-b         : L-BFGS with box constraints

Line search methods:
  - armijo-backtrack    : Backtracking with Armijo condition
  - wolfe-line-search   : Strong Wolfe conditions

Convergence:
  - make-convergence-criteria : Create custom stopping conditions
  - *default-convergence*     : Default convergence criteria
  - converged?                : Check if optimization converged")
