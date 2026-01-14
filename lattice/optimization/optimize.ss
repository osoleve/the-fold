;;; lattice/optimization/optimize.ss --- Unified Optimization API
;;;
;;; Main entry point for optimization. Loads all optimizers and provides
;;; a unified interface for minimizing functions.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Usage:
;;;   (minimize f x0)                    ; L-BFGS with defaults
;;;   (minimize f x0 'adam)              ; Adam optimizer
;;;   (minimize f x0 'newton)            ; Newton's method
;;;   (minimize-bounded f x0 lo hi)      ; L-BFGS-B with bounds
;;;
;;; Dependencies:
;;;   - All optimization submodules

(load "core/base/prelude.ss")
(load "lattice/optimization/convergence.ss")
(load "lattice/optimization/line-search.ss")
(load "lattice/optimization/first-order.ss")
(load "lattice/optimization/newton.ss")
(load "lattice/optimization/lbfgs.ss")

;;; ====
;;; Unified Minimization Interface
;;; ====

;;; minimize : ((List TracedValue) → TracedValue) × (List Number) × ... → OptResult
;;; Minimize a function starting from x0.
;;; Optional arguments:
;;;   method: 'sgd, 'momentum, 'adam, 'rmsprop, 'adagrad, 'lbfgs, 'newton, 'newton-cg
;;;   criteria: ConvergenceCriteria (or use defaults)
;;;
;;; Examples:
;;;   (minimize f x0)                    ; L-BFGS with defaults
;;;   (minimize f x0 'adam)              ; Adam with defaults
;;;   (minimize f x0 'lbfgs criteria)    ; L-BFGS with custom criteria
(define (minimize f x0 . opts)
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

;;; minimize-bounded : ((List TracedValue) → TracedValue) × (List Number) × (List Number) × (List Number) × ... → OptResult
;;; Minimize with box constraints.
;;;   f: objective function
;;;   x0: initial point
;;;   lower: lower bounds (use -inf.0 for no lower bound)
;;;   upper: upper bounds (use +inf.0 for no upper bound)
(define (minimize-bounded f x0 lower upper . opts)
  (let ([criteria (if (and (pair? opts) (convergence-criteria? (car opts)))
                      (car opts)
                      *default-convergence*)])
       (lbfgs-b f x0 lower upper criteria)))

;;; ====
;;; Learning Rate Finder
;;; ====

;;; find-learning-rate : ((List TracedValue) → TracedValue) × (List Number) × Number × Number × Nat → (List (Number × Number))
;;; Find good learning rate by exponentially increasing lr and tracking loss.
;;; Returns list of (learning-rate, loss) pairs.
;;; Look for the steepest descent region before loss explodes.
(define (find-learning-rate f x0 lr-min lr-max num-steps)
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

;;; ====
;;; Common Test Functions
;;; ====

;;; rosenbrock : (List Number) → Number
;;; Rosenbrock function: f(x,y) = (1-x)^2 + 100*(y-x^2)^2
;;; Minimum at (1, 1) with f = 0
(define (rosenbrock args)
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

;;; ====
;;; Method Selection Heuristics
;;; ====

;;; suggest-method : Nat × Bool × Bool → Symbol
;;; Suggest optimization method based on problem characteristics.
;;;   n: number of variables
;;;   is-smooth: whether function is smooth (continuous second derivatives)
;;;   is-convex: whether function is convex
(define (suggest-method n is-smooth is-convex)
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

;;; ====
;;; Exported Summary
;;; ====

;;; Available optimizers:
;;;
;;; First-order (gradient only):
;;;   - gradient-descent : Basic gradient descent with fixed learning rate
;;;   - sgd             : Stochastic gradient descent (alias)
;;;   - momentum        : Gradient descent with momentum
;;;   - nesterov        : Nesterov accelerated gradient
;;;   - adam            : Adaptive moment estimation
;;;   - adamw           : Adam with decoupled weight decay
;;;   - rmsprop         : RMSprop adaptive learning rate
;;;   - adagrad         : Adagrad adaptive learning rate
;;;
;;; Second-order (uses Hessian):
;;;   - newton-method   : Newton's method with line search
;;;   - modified-newton : Newton with positive-definite Hessian enforcement
;;;   - newton-cg       : Truncated Newton using conjugate gradient
;;;   - gauss-newton    : For nonlinear least squares
;;;
;;; Quasi-Newton (approximates Hessian):
;;;   - lbfgs           : Limited-memory BFGS (default)
;;;   - lbfgs-b         : L-BFGS with box constraints
;;;
;;; Line search methods:
;;;   - armijo-backtrack    : Backtracking with Armijo condition
;;;   - wolfe-line-search   : Strong Wolfe conditions
;;;
;;; Convergence:
;;;   - make-convergence-criteria : Create custom stopping conditions
;;;   - *default-convergence*     : Default convergence criteria
;;;   - converged?                : Check if optimization converged
