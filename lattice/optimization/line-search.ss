;;; lattice/optimization/line-search.ss --- Line Search Strategies
;;;
;;; Implements line search methods for finding step sizes in optimization.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Methods:
;;;   - Armijo (backtracking) line search
;;;   - Wolfe conditions (strong and weak)
;;;   - Exact line search for quadratics
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - autodiff/reverse-diff.ss

(load "core/base/prelude.ss")
(load "lattice/autodiff/reverse-diff.ss")

;;; ============================================================
;;; Line Search Parameters
;;; ============================================================

;;; Default parameters for line search
(define *armijo-c1* 1e-4)      ; Sufficient decrease constant
(define *wolfe-c2* 0.9)        ; Curvature condition constant
(define *backtrack-rho* 0.5)   ; Step size reduction factor
(define *max-ls-iters* 50)     ; Maximum line search iterations
(define *initial-step* 1.0)    ; Initial step size

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; dot-product : (List Number) × (List Number) → Number
;;; Compute dot product of two list-vectors.
(define (dot-product v1 v2)
  (fold-left + 0 (map * v1 v2)))

;;; vec-add-scaled : (List Number) × (List Number) × Number → (List Number)
;;; Compute v1 + alpha * v2.
(define (vec-add-scaled v1 v2 alpha)
  (map (lambda (a b) (+ a (* alpha b))) v1 v2))

;;; ============================================================
;;; Armijo Backtracking Line Search
;;; ============================================================

;;; The Armijo condition (sufficient decrease):
;;;   f(x + alpha * d) <= f(x) + c1 * alpha * grad(f)^T * d
;;;
;;; Where:
;;;   - x is current point
;;;   - d is search direction (usually -gradient for descent)
;;;   - alpha is step size
;;;   - c1 is a small constant (typically 1e-4)

;;; armijo-condition? : Number × Number × Number × Number × Number → Bool
;;; Check if Armijo sufficient decrease condition is satisfied.
;;;   f-new: f(x + alpha * d)
;;;   f-old: f(x)
;;;   alpha: step size
;;;   grad-dot-d: gradient(f)^T * d
;;;   c1: sufficient decrease constant
(define (armijo-condition? f-new f-old alpha grad-dot-d c1)
  (<= f-new (+ f-old (* c1 alpha grad-dot-d))))

;;; armijo-backtrack : ((List Number) → Number) × (List Number) × (List Number) × (List Number) → (List Number × Number)
;;; Backtracking line search with Armijo condition.
;;;   f: objective function
;;;   x: current point
;;;   grad: gradient at x
;;;   direction: search direction
;;; Returns: (new-x step-size)
(define (armijo-backtrack f x grad direction)
  (armijo-backtrack-full f x grad direction *initial-step* *armijo-c1* *backtrack-rho* *max-ls-iters*))

;;; armijo-backtrack-full : ((List Number) → Number) × (List Number) × (List Number) × (List Number) × Number × Number × Number × Nat → (List Number × Number)
;;; Full backtracking line search with all parameters.
(define (armijo-backtrack-full f x grad direction alpha c1 rho max-iters)
  (let* ([f-old (apply f x)]
         [grad-dot-d (dot-product grad direction)])
        ;; If direction is not a descent direction, return 0 step
        (if (>= grad-dot-d 0)
            (list x 0)
            (let loop ([alpha alpha] [iter 0])
                 (if (>= iter max-iters)
                     ;; Max iterations reached, return current best
                     (list (vec-add-scaled x direction alpha) alpha)
                     (let* ([x-new (vec-add-scaled x direction alpha)]
                            [f-new (apply f x-new)])
                           (if (armijo-condition? f-new f-old alpha grad-dot-d c1)
                               ;; Armijo satisfied
                               (list x-new alpha)
                               ;; Backtrack
                               (loop (* rho alpha) (+ iter 1)))))))))

;;; ============================================================
;;; Wolfe Conditions Line Search
;;; ============================================================

;;; The Wolfe conditions require both:
;;; 1. Armijo (sufficient decrease): f(x + alpha*d) <= f(x) + c1*alpha*grad^T*d
;;; 2. Curvature condition: grad(f(x + alpha*d))^T * d >= c2 * grad(f(x))^T * d
;;;
;;; Strong Wolfe additionally requires |grad_new^T * d| <= c2 * |grad_old^T * d|

;;; curvature-condition? : Number × Number × Number → Bool
;;; Check weak Wolfe curvature condition.
;;;   grad-new-dot-d: gradient at new point dotted with direction
;;;   grad-old-dot-d: gradient at old point dotted with direction
;;;   c2: curvature constant
(define (curvature-condition? grad-new-dot-d grad-old-dot-d c2)
  (>= grad-new-dot-d (* c2 grad-old-dot-d)))

;;; strong-curvature-condition? : Number × Number × Number → Bool
;;; Check strong Wolfe curvature condition.
(define (strong-curvature-condition? grad-new-dot-d grad-old-dot-d c2)
  (<= (abs grad-new-dot-d) (* c2 (abs grad-old-dot-d))))

;;; wolfe-line-search : ((List TracedValue) → TracedValue) × (List Number) × (List Number) × (List Number) → (List Number × Number)
;;; Line search satisfying Wolfe conditions using bracketing and zoom.
;;;   f: objective function (uses traced operations)
;;;   x: current point
;;;   grad: gradient at x
;;;   direction: search direction
;;; Returns: (new-x step-size)
(define (wolfe-line-search f x grad direction)
  (wolfe-line-search-full f x grad direction *initial-step* *armijo-c1* *wolfe-c2* *max-ls-iters*))

;;; wolfe-line-search-full : ((List TracedValue) → TracedValue) × (List Number) × (List Number) × (List Number) × Number × Number × Number × Nat → (List Number × Number)
;;; Full Wolfe line search with all parameters.
(define (wolfe-line-search-full f x grad direction alpha-init c1 c2 max-iters)
  (let* ([f-old (apply f (map (lambda (v) v) x))]  ; f at current point
         [grad-dot-d (dot-product grad direction)])
        ;; If not a descent direction, return 0 step
        (if (>= grad-dot-d 0)
            (list x 0)
            ;; Bracketing phase: find interval [alpha-lo, alpha-hi]
            (wolfe-bracket f x grad direction f-old grad-dot-d alpha-init c1 c2 max-iters))))

;;; wolfe-bracket : ((List TracedValue) → TracedValue) × ... → (List Number × Number)
;;; Bracketing phase of Wolfe line search.
(define (wolfe-bracket f x grad direction f-old grad-dot-d alpha-init c1 c2 max-iters)
  (let* ([alpha-max 10.0])  ; Maximum step size
        (let loop ([alpha-prev 0]
                   [f-prev f-old]
                   [alpha alpha-init]
                   [iter 0])
             (if (>= iter max-iters)
                 ;; Return best found
                 (list (vec-add-scaled x direction alpha) alpha)
                 (let* ([x-new (vec-add-scaled x direction alpha)]
                        [f-new (apply f x-new)])
                       (cond
                        ;; Armijo violated or function increased
                        [(or (> f-new (+ f-old (* c1 alpha grad-dot-d)))
                             (and (> iter 0) (>= f-new f-prev)))
                         ;; Zoom between alpha-prev and alpha
                         (wolfe-zoom f x grad direction f-old grad-dot-d
                                     alpha-prev alpha c1 c2 (- max-iters iter))]
                        ;; Check curvature condition
                        [else
                         (let* ([grad-new (gradient f x-new)]
                                [grad-new-dot-d (dot-product grad-new direction)])
                               (cond
                                ;; Strong Wolfe satisfied
                                [(strong-curvature-condition? grad-new-dot-d grad-dot-d c2)
                                 (list x-new alpha)]
                                ;; Positive slope means we've passed the minimum
                                [(>= grad-new-dot-d 0)
                                 (wolfe-zoom f x grad direction f-old grad-dot-d
                                             alpha alpha-prev c1 c2 (- max-iters iter))]
                                ;; Keep expanding
                                [else
                                 (loop alpha f-new (min (* 2 alpha) alpha-max) (+ iter 1))]))]))))))

;;; wolfe-zoom : ((List TracedValue) → TracedValue) × ... → (List Number × Number)
;;; Zoom phase: bisection search in bracket [alpha-lo, alpha-hi].
(define (wolfe-zoom f x grad direction f-old grad-dot-d alpha-lo alpha-hi c1 c2 max-iters)
  (let loop ([lo alpha-lo]
             [hi alpha-hi]
             [iter 0])
       (if (or (>= iter max-iters)
               (< (abs (- hi lo)) 1e-10))
           ;; Return midpoint
           (let* ([alpha (/ (+ lo hi) 2)]
                  [x-new (vec-add-scaled x direction alpha)])
                 (list x-new alpha))
           ;; Bisect
           (let* ([alpha (/ (+ lo hi) 2)]
                  [x-new (vec-add-scaled x direction alpha)]
                  [f-new (apply f x-new)]
                  [f-lo (apply f (vec-add-scaled x direction lo))])
                 (cond
                  ;; Armijo violated or f >= f(lo)
                  [(or (> f-new (+ f-old (* c1 alpha grad-dot-d)))
                       (>= f-new f-lo))
                   (loop lo alpha (+ iter 1))]
                  ;; Check curvature
                  [else
                   (let* ([grad-new (gradient f x-new)]
                          [grad-new-dot-d (dot-product grad-new direction)])
                         (cond
                          ;; Strong Wolfe satisfied
                          [(strong-curvature-condition? grad-new-dot-d grad-dot-d c2)
                           (list x-new alpha)]
                          ;; Adjust bracket
                          [(>= (* grad-new-dot-d (- hi lo)) 0)
                           (loop alpha lo (+ iter 1))]
                          [else
                           (loop alpha hi (+ iter 1))]))])))))

;;; ============================================================
;;; Exact Line Search (for Quadratics)
;;; ============================================================

;;; For quadratic functions f(x) = 0.5 * x^T A x - b^T x + c,
;;; the exact step size along direction d from point x is:
;;;   alpha = -(grad^T d) / (d^T A d)
;;;
;;; This requires knowing the Hessian (matrix A).

;;; exact-quadratic-step : (List Number) × (List Number) × Matrix → Number
;;; Compute exact step size for quadratic objective.
;;;   grad: gradient at current point
;;;   direction: search direction
;;;   hessian: Hessian matrix
(define (exact-quadratic-step grad direction hessian)
  (let* ([grad-dot-d (dot-product grad direction)]
         [d-vec (list->vector direction)]
         [n (length direction)]
         ;; Compute direction^T * H * direction
         [hd (let loop ([i 0] [result '()])
                  (if (= i n)
                      (reverse result)
                      (let ([row-sum (let inner ([j 0] [sum 0])
                                          (if (= j n)
                                              sum
                                              (inner (+ j 1)
                                                     (+ sum (* (matrix-ref hessian i j)
                                                               (vector-ref d-vec j))))))])
                           (loop (+ i 1) (cons row-sum result)))))]
         [d-hessian-d (dot-product direction hd)])
        ;; Avoid division by zero
        (if (< (abs d-hessian-d) 1e-15)
            1.0  ; Default step size
            (/ (- grad-dot-d) d-hessian-d))))

;;; ============================================================
;;; Adaptive Initial Step Size
;;; ============================================================

;;; For Newton and quasi-Newton methods, a step of 1.0 is often appropriate.
;;; For gradient descent, the initial step should be scaled.

;;; initial-step-size : (List Number) × Number → Number
;;; Compute a reasonable initial step size.
;;;   grad: gradient
;;;   f-val: function value
(define (initial-step-size grad f-val)
  (let ([grad-norm (sqrt (fold-left + 0 (map (lambda (x) (* x x)) grad)))])
       (if (< grad-norm 1e-10)
           1.0
           (min 1.0 (/ (abs f-val) (+ grad-norm 1e-10))))))

;;; ============================================================
;;; Simple Constant Step
;;; ============================================================

;;; constant-step : Number → ((List Number) → Number) × (List Number) × (List Number) × (List Number) → (List Number × Number)
;;; Line search that always returns the same step size.
(define (constant-step alpha)
  (lambda (f x grad direction)
          (list (vec-add-scaled x direction alpha) alpha)))
