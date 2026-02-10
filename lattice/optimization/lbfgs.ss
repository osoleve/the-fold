(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'reverse-diff)
(require 'convergence)
(require 'line-search)

(doc 'module 'lbfgs)
(doc 'description "Limited-memory BFGS quasi-Newton method for large-scale optimization")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'overview)
(doc 'note "L-BFGS approximates the inverse Hessian using a limited history of gradient differences.
This is memory-efficient for large problems.

Algorithm:
  - Store m most recent (s, y) pairs where:
    s = x_{k+1} - x_k
    y = grad_{k+1} - grad_k
  - Use two-loop recursion to compute H_k * g efficiently")

(doc 'section 'history-structure)

;;; L-BFGS history stores recent (s, y, rho) tuples.
;;; Structure: (lbfgs-history max-size current-size s-list y-list rho-list)

;;; make-lbfgs-history : Nat → LBFGSHistory
;;; Create empty L-BFGS history with given maximum size.
(define (make-lbfgs-history max-size)
  (list 'lbfgs-history max-size 0 '() '() '()))

;;; lbfgs-history? : α → Bool
(define (lbfgs-history? h)
  (and (pair? h) (eq? (car h) 'lbfgs-history)))

;;; Accessors
(define (lh-max-size h) (list-ref h 1))
(define (lh-size h) (list-ref h 2))
(define (lh-s-list h) (list-ref h 3))
(define (lh-y-list h) (list-ref h 4))
(define (lh-rho-list h) (list-ref h 5))

;;; lbfgs-history-push : LBFGSHistory × (List Number) × (List Number) → LBFGSHistory
;;; Add a new (s, y) pair to history.
;;; s = x_new - x_old, y = grad_new - grad_old
(define (lbfgs-history-push h s y)
  (let* ([ys (dot-product y s)]
         ;; Skip if curvature condition violated
         [_ (if (<= ys 1e-10) h #f)])
        (if (and _ (<= ys 1e-10))
            h  ; Don't add bad pairs
            (let* ([rho (/ 1.0 ys)]
                   [max-size (lh-max-size h)]
                   [old-size (lh-size h)]
                   [new-size (min (+ old-size 1) max-size)]
                   ;; Add to front, trim if necessary
                   [s-list (take-n (cons s (lh-s-list h)) max-size)]
                   [y-list (take-n (cons y (lh-y-list h)) max-size)]
                   [rho-list (take-n (cons rho (lh-rho-list h)) max-size)])
                  (list 'lbfgs-history max-size new-size s-list y-list rho-list)))))

;;; take-n : (List α) × Nat → (List α)
;;; Take first n elements of list.
(define (take-n lst n)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) (take-n (cdr lst) (- n 1)))))

;;; ====
;;; L-BFGS Two-Loop Recursion
;;; ====

;;; Compute H_k * q where H_k is the approximate inverse Hessian
;;; and q is typically the negative gradient.

;;; lbfgs-direction : LBFGSHistory × (List Number) → (List Number)
;;; Compute search direction using L-BFGS two-loop recursion.
;;; Returns H * (-gradient) for the search direction.
(define (lbfgs-direction h grad)
  (let* ([q (map - grad)]  ; q = -gradient
         [m (lh-size h)])
        (if (= m 0)
            ;; No history: use steepest descent
            q
            (lbfgs-two-loop h q))))

;;; lbfgs-two-loop : LBFGSHistory × (List Number) → (List Number)
;;; Two-loop recursion for computing H * q.
(define (lbfgs-two-loop h q)
  (let* ([s-list (lh-s-list h)]
         [y-list (lh-y-list h)]
         [rho-list (lh-rho-list h)]
         [m (lh-size h)])
        ;; First loop: compute alphas and update q
        (let loop1 ([i 0]
                    [q q]
                    [alphas '()]
                    [ss s-list]
                    [ys y-list]
                    [rhos rho-list])
             (if (or (= i m) (null? ss))
                 ;; Scale by initial Hessian approximation
                 (let* ([gamma (initial-hessian-scale h)]
                        [r (map (lambda (x) (* gamma x)) q)])
                       ;; Second loop: update r using alphas
                       (let loop2 ([j (- m 1)]
                                   [r r]
                                   [alphas-rev (reverse alphas)]
                                   [ss-rev (reverse s-list)]
                                   [ys-rev (reverse y-list)]
                                   [rhos-rev (reverse rho-list)])
                            (if (or (< j 0) (null? ss-rev))
                                r
                                (let* ([s (car ss-rev)]
                                       [y (car ys-rev)]
                                       [rho (car rhos-rev)]
                                       [alpha (car alphas-rev)]
                                       [beta (* rho (dot-product y r))]
                                       [r-new (vec-add-scaled r s (- alpha beta))])
                                      (loop2 (- j 1) r-new
                                             (cdr alphas-rev)
                                             (cdr ss-rev)
                                             (cdr ys-rev)
                                             (cdr rhos-rev))))))
                 ;; Continue first loop
                 (let* ([s (car ss)]
                        [y (car ys)]
                        [rho (car rhos)]
                        [alpha (* rho (dot-product s q))]
                        [q-new (vec-add-scaled q y (- alpha))])
                       (loop1 (+ i 1) q-new (cons alpha alphas)
                              (cdr ss) (cdr ys) (cdr rhos)))))))

;;; initial-hessian-scale : LBFGSHistory → Number
;;; Compute scaling factor for initial Hessian approximation H_0 = gamma * I.
;;; Uses gamma = (s^T y) / (y^T y) from most recent pair.
(define (initial-hessian-scale h)
  (if (= (lh-size h) 0)
      1.0
      (let* ([s (car (lh-s-list h))]
             [y (car (lh-y-list h))]
             [sy (dot-product s y)]
             [yy (dot-product y y)])
            (if (< yy 1e-15)
                1.0
                (/ sy yy)))))

;;; ====
;;; L-BFGS Optimizer
;;; ====

;;; lbfgs : ((List TracedValue) → TracedValue) × (List Number) × ConvergenceCriteria → OptResult
;;; L-BFGS optimization with default history size.
(define (lbfgs f x0 criteria)
  (doc 'export #t)
  (lbfgs-full f x0 10 criteria))

;;; lbfgs-full : ((List TracedValue) → TracedValue) × (List Number) × Nat × ConvergenceCriteria → OptResult
;;; L-BFGS optimization with configurable history size.
;;;   f: objective function
;;;   x0: initial point
;;;   m: history size (typically 3-20, default 10)
;;;   criteria: convergence criteria
(define (lbfgs-full f x0 m criteria)
  (let* ([init-grad (gradient f x0)]
         [init-f (apply f x0)]
         [init-state (initial-convergence-state init-f init-grad)]
         [init-history (make-lbfgs-history m)])
        (let loop ([x x0]
                   [grad init-grad]
                   [history init-history]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      (make-opt-result x (cs-f-val state) grad
                                       (cs-iter state) reason)
                      (let* (;; Compute search direction using L-BFGS
                             [direction (lbfgs-direction history grad)]
                             ;; Wolfe line search (required for L-BFGS convergence)
                             [ls-result (wolfe-line-search f x grad direction)]
                             [x-new (car ls-result)]
                             [alpha (cadr ls-result)]
                             ;; Compute new gradient
                             [grad-new (gradient f x-new)]
                             [f-new (apply f x-new)]
                             ;; Update history with (s, y)
                             [s (map - x-new x)]
                             [y (map - grad-new grad)]
                             [history-new (if (> alpha 0)
                                              (lbfgs-history-push history s y)
                                              history)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new grad-new history-new new-state)))))))

;;; ====
;;; Simplified Interface
;;; ====

;;; minimize : ((List TracedValue) → TracedValue) × (List Number) → OptResult
;;; Minimize function using L-BFGS with default settings.
(define (minimize f x0)
  (doc 'export #t)
  (lbfgs f x0 *default-convergence*))

;;; minimize-with : ((List TracedValue) → TracedValue) × (List Number) × Symbol → OptResult
;;; Minimize function with specified method.
;;;   method: 'sgd, 'adam, 'lbfgs, 'newton
(define (minimize-with f x0 method)
  (case method
        [(sgd) (gradient-descent f x0 0.01 *default-convergence*)]
        [(adam) (adam f x0 0.001 *default-convergence*)]
        [(lbfgs) (lbfgs f x0 *default-convergence*)]
        [(newton) (newton-method f x0 *default-convergence*)]
        [else (lbfgs f x0 *default-convergence*)]))

;;; Note: gradient-descent, adam, newton-method need to be loaded
;;; from their respective modules when used together.

;;; ====
;;; L-BFGS-B (Bound Constrained)
;;; ====

;;; L-BFGS-B handles simple box constraints: lower <= x <= upper.
;;; Uses projected gradient and modified Cauchy point.

;;; lbfgs-b : ((List TracedValue) → TracedValue) × (List Number) × (List Number) × (List Number) × ConvergenceCriteria → OptResult
;;; L-BFGS with box constraints (default history size 10).
;;;   f: objective function
;;;   x0: initial point
;;;   lower: lower bounds (use -inf.0 for unbounded)
;;;   upper: upper bounds (use +inf.0 for unbounded)
;;;   criteria: convergence criteria
(define (lbfgs-b f x0 lower upper criteria)
  (lbfgs-b-full f x0 lower upper 10 criteria))

;;; lbfgs-b-full : ((List TracedValue) → TracedValue) × (List Number) × (List Number) × (List Number) × Nat × ConvergenceCriteria → OptResult
;;; L-BFGS with box constraints and configurable history size.
;;;   f: objective function
;;;   x0: initial point
;;;   lower: lower bounds (use -inf.0 for unbounded)
;;;   upper: upper bounds (use +inf.0 for unbounded)
;;;   m: history size (typically 3-20, default 10)
;;;   criteria: convergence criteria
(define (lbfgs-b-full f x0 lower upper m criteria)
  (let* ([init-x (project-box x0 lower upper)]
         [init-grad (gradient f init-x)]
         [init-f (apply f init-x)]
         [init-state (initial-convergence-state init-f init-grad)]
         [init-history (make-lbfgs-history m)])
        (let loop ([x init-x]
                   [grad init-grad]
                   [history init-history]
                   [state init-state])
             (let ([reason (converged-bound? criteria state x grad lower upper)])
                  (if reason
                      (make-opt-result x (cs-f-val state) grad
                                       (cs-iter state) reason)
                      (let* (;; Compute projected gradient
                             [pg (projected-gradient x grad lower upper)]
                             ;; L-BFGS direction from projected gradient
                             [direction (lbfgs-direction history pg)]
                             ;; Projected line search
                             [ls-result (projected-line-search f x grad direction lower upper)]
                             [x-new (car ls-result)]
                             [grad-new (gradient f x-new)]
                             [f-new (apply f x-new)]
                             ;; Update history
                             [s (map - x-new x)]
                             [y (map - grad-new grad)]
                             [history-new (lbfgs-history-push history s y)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new grad-new history-new new-state)))))))

;;; project-box : (List Number) × (List Number) × (List Number) → (List Number)
;;; Project point onto box constraints.
(define (project-box x lower upper)
  (map (lambda (xi lo hi) (max lo (min hi xi))) x lower upper))

;;; projected-gradient : (List Number) × (List Number) × (List Number) × (List Number) → (List Number)
;;; Compute projected gradient for box constraints.
;;; At boundaries, only allow gradient pointing inward.
(define (projected-gradient x grad lower upper)
  (map (lambda (xi gi lo hi)
               (cond
                ;; At lower bound: only allow negative gradient (pointing inward)
                [(<= xi lo) (min 0 gi)]
                ;; At upper bound: only allow positive gradient (pointing inward)
                [(>= xi hi) (max 0 gi)]
                ;; Interior: full gradient
                [else gi]))
       x grad lower upper))

;;; converged-bound? : ConvergenceCriteria × ConvergenceState × (List Number) × (List Number) × (List Number) × (List Number) → (Either Bool Symbol)
;;; Check convergence for bound-constrained problem.
;;; Uses projected gradient norm instead of full gradient.
(define (converged-bound? criteria state x grad lower upper)
  (let* ([pg (projected-gradient x grad lower upper)]
         [pg-norm (vec-norm pg)])
        (cond
         [(< pg-norm (cc-grad-tol criteria)) 'grad-converged]
         [(and (> (cs-iter state) 0)
               (< (cs-f-change state) (cc-f-tol criteria)))
          'f-converged]
         [(>= (cs-iter state) (cc-max-iter criteria)) 'max-iter]
         [else #f])))

;;; projected-line-search : ((List TracedValue) → TracedValue) × (List Number) × (List Number) × (List Number) × (List Number) × (List Number) → (List Number × Number)
;;; Line search with projection onto box constraints.
(define (projected-line-search f x grad direction lower upper)
  (let* ([f-old (apply f x)]
         [grad-dot-d (dot-product grad direction)]
         [c1 1e-4]
         [rho 0.5]
         [max-iters 50])
        (let loop ([alpha 1.0] [iter 0])
             (if (>= iter max-iters)
                 (list x 0)
                 (let* ([x-trial (vec-add-scaled x direction alpha)]
                        [x-proj (project-box x-trial lower upper)]
                        [f-new (apply f x-proj)]
                        ;; Modified Armijo using actual step
                        [actual-step (map - x-proj x)]
                        [actual-alpha (if (> (vec-norm direction) 1e-10)
                                          (/ (vec-norm actual-step) (vec-norm direction))
                                          alpha)])
                       (if (or (<= f-new (+ f-old (* c1 actual-alpha grad-dot-d)))
                               (< (vec-norm actual-step) 1e-10))
                           (list x-proj actual-alpha)
                           (loop (* rho alpha) (+ iter 1))))))))
