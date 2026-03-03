(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires prelude matrix matrix-solvers reverse-diff higher-order-diff convergence line-search
(require 'prelude)
(require 'matrix)
(require 'matrix-solvers)
(require 'reverse-diff)
(require 'higher-order-diff)
(require 'convergence)
(require 'line-search)

(doc 'module 'newton)
(doc 'description "Newton and quasi-Newton methods using second-order Hessian information")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'overview)
(doc 'note "Second-order optimization methods using Hessian information.
Algorithms:
  - Pure Newton's method
  - Modified Newton (positive-definite Hessian enforcement)
  - Newton-CG (conjugate gradient for Newton direction)
  - Gauss-Newton (for least squares)")

(doc 'section 'newtons-method)

;;; Newton's method computes the search direction by solving:
;;;   H * d = -g
;;; where H is the Hessian and g is the gradient.
;;;
;;; The update is: x_new = x + alpha * d
;;; where alpha is typically 1 (pure Newton) or found via line search.

;;; newton-method : ((List TracedValue) → TracedValue) × (List Number) × ConvergenceCriteria → OptResult
;;; Newton's method with Hessian computation.
;;;   f: objective function (uses traced operations for gradient, jet ops for Hessian)
;;;   x0: initial point
;;;   criteria: convergence criteria
(define (newton-method f x0 criteria)
  (doc 'export #t)
  (newton-method-full f x0 criteria #t 1e-6))

;;; newton-method-full : ((List TracedValue) → TracedValue) × (List Number) × ConvergenceCriteria × Bool × Number → OptResult
;;; Newton's method with options.
;;;   f: objective function
;;;   x0: initial point
;;;   criteria: convergence criteria
;;;   use-line-search: whether to use line search (vs pure step)
;;;   regularization: Tikhonov regularization for Hessian
(define (newton-method-full f x0 criteria use-line-search regularization)
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
                             ;; Compute Hessian using numerical approximation
                             [hess (hessian f x)]
                             ;; Add regularization for numerical stability
                             [hess-reg (matrix-add-diagonal hess regularization)]
                             ;; Solve H * d = -g for Newton direction
                             [neg-grad (map - grad)]
                             [d-result (newton-solve-direction hess-reg neg-grad)]
                             [direction (if (eq? (car d-result) 'error)
                                            (map - grad)  ; Fall back to gradient descent
                                            d-result)]
                             ;; Line search or pure Newton step
                             [ls-result (if use-line-search
                                            (armijo-backtrack f x grad direction)
                                            (list (vec-add-scaled x direction 1.0) 1.0))]
                             [x-new (car ls-result)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new new-state)))))))

;;; newton-solve-direction : Matrix × (List Number) → (Either (List Number) Error)
;;; Solve H * d = b for Newton direction using LU decomposition.
(define (newton-solve-direction hess b)
  (let* ([n (length b)]
         [b-vec (list->vector b)])
        ;; Use matrix-solve from linalg
        (let ([result (matrix-solve hess b-vec)])
             (if (and (pair? result) (eq? (car result) 'error))
                 result  ; Return error
                 (vector->list result)))))

;;; matrix-add-diagonal : Matrix × Number → Matrix
;;; Add scalar to diagonal elements (Tikhonov regularization).
(define (matrix-add-diagonal m lambda)
  (let* ([n (matrix-rows m)]
         [data (matrix-data m)]
         [len (vector-length data)]
         ;; Create fresh mutable vector
         [new-data (make-vector len 0)])
        ;; Copy all elements
        (do ([k 0 (+ k 1)])
            ((= k len))
            (vector-set! new-data k (vector-ref data k)))
        ;; Add lambda to diagonal
        (do ([i 0 (+ i 1)])
            ((= i n) (list 'matrix n n new-data))
            (let ([idx (+ (* i n) i)])
                 (vector-set! new-data idx (+ (vector-ref new-data idx) lambda))))))

;;; ====
;;; Modified Newton (Positive Definite Hessian)
;;; ====

;;; When the Hessian is not positive definite, Newton's method can go uphill.
;;; Modified Newton adds a multiple of identity to make H positive definite.

;;; modified-newton : ((List TracedValue) → TracedValue) × (List Number) × ConvergenceCriteria → OptResult
;;; Newton's method with Hessian modification for global convergence.
(define (modified-newton f x0 criteria)
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
                             [hess (hessian f x)]
                             ;; Modify Hessian to be positive definite
                             [hess-mod (make-positive-definite hess)]
                             [neg-grad (map - grad)]
                             [d-result (newton-solve-direction hess-mod neg-grad)]
                             [direction (if (eq? (car d-result) 'error)
                                            (map - grad)
                                            d-result)]
                             ;; Line search
                             [ls-result (armijo-backtrack f x grad direction)]
                             [x-new (car ls-result)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new new-state)))))))

;;; make-positive-definite : Matrix → Matrix
;;; Add minimum amount to diagonal to make matrix positive definite.
;;; Uses Gershgorin circle theorem for estimate.
(define (make-positive-definite m)
  (let* ([n (matrix-rows m)]
         ;; Estimate minimum eigenvalue bound using Gershgorin
         [min-diag (let loop ([i 0] [min-val +inf.0])
                        (if (= i n)
                            min-val
                            (let* ([diag (matrix-ref m i i)]
                                   [off-sum (let inner ([j 0] [sum 0])
                                                 (if (= j n)
                                                     sum
                                                     (inner (+ j 1)
                                                            (if (= i j)
                                                                sum
                                                                (+ sum (abs (matrix-ref m i j)))))))]
                                   [lower-bound (- diag off-sum)])
                                  (loop (+ i 1) (min min-val lower-bound)))))]
         ;; If min eigenvalue bound is negative, add enough to diagonal
         [tau (if (< min-diag 1e-6)
                  (+ (abs min-diag) 1e-6)
                  0)])
        (if (> tau 0)
            (matrix-add-diagonal m tau)
            m)))

;;; ====
;;; Newton-CG (Truncated Newton)
;;; ====

;;; Newton-CG uses conjugate gradient to approximately solve H*d = -g.
;;; This is efficient for large problems where forming the full Hessian is expensive.

;;; newton-cg : ((List TracedValue) → TracedValue) × (List Number) × ConvergenceCriteria → OptResult
;;; Newton-CG (truncated Newton) method.
;;;   f: objective function
;;;   x0: initial point
;;;   criteria: convergence criteria
(define (newton-cg f x0 criteria)
  (doc 'export #t)
  (newton-cg-full f x0 criteria 50 1e-5))

;;; newton-cg-full : ((List TracedValue) → TracedValue) × (List Number) × ConvergenceCriteria × Nat × Number → OptResult
;;; Newton-CG with parameters.
;;;   f: objective function
;;;   x0: initial point
;;;   criteria: convergence criteria
;;;   max-cg-iters: maximum CG iterations per Newton step
;;;   cg-tol: CG convergence tolerance
(define (newton-cg-full f x0 criteria max-cg-iters cg-tol)
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
                             [n (length x)]
                             ;; Use CG to approximately solve H*d = -g
                             ;; We use Hessian-vector products instead of full Hessian
                             [direction (cg-newton-direction f x grad max-cg-iters cg-tol)]
                             ;; Line search
                             [ls-result (armijo-backtrack f x grad direction)]
                             [x-new (car ls-result)]
                             [f-new (apply f x-new)]
                             [grad-new (gradient f x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new new-state)))))))

;;; cg-newton-direction : ((List TracedValue) → TracedValue) × (List Number) × (List Number) × Nat × Number → (List Number)
;;; Compute approximate Newton direction using CG.
;;; Solves H * d = -g where H is accessed via Hessian-vector products.
(define (cg-newton-direction f x grad max-iters tol)
  (let* ([n (length x)]
         [neg-grad (map - grad)]
         ;; Hessian-vector product function
         [hvp (lambda (v) (hessian-vector-product f x v))]
         ;; Initial direction
         [d (make-list n 0)]
         [r neg-grad]  ; residual = -g - H*d = -g (initially)
         [p r]         ; search direction
         [rr (dot-product r r)])  ; r^T r
        (if (< rr (* tol tol))
            d  ; Already converged
            (let loop ([d d] [r r] [p p] [rr rr] [iter 0])
                 (if (>= iter max-iters)
                     d
                     (let* ([hp (hvp p)]                      ; H * p
                            [php (dot-product p hp)]          ; p^T H p
                            ;; Check for negative curvature
                            [_ (if (<= php 0)
                                   d  ; Return current direction
                                   #f)])
                           (if (and _ (<= php 0))
                               ;; On first iteration, d is zero - use steepest descent
                               (if (= iter 0) neg-grad d)
                               (let* ([alpha (/ rr php)]
                                      [d-new (vec-add-scaled d p alpha)]
                                      [r-new (vec-add-scaled r hp (- alpha))]
                                      [rr-new (dot-product r-new r-new)])
                                     (if (< rr-new (* tol tol))
                                         d-new
                                         (let* ([beta (/ rr-new rr)]
                                                [p-new (vec-add-scaled r-new p beta)])
                                               (loop d-new r-new p-new rr-new (+ iter 1))))))))))))

;;; hessian-vector-product : ((List TracedValue) → TracedValue) × (List Number) × (List Number) → (List Number)
;;; Compute H * v efficiently using forward-over-reverse AD.
;;; This avoids forming the full Hessian.
(define (hessian-vector-product f x v)
  ;; H*v = d/dt [gradient(f)(x + t*v)] at t=0
  ;; We use numerical differentiation for simplicity
  (let* ([eps 1e-6]
         [x+ (vec-add-scaled x v eps)]
         [x- (vec-add-scaled x v (- eps))]
         [grad+ (gradient f x+)]
         [grad- (gradient f x-)])
        (map (lambda (g+ g-) (/ (- g+ g-) (* 2 eps))) grad+ grad-)))

;;; ====
;;; Gauss-Newton Method (for Least Squares)
;;; ====

;;; For least squares: minimize 0.5 * ||r(x)||^2
;;; where r(x) is the residual vector.
;;;
;;; Gauss-Newton approximates the Hessian as J^T J (ignoring second-order terms).
;;; This is valid when residuals are small.

;;; gauss-newton : ((List Number) → (List Number)) × (List Number) × ConvergenceCriteria → OptResult
;;; Gauss-Newton method for nonlinear least squares.
;;;   r: residual function (returns vector of residuals)
;;;   x0: initial point
;;;   criteria: convergence criteria
(define (gauss-newton r x0 criteria)
  (let* ([n (length x0)]
         ;; Define objective as 0.5 * ||r(x)||^2
         [f (lambda args
                    (let ([residuals (apply r args)])
                         (* 0.5 (fold-left + 0 (map (lambda (ri) (* ri ri)) residuals)))))]
         [init-f (f x0)]
         [init-grad (gradient-ls r x0)]
         [init-state (initial-convergence-state init-f init-grad)])
        (let loop ([x x0]
                   [state init-state])
             (let ([reason (converged? criteria state)])
                  (if reason
                      (make-opt-result x (cs-f-val state) (gradient-ls r x)
                                       (cs-iter state) reason)
                      (let* ([residuals (apply r x)]
                             [jac (jacobian-numerical r x)]
                             ;; Solve J^T J d = -J^T r
                             [jtj (matrix-mul (matrix-transpose jac) jac)]
                             [jtr (matrix-vec-mul (matrix-transpose jac) (list->vector residuals))]
                             [neg-jtr (vector-map - jtr)]
                             ;; Add regularization
                             [jtj-reg (matrix-add-diagonal jtj 1e-8)]
                             [d-result (matrix-solve jtj-reg neg-jtr)]
                             [direction (if (and (pair? d-result) (eq? (car d-result) 'error))
                                            (map - (gradient-ls r x))
                                            (vector->list d-result))]
                             ;; Line search
                             [f-ls (lambda args (let ([res (apply r args)])
                                                     (* 0.5 (fold-left + 0 (map (lambda (ri) (* ri ri)) res)))))]
                             [grad (gradient-ls r x)]
                             [ls-result (armijo-backtrack f-ls x grad direction)]
                             [x-new (car ls-result)]
                             [f-new (f-ls x-new)]
                             [grad-new (gradient-ls r x-new)]
                             [new-state (update-convergence-state state f-new grad-new x x-new)])
                            (loop x-new new-state)))))))

;;; gradient-ls : ((List Number) → (List Number)) × (List Number) → (List Number)
;;; Compute gradient of 0.5*||r(x)||^2 = J^T r
(define (gradient-ls r x)
  (let* ([residuals (apply r x)]
         [jac (jacobian-numerical r x)]
         [jt (matrix-transpose jac)]
         [grad-vec (matrix-vec-mul jt (list->vector residuals))])
        (vector->list grad-vec)))

;;; jacobian-numerical : ((List Number) → (List Number)) × (List Number) → Matrix
;;; Compute Jacobian numerically.
(define (jacobian-numerical r x)
  (let* ([n (length x)]
         [r0 (apply r x)]
         [m (length r0)]
         [eps 1e-7]
         [data (make-vector (* m n) 0)])
        (do ([j 0 (+ j 1)])
            ((= j n) (matrix-from-data m n data))
            (let* ([x+ (list-update x j (lambda (v) (+ v eps)))]
                   [r+ (apply r x+)])
                  (do ([i 0 (+ i 1)])
                      ((= i m))
                      (vector-set! data (+ (* i n) j)
                                   (/ (- (list-ref r+ i) (list-ref r0 i)) eps)))))))

;;; list-update : (List α) × Nat × (α → α) → (List α)
(define (list-update-newton lst idx f)
  (if (= idx 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (list-update-newton (cdr lst) (- idx 1) f))))

;;; Use imported list-update or define if not available
(define list-update list-update-newton)

;;; ====
;;; Utility Functions
;;; ====

;;; matrix-vec-mul : Matrix × (Vector Number) → (Vector Number)
;;; Matrix-vector multiplication.
(define (matrix-vec-mul m v)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [result (make-vector rows 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (let ([sum (let inner ([j 0] [s 0])
                            (if (= j cols)
                                s
                                (inner (+ j 1)
                                       (+ s (* (matrix-ref m i j)
                                               (vector-ref v j))))))])
                 (vector-set! result i sum)))))

;;; matrix-from-data : Nat × Nat × (Vector Number) → Matrix
;;; Create matrix from dimensions and data vector.
;;; Note: Don't use make-matrix here as it conflicts with linalg's make-matrix
(define (matrix-from-data rows cols data)
  (list 'matrix rows cols data))
