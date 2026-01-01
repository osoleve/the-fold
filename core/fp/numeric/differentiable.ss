;;; fabric/stitches/fp/differentiable.ss -- Differentiable Type Class
;;;
;;; Provides a type class interface for automatic differentiation.
;;; Enables generic differentiation across numeric types.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Type Class:
;;;   Differentiable a where
;;;     grad     : (a -> a) -> a -> a           -- Gradient at a point
;;;     jacobian : (a -> b) -> a -> Matrix      -- Jacobian matrix
;;;     hessian  : (a -> a) -> a -> Matrix      -- Hessian matrix
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/numeric.ss
;;;   - reverse-diff.ss (for implementation)

(load "core/prelude.ss")
(load "core/fp/numeric/numeric.ss")
(load "core/reverse-diff.ss")

;;; ============================================================
;;; Differentiable Type Class
;;; ============================================================
;;;
;;; The Differentiable type class abstracts over types that support
;;; automatic differentiation operations.

;;; make-differentiable : ... -> Differentiable a
;;; Create a Differentiable instance.
;;;   num-inst    : Num a instance (for arithmetic)
;;;   grad-fn     : (a -> a) -> a -> a (compute gradient)
;;;   jacobian-fn : (a -> b) -> a -> Matrix (compute Jacobian)
;;;   hessian-fn  : (a -> a) -> a -> Matrix (compute Hessian)
(define (make-differentiable num-inst grad-fn jacobian-fn hessian-fn)
  (vector 'differentiable num-inst grad-fn jacobian-fn hessian-fn))

;;; differentiable? : Any -> Boolean
(define (differentiable? x)
  (and (vector? x)
       (= (vector-length x) 5)
       (eq? (vector-ref x 0) 'differentiable)))

;;; Differentiable accessors
(define (diff-num d) (vector-ref d 1))
(define (diff-grad d) (vector-ref d 2))
(define (diff-jacobian d) (vector-ref d 3))
(define (diff-hessian d) (vector-ref d 4))

;;; ============================================================
;;; Generic Differentiable Operations
;;; ============================================================

;;; grad : Differentiable a => (a -> a) -> a -> a
;;; Compute the gradient of f at x.
(define (grad inst f x)
  ((diff-grad inst) f x))

;;; jacobian : Differentiable a => (a -> b) -> a -> Matrix
;;; Compute the Jacobian matrix of f at x.
(define (jacobian inst f x)
  ((diff-jacobian inst) f x))

;;; hessian : Differentiable a => (a -> a) -> a -> Matrix
;;; Compute the Hessian matrix of f at x.
(define (hessian inst f x)
  ((diff-hessian inst) f x))

;;; ============================================================
;;; Derived Operations
;;; ============================================================

;;; directional-derivative : Differentiable a => (a -> a) -> a -> a -> a
;;; Compute directional derivative of f at x in direction v.
;;; D_v f(x) = grad(f)(x) · v
(define (directional-derivative inst f x v)
  (let ([g (grad inst f x)])
       (if (number? g)
           (* g v)  ; Scalar case
           (fold-left + 0 (map * g v)))))  ; Vector case (dot product)

;;; gradient-descent-step : Differentiable a => Num a => (a -> a) -> a -> a -> a
;;; Take one gradient descent step: x' = x - learning_rate * grad(f)(x)
(define (gradient-descent-step inst learning-rate f x)
  (let* ([num (diff-num inst)]
         [g (grad inst f x)])
        (if (number? g)
            (num- num x (num* num learning-rate g))
            (map (lambda (xi gi) (num- num xi (num* num learning-rate gi)))
                 x g))))

;;; gradient-descent : Differentiable a => (a -> a) -> a -> a -> Nat -> a
;;; Run gradient descent for n iterations.
(define (gradient-descent inst learning-rate f x0 n)
  (let loop ([x x0] [i 0])
       (if (>= i n)
           x
           (loop (gradient-descent-step inst learning-rate f x) (+ i 1)))))

;;; numerical-gradient : Floating a => (a -> a) -> a -> a -> a
;;; Compute gradient numerically using finite differences (for verification).
(define (numerical-gradient float-inst f x epsilon)
  (let* ([frac (floating-frac float-inst)]
         [two (from-int (fractional-num frac) 2)])
        (frac/ frac
               (num- (fractional-num frac)
                     (f (num+ (fractional-num frac) x epsilon))
                     (f (num- (fractional-num frac) x epsilon)))
               (num* (fractional-num frac) two epsilon))))

;;; check-gradient : Differentiable a => Floating a => (a -> a) -> a -> a -> Boolean
;;; Check if automatic gradient matches numerical gradient within tolerance.
(define (check-gradient diff-inst float-inst f x epsilon tolerance)
  (let ([auto-grad (grad diff-inst f x)]
        [num-grad (numerical-gradient float-inst f x epsilon)])
       (< (abs (- auto-grad num-grad)) tolerance)))

;;; ============================================================
;;; Scalar Instance (Real numbers)
;;; ============================================================

;;; real-differentiable : Differentiable Real
;;; Differentiable instance for scalar real numbers.
(define real-differentiable
  (make-differentiable
   scheme-num
   ;; grad: use reverse-mode AD
   (lambda (f x)
           (reverse-diff f x))
   ;; jacobian: for scalars, this is just the derivative
   (lambda (f x)
           (list (list (reverse-diff f x))))
   ;; hessian: second derivative (differentiate the derivative)
   ;; For scalar functions, this is f''(x)
   (lambda (f x)
           ;; Compute second derivative using nested differentiation
           ;; Note: This requires f to be twice differentiable
           (let ([df (lambda (y) (reverse-diff f y))])
                (list (list (reverse-diff df x)))))))

;;; flonum-differentiable : Differentiable Flonum
;;; Differentiable instance for floating-point numbers.
(define flonum-differentiable
  (make-differentiable
   flonum-num
   (lambda (f x) (reverse-diff f x))
   (lambda (f x) (list (list (reverse-diff f x))))
   (lambda (f x)
           (let ([df (lambda (y) (reverse-diff f y))])
                (list (list (reverse-diff df x)))))))

;;; ============================================================
;;; Vector Instance
;;; ============================================================

;;; make-vector-differentiable : Differentiable a -> Differentiable (Vec a)
;;; Lift a scalar Differentiable instance to vectors.
;;; Note: Functions must take a LIST of traced values, not individual args.
(define (make-vector-differentiable scalar-diff)
  (make-differentiable
   (diff-num scalar-diff)
   ;; grad: compute gradient for each component
   ;; We wrap f to work with gradient's calling convention
   (lambda (f xs)
           (gradient (lambda traced-args (f traced-args)) xs))
   ;; jacobian: matrix of partial derivatives
   (lambda (f xs)
           (let* ([n (length xs)]
                  [base-output (f xs)]
                  [m (if (list? base-output) (length base-output) 1)])
                 ;; Compute each row of the Jacobian
                 (if (= m 1)
                     ;; Scalar output: Jacobian is a row vector
                     (list (gradient (lambda traced-args (f traced-args)) xs))
                     ;; Vector output: Jacobian is m x n matrix
                     (map (lambda (i)
                                  (gradient (lambda traced-args
                                                    (list-ref (f traced-args) i))
                                            xs))
                          (iota m)))))
   ;; hessian: compute Hessian matrix for scalar-valued functions
   (lambda (f xs)
           (let ([n (length xs)])
                ;; Hessian[i,j] = ∂²f/∂xi∂xj
                (map (lambda (i)
                             (gradient (lambda traced-args
                                               (list-ref (gradient (lambda inner-args (f inner-args)) traced-args) i))
                                       xs))
                     (iota n))))))

;;; vector-differentiable : Differentiable (Vec Real)
(define vector-differentiable
  (make-vector-differentiable real-differentiable))

;;; ============================================================
;;; Helper: iota
;;; ============================================================

;;; iota : Nat -> (List Nat)
;;; Generate list [0, 1, ..., n-1]
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; Automatic Lifting
;;; ============================================================

;;; These functions work with the traced operations from reverse-diff.ss
;;; to enable differentiation through complex expressions.

;;; diff-compose : (b -> c) -> (a -> b) -> (a -> c)
;;; Compose differentiable functions (chain rule handled automatically).
(define (diff-compose g f)
  (lambda (x) (g (f x))))

;;; diff-sum : (List (a -> a)) -> (a -> a)
;;; Sum of differentiable functions.
(define (diff-sum fs)
  (lambda (x)
          (fold-left traced-add 0 (map (lambda (f) (f x)) fs))))

;;; diff-product : (List (a -> a)) -> (a -> a)
;;; Product of differentiable functions.
(define (diff-product fs)
  (lambda (x)
          (fold-left traced-mul 1 (map (lambda (f) (f x)) fs))))

;;; ============================================================
;;; Common Differentiable Functions
;;; ============================================================

;;; These return traced operations for use in differentiable contexts.

;;; d-square : Traced -> Traced
(define d-square traced-sq)

;;; d-sqrt : Traced -> Traced
(define d-sqrt traced-sqrt)

;;; d-exp : Traced -> Traced
(define d-exp traced-exp)

;;; d-log : Traced -> Traced
(define d-log traced-log)

;;; d-sin : Traced -> Traced
(define d-sin traced-sin)

;;; d-cos : Traced -> Traced
(define d-cos traced-cos)

;;; d-tan : Traced -> Traced
(define d-tan traced-tan)

;;; d-add : Traced -> Traced -> Traced
(define d-add traced-add)

;;; d-sub : Traced -> Traced -> Traced
(define d-sub traced-sub)

;;; d-mul : Traced -> Traced -> Traced
(define d-mul traced-mul)

;;; d-div : Traced -> Traced -> Traced
(define d-div traced-div)

;;; d-neg : Traced -> Traced
(define d-neg traced-neg)

;;; ============================================================
;;; Loss Functions (Common in Optimization)
;;; ============================================================

;;; These are pre-defined differentiable loss functions.

;;; mse-loss : (List Number) -> (List Number) -> Number
;;; Mean squared error loss (differentiable).
(define (mse-loss predictions targets)
  (let* ([n (length predictions)]
         [diffs (map traced-sub predictions targets)]
         [squared (map traced-sq diffs)]
         [sum (fold-left traced-add 0 squared)])
        (traced-div sum n)))

;;; l2-loss : (List Number) -> Number
;;; L2 regularization loss (differentiable).
(define (l2-loss weights)
  (fold-left traced-add 0 (map traced-sq weights)))

;;; cross-entropy-loss : Number -> Number -> Number
;;; Binary cross-entropy loss (differentiable).
;;; -[y*log(p) + (1-y)*log(1-p)]
(define (cross-entropy-loss prediction target)
  (traced-neg
   (traced-add
    (traced-mul target (traced-log prediction))
    (traced-mul (traced-sub 1 target)
                (traced-log (traced-sub 1 prediction))))))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Type Class:
;;;   make-differentiable, differentiable?
;;;   diff-num, diff-grad, diff-jacobian, diff-hessian
;;;
;;; Generic Operations:
;;;   grad, jacobian, hessian
;;;   directional-derivative, gradient-descent-step, gradient-descent
;;;   numerical-gradient, check-gradient
;;;
;;; Instances:
;;;   real-differentiable, flonum-differentiable, vector-differentiable
;;;   make-vector-differentiable
;;;
;;; Differentiable Primitives:
;;;   d-square, d-sqrt, d-exp, d-log
;;;   d-sin, d-cos, d-tan
;;;   d-add, d-sub, d-mul, d-div, d-neg
;;;
;;; Loss Functions:
;;;   mse-loss, l2-loss, cross-entropy-loss
;;;
;;; Utilities:
;;;   diff-compose, diff-sum, diff-product, iota
