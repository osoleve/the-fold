;;; core/autodiff/differentiable.ss --- Differentiable Type Class
;;;
;;; A type class hierarchy for automatic differentiation:
;;;   - Differentiable: types that support differentiation
;;;   - Dual/Traced/Hyperdual instances
;;;   - Type-safe AD operations
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - kinds.ss
;;;   - resolve.ss
;;;   - autodiff/comp-graph.ss (for dual numbers)
;;;   - autodiff/reverse-diff.ss (for traced values)

(load "core/prelude.ss")
(load "core/kinds.ss")
(load "core/infer.ss")     ; For unify, needed by resolve
(load "core/resolve.ss")
(load "core/autodiff/comp-graph.ss")
(load "core/autodiff/reverse-diff.ss")

;;; ============================================================
;;; Differentiable Type Class
;;; ============================================================

;;; class Differentiable d where
;;;   lift     : Real -> d          -- Lift a constant
;;;   primal   : d -> Real          -- Extract the primal value
;;;   tangent  : d -> Real          -- Extract the derivative/tangent
;;;   d+       : d -> d -> d         -- Addition
;;;   d*       : d -> d -> d         -- Multiplication
;;;   d-       : d -> d -> d         -- Subtraction
;;;   d/       : d -> d -> d         -- Division
;;;   d-neg    : d -> d             -- Negation
;;;   d-sq     : d -> d             -- Square
;;;   d-sqrt   : d -> d             -- Square root
;;;   d-exp    : d -> d             -- Exponential
;;;   d-log    : d -> d             -- Natural logarithm
;;;   d-sin    : d -> d             -- Sine
;;;   d-cos    : d -> d             -- Cosine
;;;
;;; This allows writing generic differentiable code:
;;;   (define (norm-sq d-instance x y)
;;;     (d+ d-instance (d-sq d-instance x) (d-sq d-instance y)))

(define TC-Differentiable
  (make-typeclass
   'Differentiable
   K*
   '()  ; No superclasses
   '((lift    . (forall (d) (=> (Differentiable d) (-> Real d))))
     (primal  . (forall (d) (=> (Differentiable d) (-> d Real))))
     (tangent . (forall (d) (=> (Differentiable d) (-> d Real))))
     (d+      . (forall (d) (=> (Differentiable d) (-> d d d))))
     (d*      . (forall (d) (=> (Differentiable d) (-> d d d))))
     (d-      . (forall (d) (=> (Differentiable d) (-> d d d))))
     (d/      . (forall (d) (=> (Differentiable d) (-> d d d))))
     (d-neg   . (forall (d) (=> (Differentiable d) (-> d d))))
     (d-sq    . (forall (d) (=> (Differentiable d) (-> d d))))
     (d-sqrt  . (forall (d) (=> (Differentiable d) (-> d d))))
     (d-exp   . (forall (d) (=> (Differentiable d) (-> d d))))
     (d-log   . (forall (d) (=> (Differentiable d) (-> d d))))
     (d-sin   . (forall (d) (=> (Differentiable d) (-> d d))))
     (d-cos   . (forall (d) (=> (Differentiable d) (-> d d)))))))

;;; ============================================================
;;; Differentiable Instance: Real (trivial - no derivatives)
;;; ============================================================

;;; Real numbers are "differentiable" but always have zero derivative.
;;; This allows mixing constants with AD types.

(define differentiable-real-methods
  `((lift    . ,(lambda (x) x))
    (primal  . ,(lambda (x) x))
    (tangent . ,(lambda (x) 0))
    (d+      . ,+)
    (d*      . ,*)
    (d-      . ,-)
    (d/      . ,/)
    (d-neg   . ,(lambda (x) (- x)))
    (d-sq    . ,(lambda (x) (* x x)))
    (d-sqrt  . ,sqrt)
    (d-exp   . ,exp)
    (d-log   . ,log)
    (d-sin   . ,sin)
    (d-cos   . ,cos)))

(define inst-Differentiable-Real
  (make-instance 'Differentiable 'Real '() differentiable-real-methods))

;;; ============================================================
;;; Differentiable Instance: Dual (forward mode)
;;; ============================================================

;;; Dual numbers carry first-order derivative information.
;;; Used for forward-mode AD (efficient for few inputs, many outputs).

(define differentiable-dual-methods
  `((lift    . ,dual-lift)
    (primal  . ,dual-value)
    (tangent . ,dual-deriv)
    (d+      . ,dual-add)
    (d*      . ,dual-mul)
    (d-      . ,dual-sub)
    (d/      . ,dual-div)
    (d-neg   . ,dual-neg)
    (d-sq    . ,dual-sq)
    (d-sqrt  . ,dual-sqrt)
    (d-exp   . ,dual-exp)
    (d-log   . ,dual-log)
    (d-sin   . ,dual-sin)
    (d-cos   . ,dual-cos)))

(define inst-Differentiable-Dual
  (make-instance 'Differentiable 'Dual '() differentiable-dual-methods))

;;; ============================================================
;;; Differentiable Instance: Traced (reverse mode)
;;; ============================================================

;;; Traced values record a computation graph for reverse-mode AD.
;;; Efficient for many inputs, few outputs (e.g., loss functions).

(define differentiable-traced-methods
  `((lift    . ,(lambda (x) (make-traced-const x)))
    (primal  . ,traced-value)
    (tangent . ,(lambda (t) 0))  ; tangent not directly available for traced
    (d+      . ,traced-add)
    (d*      . ,traced-mul)
    (d-      . ,traced-sub)
    (d/      . ,traced-div)
    (d-neg   . ,traced-neg)
    (d-sq    . ,traced-sq)
    (d-sqrt  . ,traced-sqrt)
    (d-exp   . ,traced-exp)
    (d-log   . ,traced-log)
    (d-sin   . ,traced-sin)
    (d-cos   . ,traced-cos)))

(define inst-Differentiable-Traced
  (make-instance 'Differentiable 'Traced '() differentiable-traced-methods))

;;; ============================================================
;;; Differentiable Instance: Hyperdual (exact second derivatives)
;;; ============================================================

;;; Hyperdual numbers carry first AND second derivative information.
;;; Used for exact Hessian computation without finite differences.

(define differentiable-hyperdual-methods
  `((lift    . ,hd-lift)
    (primal  . ,hd-value)
    (tangent . ,hd-deriv1)  ; Primary tangent
    (d+      . ,hd-add)
    (d*      . ,hd-mul)
    (d-      . ,hd-sub)
    (d/      . ,hd-div)
    (d-neg   . ,hd-neg)
    (d-sq    . ,hd-sq)
    (d-sqrt  . ,hd-sqrt)
    (d-exp   . ,hd-exp)
    (d-log   . ,hd-log)
    (d-sin   . ,hd-sin)
    (d-cos   . ,hd-cos)))

(define inst-Differentiable-Hyperdual
  (make-instance 'Differentiable 'Hyperdual '() differentiable-hyperdual-methods))

;;; ============================================================
;;; Extended Differentiable Database
;;; ============================================================

(define differentiable-instances
  (list inst-Differentiable-Real
        inst-Differentiable-Dual
        inst-Differentiable-Traced
        inst-Differentiable-Hyperdual))

;;; ad-instances : IDB
;;; Instance database including Differentiable class.
(define ad-instances
  (idb-add* standard-instances differentiable-instances))

;;; ============================================================
;;; Generic AD Utilities
;;; ============================================================

;;; These functions work with any Differentiable type.

;;; d-pow : Instance x d x Int -> d
;;; Power function using repeated squaring.
(define (d-pow inst x n)
  (let ([d* (get-method inst 'd*)]
        [lift (get-method inst 'lift)])
       (cond
        [(= n 0) (lift 1)]
        [(= n 1) x]
        [(even? n)
         (let ([half (d-pow inst x (/ n 2))])
              (d* half half))]
        [else
         (d* x (d-pow inst x (- n 1)))])))

;;; d-chain : Instance x (d -> d) x (d -> d) -> (d -> d)
;;; Chain two differentiable functions: (f o g).
(define (d-chain inst f g)
  (lambda (x) (f (g x))))

;;; ============================================================
;;; Convenience: Resolve Differentiable
;;; ============================================================

;;; resolve-differentiable : Type -> (Result Evidence)
;;; Resolve a Differentiable instance for a type.
(define (resolve-differentiable type)
  (resolve `(Differentiable ,type) ad-instances))

;;; get-diff-method : Type x Symbol -> Procedure | #f
;;; Get a Differentiable method for a specific type.
(define (get-diff-method type method-name)
  (let ([result (resolve-differentiable type)])
       (if (eq? (car result) 'ok)
           (get-method (cadr result) method-name)
           #f)))

;;; ============================================================
;;; Type-Safe Derivative Operations
;;; ============================================================

;;; These wrap the lower-level AD operations with type class dispatch.

;;; derivative-at : (Differentiable d) => (d -> d) x Real -> Real
;;; Compute derivative at a point using the appropriate AD type.
;;; Uses dual numbers for forward-mode differentiation.
(define (derivative-at f x)
  (let ([inst (cadr (resolve-differentiable 'Dual))])
       (dual-deriv (f (dual-variable x)))))

;;; gradient-at : (Real* -> Traced) x (List Real) -> (List Real)
;;; Compute gradient using reverse-mode AD.
;;; Function must use traced operations (traced-add, traced-mul, etc.)
(define gradient-at gradient)  ; Re-export from reverse-diff.ss

;;; hessian-at : (Real* -> Hyperdual) x (List Real) -> Matrix
;;; Compute exact Hessian using hyperdual numbers.
;;; Function must use hd-* operations.
(define hessian-at hessian-forward)  ; Re-export from comp-graph.ss

;;; ============================================================
;;; Type Class Registration
;;; ============================================================

;;; Add Differentiable to the class database.
(define extended-classes
  (cons (cons 'Differentiable TC-Differentiable) standard-classes))
