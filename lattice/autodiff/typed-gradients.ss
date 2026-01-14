;;; core/autodiff/typed-gradients.ss --- Type-Safe Gradient Dimensions
;;;
;;; Provides dimension-checked gradient operations for automatic differentiation.
;;; This module bridges the type system with autodiff to catch dimension mismatches
;;; at definition time rather than runtime.
;;;
;;; The approach is pragmatic: rather than full dependent type integration (which
;;; would require significant type checker work), we provide:
;;;   - Explicit dimension annotations on gradient types
;;;   - Runtime-checked wrappers that validate dimensions
;;;   - Clear error messages when dimensions mismatch
;;;   - Integration with the existing Differentiable type class
;;;
;;; Key types:
;;;   - (Gradient n)         : A gradient vector of exactly n components
;;;   - (GradientFn n)       : A function returning gradients of dimension n
;;;   - (HessianFn n)        : A function returning n x n Hessian matrices
;;;   - (JacobianFn m n)     : A function returning m x n Jacobian matrices
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types/dep-types.ss (for dependent type primitives)
;;;   - autodiff/reverse-diff.ss (for gradient computation)
;;;   - autodiff/higher-order-diff.ss (for Jacobian/Hessian)
;;;   - linalg/vec.ss (for vector operations)

(load "core/base/prelude.ss")
(load "core/types/dep-types.ss")
(load "lattice/autodiff/higher-order-diff.ss")

;;; ====
;;; Dimension Type Annotations
;;; ====

;;; These type constructors document gradient dimensions.
;;; They are validated at runtime by the wrapper functions below.
;;;
;;; Type: (Gradient n) = (Vec n Number)
;;;       Represents a gradient vector with exactly n components.
;;;
;;; Type: (GradientFn n) = (Vec n Number) -> (Gradient n)
;;;       A scalar function from R^n -> R, returning gradients in R^n.
;;;
;;; Type: (JacobianFn m n) = (Vec n Number) -> (Matrix m n Number)
;;;       A vector function from R^n -> R^m, returning m x n Jacobian.
;;;
;;; Type: (HessianFn n) = (Vec n Number) -> (Matrix n n Number)
;;;       A scalar function from R^n -> R, returning n x n Hessian.

;;; ====
;;; Dimension-Checked Gradient Wrapper
;;; ====

;;; A DimGradient packages a gradient with its expected dimension.
;;; Structure: (dim-gradient dimension values)

;;; dim-gradient : Nat × (List Number) → DimGradient
(define (dim-gradient expected-dim values)
  (let ([actual-dim (length values)])
       (if (= expected-dim actual-dim)
           (list 'dim-gradient expected-dim values)
           (list 'error 'dimension-mismatch
                 (list 'expected expected-dim)
                 (list 'got actual-dim)))))

;;; dim-gradient? : α → Boolean
(define (dim-gradient? x)
  (and (pair? x) (eq? (car x) 'dim-gradient)))

;;; dim-gradient-dim : DimGradient → Nat
(define (dim-gradient-dim g)
  (if (dim-gradient? g)
      (cadr g)
      0))

;;; dim-gradient-values : DimGradient → (List Number)
(define (dim-gradient-values g)
  (if (dim-gradient? g)
      (caddr g)
      '()))

;;; dim-gradient-ref : DimGradient × Nat → (Result Number Error)
(define (dim-gradient-ref g i)
  (if (not (dim-gradient? g))
      (list 'error 'not-a-gradient g)
      (let ([values (dim-gradient-values g)]
            [dim (dim-gradient-dim g)])
           (if (or (< i 0) (>= i dim))
               (list 'error 'index-out-of-bounds
                     (list 'index i)
                     (list 'dimension dim))
               (list-ref values i)))))

;;; dim-gradient-ok? : α → Boolean
(define (dim-gradient-ok? x)
  (and (dim-gradient? x)
       (not (and (pair? x) (eq? (car x) 'error)))))

;;; ====
;;; Type Class: GradientDimensioned
;;; ====

;;; This type class captures the relationship between a differentiable
;;; function and its gradient dimension.
;;;
;;; class GradientDimensioned f n | f -> n where
;;;   inputDim  : f -> Nat          -- Dimension of input space
;;;   gradientDim : f -> Nat        -- Dimension of gradient (= inputDim for scalar f)
;;;   computeGradient : f -> (Vec n Number) -> (Gradient n)
;;;
;;; The functional dependency f -> n means the function determines its gradient dimension.

;;; GradientSpec : Record type for gradient function specifications
;;; Fields:
;;;   - input-dim : Nat (dimension of input)
;;;   - output-dim : Nat (dimension of output, 1 for scalar functions)
;;;   - f-traced : (Traced ... -> Traced) (the traced function)
;;; make-gradient-spec : Nat × Nat × (Traced ... → Traced) → GradientSpec
(define (make-gradient-spec input-dim output-dim f-traced)
  (list 'gradient-spec input-dim output-dim f-traced))

;;; gradient-spec? : α → Boolean
(define (gradient-spec? x)
  (and (pair? x) (eq? (car x) 'gradient-spec)))

;;; gradient-spec-input-dim : GradientSpec → Nat
(define (gradient-spec-input-dim spec)
  (cadr spec))

;;; gradient-spec-output-dim : GradientSpec → Nat
(define (gradient-spec-output-dim spec)
  (caddr spec))

;;; gradient-spec-fn : GradientSpec → (Traced ... → Traced)
(define (gradient-spec-fn spec)
  (cadddr spec))

;;; ====
;;; Safe Gradient Computation
;;; ====

;;; These functions wrap the raw autodiff operations with dimension checking.

;;; safe-gradient : GradientSpec × (List Number) → (Result DimGradient Error)
(define (safe-gradient spec args)
  (if (not (gradient-spec? spec))
      (list 'error 'invalid-gradient-spec spec)
      (let ([expected-input-dim (gradient-spec-input-dim spec)]
            [expected-output-dim (gradient-spec-output-dim spec)]
            [f (gradient-spec-fn spec)]
            [actual-input-dim (length args)])
           (cond
            ;; Validate input dimension
            [(not (= expected-input-dim actual-input-dim))
             (list 'error 'input-dimension-mismatch
                   (list 'expected expected-input-dim)
                   (list 'got actual-input-dim))]
            ;; Validate this is a scalar function (output-dim = 1)
            [(not (= expected-output-dim 1))
             (list 'error 'not-a-scalar-function
                   (list 'output-dim expected-output-dim)
                   (list 'hint "Use safe-jacobian for vector-valued functions"))]
            ;; Compute gradient
            [else
             (let ([raw-gradient (gradient f args)])
                  (dim-gradient expected-input-dim raw-gradient))]))))

;;; safe-gradient-at : GradientSpec × Number ... → (Result DimGradient Error)
(define (safe-gradient-at spec . args)
  (safe-gradient spec args))

;;; ====
;;; Dimension-Checked Jacobian
;;; ====

;;; DimJacobian : (dim-jacobian rows cols matrix)
;;; A Jacobian matrix with explicit dimensions.
;;; dim-jacobian : Nat × Nat × Matrix → DimJacobian
(define (dim-jacobian rows cols matrix)
  (list 'dim-jacobian rows cols matrix))

;;; dim-jacobian? : α → Boolean
(define (dim-jacobian? x)
  (and (pair? x) (eq? (car x) 'dim-jacobian)))

;;; dim-jacobian-rows : DimJacobian → Nat
(define (dim-jacobian-rows j)
  (if (dim-jacobian? j) (cadr j) 0))

;;; dim-jacobian-cols : DimJacobian → Nat
(define (dim-jacobian-cols j)
  (if (dim-jacobian? j) (caddr j) 0))

;;; dim-jacobian-matrix : DimJacobian → (Option Matrix)
(define (dim-jacobian-matrix j)
  (if (dim-jacobian? j) (cadddr j) #f))

;;; safe-jacobian : GradientSpec × (List Number) → (Result DimJacobian Error)
(define (safe-jacobian spec args)
  (if (not (gradient-spec? spec))
      (list 'error 'invalid-gradient-spec spec)
      (let ([expected-input-dim (gradient-spec-input-dim spec)]
            [expected-output-dim (gradient-spec-output-dim spec)]
            [f (gradient-spec-fn spec)]
            [actual-input-dim (length args)])
           (if (not (= expected-input-dim actual-input-dim))
               (list 'error 'input-dimension-mismatch
                     (list 'expected expected-input-dim)
                     (list 'got actual-input-dim))
               (let ([raw-jacobian (jacobian f args)])
                    (if (and (pair? raw-jacobian) (eq? (car raw-jacobian) 'matrix))
                        (let ([actual-rows (matrix-rows raw-jacobian)]
                              [actual-cols (matrix-cols raw-jacobian)])
                             (if (and (= actual-rows expected-output-dim)
                                      (= actual-cols expected-input-dim))
                                 (dim-jacobian expected-output-dim expected-input-dim raw-jacobian)
                                 (list 'error 'jacobian-dimension-mismatch
                                       (list 'expected (list expected-output-dim expected-input-dim))
                                       (list 'got (list actual-rows actual-cols)))))
                        (list 'error 'jacobian-computation-failed raw-jacobian)))))))

;;; ====
;;; Dimension-Checked Hessian
;;; ====

;;; DimHessian : (dim-hessian n matrix)
;;; A Hessian matrix (always square) with explicit dimension.
;;; dim-hessian : Nat × Matrix → DimHessian
(define (dim-hessian n matrix)
  (list 'dim-hessian n matrix))

;;; dim-hessian? : α → Boolean
(define (dim-hessian? x)
  (and (pair? x) (eq? (car x) 'dim-hessian)))

;;; dim-hessian-dim : DimHessian → Nat
(define (dim-hessian-dim h)
  (if (dim-hessian? h) (cadr h) 0))

;;; dim-hessian-matrix : DimHessian → (Option Matrix)
(define (dim-hessian-matrix h)
  (if (dim-hessian? h) (caddr h) #f))

;;; safe-hessian : GradientSpec × (List Number) → (Result DimHessian Error)
(define (safe-hessian spec args)
  (if (not (gradient-spec? spec))
      (list 'error 'invalid-gradient-spec spec)
      (let ([expected-input-dim (gradient-spec-input-dim spec)]
            [expected-output-dim (gradient-spec-output-dim spec)]
            [f (gradient-spec-fn spec)]
            [actual-input-dim (length args)])
           (cond
            [(not (= expected-input-dim actual-input-dim))
             (list 'error 'input-dimension-mismatch
                   (list 'expected expected-input-dim)
                   (list 'got actual-input-dim))]
            [(not (= expected-output-dim 1))
             (list 'error 'not-a-scalar-function
                   (list 'output-dim expected-output-dim)
                   (list 'hint "Hessian is only defined for scalar functions"))]
            [else
             (let ([raw-hessian (hessian f args)])
                  (if (and (pair? raw-hessian) (eq? (car raw-hessian) 'matrix))
                      (let ([actual-rows (matrix-rows raw-hessian)]
                            [actual-cols (matrix-cols raw-hessian)])
                           (if (and (= actual-rows expected-input-dim)
                                    (= actual-cols expected-input-dim))
                               (dim-hessian expected-input-dim raw-hessian)
                               (list 'error 'hessian-dimension-mismatch
                                     (list 'expected expected-input-dim)
                                     (list 'got (list actual-rows actual-cols)))))
                      (list 'error 'hessian-computation-failed raw-hessian)))]))))

;;; ====
;;; Convenient Gradient Spec Constructors
;;; ====

;;; These macros/functions create properly typed gradient specifications.

;;; scalar-fn : Nat × (Traced ... → Traced) → GradientSpec
(define (scalar-fn n f)
  (make-gradient-spec n 1 f))

;;; vector-fn : Nat × Nat × ((List Traced) → (List Traced)) → GradientSpec
(define (vector-fn n m f)
  (make-gradient-spec n m f))

;;; ====
;;; Type Annotations for Documentation
;;; ====

;;; These functions generate type annotations for gradient-related types.
;;; They integrate with the dependent type system in core/types/dep-types.ss.

;;; t-gradient : Nat → Type
(define (t-gradient n)
  `(Vec ,n Number))

;;; t-gradient-fn : Nat → Type
(define (t-gradient-fn n)
  `(-> (Vec ,n Number) (Vec ,n Number)))

;;; t-jacobian : Nat × Nat → Type
(define (t-jacobian m n)
  `(Matrix ,m ,n Number))

;;; t-jacobian-fn : Nat × Nat → Type
(define (t-jacobian-fn m n)
  `(-> (Vec ,n Number) (Matrix ,m ,n Number)))

;;; t-hessian : Nat → Type
(define (t-hessian n)
  `(Matrix ,n ,n Number))

;;; t-hessian-fn : Nat → Type
(define (t-hessian-fn n)
  `(-> (Vec ,n Number) (Matrix ,n ,n Number)))

;;; ====
;;; Dimension Inference Utilities
;;; ====

;;; These utilities help determine dimensions from function signatures.

;;; infer-gradient-dim : ((List Traced) → Traced) × Nat → (Result Nat Error)
(define (infer-gradient-dim f expected-input-dim)
  ;; Create test inputs
  (let* ([test-args (map (lambda (i) (+ i 1.0)) (iota expected-input-dim))]
         [test-gradient (gradient f test-args)])
        (if (list? test-gradient)
            (length test-gradient)
            (list 'error 'gradient-inference-failed test-gradient))))

;;; validate-gradient-spec : GradientSpec × (List Number) → Boolean
(define (validate-gradient-spec spec test-args)
  (if (not (gradient-spec? spec))
      (list 'error 'invalid-spec)
      (let ([result (safe-gradient spec test-args)])
           (dim-gradient-ok? result))))

;;; ====
;;; Error Reporting
;;; ====

;;; format-gradient-error : Error → String
(define (format-gradient-error err)
  (if (and (pair? err) (eq? (car err) 'error))
      (let ([kind (cadr err)])
           (case kind
                 [(dimension-mismatch)
                  (let ([expected (caddr err)]
                        [got (cadddr err)])
                       (format "Dimension mismatch: expected ~a components, got ~a"
                               (cadr expected) (cadr got)))]
                 [(input-dimension-mismatch)
                  (let ([expected (caddr err)]
                        [got (cadddr err)])
                       (format "Input dimension mismatch: function expects ~a arguments, got ~a"
                               (cadr expected) (cadr got)))]
                 [(not-a-scalar-function)
                  (format "Not a scalar function: output dimension is ~a (expected 1)"
                          (cadr (caddr err)))]
                 [(index-out-of-bounds)
                  (let ([index (caddr err)]
                        [dim (cadddr err)])
                       (format "Index ~a out of bounds for gradient of dimension ~a"
                               (cadr index) (cadr dim)))]
                 [else (format "Gradient error: ~s" err)]))
      (format "Unknown error: ~s" err)))

;;; ====
;;; Differentiable Type Class Integration
;;; ====

;;; DimDifferentiable extends the Differentiable type class with dimension
;;; information. This allows type-safe composition of differentiable functions.
;;;
;;; class DimDifferentiable d n | d -> n where
;;;   dim : d -> Nat
;;;   gradientOf : d -> (Vec n Number) -> DimGradient
;;;
;;; Instance: (DimDifferentiable (GradientSpec n 1) n)

;;; TC-DimDifferentiable : TypeClass
(define TC-DimDifferentiable
  `(typeclass DimDifferentiable
    (kind (* Nat) Constraint)
    (supers ())
    (methods
     ((dim . (forall (d n) (=> (DimDifferentiable d n) (-> d Nat))))
      (gradientOf . (forall (d n)
                            (=> (DimDifferentiable d n)
                                (-> d (-> (Vec n Number) (DimGradient n))))))))))

;;; dim-differentiable-gradient-spec-methods : (AList Symbol Procedure)
(define dim-differentiable-gradient-spec-methods
  `((dim . ,gradient-spec-input-dim)
    (gradientOf . ,(lambda (spec) (lambda (args) (safe-gradient spec args))))))

;;; ====
;;; Composition with Dimension Checking
;;; ====

;;; These functions compose differentiable functions while checking dimensions.

;;; compose-gradient-fns : GradientSpec × GradientSpec → (Result GradientSpec Error)
(define (compose-gradient-fns f-spec g-spec)
  (if (not (and (gradient-spec? f-spec) (gradient-spec? g-spec)))
      (list 'error 'invalid-specs)
      (let ([f-input (gradient-spec-input-dim f-spec)]
            [g-output (gradient-spec-output-dim g-spec)]
            [g-input (gradient-spec-input-dim g-spec)])
           (if (not (= f-input g-output))
               (list 'error 'composition-dimension-mismatch
                     (list 'f-expects f-input)
                     (list 'g-produces g-output))
               ;; Compose the traced functions
               (let ([f (gradient-spec-fn f-spec)]
                     [g (gradient-spec-fn g-spec)])
                    (make-gradient-spec
                     g-input 1
                     (lambda traced-args
                             ;; If g is a vector function, spread its result as args to f
                             (let ([g-result (apply g traced-args)])
                                  (if (list? g-result)
                                      (apply f g-result)
                                      (f g-result))))))))))

;;; ====
;;; Gradient Vector Operations
;;; ====

;;; These operations work on DimGradients with dimension checking.

;;; dim-gradient-add : DimGradient × DimGradient → (Result DimGradient Error)
(define (dim-gradient-add g1 g2)
  (if (not (and (dim-gradient? g1) (dim-gradient? g2)))
      (list 'error 'invalid-gradients)
      (let ([d1 (dim-gradient-dim g1)]
            [d2 (dim-gradient-dim g2)])
           (if (not (= d1 d2))
               (list 'error 'gradient-dimension-mismatch
                     (list 'g1 d1) (list 'g2 d2))
               (dim-gradient d1
                             (map + (dim-gradient-values g1)
                                  (dim-gradient-values g2)))))))

;;; dim-gradient-scale : Number × DimGradient → DimGradient
(define (dim-gradient-scale c g)
  (if (not (dim-gradient? g))
      (list 'error 'invalid-gradient g)
      (dim-gradient (dim-gradient-dim g)
                    (map (lambda (x) (* c x)) (dim-gradient-values g)))))

;;; dim-gradient-dot : DimGradient × DimGradient → (Result Number Error)
(define (dim-gradient-dot g1 g2)
  (if (not (and (dim-gradient? g1) (dim-gradient? g2)))
      (list 'error 'invalid-gradients)
      (let ([d1 (dim-gradient-dim g1)]
            [d2 (dim-gradient-dim g2)])
           (if (not (= d1 d2))
               (list 'error 'gradient-dimension-mismatch
                     (list 'g1 d1) (list 'g2 d2))
               (apply + (map * (dim-gradient-values g1)
                             (dim-gradient-values g2)))))))

;;; dim-gradient-norm : DimGradient → (Result Number Error)
(define (dim-gradient-norm g)
  (if (not (dim-gradient? g))
      (list 'error 'invalid-gradient g)
      (let ([dot-result (dim-gradient-dot g g)])
           (if (and (pair? dot-result) (eq? (car dot-result) 'error))
               dot-result  ; Propagate error from dot product
               (sqrt dot-result)))))

;;; ====
;;; Helper: Matrix Accessors (if not already loaded)
;;; ====

;;; matrix-rows : Matrix → Nat
(define (matrix-rows m)
  (if (and (pair? m) (eq? (car m) 'matrix))
      (cadr m)
      0))

;;; matrix-cols : Matrix → Nat
(define (matrix-cols m)
  (if (and (pair? m) (eq? (car m) 'matrix))
      (caddr m)
      0))

;;; ====
;;; Summary of Exported Bindings
;;; ====

;;; Type annotations:
;;;   t-gradient, t-gradient-fn, t-jacobian, t-jacobian-fn, t-hessian, t-hessian-fn
;;;
;;; Gradient spec constructors:
;;;   make-gradient-spec, gradient-spec?, scalar-fn, vector-fn
;;;
;;; Dimension-checked operations:
;;;   safe-gradient, safe-gradient-at, safe-jacobian, safe-hessian
;;;
;;; Dimension-checked containers:
;;;   dim-gradient, dim-gradient?, dim-gradient-dim, dim-gradient-values, dim-gradient-ref
;;;   dim-jacobian, dim-jacobian?, dim-jacobian-rows, dim-jacobian-cols, dim-jacobian-matrix
;;;   dim-hessian, dim-hessian?, dim-hessian-dim, dim-hessian-matrix
;;;
;;; Gradient operations:
;;;   dim-gradient-add, dim-gradient-scale, dim-gradient-dot, dim-gradient-norm
;;;
;;; Composition:
;;;   compose-gradient-fns
;;;
;;; Utilities:
;;;   validate-gradient-spec, infer-gradient-dim, format-gradient-error
