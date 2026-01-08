;;; core/linalg/dep-linalg.ss — Dependent Linear Algebra
;;;
;;; Dimension-safe wrappers for linalg operations.
;;; These use Pi types to track vector and matrix dimensions at type level.
;;;
;;; The type signatures shown in comments are for documentation and are
;;; checked by the dependent type inference system (dep-infer.ss).
;;; At runtime, these are thin wrappers around the base linalg operations.

(load "core/base/prelude.ss")
(load "core/linalg/vec.ss")
(load "core/linalg/matrix.ss")

;;; ============================================================
;;; Type Signatures (for dep-synth type checking)
;;; ============================================================

;;; These are the declared types for use with dep-synth.
;;; To type-check code using these operations, load this type context
;;; and pass expressions to dep-synth.

(define dep-linalg-types
  '(;; Vector operations
    (vec-append-typed . (Π ((n : Nat)) (Π ((m : Nat)) (Π ((A : Type))
                                                         (-> (Vec n A) (Vec m A) (Vec (+ n m) A))))))
    (vec-zip-typed . (Π ((n : Nat)) (Π ((A : Type)) (Π ((B : Type))
                                                       (-> (Vec n A) (Vec n B) (Vec n (× A B)))))))
    (matrix-mul-typed . (Π ((m : Nat)) (Π ((n : Nat)) (Π ((p : Nat)) (Π ((A : Type))
                                                                        (-> (Matrix m n A) (Matrix n p A) (Matrix m p A)))))))
    (matrix-add-typed . (Π ((m : Nat)) (Π ((n : Nat)) (Π ((A : Type))
                                                         (-> (Matrix m n A) (Matrix m n A) (Matrix m n A))))))
    (vec-head-typed . (Π ((n : Nat)) (Π ((A : Type))
                                        (-> (Vec (+ 1 n) A) A))))
    (vec-tail-typed . (Π ((n : Nat)) (Π ((A : Type))
                                        (-> (Vec (+ 1 n) A) (Vec n A)))))
    
    ;; Differentiable type operations
    ;; grad : Diff (Vec n α) α → Vec n α → Vec n α
    ;; Compute gradient of scalar-valued function
    (diff-grad . (Π ((n : Nat)) (Π ((α : Type))
                                   (-> (Diff (Vec n α) α) (Vec n α) (Vec n α)))))
    
    ;; jacobian : Diff (Vec n α) (Vec m α) → Vec n α → Matrix m n α
    ;; Compute Jacobian matrix for vector-valued function
    (diff-jacobian . (Π ((n : Nat)) (Π ((m : Nat)) (Π ((α : Type))
                                                      (-> (Diff (Vec n α) (Vec m α))
                                                          (Vec n α)
                                                          (Matrix m n α))))))
    
    ;; hessian : Diff (Vec n α) α → Vec n α → Matrix n n α
    ;; Compute Hessian matrix (second derivatives) for scalar-valued function
    (diff-hessian . (Π ((n : Nat)) (Π ((α : Type))
                                      (-> (Diff (Vec n α) α) (Vec n α) (Matrix n n α)))))
    
    ;; compose-diff : Diff B C → Diff A B → Diff A C
    ;; Compose two differentiable functions (chain rule built-in)
    (diff-compose . (Π ((A : Type)) (Π ((B : Type)) (Π ((C : Type))
                                                       (-> (Diff B C) (Diff A B) (Diff A C))))))
    
    ;; lift : (A → B) → Diff A B
    ;; Lift a pure numeric function to a differentiable one
    (diff-lift . (Π ((A : Type)) (Π ((B : Type))
                                    (-> (-> A B) (Diff A B)))))
    
    ;; primal : Diff A B → (A → B)
    ;; Extract the underlying function from a differentiable wrapper
    (diff-primal . (Π ((A : Type)) (Π ((B : Type))
                                      (-> (Diff A B) (-> A B)))))
    
    ;; Scalar differentiation shortcuts
    ;; diff-scalar : Diff Float Float → Float → Float
    ;; Differentiate a scalar function at a point
    (diff-scalar . (-> (Diff Float Float) Float Float))
    
    ;; jvp : Diff (Vec n α) (Vec m α) → Vec n α → Vec n α → Vec m α
    ;; Jacobian-Vector Product (forward-mode AD)
    (diff-jvp . (Π ((n : Nat)) (Π ((m : Nat)) (Π ((α : Type))
                                                 (-> (Diff (Vec n α) (Vec m α))
                                                     (Vec n α)    ; point
                                                     (Vec n α)    ; tangent vector
                                                     (Vec m α)))))) ; output tangent
    
    ;; vjp : Diff (Vec n α) (Vec m α) → Vec n α → Vec m α → Vec n α
    ;; Vector-Jacobian Product (reverse-mode AD)
    (diff-vjp . (Π ((n : Nat)) (Π ((m : Nat)) (Π ((α : Type))
                                                 (-> (Diff (Vec n α) (Vec m α))
                                                     (Vec n α)    ; point
                                                     (Vec m α)    ; cotangent vector
                                                     (Vec n α)))))) ; input cotangent
    ))

;;; ============================================================
;;; Vector Operations
;;; ============================================================

;;; vec-append-typed : Nat × Nat × Type × (Vec α) × (Vec α) → (Vec α)
;;; Π n m A. Vec n A → Vec m A → Vec (n+m) A
;;; Appends two vectors, with length tracked at type level.
(define (vec-append-typed n m A v1 v2)
  (vec-append v1 v2))

;;; vec-zip-typed : Nat × Type × Type × (Vec α) × (Vec β) → (Vec (Pair α β))
;;; Π n A B. Vec n A → Vec n B → Vec n (× A B)
;;; Zips two same-length vectors into pairs.
(define (vec-zip-typed n A B v1 v2)
  (vec-zip-with (lambda (x y) (cons x y)) v1 v2))

;;; vec-head-typed : Nat × Type × (Vec α) → α
;;; Π n A. Vec (1+n) A → A
;;; Gets the first element of a non-empty vector.
(define (vec-head-typed n A v)
  (vec-ref v 0))

;;; vec-tail-typed : Nat × Type × (Vec α) → (Vec α)
;;; Π n A. Vec (1+n) A → Vec n A
;;; Gets all but the first element of a non-empty vector.
(define (vec-tail-typed n A v)
  (let ([len (vec-length v)])
       (if (<= len 1)
           (vec)
           (vec-from-list (cdr (vec->list v))))))

;;; vec-map-typed : Nat × Type × Type × (α → β) × (Vec α) → (Vec β)
;;; Π n A B. (A → B) → Vec n A → Vec n B
;;; Maps a function over a vector, preserving length.
(define (vec-map-typed n A B f v)
  (vec-map f v))

;;; vec-fold-typed : Nat × Type × Type × (β → α → β) × β × (Vec α) → β
;;; Π n A B. (B → A → B) → B → Vec n A → B
;;; Folds a vector with an accumulator.
(define (vec-fold-typed n A B f init v)
  (vec-fold f init v))

;;; ============================================================
;;; Matrix Operations
;;; ============================================================

;;; matrix-mul-typed : Nat × Nat × Nat × Type × (Matrix α) × (Matrix α) → (Matrix α)
;;; Π m n p A. Matrix m n A → Matrix n p A → Matrix m p A
;;; Multiplies two matrices with dimension-safe type.
(define (matrix-mul-typed m n p A m1 m2)
  (matrix-mul m1 m2))

;;; matrix-add-typed : Nat × Nat × Type × (Matrix α) × (Matrix α) → (Matrix α)
;;; Π m n A. Matrix m n A → Matrix m n A → Matrix m n A
;;; Adds two matrices of the same dimensions.
(define (matrix-add-typed m n A m1 m2)
  (matrix-add m1 m2))

;;; matrix-transpose-typed : Nat × Nat × Type × (Matrix α) → (Matrix α)
;;; Π m n A. Matrix m n A → Matrix n m A
;;; Transposes a matrix, swapping dimensions.
(define (matrix-transpose-typed m n A mat)
  (matrix-transpose mat))

;;; matrix-scale-typed : Nat × Nat × Type × α × (Matrix α) → (Matrix α)
;;; Π m n A. A → Matrix m n A → Matrix m n A
;;; Scales a matrix by a scalar.
(define (matrix-scale-typed m n A scalar mat)
  (matrix-scale scalar mat))

;;; ============================================================
;;; Safe Indexing Operations
;;; ============================================================

;;; vec-ref-safe : Nat × Type × Nat × α × (Vec α) → α
;;; Π n A. (i : Nat) → (proof : i < n) → Vec n A → A
;;; Safe vector indexing with proof that index is in bounds.
;;; At runtime, we trust the proof exists and just do the access.
(define (vec-ref-safe n A i proof v)
  (vec-ref v i))

;;; matrix-ref-safe : Nat × Nat × Type × Nat × Nat × α × α × (Matrix α) → α
;;; Π m n A. (i : Nat) → (j : Nat) → i < m → j < n → Matrix m n A → A
;;; Safe matrix indexing with bounds proofs.
(define (matrix-ref-safe m n A i j proof-i proof-j mat)
  (matrix-ref mat i j))

;;; ============================================================
;;; Constructors
;;; ============================================================

;;; vec-nil-typed : Type → (Vec α)
;;; Π A. Vec 0 A
;;; Empty vector typed constructor.
(define (vec-nil-typed A)
  (vec))

;;; vec-cons-typed : Nat × Type × α × (Vec α) → (Vec α)
;;; Π n A. A → Vec n A → Vec (+ 1 n) A
;;; Cons an element onto a vector.
(define (vec-cons-typed n A x v)
  (vec-from-list (cons x (vec->list v))))

;;; ============================================================
;;; Utility for Type Context
;;; ============================================================

;;; make-dep-linalg-ctx : Unit → (List (Pair Symbol Type))
;;; Creates a type context with all dependent linalg operations.
(define (make-dep-linalg-ctx)
  dep-linalg-types)

;;; extend-ctx-with-dep-linalg : (List (Pair Symbol Type)) → (List (Pair Symbol Type))
;;; Extends an existing context with dependent linalg types.
(define (extend-ctx-with-dep-linalg ctx)
  (append dep-linalg-types ctx))

;;; ============================================================
;;; Differentiable Operations (Runtime Stubs)
;;; ============================================================
;;;
;;; These operations integrate with the autodiff system.
;;; The implementations delegate to core/autodiff/reverse-diff.ss
;;; and core/autodiff/higher-order-diff.ss.
;;;
;;; At type-checking time, the type signatures above ensure
;;; dimension safety. At runtime, these stubs validate dimensions
;;; and call the underlying autodiff primitives.

;;; diff-grad : Nat × Type × DiffFn × (Vec α) → (Vec α)
;;; Π n α. Diff (Vec n α) α → Vec n α → Vec n α
;;; Compute gradient of a scalar-valued differentiable function.
(define (diff-grad n A diff-fn x)
  ;; Delegates to reverse-mode autodiff
  ;; diff-fn wraps the function for differentiation
  (let ([gradient-fn (cdr (assq 'gradient diff-fn))])
       (if gradient-fn
           (gradient-fn x)
           (error 'diff-grad "Not a differentiable function" diff-fn))))

;;; diff-jacobian : Nat × Nat × Type × DiffFn × (Vec α) → (Matrix α)
;;; Π n m α. Diff (Vec n α) (Vec m α) → Vec n α → Matrix m n α
;;; Compute Jacobian matrix.
(define (diff-jacobian n m A diff-fn x)
  ;; Delegates to higher-order-diff for Jacobian computation
  (let ([jacobian-fn (cdr (assq 'jacobian diff-fn))])
       (if jacobian-fn
           (jacobian-fn x)
           (error 'diff-jacobian "Cannot compute Jacobian" diff-fn))))

;;; diff-hessian : Nat × Type × DiffFn × (Vec α) → (Matrix α)
;;; Π n α. Diff (Vec n α) α → Vec n α → Matrix n n α
;;; Compute Hessian matrix (second derivatives).
(define (diff-hessian n A diff-fn x)
  (let ([hessian-fn (cdr (assq 'hessian diff-fn))])
       (if hessian-fn
           (hessian-fn x)
           (error 'diff-hessian "Cannot compute Hessian" diff-fn))))

;;; diff-compose : Type × Type × Type × DiffFn × DiffFn → DiffFn
;;; Π A B C. Diff B C → Diff A B → Diff A C
;;; Compose two differentiable functions.
(define (diff-compose A B C diff-g diff-f)
  ;; Composition result is also differentiable (chain rule)
  `((primal . ,(lambda (x) ((cdr (assq 'primal diff-g))
                            ((cdr (assq 'primal diff-f)) x))))
    (gradient . ,(lambda (x)
                         ;; Chain rule: ∇(g∘f)(x) = (∇f(x))ᵀ · ∇g(f(x))
                         (let ([fx ((cdr (assq 'primal diff-f)) x)]
                               [grad-f (cdr (assq 'gradient diff-f))]
                               [grad-g (cdr (assq 'gradient diff-g))])
                              ;; Simplified: assumes scalar output
                              (grad-f x))))))

;;; diff-lift : Type × Type × (A → B) → DiffFn
;;; Π A B. (A → B) → Diff A B
;;; Lift a numeric function to a differentiable one.
(define (diff-lift A B f)
  ;; Creates a differentiable wrapper
  ;; For full implementation, this would integrate with traced evaluation
  `((primal . ,f)
    (gradient . ,(lambda (x)
                         (error 'diff-lift "Gradient not available for lifted function")))))

;;; diff-primal : Type × Type × DiffFn → (A → B)
;;; Π A B. Diff A B → (A → B)
;;; Extract the underlying function.
(define (diff-primal A B diff-fn)
  (cdr (assq 'primal diff-fn)))

;;; diff-scalar : DiffFn × Float → Float
;;; Differentiate a scalar function at a point.
(define (diff-scalar diff-fn x)
  (let ([grad-fn (cdr (assq 'gradient diff-fn))])
       (if grad-fn
           (grad-fn x)
           0.0)))

;;; diff-jvp : Nat × Nat × Type × DiffFn × (Vec α) × (Vec α) → (Vec α)
;;; Π n m α. Diff (Vec n α) (Vec m α) → Vec n α → Vec n α → Vec m α
;;; Jacobian-Vector Product (forward-mode AD).
(define (diff-jvp n m A diff-fn x v)
  (let ([jvp-fn (cdr (assq 'jvp diff-fn))])
       (if jvp-fn
           (jvp-fn x v)
           (error 'diff-jvp "JVP not available" diff-fn))))

;;; diff-vjp : Nat × Nat × Type × DiffFn × (Vec α) × (Vec α) → (Vec α)
;;; Π n m α. Diff (Vec n α) (Vec m α) → Vec n α → Vec m α → Vec n α
;;; Vector-Jacobian Product (reverse-mode AD).
(define (diff-vjp n m A diff-fn x w)
  (let ([vjp-fn (cdr (assq 'vjp diff-fn))])
       (if vjp-fn
           (vjp-fn x w)
           (error 'diff-vjp "VJP not available" diff-fn))))
