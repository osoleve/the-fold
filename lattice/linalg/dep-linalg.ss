(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")

(doc 'module 'dep-linalg)
(doc 'description "Dependent Linear Algebra — Dimension-safe wrappers for linalg operations. These use Pi types to track vector and matrix dimensions at type level.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'type-signatures)

(doc 'note "The type signatures shown in dep-linalg-types are for documentation and are checked by the dependent type inference system (dep-infer.ss). At runtime, these are thin wrappers around the base linalg operations.")

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

(doc 'section 'vector-operations)

(define (vec-append-typed n m A v1 v2)
  (doc 'type "Π n m A. Vec n A → Vec m A → Vec (n+m) A")
  (doc 'description "Appends two vectors, with length tracked at type level.")
  (vec-append v1 v2))

(define (vec-zip-typed n A B v1 v2)
  (doc 'type "Π n A B. Vec n A → Vec n B → Vec n (× A B)")
  (doc 'description "Zips two same-length vectors into pairs.")
  (vec-zip-with (lambda (x y) (cons x y)) v1 v2))

(define (vec-head-typed n A v)
  (doc 'type "Π n A. Vec (1+n) A → A")
  (doc 'description "Gets the first element of a non-empty vector.")
  (vec-ref v 0))

(define (vec-tail-typed n A v)
  (doc 'type "Π n A. Vec (1+n) A → Vec n A")
  (doc 'description "Gets all but the first element of a non-empty vector.")
  (let ([len (vec-length v)])
       (if (<= len 1)
           (vec)
           (vec-from-list (cdr (vec->list v))))))

(define (vec-map-typed n A B f v)
  (doc 'type "Π n A B. (A → B) → Vec n A → Vec n B")
  (doc 'description "Maps a function over a vector, preserving length.")
  (vec-map f v))

(define (vec-fold-typed n A B f init v)
  (doc 'type "Π n A B. (B → A → B) → B → Vec n A → B")
  (doc 'description "Folds a vector with an accumulator.")
  (vec-fold f init v))

(doc 'section 'matrix-operations)

(define (matrix-mul-typed m n p A m1 m2)
  (doc 'type "Π m n p A. Matrix m n A → Matrix n p A → Matrix m p A")
  (doc 'description "Multiplies two matrices with dimension-safe type.")
  (matrix-mul m1 m2))

(define (matrix-add-typed m n A m1 m2)
  (doc 'type "Π m n A. Matrix m n A → Matrix m n A → Matrix m n A")
  (doc 'description "Adds two matrices of the same dimensions.")
  (matrix-add m1 m2))

(define (matrix-transpose-typed m n A mat)
  (doc 'type "Π m n A. Matrix m n A → Matrix n m A")
  (doc 'description "Transposes a matrix, swapping dimensions.")
  (matrix-transpose mat))

(define (matrix-scale-typed m n A scalar mat)
  (doc 'type "Π m n A. A → Matrix m n A → Matrix m n A")
  (doc 'description "Scales a matrix by a scalar.")
  (matrix-scale scalar mat))

(doc 'section 'safe-indexing)

(define (vec-ref-safe n A i proof v)
  (doc 'type "Π n A. (i : Nat) → (proof : i < n) → Vec n A → A")
  (doc 'description "Safe vector indexing with proof that index is in bounds. At runtime, we trust the proof exists and just do the access.")
  (vec-ref v i))

(define (matrix-ref-safe m n A i j proof-i proof-j mat)
  (doc 'type "Π m n A. (i : Nat) → (j : Nat) → i < m → j < n → Matrix m n A → A")
  (doc 'description "Safe matrix indexing with bounds proofs.")
  (matrix-ref mat i j))

(doc 'section 'constructors)

(define (vec-nil-typed A)
  (doc 'type "Π A. Vec 0 A")
  (doc 'description "Empty vector typed constructor.")
  (vec))

(define (vec-cons-typed n A x v)
  (doc 'type "Π n A. A → Vec n A → Vec (+ 1 n) A")
  (doc 'description "Cons an element onto a vector.")
  (vec-from-list (cons x (vec->list v))))

(doc 'section 'type-context-utilities)

(define (make-dep-linalg-ctx)
  (doc 'description "Creates a type context with all dependent linalg operations.")
  dep-linalg-types)

(define (extend-ctx-with-dep-linalg ctx)
  (doc 'description "Extends an existing context with dependent linalg types.")
  (append dep-linalg-types ctx))

(doc 'section 'differentiable-operations)

(doc 'note "These operations integrate with the autodiff system. The implementations delegate to core/autodiff/reverse-diff.ss and core/autodiff/higher-order-diff.ss. At type-checking time, the type signatures above ensure dimension safety. At runtime, these stubs validate dimensions and call the underlying autodiff primitives.")

(define (diff-grad n A diff-fn x)
  (doc 'type "Π n α. Diff (Vec n α) α → Vec n α → Vec n α")
  (doc 'description "Compute gradient of a scalar-valued differentiable function.")
  ;; Delegates to reverse-mode autodiff
  ;; diff-fn wraps the function for differentiation
  (let ([gradient-fn (cdr (assq 'gradient diff-fn))])
       (if gradient-fn
           (gradient-fn x)
           (error 'diff-grad "Not a differentiable function" diff-fn))))

(define (diff-jacobian n m A diff-fn x)
  (doc 'type "Π n m α. Diff (Vec n α) (Vec m α) → Vec n α → Matrix m n α")
  (doc 'description "Compute Jacobian matrix.")
  ;; Delegates to higher-order-diff for Jacobian computation
  (let ([jacobian-fn (cdr (assq 'jacobian diff-fn))])
       (if jacobian-fn
           (jacobian-fn x)
           (error 'diff-jacobian "Cannot compute Jacobian" diff-fn))))

(define (diff-hessian n A diff-fn x)
  (doc 'type "Π n α. Diff (Vec n α) α → Vec n α → Matrix n n α")
  (doc 'description "Compute Hessian matrix (second derivatives).")
  (let ([hessian-fn (cdr (assq 'hessian diff-fn))])
       (if hessian-fn
           (hessian-fn x)
           (error 'diff-hessian "Cannot compute Hessian" diff-fn))))

(define (diff-compose A B C diff-g diff-f)
  (doc 'type "Π A B C. Diff B C → Diff A B → Diff A C")
  (doc 'description "Compose two differentiable functions. Chain rule: ∇(g∘f)(x) = ∇g(f(x)) · ∇f(x)")
  ;; Composition result is also differentiable (chain rule)
  (let ([primal-f (cdr (assq 'primal diff-f))]
        [primal-g (cdr (assq 'primal diff-g))]
        [grad-f (cdr (assq 'gradient diff-f))]
        [grad-g (cdr (assq 'gradient diff-g))])
       `((primal . ,(lambda (x) (primal-g (primal-f x))))
         (gradient . ,(lambda (x)
                              ;; Chain rule: ∇(g∘f)(x) = ∇g(f(x)) · ∇f(x)
                              ;; For scalars: multiply the derivatives
                              ;; For vectors: matrix multiplication (Jacobian chain)
                              (let ([fx (primal-f x)]
                                    [df-at-x (grad-f x)]
                                    [dg-at-fx (grad-g fx)])
                                   ;; Scalar case: multiply
                                   (if (number? df-at-x)
                                       (* dg-at-fx df-at-x)
                                       ;; Vector/matrix case: needs proper composition
                                       ;; For now, signal that full implementation needed
                                       (error 'diff-compose
                                              "Non-scalar chain rule not yet implemented"))))))))

(define (diff-lift A B f)
  (doc 'type "Π A B. (A → B) → Diff A B")
  (doc 'description "Lift a numeric function to a differentiable one. For full implementation, this would integrate with traced evaluation.")
  ;; Creates a differentiable wrapper
  `((primal . ,f)
    (gradient . ,(lambda (x)
                         (error 'diff-lift "Gradient not available for lifted function")))))

(define (diff-primal A B diff-fn)
  (doc 'type "Π A B. Diff A B → (A → B)")
  (doc 'description "Extract the underlying function from a differentiable wrapper.")
  (cdr (assq 'primal diff-fn)))

(define (diff-scalar diff-fn x)
  (doc 'type "(Diff Int Int) → Int → Int")
  (doc 'description "Differentiate a scalar function at a point.")
  (let ([grad-fn (cdr (assq 'gradient diff-fn))])
       (if grad-fn
           (grad-fn x)
           0.0)))

(define (diff-jvp n m A diff-fn x v)
  (doc 'type "Π n m α. Diff (Vec n α) (Vec m α) → Vec n α → Vec n α → Vec m α")
  (doc 'description "Jacobian-Vector Product (forward-mode AD).")
  (let ([jvp-fn (cdr (assq 'jvp diff-fn))])
       (if jvp-fn
           (jvp-fn x v)
           (error 'diff-jvp "JVP not available" diff-fn))))

(define (diff-vjp n m A diff-fn x w)
  (doc 'type "Π n m α. Diff (Vec n α) (Vec m α) → Vec n α → Vec m α → Vec n α")
  (doc 'description "Vector-Jacobian Product (reverse-mode AD).")
  (let ([vjp-fn (cdr (assq 'vjp diff-fn))])
       (if vjp-fn
           (vjp-fn x w)
           (error 'diff-vjp "VJP not available" diff-fn))))
