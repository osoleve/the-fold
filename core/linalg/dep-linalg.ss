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
  '((vec-append-typed . (Π ((n : Nat)) (Π ((m : Nat)) (Π ((A : Type))
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
                                        (-> (Vec (+ 1 n) A) (Vec n A)))))))

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
