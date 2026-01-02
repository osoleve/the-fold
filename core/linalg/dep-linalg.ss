;;; fabric/stitches/dep-linalg.ss — Dependent Linear Algebra
;;;
;;; Dimension-safe wrappers for linalg operations.
;;; These use Pi types to track vector and matrix dimensions at type level.

(load "core/base/prelude.ss")
(load "core/linalg/vec.ss")
(load "core/linalg/matrix.ss")
(load "core/types/dep-types.ss")
(load "core/types/dep-infer.ss")
(load "core/lang/nbe.ss")

;;; ============================================================
;;; Vector Operations
;;; ============================================================

;;; vec-append-typed : Π n m A. Vec n A → Vec m A → Vec (n+m) A
(define vec-append-typed
  (the (Π ((n : Nat) (m : Nat) (A : Type))
          (-> (Vec n A) (Vec m A) (Vec (+ n m) A)))
       (fn (n m A v1 v2)
           (vec-append v1 v2))))

;;; vec-zip-typed : Π n A B. Vec n A → Vec n B → Vec n (× A B)
(define vec-zip-typed
  (the (Π ((n : Nat) (A : Type) (B : Type))
          (-> (Vec n A) (Vec n B) (Vec n (× A B))))
       (fn (n A B v1 v2)
           (vec-zip-with (lambda (x y) (pair x y)) v1 v2))))

;;; ============================================================
;;; Matrix Operations
;;; ============================================================

;;; matrix-mul-typed : Π m n p A. Matrix m n A → Matrix n p A → Matrix m p A
(define matrix-mul-typed
  (the (Π ((m : Nat) (n : Nat) (p : Nat) (A : Type))
          (-> (Matrix m n A) (Matrix n p A) (Matrix m p A)))
       (fn (m n p A m1 m2)
           (matrix-mul m1 m2))))

;;; matrix-add-typed : Π m n A. Matrix m n A → Matrix m n A → Matrix m n A
(define matrix-add-typed
  (the (Π ((m : Nat) (n : Nat) (A : Type))
          (-> (Matrix m n A) (Matrix m n A) (Matrix m n A)))
       (fn (m n A m1 m2)
           (matrix-add m1 m2))))
