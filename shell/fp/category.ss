;;; shell/fp/category.ss — Validated Category Theory Entry Points
;;;
;;; Shell-layer wrappers that validate inputs before calling pure
;;; lattice functions. Use these when accepting external/user input.
;;;
;;; Lattice code (lattice/fp/category/) is pure and assumes valid input.
;;; This module provides the defensive boundary.

(load "lattice/fp/category/natural-transform.ss")
(load "lattice/fp/category/adjunction.ss")

;;; ====
;;; Validated Natural Transformation Operations
;;; ====

;;; validated-nat-compose : NatTransform × NatTransform → NatTransform
;;; Validates inputs before composing natural transformations.
;;; Use this at shell boundaries; lattice code can use nat-compose directly.
(define (validated-nat-compose ε η)
  (unless (nat-transform? η)
    (error 'nat-compose "expected nat-transform for second argument" η))
  (unless (nat-transform? ε)
    (error 'nat-compose "expected nat-transform for first argument" ε))
  (nat-compose ε η))

;;; validated-nat-apply : NatTransform × F(A) → G(A)
;;; Validates the natural transformation before applying.
(define (validated-nat-apply η x)
  (unless (nat-transform? η)
    (error 'nat-apply "expected nat-transform" η))
  (nat-apply η x))

;;; validated-nat-horizontal : NatTransform × NatTransform → NatTransform
;;; Validates inputs before horizontal composition.
(define (validated-nat-horizontal η ε)
  (unless (nat-transform? η)
    (error 'nat-horizontal "expected nat-transform for first argument" η))
  (unless (nat-transform? ε)
    (error 'nat-horizontal "expected nat-transform for second argument" ε))
  (nat-horizontal η ε))

;;; ====
;;; Validated Adjunction Operations
;;; ====

;;; validated-adjunction-compose : Adjunction × Adjunction → Adjunction
;;; Validates inputs before composing adjunctions.
(define (validated-adjunction-compose adj2 adj1)
  (unless (adjunction? adj1)
    (error 'adjunction-compose "expected adjunction for first argument" adj1))
  (unless (adjunction? adj2)
    (error 'adjunction-compose "expected adjunction for second argument" adj2))
  (adjunction-compose adj2 adj1))

;;; validated-adjunction-transpose-left : Adjunction × (F(A) → B) → (A → G(B))
;;; Validates the adjunction before transposing.
(define (validated-adjunction-transpose-left adj f)
  (unless (adjunction? adj)
    (error 'adjunction-transpose-left "expected adjunction" adj))
  (unless (procedure? f)
    (error 'adjunction-transpose-left "expected procedure" f))
  (adjunction-transpose-left adj f))

;;; validated-adjunction-transpose-right : Adjunction × (A → G(B)) → (F(A) → B)
;;; Validates the adjunction before transposing.
(define (validated-adjunction-transpose-right adj g)
  (unless (adjunction? adj)
    (error 'adjunction-transpose-right "expected adjunction" adj))
  (unless (procedure? g)
    (error 'adjunction-transpose-right "expected procedure" g))
  (adjunction-transpose-right adj g))

;;; ====
;;; Re-exports (for convenience)
;;; ====

;;; Pure lattice functions that don't need validation are re-exported as-is.
;;; Users can import this module and get both validated and pure operations.
;;;
;;; Predicates (always safe):
;;;   nat-transform?, adjunction?, galois?, functor?
;;;
;;; Constructors (assume valid components):
;;;   make-nat-transform, make-adjunction, make-galois
;;;
;;; Accessors (safe on valid structures):
;;;   nat-transform-name, nat-transform-source, nat-transform-target
;;;   adjunction-name, adjunction-left, adjunction-right, etc.
;;;
;;; Identity constructors (always safe):
;;;   nat-id
