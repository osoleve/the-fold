; predicate.ss - Predicate combinators and utilities
; Part of The Fold prelude

; ============================================
; Basic predicate combinators
; ============================================

; both: Combine two predicates with and
; ((both positive? even?) 4) => #t
(define (both p1 p2)
  (fn (x) (and (p1 x) (p2 x))))

; either-pred: Combine two predicates with or
; ((either-pred positive? even?) -2) => #t
(define (either-pred p1 p2)
  (fn (x) (or (p1 x) (p2 x))))

; neither: Combine two predicates with nor
; ((neither positive? even?) -3) => #f
(define (neither p1 p2)
  (fn (x) (not (or (p1 x) (p2 x)))))

; negate: Negate a predicate
; ((negate even?) 3) => #t
(define (negate p)
  (fn (x) (not (p x))))

; ============================================
; N-ary predicate combinators
; ============================================

; all-of: Check if all predicates hold
; ((all-of positive? even? (fn (x) (< x 10))) 4) => #t
(define (all-of . preds)
  (fn (x)
      (all (fn (p) (p x)) preds)))

; any-of: Check if any predicate holds
; ((any-of negative? even?) 4) => #t
(define (any-of . preds)
  (fn (x)
      (any (fn (p) (p x)) preds)))

; none-of: Check if no predicates hold
; ((none-of negative? even?) 3) => #t
(define (none-of . preds)
  (fn (x)
      (not (any (fn (p) (p x)) preds))))

; ============================================
; Predicate utilities
; ============================================

; conjoin: Combine predicates with and (n-ary version of both)
(define conjoin all-of)

; disjoin: Combine predicates with or (n-ary version of either-pred)
(define disjoin any-of)

; complement: Negate predicate (alias for negate)
(define complement negate)

; ============================================
; Module exports
; ============================================

(module-exports
 both
 either-pred
 neither
 negate
 all-of
 any-of
 none-of
 conjoin
 disjoin
 complement)
