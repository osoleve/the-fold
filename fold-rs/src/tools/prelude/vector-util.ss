; vector-util.ss - Vector higher-order functions
; Part of The Fold prelude

; ============================================
; Vector mapping and filtering
; ============================================

; vec-map: Map function over vector
; (vec-map inc #(1 2 3)) => #(2 3 4)
(define (vec-map f v)
  (list->vec (map f (vec->list v))))

; vec-filter: Filter vector by predicate
; (vec-filter even? #(1 2 3 4)) => #(2 4)
(define (vec-filter f v)
  (list->vec (filter f (vec->list v))))

; ============================================
; Vector folding
; ============================================

; vec-foldl: Left fold over vector
; (vec-foldl + 0 #(1 2 3)) => 6
(define (vec-foldl f init v)
  (foldl f init (vec->list v)))

; vec-foldr: Right fold over vector
; (vec-foldr cons '() #(1 2 3)) => (1 2 3)
(define (vec-foldr f init v)
  (foldr f init (vec->list v)))

; ============================================
; Vector predicates
; ============================================

; vec-any: Check if any element satisfies predicate
; (vec-any even? #(1 3 4)) => #t
(define (vec-any f v)
  (any f (vec->list v)))

; vec-all: Check if all elements satisfy predicate
; (vec-all positive? #(1 2 3)) => #t
(define (vec-all f v)
  (all f (vec->list v)))

; ============================================
; Vector search
; ============================================

; vec-find: Find first element matching predicate
; (vec-find even? #(1 3 4 5)) => 4
(define (vec-find f v)
  (find-if f (vec->list v)))

; ============================================
; Vector aggregation
; ============================================

; vec-sum: Sum of vector elements
; (vec-sum #(1 2 3 4)) => 10
(define (vec-sum v)
  (foldl + 0 (vec->list v)))

; vec-product: Product of vector elements
; (vec-product #(2 3 4)) => 24
(define (vec-product v)
  (foldl * 1 (vec->list v)))

; ============================================
; Module exports
; ============================================

(module-exports
  vec-map
  vec-filter
  vec-foldl
  vec-foldr
  vec-any
  vec-all
  vec-find
  vec-sum
  vec-product)
