; function-ext.ss - Extended function combinators and utilities
; Part of The Fold prelude

; ============================================
; Fixed-point and iteration combinators
; ============================================

; converge: Apply function until result stops changing (fixed point)
; (converge (fn (x) (/ (+ x (/ 2 x)) 2)) 1.0) => ~1.414 (sqrt 2)
(define (converge f x)
  (let ((next (f x)))
    (if (equal? x next)
        x
        (converge f next))))

; fixed-point: Same as converge (alternative name)
(define fixed-point converge)

; ============================================
; Advanced currying and partial application
; ============================================

; curry3: Curry a 3-argument function
; (((curry3 (fn (a b c) (+ a (+ b c)))) 1) 2) 3) => 6
(define (curry3 f)
  (fn (a) (fn (b) (fn (c) (f a b c)))))

; uncurry3: Uncurry to a 3-argument function
; ((uncurry3 (fn (a) (fn (b) (fn (c) (+ a (+ b c)))))) 1 2 3) => 6
(define (uncurry3 f)
  (fn (a b c) (((f a) b) c)))

; partial1: Partially apply first argument
; ((partial1 + 10) 5) => 15
(define (partial1 f x)
  (fn (y) (f x y)))

; partial2: Partially apply first two arguments
; ((partial2 (fn (a b c) (+ a (+ b c))) 1 2) 3) => 6
(define (partial2 f x y)
  (fn (z) (f x y z)))

; ============================================
; Function combinators
; ============================================

; converge-with: Apply two functions and combine results
; ((converge-with + inc double) 5) => 16  ; (inc 5) + (double 5)
(define (converge-with combiner f g)
  (fn (x) (combiner (f x) (g x))))

; tap: Apply function for side effect, return original value
; Useful for debugging in pipelines
(define (tap f)
  (fn (x)
    (f x)
    x))

; dup: Duplicate a value as a pair
; (dup 5) => (5 . 5)
(define (dup x)
  (cons x x))

; swap: Swap elements of a pair
; (swap '(1 . 2)) => (2 . 1)
(define (swap p)
  (cons (cdr p) (car p)))

; ============================================
; Memoization (stub in pure context)
; ============================================

; memoize: Stub for memoization (returns function as-is)
; Note: True memoization requires mutation
(define (memoize f)
  f)

; memoize-1: Alias for memoize
(define memoize-1 memoize)

; ============================================
; Module exports
; ============================================

(module-exports
  converge
  fixed-point
  curry3
  uncurry3
  partial1
  partial2
  converge-with
  tap
  dup
  swap
  memoize
  memoize-1)
