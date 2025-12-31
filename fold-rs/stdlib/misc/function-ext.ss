; ============================================================
; Extended Function Combinators and Utilities
; Advanced currying, partial application, and function tools
; ============================================================

; converge-fn: Apply function until result stops changing (fixed point)
; (converge-fn (fn (x) (/ (+ x (/ 2 x)) 2)) 1.0) => ~1.414 (sqrt 2)
(converge-fn (fix converge-fn
                 (fn (f x)
                     (let ((next (f x)))
                          (if (equal? x next)
                              x
                              (converge-fn f next))))))

; fixed-point: Same as converge-fn
(fixed-point converge-fn)

; curry3: Curry a 3-argument function
(curry3 (fn (f)
           (fn (a) (fn (b) (fn (c) (f a b c))))))

; uncurry3: Uncurry to a 3-argument function
(uncurry3 (fn (f)
             (fn (a b c) (((f a) b) c))))

; partial-1: Partially apply first argument
(partial-1 (fn (f x)
              (fn (y) (f x y))))

; partial-2: Partially apply first two arguments
(partial-2 (fn (f x y)
              (fn (z) (f x y z))))

; converge-with: Apply two functions and combine results
(converge-with (fn (combiner f g)
                  (fn (x) (combiner (f x) (g x)))))

; tap: Apply function for side effect, return original value
(tap (fn (f)
        (fn (x)
            (f x)
            x)))

; dup: Duplicate a value as a pair
(dup (fn (x) (cons x x)))

; swap-pair: Swap elements of a pair
(swap-pair (fn (p) (cons (cdr p) (car p))))

; memoize: Stub for memoization (returns function as-is)
(memoize (fn (f) f))
