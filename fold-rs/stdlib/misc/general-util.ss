; ============================================================
; General Utility Functions and Aliases
; Basic utilities, math shortcuts, and list helpers
; ============================================================

; id-fn: Identity function
(id-fn (fn (x) x))

; const-fn: Constant function
(const-fn (fn (x)
             (fn (_) x)))

; inc: Increment by 1
(inc (fn (x) (+ x 1)))

; dec: Decrement by 1
(dec (fn (x) (- x 1)))

; succ: Successor (alias)
(succ inc)

; pred-fn: Predecessor (alias)
(pred-fn dec)

; square: Square a number
(square (fn (x) (* x x)))

; cube: Cube a number
(cube (fn (x) (* x x x)))

; double: Double a number
(double (fn (x) (* x 2)))

; halve: Halve a number
(halve (fn (x) (/ x 2)))

; signum: Sign of number
(signum (fn (x)
           (if (> x 0)
               1
               (if (< x 0)
                   -1
                   0))))

; in-range?: Check if value is in range
(in-range? (fn (lo hi x)
              (and (>= x lo) (<= x hi))))

; singleton?: Check if list has exactly one element
(singleton? (fn (lst)
               (and (pair? lst) (null? (cdr lst)))))

; filter-not: Filter elements NOT matching predicate
(filter-not (fn (p lst)
               (filter (fn (x) (not (p x))) lst)))

; remove-all: Remove all occurrences of element
(remove-all (fn (x lst)
               (filter (fn (y) (not (equal? x y))) lst)))

; build-list: Build list by applying function to indices
(build-list (fn (n f)
               (map f (range 0 n))))

; max-of-list: Maximum element by comparison function
(max-of-list (fn (compare lst)
                (if (null? lst)
                    '()
                    (foldl (fn (acc x) (if (compare x acc) x acc))
                           (car lst)
                           (cdr lst)))))

; min-of-list: Minimum element by comparison function
(min-of-list (fn (compare lst)
                (if (null? lst)
                    '()
                    (foldl (fn (acc x) (if (compare x acc) x acc))
                           (car lst)
                           (cdr lst)))))

; argmax: Element that maximizes function
(argmax (fn (f lst)
           (if (null? lst)
               '()
               (foldl (fn (acc x) (if (> (f x) (f acc)) x acc))
                      (car lst)
                      (cdr lst)))))

; argmin: Element that minimizes function
(argmin (fn (f lst)
           (if (null? lst)
               '()
               (foldl (fn (acc x) (if (< (f x) (f acc)) x acc))
                      (car lst)
                      (cdr lst)))))
