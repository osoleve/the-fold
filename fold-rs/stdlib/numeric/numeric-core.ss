; ============================================================
; Numeric - Core
; Basic arithmetic operations, predicates, and algebraic types
; Part of numeric.ss module
; ============================================================

; -- Basic Arithmetic Operations --

; sum-by: Sum elements after applying function
(sum-by (fn (f lst) (foldl (fn (acc x) (+ acc (f x))) 0 lst)))

; product-by: Product of elements after applying function
(product-by (fn (f lst) (foldl (fn (acc x) (* acc (f x))) 1 lst)))

; average: Average of a list of numbers
(average (fn (lst)
             (if (null? lst)
                 0
                 (/ (sum-list lst) (length lst)))))

; clamp-val: Clamp value to range [lo, hi]
(clamp-val (fn (lo hi x)
               (max lo (min hi x))))

; abs-diff: Absolute difference
(abs-diff (fn (a b) (abs (- a b))))

; sign-of: Get sign of number (-1, 0, or 1)
(sign-of (fn (n)
             (if (< n 0) (- 0 1)
                 (if (> n 0) 1 0))))

; divides?: Check if a divides b evenly
(divides? (fn (a b) (= (mod b a) 0)))

; -- Exponentiation --

; factorial: n!
(factorial (fix factorial
                (fn (n)
                    (if (<= n 1) 1 (* n (factorial (- n 1)))))))

; pow-int: Integer exponentiation (efficient)
; (pow-int 2 10) => 1024
(pow-int (fix pow-int
              (fn (base exp)
                  (if (= exp 0)
                      1
                      (if (even? exp)
                          (let ((half (pow-int base (/ exp 2))))
                               (* half half))
                          (* base (pow-int base (- exp 1))))))))

; -- Modular Arithmetic --

; mod-exp: Modular exponentiation (base^exp mod m)
(mod-exp (fix mod-exp
              (fn (base exp m)
                  (if (= exp 0)
                      1
                      (if (= (mod exp 2) 0)
                          (let ((half (mod-exp base (/ exp 2) m)))
                               (mod (* half half) m))
                          (mod (* base (mod-exp base (- exp 1) m)) m))))))

; mod-inverse: Modular multiplicative inverse (extended Euclidean)
(mod-inverse (fn (a m)
                 (let ((egcd (fix egcd
                                  (fn (a b)
                                      (if (= b 0)
                                          (list a 1 0)
                                          (let ((result (egcd b (mod a b))))
                                               (list (car result)
                                                     (caddr result)
                                                     (- (cadr result) (* (/ a b) (caddr result))))))))))
                      (let ((result (egcd a m)))
                           (if (= (car result) 1)
                               (mod (+ (cadr result) m) m)
                               #f)))))

; -- Combinatorics --

; binomial: Binomial coefficient (n choose k)
; (binomial 5 2) => 10
(binomial (fn (n k)
              (if (or (< k 0) (> k n))
                  0
                  (if (or (= k 0) (= k n))
                      1
                      (/ (factorial n) (* (factorial k) (factorial (- n k))))))))

; permutations-count: Number of permutations P(n,k)
(permutations-count (fn (n k)
                        (/ (factorial n) (factorial (- n k)))))

; catalan: Catalan number
(catalan (fn (n)
             (/ (binomial (* 2 n) n) (+ n 1))))

; -- Approximation --

; newton-sqrt: Square root via Newton's method
(newton-sqrt (fn (x)
                 (if (<= x 0) 0
                     (let ((improve (fn (guess) (/ (+ guess (/ x guess)) 2)))
                           (good-enough? (fn (guess) (< (abs (- (* guess guess) x)) 0.00001))))
                          (let ((iter (fix iter
                                           (fn (guess)
                                               (if (good-enough? guess)
                                                   guess
                                                   (iter (improve guess)))))))
                               (iter 1.0))))))

; nth-root: Nth root via Newton's method
(nth-root (fn (n x)
              (if (<= x 0) 0
                  (let ((improve (fn (guess)
                                     (/ (+ (* (- n 1) guess) (/ x (pow-int (floor guess) (- n 1)))) n))))
                       (let ((iter (fix iter
                                        (fn (count guess)
                                            (if (<= count 0)
                                                guess
                                                (iter (- count 1) (improve guess)))))))
                            (iter 20 1.0))))))

; convergent-seq: Sequence that converges to limit with given precision
(convergent-seq (fn (f init epsilon max-iter)
                    (let ((go (fix go
                                   (fn (val iter acc)
                                       (if (>= iter max-iter)
                                           (reverse acc)
                                           (let ((next (f val)))
                                                (if (< (abs (- next val)) epsilon)
                                                    (reverse (cons next acc))
                                                    (go next (+ iter 1) (cons next acc)))))))))
                         (go init 0 (list init)))))

; cycle-detect: Detect cycle using Floyd's algorithm
(cycle-detect (fn (f x0)
                  (let ((tortoise (f x0))
                        (hare (f (f x0))))
                       (let ((find-cycle (fix find-cycle
                                              (fn (t h)
                                                  (if (eq? t h)
                                                      t
                                                      (find-cycle (f t) (f (f h))))))))
                            (find-cycle tortoise hare)))))

; -- Complex Numbers --

; complex-new: Create complex number as (real . imag)
(complex-new (fn (r i) (cons r i)))

; complex-real: Get real part
(complex-real car)

; complex-imag: Get imaginary part
(complex-imag cdr)

; complex-real-imag: Get both real and imaginary parts as a pair
(complex-real-imag (fn (c) (cons (complex-real c) (complex-imag c))))

; complex-add: Add two complex numbers
(complex-add (fn (c1 c2)
                 (complex-new (+ (complex-real c1) (complex-real c2))
                              (+ (complex-imag c1) (complex-imag c2)))))

; complex-sub: Subtract complex numbers
(complex-sub (fn (c1 c2)
                 (complex-new (- (complex-real c1) (complex-real c2))
                              (- (complex-imag c1) (complex-imag c2)))))

; complex-mul: Multiply complex numbers
(complex-mul (fn (c1 c2)
                 (let ((r1 (complex-real c1)) (i1 (complex-imag c1))
                       (r2 (complex-real c2)) (i2 (complex-imag c2)))
                      (complex-new (- (* r1 r2) (* i1 i2))
                                   (+ (* r1 i2) (* i1 r2))))))

; complex-magnitude: Magnitude of complex number
(complex-magnitude (fn (c)
                       (sqrt (+ (* (complex-real c) (complex-real c))
                                (* (complex-imag c) (complex-imag c))))))

; complex-conjugate: Complex conjugate
(complex-conjugate (fn (c)
                       (complex-new (complex-real c) (- (complex-imag c)))))

; -- Matrix Operations --

; matrix-rows: Get number of rows
(matrix-rows (fn (m)
                 (length m)))

; matrix-cols: Get number of columns
(matrix-cols (fn (m)
                 (if (null? m) 0 (length (car m)))))

; matrix-ref: Get element at (row, col)
(matrix-ref (fn (m row col)
                (list-ref (list-ref m row) col)))

; matrix-row: Get row at index
(matrix-row list-ref)

; matrix-col: Get column at index
(matrix-col (fn (m col)
                (map (fn (row) (list-ref row col)) m)))

; matrix-transpose: Transpose matrix
(matrix-transpose (fix matrix-transpose
                       (fn (m)
                           (if (or (null? m) (null? (car m)))
                               '()
                               (cons (map car m)
                                     (matrix-transpose (map cdr m)))))))

; matrix-map: Map function over all elements
(matrix-map (fn (f m)
                (map (fn (row) (map f row)) m)))

; matrix-add: Add two matrices
(matrix-add (fn (m1 m2)
                (zip-with (fn (r1 r2) (zip-with + r1 r2)) m1 m2)))

; matrix-scale: Scale matrix by scalar
(matrix-scale (fn (k m)
                  (matrix-map (fn (x) (* k x)) m)))

; matrix-multiply: Matrix multiplication
(matrix-multiply (fn (a b)
                     (let ((bt (matrix-transpose b)))
                          (map (fn (row-a)
                                   (map (fn (col-b)
                                            (foldl + 0 (zip-with * row-a col-b)))
                                        bt))
                               a))))

; --- Module Exports ---
; (see exports.ss for exported symbols)
