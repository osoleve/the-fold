; ============================================================
; Numeric Utilities
; Math operations, statistics, and number theory
; ============================================================

; -- Basic numeric operations --

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

; factorial: n!
(factorial (fix factorial
                (fn (n)
                    (if (<= n 1) 1 (* n (factorial (- n 1)))))))

; pow-int: Integer exponentiation
(pow-int (fix pow-int
              (fn (base exp)
                  (if (= exp 0)
                      1
                      (if (= (mod exp 2) 0)
                          (let ((half (pow-int base (/ exp 2))))
                               (* half half))
                          (* base (pow-int base (- exp 1))))))))

; abs-diff: Absolute difference
(abs-diff (fn (a b) (abs (- a b))))

; sign-of: Get sign of number (-1, 0, or 1)
(sign-of (fn (n)
             (if (< n 0) (- 0 1)
                 (if (> n 0) 1 0))))

; divides?: Check if a divides b evenly
(divides? (fn (a b) (= (mod b a) 0)))

; -- Statistics --

; mean: Same as average
(mean average)

; variance: Population variance
(variance (fn (lst)
              (if (null? lst)
                  0
                  (let ((m (mean lst))
                        (n (length lst)))
                       (/ (foldl (fn (acc x) (+ acc (* (- x m) (- x m)))) 0 lst) n)))))

; std-dev: Population standard deviation
(std-dev (fn (lst) (sqrt (variance lst))))

; median: Middle value of sorted list
(median (fn (lst)
            (if (null? lst)
                0
                (let ((sorted (sort lst))
                      (n (length lst)))
                     (if (odd? n)
                         (nth sorted (/ n 2))
                         (let ((mid (/ n 2)))
                              (/ (+ (nth sorted (- mid 1)) (nth sorted mid)) 2)))))))

; percentile: Get nth percentile (0-100)
(percentile (fn (p lst)
                (if (null? lst)
                    0
                    (let ((sorted (sort lst))
                          (n (length lst))
                          (idx (/ (* p (- n 1)) 100)))
                         (nth sorted (floor idx))))))

; covariance: Calculate covariance of two lists
(covariance (fn (xs ys)
                (let ((mx (mean xs))
                      (my (mean ys))
                      (n (length xs)))
                     (/ (sum-list (zip-with * (map (fn (x) (- x mx)) xs)
                                            (map (fn (y) (- y my)) ys)))
                        (- n 1)))))

; correlation: Pearson correlation coefficient
(correlation (fn (xs ys)
                 (let ((n (length xs))
                       (mean-x (mean xs))
                       (mean-y (mean ys)))
                      (let ((num (foldl + 0 (map (fn (xy)
                                                     (* (- (car xy) mean-x)
                                                        (- (cdr xy) mean-y)))
                                                 (zip xs ys))))
                            (denom-x (sqrt (foldl + 0 (map (fn (x) (* (- x mean-x) (- x mean-x))) xs))))
                            (denom-y (sqrt (foldl + 0 (map (fn (y) (* (- y mean-y) (- y mean-y))) ys)))))
                           (if (or (= denom-x 0) (= denom-y 0))
                               0
                               (/ num (* denom-x denom-y)))))))

; z-score: Calculate z-score of value
(z-score (fn (x lst)
             (let ((m (mean lst)))
                  (let ((std (sqrt (variance lst))))
                       (if (= std 0) 0 (/ (- x m) std))))))

; linear-regression: Simple linear regression (returns (slope . intercept))
(linear-regression (fn (xs ys)
                       (let ((n (length xs))
                             (mean-x (mean xs))
                             (mean-y (mean ys)))
                            (let ((num (foldl + 0 (map (fn (xy)
                                                           (* (- (car xy) mean-x)
                                                              (- (cdr xy) mean-y)))
                                                       (zip xs ys))))
                                  (denom (foldl + 0 (map (fn (x) (* (- x mean-x) (- x mean-x))) xs))))
                                 (if (= denom 0)
                                     (cons 0 mean-y)
                                     (let ((slope (/ num denom)))
                                          (cons slope (- mean-y (* slope mean-x)))))))))

; -- Number theory --

; prime?: Check if number is prime
(prime? (fn (n)
            (if (< n 2) #f
                (let ((check (fix check
                                  (fn (i)
                                      (if (> (* i i) n) #t
                                          (if (= (mod n i) 0) #f
                                              (check (+ i 1))))))))
                     (check 2)))))

; primes-up-to: List of primes up to n (sieve)
(primes-up-to (fn (n)
                  (filter prime? (range 2 (+ n 1)))))

; divisors: All divisors of n
(divisors (fn (n)
              (filter (fn (d) (= (mod n d) 0)) (range 1 (+ n 1)))))

; proper-divisors: All divisors except n itself
(proper-divisors (fn (n)
                     (filter (fn (d) (= (mod n d) 0)) (range 1 n))))

; sum-divisors: Sum of all divisors
(sum-divisors (fn (n) (sum-list (divisors n))))

; perfect?: Check if number is perfect (sum of proper divisors equals n)
(perfect? (fn (n) (= n (sum-list (proper-divisors n)))))

; coprime?: Check if two numbers are coprime
(coprime? (fn (a b) (= (gcd a b) 1)))

; totient: Euler's totient function
(totient (fn (n)
             (length (filter (fn (k) (coprime? k n)) (range 1 n)))))

; -- Digit operations --

; digits: Get list of digits of a number
(digits (fix digits
             (fn (n)
                 (if (< n 10)
                     (list n)
                     (append (digits (/ n 10)) (list (mod n 10)))))))

; from-digits: Convert list of digits to number
(from-digits (fn (ds)
                 (foldl (fn (acc d) (+ (* acc 10) d)) 0 ds)))

; digit-sum: Sum of digits
(digit-sum (fn (n) (sum-list (digits n))))

; digit-count: Number of digits
(digit-count (fn (n) (length (digits n))))

; reverse-number: Reverse digits of a number
(reverse-number (fn (n) (from-digits (reverse (digits n)))))

; palindrome-number?: Check if number is a palindrome
(palindrome-number? (fn (n) (= n (reverse-number n))))

; -- Power of 2 operations --

; is-power-of-2?: Check if n is a power of 2
(is-power-of-2? (fix is-power-of-2?
                     (fn (n)
                         (if (<= n 0) #f
                             (if (= n 1) #t
                                 (if (odd? n) #f
                                     (is-power-of-2? (/ n 2))))))))

; next-power-of-2: Find next power of 2 >= n
(next-power-of-2 (fix next-power-of-2
                      (fn (n)
                          (if (<= n 1) 1
                              (let ((helper (fix helper
                                                 (fn (p) (if (>= p n) p (helper (* p 2)))))))
                                   (helper 1))))))

; log2-int: Integer log base 2 (floor)
(log2-int (fix log2-int
               (fn (n)
                   (if (<= n 1) 0 (+ 1 (log2-int (/ n 2)))))))

; -- Modular arithmetic --

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

; triangular: Triangular number
(triangular (fn (n) (/ (* n (+ n 1)) 2)))

; is-triangular?: Check if number is triangular
(is-triangular? (fn (n)
                    (let ((k (floor (sqrt (* 2 n)))))
                         (= n (triangular k)))))

; square-number: Square number
(square-number (fn (n) (* n n)))

; is-square?: Check if number is a perfect square
(is-square? (fn (n)
                (let ((k (floor (sqrt n))))
                     (= n (* k k)))))

; pentagonal: Pentagonal number
(pentagonal (fn (n) (/ (* n (- (* 3 n) 1)) 2)))

; hexagonal: Hexagonal number
(hexagonal (fn (n) (* n (- (* 2 n) 1))))

; -- Sequence generators --

; iota-from: Generate sequence from start with step
(iota-from (fn (n start step)
               (if (<= n 0)
                   '()
                   (cons start (iota-from (- n 1) (+ start step) step)))))

; arithmetic-seq: Arithmetic sequence
(arithmetic-seq (fn (start step count)
                    (iota-from count start step)))

; geometric-seq: Geometric sequence
(geometric-seq (fn (start ratio count)
                   (if (<= count 0)
                       '()
                       (cons start (geometric-seq (* start ratio) ratio (- count 1))))))

; fibonacci-seq: First n Fibonacci numbers
(fibonacci-seq (fn (n)
                   (let ((go (fix go
                                  (fn (a b count)
                                      (if (<= count 0)
                                          '()
                                          (cons a (go b (+ a b) (- count 1))))))))
                        (go 0 1 n))))

; naturals: Generate natural numbers from 0 to n-1
(naturals (fn (n) (range 0 n)))

; evens: Generate even numbers from 0 to 2*(n-1)
(evens (fn (n) (map (fn (x) (* 2 x)) (range 0 n))))

; odds: Generate odd numbers from 1 to 2*n-1
(odds (fn (n) (map (fn (x) (+ (* 2 x) 1)) (range 0 n))))

; squares-seq: Generate square numbers
(squares (fn (n) (map (fn (x) (* x x)) (range 0 n))))

; cubes: Generate cube numbers
(cubes (fn (n) (map (fn (x) (* x x x)) (range 0 n))))

; powers-of: Generate powers of base
(powers-of (fn (base n)
               (map (fn (i) (pow-int base i)) (range 0 n))))

; factorials-up-to: Generate factorials up to n!
(factorials-up-to (fn (n)
                      (map factorial (range 0 (+ n 1)))))

; triangular-numbers: Generate triangular numbers
(triangular-numbers (fn (n)
                        (map triangular (range 1 (+ n 1)))))

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

; iterate-n: Apply function n times, collecting results
(iterate-n (fn (f n x)
               (iterate f n x)))

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
                (map (fn (r1 r2) (zip-with + r1 r2)) m1 m2)))

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
