; ============================================================
; Numeric - Sequences
; Number sequences, generators, and number theory
; Part of numeric.ss module
; ============================================================

; -- Number Theory Predicates --

; prime?: Check if number is prime
(prime? (fn (n)
            (if (< n 2) #f
                (let ((check (fix check
                                  (fn (i)
                                      (if (> (* i i) n) #t
                                          (if (= (mod n i) 0) #f
                                              (check (+ i 1))))))))
                     (check 2)))))

; coprime?: Check if two numbers are coprime (gcd = 1)
; (coprime? 15 28) => #t
(coprime? (fn (a b) (= (gcd a b) 1)))

; perfect?: Check if number equals sum of proper divisors
; (perfect? 6) => #t (1+2+3=6)
(perfect? (fn (n) (= n (sum-list (proper-divisors n)))))

; is-power-of-2?: Check if n is a power of 2
; (is-power-of-2? 16) => #t
(is-power-of-2? (fix is-power-of-2?
                     (fn (n)
                         (if (<= n 0) #f
                             (if (= n 1) #t
                                 (if (odd? n) #f
                                     (is-power-of-2? (/ n 2))))))))

; is-triangular?: Check if number is triangular
(is-triangular? (fn (n)
                    (let ((k (floor (sqrt (* 2 n)))))
                         (= n (triangular k)))))

; is-square?: Check if number is a perfect square
(is-square? (fn (n)
                (let ((k (floor (sqrt n))))
                     (= n (* k k)))))

; palindrome-number?: Check if number is a palindrome
; (palindrome-number? 12321) => #t
(palindrome-number? (fn (n) (= n (reverse-number n))))

; -- Number Theory Functions --

; primes-up-to: List of primes up to n (sieve)
(primes-up-to (fn (n)
                  (let ((sieve (fix sieve
                                    (fn (candidates)
                                        (if (null? candidates)
                                            '()
                                            (let ((p (car candidates)))
                                                 (cons p
                                                       (sieve (filter (fn (x) (not (= 0 (mod x p))))
                                                                      (cdr candidates))))))))))
                       (sieve (range 2 (+ n 1))))))

; divisors: All divisors of n
; (divisors 12) => (1 2 3 4 6 12)
(divisors (fn (n)
              (filter (fn (d) (= (mod n d) 0)) (range 1 (+ n 1)))))

; proper-divisors: All divisors except n itself
(proper-divisors (fn (n)
                     (filter (fn (d) (= (mod n d) 0)) (range 1 n))))

; sum-divisors: Sum of all divisors
(sum-divisors (fn (n) (sum-list (divisors n))))

; totient: Euler's totient function (count of coprimes less than n)
; (totient 9) => 6
(totient (fn (n)
             (length (filter (fn (k) (coprime? k n)) (range 1 n)))))

; next-power-of-2: Find next power of 2 >= n
; (next-power-of-2 10) => 16
(next-power-of-2 (fix next-power-of-2
                      (fn (n)
                          (if (<= n 1) 1
                              (let ((helper (fix helper
                                                 (fn (p) (if (>= p n) p (helper (* p 2)))))))
                                   (helper 1))))))

; log2-int: Integer log base 2 (floor)
; (log2-int 1000) => 9
(log2-int (fix log2-int
               (fn (n)
                   (if (<= n 1) 0 (+ 1 (log2-int (/ n 2)))))))

; -- Digit Operations --

; digits: Get list of digits of a number
; (digits 1234) => (1 2 3 4)
(digits (fix digits
             (fn (n)
                 (if (< n 10)
                     (list n)
                     (append (digits (/ n 10)) (list (mod n 10)))))))

; from-digits: Convert list of digits to number
; (from-digits '(1 2 3 4)) => 1234
(from-digits (fn (ds)
                 (foldl (fn (acc d) (+ (* acc 10) d)) 0 ds)))

; digit-sum: Sum of digits
; (digit-sum 1234) => 10
(digit-sum (fn (n) (sum-list (digits n))))

; digit-count: Number of digits
; (digit-count 1234) => 4
(digit-count (fn (n) (length (digits n))))

; reverse-number: Reverse digits of a number
; (reverse-number 1234) => 4321
(reverse-number (fn (n) (from-digits (reverse (digits n)))))

; -- Figurate Numbers --

; triangular: Triangular number
(triangular (fn (n) (/ (* n (+ n 1)) 2)))

; square-number: Square number
(square-number (fn (n) (* n n)))

; pentagonal: Pentagonal number
(pentagonal (fn (n) (/ (* n (- (* 3 n) 1)) 2)))

; hexagonal: Hexagonal number
(hexagonal (fn (n) (* n (- (* 2 n) 1))))

; -- Sequence Generators --

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

; squares: Generate square numbers
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

; -- Iteration --

; iterate-n: Apply function n times, collecting results
(iterate-n (fn (f n x)
               (iterate f n x)))

; --- Module Exports ---
; (see exports.ss for exported symbols)
