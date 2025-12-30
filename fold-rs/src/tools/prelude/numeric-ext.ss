; ============================================================
; Extended Numeric Functions
; Number theory and arithmetic utilities
; Canonical versions - no duplicates
; ============================================================

; divisors: Get all divisors of n
; (divisors 12) => (1 2 3 4 6 12)
(divisors (fn (n)
              (filter (fn (d) (= 0 (mod n d)))
                      (range 1 (+ n 1)))))

; perfect?: Check if number equals sum of proper divisors
; (perfect? 6) => #t (1+2+3=6)
(perfect? (fn (n)
              (= n (- (sum-list (divisors n)) n))))

; primes-up-to: Generate all primes up to n (Sieve of Eratosthenes)
; (primes-up-to 20) => (2 3 5 7 11 13 17 19)
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

; coprime?: Check if two numbers are coprime (gcd = 1)
; (coprime? 15 28) => #t
(coprime? (fn (a b) (= (gcd a b) 1)))

; totient: Euler's totient function (count of coprimes less than n)
; (totient 9) => 6
(totient (fn (n)
             (length (filter (fn (k) (coprime? k n)) (iota n 1)))))

; is-power-of-2?: Check if n is a power of 2
; (is-power-of-2? 16) => #t
(is-power-of-2? (fn (n)
                    (and (> n 0) (= (mod n 2) 0) (or (= n 1) (is-power-of-2? (/ n 2))))))

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

; binomial: Binomial coefficient (n choose k)
; (binomial 5 2) => 10
(binomial (fn (n k)
              (if (or (< k 0) (> k n))
                  0
                  (if (or (= k 0) (= k n))
                      1
                      (/ (factorial n) (* (factorial k) (factorial (- n k))))))))

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

; palindrome-number?: Check if number is a palindrome
; (palindrome-number? 12321) => #t
(palindrome-number? (fn (n) (= n (reverse-number n))))
