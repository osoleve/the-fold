; ============================================================
; Mathematical Utility Functions
; Number theory, primes, and factorization
; ============================================================

; fib: nth Fibonacci number (recursive)
(fib (fix fib
         (fn (n)
             (if (<= n 1)
                 n
                 (+ (fib (- n 1)) (fib (- n 2)))))))

; fib-fast: Fast fibonacci using iteration
(fib-fast (fn (n)
             (let ((fib-iter (fix fib-iter
                                 (fn (count a b)
                                     (if (<= count 0)
                                         a
                                         (fib-iter (- count 1) b (+ a b)))))))
                  (fib-iter n 0 1))))

; fib-seq: Generate first n Fibonacci numbers
(fib-seq (fn (n)
            (if (<= n 0)
                '()
                (if (= n 1)
                    '(0)
                    (let ((builder (fix builder
                                       (fn (count a b acc)
                                           (if (>= count n)
                                               (reverse acc)
                                               (builder (+ count 1) b (+ a b) (cons a acc)))))))
                         (builder 0 0 1 '()))))))

; gcd-list: GCD of a list of numbers
(gcd-list (fn (lst)
             (foldl gcd (car lst) (cdr lst))))

; lcm-list: LCM of a list of numbers
(lcm-list (fn (lst)
             (foldl lcm (car lst) (cdr lst))))

; is-prime?: Check if number is prime
(is-prime? (fn (n)
              (if (<= n 1)
                  #f
                  (if (<= n 3)
                      #t
                      (if (or (= 0 (remainder n 2)) (= 0 (remainder n 3)))
                          #f
                          (let ((check (fix check
                                           (fn (i)
                                               (if (> (* i i) n)
                                                   #t
                                                   (if (or (= 0 (remainder n i))
                                                           (= 0 (remainder n (+ i 2))))
                                                       #f
                                                       (check (+ i 6))))))))
                               (check 5)))))))

; sum-range: Sum of integers from a to b
(sum-range (fn (a b)
              (if (> a b)
                  0
                  (foldl + 0 (range a (+ b 1))))))

; product-range: Product of integers from a to b
(product-range (fn (a b)
                  (if (> a b)
                      1
                      (foldl * 1 (range a (+ b 1))))))

; factors: Get all factors of n
(factors-of (fn (n)
               (filter (fn (i) (= 0 (remainder n i)))
                       (range 1 (+ n 1)))))

; prime-factors: Get prime factorization
(prime-factors (fn (n)
                  (let ((factor-helper (fix factor-helper
                                           (fn (n d acc)
                                               (if (<= n 1)
                                                   (reverse acc)
                                                   (if (= 0 (remainder n d))
                                                       (factor-helper (/ n d) d (cons d acc))
                                                       (if (> (* d d) n)
                                                           (reverse (cons n acc))
                                                           (factor-helper n (+ d 1) acc))))))))
                       (factor-helper n 2 '()))))
