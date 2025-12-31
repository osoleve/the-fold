; math-util.ss - Mathematical utility functions
; Part of The Fold prelude

; ============================================
; Number theory
; ============================================

; fibonacci: nth Fibonacci number
; (fibonacci 10) => 55
(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

; fibonacci-fast: Fast fibonacci using iteration
; (fibonacci-fast 10) => 55
(define (fibonacci-fast n)
  (let ((fib-iter (fn (count a b)
                    (if (<= count 0)
                        a
                        (fib-iter (- count 1) b (+ a b))))))
    (fib-iter n 0 1)))

; fibonacci-sequence: Generate first n Fibonacci numbers
; (fibonacci-sequence 5) => (0 1 1 2 3)
(define (fibonacci-sequence n)
  (if (<= n 0)
      '()
      (if (= n 1)
          '(0)
          (let ((builder (fn (count a b acc)
                          (if (>= count n)
                              (reverse acc)
                              (builder (+ count 1) b (+ a b) (cons a acc))))))
            (builder 0 0 1 '())))))

; gcd-list: GCD of a list of numbers
; (gcd-list '(12 18 24)) => 6
(define (gcd-list lst)
  (foldl gcd (car lst) (cdr lst)))

; lcm-list: LCM of a list of numbers
; (lcm-list '(4 6 8)) => 24
(define (lcm-list lst)
  (foldl lcm (car lst) (cdr lst)))

; ============================================
; Prime numbers
; ============================================

; prime?: Check if number is prime
; (prime? 17) => #t
(define (prime? n)
  (if (<= n 1)
      #f
      (if (<= n 3)
          #t
          (if (or (= 0 (remainder n 2)) (= 0 (remainder n 3)))
              #f
              (let ((check-divisor (fn (i)
                                    (if (> (* i i) n)
                                        #t
                                        (if (or (= 0 (remainder n i))
                                                (= 0 (remainder n (+ i 2))))
                                            #f
                                            (check-divisor (+ i 6)))))))
                (check-divisor 5))))))

; is-prime?: Alias for prime?
(define is-prime? prime?)

; ============================================
; Range and summation
; ============================================

; sum-range: Sum of integers from a to b (inclusive)
; (sum-range 1 10) => 55
(define (sum-range a b)
  (if (> a b)
      0
      (foldl + 0 (range a (+ b 1)))))

; product-range: Product of integers from a to b
; (product-range 1 5) => 120
(define (product-range a b)
  (if (> a b)
      1
      (foldl * 1 (range a (+ b 1)))))

; ============================================
; Factorization
; ============================================

; factors: Get all factors of n
; (factors 12) => (1 2 3 4 6 12)
(define (factors n)
  (filter (fn (i) (= 0 (remainder n i)))
          (range 1 (+ n 1))))

; prime-factors: Get prime factorization
; (prime-factors 12) => (2 2 3)
(define (prime-factors n)
  (let ((factor-helper (fn (n d acc)
                        (if (<= n 1)
                            (reverse acc)
                            (if (= 0 (remainder n d))
                                (factor-helper (/ n d) d (cons d acc))
                                (if (> (* d d) n)
                                    (reverse (cons n acc))
                                    (factor-helper n (+ d 1) acc)))))))
    (factor-helper n 2 '())))

; ============================================
; Module exports
; ============================================

(module-exports
  fibonacci
  fibonacci-fast
  fibonacci-sequence
  gcd-list
  lcm-list
  prime?
  is-prime?
  sum-range
  product-range
  factors
  prime-factors)
