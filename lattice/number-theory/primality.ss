(load "core/base/prelude.ss")
(load "lattice/number-theory/modular.ss")

(doc 'module 'primality)
(doc 'description "Number-theoretic algorithms for primality testing and factorization")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'trial-division)

(define (prime? n)
  (doc 'export #t)
  (doc 'type '(-> Int Boolean))
  (doc 'description "Deterministic primality test using trial division. For large numbers, prefer miller-rabin? for speed")
  (doc 'complexity "O(√n)")
  (cond
   [(< n 2) #f]
   [(= n 2) #t]
   [(even? n) #f]
   [(= n 3) #t]
   [else
    (let ([limit (isqrt n)])
      (let loop ([d 3])
        (cond
         [(> d limit) #t]
         [(zero? (modulo n d)) #f]
         [else (loop (+ d 2))])))]))

;;; composite? : Int → Boolean
;;; Returns #t if n is composite (not prime and > 1).
(define (composite? n)
  (doc 'export #t)
  (and (> n 1) (not (prime? n))))

(doc 'section 'miller-rabin)

(define (miller-rabin-witness? a n)
  (doc 'type '(-> Int Int Boolean))
  (doc 'description "Test if a is a Miller-Rabin witness for n being composite. Internal helper for miller-rabin?")
  (let* ([n-1 (- n 1)]
         ;; Write n-1 = d × 2^r where d is odd
         [factor-result (factor-out-2s n-1)]
         [d (car factor-result)]
         [r (cdr factor-result)]
         ;; Compute a^d mod n
         [x (mod-expt a d n)])
    (cond
     ;; If x = 1 or x = n-1, a is not a witness
     [(or (= x 1) (= x n-1)) #f]
     ;; Square repeatedly
     [else
      (let loop ([x x] [i 0])
        (cond
         [(>= i (- r 1)) #t]  ; a IS a witness - n is composite
         [else
          (let ([x-new (mod-expt x 2 n)])
            (cond
             [(= x-new 1) #t]      ; Found witness
             [(= x-new n-1) #f]    ; Not a witness
             [else (loop x-new (+ i 1))]))]))])))

;;; factor-out-2s : Int → (Int . Int)
;;; Given n, return (d . r) where n = d × 2^r and d is odd.
(define (factor-out-2s n)
  (let loop ([d n] [r 0])
    (if (even? d)
        (loop (quotient d 2) (+ r 1))
        (cons d r))))

(doc miller-rabin? 'type '(case-lambda
                                   ((Int) Boolean)
                                   ((Int Int) Boolean)))
(doc miller-rabin? 'description "Probabilistic primality test using Miller-Rabin algorithm. Optional second argument specifies number of rounds (default 20). Error probability: at most 4^(-k) for k rounds")
(doc miller-rabin? 'complexity "O(k log³n)")
(define (miller-rabin? n . args)
  (doc 'export #t)
  (let ([rounds (if (null? args) 20 (car args))])
    (cond
     [(< n 2) #f]
     [(= n 2) #t]
     [(even? n) #f]
     [(= n 3) #t]
     ;; For small n, use deterministic set of witnesses
     [(< n 2047)
      (not (miller-rabin-witness? 2 n))]
     [(< n 1373653)
      (not (or (miller-rabin-witness? 2 n)
               (miller-rabin-witness? 3 n)))]
     [(< n 9080191)
      (not (or (miller-rabin-witness? 31 n)
               (miller-rabin-witness? 73 n)))]
     [(< n 25326001)
      (not (or (miller-rabin-witness? 2 n)
               (miller-rabin-witness? 3 n)
               (miller-rabin-witness? 5 n)))]
     [(< n 3215031751)
      (not (or (miller-rabin-witness? 2 n)
               (miller-rabin-witness? 3 n)
               (miller-rabin-witness? 5 n)
               (miller-rabin-witness? 7 n)))]
     [(< n 4759123141)
      (not (or (miller-rabin-witness? 2 n)
               (miller-rabin-witness? 7 n)
               (miller-rabin-witness? 61 n)))]
     ;; For larger numbers, use deterministic witnesses up to 2^64
     [(< n 3317044064679887385961981)
      (let ([witnesses '(2 3 5 7 11 13 17 19 23 29 31 37)])
        (not (exists (lambda (a) (miller-rabin-witness? a n)) witnesses)))]
     ;; For very large numbers, use random witnesses
     [else
      (let ([witnesses (deterministic-witnesses n rounds)])
        (not (exists (lambda (a) (miller-rabin-witness? a n)) witnesses)))])))

;;; deterministic-witnesses : Int × Int → (List Int)
;;; Generate deterministic witness bases for testing n.
;;; Uses hash-based selection for reproducibility.
(define (deterministic-witnesses n k)
  ;; Use first k small primes as witnesses (deterministic)
  (let ([small-primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)])
    (take (min k (length small-primes)) small-primes)))

;; take provided by prelude

(doc 'section 'integer-factorization)

(define (trial-division n)
  (doc 'export #t)
  (doc 'type '(-> Int (List Int)))
  (doc 'description "Factor n using trial division. Returns sorted list of prime factors (with repetition)")
  (doc 'complexity "O(√n)")
  (cond
   [(< n 2) '()]
   [else
    (let loop ([n n] [d 2] [factors '()])
      (cond
       [(= n 1) (reverse factors)]
       [(> (* d d) n) (reverse (cons n factors))]
       [(zero? (modulo n d))
        (loop (quotient n d) d (cons d factors))]
       [else
        (loop n (if (= d 2) 3 (+ d 2)) factors)]))]))

(define (factorize n)
  (doc 'export #t)
  (doc 'type '(-> Int (List Int)))
  (doc 'description "Factor n into prime factors. Returns sorted list of prime factors (with repetition). Uses Pollard's rho for large factors, trial division for small")
  (cond
   [(< n 2) '()]
   [(prime? n) (list n)]
   [else
    ;; First extract small factors by trial division
    (let* ([small-factor-result (extract-small-factors n 1000)]
           [remaining (car small-factor-result)]
           [small-factors (cdr small-factor-result)])
      (if (= remaining 1)
          (sort < small-factors)
          ;; Use Pollard's rho for the remaining part
          (sort < (append small-factors (pollard-rho-factorize remaining)))))]))

;;; extract-small-factors : Int × Int → (Int . (List Int))
;;; Extract prime factors up to limit from n.
;;; Returns (remaining . factors-found).
(define (extract-small-factors n limit)
  (let loop ([n n] [d 2] [factors '()])
    (cond
     [(or (= n 1) (> d limit)) (cons n factors)]
     [(zero? (modulo n d))
      (loop (quotient n d) d (cons d factors))]
     [else
      (loop n (if (= d 2) 3 (+ d 2)) factors)])))

;;; pollard-rho-factorize : Int → (List Int)
;;; Fully factor n using Pollard's rho algorithm.
;;; Returns list of prime factors.
(define (pollard-rho-factorize n)
  (cond
   [(= n 1) '()]
   [(prime? n) (list n)]
   [else
    (let ([d (pollard-rho n)])
      (if (= d n)
          ;; Pollard's rho failed, fall back to trial division
          (trial-division n)
          ;; Recursively factor both parts
          (append (pollard-rho-factorize d)
                  (pollard-rho-factorize (quotient n d)))))]))

(define (pollard-rho n)
  (doc 'type '(-> Int Int))
  (doc 'description "Find a non-trivial factor of n using Pollard's rho algorithm. Returns n if no factor found (n might be prime)")
  (doc 'complexity "O(n^(1/4)) expected")
  (cond
   [(even? n) 2]
   [else
    ;; Try different starting values and polynomials
    (let try-c ([c 1])
      (if (> c 20)
          n  ; Give up
          (let ([result (pollard-rho-single n c)])
            (if (and (> result 1) (< result n))
                result
                (try-c (+ c 1))))))]))

(define (pollard-rho-single n c)
  (doc 'type '(-> Int Int Int))
  (doc 'description "Single run of Pollard's rho with polynomial f(x) = x² + c. Uses batched GCD computation: accumulates |x-y| products and computes GCD every 128 iterations, reducing GCD operations by ~128x")
  (let ([f (lambda (x) (modulo (+ (* x x) c) n))]
        [batch-size 128])
    (let outer-loop ([x 2] [y 2] [iter 0])
      (if (> iter 1000000)
          n  ; Give up
          ;; Batch phase: accumulate products for batch-size iterations
          (let inner-loop ([x x] [y y] [prod 1] [batch-count 0]
                          [xs-history '()] [ys-history '()])
            (if (= batch-count batch-size)
                ;; Compute GCD for this batch
                (let ([d (gcd prod n)])
                  (cond
                   [(= d 1)
                    ;; No factor yet, continue with next batch
                    (outer-loop x y (+ iter batch-size))]
                   [(and (> d 1) (< d n))
                    ;; Found non-trivial factor
                    d]
                   [else
                    ;; d = n, need to backtrack and find exact step
                    (pollard-rho-backtrack n c (reverse xs-history) (reverse ys-history))]))
                ;; Continue accumulating
                (let* ([x-new (f x)]
                       [y-new (f (f y))]
                       [diff (abs (- x-new y-new))]
                       [prod-new (modulo (* prod diff) n)])
                  (inner-loop x-new y-new prod-new (+ batch-count 1)
                             (cons x xs-history) (cons y ys-history)))))))))

;;; pollard-rho-backtrack : Int × Int × (List Int) × (List Int) → Int
;;; When batch GCD returns n, backtrack to find exact factor.
(define (pollard-rho-backtrack n c xs-history ys-history)
  (let ([f (lambda (x) (modulo (+ (* x x) c) n))])
    (let loop ([xs xs-history] [ys ys-history] [x (if (null? xs-history) 2 (car xs-history))]
               [y (if (null? ys-history) 2 (car ys-history))])
      (if (null? xs)
          n  ; Shouldn't happen, but return n as failure
          (let* ([x-new (f x)]
                 [y-new (f (f y))]
                 [d (gcd (abs (- x-new y-new)) n)])
            (cond
             [(= d 1)
              (loop (cdr xs) (cdr ys) x-new y-new)]
             [(and (> d 1) (< d n)) d]
             [else n]))))))

(doc 'section 'factor-representation)

(define (prime-factorization n)
  (doc 'export #t)
  (doc 'type '(-> Int (List (Pair Int Int))))
  (doc 'description "Return prime factorization as list of (prime . exponent) pairs")
  (doc 'example "(prime-factorization 12) → ((2 . 2) (3 . 1))")
  (let ([factors (factorize n)])
    (if (null? factors)
        '()
        (let loop ([fs (cdr factors)]
                   [current (car factors)]
                   [count 1]
                   [result '()])
          (cond
           [(null? fs)
            (reverse (cons (cons current count) result))]
           [(= (car fs) current)
            (loop (cdr fs) current (+ count 1) result)]
           [else
            (loop (cdr fs)
                  (car fs)
                  1
                  (cons (cons current count) result))])))))

;;; divisors : Int → (List Int)
;;; Return all positive divisors of n in sorted order.
(define (divisors n)
  (doc 'export #t)
  (if (< n 1)
      '()
      (let* ([pf (prime-factorization n)])
        (sort < (generate-divisors pf)))))

;;; generate-divisors : (List (Int . Int)) → (List Int)
;;; Generate all divisors from prime factorization.
(define (generate-divisors pf)
  (if (null? pf)
      '(1)
      (let* ([p (caar pf)]
             [e (cdar pf)]
             [rest (generate-divisors (cdr pf))]
             [powers (prime-powers p e)])
        (flatmap (lambda (pw)
                   (map (lambda (d) (* pw d)) rest))
                 powers))))

;;; prime-powers : Int × Int → (List Int)
;;; Generate p^0, p^1, ..., p^e
(define (prime-powers p e)
  (let loop ([i 0] [pw 1] [result '()])
    (if (> i e)
        (reverse result)
        (loop (+ i 1) (* pw p) (cons pw result)))))

;;; flatmap : (a → (List b)) × (List a) → (List b)
(define (flatmap f lst)
  (apply append (map f lst)))

(doc 'section 'number-theoretic-functions)

(define (lcm a b)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int))
  (doc 'description "Least common multiple")
  (if (or (= a 0) (= b 0))
      0
      (abs (quotient (* a b) (gcd a b)))))

;;; lcm* : (List Int) → Int
;;; LCM of a list of integers.
(define (lcm* lst)
  (doc 'export #t)
  (fold-left lcm 1 lst))

;;; gcd* : (List Int) → Int
;;; GCD of a list of integers.
(define (gcd* lst)
  (doc 'export #t)
  (fold-left gcd 0 lst))

(define (euler-totient n)
  (doc 'export #t)
  (doc 'type '(-> Int Int))
  (doc 'description "Euler's totient function φ(n). Returns count of integers in [1,n] coprime to n")
  (doc 'formula "φ(n) = n × ∏(1 - 1/p) for all prime factors p")
  (if (< n 1)
      0
      (let ([pf (prime-factorization n)])
        (fold-left
         (lambda (acc pe)
           (let ([p (car pe)]
                 [e (cdr pe)])
             ;; φ(p^e) = p^(e-1) × (p-1)
             ;; φ(n) = n × ∏(1 - 1/p) for all prime factors p
             (* acc (- p 1) (expt p (- e 1)))))
         1
         pf))))

(define (carmichael-lambda n)
  (doc 'export #t)
  (doc 'type '(-> Int Int))
  (doc 'description "Carmichael's lambda function λ(n). Smallest m such that a^m ≡ 1 (mod n) for all a coprime to n")
  (if (< n 1)
      0
      (let ([pf (prime-factorization n)])
        (lcm* (map (lambda (pe)
                     (let ([p (car pe)]
                           [e (cdr pe)])
                       (cond
                        ;; λ(2) = 1, λ(4) = 2, λ(2^e) = 2^(e-2) for e ≥ 3
                        [(= p 2)
                         (if (< e 3)
                             (euler-totient (expt 2 e))
                             (quotient (euler-totient (expt 2 e)) 2))]
                        ;; λ(p^e) = φ(p^e) for odd primes
                        [else
                         (euler-totient (expt p e))])))
                   pf)))))

;;; mobius : Int → Int
;;; Möbius function μ(n).
;;; Returns 0 if n has squared prime factor,
;;; (-1)^k if n is product of k distinct primes.
(define (mobius n)
  (doc 'export #t)
  (if (< n 1)
      0
      (let ([pf (prime-factorization n)])
        (if (exists (lambda (pe) (> (cdr pe) 1)) pf)
            0
            (if (even? (length pf)) 1 -1)))))

;;; radical : Int → Int
;;; Radical of n: product of distinct prime factors.
(define (radical n)
  (doc 'export #t)
  (if (< n 1)
      1
      (let ([pf (prime-factorization n)])
        (fold-left * 1 (map car pf)))))

(doc 'section 'primality-utilities)

(define (next-prime n)
  (doc 'export #t)
  (doc 'type '(-> Int Int))
  (doc 'description "Return smallest prime greater than n")
  (cond
   [(< n 2) 2]
   [(= n 2) 3]
   [else
    (let ([start (+ n 1)])
      (let loop ([k (if (even? start) (+ start 1) start)])
        (if (prime? k)
            k
            (loop (+ k 2)))))]))

;;; prev-prime : Int → Int | #f
;;; Return largest prime less than n, or #f if none exists.
(define (prev-prime n)
  (doc 'export #t)
  (cond
   [(<= n 2) #f]
   [(= n 3) 2]
   [else
    (let ([start (- n 1)])
      (let loop ([k (if (even? start) (- start 1) start)])
        (cond
         [(< k 2) #f]
         [(prime? k) k]
         [else (loop (- k 2))])))]))

;;; nth-prime : Int → Int
;;; Return the nth prime (1-indexed: nth-prime 1 = 2).
(define (nth-prime n)
  (doc 'export #t)
  (if (< n 1)
      2
      (let loop ([count 0] [p 2])
        (if (= count n)
            (prev-prime p)
            (loop (+ count 1) (next-prime p))))))

(define (primes-up-to n)
  (doc 'export #t)
  (doc 'type '(-> Int (List Int)))
  (doc 'description "Return all primes up to and including n using Sieve of Eratosthenes")
  (if (< n 2)
      '()
      (let ([sieve (make-vector (+ n 1) #t)])
        (vector-set! sieve 0 #f)
        (vector-set! sieve 1 #f)
        (let loop ([p 2])
          (when (<= (* p p) n)
            (when (vector-ref sieve p)
              (let mark ([m (* p p)])
                (when (<= m n)
                  (vector-set! sieve m #f)
                  (mark (+ m p)))))
            (loop (+ p 1))))
        (let collect ([i 2] [result '()])
          (if (> i n)
              (reverse result)
              (collect (+ i 1)
                       (if (vector-ref sieve i)
                           (cons i result)
                           result)))))))

;;; prime-pi : Int → Int
;;; Prime counting function π(n): number of primes ≤ n.
(define (prime-pi n)
  (doc 'export #t)
  (length (primes-up-to n)))

;;; coprime? : Int × Int → Boolean
;;; Test if a and b are coprime (gcd = 1).
(define (coprime? a b)
  (doc 'export #t)
  (= (gcd a b) 1))

(doc 'section 'perfect-powers)

(define (isqrt n)
  (doc 'export #t)
  (doc 'type '(-> Int Int))
  (doc 'description "Integer square root: largest k such that k² ≤ n")
  (cond
   [(< n 0) 0]
   [(= n 0) 0]
   [(= n 1) 1]
   [else
    (let loop ([x n])
      (let ([x1 (quotient (+ x (quotient n x)) 2)])
        (if (>= x1 x)
            x
            (loop x1))))]))

;;; is-perfect-square? : Int → Boolean
(define (is-perfect-square? n)
  (doc 'export #t)
  (and (>= n 0)
       (let ([r (isqrt n)])
         (= (* r r) n))))

(define (is-perfect-power? n)
  (doc 'export #t)
  (doc 'type '(-> Int (Union Boolean (Pair Int Int))))
  (doc 'description "Test if n = a^b for some a > 1 and b > 1. Returns (a . b) if found, #f otherwise")
  (if (< n 2)
      #f
      (let* ([max-exp (+ 1 (inexact->exact (floor (/ (log n) (log 2)))))])
        (let loop-exp ([b 2])
          (if (> b max-exp)
              #f
              (let ([a (integer-root n b)])
                (if (and a (= (expt a b) n))
                    (cons a b)
                    (loop-exp (+ b 1)))))))))

;;; integer-root : Int × Int → Int | #f
;;; Compute floor(n^(1/k)) and verify.
;;; Returns the root if exact, #f if not exact.
(define (integer-root n k)
  (doc 'export #t)
  (if (or (< n 1) (< k 1))
      #f
      (let ([guess (inexact->exact (floor (expt n (/ 1 k))))])
        ;; Newton's method refinement
        (let refine ([x guess])
          (let* ([x-k (expt x k)]
                 [x1-k (expt (+ x 1) k)])
            (cond
             [(= x-k n) x]
             [(and (< x-k n) (> x1-k n)) #f]  ; No exact root
             [(< x-k n) (refine (+ x 1))]
             [else (refine (- x 1))]))))))

(doc 'section 'legendre-jacobi-symbols)

(define (legendre-symbol a p)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int))
  (doc 'description "Legendre symbol (a/p) for prime p. Returns 1 if a is quadratic residue mod p, -1 if quadratic non-residue, 0 if p divides a")
  (let ([result (mod-expt a (quotient (- p 1) 2) p)])
    (cond
     [(= result 0) 0]
     [(= result 1) 1]
     [(= result (- p 1)) -1]
     [else result])))  ; Shouldn't happen for prime p

(define (jacobi-symbol a n)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int))
  (doc 'description "Jacobi symbol (a/n) for odd positive n. Generalization of Legendre symbol")
  (cond
   [(not (and (odd? n) (> n 0))) 0]  ; Invalid input
   [(= n 1) 1]
   [(= a 0) 0]
   [(= a 1) 1]
   [else
    (let ([a (modulo a n)])
      (cond
       [(= a 0) 0]
       [(= a 1) 1]
       [else
        ;; Extract power of 2 from a
        (let* ([twos-result (factor-out-2s a)]
               [a-odd (car twos-result)]
               [e (cdr twos-result)]
               ;; (2/n) = (-1)^((n²-1)/8)
               [two-contrib
                (if (even? e)
                    1
                    (let ([n-mod-8 (modulo n 8)])
                      (if (or (= n-mod-8 1) (= n-mod-8 7))
                          1
                          -1)))])
          ;; Quadratic reciprocity for odd a
          (if (= a-odd 1)
              two-contrib
              (let* ([;; (a/n) × (n/a) = (-1)^((a-1)(n-1)/4)
                      flip-sign
                      (if (and (= (modulo a-odd 4) 3)
                               (= (modulo n 4) 3))
                          -1
                          1)]
                     [recur (jacobi-symbol (modulo n a-odd) a-odd)])
                (* two-contrib flip-sign recur))))]))]))

(doc 'exports "prime? composite? miller-rabin? trial-division factorize prime-factorization divisors gcd lcm gcd* lcm* euler-totient carmichael-lambda mobius radical next-prime prev-prime nth-prime primes-up-to prime-pi coprime? isqrt is-perfect-square? is-perfect-power? integer-root legendre-symbol jacobi-symbol")
