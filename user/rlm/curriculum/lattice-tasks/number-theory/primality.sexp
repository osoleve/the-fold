;;; primality.sexp — Development tasks for lattice/number-theory/primality.ss
;;;
;;; Module: primality (tier 0, depends on prelude, sort, modular)
;;; 585 lines, ~26 exported functions
;;; Primality testing (trial division, Miller-Rabin), factorization
;;; (trial division, Pollard's rho), number-theoretic functions
;;; (totient, Mobius, divisors, Jacobi symbol).
;;;
;;; Git history:
;;;   64e5274c feat(numeric): Add primality/factorization and interpolation modules
;;;   dd151c40 fix: sort-shadowing, lens navigator bugs
;;;   b803b1e9 refactor: Replace opaque apply-append-map with transparent append-map

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement prime? (trial division)
  ;; ──────────────────────────────────────────────
  ;; The most fundamental primality test. O(sqrt(n)).

  (task
    (id "number-theory/primality/prime-impl")
    (module primality)
    (tier 0)
    (type implement)
    (description "Implement prime?: deterministic primality test using trial division. A number n is prime if n >= 2 and has no divisors other than 1 and itself. Handle edge cases first: n < 2 is not prime, 2 is prime, even numbers > 2 are not prime, 3 is prime. For odd n >= 5, check divisibility by all odd numbers d from 3 up to isqrt(n). If any d divides n evenly, it's composite. If none do, it's prime. The key optimization: only test odd divisors, and stop at sqrt(n).")
    (context "Available: prelude (<, =, even?, zero?, modulo, +, >), isqrt (integer square root — largest k where k*k <= n). No other modules needed.")
    (before "(define (prime? n)
  (doc 'type '(-> Int Boolean))
  (doc 'description \"Deterministic primality test using trial division\")
  ;; TODO: handle small cases, then check odd divisors up to sqrt(n)
  #f)")
    (test-file "lattice/number-theory/test-primality.ss")
    (test-forms (";; Edge cases
(assert-false (prime? 0))
(assert-false (prime? 1))
(assert-true (prime? 2))
(assert-true (prime? 3))"
                 ";; Small primes
(assert-true (prime? 5))
(assert-true (prime? 7))
(assert-true (prime? 11))
(assert-true (prime? 29))"
                 ";; Small composites
(assert-false (prime? 4))
(assert-false (prime? 9))
(assert-false (prime? 15))
(assert-false (prime? 25))"
                 ";; Larger values
(assert-true (prime? 97))
(assert-true (prime? 997))
(assert-false (prime? 100))
(assert-false (prime? 1001))"))
    (available-modules (prelude modular))
    (hints ("Start with cond for the small cases: (< n 2) → #f, (= n 2) → #t, (even? n) → #f, (= n 3) → #t"
            "Compute limit = (isqrt n)"
            "Named let loop from d=3: if (> d limit) → #t (prime!)"
            "If (zero? (modulo n d)) → #f (composite!)"
            "Otherwise recurse with (+ d 2) — only test odd divisors"))
    (reference-solution "(define (prime? n)
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
         [else (loop (+ d 2))])))])))")
    (alternatives ())
    (source-commit "64e5274c")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement factor-out-2s
  ;; ──────────────────────────────────────────────
  ;; Foundation for Miller-Rabin. Write n = d * 2^r.

  (task
    (id "number-theory/primality/factor-out-2s-impl")
    (module primality)
    (tier 0)
    (type implement)
    (description "Implement factor-out-2s: given an integer n, decompose it as n = d * 2^r where d is odd. Return the result as a pair (d . r). Repeatedly divide by 2 and increment r until the result is odd. For example: 12 = 3 * 2^2 so return (3 . 2). 24 = 3 * 2^3 so return (3 . 3). If n is already odd, return (n . 0). This decomposition is fundamental to Miller-Rabin primality testing.")
    (context "Available: prelude (even?, quotient, +, cons). This is a pure arithmetic function with no dependencies.")
    (before "(define (factor-out-2s n)
  (doc 'type '(-> Int (Pair Int Int)))
  (doc 'description \"Decompose n = d * 2^r where d is odd. Returns (d . r)\")
  ;; TODO: repeatedly divide by 2 until odd
  (cons n 0))")
    (test-file "lattice/number-theory/test-primality.ss")
    (test-forms (";; 12 = 3 * 2^2
(assert-equal '(3 . 2) (factor-out-2s 12))"
                 ";; 8 = 1 * 2^3
(assert-equal '(1 . 3) (factor-out-2s 8))"
                 ";; 7 is already odd
(assert-equal '(7 . 0) (factor-out-2s 7))"
                 ";; 24 = 3 * 2^3
(assert-equal '(3 . 3) (factor-out-2s 24))"
                 ";; 1 is odd
(assert-equal '(1 . 0) (factor-out-2s 1))"))
    (available-modules (prelude))
    (hints ("Named let with two accumulators: d (starts at n) and r (starts at 0)"
            "While d is even: d = (quotient d 2), r = (+ r 1)"
            "When d is odd: return (cons d r)"))
    (reference-solution "(define (factor-out-2s n)
  (let loop ([d n] [r 0])
    (if (even? d)
        (loop (quotient d 2) (+ r 1))
        (cons d r))))")
    (alternatives ())
    (source-commit "64e5274c")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement trial-division
  ;; ──────────────────────────────────────────────
  ;; Factorization by trial division. O(sqrt(n)).

  (task
    (id "number-theory/primality/trial-division-impl")
    (module primality)
    (tier 0)
    (type implement)
    (description "Implement trial-division: factor an integer n into its prime factors using trial division. Return a sorted list of prime factors with repetition. For n < 2, return empty list. Otherwise, try dividing by d starting at 2. If d divides n, add d to factors and continue dividing n by d (to handle repeated factors). If d doesn't divide n, advance to the next candidate: if d=2, try 3; otherwise try d+2 (skip evens). Stop when d*d > n; if n > 1 at that point, n itself is the remaining prime factor.")
    (context "Available: prelude (<, =, >, *, zero?, modulo, quotient, +, cons, reverse, if). No other modules needed.")
    (before "(define (trial-division n)
  (doc 'type '(-> Int (List Int)))
  (doc 'description \"Factor n using trial division. Returns sorted list of prime factors with repetition\")
  ;; TODO: divide by 2, then odd divisors, collect factors
  '())")
    (test-file "lattice/number-theory/test-primality.ss")
    (test-forms ("(assert-equal '() (trial-division 1))"
                 "(assert-equal '(2) (trial-division 2))"
                 "(assert-equal '(2 3) (trial-division 6))"
                 "(assert-equal '(2 2 3) (trial-division 12))"
                 "(assert-equal '(2 2 5 5) (trial-division 100))"
                 "(assert-equal '(2 2 2 3 3 5) (trial-division 360))"
                 "(assert-equal '(7 11 13) (trial-division 1001))"))
    (available-modules (prelude))
    (hints ("Base case: (< n 2) → '()"
            "Named let with n, d=2, and factors='()"
            "If n=1, return (reverse factors) — done"
            "If d*d > n and n > 1, return (reverse (cons n factors)) — n is the last prime factor"
            "If (zero? (modulo n d)), recurse with (quotient n d), same d, (cons d factors)"
            "Otherwise advance d: (if (= d 2) 3 (+ d 2))"))
    (reference-solution "(define (trial-division n)
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
        (loop n (if (= d 2) 3 (+ d 2)) factors)]))]))")
    (alternatives ())
    (source-commit "64e5274c")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement isqrt
  ;; ──────────────────────────────────────────────
  ;; Integer square root via Newton's method.

  (task
    (id "number-theory/primality/isqrt-impl")
    (module primality)
    (tier 0)
    (type implement)
    (description "Implement isqrt: compute the integer square root of n, which is the largest integer k such that k*k <= n. Use Newton's method (Heron's method) for integer square root. Start with x = n. At each step, compute x1 = floor((x + floor(n/x)) / 2). If x1 >= x, we've converged — return x. Otherwise, continue with x1. Handle edge cases: n < 0 returns 0, n = 0 returns 0, n = 1 returns 1. This converges in O(log log n) steps and is exact for integers.")
    (context "Available: prelude (<, =, >=, +, quotient). Use quotient for integer division (floor division for positive integers).")
    (before "(define (isqrt n)
  (doc 'type '(-> Int Int))
  (doc 'description \"Integer square root: largest k such that k^2 <= n\")
  ;; TODO: Newton's method for integer square root
  0)")
    (test-file "lattice/number-theory/test-primality.ss")
    (test-forms ("(assert-equal 0 (isqrt 0))"
                 "(assert-equal 1 (isqrt 1))"
                 "(assert-equal 2 (isqrt 4))"
                 "(assert-equal 3 (isqrt 10))"
                 "(assert-equal 10 (isqrt 100))"
                 "(assert-equal 10 (isqrt 101))"
                 ";; Verify: result squared <= n < (result+1) squared
(let ([r (isqrt 50)])
  (assert-true (and (<= (* r r) 50) (> (* (+ r 1) (+ r 1)) 50))))"))
    (available-modules (prelude))
    (hints ("Handle base cases: n < 0 → 0, n = 0 → 0, n = 1 → 1"
            "Named let starting with x = n"
            "Compute x1 = (quotient (+ x (quotient n x)) 2)"
            "If x1 >= x, return x (converged)"
            "Otherwise loop with x1"
            "This is Newton's method applied to f(x) = x^2 - n"))
    (reference-solution "(define (isqrt n)
  (cond
   [(< n 0) 0]
   [(= n 0) 0]
   [(= n 1) 1]
   [else
    (let loop ([x n])
      (let ([x1 (quotient (+ x (quotient n x)) 2)])
        (if (>= x1 x)
            x
            (loop x1))))]))")
    (alternatives ())
    (source-commit "64e5274c")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement euler-totient
  ;; ──────────────────────────────────────────────
  ;; Euler's totient from prime factorization. Multiplicative.

  (task
    (id "number-theory/primality/euler-totient-impl")
    (module primality)
    (tier 0)
    (type implement)
    (description "Implement euler-totient: compute Euler's totient function phi(n), which counts the integers in [1,n] that are coprime to n. The formula using prime factorization is: phi(n) = product over all prime-power factors p^e of (p-1) * p^(e-1). For example: phi(12) = phi(2^2 * 3^1) = (2-1)*2^1 * (3-1)*3^0 = 2 * 2 = 4. Use prime-factorization to get the list of (prime . exponent) pairs, then fold over them multiplying the contributions. For n < 1, return 0.")
    (context "Available: prelude (fold-left, *, -, <, car, cdr, expt), prime-factorization (returns list of (prime . exponent) pairs for n). Requires the primality module to be loaded.")
    (before "(define (euler-totient n)
  (doc 'type '(-> Int Int))
  (doc 'description \"Euler's totient phi(n): count of integers in [1,n] coprime to n\")
  ;; TODO: product of (p-1)*p^(e-1) over prime factorization
  0)")
    (test-file "lattice/number-theory/test-primality.ss")
    (test-forms ("(assert-equal 1 (euler-totient 1))"
                 "(assert-equal 1 (euler-totient 2))"
                 "(assert-equal 2 (euler-totient 6))"
                 "(assert-equal 4 (euler-totient 10))"
                 "(assert-equal 4 (euler-totient 12))"
                 ";; phi(p) = p-1 for prime p
(assert-equal 6 (euler-totient 7))
(assert-equal 12 (euler-totient 13))"
                 ";; phi(p^k) = p^(k-1) * (p-1)
(assert-equal 4 (euler-totient 8))
(assert-equal 18 (euler-totient 27))"))
    (available-modules (prelude sort modular primality))
    (hints ("If (< n 1), return 0"
            "Get the factorization: (let ([pf (prime-factorization n)]) ...)"
            "fold-left over pf with initial accumulator 1"
            "For each pair pe: p = (car pe), e = (cdr pe)"
            "Contribution: (* (- p 1) (expt p (- e 1)))"
            "Multiply into accumulator: (* acc contribution)"))
    (reference-solution "(define (euler-totient n)
  (if (< n 1)
      0
      (let ([pf (prime-factorization n)])
        (fold-left
         (lambda (acc pe)
           (let ([p (car pe)]
                 [e (cdr pe)])
             (* acc (- p 1) (expt p (- e 1)))))
         1
         pf))))")
    (alternatives ())
    (source-commit "64e5274c")
    (difficulty 3))

) ;; end tasks
