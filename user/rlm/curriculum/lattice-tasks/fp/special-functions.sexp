;;; special-functions.sexp — Development tasks for lattice/fp/numeric/special-functions.ss
;;;
;;; Module: special-functions (tier 0, depends on prelude)
;;; 377 lines, ~20 exported functions
;;; Standard special functions: error function (erf/erfc), gamma, log-gamma,
;;; beta, digamma, trigamma, incomplete gamma/beta, binomial coefficients,
;;; Pochhammer symbol, double factorial.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement factorial
  ;; ──────────────────────────────────────────────
  ;; n! via the gamma function — connecting discrete and continuous.

  (task
    (id "fp/special-functions/factorial-impl")
    (module special-functions)
    (tier 0)
    (type implement)
    (description "Implement factorial: compute n! = n * (n-1) * ... * 1 using the gamma function. The key identity is Gamma(n+1) = n! for non-negative integers. For integer input, compute gamma(n+1) and round to exact integer using inexact->exact and round. For non-integer input, just return gamma(n+1) (the continuous extension). Error on negative integers. The gamma function gives us factorial 'for free' — but we need to convert back to exact integers for integer inputs.")
    (context "Available: gamma (Lanczos approximation), <, =, truncate, +, inexact->exact, round, error, cond. Dispatch on input type.")
    (before "(define (factorial n)
  ;; TODO: n! via gamma(n+1)
  1)")
    (test-file "lattice/fp/numeric/test-special-functions.ss")
    (test-forms (";; Basic factorials
(assert-equal 1 (factorial 0))
(assert-equal 1 (factorial 1))
(assert-equal 120 (factorial 5))
(assert-equal 3628800 (factorial 10))"
                 ";; Large factorial
(assert-equal 2432902008176640000 (factorial 20))"))
    (available-modules (prelude))
    (hints ("Guard: (< n 0) → error"
            "Check integer: (= n (truncate n))"
            "If integer: (inexact->exact (round (gamma (+ n 1))))"
            "Else: (gamma (+ n 1))"))
    (reference-solution "(define (factorial n)
  (cond
   ((< n 0) (error 'factorial \"undefined for negative integers\" n))
   ((= n (truncate n))
    (inexact->exact (round (gamma (+ n 1)))))
   (else (gamma (+ n 1)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement double-factorial
  ;; ──────────────────────────────────────────────
  ;; n!! = n(n-2)(n-4)... — skipping every other factor.

  (task
    (id "fp/special-functions/double-factorial-impl")
    (module special-functions)
    (tier 0)
    (type implement)
    (description "Implement double-factorial: compute n!! = n * (n-2) * (n-4) * ... down to 1 or 2. For odd n: n!! = n * (n-2) * ... * 3 * 1. For even n: n!! = n * (n-2) * ... * 4 * 2. Base cases: 0!! = 1, 1!! = 1. Error on negative integers. This appears in combinatorics and the volume of n-spheres. Use simple recursion: n!! = n * (n-2)!!.")
    (context "Available: <, <=, *, -, error, cond. Direct recursion with step -2.")
    (before "(define (double-factorial n)
  ;; TODO: n!! = n(n-2)(n-4)...
  1)")
    (test-file "lattice/fp/numeric/test-special-functions.ss")
    (test-forms (";; Odd double factorials
(assert-equal 1 (double-factorial 1))
(assert-equal 15 (double-factorial 5))
(assert-equal 105 (double-factorial 7))"
                 ";; Even double factorials
(assert-equal 2 (double-factorial 2))
(assert-equal 8 (double-factorial 4))
(assert-equal 48 (double-factorial 6))"
                 ";; Base case
(assert-equal 1 (double-factorial 0))"))
    (available-modules (prelude))
    (hints ("Base: n < 0 → error"
            "Base: n <= 1 → 1"
            "Recursive: (* n (double-factorial (- n 2)))"))
    (reference-solution "(define (double-factorial n)
  (cond
   ((< n 0) (error 'double-factorial \"undefined for negative integers\" n))
   ((<= n 1) 1)
   (else (* n (double-factorial (- n 2))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement binomial
  ;; ──────────────────────────────────────────────
  ;; C(n,k) via log-gamma — numerically stable for large n.

  (task
    (id "fp/special-functions/binomial-impl")
    (module special-functions)
    (tier 0)
    (type implement)
    (description "Implement binomial: compute the binomial coefficient C(n,k) = n! / (k! * (n-k)!). For numerical stability with large values, use the log-gamma identity: log(C(n,k)) = lgamma(n+1) - lgamma(k+1) - lgamma(n-k+1), then exponentiate and round to exact integer. Edge cases: k < 0 or k > n → 0, k = 0 or k = n → 1. The lgamma approach avoids integer overflow that direct factorial computation would cause for large n.")
    (context "Available: lgamma (log of gamma function), <, >, =, or, -, +, exp, inexact->exact, round, cond. One formula with edge cases.")
    (before "(define (binomial n k)
  ;; TODO: C(n,k) via lgamma
  0)")
    (test-file "lattice/fp/numeric/test-special-functions.ss")
    (test-forms (";; Standard values
(assert-equal 10 (binomial 5 2))
(assert-equal 1 (binomial 5 0))
(assert-equal 1 (binomial 5 5))
(assert-equal 252 (binomial 10 5))"
                 ";; Edge cases
(assert-equal 0 (binomial 5 -1))
(assert-equal 0 (binomial 5 6))"))
    (available-modules (prelude))
    (hints ("Guard: k < 0 → 0, k > n → 0"
            "Guard: k = 0 or k = n → 1"
            "Compute: (- (lgamma (+ n 1)) (lgamma (+ k 1)) (lgamma (+ (- n k) 1)))"
            "Return: (inexact->exact (round (exp ...)))"))
    (reference-solution "(define (binomial n k)
  (cond
   ((< k 0) 0)
   ((> k n) 0)
   ((or (= k 0) (= k n)) 1)
   (else
    (inexact->exact
     (round (exp (- (lgamma (+ n 1))
                    (lgamma (+ k 1))
                    (lgamma (+ (- n k) 1)))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement beta
  ;; ──────────────────────────────────────────────
  ;; B(a,b) = Gamma(a)*Gamma(b)/Gamma(a+b) — via lgamma for stability.

  (task
    (id "fp/special-functions/beta-impl")
    (module special-functions)
    (tier 0)
    (type implement)
    (description "Implement beta: compute the Beta function B(a,b) = Gamma(a)*Gamma(b)/Gamma(a+b). For numerical stability, compute in log space: log(B(a,b)) = lgamma(a) + lgamma(b) - lgamma(a+b), then exponentiate. The Beta function is the normalizing constant of the Beta distribution and appears throughout Bayesian statistics. It satisfies B(a,b) = B(b,a) (symmetry) and B(1,1) = 1. Error if either argument is non-positive.")
    (context "Available: lgamma, exp, +, -, or, <=, error. Single formula in log-space.")
    (before "(define (beta a b)
  ;; TODO: B(a,b) via lgamma
  0)")
    (test-file "lattice/fp/numeric/test-special-functions.ss")
    (test-forms (";; B(2,3) = 1/12
(assert-true (< (abs (- (beta 2 3) 1/12)) 0.001))"
                 ";; B(1,1) = 1
(assert-true (< (abs (- (beta 1 1) 1.0)) 0.001))"
                 ";; Symmetry: B(a,b) = B(b,a)
(assert-true (< (abs (- (beta 3 5) (beta 5 3))) 0.001))"))
    (available-modules (prelude))
    (hints ("Guard: a <= 0 or b <= 0 → error"
            "Compute log-B: (- (+ (lgamma a) (lgamma b)) (lgamma (+ a b)))"
            "Return: (exp log-B)"))
    (reference-solution "(define (beta a b)
  (if (or (<= a 0) (<= b 0))
      (error 'beta \"requires positive arguments\" (list a b))
      (exp (- (+ (lgamma a) (lgamma b))
              (lgamma (+ a b))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement erfc
  ;; ──────────────────────────────────────────────
  ;; Complementary error function — numerically stable for large x.

  (task
    (id "fp/special-functions/erfc-impl")
    (module special-functions)
    (tier 0)
    (type implement)
    (description "Implement erfc: compute the complementary error function erfc(x) = 1 - erf(x). For negative x, use the identity erfc(-x) = 2 - erfc(x). For non-negative x, use the Abramowitz & Stegun rational approximation (7.1.26): erfc(x) = t * (a1 + t*(a2 + t*(a3 + t*(a4 + t*a5)))) * exp(-x^2), where t = 1/(1 + p*x) with p = 0.3275911 and coefficients a1=0.254829592, a2=-0.284496736, a3=1.421413741, a4=-1.453152027, a5=1.061405429. Computing erfc directly (not as 1-erf) avoids catastrophic cancellation for large x where erf(x) is very close to 1.")
    (context "Available: <, -, *, +, /, abs, exp. Horner's method polynomial evaluation.")
    (before "(define (erfc x)
  ;; TODO: complementary error function
  1)")
    (test-file "lattice/fp/numeric/test-special-functions.ss")
    (test-forms (";; erfc(0) = 1
(assert-true (< (abs (- (erfc 0) 1.0)) 0.001))"
                 ";; erfc(1) ≈ 0.1573
(assert-true (< (abs (- (erfc 1) 0.1573)) 0.001))"
                 ";; erfc(-x) = 2 - erfc(x)
(assert-true (< (abs (- (erfc -1) (- 2 (erfc 1)))) 0.001))"
                 ";; Large x: erfc approaches 0
(assert-true (< (erfc 3) 0.001))"))
    (available-modules (prelude))
    (hints ("For x < 0: (- 2 (erfc (- x)))"
            "For x >= 0: Horner evaluation"
            "t = (/ 1.0 (+ 1.0 (* 0.3275911 x)))"
            "y = (* t (+ a1 (* t (+ a2 (* t (+ a3 (* t (+ a4 (* t a5)))))))))"
            "Result: (* y (exp (- (* x x))))"))
    (reference-solution "(define (erfc x)
  (if (< x 0)
      (- 2 (erfc (- x)))
      (let* ((p 0.3275911)
             (t (/ 1.0 (+ 1.0 (* p x))))
             (a1 0.254829592)
             (a2 -0.284496736)
             (a3 1.421413741)
             (a4 -1.453152027)
             (a5 1.061405429)
             (y (* t (+ a1 (* t (+ a2 (* t (+ a3 (* t (+ a4 (* t a5)))))))))))
        (* y (exp (- (* x x)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
