((name "numeric")
 (description "Numerical computing primitives including transcendental functions
  and special mathematical functions. All implementations are pure Scheme
  with no external dependencies.")

 (modules
  ((file "transcendental.ss")
   (purpose "Elementary transcendental functions")
   (exports
    (pi-value "The constant pi (3.14159...)")
    (e-value "Euler's number e (2.71828...)")
    (fold-exp "Exponential function e^x")
    (fold-log "Natural logarithm ln(x)")
    (fold-log10 "Base-10 logarithm")
    (fold-log2 "Base-2 logarithm")
    (fold-pow "Power function x^y")
    (fold-sqrt "Square root")
    (fold-cbrt "Cube root")
    (fold-sin "Sine (radians)")
    (fold-cos "Cosine (radians)")
    (fold-tan "Tangent (radians)")
    (fold-asin "Arcsine")
    (fold-acos "Arccosine")
    (fold-atan "Arctangent")
    (fold-atan2 "Two-argument arctangent")
    (fold-sinh "Hyperbolic sine")
    (fold-cosh "Hyperbolic cosine")
    (fold-tanh "Hyperbolic tangent")
    (fold-asinh "Inverse hyperbolic sine")
    (fold-acosh "Inverse hyperbolic cosine")
    (fold-atanh "Inverse hyperbolic tangent")
    (deg->rad "Convert degrees to radians")
    (rad->deg "Convert radians to degrees"))
   (example
    ";; Trigonometric identity: sin^2 + cos^2 = 1
     (let ((x 0.7))
       (+ (expt (fold-sin x) 2)
          (expt (fold-cos x) 2)))
     ;; => 1.0 (within floating-point tolerance)

     ;; Natural logarithm is inverse of exp
     (fold-log (fold-exp 2.5))
     ;; => 2.5"))

  ((file "special-functions.ss")
   (purpose "Special mathematical functions")
   (exports
    ;; Error function family
    (erf "Error function erf(x)")
    (erfc "Complementary error function 1 - erf(x)")
    (erfinv "Inverse error function")
    ;; Gamma function family
    (gamma "Gamma function (generalized factorial)")
    (lgamma "Log-gamma function ln(Gamma(x))")
    (digamma "Digamma function (derivative of lgamma)")
    (beta "Beta function B(a,b) = Gamma(a)Gamma(b)/Gamma(a+b)")
    (lbeta "Log-beta function")
    (incomplete-gamma "Lower incomplete gamma function")
    (incomplete-gamma-upper "Upper incomplete gamma function")
    (regularized-gamma-p "Regularized lower incomplete gamma P(a,x)")
    (regularized-gamma-q "Regularized upper incomplete gamma Q(a,x)")
    (incomplete-beta "Incomplete beta function")
    (regularized-beta "Regularized incomplete beta I_x(a,b)")
    ;; Combinatorics
    (factorial "n! factorial")
    (double-factorial "n!! double factorial")
    (binomial "Binomial coefficient C(n,k)")
    (multinomial "Multinomial coefficient")
    (falling-factorial "Falling factorial (n)_k")
    (rising-factorial "Rising factorial (Pochhammer symbol)")
    ;; Bessel functions
    (bessel-j0 "Bessel function J_0(x)")
    (bessel-j1 "Bessel function J_1(x)")
    (bessel-jn "Bessel function J_n(x) for integer n")
    ;; Zeta and polylogarithm
    (zeta "Riemann zeta function")
    (polylog "Polylogarithm Li_s(z)"))
   (example
    ";; Factorial via gamma: n! = Gamma(n+1)
     (gamma 6)  ;; => 120.0 (= 5!)

     ;; Error function for normal distribution CDF
     (define (normal-cdf x)
       (* 0.5 (+ 1 (erf (/ x (sqrt 2))))))
     (normal-cdf 0)    ;; => 0.5
     (normal-cdf 1.96) ;; => ~0.975

     ;; Binomial coefficient
     (binomial 10 3)  ;; => 120")))

 (dependencies
  ("core/base/prelude.ss" "Base utilities"))

 (notes
  "All functions are implemented using series expansions, continued
   fractions, or asymptotic approximations as appropriate for the
   input domain. No FFI or external libraries are used.

   The transcendental functions use Taylor series with argument
   reduction for accuracy. Special functions use carefully chosen
   algorithms for numerical stability.

   Test suites: transcendental.ss (59 tests), special-functions.ss (extensive)."))
