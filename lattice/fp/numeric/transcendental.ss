;;; @module transcendental
;;; @requires prelude

(load "core/base/prelude.ss")

(doc 'module 'transcendental)
(doc 'description "Transcendental functions with support for high-precision computation")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Provides wrappers around Scheme's built-in functions plus
high-precision implementations for future BigNum optimization")

(doc 'dependencies "prelude.ss (for basic utilities)")

(doc 'section 'standard-transcendental-functions)

(doc 'note "These wrap Scheme's built-in transcendental functions
for consistency with our naming conventions and to provide
a foundation for future high-precision implementations")

(define (exp-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute e^x for any number")
  (exp x))

(define (log-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute natural logarithm of x")
  (doc 'note "Error if x <= 0")
  (if (<= x 0)
      (error 'log-num "logarithm undefined for non-positive numbers" x)
      (log x)))

(define (log-base b x)
  (doc 'type '(-> Number Number Number))
  (doc 'description "Compute logarithm of x in base b")
  (/ (log-num x) (log-num b)))

(define (log10 x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute base-10 logarithm")
  (log-base 10 x))

(define (log2 x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute base-2 logarithm")
  (log-base 2 x))

(doc 'section 'trigonometric-functions)

(define (sin-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute sine of x (in radians)")
  (sin x))

(define (cos-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute cosine of x (in radians)")
  (cos x))

(define (tan-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute tangent of x (in radians)")
  (tan x))

(define (asin-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute arcsine (inverse sine) of x")
  (doc 'domain "[-1, 1]")
  (doc 'range "[-π/2, π/2]")
  (if (or (< x -1) (> x 1))
      (error 'asin-num "domain error: asin requires -1 <= x <= 1" x)
      (asin x)))

(define (acos-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute arccosine (inverse cosine) of x")
  (doc 'domain "[-1, 1]")
  (doc 'range "[0, π]")
  (if (or (< x -1) (> x 1))
      (error 'acos-num "domain error: acos requires -1 <= x <= 1" x)
      (acos x)))

(define (atan-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute arctangent (inverse tangent) of x")
  (doc 'range "(-π/2, π/2)")
  (atan x))

(define (atan2-num y x)
  (doc 'type '(-> Number Number Number))
  (doc 'description "Compute atan(y/x) with correct quadrant")
  (doc 'range "(-π, π]")
  (atan y x))

(doc 'section 'hyperbolic-functions)

(define (sinh-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute hyperbolic sine: sinh(x) = (e^x - e^(-x)) / 2")
  (/ (- (exp x) (exp (- x))) 2))

(define (cosh-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute hyperbolic cosine: cosh(x) = (e^x + e^(-x)) / 2")
  (/ (+ (exp x) (exp (- x))) 2))

(define (tanh-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute hyperbolic tangent: tanh(x) = sinh(x) / cosh(x)")
  (/ (sinh-num x) (cosh-num x)))

(define (asinh-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute inverse hyperbolic sine: asinh(x) = log(x + sqrt(x^2 + 1))")
  (log (+ x (sqrt (+ (* x x) 1)))))

(define (acosh-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute inverse hyperbolic cosine: acosh(x) = log(x + sqrt(x^2 - 1))")
  (doc 'domain "[1, ∞)")
  (if (< x 1)
      (error 'acosh-num "domain error: acosh requires x >= 1" x)
      (log (+ x (sqrt (- (* x x) 1))))))

(define (atanh-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute inverse hyperbolic tangent: atanh(x) = log((1+x)/(1-x)) / 2")
  (doc 'domain "(-1, 1)")
  (if (or (<= x -1) (>= x 1))
      (error 'atanh-num "domain error: atanh requires -1 < x < 1" x)
      (/ (log (/ (+ 1 x) (- 1 x))) 2)))

(doc 'section 'power-root-functions)

(define (pow-num x y)
  (doc 'type '(-> Number Number Number))
  (doc 'description "Compute x^y")
  (expt x y))

(define (sqrt-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute square root of x")
  (doc 'note "Error if x < 0 for real numbers")
  (if (< x 0)
      (error 'sqrt-num "domain error: sqrt of negative number" x)
      (sqrt x)))

(define (cbrt-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute cube root of x")
  (if (< x 0)
      (- (expt (- x) (/ 1 3)))
      (expt x (/ 1 3))))

(doc 'section 'mathematical-constants)

(define (pi-value)
  (doc 'type '(-> Number))
  (doc 'description "Return value of π (pi)")
  (* 4 (atan 1)))

(define (e-value)
  (doc 'type '(-> Number))
  (doc 'description "Return value of e (Euler's number)")
  (exp 1))

(define (tau-value)
  (doc 'type '(-> Number))
  (doc 'description "Return value of τ (tau = 2π)")
  (* 2 (pi-value)))

(define (phi-value)
  (doc 'type '(-> Number))
  (doc 'description "Return value of φ (golden ratio) = (1 + sqrt(5)) / 2")
  (/ (+ 1 (sqrt 5)) 2))

(doc 'section 'degree-radian-conversion)

(define (deg->rad degrees)
  (doc 'type '(-> Number Number))
  (doc 'description "Convert degrees to radians")
  (* degrees (/ (pi-value) 180)))

(define (rad->deg radians)
  (doc 'type '(-> Number Number))
  (doc 'description "Convert radians to degrees")
  (* radians (/ 180 (pi-value))))

(doc 'section 'compound-functions)

(define (sec-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute secant: sec(x) = 1 / cos(x)")
  (/ 1 (cos x)))

(define (csc-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute cosecant: csc(x) = 1 / sin(x)")
  (let ([s (sin x)])
       (if (= s 0)
           (error 'csc-num "division by zero: csc(nπ) is undefined" x)
           (/ 1 s))))

(define (cot-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute cotangent: cot(x) = cos(x) / sin(x)")
  (let ([s (sin x)])
       (if (= s 0)
           (error 'cot-num "division by zero: cot(nπ) is undefined" x)
           (/ (cos x) s))))

(define (sech-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute hyperbolic secant: sech(x) = 1 / cosh(x)")
  (/ 1 (cosh-num x)))

(define (csch-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute hyperbolic cosecant: csch(x) = 1 / sinh(x)")
  (let ([s (sinh-num x)])
       (if (= s 0)
           (error 'csch-num "division by zero: csch(0) is undefined" x)
           (/ 1 s))))

(define (coth-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute hyperbolic cotangent: coth(x) = cosh(x) / sinh(x)")
  (let ([s (sinh-num x)])
       (if (= s 0)
           (error 'coth-num "division by zero: coth(0) is undefined" x)
           (/ (cosh-num x) s))))

(doc 'section 'special-functions)

(define (sinc-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute normalized sinc function: sinc(x) = sin(πx) / (πx)")
  (doc 'note "With sinc(0) = 1 by continuity")
  (if (= x 0)
      1
      (let ([px (* (pi-value) x)])
           (/ (sin px) px))))

(define (expm1-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute exp(x) - 1 accurately for small x")
  (doc 'note "Uses built-in when available, otherwise falls back to exp")
  (if (< (abs x) 1e-5)
      ;; For small x, use Taylor series: exp(x)-1 ≈ x + x²/2 + x³/6
      (+ x (* x x 0.5) (* x x x (/ 1 6)))
      (- (exp x) 1)))

(define (log1p-num x)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute log(1 + x) accurately for small x")
  (doc 'note "Uses built-in when available, otherwise falls back to log")
  (if (< (abs x) 1e-5)
      ;; For small x, use Taylor series: log(1+x) ≈ x - x²/2 + x³/3
      (- (+ x (* x x x (/ 1 3))) (* x x 0.5))
      (log (+ 1 x))))
