;;; fabric/stitches/fp/transcendental.ss — Transcendental Functions
;;;
;;; Transcendental functions with support for high-precision computation.
;;; Provides wrappers around Scheme's built-in functions plus
;;; high-precision implementations for future BigNum optimization.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss (for basic utilities)

;;; ====
;;; Standard Transcendental Functions
;;; ====

;;; These wrap Scheme's built-in transcendental functions
;;; for consistency with our naming conventions and to provide
;;; a foundation for future high-precision implementations.

;;; exp-num : Number → Number
;;; Compute e^x for any number.
(define (exp-num x)
  (exp x))

;;; log-num : Number → Number
;;; Compute natural logarithm of x.
;;; Error if x <= 0.
(define (log-num x)
  (if (<= x 0)
      (error 'log-num "logarithm undefined for non-positive numbers" x)
      (log x)))

;;; log-base : Number × Number → Number
;;; Compute logarithm of x in base b.
(define (log-base b x)
  (/ (log-num x) (log-num b)))

;;; log10 : Number → Number
;;; Compute base-10 logarithm.
(define (log10 x)
  (log-base 10 x))

;;; log2 : Number → Number
;;; Compute base-2 logarithm.
(define (log2 x)
  (log-base 2 x))

;;; ====
;;; Trigonometric Functions
;;; ====

;;; sin-num : Number → Number
;;; Compute sine of x (in radians).
(define (sin-num x)
  (sin x))

;;; cos-num : Number → Number
;;; Compute cosine of x (in radians).
(define (cos-num x)
  (cos x))

;;; tan-num : Number → Number
;;; Compute tangent of x (in radians).
(define (tan-num x)
  (tan x))

;;; asin-num : Number → Number
;;; Compute arcsine (inverse sine) of x.
;;; Domain: [-1, 1], Range: [-π/2, π/2]
(define (asin-num x)
  (if (or (< x -1) (> x 1))
      (error 'asin-num "domain error: asin requires -1 <= x <= 1" x)
      (asin x)))

;;; acos-num : Number → Number
;;; Compute arccosine (inverse cosine) of x.
;;; Domain: [-1, 1], Range: [0, π]
(define (acos-num x)
  (if (or (< x -1) (> x 1))
      (error 'acos-num "domain error: acos requires -1 <= x <= 1" x)
      (acos x)))

;;; atan-num : Number → Number
;;; Compute arctangent (inverse tangent) of x.
;;; Range: (-π/2, π/2)
(define (atan-num x)
  (atan x))

;;; atan2-num : Number × Number → Number
;;; Compute atan(y/x) with correct quadrant.
;;; Range: (-π, π]
(define (atan2-num y x)
  (atan y x))

;;; ====
;;; Hyperbolic Functions
;;; ====

;;; sinh-num : Number → Number
;;; Compute hyperbolic sine: sinh(x) = (e^x - e^(-x)) / 2
(define (sinh-num x)
  (/ (- (exp x) (exp (- x))) 2))

;;; cosh-num : Number → Number
;;; Compute hyperbolic cosine: cosh(x) = (e^x + e^(-x)) / 2
(define (cosh-num x)
  (/ (+ (exp x) (exp (- x))) 2))

;;; tanh-num : Number → Number
;;; Compute hyperbolic tangent: tanh(x) = sinh(x) / cosh(x)
(define (tanh-num x)
  (/ (sinh-num x) (cosh-num x)))

;;; asinh-num : Number → Number
;;; Compute inverse hyperbolic sine: asinh(x) = log(x + sqrt(x^2 + 1))
(define (asinh-num x)
  (log (+ x (sqrt (+ (* x x) 1)))))

;;; acosh-num : Number → Number
;;; Compute inverse hyperbolic cosine: acosh(x) = log(x + sqrt(x^2 - 1))
;;; Domain: [1, ∞)
(define (acosh-num x)
  (if (< x 1)
      (error 'acosh-num "domain error: acosh requires x >= 1" x)
      (log (+ x (sqrt (- (* x x) 1))))))

;;; atanh-num : Number → Number
;;; Compute inverse hyperbolic tangent: atanh(x) = log((1+x)/(1-x)) / 2
;;; Domain: (-1, 1)
(define (atanh-num x)
  (if (or (<= x -1) (>= x 1))
      (error 'atanh-num "domain error: atanh requires -1 < x < 1" x)
      (/ (log (/ (+ 1 x) (- 1 x))) 2)))

;;; ====
;;; Power and Root Functions
;;; ====

;;; pow-num : Number × Number → Number
;;; Compute x^y.
(define (pow-num x y)
  (expt x y))

;;; sqrt-num : Number → Number
;;; Compute square root of x.
;;; Error if x < 0 for real numbers.
(define (sqrt-num x)
  (if (< x 0)
      (error 'sqrt-num "domain error: sqrt of negative number" x)
      (sqrt x)))

;;; cbrt-num : Number → Number
;;; Compute cube root of x.
(define (cbrt-num x)
  (if (< x 0)
      (- (expt (- x) (/ 1 3)))
      (expt x (/ 1 3))))

;;; ====
;;; Mathematical Constants
;;; ====

;;; pi-value : → Number
;;; Return value of π (pi).
(define (pi-value)
  (* 4 (atan 1)))

;;; e-value : → Number
;;; Return value of e (Euler's number).
(define (e-value)
  (exp 1))

;;; tau-value : → Number
;;; Return value of τ (tau = 2π).
(define (tau-value)
  (* 2 (pi-value)))

;;; phi-value : → Number
;;; Return value of φ (golden ratio) = (1 + sqrt(5)) / 2.
(define (phi-value)
  (/ (+ 1 (sqrt 5)) 2))

;;; ====
;;; Degree/Radian Conversion
;;; ====

;;; deg->rad : Number → Number
;;; Convert degrees to radians.
(define (deg->rad degrees)
  (* degrees (/ (pi-value) 180)))

;;; rad->deg : Number → Number
;;; Convert radians to degrees.
(define (rad->deg radians)
  (* radians (/ 180 (pi-value))))

;;; ====
;;; Compound Functions
;;; ====

;;; sec-num : Number → Number
;;; Compute secant: sec(x) = 1 / cos(x)
(define (sec-num x)
  (/ 1 (cos x)))

;;; csc-num : Number → Number
;;; Compute cosecant: csc(x) = 1 / sin(x)
(define (csc-num x)
  (let ([s (sin x)])
       (if (= s 0)
           (error 'csc-num "division by zero: csc(nπ) is undefined" x)
           (/ 1 s))))

;;; cot-num : Number → Number
;;; Compute cotangent: cot(x) = cos(x) / sin(x)
(define (cot-num x)
  (let ([s (sin x)])
       (if (= s 0)
           (error 'cot-num "division by zero: cot(nπ) is undefined" x)
           (/ (cos x) s))))

;;; sech-num : Number → Number
;;; Compute hyperbolic secant: sech(x) = 1 / cosh(x)
(define (sech-num x)
  (/ 1 (cosh-num x)))

;;; csch-num : Number → Number
;;; Compute hyperbolic cosecant: csch(x) = 1 / sinh(x)
(define (csch-num x)
  (let ([s (sinh-num x)])
       (if (= s 0)
           (error 'csch-num "division by zero: csch(0) is undefined" x)
           (/ 1 s))))

;;; coth-num : Number → Number
;;; Compute hyperbolic cotangent: coth(x) = cosh(x) / sinh(x)
(define (coth-num x)
  (let ([s (sinh-num x)])
       (if (= s 0)
           (error 'coth-num "division by zero: coth(0) is undefined" x)
           (/ (cosh-num x) s))))

;;; ====
;;; Special Functions
;;; ====

;;; sinc-num : Number → Number
;;; Compute normalized sinc function: sinc(x) = sin(πx) / (πx)
;;; With sinc(0) = 1 by continuity.
(define (sinc-num x)
  (if (= x 0)
      1
      (let ([px (* (pi-value) x)])
           (/ (sin px) px))))

;;; expm1-num : Number → Number
;;; Compute exp(x) - 1 accurately for small x.
;;; Uses built-in when available, otherwise falls back to exp.
(define (expm1-num x)
  (if (< (abs x) 1e-5)
      ;; For small x, use Taylor series: exp(x)-1 ≈ x + x²/2 + x³/6
      (+ x (* x x 0.5) (* x x x (/ 1 6)))
      (- (exp x) 1)))

;;; log1p-num : Number → Number
;;; Compute log(1 + x) accurately for small x.
;;; Uses built-in when available, otherwise falls back to log.
(define (log1p-num x)
  (if (< (abs x) 1e-5)
      ;; For small x, use Taylor series: log(1+x) ≈ x - x²/2 + x³/3
      (- (+ x (* x x x (/ 1 3))) (* x x 0.5))
      (log (+ 1 x))))
