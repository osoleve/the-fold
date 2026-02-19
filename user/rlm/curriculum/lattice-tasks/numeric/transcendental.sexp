;;; transcendental.sexp — Development tasks for lattice/fp/numeric/transcendental.ss
;;;
;;; Module: transcendental (tier 0, depends on prelude)
;;; 262 lines, ~30 exported functions
;;; Transcendental functions: trig, hyperbolic, inverse hyperbolic,
;;; degree/radian conversion, mathematical constants, numerically-stable
;;; special functions (sinc, expm1, log1p).
;;;
;;; Git history:
;;;   f91aee12 refactor: Three-layer architecture (core/lattice/shell)
;;;   71b6b156 feat(module): migrate lattice/fp/ leaves to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement log-base
  ;; ──────────────────────────────────────────────
  ;; Change of base formula — foundational identity.

  (task
    (id "numeric/transcendental/log-base-impl")
    (module transcendental)
    (tier 0)
    (type implement)
    (description "Implement log-base: compute the logarithm of x in an arbitrary base b. Use the change of base formula: log_b(x) = ln(x) / ln(b), where ln is the natural logarithm. Call log-num (which computes ln and validates that its argument is positive) for both numerator and denominator. For example: log-base(2, 8) = 3, log-base(10, 100) = 2, log-base(10, 1) = 0.")
    (context "Available: log-num (natural logarithm with domain check for positive numbers), /. Pure arithmetic.")
    (before "(define (log-base b x)
  (doc 'type '(-> Number Number Number))
  (doc 'description \"Logarithm of x in base b\")
  ;; TODO: change of base formula
  0)")
    (test-file "lattice/fp/numeric/test-transcendental.ss")
    (test-forms (";; log2(8) = 3
(assert-true (< (abs (- (log-base 2 8) 3)) 1e-10))"
                 ";; log10(100) = 2
(assert-true (< (abs (- (log-base 10 100) 2)) 1e-10))"
                 ";; log_b(1) = 0 for any base
(assert-true (< (abs (log-base 7 1)) 1e-10))"
                 ";; log_b(b) = 1 for any base
(assert-true (< (abs (- (log-base 5 5) 1)) 1e-10))"))
    (available-modules (prelude))
    (hints ("The change of base formula: log_b(x) = log(x) / log(b)"
            "Use log-num for both: (/ (log-num x) (log-num b))"
            "That's the entire implementation — one line"))
    (reference-solution "(define (log-base b x)
  (/ (log-num x) (log-num b)))")
    (alternatives ())
    (source-commit "f91aee12")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement sinh-num
  ;; ──────────────────────────────────────────────
  ;; Hyperbolic sine from exponentials.

  (task
    (id "numeric/transcendental/sinh-impl")
    (module transcendental)
    (tier 0)
    (type implement)
    (description "Implement sinh-num: compute the hyperbolic sine of x. The formula is: sinh(x) = (e^x - e^(-x)) / 2. This is analogous to how sine relates to the unit circle, but sinh relates to the unit hyperbola. Use Scheme's built-in exp function to compute e^x and e^(-x). Properties: sinh(0) = 0, sinh is an odd function (sinh(-x) = -sinh(x)), and for large x, sinh(x) ≈ e^x/2.")
    (context "Available: exp (e^x), +, -, /, *. Pure arithmetic.")
    (before "(define (sinh-num x)
  (doc 'type '(-> Number Number))
  (doc 'description \"Hyperbolic sine: sinh(x) = (e^x - e^(-x)) / 2\")
  ;; TODO: compute from exponentials
  0)")
    (test-file "lattice/fp/numeric/test-transcendental.ss")
    (test-forms (";; sinh(0) = 0
(assert-true (< (abs (sinh-num 0)) 1e-10))"
                 ";; sinh(1) ≈ 1.1752
(assert-true (< (abs (- (sinh-num 1) 1.1752)) 0.001))"
                 ";; Odd function: sinh(-x) = -sinh(x)
(assert-true (< (abs (+ (sinh-num 2) (sinh-num -2))) 1e-10))"
                 ";; Large x: sinh(10) ≈ e^10/2
(assert-true (< (abs (- (sinh-num 10) (/ (exp 10) 2))) 1))"))
    (available-modules (prelude))
    (hints ("Compute e^x = (exp x) and e^(-x) = (exp (- x))"
            "Result = (/ (- e^x e^(-x)) 2)"
            "This is the definition of hyperbolic sine"))
    (reference-solution "(define (sinh-num x)
  (/ (- (exp x) (exp (- x))) 2))")
    (alternatives ())
    (source-commit "f91aee12")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement asinh-num
  ;; ──────────────────────────────────────────────
  ;; Inverse hyperbolic sine via logarithm identity.

  (task
    (id "numeric/transcendental/asinh-impl")
    (module transcendental)
    (tier 0)
    (type implement)
    (description "Implement asinh-num: compute the inverse hyperbolic sine of x. The closed-form formula is: asinh(x) = ln(x + sqrt(x^2 + 1)). Unlike arcsin (which is restricted to [-1,1]), asinh is defined for all real numbers because sinh maps R onto R. Properties: asinh(0) = 0, asinh is an odd function, and for large x, asinh(x) ≈ ln(2x).")
    (context "Available: log (natural logarithm), sqrt (square root), +, *. Pure arithmetic.")
    (before "(define (asinh-num x)
  (doc 'type '(-> Number Number))
  (doc 'description \"Inverse hyperbolic sine: asinh(x) = log(x + sqrt(x^2 + 1))\")
  ;; TODO: closed-form via log and sqrt
  0)")
    (test-file "lattice/fp/numeric/test-transcendental.ss")
    (test-forms (";; asinh(0) = 0
(assert-true (< (abs (asinh-num 0)) 1e-10))"
                 ";; asinh(1) ≈ 0.8814
(assert-true (< (abs (- (asinh-num 1) 0.8814)) 0.001))"
                 ";; Round-trip: sinh(asinh(x)) = x
(assert-true (< (abs (- (sinh-num (asinh-num 3.7)) 3.7)) 1e-10))"
                 ";; Odd function: asinh(-x) = -asinh(x)
(assert-true (< (abs (+ (asinh-num 5) (asinh-num -5))) 1e-10))"))
    (available-modules (prelude))
    (hints ("The formula: (log (+ x (sqrt (+ (* x x) 1))))"
            "Note: x^2 + 1 is always positive, so sqrt is always valid"
            "And x + sqrt(x^2+1) is always positive, so log is valid too"))
    (reference-solution "(define (asinh-num x)
  (log (+ x (sqrt (+ (* x x) 1)))))")
    (alternatives ())
    (source-commit "f91aee12")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement sinc-num
  ;; ──────────────────────────────────────────────
  ;; Normalized sinc with the removable singularity at 0.

  (task
    (id "numeric/transcendental/sinc-impl")
    (module transcendental)
    (tier 0)
    (type implement)
    (description "Implement sinc-num: compute the normalized sinc function, defined as sinc(x) = sin(pi*x) / (pi*x) for x != 0, and sinc(0) = 1 by continuity (L'Hopital's rule gives lim(x→0) sin(πx)/(πx) = 1). The sinc function appears throughout signal processing: it is the Fourier transform of the rectangular window, the ideal low-pass filter impulse response, and the interpolation kernel for Shannon sampling. Handle the x=0 case explicitly to avoid division by zero.")
    (context "Available: (pi-value) (returns π), sin (sine function), =, *, /. Pure arithmetic with a special case.")
    (before "(define (sinc-num x)
  (doc 'type '(-> Number Number))
  (doc 'description \"Normalized sinc: sinc(x) = sin(πx) / (πx), sinc(0) = 1\")
  ;; TODO: handle x=0 separately, then sin(pi*x)/(pi*x)
  0)")
    (test-file "lattice/fp/numeric/test-transcendental.ss")
    (test-forms (";; sinc(0) = 1 by continuity
(assert-true (< (abs (- (sinc-num 0) 1)) 1e-10))"
                 ";; sinc(1) = sin(π)/π = 0
(assert-true (< (abs (sinc-num 1)) 1e-10))"
                 ";; sinc(0.5) = sin(π/2)/(π/2) = 2/π
(assert-true (< (abs (- (sinc-num 0.5) (/ 2 (pi-value)))) 1e-10))"
                 ";; sinc(-x) = sinc(x) (even function)
(assert-true (< (abs (- (sinc-num 0.3) (sinc-num -0.3))) 1e-10))"))
    (available-modules (prelude))
    (hints ("Special case: (if (= x 0) 1 ...)"
            "General case: compute px = (* (pi-value) x)"
            "Then return (/ (sin px) px)"
            "Use (pi-value) to get π, not a hardcoded approximation"))
    (reference-solution "(define (sinc-num x)
  (if (= x 0)
      1
      (let ([px (* (pi-value) x)])
        (/ (sin px) px))))")
    (alternatives ())
    (source-commit "f91aee12")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement expm1-num
  ;; ──────────────────────────────────────────────
  ;; Numerically stable exp(x)-1 using Taylor series for small x.

  (task
    (id "numeric/transcendental/expm1-impl")
    (module transcendental)
    (tier 0)
    (type implement)
    (description "Implement expm1-num: compute exp(x) - 1 accurately for small x. For small values of x (|x| < 1e-5), computing exp(x) - 1 directly loses precision because exp(x) ≈ 1 and subtracting 1 cancels significant digits (catastrophic cancellation). Instead, use the Taylor series: exp(x) - 1 ≈ x + x^2/2 + x^3/6 for small x. For larger |x|, the direct computation (- (exp x) 1) is fine. This pattern of using a Taylor series near a singularity and switching to the direct formula elsewhere is fundamental to numerical computing.")
    (context "Available: exp, abs, <, -, +, *, /. Pure arithmetic with a threshold-based branch.")
    (before "(define (expm1-num x)
  (doc 'type '(-> Number Number))
  (doc 'description \"Compute exp(x)-1 accurately for small x\")
  ;; TODO: Taylor series for |x| < 1e-5, direct computation otherwise
  0)")
    (test-file "lattice/fp/numeric/test-transcendental.ss")
    (test-forms (";; expm1(0) = 0
(assert-true (< (abs (expm1-num 0)) 1e-15))"
                 ";; expm1(1) = e - 1 ≈ 1.71828
(assert-true (< (abs (- (expm1-num 1) (- (exp 1) 1))) 1e-10))"
                 ";; Small x: expm1(1e-8) ≈ 1e-8 (tests Taylor branch)
(assert-true (< (abs (- (expm1-num 1e-8) 1e-8)) 1e-14))"
                 ";; Negative small x: expm1(-1e-8) ≈ -1e-8
(assert-true (< (abs (- (expm1-num -1e-8) -1e-8)) 1e-14))"))
    (available-modules (prelude))
    (hints ("Branch on (< (abs x) 1e-5)"
            "Small x: x + x^2/2 + x^3/6 = (+ x (* x x 0.5) (* x x x (/ 1 6)))"
            "Large x: (- (exp x) 1)"
            "The Taylor series is the first three terms of exp(x) - 1 = x + x^2/2! + x^3/3! + ..."))
    (reference-solution "(define (expm1-num x)
  (if (< (abs x) 1e-5)
      (+ x (* x x 0.5) (* x x x (/ 1 6)))
      (- (exp x) 1)))")
    (alternatives ())
    (source-commit "f91aee12")
    (difficulty 3))

) ;; end tasks
