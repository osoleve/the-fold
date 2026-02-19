;;; digital-filters.sexp — Development tasks for lattice/numeric/digital-filters.ss
;;;
;;; Module: digital-filters (tier 0, depends on prelude, complex, dft, convolution, vec)
;;; 729 lines, ~40 exported functions
;;; Digital filter library: window functions, FIR filter design via windowing,
;;; IIR filter structures, biquad sections, Butterworth and Chebyshev Type I
;;; filter design via bilinear transform, frequency response analysis,
;;; real-time filtering, common presets (dc-blocker, moving average).
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement sinc
  ;; ──────────────────────────────────────────────
  ;; Normalized sinc function.

  (task
    (id "numeric/digital-filters/sinc-impl")
    (module digital-filters)
    (tier 0)
    (type implement)
    (description "Implement sinc: the normalized sinc function, defined as sin(pi*x)/(pi*x) with sinc(0) = 1. This is the ideal lowpass filter impulse response and the foundation of FIR filter design via the windowing method. The function is discontinuous at x=0 in its ratio form, but has the well-known limit of 1. Handle x near zero (|x| < 1e-10) as a special case returning 1.0. For all other x, compute pi*x, then sin(pi*x)/(pi*x).")
    (context "Available: abs, <, *, /, sin, pi-value, if, let. Special case + formula.")
    (before "(define (sinc x)
  ;; TODO: sin(pi*x)/(pi*x), with sinc(0) = 1
  0)")
    (test-file "lattice/numeric/test-digital-filters.ss")
    (test-forms (";; sinc(0) = 1
(assert-true (< (abs (- (sinc 0) 1.0)) 1e-10))"
                 ";; sinc(1) = 0 (sin(pi)/pi)
(assert-true (< (abs (sinc 1)) 1e-10))"
                 ";; sinc(0.5) = 2/pi ≈ 0.6366
(assert-true (< (abs (- (sinc 0.5) (/ 2 (pi-value)))) 1e-10))"
                 ";; sinc(-1) = 0 (symmetric)
(assert-true (< (abs (sinc -1)) 1e-10))"))
    (available-modules (prelude complex dft convolution vec))
    (hints ("Check near zero: (if (< (abs x) 1e-10) 1.0 ...)"
            "Compute: (let ([px (* (pi-value) x)]) (/ (sin px) px))"
            "pi-value is a function: (pi-value) returns pi"))
    (reference-solution "(define (sinc x)
  (if (< (abs x) 1e-10)
      1.0
      (let ([px (* (pi-value) x)])
           (/ (sin px) px))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement rectangular-window
  ;; ──────────────────────────────────────────────
  ;; Trivial window function (all ones).

  (task
    (id "numeric/digital-filters/rectangular-window-impl")
    (module digital-filters)
    (tier 0)
    (type implement)
    (description "Implement rectangular-window: create a vector of n ones. The rectangular window applies no windowing — it's equivalent to truncation. Despite being the simplest window, it has the worst spectral leakage. It's the baseline against which other windows (Hamming, Hann, Blackman) are compared. Just use make-vector with initial value 1.0.")
    (context "Available: make-vector. Single call.")
    (before "(define (rectangular-window n)
  ;; TODO: vector of n ones
  (make-vector n 0.0))")
    (test-file "lattice/numeric/test-digital-filters.ss")
    (test-forms (";; 4-point rectangular window
(assert-equal '#(1.0 1.0 1.0 1.0) (rectangular-window 4))"
                 ";; 1-point
(assert-equal '#(1.0) (rectangular-window 1))"
                 ";; Verify length
(assert-equal 8 (vector-length (rectangular-window 8)))"))
    (available-modules (prelude complex dft convolution vec))
    (hints ("Literally: (make-vector n 1.0)"
            "make-vector takes size and fill value"))
    (reference-solution "(define (rectangular-window n)
  (make-vector n 1.0))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement moving-average
  ;; ──────────────────────────────────────────────
  ;; FIR moving average filter coefficients.

  (task
    (id "numeric/digital-filters/moving-average-impl")
    (module digital-filters)
    (tier 0)
    (type implement)
    (description "Implement moving-average: create the FIR coefficients for an n-point moving average filter. Each coefficient is 1/n, producing a vector of n equal values that sum to 1. The moving average is the simplest lowpass FIR filter — it has a flat passband and -20dB/decade rolloff. It's optimal for reducing random noise while retaining a sharp step response. Compute the coefficient 1/n, then create a vector of n copies.")
    (context "Available: make-vector, /, let. One division + one allocation.")
    (before "(define (moving-average n)
  ;; TODO: vector of n copies of 1/n
  (make-vector n 0.0))")
    (test-file "lattice/numeric/test-digital-filters.ss")
    (test-forms (";; 4-point moving average
(assert-equal '#(0.25 0.25 0.25 0.25) (moving-average 4))"
                 ";; 1-point moving average (identity)
(assert-equal '#(1.0) (moving-average 1))"
                 ";; Coefficients sum to 1
(let* ([ma (moving-average 5)]
       [sum (do ([i 0 (+ i 1)] [s 0.0 (+ s (vector-ref ma i))])
                ((= i 5) s))])
  (assert-true (< (abs (- sum 1.0)) 1e-10)))"))
    (available-modules (prelude complex dft convolution vec))
    (hints ("Compute coefficient: (let ([coeff (/ 1.0 n)]) ...)"
            "Create vector: (make-vector n coeff)"))
    (reference-solution "(define (moving-average n)
  (let ([coeff (/ 1.0 n)])
       (make-vector n coeff)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement log10
  ;; ──────────────────────────────────────────────
  ;; Base-10 logarithm for dB calculations.

  (task
    (id "numeric/digital-filters/log10-impl")
    (module digital-filters)
    (tier 0)
    (type implement)
    (description "Implement log10: compute the base-10 logarithm of x using the change-of-base formula log10(x) = ln(x) / ln(10). This is essential for frequency response analysis where magnitudes are expressed in decibels (dB = 20*log10(magnitude)). Scheme provides natural log as (log x). Divide by (log 10) to convert to base 10.")
    (context "Available: log, /. Single expression.")
    (before "(define (log10 x)
  ;; TODO: base-10 logarithm
  0)")
    (test-file "lattice/numeric/test-digital-filters.ss")
    (test-forms (";; log10(1) = 0
(assert-equal 0 (log10 1))"
                 ";; log10(10) = 1
(assert-true (< (abs (- (log10 10) 1.0)) 1e-10))"
                 ";; log10(100) = 2
(assert-true (< (abs (- (log10 100) 2.0)) 1e-10))"
                 ";; log10(0.1) = -1
(assert-true (< (abs (- (log10 0.1) -1.0)) 1e-10))"))
    (available-modules (prelude complex dft convolution vec))
    (hints ("Change of base: log10(x) = ln(x) / ln(10)"
            "(/ (log x) (log 10))"))
    (reference-solution "(define (log10 x)
  (/ (log x) (log 10)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement hamming-window
  ;; ──────────────────────────────────────────────
  ;; Hamming window for FIR filter design.

  (task
    (id "numeric/digital-filters/hamming-window-impl")
    (module digital-filters)
    (tier 0)
    (type implement)
    (description "Implement hamming-window: create an n-point Hamming window. The Hamming window is defined as w[i] = 0.54 - 0.46*cos(2*pi*i/(N-1)) for i = 0, ..., N-1. It's the most commonly used window for FIR filter design because it provides a good balance between main lobe width and sidelobe suppression (~-43dB first sidelobe). Handle n=1 as a special case returning (vector 1.0). For n > 1, allocate a vector and fill using a do loop.")
    (context "Available: make-vector, vector-set!, cos, /, *, -, pi-value, do, =, if, vector, let. Special case + allocation + loop.")
    (before "(define (hamming-window n)
  ;; TODO: 0.54 - 0.46*cos(2*pi*i/(N-1))
  (make-vector n 1.0))")
    (test-file "lattice/numeric/test-digital-filters.ss")
    (test-forms (";; 5-point Hamming window: endpoints ≈ 0.08, center = 1.0
(let ([w (hamming-window 5)])
  (assert-true (< (abs (- (vector-ref w 0) 0.08)) 0.01))
  (assert-true (< (abs (- (vector-ref w 2) 1.0)) 1e-10))
  (assert-true (< (abs (- (vector-ref w 4) 0.08)) 0.01)))"
                 ";; 1-point: trivially 1.0
(assert-equal '#(1.0) (hamming-window 1))"
                 ";; Symmetric: w[i] = w[N-1-i]
(let ([w (hamming-window 7)])
  (assert-true (< (abs (- (vector-ref w 0) (vector-ref w 6))) 1e-10))
  (assert-true (< (abs (- (vector-ref w 1) (vector-ref w 5))) 1e-10)))"))
    (available-modules (prelude complex dft convolution vec))
    (hints ("Handle n=1: return (vector 1.0)"
            "Denominator: (let ([denom (- n 1)]) ...)"
            "Formula: (- 0.54 (* 0.46 (cos (/ (* 2 (pi-value) i) denom))))"
            "do loop: (do ([i 0 (+ i 1)]) ((= i n) result) ...)"))
    (reference-solution "(define (hamming-window n)
  (if (= n 1)
      (vector 1.0)
      (let ([result (make-vector n)]
            [denom (- n 1)])
           (do ([i 0 (+ i 1)])
               ((= i n) result)
               (vector-set! result i
                            (- 0.54
                               (* 0.46 (cos (/ (* 2 (pi-value) i) denom)))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
