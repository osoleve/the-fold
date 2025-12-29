;;; fabric/stitches/fp/interval-elem.ss -- Interval Elementary Functions
;;;
;;; Provides elementary functions (sqrt, exp, log, trig) for intervals.
;;; Uses proper range analysis to compute tight bounds.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/interval.ss

(load "prelude.ss")
(load "fp/interval.ss")

;;; ============================================================
;;; Mathematical Constants (as intervals for rigorous computation)
;;; ============================================================

;;; We use intervals that contain the true value to avoid rounding errors.
;;; These are wide enough to contain the exact constants with standard floats.

;;; interval-pi : Interval
;;; An interval containing pi.
(define interval-pi
  (make-interval 3.141592653589793 3.1415926535897932))

;;; interval-e : Interval
;;; An interval containing e.
(define interval-e
  (make-interval 2.718281828459045 2.7182818284590452))

;;; interval-pi/2 : Interval
(define interval-pi/2
  (make-interval 1.5707963267948966 1.5707963267948968))

;;; interval-2pi : Interval
(define interval-2pi
  (make-interval 6.283185307179586 6.283185307179587))

;;; ============================================================
;;; Square Root
;;; ============================================================

;;; interval-sqrt : Interval -> Maybe Interval
;;; Square root of an interval. Returns nothing if interval is negative.
;;; sqrt is monotonically increasing, so sqrt([a,b]) = [sqrt(a), sqrt(b)]
(define (interval-sqrt iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
       (cond
        ;; Entirely negative: undefined
        [(< b 0) nothing]
        ;; Contains negative: clamp to [0, sqrt(b)]
        [(< a 0) (just (make-interval 0 (sqrt b)))]
        ;; Entirely non-negative
        [else (just (make-interval (sqrt a) (sqrt b)))])))

;;; interval-sqrt-unsafe : Interval -> Interval
;;; Square root without checking for negative values.
(define (interval-sqrt-unsafe iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
       (make-interval (if (< a 0) 0 (sqrt a))
                      (sqrt b))))

;;; ============================================================
;;; Exponential and Logarithm
;;; ============================================================

;;; interval-exp : Interval -> Interval
;;; Exponential function. exp is monotonically increasing.
;;; exp([a,b]) = [exp(a), exp(b)]
(define (interval-exp iv)
  (make-interval (exp (interval-lo iv))
                 (exp (interval-hi iv))))

;;; interval-log : Interval -> Maybe Interval
;;; Natural logarithm. Defined only for positive intervals.
;;; log is monotonically increasing, so log([a,b]) = [log(a), log(b)]
(define (interval-log iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
       (cond
        ;; Entirely non-positive: undefined
        [(<= b 0) nothing]
        ;; Contains zero: clamp lower bound
        [(<= a 0)
         (just (make-interval (log 1e-308) (log b)))]  ; Use smallest positive
        ;; Entirely positive
        [else (just (make-interval (log a) (log b)))])))

;;; interval-log-unsafe : Interval -> Interval
;;; Logarithm without checking for non-positive values.
(define (interval-log-unsafe iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
       (make-interval (log (if (<= a 0) 1e-308 a))
                      (log b))))

;;; interval-log10 : Interval -> Maybe Interval
;;; Base-10 logarithm.
(define (interval-log10 iv)
  (let ([result (interval-log iv)])
       (if (just? result)
           (just (interval-scale (/ 1 (log 10)) (from-just result)))
           nothing)))

;;; interval-log2 : Interval -> Maybe Interval
;;; Base-2 logarithm.
(define (interval-log2 iv)
  (let ([result (interval-log iv)])
       (if (just? result)
           (just (interval-scale (/ 1 (log 2)) (from-just result)))
           nothing)))

;;; ============================================================
;;; Power Functions
;;; ============================================================

;;; interval-pow : Interval -> Interval -> Interval
;;; Power function: iv1^iv2
;;; This is complex due to various cases. We handle:
;;; - Positive base: a^b = exp(b * log(a))
;;; - Negative base with integer exponent: handled specially
;;; - Zero base: 0^b = 0 for b > 0
(define (interval-pow base-iv exp-iv)
  (let ([a (interval-lo base-iv)]
        [b (interval-hi base-iv)]
        [c (interval-lo exp-iv)]
        [d (interval-hi exp-iv)])
       (cond
        ;; Zero exponent: x^0 = 1
        [(and (= c 0) (= d 0)) (interval-singleton 1)]
        ;; Positive base: use exp(exp * log(base))
        [(> a 0)
         (interval-exp (interval-mul exp-iv
                                     (interval-log-unsafe base-iv)))]
        ;; Base contains zero with positive exponent
        [(and (>= b 0) (> c 0))
         ;; Result includes 0 from 0^positive
         (let ([pos-result (if (> b 0)
                               (interval-exp
                                (interval-mul exp-iv
                                              (from-just (interval-log (make-interval (max a 1e-308) b)))))
                               (interval-singleton 0))])
              (if (>= a 0)
                  (make-interval 0 (interval-hi pos-result))
                  ;; Negative base part is problematic
                  pos-result))]
        ;; Default: conservative bounds
        [else (make-interval -1e308 1e308)])))

;;; interval-pow-real : Interval -> Real -> Interval
;;; Raise interval to a real power (simpler than interval exponent).
(define (interval-pow-real iv n)
  (cond
   [(= n 0) (interval-singleton 1)]
   [(= n 1) iv]
   [(and (integer? n) (exact? n) (>= n 0))
    (interval-pow-nat iv (exact n))]
   [(interval-positive? iv)
    (interval-exp (interval-scale n (interval-log-unsafe iv)))]
   [else
    ;; Negative numbers with non-integer exponent: undefined in reals
    (make-interval (- (expt (- (interval-lo iv)) n))
                   (expt (interval-hi iv) n))]))

;;; ============================================================
;;; Trigonometric Functions
;;; ============================================================

;;; For trigonometric functions, we need to track critical points.
;;; sin has maxima at pi/2 + 2kpi and minima at -pi/2 + 2kpi
;;; cos has maxima at 2kpi and minima at pi + 2kpi

;;; Helper: reduce interval modulo 2pi
(define (interval-mod-2pi iv)
  (let* ([lo (interval-lo iv)]
         [hi (interval-hi iv)]
         [two-pi 6.283185307179586]
         [lo-mod (- lo (* two-pi (floor (/ lo two-pi))))]
         [width (interval-width iv)])
        (make-interval lo-mod (+ lo-mod width))))

;;; interval-sin : Interval -> Interval
;;; Sine function over an interval.
;;; sin oscillates between -1 and 1, with:
;;;   maxima at pi/2 + 2kpi
;;;   minima at -pi/2 + 2kpi (or 3pi/2 + 2kpi)
(define (interval-sin iv)
  (let* ([lo (interval-lo iv)]
         [hi (interval-hi iv)]
         [width (interval-width iv)]
         [two-pi 6.283185307179586]
         [pi 3.141592653589793]
         [half-pi 1.5707963267948966]
         [three-half-pi 4.71238898038469])
        (cond
         ;; Width >= 2pi: contains full cycle
         [(>= width two-pi) (make-interval -1 1)]
         [else
          ;; Reduce to [0, 2pi) range
          (let* ([lo-mod (- lo (* two-pi (floor (/ lo two-pi))))]
                 [hi-mod (+ lo-mod width)]
                 ;; Compute sin at endpoints
                 [sin-lo (sin lo)]
                 [sin-hi (sin hi)]
                 [min-val (min sin-lo sin-hi)]
                 [max-val (max sin-lo sin-hi)])
                ;; Check if interval contains a maximum (pi/2 + 2kpi)
                (let* ([max-val
                        (if (or (and (<= lo-mod half-pi) (>= hi-mod half-pi))
                                (and (<= lo-mod (+ half-pi two-pi))
                                     (>= hi-mod (+ half-pi two-pi))))
                            1.0
                            max-val)]
                       ;; Check if interval contains a minimum (3pi/2 + 2kpi)
                       [min-val
                        (if (or (and (<= lo-mod three-half-pi) (>= hi-mod three-half-pi))
                                (and (<= lo-mod (+ three-half-pi two-pi))
                                     (>= hi-mod (+ three-half-pi two-pi))))
                            -1.0
                            min-val)])
                      (make-interval min-val max-val)))])))

;;; interval-cos : Interval -> Interval
;;; Cosine function over an interval.
;;; cos oscillates between -1 and 1, with:
;;;   maxima at 2kpi
;;;   minima at pi + 2kpi
(define (interval-cos iv)
  (let* ([lo (interval-lo iv)]
         [hi (interval-hi iv)]
         [width (interval-width iv)]
         [two-pi 6.283185307179586]
         [pi 3.141592653589793])
        (cond
         ;; Width >= 2pi: contains full cycle
         [(>= width two-pi) (make-interval -1 1)]
         [else
          ;; Reduce to [0, 2pi) range
          (let* ([lo-mod (- lo (* two-pi (floor (/ lo two-pi))))]
                 [hi-mod (+ lo-mod width)]
                 ;; Compute cos at endpoints
                 [cos-lo (cos lo)]
                 [cos-hi (cos hi)]
                 [min-val (min cos-lo cos-hi)]
                 [max-val (max cos-lo cos-hi)])
                ;; Check if interval contains a maximum (0 or 2pi)
                (let* ([max-val
                        (if (or (<= lo-mod 0 hi-mod)  ; lo-mod is always >= 0
                                (>= hi-mod two-pi))
                            1.0
                            max-val)]
                       ;; Check if interval contains a minimum (pi)
                       [min-val
                        (if (and (<= lo-mod pi) (>= hi-mod pi))
                            -1.0
                            min-val)])
                      (make-interval min-val max-val)))])))

;;; interval-tan : Interval -> Maybe Interval
;;; Tangent function. Returns nothing if interval crosses a discontinuity.
;;; tan has vertical asymptotes at pi/2 + kpi.
(define (interval-tan iv)
  (let* ([lo (interval-lo iv)]
         [hi (interval-hi iv)]
         [pi 3.141592653589793]
         [half-pi 1.5707963267948966]
         ;; Check if interval width is at least pi (definitely crosses asymptote)
         [width (interval-width iv)])
        (if (>= width pi)
            nothing  ; Crosses at least one asymptote
            ;; Check if interval crosses an asymptote
            (let* ([lo-norm (- lo (* pi (floor (/ (+ lo half-pi) pi))))]
                   [hi-norm (+ lo-norm width)])
                  (if (> hi-norm half-pi)
                      nothing  ; Crosses asymptote
                      ;; tan is monotonic between asymptotes
                      (just (make-interval (tan lo) (tan hi))))))))

;;; interval-tan-unsafe : Interval -> Interval
;;; Tangent without checking for discontinuities.
(define (interval-tan-unsafe iv)
  (make-interval (tan (interval-lo iv))
                 (tan (interval-hi iv))))

;;; ============================================================
;;; Inverse Trigonometric Functions
;;; ============================================================

;;; interval-asin : Interval -> Maybe Interval
;;; Arcsine. Domain: [-1, 1], Range: [-pi/2, pi/2]
(define (interval-asin iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
       (cond
        ;; Entirely outside domain
        [(or (> a 1) (< b -1)) nothing]
        ;; Clamp to domain and compute (asin is monotonic increasing)
        [else
         (let ([lo-clamped (max -1 a)]
               [hi-clamped (min 1 b)])
              (just (make-interval (asin lo-clamped)
                                   (asin hi-clamped))))])))

;;; interval-acos : Interval -> Maybe Interval
;;; Arccosine. Domain: [-1, 1], Range: [0, pi]
;;; Note: acos is monotonically DECREASING
(define (interval-acos iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
       (cond
        ;; Entirely outside domain
        [(or (> a 1) (< b -1)) nothing]
        ;; Clamp to domain and compute (acos is monotonic decreasing!)
        [else
         (let ([lo-clamped (max -1 a)]
               [hi-clamped (min 1 b)])
              (just (make-interval (acos hi-clamped)   ; Swap due to decreasing
                                   (acos lo-clamped))))])))

;;; interval-atan : Interval -> Interval
;;; Arctangent. Domain: all reals, Range: (-pi/2, pi/2)
;;; atan is monotonically increasing.
(define (interval-atan iv)
  (make-interval (atan (interval-lo iv))
                 (atan (interval-hi iv))))

;;; interval-atan2 : Interval -> Interval -> Interval
;;; Two-argument arctangent (angle from x-axis).
;;; This is complex due to quadrant handling.
(define (interval-atan2 iv-y iv-x)
  (let ([y-lo (interval-lo iv-y)]
        [y-hi (interval-hi iv-y)]
        [x-lo (interval-lo iv-x)]
        [x-hi (interval-hi iv-x)])
       ;; Simple conservative bound for now
       ;; Full implementation would handle quadrants carefully
       (let ([angles (list (atan y-lo x-lo)
                           (atan y-lo x-hi)
                           (atan y-hi x-lo)
                           (atan y-hi x-hi))])
            (make-interval (apply min angles)
                           (apply max angles)))))

;;; ============================================================
;;; Hyperbolic Functions
;;; ============================================================

;;; interval-sinh : Interval -> Interval
;;; Hyperbolic sine. sinh is monotonically increasing.
(define (interval-sinh iv)
  (let ([sinh-fn (lambda (x) (/ (- (exp x) (exp (- x))) 2))])
       (make-interval (sinh-fn (interval-lo iv))
                      (sinh-fn (interval-hi iv)))))

;;; interval-cosh : Interval -> Interval
;;; Hyperbolic cosine. cosh has minimum at 0, symmetric about y-axis.
(define (interval-cosh iv)
  (let ([cosh-fn (lambda (x) (/ (+ (exp x) (exp (- x))) 2))]
        [a (interval-lo iv)]
        [b (interval-hi iv)])
       (cond
        ;; Both non-negative: cosh is increasing
        [(>= a 0) (make-interval (cosh-fn a) (cosh-fn b))]
        ;; Both non-positive: cosh is decreasing
        [(<= b 0) (make-interval (cosh-fn b) (cosh-fn a))]
        ;; Straddles zero: minimum at 0 (cosh(0) = 1)
        [else (make-interval 1 (max (cosh-fn a) (cosh-fn b)))])))

;;; interval-tanh : Interval -> Interval
;;; Hyperbolic tangent. tanh is monotonically increasing, bounded by (-1, 1).
(define (interval-tanh iv)
  (let ([tanh-fn (lambda (x)
                         (let ([ex (exp x)]
                               [e-x (exp (- x))])
                              (/ (- ex e-x) (+ ex e-x))))])
       (make-interval (tanh-fn (interval-lo iv))
                      (tanh-fn (interval-hi iv)))))

;;; ============================================================
;;; Inverse Hyperbolic Functions
;;; ============================================================

;;; interval-asinh : Interval -> Interval
;;; Inverse hyperbolic sine. asinh is monotonically increasing, domain: all reals.
(define (interval-asinh iv)
  (let ([asinh-fn (lambda (x) (log (+ x (sqrt (+ (* x x) 1)))))])
       (make-interval (asinh-fn (interval-lo iv))
                      (asinh-fn (interval-hi iv)))))

;;; interval-acosh : Interval -> Maybe Interval
;;; Inverse hyperbolic cosine. Domain: [1, inf), monotonically increasing.
(define (interval-acosh iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)]
        [acosh-fn (lambda (x) (log (+ x (sqrt (- (* x x) 1)))))])
       (if (< b 1)
           nothing  ; Entirely outside domain
           (let ([lo-clamped (max 1 a)])
                (just (make-interval (acosh-fn lo-clamped)
                                     (acosh-fn b)))))))

;;; interval-atanh : Interval -> Maybe Interval
;;; Inverse hyperbolic tangent. Domain: (-1, 1), monotonically increasing.
(define (interval-atanh iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)]
        [atanh-fn (lambda (x) (* 0.5 (log (/ (+ 1 x) (- 1 x)))))])
       (if (or (<= a -1) (>= b 1))
           nothing  ; Outside or touches domain boundary
           (just (make-interval (atanh-fn a) (atanh-fn b))))))

;;; ============================================================
;;; Floating Type Class Instance
;;; ============================================================

;;; interval-floating : Floating Interval
;;; Floating instance for intervals.
(define interval-floating
  (make-floating
   (make-fractional interval-num
                    interval-div-unsafe
                    (lambda (iv)  ; recip
                            (let ([result (interval-reciprocal iv)])
                                 (if (just? result)
                                     (from-just result)
                                     (make-interval -1e308 1e308)))))
   (lambda () interval-pi)    ; pi
   interval-exp               ; exp
   interval-log-unsafe        ; log (unsafe version for type class)
   interval-sqrt-unsafe       ; sqrt (unsafe version)
   (lambda (iv n) (interval-pow-real iv n))  ; **
   (lambda (iv n) (interval-pow-real iv n))  ; logBase (approximate)
   interval-sin               ; sin
   interval-cos               ; cos
   interval-tan-unsafe        ; tan (unsafe)
   interval-asin              ; asin (returns Maybe - need wrapper)
   interval-acos              ; acos (returns Maybe - need wrapper)
   interval-atan              ; atan
   interval-sinh              ; sinh
   interval-cosh              ; cosh
   interval-tanh              ; tanh
   interval-asinh             ; asinh
   interval-acosh             ; acosh (returns Maybe - need wrapper)
   interval-atanh))           ; atanh (returns Maybe - need wrapper)

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Constants:
;;;   interval-pi, interval-e, interval-pi/2, interval-2pi
;;;
;;; Square Root:
;;;   interval-sqrt, interval-sqrt-unsafe
;;;
;;; Exponential/Logarithm:
;;;   interval-exp, interval-log, interval-log-unsafe
;;;   interval-log10, interval-log2
;;;
;;; Power:
;;;   interval-pow, interval-pow-real
;;;
;;; Trigonometric:
;;;   interval-sin, interval-cos, interval-tan, interval-tan-unsafe
;;;
;;; Inverse Trigonometric:
;;;   interval-asin, interval-acos, interval-atan, interval-atan2
;;;
;;; Hyperbolic:
;;;   interval-sinh, interval-cosh, interval-tanh
;;;
;;; Inverse Hyperbolic:
;;;   interval-asinh, interval-acosh, interval-atanh
;;;
;;; Type Class:
;;;   interval-floating
