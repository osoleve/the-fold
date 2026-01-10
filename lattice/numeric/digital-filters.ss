;;; core/numeric/digital-filters.ss — Digital Filter Library
;;;
;;; Comprehensive digital filter implementations including:
;;; - FIR filters (Finite Impulse Response)
;;; - IIR filters (Infinite Impulse Response)
;;; - Filter design tools (windowing, Butterworth, Chebyshev)
;;; - Frequency response analysis
;;; - Real-time filtering capabilities
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - complex.ss
;;;   - dft.ss
;;;   - convolution.ss
;;;   - vec.ss

(load "core/base/prelude.ss")
(load "lattice/numeric/complex.ss")
(load "lattice/numeric/dft.ss")
(load "lattice/numeric/convolution.ss")
(load "lattice/linalg/vec.ss")

;;; ============================================================
;;; Window Functions
;;; ============================================================

;;; rectangular-window : Integer → (Vector Number)
;;; All ones (no windowing).
(define (rectangular-window n)
  (make-vector n 1.0))

;;; hamming-window : Integer → (Vector Number)
;;; Hamming window: 0.54 - 0.46*cos(2*pi*n/(N-1))
(define (hamming-window n)
  (if (= n 1)
      (vector 1.0)
      (let ([result (make-vector n)]
            [denom (- n 1)])
           (do ([i 0 (+ i 1)])
               ((= i n) result)
               (vector-set! result i
                            (- 0.54
                               (* 0.46 (cos (/ (* 2 (pi-value) i) denom)))))))))

;;; hann-window : Integer → (Vector Number)
;;; Hann window: 0.5 * (1 - cos(2*pi*n/(N-1)))
(define (hann-window n)
  (if (= n 1)
      (vector 1.0)
      (let ([result (make-vector n)]
            [denom (- n 1)])
           (do ([i 0 (+ i 1)])
               ((= i n) result)
               (vector-set! result i
                            (* 0.5 (- 1 (cos (/ (* 2 (pi-value) i) denom)))))))))

;;; blackman-window : Integer → (Vector Number)
;;; Blackman window: 0.42 - 0.5*cos(2*pi*n/(N-1)) + 0.08*cos(4*pi*n/(N-1))
(define (blackman-window n)
  (if (= n 1)
      (vector 1.0)
      (let ([result (make-vector n)]
            [denom (- n 1)])
           (do ([i 0 (+ i 1)])
               ((= i n) result)
               (vector-set! result i
                            (+ 0.42
                               (- (* 0.5 (cos (/ (* 2 (pi-value) i) denom))))
                               (* 0.08 (cos (/ (* 4 (pi-value) i) denom)))))))))

;;; kaiser-window : Integer × Number → (Vector Number)
;;; Kaiser window with shape parameter beta.
;;; beta = 0: rectangular, beta = 5.0: similar to Hamming, beta = 8.6: similar to Blackman
(define (kaiser-window n beta)
  (if (= n 1)
      (vector 1.0)
      (let* ([result (make-vector n)]
             [i0-beta (bessel-i0 beta)]
             [half (/ (- n 1) 2.0)])
            (do ([i 0 (+ i 1)])
                ((= i n) result)
                (let* ([x (/ (- i half) half)]
                       [arg (* beta (sqrt (max 0 (- 1 (* x x)))))])
                      (vector-set! result i
                                   (/ (bessel-i0 arg) i0-beta)))))))

;;; bessel-i0 : Number → Number
;;; Modified Bessel function of the first kind, order 0.
;;; Uses polynomial approximation.
(define (bessel-i0 x)
  (let ([ax (abs x)])
       (if (< ax 3.75)
           ;; Polynomial approximation for small x
           (let* ([y (/ (* x x) 14.0625)]
                  [t (* y y)])
                 (+ 1.0
                    (* y (+ 3.5156229
                            (* t (+ 3.0899424
                                    (* t (+ 1.2067492
                                            (* t (+ 0.2659732
                                                    (* t (+ 0.0360768
                                                            (* t 0.0045813)))))))))))))
           ;; Asymptotic expansion for large x
           (let* ([y (/ 3.75 ax)]
                  [t (* y y)])
                 (* (/ (exp ax) (sqrt ax))
                    (+ 0.39894228
                       (* t (+ 0.01328592
                               (* t (+ 0.00225319
                                       (* t (+ -0.00157565
                                               (* t (+ 0.00916281
                                                       (* t (+ -0.02057706
                                                               (* t (+ 0.02635537
                                                                       (* t (+ -0.01647633
                                                                               (* t 0.00392377)))))))))))))))))))))

;;; ============================================================
;;; FIR Filter Design (Windowing Method)
;;; ============================================================

;;; sinc : Number → Number
;;; Normalized sinc function: sin(pi*x)/(pi*x), sinc(0) = 1
(define (sinc x)
  (if (< (abs x) 1e-10)
      1.0
      (let ([px (* (pi-value) x)])
           (/ (sin px) px))))

;;; fir-lowpass : Number × Integer × (Integer → Vector) → (Vector Number)
;;; Design FIR lowpass filter using windowing method.
;;; cutoff: normalized cutoff frequency (0 to 1, where 1 = Nyquist)
;;; order: filter order (length = order + 1)
;;; window-fn: window function (e.g., hamming-window)
(define (fir-lowpass cutoff order window-fn)
  (let* ([n (+ order 1)]
         [half (/ order 2.0)]
         [window (window-fn n)]
         [coeffs (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) coeffs)
            (let* ([idx (- i half)]
                   [h (if (= idx 0)
                          cutoff
                          (* cutoff (sinc (* cutoff idx))))]
                   [w (vector-ref window i)])
                  (vector-set! coeffs i (* h w))))))

;;; fir-highpass : Number × Integer × (Integer → Vector) → (Vector Number)
;;; Design FIR highpass filter using spectral inversion.
;;; cutoff: normalized cutoff frequency (0 to 1)
(define (fir-highpass cutoff order window-fn)
  (let* ([lp (fir-lowpass cutoff order window-fn)]
         [n (vector-length lp)]
         [half (quotient n 2)]
         [result (make-vector n)])
        ;; Spectral inversion: negate all coefficients and add 1 to center
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([val (- (vector-ref lp i))])
                 (vector-set! result i (if (= i half) (+ 1 val) val))))))

;;; fir-bandpass : Number × Number × Integer × (Integer → Vector) → (Vector Number)
;;; Design FIR bandpass filter.
;;; low, high: normalized band edges (0 to 1)
(define (fir-bandpass low high order window-fn)
  (let* ([lp-high (fir-lowpass high order window-fn)]
         [lp-low (fir-lowpass low order window-fn)]
         [n (vector-length lp-high)]
         [result (make-vector n)])
        ;; Bandpass = lowpass(high) - lowpass(low)
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i
                         (- (vector-ref lp-high i) (vector-ref lp-low i))))))

;;; fir-bandstop : Number × Number × Integer × (Integer → Vector) → (Vector Number)
;;; Design FIR bandstop (notch) filter.
;;; low, high: normalized band edges (0 to 1)
(define (fir-bandstop low high order window-fn)
  (let* ([bp (fir-bandpass low high order window-fn)]
         [n (vector-length bp)]
         [half (quotient n 2)]
         [result (make-vector n)])
        ;; Bandstop = 1 - bandpass (spectral inversion)
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([val (- (vector-ref bp i))])
                 (vector-set! result i (if (= i half) (+ 1 val) val))))))

;;; ============================================================
;;; FIR Filter Application
;;; ============================================================

;;; fir-filter : (Vector Number) × (Vector Number) → (Vector Number)
;;; Apply FIR filter to signal using convolution.
;;; Returns output with same length as input (valid mode with padding).
(define (fir-filter signal coeffs)
  (convolve signal coeffs 'same))

;;; ============================================================
;;; IIR Filter Structures
;;; ============================================================

;;; An IIR filter is defined by:
;;; - b: numerator coefficients [b0, b1, ..., bM]
;;; - a: denominator coefficients [a0, a1, ..., aN] (a0 is typically 1)
;;;
;;; Transfer function: H(z) = (b0 + b1*z^-1 + ... + bM*z^-M) / (a0 + a1*z^-1 + ... + aN*z^-N)

;;; make-iir-filter : (Vector Number) × (Vector Number) → IIR-Filter
;;; Create an IIR filter structure.
(define (make-iir-filter b a)
  `((type . iir)
    (b . ,b)
    (a . ,a)))

;;; iir-filter? : Any → Boolean
(define (iir-filter? f)
  (and (pair? f)
       (eq? (cdr (assq 'type f)) 'iir)))

;;; iir-filter-b : IIR-Filter → (Vector Number)
(define (iir-filter-b f)
  (cdr (assq 'b f)))

;;; iir-filter-a : IIR-Filter → (Vector Number)
(define (iir-filter-a f)
  (cdr (assq 'a f)))

;;; ============================================================
;;; IIR Filter Application
;;; ============================================================

;;; iir-filter-signal : (Vector Number) × (Vector Number) × (Vector Number) → (Vector Number)
;;; Apply IIR filter using direct form I.
;;; y[n] = (1/a0) * (b0*x[n] + b1*x[n-1] + ... - a1*y[n-1] - a2*y[n-2] - ...)
(define (iir-filter-signal signal b a)
  (let* ([n (vector-length signal)]
         [nb (vector-length b)]
         [na (vector-length a)]
         [a0 (vector-ref a 0)]
         [output (make-vector n 0.0)]
         ;; State buffers (circular)
         [x-hist (make-vector nb 0.0)]
         [y-hist (make-vector na 0.0)])
        (do ([i 0 (+ i 1)])
            ((= i n) output)
            ;; Shift input history
            (do ([j (- nb 1) (- j 1)])
                ((= j 0))
                (vector-set! x-hist j (vector-ref x-hist (- j 1))))
            (vector-set! x-hist 0 (vector-ref signal i))
            ;; Compute output
            (let ([sum 0.0])
                 ;; Feedforward (b coefficients)
                 (do ([j 0 (+ j 1)])
                     ((= j nb))
                     (set! sum (+ sum (* (vector-ref b j) (vector-ref x-hist j)))))
                 ;; Feedback (a coefficients, skip a0)
                 (do ([j 1 (+ j 1)])
                     ((= j na))
                     (set! sum (- sum (* (vector-ref a j) (vector-ref y-hist j)))))
                 ;; Normalize by a0
                 (set! sum (/ sum a0))
                 ;; Shift output history
                 (do ([j (- na 1) (- j 1)])
                     ((= j 0))
                     (vector-set! y-hist j (vector-ref y-hist (- j 1))))
                 (vector-set! y-hist 0 sum)
                 (vector-set! output i sum)))))

;;; ============================================================
;;; Biquad Sections (Second Order Sections)
;;; ============================================================

;;; A biquad is a second-order IIR filter section:
;;; H(z) = (b0 + b1*z^-1 + b2*z^-2) / (1 + a1*z^-1 + a2*z^-2)

;;; make-biquad : Number × Number × Number × Number × Number → Biquad
;;; Create a biquad section with coefficients b0, b1, b2, a1, a2.
;;; Note: a0 is assumed to be 1 (normalized form).
(define (make-biquad b0 b1 b2 a1 a2)
  `((type . biquad)
    (b0 . ,b0) (b1 . ,b1) (b2 . ,b2)
    (a1 . ,a1) (a2 . ,a2)))

;;; biquad? : Any → Boolean
(define (biquad? x)
  (and (pair? x) (eq? (cdr (assq 'type x)) 'biquad)))

;;; biquad-b0 : Biquad → Number
(define (biquad-b0 bq) (cdr (assq 'b0 bq)))

;;; biquad-b1 : Biquad → Number
(define (biquad-b1 bq) (cdr (assq 'b1 bq)))

;;; biquad-b2 : Biquad → Number
(define (biquad-b2 bq) (cdr (assq 'b2 bq)))

;;; biquad-a1 : Biquad → Number
(define (biquad-a1 bq) (cdr (assq 'a1 bq)))

;;; biquad-a2 : Biquad → Number
(define (biquad-a2 bq) (cdr (assq 'a2 bq)))

;;; biquad-filter : (Vector Number) × Biquad → (Vector Number)
;;; Apply biquad filter using transposed direct form II.
(define (biquad-filter signal bq)
  (let* ([n (vector-length signal)]
         [b0 (biquad-b0 bq)]
         [b1 (biquad-b1 bq)]
         [b2 (biquad-b2 bq)]
         [a1 (biquad-a1 bq)]
         [a2 (biquad-a2 bq)]
         [output (make-vector n 0.0)]
         [z1 0.0]  ; State variable 1
         [z2 0.0]) ; State variable 2
        (do ([i 0 (+ i 1)])
            ((= i n) output)
            (let* ([x (vector-ref signal i)]
                   [y (+ (* b0 x) z1)])
                  (set! z1 (+ (* b1 x) (- (* a1 y)) z2))
                  (set! z2 (+ (* b2 x) (- (* a2 y))))
                  (vector-set! output i y)))))

;;; cascade-biquads : (Vector Number) × (List Biquad) → (Vector Number)
;;; Apply a cascade of biquad sections.
(define (cascade-biquads signal biquads)
  (if (null? biquads)
      signal
      (cascade-biquads (biquad-filter signal (car biquads))
                       (cdr biquads))))

;;; ============================================================
;;; Butterworth Filter Design
;;; ============================================================

;;; butterworth-lowpass-poles : Integer → (List Complex)
;;; Compute analog Butterworth lowpass prototype poles.
;;; Poles are equally spaced on the left half of unit circle.
;;; For order N, poles are at s_k = exp(j * π * (2k + N + 1) / (2N)) for k = 0..N-1
(define (butterworth-lowpass-poles order)
  (let loop ([k 0] [poles '()])
       (if (= k order)
           (reverse poles)
           (let* ([angle (/ (* (pi-value) (+ (* 2 k) order 1)) (* 2 order))]
                  [pole (make-polar 1 angle)])
                 (loop (+ k 1)
                       (cons pole poles))))))

;;; bilinear-transform : Complex × Number → Complex
;;; Bilinear transform: s = (2/T) * (z-1)/(z+1)
;;; Maps analog pole to digital pole.
;;; fc: prewarped critical frequency
(define (bilinear-transform s fc)
  (let* ([t (/ 1.0 fc)]
         [st (complex-scale t s)])
        ;; z = (1 + s*T/2) / (1 - s*T/2)
        (complex-div (complex-add (make-complex 1 0) (complex-scale 0.5 st))
                     (complex-sub (make-complex 1 0) (complex-scale 0.5 st)))))

;;; butterworth-lowpass : Number × Integer → IIR-Filter
;;; Design digital Butterworth lowpass filter.
;;; cutoff: normalized cutoff frequency (0 to 1)
;;; order: filter order
(define (butterworth-lowpass cutoff order)
  ;; Prewarp cutoff frequency
  (let* ([wc (* (pi-value) cutoff)]
         [fc (tan wc)]
         ;; Get analog poles
         [poles (butterworth-lowpass-poles order)]
         ;; Transform to digital domain
         [z-poles (map (lambda (p) (bilinear-transform p fc)) poles)]
         ;; All zeros at z = -1 for lowpass
         [z-zeros (make-list order (make-complex -1 0))])
        ;; Convert poles/zeros to transfer function coefficients
        (poles-zeros->tf z-poles z-zeros)))

;;; complex-sum : (List Complex) → Complex
;;; Sum a list of complex numbers.
(define (complex-sum lst)
  (fold-left complex-add (complex-zero) lst))

;;; poles-zeros->tf : (List Complex) × (List Complex) → IIR-Filter
;;; Convert poles and zeros to transfer function coefficients.
(define (poles-zeros->tf poles zeros)
  ;; Build numerator from zeros: product of (z - zero_i)
  (let* ([b (poly-from-roots zeros)]
         ;; Build denominator from poles: product of (z - pole_i)
         [a (poly-from-roots poles)]
         ;; Normalize so DC gain is 1 (sum of coefficients at z=1)
         [b-sum (complex-sum (vector->list b))]
         [a-sum (complex-sum (vector->list a))]
         [b-mag (complex-magnitude b-sum)]
         [gain (if (< b-mag 1e-10) 1.0 (/ (complex-magnitude a-sum) b-mag))]
         [b-normalized (vector-scale-real b gain)]
         [a-real (vector-scale-real a 1.0)])
        (make-iir-filter b-normalized a-real)))

;;; vector-scale-real : (Vector (Or Complex Number)) × Number → (Vector Number)
;;; Scale vector and extract real parts.
(define (vector-scale-real v scale)
  (let* ([n (vector-length v)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([val (vector-ref v i)])
                 (vector-set! result i
                              (* scale (if (complex? val) (complex-real val) val)))))))

;;; poly-from-roots : (List Complex) → (Vector Complex)
;;; Build polynomial coefficients from roots.
;;; Returns coefficients [c_n, c_{n-1}, ..., c_1, c_0] where
;;; p(z) = c_n*z^n + c_{n-1}*z^{n-1} + ... + c_0
(define (poly-from-roots roots)
  (if (null? roots)
      (vector (make-complex 1 0))
      (let loop ([roots roots] [coeffs (vector (make-complex 1 0))])
           (if (null? roots)
               coeffs
               (let ([root (car roots)])
                    (loop (cdr roots)
                          (poly-mul-binomial coeffs root)))))))

;;; poly-mul-binomial : (Vector Complex) × Complex → (Vector Complex)
;;; Multiply polynomial by (z - root).
(define (poly-mul-binomial coeffs root)
  (let* ([n (vector-length coeffs)]
         [result (make-vector (+ n 1) (complex-zero))])
        ;; result = coeffs * z - coeffs * root
        ;; Coeffs for z^k term
        (do ([i 0 (+ i 1)])
            ((= i n))
            ;; z * c_i * z^i = c_i * z^{i+1}
            (vector-set! result i
                         (complex-add (vector-ref result i)
                                      (vector-ref coeffs i))))
        ;; -root * c_i * z^i
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! result (+ i 1)
                         (complex-sub (vector-ref result (+ i 1))
                                      (complex-mul root (vector-ref coeffs i)))))
        result))

;;; ============================================================
;;; Chebyshev Filter Design
;;; ============================================================

;;; chebyshev1-lowpass : Number × Number × Integer → IIR-Filter
;;; Design Chebyshev Type I lowpass filter.
;;; cutoff: normalized cutoff frequency (0 to 1)
;;; ripple: passband ripple in dB (e.g., 0.5)
;;; order: filter order
(define (chebyshev1-lowpass cutoff ripple order)
  ;; Calculate epsilon from ripple
  (let* ([eps (sqrt (- (expt 10 (/ ripple 10)) 1))]
         [wc (* (pi-value) cutoff)]
         [fc (tan wc)]
         ;; Chebyshev poles
         [poles (chebyshev1-poles order eps)]
         [z-poles (map (lambda (p) (bilinear-transform p fc)) poles)]
         [z-zeros (make-list order (make-complex -1 0))])
        (poles-zeros->tf z-poles z-zeros)))

;;; chebyshev1-poles : Integer × Number → (List Complex)
;;; Compute Chebyshev Type I analog poles.
(define (chebyshev1-poles order eps)
  (let* ([mu (/ 1.0 order (asinh (/ 1.0 eps)))])
        (let loop ([k 0] [poles '()])
             (if (= k order)
                 (reverse poles)
                 (let* ([theta (/ (* (pi-value) (+ 1 (* 2 k))) (* 2 order))]
                        [sigma (* -1 (sinh mu) (sin theta))]
                        [omega (* (cosh mu) (cos theta))])
                       (loop (+ k 1)
                             (cons (make-complex sigma omega) poles)))))))

;;; asinh : Number → Number
;;; Inverse hyperbolic sine.
(define (asinh x)
  (log (+ x (sqrt (+ (* x x) 1)))))

;;; cosh : Number → Number
;;; Hyperbolic cosine.
(define (cosh x)
  (/ (+ (exp x) (exp (- x))) 2))

;;; sinh : Number → Number
;;; Hyperbolic sine.
(define (sinh x)
  (/ (- (exp x) (exp (- x))) 2))

;;; ============================================================
;;; Frequency Response Analysis
;;; ============================================================

;;; freqz : IIR-Filter × Integer → ((Vector Number) × (Vector Complex))
;;; Compute frequency response of IIR filter.
;;; Returns (frequencies . H(e^jw)) where frequencies are normalized (0 to pi).
(define (freqz filter n-points)
  (let* ([b (iir-filter-b filter)]
         [a (iir-filter-a filter)]
         [freqs (make-vector n-points)]
         [response (make-vector n-points)])
        (do ([i 0 (+ i 1)])
            ((= i n-points) (cons freqs response))
            (let* ([w (/ (* (pi-value) i) n-points)]
                   [ejw (make-polar 1 w)]
                   [h (eval-transfer-function b a ejw)])
                  (vector-set! freqs i w)
                  (vector-set! response i h)))))

;;; eval-transfer-function : Vector × Vector × Complex → Complex
;;; Evaluate H(z) = B(z)/A(z) at z.
(define (eval-transfer-function b a z)
  (let ([num (eval-poly b z)]
        [den (eval-poly a z)])
       (complex-div num den)))

;;; eval-poly : (Vector Number) × Complex → Complex
;;; Evaluate polynomial at z using Horner's method.
;;; Coefficients are [b0, b1, ...] for b0 + b1*z^-1 + b2*z^-2 + ...
(define (eval-poly coeffs z)
  (let* ([n (vector-length coeffs)]
         [z-inv (complex-div (make-complex 1 0) z)])
        ;; Horner's method starting from highest degree term
        ;; P(z^-1) = b0 + b1*z^-1 + ... + b_{n-1}*z^{-(n-1)}
        ;;         = b0 + z^-1*(b1 + z^-1*(b2 + ... + z^-1*b_{n-1}))
        (let loop ([i (- n 1)]
                   [result (make-complex (vector-ref coeffs (- n 1)) 0)])
             (if (= i 0)
                 result
                 (loop (- i 1)
                       (complex-add (make-complex (vector-ref coeffs (- i 1)) 0)
                                    (complex-mul result z-inv)))))))

;;; magnitude-response : IIR-Filter × Integer → ((Vector Number) × (Vector Number))
;;; Compute magnitude response in dB.
(define (magnitude-response filter n-points)
  (let* ([fr (freqz filter n-points)]
         [freqs (car fr)]
         [response (cdr fr)]
         [mag-db (make-vector n-points)])
        (do ([i 0 (+ i 1)])
            ((= i n-points) (cons freqs mag-db))
            (let ([mag (complex-magnitude (vector-ref response i))])
                 (vector-set! mag-db i
                              (if (< mag 1e-20)
                                  -400  ; Very small = very negative dB
                                  (* 20 (log10 mag))))))))

;;; log10 : Number → Number
(define (log10 x)
  (/ (log x) (log 10)))

;;; phase-response : IIR-Filter × Integer → ((Vector Number) × (Vector Number))
;;; Compute phase response in radians.
(define (phase-response filter n-points)
  (let* ([fr (freqz filter n-points)]
         [freqs (car fr)]
         [response (cdr fr)]
         [phase (make-vector n-points)])
        (do ([i 0 (+ i 1)])
            ((= i n-points) (cons freqs phase))
            (vector-set! phase i (complex-angle (vector-ref response i))))))

;;; ============================================================
;;; Real-time Filtering State
;;; ============================================================

;;; make-filter-state : IIR-Filter → Filter-State
;;; Create stateful filter for sample-by-sample processing.
(define (make-filter-state filter)
  (let* ([b (iir-filter-b filter)]
         [a (iir-filter-a filter)]
         [nb (vector-length b)]
         [na (vector-length a)])
        `((type . filter-state)
          (b . ,b)
          (a . ,a)
          (x-hist . ,(make-vector nb 0.0))
          (y-hist . ,(make-vector na 0.0)))))

;;; filter-state? : Any → Boolean
(define (filter-state? x)
  (and (pair? x) (eq? (cdr (assq 'type x)) 'filter-state)))

;;; filter-process-sample! : Filter-State × Number → Number
;;; Process one sample through the filter, updating state.
(define (filter-process-sample! state sample)
  (let* ([b (cdr (assq 'b state))]
         [a (cdr (assq 'a state))]
         [x-hist (cdr (assq 'x-hist state))]
         [y-hist (cdr (assq 'y-hist state))]
         [nb (vector-length b)]
         [na (vector-length a)]
         [a0 (vector-ref a 0)])
        ;; Shift input history
        (do ([j (- nb 1) (- j 1)])
            ((= j 0))
            (vector-set! x-hist j (vector-ref x-hist (- j 1))))
        (vector-set! x-hist 0 sample)
        ;; Compute output
        (let ([sum 0.0])
             ;; Feedforward
             (do ([j 0 (+ j 1)])
                 ((= j nb))
                 (set! sum (+ sum (* (vector-ref b j) (vector-ref x-hist j)))))
             ;; Feedback
             (do ([j 1 (+ j 1)])
                 ((= j na))
                 (set! sum (- sum (* (vector-ref a j) (vector-ref y-hist j)))))
             ;; Normalize
             (set! sum (/ sum a0))
             ;; Shift output history
             (do ([j (- na 1) (- j 1)])
                 ((= j 0))
                 (vector-set! y-hist j (vector-ref y-hist (- j 1))))
             (vector-set! y-hist 0 sum)
             sum)))

;;; filter-reset! : Filter-State → void
;;; Reset filter state to zero.
(define (filter-reset! state)
  (let ([x-hist (cdr (assq 'x-hist state))]
        [y-hist (cdr (assq 'y-hist state))])
       (let ([nx (vector-length x-hist)]
             [ny (vector-length y-hist)])
            (do ([i 0 (+ i 1)])
                ((= i nx))
                (vector-set! x-hist i 0.0))
            (do ([i 0 (+ i 1)])
                ((= i ny))
                (vector-set! y-hist i 0.0)))))

;;; ============================================================
;;; Common Filter Presets
;;; ============================================================

;;; dc-blocker : Number → IIR-Filter
;;; Simple DC blocking filter.
;;; r: pole radius (0.99 typical, higher = slower cutoff)
(define (dc-blocker r)
  (make-iir-filter (vector 1.0 -1.0)
                   (vector 1.0 (- r))))

;;; one-pole-lowpass : Number → IIR-Filter
;;; Simple one-pole lowpass filter.
;;; a: coefficient (0 to 1), lower = slower response
(define (one-pole-lowpass a)
  (make-iir-filter (vector a)
                   (vector 1.0 (- a 1))))

;;; one-pole-highpass : Number → IIR-Filter
;;; Simple one-pole highpass filter.
;;; a: coefficient (0 to 1)
(define (one-pole-highpass a)
  (make-iir-filter (vector (/ (+ 1 (- a 1)) 2) (/ (- (+ 1 (- a 1))) 2))
                   (vector 1.0 (- a 1))))

;;; moving-average : Integer → (Vector Number)
;;; FIR moving average filter coefficients.
(define (moving-average n)
  (let ([coeff (/ 1.0 n)])
       (make-vector n coeff)))

;;; ============================================================
;;; Impulse Response
;;; ============================================================

;;; impulse-response : IIR-Filter × Integer → (Vector Number)
;;; Compute impulse response of filter.
(define (impulse-response filter n)
  (let ([impulse (make-vector n 0.0)])
       (vector-set! impulse 0 1.0)
       (iir-filter-signal impulse
                          (iir-filter-b filter)
                          (iir-filter-a filter))))

;;; step-response : IIR-Filter × Integer → (Vector Number)
;;; Compute step response of filter.
(define (step-response filter n)
  (let ([step (make-vector n 1.0)])
       (iir-filter-signal step
                          (iir-filter-b filter)
                          (iir-filter-a filter))))
