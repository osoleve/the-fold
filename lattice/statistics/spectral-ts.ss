(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module spectral-ts
;;; @requires prelude dft window-functions result-types
(require 'prelude)
(require 'dft)
(require 'window-functions)
(require 'result-types)

(doc 'module 'spectral-ts)
(doc 'description "Spectral Methods for Time Series — bridges FFT/spectral analysis with time series modeling")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'see-also '(dft spectral-analysis ar acf-pacf))

;;; ============================================================
;;; Section 1: Periodogram — Power Spectral Density via FFT
;;; ============================================================
;;;
;;; The periodogram estimates the power spectral density (PSD) of a
;;; stationary time series. For a signal x[0..N-1]:
;;;
;;;   P[k] = |X[k]|^2 / N
;;;
;;; where X[k] = DFT(x)[k]. This is the simplest PSD estimator,
;;; but has high variance (it does not converge as N grows).

(doc 'section 'periodogram)

;;; ts-periodogram : Vector[Number] -> Vector[Number]
;;; Compute the raw periodogram: P[k] = |X[k]|^2 / N.
;;; Returns power at N frequency bins [0, 1/N, 2/N, ..., (N-1)/N] in
;;; cycles per sample (normalized frequency).
;;;
;;; For one-sided spectrum of real signals, use bins 0..N/2.
;;; The periodogram is an asymptotically unbiased but inconsistent
;;; estimator of the true PSD.
(define (ts-periodogram xs)
  (doc 'export #t)
  (doc 'type '(-> Vec Vec))
  (let* ([n (vector-length xs)]
         [padded-n (next-power-of-2 n)]
         [padded (if (= n padded-n)
                     xs
                     (let ([v (make-vector padded-n 0)])
                       (do ([i 0 (+ i 1)])
                           ((= i n) v)
                         (vector-set! v i (vector-ref xs i)))))]
         [X (dft-real padded)]
         [m (vector-length X)]
         [psd (make-vector m)])
    (do ([k 0 (+ k 1)])
        ((= k m) psd)
      (let* ([Xk (vector-ref X k)]
             [mag (complex-magnitude Xk)])
        (vector-set! psd k (/ (* mag mag) m))))))

;;; ============================================================
;;; Section 2: Welch's Method — Averaged Overlapping Segments
;;; ============================================================
;;;
;;; Welch's method reduces the variance of the periodogram by:
;;; 1. Dividing the signal into overlapping segments
;;; 2. Windowing each segment
;;; 3. Computing the periodogram of each segment
;;; 4. Averaging the periodograms
;;;
;;; The trade-off is reduced frequency resolution (shorter segments)
;;; for reduced variance (more averages).

(doc 'section 'welch)

;;; ts-welch-periodogram : Vector[Number] x Nat x Nat x Symbol -> Vector[Number]
;;; Compute PSD using Welch's method.
;;;
;;; Parameters:
;;;   xs         - Input time series
;;;   seg-len    - Segment length (should be power of 2 for FFT efficiency)
;;;   overlap    - Number of overlapping samples between segments
;;;   win-type   - Window type ('hann, 'hamming, 'blackman, 'rectangular)
;;;
;;; Returns: PSD estimate of length seg-len.
;;;
;;; Common usage: 50% overlap with Hann window.
;;;   (ts-welch-periodogram xs 256 128 'hann)
(define (ts-welch-periodogram xs seg-len overlap win-type)
  (doc 'export #t)
  (doc 'type '(-> Vec Nat Nat Symbol Vec))
  (let* ([n (vector-length xs)]
         [hop (- seg-len overlap)]
         [num-segs (max 1 (+ 1 (quotient (- n seg-len) hop)))]
         [window (make-window win-type seg-len)]
         [win-energy (window-energy window)]
         [psd (make-vector seg-len 0)])
    ;; Accumulate periodograms from each segment
    (do ([s 0 (+ s 1)])
        ((= s num-segs))
      (let* ([start (* s hop)]
             [seg (make-vector seg-len 0)])
        ;; Extract segment
        (do ([i 0 (+ i 1)])
            ((= i seg-len))
          (let ([idx (+ start i)])
            (when (< idx n)
              (vector-set! seg i (vector-ref xs idx)))))
        ;; Apply window
        (let ([windowed (apply-window seg window)])
          ;; Compute DFT and accumulate |X[k]|^2
          (let ([X (dft-real windowed)])
            (do ([k 0 (+ k 1)])
                ((= k seg-len))
              (let* ([Xk (vector-ref X k)]
                     [mag (complex-magnitude Xk)]
                     [power (* mag mag)])
                (vector-set! psd k (+ (vector-ref psd k) power))))))))
    ;; Average and normalize by window energy
    (do ([k 0 (+ k 1)])
        ((= k seg-len) psd)
      (vector-set! psd k (/ (vector-ref psd k)
                            (* num-segs seg-len win-energy))))))

;;; ============================================================
;;; Section 3: AR Spectral Density
;;; ============================================================
;;;
;;; For an AR(p) model with coefficients phi_1, ..., phi_p and
;;; innovation variance sigma^2, the theoretical spectral density is:
;;;
;;;   S(f) = sigma^2 / |1 - phi_1 e^{-i2pi f} - ... - phi_p e^{-i2pi pf}|^2
;;;
;;; This is a parametric PSD estimator. Advantages over periodogram:
;;; - Smooth spectrum (no leakage artifacts)
;;; - Better frequency resolution for short signals
;;; - Directly interpretable in terms of AR poles
;;;
;;; The input is either a fitted AR model result or raw coefficients.

(doc 'section 'ar-spectral)

;;; ts-ar-spectral-density : Vector[Number] x Number x Nat -> Vector[Number]
;;; Compute AR spectral density at N uniformly spaced frequencies.
;;;
;;; Parameters:
;;;   phi     - AR coefficients #(phi_1 ... phi_p)
;;;   sigma   - Innovation standard deviation
;;;   n-freqs - Number of frequency evaluation points
;;;
;;; Returns: PSD vector of length n-freqs, evaluated at
;;;   f = 0, 1/n-freqs, 2/n-freqs, ..., (n-freqs-1)/n-freqs
;;;   (normalized frequency, cycles per sample).
(define (ts-ar-spectral-density phi sigma n-freqs)
  (doc 'export #t)
  (doc 'type '(-> Vec Num Nat Vec))
  (let* ([p (vector-length phi)]
         [sigma2 (* sigma sigma)]
         [two-pi (* 2 (pi-value))]
         [psd (make-vector n-freqs)])
    (do ([k 0 (+ k 1)])
        ((= k n-freqs) psd)
      (let* ([f (/ k n-freqs)]
             ;; Compute A(f) = 1 - sum_{j=1}^{p} phi_j * e^{-i*2*pi*f*j}
             ;; Real part: 1 - sum phi_j cos(2*pi*f*j)
             ;; Imag part:     sum phi_j sin(2*pi*f*j)
             [re (let loop ([j 0] [s 1.0])
                   (if (= j p)
                       s
                       (loop (+ j 1)
                             (- s (* (vector-ref phi j)
                                     (cos (* two-pi f (+ j 1))))))))]
             [im (let loop ([j 0] [s 0.0])
                   (if (= j p)
                       s
                       (loop (+ j 1)
                             (+ s (* (vector-ref phi j)
                                     (sin (* two-pi f (+ j 1))))))))]
             [mag2 (+ (* re re) (* im im))])
        (vector-set! psd k (/ sigma2 (max mag2 1e-30)))))))

;;; ts-ar-spectral-density-from-model : ARResult x Nat -> Vector[Number]
;;; Convenience: extract coefficients and sigma from fitted AR model.
(define (ts-ar-spectral-density-from-model model n-freqs)
  (doc 'export #t)
  (doc 'type '(-> ARResult Nat Vec))
  (ts-ar-spectral-density (ar-coefficients model) (ar-sigma model) n-freqs))

;;; ============================================================
;;; Section 4: Spectral Entropy
;;; ============================================================
;;;
;;; Spectral entropy measures how "spread out" the power spectrum is.
;;; A pure sinusoid has all power at one frequency -> low entropy.
;;; White noise has uniform power -> maximum entropy.
;;;
;;; H = -sum(p_k * log(p_k)) / log(N)
;;;
;;; where p_k = P[k] / sum(P) is the normalized PSD.
;;; Division by log(N) normalizes to [0, 1].
;;;
;;; Applications: signal complexity, EEG analysis, anomaly detection.

(doc 'section 'spectral-entropy)

;;; ts-spectral-entropy : Vector[Number] -> Number
;;; Compute normalized spectral entropy of a time series.
;;;
;;; Returns a value in [0, 1]:
;;;   0 = pure tone (all power at single frequency)
;;;   1 = white noise (uniform spectrum)
;;;
;;; Uses the one-sided spectrum (bins 0..N/2) to avoid
;;; double-counting symmetric frequencies of real signals.
(define (ts-spectral-entropy xs)
  (doc 'export #t)
  (doc 'type '(-> Vec Num))
  (let* ([psd (ts-periodogram xs)]
         [n (vector-length psd)]
         ;; Use one-sided spectrum: bins 0 through N/2
         [half (+ 1 (quotient n 2))]
         ;; Sum total power in one-sided spectrum
         [total (let loop ([k 0] [s 0.0])
                  (if (= k half) s
                      (loop (+ k 1) (+ s (vector-ref psd k)))))]
         ;; Compute entropy
         [H (if (< total 1e-30)
                0.0  ; No power -> undefined, return 0
                (let loop ([k 0] [s 0.0])
                  (if (= k half) s
                      (let* ([pk (/ (vector-ref psd k) total)])
                        (if (< pk 1e-30)
                            (loop (+ k 1) s)
                            (loop (+ k 1)
                                  (- s (* pk (log pk)))))))))])
    ;; Normalize by log(half) so result is in [0, 1]
    (if (<= half 1)
        0.0
        (/ H (log half)))))

;;; ============================================================
;;; Section 5: Dominant Frequency
;;; ============================================================
;;;
;;; Find the frequency bin with maximum power in the periodogram.
;;; Returns normalized frequency in [0, 0.5] (Nyquist).

(doc 'section 'dominant-frequency)

;;; ts-dominant-frequency : Vector[Number] -> (Number . Number)
;;; Find the dominant frequency in a signal.
;;;
;;; Returns: (frequency . power) pair where frequency is in
;;; cycles per sample (0 to 0.5 = Nyquist).
;;;
;;; Excludes the DC component (bin 0) from the search.
;;; To convert to Hz: multiply frequency by sample rate.
(define (ts-dominant-frequency xs)
  (doc 'export #t)
  (doc 'type '(-> Vec (Pair Num Num)))
  (let* ([psd (ts-periodogram xs)]
         [n (vector-length psd)]
         [half (+ 1 (quotient n 2))])
    ;; Search bins 1..N/2 (skip DC)
    (let loop ([k 1] [best-k 1] [best-power (vector-ref psd 1)])
      (if (= k half)
          (cons (/ best-k n) best-power)  ; normalized frequency
          (let ([pk (vector-ref psd k)])
            (if (> pk best-power)
                (loop (+ k 1) k pk)
                (loop (+ k 1) best-k best-power)))))))

;;; ============================================================
;;; Section 6: Spectral White Noise Test
;;; ============================================================
;;;
;;; Under the null hypothesis that x is white noise, the periodogram
;;; ordinates (at non-zero frequencies) are i.i.d. exponential.
;;; The normalized cumulative periodogram should follow a uniform
;;; distribution on [0, 1].
;;;
;;; We implement Fisher's g-test (maximum periodogram ordinate test):
;;;
;;;   g = max(I_k) / sum(I_k)
;;;
;;; where I_k are the periodogram values at Fourier frequencies.
;;; Under H0 (white noise), g has a known distribution based on
;;; the number of Fourier frequencies.
;;;
;;; Alternatively, the Bartlett test uses the chi-squared distribution
;;; on binned periodogram values.

(doc 'section 'white-noise-test)

;;; ts-spectral-white-noise-test : Vector[Number] -> TestResult
;;; Test whether a time series has a flat (white noise) spectrum.
;;;
;;; Uses Fisher's g-test (exact test for periodicity in white noise):
;;;   g = max(I_k) / sum(I_k)
;;; where I_k are periodogram ordinates at Fourier frequencies.
;;;
;;; Under H0 (white noise), the exact upper-tail probability is:
;;;   P(g > x) = sum_{j=1}^{floor(1/x)} (-1)^{j+1} * C(m,j) * (1 - j*x)^{m-1}
;;; where m is the number of independent Fourier frequencies.
;;;
;;; A sinusoid concentrates all power at one bin, giving g close to 1.
;;; White noise distributes power uniformly, giving g close to 1/m.
;;;
;;; Returns a TestResult:
;;;   test-name:   'spectral-white-noise
;;;   statistic:   Fisher's g
;;;   df:          m (number of independent frequencies)
;;;   p-value:     exact upper-tail probability
;;;   ci:          #f
;;;   effect-size: Fisher's g
(define (ts-spectral-white-noise-test xs)
  (doc 'export #t)
  (doc 'type '(-> Vec TestResult))
  (let* ([psd (ts-periodogram xs)]
         [n (vector-length psd)]
         [half (quotient n 2)]
         ;; Collect periodogram ordinates at positive frequencies 1..half-1
         [m (- half 1)]  ; number of independent ordinates
         [ordinates (make-vector m)]
         [_ (do ([k 0 (+ k 1)])
                ((= k m))
              (vector-set! ordinates k (vector-ref psd (+ k 1))))]
         ;; Total power at these frequencies
         [total-power (let loop ([k 0] [s 0.0])
                        (if (= k m) s
                            (loop (+ k 1) (+ s (vector-ref ordinates k)))))]
         ;; Fisher's g: max ordinate / total
         [max-ord (let loop ([k 0] [mx 0.0])
                    (if (= k m) mx
                        (let ([v (vector-ref ordinates k)])
                          (loop (+ k 1) (if (> v mx) v mx)))))]
         [fisher-g (if (< total-power 1e-30) 0.0 (/ max-ord total-power))]
         ;; Exact p-value for Fisher's g test
         ;; P(g > x) = sum_{j=1}^{floor(1/x)} (-1)^{j+1} * C(m,j) * (1-jx)^{m-1}
         [p-value (fisher-g-pvalue fisher-g m)])
    (make-test-result 'spectral-white-noise fisher-g m p-value #f fisher-g)))

;;; fisher-g-pvalue : Number x Nat -> Number
;;; Exact upper-tail probability for Fisher's g statistic.
;;; P(g > x | m frequencies) using the inclusion-exclusion formula.
(define (fisher-g-pvalue g m)
  (cond
    [(<= m 1) 1.0]
    [(<= g 0) 1.0]
    [(>= g 1) 0.0]  ; All power at one frequency -> maximally non-white
    [else
      (let* ([max-j (inexact->exact (floor (/ 1.0 g)))]
             [max-j (min max-j m)])  ; Can't exceed m
        ;; Compute sum using inclusion-exclusion
        ;; Use log-space for binomial coefficients to avoid overflow
        (let loop ([j 1] [sum 0.0])
          (if (> j max-j)
              (max 0.0 (min 1.0 sum))  ; Clamp to [0, 1]
              (let* ([sign (if (odd? j) 1.0 -1.0)]
                     [binom (binomial m j)]
                     [base (- 1.0 (* j g))]
                     [term (* sign binom (expt base (- m 1)))])
                (loop (+ j 1) (+ sum term))))))]))
