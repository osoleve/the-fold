;;; lattice/numeric/window-functions.ss — Window Functions for Signal Processing
;;; @module window-functions
;;; @requires prelude

(require 'prelude)

(doc 'module 'window-functions)
(doc 'description "Window functions for signal processing: reduce spectral leakage, smooth signal boundaries for Fourier analysis")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Basic Window Functions
;;; ====

;;; rectangular-window : Integer → Vector[Number]
;;; Rectangular (boxcar) window - no tapering.
;;; w[n] = 1 for all n
;;;
;;; Properties:
;;; - Narrowest main lobe
;;; - Highest side lobes (-13 dB)
;;; - Use when no windowing is desired
(define (rectangular-window n)
  (doc 'export #t)
  (let ([result (make-vector n)])
       (do ([i 0 (+ i 1)])
           ((= i n) result)
           (vector-set! result i 1.0))))

;;; hann-window : Integer → Vector[Number]
;;; Hann (Hanning) window.
;;; w[n] = 0.5 * (1 - cos(2π*n/(N-1)))
;;;
;;; Properties:
;;; - Good frequency resolution
;;; - Side lobes at -31 dB
;;; - Commonly used for general purpose spectral analysis
;;; - End points go to zero
(define (hann-window n)
  (doc 'export #t)
  (let ([result (make-vector n)]
        [two-pi (* 2 (pi-value))]
        [n-1 (- n 1)])
       (do ([i 0 (+ i 1)])
           ((= i n) result)
           (let ([term (/ (* two-pi i) n-1)])
                (vector-set! result i (* 0.5 (- 1 (cos term))))))))

;;; hamming-window : Integer → Vector[Number]
;;; Hamming window.
;;; w[n] = 0.54 - 0.46 * cos(2π*n/(N-1))
;;;
;;; Properties:
;;; - Similar to Hann but with slightly different coefficients
;;; - Side lobes at -43 dB (better than Hann)
;;; - Non-zero endpoints (unlike Hann)
;;; - Optimized to minimize first side lobe
(define (hamming-window n)
  (doc 'export #t)
  (let ([result (make-vector n)]
        [two-pi (* 2 (pi-value))]
        [n-1 (- n 1)])
       (do ([i 0 (+ i 1)])
           ((= i n) result)
           (let ([term (/ (* two-pi i) n-1)])
                (vector-set! result i (- 0.54 (* 0.46 (cos term))))))))

;;; blackman-window : Integer → Vector[Number]
;;; Blackman window.
;;; w[n] = 0.42 - 0.5*cos(2π*n/(N-1)) + 0.08*cos(4π*n/(N-1))
;;;
;;; Properties:
;;; - Excellent side lobe suppression (-58 dB)
;;; - Wider main lobe than Hann/Hamming
;;; - Good for applications requiring low spectral leakage
(define (blackman-window n)
  (doc 'export #t)
  (let ([result (make-vector n)]
        [two-pi (* 2 (pi-value))]
        [four-pi (* 4 (pi-value))]
        [n-1 (- n 1)])
       (do ([i 0 (+ i 1)])
           ((= i n) result)
           (let ([term1 (/ (* two-pi i) n-1)]
                 [term2 (/ (* four-pi i) n-1)])
                (vector-set! result i
                             (+ 0.42
                                (- (* 0.5 (cos term1)))
                                (* 0.08 (cos term2))))))))

;;; bartlett-window : Integer → Vector[Number]
;;; Bartlett (triangular) window.
;;; w[n] = 1 - |n - (N-1)/2| / ((N-1)/2)
;;;
;;; Properties:
;;; - Linear taper to zero at endpoints
;;; - Side lobes at -27 dB
;;; - Simple and computationally efficient
(define (bartlett-window n)
  (doc 'export #t)
  (let ([result (make-vector n)]
        [n-1 (- n 1)]
        [mid (/ (- n 1) 2.0)])
       (do ([i 0 (+ i 1)])
           ((= i n) result)
           (vector-set! result i (- 1 (/ (abs (- i mid)) mid))))))

;;; ====
;;; Kaiser Window (Parameterized)
;;; ====

;;; Modified Bessel function of the first kind, order 0 (I₀)
;;; Used for Kaiser window computation.
;;;
;;; Approximation using series expansion:
;;; I₀(x) = Σ(k=0 to ∞) [(x/2)^k / k!]²
;;;
;;; We compute until terms become negligibly small (< 1e-10).
(define (bessel-i0 x)
  (doc 'export #t)
  (let ([half-x (/ x 2)])
       (let loop ([k 0]
                  [term 1.0]
                  [sum 1.0])
            (if (> k 100)  ; Safety limit
                sum
                (let* ([k+1 (+ k 1)]
                       [new-term (* term (/ (* half-x half-x)
                                            (* k+1 k+1)))])
                      (if (< new-term 1e-10)
                          sum
                          (loop k+1 new-term (+ sum new-term))))))))

;;; kaiser-window : Integer × Number → Vector[Number]
;;; Kaiser window with shape parameter beta.
;;; w[n] = I₀(β * sqrt(1 - ((n - N/2) / (N/2))²)) / I₀(β)
;;;
;;; The beta parameter controls the trade-off between main lobe width
;;; and side lobe level:
;;; - beta = 0: Rectangular window
;;; - beta = 5: Similar to Hamming
;;; - beta = 6: Similar to Hann
;;; - beta = 8.6: Similar to Blackman
;;; - beta > 10: Very low side lobes but wider main lobe
;;;
;;; Common choices:
;;; - beta = 5.44: For audio applications
;;; - beta = 8.6: For low side lobe applications
(define (kaiser-window n beta)
  (doc 'export #t)
  (let ([result (make-vector n)]
        [denom (bessel-i0 beta)]
        [n-1 (- n 1)]
        [mid (/ (- n 1) 2.0)])
       (do ([i 0 (+ i 1)])
           ((= i n) result)
           (let* ([x (/ (- i mid) mid)]
                  [arg (* beta (sqrt (- 1 (* x x))))]
                  [numer (bessel-i0 arg)])
                 (vector-set! result i (/ numer denom))))))

;;; ====
;;; Window Application
;;; ====

;;; apply-window : Vector[Number] × Vector[Number] → Vector[Number]
;;; Apply a window function to a signal by element-wise multiplication.
;;;
;;; Requirements:
;;; - signal and window must have the same length
;;;
;;; Returns: windowed signal w[n] * x[n]
(define (apply-window signal window)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length window)])
        (if (not (= n m))
            (error 'apply-window "signal and window must have same length"
                   (list 'signal-length n 'window-length m))
            (let ([result (make-vector n)])
                 (do ([i 0 (+ i 1)])
                     ((= i n) result)
                     (vector-set! result i
                                  (* (vector-ref signal i)
                                     (vector-ref window i))))))))

;;; apply-window-complex : Vector[Complex] × Vector[Number] → Vector[Complex]
;;; Apply a window function to a complex-valued signal.
(define (apply-window-complex signal window)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length window)])
        (if (not (= n m))
            (error 'apply-window-complex "signal and window must have same length"
                   (list 'signal-length n 'window-length m))
            (let ([result (make-vector n)])
                 (do ([i 0 (+ i 1)])
                     ((= i n) result)
                     ;; Assuming complex-scale exists in complex.ss
                     (let ([c (vector-ref signal i)]
                           [w (vector-ref window i)])
                          (vector-set! result i
                                       (make-complex (* (complex-real c) w)
                                                     (* (complex-imag c) w)))))))))

;;; ====
;;; Window Properties
;;; ====

;;; window-energy : Vector[Number] → Number
;;; Compute the energy (sum of squares) of a window.
;;; E = Σ w[n]²
;;;
;;; Used for normalizing spectral estimates.
(define (window-energy window)
  (doc 'export #t)
  (let ([n (vector-length window)])
       (let loop ([i 0] [sum 0])
            (if (= i n)
                sum
                (let ([w (vector-ref window i)])
                     (loop (+ i 1) (+ sum (* w w))))))))

;;; window-power : Vector[Number] → Number
;;; Compute the power (sum) of a window.
;;; P = Σ w[n]
;;;
;;; Used for normalizing amplitude estimates.
(define (window-power window)
  (doc 'export #t)
  (let ([n (vector-length window)])
       (let loop ([i 0] [sum 0])
            (if (= i n)
                sum
                (loop (+ i 1) (+ sum (vector-ref window i)))))))

;;; ====
;;; Window Selection Utilities
;;; ====

;;; make-window : Symbol × Integer × (Optional Number) → Vector[Number]
;;; Create a window function by name.
;;;
;;; Types:
;;; - 'rectangular, 'boxcar
;;; - 'hann, 'hanning
;;; - 'hamming
;;; - 'blackman
;;; - 'bartlett, 'triangular
;;; - 'kaiser (requires beta parameter)
;;;
;;; Example:
;;;   (make-window 'hann 512)
;;;   (make-window 'kaiser 512 8.6)
(define (make-window type n . params)
  (doc 'export #t)
  (case type
        [(rectangular boxcar) (rectangular-window n)]
        [(hann hanning) (hann-window n)]
        [(hamming) (hamming-window n)]
        [(blackman) (blackman-window n)]
        [(bartlett triangular) (bartlett-window n)]
        [(kaiser) (if (null? params)
                      (error 'make-window "kaiser window requires beta parameter")
                      (kaiser-window n (car params)))]
        [else (error 'make-window "unknown window type" type)]))
