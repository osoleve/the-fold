;;; lattice/numeric/convolution.ss — Convolution and Correlation
;;; @module convolution
;;; @requires prelude complex dft vec

(require 'prelude)
(require 'complex)
(require 'dft)
(require 'vec)

(doc 'module 'convolution)
(doc 'description "Comprehensive convolution and correlation implementations for signal processing, filtering, template matching, and feature extraction")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Convolution Modes
;;; ====

;;; Convolution can produce outputs of different sizes depending on mode:
;;; - 'full: Output length = N + M - 1 (all overlaps)
;;; - 'same: Output length = N (same as first input)
;;; - 'valid: Output length = N - M + 1 (only complete overlaps)

;;; ====
;;; Direct Convolution (Time Domain)
;;; ====

;;; convolve-direct-full : Vector[Number] × Vector[Number] → Vector[Number]
;;; Compute full convolution using direct time-domain method.
;;; Output length: N + M - 1
;;; Complexity: O(N * M)
;;;
;;; (f * g)[n] = Σ(k=0 to M-1) f[n-k] * g[k]
(define (convolve-direct-full signal kernel)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)])
        (if (or (= n 0) (= m 0))
            (vector)  ; Empty input => empty output
            (let* ([output-len (+ n m -1)]
                   [result (make-vector output-len 0)])
                  (do ([i 0 (+ i 1)])
                      ((= i output-len) result)
                      (let ([sum 0])
                           (do ([j 0 (+ j 1)])
                               ((= j m)
                                (vector-set! result i sum))
                               (let ([k (- i j)])
                                    (when (and (>= k 0) (< k n))
                                          (set! sum (+ sum (* (vector-ref signal k)
                                                              (vector-ref kernel j)))))))))))))

;;; convolve-direct-same : Vector[Number] × Vector[Number] → Vector[Number]
;;; Compute 'same' mode convolution (output length = signal length).
;;; Complexity: O(N * M)
(define (convolve-direct-same signal kernel)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)]
         [result (make-vector n 0)]
         [offset (quotient m 2)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([sum 0])
                 (do ([j 0 (+ j 1)])
                     ((= j m)
                      (vector-set! result i sum))
                     (let ([k (- (+ i offset) j)])
                          (when (and (>= k 0) (< k n))
                                (set! sum (+ sum (* (vector-ref signal k)
                                                    (vector-ref kernel j)))))))))))

;;; convolve-direct-valid : Vector[Number] × Vector[Number] → Vector[Number]
;;; Compute 'valid' mode convolution (only complete overlaps).
;;; Output length: N - M + 1 (or 0 if M > N)
;;; Complexity: O((N-M+1) * M)
(define (convolve-direct-valid signal kernel)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)]
         [output-len (max 0 (+ (- n m) 1))])
        (if (= output-len 0)
            (vector)
            (let ([result (make-vector output-len 0)])
                 (do ([i 0 (+ i 1)])
                     ((= i output-len) result)
                     (let ([sum 0])
                          (do ([j 0 (+ j 1)])
                              ((= j m)
                               (vector-set! result i sum))
                              (set! sum (+ sum (* (vector-ref signal (+ i j))
                                                  (vector-ref kernel j)))))))))))

;;; ====
;;; FFT-based Convolution (Frequency Domain)
;;; ====

;;; convolve-fft : Vector[Number] × Vector[Number] → Vector[Number]
;;; Compute convolution using FFT (convolution theorem).
;;; Output length: N + M - 1 (full convolution)
;;; Complexity: O((N+M) log (N+M))
;;;
;;; Convolution Theorem: conv(f, g) = IFFT(FFT(f) * FFT(g))
(define (convolve-fft signal kernel)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)]
         [output-len (+ n m -1)]
         ;; Pad to next power of 2 for efficient FFT
         [fft-size (next-power-of-2 output-len)]
         ;; Zero-pad both inputs
         [signal-padded (zero-pad (real->complex-vec signal) fft-size)]
         [kernel-padded (zero-pad (real->complex-vec kernel) fft-size)]
         ;; Transform to frequency domain
         [signal-fft (fft-radix2 signal-padded)]
         [kernel-fft (fft-radix2 kernel-padded)]
         ;; Multiply in frequency domain
         [product (make-vector fft-size)]
         ;; Transform back to time domain
         [result-complex #f]
         [result #f])
        ;; Element-wise multiplication
        (do ([i 0 (+ i 1)])
            ((= i fft-size))
            (vector-set! product i
                         (complex-mul (vector-ref signal-fft i)
                                      (vector-ref kernel-fft i))))
        ;; Inverse FFT
        (set! result-complex (ifft-radix2 product))
        ;; Extract real part and trim to correct length
        (set! result (make-vector output-len))
        (do ([i 0 (+ i 1)])
            ((= i output-len) result)
            (vector-set! result i (complex-real (vector-ref result-complex i))))))

;;; convolve : Vector[Number] × Vector[Number] × Symbol → Vector[Number]
;;; Compute convolution with automatic algorithm selection.
;;; mode: 'full, 'same, or 'valid
;;;
;;; Algorithm selection:
;;; - Small kernels (M < 32): Use direct convolution
;;; - Large kernels: Use FFT-based convolution
(define (convolve signal kernel mode)
  (doc 'export #t)
  (let ([m (vector-length kernel)])
       (cond
        [(< m 32)
         ;; Use direct convolution for small kernels
         (case mode
               [(full) (convolve-direct-full signal kernel)]
               [(same) (convolve-direct-same signal kernel)]
               [(valid) (convolve-direct-valid signal kernel)]
               [else (error 'convolve "invalid mode" mode)])]
        [else
         ;; Use FFT convolution for large kernels
         (let ([result-full (convolve-fft signal kernel)])
              (case mode
                    [(full) result-full]
                    [(same)
                     ;; Trim to same length as signal
                     (let* ([n (vector-length signal)]
                            [offset (quotient m 2)]
                            [trimmed (make-vector n)])
                           (do ([i 0 (+ i 1)])
                               ((= i n) trimmed)
                               (vector-set! trimmed i (vector-ref result-full (+ i offset)))))]
                    [(valid)
                     ;; Trim to valid region
                     (let* ([n (vector-length signal)]
                            [output-len (max 0 (+ (- n m) 1))])
                           (if (= output-len 0)
                               (vector)
                               (let ([trimmed (make-vector output-len)])
                                    (do ([i 0 (+ i 1)])
                                        ((= i output-len) trimmed)
                                        (vector-set! trimmed i
                                                     (vector-ref result-full (+ i (- m 1))))))))]
                    [else (error 'convolve "invalid mode" mode)]))])))

;;; ====
;;; Correlation Operations
;;; ====

;;; correlate-direct : Vector[Number] × Vector[Number] × Symbol → Vector[Number]
;;; Compute cross-correlation using direct method.
;;; Cross-correlation is like convolution but without reversing the kernel.
;;;
;;; (f ⋆ g)[n] = Σ(k=0 to M-1) f[n+k] * g[k]
(define (correlate-direct signal kernel mode)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)])
        (case mode
              [(full)
               (let* ([output-len (+ n m -1)]
                      [result (make-vector output-len 0)])
                     (do ([i 0 (+ i 1)])
                         ((= i output-len) result)
                         (let ([sum 0])
                              (do ([j 0 (+ j 1)])
                                  ((= j m)
                                   (vector-set! result i sum))
                                  (let ([k (- i (- m 1) (- j))])
                                       (when (and (>= k 0) (< k n))
                                             (set! sum (+ sum (* (vector-ref signal k)
                                                                 (vector-ref kernel j))))))))))]
              [(same)
               (let* ([result (make-vector n 0)]
                      [offset (quotient m 2)])
                     (do ([i 0 (+ i 1)])
                         ((= i n) result)
                         (let ([sum 0])
                              (do ([j 0 (+ j 1)])
                                  ((= j m)
                                   (vector-set! result i sum))
                                  (let ([k (+ i j (- offset))])
                                       (when (and (>= k 0) (< k n))
                                             (set! sum (+ sum (* (vector-ref signal k)
                                                                 (vector-ref kernel j))))))))))]
              [(valid)
               (let* ([output-len (max 0 (+ (- n m) 1))]
                      [result (make-vector output-len 0)])
                     (do ([i 0 (+ i 1)])
                         ((= i output-len) result)
                         (let ([sum 0])
                              (do ([j 0 (+ j 1)])
                                  ((= j m)
                                   (vector-set! result i sum))
                                  (set! sum (+ sum (* (vector-ref signal (+ i j))
                                                      (vector-ref kernel j)))))))
                     result)]
              [else (error 'correlate-direct "invalid mode" mode)])))

;;; correlate : Vector[Number] × Vector[Number] × Symbol → Vector[Number]
;;; Compute cross-correlation with automatic algorithm selection.
(define (correlate signal kernel mode)
  (doc 'export #t)
  (let ([m (vector-length kernel)])
       (if (< m 32)
           (correlate-direct signal kernel mode)
           ;; Use FFT: correlation(f,g) = convolve(f, reverse(g))
           (let* ([kernel-rev (vector-reverse kernel)])
                 (convolve signal kernel-rev mode)))))

;;; autocorrelate : Vector[Number] × Symbol → Vector[Number]
;;; Compute auto-correlation (signal correlated with itself).
;;; Useful for detecting periodicity and self-similarity.
(define (autocorrelate signal mode)
  (doc 'export #t)
  (correlate signal signal mode))

;;; ====
;;; Matched Filtering
;;; ====

;;; matched-filter : Vector[Number] × Vector[Number] → Vector[Number]
;;; Apply matched filter (template matching).
;;; Returns correlation peaks indicating template matches.
;;;
;;; The matched filter is the time-reversed complex conjugate of the template,
;;; but for real signals it's just cross-correlation.
(define (matched-filter signal template)
  (doc 'export #t)
  (correlate signal template 'same))

;;; find-peaks : Vector[Number] × Number → Vector[Integer]
;;; Find indices where signal exceeds threshold.
;;; Useful for detecting matched filter hits.
(define (find-peaks signal threshold)
  (doc 'export #t)
  (let ([n (vector-length signal)]
        [peaks '()])
       (if (< n 3)
           ;; Not enough points for peak detection
           (vector)
           (begin
            (do ([i 1 (+ i 1)])
                ((= i (- n 1)) (list->vector (reverse peaks)))
                (let ([curr (vector-ref signal i)]
                      [prev (vector-ref signal (- i 1))]
                      [next (vector-ref signal (+ i 1))])
                     (when (and (> curr threshold)
                                (>= curr prev)
                                (>= curr next))
                           (set! peaks (cons i peaks)))))))))

;;; ====
;;; Utility Functions
;;; ====

;;; vector-reverse : Vector → Vector
;;; Reverse a vector (for convolution/correlation conversion).
(define (vector-reverse v)
  (doc 'export #t)
  (let* ([n (vector-length v)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (vector-ref v (- n 1 i))))))

;;; normalize-signal : Vector[Number] → Vector[Number]
;;; Normalize signal to have zero mean and unit variance.
;;; Useful for preprocessing before correlation.
(define (normalize-signal signal)
  (doc 'export #t)
  (let ([n (vector-length signal)])
       (if (= n 0)
           (vector)  ; Empty input => empty output
           (let* ([;; Calculate mean
                   sum 0]
                  [mean 0]
                  ;; Calculate variance
                  [var-sum 0]
                  [variance 0]
                  [stddev 0]
                  [result (make-vector n)])
                 ;; Compute mean
                 (do ([i 0 (+ i 1)])
                     ((= i n))
                     (set! sum (+ sum (vector-ref signal i))))
                 (set! mean (/ sum n))
                 ;; Compute variance
                 (do ([i 0 (+ i 1)])
                     ((= i n))
                     (let ([diff (- (vector-ref signal i) mean)])
                          (set! var-sum (+ var-sum (* diff diff)))))
                 (set! variance (/ var-sum n))
                 (set! stddev (sqrt variance))
                 ;; Normalize
                 (if (< stddev 1e-10)
                     ;; Signal is constant, return zeros
                     result
                     (begin
                      (do ([i 0 (+ i 1)])
                          ((= i n) result)
                          (vector-set! result i
                                       (/ (- (vector-ref signal i) mean) stddev)))))))))

;;; ====
;;; 2D Convolution (for images, matrices)
;;; ====

;;; convolve-2d : Matrix × Matrix → Matrix
;;; 2D convolution for image processing.
;;; This is a placeholder for future implementation.
;;; Currently not implemented - would require matrix data structure.
(define (convolve-2d image kernel)
  (doc 'export #t)
  (error 'convolve-2d "2D convolution not yet implemented"
         "Requires matrix data structure"))

;;; ====
;;; Circular Convolution
;;; ====

;;; convolve-circular : Vector[Number] × Vector[Number] → Vector[Number]
;;; Compute circular (cyclic) convolution.
;;; Output length equals signal length (wraps around).
;;;
;;; Efficient implementation using FFT.
(define (convolve-circular signal kernel)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)])
        (if (not (= n m))
            (error 'convolve-circular "signal and kernel must have same length"
                   (list n m))
            (let* ([signal-complex (real->complex-vec signal)]
                   [kernel-complex (real->complex-vec kernel)]
                   ;; Pad to power of 2 if needed
                   [fft-size (if (power-of-2? n)
                                 n
                                 (next-power-of-2 n))]
                   [signal-padded (if (= n fft-size)
                                      signal-complex
                                      (zero-pad signal-complex fft-size))]
                   [kernel-padded (if (= m fft-size)
                                      kernel-complex
                                      (zero-pad kernel-complex fft-size))]
                   ;; FFT
                   [signal-fft (fft-radix2 signal-padded)]
                   [kernel-fft (fft-radix2 kernel-padded)]
                   ;; Multiply
                   [product (make-vector fft-size)]
                   ;; IFFT
                   [result-complex #f]
                   [result (make-vector n)])
                  ;; Element-wise multiply
                  (do ([i 0 (+ i 1)])
                      ((= i fft-size))
                      (vector-set! product i
                                   (complex-mul (vector-ref signal-fft i)
                                                (vector-ref kernel-fft i))))
                  ;; Inverse transform
                  (set! result-complex (ifft-radix2 product))
                  ;; Extract real part (trim if padded)
                  (do ([i 0 (+ i 1)])
                      ((= i n) result)
                      (vector-set! result i
                                   (complex-real (vector-ref result-complex i))))))))
