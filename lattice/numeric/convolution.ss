;;; lattice/numeric/convolution.ss — Convolution and Correlation
;;; @module convolution
;;; @requires prelude complex dft iteration
;;; @description Linear convolution: direct and FFT-based. Correlation and autocorrelation. 2D convolution for image processing. Matched filtering.
;;; @purity total
;;; @stability stable

(require 'prelude)
(require 'complex)
(require 'dft)
(require 'iteration)

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
            (let ([output-len (+ n m -1)])
                 (vec-tabulate output-len i
                   (range-fold sum 0 j 0 m
                     (let ([k (- i j)])
                          (if (and (>= k 0) (< k n))
                              (+ sum (* (vector-ref signal k)
                                        (vector-ref kernel j)))
                              sum))))))))

;;; convolve-direct-same : Vector[Number] × Vector[Number] → Vector[Number]
;;; Compute 'same' mode convolution (output length = signal length).
;;; Complexity: O(N * M)
(define (convolve-direct-same signal kernel)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)]
         [offset (quotient m 2)])
        (vec-tabulate n i
          (range-fold sum 0 j 0 m
            (let ([k (- (+ i offset) j)])
                 (if (and (>= k 0) (< k n))
                     (+ sum (* (vector-ref signal k)
                               (vector-ref kernel j)))
                     sum))))))

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
            (vec-tabulate output-len i
              (range-fold sum 0 j 0 m
                (+ sum (* (vector-ref signal (+ i j))
                          (vector-ref kernel j))))))))

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
         [product (vec-tabulate fft-size i
                    (complex-mul (vector-ref signal-fft i)
                                 (vector-ref kernel-fft i)))]
         ;; Transform back to time domain
         [result-complex (ifft-radix2 product)])
        ;; Extract real part and trim to correct length
        (vec-tabulate output-len i
          (complex-real (vector-ref result-complex i)))))

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
                            [offset (quotient m 2)])
                           (vec-tabulate n i
                             (vector-ref result-full (+ i offset))))]
                    [(valid)
                     ;; Trim to valid region
                     (let* ([n (vector-length signal)]
                            [output-len (max 0 (+ (- n m) 1))])
                           (if (= output-len 0)
                               (vector)
                               (vec-tabulate output-len i
                                 (vector-ref result-full (+ i (- m 1))))))]
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
               (let ([output-len (+ n m -1)])
                    (vec-tabulate output-len i
                      (range-fold sum 0 j 0 m
                        (let ([k (- i (- m 1) (- j))])
                             (if (and (>= k 0) (< k n))
                                 (+ sum (* (vector-ref signal k)
                                           (vector-ref kernel j)))
                                 sum)))))]
              [(same)
               (let ([offset (quotient m 2)])
                    (vec-tabulate n i
                      (range-fold sum 0 j 0 m
                        (let ([k (+ i j (- offset))])
                             (if (and (>= k 0) (< k n))
                                 (+ sum (* (vector-ref signal k)
                                           (vector-ref kernel j)))
                                 sum)))))]
              [(valid)
               (let ([output-len (max 0 (+ (- n m) 1))])
                    (vec-tabulate output-len i
                      (range-fold sum 0 j 0 m
                        (+ sum (* (vector-ref signal (+ i j))
                                  (vector-ref kernel j))))))]
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
  (let ([n (vector-length signal)])
       (if (< n 3)
           ;; Not enough points for peak detection
           (vector)
           (do ([i 1 (+ i 1)]
                [peaks '()
                       (let ([curr (vector-ref signal i)]
                             [prev (vector-ref signal (- i 1))]
                             [next (vector-ref signal (+ i 1))])
                            (if (and (> curr threshold)
                                     (>= curr prev)
                                     (>= curr next))
                                (cons i peaks)
                                peaks))])
               ((= i (- n 1)) (list->vector (reverse peaks)))))))

;;; ====
;;; Utility Functions
;;; ====

;;; vector-reverse : Vector → Vector
;;; Reverse a vector (for convolution/correlation conversion).
(define (vector-reverse v)
  (doc 'export #t)
  (let ([n (vector-length v)])
       (vec-tabulate n i
         (vector-ref v (- n 1 i)))))

;;; normalize-signal : Vector[Number] → Vector[Number]
;;; Normalize signal to have zero mean and unit variance.
;;; Useful for preprocessing before correlation.
(define (normalize-signal signal)
  (doc 'export #t)
  (let ([n (vector-length signal)])
       (if (= n 0)
           (vector)  ; Empty input => empty output
           (let* ([mean (/ (range-fold sum 0 i 0 n
                             (+ sum (vector-ref signal i)))
                           n)]
                  [variance (/ (range-fold var-sum 0 i 0 n
                                 (let ([diff (- (vector-ref signal i) mean)])
                                      (+ var-sum (* diff diff))))
                               n)]
                  [stddev (sqrt variance)])
                 (if (< stddev 1e-10)
                     ;; Signal is constant, return zeros
                     (make-vector n 0)
                     (vec-tabulate n i
                       (/ (- (vector-ref signal i) mean) stddev)))))))

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
                   ;; Multiply in frequency domain
                   [product (vec-tabulate fft-size i
                              (complex-mul (vector-ref signal-fft i)
                                           (vector-ref kernel-fft i)))]
                   ;; Inverse transform
                   [result-complex (ifft-radix2 product)])
                  ;; Extract real part (trim if padded)
                  (vec-tabulate n i
                    (complex-real (vector-ref result-complex i)))))))
