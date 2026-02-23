;;; lattice/numeric/fft-convolve.ss — FFT-based convolution bridge
;;; @module fft-convolve
;;; @requires prelude complex dft iteration

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'complex)
(require 'dft)
(require 'iteration)

(doc 'module 'fft-convolve)
(doc 'description "FFT-based convolution bridge: wires together DFT, pointwise multiply, and IDFT into the standard frequency-domain filtering pipeline. Provides linear convolution, cross-correlation, and frequency-domain filtering via FFT.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Internal helpers
;;; ====

;;; complex-pointwise-mul : Vector[Complex] x Vector[Complex] -> Vector[Complex]
;;; Element-wise complex multiplication of two equal-length vectors.
(define (complex-pointwise-mul a b)
  (let ([n (vector-length a)])
    (vec-tabulate n i
      (complex-mul (vector-ref a i)
                   (vector-ref b i)))))

;;; complex-pointwise-conj-mul : Vector[Complex] x Vector[Complex] -> Vector[Complex]
;;; Element-wise multiplication of conj(a[i]) * b[i].
;;; This is the cross-spectrum: conj(A) * B in frequency domain gives
;;; the correlation of a with b in time domain after IDFT.
(define (complex-pointwise-conj-mul a b)
  (let ([n (vector-length a)])
    (vec-tabulate n i
      (complex-mul (complex-conjugate (vector-ref a i))
                   (vector-ref b i)))))

;;; pad-to-fft-size : Vector[Number] x Integer -> Vector[Complex]
;;; Zero-pad a real-valued vector to length n and convert to complex.
(define (pad-to-fft-size v n)
  (zero-pad (real->complex-vec v) n))

;;; vec-reverse : Vector -> Vector
;;; Reverse a vector.
(define (vec-reverse v)
  (let ([n (vector-length v)])
    (vec-tabulate n i (vector-ref v (- n 1 i)))))

;;; ====
;;; FFT-based convolution (linear)
;;; ====

;;; fft-convolve : Vector[Number] x Vector[Number] -> Vector[Number]
;;; Linear convolution via the convolution theorem:
;;;   conv(x, h) = IDFT(DFT(x) * DFT(h))
;;;
;;; Pads both signals to length >= len(x) + len(h) - 1 (next power of 2)
;;; to avoid circular aliasing.
;;; Result length: len(x) + len(h) - 1
;;;
;;; Complexity: O((N+M) log(N+M)) vs O(N*M) for direct convolution
(define (fft-convolve x h)
  (doc 'export #t)
  (let* ([nx (vector-length x)]
         [nh (vector-length h)]
         [output-len (+ nx nh -1)]
         [fft-size (next-power-of-2 output-len)]
         ;; Zero-pad and convert to complex
         [xc (pad-to-fft-size x fft-size)]
         [hc (pad-to-fft-size h fft-size)]
         ;; Forward transforms
         [X (fft-radix2 xc)]
         [H (fft-radix2 hc)]
         ;; Pointwise multiply in frequency domain
         [Y (complex-pointwise-mul X H)]
         ;; Inverse transform
         [y (ifft-radix2 Y)])
    ;; Extract real part, trim to output length
    (vec-tabulate output-len i
      (complex-real (vector-ref y i)))))

;;; ====
;;; Frequency-domain filtering
;;; ====

;;; fft-filter : Vector[Number] x Vector[Number] -> Vector[Number]
;;; Apply FIR filter kernel to signal via frequency-domain multiplication.
;;; Equivalent to linear convolution, but framed as "filtering":
;;; the kernel is the impulse response of the filter.
;;;
;;; Returns output with same length as input signal ('same' mode).
;;; This is the standard overlap-save/frequency-domain filtering pattern:
;;; do the convolution in freq domain, then trim to input length.
(define (fft-filter signal kernel)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)]
         [full (fft-convolve signal kernel)]
         ;; Trim to same length as signal, centered
         [offset (quotient (- m 1) 2)])
    (vec-tabulate n i
      (let ([j (+ i offset)])
        (if (< j (vector-length full))
            (vector-ref full j)
            0.0)))))

;;; ====
;;; FFT-based cross-correlation
;;; ====

;;; fft-cross-spectrum : Vector[Number] x Vector[Number] -> Vector[Complex]
;;; Compute the cross-power spectrum: conj(DFT(x)) * DFT(y).
;;; This is the frequency-domain representation of cross-correlation.
;;; Useful for spectral analysis, coherence estimation, etc.
;;;
;;; Both inputs are zero-padded to avoid aliasing.
(define (fft-cross-spectrum x y)
  (doc 'export #t)
  (let* ([nx (vector-length x)]
         [ny (vector-length y)]
         [fft-size (next-power-of-2 (+ nx ny -1))]
         [xc (pad-to-fft-size x fft-size)]
         [yc (pad-to-fft-size y fft-size)]
         [X (fft-radix2 xc)]
         [Y (fft-radix2 yc)])
    (complex-pointwise-conj-mul X Y)))

;;; fft-correlate : Vector[Number] x Vector[Number] -> Vector[Number]
;;; Cross-correlation via FFT.
;;; Output length: N + M - 1 (full correlation).
;;; Lag layout matches the direct correlator: output[k] corresponds to lag
;;; k - (N-1), where N = len(x). The center (index N-1) is lag 0.
;;;
;;; Implementation: correlation(x, y) = convolution(reverse(x), y),
;;; computed efficiently via FFT-based convolution.
(define (fft-correlate x y)
  (doc 'export #t)
  (fft-convolve (vec-reverse x) y))

;;; fft-autocorrelate : Vector[Number] -> Vector[Number]
;;; Autocorrelation via FFT: corr(x, x).
;;; Equivalent to IDFT(|DFT(x)|^2).
;;; Output length: 2*N - 1.
(define (fft-autocorrelate x)
  (doc 'export #t)
  (fft-correlate x x))
