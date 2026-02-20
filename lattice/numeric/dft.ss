;;; lattice/numeric/dft.ss — DFT/FFT Implementation
;;; @module dft
;;; @requires prelude complex vec

(require 'prelude)
(require 'complex)
(require 'vec)

(doc 'module 'dft)
(doc 'description "Comprehensive DFT/FFT implementation for signal processing, spectral analysis, and convolution operations")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Utilities
;;; ====

;;; power-of-2? : Integer → Boolean
;;; Check if n is a power of 2.
(define (power-of-2? n)
  (doc 'export #t)
  (and (> n 0)
       (= 0 (bitwise-and n (- n 1)))))

;;; log2 : Integer → Integer
;;; Compute log base 2 of n (assuming n is a power of 2).
(define (log2-int n)
  (doc 'export #t)
  (if (<= n 1)
      0
      (+ 1 (log2-int (quotient n 2)))))

;;; bit-reverse : Integer × Integer → Integer
;;; Reverse the bits of index i using nbits bits.
(define (bit-reverse i nbits)
  (doc 'export #t)
  (let loop ([i i] [result 0] [bits nbits])
       (if (= bits 0)
           result
           (loop (quotient i 2)
                 (+ (* result 2) (remainder i 2))
                 (- bits 1)))))

;;; bit-reverse-copy : Vector → Vector
;;; Create a bit-reversed copy of the input vector.
(define (bit-reverse-copy v)
  (doc 'export #t)
  (let* ([n (vector-length v)]
         [nbits (log2-int n)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([j (bit-reverse i nbits)])
                 (vector-set! result j (vector-ref v i))))))

;;; ====
;;; Naive DFT (O(N²))
;;; ====

;;; dft-naive : Vector[Complex] → Vector[Complex]
;;; Compute the Discrete Fourier Transform using the naive O(N²) algorithm.
;;;
;;; X[k] = Σ(n=0 to N-1) x[n] * e^(-2πi*kn/N)
;;;
;;; This is the reference implementation, useful for:
;;; - Small inputs (N < 32)
;;; - Non-power-of-2 sizes
;;; - Verification of FFT correctness
(doc dft-naive 'fuel-cost '(quadratic n))
(define (dft-naive x)
  (doc 'export #t)
  (let* ([n (vector-length x)]
         [result (make-vector n)]
         [two-pi (* 2 (pi-value))])
        (do ([k 0 (+ k 1)])
            ((= k n) result)
            (let ([sum (complex-zero)])
                 (do ([m 0 (+ m 1)])
                     ((= m n)
                      (vector-set! result k sum))
                     (let* ([angle (/ (* -1 two-pi k m) n)]
                            [twiddle (make-polar 1 angle)]
                            [term (complex-mul (vector-ref x m) twiddle)])
                           (set! sum (complex-add sum term))))))))

;;; idft-naive : Vector[Complex] → Vector[Complex]
;;; Compute the Inverse Discrete Fourier Transform using the naive algorithm.
;;;
;;; x[n] = (1/N) * Σ(k=0 to N-1) X[k] * e^(2πi*kn/N)
(define (idft-naive X)
  (doc 'export #t)
  (let* ([n (vector-length X)]
         [result (make-vector n)]
         [two-pi (* 2 (pi-value))])
        (do ([m 0 (+ m 1)])
            ((= m n) result)
            (let ([sum (complex-zero)])
                 (do ([k 0 (+ k 1)])
                     ((= k n)
                      (vector-set! result m (complex-scale (/ 1.0 n) sum)))
                     (let* ([angle (/ (* two-pi k m) n)]
                            [twiddle (make-polar 1 angle)]
                            [term (complex-mul (vector-ref X k) twiddle)])
                           (set! sum (complex-add sum term))))))))

;;; ====
;;; Fast Fourier Transform (Cooley-Tukey Radix-2)
;;; ====

;;; fft-radix2-inplace! : Vector[Complex] → Vector[Complex]
;;; Compute FFT in-place using Cooley-Tukey radix-2 algorithm.
;;; Input vector must have power-of-2 length and be bit-reversed.
;;;
;;; This is the iterative implementation (bottom-up).
(define (fft-radix2-inplace! a)
  (doc 'export #t)
  (let* ([n (vector-length a)]
         [log2n (log2-int n)]
         [two-pi (* 2 (pi-value))])
        ;; Iterative FFT stages
        (do ([s 1 (+ s 1)])
            ((> s log2n) a)
            (let* ([m (expt 2 s)]
                   [m/2 (quotient m 2)]
                   [theta (/ (* -1 two-pi) m)]
                   ;; FIX: Precompute twiddle factors for this stage (fold-tg01)
                   ;; Twiddles only depend on j, not k, so cache them per stage
                   [twiddles (let ([tw (make-vector m/2)])
                                  (do ([j 0 (+ j 1)])
                                      ((= j m/2) tw)
                                      (vector-set! tw j (make-polar 1 (* theta j)))))])
                  ;; Process all groups for this stage
                  (do ([k 0 (+ k m)])
                      ((>= k n))
                      ;; Process butterfly operations within group
                      (do ([j 0 (+ j 1)])
                          ((= j m/2))
                          (let* ([w (vector-ref twiddles j)]
                                 [t-idx (+ k j m/2)]
                                 [u-idx (+ k j)]
                                 [t (complex-mul w (vector-ref a t-idx))]
                                 [u (vector-ref a u-idx)])
                                (vector-set! a u-idx (complex-add u t))
                                (vector-set! a t-idx (complex-sub u t)))))))))

;;; fft-radix2 : Vector[Complex] → Vector[Complex]
;;; Compute the Fast Fourier Transform using Cooley-Tukey radix-2 algorithm.
;;;
;;; Requirements: Input length must be a power of 2.
(doc fft-radix2 'fuel-cost '(log-linear n))
(define (fft-radix2 x)
  (doc 'export #t)
  (if (not (power-of-2? (vector-length x)))
      (error 'fft-radix2 "input length must be power of 2" (vector-length x))
      (let ([a (bit-reverse-copy x)])
           (fft-radix2-inplace! a))))

;;; ifft-radix2-inplace! : Vector[Complex] → Vector[Complex]
;;; Compute inverse FFT in-place.
(define (ifft-radix2-inplace! a)
  (doc 'export #t)
  (let* ([n (vector-length a)]
         [log2n (log2-int n)]
         [two-pi (* 2 (pi-value))])
        ;; Iterative IFFT stages (positive angle for inverse)
        (do ([s 1 (+ s 1)])
            ((> s log2n))
            (let* ([m (expt 2 s)]
                   [m/2 (quotient m 2)]
                   [theta (/ two-pi m)])
                  (do ([k 0 (+ k m)])
                      ((>= k n))
                      (do ([j 0 (+ j 1)])
                          ((= j m/2))
                          (let* ([angle (* theta j)]
                                 [w (make-polar 1 angle)]
                                 [t-idx (+ k j m/2)]
                                 [u-idx (+ k j)]
                                 [t (complex-mul w (vector-ref a t-idx))]
                                 [u (vector-ref a u-idx)])
                                (vector-set! a u-idx (complex-add u t))
                                (vector-set! a t-idx (complex-sub u t)))))))
        ;; Scale by 1/N
        (do ([i 0 (+ i 1)])
            ((= i n) a)
            (vector-set! a i (complex-scale (/ 1.0 n) (vector-ref a i))))))

;;; ifft-radix2 : Vector[Complex] → Vector[Complex]
;;; Compute the Inverse Fast Fourier Transform.
;;;
;;; Requirements: Input length must be a power of 2.
;;; Complexity: O(N log N)
(define (ifft-radix2 X)
  (doc 'export #t)
  (if (not (power-of-2? (vector-length X)))
      (error 'ifft-radix2 "input length must be power of 2" (vector-length X))
      (let ([a (bit-reverse-copy X)])
           (ifft-radix2-inplace! a))))

;;; ====
;;; Automatic Algorithm Selection
;;; ====

;;; dft : Vector[Complex] → Vector[Complex]
;;; Compute DFT using the most appropriate algorithm.
;;; - Power-of-2 sizes: Use FFT (O(N log N))
;;; - Other sizes: Use naive DFT (O(N^2))
(doc dft 'fuel-cost '(max (log-linear n) (quadratic n)))
(define (dft x)
  (doc 'export #t)
  (if (power-of-2? (vector-length x))
      (fft-radix2 x)
      (dft-naive x)))

;;; idft : Vector[Complex] → Vector[Complex]
;;; Compute inverse DFT using the most appropriate algorithm.
(define (idft X)
  (doc 'export #t)
  (if (power-of-2? (vector-length X))
      (ifft-radix2 X)
      (idft-naive X)))

;;; ====
;;; Real-valued Transforms
;;; ====

;;; real->complex-vec : Vector[Number] → Vector[Complex]
;;; Convert real-valued vector to complex vector.
(define (real->complex-vec v)
  (doc 'export #t)
  (let* ([n (vector-length v)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (make-complex (vector-ref v i) 0)))))

;;; complex->real-vec : Vector[Complex] → Vector[Number]
;;; Extract real parts from complex vector.
(define (complex->real-vec v)
  (doc 'export #t)
  (let* ([n (vector-length v)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (complex-real (vector-ref v i))))))

;;; dft-real : Vector[Number] → Vector[Complex]
;;; Compute DFT of real-valued signal.
(define (dft-real x)
  (doc 'export #t)
  (dft (real->complex-vec x)))

;;; idft-real : Vector[Complex] → Vector[Number]
;;; Compute inverse DFT and extract real part.
(define (idft-real X)
  (doc 'export #t)
  (complex->real-vec (idft X)))

;;; ====
;;; Spectral Analysis Utilities
;;; ====

;;; magnitude-spectrum : Vector[Complex] → Vector[Number]
;;; Compute magnitude spectrum from DFT output.
(define (magnitude-spectrum X)
  (doc 'export #t)
  (let* ([n (vector-length X)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (complex-magnitude (vector-ref X i))))))

;;; phase-spectrum : Vector[Complex] → Vector[Number]
;;; Compute phase spectrum from DFT output.
(define (phase-spectrum X)
  (doc 'export #t)
  (let* ([n (vector-length X)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (complex-angle (vector-ref X i))))))

;;; power-spectrum : Vector[Complex] → Vector[Number]
;;; Compute power spectrum (magnitude squared).
(define (power-spectrum X)
  (doc 'export #t)
  (let* ([n (vector-length X)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([mag (complex-magnitude (vector-ref X i))])
                 (vector-set! result i (* mag mag))))))

;;; ====
;;; Frequency Bins
;;; ====

;;; freq-bins : Integer × Number → Vector[Number]
;;; Compute frequency bin centers for DFT of length n at sample rate fs.
;;; Returns vector of frequencies [0, fs/n, 2*fs/n, ..., (n-1)*fs/n]
(define (freq-bins n fs)
  (doc 'export #t)
  (let ([result (make-vector n)]
        [df (/ fs n)])
       (do ([i 0 (+ i 1)])
           ((= i n) result)
           (vector-set! result i (* i df)))))

;;; ====
;;; Zero Padding
;;; ====

;;; zero-pad : Vector × Integer → Vector
;;; Pad vector with zeros to length n.
;;; If input is longer than n, truncate.
(define (zero-pad v n)
  (doc 'export #t)
  (let* ([m (vector-length v)]
         [result (make-vector n (if (> m 0)
                                    (if (complex? (vector-ref v 0))
                                        (complex-zero)
                                        0)
                                    0))])
        (do ([i 0 (+ i 1)])
            ((or (= i m) (= i n)) result)
            (vector-set! result i (vector-ref v i)))))

;;; next-power-of-2 : Integer → Integer
;;; Find the next power of 2 >= n.
(define (next-power-of-2 n)
  (doc 'export #t)
  (let loop ([p 1])
       (if (>= p n)
           p
           (loop (* 2 p)))))

;;; zero-pad-power-of-2 : Vector → Vector
;;; Pad vector to next power of 2 length.
(define (zero-pad-power-of-2 v)
  (doc 'export #t)
  (let ([n (vector-length v)])
       (if (power-of-2? n)
           v
           (zero-pad v (next-power-of-2 n)))))
