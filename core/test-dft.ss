;;; fabric/stitches/test-dft.ss — Tests for DFT/FFT

(load "core/prelude.ss")
(load "core/complex.ss")
(load "core/dft.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\\n" name)
       (printf "      Expected: ~s\\n" expected)
       (printf "      Got:      ~s\\n" actual))))

(define (test-approx name expected actual tolerance)
  (let ([diff (if (complex? actual)
                  (complex-magnitude (complex-sub expected actual))
                  (abs (- expected actual)))])
       (if (< diff tolerance)
           (begin
            (set! tests-passed (+ tests-passed 1))
            (printf "  [PASS] ~a\\n" name))
           (begin
            (set! tests-failed (+ tests-failed 1))
            (printf "  [FAIL] ~a\\n" name)
            (printf "      Expected: ~s (±~a)\\n" expected tolerance)
            (printf "      Got:      ~s\\n" actual)
            (printf "      Diff:     ~a\\n" diff)))))

(define (test-vec-approx name expected actual tolerance)
  (let* ([n (vector-length expected)]
         [diffs (map (lambda (i)
                             (let ([e (vector-ref expected i)]
                                   [a (vector-ref actual i)])
                                  (if (complex? a)
                                      (complex-magnitude (complex-sub e a))
                                      (abs (- e a)))))
                     (iota n))]
         [max-diff (apply max diffs)])
        (if (< max-diff tolerance)
            (begin
             (set! tests-passed (+ tests-passed 1))
             (printf "  [PASS] ~a\\n" name))
            (begin
             (set! tests-failed (+ tests-failed 1))
             (printf "  [FAIL] ~a\\n" name)
             (printf "      Expected: ~s (±~a)\\n" expected tolerance)
             (printf "      Got:      ~s\\n" actual)
             (printf "      Max diff: ~a\\n" max-diff)))))

(printf "\\n=== DFT/FFT Tests ===\\n\\n")

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(printf "--- Utility Functions ---\\n")

(test "power-of-2?: 1 is power of 2" #t (power-of-2? 1))
(test "power-of-2?: 2 is power of 2" #t (power-of-2? 2))
(test "power-of-2?: 4 is power of 2" #t (power-of-2? 4))
(test "power-of-2?: 8 is power of 2" #t (power-of-2? 8))
(test "power-of-2?: 3 is not power of 2" #f (power-of-2? 3))
(test "power-of-2?: 6 is not power of 2" #f (power-of-2? 6))
(test "power-of-2?: 0 is not power of 2" #f (power-of-2? 0))

(test "log2-int: log2(1) = 0" 0 (log2-int 1))
(test "log2-int: log2(2) = 1" 1 (log2-int 2))
(test "log2-int: log2(4) = 2" 2 (log2-int 4))
(test "log2-int: log2(8) = 3" 3 (log2-int 8))
(test "log2-int: log2(16) = 4" 4 (log2-int 16))

(test "bit-reverse: 0 with 3 bits" 0 (bit-reverse 0 3))
(test "bit-reverse: 1 with 3 bits (001 -> 100)" 4 (bit-reverse 1 3))
(test "bit-reverse: 2 with 3 bits (010 -> 010)" 2 (bit-reverse 2 3))
(test "bit-reverse: 3 with 3 bits (011 -> 110)" 6 (bit-reverse 3 3))
(test "bit-reverse: 4 with 3 bits (100 -> 001)" 1 (bit-reverse 4 3))

(test "next-power-of-2: 1" 1 (next-power-of-2 1))
(test "next-power-of-2: 3" 4 (next-power-of-2 3))
(test "next-power-of-2: 5" 8 (next-power-of-2 5))
(test "next-power-of-2: 8" 8 (next-power-of-2 8))
(test "next-power-of-2: 9" 16 (next-power-of-2 9))

;;; ============================================================
;;; DFT of Simple Signals
;;; ============================================================

(printf "\\n--- Simple DFT Tests ---\\n")

;; DFT of constant signal [1, 1, 1, 1] should give [4, 0, 0, 0]
(let ([x (vector (make-complex 1 0)
                 (make-complex 1 0)
                 (make-complex 1 0)
                 (make-complex 1 0))]
      [expected (vector (make-complex 4 0)
                        (make-complex 0 0)
                        (make-complex 0 0)
                        (make-complex 0 0))])
     (test-vec-approx "DFT of constant [1,1,1,1]"
                      expected
                      (dft-naive x)
                      1e-10))

;; DFT of impulse [1, 0, 0, 0] should give [1, 1, 1, 1]
(let ([x (vector (make-complex 1 0)
                 (make-complex 0 0)
                 (make-complex 0 0)
                 (make-complex 0 0))]
      [expected (vector (make-complex 1 0)
                        (make-complex 1 0)
                        (make-complex 1 0)
                        (make-complex 1 0))])
     (test-vec-approx "DFT of impulse [1,0,0,0]"
                      expected
                      (dft-naive x)
                      1e-10))

;; DFT of alternating [1, -1, 1, -1] should give [0, 0, 4, 0]
(let ([x (vector (make-complex 1 0)
                 (make-complex -1 0)
                 (make-complex 1 0)
                 (make-complex -1 0))]
      [expected (vector (make-complex 0 0)
                        (make-complex 0 0)
                        (make-complex 4 0)
                        (make-complex 0 0))])
     (test-vec-approx "DFT of alternating [1,-1,1,-1]"
                      expected
                      (dft-naive x)
                      1e-10))

;;; ============================================================
;;; Inverse DFT Tests
;;; ============================================================

(printf "\\n--- Inverse DFT Tests ---\\n")

;; IDFT(DFT(x)) should equal x
(let* ([x (vector (make-complex 1 0)
                  (make-complex 2 0)
                  (make-complex 3 0)
                  (make-complex 4 0))]
       [X (dft-naive x)]
       [x-recovered (idft-naive X)])
      (test-vec-approx "IDFT(DFT(x)) = x for [1,2,3,4]"
                       x
                       x-recovered
                       1e-10))

(let* ([x (vector (make-complex 1 2)
                  (make-complex 3 4)
                  (make-complex 5 6)
                  (make-complex 7 8))]
       [X (dft-naive x)]
       [x-recovered (idft-naive X)])
      (test-vec-approx "IDFT(DFT(x)) = x for complex input"
                       x
                       x-recovered
                       1e-10))

;;; ============================================================
;;; FFT vs Naive DFT Comparison
;;; ============================================================

(printf "\\n--- FFT Correctness ---\\n")

;; FFT should match naive DFT for power-of-2 sizes
(let* ([x (vector (make-complex 1 0)
                  (make-complex 2 0)
                  (make-complex 3 0)
                  (make-complex 4 0))]
       [X-naive (dft-naive x)]
       [X-fft (fft-radix2 x)])
      (test-vec-approx "FFT matches naive DFT for [1,2,3,4]"
                       X-naive
                       X-fft
                       1e-10))

(let* ([x (vector (make-complex 1 0)
                  (make-complex 0 0)
                  (make-complex 0 0)
                  (make-complex 0 0)
                  (make-complex 0 0)
                  (make-complex 0 0)
                  (make-complex 0 0)
                  (make-complex 0 0))]
       [X-naive (dft-naive x)]
       [X-fft (fft-radix2 x)])
      (test-vec-approx "FFT matches naive DFT for impulse (N=8)"
                       X-naive
                       X-fft
                       1e-10))

;; Complex input
(let* ([x (vector (make-complex 1 1)
                  (make-complex 2 -1)
                  (make-complex 3 2)
                  (make-complex 4 -2))]
       [X-naive (dft-naive x)]
       [X-fft (fft-radix2 x)])
      (test-vec-approx "FFT matches naive DFT for complex input"
                       X-naive
                       X-fft
                       1e-10))

;;; ============================================================
;;; Inverse FFT Tests
;;; ============================================================

(printf "\\n--- Inverse FFT Tests ---\\n")

(let* ([x (vector (make-complex 1 0)
                  (make-complex 2 0)
                  (make-complex 3 0)
                  (make-complex 4 0)
                  (make-complex 5 0)
                  (make-complex 6 0)
                  (make-complex 7 0)
                  (make-complex 8 0))]
       [X (fft-radix2 x)]
       [x-recovered (ifft-radix2 X)])
      (test-vec-approx "IFFT(FFT(x)) = x for [1..8]"
                       x
                       x-recovered
                       1e-10))

(let* ([x (vector (make-complex 1 2)
                  (make-complex 3 4)
                  (make-complex 5 6)
                  (make-complex 7 8))]
       [X (fft-radix2 x)]
       [x-recovered (ifft-radix2 X)])
      (test-vec-approx "IFFT(FFT(x)) = x for complex input"
                       x
                       x-recovered
                       1e-10))

;;; ============================================================
;;; Automatic Algorithm Selection
;;; ============================================================

(printf "\\n--- Automatic Algorithm Selection ---\\n")

;; dft() should use FFT for power-of-2 sizes
(let* ([x (vector (make-complex 1 0)
                  (make-complex 2 0)
                  (make-complex 3 0)
                  (make-complex 4 0))]
       [X-auto (dft x)]
       [X-fft (fft-radix2 x)])
      (test-vec-approx "dft() uses FFT for N=4"
                       X-fft
                       X-auto
                       1e-10))

;; dft() should use naive for non-power-of-2
(let* ([x (vector (make-complex 1 0)
                  (make-complex 2 0)
                  (make-complex 3 0))]
       [X-auto (dft x)]
       [X-naive (dft-naive x)])
      (test-vec-approx "dft() uses naive DFT for N=3"
                       X-naive
                       X-auto
                       1e-10))

;; idft() should work correctly
(let* ([x (vector (make-complex 1 0)
                  (make-complex 2 0)
                  (make-complex 3 0)
                  (make-complex 4 0)
                  (make-complex 5 0)
                  (make-complex 6 0)
                  (make-complex 7 0)
                  (make-complex 8 0))]
       [X (dft x)]
       [x-recovered (idft X)])
      (test-vec-approx "idft(dft(x)) = x with auto selection"
                       x
                       x-recovered
                       1e-10))

;;; ============================================================
;;; Real-valued Transform Tests
;;; ============================================================

(printf "\\n--- Real-valued Transforms ---\\n")

(let* ([x (vector 1 2 3 4)]
       [x-complex (real->complex-vec x)]
       [X (dft-real x)]
       [x-recovered (idft-real X)])
      (test-vec-approx "Real DFT round-trip"
                       x
                       x-recovered
                       1e-10))

;;; ============================================================
;;; Spectral Analysis Tests
;;; ============================================================

(printf "\\n--- Spectral Analysis ---\\n")

;; Magnitude spectrum of impulse should be constant
(let* ([x (vector (make-complex 1 0)
                  (make-complex 0 0)
                  (make-complex 0 0)
                  (make-complex 0 0))]
       [X (dft x)]
       [mag (magnitude-spectrum X)]
       [expected (vector 1 1 1 1)])
      (test-vec-approx "Magnitude spectrum of impulse is constant"
                       expected
                       mag
                       1e-10))

;; Power spectrum is magnitude squared
(let* ([x (vector (make-complex 1 0)
                  (make-complex 2 0)
                  (make-complex 3 0)
                  (make-complex 4 0))]
       [X (dft x)]
       [mag (magnitude-spectrum X)]
       [power (power-spectrum X)])
      (test-vec-approx "Power spectrum = magnitude²"
                       (vector-map (lambda (m) (* m m)) mag)
                       power
                       1e-10))

;;; ============================================================
;;; Frequency Bin Tests
;;; ============================================================

(printf "\\n--- Frequency Bins ---\\n")

(let ([bins (freq-bins 8 1000)])
     (test-vec-approx "Frequency bins for N=8, fs=1000"
                      (vector 0 125 250 375 500 625 750 875)
                      bins
                      1e-10))

;;; ============================================================
;;; Zero Padding Tests
;;; ============================================================

(printf "\\n--- Zero Padding ---\\n")

(let* ([v (vector 1 2 3)]
       [padded (zero-pad v 5)])
      (test "Zero padding extends vector"
            (vector 1 2 3 0 0)
            padded))

(let* ([v (vector (make-complex 1 0) (make-complex 2 0))]
       [padded (zero-pad v 4)])
      (test-vec-approx "Zero padding with complex zeros"
                       (vector (make-complex 1 0)
                               (make-complex 2 0)
                               (make-complex 0 0)
                               (make-complex 0 0))
                       padded
                       1e-10))

(let* ([v (vector 1 2 3)]
       [padded (zero-pad-power-of-2 v)])
      (test "Zero pad to power of 2: 3->4"
            (vector 1 2 3 0)
            padded))

(let* ([v (vector 1 2 3 4)]
       [padded (zero-pad-power-of-2 v)])
      (test "Zero pad to power of 2: 4->4 (already power of 2)"
            v
            padded))

;;; ============================================================
;;; Bit Reversal Tests
;;; ============================================================

(printf "\\n--- Bit Reversal ---\\n")

(let* ([v (vector 0 1 2 3 4 5 6 7)]
       [reversed (bit-reverse-copy v)]
       [expected (vector 0 4 2 6 1 5 3 7)])
      (test "Bit-reverse copy of [0..7]"
            expected
            reversed))

;;; ============================================================
;;; Parseval's Theorem
;;; ============================================================

(printf "\\n--- Parseval's Theorem ---\\n")

;; Energy in time domain = Energy in frequency domain / N
(let* ([x (vector (make-complex 1 0)
                  (make-complex 2 0)
                  (make-complex 3 0)
                  (make-complex 4 0))]
       [X (dft x)]
       [n (vector-length x)]
       [energy-time (apply + (map (lambda (i)
                                          (let ([xi (vector-ref x i)])
                                               (complex-magnitude
                                                (complex-mul xi (complex-conjugate xi)))))
                                  (iota n)))]
       [energy-freq (/ (apply + (map (lambda (i)
                                             (let ([Xi (vector-ref X i)])
                                                  (complex-magnitude
                                                   (complex-mul Xi (complex-conjugate Xi)))))
                                     (iota n)))
                       n)])
      (test-approx "Parseval's theorem: energy conservation"
                   energy-time
                   energy-freq
                   1e-10))

;;; ============================================================
;;; Linearity Test
;;; ============================================================

(printf "\\n--- Linearity ---\\n")

;; DFT(a*x + b*y) = a*DFT(x) + b*DFT(y)
(let* ([x (vector (make-complex 1 0)
                  (make-complex 2 0)
                  (make-complex 3 0)
                  (make-complex 4 0))]
       [y (vector (make-complex 5 0)
                  (make-complex 6 0)
                  (make-complex 7 0)
                  (make-complex 8 0))]
       [a 2]
       [b 3]
       [ax+by (vector-map (lambda (i)
                                  (complex-add (complex-scale a (vector-ref x i))
                                               (complex-scale b (vector-ref y i))))
                          (vector 0 1 2 3))]
       [left (dft ax+by)]
       [X (dft x)]
       [Y (dft y)]
       [right (vector-map (lambda (i)
                                  (complex-add (complex-scale a (vector-ref X i))
                                               (complex-scale b (vector-ref Y i))))
                          (vector 0 1 2 3))])
      (test-vec-approx "DFT linearity: DFT(2x+3y) = 2*DFT(x)+3*DFT(y)"
                       left
                       right
                       1e-10))

;;; ============================================================
;;; Results
;;; ============================================================

(printf "\\n================================================================\\n")
(printf "                    TEST RESULTS\\n")
(printf "================================================================\\n\\n")
(printf "Tests passed: ~a\\n" tests-passed)
(printf "Tests failed: ~a\\n" tests-failed)
(printf "Total tests:  ~a\\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\\n[SUCCESS] All DFT/FFT tests passed.\\n")
    (printf "\\n[FAILURE] Some tests failed.\\n"))
