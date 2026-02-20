;;; lattice/numeric/test-fft-convolve.ss — Tests for FFT convolution bridge

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/numeric/complex.ss")
(load "lattice/numeric/dft.ss")
(load "lattice/numeric/fft-convolve.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

(define (test-approx-vec name expected actual tolerance)
  (let* ([n (vector-length expected)]
         [m (vector-length actual)])
    (if (not (= n m))
        (begin
         (set! tests-failed (+ tests-failed 1))
         (printf "  [FAIL] ~a\n" name)
         (printf "      Length mismatch: expected ~a, got ~a\n" n m))
        (let* ([diffs (map (lambda (i)
                                   (abs (- (vector-ref expected i)
                                           (vector-ref actual i))))
                           (iota n))]
               [max-diff (if (null? diffs) 0 (apply max diffs))])
          (if (< max-diff tolerance)
              (begin
               (set! tests-passed (+ tests-passed 1))
               (printf "  [PASS] ~a\n" name))
              (begin
               (set! tests-failed (+ tests-failed 1))
               (printf "  [FAIL] ~a\n" name)
               (printf "      Expected: ~s (+-~a)\n" expected tolerance)
               (printf "      Got:      ~s\n" actual)
               (printf "      Max diff: ~a\n" max-diff)))))))

(printf "\n=== FFT Convolution Bridge Tests ===\n\n")

;;; ====
;;; fft-convolve: known convolution results
;;; ====

(printf "--- FFT-based Linear Convolution ---\n")

;; [1,1,1] * [1,2,3] = [1, 3, 6, 5, 3]
(test-approx-vec "convolve [1,1,1] * [1,2,3]"
                 (vector 1 3 6 5 3)
                 (fft-convolve (vector 1 1 1) (vector 1 2 3))
                 1e-10)

;; [1,2,3] * [1,1] = [1, 3, 5, 3]
(test-approx-vec "convolve [1,2,3] * [1,1]"
                 (vector 1 3 5 3)
                 (fft-convolve (vector 1 2 3) (vector 1 1))
                 1e-10)

;; [1,2,3,4] * [2] = [2, 4, 6, 8]
(test-approx-vec "convolve with scalar"
                 (vector 2 4 6 8)
                 (fft-convolve (vector 1 2 3 4) (vector 2))
                 1e-10)

;; Box filter: [1,1,1] * [1,1,1] = [1, 2, 3, 2, 1]
(test-approx-vec "box filter self-convolution"
                 (vector 1 2 3 2 1)
                 (fft-convolve (vector 1 1 1) (vector 1 1 1))
                 1e-10)

;; Output length should be N + M - 1
(let ([result (fft-convolve (vector 1 2 3 4 5) (vector 1 2 3))])
  (test "output length = N + M - 1"
        7
        (vector-length result)))

;;; ====
;;; Convolution commutativity
;;; ====

(printf "\n--- Commutativity ---\n")

(let* ([a (vector 1 2 3)]
       [b (vector 4 5 6)]
       [ab (fft-convolve a b)]
       [ba (fft-convolve b a)])
  (test-approx-vec "commutativity: a*b = b*a"
                   ab
                   ba
                   1e-10))

(let* ([a (vector 1 -1 2 -2)]
       [b (vector 3 0 1)]
       [ab (fft-convolve a b)]
       [ba (fft-convolve b a)])
  (test-approx-vec "commutativity: mixed signs"
                   ab
                   ba
                   1e-10))

;;; ====
;;; Delta function identity
;;; ====

(printf "\n--- Delta Function Identity ---\n")

;; Convolving with [1] (delta at index 0) gives the original signal
(let* ([signal (vector 1 2 3 4 5)]
       [delta (vector 1)]
       [result (fft-convolve signal delta)])
  (test-approx-vec "delta identity: x * delta = x"
                   signal
                   result
                   1e-10))

;; Convolving delta with signal also works (commutativity)
(let* ([signal (vector 7 3 1)]
       [delta (vector 1)]
       [result (fft-convolve delta signal)])
  (test-approx-vec "delta identity: delta * x = x"
                   signal
                   result
                   1e-10))

;; Convolving with shifted delta [0,1] shifts signal right by 1
(let* ([signal (vector 1 2 3)]
       [shifted-delta (vector 0 1)]
       [result (fft-convolve signal shifted-delta)])
  (test-approx-vec "shifted delta: shift by 1"
                   (vector 0 1 2 3)
                   result
                   1e-10))

;;; ====
;;; Associativity
;;; ====

(printf "\n--- Associativity ---\n")

;; (a * b) * c = a * (b * c)
(let* ([a (vector 1 2)]
       [b (vector 1 1)]
       [c (vector 1 0 1)]
       [ab-c (fft-convolve (fft-convolve a b) c)]
       [a-bc (fft-convolve a (fft-convolve b c))])
  (test-approx-vec "associativity: (a*b)*c = a*(b*c)"
                   ab-c
                   a-bc
                   1e-10))

;;; ====
;;; FFT filtering
;;; ====

(printf "\n--- FFT Filtering ---\n")

;; Filtering with delta should return the input
(let* ([signal (vector 1 2 3 4 5)]
       [delta (vector 1)]
       [result (fft-filter signal delta)])
  (test "filter with delta preserves length"
        5
        (vector-length result))
  (test-approx-vec "filter with delta preserves signal"
                   signal
                   result
                   1e-10))

;; Filtering preserves signal length
(let* ([signal (vector 1 2 3 4 5 6 7 8)]
       [kernel (vector 0.25 0.5 0.25)]
       [result (fft-filter signal kernel)])
  (test "filter preserves signal length"
        8
        (vector-length result)))

;; Smoothing filter should reduce variation
(let* ([signal (vector 0 10 0 10 0 10 0 10)]
       [smoother (vector 0.25 0.5 0.25)]
       [result (fft-filter signal smoother)]
       ;; Check that result values are less extreme than input
       [max-input (apply max (vector->list signal))]
       [max-output (apply max (vector->list result))])
  (test "smoothing filter reduces extremes"
        #t
        (< max-output max-input)))

;;; ====
;;; FFT cross-correlation
;;; ====

(printf "\n--- FFT Cross-Correlation ---\n")

;; Autocorrelation peak at lag 0 should equal signal energy
;; Lag 0 is at center index = N-1 for autocorrelation
(let* ([signal (vector 1 2 3 2 1)]
       [result (fft-correlate signal signal)]
       [n (vector-length result)]
       [center (quotient n 2)]
       [peak-val (vector-ref result center)]
       ;; Energy = sum of squares
       [energy (apply + (map (lambda (i)
                                     (let ([v (vector-ref signal i)])
                                       (* v v)))
                             (iota (vector-length signal))))])
  (test-approx-vec "autocorrelation peak = energy at center"
                   (vector energy)
                   (vector peak-val)
                   1e-10))

;; Correlation output length should be N + M - 1
(let ([result (fft-correlate (vector 1 2 3) (vector 4 5))])
  (test "correlation output length = N + M - 1"
        4
        (vector-length result)))

;; Cross-correlation with shifted copy should detect the shift
;; x has pattern at indices 2-4, y has same pattern shifted right by 2
;; Peak at center + shift = (N-1) + 2 = 9
(let* ([x (vector 0 0 1 2 1 0 0 0)]
       [y (vector 0 0 0 0 1 2 1 0)]  ; x shifted right by 2
       [corr (fft-correlate x y)]
       [n (vector-length corr)]
       [center (- (vector-length x) 1)]  ; N-1 = 7
       ;; Find index of max value
       [max-idx (let loop ([i 0] [best-i 0] [best-v -1e20])
                  (if (= i n)
                      best-i
                      (let ([v (vector-ref corr i)])
                        (if (> v best-v)
                            (loop (+ i 1) i v)
                            (loop (+ i 1) best-i best-v)))))]
       ;; Detected lag = max-idx - center
       [detected-lag (- max-idx center)])
  (test "cross-correlation detects shift of 2"
        2
        detected-lag))

;;; ====
;;; fft-autocorrelate
;;; ====

(printf "\n--- FFT Autocorrelation ---\n")

;; Autocorrelation of constant signal
(let* ([signal (vector 1 1 1 1)]
       [result (fft-autocorrelate signal)]
       [n (vector-length result)]
       [center (quotient n 2)])  ; center = 3
  (test "autocorrelation output length = 2N-1"
        7
        n)
  ;; Peak at lag 0 (center index) should be N (= 4*1^2 = 4)
  (test-approx-vec "autocorrelation peak at center"
                   (vector 4.0)
                   (vector (vector-ref result center))
                   1e-10))

;; Autocorrelation should be symmetric around center: R[k] = R[-k]
;; In our layout, result[center+k] = result[center-k]
(let* ([signal (vector 1 3 2 4)]
       [result (fft-autocorrelate signal)]
       [n (vector-length result)]  ; = 7 = 2*4-1
       [center (quotient n 2)]    ; = 3
       ;; Check symmetry around center
       [sym-ok (let loop ([k 1])
                 (if (> k center)
                     #t
                     (if (< (abs (- (vector-ref result (+ center k))
                                    (vector-ref result (- center k))))
                            1e-10)
                         (loop (+ k 1))
                         #f)))])
  (test "autocorrelation is symmetric"
        #t
        sym-ok))

;;; ====
;;; Cross-spectrum
;;; ====

(printf "\n--- Cross-Spectrum ---\n")

;; Cross-spectrum should be a complex vector with power-of-2 length
(let* ([x (vector 1 2 3)]
       [y (vector 4 5 6)]
       [cs (fft-cross-spectrum x y)]
       [n (vector-length cs)])
  (test "cross-spectrum is power-of-2 length"
        #t
        (power-of-2? n)))

;;; ====
;;; Results
;;; ====

(printf "\n====\n")
(printf "                    TEST RESULTS\n")
(printf "====\n\n")
(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n[SUCCESS] All FFT convolution bridge tests passed.\n")
    (begin
     (printf "\n[FAILURE] Some tests failed.\n")
     (exit 1)))
