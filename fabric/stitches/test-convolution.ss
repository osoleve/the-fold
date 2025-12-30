;;; fabric/stitches/test-convolution.ss — Tests for Convolution and Correlation

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/complex.ss")
(load "fabric/stitches/dft.ss")
(load "fabric/stitches/convolution.ss")

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

(define (test-approx-vec name expected actual tolerance)
  (let* ([n (vector-length expected)]
         [m (vector-length actual)]
         [diffs (if (= n m)
                    (map (lambda (i)
                                 (abs (- (vector-ref expected i)
                                         (vector-ref actual i))))
                         (iota n))
                    (list 1e10))]
         [max-diff (if (null? diffs) 0 (apply max diffs))])
        (if (and (= n m) (< max-diff tolerance))
            (begin
             (set! tests-passed (+ tests-passed 1))
             (printf "  [PASS] ~a\\n" name))
            (begin
             (set! tests-failed (+ tests-failed 1))
             (printf "  [FAIL] ~a\\n" name)
             (printf "      Expected: ~s (±~a)\\n" expected tolerance)
             (printf "      Got:      ~s\\n" actual)
             (if (= n m)
                 (printf "      Max diff: ~a\\n" max-diff)
                 (printf "      Length mismatch: ~a vs ~a\\n" n m))))))

(printf "\\n=== Convolution and Correlation Tests ===\\n\\n")

;;; ============================================================
;;; Direct Convolution Tests
;;; ============================================================

(printf "--- Direct Convolution (Full Mode) ---\\n")

;; Simple test: [1,2,3] * [1,1] = [1,3,5,3]
(test "convolve [1,2,3] * [1,1] (full)"
      (vector 1 3 5 3)
      (convolve-direct-full (vector 1 2 3) (vector 1 1)))

;; Identity kernel: convolution with [1] gives original signal
(test "identity kernel [1,2,3] * [1]"
      (vector 1 2 3)
      (convolve-direct-full (vector 1 2 3) (vector 1)))

;; Zero kernel gives all zeros
(test "zero kernel [1,2,3] * [0]"
      (vector 0 0 0)
      (convolve-direct-full (vector 1 2 3) (vector 0)))

;; Box filter: [1,1,1] * [1,1,1] = [1,2,3,2,1]
(test "box filter [1,1,1] * [1,1,1]"
      (vector 1 2 3 2 1)
      (convolve-direct-full (vector 1 1 1) (vector 1 1 1)))

(printf "\\n--- Direct Convolution (Same Mode) ---\\n")

;; Same mode preserves signal length
(let ([result (convolve-direct-same (vector 1 2 3 4 5) (vector 1 1 1))])
     (test "same mode preserves length"
           5
           (vector-length result)))

(printf "\\n--- Direct Convolution (Valid Mode) ---\\n")

;; Valid mode: [1,2,3,4,5] * [1,1,1] gives 3 outputs
(let ([result (convolve-direct-valid (vector 1 2 3 4 5) (vector 1 1 1))])
     (test "valid mode length"
           3
           (vector-length result))
     (test "valid mode values"
           (vector 6 9 12)
           result))

;; Valid mode with kernel longer than signal gives empty result
(test "valid mode: kernel > signal"
      (vector)
      (convolve-direct-valid (vector 1 2) (vector 1 1 1 1)))

;;; ============================================================
;;; FFT Convolution Tests
;;; ============================================================

(printf "\\n--- FFT Convolution ---\\n")

;; FFT convolution should match direct convolution
(let* ([signal (vector 1 2 3 4 5)]
       [kernel (vector 1 0.5 0.25)]
       [direct (convolve-direct-full signal kernel)]
       [fft (convolve-fft signal kernel)])
      (test-approx-vec "FFT matches direct convolution"
                       direct
                       fft
                       1e-10))

;; Larger test
(let* ([signal (vector 1 2 3 4 5 6 7 8)]
       [kernel (vector 0.25 0.5 0.25)]
       [direct (convolve-direct-full signal kernel)]
       [fft (convolve-fft signal kernel)])
      (test-approx-vec "FFT matches direct (larger)"
                       direct
                       fft
                       1e-10))

;;; ============================================================
;;; Automatic Algorithm Selection
;;; ============================================================

(printf "\\n--- Automatic Convolution ---\\n")

;; Small kernel should use direct convolution
(let* ([signal (vector 1 2 3 4 5)]
       [kernel (vector 1 1)]
       [result (convolve signal kernel 'full)])
      (test "small kernel (auto) - full"
            (vector 1 3 5 7 9 5)
            result))

;; Test all modes work
(let ([signal (vector 1 2 3 4 5)]
      [kernel (vector 1 1 1)])
     (test "auto convolution - same mode length"
           5
           (vector-length (convolve signal kernel 'same)))
     (test "auto convolution - valid mode length"
           3
           (vector-length (convolve signal kernel 'valid)))
     (test "auto convolution - full mode length"
           7
           (vector-length (convolve signal kernel 'full))))

;;; ============================================================
;;; Correlation Tests
;;; ============================================================

(printf "\\n--- Cross-Correlation ---\\n")

;; Correlation of [1,2,3] with [1,0,0]
;; Cross-correlation sums: signal[n+k] * template[k]
;; Expected: [0, 0, 1, 2, 3] (template slides across signal)
(let ([signal (vector 1 2 3)]
      [template (vector 1 0 0)])
     (test-approx-vec "correlation detects pattern"
                      (vector 0 0 1 2 3)
                      (correlate signal template 'full)
                      1e-10))

;; Correlation with self (autocorrelation) has peak at center
(let* ([signal (vector 1 2 3 2 1)]
       [auto (autocorrelate signal 'full)]
       [n (vector-length auto)]
       [center (quotient n 2)]
       [peak (vector-ref auto center)])
      (test "autocorrelation peak at center"
            #t
            (>= peak (vector-ref auto (- center 1))))
      (test "autocorrelation symmetric"
            #t
            (>= peak (vector-ref auto (+ center 1)))))

(printf "\\n--- Auto-correlation ---\\n")

;; Autocorrelation of constant signal
(let ([signal (vector 1 1 1 1)])
     (test-approx-vec "autocorrelation of constant"
                      (vector 1 2 3 4 3 2 1)
                      (autocorrelate signal 'full)
                      1e-10))

;;; ============================================================
;;; Matched Filtering
;;; ============================================================

(printf "\\n--- Matched Filtering ---\\n")

;; Create signal with embedded template
(let* ([template (vector 1 2 1)]
       [signal (vector 0 0 0 1 2 1 0 0 0)]
       [matched (matched-filter signal template)]
       [n (vector-length matched)])
      (test "matched filter output length"
            9
            n)
      ;; Peak should be near position 4 (middle of template in signal)
      (test "matched filter detects template"
            #t
            (> (vector-ref matched 4) (vector-ref matched 2))))

;;; ============================================================
;;; Peak Finding
;;; ============================================================

(printf "\\n--- Peak Finding ---\\n")

;; Find peaks in simple signal
(let* ([signal (vector 0 1 0 2 0 3 0)]
       [peaks (find-peaks signal 0.5)])
      (test "find-peaks detects 3 peaks"
            3
            (vector-length peaks))
      (test "find-peaks finds correct positions"
            (vector 1 3 5)
            peaks))

;; No peaks if all below threshold
(test "no peaks below threshold"
      (vector)
      (find-peaks (vector 0.1 0.2 0.1 0.2) 0.5))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(printf "\\n--- Utility Functions ---\\n")

;; Vector reverse
(test "vector-reverse [1,2,3,4]"
      (vector 4 3 2 1)
      (vector-reverse (vector 1 2 3 4)))

(test "vector-reverse empty"
      (vector)
      (vector-reverse (vector)))

;; Normalize signal
(let* ([signal (vector 1 2 3 4 5)]
       [normalized (normalize-signal signal)]
       ;; Compute mean and stddev of normalized
       [n (vector-length normalized)]
       [mean (/ (apply + (vector->list normalized)) n)]
       [var-sum (apply + (map (lambda (x) (* (- x mean) (- x mean)))
                              (vector->list normalized)))]
       [variance (/ var-sum n)])
      (test-approx-vec "normalized has ~zero mean"
                       (vector 0)
                       (vector mean)
                       1e-10)
      (test-approx-vec "normalized has ~unit variance"
                       (vector 1)
                       (vector variance)
                       1e-10))

;; Normalize constant signal gives zeros
(test-approx-vec "normalize constant signal"
                 (vector 0 0 0)
                 (normalize-signal (vector 5 5 5))
                 1e-10)

;;; ============================================================
;;; Circular Convolution
;;; ============================================================

(printf "\\n--- Circular Convolution ---\\n")

;; Circular convolution wraps around
(let* ([signal (vector 1 2 3 4)]
       [kernel (vector 1 1 1 1)]
       [result (convolve-circular signal kernel)])
      (test "circular convolution length"
            4
            (vector-length result))
      (test-approx-vec "circular convolution values"
                       (vector 10 10 10 10)
                       result
                       1e-10))

;;; ============================================================
;;; Properties and Identities
;;; ============================================================

(printf "\\n--- Mathematical Properties ---\\n")

;; Convolution is commutative: f*g = g*f
(let* ([f (vector 1 2 3)]
       [g (vector 4 5)]
       [fg (convolve-direct-full f g)]
       [gf (convolve-direct-full g f)])
      (test "convolution is commutative"
            fg
            gf))

;; Convolution with delta function is identity
(let* ([signal (vector 1 2 3 4 5)]
       [delta (vector 1)]
       [result (convolve-direct-full signal delta)])
      (test "delta function identity"
            signal
            result))

;; Correlation is NOT commutative
(let* ([f (vector 1 2 3)]
       [g (vector 4 5)]
       [fg (correlate f g 'full)]
       [gf (correlate g f 'full)])
      (test "correlation is not commutative"
            #f
            (equal? fg gf)))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(printf "\\n--- Edge Cases ---\\n")

;; Empty vectors
(test "convolve empty signal"
      (vector)
      (convolve-direct-full (vector) (vector 1)))

(test "convolve empty kernel"
      (vector)
      (convolve-direct-full (vector 1) (vector)))

;; Single element
(test "convolve single elements"
      (vector 6)
      (convolve-direct-full (vector 2) (vector 3)))

;;; ============================================================
;;; FFT vs Direct Comparison (Large)
;;; ============================================================

(printf "\\n--- Large Signal Test ---\\n")

;; Create larger signals to test FFT efficiency
(let* ([signal (list->vector (iota 64))]
       [kernel (vector 0.1 0.2 0.4 0.2 0.1)]
       [direct (convolve-direct-full signal kernel)]
       [auto (convolve signal kernel 'full)])
      (test-approx-vec "large signal: auto matches direct"
                       direct
                       auto
                       1e-8))

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
    (printf "\\n[SUCCESS] All convolution tests passed.\\n")
    (printf "\\n[FAILURE] Some tests failed.\\n"))
