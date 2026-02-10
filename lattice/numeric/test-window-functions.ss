;;; core/numeric/test-window-functions.ss — Tests for Window Functions
;;;
;;; Test suite for window functions and spectral analysis.

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/numeric/complex.ss")
(load "lattice/numeric/dft.ss")
(load "lattice/numeric/window-functions.ss")
(load "lattice/numeric/spectral-analysis.ss")

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

(define (test-approx name expected actual tolerance)
  (let ([diff (abs (- expected actual))])
       (if (< diff tolerance)
           (begin
            (set! tests-passed (+ tests-passed 1))
            (printf "  [PASS] ~a\n" name))
           (begin
            (set! tests-failed (+ tests-failed 1))
            (printf "  [FAIL] ~a\n" name)
            (printf "      Expected: ~s (±~a)\n" expected tolerance)
            (printf "      Got:      ~s\n" actual)
            (printf "      Diff:     ~a\n" diff)))))

(define (test-predicate name predicate)
  (if predicate
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name))))

(printf "\n=== Window Function Tests ===\n\n")

;;; ====
;;; Basic Window Properties
;;; ====

(printf "Basic Window Properties:\n")

(let ([w (rectangular-window 10)])
     (test-predicate "rectangular window - all ones"
                     (and (= (vector-length w) 10)
                          (let check ([i 0])
                               (cond
                                [(= i 10) #t]
                                [(> (abs (- (vector-ref w i) 1.0)) 1e-10) #f]
                                [else (check (+ i 1))])))))

(let ([w (hann-window 10)])
     (test-approx "hann window - zero at start" 0.0 (vector-ref w 0) 1e-10)
     (test-approx "hann window - zero at end" 0.0 (vector-ref w 9) 1e-10)
     (test-approx "hann window - symmetry" (vector-ref w 1) (vector-ref w 8) 1e-10))

(let ([w (hamming-window 10)])
     (test-approx "hamming window - 0.08 at start" 0.08 (vector-ref w 0) 0.01)
     (test-approx "hamming window - 0.08 at end" 0.08 (vector-ref w 9) 0.01)
     (test-approx "hamming window - symmetry" (vector-ref w 1) (vector-ref w 8) 1e-10))

(let ([w (blackman-window 10)])
     (test-approx "blackman window - symmetry 1" (vector-ref w 0) (vector-ref w 9) 1e-10)
     (test-approx "blackman window - symmetry 2" (vector-ref w 1) (vector-ref w 8) 1e-10))

(let ([w (bartlett-window 11)])
     (test-approx "bartlett window - zero at start" 0.0 (vector-ref w 0) 1e-10)
     (test-approx "bartlett window - zero at end" 0.0 (vector-ref w 10) 1e-10)
     (test-approx "bartlett window - peak at center" 1.0 (vector-ref w 5) 1e-10))

(let ([w (kaiser-window 10 0)])
     (test-predicate "kaiser window beta=0 similar to rectangular"
                     (let check ([i 0])
                          (cond
                           [(= i 10) #t]
                           [(> (abs (- (vector-ref w i) 1.0)) 0.01) #f]
                           [else (check (+ i 1))]))))

(let ([w (kaiser-window 10 5.0)])
     (test-approx "kaiser window - symmetry 1" (vector-ref w 0) (vector-ref w 9) 1e-6)
     (test-approx "kaiser window - symmetry 2" (vector-ref w 1) (vector-ref w 8) 1e-6))

;;; ====
;;; Window Application
;;; ====

(printf "\nWindow Application:\n")

(let* ([signal (vector 1 2 3 4 5)]
       [window (rectangular-window 5)]
       [windowed (apply-window signal window)])
      (test-approx "apply-window - rectangular identity 1" 1.0 (vector-ref windowed 0) 1e-10)
      (test-approx "apply-window - rectangular identity 2" 3.0 (vector-ref windowed 2) 1e-10))

(let* ([signal (vector 1 2 3 4 1)]
       [window (hann-window 5)]
       [windowed (apply-window signal window)])
      (test-approx "apply-window - hann zeros endpoints 1" 0.0 (vector-ref windowed 0) 1e-10)
      (test-approx "apply-window - hann zeros endpoints 2" 0.0 (vector-ref windowed 4) 1e-10))

;;; ====
;;; Window Properties
;;; ====

(printf "\nWindow Properties:\n")

(let* ([w (rectangular-window 10)]
       [energy (window-energy w)])
      (test-approx "window-energy - rectangular" 10.0 energy 1e-10))

(let* ([w (rectangular-window 10)]
       [power (window-power w)])
      (test-approx "window-power - rectangular" 10.0 power 1e-10))

(let* ([w-rect (rectangular-window 100)]
       [w-hann (hann-window 100)]
       [energy-rect (window-energy w-rect)]
       [energy-hann (window-energy w-hann)])
      (test-predicate "hann energy less than rectangular"
                      (< energy-hann energy-rect)))

;;; ====
;;; Make Window Utility
;;; ====

(printf "\nWindow Creation Utility:\n")

(test-predicate "make-window - hann" (vector? (make-window 'hann 10)))
(test-predicate "make-window - hamming" (vector? (make-window 'hamming 10)))
(test-predicate "make-window - blackman" (vector? (make-window 'blackman 10)))
(test-predicate "make-window - rectangular" (vector? (make-window 'rectangular 10)))
(test-predicate "make-window - kaiser" (vector? (make-window 'kaiser 10 5.0)))
(test-predicate "make-window - hanning alias" (vector? (make-window 'hanning 10)))
(test-predicate "make-window - boxcar alias" (vector? (make-window 'boxcar 10)))

;;; ====
;;; Bessel I0 Function
;;; ====

(printf "\nBessel I0 Function:\n")

(test-approx "bessel-i0(0) = 1" 1.0 (bessel-i0 0) 1e-6)
(test-approx "bessel-i0(1) ≈ 1.266" 1.266 (bessel-i0 1.0) 0.01)
(test-approx "bessel-i0(2) ≈ 2.280" 2.280 (bessel-i0 2.0) 0.01)

;;; ====
;;; STFT Tests
;;; ====

(printf "\nSTFT Tests:\n")

(let* ([signal (make-vector 512 1.0)]
       [frame-len 128]
       [hop-len 64]
       [result (stft signal frame-len hop-len 'hann)]
       [expected-frames (+ 1 (quotient (- 512 frame-len) hop-len))])
      (test "stft - number of frames" expected-frames (vector-length result))
      (test "stft - frame length" frame-len (vector-length (vector-ref result 0))))

(let* ([signal (vector 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)]
       [frame-len 8]
       [hop-len 4]
       [stft-result (stft signal frame-len hop-len 'rectangular)]
       [reconstructed (istft stft-result frame-len hop-len 'rectangular)])
      (test-predicate "istft - reconstruction length reasonable"
                      (> (vector-length reconstructed) 8))
      (test-approx "istft - reconstruction quality 1" 1.0 (vector-ref reconstructed 4) 0.1)
      (test-approx "istft - reconstruction quality 2" 0.0 (vector-ref reconstructed 5) 0.1))

;;; ====
;;; Spectrogram Tests
;;; ====

(printf "\nSpectrogram Tests:\n")

(let* ([signal (make-vector 512 1.0)]
       [frame-len 128]
       [hop-len 64]
       [result (spectrogram signal frame-len hop-len 'hann)]
       [expected-frames (+ 1 (quotient (- 512 frame-len) hop-len))])
      (test "spectrogram - number of frames" expected-frames (vector-length result))
      (test "spectrogram - frequency bins" frame-len (vector-length (vector-ref result 0))))

(let* ([signal (make-vector 256 1.0)]
       [result (spectrogram signal 64 32 'hann)])
      (test-predicate "spectrogram - all non-negative"
                      (let check-frame ([i 0])
                           (if (= i (vector-length result))
                               #t
                               (let* ([frame (vector-ref result i)]
                                      [all-positive (let check-bin ([k 0])
                                                         (cond
                                                          [(= k (vector-length frame)) #t]
                                                          [(< (vector-ref frame k) 0) #f]
                                                          [else (check-bin (+ k 1))]))])
                                     (and all-positive (check-frame (+ i 1))))))))

(let* ([signal (make-vector 256 1.0)]
       [mag-spec (spectrogram signal 64 32 'hann)]
       [pow-spec (power-spectrogram signal 64 32 'hann)]
       [mag (vector-ref (vector-ref mag-spec 0) 0)]
       [pow (vector-ref (vector-ref pow-spec 0) 0)])
      (test-approx "power-spectrogram - squared magnitude" (* mag mag) pow 1e-6))

;;; ====
;;; Periodogram Tests
;;; ====

(printf "\nPeriodogram Tests:\n")

(let* ([signal (make-vector 128 1.0)]
       [psd (periodogram signal 'hann)])
      (test "periodogram - output length" 128 (vector-length psd)))

(let* ([signal (make-vector 128 1.0)]
       [psd (periodogram signal 'hann)])
      (test-predicate "periodogram - all non-negative"
                      (let check ([i 0])
                           (cond
                            [(= i (vector-length psd)) #t]
                            [(< (vector-ref psd i) 0) #f]
                            [else (check (+ i 1))]))))

(let* ([signal (make-vector 64 1.0)]
       [psd (periodogram signal 'rectangular)])
      (test-predicate "periodogram - DC dominant for constant signal"
                      (and (> (vector-ref psd 0) 0.01)
                           (< (vector-ref psd 1) 0.01))))

;;; ====
;;; Welch's Method Tests
;;; ====

(printf "\nWelch's Method Tests:\n")

(let* ([signal (make-vector 512 1.0)]
       [segment-len 128]
       [hop-len 64]
       [psd (welch-psd signal segment-len hop-len 'hann)])
      (test "welch-psd - output length" segment-len (vector-length psd)))

(let* ([signal (make-vector 512 1.0)]
       [psd (welch-psd signal 128 64 'hann)])
      (test-predicate "welch-psd - all non-negative"
                      (let check ([i 0])
                           (cond
                            [(= i (vector-length psd)) #t]
                            [(< (vector-ref psd i) 0) #f]
                            [else (check (+ i 1))]))))

(let* ([signal (make-vector 1024 1.0)]
       [psd (welch-psd signal 256 128 'hann)])
      (test "welch-psd - correct length" 256 (vector-length psd))
      (test-predicate "welch-psd - DC component significant" (> (vector-ref psd 0) 0)))

;;; ====
;;; Spectral Features
;;; ====

(printf "\nSpectral Features:\n")

(let* ([freqs (vector 0 100 200 300 400 500 600 700 800)]
       [power (vector 0 0 0 0 0 10 0 0 0)]
       [centroid (spectral-centroid freqs power)])
      (test-approx "spectral-centroid - single peak" 500 centroid 1))

(let* ([freqs (vector 0 100 200 300 400)]
       [power (vector 0 5 0 5 0)]
       [centroid (spectral-centroid freqs power)])
      (test-approx "spectral-centroid - two equal peaks" 200 centroid 1))

(let* ([freqs (vector 0 100 200 300 400)]
       [power (vector 1 1 1 1 1)]
       [rolloff (spectral-rolloff freqs power 0.85)])
      (test-predicate "spectral-rolloff - 85% threshold"
                      (and (>= rolloff 200) (<= rolloff 400))))

;;; ====
;;; Frequency/Time Grid Tests
;;; ====

(printf "\nFrequency/Time Grids:\n")

(let* ([n 128]
       [fs 1000]
       [freqs (spectrogram-frequencies n fs)])
      (test "spectrogram-frequencies - length" n (vector-length freqs))
      (test-approx "spectrogram-frequencies - DC at zero" 0.0 (vector-ref freqs 0) 1e-10))

(let* ([signal-len 512]
       [frame-len 128]
       [hop-len 64]
       [fs 1000]
       [times (spectrogram-times signal-len frame-len hop-len fs)]
       [expected-frames (+ 1 (quotient (- signal-len frame-len) hop-len))])
      (test "spectrogram-times - frame count" expected-frames (vector-length times)))

;;; ====
;;; Test Summary
;;; ====

(printf "\n=== Test Summary ===\n")
(printf "Passed: ~a\n" tests-passed)
(printf "Failed: ~a\n" tests-failed)
(printf "Total:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n✓ All tests passed!\n\n")
    (begin
     (printf "\n✗ Some tests failed\n\n")
     (exit 1)))
