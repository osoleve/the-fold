;;; Test harness for core/numeric/digital-filters.ss — Digital Filter Library

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/numeric/digital-filters.ss")

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "  ✓ ")
       (display name)
       (newline))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "  ✗ ")
       (display name)
       (display "\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)
       (newline))))

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "  ✓ ")
       (display name)
       (newline))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "  ✗ ")
       (display name)
       (display "\n    expected: ")
       (display expected)
       (display " (± ")
       (display tolerance)
       (display ")\n    got: ")
       (display actual)
       (newline))))

(define (test-true name expr)
  (test name #t (if expr #t #f)))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Window Functions Tests
;;; ====
(test-section "Window Functions")

;; Rectangular window
(define rect4 (rectangular-window 4))
(test "rectangular window length" 4 (vector-length rect4))
(test-approx "rectangular window value" 1.0 (vector-ref rect4 0) 0.001)
(test-approx "rectangular window all ones" 1.0 (vector-ref rect4 2) 0.001)

;; Hamming window
(define hamm8 (hamming-window 8))
(test "hamming window length" 8 (vector-length hamm8))
(test-approx "hamming window start" 0.08 (vector-ref hamm8 0) 0.01)
(test-approx "hamming window center" 1.0 (vector-ref hamm8 4) 0.1)

;; Hann window
(define hann8 (hann-window 8))
(test "hann window length" 8 (vector-length hann8))
(test-approx "hann window start" 0.0 (vector-ref hann8 0) 0.01)
(test-true "hann window max at center" (> (vector-ref hann8 4) 0.9))

;; Blackman window
(define black8 (blackman-window 8))
(test "blackman window length" 8 (vector-length black8))
(test-true "blackman window positive" (> (vector-ref black8 4) 0))

;; Kaiser window
(define kaiser8 (kaiser-window 8 5.0))
(test "kaiser window length" 8 (vector-length kaiser8))
(test-true "kaiser window max at center" (> (vector-ref kaiser8 4) (vector-ref kaiser8 0)))

;;; ====
;;; FIR Filter Design Tests
;;; ====
(test-section "FIR Filter Design")

;; Low-pass FIR
(define lp-fir (fir-lowpass 0.3 20 hamming-window))
(test "FIR lowpass length" 21 (vector-length lp-fir))
(test-true "FIR lowpass center tap positive" (> (vector-ref lp-fir 10) 0))

;; High-pass FIR
(define hp-fir (fir-highpass 0.3 20 hamming-window))
(test "FIR highpass length" 21 (vector-length hp-fir))

;; Bandpass FIR
(define bp-fir (fir-bandpass 0.2 0.4 20 hamming-window))
(test "FIR bandpass length" 21 (vector-length bp-fir))

;; Bandstop FIR
(define bs-fir (fir-bandstop 0.2 0.4 20 hamming-window))
(test "FIR bandstop length" 21 (vector-length bs-fir))

;;; ====
;;; FIR Filter Application Tests
;;; ====
(test-section "FIR Filter Application")

;; Simple averaging filter (moving average)
(define ma3 (vector 0.333 0.333 0.334))
(define test-signal (vector 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
(define filtered (fir-filter test-signal ma3))
(test "FIR filter output length" 8 (vector-length filtered))

;; DC signal through lowpass should pass
(define dc-signal (make-vector 32 1.0))
(define dc-filtered (fir-filter dc-signal lp-fir))
(test-true "lowpass passes DC" (> (vector-ref dc-filtered 16) 0.5))

;;; ====
;;; IIR Filter Structure Tests
;;; ====
(test-section "IIR Filter Structure")

(define test-iir (make-iir-filter (vector 1.0 0.5) (vector 1.0 -0.5)))
(test-true "IIR filter recognized" (iir-filter? test-iir))
(test "IIR filter b coeffs" 2 (vector-length (iir-filter-b test-iir)))
(test "IIR filter a coeffs" 2 (vector-length (iir-filter-a test-iir)))

;;; ====
;;; IIR Filter Application Tests
;;; ====
(test-section "IIR Filter Application")

;; Simple first-order IIR
(define iir-b (vector 0.5 0.5))
(define iir-a (vector 1.0 0.0))
(define iir-out (iir-filter-signal test-signal iir-b iir-a))
(test "IIR output length" 8 (vector-length iir-out))
(test-approx "IIR first sample" 0.5 (vector-ref iir-out 0) 0.01)
(test-approx "IIR second sample" 1.5 (vector-ref iir-out 1) 0.01)

;;; ====
;;; Biquad Tests
;;; ====
(test-section "Biquad Sections")

(define bq (make-biquad 0.1 0.2 0.1 -0.8 0.4))
(test-true "biquad recognized" (biquad? bq))
(test-approx "biquad b0" 0.1 (biquad-b0 bq) 0.001)
(test-approx "biquad a1" -0.8 (biquad-a1 bq) 0.001)

;; Apply biquad
(define bq-out (biquad-filter test-signal bq))
(test "biquad output length" 8 (vector-length bq-out))

;;; ====
;;; Butterworth Filter Design Tests
;;; ====
(test-section "Butterworth Filter Design")

;; Butterworth poles
(define bw2-poles (butterworth-lowpass-poles 2))
(test "butterworth 2nd order poles count" 2 (length bw2-poles))

(define bw4-poles (butterworth-lowpass-poles 4))
(test "butterworth 4th order poles count" 4 (length bw4-poles))

;; All analog poles should be in left half plane (negative real part)
(test-true "butterworth analog poles in LHP"
           (andmap (lambda (p) (negative? (complex-real p))) bw4-poles))

;; Butterworth lowpass filter
(define bw-lp (butterworth-lowpass 0.2 2))
(test-true "butterworth filter created" (iir-filter? bw-lp))
(test-true "butterworth has b coeffs" (> (vector-length (iir-filter-b bw-lp)) 0))
(test-true "butterworth has a coeffs" (> (vector-length (iir-filter-a bw-lp)) 0))

;;; ====
;;; Chebyshev Filter Design Tests
;;; ====
(test-section "Chebyshev Filter Design")

;; Chebyshev poles
(define ch2-poles (chebyshev1-poles 2 0.5))
(test "chebyshev 2nd order poles count" 2 (length ch2-poles))

;; All analog poles should be in left half plane (negative real part)
(test-true "chebyshev poles in LHP"
           (andmap (lambda (p) (negative? (complex-real p))) ch2-poles))

;; Chebyshev lowpass filter
(define ch-lp (chebyshev1-lowpass 0.2 0.5 2))
(test-true "chebyshev filter created" (iir-filter? ch-lp))

;;; ====
;;; Frequency Response Tests
;;; ====
(test-section "Frequency Response Analysis")

;; Simple filter for testing
(define simple-iir (make-iir-filter (vector 0.5 0.5) (vector 1.0 0.0)))
(define fr (freqz simple-iir 64))
(test "freqz returns pair" #t (pair? fr))
(test "freqz freqs length" 64 (vector-length (car fr)))
(test "freqz response length" 64 (vector-length (cdr fr)))

;; Magnitude response
(define mag-r (magnitude-response simple-iir 64))
(test "magnitude response length" 64 (vector-length (cdr mag-r)))

;; Phase response
(define phase-r (phase-response simple-iir 64))
(test "phase response length" 64 (vector-length (cdr phase-r)))

;; DC gain should be 0 dB for this filter (b = [0.5, 0.5], sum = 1)
(test-approx "DC gain 0 dB" 0.0 (vector-ref (cdr mag-r) 0) 0.5)

;;; ====
;;; Real-time Filtering Tests
;;; ====
(test-section "Real-time Filtering")

(define rt-filter (make-iir-filter (vector 0.5 0.5) (vector 1.0 0.0)))
(define rt-state (make-filter-state rt-filter))
(test-true "filter state created" (filter-state? rt-state))

;; Process samples one by one
(define s1 (filter-process-sample! rt-state 1.0))
(test-approx "realtime sample 1" 0.5 s1 0.01)

(define s2 (filter-process-sample! rt-state 2.0))
(test-approx "realtime sample 2" 1.5 s2 0.01)

;; Reset state
(filter-reset! rt-state)
(define s3 (filter-process-sample! rt-state 1.0))
(test-approx "realtime after reset" 0.5 s3 0.01)

;;; ====
;;; Common Filter Presets Tests
;;; ====
(test-section "Common Filter Presets")

;; DC blocker
(define dc-block (dc-blocker 0.99))
(test-true "dc blocker is IIR" (iir-filter? dc-block))

;; One-pole lowpass
(define op-lp (one-pole-lowpass 0.1))
(test-true "one-pole lowpass is IIR" (iir-filter? op-lp))

;; One-pole highpass
(define op-hp (one-pole-highpass 0.9))
(test-true "one-pole highpass is IIR" (iir-filter? op-hp))

;; Moving average
(define ma5 (moving-average 5))
(test "moving average length" 5 (vector-length ma5))
(test-approx "moving average coeff" 0.2 (vector-ref ma5 0) 0.001)

;;; ====
;;; Impulse and Step Response Tests
;;; ====
(test-section "Impulse and Step Response")

(define ir (impulse-response simple-iir 16))
(test "impulse response length" 16 (vector-length ir))
(test-approx "impulse response first" 0.5 (vector-ref ir 0) 0.01)
(test-approx "impulse response second" 0.5 (vector-ref ir 1) 0.01)

(define sr (step-response simple-iir 16))
(test "step response length" 16 (vector-length sr))
(test-approx "step response converges to 1" 1.0 (vector-ref sr 15) 0.01)

;;; ====
;;; Edge Cases
;;; ====
(test-section "Edge Cases")

;; Single sample window
(define win1 (hamming-window 1))
(test "single sample hamming" 1 (vector-length win1))
(test-approx "single sample hamming value" 1.0 (vector-ref win1 0) 0.01)

;; Bessel function
(test-true "bessel-i0(0) = 1" (< (abs (- (bessel-i0 0) 1.0)) 0.001))
(test-true "bessel-i0(1) > 1" (> (bessel-i0 1) 1.0))

;; Sinc function
(test-approx "sinc(0) = 1" 1.0 (sinc 0) 0.001)
(test-approx "sinc(1) = 0" 0.0 (sinc 1) 0.001)

;;; ====
;;; Summary
;;; ====

(newline)
(display "===========================================================")
(newline)
(display "  Tests passed: ")
(display *tests-passed*)
(newline)
(display "  Tests failed: ")
(display *tests-failed*)
(newline)
(display "===========================================================")
(newline)

(if (= *tests-failed* 0)
    (display "✓ All digital filter tests passed!\n")
    (begin
     (display "✗ Some tests failed!\n")
     (exit 1)))
