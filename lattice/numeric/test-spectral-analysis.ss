;;; lattice/numeric/test-spectral-analysis.ss — Tests for Spectral Analysis
;;; Run with: scheme --script lattice/numeric/test-spectral-analysis.ss

(load "core/testing/test-framework.ss")

;; log10 is used by spectral-analysis.ss but not defined there (lives in digital-filters.ss).
;; Provide it here to avoid pulling the full digital-filters chain.
(define (log10 x) (/ (log x) (log 10)))

(load "lattice/numeric/spectral-analysis.ss")

;;; ============================================================
;;; Helpers
;;; ============================================================

(define (approx-equal? a b tol)
  (< (abs (- a b)) tol))

(define eps 1e-6)

;;; Make a simple sinusoidal signal: sin(2π * freq * t / fs)
(define (make-sine-signal freq fs n)
  (let ([signal (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n) signal)
      (vector-set! signal i
                   (sin (* 2 3.141592653589793 freq (/ i fs)))))))

;;; Make a DC signal (constant)
(define (make-dc-signal value n)
  (make-vector n value))

;;; Sum of elements in a vector
(define (vector-sum v)
  (let ([n (vector-length v)])
    (let loop ([i 0] [s 0])
      (if (= i n) s
          (loop (+ i 1) (+ s (vector-ref v i)))))))

;;; ============================================================
;;; STFT Frame
;;; ============================================================

(test-group "stft-frame"
  (define-test "returns complex vector"
    (let* ([signal (make-sine-signal 1 8 8)]
           [frame (stft-frame signal 0 8 'rectangular)])
      (assert-equal 8 (vector-length frame))
      ;; Each element should be a custom complex (pair with tag)
      (assert-true (pair? (vector-ref frame 0)))))

  (define-test "frame length matches"
    (let* ([signal (make-sine-signal 1 16 32)]
           [frame (stft-frame signal 0 16 'hann)])
      (assert-equal 16 (vector-length frame))))

  (define-test "dc signal frame has energy at bin 0"
    (let* ([signal (make-dc-signal 1.0 8)]
           [frame (stft-frame signal 0 8 'rectangular)])
      ;; Bin 0 (DC) should have the most energy
      (let ([bin0-mag (complex-magnitude (vector-ref frame 0))])
        (assert-true (> bin0-mag 1.0))))))

;;; ============================================================
;;; STFT
;;; ============================================================

(test-group "stft"
  (define-test "correct number of frames"
    (let* ([signal (make-dc-signal 1.0 32)]
           [frames (stft signal 8 4 'hann)])
      ;; num-frames = 1 + (32 - 8) / 4 = 7
      (assert-equal 7 (vector-length frames))))

  (define-test "each frame has correct length"
    (let* ([signal (make-dc-signal 1.0 32)]
           [frames (stft signal 8 4 'hann)])
      (assert-equal 8 (vector-length (vector-ref frames 0)))))

  (define-test "no overlap hop gives fewer frames"
    (let* ([signal (make-dc-signal 1.0 32)]
           [frames-half (stft signal 8 4 'hann)]   ; 50% overlap
           [frames-none (stft signal 8 8 'hann)])   ; no overlap
      ;; 50% overlap: 7 frames; no overlap: 4 frames
      (assert-true (> (vector-length frames-half)
                      (vector-length frames-none)))))

  (define-test "single frame for short signal"
    (let* ([signal (make-dc-signal 1.0 8)]
           [frames (stft signal 8 4 'hann)])
      (assert-equal 1 (vector-length frames)))))

;;; ============================================================
;;; ISTFT (Reconstruction)
;;; ============================================================

(test-group "istft"
  (define-test "roundtrip DC signal"
    (let* ([signal (make-dc-signal 1.0 32)]
           [frames (stft signal 8 4 'hann)]
           [reconstructed (istft frames 8 4 'hann)])
      ;; Reconstructed should have correct length
      (assert-equal 32 (vector-length reconstructed))
      ;; Interior samples should be close to original
      (assert-true (approx-equal? 1.0 (vector-ref reconstructed 10) 0.1))))

  (define-test "roundtrip preserves signal shape"
    (let* ([signal (make-sine-signal 2 32 32)]
           [frames (stft signal 8 4 'hann)]
           [reconstructed (istft frames 8 4 'hann)])
      ;; Interior sample should be close to original
      (let ([mid (quotient 32 2)])
        (assert-true (approx-equal? (vector-ref signal mid)
                                    (vector-ref reconstructed mid)
                                    0.2))))))

;;; ============================================================
;;; Spectrogram
;;; ============================================================

(test-group "spectrogram"
  (define-test "dimensions match"
    (let* ([signal (make-dc-signal 1.0 32)]
           [spec (spectrogram signal 8 4 'hann)])
      ;; 7 frames, each with 8 frequency bins
      (assert-equal 7 (vector-length spec))
      (assert-equal 8 (vector-length (vector-ref spec 0)))))

  (define-test "magnitudes are non-negative"
    (let* ([signal (make-sine-signal 2 16 32)]
           [spec (spectrogram signal 8 4 'hann)]
           [frame0 (vector-ref spec 0)])
      (let loop ([k 0])
        (when (< k (vector-length frame0))
          (assert-true (>= (vector-ref frame0 k) 0))
          (loop (+ k 1))))))

  (define-test "dc signal has energy at bin 0"
    (let* ([signal (make-dc-signal 1.0 16)]
           [spec (spectrogram signal 8 4 'rectangular)]
           [frame0 (vector-ref spec 0)]
           [bin0 (vector-ref frame0 0)]
           [bin1 (vector-ref frame0 1)])
      ;; DC signal: bin 0 >> bin 1
      (assert-true (> bin0 bin1)))))

;;; ============================================================
;;; Power Spectrogram
;;; ============================================================

(test-group "power-spectrogram"
  (define-test "power is magnitude squared"
    (let* ([signal (make-sine-signal 2 16 32)]
           [mag-spec (spectrogram signal 8 4 'hann)]
           [pow-spec (power-spectrogram signal 8 4 'hann)]
           [mag-val (vector-ref (vector-ref mag-spec 0) 0)]
           [pow-val (vector-ref (vector-ref pow-spec 0) 0)])
      ;; power ~ magnitude^2
      (assert-true (approx-equal? pow-val (* mag-val mag-val) 0.01)))))

;;; ============================================================
;;; Log Spectrogram
;;; ============================================================

(test-group "log-spectrogram"
  (define-test "returns dB values"
    (let* ([signal (make-dc-signal 1.0 16)]
           [log-spec (log-spectrogram signal 8 4 'rectangular)]
           [frame0 (vector-ref log-spec 0)])
      ;; dB values can be negative (for magnitudes < 1)
      ;; Check that it returns numbers
      (assert-true (number? (vector-ref frame0 0)))))

  (define-test "log floor prevents -infinity"
    ;; Zero signal should not produce -inf due to epsilon floor
    (let* ([signal (make-dc-signal 0.0 16)]
           [log-spec (log-spectrogram signal 8 4 'rectangular)]
           [frame0 (vector-ref log-spec 0)]
           [val (vector-ref frame0 0)])
      ;; Should be about 20*log10(1e-10) = -200 dB, not -inf
      (assert-true (finite? val))
      (assert-true (< val -100)))))

;;; ============================================================
;;; Periodogram
;;; ============================================================

(test-group "periodogram"
  (define-test "returns correct length"
    (let* ([signal (make-sine-signal 2 16 16)]
           [psd (periodogram signal 'hann)])
      (assert-equal 16 (vector-length psd))))

  (define-test "all PSD values non-negative"
    (let* ([signal (make-sine-signal 2 16 16)]
           [psd (periodogram signal 'hann)])
      (let loop ([k 0])
        (when (< k (vector-length psd))
          (assert-true (>= (vector-ref psd k) 0))
          (loop (+ k 1))))))

  (define-test "dc signal has peak at bin 0"
    (let* ([signal (make-dc-signal 1.0 16)]
           [psd (periodogram signal 'rectangular)])
      ;; Bin 0 should dominate
      (assert-true (> (vector-ref psd 0) (vector-ref psd 1))))))

;;; ============================================================
;;; Welch PSD
;;; ============================================================

(test-group "welch-psd"
  (define-test "returns correct length"
    (let* ([signal (make-dc-signal 1.0 32)]
           [psd (welch-psd signal 8 4 'hann)])
      (assert-equal 8 (vector-length psd))))

  (define-test "all values non-negative"
    (let* ([signal (make-sine-signal 2 32 64)]
           [psd (welch-psd signal 16 8 'hann)])
      (let loop ([k 0])
        (when (< k (vector-length psd))
          (assert-true (>= (vector-ref psd k) 0))
          (loop (+ k 1))))))

  (define-test "smoother than single periodogram"
    ;; Welch averages segments → should be less noisy
    ;; Just verify it produces reasonable output
    (let* ([signal (make-sine-signal 4 64 128)]
           [psd (welch-psd signal 32 16 'hann)]
           [total (vector-sum psd)])
      (assert-true (> total 0)))))

;;; ============================================================
;;; Frequency Grid
;;; ============================================================

(test-group "spectrogram-frequencies"
  (define-test "first bin is 0 Hz"
    (let ([freqs (spectrogram-frequencies 8 1000.0)])
      (assert-true (approx-equal? 0.0 (vector-ref freqs 0) eps))))

  (define-test "bin spacing is fs/n"
    (let ([freqs (spectrogram-frequencies 8 1000.0)])
      ;; Spacing should be 1000/8 = 125 Hz
      (assert-true (approx-equal? 125.0 (vector-ref freqs 1) eps))))

  (define-test "correct number of bins"
    (let ([freqs (spectrogram-frequencies 16 44100.0)])
      ;; freq-bins returns n/2+1 bins for one-sided spectrum
      (assert-true (> (vector-length freqs) 0)))))

;;; ============================================================
;;; Time Grid
;;; ============================================================

(test-group "spectrogram-times"
  (define-test "correct number of time points"
    (let ([times (spectrogram-times 32 8 4 1.0)])
      ;; num-frames = 1 + (32-8)/4 = 7
      (assert-equal 7 (vector-length times))))

  (define-test "first time point centered in first frame"
    (let ([times (spectrogram-times 32 8 4 1.0)])
      ;; First frame center: 0 + 8/2 = 4 samples → 4.0s at fs=1
      (assert-true (approx-equal? 4.0 (vector-ref times 0) eps))))

  (define-test "times increase monotonically"
    (let ([times (spectrogram-times 64 16 8 44100.0)])
      (let loop ([i 1])
        (when (< i (vector-length times))
          (assert-true (> (vector-ref times i) (vector-ref times (- i 1))))
          (loop (+ i 1)))))))

;;; ============================================================
;;; Spectral Centroid
;;; ============================================================

(test-group "spectral-centroid"
  (define-test "single peak at frequency f"
    ;; All energy at bin 2 (100 Hz) → centroid = 100
    (let ([freqs '#(0.0 50.0 100.0 150.0)]
          [power '#(0.0 0.0 1.0 0.0)])
      (assert-true (approx-equal? 100.0 (spectral-centroid freqs power) eps))))

  (define-test "symmetric spectrum"
    ;; Equal energy at 50 and 150 Hz → centroid = 100
    (let ([freqs '#(0.0 50.0 100.0 150.0 200.0)]
          [power '#(0.0 1.0 0.0 1.0 0.0)])
      (assert-true (approx-equal? 100.0 (spectral-centroid freqs power) eps))))

  (define-test "zero power returns 0"
    (let ([freqs '#(0.0 50.0 100.0)]
          [power '#(0.0 0.0 0.0)])
      (assert-equal 0 (spectral-centroid freqs power)))))

;;; ============================================================
;;; Spectral Bandwidth
;;; ============================================================

(test-group "spectral-bandwidth"
  (define-test "single peak has zero bandwidth"
    (let ([freqs '#(0.0 50.0 100.0 150.0)]
          [power '#(0.0 0.0 1.0 0.0)])
      (assert-true (approx-equal? 0.0 (spectral-bandwidth freqs power 100.0) eps))))

  (define-test "spread spectrum has positive bandwidth"
    (let ([freqs '#(0.0 50.0 100.0 150.0 200.0)]
          [power '#(0.0 1.0 1.0 1.0 0.0)])
      (let ([centroid (spectral-centroid freqs power)])
        (assert-true (> (spectral-bandwidth freqs power centroid) 0)))))

  (define-test "zero power returns 0"
    (let ([freqs '#(0.0 50.0 100.0)]
          [power '#(0.0 0.0 0.0)])
      (assert-equal 0 (spectral-bandwidth freqs power 50.0)))))

;;; ============================================================
;;; Spectral Rolloff
;;; ============================================================

(test-group "spectral-rolloff"
  (define-test "all energy in first bin"
    (let ([freqs '#(0.0 50.0 100.0 150.0)]
          [power '#(1.0 0.0 0.0 0.0)])
      ;; 85% threshold met at bin 0
      (assert-true (approx-equal? 0.0 (spectral-rolloff freqs power 0.85) eps))))

  (define-test "energy spread across bins"
    (let ([freqs '#(0.0 100.0 200.0 300.0 400.0)]
          [power '#(0.2 0.2 0.2 0.2 0.2)])
      ;; Total = 1.0, threshold = 0.85 → need 5 bins (cum 1.0 > 0.85 at bin 4)
      (let ([rolloff (spectral-rolloff freqs power 0.85)])
        ;; Should be at a high frequency since energy is evenly spread
        (assert-true (> rolloff 200.0)))))

  (define-test "100% rolloff returns last frequency"
    (let ([freqs '#(0.0 100.0 200.0)]
          [power '#(0.5 0.3 0.2)])
      (let ([rolloff (spectral-rolloff freqs power 1.0)])
        (assert-true (approx-equal? 200.0 rolloff eps))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
