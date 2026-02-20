;;; lattice/statistics/test-spectral-ts.ss — Tests for Spectral Time Series Module
;;;
;;; Tests use known signals (pure sinusoids, white noise) to verify
;;; spectral analysis functions.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")

(require 'spectral-ts)

;;; ====
;;; Helper: generate a pure sinusoid
;;; ====
;;; x[n] = A * sin(2*pi*k*n/N)
;;; k = number of complete cycles in N samples (DFT bin index)
(define (make-sinusoid n k amplitude)
  (let ([v (make-vector n)]
        [two-pi (* 2 (pi-value))])
    (do ([i 0 (+ i 1)])
        ((= i n) v)
      (vector-set! v i (* amplitude (sin (* two-pi k (/ i n))))))))

;;; Helper: generate white-ish noise (deterministic PRNG for reproducibility)
;;; Simple LCG: x_{n+1} = (a*x_n + c) mod m, scaled to [-1, 1]
(define (make-pseudo-noise n seed)
  (let ([v (make-vector n)]
        [a 1103515245]
        [c 12345]
        [m (expt 2 31)])
    (let loop ([i 0] [state seed])
      (if (= i n) v
          (let* ([next (remainder (+ (* a state) c) m)]
                 [val (- (* 2.0 (/ next m)) 1.0)])
            (vector-set! v i val)
            (loop (+ i 1) next))))))

;;; ====
;;; Periodogram Tests
;;; ====

(test-group "ts-periodogram"

  (define-test "periodogram of pure sinusoid has peak at correct frequency"
    ;; 128-point signal with frequency at bin 8 (8 cycles in 128 samples)
    (let* ([n 128]
           [freq 8]  ; 8 cycles in n samples -> bin 8
           [xs (make-sinusoid n freq 1.0)]
           [psd (ts-periodogram xs)]
           ;; Find the peak bin (excluding DC)
           [half (+ 1 (quotient (vector-length psd) 2))]
           [peak-bin (let loop ([k 1] [best 1] [best-val (vector-ref psd 1)])
                       (if (= k half) best
                           (let ([v (vector-ref psd k)])
                             (if (> v best-val)
                                 (loop (+ k 1) k v)
                                 (loop (+ k 1) best best-val)))))])
      (assert-equal freq peak-bin)))

  (define-test "periodogram values are non-negative"
    (let* ([xs (make-sinusoid 64 5 2.0)]
           [psd (ts-periodogram xs)]
           [all-positive (let loop ([k 0])
                           (if (= k (vector-length psd)) #t
                               (if (< (vector-ref psd k) 0)
                                   #f
                                   (loop (+ k 1)))))])
      (assert-true all-positive)))

  (define-test "periodogram power concentrates at signal frequency"
    ;; Most power should be near the signal frequency
    (let* ([n 128]
           [freq 16]
           [xs (make-sinusoid n freq 1.0)]
           [psd (ts-periodogram xs)]
           [m (vector-length psd)]
           ;; Power at peak bin
           [peak-power (vector-ref psd freq)]
           ;; Total power
           [total (let loop ([k 0] [s 0.0])
                    (if (= k m) s
                        (loop (+ k 1) (+ s (vector-ref psd k)))))]
           ;; Peak should be > 40% of total
           [ratio (/ peak-power total)])
      (assert-true (> ratio 0.4)))))

;;; ====
;;; Welch Periodogram Tests
;;; ====

(test-group "ts-welch-periodogram"

  (define-test "welch periodogram returns correct length"
    (let* ([xs (make-sinusoid 256 10 1.0)]
           [seg-len 64]
           [psd (ts-welch-periodogram xs seg-len 32 'hann)])
      (assert-equal seg-len (vector-length psd))))

  (define-test "welch periodogram values are non-negative"
    (let* ([xs (make-sinusoid 256 10 1.0)]
           [psd (ts-welch-periodogram xs 64 32 'hann)]
           [all-positive (let loop ([k 0])
                           (if (= k (vector-length psd)) #t
                               (if (< (vector-ref psd k) 0)
                                   #f
                                   (loop (+ k 1)))))])
      (assert-true all-positive)))

  (define-test "welch detects sinusoidal frequency"
    ;; Signal with 4 cycles per 64-sample segment = 16 cycles in 256 samples
    (let* ([n 256]
           [seg-len 64]
           [freq-bin 4]  ; 4 cycles per 64-sample segment
           [xs (make-sinusoid n (* freq-bin (/ n seg-len)) 1.0)]
           [psd (ts-welch-periodogram xs seg-len 32 'hann)]
           ;; Find peak
           [half (+ 1 (quotient seg-len 2))]
           [peak-bin (let loop ([k 1] [best 1] [best-val (vector-ref psd 1)])
                       (if (= k half) best
                           (let ([v (vector-ref psd k)])
                             (if (> v best-val)
                                 (loop (+ k 1) k v)
                                 (loop (+ k 1) best best-val)))))])
      ;; Peak should be at or very near freq-bin
      (assert-true (<= (abs (- peak-bin freq-bin)) 1)))))

;;; ====
;;; AR Spectral Density Tests
;;; ====

(test-group "ts-ar-spectral-density"

  (define-test "AR(0) = white noise gives flat spectrum"
    ;; phi = #() (no AR coefficients), sigma = 1
    ;; PSD should be sigma^2 = 1 everywhere
    (let* ([phi '#()]
           [psd (ts-ar-spectral-density phi 1.0 64)]
           [n (vector-length psd)]
           ;; Check all values are approximately 1.0
           [all-flat (let loop ([k 0])
                       (if (= k n) #t
                           (if (> (abs (- (vector-ref psd k) 1.0)) 0.01)
                               #f
                               (loop (+ k 1)))))])
      (assert-true all-flat)))

  (define-test "AR spectral density is positive"
    (let* ([phi (vector 0.5 -0.3)]
           [psd (ts-ar-spectral-density phi 1.0 128)]
           [all-positive (let loop ([k 0])
                           (if (= k (vector-length psd)) #t
                               (if (<= (vector-ref psd k) 0)
                                   #f
                                   (loop (+ k 1)))))])
      (assert-true all-positive)))

  (define-test "AR(1) positive phi gives peak at low frequency"
    ;; AR(1) with positive phi_1 -> positively correlated -> low-freq peak
    (let* ([phi (vector 0.9)]
           [psd (ts-ar-spectral-density phi 1.0 128)]
           [n (vector-length psd)]
           ;; DC (bin 0) should have the highest power
           [dc-power (vector-ref psd 0)]
           [nyquist-power (vector-ref psd (quotient n 2))]
           ;; Low freq should be much higher than high freq
           [ratio (/ dc-power (max nyquist-power 1e-30))])
      (assert-true (> ratio 10.0))))

  (define-test "AR(1) negative phi gives peak at Nyquist"
    ;; AR(1) with negative phi_1 -> negatively correlated -> high-freq peak
    (let* ([phi (vector -0.9)]
           [psd (ts-ar-spectral-density phi 1.0 128)]
           [n (vector-length psd)]
           [dc-power (vector-ref psd 0)]
           [nyquist-power (vector-ref psd (quotient n 2))])
      (assert-true (> nyquist-power (* 10 dc-power)))))

  (define-test "AR spectral density from model works"
    ;; Create a mock AR result and verify it matches direct call
    (let* ([phi (vector 0.6 -0.2)]
           [sigma 1.5]
           [model (make-ar-result phi 0.0 '#() '#() sigma 100.0 2)]
           [psd-direct (ts-ar-spectral-density phi sigma 64)]
           [psd-model (ts-ar-spectral-density-from-model model 64)]
           [n (vector-length psd-direct)]
           [match (let loop ([k 0])
                    (if (= k n) #t
                        (if (> (abs (- (vector-ref psd-direct k)
                                       (vector-ref psd-model k)))
                               1e-10)
                            #f
                            (loop (+ k 1)))))])
      (assert-true match))))

;;; ====
;;; Spectral Entropy Tests
;;; ====

(test-group "ts-spectral-entropy"

  (define-test "spectral entropy of pure sinusoid is low"
    ;; Pure tone -> power at one frequency -> low entropy
    (let* ([xs (make-sinusoid 128 10 1.0)]
           [H (ts-spectral-entropy xs)])
      ;; Should be well below 0.5
      (assert-true (< H 0.5))))

  (define-test "spectral entropy of noise is high"
    ;; Pseudo-random noise -> spread spectrum -> high entropy
    (let* ([xs (make-pseudo-noise 256 42)]
           [H (ts-spectral-entropy xs)])
      ;; Should be above 0.7
      (assert-true (> H 0.7))))

  (define-test "spectral entropy is between 0 and 1"
    (let* ([xs (make-sinusoid 64 5 1.0)]
           [H (ts-spectral-entropy xs)])
      (assert-true (>= H 0.0))
      (assert-true (<= H 1.0))))

  (define-test "spectral entropy of noise > entropy of sinusoid"
    (let* ([tone (make-sinusoid 128 12 1.0)]
           [noise (make-pseudo-noise 128 99)]
           [H-tone (ts-spectral-entropy tone)]
           [H-noise (ts-spectral-entropy noise)])
      (assert-true (> H-noise H-tone)))))

;;; ====
;;; Dominant Frequency Tests
;;; ====

(test-group "ts-dominant-frequency"

  (define-test "dominant frequency of sinusoid at bin 8/128"
    (let* ([n 128]
           [freq-bin 8]
           [xs (make-sinusoid n freq-bin 1.0)]
           [result (ts-dominant-frequency xs)]
           [freq (car result)]
           [expected-freq (/ freq-bin n)])
      ;; Should match expected frequency closely
      (assert-true (< (abs (- freq expected-freq)) 0.01))))

  (define-test "dominant frequency returns pair"
    (let* ([xs (make-sinusoid 64 8 1.0)]
           [result (ts-dominant-frequency xs)])
      (assert-true (pair? result))))

  (define-test "dominant frequency power is positive"
    (let* ([xs (make-sinusoid 64 5 3.0)]
           [result (ts-dominant-frequency xs)]
           [power (cdr result)])
      (assert-true (> power 0))))

  (define-test "dominant frequency is in valid range"
    (let* ([xs (make-sinusoid 128 20 1.0)]
           [result (ts-dominant-frequency xs)]
           [freq (car result)])
      ;; Must be in (0, 0.5] (normalized frequency, excluding DC)
      (assert-true (> freq 0))
      (assert-true (<= freq 0.5)))))

;;; ====
;;; White Noise Test
;;; ====

(test-group "ts-spectral-white-noise-test"

  (define-test "white noise test returns test-result"
    (let* ([xs (make-pseudo-noise 256 12345)]
           [result (ts-spectral-white-noise-test xs)])
      (assert-true (test-result? result))))

  (define-test "white noise test does not reject actual noise"
    ;; Pseudo-noise should look like white noise
    ;; p-value should be > 0.01 (not significant -> fail to reject H0)
    (let* ([xs (make-pseudo-noise 256 54321)]
           [result (ts-spectral-white-noise-test xs)]
           [p (test-p-value result)])
      ;; We expect NOT to reject H0 at alpha = 0.01
      (assert-true (> p 0.01))))

  (define-test "white noise test rejects pure sinusoid"
    ;; A sinusoid has all power at one frequency -> not white noise
    (let* ([xs (make-sinusoid 256 20 1.0)]
           [result (ts-spectral-white-noise-test xs)]
           [p (test-p-value result)])
      ;; Should reject H0 at alpha = 0.05
      (assert-true (< p 0.05))))

  (define-test "white noise test has Fisher g effect size"
    (let* ([xs (make-pseudo-noise 128 777)]
           [result (ts-spectral-white-noise-test xs)]
           [g (test-effect-size result)])
      ;; Fisher's g should be a positive number
      (assert-true (> g 0)))))

(run-all-tests)
