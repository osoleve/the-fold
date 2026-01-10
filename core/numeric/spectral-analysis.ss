;;; core/numeric/spectral-analysis.ss — Spectral Analysis Tools
;;;
;;; Spectral analysis tools for signal processing including:
;;; - Short-Time Fourier Transform (STFT) and Spectrograms
;;; - Power Spectral Density (PSD) estimation
;;; - Welch's method for PSD estimation
;;; - Periodogram analysis
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - complex.ss
;;;   - dft.ss
;;;   - window-functions.ss

(load "core/base/prelude.ss")
(load "core/numeric/complex.ss")
(load "core/numeric/dft.ss")
(load "core/numeric/window-functions.ss")

;;; ============================================================
;;; Short-Time Fourier Transform (STFT)
;;; ============================================================

;;; stft-frame : Vector[Number] × Integer × Integer × Symbol → Vector[Complex]
;;; Compute one STFT frame.
;;;
;;; Parameters:
;;; - signal: Input signal
;;; - start: Starting index for this frame
;;; - frame-len: Length of the frame (window size)
;;; - window-type: Window function to apply
;;;
;;; Returns: DFT of the windowed frame
(define (stft-frame signal start frame-len window-type)
  (let* ([n (vector-length signal)]
         [window (make-window window-type frame-len)]
         [frame (make-vector frame-len 0)])
        ;; Extract frame (with bounds checking)
        (do ([i 0 (+ i 1)])
            ((= i frame-len))
            (let ([idx (+ start i)])
                 (when (< idx n)
                       (vector-set! frame i (vector-ref signal idx)))))
        ;; Apply window and compute DFT
        (let ([windowed (apply-window frame window)])
             (dft-real windowed))))

;;; stft : Vector[Number] × Integer × Integer × Symbol → Vector[Vector[Complex]]
;;; Compute Short-Time Fourier Transform.
;;;
;;; The STFT divides the signal into overlapping frames, applies a window
;;; to each frame, and computes the DFT of each windowed frame.
;;;
;;; Parameters:
;;; - signal: Input signal
;;; - frame-len: Length of each frame (window size)
;;; - hop-len: Number of samples to advance between frames
;;; - window-type: Window function type ('hann, 'hamming, 'blackman, etc.)
;;;
;;; Returns: Vector of DFT frames, where each frame is a complex vector
;;;
;;; Example:
;;;   (stft signal 512 256 'hann)  ; 50% overlap
(define (stft signal frame-len hop-len window-type)
  (let* ([n (vector-length signal)]
         ;; Calculate number of frames
         [num-frames (+ 1 (quotient (- n frame-len) hop-len))]
         [frames (make-vector num-frames)])
        (do ([i 0 (+ i 1)])
            ((= i num-frames) frames)
            (let ([start (* i hop-len)])
                 (vector-set! frames i
                              (stft-frame signal start frame-len window-type))))))

;;; istft : Vector[Vector[Complex]] × Integer × Integer × Symbol → Vector[Number]
;;; Inverse Short-Time Fourier Transform (overlap-add reconstruction).
;;;
;;; Parameters:
;;; - frames: STFT frames (from stft)
;;; - frame-len: Length of each frame
;;; - hop-len: Hop size used in forward STFT
;;; - window-type: Window type used in forward STFT
;;;
;;; Returns: Reconstructed time-domain signal
;;;
;;; Note: Uses overlap-add method with window normalization.
(define (istft frames frame-len hop-len window-type)
  (let* ([num-frames (vector-length frames)]
         ;; Calculate output length
         [signal-len (+ frame-len (* (- num-frames 1) hop-len))]
         [signal (make-vector signal-len 0)]
         [norm (make-vector signal-len 0)]
         [window (make-window window-type frame-len)])
        ;; Overlap-add reconstruction
        (do ([i 0 (+ i 1)])
            ((= i num-frames))
            (let* ([start (* i hop-len)]
                   [frame-fft (vector-ref frames i)]
                   [frame-time (idft frame-fft)])
                  ;; Add windowed frame to output
                  (do ([j 0 (+ j 1)])
                      ((= j frame-len))
                      (let ([idx (+ start j)])
                           (when (< idx signal-len)
                                 (let ([w (vector-ref window j)]
                                       [sample (complex-real (vector-ref frame-time j))])
                                      (vector-set! signal idx
                                                   (+ (vector-ref signal idx)
                                                      (* sample w)))
                                      (vector-set! norm idx
                                                   (+ (vector-ref norm idx)
                                                      (* w w)))))))))
        ;; Normalize by window overlap
        (do ([i 0 (+ i 1)])
            ((= i signal-len) signal)
            (let ([n (vector-ref norm i)])
                 (when (> n 1e-10)
                       (vector-set! signal i
                                    (/ (vector-ref signal i) n)))))))

;;; ============================================================
;;; Spectrogram
;;; ============================================================

;;; spectrogram : Vector[Number] × Integer × Integer × Symbol → Vector[Vector[Number]]
;;; Compute magnitude spectrogram from STFT.
;;;
;;; Returns a 2D array (as vector of vectors) where:
;;; - Each row is a frequency bin
;;; - Each column is a time frame
;;; - Values are magnitudes |X[k,n]|
;;;
;;; Parameters same as stft.
(define (spectrogram signal frame-len hop-len window-type)
  (let* ([stft-result (stft signal frame-len hop-len window-type)]
         [num-frames (vector-length stft-result)])
        (if (= num-frames 0)
            (make-vector 0)
            (let* ([freq-bins (vector-length (vector-ref stft-result 0))]
                   [result (make-vector num-frames)])
                  (do ([i 0 (+ i 1)])
                      ((= i num-frames) result)
                      (vector-set! result i
                                   (magnitude-spectrum (vector-ref stft-result i))))))))

;;; power-spectrogram : Vector[Number] × Integer × Integer × Symbol → Vector[Vector[Number]]
;;; Compute power spectrogram (magnitude squared) from STFT.
;;;
;;; Returns |X[k,n]|² for each time-frequency bin.
(define (power-spectrogram signal frame-len hop-len window-type)
  (let* ([stft-result (stft signal frame-len hop-len window-type)]
         [num-frames (vector-length stft-result)])
        (if (= num-frames 0)
            (make-vector 0)
            (let* ([freq-bins (vector-length (vector-ref stft-result 0))]
                   [result (make-vector num-frames)])
                  (do ([i 0 (+ i 1)])
                      ((= i num-frames) result)
                      (vector-set! result i
                                   (power-spectrum (vector-ref stft-result i))))))))

;;; log-spectrogram : Vector[Number] × Integer × Integer × Symbol → Vector[Vector[Number]]
;;; Compute log-magnitude spectrogram in dB.
;;;
;;; Returns 20*log₁₀(|X[k,n]| + ε) where ε = 1e-10 prevents log(0).
(define (log-spectrogram signal frame-len hop-len window-type)
  (let* ([spec (spectrogram signal frame-len hop-len window-type)]
         [num-frames (vector-length spec)])
        (if (= num-frames 0)
            spec
            (let* ([freq-bins (vector-length (vector-ref spec 0))]
                   [result (make-vector num-frames)])
                  (do ([i 0 (+ i 1)])
                      ((= i num-frames) result)
                      (let* ([frame (vector-ref spec i)]
                             [log-frame (make-vector freq-bins)])
                            (do ([k 0 (+ k 1)])
                                ((= k freq-bins))
                                (let ([mag (vector-ref frame k)])
                                     (vector-set! log-frame k
                                                  (* 20 (log10 (+ mag 1e-10))))))
                            (vector-set! result i log-frame)))))))

;;; ============================================================
;;; Periodogram (Single-Frame PSD Estimate)
;;; ============================================================

;;; periodogram : Vector[Number] × Symbol → Vector[Number]
;;; Compute periodogram PSD estimate.
;;;
;;; The periodogram is the magnitude squared of the DFT, scaled by
;;; the window energy and sampling rate.
;;;
;;; PSD[k] = (1 / (fs * Σw²)) * |DFT(x·w)[k]|²
;;;
;;; Parameters:
;;; - signal: Input signal
;;; - window-type: Window function type
;;;
;;; Returns: Power spectral density estimate (one-sided for real signals)
;;;
;;; Note: For fs parameter, we assume fs=1 (normalized frequency).
;;; To get actual PSD, multiply by fs.
(define (periodogram signal window-type)
  (let* ([n (vector-length signal)]
         [window (make-window window-type n)]
         [windowed (apply-window signal window)]
         [window-energy (window-energy window)]
         [fft-result (dft-real windowed)]
         [psd (make-vector n)])
        ;; Compute |FFT|² / (N * window_energy)
        (do ([k 0 (+ k 1)])
            ((= k n) psd)
            (let* ([X-k (vector-ref fft-result k)]
                   [mag (complex-magnitude X-k)]
                   [power (* mag mag)])
                  (vector-set! psd k (/ power (* n window-energy)))))))

;;; ============================================================
;;; Welch's Method (Averaged Periodogram)
;;; ============================================================

;;; welch-psd : Vector[Number] × Integer × Integer × Symbol → Vector[Number]
;;; Compute Power Spectral Density using Welch's method.
;;;
;;; Welch's method:
;;; 1. Divide signal into overlapping segments
;;; 2. Window each segment
;;; 3. Compute periodogram of each segment
;;; 4. Average the periodograms
;;;
;;; This reduces variance compared to a single periodogram.
;;;
;;; Parameters:
;;; - signal: Input signal
;;; - segment-len: Length of each segment (window size)
;;; - hop-len: Number of samples between segments (overlap = segment-len - hop-len)
;;; - window-type: Window function type
;;;
;;; Returns: Averaged PSD estimate
;;;
;;; Common choices:
;;; - segment-len = 256, hop-len = 128 (50% overlap)
;;; - window-type = 'hann or 'hamming
(define (welch-psd signal segment-len hop-len window-type)
  (let* ([n (vector-length signal)]
         [num-segments (+ 1 (quotient (- n segment-len) hop-len))]
         [psd (make-vector segment-len 0)]
         [window (make-window window-type segment-len)]
         [window-energy (window-energy window)])
        ;; Accumulate periodograms
        (do ([seg 0 (+ seg 1)])
            ((= seg num-segments))
            (let* ([start (* seg hop-len)]
                   [segment (make-vector segment-len 0)])
                  ;; Extract segment
                  (do ([i 0 (+ i 1)])
                      ((= i segment-len))
                      (let ([idx (+ start i)])
                           (when (< idx n)
                                 (vector-set! segment i (vector-ref signal idx)))))
                  ;; Compute windowed FFT
                  (let* ([windowed (apply-window segment window)]
                         [fft-result (dft-real windowed)])
                        ;; Add |FFT|² to accumulator
                        (do ([k 0 (+ k 1)])
                            ((= k segment-len))
                            (let* ([X-k (vector-ref fft-result k)]
                                   [mag (complex-magnitude X-k)]
                                   [power (* mag mag)])
                                  (vector-set! psd k
                                               (+ (vector-ref psd k) power)))))))
        ;; Average and normalize
        (do ([k 0 (+ k 1)])
            ((= k segment-len) psd)
            (vector-set! psd k
                         (/ (vector-ref psd k)
                            (* num-segments segment-len window-energy))))))

;;; ============================================================
;;; Frequency Grid Utilities
;;; ============================================================

;;; spectrogram-frequencies : Integer × Number → Vector[Number]
;;; Compute frequency grid for spectrogram.
;;;
;;; Parameters:
;;; - n: FFT length (frame-len)
;;; - fs: Sample rate (Hz)
;;;
;;; Returns: Frequency values for each bin [0, fs/n, 2*fs/n, ..., fs/2]
(define (spectrogram-frequencies n fs)
  (freq-bins n fs))

;;; spectrogram-times : Integer × Integer × Integer × Number → Vector[Number]
;;; Compute time grid for spectrogram.
;;;
;;; Parameters:
;;; - signal-len: Length of input signal
;;; - frame-len: STFT frame length
;;; - hop-len: STFT hop length
;;; - fs: Sample rate (Hz)
;;;
;;; Returns: Time values for each frame center
(define (spectrogram-times signal-len frame-len hop-len fs)
  (let* ([num-frames (+ 1 (quotient (- signal-len frame-len) hop-len))]
         [times (make-vector num-frames)])
        (do ([i 0 (+ i 1)])
            ((= i num-frames) times)
            (let ([sample (+ (* i hop-len) (quotient frame-len 2))])
                 (vector-set! times i (/ sample fs))))))

;;; ============================================================
;;; Spectral Features
;;; ============================================================

;;; spectral-centroid : Vector[Number] × Vector[Number] → Number
;;; Compute spectral centroid (center of mass of spectrum).
;;;
;;; Centroid = Σ(f[k] * P[k]) / Σ(P[k])
;;;
;;; where f[k] are frequencies and P[k] are power values.
;;;
;;; The centroid indicates the "brightness" of a sound.
(define (spectral-centroid freqs power-spectrum)
  (let ([n (vector-length power-spectrum)])
       (let loop ([k 0] [weighted-sum 0] [total-power 0])
            (if (= k n)
                (if (< total-power 1e-10)
                    0
                    (/ weighted-sum total-power))
                (let ([freq (vector-ref freqs k)]
                      [power (vector-ref power-spectrum k)])
                     (loop (+ k 1)
                           (+ weighted-sum (* freq power))
                           (+ total-power power)))))))

;;; spectral-bandwidth : Vector[Number] × Vector[Number] × Number → Number
;;; Compute spectral bandwidth (weighted standard deviation around centroid).
;;;
;;; Bandwidth = sqrt(Σ((f[k] - centroid)² * P[k]) / Σ(P[k]))
;;;
;;; Indicates how spread out the spectrum is.
(define (spectral-bandwidth freqs power-spectrum centroid)
  (let ([n (vector-length power-spectrum)])
       (let loop ([k 0] [weighted-variance 0] [total-power 0])
            (if (= k n)
                (if (< total-power 1e-10)
                    0
                    (sqrt (/ weighted-variance total-power)))
                (let ([freq (vector-ref freqs k)]
                      [power (vector-ref power-spectrum k)]
                      [diff (- (vector-ref freqs k) centroid)])
                     (loop (+ k 1)
                           (+ weighted-variance (* diff diff power))
                           (+ total-power power)))))))

;;; spectral-rolloff : Vector[Number] × Vector[Number] × Number → Number
;;; Compute spectral rolloff frequency.
;;;
;;; The rolloff is the frequency below which a given percentage (e.g., 85%)
;;; of the total spectral energy is contained.
;;;
;;; Parameters:
;;; - freqs: Frequency values
;;; - power-spectrum: Power values
;;; - threshold: Fraction of energy (e.g., 0.85 for 85%)
;;;
;;; Returns: Rolloff frequency
(define (spectral-rolloff freqs power-spectrum threshold)
  (let* ([n (vector-length power-spectrum)]
         ;; Compute total energy
         [total-energy (let loop ([k 0] [sum 0])
                            (if (= k n)
                                sum
                                (loop (+ k 1) (+ sum (vector-ref power-spectrum k)))))]
         [target-energy (* threshold total-energy)])
        ;; Find rolloff point
        (let loop ([k 0] [cumulative 0])
             (if (or (= k n) (>= cumulative target-energy))
                 (if (= k 0)
                     (vector-ref freqs 0)
                     (vector-ref freqs (- k 1)))
                 (loop (+ k 1) (+ cumulative (vector-ref power-spectrum k)))))))
