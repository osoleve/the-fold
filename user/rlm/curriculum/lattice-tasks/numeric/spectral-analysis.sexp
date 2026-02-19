;;; spectral-analysis.sexp — Development tasks for lattice/numeric/spectral-analysis.ss
;;;
;;; Module: spectral-analysis (tier 0, depends on prelude, complex, dft, window-functions)
;;; 393 lines, ~12 exported functions
;;; Spectral analysis tools: STFT, spectrograms, PSD estimation (Welch's method),
;;; periodogram, spectral features (centroid, bandwidth, rolloff),
;;; frequency/time grid utilities.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement spectral-centroid
  ;; ──────────────────────────────────────────────
  ;; Center of mass of the spectrum — measures "brightness."

  (task
    (id "numeric/spectral-analysis/spectral-centroid-impl")
    (module spectral-analysis)
    (tier 0)
    (type implement)
    (description "Implement spectral-centroid: compute the spectral centroid (center of mass) of a power spectrum. Given a frequency vector and a power spectrum vector of the same length, compute: centroid = sum(freq[k] * power[k]) / sum(power[k]). This is the first moment of the spectral distribution — it indicates the 'brightness' of a sound. If total power is near zero (< 1e-10), return 0. Use a single-pass named let loop accumulating both the weighted sum and total power simultaneously.")
    (context "Available: vector-length, vector-ref, =, +, *, <, /. Named let with two accumulators.")
    (before "(define (spectral-centroid freqs power-spectrum)
  (doc 'export #t)
  ;; TODO: weighted mean of frequencies by power
  0)")
    (test-file "lattice/numeric/test-spectral-analysis.ss")
    (test-forms (";; All energy at one frequency
(let ([freqs (vector 0 100 200 300 400)]
      [power (vector 0 0 1.0 0 0)])
  (assert-true (< (abs (- (spectral-centroid freqs power) 200.0)) 0.001)))"
                 ";; Uniform power: centroid = mean frequency
(let ([freqs (vector 100 200 300)]
      [power (vector 1.0 1.0 1.0)])
  (assert-true (< (abs (- (spectral-centroid freqs power) 200.0)) 0.001)))"
                 ";; Zero power: returns 0
(let ([freqs (vector 100 200 300)]
      [power (vector 0 0 0)])
  (assert-equal 0 (spectral-centroid freqs power)))"))
    (available-modules (prelude complex dft window-functions))
    (hints ("Named let with k=0, weighted-sum=0, total-power=0"
            "Each step: freq = (vector-ref freqs k), power = (vector-ref power-spectrum k)"
            "Accumulate: weighted-sum += freq*power, total-power += power"
            "At end: if total-power < 1e-10 return 0 else (/ weighted-sum total-power)"))
    (reference-solution "(define (spectral-centroid freqs power-spectrum)
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
                  (+ total-power power)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement spectral-bandwidth
  ;; ──────────────────────────────────────────────
  ;; Weighted standard deviation around the centroid.

  (task
    (id "numeric/spectral-analysis/spectral-bandwidth-impl")
    (module spectral-analysis)
    (tier 0)
    (type implement)
    (description "Implement spectral-bandwidth: compute the spectral bandwidth (weighted standard deviation around the centroid). Given frequencies, power spectrum, and the centroid value: bandwidth = sqrt(sum((freq[k] - centroid)^2 * power[k]) / sum(power[k])). This measures how spread out the spectrum is. A narrow bandwidth means energy is concentrated near the centroid; a wide bandwidth means it is spread across frequencies. If total power < 1e-10, return 0. Single-pass accumulation like spectral-centroid.")
    (context "Available: vector-length, vector-ref, =, +, -, *, <, /, sqrt. Named let accumulating weighted variance and total power.")
    (before "(define (spectral-bandwidth freqs power-spectrum centroid)
  (doc 'export #t)
  ;; TODO: weighted std dev of frequencies around centroid
  0)")
    (test-file "lattice/numeric/test-spectral-analysis.ss")
    (test-forms (";; All energy at centroid: bandwidth = 0
(let ([freqs (vector 0 100 200 300 400)]
      [power (vector 0 0 1.0 0 0)])
  (assert-equal 0 (spectral-bandwidth freqs power 200)))"
                 ";; Symmetric spread
(let ([freqs (vector 100 200 300)]
      [power (vector 1.0 0 1.0)])
  ;; centroid = 200, variance = (100^2 + 100^2) / 2 = 10000
  (assert-true (< (abs (- (spectral-bandwidth freqs power 200) 100.0)) 0.001)))"
                 ";; Zero power: returns 0
(let ([freqs (vector 100 200)]
      [power (vector 0 0)])
  (assert-equal 0 (spectral-bandwidth freqs power 150)))"))
    (available-modules (prelude complex dft window-functions))
    (hints ("Named let with k=0, weighted-variance=0, total-power=0"
            "Each step: diff = (- freq[k] centroid), power = power-spectrum[k]"
            "Accumulate: weighted-variance += diff*diff*power, total-power += power"
            "At end: if total-power < 1e-10 return 0 else (sqrt (/ weighted-variance total-power))"))
    (reference-solution "(define (spectral-bandwidth freqs power-spectrum centroid)
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
                  (+ total-power power)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement spectral-rolloff
  ;; ──────────────────────────────────────────────
  ;; Cumulative energy threshold — where most energy lives.

  (task
    (id "numeric/spectral-analysis/spectral-rolloff-impl")
    (module spectral-analysis)
    (tier 0)
    (type implement)
    (description "Implement spectral-rolloff: find the frequency below which a given fraction of the total spectral energy is contained. For example, the 85% rolloff frequency is the lowest frequency f such that the cumulative power up to f is at least 85% of the total power. Steps: (1) Compute total energy as sum of all power values. (2) Compute target = threshold * total. (3) Walk through bins accumulating power until cumulative >= target. Return the frequency at that bin. If k reaches 0 before accumulating, return the first frequency.")
    (context "Available: vector-length, vector-ref, =, +, *, >=, or. Two-pass: first sum total, then scan for threshold crossing.")
    (before "(define (spectral-rolloff freqs power-spectrum threshold)
  (doc 'export #t)
  ;; TODO: find frequency where cumulative energy reaches threshold
  0)")
    (test-file "lattice/numeric/test-spectral-analysis.ss")
    (test-forms (";; Uniform power, 60% threshold
(let ([freqs (vector 0 100 200 300 400)]
      [power (vector 1.0 1.0 1.0 1.0 1.0)])
  ;; 60% of 5.0 = 3.0, need bins 0+1+2 = 3.0, rolloff at bin 2 = freq 200
  (assert-equal 200 (spectral-rolloff freqs power 0.6)))"
                 ";; All energy in first bin
(let ([freqs (vector 100 200 300)]
      [power (vector 5.0 0 0)])
  (assert-equal 100 (spectral-rolloff freqs power 0.5)))"
                 ";; 100% threshold needs all bins
(let ([freqs (vector 100 200 300)]
      [power (vector 1.0 1.0 1.0)])
  (assert-equal 300 (spectral-rolloff freqs power 1.0)))"))
    (available-modules (prelude complex dft window-functions))
    (hints ("First pass: sum all power values to get total-energy"
            "target-energy = (* threshold total-energy)"
            "Second pass: named let with k=0 and cumulative=0"
            "Each step: add power[k] to cumulative"
            "If cumulative >= target-energy or k reaches end: return freqs[k-1]"
            "Edge case: if k=0 at termination, return freqs[0]"))
    (reference-solution "(define (spectral-rolloff freqs power-spectrum threshold)
  (let* ([n (vector-length power-spectrum)]
         [total-energy (let loop ([k 0] [sum 0])
                         (if (= k n) sum
                             (loop (+ k 1) (+ sum (vector-ref power-spectrum k)))))]
         [target-energy (* threshold total-energy)])
    (let loop ([k 0] [cumulative 0])
      (if (or (= k n) (>= cumulative target-energy))
          (if (= k 0)
              (vector-ref freqs 0)
              (vector-ref freqs (- k 1)))
          (loop (+ k 1) (+ cumulative (vector-ref power-spectrum k)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement spectrogram-times
  ;; ──────────────────────────────────────────────
  ;; Time grid for STFT — straightforward arithmetic.

  (task
    (id "numeric/spectral-analysis/spectrogram-times-impl")
    (module spectral-analysis)
    (tier 0)
    (type implement)
    (description "Implement spectrogram-times: compute the time values (in seconds) for each frame center in an STFT spectrogram. The number of frames = 1 + floor((signal-len - frame-len) / hop-len). Each frame i has its center at sample position (i * hop-len + frame-len/2). Divide by the sample rate fs to get time in seconds. Return a vector of time values. This is used to label the time axis of spectrogram visualizations.")
    (context "Available: vector-length, vector-set!, make-vector, quotient, +, -, *, /. Simple arithmetic in a do loop.")
    (before "(define (spectrogram-times signal-len frame-len hop-len fs)
  (doc 'export #t)
  ;; TODO: compute time values for each STFT frame center
  (make-vector 0))")
    (test-file "lattice/numeric/test-spectral-analysis.ss")
    (test-forms (";; Correct number of frames
(let ([times (spectrogram-times 1024 256 128 44100)])
  (assert-equal 7 (vector-length times)))"
                 ";; First frame center is at frame-len/2 samples
(let ([times (spectrogram-times 1024 256 128 44100)])
  ;; First center at sample 128, time = 128/44100
  (assert-true (< (abs (- (vector-ref times 0) (/ 128 44100))) 0.0001)))"
                 ";; Times are monotonically increasing
(let ([times (spectrogram-times 512 64 32 8000)])
  (assert-true (< (vector-ref times 0) (vector-ref times 1))))"))
    (available-modules (prelude complex dft window-functions))
    (hints ("num-frames = (+ 1 (quotient (- signal-len frame-len) hop-len))"
            "Allocate result vector of size num-frames"
            "For each frame i: sample = (+ (* i hop-len) (quotient frame-len 2))"
            "Time = (/ sample fs)"
            "Store in result[i]"))
    (reference-solution "(define (spectrogram-times signal-len frame-len hop-len fs)
  (let* ([num-frames (+ 1 (quotient (- signal-len frame-len) hop-len))]
         [times (make-vector num-frames)])
    (do ([i 0 (+ i 1)])
        ((= i num-frames) times)
        (let ([sample (+ (* i hop-len) (quotient frame-len 2))])
          (vector-set! times i (/ sample fs))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement periodogram
  ;; ──────────────────────────────────────────────
  ;; Single-frame PSD estimate — window, FFT, |X|^2.

  (task
    (id "numeric/spectral-analysis/periodogram-impl")
    (module spectral-analysis)
    (tier 0)
    (type implement)
    (description "Implement periodogram: compute a power spectral density estimate of a signal. The periodogram is the simplest PSD estimator. Steps: (1) Create a window of the signal's length using make-window. (2) Apply the window to the signal using apply-window. (3) Compute the window energy using window-energy. (4) Compute the DFT of the windowed signal using dft-real. (5) For each frequency bin k, compute PSD[k] = |DFT[k]|^2 / (N * window-energy), where N is the signal length and |DFT[k]| is the complex magnitude obtained via complex-magnitude. Return the PSD vector.")
    (context "Available: vector-length, vector-ref, vector-set!, make-vector, make-window, apply-window, window-energy, dft-real, complex-magnitude, *, /. Multi-step pipeline using existing DSP building blocks.")
    (before "(define (periodogram signal window-type)
  (doc 'export #t)
  ;; TODO: PSD[k] = |DFT(windowed signal)[k]|^2 / (N * window_energy)
  (make-vector (vector-length signal) 0))")
    (test-file "lattice/numeric/test-spectral-analysis.ss")
    (test-forms (";; PSD values are non-negative
(let* ([sig (vector 1.0 0.0 -1.0 0.0)]
       [psd (periodogram sig 'rectangular)])
  (assert-true (>= (vector-ref psd 0) 0))
  (assert-true (>= (vector-ref psd 1) 0)))"
                 ";; Output length matches input length
(let* ([sig (vector 1.0 2.0 3.0 4.0)]
       [psd (periodogram sig 'hann)])
  (assert-equal 4 (vector-length psd)))"
                 ";; Constant signal: energy concentrated at DC (bin 0)
(let* ([sig (vector 1.0 1.0 1.0 1.0)]
       [psd (periodogram sig 'rectangular)])
  ;; DC bin should dominate
  (assert-true (> (vector-ref psd 0) (vector-ref psd 1))))"))
    (available-modules (prelude complex dft window-functions))
    (hints ("Get n = (vector-length signal)"
            "Create window: (make-window window-type n)"
            "Apply: (apply-window signal window)"
            "Get window energy: (window-energy window)"
            "FFT: (dft-real windowed)"
            "For each k: mag = (complex-magnitude (vector-ref fft-result k))"
            "PSD[k] = (* mag mag) / (* n window-energy)"))
    (reference-solution "(define (periodogram signal window-type)
  (let* ([n (vector-length signal)]
         [window (make-window window-type n)]
         [windowed (apply-window signal window)]
         [win-energy (window-energy window)]
         [fft-result (dft-real windowed)]
         [psd (make-vector n)])
    (do ([k 0 (+ k 1)])
        ((= k n) psd)
        (let* ([X-k (vector-ref fft-result k)]
               [mag (complex-magnitude X-k)]
               [power (* mag mag)])
          (vector-set! psd k (/ power (* n win-energy)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

) ;; end tasks
