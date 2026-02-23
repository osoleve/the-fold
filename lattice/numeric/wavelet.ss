;;; lattice/numeric/wavelet.ss — Wavelet Transforms
;;; @module wavelet
;;; @requires prelude vec iteration

(require 'prelude)
(require 'vec)
(require 'iteration)

(doc 'module 'wavelet)
(doc 'description "Wavelet transforms for multi-resolution signal analysis: Haar, Daubechies, DWT, IDWT, multi-resolution decomposition")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Wavelet Filter Coefficients
;;; ====

;;; Wavelet families are defined by their filter coefficients.
;;; Each wavelet has a scaling filter (low-pass) and wavelet filter (high-pass).

;;; haar-scaling-filter : → Vector[Number]
;;; Haar wavelet scaling (low-pass) filter coefficients.
;;; The simplest wavelet: h = [1/√2, 1/√2]
(define (haar-scaling-filter)
  (doc 'export #t)
  (let ([sqrt2-inv (/ 1 (sqrt 2))])
       (vector sqrt2-inv sqrt2-inv)))

;;; haar-wavelet-filter : → Vector[Number]
;;; Haar wavelet (high-pass) filter coefficients.
;;; g = [1/√2, -1/√2]
(define (haar-wavelet-filter)
  (doc 'export #t)
  (let ([sqrt2-inv (/ 1 (sqrt 2))])
       (vector sqrt2-inv (- sqrt2-inv))))

;;; daubechies-4-scaling-filter : → Vector[Number]
;;; Daubechies-4 (db2) scaling filter.
;;; Length 4, provides better smoothness than Haar.
(define (daubechies-4-scaling-filter)
  (doc 'export #t)
  (let ([sqrt3 (sqrt 3)]
        [denom (* 4 (sqrt 2))])
       (vector (/ (+ 1 sqrt3) denom)
               (/ (+ 3 sqrt3) denom)
               (/ (- 3 sqrt3) denom)
               (/ (- 1 sqrt3) denom))))

;;; daubechies-4-wavelet-filter : Vector[Number] → Vector[Number]
;;; Compute wavelet filter from scaling filter using QMF relationship.
;;; g[n] = (-1)^n * h[N-1-n]
(define (qmf-wavelet-from-scaling h)
  (doc 'export #t)
  (let ([n (vector-length h)])
       (vec-tabulate n i
         (let* ([sign (if (even? i) 1 -1)]
                [j (- n 1 i)])
               (* sign (vector-ref h j))))))

;;; daubechies-4-wavelet-filter : → Vector[Number]
;;; Daubechies-4 wavelet filter derived from scaling filter.
(define (daubechies-4-wavelet-filter)
  (doc 'export #t)
  (qmf-wavelet-from-scaling (daubechies-4-scaling-filter)))

;;; daubechies-6-scaling-filter : → Vector[Number]
;;; Daubechies-6 (db3) scaling filter.
;;; Length 6, higher smoothness.
(define (daubechies-6-scaling-filter)
  (doc 'export #t)
  (vector 0.3326705529509569
          0.8068915093133388
          0.4598775021193313
          -0.13501102001039084
          -0.08544127388224149
          0.035226291882100656))

;;; daubechies-6-wavelet-filter : → Vector[Number]
(define (daubechies-6-wavelet-filter)
  (doc 'export #t)
  (qmf-wavelet-from-scaling (daubechies-6-scaling-filter)))

;;; ====
;;; Filter Operations
;;; ====

;;; reverse-filter : Vector[Number] → Vector[Number]
;;; Time-reverse a filter (needed for synthesis filters).
(define (reverse-filter h)
  (doc 'export #t)
  (let ([n (vector-length h)])
       (vec-tabulate n i
         (vector-ref h (- n 1 i)))))

;;; convolve-downsample : Vector[Number] × Vector[Number] × Integer → Vector[Number]
;;; Convolve signal with filter and downsample by factor.
;;;
;;; This is the core operation for wavelet decomposition:
;;; 1. Convolve signal with filter
;;; 2. Downsample by keeping every factor-th sample
;;;
;;; Used for both approximation (scaling filter) and detail (wavelet filter).
(define (convolve-downsample signal filter factor)
  (doc 'export #t)
  (let* ([n (vector-length signal)]
         [m (vector-length filter)]
         ;; Output length after downsampling
         [out-len (ceiling (/ n factor))])
        ;; For each output sample
        (vec-tabulate out-len i
          (let ([pos (* i factor)])
               ;; Compute convolution at this position
               ;; y[i] = Σ_k h[k] * x[pos + k]
               (let loop ([k 0] [acc 0])
                    (if (= k m)
                        acc
                        (let ([sig-idx (+ pos k)])
                             (if (and (>= sig-idx 0) (< sig-idx n))
                                 (loop (+ k 1)
                                       (+ acc (* (vector-ref filter k)
                                                 (vector-ref signal sig-idx))))
                                 (loop (+ k 1) acc)))))))))

;;; upsample-convolve : Vector[Number] × Vector[Number] × Integer → Vector[Number]
;;; Upsample by inserting zeros, then convolve with filter.
;;;
;;; This is the core operation for wavelet reconstruction:
;;; 1. Upsample by inserting (factor-1) zeros between samples
;;; 2. Convolve with filter
;;;
;;; Formula: x[n] = Σ_k h[n - factor*k] * coeffs[k]
;;;
;;; Used for synthesis from approximation and detail coefficients.
(define (upsample-convolve coeffs filter factor)
  (doc 'export #t)
  (let* ([n (vector-length coeffs)]
         [m (vector-length filter)]
         ;; Output length
         [out-len (* n factor)]
         [result (make-vector out-len 0)])
        ;; For each output sample
        (do ([out-idx 0 (+ out-idx 1)])
            ((= out-idx out-len) result)
            (let ([sum 0])
                 ;; Sum contributions from all coefficients
                 (do ([coeff-idx 0 (+ coeff-idx 1)])
                     ((= coeff-idx n)
                      (vector-set! result out-idx sum))
                     (let* ([upsampled-pos (* coeff-idx factor)]
                            [filter-offset (- out-idx upsampled-pos)])
                           ;; Check if this coefficient contributes to this output sample
                           (when (and (>= filter-offset 0) (< filter-offset m))
                                 (set! sum (+ sum (* (vector-ref coeffs coeff-idx)
                                                     (vector-ref filter filter-offset)))))))))))

;;; ====
;;; Discrete Wavelet Transform (DWT)
;;; ====

;;; dwt-step : Vector[Number] × Vector[Number] × Vector[Number] → (Vector × Vector)
;;; One level of DWT decomposition.
;;;
;;; Returns: (approximation . detail)
;;; - approximation: low-frequency components (convolved with scaling filter)
;;; - detail: high-frequency components (convolved with wavelet filter)
;;;
;;; Both outputs are downsampled by 2.
(define (dwt-step signal h g)
  (doc 'export #t)
  (let ([approx (convolve-downsample signal h 2)]
        [detail (convolve-downsample signal g 2)])
       (cons approx detail)))

;;; dwt : Vector[Number] × Integer × Vector[Number] × Vector[Number] → List
;;; Discrete Wavelet Transform with multi-level decomposition.
;;;
;;; Parameters:
;;; - signal: Input signal
;;; - levels: Number of decomposition levels
;;; - h: Scaling (low-pass) filter
;;; - g: Wavelet (high-pass) filter
;;;
;;; Returns: List of (approximation detail_1 detail_2 ... detail_n)
;;;
;;; Example:
;;;   (dwt signal 3 (haar-scaling-filter) (haar-wavelet-filter))
;;;   => (approx3 detail3 detail2 detail1)
;;;
;;; The approximation is at the coarsest scale, details are ordered from coarse to fine.
(define (dwt signal levels h g)
  (doc 'export #t)
  (let loop ([current signal]
             [level 0]
             [details '()])
       (if (= level levels)
           (cons current details)
           (let* ([result (dwt-step current h g)]
                  [approx (car result)]
                  [detail (cdr result)])
                 (loop approx (+ level 1) (cons detail details))))))

;;; idwt-step : Vector[Number] × Vector[Number] × Vector[Number] × Vector[Number] × Integer → Vector[Number]
;;; One level of inverse DWT reconstruction.
;;;
;;; Reconstructs signal from approximation and detail coefficients.
(define (idwt-step approx detail h g target-len)
  (doc 'export #t)
  (let* ([up-approx (upsample-convolve approx h 2)]
         [up-detail (upsample-convolve detail g 2)]
         ;; Determine actual output length (take minimum to handle odd lengths)
         [n (min (vector-length up-approx) (vector-length up-detail) target-len)])
        ;; Add upsampled components
        (vec-tabulate n i
          (+ (vector-ref up-approx i)
             (vector-ref up-detail i)))))

;;; idwt : List × Integer × Vector[Number] × Vector[Number] → Vector[Number]
;;; Inverse Discrete Wavelet Transform.
;;;
;;; Parameters:
;;; - coeffs: List of (approximation detail_n ... detail_1) from dwt
;;; - target-len: Desired output length (should match original signal length)
;;; - h: Scaling filter (same as used in forward DWT)
;;; - g: Wavelet filter (same as used in forward DWT)
;;;
;;; Returns: Reconstructed signal
(define (idwt coeffs target-len h g)
  (doc 'export #t)
  (if (null? coeffs)
      (make-vector 0)
      (let loop ([approx (car coeffs)]
                 [details (cdr coeffs)]
                 [current-len target-len])
           (if (null? details)
               ;; If we still have extra length, trim to target
               (if (> (vector-length approx) target-len)
                   (vec-tabulate target-len i (vector-ref approx i))
                   approx)
               (let* ([detail (car details)]
                      [rest (cdr details)]
                      ;; Reconstruct one level
                      [reconstructed (idwt-step approx detail h g current-len)])
                     (loop reconstructed rest current-len))))))

;;; ====
;;; Wavelet Families API
;;; ====

;;; get-wavelet-filters : Symbol → (Vector × Vector)
;;; Get scaling and wavelet filters for a given wavelet family.
;;;
;;; Families:
;;; - 'haar : Haar wavelet (length 2)
;;; - 'db2, 'db4 : Daubechies-4 (length 4)
;;; - 'db3, 'db6 : Daubechies-6 (length 6)
;;;
;;; Returns: (cons h g) where h is scaling filter, g is wavelet filter
(define (get-wavelet-filters family)
  (doc 'export #t)
  (case family
        [(haar) (cons (haar-scaling-filter) (haar-wavelet-filter))]
        [(db2 db4) (cons (daubechies-4-scaling-filter) (daubechies-4-wavelet-filter))]
        [(db3 db6) (cons (daubechies-6-scaling-filter) (daubechies-6-wavelet-filter))]
        [else (error 'get-wavelet-filters "unknown wavelet family" family)]))

;;; dwt-family : Vector[Number] × Integer × Symbol → List
;;; DWT using a named wavelet family.
;;;
;;; Example:
;;;   (dwt-family signal 3 'haar)
;;;   (dwt-family signal 4 'db4)
(define (dwt-family signal levels family)
  (doc 'export #t)
  (let* ([filters (get-wavelet-filters family)]
         [h (car filters)]
         [g (cdr filters)])
        (dwt signal levels h g)))

;;; idwt-family : List × Integer × Symbol → Vector[Number]
;;; Inverse DWT using a named wavelet family.
(define (idwt-family coeffs target-len family)
  (doc 'export #t)
  (let* ([filters (get-wavelet-filters family)]
         [h (car filters)]
         [g (cdr filters)])
        (idwt coeffs target-len h g)))

;;; ====
;;; Multi-Resolution Analysis
;;; ====

;;; wavelet-decompose : Vector[Number] × Integer × Symbol → List
;;; Decompose signal into multi-resolution components.
;;;
;;; This is the same as dwt-family, but with clearer semantics
;;; for multi-resolution analysis.
;;;
;;; Returns: (approx detail_n ... detail_1)
;;; where approx is the smoothest approximation and each detail
;;; captures fluctuations at a specific scale.
(define (wavelet-decompose signal levels family)
  (doc 'export #t)
  (dwt-family signal levels family))

;;; wavelet-reconstruct : List × Integer × Symbol → Vector[Number]
;;; Reconstruct signal from multi-resolution components.
(define (wavelet-reconstruct coeffs target-len family)
  (doc 'export #t)
  (idwt-family coeffs target-len family))

;;; get-approximation : List → Vector[Number]
;;; Extract approximation coefficients from DWT result.
(define (get-approximation dwt-result)
  (doc 'export #t)
  (car dwt-result))

;;; get-details : List → List[Vector[Number]]
;;; Extract detail coefficients from DWT result (coarse to fine).
(define (get-details dwt-result)
  (doc 'export #t)
  (cdr dwt-result))

;;; get-detail-level : List × Integer → Vector[Number]
;;; Get detail coefficients at a specific level.
;;; Level 1 is finest, level n is coarsest.
(define (get-detail-level dwt-result level)
  (doc 'export #t)
  (let* ([details (get-details dwt-result)]
         [num-details (length details)]
         ;; Details are stored coarse-to-fine, so reverse indexing
         [idx (- num-details level)])
        (if (or (< idx 0) (>= idx num-details))
            (error 'get-detail-level "invalid level" level)
            (list-ref details idx))))

;;; ====
;;; Wavelet Thresholding (Denoising)
;;; ====

;;; hard-threshold : Number × Number → Number
;;; Hard thresholding: set to zero if |x| < threshold.
(define (hard-threshold x threshold)
  (doc 'export #t)
  (if (< (abs x) threshold)
      0
      x))

;;; soft-threshold : Number × Number → Number
;;; Soft thresholding: shrink toward zero by threshold amount.
(define (soft-threshold x threshold)
  (doc 'export #t)
  (cond
   [(> x threshold) (- x threshold)]
   [(< x (- threshold)) (+ x threshold)]
   [else 0]))

;;; threshold-coefficients : Vector[Number] × Number × Symbol → Vector[Number]
;;; Apply thresholding to wavelet coefficients.
;;;
;;; Types:
;;; - 'hard : Hard thresholding
;;; - 'soft : Soft thresholding
(define (threshold-coefficients coeffs threshold type)
  (doc 'export #t)
  (let* ([n (vector-length coeffs)]
         [threshold-fn (case type
                             [(hard) hard-threshold]
                             [(soft) soft-threshold]
                             [else (error 'threshold-coefficients "unknown threshold type" type)])])
        (vec-tabulate n i
          (threshold-fn (vector-ref coeffs i) threshold))))

;;; wavelet-denoise : Vector[Number] × Integer × Symbol × Number × Symbol → Vector[Number]
;;; Denoise signal using wavelet thresholding.
;;;
;;; Process:
;;; 1. Decompose signal using DWT
;;; 2. Threshold detail coefficients
;;; 3. Reconstruct using IDWT
;;;
;;; Parameters:
;;; - signal: Noisy input signal
;;; - levels: Number of decomposition levels
;;; - family: Wavelet family
;;; - threshold: Threshold value
;;; - threshold-type: 'hard or 'soft
;;;
;;; Returns: Denoised signal
(define (wavelet-denoise signal levels family threshold threshold-type)
  (doc 'export #t)
  (let* ([coeffs (dwt-family signal levels family)]
         [approx (car coeffs)]
         [details (cdr coeffs)]
         ;; Threshold all detail coefficients
         [thresholded-details (map (lambda (d)
                                           (threshold-coefficients d threshold threshold-type))
                                   details)]
         ;; Reconstruct
         [new-coeffs (cons approx thresholded-details)])
        (idwt-family new-coeffs (vector-length signal) family)))

;;; ====
;;; Wavelet Energy and Statistics
;;; ====

;;; wavelet-energy : Vector[Number] → Number
;;; Compute energy (sum of squares) of wavelet coefficients.
(define (wavelet-energy coeffs)
  (doc 'export #t)
  (let ([n (vector-length coeffs)])
       (let loop ([i 0] [energy 0])
            (if (= i n)
                energy
                (let ([c (vector-ref coeffs i)])
                     (loop (+ i 1) (+ energy (* c c))))))))

;;; wavelet-energy-distribution : List → List[Number]
;;; Compute energy at each decomposition level.
;;;
;;; Returns: List of energies (approximation energy_n ... energy_1)
(define (wavelet-energy-distribution dwt-result)
  (doc 'export #t)
  (map wavelet-energy dwt-result))

;;; wavelet-energy-ratio : List → List[Number]
;;; Compute fraction of total energy at each level.
(define (wavelet-energy-ratio dwt-result)
  (doc 'export #t)
  (let* ([energies (wavelet-energy-distribution dwt-result)]
         [total-energy (apply + energies)])
        (map (lambda (e) (/ e total-energy)) energies)))
