;;; core/info-theory/rate-distortion.ss — Rate-Distortion Theory
;;;
;;; Implements rate-distortion theory functions:
;;;   - Distortion measures (MSE, MAE, Hamming)
;;;   - Rate-distortion functions for common sources
;;;   - Quantization algorithms (scalar, Lloyd-Max)
;;;   - Operational rate-distortion analysis
;;;
;;; Rate-distortion theory characterizes the fundamental tradeoff between
;;; compression rate (bits) and reproduction fidelity (distortion).
;;;
;;; This is Core code: pure, total, assumes valid input.
;;;
;;; Dependencies:
;;;   - entropy.ss (for entropy calculations)

(load "lattice/info/entropy.ss")

;;; ====
;;; Distortion Measures
;;; ====

;;; squared-error : Real × Real → Real
;;; Squared error distortion d(x,y) = (x-y)^2
(define (squared-error x y)
  (* (- x y) (- x y)))

;;; absolute-error : Real × Real → Real
;;; Absolute error distortion d(x,y) = |x-y|
(define (absolute-error x y)
  (abs (- x y)))

;;; hamming-distortion : α × α → Nat
;;; Hamming distortion d(x,y) = 0 if x=y, 1 otherwise
(define (hamming-distortion x y)
  (if (equal? x y) 0 1))

;;; mse : (List Real) × (List Real) → Real
;;; Mean squared error: (1/n) * sum((xi - yi)^2)
(define (mse xs ys)
  (if (null? xs)
      0
      (/ (fold-left + 0 (map squared-error xs ys))
         (length xs))))

;;; mae : (List Real) × (List Real) → Real
;;; Mean absolute error: (1/n) * sum(|xi - yi|)
(define (mae xs ys)
  (if (null? xs)
      0
      (/ (fold-left + 0 (map absolute-error xs ys))
         (length xs))))

;;; rmse : (List Real) × (List Real) → Real
;;; Root mean squared error: sqrt(MSE)
(define (rmse xs ys)
  (sqrt (mse xs ys)))

;;; normalized-mse : (List Real) × (List Real) → Real
;;; Normalized MSE: MSE / variance(original)
(define (normalized-mse original reconstructed)
  (let ([m (mse original reconstructed)]
        [v (variance original)])
       (if (<= v 0)
           0
           (/ m v))))

;;; snr : (List Real) × (List Real) → Real
;;; Signal-to-noise ratio (linear): var(signal) / MSE
(define (snr original reconstructed)
  (let ([v (variance original)]
        [m (mse original reconstructed)])
       (if (<= m 0)
           +inf.0
           (/ v m))))

;;; snr-db : (List Real) × (List Real) → Real
;;; Signal-to-noise ratio in decibels: 10 * log10(SNR)
(define (snr-db original reconstructed)
  (let ([s (snr original reconstructed)])
       (if (= s +inf.0)
           +inf.0
           (* 10 (/ (log2 s) (log2 10))))))

;;; psnr : (List Real) × (List Real) × Real → Real
;;; Peak signal-to-noise ratio in dB: 10 * log10(peak^2 / MSE)
(define (psnr original reconstructed peak)
  (let ([m (mse original reconstructed)])
       (if (<= m 0)
           +inf.0
           (* 10 (/ (log2 (/ (* peak peak) m)) (log2 10))))))

;;; ====
;;; Statistical Helpers
;;; ====

;;; mean : (List Real) → Real
(define (mean xs)
  (if (null? xs)
      0
      (/ (fold-left + 0 xs) (length xs))))

;;; variance : (List Real) → Real
;;; Population variance: (1/n) * sum((xi - mu)^2)
(define (variance xs)
  (if (null? xs)
      0
      (let ([mu (mean xs)])
           (/ (fold-left + 0 (map (lambda (x) (squared-error x mu)) xs))
              (length xs)))))

;;; ====
;;; Rate-Distortion Functions
;;; ====

;;; gaussian-rate-distortion : Real × Real → Real
;;; Rate-distortion function for Gaussian source with variance sigma^2.
;;; R(D) = (1/2) * log2(sigma^2 / D) for D <= sigma^2
;;; R(D) = 0 for D > sigma^2
(define (gaussian-rate-distortion variance distortion)
  (cond
   [(<= variance 0) 0]
   [(<= distortion 0) +inf.0]  ; Perfect reproduction needs infinite rate
   [(>= distortion variance) 0]  ; Large distortion needs zero rate
   [else (* 0.5 (log2 (/ variance distortion)))]))

;;; gaussian-distortion-rate : Real × Real → Real
;;; Distortion-rate function for Gaussian source.
;;; D(R) = sigma^2 * 2^(-2R)
(define (gaussian-distortion-rate variance rate)
  (if (<= rate 0)
      variance  ; Zero rate gives full variance as distortion
      (* variance (expt 2 (* -2 rate)))))

;;; binary-rate-distortion : Real → Real
;;; Rate-distortion function for binary symmetric source (p=0.5).
;;; R(D) = 1 - H(D) for D <= 0.5
;;; R(D) = 0 for D > 0.5
(define (binary-rate-distortion distortion)
  (cond
   [(<= distortion 0) 1]  ; Perfect reproduction
   [(>= distortion 0.5) 0]  ; Maximum distortion (random output)
   [else (- 1 (binary-entropy distortion))]))

;;; binary-distortion-rate : Real → Real
;;; Distortion-rate function for binary source.
;;; D(R) = H^(-1)(1-R) where H is binary entropy
;;; Approximated using binary search.
(define (binary-distortion-rate rate)
  (cond
   [(<= rate 0) 0.5]
   [(>= rate 1) 0]
   [else (binary-entropy-inverse (- 1 rate))]))

;;; binary-entropy-inverse : Real → Real
;;; Inverse of binary entropy function (for h in [0,1]).
;;; Uses binary search to find p such that H(p) = h.
(define (binary-entropy-inverse h)
  (if (<= h 0)
      0
      (if (>= h 1)
          0.5
          (binary-search-entropy h 0 0.5 0.0001))))

;;; binary-search-entropy : Real × Real × Real × Real → Real
(define (binary-search-entropy target low high tolerance)
  (let* ([mid (/ (+ low high) 2)]
         [h-mid (binary-entropy mid)])
        (cond
         [(< (- high low) tolerance) mid]
         [(< h-mid target) (binary-search-entropy target mid high tolerance)]
         [else (binary-search-entropy target low mid tolerance)])))

;;; ====
;;; Uniform Scalar Quantization
;;; ====

;;; uniform-quantize : Real × Real × Nat → Real
;;; Quantize x to one of n uniform levels in [0, max-val].
;;; Returns the quantized value (reconstruction level).
(define (uniform-quantize x max-val n-levels)
  (let* ([step (/ max-val n-levels)]
         [level (min (- n-levels 1)
                     (max 0 (exact (floor (/ x step)))))]
         [reconstruction (+ (* step level) (/ step 2))])
        reconstruction))

;;; uniform-quantize-list : (List Real) × Real × Nat → (List Real)
;;; Quantize a list of values uniformly.
(define (uniform-quantize-list xs max-val n-levels)
  (map (lambda (x) (uniform-quantize x max-val n-levels)) xs))

;;; uniform-quantization-distortion : Real × Nat → Real
;;; Theoretical MSE for uniform quantizer on uniform [0,max] source.
;;; D = step^2 / 12 where step = max/n
(define (uniform-quantization-distortion max-val n-levels)
  (let ([step (/ max-val n-levels)])
       (/ (* step step) 12)))

;;; quantization-bits : Nat → Real
;;; Bits needed for n-level quantizer: log2(n)
(define (quantization-bits n-levels)
  (if (<= n-levels 1)
      0
      (log2 n-levels)))

;;; ====
;;; Lloyd-Max Quantization
;;; ====
;;;
;;; Lloyd-Max algorithm finds optimal quantization levels for a
;;; given source distribution by iteratively refining:
;;;   1. Decision boundaries (midpoints between levels)
;;;   2. Reconstruction levels (centroids of regions)

;;; lloyd-max-quantizer : (List Real) × Nat × Nat → (List Real)
;;; Train Lloyd-Max quantizer on samples.
;;; Returns list of reconstruction levels.
(define (lloyd-max-quantizer samples n-levels max-iter)
  (if (or (null? samples) (<= n-levels 0))
      '()
      (let* ([sorted (list-sort < samples)]
             [min-val (car sorted)]
             [max-val (car (reverse sorted))]
             [init-levels (uniform-init-levels min-val max-val n-levels)])
            (lloyd-max-iterate samples init-levels max-iter))))

;;; uniform-init-levels : Real × Real × Nat → (List Real)
;;; Initialize reconstruction levels uniformly.
(define (uniform-init-levels min-val max-val n-levels)
  (let ([step (/ (- max-val min-val) n-levels)])
       (map (lambda (i) (+ min-val (* step (+ i 0.5))))
            (iota n-levels))))

;;; lloyd-max-iterate : (List Real) × (List Real) × Nat → (List Real)
;;; Iterate Lloyd-Max algorithm until convergence or max iterations.
(define (lloyd-max-iterate samples levels iter)
  (if (<= iter 0)
      levels
      (let* ([boundaries (compute-boundaries levels)]
             [new-levels (compute-centroids samples boundaries)])
            (if (levels-converged? levels new-levels 0.0001)
                new-levels
                (lloyd-max-iterate samples new-levels (- iter 1))))))

;;; compute-boundaries : (List Real) → (List Real)
;;; Compute decision boundaries as midpoints between levels.
(define (compute-boundaries levels)
  (if (or (null? levels) (null? (cdr levels)))
      '()
      (cons (/ (+ (car levels) (cadr levels)) 2)
            (compute-boundaries (cdr levels)))))

;;; compute-centroids : (List Real) × (List Real) → (List Real)
;;; Compute centroids of each region defined by boundaries.
(define (compute-centroids samples boundaries)
  (let ([regions (partition-by-boundaries samples boundaries)])
       (map region-centroid regions)))

;;; partition-by-boundaries : (List Real) × (List Real) → (List (List Real))
;;; Partition samples into regions based on boundaries.
(define (partition-by-boundaries samples boundaries)
  (let ([n-regions (+ (length boundaries) 1)])
       (map (lambda (i)
                    (filter (lambda (x) (in-region? x i boundaries)) samples))
            (iota n-regions))))

;;; in-region? : Real × Nat × (List Real) → Boolean
;;; Check if x belongs to region i (0-indexed).
(define (in-region? x i boundaries)
  (let ([lower (if (= i 0) -inf.0 (list-ref boundaries (- i 1)))]
        [upper (if (>= i (length boundaries)) +inf.0 (list-ref boundaries i))])
       (and (>= x lower) (< x upper))))

;;; region-centroid : (List Real) → Real
;;; Compute centroid (mean) of a region.
(define (region-centroid region)
  (if (null? region)
      0  ; Empty region: arbitrary value
      (mean region)))

;;; levels-converged? : (List Real) × (List Real) × Real → Boolean
;;; Check if levels have converged within tolerance.
(define (levels-converged? old new tolerance)
  (cond
   [(null? old) #t]
   [(null? new) #t]
   [else
    (and (< (abs (- (car old) (car new))) tolerance)
         (levels-converged? (cdr old) (cdr new) tolerance))]))

;;; lloyd-max-quantize : Real × (List Real) → Real
;;; Quantize x using Lloyd-Max levels.
(define (lloyd-max-quantize x levels)
  (if (null? levels)
      0
      (let ([boundaries (compute-boundaries levels)])
           (find-region-level x levels boundaries 0))))

;;; find-region-level : Real × (List Real) × (List Real) × Nat → Real
(define (find-region-level x levels boundaries i)
  (cond
   [(null? boundaries) (car (reverse levels))]
   [(< x (car boundaries)) (list-ref levels i)]
   [else (find-region-level x levels (cdr boundaries) (+ i 1))]))

;;; ====
;;; Vector Quantization (Simple)
;;; ====

;;; vq-codebook-distance : (List Real) × (List Real) → Real
;;; Euclidean distance between two vectors.
(define (vq-codebook-distance v1 v2)
  (sqrt (fold-left + 0 (map squared-error v1 v2))))

;;; vq-find-nearest : (List Real) × (List (List Real)) → Nat
;;; Find index of nearest codebook vector.
(define (vq-find-nearest vector codebook)
  (let loop ([cb codebook] [i 0] [best-i 0] [best-dist +inf.0])
       (if (null? cb)
           best-i
           (let ([dist (vq-codebook-distance vector (car cb))])
                (if (< dist best-dist)
                    (loop (cdr cb) (+ i 1) i dist)
                    (loop (cdr cb) (+ i 1) best-i best-dist))))))

;;; vq-quantize : (List Real) × (List (List Real)) → (List Real)
;;; Quantize vector using codebook.
(define (vq-quantize vector codebook)
  (list-ref codebook (vq-find-nearest vector codebook)))

;;; ====
;;; Rate-Distortion Analysis
;;; ====

;;; operational-rate-distortion : (List Real) × (List Real) → Real × Real
;;; Compute operational (rate, distortion) point for a quantized signal.
;;; Returns (rate-in-bits, mse-distortion).
(define (operational-rate-distortion original quantized)
  (let* ([distortion (mse original quantized)]
         [unique-levels (unique-values quantized)]
         [rate (log2 (length unique-levels))])
        (cons rate distortion)))

;;; unique-values : (List α) → (List α)
;;; Alias for unique from prelude (provided for semantic clarity in this context).
(define unique-values unique)

;;; rd-gap : Real × Real × Real → Real
;;; Gap between operational point and rate-distortion bound.
;;; gap = R_operational - R_theoretical(D_operational)
(define (rd-gap op-rate op-distortion variance)
  (let ([theoretical-rate (gaussian-rate-distortion variance op-distortion)])
       (- op-rate theoretical-rate)))

;;; ====
;;; Entropy-Coded Quantization
;;; ====

;;; quantizer-entropy : (List Real) → Real
;;; Entropy of quantizer output distribution (in bits).
(define (quantizer-entropy quantized)
  (let* ([counts (count-values quantized)]
         [total (length quantized)]
         [probs (map (lambda (c) (/ c total)) (map cdr counts))])
        (entropy probs)))

;;; count-values : (List α) → (List (α × Nat))
;;; Count occurrences of each value.
(define (count-values lst)
  (let loop ([remaining lst] [counts '()])
       (if (null? remaining)
           counts
           (let* ([val (car remaining)]
                  [existing (assoc-helper val counts)])
                 (if existing
                     (loop (cdr remaining)
                           (map (lambda (p)
                                        (if (equal? (car p) val)
                                            (cons val (+ (cdr p) 1))
                                            p))
                                counts))
                     (loop (cdr remaining)
                           (cons (cons val 1) counts)))))))

;;; assoc-helper : α × (List (α × β)) → (α × β) | #f
(define (assoc-helper key alist)
  (cond
   [(null? alist) #f]
   [(equal? (caar alist) key) (car alist)]
   [else (assoc-helper key (cdr alist))]))

;;; entropy-coded-rate : (List Real) → Real
;;; Rate achievable with optimal entropy coding of quantizer output.
(define (entropy-coded-rate quantized)
  (quantizer-entropy quantized))

;;; ====
;;; Dithered Quantization
;;; ====

;;; dither-quantize : Real × Real × Nat × Real → Real
;;; Quantize with subtractive dither.
;;; Dither value should be uniform in [-step/2, step/2].
(define (dither-quantize x max-val n-levels dither)
  (let* ([step (/ max-val n-levels)]
         [dithered-x (+ x dither)]
         [quantized (uniform-quantize dithered-x max-val n-levels)])
        (- quantized dither)))

;;; ====
;;; High-Rate Approximations
;;; ====

;;; high-rate-bits-per-sample : Real × Real → Real
;;; High-rate approximation: R ≈ (1/2) * log2(sigma^2 / D)
;;; Same as Gaussian rate-distortion for large rates.
(define high-rate-bits-per-sample gaussian-rate-distortion)

;;; high-rate-distortion : Real × Real → Real
;;; High-rate approximation: D ≈ sigma^2 * 2^(-2R)
;;; Same as Gaussian distortion-rate.
(define high-rate-distortion gaussian-distortion-rate)

;;; ====
;;; Rate-Distortion Summary
;;; ====

;;; rd-summary : (List Real) × (List Real) × Real → String
;;; Generate summary of rate-distortion performance.
(define (rd-summary original quantized peak-val)
  (let* ([op-rd (operational-rate-distortion original quantized)]
         [rate (car op-rd)]
         [dist (cdr op-rd)]
         [var (variance original)]
         [gap (if (> var 0) (rd-gap rate dist var) 0)]
         [p (psnr original quantized peak-val)])
        (format "Rate: ~a bits/sample, MSE: ~a, PSNR: ~a dB, Gap: ~a bits"
                rate dist p gap)))
