;;; @module rate-distortion
;;; @requires entropy sort
;;; @description Rate-distortion theory: distortion metrics, R(D) curves for Gaussian/binary sources, Lloyd-Max quantization, vector quantization, entropy-coded rates.
;;; @purity total
;;; @stability stable

(require 'entropy)
(require 'sort)

(doc 'module 'rate-distortion)
(doc 'description "Rate-distortion theory: compression vs fidelity tradeoff")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'distortion-measures)

(define (squared-error x y)
  (doc 'export #t)
  (doc 'type '(-> Real Real Real))
  (doc 'description "Squared error distortion d(x,y) = (x-y)^2")
  (* (- x y) (- x y)))

(define (absolute-error x y)
  (doc 'export #t)
  (doc 'type '(-> Real Real Real))
  (doc 'description "Absolute error distortion d(x,y) = |x-y|")
  (abs (- x y)))

(define (hamming-distortion x y)
  (doc 'export #t)
  (doc 'type '(-> α α Nat))
  (doc 'description "Hamming distortion d(x,y) = 0 if x=y, 1 otherwise")
  (if (equal? x y) 0 1))

(define (mse xs ys)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Mean squared error: (1/n) * sum((xi - yi)^2)")
  (if (null? xs)
      0
      (/ (fold-left + 0 (map squared-error xs ys))
         (length xs))))

(define (mae xs ys)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Mean absolute error: (1/n) * sum(|xi - yi|)")
  (if (null? xs)
      0
      (/ (fold-left + 0 (map absolute-error xs ys))
         (length xs))))

(define (rmse xs ys)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Root mean squared error: sqrt(MSE)")
  (sqrt (mse xs ys)))

(define (normalized-mse original reconstructed)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Normalized MSE: MSE / variance(original)")
  (let ([m (mse original reconstructed)]
        [v (variance original)])
       (if (<= v 0)
           0
           (/ m v))))

(define (snr original reconstructed)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Signal-to-noise ratio (linear): var(signal) / MSE")
  (let ([v (variance original)]
        [m (mse original reconstructed)])
       (if (<= m 0)
           +inf.0
           (/ v m))))

(define (snr-db original reconstructed)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Signal-to-noise ratio in decibels: 10 * log10(SNR)")
  (let ([s (snr original reconstructed)])
       (if (= s +inf.0)
           +inf.0
           (* 10 (/ (log2 s) (log2 10))))))

(define (psnr original reconstructed peak)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real Real))
  (doc 'description "Peak signal-to-noise ratio in dB")
  (let ([m (mse original reconstructed)])
       (if (<= m 0)
           +inf.0
           (* 10 (/ (log2 (/ (* peak peak) m)) (log2 10))))))

(doc 'section 'statistical-helpers)

(define (mean xs)
  (doc 'export #t)
  (doc 'type '(-> (List Real) Real))
  (if (null? xs)
      0
      (/ (fold-left + 0 xs) (length xs))))

(define (variance xs)
  (doc 'export #t)
  (doc 'type '(-> (List Real) Real))
  (doc 'description "Population variance: (1/n) * sum((xi - mu)^2)")
  (if (null? xs)
      0
      (let ([mu (mean xs)])
           (/ (fold-left + 0 (map (lambda (x) (squared-error x mu)) xs))
              (length xs)))))

(doc 'section 'rate-distortion-functions)

(define (gaussian-rate-distortion variance distortion)
  (doc 'export #t)
  (doc 'type '(-> Real Real Real))
  (doc 'description "Rate-distortion function for Gaussian source")
  (cond
   [(<= variance 0) 0]
   [(<= distortion 0) +inf.0]  ; Perfect reproduction needs infinite rate
   [(>= distortion variance) 0]  ; Large distortion needs zero rate
   [else (* 0.5 (log2 (/ variance distortion)))]))

(define (gaussian-distortion-rate variance rate)
  (doc 'export #t)
  (doc 'type '(-> Real Real Real))
  (doc 'description "Distortion-rate function for Gaussian source")
  (if (<= rate 0)
      variance  ; Zero rate gives full variance as distortion
      (* variance (expt 2 (* -2 rate)))))

(define (binary-rate-distortion distortion)
  (doc 'export #t)
  (doc 'type '(-> Real Real))
  (doc 'description "Rate-distortion function for binary symmetric source")
  (cond
   [(<= distortion 0) 1]  ; Perfect reproduction
   [(>= distortion 0.5) 0]  ; Maximum distortion (random output)
   [else (- 1 (binary-entropy distortion))]))

(define (binary-distortion-rate rate)
  (doc 'export #t)
  (doc 'type '(-> Real Real))
  (doc 'description "Distortion-rate function for binary source")
  (cond
   [(<= rate 0) 0.5]
   [(>= rate 1) 0]
   [else (binary-entropy-inverse (- 1 rate))]))

(define (binary-entropy-inverse h)
  (doc 'export #t)
  (doc 'type '(-> Real Real))
  (doc 'description "Inverse of binary entropy function via binary search")
  (if (<= h 0)
      0
      (if (>= h 1)
          0.5
          (binary-search-entropy h 0 0.5 0.0001))))

(define (binary-search-entropy target low high tolerance)
  (doc 'type '(-> Real Real Real Real Real))
  (let* ([mid (/ (+ low high) 2)]
         [h-mid (binary-entropy mid)])
        (cond
         [(< (- high low) tolerance) mid]
         [(< h-mid target) (binary-search-entropy target mid high tolerance)]
         [else (binary-search-entropy target low mid tolerance)])))

(doc 'section 'uniform-scalar-quantization)

(define (uniform-quantize x max-val n-levels)
  (doc 'export #t)
  (doc 'type '(-> Real Real Nat Real))
  (doc 'description "Quantize x to one of n uniform levels in [0, max-val]")
  (let* ([step (/ max-val n-levels)]
         [level (min (- n-levels 1)
                     (max 0 (exact (floor (/ x step)))))]
         [reconstruction (+ (* step level) (/ step 2))])
        reconstruction))

(define (uniform-quantize-list xs max-val n-levels)
  (doc 'export #t)
  (doc 'type '(-> (List Real) Real Nat (List Real)))
  (doc 'description "Quantize a list of values uniformly")
  (map (lambda (x) (uniform-quantize x max-val n-levels)) xs))

(define (uniform-quantization-distortion max-val n-levels)
  (doc 'export #t)
  (doc 'type '(-> Real Nat Real))
  (doc 'description "Theoretical MSE for uniform quantizer")
  (let ([step (/ max-val n-levels)])
       (/ (* step step) 12)))

(define (quantization-bits n-levels)
  (doc 'export #t)
  (doc 'type '(-> Nat Real))
  (doc 'description "Bits needed for n-level quantizer: log2(n)")
  (if (<= n-levels 1)
      0
      (log2 n-levels)))

(doc 'section 'lloyd-max-quantization)

(define (lloyd-max-quantizer samples n-levels max-iter)
  (doc 'export #t)
  (doc 'type '(-> (List Real) Nat Nat (List Real)))
  (doc 'description "Train Lloyd-Max quantizer on samples")
  (if (or (null? samples) (<= n-levels 0))
      '()
      (let* ([sorted (sort-by < samples)]
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
  (doc 'export #t)
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
  (doc 'export #t)
  (if (or (null? levels) (null? (cdr levels)))
      '()
      (cons (/ (+ (car levels) (cadr levels)) 2)
            (compute-boundaries (cdr levels)))))

;;; compute-centroids : (List Real) × (List Real) → (List Real)
;;; Compute centroids of each region defined by boundaries.
(define (compute-centroids samples boundaries)
  (doc 'export #t)
  (let ([regions (partition-by-boundaries samples boundaries)])
       (map region-centroid regions)))

;;; partition-by-boundaries : (List Real) × (List Real) → (List (List Real))
;;; Partition samples into regions based on boundaries.
(define (partition-by-boundaries samples boundaries)
  (doc 'export #t)
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
  (doc 'export #t)
  (if (null? levels)
      0
      (let ([boundaries (compute-boundaries levels)])
           (find-region-level x levels boundaries 0))))

;;; find-region-level : Real × (List Real) × (List Real) × Nat → Real
(define (find-region-level x levels boundaries i)
  (doc 'export #t)
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
  (doc 'export #t)
  (sqrt (fold-left + 0 (map squared-error v1 v2))))

;;; vq-find-nearest : (List Real) × (List (List Real)) → Nat
;;; Find index of nearest codebook vector.
(define (vq-find-nearest vector codebook)
  (doc 'export #t)
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
  (doc 'export #t)
  (list-ref codebook (vq-find-nearest vector codebook)))

;;; ====
;;; Rate-Distortion Analysis
;;; ====

;;; operational-rate-distortion : (List Real) × (List Real) → Real × Real
;;; Compute operational (rate, distortion) point for a quantized signal.
;;; Returns (rate-in-bits, mse-distortion).
(define (operational-rate-distortion original quantized)
  (doc 'export #t)
  (let* ([distortion (mse original quantized)]
         [unique-levels (unique-values quantized)]
         [rate (log2 (length unique-levels))])
        (cons rate distortion)))

;;; unique-values : (List α) → (List α)
;;; Alias for unique from prelude (provided for semantic clarity in this context).
(doc unique-values 'export #t)
(define unique-values unique)

;;; rd-gap : Real × Real × Real → Real
;;; Gap between operational point and rate-distortion bound.
;;; gap = R_operational - R_theoretical(D_operational)
(define (rd-gap op-rate op-distortion variance)
  (doc 'export #t)
  (let ([theoretical-rate (gaussian-rate-distortion variance op-distortion)])
       (- op-rate theoretical-rate)))

;;; ====
;;; Entropy-Coded Quantization
;;; ====

;;; quantizer-entropy : (List Real) → Real
;;; Entropy of quantizer output distribution (in bits).
(define (quantizer-entropy quantized)
  (doc 'export #t)
  (let* ([counts (count-values quantized)]
         [total (length quantized)]
         [probs (map (lambda (c) (/ c total)) (map cdr counts))])
        (entropy probs)))

;;; count-values : (List α) → (List (α × Nat))
;;; Count occurrences of each value.
(define (count-values lst)
  (doc 'export #t)
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
  (doc 'export #t)
  (quantizer-entropy quantized))

;;; ====
;;; Dithered Quantization
;;; ====

;;; dither-quantize : Real × Real × Nat × Real → Real
;;; Quantize with subtractive dither.
;;; Dither value should be uniform in [-step/2, step/2].
(define (dither-quantize x max-val n-levels dither)
  (doc 'export #t)
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
(doc high-rate-bits-per-sample 'export #t)
(define high-rate-bits-per-sample gaussian-rate-distortion)

;;; high-rate-distortion : Real × Real → Real
;;; High-rate approximation: D ≈ sigma^2 * 2^(-2R)
;;; Same as Gaussian distortion-rate.
(doc high-rate-distortion 'export #t)
(define high-rate-distortion gaussian-distortion-rate)

;;; ====
;;; Rate-Distortion Summary
;;; ====

;;; rd-summary : (List Real) × (List Real) × Real → String
;;; Generate summary of rate-distortion performance.
(define (rd-summary original quantized peak-val)
  (doc 'export #t)
  (let* ([op-rd (operational-rate-distortion original quantized)]
         [rate (car op-rd)]
         [dist (cdr op-rd)]
         [var (variance original)]
         [gap (if (> var 0) (rd-gap rate dist var) 0)]
         [p (psnr original quantized peak-val)])
        (format "Rate: ~a bits/sample, MSE: ~a, PSNR: ~a dB, Gap: ~a bits"
                rate dist p gap)))
