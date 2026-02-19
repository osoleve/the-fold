;;; wavelet.sexp — Development tasks for lattice/numeric/wavelet.ss
;;;
;;; Module: wavelet (tier 0, depends on prelude, vec)
;;; 447 lines, ~25 exported functions
;;; Wavelet transforms: Haar and Daubechies filter banks, DWT/IDWT,
;;; multi-resolution decomposition, QMF relationship, convolve-downsample,
;;; hard/soft thresholding for denoising, energy analysis.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement reverse-filter
  ;; ──────────────────────────────────────────────
  ;; Time-reverse a filter vector — used for synthesis.

  (task
    (id "numeric/wavelet/reverse-filter-impl")
    (module wavelet)
    (tier 0)
    (type implement)
    (description "Implement reverse-filter: time-reverse a filter vector. For a vector of length n, the reversed vector has element i equal to the original element at position n-1-i. This is needed for synthesis (reconstruction) filters in the wavelet transform. Create a new result vector of the same length and fill it by reading the input in reverse order. For example: #(1 2 3 4) → #(4 3 2 1).")
    (context "Available: vector-length, vector-ref, vector-set!, make-vector, -, do. Pure vector manipulation.")
    (before "(define (reverse-filter h)
  (doc 'export #t)
  ;; TODO: create reversed copy of vector h
  h)")
    (test-file "lattice/numeric/test-wavelet.ss")
    (test-forms (";; Basic reversal
(assert-equal #(4 3 2 1) (reverse-filter (vector 1 2 3 4)))"
                 ";; Single element is identity
(assert-equal #(5) (reverse-filter (vector 5)))"
                 ";; Length preserved
(assert-equal 3 (vector-length (reverse-filter (vector 1 2 3))))"
                 ";; Double reversal is identity
(let ([h (vector 1 2 3 4)])
  (assert-equal h (reverse-filter (reverse-filter h))))"))
    (available-modules (prelude))
    (hints ("Get n = (vector-length h)"
            "Allocate result = (make-vector n)"
            "For each i from 0 to n-1: (vector-set! result i (vector-ref h (- n 1 i)))"
            "Return result"))
    (reference-solution "(define (reverse-filter h)
  (let* ([n (vector-length h)]
         [result (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
        (vector-set! result i (vector-ref h (- n 1 i))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement qmf-wavelet-from-scaling
  ;; ──────────────────────────────────────────────
  ;; The QMF relationship: wavelet filter from scaling filter.

  (task
    (id "numeric/wavelet/qmf-impl")
    (module wavelet)
    (tier 0)
    (type implement)
    (description "Implement qmf-wavelet-from-scaling: derive the wavelet (high-pass) filter from the scaling (low-pass) filter using the Quadrature Mirror Filter (QMF) relationship. The formula is: g[i] = (-1)^i * h[N-1-i], where h is the scaling filter and N is its length. This alternates signs while reversing the filter. The QMF relationship ensures the scaling and wavelet filters are orthogonal and together form a perfect reconstruction filter bank. For the Haar wavelet: h = [1/sqrt(2), 1/sqrt(2)] gives g = [1/sqrt(2), -1/sqrt(2)].")
    (context "Available: vector-length, vector-ref, vector-set!, make-vector, even?, *, -, do. Pure vector computation.")
    (before "(define (qmf-wavelet-from-scaling h)
  (doc 'export #t)
  ;; TODO: g[i] = (-1)^i * h[N-1-i]
  h)")
    (test-file "lattice/numeric/test-wavelet.ss")
    (test-forms (";; Haar: h=[s,s] → g=[s,-s] where s=1/sqrt(2)
(let* ([h (haar-scaling-filter)]
       [g (qmf-wavelet-from-scaling h)]
       [s (/ 1 (sqrt 2))])
  (assert-true (< (abs (- (vector-ref g 0) s)) 1e-10))
  (assert-true (< (abs (- (vector-ref g 1) (- s))) 1e-10)))"
                 ";; Length preserved
(assert-equal 4 (vector-length (qmf-wavelet-from-scaling (vector 1 2 3 4))))"
                 ";; QMF of QMF reverses and flips — not identity, but predictable
(let* ([h (vector 1 2 3 4)]
       [g (qmf-wavelet-from-scaling h)])
  ;; g = [4, -3, 2, -1]
  (assert-equal 4 (vector-ref g 0))
  (assert-equal -3 (vector-ref g 1))
  (assert-equal 2 (vector-ref g 2))
  (assert-equal -1 (vector-ref g 3)))"))
    (available-modules (prelude))
    (hints ("Get n = (vector-length h)"
            "For each i: compute sign = (if (even? i) 1 -1)"
            "And reversed index j = (- n 1 i)"
            "Set: (vector-set! g i (* sign (vector-ref h j)))"))
    (reference-solution "(define (qmf-wavelet-from-scaling h)
  (let* ([n (vector-length h)]
         [g (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n) g)
        (let* ([sign (if (even? i) 1 -1)]
               [j (- n 1 i)])
          (vector-set! g i (* sign (vector-ref h j)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement soft-threshold
  ;; ──────────────────────────────────────────────
  ;; Wavelet denoising primitive — shrink toward zero.

  (task
    (id "numeric/wavelet/soft-threshold-impl")
    (module wavelet)
    (tier 0)
    (type implement)
    (description "Implement soft-threshold: the soft thresholding function used in wavelet denoising. Given a value x and a threshold t: if x > t, return x - t (shrink toward zero from above); if x < -t, return x + t (shrink toward zero from below); otherwise return 0 (kill small coefficients). Soft thresholding produces smoother results than hard thresholding because it shrinks all coefficients rather than creating discontinuities at the threshold boundary. This is the proximal operator for L1 regularization.")
    (context "Available: >, <, -, +, cond. Pure arithmetic with three cases.")
    (before "(define (soft-threshold x threshold)
  (doc 'export #t)
  ;; TODO: shrink x toward zero by threshold amount
  x)")
    (test-file "lattice/numeric/test-wavelet.ss")
    (test-forms (";; Above threshold: shrink
(assert-equal 2 (soft-threshold 5 3))"
                 ";; Below negative threshold: shrink
(assert-equal -2 (soft-threshold -5 3))"
                 ";; Within threshold: zero
(assert-equal 0 (soft-threshold 2 3))"
                 ";; Exactly at threshold: zero
(assert-equal 0 (soft-threshold 3 3))"
                 ";; Zero input: zero
(assert-equal 0 (soft-threshold 0 3))"))
    (available-modules (prelude))
    (hints ("Three cases with cond:"
            "x > threshold → (- x threshold)"
            "x < (- threshold) → (+ x threshold)"
            "else → 0"
            "This is the proximal operator for the L1 norm"))
    (reference-solution "(define (soft-threshold x threshold)
  (cond
   [(> x threshold) (- x threshold)]
   [(< x (- threshold)) (+ x threshold)]
   [else 0]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement wavelet-energy
  ;; ──────────────────────────────────────────────
  ;; Sum of squares — Parseval's theorem says this is preserved.

  (task
    (id "numeric/wavelet/wavelet-energy-impl")
    (module wavelet)
    (tier 0)
    (type implement)
    (description "Implement wavelet-energy: compute the energy (sum of squares) of a vector of wavelet coefficients. E = sum(c_i^2) for all coefficients c_i. By Parseval's theorem, the total energy is preserved by the wavelet transform — the sum of energies across all decomposition levels equals the energy of the original signal. This makes it possible to analyze how energy is distributed across scales. Use a named let loop with an accumulator.")
    (context "Available: vector-length, vector-ref, +, *, =. Pure arithmetic over a vector.")
    (before "(define (wavelet-energy coeffs)
  (doc 'export #t)
  ;; TODO: sum of c_i^2 over all elements
  0)")
    (test-file "lattice/numeric/test-wavelet.ss")
    (test-forms (";; Simple known values
(assert-equal 14 (wavelet-energy (vector 1 2 3)))"
                 ";; Zero vector: energy = 0
(assert-equal 0 (wavelet-energy (vector 0 0 0)))"
                 ";; Floating point
(assert-true (< (abs (- (wavelet-energy (vector 3.0 4.0)) 25.0)) 0.001))"
                 ";; Single element
(assert-equal 9 (wavelet-energy (vector 3)))"))
    (available-modules (prelude))
    (hints ("Get n = (vector-length coeffs)"
            "Named let with i=0 and energy=0"
            "Each step: c = (vector-ref coeffs i), add (* c c) to energy"
            "Return energy when i = n"))
    (reference-solution "(define (wavelet-energy coeffs)
  (let ([n (vector-length coeffs)])
    (let loop ([i 0] [energy 0])
      (if (= i n)
          energy
          (let ([c (vector-ref coeffs i)])
            (loop (+ i 1) (+ energy (* c c))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement convolve-downsample
  ;; ──────────────────────────────────────────────
  ;; Filter + downsample — the core DWT building block.

  (task
    (id "numeric/wavelet/convolve-downsample-impl")
    (module wavelet)
    (tier 0)
    (type implement)
    (description "Implement convolve-downsample: convolve a signal with a filter and downsample by a given factor. This is the core operation for wavelet decomposition. For each output sample i: compute the convolution at position pos = i * factor, which is y[i] = sum over k of filter[k] * signal[pos+k] for valid indices. Output length = ceil(n / factor) where n is the signal length. Skip signal indices that are out of bounds (treat as zero-padding). This combines the low-pass/high-pass filtering and decimation steps of the DWT into one efficient pass.")
    (context "Available: vector-length, vector-ref, vector-set!, make-vector, ceiling, /, *, +, -, =, >=, <, and. Nested loops: outer over output samples, inner over filter taps.")
    (before "(define (convolve-downsample signal filter factor)
  (doc 'export #t)
  ;; TODO: for each output i, compute convolution at pos=i*factor, downsample
  (make-vector 0))")
    (test-file "lattice/numeric/test-wavelet.ss")
    (test-forms (";; Haar low-pass on constant signal preserves value (scaled)
(let* ([sig (vector 1.0 1.0 1.0 1.0)]
       [h (haar-scaling-filter)]
       [result (convolve-downsample sig h 2)])
  (assert-equal 2 (vector-length result)))"
                 ";; Output length = ceil(n/factor)
(assert-equal 3 (vector-length (convolve-downsample (vector 1 2 3 4 5) (vector 1) 2)))"
                 ";; Identity filter (length 1, value 1) with factor 1 = copy
(let* ([sig (vector 3 7 2)]
       [result (convolve-downsample sig (vector 1) 1)])
  (assert-equal 3 (vector-ref result 0))
  (assert-equal 7 (vector-ref result 1))
  (assert-equal 2 (vector-ref result 2)))"))
    (available-modules (prelude vec))
    (hints ("Compute output length: (ceiling (/ n factor))"
            "Outer loop: for each output index i from 0 to out-len"
            "Compute pos = (* i factor)"
            "Inner loop: for each filter tap k from 0 to m (filter length)"
            "  sig-idx = (+ pos k)"
            "  If valid (>= 0 and < n): accumulate filter[k] * signal[sig-idx]"
            "Store accumulated sum in result[i]"))
    (reference-solution "(define (convolve-downsample signal filter factor)
  (let* ([n (vector-length signal)]
         [m (vector-length filter)]
         [out-len (ceiling (/ n factor))]
         [result (make-vector out-len)])
    (do ([i 0 (+ i 1)])
        ((= i out-len) result)
        (let* ([pos (* i factor)]
               [sum (let loop ([k 0] [acc 0])
                      (if (= k m)
                          acc
                          (let ([sig-idx (+ pos k)])
                            (if (and (>= sig-idx 0) (< sig-idx n))
                                (loop (+ k 1)
                                      (+ acc (* (vector-ref filter k)
                                                (vector-ref signal sig-idx))))
                                (loop (+ k 1) acc)))))])
          (vector-set! result i sum)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

) ;; end tasks
