;;; convolution.sexp — Development tasks for lattice/numeric/convolution.ss
;;;
;;; Module: convolution (tier 0, depends on prelude, complex, dft, vec)
;;; 401 lines, ~14 exported functions
;;; Convolution and correlation: direct time-domain, FFT-based,
;;; mode selection (full/same/valid), matched filtering, peak detection,
;;; normalization, circular convolution.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement vector-reverse
  ;; ──────────────────────────────────────────────
  ;; Simple utility needed for correlation-to-convolution conversion.

  (task
    (id "numeric/convolution/vector-reverse-impl")
    (module convolution)
    (tier 0)
    (type implement)
    (description "Implement vector-reverse: create a new vector with elements in reversed order. For a vector of length n, element i of the result should contain element (n-1-i) of the input. For example, (vector-reverse (vector 1 2 3 4)) returns (vector 4 3 2 1). An empty vector returns an empty vector. This utility is used to convert between convolution and correlation (correlation(f,g) = convolution(f, reverse(g))).")
    (context "Available: prelude (vector-length, vector-ref, vector-set!, make-vector, =, +, -). Pure function, no dependencies.")
    (before "(define (vector-reverse v)
  (doc 'export #t)
  (doc 'type '(-> (Vector a) (Vector a)))
  (doc 'description \"Reverse a vector\")
  ;; TODO: create new vector with reversed elements
  v)")
    (test-file "lattice/numeric/test-convolution.ss")
    (test-forms ("(assert-equal (vector 4 3 2 1) (vector-reverse (vector 1 2 3 4)))"
                 "(assert-equal (vector) (vector-reverse (vector)))"
                 "(assert-equal (vector 42) (vector-reverse (vector 42)))"
                 ";; Double reverse is identity
(let ([v (vector 1 2 3)])
  (assert-equal v (vector-reverse (vector-reverse v))))"))
    (available-modules (prelude))
    (hints ("Get n = (vector-length v)"
            "Create result = (make-vector n)"
            "Loop from i=0 to n: (vector-set! result i (vector-ref v (- n 1 i)))"
            "The expression (- n 1 i) maps 0→n-1, 1→n-2, etc."))
    (reference-solution "(define (vector-reverse v)
  (let* ([n (vector-length v)]
         [result (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
        (vector-set! result i (vector-ref v (- n 1 i))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement convolve-direct-valid
  ;; ──────────────────────────────────────────────
  ;; Valid-mode convolution — only complete overlaps, simplest bounds.

  (task
    (id "numeric/convolution/convolve-direct-valid-impl")
    (module convolution)
    (tier 0)
    (type implement)
    (description "Implement convolve-direct-valid: compute the 'valid' mode convolution of a signal with a kernel using the direct time-domain method. In valid mode, only positions where the kernel fully overlaps the signal are computed. Output length = max(0, N - M + 1) where N is signal length and M is kernel length. If M > N, return an empty vector. For each output position i (from 0 to output-len-1), compute: result[i] = sum(j=0 to M-1) of signal[i+j] * kernel[j]. This is the simplest convolution mode because no bounds checking is needed in the inner loop — the kernel always fits within the signal.")
    (context "Available: prelude (vector-length, vector-ref, vector-set!, make-vector, max, =, +, -, *). Uses nested do loops.")
    (before "(define (convolve-direct-valid signal kernel)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) (Vector Number) (Vector Number)))
  (doc 'description \"Valid-mode convolution: only complete overlaps\")
  ;; TODO: nested loop, output[i] = sum of signal[i+j]*kernel[j]
  (vector))")
    (test-file "lattice/numeric/test-convolution.ss")
    (test-forms (";; [1,2,3,4,5] * [1,1,1] = [6,9,12] (sliding sum of 3)
(assert-equal (vector 6 9 12)
              (convolve-direct-valid (vector 1 2 3 4 5) (vector 1 1 1)))"
                 ";; Identity kernel [1]: valid output equals original
(assert-equal (vector 1 2 3)
              (convolve-direct-valid (vector 1 2 3) (vector 1)))"
                 ";; Kernel longer than signal: empty result
(assert-equal (vector)
              (convolve-direct-valid (vector 1 2) (vector 1 1 1 1)))"
                 ";; Single elements: scalar multiplication
(assert-equal (vector 6)
              (convolve-direct-valid (vector 2) (vector 3)))"))
    (available-modules (prelude))
    (hints ("Compute output-len = (max 0 (+ (- n m) 1))"
            "If output-len is 0, return (vector)"
            "Outer loop: i from 0 to output-len"
            "Inner loop: j from 0 to m, accumulate signal[i+j] * kernel[j]"
            "No bounds checking needed — i+j is always valid in valid mode"
            "Use set! on a local sum variable, then vector-set! after inner loop"))
    (reference-solution "(define (convolve-direct-valid signal kernel)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)]
         [output-len (max 0 (+ (- n m) 1))])
    (if (= output-len 0)
        (vector)
        (let ([result (make-vector output-len 0)])
          (do ([i 0 (+ i 1)])
              ((= i output-len) result)
              (let ([sum 0])
                (do ([j 0 (+ j 1)])
                    ((= j m)
                     (vector-set! result i sum))
                    (set! sum (+ sum (* (vector-ref signal (+ i j))
                                        (vector-ref kernel j)))))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement convolve-direct-full
  ;; ──────────────────────────────────────────────
  ;; Full-mode convolution — all overlaps, requires bounds checking.

  (task
    (id "numeric/convolution/convolve-direct-full-impl")
    (module convolution)
    (tier 0)
    (type implement)
    (description "Implement convolve-direct-full: compute the full convolution of signal and kernel using the direct time-domain method. Output length is N + M - 1. The formula is: result[i] = sum over j from 0 to M-1 of signal[i-j] * kernel[j], where out-of-bounds signal values are treated as 0. For each output index i: iterate j from 0 to M-1, compute k = i - j. If k is in bounds (0 <= k < N), add signal[k] * kernel[j] to the sum. If either input is empty, return an empty vector. This is the most general form: it computes all partial overlaps.")
    (context "Available: prelude (vector-length, vector-ref, vector-set!, make-vector, or, =, +, -, *, >=, <, when). Nested do loops with bounds checking.")
    (before "(define (convolve-direct-full signal kernel)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) (Vector Number) (Vector Number)))
  (doc 'description \"Full convolution: all overlaps, output length N+M-1\")
  ;; TODO: nested loop with bounds checking on signal access
  (vector))")
    (test-file "lattice/numeric/test-convolution.ss")
    (test-forms (";; [1,2,3] * [1,1] = [1,3,5,3]
(assert-equal (vector 1 3 5 3)
              (convolve-direct-full (vector 1 2 3) (vector 1 1)))"
                 ";; Identity kernel: signal * [1] = signal
(assert-equal (vector 1 2 3)
              (convolve-direct-full (vector 1 2 3) (vector 1)))"
                 ";; Box filter: [1,1,1] * [1,1,1] = [1,2,3,2,1]
(assert-equal (vector 1 2 3 2 1)
              (convolve-direct-full (vector 1 1 1) (vector 1 1 1)))"
                 ";; Commutativity: f*g = g*f
(assert-equal (convolve-direct-full (vector 1 2 3) (vector 4 5))
              (convolve-direct-full (vector 4 5) (vector 1 2 3)))"
                 ";; Empty inputs
(assert-equal (vector) (convolve-direct-full (vector) (vector 1)))"))
    (available-modules (prelude))
    (hints ("Handle empty: (if (or (= n 0) (= m 0)) (vector) ...)"
            "Output length: (+ n m -1)"
            "Outer loop: i from 0 to output-len"
            "Inner loop: j from 0 to m, compute k = (- i j)"
            "Only add when k is in bounds: (when (and (>= k 0) (< k n)) ...)"
            "Accumulate: (set! sum (+ sum (* (vector-ref signal k) (vector-ref kernel j))))"
            "After inner loop: (vector-set! result i sum)"))
    (reference-solution "(define (convolve-direct-full signal kernel)
  (let* ([n (vector-length signal)]
         [m (vector-length kernel)])
    (if (or (= n 0) (= m 0))
        (vector)
        (let* ([output-len (+ n m -1)]
               [result (make-vector output-len 0)])
          (do ([i 0 (+ i 1)])
              ((= i output-len) result)
              (let ([sum 0])
                (do ([j 0 (+ j 1)])
                    ((= j m)
                     (vector-set! result i sum))
                    (let ([k (- i j)])
                      (when (and (>= k 0) (< k n))
                        (set! sum (+ sum (* (vector-ref signal k)
                                            (vector-ref kernel j)))))))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement find-peaks
  ;; ──────────────────────────────────────────────
  ;; Local maxima detection — practical signal analysis utility.

  (task
    (id "numeric/convolution/find-peaks-impl")
    (module convolution)
    (tier 0)
    (type implement)
    (description "Implement find-peaks: find indices of local maxima in a signal that exceed a threshold. A point at index i is a peak if: (1) signal[i] > threshold, (2) signal[i] >= signal[i-1], and (3) signal[i] >= signal[i+1]. Only interior points (indices 1 through n-2) can be peaks. If the signal has fewer than 3 elements, return an empty vector. Collect peak indices into a list (using cons, building in reverse), then convert to a vector with list->vector after reversing.")
    (context "Available: prelude (vector-length, vector-ref, <, =, +, -, >, >=, when, cons, reverse, list->vector, begin, set!). Imperative style with mutation.")
    (before "(define (find-peaks signal threshold)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) Number (Vector Integer)))
  (doc 'description \"Find local maxima above threshold\")
  ;; TODO: scan interior points, collect indices of local maxima
  (vector))")
    (test-file "lattice/numeric/test-convolution.ss")
    (test-forms (";; Three distinct peaks
(assert-equal (vector 1 3 5)
              (find-peaks (vector 0 1 0 2 0 3 0) 0.5))"
                 ";; No peaks below threshold
(assert-equal (vector)
              (find-peaks (vector 0.1 0.2 0.1 0.2) 0.5))"
                 ";; Two equal-height peaks
(assert-equal (vector 1 3)
              (find-peaks (vector 0 3 1 4 2) 0.5))"
                 ";; Too short for peaks
(assert-equal (vector) (find-peaks (vector 1 2) 0))"))
    (available-modules (prelude))
    (hints ("If (< n 3) return (vector)"
            "Initialize peaks = '()"
            "Loop from i=1 to (- n 1) — skip endpoints"
            "Get curr, prev, next from signal"
            "If (and (> curr threshold) (>= curr prev) (>= curr next)): (set! peaks (cons i peaks))"
            "After loop: (list->vector (reverse peaks))"))
    (reference-solution "(define (find-peaks signal threshold)
  (let ([n (vector-length signal)]
        [peaks '()])
    (if (< n 3)
        (vector)
        (begin
          (do ([i 1 (+ i 1)])
              ((= i (- n 1)) (list->vector (reverse peaks)))
              (let ([curr (vector-ref signal i)]
                    [prev (vector-ref signal (- i 1))]
                    [next (vector-ref signal (+ i 1))])
                (when (and (> curr threshold)
                           (>= curr prev)
                           (>= curr next))
                  (set! peaks (cons i peaks)))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement normalize-signal
  ;; ──────────────────────────────────────────────
  ;; Zero mean, unit variance — standard preprocessing for correlation.

  (task
    (id "numeric/convolution/normalize-signal-impl")
    (module convolution)
    (tier 0)
    (type implement)
    (description "Implement normalize-signal: transform a signal to have zero mean and unit variance. Three passes: (1) Compute mean = sum(signal[i]) / n. (2) Compute variance = sum((signal[i] - mean)^2) / n, then stddev = sqrt(variance). (3) Normalize: result[i] = (signal[i] - mean) / stddev. If the signal is constant (stddev < 1e-10), return a zero vector instead of dividing by zero. If the input is empty, return an empty vector. This preprocessing is essential for meaningful cross-correlation results.")
    (context "Available: prelude (vector-length, vector-ref, vector-set!, make-vector, =, +, -, *, /, <, sqrt, set!, begin, do, let*). Three-pass imperative algorithm.")
    (before "(define (normalize-signal signal)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) (Vector Number)))
  (doc 'description \"Normalize to zero mean and unit variance\")
  ;; TODO: compute mean, stddev, then normalize each element
  signal)")
    (test-file "lattice/numeric/test-convolution.ss")
    (test-forms (";; Normalized signal has zero mean
(let* ([norm (normalize-signal (vector 1 2 3 4 5))]
       [n (vector-length norm)]
       [sum (let loop ([i 0] [s 0])
              (if (= i n) s (loop (+ i 1) (+ s (vector-ref norm i)))))])
  (assert-true (< (abs (/ sum n)) 1e-10)))"
                 ";; Constant signal normalizes to zeros
(let ([norm (normalize-signal (vector 5 5 5))])
  (assert-true (< (abs (vector-ref norm 0)) 1e-10))
  (assert-true (< (abs (vector-ref norm 2)) 1e-10)))"
                 ";; Output has same length as input
(assert-equal 5 (vector-length (normalize-signal (vector 1 2 3 4 5))))"
                 ";; Empty input returns empty
(assert-equal (vector) (normalize-signal (vector)))"))
    (available-modules (prelude))
    (hints ("Pass 1 (mean): loop i=0 to n, accumulate sum, then mean = sum / n"
            "Pass 2 (variance): loop i=0 to n, accumulate (signal[i] - mean)^2, then variance = var-sum / n, stddev = (sqrt variance)"
            "Guard: if (< stddev 1e-10) return zero vector"
            "Pass 3 (normalize): loop i=0 to n, result[i] = (signal[i] - mean) / stddev"
            "Use set! and do loops for an imperative style"))
    (reference-solution "(define (normalize-signal signal)
  (let ([n (vector-length signal)])
    (if (= n 0)
        (vector)
        (let* ([sum 0] [mean 0]
               [var-sum 0] [variance 0] [stddev 0]
               [result (make-vector n)])
          (do ([i 0 (+ i 1)]) ((= i n))
            (set! sum (+ sum (vector-ref signal i))))
          (set! mean (/ sum n))
          (do ([i 0 (+ i 1)]) ((= i n))
            (let ([diff (- (vector-ref signal i) mean)])
              (set! var-sum (+ var-sum (* diff diff)))))
          (set! variance (/ var-sum n))
          (set! stddev (sqrt variance))
          (if (< stddev 1e-10)
              result
              (begin
                (do ([i 0 (+ i 1)]) ((= i n) result)
                  (vector-set! result i
                    (/ (- (vector-ref signal i) mean) stddev)))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

) ;; end tasks
