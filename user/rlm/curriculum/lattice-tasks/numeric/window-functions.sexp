;;; window-functions.sexp — Development tasks for lattice/numeric/window-functions.ss
;;;
;;; Module: window-functions (tier 0, depends on prelude)
;;; 267 lines, ~12 exported functions
;;; Window functions for signal processing: rectangular, Hann, Hamming,
;;; Blackman, Bartlett, Kaiser. Includes Bessel I0 approximation,
;;; window application, and energy/power computation.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement bartlett-window
  ;; ──────────────────────────────────────────────
  ;; Triangular window — linear taper to zero at endpoints.

  (task
    (id "numeric/window-functions/bartlett-impl")
    (module window-functions)
    (tier 0)
    (type implement)
    (description "Implement bartlett-window: create a Bartlett (triangular) window of length n. The Bartlett window tapers linearly from 0 at the endpoints to 1 at the center. The formula is: w[i] = 1 - |i - mid| / mid, where mid = (n-1)/2.0. Create a vector of length n and fill each element using this formula. Use abs for absolute value. The division by 2.0 (not 2) ensures floating-point arithmetic. For example, bartlett-window(5) gives [0.0, 0.5, 1.0, 0.5, 0.0].")
    (context "Available: prelude (make-vector, vector-set!, =, +, -, /, abs). Use a do loop from i=0 to n. The constant mid = (/ (- n 1) 2.0) is computed once.")
    (before "(define (bartlett-window n)
  (doc 'export #t)
  (doc 'type '(-> Integer (Vector Number)))
  (doc 'description \"Bartlett (triangular) window\")
  ;; TODO: fill vector with 1 - |i - mid| / mid
  (make-vector n 0))")
    (test-file "lattice/numeric/test-window-functions.ss")
    (test-forms (";; Endpoints are zero
(let ([w (bartlett-window 11)])
  (assert-true (< (abs (vector-ref w 0)) 1e-10))
  (assert-true (< (abs (vector-ref w 10)) 1e-10)))"
                 ";; Peak at center is 1.0
(let ([w (bartlett-window 11)])
  (assert-true (< (abs (- (vector-ref w 5) 1.0)) 1e-10)))"
                 ";; Correct length
(assert-equal 11 (vector-length (bartlett-window 11)))"
                 ";; Symmetry: w[i] = w[n-1-i]
(let ([w (bartlett-window 11)])
  (assert-true (< (abs (- (vector-ref w 2) (vector-ref w 8))) 1e-10)))"
                 ";; Linear taper: w[1] = 0.2 for n=11
(let ([w (bartlett-window 11)])
  (assert-true (< (abs (- (vector-ref w 1) 0.2)) 1e-10)))"))
    (available-modules (prelude))
    (hints ("Compute mid = (/ (- n 1) 2.0) once before the loop"
            "Store n-1 = (- n 1) to avoid recomputing"
            "For each i: (vector-set! result i (- 1 (/ (abs (- i mid)) mid)))"
            "Use a do loop: (do ([i 0 (+ i 1)]) ((= i n) result) ...)"
            "The abs function handles both halves of the triangle"))
    (reference-solution "(define (bartlett-window n)
  (let ([result (make-vector n)]
        [n-1 (- n 1)]
        [mid (/ (- n 1) 2.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
        (vector-set! result i (- 1 (/ (abs (- i mid)) mid))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement hann-window
  ;; ──────────────────────────────────────────────
  ;; The classic raised-cosine window for spectral analysis.

  (task
    (id "numeric/window-functions/hann-impl")
    (module window-functions)
    (tier 0)
    (type implement)
    (description "Implement hann-window: create a Hann (Hanning) window of length n. The Hann window is a raised cosine that smoothly tapers to zero at both endpoints. Formula: w[i] = 0.5 * (1 - cos(2*pi*i/(n-1))). The window has excellent frequency resolution with side lobes at -31 dB. Compute two-pi = 2 * pi once (use (pi-value) to get pi). For each element i, compute term = two-pi * i / (n-1), then set w[i] = 0.5 * (1 - cos(term)). The endpoints w[0] and w[n-1] will be exactly 0.")
    (context "Available: prelude (make-vector, vector-set!, =, +, -, *, /), cos (cosine function), (pi-value) (returns pi ≈ 3.14159...). Use a do loop.")
    (before "(define (hann-window n)
  (doc 'export #t)
  (doc 'type '(-> Integer (Vector Number)))
  (doc 'description \"Hann (Hanning) window\")
  ;; TODO: w[i] = 0.5 * (1 - cos(2*pi*i/(n-1)))
  (make-vector n 0))")
    (test-file "lattice/numeric/test-window-functions.ss")
    (test-forms (";; Endpoints are zero
(let ([w (hann-window 10)])
  (assert-true (< (abs (vector-ref w 0)) 1e-10))
  (assert-true (< (abs (vector-ref w 9)) 1e-10)))"
                 ";; Symmetry
(let ([w (hann-window 10)])
  (assert-true (< (abs (- (vector-ref w 1) (vector-ref w 8))) 1e-10))
  (assert-true (< (abs (- (vector-ref w 2) (vector-ref w 7))) 1e-10)))"
                 ";; Correct length
(assert-equal 10 (vector-length (hann-window 10)))"
                 ";; Peak near center (not exactly 1.0 for even n)
(let ([w (hann-window 10)])
  (assert-true (> (vector-ref w 5) 0.9)))"))
    (available-modules (prelude))
    (hints ("Compute two-pi = (* 2 (pi-value)) once"
            "Compute n-1 = (- n 1) once"
            "For each i: term = (/ (* two-pi i) n-1)"
            "Set: (vector-set! result i (* 0.5 (- 1 (cos term))))"
            "cos(0) = 1 → w[0] = 0.5*(1-1) = 0"))
    (reference-solution "(define (hann-window n)
  (let ([result (make-vector n)]
        [two-pi (* 2 (pi-value))]
        [n-1 (- n 1)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
        (let ([term (/ (* two-pi i) n-1)])
          (vector-set! result i (* 0.5 (- 1 (cos term))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement window-energy
  ;; ──────────────────────────────────────────────
  ;; Sum of squares — used for normalizing spectral estimates.

  (task
    (id "numeric/window-functions/window-energy-impl")
    (module window-functions)
    (tier 0)
    (type implement)
    (description "Implement window-energy: compute the energy of a window function, defined as the sum of the squares of all elements. E = sum(w[i]^2) for i from 0 to n-1. This is used to normalize spectral estimates (like power spectral density) so that different window functions produce comparable results. Use a named let loop with an accumulator, iterating through the vector and adding w[i]*w[i] at each step.")
    (context "Available: prelude (vector-length, vector-ref, =, +, *). No dependencies beyond prelude.")
    (before "(define (window-energy window)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) Number))
  (doc 'description \"Compute window energy: sum of squares\")
  ;; TODO: sum w[i]^2 over all elements
  0)")
    (test-file "lattice/numeric/test-window-functions.ss")
    (test-forms (";; Rectangular window of length 10: all 1s, energy = 10
(assert-true (< (abs (- (window-energy (rectangular-window 10)) 10.0)) 1e-10))"
                 ";; Single element of value 3: energy = 9
(assert-true (< (abs (- (window-energy (vector 3.0)) 9.0)) 1e-10))"
                 ";; Empty window: energy = 0
(assert-true (< (abs (window-energy (vector))) 1e-10))"
                 ";; Hann energy is less than rectangular energy (same length)
(assert-true (< (window-energy (hann-window 100))
                (window-energy (rectangular-window 100))))"))
    (available-modules (prelude))
    (hints ("Get n = (vector-length window)"
            "Named let with i=0 and sum=0"
            "Base case: (= i n) → return sum"
            "Step: get w = (vector-ref window i), add (* w w) to sum"
            "Recurse with (+ i 1) and (+ sum (* w w))"))
    (reference-solution "(define (window-energy window)
  (let ([n (vector-length window)])
    (let loop ([i 0] [sum 0])
      (if (= i n)
          sum
          (let ([w (vector-ref window i)])
            (loop (+ i 1) (+ sum (* w w))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement apply-window
  ;; ──────────────────────────────────────────────
  ;; Element-wise multiplication of signal and window.

  (task
    (id "numeric/window-functions/apply-window-impl")
    (module window-functions)
    (tier 0)
    (type implement)
    (description "Implement apply-window: multiply a signal vector element-wise by a window vector. For each index i, result[i] = signal[i] * window[i]. The signal and window must have the same length; if they don't, raise an error. Create a new result vector of the same length, fill it with the element-wise products, and return it. This is how window functions are applied in practice: the signal is multiplied point-by-point by the window before Fourier analysis.")
    (context "Available: prelude (vector-length, vector-ref, vector-set!, make-vector, =, +, *, not, error, list). Use error for the length mismatch case.")
    (before "(define (apply-window signal window)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) (Vector Number) (Vector Number)))
  (doc 'description \"Apply window to signal by element-wise multiplication\")
  ;; TODO: check lengths match, then multiply element-wise
  signal)")
    (test-file "lattice/numeric/test-window-functions.ss")
    (test-forms (";; Rectangular window is identity
(let* ([sig (vector 1 2 3 4 5)]
       [win (rectangular-window 5)]
       [result (apply-window sig win)])
  (assert-true (< (abs (- (vector-ref result 0) 1.0)) 1e-10))
  (assert-true (< (abs (- (vector-ref result 2) 3.0)) 1e-10))
  (assert-true (< (abs (- (vector-ref result 4) 5.0)) 1e-10)))"
                 ";; Hann window zeros out endpoints
(let* ([sig (vector 1 2 3 4 1)]
       [win (hann-window 5)]
       [result (apply-window sig win)])
  (assert-true (< (abs (vector-ref result 0)) 1e-10))
  (assert-true (< (abs (vector-ref result 4)) 1e-10)))"
                 ";; Result length matches input
(assert-equal 5 (vector-length (apply-window (vector 1 2 3 4 5) (rectangular-window 5))))"
                 ";; Length mismatch raises error
(assert-error (apply-window (vector 1 2 3) (rectangular-window 5)))"))
    (available-modules (prelude))
    (hints ("Get n = (vector-length signal), m = (vector-length window)"
            "If (not (= n m)), call (error 'apply-window \"signal and window must have same length\" ...)"
            "Otherwise: make result vector, do loop from 0 to n"
            "At each step: (vector-set! result i (* (vector-ref signal i) (vector-ref window i)))"
            "Return result when i = n"))
    (reference-solution "(define (apply-window signal window)
  (let* ([n (vector-length signal)]
         [m (vector-length window)])
    (if (not (= n m))
        (error 'apply-window \"signal and window must have same length\"
               (list 'signal-length n 'window-length m))
        (let ([result (make-vector n)])
          (do ([i 0 (+ i 1)])
              ((= i n) result)
              (vector-set! result i
                           (* (vector-ref signal i)
                              (vector-ref window i))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement bessel-i0
  ;; ──────────────────────────────────────────────
  ;; Modified Bessel function via series expansion — needed for Kaiser window.

  (task
    (id "numeric/window-functions/bessel-i0-impl")
    (module window-functions)
    (tier 0)
    (type implement)
    (description "Implement bessel-i0: compute the modified Bessel function of the first kind, order 0, using a series expansion. This is needed for the Kaiser window. The series is: I0(x) = sum(k=0 to infinity) of [(x/2)^k / k!]^2. Rather than computing the power and factorial separately, use a running term: start with term=1.0, sum=1.0. At each step k, multiply term by (x/2)^2 / (k+1)^2, giving the next squared ratio. Add the new term to sum. Stop when the term becomes smaller than 1e-10 (convergence) or k exceeds 100 (safety limit). Return the final sum.")
    (context "Available: prelude (>, <, +, *, /). Pure arithmetic with a convergence loop. The key trick is computing each term from the previous one without ever computing factorials or powers directly.")
    (before "(define (bessel-i0 x)
  (doc 'export #t)
  (doc 'type '(-> Number Number))
  (doc 'description \"Modified Bessel function I0 via series expansion\")
  ;; TODO: series sum with running term, converge to 1e-10
  1.0)")
    (test-file "lattice/numeric/test-window-functions.ss")
    (test-forms (";; I0(0) = 1 exactly
(assert-true (< (abs (- (bessel-i0 0) 1.0)) 1e-10))"
                 ";; I0(1) ≈ 1.2661
(assert-true (< (abs (- (bessel-i0 1.0) 1.2661)) 0.001))"
                 ";; I0(2) ≈ 2.2796
(assert-true (< (abs (- (bessel-i0 2.0) 2.2796)) 0.001))"
                 ";; Monotonically increasing: I0(3) > I0(2)
(assert-true (> (bessel-i0 3.0) (bessel-i0 2.0)))"))
    (available-modules (prelude))
    (hints ("Compute half-x = (/ x 2) once"
            "Named let with k=0, term=1.0, sum=1.0"
            "Safety limit: if (> k 100) return sum"
            "Compute new-term = (* term (/ (* half-x half-x) (* (+ k 1) (+ k 1))))"
            "This is term * (x/2)^2 / (k+1)^2 — the ratio of consecutive squared terms"
            "If (< new-term 1e-10) return sum (converged)"
            "Otherwise loop with k+1, new-term, (+ sum new-term)"))
    (reference-solution "(define (bessel-i0 x)
  (let ([half-x (/ x 2)])
    (let loop ([k 0]
               [term 1.0]
               [sum 1.0])
      (if (> k 100)
          sum
          (let* ([k+1 (+ k 1)]
                 [new-term (* term (/ (* half-x half-x)
                                      (* k+1 k+1)))])
            (if (< new-term 1e-10)
                sum
                (loop k+1 new-term (+ sum new-term))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

) ;; end tasks
