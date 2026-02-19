;;; rate-distortion.sexp — Development tasks for lattice/info/rate-distortion.ss
;;;
;;; Module: rate-distortion (tier 0, depends on entropy, sort)
;;; 447 lines, ~30 exported functions
;;; Rate-distortion theory: compression vs fidelity tradeoff.
;;; Distortion measures (MSE, MAE, Hamming), Gaussian/binary R(D) functions,
;;; uniform and Lloyd-Max scalar quantization, vector quantization,
;;; entropy-coded quantization, operational R-D analysis.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   2696481c feat(module): migrate info/, automata/, number-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement mse
  ;; ──────────────────────────────────────────────
  ;; Mean squared error — the universal distortion metric.

  (task
    (id "info/rate-distortion/mse-impl")
    (module rate-distortion)
    (tier 0)
    (type implement)
    (description "Implement mse: compute the mean squared error between two lists of values. MSE = (1/n) * sum((xi - yi)^2) for corresponding elements xi, yi. Map squared-error over the paired elements, sum with fold-left, divide by length. Handle empty lists by returning 0. MSE is the most common distortion measure in rate-distortion theory — it corresponds to the squared error distortion function averaged over the source.")
    (context "Available: squared-error (computes (x-y)^2), fold-left, map, length, null?, +, /. One-pass computation using map + fold.")
    (before "(define (mse xs ys)
  (doc 'export #t)
  ;; TODO: mean of squared errors
  0)")
    (test-file "lattice/info/test-rate-distortion.ss")
    (test-forms (";; Known MSE
(assert-true (< (abs (- (mse '(1 2 3) '(1 2 4)) 1/3)) 0.001))"
                 ";; Zero distortion: identical lists
(assert-equal 0 (mse '(5 5 5) '(5 5 5)))"
                 ";; Symmetric: mse(a,b) = mse(b,a)
(assert-true (< (abs (- (mse '(1 2) '(3 4)) (mse '(3 4) '(1 2)))) 0.001))"
                 ";; Empty list
(assert-equal 0 (mse '() '()))"))
    (available-modules (prelude entropy sort))
    (hints ("Guard: (null? xs) → 0"
            "Compute squared errors: (map squared-error xs ys)"
            "Sum: (fold-left + 0 ...)"
            "Divide by (length xs)"))
    (reference-solution "(define (mse xs ys)
  (if (null? xs)
      0
      (/ (fold-left + 0 (map squared-error xs ys))
         (length xs))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement gaussian-rate-distortion
  ;; ──────────────────────────────────────────────
  ;; Shannon's R(D) for Gaussian sources.

  (task
    (id "info/rate-distortion/gaussian-rd-impl")
    (module rate-distortion)
    (tier 0)
    (type implement)
    (description "Implement gaussian-rate-distortion: compute the rate-distortion function R(D) for a Gaussian source with given variance. Shannon's formula: R(D) = (1/2) * log2(variance / distortion) when 0 < D < variance. Edge cases: variance <= 0 → 0 (degenerate source), distortion <= 0 → +inf.0 (perfect reproduction needs infinite rate), distortion >= variance → 0 (enough distortion to use zero bits). This is the fundamental limit: no coding scheme can achieve lower rate at the same distortion.")
    (context "Available: <=, >=, *, /, log2, cond, +inf.0. Three-case function with one formula.")
    (before "(define (gaussian-rate-distortion variance distortion)
  (doc 'export #t)
  ;; TODO: R(D) = 0.5 * log2(sigma^2 / D)
  0)")
    (test-file "lattice/info/test-rate-distortion.ss")
    (test-forms (";; Standard case: R(D) for variance=4, D=1 → 1.0 bit
(assert-true (< (abs (- (gaussian-rate-distortion 4.0 1.0) 1.0)) 0.001))"
                 ";; D = variance → R = 0
(assert-equal 0 (gaussian-rate-distortion 2.0 2.0))"
                 ";; D > variance → R = 0
(assert-equal 0 (gaussian-rate-distortion 2.0 3.0))"
                 ";; D = 0 → R = +inf
(assert-equal +inf.0 (gaussian-rate-distortion 2.0 0))"))
    (available-modules (prelude entropy sort))
    (hints ("Four cases via cond:"
            "variance <= 0 → 0"
            "distortion <= 0 → +inf.0"
            "distortion >= variance → 0"
            "else → (* 0.5 (log2 (/ variance distortion)))"))
    (reference-solution "(define (gaussian-rate-distortion variance distortion)
  (cond
   [(<= variance 0) 0]
   [(<= distortion 0) +inf.0]
   [(>= distortion variance) 0]
   [else (* 0.5 (log2 (/ variance distortion)))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement compute-boundaries
  ;; ──────────────────────────────────────────────
  ;; Decision boundaries for Lloyd-Max quantizer.

  (task
    (id "info/rate-distortion/compute-boundaries-impl")
    (module rate-distortion)
    (tier 0)
    (type implement)
    (description "Implement compute-boundaries: compute the decision boundaries between quantization levels. Given a sorted list of reconstruction levels, the optimal boundary between adjacent levels is their midpoint. For levels [L1, L2, L3, ...], boundaries are [(L1+L2)/2, (L2+L3)/2, ...]. The result has one fewer element than the input. This is the nearest-neighbor condition in the Lloyd-Max algorithm: each sample is assigned to the closest level. Base cases: empty or single-element list returns empty.")
    (context "Available: null?, cdr, car, cadr, cons, +, /. Simple pairwise recursion.")
    (before "(define (compute-boundaries levels)
  (doc 'export #t)
  ;; TODO: midpoints between adjacent levels
  '())")
    (test-file "lattice/info/test-rate-distortion.ss")
    (test-forms (";; Standard midpoints
(assert-equal '(2 4 6) (compute-boundaries '(1 3 5 7)))"
                 ";; Two levels: one boundary
(assert-equal '(5) (compute-boundaries '(0 10)))"
                 ";; Single level: no boundaries
(assert-equal '() (compute-boundaries '(5)))"
                 ";; Empty: no boundaries
(assert-equal '() (compute-boundaries '()))"))
    (available-modules (prelude entropy sort))
    (hints ("Base case: empty or single-element → '()"
            "Recursive: midpoint = (/ (+ (car levels) (cadr levels)) 2)"
            "Cons midpoint with (compute-boundaries (cdr levels))"))
    (reference-solution "(define (compute-boundaries levels)
  (if (or (null? levels) (null? (cdr levels)))
      '()
      (cons (/ (+ (car levels) (cadr levels)) 2)
            (compute-boundaries (cdr levels)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement count-values
  ;; ──────────────────────────────────────────────
  ;; Frequency table — needed for entropy-coded quantization.

  (task
    (id "info/rate-distortion/count-values-impl")
    (module rate-distortion)
    (tier 0)
    (type implement)
    (description "Implement count-values: build a frequency table counting occurrences of each value in a list. Returns an association list of (value . count) pairs. For each element: if it's already in the counts list, increment its count; otherwise add it with count 1. Use equal? for comparison. For example: (a b a c b a) → ((c . 1) (b . 2) (a . 3)). Order of entries doesn't matter. This frequency table is used to compute the entropy of a quantizer's output distribution.")
    (context "Available: null?, car, cdr, equal?, cons, +, map, lambda. Accumulator pattern with association list lookup.")
    (before "(define (count-values lst)
  (doc 'export #t)
  ;; TODO: build (value . count) frequency table
  '())")
    (test-file "lattice/info/test-rate-distortion.ss")
    (test-forms (";; Basic counting
(let ([counts (count-values '(a b a c b a))])
  (assert-equal 3 (cdr (assoc 'a counts equal?)))
  (assert-equal 2 (cdr (assoc 'b counts equal?)))
  (assert-equal 1 (cdr (assoc 'c counts equal?))))"
                 ";; Single element
(let ([counts (count-values '(x))])
  (assert-equal 1 (cdr (assoc 'x counts equal?))))"
                 ";; Empty list
(assert-equal '() (count-values '()))"
                 ";; All same
(let ([counts (count-values '(a a a))])
  (assert-equal 3 (cdr (assoc 'a counts equal?))))"))
    (available-modules (prelude entropy sort))
    (hints ("Named let with remaining list and counts accumulator"
            "For each value: search counts for existing entry"
            "Use a helper: scan counts for (equal? (car pair) val)"
            "If found: map over counts, incrementing the matching pair"
            "If not found: cons (val . 1) to counts"))
    (reference-solution "(define (count-values lst)
  (let loop ([remaining lst] [counts '()])
    (if (null? remaining)
        counts
        (let* ([val (car remaining)]
               [existing (let find ([cs counts])
                           (cond [(null? cs) #f]
                                 [(equal? (caar cs) val) (car cs)]
                                 [else (find (cdr cs))]))])
          (if existing
              (loop (cdr remaining)
                    (map (lambda (p)
                           (if (equal? (car p) val)
                               (cons val (+ (cdr p) 1))
                               p))
                         counts))
              (loop (cdr remaining)
                    (cons (cons val 1) counts)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement vq-find-nearest
  ;; ──────────────────────────────────────────────
  ;; Nearest codebook vector — core of vector quantization.

  (task
    (id "info/rate-distortion/vq-find-nearest-impl")
    (module rate-distortion)
    (tier 0)
    (type implement)
    (description "Implement vq-find-nearest: find the index of the nearest codebook vector to a given input vector. Iterate through the codebook computing the Euclidean distance from the input to each codebook entry using vq-codebook-distance. Track the best (minimum distance) index. Return the index (0-based) of the nearest codebook entry. Use a named let with four variables: remaining codebook entries, current index, best index, best distance (initialized to +inf.0).")
    (context "Available: vq-codebook-distance (Euclidean distance between two lists), null?, car, cdr, <, +, +inf.0. Linear scan with best-so-far tracking.")
    (before "(define (vq-find-nearest vector codebook)
  (doc 'export #t)
  ;; TODO: find index of nearest codebook entry
  0)")
    (test-file "lattice/info/test-rate-distortion.ss")
    (test-forms (";; Exact match at index 1
(assert-equal 1 (vq-find-nearest '(3 4) '((0 0) (3 4) (6 8))))"
                 ";; Nearest to first entry
(assert-equal 0 (vq-find-nearest '(0.1 0.1) '((0 0) (10 10))))"
                 ";; Single codebook entry
(assert-equal 0 (vq-find-nearest '(5 5) '((1 1))))"))
    (available-modules (prelude entropy sort))
    (hints ("Named let with: cb=codebook, i=0, best-i=0, best-dist=+inf.0"
            "Base: (null? cb) → best-i"
            "Compute: dist = (vq-codebook-distance vector (car cb))"
            "If dist < best-dist: update best-i=i, best-dist=dist"
            "Recurse with (cdr cb), (+ i 1)"))
    (reference-solution "(define (vq-find-nearest vector codebook)
  (let loop ([cb codebook] [i 0] [best-i 0] [best-dist +inf.0])
    (if (null? cb)
        best-i
        (let ([dist (vq-codebook-distance vector (car cb))])
          (if (< dist best-dist)
              (loop (cdr cb) (+ i 1) i dist)
              (loop (cdr cb) (+ i 1) best-i best-dist))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
