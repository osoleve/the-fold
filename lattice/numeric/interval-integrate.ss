;;; lattice/numeric/interval-integrate.ss — Verified Interval Integration
;;;
;;; Rigorous numerical integration with guaranteed enclosure.
;;; Every result is an interval that is guaranteed to contain
;;; the true value of the definite integral.
;;;
;;; Algorithms:
;;;   - Natural interval extension (naive uniform subdivision)
;;;   - Bisected (2x evaluations per subinterval, tighter bounds)
;;;   - Adaptive refinement (refine where enclosure is widest)
;;;   - Rigorous (directed rounding for formal guarantees)
;;;
;;; All methods return intervals [lo, hi] such that:
;;;   lo <= ∫_a^b f(x) dx <= hi

(load "core/base/prelude.ss")
(load "lattice/numeric/interval.ss")
(load "lattice/data/heap.ss")

(doc 'module 'interval-integrate)
(doc 'description "Verified numerical integration with rigorous enclosure bounds.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Natural Interval Extension Integration
;;; ============================================================================
;;;
;;; The simplest verified integration method. Subdivide [a,b] into n equal
;;; subintervals, evaluate f on each using interval arithmetic, and sum.
;;; The result is a rigorous enclosure of the true integral.
;;;
;;; Convergence: O(1/n) for Lipschitz functions — the enclosure width
;;; decreases linearly with subdivision count.

(define (interval-integrate-naive f a b n)
  (doc 'type '(-> (-> Interval Interval) Real Real Int Interval))
  (doc 'description "Integrate f over [a,b] using n uniform subdivisions.
f must be a function from intervals to intervals (natural interval extension).
Returns an interval guaranteed to contain the true integral.")
  (doc 'export #t)
  (if (<= n 0)
      (error 'interval-integrate-naive "n must be positive")
      (let* ([h (/ (- b a) n)])
    (let loop ([i 0] [sum (interval-singleton 0)])
      (if (>= i n)
          sum
          (let* ([xi (+ a (* i h))]
                 [xi+1 (+ a (* (+ i 1) h))]
                 [sub-iv (make-interval xi xi+1)]
                 [f-iv (f sub-iv)]
                 [contrib (interval-scale f-iv h)])
            (loop (+ i 1) (interval-add sum contrib))))))))

;;; ============================================================================
;;; Adaptive Interval Integration
;;; ============================================================================
;;;
;;; Refines the subdivision where the enclosure is widest. Starts with a
;;; single interval over [a,b], then repeatedly bisects the subinterval
;;; contributing the most uncertainty. This concentrates computational effort
;;; where it matters most.
;;;
;;; Uses a max-heap (leftist heap with custom comparator) for O(log N)
;;; extract-widest and insert. The running total is maintained incrementally
;;; via scalar component tracking, avoiding the O(N) recomputation that
;;; would be needed with interval-sub (which suffers the dependency problem).
;;;
;;; Total complexity: O(N log N) where N = number of refinement steps.

;;; Comparator for adaptive integration heap entries.
;;; Each entry is (contribution-interval . domain-interval).
;;; Wider contributions get higher priority (extracted first).
(define (adaptive-entry-wider? a b)
  (>= (interval-width (car a))
      (interval-width (car b))))

(define (interval-integrate-adaptive f a b tolerance max-subdivisions)
  (doc 'type '(-> (-> Interval Interval) Real Real Real Int Interval))
  (doc 'description "Adaptively integrate f over [a,b] to within the given tolerance.
Refines subdivisions where the enclosure is widest using a heap-based
priority queue for O(N log N) total complexity. Returns when the total
enclosure width is below tolerance or max-subdivisions is reached.")
  (doc 'export #t)
  (let* ([initial-iv (make-interval a b)]
         [f-iv (f initial-iv)]
         [h (- b a)]
         [initial-contrib (interval-scale f-iv h)]
         ;; Each heap entry: (contribution-interval . domain-interval)
         [heap (heap-insert-by adaptive-entry-wider?
                               (cons initial-contrib initial-iv)
                               heap-empty)]
         ;; Track sum components as raw floats for O(1) incremental update.
         ;; total = [sum-lo, sum-hi] at all times.
         [sum-lo (interval-lo initial-contrib)]
         [sum-hi (interval-hi initial-contrib)])
    (let loop ([heap heap]
               [sum-lo sum-lo]
               [sum-hi sum-hi]
               [n-subs 1])
      (let ([w (- sum-hi sum-lo)])
        (if (or (<= w tolerance)
                (>= n-subs max-subdivisions))
            (make-interval sum-lo sum-hi)
            ;; Pop widest entry and bisect
            (let* ([worst (heap-value heap)]
                   [heap (heap-delete-top-by adaptive-entry-wider? heap)]
                   ;; Remove old contribution from running sums
                   [sum-lo (- sum-lo (interval-lo (car worst)))]
                   [sum-hi (- sum-hi (interval-hi (car worst)))]
                   ;; Bisect domain
                   [dom (cdr worst)]
                   [mid-pt (interval-mid dom)]
                   [left-dom (make-interval (interval-lo dom) mid-pt)]
                   [right-dom (make-interval mid-pt (interval-hi dom))]
                   [left-f (f left-dom)]
                   [right-f (f right-dom)]
                   [left-h (interval-width left-dom)]
                   [right-h (interval-width right-dom)]
                   [left-contrib (interval-scale left-f left-h)]
                   [right-contrib (interval-scale right-f right-h)]
                   ;; Add new contributions to running sums
                   [sum-lo (+ sum-lo
                              (interval-lo left-contrib)
                              (interval-lo right-contrib))]
                   [sum-hi (+ sum-hi
                              (interval-hi left-contrib)
                              (interval-hi right-contrib))]
                   ;; Insert new entries into heap
                   [heap (heap-insert-by adaptive-entry-wider?
                                         (cons left-contrib left-dom)
                                         heap)]
                   [heap (heap-insert-by adaptive-entry-wider?
                                         (cons right-contrib right-dom)
                                         heap)])
              (loop heap sum-lo sum-hi (+ n-subs 1))))))))

;;; ============================================================================
;;; Bisected Integration
;;; ============================================================================
;;;
;;; Evaluates f on two half-intervals per subinterval instead of one full
;;; interval. This gives genuinely tighter bounds because interval
;;; over-estimation grows superlinearly with interval width — two evaluations
;;; on half-width intervals produce tighter enclosures than one on the full
;;; width. Costs 2x the f evaluations of naive at the same n.

(define (interval-integrate-midpoint f a b n)
  (doc 'type '(-> (-> Interval Interval) Real Real Int Interval))
  (doc 'description "Integrate f over [a,b] using bisected subintervals for tighter bounds.
Each of n subintervals is split at its midpoint, and f is evaluated on both halves.
This produces tighter enclosures than naive at the cost of 2x function evaluations.")
  (doc 'export #t)
  (if (<= n 0)
      (error 'interval-integrate-midpoint "n must be positive")
      (let* ([h (/ (- b a) n)])
        (let loop ([i 0] [sum (interval-singleton 0)])
          (if (>= i n)
              sum
              (let* ([xi (+ a (* i h))]
                     [xi+1 (+ a (* (+ i 1) h))]
                     [mid (/ (+ xi xi+1) 2)]
                     [half-h (/ h 2)]
                     ;; Left half: [xi, mid]
                     [left-f (f (make-interval xi mid))]
                     [left-contrib (interval-scale left-f half-h)]
                     ;; Right half: [mid, xi+1]
                     [right-f (f (make-interval mid xi+1))]
                     [right-contrib (interval-scale right-f half-h)]
                     [contrib (interval-add left-contrib right-contrib)])
                (loop (+ i 1) (interval-add sum contrib))))))))

;;; ============================================================================
;;; Convenience: interval-integrate
;;; ============================================================================

(define (interval-integrate f a b . opts)
  (doc 'type '(-> (-> Interval Interval) Real Real Interval))
  (doc 'description "Integrate f over [a,b] with verified enclosure.
Optional keyword arguments:
  method: 'naive, 'midpoint (default), or 'adaptive
  n: number of subdivisions (default 64)
  tolerance: for adaptive method (default 1e-8)
  max-subdivisions: for adaptive method (default 1000)")
  (doc 'export #t)
  (let* ([method (if (and (pair? opts) (symbol? (car opts)))
                     (car opts)
                     'midpoint)]
         [rest (if (and (pair? opts) (symbol? (car opts)))
                   (cdr opts)
                   opts)]
         [n (if (pair? rest) (car rest) 64)])
    (case method
      [(naive) (interval-integrate-naive f a b n)]
      [(midpoint) (interval-integrate-midpoint f a b n)]
      [(adaptive)
       (let ([tol (if (pair? rest) (car rest) 1e-8)]
             [max-subs (if (and (pair? rest) (pair? (cdr rest)))
                          (cadr rest) 1000)])
         (interval-integrate-adaptive f a b tol max-subs))]
      [else (error 'interval-integrate
                   (string-append "Unknown method: " (symbol->string method)))])))

;;; ============================================================================
;;; Rigorous Integration (Directed Rounding)
;;; ============================================================================
;;;
;;; Uses rigorous interval operations (directed rounding) for formal
;;; guarantees. Slower but suitable for computer-aided proofs.

(define (interval-integrate-rigorous f a b n)
  (doc 'type '(-> (-> Interval Interval) Real Real Int Interval))
  (doc 'description "Rigorously integrate f over [a,b] using n subdivisions.
Uses directed rounding for all operations to guarantee the result
formally encloses the true integral. ~2x slower than standard version.")
  (doc 'export #t)
  (if (<= n 0)
      (error 'interval-integrate-rigorous "n must be positive")
      (let* ([h (/ (- b a) n)])
    (let loop ([i 0] [sum (interval-singleton 0)])
      (if (>= i n)
          sum
          (let* ([xi (+ a (* i h))]
                 [xi+1 (+ a (* (+ i 1) h))]
                 [sub-iv (make-interval xi xi+1)]
                 [f-iv (f sub-iv)]
                 [contrib (interval-scale-rigorous f-iv h)])
            (loop (+ i 1) (interval-add-rigorous sum contrib))))))))

