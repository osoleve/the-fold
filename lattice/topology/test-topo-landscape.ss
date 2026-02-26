;;; lattice/topology/test-topo-landscape.ss — Tests for Topological Landscape Analysis
;;;
;;; Run with: scheme --script lattice/topology/test-topo-landscape.ss

(load "core/testing/test-framework.ss")
(load "lattice/topology/topo-landscape.ss")

(display "\n====\n")
(display "Topological Landscape Analysis Tests\n")
(display "====\n\n")

;;; Helper test functions (defined at top level)
(define (bowl-2d x y) (+ (* x x) (* y y)))
(define (two-wells x y)
  (let ([x2 (* x x)])
    (+ (* (- x2 1) (- x2 1)) (* y y))))
(define (simple-f x y) (+ (* x x) (* y y)))
(define (scaled-f x y) (+ (* 2 x x) (* 2 y y)))

;;; ====
;;; Grid Sampling Tests
;;; ====

(test-group sampling-tests

  (define-test test-sample-grid-2d-count
    "2D grid sampling produces correct number of points"
    (let ([samples (sample-grid-2d (lambda (x y) (+ (* x x) (* y y)))
                                   -1 1 -1 1 5 5)])
      (assert-equal 25 (length samples))))

  (define-test test-sample-grid-2d-values
    "2D grid sampling computes function values correctly"
    (let ([samples (sample-grid-2d (lambda (x y) (+ (* x x) (* y y)))
                                   0 1 0 1 2 2)])
      ;; Points: (0,0)=0, (0,1)=1, (1,0)=1, (1,1)=2
      (let ([fvals (map caddr samples)])
        (assert-true (if (member 0 fvals) #t #f))
        (assert-true (if (member 2 fvals) #t #f)))))

  (define-test test-sample-grid-2d-single
    "Single point grid"
    (let ([samples (sample-grid-2d (lambda (x y) 42)
                                   0 0 0 0 1 1)])
      (assert-equal 1 (length samples))
      (assert-equal 42 (caddr (car samples)))))

  (define-test test-sample-grid-nd-2d
    "N-dimensional grid sampling for 2D case"
    (let ([samples (sample-grid-nd (lambda (pt) (apply + (map (lambda (x) (* x x)) pt)))
                                   '((0 1) (0 1))
                                   '(3 3))])
      (assert-equal 9 (length samples))
      ;; Each sample should have 3 elements: x, y, f(x,y)
      (assert-equal 3 (length (car samples)))))

  (define-test test-sample-grid-nd-1d
    "N-dimensional grid sampling for 1D case"
    (let ([samples (sample-grid-nd (lambda (pt) (* (car pt) (car pt)))
                                   '((-2 2))
                                   '(5))])
      (assert-equal 5 (length samples))
      ;; Each sample: (x f(x))
      (assert-equal 2 (length (car samples)))))

  (define-test test-cartesian-product
    "Cartesian product of lists"
    (let ([result (cartesian-product '((1 2) (a b)))])
      (assert-equal 4 (length result))
      (assert-true (if (member '(1 a) result) #t #f))
      (assert-true (if (member '(2 b) result) #t #f))))

  (define-test test-cartesian-product-empty
    "Cartesian product of empty input"
    (let ([result (cartesian-product '())])
      (assert-equal '(()) result)))

  (define-test test-sample-around-points
    "Sampling around trajectory points"
    (let ([samples (sample-around-points
                    (lambda (pt) (apply + (map (lambda (x) (* x x)) pt)))
                    '((0 0) (1 1))
                    0.5
                    3)])
      ;; 3×3 = 9 per center, 2 centers = 18 max (minus deduplication)
      (assert-true (> (length samples) 0))
      (assert-true (<= (length samples) 18))))

  (define-test test-deduplicate-points
    "Point deduplication"
    (let ([result (deduplicate-points '((0 0 1) (0 0 2) (1 1 3) (1 1 4)) 2)])
      (assert-equal 2 (length result))
      ;; First occurrence wins
      (assert-equal 1 (caddr (car result)))))
)

;;; ====
;;; Landscape Persistence Tests
;;; ====

(test-group landscape-persistence-tests

  (define-test test-bowl-persistence
    "Quadratic bowl should show one dominant basin"
    (let* ([samples (sample-grid-2d bowl-2d -2 2 -2 2 8 8)]
           [pd (landscape-persistence samples 2 1 1.5 1.0)])
      (assert-true (persistence-diagram? pd))
      ;; Should have at least one H0 feature
      (let ([h0 (diagram-points-dim pd 0)])
        (assert-true (> (length h0) 0)))))

  (define-test test-two-wells-basins
    "Two-well potential should have multiple H0 features"
    (let* ([samples (sample-grid-2d two-wells -2 2 -1 1 10 6)]
           [pd (landscape-persistence samples 2 1 1.0 1.0)])
      ;; Should see at least 2 components at small epsilon
      (let ([h0 (diagram-points-dim pd 0)])
        (assert-true (> (length h0) 1)))))

  (define-test test-landscape-persistence-returns-diagram
    "landscape-persistence returns a PersistenceDiagram"
    (let* ([samples '((0 0 0) (1 0 1) (0 1 1) (1 1 2))]
           [pd (landscape-persistence samples 2 1 2.0 1.0)])
      (assert-true (persistence-diagram? pd))))
)

;;; ====
;;; Feature Extraction Tests
;;; ====

(test-group feature-extraction-tests

  (define-test test-landscape-betti
    "Betti number extraction"
    (let* ([samples (sample-grid-2d simple-f -2 2 -2 2 6 6)]
           [pd (landscape-persistence samples 2 1 2.0 1.0)])
      ;; β₀ at any time should be ≥ 0
      (assert-true (>= (landscape-betti pd 0 5.0) 0))))

  (define-test test-count-basins-simple
    "Basin counting with threshold"
    (let* ([samples (sample-grid-2d simple-f -2 2 -2 2 6 6)]
           [pd (landscape-persistence samples 2 1 2.0 1.0)])
      ;; With threshold 0, should count all features
      (assert-true (>= (count-basins pd 0) 1))))

  (define-test test-count-saddles
    "Saddle counting"
    (let* ([samples (sample-grid-2d simple-f -2 2 -2 2 6 6)]
           [pd (landscape-persistence samples 2 1 2.0 1.0)])
      ;; Number of saddles should be non-negative
      (assert-true (>= (count-saddles pd 0) 0))))

  (define-test test-landscape-signature-structure
    "Signature returns expected keys"
    (let* ([samples (sample-grid-2d simple-f -1 1 -1 1 5 5)]
           [pd (landscape-persistence samples 2 1 2.0 1.0)]
           [sig (landscape-signature pd)])
      (assert-true (if (assq 'basins-total sig) #t #f))
      (assert-true (if (assq 'basins-essential sig) #t #f))
      (assert-true (if (assq 'basins-finite sig) #t #f))
      (assert-true (if (assq 'saddles-total sig) #t #f))
      (assert-true (if (assq 'total-basin-persistence sig) #t #f))
      (assert-true (if (assq 'max-basin-persistence sig) #t #f))))

  (define-test test-landscape-complexity-non-negative
    "Landscape complexity is non-negative"
    (let* ([samples (sample-grid-2d simple-f -1 1 -1 1 5 5)]
           [pd (landscape-persistence samples 2 1 2.0 1.0)])
      (assert-true (>= (landscape-complexity pd 0) 0))))

  (define-test test-landscape-complexity-threshold
    "Higher threshold reduces complexity measure"
    (let* ([samples (sample-grid-2d simple-f -2 2 -2 2 6 6)]
           [pd (landscape-persistence samples 2 1 2.0 1.0)]
           [c-low (landscape-complexity pd 0)]
           [c-high (landscape-complexity pd 100)])
      ;; Higher threshold should give ≤ complexity
      (assert-true (>= c-low c-high))))
)

;;; ====
;;; Landscape Comparison Tests
;;; ====

(test-group comparison-tests

  (define-test test-landscape-distance-self
    "Distance of a landscape to itself is 0"
    (let* ([samples (sample-grid-2d simple-f -1 1 -1 1 5 5)]
           [pd (landscape-persistence samples 2 1 2.0 1.0)]
           [dist (landscape-distance pd pd 0)])
      (assert-true (< dist 1e-10))))

  (define-test test-landscape-similar-self
    "Landscape is similar to itself"
    (let* ([samples (sample-grid-2d simple-f -1 1 -1 1 5 5)]
           [pd (landscape-persistence samples 2 1 2.0 1.0)])
      (assert-true (landscape-similar? pd pd 0 0.01))))

  (define-test test-landscape-distance-non-negative
    "Bottleneck distance is non-negative"
    (let* ([s1 (sample-grid-2d simple-f -1 1 -1 1 5 5)]
           [s2 (sample-grid-2d scaled-f -1 1 -1 1 5 5)]
           [pd1 (landscape-persistence s1 2 1 2.0 1.0)]
           [pd2 (landscape-persistence s2 2 1 2.0 1.0)]
           [dist (landscape-distance pd1 pd2 0)])
      (assert-true (>= dist 0))))
)

;;; ====
;;; End-to-End Pipeline Tests
;;; ====

(test-group pipeline-tests

  (define-test test-analyze-landscape-2d
    "End-to-end 2D analysis pipeline"
    (let ([sig (analyze-landscape-2d
                (lambda (x y) (+ (* x x) (* y y)))
                -2 2 -2 2 6 6 2.0)])
      (assert-true (list? sig))
      (assert-true (> (length sig) 0))
      ;; Should find at least one basin
      (assert-true (>= (cdr (assq 'basins-total sig)) 1))))

  (define-test test-analyze-landscape-nd
    "End-to-end N-dimensional analysis pipeline"
    (let ([sig (analyze-landscape-nd
                (lambda (pt) (apply + (map (lambda (x) (* x x)) pt)))
                '((-1 1) (-1 1))
                '(5 5)
                2.0)])
      (assert-true (list? sig))
      (assert-true (>= (cdr (assq 'basins-total sig)) 1))))

  (define-test test-trajectory-persistence
    "Trajectory-based persistence computation"
    (let* ([trajectory '((0 0) (0.5 0.5) (1 1) (1.5 0.5) (2 0))]
           [f (lambda (pt) (apply + (map (lambda (x) (* x x)) pt)))]
           [pd (trajectory-persistence trajectory f 2 3.0 1.0)])
      (assert-true (persistence-diagram? pd))))
)

;;; ====
;;; Summary
;;; ====

(display "\n====\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(display "====\n")

(when (> *tests-failed* 0)
      (exit 1))
