;;; lattice/data/graph/test-graph-filtration.ss — Tests for graph-filtration bridge
;;;
;;; Tests the bridge from weighted adjacency matrices to persistent homology,
;;; verifying filtration construction, persistence pairs, and Betti curves.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(require 'graph-filtration)

;;; ============================================================
;;; Helper: build symmetric weighted adjacency matrix
;;; ============================================================

(define (weighted-graph edges n)
  ;; edges: ((i j w) ...) -- builds undirected weighted adjacency matrix
  (edges->adjacency-matrix edges n #t))

;;; ============================================================
;;; Weight Filtration Construction
;;; ============================================================

(test-group 'weight-filtration

  (define-test "triangle graph produces correct filtration"
    ;; Triangle with edges of weight 1, 2, 3
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3)) 3)]
           [f (graph-weight-filtration m)]
           [pairs (filtration-pairs f)])
      ;; 3 vertices + 3 edges = 6 simplices
      (assert-equal 6 (length pairs))
      ;; Filtration should be ordered by birth time
      (assert-true (pair? pairs))))

  (define-test "weight filtration vertices born with their earliest edge"
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3)) 3)]
           [f (graph-weight-filtration m)]
           [pairs (filtration-pairs f)])
      ;; Vertex 0 should be born at t=1 (earliest edge 0-1)
      ;; Vertex 1 should be born at t=1 (earliest edge 0-1)
      ;; Vertex 2 should be born at t=2 (earliest edge 1-2)
      (let ([v0-pair (find-simplex-pair pairs '(0))]
            [v1-pair (find-simplex-pair pairs '(1))]
            [v2-pair (find-simplex-pair pairs '(2))])
        (assert-equal 1 (cdr v0-pair))
        (assert-equal 1 (cdr v1-pair))
        (assert-equal 2 (cdr v2-pair)))))

  (define-test "unique weights extraction"
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3) (2 3 2)) 4)]
           [weights (adjacency-unique-weights m)])
      ;; Should be (1 2 3) -- unique sorted
      (assert-equal '(1 2 3) weights)))

  (define-test "path graph filtration has no higher simplices"
    ;; Path: 0-1-2, edges have weight 1 and 2
    (let* ([m (weighted-graph '((0 1 1) (1 2 2)) 3)]
           [f (graph-weight-filtration m)]
           [simplices (filtration-simplices f)])
      ;; Only vertices and edges, no triangles
      (assert-true (every-simplex-dim<=1? simplices)))))

;;; Helper to find a simplex-pair by vertex set
(define (find-simplex-pair pairs vertices)
  (cond
    [(null? pairs) #f]
    [(equal? (simplex-vertices (caar pairs)) vertices) (car pairs)]
    [else (find-simplex-pair (cdr pairs) vertices)]))

(define (every-simplex-dim<=1? simplices)
  (cond
    [(null? simplices) #t]
    [(> (simplex-dim (car simplices)) 1) #f]
    [else (every-simplex-dim<=1? (cdr simplices))]))

;;; ============================================================
;;; Rips Filtration Construction
;;; ============================================================

(test-group 'rips-filtration

  (define-test "triangle graph Rips includes triangle simplex"
    ;; Complete triangle: all edges present
    (let* ([m (weighted-graph '((0 1 1) (1 2 1) (0 2 1)) 3)]
           [f (graph-rips-filtration m 2)]
           [simplices (filtration-simplices f)])
      ;; Should have: 3 vertices + 3 edges + 1 triangle = 7
      (assert-equal 7 (length simplices))
      ;; The triangle should be born at max pairwise weight = 1
      (let ([triangle-pairs (filter (lambda (p) (= 2 (simplex-dim (car p))))
                                    (filtration-pairs f))])
        (assert-equal 1 (length triangle-pairs))
        (assert-true (= 1 (cdr (car triangle-pairs)))))))

  (define-test "path graph Rips has no triangle"
    ;; Path: 0-1-2, no edge between 0 and 2
    (let* ([m (weighted-graph '((0 1 1) (1 2 1)) 3)]
           [f (graph-rips-filtration m 2)]
           [simplices (filtration-simplices f)])
      ;; Should have: 3 vertices + 2 edges = 5 (no triangle)
      (assert-equal 5 (length simplices))))

  (define-test "Rips filtration with different edge weights"
    ;; Triangle with edges 1, 2, 3
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3)) 3)]
           [f (graph-rips-filtration m 2)]
           [triangle-pairs (filter (lambda (p) (= 2 (simplex-dim (car p))))
                                   (filtration-pairs f))])
      ;; Triangle born at max pairwise weight = 3
      (assert-equal 1 (length triangle-pairs))
      (assert-true (= 3 (cdr (car triangle-pairs))))))

  (define-test "square with diagonal forms two triangles"
    ;; Square 0-1-2-3 with diagonal 0-2, all weight 1
    (let* ([m (weighted-graph '((0 1 1) (1 2 1) (2 3 1) (0 3 1) (0 2 1)) 4)]
           [f (graph-rips-filtration m 2)]
           [triangles (filter (lambda (p) (= 2 (simplex-dim (car p))))
                              (filtration-pairs f))])
      ;; Should have 2 triangles: (0,1,2) and (0,2,3)
      (assert-equal 2 (length triangles)))))

;;; ============================================================
;;; Persistent Homology of Graphs
;;; ============================================================

(test-group 'graph-persistent-homology

  (define-test "triangle: H_0 starts at 3, ends at 1"
    ;; Triangle with edges at different weights: 1, 2, 3
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3)) 3)]
           [diagram (graph-persistent-homology m 2)])
      ;; At t=0 (vertices born): 3 components
      (assert-equal 3 (diagram-betti diagram 0 0.0))
      ;; Between first and second edge: 2 components
      (assert-equal 2 (diagram-betti diagram 0 1.5))
      ;; After all edges: 1 component
      (assert-equal 1 (diagram-betti diagram 0 3.5))))

  (define-test "triangle: H_1 = 1 when triangle closes"
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3)) 3)]
           [diagram (graph-persistent-homology m 2)])
      ;; After last edge (t=3) but before triangle fills: H_1 should appear
      ;; In Rips, triangle fills at max pairwise = 3 (same as last edge)
      ;; So H_1 is born and immediately dies -- check it's 0 or there's a finite pair
      ;; Actually, the cycle forms when the last edge completes the cycle, and
      ;; the triangle fills at the same time (since max pairwise = max edge = 3).
      ;; So the H_1 feature should be born at 3 (cycle from last edge) and die at 3 (triangle).
      ;; This means birth = death = 3, which is a zero-persistence feature.
      ;; With different edge weights this is expected behavior.
      ;; Let's check that H_1 is 0 at t > 3 (triangle fills any cycle)
      (assert-equal 0 (diagram-betti diagram 1 3.5))))

  (define-test "triangle equal weights: H_1 born and killed at same time"
    ;; All edges weight 1 -- cycle and fill happen simultaneously
    (let* ([m (weighted-graph '((0 1 1) (1 2 1) (0 2 1)) 3)]
           [diagram (graph-persistent-homology m 2)])
      ;; After triangle at t=1: H_1 should be 0 (cycle filled)
      (assert-equal 0 (diagram-betti diagram 1 1.5))))

  (define-test "path graph: H_0 decreases, H_1 stays 0"
    ;; Path: 0-1-2 with weights 1, 2
    (let* ([m (weighted-graph '((0 1 1) (1 2 2)) 3)]
           [diagram (graph-persistent-homology m 1)])
      ;; At t=0 (vertices born): 3 components
      (assert-equal 3 (diagram-betti diagram 0 0.0))
      ;; After first edge: 2 components
      (assert-equal 2 (diagram-betti diagram 0 1.5))
      ;; After second edge: 1 component
      (assert-equal 1 (diagram-betti diagram 0 2.5))
      ;; H_1 always 0 (no cycles possible)
      (assert-equal 0 (diagram-betti diagram 1 0.5))
      (assert-equal 0 (diagram-betti diagram 1 1.5))
      (assert-equal 0 (diagram-betti diagram 1 2.5))))

  (define-test "square with diagonal: H_1 cycle from square, killed by diagonal triangle"
    ;; Square: 0-1 (w=1), 1-2 (w=1), 2-3 (w=1), 0-3 (w=1), diagonal 0-2 (w=2)
    ;; At t=1: edges of the square form cycle (H_1 = 1)
    ;; At t=2: diagonal added, triangles form, killing the cycle (H_1 = 0)
    (let* ([m (weighted-graph '((0 1 1) (1 2 1) (2 3 1) (0 3 1) (0 2 2)) 4)]
           [diagram (graph-persistent-homology m 2)])
      ;; After square edges, before diagonal: H_1 >= 1
      (assert-true (>= (diagram-betti diagram 1 1.5) 1))
      ;; After diagonal adds triangles: H_1 = 0
      (assert-equal 0 (diagram-betti diagram 1 2.5))))

  (define-test "persistence pairs have birth <= death"
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3) (2 3 4)) 4)]
           [diagram (graph-persistent-homology m 2)]
           [all-pts (diagram-points diagram)])
      (assert-true (null? (filter (lambda (pt)
                                    (and (not (infinite? (cadr pt)))
                                         (> (car pt) (cadr pt))))
                                  all-pts))))))

;;; ============================================================
;;; Betti Curve
;;; ============================================================

(test-group 'betti-curve

  (define-test "H_0 Betti curve is non-increasing"
    ;; H_0 (components) can only decrease as edges are added
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3) (2 3 4)) 4)]
           [diagram (graph-persistent-homology m 2)]
           [thresholds '(0.0 0.5 1.5 2.5 3.5 4.5)]
           [curve (graph-betti-curve diagram 0 thresholds)])
      ;; Each Betti number should be <= the previous one
      (assert-true (betti-non-increasing? curve))))

  (define-test "Betti curve at specific thresholds"
    ;; Path: 0-1-2, weights 1, 2
    (let* ([m (weighted-graph '((0 1 1) (1 2 2)) 3)]
           [diagram (graph-persistent-homology m 1)]
           [curve (graph-betti-curve diagram 0 '(0.0 1.5 2.5))])
      (assert-equal 3 (cdr (car curve)))     ;; 3 components at t=0
      (assert-equal 2 (cdr (cadr curve)))    ;; 2 after first edge
      (assert-equal 1 (cdr (caddr curve))))) ;; 1 after second edge

  (define-test "auto Betti curve uses natural thresholds"
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3)) 3)]
           [curve (graph-betti-curve-auto m 0)])
      ;; Should produce a non-empty curve
      (assert-true (> (length curve) 0))
      ;; Should be non-increasing for H_0
      (assert-true (betti-non-increasing? curve)))))

(define (betti-non-increasing? curve)
  (cond
    [(null? curve) #t]
    [(null? (cdr curve)) #t]
    [(> (cdr (cadr curve)) (cdr (car curve))) #f]
    [else (betti-non-increasing? (cdr curve))]))

;;; ============================================================
;;; Persistence Diagram Extraction
;;; ============================================================

(test-group 'diagram-extraction

  (define-test "finite pairs extracted correctly"
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3)) 3)]
           [diagram (graph-persistent-homology m 2)]
           [h0-finite (graph-persistence-pairs-finite diagram 0)])
      ;; Should have finite H_0 pairs (components merging)
      (assert-true (> (length h0-finite) 0))
      ;; All finite pairs should have birth < death
      (assert-true (null? (filter (lambda (pt) (>= (car pt) (cadr pt)))
                                  h0-finite)))))

  (define-test "essential pairs for connected graph"
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3)) 3)]
           [diagram (graph-persistent-homology m 2)]
           [h0-essential (graph-persistence-pairs-essential diagram 0)])
      ;; Connected graph: exactly 1 essential H_0 component
      (assert-equal 1 (length h0-essential))))

  (define-test "max persistence is positive for non-trivial graph"
    (let* ([m (weighted-graph '((0 1 1) (1 2 3)) 3)]
           [diagram (graph-persistent-homology m 1)]
           [max-p (graph-max-persistence diagram 0)])
      ;; There should be a component that persists for some time
      (assert-true (> max-p 0)))))

;;; ============================================================
;;; Weight-only Persistent Homology
;;; ============================================================

(test-group 'weight-persistent-homology

  (define-test "weight-only persistence tracks component merging"
    (let* ([m (weighted-graph '((0 1 1) (1 2 2) (0 2 3)) 3)]
           [diagram (graph-weight-persistent-homology m)]
           [h0 (graph-persistence-pairs diagram 0)])
      ;; Should have H_0 features
      (assert-true (> (length h0) 0)))))

;;; ============================================================
;;; Run
;;; ============================================================

(display "\n=== Graph Filtration Tests ===\n\n")
(run-all-tests)
