;;; test-graph-bridge.ss — Tests for graph-matrix bridge
;;;
;;; Run with: scheme --script lattice/data/graph/test-graph-bridge.ss

(load "core/lang/module.ss")
(load "lattice/data/graph/graph-bridge.ss")

(load "core/testing/test-framework.ss")

(printf "\n=== Graph-Matrix Bridge Tests ===\n\n")

;;; ====
;;; Graph Record Construction
;;; ====

(test-group "graph-construction"

  (define-test "make-graph directed unweighted"
    (let ([g (make-graph '((0 1) (1 2) (2 0)))])
      (assert-true (graph? g))
      (assert-equal 3 (graph-node-count g))
      (assert-true (graph-directed? g))
      (assert-false (graph-weighted? g))))

  (define-test "make-graph directed weighted"
    (let ([g (make-graph '((0 1 2.5) (1 2 3.0)))])
      (assert-true (graph? g))
      (assert-equal 3 (graph-node-count g))
      (assert-true (graph-directed? g))
      (assert-true (graph-weighted? g))))

  (define-test "make-graph with explicit node count"
    (let ([g (make-graph '((0 1)) 5)])
      (assert-equal 5 (graph-node-count g))))

  (define-test "make-graph-undirected"
    (let ([g (make-graph-undirected '((0 1) (1 2)))])
      (assert-false (graph-directed? g))))

  (define-test "graph? rejects non-graphs"
    (assert-false (graph? 42))
    (assert-false (graph? '(not a graph)))
    (assert-false (graph? '(graph)))))

;;; ====
;;; Graph -> Matrix Conversions
;;; ====

(test-group "graph-to-matrix"

  (define-test "graph->adjacency-matrix directed"
    (let* ([g (make-graph '((0 1) (1 2) (2 0)))]
           [m (graph->adjacency-matrix g)])
      (assert-true (matrix? m))
      (assert-equal '(3 . 3) (matrix-shape m))
      (assert-equal 1 (matrix-ref m 0 1))
      (assert-equal 1 (matrix-ref m 1 2))
      (assert-equal 1 (matrix-ref m 2 0))
      ;; Directed: no reverse edges
      (assert-equal 0 (matrix-ref m 1 0))))

  (define-test "graph->adjacency-matrix undirected"
    (let* ([g (make-graph-undirected '((0 1) (1 2)))]
           [m (graph->adjacency-matrix g)])
      ;; Undirected: both directions
      (assert-equal 1 (matrix-ref m 0 1))
      (assert-equal 1 (matrix-ref m 1 0))
      (assert-equal 1 (matrix-ref m 1 2))
      (assert-equal 1 (matrix-ref m 2 1))))

  (define-test "graph->adjacency-matrix weighted"
    (let* ([g (make-graph '((0 1 2.5) (1 2 3.0)))]
           [m (graph->adjacency-matrix g)])
      (assert-equal 2.5 (matrix-ref m 0 1))
      (assert-equal 3.0 (matrix-ref m 1 2))))

  (define-test "graph->sparse-adjacency"
    (let* ([g (make-graph '((0 1) (1 2) (2 0)))]
           [m (graph->sparse-adjacency g)])
      (assert-true (sparse-csr? m))
      (assert-equal 1 (sparse-csr-ref m 0 1))
      (assert-equal 1 (sparse-csr-ref m 1 2))
      (assert-equal 0 (sparse-csr-ref m 1 0))))

  (define-test "graph->degree-matrix"
    (let* ([g (make-graph '((0 1) (0 2) (1 2)))]
           [d (graph->degree-matrix g)])
      ;; Out-degrees: node 0 has 2, node 1 has 1, node 2 has 0
      (assert-equal 2 (matrix-ref d 0 0))
      (assert-equal 1 (matrix-ref d 1 1))
      (assert-equal 0 (matrix-ref d 2 2))
      ;; Off-diagonal is 0
      (assert-equal 0 (matrix-ref d 0 1))))

  (define-test "graph->laplacian-matrix triangle"
    (let* ([g (make-graph-undirected '((0 1) (1 2) (2 0)))]
           [L (graph->laplacian-matrix g)])
      ;; L = D - A for K3 (undirected triangle)
      ;; Each node has degree 2
      ;; L[i,i] = 2, L[i,j] = -1 for adjacent, 0 otherwise
      (assert-equal 2 (matrix-ref L 0 0))
      (assert-equal 2 (matrix-ref L 1 1))
      (assert-equal -1 (matrix-ref L 0 1))
      (assert-equal -1 (matrix-ref L 1 0))))

  (define-test "graph->laplacian-matrix row sums zero"
    ;; Laplacian rows always sum to zero
    (let* ([g (make-graph-undirected '((0 1) (1 2) (2 3) (3 0)))]
           [L (graph->laplacian-matrix g)]
           [n (matrix-rows L)])
      (let check ([i 0])
        (when (< i n)
          (let row-sum ([j 0] [s 0])
            (if (= j n)
                (assert-equal 0 s)
                (row-sum (+ j 1) (+ s (matrix-ref L i j)))))
          (check (+ i 1)))))))

;;; ====
;;; Matrix -> Graph Conversions
;;; ====

(test-group "matrix-to-graph"

  (define-test "adjacency-matrix->graph directed"
    (let* ([m (edges->adjacency-matrix '((0 1) (1 2) (2 0)))]
           [g (adjacency-matrix->graph m #t)])
      (assert-true (graph? g))
      (assert-equal 3 (graph-node-count g))
      (assert-true (graph-directed? g))
      (assert-equal 3 (graph-edge-count g))))

  (define-test "adjacency-matrix->graph undirected"
    (let* ([m (edges->adjacency-matrix '((0 1) (1 2)) 3 #t)]
           [g (adjacency-matrix->graph m #f)])
      (assert-false (graph-directed? g))
      ;; Only upper-triangle edges extracted
      (assert-equal 2 (graph-edge-count g))))

  (define-test "adjacency-matrix->graph weighted detection"
    (let* ([m (edges->adjacency-matrix '((0 1 2.5) (1 2 3.0)))]
           [g (adjacency-matrix->graph m)])
      (assert-true (graph-weighted? g))))

  (define-test "adjacency-matrix->graph unweighted detection"
    (let* ([m (edges->adjacency-matrix '((0 1) (1 2)))]
           [g (adjacency-matrix->graph m)])
      (assert-false (graph-weighted? g))))

  (define-test "sparse-adjacency->graph"
    (let* ([m (edges->sparse-adjacency '((0 1) (1 2) (2 0)))]
           [g (sparse-adjacency->graph m #t)])
      (assert-true (graph? g))
      (assert-equal 3 (graph-node-count g))
      (assert-equal 3 (graph-edge-count g)))))

;;; ====
;;; Round-Trip Tests
;;; ====

(test-group "round-trip"

  (define-test "directed unweighted round-trip"
    ;; graph -> matrix -> graph preserves structure
    (let* ([g1 (make-graph '((0 1) (1 2) (2 0)))]
           [m (graph->adjacency-matrix g1)]
           [g2 (adjacency-matrix->graph m #t #f)])
      (assert-true (graph-equal? g1 g2))))

  (define-test "directed weighted round-trip"
    (let* ([g1 (make-graph '((0 1 2.5) (1 2 3.0) (2 0 1.0)))]
           [m (graph->adjacency-matrix g1)]
           [g2 (adjacency-matrix->graph m #t #t)])
      (assert-true (graph-equal? g1 g2))))

  (define-test "undirected unweighted round-trip"
    (let* ([g1 (make-graph-undirected '((0 1) (1 2) (0 2)))]
           [m (graph->adjacency-matrix g1)]
           [g2 (adjacency-matrix->graph m #f #f)])
      (assert-true (graph-equal? g1 g2))))

  (define-test "undirected weighted round-trip"
    (let* ([g1 (make-graph '((0 1 1.5) (1 2 2.5) (0 2 3.5)) 3 #f #t)]
           [m (graph->adjacency-matrix g1)]
           [g2 (adjacency-matrix->graph m #f #t)])
      (assert-true (graph-equal? g1 g2))))

  (define-test "matrix round-trip preserves entries"
    ;; matrix -> graph -> matrix gives the same matrix
    (let* ([m1 (edges->adjacency-matrix '((0 1) (1 2) (2 0)))]
           [g (adjacency-matrix->graph m1 #t)]
           [m2 (graph->adjacency-matrix g)])
      (assert-true (matrix-equal? m1 m2))))

  (define-test "sparse round-trip"
    (let* ([g1 (make-graph '((0 1) (1 2) (2 0)))]
           [s (graph->sparse-adjacency g1)]
           [g2 (sparse-adjacency->graph s #t #f)])
      (assert-true (graph-equal? g1 g2))))

  (define-test "round-trip via dense and sparse agree"
    (let* ([g (make-graph '((0 1) (1 2) (2 3) (3 0)))]
           [dense (graph->adjacency-matrix g)]
           [sparse (graph->sparse-adjacency g)]
           [g-from-dense (adjacency-matrix->graph dense #t #f)]
           [g-from-sparse (sparse-adjacency->graph sparse #t #f)])
      (assert-true (graph-equal? g-from-dense g-from-sparse)))))

;;; ====
;;; Graph Properties
;;; ====

(test-group "graph-properties"

  (define-test "graph-edge-count"
    (let ([g (make-graph '((0 1) (1 2) (2 0)))])
      (assert-equal 3 (graph-edge-count g))))

  (define-test "graph-density directed"
    ;; 3 edges out of 3*2 = 6 possible = 0.5
    (let ([g (make-graph '((0 1) (1 2) (2 0)))])
      (assert-equal 1/2 (graph-density g))))

  (define-test "graph-density undirected"
    ;; 3 edges out of 3*2/2 = 3 possible = 1.0 (complete)
    (let ([g (make-graph-undirected '((0 1) (1 2) (0 2)))])
      (assert-equal 1 (graph-density g))))

  (define-test "graph-neighbors"
    (let ([g (make-graph '((0 1) (0 2) (1 2)))])
      (assert-equal '(1 2) (graph-neighbors g 0))
      (assert-equal '(2) (graph-neighbors g 1)))))

;;; ====
;;; Graph Equality
;;; ====

(test-group "graph-equality"

  (define-test "same graphs are equal"
    (let ([g1 (make-graph '((0 1) (1 2)))]
          [g2 (make-graph '((0 1) (1 2)))])
      (assert-true (graph-equal? g1 g2))))

  (define-test "different edge order still equal"
    ;; Edge order doesn't matter -- equality is via adjacency matrix
    (let ([g1 (make-graph '((0 1) (1 2)))]
          [g2 (make-graph '((1 2) (0 1)))])
      (assert-true (graph-equal? g1 g2))))

  (define-test "different edges are not equal"
    (let ([g1 (make-graph '((0 1) (1 2)))]
          [g2 (make-graph '((0 1) (2 0)))])
      (assert-false (graph-equal? g1 g2))))

  (define-test "different directedness not equal"
    (let ([g1 (make-graph '((0 1)) 2 #t)]
          [g2 (make-graph '((0 1)) 2 #f)])
      (assert-false (graph-equal? g1 g2)))))

;;; ====
;;; Spectral Convenience
;;; ====

(test-group "spectral-convenience"

  (define-test "graph->laplacian-normalized runs"
    ;; Just verify it produces a matrix, not an error
    (let* ([g (make-graph-undirected '((0 1) (1 2) (2 0)))]
           [L (graph->laplacian-normalized g)])
      (assert-true (matrix? L))
      (assert-equal '(3 . 3) (matrix-shape L))))

  (define-test "graph->laplacian-random-walk runs"
    (let* ([g (make-graph-undirected '((0 1) (1 2) (2 0)))]
           [L (graph->laplacian-random-walk g)])
      (assert-true (matrix? L))
      (assert-equal '(3 . 3) (matrix-shape L)))))

;;; ====
;;; Edge Cases
;;; ====

(test-group "edge-cases"

  (define-test "empty graph"
    (let* ([g (make-graph '() 3)]
           [m (graph->adjacency-matrix g)])
      (assert-equal 0 (adjacency-matrix-edge-count m))
      (assert-equal 0 (graph-edge-count g))))

  (define-test "single node graph"
    (let* ([g (make-graph '() 1)]
           [m (graph->adjacency-matrix g)])
      (assert-equal '(1 . 1) (matrix-shape m))))

  (define-test "self-loop"
    (let* ([g (make-graph '((0 0)) 1)]
           [m (graph->adjacency-matrix g)])
      (assert-equal 1 (matrix-ref m 0 0))))

  (define-test "disconnected graph"
    (let* ([g (make-graph '((0 1) (2 3)) 4)]
           [m (graph->adjacency-matrix g)])
      (assert-equal 1 (matrix-ref m 0 1))
      (assert-equal 1 (matrix-ref m 2 3))
      (assert-equal 0 (matrix-ref m 0 2)))))

(run-all-tests)
