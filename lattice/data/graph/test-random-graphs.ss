;;; lattice/data/graph/test-random-graphs.ss — Tests for random graph generators

(load "core/testing/test-framework.ss")
(load "lattice/data/graph/random-graphs.ss")
(load "lattice/data/graph/graph-community.ss")

;;; ============================================================================
;;; Erdos-Renyi Tests
;;; ============================================================================

(test-group "erdos-renyi"

  (define-test "produces correct node count"
    (let ([g (with-random 42 (erdos-renyi 10 0.5))])
      (assert-equal 10 (matrix-rows g))
      (assert-equal 10 (matrix-cols g))))

  (define-test "p=0 produces empty graph"
    (let ([g (with-random 42 (erdos-renyi 10 0.0))])
      (assert-equal 0 (adjacency-matrix-edge-count g))))

  (define-test "p=1 produces complete graph"
    (let ([g (with-random 42 (erdos-renyi 10 1.0))])
      ;; Complete graph on 10 nodes: 10*9 = 90 directed entries
      ;; (each undirected edge stored in both directions)
      (assert-equal 90 (adjacency-matrix-edge-count g))))

  (define-test "is symmetric (undirected)"
    (let ([g (with-random 42 (erdos-renyi 8 0.5))])
      (assert-true
       (let check ([i 0])
         (if (>= i 8) #t
             (let check-j ([j 0])
               (if (>= j 8) (check (+ i 1))
                   (if (= (matrix-ref g i j) (matrix-ref g j i))
                       (check-j (+ j 1))
                       #f))))))))

  (define-test "no self-loops"
    (let ([g (with-random 42 (erdos-renyi 10 0.5))])
      (assert-true
       (let check ([i 0])
         (if (>= i 10) #t
             (if (= (matrix-ref g i i) 0)
                 (check (+ i 1))
                 #f))))))

  (define-test "deterministic with same seed"
    (let ([g1 (with-random 123 (erdos-renyi 10 0.3))]
          [g2 (with-random 123 (erdos-renyi 10 0.3))])
      (assert-true
       (let check ([i 0])
         (if (>= i 10) #t
             (let check-j ([j 0])
               (if (>= j 10) (check (+ i 1))
                   (if (= (matrix-ref g1 i j) (matrix-ref g2 i j))
                       (check-j (+ j 1))
                       #f))))))))

  (define-test "different seeds produce different graphs"
    (let ([g1 (with-random 1 (erdos-renyi 20 0.5))]
          [g2 (with-random 2 (erdos-renyi 20 0.5))])
      ;; With 190 possible edges at p=0.5, identical graphs are vanishingly unlikely
      (assert-true
       (let check ([i 0])
         (if (>= i 20) #f  ;; if all same, fail
             (let check-j ([j (+ i 1)])
               (if (>= j 20) (check (+ i 1))
                   (if (not (= (matrix-ref g1 i j) (matrix-ref g2 i j)))
                       #t  ;; found a difference
                       (check-j (+ j 1))))))))))

  (define-test "approximate edge density for p=0.3"
    ;; Use a larger graph so the density is closer to p by LLN
    (let* ([g (with-random 42 (erdos-renyi 50 0.3))]
           [edge-count (adjacency-matrix-edge-count g)]
           ;; Each undirected edge counted twice in matrix
           [undirected-edges (/ edge-count 2)]
           [possible-edges (/ (* 50 49) 2)]
           [density (/ undirected-edges possible-edges)])
      ;; Should be roughly 0.3, allow +/- 0.1
      (assert-true (> density 0.15))
      (assert-true (< density 0.50))))

  (define-test "n=0 produces empty matrix"
    (let ([g (with-random 42 (erdos-renyi 0 0.5))])
      (assert-equal 0 (matrix-rows g))))

  (define-test "n=1 produces single-node graph with no edges"
    (let ([g (with-random 42 (erdos-renyi 1 1.0))])
      (assert-equal 1 (matrix-rows g))
      (assert-equal 0 (adjacency-matrix-edge-count g))))
)

;;; ============================================================================
;;; Barabasi-Albert Tests
;;; ============================================================================

(test-group "barabasi-albert"

  (define-test "produces correct node count"
    (let ([g (with-random 42 (barabasi-albert 20 3))])
      (assert-equal 20 (matrix-rows g))))

  (define-test "is symmetric (undirected)"
    (let ([g (with-random 42 (barabasi-albert 15 2))])
      (assert-true
       (let check ([i 0])
         (if (>= i 15) #t
             (let check-j ([j 0])
               (if (>= j 15) (check (+ i 1))
                   (if (= (matrix-ref g i j) (matrix-ref g j i))
                       (check-j (+ j 1))
                       #f))))))))

  (define-test "no self-loops"
    (let ([g (with-random 42 (barabasi-albert 15 2))])
      (assert-true
       (let check ([i 0])
         (if (>= i 15) #t
             (if (= (matrix-ref g i i) 0)
                 (check (+ i 1))
                 #f))))))

  (define-test "initial clique has correct edges"
    ;; With m=2, initial clique is K_3 (nodes 0,1,2), which has 3 undirected edges = 6 entries
    (let ([g (with-random 42 (barabasi-albert 3 2))])
      (assert-equal 6 (adjacency-matrix-edge-count g))))

  (define-test "each new node adds exactly m edges"
    ;; With m=2, n=10: initial K_3 has 3 edges, 7 new nodes add 2 each = 17 total
    (let* ([g (with-random 42 (barabasi-albert 10 2))]
           [edge-count (/ (adjacency-matrix-edge-count g) 2)])
      ;; K_3 = 3 edges, plus 7 nodes * 2 edges each = 17
      (assert-equal 17 edge-count)))

  (define-test "deterministic with same seed"
    (let ([g1 (with-random 99 (barabasi-albert 15 3))]
          [g2 (with-random 99 (barabasi-albert 15 3))])
      (assert-true
       (let check ([i 0])
         (if (>= i 15) #t
             (let check-j ([j 0])
               (if (>= j 15) (check (+ i 1))
                   (if (= (matrix-ref g1 i j) (matrix-ref g2 i j))
                       (check-j (+ j 1))
                       #f))))))))

  (define-test "graph is connected"
    ;; BA graphs are always connected by construction
    (let ([g (with-random 42 (barabasi-albert 20 2))])
      (assert-true (is-connected? g))))
)

;;; ============================================================================
;;; Watts-Strogatz Tests
;;; ============================================================================

(test-group "watts-strogatz"

  (define-test "produces correct node count"
    (let ([g (with-random 42 (watts-strogatz 20 4 0.3))])
      (assert-equal 20 (matrix-rows g))))

  (define-test "is symmetric (undirected)"
    (let ([g (with-random 42 (watts-strogatz 20 4 0.3))])
      (assert-true
       (let check ([i 0])
         (if (>= i 20) #t
             (let check-j ([j 0])
               (if (>= j 20) (check (+ i 1))
                   (if (= (matrix-ref g i j) (matrix-ref g j i))
                       (check-j (+ j 1))
                       #f))))))))

  (define-test "no self-loops"
    (let ([g (with-random 42 (watts-strogatz 20 4 0.3))])
      (assert-true
       (let check ([i 0])
         (if (>= i 20) #t
             (if (= (matrix-ref g i i) 0)
                 (check (+ i 1))
                 #f))))))

  (define-test "p=0 preserves ring lattice edge count"
    ;; With k=4, each node connects to 4 neighbors. Total undirected edges = n*k/2
    (let* ([g (with-random 42 (watts-strogatz 20 4 0.0))]
           [edge-count (/ (adjacency-matrix-edge-count g) 2)])
      (assert-equal 40 edge-count)))  ;; 20 * 4 / 2 = 40

  (define-test "p=0 produces regular ring lattice"
    ;; Each node should have exactly k=4 neighbors
    (let ([g (with-random 42 (watts-strogatz 20 4 0.0))])
      (assert-true
       (let check ([i 0])
         (if (>= i 20) #t
             (if (= (adjacency-out-degree g i) 4)
                 (check (+ i 1))
                 #f))))))

  (define-test "rewiring preserves approximate edge count"
    ;; Rewiring replaces edges, doesn't add or remove them, so total should stay ~ n*k/2
    ;; (May lose a few if rewiring targets an existing neighbor, which removes the old edge
    ;; but the new edge is a duplicate — but with n=20, k=4, this is rare)
    (let* ([g (with-random 42 (watts-strogatz 20 4 0.5))]
           [edge-count (/ (adjacency-matrix-edge-count g) 2)])
      ;; Should be close to 40, allow small variation
      (assert-true (>= edge-count 30))
      (assert-true (<= edge-count 50))))

  (define-test "deterministic with same seed"
    (let ([g1 (with-random 77 (watts-strogatz 15 4 0.3))]
          [g2 (with-random 77 (watts-strogatz 15 4 0.3))])
      (assert-true
       (let check ([i 0])
         (if (>= i 15) #t
             (let check-j ([j 0])
               (if (>= j 15) (check (+ i 1))
                   (if (= (matrix-ref g1 i j) (matrix-ref g2 i j))
                       (check-j (+ j 1))
                       #f))))))))

  (define-test "p=0 ring lattice is connected"
    (let ([g (with-random 42 (watts-strogatz 20 4 0.0))])
      (assert-true (is-connected? g))))
)

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(test-group "random-graphs-integration"

  (define-test "erdos-renyi graph works with dijkstra"
    (let ([g (with-random 42 (erdos-renyi 10 0.5))])
      ;; Should not error; result is a pair of vectors
      (let ([result (dijkstra g 0)])
        (assert-true (pair? result))
        (assert-equal 10 (vector-length (car result))))))

  (define-test "barabasi-albert graph works with connected-components"
    (let ([g (with-random 42 (barabasi-albert 15 2))])
      (let ([cc (connected-components g)])
        ;; BA graphs are connected, so single component
        (assert-equal 1 (num-communities cc)))))

  (define-test "watts-strogatz graph works with degree analysis"
    (let ([g (with-random 42 (watts-strogatz 20 4 0.0))])
      ;; Ring lattice: every node has degree 4
      (assert-equal 4 (adjacency-out-degree g 0))
      (assert-equal 4 (adjacency-out-degree g 10))))
)

(run-all-tests)
