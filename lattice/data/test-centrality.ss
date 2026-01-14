;;; lattice/data/test-centrality.ss — Graph Centrality Tests
;;;
;;; Tests for centrality measures:
;;;   - Eigenvector centrality
;;;   - Katz centrality
;;;   - Closeness centrality
;;;   - Betweenness centrality
;;;   - Utility functions
;;;
;;; Run from project root: scheme --script lattice/data/test-centrality.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-eigen.ss")
(load "lattice/data/graph-matrix.ss")
(load "lattice/data/centrality.ss")

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "✓"))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(define (test-approx name expected actual tolerance)
  (display "  ")
  (display name)
  (display ": ")
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "✓"))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)
       (display "\n    diff: ")
       (display (abs (- expected actual)))))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; Helper: check if vector contains non-negative values
(define (vec-non-negative? v)
  (let ([n (vector-length v)])
       (let loop ([i 0])
            (if (= i n)
                #t
                (if (< (vector-ref v i) 0)
                    #f
                    (loop (+ i 1)))))))

;;; Helper: find index of maximum value
(define (vec-argmax v)
  (let ([n (vector-length v)])
       (let loop ([i 1] [max-idx 0] [max-val (vector-ref v 0)])
            (if (= i n)
                max-idx
                (let ([val (vector-ref v i)])
                     (if (> val max-val)
                         (loop (+ i 1) i val)
                         (loop (+ i 1) max-idx max-val)))))))

;;; ====
;;; Test Graphs
;;; ====

;;; Star graph: node 0 is center, connected to 1, 2, 3, 4
(define star-adj (star-graph 5))

;;; Path graph: 0 -- 1 -- 2 -- 3 -- 4
(define path-adj (path-graph 5))

;;; Triangle: 0 -- 1 -- 2 -- 0
(define triangle-adj (edges->adjacency-matrix '((0 1) (1 2) (2 0)) 3 #t))

;;; Complete graph K4
(define k4-adj (complete-graph 4))

;;; Bow-tie graph: two triangles connected by a bridge node
;;; 0 -- 1 -- 2 (triangle 1)
;;;      |
;;;      3 (bridge)
;;;      |
;;; 4 -- 5 -- 6 (triangle 2)
(define bowtie-edges '((0 1) (1 2) (2 0)   ; triangle 1
                       (1 3)               ; bridge
                       (3 4)               ; bridge
                       (4 5) (5 6) (6 4))) ; triangle 2
(define bowtie-adj (edges->adjacency-matrix bowtie-edges 7 #t))

;;; ====
;;; Eigenvector Centrality Tests
;;; ====
(test-section "Eigenvector Centrality")

(define star-evc (eigenvector-centrality star-adj))

(test "star eigenvector centrality returns vector"
      #t
      (vector? star-evc))

(test "star eigenvector centrality is non-negative"
      #t
      (vec-non-negative? star-evc))

(test "star eigenvector centrality: center (0) has highest score"
      0
      (vec-argmax star-evc))

(define path-evc (eigenvector-centrality path-adj))

(test "path eigenvector centrality: middle node has highest score"
      2  ; Node 2 is the middle of path 0-1-2-3-4
      (vec-argmax path-evc))

(define k4-evc (eigenvector-centrality k4-adj))

(test "complete graph eigenvector centrality is uniform"
      #t
      (let ([e0 (vector-ref k4-evc 0)]
            [e1 (vector-ref k4-evc 1)]
            [e2 (vector-ref k4-evc 2)]
            [e3 (vector-ref k4-evc 3)])
           (and (< (abs (- e0 e1)) 0.01)
                (< (abs (- e1 e2)) 0.01)
                (< (abs (- e2 e3)) 0.01))))

;;; ====
;;; Katz Centrality Tests
;;; ====
(test-section "Katz Centrality")

(define star-katz (katz-centrality star-adj))

(test "star Katz centrality returns vector"
      #t
      (vector? star-katz))

(test "star Katz centrality: all nodes have positive score"
      #t
      (let ([n (vector-length star-katz)])
           (let loop ([i 0])
                (if (= i n)
                    #t
                    (if (<= (vector-ref star-katz i) 0)
                        #f
                        (loop (+ i 1)))))))

(test "star Katz centrality: center (0) has highest score"
      0
      (vec-argmax star-katz))

(define path-katz (katz-centrality path-adj))

(test "path Katz centrality: middle node has highest score"
      2
      (vec-argmax path-katz))

;;; Test with different alpha values
(define katz-high-alpha (katz-centrality star-adj 0.3))
(define katz-low-alpha (katz-centrality star-adj 0.01))

(test "higher alpha gives more spread in scores"
      #t
      (let ([high-max (vector-ref katz-high-alpha (vec-argmax katz-high-alpha))]
            [high-min (apply min (vector->list katz-high-alpha))]
            [low-max (vector-ref katz-low-alpha (vec-argmax katz-low-alpha))]
            [low-min (apply min (vector->list katz-low-alpha))])
           ;; Higher alpha should give more relative difference
           (> (/ high-max high-min) (/ low-max low-min))))

;;; ====
;;; Closeness Centrality Tests
;;; ====
(test-section "Closeness Centrality")

(define star-dist (floyd-warshall star-adj))
(define star-close (closeness-centrality star-dist))

(test "star closeness centrality returns vector"
      #t
      (vector? star-close))

(test "star closeness centrality: center (0) has highest score"
      0
      (vec-argmax star-close))

(define path-dist (floyd-warshall path-adj))
(define path-close (closeness-centrality path-dist))

(test "path closeness centrality: middle node has highest score"
      2
      (vec-argmax path-close))

;;; Harmonic closeness for disconnected graph
(define disconn-edges '((0 1) (2 3)))  ; Two separate edges
(define disconn-adj (edges->adjacency-matrix disconn-edges 4 #t))
(define disconn-dist (floyd-warshall disconn-adj))

(define harmonic-close (closeness-centrality disconn-dist #t))

(test "harmonic closeness handles disconnected graphs"
      #t
      (and (vector? harmonic-close)
           (vec-non-negative? harmonic-close)))

;;; ====
;;; Betweenness Centrality Tests
;;; ====
(test-section "Betweenness Centrality")

(define star-btwn (betweenness-centrality star-adj))

(test "star betweenness centrality returns vector"
      #t
      (vector? star-btwn))

(test "star betweenness centrality: center (0) has highest score"
      0
      (vec-argmax star-btwn))

(test "star betweenness: leaf nodes have zero betweenness"
      #t
      (let ([b1 (vector-ref star-btwn 1)]
            [b2 (vector-ref star-btwn 2)]
            [b3 (vector-ref star-btwn 3)]
            [b4 (vector-ref star-btwn 4)])
           ;; Leaves should have ~0 betweenness (they're endpoints)
           (and (< b1 0.01) (< b2 0.01) (< b3 0.01) (< b4 0.01))))

(define path-btwn (betweenness-centrality path-adj))

(test "path betweenness: middle node has highest score"
      2
      (vec-argmax path-btwn))

(test "path betweenness: endpoints have zero betweenness"
      #t
      (let ([b0 (vector-ref path-btwn 0)]
            [b4 (vector-ref path-btwn 4)])
           (and (< b0 0.01) (< b4 0.01))))

;;; Bow-tie graph: bridge nodes should have high betweenness
(define bowtie-btwn (betweenness-centrality bowtie-adj))

(test "bow-tie betweenness: bridge nodes (1, 3, 4) have higher scores"
      #t
      (let ([b0 (vector-ref bowtie-btwn 0)]
            [b1 (vector-ref bowtie-btwn 1)]
            [b3 (vector-ref bowtie-btwn 3)]
            [b5 (vector-ref bowtie-btwn 5)])
           ;; Bridge nodes should have higher betweenness than non-bridges
           (and (> b1 b0) (> b3 b0) (> b3 b5))))

;;; ====
;;; Utility Function Tests
;;; ====
(test-section "Utility Functions")

;;; rank-by-centrality
(define ranked (rank-by-centrality star-evc))

(test "rank-by-centrality returns list of pairs"
      #t
      (and (list? ranked)
           (pair? (car ranked))))

(test "rank-by-centrality: first is highest"
      0
      (car (car ranked)))  ; Node 0 should be first

;;; top-k-central
(define top2 (top-k-central star-evc 2))

(test "top-k-central returns k elements"
      2
      (length top2))

(test "top-k-central: center is in top 2"
      #t
      (if (member 0 (map car top2)) #t #f))

;;; centrality-correlation
(define evc-katz-corr (centrality-correlation star-evc star-katz))

(test "centrality correlation is between -1 and 1"
      #t
      (and (>= evc-katz-corr -1.001) (<= evc-katz-corr 1.001)))  ; Allow small floating point error

(test "eigenvector and Katz are positively correlated on star"
      #t
      (> evc-katz-corr 0.5))

;;; ====
;;; Edge Case Tests
;;; ====
(test-section "Edge Cases")

;;; Single node
(define single-adj (make-matrix 1 1 0))
(define single-evc (eigenvector-centrality single-adj))

(test "single node eigenvector centrality works"
      #t
      (vector? single-evc))

;;; Two connected nodes
(define pair-adj (edges->adjacency-matrix '((0 1)) 2 #t))
(define pair-evc (eigenvector-centrality pair-adj))

(test "pair graph eigenvector centrality is uniform"
      #t
      (< (abs (- (vector-ref pair-evc 0)
                 (vector-ref pair-evc 1)))
         0.01))

;;; Complete bipartite K_{2,3}
(define k23-adj (bipartite-graph 2 3 '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2))))
(define k23-evc (eigenvector-centrality k23-adj))

(test "bipartite K_{2,3} eigenvector centrality works"
      #t
      (vector? k23-evc))

;;; ====
;;; Summary
;;; ====
(newline)
(display "====")
(newline)
(display "Tests passed: ")
(display *tests-passed*)
(newline)
(display "Tests failed: ")
(display *tests-failed*)
(newline)
(display "====")
(newline)

(when (> *tests-failed* 0)
      (exit 1))
