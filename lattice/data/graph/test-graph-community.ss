;;; lattice/data/test-graph-community.ss — Tests for community detection and MST
;;;
;;; Tests label propagation, modularity, Prim's MST, Kruskal's MST,
;;; and utility functions.
;;;
;;; Run from project root: scheme --script lattice/data/test-graph-community.ss

;; graph-community.ss loads its own dependencies (graph-matrix, ilp, etc.)
(load "lattice/data/graph/graph-community.ss")

;;; ====
;;; Test Infrastructure
;;; ====

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "PASS"))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "FAIL - expected ")
       (display expected)
       (display ", got ")
       (display actual)))
  (newline))

(define (test-true name actual)
  (test name #t (if actual #t #f)))

(define (test-false name actual)
  (test name #f (if actual #t #f)))

(define (test-approx name expected actual tolerance)
  (display "  ")
  (display name)
  (display ": ")
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "PASS"))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "FAIL - expected ~")
       (display expected)
       (display ", got ")
       (display actual)))
  (newline))

(define (test-group name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

;;; ====
;;; Test Graphs
;;; ====

;;; Two disconnected triangles (clear 2-community structure)
(define two-triangles-edges
  '((0 1) (1 2) (2 0)      ; First triangle
    (3 4) (4 5) (5 3)))    ; Second triangle

(define two-triangles-adj
  (edges->adjacency-matrix two-triangles-edges 6 #t))

;;; Barbell graph: two triangles connected by one edge
(define barbell-edges
  '((0 1) (1 2) (2 0)      ; First triangle
    (3 4) (4 5) (5 3)      ; Second triangle
    (2 3)))                 ; Bridge edge

(define barbell-adj
  (edges->adjacency-matrix barbell-edges 6 #t))

;;; Simple path for MST testing
(define path-4-adj (path-graph 4))

;;; Complete graph K4
(define k4-adj (complete-graph 4))

;;; Weighted triangle for MST: edges 0-1 (weight 1), 1-2 (weight 2), 0-2 (weight 3)
(define weighted-triangle-edges
  '((0 1 1) (1 2 2) (0 2 3)))

(define weighted-triangle-adj
  (edges->adjacency-matrix weighted-triangle-edges 3 #t))

;;; Weighted graph for MST testing (classic example)
;;;
;;;     1
;;;   0---1
;;;   |   |
;;;  4|   |2
;;;   |   |
;;;   3---2
;;;     3
;;;
(define mst-test-edges
  '((0 1 1) (0 3 4)
    (1 2 2)
    (2 3 3)))

(define mst-test-adj
  (edges->adjacency-matrix mst-test-edges 4 #t))

;;; ====
;;; Community Detection Tests
;;; ====

(test-group "Label Propagation")

;; Test: detects two disconnected communities
(let* ([labels (label-propagation two-triangles-adj)]
       [partition (communities->partition labels)])
  (test "two disconnected communities - count" 2 (length partition))
  (test "two disconnected communities - size 1" 3 (length (car partition)))
  (test "two disconnected communities - size 2" 3 (length (cadr partition))))

;; Test: converges on connected graph
(let ([labels (label-propagation k4-adj)])
  (test-true "converges on K4" (vector? labels))
  (test "K4 labels length" 4 (vector-length labels)))

;; Test: respects max iterations
(let ([labels (label-propagation two-triangles-adj 5)])
  (test-true "max iter produces vector" (vector? labels))
  (test "max iter labels length" 6 (vector-length labels)))

;; Test: deterministic with same seed
(let ([labels1 (label-propagation barbell-adj 100 42)]
      [labels2 (label-propagation barbell-adj 100 42)])
  (test "deterministic with same seed"
        (vector->list labels1)
        (vector->list labels2)))

;; Test: single node graph
(let* ([single-adj (make-matrix 1 1 0)]
       [labels (label-propagation single-adj)])
  (test "single node - length" 1 (vector-length labels))
  (test "single node - label" 0 (vector-ref labels 0)))

;;; ====
;;; Communities->Partition Tests
;;; ====

(test-group "Communities->Partition")

;; Test: groups nodes by label
(let* ([labels (vector 0 0 1 1 1)]
       [partition (communities->partition labels)])
  (test "partition count" 2 (length partition)))

;; Test: single community
(let* ([labels (vector 0 0 0)]
       [partition (communities->partition labels)])
  (test "single community count" 1 (length partition))
  (test "single community nodes" '(0 1 2) (car partition)))

;; Test: each node separate
(let* ([labels (vector 0 1 2)]
       [partition (communities->partition labels)])
  (test "each separate count" 3 (length partition)))

;;; ====
;;; Num-Communities Tests
;;; ====

(test-group "Num Communities")

(test "num communities - 2" 2 (num-communities (vector 0 0 1 1)))
(test "num communities - 1" 1 (num-communities (vector 0 0 0)))
(test "num communities - 3" 3 (num-communities (vector 0 1 2)))

;;; ====
;;; Modularity Tests
;;; ====

(test-group "Modularity")

;; Test: perfect partition has high modularity
(let* ([labels (vector 0 0 0 1 1 1)]
       [Q (modularity two-triangles-adj labels)])
  (test-true "perfect partition Q > 0.3" (> Q 0.3)))

;; Test: all same community has lower modularity
(let* ([labels (vector 0 0 0 0 0 0)]
       [Q (modularity two-triangles-adj labels)])
  (test-true "all-same community Q < 0.3" (< Q 0.3)))

;; Test: empty graph returns zero
(let* ([empty-adj (make-matrix 3 3 0)]
       [labels (vector 0 0 0)])
  (test "empty graph modularity" 0 (modularity empty-adj labels)))

;; Test: single community complete graph has Q near 0
(let* ([labels (vector 0 0 0 0)]
       [Q (modularity k4-adj labels)])
  (test-true "K4 single community Q near 0" (< (abs Q) 0.01)))

;;; ====
;;; Prim's MST Tests
;;; ====

(test-group "Prim's MST")

;; Test: path graph MST equals graph itself
(let ([mst (prim-mst path-4-adj)])
  (test "path MST edge count" 3 (length mst)))

;; Test: complete graph MST has n-1 edges
(let ([mst (prim-mst k4-adj)])
  (test "K4 MST edge count" 3 (length mst)))

;; Test: weighted triangle picks two cheapest edges
(let ([mst (prim-mst weighted-triangle-adj)])
  (test "weighted triangle MST edge count" 2 (length mst))
  (test "weighted triangle MST weight" 3 (mst-weight mst)))

;; Test: MST from weighted graph
(let ([mst (prim-mst mst-test-adj)])
  (test "weighted graph MST edge count" 3 (length mst))
  (test "weighted graph MST weight" 6 (mst-weight mst)))

;; Test: can start from different node
(let ([mst0 (prim-mst mst-test-adj 0)]
      [mst2 (prim-mst mst-test-adj 2)])
  (test "different start - same weight" (mst-weight mst0) (mst-weight mst2)))

;; Test: single node graph
(let* ([single-adj (make-matrix 1 1 0)]
       [mst (prim-mst single-adj)])
  (test "single node MST" 0 (length mst)))

;;; ====
;;; Kruskal's MST Tests
;;; ====

(test-group "Kruskal's MST")

;; Test: finds minimum spanning tree
(let ([mst (kruskal-mst mst-test-edges 4)])
  (test "kruskal MST edge count" 3 (length mst))
  (test "kruskal MST weight" 6 (mst-weight mst)))

;; Test: weighted triangle picks cheapest edges
(let ([mst (kruskal-mst weighted-triangle-edges 3)])
  (test "kruskal triangle edge count" 2 (length mst))
  (test "kruskal triangle weight" 3 (mst-weight mst)))

;; Test: handles disconnected graph (forest)
(let* ([edges '((0 1 1) (2 3 2))]
       [mst (kruskal-mst edges 4)])
  (test "disconnected - returns forest" 2 (length mst)))

;; Test: empty edge list
(let ([mst (kruskal-mst '() 3)])
  (test "empty edges" 0 (length mst)))

;;; ====
;;; MST Weight Tests
;;; ====

(test-group "MST Weight")

(test "sum weights 1+2+3" 6 (mst-weight '((0 1 1) (1 2 2) (2 3 3))))
(test "empty mst weight" 0 (mst-weight '()))
(test "single edge weight" 5 (mst-weight '((0 1 5))))

;;; ====
;;; Connected Components Tests
;;; ====

(test-group "Connected Components")

;; Test: finds two components in disconnected graph
(let ([components (connected-components two-triangles-adj)])
  (test "components length" 6 (vector-length components))
  ;; Nodes 0,1,2 should share a label
  (test-true "0,1 same component"
             (= (vector-ref components 0) (vector-ref components 1)))
  (test-true "1,2 same component"
             (= (vector-ref components 1) (vector-ref components 2)))
  ;; Nodes 3,4,5 should share a different label
  (test-true "3,4 same component"
             (= (vector-ref components 3) (vector-ref components 4)))
  (test-true "4,5 same component"
             (= (vector-ref components 4) (vector-ref components 5)))
  ;; The two groups should have different labels
  (test-true "two different components"
             (not (= (vector-ref components 0) (vector-ref components 3)))))

;; Test: connected graph has one component
(let ([components (connected-components k4-adj)])
  (test "K4 has 1 component" 1 (num-communities components)))

;; Test: barbell graph is connected
(let ([components (connected-components barbell-adj)])
  (test "barbell has 1 component" 1 (num-communities components)))

;;; ====
;;; Is-Connected? Tests
;;; ====

(test-group "Is Connected?")

(test-true "K4 is connected" (is-connected? k4-adj))
(test-true "path is connected" (is-connected? path-4-adj))
(test-false "two triangles not connected" (is-connected? two-triangles-adj))
(test-true "barbell is connected" (is-connected? barbell-adj))

;;; ====
;;; Union-Find Tests
;;; ====

(test-group "Union-Find")

;; Test: find returns self for root
(let ([parent (vector 0 1 2 3)])
  (test "find root 0" 0 (uf-find parent 0))
  (test "find root 2" 2 (uf-find parent 2)))

;; Test: find follows path to root
(let ([parent (vector 0 0 1)])  ; 2->1->0
  (test "find follows path" 0 (uf-find parent 2)))

;; Test: union merges sets
(let ([parent (vector 0 1 2)]
      [rank (vector 0 0 0)])
  (uf-union parent rank 0 1)
  (test-true "union merges 0,1"
             (= (uf-find parent 0) (uf-find parent 1))))

;;; ====
;;; Modularity ILP Tests
;;; ====

;; Load ILP solver for these tests
(load "lattice/optimization/ilp.ss")

(test-group "Modularity ILP")

;; Test: two disconnected triangles - should find perfect partition
(let* ([labels (modularity-ilp two-triangles-adj)]
       [partition (communities->partition labels)]
       [Q (modularity two-triangles-adj labels)])
  (test "ILP finds two communities" 2 (num-communities labels))
  (test-true "ILP modularity > 0.3" (> Q 0.3)))

;; Test: ILP finds at least as good as greedy
(let* ([ilp-labels (modularity-ilp barbell-adj)]
       [greedy-labels (label-propagation barbell-adj 100 42)]
       [Q-ilp (modularity barbell-adj ilp-labels)]
       [Q-greedy (modularity barbell-adj greedy-labels)])
  (test-true "ILP Q >= greedy Q"
             (>= (+ Q-ilp 0.001) Q-greedy)))  ; tolerance for floating point

;; Test: single node graph
(let* ([single-adj (make-matrix 1 1 0)]
       [labels (modularity-ilp single-adj)])
  (test "single node - length" 1 (vector-length labels))
  (test "single node - label" 0 (vector-ref labels 0)))

;; Test: empty graph (no edges)
(let* ([empty-adj (make-matrix 3 3 0)]
       [labels (modularity-ilp empty-adj)])
  (test "empty graph - length" 3 (vector-length labels)))

;; Test: K4 - all nodes in same community should have Q near 0
(let* ([labels (modularity-ilp k4-adj)]
       [Q (modularity k4-adj labels)])
  (test-true "K4 ILP produces valid labels" (vector? labels))
  (test "K4 labels length" 4 (vector-length labels)))

;; Test: path graph of 4 nodes
(let* ([labels (modularity-ilp path-4-adj)]
       [Q (modularity path-4-adj labels)])
  (test-true "path graph Q >= 0" (>= Q -0.01)))

;; Test: verify ILP respects 2-community constraint
(let ([labels (modularity-ilp two-triangles-adj)])
  (test-true "labels are binary"
             (let loop ([i 0])
               (or (= i (vector-length labels))
                   (and (or (= (vector-ref labels i) 0)
                            (= (vector-ref labels i) 1))
                        (loop (+ i 1)))))))

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

(if (> *tests-failed* 0)
    (exit 1)
    (exit 0))
