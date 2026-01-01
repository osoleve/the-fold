;;; fabric/stitches/fp/test-graph-matrix.ss — Tests for Graph Matrix Representations

(load "core/test-framework.ss")
(load "core/fp/instances/graph-matrix.ss")

(display "\n")
(display "==============================================================\n")
(display "         GRAPH MATRIX REPRESENTATION TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Adjacency Matrix Tests
;;; ============================================================

(test-group adjacency-matrix-basic
            (define-test empty-graph-adjacency-test
              (let* ([g (make-graph #t)]
                     [m (graph->adjacency-matrix g)])
                    (assert-equal 0 (matrix-rows m))
                    (assert-equal 0 (matrix-cols m))))
            
            (define-test single-vertex-adjacency-test
              (let* ([g (graph-add-vertex (make-graph #t) 'a)]
                     [m (graph->adjacency-matrix g)])
                    (assert-equal 1 (matrix-rows m))
                    (assert-equal 1 (matrix-cols m))
                    (assert-equal 0 (matrix-ref m 0 0))))
            
            (define-test simple-directed-graph-test
              ;; Graph: a -> b
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertices g '(a b))]
                     [g (graph-add-edge g 'a 'b 1)]
                     [m (graph->adjacency-matrix g)])
                    (assert-equal 2 (matrix-rows m))
                    (assert-equal 2 (matrix-cols m))
                    ;; a is at index 0, b is at index 1
                    (assert-equal 1 (matrix-ref m 0 1))  ; a -> b
                    (assert-equal 0 (matrix-ref m 1 0))  ; b -/-> a
                    (assert-equal 0 (matrix-ref m 0 0))  ; No self-loop
                    (assert-equal 0 (matrix-ref m 1 1))))
            
            (define-test simple-undirected-graph-test
              ;; Graph: a -- b
              (let* ([g (make-graph #f)]
                     [g (graph-add-vertices g '(a b))]
                     [g (graph-add-edge g 'a 'b 1)]
                     [m (graph->adjacency-matrix g)])
                    ;; Undirected edge creates both directions
                    (assert-equal 1 (matrix-ref m 0 1))  ; a -- b
                    (assert-equal 1 (matrix-ref m 1 0))  ; b -- a
                    (assert-true (adjacency-matrix-is-symmetric? m))))
            
            (define-test triangle-graph-test
              ;; Graph: a -> b -> c -> a
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertices g '(a b c))]
                     [g (graph-add-edge g 'a 'b 1)]
                     [g (graph-add-edge g 'b 'c 1)]
                     [g (graph-add-edge g 'c 'a 1)]
                     [m (graph->adjacency-matrix g)])
                    (assert-equal 3 (matrix-rows m))
                    (assert-equal 3 (matrix-cols m))
                    (assert-equal 1 (matrix-ref m 0 1))  ; a -> b
                    (assert-equal 1 (matrix-ref m 1 2))  ; b -> c
                    (assert-equal 1 (matrix-ref m 2 0))  ; c -> a
                    ;; Check no reverse edges
                    (assert-equal 0 (matrix-ref m 1 0))
                    (assert-equal 0 (matrix-ref m 2 1))
                    (assert-equal 0 (matrix-ref m 0 2)))))

(test-group weighted-adjacency-matrix
            (define-test weighted-directed-graph-test
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertices g '(a b c))]
                     [g (graph-add-edge g 'a 'b 2.5)]
                     [g (graph-add-edge g 'b 'c 3.7)]
                     [m (graph->adjacency-matrix g)])
                    (assert-equal 2.5 (matrix-ref m 0 1))
                    (assert-equal 3.7 (matrix-ref m 1 2))
                    (assert-equal 0 (matrix-ref m 2 0))))
            
            (define-test unweighted-conversion-test
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertices g '(a b c))]
                     [g (graph-add-edge g 'a 'b 5)]
                     [g (graph-add-edge g 'b 'c 10)]
                     [m (graph->adjacency-matrix-unweighted g)])
                    ;; All edges become 1
                    (assert-equal 1 (matrix-ref m 0 1))
                    (assert-equal 1 (matrix-ref m 1 2))
                    (assert-equal 0 (matrix-ref m 2 0)))))

;;; ============================================================
;;; Degree Matrix Tests
;;; ============================================================

(test-group degree-matrix
            (define-test degree-matrix-simple-test
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertices g '(a b c))]
                     [g (graph-add-edge g 'a 'b 1)]
                     [g (graph-add-edge g 'a 'c 1)]
                     [d (graph->degree-matrix g)])
                    (assert-equal 3 (matrix-rows d))
                    ;; a has out-degree 2
                    (assert-equal 2 (matrix-ref d 0 0))
                    ;; b and c have out-degree 0
                    (assert-equal 0 (matrix-ref d 1 1))
                    (assert-equal 0 (matrix-ref d 2 2))
                    ;; Off-diagonal elements are 0
                    (assert-equal 0 (matrix-ref d 0 1))
                    (assert-equal 0 (matrix-ref d 1 0))))
            
            (define-test degree-matrix-weighted-test
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertices g '(a b))]
                     [g (graph-add-edge g 'a 'b 3)]
                     [g (graph-add-edge g 'a 'a 2)]  ; Self-loop
                     [d (graph->degree-matrix g)])
                    ;; Degree is sum of edge weights
                    (assert-equal 5 (matrix-ref d 0 0))  ; 3 + 2
                    (assert-equal 0 (matrix-ref d 1 1))))
            
            (define-test degree-matrix-unweighted-test
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertices g '(a b c))]
                     [g (graph-add-edge g 'a 'b 5)]
                     [g (graph-add-edge g 'a 'c 10)]
                     [d (graph->degree-matrix-unweighted g)])
                    ;; Degree is count of edges
                    (assert-equal 2 (matrix-ref d 0 0))
                    (assert-equal 0 (matrix-ref d 1 1)))))

;;; ============================================================
;;; Incidence Matrix Tests
;;; ============================================================

(test-group incidence-matrix
            (define-test incidence-matrix-directed-test
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertices g '(a b c))]
                     [g (graph-add-edge g 'a 'b 1)]
                     [g (graph-add-edge g 'b 'c 1)]
                     [b (graph->incidence-matrix g)])
                    (assert-equal 3 (matrix-rows b))  ; 3 vertices
                    (assert-equal 2 (matrix-cols b))  ; 2 edges
                    ;; First edge: a -> b
                    ;; a is source (-1), b is target (+1)
                    (let ([col0-a (matrix-ref b 0 0)]
                          [col0-b (matrix-ref b 1 0)]
                          [col0-c (matrix-ref b 2 0)])
                         ;; One should be -1 (source), one +1 (target)
                         (assert-true (or (and (= col0-a -1) (= col0-b 1))
                                          (and (= col0-a 1) (= col0-b -1)))))))
            
            (define-test incidence-matrix-undirected-test
              (let* ([g (make-graph #f)]
                     [g (graph-add-vertices g '(a b))]
                     [g (graph-add-edge g 'a 'b 1)]
                     [b (graph->incidence-matrix g)])
                    (assert-equal 2 (matrix-rows b))
                    (assert-equal 1 (matrix-cols b))
                    ;; Both endpoints have +1
                    (assert-equal 1 (matrix-ref b 0 0))
                    (assert-equal 1 (matrix-ref b 1 0)))))

;;; ============================================================
;;; Matrix to Graph Conversion Tests
;;; ============================================================

(test-group matrix-to-graph
            (define-test matrix-to-graph-basic-test
              (let* ([m (matrix-from-lists '((0 1 0)
                                             (0 0 1)
                                             (1 0 0)))]
                     [vertices '(a b c)]
                     [g (adjacency-matrix->graph m #t vertices)])
                    (assert-true (graph? g))
                    (assert-equal 3 (length (graph-vertices g)))
                    ;; Check edges: a->b, b->c, c->a
                    (assert-true (has-edge? m 0 1))
                    (assert-true (has-edge? m 1 2))
                    (assert-true (has-edge? m 2 0))))
            
            (define-test matrix-to-graph-roundtrip-test
              (let* ([g1 (make-graph #t)]
                     [g1 (graph-add-vertices g1 '(x y z))]
                     [g1 (graph-add-edge g1 'x 'y 1)]
                     [g1 (graph-add-edge g1 'y 'z 1)]
                     [m (graph->adjacency-matrix g1)]
                     [g2 (adjacency-matrix->graph m #t '(x y z))]
                     [m2 (graph->adjacency-matrix g2)])
                    ;; Matrices should be equal
                    (assert-equal (matrix-rows m) (matrix-rows m2))
                    (assert-equal (matrix-cols m) (matrix-cols m2))
                    (assert-equal (matrix-ref m 0 1) (matrix-ref m2 0 1))
                    (assert-equal (matrix-ref m 1 2) (matrix-ref m2 1 2)))))

;;; ============================================================
;;; Edge Queries Tests
;;; ============================================================

(test-group edge-queries
            (define-test has-edge-test
              (let* ([m (matrix-from-lists '((0 5 0)
                                             (0 0 3)
                                             (0 0 0)))])
                    (assert-true (has-edge? m 0 1))
                    (assert-true (has-edge? m 1 2))
                    (assert-false (has-edge? m 2 0))
                    (assert-false (has-edge? m 0 2))))
            
            (define-test get-edge-weight-test
              (let* ([m (matrix-from-lists '((0 2.5 0)
                                             (0 0 3.7)
                                             (0 0 0)))])
                    (assert-equal 2.5 (get-edge-weight m 0 1))
                    (assert-equal 3.7 (get-edge-weight m 1 2))
                    (assert-equal 0 (get-edge-weight m 2 0)))))

;;; ============================================================
;;; Symmetry Tests
;;; ============================================================

(test-group symmetry
            (define-test undirected-graph-symmetric-test
              (let* ([g (make-graph #f)]
                     [g (graph-add-vertices g '(a b c))]
                     [g (graph-add-edge g 'a 'b 1)]
                     [g (graph-add-edge g 'b 'c 1)]
                     [m (graph->adjacency-matrix g)])
                    (assert-true (adjacency-matrix-is-symmetric? m))))
            
            (define-test directed-graph-not-symmetric-test
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertices g '(a b))]
                     [g (graph-add-edge g 'a 'b 1)]
                     [m (graph->adjacency-matrix g)])
                    (assert-false (adjacency-matrix-is-symmetric? m)))))

;;; ============================================================
;;; Complex Graph Tests
;;; ============================================================

(test-group complex-graphs
            (define-test complete-graph-test
              ;; K3: complete graph on 3 vertices
              (let* ([g (make-graph #f)]
                     [g (graph-add-vertices g '(a b c))]
                     [g (graph-add-edge g 'a 'b 1)]
                     [g (graph-add-edge g 'a 'c 1)]
                     [g (graph-add-edge g 'b 'c 1)]
                     [m (graph->adjacency-matrix g)])
                    ;; All off-diagonal elements should be 1
                    (assert-equal 0 (matrix-ref m 0 0))  ; No self-loops
                    (assert-equal 1 (matrix-ref m 0 1))
                    (assert-equal 1 (matrix-ref m 0 2))
                    (assert-equal 1 (matrix-ref m 1 0))
                    (assert-equal 0 (matrix-ref m 1 1))
                    (assert-equal 1 (matrix-ref m 1 2))
                    (assert-equal 1 (matrix-ref m 2 0))
                    (assert-equal 1 (matrix-ref m 2 1))
                    (assert-equal 0 (matrix-ref m 2 2))))
            
            (define-test self-loop-test
              (let* ([g (make-graph #t)]
                     [g (graph-add-vertex g 'a)]
                     [g (graph-add-edge g 'a 'a 5)]
                     [m (graph->adjacency-matrix g)])
                    (assert-equal 5 (matrix-ref m 0 0)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All graph-matrix tests passed.\n")
    (display "\n[FAILURE] Some graph-matrix tests failed.\n"))
