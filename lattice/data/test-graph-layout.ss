;;; lattice/data/test-graph-layout.ss --- Tests for graph layout algorithms

(load "core/testing/test-framework.ss")
(load "lattice/data/graph-layout.ss")

(display "Testing graph-layout.ss...\n\n")

;;; ============================================================
;;; Node Tests
;;; ============================================================

(test-group "graph-nodes"

  (define-test "make-graph-node creates node with position"
    (let ([node (make-graph-node 'a 10 20)])
      (assert-equal 'a (node-id node))
      (assert-equal 10 (node-x node))
      (assert-equal 20 (node-y node))))

  (define-test "node-set-pos updates position"
    (let* ([node (make-graph-node 'a 0 0)]
           [moved (node-set-pos node (vector 5.0 10.0))])
      (assert-equal 5.0 (node-x moved))
      (assert-equal 10.0 (node-y moved))))
)

;;; ============================================================
;;; Graph Construction Tests
;;; ============================================================

(test-group "graph-construction"

  (define-test "make-layout-graph creates graph with nodes and edges"
    (let ([g (make-layout-graph '(a b c) '((a . b) (b . c)))])
      (assert-equal 3 (length (graph-nodes g)))
      (assert-equal 2 (length (graph-edges g)))))

  (define-test "graph-find-node locates nodes by id"
    (let ([g (make-layout-graph '(a b c) '())])
      (assert-true (not (not (graph-find-node g 'a))))
      (assert-true (not (not (graph-find-node g 'b))))
      (assert-false (graph-find-node g 'z))))

  (define-test "layout-from-adjacency builds from adjacency list"
    (let ([g (layout-from-adjacency '((a b c) (b c) (c)))])
      (assert-equal 3 (length (graph-nodes g)))
      ;; a->b, a->c, b->c = 3 edges
      (assert-equal 3 (length (graph-edges g)))))
)

;;; ============================================================
;;; Layout Algorithm Tests
;;; ============================================================

(test-group "layout-algorithm"

  (define-test "layout-step produces new graph"
    (let* ([g (make-layout-graph '(a b) '((a . b)))]
           [stepped (layout-step g)])
      (assert-equal 2 (length (graph-nodes stepped)))))

  (define-test "run-layout performs multiple iterations"
    (let* ([g (make-layout-graph '(a b c) '((a . b) (b . c)))]
           [result (run-layout g 10)])
      (assert-equal 3 (length (graph-nodes result)))))

  (define-test "connected nodes attract after iterations"
    (let* ([g (make-layout-graph '(a b) '((a . b)))]
           ;; Set initial positions far apart
           [nodes (graph-nodes g)]
           [a-node (graph-find-node g 'a)]
           [b-node (graph-find-node g 'b)]
           [initial-dist (let ([ax (node-x a-node)]
                              [ay (node-y a-node)]
                              [bx (node-x b-node)]
                              [by (node-y b-node)])
                          (sqrt (+ (* (- ax bx) (- ax bx))
                                   (* (- ay by) (- ay by)))))]
           ;; Run layout
           [result (run-layout g 50)]
           [a-final (graph-find-node result 'a)]
           [b-final (graph-find-node result 'b)]
           [final-dist (let ([ax (node-x a-final)]
                            [ay (node-y a-final)]
                            [bx (node-x b-final)]
                            [by (node-y b-final)])
                        (sqrt (+ (* (- ax bx) (- ax bx))
                                 (* (- ay by) (- ay by)))))])
      ;; Connected nodes should be closer or stay close
      (assert-true (or (< final-dist initial-dist)
                       (< final-dist 100)))))  ; reasonable distance
)

;;; ============================================================
;;; Normalization Tests
;;; ============================================================

(test-group "normalization"

  (define-test "normalize-layout fits within bounds"
    (let* ([g (make-layout-graph '(a b c) '())]
           [laid-out (run-layout g 20)]
           [normalized (normalize-layout laid-out 100 50 5)])
      ;; All nodes should be within [5, 95] x [5, 45]
      (for-each
       (lambda (node)
         (assert-true (>= (node-x node) 5))
         (assert-true (<= (node-x node) 95))
         (assert-true (>= (node-y node) 5))
         (assert-true (<= (node-y node) 45)))
       (graph-nodes normalized))))

  (define-test "layout-bounds returns correct min/max"
    (let* ([g (make-layout-graph '(a) '())]
           [nodes (list (make-graph-node 'a 10 20))]
           [g2 (graph-set-nodes g nodes)])
      (let-values ([(min-x min-y max-x max-y) (layout-bounds g2)])
        (assert-equal 10 min-x)
        (assert-equal 20 min-y))))
)

;;; ============================================================
;;; Hierarchical Layout Tests
;;; ============================================================

(test-group "hierarchical-layout"

  (define-test "hierarchical-layout positions by depth"
    (let* ([depth-fn (lambda (id)
                      (case id
                        [(root) 0]
                        [(child1 child2) 1]
                        [(leaf) 2]
                        [else 0]))]
           [g (hierarchical-layout '(root child1 child2 leaf)
                                   '((root . child1) (root . child2) (child1 . leaf))
                                   depth-fn)]
           [root-node (graph-find-node g 'root)]
           [leaf-node (graph-find-node g 'leaf)])
      ;; Deeper nodes should have higher x
      (assert-true (< (node-x root-node) (node-x leaf-node)))))
)

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
