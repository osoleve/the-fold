;;; lattice/data/test-graph-layout.ss --- Tests for graph layout algorithms

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/data/graph/graph-layout.ss")

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

  (define-test "connected nodes converge to stable distance"
    (let* ([g (make-layout-graph '(a b) '((a . b)))]
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
      ;; Connected nodes should settle to reasonable distance (not infinite)
      (assert-true (< final-dist 500))))
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
;;; Edge Case Tests (from Gemini QA)
;;; ============================================================

(test-group "edge-cases"

  (define-test "empty graph handles gracefully"
    (let* ([g (make-layout-graph '() '())]
           [stepped (layout-step g)]
           [laid-out (run-layout g 10)])
      (assert-equal 0 (length (graph-nodes stepped)))
      (assert-equal 0 (length (graph-nodes laid-out)))))

  (define-test "single node graph is stable"
    (let* ([g (make-layout-graph '(solo) '())]
           [result (run-layout g 20)]
           [node (graph-find-node result 'solo)])
      ;; Single node should stay near origin due to gravity
      (assert-true (< (abs (node-x node)) 200))
      (assert-true (< (abs (node-y node)) 200))))

  (define-test "disconnected components don't fly to infinity"
    (let* ([g (make-layout-graph '(a b c d) '((a . b)))]  ; c,d disconnected
           [result (run-layout g 50)])
      ;; All nodes should stay bounded due to central gravity
      (for-each
       (lambda (node)
         (assert-true (< (abs (node-x node)) 500))
         (assert-true (< (abs (node-y node)) 500)))
       (graph-nodes result))))

  (define-test "overlapping nodes separate"
    ;; Create two nodes at exact same position
    (let* ([g (list 'layout-graph
                    (list (make-graph-node 'a 0 0)
                          (make-graph-node 'b 0 0))
                    '())]
           [result (run-layout g 30)]
           [a (graph-find-node result 'a)]
           [b (graph-find-node result 'b)]
           [dist (sqrt (+ (expt (- (node-x a) (node-x b)) 2)
                          (expt (- (node-y a) (node-y b)) 2)))])
      ;; Nodes should have separated
      (assert-true (> dist 1))))
)

;;; ============================================================
;;; Optics Tests
;;; ============================================================

(test-group "graph-optics"

  (define-test "node-pos-lens view"
    (let ([node (make-graph-node 'a 10.0 20.0)])
      (let ([pos (^. node node-pos-lens)])
        (assert-equal 10.0 (vector-ref pos 0))
        (assert-equal 20.0 (vector-ref pos 1)))))

  (define-test "node-pos-lens set"
    (let* ([node (make-graph-node 'a 0 0)]
           [moved (& node (.~ node-pos-lens (vector 5.0 10.0)))])
      (assert-equal 5.0 (node-x moved))
      (assert-equal 10.0 (node-y moved))))

  (define-test "node-vel-lens view and set"
    (let* ([node (make-graph-node 'a 0 0)]
           [with-vel (& node (.~ node-vel-lens (vector 1.0 2.0)))])
      (let ([vel (^. with-vel node-vel-lens)])
        (assert-equal 1.0 (vector-ref vel 0))
        (assert-equal 2.0 (vector-ref vel 1)))))

  (define-test "node-x-lens composed access"
    (let* ([node (make-graph-node 'a 10.0 20.0)]
           [x (^. node node-x-lens)])
      (assert-equal 10.0 x)))

  (define-test "node-y-lens composed access"
    (let* ([node (make-graph-node 'a 10.0 20.0)]
           [y (^. node node-y-lens)])
      (assert-equal 20.0 y)))

  (define-test "graph-nodes-lens view"
    (let* ([g (make-layout-graph '(a b) '())]
           [nodes (^. g graph-nodes-lens)])
      (assert-equal 2 (length nodes))))

  (define-test "graph-nodes-lens set"
    (let* ([g (make-layout-graph '(a) '())]
           [new-nodes (list (make-graph-node 'x 0 0) (make-graph-node 'y 1 1))]
           [g2 (& g (.~ graph-nodes-lens new-nodes))])
      (assert-equal 2 (length (graph-nodes g2)))))

  (define-test "graph-nodes-each traversal to-list"
    (let* ([g (make-layout-graph '(a b c) '())]
           [nodes (^.. g graph-nodes-each)])
      (assert-equal 3 (length nodes))))

  (define-test "graph-nodes-each traversal over"
    (let* ([g (list 'layout-graph
                    (list (make-graph-node 'a 0 0)
                          (make-graph-node 'b 10 10))
                    '())]
           ;; Double all x positions via traversal
           [g2 (traversal-over graph-nodes-each
                 (lambda (n) (& n (%~ node-x-lens (lambda (x) (* x 2)))))
                 g)]
           [a2 (graph-find-node g2 'a)]
           [b2 (graph-find-node g2 'b)])
      (assert-equal 0 (node-x a2))   ; 0*2 = 0
      (assert-equal 20 (node-x b2)))) ; 10*2 = 20

  (define-test "graph-all-positions traversal"
    (let* ([g (list 'layout-graph
                    (list (make-graph-node 'a 1 2)
                          (make-graph-node 'b 3 4))
                    '())]
           [positions (^.. g graph-all-positions)])
      (assert-equal 2 (length positions))
      (assert-true (vec-equal? '#(1 2) (car positions)))))

  (define-test "graph-all-velocities traversal"
    (let* ([node-a (node-set-vel (make-graph-node 'a 0 0) (vector 1.0 0.0))]
           [node-b (node-set-vel (make-graph-node 'b 0 0) (vector 0.0 2.0))]
           [g (list 'layout-graph (list node-a node-b) '())]
           [velocities (^.. g graph-all-velocities)])
      (assert-equal 2 (length velocities))))

  (define-test "normalize-layout with optics preserves structure"
    ;; Ensure the refactored normalize-layout still works
    (let* ([g (list 'layout-graph
                    (list (make-graph-node 'a 0 0)
                          (make-graph-node 'b 100 100))
                    '((a . b)))]
           [normalized (normalize-layout g 50 50 5)])
      ;; Check all nodes exist and are within bounds
      (assert-equal 2 (length (graph-nodes normalized)))
      (for-each
       (lambda (node)
         (assert-true (>= (node-x node) 5))
         (assert-true (<= (node-x node) 45))
         (assert-true (>= (node-y node) 5))
         (assert-true (<= (node-y node) 45)))
       (graph-nodes normalized))))

  (define-test "lens laws: get-put for node-pos-lens"
    (let* ([node (make-graph-node 'a 10.0 20.0)]
           [pos (^. node node-pos-lens)]
           [node2 (& node (.~ node-pos-lens pos))])
      (assert-equal (node-x node) (node-x node2))
      (assert-equal (node-y node) (node-y node2))))

  (define-test "lens laws: put-get for node-pos-lens"
    (let* ([node (make-graph-node 'a 10.0 20.0)]
           [new-pos (vector 99.0 88.0)]
           [node2 (& node (.~ node-pos-lens new-pos))])
      (assert-true (vec-equal? new-pos (^. node2 node-pos-lens)))))
)

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
