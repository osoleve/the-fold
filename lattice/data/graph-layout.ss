;;; lattice/data/graph-layout.ss --- Force-directed graph layout algorithms
;;; @module graph-layout
;;; @requires prelude linalg/vec

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")

(doc 'module 'graph-layout)
(doc 'description "Force-directed graph layout using Fruchterman-Reingold algorithm")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Section: Graph Representation
;;; ============================================================

(doc 'section 'graph-representation)

(doc make-graph-node 'type '(-> Any Number Number GraphNode))
(doc make-graph-node 'description "Create a graph node with id and position")
(define (make-graph-node id x y)
  (list 'graph-node id (vector x y) (vector 0.0 0.0)))  ; id, pos, velocity

(define (node-id node) (list-ref node 1))
(define (node-pos node) (list-ref node 2))
(define (node-vel node) (list-ref node 3))
(define (node-x node) (vector-ref (node-pos node) 0))
(define (node-y node) (vector-ref (node-pos node) 1))

(define (node-set-pos node new-pos)
  (list 'graph-node (node-id node) new-pos (node-vel node)))

(define (node-set-vel node new-vel)
  (list 'graph-node (node-id node) (node-pos node) new-vel))

(doc make-layout-graph 'type '(-> (List Any) (List (Pair Any Any)) LayoutGraph))
(doc make-layout-graph 'description "Create a layout graph from node IDs and edges")
(define (make-layout-graph node-ids edges)
  (list 'layout-graph
        ;; Initialize nodes with random positions
        (map (lambda (id)
               (make-graph-node id
                                (- (random 200) 100)
                                (- (random 200) 100)))
             node-ids)
        edges))

(define (graph-nodes g) (list-ref g 1))
(define (graph-edges g) (list-ref g 2))

(define (graph-set-nodes g new-nodes)
  (list 'layout-graph new-nodes (graph-edges g)))

(define (graph-find-node g id)
  (let loop ([nodes (graph-nodes g)])
    (cond
      [(null? nodes) #f]
      [(equal? (node-id (car nodes)) id) (car nodes)]
      [else (loop (cdr nodes))])))

;;; ============================================================
;;; Section: Force Calculations
;;; ============================================================

(doc 'section 'forces)

(define *repulsion-constant* 10000.0)
(define *attraction-constant* 0.01)
(define *damping* 0.85)
(define *min-distance* 1.0)
(define *max-displacement* 50.0)

(doc calculate-repulsion 'type '(-> GraphNode GraphNode Vec))
(doc calculate-repulsion 'description "Calculate repulsive force between two nodes (Coulomb's law)")
(define (calculate-repulsion node1 node2)
  (let* ([pos1 (node-pos node1)]
         [pos2 (node-pos node2)]
         [delta (vec-sub pos1 pos2)]
         [dist (max *min-distance* (vec-norm delta))]
         [force-mag (/ *repulsion-constant* (* dist dist))]
         [unit-vec (vec-scale (/ 1.0 dist) delta)])
    (vec-scale force-mag unit-vec)))

(doc calculate-attraction 'type '(-> GraphNode GraphNode Vec))
(doc calculate-attraction 'description "Calculate attractive force between connected nodes (Hooke's law)")
(define (calculate-attraction node1 node2)
  (let* ([pos1 (node-pos node1)]
         [pos2 (node-pos node2)]
         [delta (vec-sub pos2 pos1)]
         [dist (vec-norm delta)]
         [force-mag (* *attraction-constant* dist)])
    (if (< dist *min-distance*)
        (vector 0.0 0.0)
        (let ([unit-vec (vec-scale (/ 1.0 dist) delta)])
          (vec-scale force-mag unit-vec)))))

;;; ============================================================
;;; Section: Layout Iteration
;;; ============================================================

(doc 'section 'iteration)

(doc layout-step 'type '(-> LayoutGraph LayoutGraph))
(doc layout-step 'description "Perform one iteration of force-directed layout")
(define (layout-step graph)
  (let* ([nodes (graph-nodes graph)]
         [edges (graph-edges graph)]
         [n (length nodes)]
         ;; Calculate forces for each node
         [forces (make-vector n (vector 0.0 0.0))])
    ;; Repulsion between all pairs
    (let loop-i ([i 0] [nodes-i nodes])
      (when (< i n)
        (let loop-j ([j (+ i 1)] [nodes-j (cdr nodes-i)])
          (when (< j n)
            (let* ([node-i (car nodes-i)]
                   [node-j (car nodes-j)]
                   [force (calculate-repulsion node-i node-j)])
              (vector-set! forces i (vec-add (vector-ref forces i) force))
              (vector-set! forces j (vec-sub (vector-ref forces j) force))
              (loop-j (+ j 1) (cdr nodes-j)))))
        (loop-i (+ i 1) (cdr nodes-i))))
    ;; Attraction along edges
    (for-each
     (lambda (edge)
       (let* ([id1 (car edge)]
              [id2 (cdr edge)]
              [node1 (graph-find-node graph id1)]
              [node2 (graph-find-node graph id2)])
         (when (and node1 node2)
           (let ([force (calculate-attraction node1 node2)]
                 [idx1 (node-index nodes id1)]
                 [idx2 (node-index nodes id2)])
             (when idx1
               (vector-set! forces idx1 (vec-add (vector-ref forces idx1) force)))
             (when idx2
               (vector-set! forces idx2 (vec-sub (vector-ref forces idx2) force)))))))
     edges)
    ;; Apply forces with damping
    (graph-set-nodes
     graph
     (let loop ([i 0] [ns nodes] [result '()])
       (if (null? ns)
           (reverse result)
           (let* ([node (car ns)]
                  [force (vector-ref forces i)]
                  [vel (vec-scale *damping* (vec-add (node-vel node) force))]
                  ;; Clamp displacement
                  [vel-mag (vec-norm vel)]
                  [vel-clamped (if (> vel-mag *max-displacement*)
                                   (vec-scale (/ *max-displacement* vel-mag) vel)
                                   vel)]
                  [new-pos (vec-add (node-pos node) vel-clamped)]
                  [new-node (node-set-pos (node-set-vel node vel-clamped) new-pos)])
             (loop (+ i 1) (cdr ns) (cons new-node result))))))))

(define (node-index nodes id)
  (let loop ([ns nodes] [i 0])
    (cond
      [(null? ns) #f]
      [(equal? (node-id (car ns)) id) i]
      [else (loop (cdr ns) (+ i 1))])))

(doc run-layout 'export #t)
(doc run-layout 'type '(-> LayoutGraph Nat LayoutGraph))
(doc run-layout 'description "Run force-directed layout for n iterations")
(define (run-layout graph n-iterations)
  (let loop ([g graph] [i 0])
    (if (>= i n-iterations)
        g
        (loop (layout-step g) (+ i 1)))))

;;; ============================================================
;;; Section: Layout Bounds and Normalization
;;; ============================================================

(doc 'section 'normalization)

(doc layout-bounds 'type '(-> LayoutGraph (Values Number Number Number Number)))
(doc layout-bounds 'description "Get bounding box of layout: (min-x, min-y, max-x, max-y)")
(define (layout-bounds graph)
  (let ([nodes (graph-nodes graph)])
    (if (null? nodes)
        (values 0 0 0 0)
        (let loop ([ns (cdr nodes)]
                   [min-x (node-x (car nodes))]
                   [min-y (node-y (car nodes))]
                   [max-x (node-x (car nodes))]
                   [max-y (node-y (car nodes))])
          (if (null? ns)
              (values min-x min-y max-x max-y)
              (let ([n (car ns)])
                (loop (cdr ns)
                      (min min-x (node-x n))
                      (min min-y (node-y n))
                      (max max-x (node-x n))
                      (max max-y (node-y n)))))))))

(doc normalize-layout 'export #t)
(doc normalize-layout 'type '(-> LayoutGraph Number Number Number LayoutGraph))
(doc normalize-layout 'description "Normalize layout to fit within width x height with margin")
(define (normalize-layout graph width height margin)
  (let-values ([(min-x min-y max-x max-y) (layout-bounds graph)])
    (let* ([range-x (max 1 (- max-x min-x))]
           [range-y (max 1 (- max-y min-y))]
           [usable-w (- width (* 2 margin))]
           [usable-h (- height (* 2 margin))]
           [scale (min (/ usable-w range-x) (/ usable-h range-y))])
      (graph-set-nodes
       graph
       (map (lambda (node)
              (let* ([x (node-x node)]
                     [y (node-y node)]
                     [norm-x (+ margin (* scale (- x min-x)))]
                     [norm-y (+ margin (* scale (- y min-y)))])
                (node-set-pos node (vector norm-x norm-y))))
            (graph-nodes graph))))))

;;; ============================================================
;;; Section: Hierarchical Layout (for DAGs)
;;; ============================================================

(doc 'section 'hierarchical)

(doc hierarchical-layout 'export #t)
(doc hierarchical-layout 'type '(-> (List Any) (List (Pair Any Any)) (-> Any Nat) LayoutGraph))
(doc hierarchical-layout 'description "Create hierarchical layout based on node depth/tier")
(define (hierarchical-layout node-ids edges depth-fn)
  ;; Group nodes by depth
  (let* ([depths (map (lambda (id) (cons id (depth-fn id))) node-ids)]
         [max-depth (fold-left (lambda (m p) (max m (cdr p))) 0 depths)]
         ;; Group by depth
         [by-depth (make-vector (+ max-depth 1) '())])
    (for-each
     (lambda (p)
       (let ([d (cdr p)])
         (vector-set! by-depth d (cons (car p) (vector-ref by-depth d)))))
     depths)
    ;; Position nodes: x based on depth, y spread within depth
    (let ([nodes '()])
      (do ([d 0 (+ d 1)])
          ((> d max-depth))
        (let* ([nodes-at-depth (reverse (vector-ref by-depth d))]
               [n (length nodes-at-depth)]
               [x (* d 100)])
          (do ([i 0 (+ i 1)]
               [ns nodes-at-depth (cdr ns)])
              ((null? ns))
            (let ([y (if (= n 1)
                         50
                         (* 100 (/ i (- n 1))))])
              (set! nodes (cons (make-graph-node (car ns) x y) nodes))))))
      (list 'layout-graph (reverse nodes) edges))))

;;; ============================================================
;;; Section: Convenience Constructors
;;; ============================================================

(doc 'section 'constructors)

(doc layout-from-adjacency 'export #t)
(doc layout-from-adjacency 'type '(-> (List (Pair Any (List Any))) LayoutGraph))
(doc layout-from-adjacency 'description "Create layout graph from adjacency list")
(define (layout-from-adjacency adj-list)
  (let* ([node-ids (map car adj-list)]
         [edges (apply append
                       (map (lambda (entry)
                              (let ([from (car entry)]
                                    [tos (cdr entry)])
                                (map (lambda (to) (cons from to)) tos)))
                            adj-list))])
    (make-layout-graph node-ids edges)))

(printf "✓ Graph layout loaded\n")
(printf "  (make-layout-graph ids edges)     - Create layout graph\n")
(printf "  (run-layout graph n)              - Run n force iterations\n")
(printf "  (normalize-layout graph w h m)    - Fit to dimensions\n")
(printf "  (hierarchical-layout ids edges f) - Tier-based layout\n")
(printf "  (layout-from-adjacency adj)       - From adjacency list\n")
