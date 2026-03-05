;;; lattice/data/graph/graph-layout.ss --- Force-directed graph layout algorithms
;;; @module graph-layout
;;; @requires prelude linalg/vec optics hamt iteration
;;; @description Force-directed graph layout algorithms
;;; @purity total
;;; @stability stable

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'vec)
(require 'optics)
(require 'hamt)
(require 'iteration)

(doc 'module 'graph-layout)
(doc 'description "Force-directed graph layout using Fruchterman-Reingold algorithm")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Section: Graph Representation
;;; ============================================================

(doc 'section 'graph-representation)

(doc make-graph-node 'type '(-> Any Number Number GraphNode))
(doc make-graph-node 'description "Create a graph node with id and position")
(define (make-graph-node id x y)
  (doc 'export #t)
  (list 'graph-node id (vector x y) (vector 0.0 0.0)))  ; id, pos, velocity

(define (graph-node? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'graph-node)))

(define (layout-graph? x)
  (and (pair? x) (eq? (car x) 'layout-graph)))

(define (node-id node)
  (doc 'export #t)
  (list-ref node 1))
(define (node-pos node)
  (doc 'export #t)
  (list-ref node 2))
(define (node-vel node)
  (doc 'export #t)
  (list-ref node 3))
(define (node-x node)
  (doc 'export #t)
  (vector-ref (node-pos node) 0))
(define (node-y node)
  (doc 'export #t)
  (vector-ref (node-pos node) 1))

(define (node-set-pos node new-pos)
  (list 'graph-node (node-id node) new-pos (node-vel node)))

(define (node-set-vel node new-vel)
  (list 'graph-node (node-id node) (node-pos node) new-vel))

;;; Node Optics - composable access to node fields
;;; Note: make-lens setter signature is (val whole) -> whole
(doc node-pos-lens 'export #t)
(doc node-pos-lens 'type '(Lens GraphNode Vec))
(doc node-pos-lens 'description "Lens focusing on node position vector")
(define node-pos-lens
  (make-lens node-pos (lambda (val node) (node-set-pos node val))))

(doc node-vel-lens 'export #t)
(doc node-vel-lens 'type '(Lens GraphNode Vec))
(doc node-vel-lens 'description "Lens focusing on node velocity vector")
(define node-vel-lens
  (make-lens node-vel (lambda (val node) (node-set-vel node val))))

;;; Composed lenses for x/y components
;;; (uses vec-element-lens from optics.ss)
(doc node-x-lens 'export #t)
(doc node-x-lens 'type '(Lens GraphNode Number))
(doc node-x-lens 'description "Lens focusing on node x coordinate")
(define node-x-lens
  (>>> node-pos-lens (vec-element-lens 0)))

(doc node-y-lens 'export #t)
(doc node-y-lens 'type '(Lens GraphNode Number))
(doc node-y-lens 'description "Lens focusing on node y coordinate")
(define node-y-lens
  (>>> node-pos-lens (vec-element-lens 1)))

(doc make-layout-graph 'type '(-> (List Any) (List (Pair Any Any)) LayoutGraph))
(doc make-layout-graph 'description "Create a layout graph from node IDs and edges")
(define (make-layout-graph node-ids edges)
  (doc 'export #t)
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

;;; Graph Optics
(doc graph-nodes-lens 'export #t)
(doc graph-nodes-lens 'type '(Lens LayoutGraph (List GraphNode)))
(doc graph-nodes-lens 'description "Lens focusing on graph's node list")
(define graph-nodes-lens
  (make-lens graph-nodes (lambda (val g) (graph-set-nodes g val))))

;;; Traversal over all nodes in a graph
(doc graph-nodes-each 'export #t)
(doc graph-nodes-each 'type '(Traversal LayoutGraph GraphNode))
(doc graph-nodes-each 'description "Traversal over all nodes in a graph")
(define graph-nodes-each
  (make-traversal
   ;; Traverse: apply f to each node
   (lambda (f g)
     (graph-set-nodes g (map f (graph-nodes g))))
   ;; Fold: get all nodes as list
   (lambda (g)
     (graph-nodes g))))

;;; Composed traversals for accessing all positions/velocities
(doc graph-all-positions 'export #t)
(doc graph-all-positions 'type '(Traversal LayoutGraph Vec))
(doc graph-all-positions 'description "Traversal over all node positions")
(define graph-all-positions
  (>>> graph-nodes-each node-pos-lens))

(doc graph-all-velocities 'export #t)
(doc graph-all-velocities 'type '(Traversal LayoutGraph Vec))
(doc graph-all-velocities 'description "Traversal over all node velocities")
(define graph-all-velocities
  (>>> graph-nodes-each node-vel-lens))

;;; ============================================================
;;; Section: Force Calculations
;;; ============================================================

(doc 'section 'forces)

;;; Tunable parameters - can be set! before running layout
(define *repulsion-constant* 50000.0)   ; Much higher = nodes push apart more
(define *attraction-constant* 0.005)    ; Lower = connected nodes don't pull as tight
(define *gravity-constant* 0.01)        ; Weak pull toward center (prevents drift)
(define *damping* 0.85)
(define *min-distance* 10.0)            ; Minimum separation
(define *max-displacement* 30.0)        ; Smaller steps = more stable

(doc calculate-repulsion 'type '(-> GraphNode GraphNode Vec))
(doc calculate-repulsion 'description "Calculate repulsive force between two nodes (Coulomb's law)")
(define (calculate-repulsion node1 node2)
  (doc 'export #t)
  (let* ([pos1 (node-pos node1)]
         [pos2 (node-pos node2)]
         [delta (vec-sub pos1 pos2)]
         [raw-dist (vec-norm delta)]
         ;; Fix: when nodes overlap, add random perturbation to separate them
         [delta (if (< raw-dist 0.01)
                    (vector (- (random 10) 5) (- (random 10) 5))
                    delta)]
         [dist (max *min-distance* (if (< raw-dist 0.01) 5.0 raw-dist))]
         [force-mag (/ *repulsion-constant* (* dist dist))]
         [unit-vec (vec-scale (/ 1.0 (max 0.01 (vec-norm delta))) delta)])
    (vec-scale force-mag unit-vec)))

(doc calculate-attraction 'type '(-> GraphNode GraphNode Vec))
(doc calculate-attraction 'description "Calculate attractive force between connected nodes (Hooke's law)")
(define (calculate-attraction node1 node2)
  (doc 'export #t)
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
  (doc 'export #t)
  (let* ([nodes (graph-nodes graph)]
         [edges (graph-edges graph)]
         [n (length nodes)]
         ;; Convert to vector for O(1) access during force computation
         [nodes-vec (list->vector nodes)]
         ;; Pre-compute id->index map for O(1) lookup
         [id-map (let build-map ([ns nodes] [i 0] [acc hamt-empty])
                   (if (pair? ns)
                       (build-map (cdr ns) (+ i 1)
                                  (hamt-assoc (node-id (car ns)) i acc))
                       acc))]
         ;; Calculate forces for each node via tabulate
         [forces (vec-tabulate n i
                   (let* ([node-i (vector-ref nodes-vec i)]
                          ;; Repulsion from all other nodes
                          [repulsion
                           (range-fold acc (vector 0.0 0.0) j 0 n
                             (if (= i j) acc
                                 (vec-add acc (calculate-repulsion
                                               node-i (vector-ref nodes-vec j)))))]
                          ;; Attraction from incident edges
                          [attraction
                           (fold-left
                            (lambda (acc edge)
                              (let* ([id1 (car edge)]
                                     [id2 (cdr edge)]
                                     [idx1 (hamt-lookup id1 id-map)]
                                     [idx2 (hamt-lookup id2 id-map)])
                                (cond
                                  [(and idx1 idx2 (= idx1 i))
                                   (vec-add acc (calculate-attraction
                                                 node-i (vector-ref nodes-vec idx2)))]
                                  [(and idx1 idx2 (= idx2 i))
                                   (vec-sub acc (calculate-attraction
                                                 (vector-ref nodes-vec idx1) node-i))]
                                  [else acc])))
                            (vector 0.0 0.0)
                            edges)]
                          ;; Central gravity
                          [gravity (vec-scale (- *gravity-constant*) (node-pos node-i))])
                     (vec-add (vec-add repulsion attraction) gravity)))])
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

;;; Kept for backward compatibility
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
  (doc 'export #t)
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
  (doc 'export #t)
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
  (doc 'export #t)
  (let-values ([(min-x min-y max-x max-y) (layout-bounds graph)])
    (let* ([range-x (max 1 (- max-x min-x))]
           [range-y (max 1 (- max-y min-y))]
           [usable-w (- width (* 2 margin))]
           [usable-h (- height (* 2 margin))]
           [scale (min (/ usable-w range-x) (/ usable-h range-y))]
           ;; Normalize position function
           [normalize-pos (lambda (node)
                            (let* ([pos (^. node node-pos-lens)]
                                   [x (vector-ref pos 0)]
                                   [y (vector-ref pos 1)]
                                   [norm-x (+ margin (* scale (- x min-x)))]
                                   [norm-y (+ margin (* scale (- y min-y)))])
                              (& node (.~ node-pos-lens (vector norm-x norm-y)))))])
      ;; Apply normalization via traversal
      (traversal-over graph-nodes-each normalize-pos graph))))

;;; ============================================================
;;; Section: Hierarchical Layout (for DAGs)
;;; ============================================================

(doc 'section 'hierarchical)

(doc hierarchical-layout 'export #t)
(doc hierarchical-layout 'type '(-> (List Any) (List (Pair Any Any)) (-> Any Nat) LayoutGraph))
(doc hierarchical-layout 'description "Create hierarchical layout based on node depth/tier")
(define (hierarchical-layout node-ids edges depth-fn)
  (doc 'export #t)
  ;; Group nodes by depth
  (let* ([depths (map (lambda (id) (cons id (depth-fn id))) node-ids)]
         [max-depth (fold-left (lambda (m p) (max m (cdr p))) 0 depths)]
         ;; Group by depth using vec-tabulate
         [by-depth (vec-tabulate (+ max-depth 1) d
                     (fold-left (lambda (acc p)
                                  (if (= (cdr p) d)
                                      (cons (car p) acc)
                                      acc))
                                '()
                                depths))])
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
  (doc 'export #t)
  (let* ([node-ids (map car adj-list)]
         [edges (append-map (lambda (entry)
                              (let ([from (car entry)]
                                    [tos (cdr entry)])
                                (map (lambda (to) (cons from to)) tos)))
                            adj-list)])
    (make-layout-graph node-ids edges)))
