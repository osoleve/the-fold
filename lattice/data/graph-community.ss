;;; lattice/data/graph-community.ss — Community Detection and MST Algorithms
;;;
;;; Graph algorithms for community structure and spanning trees:
;;;   - Label propagation community detection
;;;   - Modularity calculation and optimization
;;;   - Prim's minimum spanning tree
;;;   - Kruskal's minimum spanning tree (with union-find)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Quick Start:
;;;   (load "lattice/data/graph-matrix.ss")
;;;   (load "lattice/data/graph-community.ss")
;;;
;;;   ;; Community detection
;;;   (define g (edges->adjacency-matrix karate-edges 34 #t))
;;;   (label-propagation g)           ; => #(0 0 0 1 1 1 ...)  community labels
;;;   (modularity g communities)      ; => 0.42  quality score
;;;
;;;   ;; Minimum spanning tree
;;;   (prim-mst weighted-adj)         ; => ((0 1 2) (1 2 3) ...)  MST edges
;;;   (kruskal-mst edges n)           ; => ((0 1 2) ...)  from edge list
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec.ss
;;;   - matrix.ss
;;;   - graph-matrix.ss

;;; ============================================================
;;; Community Detection: Label Propagation
;;; ============================================================

;;; label-propagation : Matrix × [Nat] × [Nat] → Vector
;;;
;;; Detect communities using label propagation algorithm.
;;; Each node adopts the most frequent label among its neighbors.
;;; Converges when no node changes its label.
;;;
;;; Arguments:
;;;   adj      - Adjacency matrix (undirected works best)
;;;   max-iter - Maximum iterations (default: 100)
;;;   seed     - Random seed for tie-breaking (default: 42)
;;;
;;; Returns: Vector of community labels (0-indexed integers)
;;;
;;; Notes:
;;;   - Fast: O(km) where k = iterations, m = edges
;;;   - Non-deterministic due to tie-breaking and node order
;;;   - Works best on graphs with clear community structure
;;;
;;; Example:
;;;   (label-propagation (karate-club-graph)) => ~2 communities
(define (label-propagation adj . opts)
  (let* ([n (matrix-rows adj)]
         [max-iter (if (and (pair? opts) (integer? (car opts)))
                       (car opts)
                       100)]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [seed (if (and (pair? rest1) (integer? (car rest1)))
                   (car rest1)
                   42)]
         ;; Initialize: each node in its own community
         [labels (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! labels i i))
        ;; Iterate until convergence
        (let loop ([iter 0] [changed #t])
             (if (or (>= iter max-iter) (not changed))
                 labels
                 (let ([any-changed #f]
                       ;; Process nodes in pseudo-random order based on seed+iter
                       [order (label-prop-node-order n seed iter)])
                      (for-each
                       (lambda (node)
                               (let ([new-label (most-frequent-neighbor-label adj labels node n)])
                                    (when (and new-label
                                               (not (= new-label (vector-ref labels node))))
                                          (vector-set! labels node new-label)
                                          (set! any-changed #t))))
                       order)
                      (loop (+ iter 1) any-changed))))))

;;; label-prop-node-order : Nat × Nat × Nat → (List Nat)
;;; Generate pseudo-random node ordering for label propagation.
;;; Uses simple linear congruential shuffle based on seed and iteration.
(define (label-prop-node-order n seed iter)
  (let* ([nodes (let loop ([i 0] [acc '()])
                     (if (= i n) acc (loop (+ i 1) (cons i acc))))]
         ;; Simple deterministic shuffle using modular arithmetic
         [a 1103515245]
         [c 12345]
         [m (expt 2 31)]
         [state (remainder (+ (* a (+ seed iter)) c) m)])
        ;; Fisher-Yates style shuffle with LCG
        (let ([vec (list->vector nodes)])
             (do ([i (- n 1) (- i 1)])
                 ((< i 1) (vector->list vec))
                 (let* ([state-new (remainder (+ (* a state) c) m)]
                        [j (remainder state-new (+ i 1))]
                        [tmp (vector-ref vec i)])
                       (set! state state-new)
                       (vector-set! vec i (vector-ref vec j))
                       (vector-set! vec j tmp))))))

;;; most-frequent-neighbor-label : Matrix × Vector × Nat × Nat → Nat | #f
;;; Find the most frequent label among node's neighbors.
;;; Returns #f if node has no neighbors.
(define (most-frequent-neighbor-label adj labels node n)
  (let ([counts (make-vector n 0)]
        [max-count 0]
        [max-label #f])
       ;; Count neighbor labels
       (do ([j 0 (+ j 1)])
           ((= j n))
           (when (and (not (= j node))
                      (> (matrix-ref adj node j) 0))
                 (let* ([lbl (vector-ref labels j)]
                        [new-count (+ 1 (vector-ref counts lbl))])
                       (vector-set! counts lbl new-count)
                       (when (> new-count max-count)
                             (set! max-count new-count)
                             (set! max-label lbl)))))
       max-label))

;;; communities->partition : Vector → (List (List Nat))
;;; Convert label vector to list of node lists (one per community).
(define (communities->partition labels)
  (let* ([n (vector-length labels)]
         [max-label (let loop ([i 0] [mx 0])
                         (if (= i n) mx
                             (loop (+ i 1) (max mx (vector-ref labels i)))))]
         [groups (make-vector (+ max-label 1) '())])
        ;; Group nodes by label
        (do ([i 0 (+ i 1)])
            ((= i n))
            (let ([lbl (vector-ref labels i)])
                 (vector-set! groups lbl (cons i (vector-ref groups lbl)))))
        ;; Collect non-empty groups
        (let loop ([i 0] [result '()])
             (if (> i max-label)
                 (reverse result)
                 (let ([grp (vector-ref groups i)])
                      (loop (+ i 1)
                            (if (null? grp) result (cons (reverse grp) result))))))))

;;; num-communities : Vector → Nat
;;; Count number of distinct communities.
(define (num-communities labels)
  (length (communities->partition labels)))

;;; ============================================================
;;; Modularity
;;; ============================================================

;;; modularity : Matrix × Vector → Num
;;;
;;; Compute modularity Q of a partition.
;;; Modularity measures the quality of a community partition:
;;; Q = (1/2m) Σ [A_ij - k_i*k_j/(2m)] δ(c_i, c_j)
;;;
;;; Arguments:
;;;   adj    - Adjacency matrix (undirected)
;;;   labels - Community labels (vector of integers)
;;;
;;; Returns: Modularity score in [-0.5, 1]
;;;          Q > 0.3 generally indicates good community structure
;;;
;;; Example:
;;;   (modularity karate-adj (label-propagation karate-adj)) => ~0.4
(define (modularity adj labels)
  (let* ([n (matrix-rows adj)]
         ;; Total edges (2m for undirected = sum of all entries)
         [two-m (matrix-sum adj)]
         [m (/ two-m 2.0)]
         ;; Degree of each node
         [degrees (make-vector n 0)])
        (if (= m 0)
            0  ; No edges = modularity undefined, return 0
            (begin
             ;; Compute degrees
             (do ([i 0 (+ i 1)])
                 ((= i n))
                 (do ([j 0 (+ j 1)])
                     ((= j n))
                     (when (> (matrix-ref adj i j) 0)
                           (vector-set! degrees i
                                        (+ (vector-ref degrees i)
                                           (matrix-ref adj i j))))))
             ;; Compute modularity sum
             (let ([sum 0])
                  (do ([i 0 (+ i 1)])
                      ((= i n))
                      (do ([j 0 (+ j 1)])
                          ((= j n))
                          (when (= (vector-ref labels i) (vector-ref labels j))
                                (let* ([a-ij (matrix-ref adj i j)]
                                       [ki (vector-ref degrees i)]
                                       [kj (vector-ref degrees j)]
                                       [expected (/ (* ki kj) two-m)])
                                      (set! sum (+ sum (- a-ij expected)))))))
                  (/ sum two-m))))))

;;; matrix-sum : Matrix → Num
;;; Sum all elements of a matrix.
(define (matrix-sum m)
  (let* ([n (matrix-rows m)]
         [c (matrix-cols m)]
         [data (matrix-data m)])
        (let loop ([i 0] [sum 0])
             (if (= i (vector-length data))
                 sum
                 (loop (+ i 1) (+ sum (vector-ref data i)))))))

;;; ============================================================
;;; Minimum Spanning Tree: Prim's Algorithm
;;; ============================================================

;;; prim-mst : Matrix × [Nat] → (List Edge)
;;;
;;; Compute minimum spanning tree using Prim's algorithm.
;;; Starts from a given node and greedily adds the minimum weight edge
;;; connecting the tree to a new node.
;;;
;;; Arguments:
;;;   adj   - Weighted adjacency matrix (0 = no edge)
;;;   start - Starting node (default: 0)
;;;
;;; Returns: List of edges (from to weight) forming the MST
;;;
;;; Complexity: O(n²) with linear search for minimum
;;;
;;; Notes:
;;;   - Returns empty list if graph is disconnected
;;;   - For unweighted graphs, any spanning tree is minimum
;;;
;;; Example:
;;;   (prim-mst weighted-adj) => ((0 1 2) (1 2 3) (0 3 1))
(define (prim-mst adj . opts)
  (let* ([n (matrix-rows adj)]
         [start (if (pair? opts) (car opts) 0)]
         ;; Track which nodes are in the MST
         [in-mst (make-vector n #f)]
         ;; Minimum edge weight to reach each node from MST
         [min-weight (make-vector n +inf.0)]
         ;; Parent node in MST
         [parent (make-vector n -1)]
         [mst-edges '()])
        ;; Initialize
        (vector-set! min-weight start 0)
        ;; Add n nodes to MST
        (do ([count 0 (+ count 1)])
            ((= count n))
            ;; Find minimum weight node not in MST
            (let ([u (prim-find-min min-weight in-mst n)])
                 (when u
                       (vector-set! in-mst u #t)
                       ;; Add edge to MST (except for start node)
                       (when (>= (vector-ref parent u) 0)
                             (set! mst-edges
                                   (cons (list (vector-ref parent u) u
                                               (matrix-ref adj (vector-ref parent u) u))
                                         mst-edges)))
                       ;; Update weights for neighbors
                       (do ([v 0 (+ v 1)])
                           ((= v n))
                           (let ([w (matrix-ref adj u v)])
                                (when (and (> w 0)
                                           (not (vector-ref in-mst v))
                                           (< w (vector-ref min-weight v)))
                                      (vector-set! min-weight v w)
                                      (vector-set! parent v u)))))))
        (reverse mst-edges)))

;;; prim-find-min : Vector × Vector × Nat → Nat | #f
;;; Find node with minimum weight not yet in MST.
(define (prim-find-min weights in-mst n)
  (let loop ([i 0] [min-idx #f] [min-val +inf.0])
       (if (= i n)
           min-idx
           (if (and (not (vector-ref in-mst i))
                    (< (vector-ref weights i) min-val))
               (loop (+ i 1) i (vector-ref weights i))
               (loop (+ i 1) min-idx min-val)))))

;;; mst-weight : (List Edge) → Num
;;; Compute total weight of MST.
(define (mst-weight edges)
  (fold-left (lambda (sum edge) (+ sum (caddr edge))) 0 edges))

;;; ============================================================
;;; Minimum Spanning Tree: Kruskal's Algorithm
;;; ============================================================

;;; kruskal-mst : (List Edge) × Nat → (List Edge)
;;;
;;; Compute minimum spanning tree using Kruskal's algorithm.
;;; Sorts edges by weight and greedily adds edges that don't create cycles.
;;; Uses union-find for cycle detection.
;;;
;;; Arguments:
;;;   edges - List of edges (from to weight)
;;;   n     - Number of nodes
;;;
;;; Returns: List of edges forming the MST
;;;
;;; Complexity: O(m log m) for sorting, O(m α(n)) for union-find
;;;
;;; Example:
;;;   (kruskal-mst '((0 1 2) (1 2 3) (0 2 4)) 3) => ((0 1 2) (1 2 3))
(define (kruskal-mst edges n)
  (let* (;; Sort edges by weight (ascending)
         [sorted-edges (list-sort (lambda (a b) (< (caddr a) (caddr b)))
                                  edges)]
         ;; Union-find data structures
         [parent (make-vector n 0)]
         [rank (make-vector n 0)]
         [mst-edges '()]
         [edge-count 0])
        ;; Initialize union-find: each node is its own parent
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! parent i i))
        ;; Process edges in order of weight
        (for-each
         (lambda (edge)
                 (when (< edge-count (- n 1))  ; MST has n-1 edges
                       (let* ([u (car edge)]
                              [v (cadr edge)]
                              [root-u (uf-find parent u)]
                              [root-v (uf-find parent v)])
                             ;; If different components, add edge
                             (when (not (= root-u root-v))
                                   (set! mst-edges (cons edge mst-edges))
                                   (set! edge-count (+ edge-count 1))
                                   (uf-union parent rank root-u root-v)))))
         sorted-edges)
        (reverse mst-edges)))

;;; uf-find : Vector × Nat → Nat
;;; Find root of node with path compression.
(define (uf-find parent x)
  (if (= x (vector-ref parent x))
      x
      (let ([root (uf-find parent (vector-ref parent x))])
           (vector-set! parent x root)  ; Path compression
           root)))

;;; uf-union : Vector × Vector × Nat × Nat → Void
;;; Union two sets by rank.
(define (uf-union parent rank x y)
  (let ([rx (vector-ref rank x)]
        [ry (vector-ref rank y)])
       (cond [(< rx ry)
              (vector-set! parent x y)]
             [(> rx ry)
              (vector-set! parent y x)]
             [else
              (vector-set! parent y x)
              (vector-set! rank x (+ rx 1))])))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; connected-components : Matrix → Vector
;;; Find connected components using BFS.
;;; Returns vector where labels[i] = component ID for node i.
(define (connected-components adj)
  (let* ([n (matrix-rows adj)]
         [labels (make-vector n -1)]
         [component 0])
        (do ([start 0 (+ start 1)])
            ((= start n) labels)
            (when (= (vector-ref labels start) -1)
                  ;; BFS from this node
                  (let bfs ([queue (list start)])
                       (when (not (null? queue))
                             (let ([node (car queue)]
                                   [rest-q (cdr queue)]
                                   [new-nodes '()])
                                  (when (= (vector-ref labels node) -1)
                                        (vector-set! labels node component)
                                        ;; Add unvisited neighbors
                                        (do ([j 0 (+ j 1)])
                                            ((= j n))
                                            (when (and (> (matrix-ref adj node j) 0)
                                                       (= (vector-ref labels j) -1))
                                                  (set! new-nodes (cons j new-nodes)))))
                                  (bfs (append rest-q new-nodes)))))
                  (set! component (+ component 1))))))

;;; is-connected? : Matrix → Boolean
;;; Check if graph is connected.
(define (is-connected? adj)
  (let ([components (connected-components adj)])
       (= (num-communities components) 1)))

;;; mst-from-adjacency : Matrix → (List Edge)
;;; Convenience: compute MST from adjacency matrix using Prim's.
(define (mst-from-adjacency adj)
  (prim-mst adj))
