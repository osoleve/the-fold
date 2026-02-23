;;; lattice/data/graph/graph-community.ss — Community Detection
;;; @module graph-community
;;; @requires prelude sort hamt iteration

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'sort)
(require 'hamt)
(require 'iteration)

(doc 'module 'graph-community)
(doc 'purity 'partial)
(doc 'description "Community detection and minimum spanning tree algorithms")
(doc 'layer 'lattice)

(doc 'note "Graph algorithms for community structure and spanning trees:
  - Label propagation community detection
  - Modularity calculation and optimization
  - Prim's minimum spanning tree
  - Kruskal's minimum spanning tree (with union-find)
  - Connected components analysis")

(doc 'note "Quick Start:
  (load \"lattice/data/graph/graph-community.ss\")

  ;; Community detection
  (define g (edges->adjacency-matrix social-edges 34 #t))
  (define labels (label-propagation g))     ; => #(0 0 0 1 1 1 ...)
  (num-communities labels)                  ; => 2
  (communities->partition labels)           ; => ((0 1 2 ...) (3 4 5 ...))
  (modularity g labels)                     ; => 0.42  (Q > 0.3 is good)

  ;; Minimum spanning tree
  (define mst (prim-mst weighted-adj))      ; => ((0 1 2) (1 2 3) ...)
  (mst-weight mst)                          ; => 15  total weight
  (kruskal-mst '((0 1 2) (1 2 3) (0 2 4)) 3)  ; => ((0 1 2) (1 2 3))")

(doc 'note "Complexity:
  - label-propagation: O(k·n²) where k = iterations (matrix-based)
  - modularity: O(n²)
  - prim-mst: O((V + E) log V) with heap-based min extraction
  - kruskal-mst: O(m log m) for sorting + O(m·α(n)) for union-find
  - connected-components: O(n + m) BFS")

(require 'graph-matrix)
(require 'ilp)

(doc label-propagation 'type '(-> Matrix [Nat] [Nat] Vector))
(doc label-propagation 'description "Detect communities using label propagation; nodes adopt most frequent neighbor label")
(doc label-propagation 'param 'adj "Adjacency matrix (undirected works best)")
(doc label-propagation 'param 'max-iter "Maximum iterations (default: 100)")
(doc label-propagation 'param 'seed "Random seed for node ordering (default: 42)")
(doc label-propagation 'returns "Vector of community labels (0-indexed integers)")
(doc label-propagation 'note "Complexity: O(k·n²) with adjacency matrix; works best on graphs with clear community structure")
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
         [labels (vec-tabulate n i i)])
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
;;; Uses hashtable to handle sparse/large label values safely.
(define (communities->partition labels)
  (let* ([n (vector-length labels)]
         ;; Group nodes by label using HAMT (handles sparse labels)
         [groups (let loop ([i 0] [acc hamt-empty])
                   (if (= i n) acc
                       (let* ([lbl (vector-ref labels i)]
                              [existing (hamt-lookup-or lbl acc '())])
                         (loop (+ i 1) (hamt-assoc lbl (cons i existing) acc)))))]
         ;; Collect all groups, sorted by label for deterministic output
         [sorted-keys (sort-by < (hamt-keys groups))])
    (map (lambda (k) (reverse (hamt-lookup-or k groups '()))) sorted-keys)))

;;; num-communities : Vector → Nat
;;; Count number of distinct communities.
(define (num-communities labels)
  (length (communities->partition labels)))

(doc modularity 'type '(-> Matrix Vector Num))
(doc modularity 'description "Compute modularity Q: (1/2m) Σ [A_ij - k_i*k_j/(2m)] δ(c_i, c_j)")
(doc modularity 'param 'adj "Adjacency matrix (undirected)")
(doc modularity 'param 'labels "Community labels (vector of integers)")
(doc modularity 'returns "Modularity score in [-0.5, 1]; Q > 0.3 indicates good community structure")
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

(doc prim-mst 'type '(-> (Union Matrix SparseCSR) [Nat] (List Edge)))
(doc prim-mst 'description "Compute minimum spanning tree using Prim's algorithm with heap-based extraction")
(doc prim-mst 'param 'adj "Weighted adjacency matrix (0 = no edge), dense or sparse")
(doc prim-mst 'param 'start "Starting node (default: 0)")
(doc prim-mst 'returns "List of edges (from to weight) forming the MST")
(doc prim-mst 'note "Complexity: O((V + E) log V); uses lazy deletion for efficiency")
(doc prim-mst 'note "For disconnected graphs, returns MST of component containing start")
(define (prim-mst adj . opts)
  (let* ([n (adjacency-matrix-node-count adj)]
         [start (if (pair? opts) (car opts) 0)]
         ;; Track which nodes are in the MST
         [in-mst (make-vector n #f)]
         ;; Minimum edge weight to reach each node from MST
         [min-weight (make-vector n +inf.0)]
         ;; Parent node in MST
         [parent (make-vector n -1)]
         ;; Comparator: smaller weight first (min-heap)
         ;; Heap entries are (weight node from) triples
         [weight-cmp (lambda (a b) (<= (car a) (car b)))])
        ;; Initialize source
        (vector-set! min-weight start 0)
        ;; Main loop with heap-based extraction
        (let loop ([heap (heap-insert-by weight-cmp (list 0 start -1) heap-empty)]
                   [mst-edges '()])
             (if (heap-empty? heap)
                 (reverse mst-edges)
                 (let* ([top (heap-value heap)]
                        [heap-rest (heap-delete-top-by weight-cmp heap)]
                        [w (car top)]
                        [u (cadr top)]
                        [from (caddr top)])
                       (if (vector-ref in-mst u)
                           ;; Already in MST (lazy deletion) - skip
                           (loop heap-rest mst-edges)
                           (begin
                             (vector-set! in-mst u #t)
                             ;; Add edge to MST using weight from heap (avoid re-lookup)
                             (let ([new-edges (if (>= from 0)
                                                  (cons (list from u w) mst-edges)
                                                  mst-edges)])
                                  ;; Add neighbors - O(degree) instead of O(V)
                                  (let add-neighbors ([neighbors (adjacency-neighbors-with-weights adj u)]
                                                      [h heap-rest])
                                       (if (null? neighbors)
                                           (loop h new-edges)
                                           (let* ([edge (car neighbors)]
                                                  [v (car edge)]
                                                  [edge-w (cdr edge)])
                                                 (if (and (not (vector-ref in-mst v))
                                                          (< edge-w (vector-ref min-weight v)))
                                                     (begin
                                                       (vector-set! min-weight v edge-w)
                                                       (vector-set! parent v u)
                                                       (add-neighbors (cdr neighbors)
                                                                      (heap-insert-by weight-cmp
                                                                                      (list edge-w v u)
                                                                                      h)))
                                                     (add-neighbors (cdr neighbors) h)))))))))))))

;;; prim-mst-naive : Matrix × [Nat] → (List Edge)
;;; Original O(n²) implementation using linear scan for min extraction.
;;; Kept for comparison and cases where heap overhead isn't worth it.
(define (prim-mst-naive adj . opts)
  (let* ([n (matrix-rows adj)]
         [start (if (pair? opts) (car opts) 0)]
         [in-mst (make-vector n #f)]
         [min-weight (make-vector n +inf.0)]
         [parent (make-vector n -1)]
         [mst-edges '()])
        (vector-set! min-weight start 0)
        (do ([count 0 (+ count 1)])
            ((= count n))
            (let ([u (prim-find-min min-weight in-mst n)])
                 (when u
                       (vector-set! in-mst u #t)
                       (when (>= (vector-ref parent u) 0)
                             (set! mst-edges
                                   (cons (list (vector-ref parent u) u
                                               (matrix-ref adj (vector-ref parent u) u))
                                         mst-edges)))
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
;;; Find node with minimum weight not yet in MST. O(n) linear scan.
;;; Used by prim-mst-naive.
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

(doc kruskal-mst 'type '(-> (List Edge) Nat (List Edge)))
(doc kruskal-mst 'description "Compute minimum spanning tree using Kruskal's algorithm with union-find")
(doc kruskal-mst 'param 'edges "List of edges (from to weight)")
(doc kruskal-mst 'param 'n "Number of nodes")
(doc kruskal-mst 'returns "List of edges forming the MST")
(doc kruskal-mst 'note "Complexity: O(m log m) for sorting, O(m α(n)) for union-find")
(define (kruskal-mst edges n)
  (let* (;; Sort edges by weight (ascending)
         [sorted-edges (sort-by (lambda (a b) (< (caddr a) (caddr b)))
                                  edges)]
         ;; Union-find data structures
         [parent (vec-tabulate n i i)]
         [rank (make-vector n 0)]
         [mst-edges '()]
         [edge-count 0])
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

;;; ====
;;; Utility Functions
;;; ====

;;; connected-components : Matrix → Vector
;;; Find connected components using BFS.
;;; Returns vector where labels[i] = component ID for node i.
;;; Uses level-by-level BFS for O(n + m) complexity on sparse graphs.
(define (connected-components adj)
  (let* ([n (matrix-rows adj)]
         [labels (make-vector n -1)]
         [component 0])
        (do ([start 0 (+ start 1)])
            ((= start n) labels)
            (when (= (vector-ref labels start) -1)
                  ;; Mark start node immediately to avoid re-queueing
                  (vector-set! labels start component)
                  ;; Level-by-level BFS (avoids O(n²) append)
                  (let bfs ([current-level (list start)])
                       (when (not (null? current-level))
                             (let ([next-level '()])
                                  (for-each
                                   (lambda (node)
                                           ;; Add unvisited neighbors to next level
                                           (do ([j 0 (+ j 1)])
                                               ((= j n))
                                               (when (and (> (matrix-ref adj node j) 0)
                                                          (= (vector-ref labels j) -1))
                                                     ;; Mark visited when queued (not when processed)
                                                     (vector-set! labels j component)
                                                     (set! next-level (cons j next-level)))))
                                   current-level)
                                  (bfs next-level))))
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

(doc modularity-ilp 'type '(-> Matrix Vector))
(doc modularity-ilp 'description "Compute optimal 2-community partition maximizing modularity using exact ILP")
(doc modularity-ilp 'param 'adj "Adjacency matrix (undirected)")
(doc modularity-ilp 'returns "Vector of community labels (0 or 1 for each node)")
(doc modularity-ilp 'note "Complexity: Exponential in worst case; recommended for small graphs (n < 20)")
(doc modularity-ilp 'note "For large graphs, use label-propagation instead")
(define (modularity-ilp adj)
  (let* ([n (matrix-rows adj)]
         ;; Compute modularity matrix B[i,j] = A[i,j] - k_i·k_j/(2m)
         [two-m (matrix-sum adj)])
    (if (or (= two-m 0) (< n 2))
        ;; Degenerate cases: no edges or single node
        (make-vector n 0)
        (let* ([m (/ two-m 2.0)]
               [degrees (compute-degrees adj n)]
               [B (compute-modularity-matrix adj degrees two-m n)]
               ;; Build and solve ILP
               [ilp-data (build-modularity-ilp B n)]
               [result (ilp-solve ilp-data)])
          (if (ilp-optimal? result)
              ;; Extract x[0..n-1] from solution
              (let ([sol (ilp-result-x result)])
                (vec-tabulate n i (inexact->exact (round (vector-ref sol i)))))
              ;; Fallback to all zeros if ILP fails
              (make-vector n 0))))))

;;; compute-degrees : Matrix × Nat → Vector
;;; Compute degree of each node.
(define (compute-degrees adj n)
  (let ([degrees (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) degrees)
      (do ([j 0 (+ j 1)])
          ((= j n))
        (when (> (matrix-ref adj i j) 0)
          (vector-set! degrees i
                       (+ (vector-ref degrees i)
                          (matrix-ref adj i j))))))))

;;; compute-modularity-matrix : Matrix × Vector × Num × Nat → Matrix
;;; Compute modularity matrix B[i,j] = A[i,j] - k_i·k_j/(2m).
(define (compute-modularity-matrix adj degrees two-m n)
  (let ([B (make-matrix n n 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) B)
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let* ([a-ij (matrix-ref adj i j)]
               [ki (vector-ref degrees i)]
               [kj (vector-ref degrees j)]
               [expected (/ (* ki kj) two-m)])
          (matrix-set! B i j (- a-ij expected)))))))

;;; build-modularity-ilp : Matrix × Nat → ILP
;;; Build ILP for maximizing modularity with 2 communities.
;;;
;;; Variables:
;;;   x[i] ∈ {0,1} for i = 0..n-1  (community assignment)
;;;   y[i,j] = x[i]·x[j] for i < j  (linearization variables)
;;;
;;; Objective: maximize Σ_{i,j} B[i,j]·(2·y[i,j] - x[i] - x[j] + 1)
;;;          = Σ_{i<j} 2·B[i,j]·(2·y[i,j] - x[i] - x[j] + 1) + Σ_i B[i,i]
;;;
;;; Since we minimize, negate the objective.
;;;
;;; Linearization constraints for y[i,j] = x[i]·x[j]:
;;;   y[i,j] <= x[i]
;;;   y[i,j] <= x[j]
;;;   y[i,j] >= x[i] + x[j] - 1
;;;   y[i,j] >= 0  (implicit in LP)
(define (build-modularity-ilp B n)
  ;; Number of y variables: n*(n-1)/2 for i < j
  (let* ([num-y (quotient (* n (- n 1)) 2)]
         [num-vars (+ n num-y)]
         ;; Map (i,j) where i < j to y-variable index
         [y-index (lambda (i j)
                    (let ([lo (min i j)]
                          [hi (max i j)])
                      (+ n
                         (- (* hi (- hi 1) 1/2) 0)
                         lo)))]
         ;; Build cost vector (negate for minimization)
         [c (make-vector num-vars 0)])

    ;; Compute objective coefficients
    ;; For diagonal: B[i,i] contributes to constant (ignore for optimization)
    ;; For off-diagonal i < j:
    ;;   2·B[i,j]·(2·y[i,j] - x[i] - x[j] + 1)
    ;;   = 4·B[i,j]·y[i,j] - 2·B[i,j]·x[i] - 2·B[i,j]·x[j] + 2·B[i,j]
    ;; Due to symmetry B[i,j] = B[j,i], we count each pair once:
    ;;   Coef of y[i,j]: 4·B[i,j]
    ;;   Coef of x[i]: -2·Σ_{j≠i} B[i,j] = -2·(row sum - B[i,i])
    ;;   Coef of x[j]: similar

    ;; First, compute x coefficients from row sums
    (do ([i 0 (+ i 1)])
        ((= i n))
      (let ([row-sum 0])
        (do ([j 0 (+ j 1)])
            ((= j n))
          (when (not (= i j))
            (set! row-sum (+ row-sum (matrix-ref B i j)))))
        ;; Negate because we minimize, and we want to maximize
        (vector-set! c i (* 2 row-sum))))

    ;; Then, compute y coefficients
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j (+ i 1) (+ j 1)])
          ((= j n))
        (let ([y-idx (y-index i j)]
              [bij (matrix-ref B i j)])
          ;; Negate for minimization
          (vector-set! c y-idx (* -4 bij)))))

    ;; Build constraints for linearization
    ;; Each y[i,j] needs 3 constraints:
    ;;   y[i,j] - x[i] <= 0         (y <= x[i])
    ;;   y[i,j] - x[j] <= 0         (y <= x[j])
    ;;   -y[i,j] + x[i] + x[j] <= 1  (y >= x[i] + x[j] - 1)
    ;; Plus binary constraints: x[i] <= 1
    ;;
    ;; Total constraints: 3·num-y + n (for x <= 1)
    (let* ([num-lin-constraints (* 3 num-y)]
           [num-binary-constraints n]
           [num-constraints (+ num-lin-constraints num-binary-constraints)]
           ;; Standard form: Ax = b with slack variables
           ;; Each <= constraint needs one slack variable
           [num-slack num-constraints]
           [total-vars (+ num-vars num-slack)]
           [A (make-matrix num-constraints total-vars 0)]
           [b (make-vector num-constraints 0)]
           [c-full (vec-tabulate total-vars i
                    (if (< i num-vars) (vector-ref c i) 0))]
           [constraint-idx 0])

      ;; Add linearization constraints for each y[i,j]
      (do ([i 0 (+ i 1)])
          ((= i n))
        (do ([j (+ i 1) (+ j 1)])
            ((= j n))
          (let ([y-idx (y-index i j)])
            ;; Constraint 1: y[i,j] - x[i] + s = 0  (y <= x[i])
            (matrix-set! A constraint-idx y-idx 1)
            (matrix-set! A constraint-idx i -1)
            (matrix-set! A constraint-idx (+ num-vars constraint-idx) 1)
            (vector-set! b constraint-idx 0)
            (set! constraint-idx (+ constraint-idx 1))

            ;; Constraint 2: y[i,j] - x[j] + s = 0  (y <= x[j])
            (matrix-set! A constraint-idx y-idx 1)
            (matrix-set! A constraint-idx j -1)
            (matrix-set! A constraint-idx (+ num-vars constraint-idx) 1)
            (vector-set! b constraint-idx 0)
            (set! constraint-idx (+ constraint-idx 1))

            ;; Constraint 3: -y[i,j] + x[i] + x[j] + s = 1  (y >= x[i]+x[j]-1)
            (matrix-set! A constraint-idx y-idx -1)
            (matrix-set! A constraint-idx i 1)
            (matrix-set! A constraint-idx j 1)
            (matrix-set! A constraint-idx (+ num-vars constraint-idx) 1)
            (vector-set! b constraint-idx 1)
            (set! constraint-idx (+ constraint-idx 1)))))

      ;; Add binary constraints: x[i] + s = 1  (x <= 1)
      (do ([i 0 (+ i 1)])
          ((= i n))
        (matrix-set! A constraint-idx i 1)
        (matrix-set! A constraint-idx (+ num-vars constraint-idx) 1)
        (vector-set! b constraint-idx 1)
        (set! constraint-idx (+ constraint-idx 1)))

      ;; Integer variables: x[0..n-1] and y[n..n+num-y-1]
      (let ([integer-vars (let loop ([i 0] [acc '()])
                            (if (= i num-vars)
                                (reverse acc)
                                (loop (+ i 1) (cons i acc))))])
        (make-ilp c-full A b integer-vars)))))

;;; ====
;;; Community Homology Analysis
;;; ====

(doc 'section 'community-homology)
(doc 'description "Topological analysis of community structure using simplicial homology")
(doc 'note "Combines community detection with algebraic topology:
  - B_0 measures community fragmentation (ideally 1 = connected)
  - B_1 measures internal cycle structure (higher = more dense/redundant paths)
  - Quality metrics based on topological invariants")

(require 'graph-homology)

(doc community-induced-edges 'type '(-> Matrix (List Nat) (List Edge)))
(doc community-induced-edges 'description "Extract edges within a community from adjacency matrix")
(doc community-induced-edges 'param 'adj "Adjacency matrix of full graph")
(doc community-induced-edges 'param 'nodes "List of node indices in the community")
(doc community-induced-edges 'returns "List of edges (i . j) where both i and j are in nodes")
(define (community-induced-edges adj nodes)
  (let* (;; Build set for O(1) membership
         [node-set (fold-left (lambda (acc n) (hamt-assoc n #t acc)) hamt-empty nodes)]
         ;; Extract edges between community members (undirected: only i < j)
         [edges (fold-left
                 (lambda (outer-acc i)
                   (fold-left
                    (lambda (inner-acc j)
                      (if (and (< i j)
                               (hamt-lookup j node-set)
                               (> (matrix-ref adj i j) 0))
                          (cons (cons i j) inner-acc)
                          inner-acc))
                    outer-acc
                    nodes))
                 '()
                 nodes)])
    edges))

(doc community-betti-numbers 'type '(-> Matrix (List Nat) (Pair Nat Nat)))
(doc community-betti-numbers 'description "Compute Betti numbers for community-induced subgraph")
(doc community-betti-numbers 'param 'adj "Adjacency matrix of full graph")
(doc community-betti-numbers 'param 'nodes "List of node indices in the community")
(doc community-betti-numbers 'returns "Pair (B_0 . B_1) where B_0=components, B_1=independent cycles")
(doc community-betti-numbers 'note "B_0 > 1 indicates disconnected community (poor quality)")
(define (community-betti-numbers adj nodes)
  (if (null? nodes)
      (cons 0 0)
      (let ([edges (community-induced-edges adj nodes)])
        (graph-betti-numbers edges nodes))))

(doc all-communities-betti 'type '(-> Matrix Vector (List (List Any Nat Nat Nat))))
(doc all-communities-betti 'description "Compute Betti numbers for all detected communities")
(doc all-communities-betti 'param 'adj "Adjacency matrix of full graph")
(doc all-communities-betti 'param 'labels "Community label vector from label-propagation")
(doc all-communities-betti 'returns "List of (original-label size B_0 B_1) tuples, preserving original community IDs")
(define (all-communities-betti adj labels)
  (let* ([n (vector-length labels)]
         ;; Group nodes by label
         [groups (let loop ([i 0] [acc hamt-empty])
                   (if (= i n) acc
                       (let* ([lbl (vector-ref labels i)]
                              [existing (hamt-lookup-or lbl acc '())])
                         (loop (+ i 1) (hamt-assoc lbl (cons i existing) acc)))))]
         ;; Compute Betti numbers for each community, preserving original labels
         [sorted-keys (sort-by < (hamt-keys groups))])
    (map (lambda (lbl)
           (let* ([nodes (reverse (hamt-lookup-or lbl groups '()))]
                  [size (length nodes)]
                  [betti (community-betti-numbers adj nodes)]
                  [b0 (car betti)]
                  [b1 (cdr betti)])
             (list lbl size b0 b1)))
         sorted-keys)))

(doc community-homology-quality 'type '(-> Nat Nat Nat Num))
(doc community-homology-quality 'description "Compute topological quality score for a community")
(doc community-homology-quality 'param 'size "Number of nodes in community")
(doc community-homology-quality 'param 'b0 "B_0 (connected components)")
(doc community-homology-quality 'param 'b1 "B_1 (independent cycles)")
(doc community-homology-quality 'returns "Quality score in [0, 1]: higher is better")
(doc community-homology-quality 'note "Score combines:
  - Connectivity: (largest-component-fraction)^0.5 approximated as (n/B_0)/n = 1/B_0
    but dampened to avoid harsh penalties for minor fragmentation
  - Cycle density: B_1 / max-B_1 where max-B_1 = (n-1)(n-2)/2 for K_n
  - Final: weighted combination favoring connectivity (0.7) over density (0.3)")
(define (community-homology-quality size b0 b1)
  (cond
    [(= size 0) 0.0]
    [(= b0 0) 0.0]
    [(= size 1)
     ;; Single-node community: connected but trivial, give modest score
     0.5]
    [else
     (let* (;; Connectivity: use sqrt(1/B_0) to dampen harsh fragmentation penalty
            ;; B_0=1 -> 1.0, B_0=2 -> 0.71, B_0=4 -> 0.5
            [connectivity (sqrt (/ 1.0 b0))]
            ;; Cycle density: B_1 relative to theoretical max for K_n
            ;; Max B_1 = (n-1)(n-2)/2 for complete graph on n vertices
            [max-b1 (/ (* (- size 1) (- size 2)) 2)]
            [cycle-density (if (> max-b1 0)
                               (min 1.0 (/ b1 max-b1))
                               0.0)]
            ;; Weight connectivity heavily (0.7) + cycle density (0.3)
            [score (+ (* 0.7 connectivity) (* 0.3 cycle-density))])
       score)]))

(doc aggregate-community-quality 'type '(-> Matrix Vector Num))
(doc aggregate-community-quality 'description "Compute overall topological quality of community partition")
(doc aggregate-community-quality 'param 'adj "Adjacency matrix of full graph")
(doc aggregate-community-quality 'param 'labels "Community label vector")
(doc aggregate-community-quality 'returns "Size-weighted average quality score")
(define (aggregate-community-quality adj labels)
  (let* ([betti-list (all-communities-betti adj labels)]
         [n (vector-length labels)]
         [weighted-sum 0.0])
    (for-each
     (lambda (entry)
       (let* ([size (cadr entry)]
              [b0 (caddr entry)]
              [b1 (cadddr entry)]
              [quality (community-homology-quality size b0 b1)]
              [weight (/ size n)])
         (set! weighted-sum (+ weighted-sum (* weight quality)))))
     betti-list)
    weighted-sum))

;;; round-2 : Num → Num
;;; Round to 2 decimal places for display.
(define (round-2 x)
  (/ (round (* x 100)) 100.0))

;;; round-3 : Num → Num
;;; Round to 3 decimal places for display.
(define (round-3 x)
  (/ (round (* x 1000)) 1000.0))

(doc community-homology-report 'type '(-> Matrix Vector Void))
(doc community-homology-report 'description "Print detailed homology analysis of community structure")
(doc community-homology-report 'param 'adj "Adjacency matrix of full graph")
(doc community-homology-report 'param 'labels "Community label vector from label-propagation")
(define (community-homology-report adj labels)
  (let* ([betti-list (all-communities-betti adj labels)]
         [n (vector-length labels)]
         [num-communities (length betti-list)]
         [total-quality (aggregate-community-quality adj labels)]
         [mod-score (modularity adj labels)])
    (printf "\n=== Community Homology Report ===\n\n")
    (printf "Graph: ~a nodes, ~a communities\n" n num-communities)
    (printf "Modularity Q: ~a\n" (round-3 mod-score))
    (printf "Topological Quality: ~a\n\n" (round-3 total-quality))

    (printf "Per-Community Analysis:\n")
    (printf "  ID   Size   B_0   B_1   Quality   Notes\n")
    (printf "  --   ----   ---   ---   -------   -----\n")

    (for-each
     (lambda (entry)
       (let* ([id (car entry)]
              [size (cadr entry)]
              [b0 (caddr entry)]
              [b1 (cadddr entry)]
              [quality (community-homology-quality size b0 b1)]
              ;; Max B_1 for complete graph K_n = (n-1)(n-2)/2
              [max-b1 (/ (* (- size 1) (- size 2)) 2)]
              [notes (cond
                       [(> b0 1) "FRAGMENTED"]
                       [(= size 1) "singleton"]
                       [(and (>= size 2) (= b1 0)) "tree-like"]
                       [(and (> max-b1 0) (>= b1 max-b1)) "clique"]
                       [(and (> max-b1 0) (>= (/ b1 max-b1) 0.7)) "very dense"]
                       [else ""])])
         (printf "  ~a    ~a     ~a     ~a    ~a      ~a\n"
                 id size b0 b1 (round-2 quality) notes)))
     betti-list)

    (printf "\n---------------------------------\n")
    (printf "Legend:\n")
    (printf "  B_0: Connected components (ideal = 1)\n")
    (printf "  B_1: Independent cycles (higher = denser)\n")
    (printf "  FRAGMENTED: Community has disconnected parts\n")
    (printf "  singleton:  Single-node community\n")
    (printf "  tree-like:  Connected but no cycles (B_1 = 0)\n")
    (printf "  clique:     Complete subgraph (B_1 = max)\n")
    (printf "  very dense: B_1 >= 70%% of maximum\n")
    (printf "=================================\n\n")))
