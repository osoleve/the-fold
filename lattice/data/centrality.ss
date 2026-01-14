;;; lattice/data/centrality.ss — Graph Centrality Measures
;;;
;;; Matrix-based centrality metrics for graph analysis:
;;;   - Eigenvector centrality (dominant eigenvector of adjacency matrix)
;;;   - Katz centrality (attenuated walk counts)
;;;   - Closeness centrality (inverse of average distance)
;;;   - Betweenness centrality (fraction of shortest paths through node)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Quick Start:
;;;   (load "lattice/data/graph-matrix.ss")  ; Also loads matrix deps
;;;   (load "lattice/data/centrality.ss")
;;;
;;;   ;; Create a graph
;;;   (define g (star-graph 5))  ; Star with center 0, leaves 1-4
;;;
;;;   ;; Compute centralities
;;;   (eigenvector-centrality g)  ; => #(0.33 0.17 0.17 0.17 0.17)
;;;   (katz-centrality g)         ; => All positive, center highest
;;;   (closeness-centrality (floyd-warshall g))  ; From distance matrix
;;;   (betweenness-centrality g)  ; => Center has highest betweenness
;;;
;;;   ;; Compare and rank
;;;   (rank-by-centrality (eigenvector-centrality g))  ; Sorted by score
;;;   (top-k-central (katz-centrality g) 3)            ; Top 3 nodes
;;;   (centrality-correlation evc katz)                ; Pearson r
;;;   (all-centralities g)                             ; All four measures
;;;
;;; Dependencies (must be loaded by client in correct order):
;;;   - prelude.ss
;;;   - vec.ss
;;;   - matrix.ss
;;;   - graph-matrix.ss (provides floyd-warshall, star-graph, etc.)

;;; ====
;;; Constants
;;; ====

;;; Default tolerance for convergence
(define *centrality-tolerance* 1e-8)

;;; Default maximum iterations
(define *centrality-max-iterations* 100)

;;; ====
;;; Eigenvector Centrality
;;; ====

;;; eigenvector-centrality : Matrix × [Nat] × [Num] → Vec | Error
;;;
;;; Compute eigenvector centrality for each node.
;;; Eigenvector centrality measures a node's influence based on
;;; the influence of its neighbors (recursive definition).
;;;
;;; For node i: x_i = (1/λ) × Σ A_ij × x_j
;;; where λ is the largest eigenvalue.
;;;
;;; Arguments:
;;;   adj      - Adjacency matrix (undirected graphs work best)
;;;   max-iter - Maximum iterations (default: 100)
;;;   tol      - Convergence tolerance (default: 1e-8)
;;;
;;; Returns: Vector of centrality scores (non-negative, normalized)
;;;
;;; Notes:
;;;   - Works best for connected graphs
;;;   - For directed graphs, considers both in-links
;;;   - Disconnected components may have zero centrality
;;;
;;; Example:
;;;   (eigenvector-centrality (star-graph 5)) => center has highest score
(define (eigenvector-centrality adj . opts)
  (let* ([n (matrix-rows adj)]
         [m (matrix-cols adj)])
        (if (not (= n m))
            `(error not-square ,n ,m)
            (let* ([max-iter (if (and (pair? opts) (integer? (car opts)))
                                 (car opts)
                                 *centrality-max-iterations*)]
                   [rest1 (if (and (pair? opts) (integer? (car opts)))
                              (cdr opts)
                              opts)]
                   [tol (if (and (pair? rest1) (number? (car rest1)))
                            (car rest1)
                            *centrality-tolerance*)]
                   ;; Start with uniform vector
                   [v0 (make-vec n (/ 1.0 (sqrt n)))]
                   ;; Power iteration to find dominant eigenvector
                   [result (eigenvector-power-iteration adj v0 max-iter tol)])
                  (if (and (pair? result) (eq? (car result) 'error))
                      result
                      ;; Normalize to non-negative and sum to 1
                      (eigenvector-normalize result))))))

;;; eigenvector-power-iteration : Matrix × Vec × Nat × Num → Vec
;;; Power iteration specialized for centrality (returns just vector).
;;; Detects period-2 oscillation (common in bipartite graphs) and averages
;;; oscillating vectors to recover the true dominant eigenvector.
(define (eigenvector-power-iteration adj v0 max-iter tol)
  (let loop ([v v0] [v-prev #f] [iter 0])
       (if (>= iter max-iter)
           v  ; Return best estimate
           (let* ([av (matrix-vec-mul adj v)]
                  [av-norm (vec-norm av)])
                 (if (< av-norm tol)
                     ;; Zero result - return current estimate
                     v
                     (let* ([v-new (vec-scale (/ 1.0 av-norm) av)]
                            [vec-change (vec-distance v v-new)])
                           (cond
                            ;; Converged: vector not changing
                            [(< vec-change tol) v-new]
                            ;; Detect period-2 oscillation: v-new ≈ v-prev
                            ;; (the vector 2 iterations ago)
                            [(and v-prev
                                  (> iter 2)
                                  (< (vec-distance v-new v-prev) tol))
                             ;; Average the two oscillating vectors
                             (eigenvector-average-oscillation v v-new)]
                            ;; Normal iteration
                            [else
                             (loop v-new v (+ iter 1))])))))))

;;; vec-distance : Vec × Vec → Num
;;; Euclidean distance between two vectors.
(define (vec-distance v1 v2)
  (let ([n (vector-length v1)])
       (sqrt (let loop ([i 0] [sum 0])
                  (if (= i n)
                      sum
                      (let ([d (- (vector-ref v1 i) (vector-ref v2 i))])
                           (loop (+ i 1) (+ sum (* d d)))))))))

;;; eigenvector-average-oscillation : Vec × Vec → Vec
;;; Average two oscillating vectors and normalize.
;;; For period-2 oscillation between v1 and v2, the true eigenvector
;;; is in their span - averaging often recovers it.
(define (eigenvector-average-oscillation v1 v2)
  (let* ([n (vector-length v1)]
         [avg (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! avg i (/ (+ (vector-ref v1 i) (vector-ref v2 i)) 2.0)))
        ;; Normalize
        (let ([norm (vec-norm avg)])
             (if (< norm 1e-15)
                 avg
                 (vec-scale (/ 1.0 norm) avg)))))

;;; eigenvector-normalize : Vec → Vec
;;; Normalize eigenvector to non-negative values summing to 1.
;;; The dominant eigenvector may have consistent sign or need flipping.
(define (eigenvector-normalize v)
  (let* ([n (vector-length v)]
         ;; Check if mostly negative (flip sign if so)
         [sum (vec-fold + 0 v)]
         [v-pos (if (< sum 0)
                    (vec-scale -1 v)
                    v)]
         ;; Take absolute value and normalize
         [v-abs (vec-map abs v-pos)]
         [total (vec-fold + 0 v-abs)])
        (if (< total 1e-15)
            v-abs  ; All zeros
            (vec-scale (/ 1.0 total) v-abs))))

;;; eigenvector-centrality-from-edges : (List Edge) × [Nat] × [Nat] × [Num] → Vec
;;; Compute eigenvector centrality from edge list.
(define (eigenvector-centrality-from-edges edges . opts)
  (let* ([n (if (and (pair? opts) (integer? (car opts)))
                (car opts)
                (infer-node-count edges))]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [adj (edges->adjacency-matrix edges n #t)])  ; undirected
        (apply eigenvector-centrality (cons adj rest1))))

;;; ====
;;; Katz Centrality
;;; ====

;;; katz-centrality : Matrix × [Num] × [Num] × [Nat] × [Num] → Vec | Error
;;;
;;; Compute Katz centrality for each node.
;;; Katz centrality is a variant of eigenvector centrality with
;;; an attenuation factor α and a baseline β.
;;;
;;; Formula: x = β × (I - αA)^(-1) × 1 = β × Σ_{k=0}^∞ (αA)^k × 1
;;;
;;; Arguments:
;;;   adj   - Adjacency matrix
;;;   alpha - Attenuation factor (must be < 1/spectral_radius for convergence)
;;;           Default: 0.1 (conservative)
;;;   beta  - Baseline centrality (default: 1.0)
;;;   max-iter - Maximum iterations (default: 100)
;;;   tol   - Convergence tolerance (default: 1e-8)
;;;
;;; Returns: Vector of Katz centrality scores
;;;
;;; Notes:
;;;   - Unlike eigenvector centrality, all nodes get non-zero score
;;;   - α controls how much weight distant connections receive
;;;   - Large α → behaves like eigenvector centrality
;;;   - Small α → more uniform scores
;;;
;;; Example:
;;;   (katz-centrality adj 0.1) => all nodes get positive scores
(define (katz-centrality adj . opts)
  (let* ([n (matrix-rows adj)]
         [m (matrix-cols adj)])
        (if (not (= n m))
            `(error not-square ,n ,m)
            (let* ([alpha (if (and (pair? opts) (number? (car opts)))
                              (car opts)
                              0.1)]
                   [rest1 (if (and (pair? opts) (number? (car opts)))
                              (cdr opts)
                              opts)]
                   [beta (if (and (pair? rest1) (number? (car rest1)))
                             (car rest1)
                             1.0)]
                   [rest2 (if (and (pair? rest1) (number? (car rest1)))
                              (cdr rest1)
                              rest1)]
                   [max-iter (if (and (pair? rest2) (integer? (car rest2)))
                                 (car rest2)
                                 *centrality-max-iterations*)]
                   [rest3 (if (and (pair? rest2) (integer? (car rest2)))
                              (cdr rest2)
                              rest2)]
                   [tol (if (and (pair? rest3) (number? (car rest3)))
                            (car rest3)
                            *centrality-tolerance*)]
                   ;; Use power series: x = β × Σ (αA)^k × 1
                   ;; Initialize with ones vector
                   [ones (make-vec n 1.0)])
                  (katz-power-series adj alpha beta ones max-iter tol)))))

;;; katz-power-series : Matrix × Num × Num × Vec × Nat × Num → Vec
;;; Compute Katz centrality via power series expansion.
;;; x = β × (1 + αA×1 + α²A²×1 + ...)
(define (katz-power-series adj alpha beta ones max-iter tol)
  (let ([n (matrix-rows adj)])
       (let loop ([x (vec-scale beta ones)]     ; Running sum
                  [term (vec-scale beta ones)]  ; Current term: β × (αA)^k × 1
                  [iter 0])
            (if (>= iter max-iter)
                x
                ;; term_{k+1} = α × A × term_k
                (let* ([a-term (matrix-vec-mul adj term)]
                       [new-term (vec-scale alpha a-term)]
                       [term-norm (vec-norm new-term)])
                      (if (< term-norm tol)
                          x  ; Converged
                          (loop (vec-add x new-term)
                                new-term
                                (+ iter 1))))))))

;;; katz-centrality-from-edges : (List Edge) × [Nat] × [Num] × [Num] → Vec
;;; Compute Katz centrality from edge list.
(define (katz-centrality-from-edges edges . opts)
  (let* ([n (if (and (pair? opts) (integer? (car opts)))
                (car opts)
                (infer-node-count edges))]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [adj (edges->adjacency-matrix edges n)])
        (apply katz-centrality (cons adj rest1))))

;;; ====
;;; Closeness Centrality
;;; ====

;;; closeness-centrality : Matrix × [Boolean] → Vec
;;;
;;; Compute closeness centrality for each node.
;;; Closeness centrality measures how close a node is to all others.
;;;
;;; Formula: C(v) = (n-1) / Σ d(v,u) for all u ≠ v
;;;
;;; Arguments:
;;;   dist      - Distance matrix (from floyd-warshall or similar)
;;;   harmonic  - If #t, use harmonic mean (handles disconnected graphs)
;;;               Default: #f
;;;
;;; Returns: Vector of closeness centrality scores
;;;
;;; Notes:
;;;   - Standard closeness can give misleading results for disconnected graphs:
;;;     nodes in small isolated components may get inflated scores because they
;;;     have short paths to their few reachable neighbors. Use harmonic=true.
;;;   - Harmonic closeness: H(v) = Σ 1/d(v,u) handles disconnected graphs well
;;;   - Higher score = more central
;;;
;;; Example:
;;;   (closeness-centrality (floyd-warshall adj))         ; Connected graph
;;;   (closeness-centrality (floyd-warshall adj) #t)      ; Harmonic for disconnected
(define (closeness-centrality dist . opts)
  (let* ([n (matrix-rows dist)]
         [harmonic (if (pair? opts) (car opts) #f)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i
                         (if harmonic
                             (harmonic-closeness-node dist i n)
                             (standard-closeness-node dist i n))))))

;;; standard-closeness-node : Matrix × Nat × Nat → Num
;;; Standard closeness for a single node.
(define (standard-closeness-node dist i n)
  (let loop ([j 0] [sum 0] [reachable 0])
       (if (= j n)
           (if (= reachable 0)
               0  ; Isolated node
               (/ (- n 1.0) sum))  ; Normalized: (n-1) / sum
           (if (= i j)
               (loop (+ j 1) sum reachable)
               (let ([d (matrix-ref dist i j)])
                    (if (>= d *infinity*)
                        (loop (+ j 1) sum reachable)  ; Unreachable
                        (loop (+ j 1) (+ sum d) (+ reachable 1))))))))

;;; harmonic-closeness-node : Matrix × Nat × Nat → Num
;;; Harmonic closeness for a single node (robust to disconnected graphs).
(define (harmonic-closeness-node dist i n)
  (let loop ([j 0] [sum 0])
       (if (= j n)
           (/ sum (- n 1.0))  ; Normalize by n-1
           (if (= i j)
               (loop (+ j 1) sum)
               (let ([d (matrix-ref dist i j)])
                    (if (or (>= d *infinity*) (= d 0))
                        (loop (+ j 1) sum)  ; Unreachable or self
                        (loop (+ j 1) (+ sum (/ 1.0 d)))))))))

;;; closeness-centrality-from-adj : Matrix × [Boolean] → Vec
;;; Compute closeness centrality from adjacency matrix.
(define (closeness-centrality-from-adj adj . opts)
  (let ([dist (floyd-warshall adj)])
       (apply closeness-centrality (cons dist opts))))

;;; closeness-centrality-from-edges : (List Edge) × [Nat] × [Boolean] → Vec
;;; Compute closeness centrality from edge list.
(define (closeness-centrality-from-edges edges . opts)
  (let* ([n (if (and (pair? opts) (integer? (car opts)))
                (car opts)
                (infer-node-count edges))]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [adj (edges->adjacency-matrix edges n)])
        (apply closeness-centrality-from-adj (cons adj rest1))))

;;; ====
;;; Betweenness Centrality
;;; ====

;;; betweenness-centrality : Matrix → Vec
;;;
;;; Compute betweenness centrality for each node using Brandes' algorithm.
;;; Betweenness measures how often a node lies on shortest paths between
;;; other nodes.
;;;
;;; Formula: B(v) = Σ σ_st(v) / σ_st
;;; where σ_st = number of shortest paths from s to t
;;;       σ_st(v) = number of those paths passing through v
;;;
;;; Arguments:
;;;   adj - Adjacency matrix (UNWEIGHTED: non-zero = edge exists)
;;;         Edge weights are ignored; all edges treated as distance 1.
;;;
;;; Returns: Vector of betweenness centrality scores (normalized to [0, 0.5])
;;;
;;; Complexity: O(nm) where m = edges, n = nodes
;;;
;;; Note: For weighted shortest-path betweenness, use Dijkstra-based variant
;;;       (not yet implemented).
;;;
;;; Example:
;;;   (betweenness-centrality (path-graph 5)) => middle node has highest score
(define (betweenness-centrality adj)
  (let* ([n (matrix-rows adj)]
         [cb (make-vector n 0.0)])
        ;; Run BFS/Dijkstra from each source
        (do ([s 0 (+ s 1)])
            ((= s n))
            (betweenness-from-source adj s cb n))
        ;; Normalize (divide by 2 for undirected, by (n-1)(n-2) for scale)
        (let ([norm (if (> n 2)
                        (* 2.0 (- n 1) (- n 2))
                        1.0)])
             (do ([i 0 (+ i 1)])
                 ((= i n) cb)
                 (vector-set! cb i (/ (vector-ref cb i) norm))))))

;;; betweenness-from-source : Matrix × Nat × Vec × Nat → Void
;;; Brandes' algorithm: BFS from source, then accumulate dependencies.
;;; Uses level-by-level BFS with O(n) queue operations (not O(n²) append).
(define (betweenness-from-source adj s cb n)
  (let* ([dist (make-vector n -1)]        ; Distance from s (-1 = unvisited)
         [sigma (make-vector n 0)]        ; Number of shortest paths
         [pred (make-vector n '())]       ; Predecessors on shortest paths
         [delta (make-vector n 0.0)]      ; Dependency accumulator
         [stack '()])                     ; Nodes in order of discovery
        ;; Initialize source
        (vector-set! dist s 0)
        (vector-set! sigma s 1)
        ;; BFS level-by-level (avoids O(n²) append)
        (let bfs ([current-level (list s)])
             (when (not (null? current-level))
                   (let ([next-level '()])
                        ;; Process all nodes at current level
                        (for-each
                         (lambda (v)
                                 (set! stack (cons v stack))
                                 ;; Explore neighbors
                                 (do ([w 0 (+ w 1)])
                                     ((= w n))
                                     (when (> (matrix-ref adj v w) 0)
                                           (cond
                                            ;; First visit to w
                                            [(= (vector-ref dist w) -1)
                                             (vector-set! dist w (+ (vector-ref dist v) 1))
                                             (set! next-level (cons w next-level))
                                             (vector-set! sigma w (vector-ref sigma v))
                                             (vector-set! pred w (list v))]
                                            ;; Another shortest path to w
                                            [(= (vector-ref dist w) (+ (vector-ref dist v) 1))
                                             (vector-set! sigma w (+ (vector-ref sigma w)
                                                                     (vector-ref sigma v)))
                                             (vector-set! pred w (cons v (vector-ref pred w)))]))))
                         current-level)
                        (bfs next-level))))
        ;; Accumulate dependencies (back-propagation)
        (for-each
         (lambda (w)
                 (for-each
                  (lambda (v)
                          (let ([frac (/ (vector-ref sigma v)
                                         (vector-ref sigma w)
                                         1.0)])
                               (vector-set! delta v
                                            (+ (vector-ref delta v)
                                               (* frac (+ 1 (vector-ref delta w)))))))
                  (vector-ref pred w))
                 ;; Add to betweenness (except for source)
                 (when (not (= w s))
                       (vector-set! cb w (+ (vector-ref cb w)
                                            (vector-ref delta w)))))
         stack)))

;;; betweenness-centrality-from-edges : (List Edge) × [Nat] → Vec
;;; Compute betweenness centrality from edge list.
(define (betweenness-centrality-from-edges edges . opts)
  (let* ([n (if (and (pair? opts) (integer? (car opts)))
                (car opts)
                (infer-node-count edges))]
         [adj (edges->adjacency-matrix edges n)])
        (betweenness-centrality adj)))

;;; ====
;;; Centrality Comparison and Utilities
;;; ====

;;; rank-by-centrality : Vec → (List (Nat . Num))
;;; Return nodes ranked by centrality score (highest first).
;;; Returns list of (node-index . score) pairs.
(define (rank-by-centrality scores)
  (let* ([n (vector-length scores)]
         [pairs (let loop ([i 0] [acc '()])
                     (if (= i n)
                         acc
                         (loop (+ i 1)
                               (cons (cons i (vector-ref scores i)) acc))))])
        (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs)))

;;; top-k-central : Vec × Nat → (List (Nat . Num))
;;; Return top k nodes by centrality.
(define (top-k-central scores k)
  (let ([ranked (rank-by-centrality scores)])
       (take-up-to ranked k)))

;;; take-up-to : (List a) × Nat → (List a)
;;; Take up to n elements.
(define (take-up-to lst n)
  (let loop ([l lst] [count n] [acc '()])
       (if (or (null? l) (<= count 0))
           (reverse acc)
           (loop (cdr l) (- count 1) (cons (car l) acc)))))

;;; centrality-correlation : Vec × Vec → Num
;;; Compute Pearson correlation between two centrality measures.
;;; Returns value in [-1, 1].
(define (centrality-correlation c1 c2)
  (let* ([n (vector-length c1)]
         [mean1 (/ (vec-fold + 0 c1) n)]
         [mean2 (/ (vec-fold + 0 c2) n)]
         [num 0] [den1 0] [den2 0])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (let ([d1 (- (vector-ref c1 i) mean1)]
                  [d2 (- (vector-ref c2 i) mean2)])
                 (set! num (+ num (* d1 d2)))
                 (set! den1 (+ den1 (* d1 d1)))
                 (set! den2 (+ den2 (* d2 d2)))))
        (let ([denom (sqrt (* den1 den2))])
             (if (< denom 1e-15)
                 0
                 (/ num denom)))))

;;; all-centralities : Matrix → (List (Symbol . Vec))
;;; Compute all centrality measures for comparison.
;;; Returns alist of (name . scores) pairs.
(define (all-centralities adj)
  (let ([dist (floyd-warshall adj)])
       (list (cons 'eigenvector (eigenvector-centrality adj))
             (cons 'katz (katz-centrality adj))
             (cons 'closeness (closeness-centrality dist))
             (cons 'betweenness (betweenness-centrality adj)))))

;;; centrality-summary : Matrix → String
;;; Human-readable summary of centrality rankings.
(define (centrality-summary adj)
  (let* ([n (matrix-rows adj)]
         [all (all-centralities adj)]
         [ev-top (car (top-k-central (cdr (assoc 'eigenvector all)) 1))]
         [katz-top (car (top-k-central (cdr (assoc 'katz all)) 1))]
         [close-top (car (top-k-central (cdr (assoc 'closeness all)) 1))]
         [btwn-top (car (top-k-central (cdr (assoc 'betweenness all)) 1))])
        (string-append
         "Centrality summary for " (number->string n) " node graph:\n"
         "  Most influential (eigenvector): node " (number->string (car ev-top)) "\n"
         "  Best connected (Katz): node " (number->string (car katz-top)) "\n"
         "  Most central (closeness): node " (number->string (car close-top)) "\n"
         "  Most bridge-like (betweenness): node " (number->string (car btwn-top)))))
