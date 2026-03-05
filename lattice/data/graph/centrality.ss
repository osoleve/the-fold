;;; lattice/data/graph/centrality.ss — Graph Centrality Measures
;;; @module centrality
;;; @requires prelude sort iteration
;;; @description Eigenvector, Katz, closeness, betweenness centrality
;;; @purity total
;;; @stability stable

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'sort)
(require 'vec)
(require 'matrix)
(require 'graph-matrix)
(require 'iteration)

(doc 'module 'centrality)
(doc 'purity 'partial)
(doc 'description "Graph centrality measures: eigenvector, Katz, closeness, and betweenness centrality")
(doc 'layer 'lattice)

(doc 'note "Matrix-based centrality metrics for graph analysis:
  - Eigenvector centrality (dominant eigenvector of adjacency matrix)
  - Katz centrality (attenuated walk counts)
  - Closeness centrality (inverse of average distance)
  - Betweenness centrality (fraction of shortest paths through node)")

(doc 'note "Quick Start:
  (load \"lattice/data/graph/graph-matrix.ss\")  ; Also loads matrix deps
  (load \"lattice/data/graph/centrality.ss\")

  ;; Create a graph
  (define g (star-graph 5))  ; Star with center 0, leaves 1-4

  ;; Compute centralities
  (eigenvector-centrality g)  ; => #(0.33 0.17 0.17 0.17 0.17)
  (katz-centrality g)         ; => All positive, center highest
  (closeness-centrality (floyd-warshall g))  ; From distance matrix
  (betweenness-centrality g)  ; => Center has highest betweenness

  ;; Compare and rank
  (rank-by-centrality (eigenvector-centrality g))  ; Sorted by score
  (top-k-central (katz-centrality g) 3)            ; Top 3 nodes
  (centrality-correlation evc katz)                ; Pearson r
  (all-centralities g)                             ; All four measures")

(doc 'note "Dependencies (must be loaded by client in correct order):
  - prelude.ss
  - vec.ss
  - matrix.ss
  - graph-matrix.ss (provides floyd-warshall, star-graph, etc.)")

(doc 'constant '*centrality-tolerance*)
(doc 'description "Default tolerance for convergence")
(define *centrality-tolerance* 1e-8)

(doc 'constant '*centrality-max-iterations*)
(doc 'description "Default maximum iterations")
(define *centrality-max-iterations* 100)

(doc eigenvector-centrality 'type '(-> Matrix [Nat] [Num] (Union Vec Error)))
(doc eigenvector-centrality 'description "Compute eigenvector centrality for each node using power iteration")
(doc eigenvector-centrality 'param 'adj "Adjacency matrix (undirected graphs work best)")
(doc eigenvector-centrality 'param 'max-iter "Maximum iterations (default: 100)")
(doc eigenvector-centrality 'param 'tol "Convergence tolerance (default: 1e-8)")
(doc eigenvector-centrality 'returns "Vector of centrality scores (non-negative, normalized)")
(doc eigenvector-centrality 'note "Works best for connected graphs; for directed graphs, considers both in-links; disconnected components may have zero centrality")
(doc eigenvector-centrality 'note "For node i: x_i = (1/λ) × Σ A_ij × x_j where λ is the largest eigenvalue")
(define (eigenvector-centrality adj . opts)
  (doc 'export #t)
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

(doc eigenvector-power-iteration 'type '(-> Matrix Vec Nat Num Vec))
(doc eigenvector-power-iteration 'description "Power iteration specialized for centrality; detects period-2 oscillation and averages oscillating vectors")
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

(doc vec-distance 'type '(-> Vec Vec Num))
(doc vec-distance 'description "Euclidean distance between two vectors")
(define (vec-distance v1 v2)
  (let ([n (vector-length v1)])
       (sqrt (let loop ([i 0] [sum 0])
                  (if (= i n)
                      sum
                      (let ([d (- (vector-ref v1 i) (vector-ref v2 i))])
                           (loop (+ i 1) (+ sum (* d d)))))))))

(doc eigenvector-average-oscillation 'type '(-> Vec Vec Vec))
(doc eigenvector-average-oscillation 'description "Average two oscillating vectors and normalize; for period-2 oscillation, averaging recovers the true eigenvector")
(define (eigenvector-average-oscillation v1 v2)
  (let* ([n (vector-length v1)]
         [avg (vec-tabulate n i (/ (+ (vector-ref v1 i) (vector-ref v2 i)) 2.0))])
        ;; Normalize
        (let ([norm (vec-norm avg)])
             (if (< norm 1e-15)
                 avg
                 (vec-scale (/ 1.0 norm) avg)))))

(doc eigenvector-normalize 'type '(-> Vec Vec))
(doc eigenvector-normalize 'description "Normalize eigenvector to non-negative values summing to 1")
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

(doc eigenvector-centrality-from-edges 'type '(-> (List Edge) [Nat] [Nat] [Num] Vec))
(doc eigenvector-centrality-from-edges 'description "Compute eigenvector centrality from edge list")
(define (eigenvector-centrality-from-edges edges . opts)
  (doc 'export #t)
  (let* ([n (if (and (pair? opts) (integer? (car opts)))
                (car opts)
                (infer-node-count edges))]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [adj (edges->adjacency-matrix edges n #t)])  ; undirected
        (apply eigenvector-centrality (cons adj rest1))))

(doc katz-centrality 'type '(-> Matrix [Num] [Num] [Nat] [Num] (Union Vec Error)))
(doc katz-centrality 'description "Compute Katz centrality using power series expansion: x = β × Σ(αA)^k × 1")
(doc katz-centrality 'param 'adj "Adjacency matrix")
(doc katz-centrality 'param 'alpha "Attenuation factor (must be < 1/spectral_radius; default: 0.1)")
(doc katz-centrality 'param 'beta "Baseline centrality (default: 1.0)")
(doc katz-centrality 'param 'max-iter "Maximum iterations (default: 100)")
(doc katz-centrality 'param 'tol "Convergence tolerance (default: 1e-8)")
(doc katz-centrality 'returns "Vector of Katz centrality scores")
(doc katz-centrality 'note "Unlike eigenvector centrality, all nodes get non-zero score; α controls how much weight distant connections receive")
(define (katz-centrality adj . opts)
  (doc 'export #t)
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

(doc katz-power-series 'type '(-> Matrix Num Num Vec Nat Num Vec))
(doc katz-power-series 'description "Compute Katz centrality via power series expansion: x = β × (1 + αA×1 + α²A²×1 + ...)")
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

(doc katz-centrality-from-edges 'type '(-> (List Edge) [Nat] [Num] [Num] Vec))
(doc katz-centrality-from-edges 'description "Compute Katz centrality from edge list")
(define (katz-centrality-from-edges edges . opts)
  (doc 'export #t)
  (let* ([n (if (and (pair? opts) (integer? (car opts)))
                (car opts)
                (infer-node-count edges))]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [adj (edges->adjacency-matrix edges n)])
        (apply katz-centrality (cons adj rest1))))

(doc closeness-centrality 'type '(-> Matrix [Boolean] Vec))
(doc closeness-centrality 'description "Compute closeness centrality: C(v) = (n-1) / Σ d(v,u) for all u ≠ v")
(doc closeness-centrality 'param 'dist "Distance matrix (from floyd-warshall or similar)")
(doc closeness-centrality 'param 'harmonic "If #t, use harmonic mean (handles disconnected graphs); default: #f")
(doc closeness-centrality 'returns "Vector of closeness centrality scores")
(doc closeness-centrality 'note "Standard closeness can give misleading results for disconnected graphs; use harmonic=true. Harmonic closeness: H(v) = Σ 1/d(v,u)")
(define (closeness-centrality dist . opts)
  (doc 'export #t)
  (let* ([n (matrix-rows dist)]
         [harmonic (if (pair? opts) (car opts) #f)])
        (vec-tabulate n i
          (if harmonic
              (harmonic-closeness-node dist i n)
              (standard-closeness-node dist i n)))))

(doc standard-closeness-node 'type '(-> Matrix Nat Nat Num))
(doc standard-closeness-node 'description "Standard closeness for a single node")
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

(doc harmonic-closeness-node 'type '(-> Matrix Nat Nat Num))
(doc harmonic-closeness-node 'description "Harmonic closeness for a single node (robust to disconnected graphs)")
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

(doc closeness-centrality-from-adj 'type '(-> Matrix [Boolean] Vec))
(doc closeness-centrality-from-adj 'description "Compute closeness centrality from adjacency matrix")
(define (closeness-centrality-from-adj adj . opts)
  (doc 'export #t)
  (let ([dist (floyd-warshall adj)])
       (apply closeness-centrality (cons dist opts))))

(doc closeness-centrality-from-edges 'type '(-> (List Edge) [Nat] [Boolean] Vec))
(doc closeness-centrality-from-edges 'description "Compute closeness centrality from edge list")
(define (closeness-centrality-from-edges edges . opts)
  (doc 'export #t)
  (let* ([n (if (and (pair? opts) (integer? (car opts)))
                (car opts)
                (infer-node-count edges))]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [adj (edges->adjacency-matrix edges n)])
        (apply closeness-centrality-from-adj (cons adj rest1))))

(doc betweenness-centrality 'type '(-> Matrix Vec))
(doc betweenness-centrality 'description "Compute betweenness centrality using Brandes' algorithm: B(v) = Σ σ_st(v) / σ_st")
(doc betweenness-centrality 'param 'adj "Adjacency matrix (UNWEIGHTED: non-zero = edge exists)")
(doc betweenness-centrality 'returns "Vector of betweenness centrality scores (normalized to [0, 0.5])")
(doc betweenness-centrality 'note "Complexity: O(nm) where m = edges, n = nodes")
(doc betweenness-centrality 'note "For weighted shortest-path betweenness, use Dijkstra-based variant (not yet implemented)")
(define (betweenness-centrality adj)
  (doc 'export #t)
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
             (vec-tabulate n i (/ (vector-ref cb i) norm)))))

(doc betweenness-from-source 'type '(-> Matrix Nat Vec Nat Void))
(doc betweenness-from-source 'description "Brandes' algorithm: BFS from source, then accumulate dependencies; uses level-by-level BFS")
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

(doc betweenness-centrality-from-edges 'type '(-> (List Edge) [Nat] Vec))
(doc betweenness-centrality-from-edges 'description "Compute betweenness centrality from edge list")
(define (betweenness-centrality-from-edges edges . opts)
  (doc 'export #t)
  (let* ([n (if (and (pair? opts) (integer? (car opts)))
                (car opts)
                (infer-node-count edges))]
         [adj (edges->adjacency-matrix edges n)])
        (betweenness-centrality adj)))

(doc rank-by-centrality 'type '(-> Vec (List (Pair Nat Num))))
(doc rank-by-centrality 'description "Return nodes ranked by centrality score (highest first)")
(define (rank-by-centrality scores)
  (doc 'export #t)
  (let* ([n (vector-length scores)]
         [pairs (let loop ([i 0] [acc '()])
                     (if (= i n)
                         acc
                         (loop (+ i 1)
                               (cons (cons i (vector-ref scores i)) acc))))])
        (sort-by (lambda (a b) (> (cdr a) (cdr b))) pairs)))

(doc top-k-central 'type '(-> Vec Nat (List (Pair Nat Num))))
(doc top-k-central 'description "Return top k nodes by centrality")
(define (top-k-central scores k)
  (doc 'export #t)
  (let ([ranked (rank-by-centrality scores)])
       (take k ranked)))

(doc centrality-correlation 'type '(-> Vec Vec Num))
(doc centrality-correlation 'description "Compute Pearson correlation between two centrality measures; returns value in [-1, 1]")
(define (centrality-correlation c1 c2)
  (doc 'export #t)
  (let* ([n (vector-length c1)]
         [mean1 (/ (vec-fold + 0 c1) n)]
         [mean2 (/ (vec-fold + 0 c2) n)])
        (let loop ([i 0] [num 0] [den1 0] [den2 0])
             (if (= i n)
                 (let ([denom (sqrt (* den1 den2))])
                      (if (< denom 1e-15)
                          0
                          (/ num denom)))
                 (let ([d1 (- (vector-ref c1 i) mean1)]
                       [d2 (- (vector-ref c2 i) mean2)])
                      (loop (+ i 1)
                            (+ num (* d1 d2))
                            (+ den1 (* d1 d1))
                            (+ den2 (* d2 d2))))))))

(doc all-centralities 'type '(-> Matrix (List (Pair Symbol Vec))))
(doc all-centralities 'description "Compute all centrality measures for comparison; returns alist of (name . scores) pairs")
(define (all-centralities adj)
  (doc 'export #t)
  (let ([dist (floyd-warshall adj)])
       (list (cons 'eigenvector (eigenvector-centrality adj))
             (cons 'katz (katz-centrality adj))
             (cons 'closeness (closeness-centrality dist))
             (cons 'betweenness (betweenness-centrality adj)))))

(doc centrality-summary 'type '(-> Matrix String))
(doc centrality-summary 'description "Human-readable summary of centrality rankings")
(define (centrality-summary adj)
  (doc 'export #t)
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
