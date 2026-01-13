;;; core/linalg/graph-laplacian.ss — Graph Laplacian Matrices
;;;
;;; @module graph-laplacian
;;; @requires prelude matrix matrix-eigen graph-matrix
;;;
;;; Laplacian matrices for spectral graph theory and analysis.
;;;
;;; Provides three types of Laplacian:
;;;   - Unnormalized (combinatorial): L = D - A
;;;   - Normalized symmetric: L_sym = I - D^(-1/2) A D^(-1/2)
;;;   - Random walk: L_rw = I - D^(-1) A
;;;
;;; Key properties of the Laplacian:
;;;   - L is positive semi-definite for undirected graphs
;;;   - Smallest eigenvalue is always 0 (with eigenvector [1,1,...,1])
;;;   - Number of zero eigenvalues = number of connected components
;;;   - Second smallest eigenvalue (Fiedler value) = algebraic connectivity
;;;
;;; Applications:
;;;   - Graph partitioning / spectral clustering
;;;   - Community detection
;;;   - Graph connectivity analysis
;;;   - Random walk simulation
;;;   - Network flow analysis
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies (loaded by client):
;;;   - prelude.ss
;;;   - matrix.ss
;;;   - matrix-eigen.ss
;;;   - graph-matrix.ss

;;; ============================================================
;;; Unnormalized (Combinatorial) Laplacian
;;; ============================================================

;;; laplacian : (Matrix Real) → (Matrix Real)
;;; L = D - A
;;; Compute the unnormalized Laplacian matrix.
;;;
;;; Properties:
;;;   - L[i,i] = degree(i)
;;;   - L[i,j] = -A[i,j] for i ≠ j
;;;   - Row sums and column sums are zero (for undirected)
;;;   - Positive semi-definite
;;;
;;; Example:
;;;   (laplacian (edges->adjacency-matrix '((0 1) (1 2) (2 0)) 3 #t))
;;;   ; Returns 3x3 Laplacian for triangle graph
(define (laplacian adj)
  (let* ([d (degree-matrix adj)]
         [a (if (sparse-csr? adj)
                (sparse-csr->dense adj)
                adj)])
        (matrix-sub d a)))

;;; laplacian-from-edges : (List Edge) × Nat → (Matrix Real)
;;; Convenience function to create Laplacian directly from edge list.
;;; Assumes undirected graph.
(define (laplacian-from-edges edges . opts)
  (let ([n (if (null? opts) #f (car opts))])
       (laplacian (edges->adjacency-matrix edges n #t))))

;;; ============================================================
;;; Normalized Symmetric Laplacian
;;; ============================================================

;;; laplacian-normalized : (Matrix Real) → (Matrix Real)
;;; L_sym = I - D^(-1/2) A D^(-1/2)
;;;       = D^(-1/2) L D^(-1/2)
;;;
;;; The normalized symmetric Laplacian has eigenvalues in [0, 2].
;;; More suitable for spectral clustering as it normalizes for degree.
;;;
;;; Properties:
;;;   - Eigenvalues in [0, 2] for undirected graphs
;;;   - Symmetric (inherits symmetry from A if undirected)
;;;   - Better behaved for graphs with varying node degrees
;;;
;;; Note: Isolated nodes (degree 0) cause division by zero.
;;; This function assigns 0 to such entries.
(define (laplacian-normalized adj)
  (let* ([n (adjacency-matrix-node-count adj)]
         [a (if (sparse-csr? adj)
                (sparse-csr->dense adj)
                adj)]
         [result (make-matrix n n 0)])
        ;; Compute D^(-1/2)
        (let ([d-inv-sqrt (make-vector n 0)])
             (do ([i 0 (+ i 1)])
                 ((= i n))
                 (let ([deg (adjacency-out-degree adj i)])
                      (vector-set! d-inv-sqrt i
                                   (if (= deg 0)
                                       0  ; Isolated node
                                       (/ 1.0 (sqrt deg))))))
             ;; L_sym[i,j] = I[i,j] - d[i]^(-1/2) * A[i,j] * d[j]^(-1/2)
             (do ([i 0 (+ i 1)])
                 ((= i n) result)
                 (do ([j 0 (+ j 1)])
                     ((= j n))
                     (let* ([d-i (vector-ref d-inv-sqrt i)]
                            [d-j (vector-ref d-inv-sqrt j)]
                            [a-ij (matrix-ref a i j)]
                            [term (if (= i j)
                                      (- 1 (* d-i a-ij d-j))
                                      (- (* d-i a-ij d-j)))])
                           (matrix-set! result i j term)))))))

;;; ============================================================
;;; Random Walk Laplacian
;;; ============================================================

;;; laplacian-random-walk : (Matrix Real) → (Matrix Real)
;;; L_rw = I - D^(-1) A = D^(-1) L
;;;
;;; The random walk Laplacian is related to Markov chains.
;;; D^(-1)A is the transition matrix for a random walk.
;;;
;;; Properties:
;;;   - Eigenvalues in [0, 2] for undirected graphs
;;;   - NOT symmetric (unless graph is regular)
;;;   - Eigenvectors are same as L_sym but NOT orthogonal
;;;
;;; Note: Isolated nodes (degree 0) cause division by zero.
;;; This function assigns 0 to rows of isolated nodes.
(define (laplacian-random-walk adj)
  (let* ([n (adjacency-matrix-node-count adj)]
         [a (if (sparse-csr? adj)
                (sparse-csr->dense adj)
                adj)]
         [result (make-matrix n n 0)])
        ;; Compute D^(-1)
        (let ([d-inv (make-vector n 0)])
             (do ([i 0 (+ i 1)])
                 ((= i n))
                 (let ([deg (adjacency-out-degree adj i)])
                      (vector-set! d-inv i
                                   (if (= deg 0)
                                       0  ; Isolated node
                                       (/ 1.0 deg)))))
             ;; L_rw[i,j] = I[i,j] - d[i]^(-1) * A[i,j]
             (do ([i 0 (+ i 1)])
                 ((= i n) result)
                 (let ([di (vector-ref d-inv i)])
                      (do ([j 0 (+ j 1)])
                          ((= j n))
                          (let* ([a-ij (matrix-ref a i j)]
                                 [term (if (= i j)
                                           (- 1 (* di a-ij))
                                           (- (* di a-ij)))])
                                (matrix-set! result i j term))))))))

;;; ============================================================
;;; Algebraic Connectivity (Fiedler Value)
;;; ============================================================

;;; algebraic-connectivity : (Matrix Real) → Real
;;; Compute the algebraic connectivity (second smallest eigenvalue).
;;;
;;; The Fiedler value λ_2 measures how well-connected the graph is:
;;;   - λ_2 = 0  ⟺  graph is disconnected
;;;   - λ_2 > 0  ⟺  graph is connected
;;;   - Higher λ_2 means more connected / harder to partition
;;;
;;; For optimization purposes, we compute eigenvalues of the Laplacian
;;; and return the second smallest one.
(define (algebraic-connectivity adj)
  (let* ([L (laplacian adj)]
         [eigs (eigenvalues L)])
        (if (and (pair? eigs) (eq? (car eigs) 'error))
            eigs
            (let ([sorted (vector-sort < eigs)])
                 ;; Return second smallest eigenvalue
                 ;; (first is always ~0 for connected graph)
                 (if (>= (vector-length sorted) 2)
                     (vector-ref sorted 1)
                     0)))))

;;; fiedler-vector : (Matrix Real) → (Vec Real)
;;; Compute the Fiedler vector (eigenvector of second smallest eigenvalue).
;;;
;;; The Fiedler vector is useful for:
;;;   - Graph partitioning (sign of components)
;;;   - Spectral layout of graphs
;;;   - Community detection
;;;
;;; Returns the eigenvector corresponding to λ_2.
(define (fiedler-vector adj)
  (let* ([L (laplacian adj)]
         [n (matrix-rows L)]
         ;; Use symmetric-eigen since Laplacian is symmetric
         [result (symmetric-eigen L)])
        (if (and (pair? result) (eq? (car result) 'error))
            result
            (let* ([eigenvalues (car result)]
                   [eigenvectors (cdr result)]
                   ;; Find index of second smallest eigenvalue
                   [idx (second-smallest-index eigenvalues)])
                  (matrix-col eigenvectors idx)))))

;;; second-smallest-index : (Vec Real) → Nat
;;; Find index of second smallest element in vector.
(define (second-smallest-index v)
  (let ([n (vector-length v)])
       (if (< n 2)
           0
           (let loop ([i 0]
                      [min-idx 0]
                      [min-val +inf.0]
                      [second-idx 0]
                      [second-val +inf.0])
                (if (= i n)
                    second-idx
                    (let ([val (abs (vector-ref v i))])
                         (cond
                          [(< val min-val)
                           ;; New minimum - old minimum becomes second
                           (loop (+ i 1) i val min-idx min-val)]
                          [(< val second-val)
                           ;; New second minimum
                           (loop (+ i 1) min-idx min-val i val)]
                          [else
                           (loop (+ i 1) min-idx min-val second-idx second-val)])))))))

;;; ============================================================
;;; Connected Components from Laplacian
;;; ============================================================

;;; laplacian-connected-components : (Matrix Real) → Nat
;;; Count connected components by counting near-zero eigenvalues.
;;;
;;; The multiplicity of eigenvalue 0 equals the number of
;;; connected components in the graph.
;;;
;;; Uses relative tolerance based on largest eigenvalue to handle
;;; numerical precision issues across different graph scales.
(define (laplacian-connected-components adj)
  (let* ([L (laplacian adj)]
         [eigs (eigenvalues L)])
        (if (and (pair? eigs) (eq? (car eigs) 'error))
            eigs  ; Return error
            (let* ([n (vector-length eigs)]
                   ;; Find max eigenvalue for relative tolerance
                   [max-eig (let loop ([i 0] [m 0])
                                 (if (= i n)
                                     m
                                     (loop (+ i 1) (max m (abs (vector-ref eigs i))))))]
                   ;; Relative tolerance with floor of 1e-8
                   [tol (* 1e-8 (max 1.0 max-eig))])
                  (let loop ([i 0] [count 0])
                       (if (= i n)
                           count
                           (loop (+ i 1)
                                 (if (< (abs (vector-ref eigs i)) tol)
                                     (+ count 1)
                                     count))))))))

;;; ============================================================
;;; Spectral Partitioning
;;; ============================================================

;;; spectral-partition : (Matrix Real) → (List Nat) × (List Nat)
;;; Partition graph into two groups using the Fiedler vector.
;;;
;;; Nodes with positive Fiedler vector components go to one group,
;;; nodes with negative components go to the other.
;;;
;;; This is a simple but effective graph bisection algorithm.
;;;
;;; Returns: (partition-A . partition-B) where each is a list of node indices.
(define (spectral-partition adj)
  (let ([fiedler (fiedler-vector adj)])
       (if (and (pair? fiedler) (eq? (car fiedler) 'error))
           fiedler
           (let ([n (vector-length fiedler)])
                (let loop ([i 0] [part-a '()] [part-b '()])
                     (if (= i n)
                         (cons (reverse part-a) (reverse part-b))
                         (if (>= (vector-ref fiedler i) 0)
                             (loop (+ i 1) (cons i part-a) part-b)
                             (loop (+ i 1) part-a (cons i part-b)))))))))

;;; extract-induced-subgraph : (Matrix Real) × (List Nat) → (Matrix Real)
;;; Extract the induced subgraph containing only the specified nodes.
;;; Returns a new adjacency matrix with nodes renumbered 0..n-1.
(define (extract-induced-subgraph adj nodes)
  (let* ([n (length nodes)]
         [node-vec (list->vector nodes)]
         [result (make-matrix n n 0)])
        ;; Build mapping from original indices to new indices
        (do ([new-i 0 (+ new-i 1)])
            [(= new-i n) result]
            (let ([orig-i (vector-ref node-vec new-i)])
                 (do ([new-j 0 (+ new-j 1)])
                     [(= new-j n)]
                     (let ([orig-j (vector-ref node-vec new-j)])
                          (matrix-set! result new-i new-j
                                       (matrix-ref adj orig-i orig-j))))))))

;;; remap-partition : (List Nat) × (List Nat) → (List Nat)
;;; Remap local node indices back to original node indices.
(define (remap-partition local-nodes original-nodes)
  (let ([node-vec (list->vector original-nodes)])
       (map (lambda (local-idx) (vector-ref node-vec local-idx))
            local-nodes)))

;;; spectral-partition-k : (Matrix Real) × Nat → (List (List Nat))
;;; Partition graph into k groups using recursive spectral bisection.
;;;
;;; Uses the Fiedler vector (second smallest eigenvector of Laplacian)
;;; to recursively bisect the graph until k partitions are obtained.
;;;
;;; Algorithm:
;;;   1. Compute Fiedler vector of current graph
;;;   2. Split nodes by sign of Fiedler components
;;;   3. Recursively partition each half with proper subgraph extraction
(define (spectral-partition-k adj k)
  (let ([n (adjacency-matrix-node-count adj)])
       (if (<= k 1)
           ;; Base case: return all nodes as one partition
           (list (let loop ([i 0] [nodes '()])
                      (if (= i n)
                          (reverse nodes)
                          (loop (+ i 1) (cons i nodes)))))
           (let ([result (spectral-partition adj)])
                (if (and (pair? result) (eq? (car result) 'error))
                    result
                    (let ([part-a (car result)]
                          [part-b (cdr result)])
                         (if (<= k 2)
                             ;; Two partitions requested - done
                             (list part-a part-b)
                             ;; Recursively partition with proper subgraph extraction
                             (let* ([k-a (quotient k 2)]
                                    [k-b (- k k-a)]
                                    ;; Partition the larger group more
                                    [partitions-a (subgraph-partition adj part-a k-a)]
                                    [partitions-b (subgraph-partition adj part-b k-b)])
                                   (if (and (pair? partitions-a) (eq? (car partitions-a) 'error))
                                       partitions-a
                                       (if (and (pair? partitions-b) (eq? (car partitions-b) 'error))
                                           partitions-b
                                           (append partitions-a partitions-b)))))))))))

;;; subgraph-partition : (Matrix Real) × (List Nat) × Nat → (List (List Nat))
;;; Partition a subgraph induced by given nodes using recursive bisection.
;;; Extracts the induced subgraph and recomputes Fiedler vector.
(define (subgraph-partition adj nodes k)
  (cond
   [(<= k 1) (list nodes)]
   [(<= (length nodes) 1) (list nodes)]
   [(<= (length nodes) k)
    ;; Not enough nodes to partition further - return as singletons
    (map list nodes)]
   [else
    ;; Extract induced subgraph and partition recursively
    (let* ([subgraph (extract-induced-subgraph adj nodes)]
           [result (spectral-partition subgraph)])
          (if (and (pair? result) (eq? (car result) 'error))
              ;; Fallback: split in half if spectral partition fails
              (let* ([mid (quotient (length nodes) 2)]
                     [part-a (take mid nodes)]
                     [part-b (drop mid nodes)])
                    (append (subgraph-partition adj part-a (quotient k 2))
                            (subgraph-partition adj part-b (- k (quotient k 2)))))
              ;; Remap local indices back to original and recurse
              (let* ([local-a (car result)]
                     [local-b (cdr result)]
                     [orig-a (remap-partition local-a nodes)]
                     [orig-b (remap-partition local-b nodes)]
                     [k-a (quotient k 2)]
                     [k-b (- k k-a)])
                    (append (subgraph-partition adj orig-a k-a)
                            (subgraph-partition adj orig-b k-b)))))]))

;;; take : Nat × (List α) → (List α)
(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;;; drop : Nat × (List α) → (List α)
(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

;;; ============================================================
;;; Laplacian Properties
;;; ============================================================

;;; laplacian-trace : (Matrix Real) → Real
;;; Compute trace of Laplacian (= 2 x number of edges for undirected).
(define (laplacian-trace L)
  (let ([n (matrix-rows L)])
       (let loop ([i 0] [sum 0])
            (if (= i n)
                sum
                (loop (+ i 1) (+ sum (matrix-ref L i i)))))))

;;; laplacian-energy : (Matrix Real) → Real
;;; Compute Laplacian energy (sum of absolute eigenvalues).
(define (laplacian-energy adj)
  (let* ([L (laplacian adj)]
         [eigs (eigenvalues L)])
        (if (and (pair? eigs) (eq? (car eigs) 'error))
            eigs
            (let ([n (vector-length eigs)])
                 (let loop ([i 0] [sum 0])
                      (if (= i n)
                          sum
                          (loop (+ i 1) (+ sum (abs (vector-ref eigs i))))))))))

;;; spectral-gap : (Matrix Real) → Real
;;; Compute spectral gap (lambda_2 - lambda_1 = lambda_2 for Laplacian since lambda_1 = 0).
;;; This is the same as algebraic connectivity for connected graphs.
(define (spectral-gap adj)
  (algebraic-connectivity adj))

;;; spectral-radius-laplacian : (Matrix Real) → Real
;;; Compute spectral radius (largest eigenvalue) of Laplacian.
(define (spectral-radius-laplacian adj)
  (let* ([L (laplacian adj)]
         [result (power-iteration L)])
        (if (and (pair? result) (eq? (car result) 'error))
            result
            (abs (car result)))))

;;; ============================================================
;;; Graph Regularity and Bipartiteness from Spectrum
;;; ============================================================

;;; laplacian-max-eigenvalue : (Matrix Real) → Real
;;; Compute largest eigenvalue of Laplacian.
;;; For d-regular graphs, lambda_max <= 2d.
(define (laplacian-max-eigenvalue adj)
  (let* ([L (laplacian adj)]
         [eigs (eigenvalues L)])
        (if (and (pair? eigs) (eq? (car eigs) 'error))
            eigs
            (let ([n (vector-length eigs)])
                 (let loop ([i 0] [max-val 0])
                      (if (= i n)
                          max-val
                          (loop (+ i 1)
                                (max max-val (vector-ref eigs i)))))))))

;;; is-bipartite-spectral? : (Matrix Real) × Real → Boolean
;;; Test if graph is bipartite using Laplacian spectrum.
;;;
;;; A connected graph is bipartite if and only if lambda_max = 2 x d_max
;;; (for the normalized Laplacian, lambda_max = 2 iff bipartite).
(define (is-bipartite-spectral? adj . opts)
  (let* ([tol (if (null? opts) 1e-6 (car opts))]
         [L-norm (laplacian-normalized adj)]
         [eigs (eigenvalues L-norm)])
        (if (and (pair? eigs) (eq? (car eigs) 'error))
            eigs
            (let ([max-eig (let loop ([i 0] [m 0])
                                (if (= i (vector-length eigs))
                                    m
                                    (loop (+ i 1) (max m (vector-ref eigs i)))))])
                 (< (abs (- max-eig 2.0)) tol)))))

;;; ============================================================
;;; Effective Resistance
;;; ============================================================

;;; effective-resistance : (Matrix Real) × Nat × Nat → Real
;;; Compute effective resistance between nodes i and j.
;;;
;;; R_eff(i,j) = (e_i - e_j)^T L^+ (e_i - e_j)
;;;
;;; where L^+ is the Moore-Penrose pseudoinverse of L.
;;;
;;; Effective resistance has applications in:
;;;   - Network analysis (how "far apart" are two nodes)
;;;   - Graph sparsification
;;;   - Random spanning trees
;;;
;;; Note: This is computationally expensive as it requires
;;; eigendecomposition. For many queries, precompute L^+.
(define (effective-resistance adj i j)
  (let* ([L (laplacian adj)]
         [n (matrix-rows L)]
         [result (symmetric-eigen L)])
        (if (and (pair? result) (eq? (car result) 'error))
            result
            (let* ([eigenvalues (car result)]
                   [eigenvectors (cdr result)]
                   [tol 1e-10])
                  ;; R_eff = sum_k (1/λ_k) * (u_k[i] - u_k[j])^2
                  ;; Skip k where λ_k ≈ 0 (null space)
                  (let loop ([k 0] [sum 0])
                       (if (= k n)
                           sum
                           (let ([lambda-k (vector-ref eigenvalues k)])
                                (if (< (abs lambda-k) tol)
                                    (loop (+ k 1) sum)
                                    (let* ([u-k (matrix-col eigenvectors k)]
                                           [diff (- (vector-ref u-k i)
                                                    (vector-ref u-k j))])
                                          (loop (+ k 1)
                                                (+ sum (/ (* diff diff) lambda-k))))))))))))

;;; total-effective-resistance : (Matrix Real) → Real
;;; Compute total effective resistance (Kirchhoff index).
;;;
;;; K = n × sum_k (1/λ_k) for k > 0
;;;
;;; The Kirchhoff index is a measure of overall graph connectivity.
(define (total-effective-resistance adj)
  (let* ([L (laplacian adj)]
         [n (matrix-rows L)]
         [eigs (eigenvalues L)])
        (if (and (pair? eigs) (eq? (car eigs) 'error))
            eigs
            (let ([tol 1e-10])
                 (* n (let loop ([i 0] [sum 0])
                           (if (= i (vector-length eigs))
                               sum
                               (let ([lambda-i (vector-ref eigs i)])
                                    (if (< (abs lambda-i) tol)
                                        (loop (+ i 1) sum)
                                        (loop (+ i 1) (+ sum (/ 1.0 lambda-i))))))))))))

;;; ============================================================
;;; Summary Functions
;;; ============================================================

;;; laplacian-summary : (Matrix Real) → (List (Symbol × α))
;;; Compute summary statistics of the Laplacian spectrum.
(define (laplacian-summary adj)
  (let* ([L (laplacian adj)]
         [n (matrix-rows L)]
         [eigs (eigenvalues L)])
        (if (and (pair? eigs) (eq? (car eigs) 'error))
            eigs
            (let* ([sorted (vector-sort < eigs)]
                   [lambda-1 (vector-ref sorted 0)]
                   [lambda-2 (if (>= n 2) (vector-ref sorted 1) 0)]
                   [lambda-max (vector-ref sorted (- n 1))])
                  `((nodes . ,n)
                    (smallest-eigenvalue . ,lambda-1)
                    (algebraic-connectivity . ,lambda-2)
                    (largest-eigenvalue . ,lambda-max)
                    (connected-components . ,(laplacian-connected-components adj))
                    (trace . ,(laplacian-trace L))
                    (energy . ,(laplacian-energy adj)))))))

;;; ============================================================
;;; Spectral Clustering (Normalized)
;;; ============================================================

;;; spectral-clustering : (Matrix Real) × Nat → (Vector Nat)
;;; Normalized spectral clustering using Ng-Jordan-Weiss algorithm.
;;;
;;; Algorithm:
;;;   1. Compute normalized Laplacian L_sym
;;;   2. Find k smallest eigenvectors
;;;   3. Normalize rows to unit length
;;;   4. Apply k-means clustering
;;;
;;; Returns: Vector of cluster labels (0 to k-1)
(define (spectral-clustering adj k)
  (let* ([n (adjacency-matrix-node-count adj)]
         [L-sym (laplacian-normalized adj)]
         ;; Get eigendecomposition
         [result (symmetric-eigen L-sym)])
    (if (and (pair? result) (eq? (car result) 'error))
        result
        (let* ([eigenvalues (car result)]
               [eigenvectors (cdr result)]
               ;; Find indices of k smallest eigenvalues
               [k-indices (k-smallest-indices eigenvalues k)]
               ;; Extract k eigenvectors as columns → rows for clustering
               [embedding (spectral-embedding eigenvectors k-indices n)])
          ;; Apply k-means to embedded points
          (kmeans-cluster embedding k)))))

;;; spectral-clustering-unnorm : (Matrix Real) × Nat → (Vector Nat)
;;; Unnormalized spectral clustering.
;;;
;;; Uses unnormalized Laplacian L = D - A.
;;; Simpler but less robust to varying node degrees.
(define (spectral-clustering-unnorm adj k)
  (let* ([n (adjacency-matrix-node-count adj)]
         [L (laplacian adj)]
         [result (symmetric-eigen L)])
    (if (and (pair? result) (eq? (car result) 'error))
        result
        (let* ([eigenvalues (car result)]
               [eigenvectors (cdr result)]
               [k-indices (k-smallest-indices eigenvalues k)]
               [embedding (spectral-embedding eigenvectors k-indices n)])
          (kmeans-cluster embedding k)))))

;;; k-smallest-indices : (Vector Real) × Nat → (List Nat)
;;; Find indices of k smallest values in vector.
(define (k-smallest-indices v k)
  (let* ([n (vector-length v)]
         ;; Create list of (value . index) pairs
         [pairs (let loop ([i 0] [acc '()])
                  (if (= i n)
                      acc
                      (loop (+ i 1) (cons (cons (abs (vector-ref v i)) i) acc))))]
         ;; Sort by value
         [sorted (list-sort (lambda (a b) (< (car a) (car b))) pairs)]
         ;; Take first k indices
         [k-actual (min k n)])
    (let loop ([remaining sorted] [count 0] [result '()])
      (if (or (= count k-actual) (null? remaining))
          (reverse result)
          (loop (cdr remaining) (+ count 1) (cons (cdar remaining) result))))))

;;; spectral-embedding : Matrix × (List Nat) × Nat → Matrix
;;; Extract spectral embedding: n points in k-dimensional space.
;;; Each row is a point (node), each column is an eigenvector coordinate.
;;; Rows are normalized to unit length for Ng-Jordan-Weiss algorithm.
(define (spectral-embedding eigenvectors k-indices n)
  (let* ([k (length k-indices)]
         [idx-vec (list->vector k-indices)]
         [result (make-matrix n k 0)])
    ;; Copy eigenvector columns to result matrix
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j k))
        (matrix-set! result i j
                     (matrix-ref eigenvectors i (vector-ref idx-vec j)))))
    ;; Normalize rows to unit length
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (let ([row-norm (sqrt (let loop ([j 0] [sum 0])
                              (if (= j k)
                                  sum
                                  (let ([v (matrix-ref result i j)])
                                    (loop (+ j 1) (+ sum (* v v)))))))])
        (when (> row-norm 1e-10)
          (do ([j 0 (+ j 1)])
              ((= j k))
            (matrix-set! result i j (/ (matrix-ref result i j) row-norm))))))))

;;; ============================================================
;;; K-Means Clustering
;;; ============================================================

;;; kmeans-cluster : Matrix × Nat → (Vector Nat)
;;; K-means clustering on rows of matrix.
;;; Returns vector of cluster assignments (0 to k-1).
;;;
;;; Uses k-means++ initialization for better starting points.
(define (kmeans-cluster points k . opts)
  (let* ([n (matrix-rows points)]
         [d (matrix-cols points)]
         [max-iter (if (pair? opts) (car opts) 100)]
         [tol (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) 1e-6)]
         ;; Initialize centroids using k-means++
         [centroids (kmeans-init-pp points k)]
         [labels (make-vector n 0)])
    ;; Iterate until convergence
    (let loop ([iter 0] [prev-centroids #f])
      (if (>= iter max-iter)
          labels
          (begin
            ;; Assignment step: assign each point to nearest centroid
            (do ([i 0 (+ i 1)])
                ((= i n))
              (vector-set! labels i (nearest-centroid points i centroids k d)))
            ;; Update step: recompute centroids
            (let ([new-centroids (recompute-centroids points labels k d n)])
              ;; Check convergence
              (if (and prev-centroids
                       (centroids-converged? prev-centroids new-centroids k d tol))
                  labels
                  (begin
                    (copy-matrix! new-centroids centroids k d)
                    (loop (+ iter 1) new-centroids)))))))))

;;; kmeans-init-pp : Matrix × Nat → Matrix
;;; K-means++ initialization: choose centroids with probability
;;; proportional to squared distance from existing centroids.
(define (kmeans-init-pp points k)
  (let* ([n (matrix-rows points)]
         [d (matrix-cols points)]
         [centroids (make-matrix k d 0)]
         ;; Choose first centroid uniformly at random (using simple hash)
         [first-idx (modulo (* 2654435761 n) n)])
    ;; Copy first centroid
    (do ([j 0 (+ j 1)])
        ((= j d))
      (matrix-set! centroids 0 j (matrix-ref points first-idx j)))
    ;; Choose remaining k-1 centroids
    (do ([c 1 (+ c 1)])
        ((= c k) centroids)
      ;; Compute squared distances to nearest centroid
      (let ([distances (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n))
          (let ([min-dist +inf.0])
            (do ([cc 0 (+ cc 1)])
                ((= cc c))
              (let ([dist (point-centroid-distance points i centroids cc d)])
                (set! min-dist (min min-dist dist))))
            (vector-set! distances i min-dist)))
        ;; Choose next centroid with probability proportional to distance²
        (let* ([total (let loop ([i 0] [s 0])
                        (if (= i n) s (loop (+ i 1) (+ s (vector-ref distances i)))))]
               ;; Simple deterministic selection: pick the farthest point
               [next-idx (let loop ([i 0] [max-idx 0] [max-dist 0])
                           (if (= i n)
                               max-idx
                               (if (> (vector-ref distances i) max-dist)
                                   (loop (+ i 1) i (vector-ref distances i))
                                   (loop (+ i 1) max-idx max-dist))))])
          ;; Copy centroid
          (do ([j 0 (+ j 1)])
              ((= j d))
            (matrix-set! centroids c j (matrix-ref points next-idx j))))))))

;;; nearest-centroid : Matrix × Nat × Matrix × Nat × Nat → Nat
;;; Find index of nearest centroid to point i.
(define (nearest-centroid points i centroids k d)
  (let loop ([c 0] [min-c 0] [min-dist +inf.0])
    (if (= c k)
        min-c
        (let ([dist (point-centroid-distance points i centroids c d)])
          (if (< dist min-dist)
              (loop (+ c 1) c dist)
              (loop (+ c 1) min-c min-dist))))))

;;; point-centroid-distance : Matrix × Nat × Matrix × Nat × Nat → Real
;;; Compute squared Euclidean distance from point to centroid.
(define (point-centroid-distance points i centroids c d)
  (let loop ([j 0] [sum 0])
    (if (= j d)
        sum
        (let ([diff (- (matrix-ref points i j) (matrix-ref centroids c j))])
          (loop (+ j 1) (+ sum (* diff diff)))))))

;;; recompute-centroids : Matrix × Vector × Nat × Nat × Nat → Matrix
;;; Recompute centroids as mean of assigned points.
(define (recompute-centroids points labels k d n)
  (let ([centroids (make-matrix k d 0)]
        [counts (make-vector k 0)])
    ;; Sum points per cluster
    (do ([i 0 (+ i 1)])
        ((= i n))
      (let ([c (vector-ref labels i)])
        (vector-set! counts c (+ 1 (vector-ref counts c)))
        (do ([j 0 (+ j 1)])
            ((= j d))
          (matrix-set! centroids c j
                       (+ (matrix-ref centroids c j) (matrix-ref points i j))))))
    ;; Divide by count
    (do ([c 0 (+ c 1)])
        ((= c k) centroids)
      (let ([cnt (vector-ref counts c)])
        (when (> cnt 0)
          (do ([j 0 (+ j 1)])
              ((= j d))
            (matrix-set! centroids c j (/ (matrix-ref centroids c j) cnt))))))))

;;; centroids-converged? : Matrix × Matrix × Nat × Nat × Real → Boolean
;;; Check if centroids have converged (total movement < tolerance).
(define (centroids-converged? old new k d tol)
  (let loop ([c 0] [total 0])
    (if (= c k)
        (< total tol)
        (let ([dist (let loop2 ([j 0] [sum 0])
                      (if (= j d)
                          sum
                          (let ([diff (- (matrix-ref new c j) (matrix-ref old c j))])
                            (loop2 (+ j 1) (+ sum (* diff diff))))))])
          (loop (+ c 1) (+ total dist))))))

;;; copy-matrix! : Matrix × Matrix × Nat × Nat → Void
;;; Copy src matrix to dst.
(define (copy-matrix! src dst rows cols)
  (do ([i 0 (+ i 1)])
      ((= i rows))
    (do ([j 0 (+ j 1)])
        ((= j cols))
      (matrix-set! dst i j (matrix-ref src i j)))))

;;; ============================================================
;;; Clustering Quality Metrics
;;; ============================================================

;;; conductance : (Matrix Real) × (List Nat) → Real
;;; Compute conductance of a cut (set of nodes).
;;;
;;; conductance(S) = cut(S, S̄) / min(vol(S), vol(S̄))
;;; where:
;;;   cut(S, S̄) = sum of edges between S and S̄
;;;   vol(S) = sum of degrees of nodes in S
;;;
;;; Lower conductance = better cut (sparser connection between clusters).
;;; Returns 0 if S or S̄ is empty or has zero volume.
(define (conductance adj nodes)
  (let* ([n (adjacency-matrix-node-count adj)]
         [node-set (list->set nodes n)]
         ;; Compute cut and volumes
         [cut-val 0]
         [vol-s 0]
         [vol-sbar 0])
    ;; Calculate all metrics in one pass
    (do ([i 0 (+ i 1)])
        ((= i n))
      (let ([deg-i (let loop ([j 0] [d 0])
                     (if (= j n) d
                         (loop (+ j 1) (+ d (if (> (matrix-ref adj i j) 0) 1 0)))))])
        (if (vector-ref node-set i)
            (begin
              (set! vol-s (+ vol-s deg-i))
              ;; Count edges to complement
              (do ([j 0 (+ j 1)])
                  ((= j n))
                (when (and (not (vector-ref node-set j))
                           (> (matrix-ref adj i j) 0))
                  (set! cut-val (+ cut-val 1)))))
            (set! vol-sbar (+ vol-sbar deg-i)))))
    ;; Return conductance
    (let ([min-vol (min vol-s vol-sbar)])
      (if (= min-vol 0)
          0
          (/ cut-val min-vol)))))

;;; list->set : (List Nat) × Nat → Vector
;;; Convert list of node indices to boolean vector.
(define (list->set nodes n)
  (let ([s (make-vector n #f)])
    (for-each (lambda (i) (when (< i n) (vector-set! s i #t))) nodes)
    s))

;;; ratio-cut : (Matrix Real) × (List Nat) → Real
;;; Compute ratio cut of a partition.
;;;
;;; ratio-cut(S) = cut(S, S̄) / |S| + cut(S, S̄) / |S̄|
;;;
;;; Minimizing ratio cut leads to balanced partitions.
(define (ratio-cut adj nodes)
  (let* ([n (adjacency-matrix-node-count adj)]
         [node-set (list->set nodes n)]
         [size-s (length nodes)]
         [size-sbar (- n size-s)]
         [cut-val 0])
    ;; Count edges crossing the cut
    (do ([i 0 (+ i 1)])
        ((= i n))
      (when (vector-ref node-set i)
        (do ([j 0 (+ j 1)])
            ((= j n))
          (when (and (not (vector-ref node-set j))
                     (> (matrix-ref adj i j) 0))
            (set! cut-val (+ cut-val 1))))))
    ;; Compute ratio cut
    (if (or (= size-s 0) (= size-sbar 0))
        +inf.0
        (+ (/ cut-val size-s) (/ cut-val size-sbar)))))

;;; normalized-cut : (Matrix Real) × (List Nat) → Real
;;; Compute normalized cut (Ncut) of a partition.
;;;
;;; Ncut(S) = cut(S, S̄) / vol(S) + cut(S, S̄) / vol(S̄)
;;;
;;; This is the objective minimized by normalized spectral clustering.
(define (normalized-cut adj nodes)
  (let* ([n (adjacency-matrix-node-count adj)]
         [node-set (list->set nodes n)]
         [cut-val 0]
         [vol-s 0]
         [vol-sbar 0])
    ;; Calculate cut and volumes
    (do ([i 0 (+ i 1)])
        ((= i n))
      (let ([deg-i (let loop ([j 0] [d 0])
                     (if (= j n) d
                         (loop (+ j 1) (+ d (if (> (matrix-ref adj i j) 0) 1 0)))))])
        (if (vector-ref node-set i)
            (begin
              (set! vol-s (+ vol-s deg-i))
              (do ([j 0 (+ j 1)])
                  ((= j n))
                (when (and (not (vector-ref node-set j))
                           (> (matrix-ref adj i j) 0))
                  (set! cut-val (+ cut-val 1)))))
            (set! vol-sbar (+ vol-sbar deg-i)))))
    (if (or (= vol-s 0) (= vol-sbar 0))
        +inf.0
        (+ (/ cut-val vol-s) (/ cut-val vol-sbar)))))
