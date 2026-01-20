(load "core/base/prelude.ss")

(doc 'module 'pagerank)
(doc 'description "PageRank importance scoring using eigenvalue computation")
(doc 'layer 'lattice)

(doc 'note "Based on 'The PageRank Citation Ranking: Bringing Order to the Web' by Page et al.")

(doc 'note "Algorithm:
  PR(p) = (1-d)/N + d * Σ(PR(q)/L(q))
  where q links to p, L(q) is out-degree of q, d is damping factor")

(doc 'note "Implementation uses power iteration on Google matrix:
  G = d*M + (1-d)*E/N
  where M is column-stochastic transition matrix and E is all-ones matrix")

(doc 'constant '*default-damping-factor*)
(doc 'description "Default damping factor (probability of following a link)")
(define *default-damping-factor* 0.85)

(doc 'constant '*pagerank-tolerance*)
(doc 'description "Default tolerance for convergence")
(define *pagerank-tolerance* 1e-8)

(doc 'constant '*pagerank-max-iterations*)
(doc 'description "Default maximum iterations")
(define *pagerank-max-iterations* 100)

(doc adjacency->transition 'type '(-> Matrix Matrix))
(doc adjacency->transition 'description "Convert adjacency matrix to column-stochastic transition matrix for PageRank")
(doc adjacency->transition 'note "Convention: A[i][j] = 1 means edge i→j; M[j][i] = 1/out-degree(i)")
(doc adjacency->transition 'note "Column i sums to 1 (node i's vote distributed to neighbors)")
(define (adjacency->transition adj)
  (let* ([n (matrix-rows adj)]
         [m (matrix-cols adj)])
        (if (not (= n m))
            `(error not-square ,n ,m)
            ;; Compute out-degrees (row sums - outgoing edges)
            (let* ([out-degrees (make-vector n 0)]
                   [_ (do ([i 0 (+ i 1)])
                          ((= i n))
                          (let ([row-sum (matrix-row-sum adj i)])
                               (vector-set! out-degrees i row-sum)))]
                   ;; Build transition matrix: M[j][i] = A[i][j] / out-degree(i)
                   [trans-data (make-vector (* n n) 0)])
                  (do ([i 0 (+ i 1)])
                      ((= i n))
                      (do ([j 0 (+ j 1)])
                          ((= j n))
                          (let ([a-ij (matrix-ref adj i j)]
                                [out-deg-i (vector-ref out-degrees i)])
                               ;; M[j][i] = A[i][j] / out-degree(i)
                               ;; If out-degree is 0 (dangling node), distribute evenly to all
                               (vector-set! trans-data (+ (* j n) i)
                                            (if (= out-deg-i 0)
                                                (/ 1.0 n)
                                                (/ a-ij out-deg-i))))))
                  (list 'matrix n n trans-data)))))

;;; matrix-row-sum : Matrix → Nat → Num
;;; Sum of elements in row i.
(define (matrix-row-sum m i)
  (let ([n (matrix-cols m)])
       (let loop ([j 0] [sum 0])
            (if (>= j n)
                sum
                (loop (+ j 1) (+ sum (matrix-ref m i j)))))))

;;; matrix-col-sum : Matrix → Nat → Num
;;; Sum of elements in column j.
(define (matrix-col-sum m j)
  (let ([n (matrix-rows m)])
       (let loop ([i 0] [sum 0])
            (if (>= i n)
                sum
                (loop (+ i 1) (+ sum (matrix-ref m i j)))))))

(doc make-google-matrix 'type '(-> Matrix Num Matrix))
(doc make-google-matrix 'description "Construct Google matrix: G = d*M + (1-d)*E/N")
(doc make-google-matrix 'param 'transition-matrix "Column-stochastic transition matrix M")
(doc make-google-matrix 'param 'damping-factor "Damping factor d")
(define (make-google-matrix transition-matrix damping-factor)
  (let* ([n (matrix-rows transition-matrix)]
         [teleport (/ (- 1.0 damping-factor) n)]
         [google-data (make-vector (* n n) teleport)])
        ;; Add damped transition probabilities
        (do ([i 0 (+ i 1)])
            ((= i n))
            (do ([j 0 (+ j 1)])
                ((= j n))
                (let* ([idx (+ (* i n) j)]
                       [m-ij (matrix-ref transition-matrix i j)]
                       [g-ij (+ (* damping-factor m-ij) teleport)])
                      (vector-set! google-data idx g-ij))))
        (list 'matrix n n google-data)))

(doc pagerank-from-matrix 'type '(-> Matrix [Num] [Nat] [Num] (Union Vec Error)))
(doc pagerank-from-matrix 'description "Compute PageRank scores from adjacency matrix using power iteration")
(doc pagerank-from-matrix 'param 'adj "Adjacency matrix (directed graph)")
(doc pagerank-from-matrix 'param 'damping "Damping factor (default: 0.85)")
(doc pagerank-from-matrix 'param 'max-iter "Maximum iterations (default: 100)")
(doc pagerank-from-matrix 'param 'tol "Convergence tolerance (default: 1e-8)")
(doc pagerank-from-matrix 'returns "Vector of PageRank scores (one per node)")
(define (pagerank-from-matrix adj . opts)
  (let* ([damping (if (and (pair? opts) (number? (car opts)))
                      (car opts)
                      *default-damping-factor*)]
         [rest1 (if (and (pair? opts) (number? (car opts)))
                    (cdr opts)
                    opts)]
         [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                       (car rest1)
                       *pagerank-max-iterations*)]
         [rest2 (if (and (pair? rest1) (integer? (car rest1)))
                    (cdr rest1)
                    rest1)]
         [tol (if (and (pair? rest2) (number? (car rest2)))
                  (car rest2)
                  *pagerank-tolerance*)])
        ;; Build transition matrix
        (let ([trans (adjacency->transition adj)])
             (if (and (pair? trans) (eq? (car trans) 'error))
                 trans
                 ;; Build Google matrix
                 (let* ([google (make-google-matrix trans damping)]
                        [n (matrix-rows google)]
                        ;; Initialize with uniform distribution
                        [v0 (make-vec n (/ 1.0 n))])
                       ;; Use power iteration to find stationary distribution
                       (pagerank-power-iteration google v0 max-iter tol))))))

(doc pagerank-power-iteration 'type '(-> Matrix Vec Nat Num (Union Vec Error)))
(doc pagerank-power-iteration 'description "Specialized power iteration for PageRank (doesn't normalize eigenvalue)")
(define (pagerank-power-iteration google v0 max-iter tol)
  (let loop ([v v0] [iter 0])
       (if (>= iter max-iter)
           v  ; Return best estimate even without convergence
           (let* ([v-new (matrix-vec-mul google v)]
                  ;; Normalize to ensure it's a probability distribution
                  [v-sum (vec-sum v-new)]
                  [v-normalized (if (> (abs v-sum) 1e-15)
                                    (vec-scale (/ 1.0 v-sum) v-new)
                                    v-new)]
                  ;; Check convergence: ||v_new - v||₁
                  [diff (vec-sub v-normalized v)]
                  [change (vec-norm-1 diff)])
                 (if (< change tol)
                     v-normalized
                     (loop v-normalized (+ iter 1)))))))

;;; vec-norm-1 : Vec → Num
;;; L1 norm (sum of absolute values).
(define (vec-norm-1 v)
  (vec-fold (lambda (acc x) (+ acc (abs x))) 0 v))

;;; vec-sum : Vec → Num
;;; Sum of all elements.
(define (vec-sum v)
  (vec-fold + 0 v))

(doc pagerank 'type '(-> (List Edge) [Nat] [Num] [Nat] [Num] (Union Vec Error)))
(doc pagerank 'description "Compute PageRank from edge list")
(doc pagerank 'param 'edges "Edge list ((from to) ...) or ((from to weight) ...)")
(doc pagerank 'param 'n "Number of nodes (default: inferred from edges)")
(doc pagerank 'param 'damping "Damping factor (default: 0.85)")
(doc pagerank 'param 'max-iter "Maximum iterations (default: 100)")
(doc pagerank 'param 'tol "Convergence tolerance (default: 1e-8)")
(doc pagerank 'returns "Vector of PageRank scores")
(define (pagerank edges . opts)
  (let* ([n (if (and (pair? opts) (integer? (car opts)))
                (car opts)
                (infer-node-count edges))]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)])
        ;; Build adjacency matrix
        (let ([adj (edges->adjacency-matrix edges n)])
             (if (and (pair? adj) (eq? (car adj) 'error))
                 adj
                 (apply pagerank-from-matrix (cons adj rest1))))))

(doc personalized-pagerank 'type '(-> Matrix Vec [Num] [Nat] [Num] (Union Vec Error)))
(doc personalized-pagerank 'description "Compute Personalized PageRank with custom teleportation vector")
(doc personalized-pagerank 'param 'adj "Adjacency matrix")
(doc personalized-pagerank 'param 'teleport-vec "Teleportation probability distribution (must sum to 1)")
(doc personalized-pagerank 'param 'damping "Damping factor (default: 0.85)")
(doc personalized-pagerank 'param 'max-iter "Maximum iterations (default: 100)")
(doc personalized-pagerank 'param 'tol "Convergence tolerance (default: 1e-8)")
(doc personalized-pagerank 'note "Standard PageRank uses uniform distribution; teleportation vector determines where random jumps go")
(define (personalized-pagerank adj teleport-vec . opts)
  (let* ([n (matrix-rows adj)]
         [damping (if (and (pair? opts) (number? (car opts)))
                      (car opts)
                      *default-damping-factor*)]
         [rest1 (if (and (pair? opts) (number? (car opts)))
                    (cdr opts)
                    opts)]
         [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                       (car rest1)
                       *pagerank-max-iterations*)]
         [rest2 (if (and (pair? rest1) (integer? (car rest1)))
                    (cdr rest1)
                    rest1)]
         [tol (if (and (pair? rest2) (number? (car rest2)))
                  (car rest2)
                  *pagerank-tolerance*)])
        ;; Validate teleport vector
        (if (not (= (vector-length teleport-vec) n))
            `(error dimension-mismatch ,n ,(vector-length teleport-vec))
            ;; Build transition matrix
            (let ([trans (adjacency->transition adj)])
                 (if (and (pair? trans) (eq? (car trans) 'error))
                     trans
                     ;; Build custom Google matrix with teleport vector
                     (let* ([google-data (make-vector (* n n) 0)])
                           (do ([i 0 (+ i 1)])
                               ((= i n))
                               (do ([j 0 (+ j 1)])
                                   ((= j n))
                                   (let* ([idx (+ (* i n) j)]
                                          [m-ij (matrix-ref trans i j)]
                                          [teleport (* (- 1.0 damping)
                                                       (vector-ref teleport-vec i))]
                                          [g-ij (+ (* damping m-ij) teleport)])
                                         (vector-set! google-data idx g-ij))))
                           (let* ([google (list 'matrix n n google-data)]
                                  [v0 (make-vec n (/ 1.0 n))])
                                 (pagerank-power-iteration google v0 max-iter tol))))))))

(doc top-k-nodes 'type '(-> Vec Nat (List (Pair Nat Num))))
(doc top-k-nodes 'description "Return top k nodes by PageRank score, sorted descending")
(define (top-k-nodes scores k)
  (let* ([n (vector-length scores)]
         [pairs (let loop ([i 0] [acc '()])
                     (if (>= i n)
                         acc
                         (loop (+ i 1) (cons (cons i (vector-ref scores i)) acc))))])
        ;; Sort by score descending and take top k
        (let ([sorted (sort (lambda (a b) (> (cdr a) (cdr b))) pairs)])
             (if (<= k (length sorted))
                 (take sorted k)
                 sorted))))

;;; take : (List a) × Nat → (List a)
;;; Take first n elements from list.
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;;; sort : (a × a → Bool) × (List a) → (List a)
;;; Simple insertion sort for small lists.
(define (sort pred lst)
  (if (null? lst)
      '()
      (insert pred (car lst) (sort pred (cdr lst)))))

(define (insert pred x sorted)
  (cond
   [(null? sorted) (list x)]
   [(pred x (car sorted)) (cons x sorted)]
   [else (cons (car sorted) (insert pred x (cdr sorted)))]))
