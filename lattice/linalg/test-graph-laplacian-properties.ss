;;; lattice/linalg/test-graph-laplacian-properties.ss — QuickCheck properties for graph Laplacians

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix)
(require 'graph-matrix)
(require 'graph-laplacian)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (path-edges n)
  (let loop ([i 0] [acc '()])
    (if (>= i (- n 1))
        (reverse acc)
        (loop (+ i 1) (cons (list i (+ i 1)) acc)))))

(define (matrix-row-sum m i)
  (let ([cols (matrix-cols m)])
    (let loop ([j 0] [sum 0])
      (if (= j cols)
          sum
          (loop (+ j 1) (+ sum (matrix-ref m i j)))))))

(define (all-rows-zero-sum? m)
  (let ([rows (matrix-rows m)])
    (let loop ([i 0])
      (or (= i rows)
          (and (= (matrix-row-sum m i) 0)
               (loop (+ i 1)))))))

(define (all-in-range? xs lo hi)
  (or (null? xs)
      (and (>= (car xs) lo)
           (<= (car xs) hi)
           (all-in-range? (cdr xs) lo hi))))

(define (nondecreasing? xs)
  (or (null? xs)
      (null? (cdr xs))
      (and (<= (car xs) (cadr xs))
           (nondecreasing? (cdr xs)))))

(define (valid-partition? parts n)
  (let ([seen (make-vector n 0)])
    (let loop-p ([ps parts] [count 0] [ok #t])
      (if (or (not ok) (null? ps))
          (and ok
               (= count n)
               (let loop-i ([i 0])
                 (or (= i n)
                     (and (= (vector-ref seen i) 1)
                          (loop-i (+ i 1))))))
          (let loop-node ([nodes (car ps)] [count* count] [ok* ok])
            (if (or (not ok*) (null? nodes))
                (loop-p (cdr ps) count* ok*)
                (let ([v (car nodes)])
                  (if (or (< v 0) (>= v n) (= (vector-ref seen v) 1))
                      (loop-p '() count* #f)
                      (begin
                        (vector-set! seen v 1)
                        (loop-node (cdr nodes) (+ count* 1) #t))))))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group graph-laplacian-properties

  (define-property "laplacian-from-edges matches laplacian(adjacency)"
    (gen-int-range 2 12)
    (lambda (n)
      (let* ([edges (path-edges n)]
             [adj (edges->adjacency-matrix edges n #t)]
             [l1 (laplacian adj)]
             [l2 (laplacian-from-edges edges n)])
        (matrix-equal? l1 l2)))
    'tests 180)

  (define-property "Laplacian rows sum to zero for undirected path graphs"
    (gen-int-range 2 12)
    (lambda (n)
      (let* ([edges (path-edges n)]
             [l (laplacian-from-edges edges n)])
        (all-rows-zero-sum? l)))
    'tests 180)

  (define-property "normalized Laplacian has unit diagonal on non-isolated path nodes"
    (gen-int-range 2 12)
    (lambda (n)
      (let* ([edges (path-edges n)]
             [adj (edges->adjacency-matrix edges n #t)]
             [l (laplacian-normalized adj)])
        (let loop ([i 0])
          (or (= i n)
              (and (approx= (matrix-ref l i i) 1.0 1e-10)
                   (loop (+ i 1)))))))
    'tests 150)

  (define-property "random-walk Laplacian rows also sum to zero on path graphs"
    (gen-int-range 2 12)
    (lambda (n)
      (let* ([adj (edges->adjacency-matrix (path-edges n) n #t)]
             [l-rw (laplacian-random-walk adj)])
        (all-rows-zero-sum? l-rw)))
    'tests 150)

  (define-property "algebraic connectivity is positive for connected path graphs"
    (gen-int-range 2 10)
    (lambda (n)
      (> (algebraic-connectivity (edges->adjacency-matrix (path-edges n) n #t))
         0.0))
    'tests 100)

  (define-property "laplacian-connected-components is 1 for path graphs"
    (gen-int-range 2 10)
    (lambda (n)
      (= (laplacian-connected-components
          (edges->adjacency-matrix (path-edges n) n #t))
         1))
    'tests 100)

  (define-property "spectral-partition yields a disjoint full cover"
    (gen-int-range 2 10)
    (lambda (n)
      (let* ([adj (edges->adjacency-matrix (path-edges n) n #t)]
             [parts (spectral-partition adj)])
        (and (pair? parts)
             (valid-partition? (list (car parts) (cdr parts)) n))))
    'tests 60)

  (define-property "spectral-partition-k returns k partitions covering all nodes"
    (gen-bind (gen-int-range 2 10)
      (lambda (n)
        (gen-map (lambda (k) (list n k))
                 (gen-int-range 2 (min 5 n)))))
    (lambda (args)
      (let* ([n (car args)]
             [k (cadr args)]
             [adj (edges->adjacency-matrix (path-edges n) n #t)]
             [parts (spectral-partition-k adj k)])
        (and (>= (length parts) 1)
             (<= (length parts) k)
             (valid-partition? parts n))))
    'tests 40)

  (define-property "laplacian trace equals 2*(n-1) on undirected path graph"
    (gen-int-range 2 14)
    (lambda (n)
      (let* ([adj (edges->adjacency-matrix (path-edges n) n #t)]
             [l (laplacian adj)])
        (= (laplacian-trace l) (* 2 (- n 1)))))
    'tests 120)

  (define-property "spectral-gap and algebraic-connectivity agree"
    (gen-int-range 2 10)
    (lambda (n)
      (let ([adj (edges->adjacency-matrix (path-edges n) n #t)])
        (approx= (spectral-gap adj)
                 (algebraic-connectivity adj)
                 1e-8)))
    'tests 100)

  (define-property "laplacian-max-eigenvalue dominates algebraic connectivity"
    (gen-int-range 2 10)
    (lambda (n)
      (let* ([adj (edges->adjacency-matrix (path-edges n) n #t)]
             [lam2 (algebraic-connectivity adj)]
             [lam-max (laplacian-max-eigenvalue adj)])
        (>= lam-max lam2)))
    'tests 80)

  (define-property "path graphs are bipartite by spectral criterion"
    (gen-int-range 2 10)
    (lambda (n)
      (is-bipartite-spectral? (edges->adjacency-matrix (path-edges n) n #t)))
    'tests 80)

  (define-property "effective resistance is symmetric and zero on diagonal"
    (gen-bind (gen-int-range 2 9)
      (lambda (n)
        (gen-bind (gen-int-range 0 (- n 1))
          (lambda (i)
            (gen-map (lambda (j) (list n i j))
                     (gen-int-range 0 (- n 1)))))))
    (lambda (args)
      (let* ([n (car args)]
             [i (cadr args)]
             [j (caddr args)]
             [adj (edges->adjacency-matrix (path-edges n) n #t)]
             [rij (effective-resistance adj i j)]
             [rji (effective-resistance adj j i)])
        (and (approx= rij rji 1e-8)
             (approx= (effective-resistance adj i i) 0.0 1e-10))))
    'tests 40)

  (define-property "total-effective-resistance is positive on connected graphs"
    (gen-int-range 2 9)
    (lambda (n)
      (> (total-effective-resistance
          (edges->adjacency-matrix (path-edges n) n #t))
         0.0))
    'tests 40)

  (define-property "laplacian-summary includes key metrics"
    (gen-int-range 2 9)
    (lambda (n)
      (let* ([adj (edges->adjacency-matrix (path-edges n) n #t)]
             [s (laplacian-summary adj)])
        (and (assq 'nodes s)
             (assq 'smallest-eigenvalue s)
             (assq 'algebraic-connectivity s)
             (assq 'largest-eigenvalue s)
             (assq 'connected-components s)
             (assq 'trace s)
             (assq 'energy s)
             (= (cdr (assq 'nodes s)) n))))
    'tests 60)

  (define-property "spectral-clustering outputs one label per node in range"
    (gen-bind (gen-int-range 3 12)
      (lambda (n)
        (gen-map (lambda (k) (list n k))
                 (gen-int-range 2 (min 4 n)))))
    (lambda (args)
      (let* ([n (car args)]
             [k (cadr args)]
             [adj (edges->adjacency-matrix (path-edges n) n #t)]
             [labels (spectral-clustering adj k)]
             [label-list (vector->list labels)])
        (and (= (vector-length labels) n)
             (all-in-range? label-list 0 (- k 1)))))
    'tests 30)

  (define-property "spectral-clustering-unnorm outputs one label per node in range"
    (gen-bind (gen-int-range 3 12)
      (lambda (n)
        (gen-map (lambda (k) (list n k))
                 (gen-int-range 2 (min 4 n)))))
    (lambda (args)
      (let* ([n (car args)]
             [k (cadr args)]
             [adj (edges->adjacency-matrix (path-edges n) n #t)]
             [labels (spectral-clustering-unnorm adj k)]
             [label-list (vector->list labels)])
        (and (= (vector-length labels) n)
             (all-in-range? label-list 0 (- k 1)))))
    'tests 30)

  (define-property "k-smallest-indices returns sorted-by-value valid indices"
    (gen-bind (gen-int-range 1 10)
      (lambda (n)
        (gen-bind (gen-list-of n (gen-int-range -30 30))
          (lambda (vals)
            (gen-map (lambda (k) (list vals k))
                     (gen-int-range 1 n))))))
    (lambda (args)
      (let* ([vals (car args)]
             [k (cadr args)]
             [v (list->vector vals)]
             [idxs (k-smallest-indices v k)]
             [picked (map (lambda (i) (abs (vector-ref v i))) idxs)])
        (and (= (length idxs) k)
             (all-in-range? idxs 0 (- (vector-length v) 1))
             (nondecreasing? picked))))
    'tests 120)

  (define-property "spectral-embedding returns normalized n-by-k matrix"
    (gen-int-range 2 12)
    (lambda (n)
      (let* ([eigs (matrix-from-lists
                    (let loop ([i 0] [rows '()])
                      (if (= i n)
                          (reverse rows)
                          (loop (+ i 1)
                                (cons (list (+ i 1.0) (+ i 2.0) (+ i 3.0))
                                      rows)))))]
             [emb (spectral-embedding eigs '(0 1) n)])
        (and (= (matrix-rows emb) n)
             (= (matrix-cols emb) 2)
             (let loop ([i 0])
               (or (= i n)
                   (let* ([x (matrix-ref emb i 0)]
                          [y (matrix-ref emb i 1)]
                          [r (sqrt (+ (* x x) (* y y)))])
                     (and (approx= r 1.0 1e-9)
                          (loop (+ i 1)))))))))
    'tests 80)

  (define-property "kmeans-init-pp returns k centroids with matching dimension"
    (gen-bind (gen-int-range 2 12)
      (lambda (n)
        (gen-map (lambda (k) (list n k))
                 (gen-int-range 1 (min 5 n)))))
    (lambda (args)
      (let* ([n (car args)]
             [k (cadr args)]
             [points (matrix-from-lists
                      (let loop ([i 0] [rows '()])
                        (if (= i n)
                            (reverse rows)
                            (loop (+ i 1)
                                  (cons (list i (modulo (* i 3) 7)) rows)))))]
             [centroids (kmeans-init-pp points k)])
        (and (= (matrix-rows centroids) k)
             (= (matrix-cols centroids) 2))))
    'tests 60)

  (define-property "kmeans-cluster returns one label per point"
    (gen-bind (gen-int-range 2 12)
      (lambda (n)
        (gen-map (lambda (k) (list n k))
                 (gen-int-range 1 (min 4 n)))))
    (lambda (args)
      (let* ([n (car args)]
             [k (cadr args)]
             [points (matrix-from-lists
                      (let loop ([i 0] [rows '()])
                        (if (= i n)
                            (reverse rows)
                            (loop (+ i 1)
                                  (cons (list i (modulo (* i 5) 11)) rows)))))]
             [labels (kmeans-cluster points k)])
        (and (= (vector-length labels) n)
             (all-in-range? (vector->list labels) 0 (- k 1)))))
    'tests 60)

  (define-property "conductance, ratio-cut, and normalized-cut are nonnegative"
    (gen-int-range 3 12)
    (lambda (n)
      (let* ([adj (edges->adjacency-matrix (path-edges n) n #t)]
             [nodes (let loop ([i 0] [acc '()])
                      (if (>= i (quotient n 2))
                          (reverse acc)
                          (loop (+ i 1) (cons i acc))))]
             [c (conductance adj nodes)]
             [r (ratio-cut adj nodes)]
             [nc (normalized-cut adj nodes)])
        (and (>= c 0.0)
             (>= r 0.0)
             (>= nc 0.0))))
    'tests 80)

  (define-property "graph-laplacian helper surface is well-formed"
    (gen-int-range 4 12)
    (lambda (n)
      (let* ([adj (edges->adjacency-matrix (path-edges n) n #t)]
             [fv (fiedler-vector adj)]
             [nodes '(0 1 2)]
             [sub (extract-induced-subgraph adj nodes)]
             [parts (subgraph-partition adj nodes 2)]
             [remapped (remap-partition '(0 1) nodes)]
             [energy (laplacian-energy adj)]
             [radius (spectral-radius-laplacian adj)])
        (and (= (vector-length fv) n)
             (= (matrix-rows sub) (length nodes))
             (= (matrix-cols sub) (length nodes))
             (= (length parts) 2)
             (= (length remapped) 2)
             (all-in-range? remapped 0 (- n 1))
             (> energy 0.0)
             (>= radius 0.0))))
    'tests 80)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
