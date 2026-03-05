;;; lattice/data/graph/spectral-community.ss — Spectral Community Detection Bridge
;;; @module spectral-community
;;; @requires prelude graph-matrix graph-laplacian graph-community iteration
;;; @description Spectral bipartition and community detection
;;; @purity total
;;; @stability stable

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'graph-matrix)
(require 'graph-laplacian)
(require 'graph-community)
(require 'iteration)

(doc 'module 'spectral-community)
(doc 'bridges '(data linalg))
(doc 'description "Bridge between spectral graph theory (graph-laplacian) and community detection (graph-community)")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'note "Connects the Laplacian spectral methods with the community detection API:
  - spectral-bipartition: Fiedler-vector bipartition returning a label vector
  - spectral-communities: recursive spectral bisection for k communities
  - spectral-modularity: modularity of a spectral partition
  - partition-conductance: conductance of a community label assignment
  - compare-partitions: compare spectral vs label-propagation results")

(doc 'note "Handles disconnected graphs correctly: the Fiedler vector only works
  on connected graphs, so spectral-bipartition first checks connectivity.
  If the graph is disconnected, it uses the connected components directly.")

;;; ====
;;; Spectral Bipartition (Label Vector Form)
;;; ====

(doc spectral-bipartition 'type '(-> Matrix Vector))
(doc spectral-bipartition 'description "Bipartition a graph using the Fiedler vector, returning a label vector compatible with graph-community")
(doc spectral-bipartition 'param 'adj "Adjacency matrix (undirected)")
(doc spectral-bipartition 'returns "Vector of community labels (0 or 1 for each node), or error pair on failure")
(doc spectral-bipartition 'note "For disconnected graphs, assigns labels based on connected components (first component -> 0, rest -> 1).
  For connected graphs, uses sign of Fiedler vector components: non-negative -> 0, negative -> 1.")
(define (spectral-bipartition adj)
  (doc 'export #t)
  (let* ([n (adjacency-matrix-node-count adj)]
         [comp-labels (connected-components adj)]
         [num-comp (num-communities comp-labels)])
    (if (> num-comp 1)
        ;; Disconnected: first component gets label 0, rest get label 1
        (let ([first-comp (vector-ref comp-labels 0)])
          (vec-tabulate n i (if (= (vector-ref comp-labels i) first-comp) 0 1)))
        ;; Connected: use Fiedler vector
        (let ([result (spectral-partition adj)])
          (if (and (pair? result) (eq? (car result) 'error))
              result
              (let* ([part-a (car result)]
                     [part-b (cdr result)]
                     [labels (make-vector n 0)])
                ;; part-a nodes get label 0 (already default)
                ;; part-b nodes get label 1
                (for-each (lambda (i) (vector-set! labels i 1)) part-b)
                labels))))))

;;; ====
;;; Spectral Communities (k partitions as label vector)
;;; ====

(doc spectral-communities 'type '(-> Matrix Nat Vector))
(doc spectral-communities 'description "Partition graph into k communities using recursive spectral bisection, returning a label vector")
(doc spectral-communities 'param 'adj "Adjacency matrix (undirected)")
(doc spectral-communities 'param 'k "Number of communities to find")
(doc spectral-communities 'returns "Vector of community labels (0 to k-1), or error pair on failure")
(doc spectral-communities 'note "Handles disconnected graphs: first separates connected components,
  then recursively bisects within components if more partitions are needed.
  Uses spectral-partition-k for the connected-graph case.")
;;; partitions->labels : (List (List Nat)) x Nat -> Vector
;;; Convert a list of node-index lists into a flat label vector.
(define (partitions->labels partitions n)
  (let ([labels (make-vector n 0)])
    (let loop ([parts partitions] [id 0])
      (if (null? parts)
          labels
          (begin
            (for-each
              (lambda (node) (vector-set! labels node id))
              (car parts))
            (loop (cdr parts) (+ id 1)))))))

;;; split-largest-community : Matrix x Vector x Nat x Nat -> Vector
;;; Subdivide the largest community in labels by spectral bisection.
;;; Repeats until we have the target number of communities or can't split further.
(define (split-largest-community adj labels remaining next-id)
  (if (<= remaining 0)
      labels
      (let* ([partition (communities->partition labels)]
             [best (find-largest-partition partition)])
        (if (<= (length best) 1)
            labels  ; can't split a single node
            (let* ([sub-adj (extract-induced-subgraph adj best)]
                   [sub-result (spectral-partition sub-adj)])
              (if (and (pair? sub-result) (eq? (car sub-result) 'error))
                  labels  ; spectral partition failed, return what we have
                  (let ([node-vec (list->vector best)]
                        [sub-b (cdr sub-result)])
                    ;; Relabel the sub-partition-b nodes with the next label
                    (for-each
                      (lambda (local-idx)
                        (vector-set! labels (vector-ref node-vec local-idx) next-id))
                      sub-b)
                    (split-largest-community adj labels (- remaining 1) (+ next-id 1)))))))))

;;; find-largest-partition : (List (List Nat)) -> (List Nat)
;;; Return the largest partition (by number of nodes).
(define (find-largest-partition partitions)
  (let loop ([parts (cdr partitions)]
             [best (car partitions)]
             [best-size (length (car partitions))])
    (if (null? parts)
        best
        (let ([sz (length (car parts))])
          (if (> sz best-size)
              (loop (cdr parts) (car parts) sz)
              (loop (cdr parts) best best-size))))))

(define (spectral-communities adj k)
  (doc 'export #t)
  (let* ([n (adjacency-matrix-node-count adj)]
         [comp-labels (connected-components adj)]
         [num-comp (num-communities comp-labels)])
    (cond
      [(<= k 1)
       ;; Everything in one community
       (make-vector n 0)]
      [(>= num-comp k)
       ;; Already have enough (or more) components -- map component IDs to 0..k-1
       ;; Merge excess components into the last label
       (let* ([partition (communities->partition comp-labels)]
              [labels (make-vector n 0)])
         (let loop ([parts partition] [id 0])
           (if (null? parts)
               labels
               (begin
                 (for-each
                   (lambda (node) (vector-set! labels node (min id (- k 1))))
                   (car parts))
                 (loop (cdr parts) (+ id 1))))))]
      [(= num-comp 1)
       ;; Connected graph: use spectral-partition-k directly
       (let ([result (spectral-partition-k adj k)])
         (if (and (pair? result) (eq? (car result) 'error))
             result
             (partitions->labels result n)))]
      [else
       ;; Multiple components but fewer than k: assign component labels first,
       ;; then subdivide the largest components until we reach k
       (let* ([partition (communities->partition comp-labels)]
              [labels (partitions->labels partition n)])
         (split-largest-community adj labels (- k num-comp) num-comp))])))

;;; ====
;;; Spectral Modularity
;;; ====

(doc spectral-modularity 'type '(-> Matrix Nat Num))
(doc spectral-modularity 'description "Compute modularity of a spectral k-partition")
(doc spectral-modularity 'param 'adj "Adjacency matrix (undirected)")
(doc spectral-modularity 'param 'k "Number of communities")
(doc spectral-modularity 'returns "Modularity score Q in [-0.5, 1], or error pair on failure")
(doc spectral-modularity 'note "Combines spectral-communities with modularity from graph-community")
(define (spectral-modularity adj k)
  (doc 'export #t)
  (let ([labels (spectral-communities adj k)])
    (if (and (pair? labels) (eq? (car labels) 'error))
        labels
        (modularity adj labels))))

;;; ====
;;; Partition Conductance
;;; ====

(doc partition-conductance 'type '(-> Matrix Vector (List Num)))
(doc partition-conductance 'description "Compute conductance for each community in a label assignment")
(doc partition-conductance 'param 'adj "Adjacency matrix (undirected)")
(doc partition-conductance 'param 'labels "Community label vector")
(doc partition-conductance 'returns "List of conductance values, one per community (lower = better)")
(doc partition-conductance 'note "Uses graph-laplacian's conductance function for each community's node set")
(define (partition-conductance adj labels)
  (doc 'export #t)
  (let ([partition (communities->partition labels)])
    (map (lambda (nodes) (conductance adj nodes)) partition)))

;;; ====
;;; Partition Normalized Cut
;;; ====

(doc partition-normalized-cut 'type '(-> Matrix Vector (List Num)))
(doc partition-normalized-cut 'description "Compute normalized cut for each community in a label assignment")
(doc partition-normalized-cut 'param 'adj "Adjacency matrix (undirected)")
(doc partition-normalized-cut 'param 'labels "Community label vector")
(doc partition-normalized-cut 'returns "List of normalized cut values, one per community (lower = better)")
(define (partition-normalized-cut adj labels)
  (doc 'export #t)
  (let ([partition (communities->partition labels)])
    (map (lambda (nodes) (normalized-cut adj nodes)) partition)))

;;; ====
;;; Compare Spectral vs Label Propagation
;;; ====

(doc compare-partitions 'type '(-> Matrix Nat (List (Pair Symbol Any))))
(doc compare-partitions 'description "Compare spectral partitioning against label propagation for a graph")
(doc compare-partitions 'param 'adj "Adjacency matrix (undirected)")
(doc compare-partitions 'param 'k "Number of communities for spectral method")
(doc compare-partitions 'returns "Association list with modularity and community counts for both methods, or error pair")
(define (compare-partitions adj k)
  (doc 'export #t)
  (let ([spectral-labels (spectral-communities adj k)])
    (if (and (pair? spectral-labels) (eq? (car spectral-labels) 'error))
        spectral-labels
        (let* ([lp-labels (label-propagation adj)]
               [spectral-Q (modularity adj spectral-labels)]
               [lp-Q (modularity adj lp-labels)]
               [spectral-k (num-communities spectral-labels)]
               [lp-k (num-communities lp-labels)]
               [spectral-cond (partition-conductance adj spectral-labels)]
               [lp-cond (partition-conductance adj lp-labels)])
          `((spectral-modularity . ,spectral-Q)
            (spectral-communities . ,spectral-k)
            (spectral-conductances . ,spectral-cond)
            (label-prop-modularity . ,lp-Q)
            (label-prop-communities . ,lp-k)
            (label-prop-conductances . ,lp-cond))))))

;;; ====
;;; Spectral Community Quality
;;; ====

(doc spectral-quality 'type '(-> Matrix Nat (List (Pair Symbol Any))))
(doc spectral-quality 'description "Compute comprehensive quality metrics for a spectral k-partition")
(doc spectral-quality 'param 'adj "Adjacency matrix (undirected)")
(doc spectral-quality 'param 'k "Number of communities")
(doc spectral-quality 'returns "Association list with modularity, conductances, algebraic connectivity, and partition sizes")
(define (spectral-quality adj k)
  (doc 'export #t)
  (let ([labels (spectral-communities adj k)])
    (if (and (pair? labels) (eq? (car labels) 'error))
        labels
        (let* ([Q (modularity adj labels)]
               [conds (partition-conductance adj labels)]
               [ncuts (partition-normalized-cut adj labels)]
               [partition (communities->partition labels)]
               [sizes (map length partition)]
               [ac (algebraic-connectivity adj)])
          `((modularity . ,Q)
            (num-communities . ,(length partition))
            (community-sizes . ,sizes)
            (conductances . ,conds)
            (normalized-cuts . ,ncuts)
            (algebraic-connectivity . ,(if (and (pair? ac) (eq? (car ac) 'error))
                                          0
                                          ac)))))))
