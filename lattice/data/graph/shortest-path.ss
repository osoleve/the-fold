;;; lattice/data/graph/shortest-path.ss — Shortest Path Algorithms
;;; @module shortest-path
;;; @requires prelude graph-matrix iteration

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'graph-matrix)
(require 'iteration)

(doc 'module 'shortest-path)
(doc 'description "Unified shortest path algorithms on adjacency matrices.

Floyd-Warshall and Dijkstra are re-exported from graph-matrix.
Bellman-Ford is defined here for graphs with negative edge weights.

All algorithms operate on the (list 'matrix rows cols data-vector) representation.
Convention: 0 = no edge (except diagonal), positive/negative = weight.
Unreachable distances are *infinity* (1e308).")
(doc 'layer 'lattice)
(doc 'purity 'observably-pure)

(doc 'note "Algorithm selection guide:
  - All-pairs, any weights:     floyd-warshall    O(V^3)
  - Single-source, non-negative: dijkstra         O((V+E) log V)
  - Single-source, negative ok:  bellman-ford     O(V*E)
  - Single-source, unweighted:   BFS (use distance-matrix-unweighted)")

;;; ====
;;; Bellman-Ford
;;; ====

(doc bellman-ford 'type '(-> (Union Matrix SparseCSR) Nat (List Symbol (Pair Vector Vector))))
(doc bellman-ford 'description "Single-source shortest paths with negative edge weights.
Returns (ok . (dist . pred)) on success, or (negative-cycle . vertex) if a
negative-weight cycle is reachable from the source.")
(doc bellman-ford 'param 'adj "Weighted adjacency matrix (dense or sparse). 0 = no edge.")
(doc bellman-ford 'param 'source "Source node index")
(doc bellman-ford 'returns "(ok dist . pred) or (negative-cycle . v) where v is on the cycle")
(doc bellman-ford 'note "Time: O(V*E). Space: O(V). Handles negative edges but detects negative cycles.")
(define (bellman-ford adj source)
  (let* ([n (adjacency-matrix-node-count adj)]
         [dist (vec-tabulate n i (if (= i source) 0 *infinity*))]
         [pred (make-vector n -1)])
    ;; Collect edges once for efficiency
    (let ([edges (bellman-ford-collect-edges adj n)])
      ;; Relax all edges (n-1) times
      (do ([iter 0 (+ iter 1)])
          ((= iter (- n 1)))
        (for-each
         (lambda (edge)
           (let ([u (car edge)]
                 [v (cadr edge)]
                 [w (caddr edge)]
                 [du (vector-ref dist (car edge))])
             (when (< du *infinity*)
               (let ([alt (+ du w)])
                 (when (< alt (vector-ref dist v))
                   (vector-set! dist v alt)
                   (vector-set! pred v u))))))
         edges))
      ;; Check for negative cycles: one more pass
      (let ([neg-vertex
             (let check ([es edges])
               (if (null? es)
                   #f
                   (let* ([edge (car es)]
                          [u (car edge)]
                          [v (cadr edge)]
                          [w (caddr edge)]
                          [du (vector-ref dist u)])
                     (if (and (< du *infinity*)
                              (< (+ du w) (vector-ref dist v)))
                         v
                         (check (cdr es))))))])
        (if neg-vertex
            (cons 'negative-cycle neg-vertex)
            (cons 'ok (cons dist pred)))))))

;;; bellman-ford-collect-edges : (Union Matrix SparseCSR) x Nat -> (List (List Nat Nat Num))
;;; Extract all edges as (u v weight) triples. Skips zero entries (no edge).
(define (bellman-ford-collect-edges adj n)
  (let loop-i ([i 0] [edges '()])
    (if (= i n)
        edges
        (let ([neighbors (adjacency-neighbors-with-weights adj i)])
          (loop-i (+ i 1)
                  (fold-left
                   (lambda (acc pair)
                     (cons (list i (car pair) (cdr pair)) acc))
                   edges
                   neighbors))))))

;;; ====
;;; Path Reconstruction
;;; ====

(doc shortest-path-reconstruct 'type '(-> Vector Nat Nat (Union (List Nat) Boolean)))
(doc shortest-path-reconstruct 'description "Reconstruct shortest path from predecessor vector.
Returns list of node indices from source to target, or #f if unreachable.")
(doc shortest-path-reconstruct 'param 'pred "Predecessor vector (from dijkstra or bellman-ford)")
(doc shortest-path-reconstruct 'param 'source "Source node")
(doc shortest-path-reconstruct 'param 'target "Target node")
(define (shortest-path-reconstruct pred source target)
  (if (and (not (= source target))
           (= (vector-ref pred target) -1))
      #f  ; Unreachable
      (let loop ([node target] [path '()])
        (if (= node source)
            (cons source path)
            (let ([p (vector-ref pred node)])
              (if (= p -1)
                  #f  ; Should not happen if dist < infinity
                  (loop p (cons node path))))))))

;;; ====
;;; Bellman-Ford Path / Distance Convenience
;;; ====

(doc bellman-ford-distance 'type '(-> (Union Matrix SparseCSR) Nat Nat (Union Num Symbol)))
(doc bellman-ford-distance 'description "Shortest distance from source to target, handling negative edges.
Returns numeric distance, *infinity* if unreachable, or 'negative-cycle.")
(define (bellman-ford-distance adj source target)
  (let ([result (bellman-ford adj source)])
    (if (eq? (car result) 'negative-cycle)
        'negative-cycle
        (vector-ref (cadr result) target))))

(doc bellman-ford-path 'type '(-> (Union Matrix SparseCSR) Nat Nat (Union (List Nat) Boolean Symbol)))
(doc bellman-ford-path 'description "Shortest path from source to target, handling negative edges.
Returns path as node list, #f if unreachable, or 'negative-cycle.")
(define (bellman-ford-path adj source target)
  (let ([result (bellman-ford adj source)])
    (if (eq? (car result) 'negative-cycle)
        'negative-cycle
        (let ([dist (cadr result)]
              [pred (cddr result)])
          (if (>= (vector-ref dist target) *infinity*)
              #f
              (shortest-path-reconstruct pred source target))))))
