;;; lattice/data/graph/graph-matching.ss — Maximum Matching (Edmonds' Blossom)
;;; @module graph-matching
;;; @requires prelude graph-matrix
;;; @description Edmonds' Blossom algorithm for maximum matching in general graphs
;;; @purity total
;;; @stability stable
;;;
;;; Edmonds' Blossom algorithm for maximum cardinality matching in
;;; general (non-bipartite) graphs. O(V^3) time complexity.
;;;
;;; Handles odd cycles (blossoms) that defeat simple augmenting-path
;;; methods. For bipartite graphs, bipartite-max-matching in max-flow
;;; has better constant factors.
;;;
;;; Matching result: (list size pairs) where pairs is ((u v) ...)
;;; with u < v, matching bipartite-max-matching's return format.

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'graph-matrix)

(doc 'module 'graph-matching)
(doc 'description "Maximum cardinality matching in general graphs via Edmonds' Blossom algorithm.
O(V^3) time. Handles odd cycles (blossoms) that defeat augmenting-path methods on non-bipartite graphs.
Input: undirected adjacency matrix or edge list. Returns (list size matched-pairs).")
(doc 'layer 'lattice)
(doc 'purity 'observably-pure)

;;; ====
;;; Internal: Blossom LCA
;;; ====
;;;
;;; Find lowest common ancestor of v and w in the alternating tree.
;;; Walks from both vertices toward the root via base[] -> mate[] -> parent[]
;;; chain. The first base visited by both walks is the LCA.
;;;
;;; Safety invariant: walk-w does not guard against mate[bx] = -1
;;; because walk-v always marks the root's base as visited first.
;;; Since v and w share the same tree root, walk-w is guaranteed to
;;; hit a visited base before reaching any unmatched vertex.

(define (blossom-find-lca v w base mate parent n)
  (let ([visited (make-vector n #f)])
    ;; Walk from v toward root, marking bases as visited
    ;; (terminates at root where mate = -1)
    (let walk-v ([x v])
      (let ([bx (vector-ref base x)])
        (vector-set! visited bx #t)
        (when (not (= (vector-ref mate bx) -1))
          (walk-v (vector-ref parent (vector-ref mate bx))))))
    ;; Walk from w toward root, find first visited base
    ;; (safe: walk-v marked root, so we always find a hit)
    (let walk-w ([x w])
      (let ([bx (vector-ref base x)])
        (if (vector-ref visited bx)
            bx
            (walk-w (vector-ref parent (vector-ref mate bx))))))))

;;; ====
;;; Internal: Blossom Contraction
;;; ====
;;;
;;; When even vertices v and w are adjacent but in different blossoms,
;;; the cycle through v -> ... -> lca -> ... -> w -> v forms a blossom.
;;; Contraction sets base[] = lca for all vertices in the blossom,
;;; relabels odd vertices as even (they can now extend the search),
;;; and updates parent pointers for augmenting path reconstruction.

(define (blossom-contract! v w b label parent base mate)
  (let ([new-evens '()])
    (define (mark-path! u child)
      (let loop ([u u] [child child])
        (when (not (= (vector-ref base u) b))
          (let ([mu (vector-ref mate u)])
            (vector-set! base u b)
            (vector-set! base mu b)
            (vector-set! parent u child)
            (when (= (vector-ref label mu) 1) ; odd -> even
              (vector-set! label mu 0)
              (set! new-evens (cons mu new-evens)))
            (loop (vector-ref parent mu) mu)))))
    (mark-path! v w)
    (mark-path! w v)
    new-evens))

;;; ====
;;; Internal: Augmenting Path
;;; ====
;;;
;;; When unmatched vertex w is found adjacent to even vertex v,
;;; trace back through parent/mate pointers flipping matched/unmatched
;;; edges. Each flip step: match (v, w), then follow old mate of v
;;; back through the tree.

(define (blossom-augment! v w mate parent)
  (let loop ([v v] [w w])
    (let ([sv (vector-ref mate v)])
      (vector-set! mate v w)
      (vector-set! mate w v)
      (when (not (= sv -1))
        (loop (vector-ref parent sv) sv)))))

;;; ====
;;; Internal: BFS with Blossom Detection
;;; ====
;;;
;;; From an unmatched root, grow an alternating tree via BFS.
;;; Even vertices examine neighbors:
;;;   - Unlabeled + unmatched  -> augmenting path found
;;;   - Unlabeled + matched    -> extend tree (odd/even pair)
;;;   - Even + different base  -> blossom, contract
;;;   - Odd or same base       -> skip
;;; Returns #t if augmenting path found (mate[] updated), #f otherwise.

(define (blossom-find-augmenting-path! root adj n mate)
  (let ([label (make-vector n -1)]
        [parent (make-vector n -1)]
        [base (make-vector n 0)])
    ;; Initialize base[i] = i
    (let init ([i 0])
      (when (< i n)
        (vector-set! base i i)
        (init (+ i 1))))
    ;; Root is even
    (vector-set! label root 0)
    ;; BFS
    (let bfs ([queue (list root)])
      (and (pair? queue)
           (let ([v (car queue)]
                 [rest-q (cdr queue)])
             (let process ([neighbors (adjacency-neighbors adj v)]
                           [q rest-q])
               (if (null? neighbors)
                   (bfs q)
                   (let ([w (car neighbors)])
                     (cond
                       ;; Same blossom or w is odd — skip
                       [(or (= (vector-ref base v) (vector-ref base w))
                            (= (vector-ref label w) 1))
                        (process (cdr neighbors) q)]

                       ;; w is unlabeled
                       [(= (vector-ref label w) -1)
                        (if (= (vector-ref mate w) -1)
                            ;; Augmenting path found
                            (begin (blossom-augment! v w mate parent) #t)
                            ;; Extend alternating tree
                            (let ([x (vector-ref mate w)])
                              (vector-set! label w 1)  ; odd
                              (vector-set! parent w v)
                              (vector-set! label x 0)  ; even
                              (vector-set! parent x w)
                              (process (cdr neighbors) (append q (list x)))))]

                       ;; w is even with different base — blossom
                       [else
                        (let* ([b (blossom-find-lca v w base mate parent n)]
                               [new (blossom-contract! v w b label parent base mate)])
                          (process (cdr neighbors) (append q new)))])))))))))

;;; ====
;;; Internal: Core Algorithm
;;; ====

(define (max-matching-compute adj n)
  (let ([mate (make-vector n -1)])
    ;; Try to augment from each unmatched vertex
    (let outer ([v 0])
      (when (< v n)
        (when (= (vector-ref mate v) -1)
          (blossom-find-augmenting-path! v adj n mate))
        (outer (+ v 1))))
    ;; Collect matched pairs (u < v to avoid duplicates)
    (let collect ([i 0] [size 0] [pairs '()])
      (if (= i n)
          (list size (reverse pairs))
          (let ([m (vector-ref mate i)])
            (if (and (>= m 0) (< i m))
                (collect (+ i 1) (+ size 1) (cons (list i m) pairs))
                (collect (+ i 1) size pairs)))))))

;;; ====
;;; Public API
;;; ====

(doc max-matching 'type '(-> (U Matrix SparseCSR (List Edge)) [Nat] (List Nat (List (List Nat Nat)))))
(doc max-matching 'description "Maximum cardinality matching in an undirected graph.
Accepts adjacency matrix, sparse CSR, or edge list ((from to) ...).
Returns (list matching-size matched-pairs) where pairs is ((u v) ...) with u < v.
Uses Edmonds' Blossom algorithm. O(V^3) time.
Graph must be undirected (symmetric adjacency or unordered edge pairs).")
(doc max-matching 'param 'edges-or-adj "Adjacency matrix, sparse CSR, or list of (from to) edges")
(doc max-matching 'param 'n "Optional: number of nodes (inferred from edges if omitted; ignored for matrix input)")
(define (max-matching edges-or-adj . opts)
  (doc 'export #t)
  (cond
    ;; Adjacency matrix or sparse CSR
    [(and (pair? edges-or-adj)
          (memq (car edges-or-adj) '(matrix sparse-csr)))
     (max-matching-compute edges-or-adj (matrix-rows edges-or-adj))]
    ;; Empty edge list
    [(null? edges-or-adj)
     (list 0 '())]
    ;; Edge list
    [else
     (let ([n (if (null? opts)
                  (infer-node-count edges-or-adj)
                  (car opts))])
       (if (= n 0)
           (list 0 '())
           (max-matching-compute
            (edges->adjacency-matrix edges-or-adj n #t)
            n)))]))

(doc perfect-matching? 'type '(-> Nat (List Nat (List (List Nat Nat))) Boolean))
(doc perfect-matching? 'description "Check if a matching result is perfect (covers all n vertices).
A perfect matching requires n to be even and matching size = n/2.")
(define (perfect-matching? n result)
  (doc 'export #t)
  (and (even? n)
       (= (car result) (/ n 2))))

(doc matching-size 'type '(-> (List Nat (List (List Nat Nat))) Nat))
(doc matching-size 'description "Extract matching size from a max-matching result.")
(define (matching-size result)
  (doc 'export #t)
  (car result))
