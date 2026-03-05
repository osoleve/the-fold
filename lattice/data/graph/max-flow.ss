;;; lattice/data/graph/max-flow.ss — Max-Flow / Min-Cut
;;; @module max-flow
;;; @requires prelude
;;;
;;; Edmonds-Karp (BFS-based Ford-Fulkerson) max-flow algorithm.
;;; O(VE²) time complexity.
;;;
;;; Flow networks are represented as adjacency-list hashtables
;;; with forward and backward (residual) edges. Each edge is
;;; a mutable vector #(to capacity flow rev-index) where rev-index
;;; points to the reverse edge in the neighbor's adjacency list.

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'max-flow)
(doc 'description "Edmonds-Karp max-flow and min-cut on integer-indexed flow networks.
Nodes are integers 0..n-1. Edges have non-negative integer capacities.
Supports: max-flow computation, min-cut extraction, bipartite matching.")
(doc 'layer 'lattice)
(doc 'purity 'observably-pure)

;;; ====
;;; Flow Network Representation
;;; ====
;;;
;;; A flow network is (list 'flow-network n adj) where:
;;;   n   = number of nodes
;;;   adj = vector of length n, each entry is a list of edge vectors
;;;   edge vector = #(to capacity flow rev-index)
;;;
;;; For each edge u->v with capacity c, we also store a reverse edge
;;; v->u with capacity 0 (residual edge). The rev-index fields point
;;; to each other so we can update residuals in O(1).

(doc make-flow-network 'type '(-> Nat FlowNetwork))
(doc make-flow-network 'description "Create an empty flow network with n nodes.")
(define (make-flow-network n)
  (doc 'export #t)
  (let ([adj (make-vector n '())])
    (list 'flow-network n adj)))

(doc flow-network? 'type '(-> Any Boolean))
(doc flow-network? 'description "Check if value is a flow network.")
(define (flow-network? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'flow-network)
       (pair? (cdr x))
       (pair? (cddr x))))

(doc flow-network-size 'type '(-> FlowNetwork Nat))
(doc flow-network-size 'description "Number of nodes in the flow network.")
(define (flow-network-size net)
  (doc 'export #t)
  (cadr net))

(doc flow-network-adj 'type '(-> FlowNetwork Vector))
(doc flow-network-adj 'description "Adjacency vector of the flow network.")
(define (flow-network-adj net)
  (doc 'export #t)
  (caddr net))

;;; Edge accessors
(define (edge-to e)       (vector-ref e 0))
(define (edge-cap e)      (vector-ref e 1))
(define (edge-flow e)     (vector-ref e 2))
(define (edge-rev-idx e)  (vector-ref e 3))

;;; Edge residual capacity
(define (edge-residual e)
  (- (edge-cap e) (edge-flow e)))

;;; Edge mutators
(define (edge-set-flow! e f)
  (vector-set! e 2 f))

(doc flow-network-add-edge! 'type '(-> FlowNetwork Nat Nat Nat Void))
(doc flow-network-add-edge! 'description "Add a directed edge from u to v with given capacity.
Creates both the forward edge and its residual reverse edge.")
(define (flow-network-add-edge! net u v cap)
  (doc 'export #t)
  (let* ([adj (flow-network-adj net)]
         [u-edges (vector-ref adj u)]
         [v-edges (vector-ref adj v)]
         [u-idx (length u-edges)]
         [v-idx (length v-edges)]
         [fwd (vector v cap 0 v-idx)]
         [rev (vector u 0  0 u-idx)])
    (vector-set! adj u (append u-edges (list fwd)))
    (vector-set! adj v (append v-edges (list rev)))))

(doc flow-network-edges 'type '(-> FlowNetwork (List (List Nat Nat Nat Nat))))
(doc flow-network-edges 'description "Return all forward edges as (from to capacity flow) lists.
Excludes residual-only edges (those with capacity 0).")
(define (flow-network-edges net)
  (doc 'export #t)
  (let ([adj (flow-network-adj net)]
        [n (flow-network-size net)])
    (let loop-u ([u 0] [result '()])
      (if (= u n)
          (reverse result)
          (let ([edges (vector-ref adj u)])
            (let loop-e ([es edges] [acc result])
              (if (null? es)
                  (loop-u (+ u 1) acc)
                  (let ([e (car es)])
                    (if (> (edge-cap e) 0)
                        (loop-e (cdr es)
                                (cons (list u (edge-to e) (edge-cap e) (edge-flow e))
                                      acc))
                        (loop-e (cdr es) acc))))))))))

(doc flow-network-flow-on-edge 'type '(-> FlowNetwork Nat Nat Nat))
(doc flow-network-flow-on-edge 'description "Query flow on edge u->v. Returns 0 if edge not found.")
(define (flow-network-flow-on-edge net u v)
  (doc 'export #t)
  (let ([edges (vector-ref (flow-network-adj net) u)])
    (let loop ([es edges])
      (cond
        [(null? es) 0]
        [(and (= (edge-to (car es)) v)
              (> (edge-cap (car es)) 0))
         (edge-flow (car es))]
        [else (loop (cdr es))]))))

;;; ====
;;; BFS for Augmenting Path (Edmonds-Karp)
;;; ====

;;; bfs-augmenting-path : Vector Nat Nat Nat -> (Union #f (List Nat))
;;; Find shortest augmenting path from source to sink in residual graph.
;;; Returns list of node indices or #f if no path exists.
;;; Also returns parent-edge info for path reconstruction.
(define (bfs-augmenting-path adj n source sink)
  ;; parent: vector of (u . edge-index) or #f
  (let ([parent (make-vector n #f)]
        [visited (make-vector n #f)])
    (vector-set! visited source #t)
    (let bfs ([queue (list source)])
      (cond
        [(null? queue) #f]  ; No path found
        [(vector-ref visited sink)
         ;; Reconstruct path as list of (node . edge-index) pairs
         (let reconstruct ([v sink] [path '()])
           (if (= v source)
               (cons (cons source -1) path)
               (let ([p (vector-ref parent v)])
                 (reconstruct (car p) (cons p path)))))]
        [else
         (let* ([u (car queue)]
                [rest-queue (cdr queue)]
                [edges (vector-ref adj u)])
           (let explore ([es edges] [idx 0] [new-queue rest-queue])
             (if (null? es)
                 (bfs new-queue)
                 (let ([e (car es)])
                   (if (and (> (edge-residual e) 0)
                            (not (vector-ref visited (edge-to e))))
                       (begin
                         (vector-set! visited (edge-to e) #t)
                         (vector-set! parent (edge-to e) (cons u idx))
                         (if (= (edge-to e) sink)
                             ;; Found sink — reconstruct immediately
                             (let reconstruct ([v sink] [path '()])
                               (if (= v source)
                                   (cons (cons source -1) path)
                                   (let ([p (vector-ref parent v)])
                                     (reconstruct (car p) (cons p path)))))
                             (explore (cdr es) (+ idx 1)
                                      (append new-queue (list (edge-to e))))))
                       (explore (cdr es) (+ idx 1) new-queue))))))]))))

;;; list-ref-edge : (List Edge) Nat -> Edge
;;; Get the idx-th edge from a list.
(define (list-ref-edge edges idx)
  (let loop ([es edges] [i 0])
    (if (= i idx)
        (car es)
        (loop (cdr es) (+ i 1)))))

;;; ====
;;; Edmonds-Karp Max-Flow
;;; ====

(doc max-flow 'type '(-> FlowNetwork Nat Nat Nat))
(doc max-flow 'description "Compute maximum flow from source to sink using Edmonds-Karp (BFS Ford-Fulkerson).
Returns the total flow value. Mutates the flow network's edge flow values in place.
Time: O(VE²). Space: O(V+E).")
(doc max-flow 'param 'net "Flow network created with make-flow-network + flow-network-add-edge!")
(doc max-flow 'param 'source "Source node index")
(doc max-flow 'param 'sink "Sink node index")
(define (max-flow net source sink)
  (doc 'export #t)
  (if (= source sink)
      0
      (let ([adj (flow-network-adj net)]
            [n (flow-network-size net)])
        (let augment ([total-flow 0])
          (let ([path (bfs-augmenting-path adj n source sink)])
            (if (not path)
                total-flow
            ;; Find bottleneck (minimum residual along path)
            (let ([bottleneck
                   (let bn-loop ([ps (cdr path)] [min-cap +inf.0])
                     (if (null? ps)
                         (inexact->exact (floor min-cap))
                         (let* ([p (car ps)]
                                [u (car p)]
                                [idx (cdr p)]
                                [e (list-ref-edge (vector-ref adj u) idx)]
                                [r (edge-residual e)])
                           (bn-loop (cdr ps) (if (< r min-cap) r min-cap)))))])
              ;; Push flow along path
              (let push-loop ([ps (cdr path)])
                (if (null? ps)
                    (augment (+ total-flow bottleneck))
                    (let* ([p (car ps)]
                           [u (car p)]
                           [idx (cdr p)]
                           [e (list-ref-edge (vector-ref adj u) idx)]
                           [v (edge-to e)]
                           [rev-e (list-ref-edge (vector-ref adj v) (edge-rev-idx e))])
                      (edge-set-flow! e (+ (edge-flow e) bottleneck))
                      (edge-set-flow! rev-e (- (edge-flow rev-e) bottleneck))
                      (push-loop (cdr ps))))))))))))

;;; ====
;;; Min-Cut
;;; ====

(doc min-cut 'type '(-> FlowNetwork Nat Nat (List Nat Nat (List (List Nat Nat)))))
(doc min-cut 'description "Compute min-cut after max-flow has been run.
Returns (list flow-value cut-capacity cut-edges) where:
  flow-value = total flow (= min-cut capacity by max-flow min-cut theorem)
  cut-capacity = sum of capacities of cut edges (same as flow-value)
  cut-edges = list of (from to) pairs crossing the cut (S-side to T-side).
IMPORTANT: Call max-flow on the network first; this reads the residual graph.")
(doc min-cut 'param 'net "Flow network (must have max-flow already computed)")
(doc min-cut 'param 'source "Source node index")
(doc min-cut 'param 'sink "Sink node index")
(define (min-cut net source sink)
  (doc 'export #t)
  (let* ([adj (flow-network-adj net)]
         [n (flow-network-size net)]
         ;; BFS from source in residual graph to find S-side
         [s-side (min-cut-reachable adj n source)])
    ;; Find edges crossing the cut
    (let ([s-set (make-vector n #f)])
      ;; Mark S-side nodes
      (let mark ([nodes s-side])
        (unless (null? nodes)
          (vector-set! s-set (car nodes) #t)
          (mark (cdr nodes))))
      ;; Find cut edges: forward edges from S to T with capacity > 0
      (let loop-u ([nodes s-side] [cut-edges '()] [cut-cap 0])
        (if (null? nodes)
            (list cut-cap cut-cap (reverse cut-edges))
            (let* ([u (car nodes)]
                   [edges (vector-ref adj u)])
              (let loop-e ([es edges] [ce cut-edges] [cc cut-cap])
                (if (null? es)
                    (loop-u (cdr nodes) ce cc)
                    (let ([e (car es)])
                      (if (and (> (edge-cap e) 0)
                               (not (vector-ref s-set (edge-to e))))
                          (loop-e (cdr es)
                                  (cons (list u (edge-to e)) ce)
                                  (+ cc (edge-cap e)))
                          (loop-e (cdr es) ce cc)))))))))))

(doc min-cut-reachable 'type '(-> Vector Nat Nat (List Nat)))
(doc min-cut-reachable 'description "BFS from source in residual graph. Returns list of reachable nodes (S-side of cut).")
(define (min-cut-reachable adj n source)
  (let ([visited (make-vector n #f)])
    (vector-set! visited source #t)
    (let bfs ([queue (list source)] [result (list source)])
      (if (null? queue)
          result
          (let* ([u (car queue)]
                 [edges (vector-ref adj u)])
            (let explore ([es edges] [q (cdr queue)] [r result])
              (if (null? es)
                  (bfs q r)
                  (let ([e (car es)])
                    (if (and (> (edge-residual e) 0)
                             (not (vector-ref visited (edge-to e))))
                        (begin
                          (vector-set! visited (edge-to e) #t)
                          (explore (cdr es)
                                   (append q (list (edge-to e)))
                                   (cons (edge-to e) r)))
                        (explore (cdr es) q r))))))))))

(doc min-cut-partition 'type '(-> FlowNetwork Nat Nat (List (List Nat) (List Nat))))
(doc min-cut-partition 'description "Return the (S-side T-side) partition after max-flow.
S-side = nodes reachable from source in residual graph.
T-side = all other nodes.")
(define (min-cut-partition net source sink)
  (doc 'export #t)
  (let* ([adj (flow-network-adj net)]
         [n (flow-network-size net)]
         [s-side (min-cut-reachable adj n source)]
         [s-set (make-vector n #f)])
    ;; Mark S-side
    (let mark ([nodes s-side])
      (unless (null? nodes)
        (vector-set! s-set (car nodes) #t)
        (mark (cdr nodes))))
    ;; Build T-side
    (let loop ([i 0] [t-side '()])
      (if (= i n)
          (list s-side (reverse t-side))
          (if (vector-ref s-set i)
              (loop (+ i 1) t-side)
              (loop (+ i 1) (cons i t-side)))))))

;;; ====
;;; Bipartite Matching via Max-Flow
;;; ====

(doc bipartite-max-matching 'type '(-> (List Nat) (List Nat) (List (List Nat Nat)) (List Nat (List (List Nat Nat)))))
(doc bipartite-max-matching 'description "Maximum bipartite matching via max-flow reduction.
Takes left nodes, right nodes, and edges between them.
Returns (list matching-size matched-pairs).
Nodes in left and right MUST be disjoint non-negative integers.")
(doc bipartite-max-matching 'param 'left "List of left-side node indices")
(doc bipartite-max-matching 'param 'right "List of right-side node indices")
(doc bipartite-max-matching 'param 'edges "List of (left-node right-node) edges")
(define (bipartite-max-matching left right edges)
  (doc 'export #t)
  (let* ([all-nodes (append left right)]
         [max-node (if (null? all-nodes) 0
                       (+ 1 (fold-left (lambda (mx x) (if (> x mx) x mx))
                                       (car all-nodes)
                                       (cdr all-nodes))))]
         ;; Add super-source and super-sink
         [source max-node]
         [sink (+ max-node 1)]
         [n (+ max-node 2)]
         [net (make-flow-network n)])
    ;; Source -> each left node (capacity 1)
    (let add-left ([ls left])
      (unless (null? ls)
        (flow-network-add-edge! net source (car ls) 1)
        (add-left (cdr ls))))
    ;; Each right node -> sink (capacity 1)
    (let add-right ([rs right])
      (unless (null? rs)
        (flow-network-add-edge! net (car rs) sink 1)
        (add-right (cdr rs))))
    ;; Left -> right edges (capacity 1)
    (let add-edges ([es edges])
      (unless (null? es)
        (let ([e (car es)])
          (flow-network-add-edge! net (car e) (cadr e) 1))
        (add-edges (cdr es))))
    ;; Run max-flow
    (let ([flow-val (max-flow net source sink)])
      ;; Extract matched pairs: edges from left to right with flow = 1
      (let ([adj (flow-network-adj net)]
            [right-set (make-vector n #f)])
        ;; Mark right nodes
        (let mark ([rs right])
          (unless (null? rs)
            (vector-set! right-set (car rs) #t)
            (mark (cdr rs))))
        ;; Find matched edges
        (let loop ([ls left] [pairs '()])
          (if (null? ls)
              (list flow-val (reverse pairs))
              (let* ([u (car ls)]
                     [u-edges (vector-ref adj u)])
                (let find ([es u-edges] [ps pairs])
                  (if (null? es)
                      (loop (cdr ls) ps)
                      (let ([e (car es)])
                        (if (and (> (edge-cap e) 0)
                                 (= (edge-flow e) 1)
                                 (vector-ref right-set (edge-to e)))
                            (loop (cdr ls) (cons (list u (edge-to e)) ps))
                            (find (cdr es) ps))))))))))))

;;; ====
;;; Convenience: Copy Network
;;; ====

(doc copy-flow-network 'type '(-> FlowNetwork FlowNetwork))
(doc copy-flow-network 'description "Deep copy a flow network so max-flow can be run without mutating the original.")
(define (copy-flow-network net)
  (doc 'export #t)
  (let* ([n (flow-network-size net)]
         [old-adj (flow-network-adj net)]
         [new-adj (make-vector n '())])
    ;; First pass: copy all edge vectors
    (let loop-u ([u 0])
      (when (< u n)
        (vector-set! new-adj u
          (map (lambda (e)
                 (vector (edge-to e) (edge-cap e) (edge-flow e) (edge-rev-idx e)))
               (vector-ref old-adj u)))
        (loop-u (+ u 1))))
    (list 'flow-network n new-adj)))

(doc reset-flow! 'type '(-> FlowNetwork Void))
(doc reset-flow! 'description "Reset all flows to 0 in a flow network.")
(define (reset-flow! net)
  (doc 'export #t)
  (let ([adj (flow-network-adj net)]
        [n (flow-network-size net)])
    (let loop-u ([u 0])
      (when (< u n)
        (let loop-e ([es (vector-ref adj u)])
          (unless (null? es)
            (edge-set-flow! (car es) 0)
            (loop-e (cdr es))))
        (loop-u (+ u 1))))))
