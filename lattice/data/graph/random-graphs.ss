;;; lattice/data/graph/random-graphs.ss — Random Graph Generators
;;; @module random-graphs
;;; @requires prelude iteration prng

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'iteration)
(require 'prng)

(doc 'module 'random-graphs)
(doc 'description "Random graph generators for the lattice. All generators return
State monad computations producing adjacency matrices. Use with-random or
run-state to execute with a PRNG seed.

Generators:
  erdos-renyi    — G(n,p) model, each edge independently with probability p
  barabasi-albert — Preferential attachment, produces scale-free networks
  watts-strogatz  — Small-world model: ring lattice with random rewiring")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================================
;;; Erdos-Renyi G(n,p)
;;; ============================================================================

(doc erdos-renyi 'type '(-> Nat Num (State RNG Matrix)))
(doc erdos-renyi 'description "Generate Erdos-Renyi random graph G(n,p).
Each possible edge (i,j) for i<j is included independently with probability p.
The result is an undirected adjacency matrix (symmetric).")
(doc erdos-renyi 'param 'n "Number of nodes")
(doc erdos-renyi 'param 'p "Edge probability in [0,1]")
(define (erdos-renyi n p)
  (doc 'export #t)
  (make-state
   (lambda (gen)
     ;; Phase 1: collect edge set with PRNG threading
     (let collect ([i 0] [j 1] [edges '()] [g gen])
       (cond
         [(>= i n)
          ;; Phase 2: tabulate adjacency matrix from edge set
          (let ([data (vec-tabulate (* n n) idx
                        (let ([r (quotient idx n)]
                              [c (remainder idx n)])
                          (if (memv (+ (* r n) c) edges) 1 0)))])
            (cons (list 'matrix n n data) g))]
         [(>= j n)
          (collect (+ i 1) (+ i 2) edges g)]
         [else
          (let* ([result (run-state random-float g)]
                 [u (car result)]
                 [g2 (cdr result)])
            (if (< u p)
                ;; Store both (i,j) and (j,i) flat indices for symmetric lookup
                (collect i (+ j 1)
                         (cons (+ (* i n) j)
                               (cons (+ (* j n) i) edges))
                         g2)
                (collect i (+ j 1) edges g2)))])))))

;;; ============================================================================
;;; Barabasi-Albert Preferential Attachment
;;; ============================================================================

(doc barabasi-albert 'type '(-> Nat Nat (State RNG Matrix)))
(doc barabasi-albert 'description "Generate Barabasi-Albert preferential attachment graph.
Starts with a complete graph on (m+1) nodes, then adds (n - m - 1) nodes one at
a time. Each new node connects to exactly m existing nodes, chosen with probability
proportional to their current degree. Produces scale-free degree distributions.")
(doc barabasi-albert 'param 'n "Total number of nodes (must be > m)")
(doc barabasi-albert 'param 'm "Number of edges each new node adds (must be >= 1)")
(define (barabasi-albert n m)
  (doc 'export #t)
  (make-state
   (lambda (gen)
     (let* ([init-nodes (+ m 1)]
            ;; Tabulate initial complete graph K_{m+1} embedded in n×n matrix
            [data (vec-tabulate (* n n) idx
                    (let ([r (quotient idx n)]
                          [c (remainder idx n)])
                      (if (and (< r init-nodes) (< c init-nodes) (not (= r c)))
                          1 0)))]
            ;; Tabulate initial degree: each node in K_{m+1} has degree m, rest have 0
            [degree (vec-tabulate n i (if (< i init-nodes) m 0))])
       ;; Add remaining nodes via preferential attachment
       (let add-node ([node init-nodes] [g gen])
         (if (>= node n)
             (cons (list 'matrix n n data) g)
             ;; Select m distinct targets proportional to degree
             (let select ([selected '()] [count 0] [g g])
               (if (>= count m)
                   (begin
                     ;; Wire selected targets
                     (for-each
                      (lambda (target)
                        (vector-set! data (+ (* node n) target) 1)
                        (vector-set! data (+ (* target n) node) 1)
                        (vector-set! degree node (+ (vector-ref degree node) 1))
                        (vector-set! degree target (+ (vector-ref degree target) 1)))
                      selected)
                     (add-node (+ node 1) g))
                   ;; Weighted selection: pick a target proportional to degree
                   ;; among nodes 0..node-1 not already selected.
                   ;; Compute total weight of eligible nodes.
                   (let* ([total-w (let sum ([k 0] [acc 0])
                                    (if (>= k node)
                                        acc
                                        (sum (+ k 1)
                                             (if (memv k selected)
                                                 acc
                                                 ;; Use degree + 1 to avoid zero-weight
                                                 ;; nodes in degenerate cases
                                                 (+ acc (+ 1 (vector-ref degree k)))))))]
                          [r-result (run-state (random-float-range 0.0 (exact->inexact total-w)) g)]
                          [r (car r-result)]
                          [g2 (cdr r-result)]
                          ;; Walk cumulative weights to find target
                          [target (let walk ([k 0] [acc 0.0])
                                   (if (>= k node)
                                       (- node 1) ;; fallback (shouldn't happen)
                                       (if (memv k selected)
                                           (walk (+ k 1) acc)
                                           (let ([new-acc (+ acc (+ 1 (vector-ref degree k)))])
                                             (if (< r new-acc)
                                                 k
                                                 (walk (+ k 1) new-acc))))))])
                     (select (cons target selected) (+ count 1) g2))))))))))

;;; ============================================================================
;;; Watts-Strogatz Small-World
;;; ============================================================================

(doc watts-strogatz 'type '(-> Nat Nat Num (State RNG Matrix)))
(doc watts-strogatz 'description "Generate Watts-Strogatz small-world graph.
Starts with a ring lattice where each node connects to its k nearest neighbors
(k/2 on each side). Then each edge (i,j) where j is a clockwise neighbor is
rewired with probability p: the endpoint j is replaced with a uniformly random
node (avoiding self-loops and duplicate edges). Requires n >> k >> ln(n) >> 1
for meaningful small-world properties.")
(doc watts-strogatz 'param 'n "Number of nodes (should be >> k)")
(doc watts-strogatz 'param 'k "Each node connects to k nearest neighbors (must be even)")
(doc watts-strogatz 'param 'p "Rewiring probability in [0,1]; 0 = ring lattice, 1 = random")
(define (watts-strogatz n k p)
  (doc 'export #t)
  (make-state
   (lambda (gen)
     (let* ([half-k (quotient k 2)]
            ;; Tabulate ring lattice: position (i,j) is 1 iff j is within
            ;; half-k steps of i on the ring (in either direction)
            [data (vec-tabulate (* n n) idx
                    (let* ([i (quotient idx n)]
                           [j (remainder idx n)]
                           [d (modulo (- j i) n)]
                           [dist (min d (- n d))])
                      (if (and (not (= i j)) (<= dist half-k)) 1 0)))])
       ;; Step 2: Rewire — for each node i and each clockwise neighbor offset d,
       ;; with probability p, replace the edge (i, (i+d) mod n) with (i, random node)
       (let rewire-i ([i 0] [g gen])
         (if (>= i n)
             (cons (list 'matrix n n data) g)
             (let rewire-d ([d 1] [g g])
               (if (> d half-k)
                   (rewire-i (+ i 1) g)
                   (let* ([j (modulo (+ i d) n)]
                          ;; Draw random float to decide rewiring
                          [r-result (run-state random-float g)]
                          [u (car r-result)]
                          [g2 (cdr r-result)])
                     (if (< u p)
                         ;; Rewire: find a valid random target
                         (let try ([g g2] [attempts 0])
                           (if (>= attempts (* 10 n))
                               ;; Give up after too many attempts (graph is too dense)
                               (rewire-d (+ d 1) g)
                               (let* ([t-result (run-state (random-int-range 0 (- n 1)) g)]
                                      [t (car t-result)]
                                      [g3 (cdr t-result)])
                                 (if (or (= t i)
                                         (= (vector-ref data (+ (* i n) t)) 1))
                                     ;; Self-loop or duplicate: try again
                                     (try g3 (+ attempts 1))
                                     ;; Valid target: rewire
                                     (begin
                                       ;; Remove old edge
                                       (vector-set! data (+ (* i n) j) 0)
                                       (vector-set! data (+ (* j n) i) 0)
                                       ;; Add new edge
                                       (vector-set! data (+ (* i n) t) 1)
                                       (vector-set! data (+ (* t n) i) 1)
                                       (rewire-d (+ d 1) g3))))))
                         ;; No rewire
                         (rewire-d (+ d 1) g2)))))))))))
