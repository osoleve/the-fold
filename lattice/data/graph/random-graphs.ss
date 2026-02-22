;;; lattice/data/graph/random-graphs.ss — Random Graph Generators
;;; @module random-graphs
;;; @requires prelude graph-matrix prng

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'graph-matrix)
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
  (make-state
   (lambda (gen)
     (let ([data (make-vector (* n n) 0)])
       (let loop-i ([i 0] [g gen])
         (if (>= i n)
             (cons (list 'matrix n n data) g)
             (let loop-j ([j (+ i 1)] [g g])
               (if (>= j n)
                   (loop-i (+ i 1) g)
                   ;; Draw a uniform float and compare to p
                   (let* ([result (run-state random-float g)]
                          [u (car result)]
                          [g2 (cdr result)])
                     (when (< u p)
                       (vector-set! data (+ (* i n) j) 1)
                       (vector-set! data (+ (* j n) i) 1))
                     (loop-j (+ j 1) g2))))))))))

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
  (make-state
   (lambda (gen)
     (let* ([init-nodes (+ m 1)]
            [data (make-vector (* n n) 0)]
            ;; degree[i] tracks degree of node i
            [degree (make-vector n 0)])
       ;; Initialize complete graph on nodes 0..m
       (let init-i ([i 0])
         (when (< i init-nodes)
           (let init-j ([j (+ i 1)])
             (when (< j init-nodes)
               (vector-set! data (+ (* i n) j) 1)
               (vector-set! data (+ (* j n) i) 1)
               (vector-set! degree i (+ (vector-ref degree i) 1))
               (vector-set! degree j (+ (vector-ref degree j) 1))
               (init-j (+ j 1))))
           (init-i (+ i 1))))
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
  (make-state
   (lambda (gen)
     (let* ([half-k (quotient k 2)]
            [data (make-vector (* n n) 0)])
       ;; Step 1: Build ring lattice — connect each node to k/2 neighbors on each side
       (let build ([i 0])
         (when (< i n)
           (let connect ([d 1])
             (when (<= d half-k)
               (let ([j (modulo (+ i d) n)])
                 (vector-set! data (+ (* i n) j) 1)
                 (vector-set! data (+ (* j n) i) 1)
                 (connect (+ d 1)))))
           (build (+ i 1))))
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

;;; ============================================================================
;;; Load Complete
;;; ============================================================================

(printf "~c random-graphs loaded\n" #\x2713)
(printf "  (erdos-renyi n p)       - G(n,p) random graph\n")
(printf "  (barabasi-albert n m)   - Preferential attachment\n")
(printf "  (watts-strogatz n k p)  - Small-world network\n")
(printf "  Use: (with-random seed (erdos-renyi 10 0.3))\n")
