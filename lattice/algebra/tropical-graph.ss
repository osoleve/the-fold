;;; lattice/algebra/tropical-graph.ss — Tropical Algebra / Graph Algorithms Bridge
;;; @module tropical-graph
;;; @requires prelude tropical graph-matrix

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'tropical)
(require 'graph-matrix)

(doc 'module 'tropical-graph)
(doc 'description "Bridge between tropical algebra and graph algorithms.
Demonstrates that classical graph algorithms are special cases of tropical
semiring computation: Floyd-Warshall = tropical closure over min-plus,
Warshall's transitive closure = tropical closure over the boolean semiring,
critical path = tropical closure over max-plus.")
(doc 'layer 'lattice)
(doc 'purity 'observably-pure)

(doc 'note "The fundamental insight: graph algorithms are linear algebra over")
(doc 'note "the 'wrong' semiring. Shortest paths, longest paths, reachability,")
(doc 'note "and bottleneck paths all fall out as instances of A* = I + A + A^2 + ...")
(doc 'note "over different semirings. This module makes that unity concrete.")

;;; ====
;;; Section 1: Additional Semirings for Graph Problems
;;; ====

(doc 'section 'graph-semirings)

;;; boolean-semiring : Semiring
;;; The boolean semiring ({0,1}, max, *, 0, 1).
;;; Tropical closure over this semiring computes transitive closure:
;;; A*[i,j] = 1 iff j is reachable from i.
(define boolean-semiring
  (make-semiring max * 0 1 =))
(doc 'export #t)

;;; min-max-semiring : Semiring
;;; The bottleneck semiring (R, max, min, -inf, +inf).
;;; Tropical closure over this computes widest paths: A*[i,j] is the
;;; maximum over all i->j paths of the minimum edge weight on that path.
;;; Useful for maximum-capacity routing.
(define min-max-semiring
  (make-semiring max min -inf.0 +inf.0 =))
(doc 'export #t)

;;; ====
;;; Section 2: Graph -> Tropical Conversion (Weighted)
;;; ====

(doc 'section 'weighted-conversion)

;;; graph->tropical-matrix : Matrix -> Matrix
;;; Convert a graph-matrix adjacency matrix (0 = no edge, *infinity* = 1e308)
;;; to a min-plus tropical matrix (+inf.0 = no edge, 0 on diagonal).
;;; This is a thin wrapper around adjacency->tropical specialized to min-plus.
(define (graph->tropical-matrix adj)
  (doc 'export #t)
  (adjacency->tropical min-plus-semiring adj))

;;; graph->boolean-matrix : Matrix -> Matrix
;;; Convert a graph-matrix adjacency matrix to boolean form for the
;;; boolean semiring: edge present -> 1, absent -> 0, diagonal -> 1.
(define (graph->boolean-matrix adj)
  (doc 'export #t)
  (let* ([n (matrix-rows adj)]
         [data (make-vector (* n n) 0)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let ([idx (+ (* i n) j)]
              [val (matrix-ref adj i j)])
          (cond
           [(= i j) (vector-set! data idx 1)]
           [(and (number? val) (> val 0) (< val 1e308))
            (vector-set! data idx 1)]
           [else (vector-set! data idx 0)]))))
    (list 'matrix n n data)))

;;; graph->max-plus-matrix : Matrix -> Matrix
;;; Convert a graph-matrix adjacency matrix to max-plus tropical form.
;;; No-edge -> -inf.0 (max-plus additive identity), diagonal -> 0.
(define (graph->max-plus-matrix adj)
  (doc 'export #t)
  (let* ([n (matrix-rows adj)]
         [data (make-vector (* n n) -inf.0)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let ([idx (+ (* i n) j)]
              [val (matrix-ref adj i j)])
          (cond
           [(= i j) (vector-set! data idx 0)]
           [(and (number? val) (> val 0) (< val 1e308))
            (vector-set! data idx val)]
           [else (vector-set! data idx -inf.0)]))))
    (list 'matrix n n data)))

;;; graph->bottleneck-matrix : Matrix -> Matrix
;;; Convert a graph-matrix adjacency matrix to bottleneck (min-max) form.
;;; No-edge -> -inf.0, diagonal -> +inf.0.
(define (graph->bottleneck-matrix adj)
  (doc 'export #t)
  (let* ([n (matrix-rows adj)]
         [data (make-vector (* n n) -inf.0)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (let ([idx (+ (* i n) j)]
              [val (matrix-ref adj i j)])
          (cond
           [(= i j) (vector-set! data idx +inf.0)]
           [(and (number? val) (> val 0) (< val 1e308))
            (vector-set! data idx val)]
           [else (vector-set! data idx -inf.0)]))))
    (list 'matrix n n data)))

;;; ====
;;; Section 3: Core Graph Algorithms via Tropical Closure
;;; ====

(doc 'section 'tropical-graph-algorithms)

(doc 'note "Each algorithm below is a two-step pipeline:")
(doc 'note "  1. Convert adjacency matrix to the appropriate semiring form")
(doc 'note "  2. Compute tropical-matrix-closure (the Kleene star A*)")
(doc 'note "The closure IS the algorithm. Different semirings yield different")
(doc 'note "classical algorithms, all from the same O(n^3) DP template.")

;;; tropical-shortest-paths : Matrix -> Matrix
;;; All-pairs shortest paths via tropical closure over min-plus.
;;; This IS Floyd-Warshall, expressed as A* over the min-plus semiring.
;;; Input: graph-matrix adjacency format (0 = no edge).
;;; Output: distance matrix with +inf.0 for unreachable pairs, 0 on diagonal.
(define (tropical-shortest-paths adj)
  (doc 'export #t)
  (doc 'type '(-> Matrix Matrix))
  (tropical-matrix-closure min-plus-semiring
                           (graph->tropical-matrix adj)))

;;; tropical-transitive-closure : Matrix -> Matrix
;;; Reachability via tropical closure over the boolean semiring.
;;; This IS Warshall's algorithm, expressed as A* over ({0,1}, max, *, 0, 1).
;;; Input: graph-matrix adjacency format (0 = no edge).
;;; Output: matrix where entry (i,j) = 1 iff j is reachable from i.
(define (tropical-transitive-closure adj)
  (doc 'export #t)
  (doc 'type '(-> Matrix Matrix))
  (tropical-matrix-closure boolean-semiring
                           (graph->boolean-matrix adj)))

;;; tropical-longest-paths : Matrix -> Matrix
;;; All-pairs longest paths via tropical closure over max-plus.
;;; Also known as critical path analysis.
;;; WARNING: only meaningful for DAGs (no positive cycles).
;;; Input: graph-matrix adjacency format (0 = no edge).
;;; Output: distance matrix with -inf.0 for unreachable pairs, 0 on diagonal.
(define (tropical-longest-paths adj)
  (doc 'export #t)
  (doc 'type '(-> Matrix Matrix))
  (tropical-matrix-closure max-plus-semiring
                           (graph->max-plus-matrix adj)))

;;; tropical-bottleneck-paths : Matrix -> Matrix
;;; All-pairs widest (bottleneck) paths via tropical closure over min-max.
;;; Entry (i,j) = maximum over all i->j paths of the minimum edge weight.
;;; Useful for maximum-capacity routing and network reliability.
;;; Input: graph-matrix adjacency format (0 = no edge).
;;; Output: capacity matrix with -inf.0 for unreachable pairs.
(define (tropical-bottleneck-paths adj)
  (doc 'export #t)
  (doc 'type '(-> Matrix Matrix))
  (tropical-matrix-closure min-max-semiring
                           (graph->bottleneck-matrix adj)))

;;; ====
;;; Section 4: Result Extraction Utilities
;;; ====

(doc 'section 'extraction)

;;; tropical-distance : Matrix -> Nat -> Nat -> Number
;;; Extract shortest-path distance between two nodes from a tropical
;;; distance matrix. Returns +inf.0 if unreachable.
(define (tropical-distance dist i j)
  (doc 'export #t)
  (matrix-ref dist i j))

;;; tropical-reachable? : Matrix -> Nat -> Nat -> Boolean
;;; Check if node j is reachable from node i in a transitive closure matrix.
(define (tropical-reachable? tc i j)
  (doc 'export #t)
  (> (matrix-ref tc i j) 0))

;;; tropical-distance-to-graph : Matrix -> Matrix
;;; Convert a tropical distance matrix back to graph-matrix format.
;;; Thin wrapper around tropical->adjacency.
(define (tropical-distance-to-graph dist)
  (doc 'export #t)
  (tropical->adjacency dist))

;;; ====
;;; Section 5: Comparison / Verification
;;; ====

(doc 'section 'verification)

(doc 'note "Functions for verifying that tropical and classical algorithms agree.")
(doc 'note "Useful for testing and for demonstrating the algebraic unity.")

;;; tropical-agrees-with-floyd-warshall? : Matrix -> Boolean
;;; Verify that tropical-shortest-paths and floyd-warshall produce
;;; equivalent distance matrices on the same input graph.
;;; Accounts for the different infinity representations:
;;;   graph-matrix uses *infinity* (1e308), tropical uses +inf.0.
(define (tropical-agrees-with-floyd-warshall? adj)
  (doc 'export #t)
  (let* ([fw (floyd-warshall adj)]
         [tsp (tropical-shortest-paths adj)]
         [n (matrix-rows adj)])
    (let loop ([i 0])
      (if (= i n)
          #t
          (let inner ([j 0])
            (if (= j n)
                (loop (+ i 1))
                (let ([fw-val (matrix-ref fw i j)]
                      [tsp-val (matrix-ref tsp i j)])
                  (let ([fw-inf? (>= fw-val 1e308)]
                        [tsp-inf? (not (finite? tsp-val))])
                    (if (and (eq? fw-inf? tsp-inf?)
                             (or fw-inf? (= fw-val tsp-val)))
                        (inner (+ j 1))
                        #f)))))))))

;;; tropical-agrees-with-transitive-closure? : Matrix -> Boolean
;;; Verify that tropical-transitive-closure agrees with graph-matrix's
;;; transitive-closure (Warshall's algorithm).
(define (tropical-agrees-with-transitive-closure? adj)
  (doc 'export #t)
  (let* ([tc-classical (transitive-closure adj)]
         [tc-tropical (tropical-transitive-closure adj)]
         [n (matrix-rows adj)])
    (let loop ([i 0])
      (if (= i n)
          #t
          (let inner ([j 0])
            (if (= j n)
                (loop (+ i 1))
                (let ([c-val (matrix-ref tc-classical i j)]
                      [t-val (matrix-ref tc-tropical i j)])
                  ;; Classical uses >0 for reachable, tropical uses exactly 1
                  (if (eq? (> c-val 0) (> t-val 0))
                      (inner (+ j 1))
                      #f))))))))
