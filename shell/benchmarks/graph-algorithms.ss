;;; bench-graph-algorithms.ss — Comprehensive Benchmarking Suite for Graph Algorithms
;;;
;;; This suite provides performance benchmarks for all graph algorithms
;;; with varying graph sizes and structures. It measures execution time,
;;; memory usage, and scalability characteristics.
;;;
;;; FEATURES:
;;;   - Multiple graph structures (chain, star, tree, cyclic, dense, sparse)
;;;   - Scalability testing across different sizes (10, 50, 100, 500, 1000 nodes)
;;;   - Memory allocation tracking
;;;   - Statistical analysis (mean, median, stddev, percentiles)
;;;   - Comparison reports
;;;
;;; USAGE:
;;;   (load "core/bench-graph-algorithms.ss")
;;;   (run-all-benchmarks)          ; Run complete suite
;;;   (run-traversal-benchmarks)    ; Run specific category
;;;   (run-scalability-tests)       ; Test algorithm scaling

(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "shell/fs.ss")
(load "shell/store-api.ss")
(load "lattice/data/graph-algorithms.ss")
(load "shell/tools/benchmark.ss")

;;; ============================================================
;;; Test Graph Generators
;;; ============================================================

;;; make-chain-graph : FSCap Nat → Hash
;;; Create a chain graph: A -> B -> C -> ... -> Z
;;; Returns hash of first node.
(define (make-chain-graph fs n)
  (let loop ([i 0] [prev-hash #f])
       (if (>= i n)
           ;; Return first node's hash (stored in loop)
           (if (= n 0)
               #f
               ;; Need to get first node - traverse back or store separately
               ;; For now, we'll store first-hash in a variable
               prev-hash)
           (let* ([tag (string->symbol (format "chain-~a" i))]
                  [refs (if prev-hash (vector prev-hash) (vector))]
                  [block (make-block tag (string->utf8 "data") refs)])
                 (store-put! fs block)
                 (if (= i 0)
                     (loop (+ i 1) (hash-block block))
                     (loop (+ i 1) prev-hash))))))

;;; make-star-graph : FSCap Nat → Hash
;;; Create a star graph: Center -> Leaf1, Center -> Leaf2, ...
;;; Returns hash of center node.
(define (make-star-graph fs n)
  (if (= n 0)
      #f
      (let* ([leaves
              (let loop ([i 0] [hashes '()])
                   (if (>= i (- n 1))
                       (list->vector (reverse hashes))
                       (let ([block (make-block
                                     (string->symbol (format "leaf-~a" i))
                                     (string->utf8 "data")
                                     (vector))])
                            (store-put! fs block)
                            (loop (+ i 1) (cons (hash-block block) hashes)))))]
             [center (make-block 'center (string->utf8 "data") leaves)])
            (store-put! fs center)
            (hash-block center))))

;;; make-tree-graph : FSCap Nat Nat → Hash
;;; Create a binary tree of given depth.
;;; Returns hash of root node.
(define (make-tree-graph fs depth branching-factor)
  (let build-level ([d 0] [parent-hashes '()])
       (if (>= d depth)
           (if (null? parent-hashes) #f (car parent-hashes))
           (let ([new-hashes
                  (if (= d 0)
                      ;; Root level
                      (let ([root (make-block 'root (string->utf8 "data") (vector))])
                           (store-put! fs root)
                           (list (hash-block root)))
                      ;; Other levels
                      (let loop ([parents parent-hashes] [all-children '()])
                           (if (null? parents)
                               all-children
                               (let ([children
                                      (let child-loop ([i 0] [child-hashes '()])
                                           (if (>= i branching-factor)
                                               child-hashes
                                               (let ([child (make-block
                                                             (string->symbol (format "node-~a-~a" d i))
                                                             (string->utf8 "data")
                                                             (vector (car parents)))])
                                                    (store-put! fs child)
                                                    (child-loop (+ i 1)
                                                                (cons (hash-block child) child-hashes)))))])
                                    (loop (cdr parents)
                                          (append all-children children))))))])
                (build-level (+ d 1) new-hashes)))))

;;; make-cyclic-graph : FSCap Nat → Hash
;;; Create a cycle: A -> B -> C -> ... -> Z -> A
;;; Returns hash of first node.
(define (make-cyclic-graph fs n)
  (if (<= n 1)
      #f
      (let* ([nodes
              ;; First pass: create nodes with temp refs
              (let loop ([i 0] [acc '()])
                   (if (>= i n)
                       (reverse acc)
                       (let ([block (make-block
                                     (string->symbol (format "cycle-~a" i))
                                     (string->utf8 "data")
                                     (vector))])
                            (loop (+ i 1) (cons block acc)))))]
             [hashes (map hash-block nodes)])
            ;; Second pass: update refs to create cycle
            (let update-loop ([i 0] [nds nodes] [hs hashes])
                 (unless (null? nds)
                         (let* ([next-idx (if (= i (- n 1)) 0 (+ i 1))]
                                [next-hash (list-ref hashes next-idx)]
                                [updated (make-block
                                          (block-tag (car nds))
                                          (block-payload (car nds))
                                          (vector next-hash))])
                               (store-put! fs updated)
                               (update-loop (+ i 1) (cdr nds) (cdr hs)))))
            ;; Return first node's hash
            (car hashes))))

;;; make-dense-graph : FSCap Nat → Hash
;;; Create a densely connected graph where each node connects to most others.
;;; Returns hash of first node.
(define (make-dense-graph fs n)
  (if (= n 0)
      #f
      (let* ([nodes
              ;; Create all nodes first
              (let loop ([i 0] [acc '()])
                   (if (>= i n)
                       (reverse acc)
                       (let ([block (make-block
                                     (string->symbol (format "dense-~a" i))
                                     (string->utf8 "data")
                                     (vector))])
                            (loop (+ i 1) (cons block acc)))))]
             [hashes (map hash-block nodes)])
            ;; Update each node to reference 60% of other nodes
            (let update-loop ([i 0] [nds nodes])
                 (unless (null? nds)
                         (let* ([num-refs (max 1 (quotient (* n 6) 10))]
                                [ref-indices (take-n-skip i num-refs n)]
                                [refs (list->vector
                                       (map (lambda (idx) (list-ref hashes idx))
                                            ref-indices))]
                                [updated (make-block
                                          (block-tag (car nds))
                                          (block-payload (car nds))
                                          refs)])
                               (store-put! fs updated)
                               (update-loop (+ i 1) (cdr nds)))))
            (car hashes))))

;;; take-n-skip : Nat Nat Nat → (List Nat)
;;; Take n indices starting from (skip+1) mod total, wrapping around.
(define (take-n-skip skip n total)
  (let loop ([count 0] [current (modulo (+ skip 1) total)] [acc '()])
       (if (>= count n)
           (reverse acc)
           (loop (+ count 1)
                 (modulo (+ current 1) total)
                 (cons current acc)))))

;;; make-sparse-graph : FSCap Nat → Hash
;;; Create a sparsely connected graph (each node has 1-2 refs).
;;; Returns hash of first node.
(define (make-sparse-graph fs n)
  (if (= n 0)
      #f
      (let* ([nodes
              (let loop ([i 0] [acc '()])
                   (if (>= i n)
                       (reverse acc)
                       (let ([block (make-block
                                     (string->symbol (format "sparse-~a" i))
                                     (string->utf8 "data")
                                     (vector))])
                            (loop (+ i 1) (cons block acc)))))]
             [hashes (map hash-block nodes)])
            ;; Each node connects to 1-2 random others
            (let update-loop ([i 0] [nds nodes])
                 (unless (null? nds)
                         (let* ([num-refs (if (even? i) 1 2)]
                                [ref-indices (take-n-skip i num-refs n)]
                                [refs (list->vector
                                       (map (lambda (idx) (list-ref hashes idx))
                                            ref-indices))]
                                [updated (make-block
                                          (block-tag (car nds))
                                          (block-payload (car nds))
                                          refs)])
                               (store-put! fs updated)
                               (update-loop (+ i 1) (cdr nds)))))
            (car hashes))))

;;; ============================================================
;;; Benchmark Definitions
;;; ============================================================

;;; benchmark-traversal : FSCap String Hash → (List BenchmarkResult)
;;; Benchmark BFS and DFS traversal on given graph.
(define (benchmark-traversal fs name start-hash)
  (let ([visit-fn (lambda (h b) (void))])  ; No-op visitor
       (benchmark-compare
        `((,(string-append name " - BFS") .
           ,(lambda () (bfs-traverse fs start-hash visit-fn)))
          (,(string-append name " - DFS") .
           ,(lambda () (dfs-traverse fs start-hash visit-fn))))
        100)))  ; 100 iterations

;;; benchmark-pathfinding : FSCap String Hash Hash → (List BenchmarkResult)
;;; Benchmark pathfinding algorithms.
(define (benchmark-pathfinding fs name from-hash to-hash)
  (benchmark-compare
   `((,(string-append name " - path-exists?") .
      ,(lambda () (path-exists? fs from-hash to-hash)))
     (,(string-append name " - shortest-path") .
      ,(lambda () (shortest-path fs from-hash to-hash))))
   100))

;;; benchmark-analysis : FSCap String → (List BenchmarkResult)
;;; Benchmark graph analysis algorithms.
(define (benchmark-analysis fs name)
  (benchmark-compare
   `((,(string-append name " - connected-components") .
      ,(lambda () (connected-components fs)))
     (,(string-append name " - topological-sort") .
      ,(lambda () (topological-sort fs)))
     (,(string-append name " - find-cycles") .
      ,(lambda () (find-cycles fs)))
     (,(string-append name " - graph-stats") .
      ,(lambda () (graph-stats fs))))
   50))  ; Fewer iterations for expensive operations

;;; benchmark-centrality : FSCap String Hash → (List BenchmarkResult)
;;; Benchmark centrality metrics.
(define (benchmark-centrality fs name hash)
  (benchmark-compare
   `((,(string-append name " - in-degree") .
      ,(lambda () (in-degree fs hash)))
     (,(string-append name " - out-degree") .
      ,(lambda () (out-degree fs hash)))
     (,(string-append name " - total-degree") .
      ,(lambda () (total-degree fs hash)))
     (,(string-append name " - find-hubs") .
      ,(lambda () (find-hubs fs 10))))
   100))

;;; ============================================================
;;; Test Suites
;;; ============================================================

;;; run-traversal-benchmarks : → void
;;; Run traversal benchmarks on different graph structures.
(define (run-traversal-benchmarks)
  (display "
╔═══════════════════════════════════════════════════════════════╗
")
  (display "║           GRAPH TRAVERSAL BENCHMARKS                          ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")
  
  (let ([fs (make-fs-capability ".store-bench")])
       ;; Clean store
       (unless (file-exists? ".store-bench")
               (mkdir ".store-bench")
               (mkdir ".store-bench/objects"))
       
       ;; Chain graph (100 nodes)
       (display "
--- Chain Graph (100 nodes) ---
")
       (let ([start (make-chain-graph fs 100)])
            (benchmark-report (benchmark-traversal fs "Chain-100" start)))
       
       ;; Star graph (100 nodes)
       (display "
--- Star Graph (100 nodes) ---
")
       (let ([center (make-star-graph fs 100)])
            (benchmark-report (benchmark-traversal fs "Star-100" center)))
       
       ;; Binary tree (depth 7 ≈ 127 nodes)
       (display "
--- Binary Tree (depth 7) ---
")
       (let ([root (make-tree-graph fs 7 2)])
            (benchmark-report (benchmark-traversal fs "Tree-7" root)))))

;;; run-pathfinding-benchmarks : → void
;;; Run pathfinding benchmarks.
(define (run-pathfinding-benchmarks)
  (display "
╔═══════════════════════════════════════════════════════════════╗
")
  (display "║           PATHFINDING BENCHMARKS                              ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")
  
  (let ([fs (make-fs-capability ".store-bench")])
       ;; Clean store
       (unless (file-exists? ".store-bench")
               (mkdir ".store-bench")
               (mkdir ".store-bench/objects"))
       
       ;; Chain: pathfind from start to end
       (display "
--- Chain Graph Pathfinding ---
")
       (let ([start (make-chain-graph fs 50)])
            ;; Get last node by traversing
            (let ([end-hash #f])
                 (bfs-traverse fs start
                               (lambda (h b)
                                       (when (= (out-degree fs h) 0)
                                             (set! end-hash h))))
                 (when end-hash
                       (benchmark-report
                        (benchmark-pathfinding fs "Chain-50" start end-hash)))))))

;;; run-analysis-benchmarks : → void
;;; Run graph analysis benchmarks.
(define (run-analysis-benchmarks)
  (display "
╔═══════════════════════════════════════════════════════════════╗
")
  (display "║           GRAPH ANALYSIS BENCHMARKS                           ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")
  
  (let ([fs (make-fs-capability ".store-bench")])
       ;; Clean store
       (unless (file-exists? ".store-bench")
               (mkdir ".store-bench")
               (mkdir ".store-bench/objects"))
       
       ;; Dense graph
       (display "
--- Dense Graph (50 nodes) ---
")
       (make-dense-graph fs 50)
       (benchmark-report (benchmark-analysis fs "Dense-50"))
       
       ;; Sparse graph
       (display "
--- Sparse Graph (100 nodes) ---
")
       (make-sparse-graph fs 100)
       (benchmark-report (benchmark-analysis fs "Sparse-100"))))

;;; run-scalability-tests : → void
;;; Test how algorithms scale with graph size.
(define (run-scalability-tests)
  (display "
╔═══════════════════════════════════════════════════════════════╗
")
  (display "║           SCALABILITY TESTS                                   ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")
  
  (display "
Testing BFS traversal scaling on chain graphs:
")
  (display "───────────────────────────────────────────────────────────────
")
  
  (let ([sizes '(10 50 100 500)])
       (for-each
        (lambda (size)
                (let ([fs (make-fs-capability ".store-bench")])
                     ;; Clean store for each test
                     (unless (file-exists? ".store-bench")
                             (mkdir ".store-bench"))
                     (when (file-exists? ".store-bench/objects")
                           (system "rm -rf .store-bench/objects/*"))
                     (unless (file-exists? ".store-bench/objects")
                             (mkdir ".store-bench/objects"))
                     
                     (let* ([start (make-chain-graph fs size)]
                            [visit-fn (lambda (h b) (void))]
                            [result (benchmark (format "Chain-~a" size)
                                               (lambda () (bfs-traverse fs start visit-fn))
                                               100)])
                           (display (format "  Size ~a: ~a (mean)
"
                                            size
                                            (format-time-ns
                                             (benchmark-result-mean-ns result)))))))
        sizes)))

;;; run-all-benchmarks : → void
;;; Run complete benchmark suite.
(define (run-all-benchmarks)
  (display "
")
  (display "╔═══════════════════════════════════════════════════════════════╗
")
  (display "║     COMPREHENSIVE GRAPH ALGORITHM BENCHMARK SUITE             ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")
  (display "
")
  (display "Running complete suite...
")
  
  (run-traversal-benchmarks)
  (run-pathfinding-benchmarks)
  (run-analysis-benchmarks)
  (run-scalability-tests)
  
  (display "
")
  (display "╔═══════════════════════════════════════════════════════════════╗
")
  (display "║     BENCHMARK SUITE COMPLETE                                  ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")
  (display "
"))

;;; ============================================================
;;; Quick Test Functions
;;; ============================================================

;;; quick-benchmark : → void
;;; Quick test of basic functionality.
(define (quick-benchmark)
  (display "
Quick benchmark test...
")
  (let ([fs (make-fs-capability ".store-bench-quick")])
       (unless (file-exists? ".store-bench-quick")
               (mkdir ".store-bench-quick")
               (mkdir ".store-bench-quick/objects"))
       
       (let ([start (make-chain-graph fs 20)])
            (let ([result (benchmark "Quick BFS test"
                                     (lambda ()
                                             (bfs-traverse fs start
                                                           (lambda (h b) (void))))
                                     50)])
                 (benchmark-report (list result))))))

;;; ============================================================
;;; Load Message
;;; ============================================================

(display "
")
(display "Graph algorithm benchmarking suite loaded.
")
(display "
")
(display "Available functions:
")
(display "  (run-all-benchmarks)         - Run complete suite
")
(display "  (run-traversal-benchmarks)   - Traversal algorithms only
")
(display "  (run-pathfinding-benchmarks) - Pathfinding algorithms only
")
(display "  (run-analysis-benchmarks)    - Analysis algorithms only
")
(display "  (run-scalability-tests)      - Scalability tests
")
(display "  (quick-benchmark)            - Quick functionality test
")
(display "
")
