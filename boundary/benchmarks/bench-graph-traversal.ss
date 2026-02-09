(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "boundary/io/fs.ss")
(load "boundary/storage/store-api.ss")
(load "lattice/data/graph/graph-algorithms.ss")
(load "boundary/blocks/graph-traversal.ss")
(load "boundary/tools/benchmark.ss")

(doc 'module 'graph-traversal-benchmarks)
(doc 'description "Comprehensive benchmarking suite for graph algorithms with varying graph sizes and structures, measuring execution time, memory usage, and scalability")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(block sha256 fs store-api graph-algorithms benchmark))

(doc 'section 'test-graph-generators)

(define (make-chain-graph fs n)
  (doc 'type (-> FSCap Nat Hash))
  (doc 'description "Create a chain graph: A -> B -> C -> ... -> Z. Returns hash of first node.")
  (doc 'export #t)
  (let loop ([i 0] [prev-hash #f])
       (if (>= i n)
           (if (= n 0)
               #f
               prev-hash)
           (let* ([tag (string->symbol (format "chain-~a" i))]
                  [refs (if prev-hash (vector prev-hash) (vector))]
                  [block (make-block tag (string->utf8 "data") refs)])
                 (store-put! fs block)
                 (if (= i 0)
                     (loop (+ i 1) (hash-block block))
                     (loop (+ i 1) prev-hash))))))

(define (make-star-graph fs n)
  (doc 'type (-> FSCap Nat Hash))
  (doc 'description "Create a star graph: Center -> Leaf1, Center -> Leaf2, ... Returns hash of center node.")
  (doc 'export #t)
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

(define (make-tree-graph fs depth branching-factor)
  (doc 'type (-> FSCap Nat Nat Hash))
  (doc 'description "Create a tree of given depth and branching factor. Returns hash of root node.")
  (doc 'export #t)
  (let build-level ([d 0] [parent-hashes '()])
       (if (>= d depth)
           (if (null? parent-hashes) #f (car parent-hashes))
           (let ([new-hashes
                  (if (= d 0)
                      (let ([root (make-block 'root (string->utf8 "data") (vector))])
                           (store-put! fs root)
                           (list (hash-block root)))
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

(define (make-cyclic-graph fs n)
  (doc 'type (-> FSCap Nat Hash))
  (doc 'description "Create a cycle: A -> B -> C -> ... -> Z -> A. Returns hash of first node.")
  (doc 'export #t)
  (if (<= n 1)
      #f
      (let* ([nodes
              (let loop ([i 0] [acc '()])
                   (if (>= i n)
                       (reverse acc)
                       (let ([block (make-block
                                     (string->symbol (format "cycle-~a" i))
                                     (string->utf8 "data")
                                     (vector))])
                            (loop (+ i 1) (cons block acc)))))]
             [hashes (map hash-block nodes)])
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
            (car hashes))))

(define (make-dense-graph fs n)
  (doc 'type (-> FSCap Nat Hash))
  (doc 'description "Create a densely connected graph where each node connects to 60% of others. Returns hash of first node.")
  (doc 'export #t)
  (if (= n 0)
      #f
      (let* ([nodes
              (let loop ([i 0] [acc '()])
                   (if (>= i n)
                       (reverse acc)
                       (let ([block (make-block
                                     (string->symbol (format "dense-~a" i))
                                     (string->utf8 "data")
                                     (vector))])
                            (loop (+ i 1) (cons block acc)))))]
             [hashes (map hash-block nodes)])
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

(define (take-n-skip skip n total)
  (doc 'type (-> Nat Nat Nat (List Nat)))
  (doc 'description "Take n indices starting from (skip+1) mod total, wrapping around")
  (let loop ([count 0] [current (modulo (+ skip 1) total)] [acc '()])
       (if (>= count n)
           (reverse acc)
           (loop (+ count 1)
                 (modulo (+ current 1) total)
                 (cons current acc)))))

(define (make-sparse-graph fs n)
  (doc 'type (-> FSCap Nat Hash))
  (doc 'description "Create a sparsely connected graph where each node has 1-2 refs. Returns hash of first node.")
  (doc 'export #t)
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

(doc 'section 'benchmark-definitions)

(define (benchmark-traversal fs name start-hash)
  (doc 'type (-> FSCap String Hash (List BenchmarkResult)))
  (doc 'description "Benchmark BFS and DFS traversal on given graph")
  (doc 'export #t)
  (let ([visit-fn (lambda (h b) (void))])
       (benchmark-compare
        `((,(string-append name " - BFS") .
           ,(lambda () (bfs-traverse fs start-hash visit-fn)))
          (,(string-append name " - DFS") .
           ,(lambda () (dfs-traverse fs start-hash visit-fn))))
        100)))

(define (benchmark-pathfinding fs name from-hash to-hash)
  (doc 'type (-> FSCap String Hash Hash (List BenchmarkResult)))
  (doc 'description "Benchmark pathfinding algorithms")
  (doc 'export #t)
  (benchmark-compare
   `((,(string-append name " - path-exists?") .
      ,(lambda () (path-exists? fs from-hash to-hash)))
     (,(string-append name " - shortest-path") .
      ,(lambda () (shortest-path fs from-hash to-hash))))
   100))

(define (benchmark-analysis fs name)
  (doc 'type (-> FSCap String (List BenchmarkResult)))
  (doc 'description "Benchmark graph analysis algorithms")
  (doc 'export #t)
  (benchmark-compare
   `((,(string-append name " - connected-components") .
      ,(lambda () (connected-components fs)))
     (,(string-append name " - topological-sort") .
      ,(lambda () (topological-sort fs)))
     (,(string-append name " - find-cycles") .
      ,(lambda () (find-cycles fs)))
     (,(string-append name " - graph-stats") .
      ,(lambda () (graph-stats fs))))
   50))

(define (benchmark-centrality fs name hash)
  (doc 'type (-> FSCap String Hash (List BenchmarkResult)))
  (doc 'description "Benchmark centrality metrics")
  (doc 'export #t)
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

(doc 'section 'test-suites)

(define (run-traversal-benchmarks)
  (doc 'type (-> Void))
  (doc 'description "Run traversal benchmarks on different graph structures")
  (doc 'export #t)
  (display "
╔═══════════════════════════════════════════════════════════════╗
")
  (display "║           GRAPH TRAVERSAL BENCHMARKS                          ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")

  (let ([fs (make-fs-capability ".store-bench")])
       (unless (file-exists? ".store-bench")
               (mkdir ".store-bench")
               (mkdir ".store-bench/objects"))

       (display "
--- Chain Graph (100 nodes) ---
")
       (let ([start (make-chain-graph fs 100)])
            (benchmark-report (benchmark-traversal fs "Chain-100" start)))

       (display "
--- Star Graph (100 nodes) ---
")
       (let ([center (make-star-graph fs 100)])
            (benchmark-report (benchmark-traversal fs "Star-100" center)))

       (display "
--- Binary Tree (depth 7) ---
")
       (let ([root (make-tree-graph fs 7 2)])
            (benchmark-report (benchmark-traversal fs "Tree-7" root)))))

(define (run-pathfinding-benchmarks)
  (doc 'type (-> Void))
  (doc 'description "Run pathfinding benchmarks")
  (doc 'export #t)
  (display "
╔═══════════════════════════════════════════════════════════════╗
")
  (display "║           PATHFINDING BENCHMARKS                              ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")

  (let ([fs (make-fs-capability ".store-bench")])
       (unless (file-exists? ".store-bench")
               (mkdir ".store-bench")
               (mkdir ".store-bench/objects"))

       (display "
--- Chain Graph Pathfinding ---
")
       (let ([start (make-chain-graph fs 50)])
            (let ([end-hash #f])
                 (bfs-traverse fs start
                               (lambda (h b)
                                       (when (= (out-degree fs h) 0)
                                             (set! end-hash h))))
                 (when end-hash
                       (benchmark-report
                        (benchmark-pathfinding fs "Chain-50" start end-hash)))))))

(define (run-analysis-benchmarks)
  (doc 'type (-> Void))
  (doc 'description "Run graph analysis benchmarks")
  (doc 'export #t)
  (display "
╔═══════════════════════════════════════════════════════════════╗
")
  (display "║           GRAPH ANALYSIS BENCHMARKS                           ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")

  (let ([fs (make-fs-capability ".store-bench")])
       (unless (file-exists? ".store-bench")
               (mkdir ".store-bench")
               (mkdir ".store-bench/objects"))

       (display "
--- Dense Graph (50 nodes) ---
")
       (make-dense-graph fs 50)
       (benchmark-report (benchmark-analysis fs "Dense-50"))

       (display "
--- Sparse Graph (100 nodes) ---
")
       (make-sparse-graph fs 100)
       (benchmark-report (benchmark-analysis fs "Sparse-100"))))

(define (run-scalability-tests)
  (doc 'type (-> Void))
  (doc 'description "Test how algorithms scale with graph size")
  (doc 'export #t)
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

(define (run-all-benchmarks)
  (doc 'type (-> Void))
  (doc 'description "Run complete benchmark suite")
  (doc 'export #t)
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

(doc 'section 'quick-test-functions)

(define (quick-benchmark)
  (doc 'type (-> Void))
  (doc 'description "Quick test of basic functionality")
  (doc 'export #t)
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

(doc 'section 'load-message)

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
