;;; boundary/tests/test-profile-call-graph.ss — Tests for Call Graph Builder
;;;
;;; Comprehensive test suite for profile-call-graph.ss
;;; Tests graph construction, edge extraction, metrics, cycle detection.

(load "boundary/diagnostics/profile-call-graph.ss")

;;; ====
;;; Test Harness
;;; ====

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (assert-equal name actual expected)
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

(define (assert-true name actual)
  (set! test-count (+ test-count 1))
  (if actual  ; truthy check, not (eq? actual #t)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "    Expected: truthy value\n")
       (printf "    Actual:   ~s\n" actual))))

(define (assert-false name actual)
  (assert-equal name actual #f))

(define (run-tests)
  (set! test-count 0)
  (set! pass-count 0)
  (set! fail-count 0)
  
  (test-graph-construction)
  (test-edge-operations)
  (test-graph-metrics)
  (test-cycle-detection)
  (test-critical-path)
  (test-ascii-visualization)
  (test-profiler-integration)
  
  (printf "\n~a/~a tests passed\n" pass-count test-count)
  (when (> fail-count 0)
        (printf "~a tests FAILED\n" fail-count)
        (exit 1)))

;;; ====
;;; Mock Node Construction
;;; ====

;;; Create a mock profiler node for testing
(define (mock-node name start-fuel end-fuel children)
  `(node
    (name . ,name)
    (start-fuel . ,start-fuel)
    (end-fuel . ,end-fuel)
    (children . ,children)
    (call-count . 1)))

(define (mock-node-with-count name start-fuel end-fuel children count)
  `(node
    (name . ,name)
    (start-fuel . ,start-fuel)
    (end-fuel . ,end-fuel)
    (children . ,children)
    (call-count . ,count)))

;;; ====
;;; Graph Construction Tests
;;; ====

(define (test-graph-construction)
  (printf "\n=== Graph Construction Tests ===\n")
  
  ;; Test: Empty graph creation
  (let ([g (make-call-graph)])
       (assert-true "make-call-graph creates call-graph"
                    (call-graph? g))
       (assert-equal "empty graph has no nodes"
                     (call-graph-all-nodes g)
                     '()))
  
  ;; Test: Simple tree -> graph
  ;; root -> a, b
  ;;   a -> c
  (let* ([c (mock-node 'c 50 40 '())]
         [a (mock-node 'a 80 50 (list c))]
         [b (mock-node 'b 70 60 '())]
         [root (mock-node 'root 100 0 (list a b))]
         [g (build-call-graph-from-node root)])
        (assert-true "build from node creates graph"
                     (call-graph? g))
        (let ([nodes (call-graph-all-nodes g)])
             (assert-true "graph contains root"
                          (memq 'root nodes))
             (assert-true "graph contains a"
                          (memq 'a nodes))
             (assert-true "graph contains b"
                          (memq 'b nodes))
             (assert-true "graph contains c"
                          (memq 'c nodes))))
  
  ;; Test: Graph from node with multiple call counts
  (let* ([helper (mock-node-with-count 'helper 50 40 '() 3)]
         [main (mock-node 'main 100 0 (list helper))]
         [g (build-call-graph-from-node main)])
        (let ([edges (call-graph-callees g 'main)])
             (assert-equal "multiple calls create multiple edge count"
                           (assq 'helper edges)
                           '(helper . 3)))))

;;; ====
;;; Edge Operations Tests
;;; ====

(define (test-edge-operations)
  (printf "\n=== Edge Operations Tests ===\n")
  
  ;; Test: Add single edge
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'foo 'bar)
       (assert-equal "add edge creates callee entry"
                     (call-graph-callees g 'foo)
                     '((bar . 1)))
       (assert-equal "add edge creates caller entry"
                     (call-graph-callers g 'bar)
                     '((foo . 1))))
  
  ;; Test: Add multiple edges from same caller
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'main 'helper1)
       (call-graph-add-edge! g 'main 'helper2)
       (let ([callees (call-graph-callees g 'main)])
            (assert-equal "two distinct callees"
                          (length callees) 2)
            (assert-true "helper1 is callee"
                         (assq 'helper1 callees))
            (assert-true "helper2 is callee"
                         (assq 'helper2 callees))))
  
  ;; Test: Add same edge multiple times (increment count)
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'loop 'body)
       (call-graph-add-edge! g 'loop 'body)
       (call-graph-add-edge! g 'loop 'body)
       (assert-equal "repeated edges increment count"
                     (call-graph-callees g 'loop)
                     '((body . 3))))
  
  ;; Test: Callers and callees are independent
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'a 'b)
       (call-graph-add-edge! g 'c 'b)
       (let ([callers (call-graph-callers g 'b)])
            (assert-equal "b has two callers"
                          (length callers) 2))))

;;; ====
;;; Graph Metrics Tests
;;; ====

(define (test-graph-metrics)
  (printf "\n=== Graph Metrics Tests ===\n")
  
  ;; Build test graph:
  ;;   main -> helper1, helper2
  ;;   helper1 -> util
  ;;   helper2 -> util
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'main 'helper1)
       (call-graph-add-edge! g 'main 'helper2)
       (call-graph-add-edge! g 'helper1 'util)
       (call-graph-add-edge! g 'helper2 'util)
       
       ;; Test: Roots
       (assert-equal "main is only root"
                     (call-graph-roots g)
                     '(main))
       
       ;; Test: Leaves
       (assert-equal "util is only leaf"
                     (call-graph-leaves g)
                     '(util))
       
       ;; Test: Fan-out
       (assert-equal "main has fan-out of 2"
                     (call-graph-fan-out g 'main)
                     2)
       (assert-equal "helper1 has fan-out of 1"
                     (call-graph-fan-out g 'helper1)
                     1)
       (assert-equal "util has fan-out of 0"
                     (call-graph-fan-out g 'util)
                     0)
       
       ;; Test: Fan-in
       (assert-equal "util has fan-in of 2"
                     (call-graph-fan-in g 'util)
                     2)
       (assert-equal "main has fan-in of 0"
                     (call-graph-fan-in g 'main)
                     0)
       
       ;; Test: Depth
       (assert-equal "main has depth 2"
                     (call-graph-depth g 'main)
                     2)
       (assert-equal "helper1 has depth 1"
                     (call-graph-depth g 'helper1)
                     1)
       (assert-equal "util has depth 0"
                     (call-graph-depth g 'util)
                     0)
       
       ;; Test: Total calls
       (assert-equal "total calls is 4"
                     (call-graph-total-calls g)
                     4))
  
  ;; Test: Empty graph metrics
  (let ([g (make-call-graph)])
       (assert-equal "empty graph has no roots"
                     (call-graph-roots g)
                     '())
       (assert-equal "empty graph has no leaves"
                     (call-graph-leaves g)
                     '())
       (assert-equal "empty graph has 0 total calls"
                     (call-graph-total-calls g)
                     0)))

;;; ====
;;; Cycle Detection Tests
;;; ====

(define (test-cycle-detection)
  (printf "\n=== Cycle Detection Tests ===\n")
  
  ;; Test: No cycles in acyclic graph
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'a 'b)
       (call-graph-add-edge! g 'b 'c)
       (assert-equal "acyclic graph has no cycles"
                     (find-call-cycles g)
                     '()))
  
  ;; Test: Simple self-recursion
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'fact 'fact)
       (let ([cycles (find-call-cycles g)])
            (assert-equal "self-recursion found"
                          (length cycles) 1)
            (assert-equal "cycle contains fact"
                          (car cycles)
                          '(fact))))
  
  ;; Test: Mutual recursion (a -> b -> a)
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'even? 'odd?)
       (call-graph-add-edge! g 'odd? 'even?)
       (let ([cycles (find-call-cycles g)])
            (assert-equal "mutual recursion found"
                          (length cycles) 1)
            ;; Cycle could be (even? odd?) or (odd? even?)
            (assert-true "cycle has length 2"
                         (= (length (car cycles)) 2))))
  
  ;; Test: Longer cycle (a -> b -> c -> a)
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'a 'b)
       (call-graph-add-edge! g 'b 'c)
       (call-graph-add-edge! g 'c 'a)
       (let ([cycles (find-call-cycles g)])
            (assert-equal "3-node cycle found"
                          (length cycles) 1)
            (assert-equal "cycle has 3 nodes"
                          (length (car cycles)) 3)))
  
  ;; Test: Graph with cycle but also non-cyclic parts
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'main 'loop)
       (call-graph-add-edge! g 'loop 'loop)  ; self-recursion
       (call-graph-add-edge! g 'loop 'helper)
       (let ([cycles (find-call-cycles g)])
            (assert-equal "finds cycle even with extra edges"
                          (length cycles) 1)))
  
  ;; Test: Depth handles cycles gracefully
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'a 'b)
       (call-graph-add-edge! g 'b 'a)
       ;; Should not infinite loop
       (let ([depth (call-graph-depth g 'a)])
            (assert-true "depth terminates with cycles"
                         (>= depth 0)))))

;;; ====
;;; Critical Path Tests
;;; ====

(define (test-critical-path)
  (printf "\n=== Critical Path Tests ===\n")
  
  ;; Build graph with costs:
  ;;   main(10) -> heavy(50), light(5)
  ;;   heavy(50) -> util(20)
  (let ([g (make-call-graph)]
        [costs (make-hashtable symbol-hash eq?)])
       (call-graph-add-edge! g 'main 'heavy)
       (call-graph-add-edge! g 'main 'light)
       (call-graph-add-edge! g 'heavy 'util)
       (hashtable-set! costs 'main 10)
       (hashtable-set! costs 'heavy 50)
       (hashtable-set! costs 'light 5)
       (hashtable-set! costs 'util 20)
       
       (let* ([cost-fn (make-cost-fn costs)]
              [path (find-critical-path g cost-fn)])
             (assert-equal "critical path starts at main"
                           (car path) 'main)
             (assert-true "critical path includes heavy"
                          (memq 'heavy path))
             (assert-true "critical path includes util"
                          (memq 'util path))
             (assert-false "critical path excludes light"
                           (memq 'light path))
             (assert-equal "critical path has 3 nodes"
                           (length path) 3)
             (assert-equal "critical path total cost is 80"
                           (path-total-cost path cost-fn) 80)))
  
  ;; Test: Empty graph
  (let ([g (make-call-graph)])
       (assert-equal "empty graph has empty critical path"
                     (find-critical-path g (lambda (x) 0))
                     '()))
  
  ;; Test: Single node
  (let ([g (make-call-graph)]
        [costs (make-hashtable symbol-hash eq?)])
       ;; Add a node with no edges (just put it in reverse index)
       (hashtable-set! (call-graph-reverse g) 'lone '())
       (hashtable-set! costs 'lone 100)
       (let* ([cost-fn (make-cost-fn costs)]
              [path (find-critical-path g cost-fn)])
             (assert-equal "single node path"
                           path '(lone)))))

;;; ====
;;; ASCII Visualization Tests
;;; ====

(define (test-ascii-visualization)
  (printf "\n=== ASCII Visualization Tests ===\n")
  
  ;; Test: Render empty graph
  (let* ([g (make-call-graph)]
         [ascii (call-graph->ascii g)])
        (assert-true "empty graph renders"
                     (string? ascii))
        (assert-true "contains CALL GRAPH header"
                     (string-contains ascii "CALL GRAPH")))
  
  ;; Test: Render simple graph
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'main 'helper)
       (let ([ascii (call-graph->ascii g)])
            (assert-true "renders main"
                         (string-contains ascii "main"))
            (assert-true "renders helper"
                         (string-contains ascii "helper"))
            (assert-true "shows node count"
                         (string-contains ascii "Nodes: 2"))))
  
  ;; Test: Summary output
  (let ([g (make-call-graph)])
       (call-graph-add-edge! g 'a 'b)
       (call-graph-add-edge! g 'b 'c)
       (let ([summary (call-graph-summary g)])
            (assert-true "summary is string"
                         (string? summary))
            (assert-true "shows total nodes"
                         (string-contains summary "Total nodes")))))

;;; Helper: Check if string contains substring
(define (string-contains str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string=? (substring str i (+ i sub-len)) substr) #t]
             [else (loop (+ i 1))]))))

;;; ====
;;; Profiler Integration Tests
;;; ====

(define (test-profiler-integration)
  (printf "\n=== Profiler Integration Tests ===\n")
  
  ;; Test: Build graph from mock profiler
  (let* ([leaf1 (mock-node 'leaf1 30 20 '())]
         [leaf2 (mock-node 'leaf2 25 15 '())]
         [mid (mock-node 'mid 60 30 (list leaf1 leaf2))]
         [root (mock-node 'root 100 0 (list mid))]
         [mock-profiler `(profiler
                          (expr . test)
                          (env . ())
                          (initial-fuel . 100)
                          (root . ,root)
                          (status . complete)
                          (result . #t))]
         [g (build-call-graph mock-profiler)])
        (assert-true "builds graph from profiler"
                     (call-graph? g))
        (assert-equal "root calls mid"
                      (call-graph-callees g 'root)
                      '((mid . 1)))
        (let ([mid-callees (call-graph-callees g 'mid)])
             (assert-equal "mid has 2 callees"
                           (length mid-callees) 2)))
  
  ;; Test: call-graph-with-costs
  (let* ([child (mock-node 'child 50 30 '())]  ; 20 fuel
         [root (mock-node 'root 100 50 (list child))]  ; 50 fuel
         [mock-profiler `(profiler
                          (expr . test)
                          (env . ())
                          (initial-fuel . 100)
                          (root . ,root)
                          (status . complete)
                          (result . #t))]
         [result (call-graph-with-costs mock-profiler)]
         [graph (car result)]
         [costs (cdr result)])
        (assert-true "returns graph"
                     (call-graph? graph))
        (assert-equal "root cost is 50"
                      (hashtable-ref costs 'root 0) 50)
        (assert-equal "child cost is 20"
                      (hashtable-ref costs 'child 0) 20)))

;;; ====
;;; Run All Tests
;;; ====

(run-tests)
