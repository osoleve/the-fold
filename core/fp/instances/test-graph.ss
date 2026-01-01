;;; fabric/stitches/fp/test-graph.ss — Tests for Graph Library

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/instances/graph.ss")

(display "
")
(display "==============================================================
")
(display "         GRAPH LIBRARY TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Basic Graph Construction Tests
;;; ============================================================

(test-group graph-construction
            (define-test empty-graph-test
              (assert-true (graph? empty-graph))
              (assert-true (graph-directed? empty-graph))
              (assert-equal 0 (graph-vertex-count empty-graph))
              (assert-equal 0 (graph-edge-count empty-graph)))
            
            (define-test empty-ugraph-test
              (assert-true (graph? empty-ugraph))
              (assert-false (graph-directed? empty-ugraph)))
            
            (define-test add-vertex-test
              (let* ([g (graph-add-vertex empty-graph 'a)]
                     [g (graph-add-vertex g 'b)])
                    (assert-equal 2 (graph-vertex-count g))
                    (assert-true (graph-has-vertex? g 'a))
                    (assert-true (graph-has-vertex? g 'b))
                    (assert-false (graph-has-vertex? g 'c))))
            
            (define-test add-vertex-idempotent-test
              (let* ([g (graph-add-vertex empty-graph 'a)]
                     [g (graph-add-vertex g 'a)])
                    (assert-equal 1 (graph-vertex-count g))))
            
            (define-test add-edge-test
              (let ([g (graph-add-edge* empty-graph 'a 'b)])
                   (assert-true (graph-has-vertex? g 'a))
                   (assert-true (graph-has-vertex? g 'b))
                   (assert-true (graph-has-edge? g 'a 'b))
                   (assert-false (graph-has-edge? g 'b 'a))))
            
            (define-test add-edge-undirected-test
              (let ([g (graph-add-edge* empty-ugraph 'a 'b)])
                   (assert-true (graph-has-edge? g 'a 'b))
                   (assert-true (graph-has-edge? g 'b 'a))))
            
            (define-test add-weighted-edge-test
              (let ([g (graph-add-edge empty-graph 'a 'b 5)])
                   (assert-true (just? (graph-edge-weight g 'a 'b)))
                   (assert-equal 5 (from-just (graph-edge-weight g 'a 'b)))))
            
            (define-test remove-vertex-test
              (let* ([g (graph-add-edge* (graph-add-edge* empty-graph 'a 'b) 'b 'c)]
                     [g (graph-remove-vertex g 'b)])
                    (assert-true (graph-has-vertex? g 'a))
                    (assert-false (graph-has-vertex? g 'b))
                    (assert-true (graph-has-vertex? g 'c))
                    (assert-false (graph-has-edge? g 'a 'b))))
            
            (define-test remove-edge-test
              (let* ([g (graph-add-edge* empty-graph 'a 'b)]
                     [g (graph-remove-edge g 'a 'b)])
                    (assert-false (graph-has-edge? g 'a 'b)))))

;;; ============================================================
;;; Graph Query Tests
;;; ============================================================

(test-group graph-queries
            (define-test neighbors-test
              (let* ([g (graph-add-edge* (graph-add-edge* empty-graph 'a 'b) 'a 'c)]
                     [ns (graph-neighbors g 'a)])
                    (assert-equal 2 (length ns))
                    (assert-true (not (not (member 'b ns))))
                    (assert-true (not (not (member 'c ns))))))
            
            (define-test out-degree-test
              (let ([g (graph-add-edges empty-graph '((a b) (a c) (a d)))])
                   (assert-equal 3 (graph-out-degree g 'a))
                   (assert-equal 0 (graph-out-degree g 'b))))
            
            (define-test in-degree-test
              (let ([g (graph-add-edges empty-graph '((a c) (b c)))])
                   (assert-equal 2 (graph-in-degree g 'c))
                   (assert-equal 0 (graph-in-degree g 'a))))
            
            (define-test edge-count-directed-test
              (let ([g (graph-add-edges empty-graph '((a b) (b c) (c a)))])
                   (assert-equal 3 (graph-edge-count g))))
            
            (define-test edge-count-undirected-test
              (let ([g (graph-add-edges empty-ugraph '((a b) (b c)))])
                   (assert-equal 2 (graph-edge-count g)))))

;;; ============================================================
;;; DFS Tests
;;; ============================================================

(test-group dfs-traversal
            (define-test dfs-simple-test
              (let* ([g (graph-add-edges empty-graph '((a b) (b c) (c d)))]
                     [result (graph-dfs g 'a)])
                    (assert-equal 4 (length result))
                    (assert-equal 'a (car result))))
            
            (define-test dfs-branching-test
              (let* ([g (graph-add-edges empty-graph '((a b) (a c) (b d) (c e)))]
                     [result (graph-dfs g 'a)])
                    (assert-equal 5 (length result))
                    (assert-equal 'a (car result))))
            
            (define-test dfs-cycle-test
              (let* ([g (graph-add-edges empty-graph '((a b) (b c) (c a)))]
                     [result (graph-dfs g 'a)])
                    (assert-equal 3 (length result))))
            
            (define-test dfs-full-disconnected-test
              (let* ([g (graph-add-vertices empty-graph '(a b c d))]
                     [g (graph-add-edges g '((a b) (c d)))]
                     [result (graph-dfs-full g)])
                    (assert-equal 4 (length result)))))

;;; ============================================================
;;; BFS Tests
;;; ============================================================

(test-group bfs-traversal
            (define-test bfs-simple-test
              (let* ([g (graph-add-edges empty-graph '((a b) (b c) (c d)))]
                     [result (graph-bfs g 'a)])
                    (assert-equal 4 (length result))
                    (assert-equal 'a (car result))))
            
            (define-test bfs-levels-test
              (let* ([g (graph-add-edges empty-graph '((a b) (a c) (b d) (c d)))]
                     [levels (graph-bfs-levels g 'a)])
                    (assert-equal 3 (length levels))
                    (assert-equal '(a) (car levels))
                    (assert-equal 2 (length (cadr levels)))))  ; b and c at level 1
            
            (define-test bfs-shortest-path-test
              (let* ([g (graph-add-edges empty-graph '((a b) (a c) (b d) (c d) (d e)))]
                     [path (graph-shortest-path g 'a 'e)])
                    (assert-true (just? path))
                    (assert-equal 4 (length (from-just path)))
                    (assert-equal 'a (car (from-just path)))
                    (assert-equal 'e (car (reverse (from-just path)))))))

;;; ============================================================
;;; Path Finding Tests
;;; ============================================================

(test-group path-finding
            (define-test path-exists-test
              (let ([g (graph-add-edges empty-graph '((a b) (b c) (c d)))])
                   (assert-true (graph-path? g 'a 'd))
                   (assert-false (graph-path? g 'd 'a))))  ; Directed, no reverse path
            
            (define-test path-self-test
              (let ([g (graph-add-vertex empty-graph 'a)])
                   (let ([path (graph-shortest-path g 'a 'a)])
                        (assert-true (just? path))
                        (assert-equal '(a) (from-just path)))))
            
            (define-test path-not-found-test
              (let* ([g (graph-add-vertices empty-graph '(a b))]
                     [path (graph-shortest-path g 'a 'b)])
                    (assert-true (nothing? path))))
            
            (define-test all-paths-test
              (let* ([g (graph-add-edges empty-graph '((a b) (a c) (b d) (c d)))]
                     [paths (graph-all-paths g 'a 'd 10)])
                    (assert-equal 2 (length paths)))))

;;; ============================================================
;;; Topological Sort Tests
;;; ============================================================

;;; Helper for topo tests
(define (list-index lst elem)
  (let loop ([l lst] [i 0])
       (if (null? l)
           -1
           (if (equal? (car l) elem)
               i
               (loop (cdr l) (+ i 1))))))

(test-group topological-sort
            (define-test topo-simple-test
              (let* ([g (graph-add-edges empty-graph '((a b) (b c) (c d)))]
                     [result (graph-topological-sort g)])
                    (assert-true (just? result))
                    (assert-equal 4 (length (from-just result)))))
            
            (define-test topo-dag-test
              (let* ([g (graph-add-edges empty-graph '((a b) (a c) (b d) (c d)))]
                     [result (graph-topological-sort g)])
                    (assert-true (just? result))
                    (let ([order (from-just result)])
                         ;; a must come before b and c
                         (assert-true (< (list-index order 'a) (list-index order 'b)))
                         (assert-true (< (list-index order 'a) (list-index order 'c)))
                         ;; b and c must come before d
                         (assert-true (< (list-index order 'b) (list-index order 'd)))
                         (assert-true (< (list-index order 'c) (list-index order 'd))))))
            
            (define-test topo-cycle-fails-test
              (let* ([g (graph-add-edges empty-graph '((a b) (b c) (c a)))]
                     [result (graph-topological-sort g)])
                    (assert-true (nothing? result))))
            
            (define-test topo-undirected-fails-test
              (let* ([g (graph-add-edge* empty-ugraph 'a 'b)]
                     [result (graph-topological-sort g)])
                    (assert-true (nothing? result)))))

;;; ============================================================
;;; Cycle Detection Tests
;;; ============================================================

(test-group cycle-detection
            (define-test no-cycle-directed-test
              (let ([g (graph-add-edges empty-graph '((a b) (b c) (c d)))])
                   (assert-false (graph-has-cycle? g))))
            
            (define-test cycle-directed-test
              (let ([g (graph-add-edges empty-graph '((a b) (b c) (c a)))])
                   (assert-true (graph-has-cycle? g))))
            
            (define-test self-loop-test
              (let ([g (graph-add-edge* empty-graph 'a 'a)])
                   (assert-true (graph-has-cycle? g))))
            
            (define-test no-cycle-undirected-test
              ;; Tree structure - no cycle
              (let ([g (graph-add-edges empty-ugraph '((a b) (a c) (b d)))])
                   (assert-false (graph-has-cycle? g))))
            
            (define-test cycle-undirected-test
              ;; Triangle - has cycle
              (let ([g (graph-add-edges empty-ugraph '((a b) (b c) (c a)))])
                   (assert-true (graph-has-cycle? g)))))

;;; ============================================================
;;; Connected Components Tests
;;; ============================================================

(test-group connected-components
            (define-test single-component-test
              (let* ([g (graph-add-edges empty-ugraph '((a b) (b c) (c d)))]
                     [components (graph-connected-components g)])
                    (assert-equal 1 (length components))))
            
            (define-test multiple-components-test
              (let* ([g (graph-add-vertices empty-ugraph '(a b c d e f))]
                     [g (graph-add-edges g '((a b) (c d) (e f)))]
                     [components (graph-connected-components g)])
                    (assert-equal 3 (length components))))
            
            (define-test isolated-vertex-component-test
              (let* ([g (graph-add-vertices empty-ugraph '(a b c))]
                     [g (graph-add-edge* g 'a 'b)]
                     [components (graph-connected-components g)])
                    (assert-equal 2 (length components)))))

;;; ============================================================
;;; Graph Transformation Tests
;;; ============================================================

(test-group graph-transformations
            (define-test reverse-graph-test
              (let* ([g (graph-add-edges empty-graph '((a b) (b c)))]
                     [r (graph-reverse g)])
                    (assert-true (graph-has-edge? r 'b 'a))
                    (assert-true (graph-has-edge? r 'c 'b))
                    (assert-false (graph-has-edge? r 'a 'b))))
            
            (define-test subgraph-test
              (let* ([g (graph-add-edges empty-graph '((a b) (b c) (c d)))]
                     [sub (graph-subgraph g '(a b c))])
                    (assert-equal 3 (graph-vertex-count sub))
                    (assert-true (graph-has-edge? sub 'a 'b))
                    (assert-true (graph-has-edge? sub 'b 'c))
                    (assert-false (graph-has-vertex? sub 'd))))
            
            (define-test complement-test
              (let* ([g (graph-add-vertices empty-graph '(a b c))]
                     [g (graph-add-edge* g 'a 'b)]
                     [comp (graph-complement g)])
                    (assert-false (graph-has-edge? comp 'a 'b))
                    (assert-true (graph-has-edge? comp 'a 'c))
                    (assert-true (graph-has-edge? comp 'b 'a))
                    (assert-true (graph-has-edge? comp 'b 'c))))
            
            (define-test map-vertices-test
              (let* ([g (graph-add-edge* empty-graph 1 2)]
                     [mapped (graph-map-vertices (lambda (v) (* v 10)) g)])
                    (assert-true (graph-has-vertex? mapped 10))
                    (assert-true (graph-has-vertex? mapped 20))
                    (assert-true (graph-has-edge? mapped 10 20)))))

;;; ============================================================
;;; Graph Builders Tests
;;; ============================================================

(test-group graph-builders
            (define-test edges-to-graph-test
              (let ([g (edges->graph '((a b) (b c) (c d)))])
                   (assert-true (graph-directed? g))
                   (assert-equal 4 (graph-vertex-count g))
                   (assert-equal 3 (graph-edge-count g))))
            
            (define-test edges-to-ugraph-test
              (let ([g (edges->ugraph '((a b) (b c)))])
                   (assert-false (graph-directed? g))
                   (assert-equal 3 (graph-vertex-count g))
                   (assert-equal 2 (graph-edge-count g))))
            
            (define-test weighted-edges-test
              (let ([g (edges->graph '((a b 5) (b c 3)))])
                   (assert-equal 5 (from-just (graph-edge-weight g 'a 'b)))
                   (assert-equal 3 (from-just (graph-edge-weight g 'b 'c))))))

;;; ============================================================
;;; Fold Tests
;;; ============================================================

(test-group graph-folds
            (define-test fold-vertices-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3 4 5))]
                     [sum (graph-fold-vertices + 0 g)])
                    (assert-equal 15 sum)))
            
            (define-test fold-edges-test
              (let* ([g (graph-add-edges empty-graph '((a b 1) (b c 2) (c d 3)))]
                     [weight-sum (graph-fold-edges (lambda (acc from to w) (+ acc w)) 0 g)])
                    (assert-equal 6 weight-sum))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test empty-graph-operations-test
              ;; DFS/BFS return the start vertex even if not in graph (visited once)
              ;; Neighbors correctly returns empty for nonexistent vertex
              (assert-equal '() (graph-neighbors empty-graph 'nonexistent))
              (assert-equal 0 (graph-out-degree empty-graph 'nonexistent)))
            
            (define-test single-vertex-test
              (let ([g (graph-add-vertex empty-graph 'alone)])
                   (assert-equal '(alone) (graph-dfs g 'alone))
                   (assert-equal 0 (graph-out-degree g 'alone))
                   (assert-false (graph-has-cycle? g))))
            
            (define-test graph-to-string-test
              (let* ([g (graph-add-edge* empty-graph 'a 'b)]
                     [s (graph->string g)])
                    (assert-true (string? s))
                    (assert-true (> (string-length s) 0)))))

;;; ============================================================
;;; Foldable Instance Tests
;;; ============================================================

(test-group graph-foldable
            (define-test graph-foldr-sum-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3 4 5))]
                     [sum (graph-foldr + 0 g)])
                    (assert-equal 15 sum)))
            
            (define-test graph-foldl-sum-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3 4 5))]
                     [sum (graph-foldl + 0 g)])
                    (assert-equal 15 sum)))
            
            (define-test graph-fold-map-list-test
              (let* ([g (graph-add-vertices empty-graph '(a b c))]
                     [result (graph-fold-map list-monoid list g)])
                    (assert-equal 3 (length result))
                    (assert-true (not (not (member 'a result))))
                    (assert-true (not (not (member 'b result))))
                    (assert-true (not (not (member 'c result))))))
            
            (define-test graph-fold-map-sum-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3 4 5))]
                     [result (graph-fold-map sum-monoid identity g)])
                    (assert-equal 15 result)))
            
            (define-test foldable-to-list-test
              (let* ([g (graph-add-vertices empty-graph '(a b c))]
                     [lst (to-list graph-foldable g)])
                    (assert-equal 3 (length lst))
                    (assert-true (not (not (member 'a lst))))
                    (assert-true (not (not (member 'b lst))))
                    (assert-true (not (not (member 'c lst))))))
            
            (define-test foldable-fold-map-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3))]
                     [result (fold-map graph-foldable sum-monoid identity g)])
                    (assert-equal 6 result)))
            
            (define-test empty-graph-foldable-test
              (let ([result (graph-foldr cons '() empty-graph)])
                   (assert-equal '() result))))

;;; ============================================================
;;; Traversable Instance Tests
;;; ============================================================

(test-group graph-traversable
            (define-test graph-traverse-identity-test
              (let* ([g (graph-add-edges empty-graph '((a b) (b c)))]
                     [result (graph-traverse identity-applicative identity g)])
                    (assert-true (graph? result))
                    (assert-equal 3 (graph-vertex-count result))
                    (assert-true (graph-has-edge? result 'a 'b))
                    (assert-true (graph-has-edge? result 'b 'c))))
            
            (define-test graph-traverse-maybe-all-just-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3))]
                     [result (graph-traverse maybe-applicative
                                             (lambda (v) (just (* v 10)))
                                             g)])
                    (assert-true (just? result))
                    (let ([g2 (from-just result)])
                         (assert-true (graph? g2))
                         (assert-equal 3 (graph-vertex-count g2))
                         (assert-true (graph-has-vertex? g2 10))
                         (assert-true (graph-has-vertex? g2 20))
                         (assert-true (graph-has-vertex? g2 30)))))
            
            (define-test graph-traverse-maybe-with-nothing-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3))]
                     [result (graph-traverse maybe-applicative
                                             (lambda (v)
                                                     (if (= v 2)
                                                         nothing
                                                         (just (* v 10))))
                                             g)])
                    (assert-true (nothing? result))))
            
            (define-test graph-traverse-with-edges-test
              (let* ([g (graph-add-edges empty-graph '((a b) (b c) (a c)))]
                     [result (traverse graph-traversable
                                       maybe-applicative
                                       (lambda (v)
                                               (cond
                                                [(eq? v 'a) (just 'x)]
                                                [(eq? v 'b) (just 'y)]
                                                [(eq? v 'c) (just 'z)]
                                                [else nothing]))
                                       g)])
                    (assert-true (just? result))
                    (let ([g2 (from-just result)])
                         (assert-true (graph-has-edge? g2 'x 'y))
                         (assert-true (graph-has-edge? g2 'y 'z))
                         (assert-true (graph-has-edge? g2 'x 'z)))))
            
            (define-test graph-sequence-test
              (let* ([g (graph-add-vertices empty-graph
                                            (list (just 1) (just 2) (just 3)))]
                     [result (sequence graph-traversable maybe-applicative g)])
                    (assert-true (just? result))
                    (let ([g2 (from-just result)])
                         (assert-true (graph-has-vertex? g2 1))
                         (assert-true (graph-has-vertex? g2 2))
                         (assert-true (graph-has-vertex? g2 3)))))
            
            (define-test graph-traverse-either-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3))]
                     [result (traverse graph-traversable
                                       either-applicative
                                       (lambda (v)
                                               (if (> v 0)
                                                   (right (* v 10))
                                                   (left "negative")))
                                       g)])
                    (assert-true (right? result))
                    (let ([g2 (from-right result)])
                         (assert-true (graph-has-vertex? g2 10))
                         (assert-true (graph-has-vertex? g2 20))
                         (assert-true (graph-has-vertex? g2 30))))))

;;; ============================================================
;;; BFS Foldable/Traversable Tests
;;; ============================================================

(test-group graph-bfs-foldable-traversable
            (define-test bfs-foldr-test
              (let* ([g (graph-add-edges empty-graph '((a b) (a c) (b d) (c e)))]
                     [bfs-fold ((graph-bfs-foldr g 'a) cons '() g)])
                    ;; Result should be BFS order
                    (assert-equal 5 (length bfs-fold))
                    (assert-equal 'a (car bfs-fold))))
            
            (define-test bfs-fold-map-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3 4))]
                     [g (graph-add-edges g '((1 2) (1 3) (2 4)))]
                     [bfs-foldable (graph-bfs-foldable g 1)]
                     [result (fold-map bfs-foldable sum-monoid identity g)])
                    (assert-equal 10 result)))
            
            (define-test bfs-traverse-test
              (let* ([g (graph-add-edges empty-graph '((a b) (a c) (b d)))]
                     [result ((graph-bfs-traverse g 'a)
                              maybe-applicative
                              (lambda (v) (just (string->symbol
                                                 (string-append (symbol->string v) "-new"))))
                              g)])
                    (assert-true (just? result))
                    (let ([g2 (from-just result)])
                         (assert-true (graph-has-vertex? g2 'a-new))
                         (assert-true (graph-has-vertex? g2 'b-new))
                         (assert-true (graph-has-vertex? g2 'c-new))
                         (assert-true (graph-has-vertex? g2 'd-new)))))
            
            (define-test bfs-traversable-test
              (let* ([g (graph-add-edges empty-graph '((1 2) (1 3) (2 4)))]
                     [bfs-trav (graph-bfs-traversable g 1)]
                     [result (traverse bfs-trav
                                       maybe-applicative
                                       (lambda (v) (just (* v 10)))
                                       g)])
                    (assert-true (just? result))
                    (let ([g2 (from-just result)])
                         (assert-true (graph-has-vertex? g2 10))
                         (assert-true (graph-has-vertex? g2 20))
                         (assert-true (graph-has-vertex? g2 30))
                         (assert-true (graph-has-vertex? g2 40))))))

;;; ============================================================
;;; DFS Foldable/Traversable Tests
;;; ============================================================

(test-group graph-dfs-foldable-traversable
            (define-test dfs-foldr-test
              (let* ([g (graph-add-edges empty-graph '((a b) (a c) (b d) (c e)))]
                     [dfs-fold ((graph-dfs-foldr g 'a) cons '() g)])
                    ;; Result should be DFS order
                    (assert-equal 5 (length dfs-fold))
                    (assert-equal 'a (car dfs-fold))))
            
            (define-test dfs-fold-map-test
              (let* ([g (graph-add-vertices empty-graph '(1 2 3 4))]
                     [g (graph-add-edges g '((1 2) (1 3) (2 4)))]
                     [dfs-foldable (graph-dfs-foldable g 1)]
                     [result (fold-map dfs-foldable sum-monoid identity g)])
                    (assert-equal 10 result)))
            
            (define-test dfs-traverse-test
              (let* ([g (graph-add-edges empty-graph '((a b) (a c) (b d)))]
                     [result ((graph-dfs-traverse g 'a)
                              maybe-applicative
                              (lambda (v) (just (string->symbol
                                                 (string-append (symbol->string v) "-dfs"))))
                              g)])
                    (assert-true (just? result))
                    (let ([g2 (from-just result)])
                         (assert-true (graph-has-vertex? g2 'a-dfs))
                         (assert-true (graph-has-vertex? g2 'b-dfs))
                         (assert-true (graph-has-vertex? g2 'c-dfs))
                         (assert-true (graph-has-vertex? g2 'd-dfs)))))
            
            (define-test dfs-traversable-test
              (let* ([g (graph-add-edges empty-graph '((1 2) (1 3) (2 4)))]
                     [dfs-trav (graph-dfs-traversable g 1)]
                     [result (traverse dfs-trav
                                       maybe-applicative
                                       (lambda (v) (just (* v 10)))
                                       g)])
                    (assert-true (just? result))
                    (let ([g2 (from-just result)])
                         (assert-true (graph-has-vertex? g2 10))
                         (assert-true (graph-has-vertex? g2 20))
                         (assert-true (graph-has-vertex? g2 30))
                         (assert-true (graph-has-vertex? g2 40))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All graph tests passed.
")
    (display "
[FAILURE] Some graph tests failed.
"))
