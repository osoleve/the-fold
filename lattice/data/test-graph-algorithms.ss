;;; test-graph-algorithms.ss — Tests for traversal, pathfinding, and analysis

(source-directories (cons "core" (source-directories)))
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "boundary/io/fs.ss")
(load "boundary/storage/store-api.ss")
(load "lattice/data/graph-algorithms.ss")
(load "boundary/blocks/graph-traversal.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a
" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a
" name)
       (printf "      Expected: ~s
" expected)
       (printf "      Got:      ~s
" actual))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (list-unique xs)
  (let loop ([rest xs]
             [seen '()])
       (cond
        [(null? rest) (reverse seen)]
        [(memq (car rest) seen) (loop (cdr rest) seen)]
        [else (loop (cdr rest) (cons (car rest) seen))])))

(define (same-set? a b)
  (let ([ua (list-unique a)]
        [ub (list-unique b)])
       (and (= (length ua) (length ub))
            (let loop ([xs ua])
                 (cond
                  [(null? xs) #t]
                  [(memq (car xs) ub) (loop (cdr xs))]
                  [else #f])))))

(define (collect-visit-tags traverse fs start-hash)
  (let ([tags '()])
       (traverse fs start-hash
                 (lambda (_h b)
                         (set! tags (append tags (list (block-tag b))))))
       tags))

(define (path->tags fs path)
  (map (lambda (h)
               (let ([b (store-get fs h)])
                    (if b (block-tag b) 'missing)))
       path))

(define (paths->tag-paths fs paths)
  (map (lambda (p) (path->tags fs p)) paths))

(define (same-path-set? a b)
  (and (= (length a) (length b))
       (let loop ([xs a])
            (cond
             [(null? xs) #t]
             [(member (car xs) b) (loop (cdr xs))]
             [else #f]))))

(define (any? pred xs)
  (cond
   [(null? xs) #f]
   [(pred (car xs)) #t]
   [else (any? pred (cdr xs))]))

(define (component->tags fs component)
  (list-unique
   (map (lambda (h)
                (let ([b (store-get fs h)])
                     (if b (block-tag b) 'missing)))
        component)))

(define (components->tag-sets fs components)
  (map (lambda (c) (component->tags fs c)) components))

(define (same-component-set? actual expected)
  (and (= (length actual) (length expected))
       (let loop ([es expected])
            (cond
             [(null? es) #t]
             [(any? (lambda (a) (same-set? a (car es))) actual)
              (loop (cdr es))]
             [else #f]))))

(define (hash-index-of xs target)
  (let loop ([rest xs] [i 0])
       (cond
        [(null? rest) #f]
        [(bytevector=? (car rest) target) i]
        [else (loop (cdr rest) (+ i 1))])))

(define (edge-order-ok? order edge)
  (let ([from-idx (hash-index-of order (car edge))]
        [to-idx (hash-index-of order (cdr edge))])
       (and from-idx to-idx (< from-idx to-idx))))

(define (all-edges-ordered? order edges)
  (let loop ([es edges])
       (cond
        [(null? es) #t]
        [(edge-order-ok? order (car es)) (loop (cdr es))]
        [else #f])))

(define (topo-order-valid? order edges total)
  (and (list? order)
       (= (length order) total)
       (all-edges-ordered? order edges)))

(printf "
")
(printf "====
")
(printf "              GRAPH ALGORITHM TESTS
")
(printf "====

")

;;; ====
;;; Test Setup
;;; ====

(define test-store-path ".test-graph-algorithms")

(when (file-exists? test-store-path)
      (system (format "rm -rf ~a" test-store-path)))

(define fs (mint-fs-capability test-store-path))

;; Build a DAG with shared children to test duplicate visits.
;; A -> B, C
;; B -> D
;; C -> D, E
;; D -> E
;; E -> (none)
(define block-e (make-block 'node-e (string->utf8 "E") (vector)))
(define hash-e (hash-block block-e))

(define block-d (make-block 'node-d (string->utf8 "D") (vector hash-e)))
(define hash-d (hash-block block-d))

(define block-b (make-block 'node-b (string->utf8 "B") (vector hash-d)))
(define hash-b (hash-block block-b))

(define block-c (make-block 'node-c (string->utf8 "C") (vector hash-d hash-e)))
(define hash-c (hash-block block-c))

(define block-a (make-block 'node-a (string->utf8 "A") (vector hash-b hash-c)))
(define hash-a (hash-block block-a))

(define block-g (make-block 'node-g (string->utf8 "G") (vector)))
(define hash-g (hash-block block-g))

(define block-f (make-block 'node-f (string->utf8 "F") (vector hash-g)))
(define hash-f (hash-block block-f))

(define block-h (make-block 'node-h (string->utf8 "H") (vector)))
(define hash-h (hash-block block-h))

(for-each (lambda (b) (store-put! fs b))
          (list block-a block-b block-c block-d block-e block-f block-g block-h))

(printf "Test blocks created and stored: ~a blocks

" (store-count fs))

;;; ====
;;; BFS Traversal
;;; ====

(printf "BFS traversal:
")
(printf "----
")

(define bfs-tags (collect-visit-tags bfs-traverse fs hash-a))
(test "bfs-traverse order"
      '(node-a node-b node-c node-d node-e)
      bfs-tags)
(test "bfs-traverse visits each node once"
      (length (list-unique bfs-tags))
      (length bfs-tags))

(define missing-block (make-block 'missing (string->utf8 "missing") (vector)))
(define missing-hash (hash-block missing-block))
(define bfs-missing (collect-visit-tags bfs-traverse fs missing-hash))
(test "bfs-traverse skips missing start hash"
      '()
      bfs-missing)

(printf "
")

;;; ====
;;; DFS Traversal
;;; ====

(printf "DFS traversal:
")
(printf "----
")

(define dfs-tags (collect-visit-tags dfs-traverse fs hash-a))
(test "dfs-traverse order"
      '(node-a node-b node-d node-e node-c)
      dfs-tags)
(test "dfs-traverse visits each node once"
      (length (list-unique dfs-tags))
      (length dfs-tags))

(printf "
")

;;; ====
;;; Reverse BFS Traversal
;;; ====

(printf "Reverse BFS traversal:
")
(printf "----
")

(define reverse-tags (collect-visit-tags bfs-traverse-reverse fs hash-d))
(test-true "bfs-traverse-reverse reaches upstream nodes"
           (same-set? reverse-tags '(node-a node-b node-c node-d)))
(test-false "bfs-traverse-reverse does not include unrelated nodes"
            (memq 'node-e reverse-tags))

(printf "
")

;;; ====
;;; Pathfinding
;;; ====

(printf "Pathfinding:
")
(printf "----
")

(define shortest-tags (path->tags fs (shortest-path fs hash-a hash-e)))
(test "shortest-path selects shortest route"
      '(node-a node-c node-e)
      shortest-tags)
(test "shortest-path same node"
      '(node-a)
      (path->tags fs (shortest-path fs hash-a hash-a)))
(test "shortest-path unreachable"
      #f
      (shortest-path fs hash-e hash-a))

(define all-depth2 (paths->tag-paths fs (all-paths fs hash-a hash-e 2)))
(test-true "all-paths depth 2 returns only the direct shortest path"
           (same-path-set? all-depth2
                           '((node-a node-c node-e))))

(define all-depth3 (paths->tag-paths fs (all-paths fs hash-a hash-e 3)))
(test-true "all-paths depth 3 returns all routes"
           (same-path-set? all-depth3
                           '((node-a node-c node-e)
                             (node-a node-c node-d node-e)
                             (node-a node-b node-d node-e))))
(test-true "all-paths same node"
           (same-path-set? (paths->tag-paths fs (all-paths fs hash-a hash-a 0))
                           '((node-a))))

(test-true "path-exists? returns true when reachable"
           (path-exists? fs hash-a hash-e))
(test-false "path-exists? returns false when unreachable"
            (path-exists? fs hash-e hash-a))
(test-true "path-exists? same node"
           (path-exists? fs hash-a hash-a))
(test-false "path-exists? missing start"
            (path-exists? fs missing-hash hash-a))

(printf "
")

;;; ====
;;; Graph Analysis
;;; ====

(printf "Graph analysis:
")
(printf "----
")

(define component-tags (components->tag-sets fs (connected-components fs)))
(test-true "connected-components splits into expected sets"
           (same-component-set? component-tags
                                '((node-a node-b node-c node-d node-e)
                                  (node-f node-g)
                                  (node-h))))

(test-true "find-cycles reports none for DAGs"
           (null? (find-cycles fs)))

(define topo-order (topological-sort fs))
(define topo-edges
  (list (cons hash-a hash-b)
        (cons hash-a hash-c)
        (cons hash-b hash-d)
        (cons hash-c hash-d)
        (cons hash-c hash-e)
        (cons hash-d hash-e)
        (cons hash-f hash-g)))

(test-true "topological-sort returns a valid ordering"
           (topo-order-valid? topo-order topo-edges (store-count fs)))

(printf "
")

;;; ====
;;; Centrality and Importance Metrics
;;; ====

(printf "Centrality metrics:
")
(printf "----
")

;; in-degree: number of blocks referencing this block
;; D is referenced by B and C
(test "in-degree of D" 2 (in-degree fs hash-d))
;; E is referenced by C and D
(test "in-degree of E" 2 (in-degree fs hash-e))
;; A is not referenced by anyone (root)
(test "in-degree of A" 0 (in-degree fs hash-a))
;; H is isolated (no references in or out)
(test "in-degree of H" 0 (in-degree fs hash-h))

;; out-degree: number of refs from this block
;; A references B and C
(test "out-degree of A" 2 (out-degree fs hash-a))
;; C references D and E
(test "out-degree of C" 2 (out-degree fs hash-c))
;; E references nothing (leaf)
(test "out-degree of E" 0 (out-degree fs hash-e))
;; H is isolated
(test "out-degree of H" 0 (out-degree fs hash-h))

;; total-degree
;; D refs E (1 out) and is referenced by B,C (2 in) = 3 total
(test "total-degree of D (in:2 + out:1)" 3 (total-degree fs hash-d))
(test "total-degree of A (in:0 + out:2)" 2 (total-degree fs hash-a))

;; find-roots: blocks with no incoming references
(define root-hashes (find-roots fs))
(define root-tags (map (lambda (h) (block-tag (store-get fs h))) root-hashes))
(test-true "find-roots includes A (no incoming)"
           (if (memq 'node-a root-tags) #t #f))
(test-true "find-roots includes F (no incoming)"
           (if (memq 'node-f root-tags) #t #f))
(test-true "find-roots includes H (isolated)"
           (if (memq 'node-h root-tags) #t #f))
(test-false "find-roots excludes D (has incoming)"
            (if (memq 'node-d root-tags) #t #f))

;; find-leaves: blocks with no outgoing references
(define leaf-hashes (find-leaves fs))
(define leaf-tags (map (lambda (h) (block-tag (store-get fs h))) leaf-hashes))
(test-true "find-leaves includes E (no outgoing)"
           (if (memq 'node-e leaf-tags) #t #f))
(test-true "find-leaves includes G (no outgoing)"
           (if (memq 'node-g leaf-tags) #t #f))
(test-true "find-leaves includes H (isolated)"
           (if (memq 'node-h leaf-tags) #t #f))
(test-false "find-leaves excludes A (has outgoing)"
            (if (memq 'node-a leaf-tags) #t #f))

;; find-hubs: top n by total degree
(define hubs (find-hubs fs 3))
(test "find-hubs returns 3 results" 3 (length hubs))
(test-true "find-hubs results are pairs"
           (and (pair? (car hubs))
                (bytevector? (caar hubs))
                (number? (cdar hubs))))
;; Degrees sorted descending
(test-true "find-hubs sorted descending"
           (let ([degrees (map cdr hubs)])
                (>= (car degrees) (cadr degrees))))

(printf "
")

;;; ====
;;; Subgraph Operations
;;; ====

(printf "Subgraph operations:
")
(printf "----
")

;; reachable-from: all blocks reachable following outgoing refs
(define reachable-from-a (reachable-from fs hash-a))
(define reachable-tags-a (map (lambda (h) (block-tag (store-get fs h))) reachable-from-a))
(test-true "reachable-from A includes A"
           (if (memq 'node-a reachable-tags-a) #t #f))
(test-true "reachable-from A includes E"
           (if (memq 'node-e reachable-tags-a) #t #f))
(test "reachable-from A count" 5 (length reachable-from-a))
(test-false "reachable-from A excludes F"
            (if (memq 'node-f reachable-tags-a) #t #f))

;; reachable-from leaf (just itself)
(define reachable-from-e (reachable-from fs hash-e))
(test "reachable-from E (leaf)" 1 (length reachable-from-e))

;; ancestors-of: all blocks that can reach this one (following incoming)
(define ancestors-e (ancestors-of fs hash-e))
(define ancestor-tags-e (map (lambda (h) (block-tag (store-get fs h))) ancestors-e))
(test-true "ancestors-of E includes E"
           (if (memq 'node-e ancestor-tags-e) #t #f))
(test-true "ancestors-of E includes A"
           (if (memq 'node-a ancestor-tags-e) #t #f))
(test-true "ancestors-of E includes C"
           (if (memq 'node-c ancestor-tags-e) #t #f))
(test-true "ancestors-of E includes D"
           (if (memq 'node-d ancestor-tags-e) #t #f))
(test-false "ancestors-of E excludes F"
            (if (memq 'node-f ancestor-tags-e) #t #f))

;; subgraph: extract adjacency list for subset of nodes
(define sub-hashes (list hash-a hash-b hash-c))
(define sub (subgraph fs sub-hashes))
(test "subgraph returns 3 entries" 3 (length sub))
;; Check that A's neighbors are filtered to only include B and C (both in subset)
(define a-entry (assoc hash-a sub))
(test "subgraph A has 2 neighbors in subset" 2 (length (cdr a-entry)))
;; B points to D which is not in subset, so B has 0 neighbors
(define b-entry (assoc hash-b sub))
(test "subgraph B has 0 neighbors (D not in subset)" 0 (length (cdr b-entry)))

;; neighborhood: all nodes within n hops (both directions)
(define neighbors-d-1 (neighborhood fs hash-d 1))
(define neighbor-tags-d-1 (map (lambda (h) (block-tag (store-get fs h))) neighbors-d-1))
(test-true "neighborhood D radius 1 includes D"
           (if (memq 'node-d neighbor-tags-d-1) #t #f))
(test-true "neighborhood D radius 1 includes B (incoming)"
           (if (memq 'node-b neighbor-tags-d-1) #t #f))
(test-true "neighborhood D radius 1 includes C (incoming)"
           (if (memq 'node-c neighbor-tags-d-1) #t #f))
(test-true "neighborhood D radius 1 includes E (outgoing)"
           (if (memq 'node-e neighbor-tags-d-1) #t #f))
(test-false "neighborhood D radius 1 excludes A (2 hops away)"
            (if (memq 'node-a neighbor-tags-d-1) #t #f))

(define neighbors-d-2 (neighborhood fs hash-d 2))
(define neighbor-tags-d-2 (map (lambda (h) (block-tag (store-get fs h))) neighbors-d-2))
(test-true "neighborhood D radius 2 includes A (2 hops)"
           (if (memq 'node-a neighbor-tags-d-2) #t #f))

;; induced-subgraph-blocks
(define induced (induced-subgraph-blocks fs (list hash-a hash-b hash-c)))
(test "induced-subgraph-blocks count" 3 (length induced))
(test-true "induced-subgraph-blocks are blocks"
           (and (block? (car induced))
                (block? (cadr induced))))

(printf "
")

;;; ====
;;; Graph Stats
;;; ====

(printf "Graph stats:
")
(printf "----
")

(define stats (graph-stats fs))
(test "graph-stats nodes" 8 (cdr (assq 'nodes stats)))
(test "graph-stats roots" 3 (cdr (assq 'roots stats)))
(test "graph-stats leaves" 3 (cdr (assq 'leaves stats)))
(test "graph-stats components" 3 (cdr (assq 'components stats)))
(test-false "graph-stats cyclic" (cdr (assq 'cyclic stats)))

(printf "
")

;;; ====
;;; Cleanup
;;; ====

(printf "Cleaning up test store...
")
(system (format "rm -rf ~a" test-store-path))
(printf "Cleanup complete.

")

;;; ====
;;; Summary
;;; ====

(printf "====
")
(printf "                    TEST RESULTS
")
(printf "====

")

(printf "Tests passed: ~a
" tests-passed)
(printf "Tests failed: ~a
" tests-failed)
(printf "Total tests:  ~a
" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "
[SUCCESS] All graph algorithm tests passed.

")
    (printf "
[FAILURE] Some graph algorithm tests failed.

"))
