;;; boundary/blocks/graph-traversal.ss — Store-dependent graph traversal & analysis
;;; @module graph-traversal
;;; @requires graph-primitives store-api

(load "core/base/prelude.ss")
(load "boundary/storage/store-api.ss")
(load "lattice/data/graph/graph-primitives.ss")

(doc 'module 'graph-traversal)
(doc 'description "Graph traversal and analysis over CAS block references.
  Provides BFS/DFS traversal, pathfinding, connected components, cycle detection,
  centrality metrics, and subgraph operations — all operating on the content-addressed store.")
(doc 'layer 'boundary)
(doc 'purity 'impure)
(doc 'note "These functions take an fs-capability as first argument for store access.
  Pure graph data structures (queue, stack, visited set, hash utilities) and
  homology-based analysis live in lattice/data/graph/graph-primitives.ss.")

;;; --- Block Reference Helpers ---

;;; get-outgoing-hashes : FSCap Hash → (List Hash)
;;; Get hashes of all blocks referenced by block at hash.
;;; Returns empty list if block not found.
(define (get-outgoing-hashes fs hash)
  (let ([block (store-get fs hash)])
       (if block
           (vector->list (block-refs block))
           '())))

;;; get-incoming-hashes : FSCap Hash → (List Hash)
;;; Get hashes of all blocks that reference the given hash.
;;; Uses store-find-by-ref and extracts hashes.
(define (get-incoming-hashes fs hash)
  (map hash-block (store-find-by-ref fs hash)))


(doc bfs-traverse 'type '(-> FSCap Hash (-> Hash Block Void) Void))
(doc bfs-traverse 'description "Breadth-first traversal starting from start-hash; calls visit-fn for each visited node")
(doc bfs-traverse 'param 'fs "File system capability")
(doc bfs-traverse 'param 'start-hash "Starting hash")
(doc bfs-traverse 'param 'visit-fn "Function called with (hash, block) for each visited node")
(define (bfs-traverse fs start-hash visit-fn)
  (let loop ([queue (queue-enqueue (make-queue) start-hash)]
             [visited (make-visited)])
       (unless (queue-empty? queue)
               (let-values ([(current rest-queue) (queue-dequeue queue)])
                           (if (visited-contains? visited current)
                               (loop rest-queue visited)
                               (let ([block (store-get fs current)])
                                    (when block
                                          (visit-fn current block)
                                          (let* ([neighbors (vector->list (block-refs block))]
                                                 [unvisited (filter (lambda (h)
                                                                            (not (visited-contains? visited h)))
                                                                    neighbors)])
                                                (loop (queue-enqueue-all rest-queue unvisited)
                                                      (visited-add visited current))))))))))

(doc dfs-traverse 'type '(-> FSCap Hash (-> Hash Block Void) Void))
(doc dfs-traverse 'description "Depth-first traversal starting from start-hash; calls visit-fn for each visited node")
(define (dfs-traverse fs start-hash visit-fn)
  (let loop ([stack (stack-push (make-stack) start-hash)]
             [visited (make-visited)])
       (unless (stack-empty? stack)
               (let-values ([(current rest-stack) (stack-pop stack)])
                           (if (visited-contains? visited current)
                               (loop rest-stack visited)
                               (let ([block (store-get fs current)])
                                    (when block
                                          (visit-fn current block)
                                          (let* ([neighbors (vector->list (block-refs block))]
                                                 [unvisited (filter (lambda (h)
                                                                            (not (visited-contains? visited h)))
                                                                    neighbors)])
                                                (loop (stack-push-all rest-stack unvisited)
                                                      (visited-add visited current))))))))))

;;; bfs-traverse-reverse : FSCap Hash (Hash Block → Void) → Void
;;; BFS following incoming edges (referrers) instead of outgoing.
(define (bfs-traverse-reverse fs start-hash visit-fn)
  (let loop ([queue (queue-enqueue (make-queue) start-hash)]
             [visited (make-visited)])
       (unless (queue-empty? queue)
               (let-values ([(current rest-queue) (queue-dequeue queue)])
                           (if (visited-contains? visited current)
                               (loop rest-queue visited)
                               (let ([block (store-get fs current)])
                                    (when block
                                          (visit-fn current block)
                                          (let* ([referrers (get-incoming-hashes fs current)]
                                                 [unvisited (filter (lambda (h)
                                                                            (not (visited-contains? visited h)))
                                                                    referrers)])
                                                (loop (queue-enqueue-all rest-queue unvisited)
                                                      (visited-add visited current))))))))))


(doc shortest-path 'type '(-> FSCap Hash Hash (Maybe (List Hash))))
(doc shortest-path 'description "Find shortest path from from-hash to to-hash using BFS; returns list of hashes or #f")
(doc shortest-path 'returns "List of hashes representing path (includes both endpoints), or #f if no path exists")
(define (shortest-path fs from-hash to-hash)
  (if (bytevector=? from-hash to-hash)
      (list from-hash)
      (let loop ([queue (queue-enqueue (make-queue) (list from-hash))]
                 [visited (make-visited)])
           (if (queue-empty? queue)
               #f
               (let-values ([(current-path rest-queue) (queue-dequeue queue)])
                           (let ([current (car current-path)])
                                (if (visited-contains? visited current)
                                    (loop rest-queue visited)
                                    (let ([neighbors (get-outgoing-hashes fs current)])
                                         (let ([found (filter (lambda (h) (bytevector=? h to-hash))
                                                              neighbors)])
                                              (if (not (null? found))
                                                  (reverse (cons to-hash current-path))
                                                  (let* ([new-visited (visited-add visited current)]
                                                         [unvisited (filter
                                                                     (lambda (h)
                                                                             (not (visited-contains? new-visited h)))
                                                                     neighbors)]
                                                         [new-paths (map (lambda (h) (cons h current-path))
                                                                         unvisited)])
                                                        (loop (queue-enqueue-all rest-queue new-paths)
                                                              new-visited))))))))))))

;;; all-paths : FSCap Hash Hash Integer → (List (List Hash))
;;; Find all paths from from-hash to to-hash up to max-depth.
;;; Returns list of paths (each path is a list of hashes).
;;; Uses DFS with depth limiting.
(define (all-paths fs from-hash to-hash max-depth)
  (let ([results '()]
        [visited (make-visited)])
       (visited-add visited from-hash)
       (let dfs ([current from-hash]
                 [path (list from-hash)]
                 [depth 0])
            (cond
             [(bytevector=? current to-hash)
              (set! results (cons (reverse path) results))]
             [(>= depth max-depth)
              (void)]
             [else
              (let ([neighbors (get-outgoing-hashes fs current)])
                   (for-each
                    (lambda (neighbor)
                            (unless (visited-contains? visited neighbor)
                                    (visited-add visited neighbor)
                                    (dfs neighbor
                                         (cons neighbor path)
                                         (+ depth 1))
                                    (visited-remove visited neighbor)))
                    neighbors))]))
       results))

;;; path-exists? : FSCap Hash Hash → Boolean
;;; Check if any path exists from from-hash to to-hash.
;;; More efficient than shortest-path when you only need boolean result.
(define (path-exists? fs from-hash to-hash)
  (if (bytevector=? from-hash to-hash)
      #t
      (let loop ([queue (queue-enqueue (make-queue) from-hash)]
                 [visited (make-visited)])
           (if (queue-empty? queue)
               #f
               (let-values ([(current rest-queue) (queue-dequeue queue)])
                           (if (visited-contains? visited current)
                               (loop rest-queue visited)
                               (let ([neighbors (get-outgoing-hashes fs current)])
                                    (if (hash-in-list? to-hash neighbors)
                                        #t
                                        (let* ([new-visited (visited-add visited current)]
                                               [unvisited (filter (lambda (h)
                                                                          (not (visited-contains? visited h)))
                                                                  neighbors)])
                                              (loop (queue-enqueue-all rest-queue unvisited)
                                                    new-visited))))))))))


(doc connected-components 'type '(-> FSCap (List (List Hash))))
(doc connected-components 'description "Find all connected components treating graph as undirected; returns list of components")
(doc connected-components 'note "Treats the graph as undirected (follows both refs and referrers)")
(define (connected-components fs)
  (let ([all-hashes (store-all-hashes fs)]
        [visited (make-visited)]
        [components '()])
       (for-each
        (lambda (start-hash)
                (unless (visited-contains? visited start-hash)
                        (let ([component '()])
                             (let loop ([queue (queue-enqueue (make-queue) start-hash)])
                                  (unless (queue-empty? queue)
                                          (let-values ([(current rest-queue) (queue-dequeue queue)])
                                                      (if (visited-contains? visited current)
                                                          (loop rest-queue)
                                                          (begin
                                                           (set! visited (visited-add visited current))
                                                           (set! component (cons current component))
                                                           (let* ([outgoing (get-outgoing-hashes fs current)]
                                                                  [incoming (get-incoming-hashes fs current)]
                                                                  [all-neighbors (unique-hashes (append outgoing incoming))]
                                                                  [unvisited (filter (lambda (h)
                                                                                             (not (visited-contains? visited h)))
                                                                                     all-neighbors)])
                                                                 (loop (queue-enqueue-all rest-queue unvisited))))))))
                             (set! components (cons component components)))))
        all-hashes)
       components))

(doc find-cycles 'type '(-> FSCap (List (List Hash))))
(doc find-cycles 'description "Find all cycles in the directed graph using DFS-based cycle detection")
(define (find-cycles fs)
  (let ([all-hashes (store-all-hashes fs)]
        [cycles '()]
        [global-visited (make-visited)])
       (for-each
        (lambda (start-hash)
                (unless (visited-contains? global-visited start-hash)
                        (let dfs ([current start-hash]
                                  [path '()]
                                  [path-set (make-visited)])
                             (cond
                              [(visited-contains? path-set current)
                               (let ([cycle-start-idx
                                      (let find-idx ([p (reverse path)] [idx 0])
                                           (if (bytevector=? (car p) current)
                                               idx
                                               (find-idx (cdr p) (+ idx 1))))])
                                    (let ([cycle (cons current
                                                       (reverse (list-tail (reverse path)
                                                                           cycle-start-idx)))])
                                         (set! cycles (cons cycle cycles))))]
                              [(visited-contains? global-visited current)
                               (void)]
                              [else
                               (let ([new-path (cons current path)]
                                     [new-path-set (visited-add path-set current)])
                                    (for-each
                                     (lambda (neighbor)
                                             (dfs neighbor new-path new-path-set))
                                     (get-outgoing-hashes fs current))
                                    ;; No need to remove current from path-set:
                                    ;; HAMT is persistent, so path-set never had current added.
                                    ;; Backtracking is handled by the structural sharing of HAMTs.
                                    (set! global-visited (visited-add global-visited current)))]))))
        all-hashes)
       (unique-cycles cycles)))

(doc topological-sort 'type '(-> FSCap (Maybe (List Hash))))
(doc topological-sort 'description "Perform topological sort; returns sorted list or #f if graph contains cycles")
(doc topological-sort 'returns "Sorted list of hashes (dependencies before dependents), or #f if cyclic")
(define (topological-sort fs)
  (let ([all-hashes (store-all-hashes fs)]
        [result '()]
        [permanent-mark (make-visited)]
        [temporary-mark (make-visited)]
        [has-cycle #f])
       (let visit ([nodes all-hashes])
            (unless (or has-cycle (null? nodes))
                    (let ([node (car nodes)])
                         (cond
                          [(visited-contains? permanent-mark node)
                           (visit (cdr nodes))]
                          [(visited-contains? temporary-mark node)
                           (set! has-cycle #t)]
                          [else
                           (set! temporary-mark (visited-add temporary-mark node))
                           (let neighbor-loop ([neighbors (get-outgoing-hashes fs node)])
                                (unless (or has-cycle (null? neighbors))
                                        (let ([n (car neighbors)])
                                             (cond
                                              [(visited-contains? permanent-mark n)
                                               (neighbor-loop (cdr neighbors))]
                                              [(visited-contains? temporary-mark n)
                                               (set! has-cycle #t)]
                                              [else
                                               (visit (list n))
                                               (neighbor-loop (cdr neighbors))]))))
                           (unless has-cycle
                                   (set! permanent-mark (visited-add permanent-mark node))
                                   (set! result (cons node result)))
                           (visit (cdr nodes))]))))
       (if has-cycle
           #f
           result)))


;;; ====
;;; Section 5: Centrality and Importance Metrics
;;; ====

;;; in-degree : FSCap Hash → Integer
;;; Count number of incoming references to a block.
(define (in-degree fs hash)
  (length (store-find-by-ref fs hash)))

;;; out-degree : FSCap Hash → Integer
;;; Count number of outgoing references from a block.
(define (out-degree fs hash)
  (let ([block (store-get fs hash)])
       (if block
           (vector-length (block-refs block))
           0)))

;;; total-degree : FSCap Hash → Integer
;;; Sum of in-degree and out-degree.
(define (total-degree fs hash)
  (+ (in-degree fs hash) (out-degree fs hash)))

;;; find-hubs : FSCap Integer → (List (Pair Hash Integer))
;;; Find top n blocks by total degree (most connected).
;;; Returns list of (hash . degree) pairs, sorted descending.
(define (find-hubs fs n)
  (let* ([all-hashes (store-all-hashes fs)]
         [with-degrees (map (lambda (h)
                                    (cons h (total-degree fs h)))
                            all-hashes)]
         [sorted (sort-by (lambda (a b) (> (cdr a) (cdr b)))
                            with-degrees)])
        (take n sorted)))

;;; find-roots : FSCap → (List Hash)
;;; Find all blocks with no incoming references.
(define (find-roots fs)
  (let ([all-hashes (store-all-hashes fs)])
       (filter (lambda (h) (= (in-degree fs h) 0))
               all-hashes)))

;;; find-leaves : FSCap → (List Hash)
;;; Find all blocks with no outgoing references.
(define (find-leaves fs)
  (let ([all-hashes (store-all-hashes fs)])
       (filter (lambda (h) (= (out-degree fs h) 0))
               all-hashes)))


;;; ====
;;; Section 6: Subgraph Operations
;;; ====

;;; reachable-from : FSCap Hash → (List Hash)
;;; Find all blocks reachable from given hash (following outgoing refs).
;;; Includes the starting hash.
(define (reachable-from fs hash)
  (let ([result '()])
       (bfs-traverse fs hash
                     (lambda (h b)
                             (set! result (cons h result))))
       result))

;;; ancestors-of : FSCap Hash → (List Hash)
;;; Find all blocks that can reach the given hash (following incoming refs).
;;; Includes the target hash.
(define (ancestors-of fs hash)
  (let ([result '()])
       (bfs-traverse-reverse fs hash
                             (lambda (h b)
                                     (set! result (cons h result))))
       result))

;;; subgraph : FSCap (List Hash) → (List (Pair Hash (List Hash)))
;;; Extract subgraph containing only specified hashes.
;;; Returns adjacency list: ((hash . (neighbor-hashes...)) ...)
;;; Only includes edges where both endpoints are in hash-list.
(define (subgraph fs hash-list)
  (map (lambda (h)
               (let* ([outgoing (get-outgoing-hashes fs h)]
                      [filtered (filter (lambda (n) (hash-in-list? n hash-list))
                                        outgoing)])
                     (cons h filtered)))
       hash-list))

;;; induced-subgraph-blocks : FSCap (List Hash) → (List Block)
;;; Get all blocks in the induced subgraph.
(define (induced-subgraph-blocks fs hash-list)
  (filter (lambda (b) b)
          (map (lambda (h) (store-get fs h)) hash-list)))

;;; neighborhood : FSCap Hash Integer → (List Hash)
;;; Get all hashes within n hops of given hash.
;;; Includes both forward and backward edges.
(define (neighborhood fs hash n)
  (let ([result (list hash)]
        [visited (visited-add (make-visited) hash)])
       (let level-loop ([current-level (list hash)]
                        [depth 0])
            (when (< depth n)
                  (let ([next-level '()])
                       (for-each
                        (lambda (h)
                                (let* ([outgoing (get-outgoing-hashes fs h)]
                                       [incoming (get-incoming-hashes fs h)]
                                       [all-neighbors (unique-hashes (append outgoing incoming))])
                                      (for-each
                                       (lambda (neighbor)
                                               (unless (visited-contains? visited neighbor)
                                                       (set! visited (visited-add visited neighbor))
                                                       (set! result (cons neighbor result))
                                                       (set! next-level (cons neighbor next-level))))
                                       all-neighbors)))
                        current-level)
                       (level-loop next-level (+ depth 1)))))
       result))


;;; ====
;;; Section 7: Utility Functions for Analysis
;;; ====

;;; graph-stats : FSCap → Alist
;;; Compute basic statistics about the graph structure.
(define (graph-stats fs)
  (let* ([all-hashes (store-all-hashes fs)]
         [node-count (length all-hashes)]
         [edge-count (fold-left (lambda (acc h)
                                        (+ acc (out-degree fs h)))
                                0 all-hashes)]
         [roots (find-roots fs)]
         [leaves (find-leaves fs)]
         [components (connected-components fs)]
         [topo (topological-sort fs)])
        `((nodes . ,node-count)
          (edges . ,edge-count)
          (roots . ,(length roots))
          (leaves . ,(length leaves))
          (components . ,(length components))
          (cyclic . ,(not topo)))))

;;; print-graph-stats : FSCap → Void
;;; Print human-readable graph statistics.
(define (print-graph-stats fs)
  (let ([stats (graph-stats fs)])
       (printf "Graph Statistics:
")
       (printf "  Nodes:      ~a
" (cdr (assq 'nodes stats)))
       (printf "  Edges:      ~a
" (cdr (assq 'edges stats)))
       (printf "  Roots:      ~a
" (cdr (assq 'roots stats)))
       (printf "  Leaves:     ~a
" (cdr (assq 'leaves stats)))
       (printf "  Components: ~a
" (cdr (assq 'components stats)))
       (printf "  Cyclic:     ~a
" (if (cdr (assq 'cyclic stats)) "yes" "no"))))

;;; format-path : FSCap (List Hash) → String
;;; Format a path as human-readable string showing block tags.
(define (format-path fs path)
  (let ([tags (map (lambda (h)
                           (let ([b (store-get fs h)])
                                (if b
                                    (symbol->string (block-tag b))
                                    "???")))
                   path)])
       (let loop ([ts tags] [result ""])
            (if (null? ts)
                result
                (if (string=? result "")
                    (loop (cdr ts) (car ts))
                    (loop (cdr ts) (string-append result " -> " (car ts))))))))


;;; ====
;;; Load Complete
;;; ====

(printf "✓ Graph traversal loaded (boundary)
")
(printf "  Traversal:   bfs-traverse, dfs-traverse
")
(printf "  Paths:       shortest-path, all-paths, path-exists?
")
(printf "  Analysis:    connected-components, find-cycles, topological-sort
")
(printf "  Centrality:  in-degree, out-degree, find-hubs, find-roots, find-leaves
")
(printf "  Subgraphs:   reachable-from, ancestors-of, subgraph, neighborhood
")
