;;; graph-algorithms.ss — Graph Algorithms for Block Reference Networks
;;;
;;; This library provides graph algorithms for analyzing the directed graph
;;; formed by block references. Blocks are nodes, and refs are directed edges.
;;;
;;; DESIGN PRINCIPLES:
;;;   - Pure functional where possible (except fs-capability access)
;;;   - Efficient visited-set tracking to avoid cycles
;;;   - Composable with existing store-api and collection-utils
;;;   - Return meaningful results (#f for not-found, '() for empty sets)
;;;
;;; TIER ASSIGNMENT:
;;;   Tier 6: Traversal and path operations
;;;   Tier 7: Full graph analysis (connected components, cycles, etc.)
;;;
;;; DEPENDENCIES:
;;;   - store-api.ss (store-get, store-all-hashes, store-get-refs, etc.)
;;;   - collection-utils.ss (collection-hashes, etc.)
;;;
;;; USAGE:
;;;   (load "thimble/store-api.ss")
;;;   (load "fabric/patterns/graph-algorithms.ss")
;;;   (define fs (make-fs-capability ".store"))
;;;   (shortest-path fs from-hash to-hash)

(load "thimble/store-api.ss")
(load "fabric/patterns/collection-utils.ss")

;;; ============================================================
;;; Section 1: Helper Functions
;;; ============================================================

;;; --- Visited Set Management ---
;;; We use a hash table for O(1) membership testing.
;;; Keys are bytevectors (block hashes), values are #t.

;;; bytevector-hash : Bytevector → Integer
;;; Hash function for bytevectors (FNV-1a inspired).
(define (bytevector-hash bv)
  (let ([len (bytevector-length bv)])
       (let loop ([i 0] [h 2166136261])  ; FNV offset basis
            (if (>= i len)
                (bitwise-and h #xFFFFFFFF)  ; Keep 32-bit
                (loop (+ i 1)
                      (bitwise-and
                       (* (bitwise-xor h (bytevector-u8-ref bv i))
                          16777619)  ; FNV prime
                       #xFFFFFFFF))))))

;;; make-visited : → HashTable
;;; Create an empty visited set (hash table).
(define (make-visited)
  (make-hashtable bytevector-hash bytevector=?))

;;; visited-add : HashTable Hash → HashTable
;;; Add a hash to the visited set. Mutates and returns the table.
(define (visited-add visited hash)
  (hashtable-set! visited hash #t)
  visited)

;;; visited-contains? : HashTable Hash → Boolean
;;; Check if hash is in visited set. O(1) lookup.
(define (visited-contains? visited hash)
  (hashtable-ref visited hash #f))

;;; visited-remove : HashTable Hash → HashTable
;;; Remove a hash from the visited set. Used for backtracking.
(define (visited-remove visited hash)
  (hashtable-delete! visited hash)
  visited)

;;; --- Queue Operations (for BFS) ---
;;; Using Okasaki's two-list queue for amortized O(1) operations.
;;; Queue = (front-list . back-list)
;;; - Enqueue: cons to back
;;; - Dequeue: pop from front, reverse back if front empty

;;; make-queue : → Queue
;;; Create an empty queue.
(define (make-queue)
  (cons '() '()))

;;; queue-empty? : Queue → Boolean
;;; Check if queue is empty.
(define (queue-empty? q)
  (and (null? (car q)) (null? (cdr q))))

;;; queue-normalize : Queue → Queue
;;; Internal: ensure front is non-empty if possible.
(define (queue-normalize q)
  (if (null? (car q))
      (cons (reverse (cdr q)) '())
      q))

;;; queue-enqueue : Queue Any → Queue
;;; Add element to back of queue. O(1).
(define (queue-enqueue q item)
  (queue-normalize (cons (car q) (cons item (cdr q)))))

;;; queue-enqueue-all : Queue (List Any) → Queue
;;; Add all elements to back of queue. O(k) where k = length of items.
(define (queue-enqueue-all q items)
  (queue-normalize (cons (car q) (append (reverse items) (cdr q)))))

;;; queue-dequeue : Queue → (Values Any Queue)
;;; Remove and return front element. Amortized O(1).
(define (queue-dequeue q)
  (let ([nq (queue-normalize q)])
       (values (car (car nq))
               (queue-normalize (cons (cdr (car nq)) (cdr nq))))))

;;; --- Stack Operations (for DFS) ---

;;; make-stack : → (List Any)
;;; Create an empty stack (as a list).
(define (make-stack)
  '())

;;; stack-empty? : (List Any) → Boolean
;;; Check if stack is empty.
(define (stack-empty? s)
  (null? s))

;;; stack-push : (List Any) Any → (List Any)
;;; Push element onto stack.
(define (stack-push s item)
  (cons item s))

;;; stack-push-all : (List Any) (List Any) → (List Any)
;;; Push all elements onto stack (first in list will be on top).
(define (stack-push-all s items)
  (append items s))

;;; stack-pop : (List Any) → (Values Any (List Any))
;;; Pop and return top element.
(define (stack-pop s)
  (values (car s) (cdr s)))

;;; --- Hash Utilities ---

;;; hash-equal? : Hash Hash → Boolean
;;; Compare two hashes for equality.
(define (hash-equal? h1 h2)
  (bytevector=? h1 h2))

;;; hash-in-list? : Hash (List Hash) → Boolean
;;; Check if hash is in a list of hashes.
(define (hash-in-list? hash lst)
  (let loop ([l lst])
       (cond
        [(null? l) #f]
        [(bytevector=? (car l) hash) #t]
        [else (loop (cdr l))])))

;;; remove-hash : Hash (List Hash) → (List Hash)
;;; Remove a hash from a list.
(define (remove-hash hash lst)
  (filter (lambda (h) (not (bytevector=? h hash))) lst))

;;; unique-hashes : (List Hash) → (List Hash)
;;; Remove duplicate hashes from a list.
(define (unique-hashes hashes)
  (let loop ([remaining hashes]
             [seen '()]
             [result '()])
       (if (null? remaining)
           (reverse result)
           (let ([h (car remaining)])
                (if (hash-in-list? h seen)
                    (loop (cdr remaining) seen result)
                    (loop (cdr remaining)
                          (cons h seen)
                          (cons h result)))))))

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


;;; ============================================================
;;; Section 2: Traversal Primitives (Tier 6)
;;; ============================================================

;;; bfs-traverse : FSCap Hash (Hash Block → Void) → Void
;;; Breadth-first traversal starting from start-hash.
;;; Calls visit-fn with (hash, block) for each visited node.
;;; Follows outgoing references (block-refs).
;;;
;;; Example:
;;;   (bfs-traverse fs root-hash
;;;     (lambda (h b)
;;;       (printf "Visiting: ~a" (block-tag b))
;;;       (newline)))
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
                                          ;; Optimization: use block-refs directly instead of calling get-outgoing-hashes
                                          (let* ([neighbors (vector->list (block-refs block))]
                                                 [unvisited (filter (lambda (h)
                                                                            (not (visited-contains? visited h)))
                                                                    neighbors)])
                                                (loop (queue-enqueue-all rest-queue unvisited)
                                                      (visited-add visited current))))))))))

;;; dfs-traverse : FSCap Hash (Hash Block → Void) → Void
;;; Depth-first traversal starting from start-hash.
;;; Calls visit-fn with (hash, block) for each visited node.
;;; Follows outgoing references (block-refs).
;;;
;;; Example:
;;;   (dfs-traverse fs root-hash
;;;     (lambda (h b)
;;;       (printf "Visiting: ~a" (block-tag b))
;;;       (newline)))
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
                                          ;; Optimization: use block-refs directly instead of calling get-outgoing-hashes
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


;;; ============================================================
;;; Section 3: Path Finding Algorithms (Tier 6)
;;; ============================================================

;;; shortest-path : FSCap Hash Hash → (Maybe (List Hash))
;;; Find shortest path from from-hash to to-hash using BFS.
;;; Returns list of hashes representing path, or #f if no path exists.
;;; The path includes both endpoints.
;;;
;;; Example:
;;;   (shortest-path fs block-a-hash block-c-hash)
;;;   => (#vu8(...) #vu8(...) #vu8(...))  ; A -> B -> C
(define (shortest-path fs from-hash to-hash)
  (if (bytevector=? from-hash to-hash)
      (list from-hash)  ; Same node, trivial path
      (let loop ([queue (queue-enqueue (make-queue) (list from-hash))]
                 [visited (make-visited)])
           (if (queue-empty? queue)
               #f  ; No path found
               (let-values ([(current-path rest-queue) (queue-dequeue queue)])
                           (let ([current (car current-path)])
                                (if (visited-contains? visited current)
                                    (loop rest-queue visited)
                                    (let ([neighbors (get-outgoing-hashes fs current)])
                                         ;; Check if we found the target
                                         (let ([found (filter (lambda (h) (bytevector=? h to-hash))
                                                              neighbors)])
                                              (if (not (null? found))
                                                  (reverse (cons to-hash current-path))  ; Found!
                                                  ;; Continue searching
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
;;;
;;; Example:
;;;   (all-paths fs start end 5)
;;;   => (((path1...)) ((path2...)))
(define (all-paths fs from-hash to-hash max-depth)
  (let ([results '()]
        [visited (make-visited)])
       ;; Mark start as visited
       (visited-add visited from-hash)
       (let dfs ([current from-hash]
                 [path (list from-hash)]
                 [depth 0])
            (cond
             ;; Found target
             [(bytevector=? current to-hash)
              (set! results (cons (reverse path) results))]
             ;; Depth limit reached
             [(>= depth max-depth)
              (void)]
             ;; Continue searching
             [else
              (let ([neighbors (get-outgoing-hashes fs current)])
                   (for-each
                    (lambda (neighbor)
                            (unless (visited-contains? visited neighbor)
                                    ;; Mark visited before recursing
                                    (visited-add visited neighbor)
                                    (dfs neighbor
                                         (cons neighbor path)
                                         (+ depth 1))
                                    ;; Backtrack: unmark after returning
                                    (visited-remove visited neighbor)))
                    neighbors))]))
       results))

;;; path-exists? : FSCap Hash Hash → Boolean
;;; Check if any path exists from from-hash to to-hash.
;;; More efficient than shortest-path when you only need boolean result.
;;;
;;; Example:
;;;   (path-exists? fs block-a block-b) => #t
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


;;; ============================================================
;;; Section 4: Graph Analysis (Tier 7)
;;; ============================================================

;;; connected-components : FSCap → (List (List Hash))
;;; Find all connected components in the graph.
;;; Treats the graph as undirected (follows both refs and referrers).
;;; Returns list of components, each component is a list of hashes.
;;;
;;; Example:
;;;   (connected-components fs)
;;;   => (((hash1 hash2 hash3)) ((hash4 hash5)))  ; Two components
(define (connected-components fs)
  (let ([all-hashes (store-all-hashes fs)]
        [visited (make-visited)]
        [components '()])
       ;; For each unvisited node, find its component
       (for-each
        (lambda (start-hash)
                (unless (visited-contains? visited start-hash)
                        (let ([component '()])
                             ;; BFS to find all connected nodes (undirected)
                             (let loop ([queue (queue-enqueue (make-queue) start-hash)])
                                  (unless (queue-empty? queue)
                                          (let-values ([(current rest-queue) (queue-dequeue queue)])
                                                      (if (visited-contains? visited current)
                                                          (loop rest-queue)
                                                          (begin
                                                           (set! visited (visited-add visited current))
                                                           (set! component (cons current component))
                                                           ;; Get both outgoing and incoming edges
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

;;; find-cycles : FSCap → (List (List Hash))
;;; Find all cycles in the directed graph.
;;; Returns list of cycles, each cycle is a list of hashes.
;;; Uses DFS-based cycle detection.
;;;
;;; Example:
;;;   (find-cycles fs)
;;;   => (((hashA hashB hashC hashA)))  ; Cycle A -> B -> C -> A
(define (find-cycles fs)
  (let ([all-hashes (store-all-hashes fs)]
        [cycles '()]
        [global-visited (make-visited)])
       (for-each
        (lambda (start-hash)
                (unless (visited-contains? global-visited start-hash)
                        ;; DFS with path tracking
                        (let dfs ([current start-hash]
                                  [path '()]
                                  [path-set (make-visited)])
                             (cond
                              ;; Found a cycle back to a node in current path
                              [(visited-contains? path-set current)
                               (let ([cycle-start-idx
                                      (let find-idx ([p (reverse path)] [idx 0])
                                           (if (bytevector=? (car p) current)
                                               idx
                                               (find-idx (cdr p) (+ idx 1))))])
                                    ;; Extract cycle from path
                                    (let ([cycle (cons current
                                                       (reverse (list-tail (reverse path)
                                                                           cycle-start-idx)))])
                                         (set! cycles (cons cycle cycles))))]
                              ;; Already fully processed this node
                              [(visited-contains? global-visited current)
                               (void)]
                              ;; Process this node
                              [else
                               (let ([new-path (cons current path)]
                                     [new-path-set (visited-add path-set current)])
                                    (for-each
                                     (lambda (neighbor)
                                             (dfs neighbor new-path new-path-set))
                                     (get-outgoing-hashes fs current))
                                    (set! global-visited (visited-add global-visited current)))]))))
        all-hashes)
       ;; Remove duplicate cycles (same cycle may be found from different starts)
       (unique-cycles cycles)))

;;; unique-cycles : (List (List Hash)) → (List (List Hash))
;;; Remove duplicate cycles from list.
;;; Two cycles are duplicates if they contain the same nodes.
(define (unique-cycles cycles)
  (let loop ([remaining cycles]
             [result '()])
       (if (null? remaining)
           result
           (let ([cycle (car remaining)])
                (if (cycle-in-list? cycle result)
                    (loop (cdr remaining) result)
                    (loop (cdr remaining) (cons cycle result)))))))

;;; cycle-in-list? : (List Hash) (List (List Hash)) → Boolean
;;; Check if cycle is already in list (as same set of nodes).
(define (cycle-in-list? cycle cycles)
  (let ([cycle-set (cdr cycle)])  ; Remove duplicate endpoint
       (let loop ([cs cycles])
            (if (null? cs)
                #f
                (let ([other-set (cdr (car cs))])
                     (if (same-hash-set? cycle-set other-set)
                         #t
                         (loop (cdr cs))))))))

;;; same-hash-set? : (List Hash) (List Hash) → Boolean
;;; Check if two lists contain the same hashes (as sets).
(define (same-hash-set? lst1 lst2)
  (and (= (length lst1) (length lst2))
       (let loop ([l lst1])
            (if (null? l)
                #t
                (and (hash-in-list? (car l) lst2)
                     (loop (cdr l)))))))

;;; topological-sort : FSCap → (Maybe (List Hash))
;;; Perform topological sort on the directed graph.
;;; Returns sorted list of hashes (dependencies before dependents),
;;; or #f if the graph contains cycles.
;;;
;;; Example:
;;;   (topological-sort fs)
;;;   => (#vu8(...) #vu8(...) #vu8(...))  ; Sorted order
;;;   or #f if cyclic
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
                           (set! has-cycle #t)]  ; Cycle detected
                          [else
                           ;; Visit this node
                           (set! temporary-mark (visited-add temporary-mark node))
                           ;; Visit all neighbors
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
                           ;; Mark as permanently visited
                           (unless has-cycle
                                   (set! permanent-mark (visited-add permanent-mark node))
                                   (set! result (cons node result)))
                           ;; Continue with remaining nodes
                           (visit (cdr nodes))]))))
       (if has-cycle
           #f
           result)))


;;; ============================================================
;;; Section 5: Centrality and Importance Metrics (Tier 6-7)
;;; ============================================================

;;; in-degree : FSCap Hash → Integer
;;; Count number of incoming references to a block.
;;; Returns 0 if block not found.
;;;
;;; Example:
;;;   (in-degree fs some-hash) => 5
(define (in-degree fs hash)
  (length (store-find-by-ref fs hash)))

;;; out-degree : FSCap Hash → Integer
;;; Count number of outgoing references from a block.
;;; Returns 0 if block not found.
;;;
;;; Example:
;;;   (out-degree fs some-hash) => 3
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
;;;
;;; Example:
;;;   (find-hubs fs 10)
;;;   => ((#vu8(...) . 15) (#vu8(...) . 12) ...)
(define (find-hubs fs n)
  (let* ([all-hashes (store-all-hashes fs)]
         [with-degrees (map (lambda (h)
                                    (cons h (total-degree fs h)))
                            all-hashes)]
         [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                            with-degrees)])
        (take-up-to sorted n)))

;;; take-up-to : (List A) Integer → (List A)
;;; Take up to n elements from list.
(define (take-up-to lst n)
  (let loop ([l lst] [count n] [result '()])
       (if (or (null? l) (<= count 0))
           (reverse result)
           (loop (cdr l) (- count 1) (cons (car l) result)))))

;;; find-roots : FSCap → (List Hash)
;;; Find all blocks with no incoming references.
;;; These are potential entry points to the graph.
;;;
;;; Example:
;;;   (find-roots fs)
;;;   => (#vu8(...) #vu8(...))  ; Root nodes
(define (find-roots fs)
  (let ([all-hashes (store-all-hashes fs)])
       (filter (lambda (h) (= (in-degree fs h) 0))
               all-hashes)))

;;; find-leaves : FSCap → (List Hash)
;;; Find all blocks with no outgoing references.
;;; These are terminal nodes in the graph.
;;;
;;; Example:
;;;   (find-leaves fs)
;;;   => (#vu8(...) #vu8(...))  ; Leaf nodes
(define (find-leaves fs)
  (let ([all-hashes (store-all-hashes fs)])
       (filter (lambda (h) (= (out-degree fs h) 0))
               all-hashes)))


;;; ============================================================
;;; Section 6: Subgraph Operations (Tier 6-7)
;;; ============================================================

;;; reachable-from : FSCap Hash → (List Hash)
;;; Find all blocks reachable from given hash (following outgoing refs).
;;; Includes the starting hash.
;;;
;;; Example:
;;;   (reachable-from fs root-hash)
;;;   => (#vu8(...) #vu8(...) ...)  ; All descendants
(define (reachable-from fs hash)
  (let ([result '()])
       (bfs-traverse fs hash
                     (lambda (h b)
                             (set! result (cons h result))))
       result))

;;; ancestors-of : FSCap Hash → (List Hash)
;;; Find all blocks that can reach the given hash (following incoming refs).
;;; Includes the target hash.
;;;
;;; Example:
;;;   (ancestors-of fs leaf-hash)
;;;   => (#vu8(...) #vu8(...) ...)  ; All ancestors
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
;;;
;;; Example:
;;;   (subgraph fs (list hash1 hash2 hash3))
;;;   => ((hash1 . (hash2)) (hash2 . (hash3)) (hash3 . ()))
(define (subgraph fs hash-list)
  (map (lambda (h)
               (let* ([outgoing (get-outgoing-hashes fs h)]
                      [filtered (filter (lambda (n) (hash-in-list? n hash-list))
                                        outgoing)])
                     (cons h filtered)))
       hash-list))

;;; induced-subgraph-blocks : FSCap (List Hash) → (List Block)
;;; Get all blocks in the induced subgraph.
;;; Returns list of blocks corresponding to hash-list.
;;;
;;; Example:
;;;   (induced-subgraph-blocks fs (list h1 h2 h3))
;;;   => (block1 block2 block3)
(define (induced-subgraph-blocks fs hash-list)
  (filter (lambda (b) b)  ; Remove #f values
          (map (lambda (h) (store-get fs h)) hash-list)))

;;; neighborhood : FSCap Hash Integer → (List Hash)
;;; Get all hashes within n hops of given hash.
;;; Includes both forward and backward edges.
;;;
;;; Example:
;;;   (neighborhood fs center-hash 2)
;;;   => (#vu8(...) ...)  ; All nodes within 2 hops
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


;;; ============================================================
;;; Section 7: Utility Functions for Analysis (Tier 6)
;;; ============================================================

;;; graph-stats : FSCap → Alist
;;; Compute basic statistics about the graph structure.
;;; Returns: ((nodes . N) (edges . N) (roots . N) (leaves . N)
;;;          (components . N) (cyclic . Boolean))
;;;
;;; Example:
;;;   (graph-stats fs)
;;;   => ((nodes . 100) (edges . 250) (roots . 5) ...)
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

;;; path-length : (List Hash) → Integer
;;; Get length of a path (number of edges).
(define (path-length path)
  (if (null? path)
      0
      (- (length path) 1)))

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


;;; ============================================================
;;; Load Complete
;;; ============================================================

(printf "✓ Graph algorithms loaded
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
