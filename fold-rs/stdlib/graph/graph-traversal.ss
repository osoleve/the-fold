; ============================================================
; Graph - Traversal Operations
; BFS, DFS, path finding, reachability, connectivity
; Part of graph.ss module
; ============================================================

; Depends on graph-core.ss for: graph-neighbors, graph-vertices

; graph-bfs: Breadth-first search from start vertex
; Returns list of vertices in BFS order
; (graph-bfs {a: [b c], b: [d], c: [d]} 'a) => (a b c d)
(graph-bfs (fix graph-bfs
                (fn (g start)
                    (let ((bfs-helper (fix bfs-helper
                                           (fn (queue visited)
                                               (if (null? queue)
                                                   (reverse visited)
                                                   (let ((current (car queue))
                                                         (rest-queue (cdr queue)))
                                                        (if (member? current visited)
                                                            (bfs-helper rest-queue visited)
                                                            (let ((neighbors (filter (fn (n) (not (member? n visited)))
                                                                                     (graph-neighbors g current))))
                                                                 (bfs-helper (append rest-queue neighbors)
                                                                             (cons current visited))))))))))
                         (bfs-helper (list start) '())))))

; graph-dfs: Depth-first search from start vertex
; Returns list of vertices in DFS order
; (graph-dfs {a: [b c], b: [d], c: [d]} 'a) => (a b d c)
(graph-dfs (fix graph-dfs
                (fn (g start)
                    (let ((dfs-helper (fix dfs-helper
                                           (fn (stack visited)
                                               (if (null? stack)
                                                   (reverse visited)
                                                   (let ((current (car stack))
                                                         (rest-stack (cdr stack)))
                                                        (if (member? current visited)
                                                            (dfs-helper rest-stack visited)
                                                            (let ((neighbors (filter (fn (n) (not (member? n visited)))
                                                                                     (graph-neighbors g current))))
                                                                 (dfs-helper (append neighbors rest-stack)
                                                                             (cons current visited))))))))))
                         (dfs-helper (list start) '())))))

; graph-dfs-pre: DFS with pre-order callback
; Returns list of (vertex depth) pairs in pre-order
(graph-dfs-pre (fix graph-dfs-pre
                    (fn (g start)
                        (let ((helper (fix helper
                                           (fn (stack visited depth-map)
                                               (if (null? stack)
                                                   (reverse visited)
                                                   (let ((current (car (car stack)))
                                                         (depth (car (cdr (car stack))))
                                                         (rest-stack (cdr stack)))
                                                        (if (member? current (map car visited))
                                                            (helper rest-stack visited depth-map)
                                                            (let ((neighbors (filter (fn (n) (not (member? n (map car visited))))
                                                                                     (graph-neighbors g current))))
                                                                 (helper (append (map (fn (n) (list n (+ depth 1))) neighbors)
                                                                                 rest-stack)
                                                                         (cons (list current depth) visited)
                                                                         depth-map)))))))))
                             (helper (list (list start 0)) '() '())))))

; graph-path-exists?: Check if path exists from u to v
; (graph-path-exists? {a: [b], b: [c]} 'a 'c) => #t
(graph-path-exists? (fn (g u v)
                        (member? v (graph-bfs g u))))

; graph-reachable: Get all vertices reachable from start
; (graph-reachable {a: [b], b: [c], d: []} 'a) => (a b c)
(graph-reachable (fn (g start)
                     (graph-bfs g start)))

; graph-reachable-from-any: Get all vertices reachable from any vertex in list
(graph-reachable-from-any (fn (g starts)
                              (let ((all-reachable (foldl (fn (acc s)
                                                              (append acc (graph-bfs g s)))
                                                          '()
                                                          starts)))
                                   ; Remove duplicates
                                   (foldl (fn (acc v)
                                              (if (member? v acc)
                                                  acc
                                                  (cons v acc)))
                                          '()
                                          all-reachable))))

; graph-connected?: Check if undirected graph is connected
; All vertices reachable from any starting vertex
; (graph-connected? {a: [b], b: [a c], c: [b]}) => #t
(graph-connected? (fn (g)
                      (let ((vertices (graph-vertices g)))
                           (if (null? vertices)
                               #t
                               (= (length (graph-bfs g (car vertices)))
                                  (length vertices))))))

; graph-find-path: Find a path from u to v using BFS (shortest path)
; Returns list of vertices from u to v, or #f if no path exists
(graph-find-path (fix graph-find-path
                      (fn (g u v)
                          (let ((helper (fix helper
                                             (fn (queue visited parents)
                                                 (if (null? queue)
                                                     #f
                                                     (let ((current (car queue))
                                                           (rest-queue (cdr queue)))
                                                          (if (= current v)
                                                              ; Reconstruct path
                                                              (let ((reconstruct (fix reconstruct
                                                                                      (fn (node path)
                                                                                          (if (= node u)
                                                                                              (cons u path)
                                                                                              (reconstruct (dict-get parents node #f)
                                                                                                           (cons node path)))))))
                                                                   (reconstruct v '()))
                                                              (if (member? current visited)
                                                                  (helper rest-queue visited parents)
                                                                  (let ((neighbors (filter (fn (n) (not (member? n visited)))
                                                                                           (graph-neighbors g current)))
                                                                        (new-parents (foldl (fn (p n)
                                                                                                (if (dict-has? p n)
                                                                                                    p
                                                                                                    (dict-set p n current)))
                                                                                            parents
                                                                                            neighbors)))
                                                                       (helper (append rest-queue neighbors)
                                                                               (cons current visited)
                                                                               new-parents))))))))))
                               (if (= u v)
                                   (list u)
                                   (helper (list u) '() (dict-set '() u #f)))))))

; graph-all-paths: Find all simple paths from u to v (DFS based)
; Warning: can be exponential in large graphs
(graph-all-paths (fix graph-all-paths
                      (fn (g u v)
                          (let ((helper (fix helper
                                             (fn (current path visited)
                                                 (if (= current v)
                                                     (list (reverse (cons current path)))
                                                     (let ((neighbors (filter (fn (n) (not (member? n visited)))
                                                                              (graph-neighbors g current))))
                                                          (foldl (fn (acc n)
                                                                     (append acc (helper n
                                                                                         (cons current path)
                                                                                         (cons current visited))))
                                                                 '()
                                                                 neighbors)))))))
                               (helper u '() '())))))

; graph-traverse: Generic graph traversal with callback
; Applies f to each vertex in BFS order, returns list of results
(graph-traverse (fn (g start f)
                    (map f (graph-bfs g start))))

; graph-fold: Fold over graph vertices in BFS order
(graph-fold (fn (g start f init)
                (foldl f init (graph-bfs g start))))

; graph-distance: Find shortest distance between two vertices
; Returns -1 if no path exists
(graph-distance (fn (g u v)
                    (let ((path (graph-find-path g u v)))
                         (if path
                             (- (length path) 1)
                             -1))))

; graph-distances-from: Get distances from start to all reachable vertices
; Returns dictionary mapping vertex -> distance
(graph-distances-from (fix graph-distances-from
                           (fn (g start)
                               (let ((helper (fix helper
                                                  (fn (queue distances)
                                                      (if (null? queue)
                                                          distances
                                                          (let ((current (car (car queue)))
                                                                (dist (car (cdr (car queue))))
                                                                (rest-queue (cdr queue)))
                                                               (if (dict-has? distances current)
                                                                   (helper rest-queue distances)
                                                                   (let ((new-distances (dict-set distances current dist))
                                                                         (neighbors (filter (fn (n) (not (dict-has? distances n)))
                                                                                            (graph-neighbors g current))))
                                                                        (helper (append rest-queue
                                                                                        (map (fn (n) (list n (+ dist 1))) neighbors))
                                                                                new-distances)))))))))
                                    (helper (list (list start 0)) '())))))

; --- Module Exports ---
; (see exports.ss for exported symbols)
