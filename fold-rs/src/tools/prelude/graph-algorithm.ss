; ============================================================
; Graph - Algorithms
; Topological sort, cycle detection, connected components, etc.
; Part of graph.ss module
; ============================================================

; Depends on graph-core.ss and graph-traversal.ss

; graph-topological-sort: Kahn's algorithm for topological ordering
; Returns sorted list of vertices, or #f if graph has cycle
; Only valid for directed acyclic graphs (DAGs)
(graph-topological-sort (fix graph-topological-sort
                             (fn (g)
                                 (let ((vertices (graph-vertices g)))
                                      ; Compute in-degrees
                                      (let ((in-degrees (foldl (fn (d v)
                                                                   (dict-set d v (graph-in-degree g v)))
                                                               '()
                                                               vertices)))
                                           ; Find initial zero in-degree vertices
                                           (let ((zero-deg (filter (fn (v) (= 0 (dict-get in-degrees v 0)))
                                                                   vertices)))
                                                (let ((helper (fix helper
                                                                   (fn (queue result in-degs)
                                                                       (if (null? queue)
                                                                           (if (= (length result) (length vertices))
                                                                               (reverse result)
                                                                               #f) ; Cycle detected
                                                                           (let ((v (car queue))
                                                                                 (rest-queue (cdr queue)))
                                                                                ; Update in-degrees of neighbors
                                                                                (let ((neighbors (graph-neighbors g v))
                                                                                      (new-in-degs (foldl (fn (d n)
                                                                                                              (dict-set d n (- (dict-get d n 0) 1)))
                                                                                                          in-degs
                                                                                                          neighbors)))
                                                                                     ; Find new zero in-degree vertices
                                                                                     (let ((new-zeros (filter (fn (n) (= 0 (dict-get new-in-degs n 0)))
                                                                                                              neighbors)))
                                                                                          (helper (append rest-queue new-zeros)
                                                                                                  (cons v result)
                                                                                                  new-in-degs)))))))))
                                                     (helper zero-deg '() in-degrees))))))))

; graph-has-cycle?: Check if directed graph contains a cycle
; Uses DFS with three-color marking (white=0, gray=1, black=2)
(graph-has-cycle? (fix graph-has-cycle?
                       (fn (g)
                           (let ((vertices (graph-vertices g)))
                                (let ((dfs-cycle (fix dfs-cycle
                                                      (fn (v colors)
                                                          (let ((color (dict-get colors v 0)))
                                                               (if (= color 1)
                                                                   ; Gray - back edge, cycle found
                                                                   (list #t colors)
                                                                   (if (= color 2)
                                                                       ; Black - already processed
                                                                       (list #f colors)
                                                                       ; White - start DFS
                                                                       (let ((gray-colors (dict-set colors v 1))
                                                                             (neighbors (graph-neighbors g v)))
                                                                            (let ((result (foldl (fn (acc n)
                                                                                                     (if (car acc)
                                                                                                         acc ; Already found cycle
                                                                                                         (dfs-cycle n (car (cdr acc)))))
                                                                                                 (list #f gray-colors)
                                                                                                 neighbors)))
                                                                                 (if (car result)
                                                                                     result
                                                                                     (list #f (dict-set (car (cdr result)) v 2)))))))))))
                                      ; Check from each unvisited vertex
                                      (car (foldl (fn (acc v)
                                                      (if (car acc)
                                                          acc ; Already found cycle
                                                          (dfs-cycle v (car (cdr acc)))))
                                                  (list #f '())
                                                  vertices))))))))

; graph-find-cycle: Find a cycle in the graph if one exists
; Returns list of vertices forming cycle, or #f if no cycle
(graph-find-cycle (fix graph-find-cycle
                       (fn (g)
                           (let ((vertices (graph-vertices g)))
                                (let ((dfs-find (fix dfs-find
                                                     (fn (v path colors)
                                                         (let ((color (dict-get colors v 0)))
                                                              (if (= color 1)
                                                                  ; Back edge - extract cycle from path
                                                                  (let ((cycle-start (member v path)))
                                                                       (if cycle-start
                                                                           (reverse cycle-start)
                                                                           (list v)))
                                                                  (if (= color 2)
                                                                      #f
                                                                      (let ((gray-colors (dict-set colors v 1))
                                                                            (new-path (cons v path))
                                                                            (neighbors (graph-neighbors g v)))
                                                                           (let ((result (foldl (fn (acc n)
                                                                                                    (if acc
                                                                                                        acc ; Already found cycle
                                                                                                        (dfs-find n new-path (car (cdr acc)))))
                                                                                                #f
                                                                                                (map (fn (n) (list n gray-colors)) neighbors))))
                                                                                (if result
                                                                                    result
                                                                                    #f))))))))))
                                     ; Check from each vertex
                                     (foldl (fn (acc v)
                                                (if acc
                                                    acc
                                                    (dfs-find v '() '())))
                                            #f
                                            vertices))))))

; graph-connected-components: Find all connected components in undirected graph
; Returns list of vertex lists, each representing a component
(graph-connected-components (fix graph-connected-components
                                 (fn (g)
                                     (let ((vertices (graph-vertices g)))
                                          (let ((helper (fix helper
                                                             (fn (remaining components)
                                                                 (if (null? remaining)
                                                                     components
                                                                     (let ((component (graph-bfs g (car remaining)))
                                                                           (new-remaining (filter (fn (v) (not (member? v component)))
                                                                                                  remaining)))
                                                                          (helper new-remaining (cons component components))))))))
                                               (helper vertices '()))))))

; graph-strongly-connected-components: Kosaraju's algorithm for SCCs
; Returns list of vertex lists, each representing an SCC
(graph-strongly-connected-components
 (fix graph-strongly-connected-components
      (fn (g)
          (let ((vertices (graph-vertices g)))
               ; First DFS pass - get finish order
               (let ((dfs-finish (fix dfs-finish
                                      (fn (v visited finish-order)
                                          (if (member? v visited)
                                              (list visited finish-order)
                                              (let ((new-visited (cons v visited))
                                                    (neighbors (graph-neighbors g v)))
                                                   (let ((result (foldl (fn (acc n)
                                                                            (dfs-finish n (car acc) (car (cdr acc))))
                                                                        (list new-visited finish-order)
                                                                        neighbors)))
                                                        (list (car result) (cons v (car (cdr result))))))))))
                     ; Get finish order from all vertices
                     (let ((finish-result (foldl (fn (acc v)
                                                     (dfs-finish v (car acc) (car (cdr acc))))
                                                 (list '() '())
                                                 vertices)))
                          (let ((finish-order (car (cdr finish-result)))
                                ; Reverse graph
                                (rev-g (graph-reverse g)))
                               ; Second DFS pass on reversed graph in finish order
                               (let ((dfs-component (fix dfs-component
                                                         (fn (v visited)
                                                             (if (member? v visited)
                                                                 (list '() visited)
                                                                 (let ((new-visited (cons v visited))
                                                                       (neighbors (graph-neighbors rev-g v)))
                                                                      (let ((result (foldl (fn (acc n)
                                                                                               (let ((sub (dfs-component n (car (cdr acc)))))
                                                                                                    (list (append (car acc) (car sub))
                                                                                                          (car (cdr sub)))))
                                                                                           (list (list v) new-visited)
                                                                                           neighbors)))
                                                                           result)))))))
                                    (let ((scc-helper (fix scc-helper
                                                           (fn (order visited sccs)
                                                               (if (null? order)
                                                                   sccs
                                                                   (let ((v (car order)))
                                                                        (if (member? v visited)
                                                                            (scc-helper (cdr order) visited sccs)
                                                                            (let ((result (dfs-component v visited)))
                                                                                 (scc-helper (cdr order)
                                                                                             (car (cdr result))
                                                                                             (cons (car result) sccs))))))))))
                                         (scc-helper finish-order '() '())))))))))))

; graph-is-dag?: Check if graph is a directed acyclic graph
(graph-is-dag? (fn (g)
                   (not (graph-has-cycle? g))))

; graph-sources: Find all source vertices (in-degree 0)
(graph-sources (fn (g)
                   (filter (fn (v) (= 0 (graph-in-degree g v)))
                           (graph-vertices g))))

; graph-sinks: Find all sink vertices (out-degree 0)
(graph-sinks (fn (g)
                 (filter (fn (v) (= 0 (graph-out-degree g v)))
                         (graph-vertices g))))

; graph-density: Calculate graph density (edges / max possible edges)
; For directed graph: e / (n * (n-1))
(graph-density (fn (g)
                   (let ((n (length (graph-vertices g)))
                         (e (graph-edge-count g)))
                        (if (<= n 1)
                            0
                            (/ e (* n (- n 1)))))))

; graph-transpose: Transpose graph (reverse all edges) - alias for graph-reverse
(graph-transpose graph-reverse)

; graph-articulation-points: Find articulation points (cut vertices)
; Vertices whose removal disconnects the graph (for undirected graphs)
; Simplified version using connectivity test
(graph-articulation-points (fn (g)
                               (let ((vertices (graph-vertices g)))
                                    (filter (fn (v)
                                                ; Check if removing v disconnects graph
                                                (let ((remaining (filter (fn (u) (not (= u v))) vertices)))
                                                     (if (null? remaining)
                                                         #f
                                                         (let ((subg (graph-subgraph g remaining)))
                                                              (not (graph-connected? subg))))))
                                            vertices))))

; graph-bridges: Find bridge edges (edges whose removal disconnects graph)
; Simplified version for undirected graphs
(graph-bridges (fn (g)
                   (let ((edges (graph-edges g)))
                        ; For undirected, only check each edge once
                        (let ((unique-edges (filter (fn (e)
                                                        (< (car e) (car (cdr e))))
                                                    edges)))
                             (filter (fn (e)
                                         (let ((u (car e))
                                               (v (car (cdr e)))
                                               (g-removed (graph-remove-undirected-edge g u v)))
                                              (not (graph-connected? g-removed))))
                                     unique-edges)))))

; --- Module Exports ---
; (see exports.ss for exported symbols)
