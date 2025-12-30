; Graph algorithms - adjacency list representation
; Graphs are represented as dictionaries mapping vertices to neighbor lists

; graph-new: Create empty graph (as adjacency list dict)
(graph-new (fn () '()))

; graph-add-vertex: Add a vertex
(graph-add-vertex (fn (g v)
                      (if (dict-has? g v) g (dict-set g v '()))))

; graph-add-edge: Add directed edge from u to v
(graph-add-edge (fn (g u v)
                    (let ((g1 (graph-add-vertex g u)))
                         (let ((g2 (graph-add-vertex g1 v)))
                              (let ((neighbors (dict-get g2 u '())))
                                   (if (member? v neighbors)
                                       g2
                                       (dict-set g2 u (cons v neighbors))))))))

; graph-add-undirected-edge: Add undirected edge
(graph-add-undirected-edge (fn (g u v)
                               (graph-add-edge (graph-add-edge g u v) v u)))

; graph-neighbors: Get neighbors of vertex
(graph-neighbors (fn (g v)
                     (dict-get g v '())))

; graph-vertices: Get all vertices
(graph-vertices dict-keys)

; graph-has-edge?: Check if edge exists
(graph-has-edge? (fn (g u v)
                     (member? v (graph-neighbors g u))))

; graph-degree: Get degree of vertex
(graph-degree (fn (g v)
                  (length (graph-neighbors g v))))

; graph-bfs: Breadth-first search from start (returns list of visited vertices)
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

; graph-dfs: Depth-first search from start
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

; graph-path-exists?: Check if path exists from u to v
(graph-path-exists? (fn (g u v)
                        (member? v (graph-bfs g u))))

; graph-connected?: Check if graph is connected (for undirected graphs)
(graph-connected? (fn (g)
                      (let ((vertices (graph-vertices g)))
                           (if (null? vertices)
                               #t
                               (= (length (graph-bfs g (car vertices)))
                                  (length vertices))))))
