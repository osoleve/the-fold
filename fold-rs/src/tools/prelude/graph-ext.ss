; ============================================================
; Extended Graph Functions
; Canonical versions - no duplicates
; ============================================================

; graph-add-edge: Add directed edge to graph
; (graph-add-edge {} 'a 'b) => {a: [b]}
(graph-add-edge (fn (g u v)
                    (let ((g1 (graph-add-vertex g u)))
                         (let ((g2 (graph-add-vertex g1 v)))
                              (let ((neighbors (dict-get g2 u '())))
                                   (if (member? v neighbors)
                                       g2
                                       (dict-set g2 u (cons v neighbors))))))))

; graph-add-undirected-edge: Add undirected edge (both directions)
; (graph-add-undirected-edge {} 'a 'b) => {a: [b], b: [a]}
(graph-add-undirected-edge (fn (g u v)
                               (graph-add-edge (graph-add-edge g u v) v u)))

; graph-neighbors: Get neighbors of vertex
; (graph-neighbors {a: [b c]} 'a) => (b c)
(graph-neighbors (fn (g v)
                     (dict-get g v '())))

; graph-vertices: Get all vertices in graph
; (graph-vertices {a: [b], b: [c]}) => (a b)
(graph-vertices dict-keys)

; graph-has-edge?: Check if edge exists
; (graph-has-edge? {a: [b]} 'a 'b) => #t
(graph-has-edge? (fn (g u v)
                     (member? v (graph-neighbors g u))))

; graph-degree: Get degree of vertex (number of neighbors)
; (graph-degree {a: [b c]} 'a) => 2
(graph-degree (fn (g v)
                  (length (graph-neighbors g v))))

; graph-bfs: Breadth-first search from start vertex
; Returns list of vertices in BFS order
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
