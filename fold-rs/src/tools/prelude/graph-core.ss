; ============================================================
; Graph - Core Operations
; Graph construction, vertex/edge manipulation, basic queries
; Part of graph.ss module
; ============================================================

; Graphs are represented as dictionaries mapping vertices to neighbor lists
; This is an adjacency list representation

; graph-new: Create empty graph
; () => {}
(graph-new (fn () '()))

; graph-add-vertex: Add a vertex to the graph
; If vertex already exists, returns graph unchanged
; (graph-add-vertex {} 'a) => {a: []}
(graph-add-vertex (fn (g v)
                      (if (dict-has? g v) g (dict-set g v '()))))

; graph-add-edge: Add directed edge from u to v
; Creates vertices if they don't exist
; (graph-add-edge {} 'a 'b) => {a: [b], b: []}
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

; graph-remove-edge: Remove directed edge from u to v
; (graph-remove-edge {a: [b c]} 'a 'b) => {a: [c]}
(graph-remove-edge (fn (g u v)
                       (if (dict-has? g u)
                           (dict-set g u (filter (fn (n) (not (= n v)))
                                                 (dict-get g u '())))
                           g)))

; graph-remove-undirected-edge: Remove undirected edge (both directions)
(graph-remove-undirected-edge (fn (g u v)
                                  (graph-remove-edge (graph-remove-edge g u v) v u)))

; graph-neighbors: Get neighbors of vertex
; (graph-neighbors {a: [b c]} 'a) => (b c)
(graph-neighbors (fn (g v)
                     (dict-get g v '())))

; graph-vertices: Get all vertices in graph
; (graph-vertices {a: [b], b: [c]}) => (a b)
(graph-vertices dict-keys)

; graph-edges: Get all edges as list of pairs (u v)
; (graph-edges {a: [b c], b: [c]}) => ((a b) (a c) (b c))
(graph-edges (fn (g)
                 (let ((vertices (graph-vertices g)))
                      (foldl (fn (acc v)
                                 (append acc
                                         (map (fn (n) (list v n))
                                              (graph-neighbors g v))))
                             '()
                             vertices))))

; graph-edge-count: Count total edges in graph
(graph-edge-count (fn (g)
                      (foldl (fn (acc v) (+ acc (length (graph-neighbors g v))))
                             0
                             (graph-vertices g))))

; graph-has-vertex?: Check if vertex exists in graph
; (graph-has-vertex? {a: [b]} 'a) => #t
(graph-has-vertex? (fn (g v)
                       (dict-has? g v)))

; graph-has-edge?: Check if directed edge exists from u to v
; (graph-has-edge? {a: [b]} 'a 'b) => #t
(graph-has-edge? (fn (g u v)
                     (member? v (graph-neighbors g u))))

; graph-out-degree: Get out-degree of vertex (number of outgoing edges)
; (graph-out-degree {a: [b c]} 'a) => 2
(graph-out-degree (fn (g v)
                      (length (graph-neighbors g v))))

; graph-degree: Alias for out-degree
(graph-degree graph-out-degree)

; graph-in-degree: Get in-degree of vertex (number of incoming edges)
; Requires scanning all vertices
; (graph-in-degree {a: [b], c: [b]} 'b) => 2
(graph-in-degree (fn (g v)
                     (foldl (fn (acc u)
                                (if (member? v (graph-neighbors g u))
                                    (+ acc 1)
                                    acc))
                            0
                            (graph-vertices g))))

; graph-vertex-count: Count vertices in graph
(graph-vertex-count (fn (g)
                        (length (graph-vertices g))))

; graph-empty?: Check if graph has no vertices
(graph-empty? (fn (g)
                  (null? (graph-vertices g))))

; graph-from-edges: Build graph from list of edges
; (graph-from-edges '((a b) (b c) (a c))) => {a: [c b], b: [c], c: []}
(graph-from-edges (fn (edges)
                      (foldl (fn (g edge)
                                 (graph-add-edge g (car edge) (car (cdr edge))))
                             (graph-new)
                             edges)))

; graph-from-undirected-edges: Build undirected graph from edges
(graph-from-undirected-edges (fn (edges)
                                 (foldl (fn (g edge)
                                            (graph-add-undirected-edge g (car edge) (car (cdr edge))))
                                        (graph-new)
                                        edges)))

; graph-reverse: Reverse all edges in a directed graph
; (graph-reverse {a: [b], b: [c]}) => {a: [], b: [a], c: [b]}
(graph-reverse (fn (g)
                   (let ((vertices (graph-vertices g))
                         (edges (graph-edges g)))
                        (foldl (fn (rev edge)
                                   (graph-add-edge rev (car (cdr edge)) (car edge)))
                               (foldl (fn (rev v) (graph-add-vertex rev v))
                                      (graph-new)
                                      vertices)
                               edges))))

; graph-subgraph: Extract subgraph containing only specified vertices
; (graph-subgraph {a: [b c], b: [c], c: []} '(a b)) => {a: [b], b: []}
(graph-subgraph (fn (g verts)
                    (foldl (fn (sub v)
                               (if (member? v verts)
                                   (dict-set sub v
                                             (filter (fn (n) (member? n verts))
                                                     (graph-neighbors g v)))
                                   sub))
                           (graph-new)
                           verts)))

; --- Module Exports ---
; (see exports.ss for exported symbols)
