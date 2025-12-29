;;; fabric/stitches/fp/graph.ss — Functional Graph Library
;;;
;;; Pure functional graph data structures and algorithms.
;;; Supports both directed and undirected graphs with labeled edges.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Adjacency list representation
;;;   - Graph construction (add/remove vertices and edges)
;;;   - Traversals (BFS, DFS)
;;;   - Path finding (shortest path, all paths)
;;;   - Topological sort
;;;   - Cycle detection
;;;   - Connected components
;;;   - Graph properties (in-degree, out-degree, neighbors)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "prelude.ss")
(load "fp/combinators.ss")

;;; ============================================================
;;; Graph Type
;;; ============================================================
;;;
;;; A graph is represented as an adjacency list:
;;;   (graph directed? vertices edges)
;;; where:
;;;   - directed? is a boolean
;;;   - vertices is a list of vertex labels
;;;   - edges is an association list: ((from . ((to . weight) ...)) ...)

;;; make-graph : Boolean -> Graph
;;; Create an empty graph (directed or undirected).
(define (make-graph directed?)
  (list 'graph directed? '() '()))

;;; graph? : Any -> Boolean
(define (graph? x)
  (and (pair? x) (eq? (car x) 'graph)))

;;; graph-directed? : Graph -> Boolean
(define (graph-directed? g)
  (list-ref g 1))

;;; graph-vertices : Graph -> List Vertex
(define (graph-vertices g)
  (list-ref g 2))

;;; graph-edges : Graph -> AdjList
(define (graph-edges g)
  (list-ref g 3))

;;; empty-graph : Graph
;;; Empty directed graph.
(define empty-graph (make-graph #t))

;;; empty-ugraph : Graph
;;; Empty undirected graph.
(define empty-ugraph (make-graph #f))

;;; ============================================================
;;; Graph Construction
;;; ============================================================

;;; graph-add-vertex : Graph -> Vertex -> Graph
;;; Add a vertex to the graph.
(define (graph-add-vertex g v)
  (if (member v (graph-vertices g))
      g  ; Already exists
      (list 'graph
            (graph-directed? g)
            (cons v (graph-vertices g))
            (graph-edges g))))

;;; graph-add-vertices : Graph -> List Vertex -> Graph
;;; Add multiple vertices.
(define (graph-add-vertices g vs)
  (fold-left graph-add-vertex g vs))

;;; graph-remove-vertex : Graph -> Vertex -> Graph
;;; Remove a vertex and all its edges.
(define (graph-remove-vertex g v)
  (let* ([new-verts (filter (lambda (x) (not (equal? x v))) (graph-vertices g))]
         [new-edges (filter (lambda (adj) (not (equal? (car adj) v)))
                            (graph-edges g))]
         ;; Also remove edges TO this vertex
         [new-edges (map (lambda (adj)
                                 (cons (car adj)
                                       (filter (lambda (e) (not (equal? (car e) v)))
                                               (cdr adj))))
                         new-edges)])
        (list 'graph (graph-directed? g) new-verts new-edges)))

;;; graph-add-edge : Graph -> Vertex -> Vertex -> Number -> Graph
;;; Add an edge with weight (default 1).
(define (graph-add-edge g from to weight)
  (let* ([g (graph-add-vertex (graph-add-vertex g from) to)]
         [edges (graph-edges g)]
         [from-edges (assoc from edges)]
         [new-from-edges (if from-edges
                             (cons (cons to weight) (cdr from-edges))
                             (list (cons to weight)))]
         [new-edges (cons (cons from new-from-edges)
                          (filter (lambda (adj) (not (equal? (car adj) from)))
                                  edges))])
        (if (graph-directed? g)
            (list 'graph #t (graph-vertices g) new-edges)
            ;; For undirected, add reverse edge too
            (let* ([to-edges (assoc to new-edges)]
                   [new-to-edges (if to-edges
                                     (cons (cons from weight) (cdr to-edges))
                                     (list (cons from weight)))]
                   [final-edges (cons (cons to new-to-edges)
                                      (filter (lambda (adj) (not (equal? (car adj) to)))
                                              new-edges))])
                  (list 'graph #f (graph-vertices g) final-edges)))))

;;; graph-add-edge* : Graph -> Vertex -> Vertex -> Graph
;;; Add an edge with default weight 1.
(define (graph-add-edge* g from to)
  (graph-add-edge g from to 1))

;;; graph-add-edges : Graph -> List (from to weight) -> Graph
;;; Add multiple edges.
(define (graph-add-edges g edge-list)
  (fold-left (lambda (g e)
                     (graph-add-edge g (car e) (cadr e)
                                     (if (null? (cddr e)) 1 (caddr e))))
             g edge-list))

;;; graph-remove-edge : Graph -> Vertex -> Vertex -> Graph
;;; Remove an edge.
(define (graph-remove-edge g from to)
  (let* ([edges (graph-edges g)]
         [from-edges (assoc from edges)]
         [new-from-edges (if from-edges
                             (filter (lambda (e) (not (equal? (car e) to)))
                                     (cdr from-edges))
                             '())]
         [new-edges (cons (cons from new-from-edges)
                          (filter (lambda (adj) (not (equal? (car adj) from)))
                                  edges))])
        (if (graph-directed? g)
            (list 'graph #t (graph-vertices g) new-edges)
            ;; For undirected, remove reverse edge too
            (let* ([to-edges (assoc to new-edges)]
                   [new-to-edges (if to-edges
                                     (filter (lambda (e) (not (equal? (car e) from)))
                                             (cdr to-edges))
                                     '())]
                   [final-edges (cons (cons to new-to-edges)
                                      (filter (lambda (adj) (not (equal? (car adj) to)))
                                              new-edges))])
                  (list 'graph #f (graph-vertices g) final-edges)))))

;;; ============================================================
;;; Graph Queries
;;; ============================================================

;;; graph-has-vertex? : Graph -> Vertex -> Boolean
(define (graph-has-vertex? g v)
  (not (not (member v (graph-vertices g)))))

;;; graph-has-edge? : Graph -> Vertex -> Vertex -> Boolean
(define (graph-has-edge? g from to)
  (let ([from-edges (assoc from (graph-edges g))])
       (if from-edges
           (not (not (assoc to (cdr from-edges))))
           #f)))

;;; graph-edge-weight : Graph -> Vertex -> Vertex -> Maybe Number
(define (graph-edge-weight g from to)
  (let ([from-edges (assoc from (graph-edges g))])
       (if from-edges
           (let ([edge (assoc to (cdr from-edges))])
                (if edge (just (cdr edge)) nothing))
           nothing)))

;;; graph-neighbors : Graph -> Vertex -> List Vertex
;;; Get outgoing neighbors of a vertex.
(define (graph-neighbors g v)
  (let ([v-edges (assoc v (graph-edges g))])
       (if v-edges
           (map car (cdr v-edges))
           '())))

;;; graph-neighbors-weighted : Graph -> Vertex -> List (Vertex . Weight)
;;; Get outgoing neighbors with weights.
(define (graph-neighbors-weighted g v)
  (let ([v-edges (assoc v (graph-edges g))])
       (if v-edges (cdr v-edges) '())))

;;; graph-out-degree : Graph -> Vertex -> Int
(define (graph-out-degree g v)
  (length (graph-neighbors g v)))

;;; graph-in-degree : Graph -> Vertex -> Int
;;; Count incoming edges to a vertex.
(define (graph-in-degree g v)
  (fold-left (lambda (count adj)
                     (if (assoc v (cdr adj))
                         (+ count 1)
                         count))
             0
             (graph-edges g)))

;;; graph-degree : Graph -> Vertex -> Int
;;; Total degree (in + out for directed, just neighbors for undirected).
(define (graph-degree g v)
  (if (graph-directed? g)
      (+ (graph-in-degree g v) (graph-out-degree g v))
      (graph-out-degree g v)))

;;; graph-vertex-count : Graph -> Int
(define (graph-vertex-count g)
  (length (graph-vertices g)))

;;; graph-edge-count : Graph -> Int
(define (graph-edge-count g)
  (let ([total (fold-left (lambda (count adj)
                                  (+ count (length (cdr adj))))
                          0
                          (graph-edges g))])
       (if (graph-directed? g)
           total
           (quotient total 2))))  ; Undirected counts each edge twice

;;; ============================================================
;;; Graph Traversals
;;; ============================================================

;;; graph-dfs : Graph -> Vertex -> List Vertex
;;; Depth-first search from a starting vertex.
(define (graph-dfs g start)
  (let loop ([stack (list start)] [visited '()] [result '()])
       (if (null? stack)
           (reverse result)
           (let ([v (car stack)])
                (if (member v visited)
                    (loop (cdr stack) visited result)
                    (let ([neighbors (graph-neighbors g v)])
                         (loop (append neighbors (cdr stack))
                               (cons v visited)
                               (cons v result))))))))

;;; graph-dfs-full : Graph -> List Vertex
;;; DFS visiting all vertices (handles disconnected graphs).
(define (graph-dfs-full g)
  (let loop ([remaining (graph-vertices g)] [visited '()] [result '()])
       (if (null? remaining)
           (reverse result)
           (let ([v (car remaining)])
                (if (member v visited)
                    (loop (cdr remaining) visited result)
                    ;; Do DFS but pass in already-visited vertices
                    (let ([component (graph-dfs-from g v visited)])
                         (loop (cdr remaining)
                               (append component visited)
                               (append (reverse component) result))))))))

;;; graph-dfs-from : Graph -> Vertex -> List Vertex -> List Vertex
;;; DFS from vertex, respecting already visited vertices.
(define (graph-dfs-from g start already-visited)
  (let loop ([stack (list start)] [visited already-visited] [result '()])
       (if (null? stack)
           (reverse result)
           (let ([v (car stack)])
                (if (member v visited)
                    (loop (cdr stack) visited result)
                    (let ([neighbors (graph-neighbors g v)])
                         (loop (append neighbors (cdr stack))
                               (cons v visited)
                               (cons v result))))))))

;;; graph-bfs : Graph -> Vertex -> List Vertex
;;; Breadth-first search from a starting vertex.
(define (graph-bfs g start)
  (let loop ([queue (list start)] [visited '()] [result '()])
       (if (null? queue)
           (reverse result)
           (let ([v (car queue)])
                (if (member v visited)
                    (loop (cdr queue) visited result)
                    (let ([neighbors (filter (lambda (n) (not (member n visited)))
                                             (graph-neighbors g v))])
                         (loop (append (cdr queue) neighbors)
                               (cons v visited)
                               (cons v result))))))))

;;; graph-bfs-levels : Graph -> Vertex -> List (List Vertex)
;;; BFS returning vertices grouped by level/distance.
(define (graph-bfs-levels g start)
  (let loop ([current-level (list start)] [visited (list start)] [result '()])
       (if (null? current-level)
           (reverse result)
           (let* ([next-level (fold-left
                               (lambda (acc v)
                                       (append acc
                                               (filter (lambda (n) (not (member n visited)))
                                                       (graph-neighbors g v))))
                               '()
                               current-level)]
                  [unique-next (let dedup ([lst next-level] [seen visited])
                                    (if (null? lst)
                                        '()
                                        (if (member (car lst) seen)
                                            (dedup (cdr lst) seen)
                                            (cons (car lst)
                                                  (dedup (cdr lst) (cons (car lst) seen))))))])
                 (loop unique-next
                       (append unique-next visited)
                       (cons current-level result))))))

;;; ============================================================
;;; Path Finding
;;; ============================================================

;;; graph-path? : Graph -> Vertex -> Vertex -> Boolean
;;; Check if a path exists between two vertices.
(define (graph-path? g from to)
  (not (not (member to (graph-bfs g from)))))

;;; graph-shortest-path : Graph -> Vertex -> Vertex -> Maybe (List Vertex)
;;; Find shortest path (by edge count, not weight) using BFS.
(define (graph-shortest-path g from to)
  (if (equal? from to)
      (just (list from))
      (let loop ([queue (list (list from))]
                 [visited (list from)])
           (if (null? queue)
               nothing
               (let* ([path (car queue)]
                      [current (car path)]
                      [neighbors (filter (lambda (n) (not (member n visited)))
                                         (graph-neighbors g current))])
                     (let check ([ns neighbors])
                          (if (null? ns)
                              (loop (append (cdr queue)
                                            (map (lambda (n) (cons n path)) neighbors))
                                    (append neighbors visited))
                              (if (equal? (car ns) to)
                                  (just (reverse (cons to path)))
                                  (check (cdr ns))))))))))

;;; graph-all-paths : Graph -> Vertex -> Vertex -> Int -> List (List Vertex)
;;; Find all paths up to max-length (to avoid infinite loops).
(define (graph-all-paths g from to max-length)
  (let loop ([current from] [path (list from)] [length 0])
       (cond
        [(equal? current to) (list (reverse path))]
        [(>= length max-length) '()]
        [else
         (let ([neighbors (filter (lambda (n) (not (member n path)))
                                  (graph-neighbors g current))])
              (apply append
                     (map (lambda (n)
                                  (loop n (cons n path) (+ length 1)))
                          neighbors)))])))

;;; ============================================================
;;; Topological Sort
;;; ============================================================

;;; graph-topological-sort : Graph -> Maybe (List Vertex)
;;; Topological sort of a directed acyclic graph.
;;; Returns nothing if graph has a cycle.
(define (graph-topological-sort g)
  (if (not (graph-directed? g))
      nothing  ; Only for directed graphs
      (let* ([vertices (graph-vertices g)]
             [in-degrees (map (lambda (v) (cons v (graph-in-degree g v)))
                              vertices)]
             [zero-in (map car (filter (lambda (p) (= (cdr p) 0)) in-degrees))])
            (let loop ([queue zero-in]
                       [degrees in-degrees]
                       [result '()]
                       [count 0])
                 (if (null? queue)
                     (if (= count (length vertices))
                         (just (reverse result))
                         nothing)  ; Cycle detected
                     (let* ([v (car queue)]
                            [neighbors (graph-neighbors g v)]
                            [new-degrees (map (lambda (d)
                                                      (if (member (car d) neighbors)
                                                          (cons (car d) (- (cdr d) 1))
                                                          d))
                                              degrees)]
                            [new-zeros (filter (lambda (n)
                                                       (let ([deg (assoc n new-degrees)])
                                                            (and deg (= (cdr deg) 0)
                                                                 (not (member n result))
                                                                 (not (member n queue)))))
                                               neighbors)])
                           (loop (append (cdr queue) new-zeros)
                                 new-degrees
                                 (cons v result)
                                 (+ count 1))))))))

;;; ============================================================
;;; Cycle Detection
;;; ============================================================

;;; graph-has-cycle? : Graph -> Boolean
;;; Check if graph contains a cycle.
(define (graph-has-cycle? g)
  (if (graph-directed? g)
      ;; For directed graphs, try topological sort
      (nothing? (graph-topological-sort g))
      ;; For undirected graphs, use DFS with proper visited tracking
      (let loop ([remaining (graph-vertices g)] [visited '()])
           (if (null? remaining)
               #f
               (let ([v (car remaining)])
                    (if (member v visited)
                        (loop (cdr remaining) visited)
                        (let ([result (graph-cycle-dfs g v visited #f)])
                             (if (car result)  ; Found cycle
                                 #t
                                 (loop (cdr remaining) (cdr result))))))))))

;;; graph-cycle-dfs : Graph -> Vertex -> List Vertex -> Maybe Vertex -> (Boolean . List Vertex)
;;; Helper: DFS for cycle detection, returns (found-cycle? . updated-visited).
(define (graph-cycle-dfs g v visited parent)
  (let loop ([neighbors (graph-neighbors g v)] [vis (cons v visited)])
       (if (null? neighbors)
           (cons #f vis)  ; No cycle found, return updated visited
           (let ([n (car neighbors)])
                (cond
                 [(and parent (equal? n parent))
                  ;; Skip the edge we came from
                  (loop (cdr neighbors) vis)]
                 [(member n vis)
                  ;; Found a back edge - cycle!
                  (cons #t vis)]
                 [else
                  (let ([result (graph-cycle-dfs g n vis v)])
                       (if (car result)
                           result  ; Propagate cycle found
                           (loop (cdr neighbors) (cdr result))))])))))

;;; ============================================================
;;; Connected Components
;;; ============================================================

;;; graph-connected-components : Graph -> List (List Vertex)
;;; Find all connected components (for undirected graphs).
(define (graph-connected-components g)
  (let loop ([remaining (graph-vertices g)] [components '()])
       (if (null? remaining)
           (reverse components)
           (let* ([v (car remaining)]
                  [component (graph-bfs g v)]
                  [new-remaining (filter (lambda (x) (not (member x component)))
                                         remaining)])
                 (loop new-remaining (cons component components))))))

;;; graph-strongly-connected? : Graph -> Boolean
;;; Check if directed graph is strongly connected.
(define (graph-strongly-connected? g)
  (if (null? (graph-vertices g))
      #t
      (let* ([start (car (graph-vertices g))]
             [reachable (graph-bfs g start)])
            (= (length reachable) (length (graph-vertices g))))))

;;; ============================================================
;;; Graph Transformations
;;; ============================================================

;;; graph-reverse : Graph -> Graph
;;; Reverse all edges in a directed graph.
(define (graph-reverse g)
  (if (not (graph-directed? g))
      g  ; Undirected is symmetric
      (let ([reversed-edges
             (fold-left
              (lambda (acc adj)
                      (let ([from (car adj)])
                           (fold-left
                            (lambda (acc2 edge)
                                    (let* ([to (car edge)]
                                           [weight (cdr edge)]
                                           [existing (assoc to acc2)]
                                           [new-edges (if existing
                                                          (cons (cons from weight) (cdr existing))
                                                          (list (cons from weight)))])
                                          (cons (cons to new-edges)
                                                (filter (lambda (a) (not (equal? (car a) to)))
                                                        acc2))))
                            acc
                            (cdr adj))))
              '()
              (graph-edges g))])
           (list 'graph #t (graph-vertices g) reversed-edges))))

;;; graph-subgraph : Graph -> List Vertex -> Graph
;;; Create a subgraph with only the given vertices.
(define (graph-subgraph g vertices)
  (let* ([filtered-edges
          (filter (lambda (adj) (member (car adj) vertices))
                  (graph-edges g))]
         [filtered-edges
          (map (lambda (adj)
                       (cons (car adj)
                             (filter (lambda (e) (member (car e) vertices))
                                     (cdr adj))))
               filtered-edges)])
        (list 'graph (graph-directed? g) vertices filtered-edges)))

;;; graph-complement : Graph -> Graph
;;; Create complement graph (edge where there wasn't one, and vice versa).
(define (graph-complement g)
  (let* ([vertices (graph-vertices g)]
         [new-edges
          (map (lambda (v)
                       (cons v
                             (map (lambda (u) (cons u 1))
                                  (filter (lambda (u)
                                                  (and (not (equal? u v))
                                                       (not (graph-has-edge? g v u))))
                                          vertices))))
               vertices)])
        (list 'graph (graph-directed? g) vertices new-edges)))

;;; ============================================================
;;; Graph from Edges
;;; ============================================================

;;; edges->graph : List (from to) -> Graph
;;; Create directed graph from edge list.
(define (edges->graph edge-list)
  (graph-add-edges empty-graph
                   (map (lambda (e)
                                (if (null? (cddr e))
                                    (list (car e) (cadr e) 1)
                                    e))
                        edge-list)))

;;; edges->ugraph : List (from to) -> Graph
;;; Create undirected graph from edge list.
(define (edges->ugraph edge-list)
  (graph-add-edges empty-ugraph
                   (map (lambda (e)
                                (if (null? (cddr e))
                                    (list (car e) (cadr e) 1)
                                    e))
                        edge-list)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; graph-map-vertices : (Vertex -> Vertex') -> Graph -> Graph
;;; Map a function over vertex labels.
(define (graph-map-vertices f g)
  (let* ([new-vertices (map f (graph-vertices g))]
         [new-edges (map (lambda (adj)
                                 (cons (f (car adj))
                                       (map (lambda (e)
                                                    (cons (f (car e)) (cdr e)))
                                            (cdr adj))))
                         (graph-edges g))])
        (list 'graph (graph-directed? g) new-vertices new-edges)))

;;; graph-fold-vertices : (acc -> Vertex -> acc) -> acc -> Graph -> acc
;;; Fold over vertices.
(define (graph-fold-vertices f init g)
  (fold-left f init (graph-vertices g)))

;;; graph-fold-edges : (acc -> from -> to -> weight -> acc) -> acc -> Graph -> acc
;;; Fold over edges.
(define (graph-fold-edges f init g)
  (fold-left (lambda (acc adj)
                     (fold-left (lambda (acc2 edge)
                                        (f acc2 (car adj) (car edge) (cdr edge)))
                                acc
                                (cdr adj)))
             init
             (graph-edges g)))

;;; ============================================================
;;; Graph Display
;;; ============================================================

;;; graph->string : Graph -> String
;;; Convert graph to string representation.
(define (graph->string g)
  (string-append
   (if (graph-directed? g) "Directed" "Undirected")
   " Graph:
"
   "  Vertices: " (format-list (graph-vertices g)) "
"
   "  Edges:
"
   (apply string-append
          (map (lambda (adj)
                       (string-append
                        "    " (format-value (car adj)) " -> "
                        (format-list (map car (cdr adj))) "
"))
               (graph-edges g)))))

(define (format-value v)
  (cond
   [(symbol? v) (symbol->string v)]
   [(number? v) (number->string v)]
   [(string? v) v]
   [else "?"]))

(define (format-list lst)
  (string-append
   "["
   (let loop ([l lst] [first #t])
        (if (null? l)
            ""
            (string-append
             (if first "" ", ")
             (format-value (car l))
             (loop (cdr l) #f))))
   "]"))
