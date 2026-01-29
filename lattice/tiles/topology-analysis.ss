(load "lattice/tiles/core.ss")
(load "lattice/topology/simplicial-complex.ss")
(load "lattice/topology/homology.ss")

(doc 'module 'tiles/topology-analysis)
(doc 'description "Topological analysis of game boards using simplicial homology.
Detects bottlenecks, holes in terrain, and critical corridors for strategic pathfinding.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'tier 2)
(doc 'dependencies '(tiles/core topology/simplicial-complex topology/homology))

(doc 'note "Key concepts:
  β₀ (Betti-0) = number of connected walkable regions
  β₁ (Betti-1) = number of holes (impassable islands surrounded by walkable terrain)
  Critical edge = edge whose removal increases β₀ (disconnects regions)
  Bottleneck = narrow corridor (few edges connecting large regions)")

(doc 'section 'board-to-complex)

(doc board->simplicial-complex 'export #t)
(doc board->simplicial-complex 'type '(-> Board (-> Coord (List Coord)) SC))
(doc board->simplicial-complex 'description "Convert walkable terrain to a simplicial complex.
Vertices = walkable tiles, Edges = adjacent walkable tiles, Faces = filled 2×2 squares.
The neighbor-fn determines adjacency (e.g., square-neighbors-ortho, hex-neighbors).
Adding 2-simplices (faces) for filled squares ensures β₁ counts terrain holes, not graph cycles.")
(define (board->simplicial-complex board neighbor-fn)
  (let* ([walkable-coords (filter (lambda (c)
                                    (let ([tile (board-get board c)])
                                      (and tile (tile-walkable? tile))))
                                  (board-coords board))]
         ;; Build index for O(1) membership check
         [walkable-set (coords->set walkable-coords)]
         ;; Create vertices (0-simplices)
         [vertices (map (lambda (c) (make-simplex (list (coord->vertex-id c))))
                        walkable-coords)]
         ;; Create edges (1-simplices) for adjacent walkable pairs
         [edges (collect-edges walkable-coords walkable-set neighbor-fn)]
         ;; Create faces (2-simplices) for filled 2×2 squares
         ;; This collapses graph cycles, making β₁ count terrain holes correctly
         [faces (collect-square-faces walkable-coords walkable-set neighbor-fn)])
    (sc-from-simplices (append vertices edges faces))))

(define (coords->set coords)
  (doc 'description "Build hashtable set for O(1) membership")
  (let ([ht (make-hashtable coord-hash coord-equal?)])
    (for-each (lambda (c) (hashtable-set! ht c #t)) coords)
    ht))

(define (coord-in-set? set c)
  (hashtable-ref set c #f))

(define (coord->vertex-id c)
  (doc 'description "Convert coordinate to unique vertex identifier for simplicial complex")
  ;; Use a list as vertex ID (works with generic<? comparison in simplicial-complex)
  (list (coord-x c) (coord-y c)))

(define (vertex-id->coord v)
  (doc 'description "Convert vertex ID back to coordinate")
  (coord (car v) (cadr v)))

(define (collect-edges coords walkable-set neighbor-fn)
  (doc 'description "Collect all edges between adjacent walkable tiles")
  (let ([edges '()]
        [seen (make-hashtable equal-hash equal?)])
    (for-each
      (lambda (c)
        (let ([v1 (coord->vertex-id c)])
          (for-each
            (lambda (n)
              (when (coord-in-set? walkable-set n)
                (let* ([v2 (coord->vertex-id n)]
                       [edge-key (if (vertex<? v1 v2) (cons v1 v2) (cons v2 v1))])
                  (unless (hashtable-ref seen edge-key #f)
                    (hashtable-set! seen edge-key #t)
                    (set! edges (cons (make-simplex (list v1 v2)) edges))))))
            (neighbor-fn c))))
      coords)
    edges))

(define (vertex<? v1 v2)
  (doc 'description "Ordering on vertex IDs for canonical edge representation")
  (or (< (car v1) (car v2))
      (and (= (car v1) (car v2))
           (< (cadr v1) (cadr v2)))))

(define (collect-square-faces coords walkable-set neighbor-fn)
  (doc 'description "Collect 2-simplices (triangular faces) for filled 2×2 squares.
For each coordinate (x,y), check if (x,y), (x+1,y), (x,y+1), (x+1,y+1) are all walkable
AND mutually adjacent. If so, triangulate the square into 2 triangles.
This 'fills in' solid walkable regions so β₁ counts terrain holes, not graph cycles.")
  (let ([faces '()]
        [seen (make-hashtable equal-hash equal?)])
    (for-each
      (lambda (c)
        (let* ([x (coord-x c)]
               [y (coord-y c)]
               ;; The 4 corners of a potential 2×2 square with c at top-left
               [c00 c]                        ; (x, y)
               [c10 (coord (+ x 1) y)]        ; (x+1, y)
               [c01 (coord x (+ y 1))]        ; (x, y+1)
               [c11 (coord (+ x 1) (+ y 1))]) ; (x+1, y+1)
          ;; Check if all 4 corners are walkable
          (when (and (coord-in-set? walkable-set c00)
                     (coord-in-set? walkable-set c10)
                     (coord-in-set? walkable-set c01)
                     (coord-in-set? walkable-set c11))
            ;; Check if all 4 edges exist (corners are mutually adjacent)
            ;; For square grid with ortho neighbors: c00-c10, c00-c01, c10-c11, c01-c11
            (when (and (coords-adjacent? c00 c10 neighbor-fn)
                       (coords-adjacent? c00 c01 neighbor-fn)
                       (coords-adjacent? c10 c11 neighbor-fn)
                       (coords-adjacent? c01 c11 neighbor-fn))
              ;; Create unique key for this square (use top-left corner)
              (let ([square-key (list x y)])
                (unless (hashtable-ref seen square-key #f)
                  (hashtable-set! seen square-key #t)
                  ;; Triangulate: split square into 2 triangles along diagonal
                  ;; Triangle 1: (x,y), (x+1,y), (x+1,y+1)
                  ;; Triangle 2: (x,y), (x,y+1), (x+1,y+1)
                  (let ([v00 (coord->vertex-id c00)]
                        [v10 (coord->vertex-id c10)]
                        [v01 (coord->vertex-id c01)]
                        [v11 (coord->vertex-id c11)])
                    (set! faces (cons (make-simplex (list v00 v10 v11)) faces))
                    (set! faces (cons (make-simplex (list v00 v01 v11)) faces)))))))))
      coords)
    faces))

(define (coords-adjacent? c1 c2 neighbor-fn)
  (doc 'description "Check if two coordinates are adjacent according to neighbor-fn")
  (memp (lambda (n) (coord-equal? n c2)) (neighbor-fn c1)))

(doc 'section 'topological-analysis)

(doc board-betti-numbers 'export #t)
(doc board-betti-numbers 'type '(-> Board (-> Coord (List Coord)) (List Integer)))
(doc board-betti-numbers 'description "Compute Betti numbers of walkable terrain.
Returns (β₀ β₁) where:
  β₀ = number of connected walkable regions
  β₁ = number of holes (impassable islands completely surrounded by walkable terrain)")
(define (board-betti-numbers board neighbor-fn)
  (let ([sc (board->simplicial-complex board neighbor-fn)])
    (sc-betti-numbers sc)))

(doc board-connected-regions 'export #t)
(doc board-connected-regions 'type '(-> Board (-> Coord (List Coord)) Integer))
(doc board-connected-regions 'description "Count connected walkable regions (β₀)")
(define (board-connected-regions board neighbor-fn)
  (let ([sc (board->simplicial-complex board neighbor-fn)])
    (sc-betti sc 0)))

(doc board-terrain-holes 'export #t)
(doc board-terrain-holes 'type '(-> Board (-> Coord (List Coord)) Integer))
(doc board-terrain-holes 'description "Count holes in terrain (β₁).
A hole is an impassable island completely surrounded by walkable terrain.
Strategic significance: units must path around these obstacles.")
(define (board-terrain-holes board neighbor-fn)
  (let ([sc (board->simplicial-complex board neighbor-fn)])
    (sc-betti sc 1)))

(doc 'section 'bottleneck-detection)

(doc board-critical-edges 'export #t)
(doc board-critical-edges 'type '(-> Board (-> Coord (List Coord)) (List (Pair Coord Coord))))
(doc board-critical-edges 'description "Find critical edges (bridges) whose removal disconnects regions.
These are strategic chokepoints - controlling them controls map access.
Returns list of coordinate pairs representing critical passages.
Uses Tarjan's bridge-finding algorithm: O(V + E) time complexity.")
(define (board-critical-edges board neighbor-fn)
  (find-bridges-tarjan board neighbor-fn))

(doc 'note "Tarjan's Bridge-Finding Algorithm:
An edge (u,v) is a bridge iff there is no back edge from v's subtree to u or above.
We track:
  - disc[v]: discovery time when v was first visited
  - low[v]: minimum discovery time reachable from v's subtree
Edge (u,v) is a bridge when low[v] > disc[u].")

(define (find-bridges-tarjan board neighbor-fn)
  (doc 'description "Find all bridges using Tarjan's algorithm in O(V+E) time")
  (let* ([walkable-coords (filter (lambda (c)
                                    (let ([tile (board-get board c)])
                                      (and tile (tile-walkable? tile))))
                                  (board-coords board))]
         [n (length walkable-coords)]
         ;; Map coords to indices for array access
         [coord->idx (make-hashtable coord-hash coord-equal?)]
         [idx->coord (make-vector n)])
    ;; Build index mappings
    (let build-idx ([coords walkable-coords] [i 0])
      (unless (null? coords)
        (hashtable-set! coord->idx (car coords) i)
        (vector-set! idx->coord i (car coords))
        (build-idx (cdr coords) (+ i 1))))

    ;; Build adjacency list (only walkable neighbors)
    (let* ([adj (make-vector n '())]
           [_ (for-each
                (lambda (c)
                  (let ([i (hashtable-ref coord->idx c #f)])
                    (when i
                      (let ([neighbors (filter-map
                                         (lambda (nc)
                                           (hashtable-ref coord->idx nc #f))
                                         (neighbor-fn c))])
                        (vector-set! adj i neighbors)))))
                walkable-coords)]
           ;; DFS state
           [disc (make-vector n -1)]      ; discovery time
           [low (make-vector n -1)]       ; lowest reachable discovery time
           [parent (make-vector n -1)]    ; parent in DFS tree
           [time-counter (list 0)]        ; mutable counter (boxed in list)
           [bridges '()])                 ; result accumulator

      ;; DFS function
      (letrec ([dfs
                (lambda (u)
                  (let ([current-time (car time-counter)])
                    ;; Set discovery time and low value
                    (vector-set! disc u current-time)
                    (vector-set! low u current-time)
                    (set-car! time-counter (+ current-time 1))

                    ;; Visit all neighbors
                    (for-each
                      (lambda (v)
                        (cond
                          ;; Not visited yet - tree edge
                          [(= (vector-ref disc v) -1)
                           (vector-set! parent v u)
                           (dfs v)
                           ;; Update low[u] from child
                           (vector-set! low u (min (vector-ref low u)
                                                   (vector-ref low v)))
                           ;; Check if (u,v) is a bridge
                           (when (> (vector-ref low v) (vector-ref disc u))
                             (set! bridges
                               (cons (cons (vector-ref idx->coord u)
                                           (vector-ref idx->coord v))
                                     bridges)))]
                          ;; Back edge (not to parent)
                          [(not (= v (vector-ref parent u)))
                           (vector-set! low u (min (vector-ref low u)
                                                   (vector-ref disc v)))]))
                      (vector-ref adj u))))])

        ;; Run DFS from each unvisited vertex (handles disconnected components)
        (let run-dfs ([i 0])
          (when (< i n)
            (when (= (vector-ref disc i) -1)
              (dfs i))
            (run-dfs (+ i 1))))

        bridges))))

(doc board-bottleneck-score 'export #t)
(doc board-bottleneck-score 'type '(-> Board (-> Coord (List Coord)) Coord Number))
(doc board-bottleneck-score 'description "Compute bottleneck score for a tile.
Higher score = more critical position. Score is based on how many critical
edges the tile participates in, weighted by the sizes of regions it connects.")
(define (board-bottleneck-score board neighbor-fn tile-coord)
  (let ([critical (board-critical-edges board neighbor-fn)])
    ;; Count how many critical edges involve this tile
    (length (filter (lambda (edge)
                      (or (coord-equal? (car edge) tile-coord)
                          (coord-equal? (cdr edge) tile-coord)))
                    critical))))

(doc 'section 'strategic-analysis)

(doc board-topology-summary 'export #t)
(doc board-topology-summary 'type '(-> Board (-> Coord (List Coord)) Void))
(doc board-topology-summary 'description "Print strategic topology summary of a board")
(define (board-topology-summary board neighbor-fn)
  (let* ([sc (board->simplicial-complex board neighbor-fn)]
         [betti (sc-betti-numbers sc)]
         [critical (board-critical-edges board neighbor-fn)]
         [total-tiles (length (board-coords board))]
         [walkable (length (filter (lambda (c)
                                     (let ([t (board-get board c)])
                                       (and t (tile-walkable? t))))
                                   (board-coords board)))])
    (printf "~n=== Board Topology Summary ===~n")
    (printf "Total tiles:        ~a~n" total-tiles)
    (printf "Walkable tiles:     ~a~n" walkable)
    (printf "Walkable ratio:     ~a%~n" (round (* 100 (/ walkable total-tiles))))
    (printf "~n--- Homology ---~n")
    (printf "Connected regions:  ~a (β₀)~n" (if (null? betti) 0 (car betti)))
    (printf "Terrain holes:      ~a (β₁)~n" (if (< (length betti) 2) 0 (cadr betti)))
    (printf "~n--- Strategic Analysis ---~n")
    (printf "Critical edges:     ~a~n" (length critical))
    (when (and (not (null? critical)) (<= (length critical) 10))
      (printf "Chokepoints:~n")
      (for-each
        (lambda (edge)
          (printf "  (~a,~a) ↔ (~a,~a)~n"
                  (coord-x (car edge)) (coord-y (car edge))
                  (coord-x (cdr edge)) (coord-y (cdr edge))))
        critical))
    (printf "===============================~n")))

(doc board-is-connected? 'export #t)
(doc board-is-connected? 'type '(-> Board (-> Coord (List Coord)) Boolean))
(doc board-is-connected? 'description "Check if all walkable terrain is connected (β₀ = 1)")
(define (board-is-connected? board neighbor-fn)
  (= (board-connected-regions board neighbor-fn) 1))

(doc board-has-chokepoints? 'export #t)
(doc board-has-chokepoints? 'type '(-> Board (-> Coord (List Coord)) Boolean))
(doc board-has-chokepoints? 'description "Check if board has any critical edges (strategic chokepoints)")
(define (board-has-chokepoints? board neighbor-fn)
  (not (null? (board-critical-edges board neighbor-fn))))
