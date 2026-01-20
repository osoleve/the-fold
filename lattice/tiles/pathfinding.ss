(doc 'module 'tiles/pathfinding)
(doc 'description "Generic pathfinding algorithms: BFS, Dijkstra, A*")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'priority-queue)

(define (make-pqueue)
  (doc 'description "Create empty priority queue")
  (doc 'returns "Empty priority queue")
  '())

(define (pqueue-empty? pq)
  (doc 'description "Check if priority queue is empty")
  (null? pq))

(define (pqueue-insert pq priority value)
  (doc 'description "Insert value into priority queue with given priority")
  (doc 'param 'pq "Priority queue")
  (doc 'param 'priority "Numeric priority (lower = higher priority)")
  (doc 'param 'value "Value to insert")
  (doc 'returns "New priority queue with value inserted")
  (let loop ([lst pq])
       (cond
        [(null? lst) (list (cons priority value))]
        [(< priority (caar lst))
         (cons (cons priority value) lst)]
        [else
         (cons (car lst) (loop (cdr lst)))])))

(define (pqueue-pop pq)
  (doc 'description "Remove and return minimum priority element")
  (doc 'returns "Three values: value, priority, new-queue")
  (if (null? pq)
      (values #f #f #f)
      (values (cdar pq) (caar pq) (cdr pq))))

(doc 'section 'path-reconstruction)

(define (reconstruct-path came-from start goal)
  (doc 'description "Reconstruct path from came-from map")
  (doc 'type '(-> Hashtable Coord Coord (List Coord)))
  (doc 'returns "List of coordinates from start to goal, or #f if no path")
  (let loop ([current goal] [path '()])
       (if (equal? current start)
           (cons start path)
           (let ([prev (hashtable-ref came-from current #f)])
                (if prev
                    (loop prev (cons current path))
                    #f))))) ; No path found

(doc 'section 'bfs)

(define (bfs-path start goal neighbor-fn walkable-fn)
  (doc 'export #t)
  (doc 'description "Find shortest path using BFS (unweighted)")
  (doc 'type '(-> Coord Coord (-> Coord (List Coord)) (-> Coord Bool) (Maybe (List Coord))))
  (doc 'param 'start "Starting coordinate")
  (doc 'param 'goal "Goal coordinate")
  (doc 'param 'neighbor-fn "Function returning list of neighbors")
  (doc 'param 'walkable-fn "Predicate checking if coordinate is walkable")
  (doc 'returns "List of coordinates from start to goal, or #f if no path")
  (let ([queue (list start)]
        [visited (make-hashtable equal-hash equal?)]
        [came-from (make-hashtable equal-hash equal?)])
       (hashtable-set! visited start #t)
       (let loop ([q queue])
            (if (null? q)
                #f ; No path found
                (let ([current (car q)]
                      [rest-q (cdr q)])
                     (if (equal? current goal)
                         (reconstruct-path came-from start goal)
                         (let* ([neighbors (neighbor-fn current)]
                                [valid-neighbors
                                 (filter (lambda (n)
                                                 (and (walkable-fn n)
                                                      (not (hashtable-ref visited n #f))))
                                         neighbors)])
                               (for-each
                                (lambda (n)
                                        (hashtable-set! visited n #t)
                                        (hashtable-set! came-from n current))
                                valid-neighbors)
                               (loop (append rest-q valid-neighbors)))))))))

(doc 'section 'dijkstra)

(define (dijkstra-path start goal neighbor-fn cost-fn walkable-fn)
  (doc 'export #t)
  (doc 'description "Find shortest weighted path using Dijkstra's algorithm")
  (doc 'type '(-> Coord Coord (-> Coord (List Coord)) (-> Coord Number) (-> Coord Bool) (Maybe (List Coord))))
  (doc 'param 'start "Starting coordinate")
  (doc 'param 'goal "Goal coordinate")
  (doc 'param 'neighbor-fn "Function returning list of neighbors")
  (doc 'param 'cost-fn "Function returning movement cost for a coordinate")
  (doc 'param 'walkable-fn "Predicate checking if coordinate is walkable")
  (doc 'returns "List of coordinates from start to goal, or #f if no path")
  (let ([pq (pqueue-insert (make-pqueue) 0 start)]
        [cost-so-far (make-hashtable equal-hash equal?)]
        [came-from (make-hashtable equal-hash equal?)])
       (hashtable-set! cost-so-far start 0)
       (let loop ([queue pq])
            (if (pqueue-empty? queue)
                #f ; No path found
                (let-values ([(current priority new-queue) (pqueue-pop queue)])
                            (if (equal? current goal)
                                (reconstruct-path came-from start goal)
                                (let* ([current-cost (hashtable-ref cost-so-far current 0)]
                                       [neighbors (neighbor-fn current)]
                                       [valid-neighbors (filter walkable-fn neighbors)])
                                      (let process-neighbors ([ns valid-neighbors] [q new-queue])
                                           (if (null? ns)
                                               (loop q)
                                               (let* ([next (car ns)]
                                                      [new-cost (+ current-cost (cost-fn next))]
                                                      [old-cost (hashtable-ref cost-so-far next #f)])
                                                     (if (or (not old-cost) (< new-cost old-cost))
                                                         (begin
                                                          (hashtable-set! cost-so-far next new-cost)
                                                          (hashtable-set! came-from next current)
                                                          (process-neighbors
                                                           (cdr ns)
                                                           (pqueue-insert q new-cost next)))
                                                         (process-neighbors (cdr ns) q))))))))))))

(doc 'section 'astar)

(define (astar-path start goal neighbor-fn cost-fn heuristic-fn walkable-fn)
  (doc 'export #t)
  (doc 'description "Find shortest path using A* with heuristic. Heuristic must be admissible.")
  (doc 'type '(-> Coord Coord (-> Coord (List Coord)) (-> Coord Number) (-> Coord Coord Number) (-> Coord Bool) (Maybe (List Coord))))
  (doc 'param 'start "Starting coordinate")
  (doc 'param 'goal "Goal coordinate")
  (doc 'param 'neighbor-fn "Function returning list of neighbors")
  (doc 'param 'cost-fn "Function returning movement cost for a coordinate")
  (doc 'param 'heuristic-fn "Function (coord1, coord2) → estimated distance")
  (doc 'param 'walkable-fn "Predicate checking if coordinate is walkable")
  (doc 'returns "List of coordinates from start to goal, or #f if no path")
  (doc 'note "Heuristic must be admissible (never overestimate) for A* to be optimal")
  (let ([pq (pqueue-insert (make-pqueue) 0 start)]
        [cost-so-far (make-hashtable equal-hash equal?)]
        [came-from (make-hashtable equal-hash equal?)])
       (hashtable-set! cost-so-far start 0)
       (let loop ([queue pq])
            (if (pqueue-empty? queue)
                #f ; No path found
                (let-values ([(current priority new-queue) (pqueue-pop queue)])
                            (if (equal? current goal)
                                (reconstruct-path came-from start goal)
                                (let* ([current-cost (hashtable-ref cost-so-far current 0)]
                                       [neighbors (neighbor-fn current)]
                                       [valid-neighbors (filter walkable-fn neighbors)])
                                      (let process-neighbors ([ns valid-neighbors] [q new-queue])
                                           (if (null? ns)
                                               (loop q)
                                               (let* ([next (car ns)]
                                                      [new-cost (+ current-cost (cost-fn next))]
                                                      [old-cost (hashtable-ref cost-so-far next #f)])
                                                     (if (or (not old-cost) (< new-cost old-cost))
                                                         (let ([f-score (+ new-cost (heuristic-fn next goal))])
                                                              (hashtable-set! cost-so-far next new-cost)
                                                              (hashtable-set! came-from next current)
                                                              (process-neighbors
                                                               (cdr ns)
                                                               (pqueue-insert q f-score next)))
                                                         (process-neighbors (cdr ns) q))))))))))))

(doc 'section 'board-integration)

(define (board-walkable? board coord)
  (doc 'description "Check if a coordinate is walkable on the board")
  (doc 'type '(-> Board Coord Bool))
  (let ([tile (board-get board coord)])
       (and tile (tile-walkable? tile))))

(define (board-cost board coord)
  (doc 'description "Get movement cost for a coordinate on the board")
  (doc 'type '(-> Board Coord Number))
  (let ([tile (board-get board coord)])
       (if tile
           (tile-cost tile)
           999))) ; High cost for missing tiles

(define (find-path-bfs board start goal neighbor-fn)
  (doc 'export #t)
  (doc 'description "Find path on board using BFS")
  (doc 'type '(-> Board Coord Coord (-> Coord (List Coord)) (Maybe (List Coord))))
  (bfs-path start goal
            neighbor-fn
            (lambda (c) (board-walkable? board c))))

(define (find-path-dijkstra board start goal neighbor-fn)
  (doc 'export #t)
  (doc 'description "Find path on board using Dijkstra")
  (doc 'type '(-> Board Coord Coord (-> Coord (List Coord)) (Maybe (List Coord))))
  (dijkstra-path start goal
                 neighbor-fn
                 (lambda (c) (board-cost board c))
                 (lambda (c) (board-walkable? board c))))

(define (find-path-astar board start goal neighbor-fn heuristic-fn)
  (doc 'export #t)
  (doc 'description "Find path on board using A*")
  (doc 'type '(-> Board Coord Coord (-> Coord (List Coord)) (-> Coord Coord Number) (Maybe (List Coord))))
  (astar-path start goal
              neighbor-fn
              (lambda (c) (board-cost board c))
              heuristic-fn
              (lambda (c) (board-walkable? board c))))

(doc 'section 'reachability)

(define (find-reachable start max-cost neighbor-fn walkable-fn cost-fn)
  (doc 'export #t)
  (doc 'description "Find all coordinates reachable within N movement points")
  (doc 'type '(-> Coord Integer (-> Coord (List Coord)) (-> Coord Bool) (-> Coord Number) (List (Pair Coord Number))))
  (doc 'returns "List of (coord . cost) pairs")
  (let ([visited (make-hashtable equal-hash equal?)]
        [results '()])
       (hashtable-set! visited start 0)
       (let loop ([queue (list (cons start 0))])
            (if (null? queue)
                results
                (let* ([current-entry (car queue)]
                       [current (car current-entry)]
                       [current-cost (cdr current-entry)]
                       [rest-q (cdr queue)])
                      (set! results (cons current-entry results))
                      (let* ([neighbors (neighbor-fn current)]
                             [valid-neighbors (filter walkable-fn neighbors)]
                             [new-entries
                              (filter-map
                               (lambda (n)
                                       (let ([new-cost (+ current-cost (cost-fn n))])
                                            (if (and (<= new-cost max-cost)
                                                     (or (not (hashtable-ref visited n #f))
                                                         (< new-cost (hashtable-ref visited n 0))))
                                                (begin
                                                 (hashtable-set! visited n new-cost)
                                                 (cons n new-cost))
                                                #f)))
                               valid-neighbors)])
                            (loop (append rest-q new-entries))))))))

(define (board-reachable board start max-cost neighbor-fn)
  (doc 'export #t)
  (doc 'description "Find all tiles reachable within movement budget on board")
  (doc 'type '(-> Board Coord Integer (-> Coord (List Coord)) (List (Pair Coord Number))))
  (find-reachable start max-cost
                  neighbor-fn
                  (lambda (c) (board-walkable? board c))
                  (lambda (c) (board-cost board c))))

;;; ====
;;; Exports Summary
;;; ====

;;; This module provides:
;;;   Core Algorithms:
;;;     • bfs-path — Unweighted shortest path
;;;     • dijkstra-path — Weighted shortest path
;;;     • astar-path — Heuristic-guided shortest path
;;;
;;;   Board Integration:
;;;     • find-path-bfs — BFS on board
;;;     • find-path-dijkstra — Dijkstra on board
;;;     • find-path-astar — A* on board
;;;     • board-reachable — Find reachable tiles within budget
;;;
;;;   Reachability:
;;;     • find-reachable — Generic reachability analysis
;;;
;;; All algorithms work with any tile shape through the neighbor protocol.
