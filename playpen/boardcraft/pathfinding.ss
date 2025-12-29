;;; playpen/boardcraft/pathfinding.ss — Generic Pathfinding Algorithms
;;;
;;; Pathfinding algorithms that work with any tile shape through
;;; the neighbor protocol defined in each tile module.
;;;
;;; Implements:
;;;   • BFS (Breadth-First Search) - unweighted shortest path
;;;   • Dijkstra - weighted shortest path
;;;   • A* - heuristic-guided shortest path (most efficient)
;;;
;;; All algorithms work generically by accepting neighbor and cost functions.
;;;
;;; Dependencies:
;;;   - playpen/boardcraft/core.ss

;;; ============================================================
;;; Priority Queue (Min-Heap)
;;; ============================================================

;;; Simple priority queue implementation for Dijkstra and A*
;;; Uses a list of (priority . value) pairs, sorted by priority

(define (make-pqueue) '())

(define (pqueue-empty? pq) (null? pq))

(define (pqueue-insert pq priority value)
  (let loop ([lst pq])
       (cond
        [(null? lst) (list (cons priority value))]
        [(< priority (caar lst))
         (cons (cons priority value) lst)]
        [else
         (cons (car lst) (loop (cdr lst)))])))

(define (pqueue-pop pq)
  (if (null? pq)
      (values #f #f #f)
      (values (cdar pq) (caar pq) (cdr pq))))

;;; ============================================================
;;; Path Reconstruction
;;; ============================================================

;;; reconstruct-path : HashTable × Coord × Coord → (List Coord)
;;; Reconstruct path from came-from map
(define (reconstruct-path came-from start goal)
  (let loop ([current goal] [path '()])
       (if (equal? current start)
           (cons start path)
           (let ([prev (hashtable-ref came-from current #f)])
                (if prev
                    (loop prev (cons current path))
                    #f))))) ; No path found

;;; ============================================================
;;; Breadth-First Search (BFS)
;;; ============================================================

;;; bfs-path : Coord × Coord × (Coord → List Coord) × (Coord → Bool) → (List Coord) | #f
;;; Find shortest path using BFS (unweighted)
;;;
;;; Parameters:
;;;   start - starting coordinate
;;;   goal - goal coordinate
;;;   neighbor-fn - function that returns list of neighbors
;;;   walkable-fn - predicate to check if coordinate is walkable
;;;
;;; Returns: List of coordinates from start to goal, or #f if no path
(define (bfs-path start goal neighbor-fn walkable-fn)
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

;;; ============================================================
;;; Dijkstra's Algorithm
;;; ============================================================

;;; dijkstra-path : Coord × Coord × (Coord → List Coord) × (Coord → Number) × (Coord → Bool) → (List Coord) | #f
;;; Find shortest weighted path using Dijkstra's algorithm
;;;
;;; Parameters:
;;;   start - starting coordinate
;;;   goal - goal coordinate
;;;   neighbor-fn - function that returns list of neighbors
;;;   cost-fn - function that returns movement cost for a coordinate
;;;   walkable-fn - predicate to check if coordinate is walkable
;;;
;;; Returns: List of coordinates from start to goal, or #f if no path
(define (dijkstra-path start goal neighbor-fn cost-fn walkable-fn)
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

;;; ============================================================
;;; A* Algorithm
;;; ============================================================

;;; astar-path : Coord × Coord × (Coord → List Coord) × (Coord → Number) × (Coord × Coord → Number) × (Coord → Bool) → (List Coord) | #f
;;; Find shortest path using A* with heuristic
;;;
;;; Parameters:
;;;   start - starting coordinate
;;;   goal - goal coordinate
;;;   neighbor-fn - function that returns list of neighbors
;;;   cost-fn - function that returns movement cost for a coordinate
;;;   heuristic-fn - function (coord1, coord2) → estimated distance
;;;   walkable-fn - predicate to check if coordinate is walkable
;;;
;;; Returns: List of coordinates from start to goal, or #f if no path
;;;
;;; The heuristic must be admissible (never overestimate) for A* to be optimal.
(define (astar-path start goal neighbor-fn cost-fn heuristic-fn walkable-fn)
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

;;; ============================================================
;;; Board-Integrated Pathfinding
;;; ============================================================

;;; These functions integrate with the board system, handling
;;; tile walkability checks automatically.

;;; board-walkable? : Board × Coord → Boolean
;;; Check if a coordinate is walkable on the board
(define (board-walkable? board coord)
  (let ([tile (board-get board coord)])
       (and tile (tile-walkable? tile))))

;;; board-cost : Board × Coord → Number
;;; Get movement cost for a coordinate on the board
(define (board-cost board coord)
  (let ([tile (board-get board coord)])
       (if tile
           (tile-cost tile)
           999))) ; High cost for missing tiles

;;; find-path-bfs : Board × Coord × Coord × (Coord → List Coord) → (List Coord) | #f
;;; Find path on board using BFS
(define (find-path-bfs board start goal neighbor-fn)
  (bfs-path start goal
            neighbor-fn
            (lambda (c) (board-walkable? board c))))

;;; find-path-dijkstra : Board × Coord × Coord × (Coord → List Coord) → (List Coord) | #f
;;; Find path on board using Dijkstra
(define (find-path-dijkstra board start goal neighbor-fn)
  (dijkstra-path start goal
                 neighbor-fn
                 (lambda (c) (board-cost board c))
                 (lambda (c) (board-walkable? board c))))

;;; find-path-astar : Board × Coord × Coord × (Coord → List Coord) × (Coord × Coord → Number) → (List Coord) | #f
;;; Find path on board using A*
(define (find-path-astar board start goal neighbor-fn heuristic-fn)
  (astar-path start goal
              neighbor-fn
              (lambda (c) (board-cost board c))
              heuristic-fn
              (lambda (c) (board-walkable? board c))))

;;; ============================================================
;;; Reachability Analysis
;;; ============================================================

;;; find-reachable : Coord × Integer × (Coord → List Coord) × (Coord → Bool) × (Coord → Number) → (List (Coord . Number))
;;; Find all coordinates reachable within N movement points
;;;
;;; Returns: List of (coord . cost) pairs
(define (find-reachable start max-cost neighbor-fn walkable-fn cost-fn)
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

;;; board-reachable : Board × Coord × Integer × (Coord → List Coord) → (List (Coord . Number))
;;; Find all tiles reachable within movement budget on board
(define (board-reachable board start max-cost neighbor-fn)
  (find-reachable start max-cost
                  neighbor-fn
                  (lambda (c) (board-walkable? board c))
                  (lambda (c) (board-cost board c))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

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
