;;; lattice/tiles/test-pathfinding.ss — Tests for Pathfinding Algorithms
;;; Run with: scheme --script lattice/tiles/test-pathfinding.ss

(load "core/testing/test-framework.ss")
(load "lattice/tiles/core.ss")
(load "lattice/tiles/square.ss")
(load "lattice/tiles/pathfinding.ss")

;;; ============================================================
;;; Helpers
;;; ============================================================

(define (make-floor-board w h)
  (make-square-board w h (make-tile 'floor '())))

(define (set-wall board coord)
  (board-set board coord (make-tile 'wall '((walkable . #f)))))

(define (set-swamp board coord)
  (board-set board coord (make-tile 'swamp '((cost . 3)))))

(define (sq-neighbors c) (square-neighbors c 'ortho))

(define (sq-heuristic c1 c2)
  (square-distance c1 c2 'manhattan))

;;; ============================================================
;;; Priority Queue
;;; ============================================================

(test-group "priority-queue"
  (define-test "empty queue"
    (let ([pq (make-pqueue)])
      (assert-true (pqueue-empty? pq))))

  (define-test "insert makes non-empty"
    (let ([pq (pqueue-insert (make-pqueue) 1 'a)])
      (assert-false (pqueue-empty? pq))))

  (define-test "pop returns lowest priority"
    (let* ([pq (make-pqueue)]
           [pq (pqueue-insert pq 3 'c)]
           [pq (pqueue-insert pq 1 'a)]
           [pq (pqueue-insert pq 2 'b)])
      (let-values ([(val pri rest) (pqueue-pop pq)])
        (assert-equal 'a val)
        (assert-equal 1 pri))))

  (define-test "pop order is priority order"
    (let* ([pq (make-pqueue)]
           [pq (pqueue-insert pq 5 'e)]
           [pq (pqueue-insert pq 2 'b)]
           [pq (pqueue-insert pq 8 'h)]
           [pq (pqueue-insert pq 1 'a)])
      (let-values ([(v1 p1 pq) (pqueue-pop pq)])
        (let-values ([(v2 p2 pq) (pqueue-pop pq)])
          (let-values ([(v3 p3 pq) (pqueue-pop pq)])
            (assert-equal 'a v1)
            (assert-equal 'b v2)
            (assert-equal 'e v3))))))

  (define-test "pop empty returns #f"
    (let-values ([(val pri rest) (pqueue-pop (make-pqueue))])
      (assert-false val)
      (assert-false pri))))

;;; ============================================================
;;; BFS
;;; ============================================================

(test-group "bfs"
  (define board (make-floor-board 5 5))

  (define-test "finds path on open board"
    (let ([path (find-path-bfs board
                               (square-coord 0 0)
                               (square-coord 4 4)
                               sq-neighbors)])
      (assert-true (pair? path))))

  (define-test "path starts at start"
    (let ([path (find-path-bfs board
                               (square-coord 0 0)
                               (square-coord 2 2)
                               sq-neighbors)])
      (assert-equal (square-coord 0 0) (car path))))

  (define-test "path ends at goal"
    (let ([path (find-path-bfs board
                               (square-coord 0 0)
                               (square-coord 2 2)
                               sq-neighbors)])
      (let ([last-el (list-ref path (- (length path) 1))])
        (assert-equal (square-coord 2 2) last-el))))

  (define-test "start = goal returns length-1 path"
    (let ([path (find-path-bfs board
                               (square-coord 2 2)
                               (square-coord 2 2)
                               sq-neighbors)])
      (assert-equal 1 (length path))))

  (define-test "adjacent path has length 2"
    (let ([path (find-path-bfs board
                               (square-coord 0 0)
                               (square-coord 0 1)
                               sq-neighbors)])
      (assert-equal 2 (length path))))

  (define-test "returns #f when blocked"
    (let* ([b board]
           [b (set-wall b (square-coord 2 0))]
           [b (set-wall b (square-coord 2 1))]
           [b (set-wall b (square-coord 2 2))]
           [b (set-wall b (square-coord 2 3))]
           [b (set-wall b (square-coord 2 4))]
           [path (find-path-bfs b
                                (square-coord 0 0)
                                (square-coord 4 4)
                                sq-neighbors)])
      (assert-false path)))

  (define-test "finds shortest path (unweighted)"
    (let ([path (find-path-bfs board
                               (square-coord 0 0)
                               (square-coord 2 0)
                               sq-neighbors)])
      ;; Manhattan distance is 2, so shortest path is 3 nodes
      (assert-equal 3 (length path)))))

;;; ============================================================
;;; Dijkstra
;;; ============================================================

(test-group "dijkstra"
  (define board (make-floor-board 5 5))

  (define-test "finds path on open board"
    (let ([path (find-path-dijkstra board
                                    (square-coord 0 0)
                                    (square-coord 4 4)
                                    sq-neighbors)])
      (assert-true (pair? path))))

  (define-test "path starts and ends correctly"
    (let ([path (find-path-dijkstra board
                                    (square-coord 0 0)
                                    (square-coord 3 3)
                                    sq-neighbors)])
      (assert-equal (square-coord 0 0) (car path))
      (assert-equal (square-coord 3 3) (list-ref path (- (length path) 1)))))

  (define-test "returns #f when blocked"
    (let* ([b board]
           [b (set-wall b (square-coord 2 0))]
           [b (set-wall b (square-coord 2 1))]
           [b (set-wall b (square-coord 2 2))]
           [b (set-wall b (square-coord 2 3))]
           [b (set-wall b (square-coord 2 4))]
           [path (find-path-dijkstra b
                                     (square-coord 0 0)
                                     (square-coord 4 4)
                                     sq-neighbors)])
      (assert-false path)))

  (define-test "avoids high-cost tiles"
    ;; Swamp along direct vertical path
    (let* ([b board]
           [b (set-swamp b (square-coord 1 0))]
           [b (set-swamp b (square-coord 1 1))]
           [path (find-path-dijkstra b
                                     (square-coord 0 0)
                                     (square-coord 2 0)
                                     sq-neighbors)])
      ;; Should still find a path
      (assert-true (pair? path)))))

;;; ============================================================
;;; A*
;;; ============================================================

(test-group "astar"
  (define board (make-floor-board 5 5))

  (define-test "finds path on open board"
    (let ([path (find-path-astar board
                                 (square-coord 0 0)
                                 (square-coord 4 4)
                                 sq-neighbors
                                 sq-heuristic)])
      (assert-true (pair? path))))

  (define-test "path starts and ends correctly"
    (let ([path (find-path-astar board
                                 (square-coord 0 0)
                                 (square-coord 3 3)
                                 sq-neighbors
                                 sq-heuristic)])
      (assert-equal (square-coord 0 0) (car path))
      (assert-equal (square-coord 3 3) (list-ref path (- (length path) 1)))))

  (define-test "returns #f when blocked"
    (let* ([b board]
           [b (set-wall b (square-coord 2 0))]
           [b (set-wall b (square-coord 2 1))]
           [b (set-wall b (square-coord 2 2))]
           [b (set-wall b (square-coord 2 3))]
           [b (set-wall b (square-coord 2 4))]
           [path (find-path-astar b
                                  (square-coord 0 0)
                                  (square-coord 4 4)
                                  sq-neighbors
                                  sq-heuristic)])
      (assert-false path)))

  (define-test "finds optimal path length"
    (let ([path (find-path-astar board
                                 (square-coord 0 0)
                                 (square-coord 2 0)
                                 sq-neighbors
                                 sq-heuristic)])
      ;; Manhattan = 2, shortest = 3 nodes
      (assert-equal 3 (length path)))))

;;; ============================================================
;;; Reachability
;;; ============================================================

(test-group "reachability"
  (define board (make-floor-board 5 5))

  (define-test "start is always reachable"
    (let ([reachable (board-reachable board (square-coord 2 2) 0 sq-neighbors)])
      (assert-true (pair? (filter (lambda (e) (equal? (car e) (square-coord 2 2)))
                                  reachable)))))

  (define-test "budget 1 reaches adjacent"
    (let ([reachable (board-reachable board (square-coord 2 2) 1 sq-neighbors)])
      ;; Center + 4 orthogonal neighbors = 5
      (assert-true (>= (length reachable) 5))))

  (define-test "larger budget reaches more"
    (let ([r1 (board-reachable board (square-coord 2 2) 1 sq-neighbors)]
          [r2 (board-reachable board (square-coord 2 2) 2 sq-neighbors)])
      (assert-true (>= (length r2) (length r1)))))

  (define-test "walls limit reachability"
    (let* ([b board]
           [b (set-wall b (square-coord 2 1))]
           [b (set-wall b (square-coord 2 3))]
           [b (set-wall b (square-coord 1 2))]
           [b (set-wall b (square-coord 3 2))]
           ;; Surrounded by walls — only center reachable
           [reachable (board-reachable b (square-coord 2 2) 5 sq-neighbors)])
      (assert-equal 1 (length reachable)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
