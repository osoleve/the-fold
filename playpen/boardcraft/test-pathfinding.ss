;;; Quick test of pathfinding

(load "playpen/boardcraft/boardcraft.ss")

(display "Testing pathfinding...\n")

;;; Simple BFS test
(define floor (make-tile 'floor '((walkable . #t))))
(define board (make-square-board 5 5 floor))
(define start (square-coord 0 0))
(define goal (square-coord 4 4))

(display "Board created\n")
(display "Finding path...\n")

(let ([path (find-path-bfs board start goal
                          (lambda (c) (square-neighbors c 'ortho)))])
  (if path
      (begin
        (display "Path found! Length: ")
        (display (length path))
        (newline))
      (display "No path\n")))

(display "Done\n")
