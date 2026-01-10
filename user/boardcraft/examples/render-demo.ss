;;; playpen/boardcraft/examples/render-demo.ss — Board Rendering Demo
;;;
;;; Demonstrates ASCII art rendering of boards, including
;;; visualization of pathfinding and field-of-view results.
;;;
;;; Usage:
;;;   scheme --script playpen/boardcraft/examples/render-demo.ss

(load "user/boardcraft/boardcraft.ss")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  BOARDCRAFT RENDERING DEMO\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

;;; ============================================================
;;; Example 1: Basic Square Board
;;; ============================================================

(display "Example 1: Basic Square Board Rendering\n")
(display "──────────────────────────────────────────────────────────\n")

(define floor (make-tile 'floor '()))
(define wall (make-tile 'wall '()))

(define simple-board (make-square-board 8 6 floor))

;;; Add some walls
(define simple-board
  (board-set (board-set (board-set simple-board
                                   (square-coord 3 2) wall)
                        (square-coord 3 3) wall)
             (square-coord 3 4) wall))

(display "Legend: . = floor, # = wall\n\n")
(display-board simple-board default-tile-char)
(newline)

;;; ============================================================
;;; Example 2: Pathfinding Visualization
;;; ============================================================

(display "Example 2: Pathfinding Visualization\n")
(display "──────────────────────────────────────────────────────────\n")

(define maze-board (make-square-board 12 8 floor))

;;; Create a maze
(define maze-board
  (let loop ([y 0] [b maze-board])
       (if (> y 7)
           b
           (loop (+ y 1)
                 (board-set (board-set b
                                       (square-coord 4 y) wall)
                            (square-coord 7 y) wall)))))

;;; Add doorways
(define maze-board
  (board-set (board-set maze-board
                        (square-coord 4 3) floor)
             (square-coord 7 5) floor))

(define start (square-coord 1 3))
(define goal (square-coord 10 5))

(display "Finding path from ")
(display start)
(display " to ")
(display goal)
(newline)

(define path (find-path-bfs maze-board start goal
                            (lambda (c) (square-neighbors c 'ortho))))

(display "Legend: . = floor, # = wall, * = path\n\n")
(if path
    (begin
     (display "Path found with length ")
     (display (length path))
     (display ":\n\n")
     (display-board-with-path maze-board default-tile-char path))
    (display "No path found!\n"))
(newline)

;;; ============================================================
;;; Example 3: Field of View Visualization
;;; ============================================================

(display "Example 3: Field of View Visualization\n")
(display "──────────────────────────────────────────────────────────\n")

(define fov-board (make-square-board 15 10 floor))

;;; Add walls
(define fov-board
  (let loop ([y 0] [b fov-board])
       (if (> y 9)
           b
           (loop (+ y 1)
                 (board-set b (square-coord 7 y) wall)))))

;;; Add doorway
(define fov-board
  (board-set (board-set fov-board
                        (square-coord 7 4) floor)
             (square-coord 7 5) floor))

(define viewer (square-coord 3 4))

(display "Calculating FOV from ")
(display viewer)
(display " with range 6\n")

(define visible (board-fov fov-board viewer 6
                           (lambda (c) (square-neighbors c 'all))
                           square-line))

(display "Legend: . = floor, # = wall, + = visible\n\n")
(display "Can see ")
(display (length visible))
(display " tiles:\n\n")
(display-board-with-fov fov-board default-tile-char visible)
(newline)

;;; ============================================================
;;; Example 4: Hexagonal Board Rendering
;;; ============================================================

(display "Example 4: Hexagonal Board Rendering\n")
(display "──────────────────────────────────────────────────────────\n")

(define grass (make-tile 'grass '((walkable . #t))))
(define water (make-tile 'water '((walkable . #f))))
(define mountain (make-tile 'mountain '((walkable . #f))))

(define hex-board (make-hex-board 'axial 3 grass))

;;; Add some terrain variety
(define hex-board
  (board-set (board-set (board-set hex-board
                                   (axial-coord 0 0) mountain)
                        (axial-coord 1 1) water)
             (axial-coord -1 2) water))

(display "Legend: , = grass, ~ = water, ^ = mountain\n\n")
(display-board hex-board default-tile-char)
(newline)

;;; ============================================================
;;; Example 5: Hexagonal Pathfinding Visualization
;;; ============================================================

(display "Example 5: Hexagonal Pathfinding Visualization\n")
(display "──────────────────────────────────────────────────────────\n")

(define hex-path-board (make-hex-board 'axial 4 grass))

;;; Add water obstacle
(define hex-path-board
  (board-set (board-set (board-set hex-path-board
                                   (axial-coord 0 0) water)
                        (axial-coord 1 0) water)
             (axial-coord 0 1) water))

(define hex-start (axial-coord -3 -1))
(define hex-goal (axial-coord 3 2))

(display "Finding path from ")
(display hex-start)
(display " to ")
(display hex-goal)
(newline)

(define hex-path (find-path-astar hex-path-board hex-start hex-goal
                                  hex-neighbors
                                  hex-distance))

(display "Legend: , = grass, ~ = water (impassable), * = path\n\n")
(if hex-path
    (begin
     (display "Path found with length ")
     (display (length hex-path))
     (display ":\n\n")
     (display-board-with-path hex-path-board default-tile-char hex-path))
    (display "No path found!\n"))
(newline)

;;; ============================================================
;;; Summary
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "  Demo Complete!\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)
(display "Board rendering supports:\n")
(display "  ✓ Square grids (rectangular layout)\n")
(display "  ✓ Hexagonal grids (staggered layout)\n")
(display "  ✓ Custom tile styles\n")
(display "  ✓ Path highlighting\n")
(display "  ✓ Field-of-view highlighting\n")
(newline)
(display "Use cases:\n")
(display "  • Debugging board layouts\n")
(display "  • Visualizing pathfinding\n")
(display "  • Demonstrating game mechanics\n")
(display "  • Creating roguelike displays\n")
(newline)
