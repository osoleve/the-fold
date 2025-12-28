;;; playpen/boardcraft/examples/pathfinding-demo.ss — Pathfinding Demo
;;;
;;; Demonstrates BFS, Dijkstra, and A* pathfinding across
;;; all tile shapes (square, hex, triangle).
;;;
;;; Usage:
;;;   scheme --script playpen/boardcraft/examples/pathfinding-demo.ss

(load "playpen/boardcraft/boardcraft.ss")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  BOARDCRAFT PATHFINDING DEMO\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

;;; ============================================================
;;; Example 1: BFS on Square Grid
;;; ============================================================

(display "Example 1: BFS on Square Grid (Chess-like)\n")
(display "──────────────────────────────────────────────────────────\n")

;;; Create a simple maze
(define floor (make-tile 'floor '((walkable . #t))))
(define wall (make-tile 'wall '((walkable . #f))))

(define square-board (make-square-board 8 8 floor))

;;; Add some walls
(define square-board
  (board-set (board-set (board-set square-board
                                   (square-coord 3 2) wall)
                        (square-coord 3 3) wall)
             (square-coord 3 4) wall))

(define sq-start (square-coord 1 1))
(define sq-goal (square-coord 6 6))

(display "Finding path from ")
(display sq-start)
(display " to ")
(display sq-goal)
(newline)

(let ([path (find-path-bfs square-board sq-start sq-goal
                          (lambda (c) (square-neighbors c 'ortho)))])
  (if path
      (begin
        (display "  Path found! Length: ")
        (display (length path))
        (newline)
        (display "  Path: ")
        (for-each (lambda (c) (display c) (display " ")) path)
        (newline))
      (display "  No path found\n")))
(newline)

;;; ============================================================
;;; Example 2: Dijkstra on Square Grid with Costs
;;; ============================================================

(display "Example 2: Dijkstra with Variable Costs\n")
(display "──────────────────────────────────────────────────────────\n")

;;; Create terrain with different movement costs
(define grass (make-tile 'grass '((walkable . #t) (cost . 1))))
(define swamp (make-tile 'swamp '((walkable . #t) (cost . 3))))

(define cost-board (make-square-board 6 6 grass))

;;; Add swamp in the middle
(define cost-board
  (board-set (board-set (board-set cost-board
                                   (square-coord 2 2) swamp)
                        (square-coord 3 2) swamp)
             (square-coord 2 3) swamp))

(define cost-start (square-coord 0 0))
(define cost-goal (square-coord 5 5))

(display "Terrain: grass (cost=1), swamp (cost=3)\n")
(display "Finding cheapest path from ")
(display cost-start)
(display " to ")
(display cost-goal)
(newline)

(let ([path (find-path-dijkstra cost-board cost-start cost-goal
                               (lambda (c) (square-neighbors c 'all)))])
  (if path
      (begin
        (display "  Path found! Length: ")
        (display (length path))
        (newline)
        (display "  Path avoids swamp: ")
        (for-each (lambda (c) (display c) (display " ")) path)
        (newline))
      (display "  No path found\n")))
(newline)

;;; ============================================================
;;; Example 3: A* on Hexagonal Grid
;;; ============================================================

(display "Example 3: A* on Hexagonal Grid\n")
(display "──────────────────────────────────────────────────────────\n")

(define hex-board (make-hex-board 'axial 5 floor))

;;; Add a wall of hexes
(define hex-board
  (board-set (board-set (board-set hex-board
                                   (axial-coord 0 1) wall)
                        (axial-coord 0 2) wall)
             (axial-coord 1 1) wall))

(define hex-start (axial-coord -2 -2))
(define hex-goal (axial-coord 3 2))

(display "Finding path from ")
(display hex-start)
(display " to ")
(display hex-goal)
(newline)

(let ([path (find-path-astar hex-board hex-start hex-goal
                            hex-neighbors
                            hex-distance)])
  (if path
      (begin
        (display "  A* path found! Length: ")
        (display (length path))
        (newline)
        (display "  Path: ")
        (for-each (lambda (c) (display c) (display " ")) path)
        (newline))
      (display "  No path found\n")))
(newline)

;;; ============================================================
;;; Example 4: BFS on Triangular Grid
;;; ============================================================

(display "Example 4: BFS on Triangular Grid\n")
(display "──────────────────────────────────────────────────────────\n")

(define tri-board (make-triangle-board 5 5 floor))

;;; Add some walls
(define tri-board
  (board-set (board-set tri-board
                        (triangle-coord 2 2 'up) wall)
             (triangle-coord 2 2 'down) wall))

(define tri-start (triangle-coord 0 0 'up))
(define tri-goal (triangle-coord 4 4 'up))

(display "Finding path from ")
(display tri-start)
(newline)
(display "                to ")
(display tri-goal)
(newline)

(let ([path (find-path-bfs tri-board tri-start tri-goal
                          (lambda (c) (triangle-neighbors c 'edge)))])
  (if path
      (begin
        (display "  Path found! Length: ")
        (display (length path))
        (newline)
        (display "  (using edge neighbors only)\n"))
      (display "  No path found\n")))
(newline)

;;; ============================================================
;;; Example 5: Reachability Analysis
;;; ============================================================

(display "Example 5: Reachability Analysis\n")
(display "──────────────────────────────────────────────────────────\n")

(define reach-board (make-square-board 10 10 floor))
(define reach-start (square-coord 5 5))
(define movement-points 3)

(display "Unit at ")
(display reach-start)
(display " with ")
(display movement-points)
(display " movement points\n")

(let ([reachable (board-reachable reach-board reach-start movement-points
                                 (lambda (c) (square-neighbors c 'ortho)))])
  (display "  Can reach ")
  (display (length reachable))
  (display " tiles:\n")
  (display "  (showing first 10)\n")
  (let loop ([tiles reachable] [count 0])
    (when (and (pair? tiles) (< count 10))
      (let ([entry (car tiles)])
        (display "    ")
        (display (car entry))
        (display " cost=")
        (display (cdr entry))
        (newline)
        (loop (cdr tiles) (+ count 1))))))
(newline)

;;; ============================================================
;;; Example 6: Hex Pathfinding with Obstacles
;;; ============================================================

(display "Example 6: Hex Maze Navigation\n")
(display "──────────────────────────────────────────────────────────\n")

(define maze-board (make-hex-board 'axial 4 floor))

;;; Create a maze pattern
(define maze-board
  (board-set (board-set (board-set (board-set maze-board
                                              (axial-coord 0 0) wall)
                                   (axial-coord 1 0) wall)
                        (axial-coord -1 1) wall)
             (axial-coord 0 -1) wall))

(define maze-start (axial-coord -3 -1))
(define maze-goal (axial-coord 3 1))

(display "Navigating hex maze from ")
(display maze-start)
(display " to ")
(display maze-goal)
(newline)

(let ([path (find-path-astar maze-board maze-start maze-goal
                            hex-neighbors
                            hex-distance)])
  (if path
      (begin
        (display "  Path found! ")
        (display (length path))
        (display " hexes:\n  ")
        (for-each (lambda (c) (display c) (display " ")) path)
        (newline))
      (display "  No path exists!\n")))
(newline)

;;; ============================================================
;;; Summary
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "  Demo Complete!\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)
(display "Pathfinding works generically across all tile shapes:\n")
(display "  ✓ Square tiles (4-way, 8-way movement)\n")
(display "  ✓ Hexagonal tiles (6-way movement)\n")
(display "  ✓ Triangular tiles (3 edge neighbors)\n")
(newline)
(display "Three algorithms available:\n")
(display "  • BFS — Unweighted shortest path (simple, fast)\n")
(display "  • Dijkstra — Weighted shortest path (handles terrain costs)\n")
(display "  • A* — Heuristic-guided (most efficient with good heuristic)\n")
(newline)
(display "Additional features:\n")
(display "  • Reachability analysis (movement budget)\n")
(display "  • Obstacle avoidance\n")
(display "  • Variable terrain costs\n")
(display "  • Works with any neighbor function\n")
(newline)
