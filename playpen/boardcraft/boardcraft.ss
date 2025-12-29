;;; playpen/boardcraft/boardcraft.ss — BoardCraft SDK Main Loader
;;;
;;; Load this file to access the complete BoardCraft SDK.
;;;
;;; Usage:
;;;   (load "playpen/boardcraft/boardcraft.ss")
;;;
;;; This loads all modules in the correct order.

(display "Loading BoardCraft SDK...
")

;;; Core types and utilities
(display "  Loading core.ss...
")
(load "playpen/boardcraft/core.ss")

;;; Square tiles
(display "  Loading square.ss...
")
(load "playpen/boardcraft/square.ss")

;;; Hexagonal tiles
(display "  Loading hex.ss...
")
(load "playpen/boardcraft/hex.ss")

;;; Triangular tiles
(display "  Loading triangle.ss...
")
(load "playpen/boardcraft/triangle.ss")

;;; Pathfinding algorithms
(display "  Loading pathfinding.ss...
")
(load "playpen/boardcraft/pathfinding.ss")

;;; Visibility and line of sight
(display "  Loading visibility.ss...
")
(load "playpen/boardcraft/visibility.ss")

;;; Board rendering (ASCII art)
(display "  Loading render.ss...
")
(load "playpen/boardcraft/render.ss")

;;; Unit/entity management
(display "  Loading units.ss...
")
(load "playpen/boardcraft/units.ss")

;;; Turn-based game system
(display "  Loading turns.ss...
")
(load "playpen/boardcraft/turns.ss")

(display "BoardCraft SDK loaded successfully!
")
(display "Available tile shapes: square, hex, triangle
")
(display "Pathfinding: BFS, Dijkstra, A*
")
(display "Visibility: Line of sight, Field of view
")
(display "Rendering: ASCII art visualization
")
(display "Units: Placement, movement, visibility
")
(display "Turns: Turn order, action points, phases
")
(display "See playpen/boardcraft/README.ss for documentation.
")
(newline)

;;; Quick reference
(define (boardcraft-help)
  (display "╔══════════════════════════════════════════════════════════════╗
")
  (display "║  BOARDCRAFT SDK — Quick Reference                           ║
")
  (display "╚══════════════════════════════════════════════════════════════╝
")
  (newline)
  (display "SQUARE TILES:
")
  (display "  (make-square-board width height [tile])  Create board
")
  (display "  (square-coord x y)                       Create coordinate
")
  (display "  (square-neighbors coord 'all)            Get 8 neighbors
")
  (display "  (square-neighbors coord 'ortho)          Get 4 neighbors
")
  (display "  (square-distance c1 c2 'manhattan)       Distance
")
  (display "  (square-range center radius 'chebyshev)  Area in range
")
  (newline)
  (display "HEXAGONAL TILES:
")
  (display "  (make-hex-board 'axial radius [tile])    Create board
")
  (display "  (axial-coord q r)                        Create coordinate
")
  (display "  (hex-neighbors coord)                    Get 6 neighbors
")
  (display "  (hex-distance c1 c2)                     Hex steps
")
  (display "  (hex-range center radius)                Hexes in range
")
  (display "  (hex-line c1 c2)                         Line between hexes
")
  (newline)
  (display "BOARD OPERATIONS:
")
  (display "  (board-get board coord)                  Get tile
")
  (display "  (board-set board coord tile)             Set tile (returns new board)
")
  (display "  (board-tiles board)                      List all (coord . tile)
")
  (display "  (board-size board)                       Count stored tiles
")
  (display "  (board-empty? board)                     Check if no tiles stored
")
  (display "  (hex-board-capacity board)               Total hexes for radius
")
  (display "  (square-board-capacity board)            Total squares (w*h)
")
  (newline)
  (display "For full documentation:
")
  (display "  (load \"playpen/boardcraft/README.ss\")
")
  (newline))
