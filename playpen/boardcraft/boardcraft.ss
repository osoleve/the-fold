;;; playpen/boardcraft/boardcraft.ss — BoardCraft SDK Main Loader
;;;
;;; Load this file to access the complete BoardCraft SDK.
;;;
;;; Usage:
;;;   (load "playpen/boardcraft/boardcraft.ss")
;;;
;;; This loads all modules in the correct order.

(display "Loading BoardCraft SDK...\n")

;;; Core types and utilities
(display "  Loading core.ss...\n")
(load "playpen/boardcraft/core.ss")

;;; Square tiles
(display "  Loading square.ss...\n")
(load "playpen/boardcraft/square.ss")

;;; Hexagonal tiles
(display "  Loading hex.ss...\n")
(load "playpen/boardcraft/hex.ss")

;;; Triangular tiles
(display "  Loading triangle.ss...\n")
(load "playpen/boardcraft/triangle.ss")

;;; Pathfinding algorithms
(display "  Loading pathfinding.ss...\n")
(load "playpen/boardcraft/pathfinding.ss")

;;; Visibility and line of sight
(display "  Loading visibility.ss...\n")
(load "playpen/boardcraft/visibility.ss")

(display "BoardCraft SDK loaded successfully!\n")
(display "Available tile shapes: square, hex, triangle\n")
(display "Pathfinding: BFS, Dijkstra, A*\n")
(display "Visibility: Line of sight, Field of view\n")
(display "See playpen/boardcraft/README.ss for documentation.\n")
(newline)

;;; Quick reference
(define (boardcraft-help)
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║  BOARDCRAFT SDK — Quick Reference                           ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (newline)
  (display "SQUARE TILES:\n")
  (display "  (make-square-board width height [tile])  Create board\n")
  (display "  (square-coord x y)                       Create coordinate\n")
  (display "  (square-neighbors coord 'all)            Get 8 neighbors\n")
  (display "  (square-neighbors coord 'ortho)          Get 4 neighbors\n")
  (display "  (square-distance c1 c2 'manhattan)       Distance\n")
  (display "  (square-range center radius 'chebyshev)  Area in range\n")
  (newline)
  (display "HEXAGONAL TILES:\n")
  (display "  (make-hex-board 'axial radius [tile])    Create board\n")
  (display "  (axial-coord q r)                        Create coordinate\n")
  (display "  (hex-neighbors coord)                    Get 6 neighbors\n")
  (display "  (hex-distance c1 c2)                     Hex steps\n")
  (display "  (hex-range center radius)                Hexes in range\n")
  (display "  (hex-line c1 c2)                         Line between hexes\n")
  (newline)
  (display "BOARD OPERATIONS:\n")
  (display "  (board-get board coord)                  Get tile\n")
  (display "  (board-set board coord tile)             Set tile (returns new board)\n")
  (display "  (board-tiles board)                      List all (coord . tile)\n")
  (display "  (board-size board)                       Count tiles\n")
  (newline)
  (display "For full documentation:\n")
  (display "  (load \"playpen/boardcraft/README.ss\")\n")
  (newline))
