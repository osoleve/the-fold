(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude 'tiles/core 'tiles/square 'tiles/hex 'tiles/triangle
         'pathfinding 'visibility 'tiles/render 'tiles/units 'turns)

(doc 'module 'tiles/boardcraft)
(doc 'description "BoardCraft SDK main loader - loads all tile game modules")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'quick-reference)

(define (boardcraft-help)
  (doc 'description "Display BoardCraft SDK quick reference guide")
  (doc 'returns "Void - prints help text to stdout")
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
  (display "  (load \"lattice/tiles/README.sexp\")
")
  (newline))
