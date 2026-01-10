;;; playpen/boardcraft/examples/tutorial-1-hex-basics.ss
;;; BoardCraft Tutorial 1: Hexagonal Board Basics
;;;
;;; This tutorial introduces the fundamental concepts of working with
;;; hexagonal boards in BoardCraft. By the end, you'll understand:
;;; - How to create hexagonal boards
;;; - Different coordinate systems (axial, cubic, offset)
;;; - How to find neighbors and calculate distances
;;; - Basic board operations

(load "user/boardcraft/boardcraft.ss")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  BOARDCRAFT TUTORIAL 1: HEXAGONAL BOARD BASICS\n")
(display "═══════════════════════════════════════════════════════════════\n\n")

;;; ============================================================
;;; Part 1: Creating Hexagonal Boards
;;; ============================================================

(display "1. CREATING HEXAGONAL BOARDS\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "BoardCraft supports hexagonal boards with different coordinate systems.\n")
(display "Let's create a basic hex board using axial coordinates:\n\n")

;; Create a hex board with radius 3 (meaning 3 rings around center)
(define board (make-hex-board 'axial 3))

(display "Created hex board with radius 3\n")
(display "Board size: ")
(display (board-size board))
(display " tiles\n\n")

(display "The 'axial' coordinate system uses (q, r) coordinates where:\n")
(display "- q = column (roughly)\n")
(display "- r = row (roughly)\n")
(display "- The third coordinate s = -q - r is implicit\n\n")

;;; ============================================================
;;; Part 2: Understanding Coordinates
;;; ============================================================

(display "2. COORDINATE SYSTEMS\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "BoardCraft supports three coordinate systems for hexagons:\n\n")

;; Axial coordinates (most common for algorithms)
(define axial-coord (axial-coord 2 1))
(display "Axial coordinates: ")
(display axial-coord)
(display "\n")

;; Convert to cubic coordinates (good for symmetry operations)
(define cubic-coord (axial->cubic axial-coord))
(display "Cubic coordinates: x=")
(display (cubic-coord-x cubic-coord))
(display " y=")
(display (cubic-coord-y cubic-coord))
(display " z=")
(display (cubic-coord-z cubic-coord))
(display "\n")

;; Verify cubic constraint: x + y + z = 0
(display "Verify x+y+z=0: ")
(display (+ (cubic-coord-x cubic-coord)
            (cubic-coord-y cubic-coord)
            (cubic-coord-z cubic-coord)))
(display "\n\n")

;; Convert to offset coordinates (good for storage/display)
(define offset-coord (axial->offset axial-coord 'odd-r))
(display "Offset coordinates: ")
(display offset-coord)
(display "\n\n")

;;; ============================================================
;;; Part 3: Finding Neighbors
;;; ============================================================

(display "3. FINDING NEIGHBORS\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "Each hexagon has 6 neighbors. Let's find them for the center hex (0,0):\n\n")

(define center (axial-coord 0 0))
(define neighbors (hex-neighbors center))

(display "Center hex: ")
(display center)
(display "\n")
(display "Neighbors: ")
(display neighbors)
(display "\n")
(display "Number of neighbors: ")
(display (length neighbors))
(display "\n\n")

(display "The neighbors are arranged like this:\n")
(display "     (-1, 0)  (0, -1)\n")
(display "  (-1, 1)  (0, 0)  (1, 0)\n")
(display "     (0, 1)  (1, -1)\n\n")

;;; ============================================================
;;; Part 4: Distance Calculations
;;; ============================================================

(display "4. DISTANCE CALCULATIONS\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "Distance in hexagonal grids is measured in "hex steps":\n\n")

(define hex-a (axial-coord 0 0))
(define hex-b (axial-coord 3 2))

(display "Distance from ")
(display hex-a)
(display " to ")
(display hex-b)
(display ": ")
(display (hex-distance hex-a hex-b))
(display " steps\n\n")

(display "This means it takes ")
(display (hex-distance hex-a hex-b))
(display " moves to get from hex A to hex B.\n\n")

;;; ============================================================
;;; Part 5: Working with Tiles
;;; ============================================================

(display "5. WORKING WITH TILES\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "Hexagons can contain tile data. Let's create some tiles and place them:\n\n")

;; Create different tile types
(define floor-tile (make-tile 'floor '((walkable . #t) (glyph . #\.))))
(define wall-tile (make-tile 'wall '((walkable . #f) (glyph . #//#))))
(define water-tile (make-tile 'water '((walkable . #f) (glyph . #//) (color . blue))))

(display "Created tiles:\n")
(display "  Floor: walkable=")
(display (tile-walkable? floor-tile))
(display " glyph=")
(display (tile-glyph floor-tile))
(display "\n")

(display "  Wall: walkable=")
(display (tile-walkable? wall-tile))
(display " glyph=")
(display (tile-glyph wall-tile))
(display "\n")

(display "  Water: walkable=")
(display (tile-walkable? water-tile))
(display " glyph=")
(display (tile-glyph water-tile))
(display "\n\n")

;; Place some tiles on the board
(define board-with-tiles
  (board-set (board-set (board-set board
                                   (axial-coord 0 0)) floor-tile)
             (axial-coord 1 0)) wall-tile)
             (axial-coord 2 1)) water-tile)

(display "Placed tiles on board at positions:\n")
(display "  (0,0): Floor tile\n")
(display "  (1,0): Wall tile\n")
(display "  (2,1): Water tile\n\n")

;;; ============================================================
;;; Part 6: Visualization
;;; ============================================================

(display "6. VISUALIZATION\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "While BoardCraft has rendering capabilities, let's visualize\n")
(display "our board conceptually:\n\n")

(display "Imagine a hexagonal grid where:\n")
(display "- Each hex is a position where units can exist\n")
(display "- Lines connect neighboring hexes (6 per hex)\n")
(display "- Different tile types have different properties\n")
(display "- Distance determines movement range\n\n")

(display "For a 3-radius hex board, you get this pattern:\n")
(display "      . . .\n")
(display "     . . . .\n")
(display "    . . . . .\n")
(display "   . . . . . .\n")
(display "  . . . . . . .\n")
(display "   . . . . . .\n")
(display "    . . . . .\n")
(display "     . . . .\n")
(display "      . . .\n\n")

(display "(37 hexes total for radius 3)\n\n")

;;; ============================================================
;;; Summary and Next Steps
;;; ============================================================

(display "7. SUMMARY\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "✓ You've learned the basics of hexagonal boards!\n")
(display "✓ You can create boards with different coordinate systems\n")
(display "✓ You can find neighbors and calculate distances\n")
(display "✓ You can work with tiles and their properties\n\n")

(display "This is the foundation for building hexagonal strategy games like:\n")
(display "- Settlers of Catan-style games\n")
(display "- Civilization-style strategy games\n")
(display "- Wargames and tactical combat\n")
(display "- Territory control games\n\n")

(display "Ready for more? Check out Tutorial 2: Unit Placement and Movement!\n")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  Tutorial 1 Complete! You now understand hexagonal boards.\n")
(display "═══════════════════════════════════════════════════════════════\n")
