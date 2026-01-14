;;; playpen/boardcraft/examples/tutorial-1-hex-simple.ss
;;; BoardCraft Tutorial 1: Simple Hexagonal Board Introduction

(load "lattice/tiles/boardcraft.ss")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  BOARDCRAFT TUTORIAL 1: SIMPLE HEXAGONAL BOARDS\n")
(display "═══════════════════════════════════════════════════════════════\n\n")

(display "Welcome to BoardCraft! This tutorial introduces hexagonal boards.\n\n")

;;; ====
;;; Part 1: Creating a Hexagonal Board
;;; ====

(display "1. CREATING A HEXAGONAL BOARD\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "Let's create a small hexagonal board with radius 2:\n\n")

;; Create a hex board - radius 2 means 2 rings around center
(define board (make-hex-board 'axial 2))

(display "✓ Created hexagonal board with radius 2\n")
(display "✓ Using axial coordinate system (q, r)\n\n")

;;; ====
;;; Part 2: Working with Coordinates
;;; ====

(display "2. WORKING WITH COORDINATES\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "Axial coordinates use (q, r) where q is like column and r is like row.\n\n")

;; Create some coordinates
(define center (axial-coord 0 0))
(define northeast (axial-coord 1 -1))
(define southeast (axial-coord 1 0))

(display "Coordinates:\n")
(display "  Center: ")
(display center)
(display "\n")
(display "  Northeast: ")
(display northeast)
(display "\n")
(display "  Southeast: ")
(display southeast)
(display "\n\n")

;;; ====
;;; Part 3: Finding Neighbors
;;; ====

(display "3. FINDING NEIGHBORS\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "Each hexagon has exactly 6 neighbors. Let's find them:\n\n")

(define neighbors (hex-neighbors center))

(display "Neighbors of center hex (0, 0):\n")
(for-each (lambda (neighbor)
                  (display "  ")
                  (display neighbor)
                  (newline))
          neighbors)

(display "\nTotal neighbors: ")
(display (length neighbors))
(display "\n\n")

;;; ====
;;; Part 4: Distance Calculations
;;; ====

(display "4. DISTANCE CALCULATIONS\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "Distance is measured in "hex steps" - the minimum moves needed.\n\n")

(define hex1 (axial-coord 0 0))
(define hex2 (axial-coord 2 1))

(display "Distance from ")
(display hex1)
(display " to ")
(display hex2)
(display ": ")
(display (hex-distance hex1 hex2))
(display " steps\n\n")

;;; ====
;;; Part 5: Range and Ring Calculations
;;; ====

(display "5. RANGE AND RING CALCULATIONS\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "Find all hexes within a certain distance (range):\n\n")

(define nearby (hex-range center 1))
(display "All hexes within 1 step of center: ")
(display (length nearby))
(display " hexes\n")
(display "Positions: ")
(display nearby)
(display "\n\n")

(display "Find hexes exactly 2 steps away (ring):\n\n")

(define ring2 (hex-ring center 2))
(display "Hexes exactly 2 steps away: ")
(display (length ring2))
(display " hexes\n")
(display "Positions: ")
(display ring2)
(display "\n\n")

;;; ====
;;; Part 6: Working with Tiles
;;; ====

(display "6. WORKING WITH TILES\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "Each hex can contain tile data. Let's create some tiles:\n\n")

;; Create different types of tiles
(define grass-tile (make-tile 'grass '((walkable . #t))))
(define mountain-tile (make-tile 'mountain '((walkable . #f))))
(define water-tile (make-tile 'water '((walkable . #f) (color . blue))))

(display "Created tiles:\n")
(display "  Grass tile: walkable=")
(display (tile-walkable? grass-tile))
(display "\n")

(display "  Mountain tile: walkable=")
(display (tile-walkable? mountain-tile))
(display "\n")

(display "  Water tile: walkable=")
(display (tile-walkable? water-tile))
(display "\n\n")

(display "You can place tiles on the board using board-set:\n")
(display "  (board-set board position tile-data)\n\n")

;;; ====
;;; Summary and Next Steps
;;; ====

(display "7. SUMMARY\n")
(display "───────────────────────────────────────────────────────────────\n\n")

(display "🎉 Congratulations! You've learned the basics of hexagonal boards:\n\n")
(display "✓ How to create hexagonal boards\n")
(display "✓ How to work with axial coordinates\n")
(display "✓ How to find neighbors (6 per hex)\n")
(display "✓ How to calculate distances\n")
(display "✓ How to work with ranges and rings\n")
(display "✓ How to create and use tiles\n\n")

(display "This foundation lets you build games like:\n")
(display "- Settlers of Catan-style strategy games\n")
(display "- Civilization-style empire builders\n")
(display "- Tactical wargames\n")
(display "- Territory control games\n\n")

(display "Ready for Tutorial 2? We'll add units and movement!\n")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  Tutorial 1 Complete! You understand hexagonal boards.\n")
(display "═══════════════════════════════════════════════════════════════\n")
