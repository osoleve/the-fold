;;; playpen/boardcraft/examples/hex-demo.ss — Hexagonal Board Demo
;;;
;;; Demonstrates hex board creation, neighbor finding, and distance calculation.
;;;
;;; Usage:
;;;   scheme --script playpen/boardcraft/examples/hex-demo.ss

(load "playpen/boardcraft/boardcraft.ss")

;;; Helper function
(define (take lst n)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(display "═══════════════════════════════════════════════════════════════\n")
(display "  BOARDCRAFT HEX DEMO\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

;;; ============================================================
;;; Example 1: Basic Hex Coordinates
;;; ============================================================

(display "Example 1: Hexagonal Coordinates\n")
(display "──────────────────────────────────\n")

(define center (axial-coord 0 0))
(display "Center hex: ")
(display center)
(newline)

(display "6 neighbors of center:\n")
(let ([neighbors (hex-neighbors center)])
  (for-each
    (lambda (n)
      (display "  ")
      (display n)
      (newline))
    neighbors))
(newline)

;;; ============================================================
;;; Example 2: Distance Calculation
;;; ============================================================

(display "Example 2: Distance Calculation\n")
(display "──────────────────────────────────\n")

(define hex-a (axial-coord 0 0))
(define hex-b (axial-coord 3 2))

(display "Hex A: ")
(display hex-a)
(newline)
(display "Hex B: ")
(display hex-b)
(newline)
(display "Distance A→B: ")
(display (hex-distance hex-a hex-b))
(display " steps\n")
(newline)

;;; ============================================================
;;; Example 3: Line Drawing
;;; ============================================================

(display "Example 3: Line from A to B\n")
(display "──────────────────────────────────\n")

(let ([line (hex-line hex-a hex-b)])
  (display "Path: ")
  (for-each
    (lambda (hex)
      (display hex)
      (display " "))
    line)
  (newline))
(newline)

;;; ============================================================
;;; Example 4: Range Calculation
;;; ============================================================

(display "Example 4: Range Calculation\n")
(display "──────────────────────────────────\n")

(display "Hexes within 2 steps of center:\n")
(let ([hexes (hex-range center 2)])
  (display "  Count: ")
  (display (length hexes))
  (newline)
  (display "  Hexes: ")
  (for-each
    (lambda (h)
      (display h)
      (display " "))
    (take hexes 10))
  (display "...\n"))
(newline)

;;; ============================================================
;;; Example 5: Ring Pattern
;;; ============================================================

(display "Example 5: Ring at Distance 2\n")
(display "──────────────────────────────────\n")

(let ([ring (hex-ring center 2)])
  (display "  Count: ")
  (display (length ring))
  (newline)
  (display "  Expected: 12 hexes (6 × 2)\n"))
(newline)

;;; ============================================================
;;; Example 6: Board Creation
;;; ============================================================

(display "Example 6: Create Hex Board\n")
(display "──────────────────────────────────\n")

(define floor-tile (make-tile 'floor '((walkable . #t))))
(define board (make-hex-board 'axial 3 floor-tile))

(display "Board created with radius 3\n")
(display "  Total tiles: ")
(display (board-size board))
(newline)
(display "  Expected: 37 hexes (1 + 6 + 12 + 18)\n")
(newline)

;;; ============================================================
;;; Example 7: Coordinate System Conversion
;;; ============================================================

(display "Example 7: Coordinate Conversions\n")
(display "──────────────────────────────────\n")

(define ax (axial-coord 2 1))
(display "Axial: ")
(display ax)
(newline)

(define cu (axial->cubic ax))
(display "Cubic: x=")
(display (cubic-coord%-x cu))
(display " y=")
(display (cubic-coord%-y cu))
(display " z=")
(display (cubic-coord%-z cu))
(newline)

(display "Verify x+y+z=0: ")
(display (+ (cubic-coord%-x cu) (cubic-coord%-y cu) (cubic-coord%-z cu)))
(newline)

(define off (axial->offset ax))
(display "Offset: ")
(display off)
(newline)

(define back (offset->axial off))
(display "Back to axial: ")
(display back)
(newline)
(newline)

;;; ============================================================
;;; Example 8: Rotation
;;; ============================================================

(display "Example 8: Hex Rotation\n")
(display "──────────────────────────────────\n")

(define origin (axial-coord 0 0))
(define point (axial-coord 2 0))

(display "Original point: ")
(display point)
(newline)

(display "Rotated 60° left: ")
(display (hex-rotate-left origin point))
(newline)

(display "Rotated 60° right: ")
(display (hex-rotate-right origin point))
(newline)
(newline)

;;; ============================================================
;;; Summary
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "  Demo Complete!\n")
(display "═══════════════════════════════════════════════════════════════\n")
(display "\nHexagonal boards are perfect for:\n")
(display "  • Strategy games (Civilization, Settlers of Catan)\n")
(display "  • Wargames (Panzer General, Battle for Wesnoth)\n")
(display "  • Territory control games\n")
(display "  • Any game where 6-way symmetry is desired\n")
(newline)
