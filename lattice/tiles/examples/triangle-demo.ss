;;; playpen/boardcraft/examples/triangle-demo.ss — Triangular Tile Demo
;;;
;;; Demonstrates triangular board creation and operations.
;;; Triangles alternate between up (△) and down (▽) orientations.
;;;
;;; Usage:
;;;   scheme --script playpen/boardcraft/examples/triangle-demo.ss

(load "lattice/tiles/boardcraft.ss")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  BOARDCRAFT TRIANGLE DEMO\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

;;; ============================================================
;;; Example 1: Triangle Coordinates and Orientation
;;; ============================================================

(display "Example 1: Triangle Coordinates\n")
(display "──────────────────────────────────\n")

(define tri-up (triangle-coord 0 0 'up))
(define tri-down (triangle-coord 0 0 'down))

(display "Up triangle at (0,0): ")
(display tri-up)
(newline)
(display "Down triangle at (0,0): ")
(display tri-down)
(newline)
(newline)

;;; ============================================================
;;; Example 2: Triangle Flip
;;; ============================================================

(display "Example 2: Flipping Orientation\n")
(display "──────────────────────────────────\n")

(display "Original: ")
(display tri-up)
(display " orientation=")
(display (triangle-orientation tri-up))
(newline)

(define flipped (triangle-flip tri-up))
(display "Flipped: ")
(display flipped)
(display " orientation=")
(display (triangle-orientation flipped))
(newline)
(newline)

;;; ============================================================
;;; Example 3: Edge Neighbors
;;; ============================================================

(display "Example 3: Edge-Adjacent Neighbors\n")
(display "──────────────────────────────────\n")

(define center-up (triangle-coord 2 2 'up))
(display "Up triangle at (2,2):\n")
(let ([neighbors (triangle-neighbors-edge center-up)])
     (display "  3 edge neighbors:\n")
     (for-each
      (lambda (n)
              (display "    ")
              (display n)
              (display " orientation=")
              (display (triangle-orientation n))
              (newline))
      neighbors))
(newline)

(define center-down (triangle-coord 2 2 'down))
(display "Down triangle at (2,2):\n")
(let ([neighbors (triangle-neighbors-edge center-down)])
     (display "  3 edge neighbors:\n")
     (for-each
      (lambda (n)
              (display "    ")
              (display n)
              (display " orientation=")
              (display (triangle-orientation n))
              (newline))
      neighbors))
(newline)

;;; ============================================================
;;; Example 4: Vertex Neighbors
;;; ============================================================

(display "Example 4: Vertex-Adjacent Neighbors\n")
(display "──────────────────────────────────\n")

(let ([vertex-neighbors (triangle-neighbors-vertex center-up)])
     (display "Up triangle at (2,2) has ")
     (display (length vertex-neighbors))
     (display " vertex neighbors:\n")
     (for-each
      (lambda (n)
              (display "  ")
              (display n)
              (newline))
      vertex-neighbors))
(newline)

;;; ============================================================
;;; Example 5: All Neighbors
;;; ============================================================

(display "Example 5: All Neighbors (Edge + Vertex + Flip)\n")
(display "──────────────────────────────────\n")

(let ([all-neighbors (triangle-neighbors-all center-up)])
     (display "Triangle at (2,2,up) has ")
     (display (length all-neighbors))
     (display " total neighbors:\n")
     (display "  (3 edge + 3 vertex + 1 flip)\n"))
(newline)

;;; ============================================================
;;; Example 6: Distance Calculation
;;; ============================================================

(display "Example 6: Distance Between Triangles\n")
(display "──────────────────────────────────\n")

(define t1 (triangle-coord 0 0 'up))
(define t2 (triangle-coord 3 4 'down))

(display "From ")
(display t1)
(newline)
(display "To ")
(display t2)
(newline)
(display "Manhattan distance: ")
(display (triangle-distance-manhattan t1 t2))
(newline)
(display "(This is an approximation; exact distance requires pathfinding)\n")
(newline)

;;; ============================================================
;;; Example 7: Line Drawing
;;; ============================================================

(display "Example 7: Line Between Triangles\n")
(display "──────────────────────────────────\n")

(define start (triangle-coord 0 0 'up))
(define end (triangle-coord 3 3 'up))

(display "Line from ")
(display start)
(display " to ")
(display end)
(display ":\n")
(let ([line (triangle-line start end)])
     (for-each
      (lambda (t)
              (display "  ")
              (display t)
              (newline))
      line))
(newline)

;;; ============================================================
;;; Example 8: Range Calculation
;;; ============================================================

(display "Example 8: Triangles Within Range\n")
(display "──────────────────────────────────\n")

(define origin (triangle-coord 2 2 'up))
(display "Triangles within distance 1 of ")
(display origin)
(display ":\n")
(let ([in-range (triangle-range origin 1)])
     (display "  Count: ")
     (display (length in-range))
     (newline)
     (display "  Triangles:\n")
     (for-each
      (lambda (t)
              (display "    ")
              (display t)
              (newline))
      in-range))
(newline)

;;; ============================================================
;;; Example 9: Board Creation
;;; ============================================================

(display "Example 9: Create Triangular Board\n")
(display "──────────────────────────────────\n")

(define floor-tile (make-tile 'floor '((walkable . #t))))
(define board (make-triangle-board 4 4 floor-tile))

(display "Created 4×4 triangular board\n")
(display "  Rows: ")
(display (triangle-board-rows board))
(newline)
(display "  Cols: ")
(display (triangle-board-cols board))
(newline)
(display "  Total tiles: ")
(display (board-size board))
(newline)
(display "  Expected: 32 triangles (4×4×2, since each position has 2 triangles)\n")
(newline)

;;; ============================================================
;;; Example 10: Bounds Checking
;;; ============================================================

(display "Example 10: Bounds Checking\n")
(display "──────────────────────────────────\n")

(define in-bounds (triangle-coord 2 2 'up))
(define out-bounds (triangle-coord 10 10 'down))

(display "Triangle ")
(display in-bounds)
(display " in bounds? ")
(display (triangle-in-bounds? board in-bounds))
(newline)

(display "Triangle ")
(display out-bounds)
(display " in bounds? ")
(display (triangle-in-bounds? board out-bounds))
(newline)
(newline)

;;; ============================================================
;;; Example 11: Rotation
;;; ============================================================

(display "Example 11: 180° Rotation\n")
(display "──────────────────────────────────\n")

(define original (triangle-coord 1 1 'up))
(display "Original: ")
(display original)
(display " orientation=")
(display (triangle-orientation original))
(newline)

(define rotated (triangle-rotate-180 original 2 2))
(display "Rotated 180° around (2,2): ")
(display rotated)
(display " orientation=")
(display (triangle-orientation rotated))
(newline)
(display "(Note: rotation flips orientation and mirrors position)\n")
(newline)

;;; ============================================================
;;; Example 12: Reflection
;;; ============================================================

(display "Example 12: Reflection\n")
(display "──────────────────────────────────\n")

(define point (triangle-coord 1 1 'up))
(display "Original: ")
(display point)
(newline)

(define h-reflect (triangle-reflect-horizontal point 2))
(display "Horizontal reflection (axis=col 2): ")
(display h-reflect)
(newline)

(define v-reflect (triangle-reflect-vertical point 2))
(display "Vertical reflection (axis=row 2): ")
(display v-reflect)
(display " orientation=")
(display (triangle-orientation v-reflect))
(newline)
(newline)

;;; ============================================================
;;; Example 13: Default Orientation Pattern
;;; ============================================================

(display "Example 13: Alternating Orientation Pattern\n")
(display "──────────────────────────────────\n")

(display "Default orientations in a 3×3 grid:\n")
(let loop-r ([r 0])
     (when (< r 3)
           (display "  Row ")
           (display r)
           (display ": ")
           (let loop-c ([c 0])
                (when (< c 3)
                      (display (triangle-default-orientation r c))
                      (display " ")
                      (loop-c (+ c 1))))
           (newline)
           (loop-r (+ r 1))))
(display "  (Alternates based on (row + col) parity)\n")
(newline)

;;; ============================================================
;;; Summary
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "  Demo Complete!\n")
(display "═══════════════════════════════════════════════════════════════\n")
(display "\nTriangular boards are perfect for:\n")
(display "  • TriHex-style games\n")
(display "  • Tessellation puzzles\n")
(display "  • Games with alternating dual structures\n")
(display "  • Unique strategic movement patterns\n")
(newline)
(display "Key features:\n")
(display "  • Each position contains 2 triangles (up & down)\n")
(display "  • 3 edge neighbors per triangle\n")
(display "  • 3 vertex neighbors per triangle\n")
(display "  • Orientation flipping and rotation\n")
(newline)
