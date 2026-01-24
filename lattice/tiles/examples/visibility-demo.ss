;;; lattice/tiles/examples/visibility-demo.ss — Visibility Demo
;;;
;;; Demonstrates line-of-sight and field-of-view calculations
;;; across different tile shapes.
;;;
;;; Usage:
;;;   scheme --script lattice/tiles/examples/visibility-demo.ss

(load "lattice/tiles/boardcraft.ss")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  BOARDCRAFT VISIBILITY DEMO\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

;;; ====
;;; Example 1: Line of Sight on Square Grid
;;; ====

(display "Example 1: Line of Sight on Square Grid\n")
(display "──────────────────────────────────────────────────────────\n")

;;; Create a board with walls
(define floor (make-tile 'floor '((walkable . #t) (blocks-vision . #f))))
(define wall (make-tile 'wall '((walkable . #f) (blocks-vision . #t))))

(define los-board (make-square-board 10 10 floor))

;;; Add a wall
(define los-board
  (board-set (board-set (board-set los-board
                                   (square-coord 5 3) wall)
                        (square-coord 5 4) wall)
             (square-coord 5 5) wall))

(define observer (square-coord 2 4))
(define target1 (square-coord 8 4))
(define target2 (square-coord 8 2))

(display "Observer at ")
(display observer)
(newline)

(display "Target 1 at ")
(display target1)
(display " (blocked by wall): ")
(display (if (board-has-los? los-board observer target1 square-line)
             "VISIBLE"
             "BLOCKED"))
(newline)

(display "Target 2 at ")
(display target2)
(display " (clear line): ")
(display (if (board-has-los? los-board observer target2 square-line)
             "VISIBLE"
             "BLOCKED"))
(newline)
(newline)

;;; ====
;;; Example 2: Field of View on Square Grid
;;; ====

(display "Example 2: Field of View on Square Grid\n")
(display "──────────────────────────────────────────────────────────\n")

;;; Create a room with walls
(define fov-board (make-square-board 12 12 floor))

;;; Add walls in a pattern (creating rooms)
(define fov-board
  (let loop ([y 0] [b fov-board])
       (if (> y 11)
           b
           (loop (+ y 1)
                 (board-set (board-set b
                                       (square-coord 6 y) wall)
                            (square-coord 7 y) wall)))))

;;; Add doorway
(define fov-board
  (board-set (board-set fov-board
                        (square-coord 6 5) floor)
             (square-coord 6 6) floor))

(define viewer (square-coord 3 5))

(display "Viewer at ")
(display viewer)
(newline)

(define visible-tiles (board-fov fov-board viewer 8
                                 (lambda (c) (square-neighbors c 'all))
                                 square-line))

(display "Can see ")
(display (length visible-tiles))
(display " tiles within range 8\n")
(display "(showing first 10): ")
(let loop ([tiles visible-tiles] [count 0])
     (when (and (pair? tiles) (< count 10))
           (display (car tiles))
           (display " ")
           (loop (cdr tiles) (+ count 1))))
(newline)
(newline)

;;; ====
;;; Example 3: Line of Sight on Hex Grid
;;; ====

(display "Example 3: Line of Sight on Hexagonal Grid\n")
(display "──────────────────────────────────────────────────────────\n")

(define hex-board (make-hex-board 'axial 6 floor))

;;; Add walls
(define hex-board
  (board-set (board-set (board-set hex-board
                                   (axial-coord 0 0) wall)
                        (axial-coord 1 0) wall)
             (axial-coord 0 1) wall))

(define hex-observer (axial-coord -2 -1))
(define hex-target1 (axial-coord 3 2))
(define hex-target2 (axial-coord -1 3))

(display "Observer at ")
(display hex-observer)
(newline)

(display "Target at ")
(display hex-target1)
(display ": ")
(display (if (board-has-los? hex-board hex-observer hex-target1 hex-line)
             "VISIBLE"
             "BLOCKED"))
(newline)

(display "Target at ")
(display hex-target2)
(display ": ")
(display (if (board-has-los? hex-board hex-observer hex-target2 hex-line)
             "VISIBLE"
             "BLOCKED"))
(newline)
(newline)

;;; ====
;;; Example 4: Field of View on Hex Grid
;;; ====

(display "Example 4: Field of View on Hexagonal Grid\n")
(display "──────────────────────────────────────────────────────────\n")

(define hex-fov-board (make-hex-board 'axial 5 floor))

(define hex-viewer (axial-coord 0 0))

(define hex-visible (board-fov hex-fov-board hex-viewer 3
                               hex-neighbors
                               hex-line))

(display "Viewer at ")
(display hex-viewer)
(newline)
(display "Can see ")
(display (length hex-visible))
(display " hexes within range 3\n")
(newline)

;;; ====
;;; Example 5: Tactical Visibility
;;; ====

(display "Example 5: Tactical Visibility (Finding Visible Enemies)\n")
(display "──────────────────────────────────────────────────────────\n")

(define tactical-board (make-square-board 15 15 floor))

;;; Add some walls
(define tactical-board
  (board-set (board-set (board-set tactical-board
                                   (square-coord 7 5) wall)
                        (square-coord 7 6) wall)
             (square-coord 7 7) wall))

(define player-pos (square-coord 3 6))
(define enemy-positions
  (list (square-coord 5 6)    ; Visible
        (square-coord 10 6)   ; Behind wall
        (square-coord 4 8)))  ; Visible

(display "Player at ")
(display player-pos)
(newline)
(display "Enemy positions: ")
(for-each (lambda (e) (display e) (display " ")) enemy-positions)
(newline)

(define visible-enemies
  (visible-enemies tactical-board player-pos 10
                   (lambda (c) (square-neighbors c 'all))
                   square-line
                   (lambda (coord)
                           (member coord enemy-positions))))

(display "Visible enemies: ")
(if (null? visible-enemies)
    (display "none")
    (for-each (lambda (e) (display e) (display " ")) visible-enemies))
(newline)
(display "(should see enemies at ")
(display (car enemy-positions))
(display " and ")
(display (caddr enemy-positions))
(display ")\n")
(newline)

;;; ====
;;; Summary
;;; ====

(display "═══════════════════════════════════════════════════════════════\n")
(display "  Demo Complete!\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)
(display "Visibility systems work across all tile shapes:\n")
(display "  ✓ Line of sight checking\n")
(display "  ✓ Field of view calculation\n")
(display "  ✓ Vision blocking tiles\n")
(display "  ✓ Tactical visibility (finding visible enemies)\n")
(newline)
(display "Use cases:\n")
(display "  • Fog of war systems\n")
(display "  • Tactical combat games\n")
(display "  • Stealth mechanics\n")
(display "  • Cover systems\n")
(newline)
