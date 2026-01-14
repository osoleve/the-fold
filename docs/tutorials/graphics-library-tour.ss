;;; graphics-library-tour.ss — A Whirlwind Tour of The Fold's Graphics Library
;;;
;;; Welcome, traveler! This tutorial will take you on a journey through
;;; the declarative graphics system. Along the way you'll meet:
;;;
;;;   - Shape primitives (the building blocks)
;;;   - Transforms (bend reality to your will)
;;;   - Pattern fills (infinite tessellations)
;;;   - Connection combinators (draw relationships)
;;;   - Layout combinators (compose with elegance)
;;;
;;; Run this file through the Fold REPL daemon!
;;;
;;; Usage:
;;;   ./daemon.sh start
;;;   ./fold-agent.py docs/tutorials/graphics-library-tour.ss
;;;
;;; Or load interactively in the REPL.

;;; ====
;;; Setup: Load the graphics libraries
;;; ====

(load "core/base/prelude.ss")
(load "shell/ui/layout.ss")
(import (shell easing))
(import (shell graphics-primitives))
(import (shell layout-combinators))
(import (shell transforms))

(display "\n")
(display "╔══════════════════════════════════════════════════════════════╗\n")
(display "║     THE FOLD GRAPHICS LIBRARY - A VISUAL TOUR               ║\n")
(display "╚══════════════════════════════════════════════════════════════╝\n\n")

;;; ====
;;; Chapter 1: The Canvas - Your Blank Slate
;;; ====

(display "═══ CHAPTER 1: The Canvas ═══\n\n")
(display "Everything starts with a canvas - a grid of characters.\n")
(display "Let's create a 30x10 canvas and draw a simple box:\n\n")

(define canvas1 (make-canvas 30 10))
(define canvas1 (draw-box canvas1 (make-rect (point 2 1) 26 8) 'light))
(define canvas1 (draw-string canvas1 (point 10 4) "Hello, Fold!"))

(canvas-display canvas1)
(display "\n")

;;; ====
;;; Chapter 2: Shapes - The Building Blocks
;;; ====

(display "═══ CHAPTER 2: Shapes ═══\n\n")
(display "Circles, lines, polygons - oh my!\n\n")

(define canvas2 (make-canvas 40 15))

;; Draw a circle
(define canvas2 (draw-circle canvas2 (point 10 7) 5 #\*))

;; Draw a filled rectangle
(define canvas2 (fill-rect canvas2 (make-rect (point 25 3) 12 8) #\░))
(define canvas2 (draw-box canvas2 (make-rect (point 25 3) 12 8) 'heavy))

;; Draw a polyline (a zigzag!)
(define canvas2 (draw-polyline canvas2
                               (list (point 18 2) (point 22 5) (point 18 8) (point 22 11))
                               #\#))

(canvas-display canvas2)
(display "\n")
(display "A circle, a box, and a zigzag walk into a canvas...\n\n")

;;; ====
;;; Chapter 3: Pattern Fills - Infinite Tessellations
;;; ====

(display "═══ CHAPTER 3: Pattern Fills ═══\n\n")
(display "Why fill with one character when you can tile a pattern?\n\n")

(define canvas3 (make-canvas 50 12))

;; Checkerboard pattern
(define canvas3 (pattern-fill canvas3 (make-rect (point 2 1) 10 10) pattern-checker))

;; Dots pattern
(define canvas3 (pattern-fill canvas3 (make-rect (point 15 1) 10 10) pattern-dots))

;; Diagonal pattern
(define canvas3 (pattern-fill canvas3 (make-rect (point 28 1) 10 10) pattern-diagonal))

;; Hatch pattern
(define canvas3 (pattern-fill canvas3 (make-rect (point 41 1) 8 10) pattern-hatch))

;; Labels
(define canvas3 (draw-string canvas3 (point 3 11) "checker"))
(define canvas3 (draw-string canvas3 (point 17 11) "dots"))
(define canvas3 (draw-string canvas3 (point 29 11) "diagonal"))
(define canvas3 (draw-string canvas3 (point 43 11) "hatch"))

(canvas-display canvas3)
(display "\n")
(display "Each pattern tiles infinitely. Create your own with make-pattern!\n\n")

;;; ====
;;; Chapter 4: Connection Combinators - Drawing Relationships
;;; ====

(display "═══ CHAPTER 4: Connections ═══\n\n")
(display "Connect points with lines, arrows, and smart edges!\n\n")

(define canvas4 (make-canvas 50 16))

;; Draw some boxes to connect
(define canvas4 (draw-box canvas4 (make-rect (point 2 2) 8 4) 'double))
(define canvas4 (draw-string canvas4 (point 4 4) "START"))

(define canvas4 (draw-box canvas4 (make-rect (point 20 2) 8 4) 'double))
(define canvas4 (draw-string canvas4 (point 22 4) "MID"))

(define canvas4 (draw-box canvas4 (make-rect (point 38 2) 8 4) 'double))
(define canvas4 (draw-string canvas4 (point 41 4) "END"))

;; Straight arrow
(define canvas4 (arrow canvas4 (point 10 4) (point 19 4) #\─))

;; Another arrow
(define canvas4 (arrow canvas4 (point 28 4) (point 37 4) #\─))

;; Now show different edge styles
(define canvas4 (draw-box canvas4 (make-rect (point 2 10) 10 4) 'light))
(define canvas4 (draw-string canvas4 (point 4 12) "Source"))

(define canvas4 (draw-box canvas4 (make-rect (point 35 10) 10 4) 'light))
(define canvas4 (draw-string canvas4 (point 38 12) "Dest"))

;; Orthogonal edge (goes horizontal, then vertical, then horizontal)
(define canvas4 (edge canvas4 (point 12 12) (point 34 12) 'orthogonal))

(canvas-display canvas4)
(display "\n")
(display "Edge styles: 'straight, 'horizontal-first, 'vertical-first, 'orthogonal\n\n")

;;; ====
;;; Chapter 5: Layout Combinators - Compose with Elegance
;;; ====

(display "═══ CHAPTER 5: Layout Combinators ═══\n\n")
(display "Combine canvases with beside, above, and atop!\n\n")

;; Create three small labeled boxes
(define box-a (make-canvas 8 4))
(define box-a (draw-box box-a (make-rect (point 0 0) 8 4) 'light))
(define box-a (draw-string box-a (point 2 1) "Box A"))

(define box-b (make-canvas 8 4))
(define box-b (draw-box box-b (make-rect (point 0 0) 8 4) 'light))
(define box-b (draw-string box-b (point 2 1) "Box B"))

(define box-c (make-canvas 8 4))
(define box-c (draw-box box-c (make-rect (point 0 0) 8 4) 'light))
(define box-c (draw-string box-c (point 2 1) "Box C"))

;; beside: place horizontally
(display "beside (horizontal):\n")
(canvas-display (beside (beside box-a box-b 1) box-c 1))

;; above: stack vertically
(display "\nabove (vertical):\n")
(canvas-display (above (above box-a box-b 0) box-c 0))

;; hcat: horizontal concatenation of a list
(display "\nhcat with spacing:\n")
(canvas-display (hcat (list box-a box-b box-c) 2))

(display "\n")

;;; ====
;;; Chapter 6: Transforms - Bend Reality
;;; ====

(display "═══ CHAPTER 6: Transforms ═══\n\n")
(display "Translate, rotate, scale, flip, and SKEW!\n\n")

;; Create a simple shape to transform
(define arrow-shape (make-canvas 7 3))
(define arrow-shape (draw-string arrow-shape (point 0 1) "-->-->"))

(display "Original arrow:\n")
(canvas-display arrow-shape)

;; Flip horizontal
(define flipped (canvas-flip-horizontal arrow-shape))
(display "\nFlipped horizontal:\n")
(canvas-display flipped)

;; Show transform capabilities
(display "\nTransforms available:\n")
(display "  - canvas-translate: Move content by (dx, dy)\n")
(display "  - canvas-rotate: Rotate around a center point\n")
(display "  - canvas-scale: Scale by (sx, sy) factors\n")
(display "  - canvas-skew: Skew by angles (radians) - NEW!\n")
(display "  - canvas-flip-horizontal/vertical: Mirror content\n")
(display "  - canvas-clip: Extract a rectangular region\n")
(display "  - canvas-mask: Apply a transparency mask\n\n")

;;; ====
;;; Grand Finale: A Complete Diagram
;;; ====

(display "═══ GRAND FINALE: A Complete Diagram ═══\n\n")

(define final (make-canvas 60 20))

;; Title
(define final (draw-string final (point 15 1) "THE FOLD ARCHITECTURE"))
(define final (draw-string final (point 15 2) "════════════════════"))

;; Core box
(define final (draw-box final (make-rect (point 22 5) 14 6) 'double))
(define final (draw-string final (point 26 7) "CORE"))
(define final (draw-string final (point 25 8) "(pure)"))

;; Shell box
(define final (draw-box final (make-rect (point 2 5) 14 6) 'light))
(define final (draw-string final (point 5 7) "SHELL"))
(define final (draw-string final (point 4 8) "(impure)"))

;; User box
(define final (draw-box final (make-rect (point 42 5) 14 6) 'light))
(define final (draw-string final (point 46 7) "USER"))
(define final (draw-string final (point 45 8) "(play)"))

;; CAS store (with pattern fill!)
(define final (draw-box final (make-rect (point 22 14) 14 4) 'heavy))
(define final (pattern-fill final (make-rect (point 23 15) 12 2) pattern-dots))
(define final (draw-string final (point 26 15) "CAS"))

;; Arrows connecting components
(define final (arrow final (point 16 8) (point 21 8) #\─))  ;; Shell -> Core
(define final (arrow final (point 36 8) (point 41 8) #\─))  ;; Core -> User
(define final (edge final (point 29 11) (point 29 13) 'straight))  ;; Core -> CAS (down)
(define final (draw-string final (point 28 12) "v"))

;; Legend
(define final (draw-string final (point 2 18) "Legend: ══ Core boundary  ── Data flow  ·· Storage"))

(canvas-display final)

(display "\n")
(display "╔══════════════════════════════════════════════════════════════╗\n")
(display "║  You've completed the Graphics Library Tour!                 ║\n")
(display "║                                                              ║\n")
(display "║  Key files to explore:                                       ║\n")
(display "║    shell/ui/layout.ss           - Canvas basics              ║\n")
(display "║    shell/ui/graphics-primitives.ss - Shapes & patterns       ║\n")
(display "║    shell/ui/layout-combinators.ss  - beside/above/connect    ║\n")
(display "║    shell/ui/transforms.ss       - Transforms & groups        ║\n")
(display "║    shell/ui/svg-renderer.ss     - Export to SVG              ║\n")
(display "╚══════════════════════════════════════════════════════════════╝\n")
