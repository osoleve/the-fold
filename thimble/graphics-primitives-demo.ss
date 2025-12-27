;;; Demo/showcase for graphics primitives
;;; Run this to see visual examples of all graphics features

(import (chezscheme)
        (shell layout)
        (shell easing)
        (shell graphics-primitives))

(display "╔════════════════════════════════════════════════════════════════╗\n")
(display "║      GRAPHICS PRIMITIVES DEMONSTRATION                        ║\n")
(display "╚════════════════════════════════════════════════════════════════╝\n\n")

;;; ============================================================
;;; Line Drawing
;;; ============================================================

(display "─────────────────────────────────────────────────────────────────\n")
(display " 1. LINE DRAWING\n")
(display "─────────────────────────────────────────────────────────────────\n\n")

(display "Basic lines (horizontal, vertical, diagonal):\n")
(let* ([canvas (make-canvas 40 15)]
       [canvas (draw-line canvas (point 2 2) (point 37 2) #\─)]
       [canvas (draw-line canvas (point 5 4) (point 5 12) #\│)]
       [canvas (draw-line canvas (point 10 4) (point 20 12) #\/)]
       [canvas (draw-line canvas (point 30 4) (point 20 12) #\\)])
  (display (canvas->string canvas))
  (newline))

(display "\nGradient line (using character intensity):\n")
(let* ([canvas (make-canvas 60 5)]
       [start (point 2 2)]
       [end (point 57 2)]
       [canvas (draw-line-gradient canvas start end intensity-palette-simple)])
  (display (canvas->string canvas))
  (newline))

;;; ============================================================
;;; Box Drawing
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────────────\n")
(display " 2. ROUNDED BOXES\n")
(display "─────────────────────────────────────────────────────────────────\n\n")

(display "ASCII rounded corners:\n")
(let* ([canvas (make-canvas 30 7)]
       [rect (make-rect (point 2 1) 26 5)]
       [canvas (draw-rounded-box canvas rect 'ascii)])
  (display (canvas->string canvas))
  (newline))

(display "\nLight Unicode rounded corners:\n")
(let* ([canvas (make-canvas 30 7)]
       [rect (make-rect (point 2 1) 26 5)]
       [canvas (draw-rounded-box canvas rect 'light)])
  (display (canvas->string canvas))
  (newline))

;;; ============================================================
;;; Circle Drawing
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────────────\n")
(display " 3. CIRCLES\n")
(display "─────────────────────────────────────────────────────────────────\n\n")

(display "Circle outline (radius 8):\n")
(let* ([canvas (make-canvas 30 19)]
       [center (point 15 9)]
       [canvas (draw-circle canvas center 8 #\○)])
  (display (canvas->string canvas))
  (newline))

(display "\nFilled circle (radius 6):\n")
(let* ([canvas (make-canvas 30 15)]
       [center (point 15 7)]
       [canvas (draw-filled-circle canvas center 6 #\●)])
  (display (canvas->string canvas))
  (newline))

;;; ============================================================
;;; Gradient Fills
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────────────\n")
(display " 4. GRADIENT FILLS\n")
(display "─────────────────────────────────────────────────────────────────\n\n")

(display "Horizontal gradient (linear easing):\n")
(let* ([canvas (make-canvas 60 8)]
       [rect (make-rect (point 0 0) 60 8)]
       [canvas (gradient-fill-horizontal canvas rect 0.0 1.0 ease-linear intensity-palette-simple)])
  (display (canvas->string canvas))
  (newline))

(display "\nHorizontal gradient (ease-in-out-cubic):\n")
(let* ([canvas (make-canvas 60 8)]
       [rect (make-rect (point 0 0) 60 8)]
       [canvas (gradient-fill-horizontal canvas rect 0.0 1.0 ease-in-out-cubic intensity-palette-simple)])
  (display (canvas->string canvas))
  (newline))

(display "\nVertical gradient with Unicode blocks (ease-out-quad):\n")
(let* ([canvas (make-canvas 40 16)]
       [rect (make-rect (point 0 0) 40 16)]
       [canvas (gradient-fill-vertical canvas rect 0.0 1.0 ease-out-quad intensity-palette-blocks)])
  (display (canvas->string canvas))
  (newline))

(display "\nRadial gradient (ease-in-sine):\n")
(let* ([canvas (make-canvas 50 22)]
       [center (point 25 11)]
       [canvas (gradient-fill-radial canvas center 15 intensity-palette-simple ease-in-sine)])
  (display (canvas->string canvas))
  (newline))

;;; ============================================================
;;; Combined Effects
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────────────\n")
(display " 5. COMBINED EFFECTS\n")
(display "─────────────────────────────────────────────────────────────────\n\n")

(display "Box with gradient fill:\n")
(let* ([canvas (make-canvas 50 16)]
       [outer-rect (make-rect (point 2 1) 46 14)]
       [inner-rect (make-rect (point 4 3) 42 10)]
       [canvas (draw-rounded-box canvas outer-rect 'light)]
       [canvas (gradient-fill-horizontal canvas inner-rect 0.0 1.0 ease-in-out-sine intensity-palette-simple)])
  (display (canvas->string canvas))
  (newline))

(display "\nCircle with radial gradient:\n")
(let* ([canvas (make-canvas 40 20)]
       [center (point 20 10)]
       ;; First fill with radial gradient
       [canvas (gradient-fill-radial canvas center 12 intensity-palette-blocks ease-out-cubic)]
       ;; Then overlay circle outline
       [canvas (draw-circle canvas center 12 #\○)])
  (display (canvas->string canvas))
  (newline))

(display "\nCrisscross gradient lines:\n")
(let* ([canvas (make-canvas 50 20)]
       [canvas (draw-line-gradient canvas (point 5 5) (point 45 5) intensity-palette-simple)]
       [canvas (draw-line-gradient canvas (point 5 10) (point 45 10) intensity-palette-simple)]
       [canvas (draw-line-gradient canvas (point 5 15) (point 45 15) intensity-palette-simple)]
       [canvas (draw-line-gradient canvas (point 10 2) (point 10 17) intensity-palette-blocks)]
       [canvas (draw-line-gradient canvas (point 25 2) (point 25 17) intensity-palette-blocks)]
       [canvas (draw-line-gradient canvas (point 40 2) (point 40 17) intensity-palette-blocks)])
  (display (canvas->string canvas))
  (newline))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n╔════════════════════════════════════════════════════════════════╗\n")
(display "║  All graphics primitives demonstrated successfully!           ║\n")
(display "║                                                                ║\n")
(display "║  Features:                                                     ║\n")
(display "║   ✓ Line drawing (Bresenham's algorithm)                      ║\n")
(display "║   ✓ Gradient lines with easing                                ║\n")
(display "║   ✓ Rounded boxes (multiple styles)                           ║\n")
(display "║   ✓ Circle outlines and filled circles                        ║\n")
(display "║   ✓ Gradient fills (horizontal, vertical, radial)             ║\n")
(display "║   ✓ Easing integration for smooth effects                     ║\n")
(display "╚════════════════════════════════════════════════════════════════╝\n")
