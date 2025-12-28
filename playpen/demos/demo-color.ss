;;; demo-color.ss — Color System Demonstration
;;; Run with: scheme --script demo-color.ss
;;;
;;; Shows off the ANSI color capabilities of the canvas system.

;;; Load color-enabled layout
(load "thimble/layout-color.ss")

;;; ============================================================
;;; Demo 1: Basic Colors
;;; ============================================================

(define (demo-basic-colors)
  (display "\n")
  (display "═══════════════════════════════════════════\n")
  (display "  DEMO 1: Basic 256-Color Palette\n")
  (display "═══════════════════════════════════════════\n\n")

  (let* ([c (make-canvas 50 12)]
         ;; Title
         [c (draw-string-colored c (point 2 1) "Basic ANSI Colors:"
                                color-bright-white color-default)]
         ;; Draw color swatches
         [c (draw-string-colored c (point 2 3) "  RED   "
                                color-white color-red)]
         [c (draw-string-colored c (point 12 3) " GREEN "
                                color-white color-green)]
         [c (draw-string-colored c (point 22 3) " BLUE  "
                                color-white color-blue)]
         [c (draw-string-colored c (point 32 3) " YELLOW "
                                color-black color-yellow)]

         [c (draw-string-colored c (point 2 5) " MAGENTA "
                                color-white color-magenta)]
         [c (draw-string-colored c (point 12 5) "  CYAN  "
                                color-black color-cyan)]
         [c (draw-string-colored c (point 22 5) " ORANGE "
                                color-black color-orange)]
         [c (draw-string-colored c (point 32 5) "  PINK  "
                                color-black color-pink)]

         ;; Foreground colors
         [c (draw-string-colored c (point 2 7) "Text in "
                                color-default color-default)]
         [c (draw-string-colored c (point 10 7) "different "
                                color-bright-red color-default)]
         [c (draw-string-colored c (point 20 7) "colors!"
                                color-bright-blue color-default)]

         ;; Border
         [c (draw-box-colored c (make-rect (point 0 0) 50 12)
                             'light color-cyan color-default)])

    (display (canvas->string c))
    (newline)))

;;; ============================================================
;;; Demo 2: RGB Truecolor
;;; ============================================================

(define (demo-truecolor)
  (display "\n")
  (display "═══════════════════════════════════════════\n")
  (display "  DEMO 2: RGB Truecolor (24-bit)\n")
  (display "═══════════════════════════════════════════\n\n")

  (let* ([c (make-canvas 50 14)]
         ;; Title
         [c (draw-string-colored c (point 2 1) "24-bit RGB Colors:"
                                (rgb 255 255 255) color-default)]

         ;; Color gradient - horizontal
         [c (draw-string-colored c (point 2 3) "Gradient:"
                                color-default color-default)]

         ;; Red to Yellow gradient
         [c (let loop ([i 0] [canvas c])
              (if (>= i 40)
                  canvas
                  (let* ([t (/ i 39.0)]
                         [r 255]
                         [g (inexact->exact (round (* 255 t)))]
                         [b 0]
                         [color (rgb r g b)])
                    (loop (+ i 1)
                          (draw-char-colored canvas (point (+ 5 i) 5)
                                           #\█ color color)))))]

         ;; Blue to Cyan gradient
         [c (let loop ([i 0] [canvas c])
              (if (>= i 40)
                  canvas
                  (let* ([t (/ i 39.0)]
                         [r 0]
                         [g (inexact->exact (round (* 255 t)))]
                         [b 255]
                         [color (rgb r g b)])
                    (loop (+ i 1)
                          (draw-char-colored canvas (point (+ 5 i) 7)
                                           #\█ color color)))))]

         ;; Purple to Pink gradient
         [c (let loop ([i 0] [canvas c])
              (if (>= i 40)
                  canvas
                  (let* ([t (/ i 39.0)]
                         [r (inexact->exact (round (+ 128 (* 127 t))))]
                         [g 0]
                         [b (inexact->exact (round (- 255 (* 128 t))))]
                         [color (rgb r g b)])
                    (loop (+ i 1)
                          (draw-char-colored canvas (point (+ 5 i) 9)
                                           #\█ color color)))))]

         ;; Border
         [c (draw-box-colored c (make-rect (point 0 0) 50 14)
                             'light (rgb 100 200 255) color-default)])

    (display (canvas->string c))
    (newline)))

;;; ============================================================
;;; Demo 3: Colored Shapes
;;; ============================================================

(define (demo-shapes)
  (display "\n")
  (display "═══════════════════════════════════════════\n")
  (display "  DEMO 3: Colored Shapes & Patterns\n")
  (display "═══════════════════════════════════════════\n\n")

  (let* ([c (make-canvas 60 16)]
         ;; Title
         [c (draw-string-colored c (point 2 1) "Filled Rectangles:"
                                color-bright-yellow color-default)]

         ;; Red square
         [c (fill-rect-colored c (make-rect (point 5 3) 8 6)
                              #\░ color-bright-red color-default)]

         ;; Green square
         [c (fill-rect-colored c (make-rect (point 16 3) 8 6)
                              #\▒ color-bright-green color-default)]

         ;; Blue square
         [c (fill-rect-colored c (make-rect (point 27 3) 8 6)
                              #\▓ color-bright-blue color-default)]

         ;; Yellow square
         [c (fill-rect-colored c (make-rect (point 38 3) 8 6)
                              #\█ color-bright-yellow color-default)]

         ;; Colored boxes
         [c (draw-box-colored c (make-rect (point 5 10) 12 5)
                             'heavy color-red color-default)]
         [c (draw-string-colored c (point 7 12) "  RED  "
                                color-red color-default)]

         [c (draw-box-colored c (make-rect (point 20 10) 12 5)
                             'heavy color-green color-default)]
         [c (draw-string-colored c (point 22 12) " GREEN "
                                color-green color-default)]

         [c (draw-box-colored c (make-rect (point 35 10) 12 5)
                             'heavy color-blue color-default)]
         [c (draw-string-colored c (point 37 12) " BLUE  "
                                color-blue color-default)]

         ;; Outer border
         [c (draw-box-colored c (make-rect (point 0 0) 60 16)
                             'double (rgb 200 150 255) color-default)])

    (display (canvas->string c))
    (newline)))

;;; ============================================================
;;; Demo 4: Mood Colors
;;; ============================================================

(define (demo-mood-colors)
  (display "\n")
  (display "═══════════════════════════════════════════\n")
  (display "  DEMO 4: DUCKIE Mood Colors\n")
  (display "═══════════════════════════════════════════\n\n")

  (let* ([c (make-canvas 50 20)]
         ;; Title
         [c (draw-string-colored c (point 2 1) "Mood-Based Color System:"
                                color-bright-white color-default)]

         ;; Happy - Gold/Yellow
         [happy-color (mood->color 'happy)]
         [c (fill-rect-colored c (make-rect (point 5 3) 12 2)
                              #\█ happy-color happy-color)]
         [c (draw-string-colored c (point 7 4) " HAPPY "
                                color-black happy-color)]
         [c (draw-string-colored c (point 20 4) "Bright, cheerful"
                                happy-color color-default)]

         ;; Curious - Light Blue
         [curious-color (mood->color 'curious)]
         [c (fill-rect-colored c (make-rect (point 5 6) 12 2)
                              #\█ curious-color curious-color)]
         [c (draw-string-colored c (point 6 7) " CURIOUS "
                                color-black curious-color)]
         [c (draw-string-colored c (point 20 7) "Inquisitive"
                                curious-color color-default)]

         ;; Sleepy - Soft Purple
         [sleepy-color (mood->color 'sleepy)]
         [c (fill-rect-colored c (make-rect (point 5 9) 12 2)
                              #\█ sleepy-color sleepy-color)]
         [c (draw-string-colored c (point 7 10) " SLEEPY "
                                color-black sleepy-color)]
         [c (draw-string-colored c (point 20 10) "Drowsy, calm"
                                sleepy-color color-default)]

         ;; Content - Soft Green
         [content-color (mood->color 'content)]
         [c (fill-rect-colored c (make-rect (point 5 12) 12 2)
                              #\█ content-color content-color)]
         [c (draw-string-colored c (point 6 13) " CONTENT "
                                color-black content-color)]
         [c (draw-string-colored c (point 20 13) "Peaceful"
                                content-color color-default)]

         ;; Playful - Pink
         [playful-color (mood->color 'playful)]
         [c (fill-rect-colored c (make-rect (point 5 15) 12 2)
                              #\█ playful-color playful-color)]
         [c (draw-string-colored c (point 6 16) " PLAYFUL "
                                color-black playful-color)]
         [c (draw-string-colored c (point 20 16) "Energetic, fun"
                                playful-color color-default)]

         ;; Border
         [c (draw-box-colored c (make-rect (point 0 0) 50 20)
                             'light color-bright-magenta color-default)])

    (display (canvas->string c))
    (newline)))

;;; ============================================================
;;; Demo 5: Energy Gradient
;;; ============================================================

(define (demo-energy-gradient)
  (display "\n")
  (display "═══════════════════════════════════════════\n")
  (display "  DEMO 5: Energy Level Visualization\n")
  (display "═══════════════════════════════════════════\n\n")

  (let* ([c (make-canvas 50 10)]
         ;; Title
         [c (draw-string-colored c (point 2 1) "Energy: 0% → 100%"
                                color-bright-white color-default)]

         ;; Energy bar with gradient
         [c (let loop ([e 0] [canvas c])
              (if (> e 100)
                  canvas
                  (let* ([x (+ 5 (quotient (* e 35) 100))]
                         [color (energy->color e)])
                    (loop (+ e 3)
                          (draw-char-colored canvas (point x 4)
                                           #\█ color color)))))]

         ;; Labels
         [c (draw-string-colored c (point 5 6) "0%"
                                (energy->color 0) color-default)]
         [c (draw-string-colored c (point 20 6) "50%"
                                (energy->color 50) color-default)]
         [c (draw-string-colored c (point 38 6) "100%"
                                (energy->color 100) color-default)]

         ;; Border
         [c (draw-box-colored c (make-rect (point 0 0) 50 10)
                             'light color-yellow color-default)])

    (display (canvas->string c))
    (newline)))

;;; ============================================================
;;; Demo 6: Color Helpers
;;; ============================================================

(define (demo-color-helpers)
  (display "\n")
  (display "═══════════════════════════════════════════\n")
  (display "  DEMO 6: Color Helper Functions\n")
  (display "═══════════════════════════════════════════\n\n")

  (let* ([c (make-canvas 50 14)]
         [base-color (rgb 200 100 150)]

         ;; Title
         [c (draw-string-colored c (point 2 1) "Base Color:"
                                color-default color-default)]
         [c (fill-rect-colored c (make-rect (point 15 1) 10 1)
                              #\█ base-color base-color)]

         ;; Lightened versions
         [c (draw-string-colored c (point 2 3) "Lighten:"
                                color-default color-default)]
         [c (fill-rect-colored c (make-rect (point 15 3) 8 1)
                              #\█ (lighten base-color 0.2) (lighten base-color 0.2))]
         [c (fill-rect-colored c (make-rect (point 24 3) 8 1)
                              #\█ (lighten base-color 0.4) (lighten base-color 0.4))]
         [c (fill-rect-colored c (make-rect (point 33 3) 8 1)
                              #\█ (lighten base-color 0.6) (lighten base-color 0.6))]

         ;; Darkened versions
         [c (draw-string-colored c (point 2 5) "Darken:"
                                color-default color-default)]
         [c (fill-rect-colored c (make-rect (point 15 5) 8 1)
                              #\█ (darken base-color 0.8) (darken base-color 0.8))]
         [c (fill-rect-colored c (make-rect (point 24 5) 8 1)
                              #\█ (darken base-color 0.6) (darken base-color 0.6))]
         [c (fill-rect-colored c (make-rect (point 33 5) 8 1)
                              #\█ (darken base-color 0.4) (darken base-color 0.4))]

         ;; Color interpolation
         [c (draw-string-colored c (point 2 7) "Lerp (Red → Blue):"
                                color-default color-default)]
         [c (let loop ([i 0] [canvas c])
              (if (>= i 35)
                  canvas
                  (let* ([t (/ i 34.0)]
                         [color (lerp-color (rgb 255 0 0) (rgb 0 0 255) t)])
                    (loop (+ i 1)
                          (draw-char-colored canvas (point (+ 5 i) 9)
                                           #\█ color color)))))]

         ;; Border
         [c (draw-box-colored c (make-rect (point 0 0) 50 14)
                             'light color-purple color-default)])

    (display (canvas->string c))
    (newline)))

;;; ============================================================
;;; Run All Demos
;;; ============================================================

(display "\n\n")
(display "╔═══════════════════════════════════════════════════╗\n")
(display "║                                                   ║\n")
(display "║        THE FOLD — COLOR SYSTEM DEMO              ║\n")
(display "║        ANSI Graphics for DUCKIE                  ║\n")
(display "║                                                   ║\n")
(display "╚═══════════════════════════════════════════════════╝\n")

(demo-basic-colors)
(demo-truecolor)
(demo-shapes)
(demo-mood-colors)
(demo-energy-gradient)
(demo-color-helpers)

(display "\n")
(display "═══════════════════════════════════════════\n")
(display "  ✓ Color system operational!\n")
(display "  ✓ DUCKIE can now see in color.\n")
(display "═══════════════════════════════════════════\n\n")
