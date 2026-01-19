;;; playpen/creations/color-palette-explorer.ss — ASCII Art Color Palette Generator
;;;
;;; An interactive system for exploring and generating beautiful ASCII art color palettes.
;;; Demonstrates creative use of character density, patterns, and randomization.
;;;
;;; Created by: Game Tester (Haiku Agent)
;;; Date: 2025-12-26

(load "boundary/ui/layout.ss")

;;; ====
;;; ASCII Character Density Levels
;;; ====

;;; Characters from lightest to darkest
(define *density-levels*
  '((" "   0.00)
    ("·"   0.15)
    ("░"   0.25)
    ("▒"   0.40)
    ("▓"   0.60)
    ("█"   1.00)))

;;; ====
;;; Color-like Patterns using Special Characters
;;; ====

(define *palette-red*
  '(#\╱ #\╲ #\× #\◆ #\● #\■ #\█))

(define *palette-blue*
  '(#\~ #\≈ #\∿ #\⋈ #\◇ #\□ #\▓))

(define *palette-green*
  '(#\✿ #\♣ #\❀ #\◈ #\◊ #\▒ #\░))

(define *palette-yellow*
  '(#\✦ #\★ #\✧ #\◆ #\♦ #\▬ #\─))

(define *palette-purple*
  '(#\◈ #\◆ #\✤ #\✥ #\♦ #\▲ #\△))

(define *palette-cyan*
  '(#\≈ #\~ #\∼ #\◇ #\◆ #\◈ #\█))

;;; ====
;;; Palette Mixing
;;; ====

(define (random-element lst)
  "Pick a random element from a list"
  (list-ref lst (random (length lst))))

(define (random-palette)
  "Return a random color palette"
  (random-element
   (list *palette-red* *palette-blue* *palette-green*
         *palette-yellow* *palette-purple* *palette-cyan*)))

;;; ====
;;; Canvas Operations
;;; ====

(define (fill-canvas-pattern canvas pattern-char)
  "Fill canvas with a repeating pattern"
  (let loop-y ([y 0])
       (when (< y (canvas-height canvas))
             (let loop-x ([x 0])
                  (when (< x (canvas-width canvas))
                        (canvas-set! canvas x y pattern-char)
                        (loop-x (+ x 1))))
             (loop-y (+ y 1)))))

(define (create-gradient canvas palette direction)
  "Create a gradient using palette characters"
  (let ([w (canvas-width canvas)]
        [h (canvas-height canvas)]
        [pal-len (length palette)])
       (case direction
             ((horizontal)
              (let loop-y ([y 0])
                   (when (< y h)
                         (let loop-x ([x 0])
                              (when (< x w)
                                    (let* ([ratio (/ x (max 1 (- w 1)))]
                                           [index (inexact->exact
                                                   (floor (* ratio (- pal-len 1))))]
                                           [char (list-ref palette index)])
                                          (canvas-set! canvas x y char)
                                          (loop-x (+ x 1)))))
                         (loop-y (+ y 1)))))
             
             ((vertical)
              (let loop-y ([y 0])
                   (when (< y h)
                         (let loop-x ([x 0])
                              (when (< x w)
                                    (let* ([ratio (/ y (max 1 (- h 1)))]
                                           [index (inexact->exact
                                                   (floor (* ratio (- pal-len 1))))]
                                           [char (list-ref palette index)])
                                          (canvas-set! canvas x y char)
                                          (loop-x (+ x 1)))))
                         (loop-y (+ y 1)))))
             
             ((radial)
              (let* ([cx (/ w 2.0)]
                     [cy (/ h 2.0)]
                     [max-dist (sqrt (+ (* cx cx) (* cy cy)))])
                    (let loop-y ([y 0])
                         (when (< y h)
                               (let loop-x ([x 0])
                                    (when (< x w)
                                          (let* ([dx (- x cx)]
                                                 [dy (- y cy)]
                                                 [dist (sqrt (+ (* dx dx) (* dy dy)))]
                                                 [ratio (min 1.0 (/ dist max-dist))]
                                                 [index (inexact->exact
                                                         (floor (* ratio (- pal-len 1))))]
                                                 [char (list-ref palette index)])
                                                (canvas-set! canvas x y char)
                                                (loop-x (+ x 1)))))
                               (loop-y (+ y 1)))))))))

;;; ====
;;; Display Functions
;;; ====

(define (display-palette-name name)
  "Display palette name with decorative border"
  (display "╔════════════════════════════════════════════════════════════╗\n")
  (display "║ ")
  (display (string-append
            (string-pad name 58 #\space)
            " ║\n"))
  (display "╚════════════════════════════════════════════════════════════╝\n\n"))

(define (display-palette palette)
  "Display a palette as a series of characters"
  (display "  Palette: ")
  (for-each (lambda (char) (display char) (display " "))
            palette)
  (newline)
  (newline))

(define (display-gradient-demo palette direction)
  "Display a gradient demonstration"
  (let ([canvas (make-canvas 60 5)])
       (create-gradient canvas palette direction)
       (display (canvas->string canvas))
       (newline)))

;;; ====
;;; Palette Explorer Functions
;;; ====

(define (explore-palette-gradient seed)
  "Explore a single palette with all gradient types"
  (random-seed seed)
  (let ([palette (random-palette)])
       (display "\n")
       (display-palette-name (format "Palette Explorer — Seed: ~a" seed))
       (display-palette palette)
       
       (display "HORIZONTAL GRADIENT:\n")
       (display-gradient-demo palette 'horizontal)
       
       (display "VERTICAL GRADIENT:\n")
       (display-gradient-demo palette 'vertical)
       
       (display "RADIAL GRADIENT:\n")
       (display-gradient-demo palette 'radial)
       
       (display "────────────────────────────────────────────────────────────\n\n")))

(define (palette-series n)
  "Generate and display a series of palettes"
  (let loop ([i 0])
       (when (< i n)
             (explore-palette-gradient (+ 1000 (* i 100)))
             (loop (+ i 1)))))

;;; ====
;;; Interactive Palette Creator
;;; ====

(define (create-custom-palette)
  "Interactively create a custom palette"
  (display "\n╔════════════════════════════════════════════════════════════╗\n")
  (display "║          CUSTOM PALETTE CREATOR                           ║\n")
  (display "╚════════════════════════════════════════════════════════════╝\n\n")
  
  (display "Available character sets:\n")
  (display "  1. Red palette:    ")
  (for-each (lambda (c) (display c) (display " ")) *palette-red*)
  (newline)
  
  (display "  2. Blue palette:   ")
  (for-each (lambda (c) (display c) (display " ")) *palette-blue*)
  (newline)
  
  (display "  3. Green palette:  ")
  (for-each (lambda (c) (display c) (display " ")) *palette-green*)
  (newline)
  
  (display "  4. Yellow palette: ")
  (for-each (lambda (c) (display c) (display " ")) *palette-yellow*)
  (newline)
  
  (display "  5. Purple palette: ")
  (for-each (lambda (c) (display c) (display " ")) *palette-purple*)
  (newline)
  
  (display "  6. Cyan palette:   ")
  (for-each (lambda (c) (display c) (display " ")) *palette-cyan*)
  (newline)
  
  (newline)
  (display "Try: (explore-palette-gradient 42)\n")
  (display "Or:  (palette-series 3)\n\n"))

;;; ====
;;; Mosaic Art Generator
;;; ====

(define (generate-mosaic seed size-factor)
  "Generate a random mosaic art piece"
  (random-seed seed)
  (let* ([w (* 40 size-factor)]
         [h (* 15 size-factor)]
         [canvas (make-canvas w h)])
        
        ;; Fill with random tiles from palettes
        (let loop-y ([y 0])
             (when (< y h)
                   (let loop-x ([x 0])
                        (when (< x w)
                              (let* ([palette (random-palette)]
                                     [char (random-element palette)])
                                    (canvas-set! canvas x y char)
                                    (loop-x (+ x 1)))))
                   (loop-y (+ y 1))))
        
        (newline)
        (display-palette-name (format "Mosaic Art — Seed: ~a" seed))
        (display (canvas->string canvas))
        (newline)
        (newline)))

;;; ====
;;; Loaded
;;; ====

(display "\n")
(display "╔════════════════════════════════════════════════════════════╗\n")
(display "║        COLOR PALETTE EXPLORER — Now Exploring              ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n\n")

(display "Welcome to the ASCII Art Color Palette Explorer!\n")
(display "Discover beautiful gradients and patterns.\n\n")

(display "COMMANDS:\n")
(display "  (explore-palette-gradient seed) — Explore a palette by seed\n")
(display "  (palette-series n)              — Show n different palettes\n")
(display "  (generate-mosaic seed factor)   — Create a mosaic art piece\n")
(display "  (create-custom-palette)         — See available palettes\n\n")

(display "EXAMPLES:\n")
(display "  (explore-palette-gradient 42)\n")
(display "  (palette-series 2)\n")
(display "  (generate-mosaic 123 1)\n\n")

(display "Have fun exploring!\n\n")
