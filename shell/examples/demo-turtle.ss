;;; shell/demo-turtle.ss — Turtle Graphics Demonstrations
;;;
;;; Example drawings showcasing the turtle graphics system.
;;;
;;; Run with: scheme --script demo-turtle.ss
;;;
;;; Each demo creates an SVG file in the current directory.

;;; ====
;;; Load Turtle Modules
;;; ====

(display "Loading turtle modules...\n")
(load "turtle-color.ss")
(load "turtle-path.ss")
(load "turtle.ss")
(load "turtle-svg.ss")
(display "Ready!\n\n")

;;; ====
;;; Demo 1: Basic Square
;;; ====

(define (demo-square)
  (display "Demo 1: Drawing a square...\n")
  (let* ([t (make-turtle)]
         [t (setpc t 'blue)]
         [t (setpw t 2)]
         [t (square t 100)]
         [d (turtle->drawing t)])
        (save-svg d "turtle-square.svg")
        (display "  Saved to turtle-square.svg\n")))

;;; ====
;;; Demo 2: Colorful Star
;;; ====

(define (demo-star)
  (display "Demo 2: Drawing a colorful star...\n")
  (let* ([t (make-turtle)]
         [t (setbg t 'navy)]
         [t (setpc t 'gold)]
         [t (setpw t 3)]
         [t (star t 150)]
         [d (turtle->drawing t)])
        (save-svg d "turtle-star.svg")
        (display "  Saved to turtle-star.svg\n")))

;;; ====
;;; Demo 3: Spiral
;;; ====

(define (demo-spiral)
  (display "Demo 3: Drawing a spiral...\n")
  (let* ([t (make-turtle)]
         [t (setpc t 'magenta)]
         [t (setpw t 1)]
         [t (spiral t 5 3 50)]
         [d (turtle->drawing t)])
        (save-svg d "turtle-spiral.svg")
        (display "  Saved to turtle-spiral.svg\n")))

;;; ====
;;; Demo 4: Nested Squares
;;; ====

(define (nested-squares t n size)
  (if (<= n 0)
      t
      (let* ([t (square t size)]
             [t (pu t)]
             [t (fd t 10)]
             [t (rt t 5)]
             [t (pd t)])
            (nested-squares t (- n 1) (- size 15)))))

(define (demo-nested-squares)
  (display "Demo 4: Drawing nested squares...\n")
  (let* ([t (make-turtle)]
         [t (setpc t 'cyan)]
         [t (setpw t 1)]
         [t (nested-squares t 10 200)]
         [d (turtle->drawing t)])
        (save-svg d "turtle-nested.svg")
        (display "  Saved to turtle-nested.svg\n")))

;;; ====
;;; Demo 5: Flower Pattern
;;; ====

(define (flower-petal t size)
  ;; Draw a petal: arc right, arc left
  (let* ([t (arc t 60 size)]
         [t (rt t 120)]
         [t (arc t 60 size)]
         [t (rt t 120)])
        t))

(define (flower t petals size)
  (let loop ([t t] [n petals])
       (if (<= n 0)
           t
           (let* ([t (flower-petal t size)]
                  [t (rt t (/ 360 petals))])
                 (loop t (- n 1))))))

(define (demo-flower)
  (display "Demo 5: Drawing a flower...\n")
  (let* ([t (make-turtle)]
         [t (setbg t 'white)]
         [t (setpc t 'red)]
         [t (setpw t 2)]
         [t (flower t 8 50)]
         [d (turtle->drawing t)])
        (save-svg d "turtle-flower.svg")
        (display "  Saved to turtle-flower.svg\n")))

;;; ====
;;; Demo 6: Polygon Gallery
;;; ====

(define (demo-polygons)
  (display "Demo 6: Drawing polygon gallery...\n")
  (let* ([t (make-turtle)]
         [t (setbg t 'black)]
         ;; Triangle (red)
         [t (pu t)]
         [t (setxy t 150 150)]
         [t (pd t)]
         [t (setpc t 'red)]
         [t (polygon t 3 80)]
         ;; Square (green)
         [t (pu t)]
         [t (setxy t 350 150)]
         [t (pd t)]
         [t (setpc t 'green)]
         [t (polygon t 4 60)]
         ;; Pentagon (blue)
         [t (pu t)]
         [t (setxy t 500 150)]
         [t (pd t)]
         [t (setpc t 'blue)]
         [t (polygon t 5 50)]
         ;; Hexagon (yellow)
         [t (pu t)]
         [t (setxy t 150 350)]
         [t (pd t)]
         [t (setpc t 'yellow)]
         [t (polygon t 6 45)]
         ;; Octagon (cyan)
         [t (pu t)]
         [t (setxy t 350 350)]
         [t (pd t)]
         [t (setpc t 'cyan)]
         [t (polygon t 8 35)]
         ;; Circle (magenta)
         [t (pu t)]
         [t (setxy t 500 350)]
         [t (pd t)]
         [t (setpc t 'magenta)]
         [t (circle t 40)]
         [d (turtle->drawing t)])
        (save-svg d "turtle-polygons.svg")
        (display "  Saved to turtle-polygons.svg\n")))

;;; ====
;;; Demo 7: Koch Snowflake (Fractal)
;;; ====

(define (koch-segment t length depth)
  (if (= depth 0)
      (fd t length)
      (let ([len3 (/ length 3)])
           (let* ([t (koch-segment t len3 (- depth 1))]
                  [t (lt t 60)]
                  [t (koch-segment t len3 (- depth 1))]
                  [t (rt t 120)]
                  [t (koch-segment t len3 (- depth 1))]
                  [t (lt t 60)]
                  [t (koch-segment t len3 (- depth 1))])
                 t))))

(define (koch-snowflake t size depth)
  (let loop ([t t] [i 0])
       (if (= i 3)
           t
           (loop (rt (koch-segment t size depth) 120) (+ i 1)))))

(define (demo-koch)
  (display "Demo 7: Drawing Koch snowflake (depth 3)...\n")
  (let* ([t (make-turtle)]
         [t (setbg t 'navy)]
         [t (setpc t 'white)]
         [t (setpw t 1)]
         ;; Position at top-left of snowflake
         [t (pu t)]
         [t (setxy t 170 120)]
         [t (setheading t 0)]
         [t (pd t)]
         [t (koch-snowflake t 300 3)]
         [d (turtle->drawing t)])
        (save-svg d "turtle-koch.svg")
        (display "  Saved to turtle-koch.svg\n")))

;;; ====
;;; Demo 8: Colorful Circles
;;; ====

(define (color-wheel t n radius)
  (let loop ([t t] [i 0])
       (if (>= i n)
           t
           (let* ([hue (inexact->exact (floor (* 15 (/ i n))))]
                  [color (make-color12 15 hue 0)]
                  [t (setpc t color)]
                  [t (circle t radius)]
                  [t (rt t (/ 360 n))]
                  [t (pu t)]
                  [t (fd t 20)]
                  [t (pd t)])
                 (loop t (+ i 1))))))

(define (demo-circles)
  (display "Demo 8: Drawing color wheel of circles...\n")
  (let* ([t (make-turtle)]
         [t (setbg t 'black)]
         [t (setpw t 2)]
         [t (color-wheel t 18 30)]
         [d (turtle->drawing t)])
        (save-svg d "turtle-circles.svg")
        (display "  Saved to turtle-circles.svg\n")))

;;; ====
;;; Run All Demos
;;; ====

(define (run-demos)
  (display "=== Turtle Graphics Demos ===\n\n")
  (demo-square)
  (demo-star)
  (demo-spiral)
  (demo-nested-squares)
  (demo-flower)
  (demo-polygons)
  (demo-koch)
  (demo-circles)
  (display "\nAll demos complete!\n"))

(run-demos)
