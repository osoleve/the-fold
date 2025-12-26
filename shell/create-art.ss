;;; Create art using the graphics primitives
;;; Dogfooding the graphics system!

(load "shell/layout.ss")
(load "shell/easing.ss")
(load "shell/graphics-primitives.ss")

(import (shell layout)
        (shell easing)
        (shell graphics-primitives))

;;; Create "The Eye of the Fold"
;;; A cosmic eye watching over the content-addressed universe

(define (create-the-eye)
  (let* ([w 60]
         [h 30]
         [canvas (make-canvas w h)]
         [center-x 30]
         [center-y 15])

    ;; Background: radial gradient with cubic easing
    (let* ([canvas (gradient-fill-radial
                     canvas
                     (point center-x center-y)
                     25
                     intensity-palette-simple
                     ease-out-cubic)])

      ;; Outer eye circle
      (let* ([canvas (draw-circle canvas (point center-x center-y) 12 #\○)])

        ;; Inner iris with gradient
        (let* ([canvas (gradient-fill-radial
                         canvas
                         (point center-x center-y)
                         8
                         intensity-palette-blocks
                         ease-in-quad)])

          ;; Pupil - the hash at the center
          (let* ([canvas (draw-filled-circle canvas (point center-x center-y) 4 #\█)])

            ;; Add some "light reflection" - a smaller bright spot
            (let* ([canvas (draw-filled-circle canvas (point (- center-x 2) (- center-y 1)) 2 #\░)])

              ;; Draw decorative lines emanating from the eye
              (let loop ([canvas canvas]
                        [angle 0])
                (if (>= angle 360)
                    canvas
                    (let* ([rad (* angle (/ 3.141592653589793 180))]
                           [x1 (inexact->exact (round (+ center-x (* 13 (cos rad)))))]
                           [y1 (inexact->exact (round (+ center-y (* 13 (sin rad)))))]
                           [x2 (inexact->exact (round (+ center-x (* 18 (cos rad)))))]
                           [y2 (inexact->exact (round (+ center-y (* 18 (sin rad)))))])
                      (if (= (modulo angle 45) 0)
                          (loop (draw-line canvas (point x1 y1) (point x2 y2) #\·)
                                (+ angle 45))
                          (loop canvas (+ angle 45)))))))))))))

;;; Create "The Fold Spiral"
;;; Representing the recursive nature of content addressing

(define (create-the-spiral)
  (let* ([w 70]
         [h 35]
         [canvas (make-canvas w h)]
         [center-x 35]
         [center-y 17])

    ;; Background gradient
    (let* ([canvas (gradient-fill-horizontal
                     canvas
                     (rect 0 0 w h)
                     intensity-palette-simple
                     ease-in-out-sine)])

      ;; Draw spiral using mathematical formula
      (let loop ([canvas canvas]
                [t 0])
        (if (> t 720)
            canvas
            (let* ([rad (* t (/ 3.141592653589793 180))]
                   [r (* 0.05 t)]
                   [x (inexact->exact (round (+ center-x (* r (cos rad)))))]
                   [y (inexact->exact (round (+ center-y (* r (sin rad)))))]
                   [char (cond
                           [(< t 180) #\·]
                           [(< t 360) #\•]
                           [(< t 540) #\○]
                           [else #\●])])
              (if (and (>= x 0) (< x w) (>= y 0) (< y h))
                  (loop (draw-char canvas (point x y) char)
                        (+ t 15))
                  (loop canvas (+ t 15)))))))))

;;; Create "Hash Constellation"
;;; Content-addressed blocks as stars in the universe

(define (create-constellation)
  (let* ([w 80]
         [h 40]
         [canvas (make-canvas w h)])

    ;; Draw "stars" at pseudo-random positions
    ;; (using simple deterministic formula for reproducibility)
    (let loop ([canvas canvas]
              [i 0])
      (if (>= i 50)
          canvas
          (let* ([x (modulo (* i 17) w)]
                 [y (modulo (* i 23) h)]
                 [brightness (modulo (* i 7) 5)]
                 [char (case brightness
                         [(0) #\·]
                         [(1) #\∙]
                         [(2) #\•]
                         [(3) #\○]
                         [(4) #\●])])
            (loop (draw-char canvas (point x y) char)
                  (+ i 1)))))))

;;; Main: create all three pieces
(define (main)
  (display "=== THE EYE OF THE FOLD ===\n\n")
  (display (canvas->string (create-the-eye)))
  (display "\n\n")

  (display "=== THE FOLD SPIRAL ===\n\n")
  (display (canvas->string (create-the-spiral)))
  (display "\n\n")

  (display "=== HASH CONSTELLATION ===\n\n")
  (display (canvas->string (create-constellation)))
  (display "\n"))

(main)
