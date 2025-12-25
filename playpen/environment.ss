;;; playpen/environment.ss — DUCKIE's World
;;;
;;; Environmental rendering for DUCKIE's habitat:
;;;   - Pond with water and ripples
;;;   - Garden with plants and flowers
;;;   - Sky with clouds
;;;   - Day/night cycle support
;;;
;;; This is Playpen code: experimental environment generation.
;;; Uses the color and layering systems to create atmospheric backgrounds.

;;; ============================================================
;;; Dependencies
;;; ============================================================

(load "shell/color.ss")
(load "shell/layout-color.ss")

;;; ============================================================
;;; Time of Day
;;; ============================================================

;;; TimeOfDay : (+ 'day 'dusk 'night 'dawn)
;;;
;;; Represents the current time for lighting and color.

;;; time-of-day-color : TimeOfDay → Color
;;;
;;; Get the sky color for a given time of day.
(define (time-of-day-color time)
  (case time
    [(day)   (rgb 135 206 235)]  ; Sky blue
    [(dusk)  (rgb 255 140  60)]  ; Orange sunset
    [(night) (rgb  25  25  50)]  ; Dark blue night
    [(dawn)  (rgb 255 200 150)]  ; Soft pink/yellow
    [else    (rgb 135 206 235)])) ; Default to day

;;; ============================================================
;;; Pond Rendering
;;; ============================================================

;;; draw-water-surface : Canvas × Nat × Nat × Color → Canvas
;;;
;;; Draw a water surface with wave patterns.
;;; Uses ~ characters to suggest gentle waves.
(define (draw-water-surface canvas y-start y-end water-color)
  (let ([wave-chars (list #\~ #\≈ #\~ #\≈)])  ; Alternating wave symbols
    (let y-loop ([y y-start] [c canvas])
      (if (>= y y-end)
          c
          (let x-loop ([x 1] [wave-idx 0] [c c])
            (if (>= x (- (canvas-width c) 1))
                (y-loop (+ y 1) c)
                (let* ([char-idx (modulo (+ wave-idx (quotient x 2)) (length wave-chars))]
                       [wave-char (list-ref wave-chars char-idx)]
                       [c (draw-char-colored c (point x y) wave-char water-color color-default)])
                  (x-loop (+ x 1) (+ wave-idx 1) c))))))))

;;; draw-ripple : Canvas × Point × Nat × Color → Canvas
;;;
;;; Draw a circular ripple effect at a given point.
;;; Used for subtle water animation.
(define (draw-ripple canvas center radius ripple-color)
  (let ([cx (point-x center)]
        [cy (point-y center)])
    (let ([c (draw-char-colored canvas center #\o ripple-color color-default)])
      (if (> radius 0)
          (let ([c (draw-char-colored c (point (- cx 1) cy) #\. ripple-color color-default)]
                [c (draw-char-colored c (point (+ cx 1) cy) #\. ripple-color color-default)]
                [c (draw-char-colored c (point cx (- cy 1)) #\. ripple-color color-default)]
                [c (draw-char-colored c (point cx (+ cy 1)) #\. ripple-color color-default)])
            c)
          c))))

;;; render-pond : Canvas × TimeOfDay → Canvas
;;;
;;; Render a pond environment with water.
(define (render-pond canvas time-of-day)
  (let* ([width (canvas-width canvas)]
         [height (canvas-height canvas)]
         [water-start (quotient (* height 2) 3)]  ; Water fills bottom third
         [water-color (case time-of-day
                       [(night) (rgb 30 60 90)]   ; Dark water at night
                       [(dusk)  (rgb 80 120 150)] ; Reflective dusk water
                       [else    (rgb 100 150 200)])] ; Bright day water
         ;; Draw water surface
         [canvas (draw-water-surface canvas water-start height water-color)])
    canvas))

;;; ============================================================
;;; Plant Rendering
;;; ============================================================

;;; draw-reed : Canvas × Point × Nat × Color → Canvas
;;;
;;; Draw a single reed/plant at a position with given height.
(define (draw-reed canvas pos height reed-color)
  (let ([x (point-x pos)]
        [y (point-y pos)])
    (let loop ([i 0] [c canvas])
      (if (>= i height)
          ;; Draw flower/top
          (draw-char-colored c (point x (- y height)) #\* reed-color color-default)
          (loop (+ i 1)
                (draw-char-colored c (point x (- y i)) #\| reed-color color-default))))))

;;; draw-grass : Canvas × Nat × Color → Canvas
;;;
;;; Draw grass along the bottom of the canvas.
(define (draw-grass canvas y grass-color)
  (let loop ([x 1] [c canvas])
    (if (>= x (- (canvas-width c) 1))
        c
        (let ([char (if (zero? (modulo x 2)) #\" #\')])
          (loop (+ x 1)
                (draw-char-colored c (point x y) char grass-color color-default))))))

;;; render-plants : Canvas × TimeOfDay → Canvas
;;;
;;; Render plants and vegetation.
(define (render-plants canvas time-of-day)
  (let* ([width (canvas-width canvas)]
         [height (canvas-height canvas)]
         [grass-color (case time-of-day
                       [(night) (rgb 20 60 20)]   ; Dark green at night
                       [else    (rgb 50 150 50)])] ; Bright green
         [reed-color (case time-of-day
                      [(night) (rgb 40 80 40)]
                      [else    (rgb 60 180 60)])]
         ;; Draw grass at water line
         [water-y (quotient (* height 2) 3)]
         [canvas (draw-grass canvas (- water-y 1) grass-color)]
         ;; Draw a few reeds
         [canvas (draw-reed canvas (point 5 (- water-y 1)) 4 reed-color)]
         [canvas (draw-reed canvas (point 12 (- water-y 1)) 3 reed-color)]
         [canvas (draw-reed canvas (point (- width 8) (- water-y 1)) 5 reed-color)]
         [canvas (draw-reed canvas (point (- width 15) (- water-y 1)) 3 reed-color)])
    canvas))

;;; ============================================================
;;; Sky Rendering
;;; ============================================================

;;; draw-cloud : Canvas × Point × Color → Canvas
;;;
;;; Draw a simple cloud shape.
(define (draw-cloud canvas pos cloud-color)
  (let ([x (point-x pos)]
        [y (point-y pos)])
    (let* ([c (draw-string-colored canvas (point x y) ".-." cloud-color color-default)]
           [c (draw-string-colored c (point (- x 1) (+ y 1)) "(   )" cloud-color color-default)])
      c)))

;;; draw-sun : Canvas × Point × Color → Canvas
;;;
;;; Draw the sun.
(define (draw-sun canvas pos sun-color)
  (let ([x (point-x pos)]
        [y (point-y pos)])
    (let* ([c (draw-string-colored canvas (point (- x 1) y) "\\|/" sun-color color-default)]
           [c (draw-string-colored c (point (- x 1) (+ y 1)) "-O-" sun-color color-default)]
           [c (draw-string-colored c (point (- x 1) (+ y 2)) "/|\\" sun-color color-default)])
      c)))

;;; draw-moon : Canvas × Point × Color → Canvas
;;;
;;; Draw the moon.
(define (draw-moon canvas pos moon-color)
  (let ([x (point-x pos)]
        [y (point-y pos)])
    (let* ([c (draw-string-colored canvas (point x y) " _" moon-color color-default)]
           [c (draw-string-colored c (point x (+ y 1)) "( )" moon-color color-default)]
           [c (draw-string-colored c (point (+ x 1) (+ y 2)) "--" moon-color color-default)])
      c)))

;;; render-sky : Canvas × TimeOfDay → Canvas
;;;
;;; Render the sky with celestial bodies and clouds.
(define (render-sky canvas time-of-day)
  (let* ([width (canvas-width canvas)]
         [height (canvas-height canvas)]
         [sky-y-end (quotient (* height 2) 3)]
         [sky-color (time-of-day-color time-of-day)]
         ;; Fill sky area with sky color (using spaces)
         [canvas (let y-loop ([y 1] [c canvas])
                   (if (>= y sky-y-end)
                       c
                       (y-loop (+ y 1)
                              (let x-loop ([x 1] [c c])
                                (if (>= x (- width 1))
                                    c
                                    (x-loop (+ x 1)
                                           (draw-char-colored c (point x y) #\space
                                                            sky-color color-default)))))))]
         ;; Draw celestial body based on time
         [canvas (case time-of-day
                  [(day dusk dawn)
                   (draw-sun canvas (point (- width 8) 3) color-yellow)]
                  [(night)
                   (draw-moon canvas (point (- width 8) 3) color-white)]
                  [else canvas])]
         ;; Draw clouds (during day/dawn/dusk)
         [canvas (if (not (eq? time-of-day 'night))
                    (let* ([c (draw-cloud canvas (point 8 4) color-white)]
                           [c (draw-cloud c (point 25 2) color-white)]
                           [c (draw-cloud c (point 42 5) color-white)])
                      c)
                    canvas)])
    canvas))

;;; ============================================================
;;; Complete Environment
;;; ============================================================

;;; render-environment : Nat × Nat × TimeOfDay → Canvas
;;;
;;; Render a complete environment with sky, plants, and pond.
;;; Returns a canvas ready to be used as a background layer.
(define (render-environment width height time-of-day)
  (let* ([canvas (make-transparent-canvas width height)]
         ;; Render in back-to-front order
         [canvas (render-sky canvas time-of-day)]
         [canvas (render-pond canvas time-of-day)]
         [canvas (render-plants canvas time-of-day)])
    canvas))

;;; ============================================================
;;; Animated Environment
;;; ============================================================

;;; frame->time-of-day : Nat → TimeOfDay
;;;
;;; Convert frame counter to time of day for animation.
;;; Full cycle every 800 frames.
(define (frame->time-of-day frame)
  (let ([cycle (modulo frame 800)])
    (cond
      [(< cycle 200) 'day]
      [(< cycle 300) 'dusk]
      [(< cycle 600) 'night]
      [(< cycle 700) 'dawn]
      [else 'day])))

;;; render-animated-environment : Nat × Nat × Nat → Canvas
;;;
;;; Render environment with time-of-day based on frame counter.
(define (render-animated-environment width height frame)
  (let ([time (frame->time-of-day frame)])
    (render-environment width height time)))
