;;; Mountain Dreams - A layered ASCII landscape
;;; Created by ClaudeOpus using the canvas system

(load "shell/ui/layout.ss")

(define width 60)
(define height 18)

;; Create the main canvas
(define scene (make-canvas width height))

;; Draw starfield background
(let loop ([i 0])
     (when (< i 60)
           (let ([x1 (modulo (* i 17) width)]
                 [y1 (modulo (+ 1 (* i 7)) 8)]  ; Stars in upper portion
                 [x2 (modulo (* i 23) width)]
                 [y2 (modulo (* i 11) 6)])
                (when (and (>= x1 0) (< x1 width) (>= y1 0) (< y1 8))
                      (canvas-set! scene x1 y1 #\.))
                (when (and (>= x2 0) (< x2 width) (>= y2 0) (< y2 6))
                      (canvas-set! scene x2 y2 #\*)))
           (loop (+ i 1))))

;; Draw distant mountains (lighter shade)
(let loop ([x 0])
     (when (< x width)
           (let* ([wave (+ 9 (* 2 (sin (/ x 10.0))))]
                  [peak (inexact->exact (floor wave))])
                 (let fill ([y peak])
                      (when (< y 14)
                            (canvas-set! scene x y #\░)
                            (fill (+ y 1)))))
           (loop (+ x 1))))

;; Draw near mountains (darker shade)
(let loop ([x 0])
     (when (< x width)
           (let* ([wave1 (* 3 (sin (/ x 7.0)))]
                  [wave2 (* 2 (cos (/ x 4.0)))]
                  [h (+ 11 wave1 wave2)]
                  [peak (inexact->exact (floor h))])
                 (let fill ([y peak])
                      (when (< y 15)
                            (canvas-set! scene x y (if (< y (+ peak 1)) #\▒ #\▓))
                            (fill (+ y 1)))))
           (loop (+ x 1))))

;; Draw foreground ridge
(let loop ([x 0])
     (when (< x width)
           (let* ([wave (* 1.5 (sin (/ x 5.0)))]
                  [h (+ 14 wave)]
                  [peak (inexact->exact (floor h))])
                 (let fill ([y peak])
                      (when (< y 16)
                            (canvas-set! scene x y #\█)
                            (fill (+ y 1)))))
           (loop (+ x 1))))

;; Water/reflection at bottom
(let loop ([x 0])
     (when (< x width)
           (canvas-set! scene x 16 #\~)
           (canvas-set! scene x 17 #\≈)
           (loop (+ x 1))))

;; Moon
(canvas-set! scene 48 2 #\◐)

;; Frame corners
(canvas-set! scene 0 0 #\┌)
(canvas-set! scene 59 0 #\┐)
(canvas-set! scene 0 17 #\└)
(canvas-set! scene 59 17 #\┘)

;; Top border
(let loop ([x 1])
     (when (< x 17)
           (canvas-set! scene x 0 #\─)
           (loop (+ x 1))))
(let loop ([x 43])
     (when (< x 59)
           (canvas-set! scene x 0 #\─)
           (loop (+ x 1))))

;; Title
(draw-string scene (point 17 0) " MOUNTAIN DREAMS ")

;; Signature
(draw-string scene (point 45 17) "~ClaudeOpus")

;; Output
(display (canvas->string scene))
(newline)
