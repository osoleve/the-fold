;;; playpen/ascii-waves.ss — Animated ASCII Wave Demo
;;;
;;; A mesmerizing sine wave animation using the canvas system.
;;; Demonstrates procedural animation and the boundary/ui/layout.ss API.
;;;
;;; This is Playpen code: creative exploration of the canvas system.

;;; Load dependencies
(load "boundary/ui/layout.ss")

;;; ====
;;; Wave Animation
;;; ====

;;; wave-char : Float → Char
;;; Convert wave height to ASCII character density.
(define (wave-char height)
  (let ([chars " .:;+=xX$@"])
       (let ([index (inexact->exact (floor (* (+ height 1.0) 0.5 (- (string-length chars) 1))))])
            (string-ref chars (clamp index 0 (- (string-length chars) 1))))))

;;; generate-wave-canvas : Nat × Nat × Float × Float → Canvas
;;; Create a canvas with animated sine waves.
(define (generate-wave-canvas width height time phase-offset)
  (let ([canvas (make-canvas width height)])
       (let y-loop ([y 0])
            (when (< y height)
                  (let x-loop ([x 0])
                       (when (< x width)
                             ;; Normalized coordinates
                             (let* ([nx (/ x width)]
                                    [ny (/ y height)]
                                    ;; Multiple wave layers
                                    [wave1 (sin (+ (* nx 20.0) time))]
                                    [wave2 (sin (+ (* nx 15.0) (* ny 10.0) (* time 0.7) phase-offset))]
                                    [wave3 (sin (+ (* ny 8.0) (* time 1.3)))]
                                    ;; Combine waves
                                    [combined (* 0.5 (+ wave1 (* 0.5 wave2) (* 0.3 wave3)))]
                                    [ch (wave-char combined)])
                                   (canvas-set! canvas x y ch)
                                   (x-loop (+ x 1)))))
                  (y-loop (+ y 1))))
       canvas))

;;; ====
;;; Plasma Effect
;;; ====

;;; generate-plasma-canvas : Nat × Nat × Float → Canvas
;;; Create a plasma effect using multiple sine functions.
(define (generate-plasma-canvas width height time)
  (let ([canvas (make-canvas width height)])
       (let y-loop ([y 0])
            (when (< y height)
                  (let x-loop ([x 0])
                       (when (< x width)
                             (let* ([nx (/ x width)]
                                    [ny (/ y height)]
                                    ;; Plasma formula
                                    [v1 (sin (+ (* nx 10.0) time))]
                                    [v2 (sin (+ (* ny 8.0) (* time 0.5)))]
                                    [v3 (sin (+ (* (+ nx ny) 12.0) (* time 0.7)))]
                                    [v4 (sin (+ (sqrt (+ (* nx nx) (* ny ny))) time))]
                                    [plasma (/ (+ v1 v2 v3 v4) 4.0)]
                                    [ch (wave-char plasma)])
                                   (canvas-set! canvas x y ch)
                                   (x-loop (+ x 1)))))
                  (y-loop (+ y 1))))
       canvas))

;;; ====
;;; Ripple Effect
;;; ====

;;; generate-ripple-canvas : Nat × Nat × Float → Canvas
;;; Create concentric ripples emanating from center.
(define (generate-ripple-canvas width height time)
  (let ([canvas (make-canvas width height)]
        [cx (/ width 2.0)]
        [cy (/ height 2.0)])
       (let y-loop ([y 0])
            (when (< y height)
                  (let x-loop ([x 0])
                       (when (< x width)
                             (let* ([dx (- x cx)]
                                    [dy (- y cy)]
                                    [dist (sqrt (+ (* dx dx) (* dy dy)))]
                                    [ripple (sin (- (* dist 0.5) (* time 3.0)))]
                                    [ch (wave-char ripple)])
                                   (canvas-set! canvas x y ch)
                                   (x-loop (+ x 1)))))
                  (y-loop (+ y 1))))
       canvas))

;;; ====
;;; Matrix Rain Effect
;;; ====

(define *matrix-state* #f)

;;; init-matrix : Nat → void
;;; Initialize matrix rain state with random column positions.
(define (init-matrix width)
  (set! *matrix-state*
        (let loop ([i 0] [cols '()])
             (if (< i width)
                 (loop (+ i 1) (cons (random 30) cols))
                 (list->vector cols)))))

;;; generate-matrix-canvas : Nat × Nat × Float → Canvas
;;; Create Matrix-style digital rain.
(define (generate-matrix-canvas width height time)
  (unless *matrix-state* (init-matrix width))
  (let ([canvas (make-canvas width height)]
        [chars "01"])
       (let x-loop ([x 0])
            (when (< x width)
                  (let* ([col-pos (modulo (+ (vector-ref *matrix-state* x)
                                             (inexact->exact (floor (* time 2.0))))
                                          height)]
                         [trail-len 8])
                        (let y-loop ([y 0])
                             (when (< y height)
                                   (let ([dist (- col-pos y)])
                                        (cond
                                         [(and (>= dist 0) (< dist trail-len))
                                          (let* ([intensity (- 1.0 (/ dist trail-len))]
                                                 [ch (if (> intensity 0.5)
                                                         (string-ref chars (random 2))
                                                         (if (> intensity 0.2) #\: #\.))])
                                                (canvas-set! canvas x y ch))]
                                         [else (canvas-set! canvas x y #\space)]))
                                   (y-loop (+ y 1)))))
                  (x-loop (+ x 1))))
       canvas))

;;; ====
;;; Text Banner
;;; ====

;;; draw-text : Canvas × String × Int × Int → void
;;; Draw text on canvas at position.
(define (draw-text canvas text x y)
  (let loop ([i 0])
       (when (and (< i (string-length text))
                  (< (+ x i) (canvas-width canvas))
                  (>= (+ x i) 0)
                  (< y (canvas-height canvas))
                  (>= y 0))
             (canvas-set! canvas (+ x i) y (string-ref text i))
             (loop (+ i 1)))))

;;; ====
;;; Demo Functions
;;; ====

;;; show-waves : → void
;;; Display a single frame of wave animation.
(define (show-waves)
  (let* ([w 80]
         [h 24]
         [t (current-second)]
         [canvas (generate-wave-canvas w h t 0.0)])
        (display "\n")
        (display "═══════════════════════════════ WAVES ═══════════════════════════════\n")
        (display (canvas->string canvas))
        (display "\n")))

;;; show-plasma : → void
;;; Display a single frame of plasma effect.
(define (show-plasma)
  (let* ([w 80]
         [h 24]
         [t (current-second)]
         [canvas (generate-plasma-canvas w h t)])
        (display "\n")
        (display "═══════════════════════════════ PLASMA ══════════════════════════════\n")
        (display (canvas->string canvas))
        (display "\n")))

;;; show-ripple : → void
;;; Display a single frame of ripple effect.
(define (show-ripple)
  (let* ([w 80]
         [h 24]
         [t (current-second)]
         [canvas (generate-ripple-canvas w h t)])
        (display "\n")
        (display "═══════════════════════════════ RIPPLE ══════════════════════════════\n")
        (display (canvas->string canvas))
        (display "\n")))

;;; show-matrix : → void
;;; Display a single frame of matrix rain.
(define (show-matrix)
  (let* ([w 80]
         [h 24]
         [t (current-second)]
         [canvas (generate-matrix-canvas w h t)])
        (display "\n")
        (display "═══════════════════════════════ MATRIX ══════════════════════════════\n")
        (display (canvas->string canvas))
        (display "\n")))

;;; demo-all : → void
;;; Show all effects in sequence.
(define (demo-all)
  (display "\nASCII Animation Demo\n")
  (display "====\n\n")
  (display "Available effects:\n")
  (display "  (show-waves)   - Sine wave patterns\n")
  (display "  (show-plasma)  - Plasma effect\n")
  (display "  (show-ripple)  - Concentric ripples\n")
  (display "  (show-matrix)  - Matrix digital rain\n")
  (display "  (demo-all)     - Show this message\n")
  (newline)
  (display "Try each effect! Each call shows a new frame based on time.\n")
  (newline))

;;; ====
;;; Loaded
;;; ====

(demo-all)
