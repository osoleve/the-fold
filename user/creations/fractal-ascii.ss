;;; user/creations/fractal-ascii.ss --- ASCII Fractal Renderer
;;;
;;; Because The Fold needed fractals rendered in monospace.
;;;
;;; Features:
;;; - Mandelbrot set with smooth coloring
;;; - Julia sets with animated parameters
;;; - ASCII palette for iteration count visualization
;;; - Animation system for parameter sweeps
;;; - Discord integration

(load "user/creations/ascii-video.ss")

;;; ============================================================
;;; Complex Number Operations
;;; ============================================================

;;; Complex numbers as (real . imag) pairs
(define (complex r i) (cons r i))
(define (complex-real z) (car z))
(define (complex-imag z) (cdr z))

(define (c+ a b)
  (complex (+ (complex-real a) (complex-real b))
           (+ (complex-imag a) (complex-imag b))))

(define (c* a b)
  (let ([ar (complex-real a)] [ai (complex-imag a)]
        [br (complex-real b)] [bi (complex-imag b)])
       (complex (- (* ar br) (* ai bi))
                (+ (* ar bi) (* ai br)))))

(define (c-sq z)
  (let ([r (complex-real z)] [i (complex-imag z)])
       (complex (- (* r r) (* i i))
                (* 2 r i))))

(define (c-mag-sq z)
  (let ([r (complex-real z)] [i (complex-imag z)])
       (+ (* r r) (* i i))))

(define (c-mag z)
  (sqrt (c-mag-sq z)))

;;; ============================================================
;;; Iteration Functions
;;; ============================================================

(define *max-iters* 80)
(define *escape-radius* 4.0)

;;; Mandelbrot: z_n+1 = z_n^2 + c, starting with z_0 = 0
(define (mandelbrot-iter c max-iters)
  (let loop ([z (complex 0 0)] [n 0])
       (cond
        [(>= n max-iters) max-iters]
        [(> (c-mag-sq z) *escape-radius*) n]
        [else (loop (c+ (c-sq z) c) (+ n 1))])))

;;; Julia: z_n+1 = z_n^2 + c, starting with z_0 = point
(define (julia-iter z c max-iters)
  (let loop ([z z] [n 0])
       (cond
        [(>= n max-iters) max-iters]
        [(> (c-mag-sq z) *escape-radius*) n]
        [else (loop (c+ (c-sq z) c) (+ n 1))])))

;;; Burning Ship: z_n+1 = (|re(z_n)| + i*|im(z_n)|)^2 + c
(define (burning-ship-iter c max-iters)
  (let loop ([z (complex 0 0)] [n 0])
       (cond
        [(>= n max-iters) max-iters]
        [(> (c-mag-sq z) *escape-radius*) n]
        [else
         (let* ([zr (abs (complex-real z))]
                [zi (abs (complex-imag z))]
                [z-abs (complex zr zi)])
               (loop (c+ (c-sq z-abs) c) (+ n 1)))])))

;;; ============================================================
;;; ASCII Shading
;;; ============================================================

;;; Gradient from dark to light (for inside = dark)
(define *fractal-chars* " .,'~-:;=+*#%@")  ; No backticks (breaks Discord)

;;; Alternative: more dramatic gradient
(define *dramatic-chars* " .,'^\":;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$")  ; No backticks

(define (iter->char iters max-iters charset)
  (if (= iters max-iters)
      #\space  ; Inside the set = dark
      (let* ([n (string-length charset)]
             [ratio (/ iters max-iters)]
             ;; Use log scaling for better visual distribution
             [scaled (if (> ratio 0)
                         (min 0.99 (/ (log (+ 1 (* ratio 20))) (log 21)))
                         0)]
             [idx (inexact->exact (floor (* scaled n)))])
            (string-ref charset idx))))

;;; ============================================================
;;; Rendering Functions
;;; ============================================================

(define (render-mandelbrot width height x-min x-max y-min y-max max-iters)
  "Render Mandelbrot set to an ASCII frame"
  (let ([frame (make-frame width height #\space)]
        [x-step (/ (- x-max x-min) (- width 1))]
        [y-step (/ (- y-max y-min) (- height 1))])
       (do ([row 0 (+ row 1)])
           ((>= row height))
           (let ([y (+ y-min (* row y-step))])
                (do ([col 0 (+ col 1)])
                    ((>= col width))
                    (let* ([x (+ x-min (* col x-step))]
                           [c (complex x y)]
                           [iters (mandelbrot-iter c max-iters)]
                           [char (iter->char iters max-iters *fractal-chars*)])
                          (frame-set! frame col row char)))))
       frame))

(define (render-julia width height c x-min x-max y-min y-max max-iters)
  "Render Julia set for constant c"
  (let ([frame (make-frame width height #\space)]
        [x-step (/ (- x-max x-min) (- width 1))]
        [y-step (/ (- y-max y-min) (- height 1))])
       (do ([row 0 (+ row 1)])
           ((>= row height))
           (let ([y (+ y-min (* row y-step))])
                (do ([col 0 (+ col 1)])
                    ((>= col width))
                    (let* ([x (+ x-min (* col x-step))]
                           [z (complex x y)]
                           [iters (julia-iter z c max-iters)]
                           [char (iter->char iters max-iters *fractal-chars*)])
                          (frame-set! frame col row char)))))
       frame))

(define (render-burning-ship width height x-min x-max y-min y-max max-iters)
  "Render the Burning Ship fractal"
  (let ([frame (make-frame width height #\space)]
        [x-step (/ (- x-max x-min) (- width 1))]
        [y-step (/ (- y-max y-min) (- height 1))])
       (do ([row 0 (+ row 1)])
           ((>= row height))
           (let ([y (+ y-min (* row y-step))])
                (do ([col 0 (+ col 1)])
                    ((>= col width))
                    (let* ([x (+ x-min (* col x-step))]
                           [c (complex x y)]
                           [iters (burning-ship-iter c max-iters)]
                           [char (iter->char iters max-iters *fractal-chars*)])
                          (frame-set! frame col row char)))))
       frame))

;;; ============================================================
;;; Famous Julia Set Constants
;;; ============================================================

(define julia-classic (complex -0.7 0.27015))
(define julia-spiral (complex -0.8 0.156))
(define julia-dendrite (complex 0.285 0.01))
(define julia-galaxy (complex -0.4 0.6))
(define julia-rabbit (complex -0.123 0.745))
(define julia-dragon (complex -0.8 0.156))
(define julia-seahorse (complex -0.75 0.11))
(define julia-lightning (complex -0.70176 -0.3842))

;;; ============================================================
;;; Animated Julia Set (Parameter Sweep)
;;; ============================================================

(define (animate-julia-sweep width height num-frames center radius max-iters)
  "Animate Julia set with c tracing a circle in parameter space"
  (let ([video (make-video)])
       (do ([i 0 (+ i 1)])
           ((>= i num-frames))
           (let* ([angle (* i (/ 6.28318 num-frames))]
                  [c (complex (+ (complex-real center) (* radius (cos angle)))
                              (+ (complex-imag center) (* radius (sin angle))))]
                  [frame (render-julia width height c -1.5 1.5 -1.0 1.0 max-iters)])
                 ;; Add label showing current c value
                 (frame-put-string! frame 1 0
                                    (string-append "c = ("
                                                   (number->string (round-to (complex-real c) 3))
                                                   ", "
                                                   (number->string (round-to (complex-imag c) 3))
                                                   "i)"))
                 (video-add-frame! video frame)))
       video))

(define (round-to n places)
  (let ([factor (expt 10 places)])
       (/ (round (* n factor)) factor)))

;;; ============================================================
;;; Zoom Animation
;;; ============================================================

(define (animate-mandelbrot-zoom width height num-frames target-x target-y start-scale end-scale max-iters)
  "Animate zooming into the Mandelbrot set"
  (let ([video (make-video)])
       (do ([i 0 (+ i 1)])
           ((>= i num-frames))
           (let* ([t (/ i (max 1 (- num-frames 1)))]
                  ;; Exponential interpolation for smooth zoom
                  [scale (exp (+ (* (log start-scale) (- 1 t))
                                 (* (log end-scale) t)))]
                  [half-w (* scale 1.5)]  ; Aspect ratio
                  [half-h scale]
                  [x-min (- target-x half-w)]
                  [x-max (+ target-x half-w)]
                  [y-min (- target-y half-h)]
                  [y-max (+ target-y half-h)]
                  [frame (render-mandelbrot width height x-min x-max y-min y-max max-iters)])
                 ;; Add zoom level indicator
                 (frame-put-string! frame 1 0
                                    (string-append "Zoom: " (number->string (inexact->exact (round (/ 1 scale)))) "x"))
                 (video-add-frame! video frame)))
       video))

;;; ============================================================
;;; Quick Renders (for Discord)
;;; ============================================================

(define (quick-mandelbrot)
  "Quick Mandelbrot render"
  (let ([frame (render-mandelbrot 60 24 -2.5 1.0 -1.2 1.2 50)])
       (frame-render-to-string frame)))

(define (quick-julia c)
  "Quick Julia set render"
  (let ([frame (render-julia 60 24 c -1.5 1.5 -1.0 1.0 50)])
       (frame-render-to-string frame)))

(define (quick-burning-ship)
  "Quick Burning Ship render"
  (let ([frame (render-burning-ship 60 24 -2.0 1.0 -1.8 0.6 50)])
       (frame-render-to-string frame)))

;;; Discord wrappers
(define (discord-mandelbrot)
  (string-append "```\n" (quick-mandelbrot) "```"))

(define (discord-julia c)
  (string-append "```\n" (quick-julia c) "```"))

(define (discord-burning-ship)
  (string-append "```\n" (quick-burning-ship) "```"))

;;; ============================================================
;;; Deep Zoom Locations (Famous spots in Mandelbrot)
;;; ============================================================

(define *seahorse-valley* (complex -0.745428 0.113009))
(define *elephant-valley* (complex 0.281717 0.011547))
(define *spiral-arm* (complex -0.749 0.1))
(define *mini-mandelbrot* (complex -1.75 0.0))

;;; ============================================================
;;; Demo
;;; ============================================================

(define (run-fractal-demo)
  (display "\n")
  (display "============================================================\n")
  (display "  ASCII FRACTAL RENDERER\n")
  (display "============================================================\n")
  (display "  Complex iteration rendered in glorious monospace\n")
  (display "============================================================\n\n")
  
  (display "Mandelbrot Set:\n")
  (display "z_n+1 = z_n^2 + c, starting at z_0 = 0\n\n")
  (let ([frame (render-mandelbrot 60 24 -2.5 1.0 -1.2 1.2 50)])
       (frame-render frame))
  
  (display "\nJulia Set (classic spiral):\n")
  (display "z_n+1 = z_n^2 + c, where c = -0.7 + 0.27015i\n\n")
  (let ([frame (render-julia 60 24 julia-classic -1.5 1.5 -1.0 1.0 50)])
       (frame-render frame))
  
  (display "\nBurning Ship Fractal:\n")
  (display "z_n+1 = (|Re(z_n)| + i|Im(z_n)|)^2 + c\n\n")
  (let ([frame (render-burning-ship 60 24 -2.0 1.0 -1.8 0.6 50)])
       (frame-render frame))
  
  (display "\n============================================================\n")
  (display "Available functions:\n")
  (display "  (discord-mandelbrot)     ; Mandelbrot set\n")
  (display "  (discord-julia c)        ; Julia set for complex c\n")
  (display "  (discord-burning-ship)   ; Burning Ship fractal\n")
  (display "\n")
  (display "Famous Julia constants:\n")
  (display "  julia-classic, julia-spiral, julia-dendrite\n")
  (display "  julia-galaxy, julia-rabbit, julia-seahorse\n")
  (display "\n")
  (display "Animation:\n")
  (display "  (animate-julia-sweep w h frames center radius iters)\n")
  (display "  (animate-mandelbrot-zoom w h frames x y start end iters)\n")
  (display "============================================================\n"))

;; Run demo
(run-fractal-demo)
