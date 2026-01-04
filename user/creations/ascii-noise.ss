;;; user/creations/ascii-noise.ss --- Smooth Noise in ASCII
;;;
;;; Perlin-style value noise rendered in glorious monospace.

(load "user/creations/ascii-video.ss")

;;; ============================================================
;;; Pseudo-random Hash (deterministic)
;;; ============================================================

(define (hash-2d x y)
  "Deterministic hash for 2D integer coordinates"
  (let* ([n (+ x (* y 57))]
         [n (bitwise-xor n (bitwise-arithmetic-shift-left n 13))]
         [m (+ 1 (* n (+ (* n n 15731) 789221)))]
         [h (bitwise-and m #x7fffffff)])
        (/ h 2147483647.0)))

;;; ============================================================
;;; Smoothstep Interpolation
;;; ============================================================

(define (smoothstep t)
  "Smooth Hermite interpolation: 3t^2 - 2t^3"
  (* t t (- 3 (* 2 t))))

(define (lerp a b t)
  "Linear interpolation"
  (+ a (* t (- b a))))

;;; ============================================================
;;; Value Noise
;;; ============================================================

(define (value-noise x y)
  "Smooth value noise at point (x, y)"
  (let* ([xi (inexact->exact (floor x))]
         [yi (inexact->exact (floor y))]
         [xf (- x xi)]
         [yf (- y yi)]
         ;; Smooth the fractional parts
         [u (smoothstep xf)]
         [v (smoothstep yf)]
         ;; Corner values
         [n00 (hash-2d xi yi)]
         [n10 (hash-2d (+ xi 1) yi)]
         [n01 (hash-2d xi (+ yi 1))]
         [n11 (hash-2d (+ xi 1) (+ yi 1))]
         ;; Bilinear interpolation with smoothstep
         [nx0 (lerp n00 n10 u)]
         [nx1 (lerp n01 n11 u)])
        (lerp nx0 nx1 v)))

;;; ============================================================
;;; Fractal Brownian Motion (fBm)
;;; ============================================================

(define (fbm x y octaves persistence lacunarity)
  "Fractal Brownian motion - layered noise"
  (let loop ([i 0] [amplitude 1.0] [frequency 1.0] [total 0.0] [max-val 0.0])
       (if (>= i octaves)
           (/ total max-val)  ; Normalize to [0, 1]
           (loop (+ i 1)
                 (* amplitude persistence)
                 (* frequency lacunarity)
                 (+ total (* amplitude (value-noise (* x frequency) (* y frequency))))
                 (+ max-val amplitude)))))

;;; ============================================================
;;; Rendering
;;; ============================================================

(define *noise-chars* " .,:;+*oO#@")

(define (noise->char n)
  "Map noise value [0,1] to ASCII character"
  (let* ([len (string-length *noise-chars*)]
         [idx (min (- len 1) (max 0 (inexact->exact (floor (* n len)))))])
        (string-ref *noise-chars* idx)))

(define (render-noise width height scale octaves seed)
  "Render noise to ASCII frame"
  (let ([frame (make-frame width height #\space)])
       (do ([row 0 (+ row 1)])
           ((>= row height))
           (do ([col 0 (+ col 1)])
               ((>= col width))
               (let* ([x (+ seed (/ col scale))]
                      [y (+ seed (/ row scale 0.5))]  ; Aspect ratio correction
                      [n (fbm x y octaves 0.5 2.0)]
                      [char (noise->char n)])
                     (frame-set! frame col row char))))
       frame))

;;; ============================================================
;;; Quick Renders
;;; ============================================================

(define (quick-noise)
  "Quick smooth noise render"
  (let ([frame (render-noise 60 24 8.0 4 0.0)])
       (frame-render-to-string frame)))

(define (quick-noise-fine)
  "Fine-grained noise"
  (let ([frame (render-noise 60 24 4.0 6 42.0)])
       (frame-render-to-string frame)))

(define (quick-clouds)
  "Cloud-like turbulent noise"
  (let ([frame (render-noise 60 24 12.0 5 123.0)])
       (frame-render-to-string frame)))

;;; Discord wrappers
(define (discord-noise)
  (string-append "```\n" (quick-noise) "```"))

(define (discord-noise-fine)
  (string-append "```\n" (quick-noise-fine) "```"))

(define (discord-clouds)
  (string-append "```\n" (quick-clouds) "```"))

;;; ============================================================
;;; Demo
;;; ============================================================

(define (run-noise-demo)
  (display "\n")
  (display "============================================================\n")
  (display "  SMOOTH NOISE IN ASCII\n")
  (display "============================================================\n")
  (display "  Value noise with smoothstep interpolation + fBm\n")
  (display "============================================================\n\n")
  
  (display "Smooth noise (4 octaves):\n\n")
  (let ([frame (render-noise 60 24 8.0 4 0.0)])
       (frame-render frame))
  
  (display "\nFine-grained (6 octaves):\n\n")
  (let ([frame (render-noise 60 24 4.0 6 42.0)])
       (frame-render frame))
  
  (display "\n============================================================\n")
  (display "Available: (discord-noise) (discord-noise-fine) (discord-clouds)\n")
  (display "============================================================\n"))

(run-noise-demo)
