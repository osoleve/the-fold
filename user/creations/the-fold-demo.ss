;;; user/creations/the-fold-demo.ss — THE FOLD Demo
;;;
;;; A demoscene-style ASCII animation visualizing The Fold:
;;; - S-expressions flowing and normalizing
;;; - Code crystallizing into content-addressed blocks
;;; - Three-layer architecture (Core/Lattice/Shell)
;;; - Hashes emerging from code
;;;
;;; The visual journey:
;;;   1. INTRO: Starfield, title emerges
;;;   2. S-EXPR FLOW: S-expressions stream across screen
;;;   3. NORMALIZATION: Code transforms, alpha-normalizes
;;;   4. CRYSTALLIZATION: Hashes crystallize from normalized code
;;;   5. THE LATTICE: Three layers visualized as strata
;;;   6. OUTRO: "Everything is content-addressed" message

(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "boundary/ui/animation.ss")

;;; ====
;;; Constants
;;; ====

(define WIDTH 100)
(define HEIGHT 42)
(define TOTAL-FRAMES 360)  ; 6 seconds @ 60fps

;;; Phase boundaries (frames) - 60 frames = 1 second per phase
(define INTRO-START 0)
(define INTRO-END 60)
(define FLOW-START 60)
(define FLOW-END 120)
(define NORM-START 120)
(define NORM-END 180)
(define CRYSTAL-START 180)
(define CRYSTAL-END 240)
(define LATTICE-START 240)
(define LATTICE-END 300)
(define OUTRO-START 300)
(define OUTRO-END 360)

;;; Character density ramp (light to dark)
(define DENSITY " .:-=+*#%@")

;;; ====
;;; Math Utilities
;;; ====

(define PI 3.141592653589793)

(define (sin-deg deg)
  (sin (* deg PI (/ 1.0 180.0))))

(define (cos-deg deg)
  (cos (* deg PI (/ 1.0 180.0))))

(define (clamp x min-val max-val)
  (max min-val (min max-val x)))

;;; ====
;;; Starfield (Background)
;;; ====

;;; Generate a starfield of random positions
(define (make-starfield n)
  (define stars '())
  (do ([i 0 (+ i 1)])
      ((>= i n))
      (set! stars (cons (list (random WIDTH)
                              (random HEIGHT)
                              (+ 1 (random 3)))  ; depth layer
                        stars)))
  stars)

(define *starfield* (make-starfield 80))

(define (draw-starfield! frame stars alpha)
  "Draw stars with alpha blend (0.0-1.0)"
  (for-each
   (lambda (star)
     (let* ([x (car star)]
            [y (cadr star)]
            [depth (caddr star)]
            [char (cond
                    ((= depth 1) #\.)
                    ((= depth 2) #\*)
                    (else #\+))])
       (when (< (random 100) (* alpha 100))
         (frame-set! frame x y char))))
   stars))

;;; ====
;;; Title Rendering
;;; ====

(define (draw-title! frame text x y alpha)
  "Draw centered title with alpha fade"
  (let* ([len (string-length text)]
         [start-x (- x (quotient len 2))])
    (do ([i 0 (+ i 1)])
        ((>= i len))
        (when (< (random 100) (* alpha 100))
          (frame-set! frame (+ start-x i) y (string-ref text i))))))

;;; ====
;;; S-Expression Flow
;;; ====

;;; Sample S-expressions for display
(define *sample-exprs*
  '("(lambda (x) (+ x 1))"
    "(define block (list 'tag payload refs))"
    "(cons 'quote (list x))"
    "(hash-normalize expr)"
    "(alpha-convert '(lambda (x) x))"
    "(make-block 'expr code '())"))

(define (draw-flowing-expr! frame expr x y offset)
  "Draw an expression flowing horizontally"
  (let ([len (string-length expr)])
    (do ([i 0 (+ i 1)])
        ((>= i len))
        (let ([char-x (+ x i offset)])
          (when (and (>= char-x 0) (< char-x WIDTH))
            (frame-set! frame char-x y (string-ref expr i)))))))

;;; ====
;;; Normalization Effect
;;; ====

(define (draw-normalize-effect! frame t)
  "Show code transforming via normalization"
  (let* ([y-base 15]
         [original "(lambda (x) (+ x 1))"]
         [normalized "(lambda (0) (+ 0 1))"]
         [alpha (ease-in-out-cubic t)]
         [len (string-length original)])

    ;; Draw "BEFORE" label
    (frame-put-string! frame 5 (- y-base 2) "BEFORE:")

    ;; Original expression
    (frame-put-string! frame 10 y-base original)

    ;; Arrow showing transformation
    (let ([arrow-y (+ y-base 2)])
      (frame-put-string! frame 30 arrow-y "|")
      (frame-put-string! frame 30 (+ arrow-y 1) "v"))

    ;; "AFTER" label
    (frame-put-string! frame 5 (+ y-base 4) "AFTER:")

    ;; Normalized expression (fade in)
    (do ([i 0 (+ i 1)])
        ((>= i (string-length normalized)))
        (when (< (random 100) (* alpha 100))
          (frame-set! frame (+ 10 i) (+ y-base 5) (string-ref normalized i))))

    ;; Hash appears as it normalizes
    (when (> t 0.5)
      (let ([hash-alpha (* 2 (- t 0.5))]
            [hash-text "=> SHA256: a3f9..."])
        (when (< (random 100) (* hash-alpha 100))
          (frame-put-string! frame 10 (+ y-base 8) hash-text))))))

;;; ====
;;; Crystallization Effect
;;; ====

(define (draw-crystal! frame cx cy size density-level)
  "Draw a crystalline hash block"
  (let ([char (string-ref DENSITY (min density-level (- (string-length DENSITY) 1)))])
    ;; Diamond shape
    (do ([dy (- size) (+ dy 1)])
        ((> dy size))
        (let ([width (- size (abs dy))])
          (do ([dx (- width) (+ dx 1)])
              ((> dx width))
              (let ([x (+ cx dx)]
                    [y (+ cy dy)])
                (when (and (>= x 0) (< x WIDTH)
                           (>= y 0) (< y HEIGHT))
                  (frame-set! frame x y char))))))))

(define (draw-crystallization! frame t)
  "Hashes crystallize from code"
  (let* ([num-crystals 5]
         [phase (ease-in-out-cubic t)])
    (do ([i 0 (+ i 1)])
        ((>= i num-crystals))
        (let* ([cx (+ 20 (* i 15))]
               [cy (+ 20 (inexact->exact (round (* 8 (sin (* i 0.8 t))))))]
               [size (inexact->exact (round (* 3 phase)))]
               [density (inexact->exact (round (* phase 8)))])
          (when (> size 0)
            (draw-crystal! frame cx cy size density))))))

;;; ====
;;; Three-Layer Architecture
;;; ====

(define (draw-layers! frame t)
  "Visualize Core/Lattice/Shell as horizontal strata"
  (let* ([layer-height (quotient HEIGHT 3)]
         [y-core 0]
         [y-lattice layer-height]
         [y-shell (* 2 layer-height)]
         [alpha (ease-in-out-cubic t)])

    ;; Core layer (top third) - densest
    (do ([y y-core (+ y 1)])
        ((>= y y-lattice))
        (do ([x 0 (+ x 4)])
            ((>= x WIDTH))
            (when (< (random 100) (* alpha 60))
              (frame-set! frame x y #\#))))

    ;; Lattice layer (middle third) - medium
    (do ([y y-lattice (+ y 1)])
        ((>= y y-shell))
        (do ([x 0 (+ x 3)])
            ((>= x WIDTH))
            (when (< (random 100) (* alpha 50))
              (frame-set! frame x y #\:))))

    ;; Boundary layer (bottom third) - lightest
    (do ([y y-shell (+ y 1)])
        ((>= y HEIGHT))
        (do ([x 0 (+ x 4)])
            ((>= x WIDTH))
            (when (< (random 100) (* alpha 40))
              (frame-set! frame x y #\.))))

    ;; Labels
    (when (> t 0.3)
      (let ([label-alpha (* (/ (- t 0.3) 0.7) 100)])
        (when (< (random 100) label-alpha)
          (frame-put-string! frame 5 (+ y-core 6) "CORE")
          (frame-put-string! frame 82 (+ y-core 6) "(pure)")

          (frame-put-string! frame 5 (+ y-lattice 6) "LATTICE")
          (frame-put-string! frame 78 (+ y-lattice 6) "(verified)")

          (frame-put-string! frame 5 (+ y-shell 5) "SHELL")
          (frame-put-string! frame 80 (+ y-shell 5) "(impure)"))))))

;;; ====
;;; Main Rendering
;;; ====

(define (render-frame frame-num)
  "Render a single frame of the demo"
  (let ([frame (make-frame WIDTH HEIGHT #\space)])

    (cond
     ;; INTRO: Starfield + Title
     [(and (>= frame-num INTRO-START) (< frame-num INTRO-END))
      (let* ([t (/ (- frame-num INTRO-START) (- INTRO-END INTRO-START))]
             [star-alpha (ease-in-quad t)]
             [title-alpha (ease-out-cubic (max 0 (- t 0.3)))])
        (draw-starfield! frame *starfield* star-alpha)
        (draw-title! frame "THE FOLD" 50 18 title-alpha)
        (draw-title! frame "content-addressed homoiconic universe" 50 22
                     (* title-alpha 0.8)))]

     ;; FLOW: S-expressions streaming
     [(and (>= frame-num FLOW-START) (< frame-num FLOW-END))
      (let* ([t (/ (- frame-num FLOW-START) (- FLOW-END FLOW-START))]
             [offset (inexact->exact (round (* t WIDTH 2)))])
        (draw-starfield! frame *starfield* 0.3)
        (do ([i 0 (+ i 1)])
            ((>= i (length *sample-exprs*)))
            (let* ([expr (list-ref *sample-exprs* i)]
                   [y (+ 10 (* i 4))]
                   [expr-offset (- offset (* i 15))])
              (draw-flowing-expr! frame expr 0 y expr-offset))))]

     ;; NORMALIZATION: Code transforms
     [(and (>= frame-num NORM-START) (< frame-num NORM-END))
      (let ([t (/ (- frame-num NORM-START) (- NORM-END NORM-START))])
        (draw-starfield! frame *starfield* 0.2)
        (draw-normalize-effect! frame t))]

     ;; CRYSTALLIZATION: Hashes form
     [(and (>= frame-num CRYSTAL-START) (< frame-num CRYSTAL-END))
      (let ([t (/ (- frame-num CRYSTAL-START) (- CRYSTAL-END CRYSTAL-START))])
        (draw-starfield! frame *starfield* 0.15)
        (draw-crystallization! frame t)
        (when (> t 0.6)
          (draw-title! frame "{tag, payload, refs[]}" 50 35
                       (* (- t 0.6) 2.5))))]

     ;; LATTICE: Three layers
     [(and (>= frame-num LATTICE-START) (< frame-num LATTICE-END))
      (let ([t (/ (- frame-num LATTICE-START) (- LATTICE-END LATTICE-START))])
        (draw-layers! frame t))]

     ;; OUTRO: Final message
     [(and (>= frame-num OUTRO-START) (< frame-num OUTRO-END))
      (let* ([t (/ (- frame-num OUTRO-START) (- OUTRO-END OUTRO-START))]
             [alpha (ease-in-out-cubic t)])
        (draw-starfield! frame *starfield* (* 0.4 alpha))
        (draw-title! frame "Everything is content-addressed" 50 18 alpha)
        (draw-title! frame "The hash IS the identity" 50 22 (* alpha 0.9))
        (when (> t 0.5)
          (draw-title! frame "github.com/osoleve/the-fold" 50 28
                       (* (- t 0.5) 2))))])

    frame))

;;; ====
;;; Video Generation
;;; ====

(define (generate-fold-demo)
  (display "Generating THE FOLD demo...\n")
  (display (string-append "Resolution: "
                         (number->string WIDTH) "x"
                         (number->string HEIGHT) "\n"))
  (display (string-append "Total frames: "
                         (number->string TOTAL-FRAMES)
                         " (6 seconds @ 60fps)\n"))

  (let ([video (make-video)])

    ;; Generate all frames
    (do ([i 0 (+ i 1)])
        ((>= i TOTAL-FRAMES))
        (when (= (modulo i 30) 0)
          (display (string-append "Frame "
                                 (number->string i)
                                 "/"
                                 (number->string TOTAL-FRAMES)
                                 "\n")))
        (let ([frame (render-frame i)])
          (video-add-frame! video frame)))

    ;; Hold final frame for 1 second
    (display "Adding hold frames...\n")
    (let ([final-frame (render-frame (- TOTAL-FRAMES 1))])
      (do ([i 0 (+ i 1)])
          ((>= i 60))
          (video-add-frame! video final-frame)))

    (display "Done! Exporting...\n")
    video))

;;; ====
;;; Export
;;; ====

(define (demo)
  (let ([video (generate-fold-demo)])
    (display "\nExporting GIF (800x600, 17ms per frame ~= 60fps)...\n")
    (video->gif video
                "/home/oso/the-fold/user/creations/the-fold.gif"
                17)
    (display "\nDemo complete!\n")
    (display "Output: /home/oso/the-fold/user/creations/the-fold.gif\n")))

;; Run the demo
(demo)
