;;; user/creations/lattice-demo.ss — LATTICE CAPABILITIES Demo
;;;
;;; A 15-second demoscene-style ASCII animation showcasing The Fold's
;;; lattice mathematical capabilities:
;;;
;;; PHASE 1 (0-3s):   INTRO - Starfield + title reveal
;;; PHASE 2 (3-6s):   RAYMARCHER - 3D SDF sphere rotation with shading
;;; PHASE 3 (6-9s):   PLASMA - Mathematical interference patterns
;;; PHASE 4 (9-12s):  SINE SCROLLER - Lattice features with wave motion
;;; PHASE 5 (12-15s): OUTRO - Credits with starfield

(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "boundary/ui/animation.ss")

;;; ====
;;; Constants
;;; ====

(define WIDTH 100)
(define HEIGHT 42)
(define TOTAL-FRAMES 900)  ; 15 seconds @ 60fps

;;; Phase boundaries (frames) - 180 frames = 3 seconds per phase
(define INTRO-START 0)
(define INTRO-END 180)
(define RAYMARCH-START 180)
(define RAYMARCH-END 360)
(define PLASMA-START 360)
(define PLASMA-END 540)
(define SCROLLER-START 540)
(define SCROLLER-END 720)
(define OUTRO-START 720)
(define OUTRO-END 900)

;;; Character density ramp (light to dark)
(define DENSITY " .:-=+*#%@")
(define DENSITY-LEN (string-length DENSITY))

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

(define (fract x)
  (- x (floor x)))

(define (mix a b t)
  (+ (* a (- 1.0 t)) (* b t)))

;;; ====
;;; Starfield (Background)
;;; ====

(define (make-starfield n)
  (define stars '())
  (do ([i 0 (+ i 1)])
      ((>= i n))
      (set! stars (cons (list (random WIDTH)
                              (random HEIGHT)
                              (+ 1 (random 3)))  ; depth layer
                        stars)))
  stars)

(define *starfield* (make-starfield 120))

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
;;; Text Rendering Utilities
;;; ====

(define (clear-text-box! frame x y width)
  "Clear a horizontal region for text (prevents starfield interference)"
  (do ([i 0 (+ i 1)])
      ((>= i width))
      (when (and (>= (+ x i) 0) (< (+ x i) WIDTH))
        (frame-set! frame (+ x i) y #\space))))

(define (draw-centered-text! frame text y alpha)
  "Draw centered text with alpha fade - clears background first"
  (let* ([len (string-length text)]
         [x (- (quotient WIDTH 2) (quotient len 2))])
    ;; Clear the text region first (with 2-char padding on each side)
    (when (> alpha 0.3)
      (clear-text-box! frame (- x 2) y (+ len 4)))
    ;; Draw text - deterministic when alpha > 0.8, probabilistic otherwise
    (do ([i 0 (+ i 1)])
        ((>= i len))
        (when (or (>= alpha 0.8)
                  (< (random 100) (* alpha 100)))
          (frame-set! frame (+ x i) y (string-ref text i))))))

(define (draw-solid-text! frame text x y)
  "Draw text without any alpha effects - always fully visible"
  (do ([i 0 (+ i 1)])
      ((>= i (string-length text)))
      (let ([px (+ x i)])
        (when (and (>= px 0) (< px WIDTH))
          (frame-set! frame px y (string-ref text i))))))

(define (draw-centered-solid! frame text y)
  "Draw centered text, always fully visible, with cleared background"
  (let* ([len (string-length text)]
         [x (- (quotient WIDTH 2) (quotient len 2))])
    (clear-text-box! frame (- x 2) y (+ len 4))
    (draw-solid-text! frame text x y)))

;;; ====
;;; PHASE 1: Title Reveal
;;; ====

(define (draw-intro! frame t)
  "Starfield with title reveal"
  (let* ([star-alpha (ease-in-quad t)]
         [title-alpha (ease-out-cubic (max 0 (- t 0.2)))]
         [subtitle-alpha (ease-out-cubic (max 0 (- t 0.4)))])

    (draw-starfield! frame *starfield* star-alpha)

    ;; Main title
    (draw-centered-text! frame "THE LATTICE" 16 title-alpha)

    ;; Subtitle
    (draw-centered-text! frame "verified mathematical capabilities" 20 subtitle-alpha)

    ;; Tagline appears late
    (when (> t 0.6)
      (draw-centered-text! frame "pure - total - content-addressed"
                          24 (* (- t 0.6) 2.5)))))

;;; ====
;;; PHASE 2: 3D SDF Raymarcher
;;; ====

;;; Simple 3D vector operations
(define (vec3 x y z) (list x y z))
(define (vec3-x v) (car v))
(define (vec3-y v) (cadr v))
(define (vec3-z v) (caddr v))

(define (vec3-add a b)
  (vec3 (+ (vec3-x a) (vec3-x b))
        (+ (vec3-y a) (vec3-y b))
        (+ (vec3-z a) (vec3-z b))))

(define (vec3-sub a b)
  (vec3 (- (vec3-x a) (vec3-x b))
        (- (vec3-y a) (vec3-y b))
        (- (vec3-z a) (vec3-z b))))

(define (vec3-scale v s)
  (vec3 (* (vec3-x v) s)
        (* (vec3-y v) s)
        (* (vec3-z v) s)))

(define (vec3-length v)
  (sqrt (+ (* (vec3-x v) (vec3-x v))
           (* (vec3-y v) (vec3-y v))
           (* (vec3-z v) (vec3-z v)))))

(define (vec3-normalize v)
  (let ([len (vec3-length v)])
    (if (< len 0.0001)
        (vec3 0 1 0)
        (vec3-scale v (/ 1.0 len)))))

(define (vec3-dot a b)
  (+ (* (vec3-x a) (vec3-x b))
     (* (vec3-y a) (vec3-y b))
     (* (vec3-z a) (vec3-z b))))

;;; SDF: Sphere at origin
(define (sdf-sphere pos radius)
  (lambda (p)
    (- (vec3-length (vec3-sub p pos)) radius)))

;;; Simple raymarcher
(define (raymarch sdf-fn ray-origin ray-dir max-steps)
  (define (march t steps)
    (if (or (>= steps max-steps) (>= t 20.0))
        #f
        (let* ([pos (vec3-add ray-origin (vec3-scale ray-dir t))]
               [dist (sdf-fn pos)])
          (if (< dist 0.01)
              (list pos t)
              (march (+ t (max 0.01 dist)) (+ steps 1))))))
  (march 0.0 0))

;;; Compute normal via gradient
(define (sdf-normal sdf-fn p)
  (let* ([eps 0.001]
         [dx (- (sdf-fn (vec3-add p (vec3 eps 0 0)))
                (sdf-fn (vec3-sub p (vec3 eps 0 0))))]
         [dy (- (sdf-fn (vec3-add p (vec3 0 eps 0)))
                (sdf-fn (vec3-sub p (vec3 0 eps 0))))]
         [dz (- (sdf-fn (vec3-add p (vec3 0 0 eps)))
                (sdf-fn (vec3-sub p (vec3 0 0 eps))))])
    (vec3-normalize (vec3 dx dy dz))))

(define (draw-raymarch! frame t)
  "Render rotating 3D sphere using SDF raymarching"
  (let* ([angle (* t PI 2)]
         [sphere-pos (vec3 0 0 0)]
         [sphere-radius 2.0]
         [sdf (sdf-sphere sphere-pos sphere-radius)]
         [light-dir (vec3-normalize (vec3 0.5 0.8 0.3))]

         ;; Camera rotating around sphere
         [cam-dist 6.0]
         [cam-x (* cam-dist (cos angle))]
         [cam-z (* cam-dist (sin angle))]
         [cam-pos (vec3 cam-x 1.0 cam-z)]

         ;; Camera basis
         [cam-fwd (vec3-normalize (vec3-sub sphere-pos cam-pos))]
         [cam-right (vec3-normalize (vec3 (- (vec3-z cam-fwd)) 0 (vec3-x cam-fwd)))]
         [cam-up (vec3 0 1 0)]

         ;; Render params
         [aspect (/ WIDTH HEIGHT)]
         [fov 0.8])

    ;; Dim starfield background
    (draw-starfield! frame *starfield* 0.15)

    ;; Label at top (solid, always readable)
    (draw-centered-solid! frame "lattice/geometry/raymarch.ss" 2)
    (draw-centered-solid! frame "SDF sphere tracing with shading" 4)

    ;; Render sphere
    (do ([screen-y 8 (+ screen-y 1)])
        ((>= screen-y (- HEIGHT 4)))
        (do ([screen-x 10 (+ screen-x 1)])
            ((>= screen-x (- WIDTH 10)))
            (let* ([u (* (- (/ screen-x WIDTH) 0.5) 2.0 aspect)]
                   [v (* (- 0.5 (/ screen-y HEIGHT)) 2.0)]
                   [ray-dir (vec3-normalize
                            (vec3-add
                             (vec3-add cam-fwd
                                      (vec3-scale cam-right (* u fov)))
                             (vec3-scale cam-up (* v fov))))]
                   [hit (raymarch sdf cam-pos ray-dir 30)])

              (when hit
                (let* ([hit-pos (car hit)]
                       [normal (sdf-normal sdf hit-pos)]
                       [ndotl (max 0.1 (vec3-dot normal light-dir))]
                       [density-idx (inexact->exact
                                    (floor (* (min 1.0 ndotl)
                                            (- DENSITY-LEN 1))))]
                       [char (string-ref DENSITY density-idx)])
                  (frame-set! frame screen-x screen-y char))))))))

;;; ====
;;; PHASE 3: Plasma Effect
;;; ====

(define (plasma x y t)
  "Interference pattern from sine waves"
  (let* ([cx (/ (- x (/ WIDTH 2)) 10.0)]
         [cy (/ (- y (/ HEIGHT 2)) 10.0)]
         [v1 (sin (+ (* cx 0.5) t))]
         [v2 (sin (+ (* cy 0.5) (* t 0.8)))]
         [v3 (sin (+ (* (sqrt (+ (* cx cx) (* cy cy))) 0.3) (* t 1.2)))]
         [v4 (sin (+ cx cy (* t 0.5)))]
         [combined (/ (+ v1 v2 v3 v4) 4.0)])
    ;; Map [-1, 1] to [0, 1]
    (* 0.5 (+ 1.0 combined))))

(define (draw-plasma! frame t)
  "Mathematical interference patterns"
  (let ([time-phase (* t 6.0)])

    ;; Draw plasma
    (do ([y 0 (+ y 1)])
        ((>= y HEIGHT))
        (do ([x 0 (+ x 1)])
            ((>= x WIDTH))
            (let* ([intensity (plasma x y time-phase)]
                   [idx (inexact->exact
                         (floor (* intensity (- DENSITY-LEN 1))))]
                   [char (string-ref DENSITY (clamp idx 0 (- DENSITY-LEN 1)))])
              (frame-set! frame x y char))))

    ;; Labels (solid, always readable above plasma)
    (draw-centered-solid! frame "lattice/numeric/dft.ss + sin/cos interference" 2)
    (draw-centered-solid! frame "Mathematical beauty in motion" 4)))

;;; ====
;;; PHASE 4: Sine Scroller
;;; ====

(define *lattice-features*
  '("linalg: vectors, matrices, decompositions"
    "autodiff: reverse-mode automatic differentiation"
    "geometry: SDFs, raymarching, mesh topology"
    "topology: simplicial complexes, homology"
    "diffgeo: charts, Lie groups, curvature"
    "fp: monads, parsers, game theory"
    "statistics: regression, hypothesis testing"
    "optimization: LP, gradient descent, L-BFGS"
    "physics: differentiable 2D/3D dynamics"
    "crypto: SHA-512, BLAKE2b, HMAC"))

(define (draw-sine-scroller! frame t)
  "Scrolling text with sine wave motion"
  (let* ([scroll-speed 2.0]
         [amplitude 8.0]
         [frequency 0.15]
         [base-y (quotient HEIGHT 2)])

    ;; Background
    (draw-starfield! frame *starfield* 0.2)

    ;; Title (solid, always readable)
    (draw-centered-solid! frame "THE LATTICE SKILLS" 6)
    (draw-centered-solid! frame "verified - pure - total" 8)

    ;; Scrolling features
    (do ([i 0 (+ i 1)])
        ((>= i (length *lattice-features*)))
        (let* ([feature (list-ref *lattice-features* i)]
               [feature-len (string-length feature)]
               [y-base (+ base-y (* i 3))]
               ;; Scroll offset
               [scroll-offset (inexact->exact
                              (floor (* t scroll-speed WIDTH)))]
               [x-start (- WIDTH scroll-offset (* i 30))])

          ;; Draw each character with sine wave
          (do ([char-idx 0 (+ char-idx 1)])
              ((>= char-idx feature-len))
              (let* ([x (+ x-start char-idx)]
                     [wave-offset (* amplitude
                                   (sin (* (+ x (* t 20)) frequency)))]
                     [y (+ y-base (inexact->exact (round wave-offset)))]
                     [char (string-ref feature char-idx)])

                (when (and (>= x 0) (< x WIDTH)
                          (>= y 0) (< y HEIGHT))
                  (frame-set! frame x y char))))))

    ;; Footer hint (solid once it appears)
    (when (> t 0.5)
      (draw-centered-solid! frame "explore with (lf query)" (- HEIGHT 3)))))

;;; ====
;;; PHASE 5: Outro/Credits
;;; ====

(define (draw-outro! frame t)
  "Credits with starfield"
  (let* ([alpha (ease-in-out-cubic t)])

    (draw-starfield! frame *starfield* (* 0.5 alpha))

    (draw-centered-text! frame "THE FOLD" 14 alpha)
    (draw-centered-text! frame "content-addressed homoiconic universe" 16 (* alpha 0.9))

    (when (> t 0.3)
      (let ([sub-alpha (* (- t 0.3) 1.4)])
        (draw-centered-text! frame "-- lattice skills --" 20 sub-alpha)
        (draw-centered-text! frame "verified - pure - total - typed" 22 (* sub-alpha 0.9))))

    (when (> t 0.6)
      (draw-centered-solid! frame "greets to:" 26)
      (draw-centered-solid! frame "Future Crew - Farbrausch - All ASCII Artists" 28))

    (when (> t 0.8)
      (draw-centered-text! frame "github.com/osoleve/the-fold"
                          32 (* (- t 0.8) 5)))))

;;; ====
;;; Main Rendering
;;; ====

(define (render-frame frame-num)
  "Render a single frame of the demo"
  (let ([frame (make-frame WIDTH HEIGHT #\space)])

    (cond
     ;; INTRO: Starfield + Title
     [(and (>= frame-num INTRO-START) (< frame-num INTRO-END))
      (let ([t (/ (- frame-num INTRO-START)
                 (- INTRO-END INTRO-START))])
        (draw-intro! frame t))]

     ;; RAYMARCH: 3D rotating sphere
     [(and (>= frame-num RAYMARCH-START) (< frame-num RAYMARCH-END))
      (let ([t (/ (- frame-num RAYMARCH-START)
                 (- RAYMARCH-END RAYMARCH-START))])
        (draw-raymarch! frame t))]

     ;; PLASMA: Interference patterns
     [(and (>= frame-num PLASMA-START) (< frame-num PLASMA-END))
      (let ([t (/ (- frame-num PLASMA-START)
                 (- PLASMA-END PLASMA-START))])
        (draw-plasma! frame t))]

     ;; SCROLLER: Sine wave text
     [(and (>= frame-num SCROLLER-START) (< frame-num SCROLLER-END))
      (let ([t (/ (- frame-num SCROLLER-START)
                 (- SCROLLER-END SCROLLER-START))])
        (draw-sine-scroller! frame t))]

     ;; OUTRO: Credits
     [(and (>= frame-num OUTRO-START) (< frame-num OUTRO-END))
      (let ([t (/ (- frame-num OUTRO-START)
                 (- OUTRO-END OUTRO-START))])
        (draw-outro! frame t))])

    frame))

;;; ====
;;; Video Generation
;;; ====

(define (generate-lattice-demo)
  (display "Generating LATTICE CAPABILITIES demo...\n")
  (display (string-append "Resolution: "
                         (number->string WIDTH) "x"
                         (number->string HEIGHT) "\n"))
  (display (string-append "Total frames: "
                         (number->string TOTAL-FRAMES)
                         " (15 seconds @ 60fps)\n"))
  (display "Phases:\n")
  (display "  0-3s:   Intro (title reveal)\n")
  (display "  3-6s:   Raymarcher (3D SDF sphere)\n")
  (display "  6-9s:   Plasma (interference patterns)\n")
  (display "  9-12s:  Sine Scroller (features)\n")
  (display " 12-15s:  Outro (credits)\n\n")

  (let ([video (make-video)])

    ;; Generate all frames
    (do ([i 0 (+ i 1)])
        ((>= i TOTAL-FRAMES))
        (when (= (modulo i 60) 0)
          (display (string-append "Frame "
                                 (number->string i) "/"
                                 (number->string TOTAL-FRAMES)
                                 " ("
                                 (number->string (quotient (* i 100) TOTAL-FRAMES))
                                 "%)\n")))
        (let ([frame (render-frame i)])
          (video-add-frame! video frame)))

    ;; Hold final frame for 1 second
    (display "Adding hold frames...\n")
    (let ([final-frame (render-frame (- TOTAL-FRAMES 1))])
      (do ([i 0 (+ i 1)])
          ((>= i 60))
          (video-add-frame! video final-frame)))

    (display "Done!\n")
    video))

;;; ====
;;; Export
;;; ====

(define (demo)
  (let ([video (generate-lattice-demo)])
    (display "\nExporting GIF (800x600, 17ms per frame ~= 60fps)...\n")
    (video->gif video
                "/home/oso/the-fold/user/creations/lattice-demo.gif"
                17)
    (display "\nExporting MP4 (60fps)...\n")
    (video->mp4 video
                "/home/oso/the-fold/user/creations/lattice-demo.mp4"
                60)
    (display "\nDemo complete!\n")
    (display "Outputs:\n")
    (display "  GIF: /home/oso/the-fold/user/creations/lattice-demo.gif\n")
    (display "  MP4: /home/oso/the-fold/user/creations/lattice-demo.mp4\n")))

;; Run the demo
(demo)
