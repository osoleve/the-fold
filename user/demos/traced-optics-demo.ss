;;; user/demos/traced-optics-demo.ss — TRACED OPTICS VISUALIZATION
;;;
;;; A demoscene-style ASCII animation demonstrating gradient flow
;;; through optic-focused paths. Shows the marriage of optics + autodiff.
;;;
;;; Run with: scheme --script user/demos/traced-optics-demo.ss

(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "lattice/autodiff/traced-optics.ss")
(load "lattice/linalg/vec.ss")

;;; ====
;;; Configuration
;;; ====

(define WIDTH 100)
(define HEIGHT 42)
(define FPS 12)

;;; ====
;;; Character Density Palette
;;; ====

(define DENSITY-RAMP " .',:;-~=+*#%@")
(define RAMP-LEN (string-length DENSITY-RAMP))

(define (value->char v)
  "Map 0.0-1.0 to character density"
  (let* ([clamped (max 0.0 (min 1.0 v))]
         [idx (inexact->exact (floor (* clamped (- RAMP-LEN 1))))])
    (string-ref DENSITY-RAMP idx)))

(define (gradient->char g)
  "Map gradient magnitude to character"
  (value->char (/ (abs g) 5.0)))  ; Scale factor for visibility

;;; ====
;;; Starfield Background (Classic Demoscene)
;;; ====

(define (make-starfield n-stars)
  "Generate starfield: list of (x y depth char)"
  (let loop ([i 0] [stars '()])
    (if (>= i n-stars)
        stars
        (let* ([x (random WIDTH)]
               [y (random HEIGHT)]
               [depth (+ 1 (random 4))]
               [char (case depth
                       [(1) #\.]
                       [(2) #\·]
                       [(3) #\*]
                       [else #\✦])])
          (loop (+ i 1)
                (cons (list x y depth char) stars))))))

(define (render-starfield! frame stars phase)
  "Render starfield with parallax motion"
  (for-each
   (lambda (star)
     (let* ([x (car star)]
            [y (cadr star)]
            [depth (caddr star)]
            [char (cadddr star)]
            ;; Parallax motion
            [offset (inexact->exact (floor (/ (* phase depth) 4)))]
            [x-shifted (modulo (- x offset) WIDTH)])
       (frame-set! frame x-shifted y char)))
   stars))

;;; ====
;;; Scene 1: LENS GRADIENT FLOW
;;; ====
;;; Show a nested structure with lens focusing on one parameter.
;;; Visualize gradient computation with arrows and values.

(define (make-nested-structure x y)
  "Create nested data: (outer (inner x y))"
  (cons 'outer (cons (cons 'inner (cons x y)) '())))

(define outer-lens
  (make-lens
   (lambda (s) (cadr s))     ; view: get (inner x . y)
   (lambda (v s) (cons 'outer (cons v '())))))  ; set: replace inner

(define inner-x-lens
  (make-lens
   (lambda (s) (cadr s))    ; view: get x from (inner x . y)
   (lambda (v s) (cons 'inner (cons v (cddr s))))))

(define lens-path (>>> outer-lens inner-x-lens))

(define (loss-fn-1 struct)
  "Loss = (x - 5)^2, we want gradient to push x toward 5"
  (let* ([x (view lens-path struct)]
         [diff (traced-sub x 5)])
    (traced-mul diff diff)))

(define (scene-1-frame t frame-num total-frames stars)
  "Scene 1: Lens gradient visualization"
  (let* ([frame (make-frame WIDTH HEIGHT #\space)]

         ;; Phase for animations
         [phase (/ t 1.5)]

         ;; Current parameter value (starts at 10, should move toward 5)
         [x-val (- 10 (* t 0.5))]  ; Gradually approaching 5
         [x-val (max 5.0 x-val)]   ; Clamp at target

         ;; Compute gradient
         [struct (make-nested-structure x-val 3.0)]
         [grad (optic-gradient loss-fn-1 lens-path struct)]

         ;; Background starfield
         [_ (render-starfield! frame stars phase)]

         ;; Title (top)
         [title "═══ LENS GRADIENT FLOW ═══"]
         [title-x (- (quotient WIDTH 2) (quotient (string-length title) 2))]
         [_ (frame-put-string! frame title-x 2 title)]

         ;; Nested structure visualization (left side)
         [y-base 8]
         [_ (frame-put-string! frame 8 y-base "OUTER")]
         [_ (frame-draw-box! frame 6 (+ y-base 1) 24 10)]
         [_ (frame-put-string! frame 10 (+ y-base 2) "INNER")]
         [_ (frame-draw-box! frame 8 (+ y-base 3) 20 6)]

         ;; Parameter display
         [x-str (number->string (exact->inexact x-val))]
         [_ (frame-put-string! frame 11 (+ y-base 5) "x = ")]
         [_ (frame-put-string! frame 16 (+ y-base 5) x-str)]
         [_ (frame-put-string! frame 11 (+ y-base 7) "y = 3.0")]

         ;; Lens path arrow (center)
         [arrow-x 32]
         [arrow-y 12]
         [_ (frame-put-string! frame arrow-x arrow-y "LENS PATH")]
         [_ (frame-put-string! frame (+ arrow-x 2) (+ arrow-y 2) "|")]
         [_ (frame-put-string! frame (+ arrow-x 2) (+ arrow-y 3) "V")]
         [_ (frame-put-string! frame arrow-x (+ arrow-y 4) "outer >>> x")]

         ;; Gradient visualization (right side)
         [grad-x 52]
         [grad-y 8]
         [_ (frame-put-string! frame grad-x grad-y "GRADIENT")]
         [_ (frame-draw-box! frame (- grad-x 2) (+ grad-y 1) 28 12)]

         [grad-str (number->string (exact->inexact grad))]
         [_ (frame-put-string! frame grad-x (+ grad-y 3) "dx/dloss =")]
         [_ (frame-put-string! frame (+ grad-x 2) (+ grad-y 5) grad-str)]

         ;; Gradient arrow visualization
         [arrow-len (inexact->exact (floor (min 15 (max 1 (abs (/ grad 0.5))))))]
         [arrow-dir (if (< grad 0) #\< #\>)]
         [arrow-body (make-string (max 0 (inexact->exact (- arrow-len 1))) #\-)]
         [full-arrow (if (< grad 0)
                         (string-append (string arrow-dir) arrow-body)
                         (string-append arrow-body (string arrow-dir)))]
         [_ (frame-put-string! frame (+ grad-x 2) (+ grad-y 8) full-arrow)]

         ;; Target indicator
         [_ (frame-put-string! frame grad-x (+ grad-y 10) "Target: x = 5.0")]

         ;; Loss value (bottom center)
         [loss-val (let ([diff (- x-val 5)])
                     (* diff diff))]
         [loss-str (number->string (exact->inexact loss-val))]
         [_ (frame-put-string! frame 38 35 "Loss = ")]
         [_ (frame-put-string! frame 45 35 loss-str)]

         ;; Plasma-like gradient intensity visualization
         [_ (let loop ([x 10] [y 25])
              (if (>= y 32)
                  #f
                  (if (>= x 80)
                      (loop 10 (+ y 1))
                      (let* ([dx (- x 45)]
                             [dy (- y 28.5)]
                             [dist (sqrt (+ (* dx dx) (* dy dy)))]
                             [intensity (+ 0.5 (* 0.5 (sin (+ (/ dist 3) phase))))]
                             [char (value->char intensity)])
                        (frame-set! frame x y char)
                        (loop (+ x 1) y)))))])
    frame))

;;; ====
;;; Scene 2: TRAVERSAL GRADIENT
;;; ====
;;; Multiple targets being optimized simultaneously

(define (scene-2-frame t frame-num total-frames stars)
  "Scene 2: Traversal with multiple gradient targets"
  (let* ([frame (make-frame WIDTH HEIGHT #\space)]

         [phase (/ t 1.5)]

         ;; Three parameters being optimized toward [2, 4, 6]
         [targets '(2.0 4.0 6.0)]
         [initial '(8.0 9.0 10.0)]
         [current (map (lambda (init target)
                         (let ([diff (- init target)]
                               [step (* t 0.5)])
                           (+ target (* diff (exp (- step))))))
                       initial targets)]

         ;; Compute gradients (loss = sum of (x_i - target_i)^2)
         [loss-fn (lambda (xs)
                    (traced-sum
                     (map (lambda (x target)
                            (let ([diff (traced-sub x target)])
                              (traced-mul diff diff)))
                          xs targets)))]
         [grads (optic-gradient-list loss-fn traversal-each current)]

         ;; Background
         [_ (render-starfield! frame stars phase)]

         ;; Title
         [title "═══ TRAVERSAL GRADIENT (Multi-Target) ═══"]
         [title-x (- (quotient WIDTH 2) (quotient (string-length title) 2))]
         [_ (frame-put-string! frame title-x 2 title)]

         ;; Render each parameter with its gradient
         [_ (let loop ([i 0] [y-pos 8])
              (if (>= i 3)
                  #f
                  (let* ([x-base 15]
                         [val (list-ref current i)]
                         [grad (list-ref grads i)]
                         [target (list-ref targets i)]
                         [label (string-append "x" (number->string i))]

                         ;; Box for parameter
                         [_ (frame-draw-box! frame x-base y-pos 25 6)]
                         [_ (frame-put-string! frame (+ x-base 2) (+ y-pos 1) label)]

                         ;; Current value
                         [val-str (number->string (exact->inexact val))]
                         [_ (frame-put-string! frame (+ x-base 2) (+ y-pos 3) "val: ")]
                         [_ (frame-put-string! frame (+ x-base 8) (+ y-pos 3) val-str)]

                         ;; Gradient arrow
                         [grad-len (inexact->exact (floor (min 10 (max 1 (abs (/ grad 0.3))))))]
                         [grad-dir (if (< grad 0) #\< #\>)]
                         [grad-body (make-string (max 0 (inexact->exact (- grad-len 1))) #\=)]
                         [grad-arrow (if (< grad 0)
                                         (string-append (string grad-dir) grad-body)
                                         (string-append grad-body (string grad-dir)))]
                         [_ (frame-put-string! frame (+ x-base 2) (+ y-pos 4) grad-arrow)]

                         ;; Gradient on right side
                         [grad-x 50]
                         [grad-str (number->string (exact->inexact grad))]
                         [_ (frame-put-string! frame grad-x (+ y-pos 2) "grad:")]
                         [_ (frame-put-string! frame (+ grad-x 6) (+ y-pos 2) grad-str)]

                         ;; Target marker
                         [target-str (number->string (exact->inexact target))]
                         [_ (frame-put-string! frame grad-x (+ y-pos 4) "→")]
                         [_ (frame-put-string! frame (+ grad-x 2) (+ y-pos 4) target-str)])
                    (loop (+ i 1) (+ y-pos 9)))))]

         ;; Flow visualization (bottom)
         [_ (frame-put-string! frame 30 32 "Simultaneous Gradient Descent")]

         ;; Sine wave pattern showing convergence
         [_ (let loop ([x 10])
              (if (>= x 90)
                  #f
                  (let* ([amp (exp (- (* t 0.3)))]  ; Decaying amplitude
                         [wave (* amp (sin (+ (* x 0.3) phase)))]
                         [y (+ 37 (inexact->exact (round (* wave 3))))]
                         [char (value->char (+ 0.5 (* 0.5 wave)))])
                    (when (and (>= y 0) (< y HEIGHT))
                      (frame-set! frame x y char))
                    (loop (+ x 1)))))])
    frame))

;;; ====
;;; Scene 3: OPTIMIZATION PATH
;;; ====
;;; Animate gradient descent steps

(define (scene-3-frame t frame-num total-frames stars)
  "Scene 3: Optimization trajectory"
  (let* ([frame (make-frame WIDTH HEIGHT #\space)]

         [phase (/ t 1.5)]

         ;; Parameter space: 2D optimization
         ;; Minimum at (5, 5), start at (15, 15)
         [n-steps (min 20 (inexact->exact (floor (* t 2))))]

         ;; Loss: (x-5)^2 + (y-5)^2
         [loss-fn-2d (lambda (struct)
                       (let* ([x (car struct)]
                              [y (cadr struct)]
                              [dx (traced-sub x 5)]
                              [dy (traced-sub y 5)])
                         (traced-add (traced-mul dx dx)
                                     (traced-mul dy dy))))]

         ;; Lens for first element
         [x-lens (make-lens car (lambda (v s) (cons v (cdr s))))]

         ;; Simulate gradient descent steps
         [trajectory
          (let loop ([i 0] [pos (list 15.0 15.0)] [path '()])
            (if (>= i n-steps)
                (reverse (cons pos path))
                (let* ([grad-x (optic-gradient loss-fn-2d x-lens pos)]
                       [rate 0.2]
                       [new-x (- (car pos) (* rate grad-x))]
                       [new-pos (list new-x (cadr pos))])  ; Optimize x only for demo
                  (loop (+ i 1) new-pos (cons pos path)))))]

         ;; Background
         [_ (render-starfield! frame stars phase)]

         ;; Title
         [title "═══ OPTIMIZATION PATH ═══"]
         [title-x (- (quotient WIDTH 2) (quotient (string-length title) 2))]
         [_ (frame-put-string! frame title-x 2 title)]

         ;; Parameter space visualization
         [grid-x 20]
         [grid-y 10]
         [grid-w 60]
         [grid-h 20]

         ;; Grid border
         [_ (frame-draw-box! frame grid-x grid-y grid-w grid-h)]

         ;; Contour lines (loss landscape)
         [_ (let y-loop ([gy 0])
              (if (>= gy grid-h)
                  #f
                  (let x-loop ([gx 0])
                    (if (>= gx grid-w)
                        (y-loop (+ gy 1))
                        (let* ([x-param (+ 0 (* 20 (/ gx grid-w)))]
                               [y-param (+ 0 (* 20 (/ gy grid-h)))]
                               [loss (+ (* (- x-param 5) (- x-param 5))
                                        (* (- y-param 5) (- y-param 5)))]
                               [level (sqrt loss)]
                               [char (cond
                                      [(< level 2) #\@]
                                      [(< level 5) #\#]
                                      [(< level 8) #\*]
                                      [(< level 12) #\+]
                                      [else #\.])])
                          (frame-set! frame (+ grid-x gx) (+ grid-y gy) char)
                          (x-loop (+ gx 1)))))))]

         ;; Trajectory path
         [_ (for-each
             (lambda (pos)
               (let* ([x (car pos)]
                      [y (cadr pos)]
                      ;; Map to grid coordinates
                      [gx (+ grid-x (inexact->exact (round (* (/ x 20) grid-w))))]
                      [gy (+ grid-y (inexact->exact (round (* (/ y 20) grid-h))))])
                 (when (and (>= gx grid-x) (< gx (+ grid-x grid-w))
                            (>= gy grid-y) (< gy (+ grid-y grid-h)))
                   (frame-set! frame gx gy #\O))))
             trajectory)]

         ;; Current position marker
         [_ (when (pair? trajectory)
              (let* ([current (car (reverse trajectory))]
                     [x (car current)]
                     [y (cadr current)]
                     [gx (+ grid-x (inexact->exact (round (* (/ x 20) grid-w))))]
                     [gy (+ grid-y (inexact->exact (round (* (/ y 20) grid-h))))])
                (when (and (>= gx grid-x) (< gx (+ grid-x grid-w))
                           (>= gy grid-y) (< gy (+ grid-y grid-h)))
                  (frame-set! frame gx gy #\X))))]

         ;; Info panel
         [info-x 22]
         [info-y 32]
         [_ (frame-put-string! frame info-x info-y "Gradient Descent Steps:")]
         [steps-str (number->string n-steps)]
         [_ (frame-put-string! frame (+ info-x 25) info-y steps-str)]

         [_ (when (pair? trajectory)
              (let* ([current (car (reverse trajectory))]
                     [x (car current)]
                     [y (cadr current)]
                     [x-str (number->string (exact->inexact x))]
                     [y-str (number->string (exact->inexact y))])
                (frame-put-string! frame info-x (+ info-y 2) "Current: (")
                (frame-put-string! frame (+ info-x 10) (+ info-y 2) x-str)
                (frame-put-string! frame (+ info-x 16) (+ info-y 2) ", ")
                (frame-put-string! frame (+ info-x 18) (+ info-y 2) y-str)
                (frame-put-string! frame (+ info-x 24) (+ info-y 2) ")")))]

         [_ (frame-put-string! frame info-x (+ info-y 4) "Target:  (5.0, 5.0)")]

         ;; Legend
         [_ (frame-put-string! frame info-x (+ info-y 7) "@ = minimum")]
         [_ (frame-put-string! frame info-x (+ info-y 8) "O = path")]
         [_ (frame-put-string! frame info-x (+ info-y 9) "X = current")])
    frame))

;;; ====
;;; Scene 4: FINALE - CREDITS
;;; ====

(define (scene-4-frame t frame-num total-frames stars)
  "Scene 4: Credits with sine scroller"
  (let* ([frame (make-frame WIDTH HEIGHT #\space)]

         [phase (/ t 1.2)]

         ;; Background
         [_ (render-starfield! frame stars phase)]

         ;; Sine scroller message
         [message "    TRACED OPTICS    GRADIENTS THROUGH LENSES    AUTODIFF + OPTICS    THE FOLD    "]
         [msg-len (string-length message)]

         ;; Render sine scroller
         [_ (let loop ([i 0])
              (if (>= i msg-len)
                  #f
                  (let* ([char (string-ref message i)]
                         [x (+ 10 i)]
                         [amplitude 4]
                         [y-offset (* amplitude (sin (+ (* i 0.3) phase)))]
                         [y (+ 15 (inexact->exact (round y-offset)))])
                    (when (and (>= x 0) (< x WIDTH)
                               (>= y 0) (< y HEIGHT))
                      (frame-set! frame x y char))
                    (loop (+ i 1)))))]

         ;; Credits
         [credits-y 25]
         [_ (frame-put-string! frame 35 credits-y "═══════════════════")]
         [_ (frame-put-string! frame 38 (+ credits-y 2) "code: DEMOSCENE")]
         [_ (frame-put-string! frame 39 (+ credits-y 3) "math: The Fold")]
         [_ (frame-put-string! frame 35 (+ credits-y 5) "═══════════════════")]

         ;; Greets
         [_ (frame-put-string! frame 38 (+ credits-y 8) "─── greets to ───")]
         [_ (frame-put-string! frame 30 (+ credits-y 10) "Anthropic · Future Crew · All ASCII Artists")]

         ;; Tech specs
         [_ (frame-put-string! frame 32 (+ credits-y 13) "100x42 chars · 12fps · 800x600 pixels")])
    frame))

;;; ====
;;; Main Rendering
;;; ====

(define (render-video)
  (display "\n")
  (display "╔═══════════════════════════════════════════════════════════════════╗\n")
  (display "║                                                                   ║\n")
  (display "║               TRACED OPTICS VISUALIZATION                        ║\n")
  (display "║               Gradients Through Optic Paths                      ║\n")
  (display "║                                                                   ║\n")
  (display "╚═══════════════════════════════════════════════════════════════════╝\n\n")

  (display "Rendering demoscene animation...\n\n")

  (let* ([video (make-video)]
         [stars (make-starfield 150)]

         ;; Scene timing (12fps)
         ;; Scene 1: 0-2.5s (30 frames)
         ;; Scene 2: 2.5-5s (30 frames)
         ;; Scene 3: 5-8s (36 frames)
         ;; Scene 4: 8-12s (48 frames)
         [total-frames 144]  ; 12 seconds at 12fps

         [_ (display "Scene 1: Lens Gradient Flow...\n")]
         [_ (let loop ([f 0])
              (when (< f 30)
                (let* ([t (/ f FPS)]
                       [frame (scene-1-frame t f total-frames stars)])
                  (video-add-frame! video frame)
                  (when (zero? (modulo f 10))
                    (display (string-append "  Frame " (number->string f) "/30\n")))
                  (loop (+ f 1)))))]

         [_ (display "\nScene 2: Traversal Gradients...\n")]
         [_ (let loop ([f 0])
              (when (< f 30)
                (let* ([t (/ f FPS)]
                       [frame (scene-2-frame t f total-frames stars)])
                  (video-add-frame! video frame)
                  (when (zero? (modulo f 10))
                    (display (string-append "  Frame " (number->string f) "/30\n")))
                  (loop (+ f 1)))))]

         [_ (display "\nScene 3: Optimization Path...\n")]
         [_ (let loop ([f 0])
              (when (< f 36)
                (let* ([t (/ f FPS)]
                       [frame (scene-3-frame t f total-frames stars)])
                  (video-add-frame! video frame)
                  (when (zero? (modulo f 10))
                    (display (string-append "  Frame " (number->string f) "/36\n")))
                  (loop (+ f 1)))))]

         [_ (display "\nScene 4: Credits...\n")]
         [_ (let loop ([f 0])
              (when (< f 48)
                (let* ([t (/ f FPS)]
                       [frame (scene-4-frame t f total-frames stars)])
                  (video-add-frame! video frame)
                  (when (zero? (modulo f 10))
                    (display (string-append "  Frame " (number->string f) "/48\n")))
                  (loop (+ f 1)))))])

    (display "\n")
    (display (string-append "Total frames: " (number->string (video-frame-count video)) "\n"))
    (display "Exporting to GIF...\n\n")

    ;; Export at 12fps = 83.33ms per frame
    (video->gif video "user/demos/traced-optics-demo.gif" 83)

    (display "\n")
    (display "═══════════════════════════════════════════\n")
    (display "  ✓ Demo complete!\n")
    (display "  ✓ Saved: user/demos/traced-optics-demo.gif\n")
    (display "═══════════════════════════════════════════\n\n")))

;;; Execute
(render-video)
