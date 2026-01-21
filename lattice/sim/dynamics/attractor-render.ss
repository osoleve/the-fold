(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/sim/dynamics/chaos.ss")

(doc 'module 'attractor-render)
(doc 'description "ASCII visualization of strange attractors with rotation, depth shading, and animation")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'features "3D point cloud rendering, camera rotation, depth-based intensity, ANSI color support, animation frames")

;;; ============================================================
;;; Section: Point Cloud Rendering
;;; ============================================================

(doc 'section 'point-cloud-rendering)

(define attractor-ascii-ramp " .'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$")
(doc attractor-ascii-ramp 'description "Extended ASCII ramp for attractor rendering (69 chars, dark to bright)")

(define (intensity->attractor-char intensity)
  (doc 'type '(-> Number Char))
  (doc 'description "Map intensity [0,1] to ASCII character from extended ramp")
  (let* ([ramp-len (string-length attractor-ascii-ramp)]
         [idx (min (- ramp-len 1)
                   (max 0 (inexact->exact (floor (* intensity ramp-len)))))])
        (string-ref attractor-ascii-ramp idx)))

;;; ============================================================
;;; Section: 3D Rotation
;;; ============================================================

(doc 'section 'rotation)

(define (rotate-x point angle)
  (doc 'type '(-> Vec Number Vec))
  (doc 'description "Rotate point around X axis")
  (let* ([y (vector-ref point 1)]
         [z (vector-ref point 2)]
         [cos-a (cos angle)]
         [sin-a (sin angle)])
        (vector (vector-ref point 0)
                (- (* y cos-a) (* z sin-a))
                (+ (* y sin-a) (* z cos-a)))))

(define (rotate-y point angle)
  (doc 'type '(-> Vec Number Vec))
  (doc 'description "Rotate point around Y axis")
  (let* ([x (vector-ref point 0)]
         [z (vector-ref point 2)]
         [cos-a (cos angle)]
         [sin-a (sin angle)])
        (vector (+ (* x cos-a) (* z sin-a))
                (vector-ref point 1)
                (+ (* (- x) sin-a) (* z cos-a)))))

(define (rotate-z point angle)
  (doc 'type '(-> Vec Number Vec))
  (doc 'description "Rotate point around Z axis")
  (let* ([x (vector-ref point 0)]
         [y (vector-ref point 1)]
         [cos-a (cos angle)]
         [sin-a (sin angle)])
        (vector (- (* x cos-a) (* y sin-a))
                (+ (* x sin-a) (* y cos-a))
                (vector-ref point 2))))

(define (rotate-point point angle-x angle-y angle-z)
  (doc 'export #t)
  (doc 'type '(-> Vec Number Number Number Vec))
  (doc 'description "Apply X, Y, Z rotations in sequence")
  (rotate-z (rotate-y (rotate-x point angle-x) angle-y) angle-z))

;;; ============================================================
;;; Section: Projection and Depth Buffer
;;; ============================================================

(doc 'section 'projection)

(define (project-orthographic point scale)
  (doc 'type '(-> Vec Number (Values Number Number Number)))
  (doc 'description "Orthographic projection: returns (x, y, depth)")
  (values (* scale (vector-ref point 0))
          (* scale (vector-ref point 1))
          (vector-ref point 2)))

(define (project-perspective point fov distance)
  (doc 'type '(-> Vec Number Number (Values Number Number Number)))
  (doc 'description "Perspective projection with camera at (0, 0, -distance)")
  (let* ([z (+ (vector-ref point 2) distance)]
         [factor (if (> z 0.1) (/ fov z) fov)])
        (values (* factor (vector-ref point 0))
                (* factor (vector-ref point 1))
                (vector-ref point 2))))

;;; ============================================================
;;; Section: Frame Buffer
;;; ============================================================

(doc 'section 'frame-buffer)

(define (make-frame-buffer width height)
  (doc 'type '(-> Nat Nat FrameBuffer))
  (doc 'description "Create empty frame buffer with char and depth channels")
  (list 'frame-buffer
        width height
        (make-vector (* width height) #\space)  ; char buffer
        (make-vector (* width height) -inf.0))) ; depth buffer (z, closer = higher)

(define (fb-width fb) (list-ref fb 1))
(define (fb-height fb) (list-ref fb 2))
(define (fb-chars fb) (list-ref fb 3))
(define (fb-depths fb) (list-ref fb 4))

(define (fb-index fb x y)
  (+ x (* y (fb-width fb))))

(define (fb-set! fb x y char depth)
  (doc 'type '(-> FrameBuffer Nat Nat Char Number Void))
  (doc 'description "Set pixel if depth is closer (higher z = closer to camera)")
  (let* ([w (fb-width fb)]
         [h (fb-height fb)]
         [xi (inexact->exact (round x))]
         [yi (inexact->exact (round y))])
        (when (and (>= xi 0) (< xi w) (>= yi 0) (< yi h))
              (let ([idx (fb-index fb xi yi)]
                    [depths (fb-depths fb)]
                    [chars (fb-chars fb)])
                   (when (> depth (vector-ref depths idx))
                         (vector-set! depths idx depth)
                         (vector-set! chars idx char))))))

(define (fb-set-with-intensity! fb x y depth intensity)
  (doc 'type '(-> FrameBuffer Number Number Number Number Void))
  (doc 'description "Set pixel with intensity-based character")
  (fb-set! fb x y (intensity->attractor-char intensity) depth))

(define (fb-to-string fb)
  (doc 'type '(-> FrameBuffer String))
  (doc 'description "Convert frame buffer to string")
  (let ([w (fb-width fb)]
        [h (fb-height fb)]
        [chars (fb-chars fb)]
        [result '()])
       (do ([y 0 (+ y 1)])
           ((>= y h) (apply string-append (reverse result)))
           (do ([x 0 (+ x 1)])
               ((>= x w))
               (set! result (cons (string (vector-ref chars (fb-index fb x y)))
                                  result)))
           (set! result (cons "\n" result)))))

;;; ============================================================
;;; Section: Attractor Rendering
;;; ============================================================

(doc 'section 'attractor-rendering)

(define (render-attractor trajectory width height scale angle-x angle-y angle-z)
  (doc 'export #t)
  (doc 'type '(-> (List Vec) Nat Nat Number Number Number Number String))
  (doc 'description "Render attractor trajectory to ASCII string")
  (let* ([fb (make-frame-buffer width height)]
         [half-w (/ width 2)]
         [half-h (/ height 2)]
         ;; Normalize trajectory to unit cube
         [bounds (attractor-bounds trajectory)]
         [norm-traj (normalize-trajectory trajectory bounds 1.0)]
         ;; Find z range for depth-based intensity
         [z-min (fold-left (lambda (m pt) (min m (vector-ref pt 2))) +inf.0 norm-traj)]
         [z-max (fold-left (lambda (m pt) (max m (vector-ref pt 2))) -inf.0 norm-traj)]
         [z-range (max 0.001 (- z-max z-min))])
        ;; Render each point
        (for-each
         (lambda (pt)
                 (let* ([rotated (rotate-point pt angle-x angle-y angle-z)]
                        [proj-x (* scale (vector-ref rotated 0))]
                        [proj-y (* scale (vector-ref rotated 1))]
                        [depth (vector-ref rotated 2)]
                        ;; Screen coordinates (centered)
                        [sx (+ half-w proj-x)]
                        [sy (- half-h proj-y)]  ; flip y
                        ;; Intensity based on depth (closer = brighter)
                        [intensity (/ (- depth z-min) z-range)])
                       (fb-set-with-intensity! fb sx sy depth intensity)))
         norm-traj)
        (fb-to-string fb)))

(define (render-attractor-colored trajectory width height scale angle-x angle-y angle-z)
  (doc 'export #t)
  (doc 'type '(-> (List Vec) Nat Nat Number Number Number Number String))
  (doc 'description "Render attractor with ANSI 256-color based on trajectory position")
  (let* ([fb-chars (make-vector (* width height) #\space)]
         [fb-colors (make-vector (* width height) 0)]
         [fb-depths (make-vector (* width height) -inf.0)]
         [half-w (/ width 2)]
         [half-h (/ height 2)]
         [bounds (attractor-bounds trajectory)]
         [norm-traj (normalize-trajectory trajectory bounds 1.0)]
         [n-points (length norm-traj)]
         [z-min (fold-left (lambda (m pt) (min m (vector-ref pt 2))) +inf.0 norm-traj)]
         [z-max (fold-left (lambda (m pt) (max m (vector-ref pt 2))) -inf.0 norm-traj)]
         [z-range (max 0.001 (- z-max z-min))])
        ;; Render each point with color based on position in trajectory
        (let render-loop ([pts norm-traj] [i 0])
             (when (pair? pts)
                   (let* ([pt (car pts)]
                          [rotated (rotate-point pt angle-x angle-y angle-z)]
                          [proj-x (* scale (vector-ref rotated 0))]
                          [proj-y (* scale (vector-ref rotated 1))]
                          [depth (vector-ref rotated 2)]
                          [sx (inexact->exact (round (+ half-w proj-x)))]
                          [sy (inexact->exact (round (- half-h proj-y)))]
                          [intensity (/ (- depth z-min) z-range)]
                          [hue (/ i n-points)]
                          [r (* 0.5 (+ 1 (sin (* 2 3.14159 hue))))]
                          [g (* 0.5 (+ 1 (sin (* 2 3.14159 (+ hue 0.333)))))]
                          [b (* 0.5 (+ 1 (sin (* 2 3.14159 (+ hue 0.666)))))]
                          [r-final (* r (+ 0.3 (* 0.7 intensity)))]
                          [g-final (* g (+ 0.3 (* 0.7 intensity)))]
                          [b-final (* b (+ 0.3 (* 0.7 intensity)))]
                          [ri (min 5 (max 0 (inexact->exact (round (* r-final 5)))))]
                          [gi (min 5 (max 0 (inexact->exact (round (* g-final 5)))))]
                          [bi (min 5 (max 0 (inexact->exact (round (* b-final 5)))))]
                          [color-code (+ 16 (* ri 36) (* gi 6) bi)])
                         (when (and (>= sx 0) (< sx width) (>= sy 0) (< sy height))
                               (let ([idx (+ sx (* sy width))])
                                    (when (> depth (vector-ref fb-depths idx))
                                          (vector-set! fb-depths idx depth)
                                          (vector-set! fb-chars idx (intensity->attractor-char intensity))
                                          (vector-set! fb-colors idx color-code)))))
                   (render-loop (cdr pts) (+ i 1))))
        ;; Convert to colored string using simple loops
        (let ([result '()])
             (let row-loop ([y 0])
                  (if (>= y height)
                      (string-append (apply string-append (reverse result)) "\x1b;[0m")
                      (begin
                        (let col-loop ([x 0])
                             (when (< x width)
                                   (let* ([idx (+ x (* y width))]
                                          [ch (vector-ref fb-chars idx)]
                                          [color (vector-ref fb-colors idx)])
                                         (if (char=? ch #\space)
                                             (set! result (cons " " result))
                                             (set! result (cons (string-append
                                                                 "\x1b;[38;5;"
                                                                 (number->string color)
                                                                 "m"
                                                                 (string ch))
                                                                result))))
                                   (col-loop (+ x 1))))
                        (set! result (cons "\n" result))
                        (row-loop (+ y 1))))))))

;;; ============================================================
;;; Section: Animation
;;; ============================================================

(doc 'section 'animation)

(define (render-spinning-attractor trajectory width height scale n-frames)
  (doc 'export #t)
  (doc 'type '(-> (List Vec) Nat Nat Number Nat (List String)))
  (doc 'description "Render N frames of spinning attractor (monochrome)")
  (let ([pi 3.14159265358979])
       (map (lambda (i)
                    (let* ([angle (* 2 pi (/ i n-frames))]
                           [tilt 0.3])  ; slight tilt for better 3D effect
                          (render-attractor trajectory width height scale
                                            tilt angle 0)))
            (iota n-frames))))

(define (render-spinning-attractor-colored trajectory width height scale n-frames)
  (doc 'export #t)
  (doc 'type '(-> (List Vec) Nat Nat Number Nat (List String)))
  (doc 'description "Render N frames of spinning attractor (colored)")
  (let ([pi 3.14159265358979])
       (map (lambda (i)
                    (let* ([angle (* 2 pi (/ i n-frames))]
                           [tilt 0.3])
                          (render-attractor-colored trajectory width height scale
                                                    tilt angle 0)))
            (iota n-frames))))

(define (play-animation frames delay-ms)
  (doc 'export #t)
  (doc 'type '(-> (List String) Number Void))
  (doc 'description "Play animation in terminal with delay between frames")
  (for-each (lambda (frame)
                    (display "\x1b;[2J\x1b;[H")  ; clear screen, home cursor
                    (display frame)
                    (flush-output-port (current-output-port))
                    ;; Busy-wait delay (no sleep in R6RS)
                    (let ([start (current-time)])
                         (let loop ()
                              (when (< (- (current-time) start) (/ delay-ms 1000.0))
                                    (loop)))))
            frames))

(define (loop-animation frames delay-ms n-loops)
  (doc 'export #t)
  (doc 'type '(-> (List String) Number Nat Void))
  (doc 'description "Loop animation n times")
  (do ([i 0 (+ i 1)])
      ((>= i n-loops))
      (play-animation frames delay-ms)))

;;; ============================================================
;;; Section: Quick Demos
;;; ============================================================

(doc 'section 'demos)

(define (demo-lorenz width height n-points)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat String))
  (doc 'description "Generate single frame of Lorenz attractor")
  (let* ([traj (generate-attractor lorenz-classic
                                   (vector 1.0 1.0 1.0)
                                   0.005 n-points 1000)]
         [scale (* (min width height) 0.35)])
        (render-attractor traj width height scale 0.3 0.5 0)))

(define (demo-lorenz-colored width height n-points)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat String))
  (doc 'description "Generate colored single frame of Lorenz attractor")
  (let* ([traj (generate-attractor lorenz-classic
                                   (vector 1.0 1.0 1.0)
                                   0.005 n-points 1000)]
         [scale (* (min width height) 0.35)])
        (render-attractor-colored traj width height scale 0.3 0.5 0)))

(define (demo-rossler width height n-points)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat String))
  (doc 'description "Generate single frame of Rössler attractor")
  (let* ([traj (generate-attractor rossler-classic
                                   (vector 1.0 1.0 1.0)
                                   0.02 n-points 500)]
         [scale (* (min width height) 0.4)])
        (render-attractor-colored traj width height scale 0.3 0.7 0)))

(define (demo-thomas width height n-points)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat String))
  (doc 'description "Generate single frame of Thomas attractor")
  (let* ([traj (generate-attractor thomas-classic
                                   (vector 1.0 0.0 0.0)
                                   0.05 n-points 200)]
         [scale (* (min width height) 0.25)])
        (render-attractor-colored traj width height scale 0.3 0.5 0)))

(define (spinning-lorenz-demo width height n-points n-frames)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat Nat (List String)))
  (doc 'description "Generate spinning Lorenz attractor animation frames")
  (let* ([traj (generate-attractor lorenz-classic
                                   (vector 1.0 1.0 1.0)
                                   0.005 n-points 1000)]
         [scale (* (min width height) 0.35)])
        (render-spinning-attractor-colored traj width height scale n-frames)))
