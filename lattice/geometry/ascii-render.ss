;;; core/geometry/ascii-render.ss — ASCII Art Mesh Renderer
;;;
;;; Renders 3D meshes to colored ASCII art using BVH-accelerated ray casting.
;;; Supports ANSI 256-color output for terminal display.
;;;
;;; This is Core code: pure rendering, assumes valid mesh input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - geometry.ss
;;;   - mesh-sdf.ss

(load "core/base/prelude.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/geometry/mesh-sdf.ss")

;;; ====
;;; ASCII Character Ramp
;;; ====

;;; Characters ordered by visual density (dark to light)
;;; ascii-ramp : String
(define ascii-ramp " .:-=+*#%@")
;;; ascii-ramp-len : Nat
(define ascii-ramp-len (string-length ascii-ramp))

;;; intensity->char : Number → Char
;;; Map intensity [0,1] to ASCII character
(define (intensity->char intensity)
  (let ([idx (min (- ascii-ramp-len 1)
                  (max 0 (inexact->exact (floor (* intensity ascii-ramp-len)))))])
       (string-ref ascii-ramp idx)))

;;; ====
;;; ANSI Color Codes
;;; ====

;;; rgb->ansi256 : Number × Number × Number → Number
;;; Convert RGB [0,1] to ANSI 256-color code
(define (rgb->ansi256 r g b)
  (let ([ri (min 5 (max 0 (inexact->exact (round (* r 5)))))]
        [gi (min 5 (max 0 (inexact->exact (round (* g 5)))))]
        [bi (min 5 (max 0 (inexact->exact (round (* b 5)))))])
       (+ 16 (* ri 36) (* gi 6) bi)))

;;; ansi-fg : Number → String
;;; Generate ANSI escape for 256-color foreground
(define (ansi-fg color-code)
  (string-append "\x1b;[38;5;" (number->string color-code) "m"))

;;; ansi-reset : String
;;; ANSI escape sequence to reset terminal formatting
(define ansi-reset "\x1b;[0m")

;;; ====
;;; Camera and Ray Generation
;;; ====

;;; make-camera : Vec3 × Vec3 × Vec3 × Number × Number → Camera
;;; Create camera with position, look-at point, up vector, and aspect ratio
(define (make-camera pos look-at up fov aspect)
  (let* ([forward (vec3-normalize (vec3-sub look-at pos))]
         [right (vec3-normalize (vec3-cross forward up))]
         [cam-up (vec3-cross right forward)]
         [half-height (tan (* fov 0.5))]
         [half-width (* half-height aspect)])
        (list 'camera pos forward right cam-up half-width half-height)))

;;; camera-pos : Camera → Vec3
(define (camera-pos cam) (list-ref cam 1))
;;; camera-forward : Camera → Vec3
(define (camera-forward cam) (list-ref cam 2))
;;; camera-right : Camera → Vec3
(define (camera-right cam) (list-ref cam 3))
;;; camera-up : Camera → Vec3
(define (camera-up cam) (list-ref cam 4))
;;; camera-half-width : Camera → Real
(define (camera-half-width cam) (list-ref cam 5))
;;; camera-half-height : Camera → Real
(define (camera-half-height cam) (list-ref cam 6))

;;; camera-ray : Camera × Number × Number → Ray3
;;; Generate ray for normalized screen coords u,v in [-1,1]
(define (camera-ray cam u v)
  (let* ([pos (camera-pos cam)]
         [dir (vec3-normalize
               (vec3-add
                (vec3-add
                 (camera-forward cam)
                 (vec3-scale (camera-right cam) (* u (camera-half-width cam))))
                (vec3-scale (camera-up cam) (* v (camera-half-height cam)))))])
        (ray3 pos dir)))

;;; ====
;;; Mesh Rendering
;;; ====

;;; render-pixel-color : Mesh × Ray3 × Vec3 → (Number Number Number) | #f
;;; Render a single ray, return RGB [0,1] or #f for background
(define (render-pixel-color mesh ray light-dir)
  (let ([hit (mesh-intersect-ray mesh ray)])
       (if hit
           (let* ([point (car hit)]
                  [triangle (caddr hit)]
                  [normal (triangle-normal triangle)]
                  ;; Basic diffuse lighting
                  [ndotl (max 0.1 (vec3-dot normal light-dir))]
                  ;; Warm yellow-orange color for duck
                  [r (* 1.0 ndotl)]
                  [g (* 0.8 ndotl)]
                  [b (* 0.2 ndotl)])
                 (list r g b))
           #f)))

;;; ====
;;; Frame Rendering
;;; ====

;;; render-frame : Mesh × Camera × Vec3 × Number × Number → String
;;; Render mesh to colored ASCII string
;;; width/height in characters
(define (render-frame mesh cam light-dir width height)
  (let ([result '()])
       (do ([y 0 (+ y 1)])
           ((>= y height) (apply string-append (reverse result)))
           (do ([x 0 (+ x 1)])
               ((>= x width))
               (let* ([u (- (* 2.0 (/ x width)) 1.0)]
                      [v (- 1.0 (* 2.0 (/ y height)))]  ; Flip Y
                      [ray (camera-ray cam u v)]
                      [color (render-pixel-color mesh ray light-dir)])
                     (if color
                         (let* ([r (car color)]
                                [g (cadr color)]
                                [b (caddr color)]
                                [intensity (/ (+ r g b) 3.0)]
                                [ch (intensity->char intensity)]
                                [ansi-code (rgb->ansi256 r g b)])
                               (set! result (cons (string-append
                                                   (ansi-fg ansi-code)
                                                   (string ch))
                                                  result)))
                         (set! result (cons " " result)))))
           (set! result (cons (string-append ansi-reset "\n") result)))))

;;; ====
;;; Animation
;;; ====

;;; rotate-camera-around : Vec3 × Number × Number → Vec3
;;; Rotate camera position around Y axis at given distance
(define (rotate-camera-around center angle distance)
  (let ([x (* distance (cos angle))]
        [z (* distance (sin angle))])
       (vec3-add center (vec3 x 0 z))))

;;; render-spinning-frames : Mesh × Number × Number × Number × Number → (List String)
;;; Render N frames of mesh spinning
(define (render-spinning-frames mesh width height num-frames distance)
  (let* ([bounds (mesh-bounds mesh)]
         [center (aabb-center bounds)]
         [extents (aabb-extents bounds)]
         [max-extent (max (vec3-x extents) (vec3-y extents) (vec3-z extents))]
         [cam-distance (if (> distance 0) distance (* max-extent 3))]
         [cam-height (* max-extent 0.5)]
         [light-dir (vec3-normalize (vec3 0.5 1.0 0.3))])
        (map (lambda (i)
                     (let* ([angle (* 2.0 3.141592653589793 (/ i num-frames))]
                            [cam-pos (vec3-add
                                      (rotate-camera-around center angle cam-distance)
                                      (vec3 0 cam-height 0))]
                            [cam (make-camera cam-pos center (vec3 0 1 0) 0.8 (/ width height))])
                           (render-frame mesh cam light-dir width height)))
             (iota num-frames))))

;;; ====
;;; Output Utilities
;;; ====

;;; frames->ansi-animation : (List String) × Number → Void
;;; Play animation in terminal with delay (ms)
(define (frames->ansi-animation frames delay-ms)
  (for-each (lambda (frame)
                    (display "\x1b;[2J\x1b;[H")  ; Clear screen, home cursor
                    (display frame)
                    (flush-output-port (current-output-port))
                    ;; Simple busy-wait delay (no sleep in R6RS)
                    (let ([start (current-time)])
                         (let loop ()
                              (when (< (- (current-time) start) (/ delay-ms 1000.0))
                                    (loop)))))
            frames))

;;; save-frame : String × String → Void
;;; Save frame to file
(define (save-frame frame filename)
  (call-with-output-file filename
                         (lambda (port)
                                 (display frame port))))
