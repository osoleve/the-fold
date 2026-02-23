(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'geometry)
(require 'mesh-sdf)

(doc 'module 'ascii-render)
(doc 'description "ASCII art mesh renderer with BVH-accelerated ray casting and ANSI 256-color output")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'provides "Render 3D meshes to colored ASCII art for terminal display")

(doc 'section 'ascii-character-ramp)

(define ascii-ramp " .:-=+*#%@")
(doc 'export #t)
(doc ascii-ramp 'description "Characters ordered by visual density (dark to light)")
(define ascii-ramp-len (string-length ascii-ramp))

(define (intensity->char intensity)
  (doc 'export #t)
  (doc 'type '(-> Number Char))
  (doc 'description "Map intensity [0,1] to ASCII character")
  (let ([idx (min (- ascii-ramp-len 1)
                  (max 0 (inexact->exact (floor (* intensity ascii-ramp-len)))))])
       (string-ref ascii-ramp idx)))

(doc 'section 'ansi-color-codes)

(define (rgb->ansi256 r g b)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "Convert RGB [0,1] to ANSI 256-color code")
  (let ([ri (min 5 (max 0 (inexact->exact (round (* r 5)))))]
        [gi (min 5 (max 0 (inexact->exact (round (* g 5)))))]
        [bi (min 5 (max 0 (inexact->exact (round (* b 5)))))])
       (+ 16 (* ri 36) (* gi 6) bi)))

(define (ansi-fg color-code)
  (doc 'export #t)
  (doc 'type '(-> Number String))
  (doc 'description "Generate ANSI escape for 256-color foreground")
  (string-append "\x1b;[38;5;" (number->string color-code) "m"))

(define ansi-reset "\x1b;[0m")
(doc 'export #t)
(doc ansi-reset 'description "ANSI escape sequence to reset terminal formatting")

(doc 'section 'camera-and-ray-generation)

(define (make-camera pos look-at up fov aspect)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Vec3 Vec3 Number Number Camera))
  (doc 'description "Create camera with position, look-at point, up vector, FOV, and aspect ratio")
  (let* ([forward (vec3-normalize (vec3-sub look-at pos))]
         [right (vec3-normalize (vec3-cross forward up))]
         [cam-up (vec3-cross right forward)]
         [half-height (tan (* fov 0.5))]
         [half-width (* half-height aspect)])
        (list 'camera pos forward right cam-up half-width half-height)))

(define (camera-pos cam)
  (doc 'export #t)
  (doc 'type '(-> Camera Vec3))
  (list-ref cam 1))

(define (camera-forward cam)
  (doc 'export #t)
  (doc 'type '(-> Camera Vec3))
  (list-ref cam 2))

(define (camera-right cam)
  (doc 'export #t)
  (doc 'type '(-> Camera Vec3))
  (list-ref cam 3))

(define (camera-up cam)
  (doc 'export #t)
  (doc 'type '(-> Camera Vec3))
  (list-ref cam 4))

(define (camera-half-width cam)
  (doc 'export #t)
  (doc 'type '(-> Camera Real))
  (list-ref cam 5))

(define (camera-half-height cam)
  (doc 'export #t)
  (doc 'type '(-> Camera Real))
  (list-ref cam 6))

(define (camera-ray cam u v)
  (doc 'export #t)
  (doc 'type '(-> Camera Number Number Ray3))
  (doc 'description "Generate ray for normalized screen coords u,v in [-1,1]")
  (let* ([pos (camera-pos cam)]
         [dir (vec3-normalize
               (vec3-add
                (vec3-add
                 (camera-forward cam)
                 (vec3-scale (camera-right cam) (* u (camera-half-width cam))))
                (vec3-scale (camera-up cam) (* v (camera-half-height cam)))))])
        (ray3 pos dir)))

(doc 'section 'mesh-rendering)

(define (render-pixel-color mesh ray light-dir)
  (doc 'export #t)
  (doc 'type '(-> Mesh Ray3 Vec3 (Maybe (List Number Number Number))))
  (doc 'description "Render a single ray, return RGB [0,1] or #f for background")
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

(doc 'section 'frame-rendering)

(define (render-frame mesh cam light-dir width height)
  (doc 'export #t)
  (doc 'type '(-> Mesh Camera Vec3 Number Number String))
  (doc 'description "Render mesh to colored ASCII string; width/height in characters")
  (let render-rows ([y 0] [result '()])
       (if (>= y height)
           (apply string-append (reverse result))
           (let render-cols ([x 0] [result result])
                (if (>= x width)
                    (render-rows (+ y 1)
                                 (cons (string-append ansi-reset "\n") result))
                    (let* ([u (- (* 2.0 (/ x width)) 1.0)]
                           [v (- 1.0 (* 2.0 (/ y height)))]  ; Flip Y
                           [ray (camera-ray cam u v)]
                           [color (render-pixel-color mesh ray light-dir)])
                          (render-cols (+ x 1)
                                       (if color
                                           (let* ([r (car color)]
                                                  [g (cadr color)]
                                                  [b (caddr color)]
                                                  [intensity (/ (+ r g b) 3.0)]
                                                  [ch (intensity->char intensity)]
                                                  [ansi-code (rgb->ansi256 r g b)])
                                                 (cons (string-append
                                                        (ansi-fg ansi-code)
                                                        (string ch))
                                                       result))
                                           (cons " " result)))))))))

(doc 'section 'animation)

(define (rotate-camera-around center angle distance)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Number Number Vec3))
  (doc 'description "Rotate camera position around Y axis at given distance")
  (let ([x (* distance (cos angle))]
        [z (* distance (sin angle))])
       (vec3-add center (vec3 x 0 z))))

(define (render-spinning-frames mesh width height num-frames distance)
  (doc 'export #t)
  (doc 'type '(-> Mesh Number Number Number Number (List String)))
  (doc 'description "Render N frames of mesh spinning")
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

