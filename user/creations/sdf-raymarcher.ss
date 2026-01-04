;;; user/creations/sdf-raymarcher.ss --- Differentiable Ray Marching in ASCII
;;;
;;; A gloriously over-engineered ASCII renderer that uses:
;;; - Signed Distance Fields for implicit 3D geometry
;;; - Automatic Differentiation for computing surface normals
;;; - Ray marching for rendering
;;; - ASCII art for output
;;;
;;; Because why wouldn't you render a spinning torus in a Scheme REPL?

(load "core/base/prelude.ss")
(load "user/creations/ascii-video.ss")

;;; ============================================================
;;; Vector3 Operations (Inline for Performance)
;;; ============================================================

(define (vec3 x y z) (vector x y z))
(define (vec3-x v) (vector-ref v 0))
(define (vec3-y v) (vector-ref v 1))
(define (vec3-z v) (vector-ref v 2))

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

(define (vec3-dot a b)
  (+ (* (vec3-x a) (vec3-x b))
     (* (vec3-y a) (vec3-y b))
     (* (vec3-z a) (vec3-z b))))

(define (vec3-length v)
  (sqrt (vec3-dot v v)))

(define (vec3-normalize v)
  (let ([len (vec3-length v)])
       (if (< len 0.0001)
           (vec3 0 1 0)
           (vec3-scale v (/ 1.0 len)))))

;;; ============================================================
;;; Rotation Matrices
;;; ============================================================

(define (rotate-y v angle)
  (let ([c (cos angle)]
        [s (sin angle)]
        [x (vec3-x v)]
        [y (vec3-y v)]
        [z (vec3-z v)])
       (vec3 (+ (* c x) (* s z))
             y
             (+ (* (- s) x) (* c z)))))

(define (rotate-x v angle)
  (let ([c (cos angle)]
        [s (sin angle)]
        [x (vec3-x v)]
        [y (vec3-y v)]
        [z (vec3-z v)])
       (vec3 x
             (- (* c y) (* s z))
             (+ (* s y) (* c z)))))

;;; ============================================================
;;; Signed Distance Field Primitives
;;; ============================================================

;;; Sphere: distance from surface
(define (sdf-sphere p center radius)
  (- (vec3-length (vec3-sub p center)) radius))

;;; Torus: the classic SDF flex
;;; Major radius R, minor radius r
;;; Distance from point to donut surface
(define (sdf-torus p center R r)
  (let* ([q (vec3-sub p center)]
         [x (vec3-x q)]
         [y (vec3-y q)]
         [z (vec3-z q)]
         ;; Project to XZ plane, measure distance from ring
         [xz-len (sqrt (+ (* x x) (* z z)))]
         [ring-dist (- xz-len R)])
        (- (sqrt (+ (* ring-dist ring-dist) (* y y))) r)))

;;; Box: axis-aligned
(define (sdf-box p center size)
  (let* ([q (vec3-sub p center)]
         [d (vec3 (- (abs (vec3-x q)) (vec3-x size))
                  (- (abs (vec3-y q)) (vec3-y size))
                  (- (abs (vec3-z q)) (vec3-z size)))]
         [outside (vec3-length (vec3 (max (vec3-x d) 0)
                                     (max (vec3-y d) 0)
                                     (max (vec3-z d) 0)))]
         [inside (min (max (vec3-x d) (max (vec3-y d) (vec3-z d))) 0)])
        (+ outside inside)))

;;; Cylinder: along Y axis
(define (sdf-cylinder p center radius height)
  (let* ([q (vec3-sub p center)]
         [d-xz (- (sqrt (+ (* (vec3-x q) (vec3-x q))
                           (* (vec3-z q) (vec3-z q))))
                  radius)]
         [d-y (- (abs (vec3-y q)) height)])
        (+ (min (max d-xz d-y) 0)
           (sqrt (+ (* (max d-xz 0) (max d-xz 0))
                    (* (max d-y 0) (max d-y 0)))))))

;;; ============================================================
;;; SDF Combinators (CSG Operations)
;;; ============================================================

;;; Union: minimum of distances
(define (sdf-union d1 d2)
  (min d1 d2))

;;; Intersection: maximum of distances
(define (sdf-intersect d1 d2)
  (max d1 d2))

;;; Subtraction: d1 minus d2
(define (sdf-subtract d1 d2)
  (max d1 (- d2)))

;;; Smooth union: blend with smoothness k
(define (sdf-smooth-union d1 d2 k)
  (let* ([h (max (- k (abs (- d1 d2))) 0)]
         [blend (/ (* h h) (* 4 k))])
        (- (min d1 d2) blend)))

;;; ============================================================
;;; Automatic Differentiation for Normals!
;;; ============================================================
;;;
;;; Here's where the magic happens. Instead of hardcoding normal
;;; formulas, we use numerical differentiation as a standin for
;;; full autodiff. The *principle* is the same: gradients give normals.
;;;
;;; For a true flex, we could wire this into core/autodiff/, but
;;; the visual effect is identical and this is more self-contained.

(define (sdf-normal sdf p)
  "Compute surface normal via gradient of SDF (the autodiff money shot)"
  (let ([eps 0.0001])
       (vec3-normalize
        (vec3 (- (sdf (vec3 (+ (vec3-x p) eps) (vec3-y p) (vec3-z p)))
                 (sdf (vec3 (- (vec3-x p) eps) (vec3-y p) (vec3-z p))))
              (- (sdf (vec3 (vec3-x p) (+ (vec3-y p) eps) (vec3-z p)))
                 (sdf (vec3 (vec3-x p) (- (vec3-y p) eps) (vec3-z p))))
              (- (sdf (vec3 (vec3-x p) (vec3-y p) (+ (vec3-z p) eps)))
                 (sdf (vec3 (vec3-x p) (vec3-y p) (- (vec3-z p) eps))))))))

;;; ============================================================
;;; Ray Marching Engine
;;; ============================================================

(define *max-ray-steps* 64)
(define *max-ray-dist* 100.0)
(define *surface-threshold* 0.001)

(define (ray-march sdf origin direction)
  "March a ray through the SDF, return (distance . hit-point) or #f"
  (let loop ([t 0.0] [steps 0])
       (cond
        [(>= steps *max-ray-steps*) #f]
        [(> t *max-ray-dist*) #f]
        [else
         (let* ([p (vec3-add origin (vec3-scale direction t))]
                [d (sdf p)])
               (if (< (abs d) *surface-threshold*)
                   (cons t p)
                   (loop (+ t (max d 0.001)) (+ steps 1))))])))

;;; ============================================================
;;; Lighting & Shading
;;; ============================================================

(define *light-dir* (vec3-normalize (vec3 0.5 0.8 -0.6)))
(define *ambient* 0.15)

(define (compute-shading sdf hit-point)
  "Compute lighting at hit point using normal from autodiff"
  (let* ([normal (sdf-normal sdf hit-point)]
         [ndotl (vec3-dot normal *light-dir*)]
         [diffuse (max 0 ndotl)]
         [brightness (+ *ambient* (* (- 1.0 *ambient*) diffuse))])
        brightness))

;;; ASCII shading ramp (dark to light)
(define *shade-chars* " .,:;+*oO#@")

(define (brightness->char b)
  "Convert brightness [0,1] to ASCII character"
  (let* ([n (string-length *shade-chars*)]
         [idx (min (- n 1) (max 0 (inexact->exact (floor (* b n)))))])
        (string-ref *shade-chars* idx)))

;;; ============================================================
;;; Camera & Rendering
;;; ============================================================

(define (make-camera position look-at up fov)
  "Create a camera looking from position toward look-at"
  (let* ([forward (vec3-normalize (vec3-sub look-at position))]
         [right (vec3-normalize (vec3 (- (vec3-z forward)) 0 (vec3-x forward)))]
         [cam-up (vec3-normalize (vec3-sub up (vec3-scale forward (vec3-dot up forward))))])
        (list position forward right cam-up fov)))

(define (camera-position cam) (list-ref cam 0))
(define (camera-forward cam) (list-ref cam 1))
(define (camera-right cam) (list-ref cam 2))
(define (camera-up cam) (list-ref cam 3))
(define (camera-fov cam) (list-ref cam 4))

(define (camera-ray cam u v aspect)
  "Generate ray direction for screen coordinates u,v in [-1,1]"
  (let* ([fov-scale (tan (/ (camera-fov cam) 2.0))]
         [right-offset (* u fov-scale aspect)]
         [up-offset (* v fov-scale)]
         [dir (vec3-add (camera-forward cam)
                        (vec3-add (vec3-scale (camera-right cam) right-offset)
                                  (vec3-scale (camera-up cam) up-offset)))])
        (vec3-normalize dir)))

;;; ============================================================
;;; Render a Frame
;;; ============================================================

(define (render-sdf-frame sdf camera width height)
  "Render the SDF scene to an ASCII frame"
  (let ([frame (make-frame width height #\space)]
        [aspect (/ width height 2.0)])  ; 2.0 because chars are ~2:1
       (do ([row 0 (+ row 1)])
           ((>= row height))
           (let ([v (- 1.0 (* 2.0 (/ row (- height 1))))])  ; v: 1 to -1
                (do ([col 0 (+ col 1)])
                    ((>= col width))
                    (let* ([u (- (* 2.0 (/ col (- width 1))) 1.0)]  ; u: -1 to 1
                           [ray-dir (camera-ray camera u v aspect)]
                           [hit (ray-march sdf (camera-position camera) ray-dir)])
                          (when hit
                                (let* ([hit-point (cdr hit)]
                                       [brightness (compute-shading sdf hit-point)]
                                       [char (brightness->char brightness)])
                                      (frame-set! frame col row char)))))))
       frame))

;;; ============================================================
;;; Scene Definitions
;;; ============================================================

(define (make-torus-scene angle)
  "A spinning torus - the classic ray marching demo"
  (lambda (p)
          (let ([rotated (rotate-y (rotate-x p (* 0.3 angle)) angle)])
               (sdf-torus rotated (vec3 0 0 0) 0.6 0.25))))

(define (make-morphing-blobs-scene t)
  "Two spheres that morph and blend"
  (let* ([phase (* t 0.15)]
         [offset (* 0.5 (+ 1.0 (sin phase)))]
         [k (+ 0.2 (* 0.3 (+ 0.5 (* 0.5 (sin (* t 0.1))))))])  ; varying smoothness
        (lambda (p)
                (sdf-smooth-union
                 (sdf-sphere p (vec3 (- offset) 0 0) 0.5)
                 (sdf-sphere p (vec3 offset 0 0) 0.5)
                 k))))

(define (make-csg-scene angle)
  "CSG demonstration: sphere minus box"
  (lambda (p)
          (let ([rotated (rotate-y (rotate-x p (* 0.5 angle)) angle)])
               (sdf-subtract
                (sdf-sphere rotated (vec3 0 0 0) 0.7)
                (sdf-box rotated (vec3 0 0 0) (vec3 0.4 0.4 0.4))))))

(define (make-atom-scene angle)
  "An atom with orbiting electrons"
  (lambda (p)
          (let* ([nucleus (sdf-sphere p (vec3 0 0 0) 0.3)]
                 ;; Three orbiting electrons
                 [e1-pos (vec3 (* 0.8 (cos angle)) 0 (* 0.8 (sin angle)))]
                 [e2-pos (vec3 0 (* 0.8 (cos (* angle 1.3))) (* 0.8 (sin (* angle 1.3))))]
                 [e3-pos (rotate-y (vec3 (* 0.8 (cos (* angle 0.7))) (* 0.8 (sin (* angle 0.7))) 0) 1.0)]
                 [e1 (sdf-sphere p e1-pos 0.12)]
                 [e2 (sdf-sphere p e2-pos 0.12)]
                 [e3 (sdf-sphere p e3-pos 0.12)])
                (sdf-union nucleus (sdf-union e1 (sdf-union e2 e3))))))

;;; ============================================================
;;; Animation System
;;; ============================================================

(define (render-animation scene-fn camera num-frames width height)
  "Record an animation of a parametric scene"
  (let ([video (make-video)])
       (do ([i 0 (+ i 1)])
           ((>= i num-frames))
           (let* ([t (* i (/ 6.28318 num-frames))]  ; 0 to 2*pi
                  [scene (scene-fn t)]
                  [frame (render-sdf-frame scene camera width height)])
                 (video-add-frame! video frame)))
       video))

;;; ============================================================
;;; Demo Cameras
;;; ============================================================

(define *default-camera*
  (make-camera (vec3 0 0 -2.5)      ; position
               (vec3 0 0 0)          ; look-at
               (vec3 0 1 0)          ; up
               1.2))                 ; fov (radians)

(define *close-camera*
  (make-camera (vec3 0 0.3 -2.0)
               (vec3 0 0 0)
               (vec3 0 1 0)
               1.0))

(define *dramatic-camera*
  (make-camera (vec3 1.5 1.0 -2.0)
               (vec3 0 0 0)
               (vec3 0 1 0)
               1.0))

;;; ============================================================
;;; Quick Renders (for Discord)
;;; ============================================================

(define (quick-torus angle)
  "Render a single torus frame at given angle"
  (let* ([scene (make-torus-scene angle)]
         [frame (render-sdf-frame scene *default-camera* 54 22)])  ; 54 fits Discord
        (frame-render-to-string frame)))

(define (quick-blobs t)
  "Render morphing blobs at time t"
  (let* ([scene (make-morphing-blobs-scene t)]
         [frame (render-sdf-frame scene *default-camera* 54 22)])
        (frame-render-to-string frame)))

(define (quick-atom angle)
  "Render atom at given angle"
  (let* ([scene (make-atom-scene angle)]
         [frame (render-sdf-frame scene *close-camera* 54 22)])
        (frame-render-to-string frame)))

(define (quick-csg angle)
  "Render CSG scene (sphere - box)"
  (let* ([scene (make-csg-scene angle)]
         [frame (render-sdf-frame scene *dramatic-camera* 54 22)])
        (frame-render-to-string frame)))

;;; For Discord: wrap in code block
(define (discord-torus angle)
  (string-append "```\n" (quick-torus angle) "```"))

(define (discord-blobs t)
  (string-append "```\n" (quick-blobs t) "```"))

(define (discord-atom angle)
  (string-append "```\n" (quick-atom angle) "```"))

(define (discord-csg angle)
  (string-append "```\n" (quick-csg angle) "```"))

;;; ============================================================
;;; Full Demo
;;; ============================================================

(define (run-sdf-demo)
  (display "\n")
  (display "====================================================\n")
  (display "  DIFFERENTIABLE RAY MARCHING IN ASCII\n")
  (display "====================================================\n")
  (display "  SDF geometry + autodiff normals + ray marching\n")
  (display "  ...because Scheme needed a 3D renderer\n")
  (display "====================================================\n\n")
  
  (display "Rendering spinning torus (24 frames)...\n\n")
  
  (let ([video (render-animation make-torus-scene
                                 *default-camera*
                                 24   ; frames
                                 60   ; width
                                 24)])  ; height
       
       (display "Flipbook preview (every 4th frame):\n\n")
       (video-flipbook video 4)
       
       (display "\n====================================================\n")
       (display "What's happening here:\n")
       (display "  1. Signed Distance Fields define implicit geometry\n")
       (display "  2. Ray marching steps through the field\n")
       (display "  3. Autodiff computes surface normals from SDF gradient\n")
       (display "  4. Normals give us lighting (diffuse shading)\n")
       (display "  5. ASCII characters map to brightness levels\n")
       (display "====================================================\n\n")
       
       (display "Available scenes:\n")
       (display "  (quick-torus angle)    ; Spinning donut\n")
       (display "  (quick-blobs t)        ; Morphing metaballs\n")
       (display "  (quick-atom angle)     ; Orbiting electrons\n")
       (display "  (quick-csg angle)      ; Boolean geometry\n")
       (display "\n")
       (display "For Discord (wrapped in code blocks):\n")
       (display "  (discord-torus 0.5)\n")
       (display "  (discord-blobs 3.14)\n")
       
       video))

;;; Auto-run demo when loaded as script
(define *sdf-video* (run-sdf-demo))

(display "\nAnimation stored in *sdf-video*\n")
(display "In terminal: (video-play *sdf-video* 100)\n")
