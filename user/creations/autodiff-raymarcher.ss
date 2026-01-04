;;; user/creations/autodiff-raymarcher.ss --- Ray Marching with TRUE Autodiff
;;;
;;; THE ULTIMATE NERD-SNIPE:
;;; - Signed Distance Fields define 3D geometry
;;; - Dual numbers from core/autodiff compute EXACT gradients
;;; - Gradients give surface normals FOR FREE
;;; - Ray marching renders the scene in ASCII
;;;
;;; Unlike numerical differentiation, this uses symbolic derivatives
;;; propagated through the computational graph via dual numbers.
;;;
;;; This is the real deal: analytic derivatives via autodiff.

(load "core/autodiff/comp-graph.ss")  ; Dual numbers and autodiff
(load "user/creations/ascii-video.ss")

;;; ============================================================
;;; Dual Number Vector3 Operations
;;; ============================================================
;;;
;;; A DualVec3 is a vector of 3 dual numbers, representing both
;;; a position AND its partial derivatives w.r.t. some variable.

(define (dual-vec3 dx dy dz)
  "Create a vector of dual numbers"
  (vector dx dy dz))

(define (dv3-x v) (vector-ref v 0))
(define (dv3-y v) (vector-ref v 1))
(define (dv3-z v) (vector-ref v 2))

;;; Create a dual vec3 where component i is the variable (deriv=1)
(define (dual-vec3-variable x y z component)
  "Create dual vec3 where component has derivative 1"
  (dual-vec3
   (if (= component 0) (dual-variable x) (dual-lift x))
   (if (= component 1) (dual-variable y) (dual-lift y))
   (if (= component 2) (dual-variable z) (dual-lift z))))

;;; Create a constant dual vec3 (all derivs = 0)
(define (dual-vec3-const x y z)
  (dual-vec3 (dual-lift x) (dual-lift y) (dual-lift z)))

;;; Vector operations lifted to dual numbers
(define (dv3-add a b)
  (dual-vec3 (dual-add (dv3-x a) (dv3-x b))
             (dual-add (dv3-y a) (dv3-y b))
             (dual-add (dv3-z a) (dv3-z b))))

(define (dv3-sub a b)
  (dual-vec3 (dual-sub (dv3-x a) (dv3-x b))
             (dual-sub (dv3-y a) (dv3-y b))
             (dual-sub (dv3-z a) (dv3-z b))))

(define (dv3-scale v s)
  (let ([sd (if (dual? s) s (dual-lift s))])
       (dual-vec3 (dual-mul (dv3-x v) sd)
                  (dual-mul (dv3-y v) sd)
                  (dual-mul (dv3-z v) sd))))

(define (dv3-dot a b)
  (dual-add (dual-mul (dv3-x a) (dv3-x b))
            (dual-add (dual-mul (dv3-y a) (dv3-y b))
                      (dual-mul (dv3-z a) (dv3-z b)))))

(define (dv3-length v)
  (dual-sqrt (dv3-dot v v)))

(define (dv3-normalize v)
  (let ([len (dv3-length v)])
       (dv3-scale v (dual-recip len))))

;;; Additional Dual Operations
(define (dual-abs a)
  (let ([a (dual-lift a)])
       (dual (abs (dual-value a))
             (let ([v (dual-value a)])
                  (cond [(> v 0) (dual-deriv a)]
                        [(< v 0) (- (dual-deriv a))]
                        [else 0])))))

(define (dual-min a b)
  (let ([a (dual-lift a)]
        [b (dual-lift b)])
       (if (< (dual-value a) (dual-value b)) a b)))

(define (dual-max a b)
  (let ([a (dual-lift a)]
        [b (dual-lift b)])
       (if (> (dual-value a) (dual-value b)) a b)))

;;; Extract just the values from a dual vec3
(define (dv3-values v)
  (vector (dual-value (dv3-x v))
          (dual-value (dv3-y v))
          (dual-value (dv3-z v))))

;;; ============================================================
;;; Plain Vector3 for Camera/Lighting (No Autodiff Needed)
;;; ============================================================

(define (vec3 x y z) (vector x y z))
(define (v3-x v) (vector-ref v 0))
(define (v3-y v) (vector-ref v 1))
(define (v3-z v) (vector-ref v 2))

(define (v3-add a b)
  (vec3 (+ (v3-x a) (v3-x b))
        (+ (v3-y a) (v3-y b))
        (+ (v3-z a) (v3-z b))))

(define (v3-sub a b)
  (vec3 (- (v3-x a) (v3-x b))
        (- (v3-y a) (v3-y b))
        (- (v3-z a) (v3-z b))))

(define (v3-scale v s)
  (vec3 (* (v3-x v) s) (* (v3-y v) s) (* (v3-z v) s)))

(define (v3-dot a b)
  (+ (* (v3-x a) (v3-x b))
     (* (v3-y a) (v3-y b))
     (* (v3-z a) (v3-z b))))

(define (v3-length v)
  (sqrt (v3-dot v v)))

(define (v3-normalize v)
  (let ([len (v3-length v)])
       (if (< len 0.0001) (vec3 0 1 0)
           (v3-scale v (/ 1.0 len)))))

;;; ============================================================
;;; Rotation (Plain Vectors)
;;; ============================================================

(define (rotate-y v angle)
  (let ([c (cos angle)] [s (sin angle)]
        [x (v3-x v)] [y (v3-y v)] [z (v3-z v)])
       (vec3 (+ (* c x) (* s z)) y (+ (* (- s) x) (* c z)))))

(define (rotate-x v angle)
  (let ([c (cos angle)] [s (sin angle)]
        [x (v3-x v)] [y (v3-y v)] [z (v3-z v)])
       (vec3 x (- (* c y) (* s z)) (+ (* s y) (* c z)))))

;;; ============================================================
;;; SDFs Using Dual Numbers (Differentiable!)
;;; ============================================================

;;; Each SDF takes a DualVec3 and returns a Dual (distance + derivative)

;;; Sphere SDF: |p - center| - radius
(define (sdf-sphere-dual p center radius)
  (let ([offset (dv3-sub p (dual-vec3-const (v3-x center) (v3-y center) (v3-z center)))])
       (dual-sub (dv3-length offset) (dual-lift radius))))

;;; Torus SDF: |vec2(|p.xz| - R, p.y)| - r
(define (sdf-torus-dual p center R r)
  (let* ([q (dv3-sub p (dual-vec3-const (v3-x center) (v3-y center) (v3-z center)))]
         [xz-sq (dual-add (dual-sq (dv3-x q)) (dual-sq (dv3-z q)))]
         [xz-len (dual-sqrt xz-sq)]
         [ring-dist (dual-sub xz-len (dual-lift R))]
         [y (dv3-y q)]
         [dist-sq (dual-add (dual-sq ring-dist) (dual-sq y))])
        (dual-sub (dual-sqrt dist-sq) (dual-lift r))))

;;; Box SDF (fully differentiable)
(define (sdf-box-dual p center size)
  (let* ([q (dv3-sub p (dual-vec3-const (v3-x center) (v3-y center) (v3-z center)))]
         [dx (dual-sub (dual-abs (dv3-x q)) (dual-lift (v3-x size)))]
         [dy (dual-sub (dual-abs (dv3-y q)) (dual-lift (v3-y size)))]
         [dz (dual-sub (dual-abs (dv3-z q)) (dual-lift (v3-z size)))]
         [mx (dual-max dx (dual-lift 0))]
         [my (dual-max dy (dual-lift 0))]
         [mz (dual-max dz (dual-lift 0))]
         [outside (dual-sqrt (dual-add (dual-sq mx) (dual-add (dual-sq my) (dual-sq mz))))]
         [inside (dual-min (dual-max dx (dual-max dy dz)) (dual-lift 0))])
        (dual-add outside inside)))

;;; ============================================================
;;; SDF Combinators (Differentiable!)
;;; ============================================================

(define (sdf-union-dual d1 d2)
  "Union: min of distances"
  (dual-min d1 d2))

(define (sdf-smooth-union-dual d1 d2 k)
  "Smooth minimum: differentiable blending"
  (let* ([kd (dual-lift k)]
         [diff (dual-sub d1 d2)]
         [h (dual-max (dual-lift 0) (dual-sub kd (dual-abs diff)))]
         [h-scaled (dual-div h kd)]
         [blend (dual-mul (dual-mul h h-scaled) (dual-mul kd (dual-lift 0.25)))])
        (dual-sub (dual-min d1 d2) blend)))

;;; ============================================================
;;; Rotation (Dual Vectors)
;;; ============================================================

(define (dv3-rotate-y v angle)
  (let ([c (dual-lift (cos angle))] [s (dual-lift (sin angle))]
        [x (dv3-x v)] [y (dv3-y v)] [z (dv3-z v)])
       (dual-vec3 (dual-add (dual-mul c x) (dual-mul s z))
                  y
                  (dual-add (dual-mul (dual-neg s) x) (dual-mul c z)))))

(define (dv3-rotate-x v angle)
  (let ([c (dual-lift (cos angle))] [s (dual-lift (sin angle))]
        [x (dv3-x v)] [y (dv3-y v)] [z (dv3-z v)])
       (dual-vec3 x
                  (dual-sub (dual-mul c y) (dual-mul s z))
                  (dual-add (dual-mul s y) (dual-mul c z)))))

;;; ============================================================
;;; THE MAGIC: Autodiff Normals
;;; ============================================================
;;;
;;; This is where the magic happens! We compute the gradient of
;;; the SDF by running it three times with different dual numbers:
;;;   - Once with dx=1: get df/dx
;;;   - Once with dy=1: get df/dy
;;;   - Once with dz=1: get df/dz
;;;
;;; The gradient IS the surface normal (normalized).
;;; By differentiating through the rotation, we get the normal
;;; in world space directly.

(define (autodiff-normal sdf-func px py pz rotation-angle)
  "Compute surface normal using TRUE automatic differentiation"
  (let* ([sdf-world
          (lambda (p-dv3)
                  (sdf-func (dv3-rotate-y (dv3-rotate-x p-dv3 (* 0.3 rotation-angle))
                                          rotation-angle)))]
         ;; Run SDF with x as the variable
         [res-x (sdf-world (dual-vec3-variable px py pz 0))]
         [grad-x (dual-deriv res-x)]
         ;; Run SDF with y as the variable
         [res-y (sdf-world (dual-vec3-variable px py pz 1))]
         [grad-y (dual-deriv res-y)]
         ;; Run SDF with z as the variable
         [res-z (sdf-world (dual-vec3-variable px py pz 2))]
         [grad-z (dual-deriv res-z)])
        ;; Normalize the gradient to get the normal
        (v3-normalize (vec3 grad-x grad-y grad-z))))

;;; ============================================================
;;; Ray Marching Engine
;;; ============================================================

(define *max-steps* 64)
(define *max-dist* 100.0)
(define *hit-threshold* 0.001)

(define (ray-march sdf-func origin direction rotation-angle)
  "March a ray through the SDF, return (distance . hit-point) or #f"
  (let loop ([t 0.0] [steps 0])
       (cond
        [(>= steps *max-steps*) #f]
        [(> t *max-dist*) #f]
        [else
         (let* ([px (+ (v3-x origin) (* t (v3-x direction)))]
                [py (+ (v3-y origin) (* t (v3-y direction)))]
                [pz (+ (v3-z origin) (* t (v3-z direction)))]
                ;; Rotate the point for animation
                [rotated (rotate-y (rotate-x (vec3 px py pz) (* 0.3 rotation-angle))
                                   rotation-angle)]
                ;; Evaluate SDF using constant dual vec3 (no gradient needed here)
                [p-dual (dual-vec3-const (v3-x rotated) (v3-y rotated) (v3-z rotated))]
                [d (dual-value (sdf-func p-dual))])
               (if (< (abs d) *hit-threshold*)
                   (cons t (vec3 px py pz))
                   (loop (+ t (max d 0.001)) (+ steps 1))))])))

;;; ============================================================
;;; Lighting
;;; ============================================================

(define *light-dir* (v3-normalize (vec3 0.5 0.8 -0.6)))
(define *ambient* 0.15)

(define (compute-lighting normal)
  "Diffuse lighting from surface normal"
  (let* ([ndotl (v3-dot normal *light-dir*)]
         [diffuse (max 0 ndotl)])
        (+ *ambient* (* (- 1.0 *ambient*) diffuse))))

;;; ASCII shading
(define *shade-chars* " .,:;+*oO#@")

(define (brightness->char b)
  (let* ([n (string-length *shade-chars*)]
         [idx (min (- n 1) (max 0 (inexact->exact (floor (* b n)))))])
        (string-ref *shade-chars* idx)))

;;; ============================================================
;;; Scene: Spinning Torus with True Autodiff
;;; ============================================================

(define (make-autodiff-torus-scene)
  "Returns an SDF function that takes a DualVec3"
  (lambda (p)
          (sdf-torus-dual p (vec3 0 0 0) 0.6 0.25)))

(define (make-autodiff-blobs-scene t)
  "Morphing metaballs with smooth union"
  (let* ([phase (* t 0.2)]
         [offset (* 0.4 (+ 1.0 (sin phase)))]
         [k 0.3])
        (lambda (p)
                (sdf-smooth-union-dual
                 (sdf-sphere-dual p (vec3 (- offset) 0 0) 0.4)
                 (sdf-sphere-dual p (vec3 offset 0 0) 0.4)
                 k))))

;;; ============================================================
;;; Rendering with True Autodiff
;;; ============================================================

(define (render-autodiff-frame sdf-func angle width height camera-pos fov)
  "Render using true autodiff for normals"
  (let ([frame (make-frame width height #\space)]
        [aspect (/ width height 2.0)])
       (do ([row 0 (+ row 1)])
           ((>= row height))
           (let ([v (- 1.0 (* 2.0 (/ row (- height 1))))])
                (do ([col 0 (+ col 1)])
                    ((>= col width))
                    (let* ([u (- (* 2.0 (/ col (- width 1))) 1.0)]
                           ;; Camera ray
                           [fov-scale (tan (/ fov 2.0))]
                           [ray-dir (v3-normalize (vec3 (* u fov-scale aspect)
                                                        (* v fov-scale)
                                                        1.0))]
                           [hit (ray-march sdf-func camera-pos ray-dir angle)])
                          (when hit
                                (let* ([hit-point (cdr hit)]
                                       ;; HERE'S THE AUTODIFF MAGIC!
                                       [normal (autodiff-normal sdf-func
                                                                (v3-x hit-point)
                                                                (v3-y hit-point)
                                                                (v3-z hit-point)
                                                                angle)]
                                       [brightness (compute-lighting normal)]
                                       [char (brightness->char brightness)])
                                      (frame-set! frame col row char)))))))
       frame))

;;; ============================================================
;;; Animation
;;; ============================================================

(define (render-autodiff-animation sdf-func num-frames width height)
  "Record spinning animation with true autodiff normals"
  (let ([video (make-video)]
        [camera-pos (vec3 0 0 -2.5)]
        [fov 1.2])
       (do ([i 0 (+ i 1)])
           ((>= i num-frames))
           (let* ([angle (* i (/ 6.28318 num-frames))]
                  [frame (render-autodiff-frame sdf-func angle width height camera-pos fov)])
                 (video-add-frame! video frame)))
       video))

;;; ============================================================
;;; Discord Integration
;;; ============================================================

(define (discord-autodiff-torus angle)
  "Single torus frame wrapped for Discord"
  (let* ([sdf (make-autodiff-torus-scene)]
         [frame (render-autodiff-frame sdf angle 50 20 (vec3 0 0 -2.5) 1.2)])
        (string-append "```\n" (frame-render-to-string frame) "```")))

(define (discord-autodiff-blobs t)
  "Morphing blobs with true autodiff"
  (let* ([sdf (make-autodiff-blobs-scene t)]
         [frame (render-autodiff-frame sdf 0.0 50 20 (vec3 0 0 -2.5) 1.2)])
        (string-append "```\n" (frame-render-to-string frame) "```")))

;;; ============================================================
;;; Demo
;;; ============================================================

(define (run-autodiff-demo)
  (display "\n")
  (display "============================================================\n")
  (display "  RAY MARCHING WITH TRUE AUTOMATIC DIFFERENTIATION\n")
  (display "============================================================\n")
  (display "  - SDFs written using dual number arithmetic\n")
  (display "  - Surface normals computed via EXACT gradients\n")
  (display "  - No finite differences - pure symbolic derivatives\n")
  (display "  - All in Chez Scheme, rendered in ASCII\n")
  (display "============================================================\n\n")
  
  (display "Rendering torus with TRUE autodiff normals...\n\n")
  
  (let ([sdf (make-autodiff-torus-scene)])
       ;; Show three frames
       (display "Frame 0 (angle = 0):\n")
       (let ([frame (render-autodiff-frame sdf 0.0 50 20 (vec3 0 0 -2.5) 1.2)])
            (frame-render frame))
       
       (display "\nFrame 1 (angle = pi/2):\n")
       (let ([frame (render-autodiff-frame sdf 1.57 50 20 (vec3 0 0 -2.5) 1.2)])
            (frame-render frame))
       
       (display "\nFrame 2 (angle = pi):\n")
       (let ([frame (render-autodiff-frame sdf 3.14 50 20 (vec3 0 0 -2.5) 1.2)])
            (frame-render frame)))
  
  (display "\n============================================================\n")
  (display "The magic: dual-number arithmetic propagates derivatives\n")
  (display "through the SDF. The gradient IS the surface normal.\n")
  (display "\n")
  (display "Available functions:\n")
  (display "  (discord-autodiff-torus angle)  ; For Discord\n")
  (display "  (discord-autodiff-blobs t)      ; Morphing blobs\n")
  (display "============================================================\n"))

;; Run demo
(run-autodiff-demo)
