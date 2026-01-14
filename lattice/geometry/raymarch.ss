;;; core/geometry/raymarch.ss — SDF Raymarching with BVH Acceleration
;;;
;;; Provides sphere tracing / raymarching algorithms for rendering
;;; signed distance fields (SDFs), including mesh SDFs with BVH acceleration.
;;;
;;; Raymarching is a rendering technique that:
;;; 1. Steps along a ray from the camera
;;; 2. At each step, queries the SDF to get distance to nearest surface
;;; 3. Advances by that distance (guaranteed not to intersect)
;;; 4. Repeats until surface hit or max distance reached
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - geometry.ss
;;;   - mesh-sdf.ss

(load "core/base/prelude.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/geometry/mesh-sdf.ss")

;;; ====
;;; Raymarching Parameters
;;; ====

;;; raymarch-params : Configuration for raymarching
;;; (raymarch-params max-steps max-distance hit-threshold)

;;; raymarch-params : Nat × Real × Real → RaymarchParams
(define (raymarch-params max-steps max-distance hit-threshold)
  (list 'raymarch-params max-steps max-distance hit-threshold))

;;; raymarch-params-max-steps : RaymarchParams → Nat
(define (raymarch-params-max-steps p) (cadr p))
;;; raymarch-params-max-distance : RaymarchParams → Real
(define (raymarch-params-max-distance p) (caddr p))
;;; raymarch-params-hit-threshold : RaymarchParams → Real
(define (raymarch-params-hit-threshold p) (cadddr p))

;;; Default parameters
;;; default-raymarch-params : RaymarchParams
(define default-raymarch-params
  (raymarch-params 100 1000.0 0.001))

;;; ====
;;; Raymarching Algorithm
;;; ====

;;; raymarch : (Vec3 → Real) × Ray3 × RaymarchParams → (Vec3 × Real × Nat) | #f
;;; Sphere trace along a ray until surface hit or max distance
;;; SDF-Function is (Point3 → Number)
;;; Returns (hit-point distance num-steps) or #f if no hit
(define (raymarch sdf-fn ray params)
  (let ([max-steps (raymarch-params-max-steps params)]
        [max-dist (raymarch-params-max-distance params)]
        [threshold (raymarch-params-hit-threshold params)]
        [origin (ray3-origin ray)]
        [direction (ray3-direction ray)])
       (define (march t steps)
         (cond
          ;; Hit max steps
          [(>= steps max-steps) #f]
          
          ;; Gone too far
          [(>= t max-dist) #f]
          
          ;; Keep marching
          [else
           (let* ([point (ray3-point-at ray t)]
                  [dist (sdf-fn point)])
                 (cond
                  ;; Hit surface
                  [(< dist threshold)
                   (list point t steps)]
                  
                  ;; Keep going
                  [else
                   (march (+ t (abs dist)) (+ steps 1))]))]))
       (march 0.0 0)))

;;; raymarch-mesh : Mesh × Ray3 × RaymarchParams → (Vec3 × Real × Nat) | #f
;;; Specialized raymarcher for mesh SDFs (uses BVH acceleration)
(define (raymarch-mesh mesh ray params)
  (let ([sdf-fn (lambda (p) (mesh-sdf mesh p))])
       (raymarch sdf-fn ray params)))

;;; ====
;;; Normal Computation
;;; ====

;;; sdf-normal : (Vec3 → Real) × Vec3 → Vec3
;;; Compute surface normal using gradient of SDF
;;; Uses central differences for better accuracy
(define (sdf-normal sdf-fn point)
  (let* ([eps 0.001]
         [dx (- (sdf-fn (vec3-add point (vec3 eps 0 0)))
                (sdf-fn (vec3-sub point (vec3 eps 0 0))))]
         [dy (- (sdf-fn (vec3-add point (vec3 0 eps 0)))
                (sdf-fn (vec3-sub point (vec3 0 eps 0))))]
         [dz (- (sdf-fn (vec3-add point (vec3 0 0 eps)))
                (sdf-fn (vec3-sub point (vec3 0 0 eps))))])
        (vec3-normalize (vec3 dx dy dz))))

;;; mesh-sdf-normal : Mesh × Vec3 → Vec3
;;; Compute surface normal for mesh SDF
(define (mesh-sdf-normal mesh point)
  (mesh-sdf-gradient mesh point))

;;; ====
;;; Soft Shadows
;;; ====

;;; raymarch-shadow : (Vec3 → Real) × Ray3 × Real × Real × RaymarchParams → Real
;;; Compute soft shadow factor (0 = full shadow, 1 = no shadow)
;;; light-distance: distance to light source
;;; softness: higher = softer shadows (typically 4-32)
(define (raymarch-shadow sdf-fn ray light-distance softness params)
  (let ([max-steps (raymarch-params-max-steps params)]
        [threshold (raymarch-params-hit-threshold params)]
        [direction (ray3-direction ray)])
       (define (march t steps shadow-factor)
         (cond
          ;; Hit max steps or reached light
          [(or (>= steps max-steps) (>= t light-distance))
           shadow-factor]
          
          ;; Check SDF
          [else
           (let* ([point (ray3-point-at ray t)]
                  [dist (sdf-fn point)])
                 (cond
                  ;; Hit surface - full shadow
                  [(< dist threshold) 0.0]
                  
                  ;; Update shadow factor and keep going
                  [else
                   (let ([new-factor (min shadow-factor (* softness (/ dist t)))])
                        (march (+ t dist) (+ steps 1) new-factor))]))]))
       (march (raymarch-params-hit-threshold params) 0 1.0)))

;;; ====
;;; Ambient Occlusion
;;; ====

;;; raymarch-ao : (Vec3 → Real) × Vec3 × Vec3 × Nat → Real
;;; Compute ambient occlusion factor (0 = fully occluded, 1 = no occlusion)
;;; Uses multiple samples along normal direction
;;; num-samples: typically 5
(define (raymarch-ao sdf-fn point normal num-samples)
  (define (sample-ao i total sum)
    (if (>= i num-samples)
        (/ sum num-samples)
        (let* ([step-size (* (+ i 1) 0.1)]
               [sample-point (vec3-add point (vec3-scale normal step-size))]
               [dist (sdf-fn sample-point)]
               [occlusion (- step-size dist)]
               [weight (/ 1.0 (expt 2.0 i))])
              (sample-ao (+ i 1)
                         total
                         (+ sum (* weight (max 0.0 occlusion)))))))
  (max 0.0 (- 1.0 (sample-ao 0 0 0.0))))

;;; ====
;;; Scene Rendering Helpers
;;; ====

;;; simple-shading : Vec3 × Vec3 × Vec3 → Real
;;; Simple diffuse shading
;;; normal: surface normal
;;; light-dir: direction to light (normalized)
;;; view-dir: direction to camera (normalized)
(define (simple-shading normal light-dir view-dir)
  (let ([diffuse (max 0.0 (vec3-dot normal light-dir))]
        [ambient 0.2])
       (+ ambient (* 0.8 diffuse))))

;;; render-pixel : (Vec3 → Real) × Ray3 × Vec3 × RaymarchParams → Real
;;; Render a single pixel (returns grayscale value 0-1)
;;; light-pos: position of light source
(define (render-pixel sdf-fn ray light-pos params)
  (let ([result (raymarch sdf-fn ray params)])
       (if result
           (let* ([hit-point (car result)]
                  [normal (sdf-normal sdf-fn hit-point)]
                  [to-light (vec3-normalize (vec3-sub light-pos hit-point))]
                  [to-camera (vec3-normalize (vec3-scale (ray3-direction ray) -1.0))]
                  [shading (simple-shading normal to-light to-camera)])
                 shading)
           ;; No hit - background
           0.0)))

;;; ====
;;; Performance Optimization Helpers
;;; ====

;;; adaptive-step : (Vec3 → Real) × Vec3 → Real
;;; Compute adaptive step size based on local SDF variation
;;; Can skip larger steps in areas of low curvature
(define (adaptive-step sdf-fn point)
  (let* ([dist (sdf-fn point)]
         [curvature-eps 0.1]
         ;; Sample nearby points to estimate curvature
         [d-fwd (sdf-fn (vec3-add point (vec3 curvature-eps 0 0)))]
         [d-back (sdf-fn (vec3-sub point (vec3 curvature-eps 0 0)))]
         [variation (abs (- d-fwd d-back))])
        ;; In low-variation areas, can step more aggressively
        (if (< variation 0.01)
            (* dist 1.5)  ; 50% larger steps
            dist)))

;;; bvh-accelerated-raymarch : Mesh × Ray3 × RaymarchParams → (Vec3 × Real × Nat) | #f
;;; Hybrid approach: use BVH for initial ray-mesh intersection,
;;; then raymarch in vicinity of hit for accurate SDF
(define (bvh-accelerated-raymarch mesh ray params)
  (let ([ray-result (mesh-intersect-ray mesh ray)])
       (if ray-result
           (let* ([coarse-hit (car ray-result)]
                  [t-coarse (cadr ray-result)]
                  ;; Start raymarching slightly before the BVH hit
                  [start-t (max 0.0 (- t-coarse 1.0))]
                  [adjusted-ray (ray3 (ray3-point-at ray start-t)
                                      (ray3-direction ray))]
                  ;; Raymarch in local region
                  [sdf-fn (lambda (p) (mesh-sdf mesh p))]
                  [local-result (raymarch sdf-fn adjusted-ray
                                          (raymarch-params 50 10.0 0.001))])
                 (if local-result
                     (let* ([local-hit (car local-result)]
                            [local-t (cadr local-result)]
                            [local-steps (caddr local-result)]
                            [global-t (+ start-t local-t)])
                           (list local-hit global-t local-steps))
                     ;; Fallback to coarse hit
                     (list coarse-hit t-coarse 1)))
           ;; No BVH hit, try full raymarch
           (raymarch-mesh mesh ray params))))
