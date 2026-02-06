(load "core/base/prelude.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/geometry/mesh-sdf.ss")

(doc 'module 'raymarch)
(doc 'description "SDF raymarching with BVH acceleration")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'provides "Sphere tracing / raymarching algorithms for rendering signed distance fields (SDFs), including mesh SDFs with BVH acceleration")
(doc 'note "Raymarching technique: 1. Steps along ray from camera, 2. Queries SDF for distance to nearest surface, 3. Advances by that distance (guaranteed not to intersect), 4. Repeats until surface hit or max distance reached")

(doc 'section 'raymarch-parameters)

(doc 'note "raymarch-params: Configuration for raymarching (max-steps max-distance hit-threshold)")

(define (raymarch-params max-steps max-distance hit-threshold)
  (doc 'export #t)
  (doc 'type '(-> Nat Real Real RaymarchParams))
  (list 'raymarch-params max-steps max-distance hit-threshold))

(define (raymarch-params-max-steps p)
  (doc 'export #t)
  (doc 'type '(-> RaymarchParams Nat))
  (cadr p))
(define (raymarch-params-max-distance p)
  (doc 'export #t)
  (doc 'type '(-> RaymarchParams Real))
  (caddr p))
(define (raymarch-params-hit-threshold p)
  (doc 'export #t)
  (doc 'type '(-> RaymarchParams Real))
  (cadddr p))

(doc default-raymarch-params 'type 'RaymarchParams)
(doc default-raymarch-params 'description "Default parameters")
(doc default-raymarch-params 'export #t)
(define default-raymarch-params
  (raymarch-params 100 1000.0 0.001))

(doc 'section 'raymarch-algorithm)

(define (raymarch sdf-fn ray params)
  (doc 'export #t)
  (doc 'type '(-> (-> Vec3 Real) Ray3 RaymarchParams (Or (List Vec3 Real Nat) #f)))
  (doc 'description "Sphere trace along a ray until surface hit or max distance")
  (doc 'param 'sdf-fn "SDF function (Point3 → Number)")
  (doc 'returns "(hit-point distance num-steps) or #f if no hit")
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

(define (raymarch-mesh mesh ray params)
  (doc 'export #t)
  (doc 'type '(-> Mesh Ray3 RaymarchParams (Or (List Vec3 Real Nat) #f)))
  (doc 'description "Specialized raymarcher for mesh SDFs (uses BVH acceleration)")
  (let ([sdf-fn (lambda (p) (mesh-sdf mesh p))])
       (raymarch sdf-fn ray params)))

(doc 'section 'normal-computation)

(define (sdf-normal sdf-fn point)
  (doc 'export #t)
  (doc 'type '(-> (-> Vec3 Real) Vec3 Vec3))
  (doc 'description "Compute surface normal using gradient of SDF")
  (doc 'note "Uses central differences for better accuracy")
  (let* ([eps 0.001]
         [dx (- (sdf-fn (vec3-add point (vec3 eps 0 0)))
                (sdf-fn (vec3-sub point (vec3 eps 0 0))))]
         [dy (- (sdf-fn (vec3-add point (vec3 0 eps 0)))
                (sdf-fn (vec3-sub point (vec3 0 eps 0))))]
         [dz (- (sdf-fn (vec3-add point (vec3 0 0 eps)))
                (sdf-fn (vec3-sub point (vec3 0 0 eps))))])
        (vec3-normalize (vec3 dx dy dz))))

(define (mesh-sdf-normal mesh point)
  (doc 'export #t)
  (doc 'type '(-> Mesh Vec3 Vec3))
  (doc 'description "Compute surface normal for mesh SDF")
  (mesh-sdf-gradient mesh point))

(doc 'section 'soft-shadows)

(define (raymarch-shadow sdf-fn ray light-distance softness params)
  (doc 'export #t)
  (doc 'type '(-> (-> Vec3 Real) Ray3 Real Real RaymarchParams Real))
  (doc 'description "Compute soft shadow factor (0 = full shadow, 1 = no shadow)")
  (doc 'param 'light-distance "distance to light source")
  (doc 'param 'softness "higher = softer shadows (typically 4-32)")
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

(doc 'section 'ambient-occlusion)

(define (raymarch-ao sdf-fn point normal num-samples)
  (doc 'export #t)
  (doc 'type '(-> (-> Vec3 Real) Vec3 Vec3 Nat Real))
  (doc 'description "Compute ambient occlusion factor (0 = fully occluded, 1 = no occlusion)")
  (doc 'note "Uses multiple samples along normal direction")
  (doc 'param 'num-samples "typically 5")
  (max 0.0
       (- 1.0
          (let sample-ao ([i 0] [total 0] [sum 0.0])
               (if (>= i num-samples)
                   (/ sum num-samples)
                   (let* ([step-size (* (+ i 1) 0.1)]
                          [sample-point (vec3-add point (vec3-scale normal step-size))]
                          [dist (sdf-fn sample-point)]
                          [occlusion (- step-size dist)]
                          [weight (/ 1.0 (expt 2.0 i))])
                         (sample-ao (+ i 1)
                                    total
                                    (+ sum (* weight (max 0.0 occlusion))))))))))

(doc 'section 'scene-rendering)

(define (simple-shading normal light-dir view-dir)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Vec3 Vec3 Real))
  (doc 'description "Simple diffuse shading")
  (doc 'param 'normal "surface normal")
  (doc 'param 'light-dir "direction to light (normalized)")
  (doc 'param 'view-dir "direction to camera (normalized)")
  (let ([diffuse (max 0.0 (vec3-dot normal light-dir))]
        [ambient 0.2])
       (+ ambient (* 0.8 diffuse))))

(define (render-pixel sdf-fn ray light-pos params)
  (doc 'export #t)
  (doc 'type '(-> (-> Vec3 Real) Ray3 Vec3 RaymarchParams Real))
  (doc 'description "Render a single pixel (returns grayscale value 0-1)")
  (doc 'param 'light-pos "position of light source")
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

(doc 'section 'performance-optimization)

(define (adaptive-step sdf-fn point)
  (doc 'export #t)
  (doc 'type '(-> (-> Vec3 Real) Vec3 Real))
  (doc 'description "Compute adaptive step size based on local SDF variation")
  (doc 'note "Can skip larger steps in areas of low curvature")
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

(define (bvh-accelerated-raymarch mesh ray params)
  (doc 'export #t)
  (doc 'type '(-> Mesh Ray3 RaymarchParams (Or (List Vec3 Real Nat) #f)))
  (doc 'description "Hybrid approach: use BVH for initial ray-mesh intersection, then raymarch in vicinity of hit for accurate SDF")
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
