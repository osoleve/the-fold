;;; core/geometry/mesh-sdf.ss — Mesh Signed Distance Fields with BVH Acceleration
;;;
;;; Provides signed distance field (SDF) computation for triangle meshes.
;;; Uses BVH acceleration structure for efficient queries.
;;;
;;; A mesh SDF is a function that returns the signed distance from any point
;;; in space to the surface of the mesh:
;;; - Negative inside the mesh
;;; - Zero on the surface
;;; - Positive outside the mesh
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - geometry.ss
;;;   - bvh.ss

(load "core/base/prelude.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/geometry/bvh.ss")

;;; ====
;;; Mesh Structure
;;; ====

;;; Mesh: (mesh triangles bvh)
;;; Represents a triangle mesh with precomputed BVH for acceleration

;;; make-mesh : (List Triangle3) → Mesh
(define (make-mesh triangles)
  (let ([bvh (bvh-build triangles 10)])  ; Max 10 triangles per leaf
       (list 'mesh triangles bvh)))

;;; mesh? : α → Bool
(define (mesh? m)
  (and (pair? m) (eq? (car m) 'mesh)))

;;; mesh-triangles : Mesh → (List Triangle3)
(define (mesh-triangles m)
  (cadr m))

;;; mesh-bvh : Mesh → BVH
(define (mesh-bvh m)
  (caddr m))

;;; ====
;;; Mesh SDF Computation
;;; ====

;;; mesh-sdf : Mesh × Point3 → Number
;;; Compute signed distance from point to mesh surface
;;; Uses BVH for acceleration
(define (mesh-sdf mesh point)
  (let* ([bvh (mesh-bvh mesh)]
         [result (bvh-closest-point bvh point)])
        (if result
            (let* ([closest-point (car result)]
                   [distance (cadr result)]
                   [triangle (caddr result)]
                   ;; Determine sign based on normal
                   [normal (triangle-normal triangle)]
                   [to-point (vec3-sub point closest-point)]
                   [dot (vec3-dot normal to-point)])
                  ;; Negative if inside (dot < 0), positive if outside
                  (if (>= dot 0) (- distance) distance))
            ;; No triangles in mesh, return large positive distance
            1e10)))

;;; mesh-sdf-gradient : Mesh × Point3 → Vec3
;;; Compute gradient (normal) of the SDF at a point
;;; Uses finite differences for numerical gradient
(define (mesh-sdf-gradient mesh point)
  (let* ([eps 0.001]
         [dx (- (mesh-sdf mesh (vec3-add point (vec3 eps 0 0)))
                (mesh-sdf mesh (vec3-sub point (vec3 eps 0 0))))]
         [dy (- (mesh-sdf mesh (vec3-add point (vec3 0 eps 0)))
                (mesh-sdf mesh (vec3-sub point (vec3 0 eps 0))))]
         [dz (- (mesh-sdf mesh (vec3-add point (vec3 0 0 eps)))
                (mesh-sdf mesh (vec3-sub point (vec3 0 0 eps))))])
        (vec3-normalize (vec3 dx dy dz))))

;;; ====
;;; Mesh Construction Helpers
;;; ====

;;; make-mesh-cube : Number → Mesh
;;; Create a cube mesh centered at origin with given half-size
(define (make-mesh-cube size)
  (let* ([s size]
         [v000 (vec3 (- s) (- s) (- s))]
         [v001 (vec3 (- s) (- s) s)]
         [v010 (vec3 (- s) s (- s))]
         [v011 (vec3 (- s) s s)]
         [v100 (vec3 s (- s) (- s))]
         [v101 (vec3 s (- s) s)]
         [v110 (vec3 s s (- s))]
         [v111 (vec3 s s s)]
         ;; 12 triangles (2 per face)
         [triangles
          (list
           ;; Front face (z+)
           (triangle3 v001 v011 v111)
           (triangle3 v001 v111 v101)
           ;; Back face (z-)
           (triangle3 v000 v110 v010)
           (triangle3 v000 v100 v110)
           ;; Right face (x+)
           (triangle3 v100 v101 v111)
           (triangle3 v100 v111 v110)
           ;; Left face (x-)
           (triangle3 v000 v010 v011)
           (triangle3 v000 v011 v001)
           ;; Top face (y+)
           (triangle3 v010 v110 v111)
           (triangle3 v010 v111 v011)
           ;; Bottom face (y-)
           (triangle3 v000 v001 v101)
           (triangle3 v000 v101 v100))])
        (make-mesh triangles)))

;;; subdivide-icosphere-triangle : Triangle3 × Number → (List Triangle3)
;;; Subdivide a single triangle on the sphere surface into 4 triangles.
;;; Each edge midpoint is normalized to the sphere radius.
(define (subdivide-icosphere-triangle tri radius)
  (let* ([v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         ;; Compute edge midpoints
         [m01 (vec3-scale (vec3-add v0 v1) 0.5)]
         [m12 (vec3-scale (vec3-add v1 v2) 0.5)]
         [m20 (vec3-scale (vec3-add v2 v0) 0.5)]
         ;; Project midpoints onto sphere surface (normalize to radius)
         [m01-norm (vec3-scale (vec3-normalize m01) radius)]
         [m12-norm (vec3-scale (vec3-normalize m12) radius)]
         [m20-norm (vec3-scale (vec3-normalize m20) radius)])
        ;; Create 4 new triangles:
        ;;       v0
        ;;      /  \
        ;;    m01--m20
        ;;    / \  / \
        ;;  v1--m12--v2
        (list
         (triangle3 v0 m01-norm m20-norm)       ; Top triangle
         (triangle3 m01-norm v1 m12-norm)       ; Bottom-left triangle
         (triangle3 m20-norm m12-norm v2)       ; Bottom-right triangle
         (triangle3 m01-norm m12-norm m20-norm)))) ; Center triangle

;;; subdivide-icosphere-triangles : (List Triangle3) × Number × Number → (List Triangle3)
;;; Apply n levels of subdivision to a list of triangles.
(define (subdivide-icosphere-triangles triangles radius levels)
  (if (<= levels 0)
      triangles
      (let ([subdivided (apply append
                               (map (lambda (tri)
                                            (subdivide-icosphere-triangle tri radius))
                                    triangles))])
           (subdivide-icosphere-triangles subdivided radius (- levels 1)))))

;;; make-mesh-sphere-ico : Number × Number → Mesh
;;; Create a sphere mesh using icosphere subdivision
;;; radius: sphere radius
;;; subdivisions: number of subdivision levels (0-3 recommended)
;;;   - subdivision=0: 20 triangles (base icosahedron)
;;;   - subdivision=1: 80 triangles
;;;   - subdivision=2: 320 triangles
;;;   - subdivision=3: 1280 triangles
(define (make-mesh-sphere-ico radius subdivisions)
  ;; Start with icosahedron
  (let* ([phi (* 0.5 (+ 1 (sqrt 5)))]  ; Golden ratio
         [a (/ 1.0 (sqrt (+ 1 (* phi phi))))]
         [b (* a phi)]
         ;; 12 vertices of icosahedron (normalized to radius)
         [vertices
          (list
           (vec3 0 (* a radius) (* b radius))
           (vec3 0 (* a radius) (- (* b radius)))
           (vec3 0 (- (* a radius)) (* b radius))
           (vec3 0 (- (* a radius)) (- (* b radius)))
           (vec3 (* a radius) (* b radius) 0)
           (vec3 (* a radius) (- (* b radius)) 0)
           (vec3 (- (* a radius)) (* b radius) 0)
           (vec3 (- (* a radius)) (- (* b radius)) 0)
           (vec3 (* b radius) 0 (* a radius))
           (vec3 (- (* b radius)) 0 (* a radius))
           (vec3 (* b radius) 0 (- (* a radius)))
           (vec3 (- (* b radius)) 0 (- (* a radius))))]
         ;; 20 triangular faces
         [indices
          '((0 4 8) (0 8 9) (0 9 6) (0 6 4) (0 1 10)
            (1 0 4) (1 4 10) (1 10 5) (1 5 3) (1 3 11)
            (2 3 7) (2 7 9) (2 9 8) (2 8 5) (2 5 3)
            (3 5 10) (3 10 11) (3 11 7) (4 6 10) (5 8 4))]
         [base-triangles
          (map (lambda (idx)
                       (triangle3 (list-ref vertices (car idx))
                                  (list-ref vertices (cadr idx))
                                  (list-ref vertices (caddr idx))))
               indices)]
         ;; Apply subdivision
         [triangles (subdivide-icosphere-triangles base-triangles radius subdivisions)])
        (make-mesh triangles)))

;;; ====
;;; Mesh Ray Intersection
;;; ====

;;; mesh-intersect-ray : Mesh × Ray3 → (Point3 Number Triangle3) | #f
;;; Find closest intersection of ray with mesh
;;; Returns (hit-point t-value triangle) or #f
(define (mesh-intersect-ray mesh ray)
  (let ([result (bvh-intersect-ray (mesh-bvh mesh) ray)])
       (if result
           (let* ([triangle (car result)]
                  [t (cadr result)]
                  [hit-point (ray3-point-at ray t)])
                 (list hit-point t triangle))
           #f)))

;;; ====
;;; Mesh Statistics
;;; ====

;;; mesh-triangle-count : Mesh → Number
(define (mesh-triangle-count mesh)
  (length (mesh-triangles mesh)))

;;; mesh-bvh-depth : Mesh → Number
(define (mesh-bvh-depth mesh)
  (bvh-depth (mesh-bvh mesh)))

;;; mesh-bvh-node-count : Mesh → Number
(define (mesh-bvh-node-count mesh)
  (bvh-count-nodes (mesh-bvh mesh)))

;;; mesh-bounds : Mesh → AABB
;;; Get the bounding box of the entire mesh
(define (mesh-bounds mesh)
  (bvh-bbox (mesh-bvh mesh)))
