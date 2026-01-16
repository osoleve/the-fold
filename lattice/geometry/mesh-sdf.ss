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

;;; subdivide-icosphere-triangles : (List Triangle3) × Number × Number → (List Triangle3)
;;; Apply n levels of subdivision to a list of triangles.
;;; Uses shared edge midpoint table to ensure adjacent triangles share exact vertices.
(define (subdivide-icosphere-triangles triangles radius levels)
  (if (<= levels 0)
      triangles
      (let ([subdivided (subdivide-icosphere-once triangles radius)])
        (subdivide-icosphere-triangles subdivided radius (- levels 1)))))

;;; subdivide-icosphere-once : (List Triangle3) × Number → (List Triangle3)
;;; Single subdivision pass using shared edge midpoints.
;;; Key insight: each edge midpoint is computed ONCE and shared by adjacent triangles.
;;; Uses vertex identity (not coordinates) for edge keying to avoid floating-point issues.
(define (subdivide-icosphere-once triangles radius)
  ;; First, collect all unique vertices and assign stable IDs
  (let* ([vertex-id-table (make-eq-hashtable)]
         [next-id 0]
         [get-vertex-id
          (lambda (v)
            (or (eq-hashtable-ref vertex-id-table v #f)
                (let ([id next-id])
                  (eq-hashtable-set! vertex-id-table v id)
                  (set! next-id (+ next-id 1))
                  id)))]
         ;; Build edge → midpoint table using vertex IDs as keys
         [edge-midpoints (make-hashtable equal-hash equal?)])
    ;; First pass: assign IDs to all vertices
    (for-each
      (lambda (tri)
        (get-vertex-id (triangle3-p1 tri))
        (get-vertex-id (triangle3-p2 tri))
        (get-vertex-id (triangle3-p3 tri)))
      triangles)
    ;; Second pass: compute midpoints with ID-based keys
    (for-each
      (lambda (tri)
        (let ([v0 (triangle3-p1 tri)]
              [v1 (triangle3-p2 tri)]
              [v2 (triangle3-p3 tri)])
          (ensure-edge-midpoint-by-id! edge-midpoints v0 v1 radius
                                        (get-vertex-id v0) (get-vertex-id v1))
          (ensure-edge-midpoint-by-id! edge-midpoints v1 v2 radius
                                        (get-vertex-id v1) (get-vertex-id v2))
          (ensure-edge-midpoint-by-id! edge-midpoints v2 v0 radius
                                        (get-vertex-id v2) (get-vertex-id v0))))
      triangles)
    ;; Third pass: subdivide using shared midpoints
    (apply append
      (map (lambda (tri)
             (subdivide-triangle-with-midpoints-by-id tri edge-midpoints get-vertex-id))
           triangles))))

;;; vec3-edge-key : Vec3 × Vec3 → (List Number Number Number Number Number Number)
;;; Create a canonical edge key from two vertices.
;;; Uses sorted coordinates to ensure (v0,v1) and (v1,v0) produce same key.
(define (vec3-edge-key v0 v1)
  (let ([coords0 (list (vec3-x v0) (vec3-y v0) (vec3-z v0))]
        [coords1 (list (vec3-x v1) (vec3-y v1) (vec3-z v1))])
    (if (vec3-coords<? coords0 coords1)
        (append coords0 coords1)
        (append coords1 coords0))))

;;; vec3-coords<? : (List Num Num Num) × (List Num Num Num) → Boolean
;;; Lexicographic comparison for coordinate lists.
(define (vec3-coords<? a b)
  (cond
    [(< (car a) (car b)) #t]
    [(> (car a) (car b)) #f]
    [(< (cadr a) (cadr b)) #t]
    [(> (cadr a) (cadr b)) #f]
    [else (< (caddr a) (caddr b))]))

;;; edge-id-key : Int × Int → (Int . Int)
;;; Create canonical edge key from vertex IDs (smaller ID first).
(define (edge-id-key id0 id1)
  (if (< id0 id1) (cons id0 id1) (cons id1 id0)))

;;; ensure-edge-midpoint-by-id! : HashTable × Vec3 × Vec3 × Number × Int × Int → Vec3
;;; Get or compute the midpoint for an edge, using vertex IDs as key.
(define (ensure-edge-midpoint-by-id! table v0 v1 radius id0 id1)
  (let ([key (edge-id-key id0 id1)])
    (or (hashtable-ref table key #f)
        (let* ([mid (vec3-scale (vec3-add v0 v1) 0.5)]
               [mid-norm (vec3-scale (vec3-normalize mid) radius)])
          (hashtable-set! table key mid-norm)
          mid-norm))))

;;; get-edge-midpoint-by-id : HashTable × Int × Int → Vec3
;;; Retrieve the shared midpoint for an edge by vertex IDs.
(define (get-edge-midpoint-by-id table id0 id1)
  (hashtable-ref table (edge-id-key id0 id1) #f))

;;; subdivide-triangle-with-midpoints-by-id : Triangle3 × HashTable × (Vec3 → Int) → (List Triangle3)
;;; Subdivide a triangle using pre-computed shared midpoints (ID-based lookup).
(define (subdivide-triangle-with-midpoints-by-id tri midpoint-table get-id)
  (let* ([v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         [id0 (get-id v0)]
         [id1 (get-id v1)]
         [id2 (get-id v2)]
         ;; Look up shared midpoints by ID
         [m01 (get-edge-midpoint-by-id midpoint-table id0 id1)]
         [m12 (get-edge-midpoint-by-id midpoint-table id1 id2)]
         [m20 (get-edge-midpoint-by-id midpoint-table id2 id0)])
    ;; Create 4 new triangles
    (list
     (triangle3 v0 m01 m20)       ; Top triangle
     (triangle3 m01 v1 m12)       ; Bottom-left triangle
     (triangle3 m20 m12 v2)       ; Bottom-right triangle
     (triangle3 m01 m12 m20))))   ; Center triangle

;;; get-edge-midpoint : HashTable × Vec3 × Vec3 → Vec3
;;; Retrieve the shared midpoint for an edge.
(define (get-edge-midpoint table v0 v1)
  (hashtable-ref table (vec3-edge-key v0 v1) #f))

;;; subdivide-triangle-with-midpoints : Triangle3 × HashTable → (List Triangle3)
;;; Subdivide a triangle using pre-computed shared midpoints.
(define (subdivide-triangle-with-midpoints tri midpoint-table)
  (let* ([v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         ;; Look up shared midpoints
         [m01 (get-edge-midpoint midpoint-table v0 v1)]
         [m12 (get-edge-midpoint midpoint-table v1 v2)]
         [m20 (get-edge-midpoint midpoint-table v2 v0)])
    ;; Create 4 new triangles:
    ;;       v0
    ;;      /  \
    ;;    m01--m20
    ;;    / \  / \
    ;;  v1--m12--v2
    (list
     (triangle3 v0 m01 m20)       ; Top triangle
     (triangle3 m01 v1 m12)       ; Bottom-left triangle
     (triangle3 m20 m12 v2)       ; Bottom-right triangle
     (triangle3 m01 m12 m20))))   ; Center triangle

;;; Legacy function for backwards compatibility
;;; subdivide-icosphere-triangle : Triangle3 × Number → (List Triangle3)
;;; Subdivide a single triangle on the sphere surface into 4 triangles.
;;; NOTE: Use subdivide-icosphere-triangles for proper vertex sharing.
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
         ;; 20 triangular faces of icosahedron
         ;; Vertices form 3 orthogonal golden rectangles:
         ;;   0-3: (0, ±a, ±b) - yz plane rectangle
         ;;   4-7: (±a, ±b, 0) - xy plane rectangle
         ;;   8-11: (±b, 0, ±a) - xz plane rectangle
         ;; Adjacent vertices are at distance 2a (edge length).
         ;; Each vertex has degree 5 (connected to 5 neighbors).
         ;; Correct topology: 12 vertices, 30 edges, 20 faces, χ=2
         [indices
          '(;; 5 triangles around vertex 0 (0,a,b) - neighbors: 2,4,6,8,9
            (0 2 8) (0 8 4) (0 4 6) (0 6 9) (0 9 2)
            ;; 5 triangles around vertex 1 (0,a,-b) - neighbors: 3,4,6,10,11
            (1 4 10) (1 10 3) (1 3 11) (1 11 6) (1 6 4)
            ;; 5 equatorial triangles connecting "front" (z>0) vertices
            (2 5 8) (8 5 10) (10 5 3) (3 5 7) (7 5 2)
            ;; 5 equatorial triangles connecting "back" (z<0) vertices
            (9 7 2) (9 11 7) (11 3 7) (11 9 6) (4 8 10))]
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
