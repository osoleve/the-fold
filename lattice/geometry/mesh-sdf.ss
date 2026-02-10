(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'geometry)
(require 'bvh)

(doc 'module 'mesh-sdf)
(doc 'description "Mesh signed distance fields with BVH acceleration")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'provides "SDF computation for triangle meshes: signed distance from any point to surface (negative inside, zero on surface, positive outside)")

(doc 'section 'mesh-structure)
(doc 'note "Mesh: (mesh triangles bvh) - represents a triangle mesh with precomputed BVH for acceleration")

(define (make-mesh triangles)
  (doc 'export #t)
  (doc 'type '(-> (List Triangle3) Mesh))
  (let ([bvh (bvh-build triangles 10)])  ; Max 10 triangles per leaf
       (list 'mesh triangles bvh)))

(define (mesh? m)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? m) (eq? (car m) 'mesh)))

(define (mesh-triangles m)
  (doc 'export #t)
  (doc 'type '(-> Mesh (List Triangle3)))
  (cadr m))

(define (mesh-bvh m)
  (doc 'export #t)
  (doc 'type '(-> Mesh BVH))
  (caddr m))

(doc 'section 'mesh-sdf-computation)

(define (mesh-sdf mesh point)
  (doc 'export #t)
  (doc 'type '(-> Mesh Point3 Number))
  (doc 'description "Compute signed distance from point to mesh surface; uses BVH for acceleration")
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

(define (mesh-sdf-gradient mesh point)
  (doc 'export #t)
  (doc 'type '(-> Mesh Point3 Vec3))
  (doc 'description "Compute gradient (normal) of the SDF at a point; uses finite differences for numerical gradient")
  (let* ([eps 0.001]
         [dx (- (mesh-sdf mesh (vec3-add point (vec3 eps 0 0)))
                (mesh-sdf mesh (vec3-sub point (vec3 eps 0 0))))]
         [dy (- (mesh-sdf mesh (vec3-add point (vec3 0 eps 0)))
                (mesh-sdf mesh (vec3-sub point (vec3 0 eps 0))))]
         [dz (- (mesh-sdf mesh (vec3-add point (vec3 0 0 eps)))
                (mesh-sdf mesh (vec3-sub point (vec3 0 0 eps))))])
        (vec3-normalize (vec3 dx dy dz))))

(doc 'section 'mesh-construction-helpers)

(define (make-mesh-cube size)
  (doc 'export #t)
  (doc 'type '(-> Number Mesh))
  (doc 'description "Create a cube mesh centered at origin with given half-size")
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

(define (subdivide-icosphere-triangles triangles radius levels)
  (doc 'export #t)
  (doc 'type '(-> (List Triangle3) Number Number (List Triangle3)))
  (doc 'description "Apply n levels of subdivision to a list of triangles; uses shared edge midpoint table to ensure adjacent triangles share exact vertices")
  (if (<= levels 0)
      triangles
      (let ([subdivided (subdivide-icosphere-once triangles radius)])
        (subdivide-icosphere-triangles subdivided radius (- levels 1)))))

(define (subdivide-icosphere-once triangles radius)
  (doc 'type '(-> (List Triangle3) Number (List Triangle3)))
  (doc 'description "Single subdivision pass using shared edge midpoints")
  (doc 'note "Key insight: each edge midpoint is computed ONCE and shared by adjacent triangles; uses vertex identity (not coordinates) for edge keying to avoid floating-point issues")
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
    (append-map (lambda (tri)
                  (subdivide-triangle-with-midpoints-by-id tri edge-midpoints get-vertex-id))
                triangles)))

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
  (doc 'export #t)
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

(define (make-mesh-sphere-ico radius subdivisions)
  (doc 'export #t)
  (doc 'type '(-> Number Number Mesh))
  (doc 'description "Create a sphere mesh using icosphere subdivision")
  (doc 'param 'radius "Sphere radius")
  (doc 'param 'subdivisions "Number of subdivision levels (0-3 recommended): 0=20 triangles, 1=80, 2=320, 3=1280")
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

(doc 'section 'mesh-ray-intersection)

(define (mesh-intersect-ray mesh ray)
  (doc 'export #t)
  (doc 'type '(-> Mesh Ray3 (Maybe (List Point3 Number Triangle3))))
  (doc 'description "Find closest intersection of ray with mesh")
  (doc 'returns "(hit-point t-value triangle) or #f")
  (let ([result (bvh-intersect-ray (mesh-bvh mesh) ray)])
       (if result
           (let* ([triangle (car result)]
                  [t (cadr result)]
                  [hit-point (ray3-point-at ray t)])
                 (list hit-point t triangle))
           #f)))

(doc 'section 'mesh-statistics)

(define (mesh-triangle-count mesh)
  (doc 'export #t)
  (doc 'type '(-> Mesh Number))
  (length (mesh-triangles mesh)))

(define (mesh-bvh-depth mesh)
  (doc 'export #t)
  (doc 'type '(-> Mesh Number))
  (bvh-depth (mesh-bvh mesh)))

(define (mesh-bvh-node-count mesh)
  (doc 'export #t)
  (doc 'type '(-> Mesh Number))
  (bvh-count-nodes (mesh-bvh mesh)))

(define (mesh-bounds mesh)
  (doc 'export #t)
  (doc 'type '(-> Mesh AABB))
  (doc 'description "Get the bounding box of the entire mesh")
  (bvh-bbox (mesh-bvh mesh)))
