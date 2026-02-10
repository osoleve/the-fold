;;; lattice/geometry/shape-protocol.ss — Unified Shape Protocol System
;;;
;;; Provides protocol-based interface for geometric shapes, enabling:
;;; - Polymorphic ray intersection across shape types
;;; - Unified bounding box computation
;;; - Generic containment and distance queries
;;; - Heterogeneous scene graphs with unified traversal
;;;
;;; @requires lattice/fp/protocol.ss
;;; @requires lattice/geometry/geometry.ss

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'protocol)
(require 'vec3)
(require 'geometry)

(doc 'module 'shape-protocol)
(doc 'description "Unified protocol interface for geometric shapes")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================================
;;; Core Shape Protocols
;;; ============================================================================

(doc 'section 'ray-intersection)

;;; shape-intersect-ray : Shape × Ray3 → (Or Number (List Number Number) #f)
;;; Test ray intersection with any shape.
;;; Returns t parameter(s) if hit, #f otherwise.
(define-protocol (shape-intersect-ray shape ray)
  "Test ray intersection, returns t parameter(s) or #f")

(doc 'section 'bounding-box)

;;; shape-aabb : Shape → AABB
;;; Compute axis-aligned bounding box for any shape.
(define-protocol (shape-aabb shape)
  "Compute axis-aligned bounding box")

(doc 'section 'containment)

;;; shape-contains-point? : Shape × Point3 → Bool
;;; Test if point is inside or on shape boundary.
(define-protocol (shape-contains-point? shape point)
  "Test if shape contains point")

(doc 'section 'distance)

;;; shape-distance-point : Shape × Point3 → Number
;;; Signed distance from point to shape surface.
;;; Negative inside, positive outside, zero on surface.
(define-protocol (shape-distance-point shape point)
  "Signed distance from point to shape surface")

(doc 'section 'center)

;;; shape-center : Shape → Point3
;;; Get the center point of any shape.
(define-protocol (shape-center shape)
  "Get shape center point")

(doc 'section 'volume-and-area)

;;; shape-volume : Shape → Number | #f
;;; Compute volume of volumetric shapes. Returns #f for 2D shapes.
(define-protocol/default (shape-volume shape)
  (lambda (s) #f))

;;; shape-surface-area : Shape → Number | #f
;;; Compute surface area. Returns #f if not applicable.
(define-protocol/default (shape-surface-area shape)
  (lambda (s) #f))

(doc 'section 'normal)

;;; shape-normal-at : Shape × Point3 → Vec3
;;; Get outward normal at point on shape surface.
(define-protocol/default (shape-normal-at shape point)
  (lambda (s p)
    ;; Default: approximate via gradient of distance field
    (let* ([eps 0.001]
           [px (vec3 (+ (vec3-x p) eps) (vec3-y p) (vec3-z p))]
           [nx (vec3 (- (vec3-x p) eps) (vec3-y p) (vec3-z p))]
           [py (vec3 (vec3-x p) (+ (vec3-y p) eps) (vec3-z p))]
           [ny (vec3 (vec3-x p) (- (vec3-y p) eps) (vec3-z p))]
           [pz (vec3 (vec3-x p) (vec3-y p) (+ (vec3-z p) eps))]
           [nz (vec3 (vec3-x p) (vec3-y p) (- (vec3-z p) eps))]
           [dx (- (shape-distance-point s px) (shape-distance-point s nx))]
           [dy (- (shape-distance-point s py) (shape-distance-point s ny))]
           [dz (- (shape-distance-point s pz) (shape-distance-point s nz))])
      (vec3-normalize (vec3 dx dy dz)))))

;;; ============================================================================
;;; Implementations for Sphere
;;; ============================================================================

(doc 'section 'sphere-implementation)

(implement-protocol! 'shape-intersect-ray 'sphere
  (lambda (s ray)
    (intersect-ray-sphere ray s)))

(implement-protocol! 'shape-aabb 'sphere
  (lambda (s)
    (let* ([c (sphere-center s)]
           [r (sphere-radius s)]
           [rv (vec3 r r r)])
      (aabb (vec3-sub c rv) (vec3-add c rv)))))

(implement-protocol! 'shape-contains-point? 'sphere
  (lambda (s point)
    (point-in-sphere? point s)))

(implement-protocol! 'shape-distance-point 'sphere
  (lambda (s point)
    (distance-point-sphere point s)))

(implement-protocol! 'shape-center 'sphere
  (lambda (s) (sphere-center s)))

(implement-protocol! 'shape-volume 'sphere
  (lambda (s) (sphere-volume s)))

(implement-protocol! 'shape-surface-area 'sphere
  (lambda (s) (sphere-surface-area s)))

(implement-protocol! 'shape-normal-at 'sphere
  (lambda (s point)
    (vec3-normalize (vec3-sub point (sphere-center s)))))

;;; ============================================================================
;;; Implementations for AABB
;;; ============================================================================

(doc 'section 'aabb-implementation)

(implement-protocol! 'shape-intersect-ray 'aabb
  (lambda (box ray)
    (intersect-ray-aabb ray box)))

(implement-protocol! 'shape-aabb 'aabb
  (lambda (box) box))  ; AABB is its own bounding box

(implement-protocol! 'shape-contains-point? 'aabb
  (lambda (box point)
    (point-in-aabb? point box)))

(implement-protocol! 'shape-distance-point 'aabb
  (lambda (box point)
    ;; Signed distance to AABB
    (let* ([bmin (aabb-min box)]
           [bmax (aabb-max box)]
           [closest (closest-point-on-aabb point box)]
           [dist (vec3-length (vec3-sub point closest))])
      ;; Check if inside
      (if (point-in-aabb? point box)
          ;; Inside: negative distance to nearest face
          (let* ([to-min (vec3-sub point bmin)]
                 [to-max (vec3-sub bmax point)]
                 [dx (min (vec3-x to-min) (vec3-x to-max))]
                 [dy (min (vec3-y to-min) (vec3-y to-max))]
                 [dz (min (vec3-z to-min) (vec3-z to-max))])
            (- (min dx dy dz)))
          ;; Outside: positive distance
          dist))))

(implement-protocol! 'shape-center 'aabb
  (lambda (box) (aabb-center box)))

(implement-protocol! 'shape-volume 'aabb
  (lambda (box) (aabb-volume box)))

(implement-protocol! 'shape-surface-area 'aabb
  (lambda (box)
    (let* ([ext (aabb-extents box)]
           [x (* 2 (vec3-x ext))]
           [y (* 2 (vec3-y ext))]
           [z (* 2 (vec3-z ext))])
      (* 2 (+ (* x y) (* y z) (* x z))))))

(implement-protocol! 'shape-normal-at 'aabb
  (lambda (box point)
    ;; Find which face is closest
    (let* ([bmin (aabb-min box)]
           [bmax (aabb-max box)]
           [c (aabb-center box)]
           [ext (aabb-extents box)]
           [rel (vec3-sub point c)]
           [nx (/ (vec3-x rel) (max (vec3-x ext) 0.0001))]
           [ny (/ (vec3-y rel) (max (vec3-y ext) 0.0001))]
           [nz (/ (vec3-z rel) (max (vec3-z ext) 0.0001))]
           [ax (abs nx)]
           [ay (abs ny)]
           [az (abs nz)])
      (cond
        [(and (>= ax ay) (>= ax az))
         (vec3 (if (> nx 0) 1 -1) 0 0)]
        [(and (>= ay ax) (>= ay az))
         (vec3 0 (if (> ny 0) 1 -1) 0)]
        [else
         (vec3 0 0 (if (> nz 0) 1 -1))]))))

;;; ============================================================================
;;; Implementations for Triangle3
;;; ============================================================================

(doc 'section 'triangle3-implementation)

(implement-protocol! 'shape-intersect-ray 'triangle3
  (lambda (tri ray)
    (intersect-ray-triangle ray tri)))

(implement-protocol! 'shape-aabb 'triangle3
  (lambda (tri)
    (aabb-from-points (list (triangle3-p1 tri)
                            (triangle3-p2 tri)
                            (triangle3-p3 tri)))))

(implement-protocol! 'shape-contains-point? 'triangle3
  (lambda (tri point)
    (point-in-triangle? point tri)))

(implement-protocol! 'shape-distance-point 'triangle3
  (lambda (tri point)
    ;; Distance to triangle (unsigned - triangles have no interior)
    (let* ([p1 (triangle3-p1 tri)]
           [p2 (triangle3-p2 tri)]
           [p3 (triangle3-p3 tri)]
           ;; Project point onto triangle plane
           [normal (triangle-normal tri)]
           [plane (plane3-from-point-normal p1 normal)]
           [proj-dist (distance-point-plane point plane)]
           [proj (vec3-sub point (vec3-scale normal proj-dist))]
           ;; Check if projection is inside triangle
           [bary (barycentric-coords proj p1 p2 p3)]
           [u (car bary)]
           [v (cadr bary)]
           [w (caddr bary)])
      (if (and (>= u -0.001) (>= v -0.001) (>= w -0.001))
          ;; Inside projection: distance is just plane distance
          (abs proj-dist)
          ;; Outside: need distance to nearest edge
          (let* ([d1 (distance-to-segment point p1 p2)]
                 [d2 (distance-to-segment point p2 p3)]
                 [d3 (distance-to-segment point p3 p1)])
            (min d1 d2 d3))))))

(implement-protocol! 'shape-center 'triangle3
  (lambda (tri)
    (vec3-scale (vec3-add (vec3-add (triangle3-p1 tri)
                                    (triangle3-p2 tri))
                          (triangle3-p3 tri))
                (/ 1.0 3.0))))

(implement-protocol! 'shape-surface-area 'triangle3
  (lambda (tri) (triangle-area tri)))

(implement-protocol! 'shape-normal-at 'triangle3
  (lambda (tri point)
    (triangle-normal tri)))

;;; ============================================================================
;;; Implementations for Plane3
;;; ============================================================================

(doc 'section 'plane3-implementation)

(implement-protocol! 'shape-intersect-ray 'plane3
  (lambda (plane ray)
    (intersect-ray-plane ray plane)))

(implement-protocol! 'shape-contains-point? 'plane3
  (lambda (plane point)
    ;; A point is "on" the plane if distance is ~0
    (< (abs (distance-point-plane point plane)) 0.0001)))

(implement-protocol! 'shape-distance-point 'plane3
  (lambda (plane point)
    (distance-point-plane point plane)))

(implement-protocol! 'shape-normal-at 'plane3
  (lambda (plane point)
    (plane3-normal plane)))

;;; ============================================================================
;;; Implementations for Circle (2D)
;;; ============================================================================

(doc 'section 'circle-implementation)

(implement-protocol! 'shape-center 'circle
  (lambda (c) (circle-center c)))

(implement-protocol! 'shape-contains-point? 'circle
  (lambda (c point)
    ;; 2D containment (ignores z if vec3)
    (let* ([center (circle-center c)]
           [r (circle-radius c)]
           [dx (- (if (pair? point) (car point) (vec3-x point))
                  (if (pair? center) (car center) (vec3-x center)))]
           [dy (- (if (pair? point) (cadr point) (vec3-y point))
                  (if (pair? center) (cadr center) (vec3-y center)))])
      (<= (+ (* dx dx) (* dy dy)) (* r r)))))

(implement-protocol! 'shape-surface-area 'circle
  (lambda (c)
    ;; Area of 2D circle
    (let ([r (circle-radius c)])
      (* 3.141592653589793 r r))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (distance-to-segment point a b)
  "Distance from point to line segment a-b"
  (let* ([ab (vec3-sub b a)]
         [ap (vec3-sub point a)]
         [ab-len-sq (vec3-dot ab ab)])
    (if (< ab-len-sq 0.0001)
        ;; Degenerate segment
        (vec3-length ap)
        (let* ([t (/ (vec3-dot ap ab) ab-len-sq)]
               [t-clamped (max 0 (min 1 t))]
               [closest (vec3-add a (vec3-scale ab t-clamped))])
          (vec3-length (vec3-sub point closest))))))

;;; ============================================================================
;;; Derived Operations (work with any shape via protocols)
;;; ============================================================================

(doc 'section 'derived-operations)

;;; scene-intersect-ray : (List Shape) × Ray3 → (Or (Pair Shape Number) #f)
;;; Find first shape hit by ray in a scene.
;;; Returns (shape . t) pair or #f if no hit.
(define (scene-intersect-ray shapes ray)
  (doc 'export #t)
  (doc 'type '(-> (List Shape) Ray3 (Or (Pair Shape Number) #f)))
  (doc 'description "Find first shape hit by ray, returns (shape . t) or #f")
  (let loop ([shapes shapes]
             [best-shape #f]
             [best-t +inf.0])
    (if (null? shapes)
        (if best-shape (cons best-shape best-t) #f)
        (let* ([shape (car shapes)]
               [hit (shape-intersect-ray shape ray)])
          (if hit
              (let ([t (if (pair? hit)
                           ;; Multiple t values - take smallest positive
                           (let ([t1 (car hit)]
                                 [t2 (cadr hit)])
                             (cond
                               [(and (>= t1 0) (>= t2 0)) (min t1 t2)]
                               [(>= t1 0) t1]
                               [(>= t2 0) t2]
                               [else +inf.0]))
                           hit)])
                (if (and (> t 0) (< t best-t))
                    (loop (cdr shapes) shape t)
                    (loop (cdr shapes) best-shape best-t)))
              (loop (cdr shapes) best-shape best-t))))))

;;; scene-aabb : (List Shape) → AABB
;;; Compute bounding box enclosing all shapes.
(define (scene-aabb shapes)
  (doc 'export #t)
  (doc 'type '(-> (List Shape) AABB))
  (doc 'description "Compute AABB enclosing all shapes in scene")
  (if (null? shapes)
      (aabb (vec3-zero) (vec3-zero))
      (let loop ([shapes (cdr shapes)]
                 [result (shape-aabb (car shapes))])
        (if (null? shapes)
            result
            (loop (cdr shapes)
                  (aabb-merge result (shape-aabb (car shapes))))))))

;;; shapes-containing-point : (List Shape) × Point3 → (List Shape)
;;; Find all shapes containing a point.
(define (shapes-containing-point shapes point)
  (doc 'export #t)
  (doc 'type '(-> (List Shape) Point3 (List Shape)))
  (doc 'description "Find all shapes containing the point")
  (filter (lambda (s) (shape-contains-point? s point)) shapes))

;;; closest-shape : (List Shape) × Point3 → (Pair Shape Number)
;;; Find shape closest to point and its distance.
(define (closest-shape shapes point)
  (doc 'export #t)
  (doc 'type '(-> (List Shape) Point3 (Or (Pair Shape Number) #f)))
  (doc 'description "Find shape closest to point, returns (shape . distance)")
  (if (null? shapes)
      #f
      (let loop ([shapes (cdr shapes)]
                 [best-shape (car shapes)]
                 [best-dist (abs (shape-distance-point (car shapes) point))])
        (if (null? shapes)
            (cons best-shape best-dist)
            (let* ([shape (car shapes)]
                   [dist (abs (shape-distance-point shape point))])
              (if (< dist best-dist)
                  (loop (cdr shapes) shape dist)
                  (loop (cdr shapes) best-shape best-dist)))))))

;;; scene-sdf : (List Shape) × Point3 → Number
;;; Signed distance field for scene (union of shapes).
;;; Uses min of individual SDFs.
(define (scene-sdf shapes point)
  (doc 'export #t)
  (doc 'type '(-> (List Shape) Point3 Number))
  (doc 'description "SDF for scene - minimum distance to any shape")
  (if (null? shapes)
      +inf.0
      (let loop ([shapes (cdr shapes)]
                 [min-d (shape-distance-point (car shapes) point)])
        (if (null? shapes)
            min-d
            (loop (cdr shapes)
                  (min min-d (shape-distance-point (car shapes) point)))))))

;;; scene-total-volume : (List Shape) → Number
;;; Sum of volumes of all shapes (ignores overlaps).
(define (scene-total-volume shapes)
  (doc 'export #t)
  (doc 'type '(-> (List Shape) Number))
  (doc 'description "Sum of volumes of all volumetric shapes")
  (let loop ([shapes shapes] [total 0])
    (if (null? shapes)
        total
        (let ([v (shape-volume (car shapes))])
          (loop (cdr shapes)
                (if v (+ total v) total))))))

;;; ============================================================================
;;; Exports Summary
;;; ============================================================================

(doc 'section 'exports-summary)
(doc 'exports "
Core Protocols (work with any shape type):
  shape-intersect-ray    Ray intersection test
  shape-aabb             Axis-aligned bounding box
  shape-contains-point?  Point containment test
  shape-distance-point   Signed distance to surface
  shape-center           Shape center point
  shape-volume           Volume (volumetric shapes)
  shape-surface-area     Surface area
  shape-normal-at        Surface normal at point

Implemented for:
  'sphere     - Full support
  'aabb       - Full support
  'triangle3  - Full support
  'plane3     - Partial (infinite, no volume)
  'circle     - Partial (2D)

Derived Operations:
  scene-intersect-ray      First ray hit in scene
  scene-aabb               Scene bounding box
  shapes-containing-point  Find shapes containing point
  closest-shape            Find nearest shape to point
  scene-sdf                Scene signed distance field
  scene-total-volume       Sum of shape volumes
")

(display "  Use (shape-intersect-ray shape ray) for any shape type\n")
(display "  Use (scene-intersect-ray shapes ray) for scene traversal\n")
