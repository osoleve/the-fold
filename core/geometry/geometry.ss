;;; core/geometry/geometry.ss — Geometric Primitives and Transformations
;;;
;;; Core geometry library for graphics and physics.
;;;
;;; This module provides:
;;; 1. Geometric primitives (points, lines, rays, planes, circles, spheres, boxes)
;;; 2. Transformation matrices (translation, rotation, scaling)
;;; 3. Geometric operations (distance, intersection, containment)
;;; 4. Utilities (coordinate conversions, normals, barycentric)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/vec3.ss
;;;   - linalg/matrix.ss
;;;   - linalg/quaternion.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec3.ss")
(load "core/linalg/matrix.ss")
(load "core/linalg/quaternion.ss")

;;; ============================================================
;;; Geometric Primitives
;;; ============================================================

;;; Point3 is just a Vec3
;;; point3 : Number × Number × Number → Point3
(define point3 vec3)

;;; Line3: (line3 origin direction)
;;; Infinite line through origin in direction
(define (line3 origin direction)
  (list 'line3 origin direction))

(define (line3? l)
  (and (pair? l) (eq? (car l) 'line3)))

(define (line3-origin l)
  (cadr l))

(define (line3-direction l)
  (caddr l))

;;; Ray3: (ray3 origin direction)
;;; Half-infinite ray starting at origin going in direction
(define (ray3 origin direction)
  (list 'ray3 origin direction))

(define (ray3? r)
  (and (pair? r) (eq? (car r) 'ray3)))

(define (ray3-origin r)
  (cadr r))

(define (ray3-direction r)
  (caddr r))

;;; ray3-point-at : Ray3 × Number → Point3
;;; Get point at parameter t along ray (t >= 0)
(define (ray3-point-at ray t)
  (vec3-add (ray3-origin ray)
            (vec3-scale (ray3-direction ray) t)))

;;; Plane3: (plane3 normal d)
;;; Plane defined by normal·p + d = 0
(define (plane3 normal d)
  (list 'plane3 normal d))

(define (plane3? p)
  (and (pair? p) (eq? (car p) 'plane3)))

(define (plane3-normal p)
  (cadr p))

(define (plane3-d p)
  (caddr p))

;;; plane3-from-point-normal : Point3 × Vec3 → Plane3
;;; Create plane from point and normal
(define (plane3-from-point-normal point normal)
  (let ([d (- (vec3-dot normal point))])
       (plane3 normal d)))

;;; plane3-from-points : Point3 × Point3 × Point3 → Plane3 | Error
;;; Create plane from three points
;;; Returns error if points are collinear
(define (plane3-from-points p1 p2 p3)
  (let* ([v1 (vec3-sub p2 p1)]
         [v2 (vec3-sub p3 p1)]
         [cp (vec3-cross v1 v2)]
         [mag (vec3-magnitude cp)])
        (if (< mag 1e-10)
            (list 'error 'plane3-from-points "Collinear points do not define a unique plane")
            (let ([normal (vec3-scale-inv cp mag)])
                 (plane3-from-point-normal p1 normal)))))

;;; Triangle3: (triangle3 p1 p2 p3)
(define (triangle3 p1 p2 p3)
  (list 'triangle3 p1 p2 p3))

(define (triangle3? t)
  (and (pair? t) (eq? (car t) 'triangle3)))

(define (triangle3-p1 t) (cadr t))
(define (triangle3-p2 t) (caddr t))
(define (triangle3-p3 t) (cadddr t))

;;; Circle: (circle center radius)
(define (circle center radius)
  (list 'circle center radius))

(define (circle? c)
  (and (pair? c) (eq? (car c) 'circle)))

(define (circle-center c) (cadr c))
(define (circle-radius c) (caddr c))

;;; Sphere: (sphere center radius)
(define (sphere center radius)
  (list 'sphere center radius))

(define (sphere? s)
  (and (pair? s) (eq? (car s) 'sphere)))

(define (sphere-center s) (cadr s))
(define (sphere-radius s) (caddr s))

;;; AABB (Axis-Aligned Bounding Box): (aabb min max)
(define (aabb min-point max-point)
  (list 'aabb min-point max-point))

(define (aabb? b)
  (and (pair? b) (eq? (car b) 'aabb)))

(define (aabb-min b) (cadr b))
(define (aabb-max b) (caddr b))

;;; aabb-center : AABB → Point3
(define (aabb-center box)
  (vec3-scale (vec3-add (aabb-min box) (aabb-max box)) 0.5))

;;; aabb-extents : AABB → Vec3
;;; Half-size in each dimension
(define (aabb-extents box)
  (vec3-scale (vec3-sub (aabb-max box) (aabb-min box)) 0.5))

;;; OBB (Oriented Bounding Box): (obb center axes extents)
;;; axes is list of 3 orthonormal vectors
;;; extents is vec3 of half-sizes along each axis
(define (obb center axes extents)
  (list 'obb center axes extents))

(define (obb? b)
  (and (pair? b) (eq? (car b) 'obb)))

(define (obb-center b) (cadr b))
(define (obb-axes b) (caddr b))
(define (obb-extents b) (cadddr b))

;;; ============================================================
;;; Transformations
;;; ============================================================

;;; transform4x4 : Matrix 4×4 representing homogeneous transformation
;;; We use the matrix module's representation

;;; transform-identity : → Matrix
(define (transform-identity)
  (matrix-from-lists '((1 0 0 0)
                       (0 1 0 0)
                       (0 0 1 0)
                       (0 0 0 1))))

;;; transform-translation : Vec3 → Matrix
(define (transform-translation v)
  (matrix-from-lists `((1 0 0 ,(vec3-x v))
                       (0 1 0 ,(vec3-y v))
                       (0 0 1 ,(vec3-z v))
                       (0 0 0 1))))

;;; transform-scale : Number | Vec3 → Matrix
;;; Uniform scale if number, non-uniform if vec3
(define (transform-scale s)
  (if (number? s)
      (matrix-from-lists `((,s 0 0 0)
                           (0 ,s 0 0)
                           (0 0 ,s 0)
                           (0 0 0 1)))
      (matrix-from-lists `((,(vec3-x s) 0 0 0)
                           (0 ,(vec3-y s) 0 0)
                           (0 0 ,(vec3-z s) 0)
                           (0 0 0 1)))))

;;; transform-rotation-x : Number → Matrix
;;; Rotation around x-axis by angle (radians)
(define (transform-rotation-x angle)
  (let ([c (cos angle)]
        [s (sin angle)])
       (matrix-from-lists `((1 0 0 0)
                            (0 ,c ,(- s) 0)
                            (0 ,s ,c 0)
                            (0 0 0 1)))))

;;; transform-rotation-y : Number → Matrix
(define (transform-rotation-y angle)
  (let ([c (cos angle)]
        [s (sin angle)])
       (matrix-from-lists `((,c 0 ,s 0)
                            (0 1 0 0)
                            (,(- s) 0 ,c 0)
                            (0 0 0 1)))))

;;; transform-rotation-z : Number → Matrix
(define (transform-rotation-z angle)
  (let ([c (cos angle)]
        [s (sin angle)])
       (matrix-from-lists `((,c ,(- s) 0 0)
                            (,s ,c 0 0)
                            (0 0 1 0)
                            (0 0 0 1)))))

;;; transform-rotation-axis : Vec3 × Number → Matrix
;;; Rotation around arbitrary axis by angle (Rodriguez formula)
(define (transform-rotation-axis axis angle)
  (let* ([ax (vec3-normalize axis)]
         [x (vec3-x ax)]
         [y (vec3-y ax)]
         [z (vec3-z ax)]
         [c (cos angle)]
         [s (sin angle)]
         [t (- 1 c)])
        (matrix-from-lists
         `((,(+ (* t x x) c)      ,(- (* t x y) (* s z)) ,(+ (* t x z) (* s y)) 0)
           (,(+ (* t x y) (* s z)) ,(+ (* t y y) c)      ,(- (* t y z) (* s x)) 0)
           (,(- (* t x z) (* s y)) ,(+ (* t y z) (* s x)) ,(+ (* t z z) c)      0)
           (0                     0                     0                     1)))))

;;; transform-from-quaternion : Quaternion → Matrix
;;; Convert quaternion to 4x4 transformation matrix
(define (transform-from-quaternion q)
  (let* ([w (quat-w q)]
         [x (quat-x q)]
         [y (quat-y q)]
         [z (quat-z q)]
         [x2 (* x x)]
         [y2 (* y y)]
         [z2 (* z z)]
         [xy (* x y)]
         [xz (* x z)]
         [yz (* y z)]
         [wx (* w x)]
         [wy (* w y)]
         [wz (* w z)])
        (matrix-from-lists
         `((,(- 1 (* 2 (+ y2 z2))) ,(* 2 (- xy wz))       ,(* 2 (+ xz wy))       0)
           (,(* 2 (+ xy wz))       ,(- 1 (* 2 (+ x2 z2))) ,(* 2 (- yz wx))       0)
           (,(* 2 (- xz wy))       ,(* 2 (+ yz wx))       ,(- 1 (* 2 (+ x2 y2))) 0)
           (0                     0                     0                     1)))))

;;; transform-point : Matrix × Point3 → Point3
;;; Apply transformation to point (with translation)
(define (transform-point mat p)
  (let ([x (vec3-x p)]
        [y (vec3-y p)]
        [z (vec3-z p)])
       (let ([nx (+ (* (matrix-ref mat 0 0) x)
                    (* (matrix-ref mat 0 1) y)
                    (* (matrix-ref mat 0 2) z)
                    (matrix-ref mat 0 3))]
             [ny (+ (* (matrix-ref mat 1 0) x)
                    (* (matrix-ref mat 1 1) y)
                    (* (matrix-ref mat 1 2) z)
                    (matrix-ref mat 1 3))]
             [nz (+ (* (matrix-ref mat 2 0) x)
                    (* (matrix-ref mat 2 1) y)
                    (* (matrix-ref mat 2 2) z)
                    (matrix-ref mat 2 3))]
             [nw (+ (* (matrix-ref mat 3 0) x)
                    (* (matrix-ref mat 3 1) y)
                    (* (matrix-ref mat 3 2) z)
                    (matrix-ref mat 3 3))])
            (cond
             [(< (abs nw) 1e-10)
              (list 'error 'transform-point "Point transformed to infinity (w=0)")]
             [(= nw 1)
              (vec3 nx ny nz)]
             [else
              (vec3 (/ nx nw) (/ ny nw) (/ nz nw))]))))

;;; transform-vector : Matrix × Vec3 → Vec3
;;; Apply transformation to vector (no translation)
(define (transform-vector mat v)
  (let ([x (vec3-x v)]
        [y (vec3-y v)]
        [z (vec3-z v)])
       (vec3 (+ (* (matrix-ref mat 0 0) x)
                (* (matrix-ref mat 0 1) y)
                (* (matrix-ref mat 0 2) z))
             (+ (* (matrix-ref mat 1 0) x)
                (* (matrix-ref mat 1 1) y)
                (* (matrix-ref mat 1 2) z))
             (+ (* (matrix-ref mat 2 0) x)
                (* (matrix-ref mat 2 1) y)
                (* (matrix-ref mat 2 2) z)))))

;;; ============================================================
;;; Distance Calculations
;;; ============================================================

;;; distance-point-point : Point3 × Point3 → Number
(define (distance-point-point p1 p2)
  (vec3-length (vec3-sub p2 p1)))

;;; distance-point-plane : Point3 × Plane3 → Number
;;; Signed distance (positive = in front of plane)
(define (distance-point-plane point plane)
  (+ (vec3-dot (plane3-normal plane) point)
     (plane3-d plane)))

;;; distance-point-line : Point3 × Line3 → Number
(define (distance-point-line point line)
  (let* ([origin (line3-origin line)]
         [dir (vec3-normalize (line3-direction line))]
         [v (vec3-sub point origin)]
         [proj (vec3-scale dir (vec3-dot v dir))]
         [perp (vec3-sub v proj)])
        (vec3-length perp)))

;;; distance-point-sphere : Point3 × Sphere → Number
;;; Negative if inside
(define (distance-point-sphere point sphere)
  (- (distance-point-point point (sphere-center sphere))
     (sphere-radius sphere)))

;;; ============================================================
;;; Intersection Tests
;;; ============================================================

;;; intersect-ray-plane : Ray3 × Plane3 → Number | #f
;;; Returns t parameter if intersects, #f otherwise
(define (intersect-ray-plane ray plane)
  (let* ([origin (ray3-origin ray)]
         [dir (ray3-direction ray)]
         [normal (plane3-normal plane)]
         [denom (vec3-dot normal dir)])
        (if (< (abs denom) 1e-10)
            #f  ; Ray parallel to plane
            (let ([t (/ (- (+ (vec3-dot normal origin) (plane3-d plane)))
                        denom)])
                 (if (>= t 0) t #f)))))

;;; intersect-ray-sphere : Ray3 × Sphere → (Number Number) | #f
;;; Returns (t1 t2) if intersects, #f otherwise
(define (intersect-ray-sphere ray sphere)
  (let* ([origin (ray3-origin ray)]
         [dir (ray3-direction ray)]
         [center (sphere-center sphere)]
         [radius (sphere-radius sphere)]
         [oc (vec3-sub origin center)]
         [a (vec3-dot dir dir)]
         [b (* 2 (vec3-dot oc dir))]
         [c (- (vec3-dot oc oc) (* radius radius))]
         [discriminant (- (* b b) (* 4 a c))])
        (if (< discriminant 0)
            #f
            (let* ([sqrt-d (sqrt discriminant)]
                   [t1 (/ (- (- b) sqrt-d) (* 2 a))]
                   [t2 (/ (+ (- b) sqrt-d) (* 2 a))])
                  (if (or (>= t1 0) (>= t2 0))
                      (list t1 t2)
                      #f)))))

;;; intersect-ray-aabb : Ray3 × AABB → (Number Number) | #f
;;; Returns (tmin tmax) if intersects, #f otherwise
(define (intersect-ray-aabb ray box)
  (let* ([origin (ray3-origin ray)]
         [dir (ray3-direction ray)]
         [bmin (aabb-min box)]
         [bmax (aabb-max box)])
        ; Helper to compute t-range for one axis
        (define (slab-t dir-comp origin-comp box-min box-max)
          (if (< (abs dir-comp) 1e-10)
              ; Ray parallel to slab - check if origin is within slab
              (if (and (>= origin-comp box-min) (<= origin-comp box-max))
                  (list -1e10 1e10)  ; Effectively infinite range
                  (list 1 -1))       ; Invalid range (will fail intersection)
              ; Normal case
              (let* ([inv-dir (/ 1.0 dir-comp)]
                     [t1 (* (- box-min origin-comp) inv-dir)]
                     [t2 (* (- box-max origin-comp) inv-dir)])
                    (if (< t1 t2)
                        (list t1 t2)
                        (list t2 t1)))))
        
        (let* ([x-range (slab-t (vec3-x dir) (vec3-x origin) (vec3-x bmin) (vec3-x bmax))]
               [y-range (slab-t (vec3-y dir) (vec3-y origin) (vec3-y bmin) (vec3-y bmax))]
               [z-range (slab-t (vec3-z dir) (vec3-z origin) (vec3-z bmin) (vec3-z bmax))]
               [tmin (max (car x-range) (car y-range) (car z-range))]
               [tmax (min (cadr x-range) (cadr y-range) (cadr z-range))])
              (if (and (<= tmin tmax) (>= tmax 0))
                  (list tmin tmax)
                  #f))))

;;; intersect-ray-triangle : Ray3 × Triangle3 → Number | #f
;;; Möller-Trumbore algorithm
(define (intersect-ray-triangle ray tri)
  (let* ([origin (ray3-origin ray)]
         [dir (ray3-direction ray)]
         [v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         [edge1 (vec3-sub v1 v0)]
         [edge2 (vec3-sub v2 v0)]
         [h (vec3-cross dir edge2)]
         [a (vec3-dot edge1 h)])
        (if (< (abs a) 1e-10)
            #f  ; Ray parallel to triangle
            (let* ([f (/ 1.0 a)]
                   [s (vec3-sub origin v0)]
                   [u (* f (vec3-dot s h))])
                  (if (or (< u 0.0) (> u 1.0))
                      #f
                      (let* ([q (vec3-cross s edge1)]
                             [v (* f (vec3-dot dir q))])
                            (if (or (< v 0.0) (> (+ u v) 1.0))
                                #f
                                (let ([t (* f (vec3-dot edge2 q))])
                                     (if (> t 1e-10) t #f)))))))))

;;; ============================================================
;;; Containment Tests
;;; ============================================================

;;; point-in-sphere? : Point3 × Sphere → Boolean
(define (point-in-sphere? point sphere)
  (<= (distance-point-sphere point sphere) 0))

;;; point-in-aabb? : Point3 × AABB → Boolean
(define (point-in-aabb? point box)
  (let ([p point]
        [bmin (aabb-min box)]
        [bmax (aabb-max box)])
       (and (>= (vec3-x p) (vec3-x bmin))
            (<= (vec3-x p) (vec3-x bmax))
            (>= (vec3-y p) (vec3-y bmin))
            (<= (vec3-y p) (vec3-y bmax))
            (>= (vec3-z p) (vec3-z bmin))
            (<= (vec3-z p) (vec3-z bmax)))))

;;; point-in-triangle? : Point3 × Triangle3 → Boolean
;;; Point must be coplanar with triangle
(define (point-in-triangle? point tri)
  (let* ([v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         [bary (barycentric-coords point v0 v1 v2)]
         [u (car bary)]
         [v (cadr bary)]
         [w (caddr bary)])
        (and (>= u 0) (>= v 0) (>= w 0)
             (<= (+ u v w) 1.0001))))  ; Small epsilon for floating point

;;; ============================================================
;;; Closest Point Queries
;;; ============================================================

;;; closest-point-on-line : Point3 × Line3 → Point3
(define (closest-point-on-line point line)
  (let* ([origin (line3-origin line)]
         [dir (vec3-normalize (line3-direction line))]
         [v (vec3-sub point origin)]
         [t (vec3-dot v dir)])
        (vec3-add origin (vec3-scale dir t))))

;;; closest-point-on-plane : Point3 × Plane3 → Point3
(define (closest-point-on-plane point plane)
  (let ([dist (distance-point-plane point plane)]
        [normal (plane3-normal plane)])
       (vec3-sub point (vec3-scale normal dist))))

;;; closest-point-on-aabb : Point3 × AABB → Point3
(define (closest-point-on-aabb point box)
  (let ([bmin (aabb-min box)]
        [bmax (aabb-max box)])
       (vec3 (max (vec3-x bmin) (min (vec3-x point) (vec3-x bmax)))
             (max (vec3-y bmin) (min (vec3-y point) (vec3-y bmax)))
             (max (vec3-z bmin) (min (vec3-z point) (vec3-z bmax))))))

;;; ============================================================
;;; Area and Volume Calculations
;;; ============================================================

;;; triangle-area : Triangle3 → Number
(define (triangle-area tri)
  (let* ([v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         [edge1 (vec3-sub v1 v0)]
         [edge2 (vec3-sub v2 v0)]
         [cross (vec3-cross edge1 edge2)])
        (* 0.5 (vec3-length cross))))

;;; sphere-volume : Sphere → Number
(define (sphere-volume sphere)
  (let ([r (sphere-radius sphere)])
       (* (/ 4.0 3.0) 3.141592653589793 r r r)))

;;; sphere-surface-area : Sphere → Number
(define (sphere-surface-area sphere)
  (let ([r (sphere-radius sphere)])
       (* 4.0 3.141592653589793 r r)))

;;; aabb-volume : AABB → Number
(define (aabb-volume box)
  (let* ([extents (aabb-extents box)]
         [x (vec3-x extents)]
         [y (vec3-y extents)]
         [z (vec3-z extents)])
        (* 8 x y z)))  ; extents are half-sizes

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; barycentric-coords : Point3 × Point3 × Point3 × Point3 → (Number Number Number)
;;; Compute barycentric coordinates (u, v, w) of point p with respect to triangle (a, b, c)
;;; p = u*a + v*b + w*c where u + v + w = 1
(define (barycentric-coords p a b c)
  (let* ([v0 (vec3-sub b a)]
         [v1 (vec3-sub c a)]
         [v2 (vec3-sub p a)]
         [d00 (vec3-dot v0 v0)]
         [d01 (vec3-dot v0 v1)]
         [d11 (vec3-dot v1 v1)]
         [d20 (vec3-dot v2 v0)]
         [d21 (vec3-dot v2 v1)]
         [denom (- (* d00 d11) (* d01 d01))])
        (if (< (abs denom) 1e-10)
            (list 1.0 0.0 0.0)  ; Degenerate triangle
            (let* ([v (/ (- (* d11 d20) (* d01 d21)) denom)]
                   [w (/ (- (* d00 d21) (* d01 d20)) denom)]
                   [u (- 1.0 v w)])
                  (list u v w)))))

;;; triangle-normal : Triangle3 → Vec3
;;; Compute face normal (counter-clockwise winding)
(define (triangle-normal tri)
  (let* ([v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         [edge1 (vec3-sub v1 v0)]
         [edge2 (vec3-sub v2 v0)])
        (vec3-normalize (vec3-cross edge1 edge2))))

;;; aabb-merge : AABB × AABB → AABB
;;; Compute minimal AABB containing both boxes
(define (aabb-merge b1 b2)
  (let ([min1 (aabb-min b1)]
        [max1 (aabb-max b1)]
        [min2 (aabb-min b2)]
        [max2 (aabb-max b2)])
       (aabb (vec3 (min (vec3-x min1) (vec3-x min2))
                   (min (vec3-y min1) (vec3-y min2))
                   (min (vec3-z min1) (vec3-z min2)))
             (vec3 (max (vec3-x max1) (vec3-x max2))
                   (max (vec3-y max1) (vec3-y max2))
                   (max (vec3-z max1) (vec3-z max2))))))

;;; aabb-from-points : (List Point3) → AABB
;;; Compute minimal AABB containing all points
;;; Returns a "null" AABB (min > max) for empty input, which merges correctly
(define (aabb-from-points points)
  (if (null? points)
      ;; Empty AABB: min=+inf, max=-inf
      ;; This ensures correct merging: any real point will establish proper bounds
      (aabb (vec3 +inf.0 +inf.0 +inf.0)
            (vec3 -inf.0 -inf.0 -inf.0))
      (let loop ([pts (cdr points)]
                 [min-p (car points)]
                 [max-p (car points)])
           (if (null? pts)
               (aabb min-p max-p)
               (let ([p (car pts)])
                    (loop (cdr pts)
                          (vec3 (min (vec3-x min-p) (vec3-x p))
                                (min (vec3-y min-p) (vec3-y p))
                                (min (vec3-z min-p) (vec3-z p)))
                          (vec3 (max (vec3-x max-p) (vec3-x p))
                                (max (vec3-y max-p) (vec3-y p))
                                (max (vec3-z max-p) (vec3-z p)))))))))

;;; ============================================================
;;; Coordinate System Conversions
;;; ============================================================

;;; vec3-to-spherical : Vec3 → (Number Number Number)
;;; Convert Cartesian to spherical (r, θ, φ)
(define (vec3-to-spherical v)
  (let* ([x (vec3-x v)]
         [y (vec3-y v)]
         [z (vec3-z v)]
         [r (vec3-length v)]
         [theta (if (= r 0) 0 (acos (/ z r)))]
         [phi (atan y x)])
        (list r theta phi)))

;;; vec3-to-cylindrical : Vec3 → (Number Number Number)
;;; Convert Cartesian to cylindrical (r, θ, z)
(define (vec3-to-cylindrical v)
  (let* ([x (vec3-x v)]
         [y (vec3-y v)]
         [z (vec3-z v)]
         [r (sqrt (+ (* x x) (* y y)))]
         [theta (atan y x)])
        (list r theta z)))
