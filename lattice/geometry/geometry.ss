(load "core/base/prelude.ss")
(load "lattice/linalg/vec3.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/quaternion.ss")

(doc 'module 'geometry)
(doc 'description "Geometric primitives and transformations for graphics and physics")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'provides "1. Geometric primitives (points, lines, rays, planes, circles, spheres, boxes)
2. Transformation matrices (translation, rotation, scaling)
3. Geometric operations (distance, intersection, containment)
4. Utilities (coordinate conversions, normals, barycentric)")

(doc 'section 'primitives)

(doc point3 'type '(-> Number Number Number Point3))
(doc point3 'description "Point3 is just a Vec3")
(define point3 vec3)
(doc 'export #t)

(define (line3 origin direction)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Vec3 Line3))
  (doc 'description "Infinite line through origin in direction")
  (list 'line3 origin direction))

(define (line3? l)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? l) (eq? (car l) 'line3)))

(define (line3-origin l)
  (doc 'export #t)
  (doc 'type '(-> Line3 Vec3))
  (cadr l))

(define (line3-direction l)
  (doc 'export #t)
  (doc 'type '(-> Line3 Vec3))
  (caddr l))

(define (ray3 origin direction)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Vec3 Ray3))
  (doc 'description "Half-infinite ray starting at origin going in direction")
  (list 'ray3 origin direction))

(define (ray3? r)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? r) (eq? (car r) 'ray3)))

(define (ray3-origin r)
  (doc 'export #t)
  (doc 'type '(-> Ray3 Vec3))
  (cadr r))

(define (ray3-direction r)
  (doc 'export #t)
  (doc 'type '(-> Ray3 Vec3))
  (caddr r))

(define (ray3-point-at ray t)
  (doc 'export #t)
  (doc 'type '(-> Ray3 Number Point3))
  (doc 'description "Get point at parameter t along ray (t >= 0)")
  (vec3-add (ray3-origin ray)
            (vec3-scale (ray3-direction ray) t)))

(define (plane3 normal d)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Number Plane3))
  (doc 'description "Plane defined by normal·p + d = 0")
  (list 'plane3 normal d))

(define (plane3? p)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? p) (eq? (car p) 'plane3)))

(define (plane3-normal p)
  (doc 'export #t)
  (doc 'type '(-> Plane3 Vec3))
  (cadr p))

(define (plane3-d p)
  (doc 'export #t)
  (doc 'type '(-> Plane3 Number))
  (caddr p))

(define (plane3-from-point-normal point normal)
  (doc 'export #t)
  (doc 'type '(-> Point3 Vec3 Plane3))
  (doc 'description "Create plane from point and normal")
  (let ([d (- (vec3-dot normal point))])
       (plane3 normal d)))

(define (plane3-from-points p1 p2 p3)
  (doc 'export #t)
  (doc 'type '(-> Point3 Point3 Point3 (Or Plane3 Error)))
  (doc 'description "Create plane from three points")
  (doc 'note "Returns error if points are collinear")
  (let* ([v1 (vec3-sub p2 p1)]
         [v2 (vec3-sub p3 p1)]
         [cp (vec3-cross v1 v2)]
         [mag (vec3-magnitude cp)])
        (if (< mag 1e-10)
            (list 'error 'plane3-from-points "Collinear points do not define a unique plane")
            (let ([normal (vec3-scale-inv cp mag)])
                 (plane3-from-point-normal p1 normal)))))

(define (triangle3 p1 p2 p3)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Vec3 Vec3 Triangle3))
  (list 'triangle3 p1 p2 p3))

(define (triangle3? t)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? t) (eq? (car t) 'triangle3)))

(define (triangle3-p1 t)
  (doc 'export #t)
  (doc 'type '(-> Triangle3 Vec3))
  (cadr t))
(define (triangle3-p2 t)
  (doc 'export #t)
  (doc 'type '(-> Triangle3 Vec3))
  (caddr t))
(define (triangle3-p3 t)
  (doc 'export #t)
  (doc 'type '(-> Triangle3 Vec3))
  (cadddr t))

(define (circle center radius)
  (doc 'export #t)
  (doc 'type '(-> Vec2 Number Circle))
  (list 'circle center radius))

(define (circle? c)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? c) (eq? (car c) 'circle)))

(define (circle-center c)
  (doc 'export #t)
  (doc 'type '(-> Circle Vec2))
  (cadr c))
(define (circle-radius c)
  (doc 'export #t)
  (doc 'type '(-> Circle Number))
  (caddr c))

(define (sphere center radius)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Number Sphere))
  (list 'sphere center radius))

(define (sphere? s)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? s) (eq? (car s) 'sphere)))

(define (sphere-center s)
  (doc 'export #t)
  (doc 'type '(-> Sphere Vec3))
  (cadr s))
(define (sphere-radius s)
  (doc 'export #t)
  (doc 'type '(-> Sphere Number))
  (caddr s))

(define (aabb min-point max-point)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Vec3 AABB))
  (doc 'description "Axis-Aligned Bounding Box from min/max corners")
  (list 'aabb min-point max-point))

(define (aabb? b)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? b) (eq? (car b) 'aabb)))

(define (aabb-min b)
  (doc 'export #t)
  (doc 'type '(-> AABB Vec3))
  (cadr b))
(define (aabb-max b)
  (doc 'export #t)
  (doc 'type '(-> AABB Vec3))
  (caddr b))

(define (aabb-center box)
  (doc 'export #t)
  (doc 'type '(-> AABB Point3))
  (vec3-scale (vec3-add (aabb-min box) (aabb-max box)) 0.5))

(define (aabb-extents box)
  (doc 'export #t)
  (doc 'type '(-> AABB Vec3))
  (doc 'description "Half-size in each dimension")
  (vec3-scale (vec3-sub (aabb-max box) (aabb-min box)) 0.5))

(define (obb center axes extents)
  (doc 'export #t)
  (doc 'type '(-> Vec3 (List Vec3) Vec3 OBB))
  (doc 'description "Oriented Bounding Box from center, axes, and extents")
  (doc 'param 'axes "list of 3 orthonormal vectors")
  (doc 'param 'extents "vec3 of half-sizes along each axis")
  (list 'obb center axes extents))

(define (obb? b)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? b) (eq? (car b) 'obb)))

(define (obb-center b)
  (doc 'export #t)
  (doc 'type '(-> OBB Vec3))
  (cadr b))
(define (obb-axes b)
  (doc 'export #t)
  (doc 'type '(-> OBB (List Vec3)))
  (caddr b))
(define (obb-extents b)
  (doc 'export #t)
  (doc 'type '(-> OBB Vec3))
  (cadddr b))

(doc 'section 'transformations)

(doc 'note "transform4x4: Matrix 4×4 representing homogeneous transformation")
(doc 'note "We use the matrix module's representation")

(define (transform-identity)
  (doc 'export #t)
  (doc 'type '(-> Matrix))
  (matrix-from-lists '((1 0 0 0)
                       (0 1 0 0)
                       (0 0 1 0)
                       (0 0 0 1))))

(define (transform-translation v)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Matrix))
  (matrix-from-lists `((1 0 0 ,(vec3-x v))
                       (0 1 0 ,(vec3-y v))
                       (0 0 1 ,(vec3-z v))
                       (0 0 0 1))))

(define (transform-scale s)
  (doc 'export #t)
  (doc 'type '(-> (Or Number Vec3) Matrix))
  (doc 'description "Uniform scale if number, non-uniform if vec3")
  (if (number? s)
      (matrix-from-lists `((,s 0 0 0)
                           (0 ,s 0 0)
                           (0 0 ,s 0)
                           (0 0 0 1)))
      (matrix-from-lists `((,(vec3-x s) 0 0 0)
                           (0 ,(vec3-y s) 0 0)
                           (0 0 ,(vec3-z s) 0)
                           (0 0 0 1)))))

(define (transform-rotation-x angle)
  (doc 'export #t)
  (doc 'type '(-> Number Matrix))
  (doc 'description "Rotation around x-axis by angle (radians)")
  (let ([c (cos angle)]
        [s (sin angle)])
       (matrix-from-lists `((1 0 0 0)
                            (0 ,c ,(- s) 0)
                            (0 ,s ,c 0)
                            (0 0 0 1)))))

(define (transform-rotation-y angle)
  (doc 'export #t)
  (doc 'type '(-> Number Matrix))
  (let ([c (cos angle)]
        [s (sin angle)])
       (matrix-from-lists `((,c 0 ,s 0)
                            (0 1 0 0)
                            (,(- s) 0 ,c 0)
                            (0 0 0 1)))))

(define (transform-rotation-z angle)
  (doc 'export #t)
  (doc 'type '(-> Number Matrix))
  (let ([c (cos angle)]
        [s (sin angle)])
       (matrix-from-lists `((,c ,(- s) 0 0)
                            (,s ,c 0 0)
                            (0 0 1 0)
                            (0 0 0 1)))))

(define (transform-rotation-axis axis angle)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Number Matrix))
  (doc 'description "Rotation around arbitrary axis by angle (Rodriguez formula)")
  (doc 'note "Returns identity matrix if axis is zero vector")
  (let ([mag (vec3-magnitude axis)])
       (if (< mag 1e-10)
           ; Zero axis: return identity transformation
           (transform-identity)
           (let* ([ax (vec3-scale-inv axis mag)]
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
                    (0                     0                     0                     1)))))))

(define (transform-from-quaternion q)
  (doc 'export #t)
  (doc 'type '(-> Quaternion Matrix))
  (doc 'description "Convert quaternion to 4x4 transformation matrix")
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

(define (transform-point mat p)
  (doc 'export #t)
  (doc 'type '(-> Matrix Point3 Point3))
  (doc 'description "Apply transformation to point (with translation)")
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

(define (transform-vector mat v)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec3 Vec3))
  (doc 'description "Apply transformation to vector (no translation)")
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

(doc 'section 'distance-calculations)

(define (distance-point-point p1 p2)
  (doc 'export #t)
  (doc 'type '(-> Point3 Point3 Number))
  (vec3-length (vec3-sub p2 p1)))

(define (distance-point-plane point plane)
  (doc 'export #t)
  (doc 'type '(-> Point3 Plane3 Number))
  (doc 'description "Signed distance (positive = in front of plane)")
  (doc 'note "Handles non-unit normals correctly by dividing by magnitude")
  (let ([n (plane3-normal plane)]
        [d (plane3-d plane)])
       (/ (+ (vec3-dot n point) d)
          (vec3-magnitude n))))

(define (distance-point-line point line)
  (doc 'export #t)
  (doc 'type '(-> Point3 Line3 Number))
  (let* ([origin (line3-origin line)]
         [dir (vec3-normalize (line3-direction line))]
         [v (vec3-sub point origin)]
         [proj (vec3-scale dir (vec3-dot v dir))]
         [perp (vec3-sub v proj)])
        (vec3-length perp)))

(define (distance-point-sphere point sphere)
  (doc 'export #t)
  (doc 'type '(-> Point3 Sphere Number))
  (doc 'description "Negative if inside")
  (- (distance-point-point point (sphere-center sphere))
     (sphere-radius sphere)))

(doc 'section 'intersection-tests)

(define (intersect-ray-plane ray plane)
  (doc 'export #t)
  (doc 'type '(-> Ray3 Plane3 (Or Number #f)))
  (doc 'returns "t parameter if intersects, #f otherwise")
  (let* ([origin (ray3-origin ray)]
         [dir (ray3-direction ray)]
         [normal (plane3-normal plane)]
         [denom (vec3-dot normal dir)])
        (if (< (abs denom) 1e-10)
            #f  ; Ray parallel to plane
            (let ([t (/ (- (+ (vec3-dot normal origin) (plane3-d plane)))
                        denom)])
                 (if (>= t 0) t #f)))))

(define (intersect-ray-sphere ray sphere)
  (doc 'export #t)
  (doc 'type '(-> Ray3 Sphere (Or (List Number Number) #f)))
  (doc 'returns "(t1 t2) if intersects, #f otherwise")
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

(define (intersect-ray-aabb ray box)
  (doc 'export #t)
  (doc 'type '(-> Ray3 AABB (Or (List Number Number) #f)))
  (doc 'returns "(tmin tmax) if intersects, #f otherwise")
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

(define (intersect-ray-triangle ray tri)
  (doc 'export #t)
  (doc 'type '(-> Ray3 Triangle3 (Or Number #f)))
  (doc 'description "Möller-Trumbore algorithm")
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

(doc 'section 'containment-tests)

(define (point-in-sphere? point sphere)
  (doc 'export #t)
  (doc 'type '(-> Point3 Sphere Bool))
  (<= (distance-point-sphere point sphere) 0))

(define (point-in-aabb? point box)
  (doc 'export #t)
  (doc 'type '(-> Point3 AABB Bool))
  (let ([p point]
        [bmin (aabb-min box)]
        [bmax (aabb-max box)])
       (and (>= (vec3-x p) (vec3-x bmin))
            (<= (vec3-x p) (vec3-x bmax))
            (>= (vec3-y p) (vec3-y bmin))
            (<= (vec3-y p) (vec3-y bmax))
            (>= (vec3-z p) (vec3-z bmin))
            (<= (vec3-z p) (vec3-z bmax)))))

(define (point-in-triangle? point tri)
  (doc 'export #t)
  (doc 'type '(-> Point3 Triangle3 Bool))
  (doc 'note "Point must be coplanar with triangle")
  (let* ([v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         [bary (barycentric-coords point v0 v1 v2)]
         [u (car bary)]
         [v (cadr bary)]
         [w (caddr bary)])
        (and (>= u 0) (>= v 0) (>= w 0)
             (<= (+ u v w) 1.0001))))  ; Small epsilon for floating point

(doc 'section 'closest-point-queries)

(define (closest-point-on-line point line)
  (doc 'export #t)
  (doc 'type '(-> Point3 Line3 Point3))
  (let* ([origin (line3-origin line)]
         [dir (vec3-normalize (line3-direction line))]
         [v (vec3-sub point origin)]
         [t (vec3-dot v dir)])
        (vec3-add origin (vec3-scale dir t))))

(define (closest-point-on-plane point plane)
  (doc 'export #t)
  (doc 'type '(-> Point3 Plane3 Point3))
  (let ([dist (distance-point-plane point plane)]
        [normal (plane3-normal plane)])
       (vec3-sub point (vec3-scale normal dist))))

(define (closest-point-on-aabb point box)
  (doc 'export #t)
  (doc 'type '(-> Point3 AABB Point3))
  (let ([bmin (aabb-min box)]
        [bmax (aabb-max box)])
       (vec3 (max (vec3-x bmin) (min (vec3-x point) (vec3-x bmax)))
             (max (vec3-y bmin) (min (vec3-y point) (vec3-y bmax)))
             (max (vec3-z bmin) (min (vec3-z point) (vec3-z bmax))))))

(doc 'section 'area-and-volume)

(define (triangle-area tri)
  (doc 'export #t)
  (doc 'type '(-> Triangle3 Number))
  (let* ([v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         [edge1 (vec3-sub v1 v0)]
         [edge2 (vec3-sub v2 v0)]
         [cross (vec3-cross edge1 edge2)])
        (* 0.5 (vec3-length cross))))

(define (sphere-volume sphere)
  (doc 'export #t)
  (doc 'type '(-> Sphere Number))
  (let ([r (sphere-radius sphere)])
       (* (/ 4.0 3.0) 3.141592653589793 r r r)))

(define (sphere-surface-area sphere)
  (doc 'export #t)
  (doc 'type '(-> Sphere Number))
  (let ([r (sphere-radius sphere)])
       (* 4.0 3.141592653589793 r r)))

(define (aabb-volume box)
  (doc 'export #t)
  (doc 'type '(-> AABB Number))
  (let* ([extents (aabb-extents box)]
         [x (vec3-x extents)]
         [y (vec3-y extents)]
         [z (vec3-z extents)])
        (* 8 x y z)))  ; extents are half-sizes

(doc 'section 'utilities)

(define (barycentric-coords p a b c)
  (doc 'export #t)
  (doc 'type '(-> Point3 Point3 Point3 Point3 (List Number Number Number)))
  (doc 'description "Compute barycentric coordinates (u, v, w) of point p with respect to triangle (a, b, c)")
  (doc 'note "p = u*a + v*b + w*c where u + v + w = 1")
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

(define (triangle-normal tri)
  (doc 'export #t)
  (doc 'type '(-> Triangle3 Vec3))
  (doc 'description "Compute face normal (counter-clockwise winding)")
  (let* ([v0 (triangle3-p1 tri)]
         [v1 (triangle3-p2 tri)]
         [v2 (triangle3-p3 tri)]
         [edge1 (vec3-sub v1 v0)]
         [edge2 (vec3-sub v2 v0)])
        (vec3-normalize (vec3-cross edge1 edge2))))

(define (aabb-merge b1 b2)
  (doc 'export #t)
  (doc 'type '(-> AABB AABB AABB))
  (doc 'description "Compute minimal AABB containing both boxes")
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

(define (aabb-from-points points)
  (doc 'export #t)
  (doc 'type '(-> (List Point3) AABB))
  (doc 'description "Compute minimal AABB containing all points")
  (if (null? points)
      (aabb (vec3-zero) (vec3-zero))
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

(doc 'section 'coordinate-conversions)

(define (vec3-to-spherical v)
  (doc 'export #t)
  (doc 'type '(-> Vec3 (List Number Number Number)))
  (doc 'description "Convert Cartesian to spherical (r, θ, φ)")
  (let* ([x (vec3-x v)]
         [y (vec3-y v)]
         [z (vec3-z v)]
         [r (vec3-length v)]
         [theta (if (= r 0) 0 (acos (/ z r)))]
         [phi (atan y x)])
        (list r theta phi)))

(define (vec3-to-cylindrical v)
  (doc 'export #t)
  (doc 'type '(-> Vec3 (List Number Number Number)))
  (doc 'description "Convert Cartesian to cylindrical (r, θ, z)")
  (let* ([x (vec3-x v)]
         [y (vec3-y v)]
         [z (vec3-z v)]
         [r (sqrt (+ (* x x) (* y y)))]
         [theta (atan y x)])
        (list r theta z)))
