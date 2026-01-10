;;; core/linalg/vec3.ss — 3D Vector Math Library
;;;
;;; Pure, functional 3D vector operations for physics and graphics.
;;;
;;; A vec3 is represented as: (vec3 x y z)
;;; All operations are pure and return new vectors.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Vector Construction
;;; ============================================================

;;; vec3 : Number × Number × Number → Vec3
;;; Create a 3D vector.
(define (vec3 x y z)
  (list 'vec3 x y z))

;;; vec3-zero : → Vec3
;;; The zero vector (0, 0, 0).
(define (vec3-zero)
  (vec3 0 0 0))

;;; vec3-one : → Vec3
;;; The unit vector (1, 1, 1).
(define (vec3-one)
  (vec3 1 1 1))

;;; vec3-unit-x : → Vec3
;;; Unit vector in x direction (1, 0, 0).
(define (vec3-unit-x)
  (vec3 1 0 0))

;;; vec3-unit-y : → Vec3
;;; Unit vector in y direction (0, 1, 0).
(define (vec3-unit-y)
  (vec3 0 1 0))

;;; vec3-unit-z : → Vec3
;;; Unit vector in z direction (0, 0, 1).
(define (vec3-unit-z)
  (vec3 0 0 1))

;;; vec3-from-spherical : Number × Number × Number → Vec3
;;; Create vector from spherical coordinates (r, θ, φ).
;;; θ = polar angle from z-axis, φ = azimuthal angle from x-axis.
(define (vec3-from-spherical r theta phi)
  (let ([sin-theta (sin theta)])
       (vec3 (* r sin-theta (cos phi))
             (* r sin-theta (sin phi))
             (* r (cos theta)))))

;;; vec3-from-cylindrical : Number × Number × Number → Vec3
;;; Create vector from cylindrical coordinates (r, θ, z).
(define (vec3-from-cylindrical r theta z)
  (vec3 (* r (cos theta))
        (* r (sin theta))
        z))

;;; ============================================================
;;; Predicates
;;; ============================================================

;;; vec3? : Any → Boolean
(define (vec3? v)
  (and (pair? v) (eq? (car v) 'vec3)))

;;; vec3-zero? : Vec3 → Boolean
;;; Check if vector is (near) zero.
(define (vec3-zero? v)
  (and (< (abs (vec3-x v)) 1e-10)
       (< (abs (vec3-y v)) 1e-10)
       (< (abs (vec3-z v)) 1e-10)))

;;; vec3-unit? : Vec3 → Boolean
;;; Check if vector is (near) unit length.
(define (vec3-unit? v)
  (< (abs (- (vec3-magnitude-sq v) 1.0)) 1e-10))

;;; ============================================================
;;; Accessors
;;; ============================================================

;;; vec3-x : Vec3 → Number
(define (vec3-x v) (list-ref v 1))

;;; vec3-y : Vec3 → Number
(define (vec3-y v) (list-ref v 2))

;;; vec3-z : Vec3 → Number
(define (vec3-z v) (list-ref v 3))

;;; vec3->list : Vec3 → (List Number)
(define (vec3->list v)
  (list (vec3-x v) (vec3-y v) (vec3-z v)))

;;; list->vec3 : (List Number) → Vec3
(define (list->vec3 lst)
  (vec3 (car lst) (cadr lst) (caddr lst)))

;;; vec3-ref : Vec3 × Nat → Number
;;; Access component by index (0=x, 1=y, 2=z).
(define (vec3-ref v i)
  (list-ref v (+ i 1)))

;;; ============================================================
;;; Basic Arithmetic
;;; ============================================================

;;; vec3-add : Vec3 × Vec3 → Vec3
;;; Vector addition.
(define (vec3-add a b)
  (vec3 (+ (vec3-x a) (vec3-x b))
        (+ (vec3-y a) (vec3-y b))
        (+ (vec3-z a) (vec3-z b))))

;;; vec3-sub : Vec3 × Vec3 → Vec3
;;; Vector subtraction.
(define (vec3-sub a b)
  (vec3 (- (vec3-x a) (vec3-x b))
        (- (vec3-y a) (vec3-y b))
        (- (vec3-z a) (vec3-z b))))

;;; vec3-neg : Vec3 → Vec3
;;; Negate a vector.
(define (vec3-neg v)
  (vec3 (- (vec3-x v)) (- (vec3-y v)) (- (vec3-z v))))

;;; vec3-mul : Vec3 × Vec3 → Vec3
;;; Component-wise multiplication (Hadamard product).
(define (vec3-mul a b)
  (vec3 (* (vec3-x a) (vec3-x b))
        (* (vec3-y a) (vec3-y b))
        (* (vec3-z a) (vec3-z b))))

;;; vec3-div : Vec3 × Vec3 → Vec3
;;; Component-wise division.
(define (vec3-div a b)
  (vec3 (/ (vec3-x a) (vec3-x b))
        (/ (vec3-y a) (vec3-y b))
        (/ (vec3-z a) (vec3-z b))))

;;; vec3-scale : Vec3 × Number → Vec3
;;; Scalar multiplication.
(define (vec3-scale v s)
  (vec3 (* (vec3-x v) s)
        (* (vec3-y v) s)
        (* (vec3-z v) s)))

;;; vec3-scale-inv : Vec3 × Number → Vec3
;;; Scalar division (v / s).
(define (vec3-scale-inv v s)
  (vec3 (/ (vec3-x v) s)
        (/ (vec3-y v) s)
        (/ (vec3-z v) s)))

;;; ============================================================
;;; Products
;;; ============================================================

;;; vec3-dot : Vec3 × Vec3 → Number
;;; Dot product.
(define (vec3-dot a b)
  (+ (* (vec3-x a) (vec3-x b))
     (* (vec3-y a) (vec3-y b))
     (* (vec3-z a) (vec3-z b))))

;;; vec3-cross : Vec3 × Vec3 → Vec3
;;; Cross product.
(define (vec3-cross a b)
  (vec3 (- (* (vec3-y a) (vec3-z b)) (* (vec3-z a) (vec3-y b)))
        (- (* (vec3-z a) (vec3-x b)) (* (vec3-x a) (vec3-z b)))
        (- (* (vec3-x a) (vec3-y b)) (* (vec3-y a) (vec3-x b)))))

;;; vec3-triple-scalar : Vec3 × Vec3 × Vec3 → Number
;;; Scalar triple product: a · (b × c).
;;; Gives signed volume of parallelepiped.
(define (vec3-triple-scalar a b c)
  (vec3-dot a (vec3-cross b c)))

;;; vec3-triple-vector : Vec3 × Vec3 × Vec3 → Vec3
;;; Vector triple product: a × (b × c) = b(a·c) - c(a·b).
(define (vec3-triple-vector a b c)
  (vec3-sub (vec3-scale b (vec3-dot a c))
            (vec3-scale c (vec3-dot a b))))

;;; ============================================================
;;; Length and Distance
;;; ============================================================

;;; vec3-magnitude-sq : Vec3 → Number
;;; Squared magnitude (avoids sqrt).
(define (vec3-magnitude-sq v)
  (+ (* (vec3-x v) (vec3-x v))
     (* (vec3-y v) (vec3-y v))
     (* (vec3-z v) (vec3-z v))))

;;; vec3-magnitude : Vec3 → Number
;;; Vector magnitude (length).
(define (vec3-magnitude v)
  (sqrt (vec3-magnitude-sq v)))

;;; vec3-length : Vec3 → Number
;;; Alias for magnitude.
(define vec3-length vec3-magnitude)

;;; vec3-distance-sq : Vec3 × Vec3 → Number
;;; Squared distance between two points.
(define (vec3-distance-sq a b)
  (vec3-magnitude-sq (vec3-sub a b)))

;;; vec3-distance : Vec3 × Vec3 → Number
;;; Euclidean distance between two points.
(define (vec3-distance a b)
  (sqrt (vec3-distance-sq a b)))

;;; ============================================================
;;; Normalization
;;; ============================================================

;;; vec3-normalize : Vec3 → Vec3
;;; Return unit vector in same direction.
;;; Returns zero vector if input is zero.
(define (vec3-normalize v)
  (let ([mag (vec3-magnitude v)])
       (if (< mag 1e-10)
           (vec3-zero)
           (vec3-scale-inv v mag))))

;;; vec3-normalize-or : Vec3 × Vec3 → Vec3
;;; Normalize, or return default if zero.
(define (vec3-normalize-or v default)
  (let ([mag (vec3-magnitude v)])
       (if (< mag 1e-10)
           default
           (vec3-scale-inv v mag))))

;;; vec3-set-magnitude : Vec3 × Number → Vec3
;;; Scale vector to have given magnitude.
(define (vec3-set-magnitude v mag)
  (vec3-scale (vec3-normalize v) mag))

;;; ============================================================
;;; Interpolation
;;; ============================================================

;;; vec3-lerp : Vec3 × Vec3 × Number → Vec3
;;; Linear interpolation between a and b.
;;; t=0 returns a, t=1 returns b.
(define (vec3-lerp a b t)
  (vec3-add (vec3-scale a (- 1 t))
            (vec3-scale b t)))

;;; vec3-slerp : Vec3 × Vec3 × Number → Vec3
;;; Spherical linear interpolation for unit vectors.
;;; Interpolates along the great circle.
(define (vec3-slerp a b t)
  (let* ([dot (vec3-dot a b)]
         [dot-clamped (max -1.0 (min 1.0 dot))]
         [theta (acos dot-clamped)]
         [sin-theta (sin theta)])
        (if (< (abs sin-theta) 1e-10)
            ;; Vectors are parallel, use lerp
            (vec3-normalize (vec3-lerp a b t))
            (let ([s0 (/ (sin (* (- 1 t) theta)) sin-theta)]
                  [s1 (/ (sin (* t theta)) sin-theta)])
                 (vec3-add (vec3-scale a s0)
                           (vec3-scale b s1))))))

;;; vec3-nlerp : Vec3 × Vec3 × Number → Vec3
;;; Normalized linear interpolation (cheaper than slerp).
(define (vec3-nlerp a b t)
  (vec3-normalize (vec3-lerp a b t)))

;;; ============================================================
;;; Projection and Reflection
;;; ============================================================

;;; vec3-project : Vec3 × Vec3 → Vec3
;;; Project a onto b.
(define (vec3-project a b)
  (let ([b-mag-sq (vec3-magnitude-sq b)])
       (if (< b-mag-sq 1e-10)
           (vec3-zero)
           (vec3-scale b (/ (vec3-dot a b) b-mag-sq)))))

;;; vec3-reject : Vec3 × Vec3 → Vec3
;;; Rejection of a from b (perpendicular component).
(define (vec3-reject a b)
  (vec3-sub a (vec3-project a b)))

;;; vec3-reflect : Vec3 × Vec3 → Vec3
;;; Reflect v across normal n (n should be unit).
(define (vec3-reflect v n)
  (vec3-sub v (vec3-scale n (* 2 (vec3-dot v n)))))

;;; vec3-refract : Vec3 × Vec3 × Number → Vec3 | #f
;;; Refract v through surface with normal n and ratio eta.
;;; Returns #f for total internal reflection.
(define (vec3-refract v n eta)
  (let* ([cos-i (- (vec3-dot v n))]
         [sin2-t (* eta eta (- 1 (* cos-i cos-i)))])
        (if (> sin2-t 1.0)
            #f  ; Total internal reflection
            (let ([cos-t (sqrt (- 1 sin2-t))])
                 (vec3-add (vec3-scale v eta)
                           (vec3-scale n (- (* eta cos-i) cos-t)))))))

;;; ============================================================
;;; Angles
;;; ============================================================

;;; vec3-angle : Vec3 × Vec3 → Number
;;; Angle between two vectors in radians.
(define (vec3-angle a b)
  (let* ([dot (vec3-dot a b)]
         [mags (* (vec3-magnitude a) (vec3-magnitude b))])
        (if (< mags 1e-10)
            0
            (acos (max -1.0 (min 1.0 (/ dot mags)))))))

;;; vec3-signed-angle : Vec3 × Vec3 × Vec3 → Number
;;; Signed angle from a to b around axis.
(define (vec3-signed-angle a b axis)
  (let* ([cross (vec3-cross a b)]
         [angle (vec3-angle a b)]
         [sign (if (< (vec3-dot cross axis) 0) -1 1)])
        (* sign angle)))

;;; ============================================================
;;; Rotation
;;; ============================================================

;;; vec3-rotate-x : Vec3 × Number → Vec3
;;; Rotate around x-axis by angle (radians).
(define (vec3-rotate-x v angle)
  (let ([c (cos angle)]
        [s (sin angle)]
        [y (vec3-y v)]
        [z (vec3-z v)])
       (vec3 (vec3-x v)
             (- (* c y) (* s z))
             (+ (* s y) (* c z)))))

;;; vec3-rotate-y : Vec3 × Number → Vec3
;;; Rotate around y-axis by angle (radians).
(define (vec3-rotate-y v angle)
  (let ([c (cos angle)]
        [s (sin angle)]
        [x (vec3-x v)]
        [z (vec3-z v)])
       (vec3 (+ (* c x) (* s z))
             (vec3-y v)
             (- (* c z) (* s x)))))

;;; vec3-rotate-z : Vec3 × Number → Vec3
;;; Rotate around z-axis by angle (radians).
(define (vec3-rotate-z v angle)
  (let ([c (cos angle)]
        [s (sin angle)]
        [x (vec3-x v)]
        [y (vec3-y v)])
       (vec3 (- (* c x) (* s y))
             (+ (* s x) (* c y))
             (vec3-z v))))

;;; vec3-rotate-axis : Vec3 × Vec3 × Number → Vec3
;;; Rotate v around arbitrary axis by angle (Rodrigues' rotation).
;;; Axis should be a unit vector.
(define (vec3-rotate-axis v axis angle)
  (let* ([c (cos angle)]
         [s (sin angle)]
         [k axis]
         ;; v_rot = v*cos(θ) + (k × v)*sin(θ) + k*(k·v)*(1-cos(θ))
         [term1 (vec3-scale v c)]
         [term2 (vec3-scale (vec3-cross k v) s)]
         [term3 (vec3-scale k (* (vec3-dot k v) (- 1 c)))])
        (vec3-add (vec3-add term1 term2) term3)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; vec3-min : Vec3 × Vec3 → Vec3
;;; Component-wise minimum.
(define (vec3-min a b)
  (vec3 (min (vec3-x a) (vec3-x b))
        (min (vec3-y a) (vec3-y b))
        (min (vec3-z a) (vec3-z b))))

;;; vec3-max : Vec3 × Vec3 → Vec3
;;; Component-wise maximum.
(define (vec3-max a b)
  (vec3 (max (vec3-x a) (vec3-x b))
        (max (vec3-y a) (vec3-y b))
        (max (vec3-z a) (vec3-z b))))

;;; vec3-abs : Vec3 → Vec3
;;; Component-wise absolute value.
(define (vec3-abs v)
  (vec3 (abs (vec3-x v))
        (abs (vec3-y v))
        (abs (vec3-z v))))

;;; vec3-floor : Vec3 → Vec3
;;; Component-wise floor.
(define (vec3-floor v)
  (vec3 (floor (vec3-x v))
        (floor (vec3-y v))
        (floor (vec3-z v))))

;;; vec3-ceil : Vec3 → Vec3
;;; Component-wise ceiling.
(define (vec3-ceil v)
  (vec3 (ceiling (vec3-x v))
        (ceiling (vec3-y v))
        (ceiling (vec3-z v))))

;;; vec3-clamp : Vec3 × Vec3 × Vec3 → Vec3
;;; Clamp each component between min and max.
(define (vec3-clamp v vmin vmax)
  (vec3 (max (vec3-x vmin) (min (vec3-x v) (vec3-x vmax)))
        (max (vec3-y vmin) (min (vec3-y v) (vec3-y vmax)))
        (max (vec3-z vmin) (min (vec3-z v) (vec3-z vmax)))))

;;; vec3-clamp-magnitude : Vec3 × Number × Number → Vec3
;;; Clamp vector magnitude between min and max.
(define (vec3-clamp-magnitude v min-mag max-mag)
  (let ([mag (vec3-magnitude v)])
       (cond
        [(< mag min-mag) (vec3-set-magnitude v min-mag)]
        [(> mag max-mag) (vec3-set-magnitude v max-mag)]
        [else v])))

;;; ============================================================
;;; Basis and Orthogonalization
;;; ============================================================

;;; vec3-orthonormal-basis : Vec3 → (Vec3 × Vec3 × Vec3)
;;; Create orthonormal basis with v as first vector.
;;; Returns (tangent1, tangent2, normal) where normal ≈ v.
(define (vec3-orthonormal-basis v)
  (let* ([n (vec3-normalize v)]
         ;; Choose a vector not parallel to n
         [up (if (< (abs (vec3-y n)) 0.9)
                 (vec3 0 1 0)
                 (vec3 1 0 0))]
         [t1 (vec3-normalize (vec3-cross n up))]
         [t2 (vec3-cross n t1)])
        (list t1 t2 n)))

;;; vec3-gram-schmidt : Vec3 × Vec3 → (Vec3 × Vec3)
;;; Orthogonalize v2 with respect to v1.
;;; Returns (v1-normalized, v2-orthonormalized).
(define (vec3-gram-schmidt v1 v2)
  (let* ([u1 (vec3-normalize v1)]
         [u2 (vec3-normalize (vec3-reject v2 u1))])
        (list u1 u2)))

;;; ============================================================
;;; Coordinate Conversions
;;; ============================================================

;;; vec3->spherical : Vec3 → (r × θ × φ)
;;; Convert to spherical coordinates.
(define (vec3->spherical v)
  (let* ([x (vec3-x v)]
         [y (vec3-y v)]
         [z (vec3-z v)]
         [r (vec3-magnitude v)]
         [theta (if (< r 1e-10) 0 (acos (/ z r)))]
         [phi (atan y x)])
        (list r theta phi)))

;;; vec3->cylindrical : Vec3 → (r × θ × z)
;;; Convert to cylindrical coordinates.
(define (vec3->cylindrical v)
  (let* ([x (vec3-x v)]
         [y (vec3-y v)]
         [r (sqrt (+ (* x x) (* y y)))]
         [theta (atan y x)])
        (list r theta (vec3-z v))))

;;; ============================================================
;;; Comparison
;;; ============================================================

;;; vec3-equal? : Vec3 × Vec3 → Boolean
;;; Exact equality.
(define (vec3-equal? a b)
  (and (= (vec3-x a) (vec3-x b))
       (= (vec3-y a) (vec3-y b))
       (= (vec3-z a) (vec3-z b))))

;;; vec3-nearly-equal? : Vec3 × Vec3 × Number → Boolean
;;; Approximate equality within tolerance.
(define (vec3-nearly-equal? a b epsilon)
  (and (< (abs (- (vec3-x a) (vec3-x b))) epsilon)
       (< (abs (- (vec3-y a) (vec3-y b))) epsilon)
       (< (abs (- (vec3-z a) (vec3-z b))) epsilon)))
