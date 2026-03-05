;;; lattice/linalg/quaternion.ss — Quaternion Math
;;; @module quaternion
;;; @requires prelude vec3

(require 'prelude)
(require 'vec3)

(doc 'module 'quaternion)
(doc 'purity 'total)
(doc 'description "Quaternion Math Library

Pure, functional quaternion operations for 3D rotations.

A quaternion is represented as: (quat w x y z)
where w is the scalar part and (x, y, z) is the vector part.

Quaternions represent rotations as q = cos(θ/2) + sin(θ/2)(xi + yj + zk)
where (x, y, z) is the unit rotation axis and θ is the rotation angle.

This is Core code: pure, total, assumes reasonable input.

Dependencies:
  - prelude.ss
  - linalg/vec3.ss")

(doc 'section 'quaternion-construction
     'description "Quaternion constructors and factory functions")

(doc quat
     'type (-> Number Number Number Number Quaternion)
     'description "Create a quaternion (w, x, y, z)")
(define (quat w x y z)
  (doc 'export #t)
  (list 'quat w x y z))

;;; quat-identity : → Quaternion
;;; Identity quaternion (represents no rotation).
(define (quat-identity)
  (doc 'export #t)
  (quat 1 0 0 0))

;;; quat-zero : → Quaternion
;;; Zero quaternion (not a valid rotation).
(define (quat-zero)
  (doc 'export #t)
  (quat 0 0 0 0))

;;; quat-from-axis-angle : Vec3 × Number → Quaternion
;;; Create quaternion from axis-angle representation.
;;; Axis should be a unit vector, angle in radians.
(define (quat-from-axis-angle axis angle)
  (doc 'export #t)
  (let* ([half-angle (/ angle 2)]
         [s (sin half-angle)]
         [c (cos half-angle)])
        (quat c
              (* s (vec3-x axis))
              (* s (vec3-y axis))
              (* s (vec3-z axis)))))

;;; quat-from-euler : Number × Number × Number → Quaternion
;;; Create quaternion from Euler angles (roll, pitch, yaw) in radians.
;;; Uses ZYX convention (yaw-pitch-roll).
(define (quat-from-euler roll pitch yaw)
  (doc 'export #t)
  (let* ([cr (cos (/ roll 2))]
         [sr (sin (/ roll 2))]
         [cp (cos (/ pitch 2))]
         [sp (sin (/ pitch 2))]
         [cy (cos (/ yaw 2))]
         [sy (sin (/ yaw 2))])
        (quat (+ (* cr cp cy) (* sr sp sy))
              (- (* sr cp cy) (* cr sp sy))
              (+ (* cr sp cy) (* sr cp sy))
              (- (* cr cp sy) (* sr sp cy)))))

;;; quat-from-rotation-matrix : Matrix3 → Quaternion
;;; Create quaternion from 3x3 rotation matrix.
;;; Matrix is represented as ((m00 m01 m02) (m10 m11 m12) (m20 m21 m22)).
(define (quat-from-rotation-matrix m)
  (doc 'export #t)
  (let* ([m00 (list-ref (list-ref m 0) 0)]
         [m11 (list-ref (list-ref m 1) 1)]
         [m22 (list-ref (list-ref m 2) 2)]
         [trace (+ m00 m11 m22)])
        (cond
         [(> trace 0)
          (let* ([s (sqrt (+ trace 1.0))]
                 [w (/ s 2)]
                 [s (/ 0.5 s)]
                 [m12 (list-ref (list-ref m 1) 2)]
                 [m21 (list-ref (list-ref m 2) 1)]
                 [m20 (list-ref (list-ref m 2) 0)]
                 [m02 (list-ref (list-ref m 0) 2)]
                 [m01 (list-ref (list-ref m 0) 1)]
                 [m10 (list-ref (list-ref m 1) 0)])
                (quat w
                      (* s (- m21 m12))
                      (* s (- m02 m20))
                      (* s (- m10 m01))))]
         [(and (> m00 m11) (> m00 m22))
          (let* ([m01 (list-ref (list-ref m 0) 1)]
                 [m10 (list-ref (list-ref m 1) 0)]
                 [m02 (list-ref (list-ref m 0) 2)]
                 [m20 (list-ref (list-ref m 2) 0)]
                 [m12 (list-ref (list-ref m 1) 2)]
                 [m21 (list-ref (list-ref m 2) 1)]
                 [s (sqrt (+ 1.0 m00 (- m11) (- m22)))]
                 [x (/ s 2)]
                 [s (/ 0.5 s)])
                (quat (* s (- m21 m12))
                      x
                      (* s (+ m01 m10))
                      (* s (+ m02 m20))))]
         [(> m11 m22)
          (let* ([m01 (list-ref (list-ref m 0) 1)]
                 [m10 (list-ref (list-ref m 1) 0)]
                 [m12 (list-ref (list-ref m 1) 2)]
                 [m21 (list-ref (list-ref m 2) 1)]
                 [m02 (list-ref (list-ref m 0) 2)]
                 [m20 (list-ref (list-ref m 2) 0)]
                 [s (sqrt (+ 1.0 m11 (- m00) (- m22)))]
                 [y (/ s 2)]
                 [s (/ 0.5 s)])
                (quat (* s (- m02 m20))
                      (* s (+ m01 m10))
                      y
                      (* s (+ m12 m21))))]
         [else
          (let* ([m02 (list-ref (list-ref m 0) 2)]
                 [m20 (list-ref (list-ref m 2) 0)]
                 [m12 (list-ref (list-ref m 1) 2)]
                 [m21 (list-ref (list-ref m 2) 1)]
                 [m01 (list-ref (list-ref m 0) 1)]
                 [m10 (list-ref (list-ref m 1) 0)]
                 [s (sqrt (+ 1.0 m22 (- m00) (- m11)))]
                 [z (/ s 2)]
                 [s (/ 0.5 s)])
                (quat (* s (- m10 m01))
                      (* s (+ m02 m20))
                      (* s (+ m12 m21))
                      z))])))

;;; quat-from-two-vectors : Vec3 × Vec3 → Quaternion
;;; Create quaternion that rotates v1 to v2.
;;; Both vectors should be unit vectors.
(define (quat-from-two-vectors v1 v2)
  (doc 'export #t)
  (let* ([dot (vec3-dot v1 v2)]
         [cross (vec3-cross v1 v2)])
        (cond
         [(< dot -0.9999)
          ;; Vectors are opposite, find perpendicular axis
          (let* ([perp (if (< (abs (vec3-x v1)) 0.9)
                           (vec3-cross v1 (vec3 1 0 0))
                           (vec3-cross v1 (vec3 0 1 0)))]
                 [axis (vec3-normalize perp)])
                (quat 0 (vec3-x axis) (vec3-y axis) (vec3-z axis)))]
         [else
          (let* ([s (sqrt (* 2 (+ 1 dot)))]
                 [inv-s (/ 1 s)])
                (quat-normalize
                 (quat (/ s 2)
                       (* (vec3-x cross) inv-s)
                       (* (vec3-y cross) inv-s)
                       (* (vec3-z cross) inv-s))))])))

;;; quat-look-at : Vec3 × Vec3 → Quaternion
;;; Create quaternion that rotates forward to look at direction.
;;; forward = direction to look, up = world up vector.
(define (quat-look-at forward up)
  (doc 'export #t)
  (let* ([f (vec3-normalize forward)]
         [r (vec3-normalize (vec3-cross up f))]
         [u (vec3-cross f r)]
         ;; Construct rotation matrix and convert
         [m (list (list (vec3-x r) (vec3-x u) (vec3-x f))
                  (list (vec3-y r) (vec3-y u) (vec3-y f))
                  (list (vec3-z r) (vec3-z u) (vec3-z f)))])
        (quat-from-rotation-matrix m)))

;;; ====
;;; Predicates
;;; ====

;;; quat? : Any → Boolean
(define (quat? q)
  (doc 'export #t)
  (and (pair? q) (eq? (car q) 'quat)))

;;; quat-identity? : Quaternion → Boolean
;;; Check if quaternion is (near) identity.
(define (quat-identity? q)
  (doc 'export #t)
  (and (< (abs (- (quat-w q) 1.0)) 1e-10)
       (< (abs (quat-x q)) 1e-10)
       (< (abs (quat-y q)) 1e-10)
       (< (abs (quat-z q)) 1e-10)))

;;; quat-unit? : Quaternion → Boolean
;;; Check if quaternion is (near) unit length.
(define (quat-unit? q)
  (doc 'export #t)
  (< (abs (- (quat-magnitude-sq q) 1.0)) 1e-10))

;;; ====
;;; Accessors
;;; ====

;;; quat-w : Quaternion → Number
(define (quat-w q) (list-ref q 1))

;;; quat-x : Quaternion → Number
(define (quat-x q) (list-ref q 2))

;;; quat-y : Quaternion → Number
(define (quat-y q) (list-ref q 3))

;;; quat-z : Quaternion → Number
(define (quat-z q) (list-ref q 4))

;;; quat-scalar : Quaternion → Number
;;; Get scalar (real) part.
(define quat-scalar quat-w)

;;; quat-vector : Quaternion → Vec3
;;; Get vector (imaginary) part.
(define (quat-vector q)
  (doc 'export #t)
  (vec3 (quat-x q) (quat-y q) (quat-z q)))

;;; quat->list : Quaternion → (List Number)
(define (quat->list q)
  (doc 'export #t)
  (list (quat-w q) (quat-x q) (quat-y q) (quat-z q)))

;;; list->quat : (List Number) → Quaternion
(define (list->quat lst)
  (doc 'export #t)
  (quat (car lst) (cadr lst) (caddr lst) (cadddr lst)))

(doc 'section 'quaternion-arithmetic
     'description "Basic quaternion arithmetic operations")

(doc quat-add
     'type (-> Quaternion Quaternion Quaternion)
     'description "Quaternion addition")
(define (quat-add a b)
  (doc 'export #t)
  (quat (+ (quat-w a) (quat-w b))
        (+ (quat-x a) (quat-x b))
        (+ (quat-y a) (quat-y b))
        (+ (quat-z a) (quat-z b))))

;;; quat-sub : Quaternion × Quaternion → Quaternion
;;; Quaternion subtraction.
(define (quat-sub a b)
  (doc 'export #t)
  (quat (- (quat-w a) (quat-w b))
        (- (quat-x a) (quat-x b))
        (- (quat-y a) (quat-y b))
        (- (quat-z a) (quat-z b))))

;;; quat-neg : Quaternion → Quaternion
;;; Negate quaternion.
(define (quat-neg q)
  (doc 'export #t)
  (quat (- (quat-w q))
        (- (quat-x q))
        (- (quat-y q))
        (- (quat-z q))))

;;; quat-scale : Quaternion × Number → Quaternion
;;; Scalar multiplication.
(define (quat-scale q s)
  (doc 'export #t)
  (quat (* (quat-w q) s)
        (* (quat-x q) s)
        (* (quat-y q) s)
        (* (quat-z q) s)))

;;; quat-mul : Quaternion × Quaternion → Quaternion
;;; Quaternion multiplication (Hamilton product).
;;; Represents composition of rotations: first b, then a.
(define (quat-mul a b)
  (doc 'export #t)
  (let ([aw (quat-w a)] [ax (quat-x a)] [ay (quat-y a)] [az (quat-z a)]
        [bw (quat-w b)] [bx (quat-x b)] [by (quat-y b)] [bz (quat-z b)])
       (quat (- (* aw bw) (* ax bx) (* ay by) (* az bz))
             (+ (* aw bx) (* ax bw) (* ay bz) (- (* az by)))
             (+ (* aw by) (- (* ax bz)) (* ay bw) (* az bx))
             (+ (* aw bz) (* ax by) (- (* ay bx)) (* az bw)))))

;;; ====
;;; Length and Normalization
;;; ====

;;; quat-magnitude-sq : Quaternion → Number
;;; Squared magnitude.
(define (quat-magnitude-sq q)
  (doc 'export #t)
  (+ (* (quat-w q) (quat-w q))
     (* (quat-x q) (quat-x q))
     (* (quat-y q) (quat-y q))
     (* (quat-z q) (quat-z q))))

;;; quat-magnitude : Quaternion → Number
;;; Magnitude (norm).
(define (quat-magnitude q)
  (doc 'export #t)
  (sqrt (quat-magnitude-sq q)))

;;; quat-normalize : Quaternion → Quaternion
;;; Return unit quaternion.
(define (quat-normalize q)
  (doc 'export #t)
  (let ([mag (quat-magnitude q)])
       (if (< mag 1e-10)
           (quat-identity)
           (quat (/ (quat-w q) mag)
                 (/ (quat-x q) mag)
                 (/ (quat-y q) mag)
                 (/ (quat-z q) mag)))))

;;; ====
;;; Conjugate and Inverse
;;; ====

;;; quat-conjugate : Quaternion → Quaternion
;;; Conjugate: negate the vector part.
;;; For unit quaternions, this equals the inverse.
(define (quat-conjugate q)
  (doc 'export #t)
  (quat (quat-w q)
        (- (quat-x q))
        (- (quat-y q))
        (- (quat-z q))))

;;; quat-inverse : Quaternion → Quaternion
;;; Multiplicative inverse: q^(-1) = conjugate(q) / |q|^2.
(define (quat-inverse q)
  (doc 'export #t)
  (let ([mag-sq (quat-magnitude-sq q)])
       (if (< mag-sq 1e-10)
           (quat-identity)
           (quat-scale (quat-conjugate q) (/ 1 mag-sq)))))

;;; ====
;;; Dot Product and Angle
;;; ====

;;; quat-dot : Quaternion × Quaternion → Number
;;; Dot product (4D).
(define (quat-dot a b)
  (doc 'export #t)
  (+ (* (quat-w a) (quat-w b))
     (* (quat-x a) (quat-x b))
     (* (quat-y a) (quat-y b))
     (* (quat-z a) (quat-z b))))

;;; quat-angle : Quaternion × Quaternion → Number
;;; Angle between two quaternions in radians.
(define (quat-angle a b)
  (doc 'export #t)
  (let ([dot (abs (quat-dot a b))])
       (* 2 (acos (min 1.0 dot)))))

;;; ====
;;; Interpolation
;;; ====

;;; quat-lerp : Quaternion × Quaternion → Number → Quaternion
;;; Linear interpolation (not normalized).
(define (quat-lerp a b t)
  (doc 'export #t)
  (quat-add (quat-scale a (- 1 t))
            (quat-scale b t)))

;;; quat-nlerp : Quaternion × Quaternion × Number → Quaternion
;;; Normalized linear interpolation.
;;; Faster than slerp, good for small angles.
(define (quat-nlerp a b t)
  (doc 'export #t)
  ;; Take shortest path
  (let ([b (if (< (quat-dot a b) 0) (quat-neg b) b)])
       (quat-normalize (quat-lerp a b t))))

;;; quat-slerp : Quaternion × Quaternion × Number → Quaternion
;;; Spherical linear interpolation.
;;; Constant angular velocity, best for animation.
(define (quat-slerp a b t)
  (doc 'export #t)
  (let* ([dot (quat-dot a b)]
         ;; Take shortest path
         [dot (if (< dot 0) (- dot) dot)]
         [b (if (< (quat-dot a b) 0) (quat-neg b) b)])
        (if (> dot 0.9995)
            ;; Very close, use nlerp to avoid numerical issues
            (quat-nlerp a b t)
            (let* ([theta (acos dot)]
                   [sin-theta (sin theta)]
                   [s0 (/ (sin (* (- 1 t) theta)) sin-theta)]
                   [s1 (/ (sin (* t theta)) sin-theta)])
                  (quat-add (quat-scale a s0)
                            (quat-scale b s1))))))

(doc 'section 'rotation-operations
     'description "Quaternion rotation and conversion operations")

(doc quat-rotate-vec3
     'type (-> Quaternion Vec3 Vec3)
     'description "Rotate a vector by the quaternion. v' = q * v * q^(-1) for unit quaternion.")
(define (quat-rotate-vec3 q v)
  (doc 'export #t)
  (let* ([qv (quat 0 (vec3-x v) (vec3-y v) (vec3-z v))]
         [result (quat-mul (quat-mul q qv) (quat-conjugate q))])
        (vec3 (quat-x result) (quat-y result) (quat-z result))))

;;; quat-rotate-vec3-inverse : Quaternion × Vec3 → Vec3
;;; Rotate a vector by the inverse quaternion.
(define (quat-rotate-vec3-inverse q v)
  (doc 'export #t)
  (quat-rotate-vec3 (quat-conjugate q) v))

;;; quat-to-axis-angle : Quaternion → (Vec3 × Number)
;;; Convert to axis-angle representation.
;;; Returns (axis, angle) where axis is unit and angle is in radians.
(define (quat-to-axis-angle q)
  (doc 'export #t)
  (let* ([q (quat-normalize q)]
         [w (quat-w q)]
         [angle (* 2 (acos (max -1.0 (min 1.0 w))))]
         [s (sqrt (- 1 (* w w)))])
        (if (< s 1e-10)
            ;; Angle is 0, axis is arbitrary
            (list (vec3 1 0 0) 0)
            (list (vec3 (/ (quat-x q) s)
                        (/ (quat-y q) s)
                        (/ (quat-z q) s))
                  angle))))

;;; quat-to-euler : Quaternion → (roll × pitch × yaw)
;;; Convert to Euler angles (ZYX convention).
;;; Returns (roll, pitch, yaw) in radians.
(define (quat-to-euler q)
  (doc 'export #t)
  (let* ([w (quat-w q)]
         [x (quat-x q)]
         [y (quat-y q)]
         [z (quat-z q)]
         ;; Roll (x-axis rotation)
         [sinr-cosp (* 2 (+ (* w x) (* y z)))]
         [cosr-cosp (- 1 (* 2 (+ (* x x) (* y y))))]
         [roll (atan sinr-cosp cosr-cosp)]
         ;; Pitch (y-axis rotation)
         [sinp (* 2 (- (* w y) (* z x)))]
         [pitch (if (>= (abs sinp) 1)
                    (* (if (< sinp 0) -1 1) (/ 3.141592653589793 2))
                    (asin sinp))]
         ;; Yaw (z-axis rotation)
         [siny-cosp (* 2 (+ (* w z) (* x y)))]
         [cosy-cosp (- 1 (* 2 (+ (* y y) (* z z))))]
         [yaw (atan siny-cosp cosy-cosp)])
        (list roll pitch yaw)))

;;; quat-to-rotation-matrix : Quaternion → Matrix3
;;; Convert to 3x3 rotation matrix.
(define (quat-to-rotation-matrix q)
  (doc 'export #t)
  (let* ([q (quat-normalize q)]
         [w (quat-w q)]
         [x (quat-x q)]
         [y (quat-y q)]
         [z (quat-z q)]
         [x2 (* 2 x x)] [y2 (* 2 y y)] [z2 (* 2 z z)]
         [xy (* 2 x y)] [xz (* 2 x z)] [yz (* 2 y z)]
         [wx (* 2 w x)] [wy (* 2 w y)] [wz (* 2 w z)])
        (list (list (- 1 y2 z2) (- xy wz) (+ xz wy))
              (list (+ xy wz) (- 1 x2 z2) (- yz wx))
              (list (- xz wy) (+ yz wx) (- 1 x2 y2)))))

;;; ====
;;; Angular Velocity
;;; ====

;;; quat-angular-velocity : Quaternion × Quaternion × Number → Vec3
;;; Compute angular velocity from q1 to q2 over time dt.
;;; Returns angular velocity vector in radians/second.
(define (quat-angular-velocity q1 q2 dt)
  (doc 'export #t)
  (if (< dt 1e-10)
      (vec3-zero)
      (let* ([dq (quat-mul q2 (quat-inverse q1))]
             [dq (if (< (quat-w dq) 0) (quat-neg dq) dq)]
             [axis-angle (quat-to-axis-angle dq)]
             [axis (car axis-angle)]
             [angle (cadr axis-angle)])
            (vec3-scale axis (/ angle dt)))))

;;; quat-integrate : Quaternion × Vec3 × Number → Quaternion
;;; Integrate orientation by angular velocity over time dt.
;;; omega is angular velocity vector in radians/second.
(define (quat-integrate q omega dt)
  (doc 'export #t)
  (let* ([omega-mag (vec3-magnitude omega)]
         [theta (* omega-mag dt 0.5)])
        (if (< omega-mag 1e-10)
            q
            (let* ([axis (vec3-scale-inv omega omega-mag)]
                   [dq (quat (cos theta)
                             (* (sin theta) (vec3-x axis))
                             (* (sin theta) (vec3-y axis))
                             (* (sin theta) (vec3-z axis)))])
                  (quat-normalize (quat-mul dq q))))))

;;; ====
;;; Utility Functions
;;; ====

;;; quat-forward : Quaternion → Vec3
;;; Get the forward direction (rotated +Z axis).
(define (quat-forward q)
  (doc 'export #t)
  (quat-rotate-vec3 q (vec3 0 0 1)))

;;; quat-right : Quaternion → Vec3
;;; Get the right direction (rotated +X axis).
(define (quat-right q)
  (doc 'export #t)
  (quat-rotate-vec3 q (vec3 1 0 0)))

;;; quat-up : Quaternion → Vec3
;;; Get the up direction (rotated +Y axis).
(define (quat-up q)
  (doc 'export #t)
  (quat-rotate-vec3 q (vec3 0 1 0)))

;;; quat-equal? : Quaternion × Quaternion → Boolean
;;; Check if quaternions represent the same rotation.
;;; Note: q and -q represent the same rotation.
(define (quat-equal? a b)
  (doc 'export #t)
  (or (and (< (abs (- (quat-w a) (quat-w b))) 1e-10)
           (< (abs (- (quat-x a) (quat-x b))) 1e-10)
           (< (abs (- (quat-y a) (quat-y b))) 1e-10)
           (< (abs (- (quat-z a) (quat-z b))) 1e-10))
      (and (< (abs (+ (quat-w a) (quat-w b))) 1e-10)
           (< (abs (+ (quat-x a) (quat-x b))) 1e-10)
           (< (abs (+ (quat-y a) (quat-y b))) 1e-10)
           (< (abs (+ (quat-z a) (quat-z b))) 1e-10))))

;;; quat-nearly-equal? : Quaternion × Quaternion × Number → Boolean
;;; Check if quaternions are approximately equal within tolerance.
(define (quat-nearly-equal? a b epsilon)
  (doc 'export #t)
  (or (and (< (abs (- (quat-w a) (quat-w b))) epsilon)
           (< (abs (- (quat-x a) (quat-x b))) epsilon)
           (< (abs (- (quat-y a) (quat-y b))) epsilon)
           (< (abs (- (quat-z a) (quat-z b))) epsilon))
      (and (< (abs (+ (quat-w a) (quat-w b))) epsilon)
           (< (abs (+ (quat-x a) (quat-x b))) epsilon)
           (< (abs (+ (quat-y a) (quat-y b))) epsilon)
           (< (abs (+ (quat-z a) (quat-z b))) epsilon))))
