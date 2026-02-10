(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module rigid-body3d
;;; @requires vec3 quaternion
(require 'vec3)
(require 'quaternion)

(doc 'module 'rigid-body3d)
(doc 'description "3D Rigid Body Dynamics - Defines the 3D rigid body data structure with: Position (Vec3) and linear velocity (Vec3), Orientation (Quaternion) and angular velocity (Vec3), Mass and inverse mass, Inertia tensor and inverse inertia tensor (3x3 matrices).")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Key difference from 2D: inertia is a 3x3 tensor, not a scalar.")

(doc 'section 'mat3-utilities)

;;; mat3-identity : → Mat3
;;; Identity 3x3 matrix.
(define (mat3-identity)
  '((1 0 0) (0 1 0) (0 0 1)))

;;; mat3-zero : → Mat3
;;; Zero 3x3 matrix.
(define (mat3-zero)
  '((0 0 0) (0 0 0) (0 0 0)))

;;; mat3-diagonal : Number × Number × Number → Mat3
;;; Create diagonal matrix.
(define (mat3-diagonal a b c)
  (list (list a 0 0) (list 0 b 0) (list 0 0 c)))

;;; mat3-ref : Mat3 × Nat × Nat → Number
;;; Get element at (row, col).
(define (mat3-ref m i j)
  (list-ref (list-ref m i) j))

;;; mat3-row : Mat3 × Nat → Vec3
;;; Get row as vec3.
(define (mat3-row m i)
  (let ([row (list-ref m i)])
       (vec3 (list-ref row 0) (list-ref row 1) (list-ref row 2))))

;;; mat3-col : Mat3 × Nat → Vec3
;;; Get column as vec3.
(define (mat3-col m j)
  (vec3 (mat3-ref m 0 j)
        (mat3-ref m 1 j)
        (mat3-ref m 2 j)))

;;; mat3-transpose : Mat3 → Mat3
;;; Transpose matrix.
(define (mat3-transpose m)
  (list (list (mat3-ref m 0 0) (mat3-ref m 1 0) (mat3-ref m 2 0))
        (list (mat3-ref m 0 1) (mat3-ref m 1 1) (mat3-ref m 2 1))
        (list (mat3-ref m 0 2) (mat3-ref m 1 2) (mat3-ref m 2 2))))

;;; mat3-scale : Mat3 × Number → Mat3
;;; Scale all elements.
(define (mat3-scale m s)
  (map (lambda (row) (map (lambda (x) (* x s)) row)) m))

;;; mat3-add : Mat3 × Mat3 → Mat3
;;; Add matrices.
(define (mat3-add a b)
  (map (lambda (ra rb)
               (map + ra rb))
       a b))

;;; mat3-mul-vec3 : Mat3 × Vec3 → Vec3
;;; Matrix-vector multiplication.
(define (mat3-mul-vec3 m v)
  (vec3 (+ (* (mat3-ref m 0 0) (vec3-x v))
           (* (mat3-ref m 0 1) (vec3-y v))
           (* (mat3-ref m 0 2) (vec3-z v)))
        (+ (* (mat3-ref m 1 0) (vec3-x v))
           (* (mat3-ref m 1 1) (vec3-y v))
           (* (mat3-ref m 1 2) (vec3-z v)))
        (+ (* (mat3-ref m 2 0) (vec3-x v))
           (* (mat3-ref m 2 1) (vec3-y v))
           (* (mat3-ref m 2 2) (vec3-z v)))))

;;; mat3-mul : Mat3 × Mat3 → Mat3
;;; Matrix-matrix multiplication.
(define (mat3-mul a b)
  (list
   (list (+ (* (mat3-ref a 0 0) (mat3-ref b 0 0))
            (* (mat3-ref a 0 1) (mat3-ref b 1 0))
            (* (mat3-ref a 0 2) (mat3-ref b 2 0)))
         (+ (* (mat3-ref a 0 0) (mat3-ref b 0 1))
            (* (mat3-ref a 0 1) (mat3-ref b 1 1))
            (* (mat3-ref a 0 2) (mat3-ref b 2 1)))
         (+ (* (mat3-ref a 0 0) (mat3-ref b 0 2))
            (* (mat3-ref a 0 1) (mat3-ref b 1 2))
            (* (mat3-ref a 0 2) (mat3-ref b 2 2))))
   (list (+ (* (mat3-ref a 1 0) (mat3-ref b 0 0))
            (* (mat3-ref a 1 1) (mat3-ref b 1 0))
            (* (mat3-ref a 1 2) (mat3-ref b 2 0)))
         (+ (* (mat3-ref a 1 0) (mat3-ref b 0 1))
            (* (mat3-ref a 1 1) (mat3-ref b 1 1))
            (* (mat3-ref a 1 2) (mat3-ref b 2 1)))
         (+ (* (mat3-ref a 1 0) (mat3-ref b 0 2))
            (* (mat3-ref a 1 1) (mat3-ref b 1 2))
            (* (mat3-ref a 1 2) (mat3-ref b 2 2))))
   (list (+ (* (mat3-ref a 2 0) (mat3-ref b 0 0))
            (* (mat3-ref a 2 1) (mat3-ref b 1 0))
            (* (mat3-ref a 2 2) (mat3-ref b 2 0)))
         (+ (* (mat3-ref a 2 0) (mat3-ref b 0 1))
            (* (mat3-ref a 2 1) (mat3-ref b 1 1))
            (* (mat3-ref a 2 2) (mat3-ref b 2 1)))
         (+ (* (mat3-ref a 2 0) (mat3-ref b 0 2))
            (* (mat3-ref a 2 1) (mat3-ref b 1 2))
            (* (mat3-ref a 2 2) (mat3-ref b 2 2))))))

;;; mat3-determinant : Mat3 → Number
;;; Compute determinant.
(define (mat3-determinant m)
  (let ([a (mat3-ref m 0 0)] [b (mat3-ref m 0 1)] [c (mat3-ref m 0 2)]
        [d (mat3-ref m 1 0)] [e (mat3-ref m 1 1)] [f (mat3-ref m 1 2)]
        [g (mat3-ref m 2 0)] [h (mat3-ref m 2 1)] [i (mat3-ref m 2 2)])
       (+ (* a (- (* e i) (* f h)))
          (- (* b (- (* d i) (* f g))))
          (* c (- (* d h) (* e g))))))

;;; mat3-inverse : Mat3 → Mat3
;;; Compute inverse (assumes non-singular).
(define (mat3-inverse m)
  (let* ([det (mat3-determinant m)]
         [inv-det (/ 1 det)]
         [a (mat3-ref m 0 0)] [b (mat3-ref m 0 1)] [c (mat3-ref m 0 2)]
         [d (mat3-ref m 1 0)] [e (mat3-ref m 1 1)] [f (mat3-ref m 1 2)]
         [g (mat3-ref m 2 0)] [h (mat3-ref m 2 1)] [i (mat3-ref m 2 2)])
        (list
         (list (* inv-det (- (* e i) (* f h)))
               (* inv-det (- (* c h) (* b i)))
               (* inv-det (- (* b f) (* c e))))
         (list (* inv-det (- (* f g) (* d i)))
               (* inv-det (- (* a i) (* c g)))
               (* inv-det (- (* c d) (* a f))))
         (list (* inv-det (- (* d h) (* e g)))
               (* inv-det (- (* b g) (* a h)))
               (* inv-det (- (* a e) (* b d)))))))

;;; mat3-zero? : Mat3 → Boolean
;;; Check if matrix is zero.
(define (mat3-zero? m)
  (andmap (lambda (row)
                  (andmap (lambda (x) (< (abs x) 1e-10)) row))
          m))

;;; ====
;;; Inertia Tensor Calculations
;;; ====

;;; inertia-solid-sphere : Number × Number → Mat3
;;; Inertia tensor for solid sphere.
;;; I = (2/5) * m * r² * Identity
(define (inertia-solid-sphere mass radius)
  (let ([I (* (/ 2 5) mass radius radius)])
       (mat3-diagonal I I I)))

;;; inertia-hollow-sphere : Number × Number → Mat3
;;; Inertia tensor for hollow sphere (thin shell).
;;; I = (2/3) * m * r² * Identity
(define (inertia-hollow-sphere mass radius)
  (let ([I (* (/ 2 3) mass radius radius)])
       (mat3-diagonal I I I)))

;;; inertia-solid-box : Number × Number × Number × Number → Mat3
;;; Inertia tensor for solid box.
;;; Half-extents: hx, hy, hz (half-widths along each axis)
;;; Full dimensions: 2*hx, 2*hy, 2*hz
;;;   Ixx = (1/12) * m * (4*hy² + 4*hz²) = (1/3) * m * (hy² + hz²)
;;;   Iyy = (1/12) * m * (4*hx² + 4*hz²) = (1/3) * m * (hx² + hz²)
;;;   Izz = (1/12) * m * (4*hx² + 4*hy²) = (1/3) * m * (hx² + hy²)
(define (inertia-solid-box mass hx hy hz)
  (let* ([hx2 (* hx hx)]
         [hy2 (* hy hy)]
         [hz2 (* hz hz)]
         [k (/ mass 3)])
        (mat3-diagonal (* k (+ hy2 hz2))
                       (* k (+ hx2 hz2))
                       (* k (+ hx2 hy2)))))

;;; inertia-solid-cylinder : Number × Number × Number → Mat3
;;; Inertia tensor for solid cylinder along z-axis.
;;; radius: cylinder radius
;;; half-height: half the height
;;;   Ixx = Iyy = (1/12) * m * (3*r² + 4*h²) = (1/4) * m * r² + (1/3) * m * h²
;;;   Izz = (1/2) * m * r²
(define (inertia-solid-cylinder mass radius half-height)
  (let* ([r2 (* radius radius)]
         [h2 (* half-height half-height)]
         [Ixy (+ (* (/ 1 4) mass r2) (* (/ 1 3) mass h2))]
         [Iz (* (/ 1 2) mass r2)])
        (mat3-diagonal Ixy Ixy Iz)))

;;; inertia-solid-rod : Number × Number → Mat3
;;; Inertia tensor for thin rod along z-axis.
;;; half-length: half the rod length
;;;   Ixx = Iyy = (1/3) * m * L²
;;;   Izz = 0 (idealized, use small value)
(define (inertia-solid-rod mass half-length)
  (let ([I (* (/ 1 3) mass half-length half-length)])
       (mat3-diagonal I I 1e-10)))

;;; inertia-point-mass : Number → Mat3
;;; Inertia tensor for point mass at origin.
;;; All zeros (no resistance to rotation about any axis through the point).
(define (inertia-point-mass mass)
  (mat3-zero))

;;; ====
;;; RigidBody3D Structure
;;; ====

;;; rigid-body-3d : Vec3 × Vec3 × Quat × Vec3 × Number × Number × Mat3 × Mat3 → RigidBody3D
;;; Create a 3D rigid body.
;;;   pos: position (world-space center of mass)
;;;   vel: linear velocity (world-space)
;;;   orientation: rotation quaternion
;;;   angular-vel: angular velocity (world-space, radians/second)
;;;   mass: total mass (0 for static bodies)
;;;   inv-mass: inverse mass (0 for static bodies)
;;;   inertia: inertia tensor (body-space)
;;;   inv-inertia: inverse inertia tensor (body-space)
(define (rigid-body-3d pos vel orientation angular-vel mass inv-mass inertia inv-inertia)
  (list 'rigid-body-3d pos vel orientation angular-vel mass inv-mass inertia inv-inertia))

;;; make-rigid-body-3d : Vec3 × Vec3 × Quat × Vec3 × Number × Mat3 → RigidBody3D
;;; Create rigid body, computing inverse mass and inverse inertia.
(define (make-rigid-body-3d pos vel orientation angular-vel mass inertia)
  (let ([inv-mass (if (= mass 0) 0 (/ 1 mass))]
        [inv-inertia (if (or (= mass 0) (mat3-zero? inertia))
                         (mat3-zero)
                         (mat3-inverse inertia))])
       (rigid-body-3d pos vel orientation angular-vel mass inv-mass inertia inv-inertia)))

;;; make-static-body-3d : Vec3 × Quat → RigidBody3D
;;; Create static (immovable) body.
(define (make-static-body-3d pos orientation)
  (rigid-body-3d pos (vec3-zero) orientation (vec3-zero)
                 0 0 (mat3-zero) (mat3-zero)))

;;; make-dynamic-sphere : Vec3 × Vec3 × Number × Number → RigidBody3D
;;; Create dynamic sphere with proper inertia.
(define (make-dynamic-sphere pos vel mass radius)
  (make-rigid-body-3d pos vel (quat-identity) (vec3-zero)
                      mass (inertia-solid-sphere mass radius)))

;;; make-dynamic-box : Vec3 × Vec3 × Number × Vec3 → RigidBody3D
;;; Create dynamic box with proper inertia.
;;; half-extents: (hx, hy, hz)
(define (make-dynamic-box pos vel mass half-extents)
  (make-rigid-body-3d pos vel (quat-identity) (vec3-zero)
                      mass (inertia-solid-box mass
                                              (vec3-x half-extents)
                                              (vec3-y half-extents)
                                              (vec3-z half-extents))))

;;; ====
;;; Predicates and Accessors
;;; ====

;;; rigid-body-3d? : Any → Boolean
(define (rigid-body-3d? b)
  (and (pair? b) (eq? (car b) 'rigid-body-3d)))

;;; rigid-body-3d-pos : RigidBody3D → Vec3
(define (rigid-body-3d-pos b) (list-ref b 1))

;;; rigid-body-3d-vel : RigidBody3D → Vec3
(define (rigid-body-3d-vel b) (list-ref b 2))

;;; rigid-body-3d-orientation : RigidBody3D → Quaternion
(define (rigid-body-3d-orientation b) (list-ref b 3))

;;; rigid-body-3d-angular-vel : RigidBody3D → Vec3
(define (rigid-body-3d-angular-vel b) (list-ref b 4))

;;; rigid-body-3d-mass : RigidBody3D → Number
(define (rigid-body-3d-mass b) (list-ref b 5))

;;; rigid-body-3d-inv-mass : RigidBody3D → Number
(define (rigid-body-3d-inv-mass b) (list-ref b 6))

;;; rigid-body-3d-inertia : RigidBody3D → Mat3
(define (rigid-body-3d-inertia b) (list-ref b 7))

;;; rigid-body-3d-inv-inertia : RigidBody3D → Mat3
(define (rigid-body-3d-inv-inertia b) (list-ref b 8))

;;; rigid-body-3d-static? : RigidBody3D → Boolean
(define (rigid-body-3d-static? b)
  (= (rigid-body-3d-inv-mass b) 0))

;;; rigid-body-3d-dynamic? : RigidBody3D → Boolean
(define (rigid-body-3d-dynamic? b)
  (not (rigid-body-3d-static? b)))

;;; ====
;;; Updaters (Immutable)
;;; ====

;;; rigid-body-3d-with-pos : RigidBody3D × Vec3 → RigidBody3D
(define (rigid-body-3d-with-pos b new-pos)
  (rigid-body-3d new-pos
                 (rigid-body-3d-vel b)
                 (rigid-body-3d-orientation b)
                 (rigid-body-3d-angular-vel b)
                 (rigid-body-3d-mass b)
                 (rigid-body-3d-inv-mass b)
                 (rigid-body-3d-inertia b)
                 (rigid-body-3d-inv-inertia b)))

;;; rigid-body-3d-with-vel : RigidBody3D × Vec3 → RigidBody3D
(define (rigid-body-3d-with-vel b new-vel)
  (rigid-body-3d (rigid-body-3d-pos b)
                 new-vel
                 (rigid-body-3d-orientation b)
                 (rigid-body-3d-angular-vel b)
                 (rigid-body-3d-mass b)
                 (rigid-body-3d-inv-mass b)
                 (rigid-body-3d-inertia b)
                 (rigid-body-3d-inv-inertia b)))

;;; rigid-body-3d-with-orientation : RigidBody3D × Quaternion → RigidBody3D
(define (rigid-body-3d-with-orientation b new-orientation)
  (rigid-body-3d (rigid-body-3d-pos b)
                 (rigid-body-3d-vel b)
                 new-orientation
                 (rigid-body-3d-angular-vel b)
                 (rigid-body-3d-mass b)
                 (rigid-body-3d-inv-mass b)
                 (rigid-body-3d-inertia b)
                 (rigid-body-3d-inv-inertia b)))

;;; rigid-body-3d-with-angular-vel : RigidBody3D × Vec3 → RigidBody3D
(define (rigid-body-3d-with-angular-vel b new-angular-vel)
  (rigid-body-3d (rigid-body-3d-pos b)
                 (rigid-body-3d-vel b)
                 (rigid-body-3d-orientation b)
                 new-angular-vel
                 (rigid-body-3d-mass b)
                 (rigid-body-3d-inv-mass b)
                 (rigid-body-3d-inertia b)
                 (rigid-body-3d-inv-inertia b)))

;;; rigid-body-3d-with-state : RigidBody3D × Vec3 × Vec3 × Quat × Vec3 → RigidBody3D
;;; Update all dynamic state at once.
(define (rigid-body-3d-with-state b new-pos new-vel new-orientation new-angular-vel)
  (rigid-body-3d new-pos new-vel new-orientation new-angular-vel
                 (rigid-body-3d-mass b)
                 (rigid-body-3d-inv-mass b)
                 (rigid-body-3d-inertia b)
                 (rigid-body-3d-inv-inertia b)))

;;; ====
;;; Coordinate Transformations
;;; ====

;;; rigid-body-3d-local-to-world : RigidBody3D × Vec3 → Vec3
;;; Transform point from body-local to world coordinates.
(define (rigid-body-3d-local-to-world b local-point)
  (vec3-add (rigid-body-3d-pos b)
            (quat-rotate-vec3 (rigid-body-3d-orientation b) local-point)))

;;; rigid-body-3d-world-to-local : RigidBody3D × Vec3 → Vec3
;;; Transform point from world to body-local coordinates.
(define (rigid-body-3d-world-to-local b world-point)
  (quat-rotate-vec3-inverse (rigid-body-3d-orientation b)
                            (vec3-sub world-point (rigid-body-3d-pos b))))

;;; rigid-body-3d-local-to-world-dir : RigidBody3D × Vec3 → Vec3
;;; Transform direction (no translation) from body-local to world.
(define (rigid-body-3d-local-to-world-dir b local-dir)
  (quat-rotate-vec3 (rigid-body-3d-orientation b) local-dir))

;;; rigid-body-3d-world-to-local-dir : RigidBody3D × Vec3 → Vec3
;;; Transform direction from world to body-local.
(define (rigid-body-3d-world-to-local-dir b world-dir)
  (quat-rotate-vec3-inverse (rigid-body-3d-orientation b) world-dir))

;;; ====
;;; World-Space Inertia
;;; ====

;;; rigid-body-3d-world-inertia : RigidBody3D → Mat3
;;; Get inertia tensor in world coordinates.
;;; I_world = R * I_body * R^T
;;; where R is the rotation matrix from the body's quaternion.
(define (rigid-body-3d-world-inertia b)
  (let* ([q (rigid-body-3d-orientation b)]
         [R (quat-to-rotation-matrix q)]
         [I-body (rigid-body-3d-inertia b)]
         [R-T (mat3-transpose R)])
        (mat3-mul R (mat3-mul I-body R-T))))

;;; rigid-body-3d-world-inv-inertia : RigidBody3D → Mat3
;;; Get inverse inertia tensor in world coordinates.
;;; I_world^-1 = R * I_body^-1 * R^T
(define (rigid-body-3d-world-inv-inertia b)
  (let* ([q (rigid-body-3d-orientation b)]
         [R (quat-to-rotation-matrix q)]
         [I-inv-body (rigid-body-3d-inv-inertia b)]
         [R-T (mat3-transpose R)])
        (mat3-mul R (mat3-mul I-inv-body R-T))))

;;; ====
;;; Velocity and Momentum
;;; ====

;;; rigid-body-3d-velocity-at : RigidBody3D × Vec3 → Vec3
;;; Get velocity at a world-space point on the body.
;;; v_point = v_cm + omega × r
;;; where r = point - pos
(define (rigid-body-3d-velocity-at b world-point)
  (let* ([r (vec3-sub world-point (rigid-body-3d-pos b))]
         [omega (rigid-body-3d-angular-vel b)]
         [omega-cross-r (vec3-cross omega r)])
        (vec3-add (rigid-body-3d-vel b) omega-cross-r)))

;;; rigid-body-3d-momentum : RigidBody3D → Vec3
;;; Linear momentum: p = m * v
(define (rigid-body-3d-momentum b)
  (vec3-scale (rigid-body-3d-vel b) (rigid-body-3d-mass b)))

;;; rigid-body-3d-angular-momentum : RigidBody3D → Vec3
;;; Angular momentum: L = I_world * omega
(define (rigid-body-3d-angular-momentum b)
  (mat3-mul-vec3 (rigid-body-3d-world-inertia b)
                 (rigid-body-3d-angular-vel b)))

;;; rigid-body-3d-kinetic-energy : RigidBody3D → Number
;;; Total kinetic energy: KE = 0.5*m*v² + 0.5*omega·(I*omega)
(define (rigid-body-3d-kinetic-energy b)
  (let* ([m (rigid-body-3d-mass b)]
         [v (rigid-body-3d-vel b)]
         [omega (rigid-body-3d-angular-vel b)]
         [I-omega (mat3-mul-vec3 (rigid-body-3d-world-inertia b) omega)]
         [linear-ke (* 0.5 m (vec3-magnitude-sq v))]
         [angular-ke (* 0.5 (vec3-dot omega I-omega))])
        (+ linear-ke angular-ke)))

;;; ====
;;; Forces and Impulses
;;; ====

;;; rigid-body-3d-apply-force : RigidBody3D × Vec3 × Vec3 × Number → RigidBody3D
;;; Apply force at world-space point for duration dt.
;;; Force causes linear acceleration: a = F/m
;;; Torque causes angular acceleration: alpha = I^-1 * tau
;;; where tau = r × F
(define (rigid-body-3d-apply-force b force world-point dt)
  (if (rigid-body-3d-static? b)
      b
      (let* ([r (vec3-sub world-point (rigid-body-3d-pos b))]
             [torque (vec3-cross r force)]
             [linear-accel (vec3-scale force (rigid-body-3d-inv-mass b))]
             [angular-accel (mat3-mul-vec3 (rigid-body-3d-world-inv-inertia b) torque)]
             [new-vel (vec3-add (rigid-body-3d-vel b)
                                (vec3-scale linear-accel dt))]
             [new-angular-vel (vec3-add (rigid-body-3d-angular-vel b)
                                        (vec3-scale angular-accel dt))])
            (rigid-body-3d-with-vel
             (rigid-body-3d-with-angular-vel b new-angular-vel)
             new-vel))))

;;; rigid-body-3d-apply-impulse : RigidBody3D × Vec3 × Vec3 → RigidBody3D
;;; Apply impulse at world-space point.
;;; Impulse = force × dt, directly changes velocity.
;;; Delta-v = J / m
;;; Delta-omega = I^-1 * (r × J)
(define (rigid-body-3d-apply-impulse b impulse world-point)
  (if (rigid-body-3d-static? b)
      b
      (let* ([r (vec3-sub world-point (rigid-body-3d-pos b))]
             [torque-impulse (vec3-cross r impulse)]
             [delta-vel (vec3-scale impulse (rigid-body-3d-inv-mass b))]
             [delta-angular-vel (mat3-mul-vec3 (rigid-body-3d-world-inv-inertia b)
                                               torque-impulse)]
             [new-vel (vec3-add (rigid-body-3d-vel b) delta-vel)]
             [new-angular-vel (vec3-add (rigid-body-3d-angular-vel b)
                                        delta-angular-vel)])
            (rigid-body-3d-with-vel
             (rigid-body-3d-with-angular-vel b new-angular-vel)
             new-vel))))

;;; rigid-body-3d-apply-central-force : RigidBody3D × Vec3 × Number → RigidBody3D
;;; Apply force at center of mass (no torque).
(define (rigid-body-3d-apply-central-force b force dt)
  (if (rigid-body-3d-static? b)
      b
      (let* ([accel (vec3-scale force (rigid-body-3d-inv-mass b))]
             [new-vel (vec3-add (rigid-body-3d-vel b)
                                (vec3-scale accel dt))])
            (rigid-body-3d-with-vel b new-vel))))

;;; rigid-body-3d-apply-central-impulse : RigidBody3D × Vec3 → RigidBody3D
;;; Apply impulse at center of mass (no angular change).
(define (rigid-body-3d-apply-central-impulse b impulse)
  (if (rigid-body-3d-static? b)
      b
      (let ([delta-vel (vec3-scale impulse (rigid-body-3d-inv-mass b))])
           (rigid-body-3d-with-vel b (vec3-add (rigid-body-3d-vel b) delta-vel)))))

;;; rigid-body-3d-apply-torque : RigidBody3D × Vec3 × Number → RigidBody3D
;;; Apply torque for duration dt.
(define (rigid-body-3d-apply-torque b torque dt)
  (if (rigid-body-3d-static? b)
      b
      (let* ([angular-accel (mat3-mul-vec3 (rigid-body-3d-world-inv-inertia b) torque)]
             [new-angular-vel (vec3-add (rigid-body-3d-angular-vel b)
                                        (vec3-scale angular-accel dt))])
            (rigid-body-3d-with-angular-vel b new-angular-vel))))

;;; rigid-body-3d-apply-torque-impulse : RigidBody3D × Vec3 → RigidBody3D
;;; Apply angular impulse directly.
(define (rigid-body-3d-apply-torque-impulse b torque-impulse)
  (if (rigid-body-3d-static? b)
      b
      (let ([delta-angular-vel (mat3-mul-vec3 (rigid-body-3d-world-inv-inertia b)
                                              torque-impulse)])
           (rigid-body-3d-with-angular-vel b
                                           (vec3-add (rigid-body-3d-angular-vel b)
                                                     delta-angular-vel)))))
