;;; core/diff-physics-3d/traced-body3d.ss --- Differentiable 3D Rigid Body State
;;;
;;; Traced 3D rigid body state for automatic differentiation through physics.
;;; A traced body contains traced position, velocity, orientation (quaternion),
;;; and angular velocity. Mass and inertia tensor are constants.
;;;
;;; State dimension: 13 (3 pos + 3 vel + 4 quat + 3 omega)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/vec3.ss
;;;   - autodiff/reverse-diff.ss
;;;   - diff-physics-3d/traced-vec3.ss
;;;   - diff-physics-3d/traced-quaternion.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec3.ss")
(load "core/autodiff/reverse-diff.ss")
(load "lattice/physics/diff3d/traced-vec3.ss")
(load "lattice/physics/diff3d/traced-quaternion.ss")

;;; ====
;;; Mat3 Operations for Inertia Tensors
;;; ====

;;; mat3-identity : → Mat3
(define (mat3-identity)
  (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

;;; mat3-diagonal : Number × Number × Number → Mat3
(define (mat3-diagonal a b c)
  (list (list a 0 0) (list 0 b 0) (list 0 0 c)))

;;; mat3-ref : Mat3 × Nat × Nat → Number
(define (mat3-ref m i j)
  (list-ref (list-ref m i) j))

;;; mat3-mul-vec3 : Mat3 × Vec3 → Vec3
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

;;; mat3-transpose : Mat3 → Mat3
(define (mat3-transpose m)
  (list (list (mat3-ref m 0 0) (mat3-ref m 1 0) (mat3-ref m 2 0))
        (list (mat3-ref m 0 1) (mat3-ref m 1 1) (mat3-ref m 2 1))
        (list (mat3-ref m 0 2) (mat3-ref m 1 2) (mat3-ref m 2 2))))

;;; mat3-mul : Mat3 × Mat3 → Mat3
(define (mat3-mul a b)
  (let ([b-t (mat3-transpose b)])
       (map (lambda (row)
                    (map (lambda (col)
                                 (+ (* (car row) (car col))
                                    (* (cadr row) (cadr col))
                                    (* (caddr row) (caddr col))))
                         b-t))
            a)))

;;; mat3-scale : Mat3 × Number → Mat3
(define (mat3-scale m s)
  (map (lambda (row) (map (lambda (x) (* x s)) row)) m))

;;; ====
;;; Traced Mat3 Operations
;;; ====

;;; traced-mat3-mul-vec3 : Mat3 × TracedVec3 → TracedVec3
;;; Multiply constant matrix by traced vector.
(define (traced-mat3-mul-vec3 m v)
  (let ([vx (traced-vec3-x v)]
        [vy (traced-vec3-y v)]
        [vz (traced-vec3-z v)])
       (traced-vec3
        (traced-add (traced-add (traced-mul (mat3-ref m 0 0) vx)
                                (traced-mul (mat3-ref m 0 1) vy))
                    (traced-mul (mat3-ref m 0 2) vz))
        (traced-add (traced-add (traced-mul (mat3-ref m 1 0) vx)
                                (traced-mul (mat3-ref m 1 1) vy))
                    (traced-mul (mat3-ref m 1 2) vz))
        (traced-add (traced-add (traced-mul (mat3-ref m 2 0) vx)
                                (traced-mul (mat3-ref m 2 1) vy))
                    (traced-mul (mat3-ref m 2 2) vz)))))

;;; ====
;;; Traced 3D Rigid Body Data Structure
;;; ====

;;; A traced 3D rigid body has:
;;;   - pos: position (TracedVec3)
;;;   - vel: velocity (TracedVec3)
;;;   - orientation: rotation quaternion (TracedQuat)
;;;   - angular-vel: angular velocity in world space (TracedVec3)
;;;   - mass: mass (Number > 0, 0 for static) - constant
;;;   - inv-mass: inverse mass (0 for static) - constant
;;;   - inertia: body-space inertia tensor (Mat3) - constant
;;;   - inv-inertia: body-space inverse inertia tensor (Mat3) - constant

;;; make-traced-body-3d : TracedVec3 × TracedVec3 × TracedQuat × TracedVec3 × Number × Mat3 → TracedBody3D
;;; Create a traced 3D rigid body.
(define (make-traced-body-3d pos vel orientation angular-vel mass inertia)
  (list 'traced-body-3d
        pos
        vel
        orientation
        angular-vel
        mass
        (if (= mass 0) 0 (/ 1 mass))
        inertia
        (if (= mass 0) (mat3-scale inertia 0) (mat3-invert-diagonal inertia))))

;;; mat3-invert-diagonal : Mat3 → Mat3
;;; Invert a diagonal matrix (assumes diagonal, doesn't check).
(define (mat3-invert-diagonal m)
  (let ([a (mat3-ref m 0 0)]
        [b (mat3-ref m 1 1)]
        [c (mat3-ref m 2 2)])
       (mat3-diagonal (if (= a 0) 0 (/ 1 a))
                      (if (= b 0) 0 (/ 1 b))
                      (if (= c 0) 0 (/ 1 c)))))

;;; traced-body-3d? : Any → Boolean
(define (traced-body-3d? b)
  (and (pair? b) (eq? (car b) 'traced-body-3d)))

;;; ====
;;; Accessors
;;; ====

;;; traced-body-3d-pos : TracedBody3D → TracedVec3
(define (traced-body-3d-pos b) (list-ref b 1))

;;; traced-body-3d-vel : TracedBody3D → TracedVec3
(define (traced-body-3d-vel b) (list-ref b 2))

;;; traced-body-3d-orientation : TracedBody3D → TracedQuat
(define (traced-body-3d-orientation b) (list-ref b 3))

;;; traced-body-3d-angular-vel : TracedBody3D → TracedVec3
(define (traced-body-3d-angular-vel b) (list-ref b 4))

;;; traced-body-3d-mass : TracedBody3D → Number
(define (traced-body-3d-mass b) (list-ref b 5))

;;; traced-body-3d-inv-mass : TracedBody3D → Number
(define (traced-body-3d-inv-mass b) (list-ref b 6))

;;; traced-body-3d-inertia : TracedBody3D → Mat3
(define (traced-body-3d-inertia b) (list-ref b 7))

;;; traced-body-3d-inv-inertia : TracedBody3D → Mat3
(define (traced-body-3d-inv-inertia b) (list-ref b 8))

;;; traced-body-3d-static? : TracedBody3D → Boolean
(define (traced-body-3d-static? b)
  (= (traced-body-3d-inv-mass b) 0))

;;; traced-body-3d-dynamic? : TracedBody3D → Boolean
(define (traced-body-3d-dynamic? b)
  (not (traced-body-3d-static? b)))

;;; ====
;;; World-Space Inertia
;;; ====

;;; For impulse calculations, we need the world-space inverse inertia tensor:
;;;   I_world^-1 = R * I_body^-1 * R^T
;;; where R is the rotation matrix from the quaternion.

;;; traced-quat-to-rotation-matrix : TracedQuat → (values Number ...) → Mat3
;;; Convert quaternion to 3x3 rotation matrix (extracts numeric values).
(define (quat-to-rotation-matrix q)
  (let* ([w (traced-value (traced-quat-w q))]
         [x (traced-value (traced-quat-x q))]
         [y (traced-value (traced-quat-y q))]
         [z (traced-value (traced-quat-z q))]
         ;; Rotation matrix elements
         [r00 (- 1 (* 2 (+ (* y y) (* z z))))]
         [r01 (* 2 (- (* x y) (* w z)))]
         [r02 (* 2 (+ (* x z) (* w y)))]
         [r10 (* 2 (+ (* x y) (* w z)))]
         [r11 (- 1 (* 2 (+ (* x x) (* z z))))]
         [r12 (* 2 (- (* y z) (* w x)))]
         [r20 (* 2 (- (* x z) (* w y)))]
         [r21 (* 2 (+ (* y z) (* w x)))]
         [r22 (- 1 (* 2 (+ (* x x) (* y y))))])
        (list (list r00 r01 r02)
              (list r10 r11 r12)
              (list r20 r21 r22))))

;;; traced-body-3d-world-inv-inertia : TracedBody3D → Mat3
;;; Compute world-space inverse inertia tensor.
(define (traced-body-3d-world-inv-inertia b)
  (if (traced-body-3d-static? b)
      (mat3-scale (mat3-identity) 0)
      (let* ([R (quat-to-rotation-matrix (traced-body-3d-orientation b))]
             [I-inv (traced-body-3d-inv-inertia b)]
             [R-t (mat3-transpose R)])
            ;; I_world^-1 = R * I_body^-1 * R^T
            (mat3-mul R (mat3-mul I-inv R-t)))))

;;; ====
;;; Updaters (Immutable)
;;; ====

;;; traced-body-3d-with-pos : TracedBody3D × TracedVec3 → TracedBody3D
(define (traced-body-3d-with-pos b new-pos)
  (list 'traced-body-3d
        new-pos
        (traced-body-3d-vel b)
        (traced-body-3d-orientation b)
        (traced-body-3d-angular-vel b)
        (traced-body-3d-mass b)
        (traced-body-3d-inv-mass b)
        (traced-body-3d-inertia b)
        (traced-body-3d-inv-inertia b)))

;;; traced-body-3d-with-vel : TracedBody3D × TracedVec3 → TracedBody3D
(define (traced-body-3d-with-vel b new-vel)
  (list 'traced-body-3d
        (traced-body-3d-pos b)
        new-vel
        (traced-body-3d-orientation b)
        (traced-body-3d-angular-vel b)
        (traced-body-3d-mass b)
        (traced-body-3d-inv-mass b)
        (traced-body-3d-inertia b)
        (traced-body-3d-inv-inertia b)))

;;; traced-body-3d-with-orientation : TracedBody3D × TracedQuat → TracedBody3D
(define (traced-body-3d-with-orientation b new-orientation)
  (list 'traced-body-3d
        (traced-body-3d-pos b)
        (traced-body-3d-vel b)
        new-orientation
        (traced-body-3d-angular-vel b)
        (traced-body-3d-mass b)
        (traced-body-3d-inv-mass b)
        (traced-body-3d-inertia b)
        (traced-body-3d-inv-inertia b)))

;;; traced-body-3d-with-angular-vel : TracedBody3D × TracedVec3 → TracedBody3D
(define (traced-body-3d-with-angular-vel b new-angular-vel)
  (list 'traced-body-3d
        (traced-body-3d-pos b)
        (traced-body-3d-vel b)
        (traced-body-3d-orientation b)
        new-angular-vel
        (traced-body-3d-mass b)
        (traced-body-3d-inv-mass b)
        (traced-body-3d-inertia b)
        (traced-body-3d-inv-inertia b)))

;;; traced-body-3d-with-state : TracedBody3D × TracedVec3 × TracedVec3 × TracedQuat × TracedVec3 → TracedBody3D
;;; Update all kinematic state at once.
(define (traced-body-3d-with-state b new-pos new-vel new-orientation new-angular-vel)
  (list 'traced-body-3d
        new-pos
        new-vel
        new-orientation
        new-angular-vel
        (traced-body-3d-mass b)
        (traced-body-3d-inv-mass b)
        (traced-body-3d-inertia b)
        (traced-body-3d-inv-inertia b)))

;;; ====
;;; Body State Flattening (for optimization)
;;; ====

;;; traced-body-3d-state->list : TracedBody3D → (List TracedValue)
;;; Flatten body state to list of 13 traced values:
;;; (px, py, pz, vx, vy, vz, qw, qx, qy, qz, wx, wy, wz)
(define (traced-body-3d-state->list tb)
  (list (traced-vec3-x (traced-body-3d-pos tb))
        (traced-vec3-y (traced-body-3d-pos tb))
        (traced-vec3-z (traced-body-3d-pos tb))
        (traced-vec3-x (traced-body-3d-vel tb))
        (traced-vec3-y (traced-body-3d-vel tb))
        (traced-vec3-z (traced-body-3d-vel tb))
        (traced-quat-w (traced-body-3d-orientation tb))
        (traced-quat-x (traced-body-3d-orientation tb))
        (traced-quat-y (traced-body-3d-orientation tb))
        (traced-quat-z (traced-body-3d-orientation tb))
        (traced-vec3-x (traced-body-3d-angular-vel tb))
        (traced-vec3-y (traced-body-3d-angular-vel tb))
        (traced-vec3-z (traced-body-3d-angular-vel tb))))

;;; list->traced-body-3d-state : (List TracedValue) × Number × Mat3 → TracedBody3D
;;; Reconstruct body state from flattened list.
(define (list->traced-body-3d-state vals mass inertia)
  (make-traced-body-3d
   (traced-vec3 (list-ref vals 0) (list-ref vals 1) (list-ref vals 2))
   (traced-vec3 (list-ref vals 3) (list-ref vals 4) (list-ref vals 5))
   (traced-quat (list-ref vals 6) (list-ref vals 7) (list-ref vals 8) (list-ref vals 9))
   (traced-vec3 (list-ref vals 10) (list-ref vals 11) (list-ref vals 12))
   mass
   inertia))

;;; body-3d-state-dimension : → Number
;;; Dimension of body state vector (13 for 3D rigid body).
(define (body-3d-state-dimension) 13)

;;; ====
;;; Physics Operations on Traced Bodies
;;; ====

;;; traced-body-3d-velocity-at : TracedBody3D × TracedVec3 → TracedVec3
;;; Compute velocity at a world-space point on the body.
;;; v_point = v_cm + omega × r
(define (traced-body-3d-velocity-at body world-point)
  (let* ([r (traced-vec3-sub world-point (traced-body-3d-pos body))]
         [omega (traced-body-3d-angular-vel body)]
         [rotational-vel (traced-vec3-cross omega r)])
        (traced-vec3-add (traced-body-3d-vel body) rotational-vel)))

;;; traced-apply-impulse-3d : TracedBody3D × TracedVec3 × TracedVec3 → TracedBody3D
;;; Apply an impulse J at world-space point contact.
;;; Changes linear velocity by J/m and angular velocity by I_world^-1 * (r × J).
(define (traced-apply-impulse-3d body impulse contact)
  (if (traced-body-3d-static? body)
      body
      (let* ([r (traced-vec3-sub contact (traced-body-3d-pos body))]
             [inv-mass (traced-body-3d-inv-mass body)]
             [I-world-inv (traced-body-3d-world-inv-inertia body)]
             ;; Δv = J / m
             [delta-vel (traced-vec3-scale impulse inv-mass)]
             ;; Δω = I_world^-1 * (r × J)
             [r-cross-J (traced-vec3-cross r impulse)]
             [delta-omega (traced-mat3-mul-vec3 I-world-inv r-cross-J)]
             [new-vel (traced-vec3-add (traced-body-3d-vel body) delta-vel)]
             [new-omega (traced-vec3-add (traced-body-3d-angular-vel body) delta-omega)])
            (traced-body-3d-with-state body
                                       (traced-body-3d-pos body)
                                       new-vel
                                       (traced-body-3d-orientation body)
                                       new-omega))))

;;; traced-apply-central-impulse-3d : TracedBody3D × TracedVec3 → TracedBody3D
;;; Apply impulse at center of mass (no torque).
(define (traced-apply-central-impulse-3d body impulse)
  (if (traced-body-3d-static? body)
      body
      (let* ([delta-vel (traced-vec3-scale impulse (traced-body-3d-inv-mass body))]
             [new-vel (traced-vec3-add (traced-body-3d-vel body) delta-vel)])
            (traced-body-3d-with-vel body new-vel))))

;;; traced-apply-torque-impulse-3d : TracedBody3D × TracedVec3 → TracedBody3D
;;; Apply angular impulse (torque * dt) directly.
(define (traced-apply-torque-impulse-3d body torque-impulse)
  (if (traced-body-3d-static? body)
      body
      (let* ([I-world-inv (traced-body-3d-world-inv-inertia body)]
             [delta-omega (traced-mat3-mul-vec3 I-world-inv torque-impulse)]
             [new-omega (traced-vec3-add (traced-body-3d-angular-vel body) delta-omega)])
            (traced-body-3d-with-angular-vel body new-omega))))

;;; traced-apply-force-3d : TracedBody3D × TracedVec3 × TracedVec3 × TracedValue → TracedBody3D
;;; Apply force F at world-space point for duration dt.
(define (traced-apply-force-3d body force point dt)
  (traced-apply-impulse-3d body (traced-vec3-scale force dt) point))

;;; traced-apply-central-force-3d : TracedBody3D × TracedVec3 × TracedValue → TracedBody3D
;;; Apply force at center of mass (no torque).
(define (traced-apply-central-force-3d body force dt)
  (if (traced-body-3d-static? body)
      body
      (let* ([impulse (traced-vec3-scale force dt)]
             [delta-vel (traced-vec3-scale impulse (traced-body-3d-inv-mass body))]
             [new-vel (traced-vec3-add (traced-body-3d-vel body) delta-vel)])
            (traced-body-3d-with-vel body new-vel))))

;;; traced-apply-torque-3d : TracedBody3D × TracedVec3 × TracedValue → TracedBody3D
;;; Apply torque for duration dt.
(define (traced-apply-torque-3d body torque dt)
  (traced-apply-torque-impulse-3d body (traced-vec3-scale torque dt)))

;;; ====
;;; World-Space Transformations
;;; ====

;;; traced-local-to-world-3d : TracedBody3D × TracedVec3 → TracedVec3
;;; Transform a point from body-local coordinates to world coordinates.
;;; world = pos + rotate(local, orientation)
(define (traced-local-to-world-3d body local-point)
  (traced-vec3-add (traced-body-3d-pos body)
                   (traced-quat-rotate-vec3 (traced-body-3d-orientation body) local-point)))

;;; traced-world-to-local-3d : TracedBody3D × TracedVec3 → TracedVec3
;;; Transform a point from world coordinates to body-local coordinates.
;;; local = rotate(world - pos, conjugate(orientation))
(define (traced-world-to-local-3d body world-point)
  (let ([q-inv (traced-quat-conjugate (traced-body-3d-orientation body))])
       (traced-quat-rotate-vec3 q-inv
                                (traced-vec3-sub world-point (traced-body-3d-pos body)))))

;;; traced-local-dir-to-world-3d : TracedBody3D × TracedVec3 → TracedVec3
;;; Transform a direction vector from body-local to world coordinates.
(define (traced-local-dir-to-world-3d body local-dir)
  (traced-quat-rotate-vec3 (traced-body-3d-orientation body) local-dir))

;;; traced-world-dir-to-local-3d : TracedBody3D × TracedVec3 → TracedVec3
;;; Transform a direction vector from world to body-local coordinates.
(define (traced-world-dir-to-local-3d body world-dir)
  (let ([q-inv (traced-quat-conjugate (traced-body-3d-orientation body))])
       (traced-quat-rotate-vec3 q-inv world-dir)))

;;; ====
;;; Energy and Momentum
;;; ====

;;; traced-body-3d-kinetic-energy : TracedBody3D → TracedValue
;;; Compute total kinetic energy: (1/2)mv² + (1/2)ω·I·ω
(define (traced-body-3d-kinetic-energy body)
  (let* ([m (traced-body-3d-mass body)]
         [I (traced-body-3d-inertia body)]
         [v-sq (traced-vec3-magnitude-sq (traced-body-3d-vel body))]
         [omega (traced-body-3d-angular-vel body)]
         ;; For diagonal inertia: ω·I·ω = Ixx*ωx² + Iyy*ωy² + Izz*ωz²
         [I-omega (traced-mat3-mul-vec3 I omega)]
         [omega-I-omega (traced-vec3-dot omega I-omega)])
        (traced-add (traced-mul (* 0.5 m) v-sq)
                    (traced-mul 0.5 omega-I-omega))))

;;; traced-body-3d-momentum : TracedBody3D → TracedVec3
;;; Compute linear momentum: p = mv
(define (traced-body-3d-momentum body)
  (traced-vec3-scale (traced-body-3d-vel body) (traced-body-3d-mass body)))

;;; traced-body-3d-angular-momentum : TracedBody3D → TracedVec3
;;; Compute angular momentum in world space: L = I_world * ω
(define (traced-body-3d-angular-momentum body)
  (let* ([R (quat-to-rotation-matrix (traced-body-3d-orientation body))]
         [I (traced-body-3d-inertia body)]
         [omega (traced-body-3d-angular-vel body)]
         ;; L = R * I * R^T * ω = R * (I * (R^T * ω))
         [R-t (mat3-transpose R)]
         [local-omega (traced-mat3-mul-vec3 R-t omega)]
         [I-local-omega (traced-mat3-mul-vec3 I local-omega)])
        (traced-mat3-mul-vec3 R I-local-omega)))

;;; ====
;;; Static Body Constructor
;;; ====

;;; make-traced-static-body-3d : TracedVec3 × TracedQuat → TracedBody3D
;;; Create a static (immovable) traced 3D rigid body.
(define (make-traced-static-body-3d pos orientation)
  (let ([tape (or (and (traced? (traced-vec3-x pos))
                       (traced-tape (traced-vec3-x pos)))
                  (and (traced? (traced-quat-w orientation))
                       (traced-tape (traced-quat-w orientation))))])
       (list 'traced-body-3d
             pos
             (if tape
                 (traced-vec3-zero tape)
                 (lift-vec3-const (vec3-zero)))
             orientation
             (if tape
                 (traced-vec3-zero tape)
                 (lift-vec3-const (vec3-zero)))
             0    ; mass = 0 for static
             0    ; inv-mass = 0 for static
             (mat3-identity)
             (mat3-scale (mat3-identity) 0))))  ; inv-inertia = 0 for static

;;; ====
;;; Inertia Tensor Helpers
;;; ====

;;; inertia-solid-sphere : Number × Number → Mat3
;;; I = (2/5) * m * r² for solid sphere.
(define (inertia-solid-sphere mass radius)
  (let ([I (* (/ 2 5) mass radius radius)])
       (mat3-diagonal I I I)))

;;; inertia-hollow-sphere : Number × Number → Mat3
;;; I = (2/3) * m * r² for thin hollow sphere.
(define (inertia-hollow-sphere mass radius)
  (let ([I (* (/ 2 3) mass radius radius)])
       (mat3-diagonal I I I)))

;;; inertia-solid-box : Number × Number × Number × Number → Mat3
;;; I_xx = (1/12) * m * (h² + d²), etc. for solid box with dimensions (w, h, d).
(define (inertia-solid-box mass width height depth)
  (let ([c (/ mass 12)])
       (mat3-diagonal (* c (+ (* height height) (* depth depth)))
                      (* c (+ (* width width) (* depth depth)))
                      (* c (+ (* width width) (* height height))))))

;;; inertia-solid-cylinder : Number × Number × Number → Mat3
;;; Cylinder along z-axis with given radius and height.
;;; I_xx = I_yy = (1/12) * m * (3r² + h²)
;;; I_zz = (1/2) * m * r²
(define (inertia-solid-cylinder mass radius height)
  (let ([I-xy (* (/ 1 12) mass (+ (* 3 radius radius) (* height height)))]
        [I-z (* (/ 1 2) mass radius radius)])
       (mat3-diagonal I-xy I-xy I-z)))
