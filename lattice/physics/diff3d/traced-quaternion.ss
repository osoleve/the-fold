;;; core/diff-physics-3d/traced-quaternion.ss --- Differentiable Quaternion Operations
;;;
;;; Traced quaternion operations for automatic differentiation through 3D rotations.
;;; A traced-quat is a quadruple of traced scalar values (w, x, y, z).
;;;
;;; Quaternion notation: q = w + xi + yj + zk
;;; Unit quaternions represent rotations: q = cos(θ/2) + sin(θ/2)(axis)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/quaternion.ss
;;;   - diff-physics-3d/traced-vec3.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/quaternion.ss")
(load "lattice/physics/diff3d/traced-vec3.ss")

;;; ====
;;; Traced Quaternion Construction
;;; ====

;;; traced-quat : TracedValue × TracedValue × TracedValue × TracedValue → TracedQuat
;;; Create a traced quaternion from traced w, x, y, z components.
(define (traced-quat w x y z)
  (list 'traced-quat w x y z))

;;; traced-quat? : Any → Boolean
(define (traced-quat? q)
  (and (pair? q) (eq? (car q) 'traced-quat)))

;;; traced-quat-w : TracedQuat → TracedValue
(define (traced-quat-w q) (list-ref q 1))

;;; traced-quat-x : TracedQuat → TracedValue
(define (traced-quat-x q) (list-ref q 2))

;;; traced-quat-y : TracedQuat → TracedValue
(define (traced-quat-y q) (list-ref q 3))

;;; traced-quat-z : TracedQuat → TracedValue
(define (traced-quat-z q) (list-ref q 4))

;;; traced-quat-identity : Tape → TracedQuat
;;; Create traced identity quaternion (1, 0, 0, 0).
(define (traced-quat-identity tape)
  (traced-quat (make-traced-var 1 tape)
               (make-traced-var 0 tape)
               (make-traced-var 0 tape)
               (make-traced-var 0 tape)))

;;; ====
;;; Conversion Between Quat and TracedQuat
;;; ====

;;; lift-quat : Quaternion × Tape → TracedQuat
;;; Convert regular quaternion to traced quaternion (creates traced variables).
(define (lift-quat q tape)
  (traced-quat (make-traced-var (quat-w q) tape)
               (make-traced-var (quat-x q) tape)
               (make-traced-var (quat-y q) tape)
               (make-traced-var (quat-z q) tape)))

;;; lift-quat-const : Quaternion → TracedQuat
;;; Convert regular quaternion to traced quaternion with constant components.
;;; Constants have zero gradient and don't need tape tracking.
(define (lift-quat-const q)
  (traced-quat (quat-w q) (quat-x q) (quat-y q) (quat-z q)))

;;; unpack-traced-quat : TracedQuat → Quaternion
;;; Extract numeric values from traced quaternion.
(define (unpack-traced-quat tq)
  (quat (traced-value (traced-quat-w tq))
        (traced-value (traced-quat-x tq))
        (traced-value (traced-quat-y tq))
        (traced-value (traced-quat-z tq))))

;;; traced-quat->list : TracedQuat → (List TracedValue)
;;; Extract components as list.
(define (traced-quat->list tq)
  (list (traced-quat-w tq) (traced-quat-x tq) (traced-quat-y tq) (traced-quat-z tq)))

;;; traced-quat-vector : TracedQuat → TracedVec3
;;; Get vector (imaginary) part as traced vec3.
(define (traced-quat-vector q)
  (traced-vec3 (traced-quat-x q) (traced-quat-y q) (traced-quat-z q)))

;;; ====
;;; Basic Arithmetic
;;; ====

;;; traced-quat-add : TracedQuat × TracedQuat → TracedQuat
;;; Quaternion addition with gradient tracking.
(define (traced-quat-add a b)
  (traced-quat (traced-add (traced-quat-w a) (traced-quat-w b))
               (traced-add (traced-quat-x a) (traced-quat-x b))
               (traced-add (traced-quat-y a) (traced-quat-y b))
               (traced-add (traced-quat-z a) (traced-quat-z b))))

;;; traced-quat-sub : TracedQuat × TracedQuat → TracedQuat
;;; Quaternion subtraction.
(define (traced-quat-sub a b)
  (traced-quat (traced-sub (traced-quat-w a) (traced-quat-w b))
               (traced-sub (traced-quat-x a) (traced-quat-x b))
               (traced-sub (traced-quat-y a) (traced-quat-y b))
               (traced-sub (traced-quat-z a) (traced-quat-z b))))

;;; traced-quat-neg : TracedQuat → TracedQuat
;;; Negate quaternion.
(define (traced-quat-neg q)
  (traced-quat (traced-neg (traced-quat-w q))
               (traced-neg (traced-quat-x q))
               (traced-neg (traced-quat-y q))
               (traced-neg (traced-quat-z q))))

;;; traced-quat-scale : TracedQuat × TracedValue → TracedQuat
;;; Scalar multiplication.
(define (traced-quat-scale q s)
  (traced-quat (traced-mul (traced-quat-w q) s)
               (traced-mul (traced-quat-x q) s)
               (traced-mul (traced-quat-y q) s)
               (traced-mul (traced-quat-z q) s)))

;;; traced-quat-scale-inv : TracedQuat × TracedValue → TracedQuat
;;; Scalar division (q / s).
(define (traced-quat-scale-inv q s)
  (traced-quat (traced-div (traced-quat-w q) s)
               (traced-div (traced-quat-x q) s)
               (traced-div (traced-quat-y q) s)
               (traced-div (traced-quat-z q) s)))

;;; traced-quat-mul : TracedQuat × TracedQuat → TracedQuat
;;; Quaternion multiplication (Hamilton product).
;;; Represents composition of rotations: first b, then a.
;;; (aw + ax*i + ay*j + az*k) * (bw + bx*i + by*j + bz*k) =
;;;   (aw*bw - ax*bx - ay*by - az*bz) +
;;;   (aw*bx + ax*bw + ay*bz - az*by)*i +
;;;   (aw*by - ax*bz + ay*bw + az*bx)*j +
;;;   (aw*bz + ax*by - ay*bx + az*bw)*k
(define (traced-quat-mul a b)
  (let ([aw (traced-quat-w a)] [ax (traced-quat-x a)]
        [ay (traced-quat-y a)] [az (traced-quat-z a)]
        [bw (traced-quat-w b)] [bx (traced-quat-x b)]
        [by (traced-quat-y b)] [bz (traced-quat-z b)])
       (traced-quat
        ;; w component: aw*bw - ax*bx - ay*by - az*bz
        (traced-sub (traced-mul aw bw)
                    (traced-add (traced-mul ax bx)
                                (traced-add (traced-mul ay by)
                                            (traced-mul az bz))))
        ;; x component: aw*bx + ax*bw + ay*bz - az*by
        (traced-add (traced-mul aw bx)
                    (traced-add (traced-mul ax bw)
                                (traced-sub (traced-mul ay bz)
                                            (traced-mul az by))))
        ;; y component: aw*by - ax*bz + ay*bw + az*bx
        (traced-add (traced-sub (traced-mul aw by)
                                (traced-mul ax bz))
                    (traced-add (traced-mul ay bw)
                                (traced-mul az bx)))
        ;; z component: aw*bz + ax*by - ay*bx + az*bw
        (traced-add (traced-mul aw bz)
                    (traced-add (traced-sub (traced-mul ax by)
                                            (traced-mul ay bx))
                                (traced-mul az bw))))))

;;; ====
;;; Length and Normalization
;;; ====

;;; traced-quat-dot : TracedQuat × TracedQuat → TracedValue
;;; Dot product (4D).
(define (traced-quat-dot a b)
  (traced-add (traced-mul (traced-quat-w a) (traced-quat-w b))
              (traced-add (traced-mul (traced-quat-x a) (traced-quat-x b))
                          (traced-add (traced-mul (traced-quat-y a) (traced-quat-y b))
                                      (traced-mul (traced-quat-z a) (traced-quat-z b))))))

;;; traced-quat-magnitude-sq : TracedQuat → TracedValue
;;; Squared magnitude.
(define (traced-quat-magnitude-sq q)
  (traced-quat-dot q q))

;;; traced-quat-magnitude : TracedQuat → TracedValue
;;; Magnitude (norm).
;;; WARNING: Gradient undefined at |q|=0. Use smooth-magnitude for safety.
(define (traced-quat-magnitude q)
  (traced-sqrt (traced-quat-magnitude-sq q)))

;;; traced-quat-smooth-magnitude : TracedQuat × Number → TracedValue
;;; Smoothed magnitude with epsilon to avoid gradient singularity at zero.
;;; Returns sqrt(w² + x² + y² + z² + ε²) which is always > 0.
(define (traced-quat-smooth-magnitude q epsilon)
  (traced-sqrt (traced-add (traced-quat-magnitude-sq q)
                           (* epsilon epsilon))))

;;; traced-quat-normalize : TracedQuat → TracedQuat
;;; Return unit quaternion.
;;; WARNING: Gradient undefined at |q|=0.
(define (traced-quat-normalize q)
  (traced-quat-scale-inv q (traced-quat-magnitude q)))

;;; traced-quat-smooth-normalize : TracedQuat × Number → TracedQuat
;;; Normalized quaternion with smooth behavior near zero.
;;; Uses sqrt(|q|² + ε²) in denominator for gradient stability.
;;; CRITICAL for physics: quaternions must stay normalized.
(define (traced-quat-smooth-normalize q epsilon)
  (traced-quat-scale-inv q (traced-quat-smooth-magnitude q epsilon)))

;;; ====
;;; Conjugate and Inverse
;;; ====

;;; traced-quat-conjugate : TracedQuat → TracedQuat
;;; Conjugate: negate the vector part.
;;; For unit quaternions, this equals the inverse.
(define (traced-quat-conjugate q)
  (traced-quat (traced-quat-w q)
               (traced-neg (traced-quat-x q))
               (traced-neg (traced-quat-y q))
               (traced-neg (traced-quat-z q))))

;;; traced-quat-inverse : TracedQuat → TracedQuat
;;; Multiplicative inverse: q^(-1) = conjugate(q) / |q|².
;;; For unit quaternions, use conjugate instead (more efficient).
(define (traced-quat-inverse q)
  (traced-quat-scale-inv (traced-quat-conjugate q) (traced-quat-magnitude-sq q)))

;;; traced-quat-smooth-inverse : TracedQuat × Number → TracedQuat
;;; Smooth inverse for gradient stability.
(define (traced-quat-smooth-inverse q epsilon)
  (let ([mag-sq-smooth (traced-add (traced-quat-magnitude-sq q) (* epsilon epsilon))])
       (traced-quat-scale-inv (traced-quat-conjugate q) mag-sq-smooth)))

;;; ====
;;; Rotation Operations
;;; ====

;;; traced-quat-rotate-vec3 : TracedQuat × TracedVec3 → TracedVec3
;;; Rotate a vector by the quaternion.
;;; v' = q * (0,v) * q^(-1) for unit quaternion (use conjugate).
;;; For non-unit quaternions, use inverse.
(define (traced-quat-rotate-vec3 q v)
  (let* ([qv (traced-quat 0
                          (traced-vec3-x v)
                          (traced-vec3-y v)
                          (traced-vec3-z v))]
         [q-conj (traced-quat-conjugate q)]
         [result (traced-quat-mul (traced-quat-mul q qv) q-conj)])
        (traced-vec3 (traced-quat-x result)
                     (traced-quat-y result)
                     (traced-quat-z result))))

;;; traced-quat-rotate-vec3-inverse : TracedQuat × TracedVec3 → TracedVec3
;;; Rotate a vector by the inverse quaternion.
(define (traced-quat-rotate-vec3-inverse q v)
  (traced-quat-rotate-vec3 (traced-quat-conjugate q) v))

;;; ====
;;; Integration for Rigid Body Dynamics
;;; ====

;;; traced-quat-derivative : TracedQuat × TracedVec3 → TracedQuat
;;; Compute quaternion derivative given angular velocity.
;;; dq/dt = 0.5 * omega_quat * q
;;; where omega_quat = (0, omega.x, omega.y, omega.z)
(define (traced-quat-derivative q omega)
  (let ([omega-quat (traced-quat 0
                                 (traced-vec3-x omega)
                                 (traced-vec3-y omega)
                                 (traced-vec3-z omega))])
       (traced-quat-scale (traced-quat-mul omega-quat q) 0.5)))

;;; traced-quat-integrate : TracedQuat × TracedVec3 × Number → TracedQuat
;;; Integrate orientation by angular velocity over time dt.
;;; Uses first-order approximation: q_new = normalize(q + dt * dq/dt)
;;; omega is angular velocity vector in radians/second (world-space).
;;;
;;; For high accuracy, use:
;;;   q_new = exp(0.5 * dt * omega) * q
;;; But first-order + renormalization is sufficient for physics.
(define (traced-quat-integrate q omega dt)
  (let* ([dq (traced-quat-derivative q omega)]
         [q-new (traced-quat-add q (traced-quat-scale dq dt))])
        ;; CRITICAL: Renormalize to prevent drift
        (traced-quat-smooth-normalize q-new 1e-10)))

;;; traced-quat-integrate-body : TracedQuat × TracedVec3 × Number → TracedQuat
;;; Integrate orientation by body-space angular velocity.
;;; omega is in body-local coordinates.
(define (traced-quat-integrate-body q omega-body dt)
  (let ([omega-world (traced-quat-rotate-vec3 q omega-body)])
       (traced-quat-integrate q omega-world dt)))

;;; ====
;;; Interpolation
;;; ====

;;; traced-quat-lerp : TracedQuat × TracedQuat × TracedValue → TracedQuat
;;; Linear interpolation (not normalized).
(define (traced-quat-lerp a b t)
  (traced-quat-add (traced-quat-scale a (traced-sub 1 t))
                   (traced-quat-scale b t)))

;;; traced-quat-nlerp : TracedQuat × TracedQuat × TracedValue × Number → TracedQuat
;;; Normalized linear interpolation.
;;; Faster than slerp, good for small angles.
;;; Note: doesn't handle sign flip (both +q and -q represent same rotation).
(define (traced-quat-nlerp a b t epsilon)
  (traced-quat-smooth-normalize (traced-quat-lerp a b t) epsilon))

;;; ====
;;; Conversion to Rotation Matrix
;;; ====

;;; traced-quat-to-rotation-matrix : TracedQuat → (List (List TracedValue))
;;; Convert to 3x3 rotation matrix.
;;; Returns matrix as list of rows: ((r00 r01 r02) (r10 r11 r12) (r20 r21 r22))
(define (traced-quat-to-rotation-matrix q)
  (let* ([w (traced-quat-w q)]
         [x (traced-quat-x q)]
         [y (traced-quat-y q)]
         [z (traced-quat-z q)]
         [x2 (traced-mul x x)]
         [y2 (traced-mul y y)]
         [z2 (traced-mul z z)]
         [xy (traced-mul x y)]
         [xz (traced-mul x z)]
         [yz (traced-mul y z)]
         [wx (traced-mul w x)]
         [wy (traced-mul w y)]
         [wz (traced-mul w z)]
         ;; Build matrix elements
         [r00 (traced-sub 1 (traced-mul 2 (traced-add y2 z2)))]
         [r01 (traced-mul 2 (traced-sub xy wz))]
         [r02 (traced-mul 2 (traced-add xz wy))]
         [r10 (traced-mul 2 (traced-add xy wz))]
         [r11 (traced-sub 1 (traced-mul 2 (traced-add x2 z2)))]
         [r12 (traced-mul 2 (traced-sub yz wx))]
         [r20 (traced-mul 2 (traced-sub xz wy))]
         [r21 (traced-mul 2 (traced-add yz wx))]
         [r22 (traced-sub 1 (traced-mul 2 (traced-add x2 y2)))])
        (list (list r00 r01 r02)
              (list r10 r11 r12)
              (list r20 r21 r22))))

;;; ====
;;; Gradient Utilities
;;; ====

;;; quat-gradient : ((TracedQuat) → TracedValue) × Quat → (Number × Number × Number × Number)
;;; Compute gradient of scalar function f at quaternion q.
;;; Returns (∂f/∂w, ∂f/∂x, ∂f/∂y, ∂f/∂z).
(define (quat-gradient f q)
  (gradient (lambda (w x y z)
                    (f (traced-quat w x y z)))
            (list (quat-w q) (quat-x q) (quat-y q) (quat-z q))))

;;; quat-gradient-2arg : ((TracedQuat × TracedQuat) → TracedValue) × Quat × Quat → ((Numbers) × (Numbers))
;;; Compute gradients w.r.t. two quaternion arguments.
(define (quat-gradient-2arg f a b)
  (let* ([grads (gradient (lambda (aw ax ay az bw bx by bz)
                                  (f (traced-quat aw ax ay az)
                                     (traced-quat bw bx by bz)))
                          (list (quat-w a) (quat-x a) (quat-y a) (quat-z a)
                                (quat-w b) (quat-x b) (quat-y b) (quat-z b)))])
        (values (list (list-ref grads 0) (list-ref grads 1)
                      (list-ref grads 2) (list-ref grads 3))
                (list (list-ref grads 4) (list-ref grads 5)
                      (list-ref grads 6) (list-ref grads 7)))))
