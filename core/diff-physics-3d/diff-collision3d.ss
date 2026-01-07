;;; core/diff-physics-3d/diff-collision3d.ss --- Differentiable 3D Collision Response
;;;
;;; Impulse-based collision response with gradient support for 3D.
;;; Combines soft contact forces with rigid body dynamics.
;;;
;;; For fully differentiable physics, this module provides:
;;; 1. Soft contact model (via smooth-collision3d.ss)
;;; 2. Contact geometry gradients (analytical)
;;; 3. Integration with traced 3D rigid bodies
;;;
;;; Key difference from 2D: torque is a Vec3 (cross product), not scalar.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/vec3.ss
;;;   - autodiff/reverse-diff.ss
;;;   - diff-physics-3d/traced-vec3.ss
;;;   - diff-physics-3d/traced-body3d.ss
;;;   - diff-physics-3d/smooth-collision3d.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec3.ss")
(load "core/autodiff/reverse-diff.ss")
(load "core/diff-physics-3d/traced-vec3.ss")
(load "core/diff-physics-3d/traced-body3d.ss")
(load "core/diff-physics-3d/traced-integrators3d.ss")
(load "core/diff-physics-3d/smooth-collision3d.ss")

;;; ============================================================
;;; Sphere-Sphere Collision Response
;;; ============================================================

;;; traced-sphere-collision-force : TracedBody3D × Number × TracedBody3D × Number × SoftMaterial3D → (Values TracedVec3 TracedVec3)
;;; Compute soft contact force and torque between two spherical bodies.
;;; Returns (force-on-A, torque-on-A).
(define (traced-sphere-collision-force body-a radius-a body-b radius-b mat)
  (let* ([pos-a (traced-body-3d-pos body-a)]
         [vel-a (traced-body-3d-vel body-a)]
         [pos-b (traced-body-3d-pos body-b)]
         [vel-b (traced-body-3d-vel body-b)]
         [stiffness (soft-material-3d-stiffness mat)]
         [damping (soft-material-3d-damping mat)]
         [alpha (soft-material-3d-alpha mat)]
         [mu (soft-material-3d-friction mat)])
        (call-with-values
         (lambda () (traced-sphere-contact pos-a radius-a pos-b radius-b))
         (lambda (contact normal penetration)
                 (let* (;; Velocity at contact point for each body
                        [vel-at-a (traced-body-3d-velocity-at body-a contact)]
                        [vel-at-b (traced-body-3d-velocity-at body-b contact)]
                        [rel-vel (traced-vec3-sub vel-at-a vel-at-b)]
                        ;; Approach velocity (positive when approaching)
                        [approach-vel (traced-neg (traced-vec3-dot rel-vel normal))]
                        ;; Normal force magnitude
                        [normal-force-mag (traced-soft-contact-force-damped
                                           penetration approach-vel stiffness damping alpha)]
                        ;; Normal force vector (repulsive, opposite to normal)
                        [normal-force (traced-vec3-scale normal (traced-neg normal-force-mag))]
                        ;; Tangent velocity (perpendicular to normal)
                        [vel-normal-component (traced-vec3-scale normal (traced-vec3-dot rel-vel normal))]
                        [tangent-vel (traced-vec3-sub rel-vel vel-normal-component)]
                        ;; Friction force
                        [friction-force (traced-soft-friction-3d tangent-vel normal-force-mag mu 10)]
                        ;; Total force
                        [total-force (traced-vec3-add normal-force friction-force)]
                        ;; Torque on A: r_A x F (cross product gives Vec3 in 3D)
                        [r-a (traced-vec3-sub contact pos-a)]
                        [torque-a (traced-vec3-cross r-a total-force)])
                       (values total-force torque-a))))))

;;; traced-sphere-collision-impulses : TracedBody3D × Number × TracedBody3D × Number × SoftMaterial3D × Number → (Values TracedBody3D TracedBody3D)
;;; Apply soft collision forces to both bodies for time dt.
;;; Returns updated (body-a, body-b).
(define (traced-sphere-collision-impulses body-a radius-a body-b radius-b mat dt)
  (call-with-values
   (lambda () (traced-sphere-collision-force body-a radius-a body-b radius-b mat))
   (lambda (force-a torque-a)
           (let* ([force-b (traced-vec3-neg force-a)]
                  [pos-a (traced-body-3d-pos body-a)]
                  [pos-b (traced-body-3d-pos body-b)])
                 (call-with-values
                  (lambda () (traced-sphere-contact pos-a radius-a pos-b radius-b))
                  (lambda (contact _normal _penetration)
                          (let* ([r-b (traced-vec3-sub contact pos-b)]
                                 [torque-b (traced-vec3-cross r-b force-b)]
                                 [new-a (traced-apply-central-force-3d
                                         (traced-apply-torque-3d body-a torque-a dt)
                                         force-a dt)]
                                 [new-b (traced-apply-central-force-3d
                                         (traced-apply-torque-3d body-b torque-b dt)
                                         force-b dt)])
                                (values new-a new-b))))))))

;;; ============================================================
;;; Sphere-Ground Collision
;;; ============================================================

;;; traced-ground-collision-force-3d : TracedBody3D × Number × Number × SoftMaterial3D → (Values TracedVec3 TracedVec3)
;;; Soft contact force from horizontal ground at y = ground-y.
;;; Ground normal is (0, 1, 0).
;;; Returns (force, torque).
(define (traced-ground-collision-force-3d body radius ground-y mat)
  (let* ([pos (traced-body-3d-pos body)]
         [stiffness (soft-material-3d-stiffness mat)]
         [damping (soft-material-3d-damping mat)]
         [alpha (soft-material-3d-alpha mat)]
         [mu (soft-material-3d-friction mat)]
         ;; Penetration depth (positive when below ground + radius)
         [penetration (traced-sub (+ ground-y radius)
                                  (traced-vec3-y pos))]
         ;; Contact point (bottom of sphere)
         [contact (traced-vec3 (traced-vec3-x pos)
                               (traced-sub (traced-vec3-y pos) radius)
                               (traced-vec3-z pos))]
         ;; Velocity at contact
         [vel-at-contact (traced-body-3d-velocity-at body contact)]
         ;; Approach velocity (downward velocity)
         [approach-vel (traced-neg (traced-vec3-y vel-at-contact))]
         ;; Normal force magnitude (upward)
         [normal-force-mag (traced-soft-contact-force-damped
                            penetration approach-vel stiffness damping alpha)]
         ;; Tangent velocity (horizontal XZ plane)
         [tangent-vel (traced-vec3 (traced-vec3-x vel-at-contact)
                                   0
                                   (traced-vec3-z vel-at-contact))]
         ;; Friction force
         [friction-force (traced-soft-friction-3d tangent-vel normal-force-mag mu 10)]
         ;; Total force: normal (up) + friction (horizontal)
         [zero (if (traced-tape (traced-vec3-x pos))
                   (make-traced-var 0 (traced-tape (traced-vec3-x pos)))
                   0)]
         [total-force (traced-vec3-add (traced-vec3 zero normal-force-mag zero)
                                       friction-force)]
         ;; Torque from friction at contact
         [r (traced-vec3-sub contact pos)]
         [torque (traced-vec3-cross r total-force)])
        (values total-force torque)))

;;; traced-ground-collision-step-3d : TracedBody3D × Number × Number × SoftMaterial3D × Number → TracedBody3D
;;; Apply ground collision forces for time dt.
(define (traced-ground-collision-step-3d body radius ground-y mat dt)
  (call-with-values
   (lambda () (traced-ground-collision-force-3d body radius ground-y mat))
   (lambda (force torque)
           (traced-apply-central-force-3d
            (traced-apply-torque-3d body torque dt)
            force dt))))

;;; ============================================================
;;; Multi-Body Collision System
;;; ============================================================

;;; For simulating many bodies, we accumulate forces then integrate.

;;; make-collision-pair-3d : Nat × Nat × Number × Number → CollisionPair3D
;;; Define a collision pair for the system.
(define (make-collision-pair-3d idx-a idx-b radius-a radius-b)
  (list 'collision-pair-3d idx-a idx-b radius-a radius-b))

;;; collision-pair-3d-idx-a : CollisionPair3D → Nat
(define (collision-pair-3d-idx-a cp) (list-ref cp 1))

;;; collision-pair-3d-idx-b : CollisionPair3D → Nat
(define (collision-pair-3d-idx-b cp) (list-ref cp 2))

;;; collision-pair-3d-radius-a : CollisionPair3D → Number
(define (collision-pair-3d-radius-a cp) (list-ref cp 3))

;;; collision-pair-3d-radius-b : CollisionPair3D → Number
(define (collision-pair-3d-radius-b cp) (list-ref cp 4))

;;; traced-accumulate-collision-forces-3d : (Vector TracedBody3D) × (List CollisionPair3D) × SoftMaterial3D → (Values (Vector TracedVec3) (Vector TracedVec3))
;;; Compute force and torque on each body from all collision pairs.
;;; Returns vector of forces and vector of torques per body.
(define (traced-accumulate-collision-forces-3d bodies pairs mat)
  (let* ([n (vector-length bodies)]
         ;; Initialize force accumulators
         [forces (make-vector n)]
         [torques (make-vector n)])
        ;; Initialize to zero
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! forces i (lift-vec3-const (vec3-zero)))
            (vector-set! torques i (lift-vec3-const (vec3-zero))))
        ;; Accumulate from each pair
        (for-each
         (lambda (pair)
                 (let* ([ia (collision-pair-3d-idx-a pair)]
                        [ib (collision-pair-3d-idx-b pair)]
                        [ra (collision-pair-3d-radius-a pair)]
                        [rb (collision-pair-3d-radius-b pair)]
                        [body-a (vector-ref bodies ia)]
                        [body-b (vector-ref bodies ib)])
                       (call-with-values
                        (lambda () (traced-sphere-collision-force body-a ra body-b rb mat))
                        (lambda (force-a torque-a)
                                ;; Add to A
                                (vector-set! forces ia
                                             (traced-vec3-add (vector-ref forces ia) force-a))
                                (vector-set! torques ia
                                             (traced-vec3-add (vector-ref torques ia) torque-a))
                                ;; Add to B (opposite force, torque computed from contact)
                                (vector-set! forces ib
                                             (traced-vec3-sub (vector-ref forces ib) force-a))
                                ;; Torque on B needs to be computed from r_B x (-force)
                                ;; For simplicity, negate torque (not quite correct for offset contacts)
                                (vector-set! torques ib
                                             (traced-vec3-sub (vector-ref torques ib) torque-a))))))
         pairs)
        (values forces torques)))

;;; ============================================================
;;; Restitution Coefficient (for hard contact comparison)
;;; ============================================================

;;; In soft contact, restitution is implicitly controlled by stiffness/damping.
;;; For comparison with impulse-based methods, we can compute effective restitution.

;;; effective-restitution-3d : Number × Number × Number × Number → Number
;;; Compute effective coefficient of restitution from material parameters.
;;;   stiffness: contact stiffness
;;;   damping: contact damping
;;;   mass: effective mass at contact
;;;   approach-vel: initial approach velocity
;;; This is an approximation based on linearized contact dynamics.
(define (effective-restitution-3d stiffness damping mass approach-vel)
  (let* ([omega (sqrt (/ stiffness mass))]
         [zeta (/ damping (* 2 (sqrt (* stiffness mass))))])  ; damping ratio
        (if (>= zeta 1)
            0  ; overdamped = no bounce
            (exp (/ (* (- 3.141592653589793) zeta)
                    (sqrt (- 1 (* zeta zeta))))))))

;;; ============================================================
;;; Contact Detection Utilities
;;; ============================================================

;;; These help determine which pairs need collision response.
;;; Note: For differentiable physics, we often want soft forces even
;;; for non-contacting bodies (with smooth cutoff).

;;; traced-spheres-overlapping? : TracedVec3 × Number × TracedVec3 × Number → Boolean
;;; Check if two spheres overlap (for filtering).
;;; Uses traced values but returns boolean (non-differentiable).
(define (traced-spheres-overlapping? pos-a radius-a pos-b radius-b)
  (let ([dist (traced-value (traced-vec3-smooth-distance pos-a pos-b 1e-10))])
       (< dist (+ radius-a radius-b))))

;;; traced-sphere-ground-overlapping? : TracedVec3 × Number × Number → Boolean
;;; Check if sphere overlaps ground plane.
(define (traced-sphere-ground-overlapping? pos radius ground-y)
  (< (traced-value (traced-vec3-y pos)) (+ ground-y radius)))

;;; ============================================================
;;; Complete Collision Step
;;; ============================================================

;;; traced-collision-step-3d : TracedBody3D × Number × (List (TracedBody3D × Number)) × Number × SoftMaterial3D × Number → TracedBody3D
;;; Apply collision forces from other bodies and ground for one timestep.
;;;   body: the body to update
;;;   radius: body's collision radius
;;;   others: list of (other-body, other-radius) pairs
;;;   ground-y: ground plane y-coordinate (or #f for no ground)
;;;   mat: soft material properties
;;;   dt: timestep
(define (traced-collision-step-3d body radius others ground-y mat dt)
  (let* (;; Accumulate forces from all collisions
         [initial-force (lift-vec3-const (vec3-zero))]
         [initial-torque (lift-vec3-const (vec3-zero))]
         ;; Add forces from other bodies
         [body-result
          (fold-left
           (lambda (acc other)
                   (let* ([other-body (car other)]
                          [other-radius (cadr other)]
                          [force-acc (car acc)]
                          [torque-acc (cadr acc)])
                         (call-with-values
                          (lambda () (traced-sphere-collision-force body radius other-body other-radius mat))
                          (lambda (f t)
                                  (list (traced-vec3-add force-acc f)
                                        (traced-vec3-add torque-acc t))))))
           (list initial-force initial-torque)
           others)]
         [force-from-bodies (car body-result)]
         [torque-from-bodies (cadr body-result)]
         ;; Add ground force if ground specified
         [ground-result
          (if ground-y
              (call-with-values
               (lambda () (traced-ground-collision-force-3d body radius ground-y mat))
               (lambda (gf gt)
                       (list (traced-vec3-add force-from-bodies gf)
                             (traced-vec3-add torque-from-bodies gt))))
              body-result)]
         [total-force (car ground-result)]
         [total-torque (cadr ground-result)])
        ;; Apply forces
        (traced-apply-central-force-3d
         (traced-apply-torque-3d body total-torque dt)
         total-force dt)))

;;; ============================================================
;;; Wall Collision Forces (Box Boundary)
;;; ============================================================

;;; traced-wall-collision-force-3d : TracedBody3D × Number × Vec3 × Vec3 × SoftMaterial3D → (Values TracedVec3 TracedVec3)
;;; Compute forces from box-shaped boundary walls.
;;; Returns (force, torque).
(define (traced-wall-collision-force-3d body radius min-corner max-corner mat)
  (let* ([pos (traced-body-3d-pos body)]
         [vel (traced-body-3d-vel body)]
         [force (traced-wall-forces-3d pos vel radius min-corner max-corner mat)]
         ;; For wall forces, torque is from friction at contact
         ;; Simplified: assume no torque from wall (or compute properly)
         [torque (lift-vec3-const (vec3-zero))])
        (values force torque)))

;;; traced-wall-collision-step-3d : TracedBody3D × Number × Vec3 × Vec3 × SoftMaterial3D × Number → TracedBody3D
;;; Apply wall collision forces for time dt.
(define (traced-wall-collision-step-3d body radius min-corner max-corner mat dt)
  (call-with-values
   (lambda () (traced-wall-collision-force-3d body radius min-corner max-corner mat))
   (lambda (force torque)
           (traced-apply-central-force-3d
            (traced-apply-torque-3d body torque dt)
            force dt))))

;;; ============================================================
;;; Gravity Integration Helper
;;; ============================================================

;;; traced-apply-gravity-3d : TracedBody3D × Vec3 × Number → TracedBody3D
;;; Apply gravity acceleration for time dt.
;;; Gravity is typically (vec3 0 -9.8 0).
(define (traced-apply-gravity-3d body gravity dt)
  (let* ([tape (traced-tape (traced-vec3-x (traced-body-3d-pos body)))]
         [g (if tape (lift-vec3 gravity tape) (lift-vec3-const gravity))])
        (traced-euler-step-3d body g (lift-vec3-const (vec3-zero)) dt)))

;;; ============================================================
;;; Combined Physics Step
;;; ============================================================

;;; traced-physics-step-3d : TracedBody3D × Number × (List (TracedBody3D × Number)) × Number × Vec3 × Vec3 × Vec3 × SoftMaterial3D × Number → TracedBody3D
;;; Complete physics step including gravity, ground, walls, and other bodies.
;;;   body: body to update
;;;   radius: collision radius
;;;   others: list of (other-body, radius) for sphere-sphere collisions
;;;   ground-y: ground plane y (or #f)
;;;   gravity: gravity vector (e.g., (vec3 0 -9.8 0))
;;;   min-corner, max-corner: wall boundaries (or #f for no walls)
;;;   mat: soft material
;;;   dt: timestep
(define (traced-physics-step-3d body radius others ground-y gravity min-corner max-corner mat dt)
  (let* (;; Apply gravity
         [body-with-gravity (traced-apply-gravity-3d body gravity dt)]
         ;; Apply collision forces
         [body-with-collisions (traced-collision-step-3d body-with-gravity radius others ground-y mat dt)]
         ;; Apply wall forces if boundaries specified
         [body-final (if min-corner
                         (traced-wall-collision-step-3d body-with-collisions radius min-corner max-corner mat dt)
                         body-with-collisions)])
        body-final))
