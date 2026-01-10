;;; core/diff-physics/diff-collision.ss --- Differentiable Collision Response
;;;
;;; Impulse-based collision response with gradient support.
;;; Combines soft contact forces with rigid body dynamics.
;;;
;;; For fully differentiable physics, this module provides:
;;; 1. Soft contact model (via smooth-collision.ss)
;;; 2. Contact geometry gradients (analytical)
;;; 3. Integration with traced rigid bodies
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/vec2.ss
;;;   - autodiff/reverse-diff.ss
;;;   - diff-physics/traced-vec2.ss
;;;   - diff-physics/traced-body.ss
;;;   - diff-physics/smooth-collision.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec2.ss")
(load "core/autodiff/reverse-diff.ss")
(load "core/diff-physics/traced-vec2.ss")
(load "core/diff-physics/traced-body.ss")
(load "core/diff-physics/smooth-collision.ss")

;;; ============================================================
;;; Circle-Circle Collision Response
;;; ============================================================

;;; traced-circle-collision-force : TracedBody × Number × TracedBody × Number × SoftMaterial → (TracedVec2 × TracedValue)
;;; Compute soft contact force and torque between two circular bodies.
;;; Returns (force-on-A, torque-on-A).
(define (traced-circle-collision-force body-a radius-a body-b radius-b mat)
  (let* ([pos-a (traced-body-pos body-a)]
         [vel-a (traced-body-vel body-a)]
         [pos-b (traced-body-pos body-b)]
         [vel-b (traced-body-vel body-b)]
         [stiffness (soft-material-stiffness mat)]
         [damping (soft-material-damping mat)]
         [alpha (soft-material-alpha mat)]
         [mu (soft-material-friction mat)])
        (call-with-values
         (lambda () (traced-circle-contact pos-a radius-a pos-b radius-b))
         (lambda (contact normal penetration)
                 (let* ([vel-at-a (traced-body-velocity-at body-a contact)]
                        [vel-at-b (traced-body-velocity-at body-b contact)]
                        [rel-vel (traced-vec2-sub vel-at-a vel-at-b)]
                        [approach-vel (traced-neg (traced-vec2-dot rel-vel normal))]
                        [normal-force-mag (traced-soft-contact-force-damped
                                           penetration approach-vel stiffness damping alpha)]
                        [normal-force (traced-vec2-scale normal (traced-neg normal-force-mag))]
                        [vel-normal-component (traced-vec2-scale normal (traced-vec2-dot rel-vel normal))]
                        [tangent-vel (traced-vec2-sub rel-vel vel-normal-component)]
                        [friction-force (traced-soft-friction tangent-vel normal-force-mag mu 10)]
                        [total-force (traced-vec2-add normal-force friction-force)]
                        [r-a (traced-vec2-sub contact pos-a)]
                        [torque-a (traced-vec2-cross r-a total-force)])
                       (values total-force torque-a))))))

;;; traced-circle-collision-impulses : TracedBody × Number × TracedBody × Number × SoftMaterial × Number → (TracedBody × TracedBody)
;;; Apply soft collision forces to both bodies for time dt.
;;; Returns updated (body-a, body-b).
(define (traced-circle-collision-impulses body-a radius-a body-b radius-b mat dt)
  (call-with-values
   (lambda () (traced-circle-collision-force body-a radius-a body-b radius-b mat))
   (lambda (force-a torque-a)
           (let* ([force-b (traced-vec2-neg force-a)]
                  [pos-a (traced-body-pos body-a)]
                  [pos-b (traced-body-pos body-b)])
                 (call-with-values
                  (lambda () (traced-circle-contact pos-a radius-a pos-b radius-b))
                  (lambda (contact _normal _penetration)
                          (let* ([r-b (traced-vec2-sub contact pos-b)]
                                 [torque-b (traced-vec2-cross r-b force-b)]
                                 [new-a (traced-apply-central-force
                                         (traced-apply-torque body-a torque-a dt)
                                         force-a dt)]
                                 [new-b (traced-apply-central-force
                                         (traced-apply-torque body-b torque-b dt)
                                         force-b dt)])
                                (values new-a new-b))))))))

;;; ============================================================
;;; Circle-Ground Collision
;;; ============================================================

;;; traced-ground-collision-force : TracedBody × Number × Number × SoftMaterial → (TracedVec2 × TracedValue)
;;; Soft contact force from horizontal ground at y = ground-y.
;;; Returns (force, torque).
(define (traced-ground-collision-force body radius ground-y mat)
  (let* ([pos (traced-body-pos body)]
         [stiffness (soft-material-stiffness mat)]
         [damping (soft-material-damping mat)]
         [alpha (soft-material-alpha mat)]
         [mu (soft-material-friction mat)])
        ;; Penetration depth (positive when below ground + radius)
        (let* ([penetration (traced-sub (+ ground-y radius)
                                        (traced-vec2-y pos))]
               ;; Contact point (bottom of circle)
               [contact (traced-vec2 (traced-vec2-x pos)
                                     (traced-sub (traced-vec2-y pos) radius))]
               ;; Velocity at contact
               [vel-at-contact (traced-body-velocity-at body contact)]
               ;; Approach velocity (downward velocity)
               [approach-vel (traced-neg (traced-vec2-y vel-at-contact))]
               ;; Normal force magnitude (upward)
               [normal-force-mag (traced-soft-contact-force-damped
                                  penetration approach-vel stiffness damping alpha)]
               ;; Tangent velocity (horizontal)
               [tangent-vel (traced-vec2 (traced-vec2-x vel-at-contact) 0)]
               ;; Friction force
               [friction-force (traced-soft-friction tangent-vel normal-force-mag mu 10)]
               ;; Total force: normal (up) + friction (horizontal)
               [total-force (traced-vec2-add (traced-vec2 0 normal-force-mag)
                                             friction-force)]
               ;; Torque from friction at contact
               [r (traced-vec2-sub contact pos)]
               [torque (traced-vec2-cross r total-force)])
              (values total-force torque))))

;;; traced-ground-collision-step : TracedBody × Number × Number × SoftMaterial × Number → TracedBody
;;; Apply ground collision forces for time dt.
(define (traced-ground-collision-step body radius ground-y mat dt)
  (call-with-values
   (lambda () (traced-ground-collision-force body radius ground-y mat))
   (lambda (force torque)
           (traced-apply-central-force
            (traced-apply-torque body torque dt)
            force dt))))

;;; ============================================================
;;; Multi-Body Collision System
;;; ============================================================

;;; For simulating many bodies, we accumulate forces then integrate.

;;; make-collision-pair : Nat × Nat × Number × Number → CollisionPair
;;; Define a collision pair for the system.
(define (make-collision-pair idx-a idx-b radius-a radius-b)
  (list 'collision-pair idx-a idx-b radius-a radius-b))

;;; collision-pair-idx-a : CollisionPair → Nat
(define (collision-pair-idx-a cp) (list-ref cp 1))

;;; collision-pair-idx-b : CollisionPair → Nat
(define (collision-pair-idx-b cp) (list-ref cp 2))

;;; collision-pair-radius-a : CollisionPair → Number
(define (collision-pair-radius-a cp) (list-ref cp 3))

;;; collision-pair-radius-b : CollisionPair → Number
(define (collision-pair-radius-b cp) (list-ref cp 4))

;;; traced-accumulate-collision-forces : (Vector TracedBody) × (List CollisionPair) × SoftMaterial → (Vector (TracedVec2 × TracedValue))
;;; Compute force and torque on each body from all collision pairs.
;;; Returns vector of (force, torque) per body.
(define (traced-accumulate-collision-forces bodies pairs mat)
  (let* ([n (vector-length bodies)]
         ;; Initialize force accumulators
         [forces (make-vector n)]
         [torques (make-vector n)])
        ;; Initialize to zero
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! forces i (lift-vec2-const (vec2-zero)))
            (vector-set! torques i 0))
        ;; Accumulate from each pair
        (for-each
         (lambda (pair)
                 (let* ([ia (collision-pair-idx-a pair)]
                        [ib (collision-pair-idx-b pair)]
                        [ra (collision-pair-radius-a pair)]
                        [rb (collision-pair-radius-b pair)]
                        [body-a (vector-ref bodies ia)]
                        [body-b (vector-ref bodies ib)])
                       (call-with-values
                        (lambda () (traced-circle-collision-force body-a ra body-b rb mat))
                        (lambda (force-a torque-a)
                                ;; Add to A
                                (vector-set! forces ia
                                             (traced-vec2-add (vector-ref forces ia) force-a))
                                (vector-set! torques ia
                                             (traced-add (vector-ref torques ia) torque-a))
                                ;; Add to B (opposite)
                                (vector-set! forces ib
                                             (traced-vec2-sub (vector-ref forces ib) force-a))
                                (vector-set! torques ib
                                             (traced-sub (vector-ref torques ib) torque-a))))))
         pairs)
        (values forces torques)))

;;; ============================================================
;;; Restitution Coefficient (for hard contact comparison)
;;; ============================================================

;;; In soft contact, restitution is implicitly controlled by stiffness/damping.
;;; For comparison with impulse-based methods, we can compute effective restitution.

;;; effective-restitution : Number × Number × Number × Number → Number
;;; Compute effective coefficient of restitution from material parameters.
;;;   stiffness: contact stiffness
;;;   damping: contact damping
;;;   mass: effective mass at contact
;;;   approach-vel: initial approach velocity
;;; This is an approximation based on linearized contact dynamics.
(define (effective-restitution stiffness damping mass approach-vel)
  (let* ([omega (sqrt (/ stiffness mass))]
         [zeta (/ damping (* 2 (sqrt (* stiffness mass))))])  ; damping ratio
        (if (>= zeta 1)
            0  ; overdamped = no bounce
            (exp (/ (* (- pi) zeta) (sqrt (- 1 (* zeta zeta))))))))

;;; ============================================================
;;; Contact Detection Utilities
;;; ============================================================

;;; These help determine which pairs need collision response.
;;; Note: For differentiable physics, we often want soft forces even
;;; for non-contacting bodies (with smooth cutoff).

;;; traced-circles-overlapping? : TracedVec2 × Number × TracedVec2 × Number → Boolean
;;; Check if two circles overlap (for filtering).
;;; Uses traced values but returns boolean (non-differentiable).
(define (traced-circles-overlapping? pos-a radius-a pos-b radius-b)
  (let ([dist (traced-value (traced-vec2-distance pos-a pos-b))])
       (< dist (+ radius-a radius-b))))

;;; traced-circle-ground-overlapping? : TracedVec2 × Number × Number → Boolean
;;; Check if circle overlaps ground plane.
(define (traced-circle-ground-overlapping? pos radius ground-y)
  (< (traced-value (traced-vec2-y pos)) (+ ground-y radius)))

;;; ============================================================
;;; Complete Collision Step
;;; ============================================================

;;; traced-collision-step : TracedBody × Number × (List (TracedBody × Number)) × Number × SoftMaterial × Number → TracedBody
;;; Apply collision forces from other bodies and ground for one timestep.
;;;   body: the body to update
;;;   radius: body's collision radius
;;;   others: list of (other-body, other-radius) pairs
;;;   ground-y: ground plane y-coordinate (or #f for no ground)
;;;   mat: soft material properties
;;;   dt: timestep
(define (traced-collision-step body radius others ground-y mat dt)
  (let* (;; Accumulate forces from all collisions
         [initial-force (lift-vec2-const (vec2-zero))]
         [initial-torque 0]
         ;; Add forces from other bodies
         [body-result
          (fold-left
           (lambda (acc other)
                   (let* ([other-body (car other)]
                          [other-radius (cadr other)]
                          [force-acc (car acc)]
                          [torque-acc (cadr acc)])
                         (call-with-values
                          (lambda () (traced-circle-collision-force body radius other-body other-radius mat))
                          (lambda (f t)
                                  (list (traced-vec2-add force-acc f)
                                        (traced-add torque-acc t))))))
           (list initial-force initial-torque)
           others)]
         [force-from-bodies (car body-result)]
         [torque-from-bodies (cadr body-result)]
         ;; Add ground force if ground specified
         [ground-result
          (if ground-y
              (call-with-values
               (lambda () (traced-ground-collision-force body radius ground-y mat))
               (lambda (gf gt)
                       (list (traced-vec2-add force-from-bodies gf)
                             (traced-add torque-from-bodies gt))))
              body-result)]
         [total-force (car ground-result)]
         [total-torque (cadr ground-result)])
        ;; Apply forces
        (traced-apply-central-force
         (traced-apply-torque body total-torque dt)
         total-force dt)))
