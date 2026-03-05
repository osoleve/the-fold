(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module diff-constraints3d
;;; @requires prelude vec3 reverse-diff traced-vec3 traced-quaternion traced-body3d
(require 'prelude)
(require 'vec3)
(require 'reverse-diff)
(require 'traced-vec3)
(require 'traced-quaternion)
(require 'traced-body3d)

(doc 'module 'diff-constraints3d)
(doc 'description "Differentiable 3D Constraint Solver - Constraint system with gradient support using truncated unrolling. Implements soft constraint forces that enable differentiable simulation.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Supported constraint types: Spring constraint (soft distance with stiffness), Distance constraint (stiff spring), Ball-socket joint (point-to-point, 3 DOF removed), Hinge joint (5 DOF removed), Fixed joint (6 DOF removed), Anchor constraint (pin to world point)")

(doc 'section 'spring-constraints)

(doc 'note "Springs are inherently smooth - the simplest differentiable constraint.")

;;; traced-spring-force-3d : TracedVec3 × TracedVec3 × Number × Number → TracedVec3
;;; Compute spring force between two points.
;;;   a, b: attachment points
;;;   rest-length: natural length of spring
;;;   stiffness: spring constant (k in F = -kx)
;;; Returns force on point a (negate for b).
(define (traced-spring-force-3d a b rest-length stiffness)
  (doc 'export #t)
  (let* ([delta (traced-vec3-sub b a)]
         [dist (traced-vec3-smooth-magnitude delta 1e-8)]
         [stretch (traced-sub dist rest-length)]
         ;; Force magnitude: F = k * stretch
         [force-mag (traced-mul stiffness stretch)]
         ;; Direction: toward b when stretched
         [dir (traced-vec3-smooth-normalize delta 1e-8)])
        (traced-vec3-scale dir force-mag)))

;;; traced-spring-force-damped-3d : TracedVec3 × TracedVec3 × TracedVec3 × TracedVec3 × Number × Number × Number → TracedVec3
;;; Spring force with velocity damping.
;;;   va, vb: velocities at attachment points
(define (traced-spring-force-damped-3d a b va vb rest-length stiffness damping)
  (doc 'export #t)
  (let* ([delta (traced-vec3-sub b a)]
         [dist (traced-vec3-smooth-magnitude delta 1e-8)]
         [dir (traced-vec3-smooth-normalize delta 1e-8)]
         ;; Spring force
         [stretch (traced-sub dist rest-length)]
         [spring-force-mag (traced-mul stiffness stretch)]
         ;; Damping force (relative velocity along spring axis)
         [rel-vel (traced-vec3-sub vb va)]
         [rel-vel-along (traced-vec3-dot rel-vel dir)]
         [damp-force-mag (traced-mul damping rel-vel-along)]
         ;; Total force
         [total-mag (traced-add spring-force-mag damp-force-mag)])
        (traced-vec3-scale dir total-mag)))

;;; make-spring-constraint-3d : Nat × Vec3 × Nat × Vec3 × Number × Number × Number → SpringConstraint3D
;;; Create a spring constraint between two bodies.
;;;   body-a-idx: index of first body
;;;   anchor-a: local attachment point on body A
;;;   body-b-idx: index of second body
;;;   anchor-b: local attachment point on body B
(define (make-spring-constraint-3d body-a-idx anchor-a body-b-idx anchor-b
                                   rest-length stiffness damping)
  (list 'spring-constraint-3d
        body-a-idx anchor-a
        body-b-idx anchor-b
        rest-length stiffness damping))

;;; Spring constraint accessors
(define (spring-constraint-3d-body-a c) (list-ref c 1))
(define (spring-constraint-3d-anchor-a c) (list-ref c 2))
(define (spring-constraint-3d-body-b c) (list-ref c 3))
(define (spring-constraint-3d-anchor-b c) (list-ref c 4))
(define (spring-constraint-3d-rest-length c) (list-ref c 5))
(define (spring-constraint-3d-stiffness c) (list-ref c 6))
(define (spring-constraint-3d-damping c) (list-ref c 7))

;;; ====
;;; Distance Constraint (Stiff Spring)
;;; ====

;;; Distance constraints are implemented as very stiff springs.
;;; This is smooth but may require small timesteps for stability.

;;; *distance-constraint-stiffness-3d* : Number
;;; Default stiffness for distance constraints.
(define *distance-constraint-stiffness-3d* 100000)

;;; *distance-constraint-damping-3d* : Number
;;; Default damping for distance constraints.
(define *distance-constraint-damping-3d* 1000)

;;; make-distance-constraint-3d : Nat × Vec3 × Nat × Vec3 × Number → DistanceConstraint3D
;;; Create a distance constraint (rigid connection).
(define (make-distance-constraint-3d body-a-idx anchor-a body-b-idx anchor-b distance)
  (doc 'export #t)
  (make-spring-constraint-3d body-a-idx anchor-a body-b-idx anchor-b
                             distance
                             *distance-constraint-stiffness-3d*
                             *distance-constraint-damping-3d*))

;;; ====
;;; Anchor Constraint (Pin to World)
;;; ====

;;; Anchors a point on a body to a fixed world position.

;;; make-anchor-constraint-3d : Nat × Vec3 × Vec3 × Number × Number → AnchorConstraint3D
;;; Pin a body to a world point.
;;;   body-idx: index of body
;;;   local-anchor: attachment point in body-local coordinates
;;;   world-target: target position in world coordinates
(define (make-anchor-constraint-3d body-idx local-anchor world-target stiffness damping)
  (doc 'export #t)
  (list 'anchor-constraint-3d body-idx local-anchor world-target stiffness damping))

;;; Anchor constraint accessors
(define (anchor-constraint-3d-body c) (list-ref c 1))
(define (anchor-constraint-3d-local-anchor c) (list-ref c 2))
(define (anchor-constraint-3d-world-target c) (list-ref c 3))
(define (anchor-constraint-3d-stiffness c) (list-ref c 4))
(define (anchor-constraint-3d-damping c) (list-ref c 5))

;;; traced-anchor-force-3d : TracedBody3D × Vec3 × Vec3 × Number × Number → (values TracedVec3 TracedVec3)
;;; Compute force and torque from anchor constraint.
;;; Returns (force-on-body, torque-on-body).
(define (traced-anchor-force-3d body local-anchor world-target stiffness damping)
  (doc 'export #t)
  (let* (;; World position of anchor point
         [world-anchor (traced-local-to-world-3d body (lift-vec3-const local-anchor))]
         ;; Velocity at anchor point
         [vel-at-anchor (traced-body-3d-velocity-at body world-anchor)]
         ;; Target is stationary
         [target-vel (lift-vec3-const (vec3-zero))]
         ;; Spring force toward target
         [force (traced-spring-force-damped-3d world-anchor (lift-vec3-const world-target)
                                               vel-at-anchor target-vel
                                               0 stiffness damping)]
         ;; Torque from force at anchor: τ = r × F
         [r (traced-vec3-sub world-anchor (traced-body-3d-pos body))]
         [torque (traced-vec3-cross r force)])
        (values force torque)))

;;; ====
;;; Ball-Socket Joint (Point-to-Point, 3 DOF removed)
;;; ====

;;; A ball-socket joint constrains two bodies to share a pivot point.
;;; Each body can rotate freely around the joint.
;;; Implemented as spring forces pulling anchor points together.

;;; make-ball-socket-joint-3d : Nat × Vec3 × Nat × Vec3 × Number × Number → BallSocketJoint3D
;;; Create a ball-socket joint between two bodies.
;;;   anchor-a: pivot point in body A's local coordinates
;;;   anchor-b: pivot point in body B's local coordinates
(define (make-ball-socket-joint-3d body-a-idx anchor-a body-b-idx anchor-b stiffness damping)
  (doc 'export #t)
  (list 'ball-socket-joint-3d body-a-idx anchor-a body-b-idx anchor-b stiffness damping))

;;; Ball-socket joint accessors
(define (ball-socket-joint-3d-body-a j) (list-ref j 1))
(define (ball-socket-joint-3d-anchor-a j) (list-ref j 2))
(define (ball-socket-joint-3d-body-b j) (list-ref j 3))
(define (ball-socket-joint-3d-anchor-b j) (list-ref j 4))
(define (ball-socket-joint-3d-stiffness j) (list-ref j 5))
(define (ball-socket-joint-3d-damping j) (list-ref j 6))

;;; traced-ball-socket-forces-3d : TracedBody3D × Vec3 × TracedBody3D × Vec3 × Number × Number
;;;                              → (values (TracedVec3 TracedVec3) (TracedVec3 TracedVec3))
;;; Compute forces and torques for ball-socket joint.
;;; Returns ((force-a, torque-a), (force-b, torque-b)).
(define (traced-ball-socket-forces-3d body-a anchor-a body-b anchor-b stiffness damping)
  (doc 'export #t)
  (let* (;; World positions of anchors
         [world-a (traced-local-to-world-3d body-a (lift-vec3-const anchor-a))]
         [world-b (traced-local-to-world-3d body-b (lift-vec3-const anchor-b))]
         ;; Velocities at anchors
         [vel-a (traced-body-3d-velocity-at body-a world-a)]
         [vel-b (traced-body-3d-velocity-at body-b world-b)]
         ;; Spring force pulling A toward B's position
         [force-a (traced-spring-force-damped-3d world-a world-b vel-a vel-b 0 stiffness damping)]
         [force-b (traced-vec3-neg force-a)]
         ;; Torques: τ = r × F
         [r-a (traced-vec3-sub world-a (traced-body-3d-pos body-a))]
         [r-b (traced-vec3-sub world-b (traced-body-3d-pos body-b))]
         [torque-a (traced-vec3-cross r-a force-a)]
         [torque-b (traced-vec3-cross r-b force-b)])
        (values (list force-a torque-a)
                (list force-b torque-b))))

;;; ====
;;; Hinge Joint (5 DOF removed)
;;; ====

;;; A hinge joint constrains:
;;; 1. Two bodies to share a pivot point (3 DOF from ball-socket)
;;; 2. Their local axes to align (2 DOF from angular alignment)
;;; Bodies can only rotate around the shared hinge axis.

;;; make-hinge-joint-3d : Nat × Vec3 × Vec3 × Nat × Vec3 × Vec3 × Number × Number → HingeJoint3D
;;; Create a hinge joint between two bodies.
;;;   anchor-a, anchor-b: pivot points in local coordinates
;;;   axis-a, axis-b: hinge axis in local coordinates (must align)
(define (make-hinge-joint-3d body-a-idx anchor-a axis-a body-b-idx anchor-b axis-b stiffness damping)
  (doc 'export #t)
  (list 'hinge-joint-3d body-a-idx anchor-a axis-a body-b-idx anchor-b axis-b stiffness damping))

;;; Hinge joint accessors
(define (hinge-joint-3d-body-a j) (list-ref j 1))
(define (hinge-joint-3d-anchor-a j) (list-ref j 2))
(define (hinge-joint-3d-axis-a j) (list-ref j 3))
(define (hinge-joint-3d-body-b j) (list-ref j 4))
(define (hinge-joint-3d-anchor-b j) (list-ref j 5))
(define (hinge-joint-3d-axis-b j) (list-ref j 6))
(define (hinge-joint-3d-stiffness j) (list-ref j 7))
(define (hinge-joint-3d-damping j) (list-ref j 8))

;;; traced-hinge-forces-3d : TracedBody3D × Vec3 × Vec3 × TracedBody3D × Vec3 × Vec3 × Number × Number
;;;                        → (values (TracedVec3 TracedVec3) (TracedVec3 TracedVec3))
;;; Compute forces and torques for hinge joint.
;;; Returns ((force-a, torque-a), (force-b, torque-b)).
(define (traced-hinge-forces-3d body-a anchor-a axis-a body-b anchor-b axis-b stiffness damping)
  (doc 'export #t)
  (let* (;; First, apply ball-socket constraint for position
         [world-a (traced-local-to-world-3d body-a (lift-vec3-const anchor-a))]
         [world-b (traced-local-to-world-3d body-b (lift-vec3-const anchor-b))]
         [vel-a (traced-body-3d-velocity-at body-a world-a)]
         [vel-b (traced-body-3d-velocity-at body-b world-b)]
         ;; Position constraint force
         [pos-force-a (traced-spring-force-damped-3d world-a world-b vel-a vel-b 0 stiffness damping)]
         [pos-force-b (traced-vec3-neg pos-force-a)]
         
         ;; Angular constraint: align axes
         ;; World-space axes
         [world-axis-a (traced-local-dir-to-world-3d body-a (lift-vec3-const axis-a))]
         [world-axis-b (traced-local-dir-to-world-3d body-b (lift-vec3-const axis-b))]
         
         ;; The constraint error is that the axes should be parallel.
         ;; We use the cross product: axis-a × axis-b should be zero.
         ;; Torque to align: τ = k * (axis-a × axis-b)
         [axis-error (traced-vec3-cross world-axis-a world-axis-b)]
         
         ;; Angular velocities
         [omega-a (traced-body-3d-angular-vel body-a)]
         [omega-b (traced-body-3d-angular-vel body-b)]
         ;; Relative angular velocity perpendicular to hinge axis
         [rel-omega (traced-vec3-sub omega-b omega-a)]
         ;; Project out the hinge axis component (bodies can rotate around hinge)
         [along-axis (traced-vec3-dot rel-omega world-axis-a)]
         [rel-omega-perp (traced-vec3-sub rel-omega (traced-vec3-scale world-axis-a along-axis))]
         
         ;; Angular spring-damper torque
         [spring-torque (traced-vec3-scale axis-error stiffness)]
         [damp-torque (traced-vec3-scale rel-omega-perp damping)]
         [angular-torque-a (traced-vec3-add spring-torque damp-torque)]
         [angular-torque-b (traced-vec3-neg angular-torque-a)]
         
         ;; Position torques
         [r-a (traced-vec3-sub world-a (traced-body-3d-pos body-a))]
         [r-b (traced-vec3-sub world-b (traced-body-3d-pos body-b))]
         [pos-torque-a (traced-vec3-cross r-a pos-force-a)]
         [pos-torque-b (traced-vec3-cross r-b pos-force-b)]
         
         ;; Total torques
         [total-torque-a (traced-vec3-add pos-torque-a angular-torque-a)]
         [total-torque-b (traced-vec3-add pos-torque-b angular-torque-b)])
        
        (values (list pos-force-a total-torque-a)
                (list pos-force-b total-torque-b))))

;;; ====
;;; Fixed Joint (6 DOF removed)
;;; ====

;;; A fixed joint constrains:
;;; 1. Position (3 DOF from ball-socket)
;;; 2. All rotation (3 DOF from relative orientation)
;;; No relative motion allowed.

;;; make-fixed-joint-3d : Nat × Vec3 × Nat × Vec3 × Quat × Number × Number → FixedJoint3D
;;; Create a fixed joint between two bodies.
;;;   ref-orientation: desired relative orientation (B relative to A)
(define (make-fixed-joint-3d body-a-idx anchor-a body-b-idx anchor-b ref-orientation stiffness damping)
  (doc 'export #t)
  (list 'fixed-joint-3d body-a-idx anchor-a body-b-idx anchor-b ref-orientation stiffness damping))

;;; Fixed joint accessors
(define (fixed-joint-3d-body-a j) (list-ref j 1))
(define (fixed-joint-3d-anchor-a j) (list-ref j 2))
(define (fixed-joint-3d-body-b j) (list-ref j 3))
(define (fixed-joint-3d-anchor-b j) (list-ref j 4))
(define (fixed-joint-3d-ref-orientation j) (list-ref j 5))
(define (fixed-joint-3d-stiffness j) (list-ref j 6))
(define (fixed-joint-3d-damping j) (list-ref j 7))

;;; traced-fixed-forces-3d : TracedBody3D × Vec3 × TracedBody3D × Vec3 × Quat × Number × Number
;;;                        → (values (TracedVec3 TracedVec3) (TracedVec3 TracedVec3))
;;; Compute forces and torques for fixed joint.
;;; Returns ((force-a, torque-a), (force-b, torque-b)).
(define (traced-fixed-forces-3d body-a anchor-a body-b anchor-b ref-orientation stiffness damping)
  (doc 'export #t)
  (let* (;; Position constraint (same as ball-socket)
         [world-a (traced-local-to-world-3d body-a (lift-vec3-const anchor-a))]
         [world-b (traced-local-to-world-3d body-b (lift-vec3-const anchor-b))]
         [vel-a (traced-body-3d-velocity-at body-a world-a)]
         [vel-b (traced-body-3d-velocity-at body-b world-b)]
         [pos-force-a (traced-spring-force-damped-3d world-a world-b vel-a vel-b 0 stiffness damping)]
         [pos-force-b (traced-vec3-neg pos-force-a)]
         
         ;; Orientation constraint: B should have orientation A * ref
         ;; Current orientations
         [q-a (traced-body-3d-orientation body-a)]
         [q-b (traced-body-3d-orientation body-b)]
         
         ;; Desired orientation of B: q_desired = q_a * ref
         ;; Error quaternion: q_error = q_b * conj(q_desired) = q_b * conj(ref) * conj(q_a)
         ;; For small errors, the vector part of q_error gives the rotation axis * sin(angle/2)
         ;; This gives us a torque direction.
         
         ;; Compute q_a * ref
         [q-desired (traced-quat-mul q-a (lift-quat-const ref-orientation))]
         ;; Error: q_error = q_b * conj(q_desired)
         [q-error (traced-quat-mul q-b (traced-quat-conjugate q-desired))]
         
         ;; Extract axis-angle error from quaternion vector part
         ;; For small angles: (qx, qy, qz) ≈ axis * sin(θ/2) ≈ axis * θ/2
         ;; So torque ∝ 2 * (qx, qy, qz)
         [error-x (traced-quat-x q-error)]
         [error-y (traced-quat-y q-error)]
         [error-z (traced-quat-z q-error)]
         ;; The w component tells us if we need to flip (when w < 0, use -vec part)
         ;; For smooth differentiability, we scale by sign of w
         [qw (traced-quat-w q-error)]
         ;; Use 2 * vec * sign(w) to get error in consistent direction
         ;; For small angles, this approximates the rotation vector
         [error-vec (traced-vec3 (traced-mul 2 error-x)
                                 (traced-mul 2 error-y)
                                 (traced-mul 2 error-z))]
         
         ;; Angular velocities
         [omega-a (traced-body-3d-angular-vel body-a)]
         [omega-b (traced-body-3d-angular-vel body-b)]
         [rel-omega (traced-vec3-sub omega-b omega-a)]
         
         ;; Angular spring-damper torque (applied to align orientations)
         [spring-torque (traced-vec3-scale error-vec stiffness)]
         [damp-torque (traced-vec3-scale rel-omega damping)]
         [angular-torque-a (traced-vec3-add spring-torque damp-torque)]
         [angular-torque-b (traced-vec3-neg angular-torque-a)]
         
         ;; Position torques
         [r-a (traced-vec3-sub world-a (traced-body-3d-pos body-a))]
         [r-b (traced-vec3-sub world-b (traced-body-3d-pos body-b))]
         [pos-torque-a (traced-vec3-cross r-a pos-force-a)]
         [pos-torque-b (traced-vec3-cross r-b pos-force-b)]
         
         ;; Total torques
         [total-torque-a (traced-vec3-add pos-torque-a angular-torque-a)]
         [total-torque-b (traced-vec3-add pos-torque-b angular-torque-b)])
        
        (values (list pos-force-a total-torque-a)
                (list pos-force-b total-torque-b))))

;;; ====
;;; Constraint System Solver
;;; ====

;;; Apply all constraints to a system of bodies.
;;; Uses truncated unrolling: computes forces once per timestep.

;;; constraint-type-3d : Constraint → Symbol
(define (constraint-type-3d c) (car c))

;;; traced-apply-spring-constraint-3d : (Vector TracedBody3D) × SpringConstraint3D × Number → (Vector TracedBody3D)
;;; Apply a spring constraint to the body system.
(define (traced-apply-spring-constraint-3d bodies spring dt)
  (doc 'export #t)
  (let* ([idx-a (spring-constraint-3d-body-a spring)]
         [anchor-a (spring-constraint-3d-anchor-a spring)]
         [idx-b (spring-constraint-3d-body-b spring)]
         [anchor-b (spring-constraint-3d-anchor-b spring)]
         [rest-len (spring-constraint-3d-rest-length spring)]
         [stiffness (spring-constraint-3d-stiffness spring)]
         [damping (spring-constraint-3d-damping spring)]
         [body-a (vector-ref bodies idx-a)]
         [body-b (vector-ref bodies idx-b)]
         ;; World positions of anchors
         [world-a (traced-local-to-world-3d body-a (lift-vec3-const anchor-a))]
         [world-b (traced-local-to-world-3d body-b (lift-vec3-const anchor-b))]
         ;; Velocities at anchors
         [vel-a (traced-body-3d-velocity-at body-a world-a)]
         [vel-b (traced-body-3d-velocity-at body-b world-b)]
         ;; Spring force on A
         [force-a (traced-spring-force-damped-3d world-a world-b vel-a vel-b
                                                 rest-len stiffness damping)]
         [force-b (traced-vec3-neg force-a)]
         ;; Torques
         [r-a (traced-vec3-sub world-a (traced-body-3d-pos body-a))]
         [r-b (traced-vec3-sub world-b (traced-body-3d-pos body-b))]
         [torque-a (traced-vec3-cross r-a force-a)]
         [torque-b (traced-vec3-cross r-b force-b)]
         ;; Apply forces
         [new-a (traced-apply-central-force-3d
                 (traced-apply-torque-3d body-a torque-a dt)
                 force-a dt)]
         [new-b (traced-apply-central-force-3d
                 (traced-apply-torque-3d body-b torque-b dt)
                 force-b dt)]
         ;; Update vector
         [result (vector-copy bodies)])
        (vector-set! result idx-a new-a)
        (vector-set! result idx-b new-b)
        result))

;;; traced-apply-anchor-constraint-3d : (Vector TracedBody3D) × AnchorConstraint3D × Number → (Vector TracedBody3D)
;;; Apply an anchor constraint to the body system.
(define (traced-apply-anchor-constraint-3d bodies anchor dt)
  (doc 'export #t)
  (let* ([idx (anchor-constraint-3d-body anchor)]
         [local-anchor (anchor-constraint-3d-local-anchor anchor)]
         [world-target (anchor-constraint-3d-world-target anchor)]
         [stiffness (anchor-constraint-3d-stiffness anchor)]
         [damping (anchor-constraint-3d-damping anchor)]
         [body (vector-ref bodies idx)])
        (call-with-values
         (lambda () (traced-anchor-force-3d body local-anchor world-target stiffness damping))
         (lambda (force torque)
                 (let* ([new-body (traced-apply-central-force-3d
                                   (traced-apply-torque-3d body torque dt)
                                   force dt)]
                        [result (vector-copy bodies)])
                       (vector-set! result idx new-body)
                       result)))))

;;; traced-apply-ball-socket-joint-3d : (Vector TracedBody3D) × BallSocketJoint3D × Number → (Vector TracedBody3D)
;;; Apply a ball-socket joint to the body system.
(define (traced-apply-ball-socket-joint-3d bodies joint dt)
  (doc 'export #t)
  (let* ([idx-a (ball-socket-joint-3d-body-a joint)]
         [anchor-a (ball-socket-joint-3d-anchor-a joint)]
         [idx-b (ball-socket-joint-3d-body-b joint)]
         [anchor-b (ball-socket-joint-3d-anchor-b joint)]
         [stiffness (ball-socket-joint-3d-stiffness joint)]
         [damping (ball-socket-joint-3d-damping joint)]
         [body-a (vector-ref bodies idx-a)]
         [body-b (vector-ref bodies idx-b)])
        (call-with-values
         (lambda () (traced-ball-socket-forces-3d body-a anchor-a body-b anchor-b stiffness damping))
         (lambda (result-a result-b)
                 (let* ([force-a (car result-a)]
                        [torque-a (cadr result-a)]
                        [force-b (car result-b)]
                        [torque-b (cadr result-b)]
                        [new-a (traced-apply-central-force-3d
                                (traced-apply-torque-3d body-a torque-a dt)
                                force-a dt)]
                        [new-b (traced-apply-central-force-3d
                                (traced-apply-torque-3d body-b torque-b dt)
                                force-b dt)]
                        [result (vector-copy bodies)])
                       (vector-set! result idx-a new-a)
                       (vector-set! result idx-b new-b)
                       result)))))

;;; traced-apply-hinge-joint-3d : (Vector TracedBody3D) × HingeJoint3D × Number → (Vector TracedBody3D)
;;; Apply a hinge joint to the body system.
(define (traced-apply-hinge-joint-3d bodies joint dt)
  (doc 'export #t)
  (let* ([idx-a (hinge-joint-3d-body-a joint)]
         [anchor-a (hinge-joint-3d-anchor-a joint)]
         [axis-a (hinge-joint-3d-axis-a joint)]
         [idx-b (hinge-joint-3d-body-b joint)]
         [anchor-b (hinge-joint-3d-anchor-b joint)]
         [axis-b (hinge-joint-3d-axis-b joint)]
         [stiffness (hinge-joint-3d-stiffness joint)]
         [damping (hinge-joint-3d-damping joint)]
         [body-a (vector-ref bodies idx-a)]
         [body-b (vector-ref bodies idx-b)])
        (call-with-values
         (lambda () (traced-hinge-forces-3d body-a anchor-a axis-a body-b anchor-b axis-b stiffness damping))
         (lambda (result-a result-b)
                 (let* ([force-a (car result-a)]
                        [torque-a (cadr result-a)]
                        [force-b (car result-b)]
                        [torque-b (cadr result-b)]
                        [new-a (traced-apply-central-force-3d
                                (traced-apply-torque-3d body-a torque-a dt)
                                force-a dt)]
                        [new-b (traced-apply-central-force-3d
                                (traced-apply-torque-3d body-b torque-b dt)
                                force-b dt)]
                        [result (vector-copy bodies)])
                       (vector-set! result idx-a new-a)
                       (vector-set! result idx-b new-b)
                       result)))))

;;; traced-apply-fixed-joint-3d : (Vector TracedBody3D) × FixedJoint3D × Number → (Vector TracedBody3D)
;;; Apply a fixed joint to the body system.
(define (traced-apply-fixed-joint-3d bodies joint dt)
  (doc 'export #t)
  (let* ([idx-a (fixed-joint-3d-body-a joint)]
         [anchor-a (fixed-joint-3d-anchor-a joint)]
         [idx-b (fixed-joint-3d-body-b joint)]
         [anchor-b (fixed-joint-3d-anchor-b joint)]
         [ref-orientation (fixed-joint-3d-ref-orientation joint)]
         [stiffness (fixed-joint-3d-stiffness joint)]
         [damping (fixed-joint-3d-damping joint)]
         [body-a (vector-ref bodies idx-a)]
         [body-b (vector-ref bodies idx-b)])
        (call-with-values
         (lambda () (traced-fixed-forces-3d body-a anchor-a body-b anchor-b ref-orientation stiffness damping))
         (lambda (result-a result-b)
                 (let* ([force-a (car result-a)]
                        [torque-a (cadr result-a)]
                        [force-b (car result-b)]
                        [torque-b (cadr result-b)]
                        [new-a (traced-apply-central-force-3d
                                (traced-apply-torque-3d body-a torque-a dt)
                                force-a dt)]
                        [new-b (traced-apply-central-force-3d
                                (traced-apply-torque-3d body-b torque-b dt)
                                force-b dt)]
                        [result (vector-copy bodies)])
                       (vector-set! result idx-a new-a)
                       (vector-set! result idx-b new-b)
                       result)))))

;;; traced-apply-constraint-3d : (Vector TracedBody3D) × Constraint × Number → (Vector TracedBody3D)
;;; Apply any constraint type.
(define (traced-apply-constraint-3d bodies constraint dt)
  (doc 'export #t)
  (case (constraint-type-3d constraint)
        [(spring-constraint-3d) (traced-apply-spring-constraint-3d bodies constraint dt)]
        [(anchor-constraint-3d) (traced-apply-anchor-constraint-3d bodies constraint dt)]
        [(ball-socket-joint-3d) (traced-apply-ball-socket-joint-3d bodies constraint dt)]
        [(hinge-joint-3d) (traced-apply-hinge-joint-3d bodies constraint dt)]
        [(fixed-joint-3d) (traced-apply-fixed-joint-3d bodies constraint dt)]
        [else bodies]))

;;; traced-apply-all-constraints-3d : (Vector TracedBody3D) × (List Constraint) × Number → (Vector TracedBody3D)
;;; Apply all constraints to the system.
(define (traced-apply-all-constraints-3d bodies constraints dt)
  (doc 'export #t)
  (fold-left
   (lambda (current-bodies constraint)
           (traced-apply-constraint-3d current-bodies constraint dt))
   bodies
   constraints))

;;; ====
;;; Complete Constraint Solver Step
;;; ====

;;; traced-constraint-step-3d : (Vector TracedBody3D) × (List Constraint) × Vec3 × Number × Nat → (Vector TracedBody3D)
;;; One complete constraint solver step with optional iteration.
;;;   gravity: gravity acceleration
;;;   iterations: number of constraint iterations per step (default 1 for differentiability)
;;;
;;; For differentiable physics, use iterations=1 (or small k).
;;; Higher iterations improve accuracy but increase gradient complexity.
(define (traced-constraint-step-3d bodies constraints gravity dt iterations)
  (doc 'export #t)
  (let loop ([current bodies] [i 0])
       (if (>= i iterations)
           current
           (let* (;; Apply gravity to all bodies
                  [with-gravity
                   (vector-map-3d
                    (lambda (body)
                            (if (traced-body-3d-static? body)
                                body
                                (traced-apply-central-force-3d body (lift-vec3-const gravity) dt)))
                    current)]
                  ;; Apply constraints
                  [with-constraints (traced-apply-all-constraints-3d with-gravity constraints dt)])
                 (loop with-constraints (+ i 1))))))

;;; Helper: vector-map for mutable vectors
(define (vector-map-3d f v)
  (doc 'export #t)
  (let* ([n (vector-length v)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (f (vector-ref v i))))))

;;; ====
;;; Pendulum Example Helpers
;;; ====

;;; make-pendulum-system-3d : Vec3 × Number × Number × Number × Tape → (TracedBody3D × AnchorConstraint3D)
;;; Create a single pendulum bob anchored at a pivot.
(define (make-pendulum-system-3d pivot length mass radius tape)
  (doc 'export #t)
  (let* (;; Bob starts directly below pivot
         [bob-pos (lift-vec3 (vec3 (vec3-x pivot) (- (vec3-y pivot) length) (vec3-z pivot)) tape)]
         [bob-vel (lift-vec3 (vec3-zero) tape)]
         [bob-orient (lift-quat (quat-identity) tape)]
         [bob-omega (lift-vec3 (vec3-zero) tape)]
         [inertia (inertia-solid-sphere mass radius)]
         [bob (make-traced-body-3d bob-pos bob-vel bob-orient bob-omega mass inertia)]
         ;; Anchor at pivot
         [constraint (make-anchor-constraint-3d
                      0
                      (vec3-zero)  ; anchor at center
                      pivot
                      *distance-constraint-stiffness-3d*
                      *distance-constraint-damping-3d*)])
        (values bob constraint)))

;;; make-chain-system-3d : Vec3 × Number × Number × Number × Number × Tape → (Vector TracedBody3D × List Constraint)
;;; Create a chain of linked bodies.
(define (make-chain-system-3d start-pos num-links link-length mass radius tape)
  (doc 'export #t)
  (let* ([inertia (inertia-solid-sphere mass radius)]
         ;; Create bodies
         [bodies
          (list->vector
           (let loop ([i 0] [y (vec3-y start-pos)])
                (if (= i num-links)
                    '()
                    (let ([pos (lift-vec3 (vec3 (vec3-x start-pos) y (vec3-z start-pos)) tape)]
                          [vel (lift-vec3 (vec3-zero) tape)]
                          [orient (lift-quat (quat-identity) tape)]
                          [omega (lift-vec3 (vec3-zero) tape)])
                         (cons (make-traced-body-3d pos vel orient omega mass inertia)
                               (loop (+ i 1) (- y link-length)))))))]
         ;; Create constraints: anchor first body, link rest with ball-socket joints
         [anchor (make-anchor-constraint-3d
                  0
                  (vec3-zero)
                  start-pos
                  *distance-constraint-stiffness-3d*
                  *distance-constraint-damping-3d*)]
         [links
          (let loop ([i 0])
               (if (>= i (- num-links 1))
                   '()
                   (cons (make-ball-socket-joint-3d
                          i (vec3-zero)
                          (+ i 1) (vec3-zero)
                          *distance-constraint-stiffness-3d*
                          *distance-constraint-damping-3d*)
                         (loop (+ i 1)))))])
        (values bodies (cons anchor links))))

