(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module diff-constraints
;;; @requires prelude vec2 reverse-diff traced-vec2 traced-body
(require 'prelude)
(require 'vec2)
(require 'reverse-diff)
(require 'traced-vec2)
(require 'traced-body)

(doc 'module 'diff-constraints)
(doc 'description "Differentiable Constraint Solver

Constraint system with gradient support using truncated unrolling.
Implements soft constraint forces that enable differentiable simulation.

Supported constraint types:
- Distance constraint (fixed distance between points)
- Spring constraint (soft distance with stiffness)
- Revolute joint (shared pivot point)
- Anchor constraint (pin to world point)")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'spring-constraint)

;;; Springs are inherently smooth - the simplest differentiable constraint.

;;; traced-spring-force : TracedVec2 × TracedVec2 × Number × Number → TracedVec2
;;; Compute spring force between two points.
;;;   a, b: attachment points
;;;   rest-length: natural length of spring
;;;   stiffness: spring constant (k in F = -kx)
;;; Returns force on point a (negate for b).
(define (traced-spring-force a b rest-length stiffness)
  (doc 'export #t)
  (let* ([delta (traced-vec2-sub b a)]
         [dist (traced-vec2-smooth-magnitude delta 1e-8)]
         [stretch (traced-sub dist rest-length)]
         ;; Force magnitude: F = k * stretch
         [force-mag (traced-mul stiffness stretch)]
         ;; Direction: toward b when stretched
         [dir (traced-vec2-smooth-normalize delta 1e-8)])
        (traced-vec2-scale dir force-mag)))

;;; traced-spring-force-damped : TracedVec2 × TracedVec2 × TracedVec2 × TracedVec2 × Number × Number × Number → TracedVec2
;;; Spring force with velocity damping.
;;;   va, vb: velocities at attachment points
(define (traced-spring-force-damped a b va vb rest-length stiffness damping)
  (doc 'export #t)
  (let* ([delta (traced-vec2-sub b a)]
         [dist (traced-vec2-smooth-magnitude delta 1e-8)]
         [dir (traced-vec2-smooth-normalize delta 1e-8)]
         ;; Spring force
         [stretch (traced-sub dist rest-length)]
         [spring-force-mag (traced-mul stiffness stretch)]
         ;; Damping force (relative velocity along spring axis)
         [rel-vel (traced-vec2-sub vb va)]
         [rel-vel-along (traced-vec2-dot rel-vel dir)]
         [damp-force-mag (traced-mul damping rel-vel-along)]
         ;; Total force
         [total-mag (traced-add spring-force-mag damp-force-mag)])
        (traced-vec2-scale dir total-mag)))

;;; make-spring-constraint : Nat × TracedVec2 × Nat × TracedVec2 × Number × Number × Number → SpringConstraint
;;; Create a spring constraint between two bodies.
;;;   body-a-idx: index of first body
;;;   anchor-a: local attachment point on body A
;;;   body-b-idx: index of second body
;;;   anchor-b: local attachment point on body B
(define (make-spring-constraint body-a-idx anchor-a body-b-idx anchor-b
                                rest-length stiffness damping)
  (list 'spring-constraint
        body-a-idx anchor-a
        body-b-idx anchor-b
        rest-length stiffness damping))

;;; spring-constraint? : Any → Boolean
(define (spring-constraint? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'spring-constraint)))

;;; Spring constraint accessors
(define (spring-constraint-body-a c) (list-ref c 1))
(define (spring-constraint-anchor-a c) (list-ref c 2))
(define (spring-constraint-body-b c) (list-ref c 3))
(define (spring-constraint-anchor-b c) (list-ref c 4))
(define (spring-constraint-rest-length c) (list-ref c 5))
(define (spring-constraint-stiffness c) (list-ref c 6))
(define (spring-constraint-damping c) (list-ref c 7))

;;; ====
;;; Distance Constraint (Stiff Spring)
;;; ====

;;; Distance constraints are implemented as very stiff springs.
;;; This is smooth but may require small timesteps for stability.

;;; *distance-constraint-stiffness* : Number
;;; Default stiffness for distance constraints.
(define *distance-constraint-stiffness* 100000)

;;; *distance-constraint-damping* : Number
;;; Default damping for distance constraints.
(define *distance-constraint-damping* 1000)

;;; make-distance-constraint : Nat × TracedVec2 × Nat × TracedVec2 × Number → DistanceConstraint
;;; Create a distance constraint (rigid connection).
(define (make-distance-constraint body-a-idx anchor-a body-b-idx anchor-b distance)
  (doc 'export #t)
  (make-spring-constraint body-a-idx anchor-a body-b-idx anchor-b
                          distance
                          *distance-constraint-stiffness*
                          *distance-constraint-damping*))

;;; ====
;;; Anchor Constraint (Pin to World)
;;; ====

;;; Anchors a point on a body to a fixed world position.

;;; make-anchor-constraint : Nat × TracedVec2 × TracedVec2 × Number × Number → AnchorConstraint
;;; Pin a body to a world point.
;;;   body-idx: index of body
;;;   local-anchor: attachment point in body-local coordinates
;;;   world-target: target position in world coordinates
(define (make-anchor-constraint body-idx local-anchor world-target stiffness damping)
  (doc 'export #t)
  (list 'anchor-constraint body-idx local-anchor world-target stiffness damping))

;;; anchor-constraint? : Any → Boolean
(define (anchor-constraint? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'anchor-constraint)))

;;; Anchor constraint accessors
(define (anchor-constraint-body c) (list-ref c 1))
(define (anchor-constraint-local-anchor c) (list-ref c 2))
(define (anchor-constraint-world-target c) (list-ref c 3))
(define (anchor-constraint-stiffness c) (list-ref c 4))
(define (anchor-constraint-damping c) (list-ref c 5))

;;; traced-anchor-force : TracedBody × TracedVec2 × TracedVec2 × Number × Number → (TracedVec2 × TracedValue)
;;; Compute force and torque from anchor constraint.
;;; Returns (force-on-body, torque-on-body).
(define (traced-anchor-force body local-anchor world-target stiffness damping)
  (doc 'export #t)
  (let* (;; World position of anchor point
         [world-anchor (traced-local-to-world body local-anchor)]
         ;; Velocity at anchor point
         [vel-at-anchor (traced-body-velocity-at body world-anchor)]
         ;; Target is stationary
         [target-vel (lift-vec2-const (vec2-zero))]
         ;; Spring force toward target
         [force (traced-spring-force-damped world-anchor world-target
                                            vel-at-anchor target-vel
                                            0 stiffness damping)]
         ;; Torque from force at anchor
         [r (traced-vec2-sub world-anchor (traced-body-pos body))]
         [torque (traced-vec2-cross r force)])
        (values force torque)))

;;; ====
;;; Revolute Joint (Shared Pivot)
;;; ====

;;; A revolute joint constrains two bodies to share a pivot point.
;;; Each body can rotate freely around the joint.
;;; Implemented as two anchor constraints pulling toward the same point.

;;; make-revolute-joint : Nat × TracedVec2 × Nat × TracedVec2 × Number × Number → RevoluteJoint
;;; Create a revolute joint between two bodies.
;;;   anchor-a: pivot point in body A's local coordinates
;;;   anchor-b: pivot point in body B's local coordinates
(define (make-revolute-joint body-a-idx anchor-a body-b-idx anchor-b stiffness damping)
  (doc 'export #t)
  (list 'revolute-joint body-a-idx anchor-a body-b-idx anchor-b stiffness damping))

;;; revolute-joint? : Any → Boolean
(define (revolute-joint? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'revolute-joint)))

;;; Revolute joint accessors
(define (revolute-joint-body-a j) (list-ref j 1))
(define (revolute-joint-anchor-a j) (list-ref j 2))
(define (revolute-joint-body-b j) (list-ref j 3))
(define (revolute-joint-anchor-b j) (list-ref j 4))
(define (revolute-joint-stiffness j) (list-ref j 5))
(define (revolute-joint-damping j) (list-ref j 6))

;;; traced-revolute-forces : TracedBody × TracedVec2 × TracedBody × TracedVec2 × Number × Number → ((TracedVec2 × TracedValue) × (TracedVec2 × TracedValue))
;;; Compute forces and torques for revolute joint.
;;; Returns ((force-a, torque-a), (force-b, torque-b)).
(define (traced-revolute-forces body-a anchor-a body-b anchor-b stiffness damping)
  (doc 'export #t)
  (let* (;; World positions of anchors
         [world-a (traced-local-to-world body-a anchor-a)]
         [world-b (traced-local-to-world body-b anchor-b)]
         ;; Velocities at anchors
         [vel-a (traced-body-velocity-at body-a world-a)]
         [vel-b (traced-body-velocity-at body-b world-b)]
         ;; Spring force pulling A toward B's position
         [force-a (traced-spring-force-damped world-a world-b vel-a vel-b 0 stiffness damping)]
         [force-b (traced-vec2-neg force-a)]
         ;; Torques
         [r-a (traced-vec2-sub world-a (traced-body-pos body-a))]
         [r-b (traced-vec2-sub world-b (traced-body-pos body-b))]
         [torque-a (traced-vec2-cross r-a force-a)]
         [torque-b (traced-vec2-cross r-b force-b)])
        (values (list force-a torque-a)
                (list force-b torque-b))))

;;; ====
;;; Angular Spring (Rotational Constraint)
;;; ====

;;; Constrains the relative angle between two bodies.

;;; traced-angular-spring-torque : TracedValue × TracedValue × TracedValue × TracedValue × Number × Number × Number → TracedValue
;;; Compute torque from angular spring.
;;;   angle-a, angle-b: body angles
;;;   omega-a, omega-b: body angular velocities
;;;   target-diff: desired angle difference (b - a)
;;; Returns torque on body A (negate for B).
(define (traced-angular-spring-torque angle-a angle-b omega-a omega-b
                                      target-diff stiffness damping)
  (let* (;; Current angle difference
         [angle-diff (traced-sub angle-b angle-a)]
         ;; Error from target
         [error (traced-sub angle-diff target-diff)]
         ;; Spring torque
         [spring-torque (traced-mul stiffness error)]
         ;; Damping torque (relative angular velocity)
         [rel-omega (traced-sub omega-b omega-a)]
         [damp-torque (traced-mul damping rel-omega)]
         ;; Total torque on A (positive = CCW)
         [total (traced-add spring-torque damp-torque)])
        total))

;;; ====
;;; Constraint System Solver
;;; ====

;;; Apply all constraints to a system of bodies.
;;; Uses truncated unrolling: computes forces once per timestep.

;;; constraint-type : Constraint → Symbol
(define (constraint-type c) (car c))

;;; traced-apply-spring-constraint : (Vector TracedBody) × SpringConstraint × Number → (Vector TracedBody)
;;; Apply a spring constraint to the body system.
(define (traced-apply-spring-constraint bodies spring dt)
  (doc 'export #t)
  (let* ([idx-a (spring-constraint-body-a spring)]
         [anchor-a (spring-constraint-anchor-a spring)]
         [idx-b (spring-constraint-body-b spring)]
         [anchor-b (spring-constraint-anchor-b spring)]
         [rest-len (spring-constraint-rest-length spring)]
         [stiffness (spring-constraint-stiffness spring)]
         [damping (spring-constraint-damping spring)]
         [body-a (vector-ref bodies idx-a)]
         [body-b (vector-ref bodies idx-b)]
         ;; World positions of anchors
         [world-a (traced-local-to-world body-a anchor-a)]
         [world-b (traced-local-to-world body-b anchor-b)]
         ;; Velocities at anchors
         [vel-a (traced-body-velocity-at body-a world-a)]
         [vel-b (traced-body-velocity-at body-b world-b)]
         ;; Spring force on A
         [force-a (traced-spring-force-damped world-a world-b vel-a vel-b
                                              rest-len stiffness damping)]
         [force-b (traced-vec2-neg force-a)]
         ;; Torques
         [r-a (traced-vec2-sub world-a (traced-body-pos body-a))]
         [r-b (traced-vec2-sub world-b (traced-body-pos body-b))]
         [torque-a (traced-vec2-cross r-a force-a)]
         [torque-b (traced-vec2-cross r-b force-b)]
         ;; Apply forces
         [new-a (traced-apply-central-force
                 (traced-apply-torque body-a torque-a dt)
                 force-a dt)]
         [new-b (traced-apply-central-force
                 (traced-apply-torque body-b torque-b dt)
                 force-b dt)]
         ;; Update vector
         [result (vector-copy bodies)])
        (vector-set! result idx-a new-a)
        (vector-set! result idx-b new-b)
        result))

;;; traced-apply-anchor-constraint : (Vector TracedBody) × AnchorConstraint × Number → (Vector TracedBody)
;;; Apply an anchor constraint to the body system.
(define (traced-apply-anchor-constraint bodies anchor dt)
  (doc 'export #t)
  (let* ([idx (anchor-constraint-body anchor)]
         [local-anchor (anchor-constraint-local-anchor anchor)]
         [world-target (anchor-constraint-world-target anchor)]
         [stiffness (anchor-constraint-stiffness anchor)]
         [damping (anchor-constraint-damping anchor)]
         [body (vector-ref bodies idx)])
        (call-with-values
         (lambda () (traced-anchor-force body local-anchor world-target stiffness damping))
         (lambda (force torque)
                 (let* ([new-body (traced-apply-central-force
                                   (traced-apply-torque body torque dt)
                                   force dt)]
                        [result (vector-copy bodies)])
                       (vector-set! result idx new-body)
                       result)))))

;;; traced-apply-revolute-joint : (Vector TracedBody) × RevoluteJoint × Number → (Vector TracedBody)
;;; Apply a revolute joint to the body system.
(define (traced-apply-revolute-joint bodies joint dt)
  (doc 'export #t)
  (let* ([idx-a (revolute-joint-body-a joint)]
         [anchor-a (revolute-joint-anchor-a joint)]
         [idx-b (revolute-joint-body-b joint)]
         [anchor-b (revolute-joint-anchor-b joint)]
         [stiffness (revolute-joint-stiffness joint)]
         [damping (revolute-joint-damping joint)]
         [body-a (vector-ref bodies idx-a)]
         [body-b (vector-ref bodies idx-b)])
        (call-with-values
         (lambda () (traced-revolute-forces body-a anchor-a body-b anchor-b stiffness damping))
         (lambda (result-a result-b)
                 (let* ([force-a (car result-a)]
                        [torque-a (cadr result-a)]
                        [force-b (car result-b)]
                        [torque-b (cadr result-b)]
                        [new-a (traced-apply-central-force
                                (traced-apply-torque body-a torque-a dt)
                                force-a dt)]
                        [new-b (traced-apply-central-force
                                (traced-apply-torque body-b torque-b dt)
                                force-b dt)]
                        [result (vector-copy bodies)])
                       (vector-set! result idx-a new-a)
                       (vector-set! result idx-b new-b)
                       result)))))

;;; traced-apply-constraint : (Vector TracedBody) × Constraint × Number → (Vector TracedBody)
;;; Apply any constraint type.
(define (traced-apply-constraint bodies constraint dt)
  (doc 'export #t)
  (case (constraint-type constraint)
        [(spring-constraint) (traced-apply-spring-constraint bodies constraint dt)]
        [(anchor-constraint) (traced-apply-anchor-constraint bodies constraint dt)]
        [(revolute-joint) (traced-apply-revolute-joint bodies constraint dt)]
        [else bodies]))

;;; traced-apply-all-constraints : (Vector TracedBody) × (List Constraint) × Number → (Vector TracedBody)
;;; Apply all constraints to the system.
(define (traced-apply-all-constraints bodies constraints dt)
  (doc 'export #t)
  (fold-left
   (lambda (current-bodies constraint)
           (traced-apply-constraint current-bodies constraint dt))
   bodies
   constraints))

;;; ====
;;; Complete Constraint Solver Step
;;; ====

;;; traced-constraint-step : (Vector TracedBody) × (List Constraint) × TracedVec2 × Number × Nat → (Vector TracedBody)
;;; One complete constraint solver step with optional iteration.
;;;   gravity: gravity acceleration
;;;   iterations: number of constraint iterations per step (default 1 for differentiability)
;;;
;;; For differentiable physics, use iterations=1 (or small k).
;;; Higher iterations improve accuracy but increase gradient complexity.
(define (traced-constraint-step bodies constraints gravity dt iterations)
  (doc 'export #t)
  (let loop ([current bodies] [i 0])
       (if (>= i iterations)
           current
           (let* (;; Apply gravity to all bodies
                  [with-gravity
                   (vector-map
                    (lambda (body)
                            (if (traced-body-static? body)
                                body
                                (traced-euler-step body gravity 0 dt)))
                    current)]
                  ;; Apply constraints
                  [with-constraints (traced-apply-all-constraints with-gravity constraints dt)])
                 (loop with-constraints (+ i 1))))))

;;; Helper: vector-map for mutable vectors
(define (vector-map f v)
  (doc 'export #t)
  (let* ([n (vector-length v)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (f (vector-ref v i))))))

;;; ====
;;; Pendulum Example Helpers
;;; ====

;;; make-pendulum-system : TracedVec2 × Number × Number × Number × Tape → (TracedBody × AnchorConstraint)
;;; Create a single pendulum bob anchored at a pivot.
(define (make-pendulum-system pivot length mass radius tape)
  (doc 'export #t)
  (let* (;; Bob starts directly below pivot
         [bob-pos (lift-vec2 (vec2 (vec2-x pivot) (- (vec2-y pivot) length)) tape)]
         [bob-vel (lift-vec2 (vec2-zero) tape)]
         [bob-angle (make-traced-var 0 tape)]
         [bob-omega (make-traced-var 0 tape)]
         [inertia (circle-inertia mass radius)]
         [bob (make-traced-body bob-pos bob-vel bob-angle bob-omega mass inertia)]
         ;; Anchor at pivot
         [constraint (make-anchor-constraint
                      0
                      (lift-vec2-const (vec2-zero))  ; anchor at center
                      (lift-vec2-const pivot)
                      *distance-constraint-stiffness*
                      *distance-constraint-damping*)])
        (values bob constraint)))

;;; make-chain-system : TracedVec2 × Number × Number × Number × Number × Tape → (Vector TracedBody × List Constraint)
;;; Create a chain of linked bodies.
(define (make-chain-system start-pos num-links link-length mass radius tape)
  (doc 'export #t)
  (let* ([inertia (circle-inertia mass radius)]
         ;; Create bodies
         [bodies
          (list->vector
           (let loop ([i 0] [y (vec2-y start-pos)])
                (if (= i num-links)
                    '()
                    (let ([pos (lift-vec2 (vec2 (vec2-x start-pos) y) tape)]
                          [vel (lift-vec2 (vec2-zero) tape)]
                          [angle (make-traced-var 0 tape)]
                          [omega (make-traced-var 0 tape)])
                         (cons (make-traced-body pos vel angle omega mass inertia)
                               (loop (+ i 1) (- y link-length)))))))]
         ;; Create constraints: anchor first body, link rest
         [anchor (make-anchor-constraint
                  0
                  (lift-vec2-const (vec2-zero))
                  (lift-vec2-const start-pos)
                  *distance-constraint-stiffness*
                  *distance-constraint-damping*)]
         [links
          (let loop ([i 0])
               (if (>= i (- num-links 1))
                   '()
                   (cons (make-distance-constraint
                          i (lift-vec2-const (vec2-zero))
                          (+ i 1) (lift-vec2-const (vec2-zero))
                          link-length)
                         (loop (+ i 1)))))])
        (values bodies (cons anchor links))))
