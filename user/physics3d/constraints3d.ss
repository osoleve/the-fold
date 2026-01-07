;;; user/physics3d/constraints3d.ss — 3D Constraint Data Structures
;;;
;;; Defines the base constraint types for 3D physics joints:
;;;   - Distance constraint: Fixed distance between anchor points
;;;   - Ball-socket joint: Shared pivot point (3 DOF removed)
;;;   - Hinge joint: Shared pivot + aligned axis (5 DOF removed)
;;;   - Fixed joint: No relative motion (6 DOF removed)
;;;   - Spring constraint: Soft distance with stiffness/damping
;;;
;;; This is user code: interfaces with core but may have pragmatic shortcuts.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/linalg/vec3.ss
;;;   - core/linalg/quaternion.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec3.ss")
(load "core/linalg/quaternion.ss")

;;; ============================================================
;;; Base Constraint Structure
;;; ============================================================

;;; A constraint connects two bodies (or one body to world) with
;;; specific movement restrictions.
;;;
;;; Structure: (list 'constraint-3d type id entity-a entity-b data
;;;                  solver-velocity solver-position cached-lambda)
;;;
;;; Fields:
;;;   - type: Symbol ('distance-3d, 'ball-socket-3d, 'hinge-3d, 'fixed-3d, 'spring-3d)
;;;   - id: Unique identifier
;;;   - entity-a: ID of first entity
;;;   - entity-b: ID of second entity (or #f for world anchor)
;;;   - data: Type-specific parameters
;;;   - solver-velocity: Function (world constraint dt) -> void
;;;   - solver-position: Function (world constraint) -> void
;;;   - cached-lambda: Cached impulse for warm starting

;;; make-constraint-3d : Symbol × Any × Any × Any × Any × Proc × Proc × Any → Constraint3D
(define (make-constraint-3d type id entity-a entity-b data
                            solver-velocity solver-position cached-lambda)
  (list 'constraint-3d type id entity-a entity-b data
        solver-velocity solver-position cached-lambda))

;;; constraint-3d? : Any → Boolean
(define (constraint-3d? c)
  (and (pair? c) (eq? (car c) 'constraint-3d)))

;;; Accessors

;;; constraint-3d-type : Constraint3D → Symbol
(define (constraint-3d-type c) (list-ref c 1))

;;; constraint-3d-id : Constraint3D → Any
(define (constraint-3d-id c) (list-ref c 2))

;;; constraint-3d-entity-a : Constraint3D → Any
(define (constraint-3d-entity-a c) (list-ref c 3))

;;; constraint-3d-entity-b : Constraint3D → Any | #f
(define (constraint-3d-entity-b c) (list-ref c 4))

;;; constraint-3d-data : Constraint3D → Any
(define (constraint-3d-data c) (list-ref c 5))

;;; constraint-3d-solver-velocity : Constraint3D → Procedure | #f
(define (constraint-3d-solver-velocity c) (list-ref c 6))

;;; constraint-3d-solver-position : Constraint3D → Procedure | #f
(define (constraint-3d-solver-position c) (list-ref c 7))

;;; constraint-3d-cached-lambda : Constraint3D → Any
(define (constraint-3d-cached-lambda c) (list-ref c 8))

;;; constraint-3d-with-cached-lambda : Constraint3D × Any → Constraint3D
(define (constraint-3d-with-cached-lambda c new-lambda)
  (make-constraint-3d (constraint-3d-type c)
                      (constraint-3d-id c)
                      (constraint-3d-entity-a c)
                      (constraint-3d-entity-b c)
                      (constraint-3d-data c)
                      (constraint-3d-solver-velocity c)
                      (constraint-3d-solver-position c)
                      new-lambda))

;;; ============================================================
;;; Distance Constraint (1 DOF)
;;; ============================================================

;;; Distance constraint data:
;;;   (list local-anchor-a local-anchor-b target-distance)

;;; make-distance-constraint-data-3d : Vec3 × Vec3 × Number → DistanceData3D
(define (make-distance-constraint-data-3d local-anchor-a local-anchor-b target-distance)
  (list local-anchor-a local-anchor-b target-distance))

;;; distance-data-3d-anchor-a : DistanceData3D → Vec3
(define (distance-data-3d-anchor-a d) (list-ref d 0))

;;; distance-data-3d-anchor-b : DistanceData3D → Vec3
(define (distance-data-3d-anchor-b d) (list-ref d 1))

;;; distance-data-3d-target : DistanceData3D → Number
(define (distance-data-3d-target d) (list-ref d 2))

;;; ============================================================
;;; Ball-Socket Joint (3 DOF removed - point-to-point)
;;; ============================================================

;;; Ball-socket constraint data:
;;;   (list local-anchor-a local-anchor-b)

;;; make-ball-socket-constraint-data-3d : Vec3 × Vec3 → BallSocketData3D
(define (make-ball-socket-constraint-data-3d local-anchor-a local-anchor-b)
  (list local-anchor-a local-anchor-b))

;;; ball-socket-data-3d-anchor-a : BallSocketData3D → Vec3
(define (ball-socket-data-3d-anchor-a d) (list-ref d 0))

;;; ball-socket-data-3d-anchor-b : BallSocketData3D → Vec3
(define (ball-socket-data-3d-anchor-b d) (list-ref d 1))

;;; ============================================================
;;; Hinge Joint (5 DOF removed - point + axis alignment)
;;; ============================================================

;;; Hinge constraint data:
;;;   (list local-anchor-a local-anchor-b local-axis-a local-axis-b)

;;; make-hinge-constraint-data-3d : Vec3 × Vec3 × Vec3 × Vec3 → HingeData3D
(define (make-hinge-constraint-data-3d local-anchor-a local-anchor-b local-axis-a local-axis-b)
  (list local-anchor-a local-anchor-b local-axis-a local-axis-b))

;;; hinge-data-3d-anchor-a : HingeData3D → Vec3
(define (hinge-data-3d-anchor-a d) (list-ref d 0))

;;; hinge-data-3d-anchor-b : HingeData3D → Vec3
(define (hinge-data-3d-anchor-b d) (list-ref d 1))

;;; hinge-data-3d-axis-a : HingeData3D → Vec3
(define (hinge-data-3d-axis-a d) (list-ref d 2))

;;; hinge-data-3d-axis-b : HingeData3D → Vec3
(define (hinge-data-3d-axis-b d) (list-ref d 3))

;;; ============================================================
;;; Fixed Joint (6 DOF removed - rigid connection)
;;; ============================================================

;;; Fixed constraint data:
;;;   (list local-anchor-a local-anchor-b reference-orientation)

;;; make-fixed-constraint-data-3d : Vec3 × Vec3 × Quat → FixedData3D
(define (make-fixed-constraint-data-3d local-anchor-a local-anchor-b reference-orientation)
  (list local-anchor-a local-anchor-b reference-orientation))

;;; fixed-data-3d-anchor-a : FixedData3D → Vec3
(define (fixed-data-3d-anchor-a d) (list-ref d 0))

;;; fixed-data-3d-anchor-b : FixedData3D → Vec3
(define (fixed-data-3d-anchor-b d) (list-ref d 1))

;;; fixed-data-3d-reference-orientation : FixedData3D → Quat
(define (fixed-data-3d-reference-orientation d) (list-ref d 2))

;;; ============================================================
;;; Spring Constraint (Soft 1 DOF)
;;; ============================================================

;;; Spring constraint data:
;;;   (list local-anchor-a local-anchor-b rest-length stiffness damping)

;;; make-spring-constraint-data-3d : Vec3 × Vec3 × Number × Number × Number → SpringData3D
(define (make-spring-constraint-data-3d local-anchor-a local-anchor-b
                                        rest-length stiffness damping)
  (list local-anchor-a local-anchor-b rest-length stiffness damping))

;;; spring-data-3d-anchor-a : SpringData3D → Vec3
(define (spring-data-3d-anchor-a d) (list-ref d 0))

;;; spring-data-3d-anchor-b : SpringData3D → Vec3
(define (spring-data-3d-anchor-b d) (list-ref d 1))

;;; spring-data-3d-rest-length : SpringData3D → Number
(define (spring-data-3d-rest-length d) (list-ref d 2))

;;; spring-data-3d-stiffness : SpringData3D → Number
(define (spring-data-3d-stiffness d) (list-ref d 3))

;;; spring-data-3d-damping : SpringData3D → Number
(define (spring-data-3d-damping d) (list-ref d 4))

;;; ============================================================
;;; Anchor Constraint (Pin to World - 3 DOF removed)
;;; ============================================================

;;; Anchor constraint data:
;;;   (list local-anchor world-target)

;;; make-anchor-constraint-data-3d : Vec3 × Vec3 → AnchorData3D
(define (make-anchor-constraint-data-3d local-anchor world-target)
  (list local-anchor world-target))

;;; anchor-data-3d-local-anchor : AnchorData3D → Vec3
(define (anchor-data-3d-local-anchor d) (list-ref d 0))

;;; anchor-data-3d-world-target : AnchorData3D → Vec3
(define (anchor-data-3d-world-target d) (list-ref d 1))

;;; ============================================================
;;; Helper: Local to World Transform
;;; ============================================================

;;; local-to-world-3d : RigidBody3D × Vec3 → Vec3
;;; Transform a local point to world coordinates using body position and orientation.
;;; world = pos + rotate(local, orientation)
(define (local-to-world-3d body local-point)
  ;; Requires the body to have a position accessor and orientation accessor
  ;; For now, assume body has (rigid-body-3d-pos body) and (rigid-body-3d-orientation body)
  ;; This will be resolved when integrated with rigid-body3d.ss
  (let* ([pos (rigid-body-3d-pos body)]
         [orient (rigid-body-3d-orientation body)]
         [rotated (quat-rotate-vec3 orient local-point)])
        (vec3-add pos rotated)))

;;; world-to-local-3d : RigidBody3D × Vec3 → Vec3
;;; Transform a world point to local coordinates.
;;; local = rotate(world - pos, conjugate(orientation))
(define (world-to-local-3d body world-point)
  (let* ([pos (rigid-body-3d-pos body)]
         [orient (rigid-body-3d-orientation body)]
         [q-inv (quat-conjugate orient)]
         [rel (vec3-sub world-point pos)])
        (quat-rotate-vec3 q-inv rel)))

;;; local-dir-to-world-3d : RigidBody3D × Vec3 → Vec3
;;; Transform a local direction to world coordinates (no translation).
(define (local-dir-to-world-3d body local-dir)
  (let ([orient (rigid-body-3d-orientation body)])
       (quat-rotate-vec3 orient local-dir)))

;;; ============================================================
;;; Helper: Get Anchor World Positions (Placeholder)
;;; ============================================================

;;; These functions compute world-space anchor positions from local anchors.
;;; They require integration with the world/entity system.
;;; For now, provide signatures that will be implemented by constraint-solver3d.ss.

;;; constraint-anchor-world-a-3d : World3D × Constraint3D → Vec3
;;; Get world position of anchor A.
;;; (Placeholder - will be implemented by solver)

;;; constraint-anchor-world-b-3d : World3D × Constraint3D → Vec3
;;; Get world position of anchor B.
;;; (Placeholder - will be implemented by solver)

;;; ============================================================
;;; Constraint Factory Stubs
;;; ============================================================

;;; These create constraints without solvers attached.
;;; The solver functions will be attached by constraint-solver3d.ss.

;;; make-distance-constraint-stub-3d : Any × Any × Any × Vec3 × Vec3 × Number → Constraint3D
(define (make-distance-constraint-stub-3d id entity-a entity-b local-anchor-a local-anchor-b distance)
  (make-constraint-3d 'distance-3d id entity-a entity-b
                      (make-distance-constraint-data-3d local-anchor-a local-anchor-b distance)
                      #f #f  ; solvers attached by constraint-solver3d.ss
                      0))    ; cached lambda

;;; make-ball-socket-constraint-stub-3d : Any × Any × Any × Vec3 × Vec3 → Constraint3D
(define (make-ball-socket-constraint-stub-3d id entity-a entity-b local-anchor-a local-anchor-b)
  (make-constraint-3d 'ball-socket-3d id entity-a entity-b
                      (make-ball-socket-constraint-data-3d local-anchor-a local-anchor-b)
                      #f #f
                      (vec3-zero)))  ; cached lambda (3D)

;;; make-hinge-constraint-stub-3d : Any × Any × Any × Vec3 × Vec3 × Vec3 × Vec3 → Constraint3D
(define (make-hinge-constraint-stub-3d id entity-a entity-b
                                       local-anchor-a local-anchor-b
                                       local-axis-a local-axis-b)
  (make-constraint-3d 'hinge-3d id entity-a entity-b
                      (make-hinge-constraint-data-3d local-anchor-a local-anchor-b
                                                     local-axis-a local-axis-b)
                      #f #f
                      (list (vec3-zero) (vec3-zero))))  ; cached lambda (point + angular)

;;; make-fixed-constraint-stub-3d : Any × Any × Any × Vec3 × Vec3 × Quat → Constraint3D
(define (make-fixed-constraint-stub-3d id entity-a entity-b
                                       local-anchor-a local-anchor-b
                                       reference-orientation)
  (make-constraint-3d 'fixed-3d id entity-a entity-b
                      (make-fixed-constraint-data-3d local-anchor-a local-anchor-b
                                                     reference-orientation)
                      #f #f
                      (list (vec3-zero) (vec3-zero))))  ; cached lambda (point + angular)

;;; make-spring-constraint-stub-3d : Any × Any × Any × Vec3 × Vec3 × Number × Number × Number → Constraint3D
(define (make-spring-constraint-stub-3d id entity-a entity-b
                                        local-anchor-a local-anchor-b
                                        rest-length stiffness damping)
  (make-constraint-3d 'spring-3d id entity-a entity-b
                      (make-spring-constraint-data-3d local-anchor-a local-anchor-b
                                                      rest-length stiffness damping)
                      #f #f
                      0))  ; cached lambda (scalar for spring)

;;; make-anchor-constraint-stub-3d : Any × Any × Vec3 × Vec3 → Constraint3D
;;; Pin entity to a world point (entity-b is #f).
(define (make-anchor-constraint-stub-3d id entity-a local-anchor world-target)
  (make-constraint-3d 'anchor-3d id entity-a #f
                      (make-anchor-constraint-data-3d local-anchor world-target)
                      #f #f
                      (vec3-zero)))  ; cached lambda (3D)

