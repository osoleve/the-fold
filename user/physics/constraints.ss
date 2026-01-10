;;; user/physics/constraints.ss — Constraint Data Structures
;;;
;;; Defines the base constraint types for 2D physics joints:
;;;   - Distance constraint: Fixed distance between anchor points
;;;   - Revolute joint: Bodies rotate around shared pivot
;;;   - Spring constraint: Soft distance with stiffness/damping
;;;   - Weld joint: Fixed relative position and angle
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/linalg/vec2.ss
;;;   - user/physics/rigid-body.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec2.ss")
(load "user/physics/rigid-body.ss")

;;; ============================================================
;;; Base Constraint Structure
;;; ============================================================

;;; A constraint connects two bodies (or one body to world) with
;;; specific movement restrictions.
;;;
;;; Structure: (list 'constraint type id entity-a entity-b data
;;;                  solver-velocity solver-position cached-lambda)
;;;
;;; Fields:
;;;   - type: Symbol ('distance, 'revolute, 'spring, 'weld)
;;;   - id: Unique identifier
;;;   - entity-a: ID of first entity
;;;   - entity-b: ID of second entity (or #f for world anchor)
;;;   - data: Type-specific parameters
;;;   - solver-velocity: Function (world constraint dt) -> void
;;;   - solver-position: Function (world constraint) -> void
;;;   - cached-lambda: Cached impulse for warm starting

;;; make-constraint : Symbol x Any x Any x Any x Any x Proc x Proc x Any -> Constraint
(define (make-constraint type id entity-a entity-b data
                         solver-velocity solver-position cached-lambda)
  (list 'constraint type id entity-a entity-b data
        solver-velocity solver-position cached-lambda))

;;; constraint? : Any -> Boolean
(define (constraint? c)
  (and (pair? c) (eq? (car c) 'constraint)))

;;; Accessors

;;; constraint-type : Constraint -> Symbol
(define (constraint-type c) (list-ref c 1))

;;; constraint-id : Constraint -> Any
(define (constraint-id c) (list-ref c 2))

;;; constraint-entity-a : Constraint -> Any
(define (constraint-entity-a c) (list-ref c 3))

;;; constraint-entity-b : Constraint -> Any | #f
(define (constraint-entity-b c) (list-ref c 4))

;;; constraint-data : Constraint -> Any
(define (constraint-data c) (list-ref c 5))

;;; constraint-solver-velocity : Constraint -> Procedure | #f
(define (constraint-solver-velocity c) (list-ref c 6))

;;; constraint-solver-position : Constraint -> Procedure | #f
(define (constraint-solver-position c) (list-ref c 7))

;;; constraint-cached-lambda : Constraint -> Any
(define (constraint-cached-lambda c) (list-ref c 8))

;;; constraint-with-cached-lambda : Constraint x Any -> Constraint
(define (constraint-with-cached-lambda c new-lambda)
  (make-constraint (constraint-type c)
                   (constraint-id c)
                   (constraint-entity-a c)
                   (constraint-entity-b c)
                   (constraint-data c)
                   (constraint-solver-velocity c)
                   (constraint-solver-position c)
                   new-lambda))

;;; ============================================================
;;; Distance Constraint
;;; ============================================================

;;; Distance constraint data:
;;;   (list local-anchor-a local-anchor-b target-distance)

;;; make-distance-constraint-data : Vec2 x Vec2 x Number -> DistanceData
(define (make-distance-constraint-data local-anchor-a local-anchor-b target-distance)
  (list local-anchor-a local-anchor-b target-distance))

;;; distance-data-anchor-a : DistanceData -> Vec2
(define (distance-data-anchor-a d) (list-ref d 0))

;;; distance-data-anchor-b : DistanceData -> Vec2
(define (distance-data-anchor-b d) (list-ref d 1))

;;; distance-data-target : DistanceData -> Number
(define (distance-data-target d) (list-ref d 2))

;;; ============================================================
;;; Revolute Joint
;;; ============================================================

;;; Revolute constraint data:
;;;   (list local-anchor-a local-anchor-b)

;;; make-revolute-constraint-data : Vec2 x Vec2 -> RevoluteData
(define (make-revolute-constraint-data local-anchor-a local-anchor-b)
  (list local-anchor-a local-anchor-b))

;;; revolute-data-anchor-a : RevoluteData -> Vec2
(define (revolute-data-anchor-a d) (list-ref d 0))

;;; revolute-data-anchor-b : RevoluteData -> Vec2
(define (revolute-data-anchor-b d) (list-ref d 1))

;;; ============================================================
;;; Spring Constraint
;;; ============================================================

;;; Spring constraint data:
;;;   (list local-anchor-a local-anchor-b rest-length stiffness damping)

;;; make-spring-constraint-data : Vec2 x Vec2 x Number x Number x Number -> SpringData
(define (make-spring-constraint-data local-anchor-a local-anchor-b
                                     rest-length stiffness damping)
  (list local-anchor-a local-anchor-b rest-length stiffness damping))

;;; spring-data-anchor-a : SpringData -> Vec2
(define (spring-data-anchor-a d) (list-ref d 0))

;;; spring-data-anchor-b : SpringData -> Vec2
(define (spring-data-anchor-b d) (list-ref d 1))

;;; spring-data-rest-length : SpringData -> Number
(define (spring-data-rest-length d) (list-ref d 2))

;;; spring-data-stiffness : SpringData -> Number
(define (spring-data-stiffness d) (list-ref d 3))

;;; spring-data-damping : SpringData -> Number
(define (spring-data-damping d) (list-ref d 4))

;;; ============================================================
;;; Weld Joint
;;; ============================================================

;;; Weld constraint data:
;;;   (list local-anchor-a local-anchor-b reference-angle)

;;; make-weld-constraint-data : Vec2 x Vec2 x Number -> WeldData
(define (make-weld-constraint-data local-anchor-a local-anchor-b reference-angle)
  (list local-anchor-a local-anchor-b reference-angle))

;;; weld-data-anchor-a : WeldData -> Vec2
(define (weld-data-anchor-a d) (list-ref d 0))

;;; weld-data-anchor-b : WeldData -> Vec2
(define (weld-data-anchor-b d) (list-ref d 1))

;;; weld-data-reference-angle : WeldData -> Number
(define (weld-data-reference-angle d) (list-ref d 2))

;;; ============================================================
;;; Helper: Get Anchor World Positions
;;; ============================================================

;;; These functions compute world-space anchor positions from local anchors.
;;; Used by constraint solvers.

;;; constraint-anchor-world-a : World x Constraint -> Vec2
;;; Get world position of anchor A.
(define (constraint-anchor-world-a world constraint)
  (let* ([entity-id (constraint-entity-a constraint)]
         [data (constraint-data constraint)]
         [local-anchor (car data)])  ; First element is always local-anchor-a
        (constraint-entity-anchor-world world entity-id local-anchor)))

;;; constraint-anchor-world-b : World x Constraint -> Vec2
;;; Get world position of anchor B.
;;; If entity-b is #f, local-anchor-b IS the world position.
(define (constraint-anchor-world-b world constraint)
  (let* ([entity-id (constraint-entity-b constraint)]
         [data (constraint-data constraint)]
         [local-anchor (cadr data)])  ; Second element is always local-anchor-b
        (if entity-id
            (constraint-entity-anchor-world world entity-id local-anchor)
            local-anchor)))  ; World anchor

;;; constraint-entity-anchor-world : World x Any x Vec2 -> Vec2
;;; Transform local anchor to world space for an entity.
(define (constraint-entity-anchor-world world entity-id local-anchor)
  (let ([entity (world-get-entity world entity-id)])
       (if entity
           (let ([body (entity-body entity)])
                ;; For Body2D (no rotation), just add position
                ;; For RigidBody2D, would use local-to-world
                (if (rigid-body? body)
                    (local-to-world body local-anchor)
                    (vec2-add (body-pos body) local-anchor)))
           (vec2 0 0))))  ; Entity not found

;;; ============================================================
;;; Constraint Bodies Helper
;;; ============================================================

;;; constraint-get-bodies : World x Constraint -> (Body | #f) x (Body | #f)
;;; Get the two bodies connected by a constraint.
;;; Returns (body-a . body-b) where body-b may be #f for world anchor.
(define (constraint-get-bodies world constraint)
  (let* ([id-a (constraint-entity-a constraint)]
         [id-b (constraint-entity-b constraint)]
         [ent-a (world-get-entity world id-a)]
         [ent-b (and id-b (world-get-entity world id-b))]
         [body-a (and ent-a (entity-body ent-a))]
         [body-b (and ent-b (entity-body ent-b))])
        (cons body-a body-b)))
