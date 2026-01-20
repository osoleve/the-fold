
(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "lattice/physics/classical/rigid-body.ss")


(doc 'module 'constraints)
(doc 'description "Constraint data structures for 2D physics joints")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'section 'base)

(doc "A constraint connects two bodies (or one body to world) with")
(doc "specific movement restrictions.")
;;;
(doc "Structure: (list 'constraint type id entity-a entity-b data")
(doc "                 solver-velocity solver-position cached-lambda)")
;;;
(doc "Fields:")
(doc "  - type: Symbol ('distance, 'revolute, 'spring, 'weld)")
(doc "  - id: Unique identifier")
(doc "  - entity-a: ID of first entity")
(doc "  - entity-b: ID of second entity (or #f for world anchor)")
(doc "  - data: Type-specific parameters")
(doc "  - solver-velocity: Function (world constraint dt) -> void")
(doc "  - solver-position: Function (world constraint) -> void")
(doc "  - cached-lambda: Cached impulse for warm starting")

(define (make-constraint type id entity-a entity-b data
                         solver-velocity solver-position cached-lambda)
  (doc 'type '(make-constraint : Symbol x Any x Any x Any x Any x Proc x Proc x Any -> Constraint))
  (list 'constraint type id entity-a entity-b data
        solver-velocity solver-position cached-lambda))
(doc "constraint? : Any -> Boolean")
(define (constraint? c)
  (and (pair? c) (eq? (car c) 'constraint)))

(doc "Accessors")

(define (constraint-type c) (list-ref c 1))
  (doc 'type '(constraint-type : Constraint -> Symbol))

(define (constraint-id c) (list-ref c 2))
  (doc 'type '(constraint-id : Constraint -> Any))

(define (constraint-entity-a c) (list-ref c 3))
  (doc 'type '(constraint-entity-a : Constraint -> Any))

(define (constraint-entity-b c) (list-ref c 4))
  (doc 'type '(constraint-entity-b : Constraint -> Any or #f))

(define (constraint-data c) (list-ref c 5))
  (doc 'type '(constraint-data : Constraint -> Any))

(define (constraint-solver-velocity c) (list-ref c 6))
  (doc 'type '(constraint-solver-velocity : Constraint -> MaybeProcedure))

(define (constraint-solver-position c) (list-ref c 7))
  (doc 'type '(constraint-solver-position : Constraint -> MaybeProcedure))

(define (constraint-cached-lambda c) (list-ref c 8))
  (doc 'type '(constraint-cached-lambda : Constraint -> Any))

(define (constraint-with-cached-lambda c new-lambda)
  (doc 'type '(constraint-with-cached-lambda : Constraint x Any -> Constraint))
  (make-constraint (constraint-type c)
                   (constraint-id c)
                   (constraint-entity-a c)
                   (constraint-entity-b c)
                   (constraint-data c)
                   (constraint-solver-velocity c)
                   (constraint-solver-position c)
                   new-lambda))

(doc 'section 'distance)

(doc "Distance constraint data:")
(doc "  (list local-anchor-a local-anchor-b target-distance)")

(define (make-distance-constraint-data local-anchor-a local-anchor-b target-distance)
  (doc 'type '(make-distance-constraint-data : Vec2 x Vec2 x Number -> DistanceData))
  (list local-anchor-a local-anchor-b target-distance))

(define (distance-data-anchor-a d) (list-ref d 0))
  (doc 'type '(distance-data-anchor-a : DistanceData -> Vec2))

(define (distance-data-anchor-b d) (list-ref d 1))
  (doc 'type '(distance-data-anchor-b : DistanceData -> Vec2))

(define (distance-data-target d) (list-ref d 2))
  (doc 'type '(distance-data-target : DistanceData -> Number))

(doc 'section 'revolute)

(doc "Revolute constraint data:")
(doc "  (list local-anchor-a local-anchor-b)")

(define (make-revolute-constraint-data local-anchor-a local-anchor-b)
  (doc 'type '(make-revolute-constraint-data : Vec2 x Vec2 -> RevoluteData))
  (list local-anchor-a local-anchor-b))

(define (revolute-data-anchor-a d) (list-ref d 0))
  (doc 'type '(revolute-data-anchor-a : RevoluteData -> Vec2))

(define (revolute-data-anchor-b d) (list-ref d 1))
  (doc 'type '(revolute-data-anchor-b : RevoluteData -> Vec2))

(doc 'section 'spring)

(doc "Spring constraint data:")
(doc "  (list local-anchor-a local-anchor-b rest-length stiffness damping)")

(define (make-spring-constraint-data local-anchor-a local-anchor-b
                                     rest-length stiffness damping)
  (doc 'type '(make-spring-constraint-data : Vec2 x Vec2 x Number x Number x Number -> SpringData))
  (list local-anchor-a local-anchor-b rest-length stiffness damping))
(define (spring-data-anchor-a d) (list-ref d 0))
  (doc 'type '(spring-data-anchor-a : SpringData -> Vec2))

(define (spring-data-anchor-b d) (list-ref d 1))
  (doc 'type '(spring-data-anchor-b : SpringData -> Vec2))

(define (spring-data-rest-length d) (list-ref d 2))
  (doc 'type '(spring-data-rest-length : SpringData -> Number))

(define (spring-data-stiffness d) (list-ref d 3))
  (doc 'type '(spring-data-stiffness : SpringData -> Number))

(define (spring-data-damping d) (list-ref d 4))
  (doc 'type '(spring-data-damping : SpringData -> Number))

(doc 'section 'weld)

(doc "Weld constraint data:")
(doc "  (list local-anchor-a local-anchor-b reference-angle)")

(define (make-weld-constraint-data local-anchor-a local-anchor-b reference-angle)
  (doc 'type '(make-weld-constraint-data : Vec2 x Vec2 x Number -> WeldData))
  (list local-anchor-a local-anchor-b reference-angle))

(define (weld-data-anchor-a d) (list-ref d 0))
  (doc 'type '(weld-data-anchor-a : WeldData -> Vec2))

(define (weld-data-anchor-b d) (list-ref d 1))
  (doc 'type '(weld-data-anchor-b : WeldData -> Vec2))

(define (weld-data-reference-angle d) (list-ref d 2))
  (doc 'type '(weld-data-reference-angle : WeldData -> Number))

(doc 'section 'helper:)

(doc "These functions compute world-space anchor positions from local anchors.")
(doc "Used by constraint solvers.")

(doc 'constraint-anchor-world-a 'type 'World x Constraint -> Vec2)
(doc "Get world position of anchor A.")
(define (constraint-anchor-world-a world constraint)
  (let* ([entity-id (constraint-entity-a constraint)]
         [data (constraint-data constraint)]
         [local-anchor (car data)])  ; First element is always local-anchor-a
        (constraint-entity-anchor-world world entity-id local-anchor)))

(doc 'constraint-anchor-world-b 'type 'World x Constraint -> Vec2)
(doc "Get world position of anchor B.")
(doc "If entity-b is #f, local-anchor-b IS the world position.")
(define (constraint-anchor-world-b world constraint)
  (let* ([entity-id (constraint-entity-b constraint)]
         [data (constraint-data constraint)]
         [local-anchor (cadr data)])  ; Second element is always local-anchor-b
        (if entity-id
            (constraint-entity-anchor-world world entity-id local-anchor)
            local-anchor)))  ; World anchor

(doc 'constraint-entity-anchor-world 'type 'World x Any x Vec2 -> Vec2)
(doc "Transform local anchor to world space for an entity.")
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

(doc 'section 'constraint)

(doc 'constraint-get-bodies 'type '(-> World Constraint (Pair (Maybe Body) (Maybe Body))))
(doc "Get the two bodies connected by a constraint.")
(doc "Returns (body-a . body-b) where body-b may be #f for world anchor.")
(define (constraint-get-bodies world constraint)
  (let* ([id-a (constraint-entity-a constraint)]
         [id-b (constraint-entity-b constraint)]
         [ent-a (world-get-entity world id-a)]
         [ent-b (and id-b (world-get-entity world id-b))]
         [body-a (and ent-a (entity-body ent-a))]
         [body-b (and ent-b (entity-body ent-b))])
        (cons body-a body-b)))
