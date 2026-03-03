(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module world
;;; @requires prelude integrators collision-detection collision-response raycasting constraints sort hamt
(require 'prelude)
(require 'integrators)
(require 'collision-detection)
(require 'collision-response)
(require 'raycasting)
(require 'constraints)
(require 'sort)
(require 'hamt)

(doc 'module 'world)
(doc 'description "2D physics world with collision detection and resolution")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'section 'physics)

(doc "A physics entity combines:")
(doc "  - id: unique identifier")
(doc "  - body: physics body (position, velocity, mass)")
(doc "  - shape: collision shape")
(doc "  - material: physics material (restitution, friction)")
(doc "  - user-data: optional user data")

(define (make-entity id body shape material user-data)
  (doc 'type '(make-entity : Any × Body2D × Shape × Material × Any → Entity))
  (list 'entity id body shape material user-data))

(doc "entity? : Any → Boolean")
(define (entity? e)
  (and (pair? e) (eq? (car e) 'entity)))

(define (entity-id e) (list-ref e 1))
  (doc 'type '(entity-id : Entity → Any))

(define (entity-body e) (list-ref e 2))
  (doc 'type '(entity-body : Entity → Body2D))

(define (entity-shape e) (list-ref e 3))
  (doc 'type '(entity-shape : Entity → Shape))

(define (entity-material e) (list-ref e 4))
  (doc 'type '(entity-material : Entity → Material))

(define (entity-user-data e) (list-ref e 5))
  (doc 'type '(entity-user-data : Entity → Any))

(define (entity-with-body e new-body)
  (doc 'type '(entity-with-body : Entity × Body2D → Entity))
  (make-entity (entity-id e) new-body (entity-shape e)
               (entity-material e) (entity-user-data e)))

(define (entity-pos e) (body-pos (entity-body e)))
  (doc 'type '(entity-pos : Entity → Vec2))

(define (entity-vel e) (body-vel (entity-body e)))
  (doc 'type '(entity-vel : Entity → Vec2))

(doc "entity-static? : Entity → Boolean")
(define (entity-static? e) (body-static? (entity-body e)))

(doc 'section 'shape)

(doc 'transform-shape 'type 'Shape × Vec2 → Shape)
(doc "Translate a shape by the given offset.")
(define (transform-shape shape offset)
  (cond
   [(aabb? shape)
    (make-aabb (vec2-add (aabb-min shape) offset)
               (vec2-add (aabb-max shape) offset))]
   [(circle? shape)
    (make-circle (vec2-add (circle-center shape) offset)
                 (circle-radius shape))]
   [(polygon? shape)
    (make-polygon
     (map (lambda (v) (vec2-add v offset))
          (polygon-vertices shape)))]
   [else shape]))

(doc 'entity-world-shape 'type 'Entity → Shape)
(doc "Get entity's shape in world coordinates.")
(doc "Shapes are stored in local coordinates (centered at origin).")
(doc "This function transforms them by adding the entity's position.")
(define (entity-world-shape e)
  (let ([shape (entity-shape e)]
        [pos (entity-pos e)])
       (transform-shape shape pos)))

(doc 'section 'physics)

(doc "A physics world contains:")
(doc "  - entities: HAMT of id → entity")
(doc "  - gravity: gravity force vector")
(doc "  - spatial-hash: broad phase structure")
(doc "  - config: world configuration")

(define (make-world-config)
  (doc 'type '(make-world-config : → WorldConfig))
  (list 'world-config
        0.016667            ; fixed-dt (60 fps)
        10                  ; max-substeps
        8                   ; solver-iterations
        32))                ; spatial-hash-cell-size

(define (world-config? x)
  (and (pair? x) (eq? (car x) 'world-config)))

(define (config-fixed-dt c) (list-ref c 1))
  (doc 'type '(config-fixed-dt : WorldConfig → Number))

(define (config-max-substeps c) (list-ref c 2))
  (doc 'type '(config-max-substeps : WorldConfig → Nat))

(define (config-solver-iterations c) (list-ref c 3))
  (doc 'type '(config-solver-iterations : WorldConfig → Nat))

(define (config-cell-size c) (list-ref c 4))
  (doc 'type '(config-cell-size : WorldConfig → Number))

(doc 'make-world 'type 'Vec2 → World)
(doc "Create a new physics world with given gravity.")
(define (make-world gravity)
  (let ([config (make-world-config)])
       (list 'world
             hamt-empty              ; entities (HAMT)
             gravity
             (make-spatial-hash (config-cell-size config))
             config
             (make-time-acc (config-fixed-dt config)
                            (config-max-substeps config))
             hamt-empty)))           ; constraints (HAMT)

(doc "world? : Any → Boolean")
(define (world? w)
  (and (pair? w) (eq? (car w) 'world)))

(define (world-entities w) (list-ref w 1))
  (doc 'type '(world-entities : World → HAMT))

(define (world-gravity w) (list-ref w 2))
  (doc 'type '(world-gravity : World → Vec2))

(define (world-spatial-hash w) (list-ref w 3))
  (doc 'type '(world-spatial-hash : World → SpatialHash))

(define (world-config w) (list-ref w 4))
  (doc 'type '(world-config : World → WorldConfig))

(define (world-time-acc w) (list-ref w 5))
  (doc 'type '(world-time-acc : World → TimeAcc))

(define (world-constraints w) (list-ref w 6))
  (doc 'type '(world-constraints : World → HAMT))

(define (world-with-time-acc w new-acc)
  (doc 'type '(world-with-time-acc : World × TimeAcc → World))
  (list 'world
        (world-entities w)
        (world-gravity w)
        (world-spatial-hash w)
        (world-config w)
        new-acc
        (world-constraints w)))

(doc 'section 'entity)

(doc "world-add-entity! : World × Entity → Void")
(doc "Add an entity to the world.")
(define (world-add-entity! world entity)
  (set-car! (list-tail world 1)
            (hamt-assoc (entity-id entity) entity (world-entities world))))

(doc "world-remove-entity! : World × Any → Void")
(doc "Remove an entity by id.")
(define (world-remove-entity! world id)
  (set-car! (list-tail world 1)
            (hamt-dissoc id (world-entities world))))

(doc 'world-get-entity 'type 'World × Any → MaybeEntity)
(doc "Get entity by id.")
(define (world-get-entity world id)
  (hamt-lookup id (world-entities world)))

(doc 'world-entity-list 'type 'World → (List Entity))
(doc "Get all entities as a list.")
(define (world-entity-list world)
  (hamt-values (world-entities world)))

(doc "world-update-entity! : World × Any × (Entity → Entity) → Void")
(doc "Update an entity by applying a function.")
(define (world-update-entity! world id f)
  (let ([entity (world-get-entity world id)])
       (when entity
             (set-car! (list-tail world 1)
                       (hamt-assoc id (f entity) (world-entities world))))))

(doc 'section 'constraint)

(doc "world-add-constraint! : World × Constraint → Void")
(doc "Add a constraint to the world.")
(define (world-add-constraint! world constraint)
  (let ([id (constraint-id constraint)])
       (set-car! (list-tail world 6)
                 (hamt-assoc id constraint (world-constraints world)))))

(doc "world-remove-constraint! : World × Any → Void")
(doc "Remove a constraint by id.")
(define (world-remove-constraint! world id)
  (set-car! (list-tail world 6)
            (hamt-dissoc id (world-constraints world))))

(doc 'world-get-constraint 'type 'World × Any → Constraint or #f)
(doc "Get a constraint by id.")
(define (world-get-constraint world id)
  (hamt-lookup id (world-constraints world)))

(doc 'world-constraint-list 'type 'World → (List Constraint))
(doc "Get all constraints as a list.")
(define (world-constraint-list world)
  (hamt-values (world-constraints world)))

(doc "world-update-constraint! : World × Any × (Constraint → Constraint) → Void")
(doc "Update a constraint by applying a function.")
(define (world-update-constraint! world id f)
  (let ([constraint (world-get-constraint world id)])
       (when constraint
             (set-car! (list-tail world 6)
                       (hamt-assoc id (f constraint) (world-constraints world))))))

(doc 'section 'force)

(doc "A force accumulator for an entity (mutable box holding HAMT)")
(define (make-force-acc)
  (list hamt-empty))

(doc "force-acc-add! : ForceAcc × Any × Vec2 → Void")
(doc "Add force to an entity's accumulator.")
(define (force-acc-add! acc id force)
  (let ([current (hamt-lookup-or id (car acc) (vec2 0 0))])
       (set-car! acc (hamt-assoc id (vec2-add current force) (car acc)))))

(doc 'force-acc-get 'type 'ForceAcc × Any → Vec2)
(doc "Get accumulated force for an entity.")
(define (force-acc-get acc id)
  (hamt-lookup-or id (car acc) (vec2 0 0)))

(doc "force-acc-clear! : ForceAcc → Void")
(define (force-acc-clear! acc)
  (set-car! acc hamt-empty))

(doc 'section 'world)

(doc "world-step! : World × Number → Void")
(doc "Step the physics simulation forward by dt seconds.")
(define (world-step! world dt)
  (let* ([config (world-config world)]
         [fixed-dt (config-fixed-dt config)]
         [max-steps (config-max-substeps config)]
         [time-acc (time-acc-add (world-time-acc world) dt)])
        ;; Consume fixed timesteps
        (let loop ([acc time-acc] [steps 0])
             (let* ([result (time-acc-consume acc)]
                    [new-acc (car result)]
                    [consumed (cadr result)])
                   (if (or (= consumed 0) (>= steps max-steps))
                       ;; Done stepping, update time accumulator
                       (world-with-time-acc world new-acc)
                       ;; Perform one physics step
                       (begin
                        (world-fixed-step! world fixed-dt)
                        (loop new-acc (+ steps 1))))))))

(doc "world-fixed-step! : World × Number → Void")
(doc "Perform one fixed-timestep physics update.")
(define (world-fixed-step! world dt)
  ;; 1. Apply forces and integrate velocities
  (world-integrate-velocities! world dt)
  ;; 2. Detect collisions
  (let ([collisions (world-detect-collisions world)]
        [constraints (world-constraint-list world)]
        [iterations (config-solver-iterations (world-config world))])
       ;; 3. Solve velocity constraints (interleaved)
       (let iter-loop ([i 0])
            (when (< i iterations)
                  ;; Solve collision velocity constraints
                  (world-resolve-collision-velocities! world collisions)
                  ;; Solve joint velocity constraints
                  (world-solve-constraint-velocities! world constraints dt)
                  (iter-loop (+ i 1))))
       ;; 4. Integrate positions
       (world-integrate-positions! world dt)
       ;; 5. Position correction
       (let iter-loop ([i 0])
            (when (< i (quotient iterations 2))
                  (world-correct-collision-positions! world collisions)
                  (world-correct-constraint-positions! world constraints)
                  (iter-loop (+ i 1))))))

(doc "world-resolve-collision-velocities! : World × (List Collision) → Void")
(doc "Resolve collision velocity constraints (impulses only, no position correction).")
(define (world-resolve-collision-velocities! world collisions)
  (for-each
   (lambda (collision)
           (let* ([ent-a (car collision)]
                  [ent-b (cadr collision)]
                  [manifold-data (caddr collision)]
                  [normal (car manifold-data)]
                  [penetration (cadr manifold-data)]
                  [contact (caddr manifold-data)])
                 (let* ([current-a (world-get-entity world (entity-id ent-a))]
                        [current-b (world-get-entity world (entity-id ent-b))])
                       (when (and current-a current-b)
                             (let* ([body-a (entity-body current-a)]
                                    [body-b (entity-body current-b)]
                                    [mat-a (entity-material current-a)]
                                    [mat-b (entity-material current-b)]
                                    [manifold (make-manifold body-a body-b normal penetration contact)]
                                    [result (resolve-collision manifold mat-a mat-b)]
                                    [new-body-a (car result)]
                                    [new-body-b (cadr result)])
                                   (world-update-entity! world (entity-id ent-a)
                                                         (lambda (e) (entity-with-body e new-body-a)))
                                   (world-update-entity! world (entity-id ent-b)
                                                         (lambda (e) (entity-with-body e new-body-b))))))))
   collisions))

(doc "world-correct-collision-positions! : World × (List Collision) → Void")
(doc "Apply position correction for collisions.")
(define (world-correct-collision-positions! world collisions)
  (for-each
   (lambda (collision)
           (let* ([ent-a (car collision)]
                  [ent-b (cadr collision)]
                  [manifold-data (caddr collision)]
                  [normal (car manifold-data)]
                  [penetration (cadr manifold-data)]
                  [contact (caddr manifold-data)])
                 (let* ([current-a (world-get-entity world (entity-id ent-a))]
                        [current-b (world-get-entity world (entity-id ent-b))])
                       (when (and current-a current-b)
                             (let* ([body-a (entity-body current-a)]
                                    [body-b (entity-body current-b)]
                                    [manifold (make-manifold body-a body-b normal penetration contact)]
                                    [result (correct-positions manifold)]
                                    [new-body-a (car result)]
                                    [new-body-b (cadr result)])
                                   (world-update-entity! world (entity-id ent-a)
                                                         (lambda (e) (entity-with-body e new-body-a)))
                                   (world-update-entity! world (entity-id ent-b)
                                                         (lambda (e) (entity-with-body e new-body-b))))))))
   collisions))

(doc "world-solve-constraint-velocities! : World × (List Constraint) × Number → Void")
(doc "Solve velocity constraints for all joints.")
(doc "Dispatches to constraint-specific solvers.")
(define (world-solve-constraint-velocities! world constraints dt)
  (for-each
   (lambda (c)
           (when (constraint-solver-velocity c)
                 ((constraint-solver-velocity c) world c dt)))
   constraints))

(doc "world-correct-constraint-positions! : World × (List Constraint) → Void")
(doc "Apply position correction for all joints.")
(define (world-correct-constraint-positions! world constraints)
  (for-each
   (lambda (c)
           (when (constraint-solver-position c)
                 ((constraint-solver-position c) world c)))
   constraints))

(doc "world-integrate-velocities! : World × Number → Void")
(doc "Apply forces and update velocities (first half of integration).")
(define (world-integrate-velocities! world dt)
  (let ([gravity (world-gravity world)]
        [entities (world-entity-list world)])
       (for-each
        (lambda (e)
                (unless (entity-static? e)
                        (let* ([body (entity-body e)]
                               [mass (body-mass body)]
                               [inv-mass (body-inv-mass body)]
                               ;; Apply gravity
                               [gravity-force (vec2-scale gravity mass)]
                               [accel (vec2-scale gravity-force inv-mass)]
                               [new-vel (vec2-add (body-vel body)
                                                  (vec2-scale accel dt))]
                               [new-body (body-with-vel body new-vel)])
                              (world-update-entity! world (entity-id e)
                                                    (lambda (ent)
                                                            (entity-with-body ent new-body))))))
        entities)))

(doc "world-integrate-positions! : World × Number → Void")
(doc "Update positions from velocities.")
(define (world-integrate-positions! world dt)
  (let ([entities (world-entity-list world)])
       (for-each
        (lambda (e)
                (unless (entity-static? e)
                        (let* ([body (entity-body e)]
                               [new-pos (vec2-add (body-pos body)
                                                  (vec2-scale (body-vel body) dt))]
                               [new-body (body-with-pos body new-pos)])
                              (world-update-entity! world (entity-id e)
                                                    (lambda (ent)
                                                            (entity-with-body ent new-body))))))
        entities)))

(doc 'section 'collision)

(doc 'world-detect-collisions 'type 'World → (List Collision))
(doc "Detect all collisions in the world.")
(doc "Returns list of (entity-a entity-b manifold).")
(doc "Uses entity-world-shape to get shapes in world coordinates.")
(define (world-detect-collisions world)
  (let* ([hash (world-spatial-hash world)]
         [entities (world-entity-list world)])
        ;; Clear and rebuild spatial hash with world-space AABBs
        (spatial-hash-clear! hash)
        (for-each
         (lambda (e)
                 (spatial-hash-insert! hash (entity-id e)
                                       (shape-aabb (entity-world-shape e))))
         entities)
        ;; Find collision pairs
        (let ([checked (list hamt-empty)])  ; mutable box holding HAMT
             (fold-left
              (lambda (collisions e)
                      (let* ([id-a (entity-id e)]
                             [shape-a (entity-world-shape e)]
                             [candidates (spatial-hash-query hash (shape-aabb shape-a))])
                            (fold-left
                             (lambda (collisions id-b)
                                     (if (or (eq? id-a id-b)
                                             (hamt-lookup (make-pair-key id-a id-b)
                                                          (car checked)))
                                         collisions
                                         (begin
                                           ;; Mark as checked
                                           (set-car! checked
                                                     (hamt-assoc (make-pair-key id-a id-b) #t
                                                                 (car checked)))
                                           ;; Narrow phase with world-space shapes
                                           (let* ([ent-b (world-get-entity world id-b)]
                                                  [shape-b (entity-world-shape ent-b)]
                                                  [manifold (shapes-manifold shape-a shape-b)])
                                                 (if manifold
                                                     (cons (list e ent-b manifold)
                                                           collisions)
                                                     collisions)))))
                             collisions candidates)))
              '() entities))))

(doc 'make-pair-key 'type 'Any × Any → Any)
(doc "Create a canonical key for a pair of ids.")
(define (make-pair-key a b)
  (if (< (equal-hash a) (equal-hash b))
      (cons a b)
      (cons b a)))


(doc 'section 'convenience)

(doc 'make-circle-entity 'type 'Any × Vec2 × Number × Number × Material → Entity)
(doc "Create a circular physics entity.")
(doc "Shape is stored in local coordinates (centered at origin).")
(define (make-circle-entity id pos radius mass material)
  (let ([body (make-body-2d pos (vec2 0 0) mass)]
        [shape (make-circle (vec2 0 0) radius)])  ; Local coords
       (make-entity id body shape material #f)))

(doc 'make-box-entity 'type 'Any × Vec2 × Vec2 × Number × Material → Entity)
(doc "Create a box physics entity.")
(doc "Shape is stored in local coordinates (centered at origin).")
(define (make-box-entity id pos half-extents mass material)
  (let ([body (make-body-2d pos (vec2 0 0) mass)]
        [shape (make-box (vec2 0 0) half-extents)])  ; Local coords
       (make-entity id body shape material #f)))

(doc 'make-static-circle 'type 'Any × Vec2 × Number × Material → Entity)
(doc "Create a static circular entity.")
(doc "Shape is stored in local coordinates (centered at origin).")
(define (make-static-circle id pos radius material)
  (let ([body (make-static-body pos)]
        [shape (make-circle (vec2 0 0) radius)])  ; Local coords
       (make-entity id body shape material #f)))

(doc 'make-static-box 'type 'Any × Vec2 × Vec2 × Material → Entity)
(doc "Create a static box entity.")
(doc "Shape is stored in local coordinates (centered at origin).")
(define (make-static-box id pos half-extents material)
  (let ([body (make-static-body pos)]
        [shape (make-box (vec2 0 0) half-extents)])  ; Local coords
       (make-entity id body shape material #f)))

(doc 'make-ground 'type 'Any × Number × Number × Number → Entity)
(doc "Create a static ground plane at given y position.")
(doc "Shape is stored in local coordinates.")
(define (make-ground id y width material)
  (let* ([half-width (/ width 2)]
         [pos (vec2 0 y)]
         [body (make-static-body pos)]
         ;; Local coords: centered at origin, extends down
         [shape (make-aabb (vec2 (- half-width) -100)
                           (vec2 half-width 0))])
        (make-entity id body shape material #f)))

(doc 'section 'world)

(doc 'world-query-aabb 'type 'World × AABB → (List Entity))
(doc "Find all entities overlapping an AABB.")
(define (world-query-aabb world aabb)
  ;; Rebuild spatial hash with current entities using world-space shapes
  (let* ([hash (world-spatial-hash world)]
         [entities (world-entity-list world)])
        (spatial-hash-clear! hash)
        (for-each
         (lambda (e)
                 (spatial-hash-insert! hash (entity-id e)
                                       (shape-aabb (entity-world-shape e))))
         entities)
        ;; Query and filter using world-space shapes
        (let ([candidates (spatial-hash-query hash aabb)])
             (filter
              (lambda (e)
                      (and e (aabb-aabb? aabb (shape-aabb (entity-world-shape e)))))
              (map (lambda (id) (world-get-entity world id))
                   candidates)))))

(doc 'world-query-point 'type 'World × Vec2 → (List Entity))
(doc "Find all entities containing a point.")
(define (world-query-point world point)
  (let ([entities (world-entity-list world)])
       (filter
        (lambda (e)
                (let ([shape (entity-world-shape e)])  ; Use world-space shape
                     (cond
                      [(circle? shape) (point-in-circle? point shape)]
                      [(aabb? shape) (point-in-aabb? point shape)]
                      [(polygon? shape) (point-in-polygon? point shape)]
                      [else #f])))
        entities)))

(doc 'world-raycast-closest 'type 'World × Ray2 → (Entity . HitInfo) or #f)
(doc "Cast ray through world, returning closest hit with full shape testing.")
(define (world-raycast-closest world ray)
  (let* ([entities (world-entity-list world)]
         [result (fold-left
                  (lambda (best e)
                          (let* ([shape (entity-world-shape e)]
                                 [hit (ray2-shape ray shape)]
                                 [best-dist (caddr best)])
                                (if (and hit (< (hit-info-distance hit) best-dist))
                                    (list hit e (hit-info-distance hit))
                                    best)))
                  (list #f #f (ray2-max-dist ray))
                  entities)]
         [best-hit (car result)]
         [best-entity (cadr result)])
        (if best-hit
            (cons best-entity best-hit)
            #f)))

(doc 'world-raycast-all 'type 'World × Ray2 → (List (Entity . HitInfo)))
(doc "Cast ray through world, returning all hits sorted by distance.")
(define (world-raycast-all world ray)
  (let* ([entities (world-entity-list world)]
         [hits (fold-left
                (lambda (acc e)
                        (let* ([shape (entity-world-shape e)]  ; Use world-space shape
                               [hit (ray2-shape ray shape)])
                              (if hit
                                  (cons (cons e hit) acc)
                                  acc)))
                '() entities)])
        ;; Sort by distance
        (sort-by (lambda (a b)
                           (< (hit-info-distance (cdr a))
                              (hit-info-distance (cdr b))))
                   hits)))

(doc 'section 'entity)

(doc "apply-force! : World × Any × Vec2 → Void")
(doc "Apply a force to an entity.")
(define (apply-force! world id force)
  (world-update-entity!
   world id
   (lambda (e)
           (if (entity-static? e)
               e
               (let* ([body (entity-body e)]
                      [inv-mass (body-inv-mass body)]
                      ;; Impulse: F = ma → Δv = F/m (for dt=1)
                      [new-vel (vec2-add (body-vel body)
                                         (vec2-scale force inv-mass))]
                      [new-body (body-with-vel body new-vel)])
                     (entity-with-body e new-body))))))

(doc "apply-impulse! : World × Any × Vec2 → Void")
(doc "Apply an impulse to an entity (instant velocity change).")
(define (apply-impulse! world id impulse)
  (world-update-entity!
   world id
   (lambda (e)
           (if (entity-static? e)
               e
               (let* ([body (entity-body e)]
                      [inv-mass (body-inv-mass body)]
                      [new-vel (vec2-add (body-vel body)
                                         (vec2-scale impulse inv-mass))]
                      [new-body (body-with-vel body new-vel)])
                     (entity-with-body e new-body))))))

(doc "set-velocity! : World × Any × Vec2 → Void")
(doc "Set an entity's velocity directly.")
(define (set-velocity! world id velocity)
  (world-update-entity!
   world id
   (lambda (e)
           (let* ([body (entity-body e)]
                  [new-body (body-with-vel body velocity)])
                 (entity-with-body e new-body)))))

(doc "set-position! : World × Any × Vec2 → Void")
(doc "Set an entity's position directly.")
(define (set-position! world id position)
  (world-update-entity!
   world id
   (lambda (e)
           (let* ([body (entity-body e)]
                  [new-body (body-with-pos body position)]
                  [old-shape (entity-shape e)]
                  ;; Also update shape position
                  [old-pos (body-pos body)]
                  [offset (vec2-sub position old-pos)]
                  [new-shape (transform-shape old-shape offset)])
                 (make-entity (entity-id e) new-body new-shape
                              (entity-material e) (entity-user-data e))))))

(doc 'section 'convenience)

(doc "world-resolve-collisions! : World × (List Collision) → Void")
(doc "Resolve collisions with both velocity impulses and position correction.")
(define (world-resolve-collisions! world collisions)
  (world-resolve-collision-velocities! world collisions)
  (world-correct-collision-positions! world collisions))

(doc 'world-raycast 'type 'World × Vec2 × Vec2 × Number → (List Entity))
(doc "Cast a ray and return list of entities hit, sorted by distance.")
(define (world-raycast world origin direction max-dist)
  (let ([ray (make-ray2 origin direction max-dist)])
       (map car (world-raycast-all world ray))))
