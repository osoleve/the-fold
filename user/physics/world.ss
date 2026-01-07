;;; fabric/stitches/physics-2d/world.ss — 2D Physics World
;;;
;;; The physics world coordinates all physics components:
;;;   - Body management (add, remove, query)
;;;   - Collision detection (broad + narrow phase)
;;;   - Collision resolution (impulse + correction)
;;;   - Integration (forces, velocity, position)
;;;
;;; This is Core code: pure simulation step, with world mutation.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec2.ss
;;;   - physics-2d/integrators.ss
;;;   - physics-2d/collision-detection.ss
;;;   - physics-2d/collision-response.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec2.ss")
(load "user/physics/integrators.ss")
(load "user/physics/collision-detection.ss")
(load "user/physics/collision-response.ss")
(load "user/physics/raycasting.ss")

;;; ============================================================
;;; Physics Entity
;;; ============================================================

;;; A physics entity combines:
;;;   - id: unique identifier
;;;   - body: physics body (position, velocity, mass)
;;;   - shape: collision shape
;;;   - material: physics material (restitution, friction)
;;;   - user-data: optional user data

;;; make-entity : Any × Body2D × Shape × Material × Any → Entity
(define (make-entity id body shape material user-data)
  (list 'entity id body shape material user-data))

;;; entity? : Any → Boolean
(define (entity? e)
  (and (pair? e) (eq? (car e) 'entity)))

;;; entity-id : Entity → Any
(define (entity-id e) (list-ref e 1))

;;; entity-body : Entity → Body2D
(define (entity-body e) (list-ref e 2))

;;; entity-shape : Entity → Shape
(define (entity-shape e) (list-ref e 3))

;;; entity-material : Entity → Material
(define (entity-material e) (list-ref e 4))

;;; entity-user-data : Entity → Any
(define (entity-user-data e) (list-ref e 5))

;;; entity-with-body : Entity × Body2D → Entity
(define (entity-with-body e new-body)
  (make-entity (entity-id e) new-body (entity-shape e)
               (entity-material e) (entity-user-data e)))

;;; entity-pos : Entity → Vec2
(define (entity-pos e) (body-pos (entity-body e)))

;;; entity-vel : Entity → Vec2
(define (entity-vel e) (body-vel (entity-body e)))

;;; entity-static? : Entity → Boolean
(define (entity-static? e) (body-static? (entity-body e)))

;;; ============================================================
;;; Shape Transformation
;;; ============================================================

;;; transform-shape : Shape × Vec2 → Shape
;;; Translate a shape by the given offset.
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

;;; entity-world-shape : Entity → Shape
;;; Get entity's shape in world coordinates.
(define (entity-world-shape e)
  (let ([shape (entity-shape e)]
        [pos (entity-pos e)])
       ;; Shapes are stored relative to origin, transform by position
       ;; (For now, assume shapes are already in world coords from body pos)
       shape))

;;; ============================================================
;;; Physics World
;;; ============================================================

;;; A physics world contains:
;;;   - entities: hashtable of id → entity
;;;   - gravity: gravity force vector
;;;   - spatial-hash: broad phase structure
;;;   - config: world configuration

;;; make-world-config : → WorldConfig
(define (make-world-config)
  (list 'world-config
        0.016667            ; fixed-dt (60 fps)
        10                  ; max-substeps
        8                   ; solver-iterations
        32))                ; spatial-hash-cell-size

;;; config-fixed-dt : WorldConfig → Number
(define (config-fixed-dt c) (list-ref c 1))

;;; config-max-substeps : WorldConfig → Nat
(define (config-max-substeps c) (list-ref c 2))

;;; config-solver-iterations : WorldConfig → Nat
(define (config-solver-iterations c) (list-ref c 3))

;;; config-cell-size : WorldConfig → Number
(define (config-cell-size c) (list-ref c 4))

;;; make-world : Vec2 → World
;;; Create a new physics world with given gravity.
(define (make-world gravity)
  (let ([config (make-world-config)])
       (list 'world
             (make-hashtable equal-hash equal?)  ; entities
             gravity
             (make-spatial-hash (config-cell-size config))
             config
             (make-time-acc (config-fixed-dt config)
                            (config-max-substeps config)))))

;;; world? : Any → Boolean
(define (world? w)
  (and (pair? w) (eq? (car w) 'world)))

;;; world-entities : World → Hashtable
(define (world-entities w) (list-ref w 1))

;;; world-gravity : World → Vec2
(define (world-gravity w) (list-ref w 2))

;;; world-spatial-hash : World → SpatialHash
(define (world-spatial-hash w) (list-ref w 3))

;;; world-config : World → WorldConfig
(define (world-config w) (list-ref w 4))

;;; world-time-acc : World → TimeAcc
(define (world-time-acc w) (list-ref w 5))

;;; world-with-time-acc : World × TimeAcc → World
(define (world-with-time-acc w new-acc)
  (list 'world
        (world-entities w)
        (world-gravity w)
        (world-spatial-hash w)
        (world-config w)
        new-acc))

;;; ============================================================
;;; Entity Management
;;; ============================================================

;;; world-add-entity! : World × Entity → Void
;;; Add an entity to the world.
(define (world-add-entity! world entity)
  (hashtable-set! (world-entities world)
                  (entity-id entity)
                  entity))

;;; world-remove-entity! : World × Any → Void
;;; Remove an entity by id.
(define (world-remove-entity! world id)
  (hashtable-delete! (world-entities world) id))

;;; world-get-entity : World × Any → Entity | #f
;;; Get entity by id.
(define (world-get-entity world id)
  (hashtable-ref (world-entities world) id #f))

;;; world-entity-list : World → (List Entity)
;;; Get all entities as a list.
(define (world-entity-list world)
  (let-values ([(keys vals) (hashtable-entries (world-entities world))])
              (vector->list vals)))

;;; world-update-entity! : World × Any × (Entity → Entity) → Void
;;; Update an entity by applying a function.
(define (world-update-entity! world id f)
  (let ([entity (world-get-entity world id)])
       (when entity
             (hashtable-set! (world-entities world) id (f entity)))))

;;; ============================================================
;;; Force Accumulation
;;; ============================================================

;;; A force accumulator for an entity
(define (make-force-acc)
  (make-hashtable equal-hash equal?))

;;; force-acc-add! : ForceAcc × Any × Vec2 → Void
;;; Add force to an entity's accumulator.
(define (force-acc-add! acc id force)
  (let ([current (hashtable-ref acc id (vec2 0 0))])
       (hashtable-set! acc id (vec2-add current force))))

;;; force-acc-get : ForceAcc × Any → Vec2
;;; Get accumulated force for an entity.
(define (force-acc-get acc id)
  (hashtable-ref acc id (vec2 0 0)))

;;; force-acc-clear! : ForceAcc → Void
(define (force-acc-clear! acc)
  (hashtable-clear! acc))

;;; ============================================================
;;; World Step
;;; ============================================================

;;; world-step! : World × Number → Void
;;; Step the physics simulation forward by dt seconds.
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

;;; world-fixed-step! : World × Number → Void
;;; Perform one fixed-timestep physics update.
(define (world-fixed-step! world dt)
  ;; 1. Apply forces and integrate velocities
  (world-integrate-velocities! world dt)
  ;; 2. Detect collisions
  (let ([collisions (world-detect-collisions world)])
       ;; 3. Resolve collisions
       (world-resolve-collisions! world collisions)
       ;; 4. Integrate positions
       (world-integrate-positions! world dt)))

;;; world-integrate-velocities! : World × Number → Void
;;; Apply forces and update velocities (first half of integration).
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

;;; world-integrate-positions! : World × Number → Void
;;; Update positions from velocities.
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

;;; ============================================================
;;; Collision Detection
;;; ============================================================

;;; world-detect-collisions : World → (List Collision)
;;; Detect all collisions in the world.
;;; Returns list of (entity-a entity-b manifold).
(define (world-detect-collisions world)
  (let* ([hash (world-spatial-hash world)]
         [entities (world-entity-list world)])
        ;; Clear and rebuild spatial hash
        (spatial-hash-clear! hash)
        (for-each
         (lambda (e)
                 (spatial-hash-insert! hash (entity-id e)
                                       (shape-aabb (entity-shape e))))
         entities)
        ;; Find collision pairs
        (let ([checked (make-hashtable equal-hash equal?)]
              [collisions '()])
             (for-each
              (lambda (e)
                      (let* ([id-a (entity-id e)]
                             [shape-a (entity-shape e)]
                             [candidates (spatial-hash-query hash (shape-aabb shape-a))])
                            (for-each
                             (lambda (id-b)
                                     (when (and (not (eq? id-a id-b))
                                                (not (hashtable-ref checked
                                                                    (make-pair-key id-a id-b) #f)))
                                           ;; Mark as checked
                                           (hashtable-set! checked (make-pair-key id-a id-b) #t)
                                           ;; Narrow phase
                                           (let* ([ent-b (world-get-entity world id-b)]
                                                  [shape-b (entity-shape ent-b)]
                                                  [manifold (shapes-manifold shape-a shape-b)])
                                                 (when manifold
                                                       (set! collisions
                                                             (cons (list e ent-b manifold)
                                                                   collisions))))))
                             candidates)))
              entities)
             collisions)))

;;; make-pair-key : Any × Any → Any
;;; Create a canonical key for a pair of ids.
(define (make-pair-key a b)
  (if (< (equal-hash a) (equal-hash b))
      (cons a b)
      (cons b a)))

;;; ============================================================
;;; Collision Resolution
;;; ============================================================

;;; world-resolve-collisions! : World × (List Collision) → Void
;;; Resolve all detected collisions.
(define (world-resolve-collisions! world collisions)
  (let ([iterations (config-solver-iterations (world-config world))])
       ;; Multiple iterations for stability
       (let iter-loop ([i 0])
            (when (< i iterations)
                  (for-each
                   (lambda (collision)
                           (let* ([ent-a (car collision)]
                                  [ent-b (cadr collision)]
                                  [manifold-data (caddr collision)]
                                  [normal (car manifold-data)]
                                  [penetration (cadr manifold-data)]
                                  [contact (caddr manifold-data)])
                                 ;; Get current bodies (may have changed)
                                 (let* ([current-a (world-get-entity world (entity-id ent-a))]
                                        [current-b (world-get-entity world (entity-id ent-b))])
                                       (when (and current-a current-b)
                                             (let* ([body-a (entity-body current-a)]
                                                    [body-b (entity-body current-b)]
                                                    [mat-a (entity-material current-a)]
                                                    [mat-b (entity-material current-b)]
                                                    ;; Create manifold with current bodies
                                                    [manifold (make-manifold body-a body-b
                                                                             normal penetration contact)]
                                                    ;; Resolve
                                                    [result (resolve-with-correction manifold mat-a mat-b)]
                                                    [new-body-a (car result)]
                                                    [new-body-b (cadr result)])
                                                   ;; Update entities
                                                   (world-update-entity!
                                                    world (entity-id ent-a)
                                                    (lambda (e) (entity-with-body e new-body-a)))
                                                   (world-update-entity!
                                                    world (entity-id ent-b)
                                                    (lambda (e) (entity-with-body e new-body-b))))))))
                   collisions)
                  (iter-loop (+ i 1))))))

;;; ============================================================
;;; Convenience Constructors
;;; ============================================================

;;; make-circle-entity : Any × Vec2 × Number × Number × Material → Entity
;;; Create a circular physics entity.
(define (make-circle-entity id pos radius mass material)
  (let ([body (make-body-2d pos (vec2 0 0) mass)]
        [shape (make-circle pos radius)])
       (make-entity id body shape material #f)))

;;; make-box-entity : Any × Vec2 × Vec2 × Number × Material → Entity
;;; Create a box physics entity.
(define (make-box-entity id pos half-extents mass material)
  (let ([body (make-body-2d pos (vec2 0 0) mass)]
        [shape (make-box pos half-extents)])
       (make-entity id body shape material #f)))

;;; make-static-circle : Any × Vec2 × Number × Material → Entity
;;; Create a static circular entity.
(define (make-static-circle id pos radius material)
  (let ([body (make-static-body pos)]
        [shape (make-circle pos radius)])
       (make-entity id body shape material #f)))

;;; make-static-box : Any × Vec2 × Vec2 × Material → Entity
;;; Create a static box entity.
(define (make-static-box id pos half-extents material)
  (let ([body (make-static-body pos)]
        [shape (make-box pos half-extents)])
       (make-entity id body shape material #f)))

;;; make-ground : Any × Number × Number × Number → Entity
;;; Create a static ground plane at given y position.
(define (make-ground id y width material)
  (let* ([half-width (/ width 2)]
         [pos (vec2 0 y)]
         [body (make-static-body pos)]
         [shape (make-aabb (vec2 (- half-width) (- y 100))
                           (vec2 half-width y))])
        (make-entity id body shape material #f)))

;;; ============================================================
;;; World Queries
;;; ============================================================

;;; world-query-aabb : World × AABB → (List Entity)
;;; Find all entities overlapping an AABB.
(define (world-query-aabb world aabb)
  ;; Rebuild spatial hash with current entities
  (let* ([hash (world-spatial-hash world)]
         [entities (world-entity-list world)])
        (spatial-hash-clear! hash)
        (for-each
         (lambda (e)
                 (spatial-hash-insert! hash (entity-id e)
                                       (shape-aabb (entity-shape e))))
         entities)
        ;; Query and filter
        (let ([candidates (spatial-hash-query hash aabb)])
             (filter
              (lambda (e)
                      (and e (aabb-aabb? aabb (shape-aabb (entity-shape e)))))
              (map (lambda (id) (world-get-entity world id))
                   candidates)))))

;;; world-query-point : World × Vec2 → (List Entity)
;;; Find all entities containing a point.
(define (world-query-point world point)
  (let ([entities (world-entity-list world)])
       (filter
        (lambda (e)
                (let ([shape (entity-shape e)])
                     (cond
                      [(circle? shape) (point-in-circle? point shape)]
                      [(aabb? shape) (point-in-aabb? point shape)]
                      [(polygon? shape) (point-in-polygon? point shape)]
                      [else #f])))
        entities)))

;;; world-raycast-closest : World × Ray2 → (Entity . HitInfo) | #f
;;; Cast ray through world, returning closest hit with full shape testing.
(define (world-raycast-closest world ray)
  (let* ([entities (world-entity-list world)]
         [best-hit #f]
         [best-entity #f]
         [best-dist (ray2-max-dist ray)])
        (for-each
         (lambda (e)
                 (let* ([shape (entity-shape e)]
                        [hit (ray2-shape ray shape)])
                       (when (and hit (< (hit-info-distance hit) best-dist))
                             (set! best-hit hit)
                             (set! best-entity e)
                             (set! best-dist (hit-info-distance hit)))))
         entities)
        (if best-hit
            (cons best-entity best-hit)
            #f)))

;;; world-raycast-all : World × Ray2 → (List (Entity . HitInfo))
;;; Cast ray through world, returning all hits sorted by distance.
(define (world-raycast-all world ray)
  (let* ([entities (world-entity-list world)]
         [hits '()])
        (for-each
         (lambda (e)
                 (let* ([shape (entity-shape e)]
                        [hit (ray2-shape ray shape)])
                       (when hit
                             (set! hits (cons (cons e hit) hits)))))
         entities)
        ;; Sort by distance
        (list-sort (lambda (a b)
                           (< (hit-info-distance (cdr a))
                              (hit-info-distance (cdr b))))
                   hits)))

;;; ============================================================
;;; Entity Manipulation
;;; ============================================================

;;; apply-force! : World × Any × Vec2 → Void
;;; Apply a force to an entity.
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

;;; apply-impulse! : World × Any × Vec2 → Void
;;; Apply an impulse to an entity (instant velocity change).
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

;;; set-velocity! : World × Any × Vec2 → Void
;;; Set an entity's velocity directly.
(define (set-velocity! world id velocity)
  (world-update-entity!
   world id
   (lambda (e)
           (let* ([body (entity-body e)]
                  [new-body (body-with-vel body velocity)])
                 (entity-with-body e new-body)))))

;;; set-position! : World × Any × Vec2 → Void
;;; Set an entity's position directly.
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
