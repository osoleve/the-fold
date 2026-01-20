(load "core/base/prelude.ss")
(load "lattice/linalg/vec3.ss")
(load "lattice/linalg/quaternion.ss")
(load "lattice/physics/classical3d/rigid-body3d.ss")
(load "lattice/physics/classical3d/shapes3d.ss")
(load "lattice/physics/classical3d/collision-detection3d.ss")
(load "lattice/physics/classical3d/constraint-solver3d.ss")

(doc 'module 'world3d)
(doc 'description "3D Physics World - The physics world coordinates all 3D physics components: Body management (add, remove, query), Collision detection (broad + narrow phase), Collision resolution (impulse + correction), Constraint solving, Integration (forces, velocity, position).")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'physics-entity)

(doc 'note "A physics entity combines: id (unique identifier), body (3D rigid body with position, velocity, orientation, angular velocity), shape (collision shape), material (physics material with restitution and friction), user-data (optional user data)")

;;; make-entity-3d : Any × RigidBody3D × Shape3D × Material × Any → Entity3D
(define (make-entity-3d id body shape material user-data)
  (list 'entity-3d id body shape material user-data))

;;; entity-3d? : Any → Boolean
(define (entity-3d? e)
  (and (pair? e) (eq? (car e) 'entity-3d)))

;;; entity-3d-id : Entity3D → Any
(define (entity-3d-id e) (list-ref e 1))

;;; entity-3d-body : Entity3D → RigidBody3D
(define (entity-3d-body e) (list-ref e 2))

;;; entity-3d-shape : Entity3D → Shape3D
(define (entity-3d-shape e) (list-ref e 3))

;;; entity-3d-material : Entity3D → Material
(define (entity-3d-material e) (list-ref e 4))

;;; entity-3d-user-data : Entity3D → Any
(define (entity-3d-user-data e) (list-ref e 5))

;;; entity-3d-with-body : Entity3D × RigidBody3D → Entity3D
(define (entity-3d-with-body e new-body)
  (make-entity-3d (entity-3d-id e) new-body (entity-3d-shape e)
                  (entity-3d-material e) (entity-3d-user-data e)))

;;; entity-3d-pos : Entity3D → Vec3
(define (entity-3d-pos e) (rigid-body-3d-pos (entity-3d-body e)))

;;; entity-3d-vel : Entity3D → Vec3
(define (entity-3d-vel e) (rigid-body-3d-vel (entity-3d-body e)))

;;; entity-3d-static? : Entity3D → Boolean
(define (entity-3d-static? e) (rigid-body-3d-static? (entity-3d-body e)))

;;; ====
;;; Material (shared with 2D)
;;; ====

;;; make-material : Number × Number → Material
;;; Create physics material with restitution and friction.
(define (make-material restitution friction)
  (list 'material restitution friction))

;;; material? : Any → Boolean
(define (material? m)
  (and (pair? m) (eq? (car m) 'material)))

;;; material-restitution : Material → Number
(define (material-restitution m) (list-ref m 1))

;;; material-friction : Material → Number
(define (material-friction m) (list-ref m 2))

;;; Default materials
(define *default-material* (make-material 0.3 0.5))
(define *bouncy-material* (make-material 0.9 0.3))
(define *sticky-material* (make-material 0.0 0.9))

;;; ====
;;; Time Accumulator (for fixed timestep)
;;; ====

;;; make-time-acc-3d : Number × Nat → TimeAcc3D
;;; Create timestep accumulator for fixed-timestep simulation.
(define (make-time-acc-3d fixed-dt max-substeps)
  (list 'time-acc-3d fixed-dt max-substeps 0.0))

;;; time-acc-3d-fixed-dt : TimeAcc3D → Number
(define (time-acc-3d-fixed-dt acc) (list-ref acc 1))

;;; time-acc-3d-max-substeps : TimeAcc3D → Nat
(define (time-acc-3d-max-substeps acc) (list-ref acc 2))

;;; time-acc-3d-accumulated : TimeAcc3D → Number
(define (time-acc-3d-accumulated acc) (list-ref acc 3))

;;; time-acc-3d-add : TimeAcc3D × Number → TimeAcc3D
;;; Add elapsed time to accumulator.
(define (time-acc-3d-add acc dt)
  (list 'time-acc-3d
        (time-acc-3d-fixed-dt acc)
        (time-acc-3d-max-substeps acc)
        (+ (time-acc-3d-accumulated acc) dt)))

;;; time-acc-3d-consume : TimeAcc3D → (TimeAcc3D × Number)
;;; Consume one fixed timestep if available.
;;; Returns (new-acc, consumed-dt) where consumed-dt is 0 or fixed-dt.
(define (time-acc-3d-consume acc)
  (let ([fixed-dt (time-acc-3d-fixed-dt acc)]
        [accumulated (time-acc-3d-accumulated acc)])
       (if (>= accumulated fixed-dt)
           (list (list 'time-acc-3d
                       fixed-dt
                       (time-acc-3d-max-substeps acc)
                       (- accumulated fixed-dt))
                 fixed-dt)
           (list acc 0))))

;;; ====
;;; 3D Spatial Hash (Broad Phase)
;;; ====

;;; make-spatial-hash-3d : Number → SpatialHash3D
;;; Create 3D spatial hash with given cell size.
(define (make-spatial-hash-3d cell-size)
  (list 'spatial-hash-3d
        cell-size
        (make-hashtable equal-hash equal?)))

;;; spatial-hash-3d-cell-size : SpatialHash3D → Number
(define (spatial-hash-3d-cell-size h) (list-ref h 1))

;;; spatial-hash-3d-table : SpatialHash3D → Hashtable
(define (spatial-hash-3d-table h) (list-ref h 2))

;;; spatial-hash-3d-key : SpatialHash3D × Vec3 → (Integer × Integer × Integer)
;;; Convert position to cell key.
(define (spatial-hash-3d-key hash pos)
  (let ([cell-size (spatial-hash-3d-cell-size hash)])
       (list (exact (floor (/ (vec3-x pos) cell-size)))
             (exact (floor (/ (vec3-y pos) cell-size)))
             (exact (floor (/ (vec3-z pos) cell-size))))))

;;; spatial-hash-3d-aabb-keys : SpatialHash3D × AABB3D → (List Key)
;;; Get all cell keys overlapping an AABB.
(define (spatial-hash-3d-aabb-keys hash aabb)
  (let* ([cell-size (spatial-hash-3d-cell-size hash)]
         [min-pt (aabb3d-min aabb)]
         [max-pt (aabb3d-max aabb)]
         [min-x (exact (floor (/ (vec3-x min-pt) cell-size)))]
         [min-y (exact (floor (/ (vec3-y min-pt) cell-size)))]
         [min-z (exact (floor (/ (vec3-z min-pt) cell-size)))]
         [max-x (exact (floor (/ (vec3-x max-pt) cell-size)))]
         [max-y (exact (floor (/ (vec3-y max-pt) cell-size)))]
         [max-z (exact (floor (/ (vec3-z max-pt) cell-size)))])
        (let loop-x ([x min-x] [keys '()])
             (if (> x max-x)
                 keys
                 (loop-x (+ x 1)
                         (let loop-y ([y min-y] [ks keys])
                              (if (> y max-y)
                                  ks
                                  (loop-y (+ y 1)
                                          (let loop-z ([z min-z] [kz ks])
                                               (if (> z max-z)
                                                   kz
                                                   (loop-z (+ z 1)
                                                           (cons (list x y z) kz))))))))))))

;;; spatial-hash-3d-clear! : SpatialHash3D → Void
(define (spatial-hash-3d-clear! hash)
  (hashtable-clear! (spatial-hash-3d-table hash)))

;;; spatial-hash-3d-insert! : SpatialHash3D × Any × AABB3D → Void
;;; Insert id into all cells overlapping its AABB.
(define (spatial-hash-3d-insert! hash id aabb)
  (let ([table (spatial-hash-3d-table hash)]
        [keys (spatial-hash-3d-aabb-keys hash aabb)])
       (for-each
        (lambda (key)
                (let ([current (hashtable-ref table key '())])
                     (hashtable-set! table key (cons id current))))
        keys)))

;;; spatial-hash-3d-query : SpatialHash3D × AABB3D → (List Any)
;;; Query all ids in cells overlapping the AABB.
(define (spatial-hash-3d-query hash aabb)
  (let* ([table (spatial-hash-3d-table hash)]
         [keys (spatial-hash-3d-aabb-keys hash aabb)]
         [seen (make-hashtable equal-hash equal?)]
         [results '()])
        (for-each
         (lambda (key)
                 (let ([ids (hashtable-ref table key '())])
                      (for-each
                       (lambda (id)
                               (unless (hashtable-ref seen id #f)
                                       (hashtable-set! seen id #t)
                                       (set! results (cons id results))))
                       ids)))
         keys)
        results))

;;; ====
;;; Physics World 3D
;;; ====

;;; A physics world contains:
;;;   - entities: hashtable of id → entity
;;;   - gravity: gravity force vector
;;;   - spatial-hash: broad phase structure
;;;   - config: world configuration
;;;   - time-acc: timestep accumulator
;;;   - constraints: hashtable of id → constraint

;;; make-world-config-3d : → WorldConfig3D
(define (make-world-config-3d)
  (list 'world-config-3d
        0.016667            ; fixed-dt (60 fps)
        10                  ; max-substeps
        8                   ; solver-iterations
        32))                ; spatial-hash-cell-size

;;; config-3d-fixed-dt : WorldConfig3D → Number
(define (config-3d-fixed-dt c) (list-ref c 1))

;;; config-3d-max-substeps : WorldConfig3D → Nat
(define (config-3d-max-substeps c) (list-ref c 2))

;;; config-3d-solver-iterations : WorldConfig3D → Nat
(define (config-3d-solver-iterations c) (list-ref c 3))

;;; config-3d-cell-size : WorldConfig3D → Number
(define (config-3d-cell-size c) (list-ref c 4))

;;; make-world-3d : Vec3 → World3D
;;; Create a new 3D physics world with given gravity.
(define (make-world-3d gravity)
  (let ([config (make-world-config-3d)])
       (list 'world-3d
             (make-hashtable equal-hash equal?)  ; entities
             gravity
             (make-spatial-hash-3d (config-3d-cell-size config))
             config
             (make-time-acc-3d (config-3d-fixed-dt config)
                               (config-3d-max-substeps config))
             (make-hashtable equal-hash equal?))))  ; constraints

;;; world-3d? : Any → Boolean
(define (world-3d? w)
  (and (pair? w) (eq? (car w) 'world-3d)))

;;; world-3d-entities : World3D → Hashtable
(define (world-3d-entities w) (list-ref w 1))

;;; world-3d-gravity : World3D → Vec3
(define (world-3d-gravity w) (list-ref w 2))

;;; world-3d-spatial-hash : World3D → SpatialHash3D
(define (world-3d-spatial-hash w) (list-ref w 3))

;;; world-3d-config : World3D → WorldConfig3D
(define (world-3d-config w) (list-ref w 4))

;;; world-3d-time-acc : World3D → TimeAcc3D
(define (world-3d-time-acc w) (list-ref w 5))

;;; world-3d-constraints : World3D → Hashtable
(define (world-3d-constraints w) (list-ref w 6))

;;; world-3d-with-time-acc : World3D × TimeAcc3D → World3D
(define (world-3d-with-time-acc w new-acc)
  (list 'world-3d
        (world-3d-entities w)
        (world-3d-gravity w)
        (world-3d-spatial-hash w)
        (world-3d-config w)
        new-acc
        (world-3d-constraints w)))

;;; ====
;;; Entity Management
;;; ====

;;; world-3d-add-entity! : World3D × Entity3D → Void
(define (world-3d-add-entity! world entity)
  (hashtable-set! (world-3d-entities world)
                  (entity-3d-id entity)
                  entity))

;;; world-3d-remove-entity! : World3D × Any → Void
(define (world-3d-remove-entity! world id)
  (hashtable-delete! (world-3d-entities world) id))

;;; world-3d-get-entity : World3D × Any → Entity3D | #f
(define (world-3d-get-entity world id)
  (hashtable-ref (world-3d-entities world) id #f))

;;; world-3d-entity-list : World3D → (List Entity3D)
(define (world-3d-entity-list world)
  (let-values ([(keys vals) (hashtable-entries (world-3d-entities world))])
              (vector->list vals)))

;;; world-3d-update-entity! : World3D × Any × (Entity3D → Entity3D) → Void
(define (world-3d-update-entity! world id f)
  (let ([entity (world-3d-get-entity world id)])
       (when entity
             (hashtable-set! (world-3d-entities world) id (f entity)))))

;;; ====
;;; Constraint Management
;;; ====

;;; world-3d-add-constraint! : World3D × Constraint3D → Void
(define (world-3d-add-constraint! world constraint)
  (let ([id (constraint-3d-id constraint)])
       (hashtable-set! (world-3d-constraints world) id constraint)))

;;; world-3d-remove-constraint! : World3D × Any → Void
(define (world-3d-remove-constraint! world id)
  (hashtable-delete! (world-3d-constraints world) id))

;;; world-3d-get-constraint : World3D × Any → Constraint3D | #f
(define (world-3d-get-constraint world id)
  (hashtable-ref (world-3d-constraints world) id #f))

;;; world-3d-constraint-list : World3D → (List Constraint3D)
(define (world-3d-constraint-list world)
  (let-values ([(keys vals) (hashtable-entries (world-3d-constraints world))])
              (vector->list vals)))

;;; ====
;;; World Step
;;; ====

;;; world-3d-step! : World3D × Number → World3D
;;; Step the physics simulation forward by dt seconds.
(define (world-3d-step! world dt)
  (let* ([config (world-3d-config world)]
         [fixed-dt (config-3d-fixed-dt config)]
         [max-steps (config-3d-max-substeps config)]
         [time-acc (time-acc-3d-add (world-3d-time-acc world) dt)])
        ;; Consume fixed timesteps
        (let loop ([acc time-acc] [steps 0])
             (let* ([result (time-acc-3d-consume acc)]
                    [new-acc (car result)]
                    [consumed (cadr result)])
                   (if (or (= consumed 0) (>= steps max-steps))
                       ;; Done stepping, return world with updated time accumulator
                       (world-3d-with-time-acc world new-acc)
                       ;; Perform one physics step
                       (begin
                        (world-3d-fixed-step! world fixed-dt)
                        (loop new-acc (+ steps 1))))))))

;;; world-3d-fixed-step! : World3D × Number → Void
;;; Perform one fixed-timestep physics update.
(define (world-3d-fixed-step! world dt)
  ;; 1. Apply forces and integrate velocities
  (world-3d-integrate-velocities! world dt)
  ;; 2. Detect collisions
  (let ([collisions (world-3d-detect-collisions world)]
        [constraints (world-3d-constraint-list world)]
        [iterations (config-3d-solver-iterations (world-3d-config world))])
       ;; 3. Solve velocity constraints (interleaved)
       (let iter-loop ([i 0])
            (when (< i iterations)
                  ;; Solve collision velocity constraints
                  (world-3d-resolve-collision-velocities! world collisions)
                  ;; Solve joint velocity constraints
                  (world-3d-solve-constraint-velocities! world constraints dt)
                  (iter-loop (+ i 1))))
       ;; 4. Integrate positions and orientations
       (world-3d-integrate-positions! world dt)
       ;; 5. Position correction
       (let iter-loop ([i 0])
            (when (< i (quotient iterations 2))
                  (world-3d-correct-collision-positions! world collisions)
                  (world-3d-correct-constraint-positions! world constraints)
                  (iter-loop (+ i 1))))))

;;; ====
;;; Velocity Integration
;;; ====

;;; world-3d-integrate-velocities! : World3D × Number → Void
;;; Apply forces and update velocities (first half of integration).
(define (world-3d-integrate-velocities! world dt)
  (let ([gravity (world-3d-gravity world)]
        [entities (world-3d-entity-list world)])
       (for-each
        (lambda (e)
                (unless (entity-3d-static? e)
                        (let* ([body (entity-3d-body e)]
                               [mass (rigid-body-3d-mass body)]
                               ;; Apply gravity
                               [gravity-force (vec3-scale gravity mass)]
                               [accel gravity]  ; F = ma, so a = F/m = gravity
                               [new-vel (vec3-add (rigid-body-3d-vel body)
                                                  (vec3-scale accel dt))]
                               [new-body (rigid-body-3d-with-vel body new-vel)])
                              (world-3d-update-entity! world (entity-3d-id e)
                                                       (lambda (ent)
                                                               (entity-3d-with-body ent new-body))))))
        entities)))

;;; ====
;;; Position Integration
;;; ====

;;; world-3d-integrate-positions! : World3D × Number → Void
;;; Update positions and orientations from velocities.
(define (world-3d-integrate-positions! world dt)
  (let ([entities (world-3d-entity-list world)])
       (for-each
        (lambda (e)
                (unless (entity-3d-static? e)
                        (let* ([body (entity-3d-body e)]
                               ;; Update position
                               [new-pos (vec3-add (rigid-body-3d-pos body)
                                                  (vec3-scale (rigid-body-3d-vel body) dt))]
                               ;; Update orientation using quaternion integration
                               [omega (rigid-body-3d-angular-vel body)]
                               [orient (rigid-body-3d-orientation body)]
                               [new-orient (quat-integrate orient omega dt)]
                               [body-1 (rigid-body-3d-with-pos body new-pos)]
                               [new-body (rigid-body-3d-with-orientation body-1 new-orient)])
                              (world-3d-update-entity! world (entity-3d-id e)
                                                       (lambda (ent)
                                                               (entity-3d-with-body ent new-body))))))
        entities)))

;;; ====
;;; Collision Detection
;;; ====

;;; world-3d-detect-collisions : World3D → (List Collision3D)
;;; Detect all collisions in the world.
;;; Returns list of (entity-a entity-b manifold).
(define (world-3d-detect-collisions world)
  (let* ([hash (world-3d-spatial-hash world)]
         [entities (world-3d-entity-list world)])
        ;; Clear and rebuild spatial hash
        (spatial-hash-3d-clear! hash)
        (for-each
         (lambda (e)
                 (spatial-hash-3d-insert! hash (entity-3d-id e)
                                          (shape3d-aabb (entity-3d-shape e))))
         entities)
        ;; Find collision pairs
        (let ([checked (make-hashtable equal-hash equal?)]
              [collisions '()])
             (for-each
              (lambda (e)
                      (let* ([id-a (entity-3d-id e)]
                             [shape-a (entity-3d-shape e)]
                             [candidates (spatial-hash-3d-query hash (shape3d-aabb shape-a))])
                            (for-each
                             (lambda (id-b)
                                     (when (and (not (eq? id-a id-b))
                                                (not (hashtable-ref checked
                                                                    (make-pair-key-3d id-a id-b) #f)))
                                           ;; Mark as checked
                                           (hashtable-set! checked (make-pair-key-3d id-a id-b) #t)
                                           ;; Narrow phase
                                           (let* ([ent-b (world-3d-get-entity world id-b)]
                                                  [shape-b (entity-3d-shape ent-b)]
                                                  [manifold (shapes-manifold-3d shape-a shape-b)])
                                                 (when manifold
                                                       (set! collisions
                                                             (cons (list e ent-b manifold)
                                                                   collisions))))))
                             candidates)))
              entities)
             collisions)))

;;; make-pair-key-3d : Any × Any → Any
;;; Create a canonical key for a pair of ids.
(define (make-pair-key-3d a b)
  (if (< (equal-hash a) (equal-hash b))
      (cons a b)
      (cons b a)))

;;; ====
;;; Collision Resolution
;;; ====

;;; world-3d-resolve-collision-velocities! : World3D × (List Collision3D) → Void
;;; Resolve collision velocity constraints (impulses only).
(define (world-3d-resolve-collision-velocities! world collisions)
  (for-each
   (lambda (collision)
           (let* ([ent-a (car collision)]
                  [ent-b (cadr collision)]
                  [manifold (caddr collision)]
                  [normal (manifold-3d-normal manifold)]
                  [penetration (manifold-3d-penetration manifold)]
                  [contact (manifold-3d-contact manifold)])
                 (let* ([current-a (world-3d-get-entity world (entity-3d-id ent-a))]
                        [current-b (world-3d-get-entity world (entity-3d-id ent-b))])
                       (when (and current-a current-b)
                             (let* ([body-a (entity-3d-body current-a)]
                                    [body-b (entity-3d-body current-b)]
                                    [mat-a (entity-3d-material current-a)]
                                    [mat-b (entity-3d-material current-b)]
                                    [restitution (min (material-restitution mat-a)
                                                      (material-restitution mat-b))]
                                    [friction (sqrt (* (material-friction mat-a)
                                                       (material-friction mat-b)))]
                                    [result (resolve-collision-3d body-a body-b
                                                                  normal contact
                                                                  restitution friction)]
                                    [new-body-a (car result)]
                                    [new-body-b (cadr result)])
                                   (world-3d-update-entity! world (entity-3d-id ent-a)
                                                            (lambda (e) (entity-3d-with-body e new-body-a)))
                                   (world-3d-update-entity! world (entity-3d-id ent-b)
                                                            (lambda (e) (entity-3d-with-body e new-body-b))))))))
   collisions))

;;; resolve-collision-3d : RigidBody3D × RigidBody3D × Vec3 × Vec3 × Number × Number → (RigidBody3D × RigidBody3D)
;;; Apply collision impulse between two bodies.
(define (resolve-collision-3d body-a body-b normal contact restitution friction)
  ;; Compute relative velocity at contact point
  (let* ([vel-a (rigid-body-3d-velocity-at body-a contact)]
         [vel-b (rigid-body-3d-velocity-at body-b contact)]
         [rel-vel (vec3-sub vel-a vel-b)]
         [vel-along-normal (vec3-dot rel-vel normal)])
        ;; Only resolve if approaching
        (if (> vel-along-normal 0)
            (values body-a body-b)  ; Separating, do nothing
            (let* ([inv-mass-a (rigid-body-3d-inv-mass body-a)]
                   [inv-mass-b (rigid-body-3d-inv-mass body-b)]
                   [r-a (vec3-sub contact (rigid-body-3d-pos body-a))]
                   [r-b (vec3-sub contact (rigid-body-3d-pos body-b))]
                   ;; Compute effective mass
                   [r-a-cross-n (vec3-cross r-a normal)]
                   [r-b-cross-n (vec3-cross r-b normal)]
                   [inv-inertia-a (rigid-body-3d-inv-inertia-world body-a)]
                   [inv-inertia-b (rigid-body-3d-inv-inertia-world body-b)]
                   [ang-term-a (vec3-cross (mat3-mul-vec3 inv-inertia-a r-a-cross-n) r-a)]
                   [ang-term-b (vec3-cross (mat3-mul-vec3 inv-inertia-b r-b-cross-n) r-b)]
                   [effective-mass (+ inv-mass-a inv-mass-b
                                      (vec3-dot ang-term-a normal)
                                      (vec3-dot ang-term-b normal))]
                   ;; Compute impulse magnitude
                   [j (/ (- (* (+ 1 restitution) vel-along-normal))
                         effective-mass)]
                   [impulse (vec3-scale normal j)]
                   ;; Apply impulse
                   [new-a (rigid-body-3d-apply-impulse body-a impulse contact)]
                   [new-b (rigid-body-3d-apply-impulse body-b (vec3-negate impulse) contact)])
                  ;; Apply friction
                  (apply-friction-3d new-a new-b contact rel-vel normal j friction)))))

;;; apply-friction-3d : RigidBody3D × RigidBody3D × Vec3 × Vec3 × Vec3 × Number × Number → (RigidBody3D × RigidBody3D)
;;; Apply friction impulse.
(define (apply-friction-3d body-a body-b contact rel-vel normal j-normal friction)
  (let* ([tangent-vel (vec3-sub rel-vel (vec3-scale normal (vec3-dot rel-vel normal)))]
         [tangent-speed (vec3-magnitude tangent-vel)])
        (if (< tangent-speed 0.0001)
            (values body-a body-b)  ; No tangential motion
            (let* ([tangent (vec3-scale tangent-vel (/ -1 tangent-speed))]
                   [j-friction (min (* friction (abs j-normal)) tangent-speed)]
                   [friction-impulse (vec3-scale tangent j-friction)]
                   [new-a (rigid-body-3d-apply-impulse body-a friction-impulse contact)]
                   [new-b (rigid-body-3d-apply-impulse body-b (vec3-negate friction-impulse) contact)])
                  (values new-a new-b)))))

;;; world-3d-correct-collision-positions! : World3D × (List Collision3D) → Void
;;; Apply position correction for collisions.
(define (world-3d-correct-collision-positions! world collisions)
  (let ([slop 0.01]
        [percent 0.2])
       (for-each
        (lambda (collision)
                (let* ([ent-a (car collision)]
                       [ent-b (cadr collision)]
                       [manifold (caddr collision)]
                       [normal (manifold-3d-normal manifold)]
                       [penetration (manifold-3d-penetration manifold)])
                      (let* ([current-a (world-3d-get-entity world (entity-3d-id ent-a))]
                             [current-b (world-3d-get-entity world (entity-3d-id ent-b))])
                            (when (and current-a current-b)
                                  (let* ([body-a (entity-3d-body current-a)]
                                         [body-b (entity-3d-body current-b)]
                                         [inv-mass-a (rigid-body-3d-inv-mass body-a)]
                                         [inv-mass-b (rigid-body-3d-inv-mass body-b)]
                                         [correction-mag (* percent
                                                            (max 0 (- penetration slop))
                                                            (/ 1 (+ inv-mass-a inv-mass-b)))]
                                         [correction (vec3-scale normal correction-mag)]
                                         [new-pos-a (vec3-sub (rigid-body-3d-pos body-a)
                                                              (vec3-scale correction inv-mass-a))]
                                         [new-pos-b (vec3-add (rigid-body-3d-pos body-b)
                                                              (vec3-scale correction inv-mass-b))]
                                         [new-body-a (rigid-body-3d-with-pos body-a new-pos-a)]
                                         [new-body-b (rigid-body-3d-with-pos body-b new-pos-b)])
                                        (world-3d-update-entity! world (entity-3d-id ent-a)
                                                                 (lambda (e) (entity-3d-with-body e new-body-a)))
                                        (world-3d-update-entity! world (entity-3d-id ent-b)
                                                                 (lambda (e) (entity-3d-with-body e new-body-b))))))))
        collisions)))

;;; ====
;;; Constraint Solving
;;; ====

;;; world-3d-solve-constraint-velocities! : World3D × (List Constraint3D) × Number → Void
(define (world-3d-solve-constraint-velocities! world constraints dt)
  (for-each
   (lambda (c)
           (when (constraint-3d-solver-velocity c)
                 ((constraint-3d-solver-velocity c) world c dt)))
   constraints))

;;; world-3d-correct-constraint-positions! : World3D × (List Constraint3D) → Void
(define (world-3d-correct-constraint-positions! world constraints)
  (for-each
   (lambda (c)
           (when (constraint-3d-solver-position c)
                 ((constraint-3d-solver-position c) world c)))
   constraints))

;;; ====
;;; Convenience Constructors
;;; ====

;;; make-sphere-entity-3d : Any × Vec3 × Number × Number × Material → Entity3D
;;; Create a spherical physics entity.
(define (make-sphere-entity-3d id pos radius mass material)
  (let* ([inertia (inertia-solid-sphere mass radius)]
         [body (make-rigid-body-3d pos (vec3-zero) (quat-identity) (vec3-zero)
                                   mass inertia)]
         [shape (sphere3d pos radius)])
        (make-entity-3d id body shape material #f)))

;;; make-box-entity-3d : Any × Vec3 × Vec3 × Number × Material → Entity3D
;;; Create a box physics entity.
(define (make-box-entity-3d id pos half-extents mass material)
  (let* ([inertia (inertia-solid-box mass
                                     (* 2 (vec3-x half-extents))
                                     (* 2 (vec3-y half-extents))
                                     (* 2 (vec3-z half-extents)))]
         [body (make-rigid-body-3d pos (vec3-zero) (quat-identity) (vec3-zero)
                                   mass inertia)]
         [shape (box3d pos half-extents)])
        (make-entity-3d id body shape material #f)))

;;; make-static-sphere-3d : Any × Vec3 × Number × Material → Entity3D
(define (make-static-sphere-3d id pos radius material)
  (let* ([body (make-static-body-3d pos (quat-identity))]
         [shape (sphere3d pos radius)])
        (make-entity-3d id body shape material #f)))

;;; make-static-box-3d : Any × Vec3 × Vec3 × Material → Entity3D
(define (make-static-box-3d id pos half-extents material)
  (let* ([body (make-static-body-3d pos (quat-identity))]
         [shape (box3d pos half-extents)])
        (make-entity-3d id body shape material #f)))

;;; make-ground-3d : Any × Number × Number × Material → Entity3D
;;; Create a static ground plane at given y position.
(define (make-ground-3d id y extent material)
  (let* ([pos (vec3 0 (- y 50) 0)]
         [body (make-static-body-3d pos (quat-identity))]
         [shape (aabb3d (vec3 (- extent) (- y 100) (- extent))
                        (vec3 extent y extent))])
        (make-entity-3d id body shape material #f)))

;;; ====
;;; World Queries
;;; ====

;;; world-3d-query-aabb : World3D × AABB3D → (List Entity3D)
;;; Find all entities overlapping an AABB.
(define (world-3d-query-aabb world aabb)
  (let* ([hash (world-3d-spatial-hash world)]
         [entities (world-3d-entity-list world)])
        ;; Rebuild spatial hash with current entities
        (spatial-hash-3d-clear! hash)
        (for-each
         (lambda (e)
                 (spatial-hash-3d-insert! hash (entity-3d-id e)
                                          (shape3d-aabb (entity-3d-shape e))))
         entities)
        ;; Query and filter
        (let ([candidates (spatial-hash-3d-query hash aabb)])
             (filter
              (lambda (e)
                      (and e (aabb3d-aabb3d? aabb (shape3d-aabb (entity-3d-shape e)))))
              (map (lambda (id) (world-3d-get-entity world id))
                   candidates)))))

;;; world-3d-query-point : World3D × Vec3 → (List Entity3D)
;;; Find all entities containing a point.
(define (world-3d-query-point world point)
  (let ([entities (world-3d-entity-list world)])
       (filter
        (lambda (e)
                (let ([shape (entity-3d-shape e)])
                     (cond
                      [(sphere3d? shape) (point-in-sphere3d? point shape)]
                      [(box3d? shape) (point-in-box3d? point shape)]
                      [(aabb3d? shape) (point-in-aabb3d? point shape)]
                      [else #f])))
        entities)))

;;; ====
;;; Entity Manipulation
;;; ====

;;; apply-force-3d! : World3D × Any × Vec3 → Void
;;; Apply a force to an entity (integrated over dt).
(define (apply-force-3d! world id force)
  (world-3d-update-entity!
   world id
   (lambda (e)
           (if (entity-3d-static? e)
               e
               (let* ([body (entity-3d-body e)]
                      [inv-mass (rigid-body-3d-inv-mass body)]
                      [new-vel (vec3-add (rigid-body-3d-vel body)
                                         (vec3-scale force inv-mass))]
                      [new-body (rigid-body-3d-with-vel body new-vel)])
                     (entity-3d-with-body e new-body))))))

;;; apply-impulse-3d! : World3D × Any × Vec3 → Void
;;; Apply an impulse to an entity at center of mass.
(define (apply-impulse-3d! world id impulse)
  (world-3d-update-entity!
   world id
   (lambda (e)
           (if (entity-3d-static? e)
               e
               (let* ([body (entity-3d-body e)]
                      [inv-mass (rigid-body-3d-inv-mass body)]
                      [new-vel (vec3-add (rigid-body-3d-vel body)
                                         (vec3-scale impulse inv-mass))]
                      [new-body (rigid-body-3d-with-vel body new-vel)])
                     (entity-3d-with-body e new-body))))))

;;; apply-impulse-at-3d! : World3D × Any × Vec3 × Vec3 → Void
;;; Apply an impulse at a world point.
(define (apply-impulse-at-3d! world id impulse point)
  (world-3d-update-entity!
   world id
   (lambda (e)
           (if (entity-3d-static? e)
               e
               (let* ([body (entity-3d-body e)]
                      [new-body (rigid-body-3d-apply-impulse body impulse point)])
                     (entity-3d-with-body e new-body))))))

;;; set-velocity-3d! : World3D × Any × Vec3 → Void
(define (set-velocity-3d! world id velocity)
  (world-3d-update-entity!
   world id
   (lambda (e)
           (let* ([body (entity-3d-body e)]
                  [new-body (rigid-body-3d-with-vel body velocity)])
                 (entity-3d-with-body e new-body)))))

;;; set-position-3d! : World3D × Any × Vec3 → Void
(define (set-position-3d! world id position)
  (world-3d-update-entity!
   world id
   (lambda (e)
           (let* ([body (entity-3d-body e)]
                  [new-body (rigid-body-3d-with-pos body position)])
                 (entity-3d-with-body e new-body)))))

;;; set-orientation-3d! : World3D × Any × Quat → Void
(define (set-orientation-3d! world id orientation)
  (world-3d-update-entity!
   world id
   (lambda (e)
           (let* ([body (entity-3d-body e)]
                  [new-body (rigid-body-3d-with-orientation body orientation)])
                 (entity-3d-with-body e new-body)))))

