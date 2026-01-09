;;; core/fp/game/physics-dsl.ss — Physics Simulation DSL via Free Monad
;;;
;;; A domain-specific language for physics simulations built on the Free monad.
;;; Separates simulation description from execution, enabling multiple interpreters:
;;;   - Deterministic: Direct execution using the physics engine
;;;   - Stochastic: Adds random noise for uncertainty modeling
;;;   - Logging: Records all commands for replay/debugging
;;;   - Pure: Functional state threading for testing/autodiff
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/control/free.ss
;;;   - linalg/vec2.ss
;;;   - physics/world.ss (for interpreters)

(load "core/base/prelude.ss")
(load "lattice/fp/control/free.ss")
(load "lattice/linalg/vec2.ss")

;;; Load physics engine components for entity/body structures
(load "lattice/physics/classical/integrators.ss")
(load "lattice/physics/classical/collision-detection.ss")
(load "lattice/physics/classical/collision-response.ss")
(load "lattice/physics/classical/world.ss")

;;; ============================================================
;;; PhysicsF Functor - Command Algebra
;;; ============================================================
;;;
;;; PhysicsF represents the primitive operations of a physics simulation.
;;; Each command is a tagged list containing operation data and a continuation.
;;;
;;; Commands:
;;;   ('apply-force id force next)        — Apply force to entity
;;;   ('apply-impulse id impulse next)    — Apply instant velocity change
;;;   ('set-position id pos next)         — Set entity position
;;;   ('set-velocity id vel next)         — Set entity velocity
;;;   ('step dt next)                     — Advance simulation by dt
;;;   ('spawn entity k)                   — Add entity, pass id to k
;;;   ('destroy id next)                  — Remove entity
;;;   ('get-world k)                      — Get world state, pass to k
;;;   ('get-entity id k)                  — Get entity by id, pass to k
;;;   ('detect-collisions k)              — Run collision detection, pass manifolds to k

;;; physics-fmap : (a -> b) -> PhysicsF a -> PhysicsF b
;;; Functor instance for physics commands.
(define (physics-fmap f cmd)
  (let ([tag (car cmd)])
       (case tag
             [(apply-force)
              (let ([id (list-ref cmd 1)]
                    [force (list-ref cmd 2)]
                    [next (list-ref cmd 3)])
                   (list 'apply-force id force (f next)))]
             [(apply-impulse)
              (let ([id (list-ref cmd 1)]
                    [impulse (list-ref cmd 2)]
                    [next (list-ref cmd 3)])
                   (list 'apply-impulse id impulse (f next)))]
             [(set-position)
              (let ([id (list-ref cmd 1)]
                    [pos (list-ref cmd 2)]
                    [next (list-ref cmd 3)])
                   (list 'set-position id pos (f next)))]
             [(set-velocity)
              (let ([id (list-ref cmd 1)]
                    [vel (list-ref cmd 2)]
                    [next (list-ref cmd 3)])
                   (list 'set-velocity id vel (f next)))]
             [(step)
              (let ([dt (list-ref cmd 1)]
                    [next (list-ref cmd 2)])
                   (list 'step dt (f next)))]
             [(spawn)
              (let ([entity (list-ref cmd 1)]
                    [k (list-ref cmd 2)])
                   (list 'spawn entity (lambda (id) (f (k id)))))]
             [(destroy)
              (let ([id (list-ref cmd 1)]
                    [next (list-ref cmd 2)])
                   (list 'destroy id (f next)))]
             [(get-world)
              (let ([k (list-ref cmd 1)])
                   (list 'get-world (lambda (w) (f (k w)))))]
             [(get-entity)
              (let ([id (list-ref cmd 1)]
                    [k (list-ref cmd 2)])
                   (list 'get-entity id (lambda (e) (f (k e)))))]
             [(detect-collisions)
              (let ([k (list-ref cmd 1)])
                   (list 'detect-collisions (lambda (cs) (f (k cs)))))]
             [else (error 'physics-fmap "Unknown physics command" tag)])))

;;; ============================================================
;;; Lifting Functions (Smart Constructors)
;;; ============================================================
;;;
;;; These create Free monad values from primitive commands.
;;; They provide the monadic interface for building simulations.

;;; phys-apply-force : id -> Vec2 -> Free PhysicsF ()
;;; Apply a force to an entity.
(define (phys-apply-force id force)
  (free (list 'apply-force id force (pure-free '()))))

;;; phys-apply-impulse : id -> Vec2 -> Free PhysicsF ()
;;; Apply an impulse (instant velocity change) to an entity.
(define (phys-apply-impulse id impulse)
  (free (list 'apply-impulse id impulse (pure-free '()))))

;;; phys-set-position : id -> Vec2 -> Free PhysicsF ()
;;; Set an entity's position.
(define (phys-set-position id pos)
  (free (list 'set-position id pos (pure-free '()))))

;;; phys-set-velocity : id -> Vec2 -> Free PhysicsF ()
;;; Set an entity's velocity.
(define (phys-set-velocity id vel)
  (free (list 'set-velocity id vel (pure-free '()))))

;;; phys-step : Number -> Free PhysicsF ()
;;; Advance the simulation by dt seconds.
(define (phys-step dt)
  (free (list 'step dt (pure-free '()))))

;;; phys-spawn : Entity -> Free PhysicsF id
;;; Spawn an entity, returning its id.
(define (phys-spawn entity)
  (free (list 'spawn entity pure-free)))

;;; phys-destroy : id -> Free PhysicsF ()
;;; Remove an entity from the simulation.
(define (phys-destroy id)
  (free (list 'destroy id (pure-free '()))))

;;; phys-get-world : Free PhysicsF World
;;; Get the current world state.
(define phys-get-world
  (free (list 'get-world pure-free)))

;;; phys-get-entity : id -> Free PhysicsF (Maybe Entity)
;;; Get an entity by id.
(define (phys-get-entity id)
  (free (list 'get-entity id pure-free)))

;;; phys-detect-collisions : Free PhysicsF (List Collision)
;;; Detect all current collisions.
(define phys-detect-collisions
  (free (list 'detect-collisions pure-free)))

;;; ============================================================
;;; Monadic Combinators
;;; ============================================================

;;; phys-bind : Free PhysicsF a -> (a -> Free PhysicsF b) -> Free PhysicsF b
;;; Bind specialized for physics DSL.
(define (phys-bind m f)
  (free-bind physics-fmap m f))

;;; phys-then : Free PhysicsF a -> Free PhysicsF b -> Free PhysicsF b
;;; Sequence, discarding first result.
(define (phys-then m1 m2)
  (free-then physics-fmap m1 m2))

;;; phys-sequence : (List (Free PhysicsF a)) -> Free PhysicsF (List a)
;;; Sequence a list of physics computations.
(define (phys-sequence ms)
  (free-sequence physics-fmap ms))

;;; phys-for-each : (List a) -> (a -> Free PhysicsF ()) -> Free PhysicsF ()
;;; Execute a physics action for each element.
(define (phys-for-each lst f)
  (free-for-each physics-fmap lst f))

;;; phys-when : Boolean -> Free PhysicsF () -> Free PhysicsF ()
;;; Conditional execution.
(define (phys-when cond action)
  (free-when physics-fmap cond action))

;;; phys-steps : Nat -> Number -> Free PhysicsF ()
;;; Run n simulation steps of dt seconds each.
(define (phys-steps n dt)
  (if (<= n 0)
      (pure-free '())
      (phys-then (phys-step dt)
                 (phys-steps (- n 1) dt))))

;;; ============================================================
;;; Deterministic Interpreter
;;; ============================================================
;;;
;;; Executes physics commands directly using the physics engine.
;;; State is a World from user/physics/world.ss.

;;; run-physics-deterministic : Free PhysicsF a -> World -> (a . World)
;;; Run a physics program deterministically.
;;; Requires world.ss to be loaded for world operations.
(define (run-physics-deterministic program)
  (lambda (world)
          (let loop ([prog program] [w world])
               (if (pure-free? prog)
                   (cons (from-pure-free prog) w)
                   (let* ([cmd (from-free prog)]
                          [tag (car cmd)])
                         (case tag
                               [(apply-force)
                                (let ([id (list-ref cmd 1)]
                                      [force (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (apply-force! w id force)
                                     (loop next w))]
                               [(apply-impulse)
                                (let ([id (list-ref cmd 1)]
                                      [impulse (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (apply-impulse! w id impulse)
                                     (loop next w))]
                               [(set-position)
                                (let ([id (list-ref cmd 1)]
                                      [pos (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (set-position! w id pos)
                                     (loop next w))]
                               [(set-velocity)
                                (let ([id (list-ref cmd 1)]
                                      [vel (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (set-velocity! w id vel)
                                     (loop next w))]
                               [(step)
                                (let ([dt (list-ref cmd 1)]
                                      [next (list-ref cmd 2)])
                                     (let ([new-w (world-step! w dt)])
                                          (loop next (or new-w w))))]
                               [(spawn)
                                (let ([entity (list-ref cmd 1)]
                                      [k (list-ref cmd 2)])
                                     (let ([id (entity-id entity)])
                                          (world-add-entity! w entity)
                                          (loop (k id) w)))]
                               [(destroy)
                                (let ([id (list-ref cmd 1)]
                                      [next (list-ref cmd 2)])
                                     (world-remove-entity! w id)
                                     (loop next w))]
                               [(get-world)
                                (let ([k (list-ref cmd 1)])
                                     (loop (k w) w))]
                               [(get-entity)
                                (let ([id (list-ref cmd 1)]
                                      [k (list-ref cmd 2)])
                                     (let ([entity (world-get-entity w id)])
                                          (loop (k entity) w)))]
                               [(detect-collisions)
                                (let ([k (list-ref cmd 1)])
                                     (let ([collisions (world-detect-collisions w)])
                                          (loop (k collisions) w)))]
                               [else (error 'run-physics-deterministic "Unknown command" tag)]))))))

;;; ============================================================
;;; Logging Interpreter
;;; ============================================================
;;;
;;; Records all commands while executing them.
;;; Useful for debugging, replay, and visualization.

;;; make-log-entry : Symbol -> List -> LogEntry
(define (make-log-entry tag args)
  (list 'log-entry tag args))

;;; log-entry-tag : LogEntry -> Symbol
(define (log-entry-tag e) (list-ref e 1))

;;; log-entry-args : LogEntry -> List
(define (log-entry-args e) (list-ref e 2))

;;; run-physics-logging : Free PhysicsF a -> World -> (a . World . Log)
;;; Run a physics program, logging all commands.
(define (run-physics-logging program)
  (lambda (world)
          (let loop ([prog program] [w world] [log '()])
               (if (pure-free? prog)
                   (list (from-pure-free prog) w (reverse log))
                   (let* ([cmd (from-free prog)]
                          [tag (car cmd)])
                         (case tag
                               [(apply-force)
                                (let ([id (list-ref cmd 1)]
                                      [force (list-ref cmd 2)]
                                      [next (list-ref cmd 3)]
                                      [entry (make-log-entry 'apply-force (list id force))])
                                     (apply-force! w id force)
                                     (loop next w (cons entry log)))]
                               [(apply-impulse)
                                (let ([id (list-ref cmd 1)]
                                      [impulse (list-ref cmd 2)]
                                      [next (list-ref cmd 3)]
                                      [entry (make-log-entry 'apply-impulse (list id impulse))])
                                     (apply-impulse! w id impulse)
                                     (loop next w (cons entry log)))]
                               [(set-position)
                                (let ([id (list-ref cmd 1)]
                                      [pos (list-ref cmd 2)]
                                      [next (list-ref cmd 3)]
                                      [entry (make-log-entry 'set-position (list id pos))])
                                     (set-position! w id pos)
                                     (loop next w (cons entry log)))]
                               [(set-velocity)
                                (let ([id (list-ref cmd 1)]
                                      [vel (list-ref cmd 2)]
                                      [next (list-ref cmd 3)]
                                      [entry (make-log-entry 'set-velocity (list id vel))])
                                     (set-velocity! w id vel)
                                     (loop next w (cons entry log)))]
                               [(step)
                                (let ([dt (list-ref cmd 1)]
                                      [next (list-ref cmd 2)]
                                      [entry (make-log-entry 'step (list dt))])
                                     (let ([new-w (world-step! w dt)])
                                          (loop next (or new-w w) (cons entry log))))]
                               [(spawn)
                                (let ([entity (list-ref cmd 1)]
                                      [k (list-ref cmd 2)]
                                      [id (entity-id entity)]
                                      [entry (make-log-entry 'spawn (list id))])
                                     (world-add-entity! w entity)
                                     (loop (k id) w (cons entry log)))]
                               [(destroy)
                                (let ([id (list-ref cmd 1)]
                                      [next (list-ref cmd 2)]
                                      [entry (make-log-entry 'destroy (list id))])
                                     (world-remove-entity! w id)
                                     (loop next w (cons entry log)))]
                               [(get-world)
                                (let ([k (list-ref cmd 1)]
                                      [entry (make-log-entry 'get-world '())])
                                     (loop (k w) w (cons entry log)))]
                               [(get-entity)
                                (let ([id (list-ref cmd 1)]
                                      [k (list-ref cmd 2)]
                                      [entry (make-log-entry 'get-entity (list id))])
                                     (let ([entity (world-get-entity w id)])
                                          (loop (k entity) w (cons entry log))))]
                               [(detect-collisions)
                                (let ([k (list-ref cmd 1)]
                                      [entry (make-log-entry 'detect-collisions '())])
                                     (let ([collisions (world-detect-collisions w)])
                                          (loop (k collisions) w (cons entry log))))]
                               [else (error 'run-physics-logging "Unknown command" tag)]))))))

;;; ============================================================
;;; Pure Interpreter (Functional State)
;;; ============================================================
;;;
;;; A pure interpreter that doesn't mutate world state.
;;; Instead, it threads an immutable state representation.
;;; Suitable for testing, property-based testing, and autodiff.

;;; Pure world state representation:
;;;   (entities gravity forces)
;;; Where:
;;;   - entities is an alist of (id . entity)
;;;   - gravity is a Vec2
;;;   - forces is an alist of (id . force-accumulator)

;;; make-pure-world : Vec2 -> PureWorld
(define (make-pure-world gravity)
  (list '() gravity '()))

;;; pure-world-entities : PureWorld -> Alist
(define (pure-world-entities pw) (car pw))

;;; pure-world-gravity : PureWorld -> Vec2
(define (pure-world-gravity pw) (cadr pw))

;;; pure-world-forces : PureWorld -> Alist
(define (pure-world-forces pw) (if (null? (cddr pw)) '() (caddr pw)))

;;; pure-world-with-entities : PureWorld -> Alist -> PureWorld
(define (pure-world-with-entities pw entities)
  (list entities (pure-world-gravity pw) (pure-world-forces pw)))

;;; pure-world-with-forces : PureWorld -> Alist -> PureWorld
(define (pure-world-with-forces pw forces)
  (list (pure-world-entities pw) (pure-world-gravity pw) forces))

;;; pure-world-get-entity : PureWorld -> id -> Entity | #f
(define (pure-world-get-entity pw id)
  (let ([pair (assoc id (pure-world-entities pw))])
       (if pair (cdr pair) #f)))

;;; pure-world-add-entity : PureWorld -> Entity -> PureWorld
(define (pure-world-add-entity pw entity)
  (let ([id (entity-id entity)]
        [entities (pure-world-entities pw)])
       (pure-world-with-entities pw (cons (cons id entity) entities))))

;;; pure-world-remove-entity : PureWorld -> id -> PureWorld
(define (pure-world-remove-entity pw id)
  (let ([entities (filter (lambda (p) (not (equal? (car p) id)))
                          (pure-world-entities pw))])
       (pure-world-with-entities pw entities)))

;;; pure-world-update-entity : PureWorld -> id -> (Entity -> Entity) -> PureWorld
(define (pure-world-update-entity pw id f)
  (let ([entities (map (lambda (p)
                               (if (equal? (car p) id)
                                   (cons id (f (cdr p)))
                                   p))
                       (pure-world-entities pw))])
       (pure-world-with-entities pw entities)))

;;; pure-apply-force : PureWorld -> id -> Vec2 -> PureWorld
;;; Accumulate force to be applied during next step.
(define (pure-apply-force pw id force)
  (let* ([forces (pure-world-forces pw)]
         [existing (assoc id forces)]
         [current-force (if existing (cdr existing) (vec2 0 0))]
         [new-force (vec2-add current-force force)]
         [new-forces (if existing
                         (map (lambda (p)
                                      (if (equal? (car p) id)
                                          (cons id new-force)
                                          p))
                              forces)
                         (cons (cons id new-force) forces))])
        (pure-world-with-forces pw new-forces)))

;;; pure-apply-impulse : PureWorld -> id -> Vec2 -> PureWorld
;;; Apply impulse (instant velocity change) to entity.
;;; Unlike force, impulse is instant so no dt needed: Δv = J/m
(define (pure-apply-impulse pw id impulse)
  (pure-world-update-entity pw id
                            (lambda (e)
                                    (if (entity-static? e)
                                        e
                                        (let* ([body (entity-body e)]
                                               [inv-mass (body-inv-mass body)]
                                               [new-vel (vec2-add (body-vel body)
                                                                  (vec2-scale impulse inv-mass))]
                                               [new-body (body-with-vel body new-vel)])
                                              (entity-with-body e new-body))))))

;;; pure-set-position : PureWorld -> id -> Vec2 -> PureWorld
(define (pure-set-position pw id pos)
  (pure-world-update-entity pw id
                            (lambda (e)
                                    (let* ([body (entity-body e)]
                                           [new-body (body-with-pos body pos)])
                                          (entity-with-body e new-body)))))

;;; pure-set-velocity : PureWorld -> id -> Vec2 -> PureWorld
(define (pure-set-velocity pw id vel)
  (pure-world-update-entity pw id
                            (lambda (e)
                                    (let* ([body (entity-body e)]
                                           [new-body (body-with-vel body vel)])
                                          (entity-with-body e new-body)))))

;;; pure-step : PureWorld -> Number -> PureWorld
;;; Simple Euler integration step (no collision detection).
;;; Applies accumulated forces, then clears them.
(define (pure-step pw dt)
  (let* ([gravity (pure-world-gravity pw)]
         [entities (pure-world-entities pw)]
         [forces (pure-world-forces pw)]
         [updated (map (lambda (p)
                               (let ([id (car p)]
                                     [e (cdr p)])
                                    (if (entity-static? e)
                                        p
                                        (let* ([body (entity-body e)]
                                               [mass (body-mass body)]
                                               [inv-mass (body-inv-mass body)]
                                               ;; Apply gravity
                                               [gravity-force (vec2-scale gravity mass)]
                                               ;; Apply accumulated forces
                                               [accumulated (let ([pair (assoc id forces)])
                                                                 (if pair (cdr pair) (vec2 0 0)))]
                                               [total-force (vec2-add gravity-force accumulated)]
                                               [accel (vec2-scale total-force inv-mass)]
                                               [new-vel (vec2-add (body-vel body)
                                                                  (vec2-scale accel dt))]
                                               ;; Update position
                                               [new-pos (vec2-add (body-pos body)
                                                                  (vec2-scale new-vel dt))]
                                               [new-body (body-with-pos
                                                          (body-with-vel body new-vel)
                                                          new-pos)])
                                              (cons id (entity-with-body e new-body))))))
                       entities)])
        ;; Clear accumulated forces after applying them
        (pure-world-with-forces (pure-world-with-entities pw updated) '())))

;;; run-physics-pure : Free PhysicsF a -> PureWorld -> (a . PureWorld)
;;; Run a physics program purely (no mutation).
(define (run-physics-pure program)
  (lambda (pw)
          (let loop ([prog program] [w pw])
               (if (pure-free? prog)
                   (cons (from-pure-free prog) w)
                   (let* ([cmd (from-free prog)]
                          [tag (car cmd)])
                         (case tag
                               [(apply-force)
                                (let ([id (list-ref cmd 1)]
                                      [force (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (loop next (pure-apply-force w id force)))]
                               [(apply-impulse)
                                (let ([id (list-ref cmd 1)]
                                      [impulse (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (loop next (pure-apply-impulse w id impulse)))]
                               [(set-position)
                                (let ([id (list-ref cmd 1)]
                                      [pos (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (loop next (pure-set-position w id pos)))]
                               [(set-velocity)
                                (let ([id (list-ref cmd 1)]
                                      [vel (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (loop next (pure-set-velocity w id vel)))]
                               [(step)
                                (let ([dt (list-ref cmd 1)]
                                      [next (list-ref cmd 2)])
                                     (loop next (pure-step w dt)))]
                               [(spawn)
                                (let ([entity (list-ref cmd 1)]
                                      [k (list-ref cmd 2)])
                                     (let ([id (entity-id entity)])
                                          (loop (k id) (pure-world-add-entity w entity))))]
                               [(destroy)
                                (let ([id (list-ref cmd 1)]
                                      [next (list-ref cmd 2)])
                                     (loop next (pure-world-remove-entity w id)))]
                               [(get-world)
                                (let ([k (list-ref cmd 1)])
                                     (loop (k w) w))]
                               [(get-entity)
                                (let ([id (list-ref cmd 1)]
                                      [k (list-ref cmd 2)])
                                     (let ([entity (pure-world-get-entity w id)])
                                          (loop (k entity) w)))]
                               [(detect-collisions)
                                (let ([k (list-ref cmd 1)])
                                     ;; Pure collision detection: brute force O(N²) narrow phase
                                     ;; Detect collisions by checking all entity pairs
                                     (let* ([entities (pure-world-entities w)]
                                            [collisions
                                             (let outer-loop ([remaining entities] [cols '()])
                                                  (if (null? remaining)
                                                      cols
                                                      (let* ([pair-a (car remaining)]
                                                             [ent-a (cdr pair-a)]
                                                             [shape-a (entity-shape ent-a)])
                                                            (let inner-loop ([others (cdr remaining)] [acc cols])
                                                                 (if (null? others)
                                                                     (outer-loop (cdr remaining) acc)
                                                                     (let* ([pair-b (car others)]
                                                                            [ent-b (cdr pair-b)]
                                                                            [shape-b (entity-shape ent-b)]
                                                                            [manifold (shapes-manifold shape-a shape-b)])
                                                                           (if manifold
                                                                               (inner-loop (cdr others)
                                                                                           (cons (list ent-a ent-b manifold) acc))
                                                                               (inner-loop (cdr others) acc))))))))])
                                           (loop (k collisions) w)))]
                               [else (error 'run-physics-pure "Unknown command" tag)]))))))

;;; ============================================================
;;; Stochastic Interpreter
;;; ============================================================
;;;
;;; Adds random noise to forces and positions for uncertainty modeling.
;;; Threads a PRNG state alongside the world state.

;;; run-physics-stochastic : Free PhysicsF a -> Number -> World -> PRNG -> (a . World . PRNG)
;;; Run with stochastic noise. noise-scale controls magnitude of perturbations.
(define (run-physics-stochastic program noise-scale)
  (lambda (world prng)
          (let loop ([prog program] [w world] [rng prng])
               (if (pure-free? prog)
                   (list (from-pure-free prog) w rng)
                   (let* ([cmd (from-free prog)]
                          [tag (car cmd)])
                         (case tag
                               [(apply-force)
                                (let* ([id (list-ref cmd 1)]
                                       [force (list-ref cmd 2)]
                                       [next (list-ref cmd 3)]
                                       ;; Add noise to force
                                       [result1 (pcg-next-double rng)]
                                       [rng1 (car result1)]
                                       [noise-x (* noise-scale (- (cdr result1) 0.5))]
                                       [result2 (pcg-next-double rng1)]
                                       [rng2 (car result2)]
                                       [noise-y (* noise-scale (- (cdr result2) 0.5))]
                                       [noisy-force (vec2-add force (vec2 noise-x noise-y))])
                                      (apply-force! w id noisy-force)
                                      (loop next w rng2))]
                               [(apply-impulse)
                                (let* ([id (list-ref cmd 1)]
                                       [impulse (list-ref cmd 2)]
                                       [next (list-ref cmd 3)]
                                       ;; Add noise to impulse
                                       [result1 (pcg-next-double rng)]
                                       [rng1 (car result1)]
                                       [noise-x (* noise-scale (- (cdr result1) 0.5))]
                                       [result2 (pcg-next-double rng1)]
                                       [rng2 (car result2)]
                                       [noise-y (* noise-scale (- (cdr result2) 0.5))]
                                       [noisy-impulse (vec2-add impulse (vec2 noise-x noise-y))])
                                      (apply-impulse! w id noisy-impulse)
                                      (loop next w rng2))]
                               [(set-position)
                                (let ([id (list-ref cmd 1)]
                                      [pos (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     ;; No noise on explicit position setting
                                     (set-position! w id pos)
                                     (loop next w rng))]
                               [(set-velocity)
                                (let ([id (list-ref cmd 1)]
                                      [vel (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (set-velocity! w id vel)
                                     (loop next w rng))]
                               [(step)
                                (let ([dt (list-ref cmd 1)]
                                      [next (list-ref cmd 2)])
                                     (let ([new-w (world-step! w dt)])
                                          (loop next (or new-w w) rng)))]
                               [(spawn)
                                (let ([entity (list-ref cmd 1)]
                                      [k (list-ref cmd 2)])
                                     (let ([id (entity-id entity)])
                                          (world-add-entity! w entity)
                                          (loop (k id) w rng)))]
                               [(destroy)
                                (let ([id (list-ref cmd 1)]
                                      [next (list-ref cmd 2)])
                                     (world-remove-entity! w id)
                                     (loop next w rng))]
                               [(get-world)
                                (let ([k (list-ref cmd 1)])
                                     (loop (k w) w rng))]
                               [(get-entity)
                                (let ([id (list-ref cmd 1)]
                                      [k (list-ref cmd 2)])
                                     (let ([entity (world-get-entity w id)])
                                          (loop (k entity) w rng)))]
                               [(detect-collisions)
                                (let ([k (list-ref cmd 1)])
                                     (let ([collisions (world-detect-collisions w)])
                                          (loop (k collisions) w rng)))]
                               [else (error 'run-physics-stochastic "Unknown command" tag)]))))))

;;; ============================================================
;;; Visualization Interpreter
;;; ============================================================
;;;
;;; Produces a stream of world states for rendering.
;;; Each step emits the current world state.

;;; run-physics-visualize : Free PhysicsF a -> World -> (a . World . (List WorldSnapshot))
;;; Run and collect world snapshots after each step.
(define (run-physics-visualize program)
  (lambda (world)
          (let loop ([prog program] [w world] [snapshots '()])
               (if (pure-free? prog)
                   (list (from-pure-free prog) w (reverse snapshots))
                   (let* ([cmd (from-free prog)]
                          [tag (car cmd)])
                         (case tag
                               [(apply-force)
                                (let ([id (list-ref cmd 1)]
                                      [force (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (apply-force! w id force)
                                     (loop next w snapshots))]
                               [(apply-impulse)
                                (let ([id (list-ref cmd 1)]
                                      [impulse (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (apply-impulse! w id impulse)
                                     (loop next w snapshots))]
                               [(set-position)
                                (let ([id (list-ref cmd 1)]
                                      [pos (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (set-position! w id pos)
                                     (loop next w snapshots))]
                               [(set-velocity)
                                (let ([id (list-ref cmd 1)]
                                      [vel (list-ref cmd 2)]
                                      [next (list-ref cmd 3)])
                                     (set-velocity! w id vel)
                                     (loop next w snapshots))]
                               [(step)
                                (let ([dt (list-ref cmd 1)]
                                      [next (list-ref cmd 2)])
                                     (let ([new-w (world-step! w dt)])
                                          ;; Snapshot the world state after stepping
                                          (let* ([actual-w (or new-w w)]
                                                 [snapshot (snapshot-world actual-w)])
                                                (loop next actual-w (cons snapshot snapshots)))))]
                               [(spawn)
                                (let ([entity (list-ref cmd 1)]
                                      [k (list-ref cmd 2)])
                                     (let ([id (entity-id entity)])
                                          (world-add-entity! w entity)
                                          (loop (k id) w snapshots)))]
                               [(destroy)
                                (let ([id (list-ref cmd 1)]
                                      [next (list-ref cmd 2)])
                                     (world-remove-entity! w id)
                                     (loop next w snapshots))]
                               [(get-world)
                                (let ([k (list-ref cmd 1)])
                                     (loop (k w) w snapshots))]
                               [(get-entity)
                                (let ([id (list-ref cmd 1)]
                                      [k (list-ref cmd 2)])
                                     (let ([entity (world-get-entity w id)])
                                          (loop (k entity) w snapshots)))]
                               [(detect-collisions)
                                (let ([k (list-ref cmd 1)])
                                     (let ([collisions (world-detect-collisions w)])
                                          (loop (k collisions) w snapshots)))]
                               [else (error 'run-physics-visualize "Unknown command" tag)]))))))

;;; snapshot-world : World -> WorldSnapshot
;;; Create a snapshot of entity positions for visualization.
(define (snapshot-world world)
  (let ([entities (world-entity-list world)])
       (map (lambda (e)
                    (list (entity-id e)
                          (entity-pos e)
                          (entity-vel e)))
            entities)))

;;; ============================================================
;;; Example Programs
;;; ============================================================

;;; example-falling-ball : Entity -> Free PhysicsF Vec2
;;; Spawn a ball, simulate 60 frames, return final position.
(define (example-falling-ball entity)
  (phys-bind (phys-spawn entity)
             (lambda (id)
                     (phys-bind (phys-steps 60 0.016667)
                                (lambda (_)
                                        (phys-bind (phys-get-entity id)
                                                   (lambda (e)
                                                           (pure-free (if e (entity-pos e) (vec2 0 0))))))))))

;;; example-projectile : Vec2 -> Vec2 -> Free PhysicsF (List Vec2)
;;; Launch a projectile with initial velocity, collect positions over time.
(define (example-projectile initial-pos initial-vel)
  (phys-bind (phys-spawn (make-pure-entity 'projectile initial-pos 1.0))
             (lambda (id)
                     (phys-bind (phys-set-velocity id initial-vel)
                                (lambda (_)
                                        (let collect-loop ([n 60] [positions '()])
                                             (if (<= n 0)
                                                 (pure-free (reverse positions))
                                                 (phys-bind (phys-step 0.016667)
                                                            (lambda (_)
                                                                    (phys-bind (phys-get-entity id)
                                                                               (lambda (e)
                                                                                       (let ([pos (if e (entity-pos e) (vec2 0 0))])
                                                                                            (collect-loop (- n 1) (cons pos positions))))))))))))))

;;; make-pure-entity : id -> Vec2 -> Number -> Entity
;;; Helper to create a simple entity for pure interpreter.
(define (make-pure-entity id pos mass)
  (let ([body (make-body-2d pos (vec2 0 0) mass)]
        [shape (make-circle pos 1)]
        [material (make-material 0.5 0.3 0.3)])  ; restitution, static-friction, dynamic-friction
       (make-entity id body shape material #f)))
