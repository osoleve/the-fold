;;; lattice/physics/problems/simulation.ss — World Setup and Frame Capture
;;; @module simulation
;;; @requires prelude world ascii-renderer

(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
;; world.ss loads: integrators (bodies), collision-detection (shapes),
;; collision-response (materials), and other physics modules
(load "lattice/physics/classical/world.ss")
(load "lattice/physics/classical/ascii-renderer.ss")

(doc 'module 'simulation)
(doc 'description "World setup and frame capture for physics problems. Bridges the Dataset SDK with the classical physics engine.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Part 1: World Configuration
;;; ============================================================

(doc 'section 'world-config)

;;; make-problem-config : Number × Number × Number × Number × Number → ProblemConfig
;;; Create configuration for problem generation
(define (make-problem-config width height scale dt frames-per-step)
  (doc 'export #t)
  (list 'problem-config width height scale dt frames-per-step))

(define (problem-config? x)
  (and (pair? x) (eq? (car x) 'problem-config)))

(define (problem-config-width c) (list-ref c 1))
(define (problem-config-height c) (list-ref c 2))
(define (problem-config-scale c) (list-ref c 3))
(define (problem-config-dt c) (list-ref c 4))
(define (problem-config-frames-per-step c) (list-ref c 5))

;;; default-problem-config : → ProblemConfig
;;; Default: 70x35 characters, scale 2.0, dt=1/60, 4 physics steps per frame
(define (default-problem-config)
  (doc 'export #t)
  (make-problem-config 70 35 2.0 0.016667 4))

;;; high-res-problem-config : → ProblemConfig
;;; Higher resolution for precise problems
(define (high-res-problem-config)
  (doc 'export #t)
  (make-problem-config 100 50 3.0 0.016667 4))

;;; ============================================================
;;; Part 2: Entity Creation Helpers
;;; ============================================================

(doc 'section 'entity-helpers)

;;; make-ball : Symbol × Vec2 × Number × Number × Vec2 → Entity
;;; Create a ball entity with given properties
(define (make-ball id pos radius mass vel)
  (doc 'export #t)
  (let ([body (make-body-2d pos vel mass)]
        [shape (make-circle (vec2 0 0) radius)]
        [material (make-material 0.8 0.3 0.2)])  ; restitution, static-friction, dynamic-friction
    (make-entity id body shape material '())))

;;; make-ball-static : Symbol × Vec2 × Number → Entity
;;; Create a static ball (infinite mass, no velocity)
(define (make-ball-static id pos radius)
  (doc 'export #t)
  (let ([body (make-static-body pos)]
        [shape (make-circle (vec2 0 0) radius)]
        [material (make-material 0.8 0.3 0.2)])
    (make-entity id body shape material '())))

;;; make-box : Symbol × Vec2 × Number × Number × Number × Vec2 → Entity
;;; Create a box entity
(define (make-box id pos width height mass vel)
  (doc 'export #t)
  (let* ([hw (/ width 2)]
         [hh (/ height 2)]
         [body (make-body-2d pos vel mass)]
         [shape (make-aabb (vec2 (- hw) (- hh)) (vec2 hw hh))]
         [material (make-material 0.6 0.4 0.3)])
    (make-entity id body shape material '())))

;;; make-ground : Number × Number → Entity
;;; Create a static ground plane
(define (make-ground y width)
  (doc 'export #t)
  (let ([body (make-static-body (vec2 0 y))]
        [shape (make-aabb (vec2 (- width) -1) (vec2 width 0))]
        [material (make-material 0.5 0.5 0.3)])
    (make-entity 'ground body shape material '())))

;;; ============================================================
;;; Part 3: World Setup
;;; ============================================================

(doc 'section 'world-setup)

;;; setup-problem-world : (List Entity) × Vec2 → World
;;; Create a physics world with given entities and gravity
(define (setup-problem-world entities gravity)
  (doc 'export #t)
  (let ([world (make-world gravity)])
    (for-each (lambda (e)
                (world-add-entity! world e))
              entities)
    world))

;;; setup-no-gravity-world : (List Entity) → World
;;; Create a world with no gravity (space/frictionless surface)
(define (setup-no-gravity-world entities)
  (doc 'export #t)
  (setup-problem-world entities (vec2 0 0)))

;;; setup-gravity-world : (List Entity) → World
;;; Create a world with standard gravity (-9.8 m/s²)
(define (setup-gravity-world entities)
  (doc 'export #t)
  (setup-problem-world entities (vec2 0 -9.8)))

;;; ============================================================
;;; Part 4: Simulation
;;; ============================================================

(doc 'section 'simulation)

;;; simulate-step : World × Number → World
;;; Advance simulation by one timestep
(define (simulate-step world dt)
  (doc 'export #t)
  (world-step! world dt))

;;; simulate-n-steps : World × Number × Nat → World
;;; Advance simulation by n timesteps
(define (simulate-n-steps world dt n)
  (doc 'export #t)
  (if (<= n 0)
      world
      (simulate-n-steps (simulate-step world dt) dt (- n 1))))

;;; simulate-frames : World × ProblemConfig × Nat → (List World)
;;; Run simulation and capture world state at each frame
(define (simulate-frames world config n-frames)
  (doc 'export #t)
  (let ([dt (problem-config-dt config)]
        [steps-per-frame (problem-config-frames-per-step config)])
    (let loop ([w world] [frames '()] [remaining n-frames])
      (if (<= remaining 0)
          (reverse frames)
          (let ([next-w (simulate-n-steps w dt steps-per-frame)])
            (loop next-w (cons w frames) (- remaining 1)))))))

;;; ============================================================
;;; Part 5: Rendering
;;; ============================================================

(doc 'section 'rendering)

;;; make-problem-renderer : ProblemConfig → WorldRenderer
;;; Create renderer from problem config
(define (make-problem-renderer config)
  (doc 'export #t)
  (let ([render-config (make-render-config
                        (problem-config-width config)
                        (problem-config-height config)
                        (problem-config-scale config)
                        (quotient (problem-config-width config) 2)  ; center origin
                        (- (problem-config-height config) 5))])     ; origin near bottom
    (make-world-renderer render-config
                         (default-debug-options)
                         (default-render-style))))

;;; render-world-frame : World × WorldRenderer → String
;;; Render world state to ASCII string
(define (render-world-frame world renderer)
  (doc 'export #t)
  (render-world world renderer))

;;; render-frames : (List World) × WorldRenderer → (List String)
;;; Render all frames to ASCII strings
(define (render-frames worlds renderer)
  (doc 'export #t)
  (map (lambda (w) (render-world-frame w renderer)) worlds))

;;; capture-frames : World × ProblemConfig × Nat → (List String)
;;; Convenience: simulate and render in one call
(define (capture-frames world config n-frames)
  (doc 'export #t)
  (let* ([renderer (make-problem-renderer config)]
         [worlds (simulate-frames world config n-frames)])
    (render-frames worlds renderer)))

;;; ============================================================
;;; Part 6: State Extraction (for Answer Computation)
;;; ============================================================

(doc 'section 'state-extraction)

;;; get-entity-position : World × Symbol → Vec2
;;; Get position of named entity
(define (get-entity-position world id)
  (doc 'export #t)
  (let ([entity (world-get-entity world id)])
    (if entity
        (entity-pos entity)
        (error 'get-entity-position "Entity not found" id))))

;;; get-entity-velocity : World × Symbol → Vec2
;;; Get velocity of named entity
(define (get-entity-velocity world id)
  (doc 'export #t)
  (let ([entity (world-get-entity world id)])
    (if entity
        (entity-vel entity)
        (error 'get-entity-velocity "Entity not found" id))))

;;; get-entity-radius : World × Symbol → Number
;;; Get radius of circular entity
(define (get-entity-radius world id)
  (doc 'export #t)
  (let ([entity (world-get-entity world id)])
    (if entity
        (let ([shape (entity-shape entity)])
          (if (circle? shape)
              (circle-radius shape)
              (error 'get-entity-radius "Entity is not a circle" id)))
        (error 'get-entity-radius "Entity not found" id))))

;;; compute-distance : World × Symbol × Symbol → Number
;;; Compute distance between two entities
(define (compute-distance world id1 id2)
  (doc 'export #t)
  (let ([pos1 (get-entity-position world id1)]
        [pos2 (get-entity-position world id2)])
    (vec2-magnitude (vec2-sub pos2 pos1))))

;;; compute-relative-velocity : World × Symbol × Symbol → Vec2
;;; Compute velocity of id1 relative to id2
(define (compute-relative-velocity world id1 id2)
  (doc 'export #t)
  (let ([vel1 (get-entity-velocity world id1)]
        [vel2 (get-entity-velocity world id2)])
    (vec2-sub vel1 vel2)))

;;; compute-speed : World × Symbol → Number
;;; Compute speed (magnitude of velocity) of entity
(define (compute-speed world id)
  (doc 'export #t)
  (vec2-magnitude (get-entity-velocity world id)))

;;; ============================================================
;;; Part 7: Collision Detection Helpers
;;; ============================================================

(doc 'section 'collision-helpers)

;;; time-to-collision : World × Symbol × Symbol → Number | #f
;;; Estimate time until two entities collide (linear approximation)
;;; Returns #f if they won't collide (moving apart or parallel)
(define (time-to-collision world id1 id2)
  (doc 'export #t)
  (let* ([pos1 (get-entity-position world id1)]
         [pos2 (get-entity-position world id2)]
         [vel1 (get-entity-velocity world id1)]
         [vel2 (get-entity-velocity world id2)]
         [r1 (get-entity-radius world id1)]
         [r2 (get-entity-radius world id2)]
         [rel-pos (vec2-sub pos2 pos1)]
         [rel-vel (vec2-sub vel2 vel1)]
         [dist (vec2-magnitude rel-pos)]
         [closing-speed (- (vec2-dot rel-vel (vec2-normalize rel-pos)))])
    (if (<= closing-speed 0)
        #f  ; Moving apart
        (/ (- dist r1 r2) closing-speed))))

;;; will-collide? : World × Symbol × Symbol × Number → Boolean
;;; Check if two entities will collide within time limit
(define (will-collide? world id1 id2 max-time)
  (doc 'export #t)
  (let ([ttc (time-to-collision world id1 id2)])
    (and ttc (< ttc max-time) (> ttc 0))))
