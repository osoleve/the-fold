;;; lattice/physics/classical/README.sexp --- 2D Classical Physics Module
;;;
;;; Module documentation in S-expression format.

(module physics-classical
  (description "2D rigid body physics with collision detection and response")

  (purpose
    "Simulate realistic 2D physics for games and simulations:"
    "- Rigid body dynamics (linear + angular motion)"
    "- Collision detection (AABB, circle, polygon)"
    "- Impulse-based collision response with friction"
    "- Joint constraints (distance, revolute, spring, weld)"
    "- Ray casting for line-of-sight and projectiles")

  (files
    (integrators.ss "Numerical integration: Euler, Verlet, RK4"
      (key-functions
        "make-body-2d, body-pos, body-vel, body-mass"
        "integrate-body-euler, integrate-body-verlet"
        "integrate-body-symplectic, integrate-body-rk4"
        "gravity-force, spring-force, drag-force, combine-forces"))

    (rigid-body.ss "Full 2D rigid body with rotation (312 lines)"
      (key-functions
        "make-rigid-body, rigid-body-pos, rigid-body-vel"
        "rigid-body-angle, rigid-body-angular-vel"
        "circle-inertia, rectangle-inertia, rod-inertia"
        "local-to-world, world-to-local"
        "apply-impulse-at-point, rigid-body-kinetic-energy"))

    (collision-detection.ss "Shape primitives and collision tests (777 lines)"
      (key-functions
        "make-aabb, make-circle, make-polygon, make-box"
        "point-in-aabb?, point-in-circle?, point-in-polygon?"
        "aabb-manifold, circle-manifold, polygon-manifold"
        "make-spatial-hash, spatial-hash-query"))

    (collision-response.ss "Impulse-based collision resolution (576 lines)"
      (key-functions
        "make-material, rubber-material, metal-material"
        "calculate-impulse, calculate-friction-impulse"
        "resolve-collision, resolve-with-rotation-and-correction"))

    (world.ss "Physics world coordinator (676 lines)"
      (key-functions
        "make-world, world-add-entity!, world-remove-entity!"
        "world-step!, world-fixed-step!"
        "make-circle-entity, make-box-entity, make-ground"
        "world-query-aabb, world-raycast-closest"))

    (constraints.ss "Joint constraint types (8.9 KB)"
      (key-functions
        "make-distance-constraint, make-revolute-constraint"
        "make-spring-constraint, make-weld-constraint"))

    (constraint-solver.ss "Sequential impulse constraint solver (32 KB)"
      (key-functions
        "solve-constraint-velocity, solve-constraint-position"))

    (raycasting.ss "Ray-shape intersection tests (12.8 KB)"
      (key-functions
        "make-ray2, ray-aabb-intersect, ray-circle-intersect"
        "ray-polygon-intersect"))

    (ascii-renderer.ss "Text-based debug visualization (22.6 KB)")

    (particles.ss "Particle emitter system"
      (key-functions
        "make-particle, make-emitter, make-particle-system"
        "gravity-field, wind-field, attractor-field, drag-field"
        "particle-system-step, explosion-burst")))

  (data-structures
    "Body2D = {pos: Vec2, vel: Vec2, mass: Number, inv-mass: Number}"
    ""
    "RigidBody2D = {pos: Vec2, vel: Vec2, angle: Number, angular-vel: Number,"
    "               mass: Number, inv-mass: Number, inertia: Number, inv-inertia: Number}"
    ""
    "Shape = AABB | Circle | Polygon"
    "  AABB = {min: Vec2, max: Vec2}"
    "  Circle = {center: Vec2, radius: Number}"
    "  Polygon = {vertices: [Vec2], edges: [(Vec2, Vec2)], normals: [Vec2]}"
    ""
    "Material = {restitution: [0,1], static-friction: Number, dynamic-friction: Number}"
    ""
    "Entity = {id: Any, body: Body2D|RigidBody2D, shape: Shape,"
    "          material: Material, user-data: Any}"
    ""
    "Manifold = {body-a: Body, body-b: Body, normal: Vec2,"
    "            penetration: Number, contact: Vec2}")

  (usage-examples
    ";;; Basic simulation with gravity"
    "(define world (make-world (vec2 0 9.8)))"
    "(world-add-entity! world (make-circle-entity 'ball (vec2 100 50) 20 1.0))"
    "(world-add-entity! world (make-ground 'floor (vec2 320 450) 600 20))"
    "(world-step! world 0.016)"
    ""
    ";;; Custom force function"
    "(define wind+gravity (combine-forces (gravity-force (vec2 0 9.8))"
    "                                      (wind-field (vec2 1 0) 2.0)))"
    ""
    ";;; Ray casting"
    "(define hit (world-raycast-closest world origin direction max-dist))"
    "(when hit"
    "  (display (format \"Hit entity ~a at ~a\\n\" (hit-entity hit) (hit-point hit))))"
    ""
    ";;; Constraints"
    "(world-add-constraint! world"
    "  (make-distance-constraint 'chain entity-a entity-b 50))")

  (numerical-integration
    "Available methods:"
    "- Euler (simple, first-order, can be unstable)"
    "- Symplectic Euler (more stable, preserves energy better)"
    "- Velocity Verlet (second-order, good for physics)"
    "- RK4 (fourth-order, most accurate, more computation)")

  (collision-detection-flow
    "1. Broad Phase: Spatial hash culls distant pairs (O(1) lookup)"
    "2. Narrow Phase: Per-shape collision tests"
    "3. Contact Generation: Manifold with normal, depth, contact point"
    "4. Resolution: Impulse calculation and application")

  (dependencies
    (vec2.ss "2D vector math")
    (prelude.ss "Scheme prelude"))

  (tests
    (test-physics.ss "Full integration tests")
    (test-integrators.ss "Integration method tests")
    (test-collision-detection.ss "Shape and manifold tests")
    (test-collision-response.ss "Impulse calculation tests")
    (test-world.ss "World stepping tests")
    (test-constraints.ss "Constraint solver tests")
    (test-raycasting.ss "Ray intersection tests")
    (test-particles.ss "Particle system tests"))

  (tier 2)
  (test-command "scheme --script lattice/physics/classical/test-physics.ss"))
