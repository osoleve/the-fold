;;; lattice/physics/README.sexp --- Physics Engine Module Overview
;;;
;;; Module documentation in S-expression format.

(module physics
  (description "Physics simulation library for 2D and 3D rigid body dynamics")

  (purpose
    "Provide production-grade physics simulation for:"
    "- Games and interactive applications"
    "- Scientific simulations and visualizations"
    "- Trajectory optimization and inverse physics"
    "- Educational demonstrations")

  (subdirectories
    (lenses "Functional optics for physics state access"
      (features
        "Lens-based getters and setters for bodies and particles"
        "Composable lenses for deep nested access"
        "Generic body-pos-lens works with any body type"
        "Convenient (body. pos x) dot notation macro"
        "Integration utilities for lens-based physics updates"
        "Satisfies lens laws (Get-Put, Put-Get, Put-Put)"))

    (classical "2D rigid body physics with collision detection and response"
      (features
        "Multiple integration methods (Euler, Verlet, RK4)"
        "Shape primitives: AABB, Circle, Polygon"
        "SAT-based collision detection"
        "Impulse-based collision response with friction"
        "Joint constraints (distance, revolute, spring, weld)"
        "Spatial hashing for broad-phase optimization"
        "Ray casting queries"))

    (classical3d "3D rigid body physics extending 2D patterns"
      (features
        "Quaternion-based rotation"
        "3D collision shapes: Sphere, Box, Convex Hull"
        "3D constraint system"
        "Inertia tensor support"))

    (diff "Differentiable 2D physics for gradient-based optimization"
      (features
        "Automatic differentiation through physics"
        "Soft contact models for smooth gradients"
        "Trajectory optimization"
        "Parameter inference"))

    (diff3d "Differentiable 3D physics extending diff patterns"
      (features
        "3D traced vectors and quaternions"
        "Differentiable 3D integration"
        "3D trajectory optimization")))

  (architecture
    "The physics engine follows a layered architecture:"
    ""
    "1. Data Structures (Vec2/Vec3, RigidBody, Shape, Material)"
    "2. Integration (Euler, Verlet, RK4 numerical methods)"
    "3. Collision Detection (broad phase + narrow phase)"
    "4. Collision Response (impulse calculation + resolution)"
    "5. Constraints (joint types + sequential impulse solver)"
    "6. World Coordination (entity management, stepping)")

  (key-design-decisions
    (immutable-bodies "All body updates return new objects for purity")
    (manifold-based "Collision info decoupled from resolution")
    (fixed-timestep "Time accumulator prevents physics drift")
    (iterative-solver "Multiple passes for constraint stability")
    (spatial-hashing "O(1) collision pair finding"))

  (quick-start
    ";;; Load the 2D classical physics module"
    "(load \"lattice/physics/classical/world.ss\")"
    ""
    ";;; Create a physics world with gravity"
    "(define world (make-world (vec2 0 9.8)))"
    ""
    ";;; Add a bouncing ball"
    "(world-add-entity! world"
    "  (make-circle-entity 'ball (vec2 100 50) 20 1.0))"
    ""
    ";;; Add a ground plane"
    "(world-add-entity! world"
    "  (make-ground 'floor (vec2 320 450) 600 20))"
    ""
    ";;; Step the simulation (60 FPS)"
    "(world-step! world (/ 1 60))")

  (tier 2)
  (dependencies
    (linalg/vec2.ss "2D vector math")
    (linalg/vec3.ss "3D vector math")
    (linalg/mat3.ss "3x3 matrices for inertia")
    (linalg/quaternion.ss "Quaternion math for 3D rotation")
    (autodiff/reverse-diff.ss "For differentiable physics")
    (fp/templates.ss "Lens infrastructure for physics/lenses"))

  (test-command "scheme --script lattice/physics/classical/test-physics.ss")

  (author "Shepherd (Opus)")
  (tier "lattice:advanced"))
