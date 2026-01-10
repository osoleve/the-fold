;;; lattice/physics/classical3d/README.sexp --- 3D Classical Physics Module
;;;
;;; Module documentation in S-expression format.

(module physics-classical3d
  (description "3D rigid body physics extending the 2D classical physics patterns")

  (purpose
    "Simulate realistic 3D physics for games and simulations:"
    "- 3D rigid body dynamics with quaternion rotation"
    "- 3D collision detection (sphere, box, convex hull)"
    "- Impulse-based 3D collision response"
    "- 3D joint constraints")

  (files
    (rigid-body3d.ss "3D rigid body with quaternion rotation"
      (key-functions
        "make-rigid-body3d, rigid-body3d-pos, rigid-body3d-vel"
        "rigid-body3d-orientation, rigid-body3d-angular-vel"
        "rigid-body3d-inertia-tensor, rigid-body3d-inv-inertia"
        "sphere-inertia-tensor, box-inertia-tensor"
        "local-to-world3d, world-to-local3d"))

    (world3d.ss "3D physics world coordinator"
      (key-functions
        "make-world3d, world3d-add-entity!, world3d-step!"
        "make-sphere-entity, make-box-entity3d"))

    (collision-detection3d.ss "3D shape primitives and collision tests"
      (key-functions
        "make-sphere3d, make-aabb3d, make-convex-hull"
        "sphere-sphere-collision, sphere-box-collision"
        "gjk-collision, epa-contact"))

    (shapes3d.ss "3D shape primitives"
      (key-functions
        "make-sphere3d, make-box3d, make-plane3d"))

    (constraints3d.ss "3D joint constraints"
      (key-functions
        "make-ball-socket-constraint, make-hinge-constraint"))

    (constraint-solver3d.ss "3D constraint solving"))

  (data-structures
    "RigidBody3D = {pos: Vec3, vel: Vec3,"
    "               orientation: Quaternion, angular-vel: Vec3,"
    "               mass: Number, inv-mass: Number,"
    "               inertia: Mat3, inv-inertia: Mat3}"
    ""
    "Shape3D = Sphere3D | AABB3D | ConvexHull"
    "  Sphere3D = {center: Vec3, radius: Number}"
    "  AABB3D = {min: Vec3, max: Vec3}"
    "  ConvexHull = {vertices: [Vec3], faces: [[Index]]}")

  (key-differences-from-2d
    "- Rotation uses quaternions instead of single angle"
    "- Angular velocity is Vec3 instead of scalar"
    "- Inertia tensor is 3x3 matrix instead of scalar"
    "- GJK/EPA for convex hull collision detection"
    "- More complex constraint formulations")

  (usage-example
    ";;; Create a 3D physics world"
    "(define world (make-world3d (vec3 0 -9.8 0)))"
    ""
    ";;; Add a falling sphere"
    "(world3d-add-entity! world"
    "  (make-sphere-entity 'ball (vec3 0 10 0) 1.0 1.0))"
    ""
    ";;; Add a ground plane"
    "(world3d-add-entity! world"
    "  (make-plane-entity 'floor (vec3 0 0 0) (vec3 0 1 0)))"
    ""
    ";;; Step the simulation"
    "(world3d-step! world 0.016)")

  (dependencies
    (vec3.ss "3D vector math")
    (mat3.ss "3x3 matrices for inertia tensors")
    (quaternion.ss "Quaternion math for rotation"))

  (tier 2)
  (test-command "scheme --script lattice/physics/classical3d/test-physics3d.ss"))
