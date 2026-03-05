;;; lattice/physics/classical3d/manifest.sexp — Classical 3D Physics Skill Manifest

(skill physics/classical3d
  (version "0.1.0")
  (path "lattice/physics/classical3d")
  (purity partial)  ; World simulation uses mutation
  (stability experimental)
  (fuel-bound "O(n^2 x iterations) for n entities")
  (deps (linalg))

  (description
   "Classical 3D rigid body physics simulation with quaternion-based rotation,
    collision detection for spheres and boxes, constraint solving with multiple
    joint types, and a complete physics world with spatial hashing.")

  (keywords (physics simulation rigid-body 3d collision quaternion
             constraints world spatial-hash inertia-tensor))
  (aliases (physics-3d classical-physics-3d))

  (concepts
    (concept rigid-body-physics-3d
      (description "3D rigid body simulation with quaternion rotation, inertia tensors, collision detection for spheres and boxes, and constraint solving.")
      (parent physics-simulation)
      (synonyms physics-3d classical-physics-3d physics/classical3d)))

  (exports
   ;; No exports annotated with (doc 'export #t) yet
   )

  (modules
   (rigid-body3d "rigid-body3d.ss" "3D rigid body with quaternion orientation and inertia tensors")
   (shapes3d "shapes3d.ss" "3D collision shapes: sphere, box, AABB")
   (collision-detection3d "collision-detection3d.ss" "3D collision detection with spatial hashing")
   (collision-protocol3d "collision-protocol3d.ss" "Extensible 3D collision response protocols")
   (constraints3d "constraints3d.ss" "3D constraint data structures for joints")
   (constraint-solver3d "constraint-solver3d.ss" "Iterative 3D constraint solver")
   (world3d "world3d.ss" "3D physics world: entities, simulation stepping, queries")))
