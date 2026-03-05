;;; skills/physics/classical3d.sexp — Skill curation file

(skill physics/classical3d
  (description
    "Classical 3D rigid body physics simulation with quaternion-based rotation,\n    collision detection for spheres and boxes, constraint solving with multiple\n    joint types, and a complete physics world with spatial hashing.")

  (keywords (physics simulation rigid-body 3d collision quaternion constraints world spatial-hash inertia-tensor))

  (aliases (physics-3d classical-physics-3d))

  (concepts (rigid-body-physics-3d))

  (modules
    rigid-body3d
    shapes3d
    collision-detection3d
    collision-protocol3d
    constraints3d
    constraint-solver3d
    world3d
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
