;;; skills/physics/classical.sexp — Skill curation file

(skill physics/classical
  (description
    "Classical 2D rigid body physics simulation with collision detection,\n    constraint solving, and particle systems. Provides a complete physics\n    world with spatial hashing, materials, and ASCII rendering.")

  (keywords (physics simulation rigid-body 2d collision particles constraints world spatial-hash ascii-render graph-coloring constraint-islands scc parallel-solving))

  (aliases (physics-2d classical-physics))

  (concepts (rigid-body-physics))

  (modules
    ode-integrators
    integrators
    rigid-body
    collision-detection
    collision-response
    constraints
    constraint-solver
    constraint-graph
    world
    particles
    raycasting
    ascii-renderer
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
