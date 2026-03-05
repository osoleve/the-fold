;;; skills/physics/lenses.sexp — Skill curation file

(skill physics/lenses
  (description
    "Optics (lenses) for functional access to 2D and 3D physics state. Enables elegant\n    composition of getters and setters for nested physics structures like\n    rigid bodies and particles. Supports dot notation for concise deep access.\n    Includes rotational dynamics protocols (angle, angular-vel, inertia) for\n    generic access across body types. 3D support includes quaternion lenses,\n    rigid body 3D lenses, and traversals for body collections. Integration with\n    the optics tower for traversals, affines, and world-level composition.")

  (keywords (lens optics physics functional getter setter composition rigid-body particle vec2 vec3 quaternion traversal affine world rotation angular-velocity inertia protocol 3d rigid-body-3d))

  (aliases (physics-lens physics-optics physics-lenses-3d))

  (concepts (physics-optics))

  (modules
    lenses
    lenses3d
    optics-integration
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
