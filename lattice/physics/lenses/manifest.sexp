;;; lattice/physics/lenses/manifest.sexp — Physics Lenses Skill Manifest

(skill physics/lenses
  (version "0.2.0")
  (tier 1)
  (path "lattice/physics/lenses")
  (purity total)
  (stability stable)
  (fuel-bound "O(1) for all lens operations, O(n) for traversals")
  (deps (fp fp/optics physics/classical))

  (description
   "Optics (lenses) for functional access to physics state. Enables elegant
    composition of getters and setters for nested physics structures like
    rigid bodies and particles. Supports dot notation for concise deep access.
    Includes integration with the optics tower for traversals, affines, and
    world-level composition.")

  (keywords (lens optics physics functional getter setter composition
             rigid-body particle vec2 traversal affine world))
  (aliases (physics-lens physics-optics))

  (exports
   ;; No exports annotated with (doc 'export #t) yet
   )

  (modules
   (lenses "lenses.ss" "Physics lenses for rigid bodies, particles, and vec2")
   (optics-integration "optics-integration.ss" "Integration with optics tower")))
