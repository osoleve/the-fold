;;; lattice/physics/lenses/manifest.sexp — Physics Lenses Skill Manifest

(skill physics/lenses
  (version "0.3.0")
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
    Includes rotational dynamics protocols (angle, angular-vel, inertia) for
    generic access across body types. Integration with the optics tower for
    traversals, affines, and world-level composition.")

  (keywords (lens optics physics functional getter setter composition
             rigid-body particle vec2 traversal affine world rotation
             angular-velocity inertia protocol))
  (aliases (physics-lens physics-optics))

  (exports
   ;; Generic body lenses (work with any body type)
   (lenses body-pos-lens body-vel-lens body-mass-lens
           body-angle-lens body-angular-vel-lens body-inertia-lens)
   ;; Protocol bundles
   (lenses body-ops rotational-body-ops)
   ;; Protocols (for implementing new body types)
   (lenses body-pos body-set-pos body-vel body-set-vel body-mass body-set-mass
           body-angle body-set-angle body-angular-vel body-set-angular-vel
           body-inertia body-set-inertia)
   ;; Lens-based physics helpers (explicit lens arguments)
   (lenses apply-force-via-lens integrate-position-via-lens
           apply-torque-via-lens integrate-rotation-via-lens
           integrate-body-via-lens apply-impulse-at-point-via-lens)
   ;; Generic helpers (auto-detect rotation support)
   (lenses rotates? apply-torque integrate-body apply-impulse-at-point)
   ;; Aliases
   (lenses position-lens velocity-lens mass-lens rotation-lens
           angular-velocity-lens moment-of-inertia-lens))

  (modules
   (lenses "lenses.ss" "Physics lenses for rigid bodies, particles, and vec2")
   (optics-integration "optics-integration.ss" "Integration with optics tower")))
