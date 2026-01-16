;;; lattice/physics/lenses/manifest.sexp — Physics Lenses Skill Manifest

(skill physics/lenses
  (version "0.1.0")
  (tier 1)
  (path "lattice/physics/lenses")
  (purity total)
  (stability stable)
  (fuel-bound "O(1) for all lens operations")
  (deps (fp physics/classical))

  (description
   "Optics (lenses) for functional access to physics state. Enables elegant
    composition of getters and setters for nested physics structures like
    rigid bodies and particles. Supports dot notation for concise deep access.")

  (keywords (lens optics physics functional getter setter composition
             rigid-body particle vec2))
  (aliases (physics-lens physics-optics))

  (exports
   (vec2-lenses vec2-x-lens vec2-y-lens)
   (rigid-body-lenses rigid-body-pos-lens rigid-body-vel-lens
                      rigid-body-angle-lens rigid-body-angular-vel-lens
                      rigid-body-mass-lens rigid-body-inertia-lens
                      rigid-body-pos-x-lens rigid-body-pos-y-lens
                      rigid-body-vel-x-lens rigid-body-vel-y-lens)
   (particle-lenses particle-pos-lens particle-vel-lens
                    particle-lifetime-lens particle-size-lens particle-color-lens
                    particle-pos-x-lens particle-pos-y-lens
                    particle-vel-x-lens particle-vel-y-lens)
   (generic-lenses body-pos-lens body-vel-lens body-pos body-vel
                   position-lens velocity-lens mass-lens rotation-lens)
   (utilities apply-force-via-lens integrate-position-via-lens)
   (syntax body.))

  (modules
   (lenses "lenses.ss" "Physics lenses for rigid bodies, particles, and vec2")))
