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
   (syntax body.)
   ;; Optics integration exports
   (composition >>>)
   (traversals bodies-each particles-each particles-alive
               bodies-filtered rigid-bodies-only particles-only)
   (affines affine-rigid-angle affine-rigid-angular-vel affine-rigid-inertia
            affine-particle-lifetime affine-particle-size affine-particle-color)
   (world make-world world? world-bodies world-with-bodies
          world-body world-bodies-lens world-all-bodies)
   (convenience body-view body-modify body-set body-preview
                apply-gravity apply-impulse move-by)
   (folds fold-bodies total-mass center-of-mass bodies-in-region)
   (physics-steps step-body step-bodies step-world apply-forces-to-world))

  (modules
   (lenses "lenses.ss" "Physics lenses for rigid bodies, particles, and vec2")
   (optics-integration "optics-integration.ss" "Integration with optics tower")))
