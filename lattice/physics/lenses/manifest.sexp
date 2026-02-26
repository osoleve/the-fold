;;; lattice/physics/lenses/manifest.sexp — Physics Lenses Skill Manifest

(skill physics/lenses
  (version "0.4.0")
  (path "lattice/physics/lenses")
  (purity total)
  (stability stable)
  (fuel-bound "O(1) for all lens operations, O(n) for traversals")
  (deps (fp optics geometry physics/classical physics/classical3d))

  (description
   "Optics (lenses) for functional access to 2D and 3D physics state. Enables elegant
    composition of getters and setters for nested physics structures like
    rigid bodies and particles. Supports dot notation for concise deep access.
    Includes rotational dynamics protocols (angle, angular-vel, inertia) for
    generic access across body types. 3D support includes quaternion lenses,
    rigid body 3D lenses, and traversals for body collections. Integration with
    the optics tower for traversals, affines, and world-level composition.")

  (keywords (lens optics physics functional getter setter composition
             rigid-body particle vec2 vec3 quaternion traversal affine world rotation
             angular-velocity inertia protocol 3d rigid-body-3d))
  (aliases (physics-lens physics-optics physics-lenses-3d))

  (concepts
    (concept physics-optics
      (description "Optics (lenses, traversals, affines) for functional access to 2D and 3D physics state, enabling composable getters and setters over nested body structures.")
      (parent physics-simulation)
      (synonyms physics-lens physics-lenses-3d)))

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
           angular-velocity-lens moment-of-inertia-lens)
   ;; 3D quaternion lenses
   (lenses3d quat-w-lens quat-x-lens quat-y-lens quat-z-lens)
   ;; 3D rigid body lenses
   (lenses3d rigid-body-3d-pos-lens rigid-body-3d-vel-lens
             rigid-body-3d-orientation-lens rigid-body-3d-angular-vel-lens
             rigid-body-3d-mass-lens rigid-body-3d-inertia-lens
             rigid-body-3d-inv-mass-getter rigid-body-3d-inv-inertia-getter)
   ;; 3D composed lenses
   (lenses3d rigid-body-3d-pos-x-lens rigid-body-3d-pos-y-lens rigid-body-3d-pos-z-lens
             rigid-body-3d-vel-x-lens rigid-body-3d-vel-y-lens rigid-body-3d-vel-z-lens
             rigid-body-3d-angular-vel-x-lens rigid-body-3d-angular-vel-y-lens
             rigid-body-3d-angular-vel-z-lens
             rigid-body-3d-orientation-w-lens rigid-body-3d-orientation-x-lens
             rigid-body-3d-orientation-y-lens rigid-body-3d-orientation-z-lens)
   ;; 3D dot notation macro
   (lenses3d body3d.)
   ;; 3D traversals
   (lenses3d bodies-3d-each bodies-3d-filtered dynamic-bodies-3d static-bodies-3d)
   ;; 3D convenience functions
   (lenses3d translate-body-3d apply-central-impulse-3d apply-gravity-3d
             step-body-3d step-bodies-3d)
   ;; 3D folds
   (lenses3d total-mass-3d center-of-mass-3d total-kinetic-energy))

  (modules
   (lenses "lenses.ss" "Physics lenses for rigid bodies, particles, and vec2")
   (lenses3d "lenses3d.ss" "Physics lenses for 3D rigid bodies, quaternions, and vec3")
   (optics-integration "optics-integration.ss" "Integration with optics tower")))
