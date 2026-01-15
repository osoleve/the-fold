(skill diffgeo
  (version "0.2.0")
  (tier 1)
  (path "lattice/diffgeo")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n²) for Jacobian computation, O(k) for atlas operations with k charts")
  (deps (linalg))

  (description
   "Differential geometry foundations for smooth manifolds. Provides coordinate
    charts, atlases, transition functions, Jacobian computation, tangent and
    cotangent spaces, pushforward and pullback operations, and tangent/cotangent
    bundles. Includes standard coordinate systems (polar, spherical, cylindrical)
    and tools for manifold representation.")

  (keywords (differential-geometry manifold chart atlas coordinates
             transition-function jacobian polar spherical cylindrical
             tangent-vector cotangent-vector tangent-space cotangent-space
             tangent-bundle cotangent-bundle pushforward pullback differential))
  (aliases (diffgeom manifolds smooth-manifolds))

  (exports
   (charts
    chart? chart-name chart-dim chart-domain-pred chart-coord-map chart-inverse-map
    make-chart chart-contains? chart-apply chart-apply-inverse
    make-transition transition-apply
    jacobian-numerical transition-jacobian jacobian-determinant
    atlas? atlas-name atlas-charts atlas-dim atlas-chart-count
    make-atlas atlas-add-chart atlas-find-chart atlas-find-chart-by-name atlas-coords
    charts-overlap? transition-smooth?
    make-identity-chart make-polar-chart make-cartesian-chart
    make-spherical-chart make-cylindrical-chart)

   (tangent
    ;; Tangent vectors
    tangent-vector? tangent-vector-point tangent-vector-chart tangent-vector-components
    tangent-vector-dim make-tangent-vector tangent-zero tangent-basis-vector
    tangent-add tangent-sub tangent-scale tangent-negate tangent-change-chart
    ;; Cotangent vectors
    cotangent-vector? cotangent-vector-point cotangent-vector-chart cotangent-vector-components
    cotangent-vector-dim make-cotangent-vector cotangent-zero cotangent-basis-form
    cotangent-add cotangent-sub cotangent-scale cotangent-negate cotangent-change-chart
    ;; Pairing
    covector-apply
    ;; Pushforward/pullback
    pushforward pullback-at
    ;; Tangent space
    tangent-space? tangent-space-point tangent-space-chart tangent-space-dim
    make-tangent-space tangent-space-basis tangent-space-vector
    ;; Cotangent space
    cotangent-space? cotangent-space-point cotangent-space-chart cotangent-space-dim
    make-cotangent-space cotangent-space-basis cotangent-space-form
    ;; Tangent bundle
    tangent-bundle? tangent-bundle-atlas tangent-bundle-dim
    make-tangent-bundle tangent-bundle-fiber tangent-bundle-section
    ;; Cotangent bundle
    cotangent-bundle? cotangent-bundle-atlas cotangent-bundle-dim
    make-cotangent-bundle cotangent-bundle-fiber cotangent-bundle-section
    ;; Differential
    differential
    ;; Utilities
    matrix-vec-mul matrix-transpose))

  (modules
   (charts "charts.ss"
    "Coordinate charts, atlases, transition functions, and Jacobian computation.
     Foundation for smooth manifold representation.")
   (tangent "tangent.ss"
    "Tangent and cotangent spaces, vectors, pushforward/pullback operations,
     and bundle constructions. Foundation for vector fields and differential forms.")))
