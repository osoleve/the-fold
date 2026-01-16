(skill diffgeo
  (version "0.3.0")
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
    bundles. Includes standard coordinate systems (polar, spherical, cylindrical),
    Lie groups (SO(2), SO(3), SE(2), SE(3)) with exponential/logarithm maps,
    adjoint representations, and Baker-Campbell-Hausdorff approximations.")

  (keywords (differential-geometry manifold chart atlas coordinates
             transition-function jacobian polar spherical cylindrical
             tangent-vector cotangent-vector tangent-space cotangent-space
             tangent-bundle cotangent-bundle pushforward pullback differential
             lie-group lie-algebra so2 so3 se2 se3 rotation rigid-transformation
             exponential-map rodrigues adjoint bch interpolation slerp))
  (aliases (diffgeom manifolds smooth-manifolds lie-groups))

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
    ;; Lie bracket
    lie-bracket lie-bracket-field)

   (lie-groups
    ;; Constants/utilities
    identity-matrix matrix-set! matrix-trace skew-symmetric?
    ;; SO(2) - 2D rotations
    so2? so2-matrix make-so2 so2-identity so2-angle
    so2-compose so2-inverse so2-act
    so2-alg? so2-alg-omega make-so2-alg so2-alg-to-matrix so2-matrix-to-alg
    so2-exp so2-log
    ;; SO(3) - 3D rotations
    so3? so3-matrix make-so3-from-matrix make-so3 so3-identity
    so3-compose so3-inverse so3-act
    so3-alg? so3-alg-vec make-so3-alg so3-hat so3-vee
    so3-alg-to-matrix so3-matrix-to-alg so3-exp so3-log so3-axis-angle
    ;; SE(2) - 2D rigid transformations
    se2? se2-rotation se2-translation make-se2 make-se2-from-angle
    se2-identity se2-to-matrix matrix-to-se2
    se2-compose se2-inverse se2-act
    se2-alg? se2-alg-omega se2-alg-v make-se2-alg se2-alg-to-matrix
    se2-exp se2-log
    ;; SE(3) - 3D rigid transformations
    se3? se3-rotation se3-translation make-se3 se3-identity
    se3-to-matrix matrix-to-se3 se3-compose se3-inverse se3-act
    se3-alg? se3-alg-omega se3-alg-v make-se3-alg se3-alg-to-matrix
    se3-exp se3-log
    ;; Adjoint representation
    so3-adjoint se3-adjoint vec-cross
    ;; Baker-Campbell-Hausdorff
    so3-bracket so3-bch-2 so3-bch-4
    ;; Interpolation
    so3-interpolate se3-interpolate))

  (modules
   (charts "charts.ss"
    "Coordinate charts, atlases, transition functions, and Jacobian computation.
     Foundation for smooth manifold representation.")
   (tangent "tangent.ss"
    "Tangent and cotangent spaces, vectors, pushforward/pullback operations,
     and bundle constructions. Foundation for vector fields and differential forms.")
   (lie-groups "lie-groups.ss"
    "Lie groups (SO(2), SO(3), SE(2), SE(3)) and their Lie algebras. Provides
     exponential/logarithm maps via Rodrigues formula, adjoint representations,
     Baker-Campbell-Hausdorff approximations, and SLERP interpolation.")))
