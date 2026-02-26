(skill interval
  (version "0.1.0")
  (path "lattice/interval")
  (purity total)
  (stability stable)
  (fuel-bound (linear n))
  (deps (data))

  (description
   "Verified numerical computation with rigorous bounds. Interval arithmetic
    represents sets of real numbers with guaranteed enclosure. Affine arithmetic
    provides tighter bounds via correlation tracking, solving the dependency
    problem where x - x yields [0, 0] instead of [-r, r]. Includes verified
    numerical integration via adaptive interval quadrature.")

  (keywords (interval-arithmetic affine-arithmetic verified-computation bounds
             correlation-tracking dependency-problem noise-symbols
             directed-rounding rigorous-enclosure three-valued-logic
             interval-integration adaptive-quadrature))
  (aliases (interval affine))

  (concepts
    (concept verified-computation
      (description "Numerics with guaranteed error bounds: interval arithmetic, affine arithmetic, and rigorous enclosure methods.")
      (parent numerical-computing)
      (synonyms interval interval-arithmetic rigorous-bounds affine affine-arithmetic correlation-tracking dependency-problem)))

  (exports
   (interval
    ;; Constructors and type
    make-interval interval interval? interval-singleton entire-interval
    ;; Accessors
    interval-lo interval-hi interval-mid interval-width interval-radius
    interval-magnitude interval-mignitude
    ;; Predicates
    interval-empty? interval-singleton? interval-contains? interval-contains-zero?
    interval-positive? interval-negative? interval-subset?
    intervals-overlap? intervals-disjoint?
    ;; Comparisons (three-valued)
    interval-definitely< interval-definitely<= interval-definitely> interval-definitely>=
    interval-possibly< interval-possibly<= interval-possibly> interval-possibly>=
    interval-definitely= interval-possibly=
    ;; Arithmetic (standard - fast, round-to-nearest)
    interval-neg interval-add interval-sub interval-mul interval-sqr
    interval-recip interval-div interval-scale
    ;; Arithmetic (rigorous - directed rounding, guaranteed enclosure)
    interval-add-rigorous interval-sub-rigorous interval-mul-rigorous
    interval-div-rigorous interval-sqrt-rigorous interval-sqr-rigorous
    interval-scale-rigorous
    ;; Directed rounding primitives
    fl-next-up fl-next-down
    add-down add-up sub-down sub-up mul-down mul-up div-down div-up
    sqrt-down sqrt-up
    ;; Elementary functions (standard)
    interval-abs interval-sqrt interval-pow interval-min interval-max
    interval-exp interval-log interval-log10
    interval-sin interval-cos interval-tan
    interval-asin interval-acos interval-atan interval-atan2
    interval-sinh interval-cosh interval-tanh
    ;; Elementary functions (rigorous - directed rounding)
    interval-exp-rigorous interval-log-rigorous interval-log10-rigorous
    interval-sin-rigorous interval-cos-rigorous interval-atan-rigorous
    interval-sinh-rigorous interval-cosh-rigorous interval-tanh-rigorous
    ;; Set operations
    interval-union interval-hull interval-hull-list interval-intersection interval-bisect
    ;; Coercion
    real->interval interval->string interval-print
    ;; Multi-dimensional boxes
    make-box box-dimension box-volume box-contains?
    ;; Critical point detection (for rigorous transcendentals)
    interval-contains-critical? interval-contains-critical-rigorous?
    pi-down pi-up 2pi-down 2pi-up
    ;; Constants
    pi-interval e-interval
    ;; Short aliases
    iv+ iv- iv* iv/)

   (affine
    ;; Noise symbol management
    affine-fresh-noise-id! affine-reset-noise-counter!
    ;; Constructors and type
    make-affine affine? affine-center affine-terms
    affine-constant affine-noise
    ;; Interval conversion
    affine-from-interval affine->interval affine-radius
    ;; Affine operations (correlation-preserving)
    affine-neg affine-add affine-sub affine-scale affine-add-constant
    ;; Non-affine operations
    affine-mul affine-sqr affine-recip affine-div affine-sqrt
    ;; Elementary functions
    affine-exp affine-log
    ;; Min/max
    affine-min affine-max affine-abs
    ;; Predicates
    affine-definitely-positive? affine-definitely-negative? affine-possibly-zero?
    affine-definitely< affine-definitely<= affine-definitely> affine-definitely>=
    ;; Display
    affine->string affine-print
    ;; Short aliases
    af+ af- af* af/ af-neg af-sqr af-sqrt af-exp af-log
    ;; Higher-level operations
    affine-sum affine-product affine-linear-combination affine-horner)

   (interval-integrate
    interval-integrate-naive interval-integrate-adaptive
    interval-integrate-midpoint interval-integrate interval-integrate-rigorous))

  (modules
   (interval "interval.ss"
    "Interval arithmetic for verified numerical computation. Represents sets of
     real numbers with guaranteed enclosure. Supports arithmetic, comparisons
     (three-valued logic), set operations, and transcendental elementary functions.
     Provides both standard (fast) and rigorous (directed rounding) versions.")
   (affine "affine.ss"
    "Affine arithmetic for tighter bounds via correlation tracking. Solves the
     dependency problem in interval arithmetic. Affine forms represent values as
     x0 + sum(xi*ei) where ei are noise symbols in [-1, 1].")
   (interval-integrate "interval-integrate.ss"
    "Verified numerical integration via adaptive interval quadrature.
     Guaranteed enclosure of definite integrals with error bounds.")))
