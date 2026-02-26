;;; lattice/linalg/manifest.sexp — Linear Algebra Skill Manifest

(skill linalg
  (version "0.1.0")
  (path "lattice/linalg")
  (purity total)
  (stability stable)
  (fuel-bound (max (cubic n) (max (quadratic n) (linear n))))
  (deps ())  ; Tier 0 - no lattice dependencies (matrix-optics requires optics but is a bridge module)

  (description
   "Pure functional linear algebra library with vectors, matrices,
    decompositions, solvers, and quaternions.")

  (keywords (linear-algebra matrix vector quaternion decomposition
             lu qr cholesky eigenvalue svd sparse solver numerical
             toeplitz levinson-durbin spectral-clustering))
  (aliases (la lin-alg linear matrix-math))

  (concepts
    (concept linear-algebra
      (description "The mathematics of vectors, matrices, linear maps, and their decompositions and solvers.")
      (parent mathematics)
      (synonyms la lin-alg linear matrix-math linalg))
    (concept matrix-theory
      (description "Dense matrix representations, arithmetic, decompositions (LU, QR, Cholesky), and direct solvers.")
      (parent linear-algebra))
    (concept vector-spaces
      (description "Abstract vector operations including 2D, 3D, and n-dimensional vectors with norms, dot products, and transformations.")
      (parent linear-algebra)
      (synonyms quaternion rotation slerp rodrigues))
    (concept eigenvalue-theory
      (description "Computation of eigenvalues, eigenvectors, SVD, spectral decomposition, and related matrix properties.")
      (parent linear-algebra)
      (synonyms eigenvalue eigenvalues eigenvectors svd singular-value-decomposition))
    (concept sparse-methods
      (description "Efficient representations (COO, CSR, CSC) and algorithms for matrices with mostly zero entries.")
      (parent linear-algebra))
    (concept spectral-methods
      (description "Graph Laplacians, spectral clustering, Fiedler vectors, and algebraic connectivity of graphs.")
      (parent linear-algebra)
      (synonyms spectral spectral-clustering graph-laplacian fiedler-vector algebraic-connectivity)))

  (exports
   (vec
    make-vec vec-from-list vec->list vec
    vec-ref vec-length vec-empty? vec-first vec-last
    vec-map vec-fold vec-fold-right vec-zip-with vec-append
    vec-take vec-drop vec-slice
    vec-add vec-sub vec-mul vec-div vec-scale vec-negate
    vec-dot vec-norm-squared vec-norm vec-norm-l1 vec-norm-linf
    vec-normalize vec-distance
    vec-equal? vec-approx-equal?
    vec-sum vec-product vec-mean vec-min vec-max
    vec-argmin vec-argmax vec-reverse vec-copy
    vec-zeros vec-ones vec-range vec-linspace vec-unit)

   (vec2
    vec2 vec2? vec2-x vec2-y vec2->list list->vec2
    vec2-zero vec2-one vec2-unit-x vec2-unit-y
    vec2-add vec2-sub vec2-neg vec2-mul vec2-div vec2-scale vec2-scale-inv
    vec2-dot vec2-magnitude-sq vec2-magnitude vec2-length
    vec2-distance-sq vec2-distance vec2-normalize vec2-unit vec2-set-magnitude
    vec2-cross vec2-perp-dot
    vec2-angle vec2-angle-between vec2-angle-to vec2-heading
    vec2-lerp vec2-slerp vec2-move-towards
    vec2-project vec2-reject vec2-reflect
    vec2-equal? vec2-nearly-equal? vec2-zero?
    vec2-min vec2-max vec2-clamp vec2-abs vec2-floor vec2-ceil vec2-round
    vec2-map vec2-fold vec2-sum vec2-product vec2-clamp-magnitude vec2-limit
    vec2-from-angle vec2-from-polar
    vec2-rotate vec2-rotate-90 vec2-rotate-neg-90 vec2-perp
    vec2->string)

   (vec3
    vec3 vec3? vec3-x vec3-y vec3-z vec3-ref vec3->list list->vec3
    vec3-zero vec3-one vec3-unit-x vec3-unit-y vec3-unit-z
    vec3-add vec3-sub vec3-neg vec3-mul vec3-div vec3-scale vec3-scale-inv
    vec3-dot vec3-cross vec3-triple-scalar vec3-triple-vector
    vec3-magnitude-sq vec3-magnitude vec3-length
    vec3-distance-sq vec3-distance vec3-normalize vec3-normalize-or vec3-set-magnitude
    vec3-lerp vec3-slerp vec3-nlerp
    vec3-project vec3-reject vec3-reflect vec3-refract
    vec3-equal? vec3-nearly-equal? vec3-zero? vec3-unit?
    vec3-min vec3-max vec3-clamp vec3-abs vec3-floor vec3-ceil vec3-clamp-magnitude
    vec3-angle vec3-signed-angle
    vec3-rotate-x vec3-rotate-y vec3-rotate-z vec3-rotate-axis
    vec3-orthonormal-basis vec3-gram-schmidt
    vec3-from-spherical vec3-from-cylindrical vec3->spherical vec3->cylindrical)

   (quaternion
    quat quat-identity quat-zero
    quat-from-axis-angle quat-from-euler quat-from-rotation-matrix
    quat-from-two-vectors quat-look-at
    quat? quat-identity? quat-unit?
    quat-w quat-x quat-y quat-z quat-scalar quat-vector
    quat->list list->quat
    quat-add quat-sub quat-neg quat-scale quat-mul
    quat-magnitude-sq quat-magnitude quat-normalize
    quat-conjugate quat-inverse
    quat-dot quat-angle
    quat-lerp quat-nlerp quat-slerp
    quat-rotate-vec3 quat-rotate-vec3-inverse
    quat-to-axis-angle quat-to-euler quat-to-rotation-matrix
    quat-angular-velocity quat-integrate
    quat-forward quat-right quat-up
    quat-equal? quat-nearly-equal?)

   (matrix
    matrix? matrix-rows matrix-cols matrix-data matrix-shape
    make-matrix matrix-from-lists matrix-from-vec
    matrix-ref matrix-row matrix-col matrix-diagonal
    matrix-transpose matrix-map matrix-map2 matrix-fold
    matrix-add matrix-sub matrix-hadamard matrix-scale matrix-negate
    matrix-mul matrix-vec-mul vec-matrix-mul
    matrix-equal? matrix-approx-equal?
    matrix-square? matrix-symmetric? matrix-trace frobenius-norm
    zeros ones matrix-identity diagonal
    matrix-submatrix matrix-hstack matrix-vstack
    matrix->lists matrix-copy)

   (matrix-decomp
    matrix-lu matrix-lu-solve
    matrix-qr
    matrix-cholesky
    matrix-det permutation-sign)

   (matrix-solvers
    matrix-forward-substitute matrix-back-substitute
    matrix-solve matrix-inverse
    permutation-parity matrix-determinant
    matrix-least-squares
    matrix-gauss-elim matrix-rank
    matrix-condition-number
    levinson-durbin levinson-durbin-general)

   (matrix-eigen
    power-iteration
    qr-algorithm qr-algorithm-shifted
    inverse-iteration
    eigen-decomposition symmetric-eigen
    spectral-radius eigenvalue-condition
    eigenvalues eigenvectors
    verify-eigenvalue verify-decomposition)

   (iterative-solvers
    residual converged?
    jacobi gauss-seidel sor
    conjugate-gradient pcg
    gmres
    apply-givens-rotations compute-givens
    make-jacobi-preconditioner is-diagonally-dominant?
    spectral-radius-estimate)

   (sparse
    sparse-coo? make-sparse-coo
    sparse-coo-rows sparse-coo-cols sparse-coo-row-indices
    sparse-coo-col-indices sparse-coo-values sparse-coo-nnz
    sparse-coo-from-triplets sparse-coo-ref
    sparse-csr? make-sparse-csr
    sparse-csr-rows sparse-csr-cols sparse-csr-row-ptrs
    sparse-csr-col-indices sparse-csr-values sparse-csr-nnz
    sparse-csr-ref sparse-csr-row sparse-csr-row-alist
    sparse-csc? make-sparse-csc
    sparse-csc-rows sparse-csc-cols sparse-csc-col-ptrs
    sparse-csc-row-indices sparse-csc-values sparse-csc-nnz
    sparse-csc-ref sparse-csc-col
    coo->csr coo->csc csr->coo csc->coo csr->csc csc->csr
    dense->sparse-coo dense->sparse-csr
    sparse-coo->dense sparse-csr->dense sparse-csc->dense
    sparse-csr-vec-mul sparse-csc-vec-mul sparse-coo-vec-mul
    sparse-csr-add sparse-coo-add
    sparse-csr-transpose sparse-coo-transpose sparse-csc-transpose
    sparse-csr-mul
    sparse-csr-scale sparse-coo-scale
    sparse-identity sparse-diagonal
    sparse-shape sparse-nnz sparse-density sparse-memory-ratio
    sparse-coo-drop-below sparse-csr-drop-below sparse-csc-drop-below
    sparse-approx-equal? sparse-coo-approx-equal?)

   (graph-laplacian
    laplacian laplacian-from-edges
    laplacian-normalized laplacian-random-walk
    algebraic-connectivity fiedler-vector
    laplacian-connected-components
    spectral-partition spectral-partition-k
    extract-induced-subgraph remap-partition subgraph-partition
    laplacian-trace laplacian-energy spectral-gap
    spectral-radius-laplacian laplacian-max-eigenvalue
    is-bipartite-spectral?
    effective-resistance total-effective-resistance
    laplacian-summary
    spectral-clustering spectral-clustering-unnorm
    k-smallest-indices spectral-embedding
    kmeans-cluster kmeans-init-pp
    conductance ratio-cut normalized-cut)

   (svd
    svd svd-thin
    pseudoinverse
    low-rank-approx
    matrix-rank
    singular-values
    verify-svd
    matrix-frobenius-norm)

   (iteration
    vec-do! vec-map-idx! vec-map-idx vec-fold-idx vec-fold
    vec-zip-do! vec-zip-map-idx
    vec-any? vec-all?
    matrix-do! matrix-map-ij! matrix-fold-ij
    matrix-row-do! matrix-col-do!
    range-do! range-fold
    vec-do-reverse! vec-fold-reverse
    vec-find-idx
    matrix-mul-do! dot-product-loop)

   (numeric-instances
    vec+ vec- vec* vec-abs vec-signum vec-from-integer
    vec/ vec-recip
    vec-pi vec-exp vec-log vec-sqrt vec-pow
    vec-sin vec-cos vec-tan vec-asin vec-acos vec-atan
    vec-sinh vec-cosh vec-tanh vec-asinh vec-acosh vec-atanh
    matrix+ matrix- matrix* matrix-negate matrix-abs matrix-signum
    matrix-from-integer matrix/ matrix-recip
    matrix-pi matrix-exp matrix-log matrix-sqrt matrix-pow
    matrix-sin matrix-cos matrix-tan matrix-asin matrix-acos matrix-atan
    matrix-sinh matrix-cosh matrix-tanh matrix-asinh matrix-acosh matrix-atanh
    scalar-vec+ scalar-vec* scalar-matrix+ scalar-matrix*
    vec-fmap matrix-fmap
    vec-pure vec-ap vec-lift2
    matrix-pure matrix-ap matrix-lift2
    matrix-hadamard)

   (dep-linalg
    dep-linalg-types
    vec-append-typed vec-zip-typed vec-head-typed vec-tail-typed
    vec-map-typed vec-fold-typed
    matrix-mul-typed matrix-add-typed matrix-transpose-typed matrix-scale-typed
    vec-ref-safe matrix-ref-safe
    vec-nil-typed vec-cons-typed
    make-dep-linalg-ctx extend-ctx-with-dep-linalg
    diff-grad diff-jacobian diff-hessian diff-compose
    diff-lift diff-primal diff-scalar diff-jvp diff-vjp)

   (integer-matrix
    smith-normal-form hermite-normal-form)

   (matrix-blocked
    matrix-mul-blocked matrix-transpose-blocked matrix-mul-strassen)

   (matrix-blocked-decomp
    lu-column-update qr-column-orthogonalize)

   (matrix-optics
    matrix-cell-lens matrix-cell-affine
    matrix-row-lens matrix-col-lens matrix-submatrix-lens
    matrix-update-cell matrix-update-row matrix-update-col
    matrix-update-diagonal matrix-scale-row matrix-scale-col
    matrix-swap-rows matrix-add-row-scaled))

  (modules
   (vec "vec.ss" "Generic vector operations")
   (vec2 "vec2.ss" "2D vector specialization")
   (vec3 "vec3.ss" "3D vector specialization")
   (quaternion "quaternion.ss" "Quaternion algebra for rotations")
   (matrix "matrix.ss" "Dense matrix operations")
   (matrix-decomp "matrix-decomp.ss" "LU, QR, Cholesky decomposition")
   (matrix-solvers "matrix-solvers.ss" "Direct solvers, Levinson-Durbin for Toeplitz")
   (matrix-eigen "matrix-eigen.ss" "Eigenvalue computation")
   (iterative-solvers "iterative-solvers.ss" "CG, GMRES, BiCGSTAB")
   (sparse "sparse.ss" "Sparse matrix formats")
   (graph-laplacian "graph-laplacian.ss" "Graph Laplacian, spectral clustering, k-means")
   (svd "svd.ss" "Singular value decomposition")
   (iteration "iteration.ss" "Iterative method utilities")
   (numeric-instances "numeric-instances.ss" "Numeric type class instances")
   (dep-linalg "dep-linalg.ss" "Dependently-typed linear algebra")
   (vec-common "vec-common.ss" "Shared vector macros for vec2/vec3")
   (integer-matrix "integer-matrix.ss" "Smith and Hermite normal forms for integer matrices")
   (matrix-blocked "matrix-blocked.ss" "Cache-efficient blocked/tiled matrix algorithms, Strassen")
   (matrix-blocked-decomp "matrix-blocked-decomp.ss" "Range-parameterized LU/QR kernels for parallel dispatch")
   (matrix-optics "matrix-optics.ss" "Lenses and traversals for matrix manipulation")))
