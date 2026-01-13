;;; lattice/linalg/manifest.sexp — Linear Algebra Skill Manifest

(skill linalg
  (version "0.1.0")
  (tier 0)
  (path "lattice/linalg")
  (purity total)
  (stability stable)
  (fuel-bound "O(n³) for general matrices, O(n²) for Toeplitz systems, O(n) for vectors")
  (deps ())  ; Tier 0 - no lattice dependencies

  (description
   "Pure functional linear algebra library with vectors, matrices,
    decompositions, solvers, and quaternions.")

  (keywords (linear-algebra matrix vector quaternion decomposition
             lu qr cholesky eigenvalue svd sparse solver numerical
             toeplitz levinson-durbin spectral-clustering))
  (aliases (la lin-alg linear matrix-math))

  (exports
   (vec vec2 vec3 quaternion matrix)
   (matrix-decomp matrix-solvers matrix-eigen)
   (iterative-solvers sparse graph-laplacian))

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
   (dep-linalg "dep-linalg.ss" "Dependently-typed linear algebra")))
