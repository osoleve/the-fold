((name "linalg")
 (purpose "Linear algebra operations")
 (description "Comprehensive linear algebra library including vectors,
matrices, decompositions (LU, QR, Cholesky), eigenvalues, sparse matrices,
and linear equation solvers. Dense matrices use row-major flat vectors for
cache efficiency. Sparse matrices support COO, CSR, and CSC formats.")

 (vector-system
  (overview "Three-level vector abstraction:
    1. vec.ss - Generic n-dimensional vectors using Scheme vectors
    2. vec2.ss - Concrete 2D vectors for physics/graphics (tagged lists)
    3. vec3.ss - Concrete 3D vectors for physics/graphics (tagged lists)

    vec2 and vec3 share common infrastructure via vec-common.ss macros,
    eliminating code duplication while maintaining full performance.")

  (consolidation
   (approach "Macro-based code generation")
   (rationale "Vec2 and vec3 share identical operations (add, sub, dot,
     magnitude, normalize, lerp, project, etc.) but differ in dimension.
     Macros generate these operations, eliminating source duplication
     while producing identical runtime code (zero overhead).")
   (results
    (vec2 (before "405 lines") (after "182 lines") (reduction "55%"))
    (vec3 (before "505 lines") (after "123 lines") (reduction "76%")))))

 (modules
  ((vec-common.ss "Macro infrastructure for generating vector operations")
   (vec.ss "Generic n-dimensional vector operations - 56 tests")
   (vec2.ss "2D graphics/physics vectors - 52 tests")
   (vec3.ss "3D graphics/physics vectors - 41 tests")
   (quaternion.ss "Quaternion rotations (uses vec3) - 34 tests")
   (matrix.ss "Matrix operations - 50 tests")
   (matrix-decomp.ss "LU, QR, Cholesky decompositions - 27 tests")
   (matrix-solvers.ss "Linear equation solvers - 22 tests")
   (matrix-eigen.ss "Eigenvalue/eigenvector computation - 31 tests")
   (sparse.ss "Sparse matrix formats (COO, CSR, CSC) - 119 tests")
   (iterative-solvers.ss "CG, GMRES iterative methods")
   (iteration.ss "Iteration macros for vectors/matrices")
   (graph-laplacian.ss "Graph Laplacian operations")
   (dep-linalg.ss "Dependent-typed linear algebra")))

 (dependencies (base))

 (representation
  (vec "Scheme vector: #(element ...)")
  (vec2 "Tagged list: (vec2 x y)")
  (vec3 "Tagged list: (vec3 x y z)")
  (dense-matrix "(matrix rows cols data) with flat row-major data")
  (sparse-coo "(sparse-coo rows cols row-idx col-idx values)")
  (sparse-csr "(sparse-csr rows cols row-ptrs col-idx values)")
  (sparse-csc "(sparse-csc rows cols col-ptrs row-idx values)"))

 (total-tests 331))
