((name "linalg")
 (purpose "Linear algebra operations")
 (description "Comprehensive linear algebra library including vectors,
matrices, decompositions (LU, QR, Cholesky), eigenvalues, sparse matrices,
and linear equation solvers. Dense matrices use row-major flat vectors for
cache efficiency. Sparse matrices support COO, CSR, and CSC formats.")
 (modules
  ((vec.ss "Vector operations - 55 tests")
   (vec2.ss "2D graphics vectors")
   (matrix.ss "Matrix operations - 50 tests")
   (matrix-decomp.ss "LU, QR, Cholesky decompositions - 27 tests")
   (matrix-solvers.ss "Linear equation solvers - 22 tests")
   (matrix-eigen.ss "Eigenvalue/eigenvector computation - 31 tests")
   (sparse.ss "Sparse matrix formats (COO, CSR, CSC) - 119 tests")
   (dep-linalg.ss "Dependent-typed linear algebra")))
 (dependencies (base))
 (representation "Dense: (matrix rows cols data) with flat row-major data.
Sparse COO: (sparse-coo rows cols row-idx col-idx values).
Sparse CSR: (sparse-csr rows cols row-ptrs col-idx values).
Sparse CSC: (sparse-csc rows cols col-ptrs row-idx values).")
 (total-tests 304))
