((name "linalg")
 (purpose "Linear algebra operations")
 (description "Comprehensive linear algebra library including vectors,
matrices, decompositions (LU, QR, Cholesky), and linear equation solvers.
Matrices use row-major flat vectors for cache efficiency.")
 (modules
  ((vec.ss "Vector operations - 55 tests")
   (vec2.ss "2D graphics vectors")
   (matrix.ss "Matrix operations - 50 tests")
   (matrix-decomp.ss "LU, QR, Cholesky decompositions - 22 tests")
   (matrix-solvers.ss "Linear equation solvers - 204 tests")
   (dep-linalg.ss "Dependent-typed linear algebra")))
 (dependencies (base))
 (representation "Matrices are (matrix rows cols data) with flat row-major data")
 (total-tests 331))
