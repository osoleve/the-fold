;;; scripture/linalg-design.sexp — Linear Algebra Library Architecture
;;;
;;; Design document for The Fold's linear algebra subsystem.
;;; Created: 2025-12-29

(linalg-architecture
 (version "0.1.0")
 (status 'approved)

 ;;; ============================================================
 ;;; Design Principles
 ;;; ============================================================

 (principles
  (pure-core "All core operations are pure functions"
             "No mutation in fabric/stitches/"
             "Shell layer may provide mutable convenience wrappers")

  (total-functions "All operations must terminate"
                   "Fuel costs based on operation complexity"
                   "Return (error ...) rather than diverge")

  (type-integration "Optional dependent typing via dep-types.ss"
                    "Gradual adoption - works with or without types"
                    "Runtime checks when types not used")

  (performance "Efficient representation for common cases"
               "Avoid unnecessary copying"
               "Consider cache locality"))

 ;;; ============================================================
 ;;; Data Representations
 ;;; ============================================================

 (data-structures
  (vec
   (representation 'scheme-vector
                   "Use Scheme's built-in vector type for efficiency"
                   "O(1) random access, O(1) length")
   (construction
    '((make-vec n init)        "Create vector of n elements, all init")
    '((vec-from-list lst)      "Convert list to vector")
    '((vec e1 e2 ...)          "Variadic vector construction"))
   (invariants
    "Elements must be numbers (Int, Float, Complex)"
    "Length is non-negative"))

  (matrix
   (representation 'row-major-vector
                   "Flat vector with row-major storage"
                   "Cache-friendly row access"
                   "(matrix rows cols data)")
   (construction
    '((make-matrix m n init)   "Create m×n matrix, all init")
    '((matrix-from-lists rows) "Create from list of row lists")
    '((identity n)             "n×n identity matrix")
    '((zeros m n)              "m×n zero matrix"))
   (invariants
    "rows >= 0, cols >= 0"
    "data length = rows * cols"
    "Elements must be numbers"))

  (sparse-matrix
   (representation 'coordinate-list
                   "For very sparse matrices (>90% zeros)"
                   "((row col value) ...)"
                   "Sorted by (row, col) for efficient access")
   (note "Phase 2 implementation")))

 ;;; ============================================================
 ;;; Module Organization
 ;;; ============================================================

 (modules
  (core-modules "fabric/stitches/"
   (vec.ss
    "Core vector operations"
    "Pure, total, typed"
    (exports
     make-vec vec-from-list vec
     vec-ref vec-length vec-empty?
     vec-map vec-fold vec-zip-with
     vec-add vec-sub vec-scale vec-negate
     vec-dot vec-norm vec-normalize
     vec-equal? vec-approx-equal?))

   (matrix.ss
    "Core matrix operations"
    "Pure, total, typed"
    (requires vec.ss)
    (exports
     make-matrix matrix-from-lists identity zeros ones
     matrix-ref matrix-set matrix-rows matrix-cols
     matrix-row matrix-col matrix-diagonal
     matrix-transpose matrix-map
     matrix-add matrix-sub matrix-scale
     matrix-mul matrix-vec-mul vec-matrix-mul
     matrix-equal? matrix-approx-equal?))

   (linalg.ss
    "Higher-level linear algebra"
    "Decompositions, solvers, eigenvalues"
    (requires matrix.ss)
    (exports
     lu-decompose lu-solve
     qr-decompose
     eigenvalues eigenvectors
     determinant trace rank
     matrix-inv matrix-pinv
     solve-linear)))

  (typed-modules "fabric/stitches/"
   (dependent-vec.ss
    "Length-indexed vectors using dependent types"
    "Compile-time dimension checking"
    (requires vec.ss dep-types.ss nbe.ss)
    (exports
     Vec ;; Type: Vec n A
     vec-append-typed ;; Vec n A → Vec m A → Vec (+ n m) A
     vec-split-typed  ;; Vec (+ n m) A → (Vec n A × Vec m A)
     vec-zip-typed    ;; Vec n A → Vec n B → Vec n (× A B)
     vec-head-typed   ;; Vec (+ 1 n) A → A
     vec-tail-typed)) ;; Vec (+ 1 n) A → Vec n A

   (dependent-matrix.ss
    "Dimension-safe matrices using dependent types"
    (requires matrix.ss dependent-vec.ss)
    (exports
     Matrix ;; Type: Matrix m n A
     matrix-mul-typed ;; Matrix m n A → Matrix n p A → Matrix m p A
     matrix-vec-mul-typed))) ;; Matrix m n A → Vec n A → Vec m A

  (shell-modules "thimble/"
   (linalg-io.ss
    "Matrix I/O operations"
    (exports
     matrix-to-string matrix-print
     matrix-read-csv matrix-write-csv))))

 ;;; ============================================================
 ;;; API Design
 ;;; ============================================================

 (api-conventions
  (naming
   "Prefix with vec- or matrix- for clarity"
   "Use ! suffix for destructive operations (shell only)"
   "Use ? suffix for predicates"
   "Use -typed suffix for dependent type versions")

  (error-handling
   "Return (error 'dimension-mismatch ...) for dimension errors"
   "Return (error 'singular-matrix ...) for singular matrices"
   "Return (error 'out-of-bounds ...) for index errors"
   "Never throw exceptions in core")

  (numeric-tolerance
   "Default epsilon: 1e-10 for floating-point comparisons"
   "Configurable via optional parameter"
   "Document precision guarantees"))

 ;;; ============================================================
 ;;; Fuel Costs
 ;;; ============================================================

 (fuel-costs
  (principle "Fuel proportional to work performed"
             "1 fuel per element access/write"
             "Compound operations sum component costs")

  (vec-costs
   (vec-ref         1)
   (vec-length      1)
   (vec-add         "n (vector length)")
   (vec-dot         "n")
   (vec-norm        "n")
   (vec-map         "n + f*n (where f is function fuel)")
   (vec-fold        "n + f*n"))

  (matrix-costs
   (matrix-ref      1)
   (matrix-add      "m*n")
   (matrix-mul      "m*n*p for m×n times n×p")
   (matrix-inv      "O(n³)")
   (lu-decompose    "O(n³)")
   (eigenvalues     "O(n³) typical")))

 ;;; ============================================================
 ;;; Type System Integration
 ;;; ============================================================

 (type-integration
  (untyped-mode
   "Default mode - no compile-time dimension checks"
   "Runtime dimension validation on operations"
   "Error returned for mismatched dimensions")

  (typed-mode
   "Opt-in via type annotations"
   "Uses Pi types for length indexing: Vec n A"
   "Uses type-level computation for dimension arithmetic"
   "Dimension errors become type errors")

  (gradual-migration
   "Can mix typed and untyped code"
   "Boundary coercions validated at runtime"
   "(unsafe-coerce-vec vec n) for explicit typing"))

 ;;; ============================================================
 ;;; Numeric Stability
 ;;; ============================================================

 (numeric-considerations
  (float-representation "IEEE 754 double precision")

  (stability-strategies
   "Pivoting in LU decomposition"
   "Gram-Schmidt orthogonalization for QR"
   "Condition number estimation before solve")

  (special-values
   "Handle +inf, -inf, NaN appropriately"
   "Document behavior for edge cases"
   "Consider IEEE 754 semantics"))

 ;;; ============================================================
 ;;; Implementation Roadmap
 ;;; ============================================================

 (roadmap
  (phase-1 "Core Implementation"
   (priority 0)
   (tasks
    "vec.ss - Basic vector operations"
    "matrix.ss - Basic matrix operations"
    "Tests for all operations"
    "Documentation"))

  (phase-2 "Linear Algebra"
   (priority 1)
   (tasks
    "LU decomposition and solve"
    "QR decomposition"
    "Determinant and inverse"
    "Eigenvalue algorithms"))

  (phase-3 "Dependent Types"
   (priority 2)
   (tasks
    "dependent-vec.ss"
    "dependent-matrix.ss"
    "Type-safe dimension operations"))

  (phase-4 "Advanced Features"
   (priority 3)
   (tasks
    "Sparse matrices"
    "Complex number support"
    "BLAS-like optimizations"
    "Parallel operations")))

 ;;; ============================================================
 ;;; Examples
 ;;; ============================================================

 (examples
  (basic-vec
   "Vector arithmetic"
   (code
    "(define v1 (vec 1 2 3))"
    "(define v2 (vec 4 5 6))"
    "(vec-add v1 v2)       ; → #(5 7 9)"
    "(vec-dot v1 v2)       ; → 32"
    "(vec-norm v1)         ; → √14 ≈ 3.742"))

  (basic-matrix
   "Matrix operations"
   (code
    "(define m (matrix-from-lists '((1 2) (3 4))))"
    "(define v (vec 1 2))"
    "(matrix-vec-mul m v)  ; → #(5 11)"))

  (typed-vec
   "Dimension-safe vectors"
   (code
    ";; Type: Vec 3 Int"
    "(the (Vec 3 Int) (vec 1 2 3))"
    ""
    ";; Type: Vec 6 Int"
    "(vec-append-typed (vec 1 2 3) (vec 4 5 6))"
    ""
    ";; Type error: dimension mismatch"
    "(vec-add-typed (vec 1 2) (vec 1 2 3))"))

  (matrix-solve
   "Solving linear systems"
   (code
    ";; Solve Ax = b"
    "(define A (matrix-from-lists '((2 1) (1 3))))"
    "(define b (vec 5 10))"
    "(solve-linear A b)  ; → #(1 3)")))))
