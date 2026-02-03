# Linear Algebra Library Guide

The Fold's linear algebra library provides pure functional operations on vectors and matrices, including decompositions, solvers, and eigenvalue computation.

## Quick Start

```scheme
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-solvers.ss")

;; Create a matrix and solve Ax = b
(define A (matrix-from-lists '((4 1) (1 3))))
(define b (vec 1 2))
(define x (matrix-solve A b))  ; => #(0.1 0.6333...)
```

---

## 1. Vectors

Vectors are the foundation of the library. They're represented as Scheme vectors for O(1) random access.

### Construction

```scheme
(load "lattice/linalg/vec.ss")

;; Variadic construction
(vec 1 2 3)           ; => #(1 2 3)

;; From list
(vec-from-list '(1 2 3))

;; Uniform initialization
(make-vec 5 0.0)      ; => #(0.0 0.0 0.0 0.0 0.0)
```

### Basic Operations

```scheme
;; Accessors
(vec-length v)        ; Length
(vec-ref v 2)         ; Element at index 2
(vec-first v)         ; First element
(vec-last v)          ; Last element

;; Slicing
(vec-take v 3)        ; First 3 elements
(vec-drop v 2)        ; All but first 2
(vec-slice v 1 4)     ; Elements [1, 4)
```

### Arithmetic

```scheme
(vec-add v1 v2)       ; Element-wise addition
(vec-sub v1 v2)       ; Element-wise subtraction
(vec-mul v1 v2)       ; Hadamard (element-wise) product
(vec-scale 2.0 v)     ; Scalar multiplication
(vec-negate v)        ; Negate all elements
```

### Products and Norms

```scheme
(vec-dot v1 v2)       ; Dot product (inner product)
(vec-norm v)          ; L2 norm (Euclidean length)
(vec-norm-squared v)  ; Squared L2 norm (faster)
(vec-normalize v)     ; Unit vector in same direction
(vec-distance v1 v2)  ; Euclidean distance
```

### Higher-Order Operations

```scheme
(vec-map f v)         ; Apply f to each element
(vec-fold f init v)   ; Left fold
(vec-zip-with f v1 v2); Combine element-wise with f
```

---

## 2. Matrices

Matrices are stored in row-major order as `(matrix rows cols data)` where `data` is a flat vector.

### Construction

```scheme
(load "lattice/linalg/matrix.ss")

;; From nested lists (row-major)
(matrix-from-lists '((1 2 3)
                     (4 5 6)))  ; 2x3 matrix

;; Zero matrix
(make-matrix 3 3 0)

;; Identity matrix
(matrix-identity 4)

;; Diagonal matrix
(matrix-diagonal (vec 1 2 3))
```

### Accessors

```scheme
(matrix-rows M)       ; Number of rows
(matrix-cols M)       ; Number of columns
(matrix-shape M)      ; (rows . cols)
(matrix-ref M i j)    ; Element at row i, column j
(matrix-row M i)      ; Extract row i as vector
(matrix-col M j)      ; Extract column j as vector
```

### Arithmetic

```scheme
(matrix-add A B)      ; Element-wise addition
(matrix-sub A B)      ; Element-wise subtraction
(matrix-scale k A)    ; Scalar multiplication
(matrix-mul A B)      ; Matrix multiplication
(matrix-transpose A)  ; Transpose
```

### Matrix-Vector Operations

```scheme
(matrix-vec-mul A v)  ; A * v (matrix-vector product)
(vec-matrix-mul v A)  ; v^T * A (row vector times matrix)
```

---

## 3. Solving Linear Systems

The primary goal of linear algebra is often solving `Ax = b`.

### Direct Solution

```scheme
(load "lattice/linalg/matrix-solvers.ss")

(define A (matrix-from-lists '((2 1) (1 3))))
(define b (vec 4 5))

;; Solve using LU decomposition
(matrix-solve A b)    ; => solution vector x
```

### Matrix Inverse

```scheme
;; Compute inverse (when it exists)
(matrix-inverse A)

;; Verify: A * A^-1 = I
(matrix-mul A (matrix-inverse A))
```

### Triangular Systems

For triangular matrices, use specialized solvers:

```scheme
;; Forward substitution (lower triangular L)
(matrix-forward-substitute L b)

;; Back substitution (upper triangular U)
(matrix-back-substitute U y)
```

---

## 4. Matrix Decompositions

Decompositions are the workhorses of numerical linear algebra.

### LU Decomposition

Factors A = LU (or PA = LU with pivoting).

```scheme
(load "lattice/linalg/matrix-decomp.ss")

(define lu-result (matrix-lu A))
;; Returns: (L U P) where PA = LU

;; Solve using pre-computed LU
(matrix-lu-solve lu-result b)
```

**Use when:** Solving multiple systems with the same A, computing determinant.

### QR Decomposition

Factors A = QR where Q is orthogonal, R is upper triangular.

```scheme
(define qr-result (matrix-qr A))
;; Returns: (Q R)

;; Q is orthogonal: Q^T * Q = I
;; R is upper triangular
```

**Use when:** Least squares problems, eigenvalue computation, numerical stability needed.

### Cholesky Decomposition

For symmetric positive definite matrices: A = LL^T.

```scheme
(define L (matrix-cholesky A))
;; Returns lower triangular L such that A = L * L^T
```

**Use when:** A is symmetric positive definite (covariance matrices, optimization).

---

## 5. Eigenvalues and Eigenvectors

Finding eigenvalues (λ) and eigenvectors (v) where Av = λv.

```scheme
(load "lattice/linalg/matrix-eigen.ss")

;; All eigenvalues
(eigenvalues A)

;; All eigenvectors (as columns of matrix)
(eigenvectors A)

;; Full decomposition: A = V * diag(λ) * V^-1
(eigen-decomposition A)
;; Returns: (eigenvalues . eigenvector-matrix)
```

### Power Iteration

Find the dominant eigenvalue (largest magnitude):

```scheme
(power-iteration A initial-guess max-iters tolerance)
;; Returns: (eigenvalue . eigenvector)
```

### Inverse Iteration

Find eigenvalue closest to a target:

```scheme
(inverse-iteration A target-eigenvalue)
```

---

## 6. Singular Value Decomposition (SVD)

The SVD factors A = UΣV^T where:
- U, V are orthogonal
- Σ is diagonal with non-negative singular values

```scheme
(load "lattice/linalg/svd.ss")

(define result (svd A))
;; Returns: (U Σ V)

;; Get singular values directly
(singular-values A)

;; Thin SVD (more efficient when m >> n or n >> m)
(svd-thin A)
```

### Applications

**Pseudoinverse** (Moore-Penrose):
```scheme
(pseudoinverse A)
```

**Low-rank approximation** (keep top k singular values):
```scheme
(low-rank-approx A k)  ; Best rank-k approximation
```

**Matrix rank:**
```scheme
(matrix-rank A)
```

---

## 7. Iterative Solvers

For large sparse systems, iterative methods are more efficient.

```scheme
(load "lattice/linalg/iterative-solvers.ss")

;; Conjugate Gradient (for symmetric positive definite)
(cg-solve A b tolerance max-iters)

;; GMRES (for general matrices)
(gmres-solve A b tolerance max-iters restart)

;; BiCGSTAB (for non-symmetric)
(bicgstab-solve A b tolerance max-iters)
```

---

## 8. Sparse Matrices

For matrices with mostly zero entries:

```scheme
(load "lattice/linalg/sparse.ss")

;; Create from triplets (row, col, value)
(sparse-from-triplets rows cols triplets)

;; CSR format (Compressed Sparse Row)
(sparse-csr-from-dense A)

;; Matrix-vector multiply (efficient for sparse)
(sparse-matvec S v)
```

---

## 9. Worked Examples

### Example: Least Squares Regression

Given data points, find the best-fit line y = mx + c.

```scheme
;; Data: (x, y) pairs
(define xs '(1 2 3 4 5))
(define ys '(2.1 3.9 6.2 7.8 10.1))

;; Build design matrix [1, x]
(define A (matrix-from-lists
           (map (lambda (x) (list 1 x)) xs)))

;; Solve normal equations: A^T A x = A^T y
(define AtA (matrix-mul (matrix-transpose A) A))
(define Aty (matrix-vec-mul (matrix-transpose A) (vec-from-list ys)))
(define coeffs (matrix-solve AtA Aty))

;; coeffs = #(c m) for y = c + mx
```

### Example: Principal Component Analysis (PCA)

```scheme
;; Data matrix X (samples as rows)
(define X (matrix-from-lists ...))

;; Center the data
(define means (matrix-col-means X))
(define X-centered (matrix-sub-row X means))

;; Covariance matrix
(define n (matrix-rows X))
(define cov (matrix-scale (/ 1 (- n 1))
                          (matrix-mul (matrix-transpose X-centered) X-centered)))

;; Eigendecomposition
(define result (eigen-decomposition cov))
(define eigenvalues (car result))
(define eigenvectors (cdr result))

;; Principal components are eigenvectors sorted by eigenvalue
;; Project data: X-centered * eigenvectors
```

### Example: Image Compression with SVD

```scheme
;; Image as matrix (grayscale, each row is a scan line)
(define img (matrix-from-lists ...))

;; SVD
(define-values (U S V) (svd img))

;; Keep top k singular values for compression
(define k 50)
(define compressed (low-rank-approx img k))

;; Compression ratio: k * (m + n + 1) / (m * n)
```

---

## 10. Performance Notes

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Vector dot product | O(n) | |
| Matrix multiply | O(n³) | Use Strassen for very large |
| LU decomposition | O(n³) | |
| QR decomposition | O(n³) | More stable than LU |
| Eigenvalues | O(n³) | QR algorithm |
| SVD | O(min(mn², m²n)) | |
| Iterative solvers | O(kn²) | k = iterations to converge |
| Sparse matvec | O(nnz) | nnz = non-zeros |

### Numerical Stability

- **LU**: Can be unstable without pivoting; library uses partial pivoting
- **QR**: More stable for ill-conditioned systems
- **Cholesky**: Most stable when applicable (symmetric positive definite)
- **SVD**: Most stable overall; use for rank-deficient or ill-conditioned problems

---

## Module Reference

| Module | Contents |
|--------|----------|
| `vec.ss` | Vector construction, arithmetic, products, norms |
| `vec2.ss` | Specialized 2D vectors |
| `vec3.ss` | Specialized 3D vectors |
| `matrix.ss` | Matrix construction, arithmetic, transforms |
| `matrix-decomp.ss` | LU, QR, Cholesky decompositions |
| `matrix-solvers.ss` | Direct solvers, substitution |
| `matrix-eigen.ss` | Eigenvalue/eigenvector computation |
| `svd.ss` | Singular value decomposition, pseudoinverse |
| `iterative-solvers.ss` | CG, GMRES, BiCGSTAB |
| `sparse.ss` | Sparse matrix formats (CSR, COO) |
| `quaternion.ss` | Quaternion algebra for 3D rotations |
| `graph-laplacian.ss` | Graph Laplacian, spectral clustering |
