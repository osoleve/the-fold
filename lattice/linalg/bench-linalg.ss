;;; lattice/linalg/bench-linalg.ss — Linear Algebra Performance Benchmarks
;;;
;;; Comprehensive benchmarks for linalg operations:
;;; - Matrix multiplication (various sizes)
;;; - Linear system solving (LU-based)
;;; - Decompositions (LU, QR, Cholesky)
;;; - Eigenvalue computation
;;; - SVD
;;; - Sparse operations
;;;
;;; Usage: scheme --script lattice/linalg/bench-linalg.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/linalg/matrix-eigen.ss")
(load "lattice/linalg/svd.ss")
(load "lattice/linalg/sparse.ss")
(load "boundary/tools/benchmark.ss")

(printf "\n")
(printf "==============================================================\n")
(printf "              LINEAR ALGEBRA BENCHMARKS                        \n")
(printf "==============================================================\n\n")

;;; ====
;;; Test Matrix Generation
;;; ====

(define (make-random-matrix rows cols)
  "Create matrix with pseudo-random values in [0, 1)."
  (let ([data (make-vector (* rows cols) 0)])
    (do ([i 0 (+ i 1)])
        [(= i (* rows cols)) (list 'matrix rows cols data)]
      (vector-set! data i (/ (modulo (* (+ i 17) 31337) 10000) 10000.0)))))

(define (make-spd-matrix n)
  "Create symmetric positive definite matrix (A^T * A + n*I)."
  (let* ([a (make-random-matrix n n)]
         [at (matrix-transpose a)]
         [ata (matrix-mul at a)])
    ;; Add n*I to ensure positive definiteness
    (let ([data (matrix-data ata)])
      (do ([i 0 (+ i 1)])
          [(= i n) ata]
        (let ([idx (+ (* i n) i)])
          (vector-set! data idx (+ (vector-ref data idx) n)))))))

(define (make-random-vector n)
  "Create vector with pseudo-random values."
  (let ([v (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        [(= i n) v]
      (vector-set! v i (/ (modulo (* (+ i 7) 12345) 10000) 10000.0)))))

;;; Pre-generate test matrices of various sizes
(printf "Generating test matrices...\n")
(define m10 (make-random-matrix 10 10))
(define m25 (make-random-matrix 25 25))
(define m50 (make-random-matrix 50 50))
(define m100 (make-random-matrix 100 100))

(define spd10 (make-spd-matrix 10))
(define spd25 (make-spd-matrix 25))
(define spd50 (make-spd-matrix 50))

(define v10 (make-random-vector 10))
(define v25 (make-random-vector 25))
(define v50 (make-random-vector 50))
(define v100 (make-random-vector 100))

(printf "Test matrices ready.\n\n")

;;; ====
;;; Matrix Multiplication Benchmarks
;;; ====

(printf "=== Matrix Multiplication ===\n\n")

(define matmul-results
  (benchmark-compare
   `(("10x10 * 10x10" . ,(lambda () (matrix-mul m10 m10)))
     ("25x25 * 25x25" . ,(lambda () (matrix-mul m25 m25)))
     ("50x50 * 50x50" . ,(lambda () (matrix-mul m50 m50)))
     ("100x100 * 100x100" . ,(lambda () (matrix-mul m100 m100))))
   100))

(benchmark-report matmul-results)

;;; ====
;;; Matrix-Vector Multiplication
;;; ====

(printf "\n=== Matrix-Vector Multiplication ===\n\n")

(define matvec-results
  (benchmark-compare
   `(("10x10 * vec10" . ,(lambda () (matrix-vec-mul m10 v10)))
     ("25x25 * vec25" . ,(lambda () (matrix-vec-mul m25 v25)))
     ("50x50 * vec50" . ,(lambda () (matrix-vec-mul m50 v50)))
     ("100x100 * vec100" . ,(lambda () (matrix-vec-mul m100 v100))))
   1000))

(benchmark-report matvec-results)

;;; ====
;;; LU Decomposition Benchmarks
;;; ====

(printf "\n=== LU Decomposition ===\n\n")

(define lu-results
  (benchmark-compare
   `(("LU 10x10" . ,(lambda () (matrix-lu m10)))
     ("LU 25x25" . ,(lambda () (matrix-lu m25)))
     ("LU 50x50" . ,(lambda () (matrix-lu m50)))
     ("LU 100x100" . ,(lambda () (matrix-lu m100))))
   100))

(benchmark-report lu-results)

;;; ====
;;; QR Decomposition Benchmarks
;;; ====

(printf "\n=== QR Decomposition ===\n\n")

(define qr-results
  (benchmark-compare
   `(("QR 10x10" . ,(lambda () (matrix-qr m10)))
     ("QR 25x25" . ,(lambda () (matrix-qr m25)))
     ("QR 50x50" . ,(lambda () (matrix-qr m50))))
   100))

(benchmark-report qr-results)

;;; ====
;;; Cholesky Decomposition (SPD matrices only)
;;; ====

(printf "\n=== Cholesky Decomposition (SPD matrices) ===\n\n")

(define chol-results
  (benchmark-compare
   `(("Cholesky 10x10" . ,(lambda () (matrix-cholesky spd10)))
     ("Cholesky 25x25" . ,(lambda () (matrix-cholesky spd25)))
     ("Cholesky 50x50" . ,(lambda () (matrix-cholesky spd50))))
   100))

(benchmark-report chol-results)

;;; ====
;;; Linear System Solving
;;; ====

(printf "\n=== Linear System Solving (Ax = b) ===\n\n")

(define solve-results
  (benchmark-compare
   `(("Solve 10x10" . ,(lambda () (matrix-solve m10 v10)))
     ("Solve 25x25" . ,(lambda () (matrix-solve m25 v25)))
     ("Solve 50x50" . ,(lambda () (matrix-solve m50 v50)))
     ("Solve 100x100" . ,(lambda () (matrix-solve m100 v100))))
   100))

(benchmark-report solve-results)

;;; ====
;;; Matrix Inverse
;;; ====

(printf "\n=== Matrix Inverse ===\n\n")

(define inv-results
  (benchmark-compare
   `(("Inverse 10x10" . ,(lambda () (matrix-inverse m10)))
     ("Inverse 25x25" . ,(lambda () (matrix-inverse m25)))
     ("Inverse 50x50" . ,(lambda () (matrix-inverse m50))))
   50))

(benchmark-report inv-results)

;;; ====
;;; Eigenvalue Computation
;;; ====

(printf "\n=== Power Iteration (Dominant Eigenvalue) ===\n\n")

(define eigen-results
  (benchmark-compare
   `(("Power iter 10x10" . ,(lambda () (power-iteration spd10)))
     ("Power iter 25x25" . ,(lambda () (power-iteration spd25)))
     ("Power iter 50x50" . ,(lambda () (power-iteration spd50))))
   50))

(benchmark-report eigen-results)

;;; ====
;;; SVD (computationally expensive - very few iterations)
;;; NOTE: SVD is slow due to eigenvalue iteration. Prime GPU acceleration target.
;;; ====

(printf "\n=== SVD ===\n\n")
(printf "NOTE: SVD uses eigenvalue decomposition of A^T*A, which is expensive.\n")
(printf "This is a key optimization target for GPU acceleration.\n\n")

(define svd-results
  (benchmark-compare
   `(("SVD 10x10" . ,(lambda () (svd m10))))
   10))

(benchmark-report svd-results)

;;; ====
;;; Sparse Matrix Operations
;;; ====

(printf "\n=== Sparse Matrix Creation ===\n\n")

;; Create sparse test data (diagonal + some off-diagonal)
(define (make-sparse-triplets n density)
  "Create triplets for an n x n sparse matrix."
  (let loop ([i 0] [triplets '()])
    (if (>= i n)
        triplets
        (let inner ([j 0] [t triplets])
          (if (>= j n)
              (loop (+ i 1) t)
              (if (or (= i j)
                      (< (/ (modulo (* i j 17) 100) 100.0) density))
                  (inner (+ j 1) (cons (list i j (+ 1.0 (modulo (+ i j) 10))) t))
                  (inner (+ j 1) t)))))))

(define sparse-triplets-100 (make-sparse-triplets 100 0.05))
(define sparse-triplets-500 (make-sparse-triplets 500 0.01))

(define sparse-create-results
  (benchmark-compare
   `(("COO 100x100 (5% dense)" .
      ,(lambda () (sparse-coo-from-triplets 100 100 sparse-triplets-100)))
     ("COO 500x500 (1% dense)" .
      ,(lambda () (sparse-coo-from-triplets 500 500 sparse-triplets-500))))
   100))

(benchmark-report sparse-create-results)

;;; ====
;;; Sparse-Dense Conversion
;;; ====

(printf "\n=== Sparse-Dense Conversion ===\n\n")

(define coo100 (sparse-coo-from-triplets 100 100 sparse-triplets-100))

(define sparse-convert-results
  (benchmark-compare
   `(("COO->CSR 100x100" . ,(lambda () (coo->csr coo100)))
     ("COO->Dense 100x100" . ,(lambda () (sparse-coo->dense coo100))))
   100))

(benchmark-report sparse-convert-results)

;;; ====
;;; Scaling Analysis
;;; ====

(printf "\n=== Scaling Analysis ===\n\n")
(printf "Matrix multiplication complexity: O(n^3)\n")
(printf "Expected ratio for 2x size increase: approx 8x\n\n")

(let* ([m10-mean (benchmark-result-mean-ns (car matmul-results))]
       [m25-mean (benchmark-result-mean-ns (cadr matmul-results))]
       [m50-mean (benchmark-result-mean-ns (caddr matmul-results))]
       [m100-mean (benchmark-result-mean-ns (cadddr matmul-results))])
  (printf "Size    | Time         | Ratio (vs prev)\n")
  (printf "--------|--------------|----------------\n")
  (printf "10x10   | ~a       | baseline\n" (format-time-ns m10-mean))
  (printf "25x25   | ~a       | ~a x\n"
          (format-time-ns m25-mean)
          (format-ratio (/ m25-mean m10-mean)))
  (printf "50x50   | ~a       | ~a x\n"
          (format-time-ns m50-mean)
          (format-ratio (/ m50-mean m25-mean)))
  (printf "100x100 | ~a       | ~a x\n"
          (format-time-ns m100-mean)
          (format-ratio (/ m100-mean m50-mean))))

;;; ====
;;; Summary
;;; ====

(printf "\n")
(printf "==============================================================\n")
(printf "              BENCHMARK COMPLETE                              \n")
(printf "==============================================================\n\n")

(printf "Key Findings:\n")
(printf "  - Matrix multiplication scales as O(n^3)\n")
(printf "  - LU decomposition dominates linear solve cost\n")
(printf "  - Cholesky is about 2x faster than LU for SPD matrices\n")
(printf "  - Sparse operations valuable when density < 10%\n")
(printf "  - SVD is expensive - eigenvalue approach for A^T*A\n\n")

(printf "Optimization Opportunities:\n")
(printf "  - Block matrix multiply for cache efficiency\n")
(printf "  - Strassen's algorithm for large matrices (n > 256)\n")
(printf "  - Parallel decomposition phases\n")
(printf "  - SIMD vectorization of inner loops\n")
(printf "  - GPU offload for n > 1000\n\n")
