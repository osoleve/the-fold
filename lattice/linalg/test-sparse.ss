;;; test-sparse.ss — Tests for sparse matrix operations
;;;
;;; Run with: scheme --script core/linalg/test-sparse.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/sparse.ss")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a~n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a~n" name)
       (printf "      Expected: ~s~n" expected)
       (printf "      Got:      ~s~n" actual))))

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a~n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a~n" name)
       (printf "      Expected: ~a (±~a)~n" expected tolerance)
       (printf "      Got:      ~a~n" actual))))

(define (test-vec-approx name expected actual tol)
  (let ([n (vector-length expected)])
       (if (and (= n (vector-length actual))
                (let loop ([i 0])
                     (or (= i n)
                         (and (< (abs (- (vector-ref expected i)
                                         (vector-ref actual i)))
                                 tol)
                              (loop (+ i 1))))))
           (begin
            (set! tests-passed (+ tests-passed 1))
            (printf "  [PASS] ~a~n" name))
           (begin
            (set! tests-failed (+ tests-failed 1))
            (printf "  [FAIL] ~a~n" name)
            (printf "      Expected: ~s~n" expected)
            (printf "      Got:      ~s~n" actual)))))

(printf "~n=== Sparse Matrix Tests ===~n~n")

;;; ============================================================
;;; COO Format Tests
;;; ============================================================

(printf "--- COO Format ---~n")

;; Test: create COO from triplets
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 1) (0 2 2) (1 1 3) (2 0 4)))])
      (test "COO creation: is sparse-coo?" #t (sparse-coo? coo))
      (test "COO creation: rows" 3 (sparse-coo-rows coo))
      (test "COO creation: cols" 3 (sparse-coo-cols coo))
      (test "COO creation: nnz" 4 (sparse-coo-nnz coo)))

;; Test: COO element access
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 5) (1 2 7) (2 1 9)))])
      (test "COO ref: (0,0)" 5 (sparse-coo-ref coo 0 0))
      (test "COO ref: (1,2)" 7 (sparse-coo-ref coo 1 2))
      (test "COO ref: (2,1)" 9 (sparse-coo-ref coo 2 1))
      (test "COO ref: zero element" 0 (sparse-coo-ref coo 0 1)))

;; Test: COO transpose
(let* ([coo (sparse-coo-from-triplets 2 3 '((0 0 1) (0 2 2) (1 1 3)))]
       [coo-t (sparse-coo-transpose coo)])
      (test "COO transpose: rows" 3 (sparse-coo-rows coo-t))
      (test "COO transpose: cols" 2 (sparse-coo-cols coo-t))
      (test "COO transpose: (0,0)" 1 (sparse-coo-ref coo-t 0 0))
      (test "COO transpose: (2,0)" 2 (sparse-coo-ref coo-t 2 0))
      (test "COO transpose: (1,1)" 3 (sparse-coo-ref coo-t 1 1)))

;;; ============================================================
;;; CSR Format Tests
;;; ============================================================

(printf "~n--- CSR Format ---~n")

;; Test: COO to CSR conversion
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 1) (0 2 2) (1 1 3) (2 0 4) (2 2 5)))]
       [csr (coo->csr coo)])
      (test "CSR conversion: is sparse-csr?" #t (sparse-csr? csr))
      (test "CSR conversion: rows" 3 (sparse-csr-rows csr))
      (test "CSR conversion: cols" 3 (sparse-csr-cols csr))
      (test "CSR conversion: nnz" 5 (sparse-csr-nnz csr)))

;; Test: CSR element access
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 5) (0 2 3) (1 1 7) (2 0 2) (2 2 9)))]
       [csr (coo->csr coo)])
      (test "CSR ref: (0,0)" 5 (sparse-csr-ref csr 0 0))
      (test "CSR ref: (0,2)" 3 (sparse-csr-ref csr 0 2))
      (test "CSR ref: (1,1)" 7 (sparse-csr-ref csr 1 1))
      (test "CSR ref: (2,0)" 2 (sparse-csr-ref csr 2 0))
      (test "CSR ref: (2,2)" 9 (sparse-csr-ref csr 2 2))
      (test "CSR ref: zero element" 0 (sparse-csr-ref csr 0 1)))

;; Test: CSR row extraction
(let* ([coo (sparse-coo-from-triplets 3 4 '((0 0 1) (0 3 2) (1 1 3) (1 2 4) (2 0 5)))]
       [csr (coo->csr coo)]
       [row0 (sparse-csr-row csr 0)]
       [row1 (sparse-csr-row csr 1)])
      (test "CSR row 0 length" 2 (length row0))
      (test "CSR row 1 length" 2 (length row1)))

;; Test: CSR transpose
(let* ([coo (sparse-coo-from-triplets 2 3 '((0 0 1) (0 2 2) (1 1 3)))]
       [csr (coo->csr coo)]
       [csr-t (sparse-csr-transpose csr)])
      (test "CSR transpose: rows" 3 (sparse-csr-rows csr-t))
      (test "CSR transpose: cols" 2 (sparse-csr-cols csr-t))
      (test "CSR transpose ref: (0,0)" 1 (sparse-csr-ref csr-t 0 0))
      (test "CSR transpose ref: (2,0)" 2 (sparse-csr-ref csr-t 2 0))
      (test "CSR transpose ref: (1,1)" 3 (sparse-csr-ref csr-t 1 1)))

;;; ============================================================
;;; CSC Format Tests
;;; ============================================================

(printf "~n--- CSC Format ---~n")

;; Test: COO to CSC conversion
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 1) (1 0 2) (0 2 3) (2 2 4)))]
       [csc (coo->csc coo)])
      (test "CSC conversion: is sparse-csc?" #t (sparse-csc? csc))
      (test "CSC conversion: rows" 3 (sparse-csc-rows csc))
      (test "CSC conversion: cols" 3 (sparse-csc-cols csc))
      (test "CSC conversion: nnz" 4 (sparse-csc-nnz csc)))

;; Test: CSC element access
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 5) (1 0 3) (1 2 7) (2 2 9)))]
       [csc (coo->csc coo)])
      (test "CSC ref: (0,0)" 5 (sparse-csc-ref csc 0 0))
      (test "CSC ref: (1,0)" 3 (sparse-csc-ref csc 1 0))
      (test "CSC ref: (1,2)" 7 (sparse-csc-ref csc 1 2))
      (test "CSC ref: (2,2)" 9 (sparse-csc-ref csc 2 2))
      (test "CSC ref: zero element" 0 (sparse-csc-ref csc 0 1)))

;; Test: CSC column extraction
(let* ([coo (sparse-coo-from-triplets 4 3 '((0 0 1) (2 0 2) (1 1 3) (3 2 4)))]
       [csc (coo->csc coo)]
       [col0 (sparse-csc-col csc 0)])
      (test "CSC col 0 length" 2 (length col0)))

;;; ============================================================
;;; Format Conversion Round-trips
;;; ============================================================

(printf "~n--- Format Conversions ---~n")

;; Test: COO -> CSR -> COO preserves values
(let* ([coo1 (sparse-coo-from-triplets 3 3 '((0 0 1) (1 1 2) (2 2 3)))]
       [csr (coo->csr coo1)]
       [coo2 (csr->coo csr)])
      (test "COO->CSR->COO: (0,0)" 1 (sparse-coo-ref coo2 0 0))
      (test "COO->CSR->COO: (1,1)" 2 (sparse-coo-ref coo2 1 1))
      (test "COO->CSR->COO: (2,2)" 3 (sparse-coo-ref coo2 2 2))
      (test "COO->CSR->COO: nnz" 3 (sparse-coo-nnz coo2)))

;; Test: COO -> CSC -> COO preserves values
(let* ([coo1 (sparse-coo-from-triplets 3 3 '((0 1 5) (1 0 6) (2 2 7)))]
       [csc (coo->csc coo1)]
       [coo2 (csc->coo csc)])
      (test "COO->CSC->COO: (0,1)" 5 (sparse-coo-ref coo2 0 1))
      (test "COO->CSC->COO: (1,0)" 6 (sparse-coo-ref coo2 1 0))
      (test "COO->CSC->COO: (2,2)" 7 (sparse-coo-ref coo2 2 2)))

;; Test: CSR -> CSC -> CSR
(let* ([coo (sparse-coo-from-triplets 2 3 '((0 0 1) (0 2 2) (1 1 3)))]
       [csr1 (coo->csr coo)]
       [csc (csr->csc csr1)]
       [csr2 (csc->csr csc)])
      (test "CSR->CSC->CSR: (0,0)" 1 (sparse-csr-ref csr2 0 0))
      (test "CSR->CSC->CSR: (0,2)" 2 (sparse-csr-ref csr2 0 2))
      (test "CSR->CSC->CSR: (1,1)" 3 (sparse-csr-ref csr2 1 1)))

;;; ============================================================
;;; Dense <-> Sparse Conversions
;;; ============================================================

(printf "~n--- Dense/Sparse Conversion ---~n")

;; Test: dense to sparse COO
(let* ([dense (matrix-from-lists '((1 0 2)
                                   (0 3 0)
                                   (4 0 5)))]
       [coo (dense->sparse-coo dense)])
      (test "Dense->COO: nnz" 5 (sparse-coo-nnz coo))
      (test "Dense->COO: (0,0)" 1 (sparse-coo-ref coo 0 0))
      (test "Dense->COO: (0,2)" 2 (sparse-coo-ref coo 0 2))
      (test "Dense->COO: (1,1)" 3 (sparse-coo-ref coo 1 1))
      (test "Dense->COO: (2,0)" 4 (sparse-coo-ref coo 2 0))
      (test "Dense->COO: (2,2)" 5 (sparse-coo-ref coo 2 2)))

;; Test: sparse COO to dense
(let* ([coo (sparse-coo-from-triplets 2 3 '((0 0 1) (0 2 2) (1 1 3)))]
       [dense (sparse-coo->dense coo)])
      (test "COO->Dense: shape" '(2 . 3) (matrix-shape dense))
      (test "COO->Dense: (0,0)" 1 (matrix-ref dense 0 0))
      (test "COO->Dense: (0,1)" 0 (matrix-ref dense 0 1))
      (test "COO->Dense: (0,2)" 2 (matrix-ref dense 0 2))
      (test "COO->Dense: (1,1)" 3 (matrix-ref dense 1 1)))

;; Test: sparse CSR to dense
(let* ([coo (sparse-coo-from-triplets 2 2 '((0 0 5) (0 1 6) (1 0 7) (1 1 8)))]
       [csr (coo->csr coo)]
       [dense (sparse-csr->dense csr)])
      (test "CSR->Dense: (0,0)" 5 (matrix-ref dense 0 0))
      (test "CSR->Dense: (0,1)" 6 (matrix-ref dense 0 1))
      (test "CSR->Dense: (1,0)" 7 (matrix-ref dense 1 0))
      (test "CSR->Dense: (1,1)" 8 (matrix-ref dense 1 1)))

;; Test: round-trip dense -> sparse -> dense
(let* ([dense1 (matrix-from-lists '((1 0 0)
                                    (0 2 0)
                                    (0 0 3)))]
       [csr (dense->sparse-csr dense1)]
       [dense2 (sparse-csr->dense csr)])
      (test "Dense round-trip: equal" #t (matrix-equal? dense1 dense2)))

;;; ============================================================
;;; Sparse Matrix-Vector Multiplication
;;; ============================================================

(printf "~n--- Sparse Matrix-Vector Multiplication ---~n")

;; Test: CSR matrix-vector multiplication
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 1) (0 1 2) (1 1 3) (2 0 4) (2 2 5)))]
       [csr (coo->csr coo)]
       [v (vec 1 2 3)]
       [result (sparse-csr-vec-mul csr v)])
      ;; Row 0: 1*1 + 2*2 = 5
      ;; Row 1: 3*2 = 6
      ;; Row 2: 4*1 + 5*3 = 19
      (test-vec-approx "CSR matvec" (vec 5 6 19) result 1e-10))

;; Test: CSC matrix-vector multiplication
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 1) (0 1 2) (1 1 3) (2 0 4) (2 2 5)))]
       [csc (coo->csc coo)]
       [v (vec 1 2 3)]
       [result (sparse-csc-vec-mul csc v)])
      (test-vec-approx "CSC matvec" (vec 5 6 19) result 1e-10))

;; Test: COO matrix-vector multiplication
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 1) (0 1 2) (1 1 3) (2 0 4) (2 2 5)))]
       [v (vec 1 2 3)]
       [result (sparse-coo-vec-mul coo v)])
      (test-vec-approx "COO matvec" (vec 5 6 19) result 1e-10))

;; Test: dimension mismatch error
(let* ([coo (sparse-coo-from-triplets 2 3 '((0 0 1)))]
       [csr (coo->csr coo)]
       [v (vec 1 2)]  ;; Wrong size
       [result (sparse-csr-vec-mul csr v)])
      (test "CSR matvec: dimension error" 'error (car result)))

;; Test: compare with dense multiplication
(let* ([dense (matrix-from-lists '((2 0 1)
                                   (0 3 0)
                                   (4 0 2)))]
       [csr (dense->sparse-csr dense)]
       [v (vec 1 2 3)]
       [sparse-result (sparse-csr-vec-mul csr v)]
       [dense-result (matrix-vec-mul dense v)])
      (test-vec-approx "Sparse vs Dense matvec" dense-result sparse-result 1e-10))

;;; ============================================================
;;; Sparse Matrix Addition
;;; ============================================================

(printf "~n--- Sparse Matrix Addition ---~n")

;; Test: COO addition
(let* ([a (sparse-coo-from-triplets 2 2 '((0 0 1) (1 1 2)))]
       [b (sparse-coo-from-triplets 2 2 '((0 0 3) (0 1 4)))]
       [c (sparse-coo-add a b)])
      (test "COO add: (0,0)" 4 (sparse-coo-ref c 0 0))
      (test "COO add: (0,1)" 4 (sparse-coo-ref c 0 1))
      (test "COO add: (1,1)" 2 (sparse-coo-ref c 1 1)))

;; Test: CSR addition
(let* ([a (sparse-coo-from-triplets 2 2 '((0 0 1) (1 1 2)))]
       [b (sparse-coo-from-triplets 2 2 '((0 0 3) (0 1 4)))]
       [csr-a (coo->csr a)]
       [csr-b (coo->csr b)]
       [c (sparse-csr-add csr-a csr-b)])
      (test "CSR add: (0,0)" 4 (sparse-csr-ref c 0 0))
      (test "CSR add: (0,1)" 4 (sparse-csr-ref c 0 1))
      (test "CSR add: (1,1)" 2 (sparse-csr-ref c 1 1)))

;; Test: addition cancellation (a + (-a) = 0)
(let* ([a (sparse-coo-from-triplets 2 2 '((0 0 5) (1 1 7)))]
       [neg-a (sparse-coo-scale -1 a)]
       [sum (sparse-coo-add a neg-a)])
      (test "COO add cancellation: nnz" 0 (sparse-coo-nnz sum)))

;; Test: dimension mismatch
(let* ([a (sparse-coo-from-triplets 2 3 '((0 0 1)))]
       [b (sparse-coo-from-triplets 3 2 '((0 0 1)))]
       [result (sparse-coo-add a b)])
      (test "COO add: dimension error" 'error (car result)))

;;; ============================================================
;;; Sparse Matrix-Matrix Multiplication
;;; ============================================================

(printf "~n--- Sparse Matrix-Matrix Multiplication ---~n")

;; Test: identity multiplication
(let* ([a (sparse-coo-from-triplets 3 3 '((0 0 1) (0 1 2) (1 1 3) (2 2 4)))]
       [csr-a (coo->csr a)]
       [I (sparse-identity 3)]
       [result (sparse-csr-mul csr-a I)])
      (test "A * I = A: (0,0)" 1 (sparse-csr-ref result 0 0))
      (test "A * I = A: (0,1)" 2 (sparse-csr-ref result 0 1))
      (test "A * I = A: (1,1)" 3 (sparse-csr-ref result 1 1))
      (test "A * I = A: (2,2)" 4 (sparse-csr-ref result 2 2)))

;; Test: simple multiplication
(let* ([a (sparse-coo-from-triplets 2 2 '((0 0 1) (0 1 2) (1 0 3) (1 1 4)))]
       [b (sparse-coo-from-triplets 2 2 '((0 0 5) (0 1 6) (1 0 7) (1 1 8)))]
       [csr-a (coo->csr a)]
       [csr-b (coo->csr b)]
       [c (sparse-csr-mul csr-a csr-b)])
      ;; [1 2] * [5 6] = [1*5+2*7  1*6+2*8] = [19 22]
      ;; [3 4]   [7 8]   [3*5+4*7  3*6+4*8]   [43 50]
      (test "CSR mul: (0,0)" 19 (sparse-csr-ref c 0 0))
      (test "CSR mul: (0,1)" 22 (sparse-csr-ref c 0 1))
      (test "CSR mul: (1,0)" 43 (sparse-csr-ref c 1 0))
      (test "CSR mul: (1,1)" 50 (sparse-csr-ref c 1 1)))

;; Test: compare with dense multiplication
(let* ([dense-a (matrix-from-lists '((1 2 0)
                                     (0 3 4)))]
       [dense-b (matrix-from-lists '((1 0)
                                     (2 3)
                                     (0 4)))]
       [csr-a (dense->sparse-csr dense-a)]
       [csr-b (dense->sparse-csr dense-b)]
       [sparse-c (sparse-csr-mul csr-a csr-b)]
       [dense-c (matrix-mul dense-a dense-b)]
       [sparse-dense (sparse-csr->dense sparse-c)])
      (test "Sparse vs Dense matmul" #t (matrix-equal? dense-c sparse-dense)))

;; Test: dimension mismatch
(let* ([a (coo->csr (sparse-coo-from-triplets 2 3 '((0 0 1))))]
       [b (coo->csr (sparse-coo-from-triplets 2 2 '((0 0 1))))]
       [result (sparse-csr-mul a b)])
      (test "CSR mul: dimension error" 'error (car result)))

;;; ============================================================
;;; Sparse Scalar Operations
;;; ============================================================

(printf "~n--- Sparse Scalar Operations ---~n")

;; Test: CSR scale
(let* ([coo (sparse-coo-from-triplets 2 2 '((0 0 2) (1 1 3)))]
       [csr (coo->csr coo)]
       [scaled (sparse-csr-scale 3 csr)])
      (test "CSR scale: (0,0)" 6 (sparse-csr-ref scaled 0 0))
      (test "CSR scale: (1,1)" 9 (sparse-csr-ref scaled 1 1))
      (test "CSR scale: nnz preserved" 2 (sparse-csr-nnz scaled)))

;; Test: COO scale
(let* ([coo (sparse-coo-from-triplets 2 2 '((0 1 4) (1 0 5)))]
       [scaled (sparse-coo-scale 2 coo)])
      (test "COO scale: (0,1)" 8 (sparse-coo-ref scaled 0 1))
      (test "COO scale: (1,0)" 10 (sparse-coo-ref scaled 1 0)))

;;; ============================================================
;;; Special Sparse Matrices
;;; ============================================================

(printf "~n--- Special Sparse Matrices ---~n")

;; Test: sparse identity
(let* ([I (sparse-identity 4)])
      (test "Sparse identity: is CSR" #t (sparse-csr? I))
      (test "Sparse identity: shape" '(4 . 4) (sparse-shape I))
      (test "Sparse identity: nnz" 4 (sparse-nnz I))
      (test "Sparse identity: (0,0)" 1 (sparse-csr-ref I 0 0))
      (test "Sparse identity: (1,1)" 1 (sparse-csr-ref I 1 1))
      (test "Sparse identity: (0,1)" 0 (sparse-csr-ref I 0 1)))

;; Test: sparse diagonal
(let* ([d (sparse-diagonal (vec 2 3 4))])
      (test "Sparse diagonal: shape" '(3 . 3) (sparse-shape d))
      (test "Sparse diagonal: nnz" 3 (sparse-nnz d))
      (test "Sparse diagonal: (0,0)" 2 (sparse-csr-ref d 0 0))
      (test "Sparse diagonal: (1,1)" 3 (sparse-csr-ref d 1 1))
      (test "Sparse diagonal: (2,2)" 4 (sparse-csr-ref d 2 2)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(printf "~n--- Utility Functions ---~n")

;; Test: sparse-shape
(let* ([coo (sparse-coo-from-triplets 3 4 '((0 0 1)))]
       [csr (coo->csr coo)]
       [csc (coo->csc coo)])
      (test "sparse-shape COO" '(3 . 4) (sparse-shape coo))
      (test "sparse-shape CSR" '(3 . 4) (sparse-shape csr))
      (test "sparse-shape CSC" '(3 . 4) (sparse-shape csc)))

;; Test: sparse-nnz
(let* ([coo (sparse-coo-from-triplets 3 3 '((0 0 1) (1 1 2) (2 2 3)))]
       [csr (coo->csr coo)])
      (test "sparse-nnz COO" 3 (sparse-nnz coo))
      (test "sparse-nnz CSR" 3 (sparse-nnz csr)))

;; Test: sparse-density
(let* ([coo (sparse-coo-from-triplets 4 4 '((0 0 1) (1 1 2) (2 2 3) (3 3 4)))])
      (test-approx "sparse-density" 0.25 (sparse-density coo) 1e-10))

;; Test: sparse-memory-ratio
(let* ([coo (sparse-coo-from-triplets 10 10 '((0 0 1) (5 5 2)))])
      ;; COO: 3*2 = 6 values, dense = 100, ratio = 0.06
      (test-approx "sparse-memory-ratio COO" 0.06 (sparse-memory-ratio coo) 1e-10))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(printf "~n--- Edge Cases ---~n")

;; Test: empty sparse matrix
(let* ([coo (sparse-coo-from-triplets 3 3 '())])
      (test "Empty sparse: nnz" 0 (sparse-coo-nnz coo))
      (test "Empty sparse: all zeros" 0 (sparse-coo-ref coo 1 1)))

;; Test: fully dense sparse matrix
(let* ([coo (sparse-coo-from-triplets 2 2 '((0 0 1) (0 1 2) (1 0 3) (1 1 4)))])
      (test "Full sparse: nnz" 4 (sparse-coo-nnz coo))
      (test-approx "Full sparse: density" 1.0 (sparse-density coo) 1e-10))

;; Test: single element
(let* ([coo (sparse-coo-from-triplets 100 100 '((50 50 42)))]
       [csr (coo->csr coo)])
      (test "Single element: nnz" 1 (sparse-csr-nnz csr))
      (test "Single element: ref" 42 (sparse-csr-ref csr 50 50))
      (test "Single element: zero" 0 (sparse-csr-ref csr 0 0)))

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "~n=== Summary ===~n")
(printf "  Passed: ~a~n" tests-passed)
(printf "  Failed: ~a~n" tests-failed)
(printf "  Total:  ~a~n" (+ tests-passed tests-failed))
(if (= tests-failed 0)
    (printf "~n  All tests passed!~n~n")
    (printf "~n  Some tests failed.~n~n"))
