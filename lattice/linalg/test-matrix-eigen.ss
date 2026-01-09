;;; test-matrix-eigen.ss — Tests for eigenvalue/eigenvector computation
;;;
;;; Run with: scheme --script core/linalg/test-matrix-eigen.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-eigen.ss")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~a (±~a)\n" expected tolerance)
       (printf "      Got:      ~a\n" actual))))

;;; Check if v1 and v2 are parallel (same direction or opposite)
(define (vectors-parallel? v1 v2 tol)
  (let* ([norm1 (vec-norm v1)]
         [norm2 (vec-norm v2)])
        (if (or (< norm1 tol) (< norm2 tol))
            #f
            (let* ([n1 (vec-scale (/ 1.0 norm1) v1)]
                   [n2 (vec-scale (/ 1.0 norm2) v2)]
                   [dot (abs (vec-dot n1 n2))])
                  (> dot (- 1.0 tol))))))

(printf "\n=== Eigenvalue/Eigenvector Tests ===\n\n")

;;; ============================================================
;;; Power Iteration Tests
;;; ============================================================

(printf "--- Power Iteration ---\n")

;; Test: 2x2 diagonal matrix
(let* ([a (matrix-from-lists '((3 0)
                               (0 1)))]
       [result (power-iteration a)]
       [lambda-val (car result)]
       [v (cdr result)])
      ;; Dominant eigenvalue is 3
      (test-approx "Power iteration: 2x2 diagonal eigenvalue" 3.0 lambda-val 1e-6)
      ;; Eigenvector should be [1, 0] or [-1, 0]
      (test "Power iteration: 2x2 diagonal eigenvector parallel"
            #t
            (vectors-parallel? v (vec 1 0) 1e-6)))

;; Test: 2x2 symmetric matrix
(let* ([a (matrix-from-lists '((2 1)
                               (1 2)))]
       [result (power-iteration a)]
       [lambda-val (car result)])
      ;; Eigenvalues are 3 and 1; dominant is 3
      (test-approx "Power iteration: 2x2 symmetric eigenvalue" 3.0 lambda-val 1e-6))

;; Test: 3x3 matrix
(let* ([a (matrix-from-lists '((4 1 1)
                               (1 4 1)
                               (1 1 4)))]
       [result (power-iteration a)]
       [lambda-val (car result)])
      ;; Dominant eigenvalue is 6
      (test-approx "Power iteration: 3x3 eigenvalue" 6.0 lambda-val 1e-5))

;; Test: identity matrix
(let* ([a (identity 3)]
       [result (power-iteration a)]
       [lambda-val (car result)])
      ;; All eigenvalues are 1
      (test-approx "Power iteration: identity eigenvalue" 1.0 lambda-val 1e-6))

;; Test: non-square returns error
(let* ([a (matrix-from-lists '((1 2 3)
                               (4 5 6)))]
       [result (power-iteration a)])
      (test "Power iteration: non-square returns error"
            #t
            (and (pair? result)
                 (eq? (car result) 'error)
                 (eq? (cadr result) 'not-square))))

;;; ============================================================
;;; QR Algorithm Tests
;;; ============================================================

(printf "\n--- QR Algorithm ---\n")

;; Test: 2x2 diagonal matrix
(let* ([a (matrix-from-lists '((5 0)
                               (0 2)))]
       [eigenvalues (qr-algorithm a)])
      (test "QR algorithm: returns vector"
            #t
            (vector? eigenvalues))
      (test "QR algorithm: correct length"
            2
            (vector-length eigenvalues)))

;; Test: 2x2 symmetric - check eigenvalue sum = trace
(let* ([a (matrix-from-lists '((4 2)
                               (2 4)))]
       [eigenvalues (qr-algorithm a)]
       [tr (trace a)])
      ;; Eigenvalues are 6 and 2, sum should be 8
      (test-approx "QR algorithm: eigenvalue sum = trace" tr (vec-sum eigenvalues) 1e-4))

;; Test: QR algorithm shifted - 2x2 diagonal
(let* ([a (matrix-from-lists '((7 0)
                               (0 3)))]
       [eigenvalues (qr-algorithm-shifted a)])
      (test "QR shifted: returns vector"
            #t
            (vector? eigenvalues)))

;; Test: 1x1 matrix
(let* ([a (matrix-from-lists '((7)))]
       [eigenvalues (qr-algorithm-shifted a)])
      (test "QR algorithm: 1x1 matrix"
            #t
            (and (vector? eigenvalues)
                 (= (vector-length eigenvalues) 1)
                 (< (abs (- (vector-ref eigenvalues 0) 7.0)) 1e-10))))

;; Test: eigenvalue sum equals trace (3x3) - use non-singular matrix
(let* ([a (matrix-from-lists '((4 1 0)
                               (1 4 1)
                               (0 1 4)))]
       [eigenvalues (qr-algorithm-shifted a)]
       [tr (trace a)])
      (if (vector? eigenvalues)
          (test-approx "QR algorithm: 3x3 eigenvalue sum = trace" tr (vec-sum eigenvalues) 1e-3)
          (test "QR algorithm: 3x3 non-singular matrix" #t (vector? eigenvalues))))

;;; ============================================================
;;; Inverse Iteration Tests
;;; ============================================================

(printf "\n--- Inverse Iteration ---\n")

;; Test: find eigenvector for approximate eigenvalue
;; Use approximate eigenvalue to avoid singular matrix
(let* ([a (matrix-from-lists '((3 0)
                               (0 1)))]
       ;; Find eigenvector for eigenvalue close to 3
       [v (inverse-iteration a 2.99)])
      ;; Should be approximately [1, 0] or [-1, 0]
      (test "Inverse iteration: eigenvector for λ≈3"
            #t
            (and (vector? v)
                 (vectors-parallel? v (vec 1 0) 1e-3))))

;; Test: 2x2 symmetric with approximate eigenvalue
(let* ([a (matrix-from-lists '((2 1)
                               (1 2)))]
       ;; Eigenvalue 1 has eigenvector [1, -1]/sqrt(2)
       ;; Use approximate eigenvalue to avoid singularity
       [v (inverse-iteration a 1.01)])
      (test "Inverse iteration: eigenvector for λ≈1"
            #t
            (and (vector? v)
                 (vectors-parallel? v (vec 1 -1) 1e-3))))

;;; ============================================================
;;; Eigenvalue Decomposition Tests
;;; ============================================================

(printf "\n--- Eigenvalue Decomposition ---\n")

;; Test: 2x2 diagonal
(let* ([a (matrix-from-lists '((4 0)
                               (0 2)))]
       [result (eigen-decomposition a)])
      (test "Eigen decomposition: returns pair"
            #t
            (pair? result))
      (test "Eigen decomposition: eigenvalues is vector"
            #t
            (vector? (car result)))
      (test "Eigen decomposition: eigenvectors is matrix"
            #t
            (matrix? (cdr result))))

;; Test: non-square error
(let* ([a (matrix-from-lists '((1 2 3)
                               (4 5 6)))]
       [result (eigen-decomposition a)])
      (test "Eigen decomposition: non-square error"
            #t
            (and (pair? result)
                 (eq? (car result) 'error))))

;;; ============================================================
;;; Symmetric Matrix Tests
;;; ============================================================

(printf "\n--- Symmetric Eigenvalues ---\n")

;; Test: 2x2 symmetric
(let* ([a (matrix-from-lists '((4 1)
                               (1 4)))]
       [result (symmetric-eigen a)])
      (test "Symmetric eigen: returns pair"
            #t
            (pair? result)))

;; Test: eigenvalues are correct (5 2; 2 5) has eigenvalues 7 and 3
(let* ([a (matrix-from-lists '((5 2)
                               (2 5)))]
       [result (symmetric-eigen a)]
       [eigenvalues (car result)]
       [sum-ev (vec-sum eigenvalues)])
      ;; Eigenvalues should sum to 10
      (test-approx "Symmetric eigen: eigenvalue sum" 10.0 sum-ev 1e-5))

;; Test: 3x3 symmetric - eigenvalue sum = trace
(let* ([a (matrix-from-lists '((2 1 0)
                               (1 2 1)
                               (0 1 2)))]
       [result (symmetric-eigen a)]
       [eigenvalues (car result)]
       [tr (trace a)])
      ;; Sum of eigenvalues = trace = 6
      (test-approx "Symmetric eigen: 3x3 eigenvalue sum = trace" tr (vec-sum eigenvalues) 1e-4))

;; Test: eigenvectors orthogonal
(let* ([a (matrix-from-lists '((3 1)
                               (1 3)))]
       [result (symmetric-eigen a)]
       [eigenvectors (cdr result)]
       [v1 (matrix-col eigenvectors 0)]
       [v2 (matrix-col eigenvectors 1)]
       [dot (vec-dot v1 v2)])
      ;; Eigenvectors of symmetric matrix are orthogonal
      (test-approx "Symmetric eigen: eigenvectors orthogonal" 0.0 dot 1e-5))

;; Test: rejects non-symmetric
(let* ([a (matrix-from-lists '((1 2)
                               (3 4)))]
       [result (symmetric-eigen a)])
      (test "Symmetric eigen: rejects non-symmetric"
            #t
            (and (pair? result)
                 (eq? (car result) 'error)
                 (eq? (cadr result) 'not-symmetric))))

;;; ============================================================
;;; Spectral Analysis Tests
;;; ============================================================

(printf "\n--- Spectral Analysis ---\n")

;; Test: spectral radius - diagonal matrix with positive eigenvalues
;; Using positive eigenvalues avoids power iteration oscillation issues
(let* ([a (matrix-from-lists '((5 0)
                               (0 2)))]
       [rho (spectral-radius a)])
      ;; Spectral radius is max(|5|, |2|) = 5
      (test-approx "Spectral radius: diagonal matrix" 5.0 rho 1e-5))

;; Test: spectral radius - identity
(let* ([a (identity 3)]
       [rho (spectral-radius a)])
      (test-approx "Spectral radius: identity" 1.0 rho 1e-6))

;; Test: eigenvalue condition - diagonal matrix
(let* ([a (matrix-from-lists '((10 0)
                               (0  2)))]
       [kappa (eigenvalue-condition a)])
      ;; Condition number = 10/2 = 5
      (test-approx "Eigenvalue condition: diagonal" 5.0 kappa 1e-3))

;; Test: eigenvalue condition - identity is 1
(let* ([a (identity 3)]
       [kappa (eigenvalue-condition a)])
      (test-approx "Eigenvalue condition: identity" 1.0 kappa 1e-5))

;;; ============================================================
;;; Verification Tests
;;; ============================================================

(printf "\n--- Verification Functions ---\n")

;; Test: verify-eigenvalue - correct pair
(let* ([a (matrix-from-lists '((3 0)
                               (0 1)))]
       [lambda-val 3.0]
       [v (vec 1 0)])
      (test "Verify eigenvalue: correct pair"
            #t
            (verify-eigenvalue a lambda-val v 1e-10)))

;; Test: verify-eigenvalue - incorrect pair
(let* ([a (matrix-from-lists '((3 0)
                               (0 1)))]
       [lambda-val 2.0]  ;; Wrong eigenvalue
       [v (vec 1 0)])
      (test "Verify eigenvalue: incorrect pair"
            #f
            (verify-eigenvalue a lambda-val v 1e-10)))

;;; ============================================================
;;; Convenience Function Tests
;;; ============================================================

(printf "\n--- Convenience Functions ---\n")

;; Test: eigenvalues returns vector
(let* ([a (matrix-from-lists '((2 0)
                               (0 3)))]
       [evs (eigenvalues a)])
      (test "eigenvalues: returns vector"
            #t
            (and (vector? evs)
                 (= (vector-length evs) 2))))

;; Test: eigenvectors returns matrix
(let* ([a (matrix-from-lists '((2 0)
                               (0 3)))]
       [vecs (eigenvectors a)])
      (test "eigenvectors: returns matrix"
            #t
            (and (matrix? vecs)
                 (= (matrix-rows vecs) 2)
                 (= (matrix-cols vecs) 2))))

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "\n=== Summary ===\n")
(printf "  Passed: ~a\n" tests-passed)
(printf "  Failed: ~a\n" tests-failed)
(printf "  Total:  ~a\n" (+ tests-passed tests-failed))
(if (= tests-failed 0)
    (printf "\n  All tests passed!\n\n")
    (printf "\n  Some tests failed.\n\n"))
