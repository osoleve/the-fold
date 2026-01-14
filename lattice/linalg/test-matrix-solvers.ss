;;; fabric/stitches/test-matrix-solvers.ss — Tests for Matrix Solvers
;;;
;;; Comprehensive tests for linear solvers, inversion, determinants, and least squares.

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-solvers.ss")

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

(printf "\n=== Matrix Solver Tests ===\n\n")

;;; ====
;;; Basic Substitution Tests
;;; ====

(printf "--- Basic Substitution ---\n")

;; Forward substitution: Ly = b
;; [2 0] [y1] = [4]
;; [1 3] [y2]   [8]
;; 2y1 = 4 => y1 = 2
;; 1y1 + 3y2 = 8 => 2 + 3y2 = 8 => 3y2 = 6 => y2 = 2
(let* ([l (matrix-from-lists '((2 0) (1 3)))]
       [b '#(4 8)]
       [y (matrix-forward-substitute l b)])
      (test-approx "Forward substitution y[0]" 2.0 (vector-ref y 0) 1e-10)
      (test-approx "Forward substitution y[1]" 2.0 (vector-ref y 1) 1e-10))

;; Back substitution: Ux = y
;; [2 1] [x1] = [5]
;; [0 3] [x2]   [6]
;; 3x2 = 6 => x2 = 2
;; 2x1 + 1x2 = 5 => 2x1 + 2 = 5 => 2x1 = 3 => x1 = 1.5
(let* ([u (matrix-from-lists '((2 1) (0 3)))]
       [y '#(5 6)]
       [x (matrix-back-substitute u y)])
      (test-approx "Back substitution x[0]" 1.5 (vector-ref x 0) 1e-10)
      (test-approx "Back substitution x[1]" 2.0 (vector-ref x 1) 1e-10))

;;; ====
;;; Matrix Solve Tests
;;; ====

(printf "\n--- Matrix Solve (Ax = b) ---\n")

(let* ([a (matrix-from-lists '((1 1 1) (0 2 5) (2 5 -1)))]
       [b '#(6 -4 27)] ; Known solution x = [5, 3, -2]
       [x (matrix-solve a b)])
      (if (vector? x)
          (begin
           (test-approx "Solve 3×3: x[0]" 5.0 (vector-ref x 0) 1e-10)
           (test-approx "Solve 3×3: x[1]" 3.0 (vector-ref x 1) 1e-10)
           (test-approx "Solve 3×3: x[2]" -2.0 (vector-ref x 2) 1e-10))
          (test "Solve 3×3 succeeds" #t #f)))

;;; ====
;;; Matrix Inverse Tests
;;; ====

(printf "\n--- Matrix Inverse ---\n")

(let* ([a (matrix-from-lists '((4 7) (2 6)))]
       ;; det = 4*6 - 7*2 = 24 - 14 = 10
       ;; inv = 1/10 * [[6 -7] [-2 4]] = [[0.6 -0.7] [-0.2 0.4]]
       [ainv (matrix-inverse a)])
      (if (matrix? ainv)
          (begin
           (test-approx "Inverse 2×2: [0,0]" 0.6 (matrix-ref ainv 0 0) 1e-10)
           (test-approx "Inverse 2×2: [0,1]" -0.7 (matrix-ref ainv 0 1) 1e-10)
           (test-approx "Inverse 2×2: [1,0]" -0.2 (matrix-ref ainv 1 0) 1e-10)
           (test-approx "Inverse 2×2: [1,1]" 0.4 (matrix-ref ainv 1 1) 1e-10)
           ;; Verify A * A^-1 = I
           (let ([ident (matrix-mul a ainv)]
                 [i2 (identity 2)])
                (test "Inverse verification: A * A^-1 ≈ I" #t (matrix-approx-equal? ident i2 1e-10))))
          (test "Inverse 2×2 succeeds" #t #f)))

;;; ====
;;; Determinant Tests
;;; ====

(printf "\n--- Determinant ---\n")

(let ([a (matrix-from-lists '((1 2) (3 4)))])
     (test-approx "Determinant 2×2: (1*4 - 2*3)" -2.0 (matrix-determinant a) 1e-10))

(let ([a (matrix-from-lists '((6 1 1) (4 -2 5) (2 8 7)))])
     ;; det = 6(-2*7 - 5*8) - 1(4*7 - 5*2) + 1(4*8 - (-2)*2)
     ;;     = 6(-14 - 40) - 1(28 - 10) + 1(32 + 4)
     ;;     = 6(-54) - 18 + 36
     ;;     = -324 - 18 + 36 = -306
     (test-approx "Determinant 3×3" -306.0 (matrix-determinant a) 1e-10))

(let ([a (matrix-from-lists '((1 2 3) (2 4 6) (7 8 9)))])
     ;; Row 2 is 2 * Row 1, so det should be 0
     (test-approx "Determinant singular matrix" 0.0 (matrix-determinant a) 1e-10))

;;; ====
;;; Least Squares Tests
;;; ====

(printf "\n--- Least Squares ---\n")

;; Overdetermined system:
;; x + y = 2
;; x - y = 0
;; x + 2y = 3
;;
;; Matrix A:
;; [1 1]
;; [1 -1]
;; [1 2]
;; Vector b: [2, 0, 3]^T
;;
;; Normal equations A^T A x = A^T b:
;; A^T A = [[3 2] [2 6]]
;; A^T b = [[5 8]]
;; Solve:
;; 3x + 2y = 5
;; 2x + 6y = 8 => x + 3y = 4 => x = 4 - 3y
;; 3(4 - 3y) + 2y = 5 => 12 - 9y + 2y = 5 => 7y = 7 => y = 1
;; x = 4 - 3(1) = 1
;; Solution x = [1, 1]^T
(let* ([a (matrix-from-lists '((1 1) (1 -1) (1 2)))]
       [b '#(2 0 3)]
       [x (matrix-least-squares a b)])
      (if (vector? x)
          (begin
           (test-approx "Least Squares: x[0]" 1.0 (vector-ref x 0) 1e-10)
           (test-approx "Least Squares: x[1]" 1.0 (vector-ref x 1) 1e-10))
          (test "Least Squares succeeds" #t #f)))

          ;;; ====
          ;;; Rank and Gaussian Elimination Tests
          ;;; ====
          
          (printf "\n--- Rank and Gaussian Elimination ---\n")

          (let ([a (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))])
               ;; This matrix has rank 2 (row3 = 2*row2 - row1)
               (test "Rank of 3×3 rank-2 matrix" 2 (matrix-rank a)))

          (let ([a (matrix-from-lists '((1 0 0) (0 1 0) (0 0 1)))])
               (test "Rank of I3" 3 (matrix-rank a)))

          (let ([a (matrix-from-lists '((1 2) (2 4) (3 6)))])
               ;; All rows are multiples of [1 2]
               (test "Rank of 3×2 rank-1 matrix" 1 (matrix-rank a)))

          ;;; ====
          ;;; Condition Number Tests
          ;;; ====
          
          (printf "\n--- Condition Number ---\n")

          (let ([a (identity 3)])
               ;; cond(I) = ||I||_F * ||I||_F = sqrt(3) * sqrt(3) = 3
               (test-approx "Condition number of I3" 3.0 (matrix-condition-number a) 1e-10))

          (let ([a (matrix-from-lists '((1 1000) (0 1)))])
               ;; A is somewhat ill-conditioned
               (printf "  Condition number of ill-conditioned matrix: ~a\n" (matrix-condition-number a))
               (test "Condition number > 1" #t (> (matrix-condition-number a) 1)))

          ;;; ====
          ;;; Levinson-Durbin Tests
          ;;; ====

          (printf "\n--- Levinson-Durbin (Toeplitz Solver) ---\n")

          ;; Test 1: Simple AR(1) case
          ;; r = [1, 0.5] means autocorrelations rho_0 = 1, rho_1 = 0.5
          ;; Solve R * phi = [rho_1] where R = [rho_0] = [1]
          ;; phi_1 = rho_1 / rho_0 = 0.5 / 1 = 0.5
          (let* ([r '#(1.0 0.5)]
                 [phi (levinson-durbin r)])
                (if (vector? phi)
                    (begin
                      (test "Levinson-Durbin AR(1): length" 1 (vector-length phi))
                      (test-approx "Levinson-Durbin AR(1): phi_1" 0.5 (vector-ref phi 0) 1e-10))
                    (test "Levinson-Durbin AR(1) succeeds" #t #f)))

          ;; Test 2: AR(2) case
          ;; r = [1, 0.7, 0.4]
          ;; Toeplitz matrix R = [[1, 0.7], [0.7, 1]]
          ;; RHS = [0.7, 0.4]
          ;; Direct solve: phi_1 = (r_0*r_1 - r_2*r_1) / (r_0^2 - r_1^2)
          ;;                     = (1*0.7 - 0.4*0.7) / (1 - 0.49)
          ;;                     = (0.7 - 0.28) / 0.51 = 0.42 / 0.51 ≈ 0.8235
          ;;             phi_2 = (r_2*r_0 - r_1^2) / (r_0^2 - r_1^2)
          ;;                   = (0.4*1 - 0.49) / 0.51 = -0.09 / 0.51 ≈ -0.1765
          (let* ([r '#(1.0 0.7 0.4)]
                 [phi (levinson-durbin r)])
                (if (vector? phi)
                    (begin
                      (test "Levinson-Durbin AR(2): length" 2 (vector-length phi))
                      (test-approx "Levinson-Durbin AR(2): phi_1" 0.8235294117647058 (vector-ref phi 0) 1e-10)
                      (test-approx "Levinson-Durbin AR(2): phi_2" -0.17647058823529413 (vector-ref phi 1) 1e-10))
                    (test "Levinson-Durbin AR(2) succeeds" #t #f)))

          ;; Test 3: Compare with direct matrix solve
          (let* ([r '#(1.0 0.8 0.5 0.2)]
                 [phi (levinson-durbin r)]
                 ;; Build Toeplitz matrix manually
                 [toeplitz (matrix-from-lists '((1.0 0.8 0.5)
                                                (0.8 1.0 0.8)
                                                (0.5 0.8 1.0)))]
                 [rhs '#(0.8 0.5 0.2)]
                 [phi-direct (matrix-solve toeplitz rhs)])
                (if (and (vector? phi) (vector? phi-direct))
                    (begin
                      (test "Levinson-Durbin AR(3): length" 3 (vector-length phi))
                      (test-approx "Levinson-Durbin vs matrix-solve: phi_1" (vector-ref phi-direct 0) (vector-ref phi 0) 1e-10)
                      (test-approx "Levinson-Durbin vs matrix-solve: phi_2" (vector-ref phi-direct 1) (vector-ref phi 1) 1e-10)
                      (test-approx "Levinson-Durbin vs matrix-solve: phi_3" (vector-ref phi-direct 2) (vector-ref phi 2) 1e-10))
                    (test "Levinson-Durbin AR(3) succeeds" #t #f)))

          ;; Test 4: General Toeplitz solver with arbitrary RHS
          (let* ([r '#(4.0 1.0 0.5)]  ; Toeplitz defining vector
                 [b '#(3.0 2.0 1.0)]  ; Arbitrary RHS
                 [x (levinson-durbin-general r b)]
                 ;; Build Toeplitz matrix manually and solve directly
                 [toeplitz (matrix-from-lists '((4.0 1.0 0.5)
                                                (1.0 4.0 1.0)
                                                (0.5 1.0 4.0)))]
                 [x-direct (matrix-solve toeplitz b)])
                (if (and (vector? x) (vector? x-direct))
                    (begin
                      (test "Levinson-Durbin-General: length" 3 (vector-length x))
                      (test-approx "Levinson-Durbin-General vs matrix-solve: x_0" (vector-ref x-direct 0) (vector-ref x 0) 1e-10)
                      (test-approx "Levinson-Durbin-General vs matrix-solve: x_1" (vector-ref x-direct 1) (vector-ref x 1) 1e-10)
                      (test-approx "Levinson-Durbin-General vs matrix-solve: x_2" (vector-ref x-direct 2) (vector-ref x 2) 1e-10))
                    (test "Levinson-Durbin-General succeeds" #t #f)))

          ;; Test 5: Edge case - single element
          (let* ([r '#(2.0)]
                 [b '#(4.0)]
                 [x (levinson-durbin-general r b)])
                (if (vector? x)
                    (test-approx "Levinson-Durbin-General n=1" 2.0 (vector-ref x 0) 1e-10)
                    (test "Levinson-Durbin-General n=1 succeeds" #t #f)))

          ;; Test 6: Empty case
          (let* ([r '#(1.0)]
                 [phi (levinson-durbin r)])
                (if (vector? phi)
                    (test "Levinson-Durbin p=0: empty result" 0 (vector-length phi))
                    (test "Levinson-Durbin p=0 succeeds" #t #f)))

          ;;; ====
          ;;; Results
          ;;; ====
          (printf "\n====\n")
(printf "                    TEST RESULTS\n")
(printf "====\n\n")
(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n[SUCCESS] All matrix solver tests passed.\n")
    (printf "\n[FAILURE] Some tests failed.\n"))
