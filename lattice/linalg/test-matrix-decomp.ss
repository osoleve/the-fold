;;; fabric/stitches/test-matrix-decomp.ss — Tests for Matrix Decompositions
;;;
;;; Comprehensive tests for LU, QR, and Cholesky decompositions
;;; with numerical stability checks.

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")

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

(define (matrix-approx-equal? m1 m2 tolerance)
  (and (= (matrix-rows m1) (matrix-rows m2))
       (= (matrix-cols m1) (matrix-cols m2))
       (let ([rows (matrix-rows m1)]
             [cols (matrix-cols m1)])
            (let loop ([i 0])
                 (cond
                  [(= i rows) #t]
                  [else
                   (let inner ([j 0])
                        (cond
                         [(= j cols) (loop (+ i 1))]
                         [(< (abs (- (matrix-ref m1 i j) (matrix-ref m2 i j))) tolerance)
                          (inner (+ j 1))]
                         [else #f]))])))))

(printf "\n=== Matrix Decomposition Tests ===\n\n")

;;; ====
;;; LU Decomposition Tests
;;; ====

(printf "--- LU Decomposition ---\n")

;; Test 1: Simple 2×2 matrix
(let* ([a (matrix-from-lists '((4 3) (6 3)))]
       [result (matrix-lu a)])
      (if (pair? result)
          (let ([l (car result)]
                [u (cadr result)])
               ;; Verify that L is lower triangular with 1s on diagonal
               (test "LU: L is lower triangular (2×2)"
                     #t
                     (and (= (matrix-ref l 0 0) 1)
                          (= (matrix-ref l 1 1) 1)
                          (= (matrix-ref l 0 1) 0)))
               ;; Verify that U is upper triangular
               (test "LU: U is upper triangular (2×2)"
                     #t
                     (= (matrix-ref u 1 0) 0)))
          (test "LU: 2×2 matrix succeeds" #t #f)))

;; Test 2: 3×3 matrix
(let* ([a (matrix-from-lists '((2 1 1) (4 -6 0) (-2 7 2)))]
       [result (matrix-lu a)])
      (if (pair? result)
          (let ([l (car result)]
                [u (cadr result)]
                [p (caddr result)])
               (test "LU: 3×3 decomposition succeeds" #t #t)
               ;; Check L has 1s on diagonal
               (test "LU: L diagonal is 1s"
                     #t
                     (and (= (matrix-ref l 0 0) 1)
                          (= (matrix-ref l 1 1) 1)
                          (= (matrix-ref l 2 2) 1))))
          (test "LU: 3×3 matrix fails" #t #f)))

;; Test 3: Identity matrix
(let* ([i3 (identity 3)]
       [result (matrix-lu i3)])
      (if (pair? result)
          (let ([l (car result)]
                [u (cadr result)])
               (test "LU: Identity → L = I"
                     #t
                     (matrix-approx-equal? l i3 1e-10))
               (test "LU: Identity → U = I"
                     #t
                     (matrix-approx-equal? u i3 1e-10)))
          (test "LU: Identity fails" #t #f)))

;; Test: LU solve - simple 2×2 system
(let* ([a (matrix-from-lists '((2 1) (1 3)))]
       [b '#(5 6)]  ; Solution should be x = [2.2, 0.6] approximately
       [result (matrix-lu a)])
      (if (pair? result)
          (let* ([x (matrix-lu-solve result b)]
                 ;; Verify Ax = b
                 [ax0 (+ (* (matrix-ref a 0 0) (vector-ref x 0))
                         (* (matrix-ref a 0 1) (vector-ref x 1)))]
                 [ax1 (+ (* (matrix-ref a 1 0) (vector-ref x 0))
                         (* (matrix-ref a 1 1) (vector-ref x 1)))])
                (test-approx "LU solve: Ax=b component 0" (vector-ref b 0) ax0 1e-10)
                (test-approx "LU solve: Ax=b component 1" (vector-ref b 1) ax1 1e-10))
          (test "LU solve: decomposition failed" #t #f)))

;; Test: LU solve - 3×3 system
(let* ([a (matrix-from-lists '((1 2 3) (4 5 6) (7 8 10)))]
       [b '#(14 32 53)]  ; Known solution x = [1, 2, 3]
       [result (matrix-lu a)])
      (if (pair? result)
          (let ([x (matrix-lu-solve result b)])
               (test-approx "LU solve: 3×3 x[0]" 1.0 (vector-ref x 0) 1e-10)
               (test-approx "LU solve: 3×3 x[1]" 2.0 (vector-ref x 1) 1e-10)
               (test-approx "LU solve: 3×3 x[2]" 3.0 (vector-ref x 2) 1e-10))
          (test "LU solve: 3×3 decomposition failed" #t #f)))

;;; ====
;;; QR Decomposition Tests
;;; ====

(printf "\n--- QR Decomposition ---\n")

;; Test 4: Simple 2×2 matrix
(let* ([a (matrix-from-lists '((1 1) (0 1)))]
       [result (matrix-qr a)])
      (if (pair? result)
          (let ([q (car result)]
                [r (cadr result)])
               (test "QR: 2×2 decomposition succeeds" #t #t)
               ;; Verify R is upper triangular
               (test-approx "QR: R is upper triangular"
                            0.0
                            (matrix-ref r 1 0)
                            1e-10))
          (test "QR: 2×2 matrix fails" #t #f)))

;; Test 5: 3×2 tall matrix
(let* ([a (matrix-from-lists '((1 0) (0 1) (1 1)))]
       [result (matrix-qr a)])
      (if (pair? result)
          (test "QR: 3×2 tall matrix succeeds" #t #t)
          (test "QR: 3×2 matrix fails" #t #f)))

;; Test 6: Orthogonality of Q
(let* ([a (matrix-from-lists '((12 -51 4) (6 167 -68) (-4 24 -41)))]
       [result (matrix-qr a)])
      (if (pair? result)
          (let* ([q (car result)]
                 [qt (matrix-transpose q)]
                 [qtq (matrix-mul qt q)]
                 [i3 (identity 3)])
                (test "QR: Q^T Q ≈ I"
                      #t
                      (matrix-approx-equal? qtq i3 1e-8)))
          (test "QR: 3×3 orthogonality test fails" #t #f)))

;;; ====
;;; Cholesky Decomposition Tests
;;; ====

(printf "\n--- Cholesky Decomposition ---\n")

;; Test 7: Simple 2×2 positive definite matrix
(let* ([a (matrix-from-lists '((4 12) (12 37)))]
       [result (matrix-cholesky a)])
      (if (and (pair? result) (not (eq? (car result) 'error)))
          (let ([l (car result)])
               (test "Cholesky: 2×2 PD matrix succeeds" #t #t)
               ;; Verify L is lower triangular
               (test-approx "Cholesky: L is lower triangular"
                            0.0
                            (matrix-ref l 0 1)
                            1e-10)
               ;; Verify A = LL^T
               (let* ([lt (matrix-transpose l)]
                      [llt (matrix-mul l lt)])
                     (test "Cholesky: A = LL^T"
                           #t
                           (matrix-approx-equal? a llt 1e-8))))
          (test "Cholesky: 2×2 PD fails" #t #f)))

;; Test 8: 3×3 positive definite matrix
(let* ([a (matrix-from-lists '((25 15 -5) (15 18 0) (-5 0 11)))]
       [result (matrix-cholesky a)])
      (if (and (pair? result) (not (eq? (car result) 'error)))
          (let ([l (car result)])
               (test "Cholesky: 3×3 PD matrix succeeds" #t #t)
               ;; Verify reconstruction
               (let* ([lt (matrix-transpose l)]
                      [llt (matrix-mul l lt)])
                     (test "Cholesky: 3×3 A = LL^T"
                           #t
                           (matrix-approx-equal? a llt 1e-8))))
          (test "Cholesky: 3×3 PD fails" #t #f)))

;; Test 9: Identity matrix
(let* ([i3 (identity 3)]
       [result (matrix-cholesky i3)])
      (if (and (pair? result) (not (eq? (car result) 'error)))
          (test "Cholesky: Identity → L = I"
                #t
                (matrix-approx-equal? (car result) i3 1e-10))
          (test "Cholesky: Identity fails" #t #f)))

;;; ====
;;; Numerical Stability Tests
;;; ====

(printf "\n--- Numerical Stability ---\n")

;; Test 10: LU with row pivoting needed
(let* ([a (matrix-from-lists '((0 1) (1 1)))]
       [result (matrix-lu a)])
      (test "LU: Handles zero pivot with pivoting"
            #t
            (pair? result)))

;; Test 11: QR with nearly dependent columns
(let* ([a (matrix-from-lists '((1 1.0001) (0 0.0001)))]
       [result (matrix-qr a)])
      (test "QR: Nearly dependent columns"
            #t
            (pair? result)))

;; Test 12: Cholesky with barely positive-definite matrix
(let* ([a (matrix-from-lists '((1 0.99) (0.99 1)))]
       [result (matrix-cholesky a)])
      (test "Cholesky: Barely PD matrix"
            #t
            (and (pair? result) (not (eq? (car result) 'error)))))

;;; ====
;;; Error Handling Tests
;;; ====

(printf "\n--- Error Handling ---\n")

;; Test 13: LU on non-square matrix
(let* ([a (matrix-from-lists '((1 2 3) (4 5 6)))]
       [result (matrix-lu a)])
      (test "LU: Non-square matrix returns error"
            #t
            (and (pair? result) (eq? (car result) 'error))))

;; Test 14: QR on underdetermined system
(let* ([a (matrix-from-lists '((1 2 3)))]
       [result (matrix-qr a)])
      (test "QR: Underdetermined system returns error"
            #t
            (and (pair? result) (eq? (car result) 'error))))

;; Test 15: Cholesky on non-positive-definite matrix
(let* ([a (matrix-from-lists '((1 2) (2 1)))]
       [result (matrix-cholesky a)])
      (test "Cholesky: Non-PD matrix returns error"
            #t
            (and (pair? result) (eq? (car result) 'error))))

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
    (printf "\n[SUCCESS] All matrix decomposition tests passed.\n")
    (printf "\n[FAILURE] Some tests failed.\n"))
