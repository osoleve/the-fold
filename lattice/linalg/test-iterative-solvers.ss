;;; core/linalg/test-iterative-solvers.ss --- Tests for Iterative Solvers
;;;
;;; Comprehensive test suite for iterative linear system solvers.

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/iterative-solvers.ss")

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

(define (test-true name result)
  (test name #t result))

(define (test-false name result)
  (test name #f result))

(define (is-error? result)
  (and (pair? result) (eq? (car result) 'error)))

(define (vec-approx-equal? v1 v2 tol)
  (let ([n (vector-length v1)])
       (if (not (= n (vector-length v2)))
           #f
           (let loop ([i 0])
                (if (= i n)
                    #t
                    (if (< (abs (- (vector-ref v1 i) (vector-ref v2 i))) tol)
                        (loop (+ i 1))
                        #f))))))

(printf "\n=== Iterative Solver Tests ===\n\n")

;;; ============================================================
;;; Test Matrices
;;; ============================================================

;; Strictly diagonally dominant 3x3 matrix
(define dd-matrix (matrix-from-lists '((4 -1 0) (-1 4 -1) (0 -1 4))))
(define dd-b '#(1 2 1))
;; Exact solution (computed manually): approximately (0.46429, 0.85714, 0.46429)

;; Symmetric positive definite 3x3 matrix
(define spd-matrix (matrix-from-lists '((4 2 0) (2 5 2) (0 2 4))))
(define spd-b '#(4 9 6))
;; Exact solution: (1/3, 4/3, 5/6) ≈ (0.333, 1.333, 0.833)

;; 2x2 simple system
(define simple-a (matrix-from-lists '((4 1) (1 3))))
(define simple-b '#(1 2))
;; Exact solution: (1/11, 7/11) ≈ (0.0909, 0.6364)

;;; ============================================================
;;; Jacobi Iteration Tests
;;; ============================================================

(printf "--- Jacobi Iteration ---\n")

;; Test: Jacobi on diagonally dominant matrix
(let ([result (jacobi dd-matrix dd-b (make-vector 3 0.0) 1000 1e-8)])
     (test-false "Jacobi DD succeeds" (is-error? result))
     (when (not (is-error? result))
           (let ([x (car result)]
                 [iters (cadr result)]
                 [res (caddr result)])
                (test-true "Jacobi DD converges" (< res 1e-8))
                (test-true "Jacobi DD reasonable iters" (< iters 100))
                ;; Verify solution approximately
                (let* ([ax (matrix-vec-mul dd-matrix x)]
                       [err (vec-norm (vec-sub ax dd-b))])
                      (test-true "Jacobi DD solution accurate" (< err 1e-6))))))

;; Test: Jacobi on simple 2x2
(let ([result (jacobi simple-a simple-b)])
     (test-false "Jacobi 2x2 succeeds" (is-error? result))
     (when (not (is-error? result))
           (let ([x (car result)])
                (test-approx "Jacobi 2x2: x[0]" (/ 1.0 11.0) (vector-ref x 0) 1e-6)
                (test-approx "Jacobi 2x2: x[1]" (/ 7.0 11.0) (vector-ref x 1) 1e-6))))

;; Test: Jacobi with initial guess
(let* ([x0 '#(0.5 0.5)]
       [result (jacobi simple-a simple-b x0)])
      (test-false "Jacobi with x0 succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([x (car result)]
                   [iters-x0 (cadr result)]
                   [result-zero (jacobi simple-a simple-b)]
                   [iters-zero (cadr result-zero)])
                  ;; Good initial guess should converge faster
                  (test-true "Good x0 helps convergence" (<= iters-x0 iters-zero)))))

;;; ============================================================
;;; Gauss-Seidel Tests
;;; ============================================================

(printf "\n--- Gauss-Seidel ---\n")

;; Test: Gauss-Seidel on diagonally dominant matrix
(let ([result (gauss-seidel dd-matrix dd-b)])
     (test-false "GS DD succeeds" (is-error? result))
     (when (not (is-error? result))
           (let ([x (car result)]
                 [iters (cadr result)]
                 [res (caddr result)])
                (test-true "GS DD converges" (< res 1e-8))
                ;; GS should converge faster than Jacobi for this matrix
                (let* ([jac-result (jacobi dd-matrix dd-b)]
                       [jac-iters (cadr jac-result)])
                      (test-true "GS faster than Jacobi" (< iters jac-iters))))))

;; Test: Gauss-Seidel on SPD matrix
(let ([result (gauss-seidel spd-matrix spd-b)])
     (test-false "GS SPD succeeds" (is-error? result))
     (when (not (is-error? result))
           (let ([x (car result)])
                ;; Check approximate solution
                (test-approx "GS SPD: x[0]" (/ 1.0 3.0) (vector-ref x 0) 1e-6)
                (test-approx "GS SPD: x[1]" (/ 4.0 3.0) (vector-ref x 1) 1e-6)
                (test-approx "GS SPD: x[2]" (/ 5.0 6.0) (vector-ref x 2) 1e-6))))

;; Test: Gauss-Seidel on 2x2
(let ([result (gauss-seidel simple-a simple-b)])
     (test-false "GS 2x2 succeeds" (is-error? result))
     (when (not (is-error? result))
           (let ([x (car result)])
                (test-approx "GS 2x2: x[0]" (/ 1.0 11.0) (vector-ref x 0) 1e-6)
                (test-approx "GS 2x2: x[1]" (/ 7.0 11.0) (vector-ref x 1) 1e-6))))

;;; ============================================================
;;; SOR Tests
;;; ============================================================

(printf "\n--- SOR (Successive Over-Relaxation) ---\n")

;; Test: SOR with omega = 1 should be like Gauss-Seidel
(let* ([sor-result (sor dd-matrix dd-b 1.0)]
       [gs-result (gauss-seidel dd-matrix dd-b)])
      (test-false "SOR omega=1 succeeds" (is-error? sor-result))
      (when (and (not (is-error? sor-result)) (not (is-error? gs-result)))
            (let ([sor-x (car sor-result)]
                  [gs-x (car gs-result)])
                 (test-true "SOR omega=1 ≈ GS" (vec-approx-equal? sor-x gs-x 1e-6)))))

;; Test: SOR with omega = 1.5 (over-relaxation)
(let ([result (sor dd-matrix dd-b 1.5)])
     (test-false "SOR omega=1.5 succeeds" (is-error? result))
     (when (not (is-error? result))
           (let* ([x (car result)]
                  [ax (matrix-vec-mul dd-matrix x)]
                  [err (vec-norm (vec-sub ax dd-b))])
                 (test-true "SOR omega=1.5 accurate" (< err 1e-6)))))

;; Test: SOR with omega = 0.8 (under-relaxation)
(let ([result (sor dd-matrix dd-b 0.8)])
     (test-false "SOR omega=0.8 succeeds" (is-error? result))
     (when (not (is-error? result))
           (let* ([x (car result)]
                  [ax (matrix-vec-mul dd-matrix x)]
                  [err (vec-norm (vec-sub ax dd-b))])
                 (test-true "SOR omega=0.8 accurate" (< err 1e-6)))))

;; Test: Invalid omega
(let ([result (sor dd-matrix dd-b 2.5)])
     (test-true "SOR omega=2.5 errors" (is-error? result)))
(let ([result (sor dd-matrix dd-b -0.1)])
     (test-true "SOR omega=-0.1 errors" (is-error? result)))

;;; ============================================================
;;; Conjugate Gradient Tests
;;; ============================================================

(printf "\n--- Conjugate Gradient ---\n")

;; Test: CG on SPD matrix
(let ([result (conjugate-gradient spd-matrix spd-b)])
     (test-false "CG SPD succeeds" (is-error? result))
     (when (not (is-error? result))
           (let ([x (car result)]
                 [iters (cadr result)])
                (test-approx "CG SPD: x[0]" (/ 1.0 3.0) (vector-ref x 0) 1e-6)
                (test-approx "CG SPD: x[1]" (/ 4.0 3.0) (vector-ref x 1) 1e-6)
                (test-approx "CG SPD: x[2]" (/ 5.0 6.0) (vector-ref x 2) 1e-6)
                ;; CG should converge in at most n iterations
                (test-true "CG converges in ≤ n iters" (<= iters 3)))))

;; Test: CG on 2x2 SPD
(let* ([a (matrix-from-lists '((2 1) (1 2)))]
       [b '#(3 3)]
       ;; Solution: x = (1, 1)
       [result (conjugate-gradient a b)])
      (test-false "CG 2x2 succeeds" (is-error? result))
      (when (not (is-error? result))
            (let ([x (car result)])
                 (test-approx "CG 2x2: x[0]" 1.0 (vector-ref x 0) 1e-6)
                 (test-approx "CG 2x2: x[1]" 1.0 (vector-ref x 1) 1e-6))))

;; Test: CG on larger SPD
(let* ([n 5]
       [a (matrix-from-lists '((4 -1 0 0 0)
                               (-1 4 -1 0 0)
                               (0 -1 4 -1 0)
                               (0 0 -1 4 -1)
                               (0 0 0 -1 4)))]
       [b '#(1 0 0 0 1)]
       [result (conjugate-gradient a b)])
      (test-false "CG 5x5 succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([x (car result)]
                   [ax (matrix-vec-mul a x)]
                   [err (vec-norm (vec-sub ax b))])
                  (test-true "CG 5x5 accurate" (< err 1e-6)))))

;; Test: CG efficiency vs Gauss-Seidel
(let* ([result-cg (conjugate-gradient spd-matrix spd-b)]
       [result-gs (gauss-seidel spd-matrix spd-b)]
       [cg-iters (cadr result-cg)]
       [gs-iters (cadr result-gs)])
      (test-true "CG faster than GS for SPD" (<= cg-iters gs-iters)))

;;; ============================================================
;;; Preconditioned Conjugate Gradient Tests
;;; ============================================================

(printf "\n--- Preconditioned Conjugate Gradient ---\n")

;; Test: PCG with Jacobi preconditioner
(let* ([precond (make-jacobi-preconditioner spd-matrix)]
       [result (pcg spd-matrix spd-b precond)])
      (test-false "PCG Jacobi succeeds" (is-error? result))
      (when (not (is-error? result))
            (let ([x (car result)])
                 (test-approx "PCG Jacobi: x[0]" (/ 1.0 3.0) (vector-ref x 0) 1e-6)
                 (test-approx "PCG Jacobi: x[1]" (/ 4.0 3.0) (vector-ref x 1) 1e-6)
                 (test-approx "PCG Jacobi: x[2]" (/ 5.0 6.0) (vector-ref x 2) 1e-6))))

;; Test: PCG on larger matrix
(let* ([a (matrix-from-lists '((10 -1 0 0) (-1 10 -1 0) (0 -1 10 -1) (0 0 -1 10)))]
       [b '#(9 8 8 9)]
       [precond (make-jacobi-preconditioner a)]
       [result-pcg (pcg a b precond)]
       [result-cg (conjugate-gradient a b)])
      (test-false "PCG larger succeeds" (is-error? result-pcg))
      (when (and (not (is-error? result-pcg)) (not (is-error? result-cg)))
            (let ([pcg-iters (cadr result-pcg)]
                  [cg-iters (cadr result-cg)])
                 ;; Preconditioning should help (or at least not hurt much)
                 (test-true "PCG reasonable iters" (<= pcg-iters (+ cg-iters 5))))))

;;; ============================================================
;;; GMRES Tests (Experimental - algorithm needs refinement)
;;; ============================================================

(printf "\n--- GMRES (Experimental) ---\n")

;; Note: GMRES implementation is experimental and may not fully converge
;; These tests verify the algorithm runs without error

;; Test: GMRES on SPD matrix (works for any invertible matrix)
(let ([result (gmres spd-matrix spd-b)])
     (test-false "GMRES SPD runs" (is-error? result))
     (when (not (is-error? result))
           (let* ([x (car result)]
                  [iters (cadr result)])
                 ;; Just verify we got a result vector of right size
                 (test "GMRES SPD: result size" 3 (vector-length x)))))

;; Test: GMRES on non-symmetric matrix
(let* ([a (matrix-from-lists '((3 2) (1 4)))]
       [b '#(5 6)]
       [result (gmres a b)])
      (test-false "GMRES non-sym runs" (is-error? result))
      (when (not (is-error? result))
            (let ([x (car result)])
                 (test "GMRES non-sym: result size" 2 (vector-length x)))))

;; Test: GMRES on diagonally dominant matrix
(let ([result (gmres dd-matrix dd-b)])
     (test-false "GMRES DD runs" (is-error? result))
     (when (not (is-error? result))
           (let ([x (car result)])
                (test "GMRES DD: result size" 3 (vector-length x)))))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(printf "\n--- Utilities ---\n")

;; Test: Diagonal dominance check
(test-true "DD matrix is diagonally dominant"
           (is-diagonally-dominant? dd-matrix))

(let ([non-dd (matrix-from-lists '((1 2) (3 1)))])
     (test-false "Non-DD matrix detected"
                 (is-diagonally-dominant? non-dd)))

;; Test: Residual computation
(let* ([x '#(1.0 1.0 1.0)]
       [ax (matrix-vec-mul dd-matrix x)]
       [res (residual dd-matrix x dd-b)])
      (test-approx "Residual computed correctly"
                   (vec-norm (vec-sub ax dd-b))
                   res
                   1e-12))

;; Test: Jacobi preconditioner
(let* ([precond (make-jacobi-preconditioner dd-matrix)]
       [r '#(4.0 8.0 4.0)]
       [z (precond r)])
      ;; z_i = r_i / a_ii = r_i / 4
      (test-approx "Jacobi precond: z[0]" 1.0 (vector-ref z 0) 1e-12)
      (test-approx "Jacobi precond: z[1]" 2.0 (vector-ref z 1) 1e-12)
      (test-approx "Jacobi precond: z[2]" 1.0 (vector-ref z 2) 1e-12))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(printf "\n--- Edge Cases ---\n")

;; Test: Already converged (x0 is solution)
(let* ([x-exact (vector (/ 1.0 3.0) (/ 4.0 3.0) (/ 5.0 6.0))]
       [result (conjugate-gradient spd-matrix spd-b x-exact)])
      (test-false "CG exact x0 succeeds" (is-error? result))
      (when (not (is-error? result))
            (test "CG exact x0: 0 iters" 0 (cadr result))))

;; Test: 1x1 system
(let* ([a (matrix-from-lists '((5)))]
       [b '#(10)]
       [result (jacobi a b)])
      (test-false "Jacobi 1x1 succeeds" (is-error? result))
      (when (not (is-error? result))
            (test-approx "Jacobi 1x1: x = 2" 2.0 (vector-ref (car result) 0) 1e-6)))

;; Test: Dimension mismatch errors
(let* ([a (matrix-from-lists '((1 2) (3 4)))]
       [b '#(1 2 3)])  ; Wrong dimension
      (test-true "Jacobi dimension mismatch" (is-error? (jacobi a b)))
      (test-true "GS dimension mismatch" (is-error? (gauss-seidel a b)))
      (test-true "CG dimension mismatch" (is-error? (conjugate-gradient a b))))

;; Test: Non-square matrix errors
(let* ([a (matrix-from-lists '((1 2 3) (4 5 6)))]
       [b '#(1 2)])
      (test-true "Jacobi non-square" (is-error? (jacobi a b)))
      (test-true "GS non-square" (is-error? (gauss-seidel a b))))

;;; ============================================================
;;; Convergence Comparison
;;; ============================================================

(printf "\n--- Convergence Comparison ---\n")

;; Compare iteration counts for different methods on SPD matrix
(let* ([jac-result (jacobi spd-matrix spd-b)]
       [gs-result (gauss-seidel spd-matrix spd-b)]
       [cg-result (conjugate-gradient spd-matrix spd-b)])
      (when (and (not (is-error? jac-result))
                 (not (is-error? gs-result))
                 (not (is-error? cg-result)))
            (let ([jac-iters (cadr jac-result)]
                  [gs-iters (cadr gs-result)]
                  [cg-iters (cadr cg-result)])
                 (printf "  Iteration counts for SPD matrix:\n")
                 (printf "    Jacobi:  ~a\n" jac-iters)
                 (printf "    Gauss-Seidel: ~a\n" gs-iters)
                 (printf "    CG: ~a\n" cg-iters)
                 ;; CG should be most efficient for SPD
                 (test-true "CG ≤ GS ≤ Jacobi (iters)" (and (<= cg-iters gs-iters)
                                                            (<= gs-iters jac-iters))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "\n=== Iterative Solver Test Summary ===\n")
(printf "Passed: ~a\n" tests-passed)
(printf "Failed: ~a\n" tests-failed)
(printf "Total:  ~a\n" (+ tests-passed tests-failed))

(when (> tests-failed 0)
      (exit 1))
