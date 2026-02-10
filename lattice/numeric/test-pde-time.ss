;;; lattice/numeric/test-pde-time.ss — Tests for PDE time stepping
;;; @requires test-framework pde-time

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/numeric/pde-time.ss")

;;; ============================================================
;;; Test Helpers
;;; ============================================================

;; Create simple test matrices in CSR format
;; For 1D heat equation: du/dt = d²u/dx² on [0,1]
;; Discretized: du_i/dt = (u_{i-1} - 2u_i + u_{i+1}) / h²

(define (make-1d-laplacian n)
  ;; Returns CSR matrix for 1D discrete Laplacian (scaled by -1/h²)
  ;; Interior: -2 on diagonal, 1 on off-diagonals
  ;; Boundary: Dirichlet (handled separately)
  (let* ([h (/ 1.0 (+ n 1))]
         [h2 (* h h)]
         ;; Tridiagonal structure: each row has up to 3 nonzeros
         [max-nnz (* 3 n)]
         [row-ptrs (make-vector (+ n 1) 0)]
         [col-indices (make-vector max-nnz 0)]
         [values (make-vector max-nnz 0.0)]
         [nnz-box (cons 0 #f)])
    ;; Fill CSR structure
    (do ([i 0 (+ i 1)])
        ((= i n))
      (let ([nnz (car nnz-box)])
        (vector-set! row-ptrs i nnz)
        ;; Left neighbor (if not first row)
        (when (> i 0)
          (vector-set! col-indices (car nnz-box) (- i 1))
          (vector-set! values (car nnz-box) (/ 1.0 h2))
          (set-car! nnz-box (+ (car nnz-box) 1)))
        ;; Diagonal
        (vector-set! col-indices (car nnz-box) i)
        (vector-set! values (car nnz-box) (/ -2.0 h2))
        (set-car! nnz-box (+ (car nnz-box) 1))
        ;; Right neighbor (if not last row)
        (when (< i (- n 1))
          (vector-set! col-indices (car nnz-box) (+ i 1))
          (vector-set! values (car nnz-box) (/ 1.0 h2))
          (set-car! nnz-box (+ (car nnz-box) 1)))))
    ;; Final row pointer
    (vector-set! row-ptrs n (car nnz-box))
    ;; Trim arrays
    (let ([nnz (car nnz-box)]
          [trimmed-cols (make-vector (car nnz-box) 0)]
          [trimmed-vals (make-vector (car nnz-box) 0.0)])
      (do ([i 0 (+ i 1)])
          ((= i nnz))
        (vector-set! trimmed-cols i (vector-ref col-indices i))
        (vector-set! trimmed-vals i (vector-ref values i)))
      (list 'sparse-csr n n row-ptrs trimmed-cols trimmed-vals))))

(define (make-identity-csr n)
  (let* ([row-ptrs (make-vector (+ n 1) 0)]
         [col-indices (make-vector n 0)]
         [values (make-vector n 1.0)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (vector-set! row-ptrs i i)
      (vector-set! col-indices i i))
    (vector-set! row-ptrs n n)
    ;; CSR format: (sparse-csr rows cols row-ptrs col-indices values)
    (list 'sparse-csr n n row-ptrs col-indices values)))

(define (make-zero-vector n)
  (make-vector n 0.0))

(define (make-initial-sine n)
  ;; Initial condition: sin(pi*x) on interior nodes
  (let ([u (make-vector n 0.0)]
        [h (/ 1.0 (+ n 1))])
    (do ([i 0 (+ i 1)])
        ((= i n) u)
      (let ([x (* (+ i 1) h)])
        (vector-set! u i (sin (* 3.141592653589793 x)))))))

;;; ============================================================
;;; Test Groups
;;; ============================================================

(test-group "vector-operations"

  (define-test "vec-add"
    (let ([v1 (vector 1.0 2.0 3.0)]
          [v2 (vector 4.0 5.0 6.0)])
      (let ([result (pde-vec-add v1 v2)])
        (assert-true (< (abs (- (vector-ref result 0) 5.0)) 1e-10))
        (assert-true (< (abs (- (vector-ref result 1) 7.0)) 1e-10))
        (assert-true (< (abs (- (vector-ref result 2) 9.0)) 1e-10)))))

  (define-test "vec-scale"
    (let ([v (vector 1.0 2.0 3.0)])
      (let ([result (pde-vec-scale 2.0 v)])
        (assert-true (< (abs (- (vector-ref result 0) 2.0)) 1e-10))
        (assert-true (< (abs (- (vector-ref result 1) 4.0)) 1e-10)))))

  (define-test "vec-dot"
    (let ([v1 (vector 1.0 2.0 3.0)]
          [v2 (vector 4.0 5.0 6.0)])
      (assert-true (< (abs (- (pde-vec-dot v1 v2) 32.0)) 1e-10))))

  (define-test "vec-norm"
    (let ([v (vector 3.0 4.0)])
      (assert-true (< (abs (- (pde-vec-norm v) 5.0)) 1e-10)))))

(test-group "forward-euler"

  (define-test "decay-equation"
    ;; du/dt = -u has solution u(t) = u0 * exp(-t)
    ;; Forward Euler: u^{n+1} = u^n - dt * u^n = (1-dt) * u^n
    (let* ([f (lambda (u t) (pde-vec-scale -1.0 u))]
           [u0 (vector 1.0)]
           [dt 0.01])
      (let loop ([u u0] [t 0.0] [steps 0])
        (if (>= steps 100)
            ;; After 100 steps: t=1.0, u ≈ exp(-1) ≈ 0.3679
            (let ([expected (exp -1.0)])
              (assert-true (< (abs (- (vector-ref u 0) expected)) 0.02)))
            (loop (forward-euler-step f u t dt) (+ t dt) (+ steps 1))))))

  (define-test "constant-rhs"
    ;; du/dt = 1 has solution u(t) = u0 + t
    (let* ([f (lambda (u t) (vector 1.0))]
           [u0 (vector 0.0)]
           [dt 0.1])
      (let loop ([u u0] [t 0.0] [steps 0])
        (if (>= steps 10)
            ;; After 10 steps: t=1.0, u = 1.0
            (assert-true (< (abs (- (vector-ref u 0) 1.0)) 1e-10))
            (loop (forward-euler-step f u t dt) (+ t dt) (+ steps 1)))))))

(test-group "cfl-conditions"

  (define-test "cfl-parabolic"
    ;; For h=0.1, alpha=1, dim=1: dt < 0.9 * 0.01 / 2 = 0.0045
    (let ([dt (cfl-parabolic 0.1 1.0 1)])
      (assert-true (> dt 0))
      (assert-true (< dt 0.005))))

  (define-test "cfl-hyperbolic"
    ;; For h=0.1, c=1: dt < 0.9 * 0.1 = 0.09
    (let ([dt (cfl-hyperbolic 0.1 1.0)])
      (assert-true (> dt 0.08))
      (assert-true (< dt 0.1)))))

(test-group "method-of-lines"

  (define-test "mol-rk4-accuracy"
    ;; RK4 should be more accurate than Euler for same dt
    ;; Test on du/dt = -u
    (let* ([n 1]
           ;; 1x1 matrix with A[0,0] = -1 (CSR format: rows cols row-ptrs col-indices values)
           [A (list 'sparse-csr 1 1
                    (vector 0 1)
                    (vector 0)
                    (vector -1.0))]
           [b (make-zero-vector n)]
           [u0 (vector 1.0)]
           [dt 0.1])
      (let ([u-euler (mol-euler-step A b u0 0 dt)]
            [u-rk4 (mol-rk4-step A b u0 0 dt)]
            [exact (exp -0.1)])
        ;; RK4 should be closer to exact
        (let ([err-euler (abs (- (vector-ref u-euler 0) exact))]
              [err-rk4 (abs (- (vector-ref u-rk4 0) exact))])
          (assert-true (< err-rk4 err-euler)))))))

(test-group "sparse-utilities"

  (define-test "identity-matrix"
    (let ([I (sparse-csr-identity 3)])
      (assert-equal 3 (sparse-csr-rows I))
      (assert-equal 3 (sparse-csr-nnz I))))

  (define-test "scale-matrix"
    (let* ([I (sparse-csr-identity 2)]
           [scaled (sparse-csr-scale 2.0 I)])
      (let ([vals (sparse-csr-values scaled)])
        (assert-true (< (abs (- (vector-ref vals 0) 2.0)) 1e-10))
        (assert-true (< (abs (- (vector-ref vals 1) 2.0)) 1e-10)))))

  (define-test "diagonal-extraction"
    (let ([I (sparse-csr-identity 3)])
      (let ([diag (sparse-csr-diagonal-vec I)])
        (assert-equal 3 (vector-length diag))
        (assert-true (< (abs (- (vector-ref diag 0) 1.0)) 1e-10))))))

(test-group "implicit-solvers"

  (define-test "backward-euler-decay"
    ;; Test backward Euler on simple decay: du/dt = -u
    ;; Exact solution: u(t) = exp(-t)
    ;; Backward Euler is unconditionally stable, can use large dt
    (let* ([n 1]
           [A (list 'sparse-csr 1 1
                    (vector 0 1)
                    (vector 0)
                    (vector -1.0))]
           [b (make-zero-vector n)]
           [u0 (vector 1.0)]
           [dt 0.1])
      (let-values ([(u-new residual iters)
                    (backward-euler-identity-step A u0 b dt 1e-10 100)])
        ;; Backward Euler: u^{n+1} = u^n + dt * (-u^{n+1})
        ;; => (1 + dt) * u^{n+1} = u^n
        ;; => u^{n+1} = u^n / (1 + dt) = 1 / 1.1 ≈ 0.909
        (let ([expected (/ 1.0 1.1)])
          (assert-true (< (abs (- (vector-ref u-new 0) expected)) 1e-6))
          (assert-true (< residual 1e-8))))))

  (define-test "backward-euler-heat-1d"
    ;; Test backward Euler on 1D heat equation with Laplacian
    ;; du/dt = A*u where A is discrete Laplacian (SPD system)
    (let* ([n 5]
           [A (make-1d-laplacian n)]
           [b (make-zero-vector n)]
           [u0 (make-initial-sine n)]
           [dt 0.001])
      ;; Take one step
      (let-values ([(u-new residual iters)
                    (backward-euler-identity-step A u0 b dt 1e-10 100)])
        ;; Solution should decrease (heat diffuses)
        (let ([norm-before (pde-vec-norm u0)]
              [norm-after (pde-vec-norm u-new)])
          (assert-true (< norm-after norm-before))
          ;; Solver should converge
          (assert-true (< residual 1e-6))))))

  (define-test "crank-nicolson-decay"
    ;; Test Crank-Nicolson on decay equation
    ;; CN is 2nd order accurate
    (let* ([n 1]
           [A (list 'sparse-csr 1 1
                    (vector 0 1)
                    (vector 0)
                    (vector -1.0))]
           [b (make-zero-vector n)]
           [u0 (vector 1.0)]
           [dt 0.1])
      (let-values ([(u-new residual iters)
                    (crank-nicolson-identity-step A u0 b dt 1e-10 100)])
        ;; CN: u^{n+1} = u^n + dt/2 * (-u^n - u^{n+1})
        ;; => (1 + dt/2) * u^{n+1} = (1 - dt/2) * u^n
        ;; => u^{n+1} = u^n * (1 - dt/2) / (1 + dt/2)
        (let ([expected (* 1.0 (/ (- 1 0.05) (+ 1 0.05)))])  ; (1-0.05)/(1+0.05)
          (assert-true (< (abs (- (vector-ref u-new 0) expected)) 1e-6))))))

  (define-test "crank-nicolson-more-accurate-than-backward-euler"
    ;; CN (2nd order) should be more accurate than BE (1st order)
    (let* ([n 1]
           [A (list 'sparse-csr 1 1
                    (vector 0 1)
                    (vector 0)
                    (vector -1.0))]
           [b (make-zero-vector n)]
           [u0 (vector 1.0)]
           [dt 0.1]
           [exact (exp -0.1)])
      (let-values ([(u-be res-be iter-be)
                    (backward-euler-identity-step A u0 b dt 1e-10 100)]
                   [(u-cn res-cn iter-cn)
                    (crank-nicolson-identity-step A u0 b dt 1e-10 100)])
        (let ([err-be (abs (- (vector-ref u-be 0) exact))]
              [err-cn (abs (- (vector-ref u-cn 0) exact))])
          ;; CN should have smaller error
          (assert-true (< err-cn err-be))))))

  (define-test "implicit-stability-large-dt"
    ;; Backward Euler should remain stable with large dt
    ;; (where forward Euler would blow up)
    (let* ([n 5]
           [A (make-1d-laplacian n)]
           [b (make-zero-vector n)]
           [u0 (make-initial-sine n)]
           ;; Large dt that would violate CFL for explicit methods
           [h (/ 1.0 (+ n 1))]
           [dt-cfl (cfl-parabolic h 1.0 1)]
           [dt (* 10 dt-cfl)])  ; 10x the CFL limit
      ;; Take several steps with backward Euler
      (let loop ([u u0] [steps 0])
        (if (>= steps 5)
            ;; Solution should remain bounded (not blow up)
            (let ([norm (pde-vec-norm u)])
              (assert-true (< norm 10.0))  ; Reasonable bound
              (assert-true (> norm 0.0)))  ; Not collapsed to zero
            (let-values ([(u-new res iters)
                          (backward-euler-identity-step A u b dt 1e-8 200)])
              (loop u-new (+ steps 1))))))))

(test-group "adaptive-stepping"

  (define-test "adaptive-euler-reduces-error"
    ;; Adaptive stepping should keep error below tolerance
    (let* ([f (lambda (u t) (pde-vec-scale -1.0 u))]
           [u0 (vector 1.0)]
           [dt 0.1]
           [tol 0.001]
           [safety 0.9])
      (let-values ([(u-new dt-new error)
                    (adaptive-euler-step f u0 0.0 dt tol safety)])
        ;; Error should be estimated
        (assert-true (>= error 0))
        ;; New dt should be positive
        (assert-true (> dt-new 0)))))

  (define-test "integrate-adaptive-reaches-end"
    ;; Adaptive integration should reach the target time
    (let* ([f (lambda (u t) (pde-vec-scale -1.0 u))]
           [u0 (vector 1.0)]
           [t0 0.0]
           [t-end 1.0]
           [dt0 0.1]
           [tol 0.01]
           [safety 0.9]
           [max-steps 500]
           [result (integrate-adaptive f u0 t0 t-end dt0 tol safety max-steps)])
      ;; Should have results
      (assert-true (> (length result) 1))
      ;; Final time should be at or near t-end
      (let* ([final (car (reverse result))]
             [t-final (car final)])
        (assert-true (>= t-final (- t-end 0.01))))
      ;; Final value should be reasonable (close to exp(-1))
      (let* ([final (car (reverse result))]
             [u-final (cdr final)]
             [exact (exp -1.0)])
        (assert-true (< (abs (- (vector-ref u-final 0) exact)) 0.1))))))

(test-group "integration"

  (define-test "integrate-decay"
    ;; Integrate du/dt = -u from t=0 to t=1
    (let* ([f (lambda (u t) (pde-vec-scale -1.0 u))]
           [u0 (vector 1.0)]
           [stepper (lambda (u t dt) (forward-euler-step f u t dt))]
           [result (integrate-pde stepper u0 0.0 1.0 0.01 200)])
      ;; Should have multiple time points
      (assert-true (> (length result) 50))
      ;; Final value should be close to exp(-1)
      (let* ([final (car (reverse result))]
             [t-final (car final)]
             [u-final (cdr final)])
        (assert-true (< (abs (- t-final 1.0)) 0.01))
        (assert-true (< (abs (- (vector-ref u-final 0) (exp -1.0))) 0.05))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
