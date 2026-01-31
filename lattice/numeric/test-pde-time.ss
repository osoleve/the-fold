;;; lattice/numeric/test-pde-time.ss — Tests for PDE time stepping
;;; @requires test-framework pde-time

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
