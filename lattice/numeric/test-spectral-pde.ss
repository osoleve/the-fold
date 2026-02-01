;;; test-spectral-pde.ss — Tests for spectral PDE methods
;;; @requires test-framework spectral-pde

(load "core/testing/test-framework.ss")
(load "lattice/numeric/spectral-pde.ss")

(define pi-val (pi-value))

;;; ============================================================
;;; Test Group: Fourier Wavenumbers
;;; ============================================================

(test-group "fourier-wavenumbers"

  (define-test "wavenumbers for N=4 on [0, 2π)"
    ;; k should be [0, 1, -2, -1] * 1 = [0, 1, -2, -1]
    (let ([k (fourier-wavenumbers 4 (* 2 pi-val))])
      (assert-true (< (abs (- (vector-ref k 0) 0.0)) 1e-10))
      (assert-true (< (abs (- (vector-ref k 1) 1.0)) 1e-10))
      (assert-true (< (abs (- (vector-ref k 2) -2.0)) 1e-10))
      (assert-true (< (abs (- (vector-ref k 3) -1.0)) 1e-10))))

  (define-test "wavenumbers for N=8"
    (let ([k (fourier-wavenumbers 8 (* 2 pi-val))])
      (assert-equal 8 (vector-length k))
      ;; First entry should be 0
      (assert-true (< (abs (vector-ref k 0)) 1e-10))))
)

;;; ============================================================
;;; Test Group: Fourier Differentiation
;;; ============================================================

(test-group "fourier-diff"

  (define-test "derivative of sin(x) is cos(x)"
    ;; On [0, 2π) with N=64
    (let* ([n 64]
           [L (* 2 pi-val)]
           [grid (periodic-grid n L)]
           [u (sample-function sin grid)]
           [du (fourier-diff u L)]
           [du-exact (sample-function cos grid)]
           [err (spectral-error-estimate du du-exact)])
      ;; Spectral accuracy: should be very small for smooth functions
      (assert-true (< err 1e-10))))

  (define-test "derivative of cos(2x) is -2sin(2x)"
    (let* ([n 64]
           [L (* 2 pi-val)]
           [grid (periodic-grid n L)]
           [u (sample-function (lambda (x) (cos (* 2 x))) grid)]
           [du (fourier-diff u L)]
           [du-exact (sample-function (lambda (x) (* -2 (sin (* 2 x)))) grid)]
           [err (spectral-error-estimate du du-exact)])
      (assert-true (< err 1e-10))))

  (define-test "second derivative of sin(x) is -sin(x)"
    (let* ([n 64]
           [L (* 2 pi-val)]
           [grid (periodic-grid n L)]
           [u (sample-function sin grid)]
           [d2u (fourier-diff2 u L)]
           [d2u-exact (sample-function (lambda (x) (- (sin x))) grid)]
           [err (spectral-error-estimate d2u d2u-exact)])
      (assert-true (< err 1e-10))))
)

;;; ============================================================
;;; Test Group: Fourier Heat Equation
;;; ============================================================

(test-group "fourier-heat"

  (define-test "heat equation decays high frequencies"
    ;; Initial: sin(x), should decay as e^{-t}
    (let* ([n 64]
           [L (* 2 pi-val)]
           [alpha 1.0]
           [dt 0.1]
           [grid (periodic-grid n L)]
           [u0 (sample-function sin grid)]
           [u1 (fourier-heat-exact-step u0 L alpha dt)]
           ;; Exact: sin(x) * e^{-dt}
           [decay (exp (- dt))]
           [u1-exact (sample-function (lambda (x) (* decay (sin x))) grid)]
           [err (spectral-error-estimate u1 u1-exact)])
      (assert-true (< err 1e-10))))

  (define-test "heat equation multiple steps"
    (let* ([n 64]
           [L (* 2 pi-val)]
           [alpha 1.0]
           [grid (periodic-grid n L)]
           [u0 (sample-function sin grid)]
           [dt 0.1]
           [n-steps 10]  ; 10 steps of 0.1 = t=1.0
           ;; After t=1, should be sin(x)*e^{-1}
           [u-final (let loop ([u u0] [step 0])
                      (if (= step n-steps)
                          u
                          (loop (fourier-heat-exact-step u L alpha dt)
                                (+ step 1))))]
           [decay (exp -1.0)]
           [u-exact (sample-function (lambda (x) (* decay (sin x))) grid)]
           [err (spectral-error-estimate u-final u-exact)])
      (assert-true (< err 1e-10))))
)

;;; ============================================================
;;; Test Group: Fourier Advection
;;; ============================================================

(test-group "fourier-advection"

  (define-test "advection translates solution"
    ;; sin(x) translates to sin(x - c*t)
    (let* ([n 64]
           [L (* 2 pi-val)]
           [c 1.0]
           [dt 0.5]
           [grid (periodic-grid n L)]
           [u0 (sample-function sin grid)]
           [u1 (fourier-advection-exact-step u0 L c dt)]
           ;; Exact: sin(x - c*dt) = sin(x - 0.5)
           [u1-exact (sample-function (lambda (x) (sin (- x (* c dt)))) grid)]
           [err (spectral-error-estimate u1 u1-exact)])
      (assert-true (< err 1e-10))))
)

;;; ============================================================
;;; Test Group: Chebyshev Nodes
;;; ============================================================

(test-group "chebyshev-nodes"

  (define-test "N=2 gives 3 nodes"
    (let ([nodes (chebyshev-nodes 2)])
      (assert-equal 3 (vector-length nodes))))

  (define-test "endpoints are ±1"
    (let ([nodes (chebyshev-nodes 4)])
      (assert-true (< (abs (- (vector-ref nodes 0) 1.0)) 1e-10))
      (assert-true (< (abs (- (vector-ref nodes 4) -1.0)) 1e-10))))

  (define-test "nodes are symmetric"
    (let ([nodes (chebyshev-nodes 4)])
      ;; x_j = -x_{N-j}
      (assert-true (< (abs (+ (vector-ref nodes 1) (vector-ref nodes 3))) 1e-10))
      (assert-true (< (abs (vector-ref nodes 2)) 1e-10))))  ; middle node is 0
)

;;; ============================================================
;;; Test Group: Chebyshev Polynomials
;;; ============================================================

(test-group "chebyshev-poly"

  (define-test "T_0(x) = 1"
    (assert-true (< (abs (- (chebyshev-poly 0 0.5) 1.0)) 1e-10)))

  (define-test "T_1(x) = x"
    (assert-true (< (abs (- (chebyshev-poly 1 0.5) 0.5)) 1e-10)))

  (define-test "T_2(x) = 2x² - 1"
    (let ([x 0.5]
          [expected (- (* 2 (* 0.5 0.5)) 1)])
      (assert-true (< (abs (- (chebyshev-poly 2 x) expected)) 1e-10))))

  (define-test "T_n(1) = 1 for all n"
    (assert-true (< (abs (- (chebyshev-poly 5 1.0) 1.0)) 1e-10))
    (assert-true (< (abs (- (chebyshev-poly 10 1.0) 1.0)) 1e-10)))
)

;;; ============================================================
;;; Test Group: Chebyshev Differentiation Matrix
;;; ============================================================

(test-group "chebyshev-diff-matrix"

  (define-test "derivative of x is 1"
    ;; D applied to [1, x_1, x_2, ..., x_N] at nodes should give [1, 1, ..., 1]
    (let* ([n 4]
           [x (chebyshev-nodes n)]
           [D (chebyshev-diff-matrix n)]
           [u x]  ; u(x) = x at the nodes
           [du (chebyshev-diff u D)])
      ;; du should be approximately [1, 1, 1, 1, 1]
      (do ([i 0 (+ i 1)])
          ((= i (+ n 1)))
        (assert-true (< (abs (- (vector-ref du i) 1.0)) 1e-10)))))

  (define-test "derivative of x² is 2x"
    (let* ([n 8]
           [x (chebyshev-nodes n)]
           [D (chebyshev-diff-matrix n)]
           [u (let ([v (make-vector (+ n 1) 0.0)])
                (do ([i 0 (+ i 1)])
                    ((> i n) v)
                  (let ([xi (vector-ref x i)])
                    (vector-set! v i (* xi xi)))))]
           [du (chebyshev-diff u D)]
           [du-exact (let ([v (make-vector (+ n 1) 0.0)])
                       (do ([i 0 (+ i 1)])
                           ((> i n) v)
                         (vector-set! v i (* 2 (vector-ref x i)))))])
      (assert-true (< (spectral-error-estimate du du-exact) 1e-10))))
)

;;; ============================================================
;;; Test Group: Chebyshev Poisson Solver
;;; ============================================================

(test-group "chebyshev-poisson"

  (define-test "u'' = 2 with u(-1) = u(1) = 0 gives u = x² - 1"
    ;; Exact solution: u = x² - 1
    ;; u'' = 2
    (let* ([n 8]
           [f (lambda (x) 2.0)]  ; constant source
           [u (chebyshev-poisson-1d f 0.0 0.0 n)]  ; u(-1) = u(1) = 0
           [x (chebyshev-nodes n)]
           ;; Exact at the nodes
           [u-exact (let ([v (make-vector (+ n 1) 0.0)])
                      (do ([i 0 (+ i 1)])
                          ((> i n) v)
                        (let ([xi (vector-ref x i)])
                          (vector-set! v i (- (* xi xi) 1.0)))))])
      (assert-true (< (spectral-error-estimate u u-exact) 1e-10))))

  (define-test "u'' = -π²sin(πx) with u(±1) = 0"
    ;; Exact solution: u = sin(πx)
    ;; u'' = -π² sin(πx)
    (let* ([n 16]
           [f (lambda (x) (* (- (* pi-val pi-val)) (sin (* pi-val x))))]
           [u (chebyshev-poisson-1d f 0.0 0.0 n)]
           [x (chebyshev-nodes n)]
           [u-exact (let ([v (make-vector (+ n 1) 0.0)])
                      (do ([i 0 (+ i 1)])
                          ((> i n) v)
                        (vector-set! v i (sin (* pi-val (vector-ref x i))))))])
      ;; Spectral accuracy for smooth solution
      (assert-true (< (spectral-error-estimate u u-exact) 1e-8))))
)

;;; ============================================================
;;; Test Group: Convenience Functions
;;; ============================================================

(test-group "convenience"

  (define-test "periodic grid has correct spacing"
    (let* ([n 8]
           [L (* 2 pi-val)]
           [grid (periodic-grid n L)]
           [h (/ L n)])
      (assert-equal n (vector-length grid))
      (assert-true (< (abs (- (vector-ref grid 1) h)) 1e-10))))

  (define-test "sample-function works"
    (let* ([n 4]
           [grid '#(0 1 2 3)]
           [values (sample-function (lambda (x) (* x x)) grid)])
      (assert-true (< (abs (- (vector-ref values 0) 0)) 1e-10))
      (assert-true (< (abs (- (vector-ref values 1) 1)) 1e-10))
      (assert-true (< (abs (- (vector-ref values 2) 4)) 1e-10))
      (assert-true (< (abs (- (vector-ref values 3) 9)) 1e-10))))
)

;;; Run all tests
(run-all-tests)
