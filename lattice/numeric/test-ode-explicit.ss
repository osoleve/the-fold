(load "core/testing/test-framework.ss")
(load "lattice/numeric/ode-explicit.ss")

;;; ============================================================
;;; Generic ODE Integrator Tests — List VSpace
;;; ============================================================

(define (assert-= actual expected tolerance)
  (assert-true (< (abs (- actual expected)) tolerance)))

(test-group "list-euler"

  (define-test "constant derivative"
    ;; dy/dt = 1, y(0) = 0, dt = 0.5 → y = 0.5
    (let ([result (generic-euler-step list-vspace (lambda (t y) '(1)) 0 '(0) 0.5)])
      (assert-= (car result) 0.5 1e-10)))

  (define-test "linear growth"
    ;; dy/dt = 2, y(0) = 1, dt = 0.1 → y = 1.2
    (let ([result (generic-euler-step list-vspace (lambda (t y) '(2)) 0 '(1) 0.1)])
      (assert-= (car result) 1.2 1e-10))))

(test-group "list-rk4"

  (define-test "exponential growth"
    ;; dy/dt = y, y(0) = 1, dt = 0.1 → y ≈ e^0.1
    (let ([result (generic-rk4-step list-vspace (lambda (t y) y) 0 '(1.0) 0.1)])
      (assert-= (car result) (exp 0.1) 1e-6)))

  (define-test "fourth-order convergence"
    ;; Compare error at dt vs dt/2 — should reduce by factor ~16
    (let* ([f (lambda (t y) y)]
           [r1 (generic-rk4-step list-vspace f 0 '(1.0) 0.1)]
           [r2a (generic-rk4-step list-vspace f 0 '(1.0) 0.05)]
           [r2 (generic-rk4-step list-vspace f 0.05 r2a 0.05)]
           [exact (exp 0.1)]
           [err1 (abs (- (car r1) exact))]
           [err2 (abs (- (car r2) exact))])
      (assert-true (> (/ err1 err2) 10)))))

(test-group "list-midpoint"

  (define-test "constant derivative"
    (let ([result (generic-midpoint-step list-vspace (lambda (t y) '(1)) 0 '(0) 0.5)])
      (assert-= (car result) 0.5 1e-10))))

(test-group "list-heun"

  (define-test "constant derivative"
    (let ([result (generic-heun-step list-vspace (lambda (t y) '(1)) 0 '(0) 0.5)])
      (assert-= (car result) 0.5 1e-10))))

;;; ============================================================
;;; Generic ODE Integrator Tests — Scheme Vector VSpace
;;; ============================================================

(test-group "vec-euler"

  (define-test "constant derivative"
    (let ([result (generic-euler-step scheme-vec-vspace
                    (lambda (t y) '#(1)) 0 '#(0) 0.5)])
      (assert-= (vector-ref result 0) 0.5 1e-10)))

  (define-test "2D system"
    ;; dy1/dt = 1, dy2/dt = -1
    (let ([result (generic-euler-step scheme-vec-vspace
                    (lambda (t y) '#(1 -1)) 0 '#(0 0) 0.1)])
      (assert-= (vector-ref result 0) 0.1 1e-10)
      (assert-= (vector-ref result 1) -0.1 1e-10))))

(test-group "vec-rk4"

  (define-test "exponential decay"
    ;; dy/dt = -y, y(0) = 1, dt = 0.1 → y ≈ e^{-0.1}
    (let ([result (generic-rk4-step scheme-vec-vspace
                    (lambda (t y) (vector (* -1 (vector-ref y 0))))
                    0 '#(1.0) 0.1)])
      (assert-= (vector-ref result 0) (exp -0.1) 1e-6))))

;;; ============================================================
;;; Multi-step Integration
;;; ============================================================

(test-group "generic-integrate"

  (define-test "trajectory length"
    (let ([results (generic-integrate list-vspace generic-euler-step
                     (lambda (t y) '(1)) 0 '(0) 0.1 5)])
      (assert-equal 6 (length results))))

  (define-test "exponential to t=1"
    ;; dy/dt = y, y(0) = 1, integrate to t=1 with RK4
    (let* ([results (generic-integrate list-vspace generic-rk4-step
                      (lambda (t y) y) 0 '(1.0) 0.1 10)]
           [final (car (list-ref results 10))])
      (assert-= final (exp 1.0) 1e-4)))

  (define-test "vec-vspace integration"
    ;; Same test with scheme vectors
    (let* ([results (generic-integrate scheme-vec-vspace generic-rk4-step
                      (lambda (t y) (vector (vector-ref y 0)))
                      0 '#(1.0) 0.1 10)]
           [final (vector-ref (list-ref results 10) 0)])
      (assert-= final (exp 1.0) 1e-4))))

(run-all-tests)
