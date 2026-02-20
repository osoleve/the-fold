;;; lattice/sim/dynamics/test-ode-state-space.ss — Tests for ODE ↔ State-Space Bridge
;;;
;;; Run from project root: scheme --script lattice/sim/dynamics/test-ode-state-space.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/sim/dynamics/ode-state-space.ss")

(display "\n")
(display "====\n")
(display "         ODE ↔ STATE-SPACE BRIDGE TESTS\n")
(display "====\n")

;;; ============================================================
;;; Helper: check if two values are approximately equal
;;; ============================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (matrix-approx= m1 m2 tol)
  (let ([r (matrix-rows m1)]
        [c (matrix-cols m1)])
    (and (= r (matrix-rows m2))
         (= c (matrix-cols m2))
         (let loop ([i 0])
           (if (>= i r)
               #t
               (let inner ([j 0])
                 (if (>= j c)
                     (loop (+ i 1))
                     (if (approx= (matrix-ref m1 i j)
                                   (matrix-ref m2 i j)
                                   tol)
                         (inner (+ j 1))
                         #f))))))))

(define (vec-approx= v1 v2 tol)
  (and (= (vector-length v1) (vector-length v2))
       (let loop ([i 0])
         (if (>= i (vector-length v1))
             #t
             (if (approx= (vector-ref v1 i)
                           (vector-ref v2 i)
                           tol)
                 (loop (+ i 1))
                 #f)))))

;;; ============================================================
;;; 1. Numerical Jacobian Tests
;;; ============================================================

(test-group numerical-jacobian

  (define-test jacobian-linear-function
    ;; f(x) = [2*x0 + 3*x1, x0 - x1] => Jacobian = [[2, 3], [1, -1]]
    (let* ([f (lambda (x)
                (vector (+ (* 2 (vector-ref x 0)) (* 3 (vector-ref x 1)))
                        (- (vector-ref x 0) (vector-ref x 1))))]
           [x0 (vector 0.0 0.0)]
           [J (numerical-jacobian f x0 2)]
           [expected (matrix-from-lists '((2 3) (1 -1)))])
      (assert-true (matrix-approx= J expected 1e-5))))

  (define-test jacobian-at-nonzero-point
    ;; f(x) = [x0^2, x0*x1] => J = [[2*x0, 0], [x1, x0]]
    ;; At (3, 4): J = [[6, 0], [4, 3]]
    (let* ([f (lambda (x)
                (let ([x0 (vector-ref x 0)]
                      [x1 (vector-ref x 1)])
                  (vector (* x0 x0)
                          (* x0 x1))))]
           [pt (vector 3.0 4.0)]
           [J (numerical-jacobian f pt 2)]
           [expected (matrix-from-lists '((6 0) (4 3)))])
      (assert-true (matrix-approx= J expected 1e-4))))

  (define-test jacobian-scalar-field
    ;; f(x) = [sin(x0)] => J = [[cos(x0)]]
    ;; At x0 = 0: J = [[1]]
    (let* ([f (lambda (x) (vector (sin (vector-ref x 0))))]
           [pt (vector 0.0)]
           [J (numerical-jacobian f pt 1)])
      (assert-true (approx= (matrix-ref J 0 0) 1.0 1e-6))))
)

;;; ============================================================
;;; 2. Linearization Tests
;;; ============================================================

(test-group linearization

  (define-test linearize-already-linear
    ;; dx/dt = -2*x + u (scalar). Should recover A=-2, B=1 exactly.
    (let* ([f-xu (lambda (x u)
                   (vector (+ (* -2 (vector-ref x 0))
                              (vector-ref u 0))))]
           [x0 (vector 0.0)]
           [u0 (vector 0.0)]
           [sys (linearize-ode f-xu x0 u0 1 1)])
      (assert-true (ss? sys))
      (assert-true (approx= (matrix-ref (ss-A sys) 0 0) -2.0 1e-5))
      (assert-true (approx= (matrix-ref (ss-B sys) 0 0) 1.0 1e-5))))

  (define-test linearize-2d-system
    ;; dx0/dt = -x0 + x1, dx1/dt = -2*x1 + 3*u
    ;; A = [[-1, 1], [0, -2]], B = [[0], [3]]
    (let* ([f-xu (lambda (x u)
                   (vector (+ (* -1 (vector-ref x 0))
                              (vector-ref x 1))
                           (+ (* -2 (vector-ref x 1))
                              (* 3 (vector-ref u 0)))))]
           [x0 (vector 0.0 0.0)]
           [u0 (vector 0.0)]
           [sys (linearize-ode f-xu x0 u0 2 1)]
           [A-expected (matrix-from-lists '((-1 1) (0 -2)))]
           [B-expected (matrix-from-lists '((0) (3)))])
      (assert-true (ss? sys))
      (assert-equal 2 (ss-order sys))
      (assert-equal 1 (ss-inputs sys))
      (assert-true (matrix-approx= (ss-A sys) A-expected 1e-5))
      (assert-true (matrix-approx= (ss-B sys) B-expected 1e-5))))

  (define-test linearize-nonlinear-at-equilibrium
    ;; Nonlinear: dx/dt = -x^2 + u. Equilibrium at x0=1, u0=1.
    ;; df/dx = -2*x = -2 at x=1. df/du = 1.
    ;; A = [[-2]], B = [[1]]
    (let* ([f-xu (lambda (x u)
                   (vector (+ (- (* (vector-ref x 0) (vector-ref x 0)))
                              (vector-ref u 0))))]
           [x0 (vector 1.0)]
           [u0 (vector 1.0)]
           [sys (linearize-ode f-xu x0 u0 1 1)])
      (assert-true (approx= (matrix-ref (ss-A sys) 0 0) -2.0 1e-5))
      (assert-true (approx= (matrix-ref (ss-B sys) 0 0) 1.0 1e-5))))

  (define-test linearize-autonomous
    ;; Damped harmonic oscillator: dx0/dt = x1, dx1/dt = -x0 - 0.5*x1
    ;; Equilibrium at origin. A = [[0, 1], [-1, -0.5]]
    (let* ([f (lambda (x)
                (vector (vector-ref x 1)
                        (- (- (vector-ref x 0))
                           (* 0.5 (vector-ref x 1)))))]
           [x0 (vector 0.0 0.0)]
           [sys (linearize-autonomous-ode f x0 2)]
           [A-expected (matrix-from-lists '((0 1) (-1 -0.5)))])
      (assert-true (ss? sys))
      (assert-equal 2 (ss-order sys))
      (assert-true (matrix-approx= (ss-A sys) A-expected 1e-5))))
)

;;; ============================================================
;;; 3. State-Space -> ODE Conversion Tests
;;; ============================================================

(test-group state-space-to-ode

  (define-test convert-scalar-ss-to-ode
    ;; x' = -x (A=-1, B=0, C=1, D=0)
    (let* ([sys (ss-scalar -1 0 1 0)]
           [ode (state-space->ode sys)])
      (assert-true (ode-system? ode))
      (assert-equal 1 (ode-dimension ode))
      ;; Evaluate at state [2]: dx/dt = -1 * 2 = -2
      (let ([deriv (eval-vector-field ode 0.0 (vector 2.0))])
        (assert-true (approx= (vector-ref deriv 0) -2.0 1e-10)))))

  (define-test convert-2d-ss-to-ode
    ;; Mass-spring: A = [[0,1],[-1,0]], B = [[0],[1]]
    (let* ([A (matrix-from-lists '((0 1) (-1 0)))]
           [B (matrix-from-lists '((0) (1)))]
           [C (matrix-identity 2)]
           [D (make-matrix 2 1 0)]
           [sys (make-ss A B C D)]
           [ode (state-space->ode sys)])
      (assert-true (ode-system? ode))
      (assert-equal 2 (ode-dimension ode))
      ;; At state [1, 0] with zero input: dx/dt = [0, -1]
      (let ([deriv (eval-vector-field ode 0.0 (vector 1.0 0.0))])
        (assert-true (approx= (vector-ref deriv 0) 0.0 1e-10))
        (assert-true (approx= (vector-ref deriv 1) -1.0 1e-10)))))

  (define-test convert-with-input-function
    ;; x' = -x + u, with u(t) = 3
    (let* ([sys (ss-scalar -1 1 1 0)]
           [input-fn (lambda (t) (vector 3.0))]
           [ode (state-space->ode sys input-fn)])
      ;; At state [1]: dx/dt = -1 + 3 = 2
      (let ([deriv (eval-vector-field ode 0.0 (vector 1.0))])
        (assert-true (approx= (vector-ref deriv 0) 2.0 1e-10)))))
)

;;; ============================================================
;;; 4. RK4 Integration Tests
;;; ============================================================

(test-group rk4-integration

  (define-test rk4-exponential-decay
    ;; dx/dt = -x, x(0) = 1. Exact: x(t) = e^(-t).
    ;; After t=1 with dt=0.01: x ≈ e^(-1) ≈ 0.3679
    (let* ([f (lambda (t x)
                (vector (- (vector-ref x 0))))]
           [traj (vec-rk4-integrate f 0.0 (vector 1.0) 0.01 100)]
           [final (cadr (car (reverse traj)))])
      (assert-true (approx= (vector-ref final 0) (exp -1) 1e-6))))

  (define-test rk4-simple-harmonic
    ;; dx/dt = v, dv/dt = -x. x(0) = 1, v(0) = 0.
    ;; Exact: x(t) = cos(t), v(t) = -sin(t).
    ;; After t = pi: x ≈ -1, v ≈ 0.
    (let* ([f (lambda (t s)
                (vector (vector-ref s 1)
                        (- (vector-ref s 0))))]
           [traj (vec-rk4-integrate f 0.0 (vector 1.0 0.0) 0.001 3142)]
           [final (cadr (car (reverse traj)))])
      ;; pi ≈ 3.142
      (assert-true (approx= (vector-ref final 0) -1.0 1e-3))
      (assert-true (approx= (vector-ref final 1) 0.0 2e-3))))
)

;;; ============================================================
;;; 5. Simulate State-Space Tests
;;; ============================================================

(test-group simulate-state-space

  (define-test simulate-exponential-decay
    ;; x' = -x, y = x. x(0) = 1. After t=1: x ≈ e^(-1)
    (let* ([sys (ss-scalar -1 0 1 0)]
           [traj (simulate-state-space sys (vector 1.0) 0.01 100)]
           [final (car (reverse traj))]
           [t-final (car final)]
           [x-final (cadr final)]
           [y-final (caddr final)])
      (assert-true (approx= t-final 1.0 1e-10))
      (assert-true (approx= (vector-ref x-final 0) (exp -1) 1e-6))
      ;; Output should equal state (C=1, D=0, u=0)
      (assert-true (approx= (vector-ref y-final 0) (exp -1) 1e-6))))

  (define-test simulate-with-step-input
    ;; x' = -x + u, y = x. u(t) = 1 (constant step input).
    ;; Exact: x(t) = 1 - e^(-t). At t=5: x ≈ 1 - e^(-5) ≈ 0.9933
    (let* ([sys (ss-scalar -1 1 1 0)]
           [input-fn (lambda (t) (vector 1.0))]
           [traj (simulate-state-space sys (vector 0.0) 0.01 500 input-fn)]
           [final (car (reverse traj))]
           [x-final (cadr final)])
      (assert-true (approx= (vector-ref x-final 0)
                             (- 1.0 (exp -5.0))
                             1e-4))))

  (define-test simulate-2d-oscillator
    ;; Harmonic oscillator as state space: A = [[0,1],[-1,0]]
    ;; x(0) = [1, 0]. After t = pi: x ≈ [-1, 0]
    (let* ([A (matrix-from-lists '((0 1) (-1 0)))]
           [B (make-matrix 2 1 0)]
           [C (matrix-identity 2)]
           [D (make-matrix 2 1 0)]
           [sys (make-ss A B C D)]
           ;; ~3142 steps at dt=0.001 to reach t ≈ pi
           [traj (simulate-state-space sys (vector 1.0 0.0) 0.001 3142)]
           [final (car (reverse traj))]
           [x-final (cadr final)])
      (assert-true (approx= (vector-ref x-final 0) -1.0 1e-3))
      (assert-true (approx= (vector-ref x-final 1) 0.0 2e-3))))

  (define-test simulate-final-matches-trajectory
    ;; simulate-state-space-final should give same answer as last element
    (let* ([sys (ss-scalar -1 0 1 0)]
           [traj (simulate-state-space sys (vector 1.0) 0.01 100)]
           [last-traj (car (reverse traj))]
           [final (simulate-state-space-final sys (vector 1.0) 0.01 100)])
      (assert-true (approx= (vector-ref (cadr last-traj) 0)
                             (vector-ref (cadr final) 0)
                             1e-12))))
)

;;; ============================================================
;;; 6. Round-trip Tests (ODE -> SS -> ODE)
;;; ============================================================

(test-group round-trip

  (define-test linearize-then-simulate
    ;; Start with known linear ODE: dx/dt = -2*x + u
    ;; Linearize it, then simulate the resulting SS.
    ;; Should match direct ODE integration.
    (let* ([f-xu (lambda (x u)
                   (vector (+ (* -2 (vector-ref x 0))
                              (vector-ref u 0))))]
           [x0 (vector 0.0)]
           [u0 (vector 0.0)]
           [sys (linearize-ode f-xu x0 u0 1 1)]
           ;; Simulate with zero input from x(0)=1
           [final-ss (simulate-state-space-final sys (vector 1.0) 0.01 100)]
           ;; Direct RK4 of the original ODE with u=0
           [f-direct (lambda (t x) (vector (* -2.0 (vector-ref x 0))))]
           [traj-direct (vec-rk4-integrate f-direct 0.0 (vector 1.0) 0.01 100)]
           [final-direct (cadr (car (reverse traj-direct)))])
      ;; Both should give x(1) ≈ e^(-2) ≈ 0.1353
      (assert-true (approx= (vector-ref (cadr final-ss) 0)
                             (vector-ref final-direct 0)
                             1e-8))))

  (define-test ss-to-ode-preserves-dynamics
    ;; Create an SS, convert to ODE, evaluate — should match ss-state-equation
    (let* ([A (matrix-from-lists '((-1 2) (0 -3)))]
           [B (matrix-from-lists '((1) (0)))]
           [C (matrix-identity 2)]
           [D (make-matrix 2 1 0)]
           [sys (make-ss A B C D)]
           [ode (state-space->ode sys)]
           [x-test (vector 1.0 2.0)]
           [u-zero (vector 0.0)]
           [deriv-ode (eval-vector-field ode 0.0 x-test)]
           [deriv-ss (ss-state-equation sys x-test u-zero)])
      (assert-true (vec-approx= deriv-ode deriv-ss 1e-12))))
)

;;; ============================================================
;;; Run and report
;;; ============================================================

(run-all-tests)
