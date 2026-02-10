;;; fabric/stitches/numerical/test-integrators.ss — Tests for Numerical Integration

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/physics/classical/ode-integrators.ss")

;;; Helper for numeric comparison
(define (assert-= actual expected tolerance)
  (unless (< (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (display " ± ")
          (display tolerance)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; Helper for vector comparison
(define (assert-vec-= actual expected tolerance)
  (for-each (lambda (a e) (assert-= a e tolerance))
            actual expected))

(display "
══════════════════════════════════════════════════════════
         NUMERICAL INTEGRATION TESTS
══════════════════════════════════════════════════════════
")

;;; ====
;;; State Vector Operations
;;; ====

(test-group state-ops-tests
            (define-test state-add-test
              (let ([result (state-add '(1 2 3) '(4 5 6))])
                   (assert-equal '(5 7 9) result)))
            
            (define-test state-sub-test
              (let ([result (state-sub '(5 7 9) '(4 5 6))])
                   (assert-equal '(1 2 3) result)))
            
            (define-test state-scale-test
              (let ([result (state-scale 2 '(1 2 3))])
                   (assert-equal '(2 4 6) result)))
            
            (define-test state-madd-test
              ;; a + k*b = (1,2,3) + 2*(1,1,1) = (3,4,5)
              (let ([result (state-madd '(1 2 3) 2 '(1 1 1))])
                   (assert-equal '(3 4 5) result))))

;;; ====
;;; Euler Method Tests
;;; ====

(test-group euler-tests
            (define-test euler-constant
              ;; dy/dt = 1, y(0) = 0
              ;; y(t) = t, so y(1) = 1 with dt=1
              (let* ([f (lambda (t y) '(1))]
                     [result (euler-step f 0 '(0) 1)])
                    (assert-vec-= result '(1) 0.0001)))
            
            (define-test euler-linear
              ;; dy/dt = y, y(0) = 1
              ;; Exact: y(t) = e^t
              ;; Euler: y(1) = 1 + 1*1 = 2 (exact is e ≈ 2.718)
              (let* ([f (lambda (t y) y)]
                     [result (euler-step f 0 '(1) 1)])
                    (assert-vec-= result '(2) 0.0001)))
            
            (define-test euler-small-step
              ;; dy/dt = y, y(0) = 1, dt = 0.1
              ;; After 1 step: y(0.1) ≈ 1.1
              (let* ([f (lambda (t y) y)]
                     [result (euler-step f 0 '(1) 0.1)])
                    (assert-vec-= result '(1.1) 0.0001)))
            
            (define-test euler-2d
              ;; dx/dt = v, dv/dt = -x (harmonic oscillator)
              ;; State: (x, v)
              (let* ([f (lambda (t state)
                                (let ([x (car state)]
                                      [v (cadr state)])
                                     (list v (- x))))]
                     [result (euler-step f 0 '(1 0) 0.1)])
                    ;; x' = x + dt*v = 1 + 0.1*0 = 1
                    ;; v' = v + dt*(-x) = 0 + 0.1*(-1) = -0.1
                    (assert-vec-= result '(1 -0.1) 0.0001))))

;;; ====
;;; Midpoint Method Tests
;;; ====

(test-group midpoint-tests
            (define-test midpoint-constant
              (let* ([f (lambda (t y) '(1))]
                     [result (midpoint-step f 0 '(0) 1)])
                    (assert-vec-= result '(1) 0.0001)))
            
            (define-test midpoint-linear
              ;; dy/dt = y, y(0) = 1
              ;; Midpoint should be more accurate than Euler
              (let* ([f (lambda (t y) y)]
                     [result (midpoint-step f 0 '(1) 1)])
                    ;; k1 = 1
                    ;; mid = 1 + 0.5*1 = 1.5
                    ;; k2 = 1.5
                    ;; y = 1 + 1*1.5 = 2.5 (closer to e ≈ 2.718 than Euler's 2)
                    (assert-vec-= result '(2.5) 0.0001)))
            
            (define-test midpoint-quadratic
              ;; dy/dt = 2t, y(0) = 0
              ;; Exact: y(t) = t²
              ;; At t=1 with dt=1: exact is 1
              (let* ([f (lambda (t y) (list (* 2 t)))]
                     [result (midpoint-step f 0 '(0) 1)])
                    ;; k1 = 0
                    ;; k2 = 2*0.5 = 1
                    ;; y = 0 + 1*1 = 1 (exact!)
                    (assert-vec-= result '(1) 0.0001))))

;;; ====
;;; Heun Method Tests
;;; ====

(test-group heun-tests
            (define-test heun-constant
              (let* ([f (lambda (t y) '(1))]
                     [result (heun-step f 0 '(0) 1)])
                    (assert-vec-= result '(1) 0.0001)))
            
            (define-test heun-linear
              ;; dy/dt = y, y(0) = 1
              (let* ([f (lambda (t y) y)]
                     [result (heun-step f 0 '(1) 1)])
                    ;; k1 = 1
                    ;; euler = 1 + 1*1 = 2
                    ;; k2 = 2
                    ;; y = 1 + 0.5*(1+2) = 1 + 1.5 = 2.5
                    (assert-vec-= result '(2.5) 0.0001))))

;;; ====
;;; RK4 Method Tests
;;; ====

(test-group rk4-tests
            (define-test rk4-constant
              (let* ([f (lambda (t y) '(1))]
                     [result (rk4-step f 0 '(0) 1)])
                    (assert-vec-= result '(1) 0.0001)))
            
            (define-test rk4-exponential
              ;; dy/dt = y, y(0) = 1
              ;; RK4 should be very accurate
              (let* ([f (lambda (t y) y)]
                     [result (rk4-step f 0 '(1) 1)])
                    ;; Should be close to e ≈ 2.71828
                    (assert-= (car result) (exp 1) 0.01)))
            
            (define-test rk4-small-step-exponential
              ;; dy/dt = y, y(0) = 1, dt = 0.1
              (let* ([f (lambda (t y) y)]
                     [result (rk4-step f 0 '(1) 0.1)])
                    ;; e^0.1 ≈ 1.10517
                    (assert-= (car result) (exp 0.1) 0.0001)))
            
            (define-test rk4-harmonic-oscillator
              ;; d²x/dt² = -x (omega = 1)
              ;; State: (x, v), dx/dt = v, dv/dt = -x
              ;; Exact solution: x(t) = cos(t), v(t) = -sin(t)
              (let* ([f (lambda (t state)
                                (let ([x (car state)]
                                      [v (cadr state)])
                                     (list v (- x))))]
                     ;; Start at x=1, v=0 (maximum displacement)
                     ;; After dt=0.1, expect x≈cos(0.1)≈0.995, v≈-sin(0.1)≈-0.0998
                     [result (rk4-step f 0 '(1 0) 0.1)])
                    (assert-= (car result) (cos 0.1) 0.0001)
                    (assert-= (cadr result) (- (sin 0.1)) 0.0001)))
            
            (define-test rk4-polynomial
              ;; dy/dt = 3t², y(0) = 0
              ;; Exact: y(t) = t³
              (let* ([f (lambda (t y) (list (* 3 t t)))]
                     ;; At t=1 with dt=1: exact is 1
                     [result (rk4-step f 0 '(0) 1)])
                    ;; RK4 is exact for polynomials up to degree 4
                    (assert-= (car result) 1 0.0001))))

;;; ====
;;; Symplectic Euler Tests
;;; ====

(test-group symplectic-euler-tests
            (define-test symplectic-euler-free-particle
              ;; a = 0 (no force)
              ;; Starting at x=0, v=1, after dt=1 should have x=1, v=1
              (let* ([a (lambda (t pos) '(0))]
                     [result (symplectic-euler-step a 0 '(0) '(1) 1)]
                     [new-pos (car result)]
                     [new-vel (cadr result)])
                    (assert-vec-= new-pos '(1) 0.0001)
                    (assert-vec-= new-vel '(1) 0.0001)))
            
            (define-test symplectic-euler-constant-accel
              ;; a = 1 (constant acceleration)
              ;; v' = v + a*dt = 0 + 1*1 = 1
              ;; x' = x + v'*dt = 0 + 1*1 = 1
              (let* ([a (lambda (t pos) '(1))]
                     [result (symplectic-euler-step a 0 '(0) '(0) 1)]
                     [new-pos (car result)]
                     [new-vel (cadr result)])
                    (assert-vec-= new-pos '(1) 0.0001)
                    (assert-vec-= new-vel '(1) 0.0001))))

;;; ====
;;; Velocity Verlet Tests
;;; ====

(test-group velocity-verlet-tests
            (define-test velocity-verlet-free-particle
              (let* ([a (lambda (t pos) '(0))]
                     [result (velocity-verlet-step a 0 '(0) '(1) 1)]
                     [new-pos (car result)]
                     [new-vel (cadr result)])
                    (assert-vec-= new-pos '(1) 0.0001)
                    (assert-vec-= new-vel '(1) 0.0001)))
            
            (define-test velocity-verlet-constant-accel
              ;; a = 1, x(0) = 0, v(0) = 0, dt = 1
              ;; x(1) = 0 + 0*1 + 0.5*1*1² = 0.5
              ;; v(1) = 0 + 0.5*(1+1)*1 = 1
              (let* ([a (lambda (t pos) '(1))]
                     [result (velocity-verlet-step a 0 '(0) '(0) 1)]
                     [new-pos (car result)]
                     [new-vel (cadr result)])
                    (assert-vec-= new-pos '(0.5) 0.0001)
                    (assert-vec-= new-vel '(1) 0.0001)))
            
            (define-test velocity-verlet-harmonic
              ;; a = -x (harmonic oscillator)
              ;; Starting at x=1, v=0
              (let* ([a (lambda (t pos) (list (- (car pos))))]
                     [result (velocity-verlet-step a 0 '(1) '(0) 0.1)]
                     [new-pos (car result)]
                     [new-vel (cadr result)])
                    ;; Should be close to cos(0.1), -sin(0.1)
                    (assert-= (car new-pos) (cos 0.1) 0.001)
                    (assert-= (car new-vel) (- (sin 0.1)) 0.001))))

;;; ====
;;; Leapfrog Tests
;;; ====

(test-group leapfrog-tests
            (define-test leapfrog-free-particle
              (let* ([a (lambda (t pos) '(0))]
                     [result (leapfrog-step a 0 '(0) '(1) 1)]
                     [new-pos (car result)]
                     [new-vel (cadr result)])
                    (assert-vec-= new-pos '(1) 0.0001)
                    (assert-vec-= new-vel '(1) 0.0001)))
            
            (define-test leapfrog-harmonic
              ;; Leapfrog should give same result as velocity Verlet
              (let* ([a (lambda (t pos) (list (- (car pos))))]
                     [result (leapfrog-step a 0 '(1) '(0) 0.1)]
                     [new-pos (car result)]
                     [new-vel (cadr result)])
                    (assert-= (car new-pos) (cos 0.1) 0.001)
                    (assert-= (car new-vel) (- (sin 0.1)) 0.001))))

;;; ====
;;; Multi-Step Integration Tests
;;; ====

(test-group integration-tests
            (define-test integrate-constant
              ;; dy/dt = 1, integrate from t=0 with y=0 for 5 steps
              (let* ([f (lambda (t y) '(1))]
                     [results (integrate euler-step f 0 '(0) 1 5)])
                    (assert-equal 6 (length results))  ; Including initial state
                    (assert-= (car (list-ref results 5)) 5 0.0001)))
            
            (define-test integrate-symplectic-harmonic
              ;; Harmonic oscillator: a = -x
              ;; Period T = 2π, so after 2π should return to start
              ;; Test energy conservation over many steps
              (let* ([a (lambda (t pos) (list (- (car pos))))]
                     [dt 0.01]
                     [steps 628]  ; About one period
                     [results (integrate-symplectic velocity-verlet-step
                                                    a 0 '(1) '(0) dt steps)]
                     [final (list-ref results steps)]
                     [final-pos (car final)]
                     [final-vel (cadr final)])
                    ;; Position should be close to initial after one period
                    (assert-= (car final-pos) 1 0.02)
                    ;; Velocity should be close to 0
                    (assert-= (car final-vel) 0 0.02))))

;;; ====
;;; Energy Conservation Tests
;;; ====

(test-group energy-tests
            (define-test kinetic-energy-test
              (let ([ke (kinetic-energy '(3 4) 2)])
                   ;; KE = 0.5 * 2 * (9+16) = 25
                   (assert-= ke 25 0.0001)))
            
            (define-test total-energy-test
              (let* ([pos '(0)]
                     [vel '(1)]
                     [mass 1]
                     [potential (lambda (p) (* 0.5 (car p) (car p)))]  ; Spring potential
                     [E (total-energy pos vel mass potential)])
                    ;; KE = 0.5*1*1 = 0.5, PE = 0
                    (assert-= E 0.5 0.0001)))
            
            (define-test energy-conservation-verlet
              ;; Harmonic oscillator with velocity Verlet should conserve energy well
              (let* ([a (lambda (t pos) (list (- (car pos))))]
                     [dt 0.01]
                     [steps 1000]
                     [results (integrate-symplectic velocity-verlet-step
                                                    a 0 '(1) '(0) dt steps)]
                     [potential (lambda (p) (* 0.5 (car p) (car p)))]
                     [energies (energy-drift results 1 potential)]
                     [drift (max-energy-drift energies)])
                    ;; Energy drift should be very small for symplectic integrator
                    (assert-true (< drift 0.001))))
            
            (define-test energy-drift-euler
              ;; Euler method should have larger energy drift
              (let* ([f (lambda (t state)
                                (let ([x (car state)]
                                      [v (cadr state)])
                                     (list v (- x))))]
                     [dt 0.01]
                     [steps 1000]
                     [states (integrate euler-step f 0 '(1 0) dt steps)]
                     [potential (lambda (p) (* 0.5 (car p) (car p)))]
                     ;; Convert to (pos, vel) format for energy calculation
                     [traj (map (lambda (s) (list (list (car s)) (list (cadr s)))) states)]
                     [energies (energy-drift traj 1 potential)])
                    ;; Euler should have noticeable energy growth
                    (assert-true (> (max-energy-drift energies) 0.01)))))

;;; ====
;;; Order of Accuracy Tests
;;; ====

(test-group accuracy-tests
            (define-test euler-is-first-order
              ;; dy/dt = y, exact solution y(t) = e^t
              ;; Error should halve when step size halves (first order)
              (let* ([f (lambda (t y) y)]
                     [dt1 0.1]
                     [dt2 0.05]
                     [result1 (euler-step f 0 '(1) dt1)]
                     [result2-a (euler-step f 0 '(1) dt2)]
                     [result2 (euler-step f dt2 result2-a dt2)]
                     [exact1 (exp dt1)]
                     [exact2 (exp (* 2 dt2))]
                     [error1 (abs (- (car result1) exact1))]
                     [error2 (abs (- (car result2) exact2))])
                    ;; Error ratio should be about 2 for first-order method
                    (assert-= (/ error1 error2) 2 0.5)))
            
            (define-test rk4-is-fourth-order
              ;; For small enough dt, RK4 error should scale as dt^5
              (let* ([f (lambda (t y) y)]
                     [dt1 0.1]
                     [dt2 0.05]
                     [steps1 10]
                     [steps2 20]
                     ;; Integrate to t=1
                     [results1 (integrate rk4-step f 0 '(1) dt1 steps1)]
                     [results2 (integrate rk4-step f 0 '(1) dt2 steps2)]
                     [final1 (car (list-ref results1 steps1))]
                     [final2 (car (list-ref results2 steps2))]
                     [exact (exp 1)]
                     [error1 (abs (- final1 exact))]
                     [error2 (abs (- final2 exact))])
                    ;; Error ratio should be about 16 (2^4) for fourth-order
                    (assert-true (> (/ error1 error2) 10)))))

;;; ====
;;; Summary
;;; ====

(display "
══════════════════════════════════════════════════════════
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All numerical integration tests passed.
")
    (display "
[FAILURE] Some numerical integration tests failed.
"))
