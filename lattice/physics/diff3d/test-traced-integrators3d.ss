;;; core/diff-physics-3d/test-traced-integrators3d.ss --- Tests for traced-integrators3d
;;;
;;; Run with: scheme --script core/diff-physics-3d/test-traced-integrators3d.ss

(load "core/test-framework.ss")
(load "lattice/physics/diff3d/traced-integrators3d.ss")

(display "\n")
(display "==============================================================\n")
(display "         TRACED INTEGRATORS 3D TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Helper to create a simple body
;;; ============================================================

(define (make-test-body tape pos-vec vel-vec angular-vel-vec)
  (make-traced-body-3d (lift-vec3 pos-vec tape)
                       (lift-vec3 vel-vec tape)
                       (lift-quat (quat-identity) tape)
                       (lift-vec3 angular-vel-vec tape)
                       1.0
                       (inertia-solid-sphere 1.0 1.0)))

;;; ============================================================
;;; Euler Step Tests
;;; ============================================================

(test-group euler-step-tests
            
            (define-test static-body-no-movement
              (let* ([tape (make-reverse-tape)]
                     [b (make-traced-static-body-3d (lift-vec3 (vec3 0 0 0) tape)
                                                    (lift-quat (quat-identity) tape))]
                     [accel (lift-vec3 (vec3 1 0 0) tape)]
                     [angular-accel (lift-vec3 (vec3 0 0 1) tape)]
                     [b2 (traced-euler-step-3d b accel angular-accel 0.1)]
                     [pos (traced-body-3d-pos b2)])
                    (assert-true (< (abs (traced-value (traced-vec3-x pos))) 1e-10))))
            
            (define-test constant-velocity-motion
              ;; No acceleration, just constant velocity
              (let* ([tape (make-reverse-tape)]
                     [b (make-test-body tape (vec3 0 0 0) (vec3 1 0 0) (vec3 0 0 0))]
                     [accel (lift-vec3 (vec3 0 0 0) tape)]
                     [angular-accel (lift-vec3 (vec3 0 0 0) tape)]
                     [b2 (traced-euler-step-3d b accel angular-accel 1.0)]
                     [pos (traced-body-3d-pos b2)])
                    ;; After 1 second at v=1, x should be 1
                    (assert-true (< (abs (- (traced-value (traced-vec3-x pos)) 1)) 1e-10))))
            
            (define-test constant-acceleration
              ;; a = (1, 0, 0), v0 = 0, after dt=1: v = 1, x = 1 (symplectic: uses new v)
              (let* ([tape (make-reverse-tape)]
                     [b (make-test-body tape (vec3 0 0 0) (vec3 0 0 0) (vec3 0 0 0))]
                     [accel (lift-vec3 (vec3 1 0 0) tape)]
                     [angular-accel (lift-vec3 (vec3 0 0 0) tape)]
                     [b2 (traced-euler-step-3d b accel angular-accel 1.0)]
                     [pos (traced-body-3d-pos b2)]
                     [vel (traced-body-3d-vel b2)])
                    ;; Velocity should be 1
                    (assert-true (< (abs (- (traced-value (traced-vec3-x vel)) 1)) 1e-10))
                    ;; Position should be 1 (symplectic uses new velocity)
                    (assert-true (< (abs (- (traced-value (traced-vec3-x pos)) 1)) 1e-10))))
            
            (define-test angular-acceleration
              ;; alpha = (0, 0, 1), omega0 = 0, after dt=1: omega = 1
              (let* ([tape (make-reverse-tape)]
                     [b (make-test-body tape (vec3 0 0 0) (vec3 0 0 0) (vec3 0 0 0))]
                     [accel (lift-vec3 (vec3 0 0 0) tape)]
                     [angular-accel (lift-vec3 (vec3 0 0 1) tape)]
                     [b2 (traced-euler-step-3d b accel angular-accel 1.0)]
                     [omega (traced-body-3d-angular-vel b2)])
                    (assert-true (< (abs (- (traced-value (traced-vec3-z omega)) 1)) 1e-10)))))

;;; ============================================================
;;; Gravity Step Tests
;;; ============================================================

(test-group gravity-tests
            
            (define-test gravity-step
              ;; Free fall under gravity
              (let* ([tape (make-reverse-tape)]
                     [b (make-test-body tape (vec3 0 10 0) (vec3 0 0 0) (vec3 0 0 0))]
                     [gravity (vec3 0 -10 0)]  ; 10 m/s² downward
                     [b2 (traced-gravity-step-3d-const b gravity 1.0)]
                     [pos (traced-body-3d-pos b2)]
                     [vel (traced-body-3d-vel b2)])
                    ;; After 1 second: v = -10, y = 10 + (-10)*1 = 0
                    (assert-true (< (abs (- (traced-value (traced-vec3-y vel)) -10)) 1e-10))
                    (assert-true (< (abs (traced-value (traced-vec3-y pos))) 1e-10)))))

;;; ============================================================
;;; Multi-Step Integration Tests
;;; ============================================================

(test-group multi-step-tests
            
            (define-test n-steps-constant-velocity
              ;; v = (1, 0, 0), x0 = 0, after 10 steps of dt=0.1 each: x = 1
              (let* ([tape (make-reverse-tape)]
                     [b (make-test-body tape (vec3 0 0 0) (vec3 1 0 0) (vec3 0 0 0))]
                     [compute-accel (lambda (body)
                                            (values (lift-vec3 (vec3 0 0 0) tape)
                                                    (lift-vec3 (vec3 0 0 0) tape)))]
                     [b2 (traced-integrate-n-steps-3d b compute-accel 0.1 10)]
                     [pos (traced-body-3d-pos b2)])
                    (assert-true (< (abs (- (traced-value (traced-vec3-x pos)) 1)) 1e-5))))
            
            (define-test integrate-until
              ;; Same as above but using integrate-until
              (let* ([tape (make-reverse-tape)]
                     [b (make-test-body tape (vec3 0 0 0) (vec3 1 0 0) (vec3 0 0 0))]
                     [compute-accel (lambda (body)
                                            (values (lift-vec3 (vec3 0 0 0) tape)
                                                    (lift-vec3 (vec3 0 0 0) tape)))]
                     [b2 (traced-integrate-until-3d b compute-accel 0.1 1.0)]
                     [pos (traced-body-3d-pos b2)])
                    (assert-true (< (abs (- (traced-value (traced-vec3-x pos)) 1)) 1e-5)))))

;;; ============================================================
;;; Point Mass Integration Tests
;;; ============================================================

(test-group point-mass-tests
            
            (define-test projectile-motion
              ;; Initial velocity (10, 10, 0), gravity (0, -10, 0)
              ;; After 1 second: v = (10, 0, 0), pos = (10, 5, 0)
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 10 10 0) tape)]
                     [gravity (lift-vec3 (vec3 0 -10 0) tape)])
                    (call-with-values
                     (lambda () (traced-projectile-step-3d pos vel gravity 1.0))
                     (lambda (new-pos new-vel)
                             ;; Velocity: vx = 10, vy = 10 + (-10)*1 = 0
                             (assert-true (< (abs (- (traced-value (traced-vec3-x new-vel)) 10)) 1e-10))
                             (assert-true (< (abs (traced-value (traced-vec3-y new-vel))) 1e-10))
                             ;; Position (symplectic): x = 0 + 10*1 = 10, y = 0 + 0*1 = 0
                             (assert-true (< (abs (- (traced-value (traced-vec3-x new-pos)) 10)) 1e-10))
                             (assert-true (< (abs (traced-value (traced-vec3-y new-pos))) 1e-10)))))))

;;; ============================================================
;;; Spring Force Tests
;;; ============================================================

(test-group spring-tests
            
            (define-test spring-force-at-rest
              ;; Point at rest length from anchor: no force
              (let* ([tape (make-reverse-tape)]
                     [anchor (lift-vec3 (vec3 0 0 0) tape)]
                     [point (lift-vec3 (vec3 1 0 0) tape)]  ; rest length = 1
                     [f (traced-spring-force-3d anchor point 10.0 1.0)])
                    (assert-true (< (traced-value (traced-vec3-magnitude f)) 1e-5))))
            
            (define-test spring-force-stretched
              ;; Point stretched to distance 2, rest length 1: force = k * 1 = 10
              (let* ([tape (make-reverse-tape)]
                     [anchor (lift-vec3 (vec3 0 0 0) tape)]
                     [point (lift-vec3 (vec3 2 0 0) tape)]
                     [f (traced-spring-force-3d anchor point 10.0 1.0)])
                    ;; Force should be toward anchor, magnitude 10
                    (assert-true (< (abs (- (traced-value (traced-vec3-x f)) -10)) 1e-5))
                    (assert-true (< (abs (traced-value (traced-vec3-y f))) 1e-10))))
            
            (define-test spring-force-compressed
              ;; Point compressed to distance 0.5, rest length 1: force = k * (-0.5) = -5 (outward)
              (let* ([tape (make-reverse-tape)]
                     [anchor (lift-vec3 (vec3 0 0 0) tape)]
                     [point (lift-vec3 (vec3 0.5 0 0) tape)]
                     [f (traced-spring-force-3d anchor point 10.0 1.0)])
                    ;; Force should be away from anchor (positive x), magnitude 5
                    (assert-true (< (abs (- (traced-value (traced-vec3-x f)) 5)) 1e-5)))))

;;; ============================================================
;;; Energy Tests
;;; ============================================================

(test-group energy-tests
            
            (define-test total-energy-constant-under-free-fall
              ;; In free fall, total energy should be roughly conserved
              ;; First-order Euler has some drift; we test that it's bounded
              (let* ([tape (make-reverse-tape)]
                     [b0 (make-test-body tape (vec3 0 10 0) (vec3 0 0 0) (vec3 0 0 0))]
                     [gravity (vec3 0 -10 0)]
                     [potential-fn (lambda (body)
                                           ;; PE = m * g * h = 1 * 10 * y
                                           (traced-mul 10 (traced-vec3-y (traced-body-3d-pos body))))]
                     [e0 (traced-total-energy-3d b0 potential-fn)]
                     ;; Integrate for 10 steps
                     [b-final (let loop ([b b0] [n 10])
                                   (if (= n 0) b
                                       (loop (traced-gravity-step-3d-const b gravity 0.1) (- n 1))))]
                     [e-final (traced-total-energy-3d b-final potential-fn)]
                     [energy-drift (abs (- (traced-value e-final) (traced-value e0)))]
                     [relative-drift (/ energy-drift (traced-value e0))])
                    ;; Energy drift should be bounded (allow 10% for first-order Euler)
                    (assert-true (< relative-drift 0.1)))))  ; Allow some drift for this test

;;; ============================================================
;;; Damping Tests
;;; ============================================================

(test-group damping-tests
            
            (define-test semi-implicit-with-damping
              ;; Velocity should decrease with damping
              (let* ([tape (make-reverse-tape)]
                     [b (make-test-body tape (vec3 0 0 0) (vec3 10 0 0) (vec3 0 0 0))]
                     [accel (lift-vec3 (vec3 0 0 0) tape)]
                     [angular-accel (lift-vec3 (vec3 0 0 0) tape)]
                     [b2 (traced-semi-implicit-step-3d b accel angular-accel 0.1 0.9)]
                     [vel (traced-body-3d-vel b2)])
                    ;; v_new = v * 0.9 = 9
                    (assert-true (< (abs (- (traced-value (traced-vec3-x vel)) 9)) 1e-10)))))

;;; ============================================================
;;; Trajectory Recording Tests
;;; ============================================================

(test-group trajectory-tests
            
            (define-test trajectory-recording
              (let* ([tape (make-reverse-tape)]
                     [b (make-test-body tape (vec3 0 0 0) (vec3 1 0 0) (vec3 0 0 0))]
                     [compute-accel (lambda (body)
                                            (values (lift-vec3 (vec3 0 0 0) tape)
                                                    (lift-vec3 (vec3 0 0 0) tape)))]
                     [traj (traced-integrate-with-trajectory-3d b compute-accel 0.1 5)])
                    ;; Should have 6 states (initial + 5 steps)
                    (assert-equal 6 (length traj))
                    ;; First position should be at origin
                    (assert-true (< (abs (traced-value (traced-vec3-x (traced-body-3d-pos (car traj))))) 1e-10))
                    ;; Last position should be at x = 0.5 (5 steps * 0.1 dt * 1 velocity)
                    (assert-true (< (abs (- (traced-value (traced-vec3-x (traced-body-3d-pos (car (reverse traj))))) 0.5)) 1e-5)))))

;;; ============================================================
;;; Run all tests
;;; ============================================================

(run-all-tests)
