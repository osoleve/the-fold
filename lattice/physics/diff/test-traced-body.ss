(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/physics/diff/traced-body.ss")

(doc 'module 'test-traced-body)
(doc 'description "Tests for Traced Rigid Body

Comprehensive tests for differentiable rigid body operations including:
- Construction and accessors
- State updates (immutable)
- Conversion to/from regular rigid body
- Velocity at point
- Impulse and force application
- Coordinate transformations
- Energy and momentum")
(doc 'layer 'lattice)
(doc 'note "Run with: scheme --script lattice/physics/diff/test-traced-body.ss")

(display "
====
         TRACED RIGID BODY TESTS
====
")

;;; ====
;;; Test Utilities
;;; ====

(define (assert-near actual expected tolerance)
  (unless (< (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    FAIL: Expected ")
          (display expected)
          (display " +/- ")
          (display tolerance)
          (display ", got ")
          (display actual)
          (newline)))

(define (finite-diff f x epsilon)
  (/ (- (f (+ x epsilon)) (f (- x epsilon)))
     (* 2 epsilon)))

(define pi 3.141592653589793)

;;; ====
;;; Construction Tests
;;; ====

(test-group construction-tests
            
            (define-test make-traced-body-creates-valid
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 1 2) tape)]
                     [vel (lift-vec2 (vec2 3 4) tape)]
                     [angle (make-traced-var 0.5 tape)]
                     [omega (make-traced-var 0.1 tape)]
                     [body (make-traced-body pos vel angle omega 2 0.5)])
                    (assert-true (traced-body? body))))
            
            (define-test traced-body-accessors
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 5 6) tape)]
                     [vel (lift-vec2 (vec2 7 8) tape)]
                     [angle (make-traced-var 1.2 tape)]
                     [omega (make-traced-var -0.3 tape)]
                     [mass 3]
                     [inertia 0.8]
                     [body (make-traced-body pos vel angle omega mass inertia)])
                    (assert-near (traced-value (traced-vec2-x (traced-body-pos body))) 5 1e-10)
                    (assert-near (traced-value (traced-vec2-y (traced-body-pos body))) 6 1e-10)
                    (assert-near (traced-value (traced-vec2-x (traced-body-vel body))) 7 1e-10)
                    (assert-near (traced-value (traced-body-angle body)) 1.2 1e-10)
                    (assert-near (traced-value (traced-body-angular-vel body)) -0.3 1e-10)
                    (assert-near (traced-body-mass body) 3 1e-10)
                    (assert-near (traced-body-inertia body) 0.8 1e-10)))
            
            (define-test traced-body-inv-mass
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 4 2)])
                    (assert-near (traced-body-inv-mass body) 0.25 1e-10)
                    (assert-near (traced-body-inv-inertia body) 0.5 1e-10)))
            
            (define-test traced-body-static-detection
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     ;; mass = 0 means static
                     [static-body (make-traced-body pos vel angle omega 0 0)]
                     [dynamic-body (make-traced-body pos vel angle omega 1 0.5)])
                    (assert-true (traced-body-static? static-body))
                    (assert-false (traced-body-static? dynamic-body))))
            
            (define-test make-traced-static-body
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 10 20) tape)]
                     [angle (make-traced-var 0.5 tape)]
                     [body (make-traced-static-body pos angle)])
                    (assert-true (traced-body-static? body))
                    (assert-near (traced-body-mass body) 0 1e-10)))
            )

;;; ====
;;; Updater Tests
;;; ====

(test-group updater-tests
            
            (define-test traced-body-with-pos
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 1 2) tape)]
                     [vel (lift-vec2 (vec2 3 4) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [new-pos (lift-vec2 (vec2 10 20) tape)]
                     [new-body (traced-body-with-pos body new-pos)])
                    ;; New position
                    (assert-near (traced-value (traced-vec2-x (traced-body-pos new-body))) 10 1e-10)
                    ;; Velocity unchanged
                    (assert-near (traced-value (traced-vec2-x (traced-body-vel new-body))) 3 1e-10)))
            
            (define-test traced-body-with-vel
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 1 2) tape)]
                     [vel (lift-vec2 (vec2 3 4) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [new-vel (lift-vec2 (vec2 100 200) tape)]
                     [new-body (traced-body-with-vel body new-vel)])
                    ;; Position unchanged
                    (assert-near (traced-value (traced-vec2-x (traced-body-pos new-body))) 1 1e-10)
                    ;; New velocity
                    (assert-near (traced-value (traced-vec2-x (traced-body-vel new-body))) 100 1e-10)))
            
            (define-test traced-body-with-angle
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [new-angle (make-traced-var 1.5 tape)]
                     [new-body (traced-body-with-angle body new-angle)])
                    (assert-near (traced-value (traced-body-angle new-body)) 1.5 1e-10)))
            
            (define-test traced-body-with-state
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [new-pos (lift-vec2 (vec2 5 6) tape)]
                     [new-vel (lift-vec2 (vec2 7 8) tape)]
                     [new-angle (make-traced-var 2 tape)]
                     [new-omega (make-traced-var 3 tape)]
                     [new-body (traced-body-with-state body new-pos new-vel new-angle new-omega)])
                    (assert-near (traced-value (traced-vec2-x (traced-body-pos new-body))) 5 1e-10)
                    (assert-near (traced-value (traced-vec2-x (traced-body-vel new-body))) 7 1e-10)
                    (assert-near (traced-value (traced-body-angle new-body)) 2 1e-10)
                    (assert-near (traced-value (traced-body-angular-vel new-body)) 3 1e-10)
                    ;; Mass preserved
                    (assert-near (traced-body-mass new-body) 1 1e-10)))
            )

;;; ====
;;; Conversion Tests
;;; ====

(test-group conversion-tests
            
            (define-test trace-rigid-body-conversion
              (let* ([rb (make-rigid-body (vec2 1 2) (vec2 3 4) 0.5 0.1 2 0.8)]
                     [tape (make-reverse-tape)]
                     [tb (trace-rigid-body rb tape)])
                    (assert-true (traced-body? tb))
                    (assert-near (traced-value (traced-vec2-x (traced-body-pos tb))) 1 1e-10)
                    (assert-near (traced-body-mass tb) 2 1e-10)))
            
            (define-test unpack-traced-body-roundtrip
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 1 2) tape)]
                     [vel (lift-vec2 (vec2 3 4) tape)]
                     [angle (make-traced-var 0.5 tape)]
                     [omega (make-traced-var 0.1 tape)]
                     [tb (make-traced-body pos vel angle omega 2 0.8)]
                     [rb (unpack-traced-body tb)])
                    (assert-near (vec2-x (rigid-body-pos rb)) 1 1e-10)
                    (assert-near (vec2-y (rigid-body-pos rb)) 2 1e-10)
                    (assert-near (rigid-body-angle rb) 0.5 1e-10)
                    (assert-near (rigid-body-mass rb) 2 1e-10)))
            
            (define-test traced-body-state->list-structure
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 1 2) tape)]
                     [vel (lift-vec2 (vec2 3 4) tape)]
                     [angle (make-traced-var 5 tape)]
                     [omega (make-traced-var 6 tape)]
                     [tb (make-traced-body pos vel angle omega 1 0.5)]
                     [lst (traced-body-state->list tb)])
                    (assert-equal (length lst) 6)
                    ;; px, py, vx, vy, angle, omega
                    (assert-near (traced-value (list-ref lst 0)) 1 1e-10)
                    (assert-near (traced-value (list-ref lst 4)) 5 1e-10)))
            
            (define-test list->traced-body-state-roundtrip
              (let* ([tape (make-reverse-tape)]
                     [vals (list (make-traced-var 1 tape)
                                 (make-traced-var 2 tape)
                                 (make-traced-var 3 tape)
                                 (make-traced-var 4 tape)
                                 (make-traced-var 5 tape)
                                 (make-traced-var 6 tape))]
                     [mass 2]
                     [inertia 0.5]
                     [tb (list->traced-body-state vals mass inertia)])
                    (assert-near (traced-value (traced-vec2-x (traced-body-pos tb))) 1 1e-10)
                    (assert-near (traced-body-mass tb) 2 1e-10)))
            
            (define-test body-state-dimension
              (assert-equal (body-state-dimension) 6))
            )

;;; ====
;;; Velocity At Point Tests
;;; ====

(test-group velocity-at-tests
            
            (define-test velocity-at-center-equals-vel
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 5 3) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 1 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     ;; Point at center of mass
                     [point (lift-vec2 (vec2 0 0) tape)]
                     [vel-at (traced-body-velocity-at body point)])
                    ;; r = 0, so rotational contribution is 0
                    (assert-near (traced-value (traced-vec2-x vel-at)) 5 1e-10)
                    (assert-near (traced-value (traced-vec2-y vel-at)) 3 1e-10)))
            
            (define-test velocity-at-offset-point
              ;; Body at origin, rotating with omega = 1 rad/s
              ;; Point at (1, 0) -> rotational velocity = omega * perp(r) = 1 * (0, 1)
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 1 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [point (lift-vec2 (vec2 1 0) tape)]
                     [vel-at (traced-body-velocity-at body point)])
                    ;; v_point = v_cm + omega * perp(r) = (0,0) + 1 * (-0, 1) = (0, 1)
                    (assert-near (traced-value (traced-vec2-x vel-at)) 0 1e-10)
                    (assert-near (traced-value (traced-vec2-y vel-at)) 1 1e-10)))
            
            (define-test velocity-at-combined
              ;; Body moving and rotating
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 2 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 1 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [point (lift-vec2 (vec2 0 1) tape)]  ; 1 unit above center
                     [vel-at (traced-body-velocity-at body point)])
                    ;; r = (0, 1), perp(r) = (-1, 0) * omega = (-1, 0)
                    ;; v_point = (2, 0) + (-1, 0) = (1, 0)
                    (assert-near (traced-value (traced-vec2-x vel-at)) 1 1e-10)
                    (assert-near (traced-value (traced-vec2-y vel-at)) 0 1e-10)))
            )

;;; ====
;;; Impulse and Force Tests
;;; ====

(test-group impulse-force-tests
            
            (define-test apply-impulse-at-center
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 2 1)]  ; mass = 2
                     [impulse (lift-vec2 (vec2 4 0) tape)]
                     [contact (lift-vec2 (vec2 0 0) tape)]  ; at center
                     [new-body (traced-apply-impulse body impulse contact)])
                    ;; dv = J / m = 4 / 2 = 2
                    (assert-near (traced-value (traced-vec2-x (traced-body-vel new-body))) 2 1e-10)
                    ;; No torque (impulse through center)
                    (assert-near (traced-value (traced-body-angular-vel new-body)) 0 1e-10)))
            
            (define-test apply-impulse-off-center
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 1)]  ; mass = 1, I = 1
                     [impulse (lift-vec2 (vec2 1 0) tape)]  ; rightward impulse
                     [contact (lift-vec2 (vec2 0 1) tape)]  ; 1 unit above center
                     [new-body (traced-apply-impulse body impulse contact)])
                    ;; r = (0, 1), J = (1, 0)
                    ;; r x J = 0*0 - 1*1 = -1
                    ;; domega = -1 / 1 = -1 (clockwise)
                    (assert-near (traced-value (traced-body-angular-vel new-body)) -1 1e-10)))
            
            (define-test apply-impulse-static-unchanged
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [body (make-traced-static-body pos angle)]
                     [impulse (lift-vec2 (vec2 100 100) tape)]
                     [contact (lift-vec2 (vec2 1 1) tape)]
                     [new-body (traced-apply-impulse body impulse contact)])
                    ;; Static body should not move
                    (assert-near (traced-value (traced-vec2-x (traced-body-vel new-body))) 0 1e-10)))
            
            (define-test apply-central-force
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 2 1)]
                     [force (lift-vec2 (vec2 10 0) tape)]
                     [dt 0.1]
                     [new-body (traced-apply-central-force body force dt)])
                    ;; impulse = F * dt = 1
                    ;; dv = 1 / 2 = 0.5
                    (assert-near (traced-value (traced-vec2-x (traced-body-vel new-body))) 0.5 1e-10)))
            
            (define-test apply-torque
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 2)]  ; I = 2
                     [torque (make-traced-var 4 tape)]
                     [dt 0.5]
                     [new-body (traced-apply-torque body torque dt)])
                    ;; domega = tau * dt / I = 4 * 0.5 / 2 = 1
                    (assert-near (traced-value (traced-body-angular-vel new-body)) 1 1e-10)))
            )

;;; ====
;;; Coordinate Transformation Tests
;;; ====

(test-group transform-tests
            
            (define-test local-to-world-no-rotation
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 5 3) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [local (lift-vec2 (vec2 1 2) tape)]
                     [world (traced-local-to-world body local)])
                    ;; world = pos + local = (5,3) + (1,2) = (6, 5)
                    (assert-near (traced-value (traced-vec2-x world)) 6 1e-10)
                    (assert-near (traced-value (traced-vec2-y world)) 5 1e-10)))
            
            (define-test local-to-world-with-rotation
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var (/ pi 2) tape)]  ; 90 degrees
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [local (lift-vec2 (vec2 1 0) tape)]
                     [world (traced-local-to-world body local)])
                    ;; (1, 0) rotated 90 degrees -> (0, 1)
                    (assert-near (traced-value (traced-vec2-x world)) 0 1e-6)
                    (assert-near (traced-value (traced-vec2-y world)) 1 1e-6)))
            
            (define-test world-to-local-roundtrip
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 3 4) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0.5 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [local-orig (lift-vec2 (vec2 1 2) tape)]
                     [world (traced-local-to-world body local-orig)]
                     [local-back (traced-world-to-local body world)])
                    (assert-near (traced-value (traced-vec2-x local-back)) 1 1e-6)
                    (assert-near (traced-value (traced-vec2-y local-back)) 2 1e-6)))
            
            (define-test local-dir-to-world
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 10 10) tape)]  ; position shouldn't matter
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var (/ pi 2) tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [local-dir (lift-vec2 (vec2 1 0) tape)]
                     [world-dir (traced-local-dir-to-world body local-dir)])
                    ;; (1, 0) rotated 90 degrees -> (0, 1)
                    (assert-near (traced-value (traced-vec2-x world-dir)) 0 1e-6)
                    (assert-near (traced-value (traced-vec2-y world-dir)) 1 1e-6)))
            )

;;; ====
;;; Energy and Momentum Tests
;;; ====

(test-group energy-momentum-tests
            
            (define-test kinetic-energy-linear-only
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 3 4) tape)]  ; |v| = 5
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 2 1)]  ; m = 2
                     [ke (traced-body-kinetic-energy body)])
                    ;; KE = 0.5 * m * v^2 = 0.5 * 2 * 25 = 25
                    (assert-near (traced-value ke) 25 1e-10)))
            
            (define-test kinetic-energy-rotational-only
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 3 tape)]  ; omega = 3
                     [body (make-traced-body pos vel angle omega 1 2)]  ; I = 2
                     [ke (traced-body-kinetic-energy body)])
                    ;; KE = 0.5 * I * omega^2 = 0.5 * 2 * 9 = 9
                    (assert-near (traced-value ke) 9 1e-10)))
            
            (define-test kinetic-energy-combined
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 3 4) tape)]  ; |v|^2 = 25
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 2 tape)]
                     [body (make-traced-body pos vel angle omega 2 1)]
                     [ke (traced-body-kinetic-energy body)])
                    ;; KE = 0.5 * 2 * 25 + 0.5 * 1 * 4 = 25 + 2 = 27
                    (assert-near (traced-value ke) 27 1e-10)))
            
            (define-test linear-momentum
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 5 7) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 3 1)]
                     [p (traced-body-momentum body)])
                    ;; p = m * v = 3 * (5, 7) = (15, 21)
                    (assert-near (traced-value (traced-vec2-x p)) 15 1e-10)
                    (assert-near (traced-value (traced-vec2-y p)) 21 1e-10)))
            
            (define-test angular-momentum
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 4 tape)]
                     [body (make-traced-body pos vel angle omega 1 2.5)]
                     [L (traced-body-angular-momentum body)])
                    ;; L = I * omega = 2.5 * 4 = 10
                    (assert-near (traced-value L) 10 1e-10)))
            )

;;; ====
;;; Inertia Calculation Tests
;;; ====

(test-group inertia-tests
            
            (define-test circle-inertia-formula
              ;; I = 0.5 * m * r^2
              (let ([I (traced-circle-inertia 2 3)])
                   (assert-near I (* 0.5 2 9) 1e-10)))
            
            (define-test rectangle-inertia-formula
              ;; I = m * (w^2 + h^2) / 12
              (let ([I (traced-rectangle-inertia 6 3 4)])
                   ;; 6 * (9 + 16) / 12 = 6 * 25 / 12 = 12.5
                   (assert-near I 12.5 1e-10)))
            )

;;; ====
;;; Gradient Tests
;;; ====

(test-group gradient-tests
            
            (define-test velocity-at-gradient
              ;; Verify gradient of velocity at point w.r.t. omega
              (let* ([test-fn (lambda (omega)
                                      (let* ([tape (make-reverse-tape)]
                                             [pos (lift-vec2 (vec2 0 0) tape)]
                                             [vel (lift-vec2 (vec2 0 0) tape)]
                                             [angle (make-traced-var 0 tape)]
                                             [om (make-traced-var omega tape)]
                                             [body (make-traced-body pos vel angle om 1 0.5)]
                                             [point (lift-vec2 (vec2 1 0) tape)]
                                             [v-at (traced-body-velocity-at body point)])
                                            (traced-value (traced-vec2-y v-at))))]
                     [omega0 2]
                     [numerical (finite-diff test-fn omega0 1e-5)]
                     ;; v_y = omega * r_x = omega * 1
                     ;; d(v_y)/d(omega) = 1
                     [analytical 1])
                    (assert-near numerical analytical 1e-4)))
            
            (define-test impulse-gradient-correct
              ;; Verify gradient flows through impulse application using finite diff
              (let* ([test-fn (lambda (jx)
                                      (let* ([tape (make-reverse-tape)]
                                             [pos (lift-vec2 (vec2 0 0) tape)]
                                             [vel (lift-vec2 (vec2 0 0) tape)]
                                             [angle (make-traced-var 0 tape)]
                                             [omega (make-traced-var 0 tape)]
                                             [body (make-traced-body pos vel angle omega 2 1)]
                                             [impulse (traced-vec2 (make-traced-var jx tape)
                                                                   (make-traced-var 0 tape))]
                                             [contact (lift-vec2 (vec2 0 0) tape)]
                                             [new-body (traced-apply-impulse body impulse contact)])
                                            (traced-value (traced-vec2-x (traced-body-vel new-body)))))]
                     [jx0 1]
                     [numerical (finite-diff test-fn jx0 1e-5)])
                    ;; d(vx')/d(jx) = 1/m = 0.5
                    (assert-near numerical 0.5 1e-4)))
            )

;;; ====
;;; Run All Tests
;;; ====

(newline)
(display "----")
(newline)
(display "  Test Summary:")
(newline)
(display "----")
(newline)
(display "  Total:  ")
(display *tests-run*)
(display " tests")
(newline)
(display "  Passed: ")
(display *tests-passed*)
(newline)
(display "  Failed: ")
(display *tests-failed*)
(newline)
(newline)

(when (> *tests-failed* 0)
      (exit 1))
