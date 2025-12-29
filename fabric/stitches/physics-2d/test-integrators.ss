;;; fabric/stitches/physics-2d/test-integrators.ss — Tests for 2D Physics Integration

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/physics-2d/integrators.ss")

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

;;; Helper for vec2 comparison
(define (assert-vec2-= actual expected tolerance)
  (assert-= (vec2-x actual) (vec2-x expected) tolerance)
  (assert-= (vec2-y actual) (vec2-y expected) tolerance))

(display "
══════════════════════════════════════════════════════════
         2D PHYSICS INTEGRATION TESTS
══════════════════════════════════════════════════════════
")

;;; ============================================================
;;; Body Creation Tests
;;; ============================================================

(test-group body-creation-tests
            (define-test make-body-2d-test
              (let ([b (make-body-2d (vec2 10 20) (vec2 1 2) 5)])
                   (assert-true (body-2d? b))
                   (assert-vec2-= (body-pos b) (vec2 10 20) 0.0001)
                   (assert-vec2-= (body-vel b) (vec2 1 2) 0.0001)
                   (assert-= (body-mass b) 5 0.0001)
                   (assert-= (body-inv-mass b) 0.2 0.0001)))
            
            (define-test static-body-test
              (let ([b (make-static-body (vec2 50 50))])
                   (assert-true (body-static? b))
                   (assert-= (body-inv-mass b) 0 0.0001)
                   (assert-vec2-= (body-vel b) (vec2 0 0) 0.0001)))
            
            (define-test body-with-pos-test
              (let* ([b1 (make-body-2d (vec2 0 0) (vec2 1 1) 1)]
                     [b2 (body-with-pos b1 (vec2 10 20))])
                    (assert-vec2-= (body-pos b2) (vec2 10 20) 0.0001)
                    (assert-vec2-= (body-vel b2) (vec2 1 1) 0.0001)))
            
            (define-test body-with-vel-test
              (let* ([b1 (make-body-2d (vec2 5 5) (vec2 0 0) 1)]
                     [b2 (body-with-vel b1 (vec2 3 4))])
                    (assert-vec2-= (body-pos b2) (vec2 5 5) 0.0001)
                    (assert-vec2-= (body-vel b2) (vec2 3 4) 0.0001))))

;;; ============================================================
;;; Euler Integration Tests
;;; ============================================================

(test-group euler-integration-tests
            (define-test euler-no-force
              ;; Body moving at constant velocity
              (let* ([b (make-body-2d (vec2 0 0) (vec2 10 5) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [b2 (integrate-body-euler b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 10 5) 0.0001)
                    (assert-vec2-= (body-vel b2) (vec2 10 5) 0.0001)))
            
            (define-test euler-constant-force
              ;; Body under constant acceleration
              (let* ([b (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [f (lambda (body) (vec2 10 0))]  ; F = 10, a = 10
                     [b2 (integrate-body-euler b f 1)])
                    ;; v' = 0 + 10*1 = 10, x' = 0 + 10*1 = 10
                    (assert-vec2-= (body-vel b2) (vec2 10 0) 0.0001)
                    (assert-vec2-= (body-pos b2) (vec2 10 0) 0.0001)))
            
            (define-test euler-static-body
              ;; Static body should not move
              (let* ([b (make-static-body (vec2 50 50))]
                     [f (lambda (body) (vec2 1000 1000))]
                     [b2 (integrate-body-euler b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 50 50) 0.0001))))

;;; ============================================================
;;; Symplectic Integration Tests
;;; ============================================================

(test-group symplectic-integration-tests
            (define-test symplectic-no-force
              (let* ([b (make-body-2d (vec2 0 0) (vec2 5 3) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [b2 (integrate-body-symplectic b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 5 3) 0.0001)))
            
            (define-test symplectic-gravity
              ;; Test with gravity-like force
              (let* ([b (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [g -10]  ; Gravity pointing down (positive y)
                     [f (gravity-force (- g))]  ; flip for our coords
                     [b2 (integrate-body-symplectic b f 1)])
                    ;; v = 0 + 10*1 = 10 (downward)
                    ;; x = 0 + 10*1 = 10
                    (assert-= (vec2-y (body-vel b2)) 10 0.0001)
                    (assert-= (vec2-y (body-pos b2)) 10 0.0001))))

;;; ============================================================
;;; Verlet Integration Tests
;;; ============================================================

(test-group verlet-integration-tests
            (define-test verlet-free-particle
              (let* ([b (make-body-2d (vec2 0 0) (vec2 10 0) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [b2 (integrate-body-verlet b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 10 0) 0.0001)
                    (assert-vec2-= (body-vel b2) (vec2 10 0) 0.0001)))
            
            (define-test verlet-constant-accel
              ;; a = 10, x(0) = 0, v(0) = 0, dt = 1
              ;; x = 0 + 0 + 0.5*10*1 = 5
              ;; v = 0 + 0.5*(10+10)*1 = 10
              (let* ([b (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [f (lambda (body) (vec2 10 0))]
                     [b2 (integrate-body-verlet b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 5 0) 0.0001)
                    (assert-vec2-= (body-vel b2) (vec2 10 0) 0.0001)))
            
            (define-test verlet-harmonic-one-step
              ;; Harmonic oscillator: F = -kx (k=1, m=1)
              ;; Starting at x=1, v=0
              (let* ([k 1]
                     [b (make-body-2d (vec2 1 0) (vec2 0 0) 1)]
                     [f (lambda (body)
                                (let ([x (vec2-x (body-pos body))])
                                     (vec2 (- (* k x)) 0)))]
                     [b2 (integrate-body-verlet b f 0.1)])
                    ;; Should be close to cos(0.1), -sin(0.1)
                    (assert-= (vec2-x (body-pos b2)) (cos 0.1) 0.01)
                    (assert-= (vec2-x (body-vel b2)) (- (sin 0.1)) 0.01))))

;;; ============================================================
;;; Time Accumulator Tests
;;; ============================================================

(test-group time-acc-tests
            (define-test make-time-acc-test
              (let ([t (make-time-acc 0.016 10)])
                   (assert-true (time-acc? t))
                   (assert-= (time-acc-remaining t) 0 0.0001)
                   (assert-= (time-acc-fixed-dt t) 0.016 0.0001)
                   (assert-= (time-acc-max-steps t) 10 0.0001)))
            
            (define-test time-acc-add-test
              (let* ([t (make-time-acc 0.016 10)]
                     [t2 (time-acc-add t 0.033)])
                    (assert-= (time-acc-remaining t2) 0.033 0.0001)))
            
            (define-test time-acc-consume-test
              (let* ([t (make-time-acc 0.016 10)]
                     [t2 (time-acc-add t 0.050)]
                     [result (time-acc-consume t2)]
                     [t3 (car result)]
                     [consumed (cadr result)])
                    (assert-= consumed 1 0.0001)
                    (assert-= (time-acc-remaining t3) 0.034 0.001)))
            
            (define-test time-acc-consume-insufficient
              (let* ([t (make-time-acc 0.016 10)]
                     [t2 (time-acc-add t 0.010)]
                     [result (time-acc-consume t2)]
                     [consumed (cadr result)])
                    (assert-= consumed 0 0.0001)))
            
            (define-test time-acc-alpha-test
              (let* ([t (make-time-acc 0.016 10)]
                     [t2 (time-acc-add t 0.008)])
                    (assert-= (time-acc-alpha t2) 0.5 0.0001))))

;;; ============================================================
;;; Sub-stepping Tests
;;; ============================================================

(test-group substep-tests
            (define-test substep-single
              ;; One substep should equal regular integration
              (let* ([b (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [f (lambda (body) (vec2 10 0))]
                     [b-sub (substep-body b f 1 1 'euler)]
                     [b-reg (integrate-body-euler b f 1)])
                    (assert-vec2-= (body-pos b-sub) (body-pos b-reg) 0.0001)))
            
            (define-test substep-multiple
              ;; Multiple substeps should be more accurate
              (let* ([b (make-body-2d (vec2 0 0) (vec2 10 0) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [b-1 (substep-body b f 1 1 'euler)]
                     [b-10 (substep-body b f 1 10 'euler)])
                    ;; With no force, all should reach same position
                    (assert-vec2-= (body-pos b-1) (vec2 10 0) 0.0001)
                    (assert-vec2-= (body-pos b-10) (vec2 10 0) 0.0001))))

;;; ============================================================
;;; Force Function Tests
;;; ============================================================

(test-group force-function-tests
            (define-test gravity-force-test
              (let* ([f (gravity-force 9.8)]
                     [b (make-body-2d (vec2 0 0) (vec2 0 0) 10)]
                     [force (f b)])
                    (assert-= (vec2-y force) 98 0.01)))
            
            (define-test spring-force-test
              (let* ([anchor (vec2 0 0)]
                     [k 100]
                     [f (spring-force anchor k 0)]
                     [b (make-body-2d (vec2 1 0) (vec2 0 0) 1)]
                     [force (f b)])
                    ;; F = -k*x = -100*(-1) = -100 toward anchor
                    (assert-= (vec2-x force) -100 0.01)))
            
            (define-test drag-force-test
              (let* ([f (drag-force 0.5)]
                     [b (make-body-2d (vec2 0 0) (vec2 10 0) 1)]
                     [force (f b)])
                    (assert-= (vec2-x force) -5 0.01)))
            
            (define-test combine-forces-test
              (let* ([f1 (constant-force (vec2 10 0))]
                     [f2 (constant-force (vec2 0 5))]
                     [f (combine-forces f1 f2)]
                     [b (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [force (f b)])
                    (assert-vec2-= force (vec2 10 5) 0.0001))))

;;; ============================================================
;;; Energy Tests
;;; ============================================================

(test-group energy-tests
            (define-test kinetic-energy-test
              (let ([b (make-body-2d (vec2 0 0) (vec2 3 4) 2)])
                   ;; KE = 0.5 * 2 * (9+16) = 25
                   (assert-= (body-kinetic-energy b) 25 0.0001)))
            
            (define-test potential-energy-test
              ;; Body at y=10, g=10, y-ref=0
              ;; PE = m*g*(y-ref - y) = 1*10*(0-10) = -100
              (let ([b (make-body-2d (vec2 0 10) (vec2 0 0) 1)])
                   (assert-= (body-potential-energy b 10 0) -100 0.0001)))
            
            (define-test total-energy-conservation
              ;; Harmonic oscillator energy should be conserved
              (let* ([k 1]
                     [b0 (make-body-2d (vec2 1 0) (vec2 0 0) 1)]
                     [f (lambda (body)
                                (let ([x (vec2-x (body-pos body))])
                                     (vec2 (- (* k x)) 0)))]
                     ;; Integrate many steps
                     [steps 1000]
                     [dt 0.01]
                     [final-b (let loop ([b b0] [i 0])
                                   (if (>= i steps)
                                       b
                                       (loop (integrate-body-verlet b f dt) (+ i 1))))]
                     ;; Spring energy
                     [e0 (body-spring-energy b0 (vec2 0 0) k)]
                     [ef (+ (body-kinetic-energy final-b)
                            (body-spring-energy final-b (vec2 0 0) k))])
                    ;; Energy should be conserved (initial = 0.5*k*x² = 0.5)
                    (assert-= e0 0.5 0.0001)
                    (assert-= ef 0.5 0.01))))

;;; ============================================================
;;; Interpolation Tests
;;; ============================================================

(test-group interpolation-tests
            (define-test interpolate-zero
              (let* ([b1 (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [b2 (make-body-2d (vec2 10 10) (vec2 5 5) 1)]
                     [bi (interpolate-body b1 b2 0)])
                    (assert-vec2-= (body-pos bi) (vec2 0 0) 0.0001)))
            
            (define-test interpolate-one
              (let* ([b1 (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [b2 (make-body-2d (vec2 10 10) (vec2 5 5) 1)]
                     [bi (interpolate-body b1 b2 1)])
                    (assert-vec2-= (body-pos bi) (vec2 10 10) 0.0001)))
            
            (define-test interpolate-half
              (let* ([b1 (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [b2 (make-body-2d (vec2 10 10) (vec2 10 10) 1)]
                     [bi (interpolate-body b1 b2 0.5)])
                    (assert-vec2-= (body-pos bi) (vec2 5 5) 0.0001)
                    (assert-vec2-= (body-vel bi) (vec2 5 5) 0.0001))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
══════════════════════════════════════════════════════════
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All 2D physics integration tests passed.
")
    (display "
[FAILURE] Some 2D physics integration tests failed.
"))
