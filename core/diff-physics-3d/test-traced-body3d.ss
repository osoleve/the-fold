;;; core/diff-physics-3d/test-traced-body3d.ss --- Tests for traced-body3d
;;;
;;; Run with: scheme --script core/diff-physics-3d/test-traced-body3d.ss

(load "core/test-framework.ss")
(load "core/diff-physics-3d/traced-body3d.ss")

(display "\n")
(display "==============================================================\n")
(display "         TRACED BODY 3D TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Construction Tests
;;; ============================================================

(test-group construction-tests
            
            (define-test make-traced-body-3d-basic
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 1 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))])
                    (assert-true (traced-body-3d? b))
                    (assert-true (< (abs (- (traced-body-3d-mass b) 1.0)) 1e-10))
                    (assert-true (< (abs (- (traced-body-3d-inv-mass b) 1.0)) 1e-10))))
            
            (define-test static-body
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [b (make-traced-static-body-3d pos orientation)])
                    (assert-true (traced-body-3d-static? b))
                    (assert-equal 0 (traced-body-3d-mass b))
                    (assert-equal 0 (traced-body-3d-inv-mass b))))
            
            (define-test dynamic-body
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 1 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 1) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             2.0 (inertia-solid-sphere 2.0 1.0))])
                    (assert-true (traced-body-3d-dynamic? b))
                    (assert-true (< (abs (- (traced-body-3d-inv-mass b) 0.5)) 1e-10)))))

;;; ============================================================
;;; Accessor Tests
;;; ============================================================

(test-group accessor-tests
            
            (define-test position-access
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 1 2 3) tape)]
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [p (traced-body-3d-pos b)])
                    (assert-true (< (abs (- (traced-value (traced-vec3-x p)) 1)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-y p)) 2)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-z p)) 3)) 1e-10))))
            
            (define-test velocity-access
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 4 5 6) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [v (traced-body-3d-vel b)])
                    (assert-true (< (abs (- (traced-value (traced-vec3-x v)) 4)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-y v)) 5)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-z v)) 6)) 1e-10)))))

;;; ============================================================
;;; Updater Tests
;;; ============================================================

(test-group updater-tests
            
            (define-test with-pos
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [new-pos (lift-vec3 (vec3 10 20 30) tape)]
                     [b2 (traced-body-3d-with-pos b new-pos)]
                     [p (traced-body-3d-pos b2)])
                    (assert-true (< (abs (- (traced-value (traced-vec3-x p)) 10)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-y p)) 20)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-z p)) 30)) 1e-10))))
            
            (define-test with-vel
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [new-vel (lift-vec3 (vec3 5 6 7) tape)]
                     [b2 (traced-body-3d-with-vel b new-vel)]
                     [v (traced-body-3d-vel b2)])
                    (assert-true (< (abs (- (traced-value (traced-vec3-x v)) 5)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-y v)) 6)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-z v)) 7)) 1e-10)))))

;;; ============================================================
;;; Velocity at Point Tests
;;; ============================================================

(test-group velocity-at-point-tests
            
            (define-test velocity-at-center-no-rotation
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 1 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [v-at (traced-body-3d-velocity-at b (lift-vec3 (vec3 0 0 0) tape))])
                    (assert-true (< (abs (- (traced-value (traced-vec3-x v-at)) 1)) 1e-10))
                    (assert-true (< (abs (traced-value (traced-vec3-y v-at))) 1e-10))))
            
            (define-test velocity-with-z-rotation
              ;; omega = (0,0,1) means rotating around z-axis
              ;; At point (1,0,0), velocity should be (0,1,0) from omega × r
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 1) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [point (lift-vec3 (vec3 1 0 0) tape)]
                     [v-at (traced-body-3d-velocity-at b point)])
                    (assert-true (< (abs (traced-value (traced-vec3-x v-at))) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-y v-at)) 1)) 1e-10))
                    (assert-true (< (abs (traced-value (traced-vec3-z v-at))) 1e-10)))))

;;; ============================================================
;;; Impulse Tests
;;; ============================================================

(test-group impulse-tests
            
            (define-test central-impulse-changes-velocity
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [impulse (lift-vec3 (vec3 1 0 0) tape)]
                     [b2 (traced-apply-central-impulse-3d b impulse)]
                     [v (traced-body-3d-vel b2)])
                    (assert-true (< (abs (- (traced-value (traced-vec3-x v)) 1)) 1e-10))
                    (assert-true (< (abs (traced-value (traced-vec3-y v))) 1e-10))))
            
            (define-test static-body-ignores-impulse
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [b (make-traced-static-body-3d pos orientation)]
                     [impulse (lift-vec3 (vec3 1 0 0) tape)]
                     [b2 (traced-apply-central-impulse-3d b impulse)]
                     [v (traced-body-3d-vel b2)])
                    (assert-true (< (abs (traced-value (traced-vec3-x v))) 1e-10)))))

;;; ============================================================
;;; State Flattening Tests
;;; ============================================================

(test-group state-flattening-tests
            
            (define-test state-dimension
              (assert-equal 13 (body-3d-state-dimension)))
            
            (define-test state-to-list
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 1 2 3) tape)]
                     [vel (lift-vec3 (vec3 4 5 6) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0.1 0.2 0.3) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [state (traced-body-3d-state->list b)])
                    (assert-equal 13 (length state))
                    ;; Position
                    (assert-true (< (abs (- (traced-value (list-ref state 0)) 1)) 1e-10))
                    (assert-true (< (abs (- (traced-value (list-ref state 1)) 2)) 1e-10))
                    (assert-true (< (abs (- (traced-value (list-ref state 2)) 3)) 1e-10))
                    ;; Velocity
                    (assert-true (< (abs (- (traced-value (list-ref state 3)) 4)) 1e-10))
                    (assert-true (< (abs (- (traced-value (list-ref state 4)) 5)) 1e-10))
                    (assert-true (< (abs (- (traced-value (list-ref state 5)) 6)) 1e-10))
                    ;; Quaternion (identity = w=1, x=y=z=0)
                    (assert-true (< (abs (- (traced-value (list-ref state 6)) 1)) 1e-10))
                    (assert-true (< (abs (traced-value (list-ref state 7))) 1e-10))
                    (assert-true (< (abs (traced-value (list-ref state 8))) 1e-10))
                    (assert-true (< (abs (traced-value (list-ref state 9))) 1e-10))
                    ;; Angular velocity
                    (assert-true (< (abs (- (traced-value (list-ref state 10)) 0.1)) 1e-10))
                    (assert-true (< (abs (- (traced-value (list-ref state 11)) 0.2)) 1e-10))
                    (assert-true (< (abs (- (traced-value (list-ref state 12)) 0.3)) 1e-10)))))

;;; ============================================================
;;; Energy Tests
;;; ============================================================

(test-group energy-tests
            
            (define-test kinetic-energy-linear-only
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 1 0 0) tape)]  ; v = 1, m = 2 => KE = 0.5 * 2 * 1 = 1
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             2.0 (inertia-solid-sphere 2.0 1.0))]
                     [ke (traced-body-3d-kinetic-energy b)])
                    (assert-true (< (abs (- (traced-value ke) 1.0)) 1e-10))))
            
            (define-test momentum
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 2 0 0) tape)]  ; v = 2, m = 3 => p = 6
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             3.0 (inertia-solid-sphere 3.0 1.0))]
                     [p (traced-body-3d-momentum b)])
                    (assert-true (< (abs (- (traced-value (traced-vec3-x p)) 6)) 1e-10)))))

;;; ============================================================
;;; Coordinate Transform Tests
;;; ============================================================

(test-group transform-tests
            
            (define-test local-to-world-identity-orientation
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 1 2 3) tape)]
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [local (lift-vec3 (vec3 1 0 0) tape)]
                     [world (traced-local-to-world-3d b local)])
                    ;; world = pos + local = (1,2,3) + (1,0,0) = (2,2,3)
                    (assert-true (< (abs (- (traced-value (traced-vec3-x world)) 2)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-y world)) 2)) 1e-10))
                    (assert-true (< (abs (- (traced-value (traced-vec3-z world)) 3)) 1e-10))))
            
            (define-test world-to-local-identity-orientation
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 1 0 0) tape)]
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [orientation (lift-quat (quat-identity) tape)]
                     [angular-vel (lift-vec3 (vec3 0 0 0) tape)]
                     [b (make-traced-body-3d pos vel orientation angular-vel
                                             1.0 (inertia-solid-sphere 1.0 1.0))]
                     [world (lift-vec3 (vec3 2 0 0) tape)]
                     [local (traced-world-to-local-3d b world)])
                    ;; local = world - pos = (2,0,0) - (1,0,0) = (1,0,0)
                    (assert-true (< (abs (- (traced-value (traced-vec3-x local)) 1)) 1e-10))
                    (assert-true (< (abs (traced-value (traced-vec3-y local))) 1e-10)))))

;;; ============================================================
;;; Run all tests
;;; ============================================================

(run-all-tests)
