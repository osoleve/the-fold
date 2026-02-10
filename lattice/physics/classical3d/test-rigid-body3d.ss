(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/physics/classical3d/rigid-body3d.ss")

(doc 'module 'test-rigid-body3d)
(doc 'description "Tests for rigid-body3d")
(doc 'layer 'lattice)
(doc 'note "Run with: scheme --script lattice/physics/classical3d/test-rigid-body3d.ss")

(display "\n")
(display "====\n")
(display "         3D RIGID BODY TESTS\n")
(display "====\n")

;;; ====
;;; Construction Tests
;;; ====

(test-group construction-tests
            
            (define-test make-rigid-body-3d
              (let ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 1 0 0) (quat-identity) (vec3 0 0 0)
                                           1.0 (inertia-solid-sphere 1.0 1.0))])
                   (assert-true (rigid-body-3d? b))
                   (assert-equal 1.0 (rigid-body-3d-mass b))
                   (assert-equal 1.0 (rigid-body-3d-inv-mass b))))
            
            (define-test static-body
              (let ([b (make-static-body-3d (vec3 0 0 0) (quat-identity))])
                   (assert-true (rigid-body-3d-static? b))
                   (assert-equal 0 (rigid-body-3d-mass b))
                   (assert-equal 0 (rigid-body-3d-inv-mass b))))
            
            (define-test dynamic-sphere
              (let ([b (make-dynamic-sphere (vec3 0 0 0) (vec3 1 0 0) 1.0 1.0)])
                   (assert-true (rigid-body-3d-dynamic? b))
                   (assert-equal 1 (vec3-x (rigid-body-3d-vel b)))))
            
            (define-test dynamic-box
              (let ([b (make-dynamic-box (vec3 0 0 0) (vec3 0 0 0) 1.0 (vec3 1 1 1))])
                   (assert-true (rigid-body-3d-dynamic? b)))))

;;; ====
;;; Inertia Tensor Tests
;;; ====

(test-group inertia-tests
            
            (define-test solid-sphere-inertia
              (let ([I (inertia-solid-sphere 1.0 1.0)])
                   (assert-true (< (abs (- (mat3-ref I 0 0) 0.4)) 1e-10))))
            
            (define-test hollow-sphere-inertia
              (let ([I (inertia-hollow-sphere 1.0 1.0)])
                   (assert-true (< (abs (- (mat3-ref I 0 0) (/ 2 3))) 1e-10))))
            
            (define-test solid-box-inertia-diagonal
              (let ([I (inertia-solid-box 12.0 1.0 1.0 1.0)])
                   (assert-true (< (abs (- (mat3-ref I 0 0) 8)) 1e-10))))
            
            (define-test solid-box-inertia-off-diagonal
              (let ([I (inertia-solid-box 1.0 1.0 1.0 1.0)])
                   (assert-true (< (abs (mat3-ref I 0 1)) 1e-10))
                   (assert-true (< (abs (mat3-ref I 1 2)) 1e-10))))
            
            (define-test solid-cylinder-inertia
              (let ([I (inertia-solid-cylinder 1.0 1.0 1.0)])
                   (assert-true (< (abs (- (mat3-ref I 2 2) 0.5)) 1e-10)))))

;;; ====
;;; Mat3 Operation Tests
;;; ====

(test-group mat3-tests
            
            (define-test mat3-identity-test
              (let ([I (mat3-identity)])
                   (assert-equal 1 (mat3-ref I 0 0))
                   (assert-equal 0 (mat3-ref I 0 1))
                   (assert-equal 1 (mat3-ref I 1 1))))
            
            (define-test mat3-transpose-test
              (let* ([m (list (list 1 2 3) (list 4 5 6) (list 7 8 9))]
                     [t (mat3-transpose m)])
                    (assert-equal 4 (mat3-ref t 0 1))
                    (assert-equal 2 (mat3-ref t 1 0))))
            
            (define-test mat3-mul-vec3-identity
              (let* ([I (mat3-identity)]
                     [v (vec3 1 2 3)]
                     [r (mat3-mul-vec3 I v)])
                    (assert-equal 1 (vec3-x r))
                    (assert-equal 2 (vec3-y r))
                    (assert-equal 3 (vec3-z r))))
            
            (define-test mat3-mul-identity
              (let* ([I (mat3-identity)]
                     [r (mat3-mul I I)])
                    (assert-equal 1 (mat3-ref r 0 0))))
            
            (define-test mat3-inverse-diagonal
              (let* ([m (mat3-diagonal 2 3 4)]
                     [inv (mat3-inverse m)])
                    (assert-true (< (abs (- (mat3-ref inv 0 0) 0.5)) 1e-10))
                    (assert-true (< (abs (- (mat3-ref inv 1 1) (/ 1 3))) 1e-10)))))

;;; ====
;;; Velocity at Point Tests
;;; ====

(test-group velocity-at-point-tests
            
            (define-test velocity-at-center-no-rotation
              (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 1 0 0) (quat-identity) (vec3 0 0 0)
                                            1.0 (inertia-solid-sphere 1.0 1.0))]
                     [v (rigid-body-3d-velocity-at b (vec3 0 0 0))])
                    (assert-equal 1 (vec3-x v))
                    (assert-equal 0 (vec3-y v))))
            
            (define-test velocity-with-z-rotation
              (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3 0 0 1)
                                            1.0 (inertia-solid-sphere 1.0 1.0))]
                     [v (rigid-body-3d-velocity-at b (vec3 1 0 0))])
                    (assert-true (< (abs (vec3-x v)) 1e-10))
                    (assert-true (< (abs (- (vec3-y v) 1)) 1e-10))))
            
            (define-test velocity-with-y-rotation
              (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3 0 1 0)
                                            1.0 (inertia-solid-sphere 1.0 1.0))]
                     [v (rigid-body-3d-velocity-at b (vec3 1 0 0))])
                    (assert-true (< (abs (vec3-x v)) 1e-10))
                    (assert-true (< (abs (vec3-y v)) 1e-10))
                    (assert-true (< (abs (- (vec3-z v) -1)) 1e-10)))))

;;; ====
;;; Impulse Application Tests
;;; ====

(test-group impulse-tests
            
            (define-test central-impulse-changes-velocity
              (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3 0 0 0)
                                            1.0 (inertia-solid-sphere 1.0 1.0))]
                     [b2 (rigid-body-3d-apply-central-impulse b (vec3 1 0 0))])
                    (assert-equal 1 (vec3-x (rigid-body-3d-vel b2)))
                    (assert-equal 0 (vec3-y (rigid-body-3d-vel b2)))))
            
            (define-test central-impulse-no-angular-change
              (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3 0 0 0)
                                            1.0 (inertia-solid-sphere 1.0 1.0))]
                     [b2 (rigid-body-3d-apply-central-impulse b (vec3 1 0 0))])
                    (assert-equal 0 (vec3-magnitude (rigid-body-3d-angular-vel b2)))))
            
            (define-test off-center-impulse-causes-rotation
              (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3 0 0 0)
                                            1.0 (inertia-solid-sphere 1.0 1.0))]
                     [b2 (rigid-body-3d-apply-impulse b (vec3 0 1 0) (vec3 1 0 0))])
                    (assert-true (> (vec3-magnitude (rigid-body-3d-angular-vel b2)) 0))))
            
            (define-test static-body-ignores-impulse
              (let* ([b (make-static-body-3d (vec3 0 0 0) (quat-identity))]
                     [b2 (rigid-body-3d-apply-impulse b (vec3 1 0 0) (vec3 0 0 0))])
                    (assert-equal 0 (vec3-magnitude (rigid-body-3d-vel b2))))))

;;; ====
;;; Coordinate Transform Tests
;;; ====

(test-group transform-tests
            
            (define-test local-to-world-identity-orientation
              (let* ([b (make-rigid-body-3d (vec3 1 2 3) (vec3 0 0 0) (quat-identity) (vec3 0 0 0)
                                            1.0 (inertia-solid-sphere 1.0 1.0))]
                     [w (rigid-body-3d-local-to-world b (vec3 1 0 0))])
                    (assert-equal 2 (vec3-x w))
                    (assert-equal 2 (vec3-y w))
                    (assert-equal 3 (vec3-z w))))
            
            (define-test world-to-local-identity-orientation
              (let* ([b (make-rigid-body-3d (vec3 1 0 0) (vec3 0 0 0) (quat-identity) (vec3 0 0 0)
                                            1.0 (inertia-solid-sphere 1.0 1.0))]
                     [l (rigid-body-3d-world-to-local b (vec3 2 0 0))])
                    (assert-equal 1 (vec3-x l))
                    (assert-equal 0 (vec3-y l)))))

;;; ====
;;; Energy Tests
;;; ====

(test-group energy-tests
            
            (define-test kinetic-energy-linear-only
              (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 1 0 0) (quat-identity) (vec3 0 0 0)
                                            2.0 (inertia-solid-sphere 2.0 1.0))]
                     [ke (rigid-body-3d-kinetic-energy b)])
                    (assert-true (< (abs (- ke 1.0)) 1e-10))))
            
            (define-test momentum
              (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 2 0 0) (quat-identity) (vec3 0 0 0)
                                            3.0 (inertia-solid-sphere 3.0 1.0))]
                     [p (rigid-body-3d-momentum b)])
                    (assert-equal 6 (vec3-x p)))))
