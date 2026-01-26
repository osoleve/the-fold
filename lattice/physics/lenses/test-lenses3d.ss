;;; lattice/physics/lenses/test-lenses3d.ss — Tests for 3D Physics Lenses
;;; @module test-lenses3d

(load "core/testing/test-framework.ss")
(load "lattice/physics/lenses/lenses3d.ss")

(display "Testing lenses3d.ss...\n")

;;; ============================================================
;;; Quaternion Lens Tests
;;; ============================================================

(test-group "quaternion-lenses"
  (define-test "quat-w-lens view"
    (let ([q (quat 1 0 0 0)])
      (assert-equal 1 (^. q quat-w-lens))))

  (define-test "quat-x-lens view"
    (let ([q (quat 0 1 0 0)])
      (assert-equal 1 (^. q quat-x-lens))))

  (define-test "quat-w-lens set"
    (let* ([q (quat 1 0 0 0)]
           [q2 (& q (.~ quat-w-lens 0.5))])
      (assert-equal 0.5 (quat-w q2))
      (assert-equal 0 (quat-x q2))))

  (define-test "quat-z-lens modify"
    (let* ([q (quat 1 0 0 0.5)]
           [q2 (& q (%~ quat-z-lens (lambda (z) (* z 2))))])
      (assert-equal 1.0 (quat-z q2)))))

;;; ============================================================
;;; Rigid Body 3D Core Lens Tests
;;; ============================================================

(test-group "rigid-body-3d-core-lenses"
  (define test-body
    (make-rigid-body-3d (vec3 1 2 3)
                        (vec3 4 5 6)
                        (quat-identity)
                        (vec3 0 0 0.1)
                        10.0
                        (inertia-solid-sphere 10.0 1.0)))

  (define-test "rigid-body-3d-pos-lens view"
    (assert-true (vec3-equal? (vec3 1 2 3) (^. test-body rigid-body-3d-pos-lens))))

  (define-test "rigid-body-3d-vel-lens view"
    (assert-true (vec3-equal? (vec3 4 5 6) (^. test-body rigid-body-3d-vel-lens))))

  (define-test "rigid-body-3d-mass-lens view"
    (assert-equal 10.0 (^. test-body rigid-body-3d-mass-lens)))

  (define-test "rigid-body-3d-pos-lens set"
    (let ([b2 (& test-body (.~ rigid-body-3d-pos-lens (vec3 10 20 30)))])
      (assert-true (vec3-equal? (vec3 10 20 30) (rigid-body-3d-pos b2)))
      ;; Other fields unchanged
      (assert-true (vec3-equal? (vec3 4 5 6) (rigid-body-3d-vel b2)))))

  (define-test "rigid-body-3d-vel-lens modify"
    (let ([b2 (& test-body (%~ rigid-body-3d-vel-lens (lambda (v) (vec3-scale v 2))))])
      (assert-true (vec3-equal? (vec3 8 10 12) (rigid-body-3d-vel b2)))))

  (define-test "rigid-body-3d-orientation-lens view"
    (let ([q (^. test-body rigid-body-3d-orientation-lens)])
      (assert-equal 1 (quat-w q))
      (assert-equal 0 (quat-x q))))

  (define-test "rigid-body-3d-angular-vel-lens view"
    (assert-true (vec3-equal? (vec3 0 0 0.1) (^. test-body rigid-body-3d-angular-vel-lens)))))

;;; ============================================================
;;; Composed Lens Tests
;;; ============================================================

(test-group "composed-lenses"
  (define test-body
    (make-rigid-body-3d (vec3 1 2 3)
                        (vec3 4 5 6)
                        (quat-identity)
                        (vec3-zero)
                        10.0
                        (inertia-solid-sphere 10.0 1.0)))

  (define-test "rigid-body-3d-pos-x-lens view"
    (assert-equal 1 (^. test-body rigid-body-3d-pos-x-lens)))

  (define-test "rigid-body-3d-pos-y-lens view"
    (assert-equal 2 (^. test-body rigid-body-3d-pos-y-lens)))

  (define-test "rigid-body-3d-pos-z-lens view"
    (assert-equal 3 (^. test-body rigid-body-3d-pos-z-lens)))

  (define-test "rigid-body-3d-vel-x-lens modify"
    (let ([b2 (& test-body (%~ rigid-body-3d-vel-x-lens (lambda (x) (+ x 100))))])
      (assert-equal 104 (vec3-x (rigid-body-3d-vel b2)))
      ;; y and z unchanged
      (assert-equal 5 (vec3-y (rigid-body-3d-vel b2)))
      (assert-equal 6 (vec3-z (rigid-body-3d-vel b2)))))

  (define-test ">>> composition with vec3"
    (let* ([lens (>>> rigid-body-3d-pos-lens vec3-z-lens)]
           [b2 (& test-body (.~ lens 999))])
      (assert-equal 999 (vec3-z (rigid-body-3d-pos b2))))))

;;; ============================================================
;;; Dot Notation Tests
;;; ============================================================

(test-group "dot-notation"
  (define test-body
    (make-rigid-body-3d (vec3 1 2 3)
                        (vec3 4 5 6)
                        (quat 0.707 0 0.707 0)  ; 90° around y
                        (vec3 0.1 0.2 0.3)
                        5.0
                        (inertia-solid-box 5.0 1.0 1.0 1.0)))

  (define-test "(body3d. pos) view"
    (assert-true (vec3-equal? (vec3 1 2 3) (^. test-body (body3d. pos)))))

  (define-test "(body3d. pos x) view"
    (assert-equal 1 (^. test-body (body3d. pos x))))

  (define-test "(body3d. vel z) modify"
    (let ([b2 (& test-body (%~ (body3d. vel z) (lambda (z) (* z 10))))])
      (assert-equal 60 (vec3-z (rigid-body-3d-vel b2)))))

  (define-test "(body3d. orientation) view"
    (let ([q (^. test-body (body3d. orientation))])
      (assert-true (< (abs (- (quat-w q) 0.707)) 0.01))))

  (define-test "(body3d. angular-vel y) view"
    (assert-equal 0.2 (^. test-body (body3d. angular-vel y))))

  (define-test "(body3d. mass) set"
    (let ([b2 (& test-body (.~ (body3d. mass) 100.0))])
      (assert-equal 100.0 (rigid-body-3d-mass b2))
      ;; Inverse mass should be recalculated
      (assert-equal 0.01 (rigid-body-3d-inv-mass b2))))

  (define-test "(body3d. inv-mass) getter"
    ;; test-body has mass 5.0, so inv-mass should be 0.2
    (assert-equal 0.2 (^. test-body (body3d. inv-mass))))

  (define-test "(body3d. inv-inertia) getter"
    ;; Just verify we can access it through the macro (returns a mat3 = list of lists)
    (let ([inv-I (^. test-body (body3d. inv-inertia))])
      (assert-true (list? inv-I)))))

;;; ============================================================
;;; Traversal Tests
;;; ============================================================

(test-group "traversals"
  (define bodies
    (list
     (make-rigid-body-3d (vec3 0 0 0) (vec3 1 0 0) (quat-identity) (vec3-zero) 1.0 (mat3-identity))
     (make-rigid-body-3d (vec3 1 0 0) (vec3 0 1 0) (quat-identity) (vec3-zero) 2.0 (mat3-identity))
     (make-static-body-3d (vec3 2 0 0) (quat-identity))))

  (define-test "bodies-3d-each to-list"
    (assert-equal 3 (length (traversal-to-list bodies-3d-each bodies))))

  (define-test "bodies-3d-each modify all"
    (let ([bodies2 (traversal-over bodies-3d-each
                                   (lambda (b) (& b (%~ rigid-body-3d-pos-x-lens add1)))
                                   bodies)])
      (assert-equal 1 (vec3-x (rigid-body-3d-pos (car bodies2))))
      (assert-equal 2 (vec3-x (rigid-body-3d-pos (cadr bodies2))))
      (assert-equal 3 (vec3-x (rigid-body-3d-pos (caddr bodies2))))))

  (define-test "dynamic-bodies-3d filters correctly"
    (let ([dynamic (traversal-to-list dynamic-bodies-3d bodies)])
      (assert-equal 2 (length dynamic))))

  (define-test "static-bodies-3d filters correctly"
    (let ([static (traversal-to-list static-bodies-3d bodies)])
      (assert-equal 1 (length static)))))

;;; ============================================================
;;; Convenience Function Tests
;;; ============================================================

(test-group "convenience-functions"
  (define-test "translate-body-3d"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3-zero) (quat-identity) (vec3-zero) 1.0 (mat3-identity))]
           [b2 ((translate-body-3d (vec3 10 20 30)) b)])
      (assert-true (vec3-equal? (vec3 10 20 30) (rigid-body-3d-pos b2)))))

  (define-test "apply-central-impulse-3d on dynamic body"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3-zero) 2.0 (mat3-identity))]
           [b2 ((apply-central-impulse-3d (vec3 10 0 0)) b)])
      ;; delta-v = impulse * inv-mass = (10,0,0) * 0.5 = (5,0,0)
      (assert-true (vec3-equal? (vec3 5 0 0) (rigid-body-3d-vel b2)))))

  (define-test "apply-central-impulse-3d on static body (no-op)"
    (let* ([b (make-static-body-3d (vec3 0 0 0) (quat-identity))]
           [b2 ((apply-central-impulse-3d (vec3 100 100 100)) b)])
      ;; Static body should not move
      (assert-true (vec3-equal? (vec3-zero) (rigid-body-3d-vel b2)))))

  (define-test "apply-gravity-3d"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3-zero) 1.0 (mat3-identity))]
           [gravity (vec3 0 -9.8 0)]
           [b2 ((apply-gravity-3d gravity 0.1) b)])
      ;; After 0.1s: vel.y = 0 + (-9.8 * 0.1) = -0.98
      (assert-true (< (abs (- (vec3-y (rigid-body-3d-vel b2)) -0.98)) 0.001))))

  (define-test "step-body-3d"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 10 0 0) (quat-identity) (vec3-zero) 1.0 (mat3-identity))]
           [b2 ((step-body-3d 0.5) b)])
      ;; After 0.5s at vel (10,0,0): pos = (0,0,0) + (10,0,0)*0.5 = (5,0,0)
      (assert-true (vec3-equal? (vec3 5 0 0) (rigid-body-3d-pos b2))))))

;;; ============================================================
;;; Fold Tests
;;; ============================================================

(test-group "folds"
  (define bodies
    (list
     (make-rigid-body-3d (vec3 0 0 0) (vec3-zero) (quat-identity) (vec3-zero) 1.0 (mat3-identity))
     (make-rigid-body-3d (vec3 10 0 0) (vec3-zero) (quat-identity) (vec3-zero) 2.0 (mat3-identity))
     (make-rigid-body-3d (vec3 0 10 0) (vec3-zero) (quat-identity) (vec3-zero) 3.0 (mat3-identity))))

  (define-test "total-mass-3d"
    (assert-equal 6.0 (total-mass-3d bodies)))

  (define-test "center-of-mass-3d"
    (let ([com (center-of-mass-3d bodies)])
      ;; Weighted avg: (1*0 + 2*10 + 3*0)/6 = 20/6 ≈ 3.33 for x
      ;; (1*0 + 2*0 + 3*10)/6 = 30/6 = 5 for y
      (assert-true (< (abs (- (vec3-x com) (/ 20 6))) 0.01))
      (assert-true (< (abs (- (vec3-y com) 5.0)) 0.01))
      (assert-equal 0 (vec3-z com)))))

;;; ============================================================
;;; Protocol Bundle Tests
;;; ============================================================

(test-group "protocol-bundles"
  (define test-body
    (make-rigid-body-3d (vec3 1 2 3)
                        (vec3 4 5 6)
                        (quat-identity)
                        (vec3 0.1 0.2 0.3)
                        10.0
                        (inertia-solid-sphere 10.0 1.0)))

  (define-test "body3d-ops implemented by rigid-body-3d"
    (assert-true (implements-bundle? 'rigid-body-3d body3d-ops)))

  (define-test "rotational-body3d-ops implemented by rigid-body-3d"
    (assert-true (implements-bundle? 'rigid-body-3d rotational-body3d-ops)))

  (define-test "body3d-pos protocol"
    (assert-true (vec3-equal? (vec3 1 2 3) (body3d-pos test-body))))

  (define-test "body3d-vel protocol"
    (assert-true (vec3-equal? (vec3 4 5 6) (body3d-vel test-body))))

  (define-test "body3d-mass protocol"
    (assert-equal 10.0 (body3d-mass test-body)))

  (define-test "body3d-set-pos protocol"
    (let ([b2 (body3d-set-pos test-body (vec3 99 88 77))])
      (assert-true (vec3-equal? (vec3 99 88 77) (body3d-pos b2)))))

  (define-test "body3d-set-mass protocol recalculates inv-mass"
    (let ([b2 (body3d-set-mass test-body 5.0)])
      (assert-equal 5.0 (body3d-mass b2))
      (assert-equal 0.2 (rigid-body-3d-inv-mass b2))))  ; inv-mass = 1/5 = 0.2

  (define-test "body3d-orientation protocol"
    (assert-true (quat-equal? (quat-identity) (body3d-orientation test-body))))

  (define-test "body3d-angular-vel protocol"
    (assert-true (vec3-equal? (vec3 0.1 0.2 0.3) (body3d-angular-vel test-body))))

  (define-test "rotates3d? predicate"
    (assert-true (rotates3d? test-body))))

;;; ============================================================
;;; Generic Lens Tests
;;; ============================================================

(test-group "generic-lenses"
  (define test-body
    (make-rigid-body-3d (vec3 1 2 3)
                        (vec3 4 5 6)
                        (quat-identity)
                        (vec3 0 0 0)
                        10.0
                        (mat3-identity)))

  (define-test "body3d-pos-lens view"
    (assert-true (vec3-equal? (vec3 1 2 3) (view body3d-pos-lens test-body))))

  (define-test "body3d-vel-lens view"
    (assert-true (vec3-equal? (vec3 4 5 6) (view body3d-vel-lens test-body))))

  (define-test "body3d-mass-lens view"
    (assert-equal 10.0 (view body3d-mass-lens test-body)))

  (define-test "body3d-pos-lens set"
    (let ([b2 (set-lens body3d-pos-lens (vec3 100 200 300) test-body)])
      (assert-true (vec3-equal? (vec3 100 200 300) (body3d-pos b2)))))

  (define-test "body3d-vel-lens over"
    (let ([b2 (over body3d-vel-lens (lambda (v) (vec3-scale v 2)) test-body)])
      (assert-true (vec3-equal? (vec3 8 10 12) (body3d-vel b2)))))

  (define-test "body3d-orientation-lens view"
    (assert-true (quat-equal? (quat-identity) (view body3d-orientation-lens test-body))))

  (define-test "body3d-angular-vel-lens set"
    (let ([b2 (set-lens body3d-angular-vel-lens (vec3 1 2 3) test-body)])
      (assert-true (vec3-equal? (vec3 1 2 3) (body3d-angular-vel b2))))))

;;; ============================================================
;;; Generic Helper Tests
;;; ============================================================

(test-group "generic-helpers"
  (define test-body
    (make-rigid-body-3d (vec3 0 0 0)
                        (vec3 0 0 0)
                        (quat-identity)
                        (vec3 0 0 0)
                        10.0
                        (mat3-identity)))

  (define-test "integrate-body3d position"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 10 0 0) (quat-identity) (vec3-zero) 1.0 (mat3-identity))]
           [b2 (integrate-body3d 0.5 b)])
      ;; Position should advance by vel*dt = (10,0,0)*0.5 = (5,0,0)
      (assert-true (vec3-equal? (vec3 5 0 0) (body3d-pos b2)))))

  (define-test "apply-impulse-at-point3d linear"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3-zero) 10.0 (mat3-identity))]
           ;; Apply impulse at center of mass - should only affect linear velocity
           [b2 (apply-impulse-at-point3d (vec3 100 0 0) (vec3 0 0 0) b)])
      ;; delta-v = J/m = (100,0,0)/10 = (10,0,0)
      (assert-true (vec3-equal? (vec3 10 0 0) (body3d-vel b2)))))

  (define-test "apply-force3d-via-lens"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3-zero) 10.0 (mat3-identity))]
           [b2 (apply-force3d-via-lens body3d-vel-lens body3d-mass-lens (vec3 100 0 0) 1.0 b)])
      ;; delta-v = F*dt/m = 100*1.0/10 = 10
      (assert-true (vec3-equal? (vec3 10 0 0) (body3d-vel b2)))))

  (define-test "apply-torque3d"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3-zero) 1.0 (mat3-identity))]
           ;; Apply torque around z-axis for 1 second
           ;; With identity inertia: delta-omega = I^-1 * tau * dt = (0,0,10) * 1 = (0,0,10)
           [b2 (apply-torque3d (vec3 0 0 10) 1.0 b)])
      (assert-true (< (abs (- (vec3-z (body3d-angular-vel b2)) 10.0)) 0.001))))

  (define-test "apply-impulse-at-point3d angular effect"
    ;; Apply impulse off-center to cause rotation
    (let* ([;; Sphere with I = (2/5)*m*r^2 * Identity for r=1, m=1 => I = 0.4*Identity
            b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3-zero)
                                   1.0 (inertia-solid-sphere 1.0 1.0))]
           ;; Apply impulse J=(1,0,0) at point r=(0,1,0) (offset on y-axis)
           ;; Torque impulse = r × J = (0,1,0) × (1,0,0) = (0,0,-1)
           ;; For sphere: I^-1 = (1/0.4)*Identity = 2.5*Identity
           ;; delta-omega = I^-1 * torque = (0, 0, -2.5)
           [b2 (apply-impulse-at-point3d (vec3 1 0 0) (vec3 0 1 0) b)])
      ;; Linear: delta-v = J/m = (1,0,0)/1 = (1,0,0)
      (assert-true (vec3-equal? (vec3 1 0 0) (body3d-vel b2)))
      ;; Angular: should rotate around z-axis (negative direction)
      (assert-true (< (abs (- (vec3-z (body3d-angular-vel b2)) -2.5)) 0.001))))

  (define-test "integrate-rotation3d-via-lens"
    ;; Start with identity quaternion, spin around z-axis
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity)
                                   (vec3 0 0 3.14159) ; ~PI rad/s around z
                                   1.0 (mat3-identity))]
           ;; Integrate for 0.5 seconds => should rotate ~PI/2 radians
           [b2 (integrate-rotation3d-via-lens body3d-orientation-lens body3d-angular-vel-lens 0.5 b)]
           [q (body3d-orientation b2)])
      ;; After ~90 degree rotation around z: quat should be approximately (cos(45°), 0, 0, sin(45°))
      ;; cos(45°) ≈ 0.707, sin(45°) ≈ 0.707
      (assert-true (< (abs (- (quat-w q) 0.707)) 0.1))
      (assert-true (< (abs (- (quat-z q) 0.707)) 0.1)))))

;;; ============================================================
;;; Additional Coverage Tests
;;; ============================================================

(test-group "additional-coverage"
  (define-test "quat-y-lens view and set"
    (let* ([q (quat 1 0 0.5 0)]
           [y-val (^. q quat-y-lens)]
           [q2 (& q (.~ quat-y-lens 0.25))])
      (assert-equal 0.5 y-val)
      (assert-equal 0.25 (quat-y q2))))

  (define-test "body3d-inertia-lens view"
    (let* ([inertia (inertia-solid-sphere 10.0 1.0)]
           [b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3-zero) 10.0 inertia)])
      ;; View inertia through generic lens
      (assert-true (equal? inertia (view body3d-inertia-lens b)))))

  (define-test "body3d-inertia-lens set"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3-zero) 10.0 (mat3-identity))]
           [new-inertia (mat3-diagonal 2 2 2)]
           [b2 (set-lens body3d-inertia-lens new-inertia b)])
      (assert-true (equal? new-inertia (body3d-inertia b2)))))

  (define-test "rigid-body-3d-vel-y-lens"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 1 2 3) (quat-identity) (vec3-zero) 1.0 (mat3-identity))])
      (assert-equal 2 (^. b rigid-body-3d-vel-y-lens))))

  (define-test "rigid-body-3d-vel-z-lens"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 1 2 3) (quat-identity) (vec3-zero) 1.0 (mat3-identity))])
      (assert-equal 3 (^. b rigid-body-3d-vel-z-lens))))

  (define-test "rigid-body-3d-angular-vel-x-lens"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3 0.1 0.2 0.3) 1.0 (mat3-identity))])
      (assert-equal 0.1 (^. b rigid-body-3d-angular-vel-x-lens))))

  (define-test "rigid-body-3d-angular-vel-y-lens"
    (let* ([b (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3 0.1 0.2 0.3) 1.0 (mat3-identity))])
      (assert-equal 0.2 (^. b rigid-body-3d-angular-vel-y-lens))))

  (define-test "total-kinetic-energy fold"
    (let* ([;; Body 1: m=2, v=(3,0,0), omega=0 => KE = 0.5*2*9 = 9
            b1 (make-rigid-body-3d (vec3 0 0 0) (vec3 3 0 0) (quat-identity) (vec3-zero) 2.0 (mat3-identity))]
           ;; Body 2: m=1, v=(0,0,0), omega=0 => KE = 0
           [b2 (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0) (quat-identity) (vec3-zero) 1.0 (mat3-identity))]
           [bodies (list b1 b2)]
           [ke (total-kinetic-energy bodies)])
      ;; Total KE = 9 + 0 = 9
      (assert-true (< (abs (- ke 9.0)) 0.001))))

  (define-test "static body ignores torque"
    (let* ([b (make-static-body-3d (vec3 0 0 0) (quat-identity))]
           [b2 (apply-torque3d (vec3 0 0 100) 1.0 b)])
      ;; Static body should not change
      (assert-true (vec3-equal? (vec3-zero) (rigid-body-3d-angular-vel b2))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
