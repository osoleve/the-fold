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
;;; Run Tests
;;; ============================================================

(run-all-tests)
