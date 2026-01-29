;;; lattice/physics/classical/test-collision-protocol.ss — Collision Protocol Tests
;;;
;;; Run with: scheme --script lattice/physics/classical/test-collision-protocol.ss

(load "core/testing/test-framework.ss")
(load "lattice/physics/classical/collision-impl.ss")

;;; ====
;;; Test Fixtures
;;; ====

(define test-body
  (make-body-2d (vec2 0 0) (vec2 5 0) 2.0))  ; mass = 2

(define test-static-body
  (make-static-body (vec2 10 0)))

(define test-rigid
  (make-rigid-body (vec2 0 0) (vec2 3 0) 0 1.0 4.0 8.0))  ; mass=4, inertia=8, omega=1

(define test-particle
  (make-particle (vec2 0 0) (vec2 2 0) 10 10 1.0 'white #f))

;;; ====
;;; Protocol Implementation Tests: Body2D
;;; ====

(test-group "Body2D collision protocols"
  (define-test "col-inv-mass returns inverse mass"
    (assert-equal 0.5 (col-inv-mass test-body)))  ; 1/2

  (define-test "col-inv-inertia returns 0"
    (assert-equal 0 (col-inv-inertia test-body)))

  (define-test "col-static? returns false for dynamic body"
    (assert-false (col-static? test-body)))

  (define-test "col-static? returns true for static body"
    (assert-true (col-static? test-static-body)))

  (define-test "col-pos returns position"
    (let ([pos (col-pos test-body)])
      (assert-equal 0 (vec2-x pos))
      (assert-equal 0 (vec2-y pos))))

  (define-test "col-vel-at ignores contact point"
    (let ([vel (col-vel-at test-body (vec2 100 100))])
      (assert-equal 5 (vec2-x vel))
      (assert-equal 0 (vec2-y vel))))

  (define-test "col-apply-impulse updates velocity"
    (let* ([impulse (vec2 2 0)]
           [updated (col-apply-impulse test-body impulse (vec2 0 0))])
      ;; v_new = v + impulse * inv_mass = (5,0) + (2,0) * 0.5 = (6,0)
      (assert-equal 6.0 (vec2-x (body-vel updated)))))

  (define-test "col-apply-impulse no-ops static body"
    (let* ([impulse (vec2 100 0)]
           [updated (col-apply-impulse test-static-body impulse (vec2 0 0))])
      (assert-equal 0 (vec2-x (body-vel updated))))))

;;; ====
;;; Protocol Implementation Tests: RigidBody2D
;;; ====

(test-group "RigidBody2D collision protocols"
  (define-test "col-inv-mass returns inverse mass"
    (assert-equal 0.25 (col-inv-mass test-rigid)))  ; 1/4

  (define-test "col-inv-inertia returns inverse inertia"
    (assert-equal 0.125 (col-inv-inertia test-rigid)))  ; 1/8

  (define-test "col-static? false for dynamic rigid body"
    (assert-false (col-static? test-rigid)))

  (define-test "col-vel-at includes angular contribution"
    ;; Body at origin with omega=1, vel=(3,0)
    ;; Point at (0, 2): angular contribution = omega × r = 1 × (0,2) = (-2, 0)
    ;; Total vel at point = (3,0) + (-2,0) = (1, 0)
    (let ([vel (col-vel-at test-rigid (vec2 0 2))])
      (assert-true (< (abs (- (vec2-x vel) 1)) 0.001))))

  (define-test "col-apply-impulse updates angular velocity"
    ;; Impulse perpendicular to lever arm creates torque
    (let* ([impulse (vec2 0 4)]  ; Upward impulse
           [contact (vec2 2 0)]   ; Contact 2 units right of center
           [updated (col-apply-impulse test-rigid impulse contact)])
      ;; torque = r × F = (2,0) × (0,4) = 8
      ;; delta_omega = torque * inv_inertia = 8 * 0.125 = 1
      ;; new_omega = 1 + 1 = 2
      (assert-equal 2.0 (rigid-body-angular-vel updated)))))

;;; ====
;;; Protocol Implementation Tests: Particle
;;; ====

(test-group "Particle collision protocols"
  (define-test "col-inv-mass returns 1 (unit mass)"
    (assert-equal 1.0 (col-inv-mass test-particle)))

  (define-test "col-inv-inertia returns 0"
    (assert-equal 0 (col-inv-inertia test-particle)))

  (define-test "col-static? returns false"
    (assert-false (col-static? test-particle)))

  (define-test "col-apply-impulse directly adds to velocity"
    (let* ([impulse (vec2 3 0)]
           [updated (col-apply-impulse test-particle impulse (vec2 0 0))])
      ;; Unit mass: vel += impulse
      (assert-equal 5 (vec2-x (particle-vel updated))))))

;;; ====
;;; Capability Check Tests
;;; ====

(test-group "Collision capability checks"
  (define-test "body-2d is collision capable"
    (assert-true (collision-capable? 'body-2d)))

  (define-test "rigid-body-2d is collision capable"
    (assert-true (collision-capable? 'rigid-body-2d)))

  (define-test "particle is collision capable"
    (assert-true (collision-capable? 'particle)))

  (define-test "unknown type is not collision capable"
    (assert-false (collision-capable? 'unknown-type)))

  (define-test "assert-collision-capable! passes for valid type"
    (assert-equal test-body (assert-collision-capable! test-body)))

  (define-test "assert-collision-capable! throws for invalid type"
    (assert-error (lambda () (assert-collision-capable! '(unknown 1 2 3))))))

;;; ====
;;; Derived Operations Tests
;;; ====

(test-group "Derived collision operations"
  (define-test "col-relative-vel-at calculates relative velocity"
    (let* ([b1 (make-body-2d (vec2 0 0) (vec2 2 0) 1.0)]
           [b2 (make-body-2d (vec2 5 0) (vec2 5 0) 1.0)]
           [rel-vel (col-relative-vel-at b1 b2 (vec2 2.5 0))])
      ;; rel = vel_b - vel_a = (5,0) - (2,0) = (3,0)
      (assert-equal 3 (vec2-x rel-vel))))

  (define-test "col-normal-vel-at calculates velocity along normal"
    (let* ([b1 (make-body-2d (vec2 0 0) (vec2 0 0) 1.0)]
           [b2 (make-body-2d (vec2 5 0) (vec2 -3 0) 1.0)]
           [normal (vec2 -1 0)]  ; Pointing from b2 to b1
           [normal-vel (col-normal-vel-at b1 b2 (vec2 2.5 0) normal)])
      ;; rel = (-3,0), normal = (-1,0), dot = 3
      (assert-equal 3 normal-vel)))

  (define-test "col-effective-mass for simple case"
    (let* ([b1 (make-body-2d (vec2 0 0) (vec2 0 0) 2.0)]
           [b2 (make-body-2d (vec2 4 0) (vec2 0 0) 4.0)]
           [contact (vec2 2 0)]
           [normal (vec2 1 0)]
           [eff-mass (col-effective-mass b1 b2 contact normal)])
      ;; No rotation, so just 1/m1 + 1/m2 = 0.5 + 0.25 = 0.75
      (assert-equal 0.75 eff-mass)))

  (define-test "col-effective-mass returns +inf.0 for static-static collision"
    ;; Two static bodies have zero inverse mass/inertia -> infinite effective mass
    (let* ([b1 (make-static-body (vec2 0 0))]
           [b2 (make-static-body (vec2 4 0))]
           [contact (vec2 2 0)]
           [normal (vec2 1 0)]
           [eff-mass (col-effective-mass b1 b2 contact normal)])
      ;; Should return +inf.0, not 0 (which would cause division by zero)
      (assert-true (infinite? eff-mass))
      (assert-true (positive? eff-mass)))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
