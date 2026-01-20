(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "lattice/physics/lenses/lenses.ss")

(doc 'module 'test-lenses)
(doc 'description "Tests for Physics Lenses")
(doc 'layer 'lattice)
(doc 'note "Run with: scheme --script lattice/physics/lenses/test-lenses.ss")

(doc 'section 'test-fixtures)

(define test-body
  (make-rigid-body (vec2 10 20)   ; pos
                   (vec2 1 2)     ; vel
                   0.5            ; angle
                   0.1            ; angular-vel
                   5.0            ; mass
                   2.0))          ; inertia

(define test-particle
  (make-particle (vec2 5 10)      ; pos
                 (vec2 -1 0)      ; vel
                 100              ; lifetime
                 100              ; max-life
                 1.5              ; size
                 'red             ; color
                 #f))             ; user-data

(doc 'section 'vec2-lens-tests)

(test-group "Vec2 Lenses"
  (define-test "vec2-x-lens view"
    (assert-equal 10 (view vec2-x-lens (vec2 10 20))))

  (define-test "vec2-y-lens view"
    (assert-equal 20 (view vec2-y-lens (vec2 10 20))))

  (define-test "vec2-x-lens set"
    (let ([v (set-lens vec2-x-lens 99 (vec2 10 20))])
      (assert-equal 99 (vec2-x v))
      (assert-equal 20 (vec2-y v))))

  (define-test "vec2-y-lens set"
    (let ([v (set-lens vec2-y-lens 99 (vec2 10 20))])
      (assert-equal 10 (vec2-x v))
      (assert-equal 99 (vec2-y v))))

  (define-test "vec2-x-lens over"
    (let ([v (over vec2-x-lens (lambda (x) (* x 2)) (vec2 10 20))])
      (assert-equal 20 (vec2-x v))
      (assert-equal 20 (vec2-y v)))))

(doc 'section 'rigid-body-lens-tests)

(test-group "RigidBody Lenses"
  (define-test "rigid-body-pos-lens view"
    (let ([pos (view rigid-body-pos-lens test-body)])
      (assert-equal 10 (vec2-x pos))
      (assert-equal 20 (vec2-y pos))))

  (define-test "rigid-body-vel-lens view"
    (let ([vel (view rigid-body-vel-lens test-body)])
      (assert-equal 1 (vec2-x vel))
      (assert-equal 2 (vec2-y vel))))

  (define-test "rigid-body-angle-lens view"
    (assert-equal 0.5 (view rigid-body-angle-lens test-body)))

  (define-test "rigid-body-angular-vel-lens view"
    (assert-equal 0.1 (view rigid-body-angular-vel-lens test-body)))

  (define-test "rigid-body-mass-lens view"
    (assert-equal 5.0 (view rigid-body-mass-lens test-body)))

  (define-test "rigid-body-inertia-lens view"
    (assert-equal 2.0 (view rigid-body-inertia-lens test-body)))

  (define-test "rigid-body-pos-lens set"
    (let* ([new-body (set-lens rigid-body-pos-lens (vec2 100 200) test-body)]
           [pos (rigid-body-pos new-body)])
      (assert-equal 100 (vec2-x pos))
      (assert-equal 200 (vec2-y pos))
      ;; Other fields unchanged
      (assert-equal 0.5 (rigid-body-angle new-body))))

  (define-test "rigid-body-vel-lens over"
    (let* ([new-body (over rigid-body-vel-lens
                          (lambda (v) (vec2-scale v 2))
                          test-body)]
           [vel (rigid-body-vel new-body)])
      (assert-equal 2 (vec2-x vel))
      (assert-equal 4 (vec2-y vel))))

  (define-test "rigid-body-angle-lens over"
    (let ([new-body (over rigid-body-angle-lens
                         (lambda (a) (+ a 1.0))
                         test-body)])
      (assert-equal 1.5 (rigid-body-angle new-body)))))

(doc 'section 'composed-lens-tests)

(test-group "Composed Lenses"
  (define-test "rigid-body-pos-x-lens view"
    (assert-equal 10 (view rigid-body-pos-x-lens test-body)))

  (define-test "rigid-body-pos-y-lens view"
    (assert-equal 20 (view rigid-body-pos-y-lens test-body)))

  (define-test "rigid-body-vel-x-lens view"
    (assert-equal 1 (view rigid-body-vel-x-lens test-body)))

  (define-test "rigid-body-vel-y-lens view"
    (assert-equal 2 (view rigid-body-vel-y-lens test-body)))

  (define-test "rigid-body-pos-x-lens set"
    (let ([new-body (set-lens rigid-body-pos-x-lens 999 test-body)])
      (assert-equal 999 (vec2-x (rigid-body-pos new-body)))
      (assert-equal 20 (vec2-y (rigid-body-pos new-body)))))

  (define-test "rigid-body-pos-y-lens over"
    (let ([new-body (over rigid-body-pos-y-lens (lambda (y) (+ y 100)) test-body)])
      (assert-equal 10 (vec2-x (rigid-body-pos new-body)))
      (assert-equal 120 (vec2-y (rigid-body-pos new-body)))))

  (define-test "manual lens composition"
    (let ([composed (lens-compose rigid-body-pos-lens vec2-x-lens)])
      (assert-equal 10 (view composed test-body))
      (let ([new-body (set-lens composed 42 test-body)])
        (assert-equal 42 (vec2-x (rigid-body-pos new-body)))))))

(doc 'section 'particle-lens-tests)

(test-group "Particle Lenses"
  (define-test "particle-pos-lens view"
    (let ([pos (view particle-pos-lens test-particle)])
      (assert-equal 5 (vec2-x pos))
      (assert-equal 10 (vec2-y pos))))

  (define-test "particle-vel-lens view"
    (let ([vel (view particle-vel-lens test-particle)])
      (assert-equal -1 (vec2-x vel))
      (assert-equal 0 (vec2-y vel))))

  (define-test "particle-lifetime-lens view"
    (assert-equal 100 (view particle-lifetime-lens test-particle)))

  (define-test "particle-size-lens view"
    (assert-equal 1.5 (view particle-size-lens test-particle)))

  (define-test "particle-color-lens view"
    (assert-equal 'red (view particle-color-lens test-particle)))

  (define-test "particle-lifetime-lens set"
    (let ([new-p (set-lens particle-lifetime-lens 50 test-particle)])
      (assert-equal 50 (particle-lifetime new-p))
      ;; Other fields unchanged
      (assert-equal 1.5 (particle-size new-p))))

  (define-test "particle-size-lens over"
    (let ([new-p (over particle-size-lens (lambda (s) (* s 2)) test-particle)])
      (assert-equal 3.0 (particle-size new-p))))

  (define-test "particle-color-lens set"
    (let ([new-p (set-lens particle-color-lens 'blue test-particle)])
      (assert-equal 'blue (particle-color new-p))))

  (define-test "particle-pos-x-lens view"
    (assert-equal 5 (view particle-pos-x-lens test-particle)))

  (define-test "particle-pos-y-lens set"
    (let ([new-p (set-lens particle-pos-y-lens 99 test-particle)])
      (assert-equal 5 (vec2-x (particle-pos new-p)))
      (assert-equal 99 (vec2-y (particle-pos new-p))))))

(doc 'section 'generic-body-lens-tests)

(test-group "Generic Body Lenses"
  (define-test "body-pos-lens works on rigid-body"
    (let ([pos (view body-pos-lens test-body)])
      (assert-equal 10 (vec2-x pos))
      (assert-equal 20 (vec2-y pos))))

  (define-test "body-pos-lens works on particle"
    (let ([pos (view body-pos-lens test-particle)])
      (assert-equal 5 (vec2-x pos))
      (assert-equal 10 (vec2-y pos))))

  (define-test "body-vel-lens works on rigid-body"
    (let ([vel (view body-vel-lens test-body)])
      (assert-equal 1 (vec2-x vel))
      (assert-equal 2 (vec2-y vel))))

  (define-test "body-vel-lens works on particle"
    (let ([vel (view body-vel-lens test-particle)])
      (assert-equal -1 (vec2-x vel))
      (assert-equal 0 (vec2-y vel))))

  (define-test "body-pos-lens set on rigid-body"
    (let* ([new-body (set-lens body-pos-lens (vec2 50 60) test-body)]
           [pos (rigid-body-pos new-body)])
      (assert-true (rigid-body? new-body))
      (assert-equal 50 (vec2-x pos))
      (assert-equal 60 (vec2-y pos))))

  (define-test "body-pos-lens set on particle"
    (let* ([new-p (set-lens body-pos-lens (vec2 50 60) test-particle)]
           [pos (particle-pos new-p)])
      (assert-true (particle? new-p))
      (assert-equal 50 (vec2-x pos))
      (assert-equal 60 (vec2-y pos))))

  (define-test "body-mass-lens works on rigid-body"
    (assert-equal 5.0 (view body-mass-lens test-body)))

  (define-test "body-mass-lens works on particle (implicit 1.0)"
    (assert-equal 1.0 (view body-mass-lens test-particle)))

  (define-test "body-mass-lens set on rigid-body"
    (let ([new-body (set-lens body-mass-lens 10.0 test-body)])
      (assert-equal 10.0 (rigid-body-mass new-body))
      ;; inv-mass should be recalculated
      (assert-equal 0.1 (rigid-body-inv-mass new-body))))

  (define-test "body-mass-lens set on particle is no-op"
    ;; Setting mass on particle should return same particle
    (let ([new-p (set-lens body-mass-lens 999 test-particle)])
      (assert-equal 1.0 (view body-mass-lens new-p))))

  (define-test "particle-mass-lens view"
    (assert-equal 1.0 (view particle-mass-lens test-particle)))

  (define-test "particle-mass-lens set is no-op"
    ;; Setting mass on particle via particle-mass-lens should return same particle
    (assert-equal 1.0 (view particle-mass-lens
                            (set-lens particle-mass-lens 999 test-particle)))))

(doc 'section 'dot-notation-tests)

(test-group "Dot Notation"
  (define-test "(body. pos) view"
    (let ([pos (view (body. pos) test-body)])
      (assert-equal 10 (vec2-x pos))))

  (define-test "(body. pos x) view"
    (assert-equal 10 (view (body. pos x) test-body)))

  (define-test "(body. pos y) view"
    (assert-equal 20 (view (body. pos y) test-body)))

  (define-test "(body. vel) view"
    (let ([vel (view (body. vel) test-body)])
      (assert-equal 1 (vec2-x vel))))

  (define-test "(body. vel x) view"
    (assert-equal 1 (view (body. vel x) test-body)))

  (define-test "(body. angle) view"
    (assert-equal 0.5 (view (body. angle) test-body)))

  (define-test "(body. angular-vel) view"
    (assert-equal 0.1 (view (body. angular-vel) test-body)))

  (define-test "(body. mass) view"
    (assert-equal 5.0 (view (body. mass) test-body)))

  (define-test "(body. inertia) view"
    (assert-equal 2.0 (view (body. inertia) test-body)))

  (define-test "(body. pos x) over"
    (let ([new-body (over (body. pos x) (lambda (x) (+ x 100)) test-body)])
      (assert-equal 110 (vec2-x (rigid-body-pos new-body)))))

  (define-test "(body. lifetime) view on particle"
    (assert-equal 100 (view (body. lifetime) test-particle)))

  (define-test "(body. size) view on particle"
    (assert-equal 1.5 (view (body. size) test-particle)))

  (define-test "(body. color) view on particle"
    (assert-equal 'red (view (body. color) test-particle))))

(doc 'section 'lens-laws-tests)

(test-group "Lens Laws"
  ;; Get-Put: set l (view l s) s = s
  (define-test "Get-Put law for rigid-body-pos-lens"
    (let* ([pos (view rigid-body-pos-lens test-body)]
           [body2 (set-lens rigid-body-pos-lens pos test-body)])
      (assert-equal (rigid-body-pos test-body) (rigid-body-pos body2))
      (assert-equal (rigid-body-vel test-body) (rigid-body-vel body2))
      (assert-equal (rigid-body-angle test-body) (rigid-body-angle body2))))

  ;; Put-Get: view l (set l a s) = a
  (define-test "Put-Get law for rigid-body-pos-lens"
    (let* ([new-pos (vec2 999 888)]
           [new-body (set-lens rigid-body-pos-lens new-pos test-body)])
      (assert-equal (vec2-x new-pos) (vec2-x (view rigid-body-pos-lens new-body)))
      (assert-equal (vec2-y new-pos) (vec2-y (view rigid-body-pos-lens new-body)))))

  ;; Put-Put: set l a' (set l a s) = set l a' s
  (define-test "Put-Put law for rigid-body-angle-lens"
    (let* ([a1 1.0]
           [a2 2.0]
           [body-via-two (set-lens rigid-body-angle-lens a2
                                   (set-lens rigid-body-angle-lens a1 test-body))]
           [body-direct (set-lens rigid-body-angle-lens a2 test-body)])
      (assert-equal (rigid-body-angle body-via-two)
                    (rigid-body-angle body-direct)))))

(doc 'section 'aliases-tests)

(test-group "Aliases"
  (define-test "position-lens alias"
    (assert-equal (view body-pos-lens test-body)
                  (view position-lens test-body)))

  (define-test "velocity-lens alias"
    (assert-equal (view body-vel-lens test-body)
                  (view velocity-lens test-body)))

  (define-test "mass-lens alias"
    (assert-equal (view rigid-body-mass-lens test-body)
                  (view mass-lens test-body)))

  (define-test "rotation-lens alias"
    (assert-equal (view rigid-body-angle-lens test-body)
                  (view rotation-lens test-body))))

(doc 'section 'integration-utility-tests)

(test-group "Integration Utilities"
  (define-test "integrate-position-via-lens"
    (let* ([dt 0.1]
           [new-body (integrate-position-via-lens
                      rigid-body-pos-lens
                      rigid-body-vel-lens
                      dt
                      test-body)]
           [expected-x (+ 10 (* 1 0.1))]   ; pos.x + vel.x * dt
           [expected-y (+ 20 (* 2 0.1))])  ; pos.y + vel.y * dt
      (assert-equal expected-x (vec2-x (rigid-body-pos new-body)))
      (assert-equal expected-y (vec2-y (rigid-body-pos new-body))))))

(doc 'section 'run-tests)

(run-all-tests)
