(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "lattice/physics/lenses/optics-integration.ss")

(doc 'module 'test-optics-integration)
(doc 'description "Tests for Physics Optics Integration")
(doc 'layer 'lattice)
(doc 'note "Run with: scheme --script lattice/physics/lenses/test-optics-integration.ss")

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

(define dead-particle
  (make-particle (vec2 0 0)
                 (vec2 0 0)
                 0                ; lifetime = 0 (dead)
                 100
                 1.0
                 'gray
                 #f))

(define test-bodies
  (list test-body test-particle))

(define test-world
  (make-world
   (list (cons 'player test-body)
         (cons 'enemy (make-rigid-body (vec2 100 100)
                                       (vec2 -2 -2)
                                       1.0
                                       0.0
                                       3.0
                                       1.0))
         (cons 'particle test-particle))))

(doc 'section 'optics-operators-tests)

(test-group "Optics Operators with Physics Lenses"
  (define-test "^. view with body-pos-lens"
    (let ([pos (^. test-body body-pos-lens)])
      (assert-equal 10 (vec2-x pos))
      (assert-equal 20 (vec2-y pos))))

  (define-test "^. view with body-vel-lens"
    (let ([vel (^. test-body body-vel-lens)])
      (assert-equal 1 (vec2-x vel))
      (assert-equal 2 (vec2-y vel))))

  (define-test "^. view with composed lens (body. pos x)"
    (assert-equal 10 (^. test-body (body. pos x))))

  (define-test "^. view with (body. vel y)"
    (assert-equal 2 (^. test-body (body. vel y))))

  (define-test "%~ modify position x"
    (let ([new-body (& test-body (%~ (body. pos x) (lambda (x) (+ x 100))))])
      (assert-equal 110 (vec2-x (rigid-body-pos new-body)))
      (assert-equal 20 (vec2-y (rigid-body-pos new-body)))))

  (define-test "%~ modify velocity"
    (let ([new-body (& test-body (%~ (body. vel) (lambda (v) (vec2-scale v 2))))])
      (assert-equal 2 (vec2-x (rigid-body-vel new-body)))
      (assert-equal 4 (vec2-y (rigid-body-vel new-body)))))

  (define-test ".~ set position"
    (let ([new-body (& test-body (.~ (body. pos) (vec2 999 888)))])
      (assert-equal 999 (vec2-x (rigid-body-pos new-body)))
      (assert-equal 888 (vec2-y (rigid-body-pos new-body)))))

  (define-test ".~ set angle"
    (let ([new-body (& test-body (.~ (body. angle) 3.14))])
      (assert-equal 3.14 (rigid-body-angle new-body))))

  (define-test "chained modifications"
    (let ([new-body
           (& test-body
              (%~ (body. pos x) (lambda (x) (+ x 10))))])
      (let ([new-body2
             (& new-body
                (%~ (body. vel y) (lambda (y) (* y 2))))])
        (assert-equal 20 (vec2-x (rigid-body-pos new-body2)))
        (assert-equal 4 (vec2-y (rigid-body-vel new-body2)))))))

(doc 'section 'composition-operator-tests)

(test-group ">>> Composition Operator"
  (define-test ">>> composes lenses left-to-right"
    (let ([pos-x-lens (>>> body-pos-lens vec2-x-lens)])
      (assert-equal 10 (view pos-x-lens test-body))))

  (define-test ">>> composes for deep access"
    (let ([pos-y (>>> body-pos-lens vec2-y-lens)])
      (let ([new-body (over pos-y (lambda (y) (+ y 100)) test-body)])
        (assert-equal 120 (vec2-y (rigid-body-pos new-body))))))

  (define-test ">>> with multiple compositions"
    ;; world -> body -> pos -> x (using affines for world)
    (let* ([body-x (>>> body-pos-lens vec2-x-lens)]
           [player-body-affine (world-body 'player)])
      ;; Get player position x via preview
      (let ([maybe-player (^? test-world player-body-affine)])
        (assert-true (just? maybe-player))
        (assert-equal 10 (vec2-x (rigid-body-pos (from-just maybe-player))))))))

(doc 'section 'traversal-tests)

(test-group "Physics Traversals"
  (define-test "bodies-each traverses all bodies"
    (let ([positions (traversal-to-list bodies-each test-bodies)])
      (assert-equal 2 (length positions))))

  (define-test "bodies-each modifies all bodies"
    (let* ([modified (traversal-over bodies-each
                                     (lambda (b) (& b (%~ (body. pos x)
                                                         (lambda (x) (+ x 1000)))))
                                     test-bodies)]
           [b1 (car modified)]
           [b2 (cadr modified)])
      (assert-equal 1010 (vec2-x (view body-pos-lens b1)))
      (assert-equal 1005 (vec2-x (view body-pos-lens b2)))))

  (define-test "particles-alive filters dead particles"
    (let* ([particles (list test-particle dead-particle)]
           [alive (traversal-to-list particles-alive particles)])
      (assert-equal 1 (length alive))
      (assert-equal 100 (particle-lifetime (car alive)))))

  (define-test "rigid-bodies-only filters to rigid bodies"
    (let* ([mixed (list test-body test-particle)]
           [rigid (traversal-to-list rigid-bodies-only mixed)])
      (assert-equal 1 (length rigid))
      (assert-true (rigid-body? (car rigid)))))

  (define-test "particles-only filters to particles"
    (let* ([mixed (list test-body test-particle)]
           [parts (traversal-to-list particles-only mixed)])
      (assert-equal 1 (length parts))
      (assert-true (particle? (car parts))))))

(doc 'section 'affine-tests)

(test-group "Physics Affines"
  (define-test "affine-rigid-angle works on rigid body"
    (let ([maybe-angle (^? test-body affine-rigid-angle)])
      (assert-true (just? maybe-angle))
      (assert-equal 0.5 (from-just maybe-angle))))

  (define-test "affine-rigid-angle returns nothing for particle"
    (let ([maybe-angle (^? test-particle affine-rigid-angle)])
      (assert-true (nothing? maybe-angle))))

  (define-test "affine-rigid-angular-vel works on rigid body"
    (let ([maybe-omega (^? test-body affine-rigid-angular-vel)])
      (assert-true (just? maybe-omega))
      (assert-equal 0.1 (from-just maybe-omega))))

  (define-test "affine-particle-lifetime works on particle"
    (let ([maybe-life (^? test-particle affine-particle-lifetime)])
      (assert-true (just? maybe-life))
      (assert-equal 100 (from-just maybe-life))))

  (define-test "affine-particle-lifetime returns nothing for rigid body"
    (let ([maybe-life (^? test-body affine-particle-lifetime)])
      (assert-true (nothing? maybe-life))))

  (define-test "affine-over modifies when target exists"
    (let ([modified (affine-over affine-rigid-angle
                                 (lambda (a) (+ a 1.0))
                                 test-body)])
      (assert-equal 1.5 (rigid-body-angle modified))))

  (define-test "affine-over is no-op when target missing"
    (let ([modified (affine-over affine-rigid-angle
                                 (lambda (a) (+ a 1.0))
                                 test-particle)])
      ;; Should return same particle unchanged
      (assert-true (particle? modified)))))

(doc 'section 'world-optics-tests)

(test-group "World Optics"
  (define-test "world-body finds existing body"
    (let ([maybe-player (^? test-world (world-body 'player))])
      (assert-true (just? maybe-player))
      (assert-true (rigid-body? (from-just maybe-player)))))

  (define-test "world-body returns nothing for missing body"
    (let ([maybe-ghost (^? test-world (world-body 'ghost))])
      (assert-true (nothing? maybe-ghost))))

  (define-test "world-body can be used to modify"
    (let* ([player-lens (world-body 'player)]
           [maybe-player (^? test-world player-lens)])
      (when (just? maybe-player)
        (let* ([player (from-just maybe-player)]
               [new-player (& player (.~ (body. pos) (vec2 500 500)))]
               [new-world (affine-set player-lens new-player test-world)]
               [updated-player (^? new-world player-lens)])
          (assert-true (just? updated-player))
          (assert-equal 500 (vec2-x (view body-pos-lens (from-just updated-player))))))))

  (define-test "world-all-bodies traverses all bodies"
    (let ([all (traversal-to-list world-all-bodies test-world)])
      (assert-equal 3 (length all))))

  (define-test "world-all-bodies can modify all"
    (let* ([new-world (traversal-over world-all-bodies
                                      (lambda (b)
                                        (& b (%~ (body. pos y)
                                                 (lambda (y) (+ y 1000)))))
                                      test-world)]
           [all (traversal-to-list world-all-bodies new-world)])
      (assert-true (> (vec2-y (view body-pos-lens (car all))) 999)))))

(doc 'section 'convenience-function-tests)

(test-group "Convenience Functions"
  (define-test "body-view works"
    (assert-equal 10 (vec2-x (body-view test-body (body. pos)))))

  (define-test "body-modify works"
    (let ([new-body (body-modify test-body (body. pos x) (lambda (x) (* x 2)))])
      (assert-equal 20 (vec2-x (rigid-body-pos new-body)))))

  (define-test "body-set works"
    (let ([new-body (body-set test-body (body. angle) 99.0)])
      (assert-equal 99.0 (rigid-body-angle new-body))))

  (define-test "body-preview works with affines"
    (let ([maybe-angle (body-preview test-body affine-rigid-angle)])
      (assert-true (just? maybe-angle))
      (assert-equal 0.5 (from-just maybe-angle))))

  (define-test "apply-gravity modifies velocity"
    (let ([new-body ((apply-gravity -9.8) test-body)])
      ;; vel.y was 2, now should be 2 + (-9.8) = -7.8
      (assert-true (< (abs (- (vec2-y (rigid-body-vel new-body)) -7.8)) 0.001))))

  (define-test "apply-impulse adds to velocity"
    (let ([new-body ((apply-impulse (vec2 10 20)) test-body)])
      ;; vel was (1, 2), now should be (11, 22)
      (assert-equal 11 (vec2-x (rigid-body-vel new-body)))
      (assert-equal 22 (vec2-y (rigid-body-vel new-body)))))

  (define-test "move-by adds to position"
    (let ([new-body ((move-by (vec2 100 200)) test-body)])
      (assert-equal 110 (vec2-x (rigid-body-pos new-body)))
      (assert-equal 220 (vec2-y (rigid-body-pos new-body))))))

(doc 'section 'fold-operation-tests)

(test-group "Fold Operations"
  (define-test "total-mass sums masses"
    ;; test-body has mass 5.0, test-particle has implicit mass 1.0
    (let ([total (total-mass test-bodies)])
      (assert-equal 6.0 total)))

  (define-test "center-of-mass computes weighted average"
    ;; body at (10, 20) with mass 5, particle at (5, 10) with mass 1
    ;; CoM = ((10*5 + 5*1)/6, (20*5 + 10*1)/6) = (55/6, 110/6)
    (let ([com (center-of-mass test-bodies)])
      (assert-true (< (abs (- (vec2-x com) (/ 55 6))) 0.01))
      (assert-true (< (abs (- (vec2-y com) (/ 110 6))) 0.01))))

  (define-test "bodies-in-region filters by distance"
    (let* ([region-fold (bodies-in-region (vec2 0 0) 20)]
           [nearby (fold-to-list region-fold test-bodies)])
      ;; test-body at (10, 20) - distance ~22.4, outside
      ;; test-particle at (5, 10) - distance ~11.2, inside
      (assert-equal 1 (length nearby))
      (assert-true (particle? (car nearby))))))

(doc 'section 'physics-step-tests)

(test-group "Physics Step Functions"
  (define-test "step-body integrates position"
    (let* ([dt 0.1]
           [new-body ((step-body dt) test-body)]
           ;; pos.x = 10 + 1*0.1 = 10.1
           ;; pos.y = 20 + 2*0.1 = 20.2
           [new-pos (rigid-body-pos new-body)])
      (assert-true (< (abs (- (vec2-x new-pos) 10.1)) 0.001))
      (assert-true (< (abs (- (vec2-y new-pos) 20.2)) 0.001))))

  (define-test "step-bodies updates all bodies"
    (let* ([dt 0.1]
           [new-bodies ((step-bodies dt) test-bodies)])
      (assert-equal 2 (length new-bodies))
      ;; First body (rigid) should have moved
      (let ([b1-pos (view body-pos-lens (car new-bodies))])
        (assert-true (< (abs (- (vec2-x b1-pos) 10.1)) 0.001)))))

  (define-test "step-world updates world bodies"
    (let* ([dt 0.1]
           [new-world ((step-world dt) test-world)]
           [maybe-player (^? new-world (world-body 'player))])
      (assert-true (just? maybe-player))
      (let ([player-pos (view body-pos-lens (from-just maybe-player))])
        ;; Player started at (10, 20) with vel (1, 2)
        (assert-true (< (abs (- (vec2-x player-pos) 10.1)) 0.001))))))

(doc 'section 'integration-example-tests)

(test-group "Integration Examples"
  (define-test "complete physics update pipeline"
    ;; Demonstrate: apply gravity, then step
    (let* ([dt 0.1]
           [gravity -9.8]
           ;; Apply gravity to all bodies, then step them
           [updated (traversal-over bodies-each
                      (lambda (b)
                        ((step-body dt) ((apply-gravity gravity) b)))
                      test-bodies)]
           ;; Check first body
           [b1 (car updated)]
           [b1-vel (view body-vel-lens b1)]
           [b1-pos (view body-pos-lens b1)])
      ;; vel.y was 2, now 2 + (-9.8) = -7.8
      ;; Then pos.y = 20 + (-7.8)*0.1 = 19.22
      (assert-true (< (abs (- (vec2-y b1-vel) -7.8)) 0.001))
      (assert-true (< (abs (- (vec2-y b1-pos) 19.22)) 0.001))))

  (define-test "world operations with optics"
    ;; Move player right, give enemy upward velocity
    (let* ([w1 (affine-over (>>> (world-body 'player) (lens->affine (body. pos x)))
                            (lambda (x) (+ x 50))
                            test-world)]
           [w2 (affine-over (>>> (world-body 'enemy) (lens->affine (body. vel y)))
                            (lambda (y) (+ y 100))
                            w1)]
           [player (from-just (^? w2 (world-body 'player)))]
           [enemy (from-just (^? w2 (world-body 'enemy)))])
      ;; Player pos.x was 10, now 60
      (assert-equal 60 (vec2-x (view body-pos-lens player)))
      ;; Enemy vel.y was -2, now 98
      (assert-equal 98 (vec2-y (view body-vel-lens enemy))))))

(doc 'section 'run-tests)

(run-all-tests)
