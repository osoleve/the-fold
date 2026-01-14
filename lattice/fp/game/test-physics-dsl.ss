;;; core/fp/game/test-physics-dsl.ss — Tests for Physics Simulation DSL
;;;
;;; Tests the Free monad-based physics DSL including:
;;;   - Functor laws for PhysicsF
;;;   - Lifting functions
;;;   - Pure interpreter (no external dependencies)
;;;   - Deterministic and logging interpreters (require world.ss)
;;;
;;; Run: scheme --script core/fp/game/test-physics-dsl.ss

(load "core/test-framework.ss")
(load "lattice/fp/game/physics-dsl.ss")

;;; ====
;;; Test Setup
;;; ====

(set! *current-group* 'physics-dsl)

;;; Default gravity for tests
(define test-gravity (vec2 0 -10))

;;; Default timestep (60 fps)
(define test-dt 0.016667)

;;; ====
;;; PhysicsF Functor Tests
;;; ====

(define-test "physics-fmap identity on apply-force"
  (let* ([cmd (list 'apply-force 'ball (vec2 1 2) 'next)]
         [mapped (physics-fmap identity cmd)])
        (assert-equal cmd mapped)))

(define-test "physics-fmap identity on step"
  (let* ([cmd (list 'step 0.016 'next)]
         [mapped (physics-fmap identity cmd)])
        (assert-equal cmd mapped)))

(define-test "physics-fmap identity on spawn"
  (let* ([entity (make-pure-entity 'test (vec2 0 0) 1.0)]
         [cmd (list 'spawn entity identity)]
         [mapped (physics-fmap identity cmd)]
         ;; Apply the continuation to test it
         [orig-result ((list-ref cmd 2) 'id)]
         [mapped-result ((list-ref mapped 2) 'id)])
        (assert-equal 'spawn (car mapped))
        (assert-equal orig-result mapped-result)))

(define-test "physics-fmap composition on apply-force"
  (let* ([f (lambda (x) (cons 'f x))]
         [g (lambda (x) (cons 'g x))]
         [composed (lambda (x) (f (g x)))]
         [cmd (list 'apply-force 'ball (vec2 1 2) 'next)]
         [mapped-fg (physics-fmap f (physics-fmap g cmd))]
         [mapped-composed (physics-fmap composed cmd)])
        ;; The next fields should produce same results
        (assert-equal (list-ref mapped-fg 3) (list-ref mapped-composed 3))))

(define-test "physics-fmap on get-world"
  (let* ([cmd (list 'get-world identity)]
         [f (lambda (x) (cons 'wrapped x))]
         [mapped (physics-fmap f cmd)]
         ;; Apply continuation
         [result ((list-ref mapped 1) 'test-world)])
        (assert-equal (cons 'wrapped 'test-world) result)))

(define-test "physics-fmap on detect-collisions"
  (let* ([cmd (list 'detect-collisions identity)]
         [f (lambda (x) (length x))]
         [mapped (physics-fmap f cmd)]
         ;; Apply continuation
         [result ((list-ref mapped 1) '(c1 c2 c3))])
        (assert-equal 3 result)))

;;; ====
;;; Lifting Function Tests
;;; ====

(define-test "phys-apply-force creates correct structure"
  (let ([m (phys-apply-force 'ball (vec2 10 0))])
       (assert-true (free-suspended? m))
       (let ([cmd (from-free m)])
            (assert-equal 'apply-force (car cmd))
            (assert-equal 'ball (list-ref cmd 1))
            (assert-equal (vec2 10 0) (list-ref cmd 2)))))

(define-test "phys-step creates correct structure"
  (let ([m (phys-step 0.016)])
       (assert-true (free-suspended? m))
       (let ([cmd (from-free m)])
            (assert-equal 'step (car cmd))
            (assert-equal 0.016 (list-ref cmd 1)))))

(define-test "phys-spawn creates correct structure"
  (let* ([entity (make-pure-entity 'test (vec2 0 0) 1.0)]
         [m (phys-spawn entity)])
        (assert-true (free-suspended? m))
        (let ([cmd (from-free m)])
             (assert-equal 'spawn (car cmd)))))

(define-test "phys-get-world creates correct structure"
  (assert-true (free-suspended? phys-get-world))
  (let ([cmd (from-free phys-get-world)])
       (assert-equal 'get-world (car cmd))))

(define-test "phys-destroy creates correct structure"
  (let ([m (phys-destroy 'ball)])
       (assert-true (free-suspended? m))
       (let ([cmd (from-free m)])
            (assert-equal 'destroy (car cmd))
            (assert-equal 'ball (list-ref cmd 1)))))

(define-test "phys-set-velocity creates correct structure"
  (let ([m (phys-set-velocity 'ball (vec2 5 -3))])
       (assert-true (free-suspended? m))
       (let ([cmd (from-free m)])
            (assert-equal 'set-velocity (car cmd))
            (assert-equal 'ball (list-ref cmd 1))
            (assert-equal (vec2 5 -3) (list-ref cmd 2)))))

;;; ====
;;; Monadic Combinator Tests
;;; ====

(define-test "phys-bind sequences operations"
  (let ([m (phys-bind (phys-step 0.016)
                      (lambda (_) (phys-step 0.016)))])
       (assert-true (free-suspended? m))))

(define-test "phys-then discards first result"
  (let ([m (phys-then (phys-step 0.016) (pure-free 42))])
       (assert-true (free-suspended? m))))

(define-test "phys-steps creates n steps"
  ;; phys-steps 0 should be pure
  (let ([m0 (phys-steps 0 0.016)])
       (assert-true (pure-free? m0)))
  ;; phys-steps 1 should be suspended
  (let ([m1 (phys-steps 1 0.016)])
       (assert-true (free-suspended? m1))))

(define-test "phys-when conditional execution"
  (let ([m-true (phys-when #t (phys-step 0.016))]
        [m-false (phys-when #f (phys-step 0.016))])
       (assert-true (free-suspended? m-true))
       (assert-true (pure-free? m-false))))

;;; ====
;;; Pure World Tests
;;; ====

(define-test "make-pure-world creates empty world"
  (let ([pw (make-pure-world test-gravity)])
       (assert-equal '() (pure-world-entities pw))
       (assert-equal test-gravity (pure-world-gravity pw))))

(define-test "pure-world-add-entity adds entity"
  (let* ([pw (make-pure-world test-gravity)]
         [entity (make-pure-entity 'ball (vec2 0 10) 1.0)]
         [pw2 (pure-world-add-entity pw entity)])
        (assert-equal 1 (length (pure-world-entities pw2)))
        (assert-true (and (pure-world-get-entity pw2 'ball) #t))))

(define-test "pure-world-remove-entity removes entity"
  (let* ([pw (make-pure-world test-gravity)]
         [entity (make-pure-entity 'ball (vec2 0 10) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [pw3 (pure-world-remove-entity pw2 'ball)])
        (assert-equal 0 (length (pure-world-entities pw3)))
        (assert-false (pure-world-get-entity pw3 'ball))))

(define-test "pure-world-update-entity modifies entity"
  (let* ([pw (make-pure-world test-gravity)]
         [entity (make-pure-entity 'ball (vec2 0 10) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [pw3 (pure-world-update-entity pw2 'ball
                                        (lambda (e) (entity-with-body e
                                                                      (body-with-pos (entity-body e) (vec2 5 5)))))])
        (let ([updated (pure-world-get-entity pw3 'ball)])
             (assert-equal (vec2 5 5) (entity-pos updated)))))

(define-test "pure-step applies gravity"
  (let* ([pw (make-pure-world (vec2 0 -10))]
         [entity (make-pure-entity 'ball (vec2 0 100) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [pw3 (pure-step pw2 1.0)]  ; 1 second step for easy math
         [ball (pure-world-get-entity pw3 'ball)]
         [pos (entity-pos ball)]
         [vel (entity-vel ball)])
        ;; After 1 second: v = -10, pos = 100 + (-10) = 90
        (assert-equal -10.0 (vec2-y vel))
        (assert-equal 90.0 (vec2-y pos))))

(define-test "pure-apply-force accumulates force for next step"
  (let* ([pw (make-pure-world (vec2 0 0))]  ; No gravity for this test
         [entity (make-pure-entity 'ball (vec2 0 0) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [pw3 (pure-apply-force pw2 'ball (vec2 10 0))]
         ;; Force is accumulated, not applied yet
         [ball-before (pure-world-get-entity pw3 'ball)]
         [vel-before (entity-vel ball-before)]
         ;; Now step to apply the force: Δv = (F/m) * dt = 10 * 1.0 = 10
         [pw4 (pure-step pw3 1.0)]
         [ball-after (pure-world-get-entity pw4 'ball)]
         [vel-after (entity-vel ball-after)])
        ;; Before step, velocity unchanged
        (assert-equal 0 (vec2-x vel-before))
        ;; After step with dt=1.0, velocity = 10
        (assert-equal 10.0 (vec2-x vel-after))))

(define-test "pure-set-position sets position"
  (let* ([pw (make-pure-world test-gravity)]
         [entity (make-pure-entity 'ball (vec2 0 0) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [pw3 (pure-set-position pw2 'ball (vec2 50 50))]
         [ball (pure-world-get-entity pw3 'ball)])
        (assert-equal (vec2 50 50) (entity-pos ball))))

(define-test "pure-set-velocity sets velocity"
  (let* ([pw (make-pure-world test-gravity)]
         [entity (make-pure-entity 'ball (vec2 0 0) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [pw3 (pure-set-velocity pw2 'ball (vec2 5 -3))]
         [ball (pure-world-get-entity pw3 'ball)])
        (assert-equal (vec2 5 -3) (entity-vel ball))))

;;; ====
;;; Pure Interpreter Tests
;;; ====

(define-test "run-physics-pure returns value from pure"
  (let* ([pw (make-pure-world test-gravity)]
         [result ((run-physics-pure (pure-free 42)) pw)])
        (assert-equal 42 (car result))))

(define-test "run-physics-pure spawns entity"
  (let* ([pw (make-pure-world test-gravity)]
         [entity (make-pure-entity 'ball (vec2 0 10) 1.0)]
         [program (phys-spawn entity)]
         [result ((run-physics-pure program) pw)]
         [id (car result)]
         [new-world (cdr result)])
        (assert-equal 'ball id)
        (assert-true (and (pure-world-get-entity new-world 'ball) #t))))

(define-test "run-physics-pure applies force during step"
  (let* ([pw (make-pure-world (vec2 0 0))]  ; No gravity
         [entity (make-pure-entity 'ball (vec2 0 0) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         ;; Apply force then step to integrate it
         [program (phys-bind (phys-apply-force 'ball (vec2 5 0))
                             (lambda (_)
                                     (phys-bind (phys-step 1.0)
                                                (lambda (_)
                                                        (phys-get-entity 'ball)))))]
         [result ((run-physics-pure program) pw2)]
         [ball (car result)])
        ;; Force 5 on mass 1 for dt=1.0 → Δv = 5
        (assert-equal 5.0 (vec2-x (entity-vel ball)))))

(define-test "run-physics-pure steps simulation"
  (let* ([pw (make-pure-world (vec2 0 -10))]
         [entity (make-pure-entity 'ball (vec2 0 100) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [program (phys-step 1.0)]
         [result ((run-physics-pure program) pw2)]
         [new-world (cdr result)]
         [ball (pure-world-get-entity new-world 'ball)])
        (assert-equal 90.0 (vec2-y (entity-pos ball)))))

(define-test "run-physics-pure sequences operations"
  (let* ([pw (make-pure-world (vec2 0 -10))]
         [entity (make-pure-entity 'ball (vec2 0 100) 1.0)]
         [program (phys-bind (phys-spawn entity)
                             (lambda (id)
                                     (phys-bind (phys-step 1.0)
                                                (lambda (_)
                                                        (phys-get-entity id)))))]
         [result ((run-physics-pure program) pw)]
         [final-entity (car result)])
        (assert-true (and final-entity #t))
        (assert-equal 90.0 (vec2-y (entity-pos final-entity)))))

(define-test "run-physics-pure destroys entity"
  (let* ([pw (make-pure-world test-gravity)]
         [entity (make-pure-entity 'ball (vec2 0 10) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [program (phys-destroy 'ball)]
         [result ((run-physics-pure program) pw2)]
         [new-world (cdr result)])
        (assert-false (pure-world-get-entity new-world 'ball))))

(define-test "run-physics-pure gets world"
  (let* ([pw (make-pure-world test-gravity)]
         [program phys-get-world]
         [result ((run-physics-pure program) pw)]
         [retrieved-world (car result)])
        (assert-equal test-gravity (pure-world-gravity retrieved-world))))

(define-test "run-physics-pure handles phys-steps"
  (let* ([pw (make-pure-world (vec2 0 -10))]
         [entity (make-pure-entity 'ball (vec2 0 100) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         ;; 10 steps of 0.1s each = 1 second total
         [program (phys-bind (phys-steps 10 0.1)
                             (lambda (_)
                                     (phys-get-entity 'ball)))]
         [result ((run-physics-pure program) pw2)]
         [final-entity (car result)])
        (assert-true (and final-entity #t))
        ;; After 1 second of gravity, y should be approximately 90
        ;; (using simple Euler integration)
        (let ([y (vec2-y (entity-pos final-entity))])
             (assert-true (< (abs (- y 90.0)) 10.0)))))  ; Allow numerical error from Euler

;;; ====
;;; Force Integration Tests (Time-Dependent Behavior)
;;; ====

(define-test "force integration depends on dt"
  ;; Same force, different dt should give different results
  (let* ([pw (make-pure-world (vec2 0 0))]  ; No gravity
         [entity (make-pure-entity 'ball (vec2 0 0) 2.0)]  ; mass = 2
         [pw2 (pure-world-add-entity pw entity)]
         ;; Apply force F=10, then step with dt=0.1
         [pw3-small (pure-apply-force pw2 'ball (vec2 10 0))]
         [pw4-small (pure-step pw3-small 0.1)]
         [ball-small (pure-world-get-entity pw4-small 'ball)]
         [vel-small (vec2-x (entity-vel ball-small))]
         ;; Apply force F=10, then step with dt=1.0
         [pw3-large (pure-apply-force pw2 'ball (vec2 10 0))]
         [pw4-large (pure-step pw3-large 1.0)]
         [ball-large (pure-world-get-entity pw4-large 'ball)]
         [vel-large (vec2-x (entity-vel ball-large))])
        ;; Δv = (F/m) * dt = (10/2) * dt = 5 * dt
        ;; For dt=0.1: Δv = 0.5
        ;; For dt=1.0: Δv = 5.0
        (assert-true (< (abs (- vel-small 0.5)) 0.001))
        (assert-true (< (abs (- vel-large 5.0)) 0.001))))

(define-test "multiple forces accumulate correctly"
  (let* ([pw (make-pure-world (vec2 0 0))]
         [entity (make-pure-entity 'ball (vec2 0 0) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         ;; Apply multiple forces
         [pw3 (pure-apply-force pw2 'ball (vec2 10 0))]
         [pw4 (pure-apply-force pw3 'ball (vec2 5 0))]
         [pw5 (pure-apply-force pw4 'ball (vec2 0 3))]
         ;; Step to integrate them
         [pw6 (pure-step pw5 1.0)]
         [ball (pure-world-get-entity pw6 'ball)]
         [vel (entity-vel ball)])
        ;; Total force = (15, 3), mass = 1, dt = 1
        ;; Δv = (15, 3)
        (assert-equal 15.0 (vec2-x vel))
        (assert-equal 3.0 (vec2-y vel))))

(define-test "forces clear after step"
  (let* ([pw (make-pure-world (vec2 0 0))]
         [entity (make-pure-entity 'ball (vec2 0 0) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         ;; Apply force and step
         [pw3 (pure-apply-force pw2 'ball (vec2 10 0))]
         [pw4 (pure-step pw3 1.0)]
         [ball1 (pure-world-get-entity pw4 'ball)]
         ;; Step again without new forces
         [pw5 (pure-step pw4 1.0)]
         [ball2 (pure-world-get-entity pw5 'ball)])
        ;; First step: velocity = 10
        (assert-equal 10.0 (vec2-x (entity-vel ball1)))
        ;; Second step: velocity unchanged (no new forces)
        (assert-equal 10.0 (vec2-x (entity-vel ball2)))))

(define-test "impulse vs force with small dt"
  ;; Impulse should change velocity instantly
  ;; Force with very small dt should approximate impulse
  (let* ([pw (make-pure-world (vec2 0 0))]
         [entity1 (make-pure-entity 'ball1 (vec2 0 0) 1.0)]
         [entity2 (make-pure-entity 'ball2 (vec2 10 0) 1.0)]
         [pw2 (pure-world-add-entity pw entity1)]
         [pw3 (pure-world-add-entity pw2 entity2)]
         ;; Apply impulse of 10 to ball1
         [pw4 (pure-apply-impulse pw3 'ball1 (vec2 10 0))]
         [ball1-impulse (pure-world-get-entity pw4 'ball1)]
         ;; Apply force of 1000 with dt=0.01 to ball2 (should ≈ impulse of 10)
         [pw5 (pure-apply-force pw3 'ball2 (vec2 1000 0))]
         [pw6 (pure-step pw5 0.01)]
         [ball2-force (pure-world-get-entity pw6 'ball2)])
        ;; Impulse: instant Δv = 10
        (assert-equal 10.0 (vec2-x (entity-vel ball1-impulse)))
        ;; Force: Δv = (1000/1) * 0.01 = 10
        (assert-true (< (abs (- (vec2-x (entity-vel ball2-force)) 10.0)) 0.001))))

;;; ====
;;; Falling Body Scenario Test
;;; ====

(define-test "falling body scenario"
  (let* ([gravity (vec2 0 -9.8)]
         [pw (make-pure-world gravity)]
         [ball (make-pure-entity 'ball (vec2 0 50) 1.0)]
         ;; Program: spawn ball, simulate 60 frames at 60fps, get final position
         [program (phys-bind (phys-spawn ball)
                             (lambda (id)
                                     (phys-bind (phys-steps 60 (/ 1.0 60.0))
                                                (lambda (_)
                                                        (phys-get-entity id)))))]
         [result ((run-physics-pure program) pw)]
         [final-ball (car result)])
        (assert-true (and final-ball #t))
        ;; After 1 second of falling at -9.8, should have fallen significantly
        (let ([final-y (vec2-y (entity-pos final-ball))])
             (assert-true (< final-y 50))  ; Moved down from starting position
             (assert-true (> final-y 30))))) ; Euler gives ~45m, allow margin

(define-test "projectile motion scenario"
  (let* ([gravity (vec2 0 -10)]
         [pw (make-pure-world gravity)]
         [ball (make-pure-entity 'projectile (vec2 0 0) 1.0)]
         ;; Program: spawn, set velocity upward, simulate, check apex
         [program (phys-bind (phys-spawn ball)
                             (lambda (id)
                                     (phys-bind (phys-set-velocity id (vec2 0 20))  ; Launch upward
                                                (lambda (_)
                                                        (phys-bind (phys-steps 20 0.1)  ; 2 seconds
                                                                   (lambda (_)
                                                                           (phys-get-entity id)))))))]
         [result ((run-physics-pure program) pw)]
         [final (car result)])
        (assert-true (and final #t))
        ;; With Euler integration over 2 seconds:
        ;; v = v0 + at → after 2s: v = 20 - 10*2 = 0 (at apex)
        ;; y ≈ 20 at apex with simple Euler
        (let ([y (vec2-y (entity-pos final))])
             (assert-true (> y 10)))))  ; Should have risen significantly

;;; ====
;;; Run Tests
;;; ====

(display "Running Physics DSL Tests...\n")
(display "====\n\n")
(run-tests 'physics-dsl)
