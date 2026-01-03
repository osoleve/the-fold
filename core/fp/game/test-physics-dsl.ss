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
(load "core/fp/game/physics-dsl.ss")

;;; ============================================================
;;; Test Setup
;;; ============================================================

(set! *current-group* 'physics-dsl)

;;; Default gravity for tests
(define test-gravity (vec2 0 -10))

;;; Default timestep (60 fps)
(define test-dt 0.016667)

;;; ============================================================
;;; PhysicsF Functor Tests
;;; ============================================================

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

;;; ============================================================
;;; Lifting Function Tests
;;; ============================================================

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

;;; ============================================================
;;; Monadic Combinator Tests
;;; ============================================================

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

;;; ============================================================
;;; Pure World Tests
;;; ============================================================

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

(define-test "pure-apply-force modifies velocity"
  (let* ([pw (make-pure-world test-gravity)]
         [entity (make-pure-entity 'ball (vec2 0 0) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [pw3 (pure-apply-force pw2 'ball (vec2 10 0))]
         [ball (pure-world-get-entity pw3 'ball)]
         [vel (entity-vel ball)])
        ;; Force of 10 on mass 1 = velocity change of 10
        (assert-equal 10.0 (vec2-x vel))))

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

;;; ============================================================
;;; Pure Interpreter Tests
;;; ============================================================

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

(define-test "run-physics-pure applies force"
  (let* ([pw (make-pure-world test-gravity)]
         [entity (make-pure-entity 'ball (vec2 0 0) 1.0)]
         [pw2 (pure-world-add-entity pw entity)]
         [program (phys-apply-force 'ball (vec2 5 0))]
         [result ((run-physics-pure program) pw2)]
         [new-world (cdr result)]
         [ball (pure-world-get-entity new-world 'ball)])
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

;;; ============================================================
;;; Falling Body Scenario Test
;;; ============================================================

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

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "Running Physics DSL Tests...\n")
(display "===========================\n\n")
(run-tests 'physics-dsl)
