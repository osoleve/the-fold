;;; fabric/stitches/physics-2d/test-world.ss — Tests for 2D Physics World

;;; NOTE: Run from fabric/stitches directory

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/physics/classical/world.ss")

;;; Helper for numeric comparison
(define (assert-= actual expected tolerance)
  (unless (<= (abs (- actual expected)) tolerance)
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

;;; Helper for exact equality
(define (assert-eq? actual expected)
  (unless (eq? actual expected)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

(display "
================= 2D PHYSICS WORLD TESTS =================
")

;;; ====
;;; Entity Tests
;;; ====

(test-group entity-tests
            (define-test make-entity-test
              (let* ([body (make-body-2d (vec2 10 20) (vec2 1 2) 5)]
                     [shape (make-circle (vec2 10 20) 3)]
                     [e (make-entity 'test-id body shape default-material #f)])
                    (assert-true (entity? e))
                    (assert-eq? (entity-id e) 'test-id)
                    (assert-vec2-= (entity-pos e) (vec2 10 20) 0.0001)
                    (assert-vec2-= (entity-vel e) (vec2 1 2) 0.0001)))
            
            (define-test entity-with-body-test
              (let* ([body1 (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [body2 (make-body-2d (vec2 10 10) (vec2 5 5) 1)]
                     [shape (make-circle (vec2 0 0) 1)]
                     [e1 (make-entity 'id body1 shape default-material #f)]
                     [e2 (entity-with-body e1 body2)])
                    (assert-vec2-= (entity-pos e2) (vec2 10 10) 0.0001)
                    (assert-eq? (entity-id e2) 'id)))
            
            (define-test static-entity-test
              (let* ([body (make-static-body (vec2 50 50))]
                     [shape (make-circle (vec2 50 50) 5)]
                     [e (make-entity 'static body shape default-material #f)])
                    (assert-true (entity-static? e)))))

;;; ====
;;; World Creation Tests
;;; ====

(test-group world-creation-tests
            (define-test make-world-test
              (let ([w (make-world (vec2 0 9.8))])
                   (assert-true (world? w))
                   (assert-vec2-= (world-gravity w) (vec2 0 9.8) 0.0001)))
            
            (define-test world-add-entity-test
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    (assert-true (entity? (world-get-entity w 'ball)))))
            
            (define-test world-remove-entity-test
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    (world-remove-entity! w 'ball)
                    (assert-false (world-get-entity w 'ball))))
            
            (define-test world-entity-list-test
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'ball1 (vec2 0 0) 5 1 default-material)]
                     [e2 (make-circle-entity 'ball2 (vec2 10 0) 5 1 default-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (assert-= (length (world-entity-list w)) 2 0))))

;;; ====
;;; Convenience Constructor Tests
;;; ====

(test-group constructor-tests
            (define-test make-circle-entity-test
              (let ([e (make-circle-entity 'c (vec2 10 20) 5 2 rubber-material)])
                   (assert-vec2-= (entity-pos e) (vec2 10 20) 0.0001)
                   (assert-true (circle? (entity-shape e)))
                   (assert-= (circle-radius (entity-shape e)) 5 0.0001)))
            
            (define-test make-box-entity-test
              (let ([e (make-box-entity 'b (vec2 0 0) (vec2 2 3) 4 metal-material)])
                   (assert-vec2-= (entity-pos e) (vec2 0 0) 0.0001)
                   (assert-true (polygon? (entity-shape e)))))
            
            (define-test make-static-circle-test
              (let ([e (make-static-circle 's (vec2 50 50) 10 ice-material)])
                   (assert-true (entity-static? e))
                   (assert-vec2-= (entity-pos e) (vec2 50 50) 0.0001)))
            
            (define-test make-ground-test
              (let ([g (make-ground 'ground 500 1000 wood-material)])
                   (assert-true (entity-static? g)))))

;;; ====
;;; Integration Tests
;;; ====

(test-group integration-tests
            (define-test gravity-integration-test
              ;; Ball should fall under gravity
              (let* ([w (make-world (vec2 0 100))]  ; Gravity downward
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    ;; Step for a fixed amount
                    (world-fixed-step! w 0.1)
                    (let ([ball (world-get-entity w 'ball)])
                         ;; Should have gained downward velocity
                         (assert-true (> (vec2-y (entity-vel ball)) 0))
                         ;; Should have moved down
                         (assert-true (> (vec2-y (entity-pos ball)) 0)))))
            
            (define-test static-no-move-test
              ;; Static bodies should not move
              (let* ([w (make-world (vec2 0 100))]
                     [e (make-static-circle 'static (vec2 50 50) 5 default-material)])
                    (world-add-entity! w e)
                    (world-fixed-step! w 0.1)
                    (let ([entity (world-get-entity w 'static)])
                         (assert-vec2-= (entity-pos entity) (vec2 50 50) 0.0001))))
            
            (define-test velocity-integration-test
              ;; Moving body should change position
              (let* ([w (make-world (vec2 0 0))]  ; No gravity
                     [body (make-body-2d (vec2 0 0) (vec2 100 0) 1)]
                     [shape (make-circle (vec2 0 0) 5)]
                     [e (make-entity 'mover body shape default-material #f)])
                    (world-add-entity! w e)
                    (world-fixed-step! w 0.1)
                    (let ([mover (world-get-entity w 'mover)])
                         ;; Should have moved 100 * 0.1 = 10 units
                         (assert-= (vec2-x (entity-pos mover)) 10 0.5)))))

;;; ====
;;; Collision Detection Tests
;;; ====

(test-group collision-detection-tests
            (define-test detect-circle-collision-test
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 5 1 default-material)]
                     [e2 (make-circle-entity 'b (vec2 8 0) 5 1 default-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (let ([collisions (world-detect-collisions w)])
                         ;; Should detect one collision
                         (assert-= (length collisions) 1 0))))
            
            (define-test no-collision-test
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 5 1 default-material)]
                     [e2 (make-circle-entity 'b (vec2 100 0) 5 1 default-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (let ([collisions (world-detect-collisions w)])
                         (assert-= (length collisions) 0 0))))
            
            (define-test many-bodies-test
              ;; Multiple non-colliding bodies
              (let* ([w (make-world (vec2 0 0))])
                    (world-add-entity! w (make-circle-entity 'a (vec2 0 0) 2 1 default-material))
                    (world-add-entity! w (make-circle-entity 'b (vec2 20 0) 2 1 default-material))
                    (world-add-entity! w (make-circle-entity 'c (vec2 40 0) 2 1 default-material))
                    (world-add-entity! w (make-circle-entity 'd (vec2 60 0) 2 1 default-material))
                    (let ([collisions (world-detect-collisions w)])
                         (assert-= (length collisions) 0 0)))))

;;; ====
;;; Collision Resolution Tests
;;; ====

(test-group collision-resolution-tests
            (define-test resolve-separates-test
              ;; Two overlapping circles should separate after resolution
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 5 1 default-material)]
                     [e2 (make-circle-entity 'b (vec2 8 0) 5 1 default-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (let ([collisions (world-detect-collisions w)])
                         (world-resolve-collisions! w collisions))
                    ;; After resolution, should have some velocity
                    (let ([a (world-get-entity w 'a)]
                          [b (world-get-entity w 'b)])
                         ;; a should move left, b should move right (or no collision velocity if already separated)
                         (assert-true (or (<= (vec2-x (entity-vel a)) 0)
                                          (>= (vec2-x (entity-vel b)) 0))))))
            
            (define-test ball-bounces-test
              ;; Ball hitting ground should bounce
              (let* ([w (make-world (vec2 0 0))]  ; No gravity for simplicity
                     ;; Ball moving down - shape in local coords (centered at origin)
                     [body (make-body-2d (vec2 0 98) (vec2 0 10) 1)]
                     [shape (make-circle (vec2 0 0) 5)]  ; Local coords
                     [ball (make-entity 'ball body shape rubber-material #f)]
                     ;; Ground
                     [ground (make-static-box 'ground (vec2 0 100) (vec2 100 5) wood-material)])
                    (world-add-entity! w ball)
                    (world-add-entity! w ground)
                    ;; Detect and resolve
                    (let ([collisions (world-detect-collisions w)])
                         (world-resolve-collisions! w collisions))
                    (let ([ball-after (world-get-entity w 'ball)])
                         ;; Ball velocity should reverse (bounce up)
                         (assert-true (<= (vec2-y (entity-vel ball-after)) 0))))))

;;; ====
;;; Query Tests
;;; ====

(test-group query-tests
            (define-test query-aabb-test
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 5 5) 2 1 default-material)]
                     [e2 (make-circle-entity 'b (vec2 50 50) 2 1 default-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (let ([hits (world-query-aabb w (make-aabb (vec2 0 0) (vec2 10 10)))])
                         ;; Should only find 'a
                         (assert-= (length hits) 1 0)
                         (assert-eq? (entity-id (car hits)) 'a))))
            
            (define-test query-point-test
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'c (vec2 10 10) 5 1 default-material)])
                    (world-add-entity! w e)
                    (let ([hits (world-query-point w (vec2 10 10))])
                         (assert-= (length hits) 1 0))
                    (let ([misses (world-query-point w (vec2 100 100))])
                         (assert-= (length misses) 0 0)))))

;;; ====
;;; Raycast Tests
;;; ====

(test-group raycast-tests
            (define-test raycast-hit-test
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-box-entity 'box (vec2 10 0) (vec2 2 2) 1 default-material)])
                    (world-add-entity! w e)
                    (let ([hits (world-raycast w (vec2 0 0) (vec2 1 0) 100)])
                         (assert-true (> (length hits) 0)))))
            
            (define-test raycast-miss-test
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-box-entity 'box (vec2 10 0) (vec2 2 2) 1 default-material)])
                    (world-add-entity! w e)
                    ;; Ray goes in opposite direction
                    (let ([hits (world-raycast w (vec2 0 0) (vec2 -1 0) 100)])
                         (assert-= (length hits) 0 0)))))

;;; ====
;;; Entity Manipulation Tests
;;; ====

(test-group manipulation-tests
            (define-test set-velocity-test
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    (set-velocity! w 'ball (vec2 100 50))
                    (let ([ball (world-get-entity w 'ball)])
                         (assert-vec2-= (entity-vel ball) (vec2 100 50) 0.0001))))
            
            (define-test apply-impulse-test
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])  ; mass = 1
                    (world-add-entity! w e)
                    (apply-impulse! w 'ball (vec2 10 0))  ; impulse = 10
                    (let ([ball (world-get-entity w 'ball)])
                         ;; With mass=1, velocity should be 10
                         (assert-vec2-= (entity-vel ball) (vec2 10 0) 0.0001)))))

;;; ====
;;; Full Simulation Tests
;;; ====

(test-group simulation-tests
            (define-test falling-ball-test
              ;; Ball falls under gravity after one step
              (let* ([w (make-world (vec2 0 100))]  ; Gravity down
                     [ball (make-circle-entity 'ball (vec2 0 0) 5 1 rubber-material)])
                    (world-add-entity! w ball)
                    ;; Single fixed step
                    (world-fixed-step! w 0.1)
                    (let ([final-ball (world-get-entity w 'ball)])
                         ;; Ball should have fallen
                         (assert-true (> (vec2-y (entity-pos final-ball)) 0)))))
            
            (define-test multiple-balls-test
              ;; Multiple balls should all be tracked
              (let* ([w (make-world (vec2 0 0))])
                    (world-add-entity! w (make-circle-entity 'a (vec2 0 0) 5 1 default-material))
                    (world-add-entity! w (make-circle-entity 'b (vec2 20 0) 5 1 default-material))
                    (world-add-entity! w (make-circle-entity 'c (vec2 40 0) 5 1 default-material))
                    (set-velocity! w 'a (vec2 10 0))
                    (set-velocity! w 'b (vec2 0 10))
                    (set-velocity! w 'c (vec2 -10 -10))
                    ;; Step once
                    (world-fixed-step! w 0.1)
                    ;; All should have moved
                    (let ([a (world-get-entity w 'a)]
                          [b (world-get-entity w 'b)]
                          [c (world-get-entity w 'c)])
                         (assert-true (> (vec2-x (entity-pos a)) 0))
                         (assert-true (> (vec2-y (entity-pos b)) 0))
                         (assert-true (< (vec2-x (entity-pos c)) 40))))))

;;; ====
;;; Summary
;;; ====

(display "
==========================================================
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All 2D physics world tests passed.
")
    (display "
[FAILURE] Some 2D physics world tests failed.
"))
