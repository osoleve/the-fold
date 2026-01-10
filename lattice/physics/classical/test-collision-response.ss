;;; fabric/stitches/physics-2d/test-collision-response.ss — Tests for Collision Response

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "lattice/physics/classical/collision-response.ss")

;;; Helper for numeric comparison
(define (assert-= actual expected tolerance)
  (unless (< (abs (- actual expected)) tolerance)
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

(display "
══════════════════════════════════════════════════════════
         COLLISION RESPONSE TESTS
══════════════════════════════════════════════════════════
")

;;; ============================================================
;;; Manifold Tests
;;; ============================================================

(test-group manifold-tests
            (define-test make-manifold-test
              (let* ([a (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 0 0) 1)]
                     [m (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))])
                    (assert-true (manifold? m))
                    (assert-true (eq? (manifold-body-a m) a))
                    (assert-vec2-= (manifold-normal m) (vec2 1 0) 0.0001)
                    (assert-= (manifold-penetration m) 0.1 0.0001)))
            
            (define-test manifold-accessors-test
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 2 0) (vec2 -1 0) 1)]
                     [m (make-manifold a b (vec2 1 0) 0.5 (vec2 1 0))])
                    (assert-vec2-= (manifold-contact m) (vec2 1 0) 0.0001)
                    (assert-= (manifold-penetration m) 0.5 0.0001))))

;;; ============================================================
;;; Material Tests
;;; ============================================================

(test-group material-tests
            (define-test make-material-test
              (let ([m (make-material 0.5 0.4 0.3)])
                   (assert-true (material? m))
                   (assert-= (material-restitution m) 0.5 0.0001)
                   (assert-= (material-static-friction m) 0.4 0.0001)
                   (assert-= (material-dynamic-friction m) 0.3 0.0001)))
            
            (define-test default-materials-test
              (assert-= (material-restitution rubber-material) 0.8 0.0001)
              (assert-= (material-restitution metal-material) 0.3 0.0001)
              (assert-= (material-restitution ice-material) 0.1 0.0001))
            
            (define-test combine-materials-test
              (let* ([rubber rubber-material]
                     [ice ice-material]
                     [combined (combine-materials rubber ice)])
                    ;; Restitution uses min
                    (assert-= (car combined) 0.1 0.0001)
                    ;; Friction uses geometric mean
                    (assert-= (cadr combined) (sqrt (* 0.9 0.02)) 0.0001))))

;;; ============================================================
;;; Relative Velocity Tests
;;; ============================================================

(test-group velocity-tests
            (define-test relative-velocity-test
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [rel (relative-velocity a b)])
                    ;; B relative to A: (-1) - (1) = -2
                    (assert-vec2-= rel (vec2 -2 0) 0.0001)))
            
            (define-test normal-velocity-test
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [normal (vec2 1 0)]
                     [vn (normal-velocity a b normal)])
                    ;; Approaching at 2 units/s
                    (assert-= vn -2 0.0001)))
            
            (define-test normal-velocity-separating-test
              (let* ([a (make-body-2d (vec2 0 0) (vec2 -1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 1 0) 1)]
                     [normal (vec2 1 0)]
                     [vn (normal-velocity a b normal)])
                    ;; Separating at 2 units/s
                    (assert-= vn 2 0.0001))))

;;; ============================================================
;;; Impulse Calculation Tests
;;; ============================================================

(test-group impulse-tests
            (define-test impulse-elastic-test
              ;; Two identical balls colliding head-on (perfectly elastic)
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [normal (vec2 1 0)]
                     [j (calculate-impulse a b normal 1.0)])
                    ;; j = -(1+e) * vn / (1/m1 + 1/m2)
                    ;; j = -(1+1) * (-2) / (1 + 1) = 4/2 = 2
                    (assert-= j 2 0.0001)))
            
            (define-test impulse-inelastic-test
              ;; Perfectly inelastic collision
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [normal (vec2 1 0)]
                     [j (calculate-impulse a b normal 0.0)])
                    ;; j = -(1+0) * (-2) / 2 = 1
                    (assert-= j 1 0.0001)))
            
            (define-test impulse-separating-test
              ;; Objects moving apart - no impulse
              (let* ([a (make-body-2d (vec2 0 0) (vec2 -1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 1 0) 1)]
                     [normal (vec2 1 0)]
                     [j (calculate-impulse a b normal 1.0)])
                    (assert-= j 0 0.0001)))
            
            (define-test impulse-different-masses-test
              ;; Heavy object hitting light object
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 10)]  ; Heavy
                     [b (make-body-2d (vec2 1 0) (vec2 0 0) 1)]   ; Light
                     [normal (vec2 1 0)]
                     [j (calculate-impulse a b normal 1.0)])
                    ;; j = -(1+1) * (-1) / (0.1 + 1) = 2/1.1 ≈ 1.818
                    (assert-= j (/ 2 1.1) 0.01))))

;;; ============================================================
;;; Impulse Application Tests
;;; ============================================================

(test-group apply-impulse-tests
            (define-test apply-impulse-test
              (let* ([body (make-body-2d (vec2 0 0) (vec2 0 0) 2)]
                     [impulse (vec2 4 0)]
                     [new-body (apply-impulse body impulse)])
                    ;; v = v + J/m = 0 + 4/2 = 2
                    (assert-vec2-= (body-vel new-body) (vec2 2 0) 0.0001)))
            
            (define-test apply-impulse-static-test
              (let* ([body (make-static-body (vec2 50 50))]
                     [impulse (vec2 100 100)]
                     [new-body (apply-impulse body impulse)])
                    ;; Static body shouldn't move
                    (assert-vec2-= (body-vel new-body) (vec2 0 0) 0.0001))))

;;; ============================================================
;;; Collision Resolution Tests
;;; ============================================================

(test-group resolve-collision-tests
            (define-test resolve-elastic-head-on
              ;; Two identical balls, elastic collision
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [elastic (make-material 1.0 0 0)]
                     [result (resolve-collision manifold elastic elastic)]
                     [new-a (car result)]
                     [new-b (cadr result)])
                    ;; Should exchange velocities
                    (assert-vec2-= (body-vel new-a) (vec2 -1 0) 0.0001)
                    (assert-vec2-= (body-vel new-b) (vec2 1 0) 0.0001)))
            
            (define-test resolve-inelastic-head-on
              ;; Two identical balls, perfectly inelastic
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [inelastic (make-material 0.0 0 0)]
                     [result (resolve-collision manifold inelastic inelastic)]
                     [new-a (car result)]
                     [new-b (cadr result)])
                    ;; Should have same velocity (0)
                    (assert-vec2-= (body-vel new-a) (vec2 0 0) 0.0001)
                    (assert-vec2-= (body-vel new-b) (vec2 0 0) 0.0001)))
            
            (define-test resolve-with-static-body
              ;; Ball hitting static wall
              (let* ([ball (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [wall (make-static-body (vec2 1 0))]
                     [manifold (make-manifold ball wall (vec2 1 0) 0.05 (vec2 0.95 0))]
                     [elastic (make-material 1.0 0 0)]
                     [result (resolve-collision manifold elastic elastic)]
                     [new-ball (car result)])
                    ;; Ball should bounce back
                    (assert-vec2-= (body-vel new-ball) (vec2 -1 0) 0.0001))))

;;; ============================================================
;;; Position Correction Tests
;;; ============================================================

(test-group position-correction-tests
            (define-test correct-positions-test
              (let* ([a (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [b (make-body-2d (vec2 0.9 0) (vec2 0 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.45 0))]
                     [result (correct-positions manifold)]
                     [new-a (car result)]
                     [new-b (cadr result)])
                    ;; Bodies should move apart
                    (assert-true (< (vec2-x (body-pos new-a))
                                    (vec2-x (body-pos a))))
                    (assert-true (> (vec2-x (body-pos new-b))
                                    (vec2-x (body-pos b))))))
            
            (define-test correct-positions-static
              ;; Only dynamic body should move when correcting against static
              (let* ([ball (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [wall (make-static-body (vec2 0.5 0))]
                     [manifold (make-manifold ball wall (vec2 1 0) 0.1 (vec2 0.45 0))]
                     [result (correct-positions manifold)]
                     [new-ball (car result)]
                     [new-wall (cadr result)])
                    ;; Ball should move, wall shouldn't
                    (assert-true (< (vec2-x (body-pos new-ball)) 0))
                    (assert-vec2-= (body-pos new-wall) (vec2 0.5 0) 0.0001))))

;;; ============================================================
;;; Circle-Circle Collision Tests
;;; ============================================================

(test-group circle-circle-tests
            (define-test circle-circle-collision
              (let* ([a (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [b (make-body-2d (vec2 1.5 0) (vec2 0 0) 1)]
                     [m (body-circle-circle-manifold a 1 b 1)])
                    (assert-true (manifold? m))
                    (assert-= (manifold-penetration m) 0.5 0.0001)
                    (assert-vec2-= (manifold-normal m) (vec2 1 0) 0.0001)))
            
            (define-test circle-circle-no-collision
              (let* ([a (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [b (make-body-2d (vec2 3 0) (vec2 0 0) 1)]
                     [m (body-circle-circle-manifold a 1 b 1)])
                    (assert-false m)))
            
            (define-test circle-circle-touching
              (let* ([a (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [b (make-body-2d (vec2 2 0) (vec2 0 0) 1)]
                     [m (body-circle-circle-manifold a 1 b 1)])
                    ;; Exactly touching = no penetration
                    (if m
                        (assert-= (manifold-penetration m) 0 0.0001)
                        (assert-true #t)))))  ; No manifold is also valid

;;; ============================================================
;;; Full Resolution Tests
;;; ============================================================

(test-group full-resolution-tests
            (define-test resolve-with-correction-test
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 0.9 0) (vec2 -1 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.45 0))]
                     [mat (make-material 1.0 0 0)]
                     [result (resolve-with-correction manifold mat mat)]
                     [new-a (car result)]
                     [new-b (cadr result)])
                    ;; Velocities should be resolved
                    (assert-= (vec2-x (body-vel new-a)) -1 0.0001)
                    (assert-= (vec2-x (body-vel new-b)) 1 0.0001)
                    ;; Positions should be corrected
                    (let ([dist (vec2-magnitude (vec2-sub (body-pos new-b) (body-pos new-a)))])
                         (assert-true (>= dist 0.9)))))  ; Should be at least as far as before
            
            (define-test momentum-conservation
              ;; Check momentum is conserved in collision
              (let* ([a (make-body-2d (vec2 0 0) (vec2 2 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 2)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [mat (make-material 0.5 0 0)]
                     [result (resolve-collision manifold mat mat)]
                     [new-a (car result)]
                     [new-b (cadr result)]
                     ;; Initial momentum
                     [p0 (+ (* 1 2) (* 2 -1))]  ; 2 - 2 = 0
                     ;; Final momentum
                     [p1 (+ (* 1 (vec2-x (body-vel new-a)))
                            (* 2 (vec2-x (body-vel new-b))))])
                    (assert-= p1 p0 0.0001))))

;;; ============================================================
;;; Energy Tests
;;; ============================================================

(test-group energy-tests
            (define-test elastic-energy-conservation
              ;; Elastic collision should conserve kinetic energy
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [mat (make-material 1.0 0 0)]
                     [result (resolve-collision manifold mat mat)]
                     [new-a (car result)]
                     [new-b (cadr result)]
                     ;; Initial KE
                     [ke0 (+ (body-kinetic-energy a) (body-kinetic-energy b))]
                     ;; Final KE
                     [ke1 (+ (body-kinetic-energy new-a) (body-kinetic-energy new-b))])
                    (assert-= ke1 ke0 0.0001)))
            
            (define-test inelastic-energy-loss
              ;; Inelastic collision should lose energy
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [mat (make-material 0.0 0 0)]  ; Perfectly inelastic
                     [result (resolve-collision manifold mat mat)]
                     [new-a (car result)]
                     [new-b (cadr result)]
                     [ke0 (+ (body-kinetic-energy a) (body-kinetic-energy b))]
                     [ke1 (+ (body-kinetic-energy new-a) (body-kinetic-energy new-b))])
                    (assert-true (< ke1 ke0)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
══════════════════════════════════════════════════════════
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All collision response tests passed.
")
    (display "
[FAILURE] Some collision response tests failed.
"))
