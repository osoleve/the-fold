;;; playpen/physics/test-physics.ss — Comprehensive Physics Engine Test Suite
;;;
;;; This test suite provides comprehensive coverage of the physics engine,
;;; including vector operations, integration methods, collision detection,
;;; collision response, and world simulation.
;;;
;;; Run from fabric/stitches directory:
;;;   scheme --script ../../playpen/physics/test-physics.ss
;;;
;;; Or from project root (after adding source directory):
;;;   (source-directories (cons "core" (source-directories)))
(load "core/lang/module.ss")
;;;   (load "lattice/physics/classical/test-physics.ss")

;;; Load dependencies using absolute paths from project root
(load "core/test-framework.ss")
(load "lattice/physics/classical/world.ss")

;;; ====
;;; Test Helpers
;;; ====

;;; assert-= : Number x Number x Number -> Void
;;; Check that actual is within tolerance of expected.
(define (assert-= actual expected tolerance)
  (unless (<= (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    X ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (display " +/- ")
          (display tolerance)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; assert-vec2-= : Vec2 x Vec2 x Number -> Void
;;; Check that two vectors are within tolerance of each other.
(define (assert-vec2-= actual expected tolerance)
  (assert-= (vec2-x actual) (vec2-x expected) tolerance)
  (assert-= (vec2-y actual) (vec2-y expected) tolerance))

;;; assert-eq? : Any x Any -> Void
(define (assert-eq? actual expected)
  (unless (eq? actual expected)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    X ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; Constants
(define pi 3.14159265358979)
(define epsilon 1e-10)

(display "
====
           COMPREHENSIVE PHYSICS ENGINE TEST SUITE
====
")

;;; ====
;;; SECTION 1: VECTOR OPERATIONS
;;; ====

(display "
----
                    VECTOR OPERATIONS
----
")

(test-group vec2-construction-tests
            (define-test vec2-creates-basic-vector
              (let ([v (vec2 3 4)])
                   (assert-true (vec2? v))
                   (assert-equal 3 (vec2-x v))
                   (assert-equal 4 (vec2-y v))))
            
            (define-test vec2-zero-is-origin
              (let ([v (vec2-zero)])
                   (assert-true (vec2-zero? v))
                   (assert-= (vec2-x v) 0 epsilon)
                   (assert-= (vec2-y v) 0 epsilon)))
            
            (define-test vec2-one-creates-unit-diagonal
              (let ([v (vec2-one)])
                   (assert-= (vec2-x v) 1 epsilon)
                   (assert-= (vec2-y v) 1 epsilon)))
            
            (define-test vec2-unit-vectors-correct
              (assert-vec2-= (vec2-unit-x) (vec2 1 0) epsilon)
              (assert-vec2-= (vec2-unit-y) (vec2 0 1) epsilon))
            
            (define-test vec2-from-angle-zero-is-unit-x
              (assert-vec2-= (vec2-from-angle 0) (vec2 1 0) 0.0001))
            
            (define-test vec2-from-angle-pi-half-is-unit-y
              (assert-vec2-= (vec2-from-angle (/ pi 2)) (vec2 0 1) 0.0001))
            
            (define-test vec2-from-polar-basic
              (let ([v (vec2-from-polar 2 (/ pi 4))])
                   (assert-= (vec2-x v) (sqrt 2) 0.0001)
                   (assert-= (vec2-y v) (sqrt 2) 0.0001))))

(test-group vec2-arithmetic-tests
            (define-test vec2-add-correct
              (assert-vec2-= (vec2-add (vec2 1 2) (vec2 3 4)) (vec2 4 6) epsilon))
            
            (define-test vec2-add-with-zero-identity
              (assert-vec2-= (vec2-add (vec2 5 7) (vec2-zero)) (vec2 5 7) epsilon))
            
            (define-test vec2-sub-correct
              (assert-vec2-= (vec2-sub (vec2 5 7) (vec2 2 3)) (vec2 3 4) epsilon))
            
            (define-test vec2-sub-self-is-zero
              (let ([v (vec2 3 4)])
                   (assert-true (vec2-zero? (vec2-sub v v)))))
            
            (define-test vec2-neg-correct
              (assert-vec2-= (vec2-neg (vec2 3 -4)) (vec2 -3 4) epsilon))
            
            (define-test vec2-neg-double-identity
              (let ([v (vec2 3 -4)])
                   (assert-vec2-= (vec2-neg (vec2-neg v)) v epsilon)))
            
            (define-test vec2-scale-correct
              (assert-vec2-= (vec2-scale (vec2 3 4) 2) (vec2 6 8) epsilon))
            
            (define-test vec2-scale-by-zero-is-zero
              (assert-true (vec2-zero? (vec2-scale (vec2 100 200) 0))))
            
            (define-test vec2-scale-by-negative
              (assert-vec2-= (vec2-scale (vec2 3 4) -1) (vec2 -3 -4) epsilon)))

(test-group vec2-products-tests
            (define-test vec2-dot-correct
              (assert-= (vec2-dot (vec2 1 2) (vec2 3 4)) 11 epsilon))
            
            (define-test vec2-dot-perpendicular-is-zero
              (assert-= (vec2-dot (vec2-unit-x) (vec2-unit-y)) 0 epsilon))
            
            (define-test vec2-dot-self-is-magnitude-squared
              (let ([v (vec2 3 4)])
                   (assert-= (vec2-dot v v) (vec2-magnitude-sq v) epsilon)))
            
            (define-test vec2-cross-correct
              (assert-= (vec2-cross (vec2 1 2) (vec2 3 4)) -2 epsilon))
            
            (define-test vec2-cross-parallel-is-zero
              (assert-= (vec2-cross (vec2 2 4) (vec2 1 2)) 0 epsilon))
            
            (define-test vec2-cross-perpendicular
              ;; Cross product of unit-x with unit-y = 1*1 - 0*0 = 1
              (assert-= (vec2-cross (vec2-unit-x) (vec2-unit-y)) 1 epsilon)))

(test-group vec2-magnitude-tests
            (define-test vec2-magnitude-3-4-5-triangle
              (assert-= (vec2-magnitude (vec2 3 4)) 5 epsilon))
            
            (define-test vec2-magnitude-sq-correct
              (assert-= (vec2-magnitude-sq (vec2 3 4)) 25 epsilon))
            
            (define-test vec2-magnitude-zero-vector
              (assert-= (vec2-magnitude (vec2-zero)) 0 epsilon))
            
            (define-test vec2-distance-correct
              (assert-= (vec2-distance (vec2 0 0) (vec2 3 4)) 5 epsilon))
            
            (define-test vec2-distance-sq-correct
              (assert-= (vec2-distance-sq (vec2 0 0) (vec2 3 4)) 25 epsilon)))

(test-group vec2-normalize-tests
            (define-test vec2-normalize-correct
              (let ([result (vec2-normalize (vec2 3 4))])
                   (assert-= (vec2-x result) 0.6 0.0001)
                   (assert-= (vec2-y result) 0.8 0.0001)
                   (assert-= (vec2-magnitude result) 1 0.0001)))
            
            (define-test vec2-normalize-zero-returns-zero
              (assert-true (vec2-zero? (vec2-normalize (vec2-zero)))))
            
            (define-test vec2-normalize-tiny-vector
              ;; Very small vectors should also normalize to zero
              (assert-true (vec2-zero? (vec2-normalize (vec2 1e-15 1e-15)))))
            
            (define-test vec2-set-magnitude-correct
              (let ([result (vec2-set-magnitude (vec2 3 4) 10)])
                   (assert-= (vec2-magnitude result) 10 0.0001)))
            
            (define-test vec2-limit-under-limit
              (let ([v (vec2 3 4)])  ; magnitude 5
                   (assert-= (vec2-magnitude (vec2-limit v 10)) 5 epsilon)))
            
            (define-test vec2-limit-over-limit
              (let ([result (vec2-limit (vec2 6 8) 5)])  ; magnitude 10 -> 5
                   (assert-= (vec2-magnitude result) 5 0.0001))))

(test-group vec2-rotation-tests
            (define-test vec2-rotate-90-degrees
              (let ([result (vec2-rotate (vec2 1 0) (/ pi 2))])
                   (assert-= (vec2-x result) 0 0.0001)
                   (assert-= (vec2-y result) 1 0.0001)))
            
            (define-test vec2-rotate-180-degrees
              (let ([result (vec2-rotate (vec2 1 0) pi)])
                   (assert-= (vec2-x result) -1 0.0001)
                   (assert-= (vec2-y result) 0 0.0001)))
            
            (define-test vec2-rotate-90-ccw
              (assert-vec2-= (vec2-rotate-90 (vec2 1 0)) (vec2 0 1) 0.0001))
            
            (define-test vec2-rotate-90-cw
              (assert-vec2-= (vec2-rotate-neg-90 (vec2 1 0)) (vec2 0 -1) 0.0001)))

(test-group vec2-edge-cases
            (define-test vec2-very-large-values
              (let ([big (vec2 1e10 1e10)])
                   (assert-= (vec2-magnitude big) (* (sqrt 2) 1e10) 1e5)))
            
            (define-test vec2-very-small-values
              (let ([tiny (vec2 1e-10 1e-10)])
                   (assert-= (vec2-magnitude tiny) (* (sqrt 2) 1e-10) 1e-15)))
            
            (define-test vec2-negative-components
              (let ([v (vec2 -3 -4)])
                   (assert-= (vec2-magnitude v) 5 epsilon))))

;;; ====
;;; SECTION 2: PHYSICS PRIMITIVES (BODY)
;;; ====

(display "
----
                    PHYSICS PRIMITIVES
----
")

(test-group body-creation-tests
            (define-test make-body-2d-basic
              (let ([b (make-body-2d (vec2 10 20) (vec2 1 2) 5)])
                   (assert-true (body-2d? b))
                   (assert-vec2-= (body-pos b) (vec2 10 20) epsilon)
                   (assert-vec2-= (body-vel b) (vec2 1 2) epsilon)
                   (assert-= (body-mass b) 5 epsilon)
                   (assert-= (body-inv-mass b) 0.2 epsilon)))
            
            (define-test make-static-body-correct
              (let ([b (make-static-body (vec2 50 50))])
                   (assert-true (body-static? b))
                   (assert-= (body-inv-mass b) 0 epsilon)
                   (assert-= (body-mass b) 0 epsilon)))
            
            (define-test body-with-pos-updates-position
              (let* ([b1 (make-body-2d (vec2 0 0) (vec2 1 1) 1)]
                     [b2 (body-with-pos b1 (vec2 10 20))])
                    (assert-vec2-= (body-pos b2) (vec2 10 20) epsilon)
                    (assert-vec2-= (body-vel b2) (vec2 1 1) epsilon)))
            
            (define-test body-with-vel-updates-velocity
              (let* ([b1 (make-body-2d (vec2 5 5) (vec2 0 0) 1)]
                     [b2 (body-with-vel b1 (vec2 3 4))])
                    (assert-vec2-= (body-pos b2) (vec2 5 5) epsilon)
                    (assert-vec2-= (body-vel b2) (vec2 3 4) epsilon))))

;;; ====
;;; SECTION 3: INTEGRATION METHODS
;;; ====

(display "
----
                    INTEGRATION METHODS
----
")

(test-group euler-integration-tests
            (define-test euler-no-force-constant-velocity
              (let* ([b (make-body-2d (vec2 0 0) (vec2 10 5) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [b2 (integrate-body-euler b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 10 5) 0.0001)
                    (assert-vec2-= (body-vel b2) (vec2 10 5) 0.0001)))
            
            (define-test euler-constant-force-acceleration
              (let* ([b (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [f (lambda (body) (vec2 10 0))]
                     [b2 (integrate-body-euler b f 1)])
                    (assert-vec2-= (body-vel b2) (vec2 10 0) 0.0001)
                    (assert-vec2-= (body-pos b2) (vec2 10 0) 0.0001)))
            
            (define-test euler-static-body-no-movement
              (let* ([b (make-static-body (vec2 50 50))]
                     [f (lambda (body) (vec2 1000 1000))]
                     [b2 (integrate-body-euler b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 50 50) epsilon))))

(test-group symplectic-integration-tests
            (define-test symplectic-no-force
              (let* ([b (make-body-2d (vec2 0 0) (vec2 5 3) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [b2 (integrate-body-symplectic b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 5 3) 0.0001)))
            
            (define-test symplectic-with-gravity
              (let* ([b (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [f (gravity-force 10)]
                     [b2 (integrate-body-symplectic b f 1)])
                    (assert-= (vec2-y (body-vel b2)) 10 0.0001)
                    (assert-= (vec2-y (body-pos b2)) 10 0.0001))))

(test-group verlet-integration-tests
            (define-test verlet-free-particle
              (let* ([b (make-body-2d (vec2 0 0) (vec2 10 0) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [b2 (integrate-body-verlet b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 10 0) 0.0001)
                    (assert-vec2-= (body-vel b2) (vec2 10 0) 0.0001)))
            
            (define-test verlet-constant-acceleration
              ;; a = 10, x(0) = 0, v(0) = 0, dt = 1
              ;; x = 0 + 0 + 0.5*10*1 = 5
              ;; v = 0 + 0.5*(10+10)*1 = 10
              (let* ([b (make-body-2d (vec2 0 0) (vec2 0 0) 1)]
                     [f (lambda (body) (vec2 10 0))]
                     [b2 (integrate-body-verlet b f 1)])
                    (assert-vec2-= (body-pos b2) (vec2 5 0) 0.0001)
                    (assert-vec2-= (body-vel b2) (vec2 10 0) 0.0001)))
            
            (define-test verlet-harmonic-oscillator-single-step
              ;; Test with simple harmonic oscillator: F = -kx
              (let* ([k 1]
                     [b (make-body-2d (vec2 1 0) (vec2 0 0) 1)]
                     [f (lambda (body)
                                (let ([x (vec2-x (body-pos body))])
                                     (vec2 (- (* k x)) 0)))]
                     [b2 (integrate-body-verlet b f 0.1)])
                    ;; Should be close to cos(0.1), -sin(0.1)
                    (assert-= (vec2-x (body-pos b2)) (cos 0.1) 0.01)
                    (assert-= (vec2-x (body-vel b2)) (- (sin 0.1)) 0.01))))

(test-group energy-conservation-tests
            (define-test kinetic-energy-calculation
              (let ([b (make-body-2d (vec2 0 0) (vec2 3 4) 2)])
                   ;; KE = 0.5 * 2 * (9+16) = 25
                   (assert-= (body-kinetic-energy b) 25 epsilon)))
            
            (define-test spring-energy-conservation
              ;; Harmonic oscillator should conserve total energy
              (let* ([k 1]
                     [b0 (make-body-2d (vec2 1 0) (vec2 0 0) 1)]
                     [f (lambda (body)
                                (let ([x (vec2-x (body-pos body))])
                                     (vec2 (- (* k x)) 0)))]
                     [dt 0.01]
                     ;; Integrate 1000 steps
                     [final-b (let loop ([b b0] [i 0])
                                   (if (>= i 1000)
                                       b
                                       (loop (integrate-body-verlet b f dt) (+ i 1))))]
                     ;; Initial spring energy = 0.5*k*x^2 = 0.5
                     [e0 (body-spring-energy b0 (vec2 0 0) k)]
                     ;; Final total energy
                     [ef (+ (body-kinetic-energy final-b)
                            (body-spring-energy final-b (vec2 0 0) k))])
                    (assert-= e0 0.5 epsilon)
                    (assert-= ef 0.5 0.01))))

;;; ====
;;; SECTION 4: COLLISION DETECTION
;;; ====

(display "
----
                    COLLISION DETECTION
----
")

(test-group aabb-collision-tests
            (define-test aabb-construction
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-true (aabb? a))
                   (assert-vec2-= (aabb-min a) (vec2 0 0) epsilon)
                   (assert-vec2-= (aabb-max a) (vec2 10 10) epsilon)))
            
            (define-test aabb-center-calculation
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-vec2-= (aabb-center a) (vec2 5 5) epsilon)))
            
            (define-test aabb-half-extents-calculation
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-vec2-= (aabb-half-extents a) (vec2 5 5) epsilon)))
            
            (define-test aabb-aabb-overlapping
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))]
                    [b (make-aabb (vec2 5 5) (vec2 15 15))])
                   (assert-true (aabb-aabb? a b))))
            
            (define-test aabb-aabb-separated
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))]
                    [b (make-aabb (vec2 20 20) (vec2 30 30))])
                   (assert-false (aabb-aabb? a b))))
            
            (define-test aabb-aabb-touching
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))]
                    [b (make-aabb (vec2 10 0) (vec2 20 10))])
                   (assert-true (aabb-aabb? a b))))
            
            (define-test aabb-aabb-manifold-penetration
              (let* ([a (make-aabb (vec2 0 0) (vec2 10 10))]
                     [b (make-aabb (vec2 8 0) (vec2 18 10))]
                     [m (aabb-aabb-manifold a b)])
                    (assert-true (not (not m)))
                    (assert-= (cadr m) 2 0.0001))))

(test-group circle-collision-tests
            (define-test circle-construction
              (let ([c (make-circle (vec2 5 5) 3)])
                   (assert-true (circle? c))
                   (assert-vec2-= (circle-center c) (vec2 5 5) epsilon)
                   (assert-= (circle-radius c) 3 epsilon)))
            
            (define-test circle-circle-overlapping
              (let ([a (make-circle (vec2 0 0) 5)]
                    [b (make-circle (vec2 8 0) 5)])
                   (assert-true (circle-circle? a b))))
            
            (define-test circle-circle-separated
              (let ([a (make-circle (vec2 0 0) 5)]
                    [b (make-circle (vec2 20 0) 5)])
                   (assert-false (circle-circle? a b))))
            
            (define-test circle-circle-touching
              (let ([a (make-circle (vec2 0 0) 5)]
                    [b (make-circle (vec2 10 0) 5)])
                   (assert-true (circle-circle? a b))))
            
            (define-test circle-circle-manifold
              (let* ([a (make-circle (vec2 0 0) 5)]
                     [b (make-circle (vec2 8 0) 5)]
                     [m (circle-circle-manifold a b)])
                    (assert-true (not (not m)))
                    (assert-vec2-= (car m) (vec2 1 0) 0.0001)
                    (assert-= (cadr m) 2 0.0001))))

(test-group circle-aabb-collision-tests
            (define-test circle-aabb-overlapping
              (let ([c (make-circle (vec2 5 5) 3)]
                    [a (make-aabb (vec2 0 0) (vec2 4 10))])
                   (assert-true (circle-aabb? c a))))
            
            (define-test circle-aabb-separated
              (let ([c (make-circle (vec2 10 10) 2)]
                    [a (make-aabb (vec2 0 0) (vec2 5 5))])
                   (assert-false (circle-aabb? c a))))
            
            (define-test circle-inside-aabb
              (let ([c (make-circle (vec2 5 5) 2)]
                    [a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-true (circle-aabb? c a)))))

(test-group polygon-collision-tests
            (define-test polygon-construction
              (let ([p (make-polygon (list (vec2 0 0) (vec2 4 0) (vec2 4 3) (vec2 0 3)))])
                   (assert-true (polygon? p))
                   (assert-= (polygon-vertex-count p) 4 0)))
            
            (define-test box-construction
              (let ([b (make-box (vec2 5 5) (vec2 2 2))])
                   (assert-= (polygon-vertex-count b) 4 0)))
            
            (define-test polygon-polygon-overlapping-sat
              (let ([a (make-box (vec2 0 0) (vec2 2 2))]
                    [b (make-box (vec2 1 1) (vec2 2 2))])
                   (assert-true (polygon-polygon? a b))))
            
            (define-test polygon-polygon-separated-sat
              (let ([a (make-box (vec2 0 0) (vec2 2 2))]
                    [b (make-box (vec2 10 10) (vec2 2 2))])
                   (assert-false (polygon-polygon? a b)))))

(test-group point-in-shape-tests
            (define-test point-inside-aabb
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-true (point-in-aabb? (vec2 5 5) a))
                   (assert-true (point-in-aabb? (vec2 0 0) a))
                   (assert-true (point-in-aabb? (vec2 10 10) a))))
            
            (define-test point-outside-aabb
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-false (point-in-aabb? (vec2 -1 5) a))
                   (assert-false (point-in-aabb? (vec2 11 5) a))))
            
            (define-test point-inside-circle
              (let ([c (make-circle (vec2 5 5) 3)])
                   (assert-true (point-in-circle? (vec2 5 5) c))
                   (assert-true (point-in-circle? (vec2 7 5) c))))
            
            (define-test point-outside-circle
              (let ([c (make-circle (vec2 5 5) 3)])
                   (assert-false (point-in-circle? (vec2 9 5) c)))))

(test-group edge-case-collision-tests
            (define-test coincident-circles
              (let* ([a (make-circle (vec2 5 5) 3)]
                     [b (make-circle (vec2 5 5) 2)]
                     [m (circle-circle-manifold a b)])
                    ;; Should handle coincident case with arbitrary normal
                    (assert-true (not (not m)))))
            
            (define-test zero-size-aabb
              (let ([a (make-aabb (vec2 5 5) (vec2 5 5))]
                    [b (make-aabb (vec2 5 5) (vec2 10 10))])
                   (assert-true (aabb-aabb? a b))))
            
            (define-test degenerate-segment
              (let ([closest (closest-point-on-segment (vec2 5 5) (vec2 0 0) (vec2 0 0))])
                   (assert-vec2-= closest (vec2 0 0) epsilon))))

;;; ====
;;; SECTION 5: COLLISION RESPONSE
;;; ====

(display "
----
                    COLLISION RESPONSE
----
")

(test-group material-tests
            (define-test material-construction
              (let ([m (make-material 0.5 0.4 0.3)])
                   (assert-true (material? m))
                   (assert-= (material-restitution m) 0.5 epsilon)
                   (assert-= (material-static-friction m) 0.4 epsilon)
                   (assert-= (material-dynamic-friction m) 0.3 epsilon)))
            
            (define-test default-materials-exist
              (assert-= (material-restitution rubber-material) 0.8 epsilon)
              (assert-= (material-restitution metal-material) 0.3 epsilon)
              (assert-= (material-restitution ice-material) 0.1 epsilon)))

(test-group impulse-calculation-tests
            (define-test elastic-impulse-calculation
              ;; Two identical balls colliding head-on
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [normal (vec2 1 0)]
                     [j (calculate-impulse a b normal 1.0)])
                    ;; j = -(1+1) * (-2) / 2 = 2
                    (assert-= j 2 0.0001)))
            
            (define-test inelastic-impulse-calculation
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [normal (vec2 1 0)]
                     [j (calculate-impulse a b normal 0.0)])
                    ;; j = -(1+0) * (-2) / 2 = 1
                    (assert-= j 1 0.0001)))
            
            (define-test separating-no-impulse
              (let* ([a (make-body-2d (vec2 0 0) (vec2 -1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 1 0) 1)]
                     [normal (vec2 1 0)]
                     [j (calculate-impulse a b normal 1.0)])
                    (assert-= j 0 epsilon))))

(test-group collision-resolution-tests
            (define-test elastic-head-on-collision
              ;; Two identical balls exchange velocities
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [elastic (make-material 1.0 0 0)]
                     [result (resolve-collision manifold elastic elastic)]
                     [new-a (car result)]
                     [new-b (cadr result)])
                    (assert-vec2-= (body-vel new-a) (vec2 -1 0) 0.0001)
                    (assert-vec2-= (body-vel new-b) (vec2 1 0) 0.0001)))
            
            (define-test inelastic-head-on-collision
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [inelastic (make-material 0.0 0 0)]
                     [result (resolve-collision manifold inelastic inelastic)]
                     [new-a (car result)]
                     [new-b (cadr result)])
                    ;; Equal masses + inelastic = both stop
                    (assert-vec2-= (body-vel new-a) (vec2 0 0) 0.0001)
                    (assert-vec2-= (body-vel new-b) (vec2 0 0) 0.0001)))
            
            (define-test ball-bounces-off-static-wall
              (let* ([ball (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [wall (make-static-body (vec2 1 0))]
                     [manifold (make-manifold ball wall (vec2 1 0) 0.05 (vec2 0.95 0))]
                     [elastic (make-material 1.0 0 0)]
                     [result (resolve-collision manifold elastic elastic)]
                     [new-ball (car result)])
                    (assert-vec2-= (body-vel new-ball) (vec2 -1 0) 0.0001))))

(test-group momentum-energy-tests
            (define-test momentum-conservation
              (let* ([a (make-body-2d (vec2 0 0) (vec2 2 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 2)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [mat (make-material 0.5 0 0)]
                     [result (resolve-collision manifold mat mat)]
                     [new-a (car result)]
                     [new-b (cadr result)]
                     ;; Initial momentum: 1*2 + 2*(-1) = 0
                     [p0 (+ (* 1 2) (* 2 -1))]
                     ;; Final momentum
                     [p1 (+ (* 1 (vec2-x (body-vel new-a)))
                            (* 2 (vec2-x (body-vel new-b))))])
                    (assert-= p1 p0 0.0001)))
            
            (define-test elastic-energy-conservation
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [mat (make-material 1.0 0 0)]
                     [result (resolve-collision manifold mat mat)]
                     [new-a (car result)]
                     [new-b (cadr result)]
                     [ke0 (+ (body-kinetic-energy a) (body-kinetic-energy b))]
                     [ke1 (+ (body-kinetic-energy new-a) (body-kinetic-energy new-b))])
                    (assert-= ke1 ke0 0.0001)))
            
            (define-test inelastic-energy-loss
              (let* ([a (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [b (make-body-2d (vec2 1 0) (vec2 -1 0) 1)]
                     [manifold (make-manifold a b (vec2 1 0) 0.1 (vec2 0.5 0))]
                     [mat (make-material 0.0 0 0)]
                     [result (resolve-collision manifold mat mat)]
                     [new-a (car result)]
                     [new-b (cadr result)]
                     [ke0 (+ (body-kinetic-energy a) (body-kinetic-energy b))]
                     [ke1 (+ (body-kinetic-energy new-a) (body-kinetic-energy new-b))])
                    (assert-true (< ke1 ke0)))))

;;; ====
;;; SECTION 6: PHYSICS WORLD
;;; ====

(display "
----
                    PHYSICS WORLD
----
")

(test-group world-creation-tests
            (define-test world-construction
              (let ([w (make-world (vec2 0 9.8))])
                   (assert-true (world? w))
                   (assert-vec2-= (world-gravity w) (vec2 0 9.8) epsilon)))
            
            (define-test world-add-entity
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    (assert-true (entity? (world-get-entity w 'ball)))))
            
            (define-test world-remove-entity
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    (world-remove-entity! w 'ball)
                    (assert-false (world-get-entity w 'ball))))
            
            (define-test world-entity-list
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 5 1 default-material)]
                     [e2 (make-circle-entity 'b (vec2 10 0) 5 1 default-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (assert-= (length (world-entity-list w)) 2 0))))

(test-group world-simulation-tests
            (define-test gravity-integration
              (let* ([w (make-world (vec2 0 100))]
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    (world-fixed-step! w 0.1)
                    (let ([ball (world-get-entity w 'ball)])
                         (assert-true (> (vec2-y (entity-vel ball)) 0))
                         (assert-true (> (vec2-y (entity-pos ball)) 0)))))
            
            (define-test static-body-no-movement
              (let* ([w (make-world (vec2 0 100))]
                     [e (make-static-circle 'static (vec2 50 50) 5 default-material)])
                    (world-add-entity! w e)
                    (world-fixed-step! w 0.1)
                    (let ([entity (world-get-entity w 'static)])
                         (assert-vec2-= (entity-pos entity) (vec2 50 50) epsilon))))
            
            (define-test velocity-integration
              (let* ([w (make-world (vec2 0 0))]
                     [body (make-body-2d (vec2 0 0) (vec2 100 0) 1)]
                     [shape (make-circle (vec2 0 0) 5)]
                     [e (make-entity 'mover body shape default-material #f)])
                    (world-add-entity! w e)
                    (world-fixed-step! w 0.1)
                    (let ([mover (world-get-entity w 'mover)])
                         (assert-= (vec2-x (entity-pos mover)) 10 0.5)))))

(test-group world-collision-tests
            (define-test detect-circle-collision
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 5 1 default-material)]
                     [e2 (make-circle-entity 'b (vec2 8 0) 5 1 default-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (let ([collisions (world-detect-collisions w)])
                         (assert-= (length collisions) 1 0))))
            
            (define-test no-collision-when-separated
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 5 1 default-material)]
                     [e2 (make-circle-entity 'b (vec2 100 0) 5 1 default-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (let ([collisions (world-detect-collisions w)])
                         (assert-= (length collisions) 0 0))))
            
            (define-test many-non-colliding-bodies
              (let* ([w (make-world (vec2 0 0))])
                    (world-add-entity! w (make-circle-entity 'a (vec2 0 0) 2 1 default-material))
                    (world-add-entity! w (make-circle-entity 'b (vec2 20 0) 2 1 default-material))
                    (world-add-entity! w (make-circle-entity 'c (vec2 40 0) 2 1 default-material))
                    (world-add-entity! w (make-circle-entity 'd (vec2 60 0) 2 1 default-material))
                    (let ([collisions (world-detect-collisions w)])
                         (assert-= (length collisions) 0 0)))))

(test-group world-query-tests
            (define-test query-aabb
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 5 5) 2 1 default-material)]
                     [e2 (make-circle-entity 'b (vec2 50 50) 2 1 default-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (let ([hits (world-query-aabb w (make-aabb (vec2 0 0) (vec2 10 10)))])
                         (assert-= (length hits) 1 0)
                         (assert-eq? (entity-id (car hits)) 'a))))
            
            (define-test query-point
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'c (vec2 10 10) 5 1 default-material)])
                    (world-add-entity! w e)
                    (let ([hits (world-query-point w (vec2 10 10))])
                         (assert-= (length hits) 1 0))
                    (let ([misses (world-query-point w (vec2 100 100))])
                         (assert-= (length misses) 0 0)))))

(test-group world-manipulation-tests
            (define-test set-velocity
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    (set-velocity! w 'ball (vec2 100 50))
                    (let ([ball (world-get-entity w 'ball)])
                         (assert-vec2-= (entity-vel ball) (vec2 100 50) epsilon))))
            
            (define-test apply-impulse
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'ball (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    (apply-impulse! w 'ball (vec2 10 0))
                    (let ([ball (world-get-entity w 'ball)])
                         (assert-vec2-= (entity-vel ball) (vec2 10 0) epsilon)))))

;;; ====
;;; SECTION 7: EDGE CASES AND DEGENERATE INPUTS
;;; ====

(display "
----
                    EDGE CASES
----
")

(test-group degenerate-input-tests
            (define-test zero-vector-normalization
              (let ([v (vec2-normalize (vec2 0 0))])
                   (assert-true (vec2-zero? v))))
            
            (define-test zero-mass-body-is-static
              (let ([b (make-body-2d (vec2 0 0) (vec2 0 0) 0)])
                   (assert-true (body-static? b))))
            
            (define-test empty-world-no-collisions
              (let* ([w (make-world (vec2 0 0))]
                     [collisions (world-detect-collisions w)])
                    (assert-= (length collisions) 0 0)))
            
            (define-test single-body-no-collisions
              (let* ([w (make-world (vec2 0 0))]
                     [e (make-circle-entity 'alone (vec2 0 0) 5 1 default-material)])
                    (world-add-entity! w e)
                    (let ([collisions (world-detect-collisions w)])
                         (assert-= (length collisions) 0 0)))))

(test-group numerical-stability-tests
            (define-test very-large-velocities
              (let* ([b (make-body-2d (vec2 0 0) (vec2 1e6 1e6) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [b2 (integrate-body-euler b f 0.001)])
                    ;; Should move 1000 units in each direction
                    (assert-= (vec2-x (body-pos b2)) 1000 1)))
            
            (define-test very-small-timesteps
              (let* ([b (make-body-2d (vec2 0 0) (vec2 10 0) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [b2 (integrate-body-euler b f 0.0001)])
                    (assert-= (vec2-x (body-pos b2)) 0.001 0.0001)))
            
            (define-test many-integration-steps
              ;; Ensure numerical drift doesn't accumulate badly
              (let* ([b0 (make-body-2d (vec2 0 0) (vec2 1 0) 1)]
                     [f (lambda (body) (vec2 0 0))]
                     [dt 0.001]
                     [final-b (let loop ([b b0] [i 0])
                                   (if (>= i 1000)
                                       b
                                       (loop (integrate-body-euler b f dt) (+ i 1))))])
                    ;; After 1000 steps of 0.001, should have moved 1 unit
                    (assert-= (vec2-x (body-pos final-b)) 1.0 0.01))))

;;; ====
;;; Summary
;;; ====

(display "
====
                         SUMMARY
====
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All physics engine tests passed.
")
    (display "
[FAILURE] Some physics engine tests failed.
"))

;; Exit with appropriate code
(when (> *tests-failed* 0)
      (exit 1))
