;;; core/geometry/test-geometry.ss — Tests for Geometry Primitives
;;;
;;; Comprehensive tests for geometric primitives, transformations,
;;; distance calculations, intersections, and utilities.
;;;
;;; Run: scheme --script core/geometry/test-geometry.ss

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/geometry/geometry.ss")

(set! *current-group* 'geometry)

;;; Helper for approximate equality
(define (approx= a b eps)
  (< (abs (- a b)) eps))

;;; Helper for approximate vector equality
(define (vec3-approx= v1 v2 eps)
  (and (approx= (vec3-x v1) (vec3-x v2) eps)
       (approx= (vec3-y v1) (vec3-y v2) eps)
       (approx= (vec3-z v1) (vec3-z v2) eps)))

;;; ====
;;; Primitive Construction Tests
;;; ====

(define-test "point3 creation"
  (let ([p (point3 1 2 3)])
       (assert-equal (vec3-x p) 1)
       (assert-equal (vec3-y p) 2)
       (assert-equal (vec3-z p) 3)))

(define-test "line3 creation and accessors"
  (let* ([origin (vec3 0 0 0)]
         [dir (vec3 1 0 0)]
         [line (line3 origin dir)])
        (assert-true (line3? line))
        (assert-equal (line3-origin line) origin)
        (assert-equal (line3-direction line) dir)))

(define-test "ray3 creation and accessors"
  (let* ([origin (vec3 0 0 0)]
         [dir (vec3 0 1 0)]
         [ray (ray3 origin dir)])
        (assert-true (ray3? ray))
        (assert-equal (ray3-origin ray) origin)
        (assert-equal (ray3-direction ray) dir)))

(define-test "ray3-point-at"
  (let* ([ray (ray3 (vec3 0 0 0) (vec3 1 0 0))]
         [p (ray3-point-at ray 5)])
        (assert-true (vec3-approx= p (vec3 5 0 0) 0.001))))

(define-test "plane3 creation and accessors"
  (let* ([normal (vec3 0 1 0)]
         [plane (plane3 normal 5)])
        (assert-true (plane3? plane))
        (assert-equal (plane3-normal plane) normal)
        (assert-equal (plane3-d plane) 5)))

(define-test "plane3-from-point-normal"
  (let* ([point (vec3 0 5 0)]
         [normal (vec3 0 1 0)]
         [plane (plane3-from-point-normal point normal)])
        (assert-true (plane3? plane))
        (assert-true (approx= (plane3-d plane) -5 0.001))))

(define-test "plane3-from-points valid"
  (let* ([p1 (vec3 0 0 0)]
         [p2 (vec3 1 0 0)]
         [p3 (vec3 0 1 0)]
         [plane (plane3-from-points p1 p2 p3)])
        (assert-true (plane3? plane))
        ;; Normal should point in +z direction
        (assert-true (approx= (vec3-z (plane3-normal plane)) 1.0 0.001))))

(define-test "plane3-from-points collinear returns error"
  (let* ([p1 (vec3 0 0 0)]
         [p2 (vec3 1 0 0)]
         [p3 (vec3 2 0 0)]  ; Collinear with p1 and p2
         [result (plane3-from-points p1 p2 p3)])
        (assert-true (and (pair? result) (eq? (car result) 'error)))))

(define-test "triangle3 creation and accessors"
  (let* ([p1 (vec3 0 0 0)]
         [p2 (vec3 1 0 0)]
         [p3 (vec3 0 1 0)]
         [tri (triangle3 p1 p2 p3)])
        (assert-true (triangle3? tri))
        (assert-equal (triangle3-p1 tri) p1)
        (assert-equal (triangle3-p2 tri) p2)
        (assert-equal (triangle3-p3 tri) p3)))

(define-test "circle creation and accessors"
  (let* ([center (vec3 1 2 0)]
         [c (circle center 5)])
        (assert-true (circle? c))
        (assert-equal (circle-center c) center)
        (assert-equal (circle-radius c) 5)))

(define-test "sphere creation and accessors"
  (let* ([center (vec3 1 2 3)]
         [s (sphere center 10)])
        (assert-true (sphere? s))
        (assert-equal (sphere-center s) center)
        (assert-equal (sphere-radius s) 10)))

(define-test "aabb creation and accessors"
  (let* ([bmin (vec3 -1 -1 -1)]
         [bmax (vec3 1 1 1)]
         [box (aabb bmin bmax)])
        (assert-true (aabb? box))
        (assert-equal (aabb-min box) bmin)
        (assert-equal (aabb-max box) bmax)))

(define-test "aabb-center"
  (let* ([box (aabb (vec3 0 0 0) (vec3 10 10 10))]
         [center (aabb-center box)])
        (assert-true (vec3-approx= center (vec3 5 5 5) 0.001))))

(define-test "aabb-extents"
  (let* ([box (aabb (vec3 0 0 0) (vec3 10 10 10))]
         [extents (aabb-extents box)])
        (assert-true (vec3-approx= extents (vec3 5 5 5) 0.001))))

(define-test "obb creation and accessors"
  (let* ([center (vec3 0 0 0)]
         [axes (list (vec3 1 0 0) (vec3 0 1 0) (vec3 0 0 1))]
         [extents (vec3 1 2 3)]
         [box (obb center axes extents)])
        (assert-true (obb? box))
        (assert-equal (obb-center box) center)
        (assert-equal (obb-axes box) axes)
        (assert-equal (obb-extents box) extents)))

;;; ====
;;; Transformation Tests
;;; ====

(define-test "transform-identity"
  (let* ([m (transform-identity)]
         [p (vec3 1 2 3)]
         [result (transform-point m p)])
        (assert-true (vec3-approx= result p 0.001))))

(define-test "transform-translation"
  (let* ([m (transform-translation (vec3 10 20 30))]
         [p (vec3 1 2 3)]
         [result (transform-point m p)])
        (assert-true (vec3-approx= result (vec3 11 22 33) 0.001))))

(define-test "transform-scale uniform"
  (let* ([m (transform-scale 2)]
         [p (vec3 1 2 3)]
         [result (transform-point m p)])
        (assert-true (vec3-approx= result (vec3 2 4 6) 0.001))))

(define-test "transform-scale non-uniform"
  (let* ([m (transform-scale (vec3 2 3 4))]
         [p (vec3 1 1 1)]
         [result (transform-point m p)])
        (assert-true (vec3-approx= result (vec3 2 3 4) 0.001))))

(define-test "transform-rotation-x 90 degrees"
  (let* ([angle (/ 3.141592653589793 2)]  ; 90 degrees
         [m (transform-rotation-x angle)]
         [p (vec3 0 1 0)]
         [result (transform-point m p)])
        ;; Y axis rotates to Z axis
        (assert-true (approx= (vec3-x result) 0 0.001))
        (assert-true (approx= (vec3-y result) 0 0.001))
        (assert-true (approx= (vec3-z result) 1 0.001))))

(define-test "transform-rotation-y 90 degrees"
  (let* ([angle (/ 3.141592653589793 2)]
         [m (transform-rotation-y angle)]
         [p (vec3 1 0 0)]
         [result (transform-point m p)])
        ;; X axis rotates to -Z axis
        (assert-true (approx= (vec3-x result) 0 0.001))
        (assert-true (approx= (vec3-y result) 0 0.001))
        (assert-true (approx= (vec3-z result) -1 0.001))))

(define-test "transform-rotation-z 90 degrees"
  (let* ([angle (/ 3.141592653589793 2)]
         [m (transform-rotation-z angle)]
         [p (vec3 1 0 0)]
         [result (transform-point m p)])
        ;; X axis rotates to Y axis
        (assert-true (approx= (vec3-x result) 0 0.001))
        (assert-true (approx= (vec3-y result) 1 0.001))
        (assert-true (approx= (vec3-z result) 0 0.001))))

(define-test "transform-rotation-axis"
  (let* ([axis (vec3 0 0 1)]
         [angle (/ 3.141592653589793 2)]  ; 90 degrees
         [m (transform-rotation-axis axis angle)]
         [p (vec3 1 0 0)]
         [result (transform-point m p)])
        ;; Same as rotation-z
        (assert-true (approx= (vec3-x result) 0 0.001))
        (assert-true (approx= (vec3-y result) 1 0.001))))

(define-test "transform-rotation-axis zero vector returns identity"
  (let* ([axis (vec3 0 0 0)]
         [angle 1.5]
         [m (transform-rotation-axis axis angle)]
         [p (vec3 1 2 3)]
         [result (transform-point m p)])
        ;; Should return identity transform
        (assert-true (vec3-approx= result p 0.001))))

(define-test "transform-vector ignores translation"
  (let* ([m (transform-translation (vec3 100 100 100))]
         [v (vec3 1 0 0)]
         [result (transform-vector m v)])
        ;; Vector should not be affected by translation
        (assert-true (vec3-approx= result v 0.001))))

;;; ====
;;; Distance Tests
;;; ====

(define-test "distance-point-point"
  (let* ([p1 (vec3 0 0 0)]
         [p2 (vec3 3 4 0)]
         [dist (distance-point-point p1 p2)])
        (assert-true (approx= dist 5.0 0.001))))

(define-test "distance-point-plane positive"
  (let* ([point (vec3 0 10 0)]
         [plane (plane3 (vec3 0 1 0) 0)]  ; XZ plane
         [dist (distance-point-plane point plane)])
        (assert-true (approx= dist 10.0 0.001))))

(define-test "distance-point-plane negative"
  (let* ([point (vec3 0 -5 0)]
         [plane (plane3 (vec3 0 1 0) 0)]
         [dist (distance-point-plane point plane)])
        (assert-true (approx= dist -5.0 0.001))))

(define-test "distance-point-line"
  (let* ([point (vec3 0 5 0)]
         [line (line3 (vec3 0 0 0) (vec3 1 0 0))]  ; X axis
         [dist (distance-point-line point line)])
        (assert-true (approx= dist 5.0 0.001))))

(define-test "distance-point-sphere inside"
  (let* ([point (vec3 0 0 0)]
         [s (sphere (vec3 0 0 0) 10)]
         [dist (distance-point-sphere point s)])
        (assert-true (< dist 0))
        (assert-true (approx= dist -10.0 0.001))))

(define-test "distance-point-sphere outside"
  (let* ([point (vec3 15 0 0)]
         [s (sphere (vec3 0 0 0) 10)]
         [dist (distance-point-sphere point s)])
        (assert-true (> dist 0))
        (assert-true (approx= dist 5.0 0.001))))

;;; ====
;;; Intersection Tests
;;; ====

(define-test "intersect-ray-plane hit"
  (let* ([ray (ray3 (vec3 0 10 0) (vec3 0 -1 0))]
         [plane (plane3 (vec3 0 1 0) 0)]
         [t (intersect-ray-plane ray plane)])
        (assert-false (equal? t #f))
        (assert-true (approx= t 10.0 0.001))))

(define-test "intersect-ray-plane miss (parallel)"
  (let* ([ray (ray3 (vec3 0 10 0) (vec3 1 0 0))]  ; Parallel to plane
         [plane (plane3 (vec3 0 1 0) 0)]
         [t (intersect-ray-plane ray plane)])
        (assert-false t)))

(define-test "intersect-ray-plane miss (behind origin)"
  (let* ([ray (ray3 (vec3 0 10 0) (vec3 0 1 0))]  ; Pointing away
         [plane (plane3 (vec3 0 1 0) 0)]
         [t (intersect-ray-plane ray plane)])
        (assert-false t)))

(define-test "intersect-ray-sphere hit"
  (let* ([ray (ray3 (vec3 0 0 -20) (vec3 0 0 1))]
         [s (sphere (vec3 0 0 0) 5)]
         [result (intersect-ray-sphere ray s)])
        (assert-false (equal? result #f))
        (let ([t1 (car result)]
              [t2 (cadr result)])
             (assert-true (< t1 t2))
             (assert-true (approx= t1 15.0 0.001))
             (assert-true (approx= t2 25.0 0.001)))))

(define-test "intersect-ray-sphere miss"
  (let* ([ray (ray3 (vec3 20 0 -20) (vec3 0 0 1))]  ; Off to the side
         [s (sphere (vec3 0 0 0) 5)]
         [result (intersect-ray-sphere ray s)])
        (assert-false result)))

(define-test "intersect-ray-aabb hit"
  (let* ([ray (ray3 (vec3 0 0 -10) (vec3 0 0 1))]
         [box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))]
         [result (intersect-ray-aabb ray box)])
        (assert-false (equal? result #f))
        (let ([tmin (car result)]
              [tmax (cadr result)])
             (assert-true (approx= tmin 9.0 0.001))
             (assert-true (approx= tmax 11.0 0.001)))))

(define-test "intersect-ray-aabb miss"
  (let* ([ray (ray3 (vec3 10 10 -10) (vec3 0 0 1))]
         [box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))]
         [result (intersect-ray-aabb ray box)])
        (assert-false result)))

(define-test "intersect-ray-aabb ray parallel to slab inside"
  (let* ([ray (ray3 (vec3 0 0 0) (vec3 1 0 0))]  ; Inside box, parallel to X
         [box (aabb (vec3 -5 -1 -1) (vec3 5 1 1))]
         [result (intersect-ray-aabb ray box)])
        (assert-false (equal? result #f))))

(define-test "intersect-ray-triangle hit"
  (let* ([ray (ray3 (vec3 0.25 0.25 -5) (vec3 0 0 1))]
         [tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [t (intersect-ray-triangle ray tri)])
        (assert-false (equal? t #f))
        (assert-true (approx= t 5.0 0.001))))

(define-test "intersect-ray-triangle miss"
  (let* ([ray (ray3 (vec3 10 10 -5) (vec3 0 0 1))]  ; Outside triangle
         [tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [t (intersect-ray-triangle ray tri)])
        (assert-false t)))

;;; ====
;;; Containment Tests
;;; ====

(define-test "point-in-sphere? inside"
  (let* ([point (vec3 1 1 1)]
         [s (sphere (vec3 0 0 0) 10)])
        (assert-true (point-in-sphere? point s))))

(define-test "point-in-sphere? outside"
  (let* ([point (vec3 20 0 0)]
         [s (sphere (vec3 0 0 0) 10)])
        (assert-false (point-in-sphere? point s))))

(define-test "point-in-aabb? inside"
  (let* ([point (vec3 0 0 0)]
         [box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))])
        (assert-true (point-in-aabb? point box))))

(define-test "point-in-aabb? outside"
  (let* ([point (vec3 5 0 0)]
         [box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))])
        (assert-false (point-in-aabb? point box))))

(define-test "point-in-aabb? on boundary"
  (let* ([point (vec3 1 0 0)]  ; On edge
         [box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))])
        (assert-true (point-in-aabb? point box))))

(define-test "point-in-triangle? inside"
  (let* ([point (vec3 0.25 0.25 0)]
         [tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))])
        (assert-true (point-in-triangle? point tri))))

(define-test "point-in-triangle? outside"
  (let* ([point (vec3 2 2 0)]  ; Way outside
         [tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))])
        (assert-false (point-in-triangle? point tri))))

;;; ====
;;; Closest Point Tests
;;; ====

(define-test "closest-point-on-line"
  (let* ([point (vec3 5 10 0)]
         [line (line3 (vec3 0 0 0) (vec3 1 0 0))]
         [closest (closest-point-on-line point line)])
        (assert-true (vec3-approx= closest (vec3 5 0 0) 0.001))))

(define-test "closest-point-on-plane"
  (let* ([point (vec3 5 10 5)]
         [plane (plane3 (vec3 0 1 0) 0)]
         [closest (closest-point-on-plane point plane)])
        (assert-true (vec3-approx= closest (vec3 5 0 5) 0.001))))

(define-test "closest-point-on-aabb inside"
  (let* ([point (vec3 0 0 0)]
         [box (aabb (vec3 -10 -10 -10) (vec3 10 10 10))]
         [closest (closest-point-on-aabb point box)])
        ;; Point inside returns itself clamped
        (assert-true (vec3-approx= closest point 0.001))))

(define-test "closest-point-on-aabb outside"
  (let* ([point (vec3 20 5 5)]
         [box (aabb (vec3 0 0 0) (vec3 10 10 10))]
         [closest (closest-point-on-aabb point box)])
        (assert-true (vec3-approx= closest (vec3 10 5 5) 0.001))))

;;; ====
;;; Area and Volume Tests
;;; ====

(define-test "triangle-area"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 2 0 0) (vec3 0 2 0))]
         [area (triangle-area tri)])
        (assert-true (approx= area 2.0 0.001))))  ; Base 2, height 2, area = 2

(define-test "sphere-volume"
  (let* ([s (sphere (vec3 0 0 0) 1)]
         [vol (sphere-volume s)])
        ;; V = (4/3) * pi * r^3
        (assert-true (approx= vol 4.18879 0.001))))

(define-test "sphere-surface-area"
  (let* ([s (sphere (vec3 0 0 0) 1)]
         [area (sphere-surface-area s)])
        ;; A = 4 * pi * r^2
        (assert-true (approx= area 12.56637 0.001))))

(define-test "aabb-volume"
  (let* ([box (aabb (vec3 0 0 0) (vec3 2 3 4))]
         [vol (aabb-volume box)])
        ;; Volume = 2 * 3 * 4 = 24
        (assert-true (approx= vol 24.0 0.001))))

;;; ====
;;; Utility Tests
;;; ====

(define-test "barycentric-coords at vertex"
  (let* ([p (vec3 0 0 0)]
         [a (vec3 0 0 0)]
         [b (vec3 1 0 0)]
         [c (vec3 0 1 0)]
         [bary (barycentric-coords p a b c)])
        ;; At vertex a: u=1, v=0, w=0
        (assert-true (approx= (car bary) 1.0 0.01))))

(define-test "barycentric-coords at centroid"
  (let* ([a (vec3 0 0 0)]
         [b (vec3 3 0 0)]
         [c (vec3 0 3 0)]
         [centroid (vec3 1 1 0)]  ; Centroid of triangle
         [bary (barycentric-coords centroid a b c)])
        ;; At centroid: u=v=w=1/3
        (assert-true (approx= (car bary) 0.333 0.01))
        (assert-true (approx= (cadr bary) 0.333 0.01))
        (assert-true (approx= (caddr bary) 0.333 0.01))))

(define-test "triangle-normal"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [normal (triangle-normal tri)])
        ;; Normal should point in +Z direction
        (assert-true (vec3-approx= normal (vec3 0 0 1) 0.001))))

(define-test "aabb-merge"
  (let* ([b1 (aabb (vec3 0 0 0) (vec3 1 1 1))]
         [b2 (aabb (vec3 2 2 2) (vec3 3 3 3))]
         [merged (aabb-merge b1 b2)])
        (assert-true (vec3-approx= (aabb-min merged) (vec3 0 0 0) 0.001))
        (assert-true (vec3-approx= (aabb-max merged) (vec3 3 3 3) 0.001))))

(define-test "aabb-from-points"
  (let* ([points (list (vec3 -1 5 2) (vec3 3 -2 8) (vec3 0 0 0))]
         [box (aabb-from-points points)])
        (assert-true (vec3-approx= (aabb-min box) (vec3 -1 -2 0) 0.001))
        (assert-true (vec3-approx= (aabb-max box) (vec3 3 5 8) 0.001))))

(define-test "aabb-from-points empty list"
  (let* ([box (aabb-from-points '())])
        (assert-true (aabb? box))))

;;; ====
;;; Coordinate Conversion Tests
;;; ====

(define-test "vec3-to-spherical at +Z"
  (let* ([v (vec3 0.001 0 1)]  ; Slight offset to avoid atan(0,0)
         [spherical (vec3-to-spherical v)]
         [r (car spherical)]
         [theta (cadr spherical)])
        (assert-true (approx= r 1.0 0.001))
        (assert-true (approx= theta 0.0 0.01))))  ; theta~0 near +Z pole

(define-test "vec3-to-spherical at +X"
  (let* ([v (vec3 1 0 0)]
         [spherical (vec3-to-spherical v)]
         [r (car spherical)]
         [theta (cadr spherical)]
         [phi (caddr spherical)])
        (assert-true (approx= r 1.0 0.001))
        (assert-true (approx= theta 1.5708 0.001))  ; pi/2
        (assert-true (approx= phi 0.0 0.001))))

(define-test "vec3-to-cylindrical at +X"
  (let* ([v (vec3 1 0 5)]
         [cylindrical (vec3-to-cylindrical v)]
         [r (car cylindrical)]
         [theta (cadr cylindrical)]
         [z (caddr cylindrical)])
        (assert-true (approx= r 1.0 0.001))
        (assert-true (approx= theta 0.0 0.001))
        (assert-true (approx= z 5.0 0.001))))

(define-test "vec3-to-cylindrical at 45 degrees"
  (let* ([v (vec3 1 1 0)]
         [cylindrical (vec3-to-cylindrical v)]
         [r (car cylindrical)]
         [theta (cadr cylindrical)])
        (assert-true (approx= r 1.41421 0.001))  ; sqrt(2)
        (assert-true (approx= theta 0.7854 0.001))))  ; pi/4

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "Tests run:    ")
(display *tests-run*)
(newline)
(display "Tests passed: ")
(display *tests-passed*)
(newline)
(display "Tests failed: ")
(display *tests-failed*)
(newline)

(if (> *tests-failed* 0)
    (exit 1)
    (exit 0))
