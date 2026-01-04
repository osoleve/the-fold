;;; core/geometry/test-geometry.ss — Tests for Geometry Module
;;;
;;; Tests geometric primitives, transformations, and operations.

(load "core/test-framework.ss")
(load "core/geometry/geometry.ss")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(define (approx= a b)
  (< (abs (- a b)) 1e-6))

(define (vec3-approx= v1 v2)
  (and (approx= (vec3-x v1) (vec3-x v2))
       (approx= (vec3-y v1) (vec3-y v2))
       (approx= (vec3-z v1) (vec3-z v2))))

;;; ============================================================
;;; Primitive Construction Tests
;;; ============================================================

(define-test "line3 construction"
  (let ([l (line3 (vec3 1 2 3) (vec3 0 1 0))])
       (assert (line3? l))
       (assert (vec3-approx= (line3-origin l) (vec3 1 2 3)))
       (assert (vec3-approx= (line3-direction l) (vec3 0 1 0)))))

(define-test "ray3 construction"
  (let ([r (ray3 (vec3 0 0 0) (vec3 1 0 0))])
       (assert (ray3? r))
       (assert (vec3-approx= (ray3-origin r) (vec3 0 0 0)))
       (assert (vec3-approx= (ray3-direction r) (vec3 1 0 0)))))

(define-test "ray3 point-at"
  (let* ([r (ray3 (vec3 1 2 3) (vec3 1 0 0))]
         [p (ray3-point-at r 5)])
        (assert (vec3-approx= p (vec3 6 2 3)))))

(define-test "plane3 from point and normal"
  (let* ([p (plane3-from-point-normal (vec3 0 0 1) (vec3 0 0 1))]
         [dist (distance-point-plane (vec3 0 0 1) p)])
        (assert (plane3? p))
        (assert (approx= dist 0))))

(define-test "plane3 from three points"
  (let* ([p1 (vec3 0 0 0)]
         [p2 (vec3 1 0 0)]
         [p3 (vec3 0 1 0)]
         [plane (plane3-from-points p1 p2 p3)]
         [normal (plane3-normal plane)])
        (assert (plane3? plane))
        ; Normal should point in +z direction
        (assert (approx= (vec3-z normal) 1))))

(define-test "triangle3 construction"
  (let ([t (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))])
       (assert (triangle3? t))
       (assert (vec3-approx= (triangle3-p1 t) (vec3 0 0 0)))))

(define-test "sphere construction"
  (let ([s (sphere (vec3 1 2 3) 5)])
       (assert (sphere? s))
       (assert (vec3-approx= (sphere-center s) (vec3 1 2 3)))
       (assert (= (sphere-radius s) 5))))

(define-test "aabb construction and accessors"
  (let* ([box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))]
         [center (aabb-center box)]
         [extents (aabb-extents box)])
        (assert (aabb? box))
        (assert (vec3-approx= center (vec3 0 0 0)))
        (assert (vec3-approx= extents (vec3 1 1 1)))))

;;; ============================================================
;;; Transformation Tests
;;; ============================================================

(define-test "transform identity"
  (let* ([t (transform-identity)]
         [p (vec3 1 2 3)]
         [p2 (transform-point t p)])
        (assert (vec3-approx= p p2))))

(define-test "transform translation"
  (let* ([t (transform-translation (vec3 5 0 0))]
         [p (vec3 1 2 3)]
         [p2 (transform-point t p)])
        (assert (vec3-approx= p2 (vec3 6 2 3)))))

(define-test "transform uniform scale"
  (let* ([t (transform-scale 2)]
         [p (vec3 1 2 3)]
         [p2 (transform-point t p)])
        (assert (vec3-approx= p2 (vec3 2 4 6)))))

(define-test "transform rotation-x 90 degrees"
  (let* ([t (transform-rotation-x (/ 3.141592653589793 2))]
         [p (vec3 0 1 0)]
         [p2 (transform-point t p)])
        (assert (approx= (vec3-x p2) 0))
        (assert (approx= (vec3-y p2) 0))
        (assert (approx= (vec3-z p2) 1))))

(define-test "transform rotation-z 90 degrees"
  (let* ([t (transform-rotation-z (/ 3.141592653589793 2))]
         [p (vec3 1 0 0)]
         [p2 (transform-point t p)])
        (assert (approx= (vec3-x p2) 0))
        (assert (approx= (vec3-y p2) 1))
        (assert (approx= (vec3-z p2) 0))))

(define-test "transform vector (no translation)"
  (let* ([t (transform-translation (vec3 5 5 5))]
         [v (vec3 1 0 0)]
         [v2 (transform-vector t v)])
        ; Translation should not affect vectors
        (assert (vec3-approx= v v2))))

;;; ============================================================
;;; Distance Tests
;;; ============================================================

(define-test "distance point to point"
  (let ([d (distance-point-point (vec3 0 0 0) (vec3 3 4 0))])
       (assert (approx= d 5))))

(define-test "distance point to plane"
  (let* ([plane (plane3-from-point-normal (vec3 0 0 0) (vec3 0 0 1))]
         [d (distance-point-plane (vec3 0 0 5) plane)])
        (assert (approx= d 5))))

(define-test "distance point to line"
  (let* ([line (line3 (vec3 0 0 0) (vec3 1 0 0))]
         [d (distance-point-line (vec3 5 3 0) line)])
        (assert (approx= d 3))))

(define-test "distance point to sphere - outside"
  (let* ([s (sphere (vec3 0 0 0) 5)]
         [d (distance-point-sphere (vec3 10 0 0) s)])
        (assert (approx= d 5))))

(define-test "distance point to sphere - inside"
  (let* ([s (sphere (vec3 0 0 0) 5)]
         [d (distance-point-sphere (vec3 3 0 0) s)])
        (assert (approx= d -2))))

;;; ============================================================
;;; Intersection Tests
;;; ============================================================

(define-test "ray-plane intersection - hit"
  (let* ([ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [plane (plane3-from-point-normal (vec3 0 0 0) (vec3 0 0 1))]
         [t (intersect-ray-plane ray plane)])
        (assert t)
        (assert (approx= t 5))))

(define-test "ray-plane intersection - parallel"
  (let* ([ray (ray3 (vec3 0 0 1) (vec3 1 0 0))]
         [plane (plane3-from-point-normal (vec3 0 0 0) (vec3 0 0 1))]
         [t (intersect-ray-plane ray plane)])
        (assert (not t))))

(define-test "ray-sphere intersection - hit"
  (let* ([ray (ray3 (vec3 -10 0 0) (vec3 1 0 0))]
         [s (sphere (vec3 0 0 0) 2)]
         [result (intersect-ray-sphere ray s)])
        (assert result)
        (assert (approx= (car result) 8))
        (assert (approx= (cadr result) 12))))

(define-test "ray-sphere intersection - miss"
  (let* ([ray (ray3 (vec3 -10 5 0) (vec3 1 0 0))]
         [s (sphere (vec3 0 0 0) 2)]
         [result (intersect-ray-sphere ray s)])
        (assert (not result))))

(define-test "ray-aabb intersection - hit"
  (let* ([ray (ray3 (vec3 -5 0 0) (vec3 1 0 0))]
         [box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))]
         [result (intersect-ray-aabb ray box)])
        (assert result)
        (assert (approx= (car result) 4))
        (assert (approx= (cadr result) 6))))

(define-test "ray-aabb intersection - miss"
  (let* ([ray (ray3 (vec3 -5 5 0) (vec3 1 0 0))]
         [box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))]
         [result (intersect-ray-aabb ray box)])
        (assert (not result))))

(define-test "ray-triangle intersection - hit"
  (let* ([ray (ray3 (vec3 0.25 0.25 -1) (vec3 0 0 1))]
         [tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [t (intersect-ray-triangle ray tri)])
        (assert t)
        (assert (approx= t 1))))

(define-test "ray-triangle intersection - miss"
  (let* ([ray (ray3 (vec3 2 2 -1) (vec3 0 0 1))]
         [tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [t (intersect-ray-triangle ray tri)])
        (assert (not t))))

;;; ============================================================
;;; Containment Tests
;;; ============================================================

(define-test "point in sphere - inside"
  (let ([s (sphere (vec3 0 0 0) 5)])
       (assert (point-in-sphere? (vec3 3 0 0) s))))

(define-test "point in sphere - outside"
  (let ([s (sphere (vec3 0 0 0) 5)])
       (assert (not (point-in-sphere? (vec3 10 0 0) s)))))

(define-test "point in aabb - inside"
  (let ([box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))])
       (assert (point-in-aabb? (vec3 0 0 0) box))))

(define-test "point in aabb - outside"
  (let ([box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))])
       (assert (not (point-in-aabb? (vec3 2 0 0) box)))))

(define-test "point in triangle - inside"
  (let ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))])
       (assert (point-in-triangle? (vec3 0.25 0.25 0) tri))))

(define-test "point in triangle - outside"
  (let ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))])
       (assert (not (point-in-triangle? (vec3 0.6 0.6 0) tri)))))

;;; ============================================================
;;; Closest Point Tests
;;; ============================================================

(define-test "closest point on line"
  (let* ([line (line3 (vec3 0 0 0) (vec3 1 0 0))]
         [p (vec3 5 3 0)]
         [closest (closest-point-on-line p line)])
        (assert (vec3-approx= closest (vec3 5 0 0)))))

(define-test "closest point on plane"
  (let* ([plane (plane3-from-point-normal (vec3 0 0 0) (vec3 0 0 1))]
         [p (vec3 1 2 5)]
         [closest (closest-point-on-plane p plane)])
        (assert (vec3-approx= closest (vec3 1 2 0)))))

(define-test "closest point on aabb - outside"
  (let* ([box (aabb (vec3 0 0 0) (vec3 1 1 1))]
         [p (vec3 2 0.5 0.5)]
         [closest (closest-point-on-aabb p box)])
        (assert (vec3-approx= closest (vec3 1 0.5 0.5)))))

(define-test "closest point on aabb - inside"
  (let* ([box (aabb (vec3 0 0 0) (vec3 1 1 1))]
         [p (vec3 0.5 0.5 0.5)]
         [closest (closest-point-on-aabb p box)])
        (assert (vec3-approx= closest (vec3 0.5 0.5 0.5)))))

;;; ============================================================
;;; Area and Volume Tests
;;; ============================================================

(define-test "triangle area"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [area (triangle-area tri)])
        (assert (approx= area 0.5))))

(define-test "sphere volume"
  (let* ([s (sphere (vec3 0 0 0) 1)]
         [vol (sphere-volume s)])
        (assert (approx= vol 4.1887902))))

(define-test "sphere surface area"
  (let* ([s (sphere (vec3 0 0 0) 1)]
         [area (sphere-surface-area s)])
        (assert (approx= area 12.566370))))

(define-test "aabb volume"
  (let* ([box (aabb (vec3 0 0 0) (vec3 2 2 2))]
         [vol (aabb-volume box)])
        (assert (approx= vol 8))))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(define-test "barycentric coordinates - center"
  (let* ([a (vec3 0 0 0)]
         [b (vec3 1 0 0)]
         [c (vec3 0 1 0)]
         [p (vec3 (/ 1.0 3.0) (/ 1.0 3.0) 0)]
         [coords (barycentric-coords p a b c)])
        (assert (approx= (car coords) (/ 1.0 3.0)))
        (assert (approx= (cadr coords) (/ 1.0 3.0)))
        (assert (approx= (caddr coords) (/ 1.0 3.0)))))

(define-test "barycentric coordinates - vertex"
  (let* ([a (vec3 0 0 0)]
         [b (vec3 1 0 0)]
         [c (vec3 0 1 0)]
         [coords (barycentric-coords a a b c)])
        (assert (approx= (car coords) 1))
        (assert (approx= (cadr coords) 0))
        (assert (approx= (caddr coords) 0))))

(define-test "triangle normal"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [normal (triangle-normal tri)])
        (assert (approx= (vec3-x normal) 0))
        (assert (approx= (vec3-y normal) 0))
        (assert (approx= (vec3-z normal) 1))))

(define-test "aabb merge"
  (let* ([b1 (aabb (vec3 0 0 0) (vec3 1 1 1))]
         [b2 (aabb (vec3 2 2 2) (vec3 3 3 3))]
         [merged (aabb-merge b1 b2)])
        (assert (vec3-approx= (aabb-min merged) (vec3 0 0 0)))
        (assert (vec3-approx= (aabb-max merged) (vec3 3 3 3)))))

(define-test "aabb from points"
  (let* ([points (list (vec3 1 2 3)
                       (vec3 -1 5 2)
                       (vec3 3 -1 4))]
         [box (aabb-from-points points)])
        (assert (vec3-approx= (aabb-min box) (vec3 -1 -1 2)))
        (assert (vec3-approx= (aabb-max box) (vec3 3 5 4)))))

(define-test "aabb from empty list"
  (let ([box (aabb-from-points '())])
       ;; Empty AABB should have min > max (null/empty bounding box)
       (assert (> (vec3-x (aabb-min box)) (vec3-x (aabb-max box))))
       (assert (> (vec3-y (aabb-min box)) (vec3-y (aabb-max box))))
       (assert (> (vec3-z (aabb-min box)) (vec3-z (aabb-max box))))))

(define-test "aabb empty merge does not pollute"
  ;; Regression test: merging empty AABB should not affect non-empty one
  (let* ([empty-box (aabb-from-points '())]
         [points-box (aabb-from-points (list (vec3 5 5 5) (vec3 10 10 10)))]
         [merged (aabb-merge empty-box points-box)])
        ;; Merged should equal points-box (empty doesn't contribute)
        (assert (vec3-approx= (aabb-min merged) (vec3 5 5 5)))
        (assert (vec3-approx= (aabb-max merged) (vec3 10 10 10)))))

;;; ============================================================
;;; Coordinate Conversion Tests
;;; ============================================================

(define-test "cartesian to spherical and back"
  (let* ([v (vec3 1 1 1)]
         [sph (vec3-to-spherical v)]
         [r (car sph)]
         [theta (cadr sph)]
         [phi (caddr sph)]
         [v2 (vec3-from-spherical r theta phi)])
        (assert (vec3-approx= v v2))))

(define-test "cartesian to cylindrical and back"
  (let* ([v (vec3 1 1 2)]
         [cyl (vec3-to-cylindrical v)]
         [r (car cyl)]
         [theta (cadr cyl)]
         [z (caddr cyl)]
         [v2 (vec3-from-cylindrical r theta z)])
        (assert (vec3-approx= v v2))))

;;; ============================================================
;;; Tests auto-run when defined
;;; ============================================================
