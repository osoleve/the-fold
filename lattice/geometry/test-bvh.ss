;;; core/geometry/test-bvh.ss — Tests for BVH implementation
;;;
;;; Run: scheme --script core/geometry/test-bvh.ss

(load "core/test-framework.ss")
(load "lattice/geometry/bvh.ss")

(set! *current-group* 'bvh)

;;; Helper for approximate equality
(define (approx= a b eps)
  (< (abs (- a b)) eps))

;;; ============================================================
;;; BVH Construction Tests
;;; ============================================================

(define-test "BVH construction from single triangle"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [bvh (bvh-build (list tri) 1)])
        (assert-true (bvh-leaf? bvh))
        (assert-equal (length (bvh-primitives bvh)) 1)))

(define-test "BVH construction from multiple triangles"
  (let* ([tri1 (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [tri2 (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0))]
         [tri3 (triangle3 (vec3 4 0 0) (vec3 5 0 0) (vec3 4 1 0))]
         [bvh (bvh-build (list tri1 tri2 tri3) 1)])
        (assert-true (bvh-node? bvh))
        (assert-true (>= (bvh-depth bvh) 2))
        (assert-equal (bvh-count-triangles bvh) 3)))

(define-test "BVH ray intersection - hit"
  (let* ([tri (triangle3 (vec3 -1 -1 0) (vec3 1 -1 0) (vec3 0 1 0))]
         [bvh (bvh-build (list tri) 1)]
         [ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [result (bvh-intersect-ray bvh ray)])
        (assert-false (equal? result #f))
        (assert-true (> (cadr result) 0))))

(define-test "BVH ray intersection - miss"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [bvh (bvh-build (list tri) 1)]
         [ray (ray3 (vec3 10 10 -5) (vec3 0 0 1))]
         [result (bvh-intersect-ray bvh ray)])
        (assert-false result)))

(define-test "BVH closest point - on surface"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [bvh (bvh-build (list tri) 1)]
         [point (vec3 0.25 0.25 0)]
         [result (bvh-closest-point bvh point)])
        (assert-false (equal? result #f))
        (assert-true (< (cadr result) 0.01))))

(define-test "BVH closest point - above surface"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [bvh (bvh-build (list tri) 1)]
         [point (vec3 0.25 0.25 1)]
         [result (bvh-closest-point bvh point)])
        (assert-false (equal? result #f))
        (assert-true (approx= (cadr result) 1.0 0.01))))

(define-test "BVH statistics"
  (let* ([tri1 (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [tri2 (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0))]
         [tri3 (triangle3 (vec3 4 0 0) (vec3 5 0 0) (vec3 4 1 0))]
         [tri4 (triangle3 (vec3 6 0 0) (vec3 7 0 0) (vec3 6 1 0))]
         [bvh (bvh-build (list tri1 tri2 tri3 tri4) 2)])
        (assert-true (>= (bvh-count-nodes bvh) 3))
        (assert-true (>= (bvh-count-leaves bvh) 2))
        (assert-equal (bvh-count-triangles bvh) 4)))

(define-test "Closest point on triangle - interior"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 2 0 0) (vec3 0 2 0))]
         [point (vec3 0.5 0.5 1)]
         [closest (closest-point-on-triangle point tri)])
        (assert-true (approx= (vec3-z closest) 0.0 0.01))
        (assert-true (and (>= (vec3-x closest) 0)
                          (>= (vec3-y closest) 0)
                          (<= (+ (vec3-x closest) (vec3-y closest)) 2)))))

(define-test "BVH with many triangles"
  (let* ([triangles
          (map (lambda (i)
                       (triangle3 (vec3 (* i 1.0) 0 0)
                                  (vec3 (+ (* i 1.0) 0.5) 0.5 0)
                                  (vec3 (* i 1.0) 1 0)))
               (iota 100))]
         [bvh (bvh-build triangles 5)])
        (assert-equal (bvh-count-triangles bvh) 100)
        (assert-true (>= (bvh-depth bvh) 4))
        (assert-true (< (bvh-depth bvh) 20))))

(define-test "BVH bbox encapsulates all triangles"
  (let* ([tri1 (triangle3 (vec3 -5 -5 -5) (vec3 -4 -5 -5) (vec3 -5 -4 -5))]
         [tri2 (triangle3 (vec3 5 5 5) (vec3 6 5 5) (vec3 5 6 5))]
         [bvh (bvh-build (list tri1 tri2) 5)]
         [bbox (bvh-bbox bvh)]
         [bmin (aabb-min bbox)]
         [bmax (aabb-max bbox)])
        (assert-true (<= (vec3-x bmin) -5))
        (assert-true (<= (vec3-y bmin) -5))
        (assert-true (<= (vec3-z bmin) -5))
        (assert-true (>= (vec3-x bmax) 6))
        (assert-true (>= (vec3-y bmax) 6))
        (assert-true (>= (vec3-z bmax) 5))))

(define-test "Longest axis selection"
  (let* ([bbox1 (aabb (vec3 0 0 0) (vec3 10 1 1))]
         [bbox2 (aabb (vec3 0 0 0) (vec3 1 10 1))]
         [bbox3 (aabb (vec3 0 0 0) (vec3 1 1 10))])
        (assert-equal (longest-axis bbox1) 0)
        (assert-equal (longest-axis bbox2) 1)
        (assert-equal (longest-axis bbox3) 2)))

;;; ============================================================
;;; Summary
;;; ============================================================

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
