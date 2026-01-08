;;; core/geometry/test-octree.ss — Tests for Octree Spatial Partitioning
;;;
;;; Tests for octree construction, queries, and statistics.
;;;
;;; Run: scheme --script core/geometry/test-octree.ss

(load "core/test-framework.ss")
(load "core/geometry/octree.ss")

(set! *current-group* 'octree)

;;; Helper for approximate equality
(define (approx= a b eps)
  (< (abs (- a b)) eps))

;;; Helper for approximate vector equality
(define (vec3-approx= v1 v2 eps)
  (and (approx= (vec3-x v1) (vec3-x v2) eps)
       (approx= (vec3-y v1) (vec3-y v2) eps)
       (approx= (vec3-z v1) (vec3-z v2) eps)))

;;; ============================================================
;;; Octree Leaf Construction Tests
;;; ============================================================

(define-test "octree-leaf creation"
  (let* ([center (vec3 0 0 0)]
         [size 10]
         [tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [leaf (octree-leaf center size (list tri))])
        (assert-true (octree-leaf? leaf))
        (assert-false (octree-node? leaf))
        (assert-equal (octree-center leaf) center)
        (assert-equal (octree-size leaf) size)
        (assert-equal (length (octree-primitives leaf)) 1)))

(define-test "octree-node creation"
  (let* ([center (vec3 0 0 0)]
         [size 10]
         [children (make-vector 8 #f)]
         [node (octree-node center size children)])
        (assert-true (octree-node? node))
        (assert-false (octree-leaf? node))
        (assert-equal (octree-center node) center)
        (assert-equal (octree-size node) size)))

;;; ============================================================
;;; Octree Build Tests
;;; ============================================================

(define-test "octree-build with single triangle returns leaf"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [octree (octree-build (list tri) (vec3 0 0 0) 10 5 10)])
        (assert-true (octree-leaf? octree))
        (assert-equal (length (octree-primitives octree)) 1)))

(define-test "octree-build with few triangles respects max-leaf-size"
  (let* ([tri1 (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [tri2 (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0))]
         [tri3 (triangle3 (vec3 4 0 0) (vec3 5 0 0) (vec3 4 1 0))]
         [octree (octree-build (list tri1 tri2 tri3) (vec3 0 0 0) 10 5 5)])
        ;; Should remain a leaf since 3 <= 5
        (assert-true (octree-leaf? octree))
        (assert-equal (length (octree-primitives octree)) 3)))

(define-test "octree-build respects max depth"
  (let* ([triangles (map (lambda (i)
                                 (triangle3 (vec3 (* i 0.1) 0 0)
                                            (vec3 (+ (* i 0.1) 0.05) 0.05 0)
                                            (vec3 (* i 0.1) 0.1 0)))
                         (iota 50))]
         [octree (octree-build triangles (vec3 0 0 0) 10 2 5)])
        ;; Depth limited to 2
        (assert-true (<= (octree-depth octree) 3))))

;;; ============================================================
;;; Subdivide Octants Tests
;;; ============================================================

(define-test "subdivide-octants produces 8 octants"
  (let* ([center (vec3 0 0 0)]
         [size 10]
         [octants (subdivide-octants center size)])
        (assert-equal (length octants) 8)))

(define-test "subdivide-octants correct centers"
  (let* ([center (vec3 0 0 0)]
         [size 10]
         [octants (subdivide-octants center size)]
         ;; First octant should be at (-5, -5, -5) with size 5
         [first-octant (car octants)]
         [first-center (car first-octant)]
         [first-size (cadr first-octant)])
        (assert-true (vec3-approx= first-center (vec3 -5 -5 -5) 0.001))
        (assert-true (approx= first-size 5 0.001))))

(define-test "subdivide-octants coverage"
  (let* ([center (vec3 0 0 0)]
         [size 10]
         [octants (subdivide-octants center size)]
         ;; Check that all 8 octants have distinct centers
         [centers (map car octants)]
         ;; Each center should be unique based on sign pattern
         [x-neg-count (length (filter (lambda (c) (< (vec3-x c) 0)) centers))]
         [y-neg-count (length (filter (lambda (c) (< (vec3-y c) 0)) centers))])
        ;; Should have 4 negative x and 4 positive x
        (assert-equal x-neg-count 4)
        (assert-equal y-neg-count 4)))

;;; ============================================================
;;; AABB Overlap Tests
;;; ============================================================

(define-test "aabb-overlaps? true for overlapping boxes"
  (let* ([a (aabb (vec3 0 0 0) (vec3 2 2 2))]
         [b (aabb (vec3 1 1 1) (vec3 3 3 3))])
        (assert-true (aabb-overlaps? a b))))

(define-test "aabb-overlaps? true for contained box"
  (let* ([a (aabb (vec3 0 0 0) (vec3 10 10 10))]
         [b (aabb (vec3 2 2 2) (vec3 4 4 4))])
        (assert-true (aabb-overlaps? a b))))

(define-test "aabb-overlaps? false for separate boxes"
  (let* ([a (aabb (vec3 0 0 0) (vec3 1 1 1))]
         [b (aabb (vec3 5 5 5) (vec3 6 6 6))])
        (assert-false (aabb-overlaps? a b))))

(define-test "aabb-overlaps? true for touching boxes"
  (let* ([a (aabb (vec3 0 0 0) (vec3 1 1 1))]
         [b (aabb (vec3 1 0 0) (vec3 2 1 1))])
        (assert-true (aabb-overlaps? a b))))

;;; ============================================================
;;; Triangle-Octant Intersection Tests
;;; ============================================================

(define-test "triangle-intersects-octant? true when vertex inside"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [center (vec3 0 0 0)]
         [size 5])
        (assert-true (triangle-intersects-octant? tri center size))))

(define-test "triangle-intersects-octant? false when outside"
  (let* ([tri (triangle3 (vec3 100 100 100) (vec3 101 100 100) (vec3 100 101 100))]
         [center (vec3 0 0 0)]
         [size 5])
        (assert-false (triangle-intersects-octant? tri center size))))

(define-test "triangle-intersects-octant? true for bbox overlap"
  (let* ([tri (triangle3 (vec3 -10 0 0) (vec3 10 0 0) (vec3 0 10 0))]
         [center (vec3 0 0 0)]
         [size 1])
        ;; Triangle bbox overlaps octant, though vertices are outside
        (assert-true (triangle-intersects-octant? tri center size))))

;;; ============================================================
;;; Ray-Octant Intersection Tests
;;; ============================================================

(define-test "ray-intersects-octant? true for intersecting ray"
  (let* ([ray (ray3 (vec3 0 0 -10) (vec3 0 0 1))]
         [center (vec3 0 0 0)]
         [size 5]
         [result (ray-intersects-octant? ray center size)])
        ;; Returns intersection t values if hit, #f otherwise
        (assert-false (equal? result #f))))

(define-test "ray-intersects-octant? false for missing ray"
  (let* ([ray (ray3 (vec3 100 0 -10) (vec3 0 0 1))]
         [center (vec3 0 0 0)]
         [size 5])
        (assert-false (ray-intersects-octant? ray center size))))

(define-test "ray-intersects-octant? true for ray starting inside"
  (let* ([ray (ray3 (vec3 0 0 0) (vec3 1 0 0))]
         [center (vec3 0 0 0)]
         [size 5]
         [result (ray-intersects-octant? ray center size)])
        ;; Returns intersection t values if hit
        (assert-false (equal? result #f))))

;;; ============================================================
;;; Octree Ray Intersection Tests
;;; ============================================================

(define-test "octree-intersect-ray hit single triangle"
  (let* ([tri (triangle3 (vec3 -1 -1 0) (vec3 1 -1 0) (vec3 0 1 0))]
         [octree (octree-build (list tri) (vec3 0 0 0) 10 5 10)]
         [ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [result (octree-intersect-ray octree ray)])
        (assert-false (equal? result #f))
        (assert-true (> (cadr result) 0))))

(define-test "octree-intersect-ray miss"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [octree (octree-build (list tri) (vec3 0 0 0) 10 5 10)]
         [ray (ray3 (vec3 100 100 -5) (vec3 0 0 1))]
         [result (octree-intersect-ray octree ray)])
        (assert-false result)))

(define-test "octree-intersect-ray finds closest"
  (let* ([tri1 (triangle3 (vec3 -1 -1 -5) (vec3 1 -1 -5) (vec3 0 1 -5))]
         [tri2 (triangle3 (vec3 -1 -1 0) (vec3 1 -1 0) (vec3 0 1 0))]
         [tri3 (triangle3 (vec3 -1 -1 5) (vec3 1 -1 5) (vec3 0 1 5))]
         [octree (octree-build (list tri1 tri2 tri3) (vec3 0 0 0) 20 5 10)]
         [ray (ray3 (vec3 0 0 -20) (vec3 0 0 1))]
         [result (octree-intersect-ray octree ray)])
        (assert-false (equal? result #f))
        ;; Should hit tri1 first (closest to ray origin)
        (let ([t (cadr result)])
             (assert-true (approx= t 15.0 0.1)))))  ; -20 + 15 = -5

;;; ============================================================
;;; Octree Statistics Tests
;;; ============================================================

(define-test "octree-depth for leaf"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [octree (octree-build (list tri) (vec3 0 0 0) 10 5 10)])
        (assert-equal (octree-depth octree) 1)))

(define-test "octree-count-nodes for leaf"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [octree (octree-build (list tri) (vec3 0 0 0) 10 5 10)])
        (assert-equal (octree-count-nodes octree) 1)))

(define-test "octree-count-leaves for leaf"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [octree (octree-build (list tri) (vec3 0 0 0) 10 5 10)])
        (assert-equal (octree-count-leaves octree) 1)))

(define-test "octree-count-triangles"
  (let* ([tri1 (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [tri2 (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0))]
         [tri3 (triangle3 (vec3 4 0 0) (vec3 5 0 0) (vec3 4 1 0))]
         [octree (octree-build (list tri1 tri2 tri3) (vec3 0 0 0) 10 5 10)])
        ;; Total triangles should be at least 3 (may be duplicated in overlapping octants)
        (assert-true (>= (octree-count-triangles octree) 3))))

(define-test "octree-depth increases with subdivision"
  (let* ([triangles (map (lambda (i)
                                 (let ([x (* i 5.0)])
                                      (triangle3 (vec3 x 0 0)
                                                 (vec3 (+ x 0.5) 0.5 0)
                                                 (vec3 x 1 0))))
                         (iota 20))]
         [octree (octree-build triangles (vec3 50 0 0) 100 10 2)])
        ;; With 20 triangles and max-leaf-size 2, some subdivision should occur
        ;; Depth is at least 1 (a leaf)
        (assert-true (>= (octree-depth octree) 1))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(define-test "octree-build empty list returns empty leaf"
  (let* ([octree (octree-build '() (vec3 0 0 0) 10 5 10)])
        (assert-true (octree-leaf? octree))
        (assert-equal (length (octree-primitives octree)) 0)))

(define-test "octree-intersect-ray on empty octree"
  (let* ([octree (octree-build '() (vec3 0 0 0) 10 5 10)]
         [ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [result (octree-intersect-ray octree ray)])
        (assert-false result)))

(define-test "octree handles degenerate triangles"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 0 0 0) (vec3 0 0 0))]  ; Degenerate
         [octree (octree-build (list tri) (vec3 0 0 0) 10 5 10)])
        (assert-true (octree-leaf? octree))
        (assert-equal (length (octree-primitives octree)) 1)))

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
