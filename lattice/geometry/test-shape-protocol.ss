;;; lattice/geometry/test-shape-protocol.ss — Tests for Shape Protocol System
;;;
;;; Verifies protocol dispatch for geometric shapes and scene operations.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/geometry/shape-protocol.ss")

(test-group "shape-protocol"

  ;; ========== Sphere Tests ==========

  (define-test "sphere-intersect-ray-hit"
    (let* ([s (sphere (vec3 0 0 5) 1.0)]
           [ray (ray3 (vec3 0 0 0) (vec3 0 0 1))]
           [hit (shape-intersect-ray s ray)])
      ;; Should hit at t=4 (front) and t=6 (back)
      (assert-true (pair? hit))
      (assert-true (< (abs (- (car hit) 4.0)) 0.01))))

  (define-test "sphere-intersect-ray-miss"
    (let* ([s (sphere (vec3 0 0 5) 1.0)]
           [ray (ray3 (vec3 10 0 0) (vec3 0 0 1))]  ; Parallel, misses
           [hit (shape-intersect-ray s ray)])
      (assert-false hit)))

  (define-test "sphere-aabb"
    (let* ([s (sphere (vec3 1 2 3) 2.0)]
           [box (shape-aabb s)])
      (assert-equal (vec3 -1.0 0.0 1.0) (aabb-min box))
      (assert-equal (vec3 3.0 4.0 5.0) (aabb-max box))))

  (define-test "sphere-contains-point-inside"
    (let ([s (sphere (vec3 0 0 0) 5.0)])
      (assert-true (shape-contains-point? s (vec3 1 1 1)))))

  (define-test "sphere-contains-point-outside"
    (let ([s (sphere (vec3 0 0 0) 1.0)])
      (assert-false (shape-contains-point? s (vec3 5 5 5)))))

  (define-test "sphere-distance-point-outside"
    (let* ([s (sphere (vec3 0 0 0) 1.0)]
           [dist (shape-distance-point s (vec3 3 0 0))])
      ;; Distance from (3,0,0) to sphere with r=1 at origin = 3-1 = 2
      (assert-true (< (abs (- dist 2.0)) 0.01))))

  (define-test "sphere-distance-point-inside"
    (let* ([s (sphere (vec3 0 0 0) 5.0)]
           [dist (shape-distance-point s (vec3 1 0 0))])
      ;; Inside: distance = |point| - radius = 1 - 5 = -4
      (assert-true (< dist 0))))

  (define-test "sphere-center"
    (let ([s (sphere (vec3 1 2 3) 5.0)])
      (assert-equal (vec3 1 2 3) (shape-center s))))

  (define-test "sphere-volume"
    (let* ([s (sphere (vec3 0 0 0) 1.0)]
           [vol (shape-volume s)])
      ;; V = 4/3 * pi * r^3 ≈ 4.189
      (assert-true (< (abs (- vol 4.18879)) 0.001))))

  (define-test "sphere-normal-at"
    (let* ([s (sphere (vec3 0 0 0) 1.0)]
           [n (shape-normal-at s (vec3 1 0 0))])
      ;; Normal at (1,0,0) should be (1,0,0)
      (assert-true (< (abs (- (vec3-x n) 1.0)) 0.01))
      (assert-true (< (abs (vec3-y n)) 0.01))
      (assert-true (< (abs (vec3-z n)) 0.01))))

  ;; ========== AABB Tests ==========

  (define-test "aabb-intersect-ray-hit"
    (let* ([box (aabb (vec3 -1 -1 4) (vec3 1 1 6))]
           [ray (ray3 (vec3 0 0 0) (vec3 0 0 1))]
           [hit (shape-intersect-ray box ray)])
      (assert-true (pair? hit))
      ;; Should hit at t=4
      (assert-true (< (abs (- (car hit) 4.0)) 0.01))))

  (define-test "aabb-self-is-aabb"
    (let* ([box (aabb (vec3 0 0 0) (vec3 1 1 1))]
           [result (shape-aabb box)])
      (assert-equal box result)))

  (define-test "aabb-contains-point"
    (let ([box (aabb (vec3 0 0 0) (vec3 2 2 2))])
      (assert-true (shape-contains-point? box (vec3 1 1 1)))
      (assert-false (shape-contains-point? box (vec3 5 5 5)))))

  (define-test "aabb-center"
    (let* ([box (aabb (vec3 0 0 0) (vec3 4 6 8))]
           [c (shape-center box)])
      (assert-equal (vec3 2.0 3.0 4.0) c)))

  (define-test "aabb-volume"
    (let* ([box (aabb (vec3 0 0 0) (vec3 2 3 4))]
           [vol (shape-volume box)])
      ;; V = 2 * 3 * 4 = 24
      (assert-equal 24.0 vol)))

  ;; ========== Triangle Tests ==========

  (define-test "triangle-intersect-ray-hit"
    (let* ([tri (triangle3 (vec3 -1 -1 5) (vec3 1 -1 5) (vec3 0 1 5))]
           [ray (ray3 (vec3 0 0 0) (vec3 0 0 1))]
           [hit (shape-intersect-ray tri ray)])
      ;; Should hit at t=5
      (assert-true (number? hit))
      (assert-true (< (abs (- hit 5.0)) 0.01))))

  (define-test "triangle-aabb"
    (let* ([tri (triangle3 (vec3 0.0 0.0 0.0) (vec3 2.0 0.0 0.0) (vec3 1.0 2.0 1.0))]
           [box (shape-aabb tri)])
      (assert-equal (vec3 0.0 0.0 0.0) (aabb-min box))
      (assert-equal (vec3 2.0 2.0 1.0) (aabb-max box))))

  (define-test "triangle-center"
    (let* ([tri (triangle3 (vec3 0 0 0) (vec3 3 0 0) (vec3 0 3 0))]
           [c (shape-center tri)])
      ;; Centroid = (0+3+0)/3, (0+0+3)/3, 0 = (1, 1, 0)
      (assert-true (< (abs (- (vec3-x c) 1.0)) 0.01))
      (assert-true (< (abs (- (vec3-y c) 1.0)) 0.01))))

  (define-test "triangle-surface-area"
    (let* ([tri (triangle3 (vec3 0 0 0) (vec3 2 0 0) (vec3 0 2 0))]
           [area (shape-surface-area tri)])
      ;; Area of right triangle with legs 2 = 0.5 * 2 * 2 = 2
      (assert-true (< (abs (- area 2.0)) 0.01))))

  (define-test "triangle-normal"
    (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
           [n (shape-normal-at tri (vec3 0.5 0.5 0))])
      ;; Normal should be (0, 0, 1) for CCW winding in XY plane
      (assert-true (< (abs (vec3-x n)) 0.01))
      (assert-true (< (abs (vec3-y n)) 0.01))
      (assert-true (< (abs (- (vec3-z n) 1.0)) 0.01))))

  ;; ========== Scene Operations ==========

  (define-test "scene-intersect-ray-finds-closest"
    (let* ([s1 (sphere (vec3 0 0 5) 1.0)]   ; Hit at t ≈ 4
           [s2 (sphere (vec3 0 0 10) 1.0)]  ; Hit at t ≈ 9
           [ray (ray3 (vec3 0 0 0) (vec3 0 0 1))]
           [hit (scene-intersect-ray (list s1 s2) ray)])
      ;; Should hit s1 first
      (assert-true (pair? hit))
      (assert-equal s1 (car hit))
      (assert-true (< (cdr hit) 5.0))))

  (define-test "scene-intersect-ray-miss"
    (let* ([s (sphere (vec3 100 0 0) 1.0)]
           [ray (ray3 (vec3 0 0 0) (vec3 0 0 1))]
           [hit (scene-intersect-ray (list s) ray)])
      (assert-false hit)))

  (define-test "scene-aabb-encloses-all"
    (let* ([s1 (sphere (vec3 0 0 0) 1.0)]
           [s2 (sphere (vec3 10 10 10) 1.0)]
           [box (scene-aabb (list s1 s2))])
      (assert-equal (vec3 -1.0 -1.0 -1.0) (aabb-min box))
      (assert-equal (vec3 11.0 11.0 11.0) (aabb-max box))))

  (define-test "shapes-containing-point"
    (let* ([s1 (sphere (vec3 0 0 0) 5.0)]   ; Contains origin
           [s2 (sphere (vec3 100 0 0) 1.0)] ; Doesn't contain origin
           [s3 (sphere (vec3 0 0 0) 10.0)]  ; Contains origin
           [containing (shapes-containing-point (list s1 s2 s3) (vec3 0 0 0))])
      (assert-equal 2 (length containing))))

  (define-test "closest-shape"
    (let* ([s1 (sphere (vec3 0 0 5) 1.0)]   ; Distance from origin = 4
           [s2 (sphere (vec3 0 0 10) 1.0)]  ; Distance from origin = 9
           [result (closest-shape (list s1 s2) (vec3 0 0 0))])
      (assert-true (pair? result))
      (assert-equal s1 (car result))
      (assert-true (< (abs (- (cdr result) 4.0)) 0.01))))

  (define-test "scene-sdf"
    (let* ([s1 (sphere (vec3 0 0 5) 1.0)]
           [s2 (sphere (vec3 0 0 10) 1.0)]
           [sdf (scene-sdf (list s1 s2) (vec3 0 0 0))])
      ;; Minimum distance should be to s1 = 4
      (assert-true (< (abs (- sdf 4.0)) 0.01))))

  (define-test "scene-total-volume"
    (let* ([s1 (sphere (vec3 0 0 0) 1.0)]   ; V ≈ 4.189
           [s2 (sphere (vec3 10 0 0) 1.0)]  ; V ≈ 4.189
           [box (aabb (vec3 0 0 0) (vec3 1 1 1))]  ; V = 1
           [total (scene-total-volume (list s1 s2 box))])
      ;; Total ≈ 4.189 + 4.189 + 1 ≈ 9.378
      (assert-true (< (abs (- total 9.378)) 0.1))))

  ;; ========== Heterogeneous Scene Test ==========

  (define-test "heterogeneous-scene-raycast"
    ;; Mix of different shape types in one scene
    (let* ([sphere1 (sphere (vec3 0 0 3) 1.0)]         ; Hit at t ≈ 2
           [box1 (aabb (vec3 -0.5 -0.5 6) (vec3 0.5 0.5 8))]  ; Hit at t = 6
           [tri1 (triangle3 (vec3 -2 -2 10) (vec3 2 -2 10) (vec3 0 2 10))]  ; Hit at t = 10
           [scene (list sphere1 box1 tri1)]
           [ray (ray3 (vec3 0 0 0) (vec3 0 0 1))]
           [hit (scene-intersect-ray scene ray)])
      ;; Should hit sphere first
      (assert-true (pair? hit))
      (assert-equal sphere1 (car hit))
      (assert-true (< (cdr hit) 3.0)))))

(run-all-tests)
