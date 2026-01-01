;;; fabric/stitches/physics-2d/test-collision-detection.ss — Tests for 2D Collision Detection

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "user/physics/collision-detection.ss")

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

;;; Helper for vec2 comparison
(define (assert-vec2-= actual expected tolerance)
  (assert-= (vec2-x actual) (vec2-x expected) tolerance)
  (assert-= (vec2-y actual) (vec2-y expected) tolerance))

(display "
══════════════════════════════════════════════════════════
         2D COLLISION DETECTION TESTS
══════════════════════════════════════════════════════════
")

;;; ============================================================
;;; AABB Tests
;;; ============================================================

(test-group aabb-tests
            (define-test make-aabb-test
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-true (aabb? a))
                   (assert-vec2-= (aabb-min a) (vec2 0 0) 0.0001)
                   (assert-vec2-= (aabb-max a) (vec2 10 10) 0.0001)))
            
            (define-test aabb-center-test
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-vec2-= (aabb-center a) (vec2 5 5) 0.0001)))
            
            (define-test aabb-half-extents-test
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-vec2-= (aabb-half-extents a) (vec2 5 5) 0.0001)))
            
            (define-test aabb-from-center-test
              (let ([a (make-aabb-center (vec2 5 5) (vec2 5 5))])
                   (assert-vec2-= (aabb-min a) (vec2 0 0) 0.0001)
                   (assert-vec2-= (aabb-max a) (vec2 10 10) 0.0001)))
            
            (define-test aabb-from-points-test
              (let ([a (make-aabb-from-points
                        (list (vec2 1 2) (vec2 5 1) (vec2 3 8)))])
                   (assert-vec2-= (aabb-min a) (vec2 1 1) 0.0001)
                   (assert-vec2-= (aabb-max a) (vec2 5 8) 0.0001))))

;;; ============================================================
;;; Circle Tests
;;; ============================================================

(test-group circle-tests
            (define-test make-circle-test
              (let ([c (make-circle (vec2 5 5) 3)])
                   (assert-true (circle? c))
                   (assert-vec2-= (circle-center c) (vec2 5 5) 0.0001)
                   (assert-= (circle-radius c) 3 0.0001)))
            
            (define-test circle-aabb-test
              (let* ([c (make-circle (vec2 5 5) 2)]
                     [a (circle-aabb c)])
                    (assert-vec2-= (aabb-min a) (vec2 3 3) 0.0001)
                    (assert-vec2-= (aabb-max a) (vec2 7 7) 0.0001))))

;;; ============================================================
;;; Polygon Tests
;;; ============================================================

(test-group polygon-tests
            (define-test make-polygon-test
              (let ([p (make-polygon (list (vec2 0 0) (vec2 4 0) (vec2 4 3) (vec2 0 3)))])
                   (assert-true (polygon? p))
                   (assert-= (polygon-vertex-count p) 4 0)))
            
            (define-test polygon-vertex-test
              (let ([p (make-polygon (list (vec2 0 0) (vec2 4 0) (vec2 4 3)))])
                   (assert-vec2-= (polygon-vertex p 0) (vec2 0 0) 0.0001)
                   (assert-vec2-= (polygon-vertex p 1) (vec2 4 0) 0.0001)
                   ;; Wrap around
                   (assert-vec2-= (polygon-vertex p 3) (vec2 0 0) 0.0001)))
            
            (define-test make-box-test
              (let ([b (make-box (vec2 5 5) (vec2 2 2))])
                   (assert-= (polygon-vertex-count b) 4 0)
                   (assert-vec2-= (polygon-vertex b 0) (vec2 3 3) 0.0001)
                   (assert-vec2-= (polygon-vertex b 2) (vec2 7 7) 0.0001)))
            
            (define-test polygon-aabb-test
              (let* ([p (make-polygon (list (vec2 1 2) (vec2 5 1) (vec2 3 8)))]
                     [a (polygon-aabb p)])
                    (assert-vec2-= (aabb-min a) (vec2 1 1) 0.0001)
                    (assert-vec2-= (aabb-max a) (vec2 5 8) 0.0001)))
            
            (define-test regular-polygon-test
              (let ([hex (make-regular-polygon (vec2 0 0) 1 6)])
                   (assert-= (polygon-vertex-count hex) 6 0))))

;;; ============================================================
;;; Point-in-Shape Tests
;;; ============================================================

(test-group point-in-shape-tests
            (define-test point-in-aabb-test
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-true (point-in-aabb? (vec2 5 5) a))
                   (assert-true (point-in-aabb? (vec2 0 0) a))
                   (assert-true (point-in-aabb? (vec2 10 10) a))
                   (assert-false (point-in-aabb? (vec2 -1 5) a))
                   (assert-false (point-in-aabb? (vec2 11 5) a))))
            
            (define-test point-in-circle-test
              (let ([c (make-circle (vec2 5 5) 3)])
                   (assert-true (point-in-circle? (vec2 5 5) c))
                   (assert-true (point-in-circle? (vec2 7 5) c))
                   (assert-false (point-in-circle? (vec2 9 5) c))))
            
            (define-test point-in-polygon-test
              (let ([p (make-polygon (list (vec2 0 0) (vec2 10 0) (vec2 10 10) (vec2 0 10)))])
                   (assert-true (point-in-polygon? (vec2 5 5) p))
                   (assert-false (point-in-polygon? (vec2 -1 5) p))
                   (assert-false (point-in-polygon? (vec2 15 5) p)))))

;;; ============================================================
;;; AABB vs AABB Tests
;;; ============================================================

(test-group aabb-aabb-tests
            (define-test aabb-aabb-overlap-test
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))]
                    [b (make-aabb (vec2 5 5) (vec2 15 15))])
                   (assert-true (aabb-aabb? a b))))
            
            (define-test aabb-aabb-separate-test
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))]
                    [b (make-aabb (vec2 20 20) (vec2 30 30))])
                   (assert-false (aabb-aabb? a b))))
            
            (define-test aabb-aabb-touch-test
              (let ([a (make-aabb (vec2 0 0) (vec2 10 10))]
                    [b (make-aabb (vec2 10 0) (vec2 20 10))])
                   (assert-true (aabb-aabb? a b))))
            
            (define-test aabb-aabb-manifold-test
              (let* ([a (make-aabb (vec2 0 0) (vec2 10 10))]
                     [b (make-aabb (vec2 8 0) (vec2 18 10))]
                     [m (aabb-aabb-manifold a b)])
                    (assert-true (not (not m)))  ; m is not #f
                    (assert-= (cadr m) 2 0.0001))))  ; penetration

;;; ============================================================
;;; Circle vs Circle Tests
;;; ============================================================

(test-group circle-circle-tests
            (define-test circle-circle-overlap-test
              (let ([a (make-circle (vec2 0 0) 5)]
                    [b (make-circle (vec2 8 0) 5)])
                   (assert-true (circle-circle? a b))))
            
            (define-test circle-circle-separate-test
              (let ([a (make-circle (vec2 0 0) 5)]
                    [b (make-circle (vec2 20 0) 5)])
                   (assert-false (circle-circle? a b))))
            
            (define-test circle-circle-touch-test
              (let ([a (make-circle (vec2 0 0) 5)]
                    [b (make-circle (vec2 10 0) 5)])
                   (assert-true (circle-circle? a b))))
            
            (define-test circle-circle-manifold-test
              (let* ([a (make-circle (vec2 0 0) 5)]
                     [b (make-circle (vec2 8 0) 5)]
                     [m (circle-circle-manifold a b)])
                    (assert-true (not (not m)))
                    ;; Normal should point from a to b
                    (assert-vec2-= (car m) (vec2 1 0) 0.0001)
                    ;; Penetration = 5+5-8 = 2
                    (assert-= (cadr m) 2 0.0001))))

;;; ============================================================
;;; Circle vs AABB Tests
;;; ============================================================

(test-group circle-aabb-tests
            (define-test circle-aabb-overlap-test
              (let ([c (make-circle (vec2 5 5) 3)]
                    [a (make-aabb (vec2 0 0) (vec2 4 10))])
                   (assert-true (circle-aabb? c a))))
            
            (define-test circle-aabb-separate-test
              (let ([c (make-circle (vec2 10 10) 2)]
                    [a (make-aabb (vec2 0 0) (vec2 5 5))])
                   (assert-false (circle-aabb? c a))))
            
            (define-test circle-aabb-inside-test
              ;; Circle center inside AABB
              (let ([c (make-circle (vec2 5 5) 2)]
                    [a (make-aabb (vec2 0 0) (vec2 10 10))])
                   (assert-true (circle-aabb? c a))))
            
            (define-test circle-aabb-manifold-test
              (let* ([c (make-circle (vec2 5 0) 3)]
                     [a (make-aabb (vec2 0 2) (vec2 10 10))]
                     [m (circle-aabb-manifold c a)])
                    (assert-true (not (not m)))
                    ;; Circle is below AABB, should have -y normal
                    (assert-= (vec2-y (car m)) -1 0.0001))))

;;; ============================================================
;;; Circle vs Polygon Tests
;;; ============================================================

(test-group circle-polygon-tests
            (define-test circle-polygon-overlap-test
              (let ([c (make-circle (vec2 5 5) 3)]
                    [p (make-box (vec2 3 3) (vec2 2 2))])
                   (assert-true (circle-polygon? c p))))
            
            (define-test circle-polygon-separate-test
              (let ([c (make-circle (vec2 20 20) 2)]
                    [p (make-box (vec2 5 5) (vec2 2 2))])
                   (assert-false (circle-polygon? c p))))
            
            (define-test circle-inside-polygon-test
              (let ([c (make-circle (vec2 5 5) 1)]
                    [p (make-box (vec2 5 5) (vec2 5 5))])
                   (assert-true (circle-polygon? c p))))
            
            (define-test closest-point-segment-test
              (let ([closest (closest-point-on-segment (vec2 5 5) (vec2 0 0) (vec2 10 0))])
                   (assert-vec2-= closest (vec2 5 0) 0.0001))))

;;; ============================================================
;;; Polygon vs Polygon (SAT) Tests
;;; ============================================================

(test-group polygon-sat-tests
            (define-test sat-overlap-test
              (let ([a (make-box (vec2 0 0) (vec2 2 2))]
                    [b (make-box (vec2 1 1) (vec2 2 2))])
                   (assert-true (polygon-polygon? a b))))
            
            (define-test sat-separate-test
              (let ([a (make-box (vec2 0 0) (vec2 2 2))]
                    [b (make-box (vec2 10 10) (vec2 2 2))])
                   (assert-false (polygon-polygon? a b))))
            
            (define-test sat-touch-edge-test
              (let ([a (make-box (vec2 0 0) (vec2 2 2))]
                    [b (make-box (vec2 4 0) (vec2 2 2))])
                   (assert-true (polygon-polygon? a b))))
            
            (define-test sat-manifold-test
              (let* ([a (make-box (vec2 0 0) (vec2 2 2))]
                     [b (make-box (vec2 3 0) (vec2 2 2))]
                     [m (polygon-polygon-manifold a b)])
                    (assert-true (not (not m)))
                    ;; Penetration should be 1 (overlap on x-axis)
                    (assert-= (cadr m) 1 0.0001)))
            
            (define-test sat-triangle-test
              (let ([a (make-polygon (list (vec2 0 0) (vec2 4 0) (vec2 2 3)))]
                    [b (make-polygon (list (vec2 2 1) (vec2 6 1) (vec2 4 4)))])
                   (assert-true (polygon-polygon? a b)))))

;;; ============================================================
;;; Generic Shape Tests
;;; ============================================================

(test-group generic-shape-tests
            (define-test shape-type-test
              (assert-eq? (shape-type (make-aabb (vec2 0 0) (vec2 1 1))) 'aabb)
              (assert-eq? (shape-type (make-circle (vec2 0 0) 1)) 'circle)
              (assert-eq? (shape-type (make-polygon '())) 'polygon))
            
            (define-test shapes-collide-circle-aabb-test
              (let ([c (make-circle (vec2 5 5) 3)]
                    [a (make-aabb (vec2 0 0) (vec2 4 10))])
                   (assert-true (shapes-collide? c a))
                   (assert-true (shapes-collide? a c))))
            
            (define-test shapes-collide-polygon-circle-test
              (let ([p (make-box (vec2 5 5) (vec2 3 3))]
                    [c (make-circle (vec2 0 5) 4)])
                   (assert-true (shapes-collide? p c))
                   (assert-true (shapes-collide? c p))))
            
            (define-test shapes-manifold-test
              (let* ([a (make-circle (vec2 0 0) 5)]
                     [b (make-circle (vec2 8 0) 5)]
                     [m (shapes-manifold a b)])
                    (assert-true (not (not m))))))

;;; ============================================================
;;; Spatial Hash Tests
;;; ============================================================

(test-group spatial-hash-tests
            (define-test spatial-hash-create-test
              (let ([h (make-spatial-hash 10)])
                   (assert-true (spatial-hash? h))
                   (assert-= (spatial-hash-cell-size h) 10 0.0001)))
            
            (define-test point-to-cell-test
              (let ([cell (point-to-cell (vec2 25 35) 10)])
                   (assert-= (car cell) 2 0)
                   (assert-= (cadr cell) 3 0)))
            
            (define-test aabb-to-cells-test
              (let* ([aabb (make-aabb (vec2 5 5) (vec2 25 25))]
                     [cells (aabb-to-cells aabb 10)])
                    ;; Should cover cells (0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)
                    (assert-= (length cells) 9 0)))
            
            (define-test spatial-hash-insert-query-test
              (let* ([h (make-spatial-hash 10)]
                     [obj1 'object1]
                     [aabb1 (make-aabb (vec2 0 0) (vec2 5 5))])
                    (spatial-hash-insert! h obj1 aabb1)
                    (let ([result (spatial-hash-query h (make-aabb (vec2 2 2) (vec2 8 8)))])
                         (assert-true (not (not (member obj1 result)))))))
            
            (define-test spatial-hash-no-false-positives-test
              (let* ([h (make-spatial-hash 10)]
                     [obj1 'object1]
                     [aabb1 (make-aabb (vec2 0 0) (vec2 5 5))])
                    (spatial-hash-insert! h obj1 aabb1)
                    ;; Query far away
                    (let ([result (spatial-hash-query h (make-aabb (vec2 100 100) (vec2 105 105)))])
                         (assert-false (not (not (member obj1 result))))))))

;;; ============================================================
;;; Projection Tests
;;; ============================================================

(test-group projection-tests
            (define-test project-polygon-test
              (let* ([p (make-box (vec2 5 5) (vec2 2 2))]
                     [proj (project-polygon p (vec2 1 0))])  ; x-axis
                    (assert-= (car proj) 3 0.0001)   ; min
                    (assert-= (cadr proj) 7 0.0001))) ; max
            
            (define-test project-circle-test
              (let* ([c (make-circle (vec2 5 5) 3)]
                     [proj (project-circle c (vec2 1 0))])
                    (assert-= (car proj) 2 0.0001)
                    (assert-= (cadr proj) 8 0.0001)))
            
            (define-test intervals-overlap-test
              (assert-= (intervals-overlap '(0 5) '(3 8)) 2 0.0001)
              ;; For contained interval, min translation distance
              (assert-= (intervals-overlap '(0 10) '(2 8)) 8 0.0001)
              (assert-false (intervals-overlap '(0 5) '(6 10)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-case-tests
            (define-test coincident-circles-test
              ;; Two circles at same position
              (let* ([a (make-circle (vec2 5 5) 3)]
                     [b (make-circle (vec2 5 5) 2)]
                     [m (circle-circle-manifold a b)])
                    (assert-true (not (not m)))
                    ;; Arbitrary normal when coincident
                    (assert-vec2-= (car m) (vec2 1 0) 0.0001)))
            
            (define-test zero-size-aabb-test
              (let ([a (make-aabb (vec2 5 5) (vec2 5 5))]
                    [b (make-aabb (vec2 5 5) (vec2 10 10))])
                   (assert-true (aabb-aabb? a b))))
            
            (define-test degenerate-segment-test
              ;; Segment with same start and end
              (let ([closest (closest-point-on-segment (vec2 5 5) (vec2 0 0) (vec2 0 0))])
                   (assert-vec2-= closest (vec2 0 0) 0.0001))))

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
[SUCCESS] All 2D collision detection tests passed.
")
    (display "
[FAILURE] Some 2D collision detection tests failed.
"))
