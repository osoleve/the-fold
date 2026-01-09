;;; user/physics3d/test-collision-detection3d.ss --- Tests for collision-detection3d
;;;
;;; Run with: scheme --script user/physics3d/test-collision-detection3d.ss

(load "core/test-framework.ss")
(load "lattice/physics/classical3d/collision-detection3d.ss")

(display "\n")
(display "==============================================================\n")
(display "         3D COLLISION DETECTION TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Point-in-Shape Tests
;;; ============================================================

(test-group point-in-shape-tests
            
            (define-test point-in-aabb3d-inside
              (let ([aabb (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))])
                   (assert-true (point-in-aabb3d? (vec3 0 0 0) aabb))))
            
            (define-test point-in-aabb3d-outside
              (let ([aabb (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))])
                   (assert-false (point-in-aabb3d? (vec3 2 0 0) aabb))))
            
            (define-test point-in-aabb3d-on-edge
              (let ([aabb (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))])
                   (assert-true (point-in-aabb3d? (vec3 1 0 0) aabb))))
            
            (define-test point-in-sphere3d-inside
              (let ([sphere (sphere3d (vec3 0 0 0) 2)])
                   (assert-true (point-in-sphere3d? (vec3 1 0 0) sphere))))
            
            (define-test point-in-sphere3d-outside
              (let ([sphere (sphere3d (vec3 0 0 0) 2)])
                   (assert-false (point-in-sphere3d? (vec3 3 0 0) sphere))))
            
            (define-test point-in-box3d-inside
              (let ([box (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-true (point-in-box3d? (vec3 0.5 0.5 0.5) box))))
            
            (define-test point-in-box3d-outside
              (let ([box (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-false (point-in-box3d? (vec3 2 0 0) box)))))

;;; ============================================================
;;; Closest Point Tests
;;; ============================================================

(test-group closest-point-tests
            
            (define-test closest-point-on-segment-middle
              (let* ([v1 (vec3 0 0 0)]
                     [v2 (vec3 10 0 0)]
                     [point (vec3 5 5 0)]
                     [closest (closest-point-on-segment-3d point v1 v2)])
                    (assert-true (< (vec3-distance closest (vec3 5 0 0)) 1e-6))))
            
            (define-test closest-point-on-segment-endpoint
              (let* ([v1 (vec3 0 0 0)]
                     [v2 (vec3 10 0 0)]
                     [point (vec3 -5 0 0)]
                     [closest (closest-point-on-segment-3d point v1 v2)])
                    (assert-true (< (vec3-distance closest v1) 1e-6))))
            
            (define-test closest-point-on-aabb-outside
              (let* ([aabb (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))]
                     [point (vec3 3 0 0)]
                     [closest (closest-point-on-aabb3d point aabb)])
                    (assert-true (< (vec3-distance closest (vec3 1 0 0)) 1e-6))))
            
            (define-test closest-point-on-aabb-inside
              (let* ([aabb (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))]
                     [point (vec3 0.5 0 0)]
                     [closest (closest-point-on-aabb3d point aabb)])
                    (assert-true (< (vec3-distance closest point) 1e-6))))
            
            (define-test closest-point-on-box-outside
              (let* ([box (box3d (vec3 0 0 0) (vec3 1 1 1))]
                     [point (vec3 5 0 0)]
                     [closest (closest-point-on-box3d point box)])
                    (assert-true (< (vec3-distance closest (vec3 1 0 0)) 1e-6)))))

;;; ============================================================
;;; Sphere-Sphere Collision Tests
;;; ============================================================

(test-group sphere-sphere-tests
            
            (define-test sphere-sphere-separated
              (let ([a (sphere3d (vec3 0 0 0) 1)]
                    [b (sphere3d (vec3 5 0 0) 1)])
                   (assert-false (sphere-sphere? a b))
                   (assert-false (sphere-sphere-manifold a b))))
            
            (define-test sphere-sphere-touching
              (let ([a (sphere3d (vec3 0 0 0) 1)]
                    [b (sphere3d (vec3 2 0 0) 1)])
                   (assert-true (sphere-sphere? a b))))
            
            (define-test sphere-sphere-overlapping
              (let* ([a (sphere3d (vec3 0 0 0) 1)]
                     [b (sphere3d (vec3 1.5 0 0) 1)]
                     [m (sphere-sphere-manifold a b)])
                    (assert-true (pair? m))
                    ;; Normal should point from A to B (in +x direction)
                    (assert-true (> (vec3-x (manifold-normal m)) 0.9))
                    ;; Penetration should be (1 + 1) - 1.5 = 0.5
                    (assert-true (< (abs (- (manifold-penetration m) 0.5)) 1e-6))))
            
            (define-test sphere-sphere-concentric
              (let* ([a (sphere3d (vec3 0 0 0) 1)]
                     [b (sphere3d (vec3 0 0 0) 0.5)]
                     [m (sphere-sphere-manifold a b)])
                    (assert-true (pair? m))
                    ;; Should have some arbitrary normal (we use (1, 0, 0))
                    (assert-equal 1 (vec3-x (manifold-normal m))))))

;;; ============================================================
;;; AABB-AABB Collision Tests
;;; ============================================================

(test-group aabb-aabb-tests
            
            (define-test aabb-aabb-separated
              (let ([a (aabb3d (vec3 0 0 0) (vec3 1 1 1))]
                    [b (aabb3d (vec3 5 5 5) (vec3 6 6 6))])
                   (assert-false (aabb3d-aabb3d? a b))
                   (assert-false (aabb3d-aabb3d-manifold a b))))
            
            (define-test aabb-aabb-touching
              (let ([a (aabb3d (vec3 0 0 0) (vec3 1 1 1))]
                    [b (aabb3d (vec3 1 0 0) (vec3 2 1 1))])
                   (assert-true (aabb3d-aabb3d? a b))))
            
            (define-test aabb-aabb-overlapping
              (let* ([a (aabb3d (vec3 0 0 0) (vec3 2 2 2))]
                     [b (aabb3d (vec3 1.5 0 0) (vec3 3 2 2))]
                     [m (aabb3d-aabb3d-manifold a b)])
                    (assert-true (pair? m))
                    ;; X-axis should have minimum overlap (0.5)
                    (assert-true (< (abs (- (manifold-penetration m) 0.5)) 1e-6))
                    ;; Normal should be in +x direction
                    (assert-equal 1 (vec3-x (manifold-normal m)))))
            
            (define-test aabb-aabb-y-axis-min
              (let* ([a (aabb3d (vec3 0 0 0) (vec3 2 2 2))]
                     [b (aabb3d (vec3 0 1.8 0) (vec3 2 4 2))]
                     [m (aabb3d-aabb3d-manifold a b)])
                    (assert-true (pair? m))
                    ;; Y-axis should have minimum overlap (0.2)
                    (assert-true (< (abs (- (manifold-penetration m) 0.2)) 1e-6))
                    ;; Normal should be in +y direction
                    (assert-equal 1 (vec3-y (manifold-normal m))))))

;;; ============================================================
;;; Sphere-AABB Collision Tests
;;; ============================================================

(test-group sphere-aabb-tests
            
            (define-test sphere-aabb-separated
              (let ([sphere (sphere3d (vec3 5 0 0) 1)]
                    [aabb (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))])
                   (assert-false (sphere-aabb3d? sphere aabb))
                   (assert-false (sphere-aabb3d-manifold sphere aabb))))
            
            (define-test sphere-aabb-touching
              (let ([sphere (sphere3d (vec3 2 0 0) 1)]
                    [aabb (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))])
                   (assert-true (sphere-aabb3d? sphere aabb))))
            
            (define-test sphere-aabb-overlapping
              (let* ([sphere (sphere3d (vec3 1.5 0 0) 1)]
                     [aabb (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))]
                     [m (sphere-aabb3d-manifold sphere aabb)])
                    (assert-true (pair? m))
                    ;; Normal should point from AABB to sphere (in +x direction)
                    (assert-true (> (vec3-x (manifold-normal m)) 0.9))
                    ;; Penetration should be 1 - 0.5 = 0.5
                    (assert-true (< (abs (- (manifold-penetration m) 0.5)) 1e-6))))
            
            (define-test sphere-inside-aabb
              (let* ([sphere (sphere3d (vec3 0.5 0 0) 0.2)]
                     [aabb (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))]
                     [m (sphere-aabb3d-manifold sphere aabb)])
                    (assert-true (pair? m))
                    ;; Sphere center is inside, so closest point is center
                    ;; Normal should point toward nearest face
                    (assert-true (= (abs (vec3-x (manifold-normal m))) 1)))))

;;; ============================================================
;;; Sphere-Box Collision Tests
;;; ============================================================

(test-group sphere-box-tests
            
            (define-test sphere-box-separated
              (let ([sphere (sphere3d (vec3 5 0 0) 1)]
                    [box (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-false (sphere-box3d? sphere box))))
            
            (define-test sphere-box-overlapping
              (let* ([sphere (sphere3d (vec3 1.5 0 0) 1)]
                     [box (box3d (vec3 0 0 0) (vec3 1 1 1))]
                     [m (sphere-box3d-manifold sphere box)])
                    (assert-true (pair? m))
                    ;; Normal should point from box to sphere
                    (assert-true (> (vec3-x (manifold-normal m)) 0.9)))))

;;; ============================================================
;;; Box-Box Collision Tests
;;; ============================================================

(test-group box-box-tests
            
            (define-test box-box-separated
              (let ([a (box3d (vec3 0 0 0) (vec3 1 1 1))]
                    [b (box3d (vec3 5 0 0) (vec3 1 1 1))])
                   (assert-false (box3d-box3d? a b))))
            
            (define-test box-box-overlapping
              (let* ([a (box3d (vec3 0 0 0) (vec3 1 1 1))]
                     [b (box3d (vec3 1.5 0 0) (vec3 1 1 1))]
                     [m (box3d-box3d-manifold a b)])
                    (assert-true (pair? m))
                    ;; Normal should be in +x direction
                    (assert-equal 1 (vec3-x (manifold-normal m)))
                    ;; Penetration should be (1 + 1) - 1.5 = 0.5
                    (assert-true (< (abs (- (manifold-penetration m) 0.5)) 1e-6)))))

;;; ============================================================
;;; Generic Shape Dispatch Tests
;;; ============================================================

(test-group shape-dispatch-tests
            
            (define-test shape-type-sphere
              (let ([s (sphere3d (vec3 0 0 0) 1)])
                   (assert-equal 'sphere3d (shape-type-3d s))))
            
            (define-test shape-type-box
              (let ([b (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-equal 'box3d (shape-type-3d b))))
            
            (define-test shape-type-aabb
              (let ([a (aabb3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-equal 'aabb3d (shape-type-3d a))))
            
            (define-test shapes-collide-sphere-sphere
              (let ([a (sphere3d (vec3 0 0 0) 1)]
                    [b (sphere3d (vec3 1.5 0 0) 1)])
                   (assert-true (shapes-collide-3d? a b))))
            
            (define-test shapes-collide-sphere-aabb
              (let ([s (sphere3d (vec3 1.5 0 0) 1)]
                    [a (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))])
                   (assert-true (shapes-collide-3d? s a))
                   (assert-true (shapes-collide-3d? a s))))  ; Test symmetry
            
            (define-test shapes-manifold-sphere-sphere
              (let* ([a (sphere3d (vec3 0 0 0) 1)]
                     [b (sphere3d (vec3 1.5 0 0) 1)]
                     [m (shapes-manifold-3d a b)])
                    (assert-true (pair? m))
                    (assert-true (< (abs (- (manifold-penetration m) 0.5)) 1e-6))))
            
            (define-test shapes-manifold-reverses-normal
              (let* ([s (sphere3d (vec3 1.5 0 0) 1)]
                     [a (aabb3d (vec3 -1 -1 -1) (vec3 1 1 1))]
                     [m1 (shapes-manifold-3d s a)]
                     [m2 (shapes-manifold-3d a s)])
                    (assert-true (pair? m1))
                    (assert-true (pair? m2))
                    ;; Normals should be opposite
                    (assert-true (< (abs (+ (vec3-x (manifold-normal m1))
                                            (vec3-x (manifold-normal m2)))) 1e-6)))))

;;; ============================================================
;;; Spatial Hash Tests
;;; ============================================================

(test-group spatial-hash-tests
            
            (define-test spatial-hash-insert-query
              (let* ([sh (make-spatial-hash-3d 2.0)]
                     [obj 'test-object]
                     [aabb (aabb3d (vec3 0 0 0) (vec3 1 1 1))])
                    (spatial-hash-3d-insert! sh obj aabb)
                    (let ([results (spatial-hash-3d-query sh aabb)])
                         (assert-true (if (memq obj results) #t #f)))))
            
            (define-test spatial-hash-no-false-positives
              (let* ([sh (make-spatial-hash-3d 2.0)]
                     [obj 'test-object]
                     [aabb1 (aabb3d (vec3 0 0 0) (vec3 1 1 1))]
                     [aabb2 (aabb3d (vec3 10 10 10) (vec3 11 11 11))])
                    (spatial-hash-3d-insert! sh obj aabb1)
                    (let ([results (spatial-hash-3d-query sh aabb2)])
                         (assert-false (memq obj results)))))
            
            (define-test spatial-hash-clear
              (let* ([sh (make-spatial-hash-3d 2.0)]
                     [obj 'test-object]
                     [aabb (aabb3d (vec3 0 0 0) (vec3 1 1 1))])
                    (spatial-hash-3d-insert! sh obj aabb)
                    (spatial-hash-3d-clear! sh)
                    (let ([results (spatial-hash-3d-query sh aabb)])
                         (assert-equal 0 (length results)))))
            
            (define-test point-to-cell-3d-basic
              (let ([cell (point-to-cell-3d (vec3 5.5 3.2 7.8) 2.0)])
                   (assert-equal 2 (car cell))    ; floor(5.5/2) = 2
                   (assert-equal 1 (cadr cell))   ; floor(3.2/2) = 1
                   (assert-equal 3 (caddr cell))))) ; floor(7.8/2) = 3

;;; ============================================================
;;; Collision Pair Generation Tests
;;; ============================================================

(test-group collision-pair-tests
            
            (define-test find-collision-pairs-overlapping
              (let* ([sh (make-spatial-hash-3d 2.0)]
                     [s1 (sphere3d (vec3 0 0 0) 1)]
                     [s2 (sphere3d (vec3 1 0 0) 1)]  ; Overlapping with s1
                     [s3 (sphere3d (vec3 10 10 10) 1)]  ; Far away
                     [items (list (cons 'a s1) (cons 'b s2) (cons 'c s3))]
                     [pairs (find-collision-pairs-3d items sh)])
                    ;; Should find pair involving a and b
                    (assert-true (> (length pairs) 0))
                    ;; Check that we found a,b pair (order may vary)
                    (let ([cars (map car pairs)]
                          [cdrs (map cdr pairs)])
                         (assert-true (or (and (if (memq 'a cars) #t #f) (if (memq 'b cdrs) #t #f))
                                          (and (if (memq 'b cars) #t #f) (if (memq 'a cdrs) #t #f)))))))
            
            (define-test find-collision-pairs-no-duplicates
              (let* ([sh (make-spatial-hash-3d 2.0)]
                     [s1 (sphere3d (vec3 0 0 0) 1)]
                     [s2 (sphere3d (vec3 0.5 0 0) 1)]
                     [items (list (cons 'a s1) (cons 'b s2))]
                     [pairs (find-collision-pairs-3d items sh)])
                    ;; Should only have one pair, not both (a,b) and (b,a)
                    (assert-equal 1 (length pairs)))))

;;; ============================================================
;;; Manifold Accessor Tests
;;; ============================================================

(test-group manifold-accessor-tests
            
            (define-test manifold-accessors
              (let ([m (list (vec3 1 0 0) 0.5 (vec3 0 0 0))])
                   (assert-true (vec3? (manifold-normal m)))
                   (assert-equal 0.5 (manifold-penetration m))
                   (assert-true (vec3? (manifold-contact m))))))

;;; ============================================================
;;; Run all tests
;;; ============================================================

(run-all-tests)
