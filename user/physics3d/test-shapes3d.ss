;;; user/physics3d/test-shapes3d.ss --- Tests for shapes3d
;;;
;;; Run with: scheme --script user/physics3d/test-shapes3d.ss

(load "core/test-framework.ss")
(load "user/physics3d/shapes3d.ss")

(display "\n")
(display "==============================================================\n")
(display "         3D SHAPES TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Sphere3D Tests
;;; ============================================================

(test-group sphere3d-tests
            
            (define-test sphere-construction
              (let ([s (sphere3d (vec3 1 2 3) 5)])
                   (assert-true (sphere3d? s))
                   (assert-equal 1 (vec3-x (sphere3d-center s)))
                   (assert-equal 5 (sphere3d-radius s))))
            
            (define-test sphere-volume
              (let ([s (sphere3d (vec3 0 0 0) 1)])
                   (assert-true (< (abs (- (sphere3d-volume s) 4.1887902047863905)) 1e-6))))
            
            (define-test sphere-surface-area
              (let ([s (sphere3d (vec3 0 0 0) 1)])
                   (assert-true (< (abs (- (sphere3d-surface-area s) 12.566370614359172)) 1e-6))))
            
            (define-test sphere-contains-point-inside
              (let ([s (sphere3d (vec3 0 0 0) 2)])
                   (assert-true (sphere3d-contains-point? s (vec3 0 0 0)))))
            
            (define-test sphere-contains-point-surface
              (let ([s (sphere3d (vec3 0 0 0) 2)])
                   (assert-true (sphere3d-contains-point? s (vec3 2 0 0)))))
            
            (define-test sphere-contains-point-outside
              (let ([s (sphere3d (vec3 0 0 0) 2)])
                   (assert-false (sphere3d-contains-point? s (vec3 3 0 0)))))
            
            (define-test sphere-aabb
              (let* ([s (sphere3d (vec3 0 0 0) 1)]
                     [aabb (sphere3d-aabb s)])
                    (assert-equal -1 (vec3-x (aabb3d-min aabb)))
                    (assert-equal 1 (vec3-x (aabb3d-max aabb))))))

;;; ============================================================
;;; Box3D Tests
;;; ============================================================

(test-group box3d-tests
            
            (define-test box-construction
              (let ([b (box3d (vec3 0 0 0) (vec3 1 2 3))])
                   (assert-true (box3d? b))
                   (assert-equal 1 (vec3-x (box3d-half-extents b)))
                   (assert-equal 2 (vec3-y (box3d-half-extents b)))))
            
            (define-test box-min-max
              (let ([b (box3d (vec3 0 0 0) (vec3 1 2 3))])
                   (assert-equal -1 (vec3-x (box3d-min b)))
                   (assert-equal 2 (vec3-y (box3d-max b)))
                   (assert-equal -3 (vec3-z (box3d-min b)))))
            
            (define-test box-volume
              (let ([b (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-equal 8 (box3d-volume b))))
            
            (define-test box-surface-area
              (let ([b (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-equal 24 (box3d-surface-area b))))
            
            (define-test box-contains-point-center
              (let ([b (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-true (box3d-contains-point? b (vec3 0 0 0)))))
            
            (define-test box-contains-point-inside
              (let ([b (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-true (box3d-contains-point? b (vec3 0.5 0.5 0.5)))))
            
            (define-test box-contains-point-outside
              (let ([b (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-false (box3d-contains-point? b (vec3 2 0 0)))))
            
            (define-test box-corners-count
              (let ([b (box3d (vec3 0 0 0) (vec3 1 1 1))])
                   (assert-equal 8 (length (box3d-corners b))))))

;;; ============================================================
;;; AABB3D Tests
;;; ============================================================

(test-group aabb3d-tests
            
            (define-test aabb-center
              (let ([a (aabb3d (vec3 0 0 0) (vec3 2 2 2))])
                   (let ([c (aabb3d-center a)])
                        (assert-equal 1 (vec3-x c))
                        (assert-equal 1 (vec3-y c))
                        (assert-equal 1 (vec3-z c)))))
            
            (define-test aabb-extents
              (let ([a (aabb3d (vec3 0 0 0) (vec3 2 4 6))])
                   (let ([e (aabb3d-extents a)])
                        (assert-equal 2 (vec3-x e))
                        (assert-equal 4 (vec3-y e))
                        (assert-equal 6 (vec3-z e)))))
            
            (define-test aabb-intersection-overlapping
              (let ([a (aabb3d (vec3 0 0 0) (vec3 2 2 2))]
                    [b (aabb3d (vec3 1 1 1) (vec3 3 3 3))])
                   (assert-true (aabb3d-intersects? a b))))
            
            (define-test aabb-intersection-separate
              (let ([a (aabb3d (vec3 0 0 0) (vec3 1 1 1))]
                    [b (aabb3d (vec3 5 5 5) (vec3 6 6 6))])
                   (assert-false (aabb3d-intersects? a b))))
            
            (define-test aabb-merge
              (let* ([a (aabb3d (vec3 0 0 0) (vec3 1 1 1))]
                     [b (aabb3d (vec3 2 2 2) (vec3 3 3 3))]
                     [m (aabb3d-merge a b)])
                    (assert-equal 0 (vec3-x (aabb3d-min m)))
                    (assert-equal 3 (vec3-x (aabb3d-max m)))))
            
            (define-test aabb-expand
              (let* ([a (aabb3d (vec3 0 0 0) (vec3 1 1 1))]
                     [e (aabb3d-expand a 0.5)])
                    (assert-true (< (abs (- (vec3-x (aabb3d-min e)) -0.5)) 1e-10))
                    (assert-true (< (abs (- (vec3-x (aabb3d-max e)) 1.5)) 1e-10))))
            
            (define-test aabb-from-points
              (let ([a (aabb3d-from-points (list (vec3 0 0 0)
                                                 (vec3 1 2 3)
                                                 (vec3 -1 0 1)))])
                   (assert-equal -1 (vec3-x (aabb3d-min a)))
                   (assert-equal 2 (vec3-y (aabb3d-max a))))))

;;; ============================================================
;;; Shape Utilities Tests
;;; ============================================================

(test-group shape-utility-tests
            
            (define-test shape3d-recognizes-sphere
              (assert-true (shape3d? (sphere3d (vec3 0 0 0) 1))))
            
            (define-test shape3d-recognizes-box
              (assert-true (shape3d? (box3d (vec3 0 0 0) (vec3 1 1 1)))))
            
            (define-test shape3d-recognizes-aabb
              (assert-true (shape3d? (aabb3d (vec3 0 0 0) (vec3 1 1 1)))))
            
            (define-test shape3d-center-for-sphere
              (assert-true (vec3? (shape3d-center (sphere3d (vec3 1 2 3) 1)))))
            
            (define-test shape3d-center-for-box
              (assert-true (vec3? (shape3d-center (box3d (vec3 4 5 6) (vec3 1 1 1)))))))
