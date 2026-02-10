;;; fabric/stitches/test-vec2.ss — Tests for 2D Vector Math Library

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/linalg/vec2.ss")

;;; Local helper for numeric comparison with tolerance
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

(define pi 3.14159265358979)

(display "
══════════════════════════════════════════════════════════
              2D VECTOR MATH TESTS
══════════════════════════════════════════════════════════
")

;;; ====
;;; Construction
;;; ====

(test-group construction-tests
            (define-test vec2-creates-vector
              (let ([v (vec2 3 4)])
                   (assert-true (vec2? v))
                   (assert-equal 3 (vec2-x v))
                   (assert-equal 4 (vec2-y v))))
            
            (define-test vec2-zero-is-origin
              (let ([v (vec2-zero)])
                   (assert-equal 0 (vec2-x v))
                   (assert-equal 0 (vec2-y v))))
            
            (define-test vec2-one-creates-unit
              (let ([v (vec2-one)])
                   (assert-equal 1 (vec2-x v))
                   (assert-equal 1 (vec2-y v))))
            
            (define-test vec2-unit-vectors
              (assert-equal 1 (vec2-x (vec2-unit-x)))
              (assert-equal 0 (vec2-y (vec2-unit-x)))
              (assert-equal 0 (vec2-x (vec2-unit-y)))
              (assert-equal 1 (vec2-y (vec2-unit-y))))
            
            (define-test vec2-from-angle
              (let ([v (vec2-from-angle 0)])
                   (assert-= (vec2-x v) 1 0.0001)
                   (assert-= (vec2-y v) 0 0.0001)))
            
            (define-test vec2-from-polar
              (let ([v (vec2-from-polar 2 (/ pi 4))])
                   (assert-= (vec2-x v) (sqrt 2) 0.0001)
                   (assert-= (vec2-y v) (sqrt 2) 0.0001))))

;;; ====
;;; Basic Arithmetic
;;; ====

(test-group arithmetic-tests
            (define-test vec2-add-correct
              (let ([result (vec2-add (vec2 1 2) (vec2 3 4))])
                   (assert-equal 4 (vec2-x result))
                   (assert-equal 6 (vec2-y result))))
            
            (define-test vec2-sub-correct
              (let ([result (vec2-sub (vec2 5 7) (vec2 2 3))])
                   (assert-equal 3 (vec2-x result))
                   (assert-equal 4 (vec2-y result))))
            
            (define-test vec2-neg-correct
              (let ([result (vec2-neg (vec2 3 -4))])
                   (assert-equal -3 (vec2-x result))
                   (assert-equal 4 (vec2-y result))))
            
            (define-test vec2-mul-correct
              (let ([result (vec2-mul (vec2 2 3) (vec2 4 5))])
                   (assert-equal 8 (vec2-x result))
                   (assert-equal 15 (vec2-y result))))
            
            (define-test vec2-div-correct
              (let ([result (vec2-div (vec2 6 8) (vec2 2 4))])
                   (assert-equal 3 (vec2-x result))
                   (assert-equal 2 (vec2-y result))))
            
            (define-test vec2-scale-correct
              (let ([result (vec2-scale (vec2 3 4) 2)])
                   (assert-equal 6 (vec2-x result))
                   (assert-equal 8 (vec2-y result))))
            
            (define-test vec2-scale-inv-correct
              (let ([result (vec2-scale-inv (vec2 6 8) 2)])
                   (assert-equal 3 (vec2-x result))
                   (assert-equal 4 (vec2-y result)))))

;;; ====
;;; Products
;;; ====

(test-group product-tests
            (define-test vec2-dot-correct
              ;; (1,2) · (3,4) = 1*3 + 2*4 = 11
              (assert-equal 11 (vec2-dot (vec2 1 2) (vec2 3 4))))
            
            (define-test vec2-dot-perpendicular-is-zero
              ;; (1,0) · (0,1) = 0
              (assert-equal 0 (vec2-dot (vec2-unit-x) (vec2-unit-y))))
            
            (define-test vec2-cross-correct
              ;; (1,2) × (3,4) = 1*4 - 2*3 = -2
              (assert-equal -2 (vec2-cross (vec2 1 2) (vec2 3 4))))
            
            (define-test vec2-cross-parallel-is-zero
              (assert-equal 0 (vec2-cross (vec2 2 4) (vec2 1 2)))))

;;; ====
;;; Length and Distance
;;; ====

(test-group length-tests
            (define-test vec2-magnitude-sq-correct
              ;; ||(3,4)||² = 9 + 16 = 25
              (assert-equal 25 (vec2-magnitude-sq (vec2 3 4))))
            
            (define-test vec2-magnitude-correct
              ;; ||(3,4)|| = 5
              (assert-equal 5 (vec2-magnitude (vec2 3 4))))
            
            (define-test vec2-distance-sq-correct
              ;; d²((0,0), (3,4)) = 25
              (assert-equal 25 (vec2-distance-sq (vec2-zero) (vec2 3 4))))
            
            (define-test vec2-distance-correct
              ;; d((0,0), (3,4)) = 5
              (assert-equal 5 (vec2-distance (vec2-zero) (vec2 3 4)))))

;;; ====
;;; Normalization
;;; ====

(test-group normalize-tests
            (define-test vec2-normalize-correct
              (let ([result (vec2-normalize (vec2 3 4))])
                   (assert-= (vec2-x result) 0.6 0.0001)
                   (assert-= (vec2-y result) 0.8 0.0001)
                   (assert-= (vec2-magnitude result) 1 0.0001)))
            
            (define-test vec2-normalize-zero-returns-zero
              (let ([result (vec2-normalize (vec2-zero))])
                   (assert-true (vec2-zero? result))))
            
            (define-test vec2-set-magnitude-correct
              (let ([result (vec2-set-magnitude (vec2 3 4) 10)])
                   (assert-= (vec2-magnitude result) 10 0.0001)))
            
            (define-test vec2-limit-under
              (let ([v (vec2 3 4)]  ; magnitude 5
                    [result (vec2-limit (vec2 3 4) 10)])
                   (assert-= (vec2-magnitude result) 5 0.0001)))
            
            (define-test vec2-limit-over
              (let ([result (vec2-limit (vec2 6 8) 5)])  ; magnitude 10, limit to 5
                   (assert-= (vec2-magnitude result) 5 0.0001))))

;;; ====
;;; Angles
;;; ====

(test-group angle-tests
            (define-test vec2-angle-x-axis
              (assert-= (vec2-angle (vec2 1 0)) 0 0.0001))
            
            (define-test vec2-angle-y-axis
              (assert-= (vec2-angle (vec2 0 1)) (/ pi 2) 0.0001))
            
            (define-test vec2-angle-between-perpendicular
              (let ([a (vec2 1 0)]
                    [b (vec2 0 1)])
                   (assert-= (vec2-angle-between a b) (/ pi 2) 0.0001)))
            
            (define-test vec2-angle-between-parallel
              (let ([a (vec2 1 0)]
                    [b (vec2 2 0)])
                   (assert-= (vec2-angle-between a b) 0 0.0001))))

;;; ====
;;; Rotation
;;; ====

(test-group rotation-tests
            (define-test vec2-rotate-90
              (let ([result (vec2-rotate (vec2 1 0) (/ pi 2))])
                   (assert-= (vec2-x result) 0 0.0001)
                   (assert-= (vec2-y result) 1 0.0001)))
            
            (define-test vec2-rotate-180
              (let ([result (vec2-rotate (vec2 1 0) pi)])
                   (assert-= (vec2-x result) -1 0.0001)
                   (assert-= (vec2-y result) 0 0.0001)))
            
            (define-test vec2-rotate-90-ccw
              (let ([result (vec2-rotate-90 (vec2 1 0))])
                   (assert-= (vec2-x result) 0 0.0001)
                   (assert-= (vec2-y result) 1 0.0001)))
            
            (define-test vec2-rotate-90-cw
              (let ([result (vec2-rotate-neg-90 (vec2 1 0))])
                   (assert-= (vec2-x result) 0 0.0001)
                   (assert-= (vec2-y result) -1 0.0001))))

;;; ====
;;; Reflection and Projection
;;; ====

(test-group projection-tests
            (define-test vec2-reflect-off-x-axis
              ;; Reflect (1,1) off horizontal surface (normal = (0,1))
              (let ([result (vec2-reflect (vec2 1 1) (vec2 0 1))])
                   (assert-= (vec2-x result) 1 0.0001)
                   (assert-= (vec2-y result) -1 0.0001)))
            
            (define-test vec2-project-onto-x-axis
              (let ([result (vec2-project (vec2 3 4) (vec2 1 0))])
                   (assert-= (vec2-x result) 3 0.0001)
                   (assert-= (vec2-y result) 0 0.0001)))
            
            (define-test vec2-reject-from-x-axis
              (let ([result (vec2-reject (vec2 3 4) (vec2 1 0))])
                   (assert-= (vec2-x result) 0 0.0001)
                   (assert-= (vec2-y result) 4 0.0001))))

;;; ====
;;; Interpolation
;;; ====

(test-group interpolation-tests
            (define-test vec2-lerp-at-zero
              (let* ([a (vec2 0 0)]
                     [b (vec2 10 10)]
                     [result (vec2-lerp a b 0)])
                    (assert-= (vec2-x result) 0 0.0001)
                    (assert-= (vec2-y result) 0 0.0001)))
            
            (define-test vec2-lerp-at-one
              (let* ([a (vec2 0 0)]
                     [b (vec2 10 10)]
                     [result (vec2-lerp a b 1)])
                    (assert-= (vec2-x result) 10 0.0001)
                    (assert-= (vec2-y result) 10 0.0001)))
            
            (define-test vec2-lerp-at-half
              (let* ([a (vec2 0 0)]
                     [b (vec2 10 10)]
                     [result (vec2-lerp a b 0.5)])
                    (assert-= (vec2-x result) 5 0.0001)
                    (assert-= (vec2-y result) 5 0.0001)))
            
            (define-test vec2-move-towards-within-range
              (let ([result (vec2-move-towards (vec2 0 0) (vec2 3 4) 10)])
                   (assert-equal 3 (vec2-x result))
                   (assert-equal 4 (vec2-y result))))
            
            (define-test vec2-move-towards-beyond-range
              (let ([result (vec2-move-towards (vec2 0 0) (vec2 6 8) 5)])
                   (assert-= (vec2-distance result (vec2-zero)) 5 0.0001))))

;;; ====
;;; Comparison
;;; ====

(test-group comparison-tests
            (define-test vec2-equal-true
              (assert-true (vec2-equal? (vec2 1 2) (vec2 1 2))))
            
            (define-test vec2-equal-false
              (assert-false (vec2-equal? (vec2 1 2) (vec2 1 3))))
            
            (define-test vec2-nearly-equal
              (assert-true (vec2-nearly-equal? (vec2 1.0001 2.0001)
                                               (vec2 1 2)
                                               0.001)))
            
            (define-test vec2-min-correct
              (let ([result (vec2-min (vec2 1 5) (vec2 3 2))])
                   (assert-equal 1 (vec2-x result))
                   (assert-equal 2 (vec2-y result))))
            
            (define-test vec2-max-correct
              (let ([result (vec2-max (vec2 1 5) (vec2 3 2))])
                   (assert-equal 3 (vec2-x result))
                   (assert-equal 5 (vec2-y result))))
            
            (define-test vec2-abs-correct
              (let ([result (vec2-abs (vec2 -3 -4))])
                   (assert-equal 3 (vec2-x result))
                   (assert-equal 4 (vec2-y result)))))

;;; ====
;;; Utility
;;; ====

(test-group utility-tests
            (define-test vec2-map-correct
              (let ([result (vec2-map (lambda (x) (* x 2)) (vec2 3 4))])
                   (assert-equal 6 (vec2-x result))
                   (assert-equal 8 (vec2-y result))))
            
            (define-test vec2-sum-correct
              (assert-equal 7 (vec2-sum (vec2 3 4))))
            
            (define-test vec2-product-correct
              (assert-equal 12 (vec2-product (vec2 3 4))))
            
            (define-test vec2-list-conversion
              (let* ([v (vec2 3 4)]
                     [lst (vec2->list v)]
                     [v2 (list->vec2 lst)])
                    (assert-true (vec2-equal? v v2)))))

;;; ====
;;; Summary
;;; ====

(display "
══════════════════════════════════════════════════════════
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All 2D vector tests passed.
")
    (display "
[FAILURE] Some 2D vector tests failed.
"))
