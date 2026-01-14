;;; core/diff-physics/test-traced-vec2.ss --- Tests for Traced Vec2 Operations
;;;
;;; Comprehensive tests for differentiable 2D vector operations including:
;;; - Construction and conversion
;;; - Arithmetic operations
;;; - Dot and cross products
;;; - Length and distance
;;; - Normalization
;;; - Rotation and transformation
;;; - Smooth approximations
;;; - Gradient utilities
;;;
;;; Run with: scheme --script core/diff-physics/test-traced-vec2.ss

(load "core/test-framework.ss")
(load "lattice/physics/diff/traced-vec2.ss")

(display "
====
         TRACED VEC2 TESTS
====
")

;;; ====
;;; Test Utilities
;;; ====

(define (assert-near actual expected tolerance)
  (unless (< (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    FAIL: Expected ")
          (display expected)
          (display " +/- ")
          (display tolerance)
          (display ", got ")
          (display actual)
          (newline)))

(define (finite-diff f x epsilon)
  (/ (- (f (+ x epsilon)) (f (- x epsilon)))
     (* 2 epsilon)))

(define pi 3.141592653589793)

;;; ====
;;; Construction Tests
;;; ====

(test-group construction-tests
            
            (define-test traced-vec2-predicate
              (let* ([tape (make-reverse-tape)]
                     [v (traced-vec2 (make-traced-var 1 tape)
                                     (make-traced-var 2 tape))])
                    (assert-true (traced-vec2? v))))
            
            (define-test traced-vec2-components
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 3 tape)]
                     [y (make-traced-var 4 tape)]
                     [v (traced-vec2 x y)])
                    (assert-equal (traced-vec2-x v) x)
                    (assert-equal (traced-vec2-y v) y)))
            
            (define-test traced-vec2-zero
              (let* ([tape (make-reverse-tape)]
                     [v (traced-vec2-zero tape)])
                    (assert-near (traced-value (traced-vec2-x v)) 0 1e-10)
                    (assert-near (traced-value (traced-vec2-y v)) 0 1e-10)))
            
            (define-test lift-vec2-conversion
              (let* ([tape (make-reverse-tape)]
                     [v (vec2 5 7)]
                     [tv (lift-vec2 v tape)])
                    (assert-near (traced-value (traced-vec2-x tv)) 5 1e-10)
                    (assert-near (traced-value (traced-vec2-y tv)) 7 1e-10)))
            
            (define-test lift-vec2-const-conversion
              (let* ([v (vec2 -3 8)]
                     [tv (lift-vec2-const v)])
                    ;; Constants should just be numbers
                    (assert-equal (traced-vec2-x tv) -3)
                    (assert-equal (traced-vec2-y tv) 8)))
            
            (define-test unpack-traced-vec2-roundtrip
              (let* ([tape (make-reverse-tape)]
                     [v (vec2 2.5 -1.3)]
                     [tv (lift-vec2 v tape)]
                     [unpacked (unpack-traced-vec2 tv)])
                    (assert-near (vec2-x unpacked) 2.5 1e-10)
                    (assert-near (vec2-y unpacked) -1.3 1e-10)))
            
            (define-test traced-vec2->list-extracts-components
              (let* ([tape (make-reverse-tape)]
                     [v (traced-vec2 (make-traced-var 1 tape)
                                     (make-traced-var 2 tape))]
                     [lst (traced-vec2->list v)])
                    (assert-equal (length lst) 2)))
            )

;;; ====
;;; Arithmetic Tests
;;; ====

(test-group arithmetic-tests
            
            (define-test traced-vec2-add
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 1 2) tape)]
                     [b (lift-vec2 (vec2 3 4) tape)]
                     [c (traced-vec2-add a b)])
                    (assert-near (traced-value (traced-vec2-x c)) 4 1e-10)
                    (assert-near (traced-value (traced-vec2-y c)) 6 1e-10)))
            
            (define-test traced-vec2-sub
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 5 7) tape)]
                     [b (lift-vec2 (vec2 2 3) tape)]
                     [c (traced-vec2-sub a b)])
                    (assert-near (traced-value (traced-vec2-x c)) 3 1e-10)
                    (assert-near (traced-value (traced-vec2-y c)) 4 1e-10)))
            
            (define-test traced-vec2-neg
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 3 -4) tape)]
                     [neg (traced-vec2-neg v)])
                    (assert-near (traced-value (traced-vec2-x neg)) -3 1e-10)
                    (assert-near (traced-value (traced-vec2-y neg)) 4 1e-10)))
            
            (define-test traced-vec2-mul-hadamard
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 2 3) tape)]
                     [b (lift-vec2 (vec2 4 5) tape)]
                     [c (traced-vec2-mul a b)])
                    (assert-near (traced-value (traced-vec2-x c)) 8 1e-10)
                    (assert-near (traced-value (traced-vec2-y c)) 15 1e-10)))
            
            (define-test traced-vec2-scale
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 2 3) tape)]
                     [s (make-traced-var 4 tape)]
                     [scaled (traced-vec2-scale v s)])
                    (assert-near (traced-value (traced-vec2-x scaled)) 8 1e-10)
                    (assert-near (traced-value (traced-vec2-y scaled)) 12 1e-10)))
            
            (define-test traced-vec2-scale-inv
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 8 12) tape)]
                     [s (make-traced-var 4 tape)]
                     [divided (traced-vec2-scale-inv v s)])
                    (assert-near (traced-value (traced-vec2-x divided)) 2 1e-10)
                    (assert-near (traced-value (traced-vec2-y divided)) 3 1e-10)))
            )

;;; ====
;;; Product Tests
;;; ====

(test-group product-tests
            
            (define-test traced-vec2-dot-perpendicular
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 1 0) tape)]
                     [b (lift-vec2 (vec2 0 1) tape)]
                     [d (traced-vec2-dot a b)])
                    (assert-near (traced-value d) 0 1e-10)))
            
            (define-test traced-vec2-dot-parallel
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 2 0) tape)]
                     [b (lift-vec2 (vec2 3 0) tape)]
                     [d (traced-vec2-dot a b)])
                    (assert-near (traced-value d) 6 1e-10)))
            
            (define-test traced-vec2-dot-general
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 1 2) tape)]
                     [b (lift-vec2 (vec2 3 4) tape)]
                     [d (traced-vec2-dot a b)])
                    ;; 1*3 + 2*4 = 11
                    (assert-near (traced-value d) 11 1e-10)))
            
            (define-test traced-vec2-cross-perpendicular
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 1 0) tape)]
                     [b (lift-vec2 (vec2 0 1) tape)]
                     [c (traced-vec2-cross a b)])
                    ;; 1*1 - 0*0 = 1
                    (assert-near (traced-value c) 1 1e-10)))
            
            (define-test traced-vec2-cross-parallel
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 2 3) tape)]
                     [b (lift-vec2 (vec2 4 6) tape)]
                     [c (traced-vec2-cross a b)])
                    ;; 2*6 - 3*4 = 0
                    (assert-near (traced-value c) 0 1e-10)))
            
            (define-test traced-vec2-cross-antisymmetric
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 1 2) tape)]
                     [b (lift-vec2 (vec2 3 4) tape)]
                     [c1 (traced-vec2-cross a b)]
                     [c2 (traced-vec2-cross b a)])
                    (assert-near (traced-value c1) (- (traced-value c2)) 1e-10)))
            )

;;; ====
;;; Length and Distance Tests
;;; ====

(test-group length-tests
            
            (define-test traced-vec2-magnitude-sq
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 3 4) tape)]
                     [sq (traced-vec2-magnitude-sq v)])
                    (assert-near (traced-value sq) 25 1e-10)))
            
            (define-test traced-vec2-magnitude
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 3 4) tape)]
                     [mag (traced-vec2-magnitude v)])
                    (assert-near (traced-value mag) 5 1e-10)))
            
            (define-test traced-vec2-smooth-magnitude
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 3 4) tape)]
                     [mag (traced-vec2-smooth-magnitude v 1e-8)])
                    (assert-near (traced-value mag) 5 1e-6)))
            
            (define-test traced-vec2-smooth-magnitude-near-zero
              ;; Should not blow up near zero
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 0 0) tape)]
                     [mag (traced-vec2-smooth-magnitude v 0.01)])
                    (assert-true (number? (traced-value mag)))
                    (assert-true (> (traced-value mag) 0))))
            
            (define-test traced-vec2-distance
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 3 4) tape)]
                     [d (traced-vec2-distance a b)])
                    (assert-near (traced-value d) 5 1e-10)))
            
            (define-test traced-vec2-distance-sq
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 1 1) tape)]
                     [b (lift-vec2 (vec2 4 5) tape)]
                     [d (traced-vec2-distance-sq a b)])
                    ;; (3)^2 + (4)^2 = 25
                    (assert-near (traced-value d) 25 1e-10)))
            )

;;; ====
;;; Normalization Tests
;;; ====

(test-group normalization-tests
            
            (define-test traced-vec2-normalize
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 3 4) tape)]
                     [n (traced-vec2-normalize v)])
                    (assert-near (traced-value (traced-vec2-x n)) 0.6 1e-6)
                    (assert-near (traced-value (traced-vec2-y n)) 0.8 1e-6)))
            
            (define-test traced-vec2-normalize-unit-length
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 5 12) tape)]
                     [n (traced-vec2-normalize v)]
                     [len (traced-vec2-magnitude n)])
                    (assert-near (traced-value len) 1 1e-6)))
            
            (define-test traced-vec2-smooth-normalize
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 3 4) tape)]
                     [n (traced-vec2-smooth-normalize v 1e-8)])
                    (assert-near (traced-value (traced-vec2-x n)) 0.6 1e-5)
                    (assert-near (traced-value (traced-vec2-y n)) 0.8 1e-5)))
            
            (define-test traced-vec2-safe-normalize-small-returns-zero
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 1e-10 1e-10) tape)]
                     [n (traced-vec2-safe-normalize v 1e-6)])
                    (assert-near (traced-value (traced-vec2-x n)) 0 1e-6)
                    (assert-near (traced-value (traced-vec2-y n)) 0 1e-6)))
            )

;;; ====
;;; Rotation and Transformation Tests
;;; ====

(test-group rotation-tests
            
            (define-test traced-vec2-rotate-90
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 1 0) tape)]
                     [angle (make-traced-var (/ pi 2) tape)]
                     [r (traced-vec2-rotate v angle)])
                    ;; (1, 0) rotated 90 degrees -> (0, 1)
                    (assert-near (traced-value (traced-vec2-x r)) 0 1e-6)
                    (assert-near (traced-value (traced-vec2-y r)) 1 1e-6)))
            
            (define-test traced-vec2-rotate-180
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 1 0) tape)]
                     [angle (make-traced-var pi tape)]
                     [r (traced-vec2-rotate v angle)])
                    ;; (1, 0) rotated 180 degrees -> (-1, 0)
                    (assert-near (traced-value (traced-vec2-x r)) -1 1e-6)
                    (assert-near (traced-value (traced-vec2-y r)) 0 1e-6)))
            
            (define-test traced-vec2-rotate-90-function
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 3 4) tape)]
                     [r (traced-vec2-rotate-90 v)])
                    ;; (-y, x)
                    (assert-near (traced-value (traced-vec2-x r)) -4 1e-10)
                    (assert-near (traced-value (traced-vec2-y r)) 3 1e-10)))
            
            (define-test traced-vec2-rotate-neg-90
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 3 4) tape)]
                     [r (traced-vec2-rotate-neg-90 v)])
                    ;; (y, -x)
                    (assert-near (traced-value (traced-vec2-x r)) 4 1e-10)
                    (assert-near (traced-value (traced-vec2-y r)) -3 1e-10)))
            
            (define-test traced-vec2-reflect
              (let* ([tape (make-reverse-tape)]
                     [v (lift-vec2 (vec2 1 -1) tape)]
                     [n (lift-vec2 (vec2 0 1) tape)]  ; horizontal surface
                     [r (traced-vec2-reflect v n)])
                    ;; Reflecting (1, -1) across y-axis normal -> (1, 1)
                    (assert-near (traced-value (traced-vec2-x r)) 1 1e-10)
                    (assert-near (traced-value (traced-vec2-y r)) 1 1e-10)))
            
            (define-test traced-vec2-project
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 3 4) tape)]
                     [b (lift-vec2 (vec2 1 0) tape)]  ; project onto x-axis
                     [p (traced-vec2-project a b)])
                    (assert-near (traced-value (traced-vec2-x p)) 3 1e-10)
                    (assert-near (traced-value (traced-vec2-y p)) 0 1e-10)))
            
            (define-test traced-vec2-reject
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 3 4) tape)]
                     [b (lift-vec2 (vec2 1 0) tape)]
                     [r (traced-vec2-reject a b)])
                    ;; Perpendicular component
                    (assert-near (traced-value (traced-vec2-x r)) 0 1e-10)
                    (assert-near (traced-value (traced-vec2-y r)) 4 1e-10)))
            )

;;; ====
;;; Interpolation Tests
;;; ====

(test-group interpolation-tests
            
            (define-test traced-vec2-lerp-start
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 10 20) tape)]
                     [t (make-traced-var 0 tape)]
                     [r (traced-vec2-lerp a b t)])
                    (assert-near (traced-value (traced-vec2-x r)) 0 1e-10)
                    (assert-near (traced-value (traced-vec2-y r)) 0 1e-10)))
            
            (define-test traced-vec2-lerp-end
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 10 20) tape)]
                     [t (make-traced-var 1 tape)]
                     [r (traced-vec2-lerp a b t)])
                    (assert-near (traced-value (traced-vec2-x r)) 10 1e-10)
                    (assert-near (traced-value (traced-vec2-y r)) 20 1e-10)))
            
            (define-test traced-vec2-lerp-middle
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 10 20) tape)]
                     [t (make-traced-var 0.5 tape)]
                     [r (traced-vec2-lerp a b t)])
                    (assert-near (traced-value (traced-vec2-x r)) 5 1e-10)
                    (assert-near (traced-value (traced-vec2-y r)) 10 1e-10)))
            )

;;; ====
;;; Smooth Approximation Tests
;;; ====

(test-group smooth-approx-tests
            
            (define-test traced-softplus-negative
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var -5 tape)]
                     [result (traced-softplus x 10)])
                    ;; Should be close to 0
                    (assert-near (traced-value result) 0 0.1)))
            
            (define-test traced-softplus-positive
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 5 tape)]
                     [result (traced-softplus x 10)])
                    ;; Should be close to 5
                    (assert-near (traced-value result) 5 0.1)))
            
            (define-test traced-softmax2-correct
              (let* ([tape (make-reverse-tape)]
                     [a (make-traced-var 3 tape)]
                     [b (make-traced-var 7 tape)]
                     [result (traced-softmax2 a b 100)])
                    ;; Should be close to 7
                    (assert-near (traced-value result) 7 0.1)))
            
            (define-test traced-softmin2-correct
              (let* ([tape (make-reverse-tape)]
                     [a (make-traced-var 3 tape)]
                     [b (make-traced-var 7 tape)]
                     [result (traced-softmin2 a b 100)])
                    ;; Should be close to 3
                    (assert-near (traced-value result) 3 0.1)))
            
            (define-test traced-smooth-abs-positive
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 5 tape)]
                     [result (traced-smooth-abs x 0.01)])
                    (assert-near (traced-value result) 5 0.1)))
            
            (define-test traced-smooth-abs-negative
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var -5 tape)]
                     [result (traced-smooth-abs x 0.01)])
                    (assert-near (traced-value result) 5 0.1)))
            
            (define-test traced-smooth-clamp-in-range
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 0.5 tape)]
                     [result (traced-smooth-clamp x 0 1 100)])
                    (assert-near (traced-value result) 0.5 0.1)))
            
            (define-test traced-smooth-clamp-below
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var -1 tape)]
                     [result (traced-smooth-clamp x 0 1 100)])
                    (assert-near (traced-value result) 0 0.1)))
            
            (define-test traced-smooth-clamp-above
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 5 tape)]
                     [result (traced-smooth-clamp x 0 1 100)])
                    (assert-near (traced-value result) 1 0.1)))
            )

;;; ====
;;; Gradient Tests
;;; ====

(test-group gradient-tests
            
            (define-test vec2-gradient-simple
              ;; f(x, y) = x^2 + y^2
              ;; df/dx = 2x, df/dy = 2y
              (let* ([f (lambda (tv)
                                (traced-vec2-magnitude-sq tv))]
                     [p (vec2 3 4)]
                     [grad (vec2-gradient f p)])
                    ;; At (3, 4): grad = (6, 8)
                    (assert-near (vec2-x grad) 6 0.01)
                    (assert-near (vec2-y grad) 8 0.01)))
            
            (define-test add-gradient-correct
              (let* ([grads (gradient (lambda (ax ay bx by)
                                              (traced-vec2-x (traced-vec2-add
                                                              (traced-vec2 ax ay)
                                                              (traced-vec2 bx by))))
                                      (list 1 2 3 4))])
                    ;; d(ax + bx)/d(ax) = 1
                    (assert-near (list-ref grads 0) 1 1e-10)
                    ;; d(ax + bx)/d(bx) = 1
                    (assert-near (list-ref grads 2) 1 1e-10)
                    ;; No y dependence
                    (assert-near (list-ref grads 1) 0 1e-10)))
            
            (define-test dot-gradient-correct
              (let* ([grads (gradient (lambda (ax ay bx by)
                                              (traced-vec2-dot (traced-vec2 ax ay)
                                                               (traced-vec2 bx by)))
                                      (list 1 2 3 4))])
                    ;; d(ax*bx + ay*by)/d(ax) = bx = 3
                    (assert-near (list-ref grads 0) 3 1e-10)
                    ;; d(ax*bx + ay*by)/d(ay) = by = 4
                    (assert-near (list-ref grads 1) 4 1e-10)))
            
            (define-test cross-gradient-correct
              (let* ([grads (gradient (lambda (ax ay bx by)
                                              (traced-vec2-cross (traced-vec2 ax ay)
                                                                 (traced-vec2 bx by)))
                                      (list 1 2 3 4))])
                    ;; d(ax*by - ay*bx)/d(ax) = by = 4
                    (assert-near (list-ref grads 0) 4 1e-10)
                    ;; d(ax*by - ay*bx)/d(ay) = -bx = -3
                    (assert-near (list-ref grads 1) -3 1e-10)))
            
            (define-test magnitude-gradient-correct
              ;; d|v|/dx = x/|v|, d|v|/dy = y/|v|
              (let* ([grads (gradient (lambda (x y)
                                              (traced-vec2-magnitude (traced-vec2 x y)))
                                      (list 3 4))]
                     [expected-x (/ 3 5)]
                     [expected-y (/ 4 5)])
                    (assert-near (list-ref grads 0) expected-x 1e-6)
                    (assert-near (list-ref grads 1) expected-y 1e-6)))
            
            (define-test rotate-gradient-correct
              ;; Numerical verification
              (let* ([test-fn (lambda (theta)
                                      (let* ([tape (make-reverse-tape)]
                                             [v (lift-vec2 (vec2 1 0) tape)]
                                             [a (make-traced-var theta tape)]
                                             [r (traced-vec2-rotate v a)])
                                            (traced-value (traced-vec2-x r))))]
                     [theta0 0.5]
                     [numerical (finite-diff test-fn theta0 1e-5)]
                     ;; d(cos theta)/d theta = -sin theta
                     [analytical (- (sin theta0))])
                    (assert-near numerical analytical 1e-4)))
            )

;;; ====
;;; Run All Tests
;;; ====

(newline)
(display "----")
(newline)
(display "  Test Summary:")
(newline)
(display "----")
(newline)
(display "  Total:  ")
(display *tests-run*)
(display " tests")
(newline)
(display "  Passed: ")
(display *tests-passed*)
(newline)
(display "  Failed: ")
(display *tests-failed*)
(newline)
(newline)

(when (> *tests-failed* 0)
      (exit 1))
