;;; fabric/stitches/test-higher-order-diff.ss — Tests for Higher-Order Differentiation

(load "test-framework.ss")
(load "higher-order-diff.ss")

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

;;; Helper: check matrix element
(define (assert-matrix-= m i j expected tolerance)
  (assert-= (matrix-ref m i j) expected tolerance))

(display "
══════════════════════════════════════════════════════════
         HIGHER-ORDER DIFFERENTIATION TESTS
══════════════════════════════════════════════════════════
")

;;; ============================================================
;;; Jacobian Tests
;;; ============================================================

(test-group jacobian-tests
            (define-test jacobian-identity
              ;; f(x,y) = (x, y), J = [[1,0],[0,1]]
              (let* ([f (lambda (x y) (list x y))]
                     [J (jacobian f '(1 2))])
                    (assert-equal 2 (matrix-rows J))
                    (assert-equal 2 (matrix-cols J))
                    (assert-matrix-= J 0 0 1 0.0001)
                    (assert-matrix-= J 0 1 0 0.0001)
                    (assert-matrix-= J 1 0 0 0.0001)
                    (assert-matrix-= J 1 1 1 0.0001)))
            
            (define-test jacobian-linear
              ;; f(x,y) = (2x+y, x-3y), J = [[2,1],[1,-3]]
              (let* ([f (lambda (x y)
                                (list (traced-add (traced-mul x 2) y)
                                      (traced-sub x (traced-mul y 3))))]
                     [J (jacobian f '(1 1))])
                    (assert-matrix-= J 0 0 2 0.0001)
                    (assert-matrix-= J 0 1 1 0.0001)
                    (assert-matrix-= J 1 0 1 0.0001)
                    (assert-matrix-= J 1 1 -3 0.0001)))
            
            (define-test jacobian-quadratic
              ;; f(x,y) = (x², xy, y²)
              ;; J at (2,3) = [[4,0],[3,2],[0,6]]
              (let* ([f (lambda (x y)
                                (list (traced-sq x)
                                      (traced-mul x y)
                                      (traced-sq y)))]
                     [J (jacobian f '(2 3))])
                    (assert-equal 3 (matrix-rows J))
                    (assert-equal 2 (matrix-cols J))
                    (assert-matrix-= J 0 0 4 0.0001)  ; ∂(x²)/∂x = 2x = 4
                    (assert-matrix-= J 0 1 0 0.0001)  ; ∂(x²)/∂y = 0
                    (assert-matrix-= J 1 0 3 0.0001)  ; ∂(xy)/∂x = y = 3
                    (assert-matrix-= J 1 1 2 0.0001)  ; ∂(xy)/∂y = x = 2
                    (assert-matrix-= J 2 0 0 0.0001)  ; ∂(y²)/∂x = 0
                    (assert-matrix-= J 2 1 6 0.0001)))  ; ∂(y²)/∂y = 2y = 6
            
            (define-test jacobian-scalar-function
              ;; f(x,y) = x+y (scalar), J = [[1,1]]
              (let* ([f (lambda (x y) (traced-add x y))]
                     [J (jacobian f '(1 2))])
                    (assert-equal 1 (matrix-rows J))
                    (assert-equal 2 (matrix-cols J))
                    (assert-matrix-= J 0 0 1 0.0001)
                    (assert-matrix-= J 0 1 1 0.0001)))
            
            (define-test jacobian-single-input
              ;; f(x) = (sin(x), cos(x)), J at 0 = [[1],[-0]]
              (let* ([f (lambda (x)
                                (list (traced-sin x)
                                      (traced-cos x)))]
                     [J (jacobian f '(0))])
                    (assert-equal 2 (matrix-rows J))
                    (assert-equal 1 (matrix-cols J))
                    (assert-matrix-= J 0 0 1 0.0001)   ; cos(0) = 1
                    (assert-matrix-= J 1 0 0 0.0001))))  ; -sin(0) = 0

;;; ============================================================
;;; JVP (Jacobian-Vector Product) Tests
;;; ============================================================

(test-group jvp-tests
            (define-test jvp-identity
              ;; f(x,y) = (x, y), J = I, Jv = v
              (let* ([f (lambda (x y) (list x y))]
                     [result (jvp f '(1 2) '(3 4))])
                    (assert-= (car result) 3 0.0001)
                    (assert-= (cadr result) 4 0.0001)))
            
            (define-test jvp-linear
              ;; f(x,y) = (2x+y, x-3y), Jv = [[2,1],[1,-3]][v1,v2]
              ;; v = (1, 1), Jv = (3, -2)
              (let* ([f (lambda (x y)
                                (list (dual-add (dual-mul x 2) y)
                                      (dual-sub x (dual-mul y 3))))]
                     [result (jvp f '(1 1) '(1 1))])
                    (assert-= (car result) 3 0.0001)
                    (assert-= (cadr result) -2 0.0001)))
            
            (define-test jvp-scalar
              ;; f(x,y) = xy, ∇f = (y, x), Jv = y*v1 + x*v2
              ;; At (3, 4) with v=(1, 2): Jv = 4*1 + 3*2 = 10
              (let* ([f (lambda (x y) (dual-mul x y))]
                     [result (jvp f '(3 4) '(1 2))])
                    (assert-= (car result) 10 0.0001))))

;;; ============================================================
;;; VJP (Vector-Jacobian Product) Tests
;;; ============================================================

(test-group vjp-tests
            (define-test vjp-identity
              ;; f(x,y) = (x, y), J^T = I, v^T J = v
              (let* ([f (lambda (x y) (list x y))]
                     [result (vjp f '(1 2) '(3 4))])
                    (assert-= (car result) 3 0.0001)
                    (assert-= (cadr result) 4 0.0001)))
            
            (define-test vjp-linear
              ;; f(x,y) = (2x+y, x-3y), v^T J = v^T [[2,1],[1,-3]]
              ;; v = (1, 1), v^T J = (3, -2)
              (let* ([f (lambda (x y)
                                (list (traced-add (traced-mul x 2) y)
                                      (traced-sub x (traced-mul y 3))))]
                     [result (vjp f '(1 1) '(1 1))])
                    (assert-= (car result) 3 0.0001)    ; 1*2 + 1*1 = 3
                    (assert-= (cadr result) -2 0.0001))) ; 1*1 + 1*(-3) = -2
            
            (define-test vjp-single-output
              ;; f(x,y) = xy, ∇f = (y, x), v^T J = v * (y, x)
              ;; At (3, 4) with v=(2): v^T J = 2*(4, 3) = (8, 6)
              (let* ([f (lambda (x y) (list (traced-mul x y)))]
                     [result (vjp f '(3 4) '(2))])
                    (assert-= (car result) 8 0.0001)
                    (assert-= (cadr result) 6 0.0001))))

;;; ============================================================
;;; Hessian Tests (Numerical)
;;; ============================================================

(test-group hessian-numerical-tests
            (define-test hessian-numerical-quadratic
              ;; f(x,y) = x² + y², H = [[2,0],[0,2]]
              (let* ([f (lambda (x y) (+ (* x x) (* y y)))]
                     [H (hessian-numerical f '(1 1) 1e-4)])
                    (assert-matrix-= H 0 0 2 0.01)
                    (assert-matrix-= H 0 1 0 0.01)
                    (assert-matrix-= H 1 0 0 0.01)
                    (assert-matrix-= H 1 1 2 0.01)))
            
            (define-test hessian-numerical-cross-term
              ;; f(x,y) = xy, H = [[0,1],[1,0]]
              (let* ([f (lambda (x y) (* x y))]
                     [H (hessian-numerical f '(1 1) 1e-4)])
                    (assert-matrix-= H 0 0 0 0.01)
                    (assert-matrix-= H 0 1 1 0.01)
                    (assert-matrix-= H 1 0 1 0.01)
                    (assert-matrix-= H 1 1 0 0.01)))
            
            (define-test hessian-numerical-cubic
              ;; f(x) = x³, H = [[6x]]
              ;; At x=2: H = [[12]]
              (let* ([f (lambda (x) (expt x 3))]
                     [H (hessian-numerical f '(2) 1e-4)])
                    (assert-matrix-= H 0 0 12 0.1))))

;;; ============================================================
;;; Gradient Utilities
;;; ============================================================

(test-group gradient-utility-tests
            (define-test grad-wrapper
              ;; grad should give same result as gradient
              (let* ([f (lambda (x y) (traced-add (traced-sq x) (traced-sq y)))]
                     [g (grad f '(3 4))])
                    (assert-= (car g) 6 0.0001)    ; 2x = 6
                    (assert-= (cadr g) 8 0.0001)))  ; 2y = 8
            
            (define-test directional-derivative-test
              ;; f(x,y) = x² + y², ∇f = (2x, 2y)
              ;; At (3,4) in direction (1,0): ∇f · v = 6
              (let* ([f (lambda (x y) (traced-add (traced-sq x) (traced-sq y)))]
                     [dd (directional-derivative f '(3 4) '(1 0))])
                    (assert-= dd 6 0.0001)))
            
            (define-test directional-derivative-normalized
              ;; f(x,y) = x² + y², ∇f at (3,4) = (6, 8)
              ;; Direction (0.6, 0.8) (normalized): dd = 6*0.6 + 8*0.8 = 10
              (let* ([f (lambda (x y) (traced-add (traced-sq x) (traced-sq y)))]
                     [dd (directional-derivative f '(3 4) '(0.6 0.8))])
                    (assert-= dd 10 0.0001))))

;;; ============================================================
;;; Second Derivative (Single Variable)
;;; ============================================================

(test-group second-derivative-tests
            (define-test second-derivative-quadratic
              ;; f(x) = x², f''(x) = 2
              (let* ([f (lambda (x) (traced-sq x))]
                     [d2f (second-derivative f 5)])
                    (assert-= d2f 2 0.1)))
            
            (define-test second-derivative-cubic
              ;; f(x) = x³, f''(x) = 6x
              ;; At x=2: f''(2) = 12
              (let* ([f (lambda (x) (traced-pow x 3))]
                     [d2f (second-derivative f 2)])
                    (assert-= d2f 12 0.5))))

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
[SUCCESS] All higher-order differentiation tests passed.
")
    (display "
[FAILURE] Some higher-order differentiation tests failed.
"))
