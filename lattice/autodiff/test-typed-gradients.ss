;;; core/autodiff/test-typed-gradients.ss --- Tests for Type-Safe Gradient Dimensions
;;;
;;; Tests dimension checking and type safety for gradient operations.

(load "core/test-framework.ss")
(load "lattice/autodiff/typed-gradients.ss")

;;; Local helper for numeric comparison with tolerance
(define (assert-= actual expected tolerance)
  (unless (< (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    x ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (display " +/- ")
          (display tolerance)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; Helper: check list of gradient values
(define (assert-grads-= actual expected tolerance)
  (for-each (lambda (a e) (assert-= a e tolerance))
            actual expected))

;;; Helper: check if result is an error of specific kind
(define (error-of-kind? result kind)
  (and (pair? result)
       (eq? (car result) 'error)
       (eq? (cadr result) kind)))

(display "
==============================================================
         TYPE-SAFE GRADIENT DIMENSION TESTS
==============================================================
")

;;; ============================================================
;;; DimGradient Construction and Validation
;;; ============================================================

(test-group dim-gradient-construction
            (define-test create-valid-gradient
              ;; Create a 3-dimensional gradient with 3 values
              (let ([g (dim-gradient 3 '(1.0 2.0 3.0))])
                   (assert-true (dim-gradient? g))
                   (assert-equal (dim-gradient-dim g) 3)))
            
            (define-test gradient-values-extraction
              (let ([g (dim-gradient 3 '(1.5 2.5 3.5))])
                   (assert-equal (dim-gradient-values g) '(1.5 2.5 3.5))))
            
            (define-test gradient-dimension-mismatch-error
              ;; Try to create a 3-dim gradient with 2 values
              (let ([result (dim-gradient 3 '(1.0 2.0))])
                   (assert-true (error-of-kind? result 'dimension-mismatch))))
            
            (define-test gradient-ref-valid
              (let ([g (dim-gradient 3 '(10 20 30))])
                   (assert-equal (dim-gradient-ref g 0) 10)
                   (assert-equal (dim-gradient-ref g 1) 20)
                   (assert-equal (dim-gradient-ref g 2) 30)))
            
            (define-test gradient-ref-out-of-bounds
              (let* ([g (dim-gradient 3 '(1 2 3))]
                     [result (dim-gradient-ref g 5)])
                    (assert-true (error-of-kind? result 'index-out-of-bounds)))))

;;; ============================================================
;;; GradientSpec Creation
;;; ============================================================

(test-group gradient-spec-tests
            (define-test create-scalar-fn-spec
              (let ([spec (scalar-fn 2 (lambda (x y) (traced-add x y)))])
                   (assert-true (gradient-spec? spec))
                   (assert-equal (gradient-spec-input-dim spec) 2)
                   (assert-equal (gradient-spec-output-dim spec) 1)))
            
            (define-test create-vector-fn-spec
              (let ([spec (vector-fn 2 3 (lambda (x y) (list x y (traced-add x y))))])
                   (assert-true (gradient-spec? spec))
                   (assert-equal (gradient-spec-input-dim spec) 2)
                   (assert-equal (gradient-spec-output-dim spec) 3))))

;;; ============================================================
;;; Safe Gradient Computation
;;; ============================================================

(test-group safe-gradient-tests
            (define-test gradient-sum-function
              ;; f(x,y) = x + y, gradient = (1, 1)
              (let* ([spec (scalar-fn 2 (lambda (x y) (traced-add x y)))]
                     [result (safe-gradient spec '(3.0 4.0))])
                    (assert-true (dim-gradient? result))
                    (assert-equal (dim-gradient-dim result) 2)
                    (assert-grads-= (dim-gradient-values result) '(1.0 1.0) 0.0001)))
            
            (define-test gradient-product-function
              ;; f(x,y) = x * y, gradient at (3,4) = (4, 3)
              (let* ([spec (scalar-fn 2 (lambda (x y) (traced-mul x y)))]
                     [result (safe-gradient spec '(3.0 4.0))])
                    (assert-true (dim-gradient? result))
                    (assert-grads-= (dim-gradient-values result) '(4.0 3.0) 0.0001)))
            
            (define-test gradient-wrong-input-dim
              ;; Spec expects 2 args, we provide 3
              (let* ([spec (scalar-fn 2 (lambda (x y) (traced-add x y)))]
                     [result (safe-gradient spec '(1.0 2.0 3.0))])
                    (assert-true (error-of-kind? result 'input-dimension-mismatch))))
            
            (define-test gradient-polynomial
              ;; f(x,y) = x^2 + y^2, gradient at (3,4) = (6, 8)
              (let* ([spec (scalar-fn 2 (lambda (x y)
                                                (traced-add (traced-sq x) (traced-sq y))))]
                     [result (safe-gradient spec '(3.0 4.0))])
                    (assert-true (dim-gradient? result))
                    (assert-grads-= (dim-gradient-values result) '(6.0 8.0) 0.0001)))
            
            (define-test gradient-three-vars
              ;; f(x,y,z) = xyz, gradient at (2,3,4) = (12, 8, 6)
              (let* ([spec (scalar-fn 3 (lambda (x y z)
                                                (traced-mul (traced-mul x y) z)))]
                     [result (safe-gradient spec '(2.0 3.0 4.0))])
                    (assert-true (dim-gradient? result))
                    (assert-equal (dim-gradient-dim result) 3)
                    (assert-grads-= (dim-gradient-values result) '(12.0 8.0 6.0) 0.0001))))

;;; ============================================================
;;; Safe Hessian Computation
;;; ============================================================

(test-group safe-hessian-tests
            (define-test hessian-quadratic-2d
              ;; f(x,y) = x^2 + y^2, Hessian = [[2, 0], [0, 2]]
              (let* ([spec (scalar-fn 2 (lambda (x y)
                                                (traced-add (traced-sq x) (traced-sq y))))]
                     [result (safe-hessian spec '(1.0 1.0))])
                    (assert-true (dim-hessian? result))
                    (assert-equal (dim-hessian-dim result) 2)))
            
            (define-test hessian-wrong-input-dim
              ;; Spec expects 2 args, we provide 1
              (let* ([spec (scalar-fn 2 (lambda (x y) (traced-sq x)))]
                     [result (safe-hessian spec '(1.0))])
                    (assert-true (error-of-kind? result 'input-dimension-mismatch))))
            
            (define-test hessian-not-scalar-function
              ;; Vector function cannot have Hessian
              (let* ([spec (vector-fn 2 2 (lambda (x y) (list x y)))]
                     [result (safe-hessian spec '(1.0 2.0))])
                    (assert-true (error-of-kind? result 'not-a-scalar-function)))))

;;; ============================================================
;;; Gradient Vector Operations
;;; ============================================================

(test-group gradient-operations
            (define-test gradient-addition
              (let* ([g1 (dim-gradient 3 '(1.0 2.0 3.0))]
                     [g2 (dim-gradient 3 '(4.0 5.0 6.0))]
                     [result (dim-gradient-add g1 g2)])
                    (assert-true (dim-gradient? result))
                    (assert-grads-= (dim-gradient-values result) '(5.0 7.0 9.0) 0.0001)))
            
            (define-test gradient-addition-dimension-mismatch
              (let* ([g1 (dim-gradient 3 '(1 2 3))]
                     [g2 (dim-gradient 2 '(4 5))]
                     [result (dim-gradient-add g1 g2)])
                    (assert-true (error-of-kind? result 'gradient-dimension-mismatch))))
            
            (define-test gradient-scaling
              (let* ([g (dim-gradient 3 '(2.0 4.0 6.0))]
                     [result (dim-gradient-scale 0.5 g)])
                    (assert-true (dim-gradient? result))
                    (assert-grads-= (dim-gradient-values result) '(1.0 2.0 3.0) 0.0001)))
            
            (define-test gradient-dot-product
              (let* ([g1 (dim-gradient 3 '(1.0 2.0 3.0))]
                     [g2 (dim-gradient 3 '(4.0 5.0 6.0))]
                     [result (dim-gradient-dot g1 g2)])
                    ;; 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
                    (assert-= result 32.0 0.0001)))
            
            (define-test gradient-norm
              (let* ([g (dim-gradient 2 '(3.0 4.0))]
                     [result (dim-gradient-norm g)])
                    ;; sqrt(9 + 16) = 5
                    (assert-= result 5.0 0.0001))))

;;; ============================================================
;;; Function Composition with Dimension Checking
;;; ============================================================

(test-group composition-tests
            (define-test compose-matching-dimensions
              ;; g: R^2 -> R, f: R^1 -> R
              ;; f o g: R^2 -> R
              (let* ([g-spec (scalar-fn 2 (lambda (x y) (traced-add x y)))]
                     ;; Note: We need f to take a single input that matches g's output
                     ;; But g outputs a scalar (1), so f should be 1-dimensional input
                     [f-spec (scalar-fn 1 (lambda (z) (traced-sq z)))]
                     [result (compose-gradient-fns f-spec g-spec)])
                    (assert-true (gradient-spec? result))
                    (assert-equal (gradient-spec-input-dim result) 2)
                    (assert-equal (gradient-spec-output-dim result) 1)))
            
            (define-test compose-mismatching-dimensions
              ;; g: R^2 -> R (outputs 1), f: R^3 -> R (expects 3)
              ;; Should fail: f expects 3 inputs but g produces 1
              (let* ([g-spec (scalar-fn 2 (lambda (x y) (traced-add x y)))]
                     [f-spec (scalar-fn 3 (lambda (x y z) (traced-mul x (traced-add y z))))]
                     [result (compose-gradient-fns f-spec g-spec)])
                    (assert-true (error-of-kind? result 'composition-dimension-mismatch))))
            
            (define-test compose-vector-fn-with-scalar-fn
              ;; g: R^1 -> R^2 (vector function returning list)
              ;; f: R^2 -> R (scalar function taking 2 inputs)
              ;; f o g: R^1 -> R
              ;; This tests that vector function outputs are correctly spread as args to f
              (let* ([g-spec (vector-fn 1 2 (lambda (x) (list x (traced-mul 2 x))))]
                     [f-spec (scalar-fn 2 (lambda (a b) (traced-add a b)))]
                     [composed (compose-gradient-fns f-spec g-spec)])
                    (assert-true (gradient-spec? composed))
                    (assert-equal (gradient-spec-input-dim composed) 1)
                    ;; At x=3: g(3) = (3, 6), f(3, 6) = 9
                    ;; Gradient: d(x + 2x)/dx = 3
                    (let ([result (safe-gradient composed '(3.0))])
                         (assert-true (dim-gradient? result))
                         (assert-= (car (dim-gradient-values result)) 3.0 0.01)))))

;;; ============================================================
;;; Type Annotation Generation
;;; ============================================================

(test-group type-annotation-tests
            (define-test gradient-type-construction
              (let ([t (t-gradient 3)])
                   (assert-equal t '(Vec 3 Number))))
            
            (define-test gradient-fn-type-construction
              (let ([t (t-gradient-fn 3)])
                   (assert-equal t '(-> (Vec 3 Number) (Vec 3 Number)))))
            
            (define-test jacobian-type-construction
              (let ([t (t-jacobian 2 3)])
                   (assert-equal t '(Matrix 2 3 Number))))
            
            (define-test hessian-type-construction
              (let ([t (t-hessian 4)])
                   (assert-equal t '(Matrix 4 4 Number)))))

;;; ============================================================
;;; Error Formatting
;;; ============================================================

(test-group error-formatting-tests
            (define-test format-dimension-mismatch
              (let* ([err (list 'error 'dimension-mismatch
                                '(expected 3)
                                '(got 2))]
                     [msg (format-gradient-error err)])
                    (assert-true (string? msg))
                    (assert-true (> (string-length msg) 0))))
            
            (define-test format-input-dimension-mismatch
              (let* ([err (list 'error 'input-dimension-mismatch
                                '(expected 2)
                                '(got 3))]
                     [msg (format-gradient-error err)])
                    (assert-true (string? msg)))))

;;; ============================================================
;;; Integration with Differentiable Type Class
;;; ============================================================

(test-group differentiable-integration
            (define-test gradient-spec-satisfies-constraints
              ;; A GradientSpec should provide its dimension
              (let ([spec (scalar-fn 3 (lambda (x y z) (traced-add x (traced-add y z))))])
                   (assert-equal (gradient-spec-input-dim spec) 3)))
            
            (define-test composed-spec-preserves-dimension
              ;; Composition should correctly track dimensions
              (let* ([inner (scalar-fn 2 (lambda (x y) (traced-mul x y)))]
                     [outer (scalar-fn 1 (lambda (z) (traced-exp z)))]
                     [composed (compose-gradient-fns outer inner)])
                    (when (gradient-spec? composed)
                          (assert-equal (gradient-spec-input-dim composed) 2)))))

;;; ============================================================
;;; Regression: Verify Gradients Match Expected Values
;;; ============================================================

(test-group gradient-value-regression
            (define-test rosenbrock-gradient-at-origin
              ;; Rosenbrock: f(x,y) = (1-x)^2 + 100(y-x^2)^2
              ;; At (0,0): df/dx = -2(1-0) + 100*2*(0-0)*(-2*0) = -2
              ;;          df/dy = 100*2*(0-0) = 0
              (let* ([spec (scalar-fn 2
                                      (lambda (x y)
                                              (traced-add
                                               (traced-sq (traced-sub 1 x))
                                               (traced-mul 100
                                                           (traced-sq (traced-sub y (traced-sq x)))))))]
                     [result (safe-gradient spec '(0.0 0.0))])
                    (when (dim-gradient? result)
                          (let ([vals (dim-gradient-values result)])
                               (assert-= (car vals) -2.0 0.01)
                               (assert-= (cadr vals) 0.0 0.01)))))
            
            (define-test rosenbrock-gradient-at-minimum
              ;; At minimum (1,1): gradient should be (0, 0)
              (let* ([spec (scalar-fn 2
                                      (lambda (x y)
                                              (traced-add
                                               (traced-sq (traced-sub 1 x))
                                               (traced-mul 100
                                                           (traced-sq (traced-sub y (traced-sq x)))))))]
                     [result (safe-gradient spec '(1.0 1.0))])
                    (when (dim-gradient? result)
                          (let ([vals (dim-gradient-values result)])
                               (assert-= (car vals) 0.0 0.01)
                               (assert-= (cadr vals) 0.0 0.01))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
==============================================================
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All typed gradient tests passed.
")
    (display "
[FAILURE] Some typed gradient tests failed.
"))
