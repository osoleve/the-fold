;;; core/autodiff/test-higher-order-diff.ss --- Tests for Higher-Order Differentiation

(load "core/test-framework.ss")
(load "core/autodiff/higher-order-diff.ss")

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

;;; Helper: check matrix element
(define (assert-matrix-= m i j expected tolerance)
  (assert-= (matrix-ref m i j) expected tolerance))

(display "
==============================================================
         HIGHER-ORDER DIFFERENTIATION TESTS
==============================================================
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
              ;; f(x,y) = (x^2, xy, y^2)
              ;; J at (2,3) = [[4,0],[3,2],[0,6]]
              (let* ([f (lambda (x y)
                                (list (traced-sq x)
                                      (traced-mul x y)
                                      (traced-sq y)))]
                     [J (jacobian f '(2 3))])
                    (assert-equal 3 (matrix-rows J))
                    (assert-equal 2 (matrix-cols J))
                    (assert-matrix-= J 0 0 4 0.0001)  ; d(x^2)/dx = 2x = 4
                    (assert-matrix-= J 0 1 0 0.0001)  ; d(x^2)/dy = 0
                    (assert-matrix-= J 1 0 3 0.0001)  ; d(xy)/dx = y = 3
                    (assert-matrix-= J 1 1 2 0.0001)  ; d(xy)/dy = x = 2
                    (assert-matrix-= J 2 0 0 0.0001)  ; d(y^2)/dx = 0
                    (assert-matrix-= J 2 1 6 0.0001)))  ; d(y^2)/dy = 2y = 6
            
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
              ;; f(x,y) = xy, nabla(f) = (y, x), Jv = y*v1 + x*v2
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
              ;; f(x,y) = xy, nabla(f) = (y, x), v^T J = v * (y, x)
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
              ;; f(x,y) = x^2 + y^2, H = [[2,0],[0,2]]
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
              ;; f(x) = x^3, H = [[6x]]
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
              ;; f(x,y) = x^2 + y^2, nabla(f) = (2x, 2y)
              ;; At (3,4) in direction (1,0): nabla(f) . v = 6
              (let* ([f (lambda (x y) (traced-add (traced-sq x) (traced-sq y)))]
                     [dd (directional-derivative f '(3 4) '(1 0))])
                    (assert-= dd 6 0.0001)))
            
            (define-test directional-derivative-normalized
              ;; f(x,y) = x^2 + y^2, nabla(f) at (3,4) = (6, 8)
              ;; Direction (0.6, 0.8) (normalized): dd = 6*0.6 + 8*0.8 = 10
              (let* ([f (lambda (x y) (traced-add (traced-sq x) (traced-sq y)))]
                     [dd (directional-derivative f '(3 4) '(0.6 0.8))])
                    (assert-= dd 10 0.0001))))

;;; ============================================================
;;; Second Derivative (Single Variable)
;;; ============================================================

(test-group second-derivative-tests
            (define-test second-derivative-quadratic
              ;; f(x) = x^2, f''(x) = 2
              (let* ([f (lambda (x) (traced-sq x))]
                     [d2f (second-derivative f 5)])
                    (assert-= d2f 2 0.1)))
            
            (define-test second-derivative-cubic
              ;; f(x) = x^3, f''(x) = 6x
              ;; At x=2: f''(2) = 12
              (let* ([f (lambda (x) (traced-pow x 3))]
                     [d2f (second-derivative f 2)])
                    (assert-= d2f 12 0.5))))

;;; ============================================================
;;; Exact Hessian Tests (via Hyperdual Numbers)
;;; ============================================================

(test-group exact-hessian-tests
            (define-test hessian-exact-quadratic
              ;; f(x,y) = x^2 + y^2, H = [[2, 0], [0, 2]]
              ;; Should be EXACT (no tolerance needed)
              (let* ([f (lambda (x y) (hd-add (hd-sq x) (hd-sq y)))]
                     [h (hessian-exact f '(3 4))])
                    (assert-equal 2 (matrix-ref h 0 0))
                    (assert-equal 0 (matrix-ref h 0 1))
                    (assert-equal 0 (matrix-ref h 1 0))
                    (assert-equal 2 (matrix-ref h 1 1))))
            
            (define-test hessian-exact-mixed
              ;; f(x,y) = x*y, H = [[0, 1], [1, 0]]
              ;; Should be EXACT
              (let* ([f (lambda (x y) (hd-mul x y))]
                     [h (hessian-exact f '(5 7))])
                    (assert-equal 0 (matrix-ref h 0 0))
                    (assert-equal 1 (matrix-ref h 0 1))
                    (assert-equal 1 (matrix-ref h 1 0))
                    (assert-equal 0 (matrix-ref h 1 1))))
            
            (define-test hessian-exact-vs-numerical-precision
              ;; Compare exact vs numerical for f(x,y) = x^2*y + y^3
              ;; H = [[2y, 2x], [2x, 6y]] at (2,3): [[6, 4], [4, 18]]
              ;; Exact should be precise; numerical has ~1e-6 error
              (let* ([f-hd (lambda (x y) (hd-add (hd-mul (hd-sq x) y)
                                                 (hd-pow y 3)))]
                     [h-exact (hessian-exact f-hd '(2 3))])
                    ;; These should be exactly correct
                    (assert-equal 6 (matrix-ref h-exact 0 0))
                    (assert-equal 4 (matrix-ref h-exact 0 1))
                    (assert-equal 4 (matrix-ref h-exact 1 0))
                    (assert-equal 18 (matrix-ref h-exact 1 1))))
            
            (define-test second-derivative-exact-polynomial
              ;; f(x) = x^3, f''(x) = 6x
              ;; At x=2: f''(2) = 12 EXACTLY
              (let ([d2f (second-derivative-exact (lambda (x) (hd-pow x 3)) 2)])
                   (assert-equal 12 d2f)))
            
            (define-test second-derivative-exact-exp
              ;; f(x) = e^x, f''(x) = e^x
              ;; At x=0: f''(0) = 1 (very close)
              (let ([d2f (second-derivative-exact hd-exp 0)])
                   (assert-= d2f 1.0 1e-15)))
            
            (define-test second-derivative-exact-sin
              ;; f(x) = sin(x), f''(x) = -sin(x)
              ;; At x=0: f''(0) = 0
              (let ([d2f (second-derivative-exact hd-sin 0)])
                   (assert-= d2f 0.0 1e-15))))

;;; ============================================================
;;; Jet Number Basic Tests
;;; ============================================================

(test-group jet-basic-tests
            (define-test jet-creation
              ;; Create a jet and check accessors
              (let ([j (jet '(1 2 3))])
                   (assert-true (jet? j))
                   (assert-equal 2 (jet-order j))
                   (assert-= (jet-coeff j 0) 1 1e-15)
                   (assert-= (jet-coeff j 1) 2 1e-15)
                   (assert-= (jet-coeff j 2) 3 1e-15)))
            
            (define-test jet-variable-creation
              ;; jet-variable creates [x, 1, 0, 0, ...]
              (let ([j (jet-variable 5 3)])
                   (assert-= (jet-value j) 5 1e-15)
                   (assert-= (jet-coeff j 1) 1 1e-15)
                   (assert-= (jet-coeff j 2) 0 1e-15)
                   (assert-= (jet-coeff j 3) 0 1e-15)))
            
            (define-test jet-lift
              ;; Lifting a constant
              (let ([j (jet-lift 5)])
                   (assert-= (jet-value j) 5 1e-15)
                   (assert-= (jet-coeff j 1) 0 1e-15)))
            
            (define-test jet-deriv-extraction
              ;; jet-deriv multiplies by factorial
              (let ([j (jet '(1 2 3))]) ; represents f(x) where c0=1, c1=2, c2=3
                   (assert-= (jet-deriv j 0) 1 1e-15)     ; 0! * 1 = 1
                   (assert-= (jet-deriv j 1) 2 1e-15)     ; 1! * 2 = 2
                   (assert-= (jet-deriv j 2) 6 1e-15))))  ; 2! * 3 = 6

;;; ============================================================
;;; Jet Arithmetic Tests
;;; ============================================================

(test-group jet-arithmetic-tests
            (define-test jet-add-simple
              (let* ([a (jet '(1 2 3))]
                     [b (jet '(4 5 6))]
                     [c (jet-add a b)])
                    (assert-= (jet-coeff c 0) 5 1e-15)
                    (assert-= (jet-coeff c 1) 7 1e-15)
                    (assert-= (jet-coeff c 2) 9 1e-15)))
            
            (define-test jet-sub-simple
              (let* ([a (jet '(5 3 1))]
                     [b (jet '(1 2 3))]
                     [c (jet-sub a b)])
                    (assert-= (jet-coeff c 0) 4 1e-15)
                    (assert-= (jet-coeff c 1) 1 1e-15)
                    (assert-= (jet-coeff c 2) -2 1e-15)))
            
            (define-test jet-neg
              (let* ([a (jet '(1 -2 3))]
                     [b (jet-neg a)])
                    (assert-= (jet-coeff b 0) -1 1e-15)
                    (assert-= (jet-coeff b 1) 2 1e-15)
                    (assert-= (jet-coeff b 2) -3 1e-15)))
            
            (define-test jet-mul-simple
              ;; (1 + 2e) * (3 + 4e) = 3 + (1*4 + 2*3)e + ... = 3 + 10e
              (let* ([a (jet '(1 2))]
                     [b (jet '(3 4))]
                     [c (jet-mul a b)])
                    (assert-= (jet-coeff c 0) 3 1e-15)
                    (assert-= (jet-coeff c 1) 10 1e-15)))
            
            (define-test jet-sq-polynomial
              ;; (x + 1e)^2 = x^2 + 2x*e at x=3: 9 + 6e
              (let* ([j (jet-variable 3 2)]
                     [s (jet-sq j)])
                    (assert-= (jet-coeff s 0) 9 1e-15)
                    (assert-= (jet-coeff s 1) 6 1e-15))) ; d(x^2)/dx = 2x = 6
            
            (define-test jet-div-simple
              ;; (6 + 4e) / (2 + 0e) = 3 + 2e
              (let* ([a (jet '(6 4))]
                     [b (jet '(2 0))]
                     [c (jet-div a b)])
                    (assert-= (jet-coeff c 0) 3 1e-15)
                    (assert-= (jet-coeff c 1) 2 1e-15))))

;;; ============================================================
;;; Jet Transcendental Function Tests
;;; ============================================================

(test-group jet-transcendental-tests
            (define-test jet-exp-at-zero
              ;; e^x at x=0: value=1, deriv=1, second=1
              (let* ([j (jet-variable 0 3)]
                     [e (jet-exp j)])
                    (assert-= (jet-value e) 1 1e-12)
                    (assert-= (jet-deriv e 1) 1 1e-12)
                    (assert-= (jet-deriv e 2) 1 1e-12)
                    (assert-= (jet-deriv e 3) 1 1e-12)))
            
            (define-test jet-exp-at-one
              ;; e^x at x=1: all derivatives = e
              (let* ([j (jet-variable 1 2)]
                     [e (jet-exp j)]
                     [expected (exp 1)])
                    (assert-= (jet-value e) expected 1e-12)
                    (assert-= (jet-deriv e 1) expected 1e-12)
                    (assert-= (jet-deriv e 2) expected 1e-12)))
            
            (define-test jet-sin-at-zero
              ;; sin(x) at x=0: value=0, deriv=1, second=0, third=-1
              (let* ([j (jet-variable 0 4)]
                     [s (jet-sin j)])
                    (assert-= (jet-value s) 0 1e-12)
                    (assert-= (jet-deriv s 1) 1 1e-12)   ; cos(0)=1
                    (assert-= (jet-deriv s 2) 0 1e-12)   ; -sin(0)=0
                    (assert-= (jet-deriv s 3) -1 1e-12)  ; -cos(0)=-1
                    (assert-= (jet-deriv s 4) 0 1e-12))) ; sin(0)=0
            
            (define-test jet-cos-at-zero
              ;; cos(x) at x=0: value=1, deriv=0, second=-1, third=0
              (let* ([j (jet-variable 0 4)]
                     [c (jet-cos j)])
                    (assert-= (jet-value c) 1 1e-12)
                    (assert-= (jet-deriv c 1) 0 1e-12)    ; -sin(0)=0
                    (assert-= (jet-deriv c 2) -1 1e-12)   ; -cos(0)=-1
                    (assert-= (jet-deriv c 3) 0 1e-12)    ; sin(0)=0
                    (assert-= (jet-deriv c 4) 1 1e-12)))  ; cos(0)=1
            
            (define-test jet-log-at-one
              ;; log(x) at x=1: value=0, deriv=1, second=-1, third=2
              (let* ([j (jet-variable 1 3)]
                     [l (jet-log j)])
                    (assert-= (jet-value l) 0 1e-12)
                    (assert-= (jet-deriv l 1) 1 1e-12)    ; 1/x = 1
                    (assert-= (jet-deriv l 2) -1 1e-12)   ; -1/x^2 = -1
                    (assert-= (jet-deriv l 3) 2 1e-12)))  ; 2/x^3 = 2
            
            (define-test jet-sqrt-at-four
              ;; sqrt(x) at x=4: value=2, deriv=1/4, second=-1/32
              ;; f(x) = sqrt(x), f'(x) = 1/(2*sqrt(x)), f''(x) = -1/(4*x^(3/2))
              ;; At x=4: f(4)=2, f'(4)=1/4=0.25, f''(4)=-1/(4*8)=-1/32=-0.03125
              (let* ([j (jet-variable 4 2)]
                     [s (jet-sqrt j)])
                    (assert-= (jet-value s) 2 1e-12)
                    (assert-= (jet-deriv s 1) 0.25 1e-12)      ; 1/(2*sqrt(4)) = 0.25
                    (assert-= (inexact (jet-deriv s 2)) -0.03125 1e-10))) ; -1/(4*8) = -1/32
            
            (define-test jet-pow-cubic
              ;; x^3 at x=2: value=8, deriv=12, second=12, third=6
              (let* ([j (jet-variable 2 3)]
                     [p (jet-pow j 3)])
                    (assert-= (jet-value p) 8 1e-12)
                    (assert-= (jet-deriv p 1) 12 1e-12)  ; 3x^2 = 12
                    (assert-= (jet-deriv p 2) 12 1e-12)  ; 6x = 12
                    (assert-= (jet-deriv p 3) 6 1e-12))))  ; 6

;;; ============================================================
;;; nth-Derivative Tests
;;; ============================================================

(test-group nth-derivative-tests
            (define-test nth-deriv-polynomial
              ;; f(x) = x^4, f'(x) = 4x^3, f''(x) = 12x^2, f'''(x) = 24x, f''''(x) = 24
              (let ([f (lambda (x) (jet-pow x 4))])
                   (assert-= (nth-derivative f 2 0) 16 1e-12)   ; x^4 = 16
                   (assert-= (nth-derivative f 2 1) 32 1e-12)   ; 4x^3 = 32
                   (assert-= (nth-derivative f 2 2) 48 1e-12)   ; 12x^2 = 48
                   (assert-= (nth-derivative f 2 3) 48 1e-12)   ; 24x = 48
                   (assert-= (nth-derivative f 2 4) 24 1e-12))) ; 24
            
            (define-test nth-deriv-exp
              ;; All derivatives of e^x are e^x
              (let ([f jet-exp])
                   (assert-= (nth-derivative f 0 0) 1 1e-12)
                   (assert-= (nth-derivative f 0 1) 1 1e-12)
                   (assert-= (nth-derivative f 0 5) 1 1e-12)
                   (assert-= (nth-derivative f 1 0) (exp 1) 1e-12)
                   (assert-= (nth-derivative f 1 3) (exp 1) 1e-12)))
            
            (define-test nth-deriv-sin-pattern
              ;; sin derivatives: sin, cos, -sin, -cos, sin, ...
              (let ([f jet-sin]
                    [pi/2 (/ 3.141592653589793 2)])
                   ;; At x=0
                   (assert-= (nth-derivative f 0 0) 0 1e-12)   ; sin(0)
                   (assert-= (nth-derivative f 0 1) 1 1e-12)   ; cos(0)
                   (assert-= (nth-derivative f 0 2) 0 1e-12)   ; -sin(0)
                   (assert-= (nth-derivative f 0 3) -1 1e-12)  ; -cos(0)
                   (assert-= (nth-derivative f 0 4) 0 1e-12)   ; sin(0)
                   (assert-= (nth-derivative f 0 5) 1 1e-12))) ; cos(0)
            
            (define-test all-derivatives-test
              ;; all-derivatives returns list of derivatives
              (let* ([f jet-exp]
                     [derivs (all-derivatives f 0 4)])
                    (assert-equal 5 (length derivs))
                    (assert-= (car derivs) 1 1e-12)           ; f(0)
                    (assert-= (cadr derivs) 1 1e-12)          ; f'(0)
                    (assert-= (caddr derivs) 1 1e-12)         ; f''(0)
                    (assert-= (cadddr derivs) 1 1e-12)        ; f'''(0)
                    (assert-= (car (cddddr derivs)) 1 1e-12)))) ; f''''(0)

;;; ============================================================
;;; Taylor Series Tests
;;; ============================================================

(test-group taylor-series-tests
            (define-test taylor-coefficients-exp
              ;; Taylor coefficients of e^x at 0: 1, 1, 1/2, 1/6, 1/24, ...
              (let ([coeffs (taylor-coefficients jet-exp 0 4)])
                   (assert-= (list-ref coeffs 0) 1 1e-12)
                   (assert-= (list-ref coeffs 1) 1 1e-12)
                   (assert-= (list-ref coeffs 2) 0.5 1e-12)
                   (assert-= (list-ref coeffs 3) (/ 1 6) 1e-12)
                   (assert-= (list-ref coeffs 4) (/ 1 24) 1e-12)))
            
            (define-test taylor-coefficients-sin
              ;; Taylor coefficients of sin(x) at 0: 0, 1, 0, -1/6, 0, 1/120, ...
              (let ([coeffs (taylor-coefficients jet-sin 0 5)])
                   (assert-= (list-ref coeffs 0) 0 1e-12)
                   (assert-= (list-ref coeffs 1) 1 1e-12)
                   (assert-= (list-ref coeffs 2) 0 1e-12)
                   (assert-= (list-ref coeffs 3) (/ -1 6) 1e-12)
                   (assert-= (list-ref coeffs 4) 0 1e-12)
                   (assert-= (list-ref coeffs 5) (/ 1 120) 1e-12)))
            
            (define-test taylor-series-approximation
              ;; Taylor series of e^x at 0 should approximate e^0.1
              (let* ([f-approx (taylor-series jet-exp 0 10)]
                     [actual (exp 0.1)]
                     [approx (f-approx 0.1)])
                    (assert-= approx actual 1e-10)))
            
            (define-test taylor-series-sin-approximation
              ;; Taylor series of sin(x) at 0 should approximate sin(0.5)
              (let* ([f-approx (taylor-series jet-sin 0 10)]
                     [actual (sin 0.5)]
                     [approx (f-approx 0.5)])
                    (assert-= approx actual 1e-10)))
            
            (define-test taylor-remainder-bound-test
              ;; Remainder bound should decrease with higher order
              (let* ([bound-5 (taylor-remainder-bound jet-exp 0 0.1 5)]
                     [bound-10 (taylor-remainder-bound jet-exp 0 0.1 10)])
                    (assert-true (< bound-10 bound-5)))))

;;; ============================================================
;;; Convenience Function Tests
;;; ============================================================

(test-group convenience-function-tests
            (define-test second-derivative-jet-test
              (let ([f (lambda (x) (jet-pow x 3))])
                   (assert-= (second-derivative-jet f 2) 12 1e-12)))  ; 6x = 12
            
            (define-test third-derivative-jet-test
              (let ([f (lambda (x) (jet-pow x 4))])
                   (assert-= (third-derivative-jet f 2) 48 1e-12)))  ; 24x = 48
            
            (define-test fourth-derivative-jet-test
              (let ([f (lambda (x) (jet-pow x 5))])
                   (assert-= (fourth-derivative-jet f 1) 120 1e-12)))) ; 120

;;; ============================================================
;;; Multivariate Partial Derivative Tests
;;; ============================================================

(test-group partial-derivative-tests
            (define-test partial-derivative-single-var
              ;; d/dx of x^3 at x=2 is 12
              (let ([f (lambda (x) (jet-pow x 3))])
                   (assert-= (partial-derivative f '(2) 0 1) 12 1e-12)))
            
            (define-test gradient-jet-test
              ;; f(x,y) = x^2 + y^2, grad = (2x, 2y)
              (let ([f (lambda (x y) (jet-add (jet-sq x) (jet-sq y)))])
                   (let ([g (gradient-jet f '(3 4))])
                        (assert-= (car g) 6 1e-10)   ; 2x = 6
                        (assert-= (cadr g) 8 1e-10))))) ; 2y = 8

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
[SUCCESS] All higher-order differentiation tests passed.
")
    (display "
[FAILURE] Some higher-order differentiation tests failed.
"))
