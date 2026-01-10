;;; core/autodiff/test-comp-graph.ss --- Tests for Computational Graph

(load "core/test-framework.ss")
(load "core/autodiff/comp-graph.ss")

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

(display "
==============================================================
         COMPUTATIONAL GRAPH TESTS
==============================================================
")

;;; ============================================================
;;; Node Construction and Predicates
;;; ============================================================

(test-group node-tests
            (define-test node-var-creates-variable-node
              (let ([n (node-var 'x)])
                   (assert-true (node-var? n))
                   (assert-false (node-const? n))
                   (assert-false (node-op? n))
                   (assert-equal 'x (node-var-name n))))
            
            (define-test node-const-creates-constant-node
              (let ([n (node-const 42)])
                   (assert-true (node-const? n))
                   (assert-false (node-var? n))
                   (assert-equal 42 (node-const-value n))))
            
            (define-test node-op-creates-operation-node
              (let ([x (node-var 'x)]
                    [y (node-var 'y)])
                   (let ([n (node-op 'add (list x y))])
                        (assert-true (node-op? n))
                        (assert-equal 'add (node-op-name n))
                        (assert-equal 2 (length (node-op-inputs n))))))
            
            (define-test node-inputs-empty-for-var-const
              (assert-equal '() (node-inputs (node-var 'x)))
              (assert-equal '() (node-inputs (node-const 5))))
            
            (define-test node-inputs-returns-inputs-for-op
              (let* ([x (node-var 'x)]
                     [n (node-op 'sin (list x))])
                    (assert-equal 1 (length (node-inputs n))))))

;;; ============================================================
;;; Graph Construction
;;; ============================================================

(test-group graph-construction-tests
            (define-test make-comp-graph-creates-empty-graph
              (let ([g (make-comp-graph)])
                   (assert-true (comp-graph? g))
                   (assert-equal 0 (graph-node-count g))
                   (assert-false (comp-graph-output g))))
            
            (define-test graph-add-node-adds-nodes
              (let ([g0 (make-comp-graph)])
                   (let-values ([(g1 id1) (graph-add-node g0 (node-var 'x))])
                               (assert-equal 1 (graph-node-count g1))
                               (assert-equal 0 id1)
                               (let-values ([(g2 id2) (graph-add-node g1 (node-const 5))])
                                           (assert-equal 2 (graph-node-count g2))
                                           (assert-equal 1 id2)))))
            
            (define-test graph-get-node-retrieves-nodes
              (let ([g0 (make-comp-graph)]
                    [x-node (node-var 'x)])
                   (let-values ([(g1 id) (graph-add-node g0 x-node)])
                               (let ([retrieved (graph-get-node g1 id)])
                                    (assert-true (node-var? retrieved))
                                    (assert-equal 'x (node-var-name retrieved))))))
            
            (define-test graph-set-output-sets-output
              (let ([g0 (make-comp-graph)])
                   (let-values ([(g1 id) (graph-add-node g0 (node-var 'x))])
                               (let ([g2 (graph-set-output g1 id)])
                                    (assert-equal id (comp-graph-output g2))))))
            
            (define-test graph-variables-extracts-variable-names
              (let ([g0 (make-comp-graph)])
                   (let-values ([(g1 _) (graph-add-node g0 (node-var 'x))])
                               (let-values ([(g2 _) (graph-add-node g1 (node-var 'y))])
                                           (let-values ([(g3 _) (graph-add-node g2 (node-const 1))])
                                                       (let ([vars (graph-variables g3)])
                                                            (assert-equal 2 (length vars))
                                                            (assert-true (if (memq 'x vars) #t #f))
                                                            (assert-true (if (memq 'y vars) #t #f)))))))))

;;; ============================================================
;;; Dual Numbers
;;; ============================================================

(test-group dual-number-tests
            (define-test dual-creates-dual-number
              (let ([d (dual 3 1)])
                   (assert-true (dual? d))
                   (assert-equal 3 (dual-value d))
                   (assert-equal 1 (dual-deriv d))))
            
            (define-test dual-lift-lifts-constants
              (let ([d (dual-lift 5)])
                   (assert-true (dual? d))
                   (assert-equal 5 (dual-value d))
                   (assert-equal 0 (dual-deriv d))))
            
            (define-test dual-variable-creates-variable-dual
              (let ([d (dual-variable 3)])
                   (assert-equal 3 (dual-value d))
                   (assert-equal 1 (dual-deriv d)))))

;;; ============================================================
;;; Dual Arithmetic
;;; ============================================================

(test-group dual-arithmetic-tests
            (define-test dual-add-correct
              ;; (x + 2) at x=3 -> value=5, deriv=1
              (let ([result (dual-add (dual-variable 3) 2)])
                   (assert-equal 5 (dual-value result))
                   (assert-equal 1 (dual-deriv result))))
            
            (define-test dual-sub-correct
              ;; (x - 2) at x=3 -> value=1, deriv=1
              (let ([result (dual-sub (dual-variable 3) 2)])
                   (assert-equal 1 (dual-value result))
                   (assert-equal 1 (dual-deriv result))))
            
            (define-test dual-mul-correct
              ;; (x * 2) at x=3 -> value=6, deriv=2
              (let ([result (dual-mul (dual-variable 3) 2)])
                   (assert-equal 6 (dual-value result))
                   (assert-equal 2 (dual-deriv result))))
            
            (define-test dual-mul-product-rule
              ;; (x * x) at x=3 -> value=9, deriv=6 (2x)
              (let* ([x (dual-variable 3)]
                     [result (dual-mul x x)])
                    (assert-equal 9 (dual-value result))
                    (assert-equal 6 (dual-deriv result))))
            
            (define-test dual-div-correct
              ;; (x / 2) at x=4 -> value=2, deriv=0.5
              (let ([result (dual-div (dual-variable 4) 2)])
                   (assert-= (dual-value result) 2 0.0001)
                   (assert-= (dual-deriv result) 0.5 0.0001)))
            
            (define-test dual-sq-correct
              ;; x^2 at x=3 -> value=9, deriv=6
              (let ([result (dual-sq (dual-variable 3))])
                   (assert-equal 9 (dual-value result))
                   (assert-equal 6 (dual-deriv result))))
            
            (define-test dual-sqrt-correct
              ;; sqrt(x) at x=4 -> value=2, deriv=0.25
              (let ([result (dual-sqrt (dual-variable 4))])
                   (assert-= (dual-value result) 2 0.0001)
                   (assert-= (dual-deriv result) 0.25 0.0001)))
            
            (define-test dual-exp-correct
              ;; e^x at x=0 -> value=1, deriv=1
              (let ([result (dual-exp (dual-variable 0))])
                   (assert-= (dual-value result) 1 0.0001)
                   (assert-= (dual-deriv result) 1 0.0001)))
            
            (define-test dual-log-correct
              ;; ln(x) at x=e -> value=1, deriv=1/e
              (let ([result (dual-log (dual-variable (exp 1)))])
                   (assert-= (dual-value result) 1 0.0001)
                   (assert-= (dual-deriv result) (/ 1 (exp 1)) 0.0001)))
            
            (define-test dual-sin-correct
              ;; sin(x) at x=0 -> value=0, deriv=1 (cos 0)
              (let ([result (dual-sin (dual-variable 0))])
                   (assert-= (dual-value result) 0 0.0001)
                   (assert-= (dual-deriv result) 1 0.0001)))
            
            (define-test dual-cos-correct
              ;; cos(x) at x=0 -> value=1, deriv=0 (-sin 0)
              (let ([result (dual-cos (dual-variable 0))])
                   (assert-= (dual-value result) 1 0.0001)
                   (assert-= (dual-deriv result) 0 0.0001)))
            
            (define-test dual-pow-correct
              ;; x^3 at x=2 -> value=8, deriv=12 (3x^2)
              (let ([result (dual-pow (dual-variable 2) 3)])
                   (assert-equal 8 (dual-value result))
                   (assert-equal 12 (dual-deriv result))))
            
            ;; Inverse trigonometric functions
            (define-test dual-atan-correct
              ;; atan(x) at x=1 -> value=pi/4, deriv=0.5 (1/(1+1^2))
              (let ([result (dual-atan (dual-variable 1))])
                   (assert-= (dual-value result) (atan 1) 0.0001)
                   (assert-= (dual-deriv result) 0.5 0.0001)))
            
            (define-test dual-asin-correct
              ;; asin(x) at x=0.5 -> value=pi/6, deriv=1/sqrt(0.75)
              (let ([result (dual-asin (dual-variable 0.5))])
                   (assert-= (dual-value result) (asin 0.5) 0.0001)
                   (assert-= (dual-deriv result) (/ 1 (sqrt 0.75)) 0.0001)))
            
            (define-test dual-acos-correct
              ;; acos(x) at x=0.5 -> value, deriv=-1/sqrt(0.75)
              (let ([result (dual-acos (dual-variable 0.5))])
                   (assert-= (dual-value result) (acos 0.5) 0.0001)
                   (assert-= (dual-deriv result) (/ -1 (sqrt 0.75)) 0.0001)))
            
            ;; Hyperbolic functions
            (define-test dual-sinh-correct
              ;; sinh(x) at x=0 -> value=0, deriv=1 (cosh 0)
              (let ([result (dual-sinh (dual-variable 0))])
                   (assert-= (dual-value result) 0 0.0001)
                   (assert-= (dual-deriv result) 1 0.0001)))
            
            (define-test dual-cosh-correct
              ;; cosh(x) at x=0 -> value=1, deriv=0 (sinh 0)
              (let ([result (dual-cosh (dual-variable 0))])
                   (assert-= (dual-value result) 1 0.0001)
                   (assert-= (dual-deriv result) 0 0.0001)))
            
            (define-test dual-tanh-correct
              ;; tanh(x) at x=0 -> value=0, deriv=1 (sech^2 0)
              (let ([result (dual-tanh (dual-variable 0))])
                   (assert-= (dual-value result) 0 0.0001)
                   (assert-= (dual-deriv result) 1 0.0001)))
            
            (define-test dual-sinh-at-one
              ;; sinh(x) at x=1 -> value=(e-1/e)/2, deriv=cosh(1)
              (let* ([e (exp 1)]
                     [result (dual-sinh (dual-variable 1))]
                     [expected-val (/ (- e (/ 1 e)) 2)]
                     [expected-deriv (/ (+ e (/ 1 e)) 2)])
                    (assert-= (dual-value result) expected-val 0.0001)
                    (assert-= (dual-deriv result) expected-deriv 0.0001))))

;;; ============================================================
;;; Forward Mode Differentiation
;;; ============================================================

(test-group forward-mode-tests
            (define-test forward-diff-constant-function
              ;; d/dx[5] = 0
              (let ([result (forward-diff (lambda (x) (dual-lift 5)) 3)])
                   (assert-equal 0 result)))
            
            (define-test forward-diff-identity
              ;; d/dx[x] = 1
              (let ([result (forward-diff (lambda (x) x) 3)])
                   (assert-equal 1 result)))
            
            (define-test forward-diff-linear
              ;; d/dx[2x + 1] = 2
              (let ([result (forward-diff (lambda (x) (dual-add (dual-mul x 2) 1)) 5)])
                   (assert-equal 2 result)))
            
            (define-test forward-diff-polynomial
              ;; d/dx[x^2 + 3x] at x=2 -> 2x + 3 = 7
              (let ([result (forward-diff
                             (lambda (x)
                                     (dual-add (dual-sq x)
                                               (dual-mul x 3)))
                             2)])
                   (assert-equal 7 result)))
            
            (define-test forward-diff-transcendental
              ;; d/dx[sin(x)] at x=pi/4 -> cos(pi/4) approx 0.707
              (let ([result (forward-diff dual-sin (/ 3.14159265 4))])
                   (assert-= result 0.7071 0.001)))
            
            (define-test forward-diff-chain-rule
              ;; d/dx[sin(x^2)] at x=1 -> cos(1) * 2 approx 1.08
              (let ([result (forward-diff
                             (lambda (x) (dual-sin (dual-sq x)))
                             1)])
                   (assert-= result (* 2 (cos 1)) 0.001)))
            
            (define-test forward-diff-exp-log
              ;; d/dx[e^(ln x)] = d/dx[x] = 1
              (let ([result (forward-diff
                             (lambda (x) (dual-exp (dual-log x)))
                             5)])
                   (assert-= result 1 0.0001))))

;;; ============================================================
;;; Gradient Computation
;;; ============================================================

(test-group gradient-tests
            (define-test gradient-forward-partial-derivative
              ;; f(x,y) = x*y, df/dx at (3,4) = y = 4
              (let ([result (gradient-forward
                             (lambda (x y) (dual-mul x y))
                             '(3 4)
                             0)])  ; wrt x
                   (assert-equal 4 result)))
            
            (define-test gradient-forward-all-computes-gradient
              ;; f(x,y) = x*y + x, nabla(f) at (3,4) = (y+1, x) = (5, 3)
              (let ([grad (gradient-forward-all
                           (lambda (x y)
                                   (dual-add (dual-mul x y) x))
                           '(3 4))])
                   (assert-equal 2 (length grad))
                   (assert-equal 5 (car grad))   ; df/dx = y + 1
                   (assert-equal 3 (cadr grad))))) ; df/dy = x

;;; ============================================================
;;; Tape Operations
;;; ============================================================

(test-group tape-tests
            (define-test make-tape-creates-empty-tape
              (let ([t (make-tape)])
                   (assert-true (tape? t))
                   (assert-equal '() (tape-entries t))))
            
            (define-test tape-record-adds-entries
              (let* ([t0 (make-tape)]
                     [t1 (tape-record t0 2 'add '(0 1) '(1 1))])
                    (assert-equal 1 (length (tape-entries t1)))))
            
            (define-test tape-reverse-pass-computes-gradients
              ;; Compute gradient of f = x + y at (3, 4)
              ;; Tape: result=2, op=add, inputs=(0,1), local-grads=(1,1)
              (let* ([t (tape-record (make-tape) 2 'add '(0 1) '(1 1))]
                     [grads (tape-reverse-pass t 2 1)])
                    ;; df/dx = 1, df/dy = 1
                    (let ([grad-x (cdr (assoc 0 grads))]
                          [grad-y (cdr (assoc 1 grads))])
                         (assert-equal 1 grad-x)
                         (assert-equal 1 grad-y))))
            
            (define-test tape-reverse-pass-product
              ;; f = x * y: tape records mul with local-grads (y, x)
              ;; At (3, 4): local-grads = (4, 3)
              (let* ([t (tape-record (make-tape) 2 'mul '(0 1) '(4 3))]
                     [grads (tape-reverse-pass t 2 1)])
                    (let ([grad-x (cdr (assoc 0 grads))]
                          [grad-y (cdr (assoc 1 grads))])
                         (assert-equal 4 grad-x)  ; df/dx = y
                         (assert-equal 3 grad-y))))) ; df/dy = x

;;; ============================================================
;;; Graph Traversal
;;; ============================================================

(test-group traversal-tests
            (define-test graph-topological-order-on-simple-graph
              ;; Build: x -> mul -> result
              ;;        y /
              (let* ([g0 (make-comp-graph)]
                     [x-node (node-var 'x)]
                     [y-node (node-var 'y)])
                    (let-values ([(g1 x-id) (graph-add-node g0 x-node)])
                                (let-values ([(g2 y-id) (graph-add-node g1 y-node)])
                                            (let-values ([(g3 mul-id) (graph-add-node g2 (node-op 'mul (list x-node y-node)))])
                                                        (let ([order (graph-topological-order g3)])
                                                             ;; x and y should come before mul
                                                             (assert-equal 3 (length order)))))))))

;;; ============================================================
;;; Hyperdual Numbers
;;; ============================================================

(test-group hyperdual-tests
            (define-test hyperdual-construction
              (let ([h (hyperdual 2 3 4 5)])
                   (assert-true (hyperdual? h))
                   (assert-equal 2 (hd-value h))
                   (assert-equal 3 (hd-deriv1 h))
                   (assert-equal 4 (hd-deriv2 h))
                   (assert-equal 5 (hd-deriv12 h))))
            
            (define-test hyperdual-lift-constant
              (let ([h (hd-lift 5)])
                   (assert-true (hyperdual? h))
                   (assert-equal 5 (hd-value h))
                   (assert-equal 0 (hd-deriv1 h))
                   (assert-equal 0 (hd-deriv2 h))
                   (assert-equal 0 (hd-deriv12 h))))
            
            (define-test hyperdual-var1
              (let ([h (hd-var1 3)])
                   (assert-equal 3 (hd-value h))
                   (assert-equal 1 (hd-deriv1 h))
                   (assert-equal 0 (hd-deriv2 h))))
            
            (define-test hyperdual-var2
              (let ([h (hd-var2 3)])
                   (assert-equal 3 (hd-value h))
                   (assert-equal 0 (hd-deriv1 h))
                   (assert-equal 1 (hd-deriv2 h))))
            
            (define-test hyperdual-add
              ;; (2 + 1*e1) + (3 + 1*e2) = 5 + 1*e1 + 1*e2
              (let ([h (hd-add (hd-var1 2) (hd-var2 3))])
                   (assert-equal 5 (hd-value h))
                   (assert-equal 1 (hd-deriv1 h))
                   (assert-equal 1 (hd-deriv2 h))
                   (assert-equal 0 (hd-deriv12 h))))
            
            (define-test hyperdual-mul-gives-mixed-partial
              ;; f(x,y) = x*y, d^2f/dxdy = 1
              ;; (x + e1) * (y + e2) = xy + y*e1 + x*e2 + 1*e1e2
              (let ([h (hd-mul (hd-var1 2) (hd-var2 3))])
                   (assert-equal 6 (hd-value h))    ; 2*3
                   (assert-equal 3 (hd-deriv1 h))   ; df/dx = y = 3
                   (assert-equal 2 (hd-deriv2 h))   ; df/dy = x = 2
                   (assert-equal 1 (hd-deriv12 h)))) ; d^2f/dxdy = 1
            
            (define-test hyperdual-sq-second-derivative
              ;; f(x) = x^2, d^2f/dx^2 = 2
              (let ([h (hd-sq (hd-var12 3))])
                   (assert-equal 9 (hd-value h))    ; 3^2
                   (assert-equal 6 (hd-deriv1 h))   ; 2*3 = 6
                   (assert-equal 6 (hd-deriv2 h))
                   (assert-equal 2 (hd-deriv12 h)))) ; d^2(x^2)/dx^2 = 2
            
            (define-test hyperdual-exp-second-derivative
              ;; f(x) = e^x, d^2f/dx^2 = e^x
              (let* ([h (hd-exp (hd-var12 0))]
                     [expected 1.0]) ; e^0 = 1
                    (assert-= (hd-value h) expected 1e-10)
                    (assert-= (hd-deriv12 h) expected 1e-10)))
            
            (define-test hyperdual-sin-second-derivative
              ;; f(x) = sin(x), d^2f/dx^2 = -sin(x)
              ;; At x = pi/2: sin(pi/2) = 1, d^2/dx^2 = -1
              (let* ([x (/ 3.141592653589793 2)]
                     [h (hd-sin (hd-var12 x))])
                    (assert-= (hd-value h) 1.0 1e-10)
                    (assert-= (hd-deriv12 h) -1.0 1e-10)))
            
            (define-test hyperdual-polynomial-hessian
              ;; f(x,y) = x^2*y, H[0,1] = d^2f/dxdy = 2x
              ;; At (3, 4): H[0,1] = 6
              (let* ([x (hd-var1 3)]
                     [y (hd-var2 4)]
                     [f (hd-mul (hd-sq x) y)])
                    (assert-equal 36 (hd-value f))   ; 9*4
                    (assert-equal 6 (hd-deriv12 f))))) ; 2*3 = 6

(test-group hessian-forward-tests
            (define-test hessian-forward-quadratic
              ;; f(x,y) = x^2 + y^2, H = [[2, 0], [0, 2]]
              (let* ([f (lambda (x y) (hd-add (hd-sq x) (hd-sq y)))]
                     [h (hessian-forward f '(1 1))])
                    (assert-equal 'matrix (car h))
                    (let ([data (cadddr h)])
                         (assert-equal 2 (vector-ref data 0))  ; H[0,0]
                         (assert-equal 0 (vector-ref data 1))  ; H[0,1]
                         (assert-equal 0 (vector-ref data 2))  ; H[1,0]
                         (assert-equal 2 (vector-ref data 3))))) ; H[1,1]
            
            (define-test hessian-forward-mixed
              ;; f(x,y) = x*y, H = [[0, 1], [1, 0]]
              (let* ([f (lambda (x y) (hd-mul x y))]
                     [h (hessian-forward f '(2 3))])
                    (let ([data (cadddr h)])
                         (assert-equal 0 (vector-ref data 0))  ; H[0,0]
                         (assert-equal 1 (vector-ref data 1))  ; H[0,1]
                         (assert-equal 1 (vector-ref data 2))  ; H[1,0]
                         (assert-equal 0 (vector-ref data 3))))) ; H[1,1]
            
            (define-test hessian-forward-cubic
              ;; f(x,y) = x^2*y + y^3, H = [[2y, 2x], [2x, 6y]]
              ;; At (2, 3): H = [[6, 4], [4, 18]]
              (let* ([f (lambda (x y) (hd-add (hd-mul (hd-sq x) y)
                                              (hd-pow y 3)))]
                     [h (hessian-forward f '(2 3))])
                    (let ([data (cadddr h)])
                         (assert-equal 6 (vector-ref data 0))   ; 2*3
                         (assert-equal 4 (vector-ref data 1))   ; 2*2
                         (assert-equal 4 (vector-ref data 2))   ; 2*2
                         (assert-equal 18 (vector-ref data 3))))) ; 6*3
            
            (define-test second-derivative-forward-cubic
              ;; f(x) = x^3, d^2f/dx^2 = 6x
              ;; At x = 2: d^2f/dx^2 = 12
              (let ([result (second-derivative-forward
                             (lambda (x) (hd-pow x 3))
                             2)])
                   (assert-equal 12 result))))

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
[SUCCESS] All computational graph tests passed.
")
    (display "
[FAILURE] Some computational graph tests failed.
"))
