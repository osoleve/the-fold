;;; fabric/stitches/test-comp-graph.ss — Tests for Computational Graph

(load "test-framework.ss")
(load "comp-graph.ss")

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

(display "
══════════════════════════════════════════════════════════
         COMPUTATIONAL GRAPH TESTS
══════════════════════════════════════════════════════════
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
              ;; (x + 2) at x=3 → value=5, deriv=1
              (let ([result (dual-add (dual-variable 3) 2)])
                   (assert-equal 5 (dual-value result))
                   (assert-equal 1 (dual-deriv result))))
            
            (define-test dual-sub-correct
              ;; (x - 2) at x=3 → value=1, deriv=1
              (let ([result (dual-sub (dual-variable 3) 2)])
                   (assert-equal 1 (dual-value result))
                   (assert-equal 1 (dual-deriv result))))
            
            (define-test dual-mul-correct
              ;; (x * 2) at x=3 → value=6, deriv=2
              (let ([result (dual-mul (dual-variable 3) 2)])
                   (assert-equal 6 (dual-value result))
                   (assert-equal 2 (dual-deriv result))))
            
            (define-test dual-mul-product-rule
              ;; (x * x) at x=3 → value=9, deriv=6 (2x)
              (let* ([x (dual-variable 3)]
                     [result (dual-mul x x)])
                    (assert-equal 9 (dual-value result))
                    (assert-equal 6 (dual-deriv result))))
            
            (define-test dual-div-correct
              ;; (x / 2) at x=4 → value=2, deriv=0.5
              (let ([result (dual-div (dual-variable 4) 2)])
                   (assert-= (dual-value result) 2 0.0001)
                   (assert-= (dual-deriv result) 0.5 0.0001)))
            
            (define-test dual-sq-correct
              ;; x² at x=3 → value=9, deriv=6
              (let ([result (dual-sq (dual-variable 3))])
                   (assert-equal 9 (dual-value result))
                   (assert-equal 6 (dual-deriv result))))
            
            (define-test dual-sqrt-correct
              ;; √x at x=4 → value=2, deriv=0.25
              (let ([result (dual-sqrt (dual-variable 4))])
                   (assert-= (dual-value result) 2 0.0001)
                   (assert-= (dual-deriv result) 0.25 0.0001)))
            
            (define-test dual-exp-correct
              ;; e^x at x=0 → value=1, deriv=1
              (let ([result (dual-exp (dual-variable 0))])
                   (assert-= (dual-value result) 1 0.0001)
                   (assert-= (dual-deriv result) 1 0.0001)))
            
            (define-test dual-log-correct
              ;; ln(x) at x=e → value=1, deriv=1/e
              (let ([result (dual-log (dual-variable (exp 1)))])
                   (assert-= (dual-value result) 1 0.0001)
                   (assert-= (dual-deriv result) (/ 1 (exp 1)) 0.0001)))
            
            (define-test dual-sin-correct
              ;; sin(x) at x=0 → value=0, deriv=1 (cos 0)
              (let ([result (dual-sin (dual-variable 0))])
                   (assert-= (dual-value result) 0 0.0001)
                   (assert-= (dual-deriv result) 1 0.0001)))
            
            (define-test dual-cos-correct
              ;; cos(x) at x=0 → value=1, deriv=0 (-sin 0)
              (let ([result (dual-cos (dual-variable 0))])
                   (assert-= (dual-value result) 1 0.0001)
                   (assert-= (dual-deriv result) 0 0.0001)))
            
            (define-test dual-pow-correct
              ;; x³ at x=2 → value=8, deriv=12 (3x²)
              (let ([result (dual-pow (dual-variable 2) 3)])
                   (assert-equal 8 (dual-value result))
                   (assert-equal 12 (dual-deriv result)))))

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
              ;; d/dx[x² + 3x] at x=2 → 2x + 3 = 7
              (let ([result (forward-diff
                             (lambda (x)
                                     (dual-add (dual-sq x)
                                               (dual-mul x 3)))
                             2)])
                   (assert-equal 7 result)))
            
            (define-test forward-diff-transcendental
              ;; d/dx[sin(x)] at x=π/4 → cos(π/4) ≈ 0.707
              (let ([result (forward-diff dual-sin (/ 3.14159265 4))])
                   (assert-= result 0.7071 0.001)))
            
            (define-test forward-diff-chain-rule
              ;; d/dx[sin(x²)] at x=1 → cos(1) * 2 ≈ 1.08
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
              ;; f(x,y) = x*y, ∂f/∂x at (3,4) = y = 4
              (let ([result (gradient-forward
                             (lambda (x y) (dual-mul x y))
                             '(3 4)
                             0)])  ; wrt x
                   (assert-equal 4 result)))
            
            (define-test gradient-forward-all-computes-gradient
              ;; f(x,y) = x*y + x, ∇f at (3,4) = (y+1, x) = (5, 3)
              (let ([grad (gradient-forward-all
                           (lambda (x y)
                                   (dual-add (dual-mul x y) x))
                           '(3 4))])
                   (assert-equal 2 (length grad))
                   (assert-equal 5 (car grad))   ; ∂f/∂x = y + 1
                   (assert-equal 3 (cadr grad))))) ; ∂f/∂y = x

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
                    ;; ∂f/∂x = 1, ∂f/∂y = 1
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
                         (assert-equal 4 grad-x)  ; ∂f/∂x = y
                         (assert-equal 3 grad-y))))) ; ∂f/∂y = x

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
[SUCCESS] All computational graph tests passed.
")
    (display "
[FAILURE] Some computational graph tests failed.
"))
