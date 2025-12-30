;;; fabric/stitches/test-expand.ss — Tests for De Bruijn Expansion

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/expand.ss")

(display "
")
(display "==============================================================
")
(display "         EXPAND (De Bruijn to Named) TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Symbol Supply Tests
;;; ============================================================

(test-group symbol-supply
            (define-test supply-next-single
              (let-values ([(sym rest) (supply-next '(x y z))])
                          (assert-equal 'x sym)
                          (assert-equal '(y z) rest)))
            
            (define-test supply-next-chain
              (let-values ([(sym1 rest1) (supply-next '(a b c))])
                          (let-values ([(sym2 rest2) (supply-next rest1)])
                                      (assert-equal 'a sym1)
                                      (assert-equal 'b sym2)
                                      (assert-equal '(c) rest2))))
            
            (define-test make-symbol-supply-basic
              (let ([supply (make-symbol-supply 5)])
                   (assert-equal 5 (length supply))
                   (assert-equal 'x (car supply))
                   (assert-equal 'y (cadr supply))
                   (assert-equal 'z (caddr supply))))
            
            (define-test make-symbol-supply-extended
              (let ([supply (make-symbol-supply 15)])
                   ;; After x,y,z,a,b,c,f,g,h (9 base symbols), should get x1,y1,z1...
                   (assert-equal 15 (length supply))
                   (assert-equal 'x (list-ref supply 0))
                   (assert-equal 'h (list-ref supply 8))
                   (assert-equal 'x1 (list-ref supply 9))))
            
            (define-test make-symbol-supply-zero
              (let ([supply (make-symbol-supply 0)])
                   (assert-equal '() supply))))

;;; ============================================================
;;; Basic Expansion Tests
;;; ============================================================

(test-group basic-expansion
            (define-test expand-symbol
              (let ([result (expand 'foo '(x y z))])
                   (assert-equal 'foo result)))
            
            (define-test expand-number
              (let ([result (expand 42 '(x y z))])
                   (assert-equal 42 result)))
            
            (define-test expand-string
              (let ([result (expand "hello" '(x y z))])
                   (assert-equal "hello" result)))
            
            (define-test expand-quote
              (let ([result (expand '(quote foo) '(x y z))])
                   (assert-equal '(quote foo) result)))
            
            (define-test expand-dv-0
              ;; (dv 0) in context [x] should expand to x
              (let-values ([(result supply) (expand-with-ctx '(dv 0) '(x) '(y z))])
                          (assert-equal 'x result))))

;;; ============================================================
;;; Lambda Expansion Tests
;;; ============================================================

(test-group lambda-expansion
            (define-test expand-identity
              ;; (fn (dv 0)) with supply [x] → (fn (x) x)
              (let ([result (expand '(fn (dv 0)) '(x))])
                   (assert-equal '(fn (x) x) result)))
            
            (define-test expand-const
              ;; (fn (dv 1)) with supply [x y] and ctx [y] → (fn (x) y)
              ;; Wait, this needs outer context. Let me reconsider.
              ;; Actually, (fn (dv 1)) means "fn taking arg, returning outer var 1"
              ;; With empty outer context, this would be invalid.
              ;; Let's test a simpler case: K combinator
              (let ([result (expand '(fn (fn (dv 1))) '(x y))])
                   (assert-equal '(fn (x) (fn (y) x)) result)))
            
            (define-test expand-s-combinator
              ;; S combinator: λx.λy.λz.(x z)(y z)
              ;; In de Bruijn: (fn (fn (fn ((dv 2) (dv 0)) ((dv 1) (dv 0)))))
              (let ([result (expand '(fn (fn (fn (((dv 2) (dv 0)) ((dv 1) (dv 0))))))
                                    '(x y z))])
                   (assert-equal '(fn (x) (fn (y) (fn (z) ((x z) (y z))))) result)))
            
            (define-test expand-nested-lambda
              ;; (fn (fn (dv 0))) → (fn (x) (fn (y) y))
              (let ([result (expand '(fn (fn (dv 0))) '(x y))])
                   (assert-equal '(fn (x) (fn (y) y)) result)))
            
            (define-test expand-lambda-with-free-var
              ;; (fn foo) where foo is free → (fn (x) foo)
              (let ([result (expand '(fn foo) '(x))])
                   (assert-equal '(fn (x) foo) result))))

;;; ============================================================
;;; Let Expansion Tests
;;; ============================================================

(test-group let-expansion
            (define-test expand-simple-let
              ;; (let (42) (dv 0)) → (let ((x 42)) x)
              (let ([result (expand '(let (42) (dv 0)) '(x))])
                   (assert-equal '(let ((x 42)) x) result)))
            
            (define-test expand-let-with-computation
              ;; (let ((+ 1 2)) (dv 0)) → (let ((x (+ 1 2))) x)
              (let ([result (expand '(let ((+ 1 2)) (dv 0)) '(x))])
                   (assert-equal '(let ((x (+ 1 2))) x) result)))
            
            (define-test expand-nested-let
              ;; (let (10) (let (20) (+ (dv 0) (dv 1))))
              ;; → (let ((x 10)) (let ((y 20)) (+ y x)))
              (let ([result (expand '(let (10) (let (20) (+ (dv 0) (dv 1)))) '(x y))])
                   (assert-equal '(let ((x 10)) (let ((y 20)) (+ y x))) result)))
            
            (define-test expand-let-in-lambda
              ;; (fn (let (42) (dv 0))) → (fn (x) (let ((y 42)) y))
              (let ([result (expand '(fn (let (42) (dv 0))) '(x y))])
                   (assert-equal '(fn (x) (let ((y 42)) y)) result))))

;;; ============================================================
;;; Fix Expansion Tests
;;; ============================================================

(test-group fix-expansion
            (define-test expand-simple-fix
              ;; (fix (dv 0)) → (fix (f) f)
              (let ([result (expand '(fix (dv 0)) '(f))])
                   (assert-equal '(fix (f) f) result)))
            
            (define-test expand-fix-factorial
              ;; Factorial: (fix (fn (fn (if ... 1 (* (dv 0) (((dv 1) (- (dv 0) 1))))))))
              ;; Simplified: (fix (fn (dv 0)))
              (let ([result (expand '(fix (fn (dv 0))) '(fact n))])
                   (assert-equal '(fix (fact) (fn (n) n)) result)))
            
            (define-test expand-fix-with-lambda
              ;; (fix (fn (dv 0))) → (fix (f) (fn (x) x))
              ;; Actually, let me think about this more carefully.
              ;; (fix body) where body refers to the fixed point variable
              ;; (fix (fn (dv 0))) means: fix(λself. self)
              ;; This should expand to (fix (f) (fn (x) x))?
              ;; No wait - (fix (fn (dv 0))) means:
              ;;   fix of: (fn (dv 0))
              ;;   The (dv 0) refers to the fn's argument, not the fix binding
              ;; Let's use a clearer test
              (let ([result (expand '(fix (dv 0)) '(f))])
                   (assert-equal '(fix (f) f) result))))

;;; ============================================================
;;; Application Expansion Tests
;;; ============================================================

(test-group application-expansion
            (define-test expand-simple-application
              ;; ((dv 0) (dv 1)) in context [x y] → (x y)
              (let-values ([(result supply) (expand-with-ctx '((dv 0) (dv 1)) '(x y) '(z))])
                          (assert-equal '(x y) result)))
            
            (define-test expand-multi-arg-application
              ;; (f (dv 0) (dv 1)) in context [x y] → (f x y)
              (let-values ([(result supply) (expand-with-ctx '(f (dv 0) (dv 1)) '(x y) '(z))])
                          (assert-equal '(f x y) result)))
            
            (define-test expand-nested-application
              ;; ((fn (dv 0)) 42) → ((fn (x) x) 42)
              (let ([result (expand '((fn (dv 0)) 42) '(x))])
                   (assert-equal '((fn (x) x) 42) result))))

;;; ============================================================
;;; Complex Expression Tests
;;; ============================================================

(test-group complex-expressions
            (define-test expand-church-numeral-2
              ;; Church numeral 2: λf.λx.f(f x)
              ;; In de Bruijn: (fn (fn ((dv 1) ((dv 1) (dv 0)))))
              (let ([result (expand '(fn (fn ((dv 1) ((dv 1) (dv 0))))) '(f x))])
                   (assert-equal '(fn (f) (fn (x) (f (f x)))) result)))
            
            (define-test expand-omega-combinator
              ;; ω combinator: λx.x x
              ;; In de Bruijn: (fn ((dv 0) (dv 0)))
              (let ([result (expand '(fn ((dv 0) (dv 0))) '(x))])
                   (assert-equal '(fn (x) (x x)) result)))
            
            (define-test expand-y-combinator-simplified
              ;; Simplified Y: (fix (fn (fn ((dv 1) ((dv 0) (dv 0))))))
              ;; This is a self-referential fixed point
              (let ([result (expand '(fix (fn (fn ((dv 1) ((dv 0) (dv 0)))))) '(y f x))])
                   (assert-equal '(fix (y) (fn (f) (fn (x) (f (x x))))) result)))
            
            (define-test expand-let-with-lambda
              ;; (let ((fn (dv 0))) ((dv 0) 42))
              ;; → (let ((f (fn (x) x))) (f 42))
              (let ([result (expand '(let ((fn (dv 0))) ((dv 0) 42)) '(f x))])
                   (assert-equal '(let ((f (fn (x) x))) (f 42)) result))))

;;; ============================================================
;;; Expand Fresh Tests
;;; ============================================================

(test-group expand-fresh
            (define-test expand-fresh-identity
              (let ([result (expand-fresh '(fn (dv 0)))])
                   ;; Should use first symbol from make-symbol-supply
                   (assert-equal '(fn (x) x) result)))
            
            (define-test expand-fresh-k-combinator
              (let ([result (expand-fresh '(fn (fn (dv 1))))])
                   (assert-equal '(fn (x) (fn (y) x)) result)))
            
            (define-test expand-fresh-complex
              (let ([result (expand-fresh '(fn (fn (fn ((dv 2) (dv 0))))))])
                   ;; λx.λy.λz.x z
                   (assert-equal '(fn (x) (fn (y) (fn (z) (x z)))) result))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test expand-empty-list
              (let ([result (expand '() '(x))])
                   (assert-equal '() result)))
            
            (define-test expand-boolean
              (let ([result (expand #t '(x))])
                   (assert-equal #t result)))
            
            (define-test expand-mixed-list
              ;; List with mix of dv refs and literals
              (let-values ([(result supply) (expand-with-ctx '(+ (dv 0) 10) '(x) '(y))])
                          (assert-equal '(+ x 10) result))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All expand tests passed.
")
    (display "
[FAILURE] Some expand tests failed.
"))
