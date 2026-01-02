;;; core/fp/symbolic/test-expr.ss — Tests for Symbolic Expressions
;;;
;;; NOTE: Run from project root: scheme --script core/fp/symbolic/test-expr.ss

(load "core/test-framework.ss")
(load "core/fp/symbolic/expr.ss")

(display "\n")
(display "==============================================================\n")
(display "         SYMBOLIC EXPRESSION TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Constructor Tests
;;; ============================================================

(test-group constructors
            (define-test num-constructor
              (let ([n (num 42)])
                   (assert-true (num? n))
                   (assert-equal 42 (num-val n))))
            
            (define-test var-constructor
              (let ([v (var 'x)])
                   (assert-true (var? v))
                   (assert-equal 'x (var-name v))))
            
            (define-test make-sum-empty
              (assert-equal (num 0) (make-sum '())))
            
            (define-test make-sum-single
              (let ([v (var 'x)])
                   (assert-equal v (make-sum (list v)))))
            
            (define-test make-sum-multiple
              (let ([e (make-sum (list (num 1) (num 2) (num 3)))])
                   (assert-true (sum? e))
                   (assert-equal 3 (length (sum-terms e)))))
            
            (define-test make-product-empty
              (assert-equal (num 1) (make-product '())))
            
            (define-test make-product-single
              (let ([v (var 'x)])
                   (assert-equal v (make-product (list v))))))

;;; ============================================================
;;; Predicate Tests
;;; ============================================================

(test-group predicates
            (define-test num?-test
              (assert-true (num? (num 5)))
              (assert-false (num? (var 'x))))
            
            (define-test var?-test
              (assert-true (var? (var 'x)))
              (assert-false (var? (num 5))))
            
            (define-test sum?-test
              (assert-true (sum? (list '+ (num 1) (num 2))))
              (assert-false (sum? (num 5))))
            
            (define-test product?-test
              (assert-true (product? (list '* (num 2) (var 'x))))
              (assert-false (product? (num 5))))
            
            (define-test power?-test
              (assert-true (power? (make-pow (var 'x) (num 2))))
              (assert-false (power? (num 5))))
            
            (define-test app?-test
              (assert-true (app? (sym-sin (var 'x))))
              (assert-false (app? (num 5)))
              (assert-false (app? (list '+ (num 1) (num 2))))))

;;; ============================================================
;;; Smart Constructor Tests
;;; ============================================================

(test-group smart-constructors
            (define-test sum-zero-left
              ;; 0 + x = x
              (assert-equal (var 'x) (sum (num 0) (var 'x))))
            
            (define-test sum-zero-right
              ;; x + 0 = x
              (assert-equal (var 'x) (sum (var 'x) (num 0))))
            
            (define-test sum-constants
              ;; 2 + 3 = 5
              (assert-equal (num 5) (sum (num 2) (num 3))))
            
            (define-test product-zero
              ;; 0 * x = 0
              (assert-equal (num 0) (product (num 0) (var 'x)))
              (assert-equal (num 0) (product (var 'x) (num 0))))
            
            (define-test product-one
              ;; 1 * x = x
              (assert-equal (var 'x) (product (num 1) (var 'x)))
              (assert-equal (var 'x) (product (var 'x) (num 1))))
            
            (define-test product-constants
              ;; 2 * 3 = 6
              (assert-equal (num 6) (product (num 2) (num 3))))
            
            (define-test difference-zero
              ;; x - 0 = x
              (assert-equal (var 'x) (difference (var 'x) (num 0))))
            
            (define-test difference-self
              ;; x - x = 0
              (assert-equal (num 0) (difference (var 'x) (var 'x))))
            
            (define-test quotient-one
              ;; x / 1 = x
              (assert-equal (var 'x) (quotient (var 'x) (num 1))))
            
            (define-test quotient-self
              ;; x / x = 1
              (assert-equal (num 1) (quotient (var 'x) (var 'x))))
            
            (define-test power-zero-exp
              ;; x^0 = 1
              (assert-equal (num 1) (power (var 'x) (num 0))))
            
            (define-test power-one-exp
              ;; x^1 = x
              (assert-equal (var 'x) (power (var 'x) (num 1))))
            
            (define-test power-zero-base
              ;; 0^n = 0 (n > 0)
              (assert-equal (num 0) (power (num 0) (num 5))))
            
            (define-test power-one-base
              ;; 1^n = 1
              (assert-equal (num 1) (power (num 1) (var 'n))))
            
            (define-test power-constants
              ;; 2^3 = 8
              (assert-equal (num 8) (power (num 2) (num 3)))))

;;; ============================================================
;;; Equality Tests
;;; ============================================================

(test-group equality
            (define-test num-equality
              (assert-true (expr=? (num 5) (num 5)))
              (assert-false (expr=? (num 5) (num 6))))
            
            (define-test var-equality
              (assert-true (expr=? (var 'x) (var 'x)))
              (assert-false (expr=? (var 'x) (var 'y))))
            
            (define-test sum-equality
              (let ([s1 (list '+ (num 1) (var 'x))]
                    [s2 (list '+ (num 1) (var 'x))]
                    [s3 (list '+ (num 2) (var 'x))])
                   (assert-true (expr=? s1 s2))
                   (assert-false (expr=? s1 s3))))
            
            (define-test power-equality
              (let ([p1 (make-pow (var 'x) (num 2))]
                    [p2 (make-pow (var 'x) (num 2))]
                    [p3 (make-pow (var 'x) (num 3))])
                   (assert-true (expr=? p1 p2))
                   (assert-false (expr=? p1 p3))))
            
            (define-test app-equality
              (let ([e1 (sym-sin (var 'x))]
                    [e2 (sym-sin (var 'x))]
                    [e3 (sym-cos (var 'x))])
                   (assert-true (expr=? e1 e2))
                   (assert-false (expr=? e1 e3)))))

;;; ============================================================
;;; Free Variables Tests
;;; ============================================================

(test-group free-vars
            (define-test free-vars-num
              (assert-equal '() (free-vars (num 5))))
            
            (define-test free-vars-var
              (assert-equal '(x) (free-vars (var 'x))))
            
            (define-test free-vars-sum
              (let ([fv (free-vars (list '+ (var 'x) (var 'y) (var 'x)))])
                   (assert-true (and (member 'x fv) #t))
                   (assert-true (and (member 'y fv) #t))
                   (assert-equal 2 (length fv)))) ; x appears once
            
            (define-test free-vars-nested
              (let* ([e (make-pow (list '+ (var 'x) (var 'y)) (var 'n))]
                     [fv (free-vars e)])
                    (assert-equal 3 (length fv))
                    (assert-true (and (member 'x fv) #t))
                    (assert-true (and (member 'y fv) #t))
                    (assert-true (and (member 'n fv) #t)))))

;;; ============================================================
;;; Substitution Tests
;;; ============================================================

(test-group substitution
            (define-test subst-var-match
              (let ([e (subst (var 'x) 'x (num 5))])
                   (assert-true (expr=? (num 5) e))))
            
            (define-test subst-var-no-match
              (let ([e (subst (var 'y) 'x (num 5))])
                   (assert-true (expr=? (var 'y) e))))
            
            (define-test subst-num
              (let ([e (subst (num 42) 'x (num 5))])
                   (assert-true (expr=? (num 42) e))))
            
            (define-test subst-sum
              (let* ([orig (list '+ (var 'x) (num 1))]
                     [result (subst orig 'x (num 5))])
                    (assert-true (expr=? (list '+ (num 5) (num 1)) result))))
            
            (define-test subst-nested
              (let* ([orig (make-pow (var 'x) (num 2))]
                     [result (subst orig 'x (list '+ (var 'y) (num 1)))])
                    (assert-true (power? result))
                    (assert-true (sum? (pow-base result))))))

;;; ============================================================
;;; Pattern Matching Tests
;;; ============================================================

(test-group pattern-matching
            (define-test match-wildcard
              (let ([b (match-expr (num 5) '_)])
                   (assert-true (list? b))
                   (assert-equal 0 (length b))))
            
            (define-test match-num-capture
              (let ([b (match-expr (num 42) '(num _))])
                   (assert-true (list? b))
                   (assert-equal 42 (cdr (assq '_ b)))))
            
            (define-test match-var-capture
              (let ([b (match-expr (var 'x) '(var _))])
                   (assert-true (list? b))
                   (assert-equal 'x (cdr (assq '_ b)))))
            
            (define-test match-named-wildcard
              (let ([b (match-expr (num 5) 'e)])
                   (assert-true (list? b))
                   (assert-true (expr=? (num 5) (cdr (assq 'e b))))))
            
            (define-test match-no-match
              (let ([b (match-expr (num 5) '(var _))])
                   (assert-false b))))

;;; ============================================================
;;; Display Tests
;;; ============================================================

(test-group display
            (define-test expr->string-num
              (assert-equal "42" (expr->string (num 42))))
            
            (define-test expr->string-var
              (assert-equal "x" (expr->string (var 'x))))
            
            (define-test expr->string-sum
              (let ([s (expr->string (list '+ (var 'x) (num 1)))])
                   (assert-true (string? s))
                   (assert-true (> (string-length s) 0))))
            
            (define-test expr->string-app
              (let ([s (expr->string (sym-sin (var 'x)))])
                   (assert-equal "sin(x)" s))))

;;; ============================================================
;;; Common Functions Tests
;;; ============================================================

(test-group common-functions
            (define-test sym-sin-test
              (let ([e (sym-sin (var 'x))])
                   (assert-true (app? e))
                   (assert-equal 'sin (app-fn e))))
            
            (define-test sym-cos-test
              (let ([e (sym-cos (var 'x))])
                   (assert-equal 'cos (app-fn e))))
            
            (define-test sym-exp-test
              (let ([e (sym-exp (var 'x))])
                   (assert-equal 'exp (app-fn e))))
            
            (define-test sym-log-test
              (let ([e (sym-log (var 'x))])
                   (assert-equal 'log (app-fn e)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All symbolic expression tests passed.\n")
    (display "\n[FAILURE] Some symbolic expression tests failed.\n"))
