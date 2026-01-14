;;; shell/tests/test-refactor.ss -- Tests for Refactoring Toolkit

;;; NOTE: Run from project root:
;;;   scheme --script shell/tests/test-refactor.ss

(load "core/base/prelude.ss")
(load "core/test-framework.ss")
(load "shell/tools/refactor.ss")

(display "
")
(display "====
")
(display "          REFACTORING TOOLKIT TESTS
")
(display "====
")

;;; ====
;;; Source Location Tests
;;; ====

(test-group loc-tests
            (define-test make-loc-test
              (let ([l (make-loc 'foo "test.ss" 10 5)])
                   (assert-true (loc? l))
                   (assert-equal 'foo (loc-value l))
                   (assert-equal "test.ss" (loc-file l))
                   (assert-equal 10 (loc-line l))
                   (assert-equal 5 (loc-col l))))
            
            (define-test loc-predicate-test
              (assert-true (loc? (make-loc 'x "f" 1 1)))
              (assert-false (loc? 'not-a-loc))
              (assert-false (loc? '(1 2 3)))))

;;; ====
;;; Change Tests
;;; ====

(test-group change-tests
            (define-test make-change-test
              (let ([c (make-change 'rename "test.ss" "old" "new" 10 5)])
                   (assert-true (change? c))
                   (assert-equal 'rename (change-type c))
                   (assert-equal "test.ss" (change-file c))
                   (assert-equal "old" (change-old c))
                   (assert-equal "new" (change-new c))))
            
            (define-test change-to-string-test
              (let* ([c (make-change 'rename "test.ss" "foo" "bar" 10 5)]
                     [s (change->string c)])
                    (assert-true (string? s))
                    (assert-true (> (string-length s) 0)))))

;;; ====
;;; S-Expression Traversal Tests
;;; ====

(test-group sexp-traversal-tests
            (define-test sexp-symbols-test
              (let ([syms (sexp-symbols '(define (foo x) (+ x 1)))])
                   (assert-true (if (member 'define syms) #t #f))
                   (assert-true (if (member 'foo syms) #t #f))
                   (assert-true (if (member 'x syms) #t #f))
                   (assert-true (if (member '+ syms) #t #f))))
            
            (define-test sexp-symbols-empty-test
              (assert-equal '() (sexp-symbols 42))
              (assert-equal '() (sexp-symbols "string")))
            
            (define-test sexp-find-symbol-test
              (let ([paths (sexp-find-symbol '(+ x (* x y)) 'x)])
                   (assert-equal 2 (length paths))))
            
            (define-test sexp-at-path-test
              (let ([sexp '(a (b c) d)])
                   (assert-equal 'a (sexp-at-path sexp '(car)))
                   (assert-equal '(b c) (sexp-at-path sexp '(cdr car)))
                   (assert-equal 'b (sexp-at-path sexp '(cdr car car)))))
            
            (define-test sexp-replace-at-path-test
              (let ([sexp '(a (b c) d)])
                   (assert-equal '(X (b c) d)
                                 (sexp-replace-at-path sexp '(car) 'X))
                   (assert-equal '(a (X c) d)
                                 (sexp-replace-at-path sexp '(cdr car car) 'X)))))

;;; ====
;;; Rename Symbol Tests
;;; ====

(test-group rename-tests
            (define-test rename-simple-test
              (assert-equal '(+ y 1)
                            (rename-symbol '(+ x 1) 'x 'y)))
            
            (define-test rename-multiple-test
              (assert-equal '(+ y (* y z))
                            (rename-symbol '(+ x (* x z)) 'x 'y)))
            
            (define-test rename-nested-test
              (assert-equal '(lambda (y) (+ y 1))
                            (rename-symbol '(lambda (x) (+ x 1)) 'x 'y)))
            
            (define-test rename-no-match-test
              (assert-equal '(+ a b)
                            (rename-symbol '(+ a b) 'x 'y)))
            
            (define-test rename-preserves-structure-test
              (let ([original '(define (foo x y) (+ x y))]
                    [renamed (rename-symbol '(define (foo x y) (+ x y)) 'foo 'bar)])
                   (assert-equal 'define (car renamed))
                   (assert-equal 'bar (caadr renamed)))))

;;; ====
;;; Extract Function Tests
;;; ====

(test-group extract-tests
            (define-test extract-function-simple-test
              (let* ([result (extract-function '(* x x) 'square '(x))]
                     [def (car result)]
                     [call (cdr result)])
                    (assert-equal '(define (square x) (* x x)) def)
                    (assert-equal '(square x) call)))
            
            (define-test extract-function-multiple-params-test
              (let* ([result (extract-function '(+ x y) 'add '(x y))]
                     [def (car result)]
                     [call (cdr result)])
                    (assert-equal '(define (add x y) (+ x y)) def)
                    (assert-equal '(add x y) call)))
            
            (define-test extract-let-test
              (let ([result (extract-let '(+ (* x x) (* y y))
                                         '(cdr car)  ; path to (* x x)
                                         'x-sq)])
                   (assert-equal 'let (car result))
                   ;; Should have binding and body
                   (assert-true (pair? (cadr result))))))

;;; ====
;;; Inline Function Tests
;;; ====

(test-group inline-tests
            (define-test find-definition-test
              (let ([forms '((define (foo x) (+ x 1))
                             (define (bar y) (* y 2)))]
                    [def (find-definition '((define (foo x) (+ x 1))
                                            (define (bar y) (* y 2)))
                                          'foo)])
                   (assert-true (pair? def))
                   (assert-equal 'define (car def))))
            
            (define-test definition-params-test
              (let ([def '(define (foo x y) (+ x y))])
                   (assert-equal '(x y) (definition-params def))))
            
            (define-test definition-body-test
              (let ([def '(define (foo x) (+ x 1))])
                   (assert-equal '(+ x 1) (definition-body def))))
            
            (define-test inline-call-test
              (let ([call '(square 5)]
                    [def '(define (square x) (* x x))])
                   (assert-equal '(* 5 5) (inline-call call def))))
            
            (define-test inline-call-multiple-args-test
              (let ([call '(add 3 4)]
                    [def '(define (add x y) (+ x y))])
                   (assert-equal '(+ 3 4) (inline-call call def)))))

;;; ====
;;; Free Variables Tests
;;; ====

(test-group free-vars-tests
            (define-test free-vars-symbol-test
              (assert-equal '(x) (free-variables 'x)))
            
            (define-test free-vars-literal-test
              (assert-equal '() (free-variables 42))
              (assert-equal '() (free-variables "string")))
            
            (define-test free-vars-lambda-test
              ;; x is bound, y is free
              (let ([fv (free-variables '(lambda (x) (+ x y)))])
                   (assert-true (if (member '+ fv) #t #f))
                   (assert-true (if (member 'y fv) #t #f))
                   (assert-false (if (member 'x fv) #t #f))))
            
            (define-test free-vars-let-test
              ;; a is bound, b is free
              (let ([fv (free-variables '(let ([a 1]) (+ a b)))])
                   (assert-true (if (member '+ fv) #t #f))
                   (assert-true (if (member 'b fv) #t #f))
                   (assert-false (if (member 'a fv) #t #f))))
            
            (define-test free-vars-nested-test
              (let ([fv (free-variables '(lambda (x) (lambda (y) (+ x y z))))])
                   (assert-true (if (member '+ fv) #t #f))
                   (assert-true (if (member 'z fv) #t #f))
                   (assert-false (if (member 'x fv) #t #f))
                   (assert-false (if (member 'y fv) #t #f)))))

;;; ====
;;; Unique Tests
;;; ====

(test-group unique-tests
            (define-test unique-removes-duplicates
              (assert-equal '(a b c) (unique '(a b a c b a))))
            
            (define-test unique-preserves-order
              (assert-equal '(x y z) (unique '(x y z))))
            
            (define-test unique-empty
              (assert-equal '() (unique '()))))

;;; ====
;;; Helper Function Tests
;;; ====

(test-group helper-tests
            (define-test take-test
              (assert-equal '(1 2) (take '(1 2 3 4 5) 2))
              (assert-equal '() (take '(1 2 3) 0))
              (assert-equal '(1 2 3) (take '(1 2 3) 5)))
            
            (define-test drop-test
              (assert-equal '(3 4 5) (drop '(1 2 3 4 5) 2))
              (assert-equal '(1 2 3) (drop '(1 2 3) 0))
              (assert-equal '() (drop '(1 2 3) 5))))

;;; ====
;;; Semantic Search Tests
;;; ====

(test-group search-tests
            (define-test find-functions-test
              (let* ([sexps '((define (foo x) x)
                              (define bar 42)
                              (define (baz y z) (+ y z)))]
                     [fns (find-functions sexps)])
                    (assert-equal 2 (length fns))))
            
            (define-test find-variables-test
              (let* ([sexps '((define (foo x) x)
                              (define bar 42)
                              (define qux "string"))]
                     [vars (find-variables sexps)])
                    (assert-equal 2 (length vars))))
            
            (define-test find-usages-test
              (let* ([sexps '((define (foo x) (+ x 1))
                              (foo 5)
                              (foo (foo 3)))]
                     [usages (find-usages sexps 'foo)])
                    ;; foo appears in definition, and two calls
                    (assert-true (> (length usages) 2)))))

;;; ====
;;; Session Tests
;;; ====

(test-group session-tests
            (define-test make-session-test
              (let ([s (make-refactor-session)])
                   (assert-true (session? s))
                   (assert-equal '() (session-changes s))))
            
            (define-test session-add-change-test
              (let ([s (make-refactor-session)]
                    [c (make-change 'rename "f.ss" "a" "b" 1 1)])
                   (session-add-change! s c)
                   (assert-equal 1 (length (session-changes s))))))

;;; ====
;;; Introduce Parameter Tests
;;; ====

(test-group introduce-param-tests
            (define-test introduce-parameter-test
              (let ([body '(+ (* 2 3.14159) (* 2 3.14159))]
                    [result (introduce-parameter '(+ (* 2 3.14159) (* 2 3.14159))
                                                 3.14159
                                                 'pi)])
                   (assert-equal '(+ (* 2 pi) (* 2 pi)) result)))
            
            (define-test introduce-parameter-nested-test
              (let ([result (introduce-parameter
                             '(if (> x threshold) x threshold)
                             'threshold
                             't)])
                   (assert-equal '(if (> x t) x t) result))))

;;; ====
;;; Run Tests
;;; ====

(display "
")
(display "====
")
(display (format "Tests passed: ~a
" *tests-passed*))
(display (format "Tests failed: ~a
" *tests-failed*))
(display (format "Total tests:  ~a
" *tests-run*))

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All refactoring toolkit tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
