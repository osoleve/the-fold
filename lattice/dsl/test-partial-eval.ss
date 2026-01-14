;;; lattice/dsl/test-partial-eval.ss — Tests for Partial Evaluation
;;;
;;; Comprehensive tests for binding-time analysis, online/offline
;;; partial evaluation, and specialization.

(load "core/testing/test-framework.ss")
(load "lattice/dsl/partial-eval.ss")

;;; ====
;;; Binding Time Tests
;;; ====

(test-group binding-time
            
            (define-test bt-join-bottom-identity
              (assert-equal bt-static (bt-join bt-bottom bt-static))
              (assert-equal bt-dynamic (bt-join bt-bottom bt-dynamic))
              (assert-equal bt-static (bt-join bt-static bt-bottom))
              (assert-equal bt-dynamic (bt-join bt-dynamic bt-bottom)))
            
            (define-test bt-join-dynamic-dominates
              (assert-equal bt-dynamic (bt-join bt-static bt-dynamic))
              (assert-equal bt-dynamic (bt-join bt-dynamic bt-static))
              (assert-equal bt-dynamic (bt-join bt-dynamic bt-dynamic)))
            
            (define-test bt-join-static-preserves
              (assert-equal bt-static (bt-join bt-static bt-static)))
            
            (define-test bt-meet-bottom-absorbs
              (assert-equal bt-bottom (bt-meet bt-bottom bt-static))
              (assert-equal bt-bottom (bt-meet bt-static bt-bottom)))
            
            (define-test bt-ordering
              (assert-true (bt<=? bt-bottom bt-static))
              (assert-true (bt<=? bt-bottom bt-dynamic))
              (assert-true (bt<=? bt-static bt-dynamic))
              (assert-false (bt<=? bt-dynamic bt-static))
              (assert-false (bt<=? bt-static bt-bottom))))

;;; ====
;;; Binding-Time Environment Tests
;;; ====

(test-group bt-env
            
            (define-test empty-env-defaults-to-dynamic
              (let ([env (make-bt-env)])
                   (assert-equal bt-dynamic (bt-env-lookup env 'x))))
            
            (define-test lookup-finds-extended-binding
              (let* ([env (make-bt-env)]
                     [env2 (bt-env-extend env 'x bt-static)])
                    (assert-equal bt-static (bt-env-lookup env2 'x))))
            
            (define-test extend-many-adds-multiple
              (let* ([env (make-bt-env)]
                     [env2 (bt-env-extend-many env '(x y z) (list bt-static bt-dynamic bt-static))])
                    (assert-equal bt-static (bt-env-lookup env2 'x))
                    (assert-equal bt-dynamic (bt-env-lookup env2 'y))
                    (assert-equal bt-static (bt-env-lookup env2 'z))))
            
            (define-test later-bindings-shadow
              (let* ([env (bt-env-extend (make-bt-env) 'x bt-static)]
                     [env2 (bt-env-extend env 'x bt-dynamic)])
                    (assert-equal bt-dynamic (bt-env-lookup env2 'x)))))

;;; ====
;;; Binding-Time Analysis Tests
;;; ====

(test-group bta
            
            (define-test literals-are-static
              (let ([env (make-bt-env)])
                   (assert-equal bt-static (bta 42 env))
                   (assert-equal bt-static (bta #t env))
                   (assert-equal bt-static (bta "hello" env))
                   (assert-equal bt-static (bta '() env))))
            
            (define-test quoted-expressions-static
              (let ([env (make-bt-env)])
                   (assert-equal bt-static (bta '(quote (1 2 3)) env))))
            
            (define-test unbound-variables-dynamic
              (let ([env (make-bt-env)])
                   (assert-equal bt-dynamic (bta 'x env))))
            
            (define-test bound-variables-reflect-bt
              (let ([env (bt-env-extend (make-bt-env) 'x bt-static)])
                   (assert-equal bt-static (bta 'x env))))
            
            (define-test lambda-is-static
              (let ([env (make-bt-env)])
                   (assert-equal bt-static (bta '(lambda (x) x) env))))
            
            (define-test if-static-condition
              (let ([env (make-bt-env)])
                   (assert-equal bt-static (bta '(if #t 1 2) env))))
            
            (define-test if-dynamic-condition
              (let ([env (bt-env-extend (make-bt-env) 'x bt-dynamic)])
                   (assert-equal bt-dynamic (bta '(if x 1 2) env))))
            
            (define-test let-propagates-bt
              (let ([env (make-bt-env)])
                   (assert-equal bt-static (bta '(let ([x 1]) x) env))))
            
            (define-test application-all-static
              (let ([env (make-bt-env)])
                   (assert-equal bt-static (bta '(+ 1 2) env))))
            
            (define-test application-dynamic-arg
              (let ([env (bt-env-extend (make-bt-env) 'x bt-dynamic)])
                   (assert-equal bt-dynamic (bta '(+ 1 x) env)))))

;;; ====
;;; Static Store Tests
;;; ====

(test-group static-store
            
            (define-test empty-store-no-values
              (let ([store (make-static-store)])
                   (assert-true (nothing? (static-store-lookup store 'x)))))
            
            (define-test extended-store-finds-value
              (let* ([store (make-static-store)]
                     [store2 (static-store-extend store 'x 42)])
                    (let ([result (static-store-lookup store2 'x)])
                         (assert-true (just? result))
                         (assert-equal 42 (from-just result)))))
            
            (define-test static-store-has-works
              (let* ([store (make-static-store)]
                     [store2 (static-store-extend store 'x 42)])
                    (assert-false (static-store-has? store 'x))
                    (assert-true (static-store-has? store2 'x)))))

;;; ====
;;; PE State Tests
;;; ====

(test-group pe-state
            
            (define-test initial-state-zero-depth
              (let ([state (make-pe-state 10)])
                   (assert-equal 0 (pe-state-depth state))
                   (assert-equal 10 (pe-state-limit state))))
            
            (define-test can-unfold-respects-limit
              (let ([state (make-pe-state 2)])
                   (assert-true (pe-state-can-unfold? state))
                   (let ([state2 (pe-state-incr-depth state)])
                        (assert-true (pe-state-can-unfold? state2))
                        (let ([state3 (pe-state-incr-depth state2)])
                             (assert-false (pe-state-can-unfold? state3))))))
            
            (define-test fresh-var-generates-unique
              (let* ([state (make-pe-state 10)]
                     [r1 (pe-state-fresh-var state 'x)]
                     [r2 (pe-state-fresh-var (cdr r1) 'x)]
                     [r3 (pe-state-fresh-var (cdr r2) 'y)])
                    (assert-equal 'x$0 (car r1))
                    (assert-equal 'x$1 (car r2))
                    (assert-equal 'y$2 (car r3)))))

;;; ====
;;; Online Partial Evaluation Tests
;;; ====

(test-group pe-online
            
            (define-test literals-pass-through
              (assert-equal 42 (partial-eval 42 '()))
              (assert-equal #t (partial-eval #t '()))
              (assert-equal "hello" (partial-eval "hello" '())))
            
            (define-test static-variable-substitution
              (assert-equal 10 (partial-eval 'x '((x . 10)))))
            
            (define-test dynamic-variable-preserved
              (assert-equal 'y (partial-eval 'y '((x . 10)))))
            
            (define-test arithmetic-static-values
              (assert-equal 15 (partial-eval '(+ 10 5) '()))
              (assert-equal 50 (partial-eval '(* 10 5) '()))
              (assert-equal 7 (partial-eval '(+ x 2) '((x . 5)))))
            
            (define-test arithmetic-dynamic-values
              (assert-equal '(+ x 5) (partial-eval '(+ x 5) '())))
            
            (define-test comparison-static-values
              (assert-equal #t (partial-eval '(= 5 5) '()))
              (assert-equal #f (partial-eval '(< 10 5) '())))
            
            (define-test if-static-true
              (assert-equal 1 (partial-eval '(if #t 1 2) '())))
            
            (define-test if-static-false
              (assert-equal 2 (partial-eval '(if #f 1 2) '())))
            
            (define-test if-computed-condition
              (assert-equal 1 (partial-eval '(if (= x 5) 1 2) '((x . 5))))
              (assert-equal 2 (partial-eval '(if (= x 5) 1 2) '((x . 3)))))
            
            (define-test if-dynamic-preserves-branches
              (let ([result (partial-eval '(if x 1 2) '())])
                   (assert-equal 'if (car result))
                   (assert-equal 'x (cadr result))
                   (assert-equal 1 (caddr result))
                   (assert-equal 2 (cadddr result))))
            
            (define-test let-static-binding
              (assert-equal 15 (partial-eval '(let ([x 10]) (+ x 5)) '())))
            
            (define-test let-dynamic-binding
              (let ([result (partial-eval '(let ([x y]) (+ x 1)) '())])
                   (assert-equal 'let (car result))))
            
            (define-test nested-let-mixed
              (assert-equal 8 (partial-eval
                               '(let ([x 3])
                                 (let ([y 5])
                                      (+ x y)))
                               '())))
            
            (define-test lambda-preserved
              (let ([result (partial-eval '(lambda (x) (+ x 1)) '())])
                   (assert-equal 'lambda (car result))))
            
            (define-test lambda-application-static
              (assert-equal 11 (partial-eval '((lambda (x) (+ x 1)) 10) '())))
            
            (define-test lambda-application-dynamic
              (let ([result (partial-eval '((lambda (x) (+ x 1)) y) '())])
                   (assert-true (pair? result)))))

;;; ====
;;; List Primitive Tests (QA findings from Gemini)
;;; ====

(test-group list-primitives
            
            (define-test car-static-list
              (assert-equal 1 (partial-eval '(car '(1 2 3)) '())))
            
            (define-test car-static-symbol-list
              (assert-equal ''a (partial-eval '(car '(a b c)) '())))
            
            (define-test cdr-static-list
              (assert-equal ''(2 3) (partial-eval '(cdr '(1 2 3)) '())))
            
            (define-test cons-static
              (assert-equal ''(1 . 2) (partial-eval '(cons 1 2) '())))
            
            (define-test cons-static-list
              (assert-equal ''(1 2 3) (partial-eval '(cons 1 '(2 3)) '())))
            
            (define-test null-static-empty
              (assert-equal #t (partial-eval '(null? '()) '())))
            
            (define-test null-static-nonempty
              (assert-equal #f (partial-eval '(null? '(1 2)) '())))
            
            (define-test eq-static-symbols
              (assert-equal #t (partial-eval '(eq? 'a 'a) '())))
            
            (define-test eq-static-different
              (assert-equal #f (partial-eval '(eq? 'a 'b) '())))
            
            (define-test list-static
              (assert-equal ''(1 2 3) (partial-eval '(list 1 2 3) '())))
            
            (define-test not-static-true
              (assert-equal #f (partial-eval '(not #t) '())))
            
            (define-test not-static-false
              (assert-equal #t (partial-eval '(not #f) '()))))

;;; ====
;;; Specialization Tests
;;; ====

(test-group specialize
            
            (define-test specialize-simple
              (assert-equal 15 (specialize '(+ x y) '(x y) '(10 5))))
            
            (define-test specialize-partial-static
              (let ([result (specialize '(+ x y) '(x) '(10))])
                   (assert-equal '(+ 10 y) result)))
            
            (define-test specialize-function-lambda
              (let ([result (specialize-function
                             '(lambda (x y) (+ x y))
                             '((x . 10)))])
                   (assert-equal 'lambda (car result))))
            
            (define-test specialize-function-define
              (let ([result (specialize-function
                             '(define (add x y) (+ x y))
                             '((x . 10)))])
                   (assert-equal 'define (car result)))))

;;; ====
;;; Simplification Tests
;;; ====

(test-group simplify
            
            (define-test multiply-by-1
              (assert-equal 'x (simplify '(* x 1)))
              (assert-equal 'x (simplify '(* 1 x))))
            
            (define-test multiply-by-0
              (assert-equal 0 (simplify '(* x 0)))
              (assert-equal 0 (simplify '(* 0 x))))
            
            (define-test add-0
              (assert-equal 'x (simplify '(+ x 0)))
              (assert-equal 'x (simplify '(+ 0 x))))
            
            (define-test subtract-0
              (assert-equal 'x (simplify '(- x 0))))
            
            (define-test constant-folding
              (assert-equal 15 (simplify '(+ 10 5)))
              (assert-equal 50 (simplify '(* 10 5))))
            
            (define-test nested-simplification
              (assert-equal 'x (simplify '(* 1 (+ 0 x)))))
            
            (define-test if-true
              (assert-equal 1 (simplify '(if #t 1 2))))
            
            (define-test if-false
              (assert-equal 2 (simplify '(if #f 1 2))))
            
            (define-test empty-let
              (assert-equal 'x (simplify '(let () x)))))

;;; ====
;;; Power Example Tests
;;; ====

(test-group power-example
            
            (define-test power-staged-code
              (let ([code (power-staged 3)])
                   (assert-true (code? code))
                   (assert-equal 1 (code-stage code))))
            
            (define-test power-staged-0
              (let ([code (power-staged 0)])
                   (assert-equal '1 (code-expr code))))
            
            (define-test power-staged-1
              (let ([code (power-staged 1)])
                   (assert-true (pair? (code-expr code)))))
            
            (define-test power-staged-3-structure
              (let ([code (power-staged 3)])
                   (let ([expr (code-expr code)])
                        (assert-equal '* (car expr))))))

;;; ====
;;; Offline Partial Evaluation Tests
;;; ====

(test-group pe-offline
            
            (define-test offline-same-as-online-simple
              (assert-equal (partial-eval '(+ 1 2) '())
                            (partial-eval-offline '(+ 1 2) '())))
            
            (define-test offline-static-conditional
              (assert-equal 1 (partial-eval-offline '(if #t 1 2) '())))
            
            (define-test offline-preserves-dynamic
              (let ([result (partial-eval-offline '(+ x 1) '())])
                   (assert-equal '(+ x 1) result))))

;;; ====
;;; Integration Tests
;;; ====

(test-group integration
            
            (define-test complex-nested
              (let ([expr '(let ([a 10])
                            (let ([b (+ a 5)])
                                 (* b 2)))])
                   (assert-equal 30 (partial-eval expr '()))))
            
            (define-test conditional-with-let
              (let ([expr '(let ([x 5])
                            (if (= x 5)
                                (+ x 10)
                                (- x 10)))])
                   (assert-equal 15 (partial-eval expr '()))))
            
            (define-test nested-lambdas
              (let ([result (partial-eval
                             '(((lambda (x) (lambda (y) (+ x y))) 10) 5)
                             '())])
                   (assert-equal 15 result)))
            
            (define-test partial-eval-simplified-both
              (let ([result (partial-eval-simplified
                             '(* 1 (+ 0 (+ x 0)))
                             '())])
                   (assert-equal 'x result))))

;;; ====
;;; Summary
;;; ====

(display "\n=== Partial Evaluation Tests Complete ===\n")
(display "Passed: ")
(display *tests-passed*)
(display " | Failed: ")
(display *tests-failed*)
(display " | Total: ")
(display *tests-run*)
(newline)

(when (> *tests-failed* 0)
      (exit 1))
