;;; core/dsl/test-staging.ss — Tests for Multi-Stage Programming
;;;
;;; Tests for staging annotations, code types, and staged computation.

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/dsl/staging.ss")

;;; ====
;;; Code Type Tests
;;; ====

(test-group code-type-basics
            
            (define-test make-code-creates-valid-code
              (let ([c (make-code 1 '(+ x 1))])
                   (assert-true (code? c))
                   (assert-equal (code-stage c) 1)
                   (assert-equal (code-expr c) '(+ x 1))))
            
            (define-test code-stage-returns-level
              (assert-equal (code-stage (make-code 0 'x)) 0)
              (assert-equal (code-stage (make-code 1 'x)) 1)
              (assert-equal (code-stage (make-code 2 'x)) 2))
            
            (define-test code-expr-returns-expression
              (assert-equal (code-expr (make-code 1 '(* a b))) '(* a b)))
            
            (define-test code-on-non-code-returns-defaults
              (assert-equal (code-stage 'not-code) 0)
              (assert-equal (code-expr 'not-code) 'not-code)))

;;; ====
;;; Staging Detection Tests
;;; ====

(test-group staging-detection
            
            (define-test stage-quote-detects-forms
              (assert-true (stage-quote? '(stage-quote x)))
              (assert-true (stage-quote? '(bracket x)))
              (assert-true (stage-quote? '(< x)))
              (assert-false (stage-quote? '(quote x)))
              (assert-false (stage-quote? 'stage-quote)))
            
            (define-test stage-splice-detects-forms
              (assert-true (stage-splice? '(stage-splice x)))
              (assert-true (stage-splice? '(escape x)))
              (assert-true (stage-splice? '(~ x)))
              (assert-false (stage-splice? '(unquote x)))
              (assert-false (stage-splice? 'stage-splice)))
            
            (define-test stage-run-detects-forms
              (assert-true (stage-run? '(stage-run x)))
              (assert-true (stage-run? '(run x)))
              (assert-true (stage-run? '(! x)))
              (assert-false (stage-run? '(exec x)))
              (assert-false (stage-run? 'stage-run))))

;;; ====
;;; Stage Environment Tests
;;; ====

(test-group stage-environment
            
            (define-test make-stage-env-creates-valid-env
              (let ([env (make-stage-env 0)])
                   (assert-true (stage-env? env))
                   (assert-equal (stage-env-level env) 0)
                   (assert-equal (stage-env-bindings env) '())
                   (assert-equal (stage-env-fragments env) '())))
            
            (define-test stage-env-incr-increments-level
              (let* ([env (make-stage-env 0)]
                     [env2 (stage-env-incr env)])
                    (assert-equal (stage-env-level env2) 1)))
            
            (define-test stage-env-decr-decrements-level
              (let* ([env (make-stage-env 2)]
                     [env2 (stage-env-decr env)])
                    (assert-equal (stage-env-level env2) 1)))
            
            (define-test stage-env-decr-floors-at-zero
              (let* ([env (make-stage-env 0)]
                     [env2 (stage-env-decr env)])
                    (assert-equal (stage-env-level env2) 0)))
            
            (define-test stage-env-bind-adds-binding
              (let* ([env (make-stage-env 0)]
                     [env2 (stage-env-bind env 'x 42)])
                    (assert-true (just? (stage-env-lookup env2 'x)))
                    (assert-equal (from-just (stage-env-lookup env2 'x)) 42)))
            
            (define-test stage-env-lookup-returns-nothing-for-unbound
              (let ([env (make-stage-env 0)])
                   (assert-true (nothing? (stage-env-lookup env 'undefined))))))

;;; ====
;;; Stage Expansion Tests
;;; ====

(test-group stage-expansion
            
            (define-test expand-literal-at-level-0
              (assert-equal (stage-expand 42 0) 42)
              (assert-equal (stage-expand "hello" 0) "hello")
              (assert-equal (stage-expand #t 0) #t))
            
            (define-test expand-symbol-quoted-at-higher-level
              (assert-equal (stage-expand 'x 1) '(quote x))
              (assert-equal (stage-expand 'foo 2) '(quote foo)))
            
            (define-test expand-symbol-not-quoted-at-level-0
              (assert-equal (stage-expand 'x 0) 'x))
            
            (define-test expand-stage-quote-at-level-0
              (let ([result (stage-expand '(stage-quote 1) 0)])
                   (assert-equal result '(make-code 1 '1))))
            
            (define-test expand-application-at-level-0
              (let ([result (stage-expand '(+ a b) 0)])
                   (assert-equal result '(+ a b))))
            
            (define-test expand-lambda-preserves-structure
              (let ([result (stage-expand '(lambda (x) (+ x 1)) 0)])
                   (assert-equal result '(lambda (x) (+ x 1)))))
            
            (define-test expand-if-preserves-structure
              (let ([result (stage-expand '(if test then else) 0)])
                   (assert-equal result '(if test then else))))
            
            (define-test expand-let-preserves-structure
              (let ([result (stage-expand '(let ([x 1]) (+ x 2)) 0)])
                   (assert-equal result '(let ([x 1]) (+ x 2))))))

;;; ====
;;; Code Combination Tests
;;; ====

(test-group code-combination
            
            (define-test code-combine-combines-same-stage
              (let* ([c1 (make-code 1 'a)]
                     [c2 (make-code 1 'b)]
                     [result (code-combine c1 c2 '+)])
                    (assert-true (code? result))
                    (assert-equal (code-stage result) 1)
                    (assert-equal (code-expr result) '(+ a b))))
            
            (define-test code-lift-creates-stage-1-code
              (let ([c (code-lift 42)])
                   (assert-true (code? c))
                   (assert-equal (code-stage c) 1)
                   (assert-equal (code-expr c) '(quote 42))))
            
            (define-test code-unlift-extracts-stage-0-value
              (let* ([c (make-code 0 'value)])
                    (assert-equal (code-unlift c) 'value))))

;;; ====
;;; Staged Arithmetic Tests
;;; ====

(test-group staged-arithmetic
            
            (define-test stage-add-combines-code
              (let* ([c1 (make-code 1 'x)]
                     [c2 (make-code 1 'y)]
                     [result (stage-add c1 c2)])
                    (assert-equal (code-expr result) '(+ x y))))
            
            (define-test stage-mul-combines-code
              (let* ([c1 (make-code 1 'a)]
                     [c2 (make-code 1 'b)]
                     [result (stage-mul c1 c2)])
                    (assert-equal (code-expr result) '(* a b))))
            
            (define-test stage-sub-combines-code
              (let* ([c1 (make-code 1 'm)]
                     [c2 (make-code 1 'n)]
                     [result (stage-sub c1 c2)])
                    (assert-equal (code-expr result) '(- m n))))
            
            (define-test stage-div-combines-code
              (let* ([c1 (make-code 1 'p)]
                     [c2 (make-code 1 'q)]
                     [result (stage-div c1 c2)])
                    (assert-equal (code-expr result) '(/ p q)))))

;;; ====
;;; Staged Control Flow Tests
;;; ====

(test-group staged-control-flow
            
            (define-test stage-if-creates-conditional
              (let* ([cond-c (make-code 1 '(> x 0))]
                     [then-c (make-code 1 'x)]
                     [else-c (make-code 1 '(- x))]
                     [result (stage-if cond-c then-c else-c)])
                    (assert-equal (code-expr result) '(if (> x 0) x (- x)))))
            
            (define-test stage-let-creates-binding
              (let* ([val-c (make-code 1 '(+ 1 2))]
                     [body-c (make-code 1 '(* x x))]
                     [result (stage-let 'x val-c body-c)])
                    (assert-equal (code-expr result) '(let ([x (+ 1 2)]) (* x x)))))
            
            (define-test stage-lambda-creates-function
              (let* ([body-c (make-code 1 '(* x x))]
                     [result (stage-lambda '(x) body-c)])
                    (assert-equal (code-expr result) '(lambda (x) (* x x)))))
            
            (define-test stage-app-creates-application
              (let* ([func-c (make-code 1 'f)]
                     [arg1-c (make-code 1 'x)]
                     [arg2-c (make-code 1 'y)]
                     [result (stage-app func-c (list arg1-c arg2-c))])
                    (assert-equal (code-expr result) '(f x y)))))

;;; ====
;;; Cross-Stage Persistence Tests
;;; ====

(test-group cross-stage-persistence
            
            (define-test make-csp-creates-binding
              (let ([csp (make-csp 'n 5)])
                   (assert-true (csp? csp))
                   (assert-equal (csp-name csp) 'n)
                   (assert-equal (csp-value csp) 5)))
            
            (define-test lift-csp-creates-code-reference
              (let* ([csp (make-csp 'val 42)]
                     [c (lift-csp csp)])
                    (assert-true (code? c))
                    (assert-equal (code-stage c) 1)
                    (assert-equal (code-expr c) '(csp-ref 'val)))))

;;; ====
;;; Power Example Tests
;;; ====

(test-group power-example
            
            (define-test power-staged-zero
              (let ([c (power-staged 0)])
                   (assert-true (code? c))
                   (assert-equal (code-stage c) 1)
                   (assert-equal (code-expr c) '1)))
            
            (define-test power-staged-one
              (let ([c (power-staged 1)])
                   (assert-equal (code-expr c) '(* 1 x))))
            
            (define-test power-staged-two
              (let ([c (power-staged 2)])
                   (assert-equal (code-expr c) '(* (* 1 x) x))))
            
            (define-test power-staged-three
              (let ([c (power-staged 3)])
                   (assert-equal (code-expr c) '(* (* (* 1 x) x) x))))
            
            (define-test power-specialized-returns-structure
              (let ([spec (power-specialized 2)])
                   (assert-equal (car spec) 'specialized-power)
                   (assert-equal (cadr spec) 2)
                   (assert-equal (caddr spec) '(* (* 1 x) x)))))

;;; ====
;;; Code Pretty Print Tests
;;; ====

(test-group code-pretty-print
            
            (define-test code-to-string-formats-code
              (let ([c (make-code 1 '(+ x 1))])
                   (assert-true (string? (code->string c)))))
            
            (define-test code-to-string-handles-non-code
              (assert-true (string? (code->string 42)))))

;;; ====
;;; Staged Compilation Tests
;;; ====

(test-group staged-compilation
            
            (define-test compile-staged-expands-expression
              (let ([result (compile-staged '(+ 1 2))])
                   (assert-equal result '(+ 1 2))))
            
            (define-test run-staged-extracts-stage-0
              (let ([c (make-code 0 '42)])
                   (assert-equal (run-staged c) '42))))

;;; ====
;;; Staged Pattern Matching Tests
;;; ====

(test-group staged-pattern-matching
            
            (define-test stage-match-creates-match-expr
              (let* ([scrut-c (make-code 1 'expr)]
                     [clauses '([(Lit n) n] [(Add x y) (+ x y)])]
                     [result (stage-match scrut-c clauses)])
                    (assert-true (code? result))
                    (assert-equal (car (code-expr result)) 'match))))

;;; ====
;;; Type Inference Tests
;;; ====

(test-group staging-type-inference
            
            (define-test code-type-syntax-detects-code-types
              (assert-true (code-type-syntax? '(Code 1)))
              (assert-true (code-type-syntax? '(Code 2 Int)))
              (assert-false (code-type-syntax? 'Int))
              (assert-false (code-type-syntax? '(List Int))))
            
            (define-test code-type-stage-extracts-stage
              (assert-equal (code-type-stage '(Code 1)) 1)
              (assert-equal (code-type-stage '(Code 2 Int)) 2)
              (assert-equal (code-type-stage 'Int) 0))
            
            (define-test infer-staged-type-handles-literals
              (assert-equal (infer-staged-type 42 '() 0) 'Int)
              (assert-equal (infer-staged-type "hello" '() 0) 'String)
              (assert-equal (infer-staged-type #t '() 0) 'Bool)))

;;; ====
;;; Transformation Tests
;;; ====

(test-group staging-transformations
            
            (define-test subst-expr-substitutes-variable
              (assert-equal (subst-expr 'x 'x 42) 42)
              (assert-equal (subst-expr 'y 'x 42) 'y)
              (assert-equal (subst-expr '(+ x y) 'x 1) '(+ 1 y)))
            
            (define-test subst-expr-respects-shadowing-in-lambda
              (assert-equal (subst-expr '(lambda (x) x) 'x 42) '(lambda (x) x)))
            
            (define-test subst-expr-respects-shadowing-in-let
              (assert-equal (subst-expr '(let ([x 1]) x) 'x 42) '(let ([x 1]) x)))
            
            (define-test inline-staged-handles-atoms
              (assert-equal (inline-staged 42) 42)
              (assert-equal (inline-staged 'x) 'x))
            
            (define-test beta-reduce-staged-reduces-simple
              (let ([result (beta-reduce-staged '((lambda (x) x) 42))])
                   (assert-equal result 42)))
            
            (define-test beta-reduce-staged-preserves-non-reducible
              (let ([result (beta-reduce-staged '(f x))])
                   (assert-equal result '(f x)))))

;;; ====
;;; Well-Staged Check Tests
;;; ====

(test-group well-staged-check
            
            (define-test code-well-staged-checks-level
              (let ([c0 (make-code 0 'x)]
                    [c1 (make-code 1 'x)]
                    [c2 (make-code 2 'x)])
                   (assert-true (code-well-staged? c0 0))
                   (assert-true (code-well-staged? c0 1))
                   (assert-true (code-well-staged? c1 1))
                   (assert-true (code-well-staged? c1 2))
                   (assert-false (code-well-staged? c2 1)))))

;;; ====
;;; Run All Tests
;;; ====

(display "\n====\n")
(display "Running Multi-Stage Programming Tests\n")
(display "====\n\n")

(run-all-tests)
