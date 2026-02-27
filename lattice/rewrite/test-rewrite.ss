;;; core/fp/rewrite/test-rewrite.ss — Rewrite System Test Suite
;;;
;;; Comprehensive tests for the equational reasoning system:
;;;   - Rule construction and accessors
;;;   - Pattern matching with metavariables
;;;   - Law application
;;;   - Trace operations
;;;   - Equivalence verification
;;;   - Strategy combinators
;;;
;;; Run with: scheme --script core/fp/rewrite/test-rewrite.ss

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/rewrite/rule.ss")
(load "lattice/rewrite/trace.ss")
(load "lattice/rewrite/engine.ss")
(load "lattice/rewrite/laws.ss")
(load "lattice/rewrite/verify.ss")

;;; ====
;;; Rule Construction Tests
;;; ====

(test-group rule-construction
            
            (define-test make-rule-basic
              (let ([r (make-rule 'test-rule '(?x) '(?x))])
                   (assert-true (if (rule? r) #t #f))
                   (assert-equal 'test-rule (rule-name r))
                   (assert-equal '(?x) (rule-lhs r))
                   (assert-equal '(?x) (rule-rhs r))))
            
            (define-test rule-with-category
              (let ([r (make-rule 'test-rule '(?x) '(?x) 'category 'monad)])
                   (assert-equal 'monad (rule-category r))))
            
            (define-test rule-with-direction
              (let ([r (make-rule 'test-rule '(?x) '(?x) 'direction 'bidirectional)])
                   (assert-equal 'bidirectional (rule-direction r))))
            
            (define-test rule-defaults
              (let ([r (make-rule 'test-rule '(?x) '(?x))])
                   (assert-equal 'custom (rule-category r))
                   (assert-equal 'forward (rule-direction r))
                   (assert-equal '() (rule-conditions r))))
            
            (define-test metavar-detection
              (assert-true (metavar? '(?x)))
              (assert-true (metavar? '(?foo)))
              (assert-true (metavar? '(?x number)))
              (assert-false (metavar? 'x))
              (assert-false (metavar? '(f x)))
              (assert-false (metavar? 42)))
            
            (define-test metavar-name-extraction
              (assert-equal 'x (metavar-name '(?x)))
              (assert-equal 'foo (metavar-name '(?foo)))
              (assert-equal 'var (metavar-name '(?var number))))
            
            (define-test metavar-constraint-extraction
              (assert-equal #f (metavar-constraint '(?x)))
              (assert-equal 'number (metavar-constraint '(?x number)))
              (assert-equal 'symbol (metavar-constraint '(?y symbol))))
            
            (define-test pattern-vars-collection
              (assert-equal '(x) (pattern-vars '(?x)))
              (assert-equal '(x y) (pattern-vars '(f (?x) (?y))))
              (assert-equal '(a b c) (pattern-vars '(g (?a) (h (?b) (?c))))))
            
            (define-test pattern-vars-unique-dedup
              (assert-equal '(x) (pattern-vars-unique '(+ (?x) (?x))))
              (assert-equal '(x y) (pattern-vars-unique '(+ (?x) (?y) (?x)))))
            
            (define-test valid-rule-check
              (assert-true (if (valid-rule? (make-rule 'r1 '(?x) '(?x))) #t #f))
              (assert-true (if (valid-rule? (make-rule 'r2 '(f (?x) (?y)) '(g (?y) (?x)))) #t #f))
              ;; RHS var not in LHS would be invalid
              (assert-false (valid-rule? (make-rule 'r3 '(?x) '(?y))))))

;;; ====
;;; Pattern Matching Tests
;;; ====

(test-group pattern-matching
            
            (define-test match-literal
              (let ([result (match-pattern '(+ 1 2) '(+ 1 2) '())])
                   (assert-true (list? result))
                   (assert-equal '() result)))
            
            (define-test match-literal-fail
              (let ([result (match-pattern '(+ 1 2) '(+ 1 3) '())])
                   (assert-false result)))
            
            (define-test match-single-metavar
              (let ([result (match-pattern '(?x) '(+ 1 2) '())])
                   (assert-true (list? result))
                   (assert-equal '(+ 1 2) (cdr (assq 'x result)))))
            
            (define-test match-nested-metavars
              (let ([result (match-pattern '(f (?x) (?y)) '(f 1 2) '())])
                   (assert-true (list? result))
                   (assert-equal 1 (cdr (assq 'x result)))
                   (assert-equal 2 (cdr (assq 'y result)))))
            
            (define-test match-deep-nesting
              (let ([result (match-pattern '(g (?a) (h (?b) (?c))) '(g 1 (h 2 3)) '())])
                   (assert-true (list? result))
                   (assert-equal 1 (cdr (assq 'a result)))
                   (assert-equal 2 (cdr (assq 'b result)))
                   (assert-equal 3 (cdr (assq 'c result)))))
            
            (define-test match-consistent-binding
              ;; Same metavar must bind to same value
              (let ([result1 (match-pattern '(+ (?x) (?x)) '(+ 1 1) '())])
                   (assert-true (list? result1))
                   (assert-equal 1 (cdr (assq 'x result1))))
              ;; Fails when values differ
              (let ([result2 (match-pattern '(+ (?x) (?x)) '(+ 1 2) '())])
                   (assert-false result2)))
            
            (define-test match-with-constraint-number
              (let ([result1 (match-pattern '(?x number) '42 '())])
                   (assert-true (list? result1)))
              (let ([result2 (match-pattern '(?x number) 'foo '())])
                   (assert-false result2)))
            
            (define-test match-with-constraint-symbol
              (let ([result1 (match-pattern '(?x symbol) 'foo '())])
                   (assert-true (list? result1)))
              (let ([result2 (match-pattern '(?x symbol) '42 '())])
                   (assert-false result2)))
            
            (define-test match-preserves-existing-env
              (let ([result (match-pattern '(?y) 'b '((x . a)))])
                   (assert-true (list? result))
                   (assert-equal 'a (cdr (assq 'x result)))
                   (assert-equal 'b (cdr (assq 'y result))))))

;;; ====
;;; Template Substitution Tests
;;; ====

(test-group template-substitution
            
            (define-test subst-single-var
              (let ([result (substitute-template '(?x) '((x . 42)))])
                   (assert-equal 42 result)))
            
            (define-test subst-nested
              (let ([result (substitute-template '(f (?x) (?y)) '((x . 1) (y . 2)))])
                   (assert-equal '(f 1 2) result)))
            
            (define-test subst-preserves-literals
              (let ([result (substitute-template '(+ 1 (?x)) '((x . 2)))])
                   (assert-equal '(+ 1 2) result)))
            
            (define-test subst-deep-nesting
              (let ([result (substitute-template '(g (?a) (h (?b))) '((a . x) (b . y)))])
                   (assert-equal '(g x (h y)) result))))

;;; ====
;;; Rule Application Tests
;;; ====

(test-group rule-application
            
            (define-test apply-simple-rule
              (let* ([rule (make-rule 'id '(?x) '(?x))]
                     [result (apply-rule rule '(+ 1 2))])
                    (assert-true (pair? result))
                    (assert-equal '(+ 1 2) (car result))))
            
            (define-test apply-transformation-rule
              (let* ([rule (make-rule 'swap '(pair (?x) (?y)) '(pair (?y) (?x)))]
                     [result (apply-rule rule '(pair a b))])
                    (assert-true (pair? result))
                    (assert-equal '(pair b a) (car result))))
            
            (define-test apply-rule-no-match
              (let* ([rule (make-rule 'r '(foo (?x)) '(?x))]
                     [result (apply-rule rule '(bar a))])
                    (assert-false result)))
            
            (define-test apply-monoid-left-id
              (let ([result (apply-rule monoid-left-id '(mappend mempty x))])
                   (assert-true (pair? result))
                   (assert-equal 'x (car result))))
            
            (define-test apply-monoid-right-id
              (let ([result (apply-rule monoid-right-id '(mappend x mempty))])
                   (assert-true (pair? result))
                   (assert-equal 'x (car result))))
            
            (define-test apply-monad-left-id
              (let ([result (apply-rule monad-left-id '(bind (pure a) f))])
                   (assert-true (pair? result))
                   (assert-equal '(f a) (car result))))
            
            (define-test apply-monad-right-id
              (let ([result (apply-rule monad-right-id '(bind m pure))])
                   (assert-true (pair? result))
                   (assert-equal 'm (car result))))
            
            (define-test apply-functor-id
              (let ([result (apply-rule functor-id '(fmap id fa))])
                   (assert-true (pair? result))
                   (assert-equal 'fa (car result)))))

;;; ====
;;; Position-Based Rewriting Tests
;;; ====

(test-group position-rewriting
            
            (define-test expr-at-root
              (assert-equal '(+ 1 2) (expr-at '(+ 1 2) '())))
            
            (define-test expr-at-first
              (assert-equal '+ (expr-at '(+ 1 2) '(0))))
            
            (define-test expr-at-nested
              (assert-equal 2 (expr-at '(f (g 1 2) 3) '(1 2))))
            
            (define-test expr-set-at-root
              (assert-equal 'x (expr-set-at '(+ 1 2) '() 'x)))
            
            (define-test expr-set-at-nested
              (assert-equal '(f (g 1 99) 3)
                            (expr-set-at '(f (g 1 2) 3) '(1 2) 99)))
            
            (define-test find-all-positions
              (let ([positions (find-all-positions '(f a b))])
                   (assert-true (pair? (member '() positions)))
                   (assert-true (pair? (member '(0) positions)))
                   (assert-true (pair? (member '(1) positions)))
                   (assert-true (pair? (member '(2) positions))))))

;;; ====
;;; Trace Tests
;;; ====

(test-group trace-operations
            
            (define-test make-trace-basic
              (let ([t (make-trace '(+ 1 2))])
                   (assert-true (if (trace? t) #t #f))
                   (assert-equal '(+ 1 2) (trace-initial t))
                   (assert-equal '() (trace-steps t))
                   (assert-equal '(+ 1 2) (trace-final t))
                   (assert-false (trace-verified? t))))
            
            (define-test trace-add-step
              (let* ([t (make-trace '(bind (pure a) f))]
                     [step (make-trace-step '(bind (pure a) f) '(f a) 'monad-left-id '() '())]
                     [t2 (trace-add-step t step)])
                    (assert-equal 1 (trace-step-count t2))
                    (assert-equal '(f a) (trace-final t2))))
            
            (define-test trace-multiple-steps
              (let* ([t0 (make-trace 'start)]
                     [s1 (make-trace-step 'start 'mid 'r1 '() '())]
                     [t1 (trace-add-step t0 s1)]
                     [s2 (make-trace-step 'mid 'end 'r2 '() '())]
                     [t2 (trace-add-step t1 s2)])
                    (assert-equal 2 (trace-step-count t2))
                    (assert-equal 'start (trace-initial t2))
                    (assert-equal 'end (trace-final t2))))
            
            (define-test trace-rules-used
              (let* ([t0 (make-trace 'x)]
                     [s1 (make-trace-step 'x 'y 'rule-a '() '())]
                     [t1 (trace-add-step t0 s1)]
                     [s2 (make-trace-step 'y 'z 'rule-b '() '())]
                     [t2 (trace-add-step t1 s2)])
                    (assert-equal '(rule-a rule-b) (trace-rules-used t2)))))

;;; ====
;;; Law Registry Tests
;;; ====

(test-group law-registry
            
            (define-test get-law-exists
              (let ([law (get-law 'monad-left-id)])
                   (assert-true (if (rule? law) #t #f))
                   (assert-equal 'monad-left-id (rule-name law))))
            
            (define-test get-law-not-exists
              (assert-false (get-law 'nonexistent-law)))
            
            (define-test laws-by-category-monad
              (let ([monad-laws (laws-by-category 'monad)])
                   (assert-true (> (length monad-laws) 0))
                   (assert-true (for-all (lambda (l) (eq? (rule-category l) 'monad))
                                         monad-laws))))
            
            (define-test laws-by-category-functor
              (let ([functor-laws (laws-by-category 'functor)])
                   (assert-true (> (length functor-laws) 0))))
            
            (define-test all-laws-populated
              (assert-true (> (length (all-laws)) 10)))
            
            (define-test law-categories-list
              (let ([cats (law-categories)])
                   (assert-true (if (memq 'monad cats) #t #f))
                   (assert-true (if (memq 'functor cats) #t #f))
                   (assert-true (if (memq 'monoid cats) #t #f)))))

;;; ====
;;; Verification Tests
;;; ====

(test-group verification
            
            (define-test verify-identical
              (let ([result (verify-equivalence '(+ 1 2) '(+ 1 2))])
                   (assert-true (equiv-ok? result))
                   (assert-true (equiv-result result))
                   (assert-equal 'structural (equiv-reason result))))
            
            (define-test verify-different
              (let ([result (verify-equivalence '(+ 1 2) '(+ 1 3))])
                   (assert-true (equiv-ok? result))
                   (assert-false (equiv-result result))))
            
            (define-test verify-alpha-equiv
              (let ([result (verify-equivalence '(fn (x) x) '(fn (y) y))])
                   (assert-true (equiv-ok? result))
                   (assert-true (equiv-result result))
                   (assert-equal 'alpha (equiv-reason result))))
            
            (define-test verify-alpha-different
              (let ([result (verify-equivalence '(fn (x) x) '(fn (x) y))])
                   (assert-true (equiv-ok? result))
                   (assert-false (equiv-result result))))
            
            (define-test quick-equiv-identical
              (assert-true (quick-equiv? '(+ 1 2) '(+ 1 2))))
            
            (define-test full-equiv-alpha
              (assert-true (full-equiv? '(fn (a) a) '(fn (b) b)))))

;;; ====
;;; Strategy Combinator Tests
;;; ====

(test-group strategy-combinators
            
            (define-test id-strategy-test
              (assert-equal '(+ 1 2) (id-strategy '(+ 1 2))))
            
            (define-test fail-strategy-test
              (assert-false (fail-strategy '(+ 1 2))))
            
            (define-test try-on-success
              (let* ([s (lambda (e) (if (equal? e 'a) 'b #f))]
                     [ts (try s)])
                    (assert-equal 'b (ts 'a))))
            
            (define-test try-on-failure
              (let* ([s (lambda (e) #f)]
                     [ts (try s)])
                    (assert-equal 'x (ts 'x))))
            
            (define-test seq-both-succeed
              (let* ([s1 (lambda (e) (cons 's1 e))]
                     [s2 (lambda (e) (cons 's2 e))]
                     [ss (seq s1 s2)])
                    (assert-equal '(s2 s1 . x) (ss 'x))))
            
            (define-test choice-first-succeeds
              (let* ([s1 (lambda (e) 'first)]
                     [s2 (lambda (e) 'second)]
                     [cs (choice s1 s2)])
                    (assert-equal 'first (cs 'x))))
            
            (define-test choice-first-fails
              (let* ([s1 (lambda (e) #f)]
                     [s2 (lambda (e) 'second)]
                     [cs (choice s1 s2)])
                    (assert-equal 'second (cs 'x))))
            
            (define-test repeat-multiple
              (let* ([counter 0]
                     [s (lambda (e)
                                (if (< counter 3)
                                    (begin (set! counter (+ counter 1)) (+ e 1))
                                    #f))]
                     [rs (repeat s)])
                    (assert-equal 3 (rs 0))))
            
            (define-test rule-to-strategy
              (let* ([rule (make-rule 'add-zero '(+ 0 (?x)) '(?x))]
                     [strat (rule->strategy rule)])
                    (assert-equal 'a (strat '(+ 0 a)))
                    (assert-false (strat '(+ 1 a))))))

;;; ====
;;; Free Variables Tests
;;; ====

(test-group free-variables
            
            (define-test free-vars-symbol
              (assert-equal '(x) (free-vars 'x)))
            
            (define-test free-vars-bound
              (assert-equal '() (free-vars '(fn (x) x))))
            
            (define-test free-vars-mixed
              (let ([fv (free-vars '(fn (x) (+ x y)))])
                   (assert-true (pair? (memq 'y fv)))
                   (assert-false (memq 'x fv))))
            
            (define-test not-free-true
              (assert-true (not-free? 'x '(+ 1 2))))
            
            (define-test not-free-false
              (assert-false (not-free? 'x '(+ x 2)))))

;;; ====
;;; Integration Tests
;;; ====

(test-group integration
            
            (define-test rewrite-with-trace-monad
              (let ([trace (rewrite-with-trace monad-left-id '(bind (pure a) f))])
                   (assert-true (if (trace? trace) #t #f))
                   (assert-equal '(f a) (trace-final trace))
                   (assert-equal 1 (trace-step-count trace))))
            
            (define-test simplify-arithmetic
              (let ([result (arith-simplify '(+ 0 (+ 0 x)))])
                   (assert-equal 'x result)))
            
            (define-test simplify-nested-add-zero
              (let ([result (arith-simplify '(+ 0 (+ 0 (+ 0 a))))])
                   (assert-equal 'a result)))
            
            (define-test simplify-mul-identity
              (let ([result (arith-simplify '(* 1 (* 1 x)))])
                   (assert-equal 'x result)))
            
            (define-test simplify-mul-zero
              (let ([result (arith-simplify '(* 0 (big expression)))])
                   (assert-equal 0 result))))

;;; ====
;;; Run All Tests
;;; ====

(display "\n==============================================================\n")
(display "  Rewrite System Test Suite\n")
(display "==============================================================\n\n")

(run-all-tests)
