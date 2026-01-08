;;; core/dsl/test-match.ss — Tests for Pattern Matching Compilation
;;;
;;; Tests for decision tree compilation and pattern matching.

(load "core/test-framework.ss")
(load "core/dsl/match.ss")

;;; ============================================================
;;; Extended Pattern Tests
;;; ============================================================

(test-group extended-patterns
            
            (define-test as-pattern-creation
              (let ([p (make-as-pattern 'x (make-wildcard-pattern))])
                   (assert-true (as-pattern? p))
                   (assert-equal (as-pattern-name p) 'x)
                   (assert-true (wildcard-pattern? (as-pattern-inner p)))))
            
            (define-test view-pattern-creation
              (let ([p (make-view-pattern 'string->number (make-var-pattern 'n))])
                   (assert-true (view-pattern? p))
                   (assert-equal (view-pattern-fn p) 'string->number)
                   (assert-true (var-pattern? (view-pattern-inner p)))))
            
            (define-test active-pattern-creation
              (let ([p (make-active-pattern 'even '())])
                   (assert-true (active-pattern? p))
                   (assert-equal (active-pattern-name p) 'even)
                   (assert-equal (active-pattern-args p) '()))))

;;; ============================================================
;;; Decision Tree Tests
;;; ============================================================

(test-group decision-tree-construction
            
            (define-test leaf-creation
              (let ([dt (make-leaf 'result '((x . 1)))])
                   (assert-true (leaf? dt))
                   (assert-equal (leaf-action dt) 'result)
                   (assert-equal (leaf-bindings dt) '((x . 1)))))
            
            (define-test fail-creation
              (let ([dt (make-fail)])
                   (assert-true (fail? dt))))
            
            (define-test switch-creation
              (let* ([leaf1 (make-leaf 'a '())]
                     [leaf2 (make-leaf 'b '())]
                     [branch1 (make-branch 'true 0 leaf1)]
                     [dt (make-switch 'x (list branch1) leaf2)])
                    (assert-true (switch? dt))
                    (assert-equal (switch-scrutinee dt) 'x)
                    (assert-equal (length (switch-branches dt)) 1)
                    (assert-true (leaf? (switch-default dt)))))
            
            (define-test guard-creation
              (let* ([success (make-leaf 'yes '())]
                     [failure (make-leaf 'no '())]
                     [dt (make-dt-guard '(> x 0) success failure)])
                    (assert-true (dt-guard? dt))
                    (assert-equal (dt-guard-condition dt) '(> x 0))
                    (assert-true (leaf? (dt-guard-success dt)))
                    (assert-true (leaf? (dt-guard-failure dt))))))

;;; ============================================================
;;; Branch Tests
;;; ============================================================

(test-group branch-construction
            
            (define-test branch-creation
              (let* ([subtree (make-leaf 'action '())]
                     [b (make-branch 'cons 2 subtree)])
                    (assert-true (branch? b))
                    (assert-equal (branch-tag b) 'cons)
                    (assert-equal (branch-arity b) 2)
                    (assert-true (leaf? (branch-subtree b))))))

;;; ============================================================
;;; Clause Tests
;;; ============================================================

(test-group clause-parsing
            
            (define-test parse-simple-clause
              (let ([cl (parse-clause '(_ result))])
                   (assert-true (clause? cl))
                   (assert-true (wildcard-pattern? (clause-pattern cl)))
                   (assert-false (clause-guard cl))
                   (assert-equal (clause-body cl) 'result)))
            
            (define-test parse-clause-with-ctor
              (let ([cl (parse-clause '((cons x xs) (process x xs)))])
                   (assert-true (clause? cl))
                   (assert-true (ctor-pattern? (clause-pattern cl)))))
            
            (define-test parse-clause-with-guard
              (let ([cl (parse-clause '(n (when (> n 0)) (positive n)))])
                   (assert-true (clause? cl))
                   (assert-equal (clause-guard cl) '(> n 0))
                   (assert-equal (clause-body cl) '(positive n)))))

;;; ============================================================
;;; Pattern Parsing Extended Tests
;;; ============================================================

(test-group pattern-parsing-extended
            
            (define-test parse-as-pattern
              (let ([p (parse-pattern-extended '(as whole (cons x xs)))])
                   (assert-true (as-pattern? p))
                   (assert-equal (as-pattern-name p) 'whole)))
            
            (define-test parse-view-pattern
              (let ([p (parse-pattern-extended '(view string->number (just n)))])
                   (assert-true (view-pattern? p))
                   (assert-equal (view-pattern-fn p) 'string->number)))
            
            (define-test parse-active-pattern
              (let ([p (parse-pattern-extended '(active even))])
                   (assert-true (active-pattern? p))
                   (assert-equal (active-pattern-name p) 'even)))
            
            (define-test parse-falls-through-to-base
              (let ([p (parse-pattern-extended '_)])
                   (assert-true (wildcard-pattern? p)))))

;;; ============================================================
;;; Binding Collection Tests
;;; ============================================================

(test-group binding-collection
            
            (define-test collect-from-var-pattern
              (let* ([p (make-var-pattern 'x)]
                     [bindings (collect-bindings p 'scrutinee)])
                    (assert-equal (length bindings) 1)
                    (assert-equal (caar bindings) 'x)
                    (assert-equal (cdar bindings) 'scrutinee)))
            
            (define-test collect-from-wildcard
              (let* ([p (make-wildcard-pattern)]
                     [bindings (collect-bindings p 'scrutinee)])
                    (assert-equal bindings '())))
            
            (define-test collect-from-literal
              (let* ([p (make-literal-pattern 42)]
                     [bindings (collect-bindings p 'scrutinee)])
                    (assert-equal bindings '())))
            
            (define-test collect-from-as-pattern
              (let* ([inner (make-var-pattern 'y)]
                     [p (make-as-pattern 'x inner)]
                     [bindings (collect-bindings p 'scrutinee)])
                    (assert-equal (length bindings) 2)
                    (assert-equal (caar bindings) 'x))))

;;; ============================================================
;;; Decision Tree Code Generation Tests
;;; ============================================================

(test-group dt-code-generation
            
            (define-test leaf-to-code-simple
              (let* ([dt (make-leaf 'result '())]
                     [code (dt-to-code dt '())])
                    (assert-equal code 'result)))
            
            (define-test leaf-to-code-with-bindings
              (let* ([dt (make-leaf 'body '((x . expr)))]
                     [code (dt-to-code dt '())])
                    (assert-true (pair? code))
                    (assert-equal (car code) 'let)))
            
            (define-test fail-to-code
              (let* ([dt (make-fail)]
                     [code (dt-to-code dt '())])
                    (assert-true (pair? code))
                    (assert-equal (car code) 'error)))
            
            (define-test guard-to-code
              (let* ([success (make-leaf 'yes '())]
                     [failure (make-leaf 'no '())]
                     [dt (make-dt-guard 'cond success failure)]
                     [code (dt-to-code dt '())])
                    (assert-true (pair? code))
                    (assert-equal (car code) 'if)
                    (assert-equal (cadr code) 'cond))))

;;; ============================================================
;;; First-Class Pattern Tests
;;; ============================================================

(test-group first-class-patterns
            
            (define-test fcp-creation
              (let* ([p (make-wildcard-pattern)]
                     [fcp (make-first-class-pattern 'any p)])
                    (assert-true (fcp? fcp))
                    (assert-equal (fcp-name fcp) 'any)
                    (assert-true (wildcard-pattern? (fcp-pattern fcp)))))
            
            (define-test pattern-registration
              (register-pattern! 'pair-pattern '(cons _ _))
              (let ([result (lookup-pattern 'pair-pattern)])
                   (assert-true (just? result))))
            
            (define-test pattern-lookup-missing
              (let ([result (lookup-pattern 'nonexistent-pattern)])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Active Pattern Tests
;;; ============================================================

(test-group active-patterns
            
            (define-test builtin-even-registered
              (let ([result (lookup-active-pattern 'even)])
                   (assert-true (just? result))))
            
            (define-test builtin-odd-registered
              (let ([result (lookup-active-pattern 'odd)])
                   (assert-true (just? result))))
            
            (define-test builtin-positive-registered
              (let ([result (lookup-active-pattern 'positive)])
                   (assert-true (just? result))))
            
            (define-test builtin-negative-registered
              (let ([result (lookup-active-pattern 'negative)])
                   (assert-true (just? result))))
            
            (define-test builtin-empty-registered
              (let ([result (lookup-active-pattern 'empty)])
                   (assert-true (just? result))))
            
            (define-test custom-active-pattern-registration
              (register-active-pattern! 'test-active (lambda (x) #t))
              (let ([result (lookup-active-pattern 'test-active)])
                   (assert-true (just? result)))))

;;; ============================================================
;;; List Helper Tests
;;; ============================================================

(test-group list-helpers
            
            (define-test remove-at-first
              (assert-equal (remove-at '(a b c) 0) '(b c)))
            
            (define-test remove-at-middle
              (assert-equal (remove-at '(a b c) 1) '(a c)))
            
            (define-test remove-at-last
              (assert-equal (remove-at '(a b c) 2) '(a b)))
            
            (define-test splice-at-first
              (assert-equal (splice-at '(a b c) 0 '(x y)) '(x y b c)))
            
            (define-test splice-at-middle
              (assert-equal (splice-at '(a b c) 1 '(x y)) '(a x y c)))
            
            (define-test splice-at-empty
              (assert-equal (splice-at '(a b c) 1 '()) '(a c))))

;;; ============================================================
;;; Constructor Info Tests
;;; ============================================================

(test-group ctor-info
            
            (define-test get-arity-nil
              (assert-equal (get-ctor-arity 'nil) 0))
            
            (define-test get-arity-cons
              (assert-equal (get-ctor-arity 'cons) 2))
            
            (define-test get-arity-just
              (assert-equal (get-ctor-arity 'just) 1))
            
            (define-test get-arity-nothing
              (assert-equal (get-ctor-arity 'nothing) 0))
            
            (define-test get-arity-unknown
              (assert-equal (get-ctor-arity 'unknown-ctor) 0))
            
            (define-test generate-field-accesses-zero
              (assert-equal (generate-field-accesses 'x 0) '()))
            
            (define-test generate-field-accesses-two
              (let ([result (generate-field-accesses 'x 2)])
                   (assert-equal (length result) 2)
                   (assert-equal (car result) '(field x 0))
                   (assert-equal (cadr result) '(field x 1)))))

;;; ============================================================
;;; Decision Tree Statistics Tests
;;; ============================================================

(test-group dt-statistics
            
            (define-test dt-size-leaf
              (assert-equal (dt-size (make-leaf 'x '())) 1))
            
            (define-test dt-size-fail
              (assert-equal (dt-size (make-fail)) 1))
            
            (define-test dt-size-guard
              (let* ([s (make-leaf 'yes '())]
                     [f (make-leaf 'no '())]
                     [dt (make-dt-guard 'c s f)])
                    (assert-equal (dt-size dt) 3)))
            
            (define-test dt-depth-leaf
              (assert-equal (dt-depth (make-leaf 'x '())) 1))
            
            (define-test dt-depth-fail
              (assert-equal (dt-depth (make-fail)) 1))
            
            (define-test dt-depth-guard
              (let* ([s (make-leaf 'yes '())]
                     [f (make-leaf 'no '())]
                     [dt (make-dt-guard 'c s f)])
                    (assert-equal (dt-depth dt) 2))))

;;; ============================================================
;;; Pattern Combinator Tests
;;; ============================================================

(test-group pattern-combinators
            
            (define-test pat-and-creates-guard
              (let* ([p1 (make-var-pattern 'x)]
                     [p2 (make-literal-pattern 5)]
                     [combined (pat-and p1 p2)])
                    (assert-true (guard-pattern? combined))))
            
            (define-test pat-not-creates-guard
              (let* ([p (make-literal-pattern 0)]
                     [negated (pat-not p)])
                    (assert-true (guard-pattern? negated)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group match-integration
            
            (define-test compile-simple-wildcard-match
              (let* ([clause (make-clause (make-wildcard-pattern) #f 'result)]
                     [dt (compile-match 'x (list clause))])
                    (assert-true (leaf? dt))
                    (assert-equal (leaf-action dt) 'result)))
            
            (define-test compile-two-clause-match
              (let* ([cl1 (make-clause (make-ctor-pattern 'nil '()) #f 'empty)]
                     [cl2 (make-clause (make-wildcard-pattern) #f 'nonempty)]
                     [dt (compile-match 'x (list cl1 cl2))])
                    ;; Should create a switch or handle both cases
                    (assert-true (or (switch? dt) (leaf? dt)))))
            
            (define-test compile-guarded-clause
              (let* ([clause (make-clause (make-var-pattern 'n) '(> n 0) 'positive)]
                     [dt (compile-match 'x (list clause))])
                    ;; Should include a guard check
                    (assert-true (or (dt-guard? dt) (leaf? dt)))))
            
            (define-test compile-var-pattern-captures-binding
              ;; Test that variable patterns collect bindings
              (let* ([clause (make-clause (make-var-pattern 'v) #f '(use v))]
                     [dt (compile-match 'scrutinee (list clause))])
                    (assert-true (leaf? dt))
                    (assert-equal (leaf-action dt) '(use v))
                    ;; Bindings should contain (v . scrutinee)
                    (let ([bindings (leaf-bindings dt)])
                         (assert-true (not (null? bindings)))
                         (assert-equal (caar bindings) 'v)
                         (assert-equal (cdar bindings) 'scrutinee))))
            
            (define-test compile-ctor-with-var-subpatterns
              ;; Test constructor with variable subpatterns
              ;; cons arity is 2, so we use just (arity 1) for simplicity
              (let* ([subpats (list (make-var-pattern 'inner))]
                     [pattern (make-ctor-pattern 'just subpats)]
                     [clause (make-clause pattern #f '(use inner))]
                     [dt (compile-match 'opt (list clause))])
                    (assert-true (switch? dt))
                    ;; Find the just branch
                    (let ([branches (switch-branches dt)])
                         (assert-true (pair? branches))
                         (let* ([just-branch (car branches)]
                                [subtree (branch-subtree just-branch)])
                               ;; Subtree should be a leaf with bindings for inner
                               (assert-true (leaf? subtree))
                               (let ([bindings (leaf-bindings subtree)])
                                    ;; Should have binding for inner
                                    (assert-true (pair? bindings))
                                    (assert-equal (caar bindings) 'inner)))))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n========================================\n")
(display "Running Pattern Matching Compilation Tests\n")
(display "========================================\n\n")

(run-all-tests)
