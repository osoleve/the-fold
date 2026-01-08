;;; core/types/test-kind-check.ss — Tests for Kind Checking Validation
;;;
;;; Tests the kind system validation logic used in kind-check.ss.
;;; This covers kind inference, kind unification, kind errors,
;;; higher-kinded types, and dependent kind constructs.
;;;
;;; Uses core/test-framework.ss for consistent test reporting.

(load "core/test-framework.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/lang/nbe.ss")

(display "
================================================================================
                        KIND CHECKING TESTS
================================================================================
")

;;; ============================================================
;;; Built-in Kinds Well-Formedness
;;; ============================================================

(test-group builtin-kinds-wellformed
            
            (define-test k-star-is-kind
              (assert-true (kind? K*)))
            
            (define-test k-constraint-is-kind
              (assert-true (kind? K-constraint)))
            
            (define-test k-row-is-kind
              (assert-true (kind? K-row)))
            
            (define-test k-arrow-produces-kind
              (assert-true (kind? (K=> K* K*))))
            
            (define-test k-arrow-multi-produces-kind
              (assert-true (kind? (K=>* K* K* K*))))
            
            (define-test k-nested-arrow-valid
              (assert-true (kind? (K=> (K=> K* K*) K*))))
            
            (define-test k-pi-produces-kind
              (assert-true (kind? (K-pi 'f (K=> K* K*) K*))))
            
            (define-test k-sort-0-valid
              (assert-true (kind? (K-sort))))
            
            (define-test k-sort-1-valid
              (assert-true (kind? (K-sort 1)))))

;;; ============================================================
;;; Kind Inference on Base Types
;;; ============================================================

(test-group kind-inference-base-types
            
            (define-test nat-has-kind-star
              (assert-true (kind=? (infer-kind 'Nat '()) K*)))
            
            (define-test int-has-kind-star
              (assert-true (kind=? (infer-kind 'Int '()) K*)))
            
            (define-test bool-has-kind-star
              (assert-true (kind=? (infer-kind 'Bool '()) K*)))
            
            (define-test string-has-kind-star
              (assert-true (kind=? (infer-kind 'String '()) K*)))
            
            (define-test symbol-has-kind-star
              (assert-true (kind=? (infer-kind 'Symbol '()) K*)))
            
            (define-test unit-has-kind-star
              (assert-true (kind=? (infer-kind 'Unit '()) K*)))
            
            (define-test hash-has-kind-star
              (assert-true (kind=? (infer-kind 'Hash '()) K*))))

;;; ============================================================
;;; Kind Inference on Type Constructors
;;; ============================================================

(test-group kind-inference-constructors
            
            (define-test list-kind-star-to-star
              (assert-true (kind=? (infer-kind 'List '()) (K=> K* K*))))
            
            (define-test option-kind-star-to-star
              (assert-true (kind=? (infer-kind 'Option '()) (K=> K* K*))))
            
            (define-test either-kind-star-star-star
              (assert-true (kind=? (infer-kind 'Either '()) (K=>* K* K* K*))))
            
            (define-test list-nat-kind-star
              (assert-true (kind=? (infer-kind '(List Nat) '()) K*)))
            
            (define-test either-string-nat-kind-star
              (assert-true (kind=? (infer-kind '(@ Either String Nat) '()) K*)))
            
            (define-test partial-either-kind
              (assert-true (kind=? (infer-kind '(@ Either String) '()) (K=> K* K*)))))

;;; ============================================================
;;; Kind Inference on Compound Types
;;; ============================================================

(test-group kind-inference-compound
            
            (define-test function-type-kind
              (assert-true (kind=? (infer-kind '(-> Nat Bool) '()) K*)))
            
            (define-test multi-arg-function-kind
              (assert-true (kind=? (infer-kind '(-> Nat Nat Nat) '()) K*)))
            
            (define-test product-type-kind
              (assert-true (kind=? (infer-kind '(× Nat Bool) '()) K*)))
            
            (define-test forall-simple-kind
              (assert-true (kind=? (infer-kind '(∀ (a) (-> a a)) '()) K*)))
            
            (define-test forall-multi-var-kind
              (assert-true (kind=? (infer-kind '(∀ (a b) (-> a b)) '()) K*))))

;;; ============================================================
;;; Kind Unification
;;; ============================================================

(test-group kind-unification
            
            (define-test unify-star-star
              (let ([result (unify-kinds K* K*)])
                   (assert-equal 'ok (car result))))
            
            (define-test unify-constraint-constraint
              (let ([result (unify-kinds K-constraint K-constraint)])
                   (assert-equal 'ok (car result))))
            
            (define-test unify-arrow-arrow
              (let ([result (unify-kinds (K=> K* K*) (K=> K* K*))])
                   (assert-equal 'ok (car result))))
            
            (define-test no-unify-star-constraint
              (let ([result (unify-kinds K* K-constraint)])
                   (assert-equal 'error (car result))))
            
            (define-test no-unify-star-row
              (let ([result (unify-kinds K* K-row)])
                   (assert-equal 'error (car result))))
            
            (define-test no-unify-arrow-star
              (let ([result (unify-kinds (K=> K* K*) K*)])
                   (assert-equal 'error (car result))))
            
            (define-test unify-kind-var-star
              (let ([result (unify-kinds 'κa K*)])
                   (assert-equal 'ok (car result))))
            
            (define-test unify-kind-var-arrow
              (let ([result (unify-kinds 'κa (K=> K* K*))])
                   (assert-equal 'ok (car result))))
            
            (define-test unify-complex-arrows
              (let ([result (unify-kinds (K=> 'κa K*) (K=> K* K*))])
                   (assert-equal 'ok (car result))))
            
            (define-test unify-nested-vars
              (let ([result (unify-kinds (K=> 'κa 'κb) (K=> K* K-constraint))])
                   (assert-equal 'ok (car result)))))

;;; ============================================================
;;; Kind Normalization
;;; ============================================================

(test-group kind-normalization
            
            (define-test normalize-star
              (assert-equal K* (kind-nf K*)))
            
            (define-test normalize-constraint
              (assert-equal K-constraint (kind-nf K-constraint)))
            
            (define-test normalize-row
              (assert-equal K-row (kind-nf K-row)))
            
            (define-test normalize-sort-0
              (assert-equal '□ (kind-nf '□)))
            
            (define-test normalize-sort-1
              (assert-equal '(□ 1) (kind-nf '(□ 1))))
            
            (define-test normalize-arrow
              (assert-equal '(⇒ * *) (kind-nf (K=> K* K*))))
            
            (define-test normalize-nested-arrow
              (assert-equal '(⇒ * (⇒ * *)) (kind-nf (K=> K* (K=> K* K*))))))

;;; ============================================================
;;; Kind Equivalence
;;; ============================================================

(test-group kind-equivalence
            
            (define-test equiv-star-star
              (assert-true (kinds-equal? K* K*)))
            
            (define-test equiv-constraint-constraint
              (assert-true (kinds-equal? K-constraint K-constraint)))
            
            (define-test equiv-arrow-arrow
              (assert-true (kinds-equal? (K=> K* K*) (K=> K* K*))))
            
            (define-test equiv-sort-sort
              (assert-true (kinds-equal? '□ '□)))
            
            (define-test equiv-sort-1-sort-1
              (assert-true (kinds-equal? '(□ 1) '(□ 1))))
            
            (define-test not-equiv-star-constraint
              (assert-false (kinds-equal? K* K-constraint)))
            
            (define-test not-equiv-star-arrow
              (assert-false (kinds-equal? K* (K=> K* K*))))
            
            (define-test not-equiv-sort-levels
              (assert-false (kinds-equal? '□ '(□ 1))))
            
            (define-test equiv-pi-alpha
              (assert-true (kinds-equal?
                            (K-pi 'f (K=> K* K*) K*)
                            (K-pi 'g (K=> K* K*) K*)))))

;;; ============================================================
;;; Type Class Kind Signatures
;;; ============================================================

(test-group typeclass-kinds
            
            (define-test functor-kind-wellformed
              (assert-true (kind? (typeclass-kind TC-Functor))))
            
            (define-test applicative-kind-wellformed
              (assert-true (kind? (typeclass-kind TC-Applicative))))
            
            (define-test monad-kind-wellformed
              (assert-true (kind? (typeclass-kind TC-Monad))))
            
            (define-test eq-kind-wellformed
              (assert-true (kind? (typeclass-kind TC-Eq))))
            
            (define-test ord-kind-wellformed
              (assert-true (kind? (typeclass-kind TC-Ord))))
            
            (define-test num-kind-wellformed
              (assert-true (kind? (typeclass-kind TC-Num))))
            
            (define-test bifunctor-kind-wellformed
              (assert-true (kind? (typeclass-kind TC-Bifunctor))))
            
            (define-test category-kind-wellformed
              (assert-true (kind? (typeclass-kind TC-Category))))
            
            (define-test arrow-kind-wellformed
              (assert-true (kind? (typeclass-kind TC-Arrow)))))

;;; ============================================================
;;; Dependent Kind Constructs
;;; ============================================================

(test-group dependent-kinds
            
            (define-test sort-recognizes-box
              (assert-true (sort? '□)))
            
            (define-test sort-recognizes-box-0
              (assert-true (sort? '(□ 0))))
            
            (define-test sort-recognizes-box-1
              (assert-true (sort? '(□ 1))))
            
            (define-test sort-rejects-star
              (assert-false (sort? K*)))
            
            (define-test sort-rejects-constraint
              (assert-false (sort? K-constraint)))
            
            (define-test dep-kind-recognizes-pi
              (assert-true (dep-kind? (K-pi 'f (K=> K* K*) K*))))
            
            (define-test dep-kind-rejects-arrow
              (assert-false (dep-kind? (K=> K* K*))))
            
            (define-test dep-kind-rejects-star
              (assert-false (dep-kind? K*)))
            
            (define-test dep-kind-var-extracts
              (let ([dk (K-pi 'f (K=> K* K*) K-constraint)])
                   (assert-equal 'f (dep-kind-var dk))))
            
            (define-test dep-kind-domain-extracts
              (let ([dk (K-pi 'f (K=> K* K*) K-constraint)])
                   (assert-true (kind=? (K=> K* K*) (dep-kind-domain dk)))))
            
            (define-test dep-kind-codomain-extracts
              (let ([dk (K-pi 'f (K=> K* K*) K-constraint)])
                   (assert-true (kind=? K-constraint (dep-kind-codomain dk))))))

;;; ============================================================
;;; Kind Error Handling
;;; ============================================================

(test-group kind-errors
            
            (define-test kind-error-applying-base-type
              (let ([result (infer-kind '(@ Nat Bool) '())])
                   (assert-true (and (pair? result) (eq? (car result) 'error)))))
            
            (define-test kind-error-unknown-type
              (let ([result (infer-kind 'UnknownType123 '())])
                   (assert-true (and (pair? result) (eq? (car result) 'error)))))
            
            (define-test kind-error-mismatch
              ;; List expects * but Either has kind * -> * -> *
              (let ([result (infer-kind '(List Either) '())])
                   (assert-true (and (pair? result) (eq? (car result) 'error))))))

;;; ============================================================
;;; Higher-Kinded Type Inference
;;; ============================================================

(test-group hkt-inference
            
            (define-test hkt-env-f-has-arrow-kind
              (let ([env `((f . ,(K=> K* K*)) (a . ,K*))])
                   (assert-true (kind=? (K=> K* K*) (infer-kind 'f env)))))
            
            (define-test hkt-applied-to-a
              (let ([env `((f . ,(K=> K* K*)) (a . ,K*))])
                   (assert-true (kind=? K* (infer-kind '(@ f a) env)))))
            
            (define-test hkt-applied-to-nat
              (let ([env `((f . ,(K=> K* K*)))])
                   (assert-true (kind=? K* (infer-kind '(@ f Nat) env)))))
            
            (define-test binary-constructor-in-env
              (let ([env `((g . ,(K=>* K* K* K*)))])
                   (assert-true (kind=? (K=>* K* K* K*) (infer-kind 'g env)))))
            
            (define-test partial-binary-application
              (let ([env `((g . ,(K=>* K* K* K*)))])
                   (assert-true (kind=? (K=> K* K*) (infer-kind '(@ g Nat) env)))))
            
            (define-test full-binary-application
              (let ([env `((g . ,(K=>* K* K* K*)))])
                   (assert-true (kind=? K* (infer-kind '(@ g Nat Bool) env))))))

;;; ============================================================
;;; Kinded Type Variables
;;; ============================================================

(test-group kinded-type-vars
            
            (define-test kinded-forall-valid
              (let ([type '(∀ ((f : (⇒ * *)) (a : *)) (@ f a))])
                   (assert-true (type? type))))
            
            (define-test kinded-forall-kind
              (let ([type '(∀ ((f : (⇒ * *)) (a : *)) (@ f a))])
                   (assert-true (kind=? K* (infer-kind type '())))))
            
            (define-test mixed-forall-valid
              (let ([type '(∀ (a (f : (⇒ * *))) (@ f a))])
                   (assert-true (type? type))))
            
            (define-test mixed-forall-kind
              (let ([type '(∀ (a (f : (⇒ * *))) (@ f a))])
                   (assert-true (kind=? K* (infer-kind type '())))))
            
            (define-test functor-fmap-style
              (let ([type '(∀ ((f : (⇒ * *)) (a : *) (b : *))
                            (-> (-> a b) (@ f a) (@ f b)))])
                   (assert-true (kind=? K* (infer-kind type '()))))))

;;; ============================================================
;;; Built-in Kind Table Completeness
;;; ============================================================

(test-group builtin-kind-table
            
            (define-test all-base-types-have-star
              (assert-true
               (andmap (lambda (name)
                               (kind=? K* (lookup-kind name)))
                       '(Nat Int Bool Symbol String Bytes Unit Void Hash))))
            
            (define-test all-unary-constructors-have-arrow
              (assert-true
               (andmap (lambda (name)
                               (kind=? (K=> K* K*) (lookup-kind name)))
                       '(List Vector Option Set Ref Stream Delayed))))
            
            (define-test all-binary-constructors-have-double-arrow
              (assert-true
               (andmap (lambda (name)
                               (kind=? (K=>* K* K* K*) (lookup-kind name)))
                       '(Either Pair Result)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "================================================================================")
(newline)
(display "                     KIND CHECK TESTS COMPLETE")
(newline)
(display "================================================================================")
(newline)
(exit-with-summary)
