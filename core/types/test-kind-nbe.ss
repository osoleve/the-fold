;;; Test harness for Kind NbE (Phase 2)
;;;
;;; Tests the kind normalization by evaluation implementation:
;;;   - Kind semantic values (KV-star, KV-arrow, KV-pi, KV-sort, KV-neutral)
;;;   - Kind evaluation (eval-kind)
;;;   - Kind readback (kind-readback)
;;;   - Kind equivalence (kind-equiv?, kinds-equal?)
;;;   - Kind normalization (kind-normalize, kind-nf)
;;;
;;; Note: This uses the kind syntax from kinds.ss:
;;;   - ⇒ for kind arrows (not =>)
;;;   - □ for sort (not [])
;;;   - Πκ for dependent kinds

(load "core/test-framework.ss")
(load "core/types/kinds.ss")
(load "core/lang/nbe.ss")

(display "
====
                    KIND NbE - PHASE 2 TESTS
====
")

;;; ====
;;; Kind Semantic Value Constructor Tests
;;; ====

(test-group kind-semantic-values
            
            (define-test kv-star-construction
              (assert-equal '(KV-star) (KV-star)))
            
            (define-test kv-star-predicate
              (assert-true (KV-star? (KV-star))))
            
            (define-test kv-constraint-construction
              (assert-equal '(KV-constraint) (KV-constraint)))
            
            (define-test kv-constraint-predicate
              (assert-true (KV-constraint? (KV-constraint))))
            
            (define-test kv-row-construction
              (assert-equal '(KV-row) (KV-row)))
            
            (define-test kv-row-predicate
              (assert-true (KV-row? (KV-row))))
            
            (define-test kv-sort-construction
              (assert-equal '(KV-sort 0) (KV-sort 0))
              (assert-equal '(KV-sort 1) (KV-sort 1)))
            
            (define-test kv-sort-predicate
              (assert-true (KV-sort? (KV-sort 0)))
              (assert-true (KV-sort? (KV-sort 5))))
            
            (define-test kv-sort-level-accessor
              (assert-equal 0 (KV-sort-level (KV-sort 0)))
              (assert-equal 3 (KV-sort-level (KV-sort 3))))
            
            (define-test kv-arrow-construction
              (let ([arr (KV-arrow (KV-star) (make-kind-const-closure (KV-star)))])
                   (assert-true (KV-arrow? arr))))
            
            (define-test kv-arrow-domain-accessor
              (let ([arr (KV-arrow (KV-star) (make-kind-const-closure (KV-constraint)))])
                   (assert-true (KV-star? (KV-arrow-domain arr)))))
            
            (define-test kv-pi-construction
              (let ([pi (KV-pi (KV-star) (make-kind-closure 'k '* '()))])
                   (assert-true (KV-pi? pi))))
            
            (define-test kv-neutral-construction
              (let ([neutral (KV-neutral (KN-var 0))])
                   (assert-true (KV-neutral? neutral))))
            )

;;; ====
;;; Kind Neutral Value Tests
;;; ====

(test-group kind-neutral-values
            
            (define-test kn-var-construction
              (assert-equal '(KN-var 0) (KN-var 0)))
            
            (define-test kn-var-predicate
              (assert-true (KN-var? (KN-var 0)))
              (assert-true (KN-var? (KN-var 'kappa))))
            
            (define-test kn-var-level-accessor
              (assert-equal 0 (KN-var-level (KN-var 0)))
              (assert-equal 'kappa (KN-var-level (KN-var 'kappa))))
            
            (define-test kn-app-construction
              (assert-equal '(KN-app (KN-var 0) (KV-star))
                            (KN-app (KN-var 0) (KV-star))))
            
            (define-test kn-app-predicate
              (assert-true (KN-app? (KN-app (KN-var 0) (KV-star)))))
            )

;;; ====
;;; Kind Closure Tests
;;; ====

(test-group kind-closures
            
            (define-test kind-closure-construction
              (let ([clo (make-kind-closure 'k '* '())])
                   (assert-true (kind-closure? clo))))
            
            (define-test kind-closure-accessors
              (let ([clo (make-kind-closure 'k '(=> * *) '((x . test)))])
                   (assert-equal 'k (kind-closure-param clo))
                   (assert-equal '(=> * *) (kind-closure-body clo))
                   (assert-equal '((x . test)) (kind-closure-env clo))))
            
            (define-test kind-const-closure-construction
              (let ([clo (make-kind-const-closure (KV-star))])
                   (assert-true (kind-const-closure? clo))))
            
            (define-test kind-const-closure-value
              (let ([clo (make-kind-const-closure (KV-constraint))])
                   (assert-true (KV-constraint? (kind-const-closure-value clo)))))
            
            (define-test apply-const-closure
              (let* ([clo (make-kind-const-closure (KV-star))]
                     [result (apply-kind-closure clo (KV-constraint))])
                    (assert-true (KV-star? result))))
            
            (define-test apply-regular-closure
              (let* ([clo (make-kind-closure 'k '* '())]
                     [result (apply-kind-closure clo (KV-constraint))])
                    (assert-true (KV-star? result))))
            )

;;; ====
;;; Kind Evaluation Tests
;;; ====

(test-group kind-evaluation
            
            (define-test eval-star
              (assert-true (KV-star? (eval-kind '* kind-empty-env))))
            
            (define-test eval-constraint
              (assert-true (KV-constraint? (eval-kind 'Constraint kind-empty-env))))
            
            (define-test eval-row
              (assert-true (KV-row? (eval-kind 'Row kind-empty-env))))
            
            (define-test eval-bare-sort
              (let ([result (eval-kind '□ kind-empty-env)])
                   (assert-true (KV-sort? result))
                   (assert-equal 0 (KV-sort-level result))))
            
            (define-test eval-leveled-sort
              (let ([result (eval-kind '(□ 2) kind-empty-env)])
                   (assert-true (KV-sort? result))
                   (assert-equal 2 (KV-sort-level result))))
            
            (define-test eval-kind-arrow
              (let ([result (eval-kind '(⇒ * *) kind-empty-env)])
                   (assert-true (KV-arrow? result))
                   (assert-true (KV-star? (KV-arrow-domain result)))))
            
            (define-test eval-nested-arrow
              (let ([result (eval-kind '(⇒ * (⇒ * *)) kind-empty-env)])
                   (assert-true (KV-arrow? result))
                   (let ([codomain (apply-kind-closure
                                    (KV-arrow-codomain-clo result)
                                    (KV-star))])
                        (assert-true (KV-arrow? codomain)))))
            
            (define-test eval-dep-kind
              (let ([result (eval-kind (K-pi 'f '(⇒ * *) '*) kind-empty-env)])
                   (assert-true (KV-pi? result))
                   (assert-true (KV-arrow? (KV-pi-domain result)))))
            
            (define-test eval-kind-variable
              (let* ([env (kind-env-extend kind-empty-env 'kappa (KV-star))]
                     [result (eval-kind 'kappa env)])
                    (assert-true (KV-star? result))))
            
            (define-test eval-unbound-kind-variable
              (let ([result (eval-kind 'kappa kind-empty-env)])
                   (assert-true (KV-neutral? result))))
            )

;;; ====
;;; Kind Readback Tests
;;; ====

(test-group kind-readback
            
            (define-test readback-star
              (assert-equal '* (kind-readback 0 (KV-star))))
            
            (define-test readback-constraint
              (assert-equal 'Constraint (kind-readback 0 (KV-constraint))))
            
            (define-test readback-row
              (assert-equal 'Row (kind-readback 0 (KV-row))))
            
            (define-test readback-bare-sort
              (assert-equal '□ (kind-readback 0 (KV-sort 0))))
            
            (define-test readback-leveled-sort
              (assert-equal '(□ 3) (kind-readback 0 (KV-sort 3))))
            
            (define-test readback-arrow
              (let ([result (kind-readback 0 (KV-arrow (KV-star)
                                                       (make-kind-const-closure (KV-star))))])
                   (assert-equal '(⇒ * *) result)))
            
            (define-test readback-nested-arrow
              (let* ([inner (KV-arrow (KV-star) (make-kind-const-closure (KV-star)))]
                     [outer (KV-arrow (KV-star) (make-kind-const-closure inner))]
                     [result (kind-readback 0 outer)])
                    (assert-equal '(⇒ * (⇒ * *)) result)))
            
            (define-test readback-neutral-var
              (let ([result (kind-readback 0 (KV-neutral (KN-var 'kappa)))])
                   (assert-equal 'kappa result)))
            )

;;; ====
;;; Kind Normalization Tests
;;; ====

(test-group kind-normalization
            
            (define-test normalize-star
              (assert-equal '* (kind-normalize-closed '*)))
            
            (define-test normalize-constraint
              (assert-equal 'Constraint (kind-normalize-closed 'Constraint)))
            
            (define-test normalize-arrow
              (assert-equal '(⇒ * *) (kind-normalize-closed '(⇒ * *))))
            
            (define-test normalize-nested-arrow
              (assert-equal '(⇒ * (⇒ * *))
                            (kind-normalize-closed '(⇒ * (⇒ * *)))))
            
            (define-test normalize-constraint-arrow
              (assert-equal '(⇒ (⇒ * *) Constraint)
                            (kind-normalize-closed '(⇒ (⇒ * *) Constraint))))
            
            (define-test normalize-sort
              (assert-equal '□ (kind-normalize-closed '□)))
            
            (define-test normalize-leveled-sort
              (assert-equal '(□ 1) (kind-normalize-closed '(□ 1))))
            
            (define-test kind-nf-alias
              (assert-equal '* (kind-nf '*))
              (assert-equal '(⇒ * *) (kind-nf '(⇒ * *))))
            )

;;; ====
;;; Kind Equivalence Tests
;;; ====

(test-group kind-equivalence
            
            (define-test star-equals-star
              (assert-true (kinds-equal? '* '*)))
            
            (define-test constraint-equals-constraint
              (assert-true (kinds-equal? 'Constraint 'Constraint)))
            
            (define-test row-equals-row
              (assert-true (kinds-equal? 'Row 'Row)))
            
            (define-test sort-equals-sort
              (assert-true (kinds-equal? '□ '□))
              (assert-true (kinds-equal? '(□ 1) '(□ 1))))
            
            (define-test star-not-equals-constraint
              (assert-false (kinds-equal? '* 'Constraint)))
            
            (define-test star-not-equals-sort
              (assert-false (kinds-equal? '* '□)))
            
            (define-test sort-level-matters
              (assert-false (kinds-equal? '(□ 0) '(□ 1))))
            
            (define-test arrow-equals-arrow
              (assert-true (kinds-equal? '(⇒ * *) '(⇒ * *))))
            
            (define-test arrow-domain-matters
              (assert-false (kinds-equal? '(⇒ * *) '(⇒ Constraint *))))
            
            (define-test arrow-codomain-matters
              (assert-false (kinds-equal? '(⇒ * *) '(⇒ * Constraint))))
            
            (define-test nested-arrow-equals
              (assert-true (kinds-equal? '(⇒ * (⇒ * *)) '(⇒ * (⇒ * *)))))
            
            (define-test nested-arrow-not-equals
              (assert-false (kinds-equal? '(⇒ * (⇒ * *)) '(⇒ (⇒ * *) *))))
            
            (define-test complex-arrow-equals
              (assert-true (kinds-equal? '(⇒ (⇒ * *) Constraint)
                                         '(⇒ (⇒ * *) Constraint))))
            )

;;; ====
;;; Kind Equivalence with Variables (Structural)
;;; ====

(test-group kind-equiv-structural
            
            (define-test neutrals-same-var
              (let ([v1 (eval-kind 'kappa kind-empty-env)]
                    [v2 (eval-kind 'kappa kind-empty-env)])
                   (assert-true (kind-equiv? 0 v1 v2))))
            
            (define-test neutrals-diff-var
              (let ([v1 (eval-kind 'kappa kind-empty-env)]
                    [v2 (eval-kind 'rho kind-empty-env)])
                   (assert-false (kind-equiv? 0 v1 v2))))
            )

;;; ====
;;; Dependent Kind Tests
;;; ====

(test-group dependent-kinds
            
            (define-test eval-simple-pi-kind
              (let ([result (eval-kind (K-pi 'k '* '*) kind-empty-env)])
                   (assert-true (KV-pi? result))))
            
            (define-test eval-pi-with-arrow-domain
              (let ([result (eval-kind (K-pi 'f '(⇒ * *) '*) kind-empty-env)])
                   (assert-true (KV-pi? result))
                   (assert-true (KV-arrow? (KV-pi-domain result)))))
            
            (define-test eval-nested-pi
              (let ([result (eval-kind (K-pi 'a '* (K-pi 'b '* '*)) kind-empty-env)])
                   (assert-true (KV-pi? result))
                   (let ([inner (apply-kind-closure
                                 (KV-pi-codomain-clo result)
                                 (KV-star))])
                        (assert-true (KV-pi? inner)))))
            
            (define-test pi-equals-pi-same-structure
              (let ([k1 (K-pi 'f '(⇒ * *) '*)]
                    [k2 (K-pi 'g '(⇒ * *) '*)])  ; Different var name, same structure
                   (assert-true (kinds-equal? k1 k2))))
            
            (define-test pi-not-equals-different-domain
              (let ([k1 (K-pi 'f '* '*)]
                    [k2 (K-pi 'f '(⇒ * *) '*)])
                   (assert-false (kinds-equal? k1 k2))))
            
            (define-test pi-not-equals-different-codomain
              (let ([k1 (K-pi 'f '* '*)]
                    [k2 (K-pi 'f '* 'Constraint)])
                   (assert-false (kinds-equal? k1 k2))))
            )

;;; ====
;;; Kind Polymorphism Tests
;;; ====

(test-group kind-polymorphism
            
            (define-test eval-kind-forall
              ;; (κ∀ (κa) (⇒ κa *))
              (let ([result (eval-kind '(κ∀ (κa) (⇒ κa *)) kind-empty-env)])
                   (assert-true (KV-arrow? result))))
            
            (define-test normalize-kind-forall
              (let ([result (kind-normalize-closed '(κ∀ (κa) (⇒ κa *)))])
                   ;; Should normalize to an arrow with neutral domain
                   (assert-true (pair? result))
                   (assert-equal '⇒ (car result))))
            )

;;; ====
;;; Integration: Realistic Kind Expressions
;;; ====

(test-group realistic-kinds
            
            ;; (* -> *) -> Constraint (like Functor)
            (define-test functor-kind
              (let ([k '(⇒ (⇒ * *) Constraint)])
                   (assert-equal k (kind-nf k))))
            
            ;; (* -> * -> *) -> Constraint (like Bifunctor)
            (define-test bifunctor-kind
              (let ([k '(⇒ (⇒ * (⇒ * *)) Constraint)])
                   (assert-equal k (kind-nf k))))
            
            ;; * -> * (like List, Option)
            (define-test unary-constructor-kind
              (let ([k '(⇒ * *)])
                   (assert-equal k (kind-nf k))))
            
            ;; * -> * -> * (like Either, Pair)
            (define-test binary-constructor-kind
              (let ([k '(⇒ * (⇒ * *))])
                   (assert-equal k (kind-nf k))))
            
            ;; Kind equivalence for type class kinds
            (define-test functor-kind-equiv
              (assert-true (kinds-equal? '(⇒ (⇒ * *) Constraint)
                                         '(⇒ (⇒ * *) Constraint))))
            
            ;; Sort level for type-of-types
            (define-test type-of-types-kind
              (assert-true (kinds-equal? '(⇒ * □) '(⇒ * □))))
            )

;;; ====
;;; Edge Cases
;;; ====

(test-group edge-cases
            
            (define-test empty-env-neutral
              (let ([v (eval-kind 'unknown kind-empty-env)])
                   (assert-true (KV-neutral? v))))
            
            (define-test deeply-nested-arrows
              (let ([k '(⇒ * (⇒ * (⇒ * (⇒ * *))))])
                   (assert-equal k (kind-nf k))))
            
            (define-test arrow-to-constraint
              (assert-true (kinds-equal? '(⇒ * Constraint) '(⇒ * Constraint))))
            
            (define-test arrow-to-sort
              (assert-true (kinds-equal? '(⇒ * □) '(⇒ * □))))
            
            ;; KN-app readback: neutral application produces correct syntax
            (define-test readback-kn-app
              (let* ([neutral-app (KN-app (KN-var 'kappa) (KV-star))]
                     [result (kind-readback-neutral 0 neutral-app)])
                    (assert-equal '(kappa *) result)))
            
            ;; Dependent arrow readback: codomain mentioning bound var produces Πκ
            (define-test readback-dependent-arrow
              (let* ([dep-clo (make-kind-closure 'k 'k '())]  ; body is k itself
                     [kv (KV-arrow (KV-star) dep-clo)]
                     [result (kind-readback 0 kv)])
                    ;; Should produce Πκ syntax: (Πκ ((k0 : *)) k0)
                    (assert-equal 'Πκ (car result))
                    (assert-equal '* (caddr (car (cadr result))))))  ; domain is *
            
            ;; Arrow vs Pi equivalence: non-dependent Pi equals arrow
            (define-test arrow-pi-equivalence
              (let* ([arrow-val (eval-kind '(⇒ * *) kind-empty-env)]
                     [pi-val (eval-kind (K-pi 'k '* '*) kind-empty-env)])
                    (assert-true (kind-equiv? 0 arrow-val pi-val))))
            )

;;; ====
;;; Summary
;;; ====

(newline)
(display "====")
(newline)
(display "                    KIND NbE TESTS COMPLETE")
(newline)
(display "====")
(newline)
(exit-with-summary)
