;;; Test harness for dependent kinds (Phase 1 Foundation)
;;;
;;; Tests the new dependent kind constructs:
;;;   - K-pi : dependent kind constructor
;;;   - K-sort : sort (kind of kinds) constructor
;;;   - sort?, dep-kind? : predicates
;;;   - sort-level : level extraction
;;;   - kind->string : pretty printing

(load "core/test-framework.ss")
(load "core/types/kinds.ss")

(display "
================================================================================
                    DEPENDENT KINDS - PHASE 1 FOUNDATION
================================================================================
")

;;; ============================================================
;;; K-sort Constructor Tests
;;; ============================================================

(test-group sort-constructors
            
            (define-test sort-bare
              (assert-equal '□ (K-sort)))
            
            (define-test sort-level-0
              (assert-equal '(□ 0) (K-sort 0)))
            
            (define-test sort-level-1
              (assert-equal '(□ 1) (K-sort 1)))
            
            (define-test sort-level-2
              (assert-equal '(□ 2) (K-sort 2)))
            
            (define-test sort-level-large
              (assert-equal '(□ 42) (K-sort 42)))
            )

;;; ============================================================
;;; sort? Predicate Tests
;;; ============================================================

(test-group sort-predicates
            
            (define-test sort-bare-is-sort
              (assert-true (sort? '□)))
            
            (define-test sort-0-is-sort
              (assert-true (sort? '(□ 0))))
            
            (define-test sort-1-is-sort
              (assert-true (sort? '(□ 1))))
            
            (define-test sort-large-is-sort
              (assert-true (sort? '(□ 100))))
            
            (define-test star-not-sort
              (assert-false (sort? '*)))
            
            (define-test constraint-not-sort
              (assert-false (sort? 'Constraint)))
            
            (define-test arrow-not-sort
              (assert-false (sort? '(⇒ * *))))
            
            (define-test empty-list-not-sort
              (assert-false (sort? '(□))))
            
            (define-test negative-level-not-sort
              (assert-false (sort? '(□ -1))))
            
            (define-test non-integer-level-not-sort
              (assert-false (sort? '(□ "one"))))
            )

;;; ============================================================
;;; sort-level Tests
;;; ============================================================

(test-group sort-level-extraction
            
            (define-test level-of-bare-sort
              (assert-equal 0 (sort-level '□)))
            
            (define-test level-of-sort-0
              (assert-equal 0 (sort-level '(□ 0))))
            
            (define-test level-of-sort-1
              (assert-equal 1 (sort-level '(□ 1))))
            
            (define-test level-of-sort-5
              (assert-equal 5 (sort-level '(□ 5))))
            )

;;; ============================================================
;;; K-pi Constructor Tests
;;; ============================================================

(test-group k-pi-constructors
            
            (define-test k-pi-basic
              (assert-equal '(Πκ ((n : *)) *)
                            (K-pi 'n '* '*)))
            
            (define-test k-pi-with-arrow-domain
              (assert-equal '(Πκ ((f : (⇒ * *))) *)
                            (K-pi 'f '(⇒ * *) '*)))
            
            (define-test k-pi-with-arrow-codomain
              (assert-equal '(Πκ ((n : *)) (⇒ * *))
                            (K-pi 'n '* '(⇒ * *))))
            
            (define-test k-pi-nested
              (assert-equal '(Πκ ((a : *)) (Πκ ((b : *)) *))
                            (K-pi 'a '* (K-pi 'b '* '*))))
            )

;;; ============================================================
;;; dep-kind? Predicate Tests
;;; ============================================================

(test-group dep-kind-predicates
            
            (define-test basic-dep-kind
              (assert-true (dep-kind? (K-pi 'n '* '*))))
            
            (define-test dep-kind-with-arrow
              (assert-true (dep-kind? (K-pi 'f '(⇒ * *) '*))))
            
            (define-test nested-dep-kind
              (assert-true (dep-kind? (K-pi 'a '* (K-pi 'b '* '*)))))
            
            (define-test star-not-dep-kind
              (assert-false (dep-kind? '*)))
            
            (define-test arrow-not-dep-kind
              (assert-false (dep-kind? '(⇒ * *))))
            
            (define-test sort-not-dep-kind
              (assert-false (dep-kind? '□)))
            
            (define-test constraint-not-dep-kind
              (assert-false (dep-kind? 'Constraint)))
            
            (define-test malformed-not-dep-kind-1
              (assert-false (dep-kind? '(Πκ))))
            
            (define-test malformed-not-dep-kind-2
              (assert-false (dep-kind? '(Πκ () *))))
            
            (define-test malformed-not-dep-kind-3
              (assert-false (dep-kind? '(Πκ ((n)) *))))
            )

;;; ============================================================
;;; Dependent Kind Accessor Tests
;;; ============================================================

(test-group dep-kind-accessors
            
            (define-test dep-kind-var-basic
              (let ([dk (K-pi 'n '* '*)])
                   (assert-equal 'n (dep-kind-var dk))))
            
            (define-test dep-kind-var-complex
              (let ([dk (K-pi 'alpha '(⇒ * *) 'Constraint)])
                   (assert-equal 'alpha (dep-kind-var dk))))
            
            (define-test dep-kind-domain-basic
              (let ([dk (K-pi 'n '* '*)])
                   (assert-equal '* (dep-kind-domain dk))))
            
            (define-test dep-kind-domain-arrow
              (let ([dk (K-pi 'f '(⇒ * *) '*)])
                   (assert-equal '(⇒ * *) (dep-kind-domain dk))))
            
            (define-test dep-kind-codomain-basic
              (let ([dk (K-pi 'n '* '*)])
                   (assert-equal '* (dep-kind-codomain dk))))
            
            (define-test dep-kind-codomain-arrow
              (let ([dk (K-pi 'n '* '(⇒ * *))])
                   (assert-equal '(⇒ * *) (dep-kind-codomain dk))))
            
            (define-test dep-kind-codomain-nested
              (let ([inner (K-pi 'b '* '*)]
                    [outer (K-pi 'a '* (K-pi 'b '* '*))])
                   (assert-equal inner (dep-kind-codomain outer))))
            )

;;; ============================================================
;;; kind? with New Forms
;;; ============================================================

(test-group kind-predicate-new-forms
            
            (define-test sort-is-kind
              (assert-true (kind? '□)))
            
            (define-test leveled-sort-is-kind
              (assert-true (kind? '(□ 1))))
            
            (define-test dep-kind-is-kind
              (assert-true (kind? (K-pi 'n '* '*))))
            
            (define-test nested-dep-kind-is-kind
              (assert-true (kind? (K-pi 'a '* (K-pi 'b '* '*)))))
            
            (define-test complex-dep-kind-is-kind
              (assert-true (kind? (K-pi 'f '(⇒ * *) '(⇒ * *)))))
            
            ;; Verify old forms still work
            (define-test star-still-kind
              (assert-true (kind? '*)))
            
            (define-test constraint-still-kind
              (assert-true (kind? 'Constraint)))
            
            (define-test arrow-still-kind
              (assert-true (kind? '(⇒ * *))))
            
            (define-test kappa-forall-still-kind
              (assert-true (kind? '(κ∀ (κa) (⇒ κa *)))))
            )

;;; ============================================================
;;; kind->string for New Forms
;;; ============================================================

(test-group kind-display-new-forms
            
            (define-test display-bare-sort
              (assert-equal "[]" (kind->string '□)))
            
            (define-test display-sort-0
              (assert-equal "[]0" (kind->string '(□ 0))))
            
            (define-test display-sort-1
              (assert-equal "[]1" (kind->string '(□ 1))))
            
            (define-test display-sort-42
              (assert-equal "[]42" (kind->string '(□ 42))))
            
            (define-test display-dep-kind-basic
              (assert-equal "Pi(n : *). *" (kind->string (K-pi 'n '* '*))))
            
            (define-test display-dep-kind-arrow-domain
              (assert-equal "Pi(f : * ⇒ *). *"
                            (kind->string (K-pi 'f '(⇒ * *) '*))))
            
            (define-test display-dep-kind-arrow-codomain
              (assert-equal "Pi(n : *). * ⇒ *"
                            (kind->string (K-pi 'n '* '(⇒ * *)))))
            
            (define-test display-nested-dep-kind
              (assert-equal "Pi(a : *). Pi(b : *). *"
                            (kind->string (K-pi 'a '* (K-pi 'b '* '*)))))
            
            ;; Verify old forms still display correctly
            (define-test display-star-unchanged
              (assert-equal "*" (kind->string '*)))
            
            (define-test display-arrow-unchanged
              (assert-equal "* ⇒ *" (kind->string '(⇒ * *))))
            
            (define-test display-nested-arrow-unchanged
              (assert-equal "(* ⇒ *) ⇒ *" (kind->string '(⇒ (⇒ * *) *))))
            )

;;; ============================================================
;;; Integration: Realistic Use Cases
;;; ============================================================

(test-group realistic-use-cases
            
            ;; Note: Full value-dependent kinds like "Vector : Nat -> * -> *" require
            ;; the universe hierarchy (fold-w5k). Phase 1 supports kind-level Pi types
            ;; where domains are kinds, not types.
            
            ;; HKT abstraction: forall (f : * -> *). f -> f
            ;; This is a type-constructor-polymorphic kind
            (define-test hkt-abstraction
              (let ([hkt-kind (K-pi 'f (K=> '* '*) (K=> 'f 'f))])
                   (assert-true (dep-kind? hkt-kind))
                   (assert-equal 'f (dep-kind-var hkt-kind))
                   (assert-equal '(⇒ * *) (dep-kind-domain hkt-kind))))
            
            ;; kind->string handles arbitrary symbols in domains (for display purposes)
            ;; even if they aren't strictly kinds (like 'Nat)
            (define-test fin-kind-display
              (let ([fin-kind (K-pi 'n 'Nat '*)])
                   (assert-true (dep-kind? fin-kind))
                   ;; Display works even for non-kind domains
                   (assert-equal "Pi(n : Nat). *" (kind->string fin-kind))))
            
            ;; kind? validates that domains are kinds
            ;; 'Nat is not a kind, so this should NOT pass kind?
            (define-test non-kind-domain-rejected
              (let ([invalid-kind (K-pi 'n 'Nat '*)])
                   (assert-true (dep-kind? invalid-kind))  ; Structurally valid
                   (assert-false (kind? invalid-kind))))    ; But not a valid kind
            
            ;; Valid kind-level Pi: domain is a kind
            (define-test valid-pi-kind
              (let ([valid-kind (K-pi 'f (K=> '* '*) '*)])
                   (assert-true (dep-kind? valid-kind))
                   (assert-true (kind? valid-kind))))
            
            ;; Nested valid Pi kinds
            (define-test nested-valid-pi-kinds
              (let ([nested-kind (K-pi 'f (K=> '* '*) (K-pi 'g (K=> '* '*) '*))])
                   (assert-true (kind? nested-kind))
                   (assert-true (dep-kind? nested-kind))
                   (assert-true (dep-kind? (dep-kind-codomain nested-kind)))))
            
            ;; TypeRep : * -> [] (type representations at sort level)
            (define-test type-rep-kind
              (let ([rep-kind (K=> '* '□)])
                   (assert-true (kind? rep-kind))
                   (assert-equal "* ⇒ []" (kind->string rep-kind))))
            )

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "================================================================================")
(newline)
(display "                    DEPENDENT KINDS TESTS COMPLETE")
(newline)
(display "================================================================================")
(newline)
(exit-with-summary)
