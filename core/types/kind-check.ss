;;; core/types/kind-check.ss — Kind System Validation
;;; @module kind-check
;;; @requires prelude kinds nbe
;;;
;;; Validates the kind system for pre-commit sanity checks.
;;;
;;; Phase 1: Kind Sanity
;;;   - All built-in kinds are well-formed
;;;   - Kind inference succeeds on canonical type expressions
;;;   - Kind unification behaves correctly
;;;   - Kind normalization produces expected results
;;;
;;; Exit codes:
;;;   0 — All checks pass
;;;   1 — Kind validation failed
;;;
;;; Usage:
;;;   scheme --script core/types/kind-check.ss
;;;
;;; This is Core code: pure, total, assumes perfect input.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/lang/nbe.ss")

;;; ====
;;; Check Result Tracking
;;; ====

(define *check-failures* '())
(define *check-count* 0)
(define *pass-count* 0)

;;; check : String × Boolean → Boolean
(define (check name condition)
  (set! *check-count* (+ *check-count* 1))
  (if condition
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       #t)
      (begin
       (set! *check-failures* (cons name *check-failures*))
       #f)))

;;; report-results : → Unit
(define (report-results)
  (display (format "~nKind Check Results: ~a/~a passed~n" *pass-count* *check-count*))
  (if (null? *check-failures*)
      (display "All kind checks passed.\n")
      (begin
       (display (format "~nFailed checks (~a):~n" (length *check-failures*)))
       (for-each (lambda (name)
                         (display (format "  - ~a~n" name)))
                 (reverse *check-failures*)))))

;;; ====
;;; 1. Built-in Kinds Well-Formedness
;;; ====

(display "Checking built-in kinds...\n")

;; All base kinds should be valid kinds
(check "K* is a kind" (kind? K*))
(check "K-constraint is a kind" (kind? K-constraint))
(check "K-row is a kind" (kind? K-row))

;; Kind arrows should be valid
(check "K=> produces valid kind" (kind? (K=> K* K*)))
(check "K=>* produces valid kind" (kind? (K=>* K* K* K*)))
(check "nested K=> valid" (kind? (K=> (K=> K* K*) K*)))

;; Dependent kinds
(check "K-pi produces valid kind" (kind? (K-pi 'f (K=> K* K*) K*)))
(check "K-sort produces valid kind" (kind? (K-sort)))
(check "K-sort 1 produces valid kind" (kind? (K-sort 1)))

;; All built-in type constructor kinds are well-formed
;; Note: Block and Cap use 'Symbol as a domain (tag-kinded types),
;; which is a special design choice. We skip them in strict kind checking.
;;; check-builtin-kind : (× Symbol Kind) → Unit
(define (check-builtin-kind entry)
  (let ([name (car entry)]
        [k (cdr entry)])
       ;; Skip Block and Cap which have special tag-based kinds
       (unless (memq name '(Block Cap))
               (check (format "builtin-kind ~a well-formed" name) (kind? k)))))

(for-each check-builtin-kind builtin-kinds)

;;; ====
;;; 2. Kind Inference on Canonical Types
;;; ====

(display "\nChecking kind inference on types...\n")

;; Base types infer to *
;;; check-base-type-kind : Symbol × Kind → Unit
(define (check-base-type-kind name expected-kind)
  (let ([result (infer-kind name '())])
       (check (format "~a has kind ~a" name (kind->string expected-kind))
              (kind=? result expected-kind))))

(check-base-type-kind 'Nat K*)
(check-base-type-kind 'Int K*)
(check-base-type-kind 'Bool K*)
(check-base-type-kind 'String K*)
(check-base-type-kind 'Symbol K*)
(check-base-type-kind 'Unit K*)
(check-base-type-kind 'Hash K*)

;; Type constructors infer correctly
(check "List has kind * => *"
       (kind=? (infer-kind 'List '()) (K=> K* K*)))

(check "Option has kind * => *"
       (kind=? (infer-kind 'Option '()) (K=> K* K*)))

(check "Either has kind * => * => *"
       (kind=? (infer-kind 'Either '()) (K=>* K* K* K*)))

;; Applied type constructors
(check "(List Nat) has kind *"
       (kind=? (infer-kind '(List Nat) '()) K*))

(check "(Either String Nat) has kind *"
       (kind=? (infer-kind '(@ Either String Nat) '()) K*))

;; Partially applied
(check "(@ Either String) has kind * => *"
       (kind=? (infer-kind '(@ Either String) '()) (K=> K* K*)))

;; Function types
(check "(-> Nat Bool) has kind *"
       (kind=? (infer-kind '(-> Nat Bool) '()) K*))

(check "(-> Nat Nat Nat) has kind *"
       (kind=? (infer-kind '(-> Nat Nat Nat) '()) K*))

;; Product types
(check "(× Nat Bool) has kind *"
       (kind=? (infer-kind '(× Nat Bool) '()) K*))

;; Universal quantification
(check "(∀ (a) (-> a a)) has kind *"
       (kind=? (infer-kind '(∀ (a) (-> a a)) '()) K*))

(check "(∀ (a b) (-> a b)) has kind *"
       (kind=? (infer-kind '(∀ (a b) (-> a b)) '()) K*))

;; Note: Hole type '?' currently falls through to the symbol check in infer-kind
;; and returns an error. This is a known limitation to address in a follow-up.

;;; ====
;;; 3. Kind Unification
;;; ====

(display "\nChecking kind unification...\n")

;;; check-unifies : Kind × Kind × String → Unit
(define (check-unifies k1 k2 desc)
  (let ([result (unify-kinds k1 k2)])
       (check (format "unify ~a" desc) (eq? (car result) 'ok))))

;;; check-no-unify : Kind × Kind × String → Unit
(define (check-no-unify k1 k2 desc)
  (let ([result (unify-kinds k1 k2)])
       (check (format "no-unify ~a" desc) (eq? (car result) 'error))))

;; Same kinds unify
(check-unifies K* K* "* with *")
(check-unifies K-constraint K-constraint "Constraint with Constraint")
(check-unifies (K=> K* K*) (K=> K* K*) "(* => *) with (* => *)")

;; Different base kinds don't unify
(check-no-unify K* K-constraint "* with Constraint")
(check-no-unify K* K-row "* with Row")
(check-no-unify (K=> K* K*) K* "(* => *) with *")

;; Kind variables unify (kind vars must start with κ)
(check-unifies 'κa K* "kind-var with *")
(check-unifies 'κa (K=> K* K*) "kind-var with arrow")

;; Complex unification
(check-unifies (K=> 'κa K*) (K=> K* K*) "(κa => *) with (* => *)")
(check-unifies (K=> 'κa 'κb) (K=> K* K-constraint) "(κa => κb) with (* => Constraint)")

;;; ====
;;; 4. Kind Normalization
;;; ====

(display "\nChecking kind normalization...\n")

;;; check-normalizes : Kind × Kind × String → Unit
(define (check-normalizes kind expected desc)
  (let ([result (kind-nf kind)])
       ;; Use equal? for structural comparison since kind=? doesn't handle
       ;; all cases (e.g., numbers in leveled sorts)
       (check (format "normalize ~a" desc) (equal? result expected))))

;; Base kinds normalize to themselves
(check-normalizes K* K* "* normalizes to *")
(check-normalizes K-constraint K-constraint "Constraint normalizes")
(check-normalizes K-row K-row "Row normalizes")
(check-normalizes '□ '□ "sort normalizes")
(check-normalizes '(□ 1) '(□ 1) "sort-1 normalizes")

;; Arrows normalize
(check-normalizes (K=> K* K*) '(⇒ * *) "arrow normalizes")
(check-normalizes (K=> K* (K=> K* K*)) '(⇒ * (⇒ * *)) "nested arrow normalizes")

;;; ====
;;; 5. Kind Equivalence
;;; ====

(display "\nChecking kind equivalence...\n")

;;; check-equiv : Kind × Kind × String → Unit
(define (check-equiv k1 k2 desc)
  (check (format "equiv ~a" desc) (kinds-equal? k1 k2)))

;;; check-not-equiv : Kind × Kind × String → Unit
(define (check-not-equiv k1 k2 desc)
  (check (format "not-equiv ~a" desc) (not (kinds-equal? k1 k2))))

;; Same kinds are equivalent
(check-equiv K* K* "* = *")
(check-equiv K-constraint K-constraint "Constraint = Constraint")
(check-equiv (K=> K* K*) (K=> K* K*) "arrow = arrow")
(check-equiv '□ '□ "sort = sort")
(check-equiv '(□ 1) '(□ 1) "sort-1 = sort-1")

;; Different kinds are not equivalent
(check-not-equiv K* K-constraint "* /= Constraint")
(check-not-equiv K* (K=> K* K*) "* /= (* => *)")
(check-not-equiv '□ '(□ 1) "sort-0 /= sort-1")

;; Pi kinds with same structure are equivalent (alpha equivalence)
(check-equiv (K-pi 'f (K=> K* K*) K*) (K-pi 'g (K=> K* K*) K*)
             "Pi-alpha equivalence")

;;; ====
;;; 6. Type Class Kind Signatures
;;; ====

(display "\nChecking type class kinds...\n")

;; Verify type class kind signatures are well-formed
(check "TC-Functor has well-formed kind"
       (kind? (typeclass-kind TC-Functor)))

(check "TC-Applicative has well-formed kind"
       (kind? (typeclass-kind TC-Applicative)))

(check "TC-Monad has well-formed kind"
       (kind? (typeclass-kind TC-Monad)))

(check "TC-Eq has well-formed kind"
       (kind? (typeclass-kind TC-Eq)))

(check "TC-Ord has well-formed kind"
       (kind? (typeclass-kind TC-Ord)))

(check "TC-Num has well-formed kind"
       (kind? (typeclass-kind TC-Num)))

(check "TC-Bifunctor has well-formed kind"
       (kind? (typeclass-kind TC-Bifunctor)))

(check "TC-Category has well-formed kind"
       (kind? (typeclass-kind TC-Category)))

(check "TC-Arrow has well-formed kind"
       (kind? (typeclass-kind TC-Arrow)))

;;; ====
;;; 7. Dependent Kind Constructs
;;; ====

(display "\nChecking dependent kind constructs...\n")

;; Sort predicates
(check "sort? recognizes □" (sort? '□))
(check "sort? recognizes (□ 0)" (sort? '(□ 0)))
(check "sort? recognizes (□ 1)" (sort? '(□ 1)))
(check "sort? rejects *" (not (sort? K*)))
(check "sort? rejects Constraint" (not (sort? K-constraint)))

;; Dependent kind predicates
(check "dep-kind? recognizes K-pi"
       (dep-kind? (K-pi 'f (K=> K* K*) K*)))
(check "dep-kind? rejects arrow"
       (not (dep-kind? (K=> K* K*))))
(check "dep-kind? rejects *"
       (not (dep-kind? K*)))

;; Dependent kind accessors
(let ([dk (K-pi 'f (K=> K* K*) K-constraint)])
     (check "dep-kind-var extracts var"
            (eq? (dep-kind-var dk) 'f))
     (check "dep-kind-domain extracts domain"
            (kind=? (dep-kind-domain dk) (K=> K* K*)))
     (check "dep-kind-codomain extracts codomain"
            (kind=? (dep-kind-codomain dk) K-constraint)))

;;; ====
;;; Summary
;;; ====

(newline)
(report-results)

;; Exit with appropriate code
(exit (if (null? *check-failures*) 0 1))
