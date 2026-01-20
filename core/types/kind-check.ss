(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/lang/nbe.ss")

(doc 'module 'kind-check)
(doc 'description "Validates the kind system for pre-commit sanity checks.")
(doc 'layer 'core)
(doc 'requires '(prelude kinds nbe))

(doc 'note "Phase 1: Kind Sanity - All built-in kinds are well-formed, Kind inference succeeds on canonical type expressions, Kind unification behaves correctly, Kind normalization produces expected results")

(doc 'note "Exit codes: 0 = All checks pass, 1 = Kind validation failed")

(doc 'note "Usage: scheme --script core/types/kind-check.ss")

(doc 'section 'check-result-tracking)

(doc *check-failures* 'type '(List String))
(doc *check-failures* 'description "List of failed check names.")
(define *check-failures* '())

(doc *check-count* 'type 'Nat)
(doc *check-count* 'description "Total number of checks performed.")
(define *check-count* 0)

(doc *pass-count* 'type 'Nat)
(doc *pass-count* 'description "Number of checks passed.")
(define *pass-count* 0)

(define (check name condition)
  (doc 'type '(-> String Bool Bool))
  (doc 'description "Run a check, record result, and return success status.")
  (set! *check-count* (+ *check-count* 1))
  (if condition
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       #t)
      (begin
       (set! *check-failures* (cons name *check-failures*))
       #f)))

(define (report-results)
  (doc 'type '(-> Unit))
  (doc 'description "Report check results to stdout.")
  (display (format "~nKind Check Results: ~a/~a passed~n" *pass-count* *check-count*))
  (if (null? *check-failures*)
      (display "All kind checks passed.\n")
      (begin
       (display (format "~nFailed checks (~a):~n" (length *check-failures*)))
       (for-each (lambda (name)
                         (display (format "  - ~a~n" name)))
                 (reverse *check-failures*)))))

(doc 'section 'builtin-kinds-well-formedness)

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

(doc 'note "Block and Cap use Symbol as a domain (tag-kinded types), which is a special design choice. We skip them in strict kind checking.")

(define (check-builtin-kind entry)
  (doc 'type '(-> (× Symbol Kind) Unit))
  (doc 'description "Check that a builtin kind entry is well-formed.")
  (let ([name (car entry)]
        [k (cdr entry)])
       ;; Skip Block and Cap which have special tag-based kinds
       (unless (memq name '(Block Cap))
               (check (format "builtin-kind ~a well-formed" name) (kind? k)))))

(for-each check-builtin-kind builtin-kinds)

(doc 'section 'kind-inference-on-canonical-types)

(display "\nChecking kind inference on types...\n")

(define (check-base-type-kind name expected-kind)
  (doc 'type '(-> Symbol Kind Unit))
  (doc 'description "Check that a base type has the expected kind.")
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

(doc 'note "Hole type '?' currently falls through to the symbol check in infer-kind and returns an error. This is a known limitation to address in a follow-up.")

(doc 'section 'kind-unification)

(display "\nChecking kind unification...\n")

(define (check-unifies k1 k2 desc)
  (doc 'type '(-> Kind Kind String Unit))
  (doc 'description "Check that two kinds unify successfully.")
  (let ([result (unify-kinds k1 k2)])
       (check (format "unify ~a" desc) (eq? (car result) 'ok))))

(define (check-no-unify k1 k2 desc)
  (doc 'type '(-> Kind Kind String Unit))
  (doc 'description "Check that two kinds fail to unify.")
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

(doc 'section 'kind-normalization)

(display "\nChecking kind normalization...\n")

(define (check-normalizes kind expected desc)
  (doc 'type '(-> Kind Kind String Unit))
  (doc 'description "Check that kind normalizes to expected form. Uses equal? for structural comparison since kind=? doesn't handle all cases (e.g., numbers in leveled sorts).")
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

(doc 'section 'kind-equivalence)

(display "\nChecking kind equivalence...\n")

(define (check-equiv k1 k2 desc)
  (doc 'type '(-> Kind Kind String Unit))
  (doc 'description "Check that two kinds are equivalent.")
  (check (format "equiv ~a" desc) (kinds-equal? k1 k2)))

(define (check-not-equiv k1 k2 desc)
  (doc 'type '(-> Kind Kind String Unit))
  (doc 'description "Check that two kinds are not equivalent.")
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

(check-equiv (K-pi 'f (K=> K* K*) K*) (K-pi 'g (K=> K* K*) K*)
             "Pi-alpha equivalence")

(doc 'section 'type-class-kind-signatures)

(display "\nChecking type class kinds...\n")

(doc 'note "Verify type class kind signatures are well-formed")

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

(doc 'section 'dependent-kind-constructs)

(display "\nChecking dependent kind constructs...\n")
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

(doc 'note "Dependent kind accessors")

(let ([dk (K-pi 'f (K=> K* K*) K-constraint)])
     (check "dep-kind-var extracts var"
            (eq? (dep-kind-var dk) 'f))
     (check "dep-kind-domain extracts domain"
            (kind=? (dep-kind-domain dk) (K=> K* K*)))
     (check "dep-kind-codomain extracts codomain"
            (kind=? (dep-kind-codomain dk) K-constraint)))

(doc 'section 'summary)

(newline)
(report-results)

(exit (if (null? *check-failures*) 0 1))
