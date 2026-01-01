;;; Test harness for core/kinds.ss — Higher-Kinded Types

(load "core/block.ss")
(load "core/types.ss")
(load "core/kinds.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗
    expected: ")
       (display expected)
       (display "
    got: ")
       (display actual)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ============================================================
;;; Kind Construction
;;; ============================================================
(test-section "Kind Construction")
(test "base kind *" '* K*)
(test "constraint kind" 'Constraint K-constraint)
(test "kind arrow" '(⇒ * *) (K=> K* K*))
(test "kind arrow chain" '(⇒ * (⇒ * *)) (K=>* K* K* K*))
(test "kind forall" '(κ∀ (κa) (⇒ κa *)) (K-forall '(κa) (K=> 'κa K*)))

;;; ============================================================
;;; Kind Predicates
;;; ============================================================
(test-section "Kind Predicates")
(test "* is kind" #t (kind? '*))
(test "Constraint is kind" #t (kind? 'Constraint))
(test "arrow is kind" #t (kind? '(⇒ * *)))
(test "nested arrow" #t (kind? '(⇒ (⇒ * *) *)))
(test "kind-var?" #t (kind-var? 'κa))
(test "not kind-var" #f (kind-var? 'a))
(test "kind-arrow?" #t (kind-arrow? '(⇒ * *)))
(test "kind-param" '* (kind-param '(⇒ * Constraint)))
(test "kind-result" 'Constraint (kind-result '(⇒ * Constraint)))

;;; ============================================================
;;; Kind Equality
;;; ============================================================
(test-section "Kind Equality")
(test "* = *" #t (kind=? '* '*))
(test "* ≠ Constraint" #f (kind=? '* 'Constraint))
(test "arrow equality" #t (kind=? '(⇒ * *) '(⇒ * *)))
(test "arrow inequality" #f (kind=? '(⇒ * *) '(⇒ * Constraint)))

;;; ============================================================
;;; Type Application
;;; ============================================================
(test-section "Type Application")
(test "type app" '(@ List Nat) (T@ 'List 'Nat))
(test "type app multi" '(@ Either String Nat) (T@ 'Either 'String 'Nat))
(test "type-app?" #t (type-app? '(@ List Nat)))
(test "not type-app" #f (type-app? '(List Nat)))
(test "type-app-head" 'List (type-app-head '(@ List Nat)))
(test "type-app-args" '(Nat) (type-app-args '(@ List Nat)))

;;; ============================================================
;;; Built-in Kinds
;;; ============================================================
(test-section "Built-in Kinds")
(test "Nat : *" '* (lookup-kind 'Nat))
(test "Bool : *" '* (lookup-kind 'Bool))
(test "List : * → *" '(⇒ * *) (lookup-kind 'List))
(test "Vector : * → *" '(⇒ * *) (lookup-kind 'Vector))
(test "Either : * → * → *" '(⇒ * (⇒ * *)) (lookup-kind 'Either))
(test "unknown" #f (lookup-kind 'Unknown))

;;; ============================================================
;;; Kind Inference — Base Types
;;; ============================================================
(test-section "Kind Inference — Base Types")
(test "Nat : *" '* (infer-kind 'Nat '()))
(test "Bool : *" '* (infer-kind 'Bool '()))
(test "String : *" '* (infer-kind 'String '()))

;;; ============================================================
;;; Kind Inference — Type Constructors
;;; ============================================================
(test-section "Kind Inference — Type Constructors")
(test "List : * → *" '(⇒ * *) (infer-kind 'List '()))
(test "Either : * → * → *" '(⇒ * (⇒ * *)) (infer-kind 'Either '()))

;;; ============================================================
;;; Kind Inference — Applied Types
;;; ============================================================
(test-section "Kind Inference — Applied Types")
(test "(List Nat) : *" '* (infer-kind '(List Nat) '()))
(test "(Vector Bool) : *" '* (infer-kind '(Vector Bool) '()))
(test "(@ List Nat) : *" '* (infer-kind '(@ List Nat) '()))
(test "(@ Either String) : * → *" '(⇒ * *) (infer-kind '(@ Either String) '()))
(test "(@ Either String Nat) : *" '* (infer-kind '(@ Either String Nat) '()))

;;; ============================================================
;;; Kind Inference — Compound Types
;;; ============================================================
(test-section "Kind Inference — Compound Types")
(test "(-> Nat Bool) : *" '* (infer-kind '(-> Nat Bool) '()))
(test "(× Nat Bool String) : *" '* (infer-kind '(× Nat Bool String) '()))
(test "(+ (None) (Some Nat)) : *" '* (infer-kind '(+ (None) (Some Nat)) '()))
(test "(Ref Nat) : *" '* (infer-kind '(Ref Nat) '()))

;;; ============================================================
;;; Kind Inference — Quantified Types
;;; ============================================================
(test-section "Kind Inference — Quantified Types")
(test "(∀ (a) a) : *" '* (infer-kind '(∀ (a) a) '()))
(test "(∀ (a b) (-> a b)) : *" '* (infer-kind '(∀ (a b) (-> a b)) '()))
(test "(μ t (+ (Nil) (Cons Nat t))) : *" '* (infer-kind '(μ t (+ (Nil) (Cons Nat t))) '()))

;;; ============================================================
;;; Kind Inference — With Environment
;;; ============================================================
(test-section "Kind Inference — With Environment")
;; Custom type constructor in environment
(define custom-env `((MyFunctor . ,(K=> K* K*))))
(test "custom constructor" '(⇒ * *) (infer-kind 'MyFunctor custom-env))
(test "applied custom" '* (infer-kind '(@ MyFunctor Nat) custom-env))

;;; ============================================================
;;; Kind Errors
;;; ============================================================
(test-section "Kind Errors")
(define err1 (infer-kind '(@ Nat Bool) '()))
(test "kind error is pair" #t (pair? err1))
(test "kind error tag" 'error (car err1))

;;; ============================================================
;;; Type Classes
;;; ============================================================
(test-section "Type Classes")
(test "Functor kind" '(⇒ (⇒ * *) Constraint) (typeclass-kind TC-Functor))
(test "Functor name" 'Functor (typeclass-name TC-Functor))
(test "Functor no supers" '() (typeclass-supers TC-Functor))
(test "Applicative has Functor super" '(Functor) (typeclass-supers TC-Applicative))
(test "Monad has Applicative super" '(Applicative) (typeclass-supers TC-Monad))

;;; ============================================================
;;; Constrained Types
;;; ============================================================
(test-section "Constrained Types")
(define fmap-type
  '(=> ((Functor f))
    (∀ (a b) (-> (-> a b) (@ f a) (@ f b)))))
(test "constrained-type?" #t (constrained-type? fmap-type))
(test "get-constraints" '((Functor f)) (get-constraints fmap-type))
(test "get-underlying-type" '(∀ (a b) (-> (-> a b) (@ f a) (@ f b)))
      (get-underlying-type fmap-type))

;;; ============================================================
;;; Kind Display
;;; ============================================================
(test-section "Kind Display")
(test "display *" "*" (kind->string '*))
(test "display Constraint" "Constraint" (kind->string 'Constraint))
(test "display arrow" "* ⇒ *" (kind->string '(⇒ * *)))
(test "display nested" "(* ⇒ *) ⇒ *" (kind->string '(⇒ (⇒ * *) *)))
(test "display chain" "* ⇒ * ⇒ *" (kind->string '(⇒ * (⇒ * *))))

;;; ============================================================
;;; Kinded Type Variables (HKT)
;;; ============================================================
(test-section "Kinded Type Variables")

;; Test kinded forall syntax: (∀ ((f : (⇒ * *)) (a : *)) (@ f a))
(define hkt-type '(∀ ((f : (⇒ * *)) (a : *)) (@ f a)))
(test "kinded forall is valid type" #t (type? hkt-type))
(test "kinded forall kind" '* (infer-kind hkt-type '()))

;; Test simple + kinded mixed
(define mixed-forall '(∀ (a (f : (⇒ * *))) (@ f a)))
(test "mixed forall is valid" #t (type? mixed-forall))
(test "mixed forall kind" '* (infer-kind mixed-forall '()))

;; Functor-style type: (∀ ((f : (* → *)) a b) (-> (-> a b) (@ f a) (@ f b)))
(define fmap-type-kinded
  '(∀ ((f : (⇒ * *)) (a : *) (b : *))
    (-> (-> a b) (@ f a) (@ f b))))
(test "fmap type is valid" #t (type? fmap-type-kinded))
(test "fmap type kind" '* (infer-kind fmap-type-kinded '()))

;;; ============================================================
;;; Kind Unification
;;; ============================================================
(test-section "Kind Unification")

(test "unify * *" '(ok ()) (unify-kinds '* '*))
(test "unify * Constraint fails" 'error (car (unify-kinds '* 'Constraint)))
(test "unify κa *" '(ok ((κa . *))) (unify-kinds 'κa '*))
(test "unify * κa" '(ok ((κa . *))) (unify-kinds '* 'κa))
(test "unify arrow" '(ok ()) (unify-kinds '(⇒ * *) '(⇒ * *)))
(test "unify arrow with var"
      'ok
      (car (unify-kinds '(⇒ κa *) '(⇒ * *))))
(test "unify nested arrow"
      'ok
      (car (unify-kinds '(⇒ (⇒ * *) *) '(⇒ κa *))))

;;; ============================================================
;;; HKT Kind Inference in Environment
;;; ============================================================
(test-section "HKT Kind Inference in Environment")

;; When f has kind * → *, and we apply it to Nat, result is *
(define hkt-env `((f . ,(K=> K* K*)) (a . ,K*)))
(test "f in env" '(⇒ * *) (infer-kind 'f hkt-env))
(test "(@ f a) with HKT" '* (infer-kind '(@ f a) hkt-env))
(test "(@ f Nat)" '* (infer-kind '(@ f Nat) hkt-env))

;; Binary type constructor in env
(define binary-env `((g . ,(K=>* K* K* K*))))
(test "g : * → * → *" '(⇒ * (⇒ * *)) (infer-kind 'g binary-env))
(test "(@ g Nat)" '(⇒ * *) (infer-kind '(@ g Nat) binary-env))
(test "(@ g Nat Bool)" '* (infer-kind '(@ g Nat Bool) binary-env))

;;; ============================================================
;;; Instances (deferred)
;;; ============================================================
;;; Note: Instance implementations are deferred.
;;; See docs/decisions/ADR-001-type-classes-deferred.md

(newline)
(display "✓ All higher-kinded type tests complete.
")
