;;; Test harness for core/kinds.ss — Higher-Kinded Types

(load "core/blocks/block.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")

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

;;; ============================================================
;;; Category Theory Type Classes
;;; ============================================================
(test-section "Category Theory Type Classes")

;; TC-Contravariant
(test "Contravariant exists" #t (typeclass? TC-Contravariant))
(test "Contravariant name" 'Contravariant (typeclass-name TC-Contravariant))
(test "Contravariant kind" '(⇒ (⇒ * *) Constraint) (typeclass-kind TC-Contravariant))
(test "Contravariant has contramap" #t (pair? (assq 'contramap (typeclass-methods TC-Contravariant))))

;; TC-Bifunctor
(test "Bifunctor exists" #t (typeclass? TC-Bifunctor))
(test "Bifunctor name" 'Bifunctor (typeclass-name TC-Bifunctor))
(test "Bifunctor kind" '(⇒ (⇒ * (⇒ * *)) Constraint) (typeclass-kind TC-Bifunctor))
(test "Bifunctor has bimap" #t (pair? (assq 'bimap (typeclass-methods TC-Bifunctor))))
(test "Bifunctor has first" #t (pair? (assq 'first (typeclass-methods TC-Bifunctor))))
(test "Bifunctor has second" #t (pair? (assq 'second (typeclass-methods TC-Bifunctor))))

;; TC-Category
(test "Category exists" #t (typeclass? TC-Category))
(test "Category name" 'Category (typeclass-name TC-Category))
(test "Category kind" '(⇒ (⇒ * (⇒ * *)) Constraint) (typeclass-kind TC-Category))
(test "Category has id" #t (pair? (assq 'id (typeclass-methods TC-Category))))
(test "Category has compose" #t (pair? (assq '∘ (typeclass-methods TC-Category))))

;; TC-Profunctor
(test "Profunctor exists" #t (typeclass? TC-Profunctor))
(test "Profunctor name" 'Profunctor (typeclass-name TC-Profunctor))
(test "Profunctor has dimap" #t (pair? (assq 'dimap (typeclass-methods TC-Profunctor))))
(test "Profunctor has lmap" #t (pair? (assq 'lmap (typeclass-methods TC-Profunctor))))
(test "Profunctor has rmap" #t (pair? (assq 'rmap (typeclass-methods TC-Profunctor))))

;; TC-Arrow
(test "Arrow exists" #t (typeclass? TC-Arrow))
(test "Arrow name" 'Arrow (typeclass-name TC-Arrow))
(test "Arrow requires Category" '(Category) (typeclass-supers TC-Arrow))
(test "Arrow has arr" #t (pair? (assq 'arr (typeclass-methods TC-Arrow))))

;;; ============================================================
;;; Multi-Parameter Type Classes
;;; ============================================================
(test-section "Multi-Parameter Type Classes")

;; TC-Convertible
(test "Convertible exists" #t (mparam-typeclass? TC-Convertible))
(test "Convertible name" 'Convertible (mparam-typeclass-name TC-Convertible))
(test "Convertible params" '(a b) (mparam-typeclass-params TC-Convertible))
(test "Convertible param-kinds" (list K* K*) (mparam-typeclass-param-kinds TC-Convertible))
(test "Convertible no fundeps" '() (mparam-typeclass-fundeps TC-Convertible))

;; TC-Collection with fundep
(test "Collection exists" #t (mparam-typeclass? TC-Collection))
(test "Collection params" '(c e) (mparam-typeclass-params TC-Collection))
(test "Collection has fundep" 1 (length (mparam-typeclass-fundeps TC-Collection)))

;; TC-MonadReader with fundep
(test "MonadReader exists" #t (mparam-typeclass? TC-MonadReader))
(test "MonadReader params" '(r m) (mparam-typeclass-params TC-MonadReader))
(test "MonadReader requires Monad" '(Monad) (mparam-typeclass-supers TC-MonadReader))
(test "MonadReader fundep m->r" '((m) . (r)) (car (mparam-typeclass-fundeps TC-MonadReader)))

;; TC-MonadState
(test "MonadState exists" #t (mparam-typeclass? TC-MonadState))
(test "MonadState fundep m->s" '((m) . (s)) (car (mparam-typeclass-fundeps TC-MonadState)))
(test "MonadState has get" #t (pair? (assq 'get (mparam-typeclass-methods TC-MonadState))))
(test "MonadState has put" #t (pair? (assq 'put (mparam-typeclass-methods TC-MonadState))))

;; TC-MonadWriter
(test "MonadWriter exists" #t (mparam-typeclass? TC-MonadWriter))
(test "MonadWriter supers" '(Monad Monoid) (mparam-typeclass-supers TC-MonadWriter))

;;; ============================================================
;;; Functional Dependency Utilities
;;; ============================================================
(test-section "Functional Dependency Utilities")

(test "fundep construction" '((a) . (b)) (fundep '(a) '(b)))
(test "fundep-lhs" '(a) (fundep-lhs (fundep '(a) '(b))))
(test "fundep-rhs" '(b) (fundep-rhs (fundep '(a) '(b))))

;; fundeps-closure
(test "fundeps-closure simple"
      '(b a)
      (fundeps-closure (list (fundep '(a) '(b))) '(a)))
(test "fundeps-closure chain"
      '(c b a)
      (fundeps-closure (list (fundep '(a) '(b)) (fundep '(b) '(c))) '(a)))
(test "fundeps-closure no match"
      '(x)
      (fundeps-closure (list (fundep '(a) '(b))) '(x)))

(newline)
(display "All higher-kinded type tests complete.")
(newline)
