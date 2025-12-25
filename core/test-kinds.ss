;;; Test harness for core/kinds.ss — Higher-Kinded Types

(load "block.ss")
(load "types.ss")
(load "kinds.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
        (display "✗\n    expected: ")
        (display expected)
        (display "\n    got: ")
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
;;; Instances
;;; ============================================================
(test-section "Instances")
(test "Functor List class" 'Functor (instance-class instance-Functor-List))
(test "Functor List type" 'List (instance-type instance-Functor-List))
(test "Monad Option class" 'Monad (instance-class instance-Monad-Option))

(newline)
(display "✓ All higher-kinded type tests complete.\n")
