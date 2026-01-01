;;; Test harness for core/types.ss
;;;
;;; Run from project root: scheme --script fabric/stitches/test-types.ss

(load "core/block.ss")
(load "core/types.ss")

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
;;; Base Types
;;; ============================================================
(test-section "Base Types")
(test "Nat is base" #t (base-type? 'Nat))
(test "Int is base" #t (base-type? 'Int))
(test "Bool is base" #t (base-type? 'Bool))
(test "Symbol is base" #t (base-type? 'Symbol))
(test "String is base" #t (base-type? 'String))
(test "Bytes is base" #t (base-type? 'Bytes))
(test "Unit is base" #t (base-type? 'Unit))
(test "Void is base" #t (base-type? 'Void))
(test "Hash is base" #t (base-type? 'Hash))
(test "Foo is not base" #f (base-type? 'Foo))

;;; ============================================================
;;; Type Predicates
;;; ============================================================
(test-section "Type Predicates")
(test "Nat is type" #t (type? 'Nat))
(test "hole is type" #t (type? '?))
(test "named hole is type" #t (type? '(? foo)))
(test "type var is type" #t (type? 'a))
(test "function type" #t (type? '(-> Nat Nat)))
(test "multi-arg function" #t (type? '(-> Nat Bool String)))
(test "product type" #t (type? '(× Nat Bool)))
(test "sum type" #t (type? '(+ (None) (Some Nat))))
(test "list type" #t (type? '(List Nat)))
(test "vector type" #t (type? '(Vector String)))
(test "block type" #t (type? '(Block foo Bytes)))
(test "ref type" #t (type? '(Ref (Block data Bytes))))
(test "forall type" #t (type? '(∀ (a) (-> a a))))
(test "recursive type" #t (type? '(μ list (+ (Nil) (Cons Nat list)))))
(test "capability type" #t (type? '(Cap FS (-> String Bytes))))

;;; ============================================================
;;; Type Constructors
;;; ============================================================
(test-section "Type Constructors")
(test "t->" '(-> Nat Bool) (t-> 'Nat 'Bool))
(test "t× " '(× Nat Bool String) (t× 'Nat 'Bool 'String))
(test "t+" '(+ (None) (Some Nat)) (t+ '(None) '(Some Nat)))
(test "t-list" '(List Nat) (t-list 'Nat))
(test "t-vector" '(Vector String) (t-vector 'String))
(test "t-block" '(Block foo Bytes) (t-block 'foo 'Bytes))
(test "t-ref" '(Ref (Block data Bytes)) (t-ref '(Block data Bytes)))
(test "t-forall" '(∀ (a b) (-> a b a)) (t-forall '(a b) '(-> a b a)))
(test "t-rec" '(μ t (+ (Nil) (Cons Nat t))) (t-rec 't '(+ (Nil) (Cons Nat t))))
(test "t-cap" '(Cap FS (-> String Bytes)) (t-cap 'FS '(-> String Bytes)))
(test "t-hole" '? (t-hole))
(test "t-named-hole" '(? x) (t-named-hole 'x))

;;; ============================================================
;;; Type Accessors
;;; ============================================================
(test-section "Type Accessors")
(test "function-type?" #t (function-type? '(-> Nat Bool)))
(test "not function-type" #f (function-type? 'Nat))
(test "function-param-types" '(Nat Bool) (function-param-types '(-> Nat Bool String)))
(test "function-return-type" 'String (function-return-type '(-> Nat Bool String)))
(test "product-type?" #t (product-type? '(× Nat Bool)))
(test "product-types" '(Nat Bool String) (product-types '(× Nat Bool String)))
(test "sum-type?" #t (sum-type? '(+ (A) (B Nat))))
(test "sum-variants" '((A) (B Nat)) (sum-variants '(+ (A) (B Nat))))
(test "hole?" #t (hole? '?))
(test "named hole?" #t (hole? '(? foo)))
(test "not hole" #f (hole? 'Nat))
(test "type-var?" #t (type-var? 'a))
(test "type-var? x" #t (type-var? 'x))
(test "not type-var (base)" #f (type-var? 'Nat))
(test "not type-var (upper)" #f (type-var? 'Foo))

;;; ============================================================
;;; Type Equality
;;; ============================================================
(test-section "Type Equality")
(test "base equal" #t (type=? 'Nat 'Nat))
(test "base not equal" #f (type=? 'Nat 'Bool))
(test "function equal" #t (type=? '(-> Nat Bool) '(-> Nat Bool)))
(test "function not equal" #f (type=? '(-> Nat Bool) '(-> Bool Nat)))
(test "nested equal" #t (type=? '(-> (List Nat) (× Bool String))
                                '(-> (List Nat) (× Bool String))))

;;; ============================================================
;;; Free Type Variables
;;; ============================================================
(test-section "Free Type Variables")
(test "no free vars in base" '() (free-tvars 'Nat))
(test "single tvar" '(a) (free-tvars 'a))
(test "tvar in function" '(a b) (free-tvars '(-> a b)))
(test "bound by forall" '() (free-tvars '(∀ (a) (-> a a))))
(test "partially bound" '(b) (free-tvars '(∀ (a) (-> a b))))
(test "bound by mu" '() (free-tvars '(μ t (+ (Nil) (Cons Nat t)))))
;; Kinded type variable tests (HKT support)
(test "kinded forall binds var" '() (free-tvars '(∀ ((f : (⇒ * *))) (@ f Int))))
(test "kinded forall free var" '(a) (free-tvars '(∀ ((f : (⇒ * *))) (@ f a))))
(test "mixed simple+kinded" '(b) (free-tvars '(∀ (a (f : (⇒ * *))) (-> a (@ f b)))))

;;; ============================================================
;;; Type Substitution
;;; ============================================================
(test-section "Type Substitution")
(test "subst base unchanged" 'Nat (subst-type 'Nat 'a 'Bool))
(test "subst tvar" 'Bool (subst-type 'a 'a 'Bool))
(test "subst different tvar" 'b (subst-type 'b 'a 'Bool))
(test "subst in function" '(-> Bool Bool) (subst-type '(-> a a) 'a 'Bool))
(test "no subst under forall" '(∀ (a) (-> a a)) (subst-type '(∀ (a) (-> a a)) 'a 'Bool))
(test "subst free under forall" '(∀ (a) (-> a Bool)) (subst-type '(∀ (a) (-> a b)) 'b 'Bool))
;; Kinded type variable substitution tests
(test "no subst under kinded forall"
      '(∀ ((f : (⇒ * *))) (@ f Int))
      (subst-type '(∀ ((f : (⇒ * *))) (@ f Int)) 'f 'List))
(test "subst free under kinded forall"
      '(∀ ((f : (⇒ * *))) (@ f Bool))
      (subst-type '(∀ ((f : (⇒ * *))) (@ f a)) 'a 'Bool))

;;; ============================================================
;;; Common Type Patterns
;;; ============================================================
(test-section "Common Type Patterns")
(test "t-option" '(+ (None) (Some Nat)) (t-option 'Nat))
(test "t-result" '(+ (Ok Nat) (Err String)) (t-result 'Nat 'String))
(test "t-pair" '(× Nat Bool) (t-pair 'Nat 'Bool))

;;; ============================================================
;;; Type Display
;;; ============================================================
(test-section "Type Display")
(test "display base" "Nat" (type->string 'Nat))
(test "display hole" "?" (type->string '?))
(test "display function" "(Nat → Bool)" (type->string '(-> Nat Bool)))
(test "display product" "(Nat × Bool)" (type->string '(× Nat Bool)))

;;; ============================================================
;;; Type as Block
;;; ============================================================
(test-section "Type as Block")
(define test-type '(-> Nat (List Bool)))
(define type-blk (type->block test-type))
(test "block tag" 'type (block-tag type-blk))
(test "round-trip" test-type (block->type type-blk))
(test "invalid block" #f (block->type (make-block 'not-type (make-bytevector 0) (vector))))

;;; ============================================================
;;; infer.ss — Type Inference Tests
;;; ============================================================

(load "core/infer.ss")

(test-section "Type Inference - Literals")
(test "typeof integer" 'Int (typeof 42))
(test "typeof boolean" 'Bool (typeof #t))
(test "typeof string" 'String (typeof "hello"))
(test "typeof quoted symbol" 'Symbol (typeof '(quote foo)))

(test-section "Type Inference - Lambdas")
(define id-type (typeof '(fn (x) x)))
(test "identity is polymorphic" #t (and (pair? id-type) (eq? (car id-type) '∀)))
(test "identity is function" #t (function-type? (caddr id-type)))

(define const-type (typeof '(fn (x) (fn (y) x))))
(test "const is polymorphic" #t (and (pair? const-type) (eq? (car const-type) '∀)))

(test-section "Type Inference - Application")
(test "apply identity to int" 'Int (typeof '((fn (x) x) 42)))
(test "nested application" 'Int (typeof '(((fn (x) (fn (y) x)) 42) #t)))

(test-section "Type Inference - Let")
(test "let simple" 'Int (typeof '(let ((x 42)) x)))
(test "let polymorphic" 'Int (typeof '(let ((id (fn (x) x))) (id 42))))
(test "let multiple bindings" 'Bool (typeof '(let ((x 42) (y #t)) y)))

(test-section "Type Inference - If")
(test "if simple" 'Int (typeof '(if #t 42 0)))
(define if-error (typeof '(if #t 42 "hello")))
(test "if type error" #t (and (pair? if-error) (eq? (car if-error) 'error)))

(test-section "Type Inference - Unification")
(define unify-ok (unify 'Int 'Int))
(test "unify same" 'ok (car unify-ok))
(define unify-var (unify 'a 'Int))
(test "unify var" 'ok (car unify-var))
(define unify-mismatch (unify 'Int 'Bool))
(test "unify mismatch" 'error (car unify-mismatch))
(define unify-occurs (unify 'a '(List a)))
(test "unify occurs check" 'occurs-check (cadr unify-occurs))

(test-section "Type Inference - Substitution")
(test "apply empty subst" 'Int (apply-subst empty-subst 'Int))
(define s1 (subst-extend empty-subst 'a 'Int))
(test "apply subst simple" 'Int (apply-subst s1 'a))
(test "apply subst function" '(-> Int Bool) (apply-subst s1 '(-> a Bool)))

(test-section "Type Inference - Primitives")
(test "prim add" 'Int (typeof '(prim 'add 1 2)))
(test "prim not" 'Bool (typeof '(prim 'not #t)))
(define cons-type (typeof '(prim 'cons 1 (quote ()))))
(test "prim cons list type" #t (and (pair? cons-type) (eq? (car cons-type) 'List)))

(test-section "Type Inference - Generalization")
(define inst-result (instantiate '(∀ (a) (-> a a))))
(test "instantiate forall" #t (function-type? inst-result))
(test "instantiate non-forall" 'Int (instantiate 'Int))
(define gen-result (generalize empty-tenv '(-> τ1 τ1)))
(test "generalize free vars" #t (and (pair? gen-result) (eq? (car gen-result) '∀)))

(test-section "Type Inference - Error Cases")
(define unbound-error (typeof 'unbound))
(test "unbound variable" #t (and (pair? unbound-error) (eq? (car unbound-error) 'error)))
(define app-error (typeof '(42 "hello")))
(test "type mismatch in app" #t (and (pair? app-error) (eq? (car app-error) 'error)))

;;; ============================================================
;;; kinds.ss — Kind System Tests
;;; ============================================================

(load "core/kinds.ss")

(test-section "Kinds - Predicates")
(test "kind *" #t (kind? '*))
(test "kind Constraint" #t (kind? 'Constraint))
(test "kind Row" #t (kind? 'Row))
(test "kind arrow" #t (kind? '(⇒ * *)))
(test "kind higher arrow" #t (kind? '(⇒ (⇒ * *) *)))
(test "not kind" #f (kind? 'NotAKind))

(test-section "Kinds - Constructors")
(test "K=>" '(⇒ * *) (K=> K* K*))
(test "K=>*" '(⇒ * (⇒ * *)) (K=>* K* K* K*))

(test-section "Kinds - Equality")
(test "kind= *" #t (kind=? K* K*))
(test "kind= arrow" #t (kind=? (K=> K* K*) (K=> K* K*)))
(test "kind≠ different" #f (kind=? K* (K=> K* K*)))

(test-section "Kinds - Builtins")
(test "lookup List" #t (kind=? (lookup-kind 'List) (K=> K* K*)))
(test "lookup Vector" #t (kind=? (lookup-kind 'Vector) (K=> K* K*)))
(test "lookup Int" #t (kind=? (lookup-kind 'Int) K*))
(test "lookup not found" #f (lookup-kind 'NotAType))

(test-section "Kinds - Inference")
(test "infer Int" #t (kind=? (infer-kind 'Int '()) K*))
(test "infer List Int" #t (kind=? (infer-kind '(List Int) '()) K*))
(test "infer function" #t (kind=? (infer-kind '(-> Int Bool) '()) K*))
(test "infer forall" #t (kind=? (infer-kind '(∀ (a) (-> a a)) '()) K*))
(define hole-kind (infer-kind '? '()))
(test "infer hole returns kind hole" #t (or (eq? hole-kind 'κ?) (and (pair? hole-kind) (eq? (car hole-kind) 'error))))
(test "infer product" #t (kind=? (infer-kind '(× Int Bool) '()) K*))
(test "infer mu" #t (kind=? (infer-kind '(μ t (List t)) '()) K*))

(test-section "Kinds - Display")
(test "kind->string *" "*" (kind->string K*))
(test "kind->string arrow" "* ⇒ *" (kind->string (K=> K* K*)))
(test "kind->string Constraint" "Constraint" (kind->string K-constraint))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-section "Integration - Polymorphism")
(define poly-id (typeof '(fn (x) x)))
(test "polymorphic identity" #t (and (pair? poly-id) (eq? (car poly-id) '∀)))

(define let-poly (typeof '(let ((id (fn (x) x))) ((id id) 42))))
(test "let polymorphism" 'Int let-poly)

(define compose (typeof '(fn (f) (fn (g) (fn (x) (f (g x)))))))
(test "compose function" #t (and (pair? compose) (eq? (car compose) '∀)))

(define factorial (typeof '(fix (fact) (fn (n) (if (prim 'zero? n) 1 (prim 'mul n (fact (prim 'sub n 1))))))))
(test "factorial recursion" #t (function-type? factorial))

(define annotated (typeof '(: 42 Int)))
(test "type annotation" 'Int annotated)

(define poly-list (typeof '(let ((id (fn (x) x)))
                            (prim 'cons (id 1) (prim 'cons (id 2) (quote ()))))))
(test "polymorphic list ops" #t (and (pair? poly-list) (or (eq? (car poly-list) 'List) (eq? (car poly-list) '∀))))

(newline)
(display "✓ All type system tests complete (types.ss, infer.ss, kinds.ss).
")
