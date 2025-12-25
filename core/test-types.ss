;;; Test harness for core/types.ss

(load "block.ss")
(load "types.ss")

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

(newline)
(display "✓ All type system tests complete.\n")
