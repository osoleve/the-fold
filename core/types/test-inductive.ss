;;; core/types/test-inductive.ss — Tests for Inductive Types
;;;
;;; Tests for inductive type definitions:
;;;   - Type formation (data declarations)
;;;   - Constructor types
;;;   - Eliminator types and synthesis
;;;   - Type display

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/dep-infer.ss")

;;; ====
;;; Test Framework
;;; ====

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "✗ ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-ok name result)
  (test name #t (and (pair? result) (eq? (car result) 'ok))))

(define (test-error name result)
  (test name #t (and (pair? result) (eq? (car result) 'error))))

;;; ====
;;; Test Data Declarations
;;; ====

;; Simple data type: Bool
(define bool-type
  '(data (Bool)
    [true : Bool]
    [false : Bool]))

;; Recursive data type: Nat
(define nat-type
  '(data (Nat)
    [zero : Nat]
    [succ : (Π ((n : Nat)) Nat)]))

;; Parameterized data type: List
(define list-type
  '(data (List A)
    [nil : (List A)]
    [cons : (Π ((x : A)) (Π ((xs : (List A))) (List A)))]))

;; Parameterized with explicit binding: Maybe
(define maybe-type
  '(data (Maybe (A : Type))
    [nothing : (Maybe A)]
    [just : (Π ((x : A)) (Maybe A))]))

;; Binary tree
(define tree-type
  '(data (Tree A)
    [leaf : (Tree A)]
    [node : (Π ((l : (Tree A))) (Π ((v : A)) (Π ((r : (Tree A))) (Tree A))))]))

;;; ====
;;; Tests
;;; ====

(define (run-tests)
  (display "
=== Inductive Types Test Suite ===

")
  
  ;; ----
  ;; Data Type Predicates
  ;; ----
  (display "--- Data Type Predicates ---
")
  
  (test-true "data-type? on Bool"
             (data-type? bool-type))
  (test-true "data-type? on Nat"
             (data-type? nat-type))
  (test-true "data-type? on List"
             (data-type? list-type))
  (test-false "data-type? on Nat (bare symbol)"
              (data-type? 'Nat))
  (test-false "data-type? on Pi type"
              (data-type? '(Π ((x : Nat)) Nat)))
  (test-true "data-type-well-formed? Bool"
             (data-type-well-formed? bool-type))
  (test-true "data-type-well-formed? Nat"
             (data-type-well-formed? nat-type))
  (test-true "data-type-well-formed? List"
             (data-type-well-formed? list-type))
  (test-false "data-type-well-formed? malformed"
              (data-type-well-formed? '(data)))
  (test-false "data-type-well-formed? missing header"
              (data-type-well-formed? '(data [x : Int])))
  
  ;; ----
  ;; Data Type Operations
  ;; ----
  (display "
--- Data Type Operations ---
")
  
  (test "data-name Bool"
        'Bool
        (data-name bool-type))
  (test "data-name Nat"
        'Nat
        (data-name nat-type))
  (test "data-name List"
        'List
        (data-name list-type))
  
  (test "data-params Bool (no params)"
        '()
        (data-params bool-type))
  (test "data-params Nat (no params)"
        '()
        (data-params nat-type))
  (test "data-params List (one param)"
        '(A)
        (data-params list-type))
  (test "data-params Maybe (explicit binding)"
        '((A : Type))
        (data-params maybe-type))
  
  (test "data-constructors Bool"
        '((true . Bool) (false . Bool))
        (data-constructors bool-type))
  (test "data-constructor-names Bool"
        '(true false)
        (data-constructor-names bool-type))
  (test "data-constructor-names Nat"
        '(zero succ)
        (data-constructor-names nat-type))
  
  (test "data-constructor-type Bool true"
        'Bool
        (data-constructor-type bool-type 'true))
  (test "data-constructor-type Nat zero"
        'Nat
        (data-constructor-type nat-type 'zero))
  (test "data-constructor-type Nat succ"
        '(Π ((n : Nat)) Nat)
        (data-constructor-type nat-type 'succ))
  
  (test "data-applied-type Bool"
        'Bool
        (data-applied-type bool-type))
  (test "data-applied-type Nat"
        'Nat
        (data-applied-type nat-type))
  (test "data-applied-type List"
        '(List A)
        (data-applied-type list-type))
  
  ;; ----
  ;; Data Type Construction
  ;; ----
  (display "
--- Data Type Construction ---
")
  
  (test "t-data constructs simple type"
        '(data (Unit) [unit : Unit])
        (t-data 'Unit '() '((unit . Unit))))
  
  (test "t-data-simple constructs Bool"
        '(data (Bool) [true : Bool] [false : Bool])
        (t-data-simple 'Bool '((true . Bool) (false . Bool))))
  
  (test "t-constructor creates declaration"
        '(zero : Nat)
        (t-constructor 'zero 'Nat))
  
  ;; ----
  ;; Type Display
  ;; ----
  (display "
--- Type Display ---
")
  
  (test "dep-type->string for Bool data"
        "data Bool { true : Bool | false : Bool }"
        (dep-type->string bool-type))
  
  (test "dep-type->string for Nat data"
        "data Nat { zero : Nat | succ : Π(n : Nat). Nat }"
        (dep-type->string nat-type))
  
  (test "dep-type->string for List data"
        "data List(A) { nil : (List A) | cons : Π(x : A). Π(xs : (List A)). (List A) }"
        (dep-type->string list-type))
  
  ;; ----
  ;; Wellformedness
  ;; ----
  (display "
--- Wellformedness ---
")
  
  (test-true "well-formed-dep-type? Bool"
             (well-formed-dep-type? bool-type))
  (test-true "well-formed-dep-type? Nat"
             (well-formed-dep-type? nat-type))
  (test-true "well-formed-dep-type? List"
             (well-formed-dep-type? list-type))
  
  ;; ----
  ;; Type Synthesis - Data Declarations
  ;; ----
  (display "
--- Type Synthesis: Data Declarations ---
")
  
  (test-ok "synth Bool data declaration"
           (dep-synth bool-type empty-dep-ctx))
  
  (test "synth Bool yields Type"
        '(ok Type)
        (dep-synth bool-type empty-dep-ctx))
  
  (test-ok "synth Nat data declaration"
           (dep-synth nat-type empty-dep-ctx))
  
  (test "synth Nat yields Type"
        '(ok Type)
        (dep-synth nat-type empty-dep-ctx))
  
  ;; List needs A : Type in context
  (let ([ctx (dep-ctx-extend empty-dep-ctx 'A 'Type)])
       (test-ok "synth List data declaration in context"
                (dep-synth list-type ctx))
       (test "synth List yields Type"
             '(ok Type)
             (dep-synth list-type ctx)))
  
  ;; Error case: constructor returns wrong type
  (let ([bad-data '(data (Foo)
                    [bar : Int])])  ; Int is not Foo
       (test-error "synth bad constructor return type"
                   (dep-synth bad-data empty-dep-ctx)))
  
  ;; ----
  ;; Eliminator Generation
  ;; ----
  (display "
--- Eliminator Generation ---
")
  
  ;; Generate eliminator type for Nat
  (let ([elim-nat-type (generate-eliminator-type nat-type)])
       (test-true "generated eliminator type is Pi"
                  (pi-type? elim-nat-type))
       (test "eliminator-name for Nat"
             'elim-Nat
             (eliminator-name 'Nat)))
  
  ;; ----
  ;; Type Synthesis - Eliminators
  ;; ----
  (display "
--- Type Synthesis: Eliminators ---
")
  
  ;; Test elim-Nat
  (let* ([ctx (dep-ctx-extend
               (dep-ctx-extend
                (dep-ctx-extend
                 (dep-ctx-extend empty-dep-ctx 'P '(Π ((n : Nat)) Type))
                 'z '(P zero))
                's '(Π ((m : Nat)) (Π ((ih : (P m))) (P (succ m)))))
               'n 'Nat)])
        ;; Note: This is a complex test - the eliminator synth checks types
        (test-ok "synth elim-Nat application"
                 (dep-synth '(elim-Nat P z s n) ctx)))
  
  ;; Test elim-Bool
  (let* ([ctx (dep-ctx-extend
               (dep-ctx-extend
                (dep-ctx-extend
                 (dep-ctx-extend empty-dep-ctx 'P '(Π ((b : Bool)) Type))
                 't '(P true))
                'f '(P false))
               'b 'Bool)])
        (test-ok "synth elim-Bool application"
                 (dep-synth '(elim-Bool P t f b) ctx)))
  
  ;; ----
  ;; Helper Functions
  ;; ----
  (display "
--- Helper Functions ---
")
  
  (test-true "string-prefix? matching"
             (string-prefix? "elim-" "elim-Nat"))
  (test-false "string-prefix? non-matching"
              (string-prefix? "elim-" "foo-bar"))
  (test-true "string-prefix? exact"
             (string-prefix? "abc" "abc"))
  
  (test "get-constructor-return-type simple"
        'Nat
        (get-constructor-return-type 'Nat))
  (test "get-constructor-return-type Pi"
        'Nat
        (get-constructor-return-type '(Π ((n : Nat)) Nat)))
  (test "get-constructor-return-type nested Pi"
        '(List A)
        (get-constructor-return-type '(Π ((x : A)) (Π ((xs : (List A))) (List A)))))
  
  (test-true "constructor-returns-type? simple"
             (constructor-returns-type? 'Nat 'Nat))
  (test-true "constructor-returns-type? applied"
             (constructor-returns-type? '(List A) 'List))
  (test-false "constructor-returns-type? wrong type"
              (constructor-returns-type? 'Int 'Nat))
  
  ;; ----
  ;; Integration Tests
  ;; ----
  (display "
--- Integration Tests ---
")
  
  ;; Data type can be used in Pi types
  (let ([pi-with-data (t-pi 'n 'Nat 'Bool)])
       (test-true "Pi type with Nat domain"
                  (pi-type? pi-with-data)))
  
  ;; Parameterized types
  (test "param-binding bare symbol"
        '(A . Type)
        (param-binding 'A))
  (test "param-binding explicit"
        '(n . Nat)
        (param-binding '(n : Nat)))
  
  (test "data-param-bindings List"
        '((A . Type))
        (data-param-bindings list-type))
  (test "data-param-bindings Maybe"
        '((A . Type))
        (data-param-bindings maybe-type))
  
  ;; ----
  ;; Summary
  ;; ----
  (display "
=== Test Summary ===
")
  (display "Total: ")
  (display *test-count*)
  (display ", Passed: ")
  (display *pass-count*)
  (display ", Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!
")
      (display "Some tests failed.
"))
  
  (= *fail-count* 0))

;; Run tests
(run-tests)
