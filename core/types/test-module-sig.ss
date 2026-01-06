;;; core/types/test-module-sig.ss — Tests for Module Signatures with Dependent Types
;;;
;;; Tests for module signature integration with dependent types:
;;;   - Signature predicates and operations
;;;   - Signature construction
;;;   - Signature type synthesis
;;;   - Type display

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/dep-infer.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

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

;;; ============================================================
;;; Test Signatures
;;; ============================================================

;; Simple signature with abstract type
(define sig-monoid
  '(sig Monoid
    [type T : Type]
    [val empty : T]
    [val append : (-> T (-> T T))]))

;; Signature with type alias
(define sig-int-monoid
  '(sig IntMonoid
    [type T = Int]
    [val empty : Int]
    [val append : (-> Int (-> Int Int))]))

;; Signature with dependent types
(define sig-sized-list
  '(sig SizedList
    [type A : Type]
    [type List : (-> Type Type)]
    [val nil : (List A)]
    [val cons : (Π ((x : A)) (Π ((xs : (List A))) (List A)))]))

;; Signature with data declaration
(define sig-nat
  '(sig NatSig
    [data (data (Nat)
                [zero : Nat]
                [succ : (Π ((n : Nat)) Nat)])]))

;; Signature with include
(define sig-extended
  '(sig Extended
    [include Monoid]
    [val inverse : (-> T T)]))

;;; ============================================================
;;; Tests
;;; ============================================================

(define (run-tests)
  (display "
=== Module Signature Tests ===

")
  
  ;; --------------------------------------------------------
  ;; Signature Predicates
  ;; --------------------------------------------------------
  (display "--- Signature Predicates ---
")
  
  (test-true "sig-type? on Monoid"
             (sig-type? sig-monoid))
  (test-true "sig-type? on IntMonoid"
             (sig-type? sig-int-monoid))
  (test-false "sig-type? on data"
              (sig-type? '(data (Nat) [zero : Nat])))
  (test-false "sig-type? on symbol"
              (sig-type? 'Monoid))
  
  (test-true "sig-well-formed? Monoid"
             (sig-well-formed? sig-monoid))
  (test-true "sig-well-formed? IntMonoid"
             (sig-well-formed? sig-int-monoid))
  (test-true "sig-well-formed? SizedList"
             (sig-well-formed? sig-sized-list))
  (test-false "sig-well-formed? malformed"
              (sig-well-formed? '(sig)))
  (test-false "sig-well-formed? missing name"
              (sig-well-formed? '(sig [type T : Type])))
  
  ;; --------------------------------------------------------
  ;; Signature Operations
  ;; --------------------------------------------------------
  (display "
--- Signature Operations ---
")
  
  (test "sig-name Monoid"
        'Monoid
        (sig-name sig-monoid))
  (test "sig-name IntMonoid"
        'IntMonoid
        (sig-name sig-int-monoid))
  
  (test "sig-decls length Monoid"
        3
        (length (sig-decls sig-monoid)))
  (test "sig-decls length IntMonoid"
        3
        (length (sig-decls sig-int-monoid)))
  
  ;; --------------------------------------------------------
  ;; Declaration Type Predicates
  ;; --------------------------------------------------------
  (display "
--- Declaration Type Predicates ---
")
  
  (test-true "sig-type-decl? on type decl"
             (sig-type-decl? '(type T : Type)))
  (test-true "sig-type-decl? on type alias"
             (sig-type-decl? '(type T = Int)))
  (test-false "sig-type-decl? on val"
              (sig-type-decl? '(val x : Int)))
  
  (test-true "sig-val-decl? on val decl"
             (sig-val-decl? '(val empty : T)))
  (test-false "sig-val-decl? on type"
              (sig-val-decl? '(type T : Type)))
  
  (test-true "sig-data-decl? on data"
             (sig-data-decl? '(data (data (Nat) [zero : Nat]))))
  (test-false "sig-data-decl? on type"
              (sig-data-decl? '(type T : Type)))
  
  (test-true "sig-include-decl? on include"
             (sig-include-decl? '(include Monoid)))
  (test-false "sig-include-decl? on type"
              (sig-include-decl? '(type T : Type)))
  
  ;; --------------------------------------------------------
  ;; Declaration Operations
  ;; --------------------------------------------------------
  (display "
--- Declaration Operations ---
")
  
  (test-true "sig-type-decl-abstract? on abstract"
             (sig-type-decl-abstract? '(type T : Type)))
  (test-false "sig-type-decl-abstract? on alias"
              (sig-type-decl-abstract? '(type T = Int)))
  
  (test-true "sig-type-decl-alias? on alias"
             (sig-type-decl-alias? '(type T = Int)))
  (test-false "sig-type-decl-alias? on abstract"
              (sig-type-decl-alias? '(type T : Type)))
  
  (test "sig-type-decl-name"
        'T
        (sig-type-decl-name '(type T : Type)))
  (test "sig-type-decl-kind"
        'Type
        (sig-type-decl-kind '(type T : Type)))
  (test "sig-type-decl-def"
        'Int
        (sig-type-decl-def '(type T = Int)))
  
  (test "sig-val-decl-name"
        'empty
        (sig-val-decl-name '(val empty : T)))
  (test "sig-val-decl-type"
        'T
        (sig-val-decl-type '(val empty : T)))
  
  ;; --------------------------------------------------------
  ;; Signature Construction
  ;; --------------------------------------------------------
  (display "
--- Signature Construction ---
")
  
  (test "t-sig constructs signature"
        '(sig Test [type A : Type])
        (t-sig 'Test (list (t-sig-type-abstract 'A 'Type))))
  
  (test "t-sig-type-abstract"
        '(type T : Type)
        (t-sig-type-abstract 'T 'Type))
  
  (test "t-sig-type-alias"
        '(type T = Int)
        (t-sig-type-alias 'T 'Int))
  
  (test "t-sig-val"
        '(val x : Int)
        (t-sig-val 'x 'Int))
  
  (test "t-sig-include"
        '(include Monoid)
        (t-sig-include 'Monoid))
  
  ;; --------------------------------------------------------
  ;; Signature Extraction
  ;; --------------------------------------------------------
  (display "
--- Signature Extraction ---
")
  
  (test "sig-type-names Monoid"
        '(T)
        (sig-type-names sig-monoid))
  
  (test "sig-val-names Monoid"
        '(empty append)
        (sig-val-names sig-monoid))
  
  (test "sig-includes Extended"
        '(Monoid)
        (sig-includes sig-extended))
  
  (test "sig-exports Monoid"
        '(T empty append)
        (sig-exports sig-monoid))
  
  ;; --------------------------------------------------------
  ;; Type Display
  ;; --------------------------------------------------------
  (display "
--- Type Display ---
")
  
  ;; Simple signature
  (let ([simple-sig (t-sig 'Simple (list (t-sig-type-abstract 'T 'Type)
                                         (t-sig-val 'x 'T)))])
       (test-true "dep-type->string for signature contains sig"
                  (string-starts-with? (dep-type->string simple-sig) "sig ")))
  
  ;; --------------------------------------------------------
  ;; Wellformedness
  ;; --------------------------------------------------------
  (display "
--- Wellformedness ---
")
  
  (test-true "well-formed-dep-type? sig-monoid"
             (well-formed-dep-type? sig-monoid))
  (test-true "well-formed-dep-type? sig-int-monoid"
             (well-formed-dep-type? sig-int-monoid))
  
  ;; --------------------------------------------------------
  ;; Type Synthesis
  ;; --------------------------------------------------------
  (display "
--- Type Synthesis ---
")
  
  ;; Simple signature synthesizes to Type
  (let ([simple-sig (t-sig 'Simple (list (t-sig-type-abstract 'T 'Type)))])
       (test-ok "synth simple signature"
                (dep-synth simple-sig empty-dep-ctx))
       (test "synth simple signature yields Type"
             '(ok Type)
             (dep-synth simple-sig empty-dep-ctx)))
  
  ;; Signature with value using abstract type
  (let ([sig-with-val (t-sig 'Test
                             (list (t-sig-type-abstract 'T 'Type)
                                   (t-sig-val 'x 'T)))])
       (test-ok "synth signature with val using abstract type"
                (dep-synth sig-with-val empty-dep-ctx)))
  
  ;; Signature with type alias
  (let ([sig-alias (t-sig 'Test
                          (list (t-sig-type-alias 'T 'Int)
                                (t-sig-val 'x 'T)))])
       (test-ok "synth signature with type alias"
                (dep-synth sig-alias empty-dep-ctx)))
  
  ;; Signature with Pi type
  (let ([sig-pi (t-sig 'Test
                       (list (t-sig-type-abstract 'A 'Type)
                             (t-sig-val 'id '(Π ((x : A)) A))))])
       (test-ok "synth signature with Pi type"
                (dep-synth sig-pi empty-dep-ctx)))
  
  ;; Full Monoid signature
  (test-ok "synth Monoid signature"
           (dep-synth sig-monoid empty-dep-ctx))
  
  ;; --------------------------------------------------------
  ;; Integration with Dependent Types
  ;; --------------------------------------------------------
  (display "
--- Dependent Types Integration ---
")
  
  ;; Signature can contain Pi types
  (test-true "signature with Pi in val type"
             (let ([result (dep-synth sig-sized-list empty-dep-ctx)])
                  (eq? (car result) 'ok)))
  
  ;; Signature can export data types
  (test-ok "synth signature with data export"
           (dep-synth sig-nat empty-dep-ctx))
  
  ;; --------------------------------------------------------
  ;; Summary
  ;; --------------------------------------------------------
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
