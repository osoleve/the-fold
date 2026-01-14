;;; core/types/test-existential.ss — Tests for Existential Types
;;;
;;; Tests for existential type predicates, pack/unpack, and skolemization.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/existential.ss")

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
       (display "  ") (display *pass-count*) (display ". ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
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

;;; ====
;;; Example Types for Testing
;;; ====

;; Simple existential: ∃a. a
(define simple-exist '(∃ ((a : Type)) a))

;; Showable: ∃a. (a × (a → String))
(define showable-exist '(∃ ((a : Type)) (× a (-> a String))))

;; Multi-var: ∃a b. (a × b × (a → b))
(define multi-exist '(∃ ((a : Type) (b : Type)) (× a b (-> a b))))

(define (run-tests)
  (display "
=== Existential Types Test Suite ===

")
  (reset-skolem-counter!)
  
  ;; ----
  ;; Existential Type Predicates
  ;; ----
  (display "--- Existential Type Predicates ---
")
  
  (test-true "existential-type? on ∃"
             (existential-type? simple-exist))
  
  (test-false "existential-type? on Sigma"
              (existential-type? '(Σ ((a : Type)) a)))
  
  (test-false "existential-type? on forall"
              (existential-type? '(∀ (a) a)))
  
  (test-true "existential-well-formed? simple"
             (existential-well-formed? simple-exist))
  
  (test-true "existential-well-formed? showable"
             (existential-well-formed? showable-exist))
  
  (test-true "existential-well-formed? multi"
             (existential-well-formed? multi-exist))
  
  (test-false "existential-well-formed? empty bindings"
              (existential-well-formed? '(∃ () a)))
  
  (test-false "existential-well-formed? malformed"
              (existential-well-formed? '(∃ a)))
  
  ;; ----
  ;; Existential Type Accessors
  ;; ----
  (display "
--- Existential Type Accessors ---
")
  
  (test "existential-vars simple"
        '(a)
        (existential-vars simple-exist))
  
  (test "existential-vars multi"
        '(a b)
        (existential-vars multi-exist))
  
  (test "existential-var-kinds simple"
        '((a . Type))
        (existential-var-kinds simple-exist))
  
  (test "existential-var-kinds multi"
        '((a . Type) (b . Type))
        (existential-var-kinds multi-exist))
  
  (test "existential-body simple"
        'a
        (existential-body simple-exist))
  
  (test "existential-body showable"
        '(× a (-> a String))
        (existential-body showable-exist))
  
  (test "binding-var kinded"
        'x
        (binding-var '(x : Type)))
  
  (test "binding-type kinded"
        'Type
        (binding-type '(x : Type)))
  
  (test "binding-type Nat kind"
        'Nat
        (binding-type '(n : Nat)))
  
  ;; ----
  ;; Existential Type Construction
  ;; ----
  (display "
--- Existential Type Construction ---
")
  
  (test "t-exists simple"
        '(∃ ((a : Type)) a)
        (t-exists '((a . Type)) 'a))
  
  (test "t-exists multi"
        '(∃ ((a : Type) (b : Nat)) (× a b))
        (t-exists '((a . Type) (b . Nat)) '(× a b)))
  
  (test "t-exists-simple"
        '(∃ ((x : Type)) x)
        (t-exists-simple 'x 'x))
  
  ;; ----
  ;; Pack Expression Predicates
  ;; ----
  (display "
--- Pack Expression Predicates ---
")
  
  (let ([valid-pack '(pack Int 42 : (∃ ((a : Type)) a))])
       (test-true "pack-expr? valid"
                  (pack-expr? valid-pack))
       (test-true "pack-well-formed? valid"
                  (pack-well-formed? valid-pack))
       (test "pack-witness-type"
             'Int
             (pack-witness-type valid-pack))
       (test "pack-value"
             42
             (pack-value valid-pack))
       (test "pack-existential-type"
             '(∃ ((a : Type)) a)
             (pack-existential-type valid-pack)))
  
  (test-false "pack-expr? non-pack"
              (pack-expr? '(unpack ((a x) e) body)))
  
  (test-false "pack-well-formed? missing colon"
              (pack-well-formed? '(pack Int 42 (∃ ((a : Type)) a))))
  
  ;; ----
  ;; Unpack Expression Predicates
  ;; ----
  (display "
--- Unpack Expression Predicates ---
")
  
  (let ([valid-unpack '(unpack ((a val) packed) (show val))])
       (test-true "unpack-expr? valid"
                  (unpack-expr? valid-unpack))
       (test-true "unpack-well-formed? valid"
                  (unpack-well-formed? valid-unpack))
       (test "unpack-type-var"
             'a
             (unpack-type-var valid-unpack))
       (test "unpack-val-var"
             'val
             (unpack-val-var valid-unpack))
       (test "unpack-packed-expr"
             'packed
             (unpack-packed-expr valid-unpack))
       (test "unpack-body"
             '(show val)
             (unpack-body valid-unpack)))
  
  (test-false "unpack-expr? non-unpack"
              (unpack-expr? '(pack Int 42 : T)))
  
  (test-false "unpack-well-formed? malformed"
              (unpack-well-formed? '(unpack (a packed) body)))
  
  ;; ----
  ;; Skolemization
  ;; ----
  (display "
--- Skolemization ---
")
  
  (reset-skolem-counter!)
  
  (test "fresh-skolem generates unique"
        'a!1
        (fresh-skolem 'a))
  
  (test "fresh-skolem increments"
        'b!2
        (fresh-skolem 'b))
  
  (test-true "skolem? on skolem"
             (skolem? 'a!1))
  
  (test-false "skolem? on regular"
              (skolem? 'a))
  
  (test-false "skolem? on similar"
              (skolem? 'a1))
  
  (reset-skolem-counter!)
  (let-values ([(body skolems) (skolemize-existential simple-exist)])
              (test "skolemize body"
                    'a!1
                    body)
              (test "skolemize mapping"
                    '((a . a!1))
                    skolems))
  
  (reset-skolem-counter!)
  (let-values ([(body skolems) (skolemize-existential showable-exist)])
              (test "skolemize complex body"
                    '(× a!1 (-> a!1 String))
                    body))
  
  (test-true "type-mentions-skolem? yes"
             (type-mentions-skolem? '(-> a!1 b) 'a!1))
  
  (test-false "type-mentions-skolem? no"
              (type-mentions-skolem? '(-> a b) 'a!1))
  
  (test-true "type-mentions-any-skolem?"
             (type-mentions-any-skolem? '(-> a!1 b!2) '(a!1 b!2)))
  
  (test-false "type-mentions-any-skolem? none"
              (type-mentions-any-skolem? '(-> a b) '(a!1 b!2)))
  
  ;; ----
  ;; Substitution
  ;; ----
  (display "
--- Substitution ---
")
  
  (test "existential-subst simple"
        'Int
        (existential-subst simple-exist 'a 'Int))
  
  (test "existential-subst complex"
        '(× Int (-> Int String))
        (existential-subst showable-exist 'a 'Int))
  
  ;; ----
  ;; Free Variables
  ;; ----
  (display "
--- Free Variables ---
")
  
  (test "existential-free-vars none"
        '()
        (existential-free-vars simple-exist))
  
  (test "existential-free-vars with free"
        '(c)
        (existential-free-vars '(∃ ((a : Type)) (-> a c))))
  
  ;; ----
  ;; Display
  ;; ----
  (display "
--- Display ---
")
  
  (test "existential->string simple"
        "∃ (a : Type). a"
        (existential->string simple-exist))
  
  ;; ----
  ;; Summary
  ;; ----
  (display "
=== Summary ===
")
  (display "Total: ") (display *test-count*) (newline)
  (display "Passed: ") (display *pass-count*) (newline)
  (display "Failed: ") (display *fail-count*) (newline)
  
  (if (= *fail-count* 0)
      (display "
All tests passed!
")
      (begin
       (display "
SOME TESTS FAILED!
")
       (exit 1))))

;; Run tests
(run-tests)
