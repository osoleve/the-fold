;;; core/types/test-existential-infer.ss — Tests for Existential Type Inference
;;;
;;; Tests for the inference algorithms in existential-infer.ss:
;;;   - existential-infer-synth (existential type synthesis)
;;;   - existential-infer-synth-pack (pack expression synthesis)
;;;   - existential-infer-synth-unpack (unpack expression synthesis)

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/lang/nbe.ss")
(load "core/types/infer.ss")
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
       (display "  ")
       (display *pass-count*)
       (display ". ")
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

(define (test-ok name result)
  (set! *test-count* (+ *test-count* 1))
  (if (and (pair? result) (eq? (car result) 'ok))
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display *pass-count*)
       (display ". ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
       (display name)
       (newline)
       (display "  Expected: (ok ...)
")
       (display "  Actual:   ")
       (write result)
       (newline))))

(define (test-ok-type name result expected-type)
  (set! *test-count* (+ *test-count* 1))
  (if (and (pair? result)
           (eq? (car result) 'ok)
           (equal? (cadr result) expected-type))
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display *pass-count*)
       (display ". ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
       (display name)
       (newline)
       (display "  Expected: (ok ")
       (write expected-type)
       (display ")
")
       (display "  Actual:   ")
       (write result)
       (newline))))

(define (test-error name result expected-error-tag)
  (set! *test-count* (+ *test-count* 1))
  (if (and (pair? result)
           (eq? (car result) 'error)
           (eq? (cadr result) expected-error-tag))
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display *pass-count*)
       (display ". ")
       (display name)
       (display " (correctly errors: ")
       (display expected-error-tag)
       (display ")")
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
       (display name)
       (newline)
       (display "  Expected error: ")
       (write expected-error-tag)
       (newline)
       (display "  Actual:   ")
       (write result)
       (newline))))

(define (test-any-error name result)
  (set! *test-count* (+ *test-count* 1))
  (if (and (pair? result) (eq? (car result) 'error))
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display *pass-count*)
       (display ". ")
       (display name)
       (display " (correctly errors)")
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
       (display name)
       (newline)
       (display "  Expected: (error ...)
")
       (display "  Actual:   ")
       (write result)
       (newline))))

;;; ============================================================
;;; Test Setup
;;; ============================================================

;;; Helper to synthesize with empty context
(define (synth-empty expr)
  (dep-synth expr empty-dep-ctx))

;;; Helper to synthesize with a given context
(define (synth-with-ctx expr ctx)
  (dep-synth expr ctx))

;;; Build a context with some bindings
(define (make-test-ctx . bindings)
  (let loop ([bindings bindings] [ctx empty-dep-ctx])
       (if (null? bindings)
           ctx
           (loop (cddr bindings)
                 (dep-ctx-extend ctx (car bindings) (cadr bindings))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(define (run-tests)
  (display "
=== Existential Type Inference Test Suite ===

")
  (reset-skolem-counter!)
  
  ;; --------------------------------------------------------
  ;; Existential Type Synthesis (existential-infer-synth)
  ;; --------------------------------------------------------
  (display "--- Existential Type Synthesis ---
")
  
  ;; Simple existential types synthesize to Type
  (test-ok-type "simple existential (∃ ((a : Type)) a) : Type"
                (synth-empty '(∃ ((a : Type)) a))
                'Type)
  (test-ok-type "existential with arrow body"
                (synth-empty '(∃ ((a : Type)) (-> a a)))
                'Type)
  
  ;; Malformed existential - empty bindings (uses ∃ directly to test predicate)
  (test-any-error "malformed existential: empty bindings"
                  (synth-empty '(∃ () a)))
  
  ;; Malformed existential - missing body
  (test-any-error "malformed existential: missing body"
                  (synth-empty '(∃ ((a : Type)))))
  
  ;; --------------------------------------------------------
  ;; Pack Expression Synthesis (existential-infer-synth-pack)
  ;; --------------------------------------------------------
  (display "
--- Pack Expression Synthesis ---
")
  
  ;; Simple pack: hide Int inside ∃a.a
  (let ([pack-expr '(pack Int 42 : (∃ ((a : Type)) a))])
       (test-ok "pack Int 42 : ∃a.a synthesizes"
                (synth-empty pack-expr))
       (let ([result (synth-empty pack-expr)])
            (when (and (pair? result) (eq? (car result) 'ok))
                  (test-true "pack result is the existential type"
                             (existential-type? (cadr result))))))
  
  ;; Pack with function type
  (let ([pack-expr '(pack Int (fn ((x : Int)) x) : (∃ ((a : Type)) (-> a a)))])
       (test-ok "pack Int (fn (x) x) : ∃a.(a -> a) synthesizes"
                (synth-empty pack-expr)))
  
  ;; Pack with product (showable pattern)
  (let ([pack-expr '(pack Int (pair 42 (fn ((x : Int)) "number")) : (∃ ((a : Type)) (× a (-> a String))))])
       (test-ok "pack showable pattern synthesizes"
                (synth-empty pack-expr)))
  
  ;; Pack type mismatch: witness doesn't match value
  (let ([pack-expr '(pack Bool 42 : (∃ ((a : Type)) a))])
       (test-any-error "pack type mismatch: Bool witness, Int value"
                       (synth-empty pack-expr)))
  
  ;; Pack to non-existential type
  (let ([pack-expr '(pack Int 42 : Int)])
       (test-error "pack to non-existential errors"
                   (synth-empty pack-expr)
                   'pack-not-existential))
  
  ;; Multi-variable pack
  (let ([pack-expr '(pack (Int Bool) (pair 42 #t) : (∃ ((a : Type) (b : Type)) (× a b)))])
       (test-ok "multi-var pack synthesizes"
                (synth-empty pack-expr)))
  
  ;; Multi-var pack with wrong witness count
  (let ([pack-expr '(pack Int (pair 42 #t) : (∃ ((a : Type) (b : Type)) (× a b)))])
       (test-error "multi-var pack witness count mismatch"
                   (synth-empty pack-expr)
                   'pack-witness-count-mismatch))
  
  ;; --------------------------------------------------------
  ;; Unpack Expression Synthesis (existential-infer-synth-unpack)
  ;; --------------------------------------------------------
  (display "
--- Unpack Expression Synthesis ---
")
  
  ;; Set up context with a packed value
  (let* ([exist-type '(∃ ((a : Type)) a)]
         [ctx (make-test-ctx 'packed exist-type)])
        
        ;; Simple unpack that returns a non-skolem type
        (let ([unpack-expr '(unpack ((a val) packed) 42)])
             (test-ok-type "unpack returning Int"
                           (synth-with-ctx unpack-expr ctx)
                           'Int))
        
        ;; Unpack using the value
        (let ([unpack-expr '(unpack ((a val) packed) val)])
             ;; This should error because result type mentions skolem
             (test-error "unpack with escaped skolem"
                         (synth-with-ctx unpack-expr ctx)
                         'skolem-escape)))
  
  ;; Unpack with simple body returning concrete type
  (let* ([fn-exist-type '(∃ ((a : Type)) (-> a Int))]
         [ctx (make-test-ctx 'converter fn-exist-type)])
        ;; Unpack and return a concrete type (not using the abstract value)
        (let ([unpack-expr '(unpack ((a conv) converter) 100)])
             (test-ok-type "unpack function existential, return Int"
                           (synth-with-ctx unpack-expr ctx)
                           'Int)))
  
  ;; Unpack non-existential
  (let* ([ctx (make-test-ctx 'not-exist 'Int)])
        (let ([unpack-expr '(unpack ((a val) not-exist) val)])
             (test-error "unpack non-existential errors"
                         (synth-with-ctx unpack-expr ctx)
                         'unpack-not-existential)))
  
  ;; Multi-variable unpack - returns concrete type
  ;; Syntax: (unpack (((t1 t2) val) expr) body)
  (let* ([multi-exist '(∃ ((a : Type) (b : Type)) (× a b))]
         [ctx (make-test-ctx 'multi multi-exist)])
        (let ([unpack-expr '(unpack (((a b) val) multi) 42)])
             (test-ok-type "multi-var unpack returns Int"
                           (synth-with-ctx unpack-expr ctx)
                           'Int)))
  
  ;; Multi-var unpack with type var count mismatch
  (let* ([multi-exist '(∃ ((a : Type) (b : Type)) (× a b))]
         [ctx (make-test-ctx 'multi multi-exist)])
        (let ([unpack-expr '(unpack ((a val) multi) 42)])  ; Only one type var, but need two
             (test-error "multi-var unpack type var count mismatch"
                         (synth-with-ctx unpack-expr ctx)
                         'unpack-type-var-count-mismatch)))
  
  ;; --------------------------------------------------------
  ;; Integration Tests
  ;; --------------------------------------------------------
  (display "
--- Integration Tests ---
")
  
  ;; Pack then unpack, result doesn't leak skolem
  (test-ok-type "pack-then-unpack with safe return"
                (synth-empty
                 '(unpack ((a val) (pack Int 42 : (∃ ((t : Type)) t)))
                   #t))
                'Bool)
  
  ;; Existential type in function parameter position
  (test-ok "existential in function type"
           (synth-empty '(-> (∃ ((a : Type)) a) Int)))
  
  ;; --------------------------------------------------------
  ;; Summary
  ;; --------------------------------------------------------
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
