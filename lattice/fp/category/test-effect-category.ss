;;; Test suite for effect-category.ss
;;; Verifies the categorical foundations of algebraic effects

(load "core/testing/test-framework.ss")
(load "lattice/fp/category/effect-category.ss")

(test-group "effect-category"

  ;;; ====
  ;;; Effect Signatures as Algebraic Signatures
  ;;; ====

  (define-test "effect-sig-to-signature creates valid signature"
    (let ([sig (effect-sig-to-signature sig-State)])
      (assert-true (signature? sig))
      (assert-equal 'State (signature-name sig))
      ;; Should have pure + get + put + modify = 4 operations
      (assert-true (>= (length (signature-operations sig)) 4))))

  (define-test "converted signatures have no laws (effects are free)"
    (let ([sig (effect-sig-to-signature sig-State)])
      (assert-equal '() (signature-laws sig))))

  (define-test "sig-effect-state is a valid signature"
    (assert-true (signature? sig-effect-state))
    ;; Has pure operation
    (assert-true (signature-has-op? sig-effect-state 'pure)))

  (define-test "sig-effect-reader is a valid signature"
    (assert-true (signature? sig-effect-reader)))

  (define-test "sig-effect-writer is a valid signature"
    (assert-true (signature? sig-effect-writer)))

  (define-test "sig-effect-exception is a valid signature"
    (assert-true (signature? sig-effect-exception)))

  (define-test "sig-effect-nondet is a valid signature"
    (assert-true (signature? sig-effect-nondet)))

  ;;; ====
  ;;; Handlers as Algebras
  ;;; ====

  (define-test "handler-to-algebra creates valid algebra"
    (let* ([h (deep-handler identity '())]
           [sig (make-signature 'test '((pure . 1)) '())]
           [alg (handler-to-algebra h sig)])
      (assert-true (algebra? alg))
      (assert-equal 'handler-result (algebra-carrier alg))))

  (define-test "handler-to-algebra preserves return case"
    (let* ([h (deep-handler (lambda (x) (* x 2)) '())]
           [sig (make-signature 'test '((pure . 1)) '())]
           [alg (handler-to-algebra h sig)]
           [pure-op (algebra-op alg 'pure)])
      (assert-equal 10 (pure-op 5))))

  (define-test "algebra-to-handler roundtrips handler"
    (let* ([h (deep-handler (lambda (x) (* x 3)) '())]
           [sig (make-signature 'test '((pure . 1)) '())]
           [alg (handler-to-algebra h sig)]
           [h2 (algebra-to-handler alg)]
           [result (handle h2 (eff-return 7))])
      (assert-equal 21 result)))

  (define-test "algebra-to-handler creates working handler"
    (let* ([alg (make-algebra
                  (make-signature 'double '((pure . 1)) '())
                  'number
                  `((pure . ,(lambda (x) (* x 2)))))]
           [h (algebra-to-handler alg)]
           [result (handle h (eff-return 5))])
      (assert-equal 10 result)))

  ;;; ====
  ;;; Counit Evaluation (Critical: tests actual effect evaluation)
  ;;; ====

  (define-test "evaluate-counit evaluates pure effects"
    (let* ([alg (make-algebra
                  (make-signature 'id '((pure . 1)) '())
                  'any
                  `((pure . ,identity)))]
           [eff (eff-return 42)]
           [result (evaluate-counit adj-effect-state alg eff)])
      (assert-equal 42 result)))

  (define-test "evaluate-counit evaluates with transformation"
    (let* ([alg (make-algebra
                  (make-signature 'triple '((pure . 1)) '())
                  'number
                  `((pure . ,(lambda (x) (* x 3)))))]
           [eff (eff-return 10)]
           [result (evaluate-with-algebra alg eff)])
      (assert-equal 30 result)))

  ;;; ====
  ;;; Effect Adjunctions
  ;;; ====

  (define-test "make-effect-adjunction creates valid adjunction"
    (let ([adj (make-effect-adjunction sig-effect-state)])
      (assert-true (adjunction? adj))))

  (define-test "adj-effect-state is a valid adjunction"
    (assert-true (adjunction? adj-effect-state)))

  (define-test "adj-effect-reader is a valid adjunction"
    (assert-true (adjunction? adj-effect-reader)))

  (define-test "adj-effect-writer is a valid adjunction"
    (assert-true (adjunction? adj-effect-writer)))

  (define-test "adj-effect-exception is a valid adjunction"
    (assert-true (adjunction? adj-effect-exception)))

  (define-test "adj-effect-nondet is a valid adjunction"
    (assert-true (adjunction? adj-effect-nondet)))

  (define-test "effect adjunction has correct name"
    (assert-true (symbol? (adjunction-name adj-effect-state))))

  (define-test "effect adjunction unit is eff-return"
    (let* ([adj adj-effect-state]
           [η (adjunction-unit adj)]
           [η-comp (nat-transform-component η)])
      ;; η(x) should produce a pure effectful value
      (assert-true (eff-pure? (η-comp 42)))
      (assert-equal 42 (eff-pure-value (η-comp 42)))))

  ;;; ====
  ;;; Handler Composition
  ;;; ====

  (define-test "compose-effect-adjunctions creates adjunction"
    (let ([composed (compose-effect-adjunctions adj-effect-state adj-effect-reader)])
      (assert-true (adjunction? composed))))

  (define-test "handlers-commute? detects commutative handlers"
    ;; Two trivial handlers that just pass through should commute
    (let ([h1 (deep-handler identity '())]
          [h2 (deep-handler identity '())]
          [eff (eff-return 42)])
      (assert-true (handlers-commute? h1 h2 eff equal?))))

  ;;; ====
  ;;; Effect Rows as Functors
  ;;; ====

  (define-test "make-row-functor creates valid functor"
    (let* ([row (make-effect-row '(State Reader))]
           [F (make-row-functor row)])
      (assert-true (functor? F))))

  (define-test "row-extension-morphism creates nat transform"
    (let* ([row (make-effect-row '(Reader))]
           [ext (row-extension-morphism 'State row)])
      (assert-true (nat-transform? ext))))

  ;;; ====
  ;;; Continuations as Kan Extensions
  ;;; ====

  (define-test "continuation-as-kan creates kan extension"
    (let* ([k (lambda (resp) (eff-return (* resp 2)))]
           [kan (continuation-as-kan k)])
      (assert-true (kan-extension? kan))))

  (define-test "kan-resume applies continuation"
    (let* ([k (lambda (resp) (eff-return (* resp 2)))]
           [kan (continuation-as-kan k)]
           [result (kan-resume kan 21)])
      (assert-true (eff-pure? result))
      (assert-equal 42 (eff-pure-value result))))

  (define-test "kan-factor gives correct factorization"
    (let* ([k (lambda (resp) (eff-return resp))]
           [kan (continuation-as-kan k)]
           [factored (kan-factor kan (lambda (x) (eff-return (+ x 1))))]
           [result (factored 10)])
      (assert-true (eff-pure? result))
      (assert-equal 11 (eff-pure-value result))))

  ;;; ====
  ;;; Verification Utilities
  ;;; ====

  (define-test "verify-effect-unit passes for identity handler"
    (let ([h (deep-handler identity '())])
      (assert-true (verify-effect-unit h 42))))

  (define-test "verify-effect-unit passes with transformation"
    (let ([h (deep-handler (lambda (x) (* x 2)) '())])
      ;; handle h (return x) should equal (handler-return h) x
      ;; handle h (return 5) = 10, and (lambda (x) (* x 2)) 5 = 10
      (assert-true (verify-effect-unit h 5))))

  ;;; ====
  ;;; Integration: Full Effect Handling Path
  ;;; ====

  (define-test "effect handling follows adjunction structure"
    ;; Create a simple state computation
    (let* ([eff (eff-bind state-get
                         (lambda (s)
                           (eff-bind (state-put (+ s 10))
                                     (lambda (_)
                                       (eff-return (* s 2))))))]
           ;; Run with initial state 5
           [result (run-state 5 eff)])
      ;; Value should be 5 * 2 = 10
      (assert-equal 10 (car result))
      ;; Final state should be 5 + 10 = 15
      (assert-equal 15 (cdr result))))

  (define-test "nested handlers compose correctly"
    ;; State inside Writer
    (let* ([eff (eff-bind (writer-tell "start")
                         (lambda (_)
                           (eff-bind state-get
                                     (lambda (s)
                                       (eff-bind (state-put (+ s 1))
                                                 (lambda (_)
                                                   (eff-bind (writer-tell "done")
                                                             (lambda (_)
                                                               (eff-return s)))))))))]
           ;; Handle state first (inner), then writer (outer)
           [result (run-state-writer 0 eff)])
      ;; result is ((value . final-state) . log)
      (let ([inner-result (car result)]
            [log (cdr result)])
        ;; Value should be initial state (0)
        (assert-equal 0 (car inner-result))
        ;; Final state should be 1
        (assert-equal 1 (cdr inner-result))
        ;; Log should have 2 entries
        (assert-equal 2 (length log)))))

  (define-test "effect rows support polymorphic handlers"
    (let* ([row1 (make-effect-row '(State))]
           [row2 (row-add row1 'Writer)])
      ;; Extended row contains both effects
      (assert-true (row-contains? row2 'State))
      (assert-true (row-contains? row2 'Writer))
      ;; Original row unchanged
      (assert-true (row-contains? row1 'State))
      (assert-false (row-contains? row1 'Writer))))

) ; end test-group

(run-all-tests)
