;;; fabric/stitches/pipeline/tests/test-stage.ss — Comprehensive Tests for Stage Algebra
;;;
;;; Tests the core stage primitives and composition operators.

(load "core/testing/test-framework.ss")
(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/context.ss")

;;; ====
;;; Test Context
;;; ====

(define test-ctx empty-context)

;;; ====
;;; StageResult Tests
;;; ====

(test-group "StageResult Constructors"
  (define-test "stage-ok creates ok result"
    (assert-equal 'ok (stage-result-tag (stage-ok 42))))

  (define-test "stage-ok value extraction"
    (assert-equal 42 (stage-result-value (stage-ok 42))))

  (define-test "stage-err creates err result"
    (assert-equal 'err (stage-result-tag (stage-err 'test-error "message" '()))))

  (define-test "stage-err code extraction"
    (assert-equal 'test-error (stage-err-code (stage-err 'test-error "message" '()))))

  (define-test "stage-err message extraction"
    (assert-equal "error message" (stage-err-message (stage-err 'code "error message" '()))))

  (define-test "stage-err data extraction"
    (assert-equal '(extra data) (stage-err-data (stage-err 'code "msg" '(extra data)))))

  (define-test "stage-retry creates retry result"
    (assert-equal 'retry (stage-result-tag (stage-retry "reason" 1000))))

  (define-test "stage-retry reason extraction"
    (assert-equal "retry reason" (stage-retry-reason (stage-retry "retry reason" 5000))))

  (define-test "stage-retry delay extraction"
    (assert-equal 5000 (stage-retry-delay (stage-retry "reason" 5000))))

  (define-test "stage-skip creates skip result"
    (assert-equal 'skip (stage-result-tag (stage-skip "reason"))))

  (define-test "stage-halt creates halt result"
    (assert-equal 'halt (stage-result-tag (stage-halt "reason"))))

  (define-test "stage-await creates await result"
    (assert-equal 'await (stage-result-tag (stage-await 'ref))))

  (define-test "stage-await ref extraction"
    (assert-equal 'signal-ref (stage-result-value (stage-await 'signal-ref)))))

;;; ====
;;; StageResult Predicates
;;; ====

(test-group "StageResult Predicates"
  (define-test "stage-ok? true for ok"
    (assert-true (stage-ok? (stage-ok 1))))

  (define-test "stage-ok? false for err"
    (assert-false (stage-ok? (stage-err 'e "m" '()))))

  (define-test "stage-ok? false for retry"
    (assert-false (stage-ok? (stage-retry "r" 0))))

  (define-test "stage-err? true for err"
    (assert-true (stage-err? (stage-err 'e "m" '()))))

  (define-test "stage-err? false for ok"
    (assert-false (stage-err? (stage-ok 1))))

  (define-test "stage-retry? true for retry"
    (assert-true (stage-retry? (stage-retry "r" 0))))

  (define-test "stage-retry? false for ok"
    (assert-false (stage-retry? (stage-ok 1))))

  (define-test "stage-skip? true for skip"
    (assert-true (stage-skip? (stage-skip "r"))))

  (define-test "stage-skip? false for ok"
    (assert-false (stage-skip? (stage-ok 1))))

  (define-test "stage-halt? true for halt"
    (assert-true (stage-halt? (stage-halt "r"))))

  (define-test "stage-halt? false for ok"
    (assert-false (stage-halt? (stage-ok 1))))

  (define-test "stage-await? true for await"
    (assert-true (stage-await? (stage-await 'ref))))

  (define-test "stage-await? false for ok"
    (assert-false (stage-await? (stage-ok 1))))

  (define-test "stage-result? true for any result"
    (assert-true (stage-result? (stage-ok 1))))

  (define-test "stage-result? false for non-result"
    (assert-false (stage-result? '(not a result)))))

;;; ====
;;; Stage Record Tests
;;; ====

(test-group "Stage Record"
  (define-test "make-stage creates stage"
    (let ([s (make-stage 'test-stage (lambda (ctx input) (stage-ok input)))])
      (assert-true (stage? s))))

  (define-test "stage-name extracts name"
    (let ([s (make-stage 'test-stage (lambda (ctx input) (stage-ok input)))])
      (assert-equal 'test-stage (stage-name s))))

  (define-test "stage? false for non-stage"
    (assert-false (stage? '(not a stage))))

  (define-test "run-stage executes stage function"
    (assert-equal 42
                  (stage-result-value
                   (run-stage (make-stage 'test (lambda (ctx input) (stage-ok (* input 2))))
                              test-ctx 21)))))

;;; ====
;;; Basic Stage Constructors
;;; ====

(test-group "Basic Stage Constructors"
  (define-test "stage-pure returns constant"
    (assert-equal 42 (stage-result-value (run-stage (stage-pure 42) test-ctx "ignored"))))

  (define-test "stage-pure ignores input"
    (assert-equal 'constant (stage-result-value (run-stage (stage-pure 'constant) test-ctx '(any input)))))

  (define-test "stage-read passes input through"
    (assert-equal "hello" (stage-result-value (run-stage stage-read test-ctx "hello"))))

  (define-test "stage-read with complex input"
    (assert-equal '((a . 1) (b . 2)) (stage-result-value (run-stage stage-read test-ctx '((a . 1) (b . 2))))))

  (define-test "stage-ask returns context"
    (assert-equal test-ctx (stage-result-value (run-stage stage-ask test-ctx "input"))))

  (define-test "stage-asks extracts from context"
    (assert-equal "default"
                  (stage-result-value
                   (run-stage (stage-asks ctx-session-id) test-ctx "input"))))

  (define-test "stage-fail produces error"
    (assert-equal 'my-error (stage-err-code (run-stage (stage-fail 'my-error "msg") test-ctx "input"))))

  (define-test "stage-fail error message"
    (assert-equal "failure message" (stage-err-message (run-stage (stage-fail 'code "failure message") test-ctx "input"))))

  (define-test "stage-halt-with halts pipeline"
    (assert-true (stage-halt? (run-stage (stage-halt-with "stopping") test-ctx "input"))))

  (define-test "stage-skip-with skips"
    (assert-true (stage-skip? (run-stage (stage-skip-with "skipping") test-ctx "input")))))

;;; ====
;;; stage-arr Tests
;;; ====

(test-group "stage-arr (lift function)"
  (define-test "stage-arr applies function"
    (assert-equal 10
                  (stage-result-value
                   (run-stage (stage-arr (lambda (x) (* x 2))) test-ctx 5))))

  (define-test "stage-arr with string"
    (assert-equal "HELLO"
                  (stage-result-value
                   (run-stage (stage-arr string-upcase) test-ctx "hello"))))

  (define-test "stage-arr identity"
    (assert-equal "same"
                  (stage-result-value
                   (run-stage (stage-arr (lambda (x) x)) test-ctx "same"))))

  (define-test "stage-arr with list"
    (assert-equal '(1 2 3 4)
                  (stage-result-value
                   (run-stage (stage-arr (lambda (x) (append x '(4)))) test-ctx '(1 2 3)))))

  (define-test "stage-arr-ctx uses context"
    (assert-equal 10000
                  (stage-result-value
                   (run-stage (stage-arr-ctx (lambda (ctx input) (ctx-fuel ctx)))
                              test-ctx "ignored")))))

;;; ====
;;; Sequential Composition (>>>)
;;; ====

(test-group "Sequential Composition (>>>)"
  (define double (stage-arr (lambda (x) (* x 2))))
  (define add-one (stage-arr (lambda (x) (+ x 1))))
  (define square (stage-arr (lambda (x) (* x x))))

  (define-test ">>> composes left to right"
    (assert-equal 11
                  (stage-result-value
                   (run-stage (stage->>> double add-one) test-ctx 5))))

  (define-test ">>> order matters"
    (assert-equal 12
                  (stage-result-value
                   (run-stage (stage->>> add-one double) test-ctx 5))))

  (define-test ">>> with stage-read"
    (assert-equal 5
                  (stage-result-value
                   (run-stage (stage->>> stage-read stage-read) test-ctx 5))))

  (define-test ">>> three stages chained"
    (assert-equal 100  ;; (4 + 1) * 2 = 10, 10^2 = 100
                  (stage-result-value
                   (run-stage (stage->>> (stage->>> add-one double) square) test-ctx 4))))

  (define-test "<<< composes right to left"
    (assert-equal 11
                  (stage-result-value
                   (run-stage (stage-<<< add-one double) test-ctx 5))))

  (define-test ">>> propagates errors"
    (assert-equal 'err
                  (stage-result-tag
                   (run-stage (stage->>> (stage-fail 'e "m")
                                         (stage-arr (lambda (x) x)))
                              test-ctx 1))))

  (define-test ">>> skips second on error"
    (assert-equal 'e
                  (stage-err-code
                   (run-stage (stage->>> (stage-fail 'e "m")
                                         (stage-arr (lambda (x) x)))
                              test-ctx 1))))

  (define-test ">>> propagates halt"
    (assert-equal 'halt
                  (stage-result-tag
                   (run-stage (stage->>> (stage-halt-with "stop")
                                         (stage-arr (lambda (x) x)))
                              test-ctx 1))))

  (define-test ">>> propagates skip"
    (assert-equal 'skip
                  (stage-result-tag
                   (run-stage (stage->>> (stage-skip-with "skip")
                                         (stage-arr (lambda (x) x)))
                              test-ctx 1)))))

;;; ====
;;; Parallel Composition (&&&)
;;; ====

(test-group "Parallel Composition (&&&)"
  (define double (stage-arr (lambda (x) (* x 2))))
  (define triple (stage-arr (lambda (x) (* x 3))))
  (define square (stage-arr (lambda (x) (* x x))))

  (define-test "&&& pairs results"
    (assert-equal '(10 . 15)
                  (stage-result-value
                   (run-stage (stage-&&& double triple) test-ctx 5))))

  (define-test "&&& with pure"
    (assert-equal '(42 . 5)
                  (stage-result-value
                   (run-stage (stage-&&& (stage-pure 42) stage-read) test-ctx 5))))

  (define-test "&&& with three using nested"
    (assert-equal '((10 . 15) . 25)
                  (stage-result-value
                   (run-stage (stage-&&& (stage-&&& double triple) square) test-ctx 5))))

  (define-test "&&& fails if first fails"
    (assert-equal 'err
                  (stage-result-tag
                   (run-stage (stage-&&& (stage-fail 'e "m") stage-read) test-ctx 1))))

  (define-test "&&& fails if second fails"
    (assert-equal 'err
                  (stage-result-tag
                   (run-stage (stage-&&& stage-read (stage-fail 'e "m")) test-ctx 1))))

  (define-test "&&& propagates halt from first"
    (assert-equal 'halt
                  (stage-result-tag
                   (run-stage (stage-&&& (stage-halt-with "stop") stage-read) test-ctx 1))))

  (define-test "&&& propagates halt from second"
    (assert-equal 'halt
                  (stage-result-tag
                   (run-stage (stage-&&& stage-read (stage-halt-with "stop")) test-ctx 1))))

  (define-test "&&& with skip in first, ok in second"
    (assert-equal '(() . 5)
                  (stage-result-value
                   (run-stage (stage-&&& (stage-skip-with "skip") stage-read) test-ctx 5))))

  (define-test "&&& with ok in first, skip in second"
    (assert-equal '(5 . ())
                  (stage-result-value
                   (run-stage (stage-&&& stage-read (stage-skip-with "skip")) test-ctx 5))))

  (define-test "&&& with both effects produces fanout-effect"
    (let ([effect-stage (make-effect-stage 'test-effect "payload")])
      (assert-true (fanout-effect?
                    (run-stage (stage-&&& effect-stage effect-stage) test-ctx 42)))))

  (define-test "&&& with effect and ok produces fanout-effect"
    (let ([effect-stage (make-effect-stage 'test-effect "payload")])
      (assert-true (fanout-effect?
                    (run-stage (stage-&&& effect-stage stage-read) test-ctx 42)))))

  (define-test "&&& with ok and effect produces fanout-effect"
    (let ([effect-stage (make-effect-stage 'test-effect "payload")])
      (assert-true (fanout-effect?
                    (run-stage (stage-&&& stage-read effect-stage) test-ctx 42)))))

  (define-test "fanout-effect-right extracts pure value"
    (let ([effect-stage (make-effect-stage 'test-effect "payload")])
      (let ([result (run-stage (stage-&&& effect-stage stage-read) test-ctx 42)])
        (assert-equal 42 (fanout-effect-right result))))))

;;; ====
;;; Split Composition (***)
;;; ====

(test-group "Split Composition (***)"
  (define double (stage-arr (lambda (x) (* x 2))))
  (define negate (stage-arr (lambda (x) (- x))))

  (define-test "*** applies to pair components"
    (assert-equal '(10 . -3)
                  (stage-result-value
                   (run-stage (stage-*** double negate) test-ctx '(5 . 3)))))

  (define-test "stage-first applies to first"
    (assert-equal '(10 . 3)
                  (stage-result-value
                   (run-stage (stage-first double) test-ctx '(5 . 3)))))

  (define-test "stage-second applies to second"
    (assert-equal '(5 . -3)
                  (stage-result-value
                   (run-stage (stage-second negate) test-ctx '(5 . 3)))))

  (define-test "*** propagates error from first"
    (assert-equal 'err
                  (stage-result-tag
                   (run-stage (stage-*** (stage-fail 'e "m") negate) test-ctx '(5 . 3)))))

  (define-test "*** propagates error from second"
    (assert-equal 'err
                  (stage-result-tag
                   (run-stage (stage-*** double (stage-fail 'e "m")) test-ctx '(5 . 3))))))

;;; ====
;;; Conditional Stages
;;; ====

(test-group "Conditional Stages"
  (define double (stage-arr (lambda (x) (* x 2))))
  (define negate (stage-arr (lambda (x) (- x))))

  (define-test "stage-if takes then branch"
    (assert-equal 10
                  (stage-result-value
                   (run-stage (stage-if positive? double negate) test-ctx 5))))

  (define-test "stage-if takes else branch"
    (assert-equal 5
                  (stage-result-value
                   (run-stage (stage-if positive? double negate) test-ctx -5))))

  (define-test "stage-if with zero"
    (assert-equal 0
                  (stage-result-value
                   (run-stage (stage-if positive? double negate) test-ctx 0))))

  (define-test "stage-when runs on true"
    (assert-equal 10
                  (stage-result-value
                   (run-stage (stage-when positive? double) test-ctx 5))))

  (define-test "stage-when passes through on false"
    (assert-equal -5
                  (stage-result-value
                   (run-stage (stage-when positive? double) test-ctx -5))))

  (define-test "stage-unless runs on false"
    (assert-equal 5
                  (stage-result-value
                   (run-stage (stage-unless positive? negate) test-ctx -5))))

  (define-test "stage-unless passes through on true"
    (assert-equal 5
                  (stage-result-value
                   (run-stage (stage-unless positive? negate) test-ctx 5))))

  (define-test "stage-case matches first predicate"
    (let ([classify (stage-case
                     (list (cons negative? (stage-pure 'negative))
                           (cons zero? (stage-pure 'zero)))
                     (stage-pure 'positive))])
      (assert-equal 'negative
                    (stage-result-value (run-stage classify test-ctx -5)))))

  (define-test "stage-case matches second predicate"
    (let ([classify (stage-case
                     (list (cons negative? (stage-pure 'negative))
                           (cons zero? (stage-pure 'zero)))
                     (stage-pure 'positive))])
      (assert-equal 'zero
                    (stage-result-value (run-stage classify test-ctx 0)))))

  (define-test "stage-case falls through to default"
    (let ([classify (stage-case
                     (list (cons negative? (stage-pure 'negative))
                           (cons zero? (stage-pure 'zero)))
                     (stage-pure 'positive))])
      (assert-equal 'positive
                    (stage-result-value (run-stage classify test-ctx 5))))))

;;; ====
;;; stage-guard Tests
;;; ====

(test-group "stage-guard"
  (define-test "guard passes on true"
    (assert-equal 5
                  (stage-result-value
                   (run-stage (stage-guard positive? "must be positive") test-ctx 5))))

  (define-test "guard fails on false"
    (assert-equal 'guard-failed
                  (stage-err-code
                   (run-stage (stage-guard positive? "must be positive") test-ctx -5))))

  (define-test "guard error message"
    (assert-equal "must be positive"
                  (stage-err-message
                   (run-stage (stage-guard positive? "must be positive") test-ctx -5))))

  (define-test "guard preserves input on success"
    (assert-equal '(a b c)
                  (stage-result-value
                   (run-stage (stage-guard pair? "must be pair") test-ctx '(a b c))))))

;;; ====
;;; Monadic Interface
;;; ====

(test-group "Monadic Interface"
  (define-test "stage-bind chains stages"
    (assert-equal 20
                  (stage-result-value
                   (run-stage
                    (stage-bind (stage-pure 5)
                                (lambda (x) (stage-pure (* x 4))))
                    test-ctx "ignored"))))

  (define-test "stage-bind propagates errors"
    (assert-equal 'err
                  (stage-result-tag
                   (run-stage
                    (stage-bind (stage-fail 'e "m")
                                (lambda (x) (stage-pure x)))
                    test-ctx "ignored"))))

  (define-test "stage-bind with input"
    (assert-equal "HELLO"
                  (stage-result-value
                   (run-stage
                    (stage-bind stage-read
                                (lambda (x) (stage-pure (string-upcase x))))
                    test-ctx "hello"))))

  (define-test "stage-map transforms result"
    (assert-equal 50
                  (stage-result-value
                   (run-stage
                    (stage-map (lambda (x) (* x 10)) (stage-pure 5))
                    test-ctx "ignored"))))

  (define-test "stage-map propagates errors"
    (assert-equal 'err
                  (stage-result-tag
                   (run-stage
                    (stage-map (lambda (x) (* x 10)) (stage-fail 'e "m"))
                    test-ctx "ignored"))))

  (define-test "stage-sequence collects results"
    (assert-equal '(1 2 3)
                  (stage-result-value
                   (run-stage
                    (stage-sequence (list (stage-pure 1)
                                          (stage-pure 2)
                                          (stage-pure 3)))
                    test-ctx "ignored"))))

  (define-test "stage-sequence empty list"
    (assert-equal '()
                  (stage-result-value
                   (run-stage (stage-sequence '()) test-ctx "ignored"))))

  (define-test "stage-sequence fails on first error"
    (assert-equal 'e1
                  (stage-err-code
                   (run-stage
                    (stage-sequence (list (stage-pure 1)
                                          (stage-fail 'e1 "m")
                                          (stage-pure 3)))
                    test-ctx "ignored"))))

  (define-test "stage-traverse maps and sequences"
    (assert-equal '(2 4 6)
                  (stage-result-value
                   (run-stage
                    (stage-traverse (lambda (x) (stage-pure (* x 2)))
                                    '(1 2 3))
                    test-ctx "ignored"))))

  (define-test "stage-ap applies function in stage"
    (assert-equal 15
                  (stage-result-value
                   (run-stage
                    (stage-ap (stage-pure (lambda (x) (* x 3)))
                              (stage-pure 5))
                    test-ctx "ignored")))))

;;; ====
;;; Error Handling
;;; ====

(test-group "Error Handling"
  (define-test "stage-catch handles errors"
    (assert-equal 'recovered
                  (stage-result-value
                   (run-stage
                    (stage-catch (lambda (err) (stage-pure 'recovered))
                                 (stage-fail 'e "m"))
                    test-ctx "input"))))

  (define-test "stage-catch passes through ok"
    (assert-equal 42
                  (stage-result-value
                   (run-stage
                    (stage-catch (lambda (err) (stage-pure 'recovered))
                                 (stage-pure 42))
                    test-ctx "input"))))

  (define-test "stage-catch can inspect error"
    (assert-equal 'my-error
                  (stage-result-value
                   (run-stage
                    (stage-catch (lambda (err)
                                  (stage-pure (stage-err-code err)))
                                 (stage-fail 'my-error "m"))
                    test-ctx "input"))))

  (define-test "stage-recover handles specific errors"
    (assert-equal 'handled
                  (stage-result-value
                   (run-stage
                    (stage-recover
                     (list (cons 'code1 (stage-pure 'handled)))
                     (stage-fail 'code1 "m"))
                    test-ctx "input"))))

  (define-test "stage-recover rethrows unhandled"
    (assert-equal 'code2
                  (stage-err-code
                   (run-stage
                    (stage-recover
                     (list (cons 'code1 (stage-pure 'handled)))
                     (stage-fail 'code2 "m"))
                    test-ctx "input"))))

  (define-test "stage-default provides fallback"
    (assert-equal 'default
                  (stage-result-value
                   (run-stage
                    (stage-default 'default (stage-fail 'e "m"))
                    test-ctx "input"))))

  (define-test "stage-default uses value on success"
    (assert-equal 42
                  (stage-result-value
                   (run-stage
                    (stage-default 'default (stage-pure 42))
                    test-ctx "input"))))

  (define-test "stage-optional wraps success in some"
    (assert-equal '(some 42)
                  (stage-result-value
                   (run-stage (stage-optional (stage-pure 42)) test-ctx "input"))))

  (define-test "stage-optional returns empty on error"
    (assert-equal '()
                  (stage-result-value
                   (run-stage (stage-optional (stage-fail 'e "m")) test-ctx "input")))))

;;; ====
;;; Utility Stages
;;; ====

(test-group "Utility Stages"
  (define-test "stage-id passes through"
    (assert-equal "hello"
                  (stage-result-value (run-stage stage-id test-ctx "hello"))))

  (define-test "stage-const is alias for stage-pure"
    (assert-equal 42
                  (stage-result-value (run-stage (stage-const 42) test-ctx "ignored"))))

  (define-test "stage-dup duplicates"
    (assert-equal '("x" . "x")
                  (stage-result-value (run-stage stage-dup test-ctx "x"))))

  (define-test "stage-dup with number"
    (assert-equal '(42 . 42)
                  (stage-result-value (run-stage stage-dup test-ctx 42))))

  (define-test "stage-swap swaps pair"
    (assert-equal '(2 . 1)
                  (stage-result-value (run-stage stage-swap test-ctx '(1 . 2)))))

  (define-test "stage-swap with different types"
    (assert-equal '("b" . "a")
                  (stage-result-value (run-stage stage-swap test-ctx '("a" . "b")))))

  (define-test "stage-fst extracts first"
    (assert-equal 1
                  (stage-result-value (run-stage stage-fst test-ctx '(1 . 2)))))

  (define-test "stage-snd extracts second"
    (assert-equal 2
                  (stage-result-value (run-stage stage-snd test-ctx '(1 . 2))))))

;;; ====
;;; Iteration
;;; ====

(test-group "Iteration"
  (define-test "stage-repeat 0 is identity"
    (assert-equal 5
                  (stage-result-value
                   (run-stage (stage-repeat 0 (stage-arr (lambda (x) (* x 2))))
                              test-ctx 5))))

  (define-test "stage-repeat 1 applies once"
    (assert-equal 10
                  (stage-result-value
                   (run-stage (stage-repeat 1 (stage-arr (lambda (x) (* x 2))))
                              test-ctx 5))))

  (define-test "stage-repeat 3 applies 3 times"
    (assert-equal 40
                  (stage-result-value
                   (run-stage (stage-repeat 3 (stage-arr (lambda (x) (* x 2))))
                              test-ctx 5))))

  (define-test "stage-while loops until false"
    (assert-equal 16
                  (stage-result-value
                   (run-stage (stage-while (lambda (x) (< x 10))
                                           (stage-arr (lambda (x) (* x 2))))
                              test-ctx 1))))

  (define-test "stage-while with already false predicate"
    (assert-equal 20
                  (stage-result-value
                   (run-stage (stage-while (lambda (x) (< x 10))
                                           (stage-arr (lambda (x) (* x 2))))
                              test-ctx 20))))

  (define-test "stage-fold sums list"
    (assert-equal 15
                  (stage-result-value
                   (run-stage (stage-fold (lambda (acc x)
                                           (stage-pure (+ acc x)))
                                          0
                                          '(1 2 3 4 5))
                              test-ctx "ignored"))))

  (define-test "stage-fold empty list"
    (assert-equal 0
                  (stage-result-value
                   (run-stage (stage-fold (lambda (acc x)
                                           (stage-pure (+ acc x)))
                                          0
                                          '())
                              test-ctx "ignored"))))

  (define-test "stage-for-each executes stages"
    (assert-equal '()
                  (stage-result-value
                   (run-stage (stage-for-each (lambda (x) (stage-pure x))
                                              '(1 2 3))
                              test-ctx "ignored")))))

;;; ====
;;; Pipeline Construction
;;; ====

(test-group "Pipeline Construction"
  (define-test "pipeline chains stages"
    (let ([p (pipeline 'test-pipeline
                       (stage-arr (lambda (x) (* x 2)))
                       (stage-arr (lambda (x) (+ x 1)))
                       (stage-arr (lambda (x) (* x 3))))])
      (assert-equal 33
                    (stage-result-value (run-stage p test-ctx 5)))))

  (define-test "pipeline is a stage"
    (let ([p (pipeline 'test-pipeline
                       (stage-arr (lambda (x) (* x 2)))
                       (stage-arr (lambda (x) (+ x 1)))
                       (stage-arr (lambda (x) (* x 3))))])
      (assert-true (stage? p))))

  (define-test "pipeline name"
    (let ([p (pipeline 'test-pipeline
                       (stage-arr (lambda (x) (* x 2)))
                       (stage-arr (lambda (x) (+ x 1)))
                       (stage-arr (lambda (x) (* x 3))))])
      (assert-equal 'test-pipeline
                    (stage-name p))))

  (define-test "empty pipeline passes through"
    (assert-equal 42
                  (stage-result-value (run-stage (pipeline 'empty) test-ctx 42)))))

;;; ====
;;; ArrowChoice (Either routing)
;;; ====

(test-group "ArrowChoice"
  (define double (stage-arr (lambda (x) (* x 2))))
  (define negate (stage-arr (lambda (x) (- x))))

  (define-test "left creates left value"
    (assert-equal (left 5) (left 5)))

  (define-test "right creates right value"
    (assert-equal (right 5) (right 5)))

  (define-test "left? true for left"
    (assert-true (left? (left 5))))

  (define-test "left? false for right"
    (assert-false (left? (right 5))))

  (define-test "right? true for right"
    (assert-true (right? (right 5))))

  (define-test "right? false for left"
    (assert-false (right? (left 5))))

  (define-test "from-left extracts value"
    (assert-equal 5 (from-left (left 5))))

  (define-test "from-right extracts value"
    (assert-equal 5 (from-right (right 5))))

  (define-test "stage-left applies to left values"
    (assert-equal (left 10)
                  (stage-result-value
                   (run-stage (stage-left double) test-ctx (left 5)))))

  (define-test "stage-left passes right through"
    (assert-equal (right 5)
                  (stage-result-value
                   (run-stage (stage-left double) test-ctx (right 5)))))

  (define-test "stage-right applies to right values"
    (assert-equal (right -5)
                  (stage-result-value
                   (run-stage (stage-right negate) test-ctx (right 5)))))

  (define-test "stage-right passes left through"
    (assert-equal (left 5)
                  (stage-result-value
                   (run-stage (stage-right negate) test-ctx (left 5)))))

  (define-test "stage-choice routes left"
    (assert-equal 10
                  (stage-result-value
                   (run-stage (stage-choice double negate) test-ctx (left 5)))))

  (define-test "stage-choice routes right"
    (assert-equal -5
                  (stage-result-value
                   (run-stage (stage-choice double negate) test-ctx (right 5)))))

  (define-test "stage-+++ applies to both directions"
    (assert-equal (left 10)
                  (stage-result-value
                   (run-stage (stage-+++ double negate) test-ctx (left 5)))))

  (define-test "stage-+++ applies right"
    (assert-equal (right -5)
                  (stage-result-value
                   (run-stage (stage-+++ double negate) test-ctx (right 5))))))

;;; ====
;;; Context Operations
;;; ====

(test-group "Context Operations"
  (define-test "stage-asks extracts from context"
    (let ([ctx-with-x (ctx-extend-env test-ctx '((x . 42)))])
      (assert-equal 42
                    (stage-result-value
                     (run-stage (stage-asks (lambda (c) (ctx-env-ref c 'x)))
                                ctx-with-x "input")))))

  (define-test "stage-local modifies context"
    (assert-equal 100
                  (stage-result-value
                   (run-stage
                    (stage-local
                     (lambda (c) (ctx-extend-env c '((y . 100))))
                     (stage-asks (lambda (c) (ctx-env-ref c 'y))))
                    test-ctx "input"))))

  (define-test "stage-local does not affect outer"
    (assert-equal #f
                  (stage-result-value
                   (run-stage
                    (stage->>>
                     (stage-local
                      (lambda (c) (ctx-extend-env c '((z . 999))))
                      stage-read)
                     (stage-asks (lambda (c) (ctx-env-ref c 'z))))
                    test-ctx 42)))))

;;; ====
;;; Effect Staging
;;; ====

(test-group "Effect Staging"
  (define-test "make-effect-stage creates stage"
    (let ([log-effect (make-effect-stage 'log '(info "test message"))])
      (assert-true (stage? log-effect))))

  (define-test "effect stage produces stage-effect"
    (let ([log-effect (make-effect-stage 'log '(info "test message"))])
      (assert-true (stage-effect? (run-stage log-effect test-ctx "input")))))

  (define-test "stage-effect-type extracts type"
    (let ([log-effect (make-effect-stage 'log '(info "test message"))])
      (assert-equal 'log
                    (stage-effect-type (run-stage log-effect test-ctx "input")))))

  (define-test "stage-effect-payload extracts payload"
    (let ([log-effect (make-effect-stage 'log '(info "test message"))])
      (assert-equal '(info "test message")
                    (stage-effect-payload (run-stage log-effect test-ctx "input")))))

  (define-test "stage-effect-input captures input"
    (let ([log-effect (make-effect-stage 'log '(info "test message"))])
      (assert-equal "captured input"
                    (stage-effect-input (run-stage log-effect test-ctx "captured input"))))))

;;; ====
;;; stage-tap Tests
;;; ====

(test-group "stage-tap"
  (define-test "stage-tap passes value through and executes side effect"
    (let ([side-effect-value #f])
      (let ([result (stage-result-value
                     (run-stage (stage-tap (lambda (x) (set! side-effect-value x)))
                                test-ctx 42))])
        (assert-equal 42 result)
        (assert-equal 42 side-effect-value))))

  (define-test "stage-trace creates log effect"
    (assert-true (stage-effect?
                  (run-stage (stage-trace "label") test-ctx "input")))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
