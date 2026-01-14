;;; fabric/stitches/pipeline/tests/test-stage.ss — Comprehensive Tests for Stage Algebra
;;;
;;; Tests the core stage primitives and composition operators.

(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/context.ss")

;;; ====
;;; Test Framework (minimal)
;;; ====

(define *test-count* 0)
(define *test-passed* 0)
(define *test-failed* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *test-passed* (+ *test-passed* 1))
       (display (format "  PASS: ~a\n" name)))
      (begin
       (set! *test-failed* (+ *test-failed* 1))
       (display (format "  FAIL: ~a\n    Expected: ~s\n    Actual:   ~s\n"
                        name expected actual)))))

(define (test-pred name pred actual)
  (set! *test-count* (+ *test-count* 1))
  (if (pred actual)
      (begin
       (set! *test-passed* (+ *test-passed* 1))
       (display (format "  PASS: ~a\n" name)))
      (begin
       (set! *test-failed* (+ *test-failed* 1))
       (display (format "  FAIL: ~a\n    Value did not satisfy predicate: ~s\n"
                        name actual)))))

(define (run-tests name thunk)
  (display (format "\n=== ~a ===\n" name))
  (thunk))

(define (test-summary)
  (display (format "\n=== Summary ===\n"))
  (display (format "Total: ~a  Passed: ~a  Failed: ~a\n"
                   *test-count* *test-passed* *test-failed*))
  (if (= *test-failed* 0)
      (display "All tests passed!\n")
      (begin
       (display "SOME TESTS FAILED\n")
       (exit 1))))

;;; ====
;;; Test Context
;;; ====

(define test-ctx empty-context)

;;; ====
;;; StageResult Tests
;;; ====

(run-tests "StageResult Constructors"
           (lambda ()
                   (test "stage-ok creates ok result"
                         'ok
                         (stage-result-tag (stage-ok 42)))
                   
                   (test "stage-ok value extraction"
                         42
                         (stage-result-value (stage-ok 42)))
                   
                   (test "stage-err creates err result"
                         'err
                         (stage-result-tag (stage-err 'test-error "message" '())))
                   
                   (test "stage-err code extraction"
                         'test-error
                         (stage-err-code (stage-err 'test-error "message" '())))
                   
                   (test "stage-err message extraction"
                         "error message"
                         (stage-err-message (stage-err 'code "error message" '())))
                   
                   (test "stage-err data extraction"
                         '(extra data)
                         (stage-err-data (stage-err 'code "msg" '(extra data))))
                   
                   (test "stage-retry creates retry result"
                         'retry
                         (stage-result-tag (stage-retry "reason" 1000)))
                   
                   (test "stage-retry reason extraction"
                         "retry reason"
                         (stage-retry-reason (stage-retry "retry reason" 5000)))
                   
                   (test "stage-retry delay extraction"
                         5000
                         (stage-retry-delay (stage-retry "reason" 5000)))
                   
                   (test "stage-skip creates skip result"
                         'skip
                         (stage-result-tag (stage-skip "reason")))
                   
                   (test "stage-halt creates halt result"
                         'halt
                         (stage-result-tag (stage-halt "reason")))
                   
                   (test "stage-await creates await result"
                         'await
                         (stage-result-tag (stage-await 'ref)))
                   
                   (test "stage-await ref extraction"
                         'signal-ref
                         (stage-result-value (stage-await 'signal-ref)))))

;;; ====
;;; StageResult Predicates
;;; ====

(run-tests "StageResult Predicates"
           (lambda ()
                   (test-pred "stage-ok? true for ok"
                              stage-ok?
                              (stage-ok 1))
                   
                   (test "stage-ok? false for err"
                         #f
                         (stage-ok? (stage-err 'e "m" '())))
                   
                   (test "stage-ok? false for retry"
                         #f
                         (stage-ok? (stage-retry "r" 0)))
                   
                   (test-pred "stage-err? true for err"
                              stage-err?
                              (stage-err 'e "m" '()))
                   
                   (test "stage-err? false for ok"
                         #f
                         (stage-err? (stage-ok 1)))
                   
                   (test-pred "stage-retry? true for retry"
                              stage-retry?
                              (stage-retry "r" 0))
                   
                   (test "stage-retry? false for ok"
                         #f
                         (stage-retry? (stage-ok 1)))
                   
                   (test-pred "stage-skip? true for skip"
                              stage-skip?
                              (stage-skip "r"))
                   
                   (test "stage-skip? false for ok"
                         #f
                         (stage-skip? (stage-ok 1)))
                   
                   (test-pred "stage-halt? true for halt"
                              stage-halt?
                              (stage-halt "r"))
                   
                   (test "stage-halt? false for ok"
                         #f
                         (stage-halt? (stage-ok 1)))
                   
                   (test-pred "stage-await? true for await"
                              stage-await?
                              (stage-await 'ref))
                   
                   (test "stage-await? false for ok"
                         #f
                         (stage-await? (stage-ok 1)))
                   
                   (test-pred "stage-result? true for any result"
                              stage-result?
                              (stage-ok 1))
                   
                   (test "stage-result? false for non-result"
                         #f
                         (stage-result? '(not a result)))))

;;; ====
;;; Stage Record Tests
;;; ====

(run-tests "Stage Record"
           (lambda ()
                   (let ([s (make-stage 'test-stage (lambda (ctx input) (stage-ok input)))])
                        (test-pred "make-stage creates stage"
                                   stage?
                                   s)
                        
                        (test "stage-name extracts name"
                              'test-stage
                              (stage-name s))
                        
                        (test "stage? false for non-stage"
                              #f
                              (stage? '(not a stage))))
                   
                   (test "run-stage executes stage function"
                         42
                         (stage-result-value
                          (run-stage (make-stage 'test (lambda (ctx input) (stage-ok (* input 2))))
                                     test-ctx 21)))))

;;; ====
;;; Basic Stage Constructors
;;; ====

(run-tests "Basic Stage Constructors"
           (lambda ()
                   (test "stage-pure returns constant"
                         42
                         (stage-result-value (run-stage (stage-pure 42) test-ctx "ignored")))
                   
                   (test "stage-pure ignores input"
                         'constant
                         (stage-result-value (run-stage (stage-pure 'constant) test-ctx '(any input))))
                   
                   (test "stage-read passes input through"
                         "hello"
                         (stage-result-value (run-stage stage-read test-ctx "hello")))
                   
                   (test "stage-read with complex input"
                         '((a . 1) (b . 2))
                         (stage-result-value (run-stage stage-read test-ctx '((a . 1) (b . 2)))))
                   
                   (test "stage-ask returns context"
                         test-ctx
                         (stage-result-value (run-stage stage-ask test-ctx "input")))
                   
                   (test "stage-asks extracts from context"
                         "default"
                         (stage-result-value
                          (run-stage (stage-asks ctx-session-id) test-ctx "input")))
                   
                   (test "stage-fail produces error"
                         'my-error
                         (stage-err-code (run-stage (stage-fail 'my-error "msg") test-ctx "input")))
                   
                   (test "stage-fail error message"
                         "failure message"
                         (stage-err-message (run-stage (stage-fail 'code "failure message") test-ctx "input")))
                   
                   (test-pred "stage-halt-with halts pipeline"
                              stage-halt?
                              (run-stage (stage-halt-with "stopping") test-ctx "input"))
                   
                   (test-pred "stage-skip-with skips"
                              stage-skip?
                              (run-stage (stage-skip-with "skipping") test-ctx "input"))))

;;; ====
;;; stage-arr Tests
;;; ====

(run-tests "stage-arr (lift function)"
           (lambda ()
                   (test "stage-arr applies function"
                         10
                         (stage-result-value
                          (run-stage (stage-arr (lambda (x) (* x 2))) test-ctx 5)))
                   
                   (test "stage-arr with string"
                         "HELLO"
                         (stage-result-value
                          (run-stage (stage-arr string-upcase) test-ctx "hello")))
                   
                   (test "stage-arr identity"
                         "same"
                         (stage-result-value
                          (run-stage (stage-arr (lambda (x) x)) test-ctx "same")))
                   
                   (test "stage-arr with list"
                         '(1 2 3 4)
                         (stage-result-value
                          (run-stage (stage-arr (lambda (x) (append x '(4)))) test-ctx '(1 2 3))))
                   
                   (test "stage-arr-ctx uses context"
                         10000
                         (stage-result-value
                          (run-stage (stage-arr-ctx (lambda (ctx input) (ctx-fuel ctx)))
                                     test-ctx "ignored")))))

;;; ====
;;; Sequential Composition (>>>)
;;; ====

(run-tests "Sequential Composition (>>>)"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [add-one (stage-arr (lambda (x) (+ x 1)))]
                         [square (stage-arr (lambda (x) (* x x)))])
                        
                        (test ">>> composes left to right"
                              11
                              (stage-result-value
                               (run-stage (stage->>> double add-one) test-ctx 5)))
                        
                        (test ">>> order matters"
                              12
                              (stage-result-value
                               (run-stage (stage->>> add-one double) test-ctx 5)))
                        
                        (test ">>> with stage-read"
                              5
                              (stage-result-value
                               (run-stage (stage->>> stage-read stage-read) test-ctx 5)))
                        
                        (test ">>> three stages chained"
                              100  ;; (4 + 1) * 2 = 10, 10^2 = 100
                              (stage-result-value
                               (run-stage (stage->>> (stage->>> add-one double) square) test-ctx 4)))
                        
                        (test "<<< composes right to left"
                              11
                              (stage-result-value
                               (run-stage (stage-<<< add-one double) test-ctx 5))))
                   
                   (test ">>> propagates errors"
                         'err
                         (stage-result-tag
                          (run-stage (stage->>> (stage-fail 'e "m")
                                                (stage-arr (lambda (x) x)))
                                     test-ctx 1)))
                   
                   (test ">>> skips second on error"
                         'e
                         (stage-err-code
                          (run-stage (stage->>> (stage-fail 'e "m")
                                                (stage-arr (lambda (x) x)))
                                     test-ctx 1)))
                   
                   (test ">>> propagates halt"
                         'halt
                         (stage-result-tag
                          (run-stage (stage->>> (stage-halt-with "stop")
                                                (stage-arr (lambda (x) x)))
                                     test-ctx 1)))
                   
                   (test ">>> propagates skip"
                         'skip
                         (stage-result-tag
                          (run-stage (stage->>> (stage-skip-with "skip")
                                                (stage-arr (lambda (x) x)))
                                     test-ctx 1)))))

;;; ====
;;; Parallel Composition (&&&)
;;; ====

(run-tests "Parallel Composition (&&&)"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [triple (stage-arr (lambda (x) (* x 3)))]
                         [square (stage-arr (lambda (x) (* x x)))])
                        
                        (test "&&& pairs results"
                              '(10 . 15)
                              (stage-result-value
                               (run-stage (stage-&&& double triple) test-ctx 5)))
                        
                        (test "&&& with pure"
                              '(42 . 5)
                              (stage-result-value
                               (run-stage (stage-&&& (stage-pure 42) stage-read) test-ctx 5)))
                        
                        (test "&&& with three using nested"
                              '((10 . 15) . 25)
                              (stage-result-value
                               (run-stage (stage-&&& (stage-&&& double triple) square) test-ctx 5))))
                   
                   (test "&&& fails if first fails"
                         'err
                         (stage-result-tag
                          (run-stage (stage-&&& (stage-fail 'e "m") stage-read) test-ctx 1)))
                   
                   (test "&&& fails if second fails"
                         'err
                         (stage-result-tag
                          (run-stage (stage-&&& stage-read (stage-fail 'e "m")) test-ctx 1)))
                   
                   (test "&&& propagates halt from first"
                         'halt
                         (stage-result-tag
                          (run-stage (stage-&&& (stage-halt-with "stop") stage-read) test-ctx 1)))
                   
                   (test "&&& propagates halt from second"
                         'halt
                         (stage-result-tag
                          (run-stage (stage-&&& stage-read (stage-halt-with "stop")) test-ctx 1)))
                   
                   ;; Test with skip
                   (test "&&& with skip in first, ok in second"
                         '(() . 5)
                         (stage-result-value
                          (run-stage (stage-&&& (stage-skip-with "skip") stage-read) test-ctx 5)))
                   
                   (test "&&& with ok in first, skip in second"
                         '(5 . ())
                         (stage-result-value
                          (run-stage (stage-&&& stage-read (stage-skip-with "skip")) test-ctx 5)))
                   
                   ;; Test effect handling in fanout
                   (let ([effect-stage (make-effect-stage 'test-effect "payload")])
                        (test "&&& with both effects produces fanout-effect"
                              #t
                              (fanout-effect?
                               (run-stage (stage-&&& effect-stage effect-stage) test-ctx 42)))
                        
                        (test "&&& with effect and ok produces fanout-effect"
                              #t
                              (fanout-effect?
                               (run-stage (stage-&&& effect-stage stage-read) test-ctx 42)))
                        
                        (test "&&& with ok and effect produces fanout-effect"
                              #t
                              (fanout-effect?
                               (run-stage (stage-&&& stage-read effect-stage) test-ctx 42)))
                        
                        ;; Test fanout-effect extractors
                        (let ([result (run-stage (stage-&&& effect-stage stage-read) test-ctx 42)])
                             (test "fanout-effect-right extracts pure value"
                                   42
                                   (fanout-effect-right result))))))

;;; ====
;;; Split Composition (***)
;;; ====

(run-tests "Split Composition (***)"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [negate (stage-arr (lambda (x) (- x)))]
                         [add-one (stage-arr (lambda (x) (+ x 1)))])
                        
                        (test "*** applies to pair components"
                              '(10 . -3)
                              (stage-result-value
                               (run-stage (stage-*** double negate) test-ctx '(5 . 3))))
                        
                        (test "stage-first applies to first"
                              '(10 . 3)
                              (stage-result-value
                               (run-stage (stage-first double) test-ctx '(5 . 3))))
                        
                        (test "stage-second applies to second"
                              '(5 . -3)
                              (stage-result-value
                               (run-stage (stage-second negate) test-ctx '(5 . 3))))
                        
                        (test "*** propagates error from first"
                              'err
                              (stage-result-tag
                               (run-stage (stage-*** (stage-fail 'e "m") negate) test-ctx '(5 . 3))))
                        
                        (test "*** propagates error from second"
                              'err
                              (stage-result-tag
                               (run-stage (stage-*** double (stage-fail 'e "m")) test-ctx '(5 . 3)))))))

;;; ====
;;; Conditional Stages
;;; ====

(run-tests "Conditional Stages"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [negate (stage-arr (lambda (x) (- x)))])
                        
                        (test "stage-if takes then branch"
                              10
                              (stage-result-value
                               (run-stage (stage-if positive? double negate) test-ctx 5)))
                        
                        (test "stage-if takes else branch"
                              5
                              (stage-result-value
                               (run-stage (stage-if positive? double negate) test-ctx -5)))
                        
                        (test "stage-if with zero"
                              0
                              (stage-result-value
                               (run-stage (stage-if positive? double negate) test-ctx 0)))
                        
                        (test "stage-when runs on true"
                              10
                              (stage-result-value
                               (run-stage (stage-when positive? double) test-ctx 5)))
                        
                        (test "stage-when passes through on false"
                              -5
                              (stage-result-value
                               (run-stage (stage-when positive? double) test-ctx -5)))
                        
                        (test "stage-unless runs on false"
                              5
                              (stage-result-value
                               (run-stage (stage-unless positive? negate) test-ctx -5)))
                        
                        (test "stage-unless passes through on true"
                              5
                              (stage-result-value
                               (run-stage (stage-unless positive? negate) test-ctx 5))))
                   
                   ;; stage-case tests
                   (let ([classify (stage-case
                                    (list (cons negative? (stage-pure 'negative))
                                          (cons zero? (stage-pure 'zero)))
                                    (stage-pure 'positive))])
                        (test "stage-case matches first predicate"
                              'negative
                              (stage-result-value (run-stage classify test-ctx -5)))
                        
                        (test "stage-case matches second predicate"
                              'zero
                              (stage-result-value (run-stage classify test-ctx 0)))
                        
                        (test "stage-case falls through to default"
                              'positive
                              (stage-result-value (run-stage classify test-ctx 5))))))

;;; ====
;;; stage-guard Tests
;;; ====

(run-tests "stage-guard"
           (lambda ()
                   (test "guard passes on true"
                         5
                         (stage-result-value
                          (run-stage (stage-guard positive? "must be positive") test-ctx 5)))
                   
                   (test "guard fails on false"
                         'guard-failed
                         (stage-err-code
                          (run-stage (stage-guard positive? "must be positive") test-ctx -5)))
                   
                   (test "guard error message"
                         "must be positive"
                         (stage-err-message
                          (run-stage (stage-guard positive? "must be positive") test-ctx -5)))
                   
                   (test "guard preserves input on success"
                         '(a b c)
                         (stage-result-value
                          (run-stage (stage-guard pair? "must be pair") test-ctx '(a b c))))))

;;; ====
;;; Monadic Interface
;;; ====

(run-tests "Monadic Interface"
           (lambda ()
                   (test "stage-bind chains stages"
                         20
                         (stage-result-value
                          (run-stage
                           (stage-bind (stage-pure 5)
                                       (lambda (x) (stage-pure (* x 4))))
                           test-ctx "ignored")))
                   
                   (test "stage-bind propagates errors"
                         'err
                         (stage-result-tag
                          (run-stage
                           (stage-bind (stage-fail 'e "m")
                                       (lambda (x) (stage-pure x)))
                           test-ctx "ignored")))
                   
                   (test "stage-bind with input"
                         "HELLO"
                         (stage-result-value
                          (run-stage
                           (stage-bind stage-read
                                       (lambda (x) (stage-pure (string-upcase x))))
                           test-ctx "hello")))
                   
                   (test "stage-map transforms result"
                         50
                         (stage-result-value
                          (run-stage
                           (stage-map (lambda (x) (* x 10)) (stage-pure 5))
                           test-ctx "ignored")))
                   
                   (test "stage-map propagates errors"
                         'err
                         (stage-result-tag
                          (run-stage
                           (stage-map (lambda (x) (* x 10)) (stage-fail 'e "m"))
                           test-ctx "ignored")))
                   
                   (test "stage-sequence collects results"
                         '(1 2 3)
                         (stage-result-value
                          (run-stage
                           (stage-sequence (list (stage-pure 1)
                                                 (stage-pure 2)
                                                 (stage-pure 3)))
                           test-ctx "ignored")))
                   
                   (test "stage-sequence empty list"
                         '()
                         (stage-result-value
                          (run-stage (stage-sequence '()) test-ctx "ignored")))
                   
                   (test "stage-sequence fails on first error"
                         'e1
                         (stage-err-code
                          (run-stage
                           (stage-sequence (list (stage-pure 1)
                                                 (stage-fail 'e1 "m")
                                                 (stage-pure 3)))
                           test-ctx "ignored")))
                   
                   (test "stage-traverse maps and sequences"
                         '(2 4 6)
                         (stage-result-value
                          (run-stage
                           (stage-traverse (lambda (x) (stage-pure (* x 2)))
                                           '(1 2 3))
                           test-ctx "ignored")))
                   
                   (test "stage-ap applies function in stage"
                         15
                         (stage-result-value
                          (run-stage
                           (stage-ap (stage-pure (lambda (x) (* x 3)))
                                     (stage-pure 5))
                           test-ctx "ignored")))))

;;; ====
;;; Error Handling
;;; ====

(run-tests "Error Handling"
           (lambda ()
                   (test "stage-catch handles errors"
                         'recovered
                         (stage-result-value
                          (run-stage
                           (stage-catch (lambda (err) (stage-pure 'recovered))
                                        (stage-fail 'e "m"))
                           test-ctx "input")))
                   
                   (test "stage-catch passes through ok"
                         42
                         (stage-result-value
                          (run-stage
                           (stage-catch (lambda (err) (stage-pure 'recovered))
                                        (stage-pure 42))
                           test-ctx "input")))
                   
                   (test "stage-catch can inspect error"
                         'my-error
                         (stage-result-value
                          (run-stage
                           (stage-catch (lambda (err)
                                                (stage-pure (stage-err-code err)))
                                        (stage-fail 'my-error "m"))
                           test-ctx "input")))
                   
                   (test "stage-recover handles specific errors"
                         'handled
                         (stage-result-value
                          (run-stage
                           (stage-recover
                            (list (cons 'code1 (stage-pure 'handled)))
                            (stage-fail 'code1 "m"))
                           test-ctx "input")))
                   
                   (test "stage-recover rethrows unhandled"
                         'code2
                         (stage-err-code
                          (run-stage
                           (stage-recover
                            (list (cons 'code1 (stage-pure 'handled)))
                            (stage-fail 'code2 "m"))
                           test-ctx "input")))
                   
                   (test "stage-default provides fallback"
                         'default
                         (stage-result-value
                          (run-stage
                           (stage-default 'default (stage-fail 'e "m"))
                           test-ctx "input")))
                   
                   (test "stage-default uses value on success"
                         42
                         (stage-result-value
                          (run-stage
                           (stage-default 'default (stage-pure 42))
                           test-ctx "input")))
                   
                   (test "stage-optional wraps success in some"
                         '(some 42)
                         (stage-result-value
                          (run-stage (stage-optional (stage-pure 42)) test-ctx "input")))
                   
                   (test "stage-optional returns empty on error"
                         '()
                         (stage-result-value
                          (run-stage (stage-optional (stage-fail 'e "m")) test-ctx "input")))))

;;; ====
;;; Utility Stages
;;; ====

(run-tests "Utility Stages"
           (lambda ()
                   (test "stage-id passes through"
                         "hello"
                         (stage-result-value (run-stage stage-id test-ctx "hello")))
                   
                   (test "stage-const is alias for stage-pure"
                         42
                         (stage-result-value (run-stage (stage-const 42) test-ctx "ignored")))
                   
                   (test "stage-dup duplicates"
                         '("x" . "x")
                         (stage-result-value (run-stage stage-dup test-ctx "x")))
                   
                   (test "stage-dup with number"
                         '(42 . 42)
                         (stage-result-value (run-stage stage-dup test-ctx 42)))
                   
                   (test "stage-swap swaps pair"
                         '(2 . 1)
                         (stage-result-value (run-stage stage-swap test-ctx '(1 . 2))))
                   
                   (test "stage-swap with different types"
                         '("b" . "a")
                         (stage-result-value (run-stage stage-swap test-ctx '("a" . "b"))))
                   
                   (test "stage-fst extracts first"
                         1
                         (stage-result-value (run-stage stage-fst test-ctx '(1 . 2))))
                   
                   (test "stage-snd extracts second"
                         2
                         (stage-result-value (run-stage stage-snd test-ctx '(1 . 2))))))

;;; ====
;;; Iteration
;;; ====

(run-tests "Iteration"
           (lambda ()
                   (test "stage-repeat 0 is identity"
                         5
                         (stage-result-value
                          (run-stage (stage-repeat 0 (stage-arr (lambda (x) (* x 2))))
                                     test-ctx 5)))
                   
                   (test "stage-repeat 1 applies once"
                         10
                         (stage-result-value
                          (run-stage (stage-repeat 1 (stage-arr (lambda (x) (* x 2))))
                                     test-ctx 5)))
                   
                   (test "stage-repeat 3 applies 3 times"
                         40
                         (stage-result-value
                          (run-stage (stage-repeat 3 (stage-arr (lambda (x) (* x 2))))
                                     test-ctx 5)))
                   
                   (test "stage-while loops until false"
                         16
                         (stage-result-value
                          (run-stage (stage-while (lambda (x) (< x 10))
                                                  (stage-arr (lambda (x) (* x 2))))
                                     test-ctx 1)))
                   
                   (test "stage-while with already false predicate"
                         20
                         (stage-result-value
                          (run-stage (stage-while (lambda (x) (< x 10))
                                                  (stage-arr (lambda (x) (* x 2))))
                                     test-ctx 20)))
                   
                   ;; stage-fold tests
                   (test "stage-fold sums list"
                         15
                         (stage-result-value
                          (run-stage (stage-fold (lambda (acc x)
                                                         (stage-pure (+ acc x)))
                                                 0
                                                 '(1 2 3 4 5))
                                     test-ctx "ignored")))
                   
                   (test "stage-fold empty list"
                         0
                         (stage-result-value
                          (run-stage (stage-fold (lambda (acc x)
                                                         (stage-pure (+ acc x)))
                                                 0
                                                 '())
                                     test-ctx "ignored")))
                   
                   (test "stage-for-each executes stages"
                         '()
                         (stage-result-value
                          (run-stage (stage-for-each (lambda (x) (stage-pure x))
                                                     '(1 2 3))
                                     test-ctx "ignored")))))

;;; ====
;;; Pipeline Construction
;;; ====

(run-tests "Pipeline Construction"
           (lambda ()
                   (let ([p (pipeline 'test-pipeline
                                      (stage-arr (lambda (x) (* x 2)))
                                      (stage-arr (lambda (x) (+ x 1)))
                                      (stage-arr (lambda (x) (* x 3))))])
                        
                        (test "pipeline chains stages"
                              33
                              (stage-result-value (run-stage p test-ctx 5)))
                        
                        (test-pred "pipeline is a stage"
                                   stage?
                                   p)
                        
                        (test "pipeline name"
                              'test-pipeline
                              (stage-name p)))
                   
                   (test "empty pipeline passes through"
                         42
                         (stage-result-value (run-stage (pipeline 'empty) test-ctx 42)))))

;;; ====
;;; ArrowChoice (Either routing)
;;; ====

(run-tests "ArrowChoice"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [negate (stage-arr (lambda (x) (- x)))])
                        
                        (test "left creates left value"
                              (left 5)
                              (left 5))
                        
                        (test "right creates right value"
                              (right 5)
                              (right 5))
                        
                        (test-pred "left? true for left"
                                   left?
                                   (left 5))
                        
                        (test "left? false for right"
                              #f
                              (left? (right 5)))
                        
                        (test-pred "right? true for right"
                                   right?
                                   (right 5))
                        
                        (test "right? false for left"
                              #f
                              (right? (left 5)))
                        
                        (test "from-left extracts value"
                              5
                              (from-left (left 5)))
                        
                        (test "from-right extracts value"
                              5
                              (from-right (right 5)))
                        
                        (test "stage-left applies to left values"
                              (left 10)
                              (stage-result-value
                               (run-stage (stage-left double) test-ctx (left 5))))
                        
                        (test "stage-left passes right through"
                              (right 5)
                              (stage-result-value
                               (run-stage (stage-left double) test-ctx (right 5))))
                        
                        (test "stage-right applies to right values"
                              (right -5)
                              (stage-result-value
                               (run-stage (stage-right negate) test-ctx (right 5))))
                        
                        (test "stage-right passes left through"
                              (left 5)
                              (stage-result-value
                               (run-stage (stage-right negate) test-ctx (left 5))))
                        
                        (test "stage-choice routes left"
                              10
                              (stage-result-value
                               (run-stage (stage-choice double negate) test-ctx (left 5))))
                        
                        (test "stage-choice routes right"
                              -5
                              (stage-result-value
                               (run-stage (stage-choice double negate) test-ctx (right 5))))
                        
                        (test "stage-+++ applies to both directions"
                              (left 10)
                              (stage-result-value
                               (run-stage (stage-+++ double negate) test-ctx (left 5))))
                        
                        (test "stage-+++ applies right"
                              (right -5)
                              (stage-result-value
                               (run-stage (stage-+++ double negate) test-ctx (right 5)))))))

;;; ====
;;; Context Operations
;;; ====

(run-tests "Context Operations"
           (lambda ()
                   (let ([ctx-with-x (ctx-extend-env test-ctx '((x . 42)))])
                        
                        (test "stage-asks extracts from context"
                              42
                              (stage-result-value
                               (run-stage (stage-asks (lambda (c) (ctx-env-ref c 'x)))
                                          ctx-with-x "input")))
                        
                        (test "stage-local modifies context"
                              100
                              (stage-result-value
                               (run-stage
                                (stage-local
                                 (lambda (c) (ctx-extend-env c '((y . 100))))
                                 (stage-asks (lambda (c) (ctx-env-ref c 'y))))
                                test-ctx "input")))
                        
                        (test "stage-local does not affect outer"
                              #f
                              (stage-result-value
                               (run-stage
                                (stage->>>
                                 (stage-local
                                  (lambda (c) (ctx-extend-env c '((z . 999))))
                                  stage-read)
                                 (stage-asks (lambda (c) (ctx-env-ref c 'z))))
                                test-ctx 42))))))

;;; ====
;;; Effect Staging
;;; ====

(run-tests "Effect Staging"
           (lambda ()
                   (let ([log-effect (make-effect-stage 'log '(info "test message"))])
                        (test-pred "make-effect-stage creates stage"
                                   stage?
                                   log-effect)
                        
                        (test-pred "effect stage produces stage-effect"
                                   stage-effect?
                                   (run-stage log-effect test-ctx "input"))
                        
                        (test "stage-effect-type extracts type"
                              'log
                              (stage-effect-type (run-stage log-effect test-ctx "input")))
                        
                        (test "stage-effect-payload extracts payload"
                              '(info "test message")
                              (stage-effect-payload (run-stage log-effect test-ctx "input")))
                        
                        (test "stage-effect-input captures input"
                              "captured input"
                              (stage-effect-input (run-stage log-effect test-ctx "captured input"))))))

;;; ====
;;; stage-tap Tests
;;; ====

(run-tests "stage-tap"
           (lambda ()
                   (let ([side-effect-value #f])
                        (test "stage-tap passes value through"
                              42
                              (stage-result-value
                               (run-stage (stage-tap (lambda (x) (set! side-effect-value x)))
                                          test-ctx 42)))
                        
                        (test "stage-tap executes side effect"
                              42
                              side-effect-value))
                   
                   (test "stage-trace creates log effect"
                         #t
                         (stage-effect?
                          (run-stage (stage-trace "label") test-ctx "input")))))

;;; ====
;;; Run All Tests
;;; ====

(test-summary)
