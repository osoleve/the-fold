;;; fabric/stitches/pipeline/tests/test-stage.ss — Tests for Stage Algebra
;;;
;;; Tests the core stage primitives and composition operators.

(load "fabric/stitches/pipeline/stage.ss")
(load "fabric/stitches/pipeline/context.ss")

;;; ============================================================
;;; Test Framework (minimal)
;;; ============================================================

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
      (display "SOME TESTS FAILED\n")))

;;; ============================================================
;;; Test Context
;;; ============================================================

(define test-ctx empty-context)

;;; ============================================================
;;; StageResult Tests
;;; ============================================================

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
                   
                   (test "stage-retry creates retry result"
                         'retry
                         (stage-result-tag (stage-retry "reason" 1000)))
                   
                   (test "stage-skip creates skip result"
                         'skip
                         (stage-result-tag (stage-skip "reason")))
                   
                   (test "stage-halt creates halt result"
                         'halt
                         (stage-result-tag (stage-halt "reason")))
                   
                   (test "stage-await creates await result"
                         'await
                         (stage-result-tag (stage-await 'ref)))))

;;; ============================================================
;;; StageResult Predicates
;;; ============================================================

(run-tests "StageResult Predicates"
           (lambda ()
                   (test-pred "stage-ok? true for ok"
                              stage-ok?
                              (stage-ok 1))
                   
                   (test "stage-ok? false for err"
                         #f
                         (stage-ok? (stage-err 'e "m" '())))
                   
                   (test-pred "stage-err? true for err"
                              stage-err?
                              (stage-err 'e "m" '()))
                   
                   (test "stage-err? false for ok"
                         #f
                         (stage-err? (stage-ok 1)))
                   
                   (test-pred "stage-retry? true for retry"
                              stage-retry?
                              (stage-retry "r" 0))
                   
                   (test-pred "stage-skip? true for skip"
                              stage-skip?
                              (stage-skip "r"))
                   
                   (test-pred "stage-halt? true for halt"
                              stage-halt?
                              (stage-halt "r"))
                   
                   (test-pred "stage-await? true for await"
                              stage-await?
                              (stage-await 'ref))))

;;; ============================================================
;;; Basic Stage Constructors
;;; ============================================================

(run-tests "Basic Stage Constructors"
           (lambda ()
                   (test "stage-pure returns constant"
                         42
                         (stage-result-value (run-stage (stage-pure 42) test-ctx "ignored")))
                   
                   (test "stage-read passes input through"
                         "hello"
                         (stage-result-value (run-stage stage-read test-ctx "hello")))
                   
                   (test "stage-ask returns context"
                         test-ctx
                         (stage-result-value (run-stage stage-ask test-ctx "input")))
                   
                   (test "stage-fail produces error"
                         'my-error
                         (stage-err-code (run-stage (stage-fail 'my-error "msg") test-ctx "input")))
                   
                   (test-pred "stage-halt-with halts pipeline"
                              stage-halt?
                              (run-stage (stage-halt-with "stopping") test-ctx "input"))
                   
                   (test-pred "stage-skip-with skips"
                              stage-skip?
                              (run-stage (stage-skip-with "skipping") test-ctx "input"))))

;;; ============================================================
;;; stage-arr Tests
;;; ============================================================

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
                          (run-stage (stage-arr (lambda (x) x)) test-ctx "same")))))

;;; ============================================================
;;; Sequential Composition (>>>)
;;; ============================================================

(run-tests "Sequential Composition (>>>)"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [add-one (stage-arr (lambda (x) (+ x 1)))])
                        
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
                               (run-stage (stage->>> stage-read stage-read) test-ctx 5))))
                   
                   (test ">>> propagates errors"
                         'error
                         (stage-result-tag
                          (run-stage (stage->>> (stage-fail 'e "m")
                                                (stage-arr (lambda (x) x)))
                                     test-ctx 1)))
                   
                   (test ">>> skips second on error"
                         'e
                         (stage-err-code
                          (run-stage (stage->>> (stage-fail 'e "m")
                                                (stage-arr (lambda (x) x)))
                                     test-ctx 1)))))

;;; ============================================================
;;; Parallel Composition (&&&)
;;; ============================================================

(run-tests "Parallel Composition (&&&)"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [triple (stage-arr (lambda (x) (* x 3)))])
                        
                        (test "&&& pairs results"
                              '(10 . 15)
                              (stage-result-value
                               (run-stage (stage-&&& double triple) test-ctx 5)))
                        
                        (test "&&& with pure"
                              '(42 . 5)
                              (stage-result-value
                               (run-stage (stage-&&& (stage-pure 42) stage-read) test-ctx 5))))
                   
                   (test "&&& fails if first fails"
                         'err
                         (stage-result-tag
                          (run-stage (stage-&&& (stage-fail 'e "m") stage-read) test-ctx 1)))
                   
                   (test "&&& fails if second fails"
                         'err
                         (stage-result-tag
                          (run-stage (stage-&&& stage-read (stage-fail 'e "m")) test-ctx 1)))))

;;; ============================================================
;;; Split Composition (***)
;;; ============================================================

(run-tests "Split Composition (***)"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [negate (stage-arr (lambda (x) (- x)))])
                        
                        (test "*** applies to pair components"
                              '(10 . -3)
                              (stage-result-value
                               (run-stage (stage-*** double negate) test-ctx '(5 . 3)))))))

;;; ============================================================
;;; Conditional Stages
;;; ============================================================

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
                               (run-stage (stage-unless positive? negate) test-ctx 5))))))

;;; ============================================================
;;; stage-guard Tests
;;; ============================================================

(run-tests "stage-guard"
           (lambda ()
                   (test "guard passes on true"
                         5
                         (stage-result-value
                          (run-stage (stage-guard positive? "must be positive") test-ctx 5)))
                   
                   (test "guard fails on false"
                         'guard-failed
                         (stage-err-code
                          (run-stage (stage-guard positive? "must be positive") test-ctx -5)))))

;;; ============================================================
;;; Monadic Interface
;;; ============================================================

(run-tests "Monadic Interface"
           (lambda ()
                   (test "stage-bind chains stages"
                         20
                         (stage-result-value
                          (run-stage
                           (stage-bind (stage-pure 5)
                                       (lambda (x) (stage-pure (* x 4))))
                           test-ctx "ignored")))
                   
                   (test "stage-map transforms result"
                         50
                         (stage-result-value
                          (run-stage
                           (stage-map (lambda (x) (* x 10)) (stage-pure 5))
                           test-ctx "ignored")))
                   
                   (test "stage-sequence collects results"
                         '(1 2 3)
                         (stage-result-value
                          (run-stage
                           (stage-sequence (list (stage-pure 1)
                                                 (stage-pure 2)
                                                 (stage-pure 3)))
                           test-ctx "ignored")))))

;;; ============================================================
;;; Error Handling
;;; ============================================================

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
                           test-ctx "input")))))

;;; ============================================================
;;; Utility Stages
;;; ============================================================

(run-tests "Utility Stages"
           (lambda ()
                   (test "stage-id passes through"
                         "hello"
                         (stage-result-value (run-stage stage-id test-ctx "hello")))
                   
                   (test "stage-dup duplicates"
                         '("x" . "x")
                         (stage-result-value (run-stage stage-dup test-ctx "x")))
                   
                   (test "stage-swap swaps pair"
                         '(2 . 1)
                         (stage-result-value (run-stage stage-swap test-ctx '(1 . 2))))
                   
                   (test "stage-fst extracts first"
                         1
                         (stage-result-value (run-stage stage-fst test-ctx '(1 . 2))))
                   
                   (test "stage-snd extracts second"
                         2
                         (stage-result-value (run-stage stage-snd test-ctx '(1 . 2))))))

;;; ============================================================
;;; Iteration
;;; ============================================================

(run-tests "Iteration"
           (lambda ()
                   (test "stage-repeat 0 is identity"
                         5
                         (stage-result-value
                          (run-stage (stage-repeat 0 (stage-arr (lambda (x) (* x 2))))
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
                                     test-ctx 1)))))

;;; ============================================================
;;; Pipeline Construction
;;; ============================================================

(run-tests "Pipeline Construction"
           (lambda ()
                   (let ([p (pipeline 'test-pipeline
                                      (stage-arr (lambda (x) (* x 2)))
                                      (stage-arr (lambda (x) (+ x 1)))
                                      (stage-arr (lambda (x) (* x 3))))])
                        
                        (test "pipeline chains stages"
                              33
                              (stage-result-value (run-stage p test-ctx 5))))))

;;; ============================================================
;;; ArrowChoice (Either routing)
;;; ============================================================

(run-tests "ArrowChoice"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [negate (stage-arr (lambda (x) (- x)))])
                        
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
                        
                        (test "stage-choice routes left"
                              10
                              (stage-result-value
                               (run-stage (stage-||| double negate) test-ctx (left 5))))
                        
                        (test "stage-choice routes right"
                              -5
                              (stage-result-value
                               (run-stage (stage-||| double negate) test-ctx (right 5)))))))

;;; ============================================================
;;; Context Operations
;;; ============================================================

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
                                test-ctx "input"))))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(test-summary)
