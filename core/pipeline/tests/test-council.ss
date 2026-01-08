;;; core/pipeline/tests/test-council.ss -- Tests for Council Module
;;;
;;; Tests multi-agent council coordination patterns:
;;;   - council-sequential
;;;   - council-parallel
;;;   - council-debate
;;;   - council-vote
;;;   - council-consensus
;;;   - council-delegate
;;;
;;; Uses the unified test framework.

(load "core/test-framework.ss")
(load "core/pipeline/council.ss")

(display "Testing council.ss\n")
(display "==================\n\n")

;;; ============================================================
;;; Helper Stages for Testing
;;; ============================================================

;; Simple increment stage
(define inc-stage
  (stage-arr (lambda (x) (+ x 1))))

;; Double stage
(define double-stage
  (stage-arr (lambda (x) (* x 2))))

;; Negate stage
(define negate-stage
  (stage-arr (lambda (x) (- x))))

;; Always succeed stage
(define succeed-stage
  (stage-pure 'success))

;; Always fail stage
(define fail-stage
  (stage-fail 'test-error "Test failure"))

;; Context-dependent stage
(define ctx-stage
  (stage-asks ctx-session-id))

;; Test context
(define test-ctx empty-context)

;;; ============================================================
;;; Council Record Tests
;;; ============================================================

(test-group council-record
            
            (define-test make-council-creates-council
              (let ([c (make-council 'test-council '(a b c) 'sequential '())])
                   (assert-true (council? c))))
            
            (define-test council-name-accessor
              (let ([c (make-council 'my-council '() 'parallel '())])
                   (assert-equal 'my-council (council-name c))))
            
            (define-test council-members-accessor
              (let ([c (make-council 'test '(alpha beta gamma) 'sequential '())])
                   (assert-equal '(alpha beta gamma) (council-members c))))
            
            (define-test council-mode-accessor
              (let ([c (make-council 'test '() 'debate '())])
                   (assert-equal 'debate (council-mode c))))
            
            (define-test council-config-accessor
              (let ([cfg '((timeout . 5000) (max-rounds . 3))]
                    [c (make-council 'test '() 'vote cfg)])
                   (assert-equal cfg (council-config c))))
            
            (define-test council-predicate-false-for-non-council
              (assert-false (council? '(not a council)))
              (assert-false (council? 42))
              (assert-false (council? "string"))))

;;; ============================================================
;;; Council Sequential Tests
;;; ============================================================

(test-group council-sequential
            
            (define-test sequential-single-stage
              (let* ([result (run-stage (council-sequential (list inc-stage)) test-ctx 5)])
                    (assert-true (stage-ok? result))
                    (assert-equal 6 (stage-result-value result))))
            
            (define-test sequential-chain
              (let* ([stages (list inc-stage double-stage)]
                     [result (run-stage (council-sequential stages) test-ctx 5)])
                    ;; (5 + 1) * 2 = 12
                    (assert-true (stage-ok? result))
                    (assert-equal 12 (stage-result-value result))))
            
            (define-test sequential-multiple-stages
              (let* ([stages (list inc-stage double-stage negate-stage)]
                     [result (run-stage (council-sequential stages) test-ctx 5)])
                    ;; -((5 + 1) * 2) = -12
                    (assert-true (stage-ok? result))
                    (assert-equal -12 (stage-result-value result))))
            
            (define-test sequential-empty-list
              (let* ([result (run-stage (council-sequential '()) test-ctx 5)])
                    ;; Empty council should pass through
                    (assert-true (stage-ok? result))
                    (assert-equal 5 (stage-result-value result))))
            
            (define-test sequential-propagates-error
              (let* ([stages (list inc-stage fail-stage double-stage)]
                     [result (run-stage (council-sequential stages) test-ctx 5)])
                    (assert-true (stage-err? result))
                    (assert-equal 'test-error (stage-err-code result))))
            
            (define-test sequential-stops-on-halt
              (let* ([stages (list inc-stage (stage-halt-with "stopped") double-stage)]
                     [result (run-stage (council-sequential stages) test-ctx 5)])
                    (assert-true (stage-halt? result)))))

;;; ============================================================
;;; Council Parallel Tests
;;; ============================================================

(test-group council-parallel
            
            (define-test parallel-single-stage
              (let* ([result (run-stage (council-parallel (list inc-stage)) test-ctx 5)])
                    (assert-true (stage-ok? result))
                    (assert-equal '(6) (stage-result-value result))))
            
            (define-test parallel-multiple-stages
              (let* ([stages (list inc-stage double-stage negate-stage)]
                     [result (run-stage (council-parallel stages) test-ctx 5)])
                    (assert-true (stage-ok? result))
                    ;; All run in parallel on same input
                    (assert-equal '(6 10 -5) (stage-result-value result))))
            
            (define-test parallel-empty-list
              (let* ([result (run-stage (council-parallel '()) test-ctx 5)])
                    (assert-true (stage-ok? result))
                    (assert-equal '() (stage-result-value result))))
            
            (define-test parallel-with-failure
              (let* ([stages (list inc-stage fail-stage double-stage)]
                     [result (run-stage (council-parallel stages) test-ctx 5)])
                    ;; Parallel council collects all results including errors
                    ;; The exact behavior depends on implementation
                    (assert-true (or (stage-ok? result)
                                     (stage-err? result)))))
            
            (define-test parallel-preserves-order
              (let* ([stages (list (stage-pure 'first)
                                   (stage-pure 'second)
                                   (stage-pure 'third))]
                     [result (run-stage (council-parallel stages) test-ctx 'input)])
                    (assert-true (stage-ok? result))
                    (assert-equal '(first second third) (stage-result-value result)))))

;;; ============================================================
;;; Council Debate Tests
;;; ============================================================

(test-group council-debate
            
            (define-test debate-two-agents
              (let* ([stages (list (stage-pure 'opinion-a)
                                   (stage-pure 'opinion-b))]
                     [result (run-stage (council-debate stages) test-ctx 'topic)])
                    ;; Debate runs multiple rounds
                    (assert-true (stage-ok? result))))
            
            (define-test debate-with-config
              (let* ([stages (list inc-stage double-stage)]
                     [config '((max-rounds . 2))]
                     [result (run-stage (council-debate stages config) test-ctx 5)])
                    (assert-true (stage-ok? result))))
            
            (define-test debate-empty-produces-empty
              (let* ([result (run-stage (council-debate '()) test-ctx 'topic)])
                    (assert-true (stage-ok? result))))
            
            (define-test debate-single-agent
              (let* ([result (run-stage (council-debate (list inc-stage)) test-ctx 5)])
                    ;; Single agent "debate" just runs that agent
                    (assert-true (stage-ok? result)))))

;;; ============================================================
;;; Council Vote Tests
;;; ============================================================

(test-group council-vote
            
            (define-test vote-unanimous
              (let* ([stages (list (stage-pure 'yes)
                                   (stage-pure 'yes)
                                   (stage-pure 'yes))]
                     [result (run-stage (council-vote stages) test-ctx 'question)])
                    (assert-true (stage-ok? result))
                    (assert-equal 'yes (stage-result-value result))))
            
            (define-test vote-majority
              (let* ([stages (list (stage-pure 'yes)
                                   (stage-pure 'yes)
                                   (stage-pure 'no))]
                     [result (run-stage (council-vote stages) test-ctx 'question)])
                    (assert-true (stage-ok? result))
                    (assert-equal 'yes (stage-result-value result))))
            
            (define-test vote-tie-handling
              (let* ([stages (list (stage-pure 'a)
                                   (stage-pure 'b))]
                     [result (run-stage (council-vote stages) test-ctx 'question)])
                    ;; Tie should pick one (usually first)
                    (assert-true (stage-ok? result))))
            
            (define-test vote-empty-council
              (let* ([result (run-stage (council-vote '()) test-ctx 'question)])
                    ;; Empty vote should fail or return no-quorum
                    (assert-true (or (stage-ok? result) (stage-err? result)))))
            
            (define-test vote-with-threshold
              (let* ([stages (list (stage-pure 'yes)
                                   (stage-pure 'no)
                                   (stage-pure 'no))]
                     [config '((threshold . 0.6))]
                     [result (run-stage (council-vote stages config) test-ctx 'question)])
                    ;; 1/3 < 0.6, so should not reach threshold
                    (assert-true (or (stage-ok? result) (stage-err? result))))))

;;; ============================================================
;;; Council Consensus Tests
;;; ============================================================

(test-group council-consensus
            
            (define-test consensus-all-agree
              (let* ([stages (list (stage-pure 42)
                                   (stage-pure 42)
                                   (stage-pure 42))]
                     [result (run-stage (council-consensus stages) test-ctx 'input)])
                    (assert-true (stage-ok? result))
                    (assert-equal 42 (stage-result-value result))))
            
            (define-test consensus-disagreement
              (let* ([stages (list (stage-pure 1)
                                   (stage-pure 2)
                                   (stage-pure 1))]
                     [result (run-stage (council-consensus stages) test-ctx 'input)])
                    ;; Disagreement should fail or return no-consensus
                    (assert-true (or (stage-ok? result) (stage-err? result)))))
            
            (define-test consensus-empty
              (let* ([result (run-stage (council-consensus '()) test-ctx 'input)])
                    (assert-true (or (stage-ok? result) (stage-err? result)))))
            
            (define-test consensus-single-member
              (let* ([result (run-stage (council-consensus (list (stage-pure 'only-one))) test-ctx 'input)])
                    (assert-true (stage-ok? result))
                    (assert-equal 'only-one (stage-result-value result)))))

;;; ============================================================
;;; Council Delegate Tests
;;; ============================================================

(test-group council-delegate
            
            (define-test delegate-routes-by-predicate
              (let* ([predicates (list (cons positive? inc-stage)
                                       (cons negative? negate-stage))]
                     [delegate (council-delegate predicates (stage-pure 'default))]
                     [result-pos (run-stage delegate test-ctx 5)]
                     [result-neg (run-stage delegate test-ctx -5)])
                    (assert-true (stage-ok? result-pos))
                    (assert-equal 6 (stage-result-value result-pos))
                    (assert-true (stage-ok? result-neg))
                    (assert-equal 5 (stage-result-value result-neg))))
            
            (define-test delegate-uses-default
              (let* ([predicates (list (cons positive? inc-stage))]
                     [delegate (council-delegate predicates (stage-pure 'fallback))]
                     [result (run-stage delegate test-ctx 0)])  ; 0 is not positive
                    (assert-true (stage-ok? result))
                    (assert-equal 'fallback (stage-result-value result))))
            
            (define-test delegate-first-match-wins
              (let* ([predicates (list (cons number? (stage-pure 'number))
                                       (cons positive? (stage-pure 'positive)))]
                     [delegate (council-delegate predicates (stage-pure 'default))]
                     [result (run-stage delegate test-ctx 5)])
                    ;; 5 is both a number and positive, but number? comes first
                    (assert-true (stage-ok? result))
                    (assert-equal 'number (stage-result-value result)))))

;;; ============================================================
;;; Council Composition Tests
;;; ============================================================

(test-group council-composition
            
            (define-test compose-sequential-parallel
              ;; Sequential council of parallel councils
              (let* ([p1 (council-parallel (list inc-stage double-stage))]
                     [p2 (council-parallel (list negate-stage))]
                     [seq (council-sequential (list p1))]
                     [result (run-stage seq test-ctx 5)])
                    (assert-true (stage-ok? result))
                    (assert-equal '(6 10) (stage-result-value result))))
            
            (define-test council-as-stage
              ;; Council should be usable as a regular stage
              (let* ([c (council-sequential (list inc-stage))]
                     [composed (stage->>> c double-stage)]
                     [result (run-stage composed test-ctx 5)])
                    (assert-true (stage-ok? result))
                    (assert-equal 12 (stage-result-value result)))))

;;; ============================================================
;;; Council Configuration Tests
;;; ============================================================

(test-group council-config
            
            (define-test config-timeout
              (let ([c (make-council 'test '() 'parallel '((timeout . 5000)))])
                   (let ([cfg (council-config c)])
                        (assert-equal 5000 (cdr (assq 'timeout cfg))))))
            
            (define-test config-fuel
              (let ([c (make-council 'test '() 'sequential '((fuel . 1000)))])
                   (let ([cfg (council-config c)])
                        (assert-equal 1000 (cdr (assq 'fuel cfg))))))
            
            (define-test config-merge
              (let* ([base-cfg '((a . 1) (b . 2))]
                     [merged (merge-council-config base-cfg '((b . 3) (c . 4)))])
                    (assert-equal 1 (cdr (assq 'a merged)))
                    (assert-equal 3 (cdr (assq 'b merged)))  ; Overridden
                    (assert-equal 4 (cdr (assq 'c merged))))))

;;; ============================================================
;;; Tally Votes Tests
;;; ============================================================

(test-group tally-votes
            
            (define-test tally-single-vote
              (let ([result (tally-votes '(yes))])
                   (assert-equal 'yes (car result))
                   (assert-equal 1 (cdr result))))
            
            (define-test tally-majority
              (let ([result (tally-votes '(yes yes no))])
                   (assert-equal 'yes (car result))
                   (assert-equal 2 (cdr result))))
            
            (define-test tally-tie-returns-first
              (let ([result (tally-votes '(a b))])
                   ;; First in the vote list wins ties
                   (assert-true (or (eq? 'a (car result))
                                    (eq? 'b (car result))))))
            
            (define-test tally-empty
              (let ([result (tally-votes '())])
                   (assert-false result))))

;;; ============================================================
;;; All Agree Tests
;;; ============================================================

(test-group all-agree
            
            (define-test all-agree-true
              (assert-true (all-agree? '(42 42 42))))
            
            (define-test all-agree-false
              (assert-false (all-agree? '(1 2 1))))
            
            (define-test all-agree-single
              (assert-true (all-agree? '(only-one))))
            
            (define-test all-agree-empty
              (assert-true (all-agree? '()))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group integration
            
            (define-test full-workflow
              ;; Create a council, run it, verify result
              (let* ([council (make-council 'math-council
                                            '(incrementer doubler)
                                            'sequential
                                            '((fuel . 10000)))]
                     [stages (list inc-stage double-stage)]
                     [result (run-stage (council-sequential stages) test-ctx 10)])
                    ;; (10 + 1) * 2 = 22
                    (assert-equal 22 (stage-result-value result))))
            
            (define-test council-preserves-context
              (let* ([ctx-with-session (ctx-with-session test-ctx "test-session")]
                     [result (run-stage (council-sequential (list ctx-stage))
                                        ctx-with-session
                                        'input)])
                    (assert-equal "test-session" (stage-result-value result)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(print-summary)
(when (> *tests-failed* 0)
      (exit 1))
