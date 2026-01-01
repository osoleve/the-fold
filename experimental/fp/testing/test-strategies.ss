;;; fabric/stitches/fp/test-strategies.ss — Tests for Evaluation Strategies
;;;
;;; NOTE: Run from project root: scheme --script fabric/stitches/fp/test-strategies.ss

(load "core/test-framework.ss")
(load "core/fp/testing/strategies.ss")

(display "\n")
(display "==============================================================\n")
(display "         EVALUATION STRATEGIES TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Core Strategies Tests
;;; ============================================================

(test-group core-strategies
            (define-test r0-identity-test
              (assert-equal 42 (r0 42))
              (assert-equal '(1 2 3) (r0 '(1 2 3)))
              (assert-equal "hello" (r0 "hello")))
            
            (define-test rseq-identity-test
              (assert-equal 42 (rseq 42))
              (assert-equal '(1 2 3) (rseq '(1 2 3))))
            
            (define-test rpar-identity-test
              (assert-equal 42 (rpar 42))
              (assert-equal '(a b c) (rpar '(a b c))))
            
            (define-test rdeepseq-atom-test
              (assert-equal 42 (rdeepseq 42))
              (assert-equal 'sym (rdeepseq 'sym))
              (assert-equal "str" (rdeepseq "str")))
            
            (define-test rdeepseq-list-test
              (assert-equal '() (rdeepseq '()))
              (assert-equal '(1 2 3) (rdeepseq '(1 2 3)))
              (assert-equal '((1 2) (3 4)) (rdeepseq '((1 2) (3 4)))))
            
            (define-test rdeepseq-nested-test
              (let ([nested '(((a b) c) ((d e) f))])
                   (assert-equal nested (rdeepseq nested))))
            
            (define-test rdeepseq-vector-test
              (let ([v (vector 1 2 3)])
                   (assert-equal v (rdeepseq v))))
            
            (define-test rwhnf-alias-test
              (assert-equal 42 (rwhnf 42)))
            
            (define-test rnf-alias-test
              (assert-equal '(1 2) (rnf '(1 2)))))

;;; ============================================================
;;; Strategy Application Tests
;;; ============================================================

(test-group strategy-application
            (define-test using-test
              (assert-equal 42 (using 42 rseq))
              (assert-equal '(1 2 3) (using '(1 2 3) rdeepseq)))
            
            (define-test with-strategy-test
              (assert-equal 42 (with-strategy rseq 42))
              (assert-equal '(a b) (with-strategy rpar '(a b)))))

;;; ============================================================
;;; Strategy Composition Tests
;;; ============================================================

(test-group strategy-composition
            (define-test dot-composition-test
              (let ([strat (dot rseq rpar)])
                   (assert-equal 42 (strat 42))))
            
            (define-test dot-identity-left-test
              (let ([strat (dot r0 rseq)])
                   (assert-equal 42 (strat 42))))
            
            (define-test dot-identity-right-test
              (let ([strat (dot rseq r0)])
                   (assert-equal 42 (strat 42))))
            
            (define-test strat-seq-empty-test
              (let ([strat (strat-seq '())])
                   (assert-equal 42 (strat 42))))
            
            (define-test strat-seq-single-test
              (let ([strat (strat-seq (list rseq))])
                   (assert-equal 42 (strat 42))))
            
            (define-test strat-seq-multiple-test
              (let ([strat (strat-seq (list rseq rpar rdeepseq))])
                   (assert-equal '(1 2) (strat '(1 2))))))

;;; ============================================================
;;; List Strategy Tests
;;; ============================================================

(test-group list-strategies
            (define-test eval-list-test
              (let ([strat (eval-list rseq)])
                   (assert-equal '(1 2 3) (strat '(1 2 3)))))
            
            (define-test eval-list-empty-test
              (let ([strat (eval-list rseq)])
                   (assert-equal '() (strat '()))))
            
            (define-test parList-test
              (let ([strat (parList rseq)])
                   (assert-equal '(1 2 3) (strat '(1 2 3)))))
            
            (define-test parList-nested-test
              (let ([strat (parList rdeepseq)])
                   (assert-equal '((1 2) (3 4)) (strat '((1 2) (3 4))))))
            
            (define-test parMap-test
              (let ([f (lambda (x) (* x 2))]
                    [strat rseq])
                   (assert-equal '(2 4 6) ((parMap f strat) '(1 2 3)))))
            
            (define-test parMap-empty-test
              (let ([f (lambda (x) (* x 2))]
                    [strat rseq])
                   (assert-equal '() ((parMap f strat) '()))))
            
            (define-test parBuffer-test
              (let ([strat (parBuffer 2 rseq)])
                   (assert-equal '(1 2 3 4 5) (strat '(1 2 3 4 5)))))
            
            (define-test parBuffer-exact-chunks-test
              (let ([strat (parBuffer 2 rseq)])
                   (assert-equal '(1 2 3 4) (strat '(1 2 3 4)))))
            
            (define-test parBuffer-empty-test
              (let ([strat (parBuffer 3 rseq)])
                   (assert-equal '() (strat '())))))

;;; ============================================================
;;; Tuple Strategy Tests
;;; ============================================================

(test-group tuple-strategies
            (define-test eval-tuple2-test
              (let ([strat (eval-tuple2 rseq rseq)])
                   (assert-equal '(1 . 2) (strat '(1 . 2)))))
            
            (define-test parTuple2-test
              (let ([strat (parTuple2 rseq rseq)])
                   (assert-equal '(a . b) (strat '(a . b)))))
            
            (define-test eval-tuple3-test
              (let ([strat (eval-tuple3 rseq rseq rseq)])
                   (assert-equal '(1 2 3) (strat '(1 2 3))))))

;;; ============================================================
;;; Conditional Strategy Tests
;;; ============================================================

(test-group conditional-strategies
            (define-test seqList-test
              (assert-equal '(1 2 3) (seqList '(1 2 3))))
            
            (define-test seqList-empty-test
              (assert-equal '() (seqList '())))
            
            (define-test seqListN-test
              (let ([strat (seqListN 2)])
                   (assert-equal '(1 2 3 4) (strat '(1 2 3 4)))))
            
            (define-test seqListN-short-list-test
              (let ([strat (seqListN 10)])
                   (assert-equal '(1 2) (strat '(1 2))))))

;;; ============================================================
;;; Demanding Combinator Tests
;;; ============================================================

(test-group demanding-combinators
            (define-test demanding-test
              (assert-equal 'result (demanding 'result (rdeepseq '(1 2 3)))))
            
            (define-test spark-test
              (assert-equal 42 (spark 42))
              (assert-equal '(a b) (spark '(a b)))))

;;; ============================================================
;;; Force Combinator Tests
;;; ============================================================

(test-group force-combinators
            (define-test force-test
              (assert-equal 42 (force 42)))
            
            (define-test force-list-test
              (assert-equal '(1 2 3) (force-list '(1 2 3)))
              (assert-equal '((a) (b)) (force-list '((a) (b)))))
            
            (define-test force-spine-test
              (assert-equal '(1 2 3) (force-spine '(1 2 3)))))

;;; ============================================================
;;; Chunking Tests
;;; ============================================================

(test-group chunking
            (define-test chunk-exact-test
              (assert-equal '((1 2) (3 4)) (chunk 2 '(1 2 3 4))))
            
            (define-test chunk-remainder-test
              (assert-equal '((1 2) (3 4) (5)) (chunk 2 '(1 2 3 4 5))))
            
            (define-test chunk-empty-test
              (assert-equal '() (chunk 3 '())))
            
            (define-test chunk-larger-than-list-test
              (assert-equal '((1 2)) (chunk 5 '(1 2))))
            
            (define-test parListChunk-test
              (let ([strat (parListChunk 2 rseq)])
                   (assert-equal '(1 2 3 4 5) (strat '(1 2 3 4 5))))))

;;; ============================================================
;;; Deep Evaluation Tests
;;; ============================================================

(test-group deep-evaluation
            (define-test deep-seq-test
              (assert-equal 'result (deep-seq '(1 2 3) 'result)))
            
            (define-test $!!-test
              (assert-equal 6 ($!! (lambda (x) (+ x 1)) 5)))
            
            (define-test $!-test
              (assert-equal 6 ($! (lambda (x) (+ x 1)) 5))))

;;; ============================================================
;;; Helper Function Tests
;;; ============================================================

(test-group helpers
            (define-test take-up-to-test
              (assert-equal '(1 2) (take-up-to 2 '(1 2 3 4)))
              (assert-equal '(1 2) (take-up-to 5 '(1 2)))
              (assert-equal '() (take-up-to 0 '(1 2 3))))
            
            (define-test drop-up-to-test
              (assert-equal '(3 4) (drop-up-to 2 '(1 2 3 4)))
              (assert-equal '() (drop-up-to 5 '(1 2)))
              (assert-equal '(1 2 3) (drop-up-to 0 '(1 2 3)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All evaluation strategy tests passed.\n")
    (display "\n[FAILURE] Some evaluation strategy tests failed.\n"))
