;;; Test suite for flashmob/triage.ss
;;; Run with: scheme --script boundary/flashmob/test-triage.ss

(load "core/testing/test-framework.ss")
(load "boundary/flashmob/triage.ss")

;;; Sample test data
(define *test-agents* '(agent-a agent-b agent-c))

(define *test-findings*
  '(((file . "test.ss") (line . 10) (severity . high) (category . correctness)
     (title . "Critical bug") (confidence . 0.9) (agent-id . agent-a))
    ((file . "test.ss") (line . 20) (severity . medium) (category . performance)
     (title . "Slow loop") (confidence . 0.7) (agent-id . agent-b))
    ((file . "test.ss") (line . 30) (severity . low) (category . style)
     (title . "Naming") (confidence . 0.5) (agent-id . agent-c))))

(test-group "triage configuration"

  (define-test "*flashmob-default-strategy* has sensible default"
    (assert-equal 'simple *flashmob-default-strategy*))

  (define-test "*flashmob-default-k* has sensible default"
    (assert-equal 10 *flashmob-default-k*)))

(test-group "keyword argument utility"

  (define-test "fm-get-keyword extracts present keys"
    (assert-equal 5 (fm-get-keyword '(count 5 verbose #t) 'count 10)))

  (define-test "fm-get-keyword returns default for missing keys"
    (assert-equal 10 (fm-get-keyword '(verbose #t) 'count 10)))

  (define-test "fm-get-keyword handles empty args"
    (assert-equal 10 (fm-get-keyword '() 'count 10)))

  (define-test "fm-get-keyword handles odd-length args gracefully"
    (assert-equal 10 (fm-get-keyword '(count) 'count 10))))

(test-group "triage comparison metrics"

  (define-test "triage-kendall-tau returns 1 for identical rankings"
    (let ([ranking '(((title . "A")) ((title . "B")) ((title . "C")))])
      ;; Should return 1 (exact or inexact)
      (assert-true (= 1 (triage-kendall-tau ranking ranking)))))

  (define-test "triage-overlap-ratio returns 1 for identical sets"
    (let ([selected '(((title . "A")) ((title . "B")))])
      (assert-true (= 1 (triage-overlap-ratio selected selected)))))

  (define-test "triage-overlap-ratio returns 0 for disjoint sets"
    (let ([set1 '(((title . "A")))]
          [set2 '(((title . "B")))])
      (assert-true (= 0 (triage-overlap-ratio set1 set2)))))

  (define-test "triage-overlap-ratio handles empty sets"
    (assert-true (= 1 (triage-overlap-ratio '() '())))
    (assert-true (= 0 (triage-overlap-ratio '() '(((title . "A"))))))))

(test-group "triage helper functions"

  (define-test "triage-extract-titles extracts title strings"
    (let* ([findings '(((title . "Bug 1") (severity . high))
                       ((title . "Bug 2") (severity . low)))]
           [titles (triage-extract-titles findings)])
      (assert-equal '("Bug 1" "Bug 2") titles)))

  (define-test "triage-position-of finds correct position"
    (assert-equal 0 (triage-position-of "a" '("a" "b" "c")))
    (assert-equal 2 (triage-position-of "c" '("a" "b" "c"))))

  (define-test "triage-all-pairs generates correct pairs"
    (let ([pairs (triage-all-pairs '(a b c))])
      (assert-equal 3 (length pairs))
      ;; member returns the tail, so check if it's a pair (not #f)
      (assert-true (pair? (member '(a . b) pairs)))
      (assert-true (pair? (member '(a . c) pairs)))
      (assert-true (pair? (member '(b . c) pairs))))))

(test-group "triage result accessors"

  (define sample-result
    '((ranking . (((title . "A")) ((title . "B"))))
      (selected . (((title . "A"))))
      (consensus . 0.85)
      (credits . ((agent-a . 0.5) (agent-b . 0.3)))
      (power . ((agent-a . 0.6) (agent-b . 0.4)))
      (strategy . simple)))

  (define-test "flashmob-triage-ranking extracts ranking"
    (assert-equal 2 (length (flashmob-triage-ranking sample-result))))

  (define-test "flashmob-triage-selected extracts selected"
    (assert-equal 1 (length (flashmob-triage-selected sample-result))))

  (define-test "flashmob-triage-consensus extracts score"
    (assert-equal 0.85 (flashmob-triage-consensus sample-result)))

  (define-test "flashmob-triage-strategy extracts strategy"
    (assert-equal 'simple (flashmob-triage-strategy sample-result)))

  (define-test "flashmob-triage-proportionality returns 0 if not present"
    (assert-equal 0 (flashmob-triage-proportionality sample-result)))

  (define-test "flashmob-triage-coverage returns 1.0 if not present"
    (assert-equal 1.0 (flashmob-triage-coverage sample-result))))

(test-group "strategy recommendation"

  (define-test "flashmob-recommend-strategy returns simple for many agents"
    ;; More than 15 agents should recommend simple
    (let ([many-agents (map (lambda (i) (string->symbol (format "agent-~a" i)))
                            (iota 20))])
      (assert-equal 'simple (flashmob-recommend-strategy many-agents *test-findings*))))

  (define-test "flashmob-recommend-strategy returns simple for few findings"
    (let ([few-findings (list (car *test-findings*))])
      (assert-equal 'simple (flashmob-recommend-strategy *test-agents* few-findings)))))

(run-all-tests)
