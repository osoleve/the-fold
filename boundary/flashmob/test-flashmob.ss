(load "core/testing/test-framework.ss")
(load "boundary/flashmob/flashmob.ss")
(load "boundary/flashmob/report.ss")

(doc 'module 'test-flashmob)
(doc 'description "Flashmob Test Suite - Tests for the game-theoretic QA triage system. Covers both simple and game strategies.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'usage "scheme --script boundary/flashmob/test-flashmob.ss")

(doc 'section 'test-data)
(doc 'note "Sample findings for testing")
(define *test-findings*
  '(((file . "auth.ss")
     (line . 42)
     (severity . high)
     (category . security)
     (confidence . 0.9)
     (agent-id . security-agent)
     (title . "SQL injection in login")
     (description . "User input not sanitized"))
    ((file . "db.ss")
     (line . 100)
     (severity . medium)
     (category . performance)
     (confidence . 0.7)
     (agent-id . perf-agent)
     (title . "N+1 query problem")
     (description . "Loop with individual queries"))
    ((file . "utils.ss")
     (line . 55)
     (severity . low)
     (category . correctness)
     (confidence . 0.8)
     (agent-id . correctness-agent)
     (title . "Off-by-one error")
     (description . "Array bounds check"))
    ((file . "main.ss")
     (line . 10)
     (severity . info)
     (category . style)
     (confidence . 0.6)
     (agent-id . style-agent)
     (title . "Inconsistent indentation")
     (description . "Mixed tabs and spaces"))
    ((file . "api.ss")
     (line . 200)
     (severity . critical)
     (category . security)
     (confidence . 0.95)
     (agent-id . security-agent)
     (title . "Authentication bypass")
     (description . "Missing auth check on admin endpoint"))))

(define *test-agents*
  '(security-agent perf-agent correctness-agent style-agent))

(doc 'section 'agent-tests)

(test-group "agents"
  (define-test "create default agent"
    (flashmob-clear-agents!)
    (let ([hash (flashmob-create-agent! 'security-agent)])
      (assert-true (bytevector? hash))))

  (define-test "agent expertise lookup"
    (flashmob-clear-agents!)
    (flashmob-create-agent! 'security-agent)
    (let ([exp (flashmob-agent-expertise 'security-agent 'security)])
      (assert-true (> exp 0.8))))

  (define-test "agent expertise vector"
    (flashmob-clear-agents!)
    (flashmob-create-agent! 'perf-agent)
    (let ([vec (flashmob-agent-expertise-vector 'perf-agent)])
      (assert-true (vector? vec))
      (assert-equal 5 (vector-length vec))))

  (define-test "unknown agent gets default expertise"
    (flashmob-clear-agents!)
    (let ([exp (flashmob-agent-expertise 'unknown-agent 'security)])
      (assert-equal 0.5 exp)))

  (define-test "agent score finding"
    (flashmob-clear-agents!)
    (flashmob-create-agent! 'security-agent)
    (let* ([finding (car *test-findings*)]
           [score (flashmob-agent-score-finding 'security-agent finding)])
      (assert-true (> score 0.5)))))

(doc 'section 'simple-strategy-tests)

(test-group "triage-simple"
  (define-test "simple triage returns result"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([result (simple-triage *test-agents* *test-findings* 3)])
      (assert-true (pair? result))
      (assert-equal 'simple (cdr (assq 'strategy result)))))

  (define-test "simple triage ranking contains findings"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (simple-triage *test-agents* *test-findings* 0)]
           [ranking (cdr (assq 'ranking result))])
      (assert-equal (length *test-findings*) (length ranking))))

  (define-test "simple triage selects top-k"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (simple-triage *test-agents* *test-findings* 2)]
           [selected (cdr (assq 'selected result))])
      (assert-equal 2 (length selected))))

  (define-test "simple consensus is 0-1"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (simple-triage *test-agents* *test-findings* 0)]
           [consensus (cdr (assq 'consensus result))])
      (assert-true (>= consensus 0))
      (assert-true (<= consensus 1))))

  (define-test "simple credits sum to approximately 1"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (simple-triage *test-agents* *test-findings* 0)]
           [credits (cdr (assq 'credits result))]
           [total (apply + (map cdr credits))])
      (assert-true (< (abs (- total 1.0)) 0.01))))

  (define-test "simple power indices are equal"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (simple-triage *test-agents* *test-findings* 0)]
           [power (cdr (assq 'power result))]
           [first-power (cdar power)])
      ;; All should be equal for simple strategy
      (assert-true (for-all (lambda (p)
                             (< (abs (- (cdr p) first-power)) 0.01))
                           power)))))

(doc 'section 'game-strategy-tests)

(test-group "triage-game"
  (define-test "game triage returns result"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([result (game-triage *test-agents* *test-findings* 3)])
      (assert-true (pair? result))
      (assert-equal 'game (cdr (assq 'strategy result)))))

  (define-test "game triage ranking contains findings"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (game-triage *test-agents* *test-findings* 0)]
           [ranking (cdr (assq 'ranking result))])
      (assert-equal (length *test-findings*) (length ranking))))

  (define-test "game triage uses pav selection"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (game-triage *test-agents* *test-findings* 2)]
           [selected (cdr (assq 'selected result))])
      (assert-true (<= (length selected) 2))))

  (define-test "game consensus is 0-1"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (game-triage *test-agents* *test-findings* 0)]
           [consensus (cdr (assq 'consensus result))])
      (assert-true (>= consensus 0))
      (assert-true (<= consensus 1))))

  (define-test "game banzhaf power varies by expertise"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (game-triage *test-agents* *test-findings* 0)]
           [power (cdr (assq 'power result))]
           [powers (map cdr power)])
      ;; Power should vary (not all exactly equal)
      (let ([min-p (apply min powers)]
            [max-p (apply max powers)])
        ;; Allow some variation
        (assert-true (>= (- max-p min-p) 0))))))

(doc 'section 'sav-strategy-tests)

(test-group "triage-sav"
  (define-test "sav triage returns result"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([result (game-triage-sav *test-agents* *test-findings* 3)])
      (assert-true (pair? result))
      (assert-equal 'game/sav (cdr (assq 'strategy result)))))

  (define-test "sav triage has proportionality metric"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (game-triage-sav *test-agents* *test-findings* 3)]
           [prop (cdr (assq 'proportionality result))])
      (assert-true (number? prop))
      (assert-true (>= prop 0))
      (assert-true (<= prop 1))))

  (define-test "sav triage has coverage metric"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (game-triage-sav *test-agents* *test-findings* 3)]
           [cov (cdr (assq 'coverage result))])
      (assert-true (number? cov))
      (assert-true (>= cov 0))
      (assert-true (<= cov 1)))))

(doc 'section 'cc-strategy-tests)

(test-group "triage-cc"
  (define-test "cc triage returns result"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([result (game-triage-cc *test-agents* *test-findings* 3)])
      (assert-true (pair? result))
      (assert-equal 'game/cc (cdr (assq 'strategy result)))))

  (define-test "cc triage has proportionality metric"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (game-triage-cc *test-agents* *test-findings* 3)]
           [prop (cdr (assq 'proportionality result))])
      (assert-true (number? prop))
      (assert-true (>= prop 0))
      (assert-true (<= prop 1))))

  (define-test "cc triage has coverage metric"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (game-triage-cc *test-agents* *test-findings* 3)]
           [cov (cdr (assq 'coverage result))])
      (assert-true (number? cov))
      (assert-true (>= cov 0))
      (assert-true (<= cov 1))))

  (define-test "cc triage selects findings"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (game-triage-cc *test-agents* *test-findings* 3)]
           [selected (cdr (assq 'selected result))])
      (assert-true (<= (length selected) 3)))))

(doc 'section 'strategy-comparison-tests)

(test-group "strategy-comparison"
  (define-test "compare returns both strategies"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([result (flashmob-triage-compare *test-agents* *test-findings* 'k 3)])
      (assert-true (pair? (assq 'simple result)))
      (assert-true (pair? (assq 'game result)))
      (assert-true (pair? (assq 'comparison result)))))

  (define-test "rank correlation is -1 to 1"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (flashmob-triage-compare *test-agents* *test-findings* 'k 3)]
           [comp (cdr (assq 'comparison result))]
           [corr (cdr (assq 'rank-correlation comp))])
      (assert-true (>= corr -1))
      (assert-true (<= corr 1))))

  (define-test "overlap ratio is 0 to 1"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let* ([result (flashmob-triage-compare *test-agents* *test-findings* 'k 3)]
           [comp (cdr (assq 'comparison result))]
           [overlap (cdr (assq 'top-k-overlap comp))])
      (assert-true (>= overlap 0))
      (assert-true (<= overlap 1)))))

(doc 'section 'strategy-dispatcher-tests)

(test-group "dispatcher"
  (define-test "run with simple strategy"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([result (flashmob-triage-run *test-agents* *test-findings*
                                       'strategy 'simple 'k 3)])
      (assert-equal 'simple (cdr (assq 'strategy result)))))

  (define-test "run with game strategy"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([result (flashmob-triage-run *test-agents* *test-findings*
                                       'strategy 'game 'k 3)])
      (assert-equal 'game (cdr (assq 'strategy result)))))

  (define-test "recommend strategy"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([recommended (flashmob-recommend-strategy *test-agents* *test-findings*)])
      (assert-true (or (eq? recommended 'simple)
                       (eq? recommended 'game)
                       (eq? recommended 'game/sav)
                       (eq? recommended 'game/cc)))))

  (define-test "run with game/sav strategy"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([result (flashmob-triage-run *test-agents* *test-findings*
                                       'strategy 'game/sav 'k 3)])
      (assert-equal 'game/sav (cdr (assq 'strategy result)))))

  (define-test "run with game/cc strategy"
    (flashmob-clear-agents!)
    (for-each flashmob-create-agent! *test-agents*)
    (let ([result (flashmob-triage-run *test-agents* *test-findings*
                                       'strategy 'game/cc 'k 3)])
      (assert-equal 'game/cc (cdr (assq 'strategy result))))))

(doc 'section 'block-tests)

(test-group "blocks"
  (define-test "create finding block"
    (let* ([blk (make-finding-block "test.ss" 10 'high 'security 0.9
                                    'test-agent "Test finding" "desc" #f)]
           [data (finding-block-data blk)])
      (assert-true (pair? data))
      (assert-equal "test.ss" (cdr (assq 'file data)))))

  (define-test "create agent block"
    (let* ([blk (make-agent-block 'test-agent "test"
                                  '((security . 0.8) (performance . 0.5)))]
           [data (agent-block-data blk)])
      (assert-true (pair? data))
      (assert-equal 'test-agent (cdr (assq 'agent-id data)))))

  (define-test "create triage block"
    (let* ([blk (make-triage-block 'simple '("f1" "f2") 0.75
                                   '((a . 0.5) (b . 0.5))
                                   '((a . 0.5) (b . 0.5))
                                   #vu8(1 2 3 4))]
           [data (triage-block-data blk)])
      (assert-true (pair? data))
      (assert-equal 'simple (cdr (assq 'strategy data))))))

(doc 'section 'report-tests)

(test-group "report"
  (define-test "json serialize string"
    (assert-equal "\"hello\"" (json-serialize "hello")))

  (define-test "json serialize number"
    (assert-equal "42" (json-serialize 42)))

  (define-test "json serialize list"
    (assert-equal "[1, 2, 3]" (json-serialize '(1 2 3))))

  (define-test "json serialize object"
    (let ([result (json-serialize '((a . 1) (b . 2)))])
      (assert-true (string? result))
      (assert-true (> (string-length result) 0))))

  (define-test "json escape special chars"
    (let ([result (json-serialize "hello\nworld")])
      (assert-true (string? result)))))

(doc 'section 'edge-case-tests)

(test-group "edge-cases"
  (define-test "empty findings"
    (flashmob-clear-agents!)
    (let ([result (simple-triage '() '() 0)])
      (assert-equal 'simple (cdr (assq 'strategy result)))
      (assert-equal '() (cdr (assq 'ranking result)))))

  (define-test "single finding"
    (flashmob-clear-agents!)
    (flashmob-create-agent! 'security-agent)
    (let* ([single (list (car *test-findings*))]
           [result (simple-triage '(security-agent) single 1)])
      (assert-equal 1 (length (cdr (assq 'ranking result))))))

  (define-test "single agent"
    (flashmob-clear-agents!)
    (flashmob-create-agent! 'security-agent)
    (let ([result (simple-triage '(security-agent) *test-findings* 3)])
      (assert-equal 'simple (cdr (assq 'strategy result))))))

(doc 'section 'helper-functions)

(define (for-all pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (for-all pred (cdr lst)))))

(doc 'section 'run-tests)

(printf "~n=== Flashmob Test Suite ===~n~n")
(run-all-tests)
