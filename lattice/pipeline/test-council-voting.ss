(load "core/testing/test-framework.ss")
(load "lattice/pipeline/council-voting.ss")

(doc 'module 'pipeline/test-council-voting)
(doc 'description "Tests for Council Voting Integration. Tests the integration of voting theory with the council framework.")
(doc 'layer 'lattice)

(doc 'section 'configuration-tests)

(test-group "ranked-vote-config"

  (define-test config-creation
    (let ([cfg (make-ranked-vote-config
                '(opus sonnet gemini-3)
                '(opt-a opt-b opt-c)
                'schulze
                #f #t 2)])
      (assert-true (ranked-vote-config? cfg))
      (assert-equal '(opus sonnet gemini-3) (rvc-models cfg))
      (assert-equal '(opt-a opt-b opt-c) (rvc-proposals cfg))
      (assert-equal 'schulze (rvc-voting-rule cfg))
      (assert-equal #f (rvc-require-condorcet cfg))
      (assert-equal #t (rvc-allow-ties cfg))
      (assert-equal 2 (rvc-discussion-rounds cfg))))

  (define-test config-predicate
    (assert-true (ranked-vote-config?
                  (make-ranked-vote-config '(a) '(x y) 'borda #f #f 0)))
    (assert-false (ranked-vote-config? '(not a config)))
    (assert-false (ranked-vote-config? 42)))

)

(doc 'section 'result-tests)

(test-group "ranked-vote-result"

  (define-test result-creation
    (let ([profile (make-preference-profile '((a b c) (a b c) (b a c)))])
      (let ([result (make-ranked-vote-result
                     'a
                     '(a b c)
                     profile
                     'a  ; Condorcet winner
                     '((method . schulze))
                     'schulze)])
        (assert-true (ranked-vote-result? result))
        (assert-equal 'a (rvr-winner result))
        (assert-equal '(a b c) (rvr-ranking result))
        (assert-equal 'a (rvr-condorcet result))
        (assert-equal 'schulze (rvr-rule result)))))

)

(doc 'section 'voting-rule-application-tests)

(test-group "apply-voting-rule"

  (define-test plurality-rule
    (let* ([profile (make-preference-profile '((a b c) (a b c) (b a c)))]
           [result (apply-voting-rule 'plurality profile)])
      (assert-equal 'a (car result))))

  (define-test borda-rule
    (let* ([profile (make-preference-profile '((a b c) (a b c) (b a c)))]
           [result (apply-voting-rule 'borda profile)])
      ;; a=5, b=4, c=0
      (assert-equal 'a (car result))))

  (define-test schulze-rule
    (let* ([profile (make-preference-profile '((a b c) (a b c) (b a c)))]
           [result (apply-voting-rule 'schulze profile)])
      (assert-equal 'a (car result))
      (assert-equal 'a (cadr result))))  ; First in ranking

  (define-test copeland-rule
    (let* ([profile (make-preference-profile '((a b c) (a b c) (b a c)))]
           [result (apply-voting-rule 'copeland profile)])
      (assert-equal 'a (car result))))

  (define-test condorcet-rule-with-winner
    (let* ([profile (make-preference-profile '((a b c) (a b c) (a c b)))]
           [result (apply-voting-rule 'condorcet profile)])
      (assert-equal 'a (car result))))

  (define-test condorcet-rule-with-cycle
    ;; Condorcet cycle - falls back to Schulze
    (let* ([profile (condorcet-cycle-example)]
           [result (apply-voting-rule 'condorcet profile)])
      (assert-true (and (member (car result) '(a b c)) #t))))

)

(doc 'section 'stage-creation-tests)

(test-group "council-stages"

  (define-test council-schulze-stage
    (let ([stage (council-schulze '(opus sonnet) '(opt-a opt-b))])
      (assert-true (stage? stage))
      (assert-equal 'council-vote-ranked (stage-name stage))))

  (define-test council-borda-stage
    (let ([stage (council-borda '(opus sonnet gemini-3) '(x y z))])
      (assert-true (stage? stage))
      (assert-equal 'council-vote-ranked (stage-name stage))))

  (define-test council-condorcet-stage
    (let ([stage (council-condorcet '(opus sonnet) '(approve reject))])
      (assert-true (stage? stage))))

)

(doc 'section 'deliberation-tests)

(test-group "deliberation"

  (define-test deliberation-creation
    (let ([d (make-deliberation
              "Test Topic"
              '(option-1 option-2 option-3)
              '(voter-a voter-b voter-c)
              2
              'schulze
              'voter-a)])
      (assert-true (deliberation? d))
      (assert-equal "Test Topic" (delib-topic d))
      (assert-equal '(option-1 option-2 option-3) (delib-proposals d))
      (assert-equal '(voter-a voter-b voter-c) (delib-voters d))
      (assert-equal 2 (delib-rounds d))
      (assert-equal 'schulze (delib-rule d))
      (assert-equal 'voter-a (delib-moderator d))))

  (define-test code-review-deliberation-builder
    (let ([d (code-review-deliberation
              '(opus sonnet gemini-3)
              '("approve" "request-changes" "reject"))])
      (assert-true (deliberation? d))
      (assert-equal "Code Review" (delib-topic d))
      (assert-equal 3 (length (delib-proposals d)))
      (assert-equal 3 (length (delib-voters d)))
      (assert-equal 1 (delib-rounds d))
      (assert-equal 'schulze (delib-rule d))))

  (define-test architecture-deliberation-builder
    (let ([d (architecture-deliberation
              '(opus gemini-3)
              '("microservices" "monolith" "serverless"))])
      (assert-true (deliberation? d))
      (assert-equal "Architecture Decision" (delib-topic d))
      (assert-equal 2 (delib-rounds d))))

  (define-test task-prioritization-builder
    (let ([d (task-prioritization
              '(opus sonnet)
              '("task-1" "task-2" "task-3"))])
      (assert-true (deliberation? d))
      (assert-equal "Task Prioritization" (delib-topic d))
      (assert-equal 0 (delib-rounds d))  ; No discussion
      (assert-equal 'borda (delib-rule d))))

)

(doc 'section 'result-analysis-tests)

(test-group "result-analysis"

  (define-test clear-winner-detection
    (let* ([profile (make-preference-profile '((a b c) (a b c) (a c b)))]
           [result (make-ranked-vote-result
                    'a '(a b c) profile 'a '() 'schulze)])
      (assert-true (result-has-clear-winner? result))))

  (define-test no-clear-winner
    (let* ([profile (condorcet-cycle-example)]
           [result (make-ranked-vote-result
                    'a '(a b c) profile #f '() 'schulze)])
      (assert-false (result-has-clear-winner? result))))

  (define-test controversy-score-agreement
    ;; All methods agree
    (let* ([profile (make-preference-profile '((a b c) (a b c) (a c b)))]
           [result (make-ranked-vote-result
                    'a '(a b c) profile 'a '() 'schulze)])
      (assert-equal 0 (result-controversy-score result))))

  (define-test remove-duplicates-works
    (assert-equal '(a b c) (remove-duplicates '(a b c a b c)))
    (assert-equal '(x) (remove-duplicates '(x x x)))
    (assert-equal '() (remove-duplicates '())))

)

(doc 'section 'helper-tests)

(test-group "helpers"

  (define-test remove-element
    (assert-equal '(b c) (remove 'a '(a b c)))
    (assert-equal '(a b c) (remove 'x '(a b c)))
    (assert-equal '() (remove 'a '(a))))

  (define-test voting-rule-predicate
    (assert-true (and (voting-rule? 'plurality) #t))
    (assert-true (and (voting-rule? 'borda) #t))
    (assert-true (and (voting-rule? 'schulze) #t))
    (assert-true (and (voting-rule? 'copeland) #t))
    (assert-true (and (voting-rule? 'condorcet) #t))
    (assert-false (voting-rule? 'invalid))
    (assert-false (voting-rule? 42)))

)

(run-all-tests-and-exit)
