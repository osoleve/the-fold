;;; fabric/stitches/pipeline/tests/test-council.ss — Comprehensive Tests for Multi-Model Council
;;;
;;; Tests the council primitive for structured LLM deliberation.

(load "lattice/pipeline/council.ss")

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
;;; Council Configuration Tests
;;; ====

(run-tests "Council Configuration"
           (lambda ()
                   (let ([cfg (make-council-config
                               '(opus sonnet)
                               'sequential
                               3
                               'opus
                               "${input}"
                               '("Prompt 1" "Prompt 2" "Prompt 3")
                               "Synthesize"
                               #t
                               60000)])
                        (test-pred "make-council-config creates config"
                                   council-config?
                                   cfg)
                        
                        (test "council-models extracts models"
                              '(opus sonnet)
                              (council-models cfg))
                        
                        (test "council-mode extracts mode"
                              'sequential
                              (council-mode cfg))
                        
                        (test "council-rounds extracts rounds"
                              3
                              (council-rounds cfg))
                        
                        (test "council-moderator extracts moderator"
                              'opus
                              (council-moderator cfg))
                        
                        (test "council-topic-template extracts template"
                              "${input}"
                              (council-topic-template cfg))
                        
                        (test "council-round-prompts extracts prompts"
                              '("Prompt 1" "Prompt 2" "Prompt 3")
                              (council-round-prompts cfg))
                        
                        (test "council-synthesis-prompt extracts synthesis"
                              "Synthesize"
                              (council-synthesis-prompt cfg))
                        
                        (test "council-require-consensus extracts flag"
                              #t
                              (council-require-consensus cfg))
                        
                        (test "council-timeout extracts timeout"
                              60000
                              (council-timeout cfg)))
                   
                   (test "council-config? false for non-config"
                         #f
                         (council-config? '(not a config)))))

;;; ====
;;; Council Result Tests
;;; ====

(run-tests "Council Result"
           (lambda ()
                   (let ([result (make-council-result
                                  '((opus . "Response 1") (sonnet . "Response 2"))
                                  "Synthesis text"
                                  #t
                                  '((gemini . "Dissenting view"))
                                  '(("option1" . 2) ("option2" . 1))
                                  '(((opus . "R1") (sonnet . "R1"))))])
                        (test-pred "make-council-result creates result"
                                   council-result?
                                   result)
                        
                        (test "result-responses extracts responses"
                              '((opus . "Response 1") (sonnet . "Response 2"))
                              (result-responses result))
                        
                        (test "result-synthesis extracts synthesis"
                              "Synthesis text"
                              (result-synthesis result))
                        
                        (test "result-consensus extracts consensus flag"
                              #t
                              (result-consensus result))
                        
                        (test "result-dissent extracts dissenting views"
                              '((gemini . "Dissenting view"))
                              (result-dissent result))
                        
                        (test "result-votes extracts vote counts"
                              '(("option1" . 2) ("option2" . 1))
                              (result-votes result))
                        
                        (test "result-history extracts round history"
                              '(((opus . "R1") (sonnet . "R1")))
                              (result-history result)))
                   
                   (test "council-result? false for non-result"
                         #f
                         (council-result? '(not a result)))))

;;; ====
;;; Sequential Council Tests
;;; ====

(run-tests "Sequential Council"
           (lambda ()
                   (let ([council (council-sequential '(opus sonnet) 3 'opus)])
                        (test-pred "council-sequential creates stage"
                                   stage?
                                   council)
                        
                        (test "council-sequential has correct name"
                              'council-sequential
                              (stage-name council))
                        
                        ;; Run and check effect
                        (let ([result (run-stage council test-ctx "Test topic")])
                             (test-pred "sequential council produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "effect type is council"
                                   'council
                                   (stage-effect-type result))
                             
                             (test-pred "effect is council-effect?"
                                        council-effect?
                                        result)
                             
                             (test "council-effect-mode is sequential"
                                   'sequential
                                   (council-effect-mode result))
                             
                             (test "council-effect-topic is input"
                                   "Test topic"
                                   (council-effect-topic result))
                             
                             (let ([cfg (council-effect-config result)])
                                  (test "config has correct models"
                                        '(opus sonnet)
                                        (council-models cfg))
                                  
                                  (test "config has correct rounds"
                                        3
                                        (council-rounds cfg))
                                  
                                  (test "config has correct moderator"
                                        'opus
                                        (council-moderator cfg)))))))

;;; ====
;;; Sequential Council with Custom Prompts Tests
;;; ====

(run-tests "Sequential Council with Custom Prompts"
           (lambda ()
                   (let ([council (council-sequential-with-prompts
                                   '(opus sonnet gemini)
                                   2
                                   'sonnet
                                   '("Custom prompt 1" "Custom prompt 2"))])
                        (test-pred "creates stage"
                                   stage?
                                   council)
                        
                        (let* ([result (run-stage council test-ctx "Topic")]
                               [cfg (council-effect-config result)])
                              (test "config has 3 models"
                                    '(opus sonnet gemini)
                                    (council-models cfg))
                              
                              (test "config has 2 rounds"
                                    2
                                    (council-rounds cfg))
                              
                              (test "config has custom prompts"
                                    '("Custom prompt 1" "Custom prompt 2")
                                    (council-round-prompts cfg))))))

;;; ====
;;; Parallel Council Tests
;;; ====

(run-tests "Parallel Council"
           (lambda ()
                   (let ([council (council-parallel '(opus sonnet haiku) 'opus)])
                        (test-pred "council-parallel creates stage"
                                   stage?
                                   council)
                        
                        (test "council-parallel has correct name"
                              'council-parallel
                              (stage-name council))
                        
                        (let ([result (run-stage council test-ctx "Parallel topic")])
                             (test-pred "parallel council produces effect"
                                        stage-effect?
                                        result)
                             
                             (test "council-effect-mode is parallel"
                                   'parallel
                                   (council-effect-mode result))
                             
                             (let ([cfg (council-effect-config result)])
                                  (test "config has correct models"
                                        '(opus sonnet haiku)
                                        (council-models cfg))
                                  
                                  (test "config has 1 round"
                                        1
                                        (council-rounds cfg))
                                  
                                  (test "synthesizer is opus"
                                        'opus
                                        (council-moderator cfg)))))))

;;; ====
;;; Range-from Helper Tests
;;; ====

(run-tests "Range-from Helper"
           (lambda ()
                   (test "range-from 1 3 produces (1 2 3)"
                         '(1 2 3)
                         (range-from 1 3))
                   
                   (test "range-from 0 5 produces (0 1 2 3 4)"
                         '(0 1 2 3 4)
                         (range-from 0 5))
                   
                   (test "range-from 5 0 produces empty list"
                         '()
                         (range-from 5 0))
                   
                   (test "range-from 10 1 produces (10)"
                         '(10)
                         (range-from 10 1))))

;;; ====
;;; Run All Tests
;;; ====

(test-summary)
