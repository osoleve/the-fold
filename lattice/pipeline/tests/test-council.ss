;;; fabric/stitches/pipeline/tests/test-council.ss — Comprehensive Tests for Multi-Model Council
;;;
;;; Tests the council primitive for structured LLM deliberation.

(load "core/testing/test-framework.ss")
(load "lattice/pipeline/council.ss")

;;; ====
;;; Test Context
;;; ====

(define test-ctx empty-context)

;;; ====
;;; Council Configuration Tests
;;; ====

(test-group "Council Configuration"
  (define cfg (make-council-config
               '(opus sonnet)
               'sequential
               3
               'opus
               "${input}"
               '("Prompt 1" "Prompt 2" "Prompt 3")
               "Synthesize"
               #t
               60000))

  (define-test "make-council-config creates config"
    (assert-true (council-config? cfg)))

  (define-test "council-models extracts models"
    (assert-equal '(opus sonnet) (council-models cfg)))

  (define-test "council-mode extracts mode"
    (assert-equal 'sequential (council-mode cfg)))

  (define-test "council-rounds extracts rounds"
    (assert-equal 3 (council-rounds cfg)))

  (define-test "council-moderator extracts moderator"
    (assert-equal 'opus (council-moderator cfg)))

  (define-test "council-topic-template extracts template"
    (assert-equal "${input}" (council-topic-template cfg)))

  (define-test "council-round-prompts extracts prompts"
    (assert-equal '("Prompt 1" "Prompt 2" "Prompt 3") (council-round-prompts cfg)))

  (define-test "council-synthesis-prompt extracts synthesis"
    (assert-equal "Synthesize" (council-synthesis-prompt cfg)))

  (define-test "council-require-consensus extracts flag"
    (assert-equal #t (council-require-consensus cfg)))

  (define-test "council-timeout extracts timeout"
    (assert-equal 60000 (council-timeout cfg)))

  (define-test "council-config? false for non-config"
    (assert-equal #f (council-config? '(not a config)))))

;;; ====
;;; Council Result Tests
;;; ====

(test-group "Council Result"
  (define result (make-council-result
                  '((opus . "Response 1") (sonnet . "Response 2"))
                  "Synthesis text"
                  #t
                  '((gemini . "Dissenting view"))
                  '(("option1" . 2) ("option2" . 1))
                  '(((opus . "R1") (sonnet . "R1")))))

  (define-test "make-council-result creates result"
    (assert-true (council-result? result)))

  (define-test "result-responses extracts responses"
    (assert-equal '((opus . "Response 1") (sonnet . "Response 2"))
                  (result-responses result)))

  (define-test "result-synthesis extracts synthesis"
    (assert-equal "Synthesis text" (result-synthesis result)))

  (define-test "result-consensus extracts consensus flag"
    (assert-equal #t (result-consensus result)))

  (define-test "result-dissent extracts dissenting views"
    (assert-equal '((gemini . "Dissenting view")) (result-dissent result)))

  (define-test "result-votes extracts vote counts"
    (assert-equal '(("option1" . 2) ("option2" . 1)) (result-votes result)))

  (define-test "result-history extracts round history"
    (assert-equal '(((opus . "R1") (sonnet . "R1"))) (result-history result)))

  (define-test "council-result? false for non-result"
    (assert-equal #f (council-result? '(not a result)))))

;;; ====
;;; Sequential Council Tests
;;; ====

(test-group "Sequential Council"
  ;; Setup shared data
  (define council (council-sequential '(opus sonnet) 3 'opus))
  (define result (run-stage council test-ctx "Test topic"))
  (define cfg (council-effect-config result))

  (define-test "council-sequential creates stage"
    (assert-true (stage? council)))

  (define-test "council-sequential has correct name"
    (assert-equal 'council-sequential (stage-name council)))

  (define-test "sequential council produces effect"
    (assert-true (stage-effect? result)))

  (define-test "effect type is council"
    (assert-equal 'council (stage-effect-type result)))

  (define-test "effect is council-effect?"
    (assert-true (council-effect? result)))

  (define-test "council-effect-mode is sequential"
    (assert-equal 'sequential (council-effect-mode result)))

  (define-test "council-effect-topic is input"
    (assert-equal "Test topic" (council-effect-topic result)))

  (define-test "config has correct models"
    (assert-equal '(opus sonnet) (council-models cfg)))

  (define-test "config has correct rounds"
    (assert-equal 3 (council-rounds cfg)))

  (define-test "config has correct moderator"
    (assert-equal 'opus (council-moderator cfg))))

;;; ====
;;; Sequential Council with Custom Prompts Tests
;;; ====

(test-group "Sequential Council with Custom Prompts"
  ;; Setup shared data
  (define council (council-sequential-with-prompts
                   '(opus sonnet gemini)
                   2
                   'sonnet
                   '("Custom prompt 1" "Custom prompt 2")))
  (define result (run-stage council test-ctx "Topic"))
  (define cfg (council-effect-config result))

  (define-test "creates stage"
    (assert-true (stage? council)))

  (define-test "config has 3 models"
    (assert-equal '(opus sonnet gemini) (council-models cfg)))

  (define-test "config has 2 rounds"
    (assert-equal 2 (council-rounds cfg)))

  (define-test "config has custom prompts"
    (assert-equal '("Custom prompt 1" "Custom prompt 2")
                  (council-round-prompts cfg))))

;;; ====
;;; Parallel Council Tests
;;; ====

(test-group "Parallel Council"
  ;; Setup shared data
  (define council (council-parallel '(opus sonnet haiku) 'opus))
  (define result (run-stage council test-ctx "Parallel topic"))
  (define cfg (council-effect-config result))

  (define-test "council-parallel creates stage"
    (assert-true (stage? council)))

  (define-test "council-parallel has correct name"
    (assert-equal 'council-parallel (stage-name council)))

  (define-test "parallel council produces effect"
    (assert-true (stage-effect? result)))

  (define-test "council-effect-mode is parallel"
    (assert-equal 'parallel (council-effect-mode result)))

  (define-test "config has correct models"
    (assert-equal '(opus sonnet haiku) (council-models cfg)))

  (define-test "config has 1 round"
    (assert-equal 1 (council-rounds cfg)))

  (define-test "synthesizer is opus"
    (assert-equal 'opus (council-moderator cfg))))

;;; ====
;;; Range-from Helper Tests
;;; ====

(test-group "Range-from Helper"
  (define-test "range-from 1 3 produces (1 2 3)"
    (assert-equal '(1 2 3) (range-from 1 3)))

  (define-test "range-from 0 5 produces (0 1 2 3 4)"
    (assert-equal '(0 1 2 3 4) (range-from 0 5)))

  (define-test "range-from 5 0 produces empty list"
    (assert-equal '() (range-from 5 0)))

  (define-test "range-from 10 1 produces (10)"
    (assert-equal '(10) (range-from 10 1))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
