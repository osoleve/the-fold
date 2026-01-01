;;; fabric/stitches/pipeline/council.ss — Multi-Model Council Primitive
;;;
;;; Councils enable structured deliberation between multiple LLMs.
;;; Two modes:
;;;   - Sequential rounds: Models respond in turns, seeing prior responses
;;;   - Parallel synthesis: Models respond independently, then synthesize
;;;
;;; This is Core code: pure stage construction.
;;; Effect interpretation happens in thimble/pipeline/interpreter.ss
;;;
;;; Features:
;;;   - Sequential round-robin deliberation
;;;   - Parallel independent responses
;;;   - Synthesis/moderator stage
;;;   - Consensus detection
;;;   - Vote counting
;;;
;;; Dependencies:
;;;   - pipeline/stage.ss
;;;   - pipeline/effects.ss
;;;   - pipeline/context.ss

(load "core/pipeline/stage.ss")
(load "core/pipeline/effects.ss")
(load "core/pipeline/context.ss")

;;; ============================================================
;;; Council Configuration
;;; ============================================================

;;; make-council-config : Fields -> CouncilConfig
(define (make-council-config models
                             mode
                             rounds
                             moderator
                             topic-template
                             round-prompts
                             synthesis-prompt
                             require-consensus
                             timeout-per-model)
  (list 'council-config
        models mode rounds moderator topic-template
        round-prompts synthesis-prompt require-consensus timeout-per-model))

;;; council-config? : Any -> Boolean
(define (council-config? x)
  (and (pair? x) (eq? (car x) 'council-config)))

;;; Accessors
(define (council-models cfg) (list-ref cfg 1))
(define (council-mode cfg) (list-ref cfg 2))
(define (council-rounds cfg) (list-ref cfg 3))
(define (council-moderator cfg) (list-ref cfg 4))
(define (council-topic-template cfg) (list-ref cfg 5))
(define (council-round-prompts cfg) (list-ref cfg 6))
(define (council-synthesis-prompt cfg) (list-ref cfg 7))
(define (council-require-consensus cfg) (list-ref cfg 8))
(define (council-timeout cfg) (list-ref cfg 9))

;;; ============================================================
;;; Council Result Types
;;; ============================================================

;;; make-council-result : Fields -> CouncilResult
(define (make-council-result responses
                             synthesis
                             consensus-reached
                             dissenting-views
                             vote-counts
                             round-history)
  (list 'council-result
        responses synthesis consensus-reached
        dissenting-views vote-counts round-history))

;;; council-result? : Any -> Boolean
(define (council-result? x)
  (and (pair? x) (eq? (car x) 'council-result)))

;;; Result accessors
(define (result-responses r) (list-ref r 1))
(define (result-synthesis r) (list-ref r 2))
(define (result-consensus r) (list-ref r 3))
(define (result-dissent r) (list-ref r 4))
(define (result-votes r) (list-ref r 5))
(define (result-history r) (list-ref r 6))

;;; ============================================================
;;; Sequential Council (Round-Robin)
;;; ============================================================
;;;
;;; Models take turns, each seeing all prior responses.
;;; Useful for building on each other's ideas.

;;; council-sequential : List Symbol -> Nat -> Symbol -> Stage ctx String CouncilResult
;;; models: list of model names
;;; rounds: number of rounds
;;; moderator: model to synthesize at end
(define (council-sequential models rounds moderator)
  (let ([default-prompts
          (map (lambda (r)
                       (cond
                        [(= r 1) "Share your initial position on: ${topic}"]
                        [(= r 2) "Respond to the other positions. Do you agree or disagree? Why?"]
                        [else "Seek synthesis or clarify remaining disagreements."]))
               (iota rounds 1))])
       (council-sequential-with-prompts models rounds moderator default-prompts)))

;;; council-sequential-with-prompts : List Symbol -> Nat -> Symbol -> List String -> Stage
(define (council-sequential-with-prompts models rounds moderator round-prompts)
  (make-stage 'council-sequential
              (lambda (ctx input)
                      (list 'council-effect 'sequential
                            (make-council-config
                             models
                             'sequential
                             rounds
                             moderator
                             "${input}"
                             round-prompts
                             "Synthesize the discussion. Note areas of consensus and remaining disagreement."
                             #f
                             60000)
                            input))))

;;; ============================================================
;;; Parallel Council (Independent + Synthesize)
;;; ============================================================
;;;
;;; All models respond independently to same prompt.
;;; A synthesizer combines the responses.

;;; council-parallel : List Symbol -> Symbol -> Stage ctx String CouncilResult
;;; models: list of model names
;;; synthesizer: model to combine responses
(define (council-parallel models synthesizer)
  (council-parallel-with-prompt models synthesizer
                                "Synthesize these independent perspectives into a coherent summary."))

;;; council-parallel-with-prompt : List Symbol -> Symbol -> String -> Stage
(define (council-parallel-with-prompt models synthesizer synthesis-prompt)
  (make-stage 'council-parallel
              (lambda (ctx input)
                      (list 'council-effect 'parallel
                            (make-council-config
                             models
                             'parallel
                             1
                             synthesizer
                             "${input}"
                             '("Share your perspective on: ${topic}")
                             synthesis-prompt
                             #f
                             60000)
                            input))))

;;; ============================================================
;;; Voting Council
;;; ============================================================
;;;
;;; Models vote on options, with optional discussion.

;;; council-vote : List Symbol -> List String -> Stage ctx String (Symbol . Alist)
;;; Returns (winner . vote-counts)
(define (council-vote models options)
  (make-stage 'council-vote
              (lambda (ctx input)
                      (list 'council-effect 'vote
                            (make-council-config
                             models
                             'vote
                             1
                             #f
                             "${input}"
                             (list (string-append
                                    "Choose one of these options and explain briefly:\n"
                                    (apply string-append
                                           (map (lambda (i opt)
                                                        (string-append "  " (number->string i) ". " opt "\n"))
                                                (iota (length options) 1)
                                                options))))
                             #f
                             #f
                             30000)
                            input))))

;;; ============================================================
;;; Debate Council
;;; ============================================================
;;;
;;; Two models take opposing positions, third judges.

;;; council-debate : Symbol -> Symbol -> Symbol -> Stage ctx String CouncilResult
;;; pro-model, con-model, judge-model
(define (council-debate pro-model con-model judge-model)
  (make-stage 'council-debate
              (lambda (ctx input)
                      (list 'council-effect 'debate
                            (make-council-config
                             (list pro-model con-model judge-model)
                             'debate
                             3  ; Opening, rebuttal, closing
                             judge-model
                             "${input}"
                             '("Opening: Make your strongest case for your assigned position."
                               "Rebuttal: Respond to your opponent's arguments."
                               "Closing: Summarize your position and why it should prevail.")
                             "Judge the debate. Who made the stronger argument and why?"
                             #f
                             90000)
                            input))))

;;; ============================================================
;;; Consensus Seeking Council
;;; ============================================================
;;;
;;; Continue until consensus or max rounds.

;;; council-consensus : List Symbol -> Nat -> Stage ctx String CouncilResult
;;; models: participating models
;;; max-rounds: maximum rounds before giving up
(define (council-consensus models max-rounds)
  (make-stage 'council-consensus
              (lambda (ctx input)
                      (list 'council-effect 'consensus
                            (make-council-config
                             models
                             'consensus
                             max-rounds
                             (car models)  ; First model moderates
                             "${input}"
                             '("Share your position on this question."
                               "Do you agree with any of the other positions? What would change your mind?"
                               "Can we find common ground? What do we all agree on?")
                             "Summarize areas of consensus and remaining disagreement."
                             #t  ; Require consensus
                             60000)
                            input))))

;;; ============================================================
;;; Council Helpers
;;; ============================================================

;;; with-council-moderator : Symbol -> Stage ctx i CouncilResult -> Stage ctx i CouncilResult
;;; Override the moderator for a council stage.
(define (with-council-moderator moderator council-stage)
  (make-stage 'with-moderator
              (lambda (ctx input)
                      (let ([result (run-stage council-stage ctx input)])
                           (if (and (pair? result) (eq? (car result) 'council-effect))
                               (let ([effect-type (list-ref result 1)]
                                     [config (list-ref result 2)]
                                     [topic (list-ref result 3)])
                                    (list 'council-effect effect-type
                                          (make-council-config
                                           (council-models config)
                                           (council-mode config)
                                           (council-rounds config)
                                           moderator  ; Override
                                           (council-topic-template config)
                                           (council-round-prompts config)
                                           (council-synthesis-prompt config)
                                           (council-require-consensus config)
                                           (council-timeout config))
                                          topic))
                               result)))))

;;; with-council-timeout : Nat -> Stage ctx i CouncilResult -> Stage ctx i CouncilResult
;;; Set timeout per model (milliseconds).
(define (with-council-timeout timeout-ms council-stage)
  (make-stage 'with-timeout
              (lambda (ctx input)
                      (let ([result (run-stage council-stage ctx input)])
                           (if (and (pair? result) (eq? (car result) 'council-effect))
                               (let ([effect-type (list-ref result 1)]
                                     [config (list-ref result 2)]
                                     [topic (list-ref result 3)])
                                    (list 'council-effect effect-type
                                          (make-council-config
                                           (council-models config)
                                           (council-mode config)
                                           (council-rounds config)
                                           (council-moderator config)
                                           (council-topic-template config)
                                           (council-round-prompts config)
                                           (council-synthesis-prompt config)
                                           (council-require-consensus config)
                                           timeout-ms)  ; Override
                                          topic))
                               result)))))

;;; ============================================================
;;; Council Result Extractors
;;; ============================================================

;;; extract-consensus : CouncilResult -> Maybe String
;;; Get consensus text if reached.
(define (extract-consensus result)
  (if (and (council-result? result) (result-consensus result))
      (result-synthesis result)
      #f))

;;; extract-dissent : CouncilResult -> List (Model . String)
;;; Get dissenting views.
(define (extract-dissent result)
  (if (council-result? result)
      (result-dissent result)
      '()))

;;; count-votes : CouncilResult -> Alist (Option . Nat)
;;; Get vote counts.
(define (count-votes result)
  (if (council-result? result)
      (result-votes result)
      '()))

;;; majority-position : CouncilResult -> Maybe String
;;; Get the majority position if one exists.
(define (majority-position result)
  (if (council-result? result)
      (let ([votes (result-votes result)])
           (if (null? votes)
               #f
               (let* ([sorted (sort (lambda (a b) (> (cdr a) (cdr b))) votes)]
                      [top (car sorted)]
                      [total (apply + (map cdr votes))])
                     (if (> (cdr top) (/ total 2))
                         (car top)
                         #f))))
      #f))

;;; ============================================================
;;; Common Council Patterns
;;; ============================================================

;;; architectural-council : Stage ctx String CouncilResult
;;; Standard council for architecture decisions.
(define architectural-council
  (council-sequential '(opus gemini-3) 3 'opus))

;;; quick-review-council : Stage ctx String CouncilResult
;;; Fast parallel review for code/text.
(define quick-review-council
  (council-parallel '(sonnet gemini-3 kimi) 'opus))

;;; diverse-perspectives : Stage ctx String CouncilResult
;;; Sample from many model families.
(define diverse-perspectives
  (council-parallel
   '(opus sonnet haiku gemini-3 kimi)
   'opus))

;;; technical-debate : Symbol -> Stage ctx String CouncilResult
;;; Debate a technical topic between Opus and Gemini.
(define (technical-debate topic-perspective)
  (council-debate 'opus 'gemini-3 'sonnet))

;;; ============================================================
;;; Council as Pipeline Stage
;;; ============================================================

;;; council : CouncilConfig -> Stage ctx String CouncilResult
;;; General council constructor from config.
(define (council config)
  (make-stage 'council
              (lambda (ctx input)
                      (list 'council-effect (council-mode config) config input))))

;;; council-then : Stage ctx String CouncilResult -> (CouncilResult -> Stage ctx CouncilResult o) -> Stage ctx String o
;;; Chain council result to another stage.
(define (council-then council-stage handler)
  (stage-bind council-stage
              (lambda (result)
                      (handler result))))

;;; council-on-consensus : Stage ctx String CouncilResult -> Stage ctx CouncilResult o -> Stage ctx String (Either CouncilResult o)
;;; Execute continuation only if consensus reached.
(define (council-on-consensus council-stage continuation)
  (stage-bind council-stage
              (lambda (result)
                      (if (and (council-result? result) (result-consensus result))
                          (stage-map right (run-stage continuation empty-context result))
                          (stage-pure (left result))))))

;;; council-escalate-on-no-consensus : Stage ctx String CouncilResult -> Symbol -> Stage ctx String CouncilResult
;;; Create bead if no consensus reached.
(define (council-escalate-on-no-consensus council-stage escalation-channel)
  (stage-bind council-stage
              (lambda (result)
                      (if (and (council-result? result) (not (result-consensus result)))
                          (stage->>>
                           (forum-post escalation-channel
                                       "Council needs human input"
                                       (result-synthesis result))
                           (stage-pure result))
                          (stage-pure result)))))

;;; ============================================================
;;; Council Effect Detection
;;; ============================================================

;;; council-effect? : Any -> Boolean
(define (council-effect? x)
  (and (pair? x) (eq? (car x) 'council-effect)))

;;; council-effect-mode : CouncilEffect -> Symbol
(define (council-effect-mode e) (list-ref e 1))

;;; council-effect-config : CouncilEffect -> CouncilConfig
(define (council-effect-config e) (list-ref e 2))

;;; council-effect-topic : CouncilEffect -> String
(define (council-effect-topic e) (list-ref e 3))
