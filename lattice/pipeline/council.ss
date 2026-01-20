(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")

(doc 'module 'pipeline/council)
(doc 'description "Multi-Model Council Primitive. Councils enable structured deliberation between multiple LLMs. Two modes: Sequential rounds (Models respond in turns, seeing prior responses), Parallel synthesis (Models respond independently, then synthesize)")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Effect interpretation happens in boundary/pipeline/interpreter.ss")
(doc 'features "Sequential round-robin deliberation; Parallel independent responses; Synthesis/moderator stage; Consensus detection; Vote counting")
(doc 'dependencies '(pipeline/stage.ss pipeline/effects.ss pipeline/context.ss))

(doc 'type '(-> Nat Nat (List Nat)))
(doc 'description "Helper: range from start to start+count-1")
(define (range-from start count)
  (let loop ([i 0] [acc '()])
       (if (= i count)
           (reverse acc)
           (loop (+ i 1) (cons (+ start i) acc)))))

(doc 'section 'council-configuration)

(doc 'type '(-> (List Symbol) Symbol Nat Symbol String (List String) String Boolean Nat CouncilConfig))
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

;;; council-models : CouncilConfig → (List Symbol)
(define (council-models cfg) (list-ref cfg 1))

;;; council-mode : CouncilConfig → Symbol
(define (council-mode cfg) (list-ref cfg 2))

;;; council-rounds : CouncilConfig → Nat
(define (council-rounds cfg) (list-ref cfg 3))

;;; council-moderator : CouncilConfig → Symbol
(define (council-moderator cfg) (list-ref cfg 4))

;;; council-topic-template : CouncilConfig → String
(define (council-topic-template cfg) (list-ref cfg 5))

;;; council-round-prompts : CouncilConfig → (List String)
(define (council-round-prompts cfg) (list-ref cfg 6))

;;; council-synthesis-prompt : CouncilConfig → String
(define (council-synthesis-prompt cfg) (list-ref cfg 7))

;;; council-require-consensus : CouncilConfig → Bool
(define (council-require-consensus cfg) (list-ref cfg 8))

;;; council-timeout : CouncilConfig → Nat
(define (council-timeout cfg) (list-ref cfg 9))

(doc 'section 'council-result-types)

(doc 'type '(-> Alist String Boolean List Alist List CouncilResult))
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

;;; result-responses : CouncilResult → (Alist Symbol String)
(define (result-responses r) (list-ref r 1))

;;; result-synthesis : CouncilResult → String
(define (result-synthesis r) (list-ref r 2))

;;; result-consensus : CouncilResult → Bool
(define (result-consensus r) (list-ref r 3))

;;; result-dissent : CouncilResult → (List (Pair Symbol String))
(define (result-dissent r) (list-ref r 4))

;;; result-votes : CouncilResult → (Alist String Nat)
(define (result-votes r) (list-ref r 5))

;;; result-history : CouncilResult → (List (Alist Symbol String))
(define (result-history r) (list-ref r 6))

(doc 'section 'sequential-council)
(doc 'description "Round-Robin. Models take turns, each seeing all prior responses. Useful for building on each other's ideas.")

(doc 'type '(-> (List Symbol) Nat Symbol (Stage ctx String CouncilResult)))
(doc 'param 'models "list of model names")
(doc 'param 'rounds "number of rounds")
(doc 'param 'moderator "model to synthesize at end")
(define (council-sequential models rounds moderator)
  (let ([default-prompts
          (map (lambda (r)
                       (cond
                        [(= r 1) "Share your initial position on: ${topic}"]
                        [(= r 2) "Respond to the other positions. Do you agree or disagree? Why?"]
                        [else "Seek synthesis or clarify remaining disagreements."]))
               (range-from 1 rounds))])
       (council-sequential-with-prompts models rounds moderator default-prompts)))

;;; council-sequential-with-prompts : (List Symbol) × Nat × Symbol × (List String) → (Stage ctx String CouncilResult)
(define (council-sequential-with-prompts models rounds moderator round-prompts)
  (make-stage 'council-sequential
              (lambda (ctx input)
                      (list 'stage-effect 'council
                            (list 'sequential
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
                                  input)))))

(doc 'section 'parallel-council)
(doc 'description "Independent + Synthesize. All models respond independently to same prompt. A synthesizer combines the responses.")

(doc 'type '(-> (List Symbol) Symbol (Stage ctx String CouncilResult)))
(doc 'param 'models "list of model names")
(doc 'param 'synthesizer "model to combine responses")
(define (council-parallel models synthesizer)
  (council-parallel-with-prompt models synthesizer
                                "Synthesize these independent perspectives into a coherent summary."))

;;; council-parallel-with-prompt : (List Symbol) × Symbol × String → (Stage ctx String CouncilResult)
(define (council-parallel-with-prompt models synthesizer synthesis-prompt)
  (make-stage 'council-parallel
              (lambda (ctx input)
                      (list 'stage-effect 'council
                            (list 'parallel
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
                                  input)))))

;;; ====
;;; Voting Council
;;; ====
;;;
;;; Models vote on options, with optional discussion.

;;; council-vote : (List Symbol) × (List String) → (Stage ctx String (Pair Symbol (Alist String Nat)))
;;; Returns (winner . vote-counts)
(define (council-vote models options)
  (make-stage 'council-vote
              (lambda (ctx input)
                      (list 'stage-effect 'council
                            (list 'vote
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
                                                      (range-from 1 (length options))
                                                      options))))
                                   #f
                                   #f
                                   30000)
                                  input)))))

;;; ====
;;; Debate Council
;;; ====
;;;
;;; Two models take opposing positions, third judges.

;;; council-debate : Symbol × Symbol × Symbol → (Stage ctx String CouncilResult)
;;; pro-model, con-model, judge-model
(define (council-debate pro-model con-model judge-model)
  (make-stage 'council-debate
              (lambda (ctx input)
                      (list 'stage-effect 'council
                            (list 'debate
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
                                  input)))))

;;; ====
;;; Consensus Seeking Council
;;; ====
;;;
;;; Continue until consensus or max rounds.

;;; council-consensus : (List Symbol) × Nat → (Stage ctx String CouncilResult)
;;; models: participating models
;;; max-rounds: maximum rounds before giving up
(define (council-consensus models max-rounds)
  (make-stage 'council-consensus
              (lambda (ctx input)
                      (list 'stage-effect 'council
                            (list 'consensus
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
                                  input)))))

;;; ====
;;; Council Helpers
;;; ====

;;; with-council-moderator : Symbol × (Stage ctx α CouncilResult) → (Stage ctx α CouncilResult)
;;; Override the moderator for a council stage.
(define (with-council-moderator moderator council-stage)
  (make-stage 'with-moderator
              (lambda (ctx input)
                      (let ([result (run-stage council-stage ctx input)])
                           (if (council-effect? result)
                               (let* ([payload (stage-effect-payload result)]
                                      [effect-type (car payload)]
                                      [config (cadr payload)]
                                      [topic (caddr payload)])
                                     (list 'stage-effect 'council
                                           (list effect-type
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
                                                 topic)))
                               result)))))

;;; with-council-timeout : Nat × (Stage ctx α CouncilResult) → (Stage ctx α CouncilResult)
;;; Set timeout per model (milliseconds).
(define (with-council-timeout timeout-ms council-stage)
  (make-stage 'with-timeout
              (lambda (ctx input)
                      (let ([result (run-stage council-stage ctx input)])
                           (if (council-effect? result)
                               (let* ([payload (stage-effect-payload result)]
                                      [effect-type (car payload)]
                                      [config (cadr payload)]
                                      [topic (caddr payload)])
                                     (list 'stage-effect 'council
                                           (list effect-type
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
                                                 topic)))
                               result)))))

;;; ====
;;; Council Result Extractors
;;; ====

;;; extract-consensus : CouncilResult → (Maybe String)
;;; Get consensus text if reached.
(define (extract-consensus result)
  (if (and (council-result? result) (result-consensus result))
      (result-synthesis result)
      #f))

;;; extract-dissent : CouncilResult → (List (Pair Symbol String))
;;; Get dissenting views.
(define (extract-dissent result)
  (if (council-result? result)
      (result-dissent result)
      '()))

;;; count-votes : CouncilResult → (Alist String Nat)
;;; Get vote counts.
(define (count-votes result)
  (if (council-result? result)
      (result-votes result)
      '()))

;;; majority-position : CouncilResult → (Maybe String)
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

(doc 'section 'common-patterns)

(doc 'type '(Stage ctx String CouncilResult))
(doc 'description "Standard council for architecture decisions")
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

;;; technical-debate : Symbol → (Stage ctx String CouncilResult)
;;; Debate a technical topic between Opus and Gemini.
(define (technical-debate topic-perspective)
  (council-debate 'opus 'gemini-3 'sonnet))

;;; ====
;;; Council as Pipeline Stage
;;; ====

;;; council : CouncilConfig → (Stage ctx String CouncilResult)
;;; General council constructor from config.
(define (council config)
  (make-stage 'council
              (lambda (ctx input)
                      (list 'stage-effect 'council
                            (list (council-mode config) config input)))))

;;; council-then : (Stage ctx String CouncilResult) × (CouncilResult → (Stage ctx CouncilResult β)) → (Stage ctx String β)
;;; Chain council result to another stage.
(define (council-then council-stage handler)
  (stage-bind council-stage
              (lambda (result)
                      (handler result))))

;;; council-on-consensus : (Stage ctx String CouncilResult) × (Stage ctx CouncilResult β) → (Stage ctx String (Either CouncilResult β))
;;; Execute continuation only if consensus reached.
(define (council-on-consensus council-stage continuation)
  (stage-bind council-stage
              (lambda (result)
                      (if (and (council-result? result) (result-consensus result))
                          (stage-map right (run-stage continuation empty-context result))
                          (stage-pure (left result))))))

;;; council-escalate-on-no-consensus : (Stage ctx String CouncilResult) × Symbol → (Stage ctx String CouncilResult)
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

;;; ====
;;; Council Effect Detection
;;; ====

;;; council-effect? : Any → Bool
;;; Checks for standard stage-effect with 'council type.
(define (council-effect? x)
  (and (stage-effect? x)
       (eq? (stage-effect-type x) 'council)))

;;; council-effect-mode : CouncilEffect → Symbol
;;; Payload structure: (list mode config topic)
(define (council-effect-mode e)
  (car (stage-effect-payload e)))

;;; council-effect-config : CouncilEffect → CouncilConfig
(define (council-effect-config e)
  (cadr (stage-effect-payload e)))

;;; council-effect-topic : CouncilEffect → String
(define (council-effect-topic e)
  (caddr (stage-effect-payload e)))
