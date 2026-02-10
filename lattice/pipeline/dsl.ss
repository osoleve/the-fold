(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'pipeline/stage)
(require 'pipeline/effects)
(require 'pipeline/context)
(require 'council)

(doc 'module 'pipeline/dsl)
(doc 'description "User-Facing Pipeline DSL. Provides a clean syntax for defining pipelines. Pipelines are still S-expressions (homoiconic) but with convenient constructors and combinators.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'features "define-pipeline (Named pipeline definition); stage (Named stage wrapper); config (Pipeline configuration); Operator aliases for composition; Common patterns as functions")
(doc 'dependencies '(pipeline/stage.ss pipeline/effects.ss pipeline/context.ss pipeline/council.ss))

(doc 'section 'pipeline-definition)

(doc 'type '(-> Symbol Alist Stage PipelineDef))
(doc 'description "Create a named pipeline definition")
(define (define-pipeline* name config stage)
  (make-pipeline-def name stage config))

;;; Example usage:
;;; (define my-pipeline
;;;   (define-pipeline* 'my-pipeline
;;;     '((model . sonnet) (fuel . 10000))
;;;     (>>> (stage-pure "input")
;;;          (llm 'sonnet "Process: ${input}"))))

(doc 'section 'stage-naming-tracing)

(doc 'type '(-> Symbol Stage Stage))
(doc 'description "Wrap a stage with a name for logging/debugging")
(define (named-stage name stage)
  (make-stage name (stage-run-fn stage)))

(doc stage* 'type '(-> Symbol Stage Stage))
(doc stage* 'description "Alias for named-stage")
(define stage* named-stage)

(doc 'section 'configuration-helpers)

(doc 'type 'Alist)
(doc 'description "Just an alias to make config clear in pipeline defs")
(define (config . pairs)
  pairs)

(doc 'type '(-> Symbol Stage Stage))
(doc 'description "Set default model for LLM stages")
(define (with-model model stage)
  (stage-local
   (lambda (ctx)
           (ctx-extend-env ctx (list (cons 'default-model model))))
   stage))

(doc 'type '(-> Nat Stage Stage))
(doc 'description "Set fuel limit")
(define (with-fuel fuel stage)
  (stage-local
   (lambda (ctx)
           (ctx-with-fuel ctx fuel))
   stage))

(doc 'type '(-> Persona Stage Stage))
(doc 'description "Set persona for LLM stages")
(define (with-persona persona stage)
  (stage-local
   (lambda (ctx)
           (ctx-with-persona ctx persona))
   stage))

(doc 'type '(-> Alist Stage Stage))
(doc 'description "Extend environment")
(define (with-env bindings stage)
  (stage-local
   (lambda (ctx)
           (ctx-extend-env ctx bindings))
   stage))

(doc 'section 'operator-aliases)
(doc 'description "Infix-style operators (using >>> etc. directly from stage.ss). These are re-exported for convenience.")

(doc 'type '(-> Stage Stage Stage))
(doc 'description "Sequential composition (alias for >>>)")
(define --> stage->>>)

(doc <-- 'type '(-> Stage Stage Stage))
(doc <-- 'description "Reverse sequential composition")
(define <-- stage-<<<)

(doc 'type '(-> a (-> a Stage) Stage))
(doc 'description "Pipe value into stage constructor")
(define (pipe-into value stage-fn)
  (stage-fn value))

(doc 'section 'common-stage-patterns)

(doc 'type '(Stage ctx String Any))
(doc 'description "Parse JSON string to S-expression")
(define parse-json
  (stage-arr
   (lambda (s)
           ;; Simple JSON-to-sexpr (interpreter provides real impl)
           (list 'json-parse s))))

(doc to-json 'type '(Stage ctx Any String))
(doc to-json 'description "Convert S-expression to JSON string")
(define to-json
  (stage-arr
   (lambda (x)
           (list 'json-stringify x))))

(doc split-lines 'type '(Stage ctx String (List String)))
(doc split-lines 'description "Split string into lines")
(define split-lines
  (stage-arr
   (lambda (s)
           ;; Simple split (interpreter provides real impl)
           (list 'split-lines s))))

(doc join-lines 'type '(Stage ctx (List String) String))
(doc join-lines 'description "Join list of strings with newlines")
(define join-lines
  (stage-arr
   (lambda (lines)
           (list 'join-lines lines))))

(doc 'section 'flow-control-patterns)

(doc 'type '(-> Stage Stage Stage))
(doc 'description "Run second stage only if first succeeds")
(define (on-success s1 s2)
  (stage->>> s1 s2))

(doc 'type '(-> Stage Stage Stage))
(doc 'description "Run fallback if primary fails")
(define (on-failure primary fallback)
  (stage-catch (lambda (err) fallback) primary))

(doc 'type '(-> (List Stage) Stage))
(doc 'description "Try stages in order until one succeeds")
(define (try-all stages)
  (if (null? stages)
      (stage-fail 'all-failed "All stages failed")
      (on-failure (car stages)
                  (try-all (cdr stages)))))

(doc 'type '(-> Nat Stage Stage))
(doc 'description "Retry stage up to n times on failure")
(define (retry n stage)
  (if (= n 0)
      stage
      (on-failure stage (retry (- n 1) stage))))

(doc 'type '(-> RetryPolicy Stage Stage))
(doc 'description "Apply retry policy to stage. Returns stage-retry result with delay for interpreter to handle, since Core is pure and cannot perform IO like sleeping.")
(define (with-retry-policy policy stage)
  (let ([max-attempts (retry-max-attempts policy)]
        [delay-fn (retry-delay-fn policy)]
        [retry-pred (retry-on-predicate policy)]
        [exhaust-action (retry-on-exhaust policy)])
       (make-stage 'with-retry
                   (lambda (ctx input)
                           ;; Retrieve current attempt from context env, default to 0
                           (let* ([attempt (or (ctx-env-ref ctx 'retry-attempt) 0)]
                                  [result (run-stage stage ctx input)])
                                 (cond
                                  ;; Success - return result
                                  [(stage-ok? result) result]
                                  ;; Error that should be retried
                                  [(and (stage-err? result)
                                        (< attempt max-attempts)
                                        (retry-pred (stage-err-code result)))
                                   ;; Return retry result with delay for interpreter
                                   ;; Interpreter will wait, increment attempt in context, and re-run
                                   (stage-retry
                                    (format "Retry attempt ~a/~a: ~a"
                                            (+ attempt 1)
                                            max-attempts
                                            (stage-err-message result))
                                    (delay-fn attempt))]
                                  ;; Exhausted or non-retryable error
                                  [else
                                   (case exhaust-action
                                         [(fail) result]
                                         [(halt) (stage-halt
                                                  (format "Retry exhausted after ~a attempts: ~a"
                                                          attempt
                                                          (if (stage-err? result)
                                                              (stage-err-message result)
                                                              "unknown error")))]
                                         [else result])]))))))

(doc 'type '(-> (-> a Boolean) Stage Stage))
(doc 'description "Only proceed if condition met")
(define (gate pred stage)
  (stage-if pred stage (stage-skip-with "Gate condition not met")))

(doc 'section 'collection-processing)

(doc 'type '(-> (Stage ctx a b) (Stage ctx (List a) (List b))))
(doc 'description "Apply stage to each element")
(define (map-stage stage)
  (make-stage 'map
              (lambda (ctx input)
                      (let loop ([items input] [results '()])
                           (if (null? items)
                               (stage-ok (reverse results))
                               (let ([r (run-stage stage ctx (car items))])
                                    (if (stage-ok? r)
                                        (loop (cdr items) (cons (stage-result-value r) results))
                                        r)))))))

(doc 'type '(-> (-> a Boolean) (Stage ctx (List a) (List a))))
(doc 'description "Filter list by predicate")
(define (filter-stage pred)
  (stage-arr (lambda (items) (filter pred items))))

(doc 'type '(-> (-> b a (Stage ctx a b)) b (Stage ctx (List a) b)))
(doc 'description "Reduce list with stage")
(define (reduce-stage f init)
  (make-stage 'reduce
              (lambda (ctx input)
                      (let loop ([items input] [acc init])
                           (if (null? items)
                               (stage-ok acc)
                               (let ([r (run-stage (f acc (car items)) ctx (car items))])
                                    (if (stage-ok? r)
                                        (loop (cdr items) (stage-result-value r))
                                        r)))))))

(doc 'type '(-> Nat (Stage ctx (List a) (List a))))
(doc 'description "Take first n elements")
(define (take-stage n)
  (stage-arr (lambda (items) (take n items))))

(doc 'type '(-> Nat (Stage ctx (List a) (List a))))
(doc 'description "Drop first n elements")
(define (drop-stage n)
  (stage-arr (lambda (items) (drop n items))))

;; take and drop are provided by prelude (via context.ss)

(doc 'section 'parallel-execution)

(doc 'type '(-> (List Stage) (Stage ctx a (List b))))
(doc 'description "Run multiple stages in parallel on same input")
(define (parallel stages)
  (if (null? stages)
      (stage-pure '())
      (stage-map
       (lambda (pair)
               (cons (car pair) (cdr pair)))
       (stage-&&& (car stages)
                  (parallel (cdr stages))))))

(doc 'type '(-> (List Stage) (Stage ctx a b)))
(doc 'description "Return first successful result")
(define (race stages)
  (make-stage 'race
              (lambda (ctx input)
                      ;; Interpreter handles actual racing
                      (list 'stage-effect 'race stages input))))

(doc 'type '(-> (List Stage) (Stage ctx a (List b))))
(doc 'description "All stages must succeed")
(define (all-of stages)
  (stage-sequence stages))

(doc any-of 'type '(-> (List Stage) (Stage ctx a b)))
(doc any-of 'description "At least one stage must succeed")
(define any-of race)

(doc 'section 'fsm-pipeline-pattern)

(doc 'type '(-> Alist Symbol Stage))
(doc 'description "Create FSM-based pipeline. Each state maps to (stage . ((result-pred . next-state) ...))")
(define (fsm-pipeline states initial-state accepting-states)
  (make-stage 'fsm
              (lambda (ctx input)
                      (let loop ([current-state initial-state]
                                 [data input]
                                 [fuel 1000])
                           (if (= fuel 0)
                               (stage-err 'fsm-exhausted "FSM exceeded fuel limit" current-state)
                               (if (memq current-state accepting-states)
                                   (stage-ok data)
                                   (let ([state-spec (assq current-state states)])
                                        (if (not state-spec)
                                            (stage-err 'fsm-invalid-state
                                                       (format "Unknown state: ~a" current-state)
                                                       current-state)
                                            (let* ([stage (cadr state-spec)]
                                                   [transitions (cddr state-spec)]
                                                   [result (run-stage stage ctx data)])
                                                  (cond
                                                   [(stage-ok? result)
                                                    (let ([next (find-transition 'ok transitions)])
                                                         (if next
                                                             (loop next (stage-result-value result) (- fuel 1))
                                                             (stage-ok (stage-result-value result))))]
                                                   [(stage-err? result)
                                                    (let ([next (find-transition (stage-err-code result) transitions)])
                                                         (if next
                                                             (loop next data (- fuel 1))
                                                             result))]
                                                   [else result]))))))))))

(doc 'type '(-> Symbol Alist (Maybe Symbol)))
(doc 'description "Find transition target for a given result type")
(define (find-transition result-type transitions)
  (let ([entry (assq result-type transitions)])
       (if entry (cdr entry) #f)))

;;; ====
;;; Convenience Constructors
;;; ====

(doc ask-llm 'type '(-> Symbol String (Stage ctx String String)))
(doc ask-llm 'description "Simple LLM call")
(define ask-llm llm)

(doc run-fold 'type '(-> String (Stage ctx i Any)))
(doc run-fold 'description "Run Fold expression")
(define run-fold fold-eval)

(doc run-shell 'type '(-> String (Stage ctx i String)))
(doc run-shell 'description "Run shell command")
(define run-shell shell)

(doc 'type '(-> Symbol String (Stage ctx String Hash)))
(doc 'description "Post to forum channel")
(define (post-to channel title-template)
  (make-stage 'post-to
              (lambda (ctx input)
                      (run-stage (forum-post channel title-template input) ctx input))))

(doc fetch-url 'type '(-> String (Stage ctx i String)))
(doc fetch-url 'description "Fetch URL content")
(define fetch-url http-get)

;;; ====
;;; Pipeline Composition Helpers
;;; ====

(doc 'type '(-> Stage ... Stage))
(doc 'description "Chain multiple stages sequentially")
(define (chain . stages)
  (if (null? stages)
      stage-read
      (if (null? (cdr stages))
          (car stages)
          (stage->>> (car stages) (apply chain (cdr stages))))))

(doc branch 'type '(-> (-> a Boolean) Stage Stage Stage))
(doc branch 'description "Branch based on predicate")
(define branch stage-if)

(doc switch 'type '(-> (List (Pair (-> a Boolean) Stage)) Stage Stage))
(doc switch 'description "Multiple branches")
(define switch stage-case)

(doc tap 'type '(-> (-> a Void) Stage))
(doc tap 'description "Side effect without changing value")
(define tap stage-tap)

;;; ====
;;; Logging Helpers
;;; ====

(doc log 'type '(-> String (Stage ctx a a)))
(doc log 'description "Log message")
(define log log-info)

(doc debug 'type '(-> String (Stage ctx a a)))
(doc debug 'description "Debug log")
(define debug log-debug)

(doc warn 'type '(-> String (Stage ctx a a)))
(doc warn 'description "Warning log")
(define warn log-warn)

;;; ====
;;; Checkpoint Helpers
;;; ====

(doc save 'type '(-> Symbol (Stage ctx a a)))
(doc save 'description "Save checkpoint")
(define save checkpoint)

;;; ====
;;; RLM Convenience
;;; ====

(doc ask-rlm 'type '(-> RlmConfig String Nat (Stage ctx String Any)))
(doc ask-rlm 'description "Run an RLM agent loop. Input is the task; output is the final answer. Analogous to ask-llm but iterative with CAS-backed context.")
(define ask-rlm rlm)

(doc load-checkpoint 'type '(-> Symbol (Stage ctx i Any)))
(doc load-checkpoint 'description "Load checkpoint")
(define load-checkpoint restore)

;;; ====
;;; Full Pipeline Example (as documentation)
;;; ====
;;;
;;; Example: ArXiv Pipeline
;;;
;;; (define arxiv-pipeline
;;;   (define-pipeline* 'arxiv-ingest
;;;     (config (cons 'model 'sonnet)
;;;             (cons 'fuel 10000)
;;;             (cons 'schedule (make-cron-schedule "0 6 * * *")))
;;;     (chain
;;;       ;; Fetch recent papers
;;;       (named-stage 'fetch
;;;         (run-shell "arxiv-fetch --category cs.AI --days 1"))
;;;
;;;       ;; Parse JSON response
;;;       (named-stage 'parse parse-json)
;;;
;;;       ;; Filter by relevance (parallel scoring)
;;;       (named-stage 'score
;;;         (map-stage
;;;           (stage-&&&
;;;             (ask-llm 'haiku "Rate 1-10 for AI safety relevance: ${input}")
;;;             (ask-llm 'haiku "Rate 1-10 for technical depth: ${input}"))))
;;;
;;;       ;; Keep high-scoring papers
;;;       (named-stage 'filter
;;;         (filter-stage (lambda (paper)
;;;                         (> (+ (car (cdr paper)) (cdr (cdr paper))) 12))))
;;;
;;;       ;; Summarize each paper
;;;       (named-stage 'summarize
;;;         (map-stage
;;;           (ask-llm 'sonnet "Summarize key contributions: ${input}")))
;;;
;;;       ;; Synthesize into forum post
;;;       (named-stage 'synthesize
;;;         (ask-llm 'opus "Write forum post about these papers: ${input}"))
;;;
;;;       ;; Post to forum
;;;       (named-stage 'post
;;;         (post-to 'engineering "ArXiv Digest"))
;;;
;;;       ;; Checkpoint
;;;       (save 'completed))))

;;; ====
;;; Export Summary
;;; ====
;;;
;;; Pipeline Definition:
;;;   define-pipeline*, named-stage, stage*, config
;;;
;;; Configuration:
;;;   with-model, with-fuel, with-persona, with-env
;;;
;;; Operators:
;;;   -->, <--, |>
;;;
;;; Patterns:
;;;   parse-json, to-json, split-lines, join-lines
;;;
;;; Flow Control:
;;;   on-success, on-failure, try-all, retry, with-retry-policy, gate
;;;
;;; Collections:
;;;   map-stage, filter-stage, reduce-stage, take-stage, drop-stage
;;;
;;; Parallel:
;;;   parallel, race, all-of, any-of
;;;
;;; FSM:
;;;   fsm-pipeline
;;;
;;; Convenience:
;;;   ask-llm, run-fold, run-shell, post-to, fetch-url
;;;   chain, branch, switch, tap
;;;
;;; Logging:
;;;   log, debug, warn
;;;
;;; Checkpoints:
;;;   save, load-checkpoint
