;;; fabric/stitches/pipeline/stage.ss — Core Stage Algebra
;;;
;;; Stages are the building blocks of pipelines. A Stage transforms
;;; input to output within a context, producing a StageResult.
;;;
;;; Stage ctx i o = ctx -> i -> StageResult o
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;; Effect interpretation happens in thimble/pipeline/interpreter.ss
;;;
;;; Features:
;;;   - StageResult sum type (ok, err, retry, skip, halt, await)
;;;   - Stage constructors (pure, read, ask, local)
;;;   - Arrow-style composition (>>>, &&&, choice, ***)
;;;   - Conditional stages (if, case, when, unless)
;;;   - Monadic interface (bind, map, sequence)
;;;   - Effect staging (perform, for interpreter)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;
;;; Laws:
;;;   stage-pure x >>> f   = f applied to x
;;;   f >>> stage-pure id  = f
;;;   (f >>> g) >>> h      = f >>> (g >>> h)

(load "core/base/prelude.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; StageResult — Outcome of running a stage
;;; ============================================================

;;; stage-ok : a -> StageResult a
;;; Successful result with value.
(define (stage-ok value)
  (list 'stage-result 'ok value))

;;; stage-err : Symbol -> String -> Any -> StageResult a
;;; Failure with error code, message, and optional data.
(define (stage-err code message data)
  (list 'stage-result 'err code message data))

;;; stage-retry : String -> Nat -> StageResult a
;;; Request retry with reason and delay (milliseconds).
(define (stage-retry reason delay-ms)
  (list 'stage-result 'retry reason delay-ms))

;;; stage-skip : String -> StageResult a
;;; Skip this stage but continue pipeline.
(define (stage-skip reason)
  (list 'stage-result 'skip reason))

;;; stage-halt : String -> StageResult a
;;; Stop entire pipeline.
(define (stage-halt reason)
  (list 'stage-result 'halt reason))

;;; stage-await : Symbol -> StageResult a
;;; Suspend and wait for external signal by reference.
(define (stage-await ref)
  (list 'stage-result 'await ref))

;;; stage-result? : Any -> Boolean
(define (stage-result? x)
  (and (pair? x) (eq? (car x) 'stage-result)))

;;; stage-result-tag : StageResult a -> Symbol
(define (stage-result-tag r)
  (list-ref r 1))

;;; stage-result-value : StageResult a -> a
;;; Extract value from ok result.
(define (stage-result-value r)
  (list-ref r 2))

;;; stage-ok? : StageResult a -> Boolean
(define (stage-ok? r)
  (and (stage-result? r) (eq? (stage-result-tag r) 'ok)))

;;; stage-err? : StageResult a -> Boolean
(define (stage-err? r)
  (and (stage-result? r) (eq? (stage-result-tag r) 'err)))

;;; stage-retry? : StageResult a -> Boolean
(define (stage-retry? r)
  (and (stage-result? r) (eq? (stage-result-tag r) 'retry)))

;;; stage-skip? : StageResult a -> Boolean
(define (stage-skip? r)
  (and (stage-result? r) (eq? (stage-result-tag r) 'skip)))

;;; stage-halt? : StageResult a -> Boolean
(define (stage-halt? r)
  (and (stage-result? r) (eq? (stage-result-tag r) 'halt)))

;;; stage-await? : StageResult a -> Boolean
(define (stage-await? r)
  (and (stage-result? r) (eq? (stage-result-tag r) 'await)))

;;; Error accessors

;;; stage-err-code : (StageResult α) → Symbol
(define (stage-err-code r) (list-ref r 2))

;;; stage-err-message : (StageResult α) → String
(define (stage-err-message r) (list-ref r 3))

;;; stage-err-data : (StageResult α) → Any
(define (stage-err-data r) (list-ref r 4))

;;; Retry accessors

;;; stage-retry-reason : (StageResult α) → String
(define (stage-retry-reason r) (list-ref r 2))

;;; stage-retry-delay : (StageResult α) → Nat
(define (stage-retry-delay r) (list-ref r 3))

;;; ============================================================
;;; Stage Representation
;;; ============================================================
;;;
;;; A stage is:
;;;   ('stage name run-fn)
;;;
;;; Where run-fn : ctx -> input -> StageResult output

;;; make-stage : Symbol -> (ctx -> i -> StageResult o) -> Stage ctx i o
(define (make-stage name run-fn)
  (list 'stage name run-fn))

;;; stage? : Any -> Boolean
(define (stage? x)
  (and (pair? x) (eq? (car x) 'stage)))

;;; stage-name : Stage ctx i o -> Symbol
(define (stage-name s)
  (list-ref s 1))

;;; stage-run-fn : Stage ctx i o -> (ctx -> i -> StageResult o)
(define (stage-run-fn s)
  (list-ref s 2))

;;; run-stage : Stage ctx i o -> ctx -> i -> StageResult o
;;; Execute a stage on context and input.
(define (run-stage s ctx input)
  ((stage-run-fn s) ctx input))

;;; ============================================================
;;; Basic Stage Constructors
;;; ============================================================

;;; stage-pure : a -> Stage ctx i a
;;; Inject a constant value, ignoring input.
(define (stage-pure value)
  (make-stage 'pure
              (lambda (ctx input)
                      (stage-ok value))))

;;; stage-read : Stage ctx i i
;;; Pass input through unchanged.
(define stage-read
  (make-stage 'read
              (lambda (ctx input)
                      (stage-ok input))))

;;; stage-ask : Stage ctx i ctx
;;; Access the context.
(define stage-ask
  (make-stage 'ask
              (lambda (ctx input)
                      (stage-ok ctx))))

;;; stage-asks : (ctx -> a) -> Stage ctx i a
;;; Access part of the context.
(define (stage-asks f)
  (make-stage 'asks
              (lambda (ctx input)
                      (stage-ok (f ctx)))))

;;; stage-local : (ctx -> ctx') -> Stage ctx' i o -> Stage ctx i o
;;; Run stage with modified context.
(define (stage-local f stage)
  (make-stage 'local
              (lambda (ctx input)
                      (run-stage stage (f ctx) input))))

;;; stage-fail : Symbol -> String -> Stage ctx i o
;;; Always fail with given error.
(define (stage-fail code message)
  (make-stage 'fail
              (lambda (ctx input)
                      (stage-err code message '()))))

;;; stage-halt-with : String -> Stage ctx i o
;;; Always halt pipeline.
(define (stage-halt-with reason)
  (make-stage 'halt
              (lambda (ctx input)
                      (stage-halt reason))))

;;; stage-skip-with : String -> Stage ctx i i
;;; Skip this stage, pass input through.
(define (stage-skip-with reason)
  (make-stage 'skip
              (lambda (ctx input)
                      (stage-skip reason))))

;;; ============================================================
;;; Arrow-Style Composition
;;; ============================================================

;;; stage-compose : Stage ctx a b -> Stage ctx b c -> Stage ctx a c
;;; Sequential composition (left to right).
(define (stage-compose s1 s2)
  (make-stage 'seq
              (lambda (ctx input)
                      (let ([r1 (run-stage s1 ctx input)])
                           (if (stage-ok? r1)
                               (run-stage s2 ctx (stage-result-value r1))
                               r1)))))  ; Propagate non-ok results

;;; >>> : Stage ctx a b -> Stage ctx b c -> Stage ctx a c
;;; Sequential composition operator.
(define (stage->>> s1 s2)
  (stage-compose s1 s2))

;;; <<< : Stage ctx b c -> Stage ctx a b -> Stage ctx a c
;;; Reverse sequential composition.
(define (stage-<<< s2 s1)
  (stage-compose s1 s2))

;;; stage-first : Stage ctx a b -> Stage ctx (a . c) (b . c)
;;; Apply stage to first component of pair.
(define (stage-first s)
  (make-stage 'first
              (lambda (ctx input)
                      (let ([a (car input)]
                            [c (cdr input)]
                            [r (run-stage s ctx (car input))])
                           (if (stage-ok? r)
                               (stage-ok (cons (stage-result-value r) c))
                               r)))))

;;; stage-second : Stage ctx a b -> Stage ctx (c . a) (c . b)
;;; Apply stage to second component of pair.
(define (stage-second s)
  (make-stage 'second
              (lambda (ctx input)
                      (let* ([c (car input)]
                             [a (cdr input)]
                             [r (run-stage s ctx a)])
                            (if (stage-ok? r)
                                (stage-ok (cons c (stage-result-value r)))
                                r)))))

;;; stage-split : Stage ctx a b -> Stage ctx c d -> Stage ctx (a . c) (b . d)
;;; Apply two stages in parallel to pair components (***).
(define (stage-split s1 s2)
  (stage->>> (stage-first s1) (stage-second s2)))

;;; *** : Stage ctx a b -> Stage ctx c d -> Stage ctx (a . c) (b . d)
(define stage-*** stage-split)

;;; stage-fanout : Stage ctx a b -> Stage ctx a c -> Stage ctx a (b . c)
;;; Apply two stages to same input, pair results (&&&).
;;; Handles effects: if branches return effects, produces a combined fanout-effect.
(define (stage-fanout s1 s2)
  (make-stage 'fanout
              (lambda (ctx input)
                      (let ([r1 (run-stage s1 ctx input)]
                            [r2 (run-stage s2 ctx input)])
                           (cond
                            ;; Both pure values: pair them
                            [(and (stage-ok? r1) (stage-ok? r2))
                             (stage-ok (cons (stage-result-value r1)
                                             (stage-result-value r2)))]
                            ;; Error propagation (errors take priority)
                            [(stage-err? r1) r1]
                            [(stage-err? r2) r2]
                            ;; Halt propagation
                            [(stage-halt? r1) r1]
                            [(stage-halt? r2) r2]
                            ;; Both effects: combine into fanout-effect
                            [(and (stage-effect? r1) (stage-effect? r2))
                             (list 'stage-effect 'fanout
                                   (cons r1 r2)  ; payload is pair of effects
                                   input)]
                            ;; One effect, one ok: wrap in fanout-effect with value
                            [(and (stage-effect? r1) (stage-ok? r2))
                             (list 'stage-effect 'fanout
                                   (cons r1 (stage-result-value r2))
                                   input)]
                            [(and (stage-ok? r1) (stage-effect? r2))
                             (list 'stage-effect 'fanout
                                   (cons (stage-result-value r1) r2)
                                   input)]
                            ;; Effect with skip: effect takes priority
                            [(and (stage-effect? r1) (stage-skip? r2))
                             (list 'stage-effect 'fanout
                                   (cons r1 '())
                                   input)]
                            [(and (stage-skip? r1) (stage-effect? r2))
                             (list 'stage-effect 'fanout
                                   (cons '() r2)
                                   input)]
                            ;; If one skips, still return the other
                            [(and (stage-skip? r1) (stage-ok? r2))
                             (stage-ok (cons '() (stage-result-value r2)))]
                            [(and (stage-ok? r1) (stage-skip? r2))
                             (stage-ok (cons (stage-result-value r1) '()))]
                            [else
                             (stage-err 'fanout-failed
                                        "Parallel stages produced incompatible results"
                                        (list r1 r2))])))))

;;; &&& : Stage ctx a b -> Stage ctx a c -> Stage ctx a (b . c)
(define stage-&&& stage-fanout)

;;; ============================================================
;;; ArrowChoice - Conditional Routing
;;; ============================================================

;;; Either type (if not already defined)

;;; left : α → (Either α β)
(define (left x) (cons 'left x))

;;; right : β → (Either α β)
(define (right x) (cons 'right x))

;;; left? : (Either α β) → Bool
(define (left? e) (and (pair? e) (eq? (car e) 'left)))

;;; right? : (Either α β) → Bool
(define (right? e) (and (pair? e) (eq? (car e) 'right)))

;;; from-left : (Either α β) → α
(define (from-left e) (cdr e))

;;; from-right : (Either α β) → β
(define (from-right e) (cdr e))

;;; stage-left : Stage ctx a b -> Stage ctx (Either a c) (Either b c)
;;; Apply stage to left values, pass right through.
(define (stage-left s)
  (make-stage 'left
              (lambda (ctx input)
                      (if (left? input)
                          (let ([r (run-stage s ctx (from-left input))])
                               (if (stage-ok? r)
                                   (stage-ok (left (stage-result-value r)))
                                   r))
                          (stage-ok input)))))

;;; stage-right : Stage ctx a b -> Stage ctx (Either c a) (Either c b)
;;; Apply stage to right values, pass left through.
(define (stage-right s)
  (make-stage 'right
              (lambda (ctx input)
                      (if (right? input)
                          (let ([r (run-stage s ctx (from-right input))])
                               (if (stage-ok? r)
                                   (stage-ok (right (stage-result-value r)))
                                   r))
                          (stage-ok input)))))

;;; stage-choice : Stage ctx a c -> Stage ctx b c -> Stage ctx (Either a b) c
;;; Route either left or right to appropriate stage (choice combinator).
(define (stage-choice s-left s-right)
  (make-stage 'choice
              (lambda (ctx input)
                      (if (left? input)
                          (run-stage s-left ctx (from-left input))
                          (run-stage s-right ctx (from-right input))))))

;;; Note: The Haskell-style ||| operator is available as stage-choice
;;; Using stage-||| as an identifier causes reader issues in Chez Scheme
;;; due to | being a symbol delimiter.

;;; +++ : Stage ctx a b -> Stage ctx c d -> Stage ctx (Either a c) (Either b d)
(define (stage-+++ s1 s2)
  (stage->>> (stage-left s1) (stage-right s2)))

;;; ============================================================
;;; Conditional Stages
;;; ============================================================

;;; stage-if : (i -> Boolean) -> Stage ctx i o -> Stage ctx i o -> Stage ctx i o
;;; Conditional stage based on predicate.
(define (stage-if pred then-stage else-stage)
  (make-stage 'if
              (lambda (ctx input)
                      (if (pred input)
                          (run-stage then-stage ctx input)
                          (run-stage else-stage ctx input)))))

;;; stage-when : (i -> Boolean) -> Stage ctx i i -> Stage ctx i i
;;; Run stage only if predicate holds, otherwise pass through.
(define (stage-when pred stage)
  (stage-if pred stage stage-read))

;;; stage-unless : (i -> Boolean) -> Stage ctx i i -> Stage ctx i i
;;; Run stage only if predicate fails.
(define (stage-unless pred stage)
  (stage-if pred stage-read stage))

;;; stage-case : List ((i -> Boolean) . Stage ctx i o) -> Stage ctx i o -> Stage ctx i o
;;; Pattern matching over stages.
(define (stage-case cases default-stage)
  (make-stage 'case
              (lambda (ctx input)
                      (let loop ([cs cases])
                           (if (null? cs)
                               (run-stage default-stage ctx input)
                               (let ([pred (caar cs)]
                                     [stage (cdar cs)])
                                    (if (pred input)
                                        (run-stage stage ctx input)
                                        (loop (cdr cs)))))))))

;;; stage-guard : (i -> Boolean) -> String -> Stage ctx i i
;;; Assert condition or fail.
(define (stage-guard pred error-message)
  (make-stage 'guard
              (lambda (ctx input)
                      (if (pred input)
                          (stage-ok input)
                          (stage-err 'guard-failed error-message input)))))

;;; ============================================================
;;; Monadic Interface
;;; ============================================================

;;; stage-bind : Stage ctx a b -> (b -> Stage ctx b c) -> Stage ctx a c
;;; Monadic bind for stages.
(define (stage-bind s f)
  (make-stage 'bind
              (lambda (ctx input)
                      (let ([r (run-stage s ctx input)])
                           (if (stage-ok? r)
                               (run-stage (f (stage-result-value r)) ctx (stage-result-value r))
                               r)))))

;;; stage-map : (a -> b) -> Stage ctx i a -> Stage ctx i b
;;; Map a function over stage output.
(define (stage-map f s)
  (make-stage 'map
              (lambda (ctx input)
                      (let ([r (run-stage s ctx input)])
                           (if (stage-ok? r)
                               (stage-ok (f (stage-result-value r)))
                               r)))))

;;; stage-ap : Stage ctx i (a -> b) -> Stage ctx i a -> Stage ctx i b
;;; Applicative apply.
(define (stage-ap sf sa)
  (stage-bind sf
              (lambda (f)
                      (stage-map f sa))))

;;; stage-sequence : List (Stage ctx i o) -> Stage ctx i (List o)
;;; Sequence stages, collecting results.
(define (stage-sequence stages)
  (if (null? stages)
      (stage-pure '())
      (stage-bind (car stages)
                  (lambda (x)
                          (stage-map (lambda (xs) (cons x xs))
                                     (stage-sequence (cdr stages)))))))

;;; stage-traverse : (a -> Stage ctx i b) -> List a -> Stage ctx i (List b)
;;; Map and sequence.
(define (stage-traverse f xs)
  (stage-sequence (map f xs)))

;;; ============================================================
;;; Iteration and Looping
;;; ============================================================

;;; stage-fold : (b -> a -> Stage ctx a b) -> b -> List a -> Stage ctx _ b
;;; Fold over a list with stages.
(define (stage-fold f init xs)
  (if (null? xs)
      (stage-pure init)
      (make-stage 'fold
                  (lambda (ctx input)
                          (let loop ([acc init] [remaining xs])
                               (if (null? remaining)
                                   (stage-ok acc)
                                   (let ([r (run-stage (f acc (car remaining)) ctx input)])
                                        (if (stage-ok? r)
                                            (loop (stage-result-value r) (cdr remaining))
                                            r))))))))

;;; stage-for-each : (a -> Stage ctx _ _) -> List a -> Stage ctx _ ()
;;; Execute stage for each element (for side effects).
(define (stage-for-each f xs)
  (stage-map (lambda (_) '())
             (stage-traverse f xs)))

;;; stage-repeat : Nat -> Stage ctx a a -> Stage ctx a a
;;; Repeat a stage n times.
(define (stage-repeat n stage)
  (if (= n 0)
      stage-read
      (stage->>> stage (stage-repeat (- n 1) stage))))

;;; stage-while : (a -> Boolean) -> Stage ctx a a -> Stage ctx a a
;;; Repeat while predicate holds (with fuel limit).
(define (stage-while pred stage)
  (make-stage 'while
              (lambda (ctx input)
                      (let loop ([current input] [fuel 1000])
                           (if (= fuel 0)
                               (stage-err 'while-exhausted "Loop exceeded fuel limit" current)
                               (if (pred current)
                                   (let ([r (run-stage stage ctx current)])
                                        (if (stage-ok? r)
                                            (loop (stage-result-value r) (- fuel 1))
                                            r))
                                   (stage-ok current)))))))

;;; ============================================================
;;; Effect Staging (for interpreter)
;;; ============================================================
;;;
;;; Effects are staged operations that require interpretation.
;;; The pure algebra just captures intent; interpreter executes.

;;; make-effect-stage : Symbol -> Any -> Stage ctx i o
;;; Create a stage that performs an effect.
(define (make-effect-stage effect-type payload)
  (make-stage effect-type
              (lambda (ctx input)
                      (list 'stage-effect effect-type payload input))))

;;; stage-effect? : Any -> Boolean
(define (stage-effect? x)
  (and (pair? x) (eq? (car x) 'stage-effect)))

;;; stage-effect-type : Effect -> Symbol
(define (stage-effect-type e) (list-ref e 1))

;;; stage-effect-payload : Effect -> Any
(define (stage-effect-payload e) (list-ref e 2))

;;; stage-effect-input : Effect -> Any
(define (stage-effect-input e) (list-ref e 3))

;;; fanout-effect? : Effect -> Boolean
;;; Check if effect is a combined fanout effect from parallel execution.
(define (fanout-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'fanout)))

;;; fanout-effect-left : Effect -> Any
;;; Extract left branch result from fanout effect.
(define (fanout-effect-left e)
  (car (stage-effect-payload e)))

;;; fanout-effect-right : Effect -> Any
;;; Extract right branch result from fanout effect.
(define (fanout-effect-right e)
  (cdr (stage-effect-payload e)))

;;; ============================================================
;;; Utility Stages
;;; ============================================================

;;; stage-id : Stage ctx a a
;;; Identity stage.
(define stage-id stage-read)

;;; stage-const : b -> Stage ctx a b
;;; Constant stage (alias for stage-pure).
(define stage-const stage-pure)

;;; stage-dup : Stage ctx a (a . a)
;;; Duplicate input.
(define stage-dup
  (make-stage 'dup
              (lambda (ctx input)
                      (stage-ok (cons input input)))))

;;; stage-swap : Stage ctx (a . b) (b . a)
;;; Swap pair components.
(define stage-swap
  (make-stage 'swap
              (lambda (ctx input)
                      (stage-ok (cons (cdr input) (car input))))))

;;; stage-fst : Stage ctx (a . b) a
;;; Extract first component.
(define stage-fst
  (make-stage 'fst
              (lambda (ctx input)
                      (stage-ok (car input)))))

;;; stage-snd : Stage ctx (a . b) b
;;; Extract second component.
(define stage-snd
  (make-stage 'snd
              (lambda (ctx input)
                      (stage-ok (cdr input)))))

;;; stage-arr : (a -> b) -> Stage ctx a b
;;; Lift a pure function into a stage.
(define (stage-arr f)
  (make-stage 'arr
              (lambda (ctx input)
                      (stage-ok (f input)))))

;;; stage-arr-ctx : (ctx -> a -> b) -> Stage ctx a b
;;; Lift a function that uses context.
(define (stage-arr-ctx f)
  (make-stage 'arr-ctx
              (lambda (ctx input)
                      (stage-ok (f ctx input)))))

;;; ============================================================
;;; Error Handling
;;; ============================================================

;;; stage-catch : (StageResult -> Stage ctx i o) -> Stage ctx i o -> Stage ctx i o
;;; Catch errors and handle them.
(define (stage-catch handler stage)
  (make-stage 'catch
              (lambda (ctx input)
                      (let ([r (run-stage stage ctx input)])
                           (if (stage-ok? r)
                               r
                               (run-stage (handler r) ctx input))))))

;;; stage-recover : Alist (Symbol . Stage ctx i o) -> Stage ctx i o -> Stage ctx i o
;;; Recover from specific error codes.
(define (stage-recover handlers stage)
  (stage-catch
   (lambda (err)
           (if (stage-err? err)
               (let ([handler (assq (stage-err-code err) handlers)])
                    (if handler
                        (cdr handler)
                        (make-stage 'rethrow (lambda (ctx input) err))))
               (make-stage 'rethrow (lambda (ctx input) err))))
   stage))

;;; stage-default : a -> Stage ctx i a -> Stage ctx i a
;;; Return default value on any error.
(define (stage-default default-value stage)
  (stage-catch
   (lambda (err) (stage-pure default-value))
   stage))

;;; stage-optional : Stage ctx i a -> Stage ctx i (Maybe a)
;;; Convert errors to None, success to Some.
(define (stage-optional stage)
  (stage-catch
   (lambda (err) (stage-pure '()))
   (stage-map (lambda (x) (list 'some x)) stage)))

;;; ============================================================
;;; Pipeline Construction Helpers
;;; ============================================================

;;; pipeline : Symbol -> List Stage -> Stage
;;; Name a sequence of stages.
(define (pipeline name . stages)
  (make-stage name
              (lambda (ctx input)
                      (let loop ([ss stages] [current input])
                           (if (null? ss)
                               (stage-ok current)
                               (let ([r (run-stage (car ss) ctx current)])
                                    (if (stage-ok? r)
                                        (loop (cdr ss) (stage-result-value r))
                                        r)))))))

;;; stage-tap : (a -> ()) -> Stage ctx a a
;;; Execute side effect without changing value (for logging/debugging).
;;; Note: This creates an effect that must be interpreted.
(define (stage-tap f)
  (make-stage 'tap
              (lambda (ctx input)
                      (f input)
                      (stage-ok input))))

;;; stage-trace : String -> Stage ctx a a
;;; Log input with label (creates log effect).
(define (stage-trace label)
  (make-effect-stage 'log (list 'trace label)))

;;; ============================================================
;;; Exports (conceptual - no module system in base Scheme)
;;; ============================================================
;;;
;;; StageResult:
;;;   stage-ok, stage-err, stage-retry, stage-skip, stage-halt, stage-await
;;;   stage-ok?, stage-err?, stage-retry?, stage-skip?, stage-halt?, stage-await?
;;;   stage-result-tag, stage-result-value
;;;   stage-err-code, stage-err-message, stage-err-data
;;;
;;; Stage construction:
;;;   make-stage, run-stage, stage?, stage-name
;;;   stage-pure, stage-read, stage-ask, stage-asks, stage-local
;;;   stage-fail, stage-halt-with, stage-skip-with
;;;   stage-arr, stage-arr-ctx
;;;
;;; Composition:
;;;   stage->>>, stage-<<<, stage-compose
;;;   stage-first, stage-second, stage-***, stage-&&&
;;;   stage-left, stage-right, stage-choice, stage-+++
;;;
;;; Conditionals:
;;;   stage-if, stage-when, stage-unless, stage-case, stage-guard
;;;
;;; Monadic:
;;;   stage-bind, stage-map, stage-ap, stage-sequence, stage-traverse
;;;
;;; Iteration:
;;;   stage-fold, stage-for-each, stage-repeat, stage-while
;;;
;;; Error handling:
;;;   stage-catch, stage-recover, stage-default, stage-optional
;;;
;;; Effects:
;;;   make-effect-stage, stage-effect?, stage-effect-type,
;;;   stage-effect-payload, stage-effect-input,
;;;   fanout-effect?, fanout-effect-left, fanout-effect-right
;;;
;;; Utilities:
;;;   stage-id, stage-const, stage-dup, stage-swap, stage-fst, stage-snd
;;;   pipeline, stage-tap, stage-trace
