;;; lattice/fp/control/effects.ss — Algebraic Effects
;;; @module effects
;;; @requires prelude combinators continuation

(require 'prelude)
(require 'combinators)
(require 'continuation)

(doc 'module 'effects)
(doc 'purity 'partial)  ; Global gensym counter for fresh names
(doc 'description "Algebraic effect handlers for The Fold. Provides structured side effect handling with effect signatures, effect handlers (deep and shallow), effect rows, and common effects (State, Reader, Writer, Exception, NonDet, Async).")
(doc 'layer 'lattice)
(doc 'note "Uses a free monad-like encoding where effects are operations interpreted by handlers. Effect rows enable effect polymorphism. Deep handlers handle the entire continuation recursively, shallow handlers only handle the immediate continuation.")

;;; make-effect-sig : Symbol -> (List Operation) -> EffectSig
(define (make-effect-sig name operations)
  (doc 'description "Create an effect signature defining a set of operations with their parameter and result types. At runtime this is structured data for introspection and type checking.")
  (doc 'type '(-> Symbol (List Operation) EffectSig))
  (doc 'example "(make-effect-sig 'State (list (make-operation 'get 'Unit 's) (make-operation 'put 's 'Unit)))")
  (list 'effect-sig name operations))

;;; effect-sig? : Any -> Boolean
(define (effect-sig? x)
  (and (pair? x) (eq? (car x) 'effect-sig)))

;;; effect-sig-name : EffectSig -> Symbol
(define (effect-sig-name sig)
  (list-ref sig 1))

;;; effect-sig-operations : EffectSig -> (List Operation)
(define (effect-sig-operations sig)
  (list-ref sig 2))

;;; make-operation : Symbol -> Type -> Type -> Operation
;;; Create an operation with parameter and result types.
(define (make-operation name param-type result-type)
  (list 'operation name param-type result-type))

;;; operation? : Any -> Boolean
(define (operation? x)
  (and (pair? x) (eq? (car x) 'operation)))

;;; operation-name : Operation -> Symbol
(define (operation-name op)
  (list-ref op 1))

;;; operation-param-type : Operation -> Type
(define (operation-param-type op)
  (list-ref op 2))

;;; operation-result-type : Operation -> Type
(define (operation-result-type op)
  (list-ref op 3))

;;; lookup-operation : EffectSig -> Symbol -> Operation | #f
(define (lookup-operation sig op-name)
  (let loop ([ops (effect-sig-operations sig)])
       (cond
        [(null? ops) #f]
        [(eq? (operation-name (car ops)) op-name) (car ops)]
        [else (loop (cdr ops))])))

;;; ====
;;; Built-in Effect Signatures
;;; ====

;;; State effect signature: get/put operations
(define sig-State
  (make-effect-sig 'State
                   (list (make-operation 'get 'Unit 's)
                         (make-operation 'put 's 'Unit)
                         (make-operation 'modify '(-> s s) 'Unit))))

;;; Reader effect signature: ask/local operations
(define sig-Reader
  (make-effect-sig 'Reader
                   (list (make-operation 'ask 'Unit 'r)
                         (make-operation 'local '(-> r r) 'a))))

;;; Writer effect signature: tell operation
(define sig-Writer
  (make-effect-sig 'Writer
                   (list (make-operation 'tell 'w 'Unit))))

;;; Exception effect signature: throw operation
(define sig-Exception
  (make-effect-sig 'Exception
                   (list (make-operation 'throw 'e 'a))))

;;; NonDet effect signature: choose/fail operations
(define sig-NonDet
  (make-effect-sig 'NonDet
                   (list (make-operation 'choose '(List a) 'a)
                         (make-operation 'fail 'Unit 'a))))

;;; Console effect signature: print/read operations
(define sig-Console
  (make-effect-sig 'Console
                   (list (make-operation 'print 'String 'Unit)
                         (make-operation 'read 'Unit 'String))))

;;; Async effect signature: fork/await operations
(define sig-Async
  (make-effect-sig 'Async
                   (list (make-operation 'fork '(Eff e a) '(Future a))
                         (make-operation 'await '(Future a) 'a))))

;;; ====
;;; Effect Rows (Effect Type Tracking)
;;; ====
;;;
;;; Effect rows track which effects are used in a computation.
;;; They enable effect polymorphism: functions can be generic
;;; over effect sets.
;;;
;;; Row: (row (label1 label2 ...))
;;; Row variable: (row-var r)
;;; Extended row: (row-extend label row)
;;;
;;; Example:
;;;   print : String -> Eff (row-extend Console r) Unit
;;;   with-handler removes effect from row

;;; make-effect-row : (List Symbol) -> EffectRow
(define (make-effect-row effects)
  (list 'row effects))

;;; effect-row? : Any -> Boolean
(define (effect-row? x)
  (and (pair? x) (eq? (car x) 'row)))

;;; effect-row-effects : EffectRow -> (List Symbol)
(define (effect-row-effects row)
  (list-ref row 1))

;;; empty-row : EffectRow
(define empty-row (make-effect-row '()))

;;; row-contains? : EffectRow -> Symbol -> Boolean
(define (row-contains? row effect)
  (if (memq effect (effect-row-effects row)) #t #f))

;;; row-add : EffectRow -> Symbol -> EffectRow
;;; Add an effect to the row (if not already present)
(define (row-add row effect)
  (if (row-contains? row effect)
      row
      (make-effect-row (cons effect (effect-row-effects row)))))

;;; row-remove : EffectRow -> Symbol -> EffectRow
;;; Remove an effect from the row
(define (row-remove row effect)
  (make-effect-row (filter (lambda (e) (not (eq? e effect)))
                           (effect-row-effects row))))

;;; row-union : EffectRow -> EffectRow -> EffectRow
;;; Combine two effect rows
(define (row-union row1 row2)
  (let loop ([effects (effect-row-effects row2)] [result row1])
       (if (null? effects)
           result
           (loop (cdr effects) (row-add result (car effects))))))

;;; row-difference : EffectRow -> EffectRow -> EffectRow
;;; Effects in row1 but not in row2
(define (row-difference row1 row2)
  (make-effect-row
   (filter (lambda (e) (not (row-contains? row2 e)))
           (effect-row-effects row1))))

;;; make-row-var : Symbol -> RowVar
;;; Create a row variable for effect polymorphism
(define (make-row-var name)
  (list 'row-var name))

;;; row-var? : Any -> Boolean
(define (row-var? x)
  (and (pair? x) (eq? (car x) 'row-var)))

;;; row-var-name : RowVar -> Symbol
(define (row-var-name rv)
  (list-ref rv 1))

;;; ====
;;; Eff Monad - Effectful Computations
;;; ====
;;;
;;; Eff e a = Pure a | Op (Effect e) (Response -> Eff e a)
;;;
;;; An effectful computation either:
;;; - Returns a pure value
;;; - Performs an operation and continues with the response

;;; make-eff-pure : a -> Eff e a
(define (make-eff-pure val)
  (list 'eff-pure val))

;;; eff-pure? : Eff e a -> Boolean
(define (eff-pure? eff)
  (and (pair? eff) (eq? (car eff) 'eff-pure)))

;;; eff-pure-value : Eff e a -> a
(define (eff-pure-value eff)
  (list-ref eff 1))

;;; make-eff-op : Effect -> (Response -> Eff e a) -> Eff e a
(define (make-eff-op effect continuation)
  (list 'eff-op effect continuation))

;;; eff-op? : Eff e a -> Boolean
(define (eff-op? eff)
  (and (pair? eff) (eq? (car eff) 'eff-op)))

;;; eff-op-effect : Eff e a -> Effect
(define (eff-op-effect eff)
  (list-ref eff 1))

;;; eff-op-cont : Eff e a -> (Response -> Eff e a)
(define (eff-op-cont eff)
  (list-ref eff 2))

;;; eff-return : a -> Eff e a
(define eff-return make-eff-pure)

;;; ====
;;; Codensity-based Eff Bind (O(1) amortized)
;;; ====
;;;
;;; PERFORMANCE NOTE:
;;; The naive eff-bind implementation is O(N^2) for left-associative chains:
;;;   ((a >>= b) >>= c) >>= d
;;; Each bind traverses the entire left structure to attach continuations.
;;;
;;; The Codensity transformation gives O(1) amortized bind by representing
;;; computations in continuation-passing style. Instead of building nested
;;; structures, we accumulate continuations in a queue.
;;;
;;; Eff-Queue: ('eff-queue base-eff continuation-queue)
;;; where continuation-queue is a list of (a -> Eff e b) functions.
;;;
;;; This is the "bind queue" approach: we defer the actual traversal until
;;; the effect is run, at which point we apply all queued continuations.

;;; make-eff-queue : Eff e a -> (List (a -> Eff e b)) -> Eff-Queue e b
(define (make-eff-queue base conts)
  (list 'eff-queue base conts))

;;; eff-queue? : Any -> Boolean
(define (eff-queue? x)
  (and (pair? x) (eq? (car x) 'eff-queue)))

;;; eff-queue-base : Eff-Queue e a -> Eff e x
(define (eff-queue-base q)
  (list-ref q 1))

;;; eff-queue-conts : Eff-Queue e a -> (List (x -> Eff e a))
(define (eff-queue-conts q)
  (list-ref q 2))

;;; eff-bind : Eff e a -> (a -> Eff e b) -> Eff e b
;;; O(1) amortized via continuation queue.
;;; Left-associative chains like ((a >>= b) >>= c) >>= d simply append
;;; to the continuation queue instead of rebuilding structure.
(define (eff-bind ma f)
  (cond
   ;; Pure value: apply continuation immediately
   [(eff-pure? ma) (f (eff-pure-value ma))]
   ;; Queue: append continuation to the queue (O(1))
   [(eff-queue? ma)
    (make-eff-queue (eff-queue-base ma)
                    (append (eff-queue-conts ma) (list f)))]
   ;; Op: wrap in queue with single continuation
   [(eff-op? ma)
    (make-eff-queue ma (list f))]))

;;; eff-normalize : Eff e a -> Eff e a
;;; Convert queue form back to standard form for interpretation.
;;; This is called by handlers when they need to process effects.
(define (eff-normalize eff)
  (if (eff-queue? eff)
      (eff-apply-queue (eff-queue-base eff) (eff-queue-conts eff))
      eff))

;;; eff-apply-queue : Eff e a -> (List (a -> Eff e b)) -> Eff e b
;;; Apply queued continuations to an effect.
;;; This is where the actual work happens, but only once at run time.
(define (eff-apply-queue eff conts)
  (if (null? conts)
      eff
      (eff-apply-queue-step eff conts)))

;;; eff-apply-queue-step : Eff e a -> (List (a -> Eff e b)) -> Eff e b
;;; Step through the effect, threading continuations.
(define (eff-apply-queue-step eff conts)
  (cond
   [(null? conts) eff]
   [(eff-pure? eff)
    ;; Pure value: apply first continuation, continue with rest
    (let ([next ((car conts) (eff-pure-value eff))])
         (eff-apply-queue (eff-normalize next) (cdr conts)))]
   [(eff-queue? eff)
    ;; Nested queue: flatten by prepending inner queue's continuations
    (eff-apply-queue-step (eff-queue-base eff)
                          (append (eff-queue-conts eff) conts))]
   [(eff-op? eff)
    ;; Operation: wrap continuation to apply remaining queue after response
    (make-eff-op (eff-op-effect eff)
                 (lambda (resp)
                         (eff-apply-queue ((eff-op-cont eff) resp) conts)))]))

;;; eff-map : (a -> b) -> Eff e a -> Eff e b
(define (eff-map f ea)
  (eff-bind ea (lambda (a) (eff-return (f a)))))

;;; eff-ap : Eff e (a -> b) -> Eff e a -> Eff e b
(define (eff-ap ef ea)
  (eff-bind ef (lambda (f)
                       (eff-bind ea (lambda (a)
                                            (eff-return (f a)))))))

;;; perform : Effect -> Eff e Response
;;; Perform an effect operation.
(define (perform effect)
  (make-eff-op effect eff-return))

;;; ====
;;; Effect Definitions
;;; ====

;;; make-effect : Symbol -> Any -> Effect
;;; Create an effect with a tag and payload.
(define (make-effect tag payload)
  (list 'effect tag payload))

;;; effect? : Any -> Boolean
(define (effect? x)
  (and (pair? x) (eq? (car x) 'effect)))

;;; effect-tag : Effect -> Symbol
(define (effect-tag eff)
  (list-ref eff 1))

;;; effect-payload : Effect -> Any
(define (effect-payload eff)
  (list-ref eff 2))

;;; effect-label : Effect -> Symbol
;;; Get the effect label (category) from the tag
;;; Tags are like 'state-get, 'state-put - label is 'State
(define (effect-label eff)
  (let* ([tag (effect-tag eff)]
         [tag-str (symbol->string tag)]
         [parts (string-split tag-str #\-)])
        (if (null? parts)
            tag
            (string->symbol (string-titlecase (car parts))))))

;;; string-split provided by prelude

;;; string-titlecase : String → String
;;; Helper: convert first character to uppercase.
(define (string-titlecase str)
  (if (= (string-length str) 0)
      str
      (string-append
       (string (char-upcase (string-ref str 0)))
       (substring str 1 (string-length str)))))

;;; ====
;;; Effect Handlers (Generic Framework)
;;; ====
;;;
;;; A handler defines how to interpret effects.
;;;
;;; Deep handlers: recursively handle the entire continuation
;;; Shallow handlers: only handle immediate continuation
;;;
;;; Handler structure:
;;;   (handler return-case effect-cases semantics)
;;;
;;; return-case: (a -> b) - what to do with pure values
;;; effect-cases: alist of (tag . (payload k -> b))
;;; semantics: 'deep | 'shallow

;;; make-handler : (a -> b) -> (List (Symbol . Handler)) -> Symbol -> Handler
(define (make-handler return-case effect-cases semantics)
  (list 'handler return-case effect-cases semantics))

;;; handler? : Any -> Boolean
(define (handler? x)
  (and (pair? x) (eq? (car x) 'handler)))

;;; handler-return : Handler -> (a -> b)
(define (handler-return h)
  (list-ref h 1))

;;; handler-effects : Handler -> (List (Symbol . (payload k -> b)))
(define (handler-effects h)
  (list-ref h 2))

;;; handler-semantics : Handler -> Symbol
;;; Returns 'deep or 'shallow
(define (handler-semantics h)
  (list-ref h 3))

;;; handler-lookup : Handler -> Symbol -> (payload k -> b) | #f
(define (handler-lookup h tag)
  (let ([entry (assq tag (handler-effects h))])
       (if entry (cdr entry) #f)))

;;; deep-handler : (a -> b) -> (List (Symbol . Handler)) -> Handler
;;; Convenience: create a deep handler
(define (deep-handler return-case effect-cases)
  (make-handler return-case effect-cases 'deep))

;;; shallow-handler : (a -> b) -> (List (Symbol . Handler)) -> Handler
;;; Convenience: create a shallow handler
(define (shallow-handler return-case effect-cases)
  (make-handler return-case effect-cases 'shallow))

;;; ====
;;; Handler Application
;;; ====

;;; handle : Handler -> Eff e a -> b
;;; Apply a handler to an effectful computation.
;;; Deep semantics: handler applies to entire continuation.
;;; Shallow semantics: handler only applies to immediate step.
(define (handle handler eff)
  (if (eq? (handler-semantics handler) 'deep)
      (handle-deep handler eff)
      (handle-shallow handler eff)))

;;; handle-deep : Handler -> Eff e a -> b
;;; Deep handler: recursively handles continuation
;;; Handles the queue form by normalizing first.
(define (handle-deep handler eff)
  (cond
   [(eff-pure? eff)
    ((handler-return handler) (eff-pure-value eff))]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (handle-deep handler (eff-normalize eff))]
   [(eff-op? eff)
    (let* ([effect (eff-op-effect eff)]
           [tag (effect-tag effect)]
           [case-handler (handler-lookup handler tag)])
          (if case-handler
              ;; Found handler: wrap continuation to re-apply handler
              (case-handler
               (effect-payload effect)
               (lambda (resp)
                       (handle-deep handler ((eff-op-cont eff) resp))))
              ;; Not handled: re-emit the effect
              (make-eff-op effect
                           (lambda (resp)
                                   (handle-deep handler ((eff-op-cont eff) resp))))))]))

;;; handle-shallow : Handler -> Eff e a -> b
;;; Shallow handler: only handles immediate continuation
;;; Handles the queue form by normalizing first.
(define (handle-shallow handler eff)
  (cond
   [(eff-pure? eff)
    ((handler-return handler) (eff-pure-value eff))]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (handle-shallow handler (eff-normalize eff))]
   [(eff-op? eff)
    (let* ([effect (eff-op-effect eff)]
           [tag (effect-tag effect)]
           [case-handler (handler-lookup handler tag)])
          (if case-handler
              ;; Found handler: pass raw continuation (not wrapped)
              (case-handler
               (effect-payload effect)
               (eff-op-cont eff))
              ;; Not handled: re-emit the effect
              (make-eff-op effect
                           (lambda (resp)
                                   (handle-shallow handler ((eff-op-cont eff) resp))))))]))

;;; ====
;;; Handler Composition
;;; ====

;;; compose-handlers : Handler -> Handler -> Handler
;;; Compose two handlers: h1 handles first, then h2
(define (compose-handlers h1 h2)
  (make-handler
   ;; Return case: chain returns
   (lambda (a)
           ((handler-return h2) ((handler-return h1) a)))
   ;; Effect cases: merge, h1 takes priority
   (append (handler-effects h1)
           (filter (lambda (entry)
                           (not (assq (car entry) (handler-effects h1))))
                   (handler-effects h2)))
   ;; Use h1's semantics
   (handler-semantics h1)))

;;; nest-handlers : (List Handler) -> Handler
;;; Nest multiple handlers into one
(define (nest-handlers handlers)
  (if (null? handlers)
      (deep-handler identity '())
      (fold-left compose-handlers (car handlers) (cdr handlers))))

;;; with-handler : Handler -> Eff e a -> Eff e' b
;;; Sugar for applying a handler (alias for handle)
(define with-handler handle)

;;; ====
;;; State Effect
;;; ====

;;; state-get : Eff State s
(define state-get
  (perform (make-effect 'state-get '())))

;;; state-put : s -> Eff State ()
(define (state-put s)
  (perform (make-effect 'state-put s)))

;;; state-modify : (s -> s) -> Eff State ()
(define (state-modify f)
  (eff-bind state-get
            (lambda (s)
                    (state-put (f s)))))

;;; state-gets : (s -> a) -> Eff State a
(define (state-gets f)
  (eff-bind state-get
            (lambda (s)
                    (eff-return (f s)))))

;;; make-state-handler : s -> Handler
;;; Create a state handler with initial state
;;; NOTE: This handler is not used by run-state; kept for composability.
;;; The handler itself returns a function (s -> (a . s)) that threads state.
(define (make-state-handler init-state)
  (deep-handler
   (lambda (a) (lambda (s) (cons a s)))  ; Thread the state through
   `((state-get . ,(lambda (payload k)
                           (lambda (s) ((k s) s))))
     (state-put . ,(lambda (payload k)
                           (lambda (s) ((k '()) payload)))))))

;;; run-state : s × (Eff State α) → (α . s)
;;; Handle state effect with initial state
(define (run-state init-state eff)
  (run-state-helper init-state eff))

;;; run-state-helper : s × (Eff State α) → (α . s)
;;; Internal helper for state effect handling.
(define (run-state-helper state eff)
  (cond
   [(eff-pure? eff)
    (cons (eff-pure-value eff) state)]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (run-state-helper state (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(state-get)
                (run-state-helper state (k state))]
               [(state-put)
                (run-state-helper (effect-payload effect) (k '()))]
               [else
                ;; Unknown effect, pass through
                (make-eff-op effect
                             (lambda (resp)
                                     (run-state-helper state (k resp))))]))]))

;;; ====
;;; Reader Effect
;;; ====

;;; reader-ask : Eff Reader r
(define reader-ask
  (perform (make-effect 'reader-ask '())))

;;; reader-local : (r -> r) -> Eff Reader a -> Eff Reader a
;;; Modify environment for a sub-computation.
;;; The inner computation runs with the modified environment,
;;; then the original environment is restored for subsequent operations.
(define (reader-local f eff)
  (make-eff-op (make-effect 'reader-local (cons f eff))
               eff-return))

;;; reader-asks : (r -> a) -> Eff Reader a
(define (reader-asks f)
  (eff-bind reader-ask
            (lambda (r)
                    (eff-return (f r)))))

;;; run-reader : r -> Eff Reader a -> a
;;; Handle reader effect.
(define (run-reader env eff)
  (cond
   [(eff-pure? eff)
    (eff-pure-value eff)]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (run-reader env (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(reader-ask)
                (run-reader env (k env))]
               [(reader-local)
                (let* ([payload (effect-payload effect)]
                       [f (car payload)]
                       [scoped-eff (cdr payload)]
                       ;; Run the scoped computation with the modified environment
                       [scoped-result (run-reader (f env) scoped-eff)])
                      ;; Resume the continuation with the scoped result,
                      ;; but with the ORIGINAL environment restored
                      (run-reader env (k scoped-result)))]
               [else
                ;; Unknown effect, delegate to outer handler
                (make-eff-op effect
                             (lambda (resp)
                                     (run-reader env (k resp))))]))]))

;;; ====
;;; Writer Effect
;;; ====

;;; writer-tell : w -> Eff Writer ()
(define (writer-tell msg)
  (perform (make-effect 'writer-tell msg)))

;;; writer-listen : Eff Writer a -> Eff Writer (a . (List w))
;;; Listen to the output of a sub-computation
(define (writer-listen eff)
  (perform (make-effect 'writer-listen eff)))

;;; writer-pass : Eff Writer (a . (w -> w)) -> Eff Writer a
;;; Transform output with a function
(define (writer-pass eff)
  (perform (make-effect 'writer-pass eff)))

;;; run-writer : (Eff Writer α) → (α . (List w))
;;; Handle writer effect, collecting output.
(define (run-writer eff)
  (run-writer-helper '() eff))

;;; run-writer-helper : (List w) × (Eff Writer α) → (α . (List w))
;;; Internal helper for writer effect handling.
(define (run-writer-helper log eff)
  (cond
   [(eff-pure? eff)
    (cons (eff-pure-value eff) (reverse log))]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (run-writer-helper log (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(writer-tell)
                (run-writer-helper (cons (effect-payload effect) log)
                                   (k '()))]
               [(writer-listen)
                ;; Run inner computation, capture its log
                (let* ([inner (effect-payload effect)]
                       [inner-result (run-writer inner)])
                      ;; Check if inner-result is an unhandled effect operation
                      (if (eff-op? inner-result)
                          ;; Bubble up the unhandled effect, wrapping continuation
                          (make-eff-op (eff-op-effect inner-result)
                                       (lambda (resp)
                                               (run-writer-helper log
                                                                  (k (cons ((eff-op-cont inner-result) resp)
                                                                           '())))))
                          ;; Normal case: inner-result is (val . log) pair
                          (let ([inner-val (car inner-result)]
                                [inner-log (cdr inner-result)])
                               (run-writer-helper (append (reverse inner-log) log)
                                                  (k (cons inner-val inner-log))))))]
               [(writer-pass)
                ;; Run inner, apply function to transform output
                (let* ([inner (effect-payload effect)]
                       [inner-result (run-writer inner)])
                      ;; Check if inner-result is an unhandled effect operation
                      (if (eff-op? inner-result)
                          ;; Bubble up the unhandled effect, wrapping continuation
                          (make-eff-op (eff-op-effect inner-result)
                                       (lambda (resp)
                                               (run-writer-helper log
                                                                  (k ((eff-op-cont inner-result) resp)))))
                          ;; Normal case: inner-result is ((val . f) . log) pair
                          (let* ([val-and-f (car inner-result)]
                                 [inner-log (cdr inner-result)]
                                 [val (car val-and-f)]
                                 [f (cdr val-and-f)])
                                (run-writer-helper (append (map f (reverse inner-log)) log)
                                                   (k val)))))]
               [else
                (make-eff-op effect
                             (lambda (resp)
                                     (run-writer-helper log (k resp))))]))]))

;;; ====
;;; Exception Effect
;;; ====

;;; eff-throw : e -> Eff Exception a
(define (eff-throw exn)
  (perform (make-effect 'throw exn)))

;;; eff-catch : Eff Exception a -> (e -> Eff Exception a) -> Eff Exception a
;;; Handle exceptions with a handler.
(define (eff-catch eff handler)
  (cond
   [(eff-pure? eff) eff]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (eff-catch (eff-normalize eff) handler)]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(throw)
                (handler (effect-payload effect))]
               [else
                (make-eff-op effect
                             (lambda (resp)
                                     (eff-catch (k resp) handler)))]))]))

;;; run-exception : Eff Exception a -> Either e a
;;; Handle exception effect.
(define (run-exception eff)
  (cond
   [(eff-pure? eff)
    (right (eff-pure-value eff))]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (run-exception (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(throw)
                (left (effect-payload effect))]
               [else
                (make-eff-op effect
                             (lambda (resp)
                                     (run-exception (k resp))))]))]))

;;; eff-try : Eff Exception a -> Eff e (Either e a)
;;; Try/catch that returns Either
(define (eff-try eff)
  (eff-return (run-exception eff)))

;;; ====
;;; NonDet Effect (Non-determinism)
;;; ====

;;; nondet-choose : List a -> Eff NonDet a
(define (nondet-choose options)
  (perform (make-effect 'nondet-choose options)))

;;; nondet-fail : Eff NonDet a
(define nondet-fail
  (nondet-choose '()))

;;; nondet-guard : Bool -> Eff NonDet ()
;;; Fail if condition is false
(define (nondet-guard pred)
  (if pred
      (eff-return '())
      nondet-fail))

;;; nondet-from-maybe : Maybe a -> Eff NonDet a
(define (nondet-from-maybe m)
  (if (just? m)
      (eff-return (from-just m))
      nondet-fail))

;;; run-nondet : Eff NonDet a -> List a
;;; Handle non-determinism effect, collecting all results.
;;; Uses O(N) collection via reverse-accumulate pattern instead of
;;; O(N^2) apply-append pattern.
(define (run-nondet eff)
  (reverse (run-nondet-acc '() eff)))

;;; run-nondet-acc : (List a) -> Eff NonDet a -> (List a)
;;; Accumulator-based helper for run-nondet.
;;; Collects results in reverse order (prepends via cons).
(define (run-nondet-acc acc eff)
  (cond
   [(eff-pure? eff)
    (cons (eff-pure-value eff) acc)]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (run-nondet-acc acc (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(nondet-choose)
                (let ([options (effect-payload effect)])
                     ;; Fold over options, threading the accumulator
                     ;; Each branch appends its results to acc
                     (fold-left (lambda (acc opt)
                                        (run-nondet-acc acc (k opt)))
                                acc
                                options))]
               [else
                ;; Unknown effect, delegate to outer handler
                (make-eff-op effect
                             (lambda (resp)
                                     (run-nondet-acc acc (k resp))))]))]))

;;; run-nondet-first : Eff NonDet a -> Maybe a
;;; Handle non-determinism, returning first result.
(define (run-nondet-first eff)
  (let ([results (run-nondet eff)])
       (if (null? results)
           nothing
           (just (car results)))))

;;; run-nondet-bounded : Nat -> Eff NonDet a -> List a
;;; Handle non-determinism with a bound on number of results.
;;; Uses O(N) collection via accumulator pattern.
(define (run-nondet-bounded limit eff)
  (reverse (run-nondet-bounded-acc limit '() eff)))

;;; run-nondet-bounded-acc : Nat -> (List a) -> Eff NonDet a -> (List a)
;;; Accumulator-based helper for run-nondet-bounded.
;;; Returns results in reverse order; caller must reverse.
(define (run-nondet-bounded-acc remaining acc eff)
  (if (<= remaining 0)
      acc
      (cond
       [(eff-pure? eff)
        (cons (eff-pure-value eff) acc)]
       [(eff-queue? eff)
        ;; Normalize queue form before handling
        (run-nondet-bounded-acc remaining acc (eff-normalize eff))]
       [(eff-op? eff)
        (let ([effect (eff-op-effect eff)]
              [k (eff-op-cont eff)])
             (case (effect-tag effect)
                   [(nondet-choose)
                    (let loop ([options (effect-payload effect)]
                               [acc acc]
                               [remain remaining])
                         (if (or (null? options) (<= remain 0))
                             acc
                             (let* ([new-acc (run-nondet-bounded-acc remain acc (k (car options)))]
                                    [added (- (length new-acc) (length acc))])
                                   (loop (cdr options)
                                         new-acc
                                         (- remain added)))))]
                   [else
                    ;; Unknown effect, delegate to outer handler
                    (make-eff-op effect
                                 (lambda (resp)
                                         (run-nondet-bounded-acc remaining acc (k resp))))]))])))

;;; ====
;;; Console Effect
;;; ====

;;; console-print : String -> Eff Console ()
(define (console-print msg)
  (perform (make-effect 'console-print msg)))

;;; console-read : Eff Console String
(define console-read
  (perform (make-effect 'console-read '())))

;;; console-print-line : String -> Eff Console ()
(define (console-print-line msg)
  (console-print (string-append msg "\n")))

;;; run-console-pure : (List String) × (Eff Console α) → (α . (List String))
;;; Pure handler: takes input list, returns output list.
(define (run-console-pure inputs eff)
  (run-console-helper inputs '() eff))

;;; run-console-helper : (List String) × (List String) × (Eff Console α) → (α . (List String))
;;; Internal helper for console effect handling.
(define (run-console-helper inputs outputs eff)
  (cond
   [(eff-pure? eff)
    (cons (eff-pure-value eff) (reverse outputs))]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (run-console-helper inputs outputs (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(console-print)
                (run-console-helper inputs
                                    (cons (effect-payload effect) outputs)
                                    (k '()))]
               [(console-read)
                (if (null? inputs)
                    (error 'run-console-pure "No more input")
                    (run-console-helper (cdr inputs)
                                        outputs
                                        (k (car inputs))))]
               [else
                (make-eff-op effect
                             (lambda (resp)
                                     (run-console-helper inputs outputs (k resp))))]))]))

;;; ====
;;; Async Effect (for futures/promises)
;;; ====

;;; async-fork : Eff e a -> Eff Async (Future a)
(define (async-fork computation)
  (perform (make-effect 'async-fork computation)))

;;; async-await : Future a -> Eff Async a
(define (async-await future)
  (perform (make-effect 'async-await future)))

;;; async-parallel : (List (Eff e a)) -> Eff Async (List a)
;;; Run computations in parallel
(define (async-parallel comps)
  (eff-bind (eff-sequence (map async-fork comps))
            (lambda (futures)
                    (eff-sequence (map async-await futures)))))

;;; run-async-sync : Eff Async a -> a
;;; Run async effects synchronously (no parallelism).
(define (run-async-sync eff)
  (cond
   [(eff-pure? eff)
    (eff-pure-value eff)]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (run-async-sync (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(async-fork)
                ;; Just run immediately and wrap result
                (let ([comp (effect-payload effect)]
                      [future-id (eff-gensym 'future)])
                     (let ([result (run-async-sync comp)])
                          (run-async-sync (k (cons future-id result)))))]
               [(async-await)
                ;; Future is (id . result) pair
                (let ([future (effect-payload effect)])
                     (run-async-sync (k (cdr future))))]
               [else
                ;; Unknown effect, delegate to outer handler
                (make-eff-op effect
                             (lambda (resp)
                                     (run-async-sync (k resp))))]))]))

;;; *gensym-counter* : Nat
;;; Mutable counter for generating unique symbols.
(define *gensym-counter* 0)

;;; eff-gensym : Symbol → (Symbol . Nat)
;;; Generate a unique symbol with a prefix.
(define (eff-gensym prefix)
  (set! *gensym-counter* (+ *gensym-counter* 1))
  (cons prefix *gensym-counter*))

;;; ====
;;; Combining Effects
;;; ====

;;; eff-sequence : List (Eff e a) -> Eff e (List a)
;;; Sequence effectful computations.
(define (eff-sequence effs)
  (if (null? effs)
      (eff-return '())
      (eff-bind (car effs)
                (lambda (x)
                        (eff-bind (eff-sequence (cdr effs))
                                  (lambda (xs)
                                          (eff-return (cons x xs))))))))

;;; eff-map-m : (a -> Eff e b) -> List a -> Eff e (List b)
;;; Map an effectful function over a list.
(define (eff-map-m f lst)
  (eff-sequence (map f lst)))

;;; eff-filter-m : (a -> Eff e Bool) -> List a -> Eff e (List a)
;;; Filter with effectful predicate
(define (eff-filter-m pred lst)
  (if (null? lst)
      (eff-return '())
      (eff-bind (pred (car lst))
                (lambda (keep?)
                        (eff-bind (eff-filter-m pred (cdr lst))
                                  (lambda (rest)
                                          (eff-return (if keep?
                                                          (cons (car lst) rest)
                                                          rest))))))))

;;; eff-for-each : (a -> Eff e ()) -> List a -> Eff e ()
;;; Execute effect for each element.
(define (eff-for-each f lst)
  (if (null? lst)
      (eff-return '())
      (eff-bind (f (car lst))
                (lambda (_)
                        (eff-for-each f (cdr lst))))))

;;; eff-fold : (b -> a -> Eff e b) -> b -> List a -> Eff e b
;;; Fold with effects.
(define (eff-fold f init lst)
  (if (null? lst)
      (eff-return init)
      (eff-bind (f init (car lst))
                (lambda (acc)
                        (eff-fold f acc (cdr lst))))))

;;; eff-fold-right : (a -> b -> Eff e b) -> b -> List a -> Eff e b
;;; Right fold with effects
(define (eff-fold-right f init lst)
  (if (null? lst)
      (eff-return init)
      (eff-bind (eff-fold-right f init (cdr lst))
                (lambda (acc)
                        (f (car lst) acc)))))

;;; eff-when : Bool -> Eff e () -> Eff e ()
(define (eff-when pred action)
  (if pred action (eff-return '())))

;;; eff-unless : Bool -> Eff e () -> Eff e ()
(define (eff-unless pred action)
  (eff-when (not pred) action))

;;; eff-replicate : Nat -> Eff e a -> Eff e (List a)
;;; Run effect n times
(define (eff-replicate n action)
  (if (<= n 0)
      (eff-return '())
      (eff-bind action
                (lambda (x)
                        (eff-bind (eff-replicate (- n 1) action)
                                  (lambda (xs)
                                          (eff-return (cons x xs))))))))

;;; ====
;;; Lift / Inject Effects
;;; ====

;;; eff-lift : (a -> b) -> Eff e a -> Eff e b
;;; Same as eff-map.
(define eff-lift eff-map)

;;; eff-lift2 : (a -> b -> c) -> Eff e a -> Eff e b -> Eff e c
(define (eff-lift2 f ea eb)
  (eff-bind ea (lambda (a)
                       (eff-bind eb (lambda (b)
                                            (eff-return (f a b)))))))

;;; eff-lift3 : (a -> b -> c -> d) -> Eff e a -> Eff e b -> Eff e c -> Eff e d
(define (eff-lift3 f ea eb ec)
  (eff-bind ea (lambda (a)
                       (eff-bind eb (lambda (b)
                                            (eff-bind ec (lambda (c)
                                                                 (eff-return (f a b c)))))))))

;;; ====
;;; Effect Constraints (Runtime Type Safety)
;;; ====
;;;
;;; Effect constraints track which effects a computation uses.
;;; This enables runtime checking of effect safety.

;;; make-effect-constraint : (List Symbol) -> Constraint
(define (make-effect-constraint effects)
  (list 'effect-constraint effects))

;;; effect-constraint? : Any -> Boolean
(define (effect-constraint? x)
  (and (pair? x) (eq? (car x) 'effect-constraint)))

;;; effect-constraint-effects : Constraint -> (List Symbol)
(define (effect-constraint-effects c)
  (list-ref c 1))

;;; check-effects : (List Symbol) -> Eff e a -> Bool
;;; Check if computation only uses allowed effects
(define (check-effects allowed eff)
  (cond
   [(eff-pure? eff) #t]
   [(eff-queue? eff)
    ;; Normalize queue form before checking
    (check-effects allowed (eff-normalize eff))]
   [(eff-op? eff)
    (let ([label (effect-label (eff-op-effect eff))])
         (and (memq label allowed)
              ;; Note: can't fully check continuation without running it
              #t))]))

;;; with-effect-check : (List Symbol) -> Eff e a -> Eff e a
;;; Wrap computation with effect checking (debug mode)
(define (with-effect-check allowed eff)
  (if (check-effects allowed eff)
      eff
      (eff-throw (list 'effect-violation
                       (effect-label (eff-op-effect eff))
                       allowed))))

;;; ====
;;; Resumable Continuations
;;; ====
;;;
;;; Handlers can resume continuations multiple times (for nondet),
;;; zero times (for exceptions), or exactly once (for state).

;;; resume : (Response -> Eff e a) -> Response -> Eff e a
;;; Resume a captured continuation
(define (resume k response)
  (k response))

;;; resume-with : (Response -> Eff e a) -> Response -> Handler -> b
;;; Resume a continuation under a handler
(define (resume-with k response handler)
  (handle handler (k response)))

;;; abort : a -> Eff e a
;;; Abort current computation with a value (don't resume)
(define (abort val)
  (eff-return val))

;;; ====
;;; Example: Stateful Counter
;;; ====

;;; counter-increment : Eff State ()
(define counter-increment
  (state-modify add1))

;;; counter-decrement : Eff State ()
(define counter-decrement
  (state-modify (lambda (n) (- n 1))))

;;; counter-reset : Eff State ()
(define counter-reset
  (state-put 0))

;;; counter-get : Eff State Nat
(define counter-get
  state-get)

;;; ====
;;; Example: Logging with Writer
;;; ====

;;; log-info : String -> Eff Writer ()
(define (log-info msg)
  (writer-tell (list 'info msg)))

;;; log-warn : String -> Eff Writer ()
(define (log-warn msg)
  (writer-tell (list 'warn msg)))

;;; log-error : String -> Eff Writer ()
(define (log-error msg)
  (writer-tell (list 'error msg)))

;;; log-debug : String -> Eff Writer ()
(define (log-debug msg)
  (writer-tell (list 'debug msg)))

;;; ====
;;; Combined Effect Handlers
;;; ====

;;; run-state-writer : s × (Eff (State + Writer) α) → ((α . s) . (List w))
;;; Handle both state and writer effects
(define (run-state-writer init-state eff)
  (run-writer (run-state-in-writer init-state eff)))

;;; run-state-in-writer : s × (Eff (State + Writer) α) → (Eff Writer (α . s))
;;; Internal helper: handle State effect, passing Writer through.
(define (run-state-in-writer state eff)
  (cond
   [(eff-pure? eff)
    (eff-return (cons (eff-pure-value eff) state))]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (run-state-in-writer state (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(state-get)
                (run-state-in-writer state (k state))]
               [(state-put)
                (run-state-in-writer (effect-payload effect) (k '()))]
               [else
                ;; Pass through to outer handler
                (make-eff-op effect
                             (lambda (resp)
                                     (run-state-in-writer state (k resp))))]))]))

;;; run-reader-writer : r × (Eff (Reader + Writer) α) → (α . (List w))
;;; Handle both reader and writer effects
(define (run-reader-writer env eff)
  (run-writer (run-reader-in-writer env eff)))

;;; run-reader-in-writer : r × (Eff (Reader + Writer) α) → (Eff Writer α)
;;; Internal helper: handle Reader effect, passing Writer through.
(define (run-reader-in-writer env eff)
  (cond
   [(eff-pure? eff)
    (eff-return (eff-pure-value eff))]
   [(eff-queue? eff)
    ;; Normalize queue form before handling
    (run-reader-in-writer env (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(reader-ask)
                (run-reader-in-writer env (k env))]
               [(reader-local)
                (let* ([payload (effect-payload effect)]
                       [f (car payload)]
                       [scoped-eff (cdr payload)])
                      ;; Run the scoped computation with modified env
                      ;; This returns an Eff (for writer) so we need to bind it
                      (eff-bind (run-reader-in-writer (f env) scoped-eff)
                                (lambda (scoped-result)
                                        ;; Continue with original env restored
                                        (run-reader-in-writer env (k scoped-result)))))]
               [else
                ;; Pass through to outer handler
                (make-eff-op effect
                             (lambda (resp)
                                     (run-reader-in-writer env (k resp))))]))]))

;;; ====
;;; Scoped Effects
;;; ====
;;;
;;; Scoped effects limit effect scope to a region of code.

;;; with-state : s -> Eff State a -> Eff e a
;;; Run state effect in a scope, returning only the result
(define (with-state init eff)
  (eff-return (car (run-state init eff))))

;;; with-reader : r -> Eff Reader a -> Eff e a
;;; Run reader effect in a scope
(define (with-reader env eff)
  (eff-return (run-reader env eff)))

;;; with-writer : Eff Writer a -> Eff e (a . (List w))
;;; Run writer effect in a scope
(define (with-writer eff)
  (eff-return (run-writer eff)))

;;; with-exception : Eff Exception a -> Eff e (Either e a)
;;; Run exception effect in a scope
(define (with-exception eff)
  (eff-return (run-exception eff)))

;;; with-nondet : Eff NonDet a -> Eff e (List a)
;;; Run nondet effect in a scope
(define (with-nondet eff)
  (eff-return (run-nondet eff)))

;;; ====
;;; Local Effect Handling
;;; ====

;;; local-state : s -> Eff State a -> Eff State a
;;; Run with temporary state, restore original after
(define (local-state temp-state eff)
  (eff-bind state-get
            (lambda (saved)
                    (eff-bind (state-put temp-state)
                              (lambda (_)
                                      (eff-bind eff
                                                (lambda (result)
                                                        (eff-bind (state-put saved)
                                                                  (lambda (_)
                                                                          (eff-return result))))))))))

;;; local-reader : (r -> r) -> Eff Reader a -> Eff Reader a
;;; Alias for reader-local
(define local-reader reader-local)

;;; ====
;;; Effect Interleaving
;;; ====

;;; interleave : Eff e a -> Eff e b -> Eff e (Either a b)
;;; Interleave two computations (fair scheduling for nondet)
(define (interleave eff1 eff2)
  (cond
   [(eff-pure? eff1)
    (eff-return (left (eff-pure-value eff1)))]
   [(eff-pure? eff2)
    (eff-return (right (eff-pure-value eff2)))]
   [(eff-queue? eff1)
    ;; Normalize queue form before interleaving
    (interleave (eff-normalize eff1) eff2)]
   [(eff-queue? eff2)
    ;; Normalize queue form before interleaving
    (interleave eff1 (eff-normalize eff2))]
   [(eff-op? eff1)
    ;; Run one step of eff1, then switch to eff2
    (make-eff-op (eff-op-effect eff1)
                 (lambda (resp)
                         (interleave eff2 ((eff-op-cont eff1) resp))))]))

;;; ====
;;; Example Usage (for documentation)
;;; ====
;;;
;;; ;; State effect
;;; (run-state 0
;;;   (eff-bind state-get
;;;             (lambda (n)
;;;               (eff-bind (state-put (+ n 10))
;;;                         (lambda (_)
;;;                           (eff-bind state-get
;;;                                     (lambda (m)
;;;                                       (eff-return (* m 2)))))))))
;;; ; => (20 . 10)
;;;
;;; ;; Writer effect
;;; (run-writer
;;;   (eff-bind (log-info "Starting")
;;;             (lambda (_)
;;;               (eff-bind (log-info "Done")
;;;                         (lambda (_)
;;;                           (eff-return 42))))))
;;; ; => (42 . ((info "Starting") (info "Done")))
;;;
;;; ;; Non-determinism
;;; (run-nondet
;;;   (eff-bind (nondet-choose '(1 2 3))
;;;             (lambda (x)
;;;               (eff-bind (nondet-choose '(10 20))
;;;                         (lambda (y)
;;;                           (eff-return (+ x y)))))))
;;; ; => (11 21 12 22 13 23)
;;;
;;; ;; Exception handling
;;; (run-exception
;;;   (eff-catch
;;;     (eff-bind (eff-return 10)
;;;               (lambda (x)
;;;                 (if (> x 5)
;;;                     (eff-throw "too big")
;;;                     (eff-return x))))
;;;     (lambda (e) (eff-return -1))))
;;; ; => (right -1)
;;;
;;; ;; Deep vs Shallow handlers
;;; ;; Deep: handler wraps entire continuation
;;; ;; Shallow: handler only applies once
;;;
;;; ;; Effect rows
;;; (define my-row (row-add (row-add empty-row 'State) 'Writer))
;;; ; => (row (Writer State))
;;;
;;; ;; Effect signature introspection
;;; (lookup-operation sig-State 'get)
;;; ; => (operation get Unit s)
