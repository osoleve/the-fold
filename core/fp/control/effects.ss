;;; fabric/stitches/fp/effects.ss — Algebraic Effects
;;;
;;; Algebraic effects provide a structured way to handle side effects.
;;; Effects are declared, used in computations, and handled by interpreters.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Eff monad: Effectful computations
;;;   - Effect handlers: Interpret effects
;;;   - Common effects: State, Reader, Writer, Exception, NonDet
;;;   - Effect composition and handling
;;;   - Deep and shallow handlers
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;   - fp/continuation.ss
;;;
;;; Approach:
;;;   We use a free monad-like encoding where effects are operations
;;;   that can be interpreted by handlers.

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")
(load "core/fp/control/continuation.ss")

;;; ============================================================
;;; Eff Monad - Effectful Computations
;;; ============================================================
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

;;; eff-bind : Eff e a -> (a -> Eff e b) -> Eff e b
(define (eff-bind ma f)
  (cond
   [(eff-pure? ma) (f (eff-pure-value ma))]
   [(eff-op? ma)
    (make-eff-op (eff-op-effect ma)
                 (lambda (resp)
                         (eff-bind ((eff-op-cont ma) resp) f)))]))

;;; eff-map : (a -> b) -> Eff e a -> Eff e b
(define (eff-map f ea)
  (eff-bind ea (lambda (a) (eff-return (f a)))))

;;; perform : Effect -> Eff e Response
;;; Perform an effect operation.
(define (perform effect)
  (make-eff-op effect eff-return))

;;; ============================================================
;;; Effect Definitions
;;; ============================================================

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

;;; ============================================================
;;; State Effect
;;; ============================================================

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

;;; run-state : s -> Eff State a -> (a, s)
;;; Handle state effect.
(define (run-state init-state eff)
  (run-state-helper init-state eff))

(define (run-state-helper state eff)
  (cond
   [(eff-pure? eff)
    (cons (eff-pure-value eff) state)]
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

;;; ============================================================
;;; Reader Effect
;;; ============================================================

;;; reader-ask : Eff Reader r
(define reader-ask
  (perform (make-effect 'reader-ask '())))

;;; reader-local : (r -> r) -> Eff Reader a -> Eff Reader a
;;; Modify environment for a sub-computation.
(define (reader-local f eff)
  (make-eff-op (make-effect 'reader-local f)
               (lambda (_) eff)))

;;; run-reader : r -> Eff Reader a -> a
;;; Handle reader effect.
(define (run-reader env eff)
  (cond
   [(eff-pure? eff)
    (eff-pure-value eff)]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(reader-ask)
                (run-reader env (k env))]
               [(reader-local)
                (let ([f (effect-payload effect)])
                     (run-reader (f env) (k '())))]
               [else
                ;; Unknown effect, can't handle
                (error 'run-reader "Unhandled effect" (effect-tag effect))]))]))

;;; ============================================================
;;; Writer Effect
;;; ============================================================

;;; writer-tell : w -> Eff Writer ()
(define (writer-tell msg)
  (perform (make-effect 'writer-tell msg)))

;;; run-writer : Eff Writer a -> (a, List w)
;;; Handle writer effect, collecting output.
(define (run-writer eff)
  (run-writer-helper '() eff))

(define (run-writer-helper log eff)
  (cond
   [(eff-pure? eff)
    (cons (eff-pure-value eff) (reverse log))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(writer-tell)
                (run-writer-helper (cons (effect-payload effect) log)
                                   (k '()))]
               [else
                (make-eff-op effect
                             (lambda (resp)
                                     (run-writer-helper log (k resp))))]))]))

;;; ============================================================
;;; Exception Effect
;;; ============================================================

;;; throw : e -> Eff Exception a
(define (eff-throw exn)
  (perform (make-effect 'throw exn)))

;;; catch : Eff Exception a -> (e -> Eff Exception a) -> Eff Exception a
;;; Handle exceptions with a handler.
(define (eff-catch eff handler)
  (cond
   [(eff-pure? eff) eff]
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

;;; ============================================================
;;; NonDet Effect (Non-determinism)
;;; ============================================================

;;; nondet-choose : List a -> Eff NonDet a
(define (nondet-choose options)
  (perform (make-effect 'nondet-choose options)))

;;; nondet-fail : Eff NonDet a
(define nondet-fail
  (nondet-choose '()))

;;; run-nondet : Eff NonDet a -> List a
;;; Handle non-determinism effect, collecting all results.
(define (run-nondet eff)
  (cond
   [(eff-pure? eff)
    (list (eff-pure-value eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(nondet-choose)
                (let ([options (effect-payload effect)])
                     (apply append
                            (map (lambda (opt) (run-nondet (k opt)))
                                 options)))]
               [else
                (error 'run-nondet "Unhandled effect" (effect-tag effect))]))]))

;;; run-nondet-first : Eff NonDet a -> Maybe a
;;; Handle non-determinism, returning first result.
(define (run-nondet-first eff)
  (let ([results (run-nondet eff)])
       (if (null? results)
           nothing
           (just (car results)))))

;;; ============================================================
;;; Console Effect
;;; ============================================================

;;; console-print : String -> Eff Console ()
(define (console-print msg)
  (perform (make-effect 'console-print msg)))

;;; console-read : Eff Console String
(define console-read
  (perform (make-effect 'console-read '())))

;;; run-console-pure : List String -> Eff Console a -> (a, List String)
;;; Pure handler: takes input list, returns output list.
(define (run-console-pure inputs eff)
  (run-console-helper inputs '() eff))

(define (run-console-helper inputs outputs eff)
  (cond
   [(eff-pure? eff)
    (cons (eff-pure-value eff) (reverse outputs))]
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

;;; ============================================================
;;; Async Effect (for futures/promises)
;;; ============================================================

;;; async-fork : Eff e a -> Eff Async (Future a)
(define (async-fork computation)
  (perform (make-effect 'async-fork computation)))

;;; async-await : Future a -> Eff Async a
(define (async-await future)
  (perform (make-effect 'async-await future)))

;;; run-async-sync : Eff Async a -> a
;;; Run async effects synchronously (no parallelism).
(define (run-async-sync eff)
  (cond
   [(eff-pure? eff)
    (eff-pure-value eff)]
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
                (error 'run-async-sync "Unhandled effect" (effect-tag effect))]))]))

;;; gensym counter
(define *gensym-counter* 0)
(define (eff-gensym prefix)
  (set! *gensym-counter* (+ *gensym-counter* 1))
  (cons prefix *gensym-counter*))

;;; ============================================================
;;; Effect Handlers (Generic)
;;; ============================================================

;;; make-handler : (a -> b) -> ((Effect, k) -> b) -> Handler
;;; Create a handler with return case and effect case.
(define (make-handler return-case effect-case)
  (list 'handler return-case effect-case))

;;; handler? : Any -> Boolean
(define (handler? x)
  (and (pair? x) (eq? (car x) 'handler)))

;;; handler-return : Handler -> (a -> b)
(define (handler-return h)
  (list-ref h 1))

;;; handler-effect : Handler -> ((Effect, k) -> b)
(define (handler-effect h)
  (list-ref h 2))

;;; handle : Handler -> Eff e a -> b
;;; Apply a handler to an effectful computation.
(define (handle handler eff)
  (cond
   [(eff-pure? eff)
    ((handler-return handler) (eff-pure-value eff))]
   [(eff-op? eff)
    ((handler-effect handler)
     (eff-op-effect eff)
     (lambda (resp)
             (handle handler ((eff-op-cont eff) resp))))]))

;;; ============================================================
;;; Combining Effects
;;; ============================================================

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

;;; eff-when : Bool -> Eff e () -> Eff e ()
(define (eff-when pred action)
  (if pred action (eff-return '())))

;;; eff-unless : Bool -> Eff e () -> Eff e ()
(define (eff-unless pred action)
  (eff-when (not pred) action))

;;; ============================================================
;;; Lift / Inject Effects
;;; ============================================================

;;; eff-lift : (a -> b) -> Eff e a -> Eff e b
;;; Same as eff-map.
(define eff-lift eff-map)

;;; eff-lift2 : (a -> b -> c) -> Eff e a -> Eff e b -> Eff e c
(define (eff-lift2 f ea eb)
  (eff-bind ea (lambda (a)
                       (eff-bind eb (lambda (b)
                                            (eff-return (f a b)))))))

;;; ============================================================
;;; Example: Stateful Counter
;;; ============================================================

;;; counter-increment : Eff State ()
(define counter-increment
  (state-modify add1))

;;; counter-decrement : Eff State ()
(define counter-decrement
  (state-modify (lambda (n) (- n 1))))

;;; counter-reset : Eff State ()
(define counter-reset
  (state-put 0))

;;; ============================================================
;;; Example: Logging with Writer
;;; ============================================================

;;; log-info : String -> Eff Writer ()
(define (log-info msg)
  (writer-tell (list 'info msg)))

;;; log-warn : String -> Eff Writer ()
(define (log-warn msg)
  (writer-tell (list 'warn msg)))

;;; log-error : String -> Eff Writer ()
(define (log-error msg)
  (writer-tell (list 'error msg)))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
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
