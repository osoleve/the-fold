(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @module des/engine
;;; @requires prelude stream des/event des/world
(require 'prelude)
(require 'stream)
(require 'des/event)
(require 'des/world)

(doc 'module 'des/engine)
(doc 'description "Discrete Event Simulation — engine. The core DES loop: pop next event, advance clock, dispatch handler, yield world snapshot. Produces a lazy stream of world states. Pure and fuel-bounded.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;;---------------------------------------------------------------------------
;;; Handler dispatch table
;;;---------------------------------------------------------------------------

(doc 'section 'handlers)

(define (make-handler-table . pairs)
  (doc 'type '(-> (Symbol (-> DESWorld DESEvent DESWorld)) ... HandlerTable))
  (doc 'description "Build a handler dispatch table from alternating type/handler pairs. Each handler is (world event -> world).")
  (let loop ([ps pairs] [acc '()])
    (if (null? ps) acc
        (if (null? (cdr ps))
            (error 'make-handler-table "odd number of arguments")
            (loop (cddr ps) (cons (cons (car ps) (cadr ps)) acc))))))

(define (handler-lookup table event-type)
  (doc 'type '(-> HandlerTable Symbol (Option Handler)))
  (let ([pair (assq event-type table)])
    (if pair (cdr pair) #f)))

;;;---------------------------------------------------------------------------
;;; DES Configuration
;;;---------------------------------------------------------------------------

(doc 'section 'config)

(define (make-des-config handlers stop-time max-events)
  (doc 'type '(-> HandlerTable Number Nat DESConfig))
  (doc 'description "Configuration for a DES run. handlers: dispatch table. stop-time: clock limit. max-events: fuel (max events processed).")
  (list 'des-config handlers stop-time max-events))

(define (des-config? x) (and (pair? x) (eq? (car x) 'des-config)))
(define (des-config-handlers c) (list-ref c 1))
(define (des-config-stop-time c) (list-ref c 2))
(define (des-config-max-events c) (list-ref c 3))

;;;---------------------------------------------------------------------------
;;; Single step
;;;---------------------------------------------------------------------------

(doc 'section 'step)

(define (des-step config world)
  (doc 'type '(-> DESConfig DESWorld (Option DESWorld)))
  (doc 'description "Execute one DES step: pop next event, advance clock, dispatch handler. Returns #f if simulation should stop (empty queue, past stop-time, or no handler).")
  (let ([q (des-world-queue world)])
    (if (eq-empty? q) #f
        (let ([next-event (eq-next q)])
          (if (> (des-event-time next-event) (des-config-stop-time config)) #f
              (let-values ([(event rest-q) (eq-pop q)])
                (let* ([w1 (set-lens world-queue rest-q world)]
                       [w2 (set-lens world-clock (des-event-time event) w1)]
                       [handler (handler-lookup (des-config-handlers config)
                                                (des-event-type event))])
                  (if handler
                      (handler w2 event)
                      w2))))))))

;;;---------------------------------------------------------------------------
;;; Stream-based simulation
;;;---------------------------------------------------------------------------

(doc 'section 'simulation-stream)

(define (des-run config initial-world)
  (doc 'type '(-> DESConfig DESWorld (Stream DESWorld)))
  (doc 'description "Run a DES, producing a lazy stream of world states. Each element is the world after processing one event. Stream terminates when: queue empties, clock exceeds stop-time, or max-events fuel exhausted.")
  (let ([max-events (des-config-max-events config)])
    (stream-unfold
     (lambda (seed)
       (let ([w (car seed)]
             [n (cdr seed)])
         (if (>= n max-events) nothing
             (let ([next (des-step config w)])
               (if next
                   (just (cons next (cons next (+ n 1))))
                   nothing)))))
     (cons initial-world 0))))

(define (des-run-list config initial-world)
  (doc 'type '(-> DESConfig DESWorld (List DESWorld)))
  (doc 'description "Run a DES to completion, returning a list of world states. Bounded by config stop-time and max-events.")
  (stream->list (des-config-max-events config) (des-run config initial-world)))

(define (des-run-final config initial-world)
  (doc 'type '(-> DESConfig DESWorld DESWorld))
  (doc 'description "Run a DES to completion, returning only the final world state.")
  (let loop ([w initial-world] [n 0])
    (if (>= n (des-config-max-events config)) w
        (let ([next (des-step config w)])
          (if next (loop next (+ n 1)) w)))))

;;;---------------------------------------------------------------------------
;;; Observation extractors
;;;---------------------------------------------------------------------------

(doc 'section 'observation)

(define (des-trace-metric config initial-world metric-name)
  (doc 'type '(-> DESConfig DESWorld Symbol (List (Pair Number Number))))
  (doc 'description "Run simulation and extract (time . metric-value) pairs for a named metric.")
  (map (lambda (w)
         (cons (des-world-clock w) (world-metric w metric-name)))
       (des-run-list config initial-world)))

(define (des-trace-entity config initial-world entity-id accessor)
  (doc 'type '(-> DESConfig DESWorld Symbol (-> Any Any) (List (Pair Number Any))))
  (doc 'description "Run simulation and extract (time . value) for an entity field over time.")
  (map (lambda (w)
         (let ([e (world-entity w entity-id)])
           (cons (des-world-clock w) (if e (accessor e) #f))))
       (des-run-list config initial-world)))

(define (des-snapshot-times config initial-world times)
  (doc 'type '(-> DESConfig DESWorld (List Number) (List DESWorld)))
  (doc 'description "Run simulation and capture world states at specific clock times.")
  (let ([sorted-times (list-sort < times)]
        [all-states (des-run-list config initial-world)])
    (let loop ([states all-states] [ts sorted-times] [acc '()])
      (if (or (null? ts) (null? states)) (reverse acc)
          (let ([w (car states)]
                [target (car ts)])
            (if (>= (des-world-clock w) target)
                (loop states (cdr ts) (cons w acc))
                (loop (cdr states) ts acc)))))))
