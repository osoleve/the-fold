(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @module des/event
;;; @requires prelude heap
(require 'prelude)
(require 'heap)

(doc 'module 'des/event)
(doc 'description "Discrete Event Simulation — event types and time-ordered event queue. Events carry a scheduled time, a type tag, and an arbitrary payload. The event queue is a min-heap keyed on time, giving O(log n) schedule and O(1) peek-next.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;;---------------------------------------------------------------------------
;;; Event type
;;;---------------------------------------------------------------------------

(doc 'section 'event-type)

(define (make-des-event time type payload)
  (doc 'type '(-> Number Symbol Any DESEvent))
  (doc 'description "Create a discrete event scheduled at time, with type tag and payload.")
  (list 'des-event time type payload))

(define (des-event? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'des-event)))

(define (des-event-time e)
  (doc 'type '(-> DESEvent Number))
  (list-ref e 1))

(define (des-event-type e)
  (doc 'type '(-> DESEvent Symbol))
  (list-ref e 2))

(define (des-event-payload e)
  (doc 'type '(-> DESEvent Any))
  (list-ref e 3))

(define (des-event-with-time e t)
  (doc 'type '(-> DESEvent Number DESEvent))
  (make-des-event t (des-event-type e) (des-event-payload e)))

(define (des-event-with-payload e p)
  (doc 'type '(-> DESEvent Any DESEvent))
  (make-des-event (des-event-time e) (des-event-type e) p))

;;;---------------------------------------------------------------------------
;;; Event queue (min-heap on event time)
;;;---------------------------------------------------------------------------

(doc 'section 'event-queue)

(define eq-empty heap-empty)
(doc eq-empty 'description "Empty event queue.")

(define (eq-empty? q)
  (doc 'type '(-> EventQueue Boolean))
  (heap-empty? q))

(define (des-event<? a b)
  (doc 'type '(-> DESEvent DESEvent Boolean))
  (doc 'description "Time-ordering comparator for events.")
  (< (des-event-time a) (des-event-time b)))

(define (eq-schedule q event)
  (doc 'type '(-> EventQueue DESEvent EventQueue))
  (doc 'description "Schedule an event into the queue. O(log n).")
  (heap-insert-by des-event<? event q))

(define (eq-schedule* q events)
  (doc 'type '(-> EventQueue (List DESEvent) EventQueue))
  (doc 'description "Schedule multiple events into the queue.")
  (fold-left (lambda (q e) (eq-schedule q e)) q events))

(define (eq-next q)
  (doc 'type '(-> EventQueue DESEvent))
  (doc 'description "Peek at the next (earliest) event. O(1).")
  (heap-value q))

(define (eq-pop q)
  (doc 'type '(-> EventQueue (Values DESEvent EventQueue)))
  (doc 'description "Remove and return the next event. O(log n). Returns (values event rest-queue).")
  (let ([event (heap-value q)]
        [rest (heap-delete-top-by des-event<? q)])
    (values event rest)))

(define (eq-size q)
  (doc 'type '(-> EventQueue Nat))
  (heap-size q))

(define (eq-from-list events)
  (doc 'type '(-> (List DESEvent) EventQueue))
  (doc 'description "Build an event queue from a list of events.")
  (eq-schedule* eq-empty events))

(define (eq-to-list q)
  (doc 'type '(-> EventQueue (List DESEvent)))
  (doc 'description "Drain the queue into a time-sorted list of events.")
  (let loop ([q q] [acc '()])
    (if (eq-empty? q) (reverse acc)
        (let-values ([(e rest) (eq-pop q)])
          (loop rest (cons e acc))))))

(define (eq-filter pred q)
  (doc 'type '(-> (-> DESEvent Boolean) EventQueue EventQueue))
  (doc 'description "Remove events not matching predicate. Rebuilds queue.")
  (eq-from-list (filter pred (eq-to-list q))))

(define (eq-cancel-type type q)
  (doc 'type '(-> Symbol EventQueue EventQueue))
  (doc 'description "Cancel all events of a given type.")
  (eq-filter (lambda (e) (not (eq? (des-event-type e) type))) q))
