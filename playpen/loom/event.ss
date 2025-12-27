;;; playpen/loom/event.ss — Event System for RPG SDK
;;;
;;; Event-driven architecture for game state management.
;;; Provides event creation, queuing, and pub/sub dispatch pattern.
;;;
;;; Events represent game occurrences (entity moved, damage dealt, etc.)
;;; and can be queued for deferred processing or immediately dispatched
;;; to registered handlers.
;;;
;;; Dependencies:
;;;   - playpen/loom/core.ss (alist utilities)
;;;
;;; Exports:
;;;   Event creation and accessors
;;;   Event queue (FIFO)
;;;   Event dispatcher (pub/sub pattern)
;;;   Convenience constructors for common event types

;;; ============================================================
;;; Event Types
;;; ============================================================

;;; EventType : Symbol
;;; Predefined event types for common game occurrences.
;;; Games can also define custom event types.

(define event-types
  '(;; Entity events
    entity-moved
    entity-died
    entity-spawned
    entity-damaged
    entity-healed

    ;; Combat events
    damage-dealt
    attack-missed
    attack-critical

    ;; Item events
    item-picked-up
    item-dropped
    item-used
    item-equipped
    item-unequipped

    ;; Turn events
    turn-started
    turn-ended

    ;; World events
    tile-changed
    door-opened
    door-closed
    trap-triggered
    trap-disarmed

    ;; Level events
    level-entered
    level-exited
    level-generated

    ;; Custom event type
    custom))

;;; event-type? : Symbol -> Bool
;;; Check if a symbol is a valid event type.
;;; Returns #t for predefined types and any custom symbols.
(define (event-type? sym)
  (symbol? sym))

;;; ============================================================
;;; Event Structure
;;; ============================================================

;;; Event : Record
;;;   type      — Symbol, event type identifier
;;;   source    — Any, entity ID or #f (who/what caused the event)
;;;   target    — Any, entity ID, point, or #f (who/what was affected)
;;;   data      — Alist, additional event-specific data
;;;   timestamp — Nat, turn number when event occurred

(define-record-type event%
  (fields type source target data timestamp))

;;; make-event : Symbol × [Any] × [Any] × [Alist] × [Nat] -> Event
;;; Create a new event with optional parameters.
(define make-event
  (case-lambda
    [(type)
     (make-event% type #f #f '() 0)]
    [(type source)
     (make-event% type source #f '() 0)]
    [(type source target)
     (make-event% type source target '() 0)]
    [(type source target data)
     (make-event% type source target data 0)]
    [(type source target data timestamp)
     (make-event% type source target data timestamp)]))

;;; Accessors
(define event-type event%-type)
(define event-source event%-source)
(define event-target event%-target)
(define event-data event%-data)
(define event-timestamp event%-timestamp)

;;; event-get : Event × Symbol × [Any] -> Any
;;; Get a value from event data, with optional default.
(define event-get
  (case-lambda
    [(evt key)
     (alist-ref (event-data evt) key)]
    [(evt key default)
     (alist-ref (event-data evt) key default)]))

;;; event-set : Event × Symbol × Any -> Event
;;; Create a new event with updated data.
(define (event-set evt key value)
  (make-event% (event-type evt)
               (event-source evt)
               (event-target evt)
               (alist-set (event-data evt) key value)
               (event-timestamp evt)))

;;; event-with-timestamp : Event × Nat -> Event
;;; Create a new event with updated timestamp.
(define (event-with-timestamp evt timestamp)
  (make-event% (event-type evt)
               (event-source evt)
               (event-target evt)
               (event-data evt)
               timestamp))

;;; ============================================================
;;; Event Queue
;;; ============================================================

;;; EventQueue : Record
;;;   events — List Event, FIFO queue (front is head of list)
;;;
;;; A simple queue for managing pending events.
;;; Events are added to the back and removed from the front.

(define-record-type event-queue%
  (fields (mutable events)))

;;; make-event-queue : -> EventQueue
;;; Create an empty event queue.
(define (make-event-queue)
  (make-event-queue% '()))

;;; event-queue-empty? : EventQueue -> Bool
;;; Check if the queue is empty.
(define (event-queue-empty? queue)
  (null? (event-queue%-events queue)))

;;; event-queue-size : EventQueue -> Nat
;;; Get the number of events in the queue.
(define (event-queue-size queue)
  (length (event-queue%-events queue)))

;;; event-queue-push! : EventQueue × Event -> Void
;;; Add an event to the back of the queue.
(define (event-queue-push! queue event)
  (event-queue%-events-set! queue
    (append (event-queue%-events queue) (list event))))

;;; event-queue-pop! : EventQueue -> Event | #f
;;; Remove and return the front event, or #f if empty.
(define (event-queue-pop! queue)
  (let ([events (event-queue%-events queue)])
    (if (null? events)
        #f
        (let ([front (car events)])
          (event-queue%-events-set! queue (cdr events))
          front))))

;;; event-queue-peek : EventQueue -> Event | #f
;;; Return the front event without removing it, or #f if empty.
(define (event-queue-peek queue)
  (let ([events (event-queue%-events queue)])
    (if (null? events) #f (car events))))

;;; event-queue-clear! : EventQueue -> Void
;;; Remove all events from the queue.
(define (event-queue-clear! queue)
  (event-queue%-events-set! queue '()))

;;; event-queue-filter! : EventQueue × (Event -> Bool) -> Void
;;; Remove events that don't match the predicate.
(define (event-queue-filter! queue pred)
  (event-queue%-events-set! queue
    (filter pred (event-queue%-events queue))))

;;; event-queue->list : EventQueue -> List Event
;;; Get all events as a list (without removing them).
(define (event-queue->list queue)
  (event-queue%-events queue))

;;; ============================================================
;;; Event Dispatcher
;;; ============================================================

;;; EventHandler : Event -> Any
;;; A handler function that processes an event.

;;; EventDispatcher : Record
;;;   handlers — Alist (Symbol . List EventHandler), maps event types to handlers
;;;   queue    — EventQueue, optional queue for deferred processing
;;;
;;; Pub/sub pattern for event handling. Handlers can subscribe to specific
;;; event types and will be called when those events are dispatched.

(define-record-type event-dispatcher%
  (fields (mutable handlers) queue))

;;; make-dispatcher : [Bool] -> EventDispatcher
;;; Create an event dispatcher.
;;; If with-queue? is #t, includes an event queue for deferred processing.
(define make-dispatcher
  (case-lambda
    [() (make-event-dispatcher% '() #f)]
    [(with-queue?)
     (make-event-dispatcher% '()
                             (if with-queue?
                                 (make-event-queue)
                                 #f))]))

;;; dispatcher-handlers : EventDispatcher -> Alist
(define dispatcher-handlers event-dispatcher%-handlers)

;;; dispatcher-queue : EventDispatcher -> EventQueue | #f
(define dispatcher-queue event-dispatcher%-queue)

;;; dispatcher-subscribe! : EventDispatcher × Symbol × EventHandler -> Void
;;; Register a handler for a specific event type.
;;; Multiple handlers can be registered for the same event type.
(define (dispatcher-subscribe! dispatcher event-type handler)
  (let* ([handlers (dispatcher-handlers dispatcher)]
         [type-handlers (alist-ref handlers event-type '())]
         [new-handlers (cons handler type-handlers)])
    (event-dispatcher%-handlers-set! dispatcher
      (alist-set handlers event-type new-handlers))))

;;; dispatcher-unsubscribe! : EventDispatcher × Symbol × EventHandler -> Void
;;; Remove a specific handler for an event type.
;;; Uses eq? to compare handler functions.
(define (dispatcher-unsubscribe! dispatcher event-type handler)
  (let* ([handlers (dispatcher-handlers dispatcher)]
         [type-handlers (alist-ref handlers event-type '())]
         [new-handlers (filter (lambda (h) (not (eq? h handler))) type-handlers)])
    (event-dispatcher%-handlers-set! dispatcher
      (if (null? new-handlers)
          (alist-remove handlers event-type)
          (alist-set handlers event-type new-handlers)))))

;;; dispatcher-unsubscribe-all! : EventDispatcher × Symbol -> Void
;;; Remove all handlers for a specific event type.
(define (dispatcher-unsubscribe-all! dispatcher event-type)
  (event-dispatcher%-handlers-set! dispatcher
    (alist-remove (dispatcher-handlers dispatcher) event-type)))

;;; dispatcher-clear-handlers! : EventDispatcher -> Void
;;; Remove all handlers from the dispatcher.
(define (dispatcher-clear-handlers! dispatcher)
  (event-dispatcher%-handlers-set! dispatcher '()))

;;; dispatcher-dispatch! : EventDispatcher × Event -> List Any
;;; Immediately invoke all handlers for the event's type.
;;; Returns a list of handler return values.
(define (dispatcher-dispatch! dispatcher event)
  (let* ([handlers (dispatcher-handlers dispatcher)]
         [type-handlers (alist-ref handlers (event-type event) '())])
    (map (lambda (handler)
           (handler event))
         type-handlers)))

;;; dispatcher-enqueue! : EventDispatcher × Event -> Void
;;; Add an event to the dispatcher's queue for deferred processing.
;;; Requires dispatcher to be created with a queue.
(define (dispatcher-enqueue! dispatcher event)
  (let ([queue (dispatcher-queue dispatcher)])
    (if queue
        (event-queue-push! queue event)
        (error 'dispatcher-enqueue!
               "Dispatcher does not have a queue"))))

;;; dispatcher-process-queue! : EventDispatcher × [Nat] -> Nat
;;; Process events from the queue.
;;; If max-events is specified, processes at most that many events.
;;; Returns the number of events processed.
(define dispatcher-process-queue!
  (case-lambda
    [(dispatcher)
     (dispatcher-process-queue! dispatcher #f)]
    [(dispatcher max-events)
     (let ([queue (dispatcher-queue dispatcher)])
       (if (not queue)
           (error 'dispatcher-process-queue!
                  "Dispatcher does not have a queue")
           (let loop ([count 0])
             (if (or (event-queue-empty? queue)
                     (and max-events (>= count max-events)))
                 count
                 (let ([event (event-queue-pop! queue)])
                   (dispatcher-dispatch! dispatcher event)
                   (loop (+ count 1)))))))]))

;;; dispatcher-has-handlers? : EventDispatcher × Symbol -> Bool
;;; Check if any handlers are registered for an event type.
(define (dispatcher-has-handlers? dispatcher event-type)
  (let ([type-handlers (alist-ref (dispatcher-handlers dispatcher) event-type '())])
    (not (null? type-handlers))))

;;; dispatcher-handler-count : EventDispatcher × Symbol -> Nat
;;; Get the number of handlers registered for an event type.
(define (dispatcher-handler-count dispatcher event-type)
  (length (alist-ref (dispatcher-handlers dispatcher) event-type '())))

;;; ============================================================
;;; Convenience Event Constructors
;;; ============================================================

;;; Entity Events

;;; event-entity-moved : Nat × Point × Point × [Nat] -> Event
;;; Create an entity-moved event.
;;; Data: from (Point), to (Point), direction (Symbol, optional)
(define event-entity-moved
  (case-lambda
    [(entity-id from to)
     (make-event 'entity-moved entity-id to
                 `((from . ,from) (to . ,to)))]
    [(entity-id from to timestamp)
     (make-event 'entity-moved entity-id to
                 `((from . ,from) (to . ,to))
                 timestamp)]))

;;; event-entity-died : Nat × Any × [Alist] × [Nat] -> Event
;;; Create an entity-died event.
;;; killer: entity ID or other cause (#f, 'trap, 'poison, etc.)
(define event-entity-died
  (case-lambda
    [(entity-id killer)
     (make-event 'entity-died killer entity-id
                 `((victim . ,entity-id) (killer . ,killer)))]
    [(entity-id killer data)
     (make-event 'entity-died killer entity-id
                 (alist-set data 'victim entity-id))]
    [(entity-id killer data timestamp)
     (make-event 'entity-died killer entity-id
                 (alist-set data 'victim entity-id)
                 timestamp)]))

;;; event-entity-spawned : Nat × Point × Symbol × [Nat] -> Event
;;; Create an entity-spawned event.
;;; Data: position (Point), entity-type (Symbol)
(define event-entity-spawned
  (case-lambda
    [(entity-id position entity-type)
     (make-event 'entity-spawned #f entity-id
                 `((position . ,position) (entity-type . ,entity-type)))]
    [(entity-id position entity-type timestamp)
     (make-event 'entity-spawned #f entity-id
                 `((position . ,position) (entity-type . ,entity-type))
                 timestamp)]))

;;; Combat Events

;;; event-damage-dealt : Nat × Nat × Nat × [Symbol] × [Nat] -> Event
;;; Create a damage-dealt event.
;;; Data: amount (Nat), damage-type (Symbol, optional)
(define event-damage-dealt
  (case-lambda
    [(attacker-id target-id amount)
     (make-event 'damage-dealt attacker-id target-id
                 `((amount . ,amount)))]
    [(attacker-id target-id amount damage-type)
     (make-event 'damage-dealt attacker-id target-id
                 `((amount . ,amount) (damage-type . ,damage-type)))]
    [(attacker-id target-id amount damage-type timestamp)
     (make-event 'damage-dealt attacker-id target-id
                 `((amount . ,amount) (damage-type . ,damage-type))
                 timestamp)]))

;;; Item Events

;;; event-item-picked-up : Nat × Nat × [Nat] -> Event
;;; Create an item-picked-up event.
;;; Data: item-id (Nat)
(define event-item-picked-up
  (case-lambda
    [(entity-id item-id)
     (make-event 'item-picked-up entity-id item-id
                 `((item-id . ,item-id)))]
    [(entity-id item-id timestamp)
     (make-event 'item-picked-up entity-id item-id
                 `((item-id . ,item-id))
                 timestamp)]))

;;; event-item-dropped : Nat × Nat × Point × [Nat] -> Event
;;; Create an item-dropped event.
;;; Data: item-id (Nat), position (Point)
(define event-item-dropped
  (case-lambda
    [(entity-id item-id position)
     (make-event 'item-dropped entity-id item-id
                 `((item-id . ,item-id) (position . ,position)))]
    [(entity-id item-id position timestamp)
     (make-event 'item-dropped entity-id item-id
                 `((item-id . ,item-id) (position . ,position))
                 timestamp)]))

;;; Turn Events

;;; event-turn-started : Nat × [Nat] -> Event
;;; Create a turn-started event.
;;; Data: turn-number (Nat)
(define event-turn-started
  (case-lambda
    [(turn-number)
     (make-event 'turn-started #f #f
                 `((turn-number . ,turn-number))
                 turn-number)]
    [(turn-number timestamp)
     (make-event 'turn-started #f #f
                 `((turn-number . ,turn-number))
                 timestamp)]))

;;; event-turn-ended : Nat × [Nat] -> Event
;;; Create a turn-ended event.
;;; Data: turn-number (Nat)
(define event-turn-ended
  (case-lambda
    [(turn-number)
     (make-event 'turn-ended #f #f
                 `((turn-number . ,turn-number))
                 turn-number)]
    [(turn-number timestamp)
     (make-event 'turn-ended #f #f
                 `((turn-number . ,turn-number))
                 timestamp)]))

;;; World Events

;;; event-tile-changed : Point × Symbol × Symbol × [Nat] -> Event
;;; Create a tile-changed event.
;;; Data: position (Point), old-type (Symbol), new-type (Symbol)
(define event-tile-changed
  (case-lambda
    [(position old-type new-type)
     (make-event 'tile-changed #f position
                 `((position . ,position)
                   (old-type . ,old-type)
                   (new-type . ,new-type)))]
    [(position old-type new-type timestamp)
     (make-event 'tile-changed #f position
                 `((position . ,position)
                   (old-type . ,old-type)
                   (new-type . ,new-type))
                 timestamp)]))

;;; event-door-opened : Point × [Nat] × [Nat] -> Event
;;; Create a door-opened event.
;;; Data: position (Point), opener (Nat, entity ID, optional)
(define event-door-opened
  (case-lambda
    [(position)
     (make-event 'door-opened #f position
                 `((position . ,position)))]
    [(position opener-id)
     (make-event 'door-opened opener-id position
                 `((position . ,position) (opener . ,opener-id)))]
    [(position opener-id timestamp)
     (make-event 'door-opened opener-id position
                 `((position . ,position) (opener . ,opener-id))
                 timestamp)]))

;;; event-trap-triggered : Point × Nat × [Nat] -> Event
;;; Create a trap-triggered event.
;;; Data: position (Point), victim (Nat, entity ID)
(define event-trap-triggered
  (case-lambda
    [(position victim-id)
     (make-event 'trap-triggered #f victim-id
                 `((position . ,position) (victim . ,victim-id)))]
    [(position victim-id timestamp)
     (make-event 'trap-triggered #f victim-id
                 `((position . ,position) (victim . ,victim-id))
                 timestamp)]))

;;; Level Events

;;; event-level-entered : Nat × Nat × [Nat] -> Event
;;; Create a level-entered event.
;;; Data: level-id (Nat), entity-id (Nat)
(define event-level-entered
  (case-lambda
    [(level-id entity-id)
     (make-event 'level-entered entity-id #f
                 `((level-id . ,level-id) (entity-id . ,entity-id)))]
    [(level-id entity-id timestamp)
     (make-event 'level-entered entity-id #f
                 `((level-id . ,level-id) (entity-id . ,entity-id))
                 timestamp)]))

;;; event-level-exited : Nat × Nat × [Nat] -> Event
;;; Create a level-exited event.
;;; Data: level-id (Nat), entity-id (Nat)
(define event-level-exited
  (case-lambda
    [(level-id entity-id)
     (make-event 'level-exited entity-id #f
                 `((level-id . ,level-id) (entity-id . ,entity-id)))]
    [(level-id entity-id timestamp)
     (make-event 'level-exited entity-id #f
                 `((level-id . ,level-id) (entity-id . ,entity-id))
                 timestamp)]))

;;; Custom Events

;;; event-custom : Symbol × [Any] × [Any] × [Alist] × [Nat] -> Event
;;; Create a custom event with arbitrary data.
(define event-custom
  (case-lambda
    [(custom-type)
     (make-event custom-type #f #f '() 0)]
    [(custom-type source)
     (make-event custom-type source #f '() 0)]
    [(custom-type source target)
     (make-event custom-type source target '() 0)]
    [(custom-type source target data)
     (make-event custom-type source target data 0)]
    [(custom-type source target data timestamp)
     (make-event custom-type source target data timestamp)]))

;;; ============================================================
;;; Event Utilities
;;; ============================================================

;;; event-match? : Event × Symbol × [Any] × [Any] -> Bool
;;; Check if an event matches criteria.
;;; Source and target can be #f to match any value.
(define event-match?
  (case-lambda
    [(event type)
     (eq? (event-type event) type)]
    [(event type source)
     (and (eq? (event-type event) type)
          (or (not source)
              (equal? (event-source event) source)))]
    [(event type source target)
     (and (eq? (event-type event) type)
          (or (not source)
              (equal? (event-source event) source))
          (or (not target)
              (equal? (event-target event) target)))]))

;;; event-filter : List Event × (Event -> Bool) -> List Event
;;; Filter a list of events by a predicate.
(define (event-filter events pred)
  (filter pred events))

;;; event-filter-type : List Event × Symbol -> List Event
;;; Filter events by type.
(define (event-filter-type events type)
  (filter (lambda (e) (eq? (event-type e) type)) events))

;;; event-sort-by-timestamp : List Event -> List Event
;;; Sort events by timestamp (ascending).
(define (event-sort-by-timestamp events)
  (list-sort (lambda (a b)
               (< (event-timestamp a) (event-timestamp b)))
             events))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Event Types:
;;;   event-types, event-type?
;;;
;;; Event Structure:
;;;   make-event, event-type, event-source, event-target, event-data, event-timestamp
;;;   event-get, event-set, event-with-timestamp
;;;
;;; Event Queue:
;;;   make-event-queue, event-queue-empty?, event-queue-size
;;;   event-queue-push!, event-queue-pop!, event-queue-peek
;;;   event-queue-clear!, event-queue-filter!, event-queue->list
;;;
;;; Event Dispatcher:
;;;   make-dispatcher, dispatcher-handlers, dispatcher-queue
;;;   dispatcher-subscribe!, dispatcher-unsubscribe!, dispatcher-unsubscribe-all!
;;;   dispatcher-clear-handlers!, dispatcher-dispatch!, dispatcher-enqueue!
;;;   dispatcher-process-queue!, dispatcher-has-handlers?, dispatcher-handler-count
;;;
;;; Event Constructors:
;;;   event-entity-moved, event-entity-died, event-entity-spawned
;;;   event-damage-dealt
;;;   event-item-picked-up, event-item-dropped
;;;   event-turn-started, event-turn-ended
;;;   event-tile-changed, event-door-opened, event-trap-triggered
;;;   event-level-entered, event-level-exited
;;;   event-custom
;;;
;;; Event Utilities:
;;;   event-match?, event-filter, event-filter-type, event-sort-by-timestamp
