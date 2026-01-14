;;; playpen/loom/spell/event.ss — Event Handler Macros
;;;
;;; The on-event macro simplifies event handler registration with
;;; automatic destructuring and guard conditions.
;;;
;;; This is part of Spell: the DSL for weaving game logic.
;;;
;;; Exports:
;;;   on-event       Register a single event handler
;;;   on-events      Register multiple event handlers at once
;;;   def-handler    Define a named, reusable event handler

;;; ====
;;; On-Event Macro
;;; ====

;;; on-event : Dispatcher × EventType × Options... -> Void
;;;
;;; Register an event handler with optional destructuring and guards.
;;;
;;; Syntax variants:
;;;   (on-event dispatcher event-type handler-fn)
;;;   (on-event dispatcher event-type :when condition handler-fn)
;;;   (on-event dispatcher event-type :do (source target data) body ...)
;;;   (on-event dispatcher event-type :when condition :do (source target data) body ...)
;;;
;;; Example:
;;;   ;; Simple: just pass a function
;;;   (on-event game-dispatcher entity-died handle-death)
;;;
;;;   ;; With guard condition
;;;   (on-event game-dispatcher damage-dealt
;;;     :when (> (event-get evt 'amount) 10)
;;;     handle-big-damage)
;;;
;;;   ;; With destructuring
;;;   (on-event game-dispatcher entity-died
;;;     :do (killer victim data)
;;;       (display (string-append (entity-name victim) " was slain!\n")))
;;;
;;;   ;; Full form with guard and destructuring
;;;   (on-event game-dispatcher damage-dealt
;;;     :when (> amount 20)
;;;     :do (attacker target data)
;;;       (let ([amount (alist-ref data 'amount)])
;;;         (display "CRITICAL HIT!\n")))

(define-syntax on-event
  (syntax-rules (:when :do)
                ;; Simple handler function
                [(_ dispatcher event-type handler)
                 (dispatcher-subscribe! dispatcher 'event-type handler)]
                
                ;; Handler with guard condition
                [(_ dispatcher event-type :when condition handler)
                 (dispatcher-subscribe! dispatcher 'event-type
                                        (lambda (evt)
                                                (when condition
                                                      (handler evt))))]
                
                ;; Handler with destructuring
                [(_ dispatcher event-type :do (source target data) body ...)
                 (dispatcher-subscribe! dispatcher 'event-type
                                        (lambda (evt)
                                                (let ([source (event-source evt)]
                                                      [target (event-target evt)]
                                                      [data (event-data evt)])
                                                     body ...)))]
                
                ;; Handler with guard and destructuring
                [(_ dispatcher event-type :when condition :do (source target data) body ...)
                 (dispatcher-subscribe! dispatcher 'event-type
                                        (lambda (evt)
                                                (let ([source (event-source evt)]
                                                      [target (event-target evt)]
                                                      [data (event-data evt)])
                                                     (when condition
                                                           body ...))))]
                
                ;; Guard after destructuring (alternative order)
                [(_ dispatcher event-type :do (source target data) :when condition body ...)
                 (dispatcher-subscribe! dispatcher 'event-type
                                        (lambda (evt)
                                                (let ([source (event-source evt)]
                                                      [target (event-target evt)]
                                                      [data (event-data evt)])
                                                     (when condition
                                                           body ...))))]))


;;; ====
;;; On-Events Macro (Batch Registration)
;;; ====

;;; on-events : Dispatcher × (EventType Handler)... -> Void
;;;
;;; Register multiple event handlers at once.
;;;
;;; Example:
;;;   (on-events game-dispatcher
;;;     (entity-died handle-death)
;;;     (entity-spawned handle-spawn)
;;;     (turn-started handle-turn-start)
;;;     (turn-ended handle-turn-end))

(define-syntax on-events
  (syntax-rules ()
                [(_ dispatcher (event-type handler) ...)
                 (begin
                  (on-event dispatcher event-type handler) ...)]))


;;; ====
;;; Define Handler Macro
;;; ====

;;; def-handler : Name × EventType × (Bindings...) × Body... -> Definition
;;;
;;; Define a named, reusable event handler with automatic destructuring.
;;;
;;; Example:
;;;   (def-handler death-message entity-died (killer victim data)
;;;     (let ([victim-name (entity-name (world-get-entity world victim))])
;;;       (display (string-append victim-name " has fallen!\n"))))
;;;
;;;   ;; Later, register it:
;;;   (on-event game-dispatcher entity-died death-message)

(define-syntax def-handler
  (syntax-rules ()
                [(_ name event-type (source target data) body ...)
                 (define (name evt)
                   (let ([source (event-source evt)]
                         [target (event-target evt)]
                         [data (event-data evt)])
                        body ...))]))


;;; ====
;;; Event Firing Helpers
;;; ====

;;; emit! : Dispatcher × EventType × Source × Target × Data -> Void
;;; Fire an event immediately.
;;;
;;; Example:
;;;   (emit! game-dispatcher 'entity-damaged player-id target-id
;;;          `((amount . 15) (type . fire)))

(define-syntax emit!
  (syntax-rules ()
                [(_ dispatcher event-type source target data)
                 (dispatcher-dispatch! dispatcher
                                       (make-event 'event-type source target data))]
                
                [(_ dispatcher event-type source target)
                 (dispatcher-dispatch! dispatcher
                                       (make-event 'event-type source target '()))]
                
                [(_ dispatcher event-type source)
                 (dispatcher-dispatch! dispatcher
                                       (make-event 'event-type source #f '()))]
                
                [(_ dispatcher event-type)
                 (dispatcher-dispatch! dispatcher
                                       (make-event 'event-type #f #f '()))]))


;;; queue! : Dispatcher × EventType × Source × Target × Data -> Void
;;; Add an event to the dispatcher's queue for deferred processing.
;;;
;;; Example:
;;;   (queue! game-dispatcher 'level-complete #f #f '((level . 3)))

(define-syntax queue!
  (syntax-rules ()
                [(_ dispatcher event-type source target data)
                 (dispatcher-enqueue! dispatcher
                                      (make-event 'event-type source target data))]
                
                [(_ dispatcher event-type source target)
                 (dispatcher-enqueue! dispatcher
                                      (make-event 'event-type source target '()))]
                
                [(_ dispatcher event-type source)
                 (dispatcher-enqueue! dispatcher
                                      (make-event 'event-type source #f '()))]
                
                [(_ dispatcher event-type)
                 (dispatcher-enqueue! dispatcher
                                      (make-event 'event-type #f #f '()))]))


;;; ====
;;; Example Usage (commented out)
;;; ====

#|
;;; Create a dispatcher
(define game-dispatcher (make-dispatcher #t))

;;; Register handlers
(on-event game-dispatcher entity-died
          :do (killer victim data)
          (display (format "Entity ~a died!\n" victim)))

(on-event game-dispatcher damage-dealt
          :when (> (event-get evt 'amount) 10)
          :do (attacker target data)
          (display "Big hit!\n"))

;;; Batch registration
(on-events game-dispatcher
           (turn-started (lambda (e) (display "Turn started\n")))
           (turn-ended (lambda (e) (display "Turn ended\n"))))

;;; Define reusable handler
(def-handler log-death entity-died (killer victim data)
  (display (format "~a killed ~a\n" killer victim)))

(on-event game-dispatcher entity-died log-death)

;;; Fire events
(emit! game-dispatcher entity-died 42 99 '((weapon . sword)))
|#
