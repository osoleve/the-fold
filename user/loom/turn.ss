;;; playpen/loom/turn.ss — Turn-Based Game Loop System for RPG SDK
;;;
;;; Implements a flexible turn-based system with energy-based turn order,
;;; phase management, and event generation. Entities accumulate energy based
;;; on their speed stat and can act when they reach the energy threshold.
;;;
;;; This is Playpen code: the SDK for building roguelikes and dungeon crawlers.
;;;
;;; Dependencies:
;;;   - playpen/loom/core.ss (alist utilities, clamp)
;;;   - playpen/loom/entity.ss (entity system, actor component)
;;;   - playpen/loom/action.ss (action system, action queue)
;;;   - playpen/loom/event.ss (event system)
;;;
;;; Architecture:
;;;   - Energy-based initiative: entities gain energy each tick
;;;   - When energy >= threshold (default 100), entity can act
;;;   - Actions cost energy, faster entities act more frequently
;;;   - Turn phases: start-turn -> get-input -> process-action -> resolve-effects -> end-turn
;;;   - Supports both player-paced and continuous turn modes
;;;
;;; Exports:
;;;   Turn state, actor queue, phase management, turn processing

;;; ============================================================
;;; Turn Configuration
;;; ============================================================

;;; Default energy threshold for taking an action
(define *default-energy-threshold* 100)

;;; Default energy gain per tick (modified by speed)
(define *default-energy-gain* 10)

;;; Turn Phases : Symbol
;;; Valid turn phase identifiers
(define turn-phases
  '(start-turn    ; Beginning of turn, apply start-of-turn effects
    get-input     ; Waiting for player input
    process-action; Processing the chosen action
    resolve-effects; Resolving side effects and triggers
    end-turn      ; End of turn, apply end-of-turn effects
    animate       ; Animation/visual feedback (optional)
    ai))          ; AI decision making (for NPCs)

(define (turn-phase? phase)
  (if (memq phase turn-phases) #t #f))

;;; ============================================================
;;; Actor Queue Entry
;;; ============================================================

;;; ActorEntry : Record
;;;   entity-id  — Nat, the entity's ID
;;;   energy     — Nat, current energy level
;;;   initiative — Nat, tie-breaker for simultaneous actors
;;;   speed      — Nat, how fast energy accumulates

(define-record-type actor-entry%
  (fields entity-id
          (mutable energy)
          initiative
          speed))

;;; make-actor-entry : Nat × Nat × Nat × Nat -> ActorEntry
(define make-actor-entry
  (case-lambda
   [(entity-id)
    (make-actor-entry% entity-id 0 0 100)]
   [(entity-id energy)
    (make-actor-entry% entity-id energy 0 100)]
   [(entity-id energy initiative)
    (make-actor-entry% entity-id energy initiative 100)]
   [(entity-id energy initiative speed)
    (make-actor-entry% entity-id energy initiative speed)]))

;;; Accessors
(define actor-entry-entity-id actor-entry%-entity-id)
(define actor-entry-energy actor-entry%-energy)
(define actor-entry-initiative actor-entry%-initiative)
(define actor-entry-speed actor-entry%-speed)

;;; actor-entry-can-act? : ActorEntry × [Nat] -> Bool
;;; Check if actor has enough energy to act.
(define actor-entry-can-act?
  (case-lambda
   [(entry)
    (actor-entry-can-act? entry *default-energy-threshold*)]
   [(entry threshold)
    (>= (actor-entry-energy entry) threshold)]))

;;; actor-entry-add-energy! : ActorEntry × Nat -> Void
;;; Add energy to an actor entry.
(define (actor-entry-add-energy! entry amount)
  (actor-entry%-energy-set! entry
                            (+ (actor-entry-energy entry) amount)))

;;; actor-entry-spend-energy! : ActorEntry × Nat -> Void
;;; Deduct energy from an actor entry.
(define (actor-entry-spend-energy! entry amount)
  (actor-entry%-energy-set! entry
                            (max 0 (- (actor-entry-energy entry) amount))))

;;; actor-entry-reset-energy! : ActorEntry -> Void
;;; Reset energy to 0.
(define (actor-entry-reset-energy! entry)
  (actor-entry%-energy-set! entry 0))

;;; ============================================================
;;; Actor Queue
;;; ============================================================

;;; ActorQueue : Record
;;;   entries   — List ActorEntry, all actors in turn order
;;;   threshold — Nat, energy required to act

(define-record-type actor-queue%
  (fields (mutable entries)
          threshold))

;;; make-actor-queue : [Nat] -> ActorQueue
;;; Create an empty actor queue.
(define make-actor-queue
  (case-lambda
   [() (make-actor-queue% '() *default-energy-threshold*)]
   [(threshold) (make-actor-queue% '() threshold)]))

;;; Accessors
(define actor-queue-entries actor-queue%-entries)
(define actor-queue-threshold actor-queue%-threshold)

;;; make-turn-order : List Entity × [Nat] -> ActorQueue
;;; Create a turn order queue from a list of entities.
;;; Only includes entities with an 'actor component.
(define make-turn-order
  (case-lambda
   [(entities)
    (make-turn-order entities *default-energy-threshold*)]
   [(entities threshold)
    (let* ([actors (filter entity-is-actor? entities)]
           [entries (map (lambda (entity)
                                 (let* ([actor-comp (entity-actor entity)]
                                        [stats-comp (entity-stats entity)]
                                        [energy (if actor-comp
                                                    (actor-energy actor-comp)
                                                    0)]
                                        [initiative (if actor-comp
                                                        (actor-initiative actor-comp)
                                                        0)]
                                        [speed (if stats-comp
                                                   (stats-speed stats-comp)
                                                   100)])
                                       (make-actor-entry (entity-id entity)
                                                         energy
                                                         initiative
                                                         speed)))
                         actors)])
          (make-actor-queue% entries threshold))]))

;;; actor-queue-add! : ActorQueue × ActorEntry -> Void
;;; Add an actor to the queue.
(define (actor-queue-add! queue entry)
  (actor-queue%-entries-set! queue
                             (cons entry (actor-queue-entries queue))))

;;; actor-queue-add-entity! : ActorQueue × Entity -> Void
;;; Add an entity to the queue (extracts actor info).
(define (actor-queue-add-entity! queue entity)
  (if (entity-is-actor? entity)
      (let* ([actor-comp (entity-actor entity)]
             [stats-comp (entity-stats entity)]
             [energy (if actor-comp (actor-energy actor-comp) 0)]
             [initiative (if actor-comp (actor-initiative actor-comp) 0)]
             [speed (if stats-comp (stats-speed stats-comp) 100)])
            (actor-queue-add! queue
                              (make-actor-entry (entity-id entity) energy initiative speed)))
      #f))

;;; actor-queue-remove! : ActorQueue × Nat -> Bool
;;; Remove an actor by entity ID. Returns #t if found and removed.
(define (actor-queue-remove! queue entity-id)
  (let ([old-entries (actor-queue-entries queue)]
        [new-entries (filter (lambda (entry)
                                     (not (= (actor-entry-entity-id entry) entity-id)))
                             (actor-queue-entries queue))])
       (actor-queue%-entries-set! queue new-entries)
       (not (= (length old-entries) (length new-entries)))))

;;; actor-queue-find : ActorQueue × Nat -> ActorEntry | #f
;;; Find an actor entry by entity ID.
(define (actor-queue-find queue entity-id)
  (let loop ([entries (actor-queue-entries queue)])
       (cond
        [(null? entries) #f]
        [(= (actor-entry-entity-id (car entries)) entity-id)
         (car entries)]
        [else (loop (cdr entries))])))

;;; actor-queue-empty? : ActorQueue -> Bool
(define (actor-queue-empty? queue)
  (null? (actor-queue-entries queue)))

;;; actor-queue-size : ActorQueue -> Nat
(define (actor-queue-size queue)
  (length (actor-queue-entries queue)))

;;; actor-queue-clear! : ActorQueue -> Void
(define (actor-queue-clear! queue)
  (actor-queue%-entries-set! queue '()))

;;; get-next-actor : ActorQueue -> ActorEntry | #f
;;; Get the next actor that can act (has enough energy).
;;; Ties broken by initiative (higher goes first).
;;; Returns #f if no actor can act.
(define (get-next-actor queue)
  (let* ([threshold (actor-queue-threshold queue)]
         [ready (filter (lambda (entry)
                                (actor-entry-can-act? entry threshold))
                        (actor-queue-entries queue))])
        (if (null? ready)
            #f
            ;; Sort by energy (descending), then initiative (descending)
            (car (list-sort (lambda (a b)
                                    (let ([ea (actor-entry-energy a)]
                                          [eb (actor-entry-energy b)]
                                          [ia (actor-entry-initiative a)]
                                          [ib (actor-entry-initiative b)])
                                         (or (> ea eb)
                                             (and (= ea eb) (> ia ib)))))
                            ready)))))

;;; advance-actor-energy! : ActorQueue × [Nat] -> Void
;;; Add energy to all actors based on their speed.
;;; amount is the base energy gain (modified by speed percentage).
(define advance-actor-energy!
  (case-lambda
   [(queue)
    (advance-actor-energy! queue *default-energy-gain*)]
   [(queue base-gain)
    (for-each (lambda (entry)
                      (let* ([speed (actor-entry-speed entry)]
                             ;; Speed is percentage (100 = normal, 200 = double speed)
                             [gain (quotient (* base-gain speed) 100)])
                            (actor-entry-add-energy! entry gain)))
              (actor-queue-entries queue))]))

;;; tick-energy! : ActorQueue × [Nat] -> ActorEntry | #f
;;; Advance energy for all actors until someone can act.
;;; Returns the next actor entry, or #f if queue is empty.
;;; max-iterations prevents infinite loops (default 100).
(define tick-energy!
  (case-lambda
   [(queue)
    (tick-energy! queue 100)]
   [(queue max-iterations)
    (let loop ([iterations 0])
         (cond
          [(actor-queue-empty? queue) #f]
          [(>= iterations max-iterations) #f]
          [else
           (let ([next (get-next-actor queue)])
                (if next
                    next
                    (begin
                     (advance-actor-energy! queue)
                     (loop (+ iterations 1)))))]))]))

;;; ============================================================
;;; Turn State
;;; ============================================================

;;; TurnState : Record
;;;   current-turn     — Nat, global turn counter
;;;   phase            — Symbol, current phase
;;;   active-entity-id — Nat | #f, entity currently taking turn
;;;   pending-actions  — List Action, actions queued for processing
;;;   turn-log         — List Event, events generated this turn
;;;   actor-queue      — ActorQueue, turn order queue

(define-record-type turn-state%
  (fields (mutable current-turn)
          (mutable phase)
          (mutable active-entity-id)
          (mutable pending-actions)
          (mutable turn-log)
          actor-queue))

;;; make-turn-state : [ActorQueue] -> TurnState
;;; Create a new turn state.
(define make-turn-state
  (case-lambda
   [() (make-turn-state% 0 'start-turn #f '() '() (make-actor-queue))]
   [(actor-queue)
    (make-turn-state% 0 'start-turn #f '() '() actor-queue)]))

;;; Accessors
(define turn-state-current-turn turn-state%-current-turn)
(define turn-state-phase turn-state%-phase)
(define turn-state-active-entity-id turn-state%-active-entity-id)
(define turn-state-pending-actions turn-state%-pending-actions)
(define turn-state-turn-log turn-state%-turn-log)
(define turn-state-actor-queue turn-state%-actor-queue)

;;; turn-state-increment-turn! : TurnState -> Void
(define (turn-state-increment-turn! state)
  (turn-state%-current-turn-set! state
                                 (+ (turn-state-current-turn state) 1)))

;;; turn-state-set-phase! : TurnState × Symbol -> Void
(define (turn-state-set-phase! state phase)
  (if (turn-phase? phase)
      (turn-state%-phase-set! state phase)
      (error 'turn-state-set-phase!
             "Invalid turn phase"
             phase)))

;;; turn-state-set-active-entity! : TurnState × (Nat | #f) -> Void
(define (turn-state-set-active-entity! state entity-id)
  (turn-state%-active-entity-id-set! state entity-id))

;;; turn-state-add-action! : TurnState × Action -> Void
(define (turn-state-add-action! state action)
  (turn-state%-pending-actions-set! state
                                    (append (turn-state-pending-actions state) (list action))))

;;; turn-state-pop-action! : TurnState -> Action | #f
(define (turn-state-pop-action! state)
  (let ([actions (turn-state-pending-actions state)])
       (if (null? actions)
           #f
           (let ([action (car actions)])
                (turn-state%-pending-actions-set! state (cdr actions))
                action))))

;;; turn-state-clear-actions! : TurnState -> Void
(define (turn-state-clear-actions! state)
  (turn-state%-pending-actions-set! state '()))

;;; turn-state-add-event! : TurnState × Event -> Void
(define (turn-state-add-event! state event)
  (let ([timestamped (event-with-timestamp event
                                           (turn-state-current-turn state))])
       (turn-state%-turn-log-set! state
                                  (cons timestamped (turn-state-turn-log state)))))

;;; turn-state-get-events : TurnState -> List Event
;;; Get events in chronological order (reverse of log).
(define (turn-state-get-events state)
  (reverse (turn-state-turn-log state)))

;;; turn-state-clear-events! : TurnState -> Void
(define (turn-state-clear-events! state)
  (turn-state%-turn-log-set! state '()))

;;; turn-state-has-pending-actions? : TurnState -> Bool
(define (turn-state-has-pending-actions? state)
  (not (null? (turn-state-pending-actions state))))

;;; ============================================================
;;; Turn Phase Management
;;; ============================================================

;;; transition-phase! : TurnState × Symbol × [EventQueue] -> Void
;;; Transition to a new phase and optionally generate events.
(define transition-phase!
  (case-lambda
   [(state new-phase)
    (transition-phase! state new-phase #f)]
   [(state new-phase event-queue)
    (let ([old-phase (turn-state-phase state)])
         (turn-state-set-phase! state new-phase)
         ;; Generate phase transition event if queue provided
         (when event-queue
               (let ([evt (make-event 'phase-changed
                                      #f
                                      #f
                                      `((old-phase . ,old-phase)
                                        (new-phase . ,new-phase)
                                        (turn . ,(turn-state-current-turn state)))
                                      (turn-state-current-turn state))])
                    (event-queue-push! event-queue evt))))]))

;;; next-phase : Symbol -> Symbol
;;; Get the next phase in the standard turn sequence.
(define (next-phase current-phase)
  (case current-phase
        [(start-turn) 'get-input]
        [(get-input) 'process-action]
        [(process-action) 'resolve-effects]
        [(resolve-effects) 'end-turn]
        [(end-turn) 'start-turn]
        [(ai) 'process-action]
        [(animate) 'end-turn]
        [else 'start-turn]))

;;; advance-phase! : TurnState × [EventQueue] -> Void
;;; Advance to the next phase in the standard sequence.
(define advance-phase!
  (case-lambda
   [(state)
    (advance-phase! state #f)]
   [(state event-queue)
    (transition-phase! state
                       (next-phase (turn-state-phase state))
                       event-queue)]))

;;; ============================================================
;;; Turn Processing
;;; ============================================================

;;; begin-turn! : TurnState × [EventQueue] -> Event
;;; Start a new turn. Increments turn counter and transitions to start-turn phase.
;;; Returns the turn-started event.
(define begin-turn!
  (case-lambda
   [(state)
    (begin-turn! state #f)]
   [(state event-queue)
    (turn-state-increment-turn! state)
    (transition-phase! state 'start-turn event-queue)
    (turn-state-clear-events! state)
    (let ([evt (event-turn-started (turn-state-current-turn state))])
         (turn-state-add-event! state evt)
         (when event-queue
               (event-queue-push! event-queue evt))
         evt)]))

;;; end-turn! : TurnState × [EventQueue] -> Event
;;; End the current turn. Transitions to end-turn phase.
;;; Returns the turn-ended event.
(define end-turn!
  (case-lambda
   [(state)
    (end-turn! state #f)]
   [(state event-queue)
    (transition-phase! state 'end-turn event-queue)
    (turn-state-set-active-entity! state #f)
    (let ([evt (event-turn-ended (turn-state-current-turn state))])
         (turn-state-add-event! state evt)
         (when event-queue
               (event-queue-push! event-queue evt))
         evt)]))

;;; select-next-actor! : TurnState -> ActorEntry | #f
;;; Find the next actor that can act and set them as active.
;;; Returns the actor entry, or #f if no one can act.
(define (select-next-actor! state)
  (let* ([queue (turn-state-actor-queue state)]
         [next (get-next-actor queue)])
        (if next
            (begin
             (turn-state-set-active-entity! state
                                            (actor-entry-entity-id next))
             next)
            (begin
             (turn-state-set-active-entity! state #f)
             #f))))

;;; spend-actor-energy! : TurnState × Nat × Nat -> Void
;;; Deduct energy from an actor after they perform an action.
(define (spend-actor-energy! state entity-id cost)
  (let* ([queue (turn-state-actor-queue state)]
         [entry (actor-queue-find queue entity-id)])
        (when entry
              (actor-entry-spend-energy! entry cost))))

;;; process-turn! : TurnState × (TurnState Action -> ActionResult) × [EventQueue] -> Bool
;;; Process a complete turn for the next available actor.
;;; action-handler is a function that executes actions and returns results.
;;; Returns #t if a turn was processed, #f if no actors can act.
;;;
;;; Flow:
;;;   1. Tick energy until an actor is ready
;;;   2. Start turn (begin-turn!)
;;;   3. Select actor
;;;   4. Get/generate action (caller's responsibility to add action to state)
;;;   5. Process action
;;;   6. Spend energy
;;;   7. End turn (end-turn!)
(define process-turn!
  (case-lambda
   [(state action-handler)
    (process-turn! state action-handler #f)]
   [(state action-handler event-queue)
    (let* ([queue (turn-state-actor-queue state)]
           [next-actor (tick-energy! queue)])
          (if (not next-actor)
              #f ; No actors available
              (begin
               ;; Begin turn
               (begin-turn! state event-queue)
               
               ;; Select the actor
               (let ([actor-id (actor-entry-entity-id next-actor)])
                    (turn-state-set-active-entity! state actor-id)
                    
                    ;; Phase: get-input / ai
                    ;; (Caller should add action to state before calling process-turn!
                    ;;  or handle action generation here)
                    
                    ;; Phase: process-action
                    (transition-phase! state 'process-action event-queue)
                    
                    ;; Process pending actions
                    (let loop ()
                         (let ([action (turn-state-pop-action! state)])
                              (when action
                                    ;; Execute action
                                    (let ([result (action-handler state action)])
                                         ;; Add events from result to turn log
                                         (for-each (lambda (evt)
                                                           (turn-state-add-event! state evt)
                                                           (when event-queue
                                                                 (event-queue-push! event-queue evt)))
                                                   (action-result-events result))
                                         
                                         ;; Spend energy
                                         (spend-actor-energy! state actor-id (action-cost action))
                                         
                                         ;; Continue processing if more actions
                                         (loop)))))
                    
                    ;; Phase: resolve-effects
                    (transition-phase! state 'resolve-effects event-queue)
                    
                    ;; Phase: end-turn
                    (end-turn! state event-queue)
                    
                    #t))))]))

;;; process-action-simple! : TurnState × Action × (TurnState Action -> ActionResult) × [EventQueue] -> ActionResult
;;; Process a single action for the active entity.
;;; Simpler alternative to process-turn! when you just want to process one action.
(define process-action-simple!
  (case-lambda
   [(state action action-handler)
    (process-action-simple! state action action-handler #f)]
   [(state action action-handler event-queue)
    (transition-phase! state 'process-action event-queue)
    (let ([result (action-handler state action)])
         ;; Add events to turn log
         (for-each (lambda (evt)
                           (turn-state-add-event! state evt)
                           (when event-queue
                                 (event-queue-push! event-queue evt)))
                   (action-result-events result))
         
         ;; Spend energy from active actor
         (let ([actor-id (turn-state-active-entity-id state)])
              (when actor-id
                    (spend-actor-energy! state actor-id (action-cost action))))
         
         result)]))

;;; ============================================================
;;; Continuous Turn Mode
;;; ============================================================

;;; run-until-player-turn! : TurnState × Nat × (Nat -> Action) × (TurnState Action -> ActionResult) × [EventQueue] -> Nat
;;; Run turns continuously until a specific entity's (player) turn.
;;; Returns the number of turns processed.
;;;
;;; player-id: entity ID of the player
;;; ai-action-fn: function that generates actions for NPCs (entity-id -> Action)
;;; action-handler: function that processes actions
(define run-until-player-turn!
  (case-lambda
   [(state player-id ai-action-fn action-handler)
    (run-until-player-turn! state player-id ai-action-fn action-handler #f)]
   [(state player-id ai-action-fn action-handler event-queue)
    (let loop ([count 0])
         (let* ([queue (turn-state-actor-queue state)]
                [next-actor (tick-energy! queue)])
               (if (not next-actor)
                   count ; No more actors
                   (let ([actor-id (actor-entry-entity-id next-actor)])
                        (if (= actor-id player-id)
                            count ; Player's turn, stop
                            (begin
                             ;; Process NPC turn
                             (begin-turn! state event-queue)
                             (turn-state-set-active-entity! state actor-id)
                             
                             ;; Generate AI action
                             (let ([action (ai-action-fn actor-id)])
                                  (turn-state-add-action! state action)
                                  
                                  ;; Process the turn
                                  (process-turn! state action-handler event-queue)
                                  
                                  (loop (+ count 1)))))))))])) ;; Andy was here

;;; ============================================================
;;; Turn State Queries
;;; ============================================================

;;; turn-state-get-actor-energy : TurnState × Nat -> Nat | #f
;;; Get the current energy of an actor.
(define (turn-state-get-actor-energy state entity-id)
  (let* ([queue (turn-state-actor-queue state)]
         [entry (actor-queue-find queue entity-id)])
        (if entry
            (actor-entry-energy entry)
            #f)))

;;; turn-state-get-actors-ready : TurnState -> List Nat
;;; Get list of entity IDs for all actors that can currently act.
(define (turn-state-get-actors-ready state)
  (let* ([queue (turn-state-actor-queue state)]
         [threshold (actor-queue-threshold queue)]
         [ready (filter (lambda (entry)
                                (actor-entry-can-act? entry threshold))
                        (actor-queue-entries queue))])
        (map actor-entry-entity-id ready)))

;;; turn-state-is-player-turn? : TurnState × Nat -> Bool
;;; Check if it's a specific entity's turn.
(define (turn-state-is-player-turn? state player-id)
  (let ([active (turn-state-active-entity-id state)])
       (and active (= active player-id))))

;;; turn-state-get-next-actor-id : TurnState -> Nat | #f
;;; Peek at which actor will go next (without modifying state).
(define (turn-state-get-next-actor-id state)
  (let* ([queue (turn-state-actor-queue state)]
         [next (get-next-actor queue)])
        (if next
            (actor-entry-entity-id next)
            #f)))

;;; turn-state-in-phase? : TurnState × Symbol -> Bool
;;; Check if the turn state is in a specific phase.
(define (turn-state-in-phase? state phase)
  (eq? (turn-state-phase state) phase))

;;; turn-state-waiting-for-input? : TurnState -> Bool
;;; Check if the turn state is waiting for player input.
(define (turn-state-waiting-for-input? state)
  (turn-state-in-phase? state 'get-input))

;;; ============================================================
;;; Speed and Energy Utilities
;;; ============================================================

;;; calculate-energy-gain : Nat × Nat -> Nat
;;; Calculate energy gain based on speed.
;;; base-speed is the baseline (100 = normal)
;;; speed is the entity's speed stat
(define (calculate-energy-gain base-gain speed)
  (quotient (* base-gain speed) 100))

;;; action-cost-from-speed : Nat × Nat -> Nat
;;; Calculate action cost based on speed.
;;; Faster entities have lower costs (can act more often).
;;; base-cost is the normal cost (100)
;;; speed is multiplier (100 = normal, 200 = half cost)
(define (action-cost-from-speed base-cost speed)
  (if (= speed 0)
      base-cost
      (max 1 (quotient (* base-cost 100) speed))))

;;; ticks-until-ready : Nat × Nat × Nat -> Nat
;;; Calculate how many ticks until an actor can act.
;;; current-energy: actor's current energy
;;; threshold: energy needed to act
;;; energy-per-tick: energy gained per tick
(define (ticks-until-ready current-energy threshold energy-per-tick)
  (if (>= current-energy threshold)
      0
      (if (<= energy-per-tick 0)
          999999 ; Never ready
          (quotient (+ (- threshold current-energy) energy-per-tick -1)
                    energy-per-tick))))

;;; ============================================================
;;; Turn State Formatting
;;; ============================================================

;;; turn-state->string : TurnState -> String
;;; Convert turn state to human-readable string for debugging.
(define (turn-state->string state)
  (string-append
   "Turn " (number->string (turn-state-current-turn state))
   " | Phase: " (symbol->string (turn-state-phase state))
   " | Active: "
   (if (turn-state-active-entity-id state)
       (number->string (turn-state-active-entity-id state))
       "none")
   " | Pending: " (number->string (length (turn-state-pending-actions state)))
   " actions"
   " | Events: " (number->string (length (turn-state-turn-log state)))))

;;; actor-queue->string : ActorQueue -> String
;;; Convert actor queue to human-readable string for debugging.
(define (actor-queue->string queue)
  (let ([entries (actor-queue-entries queue)])
       (if (null? entries)
           "Empty queue"
           (string-append
            (number->string (length entries)) " actors: "
            (fold-left (lambda (acc entry)
                               (string-append
                                acc
                                (if (string=? acc "") "" ", ")
                                "#" (number->string (actor-entry-entity-id entry))
                                "(" (number->string (actor-entry-energy entry)) "E)"))
                       ""
                       entries)))))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Configuration:
;;;   *default-energy-threshold*, *default-energy-gain*
;;;   turn-phases, turn-phase?
;;;
;;; Actor Entry:
;;;   make-actor-entry, actor-entry-entity-id, actor-entry-energy
;;;   actor-entry-initiative, actor-entry-speed, actor-entry-can-act?
;;;   actor-entry-add-energy!, actor-entry-spend-energy!, actor-entry-reset-energy!
;;;
;;; Actor Queue:
;;;   make-actor-queue, make-turn-order
;;;   actor-queue-entries, actor-queue-threshold
;;;   actor-queue-add!, actor-queue-add-entity!, actor-queue-remove!
;;;   actor-queue-find, actor-queue-empty?, actor-queue-size, actor-queue-clear!
;;;   get-next-actor, advance-actor-energy!, tick-energy!
;;;
;;; Turn State:
;;;   make-turn-state
;;;   turn-state-current-turn, turn-state-phase, turn-state-active-entity-id
;;;   turn-state-pending-actions, turn-state-turn-log, turn-state-actor-queue
;;;   turn-state-increment-turn!, turn-state-set-phase!, turn-state-set-active-entity!
;;;   turn-state-add-action!, turn-state-pop-action!, turn-state-clear-actions!
;;;   turn-state-add-event!, turn-state-get-events, turn-state-clear-events!
;;;   turn-state-has-pending-actions?
;;;
;;; Phase Management:
;;;   transition-phase!, next-phase, advance-phase!
;;;
;;; Turn Processing:
;;;   begin-turn!, end-turn!, select-next-actor!, spend-actor-energy!
;;;   process-turn!, process-action-simple!, run-until-player-turn!
;;;
;;; Turn State Queries:
;;;   turn-state-get-actor-energy, turn-state-get-actors-ready
;;;   turn-state-is-player-turn?, turn-state-get-next-actor-id
;;;   turn-state-in-phase?, turn-state-waiting-for-input?
;;;
;;; Energy Utilities:
;;;   calculate-energy-gain, action-cost-from-speed, ticks-until-ready
;;;
;;; Formatting:
;;;   turn-state->string, actor-queue->string
