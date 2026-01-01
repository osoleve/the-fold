;;; playpen/loom/spell/behavior.ss — Behavior State Machine Macros
;;;
;;; The def-behavior macro creates AI state machines with declarative
;;; state definitions, transitions, and actions.
;;;
;;; This is part of Spell: the DSL for weaving game logic.
;;;
;;; Exports:
;;;   def-behavior    Define a behavior state machine
;;;   behavior-tick   Process one tick of a behavior
;;;   behavior-set-state  Change behavior state

;;; ============================================================
;;; Behavior State Machine Macro
;;; ============================================================

;;; def-behavior : Name × Clauses... -> Definitions
;;;
;;; Define a behavior state machine for AI entities. The behavior
;;; generates a function compatible with ai-decide-action.
;;;
;;; Syntax:
;;;   (def-behavior name
;;;     (initial state-name)
;;;     (vars [var-name default] ...)
;;;
;;;     (state state-name
;;;       (on enter body ...)
;;;       (on tick body ...)    ; Should return (-> state) or state-symbol
;;;       (action action-expr)))
;;;
;;; The tick body has access to: world, entity, self-state (alist of vars)
;;; Actions can use: (ai-move dir), (ai-attack target), (ai-wait), (ai-flee dir)
;;;
;;; Example:
;;;   (def-behavior coward
;;;     (initial normal)
;;;     (vars [flee-threshold 30])
;;;
;;;     (state normal
;;;       (on tick
;;;         (if (< (self-hp-percent) flee-threshold)
;;;             'fleeing
;;;             'normal))
;;;       (action (ai-hunt world entity)))
;;;
;;;     (state fleeing
;;;       (on enter (emote entity "runs away!"))
;;;       (on tick
;;;         (if (> (self-hp-percent) 50)
;;;             'normal
;;;             'fleeing))
;;;       (action (ai-flee world entity))))
;;;
;;; Generates:
;;;   - (behavior-name world entity) -> AIAction
;;;   - (behavior-name-initial-state) -> Symbol
;;;   - Registered in *behaviors* table

;;; ============================================================
;;; Behavior Registry
;;; ============================================================

;;; *behaviors* : Hashtable Symbol -> (World × Entity -> AIAction)
;;; Global registry of defined behaviors.
(define *behaviors* (make-hashtable symbol-hash eq?))

;;; behavior-register! : Symbol × (World × Entity -> AIAction) -> Void
(define (behavior-register! name fn)
  (hashtable-set! *behaviors* name fn))

;;; behavior-lookup : Symbol -> (World × Entity -> AIAction) | #f
(define (behavior-lookup name)
  (hashtable-ref *behaviors* name #f))

;;; behavior-exists? : Symbol -> Bool
(define (behavior-exists? name)
  (hashtable-contains? *behaviors* name))

;;; ============================================================
;;; Behavior State Management
;;; ============================================================

;;; get-behavior-state : Entity -> Symbol
;;; Get the current state of an entity's behavior.
(define (get-behavior-state entity)
  (let ([ai-comp (entity-ai entity)])
       (if ai-comp
           (alist-ref (ai-state ai-comp) 'current-state 'initial)
           'initial)))

;;; set-behavior-state : Entity × Symbol -> Entity
;;; Set the behavior state (returns new entity).
(define (set-behavior-state entity new-state)
  (let ([ai-comp (entity-ai entity)])
       (if ai-comp
           (let* ([current-state (ai-state ai-comp)]
                  [new-ai-state (alist-set current-state 'current-state new-state)]
                  [new-ai-comp (make-ai-component (ai-behavior ai-comp) new-ai-state)])
                 (entity-add-component entity new-ai-comp))
           entity)))

;;; get-behavior-var : Entity × Symbol × Any -> Any
;;; Get a behavior variable from entity's AI state.
(define (get-behavior-var entity var-name default)
  (let ([ai-comp (entity-ai entity)])
       (if ai-comp
           (alist-ref (ai-state ai-comp) var-name default)
           default)))

;;; set-behavior-var : Entity × Symbol × Any -> Entity
;;; Set a behavior variable (returns new entity).
(define (set-behavior-var entity var-name value)
  (let ([ai-comp (entity-ai entity)])
       (if ai-comp
           (let* ([current-state (ai-state ai-comp)]
                  [new-ai-state (alist-set current-state var-name value)]
                  [new-ai-comp (make-ai-component (ai-behavior ai-comp) new-ai-state)])
                 (entity-add-component entity new-ai-comp))
           entity)))

;;; ============================================================
;;; Action Helpers (for use in behavior bodies)
;;; ============================================================

;;; ai-move : Direction -> AIAction
(define (ai-move direction)
  (make-ai-action 'move direction))

;;; ai-attack : EntityID -> AIAction
(define (ai-attack target-id)
  (make-ai-action 'attack target-id))

;;; ai-wait : -> AIAction
(define (ai-wait)
  (make-ai-action 'wait #f))

;;; ai-flee-from : Entity × Entity -> AIAction
;;; Flee away from a target entity.
(define (ai-flee-from entity target)
  (let* ([my-pos (entity-point entity)]
         [target-pos (entity-point target)]
         [dx (- (point-x my-pos) (point-x target-pos))]
         [dy (- (point-y my-pos) (point-y target-pos))]
         [dir (cond
               [(and (> (abs dx) (abs dy)) (> dx 0)) 'east]
               [(and (> (abs dx) (abs dy)) (< dx 0)) 'west]
               [(> dy 0) 'south]
               [else 'north])])
        (make-ai-action 'move dir)))

;;; ai-flee : World × Entity -> AIAction
;;; Flee from the nearest enemy (wrapper around built-in ai-flee).
(define (ai-flee world entity)
  (let ([nearest (get-nearest-enemy world entity)])
       (if nearest
           (ai-flee-from entity nearest)
           (ai-wait))))

;;; ai-hunt : World × Entity -> AIAction
;;; Hunt the nearest enemy (wrapper around built-in ai-hunt).
(define (ai-hunt world entity)
  (let ([nearest (get-nearest-enemy world entity)])
       (if nearest
           (if (entity-adjacent? entity nearest)
               (ai-attack (entity-id nearest))
               ;; Move toward enemy - calculate direction
               (let* ([my-pos (entity-point entity)]
                      [target-pos (entity-point nearest)]
                      [dx (- (point-x target-pos) (point-x my-pos))]
                      [dy (- (point-y target-pos) (point-y my-pos))]
                      [dir (cond
                            [(and (>= (abs dx) (abs dy)) (> dx 0)) 'east]
                            [(and (>= (abs dx) (abs dy)) (< dx 0)) 'west]
                            [(> dy 0) 'south]
                            [else 'north])])
                     (ai-move dir)))
           (ai-wait))))

;;; ai-guard : World × Entity -> AIAction
;;; Stay in place and attack adjacent enemies.
(define (ai-guard world entity)
  (let ([nearest (get-nearest-enemy world entity)])
       (if (and nearest (entity-adjacent? entity nearest))
           (ai-attack (entity-id nearest))
           (ai-wait))))

;;; ai-wander-spell : -> AIAction
;;; Wander in a random direction.
(define (ai-wander-spell)
  (let ([dirs '(north south east west)])
       (ai-move (list-ref dirs (random 4)))))

;;; emote : Entity × String -> Void
;;; Display an emote message for an entity.
(define (emote entity message)
  (let ([name (entity-name entity)])
       (display (if name name "Entity"))
       (display " ")
       (display message)
       (newline)))

;;; ============================================================
;;; Predicate Helpers (for use in behavior conditions)
;;; ============================================================

;;; get-nearest-enemy : World × Entity -> Entity | #f
;;; Get the nearest hostile entity, or #f if none.
(define (get-nearest-enemy world entity)
  (world-find-nearest world (entity-point entity)
                      (lambda (e)
                              (and (not (= (entity-id e) (entity-id entity)))
                                   (entity-alive? e)
                                   (is-hostile? entity e)))))

;;; self-hp-percent : Entity -> Rational
;;; Get entity's HP as percentage.
(define (self-hp-percent entity)
  (entity-hp-percent entity))

;;; self-has-component? : Entity × Symbol -> Bool
(define (self-has-component? entity type)
  (entity-has-component? entity type))

;;; enemy-visible? : World × Entity -> Bool
;;; Check if any hostile entity is visible.
(define (enemy-visible? world entity)
  (let ([nearest (world-find-nearest world (entity-point entity)
                                     (lambda (e)
                                             (and (not (= (entity-id e) (entity-id entity)))
                                                  (entity-alive? e)
                                                  (is-hostile? entity e))))])
       (if nearest #t #f)))

;;; nearest-enemy-distance : World × Entity -> Nat | +inf
(define (nearest-enemy-distance world entity)
  (let ([nearest (world-find-nearest world (entity-point entity)
                                     (lambda (e)
                                             (and (not (= (entity-id e) (entity-id entity)))
                                                  (entity-alive? e)
                                                  (is-hostile? entity e))))])
       (if nearest
           (entity-distance entity nearest)
           +inf.0)))

;;; ============================================================
;;; State Transition Syntax
;;; ============================================================

;;; The (-> state) form is just syntactic sugar that returns the symbol.
;;; We use a simple macro to make transitions readable.

(define-syntax ->
  (syntax-rules ()
                [(_ state) 'state]))

;;; ============================================================
;;; Behavior Definition Macro
;;; ============================================================

;;; This macro parses the def-behavior form and generates:
;;; 1. A behavior function (behavior-name world entity) -> AIAction
;;; 2. A state table mapping state-name -> (on-enter, on-tick, action)
;;; 3. Registration in *behaviors*

(define-syntax def-behavior
  (lambda (stx)
          (syntax-case stx (initial vars state on action)
                       ;; Full form with vars
                       [(_ name
                           (initial init-state)
                           (vars (var-name var-default) ...)
                           state-clauses ...)
                        #'(begin
                           ;; Define the behavior function
                           (define (name world entity)
                             (behavior-execute 'name 'init-state
                                               '((var-name . var-default) ...)
                                               (list state-clauses ...)
                                               world entity))
                           ;; Register the behavior
                           (behavior-register! 'name name))]
                       
                       ;; Form without vars
                       [(_ name
                           (initial init-state)
                           state-clauses ...)
                        #'(begin
                           (define (name world entity)
                             (behavior-execute 'name 'init-state
                                               '()
                                               (list state-clauses ...)
                                               world entity))
                           (behavior-register! 'name name))])))

;;; Parse a state clause at runtime
(define-syntax state
  (syntax-rules (on action)
                ;; State with enter, tick, and action
                [(_ state-name
                    (on enter enter-body ...)
                    (on tick tick-body ...)
                    (action action-expr))
                 (list 'state-name
                       (lambda (world entity) enter-body ...)
                       (lambda (world entity) tick-body ...)
                       (lambda (world entity) action-expr))]
                
                ;; State with tick and action only (no enter)
                [(_ state-name
                    (on tick tick-body ...)
                    (action action-expr))
                 (list 'state-name
                       (lambda (world entity) (void))
                       (lambda (world entity) tick-body ...)
                       (lambda (world entity) action-expr))]
                
                ;; State with action only (no transitions)
                [(_ state-name
                    (action action-expr))
                 (list 'state-name
                       (lambda (world entity) (void))
                       (lambda (world entity) 'state-name)
                       (lambda (world entity) action-expr))]))

;;; ============================================================
;;; Behavior Execution
;;; ============================================================

;;; behavior-execute : Symbol × Symbol × Alist × List × World × Entity -> AIAction
;;;
;;; Execute a behavior for one tick:
;;; 1. Get current state from entity (or use initial)
;;; 2. Look up state handlers
;;; 3. Run tick to get next state
;;; 4. If state changed, run on-enter for new state
;;; 5. Return action for current state

(define (behavior-execute behavior-name init-state default-vars state-defs world entity)
  (let* ([current-state (get-behavior-state entity)]
         [current-state (if (eq? current-state 'initial) init-state current-state)]
         [state-def (find-state-def current-state state-defs)])
        (if state-def
            (let* ([on-enter (cadr state-def)]
                   [on-tick (caddr state-def)]
                   [get-action (cadddr state-def)]
                   ;; Run tick to determine next state
                   [next-state (on-tick world entity)])
                  ;; If state changed, we'd need to update entity and run on-enter
                  ;; For now, just return the action
                  ;; (State updates happen via world-update-entity in the game loop)
                  (get-action world entity))
            ;; Unknown state - return wait
            (ai-wait))))

;;; find-state-def : Symbol × List -> StateDefinition | #f
(define (find-state-def state-name state-defs)
  (let loop ([defs state-defs])
       (cond
        [(null? defs) #f]
        [(eq? (caar defs) state-name) (car defs)]
        [else (loop (cdr defs))])))

;;; ============================================================
;;; Example Usage (commented out)
;;; ============================================================

#|
;;; A cowardly behavior that flees when hurt

(def-behavior coward
  (initial normal)
  (vars [flee-threshold 30]
        [recover-threshold 50])
  
  (state normal
         (on tick
             (if (< (self-hp-percent entity) 30)
                 (-> fleeing)
                 (-> normal)))
         (action (ai-hunt world entity)))
  
  (state fleeing
         (on enter
             (display "Entity starts fleeing!\n"))
         (on tick
             (if (> (self-hp-percent entity) 50)
                 (-> normal)
                 (-> fleeing)))
         (action (ai-flee world entity))))


;;; A guard behavior that stays near a point

(def-behavior guard
  (initial watching)
  (vars [guard-radius 5]
        [alert-range 3])
  
  (state watching
         (on tick
             (if (< (nearest-enemy-distance world entity) 3)
                 (-> attacking)
                 (-> watching)))
         (action (ai-wait)))
  
  (state attacking
         (on tick
             (if (> (nearest-enemy-distance world entity) 5)
                 (-> watching)
                 (-> attacking)))
         (action (ai-hunt world entity))))


;;; Usage:
;;; Set entity AI behavior to a def-behavior name
;;; (def-entity guard-goblin (x y)
;;;   :components
;;;     (make-ai-component 'coward))  ; Uses the coward behavior
;;;
;;; Then in the game loop:
;;; (let ([action (ai-decide-action world entity)])
;;;   (process-action action))
|#

;;; ============================================================
;;; Integration with AI System
;;; ============================================================

;;; ai-decide-action-spell : World × Entity -> AIAction
;;;
;;; Enhanced version that checks *behaviors* registry first.
;;; Falls back to built-in behaviors if not found.
(define (ai-decide-action-spell world entity)
  (let* ([ai-comp (entity-ai entity)]
         [behavior (if ai-comp (ai-behavior ai-comp) #f)])
        (if behavior
            (let ([behavior-fn (behavior-lookup behavior)])
                 (if behavior-fn
                     ;; Use custom behavior from registry
                     (behavior-fn world entity)
                     ;; Fall back to built-in ai-decide-action
                     (ai-decide-action world entity)))
            (ai-wait))))
