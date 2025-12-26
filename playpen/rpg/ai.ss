;;; playpen/rpg/ai.ss — AI System for RPG SDK
;;;
;;; Provides reference implementations for common AI behaviors.
;;; Fills the gap identified in dogfooding: behaviors defined as symbols but no logic.
;;;
;;; This is Playpen code: the SDK for building roguelikes and dungeon crawlers.
;;;
;;; Dependencies:
;;;   - playpen/rpg/core.ss
;;;   - playpen/rpg/entity.ss
;;;   - playpen/rpg/world.ss
;;;   - playpen/rpg/combat.ss
;;;
;;; Exports:
;;;   AI behavior implementations, AI action selection

;;; ============================================================
;;; AI Action Types
;;; ============================================================

;;; AIAction : One of
;;;   ('move . Direction)     — Move in a direction
;;;   ('attack . EntityID)    — Attack an entity
;;;   ('wait . #f)            — Do nothing this turn
;;;   ('flee . Direction)     — Run away
;;;   ('pickup . ItemID)      — Pick up an item
;;;   ('use . ItemID)         — Use an item

;;; make-ai-action : Symbol × Any -> AIAction
(define (make-ai-action type data)
  (cons type data))

;;; ai-action-type : AIAction -> Symbol
(define (ai-action-type action)
  (car action))

;;; ai-action-data : AIAction -> Any
(define (ai-action-data action)
  (cdr action))

;;; ============================================================
;;; Core AI Behaviors
;;; ============================================================

;;; Each AI behavior function takes:
;;;   World × Entity -> AIAction
;;;
;;; The function examines the world and entity state, then
;;; returns the action the entity should take this turn.

;;; ============================================================
;;; Idle Behavior
;;; ============================================================

;;; ai-idle : World × Entity -> AIAction
;;; Do nothing - just stand still.
(define (ai-idle world entity)
  (make-ai-action 'wait #f))

;;; ============================================================
;;; Wander Behavior
;;; ============================================================

;;; ai-wander : World × Entity -> AIAction
;;; Move randomly, avoiding obstacles.
(define (ai-wander world entity)
  (let* ([directions '(north south east west)]
         [random-dir (list-ref directions (random (length directions)))]
         [delta (direction->delta random-dir)]
         [new-x (+ (entity-x entity) (car delta))]
         [new-y (+ (entity-y entity) (cdr delta))])
    (if (world-can-move-to? world entity new-x new-y)
        (make-ai-action 'move random-dir)
        (make-ai-action 'wait #f))))

;;; ai-wander-8 : World × Entity -> AIAction
;;; Wander in 8 directions (including diagonals).
(define (ai-wander-8 world entity)
  (let* ([directions '(north south east west ne nw se sw)]
         [random-dir (list-ref directions (random (length directions)))]
         [delta (direction->delta random-dir)]
         [new-x (+ (entity-x entity) (car delta))]
         [new-y (+ (entity-y entity) (cdr delta))])
    (if (world-can-move-to? world entity new-x new-y)
        (make-ai-action 'move random-dir)
        (make-ai-action 'wait #f))))

;;; ============================================================
;;; Guard Behavior
;;; ============================================================

;;; ai-guard : World × Entity → AIAction
;;; Stay near a guard point, attack nearby enemies.
;;;
;;; AI state should contain:
;;;   (guard-point . Point)   — Position to guard
;;;   (guard-radius . Nat)    — Max distance from guard point
;;;
(define (ai-guard world entity)
  (let* ([ai-comp (entity-ai entity)]
         [state (if ai-comp (ai-state ai-comp) '())]
         [guard-point (alist-ref state 'guard-point (entity-point entity))]
         [guard-radius (alist-ref state 'guard-radius 5)]
         [current-pos (entity-point entity)]
         ;; Check for nearby enemies
         [enemies (world-find-entities world
                    (lambda (e)
                      (and (not (= (entity-id e) (entity-id entity)))
                           (entity-alive? e)
                           (is-hostile? entity e))))]
         [nearby-enemies (filter
                           (lambda (e)
                             (< (entity-distance entity e) 2))
                           enemies)])
    (cond
      ;; Attack adjacent enemy
      [(not (null? nearby-enemies))
       (make-ai-action 'attack (entity-id (car nearby-enemies)))]
      ;; Too far from guard point - return
      [(> (manhattan-distance current-pos guard-point) guard-radius)
       (let ([path (world-find-path world current-pos guard-point #f)])
         (if (and path (not (null? path)))
             (let* ([next-pos (car path)]
                    [delta (point-sub next-pos current-pos)]
                    [dir (delta->direction delta)])
               (make-ai-action 'move dir))
             (make-ai-action 'wait #f)))]
      ;; At guard point - wait
      [else
       (make-ai-action 'wait #f)])))

;;; ============================================================
;;; Hunt Behavior
;;; ============================================================

;;; ai-hunt : World × Entity → AIAction
;;; Chase and attack the nearest hostile entity.
;;; Uses pathfinding if available, otherwise moves toward target.
;;;
(define (ai-hunt world entity)
  (let* ([current-pos (entity-point entity)]
         ;; Find all hostile entities
         [enemies (world-find-entities world
                    (lambda (e)
                      (and (not (= (entity-id e) (entity-id entity)))
                           (entity-alive? e)
                           (is-hostile? entity e))))]
         ;; Find nearest enemy
         [nearest (world-find-nearest world current-pos
                    (lambda (e)
                      (and (not (= (entity-id e) (entity-id entity)))
                           (entity-alive? e)
                           (is-hostile? entity e))))])
    (cond
      ;; No enemies - wander
      [(not nearest)
       (ai-wander world entity)]
      ;; Enemy adjacent - attack
      [(entity-adjacent? entity nearest)
       (make-ai-action 'attack (entity-id nearest))]
      ;; Enemy visible - chase
      [else
       (let* ([target-pos (entity-point nearest)]
              [path (world-find-path world current-pos target-pos #f)])
         (if (and path (not (null? path)))
             ;; Use pathfinding
             (let* ([next-pos (car path)]
                    [delta (point-sub next-pos current-pos)]
                    [dir (delta->direction delta)])
               (make-ai-action 'move dir))
             ;; No path - move directly toward target
             (let* ([delta (point-sub target-pos current-pos)]
                    [dir (delta->direction delta)])
               (if (world-can-move-to? world entity
                                       (+ (entity-x entity) (car (direction->delta dir)))
                                       (+ (entity-y entity) (cdr (direction->delta dir))))
                   (make-ai-action 'move dir)
                   (make-ai-action 'wait #f)))))])))

;;; ============================================================
;;; Flee Behavior
;;; ============================================================

;;; ai-flee : World × Entity → AIAction
;;; Run away from nearest enemy.
;;;
(define (ai-flee world entity)
  (let* ([current-pos (entity-point entity)]
         ;; Find nearest enemy
         [nearest (world-find-nearest world current-pos
                    (lambda (e)
                      (and (not (= (entity-id e) (entity-id entity)))
                           (entity-alive? e)
                           (is-hostile? entity e))))])
    (cond
      ;; No enemies - wander
      [(not nearest)
       (ai-wander world entity)]
      ;; Run away from enemy
      [else
       (let* ([enemy-pos (entity-point nearest)]
              ;; Calculate direction away from enemy
              [delta (point-sub current-pos enemy-pos)]
              [dx (car delta)]
              [dy (cdr delta)]
              ;; Normalize to a direction
              [dir (cond
                     [(and (> (abs dx) (abs dy)) (> dx 0)) 'east]
                     [(and (> (abs dx) (abs dy)) (< dx 0)) 'west]
                     [(and (<= (abs dx) (abs dy)) (> dy 0)) 'south]
                     [else 'north])]
              [dir-delta (direction->delta dir)]
              [new-x (+ (entity-x entity) (car dir-delta))]
              [new-y (+ (entity-y entity) (cdr dir-delta))])
         (if (world-can-move-to? world entity new-x new-y)
             (make-ai-action 'move dir)
             (make-ai-action 'wait #f)))])))

;;; ============================================================
;;; AI Behavior Dispatcher
;;; ============================================================

;;; ai-decide-action : World × Entity -> AIAction
;;; Main AI dispatcher - looks up entity's AI behavior and executes it.
;;;
(define (ai-decide-action world entity)
  (let ([ai-comp (entity-ai entity)])
    (if ai-comp
        (let ([behavior (ai-behavior ai-comp)])
          (case behavior
            ((idle) (ai-idle world entity))
            ((wander) (ai-wander world entity))
            ((wander-8) (ai-wander-8 world entity))
            ((guard) (ai-guard world entity))
            ((hunt) (ai-hunt world entity))
            ((flee) (ai-flee world entity))
            (else
             ;; Unknown behavior - default to idle
             (ai-idle world entity))))
        ;; No AI component - idle
        (ai-idle world entity))))

;;; ============================================================
;;; AI Utilities
;;; ============================================================

;;; is-hostile? : Entity × Entity -> Bool
;;; Check if entity1 is hostile to entity2.
(define (is-hostile? entity1 entity2)
  (let ([faction1 (entity-faction entity1)]
        [faction2 (entity-faction entity2)]
        [faction-comp1 (entity-get-component entity1 'faction)])
    (if faction-comp1
        (faction-is-hostile? faction-comp1 faction2)
        #f)))

;;; delta->direction : Point -> Direction
;;; Convert a delta (dx . dy) to a cardinal direction.
;;; Picks the dominant axis.
(define (delta->direction delta)
  (let ([dx (car delta)]
        [dy (cdr delta)])
    (cond
      ;; No movement
      [(and (= dx 0) (= dy 0)) 'north]
      ;; Diagonal - pick strongest component
      [(and (> (abs dx) (abs dy)) (> dx 0)) 'east]
      [(and (> (abs dx) (abs dy)) (< dx 0)) 'west]
      [(and (<= (abs dx) (abs dy)) (> dy 0)) 'south]
      [(<= (abs dx) (abs dy)) 'north]
      ;; Fallback
      [else 'north])))

;;; ============================================================
;;; Advanced AI (Optional Extensions)
;;; ============================================================

;;; ai-ambush : World × Entity → AIAction
;;; Hide and wait for player to approach, then attack.
(define (ai-ambush world entity)
  ;; TODO: Implement ambush logic
  ;; Would need: stealth system, perception checks
  (ai-guard world entity))

;;; ai-flank : World × Entity → AIAction
;;; Try to position behind or beside the player for advantage.
(define (ai-flank world entity)
  ;; TODO: Implement flanking logic
  ;; Would need: position evaluation, tactical positioning
  (ai-hunt world entity))

;;; ai-support : World × Entity → AIAction
;;; Stay near allies, heal or buff them.
(define (ai-support world entity)
  ;; TODO: Implement support logic
  ;; Would need: healing items/spells, ally detection
  (ai-guard world entity))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; AI Actions:
;;;   make-ai-action, ai-action-type, ai-action-data
;;;
;;; Core Behaviors:
;;;   ai-idle - Do nothing
;;;   ai-wander, ai-wander-8 - Random movement
;;;   ai-guard - Defend a location
;;;   ai-hunt - Chase and attack enemies
;;;   ai-flee - Run from enemies
;;;
;;; Main Dispatcher:
;;;   ai-decide-action - Execute entity's AI behavior
;;;
;;; Utilities:
;;;   is-hostile?, delta->direction
;;;
;;; Future Extensions:
;;;   ai-ambush, ai-flank, ai-support
;;;
;;; Usage Example:
;;;   ;; Set up a hunting goblin
;;;   (define goblin (make-monster "Goblin" #\g 10 10 'hunt))
;;;
;;;   ;; Each turn, get AI decision
;;;   (let ([action (ai-decide-action world goblin)])
;;;     (case (ai-action-type action)
;;;       [(move)
;;;        (let ([dir (ai-action-data action)])
;;;          (world-move-entity world (entity-id goblin) dir))]
;;;       [(attack)
;;;        (let ([target-id (ai-action-data action)])
;;;          ;; Perform combat
;;;          ...)]
;;;       [(wait)
;;;        ;; Do nothing
;;;        world]))
