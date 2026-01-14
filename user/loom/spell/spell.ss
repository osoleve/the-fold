;;; playpen/loom/spell/spell.ss — Spell: The Game Logic DSL
;;;
;;; Spell is a domain-specific language for weaving game logic in The Fold.
;;; It provides declarative macros for defining entities, components, events,
;;; and behaviors with minimal boilerplate.
;;;
;;; "Spell" — because D-S-L sounds like "spell", and you're casting game logic!
;;;
;;; This is Playpen code: the DSL for building games with Loom.
;;;
;;; Usage:
;;;   (load "user/loom/spell/spell.ss")
;;;
;;; This loads the Loom framework first, then adds the Spell DSL on top.
;;;
;;; ====
;;; Core DSL Forms (Layer 1)
;;; ====
;;;
;;; Pipe Macros:
;;;   ->           Thread value through transformations
;;;   ->!          Chain side-effecting operations
;;;   ->>          Thread-last variant
;;;   when->       Conditional threading (if true)
;;;   unless->     Conditional threading (if false)
;;;
;;; Component Definition:
;;;   def-component    Define a component with auto-generated accessors
;;;
;;; Entity Definition:
;;;   def-entity       Define an entity factory with components
;;;   with-entity      Scoped entity transformations in a world
;;;
;;; Event Handling:
;;;   on-event         Register event handler with destructuring
;;;   on-events        Batch register multiple handlers
;;;   def-handler      Define a named reusable handler
;;;   emit!            Fire an event immediately
;;;   queue!           Queue an event for deferred processing
;;;
;;; ====
;;; Game DSL Forms (Layer 2)
;;; ====
;;;
;;; Behaviors:
;;;   def-behavior     State machine AI behaviors with states/transitions
;;;
;;; Actions:
;;;   def-action       Custom actions with validation, cost, execution
;;;   try-action       Execute a custom action with validation
;;;
;;; Predicates:
;;;   alive?, dead?, hurt?, wounded?, critical?
;;;   adjacent?, in-range?, hostile?, friendly?
;;;   player-nearby?, safe?, threatened?, outnumbered?
;;;
;;; Game Configuration:
;;;   def-game         Top-level game definition
;;;   game-create      Create world from game config
;;;   game-check-state Check win/lose conditions
;;;
;;; Generation (Layer 3 - Future):
;;;   def-generator    Procedural world generation
;;;   def-recipe       Reusable room/area templates

;;; ====
;;; Load Dependencies
;;; ====

;;; Load the Loom framework first (provides entity, event, world, etc.)
(load "user/loom/loom.ss")

;;; Load the Spell prelude (all DSL macros)
(load "user/loom/spell/prelude.ss")

;;; ====
;;; DSL Version
;;; ====

(define *spell-version* "0.2.1")

;;; ====
;;; Standard Component Reference
;;; ====
;;;
;;; Position: (make-position-component x y)
;;;   - Auto-added by def-entity when using (x y) params
;;;
;;; Stats: (make-stats-component hp max-hp attack defense speed)
;;;   - hp/max-hp: Health points
;;;   - attack: Damage dealt
;;;   - defense: Damage reduction
;;;   - speed: Turn order priority (higher = faster)
;;;
;;; Renderable: (make-renderable-component char [layer] [color])
;;;   - char: Display character (#\@ for player, #\g for goblin, etc.)
;;;   - layer: Draw order (default 50, higher = on top)
;;;   - color: Symbol like 'red, 'green, or #f
;;;
;;; Name: (make-name-component name description)
;;;   - name: Display name string
;;;   - description: Flavor text
;;;
;;; AI: (make-ai-component behavior [state])
;;;   - behavior: Symbol - 'idle, 'wander, 'hunt, 'guard, 'flee
;;;   - state: Alist for behavior-specific data
;;;
;;; Faction: (make-faction-component faction hostile-to-list)
;;;   - faction: Symbol like 'player, 'monster, 'neutral
;;;   - hostile-to-list: List of faction symbols to attack
;;;
;;; Actor: (make-actor-component [energy] [initiative])
;;;   - Marks entity as taking turns (REQUIRED for turn-based action)
;;;
;;; Blocker: (make-blocker-component blocks-movement blocks-sight)
;;;   - blocks-movement: #t if impassable
;;;   - blocks-sight: #t if opaque
;;;
;;; Inventory: (make-inventory-component [capacity] [items])
;;; Equipment: (make-equipment-component)
;;;
;;; ====
;;; Standard Event Data Shapes
;;; ====
;;;
;;; entity-died:
;;;   source = killer entity ID (or #f, 'trap, 'poison)
;;;   target = victim entity ID
;;;   data   = ((victim . ID) (killer . ID/Symbol))
;;;
;;; damage-dealt:
;;;   source = attacker entity ID
;;;   target = defender entity ID
;;;   data   = ((amount . Nat) (damage-type . Symbol))
;;;
;;; entity-moved:
;;;   source = entity ID
;;;   target = destination point (x . y)
;;;   data   = ((from . Point) (to . Point))
;;;
;;; item-picked-up:
;;;   source = picker entity ID
;;;   target = item entity ID
;;;   data   = ((item-id . ID))
;;;
;;; turn-started / turn-ended:
;;;   source = #f, target = #f
;;;   data   = ((turn-number . Nat))
;;;

;;; ====
;;; Quick Reference
;;; ====

#|
;;; ==== COMPONENT DEFINITION ====

(def-component mana
  ((current 0)     ; starting mana
   (maximum 100)   ; mana cap
   (regen 5)))     ; regen per turn

;; Generates: make-mana-component, mana-component?,
;;            mana-current, mana-set-current, etc.


;;; ==== ENTITY DEFINITION ====

;; Note: def-entity auto-adds position-component from (x y) params
;; Note: Add actor-component if entity should take turns!

(def-entity goblin (x y)
  :components
  (make-stats-component 10   ; hp
                        10   ; max-hp
                        5    ; attack
                        2    ; defense
                        80)  ; speed
  (make-renderable-component #\g)
  (make-name-component "Goblin" "A sneaky creature")
  (make-ai-component 'hunt)
  (make-actor-component)           ; needed for turns!
  (make-faction-component 'monster '(player)))

;; Generates: (make-goblin x y) factory function


;;; ==== EVENT HANDLING ====

;; Preferred form: :do first (binds variables), then :when (uses them)
(on-event dispatcher entity-died
          :do (killer victim data)
          (display "Someone died!\n"))

(on-event dispatcher damage-dealt
          :do (attacker target data)
          :when (> (alist-ref data 'amount) 10)
          (display "Big hit!\n"))


;;; ==== ENTITY UPDATES ====

(set! world
      (with-entity world player-id
                   (-> (entity-damage 10)
                       (entity-heal 5))))


;;; ==== PIPE MACROS ====

;; Use :let to capture value for use in conditions
(-> entity
    (:let e)
    (:when (entity-alive? e))
    (entity-damage 10)
    (entity-add-component (make-status-effect 'burning)))

(->> items
     (filter item-valuable?)
     (map item-value)
     (fold-left + 0))

(->! canvas
     (canvas-set! 0 0 #\#)
     (canvas-set! 1 0 #\#))


;;; ==== BEHAVIOR STATE MACHINES ====

;; Define a cowardly AI that flees when hurt
(def-behavior coward
  (initial normal)
  (vars [flee-threshold 30])
  
  (state normal
         (on tick
             (if (< (self-hp-percent entity) flee-threshold)
                 (-> fleeing)
                 (-> normal)))
         (action (ai-hunt world entity)))
  
  (state fleeing
         (on enter (display "Entity starts fleeing!\n"))
         (on tick
             (if (> (self-hp-percent entity) 50)
                 (-> normal)
                 (-> fleeing)))
         (action (ai-flee world entity))))


;;; ==== CUSTOM ACTIONS ====

;; Define a healing spell action
(def-action heal-self
  :validate (and (alive? actor)
                 (< (entity-hp-percent actor) 100))
  :cost 50
  :execute (entity-heal actor 20)
  :events ((make-event 'spell-cast (entity-id actor) (entity-id actor)
                       '((spell . heal) (amount . 20)))))

;; Action with parameters from data alist
(def-action fireball
  :params (damage mana-cost)
  :validate (and (>= (entity-mana actor) mana-cost)
                 (alive? target)
                 (in-range? actor target 10))
  :cost 150
  :execute (entity-damage target damage))


;;; ==== GAME DEFINITION ====

(def-game dungeon-escape
  (title "Dungeon Escape")
  (description "Find the stairs and escape!")
  (world 80 40)
  
  (player
   (name "Hero")
   (char #\@)
   (stats 100 100 15 8 100)
   (color 'white))
  
  (win-when (player-at-tile? world 'stairs-up))
  (lose-when (<= (player-hp world) 0)))

;; Create and run:
;; (define world (game-create dungeon-escape))
;; (game-check-state dungeon-escape world) ; -> 'playing, 'won, or 'lost


;;; ==== PREDICATES ====

;; Use predicates in conditions
(if (and (alive? entity)
         (wounded? entity 50)
         (threatened? world entity))
    (flee!)
    (attack!))

;; Common predicates: alive?, dead?, hurt?, wounded?, critical?
;;                    adjacent?, in-range?, hostile?, friendly?
;;                    player-nearby?, safe?, threatened?, outnumbered?
|#

;;; ====
;;; Loaded Message
;;; ====

(display "Spell v")
(display *spell-version*)
(display " loaded — the game logic DSL.\n")
(display "Layer 1: def-component, def-entity, on-event, with-entity, ->, ->!, ->>\n")
(display "Layer 2: def-behavior, def-action, def-game, predicates\n")
