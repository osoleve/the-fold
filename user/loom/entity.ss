;;; playpen/loom/entity.ss — Entity Component System for RPG SDK
;;;
;;; A flexible entity-component system for game objects.
;;; Entities are bags of components; components define behavior and data.
;;;
;;; This is Playpen code: the SDK for building roguelikes and dungeon crawlers.
;;;
;;; Dependencies:
;;;   - playpen/loom/core.ss (alist utilities, point operations, generate-id)
;;;
;;; Architecture:
;;;   - Entity: A unique ID with a bag of components
;;;   - Component: A named data structure attached to entities
;;;   - No inheritance; behavior from composition
;;;
;;; Exports:
;;;   Entity creation, components, queries
;;;
;;; ====
;;; MUTATION SEMANTICS — IMPORTANT!
;;; ====
;;;
;;; ALL entity and component operations are FUNCTIONAL:
;;;   - They return NEW entities/components
;;;   - Original values are NEVER modified
;;;   - You MUST capture the return value
;;;
;;; FUNCTIONAL (returns new value, MUST capture return):
;;;   - entity-add-component, entity-remove-component
;;;   - entity-update-component
;;;   - entity-move-to, entity-move-by, entity-move-dir
;;;   - entity-damage, entity-heal
;;;   - All component update functions (stats-set-hp, etc.)
;;;
;;; BEST PRACTICE:
;;;   Always capture return values:
;;;     ✓ (set! entity (entity-damage entity 10))
;;;     ✗ (entity-damage entity 10)  ; Lost the updated entity!
;;;
;;;   Use world-update-entity to update entities in the world:
;;;     ✓ (set! world (world-update-entity world eid
;;;                     (lambda (e) (entity-damage e 10))))
;;;
;;; WHY FUNCTIONAL?
;;;   - Entities are value types - easy to reason about
;;;   - No hidden mutations - clear data flow
;;;   - Easier to test, debug, and replay
;;;   - Functional transformations compose well
;;;

;;; ====
;;; Component Types
;;; ====

;;; Components are alists with a 'type key identifying the component kind.
;;; Each component type has specific expected fields.

;;; Component : Alist with at minimum (type . Symbol)

;;; component-type : Component -> Symbol
(define (component-type comp)
  (alist-ref comp 'type 'unknown))

;;; component? : Any -> Bool
(define (component? x)
  (and (list? x)
       (not (null? x))
       (assq 'type x)))

;;; ====
;;; Standard Component Constructors
;;; ====

;;; Position Component
;;; Represents location in the game world.
(define (make-position-component x y)
  `((type . position)
    (x . ,x)
    (y . ,y)))

(define (position-component? comp)
  (eq? (component-type comp) 'position))

(define (position-x comp)
  (alist-ref comp 'x 0))

(define (position-y comp)
  (alist-ref comp 'y 0))

(define (position->point comp)
  (cons (position-x comp) (position-y comp)))

(define (position-set-xy comp x y)
  (alist-set (alist-set comp 'x x) 'y y))

;;; Stats Component
;;; Core RPG statistics.
(define make-stats-component
  (case-lambda
   [() (make-stats-component 10 10 10 10 10)]
   [(hp max-hp)
    (make-stats-component hp max-hp 0 0 0)]
   [(hp max-hp attack defense speed)
    `((type . stats)
      (hp . ,hp)
      (max-hp . ,max-hp)
      (attack . ,attack)
      (defense . ,defense)
      (speed . ,speed))]))

(define (stats-component? comp)
  (eq? (component-type comp) 'stats))

(define (stats-hp comp) (alist-ref comp 'hp 0))
(define (stats-max-hp comp) (alist-ref comp 'max-hp 0))
(define (stats-attack comp) (alist-ref comp 'attack 0))
(define (stats-defense comp) (alist-ref comp 'defense 0))
(define (stats-speed comp) (alist-ref comp 'speed 0))

(define (stats-alive? comp)
  (> (stats-hp comp) 0))

(define (stats-set-hp comp hp)
  (alist-set comp 'hp (clamp hp 0 (stats-max-hp comp))))

(define (stats-modify-hp comp delta)
  (stats-set-hp comp (+ (stats-hp comp) delta)))

;;; Renderable Component
;;; How the entity appears on screen.
(define make-renderable-component
  (case-lambda
   [(char) (make-renderable-component char 50 #f)]
   [(char layer) (make-renderable-component char layer #f)]
   [(char layer color)
    `((type . renderable)
      (char . ,char)
      (layer . ,layer)
      (color . ,color))]))

(define (renderable-component? comp)
  (eq? (component-type comp) 'renderable))

(define (renderable-char comp) (alist-ref comp 'char #\?))
(define (renderable-layer comp) (alist-ref comp 'layer 50))
(define (renderable-color comp) (alist-ref comp 'color #f))

;;; Name Component
;;; Human-readable identifier.
(define (make-name-component name description)
  `((type . name)
    (name . ,name)
    (description . ,description)))

(define (name-component? comp)
  (eq? (component-type comp) 'name))

(define (name-name comp) (alist-ref comp 'name "Unknown"))
(define (name-description comp) (alist-ref comp 'description ""))

;;; AI Component
;;; Behavior controller for NPCs.
(define make-ai-component
  (case-lambda
   [(behavior) (make-ai-component behavior '())]
   [(behavior state)
    `((type . ai)
      (behavior . ,behavior)
      (state . ,state))]))

(define (ai-component? comp)
  (eq? (component-type comp) 'ai))

(define (ai-behavior comp) (alist-ref comp 'behavior 'idle))
(define (ai-state comp) (alist-ref comp 'state '()))

(define (ai-set-behavior comp behavior)
  (alist-set comp 'behavior behavior))

(define (ai-set-state comp state)
  (alist-set comp 'state state))

(define (ai-update-state comp key value)
  (alist-set comp 'state (alist-set (ai-state comp) key value)))

;;; Inventory Component
;;; Items carried by the entity.
(define make-inventory-component
  (case-lambda
   [() (make-inventory-component 20 '())]
   [(capacity) (make-inventory-component capacity '())]
   [(capacity items)
    `((type . inventory)
      (capacity . ,capacity)
      (items . ,items))]))

(define (inventory-component? comp)
  (eq? (component-type comp) 'inventory))

(define (inventory-capacity comp) (alist-ref comp 'capacity 20))
(define (inventory-items comp) (alist-ref comp 'items '()))
(define (inventory-count comp) (length (inventory-items comp)))
(define (inventory-full? comp)
  (>= (inventory-count comp) (inventory-capacity comp)))

(define (inventory-add-item comp item-id)
  (if (inventory-full? comp)
      comp
      (alist-set comp 'items (cons item-id (inventory-items comp)))))

(define (inventory-remove-item comp item-id)
  (alist-set comp 'items
             (filter (lambda (id) (not (equal? id item-id)))
                     (inventory-items comp))))

(define (inventory-has-item? comp item-id)
  (member item-id (inventory-items comp)))

;;; Equipment Component
;;; Items equipped in slots.
(define (make-equipment-component)
  `((type . equipment)
    (weapon . #f)
    (armor . #f)
    (shield . #f)
    (helmet . #f)
    (ring . #f)
    (amulet . #f)))

(define (equipment-component? comp)
  (eq? (component-type comp) 'equipment))

(define (equipment-get-slot comp slot)
  (alist-ref comp slot #f))

(define (equipment-set-slot comp slot item-id)
  (alist-set comp slot item-id))

(define (equipment-clear-slot comp slot)
  (alist-set comp slot #f))

;;; Faction Component
;;; Team/allegiance for determining hostility.
(define make-faction-component
  (case-lambda
   [(faction) (make-faction-component faction '())]
   [(faction hostile-to)
    `((type . faction)
      (faction . ,faction)
      (hostile-to . ,hostile-to))]))

(define (faction-component? comp)
  (eq? (component-type comp) 'faction))

(define (faction-faction comp) (alist-ref comp 'faction 'neutral))
(define (faction-hostile-to comp) (alist-ref comp 'hostile-to '()))

(define (faction-is-hostile? comp target-faction)
  (memq target-faction (faction-hostile-to comp)))

;;; Actor Component
;;; Marks entities that take turns.
(define make-actor-component
  (case-lambda
   [() (make-actor-component 100 0)]
   [(energy) (make-actor-component energy 0)]
   [(energy initiative)
    `((type . actor)
      (energy . ,energy)
      (max-energy . 100)
      (initiative . ,initiative))]))

(define (actor-component? comp)
  (eq? (component-type comp) 'actor))

(define (actor-energy comp) (alist-ref comp 'energy 0))
(define (actor-max-energy comp) (alist-ref comp 'max-energy 100))
(define (actor-initiative comp) (alist-ref comp 'initiative 0))

(define (actor-can-act? comp)
  (>= (actor-energy comp) 100))

(define (actor-spend-energy comp amount)
  (alist-set comp 'energy (- (actor-energy comp) amount)))

(define (actor-gain-energy comp amount)
  (alist-set comp 'energy
             (min (actor-max-energy comp)
                  (+ (actor-energy comp) amount))))

;;; Blocker Component
;;; Marks entities that block movement.
(define make-blocker-component
  (case-lambda
   [() (make-blocker-component #t #t)]
   [(blocks-movement) (make-blocker-component blocks-movement #t)]
   [(blocks-movement blocks-sight)
    `((type . blocker)
      (blocks-movement . ,blocks-movement)
      (blocks-sight . ,blocks-sight))]))

(define (blocker-component? comp)
  (eq? (component-type comp) 'blocker))

(define (blocker-blocks-movement? comp)
  (alist-ref comp 'blocks-movement #f))

(define (blocker-blocks-sight? comp)
  (alist-ref comp 'blocks-sight #f))

;;; ====
;;; Entity Structure
;;; ====

;;; Entity : Alist
;;;   (id . Nat)           — Unique identifier
;;;   (components . Alist) — Component type -> Component

;;; make-entity : [Nat] -> Entity
;;; Create a new entity with optional specific ID.
(define make-entity
  (case-lambda
   [() (make-entity (generate-id))]
   [(id)
    `((id . ,id)
      (components . ()))]))

;;; entity-id : Entity -> Nat
(define (entity-id entity)
  (alist-ref entity 'id 0))

;;; entity-components : Entity -> Alist
(define (entity-components entity)
  (alist-ref entity 'components '()))

;;; entity-add-component : Entity × Component -> Entity
;;; Add or replace a component.
(define (entity-add-component entity comp)
  (let* ([comp-type (component-type comp)]
         [comps (entity-components entity)]
         [new-comps (alist-set comps comp-type comp)])
        (alist-set entity 'components new-comps)))

;;; entity-remove-component : Entity × Symbol -> Entity
;;; Remove a component by type.
(define (entity-remove-component entity comp-type)
  (let* ([comps (entity-components entity)]
         [new-comps (alist-remove comps comp-type)])
        (alist-set entity 'components new-comps)))

;;; entity-get-component : Entity × Symbol -> Component | #f
;;; Get a component by type.
(define (entity-get-component entity comp-type)
  (alist-ref (entity-components entity) comp-type))

;;; entity-has-component? : Entity × Symbol -> Bool
(define (entity-has-component? entity comp-type)
  (not (not (entity-get-component entity comp-type))))

;;; entity-update-component : Entity × Symbol × (Component -> Component) -> Entity
;;; Update a component using a function.
(define (entity-update-component entity comp-type update-fn)
  (let ([comp (entity-get-component entity comp-type)])
       (if comp
           (entity-add-component entity (update-fn comp))
           entity)))

;;; ====
;;; Entity Convenience Accessors
;;; ====

;;; Direct accessors for common component properties.

(define (entity-position entity)
  (entity-get-component entity 'position))

(define (entity-x entity)
  (let ([pos (entity-position entity)])
       (if pos (position-x pos) 0)))

(define (entity-y entity)
  (let ([pos (entity-position entity)])
       (if pos (position-y pos) 0)))

(define (entity-point entity)
  (let ([pos (entity-position entity)])
       (if pos (position->point pos) (cons 0 0))))

(define (entity-stats entity)
  (entity-get-component entity 'stats))

(define (entity-hp entity)
  (let ([stats (entity-stats entity)])
       (if stats (stats-hp stats) 0)))

(define (entity-alive? entity)
  (let ([stats (entity-stats entity)])
       (if stats (stats-alive? stats) #t)))

(define (entity-renderable entity)
  (entity-get-component entity 'renderable))

(define (entity-char entity)
  (let ([rend (entity-renderable entity)])
       (if rend (renderable-char rend) #\?)))

(define (entity-name entity)
  (let ([name-comp (entity-get-component entity 'name)])
       (if name-comp (name-name name-comp) "Unknown")))

(define (entity-ai entity)
  (entity-get-component entity 'ai))

(define (entity-actor entity)
  (entity-get-component entity 'actor))

(define (entity-inventory entity)
  (entity-get-component entity 'inventory))

(define (entity-faction entity)
  (let ([f (entity-get-component entity 'faction)])
       (if f (faction-faction f) 'neutral)))

;;; ====
;;; Entity Modification Helpers
;;; ====

;;; entity-move-to : Entity × Int × Int -> Entity
;;; Move entity to new position.
(define (entity-move-to entity x y)
  (entity-update-component entity 'position
                           (lambda (pos) (position-set-xy pos x y))))

;;; entity-move-by : Entity × Int × Int -> Entity
;;; Move entity by delta.
(define (entity-move-by entity dx dy)
  (entity-move-to entity
                  (+ (entity-x entity) dx)
                  (+ (entity-y entity) dy)))

;;; entity-move-dir : Entity × Direction -> Entity
;;; Move entity in a direction.
(define (entity-move-dir entity dir)
  (let ([delta (direction->delta dir)])
       (entity-move-by entity (car delta) (cdr delta))))

;;; entity-damage : Entity × Nat -> Entity
;;; Apply damage to entity.
(define (entity-damage entity amount)
  (entity-update-component entity 'stats
                           (lambda (stats) (stats-modify-hp stats (- amount)))))

;;; entity-heal : Entity × Nat -> Entity
;;; Heal entity.
(define (entity-heal entity amount)
  (entity-update-component entity 'stats
                           (lambda (stats) (stats-modify-hp stats amount))))

;;; ====
;;; Entity Factory Functions
;;; ====

;;; Convenient functions for creating common entity types.

;;; make-creature : String × Char × Int × Int × Alist -> Entity
;;; Create a basic creature entity.
(define (make-creature name char x y stats-alist)
  (let* ([entity (make-entity)]
         [hp (alist-ref stats-alist 'hp 10)]
         [max-hp (alist-ref stats-alist 'max-hp hp)]
         [attack (alist-ref stats-alist 'attack 5)]
         [defense (alist-ref stats-alist 'defense 0)]
         [speed (alist-ref stats-alist 'speed 100)])
        (-> entity
            (entity-add-component (make-position-component x y))
            (entity-add-component (make-stats-component hp max-hp attack defense speed))
            (entity-add-component (make-renderable-component char))
            (entity-add-component (make-name-component name ""))
            (entity-add-component (make-actor-component))
            (entity-add-component (make-blocker-component #t #f)))))

;;; Pipe macro for cleaner chaining
(define-syntax ->
  (syntax-rules ()
                [(_ val) val]
                [(_ val (fn args ...) rest ...)
                 (-> (fn val args ...) rest ...)]
                [(_ val fn rest ...)
                 (-> (fn val) rest ...)]))

;;; make-player : String × Char × Int × Int -> Entity
;;; Create a player entity.
(define (make-player name char x y)
  (-> (make-creature name char x y
                     '((hp . 30) (max-hp . 30) (attack . 10) (defense . 5) (speed . 100)))
      (entity-add-component (make-inventory-component 20))
      (entity-add-component (make-equipment-component))
      (entity-add-component (make-faction-component 'player '(monster)))))

;;; make-monster : String × Char × Int × Int × Symbol -> Entity
;;; Create a monster entity with AI.
(define (make-monster name char x y behavior)
  (-> (make-creature name char x y
                     '((hp . 10) (max-hp . 10) (attack . 5) (defense . 2) (speed . 80)))
      (entity-add-component (make-ai-component behavior))
      (entity-add-component (make-faction-component 'monster '(player)))))

;;; make-item : String × Char × Int × Int × Symbol -> Entity
;;; Create an item entity (no movement, can be picked up).
(define (make-item name char x y item-type)
  (let ([entity (make-entity)])
       (-> entity
           (entity-add-component (make-position-component x y))
           (entity-add-component (make-renderable-component char 30))
           (entity-add-component (make-name-component name ""))
           (entity-add-component `((type . item) (item-type . ,item-type))))))

;;; ====
;;; Entity Predicates
;;; ====

(define (entity-is-player? entity)
  (eq? (entity-faction entity) 'player))

(define (entity-is-monster? entity)
  (entity-has-component? entity 'ai))

(define (entity-is-item? entity)
  (entity-has-component? entity 'item))

(define (entity-is-actor? entity)
  (entity-has-component? entity 'actor))

(define (entity-can-act? entity)
  (let ([actor (entity-actor entity)])
       (and actor (actor-can-act? actor))))

(define (entity-blocks-movement? entity)
  (let ([blocker (entity-get-component entity 'blocker)])
       (and blocker (blocker-blocks-movement? blocker))))

(define (entity-blocks-sight? entity)
  (let ([blocker (entity-get-component entity 'blocker)])
       (and blocker (blocker-blocks-sight? blocker))))

;;; ====
;;; Entity Distance and Targeting
;;; ====

;;; entity-distance : Entity × Entity -> Nat
;;; Manhattan distance between entities.
(define (entity-distance e1 e2)
  (manhattan-distance (entity-point e1) (entity-point e2)))

;;; entity-adjacent? : Entity × Entity -> Bool
;;; Check if entities are adjacent (including diagonal).
(define (entity-adjacent? e1 e2)
  (<= (chebyshev-distance (entity-point e1) (entity-point e2)) 1))

;;; entity-at-point? : Entity × Point -> Bool
(define (entity-at-point? entity pt)
  (point=? (entity-point entity) pt))

;;; ====
;;; Export Summary
;;; ====
;;;
;;; Component Utilities:
;;;   component-type, component?
;;;
;;; Position Component:
;;;   make-position-component, position-component?
;;;   position-x, position-y, position->point, position-set-xy
;;;
;;; Stats Component:
;;;   make-stats-component, stats-component?
;;;   stats-hp, stats-max-hp, stats-attack, stats-defense, stats-speed
;;;   stats-alive?, stats-set-hp, stats-modify-hp
;;;
;;; Renderable Component:
;;;   make-renderable-component, renderable-component?
;;;   renderable-char, renderable-layer, renderable-color
;;;
;;; Name Component:
;;;   make-name-component, name-component?, name-name, name-description
;;;
;;; AI Component:
;;;   make-ai-component, ai-component?, ai-behavior, ai-state
;;;   ai-set-behavior, ai-set-state, ai-update-state
;;;
;;; Inventory Component:
;;;   make-inventory-component, inventory-component?
;;;   inventory-capacity, inventory-items, inventory-count, inventory-full?
;;;   inventory-add-item, inventory-remove-item, inventory-has-item?
;;;
;;; Equipment Component:
;;;   make-equipment-component, equipment-component?
;;;   equipment-get-slot, equipment-set-slot, equipment-clear-slot
;;;
;;; Faction Component:
;;;   make-faction-component, faction-component?
;;;   faction-faction, faction-hostile-to, faction-is-hostile?
;;;
;;; Actor Component:
;;;   make-actor-component, actor-component?
;;;   actor-energy, actor-max-energy, actor-initiative
;;;   actor-can-act?, actor-spend-energy, actor-gain-energy
;;;
;;; Blocker Component:
;;;   make-blocker-component, blocker-component?
;;;   blocker-blocks-movement?, blocker-blocks-sight?
;;;
;;; Entity:
;;;   make-entity, entity-id, entity-components
;;;   entity-add-component, entity-remove-component, entity-get-component
;;;   entity-has-component?, entity-update-component
;;;
;;; Entity Accessors:
;;;   entity-position, entity-x, entity-y, entity-point
;;;   entity-stats, entity-hp, entity-alive?
;;;   entity-renderable, entity-char, entity-name
;;;   entity-ai, entity-actor, entity-inventory, entity-faction
;;;
;;; Entity Modification:
;;;   entity-move-to, entity-move-by, entity-move-dir
;;;   entity-damage, entity-heal
;;;
;;; Entity Factories:
;;;   make-creature, make-player, make-monster, make-item
;;;
;;; Entity Predicates:
;;;   entity-is-player?, entity-is-monster?, entity-is-item?, entity-is-actor?
;;;   entity-can-act?, entity-blocks-movement?, entity-blocks-sight?
;;;
;;; Entity Spatial:
;;;   entity-distance, entity-adjacent?, entity-at-point?
