;;; playpen/loom/spell/predicates.ss — Condition Predicates
;;;
;;; Common predicate functions for use in behaviors, actions, and events.
;;; These provide a consistent vocabulary for expressing game conditions.
;;;
;;; This is part of Spell: the DSL for weaving game logic.
;;;
;;; Exports:
;;;   Entity predicates: alive?, dead?, hurt?, healthy?, etc.
;;;   Position predicates: adjacent?, in-range?, at-point?, etc.
;;;   Faction predicates: hostile?, friendly?, neutral?, etc.
;;;   World predicates: tile-is?, blocked?, visible?, etc.

;;; ====
;;; Entity Health Predicates
;;; ====

;;; alive? : Entity -> Bool
;;; Entity has HP > 0.
(define (alive? entity)
  (entity-alive? entity))

;;; dead? : Entity -> Bool
;;; Entity has HP <= 0.
(define (dead? entity)
  (not (entity-alive? entity)))

;;; hurt? : Entity -> Bool
;;; Entity is below 100% HP.
(define (hurt? entity)
  (< (entity-hp-percent entity) 100))

;;; healthy? : Entity -> Bool
;;; Entity is at 100% HP.
(define (healthy? entity)
  (= (entity-hp-percent entity) 100))

;;; wounded? : Entity × [Nat] -> Bool
;;; Entity HP is below threshold (default 50%).
(define wounded?
  (case-lambda
   [(entity) (wounded? entity 50)]
   [(entity threshold)
    (< (entity-hp-percent entity) threshold)]))

;;; critical? : Entity × [Nat] -> Bool
;;; Entity HP is below critical threshold (default 25%).
(define critical?
  (case-lambda
   [(entity) (critical? entity 25)]
   [(entity threshold)
    (< (entity-hp-percent entity) threshold)]))

;;; full-hp? : Entity -> Bool
;;; Entity HP equals max HP.
(define (full-hp? entity)
  (let ([stats (entity-stats entity)])
       (if stats
           (= (stats-hp stats) (stats-max-hp stats))
           #t)))

;;; ====
;;; Entity State Predicates
;;; ====

;;; has-component? : Entity × Symbol -> Bool
;;; Entity has a component of the given type.
(define (has-component? entity type)
  (entity-has-component? entity type))

;;; is-actor? : Entity -> Bool
;;; Entity has an actor component (can take turns).
(define (is-actor? entity)
  (entity-has-component? entity 'actor))

;;; is-ai? : Entity -> Bool
;;; Entity has an AI component.
(define (is-ai? entity)
  (entity-has-component? entity 'ai))

;;; is-blocker? : Entity -> Bool
;;; Entity blocks movement.
(define (is-blocker? entity)
  (let ([blocker (entity-get-component entity 'blocker)])
       (and blocker (alist-ref blocker 'blocks-movement #f))))

;;; is-opaque? : Entity -> Bool
;;; Entity blocks line of sight.
(define (is-opaque? entity)
  (let ([blocker (entity-get-component entity 'blocker)])
       (and blocker (alist-ref blocker 'blocks-sight #f))))

;;; ====
;;; Position Predicates
;;; ====

;;; adjacent? : Entity × Entity -> Bool
;;; Entities are within 1 tile of each other (including diagonal).
(define (adjacent? e1 e2)
  (entity-adjacent? e1 e2))

;;; in-range? : Entity × Entity × Nat -> Bool
;;; Entity 2 is within range tiles of entity 1.
(define (in-range? e1 e2 range)
  (<= (entity-distance e1 e2) range))

;;; at-point? : Entity × Point -> Bool
;;; Entity is at the given point.
(define (at-point? entity point)
  (let ([pos (entity-position entity)])
       (if pos
           (and (= (position-x pos) (point-x point))
                (= (position-y pos) (point-y point)))
           #f)))

;;; same-position? : Entity × Entity -> Bool
;;; Entities are at the same position.
(define (same-position? e1 e2)
  (let ([p1 (entity-point e1)]
        [p2 (entity-point e2)])
       (and p1 p2 (= (point-x p1) (point-x p2)) (= (point-y p1) (point-y p2)))))

;;; distance-from : Entity × Entity -> Nat
;;; Get the distance between two entities.
(define (distance-from e1 e2)
  (entity-distance e1 e2))

;;; distance-from-point : Entity × Point -> Nat
;;; Get the distance from entity to a point.
(define (distance-from-point entity point)
  (let ([pos (entity-point entity)])
       (if pos
           (manhattan-distance pos point)
           +inf.0)))

;;; min-distance? : Entity × Entity × Nat -> Bool
;;; Entity 2 is at least range tiles away from entity 1.
;;; Useful for charge attacks that require minimum distance.
(define (min-distance? e1 e2 range)
  (>= (entity-distance e1 e2) range))

;;; ====
;;; Faction Predicates
;;; ====

;;; hostile? : Entity × Entity -> Bool
;;; Entity 1 considers entity 2 hostile.
(define (hostile? e1 e2)
  (is-hostile? e1 e2))

;;; friendly? : Entity × Entity -> Bool
;;; Entities are in the same faction.
(define (friendly? e1 e2)
  (let ([f1 (entity-faction e1)]
        [f2 (entity-faction e2)])
       (and f1 f2 (eq? f1 f2))))

;;; neutral-to? : Entity × Entity -> Bool
;;; Entity 1 is neutral to entity 2 (neither hostile nor friendly).
(define (neutral-to? e1 e2)
  (and (not (hostile? e1 e2))
       (not (friendly? e1 e2))))

;;; faction-is? : Entity × Symbol -> Bool
;;; Entity belongs to the given faction.
(define (faction-is? entity faction)
  (eq? (entity-faction entity) faction))

;;; is-player? : Entity -> Bool
;;; Entity is in the player faction.
(define (is-player? entity)
  (faction-is? entity 'player))

;;; is-monster? : Entity -> Bool
;;; Entity is in the monster faction.
(define (is-monster? entity)
  (faction-is? entity 'monster))

;;; ally-nearby? : World × Entity × [Nat] -> Bool
;;; There is a friendly entity within range.
(define ally-nearby?
  (case-lambda
   [(world entity) (ally-nearby? world entity 5)]
   [(world entity range)
    (not (null? (world-find-entities world
                                     (lambda (e)
                                             (and (not (= (entity-id e) (entity-id entity)))
                                                  (alive? e)
                                                  (friendly? entity e)
                                                  (in-range? entity e range))))))]))

;;; allies-in-range : World × Entity × Nat -> (List Entity)
;;; Get all friendly entities within range.
(define (allies-in-range world entity range)
  (world-find-entities world
                       (lambda (e)
                               (and (not (= (entity-id e) (entity-id entity)))
                                    (alive? e)
                                    (friendly? entity e)
                                    (in-range? entity e range)))))

;;; enemies-in-range : World × Entity × Nat -> (List Entity)
;;; Get all hostile entities within range.
(define (enemies-in-range world entity range)
  (world-find-entities world
                       (lambda (e)
                               (and (not (= (entity-id e) (entity-id entity)))
                                    (alive? e)
                                    (hostile? entity e)
                                    (in-range? entity e range)))))

;;; ====
;;; World Position Predicates
;;; ====

;;; tile-is? : World × Point × Symbol -> Bool
;;; Tile at point is of the given type.
(define (tile-is? world point tile-type)
  (let ([tile (world-get-tile world (point-x point) (point-y point))])
       (and tile (eq? (tile-type tile) tile-type))))

;;; walkable? : World × Point -> Bool
;;; Point is walkable (no blocking tiles or entities).
(define (walkable? world point)
  (world-passable? world (point-x point) (point-y point)))

;;; blocked? : World × Point -> Bool
;;; Point is blocked by tile or entity.
(define (blocked? world point)
  (not (walkable? world point)))

;;; has-entity-at? : World × Point -> Bool
;;; There is an entity at the given point.
(define (has-entity-at? world point)
  (let ([entities (world-entities-at world (point-x point) (point-y point))])
       (not (null? entities))))

;;; ====
;;; Player-Relative Predicates
;;; ====

;;; player-nearby? : World × Entity × [Nat] -> Bool
;;; The player is within range of the entity.
(define player-nearby?
  (case-lambda
   [(world entity) (player-nearby? world entity 5)]
   [(world entity range)
    (player-in-range? world entity range)]))

;;; player-adjacent-to? : World × Entity -> Bool
;;; The player is adjacent to the entity.
(define (player-adjacent-to? world entity)
  (player-adjacent? world entity))

;;; player-visible-from? : World × Entity -> Bool
;;; The entity can "see" the player (line of sight).
;;; For now, uses simple distance check. Full FOV would need raycasting.
(define (player-visible-from? world entity)
  (let ([player (world-get-player world)])
       (if player
           (let ([sight-range (or (alist-ref (entity-ai entity) 'sight-range 10) 10)])
                (<= (entity-distance entity player) sight-range))
           #f)))

;;; ====
;;; Combat Predicates
;;; ====

;;; can-attack? : Entity × Entity -> Bool
;;; Entity 1 can attack entity 2 (adjacent and hostile).
(define (can-attack? attacker target)
  (and (adjacent? attacker target)
       (hostile? attacker target)
       (alive? target)))

;;; can-be-attacked? : Entity -> Bool
;;; Entity can be targeted for attack.
(define (can-be-attacked? entity)
  (and (alive? entity)
       (entity-has-component? entity 'stats)))

;;; stronger-than? : Entity × Entity -> Bool
;;; Entity 1 has higher attack than entity 2.
(define (stronger-than? e1 e2)
  (> (entity-attack e1) (entity-attack e2)))

;;; weaker-than? : Entity × Entity -> Bool
;;; Entity 1 has lower attack than entity 2.
(define (weaker-than? e1 e2)
  (< (entity-attack e1) (entity-attack e2)))

;;; ====
;;; Composite Predicates
;;; ====

;;; safe? : World × Entity -> Bool
;;; No hostile entities nearby.
(define (safe? world entity)
  (null? (world-find-entities world
                              (lambda (e)
                                      (and (not (= (entity-id e) (entity-id entity)))
                                           (alive? e)
                                           (hostile? entity e)
                                           (in-range? entity e 5))))))

;;; threatened? : World × Entity -> Bool
;;; At least one hostile entity nearby.
(define (threatened? world entity)
  (not (safe? world entity)))

;;; outnumbered? : World × Entity -> Bool
;;; More hostile entities nearby than friendly.
(define (outnumbered? world entity)
  (let ([enemies (world-find-entities world
                                      (lambda (e)
                                              (and (not (= (entity-id e) (entity-id entity)))
                                                   (alive? e)
                                                   (hostile? entity e)
                                                   (in-range? entity e 10))))]
        [allies (world-find-entities world
                                     (lambda (e)
                                             (and (not (= (entity-id e) (entity-id entity)))
                                                  (alive? e)
                                                  (friendly? entity e)
                                                  (in-range? entity e 10))))])
       (> (length enemies) (+ 1 (length allies)))))

;;; ====
;;; Item Predicates
;;; ====

;;; has-item? : Entity × Symbol -> Bool
;;; Entity has an item with the given tag in inventory.
(define (has-item? entity item-tag)
  (let ([inv (entity-get-component entity 'inventory)])
       (if inv
           (let ([items (alist-ref inv 'items '())])
                (if (memq item-tag items) #t #f))
           #f)))

;;; inventory-full? : Entity -> Bool
;;; Entity's inventory is at capacity.
(define (inventory-full? entity)
  (let ([inv (entity-get-component entity 'inventory)])
       (if inv
           (let ([items (alist-ref inv 'items '())]
                 [capacity (alist-ref inv 'capacity 10)])
                (>= (length items) capacity))
           #t)))  ; No inventory = always full

;;; inventory-empty? : Entity -> Bool
(define (inventory-empty? entity)
  (let ([inv (entity-get-component entity 'inventory)])
       (if inv
           (null? (alist-ref inv 'items '()))
           #t)))

;;; ====
;;; Quick Reference
;;; ====

#|
Entity Health:
  alive?, dead?, hurt?, healthy?, wounded?, critical?, full-hp?
  
  Entity State:
  has-component?, is-actor?, is-ai?, is-blocker?, is-opaque?
  
  Position:
  adjacent?, in-range?, min-distance?, at-point?, same-position?
  distance-from, distance-from-point
  
  Faction:
  hostile?, friendly?, neutral-to?, faction-is?
  is-player?, is-monster?
  ally-nearby?, allies-in-range, enemies-in-range
  
  World:
  tile-is?, walkable?, blocked?, has-entity-at?
  
  Player:
  player-nearby?, player-adjacent-to?, player-visible-from?
  
  Combat:
  can-attack?, can-be-attacked?, stronger-than?, weaker-than?
  
  Composite:
  safe?, threatened?, outnumbered?
  
  Items:
  has-item?, inventory-full?, inventory-empty?
  |#
