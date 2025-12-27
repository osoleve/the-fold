;;; playpen/loom/spell/prelude.ss — Spell DSL Prelude
;;;
;;; Loads all Spell DSL modules in the correct order.
;;; This is an internal file; users should load spell.ss instead.
;;;
;;; This is part of Spell: the DSL for weaving game logic.

;;; ============================================================
;;; Load Order
;;; ============================================================

;;; Load pipe macros first (foundation for everything else)
(load "playpen/loom/spell/pipe.ss")

;;; Load component definition macro
(load "playpen/loom/spell/component.ss")

;;; Load entity definition macro (depends on pipe)
(load "playpen/loom/spell/entity.ss")

;;; Load event handler macros
(load "playpen/loom/spell/event.ss")


;;; ============================================================
;;; Common Utility Functions
;;; ============================================================

;;; These helper functions are commonly used with the DSL.

;;; entity-hp-percent : Entity -> Rational
;;; Get entity's HP as a percentage (0-100).
(define (entity-hp-percent entity)
  (let ([stats (entity-stats entity)])
    (if stats
        (let ([hp (stats-hp stats)]
              [max-hp (stats-max-hp stats)])
          (if (> max-hp 0)
              (* 100 (/ hp max-hp))
              100))
        100)))

;;; entity-mana-percent : Entity -> Rational
;;; Get entity's mana as a percentage (if it has a mana component).
(define (entity-mana-percent entity)
  (let ([mana (entity-get-component entity 'mana)])
    (if mana
        (let ([current (alist-ref mana 'current 0)]
              [maximum (alist-ref mana 'maximum 100)])
          (if (> maximum 0)
              (* 100 (/ current maximum))
              100))
        100)))

;;; player-in-range? : World × Entity × Nat -> Bool
;;; Check if the player is within range of an entity.
(define (player-in-range? world entity range)
  (let ([player (world-get-player world)])
    (if player
        (<= (entity-distance entity player) range)
        #f)))

;;; player-adjacent? : World × Entity -> Bool
;;; Check if player is adjacent (within 1 tile) of entity.
(define (player-adjacent? world entity)
  (player-in-range? world entity 1))

;;; entities-in-range : World × Point × Nat -> (List Entity)
;;; Get all entities within range of a point.
(define (entities-in-range world point range)
  (filter
    (lambda (e)
      (let ([pos (entity-position e)])
        (and pos
             (<= (manhattan-distance (position->point pos) point) range))))
    (world-entities world)))

;;; nearest-enemy : World × Entity -> Entity | #f
;;; Find the nearest hostile entity, or #f if none.
(define (nearest-enemy world entity)
  (let* ([my-faction (entity-faction entity)]
         [enemies (filter
                    (lambda (e)
                      (and (not (eq? (entity-id e) (entity-id entity)))
                           (entity-has-component? e 'faction)
                           (faction-is-hostile?
                             (entity-get-component e 'faction)
                             my-faction)))
                    (world-entities world))])
    (if (null? enemies)
        #f
        (fold-left
          (lambda (nearest e)
            (if (< (entity-distance entity e)
                   (entity-distance entity nearest))
                e
                nearest))
          (car enemies)
          (cdr enemies)))))


;;; ============================================================
;;; Entity Component Helpers
;;; ============================================================

;;; entity-set-component : Entity × Symbol × Component -> Entity
;;; Replace a component by type (wrapper around entity-add-component).
;;; The new component's type must match the type-symbol.
(define (entity-set-component entity type-symbol new-component)
  (entity-add-component entity new-component))

;;; entity-modify-component : Entity × Symbol × (Component -> Component) -> Entity
;;; Apply a transformation function to a component.
;;; Returns entity unchanged if component doesn't exist.
(define (entity-modify-component entity type-symbol transform-fn)
  (let ([comp (entity-get-component entity type-symbol)])
    (if comp
        (entity-add-component entity (transform-fn comp))
        entity)))


;;; ============================================================
;;; Result Helpers (ok/error pattern)
;;; ============================================================

;;; ok? : Result -> Bool
(define (ok? result)
  (and (pair? result) (eq? (car result) 'ok)))

;;; error? : Result -> Bool
(define (error? result)
  (and (pair? result) (eq? (car result) 'error)))

;;; ok-value : Result -> Any
(define (ok-value result)
  (if (ok? result) (cadr result) #f))

;;; error-type : Result -> Symbol
(define (error-type result)
  (if (error? result) (cadr result) #f))

;;; error-message : Result -> String
(define (error-message result)
  (if (and (error? result) (>= (length result) 3))
      (caddr result)
      ""))
