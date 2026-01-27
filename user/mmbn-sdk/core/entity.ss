;;; user/mmbn-sdk/core/entity.ss — Entity Protocol for MMBN-style games
;;; @module mmbn/entity
;;; @requires protocol
;;; Load via: (load "user/mmbn-sdk/mmbn.ss")

;; Dependencies loaded by mmbn.ss

(doc 'module 'mmbn/entity)
(doc 'description "Entity protocol for MMBN-style games. Entities are objects
that exist on the battle grid or in the Net world - navis, viruses, obstacles,
chips, programs. All entities share a common protocol for position, rendering,
and updates.")
(doc 'layer 'user)
(doc 'purity 'partial)

;;; ============================================================
;;; Entity Protocol
;;; ============================================================

(doc 'section 'entity-protocol)

;; Core protocols - every entity must implement these
(define-protocol (entity-pos e) "Get entity position as (col . row)")
(define-protocol (entity-set-pos e pos) "Return entity with new position")
(define-protocol (entity-sprite e) "Get current sprite for rendering")

;; Optional protocols with defaults
(define-protocol/default (entity-hp e) (lambda (x) #f))
(define-protocol/default (entity-set-hp e hp) (lambda (e h) e))
(define-protocol/default (entity-team e) (lambda (x) 'neutral))
(define-protocol/default (entity-solid? e) (lambda (x) #f))
(define-protocol/default (entity-update e dt world) (lambda (e d w) e))
(define-protocol/default (entity-on-hit e damage source) (lambda (e d s) e))
(define-protocol/default (entity-dead? e) (lambda (e)
  ;; Default: dead if has HP and HP <= 0
  (let ([hp (entity-hp e)])
    (and hp (<= hp 0)))))
(define-protocol/default (entity-events e) (lambda (x) '()))  ; Event accumulator

;;; ============================================================
;;; Entity Constructors
;;; ============================================================

(doc 'section 'constructors)

(define (make-entity type pos sprite . props)
  (doc 'type '(-> Symbol Pos Sprite Props... Entity))
  (doc 'description "Generic entity constructor with property list")
  (doc 'export #t)
  (list type pos sprite props))

(define (entity-type e)
  (doc 'type '(-> Entity Symbol))
  (car e))

(define (entity-props e)
  (doc 'type '(-> Entity Alist))
  (cadddr e))

(define (entity-get e prop default)
  (doc 'type '(-> Entity Symbol Any Any))
  (let ([pair (assq prop (entity-props e))])
    (if pair (cdr pair) default)))

(define (entity-with-props e new-props)
  (doc 'type '(-> Entity Alist Entity))
  (list (entity-type e) (cadr e) (caddr e) new-props))

;;; ============================================================
;;; Generic Entity Implementation
;;; ============================================================

(doc 'section 'generic-impl)

;; Default implementations for 'entity type tag
(implement-protocol! 'entity-pos 'entity
  (lambda (e) (cadr e)))

(implement-protocol! 'entity-set-pos 'entity
  (lambda (e pos) (list (car e) pos (caddr e) (cadddr e))))

(implement-protocol! 'entity-sprite 'entity
  (lambda (e) (caddr e)))

(implement-protocol! 'entity-hp 'entity
  (lambda (e) (entity-get e 'hp #f)))

(implement-protocol! 'entity-set-hp 'entity
  (lambda (e hp)
    (let ([props (entity-props e)]
          [new-props (cons (cons 'hp hp)
                           (remp (lambda (p) (eq? (car p) 'hp)) props))])
      (entity-with-props e new-props))))

(implement-protocol! 'entity-team 'entity
  (lambda (e) (entity-get e 'team 'neutral)))

(implement-protocol! 'entity-solid? 'entity
  (lambda (e) (entity-get e 'solid #f)))

;;; ============================================================
;;; Position Utilities
;;; ============================================================

(doc 'section 'position)

(define (pos col row)
  (doc 'type '(-> Int Int Pos))
  (doc 'description "Create a grid position")
  (doc 'export #t)
  (cons col row))

(define (pos-col p)
  (doc 'type '(-> Pos Int))
  (car p))

(define (pos-row p)
  (doc 'type '(-> Pos Int))
  (cdr p))

(define (pos-add p1 p2)
  (doc 'type '(-> Pos Pos Pos))
  (pos (+ (pos-col p1) (pos-col p2))
       (+ (pos-row p1) (pos-row p2))))

(define (pos-equal? p1 p2)
  (doc 'type '(-> Pos Pos Bool))
  (and (= (pos-col p1) (pos-col p2))
       (= (pos-row p1) (pos-row p2))))

(define (pos-valid? p cols rows)
  (doc 'type '(-> Pos Int Int Bool))
  (doc 'description "Check if position is within grid bounds")
  (and (>= (pos-col p) 0) (< (pos-col p) cols)
       (>= (pos-row p) 0) (< (pos-row p) rows)))

;;; ============================================================
;;; Movement Deltas
;;; ============================================================

(doc 'section 'movement)

(define dir-up    (pos 0 -1))
(define dir-down  (pos 0 1))
(define dir-left  (pos -1 0))
(define dir-right (pos 1 0))

(define (entity-move e direction)
  (doc 'type '(-> Entity Pos Entity))
  (doc 'description "Move entity by direction delta")
  (doc 'export #t)
  (entity-set-pos e (pos-add (entity-pos e) direction)))

;;; ============================================================
;;; Entity List Operations
;;; ============================================================

(doc 'section 'entity-list)

(define (entities-at entities pos)
  (doc 'type '(-> (List Entity) Pos (List Entity)))
  (doc 'description "Find all entities at a position")
  (doc 'export #t)
  (filter (lambda (e) (pos-equal? (entity-pos e) pos)) entities))

(define (entity-at entities pos)
  (doc 'type '(-> (List Entity) Pos (Maybe Entity)))
  (doc 'description "Find first entity at position")
  (doc 'export #t)
  (let ([found (entities-at entities pos)])
    (if (null? found) #f (car found))))

(define (entities-of-team entities team)
  (doc 'type '(-> (List Entity) Symbol (List Entity)))
  (filter (lambda (e) (eq? (entity-team e) team)) entities))

(define (solid-at? entities pos)
  (doc 'type '(-> (List Entity) Pos Bool))
  (doc 'description "Check if there's a solid entity at position")
  (any? (lambda (e) (and (pos-equal? (entity-pos e) pos)
                         (entity-solid? e)))
        entities))

;;; ============================================================
;;; Entity Update
;;; ============================================================

(doc 'section 'update)

(define (update-entities entities dt world)
  (doc 'type '(-> (List Entity) Number World (List Entity)))
  (doc 'description "Update all entities, removing dead ones")
  (doc 'export #t)
  (filter-map
   (lambda (e)
     (let ([updated (entity-update e dt world)])
       (if (entity-dead? updated)
           #f  ; Remove dead entities
           updated)))
   entities))

(define (collect-entity-events entities)
  (doc 'type '(-> (List Entity) (List Event)))
  (doc 'description "Gather events from all entities (fired shots, spawns, etc.)")
  (doc 'export #t)
  (apply append (map entity-events entities)))

(display "mmbn/entity.ss loaded.\n")
(display "  (make-entity type pos sprite . props) — Create entity\n")
(display "  (entity-pos e), (entity-sprite e)    — Access properties\n")
(display "  (entity-move e direction)            — Move entity\n")
(display "  (entities-at entities pos)           — Find by position\n")
