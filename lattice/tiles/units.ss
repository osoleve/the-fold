(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'tiles/core 'pathfinding 'visibility)

(doc 'module 'tiles/units)
(doc 'description "Unit/entity management: creation, placement, movement, visibility")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'unit-protocol)
(doc 'note "Unit fields: id, type, team, properties")
(doc 'note "Common properties: hp, max-hp, attack, defense, movement, vision, char")

(define-record-type unit%
  (fields id type team properties))

(define (make-unit id type team properties)
  (doc 'export #t)
  (doc 'description "Create a new unit")
  (doc 'type '(-> Symbol Symbol Symbol Alist Unit))
  (make-unit% id type team properties))

(define (unit-get-prop unit prop default)
  (doc 'description "Get unit property with default")
  (doc 'type '(-> Unit Symbol Any Any))
  (let ([props (unit%-properties unit)])
       (if (assq prop props)
           (cdr (assq prop props))
           default)))

(define (unit-set-prop unit prop value)
  (doc 'description "Set unit property (returns new unit)")
  (doc 'type '(-> Unit Symbol Any Unit))
  (let* ([props (unit%-properties unit)]
         [new-props (cons (cons prop value)
                          (filter (lambda (p) (not (eq? (car p) prop)))
                                  props))])
        (make-unit% (unit%-id unit)
                    (unit%-type unit)
                    (unit%-team unit)
                    new-props)))

(doc 'section 'game-state)
(doc 'note "Game state maintains two hashtables: coord→unit and unit→coord")

(define-record-type game-state%
  (fields board coord->unit unit->coord))

(define (make-game-state board)
  (doc 'export #t)
  (doc 'description "Create initial game state with empty units")
  (doc 'type '(-> Board GameState))
  (make-game-state% board
                    (make-hashtable equal-hash equal?)
                    (make-hashtable equal-hash equal?)))

;;; game-board : GameState → Board
;;; Get the board from game state
(define (game-board gs)
  (game-state%-board gs))

(define (game-place-unit gs unit coord)
  (doc 'export #t)
  (doc 'description "Place unit at coordinate. Removes unit from old position if it exists.")
  (doc 'type '(-> GameState Unit Coord GameState))
  (let ([c->u (hashtable-copy (game-state%-coord->unit gs) #t)]
        [u->c (hashtable-copy (game-state%-unit->coord gs) #t)])
       ;; Remove unit from old position if it exists
       (let ([old-coord (hashtable-ref u->c (unit%-id unit) #f)])
            (when old-coord
                  (hashtable-delete! c->u old-coord)))
       ;; Place unit at new position
       (hashtable-set! c->u coord unit)
       (hashtable-set! u->c (unit%-id unit) coord)
       (make-game-state% (game-state%-board gs) c->u u->c)))

;;; game-remove-unit : GameState × Symbol → GameState
;;; Remove a unit by ID
(define (game-remove-unit gs unit-id)
  (let ([c->u (hashtable-copy (game-state%-coord->unit gs) #t)]
        [u->c (hashtable-copy (game-state%-unit->coord gs) #t)]
        [coord (hashtable-ref (game-state%-unit->coord gs) unit-id #f)])
       (when coord
             (hashtable-delete! c->u coord)
             (hashtable-delete! u->c unit-id))
       (make-game-state% (game-state%-board gs) c->u u->c)))

;;; game-get-unit-at : GameState × Coord → Unit | #f
;;; Get unit at coordinate
(define (game-get-unit-at gs coord)
  (doc 'export #t)
  (hashtable-ref (game-state%-coord->unit gs) coord #f))

;;; game-get-unit-coord : GameState × Symbol → Coord | #f
;;; Get coordinate of unit by ID
(define (game-get-unit-coord gs unit-id)
  (hashtable-ref (game-state%-unit->coord gs) unit-id #f))

;;; game-all-units : GameState → (List (Coord . Unit))
;;; Get all units with their positions
(define (game-all-units gs)
  (doc 'export #t)
  (let ([c->u (game-state%-coord->unit gs)])
       (map (lambda (coord)
                    (cons coord (hashtable-ref c->u coord #f)))
            (vector->list (hashtable-keys c->u)))))

;;; game-units-by-team : GameState × Symbol → (List (Coord . Unit))
;;; Get all units of a specific team
(define (game-units-by-team gs team)
  (doc 'export #t)
  (filter (lambda (entry)
                  (eq? (unit%-team (cdr entry)) team))
          (game-all-units gs)))

(doc 'section 'movement)

(define (game-move-unit gs unit-id dest neighbor-fn)
  (doc 'export #t)
  (doc 'description "Move unit to destination if reachable. Checks movement range.")
  (doc 'type '(-> GameState Symbol Coord (-> Coord (List Coord)) (Maybe GameState)))
  (doc 'returns "New game state if move is valid, #f otherwise")
  (let* ([board (game-board gs)]
         [start (game-get-unit-coord gs unit-id)]
         [unit (game-get-unit-at gs start)])
        (if (and start unit)
            (let* ([movement (unit-get-prop unit 'movement 999)]
                   [path (find-path-dijkstra board start dest neighbor-fn)])
                  (if (and path (<= (length path) (+ movement 1))) ; +1 for including start
                      (game-place-unit gs unit dest)
                      #f)) ; Path too long or doesn't exist
            #f))) ; Unit not found

(define (game-unit-can-reach gs unit-id dest neighbor-fn)
  (doc 'description "Check if unit can reach destination within movement range")
  (doc 'type '(-> GameState Symbol Coord (-> Coord (List Coord)) Bool))
  (let* ([board (game-board gs)]
         [start (game-get-unit-coord gs unit-id)]
         [unit (game-get-unit-at gs start)])
        (if (and start unit)
            (let* ([movement (unit-get-prop unit 'movement 999)]
                   [reachable (board-reachable board start movement neighbor-fn)]
                   [reachable-coords (map car reachable)])
                  (member dest reachable-coords))
            #f)))

(doc 'section 'visibility)

(define (game-visible-units gs unit-id neighbor-fn line-fn)
  (doc 'export #t)
  (doc 'description "Get all units visible to a specific unit (uses unit's vision property)")
  (doc 'type '(-> GameState Symbol (-> Coord (List Coord)) (-> Coord Coord (List Coord)) (List Unit)))
  (let* ([board (game-board gs)]
         [viewer-coord (game-get-unit-coord gs unit-id)]
         [viewer (game-get-unit-at gs viewer-coord)])
        (if (and viewer-coord viewer)
            (let* ([vision-range (unit-get-prop viewer 'vision 999)]
                   [visible-coords (board-fov board viewer-coord vision-range
                                              neighbor-fn line-fn)]
                   [all-units (game-all-units gs)])
                  (filter-map
                   (lambda (entry)
                           (let ([coord (car entry)]
                                 [unit (cdr entry)])
                                (if (and (member coord visible-coords)
                                         (not (eq? (unit%-id unit) unit-id)))
                                    unit
                                    #f)))
                   all-units))
            '())))

(define (game-units-can-see-each-other gs unit1-id unit2-id line-fn)
  (doc 'description "Check if two units can see each other (checks line of sight)")
  (doc 'type '(-> GameState Symbol Symbol (-> Coord Coord (List Coord)) Bool))
  (let ([coord1 (game-get-unit-coord gs unit1-id)]
        [coord2 (game-get-unit-coord gs unit2-id)]
        [board (game-board gs)])
       (if (and coord1 coord2)
           (board-has-los? board coord1 coord2 line-fn)
           #f)))

(doc 'section 'rendering)

(define (unit-char unit)
  (doc 'description "Get character representation of unit. Uses 'char property or first letter of type.")
  (doc 'type '(-> Unit String))
  (unit-get-prop unit 'char
                 (substring (symbol->string (unit%-type unit)) 0 1)))

(define (game-render-overlay-coords gs)
  (doc 'description "Get all unit coordinates for rendering overlay. Units override tiles.")
  (doc 'type '(-> GameState (List Coord)))
  (map car (game-all-units gs)))

;;; ====
;;; Exports Summary
;;; ====

;;; This module provides:
;;;   Unit Management:
;;;     • make-unit — Create unit with properties
;;;     • unit-get-prop, unit-set-prop — Property access
;;;
;;;   Game State:
;;;     • make-game-state — Create game with board + units
;;;     • game-place-unit — Place unit on board
;;;     • game-remove-unit — Remove unit
;;;     • game-get-unit-at — Find unit at coordinate
;;;     • game-all-units — List all units
;;;     • game-units-by-team — Filter by team
;;;
;;;   Movement:
;;;     • game-move-unit — Move unit with pathfinding
;;;     • game-unit-can-reach — Check if destination is reachable
;;;
;;;   Visibility:
;;;     • game-visible-units — Find units visible to a unit
;;;     • game-units-can-see-each-other — Check mutual LOS
;;;
;;;   Rendering:
;;;     • unit-char — Get unit's display character
;;;     • game-render-overlay-coords — Get coords for rendering overlay
