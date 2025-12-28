;;; playpen/boardcraft/units.ss — Unit/Entity Management
;;;
;;; Manages units (entities, actors, pieces) on game boards.
;;; Units have positions, properties, and can move and interact.
;;;
;;; Implements:
;;;   • Unit creation and properties
;;;   • Unit placement and tracking
;;;   • Unit movement with pathfinding
;;;   • Unit visibility queries
;;;   • Rendering integration
;;;
;;; Dependencies:
;;;   - playpen/boardcraft/core.ss
;;;   - playpen/boardcraft/pathfinding.ss
;;;   - playpen/boardcraft/visibility.ss

;;; ============================================================
;;; Unit Protocol
;;; ============================================================

;;; A Unit represents an entity on the board.
;;; Units are records with:
;;;   - id: unique identifier
;;;   - type: unit type symbol (e.g., 'warrior, 'mage, 'king)
;;;   - team: team identifier (e.g., 'player, 'enemy, 'neutral)
;;;   - properties: association list of properties
;;;
;;; Common properties:
;;;   - hp: hit points
;;;   - max-hp: maximum hit points
;;;   - attack: attack power
;;;   - defense: defense rating
;;;   - movement: movement range
;;;   - vision: vision range
;;;   - char: character for rendering

(define-record-type unit%
  (fields id type team properties))

;;; make-unit : Symbol × Symbol × Symbol × Alist → Unit
;;; Create a new unit
(define (make-unit id type team properties)
  (make-unit% id type team properties))

;;; unit-get-prop : Unit × Symbol × Any → Any
;;; Get unit property with default
(define (unit-get-prop unit prop default)
  (let ([props (unit%-properties unit)])
    (if (assq prop props)
        (cdr (assq prop props))
        default)))

;;; unit-set-prop : Unit × Symbol × Any → Unit
;;; Set unit property (returns new unit)
(define (unit-set-prop unit prop value)
  (let* ([props (unit%-properties unit)]
         [new-props (cons (cons prop value)
                         (filter (lambda (p) (not (eq? (car p) prop)))
                                props))])
    (make-unit% (unit%-id unit)
               (unit%-type unit)
               (unit%-team unit)
               new-props)))

;;; ============================================================
;;; Game State (Board + Units)
;;; ============================================================

;;; A Game State manages both the board and units on it.
;;;
;;; game-state : Board × (Hashtable Coord → Unit) × (Hashtable UnitId → Coord)
;;;
;;; We maintain two hashtables:
;;;   - coord->unit: maps coordinates to units at that position
;;;   - unit->coord: maps unit IDs to their coordinates

(define-record-type game-state%
  (fields board coord->unit unit->coord))

;;; make-game-state : Board → GameState
;;; Create initial game state with empty units
(define (make-game-state board)
  (make-game-state% board
                   (make-hashtable equal-hash equal?)
                   (make-hashtable equal-hash equal?)))

;;; game-board : GameState → Board
;;; Get the board from game state
(define (game-board gs)
  (game-state%-board gs))

;;; game-place-unit : GameState × Unit × Coord → GameState
;;; Place a unit at a coordinate
(define (game-place-unit gs unit coord)
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
  (hashtable-ref (game-state%-coord->unit gs) coord #f))

;;; game-get-unit-coord : GameState × Symbol → Coord | #f
;;; Get coordinate of unit by ID
(define (game-get-unit-coord gs unit-id)
  (hashtable-ref (game-state%-unit->coord gs) unit-id #f))

;;; game-all-units : GameState → (List (Coord . Unit))
;;; Get all units with their positions
(define (game-all-units gs)
  (let ([c->u (game-state%-coord->unit gs)])
    (map (lambda (coord)
          (cons coord (hashtable-ref c->u coord #f)))
         (vector->list (hashtable-keys c->u)))))

;;; game-units-by-team : GameState × Symbol → (List (Coord . Unit))
;;; Get all units of a specific team
(define (game-units-by-team gs team)
  (filter (lambda (entry)
           (eq? (unit%-team (cdr entry)) team))
         (game-all-units gs)))

;;; ============================================================
;;; Unit Movement
;;; ============================================================

;;; game-move-unit : GameState × Symbol × Coord × (Coord → List Coord) → GameState | #f
;;; Move unit to destination if reachable
;;;
;;; Returns new game state if move is valid, #f otherwise
(define (game-move-unit gs unit-id dest neighbor-fn)
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

;;; game-unit-can-reach : GameState × Symbol × Coord × (Coord → List Coord) → Boolean
;;; Check if unit can reach destination
(define (game-unit-can-reach gs unit-id dest neighbor-fn)
  (let* ([board (game-board gs)]
         [start (game-get-unit-coord gs unit-id)]
         [unit (game-get-unit-at gs start)])
    (if (and start unit)
        (let* ([movement (unit-get-prop unit 'movement 999)]
               [reachable (board-reachable board start movement neighbor-fn)]
               [reachable-coords (map car reachable)])
          (member dest reachable-coords))
        #f)))

;;; ============================================================
;;; Unit Visibility
;;; ============================================================

;;; game-visible-units : GameState × Symbol × (Coord → List Coord) × (Coord × Coord → List Coord) → (List Unit)
;;; Get all units visible to a specific unit
(define (game-visible-units gs unit-id neighbor-fn line-fn)
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

;;; game-units-can-see-each-other : GameState × Symbol × Symbol × (Coord × Coord → List Coord) → Boolean
;;; Check if two units can see each other
(define (game-units-can-see-each-other gs unit1-id unit2-id line-fn)
  (let ([coord1 (game-get-unit-coord gs unit1-id)]
        [coord2 (game-get-unit-coord gs unit2-id)]
        [board (game-board gs)])
    (if (and coord1 coord2)
        (board-has-los? board coord1 coord2 line-fn)
        #f)))

;;; ============================================================
;;; Rendering Integration
;;; ============================================================

;;; unit-char : Unit → String
;;; Get character representation of unit
(define (unit-char unit)
  (unit-get-prop unit 'char
                (substring (symbol->string (unit%-type unit)) 0 1)))

;;; game-render-with-units : GameState × StyleFn → String
;;; Render board with units shown
;;;
;;; This is a helper for rendering. Units override tiles.
(define (game-render-overlay-coords gs)
  (map car (game-all-units gs)))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

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
