;;; playpen/boardcraft/turns.ss — Turn-Based Game System
;;;
;;; Manages turn-based gameplay including turn order, action points,
;;; phases, and turn history.
;;;
;;; Implements:
;;;   • Turn order management
;;;   • Action point system
;;;   • Turn phase tracking
;;;   • Turn history/log
;;;   • Initiative-based ordering
;;;
;;; Dependencies:
;;;   - playpen/boardcraft/core.ss
;;;   - playpen/boardcraft/units.ss

;;; ====
;;; Turn State
;;; ====

;;; A Turn State manages the current game turn.
;;;
;;; turn-state:
;;;   - turn-number: current turn number (starts at 1)
;;;   - active-unit: ID of currently active unit
;;;   - turn-order: ordered list of unit IDs
;;;   - action-points: hashtable of unit-id → remaining action points
;;;   - phase: current phase symbol ('movement, 'action, 'end)
;;;   - history: list of (turn-number . action) entries

(define-record-type turn-state%
  (fields turn-number active-unit turn-order action-points phase history))

;;; make-turn-state : (List UnitId) × Hashtable → TurnState
;;; Create initial turn state
;;;
;;; unit-order: list of unit IDs in turn order
;;; max-actions: hashtable mapping unit-id → max action points
(define (make-turn-state unit-order max-actions)
  (let ([ap-table (make-hashtable equal-hash equal?)])
       ;; Initialize action points
       (for-each
        (lambda (uid)
                (hashtable-set! ap-table uid
                                (hashtable-ref max-actions uid 2)))
        unit-order)
       (make-turn-state% 1  ; turn-number
                         (if (null? unit-order) #f (car unit-order))  ; active-unit
                         unit-order
                         ap-table
                         'movement  ; initial phase
                         '())))  ; empty history

;;; make-turn-state-from-game : GameState → TurnState
;;; Create turn state from game state with default action points
(define (make-turn-state-from-game gs)
  (let* ([units (game-all-units gs)]
         [unit-ids (map (lambda (entry) (unit%-id (cdr entry))) units)]
         [max-actions (make-hashtable equal-hash equal?)])
        ;; Default 2 action points per unit
        (for-each
         (lambda (uid)
                 (hashtable-set! max-actions uid 2))
         unit-ids)
        (make-turn-state unit-ids max-actions)))

;;; ====
;;; Turn Queries
;;; ====

;;; turn-current-unit : TurnState → UnitId | #f
;;; Get currently active unit ID
(define (turn-current-unit ts)
  (turn-state%-active-unit ts))

;;; turn-current-phase : TurnState → Symbol
;;; Get current turn phase
(define (turn-current-phase ts)
  (turn-state%-phase ts))

;;; turn-number : TurnState → Integer
;;; Get current turn number
(define (turn-number ts)
  (turn-state%-turn-number ts))

;;; turn-actions-remaining : TurnState × UnitId → Integer
;;; Get remaining action points for unit
(define (turn-actions-remaining ts unit-id)
  (hashtable-ref (turn-state%-action-points ts) unit-id 0))

;;; turn-can-act? : TurnState × UnitId → Boolean
;;; Check if unit can take actions
(define (turn-can-act? ts unit-id)
  (and (eq? (turn-current-unit ts) unit-id)
       (> (turn-actions-remaining ts unit-id) 0)))

;;; ====
;;; Turn Actions
;;; ====

;;; turn-spend-action : TurnState × UnitId × Integer → TurnState
;;; Spend action points
(define (turn-spend-action ts unit-id cost)
  (let ([ap-table (hashtable-copy (turn-state%-action-points ts) #t)]
        [current (hashtable-ref (turn-state%-action-points ts) unit-id 0)])
       (hashtable-set! ap-table unit-id (max 0 (- current cost)))
       (make-turn-state% (turn-state%-turn-number ts)
                         (turn-state%-active-unit ts)
                         (turn-state%-turn-order ts)
                         ap-table
                         (turn-state%-phase ts)
                         (turn-state%-history ts))))

;;; turn-log-action : TurnState × Symbol × Any → TurnState
;;; Add action to history
(define (turn-log-action ts action-type details)
  (let ([entry (list (turn-state%-turn-number ts)
                     (turn-state%-active-unit ts)
                     action-type
                     details)]
        [history (turn-state%-history ts)])
       (make-turn-state% (turn-state%-turn-number ts)
                         (turn-state%-active-unit ts)
                         (turn-state%-turn-order ts)
                         (turn-state%-action-points ts)
                         (turn-state%-phase ts)
                         (cons entry history))))

;;; ====
;;; Turn Advancement
;;; ====

;;; turn-next-phase : TurnState → TurnState
;;; Advance to next phase in turn
(define (turn-next-phase ts)
  (let ([current-phase (turn-state%-phase ts)])
       (make-turn-state% (turn-state%-turn-number ts)
                         (turn-state%-active-unit ts)
                         (turn-state%-turn-order ts)
                         (turn-state%-action-points ts)
                         (case current-phase
                               [(movement) 'action]
                               [(action) 'end]
                               [(end) 'movement])
                         (turn-state%-history ts))))

;;; turn-next-unit : TurnState × Hashtable → TurnState
;;; Advance to next unit in turn order
;;;
;;; max-actions: hashtable of unit-id → max action points (for refresh)
(define (turn-next-unit ts max-actions)
  (let* ([order (turn-state%-turn-order ts)]
         [current (turn-state%-active-unit ts)]
         [current-idx (let loop ([idx 0] [lst order])
                           (cond
                            [(null? lst) -1]
                            [(eq? (car lst) current) idx]
                            [else (loop (+ idx 1) (cdr lst))]))]
         [next-idx (modulo (+ current-idx 1) (length order))]
         [next-unit (list-ref order next-idx)]
         [new-turn? (= next-idx 0)]  ; Wrapped around to start
         [ap-table (hashtable-copy (turn-state%-action-points ts) #t)])
        ;; Refresh action points for next unit
        (hashtable-set! ap-table next-unit
                        (hashtable-ref max-actions next-unit 2))
        (make-turn-state% (if new-turn?
                              (+ (turn-state%-turn-number ts) 1)
                              (turn-state%-turn-number ts))
                          next-unit
                          order
                          ap-table
                          'movement  ; Reset to movement phase
                          (turn-state%-history ts))))

;;; turn-end-turn : TurnState × Hashtable → TurnState
;;; End current unit's turn and advance to next
(define (turn-end-turn ts max-actions)
  (turn-next-unit ts max-actions))

;;; ====
;;; Turn History
;;; ====

;;; turn-history : TurnState → (List Entry)
;;; Get turn history (most recent first)
(define (turn-history ts)
  (turn-state%-history ts))

;;; turn-recent-actions : TurnState × Integer → (List Entry)
;;; Get N most recent actions
(define (turn-recent-actions ts n)
  (let loop ([actions (turn-state%-history ts)]
             [count 0]
             [result '()])
       (if (or (null? actions) (>= count n))
           (reverse result)
           (loop (cdr actions) (+ count 1) (cons (car actions) result)))))

;;; ====
;;; Initiative System
;;; ====

;;; calculate-initiative-order : (List Unit) → (List UnitId)
;;; Calculate turn order based on unit initiative
;;;
;;; Units with higher 'initiative property go first.
;;; Ties are broken by unit ID.
(define (calculate-initiative-order units)
  (let ([sorted
         (sort (lambda (u1 u2)
                       (let ([init1 (unit-get-prop u1 'initiative 0)]
                             [init2 (unit-get-prop u2 'initiative 0)])
                            (if (= init1 init2)
                                (string<? (symbol->string (unit%-id u1))
                                          (symbol->string (unit%-id u2)))
                                (> init1 init2))))
               units)])
       (map unit%-id sorted)))

;;; make-turn-state-with-initiative : GameState → TurnState
;;; Create turn state with initiative-based order
(define (make-turn-state-with-initiative gs)
  (let* ([units-with-coords (game-all-units gs)]
         [units (map cdr units-with-coords)]
         [initiative-order (calculate-initiative-order units)]
         [max-actions (make-hashtable equal-hash equal?)])
        ;; Set max actions based on unit properties
        (for-each
         (lambda (unit)
                 (let ([actions (unit-get-prop unit 'actions-per-turn 2)])
                      (hashtable-set! max-actions (unit%-id unit) actions)))
         units)
        (make-turn-state initiative-order max-actions)))

;;; ====
;;; Integrated Turn System
;;; ====

;;; game-with-turns : GameState × TurnState → GameWithTurns
;;; Combine game state with turn state
(define-record-type game-with-turns%
  (fields game turns))

;;; make-game-with-turns : GameState → GameWithTurns
;;; Create game with turn system
(define (make-game-with-turns gs)
  (make-game-with-turns% gs (make-turn-state-from-game gs)))

;;; game-execute-action : GameWithTurns × Symbol × Procedure × Integer → GameWithTurns | #f
;;; Execute an action if unit can act
;;;
;;; Parameters:
;;;   gwt: game with turns
;;;   action-type: symbol describing action (e.g., 'move, 'attack)
;;;   action-fn: (GameState → GameState) - function to execute
;;;   cost: action point cost
;;;
;;; Returns: new GameWithTurns if action succeeds, #f otherwise
(define (game-execute-action gwt action-type action-fn cost)
  (let* ([ts (game-with-turns%-turns gwt)]
         [gs (game-with-turns%-game gwt)]
         [active (turn-current-unit ts)])
        (if (and active (turn-can-act? ts active))
            (let ([new-gs (action-fn gs)])
                 (if new-gs
                     (let* ([new-ts (turn-spend-action ts active cost)]
                            [new-ts-with-log (turn-log-action new-ts action-type
                                                              (turn-number new-ts))])
                           (make-game-with-turns% new-gs new-ts-with-log))
                     #f))  ; Action failed
            #f)))  ; Can't act

;;; ====
;;; Exports Summary
;;; ====

;;; This module provides:
;;;   Turn State:
;;;     • make-turn-state — Create turn state with order and action points
;;;     • make-turn-state-from-game — Auto-create from game state
;;;     • turn-current-unit — Get active unit
;;;     • turn-actions-remaining — Check action points
;;;
;;;   Turn Actions:
;;;     • turn-spend-action — Spend action points
;;;     • turn-log-action — Add to history
;;;     • turn-can-act? — Check if unit can act
;;;
;;;   Turn Advancement:
;;;     • turn-next-phase — Advance phase
;;;     • turn-next-unit — Next unit's turn
;;;     • turn-end-turn — End turn and advance
;;;
;;;   Initiative:
;;;     • calculate-initiative-order — Order by initiative stat
;;;     • make-turn-state-with-initiative — Auto-order by initiative
;;;
;;;   Integration:
;;;     • make-game-with-turns — Combine game + turn system
;;;     • game-execute-action — Execute action with AP cost
