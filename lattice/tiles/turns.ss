(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @requires sort dict tiles/units
(require 'sort 'dict 'tiles/units)

(doc 'module 'tiles/turns)
(doc 'description "Turn-based game system: turn order, action points, phases, history")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'turn-state)
(doc 'note "Turn state fields: turn-number, active-unit, turn-order, action-points, phase, history")

(define-record-type turn-state%
  (fields turn-number active-unit turn-order action-points phase history))

(define (make-turn-state unit-order max-actions)
  (doc 'export #t)
  (doc 'description "Create initial turn state")
  (doc 'type '(-> (List UnitId) Dict TurnState))
  (doc 'param 'unit-order "List of unit IDs in turn order")
  (doc 'param 'max-actions "Dict mapping unit-id → max action points")
  (let ([ap-table (fold-left
                    (lambda (acc uid)
                      (dict-assoc uid (or (dict-lookup uid max-actions) 2) acc))
                    dict-empty
                    unit-order)])
    (make-turn-state% 1
                      (if (null? unit-order) #f (car unit-order))
                      unit-order
                      ap-table
                      'movement
                      '())))

(define (make-turn-state-from-game gs)
  (doc 'description "Create turn state from game state with default 2 action points per unit")
  (doc 'type '(-> GameState TurnState))
  (let* ([units (game-all-units gs)]
         [unit-ids (map (lambda (entry) (unit%-id (cdr entry))) units)]
         [max-actions (fold-left
                        (lambda (acc uid) (dict-assoc uid 2 acc))
                        dict-empty
                        unit-ids)])
    (make-turn-state unit-ids max-actions)))

(doc 'section 'queries)

(define (turn-current-unit ts)
  (doc 'export #t)
  (doc 'description "Get currently active unit ID")
  (doc 'type '(-> TurnState (Maybe UnitId)))
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
  (or (dict-lookup unit-id (turn-state%-action-points ts)) 0))

;;; turn-can-act? : TurnState × UnitId → Boolean
;;; Check if unit can take actions
(define (turn-can-act? ts unit-id)
  (and (eq? (turn-current-unit ts) unit-id)
       (> (turn-actions-remaining ts unit-id) 0)))

(doc 'section 'actions)

(define (turn-spend-action ts unit-id cost)
  (doc 'export #t)
  (doc 'description "Spend action points")
  (doc 'type '(-> TurnState UnitId Integer TurnState))
  (let* ([current (turn-actions-remaining ts unit-id)]
         [ap-table (dict-assoc unit-id (max 0 (- current cost))
                               (turn-state%-action-points ts))])
    (make-turn-state% (turn-state%-turn-number ts)
                      (turn-state%-active-unit ts)
                      (turn-state%-turn-order ts)
                      ap-table
                      (turn-state%-phase ts)
                      (turn-state%-history ts))))

(define (turn-log-action ts action-type details)
  (doc 'description "Add action to history")
  (doc 'type '(-> TurnState Symbol Any TurnState))
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

(doc 'section 'advancement)

(define (turn-next-phase ts)
  (doc 'description "Advance to next phase in turn (movement → action → end → movement)")
  (doc 'type '(-> TurnState TurnState))
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

(define (turn-next-unit ts max-actions)
  (doc 'export #t)
  (doc 'description "Advance to next unit in turn order. Refreshes action points for next unit.")
  (doc 'type '(-> TurnState Dict TurnState))
  (doc 'param 'max-actions "Dict of unit-id → max action points for refresh")
  (let* ([order (turn-state%-turn-order ts)]
         [current (turn-state%-active-unit ts)]
         [current-idx (let loop ([idx 0] [lst order])
                           (cond
                            [(null? lst) -1]
                            [(eq? (car lst) current) idx]
                            [else (loop (+ idx 1) (cdr lst))]))]
         [next-idx (modulo (+ current-idx 1) (length order))]
         [next-unit (list-ref order next-idx)]
         [new-turn? (= next-idx 0)]
         [ap-table (dict-assoc next-unit
                               (or (dict-lookup next-unit max-actions) 2)
                               (turn-state%-action-points ts))])
    (make-turn-state% (if new-turn?
                          (+ (turn-state%-turn-number ts) 1)
                          (turn-state%-turn-number ts))
                      next-unit
                      order
                      ap-table
                      'movement
                      (turn-state%-history ts))))

;;; turn-end-turn : TurnState × Hashtable → TurnState
;;; End current unit's turn and advance to next
(define (turn-end-turn ts max-actions)
  (doc 'export #t)
  (turn-next-unit ts max-actions))

(doc 'section 'history)

(define (turn-history ts)
  (doc 'description "Get turn history (most recent first)")
  (doc 'type '(-> TurnState (List Entry)))
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

(doc 'section 'initiative)

(define (calculate-initiative-order units)
  (doc 'export #t)
  (doc 'description "Calculate turn order based on unit initiative. Higher initiative goes first, ties broken by ID.")
  (doc 'type '(-> (List Unit) (List UnitId)))
  (let ([sorted
         (sort-by (lambda (u1 u2)
                       (let ([init1 (unit-get-prop u1 'initiative 0)]
                             [init2 (unit-get-prop u2 'initiative 0)])
                            (if (= init1 init2)
                                (string<? (symbol->string (unit%-id u1))
                                          (symbol->string (unit%-id u2)))
                                (> init1 init2))))
               units)])
       (map unit%-id sorted)))

(define (make-turn-state-with-initiative gs)
  (doc 'description "Create turn state with initiative-based order")
  (doc 'type '(-> GameState TurnState))
  (let* ([units-with-coords (game-all-units gs)]
         [units (map cdr units-with-coords)]
         [initiative-order (calculate-initiative-order units)]
         [max-actions (fold-left
                        (lambda (acc unit)
                          (dict-assoc (unit%-id unit)
                                      (unit-get-prop unit 'actions-per-turn 2)
                                      acc))
                        dict-empty
                        units)])
    (make-turn-state initiative-order max-actions)))

(doc 'section 'integration)

(define-record-type game-with-turns%
  (fields game turns))

(doc game-with-turns% 'description "Combines game state with turn state")

(define (make-game-with-turns gs)
  (doc 'description "Create game with turn system")
  (doc 'type '(-> GameState GameWithTurns))
  (make-game-with-turns% gs (make-turn-state-from-game gs)))

(define (game-execute-action gwt action-type action-fn cost)
  (doc 'export #t)
  (doc 'description "Execute action if unit can act")
  (doc 'type '(-> GameWithTurns Symbol (-> GameState GameState) Integer (Maybe GameWithTurns)))
  (doc 'param 'action-type "Symbol describing action (e.g., 'move, 'attack)")
  (doc 'param 'action-fn "Function to execute: GameState → GameState")
  (doc 'param 'cost "Action point cost")
  (doc 'returns "New GameWithTurns if action succeeds, #f otherwise")
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
