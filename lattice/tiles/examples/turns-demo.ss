;;; lattice/tiles/examples/turns-demo.ss — Turn System Demo
;;;
;;; Demonstrates turn-based gameplay including turn order,
;;; action points, and complete game loop.
;;;
;;; Usage:
;;;   scheme --script lattice/tiles/examples/turns-demo.ss

(load "lattice/tiles/boardcraft.ss")

(display "===============================================================\n")
(display "  BOARDCRAFT TURN SYSTEM DEMO\n")
(display "===============================================================\n")
(newline)

;;; ====
;;; Example 1: Basic Turn Order
;;; ====

(display "Example 1: Basic Turn Order\n")
(display "----------------------------------------------------------\n")

;;; Create board and game
(define floor (make-tile 'floor '((walkable . #t))))
(define board (make-square-board 10 8 floor))
(define game (make-game-state board))

;;; Create units
(define unit1 (make-unit 'u1 'warrior 'player
                         '((actions-per-turn . 2))))
(define unit2 (make-unit 'u2 'archer 'player
                         '((actions-per-turn . 2))))
(define unit3 (make-unit 'u3 'mage 'player
                         '((actions-per-turn . 3))))

;;; Place units
(define game (game-place-unit game unit1 (square-coord 2 4)))
(define game (game-place-unit game unit2 (square-coord 4 4)))
(define game (game-place-unit game unit3 (square-coord 6 4)))

;;; Create turn state
(define turn-order '(u1 u2 u3))
(define max-actions
  (dict-assoc 'u1 2
    (dict-assoc 'u2 2
      (dict-assoc 'u3 3 dict-empty))))

(define turns (make-turn-state turn-order max-actions))

(display "Turn order: u1 → u2 → u3\n")
(display "Current turn: ")
(display (turn-number turns))
(newline)
(display "Active unit: ")
(display (turn-current-unit turns))
(newline)
(display "Action points remaining: ")
(display (turn-actions-remaining turns 'u1))
(newline)
(newline)

;;; ====
;;; Example 2: Spending Action Points
;;; ====

(display "Example 2: Spending Action Points\n")
(display "----------------------------------------------------------\n")

(display "Warrior (u1) starts with ")
(display (turn-actions-remaining turns 'u1))
(display " action points\n")

;;; Spend 1 action point for movement
(define turns (turn-spend-action turns 'u1 1))
(display "After moving (cost 1): ")
(display (turn-actions-remaining turns 'u1))
(display " AP remaining\n")

;;; Spend another for attack
(define turns (turn-spend-action turns 'u1 1))
(display "After attacking (cost 1): ")
(display (turn-actions-remaining turns 'u1))
(display " AP remaining\n")

(display "Can u1 still act? ")
(display (if (turn-can-act? turns 'u1) "YES" "NO"))
(newline)
(newline)

;;; ====
;;; Example 3: Advancing Turns
;;; ====

(display "Example 3: Advancing Turns\n")
(display "----------------------------------------------------------\n")

(display "Current unit: ")
(display (turn-current-unit turns))
(newline)

;;; End turn and advance to next unit
(define turns (turn-end-turn turns max-actions))

(display "After ending turn:\n")
(display "  Turn number: ")
(display (turn-number turns))
(newline)
(display "  Active unit: ")
(display (turn-current-unit turns))
(newline)
(display "  Action points: ")
(display (turn-actions-remaining turns (turn-current-unit turns)))
(newline)
(newline)

;;; ====
;;; Example 4: Turn Phases
;;; ====

(display "Example 4: Turn Phases\n")
(display "----------------------------------------------------------\n")

(display "Current phase: ")
(display (turn-current-phase turns))
(newline)

(define turns (turn-next-phase turns))
(display "After next-phase: ")
(display (turn-current-phase turns))
(newline)

(define turns (turn-next-phase turns))
(display "After next-phase: ")
(display (turn-current-phase turns))
(newline)

(define turns (turn-next-phase turns))
(display "After next-phase (wraps): ")
(display (turn-current-phase turns))
(newline)
(newline)

;;; ====
;;; Example 5: Turn History
;;; ====

(display "Example 5: Turn History and Action Log\n")
(display "----------------------------------------------------------\n")

;;; Log some actions
(define turns (turn-log-action turns 'move "Moved to (5,4)"))
(define turns (turn-log-action turns 'attack "Attacked enemy"))
(define turns (turn-log-action turns 'defend "Raised shield"))

(display "Recent actions (last 3):\n")
(for-each
 (lambda (entry)
         (display "  Turn ")
         (display (car entry))
         (display ", Unit ")
         (display (cadr entry))
         (display ": ")
         (display (caddr entry))
         (display " - ")
         (display (cadddr entry))
         (newline))
 (turn-recent-actions turns 3))
(newline)

;;; ====
;;; Example 6: Initiative-Based Turn Order
;;; ====

(display "Example 6: Initiative-Based Turn Order\n")
(display "----------------------------------------------------------\n")

;;; Create units with initiative stats
(define fast-unit (make-unit 'fast 'rogue 'player
                             '((initiative . 15))))
(define normal-unit (make-unit 'normal 'warrior 'player
                               '((initiative . 10))))
(define slow-unit (make-unit 'slow 'tank 'player
                             '((initiative . 5))))

(define init-order (calculate-initiative-order
                    (list slow-unit normal-unit fast-unit)))

(display "Units with initiative:\n")
(display "  Rogue (initiative 15)\n")
(display "  Warrior (initiative 10)\n")
(display "  Tank (initiative 5)\n")
(newline)

(display "Calculated turn order: ")
(for-each (lambda (id) (display id) (display " ")) init-order)
(newline)
(display "(faster units go first)\n")
(newline)

;;; ====
;;; Example 7: Complete Game Loop
;;; ====

(display "Example 7: Complete Game Loop with Actions\n")
(display "----------------------------------------------------------\n")

;;; Create fresh game with turns
(define game-board (make-square-board 8 6 floor))
(define gs (make-game-state game-board))

(define hero (make-unit 'hero 'knight 'player
                        '((movement . 3)
                          (actions-per-turn . 2))))

(define gs (game-place-unit gs hero (square-coord 1 3)))

(define gwt (make-game-with-turns gs))

(display "Game with turns created\n")
(display "Turn ")
(display (turn-number (game-with-turns%-turns gwt)))
(display ", active unit: ")
(display (turn-current-unit (game-with-turns%-turns gwt)))
(display ", AP: ")
(display (turn-actions-remaining (game-with-turns%-turns gwt) 'hero))
(newline)

;;; Note: Full move action integration would be demonstrated here
;;; The turn system is ready for game actions with AP costs
(display "\nTurn system supports:\n")
(display "  - Action execution with AP costs\n")
(display "  - Action history logging\n")
(display "  - Integration with pathfinding and movement\n")
(newline)

;;; ====
;;; Summary
;;; ====

(display "===============================================================\n")
(display "  Demo Complete!\n")
(display "===============================================================\n")
(newline)
(display "Turn system features:\n")
(display "  ✓ Turn order management\n")
(display "  ✓ Action point system\n")
(display "  ✓ Turn phase tracking (movement, action, end)\n")
(display "  ✓ Turn history and action log\n")
(display "  ✓ Initiative-based ordering\n")
(display "  ✓ Integrated game loop (game + turns)\n")
(display "  ✓ Action execution with AP costs\n")
(newline)
(display "Complete game loop:\n")
(display "  1. Create board and place units\n")
(display "  2. Initialize turn system\n")
(display "  3. Each turn: unit performs actions (costs AP)\n")
(display "  4. End turn → next unit → repeat\n")
(newline)
(display "Use cases:\n")
(display "  • Turn-based strategy games\n")
(display "  • Tactical RPGs\n")
(display "  • Board games (chess, checkers, etc.)\n")
(display "  • Roguelikes with turn economy\n")
(newline)
