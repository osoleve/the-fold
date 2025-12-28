;;; playpen/boardcraft/examples/units-demo.ss — Unit Management Demo
;;;
;;; Demonstrates unit/entity management including placement,
;;; movement, visibility, and integration with pathfinding.
;;;
;;; Usage:
;;;   scheme --script playpen/boardcraft/examples/units-demo.ss

(load "playpen/boardcraft/boardcraft.ss")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  BOARDCRAFT UNIT MANAGEMENT DEMO\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

;;; ============================================================
;;; Example 1: Creating and Placing Units
;;; ============================================================

(display "Example 1: Creating and Placing Units\n")
(display "──────────────────────────────────────────────────────────\n")

;;; Create a board
(define floor (make-tile 'floor '((walkable . #t))))
(define wall (make-tile 'wall '((walkable . #f) (blocks-vision . #t))))

(define board (make-square-board 10 8 floor))

;;; Add walls
(define board
  (let loop ([y 0] [b board])
    (if (> y 7)
        b
        (loop (+ y 1)
              (board-set b (square-coord 5 y) wall)))))

;;; Create game state
(define game (make-game-state board))

;;; Create units
(define warrior (make-unit 'w1 'warrior 'player
                          '((hp . 100)
                            (max-hp . 100)
                            (attack . 15)
                            (movement . 4)
                            (vision . 5)
                            (char . "W"))))

(define archer (make-unit 'a1 'archer 'player
                         '((hp . 60)
                           (max-hp . 60)
                           (attack . 10)
                           (movement . 5)
                           (vision . 7)
                           (char . "A"))))

(define enemy (make-unit 'e1 'goblin 'enemy
                        '((hp . 40)
                          (max-hp . 40)
                          (attack . 8)
                          (movement . 3)
                          (vision . 4)
                          (char . "E"))))

;;; Place units
(define game (game-place-unit game warrior (square-coord 1 4)))
(define game (game-place-unit game archer (square-coord 2 2)))
(define game (game-place-unit game enemy (square-coord 8 4)))

(display "Created 3 units:\n")
(display "  Warrior (W) at (1,4) - player team\n")
(display "  Archer  (A) at (2,2) - player team\n")
(display "  Goblin  (E) at (8,4) - enemy team\n")
(newline)

(display "All units:\n")
(for-each
  (lambda (entry)
    (let ([coord (car entry)]
          [unit (cdr entry)])
      (display "  ")
      (display (unit%-type unit))
      (display " at ")
      (display coord)
      (display " (team: ")
      (display (unit%-team unit))
      (display ", HP: ")
      (display (unit-get-prop unit 'hp 0))
      (display "/")
      (display (unit-get-prop unit 'max-hp 0))
      (display ")\n")))
  (game-all-units game))
(newline)

;;; ============================================================
;;; Example 2: Unit Movement
;;; ============================================================

(display "Example 2: Unit Movement\n")
(display "──────────────────────────────────────────────────────────\n")

(define warrior-start (game-get-unit-coord game 'w1))
(define warrior-dest (square-coord 3 4))

(display "Warrior starts at ")
(display warrior-start)
(newline)
(display "Trying to move to ")
(display warrior-dest)
(newline)

(define movement-range (unit-get-prop warrior 'movement 0))
(display "Warrior movement range: ")
(display movement-range)
(newline)

(define can-reach (game-unit-can-reach game 'w1 warrior-dest
                                      (lambda (c) (square-neighbors c 'ortho))))

(display "Can reach destination? ")
(display (if can-reach "YES" "NO"))
(newline)

(if can-reach
    (begin
      (set! game (game-move-unit game 'w1 warrior-dest
                                (lambda (c) (square-neighbors c 'ortho))))
      (display "Warrior moved to ")
      (display (game-get-unit-coord game 'w1))
      (newline))
    (display "Warrior cannot reach that location\n"))
(newline)

;;; ============================================================
;;; Example 3: Unit Visibility
;;; ============================================================

(display "Example 3: Unit Visibility\n")
(display "──────────────────────────────────────────────────────────\n")

(define archer-pos (game-get-unit-coord game 'a1))
(display "Archer at ")
(display archer-pos)
(display " with vision range ")
(display (unit-get-prop archer 'vision 0))
(newline)

(define visible-to-archer
  (game-visible-units game 'a1
                     (lambda (c) (square-neighbors c 'all))
                     square-line))

(display "Archer can see ")
(display (length visible-to-archer))
(display " other unit(s):\n")
(for-each
  (lambda (unit)
    (display "  ")
    (display (unit%-type unit))
    (display " (")
    (display (unit%-team unit))
    (display ") at ")
    (display (game-get-unit-coord game (unit%-id unit)))
    (newline))
  visible-to-archer)
(newline)

;;; Check mutual visibility
(display "Can warrior and archer see each other? ")
(display (if (game-units-can-see-each-other game 'w1 'a1 square-line)
             "YES" "NO"))
(newline)

(display "Can archer and enemy see each other? ")
(display (if (game-units-can-see-each-other game 'a1 'e1 square-line)
             "YES (wall blocks)" "NO"))
(newline)
(newline)

;;; ============================================================
;;; Example 4: Team Filtering
;;; ============================================================

(display "Example 4: Team Filtering\n")
(display "──────────────────────────────────────────────────────────\n")

(define player-units (game-units-by-team game 'player))
(define enemy-units (game-units-by-team game 'enemy))

(display "Player team has ")
(display (length player-units))
(display " units:\n")
(for-each
  (lambda (entry)
    (display "  ")
    (display (unit%-type (cdr entry)))
    (display " at ")
    (display (car entry))
    (newline))
  player-units)
(newline)

(display "Enemy team has ")
(display (length enemy-units))
(display " units:\n")
(for-each
  (lambda (entry)
    (display "  ")
    (display (unit%-type (cdr entry)))
    (display " at ")
    (display (car entry))
    (newline))
  enemy-units)
(newline)

;;; ============================================================
;;; Example 5: Unit Properties
;;; ============================================================

(display "Example 5: Modifying Unit Properties\n")
(display "──────────────────────────────────────────────────────────\n")

(define damaged-warrior (game-get-unit-at game (game-get-unit-coord game 'w1)))

(display "Warrior HP: ")
(display (unit-get-prop damaged-warrior 'hp 0))
(newline)

;;; Simulate damage
(define damaged-warrior (unit-set-prop damaged-warrior 'hp 70))

(display "After taking damage, HP: ")
(display (unit-get-prop damaged-warrior 'hp 0))
(newline)

;;; Update game state
(define game (game-place-unit game damaged-warrior
                             (game-get-unit-coord game 'w1)))

(display "Updated warrior in game state\n")
(newline)

;;; ============================================================
;;; Example 6: Hexagonal Board with Units
;;; ============================================================

(display "Example 6: Units on Hexagonal Board\n")
(display "──────────────────────────────────────────────────────────\n")

(define grass (make-tile 'grass '((walkable . #t))))
(define hex-board (make-hex-board 'axial 4 grass))

(define hex-game (make-game-state hex-board))

;;; Create hex units
(define knight (make-unit 'k1 'knight 'player
                         '((movement . 3)
                           (vision . 4)
                           (char . "K"))))

(define mage (make-unit 'm1 'mage 'player
                       '((movement . 2)
                         (vision . 6)
                         (char . "M"))))

;;; Place on hex board
(define hex-game (game-place-unit hex-game knight (axial-coord 0 0)))
(define hex-game (game-place-unit hex-game mage (axial-coord -2 1)))

(display "Placed knight at ")
(display (game-get-unit-coord hex-game 'k1))
(newline)

(display "Placed mage at ")
(display (game-get-unit-coord hex-game 'm1))
(newline)

(display "Knight can reach ")
(display (game-get-unit-coord hex-game 'm1))
(display "? ")
(display (if (game-unit-can-reach hex-game 'k1
                                  (game-get-unit-coord hex-game 'm1)
                                  hex-neighbors)
             "YES" "NO"))
(newline)
(newline)

;;; ============================================================
;;; Summary
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "  Demo Complete!\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)
(display "Unit management features:\n")
(display "  ✓ Create units with properties (HP, attack, movement, vision)\n")
(display "  ✓ Place and track units on boards\n")
(display "  ✓ Move units with pathfinding integration\n")
(display "  ✓ Check unit visibility (FOV-based)\n")
(display "  ✓ Filter units by team\n")
(display "  ✓ Modify unit properties immutably\n")
(display "  ✓ Works with any tile shape (square, hex, triangle)\n")
(newline)
(display "Use cases:\n")
(display "  • Turn-based tactics games\n")
(display "  • Roguelikes with NPCs\n")
(display "  • Strategy games\n")
(display "  • Chess-like board games\n")
(newline)
