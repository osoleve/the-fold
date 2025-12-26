;;; playpen/creations/state-dungeon.ss — Pure Functional Dungeon Crawler
;;;
;;; A mini dungeon crawler using The Fold's State monad for pure state management.
;;; Demonstrates how to build games with deterministic state threading.
;;;
;;; Created by: Sonnet (Builder tier)
;;; Date: 2025-12-26

(load "core/prelude.ss")
(load "core/state.ss")

(display "=== STATE DUNGEON: The Monad's Labyrinth ===\n\n")

;;; ============================================================
;;; Game State Type
;;; ============================================================

;;; GameState = {
;;;   position: (x, y)
;;;   health: Int
;;;   gold: Int
;;;   inventory: List of String
;;;   visited: List of (x, y)
;;;   map: List of ((x, y), Room)
;;; }

(define (make-game-state)
  `((position . (0 . 0))
    (health . 100)
    (gold . 0)
    (inventory . ())
    (visited . ())
    (map . ,(generate-dungeon))))

;;; ============================================================
;;; Dungeon Generation
;;; ============================================================

(define (generate-dungeon)
  `(((0 . 0) . ,(make-room "Entrance" "You stand at the dungeon entrance." empty))
    ((1 . 0) . ,(make-room "Dark Corridor" "A dark hallway stretches before you." empty))
    ((2 . 0) . ,(make-room "Treasure Room" "Gold coins glitter in the torchlight!" treasure))
    ((0 . 1) . ,(make-room "Guard Post" "A skeleton guard blocks your path!" enemy))
    ((1 . 1) . ,(make-room "Library" "Ancient books line dusty shelves." item))
    ((2 . 1) . ,(make-room "Dragon's Lair" "A massive dragon sleeps on a pile of gold!" boss))
    ((0 . 2) . ,(make-room "Fountain" "A magical fountain bubbles softly." healing))
    ((1 . 2) . ,(make-room "Empty Hall" "An empty hallway." empty))
    ((2 . 2) . ,(make-room "Exit" "Sunlight streams from above!" exit))))

(define (make-room name desc type)
  `((name . ,name)
    (description . ,desc)
    (type . ,type)
    (cleared . #f)))

;;; ============================================================
;;; State Accessors
;;; ============================================================

(define (get-position)
  (state-bind (state-get)
    (lambda (s)
      (state-return (cdr (assq 'position s))))))

(define (get-health)
  (state-bind (state-get)
    (lambda (s)
      (state-return (cdr (assq 'health s))))))

(define (get-gold)
  (state-bind (state-get)
    (lambda (s)
      (state-return (cdr (assq 'gold s))))))

(define (get-current-room)
  (state-bind (get-position)
    (lambda (pos)
      (state-bind (state-get)
        (lambda (s)
          (let ([dungeon (cdr (assq 'map s))])
            (let ([room-entry (assoc pos dungeon)])
              (state-return (if room-entry (cdr room-entry) #f)))))))))

;;; ============================================================
;;; State Modifiers
;;; ============================================================

(define (move-to new-pos)
  (state-modify
    (lambda (s)
      (let ([pos-entry (assq 'position s)])
        (if pos-entry
            (let ([rest (remq pos-entry s)])
              (cons (cons 'position new-pos) rest))
            (cons (cons 'position new-pos) s))))))

(define (add-gold amount)
  (state-modify
    (lambda (s)
      (let ([gold-entry (assq 'gold s)])
        (if gold-entry
            (let ([rest (remq gold-entry s)])
              (cons (cons 'gold (+ (cdr gold-entry) amount)) rest))
            s)))))

(define (take-damage amount)
  (state-modify
    (lambda (s)
      (let ([health-entry (assq 'health s)])
        (if health-entry
            (let ([rest (remq health-entry s)])
              (cons (cons 'health (max 0 (- (cdr health-entry) amount))) rest))
            s)))))

(define (heal amount)
  (state-modify
    (lambda (s)
      (let ([health-entry (assq 'health s)])
        (if health-entry
            (let ([rest (remq health-entry s)])
              (cons (cons 'health (min 100 (+ (cdr health-entry) amount))) rest))
            s)))))

;;; ============================================================
;;; Game Actions (Monadic)
;;; ============================================================

(define (explore-room)
  (state-bind (get-current-room)
    (lambda (room)
      (if room
          (let ([room-type (cdr (assq 'type room))])
            (cond
              [(eq? room-type 'treasure)
               (state-bind (add-gold 50)
                 (lambda (_)
                   (state-return "You found 50 gold!")))]
              [(eq? room-type 'healing)
               (state-bind (heal 30)
                 (lambda (_)
                   (state-return "The fountain heals you for 30 HP!")))]
              [(eq? room-type 'enemy)
               (state-bind (take-damage 20)
                 (lambda (_)
                   (state-return "The skeleton attacks! You take 20 damage.")))]
              [(eq? room-type 'boss)
               (state-bind (take-damage 50)
                 (lambda (_)
                   (state-bind (add-gold 100)
                     (lambda (_)
                       (state-return "Dragon fight! You take 50 damage but win 100 gold!")))))]
              [(eq? room-type 'exit)
               (state-return "You found the exit! Victory!")]
              [else (state-return "The room is empty.")]))
          (state-return "No room here.")))))

(define (move direction)
  (state-bind (get-position)
    (lambda (pos)
      (let* ([x (car pos)]
             [y (cdr pos)]
             [new-pos
               (cond
                 [(eq? direction 'north) (cons x (+ y 1))]
                 [(eq? direction 'south) (cons x (- y 1))]
                 [(eq? direction 'east)  (cons (+ x 1) y)]
                 [(eq? direction 'west)  (cons (- x 1) y)]
                 [else pos])])
        (state-bind (move-to new-pos)
          (lambda (_)
            (state-return (string-append "Moved " (symbol->string direction)))))))))

;;; ============================================================
;;; Display Functions
;;; ============================================================

(define (display-status state)
  (let ([pos (cdr (assq 'position state))]
        [health (cdr (assq 'health state))]
        [gold (cdr (assq 'gold state))])
    (display "\n╔════════════════════════════╗\n")
    (display "║ STATUS                     ║\n")
    (display "╠════════════════════════════╣\n")
    (display (string-append "║ Position: (" (number->string (car pos)) ", " (number->string (cdr pos)) ")          ║\n"))
    (display (string-append "║ Health:   " (number->string health) " HP             ║\n"))
    (display (string-append "║ Gold:     " (number->string gold) "                ║\n"))
    (display "╚════════════════════════════╝\n\n")))

(define (display-room room)
  (if room
      (let ([name (cdr (assq 'name room))]
            [desc (cdr (assq 'description room))])
        (display (string-append "📍 " name "\n"))
        (display (string-append "   " desc "\n\n")))
      (display "You can't go that way!\n\n")))

;;; ============================================================
;;; Game Loop
;;; ============================================================

(define (game-loop state)
  (display-status state)

  (let* ([result (run-state (get-current-room) state)]
         [room (car result)]
         [state2 (cdr result)])

    (display-room room)

    ;; Check win/lose conditions
    (let ([health (cdr (assq 'health state2))]
          [pos (cdr (assq 'position state2))])

      (cond
        [(<= health 0)
         (display "💀 You have died! Game Over.\n\n")
         (display "Final Stats:\n")
         (display-status state2)]

        [(equal? pos '(2 . 2))  ;; Exit room
         (display "🎉 You escaped the dungeon! Victory!\n\n")
         (display "Final Stats:\n")
         (display-status state2)]

        [else
         (display "Commands: (n)orth (s)outh (e)ast (w)est (ex)plore (q)uit\n")
         (display "Choice: ")
         (flush-output-port)

         (let ([cmd (read)])
           (cond
             [(or (eq? cmd 'n) (eq? cmd 'north))
              (let* ([result (run-state (move 'north) state2)]
                     [msg (car result)]
                     [new-state (cdr result)])
                (display (string-append "\n" msg "\n"))
                (game-loop new-state))]

             [(or (eq? cmd 's) (eq? cmd 'south))
              (let* ([result (run-state (move 'south) state2)]
                     [msg (car result)]
                     [new-state (cdr result)])
                (display (string-append "\n" msg "\n"))
                (game-loop new-state))]

             [(or (eq? cmd 'e) (eq? cmd 'east))
              (let* ([result (run-state (move 'east) state2)]
                     [msg (car result)]
                     [new-state (cdr result)])
                (display (string-append "\n" msg "\n"))
                (game-loop new-state))]

             [(or (eq? cmd 'w) (eq? cmd 'west))
              (let* ([result (run-state (move 'west) state2)]
                     [msg (car result)]
                     [new-state (cdr result)])
                (display (string-append "\n" msg "\n"))
                (game-loop new-state))]

             [(or (eq? cmd 'ex) (eq? cmd 'explore))
              (let* ([result (run-state (explore-room) state2)]
                     [msg (car result)]
                     [new-state (cdr result)])
                (display (string-append "\n✨ " msg "\n"))
                (game-loop new-state))]

             [(eq? cmd 'q)
              (display "\nThanks for playing STATE DUNGEON!\n\n")]

             [else
              (display "\nInvalid command.\n")
              (game-loop state2)]))]))))

;;; ============================================================
;;; Start Game
;;; ============================================================

(display "A pure functional dungeon crawler powered by State monads!\n\n")
(display "Navigate a 3x3 dungeon, fight enemies, collect treasure, and escape!\n")
(display "All state changes are pure - perfect for replay and debugging.\n\n")
(display "Press ENTER to begin your quest...")
(read-line)
(newline)

(game-loop (make-game-state))
