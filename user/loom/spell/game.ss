;;; playpen/loom/spell/game.ss — Game Definition Macro
;;;
;;; The def-game macro provides top-level game configuration including
;;; title, player setup, world dimensions, and win/lose conditions.
;;;
;;; This is part of Spell: the DSL for weaving game logic.
;;;
;;; Exports:
;;;   def-game        Define a complete game configuration
;;;   game-create     Create a new game instance from definition
;;;   game-check-end  Check win/lose conditions

;;; ====
;;; Game Configuration Structure
;;; ====

;;; GameConfig : Alist
;;;   title       — String, game title
;;;   description — String, game description
;;;   version     — String, version number
;;;   world-width — Nat, world width in tiles
;;;   world-height — Nat, world height in tiles
;;;   player-spec — Alist, player entity configuration
;;;   win-condition — (World -> Bool)
;;;   lose-condition — (World -> Bool)
;;;   init-fn     — (World -> World), initialization hook
;;;   tick-fn     — (World -> World), per-tick hook

(define (make-game-config)
  '((title . "Untitled Game")
    (description . "")
    (version . "1.0")
    (world-width . 80)
    (world-height . 40)
    (player-spec . ())
    (win-condition . #f)
    (lose-condition . #f)
    (init-fn . #f)
    (tick-fn . #f)))

;;; ====
;;; Game Registry
;;; ====

;;; *games* : Hashtable Symbol -> GameConfig
(define *games* (make-hashtable symbol-hash eq?))

;;; game-register! : Symbol × GameConfig -> Void
(define (game-register! name config)
  (hashtable-set! *games* name config))

;;; game-get : Symbol -> GameConfig | #f
(define (game-get name)
  (hashtable-ref *games* name #f))

;;; game-exists? : Symbol -> Bool
(define (game-exists? name)
  (hashtable-contains? *games* name))

;;; ====
;;; Game Definition Macro
;;; ====

;;; def-game : Name × Clauses... -> Definitions
;;;
;;; Define a complete game with configuration and rules.
;;;
;;; Syntax:
;;;   (def-game game-name
;;;     (title "Game Title")
;;;     (description "Game description")
;;;     (version "1.0")
;;;     (world width height)
;;;     (player
;;;       (name "Hero")
;;;       (char #\@)
;;;       (stats hp max-hp attack defense speed)
;;;       (color 'white))
;;;     (win-when condition)
;;;     (lose-when condition)
;;;     (on-init body ...)
;;;     (on-tick body ...))
;;;
;;; Example:
;;;   (def-game dungeon-escape
;;;     (title "Dungeon Escape")
;;;     (description "Find the stairs and escape!")
;;;     (world 80 40)
;;;     (player
;;;       (name "Hero")
;;;       (char #\@)
;;;       (stats 100 100 10 5 100))
;;;     (win-when (on-tile? 'stairs-up))
;;;     (lose-when (<= (player-hp) 0)))

(define-syntax def-game
  (lambda (stx)
          (syntax-case stx (title description version world player
                                  name char stats color
                                  win-when lose-when on-init on-tick)
                       ;; Main form - collect all clauses
                       [(_ game-name clauses ...)
                        #'(begin
                           (define game-name
                             (parse-game-clauses 'game-name (make-game-config) clauses ...))
                           (game-register! 'game-name game-name))])))

;;; parse-game-clauses : Symbol × GameConfig × Clause... -> GameConfig
;;; Parse game definition clauses and build config.
(define-syntax parse-game-clauses
  (syntax-rules (title description version world player
                       name char stats color
                       win-when lose-when on-init on-tick)
                ;; Base case: no more clauses
                [(_ game-name config)
                 config]
                
                ;; Title
                [(_ game-name config (title str) rest ...)
                 (parse-game-clauses game-name
                                     (alist-set config 'title str)
                                     rest ...)]
                
                ;; Description
                [(_ game-name config (description str) rest ...)
                 (parse-game-clauses game-name
                                     (alist-set config 'description str)
                                     rest ...)]
                
                ;; Version
                [(_ game-name config (version str) rest ...)
                 (parse-game-clauses game-name
                                     (alist-set config 'version str)
                                     rest ...)]
                
                ;; World dimensions
                [(_ game-name config (world w h) rest ...)
                 (parse-game-clauses game-name
                                     (alist-set (alist-set config 'world-width w) 'world-height h)
                                     rest ...)]
                
                ;; Player definition
                [(_ game-name config (player player-clauses ...) rest ...)
                 (parse-game-clauses game-name
                                     (alist-set config 'player-spec
                                                (parse-player-spec player-clauses ...))
                                     rest ...)]
                
                ;; Win condition
                [(_ game-name config (win-when condition) rest ...)
                 (parse-game-clauses game-name
                                     (alist-set config 'win-condition
                                                (lambda (world) condition))
                                     rest ...)]
                
                ;; Lose condition
                [(_ game-name config (lose-when condition) rest ...)
                 (parse-game-clauses game-name
                                     (alist-set config 'lose-condition
                                                (lambda (world) condition))
                                     rest ...)]
                
                ;; Init hook
                [(_ game-name config (on-init body ...) rest ...)
                 (parse-game-clauses game-name
                                     (alist-set config 'init-fn
                                                (lambda (world) body ... world))
                                     rest ...)]
                
                ;; Tick hook
                [(_ game-name config (on-tick body ...) rest ...)
                 (parse-game-clauses game-name
                                     (alist-set config 'tick-fn
                                                (lambda (world) body ... world))
                                     rest ...)]))

;;; parse-player-spec : Clause... -> Alist
(define-syntax parse-player-spec
  (syntax-rules (name char stats color)
                [(_)
                 '()]
                [(_ (name str) rest ...)
                 (alist-set (parse-player-spec rest ...) 'name str)]
                [(_ (char c) rest ...)
                 (alist-set (parse-player-spec rest ...) 'char c)]
                [(_ (stats hp max-hp atk def spd) rest ...)
                 (alist-set (parse-player-spec rest ...) 'stats
                            (list hp max-hp atk def spd))]
                [(_ (color col) rest ...)
                 (alist-set (parse-player-spec rest ...) 'color col)]))

;;; ====
;;; Game Instance Creation
;;; ====

;;; game-create : GameConfig × [Nat × Nat] -> World
;;; Create a new game world from a game configuration.
;;; Optional player starting position, defaults to center.
(define game-create
  (case-lambda
   [(config)
    (let ([w (alist-ref config 'world-width 80)]
          [h (alist-ref config 'world-height 40)])
         (game-create config (cons (quotient w 2) (quotient h 2))))]
   [(config player-pos)
    (let* ([w (alist-ref config 'world-width 80)]
           [h (alist-ref config 'world-height 40)]
           [player-spec (alist-ref config 'player-spec '())]
           ;; Create empty world
           [world (make-world w h)]
           ;; Create player entity
           [player (create-player-from-spec player-spec player-pos)]
           ;; Add player to world
           [world (world-add-entity world player)]
           ;; Run init hook if present
           [init-fn (alist-ref config 'init-fn)]
           [world (if init-fn (init-fn world) world)])
          world)]))

;;; create-player-from-spec : Alist × Point -> Entity
;;; Create a player entity from player spec.
(define (create-player-from-spec spec player-pos)
  (let* ([name (alist-ref spec 'name "Player")]
         [char (alist-ref spec 'char #\@)]
         [stats (alist-ref spec 'stats '(100 100 10 5 100))]
         [color (alist-ref spec 'color 'white)]
         [hp (if (pair? stats) (car stats) 100)]
         [max-hp (if (and (pair? stats) (pair? (cdr stats))) (cadr stats) 100)]
         [atk (if (and (pair? stats) (>= (length stats) 3)) (caddr stats) 10)]
         [def (if (and (pair? stats) (>= (length stats) 4)) (cadddr stats) 5)]
         [spd (if (and (pair? stats) (>= (length stats) 5)) (car (cddddr stats)) 100)])
        (-> (make-entity)
            (entity-add-component (make-position-component
                                   (point-x player-pos)
                                   (point-y player-pos)))
            (entity-add-component (make-stats-component hp max-hp atk def spd))
            (entity-add-component (make-renderable-component char 100 color))
            (entity-add-component (make-name-component name "The player character"))
            (entity-add-component (make-actor-component))
            (entity-add-component (make-faction-component 'player '(monster))))))

;;; ====
;;; Game State Checking
;;; ====

;;; GameState : Symbol
;;;   'playing   — Game in progress
;;;   'won       — Player won
;;;   'lost      — Player lost

;;; game-check-state : GameConfig × World -> GameState
;;; Check if the game has ended.
(define (game-check-state config world)
  (let ([win-fn (alist-ref config 'win-condition)]
        [lose-fn (alist-ref config 'lose-condition)])
       (cond
        ;; Check lose first (death is immediate)
        [(and lose-fn (lose-fn world)) 'lost]
        ;; Check win
        [(and win-fn (win-fn world)) 'won]
        ;; Still playing
        [else 'playing])))

;;; game-won? : GameConfig × World -> Bool
(define (game-won? config world)
  (eq? (game-check-state config world) 'won))

;;; game-lost? : GameConfig × World -> Bool
(define (game-lost? config world)
  (eq? (game-check-state config world) 'lost))

;;; game-over? : GameConfig × World -> Bool
(define (game-over? config world)
  (not (eq? (game-check-state config world) 'playing)))

;;; ====
;;; Win/Lose Condition Helpers
;;; ====

;;; These functions are designed to be used in win-when/lose-when
;;; They take the world implicitly via closure

;;; player-hp : World -> Nat
;;; Get player's current HP.
(define (player-hp world)
  (let ([player (world-get-player world)])
       (if player
           (let ([stats (entity-stats player)])
                (if stats (stats-hp stats) 0))
           0)))

;;; player-alive? : World -> Bool
(define (player-alive? world)
  (let ([player (world-get-player world)])
       (and player (entity-alive? player))))

;;; player-dead? : World -> Bool
(define (player-dead? world)
  (not (player-alive? world)))

;;; player-at-tile? : World × Symbol -> Bool
;;; Check if player is standing on a tile of given type.
(define (player-at-tile? world tile-type)
  (let ([player (world-get-player world)])
       (if player
           (let* ([pos (entity-position player)]
                  [tile (world-get-tile world (position-x pos) (position-y pos))])
                 (and tile (eq? (tile-type tile) tile-type)))
           #f)))

;;; all-enemies-dead? : World -> Bool
;;; All monster-faction entities are dead.
(define (all-enemies-dead? world)
  (null? (world-find-entities world
                              (lambda (e)
                                      (and (faction-is? e 'monster)
                                           (entity-alive? e))))))

;;; enemy-count : World -> Nat
;;; Count living monster-faction entities.
(define (enemy-count world)
  (length (world-find-entities world
                               (lambda (e)
                                       (and (faction-is? e 'monster)
                                            (entity-alive? e))))))

;;; turn-count : World -> Nat
;;; Get current turn number.
(define (turn-count world)
  (alist-ref (world-state world) 'turn-number 0))

;;; ====
;;; Game Tick Processing
;;; ====

;;; game-tick : GameConfig × World -> World
;;; Process one game tick, running the on-tick hook if defined.
(define (game-tick config world)
  (let ([tick-fn (alist-ref config 'tick-fn)])
       (if tick-fn
           (tick-fn world)
           world)))

;;; ====
;;; Example Usage (commented out)
;;; ====

#|
;;; Define a simple dungeon escape game
(def-game dungeon-escape
  (title "Dungeon Escape")
  (description "Find the stairs and escape the dungeon!")
  (version "1.0")
  (world 80 40)
  
  (player
   (name "Hero")
   (char #\@)
   (stats 100 100 15 8 100)
   (color 'white))
  
  (win-when (player-at-tile? world 'stairs-up))
  (lose-when (<= (player-hp world) 0))
  
  (on-init
   (display "Welcome to the dungeon!\n"))
  
  (on-tick
   (when (= (modulo (turn-count world) 10) 0)
         (display "The dungeon grows darker...\n"))))


;;; Define a survival game
(def-game monster-arena
  (title "Monster Arena")
  (description "Defeat all monsters to win!")
  (world 40 30)
  
  (player
   (name "Gladiator")
   (char #\@)
   (stats 150 150 20 10 90))
  
  (win-when (all-enemies-dead? world))
  (lose-when (player-dead? world)))


;;; Create and run a game:
;;; (define my-game dungeon-escape)
;;; (define world (game-create my-game))
;;;
;;; ;; Game loop
;;; (let loop ([w world])
;;;   (case (game-check-state my-game w)
;;;     [(won) (display "You won!\n")]
;;;     [(lost) (display "Game over!\n")]
;;;     [(playing)
;;;      (display (world->string w))
;;;      (let* ([input (read-player-input)]
;;;             [new-w (process-input w input)]
;;;             [new-w (game-tick my-game new-w)])
;;;        (loop new-w))]))


;;; ==== WAVE/ROUND SYSTEM PATTERN ====
;;;
;;; For wave-based games, track round state in world-state:
;;;
;;; (def-game arena-survival
;;;   (title "Arena Survival")
;;;   (world 40 30)
;;;   (player (name "Fighter") (char #\@) (stats 100 100 15 5 100))
;;;
;;;   ;; Win after surviving 10 waves
;;;   (win-when (>= (get-wave-number world) 10))
;;;   (lose-when (player-dead? world))
;;;
;;;   (on-init
;;;     (set-world-state! world 'wave-number 1)
;;;     (spawn-wave! world 1))
;;;
;;;   (on-tick
;;;     ;; Check if wave is complete
;;;     (when (and (all-enemies-dead? world)
;;;                (< (get-wave-number world) 10))
;;;       (let ([next-wave (+ 1 (get-wave-number world))])
;;;         (set-world-state! world 'wave-number next-wave)
;;;         (spawn-wave! world next-wave)))))
;;;
;;; The spawn-wave! function would be defined separately:
;;;
;;; (define (spawn-wave! world wave-num)
;;;   (let ([enemy-count (* wave-num 2)])
;;;     (dotimes (i enemy-count)
;;;       (let ([x (+ 5 (random 30))]
;;;             [y (+ 5 (random 20))])
;;;         (world-add-entity world (make-goblin x y))))))
;;;
;;; Note: get-wave-number and set-world-state! are helpers you define:
;;;
;;; (define (get-wave-number world)
;;;   (alist-ref (world-state world) 'wave-number 1))
;;;
;;; (define (set-world-state! world key value)
;;;   (world-set-state world (alist-set (world-state world) key value)))


;;; ==== GAME LOOP INTEGRATION ====
;;;
;;; The game loop coordinates:
;;; 1. Check win/lose with game-check-state
;;; 2. Render the world (world->canvas, canvas->string)
;;; 3. Get player input
;;; 4. Process player action
;;; 5. Run AI for all entities
;;; 6. Run game-tick hook
;;; 7. Repeat
;;;
;;; Minimal game loop skeleton:
;;;
;;; (define (run-game game-config)
;;;   (let loop ([world (game-create game-config)])
;;;     (case (game-check-state game-config world)
;;;       [(won) (display "Victory!\n")]
;;;       [(lost) (display "Defeat!\n")]
;;;       [(playing)
;;;        ;; Render
;;;        (display (canvas->string (world->canvas world)))
;;;        ;; Get input and process
;;;        (let* ([input (read-char)]
;;;               [world (process-player-input world input)]
;;;               ;; Run AI for all entities
;;;               [world (run-all-ai world)]
;;;               ;; Run game tick
;;;               [world (game-tick game-config world)])
;;;          (loop world))])))
;;;
;;; The process-player-input and run-all-ai functions use the Loom
;;; action and AI systems respectively.
|#
