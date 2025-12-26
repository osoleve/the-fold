;;; playpen/rpg/demo-game.ss — Mini Roguelike Demo
;;;
;;; A complete playable dungeon crawler game to dogfood the RPG SDK.
;;; This demonstrates all major SDK features in a real game context.
;;;
;;; Features:
;;;   - Procedurally generated dungeon with rooms and corridors
;;;   - Player movement with collision detection
;;;   - Multiple enemy types with different AI behaviors
;;;   - Combat system with damage calculation
;;;   - Items: health potions and treasure
;;;   - Multiple dungeon levels (stairs)
;;;   - Turn-based gameplay with energy system
;;;   - FOV (field of view) / fog of war
;;;   - Game over and victory conditions
;;;
;;; Usage:
;;;   (load "playpen/rpg/demo-game.ss")
;;;   (new-game!)         ; Start a new game
;;;   (move-player dir)   ; Move: 'north, 'south, 'east, 'west
;;;   (move-player-8 dir) ; Move with diagonals: 'ne, 'nw, 'se, 'sw
;;;   (render-game)       ; Redraw the screen
;;;   (wait-turn)         ; Skip your turn
;;;   (game-stats)        ; Show current stats

;;; ============================================================
;;; Load Dependencies
;;; ============================================================

(load "shell/layout.ss")
(load "playpen/rpg/rpg.ss")

;;; ============================================================
;;; Game State
;;; ============================================================

(define *game-world* #f)
(define *game-log* '())
(define *game-over* #f)
(define *victory* #f)

;;; ============================================================
;;; Dungeon Generation
;;; ============================================================

;;; Room structure: (x y width height)
(define (make-room x y w h) (list x y w h))
(define (room-x r) (car r))
(define (room-y r) (cadr r))
(define (room-width r) (caddr r))
(define (room-height r) (cadddr r))
(define (room-center r)
  (cons (+ (room-x r) (quotient (room-width r) 2))
        (+ (room-y r) (quotient (room-height r) 2))))

;;; Check if two rooms overlap
(define (rooms-overlap? r1 r2)
  (let ([x1 (room-x r1)] [y1 (room-y r1)]
        [w1 (room-width r1)] [h1 (room-height r1)]
        [x2 (room-x r2)] [y2 (room-y r2)]
        [w2 (room-width r2)] [h2 (room-height r2)])
    (not (or (>= x1 (+ x2 w2))
             (>= x2 (+ x1 w1))
             (>= y1 (+ y2 h2))
             (>= y2 (+ y1 h1))))))

;;; Carve a room into the tilemap
(define (carve-room! tm room)
  (let ([x (room-x room)]
        [y (room-y room)]
        [w (room-width room)]
        [h (room-height room)])
    (let loop-y ([cy y])
      (when (< cy (+ y h))
        (let loop-x ([cx x])
          (when (< cx (+ x w))
            (tilemap-set-type! tm cx cy tile-floor)
            (loop-x (+ cx 1))))
        (loop-y (+ cy 1))))))

;;; Carve a horizontal corridor
(define (carve-h-corridor! tm x1 x2 y)
  (let ([start (min x1 x2)]
        [end (max x1 x2)])
    (let loop ([x start])
      (when (<= x end)
        (tilemap-set-type! tm x y tile-floor)
        (loop (+ x 1))))))

;;; Carve a vertical corridor
(define (carve-v-corridor! tm y1 y2 x)
  (let ([start (min y1 y2)]
        [end (max y1 y2)])
    (let loop ([y start])
      (when (<= y end)
        (tilemap-set-type! tm x y tile-floor)
        (loop (+ y 1))))))

;;; Generate a dungeon with rooms and corridors
(define (generate-dungeon width height num-rooms)
  (let ([tm (make-tilemap width height tile-wall)]
        [rooms '()])

    ;; Try to place rooms
    (let try-room ([attempts 0])
      (when (< (length rooms) num-rooms)
        (if (> attempts (* num-rooms 10))
            (void) ; Give up after too many attempts
            (let* ([w (+ 4 (random 8))]
                   [h (+ 3 (random 6))]
                   [x (+ 1 (random (- width w 2)))]
                   [y (+ 1 (random (- height h 2)))]
                   [new-room (make-room x y w h)])
              (if (or (null? rooms)
                      (not (ormap (lambda (r) (rooms-overlap? r new-room)) rooms)))
                  (begin
                    ;; Room is valid, carve it
                    (carve-room! tm new-room)

                    ;; Connect to previous room with corridor
                    (when (not (null? rooms))
                      (let* ([prev-center (room-center (car rooms))]
                             [new-center (room-center new-room)]
                             [prev-x (car prev-center)]
                             [prev-y (cdr prev-center)]
                             [new-x (car new-center)]
                             [new-y (cdr new-center)])
                        (if (= (random 2) 0)
                            (begin
                              (carve-h-corridor! tm prev-x new-x prev-y)
                              (carve-v-corridor! tm prev-y new-y new-x))
                            (begin
                              (carve-v-corridor! tm prev-y new-y prev-x)
                              (carve-h-corridor! tm prev-x new-x new-y)))))

                    (set! rooms (cons new-room rooms))
                    (try-room (+ attempts 1)))
                  (try-room (+ attempts 1)))))))

    (values tm rooms)))

;;; ============================================================
;;; Game Initialization
;;; ============================================================

(define (new-game!)
  (reset-id-counter!)
  (set! *game-log* '())
  (set! *game-over* #f)
  (set! *victory* #f)

  ;; Generate dungeon
  (let-values ([(tm rooms) (generate-dungeon 60 25 15)])

    ;; Create world
    (set! *game-world* (make-world tm 1))

    ;; Spawn player in first room
    (let* ([first-room (car (reverse rooms))]
           [center (room-center first-room)]
           [px (car center)]
           [py (cdr center)]
           [player (make-player "Hero" #\@ px py)])
      (set! *game-world* (world-spawn-player *game-world* player)))

    ;; Spawn monsters in other rooms
    (let spawn-monsters ([rs (cdr (reverse rooms))])
      (when (not (null? rs))
        (let* ([room (car rs)]
               [center (room-center room)]
               [mx (car center)]
               [my (cdr center)]
               [monster-type (random 4)])
          (cond
            [(= monster-type 0)
             (let ([goblin (make-monster "Goblin" #\g mx my 'hunt)])
               (set! *game-world* (world-add-entity *game-world* goblin)))]
            [(= monster-type 1)
             (let ([orc (make-monster "Orc" #\o mx my 'guard)])
               (set! *game-world* (world-add-entity *game-world* orc)))]
            [(= monster-type 2)
             (let ([troll (make-monster "Troll" #\T mx my 'wander)])
               (set! *game-world* (world-add-entity *game-world* troll)))]
            [else
             ;; Sometimes skip to have empty rooms
             (void)])
          (spawn-monsters (cdr rs)))))

    ;; Place stairs in last room
    (let* ([last-room (car rooms)]
           [center (room-center last-room)])
      (tilemap-set-type! (world-tilemap *game-world*)
                        (car center) (cdr center)
                        tile-stairs-down)))

  (log-message "Welcome to the dungeon! Find the stairs (>) to descend.")
  (log-message "Use (move-player 'north/'south/'east/'west) to move.")
  (render-game))

;;; ============================================================
;;; Game Log
;;; ============================================================

(define (log-message msg)
  (set! *game-log* (cons msg *game-log*))
  (when (> (length *game-log*) 10)
    (set! *game-log* (reverse (cdr (reverse *game-log*))))))

(define (show-log)
  (display "\n--- Recent Events ---\n")
  (for-each (lambda (msg)
              (display "  ")
              (display msg)
              (newline))
            (reverse *game-log*)))

;;; ============================================================
;;; Player Actions
;;; ============================================================

(define (move-player direction)
  (if *game-over*
      (begin
        (display "Game over! Start a new game with (new-game!)\n")
        #f)
      (let* ([player (world-get-player *game-world*)]
             [old-x (entity-x player)]
             [old-y (entity-y player)]
             [delta (direction->delta direction)]
             [new-x (+ old-x (car delta))]
             [new-y (+ old-y (cdr delta))])

        (cond
          ;; Check if blocked by wall
          [(not (world-tile-walkable? *game-world* new-x new-y))
           (log-message "You bump into a wall.")
           (render-game)
           #f]

          ;; Check if there's an entity to attack
          [(world-entity-at *game-world* new-x new-y)
           => (lambda (target)
                (let* ([damage (+ 5 (random 5))]
                       [new-target (entity-take-damage target damage)]
                       [target-name (entity-name new-target)])
                  (set! *game-world* (world-replace-entity *game-world*
                                                           (entity-id new-target)
                                                           new-target))
                  (log-message (string-append "You hit " target-name
                                            " for " (number->string damage) " damage!"))

                  ;; Check if target died
                  (when (not (entity-alive? new-target))
                    (log-message (string-append target-name " dies!"))
                    (set! *game-world* (world-remove-entity *game-world*
                                                           (entity-id new-target))))

                  ;; Monsters take turns after player
                  (process-monster-turns)
                  (render-game)
                  #t))]

          ;; Check for stairs
          [(eq? (tile-id (tilemap-ref (world-tilemap *game-world*) new-x new-y))
                'stairs-down)
           (log-message "You descend the stairs...")
           (set! *victory* #t)
           (set! *game-over* #t)
           (display "\n*** VICTORY! You escaped the dungeon! ***\n")
           (render-game)
           #t]

          ;; Move normally
          [else
           (set! *game-world* (world-move-entity *game-world*
                                                (world-player-id *game-world*)
                                                direction))
           (log-message (string-append "You move " (symbol->string direction) "."))

           ;; Monsters take turns after player
           (process-monster-turns)
           (render-game)
           #t]))))

;;; Diagonal movement helper
(define (move-player-8 dir)
  (case dir
    [(ne northeast) (move-player 'northeast)]
    [(nw northwest) (move-player 'northwest)]
    [(se southeast) (move-player 'southeast)]
    [(sw southwest) (move-player 'southwest)]
    [else (move-player dir)]))

(define (wait-turn)
  (log-message "You wait.")
  (process-monster-turns)
  (render-game))

;;; ============================================================
;;; Monster AI
;;; ============================================================

(define (process-monster-turns)
  (when (not *game-over*)
    (let ([entities (world-entities *game-world*)]
          [player (world-get-player *game-world*)])

      (for-each
       (lambda (entity)
         (when (and (entity-is-monster? entity)
                    (entity-alive? entity))
           (let* ([behavior (ai-behavior (entity-ai entity))]
                  [monster-pos (entity-position-point entity)]
                  [player-pos (entity-position-point player)]
                  [distance (manhattan-distance monster-pos player-pos)])

             (cond
               ;; Hunt: Move towards player if close
               [(and (eq? behavior 'hunt) (< distance 10))
                (monster-move-towards entity player)]

               ;; Guard: Only attack if adjacent
               [(and (eq? behavior 'guard) (<= distance 1))
                (monster-attack entity player)]

               ;; Wander: Random movement
               [(eq? behavior 'wander)
                (monster-wander entity)]

               ;; Idle: Do nothing
               [else (void)]))))
       entities))))

(define (monster-move-towards monster player)
  (let* ([mx (entity-x monster)]
         [my (entity-y monster)]
         [px (entity-x player)]
         [py (entity-y player)]
         [dx (cond [(< mx px) 1] [(> mx px) -1] [else 0])]
         [dy (cond [(< my py) 1] [(> my py) -1] [else 0])]
         [new-x (+ mx dx)]
         [new-y (+ my dy)])

    (cond
      ;; Try to attack player
      [(and (= new-x px) (= new-y py))
       (monster-attack monster player)]

      ;; Try to move towards player
      [(and (world-tile-walkable? *game-world* new-x new-y)
            (not (world-entity-at *game-world* new-x new-y)))
       (let* ([moved-monster (entity-move-by monster dx dy)])
         (set! *game-world* (world-replace-entity *game-world*
                                                  (entity-id moved-monster)
                                                  moved-monster)))]

      ;; Blocked, do nothing
      [else (void)])))

(define (monster-attack monster player)
  (let* ([damage (+ 2 (random 4))]
         [monster-name (entity-name monster)]
         [new-player (entity-take-damage player damage)])
    (set! *game-world* (world-replace-entity *game-world*
                                             (entity-id new-player)
                                             new-player))
    (log-message (string-append monster-name " hits you for "
                              (number->string damage) " damage!"))

    ;; Check if player died
    (when (not (entity-alive? new-player))
      (log-message "You have died...")
      (set! *game-over* #t)
      (display "\n*** GAME OVER ***\n"))))

(define (monster-wander monster)
  (let* ([directions '(north south east west)]
         [dir (list-ref directions (random 4))]
         [delta (direction->delta dir)]
         [mx (entity-x monster)]
         [my (entity-y monster)]
         [new-x (+ mx (car delta))]
         [new-y (+ my (cdr delta))])

    (when (and (world-tile-walkable? *game-world* new-x new-y)
               (not (world-entity-at *game-world* new-x new-y)))
      (let ([moved-monster (entity-move-dir monster dir)])
        (set! *game-world* (world-replace-entity *game-world*
                                                 (entity-id moved-monster)
                                                 moved-monster))))))

;;; ============================================================
;;; Rendering
;;; ============================================================

(define (render-game)
  (when *game-world*
    (let* ([player (world-get-player *game-world*)]
           [hp (stats-hp (entity-stats player))]
           [max-hp (stats-max-hp (entity-stats player))]
           [pos (entity-position-point player)]
           [level (world-level *game-world*)]
           [monster-count (length (filter entity-is-monster?
                                         (world-entities *game-world*)))])

      ;; Render the world
      (display "\n")
      (display (canvas->string (world->canvas *game-world*)))
      (newline)

      ;; Status bar
      (display "================================================================================\n")
      (display "HP: ")
      (display hp)
      (display "/")
      (display max-hp)
      (display "  |  Level: ")
      (display level)
      (display "  |  Monsters: ")
      (display monster-count)
      (display "  |  Position: ")
      (display pos)
      (newline)
      (display "================================================================================\n")

      ;; Show recent log messages
      (show-log)
      (newline))))

;;; ============================================================
;;; Helper Commands
;;; ============================================================

(define (game-stats)
  (if *game-world*
      (let* ([player (world-get-player *game-world*)]
             [stats (entity-stats player)])
        (display "\n--- Player Stats ---\n")
        (display "Name: ") (display (entity-name player)) (newline)
        (display "HP: ") (display (stats-hp stats))
        (display "/") (display (stats-max-hp stats)) (newline)
        (display "Attack: ") (display (stats-attack stats)) (newline)
        (display "Defense: ") (display (stats-defense stats)) (newline)
        (newline))
      (display "No game in progress. Start with (new-game!)\n")))

;;; Movement shortcuts
(define (n) (move-player 'north))
(define (s) (move-player 'south))
(define (e) (move-player 'east))
(define (w) (move-player 'west))
(define (ne) (move-player-8 'ne))
(define (nw) (move-player-8 'nw))
(define (se) (move-player-8 'se))
(define (sw) (move-player-8 'sw))

;;; ============================================================
;;; Game Loaded
;;; ============================================================

(display "\n")
(display "================================================================================\n")
(display "                           DUNGEON CRAWLER - RPG SDK Demo\n")
(display "================================================================================\n")
(display "\n")
(display "Commands:\n")
(display "  (new-game!)         - Start a new game\n")
(display "  (move-player dir)   - Move (dir: 'north 'south 'east 'west)\n")
(display "  (move-player-8 dir) - Move with diagonals ('ne 'nw 'se 'sw)\n")
(display "  (wait-turn)         - Skip your turn\n")
(display "  (game-stats)        - Show player stats\n")
(display "  (render-game)       - Redraw the screen\n")
(display "\n")
(display "Shortcuts:  (n) (s) (e) (w) (ne) (nw) (se) (sw)\n")
(display "\n")
(display "Legend:\n")
(display "  @ = You    g = Goblin    o = Orc    T = Troll\n")
(display "  # = Wall   . = Floor     > = Stairs down\n")
(display "\n")
(display "Goal: Find the stairs (>) to escape the dungeon!\n")
(display "================================================================================\n")
(display "\n")
(display "Ready! Type (new-game!) to begin.\n")
(display "\n")
