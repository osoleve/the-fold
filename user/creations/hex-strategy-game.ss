;;; playpen/creations/hex-strategy-game.ss — A Simple Hexagonal Strategy Game
;;;
;;; A territory control game where players place units on a hexagonal board
;;; and try to control the most territory by the end of the game.

(load "lattice/tiles/boardcraft.ss")

;;; ============================================================
;;; Game Configuration
;;; ============================================================

(define BOARD_RADIUS 4)  ; 4-radius hex board
(define MAX_TURNS 20)    ; Game ends after 20 turns
(define STARTING_UNITS 5) ; Each player starts with 5 units

;;; ============================================================
;;; Game State
;;; ============================================================

(define (make-game-state board current-player turn units scores)
  (list 'game-state board current-player turn units scores))

(define (game-state-board state) (list-ref state 1))
(define (game-state-current-player state) (list-ref state 2))
(define (game-state-turn state) (list-ref state 3))
(define (game-state-units state) (list-ref state 4))
(define (game-state-scores state) (list-ref state 5))

;;; ============================================================
;;; Unit Definitions
;;; ============================================================

(define (make-unit player position strength)
  (list 'unit player position strength))

(define (unit-player unit) (list-ref unit 1))
(define (unit-position unit) (list-ref unit 2))
(define (unit-strength unit) (list-ref unit 3))

(define (unit? obj)
  (and (list? obj) (eq? (car obj) 'unit)))

(define (opponent player)
  (case player
        [(red) 'blue]
        [(blue) 'red]
        [else (error "Unknown player" player)]))

;;; ============================================================
;;; Game Initialization
;;; ============================================================

(define (initialize-game)
  (let* ([board (make-hex-board 'axial BOARD_RADIUS)]
         [units (make-hash-table)]
         [scores (make-hash-table)])
        
        ; Initialize empty unit lists for both players
        (hash-table-set! units 'red '())
        (hash-table-set! units 'blue '())
        
        ; Initialize scores
        (hash-table-set! scores 'red 0)
        (hash-table-set! scores 'blue 0)
        
        (make-game-state board 'red 1 units scores)))

;;; ============================================================
;;; Game Logic
;;; ============================================================

(define (valid-move? game position)
  (let ([board (game-state-board game)])
       (and (hex-in-bounds? board position)
            (not (hex-tile-data board position)))))

(define (game-over? game)
  (>= (game-state-turn game) MAX_TURNS))

(define (next-turn game)
  (let ([new-turn (if (eq? (game-state-current-player game) 'red)
                      (game-state-turn game)
                      (+ (game-state-turn game) 1))])
       (make-game-state
        (game-state-board game)
        (opponent (game-state-current-player game))
        new-turn
        (game-state-units game)
        (game-state-scores game))))

;;; ============================================================
;;; Game Display
;;; ============================================================

(define (display-game-state game)
  (display "═══════════════════════════════════════════════════════════════\n")
  (display "  HEX STRATEGY GAME - Turn ")
  (display (game-state-turn game))
  (display ", Current Player: ")
  (display (game-state-current-player game))
  (newline)
  (display "═══════════════════════════════════════════════════════════════\n")
  
  (display "\nBoard visualization would go here\n")
  (newline))

;;; ============================================================
;;; Main Game Function
;;; ============================================================

(define (play-hex-strategy-game)
  (display "Starting Hex Strategy Game...\n")
  (let ([game (initialize-game)])
       (display-game-state game)
       (display "Game initialized successfully!\n")
       (display "This is a basic framework - expand with actual gameplay.\n")))

;;; Run the game
(play-hex-strategy-game)
