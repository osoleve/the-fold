;;; playpen/creations/simple-hex-game.ss — Simple Hexagonal Board Game Demo
;;;
;;; A demonstration of creating a board game using the BoardCraft SDK

(load "playpen/boardcraft/boardcraft.ss")

;;; ============================================================
;;; Simple Game Data Structures
;;; ============================================================

; Use association lists instead of hash tables
(define (make-game-state board current-player turn red-units blue-units red-score blue-score)
  (list 'game-state board current-player turn red-units blue-units red-score blue-score))

(define (game-state-board state) (list-ref state 1))
(define (game-state-current-player state) (list-ref state 2))
(define (game-state-turn state) (list-ref state 3))
(define (game-state-red-units state) (list-ref state 4))
(define (game-state-blue-units state) (list-ref state 5))
(define (game-state-red-score state) (list-ref state 6))
(define (game-state-blue-score state) (list-ref state 7))

(define (make-unit player position)
  (list 'unit player position))

(define (unit-player unit) (list-ref unit 1))
(define (unit-position unit) (list-ref unit 2))

(define (unit? obj)
  (and (list? obj) (eq? (car obj) 'unit)))

(define (opponent player)
  (case player
        [(red) 'blue]
        [(blue) 'red]
        [else (error "Unknown player" player)]))

;;; ============================================================
;;; Game Setup
;;; ============================================================

(define (initialize-game)
  (let ([board (make-hex-board 'axial 3)])  ; Small 3-radius board
       (make-game-state board 'red 1 '() '() 0 0)))

;;; ============================================================
;;; Unit Placement Logic
;;; ============================================================

(define (add-unit-to-list unit unit-list)
  (cons unit unit-list))

(define (place-unit game position)
  (let* ([current-player (game-state-current-player game)]
         [new-unit (make-unit current-player position)]
         [board (game-state-board game)]
         [red-units (game-state-red-units game)]
         [blue-units (game-state-blue-units game)])
        
        ; Check if position is valid (empty and in bounds)
        (if (and (hex-in-bounds? board position)
                 (not (hex-tile-data board position)))
            ; Valid move - place unit
            (let ([new-board (hex-set-tile-data board position new-unit)]
                  [new-red-units (if (eq? current-player 'red)
                                     (add-unit-to-list new-unit red-units)
                                     red-units)]
                  [new-blue-units (if (eq? current-player 'blue)
                                      (add-unit-to-list new-unit blue-units)
                                      blue-units)])
                 (make-game-state new-board current-player 1 new-red-units new-blue-units 0 0))
            ; Invalid move - return game unchanged
            game)))

;;; ============================================================
;;; Game Display
;;; ============================================================

(define (display-unit-info unit)
  (display "    Unit: ")
  (display (unit-player unit))
  (display " at ")
  (display (unit-position unit))
  (newline))

(define (display-game-state game)
  (display "═══════════════════════════════════════════════════════════════\n")
  (display "  SIMPLE HEX STRATEGY GAME\n")
  (display "═══════════════════════════════════════════════════════════════\n")
  (display "Current Player: ")
  (display (game-state-current-player game))
  (newline)
  (display "Turn: ")
  (display (game-state-turn game))
  (newline)
  (display "Red Units: ")
  (display (length (game-state-red-units game)))
  (newline)
  (display "Blue Units: ")
  (display (length (game-state-blue-units game)))
  (newline)
  
  (display "\nRed Units:\n")
  (for-each display-unit-info (game-state-red-units game))
  
  (display "\nBlue Units:\n")
  (for-each display-unit-info (game-state-blue-units game))
  
  (display "\nBoard Tiles with Units:\n")
  (let ([game-board (game-state-board game)]
        [tiles (board-tiles game-board)])
       (for-each
        (lambda (tile)
                (let ([pos (car tile)]
                      [data (cdr tile)])
                     (when (unit? data)
                           (display "  Tile ")
                           (display pos)
                           (display " has ")
                           (display (unit-player data))
                           (display " unit\n"))))
        tiles))
  
  (newline))

;;; ============================================================
;;; Demo Game Play
;;; ============================================================

(define (demo-game)
  (display "Starting Simple Hex Strategy Game Demo...\n\n")
  
  ; Initialize game
  (let ([game (initialize-game)])
       (display "Initial game state:\n")
       (display-game-state game)
       
       ; Demo some moves
       (display "Placing Red unit at (0 . 0)...\n")
       (let ([game2 (place-unit game '(0 . 0))])
            (display-game-state game2)
            
            (display "Placing Blue unit at (1 . 0)...\n")
            (let ([game3 (place-unit game2 '(1 . 0))])
                 (display-game-state game3)
                 
                 (display "Attempting invalid move - placing unit on occupied tile...\n")
                 (let ([game4 (place-unit game3 '(0 . 0))])
                      (display-game-state game4)
                      
                      (display "Placing another Red unit at (-1 . 1)...\n")
                      (let ([game5 (place-unit game4 '(-1 . 1))])
                           (display-game-state game5)
                           
                           (display "Demo complete! Game shows basic unit placement mechanics.\n")
                           (display "This framework could be extended with:\n")
                           (display "  - Territory control rules\n")
                           (display "  - Movement mechanics\n")
                           (display "  - Combat system\n")
                           (display "  - Victory conditions\n")))))))

;;; Run the demo
(demo-game)
