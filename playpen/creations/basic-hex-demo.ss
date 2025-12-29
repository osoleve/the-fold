;;; playpen/creations/basic-hex-demo.ss — Basic Hexagonal Board Game Demo
;;;
;;; A simple demonstration of creating a board game using the BoardCraft SDK

(load "playpen/boardcraft/boardcraft.ss")

;;; ============================================================
;;; Simple Demo Functions
;;; ============================================================

(define (demo-hex-board)
  (display "Creating a simple hexagonal board game demo...\n\n")
  
  ; Create a small hex board
  (let ([board (make-hex-board 'axial 2)])
       (display "Created hex board with radius 2\n")
       (display "Board size: ")
       (display (board-size board))
       (newline)
       
       ; Show some basic hex operations
       (let ([center '(0 . 0)]
             [neighbor '(1 . 0)])
            (display "\nCenter hex: ")
            (display center)
            (newline)
            (display "A neighbor: ")
            (display neighbor)
            (newline)
            (display "Distance from center to neighbor: ")
            (display (hex-distance center neighbor))
            (newline)
            
            ; Get all neighbors of center
            (display "\nAll neighbors of center hex:\n")
            (let ([neighbors (hex-neighbors center)])
                 (for-each
                  (lambda (neighbor)
                          (display "  ")
                          (display neighbor)
                          (newline))
                  neighbors))
            
            ; Show coordinate conversion
            (display "\nCoordinate conversion demo:\n")
            (let ([axial '(2 . 1)])
                 (display "Axial coordinates: ")
                 (display axial)
                 (newline)
                 (let ([cubic (axial->cubic axial)])
                      (display "Cubic coordinates: ")
                      (display (cubic-coord-x cubic))
                      (display " ")
                      (display (cubic-coord-y cubic))
                      (display " ")
                      (display (cubic-coord-z cubic))
                      (newline)))))
  
  (display "\nDemo complete!\n")
  (display "The BoardCraft SDK provides powerful tools for hexagonal games:\n")
  (display "  - Multiple coordinate systems (axial, cubic, offset)\n")
  (display "  - Neighbor and distance calculations\n")
  (display "  - Pathfinding algorithms\n")
  (display "  - Line of sight and visibility\n")
  (display "  - Rendering capabilities\n"))

;;; Run the demo
(demo-hex-board)
