;;; playpen/creations/final-hex-demo.ss — Final Hexagonal Board Demo

(load "playpen/boardcraft/boardcraft.ss")

(define (final-hex-demo)
  (display "═══════════════════════════════════════════════════════════════\n")
  (display "  BOARDCRAFT HEXAGONAL DEMO\n")
  (display "═══════════════════════════════════════════════════════════════\n\n")
  
  ; Create board
  (display "1. Creating hex board with radius 2...\n")
  (let ([board (make-hex-board 'axial 2)])
       (display "   Board size: ")
       (display (board-size board))
       (newline))
  
  ; Test coordinates
  (display "\n2. Testing hex operations...\n")
  (let ([center (axial-coord 0 0)]
        [test-pos (axial-coord 1 0)])
       (display "   Center: ")
       (display center)
       (newline)
       (display "   Distance center to test-pos: ")
       (display (hex-distance center test-pos))
       (newline))
  
  ; Test pathfinding
  (display "\n3. Testing pathfinding...\n")
  (let ([board (make-hex-board 'axial 3)]
        [start (axial-coord -1 0)]
        [goal (axial-coord 2 0)])
       (let ([path (find-path-astar board start goal hex-neighbors hex-distance)])
            (if path
                (begin
                 (display "   Path found! Length: ")
                 (display (length path))
                 (newline))
                (display "   No path found\n"))))
  
  ; Test board operations
  (display "\n4. Testing board operations...\n")
  (let ([board (make-hex-board 'axial 2)]
        [tile (make-tile 'grass '((walkable . #t)))])
       (let ([new-board (board-set board (axial-coord 0 0) tile)])
            (display "   Tile placed successfully\n")
            (let ([retrieved (board-get new-board (axial-coord 0 0))])
                 (if retrieved
                     (begin
                      (display "   Tile retrieved: ")
                      (display (tile-type retrieved))
                      (newline))
                     (display "   No tile found\n")))))
  
  (display "\n═══════════════════════════════════════════════════════════════\n")
  (display "Demo complete!\n\n")
  (display "BoardCraft SDK provides:\n")
  (display "  ✓ Hexagonal coordinate operations\n")
  (display "  ✓ Pathfinding algorithms (A*, Dijkstra, BFS)\n")
  (display "  ✓ Board tile management\n")
  (display "  ✓ Multiple coordinate systems\n")
  (display "  ✓ Line of sight calculations\n")
  (display "  ✓ Rendering capabilities\n"))

(final-hex-demo)
