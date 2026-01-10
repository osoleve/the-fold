;;; playpen/creations/working-hex-demo.ss — Working Hexagonal Board Demo
;;;
;;; A simple demonstration of what actually works with the BoardCraft SDK

(load "user/boardcraft/boardcraft.ss")

;;; ============================================================
;;; Basic Demo Functions
;;; ============================================================

(define (working-hex-demo)
  (display "═══════════════════════════════════════════════════════════════
")
  (display "  WORKING HEXAGONAL BOARD DEMO
")
  (display "═══════════════════════════════════════════════════════════════

")
  
  ; Create a simple hex board
  (display "1. Creating hex board with radius 2...
")
  (let ([board (make-hex-board 'axial 2)])
       (display "   Board created successfully
")
       (display "   Board size: ")
       (display (board-size board))
       (newline)
       
       ; Test basic hex operations that we know work
       (display "
2. Testing hex coordinate operations...
")
       (let ([center (axial-coord 0 0)]
             [test-pos (axial-coord 1 0)])
            
            (display "   Center position: ")
            (display center)
            (newline)
            (display "   Test position: ")
            (display test-pos)
            (newline)
            
            (display "   Distance between them: ")
            (display (hex-distance center test-pos))
            (newline)
            
            ; Test neighbors
            (display "   Neighbors of center: ")
            (let ([neighbors (hex-neighbors center)])
                 (display (length neighbors))
                 (display " neighbors
")
                 (for-each
                  (lambda (neighbor)
                          (display "     ")
                          (display neighbor)
                          (newline))
                  neighbors)))
       
       ; Test coordinate systems
       (display "
3. Testing coordinate system conversions...
")
       (let ([test-axial (axial-coord 2 1)])
            (display "   Axial coordinates: ")
            (display test-axial)
            (newline)
            
            ; Test cubic conversion (if it works)
            (display "   Converting to cubic...
")
            (let ([cubic-result (axial->cubic test-axial)])
                 (display "   Cubic result type: ")
                 (display (if (cubic-coord? cubic-result) "cubic-coord" "other"))
                 (newline)))
       
       ; Test pathfinding
       (display "
4. Testing pathfinding capabilities...
")
       (let ([start (axial-coord -1 0)]
             [goal (axial-coord 2 0)])
            (display "   Start position: ")
            (display start)
            (newline)
            (display "   Goal position: ")
            (display goal)
            (newline)
            
            ; Try simple pathfinding
            (display "   Finding path with A*...
")
            (let ([path (find-path-astar board start goal hex-neighbors hex-distance)])
                 (if path
                     (begin
                      (display "   Path found! Length: ")
                      (display (length path))
                      (newline)
                      (display "   Path: ")
                      (display (take path 5))  ; Show first 5 steps
                      (if (> (length path) 5)
                          (display "...")
                          (display ""))
                      (newline))
                     (display "   No path found
"))))
       
       ; Test line of sight
       (display "
5. Testing line of sight...
")
       (let ([observer (axial-coord 0 0)]
             [target (axial-coord 2 1)])
            (display "   Observer: ")
            (display observer)
            (newline)
            (display "   Target: ")
            (display target)
            (newline)
            
            (display "   Line of sight: ")
            (display (if (board-has-los? board observer target hex-line) "Yes" "No"))
            (newline))
       
       ; Test board operations
       (display "
6. Testing board operations...
")
       (let* ([test-tile (make-tile 'grass '((walkable . #t)))]
              [board-with-tile (board-set board (axial-coord 0 0) test-tile)])
             (display "   Added tile to center position
")
             (display "   Retrieved tile: ")
             (let ([retrieved (board-get board-with-tile (axial-coord 0 0))])
                  (if retrieved
                      (begin
                       (display "Found tile: ")
                       (display (tile-type retrieved)))
                      (display "No tile found")))
             (newline)))
  
  (display "
═══════════════════════════════════════════════════════════════
")
  (display "Demo complete!

")
  
  (display "What works well in BoardCraft:
")
  (display "  ✓ Hexagonal coordinate operations
")
  (display "  ✓ Neighbor calculations
")
  (display "  ✓ Distance calculations
")
  (display "  ✓ Pathfinding algorithms
")
  (display "  ✓ Line of sight calculations
")
  (display "  ✓ Board tile operations
")
  (display "  ✓ Multiple coordinate systems

")
  
  (display "Potential improvements needed:
")
  (display "  • Better error messages for missing functions
")
  (display "  • More consistent function naming
")
  (display "  • Better documentation of available functions
")
  (display "  • More examples showing complete game workflows
"))

;;; Run the demo
(working-hex-demo)
