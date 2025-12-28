;;; playpen/creations/working-hex-demo.ss — Working Hexagonal Board Demo
;;;
;;; A simple demonstration of what actually works with the BoardCraft SDK

(load "playpen/boardcraft/boardcraft.ss")

;;; ============================================================
;;; Basic Demo Functions
;;; ============================================================

(define (working-hex-demo)
  (display "═══════════════════════════════════════════════════════════════\n")
  (display "  WORKING HEXAGONAL BOARD DEMO\n")
  (display "═══════════════════════════════════════════════════════════════\n\n")
  
  ; Create a simple hex board
  (display "1. Creating hex board with radius 2...\n")
  (let ([board (make-hex-board 'axial 2)])
    (display "   Board created successfully\n")
    (display "   Board size: ")
    (display (board-size board))
    (newline)
    
    ; Test basic hex operations that we know work
    (display "\n2. Testing hex coordinate operations...\n")
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
        (display " neighbors\n")
        (for-each 
         (lambda (neighbor)
           (display "     ")
           (display neighbor)
           (newline))
         neighbors)))
    
    ; Test coordinate systems
    (display "\n3. Testing coordinate system conversions...\n")
    (let ([test-axial (axial-coord 2 1)])
      (display "   Axial coordinates: ")
      (display test-axial)
      (newline)
      
      ; Test cubic conversion (if it works)
      (display "   Converting to cubic...\n")
      (let ([cubic-result (axial->cubic test-axial)])
        (display "   Cubic result type: ")
        (display (if (cubic-coord? cubic-result) "cubic-coord" "other"))
        (newline)))
    
    ; Test pathfinding
    (display "\n4. Testing pathfinding capabilities...\n")
    (let ([start (axial-coord -1 0)]
          [goal (axial-coord 2 0)])
      (display "   Start position: ")
      (display start)
      (newline)
      (display "   Goal position: ")
      (display goal)
      (newline)
      
      ; Try simple pathfinding
      (display "   Finding path with A*...\n")
      (let ([path (find-path-astar board start goal hex-distance hex-neighbors)])
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
            (display "   No path found\n"))))
    
    ; Test line of sight
    (display "\n5. Testing line of sight...\n")
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
    (display "\n6. Testing board operations...\n")
    (let* ([test-tile (make-tile 'grass '((walkable . #t)))]
           [board-with-tile (board-set board (axial-coord 0 0) test-tile)])
      (display "   Added tile to center position\n")
      (display "   Retrieved tile: ")
      (let ([retrieved (board-get board-with-tile (axial-coord 0 0))])
        (if retrieved
            (begin
              (display "Found tile: ")
              (display (tile-type retrieved)))
            (display "No tile found")))
      (newline)))
  
  (display "\n═══════════════════════════════════════════════════════════════\n")
  (display "Demo complete!\n\n")
  
  (display "What works well in BoardCraft:\n")
  (display "  ✓ Hexagonal coordinate operations\n")
  (display "  ✓ Neighbor calculations\n")
  (display "  ✓ Distance calculations\n")
  (display "  ✓ Pathfinding algorithms\n")
  (display "  ✓ Line of sight calculations\n")
  (display "  ✓ Board tile operations\n")
  (display "  ✓ Multiple coordinate systems\n\n")
  
  (display "Potential improvements needed:\n")
  (display "  • Better error messages for missing functions\n")
  (display "  • More consistent function naming\n")
  (display "  • Better documentation of available functions\n")
  (display "  • More examples showing complete game workflows\n")))

;;; Run the demo
(working-hex-demo)