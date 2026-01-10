;;; playpen/boardcraft/examples/square-chess.ss — Chess Board Example
;;;
;;; Demonstrates square board for chess-like games.
;;;
;;; Usage:
;;;   scheme --script playpen/boardcraft/examples/square-chess.ss

(load "lattice/tiles/boardcraft.ss")

;;; Helper function
(define (take lst n)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(display "═══════════════════════════════════════════════════════════════\n")
(display "  BOARDCRAFT CHESS DEMO\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

;;; ============================================================
;;; Chess Board Setup
;;; ============================================================

(display "Creating 8×8 chess board...\n")
(newline)

;;; Define chess pieces as tiles
(define (make-piece color type)
  (make-tile 'piece `((color . ,color) (type . ,type))))

;;; Create empty board
(define empty-square (make-tile 'empty '()))
(define board (make-square-board 8 8 empty-square))

(display "Board dimensions: ")
(display (square-board-width board))
(display " × ")
(display (square-board-height board))
(newline)
(newline)

;;; ============================================================
;;; Example 1: Knight Moves
;;; ============================================================

(display "Example 1: Knight Movement\n")
(display "──────────────────────────────────\n")

;;; Knights move in an L-shape: 2 squares in one direction, 1 in perpendicular
(define (knight-moves coord)
  (let* ([x (coord-x coord)]
         [y (coord-y coord)]
         [offsets '((-2 -1) (-2 1) (-1 -2) (-1 2)
                    (1 -2) (1 2) (2 -1) (2 1))]
         [moves (map (lambda (off)
                             (square-coord (+ x (car off)) (+ y (cadr off))))
                     offsets)])
        ;; Filter out moves outside board
        (filter (lambda (c)
                        (square-in-bounds? board c))
                moves)))

(define knight-pos (square-coord 3 3))
(display "Knight at ")
(display knight-pos)
(newline)
(display "Possible moves: ")
(display (length (knight-moves knight-pos)))
(newline)
(for-each
 (lambda (move)
         (display "  ")
         (display move)
         (newline))
 (knight-moves knight-pos))
(newline)

;;; ============================================================
;;; Example 2: Rook Moves (Orthogonal)
;;; ============================================================

(display "Example 2: Rook Movement\n")
(display "──────────────────────────────────\n")

;;; Rooks move orthogonally until blocked
(define (rook-line coord direction)
  (let loop ([current (square-neighbor coord direction)] [moves '()])
       (if (not (square-in-bounds? board current))
           (reverse moves)
           (let ([tile (board-get board current)])
                (if (and tile (not (eq? (tile-type tile) 'empty)))
                    ;; Blocked by piece
                    (reverse (cons current moves))
                    ;; Continue
                    (loop (square-neighbor current direction)
                          (cons current moves)))))))

(define rook-pos (square-coord 4 4))
(display "Rook at ")
(display rook-pos)
(newline)
(display "North moves: ")
(display (rook-line rook-pos 'n))
(newline)
(display "East moves: ")
(display (rook-line rook-pos 'e))
(newline)
(newline)

;;; ============================================================
;;; Example 3: King Moves (8-way)
;;; ============================================================

(display "Example 3: King Movement\n")
(display "──────────────────────────────────\n")

(define king-pos (square-coord 0 0))
(display "King at corner ")
(display king-pos)
(newline)
(display "Can move to ")
(display (length (filter (lambda (c) (square-in-bounds? board c))
                         (square-neighbors-all king-pos))))
(display " squares:\n")
(for-each
 (lambda (move)
         (when (square-in-bounds? board move)
               (display "  ")
               (display move)
               (newline)))
 (square-neighbors-all king-pos))
(newline)

;;; ============================================================
;;; Example 4: Distance Metrics
;;; ============================================================

(display "Example 4: Distance Between Squares\n")
(display "──────────────────────────────────\n")

(define a1 (square-coord 0 0))
(define h8 (square-coord 7 7))

(display "From ")
(display a1)
(display " to ")
(display h8)
(display ":\n")
(display "  Manhattan (rook): ")
(display (square-distance a1 h8 'manhattan))
(display " moves\n")
(display "  Chebyshev (king): ")
(display (square-distance a1 h8 'chebyshev))
(display " moves\n")
(display "  Euclidean: ")
(display (square-distance a1 h8 'euclidean))
(display " units\n")
(newline)

;;; ============================================================
;;; Example 5: Range and Area
;;; ============================================================

(display "Example 5: Control Area\n")
(display "──────────────────────────────────\n")

(define queen-pos (square-coord 4 4))
(display "Queen at ")
(display queen-pos)
(newline)
(display "Squares within Chebyshev distance 2:\n")
(let ([area (square-range queen-pos 2 'chebyshev)])
     (display "  Count: ")
     (display (length area))
     (display " squares\n"))
(newline)

;;; ============================================================
;;; Example 6: Line of Sight
;;; ============================================================

(display "Example 6: Line of Sight\n")
(display "──────────────────────────────────\n")

(define from (square-coord 0 0))
(define to (square-coord 7 5))

(display "Line from ")
(display from)
(display " to ")
(display to)
(display ":\n")
(let ([line (square-line from to)])
     (display "  ")
     (for-each
      (lambda (c)
              (display c)
              (display " "))
      line)
     (newline))
(newline)

;;; ============================================================
;;; Example 7: Board State Manipulation
;;; ============================================================

(display "Example 7: Board State\n")
(display "──────────────────────────────────\n")

;;; Place some pieces
(define board-with-pieces
  (board-set
   (board-set board
              (square-coord 4 4)
              (make-piece 'white 'queen))
   (square-coord 4 6)
   (make-piece 'black 'pawn)))

(display "Placed white queen at (4,4)\n")
(display "Placed black pawn at (4,6)\n")
(display "Total pieces on board: ")
(display (length (filter
                  (lambda (entry)
                          (not (eq? (tile-type (cdr entry)) 'empty)))
                  (board-tiles board-with-pieces))))
(newline)
(newline)

;;; ============================================================
;;; Example 8: Ring Pattern
;;; ============================================================

(display "Example 8: Ring Pattern\n")
(display "──────────────────────────────────\n")

(define center (square-coord 4 4))
(display "Squares at exactly distance 2 from center:\n")
(let ([ring (square-ring center 2)])
     (display "  Count: ")
     (display (length ring))
     (newline)
     (display "  First few: ")
     (for-each
      (lambda (c)
              (display c)
              (display " "))
      (take ring 5))
     (display "...\n"))
(newline)

;;; ============================================================
;;; Summary
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "  Demo Complete!\n")
(display "═══════════════════════════════════════════════════════════════\n")
(display "\nSquare boards are perfect for:\n")
(display "  • Chess, Checkers, Go\n")
(display "  • Tactical RPGs (Fire Emblem, Final Fantasy Tactics)\n")
(display "  • Grid-based puzzle games\n")
(display "  • Any game with 4-way or 8-way movement\n")
(newline)
