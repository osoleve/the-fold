;;; playpen/boardcraft/square.ss — Square Tile Implementation
;;;
;;; Square tiles are the most common tile shape in board games.
;;; This module provides coordinate systems, neighbor calculations,
;;; and distance metrics for square-tiled boards.
;;;
;;; Dependencies:
;;;   - playpen/boardcraft/core.ss

;;; ====
;;; Square Coordinates
;;; ====

;;; Square tiles use simple Cartesian coordinates (x, y).
;;; Origin is typically at (0, 0) with x increasing right, y increasing down.
;;;
;;; This matches array indexing: board[y][x]

;;; square-coord : Integer × Integer → Coord
(define square-coord coord)

;;; ====
;;; Neighbor Calculations
;;; ====

;;; Square tiles have up to 8 neighbors:
;;;   - 4 orthogonal (edge-adjacent): N, E, S, W
;;;   - 4 diagonal (corner-adjacent): NE, SE, SW, NW

;;; Direction offsets for 8-way movement
(define square-offset-8
  '((n  .  (0 . -1))
    (ne .  (1 . -1))
    (e  .  (1 .  0))
    (se .  (1 .  1))
    (s  .  (0 .  1))
    (sw . (-1 .  1))
    (w  . (-1 .  0))
    (nw . (-1 . -1))))

;;; Direction offsets for 4-way movement (orthogonal only)
(define square-offset-4
  '((n . (0 . -1))
    (e . (1 .  0))
    (s . (0 .  1))
    (w . (-1 . 0))))

;;; square-neighbor : Coord × Symbol → Coord
;;; Get neighbor in a specific direction
(define (square-neighbor coord dir)
  (let ([offset (alist-ref square-offset-8 dir)])
       (if offset
           (square-coord (+ (coord-x coord) (car offset))
                         (+ (coord-y coord) (cdr offset)))
           (error 'square-neighbor "Invalid direction" dir))))

;;; square-neighbors-ortho : Coord → (List Coord)
;;; Get 4 orthogonal neighbors (rook moves in chess)
(define (square-neighbors-ortho coord)
  (map (lambda (dir) (square-neighbor coord (car dir)))
       square-offset-4))

;;; square-neighbors-diag : Coord → (List Coord)
;;; Get 4 diagonal neighbors (bishop moves in chess)
(define (square-neighbors-diag coord)
  (map (lambda (dir) (square-neighbor coord dir))
       '(ne se sw nw)))

;;; square-neighbors-all : Coord → (List Coord)
;;; Get all 8 neighbors (king moves in chess)
(define (square-neighbors-all coord)
  (map (lambda (dir) (square-neighbor coord (car dir)))
       square-offset-8))

;;; square-neighbors : Coord × Symbol → (List Coord)
;;; Generic neighbor function with movement type
;;;   'ortho — 4 orthogonal neighbors
;;;   'diag — 4 diagonal neighbors
;;;   'all — all 8 neighbors
(define (square-neighbors coord mode)
  (case mode
        [(ortho) (square-neighbors-ortho coord)]
        [(diag)  (square-neighbors-diag coord)]
        [(all)   (square-neighbors-all coord)]
        [else    (error 'square-neighbors "Invalid mode" mode)]))

;;; ====
;;; Distance Metrics
;;; ====

;;; square-distance-manhattan : Coord × Coord → Integer
;;; Manhattan distance (L1 metric)
;;; Used for: rook movement, taxi distance
(define (square-distance-manhattan c1 c2)
  (manhattan-distance (coord-x c1) (coord-y c1)
                      (coord-x c2) (coord-y c2)))

;;; square-distance-chebyshev : Coord × Coord → Integer
;;; Chebyshev distance (L∞ metric)
;;; Used for: king movement, 8-way movement cost
(define (square-distance-chebyshev c1 c2)
  (chebyshev-distance (coord-x c1) (coord-y c1)
                      (coord-x c2) (coord-y c2)))

;;; square-distance-euclidean : Coord × Coord → Real
;;; Euclidean distance (L2 metric)
;;; Used for: straight-line distance, range calculations
(define (square-distance-euclidean c1 c2)
  (euclidean-distance (coord-x c1) (coord-y c1)
                      (coord-x c2) (coord-y c2)))

;;; square-distance : Coord × Coord × Symbol → Number
;;; Generic distance function with metric type
(define (square-distance c1 c2 metric)
  (case metric
        [(manhattan) (square-distance-manhattan c1 c2)]
        [(chebyshev) (square-distance-chebyshev c1 c2)]
        [(euclidean) (square-distance-euclidean c1 c2)]
        [else        (error 'square-distance "Invalid metric" metric)]))

;;; ====
;;; Range and Area
;;; ====

;;; square-line : Coord × Coord → (List Coord)
;;; Get all coordinates on a straight line from c1 to c2
;;; Uses Bresenham's line algorithm
(define (square-line c1 c2)
  (let* ([x1 (coord-x c1)]
         [y1 (coord-y c1)]
         [x2 (coord-x c2)]
         [y2 (coord-y c2)]
         [dx (abs (- x2 x1))]
         [dy (abs (- y2 y1))]
         [sx (if (< x1 x2) 1 -1)]
         [sy (if (< y1 y2) 1 -1)])
        (let loop ([x x1] [y y1] [err (- dx dy)] [coords '()])
             (let ([new-coords (cons (square-coord x y) coords)])
                  (if (and (= x x2) (= y y2))
                      (reverse new-coords)
                      (let* ([e2 (* 2 err)]
                             [new-x (if (> e2 (- dy)) (+ x sx) x)]
                             [new-y (if (< e2 dx) (+ y sy) y)]
                             [new-err (+ err
                                         (if (> e2 (- dy)) (- dy) 0)
                                         (if (< e2 dx) dx 0))])
                            (loop new-x new-y new-err new-coords)))))))

;;; square-range : Coord × Integer × Symbol → (List Coord)
;;; Get all coordinates within range of center using specified metric
;;;   metric: 'manhattan, 'chebyshev, or 'euclidean
(define (square-range center radius metric)
  (let ([cx (coord-x center)]
        [cy (coord-y center)])
       (let loop ([x (- cx radius)] [y (- cy radius)] [coords '()])
            (cond
             [(> y (+ cy radius)) (reverse coords)]
             [(> x (+ cx radius)) (loop (- cx radius) (+ y 1) coords)]
             [else
              (let ([coord (square-coord x y)])
                   (if (<= (square-distance center coord metric) radius)
                       (loop (+ x 1) y (cons coord coords))
                       (loop (+ x 1) y coords)))]))))

;;; square-ring : Coord × Integer → (List Coord)
;;; Get all coordinates at exactly distance N from center (Chebyshev)
(define (square-ring center radius)
  (if (= radius 0)
      (list center)
      (let ([cx (coord-x center)]
            [cy (coord-y center)])
           (append
            ;; Top edge
            (let loop ([x (- cx radius)] [acc '()])
                 (if (> x (+ cx radius))
                     acc
                     (loop (+ x 1) (cons (square-coord x (- cy radius)) acc))))
            ;; Right edge
            (let loop ([y (- cy radius -1)] [acc '()])
                 (if (> y (+ cy radius))
                     acc
                     (loop (+ y 1) (cons (square-coord (+ cx radius) y) acc))))
            ;; Bottom edge
            (let loop ([x (+ cx radius -1)] [acc '()])
                 (if (< x (- cx radius))
                     acc
                     (loop (- x 1) (cons (square-coord x (+ cy radius)) acc))))
            ;; Left edge
            (let loop ([y (+ cy radius -1)] [acc '()])
                 (if (<= y (- cy radius))
                     acc
                     (loop (- y 1) (cons (square-coord (- cx radius) y) acc))))))))

;;; ====
;;; Board Creation
;;; ====

;;; make-square-board : Integer × Integer × [Tile] → Board
;;; Create a rectangular square board with optional default tile
(define make-square-board
  (case-lambda
   [(width height)
    (make-board 'square `((width . ,width) (height . ,height)))]
   [(width height default-tile)
    (let ([board (make-board 'square `((width . ,width) (height . ,height)))])
         (let loop ([x 0] [y 0] [b board])
              (cond
               [(>= y height) b]
               [(>= x width) (loop 0 (+ y 1) b)]
               [else (loop (+ x 1) y
                           (board-set b (square-coord x y) default-tile))])))]))

;;; square-board-width : Board → Integer
(define (square-board-width board)
  (alist-ref (board-meta board) 'width))

;;; square-board-height : Board → Integer
(define (square-board-height board)
  (alist-ref (board-meta board) 'height))

;;; square-in-bounds? : Board × Coord → Boolean
;;; Check if coordinate is within board bounds
(define (square-in-bounds? board coord)
  (let ([x (coord-x coord)]
        [y (coord-y coord)]
        [w (square-board-width board)]
        [h (square-board-height board)])
       (and (>= x 0) (< x w) (>= y 0) (< y h))))

;;; square-board-capacity : Board → Integer
;;; Returns the total number of squares in the board (width * height).
;;; This is the theoretical capacity based on geometry, not the number
;;; of tiles currently stored. For stored tile count, use (board-size board).
(define (square-board-capacity board)
  (* (square-board-width board) (square-board-height board)))

;;; ====
;;; Exports Summary
;;; ====

;;; This module provides:
;;;   • square-coord — Create square coordinate
;;;   • square-neighbor — Get neighbor in direction
;;;   • square-neighbors-ortho — 4 orthogonal neighbors
;;;   • square-neighbors-diag — 4 diagonal neighbors
;;;   • square-neighbors-all — all 8 neighbors
;;;   • square-distance-manhattan — L1 distance
;;;   • square-distance-chebyshev — L∞ distance
;;;   • square-distance-euclidean — L2 distance
;;;   • square-line — Line drawing (Bresenham)
;;;   • square-range — Area within distance
;;;   • square-ring — Coordinates at exact distance
;;;   • make-square-board — Create rectangular board
;;;   • square-in-bounds? — Bounds checking
;;;   • square-board-capacity — Get total square count for board geometry
