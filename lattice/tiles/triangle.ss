;;; playpen/boardcraft/triangle.ss — Triangular Tile Implementation
;;;
;;; Triangular tiles create interesting tessellations where triangles
;;; alternate between "up" (pointing up) and "down" (pointing down).
;;;
;;; Used in games like TriHex and various tessellation puzzles.
;;;
;;; Dependencies:
;;;   - playpen/boardcraft/core.ss

;;; ====
;;; Triangular Coordinates
;;; ====

;;; Triangular tiles use a coordinate system where each position
;;; in a (row, col) grid can contain 2 triangles: one pointing up,
;;; one pointing down.
;;;
;;; We use a 3-tuple: (row, col, orientation)
;;;   where orientation is 'up or 'down

(define-record-type triangle-coord%
  (fields row col orientation))

;;; triangle-coord : Integer × Integer × Symbol → TriangleCoord
(define (triangle-coord row col orientation)
  (if (not (memq orientation '(up down)))
      (error 'triangle-coord "Orientation must be 'up or 'down" orientation)
      (make-triangle-coord% row col orientation)))

;;; Accessors
(define (triangle-row t) (triangle-coord%-row t))
(define (triangle-col t) (triangle-coord%-col t))
(define (triangle-orientation t) (triangle-coord%-orientation t))

;;; triangle-coord-equal? : TriangleCoord × TriangleCoord → Boolean
(define (triangle-coord-equal? t1 t2)
  (and (= (triangle-row t1) (triangle-row t2))
       (= (triangle-col t1) (triangle-col t2))
       (eq? (triangle-orientation t1) (triangle-orientation t2))))

;;; triangle-coord-hash : TriangleCoord → Integer
(define (triangle-coord-hash t)
  (+ (* (triangle-row t) 1000000)
     (* (triangle-col t) 1000)
     (if (eq? (triangle-orientation t) 'up) 0 1)))

;;; ====
;;; Orientation and Alternation
;;; ====

;;; In a standard triangular grid, triangles alternate:
;;;   Row 0: up down up down ...
;;;   Row 1: down up down up ...
;;;
;;; The orientation depends on (row + col) parity

;;; triangle-default-orientation : Integer × Integer → Symbol
;;; Get the default orientation for a position based on parity
(define (triangle-default-orientation row col)
  (if (even? (+ row col))
      'up
      'down))

;;; triangle-flip : TriangleCoord → TriangleCoord
;;; Flip a triangle to its opposite orientation at the same position
(define (triangle-flip t)
  (triangle-coord (triangle-row t)
                  (triangle-col t)
                  (if (eq? (triangle-orientation t) 'up) 'down 'up)))

;;; ====
;;; Neighbor Calculations
;;; ====

;;; Triangles have 3 edge-adjacent neighbors.
;;; The specific neighbors depend on orientation:
;;;
;;; UP triangle (△):
;;;   - Left neighbor (same row, col-1, down)
;;;   - Right neighbor (same row, col+1, down)
;;;   - Below (row+1, same col, up)
;;;
;;; DOWN triangle (▽):
;;;   - Left neighbor (same row, col-1, up)
;;;   - Right neighbor (same row, col+1, up)
;;;   - Above (row-1, same col, down)

;;; triangle-neighbors-edge : TriangleCoord → (List TriangleCoord)
;;; Get the 3 edge-adjacent neighbors
(define (triangle-neighbors-edge t)
  (let ([row (triangle-row t)]
        [col (triangle-col t)]
        [ori (triangle-orientation t)])
       (if (eq? ori 'up)
           ;; Up triangle: left-down, right-down, below-up
           (list (triangle-coord row (- col 1) 'down)
                 (triangle-coord row (+ col 1) 'down)
                 (triangle-coord (+ row 1) col 'up))
           ;; Down triangle: left-up, right-up, above-down
           (list (triangle-coord row (- col 1) 'up)
                 (triangle-coord row (+ col 1) 'up)
                 (triangle-coord (- row 1) col 'down)))))

;;; triangle-neighbors-vertex : TriangleCoord → (List TriangleCoord)
;;; Get vertex-adjacent neighbors (sharing a corner but not edge)
;;; Up triangles have 3 vertex neighbors (diagonal connections)
;;; Down triangles have 3 vertex neighbors
(define (triangle-neighbors-vertex t)
  (let ([row (triangle-row t)]
        [col (triangle-col t)]
        [ori (triangle-orientation t)])
       (if (eq? ori 'up)
           ;; Up triangle vertex neighbors
           (list (triangle-coord (- row 1) (- col 1) 'down)
                 (triangle-coord (- row 1) col 'up)
                 (triangle-coord (- row 1) (+ col 1) 'down))
           ;; Down triangle vertex neighbors
           (list (triangle-coord (+ row 1) (- col 1) 'up)
                 (triangle-coord (+ row 1) col 'down)
                 (triangle-coord (+ row 1) (+ col 1) 'up)))))

;;; triangle-neighbors-all : TriangleCoord → (List TriangleCoord)
;;; Get all neighbors (edge + vertex)
(define (triangle-neighbors-all t)
  (append (triangle-neighbors-edge t)
          (triangle-neighbors-vertex t)
          ;; Include the flipped triangle at same position
          (list (triangle-flip t))))

;;; triangle-neighbors : TriangleCoord × Symbol → (List TriangleCoord)
;;; Generic neighbor function
;;;   'edge — 3 edge-adjacent neighbors
;;;   'vertex — 3 vertex-adjacent neighbors
;;;   'all — all 7 neighbors (edge + vertex + flip)
(define (triangle-neighbors coord mode)
  (case mode
        [(edge)   (triangle-neighbors-edge coord)]
        [(vertex) (triangle-neighbors-vertex coord)]
        [(all)    (triangle-neighbors-all coord)]
        [else     (error 'triangle-neighbors "Invalid mode" mode)]))

;;; ====
;;; Distance Metrics
;;; ====

;;; For triangular grids, distance is more complex than square/hex.
;;; We'll use a simple metric: minimum number of edge-transitions.
;;;
;;; This is essentially BFS distance, but for simple cases we can
;;; approximate with Manhattan-like distance.

;;; triangle-distance-manhattan : TriangleCoord × TriangleCoord → Integer
;;; Approximate distance (sum of row/col differences)
;;; This is an underestimate of true triangle distance
(define (triangle-distance-manhattan t1 t2)
  (+ (abs (- (triangle-row t2) (triangle-row t1)))
     (abs (- (triangle-col t2) (triangle-col t1)))
     (if (eq? (triangle-orientation t1) (triangle-orientation t2)) 0 1)))

;;; For exact distance, we'd need pathfinding (BFS/Dijkstra)
;;; which will be in the pathfinding module

;;; ====
;;; Line Drawing
;;; ====

;;; Line drawing in triangular space is complex due to alternating
;;; orientations. For now, provide a simple stepped approach.

;;; triangle-line : TriangleCoord × TriangleCoord → (List TriangleCoord)
;;; Get triangles on approximate line from t1 to t2
;;; Uses row-major stepping
(define (triangle-line t1 t2)
  (let* ([r1 (triangle-row t1)]
         [c1 (triangle-col t1)]
         [r2 (triangle-row t2)]
         [c2 (triangle-col t2)]
         [dr (- r2 r1)]
         [dc (- c2 c1)]
         [steps (max (abs dr) (abs dc))])
        (if (= steps 0)
            (list t1)
            (let loop ([i 0] [coords '()])
                 (if (> i steps)
                     (reverse coords)
                     (let* ([t (/ i steps)]
                            [r (round (+ r1 (* dr t)))]
                            [c (round (+ c1 (* dc t)))]
                            [ori (triangle-default-orientation r c)]
                            [coord (triangle-coord r c ori)])
                           (loop (+ i 1) (cons coord coords))))))))

;;; ====
;;; Range and Area
;;; ====

;;; triangle-range : TriangleCoord × Integer → (List TriangleCoord)
;;; Get all triangles within approximate distance N
;;; Uses Manhattan approximation
(define (triangle-range center radius)
  (let ([cr (triangle-row center)]
        [cc (triangle-col center)])
       (let loop ([r (- cr radius)] [c (- cc radius)] [coords '()])
            (cond
             [(> r (+ cr radius)) coords]
             [(> c (+ cc radius)) (loop (+ r 1) (- cc radius) coords)]
             [else
              (let* ([ori-up (triangle-coord r c 'up)]
                     [ori-down (triangle-coord r c 'down)]
                     [new-coords
                      (append
                       (if (<= (triangle-distance-manhattan center ori-up) radius)
                           (list ori-up)
                           '())
                       (if (<= (triangle-distance-manhattan center ori-down) radius)
                           (list ori-down)
                           '()))])
                    (loop r (+ c 1) (append new-coords coords)))]))))

;;; ====
;;; Board Creation
;;; ====

;;; make-triangle-board : Integer × Integer × [Tile] → Board
;;; Create a rectangular triangular board
;;;   rows: number of rows
;;;   cols: number of columns
;;;   Each (row, col) contains 2 triangles (up and down)
(define make-triangle-board
  (case-lambda
   [(rows cols)
    (make-board 'triangle
                `((rows . ,rows) (cols . ,cols))
                triangle-coord-hash
                triangle-coord-equal?)]
   [(rows cols default-tile)
    (let ([board (make-board 'triangle
                             `((rows . ,rows) (cols . ,cols))
                             triangle-coord-hash
                             triangle-coord-equal?)])
         (let loop-r ([r 0] [b board])
              (if (>= r rows)
                  b
                  (let loop-c ([c 0] [board2 b])
                       (if (>= c cols)
                           (loop-r (+ r 1) board2)
                           (let* ([b1 (board-set board2 (triangle-coord r c 'up) default-tile)]
                                  [b2 (board-set b1 (triangle-coord r c 'down) default-tile)])
                                 (loop-c (+ c 1) b2)))))))]))

;;; triangle-board-rows : Board → Integer
(define (triangle-board-rows board)
  (alist-ref (board-meta board) 'rows))

;;; triangle-board-cols : Board → Integer
(define (triangle-board-cols board)
  (alist-ref (board-meta board) 'cols))

;;; triangle-in-bounds? : Board × TriangleCoord → Boolean
(define (triangle-in-bounds? board coord)
  (let ([r (triangle-row coord)]
        [c (triangle-col coord)]
        [rows (triangle-board-rows board)]
        [cols (triangle-board-cols board)])
       (and (>= r 0) (< r rows) (>= c 0) (< c cols))))

;;; ====
;;; Rotation and Reflection
;;; ====

;;; triangle-rotate-180 : TriangleCoord × (Integer Integer) → TriangleCoord
;;; Rotate triangle 180° around a center point
;;; This flips the triangle and inverts position
(define (triangle-rotate-180 coord center-r center-c)
  (let ([r (triangle-row coord)]
        [c (triangle-col coord)]
        [ori (triangle-orientation coord)])
       (triangle-coord (- (* 2 center-r) r)
                       (- (* 2 center-c) c)
                       (if (eq? ori 'up) 'down 'up))))

;;; triangle-reflect-horizontal : TriangleCoord × Integer → TriangleCoord
;;; Reflect triangle across vertical axis at column C
(define (triangle-reflect-horizontal coord axis-col)
  (let ([r (triangle-row coord)]
        [c (triangle-col coord)]
        [ori (triangle-orientation coord)])
       (triangle-coord r
                       (- (* 2 axis-col) c)
                       ori)))

;;; triangle-reflect-vertical : TriangleCoord × Integer → TriangleCoord
;;; Reflect triangle across horizontal axis at row R
(define (triangle-reflect-vertical coord axis-row)
  (let ([r (triangle-row coord)]
        [c (triangle-col coord)]
        [ori (triangle-orientation coord)])
       (triangle-coord (- (* 2 axis-row) r)
                       c
                       (if (eq? ori 'up) 'down 'up))))

;;; ====
;;; Exports Summary
;;; ====

;;; This module provides:
;;;   • triangle-coord — Create triangular coordinate
;;;   • triangle-flip — Flip orientation
;;;   • triangle-neighbors-edge — 3 edge neighbors
;;;   • triangle-neighbors-vertex — 3 vertex neighbors
;;;   • triangle-neighbors-all — all 7 neighbors
;;;   • triangle-distance-manhattan — Approximate distance
;;;   • triangle-line — Line approximation
;;;   • triangle-range — Triangles within distance
;;;   • make-triangle-board — Create board
;;;   • triangle-rotate-180 — 180° rotation
;;;   • triangle-reflect-* — Reflection operations
