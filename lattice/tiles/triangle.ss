(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @requires tiles/core
(require 'tiles/core)

(doc 'module 'tiles/triangle)
(doc 'description "Triangular tile implementation with alternating up/down orientations")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'coordinates)
(doc 'note "Coordinate system: (row, col, orientation) where orientation is 'up or 'down")

;;; Triangle coords: ((row . col) . orientation)
;;; Cons-based for structural equality under equal? (needed by dict)

(define (triangle-coord row col orientation)
  (doc 'export #t)
  (doc 'description "Create triangular coordinate")
  (doc 'type '(-> Integer Integer Symbol TriangleCoord))
  (doc 'param 'orientation "Must be 'up or 'down")
  (if (not (memq orientation '(up down)))
      (error 'triangle-coord "Orientation must be 'up or 'down" orientation)
      (cons (cons row col) orientation)))

;;; Accessors
(define (triangle-row t) (caar t))
(define (triangle-col t) (cdar t))
(define (triangle-orientation t) (cdr t))

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

(doc 'section 'orientation)
(doc 'note "Triangles alternate based on (row + col) parity")

(define (triangle-default-orientation row col)
  (doc 'description "Get default orientation for a position based on parity")
  (doc 'type '(-> Integer Integer Symbol))
  (if (even? (+ row col))
      'up
      'down))

(define (triangle-flip t)
  (doc 'description "Flip triangle to opposite orientation at same position")
  (doc 'type '(-> TriangleCoord TriangleCoord))
  (triangle-coord (triangle-row t)
                  (triangle-col t)
                  (if (eq? (triangle-orientation t) 'up) 'down 'up)))

(doc 'section 'neighbors)
(doc 'note "UP triangle has 3 edge neighbors: left-down, right-down, below-up")
(doc 'note "DOWN triangle has 3 edge neighbors: left-up, right-up, above-down")

(define (triangle-neighbors-edge t)
  (doc 'description "Get 3 edge-adjacent neighbors")
  (doc 'type '(-> TriangleCoord (List TriangleCoord)))
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

(define (triangle-neighbors-vertex t)
  (doc 'description "Get vertex-adjacent neighbors (sharing a corner but not edge)")
  (doc 'type '(-> TriangleCoord (List TriangleCoord)))
  (doc 'returns "3 vertex neighbors")
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

(define (triangle-neighbors-all t)
  (doc 'description "Get all neighbors (edge + vertex + flip)")
  (doc 'type '(-> TriangleCoord (List TriangleCoord)))
  (doc 'returns "7 neighbors total")
  (append (triangle-neighbors-edge t)
          (triangle-neighbors-vertex t)
          ;; Include the flipped triangle at same position
          (list (triangle-flip t))))

(define (triangle-neighbors coord mode)
  (doc 'export #t)
  (doc 'description "Generic neighbor function. Mode: 'edge, 'vertex, or 'all")
  (doc 'type '(-> TriangleCoord Symbol (List TriangleCoord)))
  (case mode
        [(edge)   (triangle-neighbors-edge coord)]
        [(vertex) (triangle-neighbors-vertex coord)]
        [(all)    (triangle-neighbors-all coord)]
        [else     (error 'triangle-neighbors "Invalid mode" mode)]))

(doc 'section 'distance)

(define (triangle-distance-manhattan t1 t2)
  (doc 'export #t)
  (doc 'description "Approximate distance (sum of row/col differences). Underestimates true distance.")
  (doc 'type '(-> TriangleCoord TriangleCoord Integer))
  (doc 'note "For exact distance, use pathfinding (BFS/Dijkstra)")
  (+ (abs (- (triangle-row t2) (triangle-row t1)))
     (abs (- (triangle-col t2) (triangle-col t1)))
     (if (eq? (triangle-orientation t1) (triangle-orientation t2)) 0 1)))

;;; For exact distance, we'd need pathfinding (BFS/Dijkstra)
;;; which will be in the pathfinding module

(doc 'section 'line-drawing)

(define (triangle-line t1 t2)
  (doc 'export #t)
  (doc 'description "Get triangles on approximate line from t1 to t2. Uses row-major stepping.")
  (doc 'type '(-> TriangleCoord TriangleCoord (List TriangleCoord)))
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

(doc 'section 'range)

(define (triangle-range center radius)
  (doc 'export #t)
  (doc 'description "Get all triangles within approximate distance N. Uses Manhattan approximation.")
  (doc 'type '(-> TriangleCoord Integer (List TriangleCoord)))
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

(doc 'section 'board-creation)

(doc make-triangle-board 'export #t)
(define make-triangle-board
  (case-lambda
   [(rows cols)
    (doc 'description "Create rectangular triangular board. Each (row, col) contains 2 triangles.")
    (doc 'type '(-> Integer Integer Board))
    (make-board 'triangle
                `((rows . ,rows) (cols . ,cols)))]
   [(rows cols default-tile)
    (doc 'description "Create rectangular triangular board with default tile fill")
    (doc 'type '(-> Integer Integer Tile Board))
    (let ([board (make-board 'triangle
                             `((rows . ,rows) (cols . ,cols)))])
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

(doc 'section 'transformations)

(define (triangle-rotate-180 coord center-r center-c)
  (doc 'description "Rotate triangle 180° around center point. Flips orientation and inverts position.")
  (doc 'type '(-> TriangleCoord Integer Integer TriangleCoord))
  (let ([r (triangle-row coord)]
        [c (triangle-col coord)]
        [ori (triangle-orientation coord)])
       (triangle-coord (- (* 2 center-r) r)
                       (- (* 2 center-c) c)
                       (if (eq? ori 'up) 'down 'up))))

(define (triangle-reflect-horizontal coord axis-col)
  (doc 'description "Reflect triangle across vertical axis at column C")
  (doc 'type '(-> TriangleCoord Integer TriangleCoord))
  (let ([r (triangle-row coord)]
        [c (triangle-col coord)]
        [ori (triangle-orientation coord)])
       (triangle-coord r
                       (- (* 2 axis-col) c)
                       ori)))

(define (triangle-reflect-vertical coord axis-row)
  (doc 'description "Reflect triangle across horizontal axis at row R. Flips orientation.")
  (doc 'type '(-> TriangleCoord Integer TriangleCoord))
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
