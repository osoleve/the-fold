;;; playpen/boardcraft/hex.ss — Hexagonal Tile Implementation
;;;
;;; Hexagonal tiles are used in many strategy games and wargames.
;;; This module provides multiple coordinate systems, neighbor calculations,
;;; and distance metrics optimized for hexagonal grids.
;;;
;;; Hexagons can have two orientations:
;;;   • Pointy-top: hexagons with a vertex pointing up
;;;   • Flat-top: hexagons with a flat edge on top
;;;
;;; We support three coordinate systems:
;;;   • Axial (q, r): Most common, good for algorithms
;;;   • Cubic (x, y, z) where x+y+z=0: Best for symmetry
;;;   • Offset (col, row): Good for storage/display
;;;
;;; Dependencies:
;;;   - playpen/boardcraft/core.ss

;;; ============================================================
;;; Hexagonal Coordinate Systems
;;; ============================================================

;;; AXIAL COORDINATES (q, r)
;;;   Most commonly used for hex grids.
;;;   q = column (roughly), r = row (roughly)
;;;   Simple 2D representation.

;;; axial-coord : Integer × Integer → AxialCoord
(define axial-coord coord)

;;; CUBIC COORDINATES (x, y, z) where x + y + z = 0
;;;   Best for symmetric operations.
;;;   Redundant (3 values, one constraint) but elegant.
;;;   Conversions to/from axial are simple.

(define-record-type cubic-coord%
  (fields x y z))

;;; cubic-coord : Integer × Integer × Integer → CubicCoord
(define (cubic-coord x y z)
  (if (not (= (+ x y z) 0))
      (error 'cubic-coord "Cubic coordinates must sum to 0")
      (make-cubic-coord% x y z)))

;;; OFFSET COORDINATES (col, row)
;;;   Used for array storage and display.
;;;   Two variants: odd-r and even-r (for pointy-top)
;;;                 odd-q and even-q (for flat-top)
;;;   We'll use odd-r (odd rows are offset) for pointy-top.

;;; offset-coord : Integer × Integer → OffsetCoord
(define offset-coord coord)

;;; ============================================================
;;; Coordinate Conversions
;;; ============================================================

;;; axial->cubic : AxialCoord → CubicCoord
(define (axial->cubic ac)
  (let ([q (coord-x ac)]
        [r (coord-y ac)])
       (cubic-coord q r (- (- q) r))))

;;; cubic->axial : CubicCoord → AxialCoord
(define (cubic->axial cc)
  (axial-coord (cubic-coord%-x cc) (cubic-coord%-y cc)))

;;; axial->offset : AxialCoord → OffsetCoord (odd-r offset)
(define (axial->offset ac)
  (let* ([q (coord-x ac)]
         [r (coord-y ac)]
         [col (+ q (quotient (+ r (bitwise-and r 1)) 2))]
         [row r])
        (offset-coord col row)))

;;; offset->axial : OffsetCoord → AxialCoord (odd-r offset)
(define (offset->axial oc)
  (let* ([col (coord-x oc)]
         [row (coord-y oc)]
         [q (- col (quotient (+ row (bitwise-and row 1)) 2))]
         [r row])
        (axial-coord q r)))

;;; ============================================================
;;; Hexagonal Neighbors
;;; ============================================================

;;; Hexagons have exactly 6 neighbors.
;;; In axial coordinates, the 6 directions are:
;;;   E: (+1, 0)   SE: (0, +1)   SW: (-1, +1)
;;;   W: (-1, 0)   NW: (0, -1)   NE: (+1, -1)

(define hex-axial-directions
  '((e  .  ( 1 .  0))
    (se .  ( 0 .  1))
    (sw .  (-1 .  1))
    (w  .  (-1 .  0))
    (nw .  ( 0 . -1))
    (ne .  ( 1 . -1))))

;;; hex-neighbor : AxialCoord × Symbol → AxialCoord
;;; Get neighbor in a specific direction
(define (hex-neighbor coord dir)
  (let ([offset (alist-ref hex-axial-directions dir)])
       (if offset
           (axial-coord (+ (coord-x coord) (car offset))
                        (+ (coord-y coord) (cdr offset)))
           (error 'hex-neighbor "Invalid direction" dir))))

;;; hex-neighbors : AxialCoord → (List AxialCoord)
;;; Get all 6 neighbors
(define (hex-neighbors coord)
  (map (lambda (dir) (hex-neighbor coord (car dir)))
       hex-axial-directions))

;;; hex-neighbors-at-distance : AxialCoord × Integer → (List AxialCoord)
;;; Get all hexes at exactly N steps away (ring)
(define (hex-neighbors-at-distance center distance)
  (if (= distance 0)
      (list center)
      (let* ([cc (axial->cubic center)]
             [x (cubic-coord%-x cc)]
             [y (cubic-coord%-y cc)]
             [z (cubic-coord%-z cc)])
            ;; Start at a hex exactly N steps away, then walk around the ring
            (let* ([start-cube (cubic-coord (+ x distance) (- y distance) z)]
                   [directions '((0 -1 1) (-1 0 1) (-1 1 0) (0 1 -1) (1 0 -1) (1 -1 0))])
                  (let loop ([current start-cube] [dir-idx 0] [step 0] [results '()])
                       (if (>= dir-idx 6)
                           (map cubic->axial results)
                           (if (>= step distance)
                               (loop current (+ dir-idx 1) 0 results)
                               (let* ([dir (list-ref directions dir-idx)]
                                      [next (cubic-coord (+ (cubic-coord%-x current) (list-ref dir 0))
                                                         (+ (cubic-coord%-y current) (list-ref dir 1))
                                                         (+ (cubic-coord%-z current) (list-ref dir 2)))])
                                     (loop next dir-idx (+ step 1) (cons current results))))))))))

;;; ============================================================
;;; Distance Metrics
;;; ============================================================

;;; hex-distance : AxialCoord × AxialCoord → Integer
;;; Manhattan distance in hex space (minimum number of hex steps)
;;; This is the natural distance metric for hexagons.
(define (hex-distance c1 c2)
  (let* ([cube1 (axial->cubic c1)]
         [cube2 (axial->cubic c2)]
         [dx (abs (- (cubic-coord%-x cube2) (cubic-coord%-x cube1)))]
         [dy (abs (- (cubic-coord%-y cube2) (cubic-coord%-y cube1)))]
         [dz (abs (- (cubic-coord%-z cube2) (cubic-coord%-z cube1)))])
        (quotient (+ dx dy dz) 2)))

;;; ============================================================
;;; Line Drawing and Range
;;; ============================================================

;;; hex-line : AxialCoord × AxialCoord → (List AxialCoord)
;;; Get hexes on a straight line from c1 to c2
;;; Uses linear interpolation in cubic space
(define (hex-line c1 c2)
  (let* ([cube1 (axial->cubic c1)]
         [cube2 (axial->cubic c2)]
         [N (hex-distance c1 c2)])
        (if (= N 0)
            (list c1)
            (let ([x1 (exact->inexact (cubic-coord%-x cube1))]
                  [y1 (exact->inexact (cubic-coord%-y cube1))]
                  [z1 (exact->inexact (cubic-coord%-z cube1))]
                  [x2 (exact->inexact (cubic-coord%-x cube2))]
                  [y2 (exact->inexact (cubic-coord%-y cube2))]
                  [z2 (exact->inexact (cubic-coord%-z cube2))])
                 (let loop ([i 0] [coords '()])
                      (if (> i N)
                          (reverse coords)
                          (let* ([t (/ i N)]
                                 [x (+ (* x1 (- 1 t)) (* x2 t))]
                                 [y (+ (* y1 (- 1 t)) (* y2 t))]
                                 [z (+ (* z1 (- 1 t)) (* z2 t))]
                                 ;; Round to nearest hex using cubic rounding
                                 [rx (round x)]
                                 [ry (round y)]
                                 [rz (round z)]
                                 [x-diff (abs (- rx x))]
                                 [y-diff (abs (- ry y))]
                                 [z-diff (abs (- rz z))]
                                 ;; Adjust the largest difference to maintain x+y+z=0
                                 [final-cube
                                  (cond
                                   [(and (> x-diff y-diff) (> x-diff z-diff))
                                    (cubic-coord (inexact->exact (- (- ry) rz))
                                                 (inexact->exact ry)
                                                 (inexact->exact rz))]
                                   [(> y-diff z-diff)
                                    (cubic-coord (inexact->exact rx)
                                                 (inexact->exact (- (- rx) rz))
                                                 (inexact->exact rz))]
                                   [else
                                    (cubic-coord (inexact->exact rx)
                                                 (inexact->exact ry)
                                                 (inexact->exact (- (- rx) ry)))])])
                                (loop (+ i 1) (cons (cubic->axial final-cube) coords)))))))))

;;; hex-range : AxialCoord × Integer → (List AxialCoord)
;;; Get all hexes within N steps of center
(define (hex-range center radius)
  (let* ([cc (axial->cubic center)]
         [cx (cubic-coord%-x cc)]
         [cy (cubic-coord%-y cc)]
         [cz (cubic-coord%-z cc)])
        (let loop ([x (- cx radius)] [results '()])
             (if (> x (+ cx radius))
                 (map cubic->axial results)
                 (let ([y-min (max (- cy radius) (- (- x) cz radius))]
                       [y-max (min (+ cy radius) (- (- x) cz (- radius)))])
                      (let inner ([y y-min] [res results])
                           (if (> y y-max)
                               (loop (+ x 1) res)
                               (let ([z (- (- x) y)])
                                    (inner (+ y 1) (cons (cubic-coord x y z) res))))))))))

;;; hex-ring : AxialCoord × Integer → (List AxialCoord)
;;; Get all hexes at exactly N steps from center
(define (hex-ring center radius)
  (hex-neighbors-at-distance center radius))

;;; ============================================================
;;; Rotation and Reflection
;;; ============================================================

;;; hex-rotate-left : AxialCoord × AxialCoord → AxialCoord
;;; Rotate coord 60° counter-clockwise around center
(define (hex-rotate-left center coord)
  (let* ([rel (axial-coord (- (coord-x coord) (coord-x center))
                           (- (coord-y coord) (coord-y center)))]
         [cube (axial->cubic rel)]
         [x (cubic-coord%-x cube)]
         [y (cubic-coord%-y cube)]
         [z (cubic-coord%-z cube)]
         ;; Rotate: (x,y,z) -> (-z,-x,-y)
         [rotated (cubic-coord (- z) (- x) (- y))]
         [result (cubic->axial rotated)])
        (axial-coord (+ (coord-x result) (coord-x center))
                     (+ (coord-y result) (coord-y center)))))

;;; hex-rotate-right : AxialCoord × AxialCoord → AxialCoord
;;; Rotate coord 60° clockwise around center
(define (hex-rotate-right center coord)
  (let* ([rel (axial-coord (- (coord-x coord) (coord-x center))
                           (- (coord-y coord) (coord-y center)))]
         [cube (axial->cubic rel)]
         [x (cubic-coord%-x cube)]
         [y (cubic-coord%-y cube)]
         [z (cubic-coord%-z cube)]
         ;; Rotate: (x,y,z) -> (-y,-z,-x)
         [rotated (cubic-coord (- y) (- z) (- x))]
         [result (cubic->axial rotated)])
        (axial-coord (+ (coord-x result) (coord-x center))
                     (+ (coord-y result) (coord-y center)))))

;;; ============================================================
;;; Board Creation
;;; ============================================================

;;; make-hex-board : Symbol × Integer × [Tile] → Board
;;; Create a hexagonal board with radius N
;;;   coord-system: 'axial or 'offset
;;;   radius: distance from center to edge
(define make-hex-board
  (case-lambda
   [(coord-system radius)
    (make-board 'hex `((coord-system . ,coord-system) (radius . ,radius)))]
   [(coord-system radius default-tile)
    (let ([board (make-board 'hex `((coord-system . ,coord-system) (radius . ,radius)))])
         (let ([hexes (hex-range (axial-coord 0 0) radius)])
              (let loop ([hexes hexes] [b board])
                   (if (null? hexes)
                       b
                       (loop (cdr hexes)
                             (board-set b (car hexes) default-tile))))))]))

;;; hex-board-radius : Board → Integer
(define (hex-board-radius board)
  (alist-ref (board-meta board) 'radius))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; This module provides:
;;;   • axial-coord, cubic-coord, offset-coord — Coordinate constructors
;;;   • axial->cubic, cubic->axial, axial->offset, offset->axial — Conversions
;;;   • hex-neighbor — Get neighbor in direction
;;;   • hex-neighbors — Get all 6 neighbors
;;;   • hex-distance — Minimum steps between hexes
;;;   • hex-line — Line drawing in hex space
;;;   • hex-range — All hexes within distance
;;;   • hex-ring — Hexes at exact distance
;;;   • hex-rotate-left, hex-rotate-right — Rotation operations
;;;   • make-hex-board — Create hexagonal board
