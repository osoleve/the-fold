(load "core/testing/test-framework.ss")
(load "lattice/tiles/core.ss")
(load "lattice/tiles/hex.ss")
(load "lattice/tiles/square.ss")

(doc 'module 'test-tiles)
(doc 'description "Tests for tiles core, hex, and square modules")

;;; ====
;;; Core: Coordinates
;;; ====

(test-group coordinate-protocol

  (define-test "coord creates pair"
    (let ([c (coord 3 7)])
      (assert-equal 3 (coord-x c))
      (assert-equal 7 (coord-y c))))

  (define-test "coord-equal? on equal coords"
    (assert-true (coord-equal? (coord 2 5) (coord 2 5))))

  (define-test "coord-equal? on different coords"
    (assert-false (coord-equal? (coord 2 5) (coord 3 5))))

  (define-test "coord-hash produces integer"
    (assert-true (integer? (coord-hash (coord 1 2)))))

  (define-test "coord-hash consistent for equal coords"
    (assert-equal (coord-hash (coord 4 9))
                  (coord-hash (coord 4 9))))

  (define-test "coord-hash differs for different coords"
    (assert-false (= (coord-hash (coord 1 2))
                     (coord-hash (coord 2 1))))))

;;; ====
;;; Core: Tiles
;;; ====

(test-group tile-protocol

  (define-test "make-tile creates tile with type"
    (let ([t (make-tile 'grass '())])
      (assert-equal 'grass (tile-type t))))

  (define-test "tile-walkable? true for non-wall"
    (assert-true (tile-walkable? (make-tile 'grass '()))))

  (define-test "tile-walkable? false for wall"
    (assert-false (tile-walkable? (make-tile 'wall '()))))

  (define-test "tile-cost defaults to 1"
    (assert-equal 1 (tile-cost (make-tile 'grass '()))))

  (define-test "tile-cost reads from data"
    (assert-equal 3 (tile-cost (make-tile 'swamp '((cost . 3))))))

  (define-test "tile-blocks? true for wall"
    (assert-true (tile-blocks? (make-tile 'wall '()))))

  (define-test "tile-blocks? false for non-wall"
    (assert-false (tile-blocks? (make-tile 'forest '()))))

  (define-test "tile-get-prop with existing prop"
    (let ([t (make-tile 'forest '((height . 10) (cover . high)))])
      (assert-equal 10 (tile-get-prop t 'height 0))))

  (define-test "tile-get-prop with default"
    (let ([t (make-tile 'forest '())])
      (assert-equal 42 (tile-get-prop t 'height 42)))))

;;; ====
;;; Core: Board
;;; ====

(test-group board-operations

  (define-test "make-board creates empty board"
    (let ([b (make-board 'hex '())])
      (assert-true (board-empty? b))
      (assert-equal 0 (board-size b))))

  (define-test "board-set adds tile"
    (let* ([b (make-board 'square '())]
           [b2 (board-set b (coord 0 0) (make-tile 'grass '()))])
      (assert-equal 1 (board-size b2))))

  (define-test "board-get retrieves tile"
    (let* ([t (make-tile 'forest '())]
           [b (board-set (make-board 'square '()) (coord 3 4) t)])
      (assert-equal 'forest (tile-type (board-get b (coord 3 4))))))

  (define-test "board-get returns #f for empty coord"
    (let ([b (make-board 'square '())])
      (assert-false (board-get b (coord 5 5)))))

  (define-test "board-remove removes tile"
    (let* ([b (board-set (make-board 'hex '()) (coord 0 0) (make-tile 'grass '()))]
           [b2 (board-remove b (coord 0 0))])
      (assert-true (board-empty? b2))))

  (define-test "board-set is immutable (original unchanged)"
    (let* ([b1 (make-board 'square '())]
           [b2 (board-set b1 (coord 0 0) (make-tile 'grass '()))])
      (assert-equal 0 (board-size b1))
      (assert-equal 1 (board-size b2))))

  (define-test "board-shape returns shape"
    (assert-equal 'hex (board-shape (make-board 'hex '()))))

  (define-test "board-coords returns coordinate list"
    (let* ([b (make-board 'square '())]
           [b (board-set b (coord 1 1) (make-tile 'grass '()))]
           [b (board-set b (coord 2 2) (make-tile 'grass '()))])
      (assert-equal 2 (length (board-coords b)))))

  (define-test "board-tiles returns coord-tile pairs"
    (let* ([b (board-set (make-board 'hex '()) (coord 0 0) (make-tile 'water '()))]
           [tiles (board-tiles b)])
      (assert-equal 1 (length tiles))
      (assert-equal 'water (tile-type (cdar tiles))))))

;;; ====
;;; Core: Utilities
;;; ====

(test-group core-utilities

  (define-test "clamp within bounds"
    (assert-equal 5 (clamp 5 0 10)))

  (define-test "clamp below minimum"
    (assert-equal 0 (clamp -3 0 10)))

  (define-test "clamp above maximum"
    (assert-equal 10 (clamp 15 0 10)))

  (define-test "sign positive"
    (assert-equal 1 (sign 42)))

  (define-test "sign negative"
    (assert-equal -1 (sign -7)))

  (define-test "sign zero"
    (assert-equal 0 (sign 0)))

  (define-test "manhattan-distance"
    (assert-equal 7 (manhattan-distance 1 2 4 6)))

  (define-test "chebyshev-distance"
    (assert-equal 4 (chebyshev-distance 1 2 4 6)))

  (define-test "euclidean-distance"
    (assert-equal 5 (euclidean-distance 0 0 3 4)))

  (define-test "deduplicate removes duplicates"
    (assert-equal '(1 2 3) (deduplicate '(1 2 2 3 1 3) =)))

  (define-test "deduplicate preserves order"
    (assert-equal '(3 1 2) (deduplicate '(3 1 2 1 3) =)))

  (define-test "alist-ref finds key"
    (assert-equal 42 (alist-ref '((a . 1) (b . 42)) 'b)))

  (define-test "alist-ref returns #f when missing"
    (assert-false (alist-ref '((a . 1)) 'z)))

  (define-test "alist-ref uses default"
    (assert-equal 99 (alist-ref '((a . 1)) 'z 99))))

;;; ====
;;; Hex: Coordinates
;;; ====

(test-group hex-coordinates

  (define-test "axial-coord creates coordinate"
    (let ([c (axial-coord 2 -1)])
      (assert-equal 2 (coord-x c))
      (assert-equal -1 (coord-y c))))

  (define-test "cubic-coord enforces x+y+z=0"
    (assert-error (lambda () (cubic-coord 1 2 3))))

  (define-test "cubic-coord valid creation"
    (let ([c (cubic-coord 1 -2 1)])
      (assert-equal 1 (cubic-coord%-x c))
      (assert-equal -2 (cubic-coord%-y c))
      (assert-equal 1 (cubic-coord%-z c))))

  (define-test "axial->cubic roundtrip"
    (let* ([a (axial-coord 3 -1)]
           [c (axial->cubic a)]
           [a2 (cubic->axial c)])
      (assert-true (coord-equal? a a2))))

  (define-test "axial->offset->axial roundtrip"
    (let* ([a (axial-coord 2 3)]
           [o (axial->offset a)]
           [a2 (offset->axial o)])
      (assert-true (coord-equal? a a2))))

  (define-test "origin converts cleanly"
    (let* ([a (axial-coord 0 0)]
           [c (axial->cubic a)])
      (assert-equal 0 (cubic-coord%-x c))
      (assert-equal 0 (cubic-coord%-y c))
      (assert-equal 0 (cubic-coord%-z c)))))

;;; ====
;;; Hex: Neighbors
;;; ====

(test-group hex-neighbors

  (define-test "hex-neighbors returns 6"
    (assert-equal 6 (length (hex-neighbors (axial-coord 0 0)))))

  (define-test "hex-neighbor east"
    (let ([n (hex-neighbor (axial-coord 0 0) 'e)])
      (assert-true (coord-equal? (axial-coord 1 0) n))))

  (define-test "hex-neighbor southwest"
    (let ([n (hex-neighbor (axial-coord 0 0) 'sw)])
      (assert-true (coord-equal? (axial-coord -1 1) n))))

  (define-test "hex-neighbor invalid direction"
    (assert-error (lambda () (hex-neighbor (axial-coord 0 0) 'north))))

  (define-test "neighbors are at distance 1"
    (let ([origin (axial-coord 0 0)]
          [neighbors (hex-neighbors (axial-coord 0 0))])
      (assert-true (for-all (lambda (n) (= 1 (hex-distance origin n)))
                            neighbors)))))

;;; ====
;;; Hex: Distance
;;; ====

(test-group hex-distance-tests

  (define-test "distance to self is 0"
    (assert-equal 0 (hex-distance (axial-coord 3 -1) (axial-coord 3 -1))))

  (define-test "distance to neighbor is 1"
    (assert-equal 1 (hex-distance (axial-coord 0 0) (axial-coord 1 0))))

  (define-test "distance is symmetric"
    (let ([a (axial-coord 1 2)]
          [b (axial-coord -2 3)])
      (assert-equal (hex-distance a b) (hex-distance b a))))

  (define-test "distance across origin"
    (assert-equal 4 (hex-distance (axial-coord 2 0) (axial-coord -2 0)))))

;;; ====
;;; Hex: Line and Range
;;; ====

(test-group hex-line-range

  (define-test "hex-line to self is single point"
    (assert-equal 1 (length (hex-line (axial-coord 0 0) (axial-coord 0 0)))))

  (define-test "hex-line length matches distance + 1"
    (let* ([a (axial-coord 0 0)]
           [b (axial-coord 3 0)]
           [line (hex-line a b)])
      (assert-equal 4 (length line))))

  (define-test "hex-line endpoints are correct"
    (let* ([a (axial-coord 0 0)]
           [b (axial-coord 2 0)]
           [line (hex-line a b)])
      (assert-true (coord-equal? a (car line)))
      (assert-true (coord-equal? b (car (reverse line))))))

  (define-test "hex-range radius 0 returns center"
    (let ([r (hex-range (axial-coord 0 0) 0)])
      (assert-equal 1 (length r))))

  (define-test "hex-range radius 1 returns 7 hexes"
    ;; 1 center + 6 neighbors = 7
    (assert-equal 7 (length (hex-range (axial-coord 0 0) 1))))

  (define-test "hex-range radius 2 returns 19 hexes"
    ;; 1 + 6 + 12 = 19
    (assert-equal 19 (length (hex-range (axial-coord 0 0) 2))))

  (define-test "hex-ring radius 1 returns 6 hexes"
    (assert-equal 6 (length (hex-ring (axial-coord 0 0) 1))))

  (define-test "hex-ring radius 2 returns 12 hexes"
    (assert-equal 12 (length (hex-ring (axial-coord 0 0) 2))))

  (define-test "hex-ring radius 0 returns center"
    (assert-equal 1 (length (hex-ring (axial-coord 0 0) 0)))))

;;; ====
;;; Hex: Rotation
;;; ====

(test-group hex-rotation

  (define-test "rotate-left 6 times returns to start"
    (let* ([center (axial-coord 0 0)]
           [point (axial-coord 2 0)]
           [result (let loop ([p point] [i 0])
                     (if (= i 6) p
                         (loop (hex-rotate-left center p) (+ i 1))))])
      (assert-true (coord-equal? point result))))

  (define-test "rotate-right 6 times returns to start"
    (let* ([center (axial-coord 0 0)]
           [point (axial-coord 1 -1)]
           [result (let loop ([p point] [i 0])
                     (if (= i 6) p
                         (loop (hex-rotate-right center p) (+ i 1))))])
      (assert-true (coord-equal? point result))))

  (define-test "rotate-left then rotate-right is identity"
    (let* ([center (axial-coord 0 0)]
           [point (axial-coord 2 -1)]
           [rotated (hex-rotate-right center (hex-rotate-left center point))])
      (assert-true (coord-equal? point rotated))))

  (define-test "rotation preserves distance"
    (let* ([center (axial-coord 0 0)]
           [point (axial-coord 3 -1)]
           [rotated (hex-rotate-left center point)])
      (assert-equal (hex-distance center point)
                    (hex-distance center rotated)))))

;;; ====
;;; Hex: Board Creation
;;; ====

(test-group hex-board-creation

  (define-test "make-hex-board creates empty board"
    (let ([b (make-hex-board 'axial 3)])
      (assert-true (board-empty? b))
      (assert-equal 'hex (board-shape b))))

  (define-test "make-hex-board with default tile"
    (let ([b (make-hex-board 'axial 2 (make-tile 'grass '()))])
      (assert-equal 19 (board-size b))))

  (define-test "hex-board-radius returns radius"
    (let ([b (make-hex-board 'axial 5)])
      (assert-equal 5 (hex-board-radius b))))

  (define-test "hex-board-capacity formula"
    ;; 1 + 3*n*(n+1): r=2 → 1+3*2*3 = 19
    (let ([b (make-hex-board 'axial 2)])
      (assert-equal 19 (hex-board-capacity b)))))

;;; ====
;;; Square: Coordinates
;;; ====

(test-group square-coordinates

  (define-test "square-coord creates coordinate"
    (let ([c (square-coord 5 3)])
      (assert-equal 5 (coord-x c))
      (assert-equal 3 (coord-y c)))))

;;; ====
;;; Square: Neighbors
;;; ====

(test-group square-neighbors

  (define-test "square-neighbors ortho returns 4"
    (assert-equal 4 (length (square-neighbors (square-coord 5 5) 'ortho))))

  (define-test "square-neighbors diag returns 4"
    (assert-equal 4 (length (square-neighbors (square-coord 5 5) 'diag))))

  (define-test "square-neighbors all returns 8"
    (assert-equal 8 (length (square-neighbors (square-coord 5 5) 'all))))

  (define-test "square-neighbors invalid mode"
    (assert-error (lambda () (square-neighbors (square-coord 0 0) 'invalid))))

  (define-test "ortho neighbors are at manhattan distance 1"
    (let* ([c (square-coord 3 3)]
           [ns (square-neighbors c 'ortho)])
      (assert-true (for-all (lambda (n)
                              (= 1 (square-distance-manhattan c n)))
                            ns))))

  (define-test "north neighbor is correct"
    (let ([n (square-neighbor (square-coord 5 5) 'n)])
      (assert-true (coord-equal? (square-coord 5 4) n))))

  (define-test "east neighbor is correct"
    (let ([n (square-neighbor (square-coord 5 5) 'e)])
      (assert-true (coord-equal? (square-coord 6 5) n)))))

;;; ====
;;; Square: Distance
;;; ====

(test-group square-distance

  (define-test "manhattan distance"
    (assert-equal 6 (square-distance-manhattan (square-coord 1 1) (square-coord 4 4))))

  (define-test "chebyshev distance"
    (assert-equal 3 (square-distance-chebyshev (square-coord 1 1) (square-coord 4 4))))

  (define-test "euclidean distance"
    (assert-equal 5 (square-distance-euclidean (square-coord 0 0) (square-coord 3 4))))

  (define-test "generic distance manhattan"
    (assert-equal 6 (square-distance (square-coord 1 1) (square-coord 4 4) 'manhattan)))

  (define-test "distance to self is 0"
    (assert-equal 0 (square-distance-manhattan (square-coord 3 3) (square-coord 3 3))))

  (define-test "distance is symmetric"
    (let ([a (square-coord 1 2)] [b (square-coord 5 7)])
      (assert-equal (square-distance-manhattan a b)
                    (square-distance-manhattan b a)))))

;;; ====
;;; Square: Line
;;; ====

(test-group square-line

  (define-test "line to self is single point"
    (assert-equal 1 (length (square-line (square-coord 3 3) (square-coord 3 3)))))

  (define-test "horizontal line"
    (let ([line (square-line (square-coord 0 0) (square-coord 3 0))])
      (assert-equal 4 (length line))
      (assert-true (coord-equal? (square-coord 0 0) (car line)))
      (assert-true (coord-equal? (square-coord 3 0) (car (reverse line))))))

  (define-test "vertical line"
    (let ([line (square-line (square-coord 2 0) (square-coord 2 4))])
      (assert-equal 5 (length line))))

  (define-test "diagonal line"
    (let ([line (square-line (square-coord 0 0) (square-coord 3 3))])
      (assert-equal 4 (length line)))))

;;; ====
;;; Square: Range and Ring
;;; ====

(test-group square-range-ring

  (define-test "manhattan range radius 1 = diamond of 5"
    (assert-equal 5 (length (square-range (square-coord 5 5) 1 'manhattan))))

  (define-test "chebyshev range radius 1 = 3x3 square of 9"
    (assert-equal 9 (length (square-range (square-coord 5 5) 1 'chebyshev))))

  (define-test "range radius 0 = center only"
    (assert-equal 1 (length (square-range (square-coord 0 0) 0 'manhattan))))

  (define-test "square-ring radius 0 returns center"
    (assert-equal 1 (length (square-ring (square-coord 3 3) 0))))

  (define-test "square-ring radius 1 returns 8 squares"
    (assert-equal 8 (length (square-ring (square-coord 3 3) 1)))))

;;; ====
;;; Square: Board Creation
;;; ====

(test-group square-board-creation

  (define-test "make-square-board creates empty board"
    (let ([b (make-square-board 5 3)])
      (assert-true (board-empty? b))
      (assert-equal 'square (board-shape b))))

  (define-test "make-square-board with default tile fills grid"
    (let ([b (make-square-board 4 3 (make-tile 'grass '()))])
      (assert-equal 12 (board-size b))))

  (define-test "square-board-width returns width"
    (assert-equal 8 (square-board-width (make-square-board 8 6))))

  (define-test "square-board-height returns height"
    (assert-equal 6 (square-board-height (make-square-board 8 6))))

  (define-test "square-in-bounds? inside"
    (let ([b (make-square-board 5 5)])
      (assert-true (square-in-bounds? b (square-coord 2 3)))))

  (define-test "square-in-bounds? outside"
    (let ([b (make-square-board 5 5)])
      (assert-false (square-in-bounds? b (square-coord 5 5)))))

  (define-test "square-in-bounds? negative"
    (let ([b (make-square-board 5 5)])
      (assert-false (square-in-bounds? b (square-coord -1 0)))))

  (define-test "square-board-capacity"
    (assert-equal 24 (square-board-capacity (make-square-board 6 4)))))

;;; ====
;;; Direction Constants
;;; ====

(test-group direction-constants

  (define-test "square-directions-8 has 8 entries"
    (assert-equal 8 (length square-directions-8)))

  (define-test "square-directions-4 has 4 entries"
    (assert-equal 4 (length square-directions-4)))

  (define-test "hex-directions-pointy has 6 entries"
    (assert-equal 6 (length hex-directions-pointy)))

  (define-test "hex-directions-flat has 6 entries"
    (assert-equal 6 (length hex-directions-flat))))

(run-all-tests-and-exit)
