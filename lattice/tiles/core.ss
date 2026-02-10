(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'tiles/core)
(doc 'description "BoardCraft Core Types and Utilities - Common types, protocols, and utilities used across all tile shapes. This is the foundation that specific tile implementations build upon.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'coordinate-protocol)

(doc 'note "A Coordinate is a position on a board. Different tile shapes use different coordinate representations. All coordinates must support: (coord-x coord) → Number, (coord-y coord) → Number, (coord-equal? coord1 coord2) → Boolean, (coord-hash coord) → Integer (for hash tables)")

(define (coord x y)
  (doc 'export #t)
  (doc 'type '(-> Number Number Coordinate))
  (doc 'description "Generic coordinate constructor (used for simple 2D coords)")
  (cons x y))

(define (coord-x c)
  (doc 'type '(-> Coordinate Number))
  (car c))

(define (coord-y c)
  (doc 'type '(-> Coordinate Number))
  (cdr c))

(define (coord-equal? c1 c2)
  (doc 'type '(-> Coordinate Coordinate Boolean))
  (and (= (coord-x c1) (coord-x c2))
       (= (coord-y c1) (coord-y c2))))

(define (coord-hash c)
  (doc 'type '(-> Coordinate Integer))
  (doc 'description "Simple hash combining x and y - works for small boards")
  (+ (* (coord-x c) 1000000) (coord-y c)))

(doc 'section 'tile-protocol)

(doc 'note "A Tile represents game state at a board position. Tiles are opaque to the SDK; games define their structure. The SDK only cares about tiles for: Pathfinding (is tile walkable?), Movement cost (how expensive to enter?), Blocking (does tile block line of sight?). Games implement these predicates: (tile-walkable? tile) → Boolean, (tile-cost tile) → Number (default 1), (tile-blocks? tile) → Boolean (default #f)")

(define (make-tile type data)
  (doc 'export #t)
  (doc 'type '(-> Symbol Alist Tile))
  (doc 'description "Default tile implementation: just a symbol or data structure")
  (cons type data))

(define (tile-type tile)
  (doc 'type '(-> Tile Symbol))
  (car tile))

(define (tile-data tile)
  (doc 'type '(-> Tile Alist))
  (cdr tile))

(define (tile-walkable? tile)
  (doc 'type '(-> Tile Boolean))
  (doc 'description "Default walkable predicate - can be overridden by games")
  (not (eq? (tile-type tile) 'wall)))

(define (tile-cost tile)
  (doc 'type '(-> Tile Number))
  (doc 'description "Default cost function - can be overridden by games")
  (let ([data (tile-data tile)])
       (if (and (pair? data) (assq 'cost data))
           (cdr (assq 'cost data))
           1)))

(define (tile-blocks? tile)
  (doc 'type '(-> Tile Boolean))
  (doc 'description "Default blocking predicate - can be overridden by games")
  (eq? (tile-type tile) 'wall))

(define (tile-get-prop tile prop default)
  (doc 'type '(-> Tile Symbol Any Any))
  (doc 'description "Get a property from tile data with default value")
  (let ([data (tile-data tile)])
       (if (and (pair? data) (assq prop data))
           (cdr (assq prop data))
           default)))

(doc 'section 'board-protocol)

(doc 'note "A Board is a mapping from Coordinates to Tiles. Boards are immutable hash tables. Operations: (make-board shape-type) → Board, (board-get board coord) → Tile | #f, (board-set board coord tile) → Board, (board-remove board coord) → Board, (board-tiles board) → (List (Coord . Tile)), (board-coords board) → (List Coord), (board-shape board) → Symbol")

(define-record-type board%
  (fields shape
          tiles
          meta))

(doc make-board 'type '(-> Symbol Alist Board))
(doc make-board 'description "Create a board with optional custom hash/equality functions")
(doc make-board 'export #t)
(define make-board
  (case-lambda
   [(shape-type meta)
    (make-board% shape-type (make-hashtable coord-hash coord-equal?) meta)]
   [(shape-type meta hash-func eq-func)
    (make-board% shape-type (make-hashtable hash-func eq-func) meta)]))

(define (board-shape b)
  (doc 'type '(-> Board Symbol))
  (board%-shape b))

(define (board-meta b)
  (doc 'type '(-> Board Alist))
  (board%-meta b))

(define (board-get board coord)
  (doc 'export #t)
  (doc 'type '(-> Board Coordinate (Maybe Tile)))
  (hashtable-ref (board%-tiles board) coord #f))

(define (board-set board coord tile)
  (doc 'export #t)
  (doc 'type '(-> Board Coordinate Tile Board))
  (let ([new-tiles (hashtable-copy (board%-tiles board) #t)])
       (hashtable-set! new-tiles coord tile)
       (make-board% (board-shape board) new-tiles (board-meta board))))

(define (board-remove board coord)
  (doc 'type '(-> Board Coordinate Board))
  (let ([new-tiles (hashtable-copy (board%-tiles board) #t)])
       (hashtable-delete! new-tiles coord)
       (make-board% (board-shape board) new-tiles (board-meta board))))

(define (board-tiles board)
  (doc 'type '(-> Board (List (Pair Coordinate Tile))))
  (let ([ht (board%-tiles board)])
       (map (lambda (coord)
                    (cons coord (hashtable-ref ht coord #f)))
            (vector->list (hashtable-keys ht)))))

(define (board-coords board)
  (doc 'export #t)
  (doc 'type '(-> Board (List Coordinate)))
  (map car (board-tiles board)))

(define (board-size board)
  (doc 'export #t)
  (doc 'type '(-> Board Integer))
  (doc 'description "Returns the number of tiles currently stored on the board. NOTE: This counts stored tiles, not theoretical capacity. For an empty sparse board (created without default-tile), this returns 0. For geometric capacity (e.g., how many hexes fit in the radius), use shape-specific functions like hex-board-capacity.")
  (hashtable-size (board%-tiles board)))

(define (board-empty? board)
  (doc 'type '(-> Board Boolean))
  (doc 'description "Returns #t if no tiles are stored on the board")
  (= (board-size board) 0))

(doc 'section 'neighbor-protocol)

(doc 'note "Different tile shapes have different neighbor patterns. Each shape module provides: (shape-neighbors coord) → (List Coord), (shape-distance coord1 coord2) → Number. This protocol allows generic algorithms to work with any shape.")

(doc 'section 'direction-types)

(doc 'note "Directions are symbolic representations of movement. Different shapes support different direction sets.")

(doc square-directions-8 'description "Square directions (8-way)")
(define square-directions-8
  '(n ne e se s sw w nw))

(doc square-directions-4 'description "Square directions (4-way, orthogonal only)")
(define square-directions-4
  '(n e s w))

(doc hex-directions-pointy 'description "Hex directions (6-way, pointy-top)")
(define hex-directions-pointy
  '(ne e se sw w nw))

(doc hex-directions-flat 'description "Hex directions (6-way, flat-top)")
(define hex-directions-flat
  '(n ne se s sw nw))

(doc triangle-directions 'description "Triangle directions (3-way for edge neighbors)")
(define triangle-directions
  '(e se sw))

(doc 'section 'utilities)

(define (clamp value min-val max-val)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "Clamp value between min and max")
  (max min-val (min max-val value)))

(define (sign n)
  (doc 'type '(-> Number Number))
  (doc 'description "Return sign of number (-1, 0, or 1)")
  (cond [(> n 0) 1]
        [(< n 0) -1]
        [else 0]))

(define (manhattan-distance x1 y1 x2 y2)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number Number))
  (doc 'description "Manhattan distance (L1 metric)")
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(define (chebyshev-distance x1 y1 x2 y2)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number Number))
  (doc 'description "Chebyshev distance (L∞ metric, king's move in chess)")
  (max (abs (- x2 x1)) (abs (- y2 y1))))

(define (euclidean-distance x1 y1 x2 y2)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number Number))
  (doc 'description "Euclidean distance (L2 metric)")
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

(doc 'section 'list-utilities)

;;; filter-map provided by prelude

(define (deduplicate lst equal?)
  (doc 'type '(-> (List a) (-> a a Boolean) (List a)))
  (doc 'description "Remove duplicates from list using equality predicate")
  (let loop ([lst lst] [seen '()])
       (if (null? lst)
           (reverse seen)
           (if (memp (lambda (x) (equal? x (car lst))) seen)
               (loop (cdr lst) seen)
               (loop (cdr lst) (cons (car lst) seen))))))

(doc alist-ref 'type '(-> Alist Symbol (Maybe Any)))
(doc alist-ref 'description "Get value from association list")
(define alist-ref
  (case-lambda
   [(alist key)
    (let ([pair (assq key alist)])
         (if pair (cdr pair) #f))]
   [(alist key default)
    (let ([pair (assq key alist)])
         (if pair (cdr pair) default))]))

(doc 'section 'exports)
(doc 'note "This module provides: Coordinate protocol (coord, coord-x, coord-y, coord-equal?, coord-hash), Tile protocol (make-tile, tile-type, tile-data, tile-walkable?, etc.), Board type and operations (make-board, board-get, board-set, etc.), Direction constants for different shapes, Common distance metrics, Utility functions for list processing. Shape-specific modules (square.ss, hex.ss, etc.) build on these primitives.")
