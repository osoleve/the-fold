;;; playpen/boardcraft/core.ss — BoardCraft Core Types and Utilities
;;;
;;; Common types, protocols, and utilities used across all tile shapes.
;;; This is the foundation that specific tile implementations build upon.

;;; ============================================================
;;; Coordinate Protocol
;;; ============================================================

;;; A Coordinate is a position on a board.
;;; Different tile shapes use different coordinate representations.
;;;
;;; All coordinates must support:
;;;   (coord-x coord) → Number
;;;   (coord-y coord) → Number
;;;   (coord-equal? coord1 coord2) → Boolean
;;;   (coord-hash coord) → Integer (for hash tables)

;;; coord : Number × Number → Coordinate
;;; Generic coordinate constructor (used for simple 2D coords)
(define (coord x y)
  (cons x y))

(define (coord-x c) (car c))
(define (coord-y c) (cdr c))

(define (coord-equal? c1 c2)
  (and (= (coord-x c1) (coord-x c2))
       (= (coord-y c1) (coord-y c2))))

(define (coord-hash c)
  ;; Simple hash combining x and y
  ;; This works for small boards; for large boards, use better hash
  (+ (* (coord-x c) 1000000) (coord-y c)))

;;; ============================================================
;;; Tile Protocol
;;; ============================================================

;;; A Tile represents game state at a board position.
;;; Tiles are opaque to the SDK; games define their structure.
;;;
;;; The SDK only cares about tiles for:
;;;   - Pathfinding (is tile walkable?)
;;;   - Movement cost (how expensive to enter?)
;;;   - Blocking (does tile block line of sight?)
;;;
;;; Games implement these predicates:
;;;   (tile-walkable? tile) → Boolean
;;;   (tile-cost tile) → Number (default 1)
;;;   (tile-blocks? tile) → Boolean (default #f)

;;; Default tile implementation: just a symbol or data structure
(define (make-tile type data)
  (cons type data))

(define (tile-type tile) (car tile))
(define (tile-data tile) (cdr tile))

;;; Default predicates (can be overridden by games)
(define (tile-walkable? tile)
  (not (eq? (tile-type tile) 'wall)))

(define (tile-cost tile)
  (let ([data (tile-data tile)])
       (if (and (pair? data) (assq 'cost data))
           (cdr (assq 'cost data))
           1)))

(define (tile-blocks? tile)
  (eq? (tile-type tile) 'wall))

;;; tile-get-prop : Tile × Symbol × Any → Any
;;; Get a property from tile data with default value
(define (tile-get-prop tile prop default)
  (let ([data (tile-data tile)])
       (if (and (pair? data) (assq prop data))
           (cdr (assq prop data))
           default)))

;;; ============================================================
;;; Board Protocol
;;; ============================================================

;;; A Board is a mapping from Coordinates to Tiles.
;;; Boards are immutable hash tables.
;;;
;;; Operations:
;;;   (make-board shape-type) → Board
;;;   (board-get board coord) → Tile | #f
;;;   (board-set board coord tile) → Board
;;;   (board-remove board coord) → Board
;;;   (board-tiles board) → (List (Coord . Tile))
;;;   (board-coords board) → (List Coord)
;;;   (board-shape board) → Symbol

(define-record-type board%
  (fields shape    ; Symbol: 'square, 'hex, 'triangle, etc.
          tiles    ; HashTable: Coord -> Tile
          meta))   ; Alist: metadata (dimensions, etc.)

;;; make-board : Symbol × Alist × [HashFunc] × [EqFunc] → Board
;;; Create a board with optional custom hash/equality functions
(define make-board
  (case-lambda
   [(shape-type meta)
    (make-board% shape-type (make-hashtable coord-hash coord-equal?) meta)]
   [(shape-type meta hash-func eq-func)
    (make-board% shape-type (make-hashtable hash-func eq-func) meta)]))

(define (board-shape b) (board%-shape b))
(define (board-meta b) (board%-meta b))

(define (board-get board coord)
  (hashtable-ref (board%-tiles board) coord #f))

(define (board-set board coord tile)
  (let ([new-tiles (hashtable-copy (board%-tiles board) #t)])
       (hashtable-set! new-tiles coord tile)
       (make-board% (board-shape board) new-tiles (board-meta board))))

(define (board-remove board coord)
  (let ([new-tiles (hashtable-copy (board%-tiles board) #t)])
       (hashtable-delete! new-tiles coord)
       (make-board% (board-shape board) new-tiles (board-meta board))))

(define (board-tiles board)
  (let ([ht (board%-tiles board)])
       (map (lambda (coord)
                    (cons coord (hashtable-ref ht coord #f)))
            (vector->list (hashtable-keys ht)))))

(define (board-coords board)
  (map car (board-tiles board)))

(define (board-size board)
  (hashtable-size (board%-tiles board)))

;;; ============================================================
;;; Neighbor Protocol
;;; ============================================================

;;; Different tile shapes have different neighbor patterns.
;;; Each shape module provides:
;;;   (shape-neighbors coord) → (List Coord)
;;;   (shape-distance coord1 coord2) → Number
;;;
;;; This protocol allows generic algorithms to work with any shape.

;;; ============================================================
;;; Direction Types
;;; ============================================================

;;; Directions are symbolic representations of movement.
;;; Different shapes support different direction sets.

;;; Square directions (8-way)
(define square-directions-8
  '(n ne e se s sw w nw))

;;; Square directions (4-way, orthogonal only)
(define square-directions-4
  '(n e s w))

;;; Hex directions (6-way)
(define hex-directions-pointy
  '(ne e se sw w nw))

(define hex-directions-flat
  '(n ne se s sw nw))

;;; Triangle directions (3-way for edge neighbors)
(define triangle-directions
  '(e se sw))

;;; ============================================================
;;; Common Utilities
;;; ============================================================

;;; clamp : Number × Number × Number → Number
;;; Clamp value between min and max
(define (clamp value min-val max-val)
  (max min-val (min max-val value)))

;;; sign : Number → Number
;;; Return sign of number (-1, 0, or 1)
(define (sign n)
  (cond [(> n 0) 1]
        [(< n 0) -1]
        [else 0]))

;;; manhattan-distance : Number × Number × Number × Number → Number
;;; Manhattan distance (L1 metric)
(define (manhattan-distance x1 y1 x2 y2)
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

;;; chebyshev-distance : Number × Number × Number × Number → Number
;;; Chebyshev distance (L∞ metric, king's move in chess)
(define (chebyshev-distance x1 y1 x2 y2)
  (max (abs (- x2 x1)) (abs (- y2 y1))))

;;; euclidean-distance : Number × Number × Number × Number → Number
;;; Euclidean distance (L2 metric)
(define (euclidean-distance x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

;;; ============================================================
;;; List Utilities
;;; ============================================================

;;; filter-map : (A → B | #f) × (List A) → (List B)
;;; Map function over list, filtering out #f results
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; deduplicate : (List A) × (A × A → Bool) → (List A)
;;; Remove duplicates from list using equality predicate
(define (deduplicate lst equal?)
  (let loop ([lst lst] [seen '()])
       (if (null? lst)
           (reverse seen)
           (if (memp (lambda (x) (equal? x (car lst))) seen)
               (loop (cdr lst) seen)
               (loop (cdr lst) (cons (car lst) seen))))))

;;; alist-ref : Alist × Key × [Default] → Value
;;; Get value from association list
(define alist-ref
  (case-lambda
   [(alist key)
    (let ([pair (assq key alist)])
         (if pair (cdr pair) #f))]
   [(alist key default)
    (let ([pair (assq key alist)])
         (if pair (cdr pair) default))]))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; This module provides:
;;;   • Coordinate protocol (coord, coord-x, coord-y, coord-equal?, coord-hash)
;;;   • Tile protocol (make-tile, tile-type, tile-data, tile-walkable?, etc.)
;;;   • Board type and operations (make-board, board-get, board-set, etc.)
;;;   • Direction constants for different shapes
;;;   • Common distance metrics
;;;   • Utility functions for list processing
;;;
;;; Shape-specific modules (square.ss, hex.ss, etc.) build on these primitives.
