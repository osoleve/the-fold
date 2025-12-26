;;; playpen/rpg/tile.ss — Tile System for RPG SDK
;;;
;;; Defines tiles, tile types, and tilemaps for 2D grid-based games.
;;; Tiles have visual representation, movement properties, and metadata.
;;;
;;; Dependencies:
;;;   - playpen/rpg/core.ss
;;;   - shell/layout.ss (canvas, point)
;;;
;;; Exports:
;;;   Tile types and definitions
;;;   TileMap creation and manipulation
;;;   Tile rendering to canvas

;;; ============================================================
;;; Tile Type Definitions
;;; ============================================================

;;; TileType : Record
;;;   id        — Symbol, unique identifier
;;;   char      — Char, visual representation
;;;   name      — String, human-readable name
;;;   walkable  — Bool, can entities walk through?
;;;   opaque    — Bool, does it block line of sight?
;;;   cost      — Nat, movement cost (1 = normal, higher = slower)
;;;   properties — Alist, additional custom properties

(define-record-type tile-type%
  (fields id char name walkable opaque cost properties))

;;; make-tile-type : Symbol × Char × String × Bool × Bool × [Nat] × [Alist] -> TileType
(define make-tile-type
  (case-lambda
    [(id char name walkable opaque)
     (make-tile-type% id char name walkable opaque 1 '())]
    [(id char name walkable opaque cost)
     (make-tile-type% id char name walkable opaque cost '())]
    [(id char name walkable opaque cost props)
     (make-tile-type% id char name walkable opaque cost props)]))

;;; Accessors
(define tile-type-id tile-type%-id)
(define tile-type-char tile-type%-char)
(define tile-type-name tile-type%-name)
(define tile-type-walkable tile-type%-walkable)
(define tile-type-opaque tile-type%-opaque)
(define tile-type-cost tile-type%-cost)
(define tile-type-properties tile-type%-properties)

;;; tile-type-property : TileType × Symbol × [Any] -> Any
;;; Get a custom property from a tile type.
(define tile-type-property
  (case-lambda
    [(tt key)
     (alist-ref (tile-type-properties tt) key)]
    [(tt key default)
     (alist-ref (tile-type-properties tt) key default)]))

;;; ============================================================
;;; Standard Tile Types
;;; ============================================================

;;; A collection of common tile types for dungeon crawlers.
;;; Games can define additional types or override these.

(define tile-floor
  (make-tile-type 'floor #\. "Floor" #t #f 1))

(define tile-wall
  (make-tile-type 'wall #\# "Wall" #f #t 999))

(define tile-door-closed
  (make-tile-type 'door-closed #\+ "Closed Door" #f #t 1
                  '((openable . #t))))

(define tile-door-open
  (make-tile-type 'door-open #\' "Open Door" #t #f 1
                  '((closeable . #t))))

(define tile-stairs-down
  (make-tile-type 'stairs-down #\> "Stairs Down" #t #f 1
                  '((stairs . down))))

(define tile-stairs-up
  (make-tile-type 'stairs-up #\< "Stairs Up" #t #f 1
                  '((stairs . up))))

(define tile-water
  (make-tile-type 'water #\~ "Water" #f #f 999
                  '((swimmable . #t))))

(define tile-shallow-water
  (make-tile-type 'shallow-water #\~ "Shallow Water" #t #f 2
                  '((wet . #t))))

(define tile-lava
  (make-tile-type 'lava #\~ "Lava" #f #f 999
                  '((damage . 10) (element . fire))))

(define tile-grass
  (make-tile-type 'grass #\, "Grass" #t #f 1))

(define tile-tree
  (make-tile-type 'tree #\T "Tree" #f #t 999))

(define tile-void
  (make-tile-type 'void #\space "Void" #f #f 999))

(define tile-bridge
  (make-tile-type 'bridge #\= "Bridge" #t #f 1))

(define tile-trap
  (make-tile-type 'trap #\^ "Trap" #t #f 1
                  '((trap . #t) (hidden . #t))))

(define tile-chest
  (make-tile-type 'chest #\$ "Chest" #f #f 1
                  '((container . #t) (lootable . #t))))

;;; standard-tile-types : Alist (Symbol . TileType)
;;; Registry of standard tile types by ID.
(define standard-tile-types
  `((floor . ,tile-floor)
    (wall . ,tile-wall)
    (door-closed . ,tile-door-closed)
    (door-open . ,tile-door-open)
    (stairs-down . ,tile-stairs-down)
    (stairs-up . ,tile-stairs-up)
    (water . ,tile-water)
    (shallow-water . ,tile-shallow-water)
    (lava . ,tile-lava)
    (grass . ,tile-grass)
    (tree . ,tile-tree)
    (void . ,tile-void)
    (bridge . ,tile-bridge)
    (trap . ,tile-trap)
    (chest . ,tile-chest)))

;;; get-tile-type : Symbol -> TileType | #f
;;; Look up a standard tile type by ID.
(define (get-tile-type id)
  (alist-ref standard-tile-types id))

;;; ============================================================
;;; Tile Instance
;;; ============================================================

;;; A Tile is a specific instance in a tilemap.
;;; It references a TileType and can have instance-specific state.
;;;
;;; Tile : Record
;;;   type      — TileType, the base type
;;;   explored  — Bool, has player seen this tile?
;;;   visible   — Bool, is tile currently in FOV?
;;;   state     — Alist, instance-specific state (e.g., trap triggered)

(define-record-type tile%
  (fields type explored visible state))

;;; make-tile : TileType × [Bool] × [Bool] × [Alist] -> Tile
(define make-tile
  (case-lambda
    [(type) (make-tile% type #f #f '())]
    [(type explored) (make-tile% type explored #f '())]
    [(type explored visible) (make-tile% type explored visible '())]
    [(type explored visible state) (make-tile% type explored visible state)]))

;;; Accessors
(define tile-type tile%-type)
(define tile-explored tile%-explored)
(define tile-visible tile%-visible)
(define tile-state tile%-state)

;;; Derived accessors (shortcuts to type properties)
(define (tile-id t) (tile-type-id (tile-type t)))
(define (tile-char t) (tile-type-char (tile-type t)))
(define (tile-name t) (tile-type-name (tile-type t)))
(define (tile-walkable? t) (tile-type-walkable (tile-type t)))
(define (tile-opaque? t) (tile-type-opaque (tile-type t)))
(define (tile-cost t) (tile-type-cost (tile-type t)))

;;; tile-set-explored : Tile × Bool -> Tile
(define (tile-set-explored tile explored)
  (make-tile% (tile-type tile) explored (tile-visible tile) (tile-state tile)))

;;; tile-set-visible : Tile × Bool -> Tile
(define (tile-set-visible tile visible)
  (make-tile% (tile-type tile) (tile-explored tile) visible (tile-state tile)))

;;; tile-set-type : Tile × TileType -> Tile
(define (tile-set-type tile new-type)
  (make-tile% new-type (tile-explored tile) (tile-visible tile) (tile-state tile)))

;;; tile-update-state : Tile × Symbol × Any -> Tile
(define (tile-update-state tile key value)
  (make-tile% (tile-type tile)
              (tile-explored tile)
              (tile-visible tile)
              (alist-set (tile-state tile) key value)))

;;; ============================================================
;;; TileMap
;;; ============================================================

;;; TileMap : Record
;;;   width   — Nat, width in tiles
;;;   height  — Nat, height in tiles
;;;   tiles   — Vector Tile, row-major storage
;;;   default — TileType, type for out-of-bounds queries

(define-record-type tilemap%
  (fields width height tiles default))

;;; make-tilemap : Nat × Nat × [TileType] -> TileMap
;;; Create a tilemap filled with the default tile type.
(define make-tilemap
  (case-lambda
    [(width height)
     (make-tilemap width height tile-void)]
    [(width height default-type)
     (let* ([size (* width height)]
            [tiles (make-vector size)])
       ;; Initialize with default tiles
       (let loop ([i 0])
         (when (< i size)
           (vector-set! tiles i (make-tile default-type))
           (loop (+ i 1))))
       (make-tilemap% width height tiles default-type))]))

;;; Accessors
(define tilemap-width tilemap%-width)
(define tilemap-height tilemap%-height)
(define tilemap-default tilemap%-default)

;;; tilemap-in-bounds? : TileMap × Int × Int -> Bool
;;; Check if coordinates are within the tilemap.
(define (tilemap-in-bounds? tm x y)
  (and (>= x 0) (>= y 0)
       (< x (tilemap-width tm))
       (< y (tilemap-height tm))))

;;; tilemap-index : TileMap × Int × Int -> Nat
;;; Convert (x, y) to vector index.
(define (tilemap-index tm x y)
  (+ (* y (tilemap-width tm)) x))

;;; tilemap-ref : TileMap × Int × Int -> Tile
;;; Get tile at (x, y). Returns void tile if out of bounds.
(define (tilemap-ref tm x y)
  (if (tilemap-in-bounds? tm x y)
      (vector-ref (tilemap%-tiles tm) (tilemap-index tm x y))
      (make-tile (tilemap-default tm))))

;;; tilemap-ref-point : TileMap × Point -> Tile
;;; Get tile at point.
(define (tilemap-ref-point tm pt)
  (tilemap-ref tm (car pt) (cdr pt)))

;;; tilemap-set! : TileMap × Int × Int × Tile -> Void
;;; Set tile at (x, y). Mutates in place for efficiency.
(define (tilemap-set! tm x y tile)
  (when (tilemap-in-bounds? tm x y)
    (vector-set! (tilemap%-tiles tm) (tilemap-index tm x y) tile)))

;;; tilemap-set-type! : TileMap × Int × Int × TileType -> Void
;;; Set tile type at (x, y), preserving explored/visible state.
(define (tilemap-set-type! tm x y tile-type)
  (when (tilemap-in-bounds? tm x y)
    (let ([old-tile (tilemap-ref tm x y)])
      (tilemap-set! tm x y (tile-set-type old-tile tile-type)))))

;;; tilemap-set-type-point! : TileMap × Point × TileType -> Void
(define (tilemap-set-type-point! tm pt tile-type)
  (tilemap-set-type! tm (car pt) (cdr pt) tile-type))

;;; ============================================================
;;; TileMap Queries
;;; ============================================================

;;; tilemap-walkable? : TileMap × Int × Int -> Bool
;;; Check if a tile is walkable.
(define (tilemap-walkable? tm x y)
  (tile-walkable? (tilemap-ref tm x y)))

;;; tilemap-opaque? : TileMap × Int × Int -> Bool
;;; Check if a tile blocks line of sight.
(define (tilemap-opaque? tm x y)
  (tile-opaque? (tilemap-ref tm x y)))

;;; tilemap-cost : TileMap × Int × Int -> Nat
;;; Get movement cost for a tile.
(define (tilemap-cost tm x y)
  (tile-cost (tilemap-ref tm x y)))

;;; tilemap-find-tiles : TileMap × (Tile -> Bool) -> List Point
;;; Find all tiles matching a predicate.
(define (tilemap-find-tiles tm pred)
  (let ([w (tilemap-width tm)]
        [h (tilemap-height tm)])
    (let loop-y ([y 0] [results '()])
      (if (>= y h)
          (reverse results)
          (let loop-x ([x 0] [results results])
            (if (>= x w)
                (loop-y (+ y 1) results)
                (let ([tile (tilemap-ref tm x y)])
                  (loop-x (+ x 1)
                          (if (pred tile)
                              (cons (cons x y) results)
                              results)))))))))

;;; tilemap-find-by-type : TileMap × Symbol -> List Point
;;; Find all tiles of a given type.
(define (tilemap-find-by-type tm type-id)
  (tilemap-find-tiles tm (lambda (tile) (eq? (tile-id tile) type-id))))

;;; ============================================================
;;; TileMap Visibility Operations
;;; ============================================================

;;; tilemap-clear-visibility! : TileMap -> Void
;;; Clear all visibility flags (before FOV calculation).
(define (tilemap-clear-visibility! tm)
  (let ([w (tilemap-width tm)]
        [h (tilemap-height tm)])
    (let loop ([i 0])
      (when (< i (* w h))
        (let ([tile (vector-ref (tilemap%-tiles tm) i)])
          (vector-set! (tilemap%-tiles tm) i
                       (tile-set-visible tile #f)))
        (loop (+ i 1))))))

;;; tilemap-set-visible! : TileMap × Int × Int × Bool -> Void
;;; Set visibility for a tile (also marks as explored if visible).
(define (tilemap-set-visible! tm x y visible)
  (when (tilemap-in-bounds? tm x y)
    (let* ([tile (tilemap-ref tm x y)]
           [new-tile (tile-set-visible tile visible)]
           [new-tile (if visible
                         (tile-set-explored new-tile #t)
                         new-tile)])
      (tilemap-set! tm x y new-tile))))

;;; ============================================================
;;; TileMap Iteration
;;; ============================================================

;;; tilemap-for-each : TileMap × (Int × Int × Tile -> Void) -> Void
;;; Iterate over all tiles.
(define (tilemap-for-each tm proc)
  (let ([w (tilemap-width tm)]
        [h (tilemap-height tm)])
    (let loop-y ([y 0])
      (when (< y h)
        (let loop-x ([x 0])
          (when (< x w)
            (proc x y (tilemap-ref tm x y))
            (loop-x (+ x 1))))
        (loop-y (+ y 1))))))

;;; tilemap-map : TileMap × (Int × Int × Tile -> Tile) -> TileMap
;;; Create a new tilemap by transforming each tile.
(define (tilemap-map tm proc)
  (let* ([w (tilemap-width tm)]
         [h (tilemap-height tm)]
         [new-tm (make-tilemap w h (tilemap-default tm))])
    (tilemap-for-each tm
      (lambda (x y tile)
        (tilemap-set! new-tm x y (proc x y tile))))
    new-tm))

;;; ============================================================
;;; TileMap Rendering
;;; ============================================================

;;; tilemap->canvas : TileMap × [Bool] -> Canvas
;;; Render tilemap to a canvas.
;;; If show-all? is #f, only shows explored tiles (with dimming for non-visible).
(define tilemap->canvas
  (case-lambda
    [(tm) (tilemap->canvas tm #t)]
    [(tm show-all?)
     (let* ([w (tilemap-width tm)]
            [h (tilemap-height tm)]
            [canvas (make-canvas w h)])
       (tilemap-for-each tm
         (lambda (x y tile)
           (let ([ch (cond
                       [show-all? (tile-char tile)]
                       [(tile-visible tile) (tile-char tile)]
                       [(tile-explored tile) (tile-char tile)]  ; Could dim this
                       [else #\space])])
             (set! canvas (canvas-set canvas x y ch)))))
       canvas)]))

;;; tilemap-render-region : TileMap × Rect × [Bool] -> Canvas
;;; Render a region of the tilemap to a canvas.
(define tilemap-render-region
  (case-lambda
    [(tm region) (tilemap-render-region tm region #t)]
    [(tm region show-all?)
     (let* ([rx (point-x (rect-origin region))]
            [ry (point-y (rect-origin region))]
            [rw (rect-width region)]
            [rh (rect-height region)]
            [canvas (make-canvas rw rh)])
       (let loop-y ([y 0])
         (when (< y rh)
           (let loop-x ([x 0])
             (when (< x rw)
               (let* ([tile (tilemap-ref tm (+ rx x) (+ ry y))]
                      [ch (cond
                            [show-all? (tile-char tile)]
                            [(tile-visible tile) (tile-char tile)]
                            [(tile-explored tile) (tile-char tile)]
                            [else #\space])])
                 (set! canvas (canvas-set canvas x y ch)))
               (loop-x (+ x 1))))
           (loop-y (+ y 1))))
       canvas)]))

;;; ============================================================
;;; TileMap Serialization
;;; ============================================================

;;; tilemap->string : TileMap -> String
;;; Convert tilemap to a simple string representation.
;;; Each row is one line, tiles are their characters.
(define (tilemap->string tm)
  (canvas->string (tilemap->canvas tm #t)))

;;; string->tilemap : String × Alist -> TileMap
;;; Parse a tilemap from a string.
;;; char-map maps characters to tile types.
(define (string->tilemap str char-map)
  (let* ([lines (string-split str #\newline)]
         [h (length lines)]
         [w (if (null? lines) 0 (string-length (car lines)))]
         [tm (make-tilemap w h tile-void)])
    (let loop-y ([lines lines] [y 0])
      (unless (null? lines)
        (let ([line (car lines)])
          (let loop-x ([x 0])
            (when (< x (string-length line))
              (let* ([ch (string-ref line x)]
                     [type (alist-ref char-map ch tile-void)])
                (tilemap-set-type! tm x y type))
              (loop-x (+ x 1)))))
        (loop-y (cdr lines) (+ y 1))))
    tm))

;;; Helper: split string by character
(define (string-split str delim)
  (let loop ([chars (string->list str)] [current '()] [results '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) results))]
      [(char=? (car chars) delim)
       (loop (cdr chars) '() (cons (list->string (reverse current)) results))]
      [else
       (loop (cdr chars) (cons (car chars) current) results)])))

;;; ============================================================
;;; Standard Character Map for Parsing
;;; ============================================================

(define standard-char-map
  `((#\. . ,tile-floor)
    (#\# . ,tile-wall)
    (#\+ . ,tile-door-closed)
    (#\' . ,tile-door-open)
    (#\> . ,tile-stairs-down)
    (#\< . ,tile-stairs-up)
    (#\~ . ,tile-water)
    (#\, . ,tile-grass)
    (#\T . ,tile-tree)
    (#\= . ,tile-bridge)
    (#\^ . ,tile-trap)
    (#\$ . ,tile-chest)
    (#\space . ,tile-void)))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; TileType:
;;;   make-tile-type, tile-type-id, tile-type-char, tile-type-name
;;;   tile-type-walkable, tile-type-opaque, tile-type-cost, tile-type-properties
;;;   tile-type-property
;;;
;;; Standard Tiles:
;;;   tile-floor, tile-wall, tile-door-closed, tile-door-open
;;;   tile-stairs-down, tile-stairs-up, tile-water, tile-shallow-water
;;;   tile-lava, tile-grass, tile-tree, tile-void, tile-bridge, tile-trap, tile-chest
;;;   standard-tile-types, get-tile-type
;;;
;;; Tile Instance:
;;;   make-tile, tile-type, tile-explored, tile-visible, tile-state
;;;   tile-id, tile-char, tile-name, tile-walkable?, tile-opaque?, tile-cost
;;;   tile-set-explored, tile-set-visible, tile-set-type, tile-update-state
;;;
;;; TileMap:
;;;   make-tilemap, tilemap-width, tilemap-height, tilemap-default
;;;   tilemap-in-bounds?, tilemap-ref, tilemap-ref-point
;;;   tilemap-set!, tilemap-set-type!, tilemap-set-type-point!
;;;
;;; TileMap Queries:
;;;   tilemap-walkable?, tilemap-opaque?, tilemap-cost
;;;   tilemap-find-tiles, tilemap-find-by-type
;;;
;;; TileMap Visibility:
;;;   tilemap-clear-visibility!, tilemap-set-visible!
;;;
;;; TileMap Iteration:
;;;   tilemap-for-each, tilemap-map
;;;
;;; TileMap Rendering:
;;;   tilemap->canvas, tilemap-render-region, tilemap->string
;;;
;;; TileMap Parsing:
;;;   string->tilemap, standard-char-map
