;;; playpen/loom/world.ss — World State Management for RPG SDK
;;;
;;; The world holds the complete game state: tilemap, entities, and metadata.
;;; Provides centralized state management and spatial queries.
;;;
;;; This is Playpen code: the SDK for building roguelikes and dungeon crawlers.
;;;
;;; Dependencies:
;;;   - playpen/loom/core.ss
;;;   - playpen/loom/tile.ss
;;;   - playpen/loom/entity.ss
;;;
;;; Exports:
;;;   World creation and management
;;;   Entity management (add, remove, query)
;;;   Spatial queries (at position, in area, pathfinding)
;;;   World state updates
;;;
;;; ============================================================
;;; MUTATION SEMANTICS — IMPORTANT!
;;; ============================================================
;;;
;;; This SDK uses a HYBRID approach:
;;;   - FUNCTIONAL: Most operations return NEW worlds/entities
;;;   - IMPERATIVE: Spatial index and tilemap mutations are in-place
;;;
;;; FUNCTIONAL (returns new value, MUST capture return):
;;;   - world-add-entity, world-remove-entity
;;;   - world-update-entity, world-replace-entity
;;;   - world-move-entity
;;;   - world-set-tilemap, world-set-entities, world-set-player-id
;;;   - world-pickup-item, world-drop-item, world-transfer-item
;;;   - All entity-* functions (entity-move-to, entity-damage, etc.)
;;;
;;; IMPERATIVE (mutates in place, returns Void or Bool):
;;;   - world-set-tile!, world-open-door!, world-close-door!
;;;   - tilemap-set-type!, tilemap-set-visible!
;;;   - spatial-add!, spatial-remove!, spatial-move!
;;;   - world-compute-fov!
;;;
;;; WHY THE SPLIT?
;;;   - Spatial index is internal to World - mutations stay encapsulated
;;;   - Tilemap mutations are common hot-path operations
;;;   - Entity operations are infrequent but complex - functional is safer
;;;
;;; BEST PRACTICE:
;;;   Always capture return values from functional operations:
;;;     ✓ (set! world (world-add-entity world entity))
;;;     ✗ (world-add-entity world entity)  ; Lost the new world!
;;;
;;;   Functions with '!' suffix mutate and return Void or Bool:
;;;     ✓ (world-set-tile! world x y tile-wall)
;;;     ✗ (set! world (world-set-tile! world x y tile-wall))  ; Wrong!
;;;

;;; ============================================================
;;; World Structure
;;; ============================================================

;;; World : Record
;;;   tilemap      — TileMap, the terrain
;;;   entities     — Alist (ID . Entity), all entities in world
;;;   spatial-index — Vector of Lists, entities by tile position
;;;   player-id    — Nat | #f, the player entity ID
;;;   level        — Nat, current dungeon level
;;;   metadata     — Alist, additional world data

(define-record-type world%
  (fields tilemap entities spatial-index player-id level metadata))

;;; make-world : TileMap × [Nat] × [Alist] -> World
;;; Create a new world with the given tilemap.
(define make-world
  (case-lambda
    [(tilemap)
     (make-world tilemap 1 '())]
    [(tilemap level)
     (make-world tilemap level '())]
    [(tilemap level metadata)
     (let* ([w (tilemap-width tilemap)]
            [h (tilemap-height tilemap)]
            [spatial (make-vector (* w h) '())])
       (make-world% tilemap '() spatial #f level metadata))]))

;;; Accessors
(define world-tilemap world%-tilemap)
(define world-entities world%-entities)
(define world-spatial-index world%-spatial-index)
(define world-player-id world%-player-id)
(define world-level world%-level)
(define world-metadata world%-metadata)

;;; world-width : World -> Nat
(define (world-width world)
  (tilemap-width (world-tilemap world)))

;;; world-height : World -> Nat
(define (world-height world)
  (tilemap-height (world-tilemap world)))

;;; ============================================================
;;; Spatial Index Management
;;; ============================================================

;;; The spatial index stores entity IDs at each tile position
;;; for O(1) lookup of entities by position.

;;; spatial-index : World × Int × Int -> Nat (index)
(define (spatial-index-pos world x y)
  (+ (* y (world-width world)) x))

;;; spatial-get : World × Int × Int -> List Nat
;;; Get entity IDs at position.
(define (spatial-get world x y)
  (if (tilemap-in-bounds? (world-tilemap world) x y)
      (vector-ref (world-spatial-index world)
                  (spatial-index-pos world x y))
      '()))

;;; spatial-add! : World × Int × Int × Nat -> Void
;;; Add entity ID to spatial index.
(define (spatial-add! world x y entity-id)
  (when (tilemap-in-bounds? (world-tilemap world) x y)
    (let ([idx (spatial-index-pos world x y)]
          [spatial (world-spatial-index world)])
      (vector-set! spatial idx
                   (cons entity-id (vector-ref spatial idx))))))

;;; spatial-remove! : World × Int × Int × Nat -> Void
;;; Remove entity ID from spatial index.
(define (spatial-remove! world x y entity-id)
  (when (tilemap-in-bounds? (world-tilemap world) x y)
    (let ([idx (spatial-index-pos world x y)]
          [spatial (world-spatial-index world)])
      (vector-set! spatial idx
                   (filter (lambda (id) (not (= id entity-id)))
                           (vector-ref spatial idx))))))

;;; spatial-move! : World × Nat × Int × Int × Int × Int -> Void
;;; Move entity in spatial index.
(define (spatial-move! world entity-id old-x old-y new-x new-y)
  (spatial-remove! world old-x old-y entity-id)
  (spatial-add! world new-x new-y entity-id))

;;; ============================================================
;;; World Update (Functional)
;;; ============================================================

;;; world-set-tilemap : World × TileMap -> World
(define (world-set-tilemap world new-tilemap)
  (make-world% new-tilemap
               (world-entities world)
               (world-spatial-index world)
               (world-player-id world)
               (world-level world)
               (world-metadata world)))

;;; world-set-entities : World × Alist -> World
(define (world-set-entities world new-entities)
  (make-world% (world-tilemap world)
               new-entities
               (world-spatial-index world)
               (world-player-id world)
               (world-level world)
               (world-metadata world)))

;;; world-set-player-id : World × Nat -> World
(define (world-set-player-id world player-id)
  (make-world% (world-tilemap world)
               (world-entities world)
               (world-spatial-index world)
               player-id
               (world-level world)
               (world-metadata world)))

;;; world-set-level : World × Nat -> World
(define (world-set-level world level)
  (make-world% (world-tilemap world)
               (world-entities world)
               (world-spatial-index world)
               (world-player-id world)
               level
               (world-metadata world)))

;;; world-set-metadata : World × Alist -> World
(define (world-set-metadata world metadata)
  (make-world% (world-tilemap world)
               (world-entities world)
               (world-spatial-index world)
               (world-player-id world)
               (world-level world)
               metadata))

;;; world-update-metadata : World × Symbol × Any -> World
(define (world-update-metadata world key value)
  (world-set-metadata world
                      (alist-set (world-metadata world) key value)))

;;; ============================================================
;;; Entity Management
;;; ============================================================

;;; world-add-entity : World × Entity -> World
;;; Add an entity to the world.
(define (world-add-entity world entity)
  (let* ([id (entity-id entity)]
         [entities (alist-set (world-entities world) id entity)]
         [new-world (world-set-entities world entities)])
    ;; Update spatial index if entity has position
    (when (entity-has-component? entity 'position)
      (spatial-add! new-world (entity-x entity) (entity-y entity) id))
    new-world))

;;; world-remove-entity : World × Nat -> World
;;; Remove an entity by ID.
(define (world-remove-entity world entity-id)
  (let ([entity (world-get-entity world entity-id)])
    (when (and entity (entity-has-component? entity 'position))
      (spatial-remove! world (entity-x entity) (entity-y entity) entity-id))
    (world-set-entities world
                        (alist-remove (world-entities world) entity-id))))

;;; world-get-entity : World × Nat -> Entity | #f
;;; Get entity by ID.
(define (world-get-entity world entity-id)
  (alist-ref (world-entities world) entity-id))

;;; world-update-entity : World × Nat × (Entity -> Entity) -> World
;;; Update an entity by ID using a function.
(define (world-update-entity world entity-id update-fn)
  (let ([entity (world-get-entity world entity-id)])
    (if entity
        (let* ([old-x (entity-x entity)]
               [old-y (entity-y entity)]
               [new-entity (update-fn entity)]
               [new-x (entity-x new-entity)]
               [new-y (entity-y new-entity)]
               [entities (alist-set (world-entities world) entity-id new-entity)]
               [new-world (world-set-entities world entities)])
          ;; Update spatial index if position changed
          (when (and (entity-has-component? entity 'position)
                     (or (not (= old-x new-x))
                         (not (= old-y new-y))))
            (spatial-move! new-world entity-id old-x old-y new-x new-y))
          new-world)
        world)))

;;; world-replace-entity : World × Nat × Entity -> World
;;; Replace an entity by ID with a new entity.
;;; Convenience wrapper for world-update-entity when you already have the new entity.
(define (world-replace-entity world entity-id new-entity)
  (world-update-entity world entity-id (lambda (_) new-entity)))

;;; world-entity-count : World -> Nat
(define (world-entity-count world)
  (length (world-entities world)))

;;; world-all-entities : World -> List Entity
(define (world-all-entities world)
  (map cdr (world-entities world)))

;;; world-entity-ids : World -> List Nat
(define (world-entity-ids world)
  (map car (world-entities world)))

;;; ============================================================
;;; Player Access
;;; ============================================================

;;; world-get-player : World -> Entity | #f
(define (world-get-player world)
  (let ([pid (world-player-id world)])
    (if pid
        (world-get-entity world pid)
        #f)))

;;; world-update-player : World × (Entity -> Entity) -> World
(define (world-update-player world update-fn)
  (let ([pid (world-player-id world)])
    (if pid
        (world-update-entity world pid update-fn)
        world)))

;;; world-spawn-player : World × Entity -> World
;;; Add player entity and set as player.
(define (world-spawn-player world player-entity)
  (let ([world (world-add-entity world player-entity)])
    (world-set-player-id world (entity-id player-entity))))

;;; ============================================================
;;; Spatial Queries
;;; ============================================================

;;; world-entities-at : World × Int × Int -> List Entity
;;; Get all entities at a position.
(define (world-entities-at world x y)
  (let ([ids (spatial-get world x y)])
    (filter-map (lambda (id) (world-get-entity world id)) ids)))

;;; world-entities-at-point : World × Point -> List Entity
(define (world-entities-at-point world pt)
  (world-entities-at world (car pt) (cdr pt)))

;;; world-entity-at : World × Int × Int -> Entity | #f
;;; Get first entity at position (usually for blocking queries).
(define (world-entity-at world x y)
  (let ([entities (world-entities-at world x y)])
    (if (null? entities) #f (car entities))))

;;; world-entities-in-rect : World × Rect -> List Entity
;;; Get all entities within a rectangle.
(define (world-entities-in-rect world rect)
  (let* ([ox (point-x (rect-origin rect))]
         [oy (point-y (rect-origin rect))]
         [w (rect-width rect)]
         [h (rect-height rect)]
         [results '()])
    (let loop-y ([y oy])
      (if (>= y (+ oy h))
          results
          (begin
            (let loop-x ([x ox])
              (if (>= x (+ ox w))
                  #f
                  (begin
                    (set! results (append results (world-entities-at world x y)))
                    (loop-x (+ x 1)))))
            (loop-y (+ y 1)))))))

;;; world-entities-in-radius : World × Point × Nat -> List Entity
;;; Get entities within Manhattan radius.
(define (world-entities-in-radius world center radius)
  (let ([cx (car center)]
        [cy (cdr center)]
        [results '()])
    (let loop-y ([y (- cy radius)])
      (if (> y (+ cy radius))
          results
          (begin
            (let loop-x ([x (- cx radius)])
              (if (> x (+ cx radius))
                  #f
                  (begin
                    (when (<= (manhattan-distance (cons x y) center) radius)
                      (set! results (append results (world-entities-at world x y))))
                    (loop-x (+ x 1)))))
            (loop-y (+ y 1)))))))

;;; world-find-entities : World × (Entity -> Bool) -> List Entity
;;; Find all entities matching predicate.
(define (world-find-entities world pred)
  (filter pred (world-all-entities world)))

;;; world-find-nearest : World × Point × (Entity -> Bool) -> Entity | #f
;;; Find nearest entity matching predicate.
(define (world-find-nearest world from-point pred)
  (let* ([entities (world-find-entities world pred)]
         [with-dist (map (lambda (e)
                          (cons (manhattan-distance from-point (entity-point e)) e))
                        entities)]
         [sorted (list-sort (lambda (a b) (< (car a) (car b))) with-dist)])
    (if (null? sorted)
        #f
        (cdar sorted))))

;;; ============================================================
;;; Movement and Collision
;;; ============================================================

;;; world-tile-walkable? : World × Int × Int -> Bool
;;; Check if tile is walkable (terrain check only).
(define (world-tile-walkable? world x y)
  (tilemap-walkable? (world-tilemap world) x y))

;;; world-blocked? : World × Int × Int -> Bool
;;; Check if position is blocked (tile or entity).
(define (world-blocked? world x y)
  (or (not (world-tile-walkable? world x y))
      (any (lambda (e) (entity-blocks-movement? e))
           (world-entities-at world x y))))

;;; world-position-valid? : World × Int × Int -> Bool
;;; Check if position is within bounds.
(define (world-position-valid? world x y)
  (tilemap-in-bounds? (world-tilemap world) x y))

;;; world-can-move-to? : World × Entity × Int × Int -> Bool
;;; Check if entity can move to position.
(define (world-can-move-to? world entity x y)
  (and (world-position-valid? world x y)
       (world-tile-walkable? world x y)
       (not (any (lambda (e)
                   (and (entity-blocks-movement? e)
                        (not (= (entity-id e) (entity-id entity)))))
                 (world-entities-at world x y)))))

;;; world-move-entity : World × Nat × Direction -> World
;;; Move entity in direction (if valid).
(define (world-move-entity world entity-id dir)
  (let ([entity (world-get-entity world entity-id)])
    (if entity
        (let* ([delta (direction->delta dir)]
               [new-x (+ (entity-x entity) (car delta))]
               [new-y (+ (entity-y entity) (cdr delta))])
          (if (world-can-move-to? world entity new-x new-y)
              (world-update-entity world entity-id
                (lambda (e) (entity-move-to e new-x new-y)))
              world))
        world)))

;;; ============================================================
;;; Line of Sight
;;; ============================================================

;;; world-tile-opaque? : World × Int × Int -> Bool
;;; Check if tile blocks sight.
(define (world-tile-opaque? world x y)
  (or (tilemap-opaque? (world-tilemap world) x y)
      (any entity-blocks-sight? (world-entities-at world x y))))

;;; world-has-los? : World × Point × Point -> Bool
;;; Check if there's line of sight between two points.
;;; Uses Bresenham's line algorithm.
(define (world-has-los? world from to)
  (let* ([x0 (car from)] [y0 (cdr from)]
         [x1 (car to)] [y1 (cdr to)]
         [dx (abs (- x1 x0))]
         [dy (abs (- y1 y0))]
         [sx (if (< x0 x1) 1 -1)]
         [sy (if (< y0 y1) 1 -1)]
         [err (- dx dy)])
    (let loop ([x x0] [y y0] [err err])
      (cond
        [(and (= x x1) (= y y1)) #t]  ; Reached target
        [(and (not (and (= x x0) (= y y0)))  ; Not starting point
              (world-tile-opaque? world x y))
         #f]  ; Blocked
        [else
         (let* ([e2 (* 2 err)]
                [new-err err]
                [new-x x]
                [new-y y])
           (when (> e2 (- dy))
             (set! new-err (- new-err dy))
             (set! new-x (+ new-x sx)))
           (when (< e2 dx)
             (set! new-err (+ new-err dx))
             (set! new-y (+ new-y sy)))
           (loop new-x new-y new-err))]))))

;;; ============================================================
;;; Field of View (Simple Shadowcasting)
;;; ============================================================

;;; world-compute-fov! : World × Point × Nat -> Void
;;; Compute field of view from a point with given radius.
;;; Updates tile visibility in the tilemap.
(define (world-compute-fov! world center radius)
  (let ([tm (world-tilemap world)]
        [cx (car center)]
        [cy (cdr center)])
    ;; Clear existing visibility
    (tilemap-clear-visibility! tm)
    ;; Set center as visible
    (tilemap-set-visible! tm cx cy #t)
    ;; Simple radius-based FOV (raycast to each point on perimeter)
    (let loop-angle ([angle 0.0])
      (when (< angle (* 2 3.14159))
        (let* ([dx (cos angle)]
               [dy (sin angle)])
          ;; Cast ray
          (let ray-loop ([dist 1])
            (when (<= dist radius)
              (let ([x (+ cx (round (* dist dx)))]
                    [y (+ cy (round (* dist dy)))])
                (when (world-position-valid? world x y)
                  (tilemap-set-visible! tm x y #t)
                  (unless (world-tile-opaque? world x y)
                    (ray-loop (+ dist 1))))))))
        (loop-angle (+ angle (/ 3.14159 (* 4 radius))))))))

;;; ============================================================
;;; Pathfinding (A*)
;;; ============================================================

;;; path-node : Internal structure for A* algorithm
(define (make-path-node pos g h parent)
  (list pos g h parent))

(define (path-node-pos node) (car node))
(define (path-node-g node) (cadr node))
(define (path-node-h node) (caddr node))
(define (path-node-f node) (+ (cadr node) (caddr node)))
(define (path-node-parent node) (cadddr node))

;;; world-find-path : World × Point × Point × [Bool] -> List Point | #f
;;; Find path from start to goal using A*.
;;; Returns list of points (including goal, excluding start), or #f if no path.
(define world-find-path
  (case-lambda
    [(world start goal)
     (world-find-path world start goal #f)]
    [(world start goal allow-diagonal?)
     (let* ([open-list (list (make-path-node start 0 (manhattan-distance start goal) #f))]
            [closed-set '()]
            [max-iterations 1000])
       (let loop ([open open-list] [closed closed-set] [iterations 0])
         (cond
           [(null? open) #f]  ; No path
           [(>= iterations max-iterations) #f]  ; Timeout
           [else
            (let* ([current (car (list-sort (lambda (a b)
                                              (< (path-node-f a) (path-node-f b)))
                                            open))]
                   [current-pos (path-node-pos current)]
                   [new-open (filter (lambda (n)
                                       (not (point=? (path-node-pos n) current-pos)))
                                     open)]
                   [new-closed (cons current-pos closed)])
              (if (point=? current-pos goal)
                  ;; Reconstruct path
                  (let reconstruct ([node current] [path '()])
                    (if (path-node-parent node)
                        (reconstruct (path-node-parent node)
                                    (cons (path-node-pos node) path))
                        path))
                  ;; Explore neighbors
                  (let* ([neighbors (point-neighbors current-pos allow-diagonal?)]
                         [valid-neighbors
                          (filter (lambda (pt)
                                    (and (world-position-valid? world (car pt) (cdr pt))
                                         (world-can-move-to? world
                                                            (make-entity)  ; Dummy
                                                            (car pt) (cdr pt))
                                         (not (member pt new-closed point=?))))
                                  neighbors)])
                    (let neighbor-loop ([neighbors valid-neighbors]
                                        [open new-open])
                      (if (null? neighbors)
                          (loop open new-closed (+ iterations 1))
                          (let* ([neighbor (car neighbors)]
                                 [g (+ (path-node-g current) 1)]
                                 [h (manhattan-distance neighbor goal)]
                                 [existing (find (lambda (n)
                                                   (point=? (path-node-pos n) neighbor))
                                                 open)])
                            (neighbor-loop
                             (cdr neighbors)
                             (if (and existing (<= (path-node-g existing) g))
                                 open  ; Existing path is better
                                 (cons (make-path-node neighbor g h current)
                                       (if existing
                                           (filter (lambda (n)
                                                     (not (point=? (path-node-pos n) neighbor)))
                                                   open)
                                           open))))))))))])))]))

;;; ============================================================
;;; Inventory Management (World-Level)
;;; ============================================================

;;; world-pickup-item : World × Nat × Nat -> World
;;; Entity picks up an item at their current location.
;;; Removes item from world and adds to entity's inventory.
(define (world-pickup-item world entity-id item-id)
  (let ([entity (world-get-entity world entity-id)]
        [item (world-get-entity world item-id)])
    (if (and entity item
             (entity-has-component? entity 'inventory)
             (entity-has-component? item 'item)
             (point=? (entity-point entity) (entity-point item)))
        ;; Entity and item at same location - pickup
        (let* ([inventory (entity-inventory entity)]
               [new-inventory (inventory-add-item inventory item-id)]
               [new-entity (entity-add-component entity new-inventory)]
               [world (world-update-entity world entity-id (lambda (_) new-entity))]
               [world (world-remove-entity world item-id)])
          world)
        ;; Can't pickup - return unchanged
        world)))

;;; world-drop-item : World × Nat × Any -> World
;;; Entity drops an item from inventory at their current location.
;;; Removes item from inventory and adds to world as entity.
(define (world-drop-item world entity-id item-id)
  (let ([entity (world-get-entity world entity-id)])
    (if (and entity (entity-has-component? entity 'inventory))
        (let* ([inventory (entity-inventory entity)]
               [has-item? (inventory-has-item? inventory item-id)])
          (if has-item?
              ;; Drop the item
              (let* ([new-inventory (inventory-remove-item inventory item-id)]
                     [new-entity (entity-add-component entity new-inventory)]
                     [pos (entity-point entity)]
                     ;; Note: This assumes item-id can be resolved to an item entity
                     ;; In practice, you'd need an item registry to create item entities
                     ;; For now, this is a stub that needs item system integration
                     [world (world-update-entity world entity-id (lambda (_) new-entity))])
                world)
              ;; Don't have item
              world))
        world)))

;;; world-transfer-item : World × Nat × Nat × Any -> World
;;; Transfer item from one entity's inventory to another's.
(define (world-transfer-item world from-entity-id to-entity-id item-id)
  (let ([from-entity (world-get-entity world from-entity-id)]
        [to-entity (world-get-entity world to-entity-id)])
    (if (and from-entity to-entity
             (entity-has-component? from-entity 'inventory)
             (entity-has-component? to-entity 'inventory))
        (let* ([from-inv (entity-inventory from-entity)]
               [to-inv (entity-inventory to-entity)])
          (if (inventory-has-item? from-inv item-id)
              (let* ([new-from-inv (inventory-remove-item from-inv item-id)]
                     [new-to-inv (inventory-add-item to-inv item-id)]
                     [new-from (entity-add-component from-entity new-from-inv)]
                     [new-to (entity-add-component to-entity new-to-inv)]
                     [world (world-update-entity world from-entity-id (lambda (_) new-from))]
                     [world (world-update-entity world to-entity-id (lambda (_) new-to))])
                world)
              world))
        world)))

;;; world-items-at : World × Int × Int -> List Entity
;;; Get all item entities at a position.
(define (world-items-at world x y)
  (let ([entities (world-entities-at world x y)])
    (filter entity-is-item? entities)))

;;; ============================================================
;;; Tilemap Mutation
;;; ============================================================

;;; world-set-tile! : World × Int × Int × TileType -> Void
;;; Change a tile in the world.
(define (world-set-tile! world x y tile-type)
  (tilemap-set-type! (world-tilemap world) x y tile-type))

;;; world-open-door! : World × Int × Int -> Bool
;;; Open a door at position (returns #t if successful).
(define (world-open-door! world x y)
  (let* ([tm (world-tilemap world)]
         [tile (tilemap-ref tm x y)])
    (if (eq? (tile-id tile) 'door-closed)
        (begin
          (tilemap-set-type! tm x y tile-door-open)
          #t)
        #f)))

;;; world-close-door! : World × Int × Int -> Bool
;;; Close a door at position.
(define (world-close-door! world x y)
  (let* ([tm (world-tilemap world)]
         [tile (tilemap-ref tm x y)])
    (if (eq? (tile-id tile) 'door-open)
        (begin
          (tilemap-set-type! tm x y tile-door-closed)
          #t)
        #f)))

;;; ============================================================
;;; World Rendering
;;; ============================================================

;;; world->canvas : World × [Bool] -> Canvas
;;; Render world to canvas (tiles + entities).
(define world->canvas
  (case-lambda
    [(world) (world->canvas world #t)]
    [(world show-all?)
     (let* ([tm (world-tilemap world)]
            [canvas (tilemap->canvas tm show-all?)])
       ;; Render entities sorted by layer
       (let* ([entities (world-all-entities world)]
              [renderables (filter entity-renderable entities)]
              [sorted (list-sort
                       (lambda (a b)
                         (< (renderable-layer (entity-renderable a))
                            (renderable-layer (entity-renderable b))))
                       renderables)])
         (fold-left
          (lambda (canvas entity)
            (let ([x (entity-x entity)]
                  [y (entity-y entity)]
                  [ch (entity-char entity)]
                  [visible? (or show-all?
                                (tile-visible (tilemap-ref tm x y)))])
              (if visible?
                  (canvas-set canvas x y ch)
                  canvas)))
          canvas
          sorted)))]))

;;; world-render-viewport : World × Point × Nat × Nat × [Bool] -> Canvas
;;; Render a viewport centered on a point.
(define world-render-viewport
  (case-lambda
    [(world center width height)
     (world-render-viewport world center width height #t)]
    [(world center width height show-all?)
     (let* ([cx (car center)]
            [cy (cdr center)]
            [half-w (quotient width 2)]
            [half-h (quotient height 2)]
            [start-x (- cx half-w)]
            [start-y (- cy half-h)]
            [canvas (make-canvas width height)])
       ;; Render tiles
       (let loop-y ([vy 0])
         (when (< vy height)
           (let loop-x ([vx 0])
             (when (< vx width)
               (let* ([wx (+ start-x vx)]
                      [wy (+ start-y vy)]
                      [tile (tilemap-ref (world-tilemap world) wx wy)]
                      [ch (cond
                            [show-all? (tile-char tile)]
                            [(tile-visible tile) (tile-char tile)]
                            [(tile-explored tile) (tile-char tile)]
                            [else #\space])])
                 (set! canvas (canvas-set canvas vx vy ch)))
               (loop-x (+ vx 1))))
           (loop-y (+ vy 1))))
       ;; Render entities
       (let* ([entities (world-all-entities world)]
              [renderables (filter entity-renderable entities)]
              [sorted (list-sort
                       (lambda (a b)
                         (< (renderable-layer (entity-renderable a))
                            (renderable-layer (entity-renderable b))))
                       renderables)])
         (fold-left
          (lambda (canvas entity)
            (let* ([wx (entity-x entity)]
                   [wy (entity-y entity)]
                   [vx (- wx start-x)]
                   [vy (- wy start-y)]
                   [ch (entity-char entity)]
                   [tile (tilemap-ref (world-tilemap world) wx wy)]
                   [visible? (or show-all? (tile-visible tile))])
              (if (and visible?
                       (>= vx 0) (< vx width)
                       (>= vy 0) (< vy height))
                  (canvas-set canvas vx vy ch)
                  canvas)))
          canvas
          sorted)))]))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; any : (A -> Bool) × List A -> Bool
;;; Check if any element satisfies predicate.
(define (any pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))

;;; filter-map : (A -> B | #f) × List A -> List B
;;; Map with filter for #f results.
(define (filter-map fn lst)
  (let loop ([lst lst] [results '()])
    (if (null? lst)
        (reverse results)
        (let ([result (fn (car lst))])
          (loop (cdr lst)
                (if result
                    (cons result results)
                    results))))))

;;; find : (A -> Bool) × List A -> A | #f
;;; Find first element matching predicate.
(define (find pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) (car lst)]
    [else (find pred (cdr lst))]))

;;; member with custom equality
(define (member item lst eq?)
  (cond
    [(null? lst) #f]
    [(eq? item (car lst)) lst]
    [else (member item (cdr lst) eq?)]))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; World Structure:
;;;   make-world, world-tilemap, world-entities, world-spatial-index
;;;   world-player-id, world-level, world-metadata
;;;   world-width, world-height
;;;
;;; World Update:
;;;   world-set-tilemap, world-set-entities, world-set-player-id
;;;   world-set-level, world-set-metadata, world-update-metadata
;;;
;;; Entity Management:
;;;   world-add-entity, world-remove-entity, world-get-entity
;;;   world-update-entity, world-replace-entity, world-entity-count
;;;   world-all-entities, world-entity-ids
;;;
;;; Player Access:
;;;   world-get-player, world-update-player, world-spawn-player
;;;
;;; Spatial Queries:
;;;   world-entities-at, world-entities-at-point, world-entity-at
;;;   world-entities-in-rect, world-entities-in-radius
;;;   world-find-entities, world-find-nearest
;;;
;;; Movement & Collision:
;;;   world-tile-walkable?, world-blocked?
;;;   world-position-valid?, world-can-move-to?
;;;   world-move-entity
;;;
;;; Line of Sight & FOV:
;;;   world-tile-opaque?, world-has-los?
;;;   world-compute-fov!
;;;
;;; Pathfinding:
;;;   world-find-path
;;;
;;; Inventory Management:
;;;   world-pickup-item, world-drop-item, world-transfer-item
;;;   world-items-at
;;;
;;; Tilemap Mutation:
;;;   world-set-tile!, world-open-door!, world-close-door!
;;;
;;; Rendering:
;;;   world->canvas, world-render-viewport
