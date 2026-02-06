;;; playpen/loom/test-rpg.ss — RPG SDK Test Suite
;;;
;;; Tests for the 2D tile-based turn-based RPG SDK.
;;; Run with: scheme --script playpen/loom/test-rpg.ss
;;;
;;; This is Playpen code: tests for the RPG SDK.

;;; ====
;;; Test Framework
;;; ====

(define *tests-passed* 0)
(define *tests-failed* 0)
(define *current-section* "")

(define (test-section name)
  (set! *current-section* name)
  (display "\n=== ")
  (display name)
  (display " ===\n"))

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "  [PASS] ")
       (display name)
       (newline))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "  [FAIL] ")
       (display name)
       (display "\n    Expected: ")
       (display expected)
       (display "\n    Actual:   ")
       (display actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-summary)
  (newline)
  (display "====\n")
  (display "Tests passed: ")
  (display *tests-passed*)
  (newline)
  (display "Tests failed: ")
  (display *tests-failed*)
  (newline)
  (if (= *tests-failed* 0)
      (display "All tests passed!\n")
      (display "Some tests failed.\n"))
  (display "====\n"))

;;; ====
;;; Load Dependencies
;;; ====

;;; Load canvas system from shell (needed for rendering tests)
(load "lattice/ui/layout.ss")

;;; Load the RPG SDK
(load "user/loom/core.ss")
(load "user/loom/tile.ss")
(load "user/loom/entity.ss")
(load "user/loom/event.ss")
(load "user/loom/action.ss")
(load "user/loom/turn.ss")
(load "user/loom/world.ss")

;;; ====
;;; Core Tests
;;; ====

(test-section "Core - Directions")

(test "direction->delta north" (cons 0 -1) (direction->delta 'north))
(test "direction->delta south" (cons 0 1) (direction->delta 'south))
(test "direction->delta east" (cons 1 0) (direction->delta 'east))
(test "direction->delta west" (cons -1 0) (direction->delta 'west))
(test "direction->delta northeast" (cons 1 -1) (direction->delta 'northeast))
(test "opposite-direction north" 'south (opposite-direction 'north))
(test "opposite-direction east" 'west (opposite-direction 'east))

(test-section "Core - Distance")

(test "manhattan-distance same point" 0 (manhattan-distance '(5 . 5) '(5 . 5)))
(test "manhattan-distance horizontal" 3 (manhattan-distance '(2 . 5) '(5 . 5)))
(test "manhattan-distance diagonal" 6 (manhattan-distance '(0 . 0) '(3 . 3)))
(test "chebyshev-distance diagonal" 3 (chebyshev-distance '(0 . 0) '(3 . 3)))

(test-section "Core - Utilities")

(test "clamp within range" 5 (clamp 5 0 10))
(test "clamp below min" 0 (clamp -5 0 10))
(test "clamp above max" 10 (clamp 15 0 10))
(test "point-add" '(5 . 7) (point-add '(2 . 3) '(3 . 4)))
(test "point-sub" '(1 . 1) (point-sub '(3 . 4) '(2 . 3)))

(test-section "Core - Alist Utilities")

(define test-alist '((a . 1) (b . 2) (c . 3)))
(test "alist-ref found" 2 (alist-ref test-alist 'b))
(test "alist-ref not found" #f (alist-ref test-alist 'd))
(test "alist-ref default" 99 (alist-ref test-alist 'd 99))
(test "alist-set new" '(d . 4) (car (alist-set test-alist 'd 4)))
(test "alist-remove" 2 (length (alist-remove test-alist 'b)))

;;; ====
;;; Tile Tests
;;; ====

(test-section "Tile - Tile Types")

(test "tile-floor walkable" #t (tile-type-walkable tile-floor))
(test "tile-floor not opaque" #f (tile-type-opaque tile-floor))
(test "tile-wall not walkable" #f (tile-type-walkable tile-wall))
(test "tile-wall opaque" #t (tile-type-opaque tile-wall))
(test "tile-floor char" #\. (tile-type-char tile-floor))
(test "tile-wall char" #\# (tile-type-char tile-wall))

(test-section "Tile - TileMap")

(define test-tilemap (make-tilemap 10 10 tile-floor))
(test "tilemap width" 10 (tilemap-width test-tilemap))
(test "tilemap height" 10 (tilemap-height test-tilemap))
(test "tilemap-in-bounds? valid" #t (tilemap-in-bounds? test-tilemap 5 5))
(test "tilemap-in-bounds? invalid x" #f (tilemap-in-bounds? test-tilemap 15 5))
(test "tilemap-in-bounds? invalid y" #f (tilemap-in-bounds? test-tilemap 5 15))
(test "tilemap-ref tile id" 'floor (tile-id (tilemap-ref test-tilemap 5 5)))

;;; Set a wall
(tilemap-set-type! test-tilemap 3 3 tile-wall)
(test "tilemap-set-type! works" 'wall (tile-id (tilemap-ref test-tilemap 3 3)))
(test "tilemap-walkable? floor" #t (tilemap-walkable? test-tilemap 5 5))
(test "tilemap-walkable? wall" #f (tilemap-walkable? test-tilemap 3 3))

;;; ====
;;; Entity Tests
;;; ====

(test-section "Entity - Components")

(define pos-comp (make-position-component 10 20))
(test "position-x" 10 (position-x pos-comp))
(test "position-y" 20 (position-y pos-comp))
(test "position->point" '(10 . 20) (position->point pos-comp))

(define stats-comp (make-stats-component 30 50 10 5 100))
(test "stats-hp" 30 (stats-hp stats-comp))
(test "stats-max-hp" 50 (stats-max-hp stats-comp))
(test "stats-attack" 10 (stats-attack stats-comp))
(test "stats-alive?" #t (stats-alive? stats-comp))

(define dead-stats (stats-set-hp stats-comp 0))
(test "stats-alive? dead" #f (stats-alive? dead-stats))

(test-section "Entity - Entity Creation")

(reset-id-counter!)
(define test-entity (make-entity))
(test "entity-id" 0 (entity-id test-entity))

(define entity-with-pos
  (entity-add-component test-entity (make-position-component 5 10)))
(test "entity-has-component? true" #t (entity-has-component? entity-with-pos 'position))
(test "entity-has-component? false" #f (entity-has-component? entity-with-pos 'stats))
(test "entity-x" 5 (entity-x entity-with-pos))
(test "entity-y" 10 (entity-y entity-with-pos))

(test-section "Entity - Entity Movement")

(define moved-entity (entity-move-to entity-with-pos 15 25))
(test "entity-move-to x" 15 (entity-x moved-entity))
(test "entity-move-to y" 25 (entity-y moved-entity))

(define moved-by-entity (entity-move-by entity-with-pos 3 -2))
(test "entity-move-by x" 8 (entity-x moved-by-entity))
(test "entity-move-by y" 8 (entity-y moved-by-entity))

(define moved-dir-entity (entity-move-dir entity-with-pos 'south))
(test "entity-move-dir y" 11 (entity-y moved-dir-entity))

(test-section "Entity - Factory Functions")

(reset-id-counter!)
(define test-player (make-player "Hero" #\@ 10 10))
(test "make-player has position" #t (entity-has-component? test-player 'position))
(test "make-player has stats" #t (entity-has-component? test-player 'stats))
(test "make-player has inventory" #t (entity-has-component? test-player 'inventory))
(test "make-player char" #\@ (entity-char test-player))
(test "entity-is-player?" #t (entity-is-player? test-player))

(define test-monster (make-monster "Goblin" #\g 5 5 'hunt))
(test "make-monster has ai" #t (entity-has-component? test-monster 'ai))
(test "entity-is-monster?" #t (entity-is-monster? test-monster))
(test "ai-behavior" 'hunt (ai-behavior (entity-ai test-monster)))

;;; ====
;;; Event Tests
;;; ====

(test-section "Event - Event Creation")

(define test-event (make-event 'entity-moved 1 2 '((dx . 1) (dy . 0)) 5))
(test "event type" 'entity-moved (event%-type test-event))
(test "event source" 1 (event%-source test-event))
(test "event target" 2 (event%-target test-event))
(test "event timestamp" 5 (event%-timestamp test-event))

(test-section "Event - Event Queue")

(define eq (make-event-queue))
(test "event-queue-empty? empty" #t (event-queue-empty? eq))
(event-queue-push! eq (make-event 'test-1))
(test "event-queue-empty? not empty" #f (event-queue-empty? eq))
(event-queue-push! eq (make-event 'test-2))
(test "event-queue-size" 2 (event-queue-size eq))
(define popped (event-queue-pop! eq))
(test "event-queue-pop! fifo" 'test-1 (event%-type popped))
(event-queue-clear! eq)
(test "event-queue-clear!" #t (event-queue-empty? eq))

;;; ====
;;; Action Tests
;;; ====

(test-section "Action - Action Creation")

(define move-action (make-move-action 1 'north))
(test "action-type move" 'move (action-type move-action))
(test "action-actor-id" 1 (action-actor-id move-action))
(test "action-target direction" 'north (action-target move-action))

(define attack-action (make-attack-action 1 2))
(test "action-type attack" 'attack (action-type attack-action))
(test "attack target" 2 (action-target attack-action))

(define wait-action (make-wait-action 1))
(test "action-type wait" 'wait (action-type wait-action))
(test "wait has lower cost" #t (< (action-cost wait-action) 100))

(test-section "Action - Action Queue")

(define aq (make-action-queue))
(test "action-queue-empty? empty" #t (action-queue-empty? aq))
(queue-action! aq move-action)
(queue-action! aq attack-action)
(test "action-queue-length" 2 (action-queue-length aq))
(define dequeued (dequeue-action! aq))
(test "dequeue-action! fifo" 'move (action-type dequeued))

;;; ====
;;; World Tests
;;; ====

(test-section "World - World Creation")

(define test-world-tm (make-tilemap 20 15 tile-floor))
(define test-world (make-world test-world-tm 1))
(test "world-width" 20 (world-width test-world))
(test "world-height" 15 (world-height test-world))
(test "world-level" 1 (world-level test-world))
(test "world-entity-count empty" 0 (world-entity-count test-world))

(test-section "World - Entity Management")

(reset-id-counter!)
(define w-player (make-player "Test" #\@ 10 7))
(define world-with-player (world-spawn-player test-world w-player))
(test "world-spawn-player adds entity" 1 (world-entity-count world-with-player))
(test "world-player-id set" 0 (world-player-id world-with-player))
(test "world-get-player" 0 (entity-id (world-get-player world-with-player)))

(define w-monster (make-monster "Goblin" #\g 5 5 'idle))
(define world-with-monster (world-add-entity world-with-player w-monster))
(test "world-add-entity count" 2 (world-entity-count world-with-monster))

(test-section "World - Spatial Queries")

(test "world-entities-at player pos" 1 (length (world-entities-at world-with-monster 10 7)))
(test "world-entities-at monster pos" 1 (length (world-entities-at world-with-monster 5 5)))
(test "world-entities-at empty" 0 (length (world-entities-at world-with-monster 15 10)))
(test "world-entity-at player" 0 (entity-id (world-entity-at world-with-monster 10 7)))

(test-section "World - Movement and Collision")

(test "world-tile-walkable? floor" #t (world-tile-walkable? world-with-monster 3 3))
(tilemap-set-type! (world-tilemap world-with-monster) 3 3 tile-wall)
(test "world-tile-walkable? wall" #f (world-tile-walkable? world-with-monster 3 3))
(test "world-blocked? by wall" #t (world-blocked? world-with-monster 3 3))
(test "world-blocked? by entity" #t (world-blocked? world-with-monster 10 7))
(test "world-blocked? empty" #f (world-blocked? world-with-monster 12 8))

;;; Move the player
(define world-moved (world-move-entity world-with-monster 0 'east))
(test "world-move-entity x" 11 (entity-x (world-get-player world-moved)))

;;; Try to move into wall
(define world-blocked-move (world-move-entity world-moved 0 'west))
(set! world-blocked-move (world-move-entity world-blocked-move 0 'west))
;;; Position would be at x=9 after two valid west moves

(test-section "World - Pathfinding")

(define path-world-tm (make-tilemap 10 10 tile-floor))
(tilemap-set-type! path-world-tm 5 3 tile-wall)
(tilemap-set-type! path-world-tm 5 4 tile-wall)
(tilemap-set-type! path-world-tm 5 5 tile-wall)
(define path-world (make-world path-world-tm))

(define path (world-find-path path-world '(3 . 4) '(7 . 4)))
(test "world-find-path returns path" #t (list? path))
(test "world-find-path path not empty" #t (> (length path) 0))
(test "world-find-path ends at goal" '(7 . 4) (if (null? path) #f (car (reverse path))))

;;; ====
;;; Turn System Tests
;;; ====

(test-section "Turn - Actor Queue")

(reset-id-counter!)
(define turn-player (make-player "P" #\@ 5 5))
(define turn-monster (make-monster "M" #\g 3 3 'idle))
(define turn-world (world-add-entity
                    (world-spawn-player (make-world (make-tilemap 10 10 tile-floor))
                                        turn-player)
                    turn-monster))

(define aq2 (make-actor-queue))
(actor-queue-add! aq2 (make-actor-entry 0 0 10 100))  ; player
(actor-queue-add! aq2 (make-actor-entry 1 0 5 100))   ; monster
(test "actor-queue-size" 2 (actor-queue-size aq2))

;;; Test energy gain
(advance-actor-energy! aq2 10)
(test "advance-actor-energy!" #t (> (actor-entry-energy (actor-queue-find aq2 0)) 0))

(test-section "Turn - Turn State")

(define ts (make-turn-state))
(test "turn-state current-turn" 0 (turn-state-current-turn ts))
(test "turn-state phase" 'start-turn (turn-state-phase ts))

(begin-turn! ts)
(test "begin-turn! increments" 1 (turn-state-current-turn ts))
(test "turn-state-in-phase?" #t (turn-state-in-phase? ts 'start-turn))

;;; ====
;;; Integration Test
;;; ====

(test-section "Integration - Simple Game Setup")

;;; Create a small dungeon
(define int-tm (make-tilemap 20 10 tile-wall))
;; Fill interior with floor
(let loop-y ([y 1])
     (when (< y 9)
           (let loop-x ([x 1])
                (when (< x 19)
                      (tilemap-set-type! int-tm x y tile-floor)
                      (loop-x (+ x 1))))
           (loop-y (+ y 1))))

;;; Add doors and stairs
(tilemap-set-type! int-tm 10 1 tile-door-closed)
(tilemap-set-type! int-tm 18 8 tile-stairs-down)

(define int-world (make-world int-tm 1))

;;; Spawn player
(reset-id-counter!)
(define int-player (make-player "Hero" #\@ 2 2))
(set! int-world (world-spawn-player int-world int-player))

;;; Spawn monsters
(define int-goblin (make-monster "Goblin" #\g 15 5 'hunt))
(define int-orc (make-monster "Orc" #\o 8 7 'guard))
(set! int-world (world-add-entity int-world int-goblin))
(set! int-world (world-add-entity int-world int-orc))

(test "integration entity count" 3 (world-entity-count int-world))
(test "integration player at 2,2" #t (entity-at-point? (world-get-player int-world) '(2 . 2)))

;;; Render the world
(define int-canvas (world->canvas int-world))
(test "integration canvas width" 20 (canvas-width int-canvas))
(test "integration player visible" #\@ (canvas-ref int-canvas 2 2))
(test "integration goblin visible" #\g (canvas-ref int-canvas 15 5))
(test "integration wall visible" #\# (canvas-ref int-canvas 0 0))
(test "integration door visible" #\+ (canvas-ref int-canvas 10 1))

;;; Move player
(set! int-world (world-move-entity int-world 0 'east))
(test "integration player moved" 3 (entity-x (world-get-player int-world)))

;;; Open door
(world-open-door! int-world 10 1)
(test "integration door opened" #\' (tile-char (tilemap-ref (world-tilemap int-world) 10 1)))

;;; Display final render
(display "\nIntegration test dungeon:\n")
(display (canvas->string (world->canvas int-world)))
(newline)

;;; ====
;;; Test Summary
;;; ====

(test-summary)

;;; Exit with appropriate code
(if (= *tests-failed* 0)
    (exit 0)
    (exit 1))
