;;; playpen/rpg/rpg.ss — RPG SDK Main Entry Point
;;;
;;; A generic 2D tile-based turn-based RPG SDK.
;;; Provides the foundation for roguelike dungeon crawlers and similar games.
;;;
;;; This is Playpen code: the SDK for building RPGs in The Fold.
;;;
;;; Usage:
;;;   (load "playpen/rpg/rpg.ss")
;;;
;;; This loads all SDK modules in the correct order:
;;;   1. core.ss    — Core types and utilities
;;;   2. tile.ss    — Tile system
;;;   3. entity.ss  — Entity-component system
;;;   4. event.ss   — Event system
;;;   5. action.ss  — Action/command system
;;;   6. turn.ss    — Turn-based game loop
;;;   7. world.ss   — World state management
;;;
;;; After loading, you have access to all SDK functions for building games.

;;; ============================================================
;;; Load Order
;;; ============================================================

;;; Load core utilities first (no dependencies)
(load "playpen/rpg/core.ss")

;;; Load tile system (depends on core)
(load "playpen/rpg/tile.ss")

;;; Load entity system (depends on core)
(load "playpen/rpg/entity.ss")

;;; Load event system (depends on core)
(load "playpen/rpg/event.ss")

;;; Load action system (depends on core)
(load "playpen/rpg/action.ss")

;;; Load turn system (depends on core, entity, action, event)
(load "playpen/rpg/turn.ss")

;;; Load world system (depends on all above)
(load "playpen/rpg/world.ss")

;;; ============================================================
;;; SDK Version
;;; ============================================================

(define *rpg-sdk-version* "0.1.0")

;;; ============================================================
;;; Quick Start Helpers
;;; ============================================================

;;; These are convenience functions for getting started quickly.

;;; create-simple-dungeon : Nat × Nat -> World
;;; Create a simple dungeon with walls around the edges and floor in the middle.
(define (create-simple-dungeon width height)
  (let* ([tm (make-tilemap width height tile-wall)])
    ;; Fill interior with floor
    (let loop-y ([y 1])
      (when (< y (- height 1))
        (let loop-x ([x 1])
          (when (< x (- width 1))
            (tilemap-set-type! tm x y tile-floor)
            (loop-x (+ x 1))))
        (loop-y (+ y 1))))
    (make-world tm)))

;;; spawn-player-at : World × Int × Int × String -> World
;;; Create and spawn a player at the given position.
(define (spawn-player-at world x y name)
  (let ([player (make-player name #\@ x y)])
    (world-spawn-player world player)))

;;; spawn-monster-at : World × Int × Int × String × Char × Symbol -> World
;;; Create and spawn a monster at the given position.
(define (spawn-monster-at world x y name char behavior)
  (let ([monster (make-monster name char x y behavior)])
    (world-add-entity world monster)))

;;; ============================================================
;;; Example Game Setup
;;; ============================================================

;;; This demonstrates how to set up a basic game.
;;; Uncomment and modify for your own game.

#|
;;; Create a 40x20 dungeon
(define my-world (create-simple-dungeon 40 20))

;;; Spawn player in the middle
(set! my-world (spawn-player-at my-world 20 10 "Hero"))

;;; Add some monsters
(set! my-world (spawn-monster-at my-world 10 5 "Goblin" #\g 'wander))
(set! my-world (spawn-monster-at my-world 30 15 "Orc" #\o 'hunt))

;;; Add stairs
(tilemap-set-type! (world-tilemap my-world) 35 18 tile-stairs-down)

;;; Render to see the result
(display (canvas->string (world->canvas my-world)))
(newline)
|#

;;; ============================================================
;;; SDK Loaded Message
;;; ============================================================

(display "RPG SDK v")
(display *rpg-sdk-version*)
(display " loaded.\n")
(display "Modules: core, tile, entity, event, action, turn, world\n")
