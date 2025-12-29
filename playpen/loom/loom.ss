;;; playpen/loom/loom.ss — Loom: The Game-Weaving Framework
;;;
;;; A generic 2D tile-based turn-based game SDK.
;;; Provides the foundation for roguelike dungeon crawlers and similar games.
;;;
;;; This is Playpen code: the framework for weaving games in The Fold.
;;; "Loom" — where game worlds are woven from threads of logic.
;;;
;;; Usage:
;;;   (load "playpen/loom/loom.ss")
;;;
;;; This loads all SDK modules in the correct order:
;;;   1. core.ss    — Core types and utilities
;;;   2. tile.ss    — Tile system
;;;   3. entity.ss  — Entity-component system
;;;   4. event.ss   — Event system
;;;   5. action.ss  — Action/command system
;;;   6. turn.ss    — Turn-based game loop
;;;   7. world.ss   — World state management
;;;   8. combat.ss  — Combat system (damage, attacks)
;;;   9. ai.ss      — AI behaviors (hunt, guard, wander)
;;;
;;; After loading, you have access to all SDK functions for building games.
;;;
;;; For the Spell DSL (declarative game building), load spell.ss instead:
;;;   (load "playpen/loom/spell/spell.ss")
;;;
;;; Spell provides: def-entity, def-component, def-behavior, def-action, def-game

;;; ============================================================
;;; Load Order
;;; ============================================================

;;; Load core utilities first (no dependencies)
(load "playpen/loom/core.ss")

;;; Load tile system (depends on core)
(load "playpen/loom/tile.ss")

;;; Load entity system (depends on core)
(load "playpen/loom/entity.ss")

;;; Load event system (depends on core)
(load "playpen/loom/event.ss")

;;; Load action system (depends on core)
(load "playpen/loom/action.ss")

;;; Load turn system (depends on core, entity, action, event)
(load "playpen/loom/turn.ss")

;;; Load world system (depends on all above)
(load "playpen/loom/world.ss")

;;; Load combat system (depends on entity, world)
(load "playpen/loom/combat.ss")

;;; Load AI system (depends on entity, world, combat)
(load "playpen/loom/ai.ss")

;;; ============================================================
;;; SDK Version
;;; ============================================================

(define *loom-version* "0.2.0")

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

(display "Loom v")
(display *loom-version*)
(display " loaded — the game-weaving framework.\n")
(display "Modules: core, tile, entity, event, action, turn, world, combat, ai\n")
