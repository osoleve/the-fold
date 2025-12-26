;;; playpen/rpg/QUICKSTART.ss — 5-Minute RPG SDK Guide
;;;
;;; Get started building roguelikes in under 5 minutes!

;;; ============================================================
;;; 1. Load the SDK (1 line)
;;; ============================================================

(load "playpen/rpg/rpg.ss")

;;; ============================================================
;;; 2. Create a world (3 lines)
;;; ============================================================

(define tm (make-tilemap 20 20))
(tilemap-fill! tm tile-floor)
(define world (make-world tm))

;;; ============================================================
;;; 3. Add entities (4 lines)
;;; ============================================================

(define hero (make-player "Hero" #\@ 10 10))
(set! world (world-spawn-player world hero))

(define goblin (make-monster "Goblin" #\g 15 10 'hunt))
(set! world (world-add-entity world goblin))

;;; ============================================================
;;; 4. Do things!
;;; ============================================================

;;; Move the player:
(set! world (world-move-entity world (entity-id hero) 'east))

;;; Let AI decide goblin's action:
(define action (ai-decide-action world goblin))
(ai-action-type action)    ; => 'move or 'attack
(ai-action-data action)     ; => direction or target-id

;;; Run combat:
(define result (entity-melee-attack hero goblin))
(combat-result-message result)  ; => "Hero attacks Goblin for 8 damage!"
(define hit-goblin (combat-result-defender result))

;;; Update world with damaged goblin:
(set! world (world-replace-entity world (entity-id goblin) hit-goblin))

;;; ============================================================
;;; 5. Try the examples!
;;; ============================================================

(load "playpen/rpg/example-combat-integration.ss")
(run-all-examples)

;;; Or play the demo game:
(load "playpen/rpg/demo-game.ss")
(new-game!)
(render-game)
(move-player 'north)

;;; ============================================================
;;; Next Steps
;;; ============================================================
;;;
;;; - Read README.ss for comprehensive documentation
;;; - Study example-combat-integration.ss for patterns
;;; - Explore demo-game.ss to see a complete game
;;; - Check docs/CHANGELOG.ss for latest features
;;;
;;; Happy hacking! 🎮

(display "Quick Start Guide loaded. Start coding above! ↑\n")
