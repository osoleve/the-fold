;;; playpen/rpg/README.ss — RPG SDK Documentation
;;;
;;; A complete 2D tile-based turn-based RPG SDK for building roguelikes,
;;; dungeon crawlers, and tactical RPGs in Scheme.
;;;
;;; Version: 0.1.0
;;; Status: Production-ready with combat and AI systems

;;; ============================================================
;;; Quick Start
;;; ============================================================
;;;
;;; Load the SDK:
;;;   (load "playpen/rpg/rpg.ss")
;;;
;;; Try the examples:
;;;   (load "playpen/rpg/example-combat-integration.ss")
;;;   (run-all-examples)
;;;
;;; Play the demo game:
;;;   (load "playpen/rpg/demo-game.ss")
;;;   (new-game!)
;;;
;;; Run tests:
;;;   (load "playpen/rpg/test-rpg.ss")

;;; ============================================================
;;; SDK Architecture
;;; ============================================================
;;;
;;; The SDK is built on an Entity-Component System (ECS) with
;;; functional updates and a hybrid mutation model:
;;;
;;; CORE MODULES (load via rpg.ss):
;;;   core.ss    - Fundamental data structures and utilities
;;;   tile.ss    - Tilemap, tiles, and terrain
;;;   entity.ss  - Entity-Component System
;;;   event.ss   - Event queue and game events
;;;   action.ss  - Player/AI actions and action processing
;;;   turn.ss    - Turn order, energy system, turn processing
;;;   world.ss   - World state, spatial queries, pathfinding
;;;   combat.ss  - Damage calculation, attack resolution
;;;   ai.ss      - AI behaviors (hunt, guard, wander, flee)
;;;
;;; EXAMPLE CODE:
;;;   example-combat-integration.ss  - Integration examples
;;;   demo-game.ss                   - Complete playable game
;;;   test-rpg.ss                    - Test suite
;;;
;;; DOCUMENTATION:
;;;   README.ss                      - This file
;;;   docs/dogfood-notes.ss          - Original dogfooding findings
;;;   docs/post-dogfood-findings.ss  - Improvements implemented

;;; ============================================================
;;; Key Concepts
;;; ============================================================
;;;
;;; MUTATION SEMANTICS:
;;;   - Entity operations: FUNCTIONAL (return new entities)
;;;   - World operations: FUNCTIONAL (return new worlds)
;;;   - Tilemap operations: IMPERATIVE (mutate in place with !)
;;;   - Spatial index: IMPERATIVE (internal to world)
;;;
;;;   Always capture functional returns:
;;;     ✓ (set! world (world-add-entity world entity))
;;;     ✗ (world-add-entity world entity)  ; Lost!
;;;
;;; ENTITY-COMPONENT SYSTEM:
;;;   - Entities are bags of components (no inheritance)
;;;   - Components: position, renderable, stats, ai, inventory, etc.
;;;   - Query by component: (entity-has-component? e 'stats)
;;;
;;; TURN SYSTEM:
;;;   - Energy-based: entities gain energy each tick
;;;   - When energy >= 100, entity takes a turn
;;;   - Different speeds: fast monsters move more often
;;;
;;; COMBAT SYSTEM:
;;;   - Calculate damage: base attack - defense + modifiers
;;;   - Critical hits, armor, damage variance
;;;   - Returns CombatResult with updated entities
;;;
;;; AI SYSTEM:
;;;   - Behaviors: idle, wander, guard, hunt, flee
;;;   - Returns AIAction: move, attack, wait, etc.
;;;   - Uses pathfinding and spatial queries

;;; ============================================================
;;; Common Patterns
;;; ============================================================
;;;
;;; Creating an entity:
;;;   (define hero (make-player "Hero" #\@ 10 10))
;;;   (define goblin (make-monster "Goblin" #\g 15 10 'hunt))
;;;
;;; Adding to world:
;;;   (set! world (world-add-entity world hero))
;;;
;;; Updating an entity:
;;;   (set! world (world-update-entity world entity-id
;;;                 (lambda (e) (entity-damage e 10))))
;;;
;;; Processing a turn:
;;;   (let ([action (ai-decide-action world entity)])
;;;     (case (ai-action-type action)
;;;       ((attack) (handle-attack (ai-action-data action)))
;;;       ((move) (handle-move (ai-action-data action)))
;;;       ((wait) world)))
;;;
;;; Running combat:
;;;   (let ([result (entity-melee-attack attacker defender)])
;;;     (display (combat-result-message result))
;;;     (set! world (world-replace-entity world
;;;                   (entity-id defender)
;;;                   (combat-result-defender result))))

;;; ============================================================
;;; Examples Overview
;;; ============================================================
;;;
;;; example-combat-integration.ss:
;;;   - Basic combat demonstration
;;;   - AI behavior showcase (hunt, guard, wander)
;;;   - Complete turn-based combat loop
;;;   - Inventory system usage
;;;   - Multi-system integration
;;;
;;; demo-game.ss:
;;;   - Full playable roguelike
;;;   - Procedural dungeon generation
;;;   - Player movement and combat
;;;   - Enemy AI with different behaviors
;;;   - Items and inventory
;;;   - FOV and fog of war
;;;   - Multiple dungeon levels
;;;
;;; test-rpg.ss:
;;;   - Comprehensive test suite
;;;   - Tests all core modules
;;;   - Regression testing for updates

;;; ============================================================
;;; Module Dependencies
;;; ============================================================
;;;
;;; Load order (handled by rpg.ss):
;;;   1. core.ss       - No dependencies
;;;   2. tile.ss       - Depends on: core
;;;   3. entity.ss     - Depends on: core
;;;   4. event.ss      - Depends on: core
;;;   5. action.ss     - Depends on: core, entity
;;;   6. turn.ss       - Depends on: core, entity, action, event
;;;   7. world.ss      - Depends on: tile, entity
;;;   8. combat.ss     - Depends on: entity
;;;   9. ai.ss         - Depends on: entity, world, combat

;;; ============================================================
;;; API Reference Summary
;;; ============================================================
;;;
;;; See individual module files for detailed API documentation.
;;; Each module has comprehensive doc comments for all exports.
;;;
;;; WORLD:
;;;   make-world, world-add-entity, world-remove-entity
;;;   world-get-entity, world-entities-at, world-all-entities
;;;   world-find-path, world-has-los?, world-compute-fov!
;;;   world-pickup-item, world-drop-item, world-items-at
;;;
;;; ENTITY:
;;;   make-entity, entity-add-component, entity-get-component
;;;   make-player, make-monster, make-item
;;;   entity-damage, entity-heal, entity-move-to
;;;   entity-alive?, entity-is-player?, entity-is-monster?
;;;
;;; COMBAT:
;;;   calculate-damage, entity-melee-attack
;;;   combat-result-attacker, combat-result-defender
;;;   combat-result-damage, combat-result-killed?
;;;   combat-result-critical?, combat-result-message
;;;
;;; AI:
;;;   ai-decide-action
;;;   ai-idle, ai-wander, ai-guard, ai-hunt, ai-flee
;;;   make-ai-action, ai-action-type, ai-action-data
;;;
;;; TURN:
;;;   make-turn-state, turn-state-add-actor
;;;   process-turn!, run-until-player-turn!
;;;   begin-turn!, end-turn!
;;;
;;; TILE:
;;;   make-tilemap, tilemap-get, tilemap-set-type!
;;;   tile-floor, tile-wall, tile-door-closed, tile-door-open
;;;   tile-blocks-movement?, tile-blocks-sight?

;;; ============================================================
;;; Performance Notes
;;; ============================================================
;;;
;;; The SDK is optimized for typical roguelike workloads:
;;;   - Spatial queries: O(1) via hash-based index
;;;   - Entity lookup: O(1) via alist (small N)
;;;   - Pathfinding: A* with max iteration limit
;;;   - FOV: Shadowcasting algorithm
;;;
;;; For large worlds (>10,000 tiles), consider:
;;;   - Chunking the tilemap
;;;   - Spatial partitioning for entities
;;;   - Lazy FOV computation

;;; ============================================================
;;; Extending the SDK
;;; ============================================================
;;;
;;; To add new features:
;;;
;;; 1. New component types:
;;;    - Define component structure in entity.ss
;;;    - Add factory functions if needed
;;;
;;; 2. New AI behaviors:
;;;    - Implement behavior function in ai.ss
;;;    - Add case to ai-decide-action dispatcher
;;;
;;; 3. New actions:
;;;    - Define action type in action.ss
;;;    - Implement action handler
;;;    - Update turn processing if needed
;;;
;;; 4. New systems (magic, crafting, etc.):
;;;    - Create new module file
;;;    - Add to rpg.ss load order
;;;    - Follow functional update pattern

;;; ============================================================
;;; Known Issues & Roadmap
;;; ============================================================
;;;
;;; Current limitations:
;;;   - No save/load system yet
;;;   - No scripting/dialogue system
;;;   - Limited item effects (stubs in place)
;;;   - No multiplayer/networking
;;;
;;; Potential improvements:
;;;   - Implement item effects system
;;;   - Add magic/spell casting
;;;   - Equipment and inventory UI
;;;   - Status effects (poison, stun, etc.)
;;;   - Noise/sound propagation
;;;   - Advanced AI (flanking, formations)

;;; ============================================================
;;; License & Credits
;;; ============================================================
;;;
;;; This SDK is part of The Fold project.
;;; Built by Claude (Opus & Sonnet tiers) via dogfooding and iteration.
;;;
;;; See docs/dogfood-notes.ss for development history.

;;; ============================================================
;;; Version History
;;; ============================================================
;;;
;;; v0.1.0 (2025-12-26):
;;;   - Initial release with core ECS
;;;   - Turn-based system with energy
;;;   - Spatial queries and pathfinding
;;;   - Combat system with damage calculation
;;;   - AI behaviors (hunt, guard, wander, flee)
;;;   - Inventory management
;;;   - FOV and line-of-sight
;;;   - Complete demo game
;;;   - Comprehensive test suite

(display "RPG SDK Documentation loaded. See README.ss for details.\n")
