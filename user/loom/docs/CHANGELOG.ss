;;; playpen/loom/docs/CHANGELOG.ss — RPG SDK Change Log

;;; ====
;;; Version 0.1.0 (2025-12-26)
;;; ====

;;; FEATURES ADDED:
;;;   - Combat system (combat.ss)
;;;     * Damage calculation with variance and critical hits
;;;     * Attack resolution with combat results
;;;     * Support for different damage types
;;;     * Armor and defense mechanics
;;;
;;;   - AI behavior system (ai.ss)
;;;     * Reference implementations: idle, wander, guard, hunt, flee
;;;     * AI action types: move, attack, wait, flee, pickup, use
;;;     * Dispatcher for entity AI behavior lookup
;;;     * Pathfinding integration for smart movement
;;;     * Hostility checking via faction system
;;;
;;;   - World-level inventory operations (world.ss)
;;;     * world-pickup-item, world-drop-item
;;;     * world-transfer-item, world-items-at
;;;     * Functional updates maintaining immutability
;;;
;;;   - Mutation semantics documentation
;;;     * Added comprehensive docs to world.ss and entity.ss
;;;     * Clarified functional vs imperative patterns
;;;     * Best practice examples for common operations
;;;
;;;   - Integration examples (example-combat-integration.ss)
;;;     * Basic combat demonstration
;;;     * AI behavior showcase
;;;     * Turn-based combat loop
;;;     * Inventory system usage
;;;     * Complete multi-system integration

;;; FIXES:
;;;   - Fixed bracket/parenthesis mismatch in world.ss:556 (pathfinding)
;;;   - Fixed case statement syntax in ai.ss (brackets → parens)
;;;   - Restructured all example functions to comply with Scheme's
;;;     internal define rules (defines before expressions)
;;;   - Updated rpg.ss to load new combat and AI modules

;;; IMPROVEMENTS:
;;;   - Added detailed API documentation to all new modules
;;;   - Improved code organization and consistency
;;;   - Enhanced type signatures and contracts
;;;   - Better error messages in combat system

;;; DOCUMENTATION:
;;;   - Created comprehensive README.ss
;;;   - Archived dogfooding notes to docs/ subdirectory
;;;   - Added this CHANGELOG.ss
;;;   - Documented SDK architecture and patterns

;;; TESTS:
;;;   - All existing tests pass
;;;   - Combat system tested via integration examples
;;;   - AI behaviors validated in example scenarios

;;; KNOWN ISSUES:
;;;   - Warning: member call arity in world.ss:535 (non-blocking)
;;;   - Item drop functionality is stubbed (needs item registry)

;;; ====
;;; Pre-release Development (2025-12-25 to 2025-12-26)
;;; ====

;;; INITIAL IMPLEMENTATION:
;;;   - Core ECS architecture (entity.ss, core.ss)
;;;   - Tilemap and terrain system (tile.ss)
;;;   - Event queue (event.ss)
;;;   - Action system (action.ss)
;;;   - Turn-based energy system (turn.ss)
;;;   - World state and spatial queries (world.ss)
;;;   - Pathfinding (A*) and FOV (shadowcasting)
;;;   - Demo game with procedural generation
;;;   - Comprehensive test suite

;;; DOGFOODING PROCESS:
;;;   - Identified critical gaps (combat, AI, inventory)
;;;   - Tested SDK ergonomics and API consistency
;;;   - Validated functional update patterns
;;;   - Rated 8.5/10 with potential for 9.5/10

;;; RESOLVED DOGFOODING FINDINGS:
;;;   ✓ Combat system implemented
;;;   ✓ AI behavior library created
;;;   ✓ World-level inventory helpers added
;;;   ✓ Mutation semantics documented
;;;   ✓ Integration examples created
;;;   ✓ API inconsistencies resolved

;;; ====
;;; Future Roadmap
;;; ====

;;; PLANNED FEATURES:
;;;   - Save/load system
;;;   - Item effects and consumables
;;;   - Magic and spell system
;;;   - Status effects (poison, stun, buffs, debuffs)
;;;   - Equipment system (weapons, armor, accessories)
;;;   - Dialogue and quest system
;;;   - Sound propagation and stealth
;;;   - Advanced AI (flanking, formations, tactics)
;;;   - Procedural content generation improvements

;;; POTENTIAL OPTIMIZATIONS:
;;;   - Chunked tilemap for large worlds
;;;   - Spatial partitioning for entities
;;;   - Lazy FOV computation
;;;   - Event batching for performance

;;; INFRASTRUCTURE:
;;;   - CI/CD testing pipeline
;;;   - Performance benchmarks
;;;   - Documentation generator
;;;   - Tutorial series
