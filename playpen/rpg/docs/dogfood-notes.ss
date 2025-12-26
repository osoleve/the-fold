;;; playpen/rpg/dogfood-notes.ss
;;;
;;; Documentation of RPG SDK dogfooding exercise.
;;; This file contains findings from building demo-game.ss.
;;;
;;; Format: Structured S-expressions for programmatic access.

(define *dogfood-exercise*
  '((project . rpg-sdk-dogfood)
    (version . "0.1.0")
    (date . "2025-12-26")
    (tester . claude)
    (model . sonnet-4.5)
    (test-vehicle . demo-game.ss)
    (lines-of-code . 484)
    (duration . "~2 hours")))

(define *demo-game-features*
  '((procedural-generation
      (description . "Rooms and corridors dungeon algorithm")
      (map-size . (60 25))
      (room-count . 15))
    (player-system
      (description . "Movement in 8 directions with collision")
      (stats . (hp attack defense))
      (rendering . ascii))
    (enemy-system
      (count . 3)
      (types . (goblin orc troll))
      (behaviors . (hunt guard wander)))
    (combat
      (type . turn-based)
      (mechanics . melee-only)
      (damage . random-range)
      (death . permanent))
    (game-loop
      (turn-order . player-then-monsters)
      (ui . (status-bar event-log canvas)))
    (victory-condition . stairs-descent)))

(define *sdk-components-tested*
  '((core-module
      (direction->delta . tested)
      (manhattan-distance . tested)
      (point-add . tested)
      (point-sub . tested))
    (tile-module
      (make-tilemap . tested)
      (tilemap-set-type! . tested)
      (tilemap-walkable? . tested)
      (tile-types . (floor wall stairs)))
    (entity-module
      (make-player . tested)
      (make-monster . tested)
      (entity-move-by . tested)
      (entity-move-dir . tested)
      (entity-take-damage . tested)
      (entity-alive? . tested))
    (world-module
      (make-world . tested)
      (world-spawn-player . tested)
      (world-add-entity . tested)
      (world-entity-at . tested)
      (world-tile-walkable? . tested)
      (world-move-entity . tested)
      (world->canvas . tested))
    (canvas-module
      (canvas->string . tested))
    (turn-module . not-tested)
    (event-module . not-tested)
    (action-module . not-tested)
    (pathfinding . not-tested)))

(define *sdk-strengths*
  '((api-design
      (rating . excellent)
      (notes . "Clean, intuitive, self-documenting names")
      (examples . (make-player world-spawn-player entity-alive?)))
    (feature-completeness
      (rating . comprehensive)
      (notes . "Has everything needed for basic roguelike")
      (coverage . (tiles entities spatial-queries pathfinding rendering)))
    (utilities
      (rating . excellent)
      (notes . "Direction system and point math save boilerplate")
      (used . (direction->delta opposite-direction manhattan-distance)))
    (rendering
      (rating . excellent)
      (notes . "One-line render: (canvas->string (world->canvas w))")
      (performance . acceptable-for-turn-based))
    (documentation
      (rating . good)
      (notes . "Comprehensive docstrings with type signatures")
      (missing . (tutorial architecture-doc best-practices)))))

(define *critical-gaps*
  '((world-update-entity
      (severity . critical)
      (impact . "Required for any entity state changes")
      (workaround-lines . 3)
      (recommendation . "Add to world.ss")
      (signature . "World × Nat × Entity → World"))
    (combat-system
      (severity . high)
      (impact . "Stats exist but no damage calculation")
      (workaround-lines . 10)
      (recommendation . "Add calculate-damage and entity-attack")
      (signature . "Entity × Entity → (Nat × Event)"))
    (ai-implementation
      (severity . medium)
      (impact . "Behaviors are just symbols with no logic")
      (workaround-lines . 60)
      (recommendation . "Provide reference implementations")
      (needed . (hunt-ai guard-ai wander-ai)))
    (turn-integration
      (severity . medium)
      (impact . "Energy system exists but not wired to World")
      (workaround . "Simple alternation instead")
      (recommendation . "Add world-process-turn"))
    (inventory-functions
      (severity . medium)
      (impact . "Component exists but unusable")
      (workaround . "Implemented nothing - no items in demo")
      (recommendation . "Add entity-add-item, entity-remove-item, etc"))))

(define *api-inconsistencies*
  '((mutation-model
      (issue . "Mix of functional and imperative styles")
      (functional . (entity-move-to world-add-entity entity-take-damage))
      (imperative . (tilemap-set-type! world-open-door!))
      (confusion . "Easy to forget to capture return values")
      (recommendation . "Pick one model or document clearly"))
    (incomplete-features
      (inventory . component-exists-no-functions)
      (ai . symbols-defined-no-implementation)
      (turn-system . scheduler-exists-not-integrated))))

(define *missing-for-production*
  '((essential
      (save-load . "No serialization support")
      (item-system . "Inventory component but no item objects")
      (status-effects . "Poison, stun, buffs/debuffs")
      (experience . "No leveling or progression")
      (combat-formulas . "Critical hits, accuracy, resistances"))
    (nice-to-have
      (fov . "Field of view / line of sight")
      (sound-hooks . "Event hooks for feedback")
      (dialog . "NPC conversations")
      (quests . "Goals and objectives")
      (magic . "Spells and abilities"))
    (advanced
      (multiplayer . "Network sync")
      (map-editor . "Level design tools")
      (modding-api . "User extensibility"))))

(define *performance-notes*
  '((entity-lookups
      (complexity . O-n-scan)
      (current-count . 15-20)
      (noticeable . no)
      (threshold . "~100 entities")
      (optimization . spatial-hash))
    (pathfinding
      (algorithm . a-star)
      (tested . no)
      (expected . good)
      (use-case . smarter-ai))
    (rendering
      (approach . full-rebuild)
      (acceptable-for . turn-based)
      (optimization . dirty-rectangles)
      (use-case . real-time-games))))

(define *recommendations*
  '((quick-wins
      ((priority . 1)
       (task . "Add world-update-entity")
       (effort . low)
       (impact . critical))
      ((priority . 2)
       (task . "Add world-remove-entity")
       (effort . low)
       (impact . high))
      ((priority . 3)
       (task . "Add combat helpers")
       (examples . (calculate-damage entity-attack))
       (effort . medium)
       (impact . high))
      ((priority . 4)
       (task . "Implement inventory functions")
       (effort . medium)
       (impact . medium))
      ((priority . 5)
       (task . "Document mutation semantics")
       (effort . low)
       (impact . medium)))
    (medium-term
      ((task . "Wire turn system to world")
       (effort . high)
       (impact . medium))
      ((task . "Add example AI implementations")
       (effort . medium)
       (impact . medium))
      ((task . "Add entity query helpers")
       (examples . (find-entities-by-type find-nearest-entity))
       (effort . medium)
       (impact . low)))
    (long-term
      ((task . "Save/load serialization")
       (effort . high)
       (impact . high))
      ((task . "FOV system")
       (effort . high)
       (impact . medium))
      ((task . "Status effects")
       (effort . high)
       (impact . medium)))))

(define *overall-assessment*
  '((rating . 8.5/10)
    (strengths
      . "Excellent foundation, comprehensive features, clean API, quick prototyping")
    (weaknesses
      . "Missing glue functions, inconsistent mutation, some features are stubs")
    (production-ready-for
      . (game-jams prototypes simple-roguelikes learning-projects))
    (needs-work-for
      . (complex-rpgs commercial-games real-time-games))
    (verdict
      . "Solid v0.1.0 - with targeted additions could easily be 9.5/10")
    (would-use-again . yes)
    (fun-factor . 10/10)))

(define *code-quality*
  '((naming-conventions . consistent)
    (documentation . comprehensive)
    (global-state . minimal)
    (pure-functions . mostly)
    (function-length . some-very-long)
    (nesting . deep-in-places)
    (error-handling . none)
    (testing . test-suite-exists)))

;;; Utility: Extract specific findings
(define (get-finding category)
  (case category
    [(strengths) *sdk-strengths*]
    [(gaps) *critical-gaps*]
    [(recommendations) *recommendations*]
    [(assessment) *overall-assessment*]
    [(tested) *sdk-components-tested*]
    [(features) *demo-game-features*]
    [else #f]))

;;; Utility: Count specific metrics
(define (count-tested-components)
  (length (filter (lambda (comp)
                   (let ([status (cdr comp)])
                     (and (pair? status)
                          (ormap (lambda (item)
                                  (and (pair? item)
                                       (eq? (cdr item) 'tested)))
                                 status))))
                 *sdk-components-tested*)))

;;; Utility: List critical issues
(define (list-critical-issues)
  (filter (lambda (issue)
           (eq? (cdr (assq 'severity (cdr issue))) 'critical))
          *critical-gaps*))

;;; ============================================================
;;; Summary Display
;;; ============================================================

(define (display-dogfood-summary)
  (display "\n")
  (display "RPG SDK Dogfooding Summary\n")
  (display "==========================\n\n")
  (display "Project: ")
  (display (cdr (assq 'project *dogfood-exercise*)))
  (newline)
  (display "Demo: ")
  (display (cdr (assq 'test-vehicle *dogfood-exercise*)))
  (display " (")
  (display (cdr (assq 'lines-of-code *dogfood-exercise*)))
  (display " lines)\n")
  (display "Rating: ")
  (display (cdr (assq 'rating *overall-assessment*)))
  (newline)
  (display "Verdict: ")
  (display (cdr (assq 'verdict *overall-assessment*)))
  (newline)
  (newline)
  (display "Critical Issues: ")
  (display (length (list-critical-issues)))
  (newline)
  (for-each (lambda (issue)
              (display "  - ")
              (display (car issue))
              (newline))
            (list-critical-issues))
  (newline))

;;; ============================================================
;;; End of dogfood notes
;;; ============================================================

(display "RPG SDK dogfood findings loaded.\n")
(display "Use (display-dogfood-summary) for overview.\n")
(display "Use (get-finding 'category) to query specific findings.\n")
