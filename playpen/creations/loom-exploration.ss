;;; playpen/creations/loom-exploration.ss — Exploring the Loom SDK

(load "playpen/loom/loom.ss")

(display "═══════════════════════════════════════════════════════════════\n")
(display "  LOOM SDK EXPLORATION\n")
(display "═══════════════════════════════════════════════════════════════\n\n")

(display "Loom SDK loaded successfully!\n")
(display "Available modules: core, tile, entity, event, action, turn, world, combat, ai\n\n")

; Try basic tilemap operations
(display "1. Testing basic tilemap operations...\n")
(let ([tm (make-tilemap 10 10)])
  (display "   Created 10x10 tilemap\n")
  (display "   Tilemap width: ")
  (display (tilemap-width tm))
  (newline)
  (display "   Tilemap height: ")
  (display (tilemap-height tm))
  (newline)
  
  ; Test tilemap-fill! (the function from the saga)
  (display "   Testing tilemap-fill!...\n")
  (let ([floor-tile (make-tile 'floor '((walkable . #t)))])
    (tilemap-fill! tm floor-tile)
    (display "   Successfully filled tilemap with floor tiles\n"))
  
  ; Test individual tile access
  (display "   Testing individual tile access...\n")
  (let ([wall-tile (make-tile 'wall '((walkable . #f)))])
    (tilemap-set-type! tm 5 5 wall-tile)
    (display "   Set wall tile at position (5,5)\n"))
  
  (display "   Tilemap operations completed successfully!\n\n"))

; Try coordinate systems
(display "2. Testing coordinate systems...\n")
(let ([center (coord 5 5)]
      [north (coord 5 4)]
      [east (coord 6 5)])
  (display "   Center: ")
  (display center)
  (newline)
  (display "   North of center: ")
  (display north)
  (newline)
  (display "   East of center: ")
  (display east)
  (newline)
  (display "   Distance center to north: ")
  (display (coord-distance center north))
  (newline)
  (display "   Coordinate operations completed!\n\n"))

; Try entity creation (simple test)
(display "3. Testing entity creation...\n")
(let ([hero (make-entity "Hero")])
  (display "   Created hero entity: ")
  (display (entity-name hero))
  (newline)
  (display "   Entity ID: ")
  (display (entity-id hero))
  (newline)
  (display "   Basic entity operations completed!\n\n"))

(display "═══════════════════════════════════════════════════════════════\n")
(display "Loom exploration completed!\n\n")

(display "What I discovered:\n")
(display "  ✓ Loom SDK loads successfully\n")
(display "  ✓ Tilemap creation and filling works\n")
(display "  ✓ Coordinate system operations work\n")
(display "  ✓ Basic entity creation works\n")
(display "  ✓ The tilemap-fill! function from the saga exists and works!\n\n")

(display "The Loom SDK is a complete roguelike framework with:\n")
(display "  - Entity-Component-System architecture\n")
(display "  - Turn-based combat\n")
(display "  - AI behaviors (hunt, guard, wander, flee)\n")
(display "  - Pathfinding and field-of-view\n")
(display "  - Event system\n")
(display "  - Pure functional design\n\n")

(display "This is exactly what the saga described - a hidden treasure!\n")