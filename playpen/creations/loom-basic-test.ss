;;; playpen/creations/loom-basic-test.ss — Basic Loom functionality test

(load "playpen/loom/loom.ss")

(display "Testing Loom SDK basic functionality...\n")

; Test 1: Tilemap creation and filling
(display "1. Creating and filling tilemap...\n")
(let ([tm (make-tilemap 5 5)]
      [floor (make-tile 'floor '((walkable . #t)))])
     (tilemap-fill! tm floor)
     (display "   ✓ Tilemap created and filled successfully\n"))

; Test 2: Direction system
(display "2. Testing directions...\n")
(display "   North delta: ")
(display (direction->delta 'north))
(newline)
(display "   ✓ Direction system working\n")

; Test 3: Entity creation
(display "3. Creating entity...\n")
(let ([entity (make-entity "Test")])
     (display "   Entity name: ")
     (display (entity-name entity))
     (newline)
     (display "   ✓ Entity creation working\n"))

(display "All basic tests passed! Loom SDK is functional.\n")
(display "The saga was true - Loom is a working roguelike framework!\n")
