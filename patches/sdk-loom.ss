;;; SDK Patch: Loom - Game Weaving Framework
;;;
;;; Loom is The Fold's roguelike/RPG game development SDK.
;;; Provides entity-component system, turn-based mechanics,
;;; tile-based worlds, and AI behaviors.

((name . sdk-loom)
 (description . "Roguelike game development SDK with entity-component architecture")
 (version . "0.1.0")
 (author . "The Fold")
 (requires . ())
 
 (provides .
           ;; === Core System ===
           (loom-version               ; Get SDK version string
                         
                         ;; === Entity Management ===
                         make-entity                 ; Create new entity
                         entity?                     ; Entity predicate
                         entity-id                   ; Get entity ID
                         entity-add-component!       ; Add component to entity
                         entity-get-component        ; Get component from entity
                         entity-has-component?       ; Check component presence
                         entity-remove-component!    ; Remove component
                         
                         ;; === World Management ===
                         make-world                  ; Create new game world
                         world-add-entity!           ; Add entity to world
                         world-remove-entity!        ; Remove entity from world
                         world-get-entities          ; Get all entities
                         world-query                 ; Query entities by components
                         
                         ;; === Turn System ===
                         make-turn-manager           ; Create turn manager
                         turn-advance!               ; Advance to next turn
                         turn-current-actor          ; Get current acting entity
                         turn-register!              ; Register entity for turns
                         
                         ;; === Tile System ===
                         make-tilemap                ; Create tilemap
                         tilemap-get                 ; Get tile at position
                         tilemap-set!                ; Set tile at position
                         tilemap-in-bounds?          ; Check position validity
                         
                         ;; === Events ===
                         emit-event!                 ; Emit game event
                         on-event                    ; Register event handler
                         
                         ;; === Game Loop ===
                         make-game                   ; Create game instance
                         game-run!                   ; Start game loop
                         game-step!                  ; Single game step
                         ))
 
 (files .
        ("playpen/loom/core.ss"
         "playpen/loom/entity.ss"
         "playpen/loom/world.ss"
         "playpen/loom/tile.ss"
         "playpen/loom/event.ss"
         "playpen/loom/turn.ss"
         "playpen/loom/action.ss"
         "playpen/loom/loom.ss")))
