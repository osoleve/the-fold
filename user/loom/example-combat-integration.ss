;;; playpen/loom/example-combat-integration.ss — Combat Integration Example
;;;
;;; This example demonstrates how to use the new combat system, AI system,
;;; and inventory helpers together in a complete game loop.
;;;
;;; Shows:
;;;   - Combat system (calculate-damage, entity-attack)
;;;   - AI system (hunt, guard, wander behaviors)
;;;   - World-level inventory operations
;;;   - Proper functional update patterns
;;;
;;; Run this to see all the new features in action!

;;; Load dependencies
(load "user/loom/loom.ss")
(load "user/loom/combat.ss")
(load "user/loom/ai.ss")

;;; ====
;;; Example 1: Basic Combat
;;; ====

(define (example-basic-combat)
  (display "\n=== Example 1: Basic Combat ===\n\n")
  
  ;; Create a player and a goblin
  (let* ([player (make-player "Hero" #\@ 5 5)]
         [goblin (make-monster "Goblin" #\g 6 5 'hunt)])
        
        (display (format "Player: HP=~a, Attack=~a, Defense=~a\n"
                         (entity-hp player)
                         (stats-attack (entity-stats player))
                         (stats-defense (entity-stats player))))
        
        (display (format "Goblin: HP=~a, Attack=~a, Defense=~a\n\n"
                         (entity-hp goblin)
                         (stats-attack (entity-stats goblin))
                         (stats-defense (entity-stats goblin))))
        
        ;; Player attacks goblin
        (let* ([result (entity-melee-attack player goblin)])
              (display (combat-result-message result))
              (newline)
              
              ;; Get updated goblin
              (let ([new-goblin (combat-result-defender result)])
                   (display (format "Goblin HP after attack: ~a\n\n" (entity-hp new-goblin)))
                   
                   ;; Goblin attacks back (if still alive)
                   (when (entity-alive? new-goblin)
                         (let* ([counter-result (entity-melee-attack new-goblin player)]
                                [new-player (combat-result-defender counter-result)])
                               (display (combat-result-message counter-result))
                               (newline)
                               (display (format "Player HP after counter: ~a\n" (entity-hp new-player)))))))))

;;; ====
;;; Example 2: AI Behavior
;;; ====

(define (example-ai-behavior)
  ;; Create a small world
  (define tm (make-tilemap 20 20))
  (define world #f)
  (define player #f)
  (define goblin-hunt #f)
  (define orc-wander #f)
  (define troll-guard #f)
  (define troll-with-guard #f)
  (define goblin-action #f)
  (define orc-action #f)
  (define troll-action #f)
  
  (display "\n=== Example 2: AI Behavior ===\n\n")
  
  (tilemap-fill! tm tile-floor)
  (set! world (make-world tm))
  
  ;; Add player
  (set! player (make-player "Hero" #\@ 10 10))
  (set! world (world-spawn-player world player))
  
  ;; Add a hunting goblin
  (set! goblin-hunt (make-monster "Hunter" #\g 15 10 'hunt))
  (set! world (world-add-entity world goblin-hunt))
  
  ;; Add a wandering orc
  (set! orc-wander (make-monster "Wanderer" #\o 5 5 'wander))
  (set! world (world-add-entity world orc-wander))
  
  ;; Add a guarding troll
  (set! troll-guard (make-monster "Guard" #\T 10 15 'guard))
  ;; Set guard point in AI state
  (set! troll-with-guard
        (entity-update-component troll-guard 'ai
                                 (lambda (ai) (ai-update-state ai 'guard-point (cons 10 15)))))
  (set! world (world-add-entity world troll-with-guard))
  
  (display "World created with:\n")
  (display "  - Player at (10, 10)\n")
  (display "  - Hunting Goblin at (15, 10)\n")
  (display "  - Wandering Orc at (5, 5)\n")
  (display "  - Guarding Troll at (10, 15)\n\n")
  
  ;; Simulate AI turns
  (display "AI Decisions:\n")
  
  ;; Hunter goblin - should try to hunt player
  (set! goblin-action (ai-decide-action world goblin-hunt))
  (display (format "  Hunter: ~a ~a\n"
                   (ai-action-type goblin-action)
                   (ai-action-data goblin-action)))
  
  ;; Wanderer orc - should move randomly
  (set! orc-action (ai-decide-action world orc-wander))
  (display (format "  Wanderer: ~a ~a\n"
                   (ai-action-type orc-action)
                   (ai-action-data orc-action)))
  
  ;; Guard troll - should wait at guard point
  (set! troll-action (ai-decide-action world troll-with-guard))
  (display (format "  Guard: ~a ~a\n"
                   (ai-action-type troll-action)
                   (ai-action-data troll-action))))

;;; ====
;;; Example 3: Complete Turn-Based Combat Loop
;;; ====

(define (example-combat-loop)
  (define tm (make-tilemap 20 20))
  (define world #f)
  (define player #f)
  (define player-id #f)
  (define goblin #f)
  (define goblin-id #f)
  (define turn 1)
  
  (display "\n=== Example 3: Turn-Based Combat Loop ===\n\n")
  
  ;; Create world
  (tilemap-fill! tm tile-floor)
  (set! world (make-world tm))
  
  ;; Add player
  (set! player (make-player "Hero" #\@ 10 10))
  (set! world (world-spawn-player world player))
  (set! player-id (entity-id player))
  
  ;; Add goblin nearby
  (set! goblin (make-monster "Goblin" #\g 11 10 'hunt))
  (set! world (world-add-entity world goblin))
  (set! goblin-id (entity-id goblin))
  
  (display "Starting combat: Player vs Goblin\n")
  (display (format "  Player: HP=~a\n" (entity-hp player)))
  (display (format "  Goblin: HP=~a\n\n" (entity-hp goblin)))
  
  ;; Combat loop
  (let loop ()
       (display (format "--- Turn ~a ---\n" turn))
       
       ;; Get current entities
       (let* ([current-player (world-get-entity world player-id)]
              [current-goblin (world-get-entity world goblin-id)])
             
             ;; Check if combat is over
             (cond
              [(not (entity-alive? current-player))
               (display "GAME OVER - Player defeated!\n")]
              [(not (entity-alive? current-goblin))
               (display "VICTORY - Goblin defeated!\n")]
              [else
               ;; Player turn - attack goblin
               (let ([player-attack (entity-melee-attack current-player current-goblin)])
                    (display (format "Player: ~a\n" (combat-result-message player-attack)))
                    
                    ;; Update goblin
                    (set! world (world-replace-entity world goblin-id
                                                      (combat-result-defender player-attack))))
               
               ;; Goblin turn (if still alive)
               (let ([updated-goblin (world-get-entity world goblin-id)])
                    (when (entity-alive? updated-goblin)
                          ;; AI decides action
                          (let ([goblin-action (ai-decide-action world updated-goblin)])
                               
                               (case (ai-action-type goblin-action)
                                     ((attack)
                                      ;; Attack player
                                      (let ([goblin-attack (entity-melee-attack updated-goblin current-player)])
                                           (display (format "Goblin: ~a\n" (combat-result-message goblin-attack)))
                                           
                                           ;; Update player
                                           (set! world (world-replace-entity world player-id
                                                                             (combat-result-defender goblin-attack)))))
                                     ((move)
                                      ;; Move toward player
                                      (let ([dir (ai-action-data goblin-action)])
                                           (set! world (world-move-entity world goblin-id dir))
                                           (display (format "Goblin: moves ~a\n" dir))))
                                     (else
                                      (display "Goblin: waits\n"))))))
               
               (newline)
               (set! turn (+ turn 1))
               (loop)]))))

;;; ====
;;; Example 4: Inventory System
;;; ====

(define (example-inventory)
  (define tm (make-tilemap 10 10))
  (define world #f)
  (define player #f)
  (define player-id #f)
  (define potion #f)
  (define potion-id #f)
  
  (display "\n=== Example 4: Inventory System ===\n\n")
  
  ;; Create world
  (tilemap-fill! tm tile-floor)
  (set! world (make-world tm))
  
  ;; Add player
  (set! player (make-player "Hero" #\@ 5 5))
  (set! world (world-spawn-player world player))
  (set! player-id (entity-id player))
  
  ;; Add a health potion on the ground
  (set! potion (make-item "Health Potion" #\! 5 5 'potion))
  (set! world (world-add-entity world potion))
  (set! potion-id (entity-id potion))
  
  (display "Player standing on health potion\n")
  (display (format "  Player inventory: ~a items\n"
                   (inventory-count (entity-inventory player))))
  (display (format "  Items at (5,5): ~a\n\n"
                   (length (world-items-at world 5 5))))
  
  ;; Pick up the potion
  (set! world (world-pickup-item world player-id potion-id))
  
  (display "After picking up potion:\n")
  (let ([updated-player (world-get-entity world player-id)])
       (display (format "  Player inventory: ~a items\n"
                        (inventory-count (entity-inventory updated-player))))
       (display (format "  Items at (5,5): ~a\n"
                        (length (world-items-at world 5 5))))))

;;; ====
;;; Example 5: Complete Game Turn with All Systems
;;; ====

(define (example-complete-turn)
  (define tm (make-tilemap 20 20))
  (define world #f)
  (define player #f)
  (define player-id #f)
  (define goblin #f)
  (define orc #f)
  (define potion #f)
  (define entity-list #f)
  
  (display "\n=== Example 5: Complete Game Turn ===\n\n")
  
  ;; Setup world
  (tilemap-fill! tm tile-floor)
  (set! world (make-world tm))
  
  ;; Add player
  (set! player (make-player "Hero" #\@ 10 10))
  (set! world (world-spawn-player world player))
  (set! player-id (entity-id player))
  
  ;; Add enemies
  (set! goblin (make-monster "Goblin" #\g 12 10 'hunt))
  (set! world (world-add-entity world goblin))
  
  (set! orc (make-monster "Orc" #\o 8 12 'wander))
  (set! world (world-add-entity world orc))
  
  ;; Add item
  (set! potion (make-item "Potion" #\! 10 11 'potion))
  (set! world (world-add-entity world potion))
  
  (display "Turn Processing:\n\n")
  
  ;; Process each entity with AI
  (set! entity-list (world-all-entities world))
  (for-each
   (lambda (entity)
           (when (and (entity-alive? entity) (entity-is-monster? entity))
                 (let ([action (ai-decide-action world entity)])
                      (display (format "~a (~a): ~a ~a\n"
                                       (entity-name entity)
                                       (entity-id entity)
                                       (ai-action-type action)
                                       (ai-action-data action))))))
   entity-list)
  
  (display "\nTurn complete!\n"))

;;; ====
;;; Run All Examples
;;; ====

(define (run-all-examples)
  (display "\n")
  (display "╔════════════════════════════════════════════════════════╗\n")
  (display "║  RPG SDK Combat Integration Examples                  ║\n")
  (display "╚════════════════════════════════════════════════════════╝\n")
  
  (example-basic-combat)
  (example-ai-behavior)
  (example-combat-loop)
  (example-inventory)
  (example-complete-turn)
  
  (display "\n")
  (display "╔════════════════════════════════════════════════════════╗\n")
  (display "║  All examples completed successfully!                  ║\n")
  (display "╚════════════════════════════════════════════════════════╝\n")
  (display "\n"))

;;; Uncomment to run when loaded:
; (run-all-examples)

(display "\nCombat integration examples loaded.\n")
(display "Run (run-all-examples) to see all features in action.\n")
(display "Or run individual examples:\n")
(display "  (example-basic-combat)\n")
(display "  (example-ai-behavior)\n")
(display "  (example-combat-loop)\n")
(display "  (example-inventory)\n")
(display "  (example-complete-turn)\n")
