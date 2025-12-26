;;; playpen/rpg/combat.ss — Combat System for RPG SDK
;;;
;;; Provides damage calculation, attack resolution, and combat utilities.
;;; Fills the critical gap identified in dogfooding: stats exist but no combat logic.
;;;
;;; This is Playpen code: the SDK for building roguelikes and dungeon crawlers.
;;;
;;; Dependencies:
;;;   - playpen/rpg/core.ss
;;;   - playpen/rpg/entity.ss
;;;
;;; Exports:
;;;   Combat resolution, damage calculation, attack helpers

;;; ============================================================
;;; Damage Calculation
;;; ============================================================

;;; calculate-damage : Entity × Entity × [Alist] -> Nat
;;; Calculate damage from attacker to defender.
;;;
;;; Formula: base = attacker-attack - defender-defense
;;;          actual = max(1, base) + random-variance
;;;
;;; Options alist can contain:
;;;   (variance . Nat)     — Random variance range (default 2)
;;;   (min-damage . Nat)   — Minimum damage (default 1)
;;;   (critical-chance . Nat) — Critical hit % (default 10)
;;;   (critical-mult . Nat)   — Critical multiplier (default 2)
;;;
(define calculate-damage
  (case-lambda
    [(attacker defender)
     (calculate-damage attacker defender '())]
    [(attacker defender options)
     (let* ([attacker-stats (entity-stats attacker)]
            [defender-stats (entity-stats defender)])
       (if (and attacker-stats defender-stats)
           (let* ([attack (stats-attack attacker-stats)]
                  [defense (stats-defense defender-stats)]
                  [variance (alist-ref options 'variance 2)]
                  [min-damage (alist-ref options 'min-damage 1)]
                  [crit-chance (alist-ref options 'critical-chance 10)]
                  [crit-mult (alist-ref options 'critical-mult 2)]
                  ;; Base damage
                  [base (max min-damage (- attack defense))]
                  ;; Add variance
                  [with-variance (+ base (random-int (+ variance 1)))]
                  ;; Critical hit check
                  [is-critical? (< (random-int 100) crit-chance)]
                  [final (if is-critical?
                             (* with-variance crit-mult)
                             with-variance)])
             final)
           0))]))

;;; calculate-damage-simple : Nat × Nat -> Nat
;;; Simplified damage calculation: attack - defense, minimum 1.
(define (calculate-damage-simple attack defense)
  (max 1 (- attack defense)))

;;; ============================================================
;;; Attack Resolution
;;; ============================================================

;;; CombatResult : Alist
;;;   (attacker . Entity)    — Updated attacker entity
;;;   (defender . Entity)    — Updated defender entity
;;;   (damage . Nat)         — Damage dealt
;;;   (killed? . Bool)       — Whether defender died
;;;   (critical? . Bool)     — Whether it was a critical hit
;;;   (message . String)     — Combat message for log

;;; entity-attack : Entity × Entity × [Alist] -> CombatResult
;;; Resolve an attack from attacker to defender.
;;; Returns combat result with updated entities and event info.
;;;
;;; This is the main combat function - it handles everything:
;;;   - Damage calculation
;;;   - Applying damage
;;;   - Death checking
;;;   - Message generation
;;;
(define entity-attack
  (case-lambda
    [(attacker defender)
     (entity-attack attacker defender '())]
    [(attacker defender options)
     (let* ([variance (alist-ref options 'variance 2)]
            [min-damage (alist-ref options 'min-damage 1)]
            [crit-chance (alist-ref options 'critical-chance 10)]
            [crit-mult (alist-ref options 'critical-mult 2)]
            ;; Calculate damage
            [damage (calculate-damage attacker defender options)]
            [is-critical? (< (random-int 100) crit-chance)]
            ;; Apply damage
            [new-defender (entity-damage defender damage)]
            [killed? (not (entity-alive? new-defender))]
            ;; Generate message
            [attacker-name (entity-name attacker)]
            [defender-name (entity-name defender)]
            [message (format "~a attacks ~a for ~a damage~a~a"
                            attacker-name
                            defender-name
                            damage
                            (if is-critical? " (CRITICAL!)" "")
                            (if killed? " [KILLED]" ""))])
       `((attacker . ,attacker)
         (defender . ,new-defender)
         (damage . ,damage)
         (killed? . ,killed?)
         (critical? . ,is-critical?)
         (message . ,message)))]))

;;; combat-result-attacker : CombatResult -> Entity
(define (combat-result-attacker result)
  (alist-ref result 'attacker))

;;; combat-result-defender : CombatResult -> Entity
(define (combat-result-defender result)
  (alist-ref result 'defender))

;;; combat-result-damage : CombatResult -> Nat
(define (combat-result-damage result)
  (alist-ref result 'damage 0))

;;; combat-result-killed? : CombatResult -> Bool
(define (combat-result-killed? result)
  (alist-ref result 'killed? #f))

;;; combat-result-critical? : CombatResult -> Bool
(define (combat-result-critical? result)
  (alist-ref result 'critical? #f))

;;; combat-result-message : CombatResult -> String
(define (combat-result-message result)
  (alist-ref result 'message ""))

;;; ============================================================
;;; Combat Utilities
;;; ============================================================

;;; entity-can-attack? : Entity × Entity -> Bool
;;; Check if attacker can attack defender.
;;; Requires: both alive, both have stats, and are adjacent or at range.
(define (entity-can-attack? attacker defender)
  (and (entity-alive? attacker)
       (entity-alive? defender)
       (entity-stats attacker)
       (entity-stats defender)
       ;; For melee, must be adjacent
       (entity-adjacent? attacker defender)))

;;; entity-melee-attack : Entity × Entity -> CombatResult
;;; Perform a melee attack (must be adjacent).
(define (entity-melee-attack attacker defender)
  (if (entity-can-attack? attacker defender)
      (entity-attack attacker defender '((variance . 2) (critical-chance . 10)))
      ;; Can't attack - return no-op result
      `((attacker . ,attacker)
        (defender . ,defender)
        (damage . 0)
        (killed? . #f)
        (critical? . #f)
        (message . "Cannot attack: out of range or invalid target"))))

;;; ============================================================
;;; Status Effects (Future Extension)
;;; ============================================================

;;; These are stubs for future implementation.
;;; Status effects like poison, stun, buffs would go here.

;;; apply-poison : Entity × Nat → Entity
(define (apply-poison entity damage-per-turn)
  ;; TODO: Add status-effect component and poison logic
  entity)

;;; apply-buff : Entity × Symbol × Nat → Entity
(define (apply-buff entity buff-type duration)
  ;; TODO: Add status-effect component and buff logic
  entity)

;;; ============================================================
;;; Random Utilities
;;; ============================================================

;;; random-int : Nat -> Nat
;;; Generate random integer in range [0, n).
(define (random-int n)
  (random n))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Damage Calculation:
;;;   calculate-damage - Full damage calculation with options
;;;   calculate-damage-simple - Simple attack - defense formula
;;;
;;; Attack Resolution:
;;;   entity-attack - Main combat function, returns CombatResult
;;;   entity-melee-attack - Melee attack wrapper
;;;   entity-can-attack? - Validate attack preconditions
;;;
;;; Combat Result Accessors:
;;;   combat-result-attacker, combat-result-defender
;;;   combat-result-damage, combat-result-killed?, combat-result-critical?
;;;   combat-result-message
;;;
;;; Future:
;;;   apply-poison, apply-buff (status effects)
;;;
;;; Usage Example:
;;;   (let* ([player (make-player "Hero" #\@ 5 5)]
;;;          [goblin (make-monster "Goblin" #\g 6 5 'hostile)]
;;;          [result (entity-melee-attack player goblin)]
;;;          [message (combat-result-message result)]
;;;          [new-goblin (combat-result-defender result)])
;;;     (display message)
;;;     (if (combat-result-killed? result)
;;;         (display "\nGoblin is dead!")
;;;         (display "\nGoblin survives!")))
