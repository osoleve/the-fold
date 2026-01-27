;;; user/mmbn-sdk/demo.ss — MMBN SDK Demo
;;;
;;; Run with: scheme --script user/mmbn-sdk/demo.ss

;; Load the entire SDK
(load "user/mmbn-sdk/mmbn.ss")

(display "\n")
(display "╔══════════════════════════════════════════════════════╗\n")
(display "║       MMBN SDK Demo — The Fold Battle Network        ║\n")
(display "╚══════════════════════════════════════════════════════╝\n")
(display "\n")

;;; ============================================================
;;; Setup Battle
;;; ============================================================

;; Create the battle grid
(define grid (make-battle-grid))

;; Modify some panels for variety
(set! grid (grid-set grid 1 1 (make-panel 'grass 'player)))
(set! grid (grid-set grid 4 0 (make-panel 'cracked 'enemy)))
(set! grid (grid-set grid 5 2 (make-panel 'ice 'enemy)))

;; Create entities
(define player-navi (make-navi (pos 1 1) 1000))
(define mettaur-1 (make-mettaur (pos 4 0) 40))
(define mettaur-2 (make-mettaur (pos 5 2) 40))
(define canodumb (make-canodumb (pos 4 1) 60))

(define entities (list player-navi mettaur-1 mettaur-2 canodumb))

;;; ============================================================
;;; Render
;;; ============================================================

(display "Creating battle screen...\n\n")

(define buffer (make-screen-buffer))

;; Render the battle scene
(render-battle-scene! buffer grid entities 1000 1000 75 100)

;; Display
(display-battle buffer)

(display "\n")
(display "════════════════════════════════════════════════════════\n")
(display "\n")

;;; ============================================================
;;; ASCII Debug View
;;; ============================================================

(display "Grid layout (ASCII debug view):\n")
(display-grid grid)
(display "\n")

;;; ============================================================
;;; Entity Info
;;; ============================================================

(display "Entities on field:\n")
(for-each
 (lambda (e)
   (let* ([type (car e)]
          [p (entity-pos e)]
          [hp (entity-hp e)]
          [team (entity-team e)])
     (display (format "  ~a at (~a,~a) — HP: ~a, Team: ~a\n"
                      type (pos-col p) (pos-row p) hp team))))
 entities)

(display "\n")

;;; ============================================================
;;; Movement Demo
;;; ============================================================

(display "Movement test:\n")
(display (format "  Navi can move right? ~a\n"
                 (navi-can-move? player-navi grid dir-right)))
(display (format "  Navi can move left?  ~a\n"
                 (navi-can-move? player-navi grid dir-left)))

;; Try to move navi into enemy territory (should fail)
(display (format "  Navi can enter enemy side? ~a\n"
                 (grid-can-move-to? grid (pos 3 1) 'player)))

(display "\n")

;;; ============================================================
;;; Attack Animation
;;; ============================================================

(display "Attack animation test:\n")
(define attacking-navi (navi-attack player-navi))
(display (format "  Navi state after attack: ~a\n" (navi-state attacking-navi)))

;; Update to show attack sprite
(set! entities (cons attacking-navi (cdr entities)))
(render-battle-scene! buffer grid entities 1000 1000 75 100)
(display "\nNavi attacking:\n")
(display-battle buffer)

(display "\n")
(display "════════════════════════════════════════════════════════\n")
(display "\n")
(display "Demo complete! The MMBN SDK provides:\n")
(display "  • Entity protocol (position, sprites, HP, teams)\n")
(display "  • Battle grid with panel types\n")
(display "  • Half-block pixel rendering\n")
(display "  • Sprite support with transparency\n")
(display "  • Movement validation\n")
(display "  • Scene state machine (not shown in demo)\n")
(display "\n")
(display "Next steps:\n")
(display "  • Chip system (battle cards)\n")
(display "  • Projectile entities\n")
(display "  • Input handling\n")
(display "  • Battle scene with full game loop\n")
(display "  • PET UI scenes\n")
(display "\n")
