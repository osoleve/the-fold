;;; user/mmbn-sdk/core/panel.ss — Battle Panel System for MMBN-style games
;;; @module mmbn/panel
;;; @requires entity
;;; Load via: (load "user/mmbn-sdk/mmbn.ss")

;; Dependencies loaded by mmbn.ss

(doc 'module 'mmbn/panel)
(doc 'description "Battle grid panel system. The MMBN battle field is a 6x3 grid
of panels, with 3 columns belonging to each side. Panels have terrain types
that affect movement and combat, and can change ownership during battle.")
(doc 'layer 'user)
(doc 'purity 'partial)

;;; ============================================================
;;; Panel Types
;;; ============================================================

(doc 'section 'panel-types)

(doc 'note "Panel types from Battle Network:
  normal  — Default panel, no effect
  cracked — Cracks on step, becomes broken next step
  broken  — Impassable hole, regenerates over time
  ice     — Sliding movement
  poison  — Deals damage each turn
  holy    — Halves damage taken
  grass   — Fire chips deal 2x damage, destroyed by fire
  metal   — Prevents cracking
  sand    — Slows movement
  lava    — Deals fire damage, ignored by fire types")

(define panel-types
  '(normal cracked broken ice poison holy grass metal sand lava))

(define (panel-type? sym)
  (doc 'type '(-> Symbol Bool))
  (memq sym panel-types))

;;; ============================================================
;;; Panel Structure
;;; ============================================================

(doc 'section 'panel-structure)

(define-record-type panel%
  (fields type owner stolen?))

(define (make-panel type owner)
  (doc 'type '(-> Symbol Symbol Panel))
  (doc 'description "Create a panel with type and owner (player/enemy)")
  (doc 'export #t)
  (make-panel% type owner #f))

(define (panel-type p) (panel%-type p))
(define (panel-owner p) (panel%-owner p))
(define (panel-stolen? p) (panel%-stolen? p))

(define (panel-with-type p new-type)
  (doc 'type '(-> Panel Symbol Panel))
  (make-panel% new-type (panel-owner p) (panel-stolen? p)))

(define (panel-with-owner p new-owner stolen?)
  (doc 'type '(-> Panel Symbol Bool Panel))
  (make-panel% (panel-type p) new-owner stolen?))

;;; ============================================================
;;; Panel Effects
;;; ============================================================

(doc 'section 'panel-effects)

(define (panel-walkable? panel)
  (doc 'type '(-> Panel Bool))
  (doc 'description "Can entities stand on this panel?")
  (not (eq? (panel-type panel) 'broken)))

(define (panel-damage panel)
  (doc 'type '(-> Panel Int))
  (doc 'description "Damage dealt per turn standing on panel")
  (case (panel-type panel)
    [(poison) 10]
    [(lava) 50]
    [else 0]))

(define (panel-defense-mult panel)
  (doc 'type '(-> Panel Number))
  (doc 'description "Damage multiplier when standing on panel")
  (case (panel-type panel)
    [(holy) 0.5]
    [else 1.0]))

(define (panel-on-step panel)
  (doc 'type '(-> Panel Panel))
  (doc 'description "Transform panel when stepped on")
  (case (panel-type panel)
    [(cracked) (panel-with-type panel 'broken)]
    [(ice) panel]  ; Ice handled by movement system
    [else panel]))

;;; ============================================================
;;; Battle Grid
;;; ============================================================

(doc 'section 'battle-grid)

(define *grid-cols* 6)
(define *grid-rows* 3)
(define *player-cols* 3)  ; Columns 0-2 are player side

(define-record-type battle-grid%
  (fields panels cols rows))

(define (make-battle-grid)
  (doc 'type '(-> BattleGrid))
  (doc 'description "Create standard 6x3 battle grid")
  (doc 'export #t)
  (let ([panels (make-vector (* *grid-cols* *grid-rows*))])
    ;; Initialize panels
    (do ([row 0 (+ row 1)])
        ((>= row *grid-rows*))
      (do ([col 0 (+ col 1)])
          ((>= col *grid-cols*))
        (let* ([idx (+ col (* row *grid-cols*))]
               [owner (if (< col *player-cols*) 'player 'enemy)])
          (vector-set! panels idx (make-panel 'normal owner)))))
    (make-battle-grid% panels *grid-cols* *grid-rows*)))

(define (grid-cols g) (battle-grid%-cols g))
(define (grid-rows g) (battle-grid%-rows g))

(define (grid-idx grid col row)
  (+ col (* row (grid-cols grid))))

(define (grid-ref grid col row)
  (doc 'type '(-> BattleGrid Int Int (Maybe Panel)))
  (doc 'description "Get panel at position")
  (doc 'export #t)
  (if (and (>= col 0) (< col (grid-cols grid))
           (>= row 0) (< row (grid-rows grid)))
      (vector-ref (battle-grid%-panels grid) (grid-idx grid col row))
      #f))

(define (grid-ref-pos grid pos)
  (doc 'type '(-> BattleGrid Pos (Maybe Panel)))
  (grid-ref grid (pos-col pos) (pos-row pos)))

(define (grid-set grid col row panel)
  (doc 'type '(-> BattleGrid Int Int Panel BattleGrid))
  (doc 'description "Return grid with panel at position updated")
  (doc 'export #t)
  (if (and (>= col 0) (< col (grid-cols grid))
           (>= row 0) (< row (grid-rows grid)))
      (let ([new-panels (vector-copy (battle-grid%-panels grid))])
        (vector-set! new-panels (grid-idx grid col row) panel)
        (make-battle-grid% new-panels (grid-cols grid) (grid-rows grid)))
      grid))

(define (grid-set-pos grid pos panel)
  (doc 'type '(-> BattleGrid Pos Panel BattleGrid))
  (grid-set grid (pos-col pos) (pos-row pos) panel))

;;; ============================================================
;;; Grid Queries
;;; ============================================================

(doc 'section 'grid-queries)

(define (grid-owner-at grid pos)
  (doc 'type '(-> BattleGrid Pos (Maybe Symbol)))
  (let ([panel (grid-ref-pos grid pos)])
    (and panel (panel-owner panel))))

(define (grid-can-move-to? grid pos team)
  (doc 'type '(-> BattleGrid Pos Symbol Bool))
  (doc 'description "Check if team can move to position")
  (doc 'export #t)
  (let ([panel (grid-ref-pos grid pos)])
    (and panel
         (panel-walkable? panel)
         (eq? (panel-owner panel) team))))

(define (grid-positions-for-team grid team)
  (doc 'type '(-> BattleGrid Symbol (List Pos)))
  (doc 'description "Get all positions owned by a team")
  (let ([positions '()])
    (do ([row 0 (+ row 1)])
        ((>= row (grid-rows grid)) (reverse positions))
      (do ([col 0 (+ col 1)])
          ((>= col (grid-cols grid)))
        (let ([panel (grid-ref grid col row)])
          (when (eq? (panel-owner panel) team)
            (set! positions (cons (pos col row) positions))))))))

;;; ============================================================
;;; Panel Stealing (AreaGrab mechanic)
;;; ============================================================

(doc 'section 'area-grab)

(define (grid-steal-column grid col new-owner)
  (doc 'type '(-> BattleGrid Int Symbol BattleGrid))
  (doc 'description "Steal an entire column for a team")
  (doc 'export #t)
  ;; Single vector-copy, update all rows in place
  (let ([new-panels (vector-copy (battle-grid%-panels grid))])
    (do ([row 0 (+ row 1)])
        ((>= row (grid-rows grid))
         (make-battle-grid% new-panels (grid-cols grid) (grid-rows grid)))
      (let* ([idx (grid-idx grid col row)]
             [panel (vector-ref new-panels idx)])
        (when (and panel (not (eq? (panel-owner panel) new-owner)))
          (vector-set! new-panels idx (panel-with-owner panel new-owner #t)))))))

;;; ============================================================
;;; Grid Rendering Helpers
;;; ============================================================

(doc 'section 'rendering)

(define (panel-color panel)
  (doc 'type '(-> Panel Color))
  (doc 'description "Get render color for panel type")
  (case (panel-type panel)
    [(normal)  (if (eq? (panel-owner panel) 'player)
                   (rgb 100 100 200)    ; Blue for player
                   (rgb 200 100 100))]  ; Red for enemy
    [(cracked) (rgb 150 150 150)]
    [(broken)  (rgb 50 50 50)]
    [(ice)     (rgb 200 220 255)]
    [(poison)  (rgb 180 100 200)]
    [(holy)    (rgb 255 255 200)]
    [(grass)   (rgb 100 200 100)]
    [(metal)   (rgb 180 180 180)]
    [(sand)    (rgb 220 200 150)]
    [(lava)    (rgb 255 100 50)]
    [else      (rgb 128 128 128)]))

(define (panel-char panel)
  (doc 'type '(-> Panel Char))
  (doc 'description "Get ASCII char for panel (minimap/debug)")
  (case (panel-type panel)
    [(normal)  (if (eq? (panel-owner panel) 'player) #\= #\-)]
    [(cracked) #\*]
    [(broken)  #\space]
    [(ice)     #\~]
    [(poison)  #\!]
    [(holy)    #\+]
    [(grass)   #\"]
    [(metal)   #\#]
    [(sand)    #\.]
    [(lava)    #\@]
    [else      #\?]))

;;; ============================================================
;;; Grid Display (Debug/ASCII)
;;; ============================================================

(doc 'section 'debug-display)

(define (display-grid grid)
  (doc 'type '(-> BattleGrid Void))
  (doc 'description "Display grid as ASCII (debug)")
  (doc 'export #t)
  (display "  ")
  (do ([col 0 (+ col 1)])
      ((>= col (grid-cols grid)) (newline))
    (display col))
  (do ([row 0 (+ row 1)])
      ((>= row (grid-rows grid)))
    (display row)
    (display " ")
    (do ([col 0 (+ col 1)])
        ((>= col (grid-cols grid)) (newline))
      (let ([panel (grid-ref grid col row)])
        (display (panel-char panel))))))

(display "mmbn/panel.ss loaded.\n")
(display "  (make-battle-grid)            — Create 6x3 battle grid\n")
(display "  (grid-ref grid col row)       — Get panel at position\n")
(display "  (grid-can-move-to? g p team)  — Check movement validity\n")
(display "  (display-grid grid)           — ASCII debug view\n")
