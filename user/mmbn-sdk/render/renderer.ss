;;; user/mmbn-sdk/render/renderer.ss — Battle Grid Renderer
;;; @module mmbn/renderer
;;; @requires panel, halfblock
;;; Load via: (load "user/mmbn-sdk/mmbn.ss")

;; Dependencies loaded by mmbn.ss

(doc 'module 'mmbn/renderer)
(doc 'description "Renders battle grid and entities to terminal using half-block
pixel graphics. Handles panel colors, entity sprites, and UI overlays.")
(doc 'layer 'user)
(doc 'purity 'partial)

;;; ============================================================
;;; Render Constants
;;; ============================================================

(doc 'section 'constants)

;; Panel dimensions in pixels
(define *panel-width* 16)
(define *panel-height* 12)

;; Grid pixel dimensions
(define *grid-pixel-width* (* *panel-width* *grid-cols*))
(define *grid-pixel-height* (* *panel-height* *grid-rows*))

;; UI margins
(define *ui-margin-top* 4)
(define *ui-margin-left* 2)

;; Total screen size
(define *screen-width* (+ *grid-pixel-width* (* 2 *ui-margin-left*)))
(define *screen-height* (+ *grid-pixel-height* *ui-margin-top* 4))

;;; ============================================================
;;; Screen Buffer
;;; ============================================================

(doc 'section 'screen-buffer)

(define (make-screen-buffer)
  (doc 'type '(-> PixelBuffer))
  (doc 'description "Create a screen-sized pixel buffer")
  (doc 'export #t)
  (make-pixel-buffer *screen-width* *screen-height*))

(define (clear-screen! buffer)
  (doc 'type '(-> PixelBuffer Void))
  (doc 'description "Clear buffer to black")
  (pixel-buffer-clear! buffer (rgb 0 0 0)))

;;; ============================================================
;;; Panel Rendering
;;; ============================================================

(doc 'section 'panel-rendering)

(define (panel-screen-x col)
  (+ *ui-margin-left* (* col *panel-width*)))

(define (panel-screen-y row)
  (+ *ui-margin-top* (* row *panel-height*)))

(define (render-panel! buffer col row panel)
  (doc 'type '(-> PixelBuffer Int Int Panel Void))
  (doc 'description "Render a single panel to buffer")
  (let* ([x (panel-screen-x col)]
         [y (panel-screen-y row)]
         [color (panel-color panel)]
         ;; Slightly darker border
         [border-color (rgb (max 0 (- (list-ref color 1) 40))
                            (max 0 (- (list-ref color 2) 40))
                            (max 0 (- (list-ref color 3) 40)))])
    ;; Fill panel
    (pixel-buffer-fill-rect! buffer x y *panel-width* *panel-height* color)
    ;; Draw border
    (pixel-buffer-draw-line! buffer x y (+ x *panel-width* -1) y border-color)
    (pixel-buffer-draw-line! buffer x y x (+ y *panel-height* -1) border-color)
    ;; Draw stolen indicator
    (when (panel-stolen? panel)
      (pixel-buffer-draw-line! buffer (+ x 2) (+ y 2)
                               (+ x *panel-width* -3) (+ y 2)
                               (rgb 255 255 0)))))

(define (render-grid! buffer grid)
  (doc 'type '(-> PixelBuffer BattleGrid Void))
  (doc 'description "Render entire battle grid to buffer")
  (doc 'export #t)
  (do ([row 0 (+ row 1)])
      ((>= row (grid-rows grid)))
    (do ([col 0 (+ col 1)])
        ((>= col (grid-cols grid)))
      (let ([panel (grid-ref grid col row)])
        (render-panel! buffer col row panel)))))

;;; ============================================================
;;; Entity Rendering
;;; ============================================================

(doc 'section 'entity-rendering)

(define (entity-screen-pos e)
  (doc 'type '(-> Entity (Pair Int Int)))
  (doc 'description "Get entity's screen position (center of panel)")
  (let ([p (entity-pos e)])
    (cons (+ (panel-screen-x (pos-col p)) (quotient *panel-width* 2))
          (+ (panel-screen-y (pos-row p)) (quotient *panel-height* 2)))))

(define (render-entity! buffer entity)
  (doc 'type '(-> PixelBuffer Entity Void))
  (doc 'description "Render entity sprite at its position")
  (let* ([sprite (entity-sprite entity)]
         [screen-pos (entity-screen-pos entity)]
         [sx (car screen-pos)]
         [sy (cdr screen-pos)]
         ;; Center sprite on position
         [sw (pixel-buffer-width sprite)]
         [sh (pixel-buffer-height sprite)]
         [x (- sx (quotient sw 2))]
         [y (- sy (quotient sh 2))])
    (pixel-buffer-blit! buffer sprite x y)))

(define (render-entities! buffer entities)
  (doc 'type '(-> PixelBuffer (List Entity) Void))
  (doc 'description "Render all entities (sorted by row for depth)")
  (doc 'export #t)
  ;; Sort by row (higher rows rendered later = in front)
  (let ([sorted (list-sort (lambda (a b)
                             (< (pos-row (entity-pos a))
                                (pos-row (entity-pos b))))
                           entities)])
    (for-each (lambda (e) (render-entity! buffer e)) sorted)))

;;; ============================================================
;;; HP Bar Rendering
;;; ============================================================

(doc 'section 'hp-bar)

(define (render-hp-bar! buffer x y current max)
  (doc 'type '(-> PixelBuffer Int Int Int Int Void))
  (doc 'description "Render HP bar at position")
  (let* ([bar-width 30]
         [bar-height 4]
         [fill-width (quotient (* bar-width current) max)]
         [bg-color (rgb 60 60 60)]
         [fg-color (cond [(> current (* max 0.5)) (rgb 0 255 0)]
                         [(> current (* max 0.2)) (rgb 255 255 0)]
                         [else (rgb 255 0 0)])])
    ;; Background
    (pixel-buffer-fill-rect! buffer x y bar-width bar-height bg-color)
    ;; Fill
    (pixel-buffer-fill-rect! buffer x y fill-width bar-height fg-color)))

(define (render-entity-hp! buffer entity)
  (doc 'type '(-> PixelBuffer Entity Void))
  (doc 'description "Render entity's HP bar above it")
  (let ([hp (entity-hp entity)])
    (when hp
      (let* ([screen-pos (entity-screen-pos entity)]
             [x (- (car screen-pos) 15)]
             [y (- (cdr screen-pos) 8)]
             [max-hp (entity-get entity 'max-hp hp)])
        (render-hp-bar! buffer x y hp max-hp)))))

(define (render-entities-hp! buffer entities)
  (doc 'type '(-> PixelBuffer (List Entity) Void))
  (for-each (lambda (e) (render-entity-hp! buffer e)) entities))

;;; ============================================================
;;; UI Overlays
;;; ============================================================

(doc 'section 'ui-overlays)

(define (render-player-hp! buffer player-hp max-hp)
  (doc 'type '(-> PixelBuffer Int Int Void))
  (doc 'description "Render player HP in corner")
  (render-hp-bar! buffer 2 2 player-hp max-hp))

(define (render-custom-gauge! buffer custom max-custom)
  (doc 'type '(-> PixelBuffer Int Int Void))
  (doc 'description "Render Custom gauge (for chip selection)")
  (let* ([bar-width 60]
         [bar-height 3]
         [fill-width (quotient (* bar-width custom) max-custom)]
         [x (- *screen-width* bar-width 2)]
         [y 2])
    (pixel-buffer-fill-rect! buffer x y bar-width bar-height (rgb 40 40 40))
    (pixel-buffer-fill-rect! buffer x y fill-width bar-height (rgb 0 200 255))))

;;; ============================================================
;;; Full Scene Render
;;; ============================================================

(doc 'section 'scene-render)

(define (render-battle-scene! buffer grid entities player-hp max-hp custom max-custom)
  (doc 'type '(-> PixelBuffer BattleGrid (List Entity) Int Int Int Int Void))
  (doc 'description "Render complete battle scene")
  (doc 'export #t)
  ;; Clear and render layers
  (clear-screen! buffer)
  (render-grid! buffer grid)
  (render-entities! buffer entities)
  (render-entities-hp! buffer entities)
  (render-player-hp! buffer player-hp max-hp)
  (render-custom-gauge! buffer custom max-custom))

;;; ============================================================
;;; Display
;;; ============================================================

(doc 'section 'display)

(define (display-battle buffer)
  (doc 'type '(-> PixelBuffer Void))
  (doc 'description "Display battle screen to terminal")
  (doc 'export #t)
  (display (ansi-cursor-home))
  (display (ansi-hide-cursor))
  (display-pixel-buffer buffer)
  (display (ansi-show-cursor)))

(display "mmbn/renderer.ss loaded.\n")
(display "  (make-screen-buffer)           — Create screen buffer\n")
(display "  (render-battle-scene! ...)     — Render complete battle\n")
(display "  (display-battle buffer)        — Show on terminal\n")
