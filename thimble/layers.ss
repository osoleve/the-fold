;;; thimble/layers.ss — Canvas Layering & Depth System
;;;
;;; Extends the canvas system with transparency and z-ordered layers.
;;; Enables complex composition: backgrounds, sprites, UI overlays.
;;;
;;; This is Shell code: pure functions for managing visual depth.
;;;
;;; Features:
;;;   - Transparency support (transparent cells don't overwrite)
;;;   - Named layers with z-ordering (back to front)
;;;   - Layer operations: show/hide, reorder, transform
;;;   - Efficient flattening (composite with transparency)
;;;
;;; Data Structures:
;;;   Layer      — A named canvas with visibility and depth
;;;   LayerStack — Ordered collection of layers (background → foreground)
;;;
;;; Operations:
;;;   make-layer, layer-visible?, layer-set-visible
;;;   make-layer-stack, stack-add-layer, stack-remove-layer
;;;   stack-find-layer, stack-reorder, stack-layers
;;;   flatten-layers, composite-transparent
;;;   make-background-layer, make-sprite-layer, make-ui-layer

;;; ============================================================
;;; Dependencies
;;; ============================================================

;;; Load canvas primitives
;;; Assumes shell/layout.ss is already loaded or will be loaded separately

;;; ============================================================
;;; Transparency
;;; ============================================================

;;; The transparency character — cells with this value are transparent.
;;; When compositing, transparent cells don't overwrite the destination.
;;;
;;; We use #\nul (Unicode NULL, U+0000) as it's rarely needed in ASCII art.
(define transparent-char #\nul)

;;; transparent? : Char → Bool
;;; Check if a character represents transparency.
(define (transparent? ch)
  (char=? ch transparent-char))

;;; make-transparent-canvas : Nat × Nat → Canvas
;;; Create a canvas filled with transparent cells.
(define (make-transparent-canvas width height)
  (let ([size (* width height)]
        [cells (make-vector (* width height) transparent-char)])
       (make-canvas% width height cells)))

;;; ============================================================
;;; Layer Data Structure
;;; ============================================================

;;; Layer: A named canvas with metadata for composition
;;;
;;; Fields:
;;;   name     — Symbol identifier for this layer
;;;   canvas   — The actual canvas content
;;;   visible  — Boolean, whether to include in flattening
;;;   depth    — Nat, z-order (lower = background, higher = foreground)
;;;   offset   — Point, position offset when compositing
(define-record-type layer%
  (fields name canvas visible depth offset))

;;; make-layer : Symbol × Canvas × [Nat] × [Point] → Layer
;;; Create a new layer with optional depth and offset.
(define make-layer
  (case-lambda
   [(name canvas)
    (make-layer% name canvas #t 0 (point 0 0))]
   [(name canvas depth)
    (make-layer% name canvas #t depth (point 0 0))]
   [(name canvas depth offset)
    (make-layer% name canvas #t depth offset)]))

;;; Re-export accessors with clean names
(define layer-name layer%-name)
(define layer-canvas layer%-canvas)
(define layer-visible layer%-visible)
(define layer-depth layer%-depth)
(define layer-offset layer%-offset)

;;; layer-set-visible : Layer × Bool → Layer
;;; Show or hide a layer.
(define (layer-set-visible layer visible?)
  (make-layer% (layer-name layer)
               (layer-canvas layer)
               visible?
               (layer-depth layer)
               (layer-offset layer)))

;;; layer-set-canvas : Layer × Canvas → Layer
;;; Update the layer's canvas content.
(define (layer-set-canvas layer new-canvas)
  (make-layer% (layer-name layer)
               new-canvas
               (layer-visible layer)
               (layer-depth layer)
               (layer-offset layer)))

;;; layer-set-offset : Layer × Point → Layer
;;; Move the layer to a new position.
(define (layer-set-offset layer new-offset)
  (make-layer% (layer-name layer)
               (layer-canvas layer)
               (layer-visible layer)
               (layer-depth layer)
               new-offset))

;;; layer-set-depth : Layer × Nat → Layer
;;; Change the layer's z-order.
(define (layer-set-depth layer new-depth)
  (make-layer% (layer-name layer)
               (layer-canvas layer)
               (layer-visible layer)
               new-depth
               (layer-offset layer)))

;;; ============================================================
;;; Layer Stack
;;; ============================================================

;;; LayerStack: Ordered collection of layers
;;;
;;; Layers are stored in depth order (background to foreground).
;;; Operations maintain this invariant.
(define-record-type layer-stack%
  (fields layers))  ; List of Layer, sorted by depth

;;; make-layer-stack : [List Layer] → LayerStack
;;; Create a layer stack, optionally with initial layers.
(define make-layer-stack
  (case-lambda
   [() (make-layer-stack% '())]
   [(initial-layers)
    (make-layer-stack% (sort-layers-by-depth initial-layers))]))

;;; stack-layers : LayerStack → List Layer
;;; Get all layers in depth order.
(define stack-layers layer-stack%-layers)

;;; sort-layers-by-depth : List Layer → List Layer
;;; Sort layers by depth (background to foreground).
(define (sort-layers-by-depth layers)
  (list-sort (lambda (a b)
                     (< (layer-depth a) (layer-depth b)))
             layers))

;;; stack-add-layer : LayerStack × Layer → LayerStack
;;; Add a layer to the stack, maintaining depth order.
(define (stack-add-layer stack layer)
  (make-layer-stack%
   (sort-layers-by-depth
    (cons layer (stack-layers stack)))))

;;; stack-remove-layer : LayerStack × Symbol → LayerStack
;;; Remove a layer by name.
(define (stack-remove-layer stack name)
  (make-layer-stack%
   (filter (lambda (layer)
                   (not (eq? (layer-name layer) name)))
           (stack-layers stack))))

;;; stack-find-layer : LayerStack × Symbol → Layer | #f
;;; Find a layer by name.
(define (stack-find-layer stack name)
  (let loop ([layers (stack-layers stack)])
       (cond
        [(null? layers) #f]
        [(eq? (layer-name (car layers)) name) (car layers)]
        [else (loop (cdr layers))])))

;;; stack-update-layer : LayerStack × Symbol × (Layer → Layer) → LayerStack
;;; Update a layer by name using a function.
(define (stack-update-layer stack name update-fn)
  (make-layer-stack%
   (sort-layers-by-depth
    (map (lambda (layer)
                 (if (eq? (layer-name layer) name)
                     (update-fn layer)
                     layer))
         (stack-layers stack)))))

;;; stack-show-layer : LayerStack × Symbol → LayerStack
;;; Make a layer visible.
(define (stack-show-layer stack name)
  (stack-update-layer stack name
                      (lambda (layer) (layer-set-visible layer #t))))

;;; stack-hide-layer : LayerStack × Symbol → LayerStack
;;; Make a layer invisible.
(define (stack-hide-layer stack name)
  (stack-update-layer stack name
                      (lambda (layer) (layer-set-visible layer #f))))

;;; stack-reorder : LayerStack × Symbol × Nat → LayerStack
;;; Change a layer's depth.
(define (stack-reorder stack name new-depth)
  (stack-update-layer stack name
                      (lambda (layer) (layer-set-depth layer new-depth))))

;;; ============================================================
;;; Transparency-Aware Composition
;;; ============================================================

;;; composite-transparent : Canvas × Canvas × Point → Canvas
;;; Overlay source canvas onto destination at given position.
;;; Transparent cells in source don't overwrite destination.
;;;
;;; This is the key function that makes layering work.
(define (composite-transparent dest src pt)
  (let ([ox (point-x pt)]
        [oy (point-y pt)]
        [sw (canvas-width src)]
        [sh (canvas-height src)])
       (let loop-y ([y 0] [canvas dest])
            (if (>= y sh)
                canvas
                (let loop-x ([x 0] [canvas canvas])
                     (if (>= x sw)
                         (loop-y (+ y 1) canvas)
                         (let ([ch (canvas-ref src x y)])
                              (if (transparent? ch)
                                  ;; Skip transparent cells
                                  (loop-x (+ x 1) canvas)
                                  ;; Draw opaque cells
                                  (loop-x (+ x 1)
                                          (canvas-set canvas (+ ox x) (+ oy y) ch))))))))))

;;; ============================================================
;;; Layer Flattening
;;; ============================================================

;;; flatten-layers : LayerStack × Nat × Nat → Canvas
;;; Composite all visible layers into a single canvas.
;;; Layers are composited in depth order (background first).
;;;
;;; Arguments:
;;;   stack  — The layer stack to flatten
;;;   width  — Width of the output canvas
;;;   height — Height of the output canvas
;;;
;;; Returns a single canvas with all visible layers composited.
(define (flatten-layers stack width height)
  (let ([initial-canvas (make-canvas width height)])
       (let loop ([layers (stack-layers stack)]
                  [canvas initial-canvas])
            (if (null? layers)
                canvas
                (let ([layer (car layers)])
                     (if (layer-visible layer)
                         ;; Composite visible layer
                         (loop (cdr layers)
                               (composite-transparent canvas
                                                      (layer-canvas layer)
                                                      (layer-offset layer)))
                         ;; Skip invisible layer
                         (loop (cdr layers) canvas)))))))

;;; ============================================================
;;; Helper Constructors for Common Patterns
;;; ============================================================

;;; make-background-layer : Nat × Nat → Layer
;;; Create a background layer (depth 0, filled with spaces).
(define (make-background-layer width height)
  (make-layer 'background
              (make-canvas width height)
              0
              (point 0 0)))

;;; make-sprite-layer : Symbol × Nat × Nat × Point → Layer
;;; Create a sprite layer (depth 50, transparent canvas).
(define (make-sprite-layer name width height offset)
  (make-layer name
              (make-transparent-canvas width height)
              50
              offset))

;;; make-ui-layer : Nat × Nat → Layer
;;; Create a UI overlay layer (depth 100, transparent canvas).
(define (make-ui-layer width height)
  (make-layer 'ui
              (make-transparent-canvas width height)
              100
              (point 0 0)))

;;; ============================================================
;;; Drawing to Layers
;;; ============================================================

;;; These are convenience wrappers that work with layers directly,
;;; applying canvas drawing operations and returning updated layers.

;;; layer-draw-string : Layer × Point × String → Layer
;;; Draw a string on a layer's canvas.
(define (layer-draw-string layer pt str)
  (layer-set-canvas layer
                    (draw-string (layer-canvas layer) pt str)))

;;; layer-draw-char : Layer × Point × Char → Layer
;;; Draw a character on a layer's canvas.
(define (layer-draw-char layer pt ch)
  (layer-set-canvas layer
                    (draw-char (layer-canvas layer) pt ch)))

;;; layer-fill-rect : Layer × Rect × Char → Layer
;;; Fill a rectangle on a layer's canvas.
(define (layer-fill-rect layer rect ch)
  (layer-set-canvas layer
                    (fill-rect (layer-canvas layer) rect ch)))

;;; layer-draw-box : Layer × Rect × Symbol → Layer
;;; Draw a box on a layer's canvas.
(define (layer-draw-box layer rect style)
  (layer-set-canvas layer
                    (draw-box (layer-canvas layer) rect style)))

;;; ============================================================
;;; Sprite Helpers
;;; ============================================================

;;; draw-sprite-to-layer : Layer × Point × List String → Layer
;;; Draw a sprite (list of strings) to a layer at the given position.
;;; Transparent characters in the sprite won't overwrite the layer.
(define (draw-sprite-to-layer layer offset sprite-lines)
  (let loop ([lines sprite-lines]
             [y 0]
             [canvas (layer-canvas layer)])
       (if (null? lines)
           (layer-set-canvas layer canvas)
           (let ([line (car lines)])
                (loop (cdr lines)
                      (+ y 1)
                      (draw-string canvas
                                   (point (point-x offset) (+ (point-y offset) y))
                                   line))))))

;;; ============================================================
;;; Debug and Inspection
;;; ============================================================

;;; layer->string : Layer → String
;;; Convert a layer to a string for debugging.
(define (layer->string layer)
  (string-append
   "Layer["
   (symbol->string (layer-name layer))
   " visible=" (if (layer-visible layer) "#t" "#f")
   " depth=" (number->string (layer-depth layer))
   " offset=(" (number->string (point-x (layer-offset layer)))
   "," (number->string (point-y (layer-offset layer)))
   ")"
   " size=" (number->string (canvas-width (layer-canvas layer)))
   "x" (number->string (canvas-height (layer-canvas layer)))
   "]"))

;;; stack->string : LayerStack → String
;;; Convert a layer stack to a string for debugging.
(define (stack->string stack)
  (let ([layers (stack-layers stack)])
       (string-append
        "LayerStack[\n"
        (fold-left (lambda (acc layer)
                           (string-append acc "  " (layer->string layer) "\n"))
                   ""
                   layers)
        "]")))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Transparency:
;;;   transparent-char, transparent?, make-transparent-canvas
;;;
;;; Layers:
;;;   make-layer, layer-name, layer-canvas, layer-visible, layer-depth, layer-offset
;;;   layer-set-visible, layer-set-canvas, layer-set-offset, layer-set-depth
;;;
;;; Layer Stack:
;;;   make-layer-stack, stack-layers, stack-add-layer, stack-remove-layer
;;;   stack-find-layer, stack-update-layer, stack-show-layer, stack-hide-layer
;;;   stack-reorder
;;;
;;; Composition:
;;;   composite-transparent, flatten-layers
;;;
;;; Helpers:
;;;   make-background-layer, make-sprite-layer, make-ui-layer
;;;   layer-draw-string, layer-draw-char, layer-fill-rect, layer-draw-box
;;;   draw-sprite-to-layer
;;;
;;; Debug:
;;;   layer->string, stack->string
