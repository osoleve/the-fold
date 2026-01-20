(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'layers)
(doc 'description "Canvas Layering & Depth System — Extends the canvas system with transparency and z-ordered layers. Enables complex composition: backgrounds, sprites, UI overlays.")
(doc 'layer 'boundary)
(doc 'purity 'total)
(doc 'note "Features: Transparency support, named layers with z-ordering, layer operations (show/hide, reorder, transform), efficient flattening with transparency.")
(doc 'note "Data Structures: Layer (named canvas with visibility and depth), LayerStack (ordered collection of layers, background → foreground)")
(doc 'note "Operations: make-layer, layer-visible?, layer-set-visible, make-layer-stack, stack-add-layer, stack-remove-layer, stack-find-layer, stack-reorder, stack-layers, flatten-layers, composite-transparent, make-background-layer, make-sprite-layer, make-ui-layer")

(doc 'section 'dependencies)
(doc 'note "Load canvas primitives. Assumes boundary/ui/layout.ss is already loaded or will be loaded separately.")

(doc 'section 'transparency)

(doc transparent-char 'type Char)
(doc transparent-char 'description "The transparency character — cells with this value are transparent. When compositing, transparent cells don't overwrite the destination. We use #\\nul (Unicode NULL, U+0000) as it's rarely needed in ASCII art.")
(define transparent-char #\nul)

(doc transparent? 'type (-> Char Bool))
(doc transparent? 'description "Check if a character represents transparency.")
(define (transparent? ch)
  (char=? ch transparent-char))

(doc make-transparent-canvas 'type (-> Nat Nat Canvas))
(doc make-transparent-canvas 'description "Create a canvas filled with transparent cells.")
(define (make-transparent-canvas width height)
  (let ([size (* width height)]
        [cells (make-vector (* width height) transparent-char)])
       (make-canvas% width height cells)))

(doc 'section 'layer-data-structure)

(doc 'note "Layer: A named canvas with metadata for composition. Fields: name (Symbol identifier), canvas (actual content), visible (Boolean, whether to include in flattening), depth (Nat, z-order where lower = background, higher = foreground), offset (Point, position offset when compositing).")

(define-record-type layer%
  (fields name canvas visible depth offset))

(doc make-layer 'type (case-lambda
                        [(Symbol Canvas) Layer]
                        [(Symbol Canvas Nat) Layer]
                        [(Symbol Canvas Nat Point) Layer]))
(doc make-layer 'description "Create a new layer with optional depth and offset.")
(define make-layer
  (case-lambda
   [(name canvas)
    (make-layer% name canvas #t 0 (point 0 0))]
   [(name canvas depth)
    (make-layer% name canvas #t depth (point 0 0))]
   [(name canvas depth offset)
    (make-layer% name canvas #t depth offset)]))

(doc 'note "Re-export accessors with clean names")
(define layer-name layer%-name)
(define layer-canvas layer%-canvas)
(define layer-visible layer%-visible)
(define layer-depth layer%-depth)
(define layer-offset layer%-offset)

(doc layer-set-visible 'type (-> Layer Bool Layer))
(doc layer-set-visible 'description "Show or hide a layer.")
(define (layer-set-visible layer visible?)
  (make-layer% (layer-name layer)
               (layer-canvas layer)
               visible?
               (layer-depth layer)
               (layer-offset layer)))

(doc layer-set-canvas 'type (-> Layer Canvas Layer))
(doc layer-set-canvas 'description "Update the layer's canvas content.")
(define (layer-set-canvas layer new-canvas)
  (make-layer% (layer-name layer)
               new-canvas
               (layer-visible layer)
               (layer-depth layer)
               (layer-offset layer)))

(doc layer-set-offset 'type (-> Layer Point Layer))
(doc layer-set-offset 'description "Move the layer to a new position.")
(define (layer-set-offset layer new-offset)
  (make-layer% (layer-name layer)
               (layer-canvas layer)
               (layer-visible layer)
               (layer-depth layer)
               new-offset))

(doc layer-set-depth 'type (-> Layer Nat Layer))
(doc layer-set-depth 'description "Change the layer's z-order.")
(define (layer-set-depth layer new-depth)
  (make-layer% (layer-name layer)
               (layer-canvas layer)
               (layer-visible layer)
               new-depth
               (layer-offset layer)))

(doc 'section 'layer-stack)

(doc 'note "LayerStack: Ordered collection of layers. Layers are stored in depth order (background to foreground). Operations maintain this invariant.")

(define-record-type layer-stack%
  (fields layers))  ; List of Layer, sorted by depth

(doc make-layer-stack 'type (case-lambda
                              [() LayerStack]
                              [((List Layer)) LayerStack]))
(doc make-layer-stack 'description "Create a layer stack, optionally with initial layers.")
(define make-layer-stack
  (case-lambda
   [() (make-layer-stack% '())]
   [(initial-layers)
    (make-layer-stack% (sort-layers-by-depth initial-layers))]))

(doc stack-layers 'type (-> LayerStack (List Layer)))
(doc stack-layers 'description "Get all layers in depth order.")
(define stack-layers layer-stack%-layers)

(doc sort-layers-by-depth 'type (-> (List Layer) (List Layer)))
(doc sort-layers-by-depth 'description "Sort layers by depth (background to foreground).")
(define (sort-layers-by-depth layers)
  (list-sort (lambda (a b)
                     (< (layer-depth a) (layer-depth b)))
             layers))

(doc stack-add-layer 'type (-> LayerStack Layer LayerStack))
(doc stack-add-layer 'description "Add a layer to the stack, maintaining depth order.")
(define (stack-add-layer stack layer)
  (make-layer-stack%
   (sort-layers-by-depth
    (cons layer (stack-layers stack)))))

(doc stack-remove-layer 'type (-> LayerStack Symbol LayerStack))
(doc stack-remove-layer 'description "Remove a layer by name.")
(define (stack-remove-layer stack name)
  (make-layer-stack%
   (filter (lambda (layer)
                   (not (eq? (layer-name layer) name)))
           (stack-layers stack))))

(doc stack-find-layer 'type (-> LayerStack Symbol (Maybe Layer)))
(doc stack-find-layer 'description "Find a layer by name.")
(define (stack-find-layer stack name)
  (let loop ([layers (stack-layers stack)])
       (cond
        [(null? layers) #f]
        [(eq? (layer-name (car layers)) name) (car layers)]
        [else (loop (cdr layers))])))

(doc stack-update-layer 'type (-> LayerStack Symbol (-> Layer Layer) LayerStack))
(doc stack-update-layer 'description "Update a layer by name using a function.")
(define (stack-update-layer stack name update-fn)
  (make-layer-stack%
   (sort-layers-by-depth
    (map (lambda (layer)
                 (if (eq? (layer-name layer) name)
                     (update-fn layer)
                     layer))
         (stack-layers stack)))))

(doc stack-show-layer 'type (-> LayerStack Symbol LayerStack))
(doc stack-show-layer 'description "Make a layer visible.")
(define (stack-show-layer stack name)
  (stack-update-layer stack name
                      (lambda (layer) (layer-set-visible layer #t))))

(doc stack-hide-layer 'type (-> LayerStack Symbol LayerStack))
(doc stack-hide-layer 'description "Make a layer invisible.")
(define (stack-hide-layer stack name)
  (stack-update-layer stack name
                      (lambda (layer) (layer-set-visible layer #f))))

(doc stack-reorder 'type (-> LayerStack Symbol Nat LayerStack))
(doc stack-reorder 'description "Change a layer's depth.")
(define (stack-reorder stack name new-depth)
  (stack-update-layer stack name
                      (lambda (layer) (layer-set-depth layer new-depth))))

(doc 'section 'transparency-aware-composition)

(doc composite-transparent 'type (-> Canvas Canvas Point Canvas))
(doc composite-transparent 'description "Overlay source canvas onto destination at given position. Transparent cells in source don't overwrite destination. This is the key function that makes layering work. Uses mutable canvas-set! for O(N) performance.")
(define (composite-transparent dest src pt)
  (let ([ox (point-x pt)]
        [oy (point-y pt)]
        [sw (canvas-width src)]
        [sh (canvas-height src)])
       (let loop-y ([y 0])
            (when (< y sh)
                  (let loop-x ([x 0])
                       (when (< x sw)
                             (let ([ch (canvas-ref src x y)])
                                  (unless (transparent? ch)
                                          (canvas-set! dest (+ ox x) (+ oy y) ch)))
                             (loop-x (+ x 1))))
                  (loop-y (+ y 1))))
       dest))

(doc 'section 'layer-flattening)

(doc flatten-layers 'type (-> LayerStack Nat Nat Canvas))
(doc flatten-layers 'description "Composite all visible layers into a single canvas. Layers are composited in depth order (background first). Returns a single canvas with all visible layers composited.")
(doc flatten-layers 'param 'stack "The layer stack to flatten")
(doc flatten-layers 'param 'width "Width of the output canvas")
(doc flatten-layers 'param 'height "Height of the output canvas")
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

(doc 'section 'helper-constructors)

(doc make-background-layer 'type (-> Nat Nat Layer))
(doc make-background-layer 'description "Create a background layer (depth 0, filled with spaces).")
(define (make-background-layer width height)
  (make-layer 'background
              (make-canvas width height)
              0
              (point 0 0)))

(doc make-sprite-layer 'type (-> Symbol Nat Nat Point Layer))
(doc make-sprite-layer 'description "Create a sprite layer (depth 50, transparent canvas).")
(define (make-sprite-layer name width height offset)
  (make-layer name
              (make-transparent-canvas width height)
              50
              offset))

(doc make-ui-layer 'type (-> Nat Nat Layer))
(doc make-ui-layer 'description "Create a UI overlay layer (depth 100, transparent canvas).")
(define (make-ui-layer width height)
  (make-layer 'ui
              (make-transparent-canvas width height)
              100
              (point 0 0)))

(doc 'section 'drawing-to-layers)
(doc 'note "Convenience wrappers that work with layers directly, applying canvas drawing operations and returning updated layers.")

(doc layer-draw-string 'type (-> Layer Point String Layer))
(doc layer-draw-string 'description "Draw a string on a layer's canvas.")
(define (layer-draw-string layer pt str)
  (layer-set-canvas layer
                    (draw-string (layer-canvas layer) pt str)))

(doc layer-draw-char 'type (-> Layer Point Char Layer))
(doc layer-draw-char 'description "Draw a character on a layer's canvas.")
(define (layer-draw-char layer pt ch)
  (layer-set-canvas layer
                    (draw-char (layer-canvas layer) pt ch)))

(doc layer-fill-rect 'type (-> Layer Rect Char Layer))
(doc layer-fill-rect 'description "Fill a rectangle on a layer's canvas.")
(define (layer-fill-rect layer rect ch)
  (layer-set-canvas layer
                    (fill-rect (layer-canvas layer) rect ch)))

(doc layer-draw-box 'type (-> Layer Rect Symbol Layer))
(doc layer-draw-box 'description "Draw a box on a layer's canvas.")
(define (layer-draw-box layer rect style)
  (layer-set-canvas layer
                    (draw-box (layer-canvas layer) rect style)))

(doc 'section 'sprite-helpers)

(doc draw-sprite-to-layer 'type (-> Layer Point (List String) Layer))
(doc draw-sprite-to-layer 'description "Draw a sprite (list of strings) to a layer at the given position. Transparent characters in the sprite won't overwrite the layer.")
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

(doc 'section 'debug-and-inspection)

(doc layer->string 'type (-> Layer String))
(doc layer->string 'description "Convert a layer to a string for debugging.")
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

(doc stack->string 'type (-> LayerStack String))
(doc stack->string 'description "Convert a layer stack to a string for debugging.")
(define (stack->string stack)
  (let ([layers (stack-layers stack)])
       (string-append
        "LayerStack[\n"
        (fold-left (lambda (acc layer)
                           (string-append acc "  " (layer->string layer) "\n"))
                   ""
                   layers)
        "]")))

(doc 'section 'alpha-compositing)

(doc alpha-palette 'type (List Char))
(doc alpha-palette 'description "Block shading palette for alpha blending. Characters ordered from transparent to opaque.")
(define alpha-palette '(#\space #\░ #\▒ #\▓ #\█))

(doc blend-chars 'type (-> Char Char Real Char))
(doc blend-chars 'description "Blend two characters based on alpha value [0,1]. Alpha 0 = fully dest, Alpha 1 = fully src. For ASCII art, blending uses a threshold approach: Low alpha (<0.25) uses destination char, Medium-low (0.25-0.5) uses light shade, Medium-high (0.5-0.75) uses dark shade or source, High (>0.75) uses source char.")
(define (blend-chars dest-ch src-ch alpha)
  (let ([clamped (max 0.0 (min 1.0 alpha))])
       (cond
        ;; Fully transparent - use dest
        [(< clamped 0.1) dest-ch]
        ;; Low alpha - light hint of source
        [(< clamped 0.25)
         (if (char=? dest-ch #\space) #\░ dest-ch)]
        ;; Medium-low - visible blend
        [(< clamped 0.5) #\▒]
        ;; Medium-high - mostly source
        [(< clamped 0.75) #\▓]
        ;; High alpha - use source
        [else src-ch])))

(doc composite-with-alpha 'type (-> Canvas Canvas Point Real Canvas))
(doc composite-with-alpha 'description "Overlay source canvas onto destination at given position with alpha. Alpha value [0,1] controls blending of source onto destination. Unlike composite-transparent (which skips transparent cells), this function blends all cells based on the alpha value. Uses mutable canvas-set! for O(N) performance.")
(define (composite-with-alpha dest src pt alpha)
  (let ([ox (point-x pt)]
        [oy (point-y pt)]
        [sw (canvas-width src)]
        [sh (canvas-height src)])
       (let loop-y ([y 0])
            (when (< y sh)
                  (let loop-x ([x 0])
                       (when (< x sw)
                             (let* ([src-ch (canvas-ref src x y)]
                                    [dx (+ ox x)]
                                    [dy (+ oy y)]
                                    [dest-ch (canvas-ref dest dx dy)]
                                    [blended (blend-chars dest-ch src-ch alpha)])
                                   (canvas-set! dest dx dy blended))
                             (loop-x (+ x 1))))
                  (loop-y (+ y 1))))
       dest))

(doc 'note "Extended Layer with Opacity: The standard layer has depth but no opacity. For layers with opacity, use these functions that work with opacity stored in layer properties.")

(doc layer-with-opacity 'type (-> Layer Real (Pair Layer Real)))
(doc layer-with-opacity 'description "Create a layer with associated opacity for alpha compositing.")
(define (layer-with-opacity layer opacity)
  (cons layer (max 0.0 (min 1.0 opacity))))

(doc flatten-layers-with-alpha 'type (-> (List (Pair Layer Real)) Nat Nat Canvas))
(doc flatten-layers-with-alpha 'description "Composite layers with individual opacity values. Input is a list of (layer . opacity) pairs, sorted by depth.")
(define (flatten-layers-with-alpha layer-pairs width height)
  (let ([initial-canvas (make-canvas width height)])
       (let loop ([pairs layer-pairs]
                  [canvas initial-canvas])
            (if (null? pairs)
                canvas
                (let* ([pair (car pairs)]
                       [layer (car pair)]
                       [alpha (cdr pair)])
                      (if (layer-visible layer)
                          (loop (cdr pairs)
                                (composite-with-alpha canvas
                                                      (layer-canvas layer)
                                                      (layer-offset layer)
                                                      alpha))
                          (loop (cdr pairs) canvas)))))))

(doc 'section 'export-summary)
(doc 'note "Transparency: transparent-char, transparent?, make-transparent-canvas")
(doc 'note "Layers: make-layer, layer-name, layer-canvas, layer-visible, layer-depth, layer-offset, layer-set-visible, layer-set-canvas, layer-set-offset, layer-set-depth")
(doc 'note "Layer Stack: make-layer-stack, stack-layers, stack-add-layer, stack-remove-layer, stack-find-layer, stack-update-layer, stack-show-layer, stack-hide-layer, stack-reorder")
(doc 'note "Composition: composite-transparent, flatten-layers, composite-with-alpha, blend-chars, flatten-layers-with-alpha, layer-with-opacity, alpha-palette")
(doc 'note "Helpers: make-background-layer, make-sprite-layer, make-ui-layer, layer-draw-string, layer-draw-char, layer-fill-rect, layer-draw-box, draw-sprite-to-layer")
(doc 'note "Debug: layer->string, stack->string")
(doc 'note "Alpha Compositing: composite-with-alpha, blend-chars, layer-set-opacity")
