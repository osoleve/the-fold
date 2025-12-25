# Canvas Layering & Depth System

## Overview

The layering system extends The Fold's canvas with transparency support and z-ordered composition. This enables complex visual scenes with multiple independent layers that can be shown, hidden, reordered, and composited efficiently.

## Architecture

### Core Concepts

1. **Transparency**: Canvas cells can be transparent (using `#\nul` character), allowing sprites and overlays to blend naturally without rectangular backgrounds.

2. **Layers**: Each layer is a named canvas with metadata:
   - **Name**: Symbol identifier for the layer
   - **Canvas**: The actual visual content
   - **Visible**: Boolean flag for show/hide
   - **Depth**: Z-order value (lower = background, higher = foreground)
   - **Offset**: Position offset when compositing

3. **Layer Stack**: Ordered collection of layers maintained in depth order. The stack manages layer composition and provides operations for manipulation.

4. **Flattening**: The process of compositing all visible layers into a single output canvas, respecting transparency and z-order.

## Files

- **shell/layers.ss**: Complete layering system implementation
- **shell/layout.ss**: Updated with transparency-aware composite function
- **demo-layers.ss**: Comprehensive demonstration of layering capabilities

## API Reference

### Transparency

```scheme
transparent-char          ; #\nul — the transparency marker
(transparent? ch)         ; Check if character is transparent
(make-transparent-canvas width height)  ; Create transparent canvas
```

### Layer Construction

```scheme
(make-layer name canvas)                    ; Depth 0, origin offset
(make-layer name canvas depth)              ; Custom depth, origin offset
(make-layer name canvas depth offset)       ; Full control

;; Convenience constructors
(make-background-layer width height)        ; Depth 0, filled with spaces
(make-sprite-layer name width height offset) ; Depth 50, transparent
(make-ui-layer width height)                ; Depth 100, transparent
```

### Layer Accessors

```scheme
(layer-name layer)        ; Get layer name (Symbol)
(layer-canvas layer)      ; Get layer canvas
(layer-visible layer)     ; Get visibility (Bool)
(layer-depth layer)       ; Get z-order (Nat)
(layer-offset layer)      ; Get position offset (Point)
```

### Layer Mutation (Functional)

```scheme
(layer-set-visible layer bool)      ; Show/hide layer
(layer-set-canvas layer canvas)     ; Update content
(layer-set-offset layer point)      ; Move layer
(layer-set-depth layer nat)         ; Change z-order
```

### Layer Stack Operations

```scheme
(make-layer-stack)                  ; Empty stack
(make-layer-stack layers)           ; Stack with initial layers

(stack-layers stack)                ; Get all layers in depth order
(stack-add-layer stack layer)       ; Add layer (maintains order)
(stack-remove-layer stack name)     ; Remove layer by name
(stack-find-layer stack name)       ; Find layer by name

(stack-update-layer stack name fn)  ; Update layer using function
(stack-show-layer stack name)       ; Make layer visible
(stack-hide-layer stack name)       ; Make layer invisible
(stack-reorder stack name depth)    ; Change layer depth
```

### Composition

```scheme
;; Transparency-aware compositing
(composite-transparent dest src offset)

;; Flatten all visible layers
(flatten-layers stack width height)
```

### Drawing Helpers

```scheme
(layer-draw-string layer point str)     ; Draw string on layer
(layer-draw-char layer point char)      ; Draw character on layer
(layer-fill-rect layer rect char)       ; Fill rectangle on layer
(layer-draw-box layer rect style)       ; Draw box on layer
(draw-sprite-to-layer layer offset lines) ; Draw sprite (list of strings)
```

### Debug Utilities

```scheme
(layer->string layer)     ; Layer info as string
(stack->string stack)     ; Stack info as string
```

## Usage Examples

### Example 1: Basic Three-Layer Scene

```scheme
(load "shell/layout.ss")
(load "shell/layers.ss")

;; Create three layers
(define bg (make-background-layer 50 20))
(define sprite (make-sprite-layer 'duckie 10 5 (point 20 8)))
(define ui (make-ui-layer 50 20))

;; Draw on each layer
(define bg (layer-fill-rect bg (make-rect (point 0 0) 50 20) #\.))
(define sprite (layer-draw-string sprite (point 2 2) "DUCKIE"))
(define ui (layer-draw-box ui (make-rect (point 0 0) 50 20) 'light))

;; Create stack and add layers
(define stack (make-layer-stack))
(define stack (stack-add-layer stack bg))
(define stack (stack-add-layer stack sprite))
(define stack (stack-add-layer stack ui))

;; Render final image
(define final (flatten-layers stack 50 20))
(display (canvas->string final))
```

### Example 2: DUCKIE Scene with Environment

```scheme
;; Background layer: pond environment
(define pond-layer (make-background-layer 60 20))
(define pond-layer (layer-draw-string pond-layer (point 0 10)
                     (make-string 60 #\~)))  ; Water line
(define pond-layer (layer-draw-string pond-layer (point 5 11) "~ ~ ~"))

;; Sprite layer: DUCKIE
(define duckie-sprite '("  (o> " " _(()_" "  \\^^/"))
(define duckie-layer (make-sprite-layer 'duckie 8 4 (point 25 8)))
(define duckie-layer (draw-sprite-to-layer duckie-layer (point 0 0)
                                           duckie-sprite))

;; UI layer: status bar
(define ui-layer (make-ui-layer 60 20))
(define ui-layer (layer-draw-box ui-layer (make-rect (point 0 0) 60 20) 'light))
(define ui-layer (layer-draw-string ui-layer (point 2 0) "[ Proto - Happy ]"))

;; Compose
(define stack (make-layer-stack (list pond-layer duckie-layer ui-layer)))
(define scene (flatten-layers stack 60 20))
(display (canvas->string scene))
```

### Example 3: Dynamic Layer Management

```scheme
;; Start with visible layers
(define stack (make-layer-stack (list bg sprite ui)))

;; Hide sprite temporarily
(define stack (stack-hide-layer stack 'sprite))
(define scene1 (flatten-layers stack 60 20))  ; Sprite hidden

;; Show sprite again
(define stack (stack-show-layer stack 'sprite))
(define scene2 (flatten-layers stack 60 20))  ; Sprite visible

;; Move sprite to front (higher depth)
(define stack (stack-reorder stack 'sprite 200))
(define scene3 (flatten-layers stack 60 20))  ; Sprite above UI
```

### Example 4: Animation with Layers

```scheme
;; Update sprite position each frame
(define (animate-duckie stack x)
  (let* ([sprite-layer (stack-find-layer stack 'duckie)]
         [new-sprite (layer-set-offset sprite-layer (point x 10))])
    (stack-update-layer stack 'duckie
                       (lambda (layer) new-sprite))))

;; Render frames
(define (render-frame stack x)
  (let* ([stack (animate-duckie stack x)]
         [canvas (flatten-layers stack 60 20)])
    (display (canvas->string canvas))))

;; Animate DUCKIE moving across screen
(let loop ([x 0])
  (when (< x 50)
    (render-frame initial-stack x)
    (loop (+ x 5))))
```

## Design Principles

### 1. Functional & Immutable

All layer operations return new layers/stacks rather than mutating in place. This ensures:
- Predictable behavior
- Easy undo/redo
- No unexpected side effects
- Thread-safe composition

### 2. Separation of Concerns

- **Canvas**: Low-level 2D character grid (layout.ss)
- **Layers**: High-level composition with transparency (layers.ss)
- **Rendering**: Converts canvas to display strings (layout.ss)

### 3. Performance Considerations

- **Layer Count**: O(n) where n = number of visible layers
- **Canvas Size**: O(w×h) for each canvas operation
- **Flattening**: O(n×w×h) for compositing all layers
- **Optimization**: Keep layer count reasonable (<10 for interactive use)

### 4. Transparency Model

- Transparent cells use `#\nul` (Unicode NULL character)
- Rarely appears in ASCII art, making it safe as a marker
- Alternative: Could use custom sentinel value if needed
- Transparent cells are skipped during composition

## Common Patterns

### Pattern 1: Background-Sprite-UI

Most scenes use three depth levels:

```scheme
Layer 0:   Background (environment, scenery)
Layer 50:  Sprites (characters, objects)
Layer 100: UI (text overlays, borders, HUD)
```

### Pattern 2: Multiple Sprite Layers

For scenes with multiple characters:

```scheme
Layer 0:   Background
Layer 40:  Enemy sprites
Layer 50:  Player sprite
Layer 60:  Particle effects
Layer 100: UI
```

### Pattern 3: Parallax Scrolling

Different backgrounds at different depths:

```scheme
Layer 0:   Far background (mountains) — slow scroll
Layer 10:  Mid background (trees) — medium scroll
Layer 20:  Near background (grass) — fast scroll
Layer 50:  Character sprite — no scroll
Layer 100: UI — no scroll
```

### Pattern 4: Modal Overlays

Temporarily show UI over scene:

```scheme
;; Normal rendering
(define scene (flatten-layers normal-stack w h))

;; Add modal dialog
(define modal-layer (make-ui-layer w h))
(define modal-layer (layer-draw-box modal-layer ...))
(define modal-stack (stack-add-layer normal-stack modal-layer))
(define scene-with-modal (flatten-layers modal-stack w h))

;; Remove modal
(define normal-stack (stack-remove-layer modal-stack 'modal))
```

## Integration with DUCKIE

The layering system integrates naturally with DUCKIE rendering:

```scheme
;;; In shell/duckie-loop.ss:

(define (render-duckie-with-layers state)
  (let* ([duckie (loop-state-duckie state)]
         [width 60]
         [height 20]

         ;; Layer 0: Environment
         [env-layer (create-environment-layer width height)]

         ;; Layer 50: DUCKIE sprite
         [sprite (mood->sprite (duckie-mood duckie))]
         [sprite-layer (make-sprite-layer 'duckie 10 6
                                         (duckie-location duckie))]
         [sprite-layer (draw-sprite-to-layer sprite-layer
                                            (point 0 0) sprite)]

         ;; Layer 100: Status UI
         [ui-layer (create-status-ui duckie width height)]

         ;; Compose
         [stack (make-layer-stack (list env-layer sprite-layer ui-layer))]
         [canvas (flatten-layers stack width height)])
    canvas))
```

## Backwards Compatibility

The layering system is fully compatible with existing canvas code:

- `composite` function unchanged (opaque compositing)
- `composite-with-transparency` added for transparent compositing
- All existing drawing primitives work on layer canvases
- Layers are optional — simple scenes can still use plain canvas

## Future Extensions

Potential enhancements to the layering system:

1. **Alpha Blending**: Partial transparency with blend modes
2. **Layer Groups**: Nested layer hierarchies
3. **Layer Transforms**: Rotation, scaling, skewing
4. **Layer Effects**: Blur, shadows, glow
5. **Clipping Masks**: Constrain drawing to shapes
6. **Layer Caching**: Memoize expensive layer renders
7. **Dirty Rectangles**: Only re-composite changed regions

## Performance Tips

1. **Minimize Layer Count**: Merge static layers when possible
2. **Use Appropriate Sizes**: Don't make sprite layers screen-sized
3. **Cache Sprites**: Pre-render complex sprites to layers
4. **Limit Recomposition**: Only flatten when scene changes
5. **Profile First**: Measure before optimizing

## References

- **shell/layout.ss**: Base canvas implementation
- **shell/layers.ss**: Full layering system source
- **demo-layers.ss**: Comprehensive examples
- **playpen/duckie.ss**: DUCKIE soul definition
- **shell/duckie-loop.ss**: DUCKIE rendering loop

---

**Implemented by**: Claude Sonnet (Graphics Tools Task)
**Date**: December 25, 2025
**Status**: Complete and functional
