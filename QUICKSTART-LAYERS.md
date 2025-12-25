# Layering System - Quick Start Guide

## 5-Minute Introduction

### Load the System

```scheme
(load "shell/layout.ss")  ; Canvas primitives
(load "shell/layers.ss")  ; Layering system
```

### Create Your First Layered Scene

```scheme
;; 1. Create three layers at different depths
(define background (make-background-layer 50 20))     ; Depth 0
(define sprite (make-sprite-layer 'hero 10 5 (point 20 8)))  ; Depth 50
(define ui (make-ui-layer 50 20))                     ; Depth 100

;; 2. Draw on each layer
(define background
  (layer-fill-rect background (make-rect (point 0 0) 50 20) #\.))

(define sprite
  (layer-draw-string sprite (point 2 2) "HERO"))

(define ui
  (layer-draw-box ui (make-rect (point 0 0) 50 20) 'light))

;; 3. Create a layer stack
(define stack (make-layer-stack))
(define stack (stack-add-layer stack background))
(define stack (stack-add-layer stack sprite))
(define stack (stack-add-layer stack ui))

;; 4. Flatten to single canvas
(define final-canvas (flatten-layers stack 50 20))

;; 5. Display
(display (canvas->string final-canvas))
```

That's it! You now have a three-layer scene with:
- Background layer filled with dots
- Sprite layer with "HERO" text
- UI layer with a border box

## Key Concepts

### 1. Transparency

Cells with `#\nul` are transparent and don't overwrite the layer below:

```scheme
;; Create transparent canvas
(define layer (make-transparent-canvas 20 10))

;; Draw only where you want (rest stays transparent)
(define layer (canvas-set layer 5 5 #\X))
```

### 2. Depth (Z-order)

Lower numbers = background, higher numbers = foreground:

```scheme
(make-layer 'background canvas 0)    ; Back
(make-layer 'sprite canvas 50)       ; Middle
(make-layer 'ui canvas 100)          ; Front
```

### 3. Visibility

Show/hide layers without removing them:

```scheme
(define stack (stack-hide-layer stack 'sprite))  ; Hide
(define stack (stack-show-layer stack 'sprite))  ; Show
```

### 4. Flattening

Composite all visible layers into one canvas:

```scheme
(define canvas (flatten-layers stack width height))
```

## Common Patterns

### Pattern 1: Background + Sprite + UI

```scheme
;; Background (static environment)
(define bg (make-background-layer 60 20))
(define bg (layer-draw-string bg (point 0 10)
             (make-string 60 #\~)))  ; Water

;; Sprite (moving character)
(define hero (make-sprite-layer 'hero 8 4 (point 25 8)))
(define hero (layer-draw-string hero (point 0 0) "(o>"))

;; UI (status/info)
(define ui (make-ui-layer 60 20))
(define ui (layer-draw-box ui (make-rect (point 0 0) 60 20) 'light))

;; Combine
(define stack (make-layer-stack (list bg hero ui)))
(define scene (flatten-layers stack 60 20))
```

### Pattern 2: Dynamic Sprites

```scheme
;; Update sprite position
(define (move-sprite stack name x y)
  (stack-update-layer stack name
    (lambda (layer)
      (layer-set-offset layer (point x y)))))

;; Use it
(define stack (move-sprite stack 'hero 30 10))
```

### Pattern 3: Temporary Overlays

```scheme
;; Add modal dialog
(define modal (make-ui-layer 60 20))
(define modal (layer-draw-string modal (point 20 8) "[ Paused ]"))
(define modal (layer-set-depth modal 200))  ; Above everything

(define stack-paused (stack-add-layer stack modal))
(define paused-scene (flatten-layers stack-paused 60 20))

;; Remove when done
(define stack-resumed (stack-remove-layer stack-paused 'modal))
```

## API Cheat Sheet

### Create Layers
```scheme
(make-layer name canvas [depth] [offset])
(make-background-layer width height)
(make-sprite-layer name width height offset)
(make-ui-layer width height)
```

### Modify Layers
```scheme
(layer-set-visible layer bool)
(layer-set-canvas layer canvas)
(layer-set-offset layer point)
(layer-set-depth layer depth)
```

### Manage Stack
```scheme
(make-layer-stack [initial-layers])
(stack-add-layer stack layer)
(stack-remove-layer stack name)
(stack-find-layer stack name)
(stack-show-layer stack name)
(stack-hide-layer stack name)
(stack-reorder stack name depth)
```

### Draw on Layers
```scheme
(layer-draw-string layer point string)
(layer-draw-char layer point char)
(layer-fill-rect layer rect char)
(layer-draw-box layer rect style)
```

### Render
```scheme
(flatten-layers stack width height)
(canvas->string canvas)
```

## Running Examples

### Run the Demo
```bash
scheme --script demo-layers.ss
```

Shows:
- Basic layering concepts
- DUCKIE in a pond scene
- Layer visibility/reordering
- Animation simulation

### Run the Tests
```bash
scheme --script test-layers.ss
```

Validates all functionality with 30+ unit tests.

## DUCKIE Integration

```scheme
;; In your DUCKIE rendering function:
(define (render-duckie-scene duckie)
  (let* ([w 60] [h 20]

         ;; Layer 0: Pond environment
         [pond (make-background-layer w h)]
         [pond (layer-draw-string pond (point 0 10)
                 (make-string w #\~))]

         ;; Layer 50: DUCKIE sprite
         [sprite (make-sprite-layer 'duckie 10 6
                   (duckie-location duckie))]
         [sprite (draw-sprite-to-layer sprite (point 0 0)
                   (mood->sprite (duckie-mood duckie)))]

         ;; Layer 100: Status UI
         [ui (make-ui-layer w h)]
         [ui (layer-draw-box ui (make-rect (point 0 0) w h) 'light)]
         [ui (layer-draw-string ui (point 2 0)
               (string-append "[ " (duckie-name duckie) " ]"))]

         ;; Composite
         [stack (make-layer-stack (list pond sprite ui))])
    (flatten-layers stack w h)))
```

## Next Steps

1. **Read the full documentation**: `/home/user/the-fold/shell/LAYERS.md`
2. **Study the examples**: `/home/user/the-fold/demo-layers.ss`
3. **Explore the architecture**: `/home/user/the-fold/LAYERING-ARCHITECTURE.txt`
4. **Integrate with your code**: See `/home/user/the-fold/LAYERING-SYSTEM-SUMMARY.md`

## Tips

- **Use appropriate depths**: 0-49 background, 50-99 sprites, 100+ UI
- **Keep layer count reasonable**: <10 for interactive performance
- **Use transparent canvases for sprites**: Clean overlay without rectangles
- **Update layers functionally**: All operations return new values
- **Cache static layers**: Don't recreate background every frame

## Troubleshooting

**Q: My sprite has a rectangular background**
```scheme
;; Wrong: Using regular canvas (filled with spaces)
(make-layer 'sprite (make-canvas 10 5) 50 offset)

;; Right: Using transparent canvas
(make-layer 'sprite (make-transparent-canvas 10 5) 50 offset)
;; Or use the helper:
(make-sprite-layer 'sprite 10 5 offset)
```

**Q: Layers appear in wrong order**
```scheme
;; Layers are automatically sorted by depth
;; Just set appropriate depth values:
(make-layer 'bg canvas 0)      ; Back
(make-layer 'sprite canvas 50) ; Middle
(make-layer 'ui canvas 100)    ; Front
```

**Q: How do I animate?**
```scheme
;; Update layer offset each frame
(define (animate stack name x y)
  (stack-update-layer stack name
    (lambda (layer)
      (layer-set-offset layer (point x y)))))
```

## Get Help

- Review `/home/user/the-fold/shell/LAYERS.md` for complete API
- Check `/home/user/the-fold/test-layers.ss` for working examples
- See `/home/user/the-fold/LAYERING-SYSTEM-SUMMARY.md` for integration patterns

Happy layering!
