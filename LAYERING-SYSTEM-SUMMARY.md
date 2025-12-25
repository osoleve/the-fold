# Layering & Depth System - Implementation Summary

## Overview

Successfully implemented a complete layering and depth system for The Fold's canvas, enabling transparent sprites, z-ordered composition, and complex multi-layer scenes.

## Files Created/Modified

### New Files

1. **`/home/user/the-fold/shell/layers.ss`** (330 lines)
   - Complete layering system implementation
   - Transparency support with `#\nul` character
   - Layer and LayerStack data structures
   - Z-ordering and composition algorithms
   - Helper functions for common use cases
   - Debug utilities

2. **`/home/user/the-fold/demo-layers.ss`** (420 lines)
   - Comprehensive demonstration of layering capabilities
   - 4 progressive demos showing different features
   - DUCKIE integration examples
   - Animation examples
   - Executable script format

3. **`/home/user/the-fold/test-layers.ss`** (400 lines)
   - Unit tests for all layering functionality
   - Tests transparency, layers, stacks, composition
   - Validates helper constructors
   - Test runner with pass/fail reporting

4. **`/home/user/the-fold/shell/LAYERS.md`** (500+ lines)
   - Complete API documentation
   - Usage examples for all major features
   - Design principles and architecture
   - Integration patterns
   - Performance considerations
   - Future extension ideas

### Modified Files

1. **`/home/user/the-fold/shell/layout.ss`**
   - Added reference to layers.ss in header comments
   - Added `composite-with-transparency` function (new)
   - Original `composite` function unchanged (backward compatible)
   - Documentation updated

## Implementation Details

### 1. Transparency Support

**Transparency Character**: `#\nul` (Unicode NULL, U+0000)
- Rarely used in ASCII art, making it safe as a marker
- All transparent cells skip composition
- New function: `transparent?` checks if character is transparent
- New function: `make-transparent-canvas` creates empty canvas

**Key Functions**:
```scheme
transparent-char              ; #\nul constant
(transparent? ch)             ; Predicate
(make-transparent-canvas w h) ; Constructor
```

### 2. Layer System

**Layer Data Structure**:
```scheme
(define-record-type layer%
  (fields name     ; Symbol identifier
          canvas   ; The visual content
          visible  ; Boolean show/hide
          depth    ; Nat z-order (0=back, 100=front)
          offset)) ; Point position
```

**Layer Operations**:
- `make-layer` - Create new layer
- `layer-set-visible` - Show/hide
- `layer-set-canvas` - Update content
- `layer-set-offset` - Move position
- `layer-set-depth` - Change z-order

All operations are **functional** (return new layer, don't mutate).

### 3. Layer Stack

**LayerStack Data Structure**:
```scheme
(define-record-type layer-stack%
  (fields layers))  ; List of layers, sorted by depth
```

**Stack Operations**:
- `make-layer-stack` - Create empty or initialized stack
- `stack-add-layer` - Add layer (maintains depth order)
- `stack-remove-layer` - Remove by name
- `stack-find-layer` - Find by name
- `stack-update-layer` - Functional update
- `stack-show-layer`, `stack-hide-layer` - Visibility
- `stack-reorder` - Change layer depth

Layers are automatically **sorted by depth** (background to foreground).

### 4. Z-ordering & Composition

**Depth Conventions**:
- **0-49**: Background layers (environment, scenery)
- **50-99**: Sprite layers (characters, objects)
- **100+**: UI layers (overlays, HUD, text)

**Composition Algorithm**:
```scheme
(flatten-layers stack width height)
```

1. Start with blank canvas
2. Iterate layers in depth order (low to high)
3. For each visible layer:
   - Composite onto canvas at layer's offset
   - Skip transparent cells (don't overwrite)
4. Return final composited canvas

**Complexity**: O(n × w × h) where n=visible layers, w=width, h=height

### 5. Helper Constructors

Pre-configured layer constructors for common patterns:

```scheme
(make-background-layer w h)      ; Depth 0, opaque
(make-sprite-layer name w h pt)  ; Depth 50, transparent
(make-ui-layer w h)              ; Depth 100, transparent
```

### 6. Drawing Helpers

Convenience functions for drawing directly on layers:

```scheme
(layer-draw-string layer pt str)
(layer-draw-char layer pt ch)
(layer-fill-rect layer rect ch)
(layer-draw-box layer rect style)
(draw-sprite-to-layer layer pt sprite-lines)
```

## Integration with Existing Code

### Compatible with Current Canvas API

The layering system **extends** rather than replaces the canvas API:

- All existing canvas functions work unchanged
- Layers internally use canvas data structures
- Original `composite` function preserved
- No breaking changes to existing code

### Integration with DUCKIE

Example integration with DUCKIE rendering:

```scheme
;;; In shell/duckie-loop.ss, modify render-duckie:

(define (render-duckie-layered state)
  (let* ([duckie (loop-state-duckie state)]
         [width 60]
         [height 20]

         ;; Layer 0: Background
         [bg (make-background-layer width height)]
         [bg (layer-draw-string bg (point 0 10) (make-string width #\~))]

         ;; Layer 50: DUCKIE sprite
         [sprite (make-sprite-layer 'duckie 10 6 (duckie-location duckie))]
         [sprite (draw-sprite-to-layer sprite (point 0 0)
                   (mood->sprite (duckie-mood duckie)))]

         ;; Layer 100: UI
         [ui (make-ui-layer width height)]
         [ui (layer-draw-box ui (make-rect (point 0 0) width height) 'light)]
         [ui (layer-draw-string ui (point 2 0)
               (string-append "[ " (duckie-name duckie) " ]"))]

         ;; Compose
         [stack (make-layer-stack (list bg sprite ui))]
         [canvas (flatten-layers stack width height)])
    canvas))
```

### Loading Order

```scheme
;; In your main file or REPL:
(load "shell/layout.ss")   ; Load canvas first
(load "shell/layers.ss")   ; Then load layers
```

## Key Features

### 1. Transparency
- Transparent cells don't overwrite destination
- Enables sprite overlays without rectangular backgrounds
- Clean composition of irregular shapes

### 2. Z-ordering
- Layers render back-to-front by depth
- Automatic depth sorting
- Dynamic reordering support

### 3. Layer Management
- Show/hide layers without removing them
- Move layers with offsets
- Update layer content independently
- Find and modify layers by name

### 4. Functional Design
- All operations return new values
- No mutation (except internal vector copying for performance)
- Predictable behavior
- Easy undo/redo support

### 5. Performance
- Efficient vector-based canvas storage
- Only visible layers composited
- Sorted depth list for O(n) flattening
- Reasonable for interactive use (<10 layers)

## Usage Examples

### Example 1: Simple Three-Layer Scene

```scheme
(load "shell/layout.ss")
(load "shell/layers.ss")

;; Create layers
(define bg (make-background-layer 50 20))
(define sprite (make-sprite-layer 'hero 10 5 (point 20 8)))
(define ui (make-ui-layer 50 20))

;; Draw on layers
(define bg (layer-fill-rect bg (make-rect (point 0 0) 50 20) #\.))
(define sprite (layer-draw-string sprite (point 2 2) "HERO"))
(define ui (layer-draw-box ui (make-rect (point 0 0) 50 20) 'light))

;; Compose
(define stack (stack-add-layer
               (stack-add-layer
                (stack-add-layer (make-layer-stack) bg)
                sprite)
               ui))
(define final (flatten-layers stack 50 20))

;; Display
(display (canvas->string final))
```

### Example 2: Dynamic Layer Visibility

```scheme
;; Show/hide sprite layer
(define stack-no-sprite (stack-hide-layer stack 'hero))
(define scene1 (flatten-layers stack-no-sprite 50 20))

(define stack-with-sprite (stack-show-layer stack-no-sprite 'hero))
(define scene2 (flatten-layers stack-with-sprite 50 20))
```

### Example 3: Animation

```scheme
;; Move sprite across screen
(define (animate-hero stack x)
  (stack-update-layer stack 'hero
    (lambda (layer)
      (layer-set-offset layer (point x 10)))))

;; Render frames
(let loop ([x 0])
  (when (< x 40)
    (let* ([stack (animate-hero initial-stack x)]
           [canvas (flatten-layers stack 50 20)])
      (clear-screen)
      (display (canvas->string canvas))
      (sleep 0.1)
      (loop (+ x 2)))))
```

## Demonstration

Run the comprehensive demo:

```bash
scheme --script demo-layers.ss
```

The demo shows:
1. Basic three-layer composition
2. DUCKIE in a layered pond scene
3. Layer visibility and reordering
4. Simulated animation with moving sprites

## Testing

Run the unit tests:

```bash
scheme --script test-layers.ss
```

Tests cover:
- Transparency support
- Layer construction and operations
- Layer stack management
- Composition and flattening
- Helper constructors

## Design Principles

### 1. Functional & Immutable
All operations return new values, ensuring predictable behavior and thread safety.

### 2. Separation of Concerns
- **layout.ss**: Low-level canvas primitives
- **layers.ss**: High-level composition with transparency
- Clear API boundaries

### 3. Backward Compatibility
No breaking changes to existing code. Layers are opt-in.

### 4. Performance
Optimized for interactive use with reasonable layer counts.

## Future Extensions

Potential enhancements:

1. **Alpha Blending**: Partial transparency with blend modes
2. **Layer Groups**: Nested layer hierarchies
3. **Transforms**: Rotation, scaling, skewing
4. **Effects**: Blur, shadows, glow
5. **Clipping Masks**: Constrain drawing to shapes
6. **Caching**: Memoize expensive layer renders
7. **Dirty Rectangles**: Only recomposite changed regions

## File Locations

All files are in the repository root or `/home/user/the-fold/shell/`:

- `/home/user/the-fold/shell/layers.ss` - Implementation
- `/home/user/the-fold/shell/layout.ss` - Updated canvas (modified)
- `/home/user/the-fold/demo-layers.ss` - Demonstrations
- `/home/user/the-fold/test-layers.ss` - Unit tests
- `/home/user/the-fold/shell/LAYERS.md` - Documentation
- `/home/user/the-fold/LAYERING-SYSTEM-SUMMARY.md` - This file

## API Quick Reference

### Transparency
```scheme
transparent-char
(transparent? ch)
(make-transparent-canvas w h)
```

### Layers
```scheme
(make-layer name canvas [depth] [offset])
(layer-name layer)
(layer-canvas layer)
(layer-visible layer)
(layer-depth layer)
(layer-offset layer)
(layer-set-visible layer bool)
(layer-set-canvas layer canvas)
(layer-set-offset layer point)
(layer-set-depth layer nat)
```

### Layer Stack
```scheme
(make-layer-stack [initial-layers])
(stack-layers stack)
(stack-add-layer stack layer)
(stack-remove-layer stack name)
(stack-find-layer stack name)
(stack-update-layer stack name fn)
(stack-show-layer stack name)
(stack-hide-layer stack name)
(stack-reorder stack name depth)
```

### Composition
```scheme
(composite-transparent dest src offset)
(flatten-layers stack width height)
```

### Helpers
```scheme
(make-background-layer w h)
(make-sprite-layer name w h offset)
(make-ui-layer w h)
(layer-draw-string layer pt str)
(layer-draw-char layer pt ch)
(layer-fill-rect layer rect ch)
(layer-draw-box layer rect style)
(draw-sprite-to-layer layer offset sprite-lines)
```

### Debug
```scheme
(layer->string layer)
(stack->string stack)
```

## Conclusion

The layering system is **complete and functional**. It provides:

- **Transparency** - Clean sprite overlays
- **Z-ordering** - Proper depth management
- **Layer management** - Dynamic show/hide/reorder
- **Clean API** - Functional, immutable design
- **Integration** - Compatible with existing canvas code
- **Documentation** - Comprehensive examples and tests

The system is ready for use in DUCKIE rendering and other graphical applications in The Fold project.

---

**Implementation Date**: December 25, 2025
**Implemented By**: Claude Sonnet 4.5
**Status**: ✓ Complete and tested
