# Color & ANSI Support for The Fold Canvas System

## Overview

The Fold now supports full ANSI color rendering for terminal graphics. DUCKIE and all canvas-based rendering can now use colors to express mood, energy levels, and visual richness.

## Architecture

### Module Structure

```
shell/color.ss           — Color primitives and ANSI code generation
shell/layout-color.ss    — Color-enabled canvas system
shell/layout.ss          — Original monochrome system (unchanged)
```

The color system is **opt-in**: existing code continues to work with the monochrome `layout.ss`, while new colored rendering uses `layout-color.ss`.

## Files Created

### Core Color System

- **`shell/color.ss`** — Color data structures, ANSI escape sequences, and color helpers
  - Color representation (RGB truecolor, 256-color palette, default)
  - ANSI code generation (foreground, background, reset)
  - Cell type (character + foreground + background)
  - Color constants (red, blue, green, etc.)
  - Color manipulation (lerp, darken, lighten)
  - Mood-to-color mapping for DUCKIE
  - Energy-to-color gradient

- **`shell/layout-color.ss`** — Extended canvas system with full color support
  - Canvas stores cells (char + colors) instead of plain chars
  - All drawing primitives support color
  - New colored primitives: `draw-char-colored`, `draw-string-colored`, `fill-rect-colored`, `draw-box-colored`
  - `canvas->string` emits optimized ANSI color codes
  - Backward compatible: uncolored functions use default colors

### Demos & Tests

- **`demo-color.ss`** — Comprehensive color system demonstration
  - Basic 256-color palette
  - RGB truecolor gradients
  - Colored shapes and patterns
  - DUCKIE mood colors
  - Energy level visualization
  - Color helper functions

- **`demo-duckie-color.ss`** — DUCKIE rendered with mood-based colors
  - Shows all six moods with signature colors
  - Energy bars with color gradients
  - Colored borders and decorations

- **`shell/test-color.ss`** — Unit tests for color system
  - Color construction
  - ANSI code generation
  - Cell operations
  - Color helpers (lerp, darken, lighten)
  - Mood and energy color mapping

## API Reference

### Color Construction

```scheme
;; RGB Truecolor (24-bit)
(make-color-rgb r g b)        ; r, g, b are 0-255
(rgb 255 128 64)              ; Convenience constructor

;; 256-Color Palette
(make-color-palette n)        ; n is 0-255

;; Default (terminal's default color)
color-default
```

### Color Constants

```scheme
;; Basic 16 ANSI colors
color-black, color-red, color-green, color-yellow
color-blue, color-magenta, color-cyan, color-white
color-bright-red, color-bright-green, ...

;; Extended palette colors
color-orange, color-pink, color-purple, color-gold
color-gray, color-light-gray, color-dark-gray
```

### Cell Type

```scheme
;; Cell: character + foreground + background
(make-cell char fg bg)        ; Create a colored cell
(make-cell-simple char)       ; Create cell with default colors
default-cell                  ; Blank cell (#\space, default colors)

;; Accessors
(cell%-char cell)             ; Get character
(cell%-fg cell)               ; Get foreground color
(cell%-bg cell)               ; Get background color
```

### ANSI Code Generation

```scheme
(ansi-fg color)               ; Foreground color escape sequence
(ansi-bg color)               ; Background color escape sequence
(ansi-color fg bg)            ; Both foreground and background
ansi-reset                    ; Reset all attributes
```

### Canvas Operations (Colored)

```scheme
;; Colored drawing primitives
(draw-char-colored canvas pt char fg bg)
(draw-string-colored canvas pt str fg bg)
(fill-rect-colored canvas rect char fg bg)
(draw-box-colored canvas rect style fg bg)

;; Original uncolored functions still work
(draw-char canvas pt char)               ; Uses default colors
(draw-string canvas pt str)              ; Uses default colors
```

### Color Helpers

```scheme
;; Color interpolation
(lerp-color c1 c2 t)          ; t ∈ [0.0, 1.0], RGB only

;; Lightening and darkening
(darken color factor)         ; factor ∈ [0.0, 1.0], RGB only
(lighten color factor)        ; factor ∈ [0.0, 1.0], RGB only
```

### Mood & Energy Colors

```scheme
;; Map DUCKIE mood to signature color
(mood->color mood)            ; mood: 'happy, 'curious, 'sleepy, etc.

;; Map energy level to color gradient
(energy->color energy)        ; energy: 0-100
                             ; 0 = dark blue, 100 = bright yellow
```

## Mood Color Scheme

| Mood      | Color          | RGB              | Meaning            |
|-----------|----------------|------------------|--------------------|
| happy     | Gold/Yellow    | (255, 215, 0)    | Bright, cheerful   |
| curious   | Light Blue     | (100, 200, 255)  | Inquisitive        |
| sleepy    | Soft Purple    | (180, 180, 220)  | Drowsy, calm       |
| content   | Soft Green     | (150, 220, 150)  | Peaceful           |
| lonely    | Muted Blue     | (150, 150, 200)  | Melancholy         |
| playful   | Pink           | (255, 150, 200)  | Energetic, fun     |

## Usage Examples

### Basic Colored Text

```scheme
(load "shell/layout-color.ss")

(define c (make-canvas 40 10))

;; Draw red text on yellow background
(define c (draw-string-colored c (point 5 3) "Hello, DUCKIE!"
                              color-red color-yellow))

;; Draw with RGB truecolor
(define c (draw-string-colored c (point 5 5) "RGB colors!"
                              (rgb 100 200 255) color-default))

(display (canvas->string c))
```

### DUCKIE with Mood Colors

```scheme
(load "shell/layout-color.ss")

(define (render-duckie-mood duckie-state)
  (let* ([mood (duckie-mood duckie-state)]
         [energy (duckie-energy duckie-state)]
         [mood-color (mood->color mood)]
         [c (make-canvas 50 20)])

    ;; Draw DUCKIE in mood color
    (define c (draw-sprite-colored c (point 18 5) duckie-sprite mood-color))

    ;; Draw energy bar with gradient
    (define c (draw-energy-bar c (point 5 15) energy))

    (display (canvas->string c))))
```

### Color Gradients

```scheme
;; Horizontal gradient from red to blue
(define (draw-gradient canvas y width)
  (let loop ([x 0] [c canvas])
    (if (>= x width)
        c
        (let* ([t (/ x (- width 1.0))]
               [color (lerp-color (rgb 255 0 0) (rgb 0 0 255) t)])
          (loop (+ x 1)
                (draw-char-colored c (point x y) #\█ color color))))))
```

## Design Principles

### 1. Functional & Immutable

All color operations are pure functions. Canvases remain immutable.

```scheme
(define c1 (make-canvas 10 10))
(define c2 (draw-char-colored c1 (point 5 5) #\X color-red color-default))
;; c1 is unchanged, c2 is a new canvas
```

### 2. Backward Compatible

Existing code using `layout.ss` continues to work unchanged. Color support is opt-in via `layout-color.ss`.

### 3. Optimized ANSI Output

`canvas->string` only emits ANSI codes when colors change, minimizing output size:

```
\x1B[31mRED\x1B[39mdefault\x1B[34mBLUE\x1B[0m
```

### 4. Expressive Mood System

Colors are not arbitrary — they express DUCKIE's emotional state:
- **Happy** = Gold (warm, bright)
- **Sleepy** = Purple (drowsy, muted)
- **Curious** = Blue (cool, alert)

## Running the Demos

```bash
# Color system demonstration
scheme --script demo-color.ss

# DUCKIE with mood colors
scheme --script demo-duckie-color.ss

# Color unit tests
scheme --script shell/test-color.ss
```

## Implementation Notes

### ANSI Color Modes

The system supports two color modes:

1. **256-color palette** (`\x1B[38;5;Nm`): Compatible with most terminals
2. **24-bit truecolor** (`\x1B[38;2;R;G;Bm`): Full RGB, modern terminals only

### Terminal Compatibility

ANSI color codes work in:
- Modern terminal emulators (iTerm2, Alacritty, Windows Terminal)
- tmux, screen (with proper `TERM` settings)
- SSH sessions (if client supports ANSI)

May not work in:
- Very old terminals
- Pure text console (no ANSI support)
- Redirected output (`> file.txt` will contain escape codes)

### Performance

Color rendering adds minimal overhead:
- Cell size: ~24 bytes (char + 2 colors)
- ANSI code optimization reduces output size
- Gradient generation is O(n) in gradient length

## Future Extensions

Potential enhancements:

1. **Text Styles**: Bold, italic, underline via ANSI codes
2. **Alpha Blending**: Transparency and alpha compositing
3. **Color Themes**: Switchable color schemes (dark mode, light mode)
4. **Animation**: Color transitions and pulsing effects
5. **Sprite Sheets**: Multi-frame colored sprites with animation

## Integration with DUCKIE Loop

To use colors in `shell/duckie-loop.ss`:

```scheme
;; Load color-enabled layout instead of layout.ss
(load "shell/layout-color.ss")

;; In render-duckie function:
(define (render-duckie state)
  (let* ([duckie (loop-state-duckie state)]
         [mood (duckie-mood duckie)]
         [mood-color (mood->color mood)]
         ...)
    ;; Use colored drawing functions
    (draw-sprite-colored canvas pt sprite mood-color)))
```

## Philosophy

> Color is not decoration — it is expression.
> DUCKIE's yellow is not a random choice.
> It is the visual language of joy.
>
> When DUCKIE is sleepy, the world shifts to purple.
> When lonely, the hues grow muted and blue.
> Color becomes the window to digital emotion.

## Credits

Implemented by Claude Sonnet 4.5 for The Fold project.

Following the functional, immutable design patterns established in the codebase.

Color schemes designed to express DUCKIE's emotional palette.

---

**Status**: ✓ Implemented and tested
**Version**: 1.0
**Date**: 2024-12-25
