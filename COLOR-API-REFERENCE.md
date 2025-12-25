# Color API Quick Reference

## Quick Start

```scheme
;; Load the color-enabled canvas system
(load "shell/layout-color.ss")

;; Create a canvas
(define c (make-canvas 40 10))

;; Draw colored text
(define c (draw-string-colored c (point 5 3) "Hello!"
                              color-red color-yellow))

;; Display with ANSI codes
(display (canvas->string c))
```

## Color Constructors

```scheme
(rgb 255 128 64)                    ; RGB truecolor
(make-color-palette 42)             ; 256-color palette index
color-default                       ; Terminal default
```

## Predefined Colors

```scheme
;; Basic colors (palette 0-15)
color-black color-red color-green color-yellow
color-blue color-magenta color-cyan color-white

;; Bright variants
color-bright-red color-bright-green color-bright-blue
color-bright-yellow color-bright-magenta color-bright-cyan
color-bright-black color-bright-white

;; Extended colors
color-orange color-pink color-purple color-gold
color-gray color-light-gray color-dark-gray
```

## Drawing Functions

### Colored Primitives

```scheme
;; Single character
(draw-char-colored canvas point char fg-color bg-color)

;; String (horizontal)
(draw-string-colored canvas point string fg-color bg-color)

;; Filled rectangle
(fill-rect-colored canvas rect char fg-color bg-color)

;; Box border
(draw-box-colored canvas rect style fg-color bg-color)
  ; style: 'light 'heavy 'double 'ascii
```

### Default Color Variants

```scheme
;; These use default colors (backward compatible)
(draw-char canvas point char)
(draw-string canvas point string)
(fill-rect canvas rect char)
(draw-box canvas rect style)
```

## Cell Operations

```scheme
;; Create cells
(make-cell char fg bg)              ; Full cell
(make-cell-simple char)             ; Default colors
default-cell                        ; Space + defaults

;; Access cells
(canvas-ref canvas x y)             ; Get cell at position
(cell%-char cell)                   ; Get character
(cell%-fg cell)                     ; Get foreground
(cell%-bg cell)                     ; Get background

;; Set cells
(canvas-set-cell canvas x y cell)   ; Set entire cell
```

## Color Helpers

```scheme
;; Interpolation (RGB only)
(lerp-color color1 color2 t)        ; t in [0.0, 1.0]

;; Brightness (RGB only)
(darken color factor)               ; factor in [0.0, 1.0]
(lighten color factor)              ; factor in [0.0, 1.0]
```

## DUCKIE Integration

```scheme
;; Mood colors
(mood->color 'happy)                ; Gold/yellow
(mood->color 'curious)              ; Light blue
(mood->color 'sleepy)               ; Soft purple
(mood->color 'content)              ; Soft green
(mood->color 'lonely)               ; Muted blue
(mood->color 'playful)              ; Pink

;; Energy gradient (0-100)
(energy->color 0)                   ; Dark blue (low)
(energy->color 50)                  ; Medium
(energy->color 100)                 ; Bright yellow (high)
```

## ANSI Codes (Low-Level)

```scheme
(ansi-fg color)                     ; Foreground escape sequence
(ansi-bg color)                     ; Background escape sequence
(ansi-color fg bg)                  ; Combined FG+BG
ansi-reset                          ; Reset all ("\x1B[0m")
```

## Common Patterns

### Gradient Bar

```scheme
(define (draw-gradient c y width)
  (let loop ([x 0] [canvas c])
    (if (>= x width)
        canvas
        (let* ([t (/ x (- width 1.0))]
               [color (lerp-color (rgb 255 0 0) (rgb 0 0 255) t)])
          (loop (+ x 1)
                (draw-char-colored canvas (point x y) #\█ color color))))))
```

### Energy Bar

```scheme
(define (draw-energy-bar c origin energy max-width)
  (let* ([filled (quotient (* energy max-width) 100)]
         [x0 (point-x origin)]
         [y0 (point-y origin)])
    (let loop ([i 0] [canvas c])
      (if (>= i filled)
          canvas
          (let ([color (energy->color (quotient (* i 100) max-width))])
            (loop (+ i 1)
                  (draw-char-colored canvas (point (+ x0 i) y0)
                                   #\█ color color)))))))
```

### Colored Box with Title

```scheme
(define (draw-titled-box c rect title mood)
  (let* ([mood-color (mood->color mood)]
         [c (draw-box-colored c rect 'double mood-color color-default)]
         [title-x (+ (point-x (rect-origin rect)) 2)]
         [title-y (point-y (rect-origin rect))])
    (draw-string-colored c (point title-x title-y)
                        (string-append "[ " title " ]")
                        color-bright-white color-default)))
```

### Sprite with Mood Color

```scheme
(define (draw-sprite-colored c pt sprite-lines fg)
  (let loop ([lines sprite-lines] [row 0] [canvas c])
    (if (null? lines)
        canvas
        (loop (cdr lines)
              (+ row 1)
              (draw-string-colored canvas
                                  (point (point-x pt) (+ (point-y pt) row))
                                  (car lines)
                                  fg
                                  color-default)))))
```

## Type Signatures

```scheme
;; Types
Color     = 'default | (rgb Nat Nat Nat) | (palette Nat)
Cell      = (cell Char Color Color)
Canvas    = (canvas Nat Nat (Vector Cell))
Point     = (Nat . Nat)
Rect      = (rect Point Nat Nat)

;; Core functions
make-color-rgb    : Nat × Nat × Nat → Color
make-cell         : Char × Color × Color → Cell
make-canvas       : Nat × Nat → Canvas
canvas->string    : Canvas → String
```

## Migration Guide

### From layout.ss to layout-color.ss

```scheme
;; OLD (monochrome)
(load "shell/layout.ss")
(draw-string canvas pt "Text")

;; NEW (color-enabled)
(load "shell/layout-color.ss")
(draw-string canvas pt "Text")              ; Still works (default colors)
(draw-string-colored canvas pt "Text"
                    color-red color-default) ; Now with color!
```

No breaking changes — existing code works as-is.

## Mood Color Reference

| Mood    | Color         | RGB             | Use Case           |
|---------|---------------|-----------------|--------------------|
| happy   | Gold          | (255, 215, 0)   | Joy, celebration   |
| curious | Light Blue    | (100, 200, 255) | Wonder, questions  |
| sleepy  | Soft Purple   | (180, 180, 220) | Rest, drowsiness   |
| content | Soft Green    | (150, 220, 150) | Peace, satisfaction|
| lonely  | Muted Blue    | (150, 150, 200) | Sadness, longing   |
| playful | Pink          | (255, 150, 200) | Energy, fun        |

## Terminal Compatibility

**Works in:**
- iTerm2, Alacritty, Windows Terminal
- Modern Linux terminals (GNOME Terminal, Konsole)
- macOS Terminal.app
- tmux, screen (with 256-color support)

**May not work in:**
- Very old terminals
- Pure console (no ANSI)
- Redirected output (use `> file.txt` carefully)

## Debugging

```scheme
;; Check color type
(color-rgb? my-color)
(color-palette? my-color)
(color-default? my-color)

;; Inspect ANSI codes
(display (ansi-fg color-red))
;; Output: ^[[38;5;1m

;; Test in REPL
(display (ansi-fg color-red))
(display "RED TEXT")
(display ansi-reset)
```

---

**Load**: `(load "shell/layout-color.ss")`
**Docs**: See `COLOR-SYSTEM.md` for full documentation
**Demos**: `demo-color.ss`, `demo-duckie-color.ss`
**Tests**: `shell/test-color.ss`
