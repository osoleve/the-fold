# Color System Integration Guide

## Integrating Colors into Existing Code

This guide shows how to update existing Fold code to use the color system.

## Example: Updating duckie-loop.ss

### Step 1: Change the Layout Import

**Before:**
```scheme
(load "shell/layout.ss")
```

**After:**
```scheme
(load "shell/layout-color.ss")  ; Color-enabled canvas
```

### Step 2: Update the Render Function

**Before (Monochrome):**
```scheme
(define (render-duckie state)
  (let* ([duckie (loop-state-duckie state)]
         [canvas (make-canvas 60 20)]
         [loc (duckie-location duckie)])

    ;; Draw box border
    (let ([canvas (draw-box canvas
                            (make-rect (point 0 0) 60 20)
                            'light)])
      ;; Draw DUCKIE sprite
      (let ([canvas (draw-string canvas (point x y) "__(o)>")])
        canvas))))
```

**After (With Colors):**
```scheme
(define (render-duckie state)
  (let* ([duckie (loop-state-duckie state)]
         [mood (duckie-mood duckie)]
         [energy (duckie-energy duckie)]
         [mood-color (mood->color mood)]
         [energy-color (energy->color energy)]
         [canvas (make-canvas 60 20)]
         [loc (duckie-location duckie)])

    ;; Draw colored box border
    (let ([canvas (draw-box-colored canvas
                                   (make-rect (point 0 0) 60 20)
                                   'light
                                   mood-color           ; Border in mood color
                                   color-default)])

      ;; Draw DUCKIE sprite in mood color
      (let ([canvas (draw-string-colored canvas (point x y) "__(o)>"
                                        mood-color
                                        color-default)])

        ;; Draw energy bar with gradient
        (let ([canvas (draw-energy-bar canvas energy)])
          canvas)))))
```

### Step 3: Add Helper Functions

Add these helper functions to `duckie-loop.ss`:

```scheme
;;; draw-energy-bar : Canvas × Nat → Canvas
;;; Draw an energy bar with color gradient (0-100).
(define (draw-energy-bar canvas energy)
  (let* ([bar-width 30]
         [bar-x 15]
         [bar-y 17]
         [filled (quotient (* energy bar-width) 100)])

    ;; Draw bar background
    (let ([canvas (draw-box canvas
                           (make-rect (point (- bar-x 1) (- bar-y 1))
                                     (+ bar-width 2) 3)
                           'light)])

      ;; Draw filled portion with color gradient
      (let loop ([i 0] [c canvas])
        (if (>= i filled)
            c
            (let* ([e (quotient (* i 100) bar-width)]
                   [color (energy->color e)])
              (loop (+ i 1)
                    (draw-char-colored c (point (+ bar-x i) bar-y)
                                     #\█ color color))))))))

;;; draw-sprite-colored : Canvas × Point × List[String] × Color → Canvas
;;; Draw a multi-line sprite in a given color.
(define (draw-sprite-colored canvas pt sprite-lines fg-color)
  (let loop ([lines sprite-lines]
             [row 0]
             [c canvas])
    (if (null? lines)
        c
        (loop (cdr lines)
              (+ row 1)
              (draw-string-colored c
                                  (point (point-x pt)
                                         (+ (point-y pt) row))
                                  (car lines)
                                  fg-color
                                  color-default)))))
```

### Step 4: Update Sprite Rendering

**Before:**
```scheme
;; Draw duck body
[canvas (draw-string canvas (point x duck-y) "   __(o)>")]
[canvas (draw-string canvas (point x (+ duck-y 1)) "  \\___)  ")]
```

**After:**
```scheme
;; Define sprite as list of lines
(define duckie-sprite
  '("   __(o)>"
    "  \\___)  "
    "   \\  / "
    "    \\/  "))

;; Draw sprite in mood color
[canvas (draw-sprite-colored canvas
                            (point x duck-y)
                            duckie-sprite
                            mood-color)]
```

### Step 5: Add Mood-Based Details

Enhance the rendering with mood-specific colors:

```scheme
;; Different border colors for different moods
(define (get-border-style mood)
  (case mood
    [(happy playful) 'double]   ; Energetic moods get double border
    [(sleepy lonely) 'light]    ; Quiet moods get light border
    [else 'light]))

;; Mood-specific background effects
(define (draw-mood-background canvas mood)
  (case mood
    [(sleepy)
     ;; Add "Zzz" for sleepy mood
     (draw-string-colored canvas (point 50 3) "Zzz..."
                         (darken (mood->color mood) 0.6)
                         color-default)]
    [(happy)
     ;; Add sparkles for happy mood
     (draw-string-colored canvas (point 5 3) "✨"
                         color-bright-yellow
                         color-default)]
    [else canvas]))
```

## Example: Adding Color to Custom Widgets

### Colored Progress Bar

```scheme
(define (draw-progress-bar canvas origin label value max-value)
  (let* ([width 30]
         [filled (quotient (* value width) max-value)]
         [x (point-x origin)]
         [y (point-y origin)]
         ;; Color based on completion (red → yellow → green)
         [progress-color (cond
                          [(< value (quotient max-value 3))
                           color-red]
                          [(< value (quotient (* 2 max-value) 3))
                           color-yellow]
                          [else color-green])])

    ;; Draw label
    (let ([canvas (draw-string canvas (point x y) label)])

      ;; Draw bar outline
      (let ([canvas (draw-box canvas
                             (make-rect (point (+ x (string-length label) 1) y)
                                       (+ width 2) 3)
                             'light)])

        ;; Fill bar
        (let loop ([i 0] [c canvas])
          (if (>= i filled)
              c
              (loop (+ i 1)
                    (draw-char-colored c
                                     (point (+ x (string-length label) 2 i) (+ y 1))
                                     #\█
                                     progress-color
                                     color-default))))))))
```

### Colored Menu

```scheme
(define (draw-menu canvas origin items selected-index)
  (let loop ([items items]
             [index 0]
             [c canvas]
             [y (point-y origin)])
    (if (null? items)
        c
        (let* ([item (car items)]
               [selected? (= index selected-index)]
               [fg-color (if selected? color-bright-white color-white)]
               [bg-color (if selected? color-blue color-default)]
               [prefix (if selected? "> " "  ")])
          (loop (cdr items)
                (+ index 1)
                (draw-string-colored c
                                    (point (point-x origin) y)
                                    (string-append prefix item)
                                    fg-color
                                    bg-color)
                (+ y 1))))))
```

### Status Indicators

```scheme
(define (draw-status canvas origin label status)
  (let* ([x (point-x origin)]
         [y (point-y origin)]
         [status-color (case status
                        [(success ok) color-green]
                        [(warning) color-yellow]
                        [(error danger) color-red]
                        [(info) color-blue]
                        [else color-default])]
         [status-text (symbol->string status)])

    ;; Draw label
    (let ([canvas (draw-string canvas (point x y) label)])

      ;; Draw status in color
      (draw-string-colored canvas
                          (point (+ x (string-length label) 2) y)
                          (string-append "[" status-text "]")
                          status-color
                          color-default))))
```

## Testing Your Integration

### 1. Verify Colors Work

```scheme
;; Simple color test
(load "shell/layout-color.ss")

(define test-canvas (make-canvas 30 5))
(define test-canvas (draw-string-colored test-canvas (point 5 2)
                                        "Color Test"
                                        color-bright-red
                                        color-yellow))
(display (canvas->string test-canvas))
```

### 2. Test Mood Colors

```scheme
;; Mood color test
(define moods '(happy curious sleepy content lonely playful))

(for-each
  (lambda (mood)
    (display (string-append (symbol->string mood) ": "))
    (let ([c (make-canvas 10 1)])
      (display (canvas->string
                 (fill-rect-colored c
                                   (make-rect (point 0 0) 10 1)
                                   #\█
                                   (mood->color mood)
                                   (mood->color mood))))
      (newline)))
  moods)
```

### 3. Test Energy Gradient

```scheme
;; Energy gradient test
(let ([c (make-canvas 50 3)])
  (let loop ([e 0] [canvas c])
    (if (> e 100)
        (display (canvas->string canvas))
        (let ([x (quotient (* e 40) 100)])
          (loop (+ e 5)
                (draw-char-colored canvas (point x 1) #\█
                                 (energy->color e)
                                 (energy->color e)))))))
```

## Backward Compatibility

The color system is **fully backward compatible**:

```scheme
;; This code works with BOTH layout.ss AND layout-color.ss
(define c (make-canvas 20 10))
(define c (draw-string c (point 5 5) "Hello"))
(display (canvas->string c))
```

When using `layout-color.ss`, uncolored functions use default colors.

## Performance Considerations

### ANSI Code Optimization

The `canvas->string` function optimizes ANSI output:

```scheme
;; Only emits codes when colors change
"Text"                  ; No codes (default)
"\x1B[31mRed\x1B[39m"   ; Change to red, reset to default
```

### Canvas Size

Larger canvases use more memory (cells are ~24 bytes each):
- 60×20 canvas ≈ 28 KB
- 100×50 canvas ≈ 120 KB

Still efficient for terminal graphics.

## Troubleshooting

### Colors Don't Appear

1. Check terminal supports ANSI colors:
   ```bash
   echo -e "\x1B[31mRed\x1B[0m"
   ```

2. Verify you loaded `layout-color.ss` not `layout.ss`

3. Check you're using colored functions:
   ```scheme
   draw-string-colored  ; ✓ Has color
   draw-string          ; Uses defaults
   ```

### Wrong Colors

1. Verify color type:
   ```scheme
   (color-rgb? my-color)
   (color-palette? my-color)
   ```

2. Check RGB values are 0-255:
   ```scheme
   (rgb 255 128 64)  ; ✓ Valid
   (rgb 300 -10 64)  ; ✗ Out of range
   ```

## Next Steps

1. Update `shell/duckie-loop.ss` with mood colors
2. Create colored sprite sheets
3. Add animated color transitions
4. Implement color themes (dark mode, light mode)
5. Add text styling (bold, italic, underline)

---

**See Also:**
- `COLOR-SYSTEM.md` — Full documentation
- `COLOR-API-REFERENCE.md` — Quick reference
- `demo-duckie-color.ss` — Example implementation
