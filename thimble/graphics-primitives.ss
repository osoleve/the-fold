;;; shell/graphics-primitives.ss — Advanced Graphics Primitives
;;;
;;; High-level drawing operations for the DUCKIE canvas system.
;;; Integrates with the easing system for smooth gradients and effects.
;;;
;;; This is Shell code: pure functions for advanced rendering.
;;;
;;; Features:
;;;   - Line drawing with Bresenham's algorithm
;;;   - Rounded corner boxes
;;;   - Circle approximation using ASCII characters
;;;   - Gradient fills with easing-based interpolation
;;;   - Character intensity palettes for shading
;;;
;;; Dependencies:
;;;   - shell/layout.ss — Canvas primitives and data structures
;;;   - shell/easing.ss — Easing functions for smooth transitions

(library (shell graphics-primitives)
  (export
    ;; Line drawing
    draw-line
    draw-line-gradient

    ;; Enhanced box drawing
    draw-rounded-box

    ;; Circle drawing
    draw-circle
    draw-filled-circle

    ;; Gradient fills
    gradient-fill
    gradient-fill-horizontal
    gradient-fill-vertical
    gradient-fill-radial

    ;; Character intensity palettes
    intensity-palette-simple
    intensity-palette-blocks
    intensity-palette-dots
    get-intensity-char)

  (import (chezscheme)
          (shell layout)
          (shell easing))

  ;;; ============================================================
  ;;; Character Intensity Palettes
  ;;; ============================================================

  ;;; Character palettes for representing intensity/brightness levels.
  ;;; Characters ordered from darkest to brightest.

  ;;; Simple ASCII palette (10 levels)
  (define intensity-palette-simple
    '(#\space #\. #\: #\- #\= #\+ #\* #\# #\% #\@))

  ;;; Unicode block elements (5 levels)
  (define intensity-palette-blocks
    '(#\space #\░ #\▒ #\▓ #\█))

  ;;; Dot patterns (6 levels)
  (define intensity-palette-dots
    '(#\space #\· #\• #\⋅ #\● #\⬤))

  ;;; get-intensity-char : List Char × Real → Char
  ;;; Get character from palette based on intensity [0,1].
  ;;; 0 = darkest, 1 = brightest
  (define (get-intensity-char palette intensity)
    (let* ([clamped (max 0.0 (min 1.0 intensity))]
           [len (length palette)]
           [index (inexact->exact (floor (* clamped (- len 1))))]
           [safe-index (max 0 (min index (- len 1)))])
      (list-ref palette safe-index)))

  ;;; ============================================================
  ;;; Line Drawing
  ;;; ============================================================

  ;;; draw-line : Canvas × Point × Point × Char → Canvas
  ;;; Draw a line from start to end using Bresenham's algorithm.
  (define (draw-line canvas start end ch)
    (let ([x0 (point-x start)]
          [y0 (point-y start)]
          [x1 (point-x end)]
          [y1 (point-y end)])
      (let* ([dx (abs (- x1 x0))]
             [dy (abs (- y1 y0))]
             [sx (if (< x0 x1) 1 -1)]
             [sy (if (< y0 y1) 1 -1)]
             [err (- dx dy)])
        (let loop ([x x0] [y y0] [err err] [c canvas])
          (let ([c (canvas-set c x y ch)])
            (if (and (= x x1) (= y y1))
                c
                (let* ([e2 (* 2 err)]
                       [new-err (if (> e2 (- dy)) (- err dy) err)]
                       [new-x (if (> e2 (- dy)) (+ x sx) x)]
                       [new-err (if (< e2 dx) (+ new-err dx) new-err)]
                       [new-y (if (< e2 dx) (+ y sy) y)])
                  (loop new-x new-y new-err c))))))))

  ;;; draw-line-gradient : Canvas × Point × Point × List Char → Canvas
  ;;; Draw a line with a gradient effect using a character palette.
  ;;; The palette is applied from start to end.
  (define (draw-line-gradient canvas start end palette)
    (let ([x0 (point-x start)]
          [y0 (point-y start)]
          [x1 (point-x end)]
          [y1 (point-y end)])
      (let* ([dx (abs (- x1 x0))]
             [dy (abs (- y1 y0))]
             [sx (if (< x0 x1) 1 -1)]
             [sy (if (< y0 y1) 1 -1)]
             [err (- dx dy)]
             [total-length (sqrt (+ (* dx dx) (* dy dy)))])
        (let loop ([x x0] [y y0] [err err] [c canvas] [dist 0])
          (let* ([t (if (> total-length 0)
                        (/ dist total-length)
                        0)]
                 [ch (get-intensity-char palette t)]
                 [c (canvas-set c x y ch)])
            (if (and (= x x1) (= y y1))
                c
                (let* ([e2 (* 2 err)]
                       [new-err (if (> e2 (- dy)) (- err dy) err)]
                       [new-x (if (> e2 (- dy)) (+ x sx) x)]
                       [new-err (if (< e2 dx) (+ new-err dx) new-err)]
                       [new-y (if (< e2 dx) (+ y sy) y)]
                       [new-dist (+ dist 1)])
                  (loop new-x new-y new-err c new-dist))))))))

  ;;; ============================================================
  ;;; Rounded Box Drawing
  ;;; ============================================================

  ;;; draw-rounded-box : Canvas × Rect × Symbol → Canvas
  ;;; Draw a box with rounded corners.
  ;;; Style can be 'ascii, 'light, 'heavy, or 'double.
  (define (draw-rounded-box canvas rect style-name)
    (let* ([ox (point-x (rect-origin rect))]
           [oy (point-y (rect-origin rect))]
           [w (rect-width rect)]
           [h (rect-height rect)])
      (if (or (< w 3) (< h 3))
          ;; Too small for rounded corners, use regular box
          (draw-box canvas rect style-name)
          ;; Draw with rounded corners
          (let* ([right (+ ox w -1)]
                 [bottom (+ oy h -1)]
                 ;; Choose corner characters based on style
                 [corners (case style-name
                           [(ascii) '(#\/ #\\ #\\ #\/)]
                           [(light) '(#\╭ #\╮ #\╰ #\╯)]
                           [(heavy) '(#\┏ #\┓ #\┗ #\┛)]  ; Heavy doesn't have round
                           [(double) '(#\╔ #\╗ #\╚ #\╝)] ; Double doesn't have round
                           [else '(#\+ #\+ #\+ #\+)])]
                 [tl (list-ref corners 0)]
                 [tr (list-ref corners 1)]
                 [bl (list-ref corners 2)]
                 [br (list-ref corners 3)]
                 [hz (case style-name
                       [(ascii) #\-]
                       [(light) #\─]
                       [(heavy) #\━]
                       [(double) #\═]
                       [else #\-])]
                 [vt (case style-name
                       [(ascii) #\|]
                       [(light) #\│]
                       [(heavy) #\┃]
                       [(double) #\║]
                       [else #\|])])
            (let* ([canvas canvas]
                   ;; Draw corners
                   [canvas (canvas-set canvas ox oy tl)]
                   [canvas (canvas-set canvas right oy tr)]
                   [canvas (canvas-set canvas ox bottom bl)]
                   [canvas (canvas-set canvas right bottom br)]
                   ;; Draw horizontal edges
                   [canvas (let loop ([x (+ ox 1)] [c canvas])
                             (if (>= x right)
                                 c
                                 (loop (+ x 1)
                                      (canvas-set (canvas-set c x oy hz)
                                                 x bottom hz))))]
                   ;; Draw vertical edges
                   [canvas (let loop ([y (+ oy 1)] [c canvas])
                             (if (>= y bottom)
                                 c
                                 (loop (+ y 1)
                                      (canvas-set (canvas-set c ox y vt)
                                                 right y vt))))])
              canvas)))))

  ;;; ============================================================
  ;;; Circle Drawing
  ;;; ============================================================

  ;;; draw-circle : Canvas × Point × Nat × Char → Canvas
  ;;; Draw a circle outline using Bresenham's circle algorithm.
  ;;; center: center point of the circle
  ;;; radius: radius in character cells
  (define (draw-circle canvas center radius ch)
    (let ([cx (point-x center)]
          [cy (point-y center)])
      (let loop ([x 0]
                 [y radius]
                 [d (- 3 (* 2 radius))]
                 [c canvas])
        (if (> x y)
            c
            (let* ([c (canvas-set c (+ cx x) (+ cy y) ch)]
                   [c (canvas-set c (- cx x) (+ cy y) ch)]
                   [c (canvas-set c (+ cx x) (- cy y) ch)]
                   [c (canvas-set c (- cx x) (- cy y) ch)]
                   [c (canvas-set c (+ cx y) (+ cy x) ch)]
                   [c (canvas-set c (- cx y) (+ cy x) ch)]
                   [c (canvas-set c (+ cx y) (- cy x) ch)]
                   [c (canvas-set c (- cx y) (- cy x) ch)])
              (if (< d 0)
                  (loop (+ x 1) y (+ d (* 4 x) 6) c)
                  (loop (+ x 1) (- y 1) (+ d (* 4 (- x y)) 10) c)))))))

  ;;; draw-filled-circle : Canvas × Point × Nat × Char → Canvas
  ;;; Draw a filled circle.
  (define (draw-filled-circle canvas center radius ch)
    (let ([cx (point-x center)]
          [cy (point-y center)]
          [r2 (* radius radius)])
      (let loop-y ([y (- radius)] [c canvas])
        (if (> y radius)
            c
            (let loop-x ([x (- radius)] [c c])
              (if (> x radius)
                  (loop-y (+ y 1) c)
                  (let ([dist2 (+ (* x x) (* y y))])
                    (if (<= dist2 r2)
                        (loop-x (+ x 1)
                               (canvas-set c (+ cx x) (+ cy y) ch))
                        (loop-x (+ x 1) c)))))))))

  ;;; ============================================================
  ;;; Gradient Fills
  ;;; ============================================================

  ;;; gradient-fill : Canvas × Rect × Real × Real × (Real → Real) × List Char → Canvas
  ;;; General gradient fill with custom start/end values and easing function.
  ;;;
  ;;; Arguments:
  ;;;   canvas     — Canvas to draw on
  ;;;   rect       — Rectangle to fill
  ;;;   start      — Start intensity [0,1]
  ;;;   end        — End intensity [0,1]
  ;;;   ease-fn    — Easing function for smooth transitions
  ;;;   palette    — Character palette for intensity levels
  ;;;   direction  — 'horizontal, 'vertical, or custom gradient function
  (define gradient-fill
    (case-lambda
      [(canvas rect start-intensity end-intensity ease-fn palette direction)
       (case direction
         [(horizontal) (gradient-fill-horizontal canvas rect start-intensity end-intensity ease-fn palette)]
         [(vertical) (gradient-fill-vertical canvas rect start-intensity end-intensity ease-fn palette)]
         [else canvas])]
      [(canvas rect ease-fn palette direction)
       (gradient-fill canvas rect 0.0 1.0 ease-fn palette direction)]))

  ;;; gradient-fill-horizontal : Canvas × Rect × Real × Real × (Real → Real) × List Char → Canvas
  ;;; Fill rectangle with horizontal gradient (left to right).
  (define (gradient-fill-horizontal canvas rect start-intensity end-intensity ease-fn palette)
    (let ([ox (point-x (rect-origin rect))]
          [oy (point-y (rect-origin rect))]
          [w (rect-width rect)]
          [h (rect-height rect)])
      (if (or (<= w 0) (<= h 0))
          canvas
          (let loop-y ([y oy] [c canvas])
            (if (>= y (+ oy h))
                c
                (let loop-x ([x ox] [c c])
                  (if (>= x (+ ox w))
                      (loop-y (+ y 1) c)
                      (let* ([t (if (> w 1)
                                   (/ (- x ox) (- w 1))
                                   0.5)]
                             [eased-t (ease-fn t)]
                             [intensity (interpolate start-intensity end-intensity eased-t ease-linear)]
                             [ch (get-intensity-char palette intensity)])
                        (loop-x (+ x 1)
                               (canvas-set c x y ch))))))))))

  ;;; gradient-fill-vertical : Canvas × Rect × Real × Real × (Real → Real) × List Char → Canvas
  ;;; Fill rectangle with vertical gradient (top to bottom).
  (define (gradient-fill-vertical canvas rect start-intensity end-intensity ease-fn palette)
    (let ([ox (point-x (rect-origin rect))]
          [oy (point-y (rect-origin rect))]
          [w (rect-width rect)]
          [h (rect-height rect)])
      (if (or (<= w 0) (<= h 0))
          canvas
          (let loop-y ([y oy] [c canvas])
            (if (>= y (+ oy h))
                c
                (let* ([t (if (> h 1)
                             (/ (- y oy) (- h 1))
                             0.5)]
                       [eased-t (ease-fn t)]
                       [intensity (interpolate start-intensity end-intensity eased-t ease-linear)])
                  (let loop-x ([x ox] [c c])
                    (if (>= x (+ ox w))
                        (loop-y (+ y 1) c)
                        (let ([ch (get-intensity-char palette intensity)])
                          (loop-x (+ x 1)
                                 (canvas-set c x y ch)))))))))))

  ;;; gradient-fill-radial : Canvas × Point × Nat × List Char × (Real → Real) → Canvas
  ;;; Fill area with radial gradient from center point.
  ;;; Intensity decreases from center (1.0) to radius (0.0).
  (define (gradient-fill-radial canvas center max-radius palette ease-fn)
    (let ([cx (point-x center)]
          [cy (point-y center)]
          [w (canvas-width canvas)]
          [h (canvas-height canvas)])
      (let loop-y ([y 0] [c canvas])
        (if (>= y h)
            c
            (let loop-x ([x 0] [c c])
              (if (>= x w)
                  (loop-y (+ y 1) c)
                  (let* ([dx (- x cx)]
                         [dy (- y cy)]
                         [dist (sqrt (+ (* dx dx) (* dy dy)))]
                         [t (if (> max-radius 0)
                               (min 1.0 (/ dist max-radius))
                               0.0)]
                         [eased-t (ease-fn t)]
                         ;; Invert so center is brightest
                         [intensity (- 1.0 eased-t)]
                         [ch (get-intensity-char palette intensity)])
                    (loop-x (+ x 1)
                           (canvas-set c x y ch)))))))))

  ) ; end library
