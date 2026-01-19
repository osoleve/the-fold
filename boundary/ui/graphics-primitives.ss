;;; boundary/graphics-primitives.ss — Advanced Graphics Primitives
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
;;;   - boundary/ui/layout.ss — Canvas primitives and data structures
;;;   - boundary/ui/easing.ss — Easing functions for smooth transitions

(library (shell graphics-primitives)
         (export
          ;; Line drawing
          draw-line
          draw-line-gradient
          
          ;; Polyline and polygon
          draw-polyline
          draw-polygon
          draw-filled-polygon
          draw-filled-polygon-aet
          
          ;; Path primitives
          draw-path
          
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
          get-intensity-char
          
          ;; Pattern fills
          make-pattern
          pattern-fill
          pattern-fill-circle
          pattern-ref
          pattern-width
          pattern-height
          pattern-checker
          pattern-dots
          pattern-diagonal
          pattern-cross
          pattern-hatch
          
          ;; Styling system
          make-style
          style-fill
          style-stroke
          style-opacity
          draw-with-style
          
          ;; Alpha blending
          apply-opacity-block)
         
         (import (chezscheme)
                 (shell layout)
                 (shell easing))
         
         ;;; ====
         ;;; Character Intensity Palettes
         ;;; ====
         
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
         
         ;;; ====
         ;;; Line Drawing
         ;;; ====
         
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
                      (let loop ([x x0] [y y0] [err err])
                           (canvas-set! canvas x y ch)
                           (if (and (= x x1) (= y y1))
                               canvas
                               (let* ([e2 (* 2 err)]
                                      [new-err (if (> e2 (- dy)) (- err dy) err)]
                                      [new-x (if (> e2 (- dy)) (+ x sx) x)]
                                      [new-err (if (< e2 dx) (+ new-err dx) new-err)]
                                      [new-y (if (< e2 dx) (+ y sy) y)])
                                     (loop new-x new-y new-err))))))))

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
                      (let loop ([x x0] [y y0] [err err] [dist 0])
                           (let* ([t (if (> total-length 0)
                                         (/ dist total-length)
                                         0)]
                                  [ch (get-intensity-char palette t)])
                                 (canvas-set! canvas x y ch)
                                 (if (and (= x x1) (= y y1))
                                     canvas
                                     (let* ([e2 (* 2 err)]
                                            [new-err (if (> e2 (- dy)) (- err dy) err)]
                                            [new-x (if (> e2 (- dy)) (+ x sx) x)]
                                            [new-err (if (< e2 dx) (+ new-err dx) new-err)]
                                            [new-y (if (< e2 dx) (+ y sy) y)]
                                            [new-dist (+ dist 1)])
                                           (loop new-x new-y new-err new-dist))))))))

         ;;; ====
         ;;; Rounded Box Drawing
         ;;; ====
         
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
                           ;; Draw corners
                           (canvas-set! canvas ox oy tl)
                           (canvas-set! canvas right oy tr)
                           (canvas-set! canvas ox bottom bl)
                           (canvas-set! canvas right bottom br)
                           ;; Draw horizontal edges
                           (let loop ([x (+ ox 1)])
                                (when (< x right)
                                      (canvas-set! canvas x oy hz)
                                      (canvas-set! canvas x bottom hz)
                                      (loop (+ x 1))))
                           ;; Draw vertical edges
                           (let loop ([y (+ oy 1)])
                                (when (< y bottom)
                                      (canvas-set! canvas ox y vt)
                                      (canvas-set! canvas right y vt)
                                      (loop (+ y 1))))
                           canvas)))))

         ;;; ====
         ;;; Circle Drawing
         ;;; ====
         
         ;;; draw-circle : Canvas × Point × Nat × Char → Canvas
         ;;; Draw a circle outline using Bresenham's circle algorithm.
         ;;; center: center point of the circle
         ;;; radius: radius in character cells
         (define (draw-circle canvas center radius ch)
           (let ([cx (point-x center)]
                 [cy (point-y center)])
                (let loop ([x 0]
                           [y radius]
                           [d (- 3 (* 2 radius))])
                     (if (> x y)
                         canvas
                         (begin
                          (canvas-set! canvas (+ cx x) (+ cy y) ch)
                          (canvas-set! canvas (- cx x) (+ cy y) ch)
                          (canvas-set! canvas (+ cx x) (- cy y) ch)
                          (canvas-set! canvas (- cx x) (- cy y) ch)
                          (canvas-set! canvas (+ cx y) (+ cy x) ch)
                          (canvas-set! canvas (- cx y) (+ cy x) ch)
                          (canvas-set! canvas (+ cx y) (- cy x) ch)
                          (canvas-set! canvas (- cx y) (- cy x) ch)
                          (if (< d 0)
                              (loop (+ x 1) y (+ d (* 4 x) 6))
                              (loop (+ x 1) (- y 1) (+ d (* 4 (- x y)) 10)))))))))

         ;;; draw-filled-circle : Canvas × Point × Nat × Char → Canvas
         ;;; Draw a filled circle.
         (define (draw-filled-circle canvas center radius ch)
           (let ([cx (point-x center)]
                 [cy (point-y center)]
                 [r2 (* radius radius)])
                (let loop-y ([y (- radius)])
                     (if (> y radius)
                         canvas
                         (let loop-x ([x (- radius)])
                              (if (> x radius)
                                  (loop-y (+ y 1))
                                  (let ([dist2 (+ (* x x) (* y y))])
                                       (when (<= dist2 r2)
                                             (canvas-set! canvas (+ cx x) (+ cy y) ch))
                                       (loop-x (+ x 1)))))))))

         ;;; ====
         ;;; Gradient Fills
         ;;; ====
         
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
                    (let loop-y ([y oy])
                         (if (>= y (+ oy h))
                             canvas
                             (let loop-x ([x ox])
                                  (if (>= x (+ ox w))
                                      (loop-y (+ y 1))
                                      (let* ([t (if (> w 1)
                                                    (/ (- x ox) (- w 1))
                                                    0.5)]
                                             [eased-t (ease-fn t)]
                                             [intensity (interpolate start-intensity end-intensity eased-t ease-linear)]
                                             [ch (get-intensity-char palette intensity)])
                                            (canvas-set! canvas x y ch)
                                            (loop-x (+ x 1))))))))))

         ;;; gradient-fill-vertical : Canvas × Rect × Real × Real × (Real → Real) × List Char → Canvas
         ;;; Fill rectangle with vertical gradient (top to bottom).
         (define (gradient-fill-vertical canvas rect start-intensity end-intensity ease-fn palette)
           (let ([ox (point-x (rect-origin rect))]
                 [oy (point-y (rect-origin rect))]
                 [w (rect-width rect)]
                 [h (rect-height rect)])
                (if (or (<= w 0) (<= h 0))
                    canvas
                    (let loop-y ([y oy])
                         (if (>= y (+ oy h))
                             canvas
                             (let* ([t (if (> h 1)
                                           (/ (- y oy) (- h 1))
                                           0.5)]
                                    [eased-t (ease-fn t)]
                                    [intensity (interpolate start-intensity end-intensity eased-t ease-linear)])
                                   (let loop-x ([x ox])
                                        (if (>= x (+ ox w))
                                            (loop-y (+ y 1))
                                            (let ([ch (get-intensity-char palette intensity)])
                                                 (canvas-set! canvas x y ch)
                                                 (loop-x (+ x 1)))))))))))

         ;;; gradient-fill-radial : Canvas × Point × Nat × List Char × (Real → Real) → Canvas
         ;;; Fill area with radial gradient from center point.
         ;;; Intensity decreases from center (1.0) to radius (0.0).
         (define (gradient-fill-radial canvas center max-radius palette ease-fn)
           (let ([cx (point-x center)]
                 [cy (point-y center)]
                 [w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (let loop-y ([y 0])
                     (if (>= y h)
                         canvas
                         (let loop-x ([x 0])
                              (if (>= x w)
                                  (loop-y (+ y 1))
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
                                        (canvas-set! canvas x y ch)
                                        (loop-x (+ x 1)))))))))

         ;;; ====
         ;;; Pattern Fills
         ;;; ====
         
         ;;; Built-in patterns (2x2 tiles)
         (define pattern-checker '((#\█ #\space) (#\space #\█)))
         (define pattern-dots '((#\· #\space) (#\space #\·)))
         (define pattern-diagonal '((#\/ #\space) (#\space #\/)))
         (define pattern-cross '((#\+ #\space) (#\space #\+)))
         (define pattern-hatch '((#\# #\-) (#\| #\#)))

         ;;; make-pattern : (List (List Char)) → Pattern
         ;;; Create a pattern from a 2D list of characters.
         ;;; Pattern is a list of rows, each row is a list of chars.
         (define (make-pattern rows)
           rows)

         ;;; pattern-width : Pattern → Nat
         (define (pattern-width pattern)
           (if (null? pattern)
               0
               (length (car pattern))))

         ;;; pattern-height : Pattern → Nat
         (define (pattern-height pattern)
           (length pattern))

         ;;; pattern-ref : Pattern × Nat × Nat → Char
         ;;; Get character at (x, y) in pattern, with wrapping.
         ;;; Returns space for empty patterns.
         (define (pattern-ref pattern x y)
           (let* ([h (pattern-height pattern)]
                  [w (pattern-width pattern)])
                 (if (or (<= h 0) (<= w 0))
                     #\space
                     (let ([row (list-ref pattern (modulo y h))])
                          (list-ref row (modulo x w))))))

         ;;; pattern-fill : Canvas × Rect × Pattern → Canvas
         ;;; Fill a rectangular region with a tiled pattern.
         (define (pattern-fill canvas rect pattern)
           (let ([ox (point-x (rect-origin rect))]
                 [oy (point-y (rect-origin rect))]
                 [w (rect-width rect)]
                 [h (rect-height rect)])
                (let loop-y ([y 0])
                     (if (>= y h)
                         canvas
                         (let loop-x ([x 0])
                              (if (>= x w)
                                  (loop-y (+ y 1))
                                  (let ([ch (pattern-ref pattern x y)])
                                       (unless (char=? ch #\space)
                                               (canvas-set! canvas (+ ox x) (+ oy y) ch))
                                       (loop-x (+ x 1)))))))))

         ;;; pattern-fill-circle : Canvas × Point × Nat × Pattern → Canvas
         ;;; Fill a circular region with a tiled pattern.
         ;;; Pattern coordinates are local to the circle's bounding box for
         ;;; consistent alignment with pattern-fill.
         (define (pattern-fill-circle canvas center radius pattern)
           (let ([cx (point-x center)]
                 [cy (point-y center)]
                 [r2 (* radius radius)])
                (let loop-y ([y (- radius)])
                     (if (> y radius)
                         canvas
                         (let loop-x ([x (- radius)])
                              (if (> x radius)
                                  (loop-y (+ y 1))
                                  (let ([dist2 (+ (* x x) (* y y))])
                                       (when (<= dist2 r2)
                                             ;; Use local coords (offset from top-left of bounding box)
                                             (let ([ch (pattern-ref pattern (+ x radius) (+ y radius))])
                                                  (unless (char=? ch #\space)
                                                          (canvas-set! canvas (+ cx x) (+ cy y) ch))))
                                       (loop-x (+ x 1)))))))))

         ;;; ====
         ;;; Styling System
         ;;; ====
         
         ;;; Style record for stroke/fill/opacity configuration
         (define-record-type style%
           (fields fill stroke opacity))

         ;;; make-style : Char × Char × Real → Style
         ;;; Create a style with fill character, stroke character, and opacity [0,1].
         (define make-style make-style%)

         (define style-fill style%-fill)
         (define style-stroke style%-stroke)
         (define style-opacity style%-opacity)

         ;;; apply-opacity : Char × Real → Char
         ;;; Apply opacity to a character using smooth alpha blending.
         ;;; Maps opacity [0,1] to intensity-palette-blocks for gradual fade.
         ;;;
         ;;; For special characters (ASCII art), high opacity (>0.75) preserves
         ;;; the original character. Lower opacities use block shading.
         (define (apply-opacity ch opacity)
           (let ([clamped (max 0.0 (min 1.0 opacity))])
                (cond
                 ;; Fully transparent
                 [(< clamped 0.1) #\space]
                 ;; High opacity: preserve original character
                 [(> clamped 0.75) ch]
                 ;; Medium-high: use dark shade or original for blocks
                 [(> clamped 0.5)
                  (if (char=? ch #\█) #\▓ ch)]
                 ;; Medium-low: use medium shade
                 [(> clamped 0.25) #\▒]
                 ;; Low: use light shade
                 [else #\░])))

         ;;; apply-opacity-block : Real → Char
         ;;; Get a block character based on opacity alone.
         ;;; Useful for creating opacity-based gradients and effects.
         (define (apply-opacity-block opacity)
           (get-intensity-char intensity-palette-blocks opacity))

         ;;; ====
         ;;; Polyline Drawing
         ;;; ====
         
         ;;; draw-polyline : Canvas × (List Point) × Char → Canvas
         ;;; Draw a series of connected line segments.
         ;;; Points list must have at least 2 points.
         (define (draw-polyline canvas points ch)
           (if (or (null? points) (null? (cdr points)))
               canvas
               (let loop ([pts points] [c canvas])
                    (if (null? (cdr pts))
                        c
                        (loop (cdr pts)
                              (draw-line c (car pts) (cadr pts) ch))))))

         ;;; ====
         ;;; Polygon Drawing
         ;;; ====
         
         ;;; draw-polygon : Canvas × (List Point) × Char → Canvas
         ;;; Draw a closed polygon (polyline with final edge back to start).
         ;;; Points list must have at least 3 points.
         (define (draw-polygon canvas points ch)
           (if (or (null? points) (null? (cdr points)) (null? (cddr points)))
               canvas
               (let* ([c (draw-polyline canvas points ch)])
                     ;; Close the polygon by connecting last point to first
                     (draw-line c (car (reverse points)) (car points) ch))))

         ;;; point< : Point × Point → Boolean
         ;;; Compare points for scanline sorting (by y, then x).
         (define (point< p1 p2)
           (let ([y1 (point-y p1)]
                 [y2 (point-y p2)]
                 [x1 (point-x p1)]
                 [x2 (point-x p2)])
                (or (< y1 y2)
                    (and (= y1 y2) (< x1 x2)))))

         ;;; draw-filled-polygon : Canvas × (List Point) × Char → Canvas
         ;;; Draw a filled polygon using Active Edge Table scanline algorithm.
         ;;; Points must be ordered (clockwise or counterclockwise).
         ;;; Uses optimized AET algorithm: O(Height + Edges) vs O(Height × Edges).
         (define (draw-filled-polygon canvas points ch)
           ;; Delegate to AET implementation
           (draw-filled-polygon-aet canvas points ch))

         ;;; draw-filled-polygon-naive : Canvas × (List Point) × Char → Canvas
         ;;; Simple scanline fill algorithm - O(Height × Edges).
         ;;; Kept for reference; use draw-filled-polygon-aet for better performance.
         (define (draw-filled-polygon-naive canvas points ch)
           (if (< (length points) 3)
               canvas
               (let* ([;; First draw the outline
                       c (draw-polygon canvas points ch)]
                      [;; Get bounding box
                       xs (map point-x points)]
                      [ys (map point-y points)]
                      [min-y (apply min ys)]
                      [max-y (apply max ys)])
                     ;; Scanline fill
                     (let loop-y ([y min-y] [c c])
                          (if (> y max-y)
                              c
                              ;; Find intersections with polygon edges at this y
                              (let ([intersections (find-intersections points y)])
                                   (if (< (length intersections) 2)
                                       (loop-y (+ y 1) c)
                                       ;; Fill between pairs of intersections
                                       (let ([sorted (list-sort < intersections)])
                                            (loop-y (+ y 1)
                                                    (fill-scanline c y sorted ch))))))))))

         ;;; find-intersections : (List Point) × Nat → (List Nat)
         ;;; Find x-coordinates where horizontal line at y intersects polygon edges.
         (define (find-intersections points y)
           (let loop ([pts points]
                      [first-pt (car points)]
                      [result '()])
                (if (null? (cdr pts))
                    ;; Handle wrap-around edge from last to first point
                    (let ([intersect (edge-intersection (car pts) first-pt y)])
                         (if intersect
                             (cons intersect result)
                             result))
                    (let* ([p1 (car pts)]
                           [p2 (cadr pts)]
                           [intersect (edge-intersection p1 p2 y)])
                          (loop (cdr pts)
                                first-pt
                                (if intersect
                                    (cons intersect result)
                                    result))))))

         ;;; edge-intersection : Point × Point × Nat → Nat | #f
         ;;; Find x-coordinate where edge (p1->p2) intersects horizontal line at y.
         ;;; Returns #f if no intersection.
         (define (edge-intersection p1 p2 y)
           (let ([y1 (point-y p1)]
                 [y2 (point-y p2)]
                 [x1 (point-x p1)]
                 [x2 (point-x p2)])
                ;; Check if y is within edge's y range
                (if (or (and (< y y1) (< y y2))
                        (and (> y y1) (> y y2)))
                    #f
                    ;; Compute intersection x-coordinate
                    (if (= y1 y2)
                        ;; Horizontal edge - return start x
                        x1
                        ;; Interpolate x based on y
                        (let* ([t (/ (- y y1) (- y2 y1))]
                               [x (+ x1 (* t (- x2 x1)))])
                              (inexact->exact (round x)))))))

         ;;; fill-scanline : Canvas × Nat × (List Nat) × Char → Canvas
         ;;; Fill horizontal scanline between pairs of x-coordinates.
         (define (fill-scanline canvas y xs ch)
           (let loop ([coords xs])
                (if (or (null? coords) (null? (cdr coords)))
                    canvas
                    (let ([x1 (car coords)]
                          [x2 (cadr coords)])
                         ;; Fill from x1 to x2
                         (let fill-x ([x x1])
                              (if (> x x2)
                                  (loop (cddr coords))
                                  (begin
                                   (canvas-set! canvas x y ch)
                                   (fill-x (+ x 1)))))))))

         ;;; ====
         ;;; Active Edge Table (AET) Polygon Fill - Optimized Version
         ;;; ====
         ;;;
         ;;; For complex polygons, the AET algorithm is more efficient:
         ;;; O(Height + Edges + Intersections) vs O(Height × Edges).
         ;;;
         ;;; Edge entry: (y-min y-max x-current inverse-slope)
         ;;; The edge table is pre-sorted by y-min.
         
         ;;; make-edge-entry : Point × Point → EdgeEntry | #f
         ;;; Create an edge entry for polygon filling.
         ;;; Returns #f for horizontal edges (they don't need scanline processing).
         (define (make-edge-entry p1 p2)
           (let ([y1 (point-y p1)]
                 [y2 (point-y p2)]
                 [x1 (point-x p1)]
                 [x2 (point-x p2)])
                (cond
                 ;; Skip horizontal edges
                 [(= y1 y2) #f]
                 ;; p1 is higher (smaller y)
                 [(< y1 y2)
                  (list y1 y2 x1 (/ (- x2 x1) (- y2 y1)))]
                 ;; p2 is higher
                 [else
                  (list y2 y1 x2 (/ (- x1 x2) (- y1 y2)))])))

         ;;; Edge entry accessors
         (define (edge-y-min e) (car e))
         (define (edge-y-max e) (cadr e))
         (define (edge-x e) (caddr e))
         (define (edge-inverse-slope e) (cadddr e))

         ;;; build-edge-table : (List Point) → (List EdgeEntry)
         ;;; Build sorted edge table from polygon points.
         (define (build-edge-table points)
           (let loop ([pts points]
                      [first-pt (car points)]
                      [result '()])
                (if (null? (cdr pts))
                    ;; Handle wrap-around edge
                    (let ([entry (make-edge-entry (car pts) first-pt)])
                         (list-sort (lambda (a b) (< (edge-y-min a) (edge-y-min b)))
                                    (if entry (cons entry result) result)))
                    (let ([entry (make-edge-entry (car pts) (cadr pts))])
                         (loop (cdr pts)
                               first-pt
                               (if entry (cons entry result) result))))))

         ;;; update-active-edges : (List EdgeEntry) × Nat → (List EdgeEntry)
         ;;; Remove edges whose y-max is at or below current y.
         (define (update-active-edges active y)
           (filter (lambda (e) (> (edge-y-max e) y)) active))

         ;;; advance-edge-x : EdgeEntry → EdgeEntry
         ;;; Move edge x-coordinate to next scanline.
         (define (advance-edge-x e)
           (list (edge-y-min e)
                 (edge-y-max e)
                 (+ (edge-x e) (edge-inverse-slope e))
                 (edge-inverse-slope e)))

         ;;; get-x-intersections : (List EdgeEntry) → (List Nat)
         ;;; Get sorted x-coordinates from active edges.
         (define (get-x-intersections active)
           (list-sort < (map (lambda (e) (inexact->exact (round (edge-x e)))) active)))

         ;;; draw-filled-polygon-aet : Canvas × (List Point) × Char → Canvas
         ;;; Optimized filled polygon using Active Edge Table algorithm.
         (define (draw-filled-polygon-aet canvas points ch)
           (if (< (length points) 3)
               canvas
               (let* ([;; First draw the outline
                       c (draw-polygon canvas points ch)]
                      [;; Build sorted edge table
                       edge-table (build-edge-table points)]
                      [;; Get y-range
                       ys (map point-y points)]
                      [min-y (apply min ys)]
                      [max-y (apply max ys)])
                     ;; Scanline fill with active edge tracking
                     (let loop-y ([y min-y]
                                  [edges edge-table]
                                  [active '()]
                                  [c c])
                          (if (> y max-y)
                              c
                              ;; Add edges that start at this y
                              (let add-loop ([remaining edges]
                                             [new-active active]
                                             [unused '()])
                                   (cond
                                    [(null? remaining)
                                     ;; Process this scanline
                                     (let* ([active-now (update-active-edges new-active y)]
                                            [xs (get-x-intersections active-now)])
                                           (if (< (length xs) 2)
                                               ;; Not enough intersections to fill
                                               (loop-y (+ y 1)
                                                       (reverse unused)
                                                       (map advance-edge-x active-now)
                                                       c)
                                               ;; Fill scanline and advance
                                               (loop-y (+ y 1)
                                                       (reverse unused)
                                                       (map advance-edge-x active-now)
                                                       (fill-scanline c y xs ch))))]
                                    [(= (edge-y-min (car remaining)) y)
                                     ;; This edge starts at current y, add to active
                                     (add-loop (cdr remaining)
                                               (cons (car remaining) new-active)
                                               unused)]
                                    [else
                                     ;; This edge starts later, keep for future
                                     (add-loop (cdr remaining)
                                               new-active
                                               (cons (car remaining) unused))]))))))))

         ;;; ====
         ;;; Path Drawing
         ;;; ====
         
         ;;; A Path is a list of path commands:
         ;;;   (move-to point)           - Move to point without drawing
         ;;;   (line-to point)           - Draw line to point
         ;;;   (quad-to control point)   - Quadratic bezier curve
         ;;;   (close)                   - Close path back to start
         
         ;;; draw-path : Canvas × (List PathCommand) × Char → Canvas
         ;;; Draw a path from a list of commands.
         (define (draw-path canvas commands ch)
           (if (null? commands)
               canvas
               (let loop ([cmds commands]
                          [c canvas]
                          [current-pos #f]
                          [path-start #f])
                    (if (null? cmds)
                        c
                        (let ([cmd (car cmds)])
                             (case (car cmd)
                                   [(move-to)
                                    (let ([pt (cadr cmd)])
                                         (loop (cdr cmds) c pt
                                               (or path-start pt)))]
                                   
                                   [(line-to)
                                    (if (not current-pos)
                                        (loop (cdr cmds) c #f path-start)
                                        (let* ([pt (cadr cmd)]
                                               [c (draw-line c current-pos pt ch)])
                                              (loop (cdr cmds) c pt path-start)))]
                                   
                                   [(quad-to)
                                    ;; Simplified quadratic bezier: draw as two line segments
                                    (if (not current-pos)
                                        (loop (cdr cmds) c #f path-start)
                                        (let* ([ctrl (cadr cmd)]
                                               [end (caddr cmd)]
                                               [mid (point
                                                     (quotient (+ (point-x current-pos)
                                                                  (point-x ctrl)) 2)
                                                     (quotient (+ (point-y current-pos)
                                                                  (point-y ctrl)) 2))]
                                               [c (draw-line c current-pos mid ch)]
                                               [c (draw-line c mid end ch)])
                                              (loop (cdr cmds) c end path-start)))]
                                   
                                   [(close)
                                    (if (and current-pos path-start)
                                        (loop (cdr cmds)
                                              (draw-line c current-pos path-start ch)
                                              path-start
                                              path-start)
                                        (loop (cdr cmds) c current-pos path-start))]
                                   
                                   [else
                                    (loop (cdr cmds) c current-pos path-start)]))))))

         ;;; ====
         ;;; Styled Drawing
         ;;; ====
         
         ;;; draw-with-style : Canvas × Shape × Style → Canvas
         ;;; Draw a shape with the given style.
         ;;; Shape is one of:
         ;;;   (circle center radius)
         ;;;   (rect origin width height)
         ;;;   (polygon points)
         ;;;   (polyline points)
         ;;; Style controls fill, stroke, and opacity.
         (define (draw-with-style canvas shape style)
           (let ([fill-ch (apply-opacity (style-fill style) (style-opacity style))]
                 [stroke-ch (apply-opacity (style-stroke style) (style-opacity style))])
                (case (car shape)
                      [(circle)
                       (let ([center (cadr shape)]
                             [radius (caddr shape)])
                            (let ([c (if (char=? fill-ch #\space)
                                         canvas
                                         (draw-filled-circle canvas center radius fill-ch))])
                                 (if (char=? stroke-ch #\space)
                                     c
                                     (draw-circle c center radius stroke-ch))))]
                      
                      [(rect)
                       (let* ([origin (cadr shape)]
                              [width (caddr shape)]
                              [height (cadddr shape)]
                              [rect (make-rect origin width height)])
                             (let ([c (if (char=? fill-ch #\space)
                                          canvas
                                          (fill-rect canvas rect fill-ch))])
                                  (if (char=? stroke-ch #\space)
                                      c
                                      (draw-box c rect 'ascii))))]
                      
                      [(polygon)
                       (let ([points (cadr shape)])
                            (let ([c (if (char=? fill-ch #\space)
                                         canvas
                                         (draw-filled-polygon canvas points fill-ch))])
                                 (if (char=? stroke-ch #\space)
                                     c
                                     (draw-polygon c points stroke-ch))))]
                      
                      [(polyline)
                       (let ([points (cadr shape)])
                            (if (char=? stroke-ch #\space)
                                canvas
                                (draw-polyline canvas points stroke-ch)))]
                      
                      [else canvas])))

         ) ; end library
