;;; thimble/layout-combinators.ss — Diagrams-Style Layout Combinators
;;;
;;; Composable layout primitives inspired by Haskell's Diagrams library.
;;; Enables declarative composition of graphics elements with automatic sizing.
;;;
;;; This is Shell code: pure functions for spatial composition.
;;;
;;; Features:
;;;   - beside, above, atop — Basic spatial combinators
;;;   - distribute — Even spacing of multiple elements
;;;   - align — Alignment along edges/centers
;;;   - center — Center element within bounds
;;;   - Automatic bounding box calculation
;;;   - Padding and spacing control
;;;
;;; Dependencies:
;;;   - shell/layout.ss — Canvas primitives
;;;
;;; Layout Philosophy:
;;;   - All combinators are pure functions (no mutation)
;;;   - Automatic size computation (no manual dimension passing)
;;;   - Composable (output of one is input to another)
;;;   - Declarative syntax (describe what, not how)

(library (shell layout-combinators)
         (export
          ;; Basic combinators
          beside
          above
          atop
          
          ;; Multi-element combinators
          hcat    ;; Horizontal concatenation (beside multiple)
          vcat    ;; Vertical concatenation (above multiple)
          
          ;; Distribution
          distribute-horizontal
          distribute-vertical
          
          ;; Alignment
          align-left
          align-right
          align-top
          align-bottom
          align-center-h
          align-center-v
          align-center
          
          ;; Centering
          center-h
          center-v
          center-canvas
          
          ;; Padding
          pad
          pad-h
          pad-v
          
          ;; Bounding box
          canvas-bbox
          canvas-trim
          
          ;; Canvas with envelope (size metadata)
          make-diagram
          diagram-canvas
          diagram-width
          diagram-height
          diagram-set-size)
         
         (import (chezscheme))
         
         ;;; ============================================================
         ;;; Canvas Primitives (standalone definitions for library use)
         ;;; ============================================================
         
         ;;; These definitions enable the library to work standalone.
         ;;; They mirror shell/ui/layout.ss but are necessary because
         ;;; Scheme libraries can only import other libraries.
         
         (define (make-canvas% w h cells)
           (vector 'canvas w h cells))
         
         (define (canvas-width canvas)
           (vector-ref canvas 1))
         
         (define (canvas-height canvas)
           (vector-ref canvas 2))
         
         (define (canvas-cells canvas)
           (vector-ref canvas 3))
         
         (define (canvas-ref canvas x y)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)]
                 [cells (canvas-cells canvas)])
                (if (or (< x 0) (>= x w) (< y 0) (>= y h))
                    #\space
                    (vector-ref cells (+ (* y w) x)))))
         
         (define (canvas-set! canvas x y ch)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)]
                 [cells (canvas-cells canvas)])
                (unless (or (< x 0) (>= x w) (< y 0) (>= y h))
                        (vector-set! cells (+ (* y w) x) ch))))
         
         (define (make-canvas w h)
           (make-canvas% w h (make-vector (* w h) #\space)))
         
         (define (point x y) (cons x y))
         (define (point-x pt) (car pt))
         (define (point-y pt) (cdr pt))
         
         ;;; ============================================================
         ;;; Diagram (Canvas with Size Metadata)
         ;;; ============================================================
         
         ;;; A Diagram wraps a canvas with explicit size information.
         ;;; This allows combinators to work with canvases that have
         ;;; transparent padding or non-rectangular content.
         
         (define-record-type diagram%
           (fields canvas width height))
         
         ;;; make-diagram : Canvas × [Nat] × [Nat] → Diagram
         ;;; Create diagram from canvas, optionally override dimensions.
         (define make-diagram
           (case-lambda
            [(canvas)
             (make-diagram% canvas
                            (canvas-width canvas)
                            (canvas-height canvas))]
            [(canvas width height)
             (make-diagram% canvas width height)]))
         
         (define diagram-canvas diagram%-canvas)
         (define diagram-width diagram%-width)
         (define diagram-height diagram%-height)
         
         ;;; diagram-set-size : Diagram × Nat × Nat → Diagram
         (define (diagram-set-size diag w h)
           (make-diagram% (diagram-canvas diag) w h))
         
         ;;; diagram->canvas : Diagram → Canvas
         ;;; Extract canvas from diagram (for rendering).
         (define (diagram->canvas diag)
           (diagram-canvas diag))
         
         ;;; ============================================================
         ;;; Bounding Box Calculation
         ;;; ============================================================
         
         ;;; canvas-bbox : Canvas → (x-min y-min x-max y-max)
         ;;; Calculate tight bounding box around non-space content.
         ;;; Returns (0 0 0 0) for empty canvas.
         (define (canvas-bbox canvas)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (let loop-y ([y 0]
                             [min-x w]
                             [min-y h]
                             [max-x -1]
                             [max-y -1])
                     (if (>= y h)
                         (if (< max-x 0)
                             ;; Empty canvas
                             (list 0 0 0 0)
                             (list min-x min-y (+ max-x 1) (+ max-y 1)))
                         (let loop-x ([x 0]
                                      [min-x min-x]
                                      [min-y min-y]
                                      [max-x max-x]
                                      [max-y max-y])
                              (if (>= x w)
                                  (loop-y (+ y 1) min-x min-y max-x max-y)
                                  (let ([ch (canvas-ref canvas x y)])
                                       (if (char=? ch #\space)
                                           (loop-x (+ x 1) min-x min-y max-x max-y)
                                           ;; Non-space found
                                           (loop-x (+ x 1)
                                                   (min min-x x)
                                                   (min min-y y)
                                                   (max max-x x)
                                                   (max max-y y))))))))))
         
         ;;; canvas-trim : Canvas → Canvas
         ;;; Trim canvas to minimal bounding box.
         (define (canvas-trim canvas)
           (let* ([bbox (canvas-bbox canvas)]
                  [x1 (list-ref bbox 0)]
                  [y1 (list-ref bbox 1)]
                  [x2 (list-ref bbox 2)]
                  [y2 (list-ref bbox 3)]
                  [w (- x2 x1)]
                  [h (- y2 y1)])
                 (if (or (<= w 0) (<= h 0))
                     (make-canvas 1 1)
                     (let ([result (make-canvas w h)])
                          (let loop-y ([y 0])
                               (if (>= y h)
                                   result
                                   (let loop-x ([x 0])
                                        (if (>= x w)
                                            (loop-y (+ y 1))
                                            (begin
                                             (canvas-set! result x y
                                                          (canvas-ref canvas
                                                                      (+ x1 x)
                                                                      (+ y1 y)))
                                             (loop-x (+ x 1))))))))))))

         ;;; ============================================================
         ;;; Basic Spatial Combinators
         ;;; ============================================================
         
         ;;; composite-at : Canvas × Canvas × Point → Canvas
         ;;; Helper: overlay src onto dest at given point.
         (define (composite-at dest src pt)
           (let ([ox (point-x pt)]
                 [oy (point-y pt)]
                 [sw (canvas-width src)]
                 [sh (canvas-height src)])
                (let loop-y ([y 0])
                     (if (>= y sh)
                         dest
                         (let loop-x ([x 0])
                              (if (>= x sw)
                                  (loop-y (+ y 1))
                                  (let ([ch (canvas-ref src x y)])
                                       ;; Skip spaces to preserve transparency
                                       (unless (char=? ch #\space)
                                               (canvas-set! dest
                                                            (+ ox x)
                                                            (+ oy y)
                                                            ch))
                                       (loop-x (+ x 1)))))))))

         ;;; beside : Canvas × Canvas × [Nat] → Canvas
         ;;; Place two canvases side-by-side (left | right) with optional spacing.
         (define beside
           (case-lambda
            [(left right)
             (beside left right 0)]
            [(left right spacing)
             (let* ([lw (canvas-width left)]
                    [lh (canvas-height left)]
                    [rw (canvas-width right)]
                    [rh (canvas-height right)]
                    [total-w (+ lw spacing rw)]
                    [total-h (max lh rh)]
                    [result (make-canvas total-w total-h)]
                    [result (composite-at result left (point 0 0))]
                    [result (composite-at result right (point (+ lw spacing) 0))])
                   result)]))

         ;;; above : Canvas × Canvas × [Nat] → Canvas
         ;;; Stack two canvases vertically (top / bottom) with optional spacing.
         (define above
           (case-lambda
            [(top bottom)
             (above top bottom 0)]
            [(top bottom spacing)
             (let* ([tw (canvas-width top)]
                    [th (canvas-height top)]
                    [bw (canvas-width bottom)]
                    [bh (canvas-height bottom)]
                    [total-w (max tw bw)]
                    [total-h (+ th spacing bh)]
                    [result (make-canvas total-w total-h)]
                    [result (composite-at result top (point 0 0))]
                    [result (composite-at result bottom (point 0 (+ th spacing)))])
                   result)]))

         ;;; atop : Canvas × Canvas → Canvas
         ;;; Overlay top canvas on bottom canvas (z-order: bottom, then top).
         ;;; Result size is bounding box of both.
         (define (atop bottom top)
           (let* ([bw (canvas-width bottom)]
                  [bh (canvas-height bottom)]
                  [tw (canvas-width top)]
                  [th (canvas-height top)]
                  [total-w (max bw tw)]
                  [total-h (max bh th)]
                  [result (make-canvas total-w total-h)]
                  [result (composite-at result bottom (point 0 0))]
                  [result (composite-at result top (point 0 0))])
                 result))

         ;;; ============================================================
         ;;; Multi-Element Combinators
         ;;; ============================================================
         
         ;;; hcat : (List Canvas) × [Nat] → Canvas
         ;;; Horizontal concatenation (beside multiple canvases).
         (define hcat
           (case-lambda
            [(canvases)
             (hcat canvases 0)]
            [(canvases spacing)
             (if (null? canvases)
                 (make-canvas 1 1)
                 (fold-left (lambda (acc canvas)
                                    (beside acc canvas spacing))
                            (car canvases)
                            (cdr canvases)))]))

         ;;; vcat : (List Canvas) × [Nat] → Canvas
         ;;; Vertical concatenation (above multiple canvases).
         (define vcat
           (case-lambda
            [(canvases)
             (vcat canvases 0)]
            [(canvases spacing)
             (if (null? canvases)
                 (make-canvas 1 1)
                 (fold-left (lambda (acc canvas)
                                    (above acc canvas spacing))
                            (car canvases)
                            (cdr canvases)))]))

         ;;; ============================================================
         ;;; Distribution (Even Spacing)
         ;;; ============================================================
         
         ;;; distribute-horizontal : (List Canvas) × Nat → Canvas
         ;;; Distribute canvases horizontally with even spacing.
         (define (distribute-horizontal canvases total-width)
           (if (null? canvases)
               (make-canvas total-width 1)
               (let* ([n (length canvases)]
                      [total-content-w (fold-left (lambda (acc c)
                                                          (+ acc (canvas-width c)))
                                                  0
                                                  canvases)]
                      [total-gap (- total-width total-content-w)]
                      [spacing (if (<= n 1)
                                   0
                                   (quotient total-gap (- n 1)))]
                      [h (apply max (map canvas-height canvases))])
                     (hcat canvases spacing))))

         ;;; distribute-vertical : (List Canvas) × Nat → Canvas
         ;;; Distribute canvases vertically with even spacing.
         (define (distribute-vertical canvases total-height)
           (if (null? canvases)
               (make-canvas 1 total-height)
               (let* ([n (length canvases)]
                      [total-content-h (fold-left (lambda (acc c)
                                                          (+ acc (canvas-height c)))
                                                  0
                                                  canvases)]
                      [total-gap (- total-height total-content-h)]
                      [spacing (if (<= n 1)
                                   0
                                   (quotient total-gap (- n 1)))]
                      [w (apply max (map canvas-width canvases))])
                     (vcat canvases spacing))))

         ;;; ============================================================
         ;;; Alignment
         ;;; ============================================================
         
         ;;; align-left : Canvas × Nat → Canvas
         ;;; Align canvas to left within target width.
         (define (align-left canvas target-width)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (if (>= w target-width)
                    canvas
                    (let ([result (make-canvas target-width h)])
                         (composite-at result canvas (point 0 0))))))

         ;;; align-right : Canvas × Nat → Canvas
         ;;; Align canvas to right within target width.
         (define (align-right canvas target-width)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (if (>= w target-width)
                    canvas
                    (let* ([offset (- target-width w)]
                           [result (make-canvas target-width h)])
                          (composite-at result canvas (point offset 0))))))

         ;;; align-top : Canvas × Nat → Canvas
         ;;; Align canvas to top within target height.
         (define (align-top canvas target-height)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (if (>= h target-height)
                    canvas
                    (let ([result (make-canvas w target-height)])
                         (composite-at result canvas (point 0 0))))))

         ;;; align-bottom : Canvas × Nat → Canvas
         ;;; Align canvas to bottom within target height.
         (define (align-bottom canvas target-height)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (if (>= h target-height)
                    canvas
                    (let* ([offset (- target-height h)]
                           [result (make-canvas w target-height)])
                          (composite-at result canvas (point 0 offset))))))

         ;;; align-center-h : Canvas × Nat → Canvas
         ;;; Center canvas horizontally within target width.
         (define (align-center-h canvas target-width)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (if (>= w target-width)
                    canvas
                    (let* ([offset (quotient (- target-width w) 2)]
                           [result (make-canvas target-width h)])
                          (composite-at result canvas (point offset 0))))))

         ;;; align-center-v : Canvas × Nat → Canvas
         ;;; Center canvas vertically within target height.
         (define (align-center-v canvas target-height)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (if (>= h target-height)
                    canvas
                    (let* ([offset (quotient (- target-height h) 2)]
                           [result (make-canvas w target-height)])
                          (composite-at result canvas (point 0 offset))))))

         ;;; align-center : Canvas × Nat × Nat → Canvas
         ;;; Center canvas both horizontally and vertically.
         (define (align-center canvas target-width target-height)
           (let ([w (canvas-width canvas)]
                 [h (canvas-height canvas)])
                (let* ([x-offset (quotient (max 0 (- target-width w)) 2)]
                       [y-offset (quotient (max 0 (- target-height h)) 2)]
                       [result-w (max w target-width)]
                       [result-h (max h target-height)]
                       [result (make-canvas result-w result-h)])
                      (composite-at result canvas (point x-offset y-offset)))))

         ;;; ============================================================
         ;;; Centering (Convenience Wrappers)
         ;;; ============================================================
         
         ;;; center-h : Canvas × Nat → Canvas
         ;;; Alias for align-center-h
         (define center-h align-center-h)

         ;;; center-v : Canvas × Nat → Canvas
         ;;; Alias for align-center-v
         (define center-v align-center-v)

         ;;; center-canvas : Canvas × Nat × Nat → Canvas
         ;;; Alias for align-center
         (define center-canvas align-center)

         ;;; ============================================================
         ;;; Padding
         ;;; ============================================================
         
         ;;; pad : Canvas × Nat → Canvas
         ;;; Add uniform padding on all sides.
         (define (pad canvas amount)
           (let* ([w (canvas-width canvas)]
                  [h (canvas-height canvas)]
                  [new-w (+ w (* 2 amount))]
                  [new-h (+ h (* 2 amount))]
                  [result (make-canvas new-w new-h)])
                 (composite-at result canvas (point amount amount))))

         ;;; pad-h : Canvas × Nat → Canvas
         ;;; Add horizontal padding (left and right).
         (define (pad-h canvas amount)
           (let* ([w (canvas-width canvas)]
                  [h (canvas-height canvas)]
                  [new-w (+ w (* 2 amount))]
                  [result (make-canvas new-w h)])
                 (composite-at result canvas (point amount 0))))

         ;;; pad-v : Canvas × Nat → Canvas
         ;;; Add vertical padding (top and bottom).
         (define (pad-v canvas amount)
           (let* ([w (canvas-width canvas)]
                  [h (canvas-height canvas)]
                  [new-h (+ h (* 2 amount))]
                  [result (make-canvas w new-h)])
                 (composite-at result canvas (point 0 amount))))

         ) ; end library
