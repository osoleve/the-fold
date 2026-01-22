;;; lattice/dataset/presentation.ss — Layout Combinators for Visual Reasoning
;;; @module presentation
;;; @requires prelude layout

(load "core/base/prelude.ss")
(load "boundary/ui/layout.ss")

(doc 'module 'presentation)
(doc 'description "Composable layout system for combining physics frames with textual context. Supports multiple output targets (plain text, ANSI, structured).")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Part 1: Layout Elements
;;; ============================================================
;;;
;;; Layout elements are the building blocks:
;;;   - text-block: wrapped text paragraph
;;;   - frame-block: ASCII frame with optional border
;;;   - textbox: labeled box with key-value pairs
;;;   - spacer: vertical/horizontal spacing
;;;   - divider: horizontal rule

(doc 'section 'elements)

;;; Text block - a paragraph of wrapped text
(define (make-text-block content width alignment)
  (doc 'export #t)
  (list 'text-block content width alignment))

(define (text-block? x) (and (pair? x) (eq? (car x) 'text-block)))
(define (text-block-content tb) (list-ref tb 1))
(define (text-block-width tb) (list-ref tb 2))
(define (text-block-alignment tb) (list-ref tb 3))

;;; Frame block - an ASCII frame (physics visualization)
(define (make-frame-block frame-str width height border?)
  (doc 'export #t)
  (list 'frame-block frame-str width height border?))

(define (frame-block? x) (and (pair? x) (eq? (car x) 'frame-block)))
(define (frame-block-content fb) (list-ref fb 1))
(define (frame-block-width fb) (list-ref fb 2))
(define (frame-block-height fb) (list-ref fb 3))
(define (frame-block-border? fb) (list-ref fb 4))

;;; Textbox - labeled box with parameters
(define (make-textbox label pairs width style)
  (doc 'export #t)
  (doc 'description "Create textbox with label and key-value pairs")
  (list 'textbox label pairs width style))

(define (textbox? x) (and (pair? x) (eq? (car x) 'textbox)))
(define (textbox-label tb) (list-ref tb 1))
(define (textbox-pairs tb) (list-ref tb 2))
(define (textbox-width tb) (list-ref tb 3))
(define (textbox-style tb) (list-ref tb 4))

;;; Spacer - empty vertical space
(define (make-spacer height)
  (doc 'export #t)
  (list 'spacer height))

(define (spacer? x) (and (pair? x) (eq? (car x) 'spacer)))
(define (spacer-height s) (list-ref s 1))

;;; Divider - horizontal rule
(define (make-divider width char)
  (doc 'export #t)
  (list 'divider width char))

(define (divider? x) (and (pair? x) (eq? (car x) 'divider)))
(define (divider-width d) (list-ref d 1))
(define (divider-char d) (list-ref d 2))

;;; ============================================================
;;; Part 2: Layout Combinators
;;; ============================================================
;;;
;;; Combinators compose elements into layouts:
;;;   - vstack: vertical stack
;;;   - hstack: horizontal stack
;;;   - beside: place elements side by side
;;;   - above: place elements vertically

(doc 'section 'combinators)

;;; vstack : (List Element) → Layout
;;; Stack elements vertically
(define (layout-vstack elements)
  (doc 'export #t)
  (list 'layout-vstack elements))

(define (layout-vstack? x) (and (pair? x) (eq? (car x) 'layout-vstack)))
(define (layout-vstack-elements l) (list-ref l 1))

;;; hstack : (List Element) × Nat → Layout
;;; Stack elements horizontally with given gap
(define (layout-hstack elements gap)
  (doc 'export #t)
  (list 'layout-hstack elements gap))

(define (layout-hstack? x) (and (pair? x) (eq? (car x) 'layout-hstack)))
(define (layout-hstack-elements l) (list-ref l 1))
(define (layout-hstack-gap l) (list-ref l 2))

;;; beside : Element × Element × Nat → Layout
;;; Place two elements side by side
(define (layout-beside left right gap)
  (doc 'export #t)
  (layout-hstack (list left right) gap))

;;; above : Element × Element → Layout
;;; Place two elements vertically
(define (layout-above top bottom)
  (doc 'export #t)
  (layout-vstack (list top bottom)))

;;; ============================================================
;;; Part 3: Element Dimensions
;;; ============================================================

(doc 'section 'dimensions)

;;; element-height : Element → Nat
(define (element-height elem)
  (doc 'export #t)
  (cond
    [(text-block? elem)
     (length (wrap-text (text-block-content elem) (text-block-width elem)))]
    [(frame-block? elem)
     (+ (frame-block-height elem)
        (if (frame-block-border? elem) 2 0))]
    [(textbox? elem)
     (+ 2 (length (textbox-pairs elem)))]  ; top border + content + bottom
    [(spacer? elem)
     (spacer-height elem)]
    [(divider? elem)
     1]
    [(layout-vstack? elem)
     (apply + (map element-height (layout-vstack-elements elem)))]
    [(layout-hstack? elem)
     (apply max (cons 0 (map element-height (layout-hstack-elements elem))))]
    [else 1]))

;;; element-width : Element → Nat
(define (element-width elem)
  (doc 'export #t)
  (cond
    [(text-block? elem)
     (text-block-width elem)]
    [(frame-block? elem)
     (+ (frame-block-width elem)
        (if (frame-block-border? elem) 2 0))]
    [(textbox? elem)
     (textbox-width elem)]
    [(spacer? elem)
     0]
    [(divider? elem)
     (divider-width elem)]
    [(layout-vstack? elem)
     (apply max (cons 0 (map element-width (layout-vstack-elements elem))))]
    [(layout-hstack? elem)
     (let ([widths (map element-width (layout-hstack-elements elem))]
           [gap (layout-hstack-gap elem)]
           [n (length (layout-hstack-elements elem))])
       (+ (apply + widths)
          (* gap (max 0 (- n 1)))))]
    [else 0]))

;;; ============================================================
;;; Part 4: Rendering to Canvas
;;; ============================================================

(doc 'section 'rendering)

;;; render-element : Element × Canvas × Nat × Nat → Canvas
;;; Render element to canvas at position (x, y)
(define (render-element elem canvas x y)
  (doc 'export #t)
  (cond
    [(text-block? elem)
     (render-text-block elem canvas x y)]
    [(frame-block? elem)
     (render-frame-block elem canvas x y)]
    [(textbox? elem)
     (render-textbox elem canvas x y)]
    [(spacer? elem)
     canvas]  ; No-op, just takes up space
    [(divider? elem)
     (render-divider elem canvas x y)]
    [(layout-vstack? elem)
     (render-vstack elem canvas x y)]
    [(layout-hstack? elem)
     (render-hstack elem canvas x y)]
    [else canvas]))

;;; render-text-block : TextBlock × Canvas × Nat × Nat → Canvas
(define (render-text-block tb canvas x y)
  (let* ([lines (wrap-text (text-block-content tb) (text-block-width tb))]
         [width (text-block-width tb)]
         [alignment (text-block-alignment tb)]
         [align-fn (case alignment
                     [(left) align-left]
                     [(right) align-right]
                     [(center) align-center]
                     [else align-left])])
    (let loop ([lines lines] [row 0] [c canvas])
      (if (null? lines)
          c
          (loop (cdr lines)
                (+ row 1)
                (draw-string c (point x (+ y row))
                            (align-fn (car lines) width)))))))

;;; render-frame-block : FrameBlock × Canvas × Nat × Nat → Canvas
(define (render-frame-block fb canvas x y)
  (let* ([content (frame-block-content fb)]
         [width (frame-block-width fb)]
         [height (frame-block-height fb)]
         [border? (frame-block-border? fb)]
         [lines (string-split-lines content)]
         [ox (if border? (+ x 1) x)]
         [oy (if border? (+ y 1) y)])
    ;; Draw border if requested
    (let ([c (if border?
                 (draw-box canvas
                          (make-rect (point x y)
                                    (+ width 2)
                                    (+ height 2))
                          'light)
                 canvas)])
      ;; Draw frame content
      (let loop ([lines lines] [row 0] [c c])
        (if (or (null? lines) (>= row height))
            c
            (loop (cdr lines)
                  (+ row 1)
                  (draw-string c (point ox (+ oy row))
                              (string-truncate (car lines) width))))))))

;;; string-split-lines : String → (List String)
(define (string-split-lines s)
  (let loop ([chars (string->list s)]
             [current '()]
             [lines '()])
    (cond
      [(null? chars)
       (reverse (if (null? current)
                    lines
                    (cons (list->string (reverse current)) lines)))]
      [(char=? (car chars) #\newline)
       (loop (cdr chars)
             '()
             (cons (list->string (reverse current)) lines))]
      [else
       (loop (cdr chars)
             (cons (car chars) current)
             lines)])))

;;; string-truncate : String × Nat → String
(define (string-truncate s max-len)
  (if (<= (string-length s) max-len)
      s
      (substring s 0 max-len)))

;;; render-textbox : Textbox × Canvas × Nat × Nat → Canvas
(define (render-textbox tb canvas x y)
  (let* ([label (textbox-label tb)]
         [pairs (textbox-pairs tb)]
         [width (textbox-width tb)]
         [style (textbox-style tb)]
         [box-style (or style 'light)])
    ;; Draw box
    (let* ([height (+ 2 (length pairs))]
           [c (draw-box canvas (make-rect (point x y) width height) box-style)]
           ;; Draw label
           [label-str (string-append "─" label "─")]
           [c (draw-string c (point (+ x 2) y) label-str)])
      ;; Draw pairs
      (let loop ([pairs pairs] [row 1] [c c])
        (if (null? pairs)
            c
            (let* ([pair (car pairs)]
                   [key (car pair)]
                   [val (cdr pair)]
                   [line (string-append " " (symbol->string key) "=" (format-value val) " ")])
              (loop (cdr pairs)
                    (+ row 1)
                    (draw-string c (point (+ x 1) (+ y row))
                                (string-truncate line (- width 2))))))))))

;;; format-value : Any → String
(define (format-value v)
  (cond
    [(number? v)
     (if (integer? v)
         (number->string (inexact->exact v))
         (let ([s (number->string (exact->inexact v))])
           (if (> (string-length s) 5)
               (substring s 0 5)
               s)))]
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [else (format "~a" v)]))

;;; render-divider : Divider × Canvas × Nat × Nat → Canvas
(define (render-divider d canvas x y)
  (let* ([width (divider-width d)]
         [char (divider-char d)]
         [line (make-string width char)])
    (draw-string canvas (point x y) line)))

;;; render-vstack : VStack × Canvas × Nat × Nat → Canvas
(define (render-vstack vs canvas x y)
  (let loop ([elems (layout-vstack-elements vs)]
             [row y]
             [c canvas])
    (if (null? elems)
        c
        (let* ([elem (car elems)]
               [c (render-element elem c x row)]
               [h (element-height elem)])
          (loop (cdr elems) (+ row h) c)))))

;;; render-hstack : HStack × Canvas × Nat × Nat → Canvas
(define (render-hstack hs canvas x y)
  (let ([gap (layout-hstack-gap hs)])
    (let loop ([elems (layout-hstack-elements hs)]
               [col x]
               [c canvas])
      (if (null? elems)
          c
          (let* ([elem (car elems)]
                 [c (render-element elem c col y)]
                 [w (element-width elem)])
            (loop (cdr elems) (+ col w gap) c))))))

;;; ============================================================
;;; Part 5: High-Level Rendering
;;; ============================================================

(doc 'section 'high-level)

;;; render-layout : Layout → String
;;; Render layout to string
(define (render-layout layout)
  (doc 'export #t)
  (let* ([width (element-width layout)]
         [height (element-height layout)]
         [canvas (make-canvas width height)]
         [canvas (render-element layout canvas 0 0)])
    (canvas->string canvas)))

;;; render-layout-sized : Layout × Nat × Nat → String
;;; Render to specific canvas size
(define (render-layout-sized layout width height)
  (doc 'export #t)
  (let* ([canvas (make-canvas width height)]
         [canvas (render-element layout canvas 0 0)])
    (canvas->string canvas)))

;;; ============================================================
;;; Part 6: Convenience Constructors
;;; ============================================================

(doc 'section 'convenience)

;;; text : String × Nat → TextBlock
(define (text content width)
  (doc 'export #t)
  (make-text-block content width 'left))

;;; text-center : String × Nat → TextBlock
(define (text-center content width)
  (doc 'export #t)
  (make-text-block content width 'center))

;;; frame : String × Nat × Nat → FrameBlock
(define (frame content width height)
  (doc 'export #t)
  (make-frame-block content width height #f))

;;; frame-bordered : String × Nat × Nat → FrameBlock
(define (frame-bordered content width height)
  (doc 'export #t)
  (make-frame-block content width height #t))

;;; given-box : Alist × Nat → Textbox
;;; Create a "Given" parameters textbox
(define (given-box params width)
  (doc 'export #t)
  (make-textbox "Given" params width 'light))

;;; find-box : Alist × Nat → Textbox
;;; Create a "Find" textbox
(define (find-box items width)
  (doc 'export #t)
  (make-textbox "Find" items width 'light))

;;; gap : Nat → Spacer
(define (gap height)
  (doc 'export #t)
  (make-spacer height))

;;; hrule : Nat → Divider
(define (hrule width)
  (doc 'export #t)
  (make-divider width #\─))

;;; ============================================================
;;; Part 7: Problem Layout Templates
;;; ============================================================
;;;
;;; Pre-built layout patterns for common problem presentations.

(doc 'section 'templates)

;;; layout-word-problem : String × String × (List String) × Nat → Layout
;;; Word problem with context, frames, and question
(define (layout-word-problem context frames question width)
  (doc 'export #t)
  (if (null? frames)
      ;; Handle empty frames gracefully
      (layout-vstack
       (list
        (text context width)
        (gap 1)
        (text "[No frames available]" width)
        (gap 1)
        (text question width)))
      (let ([frame-height (count-lines (car frames))])
        (layout-vstack
         (list
          (text context width)
          (gap 1)
          (layout-vstack
           (map (lambda (f) (frame f width frame-height)) frames))
          (gap 1)
          (text question width))))))

;;; layout-visual-only : (List String) × String × Nat × Nat → Layout
;;; Visual-only problem (no explicit parameters)
(define (layout-visual-only frames question width height)
  (doc 'export #t)
  (layout-vstack
   (append
    (map (lambda (f) (frame f width height)) frames)
    (list (gap 1) (text question width)))))

;;; layout-structured : Alist × (List String) × String × Nat × Nat → Layout
;;; Structured problem with Given/Find boxes
(define (layout-structured given-params frames find-items width height)
  (doc 'export #t)
  (let ([box-width (min 20 (quotient width 3))])
    (layout-vstack
     (list
      (layout-hstack
       (list
        (given-box given-params box-width)
        (layout-vstack (map (lambda (f) (frame f (- width box-width 2) height)) frames)))
       2)
      (gap 1)
      (find-box find-items box-width)))))

;;; count-lines : String → Nat
(define (count-lines s)
  (length (string-split-lines s)))

;;; ============================================================
;;; Part 8: Frame Sequence Rendering
;;; ============================================================

(doc 'section 'sequences)

;;; layout-frame-sequence : (List String) × Nat × Nat × Symbol → Layout
;;; Arrange multiple frames (vstack, hstack, or grid)
(define (layout-frame-sequence frames width height arrangement)
  (doc 'export #t)
  (let ([frame-elems (map (lambda (f) (frame f width height)) frames)])
    (case arrangement
      [(vstack) (layout-vstack frame-elems)]
      [(hstack) (layout-hstack frame-elems 1)]
      [(grid)
       ;; 2xN grid
       (let loop ([frames frame-elems] [rows '()])
         (if (null? frames)
             (layout-vstack (reverse rows))
             (let ([row-frames (take-up-to 2 frames)]
                   [rest (drop-up-to 2 frames)])
               (loop rest
                     (cons (layout-hstack row-frames 1) rows)))))]
      [else (layout-vstack frame-elems)])))

;;; take-up-to : Nat × (List a) → (List a)
(define (take-up-to n xs)
  (if (or (<= n 0) (null? xs))
      '()
      (cons (car xs) (take-up-to (- n 1) (cdr xs)))))

;;; drop-up-to : Nat × (List a) → (List a)
(define (drop-up-to n xs)
  (if (or (<= n 0) (null? xs))
      xs
      (drop-up-to (- n 1) (cdr xs))))
