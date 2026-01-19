;;; boundary/layout.ss — Text Layout Primitives
;;;
;;; The canvas for DUCKIE — a 2D character framebuffer for ASCII/ANSI rendering.
;;;
;;; This is Shell code: handles display/IO, but keeps pure functions pure.
;;;
;;; Data Structures:
;;;   Canvas — A 2D grid of characters (width × height)
;;;   Point  — A coordinate pair (x . y), origin at top-left
;;;   Rect   — A rectangle (origin, width, height)
;;;
;;; Operations:
;;;   Construction: make-canvas, canvas-width, canvas-height, canvas-ref, canvas-set, canvas-set!
;;;   Drawing: draw-char, draw-string, draw-string-v, draw-rect, fill-rect
;;;   Composition: composite, blit
;;;   Rendering: canvas->string
;;;
;;; For advanced layering with transparency and z-ordering, see boundary/ui/layers.ss

(load "core/base/prelude.ss")

;;; ====
;;; Data Structures
;;; ====

;;; Canvas: A 2D character grid
;;; Cells stored in row-major order: cells[y * width + x]
(define-record-type canvas%
  (fields width height cells))

;;; Point: (x . y) coordinate pair
;;; Origin is top-left (0, 0)
(define (point x y)
  (cons x y))

(define (point-x p) (car p))
(define (point-y p) (cdr p))

;;; Rect: Rectangle defined by origin point, width, and height
(define-record-type rect%
  (fields origin width height))

;;; make-rect : Point × Nat × Nat → Rect
;;; Constructor wrapper
(define make-rect make-rect%)

;;; Re-export accessors with clean names
(define rect-origin rect%-origin)
(define rect-width rect%-width)
(define rect-height rect%-height)

;;; ====
;;; Canvas Construction
;;; ====

;;; make-canvas : Nat × Nat → Canvas
;;; Create a canvas filled with spaces.
(define (make-canvas width height)
  (let ([size (* width height)]
        [cells (make-vector (* width height) #\space)])
       (make-canvas% width height cells)))

;;; canvas-width : Canvas → Nat
;;; canvas-height : Canvas → Nat
;;; Re-export the auto-generated accessors with clean names
(define canvas-width canvas%-width)
(define canvas-height canvas%-height)
(define canvas-cells canvas%-cells)

;;; canvas-ref : Canvas × Nat × Nat → Char
;;; Get character at (x, y). Returns #\space if out of bounds.
(define (canvas-ref c x y)
  (let ([w (canvas-width c)]
        [h (canvas-height c)])
       (if (or (< x 0) (< y 0) (>= x w) (>= y h))
           #\space
           (vector-ref (canvas-cells c) (+ (* y w) x)))))

;;; canvas-set : Canvas × Nat × Nat × Char → Canvas
;;; Set character at (x, y). Returns new canvas.
;;; Out-of-bounds coordinates are ignored (no-op).
;;; NOTE: This is O(N) per call due to vector-copy. For bulk updates,
;;; use canvas-set! with a copy, or canvas-set-many for batching.
(define (canvas-set c x y ch)
  (let ([w (canvas-width c)]
        [h (canvas-height c)])
       (if (or (< x 0) (< y 0) (>= x w) (>= y h))
           c  ; Out of bounds, return unchanged
           (let ([cells (vector-copy (canvas-cells c))]
                 [idx (+ (* y w) x)])
                (vector-set! cells idx ch)
                (make-canvas% w h cells)))))

;;; canvas-set-many : Canvas × (List (x y char)) → Canvas
;;; Set multiple characters in a single copy operation.
;;; PERFORMANCE: Use this instead of repeated canvas-set calls.
;;; Each update is (x y char) triple. Out-of-bounds updates are ignored.
(define (canvas-set-many c updates)
  (if (null? updates)
      c
      (let* ([w (canvas-width c)]
             [h (canvas-height c)]
             [cells (vector-copy (canvas-cells c))])
            ;; Apply all updates to the copied vector
            (for-each
             (lambda (update)
                     (let ([x (car update)]
                           [y (cadr update)]
                           [ch (caddr update)])
                          (unless (or (< x 0) (< y 0) (>= x w) (>= y h))
                                  (vector-set! cells (+ (* y w) x) ch))))
             updates)
            (make-canvas% w h cells))))

;;; canvas-set! : Canvas × Nat × Nat × Char → void
;;; Set character at (x, y). Mutates canvas in-place (imperative).
;;; Out-of-bounds coordinates are ignored (no-op).
(define (canvas-set! c x y ch)
  (let ([w (canvas-width c)]
        [h (canvas-height c)])
       (unless (or (< x 0) (< y 0) (>= x w) (>= y h))
               (let ([cells (canvas-cells c)]
                     [idx (+ (* y w) x)])
                    (vector-set! cells idx ch)))))

;;; ====
;;; Drawing Primitives
;;; ====

;;; draw-char : Canvas × Point × Char → Canvas
;;; Place a single character at the given point.
(define (draw-char c pt ch)
  (canvas-set c (point-x pt) (point-y pt) ch))

;;; draw-string : Canvas × Point × String → Canvas
;;; Draw string horizontally starting at point, left-to-right.
;;; Clips at canvas boundary (no wrapping).
(define (draw-string c pt str)
  (let ([x (point-x pt)]
        [y (point-y pt)]
        [w (canvas-width c)]
        [len (string-length str)])
       (let loop ([i 0] [canvas c])
            (if (or (>= i len) (>= (+ x i) w))
                canvas
                (loop (+ i 1)
                      (canvas-set canvas (+ x i) y (string-ref str i)))))))

;;; draw-string-v : Canvas × Point × String → Canvas
;;; Draw string vertically, top-to-bottom.
(define (draw-string-v c pt str)
  (let ([x (point-x pt)]
        [y (point-y pt)]
        [h (canvas-height c)]
        [len (string-length str)])
       (let loop ([i 0] [canvas c])
            (if (or (>= i len) (>= (+ y i) h))
                canvas
                (loop (+ i 1)
                      (canvas-set canvas x (+ y i) (string-ref str i)))))))

;;; draw-rect : Canvas × Rect × Char → Canvas
;;; Draw rectangle outline using the given character.
(define (draw-rect c r ch)
  (let* ([ox (point-x (rect-origin r))]
         [oy (point-y (rect-origin r))]
         [w (rect-width r)]
         [h (rect-height r)]
         [right (+ ox w -1)]
         [bottom (+ oy h -1)])
        (if (or (<= w 0) (<= h 0))
            c  ; Empty rect, no-op
            (let* ([canvas c]
                   ;; Draw top edge
                   [canvas (let loop ([x ox] [canvas canvas])
                                (if (>= x (+ ox w))
                                    canvas
                                    (loop (+ x 1) (canvas-set canvas x oy ch))))]
                   ;; Draw bottom edge
                   [canvas (let loop ([x ox] [canvas canvas])
                                (if (>= x (+ ox w))
                                    canvas
                                    (loop (+ x 1) (canvas-set canvas x bottom ch))))]
                   ;; Draw left edge
                   [canvas (let loop ([y oy] [canvas canvas])
                                (if (>= y (+ oy h))
                                    canvas
                                    (loop (+ y 1) (canvas-set canvas ox y ch))))]
                   ;; Draw right edge
                   [canvas (let loop ([y oy] [canvas canvas])
                                (if (>= y (+ oy h))
                                    canvas
                                    (loop (+ y 1) (canvas-set canvas right y ch))))])
                  canvas))))

;;; fill-rect : Canvas × Rect × Char → Canvas
;;; Fill rectangle interior with the given character.
(define (fill-rect c r ch)
  (let ([ox (point-x (rect-origin r))]
        [oy (point-y (rect-origin r))]
        [w (rect-width r)]
        [h (rect-height r)])
       (if (or (<= w 0) (<= h 0))
           c  ; Empty rect, no-op
           (let loop-y ([y oy] [canvas c])
                (if (>= y (+ oy h))
                    canvas
                    (let loop-x ([x ox] [canvas canvas])
                         (if (>= x (+ ox w))
                             (loop-y (+ y 1) canvas)
                             (loop-x (+ x 1) (canvas-set canvas x y ch)))))))))

;;; ====
;;; Composition
;;; ====

;;; composite : Canvas × Canvas × Point → Canvas
;;; Overlay source canvas onto destination at given position.
;;; Source overwrites dest (no transparency).
;;;
;;; Note: For transparency-aware compositing, see composite-with-transparency
;;; or use the layering system in boundary/ui/layers.ss
(define (composite dest src pt)
  (let ([ox (point-x pt)]
        [oy (point-y pt)]
        [sw (canvas-width src)]
        [sh (canvas-height src)])
       (let loop-y ([y 0] [canvas dest])
            (if (>= y sh)
                canvas
                (let loop-x ([x 0] [canvas canvas])
                     (if (>= x sw)
                         (loop-y (+ y 1) canvas)
                         (let ([ch (canvas-ref src x y)])
                              (loop-x (+ x 1)
                                      (canvas-set canvas (+ ox x) (+ oy y) ch)))))))))

;;; composite-with-transparency : Canvas × Canvas × Point × Char → Canvas
;;; Overlay source onto destination, skipping cells that match transparent-char.
;;; This allows transparent sprites and layered composition.
;;;
;;; Arguments:
;;;   dest            — Destination canvas
;;;   src             — Source canvas to overlay
;;;   pt              — Position to place source
;;;   transparent-char — Character value to treat as transparent
(define (composite-with-transparency dest src pt transparent-char)
  (let ([ox (point-x pt)]
        [oy (point-y pt)]
        [sw (canvas-width src)]
        [sh (canvas-height src)])
       (let loop-y ([y 0] [canvas dest])
            (if (>= y sh)
                canvas
                (let loop-x ([x 0] [canvas canvas])
                     (if (>= x sw)
                         (loop-y (+ y 1) canvas)
                         (let ([ch (canvas-ref src x y)])
                              (if (char=? ch transparent-char)
                                  ;; Skip transparent cells
                                  (loop-x (+ x 1) canvas)
                                  ;; Draw opaque cells
                                  (loop-x (+ x 1)
                                          (canvas-set canvas (+ ox x) (+ oy y) ch))))))))))

;;; blit : Canvas × Canvas × Rect × Point → Canvas
;;; Copy region from source to dest at point.
(define (blit dest src region pt)
  (let ([rx (point-x (rect-origin region))]
        [ry (point-y (rect-origin region))]
        [rw (rect-width region)]
        [rh (rect-height region)]
        [dx (point-x pt)]
        [dy (point-y pt)])
       (let loop-y ([y 0] [canvas dest])
            (if (>= y rh)
                canvas
                (let loop-x ([x 0] [canvas canvas])
                     (if (>= x rw)
                         (loop-y (+ y 1) canvas)
                         (let ([ch (canvas-ref src (+ rx x) (+ ry y))])
                              (loop-x (+ x 1)
                                      (canvas-set canvas (+ dx x) (+ dy y) ch)))))))))

;;; ====
;;; Rendering
;;; ====

;;; canvas->string : Canvas → String
;;; Convert canvas to multi-line string.
;;; Rows separated by newline.
(define (canvas->string c)
  (let ([w (canvas-width c)]
        [h (canvas-height c)])
       (let loop ([y 0] [lines '()])
            (if (>= y h)
                (if (null? lines)
                    ""
                    (fold-left (lambda (acc line)
                                       (if (string=? acc "")
                                           line
                                           (string-append acc "\n" line)))
                               ""
                               (reverse lines)))
                (let ([line (let loop-x ([x 0] [chars '()])
                                 (if (>= x w)
                                     (list->string (reverse chars))
                                     (loop-x (+ x 1)
                                             (cons (canvas-ref c x y) chars))))])
                     (loop (+ y 1) (cons line lines)))))))

;;; ====
;;; String Utilities
;;; ====

;;; NOTE: string-pad-left, string-pad-right provided by core/prelude.ss

;;; string-pad : String × Nat × Char → String
;;; Pad string to target length with pad-char on the right.
;;; If string is already longer, return as-is.
;;; (Convenience wrapper around string-pad-right)
(define (string-pad str target-len pad-char)
  (string-pad-right str target-len pad-char))

;;; ====
;;; Box Drawing (Optional Enhancement)
;;; ====

;;; Box drawing styles
(define box-style-ascii
  '((tl . #\+) (tr . #\+) (bl . #\+) (br . #\+)
    (h . #\-) (v . #\|)))

(define box-style-light
  '((tl . #\┌) (tr . #\┐) (bl . #\└) (br . #\┘)
    (h . #\─) (v . #\│)))

(define box-style-heavy
  '((tl . #\┏) (tr . #\┓) (bl . #\┗) (br . #\┛)
    (h . #\━) (v . #\┃)))

(define box-style-double
  '((tl . #\╔) (tr . #\╗) (bl . #\╚) (br . #\╝)
    (h . #\═) (v . #\║)))

;;; get-box-style : Symbol → Alist | #f
(define (get-box-style style)
  (case style
        [(ascii) box-style-ascii]
        [(light) box-style-light]
        [(heavy) box-style-heavy]
        [(double) box-style-double]
        [else #f]))

;;; draw-box : Canvas × Rect × Symbol → Canvas
;;; Draw rectangle using Unicode box-drawing characters.
(define (draw-box c r style-name)
  (let ([style (get-box-style style-name)])
       (if (not style)
           (draw-rect c r #\#)  ; Fallback
           (let* ([ox (point-x (rect-origin r))]
                  [oy (point-y (rect-origin r))]
                  [w (rect-width r)]
                  [h (rect-height r)]
                  [right (+ ox w -1)]
                  [bottom (+ oy h -1)]
                  [tl (cdr (assq 'tl style))]
                  [tr (cdr (assq 'tr style))]
                  [bl (cdr (assq 'bl style))]
                  [br (cdr (assq 'br style))]
                  [hz (cdr (assq 'h style))]
                  [vt (cdr (assq 'v style))])
                 (if (or (<= w 1) (<= h 1))
                     c  ; Too small for corners
                     (let* ([canvas c]
                            ;; Draw corners
                            [canvas (canvas-set canvas ox oy tl)]
                            [canvas (canvas-set canvas right oy tr)]
                            [canvas (canvas-set canvas ox bottom bl)]
                            [canvas (canvas-set canvas right bottom br)]
                            ;; Draw horizontal edges
                            [canvas (let loop ([x (+ ox 1)] [canvas canvas])
                                         (if (>= x right)
                                             canvas
                                             (loop (+ x 1)
                                                   (canvas-set (canvas-set canvas x oy hz)
                                                               x bottom hz))))]
                            ;; Draw vertical edges
                            [canvas (let loop ([y (+ oy 1)] [canvas canvas])
                                         (if (>= y bottom)
                                             canvas
                                             (loop (+ y 1)
                                                   (canvas-set (canvas-set canvas ox y vt)
                                                               right y vt))))])
                           canvas))))))

;;; ====
;;; Text Flow (Wrapping, Alignment, Text Blocks)
;;; ====

;;; --- Word Splitting ---

;;; split-words : String → (List String)
;;; Split string into words (by spaces).
(define (split-words str)
  (let loop ([chars (string->list str)]
             [current '()]
             [words '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      words
                      (cons (list->string (reverse current)) words)))]
        [(char=? (car chars) #\space)
         (if (null? current)
             (loop (cdr chars) '() words)
             (loop (cdr chars) '()
                   (cons (list->string (reverse current)) words)))]
        [else
         (loop (cdr chars) (cons (car chars) current) words)])))

;;; --- Text Wrapping ---

;;; wrap-text : String × Nat → (List String)
;;; Break text at word boundaries to fit within given width.
;;; Words longer than width are not broken (placed on own line).
(define (wrap-text text width)
  (if (<= width 0)
      '()
      (let ([words (split-words text)])
           (if (null? words)
               '("")
               (let loop ([words words]
                          [current-line ""]
                          [lines '()])
                    (if (null? words)
                        (reverse (if (string=? current-line "")
                                     lines
                                     (cons current-line lines)))
                        (let* ([word (car words)]
                               [word-len (string-length word)]
                               [line-len (string-length current-line)]
                               [would-be (if (= line-len 0)
                                             word-len
                                             (+ line-len 1 word-len))])
                              (cond
                               ;; Empty line, just add word (even if too long)
                               [(= line-len 0)
                                (loop (cdr words) word lines)]
                               ;; Fits on current line
                               [(<= would-be width)
                                (loop (cdr words)
                                      (string-append current-line " " word)
                                      lines)]
                               ;; Doesn't fit, start new line
                               [else
                                (loop (cdr words)
                                      word
                                      (cons current-line lines))]))))))))

;;; --- Text Alignment ---

;;; align-left : String × Nat → String
;;; Left-align string, pad with spaces to reach width.
(define (align-left str width)
  (let ([len (string-length str)])
       (if (>= len width)
           (substring str 0 width)  ; Truncate if too long
           (string-append str (make-string (- width len) #\space)))))

;;; align-right : String × Nat → String
;;; Right-align string, pad with spaces on left.
(define (align-right str width)
  (let ([len (string-length str)])
       (if (>= len width)
           (substring str 0 width)
           (string-append (make-string (- width len) #\space) str))))

;;; align-center : String × Nat → String
;;; Center string, pad with spaces on both sides.
(define (align-center str width)
  (let* ([len (string-length str)]
         [total-pad (- width len)])
        (if (<= total-pad 0)
            (substring str 0 width)
            (let* ([left-pad (quotient total-pad 2)]
                   [right-pad (- total-pad left-pad)])
                  (string-append (make-string left-pad #\space)
                                 str
                                 (make-string right-pad #\space))))))

;;; --- Drawing Helpers ---

;;; draw-lines : Canvas × Point × (List String) → Canvas
;;; Draw multiple lines vertically, one per row starting at point.
(define (draw-lines c pt lines)
  (let ([x (point-x pt)]
        [y (point-y pt)])
       (let loop ([lines lines] [row 0] [canvas c])
            (if (null? lines)
                canvas
                (loop (cdr lines)
                      (+ row 1)
                      (draw-string canvas (point x (+ y row)) (car lines)))))))

;;; draw-text-block : Canvas × Rect × String × Symbol → Canvas
;;; Draw wrapped and aligned text within a rectangle.
;;; Alignment can be: 'left, 'right, 'center
(define (draw-text-block c rect text alignment)
  (let* ([ox (point-x (rect-origin rect))]
         [oy (point-y (rect-origin rect))]
         [w (rect-width rect)]
         [h (rect-height rect)]
         [lines (wrap-text text w)]
         [align-fn (case alignment
                         [(left) align-left]
                         [(right) align-right]
                         [(center) align-center]
                         [else align-left])]
         [aligned-lines (map (lambda (line) (align-fn line w)) lines)])
        ;; Only draw as many lines as fit in rect height
        (let loop ([lines aligned-lines] [row 0] [canvas c])
             (if (or (null? lines) (>= row h))
                 canvas
                 (loop (cdr lines)
                       (+ row 1)
                       (draw-string canvas (point ox (+ oy row)) (car lines)))))))

;;; draw-text-wrapped : Canvas × Point × String × Nat → Canvas
;;; Simple wrapped text drawing without alignment (left-aligned).
(define (draw-text-wrapped c pt text width)
  (draw-lines c pt (wrap-text text width)))
