;;; shell/layout-color.ss — Color-Enabled Layout System
;;;
;;; Extended version of layout.ss with full color support.
;;; Load this instead of layout.ss for colored rendering.
;;;
;;; This is Shell code: handles display/IO with ANSI color output.

;;; ============================================================
;;; Dependencies
;;; ============================================================

;;; Load color support
(load "shell/ui/color.ss")

;;; ============================================================
;;; Data Structures
;;; ============================================================

;;; Canvas: A 2D grid of cells (char + color)
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
(define make-rect make-rect%)

;;; Re-export accessors
(define rect-origin rect%-origin)
(define rect-width rect%-width)
(define rect-height rect%-height)

;;; ============================================================
;;; Canvas Construction
;;; ============================================================

;;; make-canvas : Nat × Nat → Canvas
;;; Create a canvas filled with default cells.
(define (make-canvas width height)
  (let ([size (* width height)]
        [cells (make-vector (* width height) default-cell)])
       (make-canvas% width height cells)))

;;; canvas-width, canvas-height, canvas-cells
(define canvas-width canvas%-width)
(define canvas-height canvas%-height)
(define canvas-cells canvas%-cells)

;;; canvas-ref : Canvas × Nat × Nat → Cell
;;; Get cell at (x, y). Returns default-cell if out of bounds.
(define (canvas-ref c x y)
  (let ([w (canvas-width c)]
        [h (canvas-height c)])
       (if (or (< x 0) (< y 0) (>= x w) (>= y h))
           default-cell
           (vector-ref (canvas-cells c) (+ (* y w) x)))))

;;; canvas-ref-char : Canvas × Nat × Nat → Char
;;; Get just the character at (x, y).
(define (canvas-ref-char c x y)
  (cell%-char (canvas-ref c x y)))

;;; canvas-set : Canvas × Nat × Nat × Char → Canvas
;;; Set character with default colors.
(define (canvas-set c x y ch)
  (canvas-set-cell c x y (make-cell-simple ch)))

;;; canvas-set-cell : Canvas × Nat × Nat × Cell → Canvas
;;; Set cell at (x, y). Returns new canvas.
;;; NOTE: This is O(N) per call due to vector-copy. For bulk updates,
;;; use canvas-set-cells for batching.
(define (canvas-set-cell c x y cell)
  (let ([w (canvas-width c)]
        [h (canvas-height c)])
       (if (or (< x 0) (< y 0) (>= x w) (>= y h))
           c  ; Out of bounds, return unchanged
           (let ([cells (vector-copy (canvas-cells c))]
                 [idx (+ (* y w) x)])
                (vector-set! cells idx cell)
                (make-canvas% w h cells)))))

;;; canvas-set-cells : Canvas × (List (x y cell)) → Canvas
;;; Set multiple cells in a single copy operation.
;;; PERFORMANCE: Use this instead of repeated canvas-set-cell calls.
;;; Each update is (x y cell) triple. Out-of-bounds updates are ignored.
(define (canvas-set-cells c updates)
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
                           [cell (caddr update)])
                          (unless (or (< x 0) (< y 0) (>= x w) (>= y h))
                                  (vector-set! cells (+ (* y w) x) cell))))
             updates)
            (make-canvas% w h cells))))

;;; ============================================================
;;; Drawing Primitives
;;; ============================================================

;;; draw-char : Canvas × Point × Char → Canvas
(define (draw-char c pt ch)
  (canvas-set c (point-x pt) (point-y pt) ch))

;;; draw-char-colored : Canvas × Point × Char × Color × Color → Canvas
(define (draw-char-colored c pt ch fg bg)
  (canvas-set-cell c (point-x pt) (point-y pt) (make-cell ch fg bg)))

;;; draw-string : Canvas × Point × String → Canvas
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

;;; draw-string-colored : Canvas × Point × String × Color × Color → Canvas
(define (draw-string-colored c pt str fg bg)
  (let ([x (point-x pt)]
        [y (point-y pt)]
        [w (canvas-width c)]
        [len (string-length str)])
       (let loop ([i 0] [canvas c])
            (if (or (>= i len) (>= (+ x i) w))
                canvas
                (loop (+ i 1)
                      (canvas-set-cell canvas (+ x i) y
                                       (make-cell (string-ref str i) fg bg)))))))

;;; draw-string-v : Canvas × Point × String → Canvas
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
(define (draw-rect c r ch)
  (let* ([ox (point-x (rect-origin r))]
         [oy (point-y (rect-origin r))]
         [w (rect-width r)]
         [h (rect-height r)]
         [right (+ ox w -1)]
         [bottom (+ oy h -1)])
        (if (or (<= w 0) (<= h 0))
            c
            (let* ([canvas c]
                   [canvas (let loop ([x ox] [canvas canvas])
                                (if (>= x (+ ox w))
                                    canvas
                                    (loop (+ x 1) (canvas-set canvas x oy ch))))]
                   [canvas (let loop ([x ox] [canvas canvas])
                                (if (>= x (+ ox w))
                                    canvas
                                    (loop (+ x 1) (canvas-set canvas x bottom ch))))]
                   [canvas (let loop ([y oy] [canvas canvas])
                                (if (>= y (+ oy h))
                                    canvas
                                    (loop (+ y 1) (canvas-set canvas ox y ch))))]
                   [canvas (let loop ([y oy] [canvas canvas])
                                (if (>= y (+ oy h))
                                    canvas
                                    (loop (+ y 1) (canvas-set canvas right y ch))))])
                  canvas))))

;;; fill-rect : Canvas × Rect × Char → Canvas
(define (fill-rect c r ch)
  (let ([ox (point-x (rect-origin r))]
        [oy (point-y (rect-origin r))]
        [w (rect-width r)]
        [h (rect-height r)])
       (if (or (<= w 0) (<= h 0))
           c
           (let loop-y ([y oy] [canvas c])
                (if (>= y (+ oy h))
                    canvas
                    (let loop-x ([x ox] [canvas canvas])
                         (if (>= x (+ ox w))
                             (loop-y (+ y 1) canvas)
                             (loop-x (+ x 1) (canvas-set canvas x y ch)))))))))

;;; fill-rect-colored : Canvas × Rect × Char × Color × Color → Canvas
(define (fill-rect-colored c r ch fg bg)
  (let ([ox (point-x (rect-origin r))]
        [oy (point-y (rect-origin r))]
        [w (rect-width r)]
        [h (rect-height r)])
       (if (or (<= w 0) (<= h 0))
           c
           (let loop-y ([y oy] [canvas c])
                (if (>= y (+ oy h))
                    canvas
                    (let loop-x ([x ox] [canvas canvas])
                         (if (>= x (+ ox w))
                             (loop-y (+ y 1) canvas)
                             (loop-x (+ x 1)
                                     (canvas-set-cell canvas x y (make-cell ch fg bg))))))))))

;;; ============================================================
;;; Composition
;;; ============================================================

;;; composite : Canvas × Canvas × Point → Canvas
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
                         (let ([cell (canvas-ref src x y)])
                              (loop-x (+ x 1)
                                      (canvas-set-cell canvas (+ ox x) (+ oy y) cell)))))))))

;;; composite-with-transparency : Canvas × Canvas × Point × Char → Canvas
;;; Overlay source onto destination, skipping transparent characters.
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
                         (let ([cell (canvas-ref src x y)])
                              (if (char=? (cell%-char cell) transparent-char)
                                  (loop-x (+ x 1) canvas)  ; Skip transparent
                                  (loop-x (+ x 1)
                                          (canvas-set-cell canvas (+ ox x) (+ oy y) cell))))))))))

;;; blit : Canvas × Canvas × Rect × Point → Canvas
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
                         (let ([cell (canvas-ref src (+ rx x) (+ ry y))])
                              (loop-x (+ x 1)
                                      (canvas-set-cell canvas (+ dx x) (+ dy y) cell)))))))))

;;; ============================================================
;;; Rendering with ANSI Colors
;;; ============================================================

;;; canvas->string : Canvas → String
;;; Convert canvas to multi-line string with ANSI color codes.
(define (canvas->string c)
  (let ([w (canvas-width c)]
        [h (canvas-height c)])
       (let loop-y ([y 0] [lines '()])
            (if (>= y h)
                (if (null? lines)
                    ""
                    (string-append
                     (fold-left (lambda (acc line)
                                        (if (string=? acc "")
                                            line
                                            (string-append acc "\n" line)))
                                ""
                                (reverse lines))
                     ansi-reset))
                (let ([line (build-colored-line c y w)])
                     (loop-y (+ y 1) (cons line lines)))))))

;;; build-colored-line : Canvas × Nat × Nat → String
;;; Build a single line with ANSI codes, optimizing by only changing when needed.
(define (build-colored-line c y w)
  (let loop-x ([x 0]
               [result ""]
               [current-fg color-default]
               [current-bg color-default])
       (if (>= x w)
           result
           (let* ([cell (canvas-ref c x y)]
                  [ch (cell%-char cell)]
                  [fg (cell%-fg cell)]
                  [bg (cell%-bg cell)]
                  [fg-changed? (not (equal? fg current-fg))]
                  [bg-changed? (not (equal? bg current-bg))]
                  [color-code (cond
                               [(and fg-changed? bg-changed?)
                                (ansi-color fg bg)]
                               [fg-changed?
                                (ansi-fg fg)]
                               [bg-changed?
                                (ansi-bg bg)]
                               [else ""])]
                  [new-result (string-append result color-code (string ch))])
                 (loop-x (+ x 1) new-result fg bg)))))

;;; ============================================================
;;; Box Drawing
;;; ============================================================

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

(define (get-box-style style)
  (case style
        [(ascii) box-style-ascii]
        [(light) box-style-light]
        [(heavy) box-style-heavy]
        [(double) box-style-double]
        [else #f]))

;;; draw-box : Canvas × Rect × Symbol → Canvas
(define (draw-box c r style-name)
  (let ([style (get-box-style style-name)])
       (if (not style)
           (draw-rect c r #\#)
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
                     c
                     (let* ([canvas c]
                            [canvas (canvas-set canvas ox oy tl)]
                            [canvas (canvas-set canvas right oy tr)]
                            [canvas (canvas-set canvas ox bottom bl)]
                            [canvas (canvas-set canvas right bottom br)]
                            [canvas (let loop ([x (+ ox 1)] [canvas canvas])
                                         (if (>= x right)
                                             canvas
                                             (loop (+ x 1)
                                                   (canvas-set (canvas-set canvas x oy hz)
                                                               x bottom hz))))]
                            [canvas (let loop ([y (+ oy 1)] [canvas canvas])
                                         (if (>= y bottom)
                                             canvas
                                             (loop (+ y 1)
                                                   (canvas-set (canvas-set canvas ox y vt)
                                                               right y vt))))])
                           canvas))))))

;;; draw-box-colored : Canvas × Rect × Symbol × Color × Color → Canvas
(define (draw-box-colored c r style-name fg bg)
  (let ([style (get-box-style style-name)])
       (if (not style)
           (draw-char-colored c (point-x (rect-origin r)) (point-y (rect-origin r)) #\# fg bg)
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
                     c
                     (let* ([canvas c]
                            [canvas (canvas-set-cell canvas ox oy (make-cell tl fg bg))]
                            [canvas (canvas-set-cell canvas right oy (make-cell tr fg bg))]
                            [canvas (canvas-set-cell canvas ox bottom (make-cell bl fg bg))]
                            [canvas (canvas-set-cell canvas right bottom (make-cell br fg bg))]
                            [canvas (let loop ([x (+ ox 1)] [canvas canvas])
                                         (if (>= x right)
                                             canvas
                                             (loop (+ x 1)
                                                   (canvas-set-cell
                                                    (canvas-set-cell canvas x oy (make-cell hz fg bg))
                                                    x bottom (make-cell hz fg bg)))))]
                            [canvas (let loop ([y (+ oy 1)] [canvas canvas])
                                         (if (>= y bottom)
                                             canvas
                                             (loop (+ y 1)
                                                   (canvas-set-cell
                                                    (canvas-set-cell canvas ox y (make-cell vt fg bg))
                                                    right y (make-cell vt fg bg)))))])
                           canvas))))))
