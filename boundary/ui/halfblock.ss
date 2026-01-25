;;; boundary/ui/halfblock.ss — Half-block terminal pixel rendering
;;; @module halfblock
;;; @requires color

(load "boundary/ui/color.ss")

(doc 'module 'halfblock)
(doc 'description "Terminal pixel rendering using Unicode half-block characters.
Each terminal cell displays 2 vertical pixels using ▀▄█ characters.
Supports 24-bit color for game graphics, sprites, and pixel art.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; ============================================================
;;; Pixel Buffer
;;; ============================================================

(doc 'section 'pixel-buffer)

(doc 'note "A PixelBuffer is a 2D array of colors with width × height pixels")

(define-record-type pixel-buffer%
  (fields width height pixels))

(define make-pixel-buffer
  (case-lambda
    [(width height)
     (make-pixel-buffer width height color-default)]
    [(width height default-color)
     (doc 'type '(-> Int Int [Color] PixelBuffer))
     (doc 'description "Create a pixel buffer with given dimensions")
     (doc 'export #t)
     (let ([pixels (make-vector (* width height) default-color)])
       (make-pixel-buffer% width height pixels))]))

(define (pixel-buffer-width buf) (pixel-buffer%-width buf))
(define (pixel-buffer-height buf) (pixel-buffer%-height buf))

(define (pixel-buffer-ref buf x y)
  (doc 'type '(-> PixelBuffer Int Int Color))
  (doc 'description "Get pixel color at (x, y)")
  (doc 'export #t)
  (if (and (>= x 0) (< x (pixel-buffer-width buf))
           (>= y 0) (< y (pixel-buffer-height buf)))
      (vector-ref (pixel-buffer%-pixels buf)
                  (+ x (* y (pixel-buffer-width buf))))
      color-default))

(define (pixel-buffer-set! buf x y color)
  (doc 'type '(-> PixelBuffer Int Int Color Void))
  (doc 'description "Set pixel color at (x, y)")
  (doc 'export #t)
  (when (and (>= x 0) (< x (pixel-buffer-width buf))
             (>= y 0) (< y (pixel-buffer-height buf)))
    (vector-set! (pixel-buffer%-pixels buf)
                 (+ x (* y (pixel-buffer-width buf)))
                 color)))

(define pixel-buffer-clear!
  (case-lambda
    [(buf) (pixel-buffer-clear! buf color-default)]
    [(buf color)
     (doc 'type '(-> PixelBuffer [Color] Void))
     (doc 'description "Clear buffer to a single color")
     (doc 'export #t)
     (let ([pixels (pixel-buffer%-pixels buf)]
           [len (* (pixel-buffer-width buf) (pixel-buffer-height buf))])
       (do ([i 0 (+ i 1)])
           ((>= i len))
         (vector-set! pixels i color)))]))

(define (pixel-buffer-copy buf)
  (doc 'type '(-> PixelBuffer PixelBuffer))
  (doc 'description "Create a copy of the pixel buffer")
  (doc 'export #t)
  (let* ([w (pixel-buffer-width buf)]
         [h (pixel-buffer-height buf)]
         [new-buf (make-pixel-buffer w h)]
         [src (pixel-buffer%-pixels buf)]
         [dst (pixel-buffer%-pixels new-buf)])
    (do ([i 0 (+ i 1)])
        ((>= i (* w h)) new-buf)
      (vector-set! dst i (vector-ref src i)))))

;;; ============================================================
;;; Drawing Primitives
;;; ============================================================

(doc 'section 'drawing)

(define (pixel-buffer-fill-rect! buf x y w h color)
  (doc 'type '(-> PixelBuffer Int Int Int Int Color Void))
  (doc 'description "Fill a rectangle with a color")
  (doc 'export #t)
  (do ([dy 0 (+ dy 1)])
      ((>= dy h))
    (do ([dx 0 (+ dx 1)])
        ((>= dx w))
      (pixel-buffer-set! buf (+ x dx) (+ y dy) color))))

(define (pixel-buffer-draw-line! buf x0 y0 x1 y1 color)
  (doc 'type '(-> PixelBuffer Int Int Int Int Color Void))
  (doc 'description "Draw a line using Bresenham's algorithm")
  (doc 'export #t)
  (let* ([dx (abs (- x1 x0))]
         [dy (- (abs (- y1 y0)))]
         [sx (if (< x0 x1) 1 -1)]
         [sy (if (< y0 y1) 1 -1)]
         [err (+ dx dy)])
    (let loop ([x x0] [y y0] [e err])
      (pixel-buffer-set! buf x y color)
      (unless (and (= x x1) (= y y1))
        (let ([e2 (* 2 e)])
          (let-values ([(new-x new-e-x)
                        (if (>= e2 dy)
                            (values (+ x sx) (+ e dy))
                            (values x e))]
                       [(new-y new-e-y)
                        (if (<= e2 dx)
                            (values (+ y sy) (+ e dx))
                            (values y e))])
            (loop new-x new-y (+ (- new-e-x e) (- new-e-y e) e))))))))

(define (pixel-buffer-draw-circle! buf cx cy r color)
  (doc 'type '(-> PixelBuffer Int Int Int Color Void))
  (doc 'description "Draw a circle outline using midpoint algorithm")
  (doc 'export #t)
  (let loop ([x 0] [y r] [d (- 1 r)])
    (when (<= x y)
      ;; Draw 8 octants
      (pixel-buffer-set! buf (+ cx x) (+ cy y) color)
      (pixel-buffer-set! buf (+ cx y) (+ cy x) color)
      (pixel-buffer-set! buf (+ cx y) (- cy x) color)
      (pixel-buffer-set! buf (+ cx x) (- cy y) color)
      (pixel-buffer-set! buf (- cx x) (- cy y) color)
      (pixel-buffer-set! buf (- cx y) (- cy x) color)
      (pixel-buffer-set! buf (- cx y) (+ cy x) color)
      (pixel-buffer-set! buf (- cx x) (+ cy y) color)
      (if (< d 0)
          (loop (+ x 1) y (+ d (* 2 x) 3))
          (loop (+ x 1) (- y 1) (+ d (* 2 (- x y)) 5))))))

(define (pixel-buffer-fill-circle! buf cx cy r color)
  (doc 'type '(-> PixelBuffer Int Int Int Color Void))
  (doc 'description "Fill a circle with a color")
  (doc 'export #t)
  (do ([y (- cy r) (+ y 1)])
      ((> y (+ cy r)))
    (do ([x (- cx r) (+ x 1)])
        ((> x (+ cx r)))
      (let ([dx (- x cx)]
            [dy (- y cy)])
        (when (<= (+ (* dx dx) (* dy dy)) (* r r))
          (pixel-buffer-set! buf x y color))))))

;;; ============================================================
;;; Half-Block Rendering
;;; ============================================================

(doc 'section 'halfblock-render)

(doc 'note "Half-block characters for 2:1 vertical pixel ratio")
(define halfblock-upper "▀")  ; Upper half block (U+2580)
(define halfblock-lower "▄")  ; Lower half block (U+2584)
(define halfblock-full  "█")  ; Full block (U+2588)

(define (colors-equal? c1 c2)
  (doc 'type '(-> Color Color Bool))
  (equal? c1 c2))

(define (render-halfblock-cell top-color bottom-color)
  (doc 'type '(-> Color Color String))
  (doc 'description "Render two vertical pixels as one terminal character")
  ;; Always reset first to prevent color state leakage between cells
  (string-append
   ansi-reset
   (cond
     ;; Both same color: full block (or space if default)
     [(colors-equal? top-color bottom-color)
      (if (color-default? top-color)
          " "
          (string-append (ansi-fg top-color) halfblock-full))]

     ;; Top has color, bottom is default: upper half with fg
     [(color-default? bottom-color)
      (string-append (ansi-fg top-color) halfblock-upper)]

     ;; Bottom has color, top is default: lower half with fg
     [(color-default? top-color)
      (string-append (ansi-fg bottom-color) halfblock-lower)]

     ;; Both have different colors: upper half with fg=top, bg=bottom
     [else
      (string-append (ansi-fg top-color) (ansi-bg bottom-color) halfblock-upper)])))

(define (pixel-buffer->string buf)
  (doc 'type '(-> PixelBuffer String))
  (doc 'description "Render pixel buffer to string using half-block characters")
  (doc 'export #t)
  (let* ([w (pixel-buffer-width buf)]
         [h (pixel-buffer-height buf)]
         [rows (quotient (+ h 1) 2)]  ; Ceiling division
         [parts '()])
    (do ([row 0 (+ row 1)])
        ((>= row rows)
         (apply string-append (reverse parts)))
      (let ([y-top (* row 2)]
            [y-bot (+ (* row 2) 1)])
        ;; Build one row
        (do ([x 0 (+ x 1)])
            ((>= x w))
          (let ([top (pixel-buffer-ref buf x y-top)]
                [bot (if (< y-bot h)
                         (pixel-buffer-ref buf x y-bot)
                         color-default)])
            (set! parts (cons (render-halfblock-cell top bot) parts))))
        ;; Reset and newline
        (set! parts (cons (string-append ansi-reset "\n") parts))))))

(define (display-pixel-buffer buf)
  (doc 'type '(-> PixelBuffer Void))
  (doc 'description "Display pixel buffer to terminal")
  (doc 'export #t)
  (display (pixel-buffer->string buf)))

;;; ============================================================
;;; Sprite Support
;;; ============================================================

(doc 'section 'sprites)

(doc 'note "A Sprite is a small pixel buffer that can be drawn onto another buffer")

(define (make-sprite width height pixels)
  (doc 'type '(-> Int Int (Vector Color) Sprite))
  (doc 'description "Create a sprite from a vector of colors (row-major)")
  (doc 'export #t)
  (make-pixel-buffer% width height pixels))

(define (pixel-buffer-blit! dest src dest-x dest-y)
  (doc 'type '(-> PixelBuffer PixelBuffer Int Int Void))
  (doc 'description "Draw source buffer onto dest at (dest-x, dest-y)")
  (doc 'export #t)
  (let ([sw (pixel-buffer-width src)]
        [sh (pixel-buffer-height src)])
    (do ([sy 0 (+ sy 1)])
        ((>= sy sh))
      (do ([sx 0 (+ sx 1)])
          ((>= sx sw))
        (let ([color (pixel-buffer-ref src sx sy)])
          (unless (color-default? color)  ; Transparent
            (pixel-buffer-set! dest (+ dest-x sx) (+ dest-y sy) color)))))))

(define (pixel-buffer-blit-keyed! dest src dest-x dest-y transparent-color)
  (doc 'type '(-> PixelBuffer PixelBuffer Int Int Color Void))
  (doc 'description "Blit with custom transparency color")
  (doc 'export #t)
  (let ([sw (pixel-buffer-width src)]
        [sh (pixel-buffer-height src)])
    (do ([sy 0 (+ sy 1)])
        ((>= sy sh))
      (do ([sx 0 (+ sx 1)])
          ((>= sx sw))
        (let ([color (pixel-buffer-ref src sx sy)])
          (unless (colors-equal? color transparent-color)
            (pixel-buffer-set! dest (+ dest-x sx) (+ dest-y sy) color)))))))

;;; ============================================================
;;; Sprite Definition DSL
;;; ============================================================

(doc 'section 'sprite-dsl)

(define (parse-sprite-row row-string palette)
  (doc 'type '(-> String Alist (Vector Color)))
  (doc 'description "Parse a sprite row string using palette lookup")
  (list->vector
   (map (lambda (ch)
          (let ([entry (assq ch palette)])
            (if entry (cdr entry) color-default)))
        (string->list row-string))))

(define (define-sprite-from-strings width height rows palette)
  (doc 'type '(-> Int Int (List String) Alist Sprite))
  (doc 'description "Create sprite from ASCII art strings and color palette")
  (doc 'export #t)
  (let* ([pixels (make-vector (* width height) color-default)]
         [buf (make-pixel-buffer% width height pixels)])
    (do ([y 0 (+ y 1)]
         [remaining rows (if (null? remaining) '() (cdr remaining))])
        ((or (>= y height) (null? remaining)) buf)
      (let* ([row (car remaining)]
             [chars (string->list row)])
        (do ([x 0 (+ x 1)]
             [cs chars (if (null? cs) '() (cdr cs))])
            ((or (>= x width) (null? cs)))
          (let* ([ch (car cs)]
                 [entry (assq ch palette)]
                 [color (if entry (cdr entry) color-default)])
            (pixel-buffer-set! buf x y color)))))))

;;; ============================================================
;;; Animation Support
;;; ============================================================

(doc 'section 'animation)

(define-record-type animation%
  (fields frames frame-duration current-frame elapsed))

(define (make-animation frames frame-duration)
  (doc 'type '(-> (Vector Sprite) Number Animation))
  (doc 'description "Create an animation from frames with ms per frame")
  (doc 'export #t)
  (make-animation% frames frame-duration 0 0))

(define (animation-current-sprite anim)
  (doc 'type '(-> Animation Sprite))
  (doc 'description "Get current frame's sprite")
  (doc 'export #t)
  (vector-ref (animation%-frames anim) (animation%-current-frame anim)))

(define (animation-update anim dt)
  (doc 'type '(-> Animation Number Animation))
  (doc 'description "Advance animation by dt milliseconds, returns new animation")
  (doc 'export #t)
  (let* ([elapsed (+ (animation%-elapsed anim) dt)]
         [duration (animation%-frame-duration anim)]
         [frames (animation%-frames anim)]
         [num-frames (vector-length frames)])
    (if (>= elapsed duration)
        (make-animation% frames duration
                        (modulo (+ (animation%-current-frame anim) 1) num-frames)
                        (- elapsed duration))
        (make-animation% frames duration
                        (animation%-current-frame anim)
                        elapsed))))

;;; ============================================================
;;; Cursor Control
;;; ============================================================

(doc 'section 'cursor)

(define (ansi-cursor-home)
  (doc 'type '(-> String))
  (doc 'description "Move cursor to top-left")
  "\x1B;[H")

(define (ansi-cursor-move x y)
  (doc 'type '(-> Int Int String))
  (doc 'description "Move cursor to (x, y) - 1-indexed")
  (string-append "\x1B;[" (number->string y) ";" (number->string x) "H"))

(define (ansi-clear-screen)
  (doc 'type '(-> String))
  (doc 'description "Clear entire screen")
  "\x1B;[2J")

(define (ansi-hide-cursor)
  (doc 'type '(-> String))
  "\x1B;[?25l")

(define (ansi-show-cursor)
  (doc 'type '(-> String))
  "\x1B;[?25h")

(define (display-pixel-buffer-at buf x y)
  (doc 'type '(-> PixelBuffer Int Int Void))
  (doc 'description "Display buffer at screen position (1-indexed)")
  (doc 'export #t)
  (display (ansi-cursor-move x y))
  (display (pixel-buffer->string buf)))

;;; ============================================================
;;; Example / Demo
;;; ============================================================

(define (halfblock-demo)
  (doc 'description "Demo of half-block rendering")
  (doc 'export #t)
  (let ([buf (make-pixel-buffer 32 16)])
    ;; Draw some shapes
    (pixel-buffer-fill-rect! buf 2 2 8 4 (rgb 255 100 100))   ; Red rect
    (pixel-buffer-fill-rect! buf 12 2 8 4 (rgb 100 255 100))  ; Green rect
    (pixel-buffer-fill-rect! buf 22 2 8 4 (rgb 100 100 255))  ; Blue rect

    ;; Draw circles
    (pixel-buffer-fill-circle! buf 8 11 4 (rgb 255 200 0))    ; Yellow
    (pixel-buffer-draw-circle! buf 20 11 4 (rgb 255 255 255)) ; White outline

    ;; Draw line
    (pixel-buffer-draw-line! buf 0 0 31 15 (rgb 255 0 255))   ; Magenta diagonal

    (display-pixel-buffer buf)))

(display "halfblock.ss loaded.\n")
(display "  (make-pixel-buffer w h)        — Create pixel buffer\n")
(display "  (pixel-buffer-set! buf x y c)  — Set pixel\n")
(display "  (display-pixel-buffer buf)     — Render to terminal\n")
(display "  (halfblock-demo)               — Run demo\n")
