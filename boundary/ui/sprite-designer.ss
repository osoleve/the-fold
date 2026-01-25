;;; boundary/ui/sprite-designer.ss — Interactive sprite designer
;;; @module sprite-designer
;;; @requires halfblock

(load "boundary/ui/halfblock.ss")

(doc 'module 'sprite-designer)
(doc 'description "Interactive ASCII-based sprite designer.
Create pixel art sprites using a text-based canvas with palette.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; ============================================================
;;; Palette Management
;;; ============================================================

(doc 'section 'palette)

(define *default-palette*
  `((#\. . ,color-default)           ; Transparent
    (#\# . ,(rgb 0 0 0))             ; Black
    (#\W . ,(rgb 255 255 255))       ; White
    (#\R . ,(rgb 255 0 0))           ; Red
    (#\G . ,(rgb 0 255 0))           ; Green
    (#\B . ,(rgb 0 0 255))           ; Blue
    (#\Y . ,(rgb 255 255 0))         ; Yellow
    (#\C . ,(rgb 0 255 255))         ; Cyan
    (#\M . ,(rgb 255 0 255))         ; Magenta
    (#\O . ,(rgb 255 128 0))         ; Orange
    (#\P . ,(rgb 128 0 255))         ; Purple
    (#\r . ,(rgb 128 0 0))           ; Dark red
    (#\g . ,(rgb 0 128 0))           ; Dark green
    (#\b . ,(rgb 0 0 128))           ; Dark blue
    (#\w . ,(rgb 192 192 192))       ; Light gray
    (#\x . ,(rgb 128 128 128))       ; Gray
    (#\d . ,(rgb 64 64 64))          ; Dark gray
    (#\S . ,(rgb 255 200 150))       ; Skin tone 1
    (#\s . ,(rgb 200 150 100))       ; Skin tone 2
    (#\H . ,(rgb 139 69 19))))       ; Brown (hair)

(define (display-palette palette)
  (doc 'description "Display available palette colors")
  (doc 'export #t)
  (display "Palette:\n")
  (for-each
   (lambda (entry)
     (let ([ch (car entry)]
           [color (cdr entry)])
       (if (color-default? color)
           (display (format "  ~a = transparent\n" ch))
           (display (format "  ~a = ~a~a  ~a\n"
                           ch
                           (ansi-bg color)
                           "  "
                           ansi-reset)))))
   palette)
  (newline))

;;; ============================================================
;;; Sprite Canvas
;;; ============================================================

(doc 'section 'canvas)

(define-record-type sprite-canvas%
  (fields width height chars palette))

(define (make-sprite-canvas width height)
  (doc 'type '(-> Int Int SpriteCanvas))
  (doc 'description "Create empty sprite canvas")
  (doc 'export #t)
  (make-sprite-canvas%
   width height
   (make-vector (* width height) #\.)
   *default-palette*))

(define (canvas-width c) (sprite-canvas%-width c))
(define (canvas-height c) (sprite-canvas%-height c))
(define (canvas-palette c) (sprite-canvas%-palette c))

(define (canvas-get c x y)
  (doc 'type '(-> SpriteCanvas Int Int Char))
  (if (and (>= x 0) (< x (canvas-width c))
           (>= y 0) (< y (canvas-height c)))
      (vector-ref (sprite-canvas%-chars c)
                  (+ x (* y (canvas-width c))))
      #\.))

(define (canvas-set! c x y ch)
  (doc 'type '(-> SpriteCanvas Int Int Char Void))
  (when (and (>= x 0) (< x (canvas-width c))
             (>= y 0) (< y (canvas-height c)))
    (vector-set! (sprite-canvas%-chars c)
                 (+ x (* y (canvas-width c)))
                 ch)))

(define (canvas-clear! c)
  (doc 'type '(-> SpriteCanvas Void))
  (doc 'description "Clear canvas to transparent")
  (doc 'export #t)
  (let ([chars (sprite-canvas%-chars c)]
        [len (* (canvas-width c) (canvas-height c))])
    (do ([i 0 (+ i 1)])
        ((>= i len))
      (vector-set! chars i #\.))))

;;; ============================================================
;;; Canvas Display
;;; ============================================================

(doc 'section 'display)

(define (canvas->text-grid c)
  (doc 'type '(-> SpriteCanvas String))
  (doc 'description "Convert canvas to text grid with coordinates")
  (let* ([w (canvas-width c)]
         [h (canvas-height c)]
         [parts '()])
    ;; Column headers
    (set! parts (cons "   " parts))
    (do ([x 0 (+ x 1)])
        ((>= x w))
      (set! parts (cons (format "~a" (modulo x 10)) parts)))
    (set! parts (cons "\n" parts))
    ;; Rows
    (do ([y 0 (+ y 1)])
        ((>= y h))
      (set! parts (cons (format "~2d " y) parts))
      (do ([x 0 (+ x 1)])
          ((>= x w))
        (set! parts (cons (string (canvas-get c x y)) parts)))
      (set! parts (cons "\n" parts)))
    (apply string-append (reverse parts))))

(define (display-canvas c)
  (doc 'type '(-> SpriteCanvas Void))
  (doc 'description "Display canvas as text grid")
  (doc 'export #t)
  (display (canvas->text-grid c)))

(define (canvas->pixel-buffer c)
  (doc 'type '(-> SpriteCanvas PixelBuffer))
  (doc 'description "Convert canvas to pixel buffer for rendering")
  (doc 'export #t)
  (let* ([w (canvas-width c)]
         [h (canvas-height c)]
         [palette (canvas-palette c)]
         [buf (make-pixel-buffer w h)])
    (do ([y 0 (+ y 1)])
        ((>= y h) buf)
      (do ([x 0 (+ x 1)])
          ((>= x w))
        (let* ([ch (canvas-get c x y)]
               [entry (assq ch palette)]
               [color (if entry (cdr entry) color-default)])
          (pixel-buffer-set! buf x y color))))))

(define (preview-canvas c)
  (doc 'type '(-> SpriteCanvas Void))
  (doc 'description "Preview canvas rendered as half-blocks")
  (doc 'export #t)
  (display "\nPreview:\n")
  (display-pixel-buffer (canvas->pixel-buffer c))
  (newline))

(define (preview-canvas-scaled c scale)
  (doc 'type '(-> SpriteCanvas Int Void))
  (doc 'description "Preview canvas at larger scale")
  (doc 'export #t)
  (let* ([src (canvas->pixel-buffer c)]
         [sw (pixel-buffer-width src)]
         [sh (pixel-buffer-height src)]
         [dst (make-pixel-buffer (* sw scale) (* sh scale))])
    (do ([y 0 (+ y 1)])
        ((>= y sh))
      (do ([x 0 (+ x 1)])
          ((>= x sw))
        (let ([color (pixel-buffer-ref src x y)])
          (pixel-buffer-fill-rect! dst (* x scale) (* y scale) scale scale color))))
    (display "\nPreview (scaled):\n")
    (display-pixel-buffer dst)
    (newline)))

;;; ============================================================
;;; Drawing Commands
;;; ============================================================

(doc 'section 'drawing)

(define (canvas-draw-row! c y row-string)
  (doc 'type '(-> SpriteCanvas Int String Void))
  (doc 'description "Draw a row from a string")
  (doc 'export #t)
  (let ([chars (string->list row-string)])
    (do ([x 0 (+ x 1)]
         [cs chars (if (null? cs) '() (cdr cs))])
        ((or (>= x (canvas-width c)) (null? cs)))
      (canvas-set! c x y (car cs)))))

(define (canvas-draw-rows! c start-y rows)
  (doc 'type '(-> SpriteCanvas Int (List String) Void))
  (doc 'description "Draw multiple rows from strings")
  (doc 'export #t)
  (do ([y start-y (+ y 1)]
       [rs rows (if (null? rs) '() (cdr rs))])
      ((or (>= y (canvas-height c)) (null? rs)))
    (canvas-draw-row! c y (car rs))))

(define (canvas-fill-rect! c x y w h ch)
  (doc 'type '(-> SpriteCanvas Int Int Int Int Char Void))
  (doc 'description "Fill rectangle with character")
  (doc 'export #t)
  (do ([dy 0 (+ dy 1)])
      ((>= dy h))
    (do ([dx 0 (+ dx 1)])
        ((>= dx w))
      (canvas-set! c (+ x dx) (+ y dy) ch))))

;;; ============================================================
;;; Export/Import
;;; ============================================================

(doc 'section 'export-import)

(define (canvas->sprite-code c name)
  (doc 'type '(-> SpriteCanvas Symbol String))
  (doc 'description "Generate Scheme code to recreate the sprite")
  (doc 'export #t)
  (let* ([w (canvas-width c)]
         [h (canvas-height c)]
         [rows '()])
    ;; Collect rows as strings
    (do ([y 0 (+ y 1)])
        ((>= y h))
      (let ([row-chars '()])
        (do ([x 0 (+ x 1)])
            ((>= x w))
          (set! row-chars (cons (canvas-get c x y) row-chars)))
        (set! rows (cons (list->string (reverse row-chars)) rows))))
    ;; Generate code
    (format "(define ~a\n  (define-sprite-from-strings ~a ~a\n    '(~a)\n    *default-palette*))\n"
            name w h
            (apply string-append
                   (map (lambda (r) (format "~s\n      " r))
                        (reverse rows))))))

(define (canvas->sexp c)
  (doc 'type '(-> SpriteCanvas SExpr))
  (doc 'description "Export canvas as S-expression for storage")
  (doc 'export #t)
  (let* ([w (canvas-width c)]
         [h (canvas-height c)]
         [rows '()])
    (do ([y 0 (+ y 1)])
        ((>= y h))
      (let ([row-chars '()])
        (do ([x 0 (+ x 1)])
            ((>= x w))
          (set! row-chars (cons (canvas-get c x y) row-chars)))
        (set! rows (cons (list->string (reverse row-chars)) rows))))
    `(sprite (width ,w) (height ,h) (rows ,@(reverse rows)))))

(define (sexp->canvas sexp)
  (doc 'type '(-> SExpr SpriteCanvas))
  (doc 'description "Import canvas from S-expression")
  (doc 'export #t)
  (let* ([w (cadr (assq 'width (cdr sexp)))]
         [h (cadr (assq 'height (cdr sexp)))]
         [rows (cdr (assq 'rows (cdr sexp)))]
         [c (make-sprite-canvas w h)])
    (canvas-draw-rows! c 0 rows)
    c))

;;; ============================================================
;;; Interactive Session
;;; ============================================================

(doc 'section 'interactive)

(define *current-canvas* #f)

(define (sprite-new width height)
  (doc 'description "Create new sprite canvas")
  (doc 'export #t)
  (set! *current-canvas* (make-sprite-canvas width height))
  (display (format "Created ~ax~a canvas\n" width height)))

(define (sprite-show)
  (doc 'description "Show current canvas as text grid")
  (doc 'export #t)
  (if *current-canvas*
      (display-canvas *current-canvas*)
      (display "No canvas. Use (sprite-new w h) first.\n")))

(define (sprite-preview)
  (doc 'description "Preview current canvas rendered")
  (doc 'export #t)
  (if *current-canvas*
      (preview-canvas *current-canvas*)
      (display "No canvas. Use (sprite-new w h) first.\n")))

(define (sprite-preview-2x)
  (doc 'description "Preview at 2x scale")
  (doc 'export #t)
  (if *current-canvas*
      (preview-canvas-scaled *current-canvas* 2)
      (display "No canvas.\n")))

(define (sprite-set x y ch)
  (doc 'description "Set pixel at (x,y) to char")
  (doc 'export #t)
  (if *current-canvas*
      (canvas-set! *current-canvas* x y ch)
      (display "No canvas.\n")))

(define (sprite-row y str)
  (doc 'description "Draw row from string")
  (doc 'export #t)
  (if *current-canvas*
      (canvas-draw-row! *current-canvas* y str)
      (display "No canvas.\n")))

(define (sprite-rows y . strs)
  (doc 'description "Draw multiple rows")
  (doc 'export #t)
  (if *current-canvas*
      (canvas-draw-rows! *current-canvas* y strs)
      (display "No canvas.\n")))

(define (sprite-clear)
  (doc 'description "Clear canvas")
  (doc 'export #t)
  (if *current-canvas*
      (canvas-clear! *current-canvas*)
      (display "No canvas.\n")))

(define (sprite-export name)
  (doc 'description "Export as Scheme code")
  (doc 'export #t)
  (if *current-canvas*
      (display (canvas->sprite-code *current-canvas* name))
      (display "No canvas.\n")))

(define (sprite-save filename)
  (doc 'description "Save canvas to file")
  (doc 'export #t)
  (if *current-canvas*
      (call-with-output-file filename
        (lambda (port)
          (write (canvas->sexp *current-canvas*) port)
          (newline port)))
      (display "No canvas.\n")))

(define (sprite-load filename)
  (doc 'description "Load canvas from file")
  (doc 'export #t)
  (call-with-input-file filename
    (lambda (port)
      (set! *current-canvas* (sexp->canvas (read port)))
      (display "Loaded.\n"))))

(define (sprite-palette)
  (doc 'description "Show palette")
  (doc 'export #t)
  (display-palette *default-palette*))

(define (sprite-help)
  (doc 'description "Show help")
  (doc 'export #t)
  (display "
Sprite Designer Commands:
  (sprite-new w h)        Create new canvas
  (sprite-show)           Show canvas as text
  (sprite-preview)        Render preview
  (sprite-preview-2x)     Render at 2x scale
  (sprite-set x y ch)     Set pixel
  (sprite-row y \"...\")    Draw row from string
  (sprite-rows y \"...\" ...)  Draw multiple rows
  (sprite-clear)          Clear canvas
  (sprite-palette)        Show color palette
  (sprite-export 'name)   Export as Scheme code
  (sprite-save \"file\")    Save to file
  (sprite-load \"file\")    Load from file

Palette chars: . (transparent), # W R G B Y C M O P r g b w x d S s H
"))

;;; ============================================================
;;; Example Sprites
;;; ============================================================

(doc 'section 'examples)

(define (demo-heart)
  (doc 'description "Demo: Create a heart sprite")
  (doc 'export #t)
  (sprite-new 9 8)
  (sprite-rows 0
    "..R.R.R.."
    ".RRRRRRR."
    "RRRRRRRRR"
    "RRRRRRRRR"
    ".RRRRRRR."
    "..RRRRR.."
    "...RRR..."
    "....R....")
  (sprite-show)
  (sprite-preview))

(define (demo-smiley)
  (doc 'description "Demo: Create a smiley face")
  (doc 'export #t)
  (sprite-new 8 8)
  (sprite-rows 0
    "..YYYY.."
    ".YYYYYY."
    "YY#YY#YY"
    "YYYYYYYY"
    "Y#YYYY#Y"
    "YY####YY"
    ".YYYYYY."
    "..YYYY..")
  (sprite-show)
  (sprite-preview))

(define (demo-navi)
  (doc 'description "Demo: Simple navi-like character")
  (doc 'export #t)
  (sprite-new 12 16)
  (sprite-rows 0
    "....BBBB...."
    "...BCCCBB..."
    "..BCCCCCCB.."
    "..BWCWWCWB.."
    "..BC#CC#CB.."
    "..BCCCCCCB.."
    "...BCCCCB..."
    "....BBBB...."
    "...BBBBBB..."
    "..BCCCCCCC.."
    ".BCCCCCCCCB."
    ".BCCCCCCCCB."
    "..BCCCCCCC.."
    "...BB..BB..."
    "..BB....BB.."
    ".BB......BB.")
  (sprite-show)
  (sprite-preview)
  (sprite-preview-2x))

(display "sprite-designer.ss loaded.\n")
(display "  (sprite-help)    — Show commands\n")
(display "  (sprite-palette) — Show color palette\n")
(display "  (demo-heart)     — Demo heart sprite\n")
(display "  (demo-smiley)    — Demo smiley face\n")
(display "  (demo-navi)      — Demo navi character\n")
