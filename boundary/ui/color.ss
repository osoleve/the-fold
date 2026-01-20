(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'color)
(doc 'description "ANSI Color Support - Color primitives for terminal rendering with ANSI escape sequences. Supports both 256-color and 24-bit truecolor modes.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'color-data-structure)

(doc 'note "Color : (+ 'default (rgb Nat Nat Nat) (palette Nat))")
(doc 'note "Representation:")
(doc 'note "  'default         — Terminal's default color")
(doc 'note "  (rgb r g b)      — 24-bit truecolor (r,g,b each 0-255)")
(doc 'note "  (palette n)      — 256-color palette index (n is 0-255)")

(define (make-color-rgb r g b)
  (doc 'type (-> Nat Nat Nat Color))
  (doc 'description "Create a truecolor RGB color.")
  (list 'rgb r g b))

(define (make-color-palette n)
  (doc 'type (-> Nat Color))
  (doc 'description "Create a palette color (256-color mode).")
  (list 'palette n))

(doc color-default 'type Color)
(doc color-default 'description "The terminal's default color.")
(define color-default 'default)

(define (color-type c)
  (doc 'type (-> Color Symbol))
  (if (list? c) (car c) c))

(doc 'section 'color-predicates)

(define (color-rgb? c)
  (doc 'type (-> Color Bool))
  (and (list? c) (eq? (car c) 'rgb)))

(define (color-palette? c)
  (doc 'type (-> Color Bool))
  (and (list? c) (eq? (car c) 'palette)))

(define (color-default? c)
  (doc 'type (-> Color Bool))
  (eq? c 'default))

(doc 'section 'color-constants)

(doc 'note "Basic 16 ANSI colors (0-15)")
(define color-black         (make-color-palette 0))
(define color-red           (make-color-palette 1))
(define color-green         (make-color-palette 2))
(define color-yellow        (make-color-palette 3))
(define color-blue          (make-color-palette 4))
(define color-magenta       (make-color-palette 5))
(define color-cyan          (make-color-palette 6))
(define color-white         (make-color-palette 7))
(define color-bright-black  (make-color-palette 8))
(define color-bright-red    (make-color-palette 9))
(define color-bright-green  (make-color-palette 10))
(define color-bright-yellow (make-color-palette 11))
(define color-bright-blue   (make-color-palette 12))
(define color-bright-magenta (make-color-palette 13))
(define color-bright-cyan   (make-color-palette 14))
(define color-bright-white  (make-color-palette 15))

(doc 'note "Extended colors (useful palette indices)")
(define color-orange        (make-color-palette 208))
(define color-pink          (make-color-palette 213))
(define color-purple        (make-color-palette 141))
(define color-gold          (make-color-palette 220))
(define color-gray          (make-color-palette 244))
(define color-light-gray    (make-color-palette 250))
(define color-dark-gray     (make-color-palette 236))

(doc 'note "RGB convenience constructors")
(define (rgb r g b)
  (doc 'type (-> Nat Nat Nat Color))
  (make-color-rgb r g b))

(doc 'section 'ansi-escape-codes)

(define (ansi-fg color)
  (doc 'type (-> Color String))
  (doc 'description "Generate ANSI foreground color escape sequence.")
  (cond
   [(color-default? color)
    "\x1B;[39m"]  ; Default foreground
   
   [(color-rgb? color)
    (let ([r (list-ref color 1)]
          [g (list-ref color 2)]
          [b (list-ref color 3)])
         (string-append "\x1B;[38;2;"
                        (number->string r) ";"
                        (number->string g) ";"
                        (number->string b) "m"))]
   
   [(color-palette? color)
    (let ([n (list-ref color 1)])
         (string-append "\x1B;[38;5;"
                        (number->string n) "m"))]

   [else "\x1B;[39m"]))  ; Fallback to default

(define (ansi-bg color)
  (doc 'type (-> Color String))
  (doc 'description "Generate ANSI background color escape sequence.")
  (cond
   [(color-default? color)
    "\x1B;[49m"]  ; Default background
   
   [(color-rgb? color)
    (let ([r (list-ref color 1)]
          [g (list-ref color 2)]
          [b (list-ref color 3)])
         (string-append "\x1B;[48;2;"
                        (number->string r) ";"
                        (number->string g) ";"
                        (number->string b) "m"))]
   
   [(color-palette? color)
    (let ([n (list-ref color 1)])
         (string-append "\x1B;[48;5;"
                        (number->string n) "m"))]

   [else "\x1B;[49m"]))  ; Fallback to default

(doc ansi-reset 'type String)
(doc ansi-reset 'description "Reset all text attributes to default.")
(define ansi-reset "\x1B;[0m")

(define (ansi-color fg bg)
  (doc 'type (-> Color Color String))
  (doc 'description "Generate ANSI codes for both foreground and background.")
  (string-append (ansi-fg fg) (ansi-bg bg)))

(doc 'section 'cell-type)

(doc 'note "Cell : (× Char Color Color)")
(doc 'note "A canvas cell containing:")
(doc 'note "  - char : The character to display")
(doc 'note "  - fg   : Foreground color")
(doc 'note "  - bg   : Background color")

(define-record-type cell%
  (fields char fg bg))

(doc make-cell 'type (-> Char Color Color Cell))
(define make-cell make-cell%)

(doc 'note "cell-char : Cell → Char")
(doc 'note "cell-fg : Cell → Color")
(doc 'note "cell-bg : Cell → Color")
(doc 'note "Auto-generated accessors")

(doc default-cell 'type Cell)
(doc default-cell 'description "A blank cell with default colors.")
(define default-cell
  (make-cell #\space color-default color-default))

(define (make-cell-simple ch)
  (doc 'type (-> Char Cell))
  (doc 'description "Create a cell with default colors.")
  (make-cell ch color-default color-default))

(doc 'section 'color-helpers)

(define (lerp-color c1 c2 t)
  (doc 'type (-> Color Color Float Color))
  (doc 'description "Linear interpolation between two RGB colors. t ranges from 0.0 (color1) to 1.0 (color2).")
  (cond
   [(or (not (color-rgb? c1)) (not (color-rgb? c2)))
    c1]  ; Can't lerp non-RGB colors
   
   [else
    (let* ([r1 (list-ref c1 1)]
           [g1 (list-ref c1 2)]
           [b1 (list-ref c1 3)]
           [r2 (list-ref c2 1)]
           [g2 (list-ref c2 2)]
           [b2 (list-ref c2 3)]
           [lerp (lambda (a b t)
                         (inexact->exact
                          (round (+ a (* (- b a) t)))))])
          (make-color-rgb
           (lerp r1 r2 t)
           (lerp g1 g2 t)
           (lerp b1 b2 t)))]))


(define (darken color factor)
  (doc 'type (-> Color Float Color))
  (doc 'description "Darken an RGB color by factor (0.0 = black, 1.0 = unchanged).")
  (if (color-rgb? color)
      (let ([r (list-ref color 1)]
            [g (list-ref color 2)]
            [b (list-ref color 3)])
           (make-color-rgb
            (inexact->exact (round (* r factor)))
            (inexact->exact (round (* g factor)))
            (inexact->exact (round (* b factor)))))
      color))

(define (lighten color factor)
  (doc 'type (-> Color Float Color))
  (doc 'description "Lighten an RGB color by factor (0.0 = unchanged, 1.0 = white).")
  (if (color-rgb? color)
      (let ([r (list-ref color 1)]
            [g (list-ref color 2)]
            [b (list-ref color 3)])
           (make-color-rgb
            (inexact->exact (round (+ r (* (- 255 r) factor))))
            (inexact->exact (round (+ g (* (- 255 g) factor))))
            (inexact->exact (round (+ b (* (- 255 b) factor))))))
      color))

(doc 'section 'mood-color-schemes)

(doc 'note "DUCKIE mood → color mapping")
(doc 'note "Each mood has a signature color that expresses its emotional tone.")

(define mood-colors
  `((happy    . ,(rgb 255 215 0))     ; Gold/yellow — bright and cheerful
    (curious  . ,(rgb 100 200 255))   ; Light blue — inquisitive
    (sleepy   . ,(rgb 180 180 220))   ; Soft purple — drowsy
    (content  . ,(rgb 150 220 150))   ; Soft green — peaceful
    (lonely   . ,(rgb 150 150 200))   ; Muted blue-gray — melancholy
    (playful  . ,(rgb 255 150 200)))) ; Pink — energetic and fun

(define (mood->color mood)
  (doc 'type (-> Symbol Color))
  (doc 'description "Get the signature color for a mood.")
  (let ([entry (assq mood mood-colors)])
       (if entry
           (cdr entry)
           color-default)))

(define (energy->color energy)
  (doc 'type (-> Nat Color))
  (doc 'description "Map energy level (0-100) to a color gradient. Low energy → dark blue, high energy → bright yellow.")
  (let* ([e (max 0 (min 100 energy))]  ; Clamp to 0-100
         [t (/ e 100.0)]                 ; Normalize to 0.0-1.0
         [low (rgb 80 80 150)]           ; Dark blue (low energy)
         [high (rgb 255 220 100)])       ; Bright yellow (high energy)
        (lerp-color low high t)))

(doc 'section 'exports)

(doc 'note "Exports (implicitly available when loaded):")
(doc 'note "Types: Cell (record type)")
(doc 'note "Constructors: make-color-rgb, make-color-palette, rgb, make-cell, make-cell-simple, default-cell")
(doc 'note "Constants: color-default, color-black, color-red, color-green, color-yellow, ...")
(doc 'note "ANSI Codes: ansi-fg, ansi-bg, ansi-color, ansi-reset")
(doc 'note "Helpers: lerp-color, darken, lighten, mood->color, energy->color")
