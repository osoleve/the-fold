;;; thimble/color.ss — ANSI Color Support
;;;
;;; Color primitives for terminal rendering with ANSI escape sequences.
;;; Supports both 256-color and 24-bit truecolor modes.
;;;
;;; This is Shell code: handles display formatting (impure side).

;;; ============================================================
;;; Color Data Structure
;;; ============================================================

;;; Color : (+ 'default (rgb Nat Nat Nat) (palette Nat))
;;;
;;; Representation:
;;;   'default         — Terminal's default color
;;;   (rgb r g b)      — 24-bit truecolor (r,g,b each 0-255)
;;;   (palette n)      — 256-color palette index (n is 0-255)

;;; make-color-rgb : Nat × Nat × Nat → Color
;;; Create a truecolor RGB color.
(define (make-color-rgb r g b)
  (list 'rgb r g b))

;;; make-color-palette : Nat → Color
;;; Create a palette color (256-color mode).
(define (make-color-palette n)
  (list 'palette n))

;;; color-default : Color
;;; The terminal's default color.
(define color-default 'default)

;;; color-type : Color → Symbol
(define (color-type c)
  (if (list? c) (car c) c))

;;; Color predicates
(define (color-rgb? c)
  (and (list? c) (eq? (car c) 'rgb)))

(define (color-palette? c)
  (and (list? c) (eq? (car c) 'palette)))

(define (color-default? c)
  (eq? c 'default))

;;; ============================================================
;;; Color Constants (256-color palette)
;;; ============================================================

;;; Basic 16 ANSI colors (0-15)
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

;;; Extended colors (useful palette indices)
(define color-orange        (make-color-palette 208))
(define color-pink          (make-color-palette 213))
(define color-purple        (make-color-palette 141))
(define color-gold          (make-color-palette 220))
(define color-gray          (make-color-palette 244))
(define color-light-gray    (make-color-palette 250))
(define color-dark-gray     (make-color-palette 236))

;;; RGB convenience constructors
(define (rgb r g b)
  (make-color-rgb r g b))

;;; ============================================================
;;; ANSI Escape Code Generation
;;; ============================================================

;;; ansi-fg : Color → String
;;; Generate ANSI foreground color escape sequence.
(define (ansi-fg color)
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

;;; ansi-bg : Color → String
;;; Generate ANSI background color escape sequence.
(define (ansi-bg color)
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

;;; ansi-reset : String
;;; Reset all text attributes to default.
(define ansi-reset "\x1B;[0m")

;;; ansi-color : Color × Color → String
;;; Generate ANSI codes for both foreground and background.
(define (ansi-color fg bg)
  (string-append (ansi-fg fg) (ansi-bg bg)))

;;; ============================================================
;;; Cell Type — Character + Color
;;; ============================================================

;;; Cell : (× Char Color Color)
;;;
;;; A canvas cell containing:
;;;   - char : The character to display
;;;   - fg   : Foreground color
;;;   - bg   : Background color

(define-record-type cell%
  (fields char fg bg))

;;; make-cell : Char × Color × Color → Cell
(define make-cell make-cell%)

;;; cell-char : Cell → Char
;;; cell-fg : Cell → Color
;;; cell-bg : Cell → Color
;;; Auto-generated accessors

;;; default-cell : Cell
;;; A blank cell with default colors.
(define default-cell
  (make-cell #\space color-default color-default))

;;; make-cell-simple : Char → Cell
;;; Create a cell with default colors.
(define (make-cell-simple ch)
  (make-cell ch color-default color-default))

;;; ============================================================
;;; Color Helpers
;;; ============================================================

;;; lerp-color : Color × Color × Float → Color
;;; Linear interpolation between two RGB colors.
;;; t ranges from 0.0 (color1) to 1.0 (color2).
(define (lerp-color c1 c2 t)
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

;;; darken : Color × Float → Color
;;; Darken an RGB color by factor (0.0 = black, 1.0 = unchanged).
(define (darken color factor)
  (if (color-rgb? color)
      (let ([r (list-ref color 1)]
            [g (list-ref color 2)]
            [b (list-ref color 3)])
           (make-color-rgb
            (inexact->exact (round (* r factor)))
            (inexact->exact (round (* g factor)))
            (inexact->exact (round (* b factor)))))
      color))

;;; lighten : Color × Float → Color
;;; Lighten an RGB color by factor (0.0 = unchanged, 1.0 = white).
(define (lighten color factor)
  (if (color-rgb? color)
      (let ([r (list-ref color 1)]
            [g (list-ref color 2)]
            [b (list-ref color 3)])
           (make-color-rgb
            (inexact->exact (round (+ r (* (- 255 r) factor))))
            (inexact->exact (round (+ g (* (- 255 g) factor))))
            (inexact->exact (round (+ b (* (- 255 b) factor))))))
      color))

;;; ============================================================
;;; Mood Color Schemes
;;; ============================================================

;;; DUCKIE mood → color mapping
;;;
;;; Each mood has a signature color that expresses its emotional tone.

(define mood-colors
  `((happy    . ,(rgb 255 215 0))     ; Gold/yellow — bright and cheerful
    (curious  . ,(rgb 100 200 255))   ; Light blue — inquisitive
    (sleepy   . ,(rgb 180 180 220))   ; Soft purple — drowsy
    (content  . ,(rgb 150 220 150))   ; Soft green — peaceful
    (lonely   . ,(rgb 150 150 200))   ; Muted blue-gray — melancholy
    (playful  . ,(rgb 255 150 200)))) ; Pink — energetic and fun

;;; mood->color : Symbol → Color
;;; Get the signature color for a mood.
(define (mood->color mood)
  (let ([entry (assq mood mood-colors)])
       (if entry
           (cdr entry)
           color-default)))

;;; energy->color : Nat → Color
;;; Map energy level (0-100) to a color gradient.
;;; Low energy → dark blue, high energy → bright yellow.
(define (energy->color energy)
  (let* ([e (max 0 (min 100 energy))]  ; Clamp to 0-100
         [t (/ e 100.0)]                 ; Normalize to 0.0-1.0
         [low (rgb 80 80 150)]           ; Dark blue (low energy)
         [high (rgb 255 220 100)])       ; Bright yellow (high energy)
        (lerp-color low high t)))

;;; ============================================================
;;; Export Summary
;;; ============================================================

;;; Exports (implicitly available when loaded):
;;;
;;; Types:
;;;   - Cell (record type)
;;;
;;; Constructors:
;;;   - make-color-rgb, make-color-palette, rgb
;;;   - make-cell, make-cell-simple
;;;   - default-cell
;;;
;;; Constants:
;;;   - color-default
;;;   - color-black, color-red, color-green, color-yellow, ...
;;;   - color-orange, color-pink, color-purple, ...
;;;
;;; ANSI Codes:
;;;   - ansi-fg, ansi-bg, ansi-color, ansi-reset
;;;
;;; Helpers:
;;;   - lerp-color, darken, lighten
;;;   - mood->color, energy->color
