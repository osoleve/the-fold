;;; shell/turtle-color.ss — 12-bit Color System for Turtle Graphics
;;;
;;; Provides a 4096-color palette (4 bits per RGB channel) for turtle graphics.
;;; Supports multiple color specification formats:
;;;   - RGB triplet: (make-color12 15 0 0) or (color12 15 0 0)
;;;   - Integer: (color12-from-int #xF00)
;;;   - Named: (color12-from-name 'red)
;;;
;;; This is Shell code: pure functions for color manipulation.
;;;
;;; Dependencies: None

;;; ============================================================
;;; Color12 Data Structure
;;; ============================================================

;;; Color12 = (color12 r g b) where 0 <= r,g,b <= 15
;;;
;;; Representation uses tagged list for consistency with shell/color.ss pattern.

;;; make-color12 : Nat x Nat x Nat -> Color12
;;; Create a 12-bit color from RGB components (0-15 each).
(define (make-color12 r g b)
  (list 'color12
        (max 0 (min 15 r))
        (max 0 (min 15 g))
        (max 0 (min 15 b))))

;;; color12? : Any -> Bool
;;; Test if value is a Color12.
(define (color12? c)
  (and (list? c)
       (= (length c) 4)
       (eq? (car c) 'color12)))

;;; Accessors
(define (color12-r c) (list-ref c 1))
(define (color12-g c) (list-ref c 2))
(define (color12-b c) (list-ref c 3))

;;; color12->list : Color12 -> (List Nat Nat Nat)
;;; Extract RGB as a list.
(define (color12->list c)
  (list (color12-r c) (color12-g c) (color12-b c)))

;;; list->color12 : (List Nat Nat Nat) -> Color12
;;; Create Color12 from a list.
(define (list->color12 lst)
  (make-color12 (car lst) (cadr lst) (caddr lst)))

;;; ============================================================
;;; Color Conversions
;;; ============================================================

;;; color12-from-int : Nat -> Color12
;;; Create Color12 from integer 0-4095 (#x000 to #xFFF).
;;; Format: #xRGB where R, G, B are each 0-15.
(define (color12-from-int n)
  (let* ([clamped (max 0 (min #xFFF n))]
         [r (bitwise-arithmetic-shift-right clamped 8)]
         [g (bitwise-and (bitwise-arithmetic-shift-right clamped 4) #xF)]
         [b (bitwise-and clamped #xF)])
        (make-color12 r g b)))

;;; color12->int : Color12 -> Nat
;;; Convert Color12 to integer 0-4095.
(define (color12->int c)
  (+ (bitwise-arithmetic-shift-left (color12-r c) 8)
     (bitwise-arithmetic-shift-left (color12-g c) 4)
     (color12-b c)))

;;; color12->svg-hex : Color12 -> String
;;; Convert to SVG hex string "#RRGGBB" (scaled from 4-bit to 8-bit).
;;; Each 4-bit value (0-15) maps to 8-bit (0-255) by: value * 17.
(define (color12->svg-hex c)
  (let* ([scale (lambda (v) (* v 17))]  ; 0->0, 15->255
         [r (scale (color12-r c))]
         [g (scale (color12-g c))]
         [b (scale (color12-b c))])
        (string-append "#"
                       (hex-byte r)
                       (hex-byte g)
                       (hex-byte b))))

;;; hex-byte : Nat -> String
;;; Convert byte (0-255) to two-character hex string.
(define (hex-byte n)
  (let* ([hi (bitwise-arithmetic-shift-right n 4)]
         [lo (bitwise-and n #xF)])
        (string (hex-digit hi) (hex-digit lo))))

;;; hex-digit : Nat -> Char
;;; Convert 0-15 to hex digit.
(define (hex-digit n)
  (if (< n 10)
      (integer->char (+ n (char->integer #\0)))
      (integer->char (+ (- n 10) (char->integer #\A)))))

;;; ============================================================
;;; Named Color Palette
;;; ============================================================

;;; *color12-palette* : Alist of (Symbol . (r g b))
;;; Standard named colors for 12-bit space.
(define *color12-palette*
  '((black   .  (0  0  0))
    (white   . (15 15 15))
    (red     . (15  0  0))
    (green   .  (0 15  0))
    (blue    .  (0  0 15))
    (yellow  . (15 15  0))
    (cyan    .  (0 15 15))
    (magenta . (15  0 15))
    (orange  . (15  8  0))
    (pink    . (15 10 12))
    (purple  . (10  0 15))
    (brown   .  (8  4  0))
    (gray    .  (8  8  8))
    (grey    .  (8  8  8))
    (lime    .  (8 15  0))
    (navy    .  (0  0  8))
    (teal    .  (0  8  8))
    (maroon  .  (8  0  0))
    (olive   .  (8  8  0))
    (silver  . (12 12 12))
    (gold    . (15 13  0))))

;;; color12-from-name : Symbol -> Color12 | #f
;;; Look up a named color. Returns #f if not found.
(define (color12-from-name name)
  (let ([entry (assq name *color12-palette*)])
       (if entry
           (apply make-color12 (cdr entry))
           #f)))

;;; color12-names : -> (List Symbol)
;;; Return list of all named colors.
(define (color12-names)
  (map car *color12-palette*))

;;; ============================================================
;;; Unified Color Parser
;;; ============================================================

;;; parse-color12 : ColorSpec -> Color12 | #f
;;; Parse a color specification. Accepts:
;;;   - Color12 value (passthrough)
;;;   - List (r g b) triplet with 0-15 values
;;;   - Integer 0-4095
;;;   - Symbol naming a color
;;; Returns #f if invalid.
(define (parse-color12 spec)
  (cond
   ;; Already a Color12
   [(color12? spec) spec]
   
   ;; RGB triplet as list
   [(and (list? spec)
         (= (length spec) 3)
         (for-all integer? spec))
    (apply make-color12 spec)]
   
   ;; Integer 0-4095
   [(and (integer? spec) (>= spec 0) (<= spec #xFFF))
    (color12-from-int spec)]
   
   ;; Named color
   [(symbol? spec)
    (color12-from-name spec)]
   
   [else #f]))

;;; ============================================================
;;; Color Operations
;;; ============================================================

;;; color12-lerp : Color12 x Color12 x Real -> Color12
;;; Linear interpolation between two colors.
;;; t=0 returns c1, t=1 returns c2.
(define (color12-lerp c1 c2 t)
  (let* ([lerp-channel (lambda (a b)
                               (inexact->exact (round (+ a (* (- b a) t)))))]
         [r (lerp-channel (color12-r c1) (color12-r c2))]
         [g (lerp-channel (color12-g c1) (color12-g c2))]
         [b (lerp-channel (color12-b c1) (color12-b c2))])
        (make-color12 r g b)))

;;; color12-darken : Color12 x Real -> Color12
;;; Darken a color. factor=0 is black, factor=1 is unchanged.
(define (color12-darken c factor)
  (let ([f (max 0.0 (min 1.0 factor))])
       (make-color12
        (inexact->exact (round (* (color12-r c) f)))
        (inexact->exact (round (* (color12-g c) f)))
        (inexact->exact (round (* (color12-b c) f))))))

;;; color12-lighten : Color12 x Real -> Color12
;;; Lighten a color. factor=0 is unchanged, factor=1 is white.
(define (color12-lighten c factor)
  (let ([f (max 0.0 (min 1.0 factor))])
       (make-color12
        (inexact->exact (round (+ (color12-r c) (* (- 15 (color12-r c)) f))))
        (inexact->exact (round (+ (color12-g c) (* (- 15 (color12-g c)) f))))
        (inexact->exact (round (+ (color12-b c) (* (- 15 (color12-b c)) f)))))))

;;; ============================================================
;;; Predefined Colors
;;; ============================================================

;;; Convenience bindings for common colors
(define color12-black   (make-color12  0  0  0))
(define color12-white   (make-color12 15 15 15))
(define color12-red     (make-color12 15  0  0))
(define color12-green   (make-color12  0 15  0))
(define color12-blue    (make-color12  0  0 15))
(define color12-yellow  (make-color12 15 15  0))
(define color12-cyan    (make-color12  0 15 15))
(define color12-magenta (make-color12 15  0 15))
