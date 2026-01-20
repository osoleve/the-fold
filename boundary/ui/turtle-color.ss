(doc 'module 'turtle-color)
(doc 'description "12-bit color system for turtle graphics - 4096-color palette")
(doc 'layer 'boundary)
(doc 'purity 'total)

(doc 'note "Provides a 4096-color palette (4 bits per RGB channel) for turtle graphics")
(doc 'note "Supports multiple color specification formats:")
(doc 'note "- RGB triplet: (make-color12 15 0 0) or (color12 15 0 0)")
(doc 'note "- Integer: (color12-from-int #xF00)")
(doc 'note "- Named: (color12-from-name 'red)")
(doc 'dependencies "None")

(doc 'section 'color12-data-structure)

(doc 'note "Color12 = (color12 r g b) where 0 <= r,g,b <= 15")
(doc 'note "Representation uses tagged list for consistency with boundary/color.ss pattern")

(doc make-color12 'type (-> Nat Nat Nat Color12))
(doc make-color12 'description "Create a 12-bit color from RGB components (0-15 each)")
(define (make-color12 r g b)
  (list 'color12
        (max 0 (min 15 r))
        (max 0 (min 15 g))
        (max 0 (min 15 b))))

(doc color12? 'type (-> Any Bool))
(doc color12? 'description "Test if value is a Color12")
(define (color12? c)
  (and (list? c)
       (= (length c) 4)
       (eq? (car c) 'color12)))

(doc 'note "Accessors")
(define (color12-r c) (list-ref c 1))
(define (color12-g c) (list-ref c 2))
(define (color12-b c) (list-ref c 3))

(doc color12->list 'type (-> Color12 (List Nat Nat Nat)))
(doc color12->list 'description "Extract RGB as a list")
(define (color12->list c)
  (list (color12-r c) (color12-g c) (color12-b c)))

(doc list->color12 'type (-> (List Nat Nat Nat) Color12))
(doc list->color12 'description "Create Color12 from a list")
(define (list->color12 lst)
  (make-color12 (car lst) (cadr lst) (caddr lst)))

(doc 'section 'color-conversions)

(doc color12-from-int 'type (-> Nat Color12))
(doc color12-from-int 'description "Create Color12 from integer 0-4095 (#x000 to #xFFF)")
(doc color12-from-int 'note "Format: #xRGB where R, G, B are each 0-15")
(define (color12-from-int n)
  (let* ([clamped (max 0 (min #xFFF n))]
         [r (bitwise-arithmetic-shift-right clamped 8)]
         [g (bitwise-and (bitwise-arithmetic-shift-right clamped 4) #xF)]
         [b (bitwise-and clamped #xF)])
        (make-color12 r g b)))

(doc color12->int 'type (-> Color12 Nat))
(doc color12->int 'description "Convert Color12 to integer 0-4095")
(define (color12->int c)
  (+ (bitwise-arithmetic-shift-left (color12-r c) 8)
     (bitwise-arithmetic-shift-left (color12-g c) 4)
     (color12-b c)))

(doc color12->svg-hex 'type (-> Color12 String))
(doc color12->svg-hex 'description "Convert to SVG hex string \"#RRGGBB\" (scaled from 4-bit to 8-bit)")
(doc color12->svg-hex 'note "Each 4-bit value (0-15) maps to 8-bit (0-255) by: value * 17")
(define (color12->svg-hex c)
  (let* ([scale (lambda (v) (* v 17))]  ; 0->0, 15->255
         [r (scale (color12-r c))]
         [g (scale (color12-g c))]
         [b (scale (color12-b c))])
        (string-append "#"
                       (hex-byte r)
                       (hex-byte g)
                       (hex-byte b))))

(doc hex-byte 'type (-> Nat String))
(doc hex-byte 'description "Convert byte (0-255) to two-character hex string")
(define (hex-byte n)
  (let* ([hi (bitwise-arithmetic-shift-right n 4)]
         [lo (bitwise-and n #xF)])
        (string (hex-digit hi) (hex-digit lo))))

(doc hex-digit 'type (-> Nat Char))
(doc hex-digit 'description "Convert 0-15 to hex digit")
(define (hex-digit n)
  (if (< n 10)
      (integer->char (+ n (char->integer #\0)))
      (integer->char (+ (- n 10) (char->integer #\A)))))

(doc 'section 'named-color-palette)

(doc *color12-palette* 'type (Alist Symbol (List Nat Nat Nat)))
(doc *color12-palette* 'description "Standard named colors for 12-bit space")
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

(doc color12-from-name 'type (-> Symbol (Union Color12 Bool)))
(doc color12-from-name 'description "Look up a named color")
(doc color12-from-name 'returns "#f if not found")
(define (color12-from-name name)
  (let ([entry (assq name *color12-palette*)])
       (if entry
           (apply make-color12 (cdr entry))
           #f)))

(doc color12-names 'type (-> (List Symbol)))
(doc color12-names 'description "Return list of all named colors")
(define (color12-names)
  (map car *color12-palette*))

(doc 'section 'unified-color-parser)

(doc parse-color12 'type (-> ColorSpec (Union Color12 Bool)))
(doc parse-color12 'description "Parse a color specification")
(doc parse-color12 'param "Accepts:")
(doc parse-color12 'param "- Color12 value (passthrough)")
(doc parse-color12 'param "- List (r g b) triplet with 0-15 values")
(doc parse-color12 'param "- Integer 0-4095")
(doc parse-color12 'param "- Symbol naming a color")
(doc parse-color12 'returns "#f if invalid")
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

(doc 'section 'color-operations)

(doc color12-lerp 'type (-> Color12 Color12 Real Color12))
(doc color12-lerp 'description "Linear interpolation between two colors")
(doc color12-lerp 'note "t=0 returns c1, t=1 returns c2")
(define (color12-lerp c1 c2 t)
  (let* ([lerp-channel (lambda (a b)
                               (inexact->exact (round (+ a (* (- b a) t)))))]
         [r (lerp-channel (color12-r c1) (color12-r c2))]
         [g (lerp-channel (color12-g c1) (color12-g c2))]
         [b (lerp-channel (color12-b c1) (color12-b c2))])
        (make-color12 r g b)))

(doc color12-darken 'type (-> Color12 Real Color12))
(doc color12-darken 'description "Darken a color")
(doc color12-darken 'note "factor=0 is black, factor=1 is unchanged")
(define (color12-darken c factor)
  (let ([f (max 0.0 (min 1.0 factor))])
       (make-color12
        (inexact->exact (round (* (color12-r c) f)))
        (inexact->exact (round (* (color12-g c) f)))
        (inexact->exact (round (* (color12-b c) f))))))

(doc color12-lighten 'type (-> Color12 Real Color12))
(doc color12-lighten 'description "Lighten a color")
(doc color12-lighten 'note "factor=0 is unchanged, factor=1 is white")
(define (color12-lighten c factor)
  (let ([f (max 0.0 (min 1.0 factor))])
       (make-color12
        (inexact->exact (round (+ (color12-r c) (* (- 15 (color12-r c)) f))))
        (inexact->exact (round (+ (color12-g c) (* (- 15 (color12-g c)) f))))
        (inexact->exact (round (+ (color12-b c) (* (- 15 (color12-b c)) f)))))))

(doc 'section 'predefined-colors)

(doc 'note "Convenience bindings for common colors")
(define color12-black   (make-color12  0  0  0))
(define color12-white   (make-color12 15 15 15))
(define color12-red     (make-color12 15  0  0))
(define color12-green   (make-color12  0 15  0))
(define color12-blue    (make-color12  0  0 15))
(define color12-yellow  (make-color12 15 15  0))
(define color12-cyan    (make-color12  0 15 15))
(define color12-magenta (make-color12 15  0 15))
