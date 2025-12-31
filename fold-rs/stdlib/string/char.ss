; ============================================================
; Character Predicates and Operations
; Character classification and case conversion
; ============================================================

; char-between?: Check if character code is in range
(char-between? (fn (lo hi c)
                  (let ((code (char->integer c)))
                       (and (>= code (char->integer lo))
                            (<= code (char->integer hi))))))

; char-alpha?: Check if character is a letter
(char-alpha? (fn (c)
                (or (char-between? #\a #\z c)
                    (char-between? #\A #\Z c))))

; char-digit?: Check if character is a digit
(char-digit? (fn (c)
                (char-between? #\0 #\9 c)))

; char-alphanumeric?: Check if char is alphanumeric
(char-alphanumeric? (fn (c)
                       (or (char-alpha? c) (char-digit? c))))

; char-space?: Check if character is whitespace
(char-space? (fn (c)
                (or (eq? c #\space)
                    (or (eq? c #\tab)
                        (or (eq? c #\newline)
                            (eq? c #\return))))))

; char-lower?: Check if char is lowercase
(char-lower? (fn (c)
                (char-between? #\a #\z c)))

; char-upper?: Check if char is uppercase
(char-upper? (fn (c)
                (char-between? #\A #\Z c)))

; char-to-upper: Convert character to uppercase
(char-to-upper (fn (c)
                  (if (char-lower? c)
                      (integer->char (- (char->integer c) 32))
                      c)))

; char-to-lower: Convert character to lowercase
(char-to-lower (fn (c)
                  (if (char-upper? c)
                      (integer->char (+ (char->integer c) 32))
                      c)))
