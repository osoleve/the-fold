; char.ss - Character predicates and operations
; Part of The Fold prelude

; ============================================
; Character predicates
; ============================================

; char-between?: Check if character code is in range
(define (char-between? lo hi c)
  (let ((code (char->integer c)))
       (and (>= code (char->integer lo))
            (<= code (char->integer hi)))))

; char-alphabetic?, char-alpha?: Check if character is a letter (aliases)
(define (char-alphabetic? c)
  (or (char-between? #\a #\z c)
      (char-between? #\A #\Z c)))

(define char-alpha? char-alphabetic?)  ; Alias

; char-numeric?, char-digit?: Check if character is a digit (aliases)
(define (char-numeric? c)
  (char-between? #\0 #\9 c))

(define char-digit? char-numeric?)  ; Alias

; char-alphanumeric?: Check if char is alphanumeric
(define (char-alphanumeric? c)
  (or (char-alphabetic? c) (char-numeric? c)))

; char-whitespace?: Check if character is whitespace
(define (char-whitespace? c)
  (or (eq? c #\space)
      (or (eq? c #\tab)
          (or (eq? c #\newline)
              (eq? c #\return)))))

; char-lower?: Check if char is lowercase
(define (char-lower? c)
  (char-between? #\a #\z c))

; char-upper?: Check if char is uppercase
(define (char-upper? c)
  (char-between? #\A #\Z c))

; ============================================
; Character transformations
; ============================================

; char-upcase: Convert character to uppercase
(define (char-upcase c)
  (if (char-lower? c)
      (integer->char (- (char->integer c) 32))
      c))

; char-downcase: Convert character to lowercase
(define (char-downcase c)
  (if (char-upper? c)
      (integer->char (+ (char->integer c) 32))
      c))

; ============================================
; Module exports
; ============================================

(module-exports
 char-between?
 char-alphabetic? char-alpha?
 char-numeric? char-digit?
 char-alphanumeric?
 char-whitespace?
 char-lower?
 char-upper?
 char-upcase
 char-downcase)
