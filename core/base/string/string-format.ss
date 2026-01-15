;;; core/base/string/string-format.ss — String Formatting Operations
;;;
;;; Case conversion and padding utilities for string display.

;;; ====
;;; Case Conversion
;;; ====

;;; string-upcase : String → String
;;; Convert string to uppercase.
(define (string-upcase str)
  (list->string (map char-upcase (string->list str))))

;;; string-downcase : String → String
;;; Convert string to lowercase.
(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

;;; ====
;;; Padding
;;; ====

;;; string-pad-left : String × Integer × Char → String
;;; Pad string on the left to reach target width.
(define (string-pad-left str width pad-char)
  (let ([len (string-length str)])
       (if (>= len width)
           str
           (string-append (make-string (- width len) pad-char) str))))

;;; string-pad-right : String × Integer × Char → String
;;; Pad string on the right to reach target width.
(define (string-pad-right str width pad-char)
  (let ([len (string-length str)])
       (if (>= len width)
           str
           (string-append str (make-string (- width len) pad-char)))))
