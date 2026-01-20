(doc 'module 'string-format)
(doc 'description "Case conversion and padding utilities for string display.")
(doc 'layer 'core)

(doc 'section 'case-conversion)

(define (string-upcase str)
  (doc 'type (-> String String))
  (doc 'description "Convert string to uppercase.")
  (doc 'export #t)
  (list->string (map char-upcase (string->list str))))

(define (string-downcase str)
  (doc 'type (-> String String))
  (doc 'description "Convert string to lowercase.")
  (doc 'export #t)
  (list->string (map char-downcase (string->list str))))

(doc 'section 'padding)

(define (string-pad-left str width pad-char)
  (doc 'type (-> String Integer Char String))
  (doc 'description "Pad string on the left to reach target width.")
  (doc 'export #t)
  (let ([len (string-length str)])
       (if (>= len width)
           str
           (string-append (make-string (- width len) pad-char) str))))

(define (string-pad-right str width pad-char)
  (doc 'type (-> String Integer Char String))
  (doc 'description "Pad string on the right to reach target width.")
  (doc 'export #t)
  (let ([len (string-length str)])
       (if (>= len width)
           str
           (string-append str (make-string (- width len) pad-char)))))
