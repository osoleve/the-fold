;;; lattice/optics/format-iso.ss — Format Isomorphisms
;;; @module format-iso
;;; @requires profunctor-optics

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'sort)
(require 'profunctor-optics)

(doc 'module 'format-iso)
(doc 'description "Standard Format Isomorphisms

A library of common format conversions as profunctor isos:

  - String <-> Bytevector (UTF-8)
  - S-expression <-> String
  - S-expression <-> Bytevector
  - Alist <-> Hashtable (conceptual)
  - Number representations
  - Boolean representations

These isos are building blocks for schema migrations.
They enable bidirectional format conversion: forward (encode)
and backward (decode).")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'string-bytevector)

(doc iso-utf8 'type '(PIso String Bytevector))
(doc iso-utf8 'description "Convert between String and UTF-8 encoded Bytevector.")
(doc iso-utf8 'export #t)
(define iso-utf8
  (make-p-iso
   string->utf8    ; forward: String -> Bytevector
   utf8->string))  ; backward: Bytevector -> String

(doc iso-utf8-flip 'type '(PIso Bytevector String))
(doc iso-utf8-flip 'description "The flipped version: Bytevector to String.")
(doc iso-utf8-flip 'export #t)
(define iso-utf8-flip
  (make-p-iso
   utf8->string    ; forward: Bytevector -> String
   string->utf8))  ; backward: String -> Bytevector

(doc 'section 'sexpr)

(define (sexpr->string-fmt obj)
  (doc 'type '(-> Any String))
  (doc 'description "Convert S-expression to string using write semantics.")
  (format "~s" obj))

(define (string->sexpr str)
  (doc 'type '(-> String Any))
  (doc 'description "Parse string as S-expression.")
  (let* ([port (open-input-string str)]
         [result (read port)])
    ;; Verify we consumed all input
    (if (eof-object? (read port))
        result
        (error 'string->sexpr "Trailing data in input"))))

(doc iso-sexpr-string 'type '(PIso Any String))
(doc iso-sexpr-string 'description "Convert between S-expression and string representation.")
(doc iso-sexpr-string 'export #t)
(define iso-sexpr-string
  (make-p-iso
   sexpr->string-fmt  ; forward: Any -> String
   string->sexpr))    ; backward: String -> Any

(doc 'section 'sexpr-bytevector)

(define (sexpr->bytevector obj)
  (doc 'export #t)
  (doc 'type '(-> Any Bytevector))
  (doc 'description "Convert S-expression to UTF-8 encoded bytevector.")
  (string->utf8 (format "~s" obj)))

(define (bytevector->sexpr bv)
  (doc 'export #t)
  (doc 'type '(-> Bytevector Any))
  (doc 'description "Parse UTF-8 bytevector as S-expression.")
  (string->sexpr (utf8->string bv)))

(doc iso-sexpr-bytevector 'type '(PIso Any Bytevector))
(doc iso-sexpr-bytevector 'description "Convert between S-expression and bytevector representation.
This is the composition of iso-sexpr-string and iso-utf8.")
(doc iso-sexpr-bytevector 'export #t)
(define iso-sexpr-bytevector
  (make-p-iso
   sexpr->bytevector  ; forward: Any -> Bytevector
   bytevector->sexpr)) ; backward: Bytevector -> Any

(doc 'section 'list-vector)

(doc iso-list-vector 'type '(PIso (List a) (Vector a)))
(doc iso-list-vector 'description "Convert between list and vector.")
(doc iso-list-vector 'export #t)
(define iso-list-vector
  (make-p-iso
   list->vector      ; forward: List -> Vector
   vector->list))    ; backward: Vector -> List

(doc 'section 'alist)

(doc iso-alist-keys-sorted 'type '(PIso Alist Alist))
(doc iso-alist-keys-sorted 'description "Sort alist by keys (symbol comparison).
Useful for canonicalization before hashing.")
(doc iso-alist-keys-sorted 'export #t)
(define iso-alist-keys-sorted
  (make-p-iso
   (lambda (alist)
     (sort-by (lambda (a b)
                (string<? (symbol->string (car a))
                          (symbol->string (car b))))
              alist))
   identity))  ; Sorting is idempotent, backward is identity

(doc 'section 'numbers)

(doc iso-number-string 'type '(PIso Number String))
(doc iso-number-string 'description "Convert between number and string representation.")
(doc iso-number-string 'export #t)
(define iso-number-string
  (make-p-iso
   number->string     ; forward: Number -> String
   string->number))   ; backward: String -> Number

(doc iso-exact-inexact 'type '(PIso Exact Inexact))
(doc iso-exact-inexact 'description "Convert between exact and inexact numbers.")
(doc iso-exact-inexact 'export #t)
(define iso-exact-inexact
  (make-p-iso
   exact->inexact     ; forward: Exact -> Inexact
   inexact->exact))   ; backward: Inexact -> Exact

(doc 'section 'booleans)

(doc iso-bool-int 'type '(PIso Boolean Integer))
(doc iso-bool-int 'description "Convert between boolean and integer (0/1).")
(doc iso-bool-int 'export #t)
(define iso-bool-int
  (make-p-iso
   (lambda (b) (if b 1 0))    ; forward: Boolean -> Integer
   (lambda (n) (not (= n 0))))) ; backward: Integer -> Boolean

(doc iso-bool-symbol 'type '(PIso Boolean Symbol))
(doc iso-bool-symbol 'description "Convert between boolean and symbol ('true/'false).")
(doc iso-bool-symbol 'export #t)
(define iso-bool-symbol
  (make-p-iso
   (lambda (b) (if b 'true 'false))        ; forward: Boolean -> Symbol
   (lambda (s) (eq? s 'true))))            ; backward: Symbol -> Boolean

(doc 'section 'optional)

(doc iso-null-nothing 'type '(PIso (Maybe a) (Maybe a)))
(doc iso-null-nothing 'description "Convert between null/'() for missing and Maybe representation.
Empty list () represents nothing, any other value is just.")
(doc iso-null-nothing 'export #t)
(define iso-null-nothing
  (make-p-iso
   (lambda (x) (if (null? x) nothing (just x)))   ; forward
   (lambda (m) (if (nothing? m) '() (from-just m))))) ; backward

(doc iso-false-nothing 'type '(PIso (Maybe a) (Maybe a)))
(doc iso-false-nothing 'description "Convert between #f for missing and Maybe representation.")
(doc iso-false-nothing 'export #t)
(define iso-false-nothing
  (make-p-iso
   (lambda (x) (if x (just x) nothing))         ; forward
   (lambda (m) (if (nothing? m) #f (from-just m))))) ; backward

(doc 'section 'time)

(define (string-split str delim)
  (doc 'type '(-> String Char (List String)))
  (doc 'description "Split string by delimiter character.")
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) result))]
      [(char=? (car chars) delim)
       (loop (cdr chars)
             '()
             (cons (list->string (reverse current)) result))]
      [else
       (loop (cdr chars)
             (cons (car chars) current)
             result)])))

(doc iso-time-unix 'type '(PIso TimeString Integer))
(doc iso-time-unix 'description "Convert between ISO 8601 time string and Unix timestamp.
Note: This is a simplified version; real implementation would need proper date parsing.")
(doc iso-time-unix 'export #t)
(define iso-time-unix
  (make-p-iso
   (lambda (ts)
     ;; Simplified: parse \"YYYY-MM-DD\" to days since epoch
     ;; Real implementation would use a date library
     (let* ([parts (string-split ts #\-)]
            [year (string->number (car parts))]
            [month (string->number (cadr parts))]
            [day (string->number (caddr parts))])
       ;; Very rough approximation for demo
       (+ (* (- year 1970) 365 24 60 60)
          (* (- month 1) 30 24 60 60)
          (* (- day 1) 24 60 60))))
   (lambda (unix)
     ;; Simplified reverse
     (let* ([days (quotient unix (* 24 60 60))]
            [years (quotient days 365)]
            [year (+ 1970 years)]
            [remaining (- days (* years 365))]
            [month (+ 1 (quotient remaining 30))]
            [day (+ 1 (modulo remaining 30))])
       (format "~a-~a-~a"
               year
               (if (< month 10) (format "0~a" month) month)
               (if (< day 10) (format "0~a" day) day))))))

(doc 'section 'symbol-string)

(doc iso-symbol-string 'type '(PIso Symbol String))
(doc iso-symbol-string 'description "Convert between symbol and string.")
(doc iso-symbol-string 'export #t)
(define iso-symbol-string
  (make-p-iso
   symbol->string     ; forward: Symbol -> String
   string->symbol))   ; backward: String -> Symbol

(doc 'section 'wrapped-values)

(define (make-tag-iso tag)
  (doc 'export #t)
  (doc 'type '(-> Symbol (PIso a (Pair Symbol a))))
  (doc 'description "Create an iso that wraps/unwraps a value with a tag.")
  (make-p-iso
   (lambda (v) (cons tag v))        ; forward: wrap
   (lambda (tagged)                  ; backward: unwrap
     (if (and (pair? tagged) (eq? (car tagged) tag))
         (cdr tagged)
         (error 'make-tag-iso "Tag mismatch" tag (car tagged))))))

(doc iso-ok-unwrap 'type '(PIso a (Pair ok a)))
(doc iso-ok-unwrap 'description "Wrap/unwrap with 'ok tag (common in result types).")
(doc iso-ok-unwrap 'export #t)
(define iso-ok-unwrap
  (make-tag-iso 'ok))

(doc 'section 'helpers)

(doc format-compose 'type '(-> (PIso a b) (PIso b c) (PIso a c)))
(doc format-compose 'description "Compose two format isos (same as p-iso-compose).")
(doc format-compose 'export #t)
(define format-compose p-iso-compose)

(define (format-flip iso)
  (doc 'export #t)
  (doc 'type '(-> (PIso a b) (PIso b a)))
  (doc 'description "Flip a format iso (swap forward/backward).")
  (make-p-iso
   (p-iso-backward iso)
   (p-iso-forward iso)))

