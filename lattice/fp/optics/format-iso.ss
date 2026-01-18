;;; lattice/fp/optics/format-iso.ss — Standard Format Isomorphisms
;;;
;;; A library of common format conversions as profunctor isos:
;;;
;;;   - String <-> Bytevector (UTF-8)
;;;   - S-expression <-> String
;;;   - S-expression <-> Bytevector
;;;   - Alist <-> Hashtable (conceptual)
;;;   - Number representations
;;;   - Boolean representations
;;;
;;; These isos are building blocks for schema migrations.
;;; They enable bidirectional format conversion: forward (encode)
;;; and backward (decode).
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - profunctor-optics.ss (for p-iso infrastructure)

(load "lattice/fp/optics/profunctor-optics.ss")

;;; ============================================================
;;; Part 1: String <-> Bytevector Isomorphisms
;;; ============================================================

;;; iso-utf8 : PIso String Bytevector
;;; Convert between String and UTF-8 encoded Bytevector.
(define iso-utf8
  (make-p-iso
   string->utf8    ; forward: String -> Bytevector
   utf8->string))  ; backward: Bytevector -> String

;;; iso-utf8-flip : PIso Bytevector String
;;; The flipped version: Bytevector to String.
(define iso-utf8-flip
  (make-p-iso
   utf8->string    ; forward: Bytevector -> String
   string->utf8))  ; backward: String -> Bytevector

;;; ============================================================
;;; Part 2: S-expression <-> String Isomorphisms
;;; ============================================================

;;; sexpr->string : Any -> String
;;; Convert S-expression to string using write semantics.
(define (sexpr->string-fmt obj)
  (format "~s" obj))

;;; string->sexpr : String -> Any
;;; Parse string as S-expression.
(define (string->sexpr str)
  (let* ([port (open-input-string str)]
         [result (read port)])
    ;; Verify we consumed all input
    (if (eof-object? (read port))
        result
        (error 'string->sexpr "Trailing data in input"))))

;;; iso-sexpr-string : PIso Any String
;;; Convert between S-expression and string representation.
(define iso-sexpr-string
  (make-p-iso
   sexpr->string-fmt  ; forward: Any -> String
   string->sexpr))    ; backward: String -> Any

;;; ============================================================
;;; Part 3: S-expression <-> Bytevector Isomorphisms
;;; ============================================================

;;; sexpr->bytevector : Any -> Bytevector
;;; Convert S-expression to UTF-8 encoded bytevector.
(define (sexpr->bytevector obj)
  (string->utf8 (format "~s" obj)))

;;; bytevector->sexpr : Bytevector -> Any
;;; Parse UTF-8 bytevector as S-expression.
(define (bytevector->sexpr bv)
  (string->sexpr (utf8->string bv)))

;;; iso-sexpr-bytevector : PIso Any Bytevector
;;; Convert between S-expression and bytevector representation.
;;; This is the composition of iso-sexpr-string and iso-utf8.
(define iso-sexpr-bytevector
  (make-p-iso
   sexpr->bytevector  ; forward: Any -> Bytevector
   bytevector->sexpr)) ; backward: Bytevector -> Any

;;; ============================================================
;;; Part 4: List <-> Vector Isomorphisms
;;; ============================================================

;;; iso-list-vector : PIso (List a) (Vector a)
;;; Convert between list and vector.
(define iso-list-vector
  (make-p-iso
   list->vector      ; forward: List -> Vector
   vector->list))    ; backward: Vector -> List

;;; ============================================================
;;; Part 5: Alist Operations
;;; ============================================================
;;;
;;; Alists are the primary representation for structured data
;;; in The Fold. These isos help manipulate them.

;;; iso-alist-keys-sorted : PIso Alist Alist
;;; Sort alist by keys (symbol comparison).
;;; Useful for canonicalization before hashing.
(define iso-alist-keys-sorted
  (make-p-iso
   (lambda (alist)
     (sort (lambda (a b)
             (string<? (symbol->string (car a))
                       (symbol->string (car b))))
           alist))
   identity))  ; Sorting is idempotent, backward is identity

;;; ============================================================
;;; Part 6: Number Representation Isomorphisms
;;; ============================================================

;;; iso-number-string : PIso Number String
;;; Convert between number and string representation.
(define iso-number-string
  (make-p-iso
   number->string     ; forward: Number -> String
   string->number))   ; backward: String -> Number

;;; iso-exact-inexact : PIso Exact Inexact
;;; Convert between exact and inexact numbers.
(define iso-exact-inexact
  (make-p-iso
   exact->inexact     ; forward: Exact -> Inexact
   inexact->exact))   ; backward: Inexact -> Exact

;;; ============================================================
;;; Part 7: Boolean Representation Isomorphisms
;;; ============================================================

;;; iso-bool-int : PIso Boolean Integer
;;; Convert between boolean and integer (0/1).
(define iso-bool-int
  (make-p-iso
   (lambda (b) (if b 1 0))    ; forward: Boolean -> Integer
   (lambda (n) (not (= n 0))))) ; backward: Integer -> Boolean

;;; iso-bool-symbol : PIso Boolean Symbol
;;; Convert between boolean and symbol ('true/'false).
(define iso-bool-symbol
  (make-p-iso
   (lambda (b) (if b 'true 'false))        ; forward: Boolean -> Symbol
   (lambda (s) (eq? s 'true))))            ; backward: Symbol -> Boolean

;;; ============================================================
;;; Part 8: Optional/Maybe Representation
;;; ============================================================

;;; iso-null-nothing : PIso (a | '()) (Maybe a)
;;; Convert between null/'() for missing and Maybe representation.
;;; Empty list () represents nothing, any other value is just.
(define iso-null-nothing
  (make-p-iso
   (lambda (x) (if (null? x) nothing (just x)))   ; forward
   (lambda (m) (if (nothing? m) '() (from-just m))))) ; backward

;;; iso-false-nothing : PIso (a | #f) (Maybe a)
;;; Convert between #f for missing and Maybe representation.
(define iso-false-nothing
  (make-p-iso
   (lambda (x) (if x (just x) nothing))         ; forward
   (lambda (m) (if (nothing? m) #f (from-just m))))) ; backward

;;; ============================================================
;;; Part 9: Timestamp Representations
;;; ============================================================

;;; iso-time-unix : PIso TimeString Integer
;;; Convert between ISO 8601 time string and Unix timestamp.
;;; Note: This is a simplified version; real implementation
;;; would need proper date parsing.
(define iso-time-unix
  (make-p-iso
   (lambda (ts)
     ;; Simplified: parse "YYYY-MM-DD" to days since epoch
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

;;; string-split : String -> Char -> (List String)
;;; Split string by delimiter character.
(define (string-split str delim)
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

;;; ============================================================
;;; Part 10: Symbol <-> String
;;; ============================================================

;;; iso-symbol-string : PIso Symbol String
;;; Convert between symbol and string.
(define iso-symbol-string
  (make-p-iso
   symbol->string     ; forward: Symbol -> String
   string->symbol))   ; backward: String -> Symbol

;;; ============================================================
;;; Part 11: Wrapped Value Isomorphisms
;;; ============================================================
;;;
;;; For tagged/wrapped values like ('ok value) <-> value

;;; make-tag-iso : Symbol -> PIso a (Symbol . a)
;;; Create an iso that wraps/unwraps a value with a tag.
(define (make-tag-iso tag)
  (make-p-iso
   (lambda (v) (cons tag v))        ; forward: wrap
   (lambda (tagged)                  ; backward: unwrap
     (if (and (pair? tagged) (eq? (car tagged) tag))
         (cdr tagged)
         (error 'make-tag-iso "Tag mismatch" tag (car tagged))))))

;;; iso-ok-unwrap : PIso a (ok . a)
;;; Wrap/unwrap with 'ok tag (common in result types).
(define iso-ok-unwrap
  (make-tag-iso 'ok))

;;; ============================================================
;;; Part 12: Composition Helpers
;;; ============================================================

;;; format-compose : PIso a b -> PIso b c -> PIso a c
;;; Compose two format isos (same as p-iso-compose).
(define format-compose p-iso-compose)

;;; format-flip : PIso a b -> PIso b a
;;; Flip a format iso (swap forward/backward).
(define (format-flip iso)
  (make-p-iso
   (p-iso-backward iso)
   (p-iso-forward iso)))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; String/Bytevector:
;;;   iso-utf8, iso-utf8-flip
;;;
;;; S-expression:
;;;   iso-sexpr-string, iso-sexpr-bytevector
;;;   sexpr->bytevector, bytevector->sexpr
;;;
;;; List/Vector:
;;;   iso-list-vector
;;;
;;; Alist:
;;;   iso-alist-keys-sorted
;;;
;;; Numbers:
;;;   iso-number-string, iso-exact-inexact
;;;
;;; Booleans:
;;;   iso-bool-int, iso-bool-symbol
;;;
;;; Optional:
;;;   iso-null-nothing, iso-false-nothing
;;;
;;; Time:
;;;   iso-time-unix
;;;
;;; Symbols:
;;;   iso-symbol-string
;;;
;;; Tagged:
;;;   make-tag-iso, iso-ok-unwrap
;;;
;;; Helpers:
;;;   format-compose, format-flip
;;;   string-split

(display "Loaded: lattice/fp/optics/format-iso.ss\n")
