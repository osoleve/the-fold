;;; shell/io/json.ss — JSON Parsing and Serialization
;;;
;;; Shared JSON utilities for all effect handlers.
;;;
;;; This is Shell code: defensive logic, handles malformed input.
;;;
;;; Exports:
;;;   - parse-json-string : String -> Any | #f
;;;   - json->string : Any -> String
;;;   - json-escape : String -> String

;;; ============================================================
;;; JSON Parsing
;;; ============================================================

;;; parse-json-string : String -> Any | #f
;;; Parse a JSON string into Scheme data structures.
;;; Objects become alists, arrays become lists.
;;; Returns #f on parse failure.
(define (parse-json-string s)
  (guard (ex [else #f])
         (if (or (not s) (string=? s ""))
             #f
             (let ([trimmed (json-string-trim s)])
                  (if (string=? trimmed "")
                      #f
                      (let-values ([(result rest) (parse-json-value trimmed 0)])
                                  result))))))

;;; json-string-trim : String -> String
;;; Remove leading/trailing whitespace.
(define (json-string-trim s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                     (if (and (< i len) (char-whitespace? (string-ref s i)))
                         (loop (+ i 1))
                         i))]
         [end (let loop ([i (- len 1)])
                   (if (and (>= i start) (char-whitespace? (string-ref s i)))
                       (loop (- i 1))
                       (+ i 1)))])
        (if (>= start end)
            ""
            (substring s start end))))

;;; parse-json-value : String -> Integer -> (Values Any Integer)
;;; Parse a JSON value starting at position, return value and end position.
(define (parse-json-value s pos)
  (let ([pos (json-skip-whitespace s pos)])
       (if (>= pos (string-length s))
           (values #f pos)
           (let ([c (string-ref s pos)])
                (cond
                 [(char=? c #\{) (parse-json-object s pos)]
                 [(char=? c #\[) (parse-json-array s pos)]
                 [(char=? c #\") (parse-json-string-value s pos)]
                 [(or (char=? c #\-) (char-numeric? c)) (parse-json-number s pos)]
                 [(char=? c #\t) (parse-json-true s pos)]
                 [(char=? c #\f) (parse-json-false s pos)]
                 [(char=? c #\n) (parse-json-null s pos)]
                 [else (values #f pos)])))))

;;; json-skip-whitespace : String -> Integer -> Integer
(define (json-skip-whitespace s pos)
  (let ([len (string-length s)])
       (let loop ([i pos])
            (if (and (< i len) (char-whitespace? (string-ref s i)))
                (loop (+ i 1))
                i))))

;;; parse-json-object : String -> Integer -> (Values Alist Integer)
(define (parse-json-object s pos)
  (let ([pos (+ pos 1)])  ; skip '{'
       (let loop ([pos (json-skip-whitespace s pos)]
                  [result '()])
            (if (or (>= pos (string-length s)) (char=? (string-ref s pos) #\}))
                (values (reverse result) (+ pos 1))
                (let-values ([(key pos2) (parse-json-string-value s pos)])
                            (let ([pos3 (json-skip-whitespace s pos2)])
                                 (if (and (< pos3 (string-length s)) (char=? (string-ref s pos3) #\:))
                                     (let-values ([(val pos4) (parse-json-value s (+ pos3 1))])
                                                 (let ([pos5 (json-skip-whitespace s pos4)])
                                                      (if (and (< pos5 (string-length s)) (char=? (string-ref s pos5) #\,))
                                                          (loop (json-skip-whitespace s (+ pos5 1))
                                                                (cons (cons (string->symbol key) val) result))
                                                          (loop pos5 (cons (cons (string->symbol key) val) result)))))
                                     (values (reverse result) pos3))))))))

;;; parse-json-array : String -> Integer -> (Values List Integer)
(define (parse-json-array s pos)
  (let ([pos (+ pos 1)])  ; skip '['
       (let loop ([pos (json-skip-whitespace s pos)]
                  [result '()])
            (if (or (>= pos (string-length s)) (char=? (string-ref s pos) #\]))
                (values (reverse result) (+ pos 1))
                (let-values ([(val pos2) (parse-json-value s pos)])
                            (let ([pos3 (json-skip-whitespace s pos2)])
                                 (if (and (< pos3 (string-length s)) (char=? (string-ref s pos3) #\,))
                                     (loop (json-skip-whitespace s (+ pos3 1)) (cons val result))
                                     (loop pos3 (cons val result)))))))))

;;; parse-json-string-value : String -> Integer -> (Values String Integer)
(define (parse-json-string-value s pos)
  (let ([pos (+ pos 1)])  ; skip opening quote
       (let loop ([i pos]
                  [chars '()])
            (if (>= i (string-length s))
                (values (list->string (reverse chars)) i)
                (let ([c (string-ref s i)])
                     (cond
                      [(char=? c #\")
                       (values (list->string (reverse chars)) (+ i 1))]
                      [(char=? c #\\)
                       (if (< (+ i 1) (string-length s))
                           (let ([next (string-ref s (+ i 1))])
                                (case next
                                      [(#\n) (loop (+ i 2) (cons #\newline chars))]
                                      [(#\r) (loop (+ i 2) (cons #\return chars))]
                                      [(#\t) (loop (+ i 2) (cons #\tab chars))]
                                      [(#\b) (loop (+ i 2) (cons #\backspace chars))]
                                      [(#\f) (loop (+ i 2) (cons #\page chars))]
                                      [(#\" #\\ #\/) (loop (+ i 2) (cons next chars))]
                                      [(#\u)
                                       ;; Unicode escape \uXXXX
                                       (if (< (+ i 5) (string-length s))
                                           (let ([hex (substring s (+ i 2) (+ i 6))])
                                                (let ([code (string->number hex 16)])
                                                     (if code
                                                         (loop (+ i 6) (cons (integer->char code) chars))
                                                         (loop (+ i 2) (cons next chars)))))
                                           (loop (+ i 2) (cons next chars)))]
                                      [else (loop (+ i 2) (cons next chars))]))
                           (values (list->string (reverse chars)) i))]
                      [else (loop (+ i 1) (cons c chars))]))))))

;;; parse-json-number : String -> Integer -> (Values Number Integer)
(define (parse-json-number s pos)
  (let ([len (string-length s)])
       (let loop ([i pos]
                  [chars '()])
            (if (and (< i len)
                     (let ([c (string-ref s i)])
                          (or (char-numeric? c)
                              (char=? c #\-)
                              (char=? c #\+)
                              (char=? c #\.)
                              (char=? c #\e)
                              (char=? c #\E))))
                (loop (+ i 1) (cons (string-ref s i) chars))
                (values (string->number (list->string (reverse chars))) i)))))

;;; parse-json-true : String -> Integer -> (Values #t Integer)
(define (parse-json-true s pos)
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "true"))
      (values #t (+ pos 4))
      (values #f pos)))

;;; parse-json-false : String -> Integer -> (Values #f Integer)
(define (parse-json-false s pos)
  (if (and (<= (+ pos 5) (string-length s))
           (string=? (substring s pos (+ pos 5)) "false"))
      (values #f (+ pos 5))
      (values #f pos)))

;;; parse-json-null : String -> Integer -> (Values '() Integer)
(define (parse-json-null s pos)
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "null"))
      (values '() (+ pos 4))
      (values #f pos)))

;;; ============================================================
;;; JSON Serialization
;;; ============================================================

;;; json->string : Any -> String
;;; Convert Scheme value to JSON string.
(define (json->string v)
  (cond
   [(string? v) (string-append "\"" (json-escape v) "\"")]
   [(number? v) (number->string v)]
   [(boolean? v) (if v "true" "false")]
   [(null? v) "null"]
   [(symbol? v) (string-append "\"" (json-escape (symbol->string v)) "\"")]
   [(and (pair? v) (pair? (car v)) (symbol? (caar v)))
    ;; Alist -> JSON object
    (json-object->string v)]
   [(list? v)
    ;; List -> JSON array
    (json-array->string v)]
   [else
    ;; Fallback: convert to string
    (string-append "\"" (json-escape (format "~a" v)) "\"")]))

;;; json-object->string : Alist -> String
(define (json-object->string alist)
  (string-append
   "{"
   (json-intersperse
    ", "
    (map (lambda (pair)
                 (string-append
                  "\"" (json-escape (symbol->string (car pair))) "\": "
                  (json->string (cdr pair))))
         alist))
   "}"))

;;; json-array->string : List -> String
(define (json-array->string lst)
  (string-append
   "["
   (json-intersperse ", " (map json->string lst))
   "]"))

;;; json-escape : String -> String
;;; Escape a string for JSON (handle quotes, newlines, backslashes, control chars).
(define (json-escape s)
  (let loop ([chars (string->list s)]
             [result '()])
       (if (null? chars)
           (list->string (reverse result))
           (let ([c (car chars)])
                (cond
                 [(char=? c #\") (loop (cdr chars) (append '(#\" #\\) result))]
                 [(char=? c #\\) (loop (cdr chars) (append '(#\\ #\\) result))]
                 [(char=? c #\newline) (loop (cdr chars) (append (list #\n #\\) result))]
                 [(char=? c #\return) (loop (cdr chars) (append (list #\r #\\) result))]
                 [(char=? c #\tab) (loop (cdr chars) (append (list #\t #\\) result))]
                 [(char=? c #\backspace) (loop (cdr chars) (append (list #\b #\\) result))]
                 [(char=? c #\page) (loop (cdr chars) (append (list #\f #\\) result))]
                 ;; Control characters (< 0x20) use \uXXXX
                 [(< (char->integer c) #x20)
                  (let ([hex (format "~4,'0x" (char->integer c))])
                       (loop (cdr chars)
                             (append (reverse (string->list (string-append "u" hex)))
                                     (cons #\\ result))))]
                 [else (loop (cdr chars) (cons c result))])))))

;;; json-intersperse : String -> List String -> String
(define (json-intersperse sep lst)
  (cond
   [(null? lst) ""]
   [(null? (cdr lst)) (car lst)]
   [else (string-append (car lst) sep (json-intersperse sep (cdr lst)))]))
