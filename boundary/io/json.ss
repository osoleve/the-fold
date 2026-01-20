;;; Local doc macro - json.ss is self-contained, no prelude dependency
(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'json)
(doc 'description "JSON Parsing and Serialization — Shared JSON utilities for all effect handlers. Objects become alists, arrays become lists.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '())
(doc 'note "Self-contained module with no prelude dependency. Handles malformed input defensively.")

(doc 'section 'json-parsing)

(define (parse-json-string s)
  (doc 'type (-> String (Maybe Any)))
  (doc 'description "Parse a JSON string into Scheme data structures. Objects become alists, arrays become lists. Returns #f on parse failure.")
  (doc 'export #t)
  (guard (ex [else #f])
         (if (or (not s) (string=? s ""))
             #f
             (let ([trimmed (json-string-trim s)])
                  (if (string=? trimmed "")
                      #f
                      (let-values ([(result rest) (parse-json-value trimmed 0)])
                                  result))))))

(define (json-string-trim s)
  (doc 'type (-> String String))
  (doc 'description "Remove leading/trailing whitespace.")
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

(define (parse-json-value s pos)
  (doc 'type (-> String Integer (Values Any Integer)))
  (doc 'description "Parse a JSON value starting at position, return value and end position.")
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

(define (json-skip-whitespace s pos)
  (doc 'type (-> String Integer Integer))
  (doc 'description "Skip whitespace in string starting at position.")
  (let ([len (string-length s)])
       (let loop ([i pos])
            (if (and (< i len) (char-whitespace? (string-ref s i)))
                (loop (+ i 1))
                i))))

(define (parse-json-object s pos)
  (doc 'type (-> String Integer (Values Alist Integer)))
  (doc 'description "Parse a JSON object starting at position.")
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

(define (parse-json-array s pos)
  (doc 'type (-> String Integer (Values List Integer)))
  (doc 'description "Parse a JSON array starting at position.")
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

(define (parse-json-string-value s pos)
  (doc 'type (-> String Integer (Values String Integer)))
  (doc 'description "Parse a JSON string value starting at position.")
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

(define (parse-json-number s pos)
  (doc 'type (-> String Integer (Values Number Integer)))
  (doc 'description "Parse a JSON number starting at position.")
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

(define (parse-json-true s pos)
  (doc 'type (-> String Integer (Values Bool Integer)))
  (doc 'description "Parse JSON true literal.")
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "true"))
      (values #t (+ pos 4))
      (values #f pos)))

(define (parse-json-false s pos)
  (doc 'type (-> String Integer (Values Bool Integer)))
  (doc 'description "Parse JSON false literal.")
  (if (and (<= (+ pos 5) (string-length s))
           (string=? (substring s pos (+ pos 5)) "false"))
      (values #f (+ pos 5))
      (values #f pos)))

(define (parse-json-null s pos)
  (doc 'type (-> String Integer (Values List Integer)))
  (doc 'description "Parse JSON null literal (returns empty list).")
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "null"))
      (values '() (+ pos 4))
      (values #f pos)))

(doc 'section 'json-serialization)

(define (json->string v)
  (doc 'type (-> Any String))
  (doc 'description "Convert Scheme value to JSON string.")
  (doc 'export #t)
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

(define (json-object->string alist)
  (doc 'type (-> Alist String))
  (doc 'description "Convert alist to JSON object string.")
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

(define (json-array->string lst)
  (doc 'type (-> List String))
  (doc 'description "Convert list to JSON array string.")
  (string-append
   "["
   (json-intersperse ", " (map json->string lst))
   "]"))

(define (json-escape s)
  (doc 'type (-> String String))
  (doc 'description "Escape a string for JSON (handle quotes, newlines, backslashes, control chars).")
  (doc 'export #t)
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

(define (json-intersperse sep lst)
  (doc 'type (-> String (List String) String))
  (doc 'description "Join strings with separator.")
  (cond
   [(null? lst) ""]
   [(null? (cdr lst)) (car lst)]
   [else (string-append (car lst) sep (json-intersperse sep (cdr lst)))]))
