(load "core/base/prelude.ss")

(doc 'module 'lsp/json)
(doc 'description "Pure Scheme JSON implementation for LSP protocol. Provides JSON parsing/serialization with proper UTF-16 handling and security limits.")
(doc 'layer 'boundary)
(doc 'purity 'total)
(doc 'requires '(prelude))

(doc 'note "JSON Representation: null→'null, true→#t, false→#f, number→number, string→string, array→(json-array elem...), object→(json-object (key.value)...)")

(doc 'section 'type-predicates)

(doc json-object? 'type '(-> Any Boolean))
(define (json-object? x)
  (and (pair? x) (eq? (car x) 'json-object)))

(doc json-array? 'type '(-> Any Boolean))
(define (json-array? x)
  (and (pair? x) (eq? (car x) 'json-array)))

(doc json-null? 'type '(-> Any Boolean))
(define (json-null? x)
  (eq? x 'null))

(doc 'section 'object-access)

(doc json-get 'type '(-> JsonObject String (U Value #f)))
(doc json-get 'description "Get a value from a JSON object by key")
(define (json-get obj key)
  (if (json-object? obj)
      (let ([entry (assoc key (cdr obj))])
           (and entry (cdr entry)))
      #f))

(doc json-get-path 'type '(-> JsonValue (List String) (U Value #f)))
(doc json-get-path 'description "Navigate nested objects by path")
(define (json-get-path val path)
  (if (null? path)
      val
      (let ([next (json-get val (car path))])
           (if next
               (json-get-path next (cdr path))
               #f))))

(doc 'section 'constructors)

(doc json-obj 'type '(-> (* (Pair String Value)) JsonObject))
(doc json-obj 'description "Construct a JSON object from key-value pairs")
(define (json-obj . pairs)
  (cons 'json-object
        (let loop ([ps pairs] [acc '()])
             (if (or (null? ps) (null? (cdr ps)))
                 (reverse acc)
                 (loop (cddr ps) (cons (cons (car ps) (cadr ps)) acc))))))

(doc json-arr 'type '(-> (* Value) JsonArray))
(doc json-arr 'description "Construct a JSON array from elements")
(define (json-arr . elems)
  (cons 'json-array elems))

(doc 'section 'serializer)

(doc json-write 'type '(-> JsonValue String))
(doc json-write 'description "Serialize a JSON value to a string")
(define (json-write val)
  (cond
   [(eq? val 'null) "null"]
   [(eq? val #t) "true"]
   [(eq? val #f) "false"]
   [(number? val) (json-write-number val)]
   [(string? val) (json-escape-string val)]
   [(json-array? val)
    (string-append "["
                   (json-join (map json-write (cdr val)) ",")
                   "]")]
   [(json-object? val)
    (string-append "{"
                   (json-join
                    (map (lambda (entry)
                                 (string-append (json-escape-string (car entry))
                                                ":"
                                                (json-write (cdr entry))))
                         (cdr val))
                    ",")
                   "}")]
   [else (error 'json-write "Invalid JSON value" val)]))

(doc json-write-number 'type '(-> Number String))
(doc json-write-number 'description "Serialize a number ensuring valid JSON representation")
(define (json-write-number n)
  (if (integer? n)
      (number->string n)
      ;; Floats: ensure valid JSON representation
      (let ([s (number->string (inexact n))])
           ;; Ensure we have a decimal point OR scientific notation
           ;; Scientific notation (e.g., "1e+20") is valid JSON without decimal
           (if (or (string-contains? s ".")
                   (string-contains? s "e")
                   (string-contains? s "E"))
               s
               (string-append s ".0")))))

(doc json-escape-string 'type '(-> String String))
(doc json-escape-string 'description "Escape a string for JSON output (includes quotes)")
(doc json-escape-string 'note "Optimized to use output port instead of list cons + reverse")
(define (json-escape-string str)
  (call-with-string-output-port
   (lambda (out)
           (put-char out #\")
           (let ([len (string-length str)])
                (let loop ([i 0])
                     (when (< i len)
                           (let ([c (string-ref str i)])
                                (case c
                                      [(#\") (put-string out "\\\"")]
                                      [(#\\) (put-string out "\\\\")]
                                      [(#\backspace) (put-string out "\\b")]
                                      [(#\page) (put-string out "\\f")]
                                      [(#\newline) (put-string out "\\n")]
                                      [(#\return) (put-string out "\\r")]
                                      [(#\tab) (put-string out "\\t")]
                                      [else
                                       ;; Control characters need \uXXXX encoding
                                       (if (< (char->integer c) 32)
                                           (put-string out (json-encode-unicode c))
                                           (put-char out c))])
                                (loop (+ i 1))))))
           (put-char out #\"))))

(doc json-encode-unicode 'type '(-> Char String))
(doc json-encode-unicode 'description "Encode a character as \\uXXXX")
(define (json-encode-unicode c)
  (let* ([n (char->integer c)]
         [hex (number->string n 16)]
         [padded (string-append (make-string (- 4 (string-length hex)) #\0) hex)])
        (string-append "\\u" padded)))

(doc json-join 'type '(-> (List String) String String))
(define (json-join strs sep)
  (if (null? strs)
      ""
      (let loop ([ss (cdr strs)] [acc (car strs)])
           (if (null? ss)
               acc
               (loop (cdr ss) (string-append acc sep (car ss)))))))

(doc 'section 'parser-state)

(doc 'note "Parser operates on a string with an index. State = (index . string)")

(define (make-pstate str)
  (cons 0 str))

(define (pstate-index s)
  (car s))

(define (pstate-str s)
  (cdr s))

(define (pstate-remaining s)
  (- (string-length (pstate-str s)) (pstate-index s)))

(define (pstate-empty? s)
  (<= (pstate-remaining s) 0))

(define (pstate-peek s)
  (string-ref (pstate-str s) (pstate-index s)))

(define (pstate-advance s)
  (cons (+ 1 (pstate-index s)) (pstate-str s)))

(define (pstate-advance-n s n)
  (cons (+ n (pstate-index s)) (pstate-str s)))

(doc 'section 'parser-primitives)

(doc skip-whitespace 'type '(-> State State))
(define (skip-whitespace s)
  (if (pstate-empty? s)
      s
      (let ([c (pstate-peek s)])
           (if (or (char=? c #\space)
                   (char=? c #\tab)
                   (char=? c #\newline)
                   (char=? c #\return))
               (skip-whitespace (pstate-advance s))
               s))))

(doc match-string 'type '(-> State String (U State #f)))
(define (match-string s target)
  (let ([len (string-length target)]
        [str (pstate-str s)]
        [idx (pstate-index s)])
       (if (< (pstate-remaining s) len)
           #f
           (let loop ([i 0])
                (if (= i len)
                    (pstate-advance-n s len)
                    (if (char=? (string-ref str (+ idx i))
                                (string-ref target i))
                        (loop (+ i 1))
                        #f))))))

(doc 'section 'json-parser)

(doc 'note "Maximum nesting depth to prevent stack overflow DoS attacks. RFC 8259 doesn't specify a limit, but 100 levels is reasonable.")
(define *json-max-depth* 100)

(doc json-read 'type '(-> String (U (ok Value) (error String))))
(doc json-read 'description "Parse a JSON string")
(doc json-read 'security "Uses depth-limited parsing to prevent stack overflow")
(define (json-read str)
  (let ([result (parse-value-with-depth (skip-whitespace (make-pstate str)) 0)])
       (if (pair? result)
           (let ([val (car result)]
                 [rest (skip-whitespace (cdr result))])
                (if (pstate-empty? rest)
                    `(ok ,val)
                    `(error ,(string-append "Unexpected content after JSON at position "
                                            (number->string (pstate-index rest))))))
           `(error ,result))))

(doc parse-value 'type '(-> State (U (Pair Value State) ErrorString)))
(doc parse-value 'note "Legacy interface. Use parse-value-with-depth for security.")
(define (parse-value s)
  (parse-value-with-depth s 0))

(doc parse-value-with-depth 'type '(-> State Int (U (Pair Value State) ErrorString)))
(doc parse-value-with-depth 'security "Tracks nesting depth to prevent stack overflow DoS")
(define (parse-value-with-depth s depth)
  (if (pstate-empty? s)
      "Unexpected end of input"
      (let ([c (pstate-peek s)])
           (cond
            [(char=? c #\n) (parse-null s)]
            [(char=? c #\t) (parse-true s)]
            [(char=? c #\f) (parse-false s)]
            [(char=? c #\") (parse-string s)]
            [(char=? c #\[) (parse-array-with-depth s depth)]
            [(char=? c #\{) (parse-object-with-depth s depth)]
            [(or (char=? c #\-) (char-numeric? c)) (parse-number s)]
            [else (string-append "Unexpected character: " (string c))]))))

(doc parse-null 'type '(-> State (U (Pair Null State) ErrorString)))
(define (parse-null s)
  (let ([next (match-string s "null")])
       (if next
           (cons 'null next)
           "Expected 'null'")))

(doc parse-true 'type '(-> State (U (Pair Boolean State) ErrorString)))
(define (parse-true s)
  (let ([next (match-string s "true")])
       (if next
           (cons #t next)
           "Expected 'true'")))

(doc parse-false 'type '(-> State (U (Pair Boolean State) ErrorString)))
(define (parse-false s)
  (let ([next (match-string s "false")])
       (if next
           (cons #f next)
           "Expected 'false'")))

(doc parse-string 'type '(-> State (U (Pair String State) ErrorString)))
(doc parse-string 'note "Optimized to use output port instead of list cons + reverse")
(define (parse-string s)
  (if (not (char=? (pstate-peek s) #\"))
      "Expected '\"'"
      (let ([s (pstate-advance s)])  ; skip opening quote
           (parse-string-body s))))

(doc parse-string-body 'type '(-> State (U (Pair String State) ErrorString)))
(doc parse-string-body 'description "Parse the body of a string using output port for efficiency")
(define (parse-string-body start-state)
  (let* ([result-str #f]
         [end-state #f]
         [error-msg #f])
        ;; Use call-with-string-output-port to build string efficiently
        (set! result-str
              (call-with-string-output-port
               (lambda (out)
                       (let loop ([s start-state])
                            (cond
                             [(pstate-empty? s)
                              (set! error-msg "Unterminated string")]
                             [else
                              (let ([c (pstate-peek s)])
                                   (cond
                                    [(char=? c #\")
                                     (set! end-state (pstate-advance s))]
                                    [(char=? c #\\)
                                     (let ([esc-result (parse-escape (pstate-advance s))])
                                          (if (pair? esc-result)
                                              (begin
                                               (put-char out (car esc-result))
                                               (loop (cdr esc-result)))
                                              (set! error-msg esc-result)))]
                                    [else
                                     (put-char out c)
                                     (loop (pstate-advance s))]))])))))
        (cond
         [error-msg error-msg]
         [end-state (cons result-str end-state)]
         [else "Unterminated string"])))

(doc parse-escape 'type '(-> State (U (Pair Char State) ErrorString)))
(define (parse-escape s)
  (if (pstate-empty? s)
      "Unterminated escape sequence"
      (let ([c (pstate-peek s)])
           (case c
                 [(#\") (cons #\" (pstate-advance s))]
                 [(#\\) (cons #\\ (pstate-advance s))]
                 [(#\/) (cons #\/ (pstate-advance s))]
                 [(#\b) (cons #\backspace (pstate-advance s))]
                 [(#\f) (cons #\page (pstate-advance s))]
                 [(#\n) (cons #\newline (pstate-advance s))]
                 [(#\r) (cons #\return (pstate-advance s))]
                 [(#\t) (cons #\tab (pstate-advance s))]
                 [(#\u) (parse-unicode-escape (pstate-advance s))]
                 [else (string-append "Invalid escape character: " (string c))]))))

(doc 'note "Unicode replacement character for invalid surrogates")
(define *replacement-char* (integer->char #xFFFD))

(doc parse-unicode-escape 'type '(-> State (U (Pair Char State) ErrorString)))
(doc parse-unicode-escape 'description "Parse \\uXXXX escape sequence, with surrogate pair support")
(doc parse-unicode-escape 'note "JSON encodes non-BMP characters (U+10000+) as surrogate pairs: \\uD800-\\uDBFF (high surrogate) followed by \\uDC00-\\uDFFF (low surrogate)")
(define (parse-unicode-escape s)
  (if (< (pstate-remaining s) 4)
      "Incomplete unicode escape"
      (let* ([str (pstate-str s)]
             [idx (pstate-index s)]
             [hex (substring str idx (+ idx 4))]
             [n (string->number hex 16)])
            (if (not n)
                (string-append "Invalid unicode escape: " hex)
                (let ([s2 (pstate-advance-n s 4)])
                     (cond
                      ;; High surrogate (U+D800 to U+DBFF) - look for pair
                      [(and (>= n #xD800) (<= n #xDBFF))
                       (parse-surrogate-pair n s2)]
                      ;; Lone low surrogate (U+DC00 to U+DFFF) - invalid, use replacement
                      [(and (>= n #xDC00) (<= n #xDFFF))
                       (cons *replacement-char* s2)]
                      ;; Regular character
                      [else
                       (cons (integer->char n) s2)]))))))

(doc parse-surrogate-pair 'type '(-> Int State (U (Pair Char State) ErrorString)))
(doc parse-surrogate-pair 'description "Given a high surrogate, try to parse the following low surrogate. If found, combine them into a single non-BMP character. If not found, use the Unicode replacement character (U+FFFD) since lone surrogates are not valid Unicode scalar values.")
(define (parse-surrogate-pair high s)
  (let ([str (pstate-str s)]
        [idx (pstate-index s)])
       ;; Check for \uXXXX sequence (need 6 more chars: \u + 4 hex)
       (if (and (>= (pstate-remaining s) 6)
                (char=? (string-ref str idx) #\\)
                (char=? (string-ref str (+ idx 1)) #\u))
           ;; Parse the potential low surrogate
           (let* ([hex2 (substring str (+ idx 2) (+ idx 6))]
                  [low (string->number hex2 16)])
                 (if (and low (>= low #xDC00) (<= low #xDFFF))
                     ;; Valid surrogate pair - combine them
                     ;; Formula: 0x10000 + ((high - 0xD800) << 10) + (low - 0xDC00)
                     (let ([codepoint (+ #x10000
                                         (bitwise-arithmetic-shift-left (- high #xD800) 10)
                                         (- low #xDC00))])
                          (cons (integer->char codepoint)
                                (pstate-advance-n s 6)))  ; Skip \uXXXX
                     ;; Not a valid low surrogate - use replacement char
                     (cons *replacement-char* s)))
           ;; No following escape - use replacement char
           (cons *replacement-char* s))))

(doc parse-number 'type '(-> State (U (Pair Number State) ErrorString)))
(define (parse-number s)
  (let* ([str (pstate-str s)]
         [start (pstate-index s)]
         [len (string-length str)])
        (let loop ([i start] [has-dot #f] [has-exp #f])
             (if (>= i len)
                 (parse-number-finish s start i)
                 (let ([c (string-ref str i)])
                      (cond
                       [(char-numeric? c) (loop (+ i 1) has-dot has-exp)]
                       [(and (= i start) (char=? c #\-)) (loop (+ i 1) has-dot has-exp)]
                       [(and (not has-dot) (not has-exp) (char=? c #\.))
                        (loop (+ i 1) #t has-exp)]
                       [(and (not has-exp) (or (char=? c #\e) (char=? c #\E)))
                        (loop (+ i 1) has-dot #t)]
                       [(and has-exp (or (char=? c #\+) (char=? c #\-))
                             (let ([prev (string-ref str (- i 1))])
                                  (or (char=? prev #\e) (char=? prev #\E))))
                        (loop (+ i 1) has-dot has-exp)]
                       [else (parse-number-finish s start i)]))))))

(doc parse-number-finish 'type '(-> State Int Int (U (Pair Number State) ErrorString)))
(define (parse-number-finish s start end)
  (if (= start end)
      "Expected number"
      (let* ([str (pstate-str s)]
             [num-str (substring str start end)]
             [n (string->number num-str)])
            (if n
                (cons n (pstate-advance-n s (- end start)))
                (string-append "Invalid number: " num-str)))))

(doc parse-array 'type '(-> State (U (Pair JsonArray State) ErrorString)))
(define (parse-array s)
  (parse-array-with-depth s 0))

(doc parse-array-with-depth 'type '(-> State Int (U (Pair JsonArray State) ErrorString)))
(doc parse-array-with-depth 'security "Checks nesting depth to prevent stack overflow DoS")
(define (parse-array-with-depth s depth)
  (if (>= depth *json-max-depth*)
      (string-append "JSON nesting too deep (max " (number->string *json-max-depth*) " levels)")
      (if (not (char=? (pstate-peek s) #\[))
          "Expected '['"
          (let ([s (skip-whitespace (pstate-advance s))]
                [new-depth (+ depth 1)])
               (if (and (not (pstate-empty? s)) (char=? (pstate-peek s) #\]))
                   (cons (cons 'json-array '()) (pstate-advance s))
                   (let loop ([s s] [elems '()])
                        (let ([result (parse-value-with-depth s new-depth)])
                             (if (string? result)
                                 result
                                 (let* ([val (car result)]
                                        [rest (skip-whitespace (cdr result))])
                                       (if (pstate-empty? rest)
                                           "Unterminated array"
                                           (let ([c (pstate-peek rest)])
                                                (cond
                                                 [(char=? c #\])
                                                  (cons (cons 'json-array (reverse (cons val elems)))
                                                        (pstate-advance rest))]
                                                 [(char=? c #\,)
                                                  (loop (skip-whitespace (pstate-advance rest))
                                                        (cons val elems))]
                                                 [else "Expected ',' or ']' in array"]))))))))))))

(doc parse-object 'type '(-> State (U (Pair JsonObject State) ErrorString)))
(define (parse-object s)
  (parse-object-with-depth s 0))

(doc parse-object-with-depth 'type '(-> State Int (U (Pair JsonObject State) ErrorString)))
(doc parse-object-with-depth 'security "Checks nesting depth to prevent stack overflow DoS")
(define (parse-object-with-depth s depth)
  (if (>= depth *json-max-depth*)
      (string-append "JSON nesting too deep (max " (number->string *json-max-depth*) " levels)")
      (if (not (char=? (pstate-peek s) #\{))
          "Expected '{'"
          (let ([s (skip-whitespace (pstate-advance s))]
                [new-depth (+ depth 1)])
               (if (and (not (pstate-empty? s)) (char=? (pstate-peek s) #\}))
                   (cons (cons 'json-object '()) (pstate-advance s))
                   (let loop ([s s] [entries '()])
                        (let ([key-result (parse-string s)])
                             (if (string? key-result)
                                 (string-append "Expected string key: " key-result)
                                 (let* ([key (car key-result)]
                                        [rest (skip-whitespace (cdr key-result))])
                                       (if (or (pstate-empty? rest)
                                               (not (char=? (pstate-peek rest) #\:)))
                                           "Expected ':' after object key"
                                           (let ([val-result (parse-value-with-depth
                                                              (skip-whitespace (pstate-advance rest))
                                                              new-depth)])
                                                (if (string? val-result)
                                                    val-result
                                                    (let* ([val (car val-result)]
                                                           [entry (cons key val)]
                                                           [rest2 (skip-whitespace (cdr val-result))])
                                                          (if (pstate-empty? rest2)
                                                              "Unterminated object"
                                                              (let ([c (pstate-peek rest2)])
                                                                   (cond
                                                                    [(char=? c #\})
                                                                     (cons (cons 'json-object
                                                                                 (reverse (cons entry entries)))
                                                                           (pstate-advance rest2))]
                                                                    [(char=? c #\,)
                                                                     (loop (skip-whitespace (pstate-advance rest2))
                                                                           (cons entry entries))]
                                                                    [else "Expected ',' or '}' in object"]))))))))))))))))

(doc 'section 'tests)

(when (top-level-bound? '*running-tests*)
      (display "json.ss loaded for testing\n"))
