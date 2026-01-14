;;; core/lsp/json.ss — JSON Parser & Serializer
;;; @module json
;;; @requires prelude
;;;
;;; Pure Scheme JSON implementation for LSP protocol.
;;;
;;; JSON Representation:
;;;   null   → 'null
;;;   true   → #t
;;;   false  → #f
;;;   number → number
;;;   string → string
;;;   array  → (json-array elem ...)
;;;   object → (json-object (key . value) ...)
;;;
;;; Provides:
;;;   (json-read str)       → (ok Value) | (error msg)
;;;   (json-write val)      → String
;;;   (json-get obj key)    → Value | #f
;;;   (json-object? x)      → Boolean
;;;   (json-array? x)       → Boolean
;;;
;;; This is Core code: pure, total.

(load "core/base/prelude.ss")

;;; ====
;;; Type Predicates
;;; ====

;;; json-object? : Any → Boolean
(define (json-object? x)
  (and (pair? x) (eq? (car x) 'json-object)))

;;; json-array? : Any → Boolean
(define (json-array? x)
  (and (pair? x) (eq? (car x) 'json-array)))

;;; json-null? : Any → Boolean
(define (json-null? x)
  (eq? x 'null))

;;; ====
;;; Object Access
;;; ====

;;; json-get : JsonObject × String → Value | #f
;;; Get a value from a JSON object by key.
(define (json-get obj key)
  (if (json-object? obj)
      (let ([entry (assoc key (cdr obj))])
           (and entry (cdr entry)))
      #f))

;;; json-get-path : JsonValue × (List String) → Value | #f
;;; Navigate nested objects by path.
(define (json-get-path val path)
  (if (null? path)
      val
      (let ([next (json-get val (car path))])
           (if next
               (json-get-path next (cdr path))
               #f))))

;;; ====
;;; Constructors
;;; ====

;;; json-obj : (key val ...) → JsonObject
;;; Construct a JSON object from key-value pairs.
(define (json-obj . pairs)
  (cons 'json-object
        (let loop ([ps pairs] [acc '()])
             (if (or (null? ps) (null? (cdr ps)))
                 (reverse acc)
                 (loop (cddr ps) (cons (cons (car ps) (cadr ps)) acc))))))

;;; json-arr : (elem ...) → JsonArray
;;; Construct a JSON array from elements.
(define (json-arr . elems)
  (cons 'json-array elems))

;;; ====
;;; Serializer
;;; ====

;;; json-write : JsonValue → String
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

;;; json-write-number : Number → String
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

;;; json-escape-string : String → String
;;; Escape a string for JSON output (includes quotes).
;;; Optimized to use output port instead of list cons + reverse.
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

;;; json-encode-unicode : Char → String
;;; Encode a character as \uXXXX.
(define (json-encode-unicode c)
  (let* ([n (char->integer c)]
         [hex (number->string n 16)]
         [padded (string-append (make-string (- 4 (string-length hex)) #\0) hex)])
        (string-append "\\u" padded)))

;;; json-join : (List String) × String → String
(define (json-join strs sep)
  (if (null? strs)
      ""
      (let loop ([ss (cdr strs)] [acc (car strs)])
           (if (null? ss)
               acc
               (loop (cdr ss) (string-append acc sep (car ss)))))))

;;; ====
;;; Parser State
;;; ====

;;; Parser operates on a string with an index.
;;; State = (index . string)

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

;;; ====
;;; Parser Primitives
;;; ====

;;; skip-whitespace : State → State
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

;;; match-string : State × String → State | #f
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

;;; ====
;;; JSON Parser
;;; ====

;;; Maximum nesting depth to prevent stack overflow DoS attacks.
;;; RFC 8259 doesn't specify a limit, but 100 levels is reasonable.
(define *json-max-depth* 100)

;;; json-read : String → (ok Value) | (error String)
;;; SECURITY: Uses depth-limited parsing to prevent stack overflow.
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

;;; parse-value : State → (Value . State) | ErrorString
;;; NOTE: This is the legacy interface. Use parse-value-with-depth for security.
(define (parse-value s)
  (parse-value-with-depth s 0))

;;; parse-value-with-depth : State × Int → (Value . State) | ErrorString
;;; SECURITY: Tracks nesting depth to prevent stack overflow DoS.
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

;;; parse-null : State → (null . State) | ErrorString
(define (parse-null s)
  (let ([next (match-string s "null")])
       (if next
           (cons 'null next)
           "Expected 'null'")))

;;; parse-true : State → (#t . State) | ErrorString
(define (parse-true s)
  (let ([next (match-string s "true")])
       (if next
           (cons #t next)
           "Expected 'true'")))

;;; parse-false : State → (#f . State) | ErrorString
(define (parse-false s)
  (let ([next (match-string s "false")])
       (if next
           (cons #f next)
           "Expected 'false'")))

;;; parse-string : State → (String . State) | ErrorString
;;; Optimized to use output port instead of list cons + reverse.
(define (parse-string s)
  (if (not (char=? (pstate-peek s) #\"))
      "Expected '\"'"
      (let ([s (pstate-advance s)])  ; skip opening quote
           (parse-string-body s))))

;;; parse-string-body : State → (String . State) | ErrorString
;;; Parse the body of a string using output port for efficiency.
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

;;; parse-escape : State → (Char . State) | ErrorString
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

;;; Unicode replacement character for invalid surrogates
(define *replacement-char* (integer->char #xFFFD))

;;; parse-unicode-escape : State → (Char . State) | ErrorString
;;; Parse \uXXXX escape sequence, with surrogate pair support.
;;; JSON encodes non-BMP characters (U+10000+) as surrogate pairs:
;;;   \uD800-\uDBFF (high surrogate) followed by \uDC00-\uDFFF (low surrogate)
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

;;; parse-surrogate-pair : Int × State → (Char . State) | ErrorString
;;; Given a high surrogate, try to parse the following low surrogate.
;;; If found, combine them into a single non-BMP character.
;;; If not found, use the Unicode replacement character (U+FFFD) since
;;; lone surrogates are not valid Unicode scalar values.
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

;;; parse-number : State → (Number . State) | ErrorString
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

;;; parse-number-finish : State × Start × End → (Number . State) | ErrorString
(define (parse-number-finish s start end)
  (if (= start end)
      "Expected number"
      (let* ([str (pstate-str s)]
             [num-str (substring str start end)]
             [n (string->number num-str)])
            (if n
                (cons n (pstate-advance-n s (- end start)))
                (string-append "Invalid number: " num-str)))))

;;; parse-array : State → (JsonArray . State) | ErrorString
(define (parse-array s)
  (parse-array-with-depth s 0))

;;; parse-array-with-depth : State Int → (JsonArray . State) | ErrorString
;;; SECURITY: Checks nesting depth to prevent stack overflow DoS.
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

;;; parse-object : State → (JsonObject . State) | ErrorString
(define (parse-object s)
  (parse-object-with-depth s 0))

;;; parse-object-with-depth : State Int → (JsonObject . State) | ErrorString
;;; SECURITY: Checks nesting depth to prevent stack overflow DoS.
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

;;; ====
;;; Tests (run with test-framework)
;;; ====

;; Self-test when loaded directly
(when (top-level-bound? '*running-tests*)
      (display "json.ss loaded for testing\n"))
