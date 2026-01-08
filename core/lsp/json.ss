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

;;; ============================================================
;;; Type Predicates
;;; ============================================================

;;; json-object? : Any → Boolean
(define (json-object? x)
  (and (pair? x) (eq? (car x) 'json-object)))

;;; json-array? : Any → Boolean
(define (json-array? x)
  (and (pair? x) (eq? (car x) 'json-array)))

;;; json-null? : Any → Boolean
(define (json-null? x)
  (eq? x 'null))

;;; ============================================================
;;; Object Access
;;; ============================================================

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

;;; ============================================================
;;; Constructors
;;; ============================================================

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

;;; ============================================================
;;; Serializer
;;; ============================================================

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
      ;; Floats: avoid exponential notation for small numbers
      (let ([s (number->string (inexact n))])
           ;; Ensure we have a decimal point
           (if (string-contains? s ".")
               s
               (string-append s ".0")))))

;;; json-escape-string : String → String
;;; Escape a string for JSON output (includes quotes).
(define (json-escape-string str)
  (let loop ([chars (string->list str)] [acc (list #\")])
       (if (null? chars)
           (list->string (reverse (cons #\" acc)))
           (let ([c (car chars)])
                (case c
                      [(#\") (loop (cdr chars) (cons #\" (cons #\\ acc)))]
                      [(#\\) (loop (cdr chars) (cons #\\ (cons #\\ acc)))]
                      [(#\backspace) (loop (cdr chars) (cons #\b (cons #\\ acc)))]
                      [(#\page) (loop (cdr chars) (cons #\f (cons #\\ acc)))]
                      [(#\newline) (loop (cdr chars) (cons #\n (cons #\\ acc)))]
                      [(#\return) (loop (cdr chars) (cons #\r (cons #\\ acc)))]
                      [(#\tab) (loop (cdr chars) (cons #\t (cons #\\ acc)))]
                      [else
                       ;; Control characters need \uXXXX encoding
                       (if (< (char->integer c) 32)
                           (let ([hex (json-encode-unicode c)])
                                (loop (cdr chars) (append (reverse (string->list hex)) acc)))
                           (loop (cdr chars) (cons c acc)))])))))

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

;;; ============================================================
;;; Parser State
;;; ============================================================

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

;;; ============================================================
;;; Parser Primitives
;;; ============================================================

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

;;; ============================================================
;;; JSON Parser
;;; ============================================================

;;; json-read : String → (ok Value) | (error String)
(define (json-read str)
  (let ([result (parse-value (skip-whitespace (make-pstate str)))])
       (if (pair? result)
           (let ([val (car result)]
                 [rest (skip-whitespace (cdr result))])
                (if (pstate-empty? rest)
                    `(ok ,val)
                    `(error ,(string-append "Unexpected content after JSON at position "
                                            (number->string (pstate-index rest))))))
           `(error ,result))))

;;; parse-value : State → (Value . State) | ErrorString
(define (parse-value s)
  (if (pstate-empty? s)
      "Unexpected end of input"
      (let ([c (pstate-peek s)])
           (cond
            [(char=? c #\n) (parse-null s)]
            [(char=? c #\t) (parse-true s)]
            [(char=? c #\f) (parse-false s)]
            [(char=? c #\") (parse-string s)]
            [(char=? c #\[) (parse-array s)]
            [(char=? c #\{) (parse-object s)]
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
(define (parse-string s)
  (if (not (char=? (pstate-peek s) #\"))
      "Expected '\"'"
      (let ([s (pstate-advance s)])  ; skip opening quote
           (let loop ([s s] [chars '()])
                (if (pstate-empty? s)
                    "Unterminated string"
                    (let ([c (pstate-peek s)])
                         (cond
                          [(char=? c #\")
                           (cons (list->string (reverse chars))
                                 (pstate-advance s))]
                          [(char=? c #\\)
                           (let ([esc-result (parse-escape (pstate-advance s))])
                                (if (pair? esc-result)
                                    (loop (cdr esc-result) (cons (car esc-result) chars))
                                    esc-result))]
                          [else
                           (loop (pstate-advance s) (cons c chars))])))))))

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

;;; parse-unicode-escape : State → (Char . State) | ErrorString
;;; Parse \uXXXX escape sequence.
(define (parse-unicode-escape s)
  (if (< (pstate-remaining s) 4)
      "Incomplete unicode escape"
      (let* ([str (pstate-str s)]
             [idx (pstate-index s)]
             [hex (substring str idx (+ idx 4))])
            (let ([n (string->number hex 16)])
                 (if n
                     (cons (integer->char n) (pstate-advance-n s 4))
                     (string-append "Invalid unicode escape: " hex))))))

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
  (if (not (char=? (pstate-peek s) #\[))
      "Expected '['"
      (let ([s (skip-whitespace (pstate-advance s))])
           (if (and (not (pstate-empty? s)) (char=? (pstate-peek s) #\]))
               (cons (cons 'json-array '()) (pstate-advance s))
               (let loop ([s s] [elems '()])
                    (let ([result (parse-value s)])
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
                                             [else "Expected ',' or ']' in array"])))))))))))

;;; parse-object : State → (JsonObject . State) | ErrorString
(define (parse-object s)
  (if (not (char=? (pstate-peek s) #\{))
      "Expected '{'"
      (let ([s (skip-whitespace (pstate-advance s))])
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
                                       (let ([val-result (parse-value
                                                          (skip-whitespace (pstate-advance rest)))])
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
                                                                [else "Expected ',' or '}' in object"])))))))))))))))

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; string-contains? : String × String → Boolean
(define (string-contains? str substr)
  (let ([slen (string-length str)]
        [sublen (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sublen) slen) #f]
             [(string-prefix? (substring str i slen) substr) #t]
             [else (loop (+ i 1))]))))

;;; string-prefix? : String × String → Boolean
(define (string-prefix? str prefix)
  (let ([slen (string-length str)]
        [plen (string-length prefix)])
       (and (>= slen plen)
            (let loop ([i 0])
                 (or (= i plen)
                     (and (char=? (string-ref str i) (string-ref prefix i))
                          (loop (+ i 1))))))))

;;; ============================================================
;;; Tests (run with test-framework)
;;; ============================================================

;; Self-test when loaded directly
(when (top-level-bound? '*running-tests*)
      (display "json.ss loaded for testing\n"))
