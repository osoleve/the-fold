(load "core/base/prelude.ss")
(load "lattice/fp/parsing/parser.ss")

(doc 'module 'parser-examples)
(doc 'description "Example Parsers Built with Combinators — Demonstrates building practical parsers using the parser combinator DSL. Shows common patterns and idioms.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'examples '(json s-expression arithmetic ini-file))

(doc 'section 'character-constants)

(define %newline-ex (integer->char 10))
(define %tab-ex (integer->char 9))
(define %return-ex (integer->char 13))
(define %backspace-ex (integer->char 8))
(define %page-ex (integer->char 12))
(define %backslash (integer->char 92))
(define %char-b (integer->char 98))
(define %char-f (integer->char 102))
(define %char-n (integer->char 110))
(define %char-r (integer->char 114))
(define %char-t (integer->char 116))

(doc 'section 'json-parser)

(define json-null (parser-then (symbol "null") (parser-pure 'json-null)))
(doc json-null 'type '(Parser Symbol))
(doc json-null 'description "Parse JSON null value")

(define json-bool
  (parser-or
   (parser-then (symbol "true") (parser-pure #t))
   (parser-then (symbol "false") (parser-pure #f))))
(doc json-bool 'type '(Parser Boolean))
(doc json-bool 'description "Parse JSON boolean value")

(define json-number (lexeme decimal))
(doc json-number 'type '(Parser Number))
(doc json-number 'description "Parse JSON number")

(define json-string-char
  (parser-or
   ;; Escape sequences
   (parser-then
    (char %backslash)
    (choice (list
             (parser-then (char #\") (parser-pure #\"))
             (parser-then (char %backslash) (parser-pure %backslash))
             (parser-then (char #\/) (parser-pure #\/))
             (parser-then (char %char-b) (parser-pure %backspace-ex))
             (parser-then (char %char-f) (parser-pure %page-ex))
             (parser-then (char %char-n) (parser-pure %newline-ex))
             (parser-then (char %char-r) (parser-pure %return-ex))
             (parser-then (char %char-t) (parser-pure %tab-ex)))))
   ;; Regular characters (not " or \)
   (satisfy (lambda (c) (and (not (char=? c #\"))
                             (not (char=? c %backslash))))
            "string character")))
(doc json-string-char 'type '(Parser Char))
(doc json-string-char 'description "Parse a single character inside a string (handling escapes)")

(define json-string
  (lexeme
   (between (char #\")
            (char #\")
            (parser-map list->string (many json-string-char)))))
(doc json-string 'type '(Parser String))
(doc json-string 'description "Parse JSON string")

(define json-value #f)
(doc json-value 'type '(Parser JsonValue))
(doc json-value 'description "Forward declaration for recursive parsers")

(define json-array
  (brackets (comma-sep (make-parser (lambda (s) (run-parser json-value s))))))
(doc json-array 'type '(Parser (List JsonValue)))
(doc json-array 'description "Parse JSON array")

(define json-pair
  (parser-bind json-string
               (lambda (key)
                       (parser-then
                        (symbol ":")
                        (parser-bind (make-parser (lambda (s) (run-parser json-value s)))
                                     (lambda (val)
                                             (parser-pure (cons key val))))))))
(doc json-pair 'type '(Parser (Pair String JsonValue)))
(doc json-pair 'description "Parse JSON object key-value pair")

(define json-object (braces (comma-sep json-pair)))
(doc json-object 'type '(Parser (Alist String JsonValue)))
(doc json-object 'description "Parse JSON object")

(set! json-value
      (choice (list json-null
                    json-bool
                    json-number
                    json-string
                    json-array
                    json-object)))

(define (parse-json input)
  (doc 'type '(-> String (Either Error JsonValue)))
  (doc 'description "Parse complete JSON value")
  (parse-all json-value input))

(doc 'section 's-expression-parser)

(define sexp-comment
  (parser-then (char #\;)
               (parser-then (many (satisfy (lambda (c) (not (char=? c %newline-ex)))
                                           "non-newline"))
                            (parser-pure '()))))
(doc sexp-comment 'type '(Parser Unit))
(doc sexp-comment 'description "Skip line comment")

(define sexp-whitespace (many (parser-or space sexp-comment)))
(doc sexp-whitespace 'type '(Parser (List α)))
(doc sexp-whitespace 'description "Skip whitespace and comments")

(define (sexp-lexeme p)
  (parser-left p sexp-whitespace))
(doc sexp-lexeme 'type '(-> (Parser α) (Parser α)))
(doc sexp-lexeme 'description "Parse and consume trailing S-expression whitespace/comments")

(define sexp-symbol-char
  (satisfy (lambda (c)
                   (or (char-alphabetic? c)
                       (char-numeric? c)
                       (memv c '(#\+ #\- #\* #\/ #\< #\> #\= #\? #\! #\_ #\: #\. #\@))))
           "symbol character"))
(doc sexp-symbol-char 'type '(Parser Char))
(doc sexp-symbol-char 'description "Parse a valid symbol character")

(define sexp-symbol
  (sexp-lexeme
   (parser-bind (some sexp-symbol-char)
                (lambda (chars)
                        (let ([str (list->string chars)])
                             ;; Try to parse as number first
                             (let ([num (string->number str)])
                                  (parser-pure (if num num (string->symbol str)))))))))
(doc sexp-symbol 'type '(Parser (Union Symbol Number)))
(doc sexp-symbol 'description "Parse symbol or number")

(define sexp-string
  (sexp-lexeme
   (between (char #\")
            (char #\")
            (parser-map list->string
                        (many (parser-or
                               ;; Escaped characters
                               (parser-then (char %backslash)
                                            (choice (list
                                                     (parser-then (char #\") (parser-pure #\"))
                                                     (parser-then (char %backslash) (parser-pure %backslash))
                                                     (parser-then (char %char-n) (parser-pure %newline-ex))
                                                     (parser-then (char %char-t) (parser-pure %tab-ex)))))
                               ;; Regular characters
                               (satisfy (lambda (c) (not (char=? c #\")))
                                        "string character")))))))
(doc sexp-string 'type '(Parser String))
(doc sexp-string 'description "Parse S-expression string with escapes")

(define sexp-boolean
  (sexp-lexeme
   (parser-or
    (parser-then (string-parser "#t") (parser-pure #t))
    (parser-then (string-parser "#f") (parser-pure #f)))))
(doc sexp-boolean 'type '(Parser Boolean))
(doc sexp-boolean 'description "Parse S-expression boolean")

(define sexp-value #f)
(doc sexp-value 'type '(Parser SExp))
(doc sexp-value 'description "Forward declaration for recursive S-expressions")

(define sexp-list
  (between (sexp-lexeme (char #\())
           (sexp-lexeme (char #\)))
           (many (make-parser (lambda (s) (run-parser sexp-value s))))))
(doc sexp-list 'type '(Parser (List SExp)))
(doc sexp-list 'description "Parse S-expression list")

;;; sexp-quote : (Parser SExp)
(define sexp-quote
  (parser-bind (sexp-lexeme (char #\'))
               (lambda (_)
                       (parser-bind (make-parser (lambda (s) (run-parser sexp-value s)))
                                    (lambda (val)
                                            (parser-pure (list 'quote val)))))))

;;; sexp-quasiquote : (Parser SExp)
(define sexp-quasiquote
  (parser-bind (sexp-lexeme (char #\`))
               (lambda (_)
                       (parser-bind (make-parser (lambda (s) (run-parser sexp-value s)))
                                    (lambda (val)
                                            (parser-pure (list 'quasiquote val)))))))

;;; sexp-unquote : (Parser SExp)
(define sexp-unquote
  (parser-bind (char #\,)
               (lambda (_)
                       (parser-or
                        ;; ,@expr (no whitespace allowed between , and @)
                        (parser-bind (char #\@)
                                     (lambda (_)
                                             (parser-bind (make-parser (lambda (s) (run-parser sexp-value s)))
                                                          (lambda (val)
                                                                  (parser-pure (list 'unquote-splicing val))))))
                        ;; ,expr (whitespace is allowed after ,)
                        (parser-bind (parser-then sexp-whitespace
                                                  (make-parser (lambda (s) (run-parser sexp-value s))))
                                     (lambda (val)
                                             (parser-pure (list 'unquote val))))))))

;;; Initialize sexp-value
(set! sexp-value
      (parser-then
       sexp-whitespace
       (choice (list sexp-boolean
                     sexp-string
                     sexp-quote
                     sexp-quasiquote
                     sexp-unquote
                     sexp-list
                     sexp-symbol))))

(define (parse-sexp input)
  (doc 'type '(-> String (Either Error SExp)))
  (doc 'description "Parse single S-expression")
  (parse-all sexp-value input))

(define (parse-sexps input)
  (doc 'type '(-> String (Either Error (List SExp))))
  (doc 'description "Parse multiple S-expressions")
  (parse-all (parser-then sexp-whitespace (many sexp-value)) input))

(doc 'section 'arithmetic-parser)

(define arith-number (lexeme decimal))
(doc arith-number 'type '(Parser Number))
(doc arith-number 'description "Parse arithmetic number")

(define arith-expr #f)
(doc arith-expr 'type '(Parser AST))
(doc arith-expr 'description "Forward declaration for expression")

(define arith-term #f)
(doc arith-term 'type '(Parser AST))
(doc arith-term 'description "Forward declaration for term")

(define arith-factor #f)
(doc arith-factor 'type '(Parser AST))
(doc arith-factor 'description "Forward declaration for factor")

(define arith-atom
  (parser-or
   (parens (make-parser (lambda (s) (run-parser arith-expr s))))
   arith-number))
(doc arith-atom 'type '(Parser AST))
(doc arith-atom 'description "Number or parenthesized expression")

;;; arith-unary : (Parser AST)
;;; Unary minus.
(define arith-unary
  (parser-or
   (parser-bind (symbol "-")
                (lambda (_)
                        (parser-bind (make-parser (lambda (s) (run-parser arith-unary s)))
                                     (lambda (val)
                                             (parser-pure (list '- val))))))
   arith-atom))

;;; chain-left1 : (Parser α) × (Parser (α × α → α)) → (Parser α)
;;; Parse left-associative binary operations.
(define (chain-left1 p op)
  (define (rest-parser acc)
    (parser-or
     (parser-bind op
                  (lambda (f)
                          (parser-bind p
                                       (lambda (y)
                                               (rest-parser (f acc y))))))
     (parser-pure acc)))
  (parser-bind p rest-parser))

;;; Initialize arith-factor (handles * /)
(set! arith-factor
      (let ([mul-op (parser-or
                     (parser-then (symbol "*") (parser-pure (lambda (a b) (list '* a b))))
                     (parser-then (symbol "/") (parser-pure (lambda (a b) (list '/ a b)))))])
           (chain-left1 arith-unary mul-op)))

;;; Initialize arith-term (handles + -)
(set! arith-term
      (let ([add-op (parser-or
                     (parser-then (symbol "+") (parser-pure (lambda (a b) (list '+ a b))))
                     (parser-then (symbol "-") (parser-pure (lambda (a b) (list '- a b)))))])
           (chain-left1 arith-factor add-op)))

;;; Initialize arith-expr
(set! arith-expr arith-term)

(define (parse-arith input)
  (doc 'type '(-> String (Either Error AST)))
  (doc 'description "Parse arithmetic expression")
  (parse-all arith-expr input))

(define (eval-arith ast)
  (doc 'type '(-> AST Number))
  (doc 'description "Evaluate an arithmetic AST")
  (cond
   [(number? ast) ast]
   [(and (pair? ast) (eq? (car ast) '+))
    (+ (eval-arith (cadr ast)) (eval-arith (caddr ast)))]
   [(and (pair? ast) (eq? (car ast) '-))
    (if (null? (cddr ast))
        (- (eval-arith (cadr ast)))
        (- (eval-arith (cadr ast)) (eval-arith (caddr ast))))]
   [(and (pair? ast) (eq? (car ast) '*))
    (* (eval-arith (cadr ast)) (eval-arith (caddr ast)))]
   [(and (pair? ast) (eq? (car ast) '/))
    (/ (eval-arith (cadr ast)) (eval-arith (caddr ast)))]
   [else (error 'eval-arith "Unknown AST node" ast)]))

(doc 'section 'ini-parser)

(define ini-comment
  (parser-then
   (one-of ";#")
   (parser-then
    (many (satisfy (lambda (c) (not (char=? c %newline-ex))) "non-newline"))
    (parser-pure '()))))
(doc ini-comment 'type '(Parser Unit))
(doc ini-comment 'description "Parse INI comment")

;;; ini-hspace : (Parser (List Char))
;;; Spaces and tabs (not newlines).
(define ini-hspace
  (many (one-of " 	")))

;;; ini-line-end : (Parser Unit)
;;; End of line: newline or eof (consumes newline if present).
(define ini-line-end
  (parser-or newline-char eof))

;;; ini-skip-line : (Parser Unit)
;;; Skip to end of line including newline.
(define ini-skip-line
  (parser-then
   (many (satisfy (lambda (c) (not (char=? c %newline-ex))) "non-newline"))
   ini-line-end))

;;; ini-blank-or-comment : (Parser Char)
;;; A blank line (just whitespace) or a comment line.
(define ini-blank-or-comment
  (parser-then
   ini-hspace
   (parser-or
    ;; Comment line
    (parser-then ini-comment ini-line-end)
    ;; Just end of line (must consume newline)
    newline-char)))

;;; ini-skip-blanks : (Parser (List α))
(define ini-skip-blanks
  (many ini-blank-or-comment))

;;; ini-section-name : (Parser String)
(define ini-section-name
  (parser-map list->string
              (some (satisfy (lambda (c)
                                     (and (not (char=? c #\]))
                                          (not (char=? c %newline-ex))))
                             "section name character"))))

;;; ini-section-header : (Parser String)
(define ini-section-header
  (between (char #\[) (char #\]) ini-section-name))

;;; ini-key : (Parser String)
(define ini-key
  (parser-map list->string
              (some (satisfy (lambda (c)
                                     (or (char-alphabetic? c)
                                         (char-numeric? c)
                                         (char=? c #\_)
                                         (char=? c #\-)))
                             "key character"))))

;;; ini-value : (Parser String)
(define ini-value
  (parser-map
   (lambda (chars)
           ;; Trim trailing whitespace - O(N) single-pass approach
           ;; Find last non-whitespace index, then take only up to that point
           (let* ([len (length chars)]
                  [last-non-ws
                   (let loop ([i (- len 1)] [lst (reverse chars)])
                        (cond
                         [(< i 0) -1]
                         [(not (char-whitespace? (car lst))) i]
                         [else (loop (- i 1) (cdr lst))]))])
                 (if (< last-non-ws 0)
                     ""
                     (list->string (take (+ last-non-ws 1) chars)))))
   (many (satisfy (lambda (c) (not (char=? c %newline-ex))) "value character"))))

;;; ini-pair : (Parser (String × String))
(define ini-pair
  (parser-bind ini-key
               (lambda (key)
                       (parser-then
                        ini-hspace
                        (parser-then
                         (char #\=)
                         (parser-then
                          ini-hspace
                          (parser-bind ini-value
                                       (lambda (val)
                                               (parser-then
                                                (parser-or newline-char eof)
                                                (parser-pure (cons key val)))))))))))

;;; ini-section : (Parser (String × (Alist String String)))
(define ini-section
  (parser-bind (try (parser-then ini-skip-blanks ini-section-header))
               (lambda (name)
                       (parser-then
                        (parser-or newline-char eof)
                        (parser-bind (many (try (parser-then ini-skip-blanks ini-pair)))
                                     (lambda (pairs)
                                             (parser-pure (cons name pairs))))))))

;;; ini-file : (Parser (List (String × (Alist String String))))
(define ini-file
  (parser-then ini-skip-blanks
               (parser-left (many ini-section) (parser-then ini-skip-blanks eof))))

(define (parse-ini input)
  (doc 'type '(-> String (Either Error INI)))
  (doc 'description "Parse INI configuration file")
  (parse ini-file input))

(define (ini-get ini section key)
  (doc 'type '(-> INI String String (Option String)))
  (doc 'description "Get a value from parsed INI file")
  (let ([sec (assoc section ini)])
       (if sec
           (let ([pair (assoc key (cdr sec))])
                (if pair (just (cdr pair)) nothing))
           nothing)))

(doc 'section 'demonstration)

(define (demo)
  (doc 'type '(-> Unit Unit))
  (doc 'description "Demonstrate all example parsers")
  (display "
=== Parser Combinator Examples ===

")
  
  ;; JSON
  (display "--- JSON Parsing ---
")
  (let ([json-input "{\"name\": \"Alice\", \"age\": 30, \"active\": true}"])
       (display "Input: ")
       (display json-input)
       (newline)
       (let ([result (parse-json json-input)])
            (display "Result: ")
            (if (right? result)
                (write (from-right result))
                (display (format-error (from-left result))))
            (newline)))
  (newline)
  
  ;; S-expression
  (display "--- S-Expression Parsing ---
")
  (let ([sexp-input "(define (square x) (* x x))"])
       (display "Input: ")
       (display sexp-input)
       (newline)
       (let ([result (parse-sexp sexp-input)])
            (display "Result: ")
            (if (right? result)
                (write (from-right result))
                (display (format-error (from-left result))))
            (newline)))
  (newline)
  
  ;; Arithmetic
  (display "--- Arithmetic Expression Parsing ---
")
  (let ([arith-input "1 + 2 * 3 - 4 / 2"])
       (display "Input: ")
       (display arith-input)
       (newline)
       (let ([result (parse-arith arith-input)])
            (display "AST:    ")
            (if (right? result)
                (begin
                 (write (from-right result))
                 (newline)
                 (display "Eval:   ")
                 (display (eval-arith (from-right result))))
                (display (format-error (from-left result))))
            (newline)))
  (newline)
  
  ;; INI file
  (display "--- INI File Parsing ---
")
  (let ([ini-input "[database]
host = localhost
port = 5432

[server]
debug = true
name = my-server
"])
       (display "Input:
")
       (display ini-input)
       (let ([result (parse-ini ini-input)])
            (display "Result: ")
            (if (right? result)
                (begin
                 (write (from-right result))
                 (newline)
                 (display "Get database.host: ")
                 (let ([val (ini-get (from-right result) "database" "host")])
                      (if (just? val)
                          (display (from-just val))
                          (display "not found"))))
                (display (format-error (from-left result))))
            (newline)))
  
  (display "
=== Done ===
"))
