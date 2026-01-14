;;; fabric/stitches/fp/parser-examples.ss — Example Parsers Built with Combinators
;;;
;;; Demonstrates building practical parsers using the parser combinator DSL.
;;; Shows common patterns and idioms.
;;;
;;; Examples:
;;;   1. JSON parser (subset)
;;;   2. S-expression parser
;;;   3. Simple arithmetic expression parser with precedence
;;;   4. INI file parser
;;;
;;; Dependencies:
;;;   - fp/parser.ss

(load "core/base/prelude.ss")
(load "lattice/fp/parsing/parser.ss")

;;; ====
;;; Character Constants (to avoid formatter issues)
;;; ====

;;; %newline-ex : Char
(define %newline-ex (integer->char 10))

;;; %tab-ex : Char
(define %tab-ex (integer->char 9))

;;; %return-ex : Char
(define %return-ex (integer->char 13))

;;; %backspace-ex : Char
(define %backspace-ex (integer->char 8))

;;; %page-ex : Char
(define %page-ex (integer->char 12))

;;; %backslash : Char
(define %backslash (integer->char 92))

;;; %char-b : Char
(define %char-b (integer->char 98))

;;; %char-f : Char
(define %char-f (integer->char 102))

;;; %char-n : Char
(define %char-n (integer->char 110))

;;; %char-r : Char
(define %char-r (integer->char 114))

;;; %char-t : Char
(define %char-t (integer->char 116))

;;; ====
;;; JSON Parser (Subset)
;;; ====
;;;
;;; Parses JSON values: null, booleans, numbers, strings, arrays, objects.
;;; Represents:
;;;   - null as 'json-null
;;;   - true/false as #t/#f
;;;   - numbers as Scheme numbers
;;;   - strings as Scheme strings
;;;   - arrays as Scheme lists
;;;   - objects as alists

;;; json-null : (Parser Symbol)
(define json-null
  (parser-then (symbol "null") (parser-pure 'json-null)))

;;; json-bool : (Parser Boolean)
(define json-bool
  (parser-or
   (parser-then (symbol "true") (parser-pure #t))
   (parser-then (symbol "false") (parser-pure #f))))

;;; json-number : (Parser Number)
(define json-number
  (lexeme decimal))

;;; json-string-char : (Parser Char)
;;; Parse a single character inside a string (handling escapes).
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

;;; json-string : (Parser String)
(define json-string
  (lexeme
   (between (char #\")
            (char #\")
            (parser-map list->string (many json-string-char)))))

;;; Forward declaration for recursive parsers
;;; json-value : (Parser JsonValue)
(define json-value #f)

;;; json-array : (Parser (List JsonValue))
(define json-array
  (brackets (comma-sep (make-parser (lambda (s) (parser-run json-value s))))))

;;; json-pair : (Parser (String × JsonValue))
(define json-pair
  (parser-bind json-string
               (lambda (key)
                       (parser-then
                        (symbol ":")
                        (parser-bind (make-parser (lambda (s) (parser-run json-value s)))
                                     (lambda (val)
                                             (parser-pure (cons key val))))))))

;;; json-object : (Parser (Alist String JsonValue))
(define json-object
  (braces (comma-sep json-pair)))

;;; Initialize json-value
(set! json-value
      (choice (list json-null
                    json-bool
                    json-number
                    json-string
                    json-array
                    json-object)))

;;; parse-json : String → (Either Error JsonValue)
(define (parse-json input)
  (parse-all json-value input))

;;; ====
;;; S-Expression Parser
;;; ====
;;;
;;; Parses Scheme-style S-expressions:
;;;   - Atoms: symbols, numbers, strings, booleans
;;;   - Lists: (a b c)
;;;   - Dotted pairs: (a . b)
;;;   - Quotes: 'x, `x, ,x, ,@x

;;; sexp-comment : (Parser Unit)
;;; Skip line comment.
(define sexp-comment
  (parser-then (char #\;)
               (parser-then (many (satisfy (lambda (c) (not (char=? c %newline-ex)))
                                           "non-newline"))
                            (parser-pure '()))))

;;; sexp-whitespace : (Parser (List α))
;;; Skip whitespace and comments.
(define sexp-whitespace
  (many (parser-or space sexp-comment)))

;;; sexp-lexeme : (Parser α) → (Parser α)
(define (sexp-lexeme p)
  (parser-left p sexp-whitespace))

;;; sexp-symbol-char : (Parser Char)
(define sexp-symbol-char
  (satisfy (lambda (c)
                   (or (char-alphabetic? c)
                       (char-numeric? c)
                       (memv c '(#\+ #\- #\* #\/ #\< #\> #\= #\? #\! #\_ #\: #\. #\@))))
           "symbol character"))

;;; sexp-symbol : (Parser (Union Symbol Number))
(define sexp-symbol
  (sexp-lexeme
   (parser-bind (some sexp-symbol-char)
                (lambda (chars)
                        (let ([str (list->string chars)])
                             ;; Try to parse as number first
                             (let ([num (string->number str)])
                                  (parser-pure (if num num (string->symbol str)))))))))

;;; sexp-string : (Parser String)
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

;;; sexp-boolean : (Parser Boolean)
(define sexp-boolean
  (sexp-lexeme
   (parser-or
    (parser-then (string-parser "#t") (parser-pure #t))
    (parser-then (string-parser "#f") (parser-pure #f)))))

;;; Forward declaration
;;; sexp-value : (Parser SExp)
(define sexp-value #f)

;;; sexp-list : (Parser (List SExp))
(define sexp-list
  (between (sexp-lexeme (char #\())
           (sexp-lexeme (char #\)))
           (many (make-parser (lambda (s) (parser-run sexp-value s))))))

;;; sexp-quote : (Parser SExp)
(define sexp-quote
  (parser-bind (sexp-lexeme (char #\'))
               (lambda (_)
                       (parser-bind (make-parser (lambda (s) (parser-run sexp-value s)))
                                    (lambda (val)
                                            (parser-pure (list 'quote val)))))))

;;; sexp-quasiquote : (Parser SExp)
(define sexp-quasiquote
  (parser-bind (sexp-lexeme (char #\`))
               (lambda (_)
                       (parser-bind (make-parser (lambda (s) (parser-run sexp-value s)))
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
                                             (parser-bind (make-parser (lambda (s) (parser-run sexp-value s)))
                                                          (lambda (val)
                                                                  (parser-pure (list 'unquote-splicing val))))))
                        ;; ,expr (whitespace is allowed after ,)
                        (parser-bind (parser-then sexp-whitespace
                                                  (make-parser (lambda (s) (parser-run sexp-value s))))
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

;;; parse-sexp : String → (Either Error SExp)
(define (parse-sexp input)
  (parse-all sexp-value input))

;;; parse-sexps : String → (Either Error (List SExp))
;;; Parse multiple S-expressions.
(define (parse-sexps input)
  (parse-all (parser-then sexp-whitespace (many sexp-value)) input))

;;; ====
;;; Arithmetic Expression Parser (with Precedence)
;;; ====
;;;
;;; Parses arithmetic expressions with proper precedence:
;;;   - Addition/subtraction (lowest)
;;;   - Multiplication/division
;;;   - Unary minus
;;;   - Parentheses (highest)
;;;
;;; Returns an AST as nested lists:
;;;   (+ 1 (* 2 3)) for "1 + 2 * 3"

;;; arith-number : (Parser Number)
(define arith-number
  (lexeme decimal))

;;; Forward declarations
;;; arith-expr : (Parser AST)
(define arith-expr #f)

;;; arith-term : (Parser AST)
(define arith-term #f)

;;; arith-factor : (Parser AST)
(define arith-factor #f)

;;; arith-atom : (Parser AST)
;;; Number or parenthesized expression.
(define arith-atom
  (parser-or
   (parens (make-parser (lambda (s) (parser-run arith-expr s))))
   arith-number))

;;; arith-unary : (Parser AST)
;;; Unary minus.
(define arith-unary
  (parser-or
   (parser-bind (symbol "-")
                (lambda (_)
                        (parser-bind (make-parser (lambda (s) (parser-run arith-unary s)))
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

;;; parse-arith : String → (Either Error AST)
(define (parse-arith input)
  (parse-all arith-expr input))

;;; eval-arith : AST → Number
;;; Evaluate an arithmetic AST.
(define (eval-arith ast)
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

;;; ====
;;; INI File Parser
;;; ====
;;;
;;; Parses INI-style configuration files:
;;;   [section]
;;;   key = value
;;;   ; comment
;;;
;;; Returns: ((section . ((key . value) ...)) ...)

;;; ini-comment : (Parser Unit)
(define ini-comment
  (parser-then
   (one-of ";#")
   (parser-then
    (many (satisfy (lambda (c) (not (char=? c %newline-ex))) "non-newline"))
    (parser-pure '()))))

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

;;; parse-ini : String → (Either Error INI)
(define (parse-ini input)
  (parse ini-file input))

;;; ini-get : INI × String × String → (Option String)
;;; Get a value from parsed INI file.
(define (ini-get ini section key)
  (let ([sec (assoc section ini)])
       (if sec
           (let ([pair (assoc key (cdr sec))])
                (if pair (just (cdr pair)) nothing))
           nothing)))

;;; ====
;;; Demonstration
;;; ====

;;; demo : Unit → Unit
(define (demo)
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
