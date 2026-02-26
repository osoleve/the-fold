(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module sql-lexer
;;; @requires prelude parser sql-types
(require 'prelude)
(require 'parser)
(require 'sql-types)

(define *sql-lexer-loaded* #t)

(doc 'module 'sql-lexer)
(doc 'description "SQL Lexer (Token Parsers)")
(doc 'note "Token parsers for SQL keywords, operators, and literals. Uses the parser combinator library for case-insensitive keyword matching.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Character Constants
;;; ====

(define %underscore (integer->char 95))    ; _
(define %single-quote (integer->char 39))  ; '
(define %double-quote (integer->char 34))  ; "

;;; ====
;;; SQL Language Definition
;;; ====

;;; Reserved keywords (case-insensitive in SQL)
(define sql-reserved-words
  '("SELECT" "FROM" "WHERE" "AND" "OR" "NOT" "AS" "ON"
    "INSERT" "INTO" "VALUES" "UPDATE" "SET" "DELETE"
    "JOIN" "LEFT" "RIGHT" "INNER" "OUTER" "CROSS" "FULL" "NATURAL"
    "ORDER" "BY" "ASC" "DESC" "NULLS" "FIRST" "LAST"
    "GROUP" "HAVING" "DISTINCT" "ALL"
    "LIMIT" "OFFSET" "FETCH" "NEXT" "ROWS" "ONLY"
    "CASE" "WHEN" "THEN" "ELSE" "END"
    "IN" "BETWEEN" "LIKE" "ESCAPE" "IS" "NULL" "EXISTS"
    "TRUE" "FALSE" "UNKNOWN" "DEFAULT"
    "UNION" "INTERSECT" "EXCEPT"
    "COUNT" "SUM" "AVG" "MIN" "MAX"))

;;; Reserved operators
(define sql-reserved-ops
  '("=" "<>" "!=" "<" ">" "<=" ">=" "+" "-" "*" "/" "%" "||"))

;;; ====
;;; Whitespace and Comments
;;; ====

;;; sql-line-comment : Parser ()
;;; SQL line comments start with --
(define sql-line-comment
  (parser-then
   (parser-string "--")
   (parser-then
    (parser-many (parser-satisfy (lambda (c) (not (char=? c (integer->char 10))))
                   "non-newline"))
    (parser-pure '()))))

;;; sql-block-comment : Parser ()
;;; SQL block comments /* ... */
(define sql-block-comment
  (parser-then
   (parser-string "/*")
   (parser-many-till parser-any-char (parser-string "*/"))))

;;; sql-whitespace : Parser ()
;;; Skip whitespace and comments
(define sql-whitespace
  (skip-many (parser-choice (list parser-space sql-line-comment sql-block-comment))))

;;; sql-lexeme : Parser a → Parser a
;;; Parse and consume trailing whitespace
(define (sql-lexeme p)
  (parser-left p sql-whitespace))

;;; ====
;;; Case-Insensitive String Matching
;;; ====

;;; char-ci-equal : Char → Char → Boolean
(define (char-ci-equal c1 c2)
  (char-ci=? c1 c2))

;;; string-ci-parser : String → Parser String
;;; Case-insensitive string match, returns original casing from input
(define (string-ci-parser str)
  (if (string=? str "")
      (parser-pure "")
      (make-parser
       (lambda (state)
               (let* ([full-input (parser-state-input state)]
                      [idx (parser-state-index state)]
                      [len (string-length str)]
                      [remaining (- (string-length full-input) idx)])
                     (if (< remaining len)
                         (left (make-parse-error
                                (parser-state-pos state)
                                "unexpected end of input"
                                (list str)))
                         (let ([prefix (substring full-input idx (+ idx len))])
                              (if (string-ci-equal? prefix str)
                                  (let* ([new-idx (+ idx len)]
                                         [new-pos (fold-left
                                                   (lambda (p i)
                                                           (advance-pos p (string-ref prefix i)))
                                                   (parser-state-pos state)
                                                   (iota len))]
                                         [new-state (parser-make-state full-input new-idx new-pos)])
                                        (right (cons prefix new-state)))
                                  (left (make-parse-error
                                         (parser-state-pos state)
                                         (string-append "expected \"" str "\"")
                                         (list str)))))))))))

;;; string-ci-equal? : String × String → Boolean
(define (string-ci-equal? s1 s2)
  (and (= (string-length s1) (string-length s2))
       (let loop ([i 0])
            (or (>= i (string-length s1))
                (and (char-ci=? (string-ref s1 i) (string-ref s2 i))
                     (loop (+ i 1)))))))

;;; ====
;;; SQL Keywords
;;; ====

;;; sql-keyword : String → Parser String
;;; Parse SQL keyword (case-insensitive) with word boundary
(define (sql-keyword kw)
  (sql-lexeme
   (parser-try
    (parser-bind (string-ci-parser kw)
                 (lambda (matched)
                         (parser-bind (parser-not-followed-by (parser-or parser-alpha-num (parser-char %underscore)))
                                      (lambda (_)
                                              (parser-pure matched))))))))

;;; sql-reserved : String → Parser String
;;; Alias for sql-keyword
(define sql-reserved sql-keyword)

;;; ====
;;; SQL Identifiers
;;; ====

;;; sql-identifier-start : Parser Char
(define sql-identifier-start
  (parser-or parser-letter (parser-char %underscore)))

;;; sql-identifier-rest : Parser Char
(define sql-identifier-rest
  (parser-or parser-alpha-num (parser-char %underscore)))

;;; sql-bare-identifier : Parser String
;;; Unquoted identifier (not a reserved word)
(define sql-bare-identifier
  (parser-try
   (parser-bind sql-identifier-start
                (lambda (first)
                        (parser-bind (parser-many sql-identifier-rest)
                                     (lambda (rest)
                                             (let ([name (list->string (cons first rest))])
                                                  ;; Check if it's a reserved word
                                                  (if (any? (lambda (kw) (string-ci-equal? name kw))
                                                            sql-reserved-words)
                                                      (parser-fail (string-append "reserved word: " name))
                                                      (parser-pure name)))))))))

;;; any? : (a → Boolean) × (List a) → Boolean
(define (any? pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any? pred (cdr lst))]))

;;; sql-quoted-identifier : Parser String
;;; Double-quoted identifier (can contain reserved words, spaces)
(define sql-quoted-identifier
  (parser-between (parser-char %double-quote)
           (parser-char %double-quote)
           (parser-map list->string
                       (parser-many (parser-or
                              ;; Escaped quote: "" (use try to backtrack if only one quote)
                              (parser-try (parser-then (parser-char %double-quote)
                                                (parser-then (parser-char %double-quote)
                                                             (parser-pure %double-quote))))
                              ;; Regular character
                              (parser-satisfy (lambda (c) (not (char=? c %double-quote)))
                                       "identifier character"))))))

;;; sql-identifier : Parser String
;;; Parse any SQL identifier (bare or quoted)
(define sql-identifier
  (sql-lexeme
   (parser-or sql-quoted-identifier sql-bare-identifier)))

;;; ====
;;; SQL Literals
;;; ====

;;; sql-string-literal : Parser String
;;; Single-quoted string with '' for escaped quotes
(define sql-string-literal
  (sql-lexeme
   (parser-between (parser-char %single-quote)
            (parser-char %single-quote)
            (parser-map list->string
                        (parser-many (parser-or
                               ;; Escaped quote: '' (use try to backtrack if only one quote)
                               (parser-try (parser-then (parser-char %single-quote)
                                                 (parser-then (parser-char %single-quote)
                                                              (parser-pure %single-quote))))
                               ;; Regular character
                               (parser-satisfy (lambda (c) (not (char=? c %single-quote)))
                                        "string character")))))))

;;; sql-integer : Parser Integer
(define sql-integer
  (sql-lexeme
   (parser-bind (parser-optional (parser-one-of "+-") #\+)
                (lambda (sign)
                        (parser-bind (parser-some parser-digit)
                                     (lambda (digits)
                                             (let ([n (string->number (list->string digits))])
                                                  (parser-pure (if (char=? sign #\-)
                                                                   (- n)
                                                                   n)))))))))

;;; sql-decimal : Parser Number
(define sql-decimal
  (sql-lexeme
   (parser-bind (parser-optional (parser-one-of "+-") #\+)
                (lambda (sign)
                        (parser-bind (parser-some parser-digit)
                                     (lambda (int-part)
                                             (parser-bind (parser-optional (parser-then (parser-char #\.)
                                                                                 (parser-some parser-digit))
                                                                    '())
                                                          (lambda (frac-part)
                                                                  (let* ([int-str (list->string int-part)]
                                                                         [frac-str (if (null? frac-part)
                                                                                       ""
                                                                                       (string-append "." (list->string frac-part)))]
                                                                         [num (string->number (string-append int-str frac-str))])
                                                                        (parser-pure (if (char=? sign #\-)
                                                                                         (- num)
                                                                                         num)))))))))))

;;; sql-number : Parser Number
;;; Parse integer or decimal
(define sql-number
  (parser-try sql-decimal))

;;; sql-boolean : Parser Boolean
(define sql-boolean
  (parser-or (parser-then (sql-keyword "TRUE") (parser-pure #t))
             (parser-then (sql-keyword "FALSE") (parser-pure #f))))

;;; sql-null : Parser Symbol
(define sql-null
  (parser-then (sql-keyword "NULL") (parser-pure 'null)))

;;; ====
;;; SQL Operators
;;; ====

;;; sql-operator : String → Parser String
;;; Parse specific operator
(define (sql-operator op)
  (sql-lexeme (parser-string op)))

;;; Comparison operators
(define sql-eq (sql-operator "="))
(define sql-neq (parser-or (sql-operator "<>") (sql-operator "!=")))
(define sql-lt (sql-operator "<"))
(define sql-gt (sql-operator ">"))
(define sql-lte (sql-operator "<="))
(define sql-gte (sql-operator ">="))

;;; Arithmetic operators
(define sql-plus (sql-operator "+"))
(define sql-minus (sql-operator "-"))
(define sql-star (sql-operator "*"))
(define sql-slash (sql-operator "/"))
(define sql-percent (sql-operator "%"))

;;; String concatenation
(define sql-concat (sql-operator "||"))

;;; ====
;;; SQL Punctuation
;;; ====

(define sql-lparen (sql-lexeme (parser-char #\()))
(define sql-rparen (sql-lexeme (parser-char #\))))
(define sql-comma (sql-lexeme (parser-char #\,)))
(define sql-semicolon (sql-lexeme (parser-char #\;)))
(define sql-dot (sql-lexeme (parser-char #\.)))

;;; ====
;;; Utility Parsers
;;; ====

;;; sql-parens : Parser a → Parser a
(define (sql-parens p)
  (parser-between sql-lparen sql-rparen p))

;;; sql-comma-sep : Parser a → Parser (List a)
(define (sql-comma-sep p)
  (parser-sep-by p sql-comma))

;;; sql-comma-sep1 : Parser a → Parser (List a)
(define (sql-comma-sep1 p)
  (parser-sep-by1 p sql-comma))

;;; ====
;;; Token Record (for parser-dsl compatibility)
;;; ====

;;; sql-token-parser : TokenParser
;;; Token parser record compatible with parser-dsl.ss
(define sql-token-parser
  (list 'token-parser
        (cons 'identifier sql-identifier)
        (cons 'reserved sql-keyword)
        (cons 'number sql-number)
        (cons 'string-lit sql-string-literal)
        (cons 'lparen sql-lparen)
        (cons 'rparen sql-rparen)
        (cons 'comma sql-comma)
        (cons 'semi sql-semicolon)
        (cons 'dot sql-dot)
        (cons 'parens sql-parens)
        (cons 'comma-sep sql-comma-sep)
        (cons 'skip-ws sql-whitespace)))
