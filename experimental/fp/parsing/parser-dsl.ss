;;; fabric/stitches/fp/parser-dsl.ss — DSL Construction Toolkit
;;;
;;; Tools for building domain-specific languages with parser combinators.
;;; Provides expression parsers, token utilities, and AST helpers.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Expression parser builder with operator precedence
;;;   - Token utilities (reserved words, operators)
;;;   - AST node construction with source spans
;;;   - Language definition structure
;;;   - Common DSL patterns
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/parser.ss

(load "core/prelude.ss")
(load "core/fp/parsing/parser.ss")

;;; ============================================================
;;; AST Nodes with Source Spans
;;; ============================================================
;;;
;;; AST nodes carry source location for error reporting.
;;; Format: (ast tag span . data)

;;; make-ast : Symbol × Span × Any... → AST
(define (make-ast tag span . data)
  (list 'ast tag span data))

;;; ast? : Any → Boolean
(define (ast? x)
  (and (pair? x) (eq? (car x) 'ast)))

;;; ast-tag : AST → Symbol
(define (ast-tag node) (list-ref node 1))

;;; ast-span : AST → Span
(define (ast-span node) (list-ref node 2))

;;; ast-data : AST → List
(define (ast-data node) (list-ref node 3))

;;; ast-ref : AST × Nat → Any
;;; Get nth data element from AST node.
(define (ast-ref node n)
  (list-ref (ast-data node) n))

;;; make-span : Pos × Pos → Span
(define (make-source-span start end)
  (list 'span start end))

;;; span? : Any → Boolean
(define (source-span? x)
  (and (pair? x) (eq? (car x) 'span)))

;;; span-start : Span → Pos
(define (span-start s) (list-ref s 1))

;;; span-end : Span → Pos
(define (span-end s) (list-ref s 2))

;;; merge-spans : Span × Span → Span
;;; Create span covering both inputs.
(define (merge-spans s1 s2)
  (make-source-span (span-start s1) (span-end s2)))

;;; no-span : Span
;;; Placeholder for nodes without source info.
(define no-span (make-source-span initial-pos initial-pos))

;;; ============================================================
;;; AST-Preserving Parser Combinators
;;; ============================================================

;;; parse-ast : Symbol × Parser a → Parser AST
;;; Wrap parser result in AST node with source span.
(define (parse-ast tag p)
  (parser-bind (with-span p)
               (lambda (result)
                       (let ([val (car result)]
                             [start (cadr result)]
                             [end (caddr result)])
                            (parser-pure (make-ast tag (make-source-span start end) val))))))

;;; parse-ast* : Symbol × Parser (List Any) → Parser AST
;;; Like parse-ast but spreads list result into data.
(define (parse-ast* tag p)
  (parser-bind (with-span p)
               (lambda (result)
                       (let ([vals (car result)]
                             [start (cadr result)]
                             [end (caddr result)])
                            (parser-pure (apply make-ast tag (make-source-span start end) vals))))))

;;; ============================================================
;;; Operator Precedence Expression Parser
;;; ============================================================
;;;
;;; Build expression parsers from precedence tables.
;;;
;;; Operator types:
;;;   - (infix-l assoc parser builder) - left-associative binary
;;;   - (infix-r assoc parser builder) - right-associative binary
;;;   - (prefix parser builder)        - prefix unary
;;;   - (postfix parser builder)       - postfix unary
;;;
;;; Precedence table: list of lists, highest precedence first.

;;; Operator constructors

;;; infix-l : Parser op × (a × op × a → a) → Operator
(define (infix-l op-parser builder)
  (list 'infix-l op-parser builder))

;;; infix-r : Parser op × (a × op × a → a) → Operator
(define (infix-r op-parser builder)
  (list 'infix-r op-parser builder))

;;; prefix-op : Parser op × (op × a → a) → Operator
(define (prefix-op op-parser builder)
  (list 'prefix op-parser builder))

;;; postfix-op : Parser op × (a × op → a) → Operator
(define (postfix-op op-parser builder)
  (list 'postfix op-parser builder))

;;; Operator accessors
(define (op-type op) (car op))
(define (op-parser op) (cadr op))
(define (op-builder op) (caddr op))

;;; build-expr-parser : (List (List Operator)) × Parser a → Parser a
;;; Build expression parser from precedence table.
;;; Each list in the table is a precedence level (highest precedence first).
;;; Higher precedence operators bind tighter (are parsed as operands of lower).
(define (build-expr-parser table atom-parser)
  ;; fold-left: first element becomes innermost (highest precedence)
  ;; So mul-level uses atom, then add-level uses mul-level as operand
  (fold-left (lambda (acc ops) (build-level ops acc)) atom-parser table))

;;; build-level : (List Operator) × Parser a → Parser a
;;; Build parser for one precedence level.
(define (build-level ops next-level)
  (let ([prefix-ops (filter (lambda (op) (eq? (op-type op) 'prefix)) ops)]
        [postfix-ops (filter (lambda (op) (eq? (op-type op) 'postfix)) ops)]
        [infix-l-ops (filter (lambda (op) (eq? (op-type op) 'infix-l)) ops)]
        [infix-r-ops (filter (lambda (op) (eq? (op-type op) 'infix-r)) ops)])
       ;; Build term parser with prefix/postfix
       (let* ([with-prefix (if (null? prefix-ops)
                               next-level
                               (build-prefix prefix-ops next-level))]
              [with-postfix (if (null? postfix-ops)
                                with-prefix
                                (build-postfix postfix-ops with-prefix))]
              ;; Add infix operators
              [with-infix-l (if (null? infix-l-ops)
                                with-postfix
                                (build-infix-l infix-l-ops with-postfix))]
              [with-infix-r (if (null? infix-r-ops)
                                with-infix-l
                                (build-infix-r infix-r-ops with-infix-l))])
             with-infix-r)))

;;; build-prefix : (List Operator) × Parser a → Parser a
(define (build-prefix ops p)
  (let ([op-choice (choice (map op-parser ops))])
       (parser-or
        (parser-bind op-choice
                     (lambda (op-val)
                             (parser-bind (build-prefix ops p)
                                          (lambda (arg)
                                                  ;; Find matching builder
                                                  (parser-pure (apply-prefix-builder ops op-val arg))))))
        p)))

(define (apply-prefix-builder ops op-val arg)
  ;; For simplicity, use first builder (assumes single prefix type per level)
  ((op-builder (car ops)) op-val arg))

;;; build-postfix : (List Operator) × Parser a → Parser a
(define (build-postfix ops p)
  (parser-bind p
               (lambda (x)
                       (build-postfix-rest ops x))))

(define (build-postfix-rest ops acc)
  (let ([op-choice (choice (map op-parser ops))])
       (parser-or
        (parser-bind op-choice
                     (lambda (op-val)
                             (build-postfix-rest ops ((op-builder (car ops)) acc op-val))))
        (parser-pure acc))))

;;; build-infix-l : (List Operator) × Parser a → Parser a
(define (build-infix-l ops p)
  (let ([op-choice (choice (map (lambda (op)
                                        (parser-map (lambda (v) (cons v (op-builder op)))
                                                    (op-parser op)))
                                ops))])
       (chainl1 p (parser-map cdr op-choice))))

;;; build-infix-r : (List Operator) × Parser a → Parser a
(define (build-infix-r ops p)
  (let ([op-choice (choice (map (lambda (op)
                                        (parser-map (lambda (v) (cons v (op-builder op)))
                                                    (op-parser op)))
                                ops))])
       (chainr1 p (parser-map cdr op-choice))))

;;; ============================================================
;;; Token Utilities
;;; ============================================================

;;; make-token-parser : LanguageDef → TokenParser
;;; Create token parsers from language definition.
;;; LanguageDef is an alist with:
;;;   - reserved-names: list of reserved words
;;;   - reserved-ops: list of reserved operators
;;;   - comment-start: string starting block comment
;;;   - comment-end: string ending block comment
;;;   - comment-line: string starting line comment
;;;   - ident-start: parser for first char of identifier
;;;   - ident-letter: parser for subsequent chars
;;;   - op-start: parser for first char of operator
;;;   - op-letter: parser for subsequent chars

(define (make-token-parser lang-def)
  (let ([reserved-names (lang-get lang-def 'reserved-names '())]
        [reserved-ops (lang-get lang-def 'reserved-ops '())]
        [comment-line (lang-get lang-def 'comment-line ";")]
        [ident-start (lang-get lang-def 'ident-start letter)]
        [ident-letter (lang-get lang-def 'ident-letter alpha-num)]
        [op-start (lang-get lang-def 'op-start (one-of "+-*/<>=!&|^~"))]
        [op-letter (lang-get lang-def 'op-letter (one-of "+-*/<>=!&|^~"))])
       
       ;; Build whitespace/comment skipper
       (let* ([line-comment (if (string=? comment-line "")
                                (parser-fail "no line comments")
                                (parser-then
                                 (string-parser comment-line)
                                 (parser-then
                                  (many (satisfy (lambda (c) (not (char=? c (integer->char 10))))
                                                 "non-newline"))
                                  (parser-pure '()))))]
              [skip-ws (skip-many (parser-or space line-comment))]
              
              ;; Token wrapper
              [token (lambda (p) (parser-left p skip-ws))]
              
              ;; Identifier parser
              [ident-parser
               (try
                (token
                 (parser-bind ident-start
                              (lambda (first)
                                      (parser-bind (many ident-letter)
                                                   (lambda (rest)
                                                           (let ([name (list->string (cons first rest))])
                                                                (if (member name reserved-names)
                                                                    (parser-fail (string-append "reserved word: " name))
                                                                    (parser-pure name)))))))))]
              
              ;; Reserved word parser
              [reserved-parser
               (lambda (name)
                       (try
                        (token
                         (parser-bind (string-parser name)
                                      (lambda (_)
                                              (parser-bind (not-followed-by ident-letter)
                                                           (lambda (_)
                                                                   (parser-pure name))))))))]
              
              ;; Operator parser
              [operator-parser
               (try
                (token
                 (parser-bind op-start
                              (lambda (first)
                                      (parser-bind (many op-letter)
                                                   (lambda (rest)
                                                           (let ([op (list->string (cons first rest))])
                                                                (if (member op reserved-ops)
                                                                    (parser-fail (string-append "reserved operator: " op))
                                                                    (parser-pure op)))))))))]
              
              ;; Reserved operator parser
              [reservedOp-parser
               (lambda (op)
                       (try
                        (token
                         (parser-bind (string-parser op)
                                      (lambda (_)
                                              (parser-bind (not-followed-by op-letter)
                                                           (lambda (_)
                                                                   (parser-pure op))))))))]
              
              ;; Common tokens
              [lparen (token (char #\())]
              [rparen (token (char #\)))]
              [lbrace (token (char #\{))]
              [rbrace (token (char #\}))]
              [lbracket (token (char #\[))]
              [rbracket (token (char #\]))]
              [comma (token (char #\,))]
              [semi (token (char #\;))]
              [colon (token (char #\:))]
              [dot (token (char #\.))]
              
              ;; Number parser
              [number-parser (token decimal)]
              
              ;; String parser - use integer->char to avoid linter corruption
              [string-lit-parser
               (token
                (between (char #\")
                         (char #\")
                         (parser-map list->string
                                     (many (parser-or
                                            (parser-then
                                             (char (integer->char 92)) ; backslash
                                             (choice (list
                                                      (parser-then (char (integer->char 110)) (parser-pure (integer->char 10)))  ; n -> newline
                                                      (parser-then (char (integer->char 116)) (parser-pure (integer->char 9)))   ; t -> tab
                                                      (parser-then (char #\") (parser-pure #\"))
                                                      (parser-then (char (integer->char 92))
                                                                   (parser-pure (integer->char 92))))))
                                            (satisfy (lambda (c) (not (char=? c #\")))
                                                     "string character"))))))])
             
             ;; Return token parser record
             (list 'token-parser
                   (cons 'identifier ident-parser)
                   (cons 'reserved reserved-parser)
                   (cons 'operator operator-parser)
                   (cons 'reservedOp reservedOp-parser)
                   (cons 'number number-parser)
                   (cons 'string-lit string-lit-parser)
                   (cons 'lparen lparen)
                   (cons 'rparen rparen)
                   (cons 'lbrace lbrace)
                   (cons 'rbrace rbrace)
                   (cons 'lbracket lbracket)
                   (cons 'rbracket rbracket)
                   (cons 'comma comma)
                   (cons 'semi semi)
                   (cons 'colon colon)
                   (cons 'dot dot)
                   (cons 'parens (lambda (p) (between lparen rparen p)))
                   (cons 'braces (lambda (p) (between lbrace rbrace p)))
                   (cons 'brackets (lambda (p) (between lbracket rbracket p)))
                   (cons 'comma-sep (lambda (p) (sep-by p comma)))
                   (cons 'semi-sep (lambda (p) (sep-by p semi)))
                   (cons 'skip-ws skip-ws)))))

;;; lang-get : Alist × Symbol × Default → Value
(define (lang-get def key default)
  (let ([pair (assq key def)])
       (if pair (cdr pair) default)))

;;; token-parser-get : TokenParser × Symbol → Parser
(define (tp-get tp key)
  (cdr (assq key (cdr tp))))

;;; ============================================================
;;; Common DSL Patterns
;;; ============================================================

;;; reserved-choice : (List String) × (String → Parser a) → Parser a
;;; Parse one of several reserved words.
(define (reserved-choice words reserved-parser)
  (choice (map reserved-parser words)))

;;; binary-op : TokenParser × String × (a × a → a) → Parser (a × a → a)
;;; Create binary operator parser.
(define (binary-op tp op-str builder)
  (parser-then ((tp-get tp 'reservedOp) op-str)
               (parser-pure builder)))

;;; prefix-unary : TokenParser × String × (a → a) → Operator
;;; Create prefix unary operator.
(define (prefix-unary tp op-str builder)
  (prefix-op (parser-then ((tp-get tp 'reservedOp) op-str) (parser-pure op-str))
             (lambda (op arg) (builder arg))))

;;; postfix-unary : TokenParser × String × (a → a) → Operator
;;; Create postfix unary operator.
(define (postfix-unary tp op-str builder)
  (postfix-op (parser-then ((tp-get tp 'reservedOp) op-str) (parser-pure op-str))
              (lambda (arg op) (builder arg))))

;;; ============================================================
;;; Language Definition Helpers
;;; ============================================================

;;; scheme-style : LanguageDef
;;; Language definition for Scheme-like languages.
(define scheme-style
  (list (cons 'reserved-names '("define" "lambda" "let" "let*" "letrec"
                                "if" "cond" "case" "and" "or" "not"
                                "begin" "set!" "quote" "quasiquote"))
        (cons 'reserved-ops '())
        (cons 'comment-line ";")
        (cons 'ident-start (parser-or letter (one-of "!$%&*/:<=>?^_~")))
        (cons 'ident-letter (parser-or alpha-num (one-of "!$%&*/:<=>?^_~+-.")))
        (cons 'op-start (one-of "+-*/<>="))
        (cons 'op-letter (one-of "+-*/<>="))))

;;; c-style : LanguageDef
;;; Language definition for C-like languages.
(define c-style
  (list (cons 'reserved-names '("if" "else" "while" "for" "do" "return"
                                "break" "continue" "switch" "case" "default"
                                "int" "char" "void" "struct" "enum" "typedef"
                                "const" "static" "extern" "true" "false" "null"))
        (cons 'reserved-ops '("+" "-" "*" "/" "%" "=" "==" "!=" "<" ">" "<=" ">="
                              "&&" "||" "!" "&" "|" "^" "~" "<<" ">>" "++" "--"
                              "+=" "-=" "*=" "/=" "." "->" "?" ":"))
        (cons 'comment-line "//")
        (cons 'ident-start (parser-or letter (char #\_)))
        (cons 'ident-letter (parser-or alpha-num (char #\_)))
        (cons 'op-start (one-of "+-*/%=<>!&|^~.?:"))
        (cons 'op-letter (one-of "+-*/%=<>!&|^~."))))

;;; haskell-style : LanguageDef
;;; Language definition for Haskell-like languages.
(define haskell-style
  (list (cons 'reserved-names '("let" "in" "where" "case" "of" "if" "then" "else"
                                "data" "type" "newtype" "class" "instance"
                                "module" "import" "qualified" "as" "hiding"
                                "do" "return" "infix" "infixl" "infixr"))
        (cons 'reserved-ops '("=" "::" "->" "<-" "=>" "|" "\" "@" "~" "!"))
        (cons 'comment-line "--")
        (cons 'ident-start letter)
        (cons 'ident-letter (parser-or alpha-num (one-of "_'")))
        (cons 'op-start (one-of ":!#$%&*+./<=>?@\^|-~"))
        (cons 'op-letter (one-of ":!#$%&*+./<=>?@\^|-~"))))

;;; ============================================================
;;; Pretty Printing for Parse Errors
;;; ============================================================

;;; format-parse-error : Error × String → String
;;; Format parse error with source context.
(define (format-parse-error-with-context err source)
  (let* ([pos (error-pos err)]
         [line (pos-line pos)]
         [col (pos-col pos)]
         [lines (string-split source (integer->char 10))]
         [source-line (if (<= line (length lines))
                          (list-ref lines (- line 1))
                          "")]
         [pointer (string-append (make-string (- col 1) #\space) "^")])
        (string-append
         (format-error err)
         "

"
         (number->string line) " | " source-line "
"
         (make-string (+ (string-length (number->string line)) 3) #\space)
         pointer)))

;;; string-split : String × Char → (List String)
(define (string-split str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (cons (list->string (reverse current)) result))]
        [(char=? (car chars) delim)
         (loop (cdr chars) '() (cons (list->string (reverse current)) result))]
        [else
         (loop (cdr chars) (cons (car chars) current) result)])))

;;; make-string : Nat × Char → String
(define (make-string n ch)
  (list->string (make-list n ch)))

;;; make-list : Nat × a → (List a)
(define (make-list n val)
  (if (<= n 0)
      '()
      (cons val (make-list (- n 1) val))))

;;; ============================================================
;;; Keyword and Alias Utilities
;;; ============================================================

;;; keyword-aliases : (List (List String)) × (String → a) → Parser a
;;; Parse any of several keyword groups, mapping to results.
;;; Each group is (list result-value alias1 alias2 ...).
;;;
;;; Example:
;;;   (keyword-aliases '((north "north" "n")
;;;                      (south "south" "s"))
;;;                    identity)
;;; Parses "north" or "n" → 'north, "south" or "s" → 'south
(define (keyword-aliases groups builder)
  (choice
   (map (lambda (group)
                (let ([result-val (car group)]
                      [aliases (cdr group)])
                     (parser-map (lambda (_) (builder result-val))
                                 (choice (map (lambda (kw)
                                                      (try (kw-boundary kw)))
                                              aliases)))))
        groups)))

;;; kw-boundary : String → Parser String
;;; Parse keyword followed by word boundary (whitespace or EOF).
(define (kw-boundary kw)
  (parser-left (string-parser kw) word-boundary))

;;; word-boundary : Parser ()
;;; Succeed at word boundary (whitespace, EOF, or punctuation).
(define word-boundary
  (parser-or
   (look-ahead space)
   (parser-or
    (look-ahead eof)
    (look-ahead (one-of ",.;:!?()[]{}")))))

;;; reserved-words : (List String) → Parser String
;;; Parse any of the given reserved words with boundary checking.
;;; Uses try to allow backtracking on partial matches.
(define (reserved-words words)
  (choice (map (lambda (w) (try (kw-boundary w))) words)))

;;; ============================================================
;;; String Parsing with Escapes
;;; ============================================================

;;; string-with-escapes : Char × Char → Parser String
;;; Parse string with escape sequences.
;;; Parameters: quote-char, escape-char (usually #\" and #\)
(define (string-with-escapes quote-char escape-char)
  (between (char quote-char)
           (char quote-char)
           (parser-map list->string
                       (many (string-escape-char quote-char escape-char)))))

;;; string-escape-char : Char x Char -> Parser Char
;;; Parse a single string character with escape handling.
;;; Uses integer->char to avoid linter corruption of character literals.
(define (string-escape-char quote-char escape-char)
  (parser-or
   ;; Escape sequences
   (parser-then
    (char escape-char)
    (choice (list
             (parser-then (char (integer->char 110)) (parser-pure (integer->char 10)))  ; n -> newline
             (parser-then (char (integer->char 116)) (parser-pure (integer->char 9)))   ; t -> tab
             (parser-then (char (integer->char 114)) (parser-pure (integer->char 13)))  ; r -> carriage return
             (parser-then (char quote-char) (parser-pure quote-char))                   ; quote
             (parser-then (char escape-char) (parser-pure escape-char))                 ; escape
             (parser-then (char (integer->char 48)) (parser-pure (integer->char 0)))    ; 0 -> null
             )))
   ;; Regular character (not quote or escape)
   (satisfy (lambda (c)
                    (and (not (char=? c quote-char))
                         (not (char=? c escape-char))))
            "string character")))

;;; double-quoted-string : Parser String
;;; Parse "..." with standard escape sequences.
(define double-quoted-string
  (string-with-escapes #\" (integer->char 92)))
;;; single-quoted-string : Parser String
;;; Parse '...' with standard escape sequences.
(define single-quoted-string
  (string-with-escapes #\' (integer->char 92)))

;;; ============================================================
;;; Command Parser Utilities
;;; ============================================================
;;;
;;; Utilities for building command-line style parsers.

;;; command : String × Parser a → Parser a
;;; Parse command keyword followed by arguments.
(define (command kw args-parser)
  (parser-then (lexeme (kw-boundary kw))
               args-parser))

;;; command-with-aliases : (List String) × Parser a → Parser a
;;; Parse command with multiple aliases.
(define (command-with-aliases aliases args-parser)
  (parser-then (lexeme (reserved-words aliases))
               args-parser))

;;; optional-arg : Parser a × a → Parser a
;;; Parse optional argument with default value.
(define (optional-arg p default)
  (parser-or (parser-then spaces1 p)
             (parser-pure default)))

;;; drop-while : (a → Bool) × (List a) → (List a)
(define (drop-while pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (drop-while pred (cdr lst))]
   [else lst]))

;;; string-trim-both : String → String
;;; Trim whitespace from both ends.
(define (string-trim-both s)
  (let* ([chars (string->list s)]
         [trimmed-left (drop-while char-whitespace? chars)]
         [trimmed-right (reverse (drop-while char-whitespace? (reverse trimmed-left)))])
        (list->string trimmed-right)))

;;; rest-of-input : Parser String
;;; Consume and return all remaining input.
(define rest-of-input
  (make-parser
   (lambda (state)
           (let ([input (state-input state)])
                (right (cons input (make-state "" (state-pos state))))))))

;;; rest-trimmed : Parser String
;;; Rest of input with whitespace trimmed.
(define rest-trimmed
  (parser-map string-trim-both rest-of-input))
