;;; core/parse.ss — Parser Combinator Library
;;;
;;; The ears for DUCKIE — Parsec-style parser combinators.
;;;
;;; A Parser is a function from input to result:
;;;   Parser a = String → ParseResult a
;;;
;;; ParseResult a =
;;;   | (ok value remaining)    ; success with value and leftover input
;;;   | (err expected position) ; failure with expectation and position
;;;
;;; Encoding:
;;;   Success: (cons 'ok (cons value remaining))
;;;   Failure: (cons 'err (cons expected position))
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;; Parsers compose via monadic bind, alternative choice, and repetition.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - prim.ss (string primitives)

(load "prelude.ss")

;;; ============================================================
;;; ParseResult Construction and Destructuring
;;; ============================================================

;;; success : a × String → ParseResult a
;;; Construct a successful parse result.
(define (success value remaining)
  (cons 'ok (cons value remaining)))

;;; failure : String × Nat → ParseResult a
;;; Construct a failed parse result.
;;; Expected is a description string, position is character offset.
(define (failure expected position)
  (cons 'err (cons expected position)))

;;; parse-ok? : ParseResult → Boolean
(define (parse-ok? result)
  (eq? (car result) 'ok))

;;; parse-err? : ParseResult → Boolean
(define (parse-err? result)
  (eq? (car result) 'err))

;;; result-value : ParseResult → a
(define (result-value result)
  (cadr result))

;;; result-remaining : ParseResult → String
(define (result-remaining result)
  (cddr result))

;;; result-expected : ParseResult → String
(define (result-expected result)
  (cadr result))

;;; result-position : ParseResult → Nat
(define (result-position result)
  (cddr result))

;;; ============================================================
;;; Running Parsers
;;; ============================================================

;;; run-parser : Parser a × String → ParseResult a
;;; Apply a parser to an input string.
(define (run-parser parser input)
  (parser input))

;;; parse : Parser a × String → a | #f
;;; Parse and return value on success, #f on failure.
(define (parse parser input)
  (let ([result (run-parser parser input)])
    (if (parse-ok? result)
        (result-value result)
        #f)))

;;; parse-complete : Parser a × String → a | #f
;;; Parse and require full consumption of input.
(define (parse-complete parser input)
  (let ([result (run-parser parser input)])
    (if (and (parse-ok? result)
             (string=? (result-remaining result) ""))
        (result-value result)
        #f)))

;;; ============================================================
;;; Primitive Parsers
;;; ============================================================

;;; pure : a → Parser a
;;; Always succeeds with the given value, consumes nothing.
(define (pure value)
  (lambda (input)
    (success value input)))

;;; fail : String → Parser a
;;; Always fails with the given message.
(define (fail msg)
  (lambda (input)
    (failure msg (string-length input))))

;;; item : Parser Char
;;; Consume one character, fail on empty input.
(define item
  (lambda (input)
    (if (= (string-length input) 0)
        (failure "any character" 0)
        (success (string-ref input 0)
                 (substring input 1 (string-length input))))))

;;; eof : Parser Unit
;;; Succeed only at end of input.
(define eof
  (lambda (input)
    (if (= (string-length input) 0)
        (success '() input)
        (failure "end of input" (string-length input)))))

;;; satisfy : (Char → Bool) × String → Parser Char
;;; Consume a character if predicate holds.
(define (satisfy pred label)
  (lambda (input)
    (if (= (string-length input) 0)
        (failure label 0)
        (let ([c (string-ref input 0)])
          (if (pred c)
              (success c (substring input 1 (string-length input)))
              (failure label (string-length input)))))))

;;; ============================================================
;;; Character Parsers
;;; ============================================================

;;; char : Char → Parser Char
;;; Match a specific character.
(define (char target)
  (satisfy (lambda (c) (char=? c target))
           (list->string (list target))))

;;; digit : Parser Char
;;; Match a digit character.
(define digit
  (satisfy char-numeric? "digit"))

;;; alpha : Parser Char
;;; Match an alphabetic character.
(define alpha
  (satisfy char-alphabetic? "letter"))

;;; space : Parser Char
;;; Match a whitespace character.
(define space
  (satisfy char-whitespace? "whitespace"))

;;; alphanum : Parser Char
;;; Match an alphanumeric character.
(define alphanum
  (satisfy (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))
           "alphanumeric"))

;;; upper : Parser Char
;;; Match an uppercase letter.
(define upper
  (satisfy char-upper-case? "uppercase letter"))

;;; lower : Parser Char
;;; Match a lowercase letter.
(define lower
  (satisfy char-lower-case? "lowercase letter"))

;;; string-p : String → Parser String
;;; Match an exact string.
(define (string-p target)
  (lambda (input)
    (let ([tlen (string-length target)]
          [ilen (string-length input)])
      (if (< ilen tlen)
          (failure target (string-length input))
          (let ([prefix (substring input 0 tlen)])
            (if (string=? prefix target)
                (success target (substring input tlen ilen))
                (failure target (string-length input))))))))

;;; ============================================================
;;; Parser Combinators — Sequencing
;;; ============================================================

;;; bind : Parser a × (a → Parser b) → Parser b
;;; Monadic bind — run first parser, feed result to second.
;;; This is the heart of parser composition.
(define (bind parser f)
  (lambda (input)
    (let ([result (parser input)])
      (if (parse-ok? result)
          (let ([value (result-value result)]
                [remaining (result-remaining result)])
            ((f value) remaining))
          result))))

;;; seq : Parser a × Parser b → Parser b
;;; Run both parsers, keep second result (>>).
(define (seq p1 p2)
  (bind p1 (lambda (_) p2)))

;;; seq-left : Parser a × Parser b → Parser a
;;; Run both parsers, keep first result (<<).
(define (seq-left p1 p2)
  (bind p1 (lambda (a)
    (bind p2 (lambda (_)
      (pure a))))))

;;; ============================================================
;;; Parser Combinators — Choice
;;; ============================================================

;;; alt : Parser a × Parser a → Parser a
;;; Try first parser, on failure try second (<|>).
(define (alt p1 p2)
  (lambda (input)
    (let ([result (p1 input)])
      (if (parse-ok? result)
          result
          (p2 input)))))

;;; choice : (List (Parser a)) → Parser a
;;; Try parsers in order until one succeeds.
(define (choice parsers)
  (if (null? parsers)
      (fail "no alternatives")
      (if (null? (cdr parsers))
          (car parsers)
          (alt (car parsers) (choice (cdr parsers))))))

;;; ============================================================
;;; Parser Combinators — Transformation
;;; ============================================================

;;; map-p : (a → b) × Parser a → Parser b
;;; Transform the result of a parser (fmap).
(define (map-p f parser)
  (bind parser (lambda (a) (pure (f a)))))

;;; ============================================================
;;; Parser Combinators — Repetition
;;; ============================================================

;;; many-helper : Parser a × (List a) × String → (List a × String)
;;; Helper for many — accumulate results until parser fails.
(define (many-helper parser acc input)
  (let ([result (parser input)])
    (if (parse-ok? result)
        (many-helper parser
                     (cons (result-value result) acc)
                     (result-remaining result))
        ;; Parser failed, return accumulated results
        (cons (reverse acc) input))))

;;; many : Parser a → Parser (List a)
;;; Zero or more, greedy.
(define (many parser)
  (lambda (input)
    (let ([result (many-helper parser '() input)])
      (success (car result) (cdr result)))))

;;; many1 : Parser a → Parser (List a)
;;; One or more.
(define (many1 parser)
  (bind parser (lambda (first)
    (bind (many parser) (lambda (rest)
      (pure (cons first rest)))))))

;;; sep-by : Parser a × Parser sep → Parser (List a)
;;; Parse items separated by separator.
(define (sep-by parser sep)
  (alt
    ;; Try to parse first item, then zero or more (sep item) pairs
    (bind parser (lambda (first)
      (bind (many (seq sep parser)) (lambda (rest)
        (pure (cons first rest))))))
    ;; Or return empty list
    (pure '())))

;;; sep-by1 : Parser a × Parser sep → Parser (List a)
;;; Parse one or more items separated by separator.
(define (sep-by1 parser sep)
  (bind parser (lambda (first)
    (bind (many (seq sep parser)) (lambda (rest)
      (pure (cons first rest)))))))

;;; optional : Parser a → Parser (Maybe a)
;;; Parse or return nothing.
;;; Maybe a = '() | (a)
(define (optional parser)
  (alt
    (map-p (lambda (x) (list x)) parser)
    (pure '())))

;;; ============================================================
;;; Parser Combinators — Delimiters
;;; ============================================================

;;; between : Parser open × Parser close × Parser a → Parser a
;;; Parse something between delimiters.
(define (between open close parser)
  (seq open
    (seq-left parser close)))

;;; ============================================================
;;; Utility Parsers
;;; ============================================================

;;; spaces : Parser (List Char)
;;; Parse zero or more whitespace characters.
(define spaces
  (many space))

;;; spaces1 : Parser (List Char)
;;; Parse one or more whitespace characters.
(define spaces1
  (many1 space))

;;; lexeme : Parser a → Parser a
;;; Parse something and skip trailing whitespace.
(define (lexeme parser)
  (seq-left parser spaces))

;;; symbol : String → Parser String
;;; Parse a string and skip trailing whitespace.
(define (symbol str)
  (lexeme (string-p str)))

;;; parens : Parser a → Parser a
;;; Parse something in parentheses.
(define (parens parser)
  (between (symbol "(") (symbol ")") parser))

;;; brackets : Parser a → Parser a
;;; Parse something in brackets.
(define (brackets parser)
  (between (symbol "[") (symbol "]") parser))

;;; braces : Parser a → Parser a
;;; Parse something in braces.
(define (braces parser)
  (between (symbol "{") (symbol "}") parser))

;;; ============================================================
;;; Number Parsers
;;; ============================================================

;;; nat : Parser Nat
;;; Parse a natural number (sequence of digits).
(define nat
  (bind (many1 digit) (lambda (digits)
    (pure (string->number (list->string digits))))))

;;; int : Parser Int
;;; Parse an integer (optional minus sign followed by digits).
(define int
  (bind (optional (char #\-)) (lambda (sign)
    (bind nat (lambda (n)
      (pure (if (null? sign) n (- n))))))))

;;; ============================================================
;;; Word and Identifier Parsers
;;; ============================================================

;;; word : Parser String
;;; Parse a word (sequence of alphabetic characters).
(define word
  (map-p list->string (many1 alpha)))

;;; identifier : Parser String
;;; Parse an identifier (letter followed by alphanumerics/underscores).
(define identifier
  (bind alpha (lambda (first)
    (bind (many (alt alphanum (char #\_))) (lambda (rest)
      (pure (list->string (cons first rest))))))))

;;; ============================================================
;;; Chainl and Chainr — Operator Parsing
;;; ============================================================

;;; chainl1 : Parser a × Parser (a × a → a) → Parser a
;;; Parse one or more items separated by left-associative operators.
(define (chainl1 parser op)
  (letrec
    ([rest (lambda (x)
             (alt
               (bind op (lambda (f)
                 (bind parser (lambda (y)
                   (rest (f x y))))))
               (pure x)))])
    (bind parser rest)))

;;; chainr1 : Parser a × Parser (a × a → a) → Parser a
;;; Parse one or more items separated by right-associative operators.
(define (chainr1 parser op)
  (bind parser (lambda (x)
    (alt
      (bind op (lambda (f)
        (bind (chainr1 parser op) (lambda (y)
          (pure (f x y))))))
      (pure x)))))

;;; ============================================================
;;; Helpers
;;; ============================================================

;;; one-of : String → Parser Char
;;; Match any character in the given string.
(define (one-of chars)
  (satisfy (lambda (c)
             (let loop ([i 0])
               (cond
                 [(>= i (string-length chars)) #f]
                 [(char=? c (string-ref chars i)) #t]
                 [else (loop (+ i 1))])))
           (string-append "one of: " chars)))

;;; none-of : String → Parser Char
;;; Match any character not in the given string.
(define (none-of chars)
  (satisfy (lambda (c)
             (let loop ([i 0])
               (cond
                 [(>= i (string-length chars)) #t]
                 [(char=? c (string-ref chars i)) #f]
                 [else (loop (+ i 1))])))
           (string-append "none of: " chars)))

;;; end-by : Parser a × Parser sep → Parser (List a)
;;; Parse items each ending with separator.
(define (end-by parser sep)
  (many (seq-left parser sep)))

;;; end-by1 : Parser a × Parser sep → Parser (List a)
;;; Parse one or more items each ending with separator.
(define (end-by1 parser sep)
  (many1 (seq-left parser sep)))

;;; count : Nat × Parser a → Parser (List a)
;;; Parse exactly n items.
(define (count n parser)
  (if (zero? n)
      (pure '())
      (bind parser (lambda (x)
        (bind (count (- n 1) parser) (lambda (xs)
          (pure (cons x xs))))))))
