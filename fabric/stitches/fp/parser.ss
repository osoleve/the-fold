;;; fabric/stitches/fp/parser.ss — Monadic Parser Combinators
;;;
;;; A practical parser combinator library for building DSLs and parsers.
;;; Uses the Maybe and Either types from combinators.ss for results.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Core parser type with position tracking
;;;   - Primitive parsers (char, string, satisfy, eof)
;;;   - Sequencing (>>=, >>, <*, *>)
;;;   - Alternation (<|>, choice, try)
;;;   - Repetition (many, some, sepBy, count)
;;;   - Lookahead (lookAhead, notFollowedBy)
;;;   - Error handling (label, withError)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "prelude.ss")
(load "fp/combinators.ss")

;;; ============================================================
;;; Character Constants (to avoid formatter issues)
;;; ============================================================

(define %newline (integer->char 10))
(define %tab (integer->char 9))
(define %return (integer->char 13))
(define %backspace (integer->char 8))
(define %page (integer->char 12))

;;; ============================================================
;;; Parser State
;;; ============================================================

;;; Parser state contains:
;;;   - input: remaining input string
;;;   - pos: current position (line, column, offset)

;;; make-pos : Nat × Nat × Nat → Pos
(define (make-pos line col offset)
  (list 'pos line col offset))

;;; pos? : Any → Boolean
(define (pos? p)
  (and (pair? p) (eq? (car p) 'pos)))

;;; pos-line : Pos → Nat
(define (pos-line p) (list-ref p 1))

;;; pos-col : Pos → Nat
(define (pos-col p) (list-ref p 2))

;;; pos-offset : Pos → Nat
(define (pos-offset p) (list-ref p 3))

;;; initial-pos : Pos
(define initial-pos (make-pos 1 1 0))

;;; advance-pos : Pos × Char → Pos
;;; Advance position by one character.
(define (advance-pos pos ch)
  (if (char=? ch %newline)
      (make-pos (+ (pos-line pos) 1) 1 (+ (pos-offset pos) 1))
      (make-pos (pos-line pos) (+ (pos-col pos) 1) (+ (pos-offset pos) 1))))

;;; make-state : String × Pos → State
(define (make-state input pos)
  (list 'state input pos))

;;; state? : Any → Boolean
(define (state? s)
  (and (pair? s) (eq? (car s) 'state)))

;;; state-input : State → String
(define (state-input s) (list-ref s 1))

;;; state-pos : State → Pos
(define (state-pos s) (list-ref s 2))

;;; initial-state : String → State
(define (initial-state input)
  (make-state input initial-pos))

;;; ============================================================
;;; Parse Error
;;; ============================================================

;;; make-error : Pos × String × (List String) → Error
;;; Create a parse error with position, message, and expected items.
(define (make-parse-error pos message expected)
  (list 'parse-error pos message expected))

;;; parse-error? : Any → Boolean
(define (parse-error? e)
  (and (pair? e) (eq? (car e) 'parse-error)))

;;; error-pos : Error → Pos
(define (error-pos e) (list-ref e 1))

;;; error-message : Error → String
(define (error-message e) (list-ref e 2))

;;; error-expected : Error → (List String)
(define (error-expected e) (list-ref e 3))

;;; merge-errors : Error × Error → Error
;;; Merge two errors, keeping the one at furthest position.
(define (merge-errors e1 e2)
  (let ([p1 (pos-offset (error-pos e1))]
        [p2 (pos-offset (error-pos e2))])
       (cond
        [(> p1 p2) e1]
        [(< p1 p2) e2]
        [else (make-parse-error
               (error-pos e1)
               (error-message e1)
               (append (error-expected e1) (error-expected e2)))])))

;;; format-error : Error → String
(define (format-error err)
  (let ([pos (error-pos err)]
        [msg (error-message err)]
        [expected (error-expected err)])
       (string-append
        "Parse error at line " (number->string (pos-line pos))
        ", column " (number->string (pos-col pos))
        ": " msg
        (if (null? expected)
            ""
            (string-append ", expected: " (format-expected expected))))))

;;; format-expected : (List String) → String
(define (format-expected exps)
  (cond
   [(null? exps) ""]
   [(null? (cdr exps)) (car exps)]
   [else (string-append (car exps) " or " (format-expected (cdr exps)))]))

;;; ============================================================
;;; Parser Type
;;; ============================================================

;;; A Parser is: State → Either Error (Value × State)
;;;
;;; - On success: (right (value . new-state))
;;; - On failure: (left error)

;;; make-parser : (State → Either Error (Value × State)) → Parser
(define (make-parser run-fn)
  (list 'parser run-fn))

;;; parser? : Any → Boolean
(define (parser? p)
  (and (pair? p) (eq? (car p) 'parser)))

;;; run-parser : Parser × State → Either Error (Value × State)
(define (run-parser parser state)
  ((cadr parser) state))

;;; parse : Parser × String → Either Error Value
;;; Run parser on input string.
(define (parse parser input)
  (let ([result (run-parser parser (initial-state input))])
       (if (right? result)
           (right (car (from-right result)))  ; Extract value
           result)))  ; Return error

;;; parse-all : Parser × String → Either Error Value
;;; Run parser and require complete input consumption.
(define (parse-all parser input)
  (let ([full-parser (parser-left parser eof)])
       (parse full-parser input)))

;;; ============================================================
;;; Primitive Parsers
;;; ============================================================

;;; pure : a → Parser a
;;; Parser that succeeds with value without consuming input.
(define (parser-pure x)
  (make-parser
   (lambda (state)
           (right (cons x state)))))

;;; fail : String → Parser a
;;; Parser that always fails with message.
(define (parser-fail message)
  (make-parser
   (lambda (state)
           (left (make-parse-error (state-pos state) message '())))))

;;; eof : Parser ()
;;; Parser that succeeds only at end of input.
(define eof
  (make-parser
   (lambda (state)
           (let ([input (state-input state)])
                (if (string=? input "")
                    (right (cons '() state))
                    (left (make-parse-error
                           (state-pos state)
                           "expected end of input"
                           '("end of input"))))))))

;;; any-char : Parser Char
;;; Parser that consumes any single character.
(define any-char
  (make-parser
   (lambda (state)
           (let ([input (state-input state)])
                (if (string=? input "")
                    (left (make-parse-error
                           (state-pos state)
                           "unexpected end of input"
                           '("any character")))
                    (let* ([ch (string-ref input 0)]
                           [rest (substring input 1 (string-length input))]
                           [new-pos (advance-pos (state-pos state) ch)]
                           [new-state (make-state rest new-pos)])
                          (right (cons ch new-state))))))))

;;; satisfy : (Char → Boolean) → String → Parser Char
;;; Parser that consumes char satisfying predicate.
(define (satisfy pred description)
  (make-parser
   (lambda (state)
           (let ([input (state-input state)])
                (if (string=? input "")
                    (left (make-parse-error
                           (state-pos state)
                           "unexpected end of input"
                           (list description)))
                    (let ([ch (string-ref input 0)])
                         (if (pred ch)
                             (let* ([rest (substring input 1 (string-length input))]
                                    [new-pos (advance-pos (state-pos state) ch)]
                                    [new-state (make-state rest new-pos)])
                                   (right (cons ch new-state)))
                             (left (make-parse-error
                                    (state-pos state)
                                    (string-append "unexpected '" (string ch) "'")
                                    (list description))))))))))

;;; char : Char → Parser Char
;;; Parser that matches specific character.
(define (char c)
  (satisfy (lambda (ch) (char=? ch c))
           (string-append "'" (string c) "'")))

;;; char-ci : Char → Parser Char
;;; Case-insensitive character match.
(define (char-ci c)
  (satisfy (lambda (ch) (char-ci=? ch c))
           (string-append "'" (string c) "' (case-insensitive)")))

;;; one-of : String → Parser Char
;;; Match any character in string.
(define (one-of chars)
  (satisfy (lambda (ch)
                   (let loop ([i 0])
                        (if (>= i (string-length chars))
                            #f
                            (or (char=? ch (string-ref chars i))
                                (loop (+ i 1))))))
           (string-append "one of '" chars "'")))

;;; none-of : String → Parser Char
;;; Match any character NOT in string.
(define (none-of chars)
  (satisfy (lambda (ch)
                   (let loop ([i 0])
                        (if (>= i (string-length chars))
                            #t
                            (and (not (char=? ch (string-ref chars i)))
                                 (loop (+ i 1))))))
           (string-append "none of '" chars "'")))

;;; ============================================================
;;; Character Class Parsers
;;; ============================================================

;;; digit : Parser Char
(define digit (satisfy char-numeric? "digit"))

;;; letter : Parser Char
(define letter (satisfy char-alphabetic? "letter"))

;;; alpha-num : Parser Char
(define alpha-num
  (satisfy (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))
           "alphanumeric"))

;;; space : Parser Char
(define space (satisfy char-whitespace? "whitespace"))

;;; lower : Parser Char
(define lower (satisfy char-lower-case? "lowercase letter"))

;;; upper : Parser Char
(define upper (satisfy char-upper-case? "uppercase letter"))

;;; newline : Parser Char
(define newline-char (char %newline))

;;; tab : Parser Char
(define tab-char (char %tab))

;;; ============================================================
;;; String Parsers
;;; ============================================================

;;; string : String → Parser String
;;; Match exact string.
(define (string-parser str)
  (if (string=? str "")
      (parser-pure "")
      (make-parser
       (lambda (state)
               (let* ([input (state-input state)]
                      [len (string-length str)])
                     (if (< (string-length input) len)
                         (left (make-parse-error
                                (state-pos state)
                                "unexpected end of input"
                                (list (string-append "\"" str "\""))))
                         (let ([prefix (substring input 0 len)])
                              (if (string=? prefix str)
                                  (let* ([rest (substring input len (string-length input))]
                                         [new-pos (fold-left
                                                   (lambda (p i)
                                                           (advance-pos p (string-ref str i)))
                                                   (state-pos state)
                                                   (iota len))]
                                         [new-state (make-state rest new-pos)])
                                        (right (cons str new-state)))
                                  (left (make-parse-error
                                         (state-pos state)
                                         (string-append "expected \"" str "\"")
                                         (list (string-append "\"" str "\""))))))))))))

;;; ============================================================
;;; Monad Operations
;;; ============================================================

;;; parser-bind : Parser a × (a → Parser b) → Parser b
;;; Monadic bind (>>=).
(define (parser-bind p f)
  (make-parser
   (lambda (state)
           (let ([result (run-parser p state)])
                (if (left? result)
                    result
                    (let* ([val-state (from-right result)]
                           [val (car val-state)]
                           [new-state (cdr val-state)])
                          (run-parser (f val) new-state)))))))

;;; parser-then : Parser a × Parser b → Parser b
;;; Sequence, discarding first result (>>).
(define (parser-then p1 p2)
  (parser-bind p1 (lambda (_) p2)))

;;; parser-left : Parser a × Parser b → Parser a
;;; Sequence, discarding second result (<*).
(define (parser-left p1 p2)
  (parser-bind p1 (lambda (x)
                          (parser-bind p2 (lambda (_)
                                                  (parser-pure x))))))

;;; parser-right : Parser a × Parser b → Parser b
;;; Sequence, discarding first result (*>). Same as parser-then.
(define parser-right parser-then)

;;; parser-map : (a → b) → Parser a → Parser b
;;; Functor map.
(define (parser-map f p)
  (parser-bind p (lambda (x) (parser-pure (f x)))))

;;; parser-ap : Parser (a → b) × Parser a → Parser b
;;; Applicative apply.
(define (parser-ap pf pa)
  (parser-bind pf (lambda (f)
                          (parser-bind pa (lambda (a)
                                                  (parser-pure (f a)))))))

;;; ============================================================
;;; Alternation
;;; ============================================================

;;; parser-or : Parser a × Parser a → Parser a
;;; Try first parser, if it fails without consuming input, try second (<|>).
(define (parser-or p1 p2)
  (make-parser
   (lambda (state)
           (let ([result1 (run-parser p1 state)])
                (if (right? result1)
                    result1
                    ;; Check if any input was consumed
                    (let ([err1 (from-left result1)]
                          [start-offset (pos-offset (state-pos state))]
                          [err-offset (pos-offset (error-pos (from-left result1)))])
                         (if (> err-offset start-offset)
                             ;; Input consumed, don't try alternative
                             result1
                             ;; No input consumed, try p2
                             (let ([result2 (run-parser p2 state)])
                                  (if (right? result2)
                                      result2
                                      (left (merge-errors err1 (from-left result2))))))))))))

;;; choice : (List Parser) → Parser
;;; Try parsers in order.
(define (choice parsers)
  (if (null? parsers)
      (parser-fail "no alternatives")
      (fold-right parser-or (car parsers) (cdr parsers))))

;;; try : Parser a → Parser a
;;; Try parser, on failure pretend no input was consumed.
(define (try p)
  (make-parser
   (lambda (state)
           (let ([result (run-parser p state)])
                (if (right? result)
                    result
                    ;; Reset error position to start
                    (left (make-parse-error
                           (state-pos state)
                           (error-message (from-left result))
                           (error-expected (from-left result)))))))))

;;; optional : Parser a → a → Parser a
;;; Try parser, return default on failure.
(define (optional p default)
  (parser-or p (parser-pure default)))

;;; option-maybe : Parser a → Parser (Maybe a)
;;; Try parser, return Just on success, Nothing on failure.
(define (option-maybe p)
  (parser-or (parser-map just p)
             (parser-pure nothing)))

;;; ============================================================
;;; Repetition
;;; ============================================================

;;; many : Parser a → Parser (List a)
;;; Zero or more occurrences.
(define (many p)
  (parser-or (some p) (parser-pure '())))

;;; some : Parser a → Parser (List a)
;;; One or more occurrences.
(define (some p)
  (parser-bind p (lambda (x)
                         (parser-bind (many p)
                                      (lambda (xs)
                                              (parser-pure (cons x xs)))))))

;;; count : Nat × Parser a → Parser (List a)
;;; Exactly n occurrences.
(define (count n p)
  (if (= n 0)
      (parser-pure '())
      (parser-bind p (lambda (x)
                             (parser-bind (count (- n 1) p)
                                          (lambda (xs)
                                                  (parser-pure (cons x xs))))))))

;;; between : Parser open × Parser close × Parser a → Parser a
;;; Parse between delimiters.
(define (between open close p)
  (parser-then open (parser-left p close)))

;;; sepBy : Parser a × Parser sep → Parser (List a)
;;; Zero or more, separated by separator.
(define (sep-by p sep)
  (parser-or (sep-by1 p sep) (parser-pure '())))

;;; sepBy1 : Parser a × Parser sep → Parser (List a)
;;; One or more, separated by separator.
(define (sep-by1 p sep)
  (parser-bind p (lambda (x)
                         (parser-bind (many (parser-then sep p))
                                      (lambda (xs)
                                              (parser-pure (cons x xs)))))))

;;; endBy : Parser a × Parser sep → Parser (List a)
;;; Zero or more, each followed by separator.
(define (end-by p sep)
  (many (parser-left p sep)))

;;; endBy1 : Parser a × Parser sep → Parser (List a)
;;; One or more, each followed by separator.
(define (end-by1 p sep)
  (some (parser-left p sep)))

;;; manyTill : Parser a × Parser end → Parser (List a)
;;; Parse until end parser succeeds.
(define (many-till p end)
  (parser-or (parser-then end (parser-pure '()))
             (parser-bind p (lambda (x)
                                    (parser-bind (many-till p end)
                                                 (lambda (xs)
                                                         (parser-pure (cons x xs))))))))

;;; ============================================================
;;; Lookahead
;;; ============================================================

;;; lookAhead : Parser a → Parser a
;;; Try parser without consuming input on success.
(define (look-ahead p)
  (make-parser
   (lambda (state)
           (let ([result (run-parser p state)])
                (if (right? result)
                    ;; Restore original state
                    (right (cons (car (from-right result)) state))
                    result)))))

;;; notFollowedBy : Parser a → Parser ()
;;; Succeed only if parser fails.
(define (not-followed-by p)
  (make-parser
   (lambda (state)
           (let ([result (run-parser p state)])
                (if (right? result)
                    (left (make-parse-error
                           (state-pos state)
                           "unexpected success"
                           '()))
                    (right (cons '() state)))))))

;;; ============================================================
;;; Error Handling
;;; ============================================================

;;; label : Parser a × String → Parser a
;;; Replace expected in error messages.
(define (label p description)
  (make-parser
   (lambda (state)
           (let ([result (run-parser p state)])
                (if (right? result)
                    result
                    (let ([err (from-left result)])
                         (left (make-parse-error
                                (error-pos err)
                                (error-message err)
                                (list description)))))))))

;;; <?> : Infix alias for label
(define parser-label label)

;;; ============================================================
;;; Convenience Combinators
;;; ============================================================

;;; spaces : Parser String
;;; Zero or more whitespace characters.
(define spaces
  (parser-map list->string (many space)))

;;; spaces1 : Parser String
;;; One or more whitespace characters.
(define spaces1
  (parser-map list->string (some space)))

;;; lexeme : Parser a → Parser a
;;; Parse and consume trailing whitespace.
(define (lexeme p)
  (parser-left p spaces))

;;; symbol : String → Parser String
;;; Parse string as lexeme.
(define (symbol str)
  (lexeme (string-parser str)))

;;; parens : Parser a → Parser a
;;; Parse between parentheses.
(define (parens p)
  (between (symbol "(") (symbol ")") p))

;;; braces : Parser a → Parser a
;;; Parse between braces.
(define (braces p)
  (between (symbol "{") (symbol "}") p))

;;; brackets : Parser a → Parser a
;;; Parse between brackets.
(define (brackets p)
  (between (symbol "[") (symbol "]") p))

;;; angles : Parser a → Parser a
;;; Parse between angle brackets.
(define (angles p)
  (between (symbol "<") (symbol ">") p))

;;; comma-sep : Parser a → Parser (List a)
;;; Comma-separated values.
(define (comma-sep p)
  (sep-by p (symbol ",")))

;;; semi-sep : Parser a → Parser (List a)
;;; Semicolon-separated values.
(define (semi-sep p)
  (sep-by p (symbol ";")))

;;; ============================================================
;;; Number Parsers
;;; ============================================================

;;; natural : Parser Nat
;;; Parse natural number.
(define natural
  (parser-bind (some digit)
               (lambda (digits)
                       (parser-pure (string->number (list->string digits))))))

;;; integer : Parser Int
;;; Parse integer (with optional sign).
(define integer
  (parser-bind (optional (one-of "+-") #\+)
               (lambda (sign)
                       (parser-bind natural
                                    (lambda (n)
                                            (parser-pure (if (char=? sign #\-)
                                                             (- n)
                                                             n)))))))

;;; decimal : Parser Number
;;; Parse decimal number.
(define decimal
  (parser-bind (optional (one-of "+-") #\+)
               (lambda (sign)
                       (parser-bind (some digit)
                                    (lambda (int-part)
                                            (parser-bind (optional (parser-then (char #\.)
                                                                                (some digit))
                                                                   '())
                                                         (lambda (frac-part)
                                                                 (let* ([int-str (list->string int-part)]
                                                                        [frac-str (if (null? frac-part)
                                                                                      ""
                                                                                      (string-append "." (list->string frac-part)))]
                                                                        [num-str (string-append int-str frac-str)]
                                                                        [num (string->number num-str)])
                                                                       (parser-pure (if (char=? sign #\-)
                                                                                        (- num)
                                                                                        num))))))))))

;;; ============================================================
;;; Identifier Parser
;;; ============================================================

;;; identifier : Parser String
;;; Parse identifier (letter followed by alphanumerics).
(define identifier
  (parser-bind (parser-or letter (char #\_))
               (lambda (first)
                       (parser-bind (many (parser-or alpha-num (char #\_)))
                                    (lambda (rest)
                                            (parser-pure (list->string (cons first rest))))))))

;;; keyword : String → Parser String
;;; Parse keyword (identifier matching specific string).
(define (keyword kw)
  (try (parser-bind identifier
                    (lambda (id)
                            (if (string=? id kw)
                                (parser-pure kw)
                                (parser-fail (string-append "expected keyword '" kw "'")))))))

;;; ============================================================
;;; Higher-Order Combinators
;;; ============================================================

;;; chainl1 : Parser a × Parser (a × a → a) → Parser a
;;; Parse left-associative binary operations.
;;; Parses: p (op p)*
;;; Associates: ((a op b) op c)
(define (chainl1 p op)
  (define (rest acc)
    (parser-or
     (parser-bind op
                  (lambda (f)
                          (parser-bind p
                                       (lambda (y)
                                               (rest (f acc y))))))
     (parser-pure acc)))
  (parser-bind p rest))

;;; chainl : Parser a × Parser (a × a → a) × a → Parser a
;;; Like chainl1, but returns default if no matches.
(define (chainl p op default)
  (parser-or (chainl1 p op) (parser-pure default)))

;;; chainr1 : Parser a × Parser (a × a → a) → Parser a
;;; Parse right-associative binary operations.
;;; Parses: p (op p)*
;;; Associates: (a op (b op c))
(define (chainr1 p op)
  (parser-bind p
               (lambda (x)
                       (parser-or
                        (parser-bind op
                                     (lambda (f)
                                             (parser-bind (chainr1 p op)
                                                          (lambda (y)
                                                                  (parser-pure (f x y))))))
                        (parser-pure x)))))

;;; chainr : Parser a × Parser (a × a → a) × a → Parser a
;;; Like chainr1, but returns default if no matches.
(define (chainr p op default)
  (parser-or (chainr1 p op) (parser-pure default)))

;;; skip-many : Parser a → Parser ()
;;; Apply parser zero or more times, discarding results.
(define (skip-many p)
  (parser-or (parser-bind p (lambda (_) (skip-many p)))
             (parser-pure '())))

;;; skip-some : Parser a → Parser ()
;;; Apply parser one or more times, discarding results.
(define (skip-some p)
  (parser-bind p (lambda (_) (skip-many p))))

;;; sep-end-by : Parser a × Parser sep → Parser (List a)
;;; Zero or more, separated and optionally ended by separator.
(define (sep-end-by p sep)
  (parser-or (sep-end-by1 p sep) (parser-pure '())))

;;; sep-end-by1 : Parser a × Parser sep → Parser (List a)
;;; One or more, separated and optionally ended by separator.
(define (sep-end-by1 p sep)
  (parser-bind p
               (lambda (x)
                       (parser-or
                        (parser-bind sep
                                     (lambda (_)
                                             (parser-bind (sep-end-by p sep)
                                                          (lambda (xs)
                                                                  (parser-pure (cons x xs))))))
                        (parser-pure (list x))))))

;;; many-accum : (a × b → b) × b × Parser a → Parser b
;;; Parse zero or more, accumulating with a function.
(define (many-accum f init p)
  (define (go acc)
    (parser-or
     (parser-bind p (lambda (x) (go (f x acc))))
     (parser-pure acc)))
  (go init))

;;; fold-p : (b × a → b) × b × Parser a → Parser b
;;; Left fold over parsed values.
(define (fold-p f init p)
  (many-accum (lambda (x acc) (f acc x)) init p))

;;; scan-p : (b × a → b) × b × Parser a → Parser (List b)
;;; Like fold-p but collects intermediate results.
(define (scan-p f init p)
  (parser-map reverse
              (many-accum (lambda (x acc)
                                  (let ([new-val (f (car acc) x)])
                                       (cons new-val acc)))
                          (list init) p)))

;;; until : Parser end × Parser a → Parser (List a)
;;; Parse until end succeeds, returning parsed values (not including end).
(define (until end p)
  (many-till p end))

;;; exactly : Nat × Parser a → Parser (List a)
;;; Alias for count.
(define exactly count)

;;; at-most : Nat × Parser a → Parser (List a)
;;; Parse at most n occurrences.
(define (at-most n p)
  (if (<= n 0)
      (parser-pure '())
      (parser-or
       (parser-bind p
                    (lambda (x)
                            (parser-bind (at-most (- n 1) p)
                                         (lambda (xs)
                                                 (parser-pure (cons x xs))))))
       (parser-pure '()))))

;;; at-least : Nat × Parser a → Parser (List a)
;;; Parse at least n occurrences.
(define (at-least n p)
  (parser-bind (count n p)
               (lambda (xs)
                       (parser-bind (many p)
                                    (lambda (ys)
                                            (parser-pure (append xs ys)))))))

;;; range-of : Nat × Nat × Parser a → Parser (List a)
;;; Parse between min and max occurrences.
(define (range-of min max p)
  (parser-bind (count min p)
               (lambda (xs)
                       (parser-bind (at-most (- max min) p)
                                    (lambda (ys)
                                            (parser-pure (append xs ys)))))))

;;; ============================================================
;;; Position Utilities
;;; ============================================================

;;; get-pos : Parser Pos
;;; Get current position.
(define get-pos
  (make-parser
   (lambda (state)
           (right (cons (state-pos state) state)))))

;;; get-input : Parser String
;;; Get remaining input.
(define get-input
  (make-parser
   (lambda (state)
           (right (cons (state-input state) state)))))

;;; with-pos : Parser a → Parser (a × Pos)
;;; Attach starting position to result.
(define (with-pos p)
  (parser-bind get-pos
               (lambda (pos)
                       (parser-bind p
                                    (lambda (val)
                                            (parser-pure (cons val pos)))))))

;;; with-span : Parser a → Parser (a × Pos × Pos)
;;; Attach start and end positions to result.
(define (with-span p)
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind p
                                    (lambda (val)
                                            (parser-bind get-pos
                                                         (lambda (end)
                                                                 (parser-pure (list val start end)))))))))

;;; ============================================================
;;; Debugging Utilities
;;; ============================================================

;;; trace-parser : String × Parser a → Parser a
;;; Print debug info when parser is invoked.
(define (trace-parser label p)
  (make-parser
   (lambda (state)
           (display "TRACE ")
           (display label)
           (display " at ")
           (display (pos-line (state-pos state)))
           (display ":")
           (display (pos-col (state-pos state)))
           (display " input='")
           (display (if (> (string-length (state-input state)) 20)
                        (string-append (substring (state-input state) 0 20) "...")
                        (state-input state)))
           (display "'")
           (newline)
           (let ([result (run-parser p state)])
                (display "TRACE ")
                (display label)
                (display " -> ")
                (if (right? result)
                    (display "SUCCESS")
                    (display "FAILURE"))
                (newline)
                result))))
