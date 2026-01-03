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

(load "core/base/prelude.ss")
(load "core/fp/meta/combinators.ss")

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
;;;   - input: the FULL input string (never copied/sliced)
;;;   - index: current parse position in the string
;;;   - pos: current position (line, column, offset) for error reporting
;;;
;;; OPTIMIZATION: Instead of using (substring s 1) to consume characters,
;;; which copies the remainder of the string (O(N) per character = O(N²) total),
;;; we track position with an index and use (string-ref s index) for O(1) access.

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

;;; make-state : String × Nat × Pos → State
;;; Create parser state with full input string, current index, and position.
(define (make-state input index pos)
  (list 'state input index pos))

;;; state? : Any → Boolean
(define (state? s)
  (and (pair? s) (eq? (car s) 'state)))

;;; state-input : State → String
;;; Returns the full input string (for internal use).
(define (state-input s) (list-ref s 1))

;;; state-index : State → Nat
;;; Returns the current parse position index.
(define (state-index s) (list-ref s 2))

;;; state-pos : State → Pos
(define (state-pos s) (list-ref s 3))

;;; state-remaining : State → String
;;; Returns the remaining unparsed input (for compatibility/debugging).
;;; Note: This creates a substring copy - use sparingly!
(define (state-remaining s)
  (let ([input (state-input s)]
        [index (state-index s)])
       (substring input index (string-length input))))

;;; state-at-end? : State → Boolean
;;; Check if we've reached the end of input.
(define (state-at-end? s)
  (>= (state-index s) (string-length (state-input s))))

;;; state-current-char : State → Char
;;; Get the current character (assumes not at end).
(define (state-current-char s)
  (string-ref (state-input s) (state-index s)))

;;; initial-state : String → State
(define (initial-state input)
  (make-state input 0 initial-pos))

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
           (if (state-at-end? state)
               (right (cons '() state))
               (left (make-parse-error
                      (state-pos state)
                      "expected end of input"
                      '("end of input")))))))

;;; any-char : Parser Char
;;; Parser that consumes any single character.
;;; O(1) per character - uses index-based access instead of substring copying.
(define any-char
  (make-parser
   (lambda (state)
           (if (state-at-end? state)
               (left (make-parse-error
                      (state-pos state)
                      "unexpected end of input"
                      '("any character")))
               (let* ([ch (state-current-char state)]
                      [new-pos (advance-pos (state-pos state) ch)]
                      [new-state (make-state (state-input state)
                                             (+ (state-index state) 1)
                                             new-pos)])
                     (right (cons ch new-state)))))))

;;; satisfy : (Char → Boolean) → String → Parser Char
;;; Parser that consumes char satisfying predicate.
;;; O(1) per character - uses index-based access instead of substring copying.
(define (satisfy pred description)
  (make-parser
   (lambda (state)
           (if (state-at-end? state)
               (left (make-parse-error
                      (state-pos state)
                      "unexpected end of input"
                      (list description)))
               (let ([ch (state-current-char state)])
                    (if (pred ch)
                        (let* ([new-pos (advance-pos (state-pos state) ch)]
                               [new-state (make-state (state-input state)
                                                      (+ (state-index state) 1)
                                                      new-pos)])
                              (right (cons ch new-state)))
                        (left (make-parse-error
                               (state-pos state)
                               (string-append "unexpected '" (string ch) "'")
                               (list description)))))))))

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
;;; O(len) where len is the target string length - no copying of input.
(define (string-parser str)
  (if (string=? str "")
      (parser-pure "")
      (make-parser
       (lambda (state)
               (let* ([input (state-input state)]
                      [index (state-index state)]
                      [len (string-length str)]
                      [input-len (string-length input)]
                      [remaining (- input-len index)])
                     (if (< remaining len)
                         (left (make-parse-error
                                (state-pos state)
                                "unexpected end of input"
                                (list (string-append "\"" str "\""))))
                         ;; Compare character by character without copying
                         (let loop ([i 0])
                              (if (= i len)
                                  ;; All characters matched
                                  (let* ([new-pos (fold-left
                                                   (lambda (p j)
                                                           (advance-pos p (string-ref str j)))
                                                   (state-pos state)
                                                   (iota len))]
                                         [new-state (make-state input (+ index len) new-pos)])
                                        (right (cons str new-state)))
                                  ;; Compare next character
                                  (if (char=? (string-ref input (+ index i))
                                              (string-ref str i))
                                      (loop (+ i 1))
                                      (left (make-parse-error
                                             (state-pos state)
                                             (string-append "expected \"" str "\"")
                                             (list (string-append "\"" str "\"")))))))))))))
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
;;; Try parsers in order (left to right).
(define (choice parsers)
  (if (null? parsers)
      (parser-fail "no alternatives")
      (fold-left parser-or (car parsers) (cdr parsers))))

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
;;; Detects and breaks infinite loops when parser succeeds without consuming input.
(define (many p)
  (make-parser
   (lambda (state)
           (let loop ([acc '()]
                      [current-state state])
                (let ([start-offset (pos-offset (state-pos current-state))]
                      [result (run-parser p current-state)])
                     (if (right? result)
                         (let* ([val-state (from-right result)]
                                [val (car val-state)]
                                [new-state (cdr val-state)]
                                [end-offset (pos-offset (state-pos new-state))])
                               ;; Check if any input was consumed
                               (if (= start-offset end-offset)
                                   ;; No input consumed - break to avoid infinite loop
                                   (right (cons (reverse acc) current-state))
                                   ;; Input consumed - continue
                                   (loop (cons val acc) new-state)))
                         ;; Parser failed - return accumulated results
                         (right (cons (reverse acc) current-state))))))))

;;; some : Parser a → Parser (List a)
;;; One or more occurrences.
;;; Detects and breaks infinite loops when parser succeeds without consuming input.
(define (some p)
  (make-parser
   (lambda (state)
           (let ([first-result (run-parser p state)])
                (if (left? first-result)
                    ;; First parse failed
                    first-result
                    (let* ([val-state (from-right first-result)]
                           [val (car val-state)]
                           [new-state (cdr val-state)])
                          ;; Now get the rest with many
                          (let ([rest-result (run-parser (many p) new-state)])
                               (if (right? rest-result)
                                   (let* ([rest-val-state (from-right rest-result)]
                                          [rest-vals (car rest-val-state)]
                                          [final-state (cdr rest-val-state)])
                                         (right (cons (cons val rest-vals) final-state)))
                                   rest-result))))))))

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
;;; Get remaining input (from current position to end).
;;; Note: This creates a substring copy - use sparingly in performance-critical code.
(define get-input
  (make-parser
   (lambda (state)
           (right (cons (state-remaining state) state)))))

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
           (let ([remaining (state-remaining state)])
                (display "TRACE ")
                (display label)
                (display " at ")
                (display (pos-line (state-pos state)))
                (display ":")
                (display (pos-col (state-pos state)))
                (display " input='")
                (display (if (> (string-length remaining) 20)
                             (string-append (substring remaining 0 20) "...")
                             remaining))
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
                     result)))))

;;; ============================================================
;;; Packrat Parsing (Memoization)
;;; ============================================================
;;;
;;; Packrat parsing memoizes parser results by (rule-key, position).
;;; This ensures O(n) parsing time for grammars that would otherwise
;;; have exponential backtracking.
;;;
;;; Usage:
;;;   1. Create a memo table: (make-memo-table) or (make-bounded-memo-table limit)
;;;   2. Wrap rules with memo: (memo 'rule-name parser)
;;;   3. Parse with the memo table: (parse-with-memo parser input table)
;;;
;;; Note: Memoization uses mutation internally but the interface
;;; remains pure from the caller's perspective.
;;;
;;; SECURITY: By default, memo tables are bounded to prevent memory exhaustion
;;; attacks via crafted inputs that create many unique parse states.

;;; Default maximum entries in memo table (prevents DoS via memory exhaustion)
(define *default-memo-table-limit* 50000)

;;; memo-table-entry : Result × Nat → Entry
;;; Create a memo table entry with result and access timestamp.
(define (make-memo-entry result timestamp)
  (cons result timestamp))

;;; memo-entry-result : Entry → Result
(define (memo-entry-result entry) (car entry))

;;; memo-entry-timestamp : Entry → Nat
(define (memo-entry-timestamp entry) (cdr entry))

;;; make-memo-table : () → MemoTable
;;; Create a new bounded memoization table with default limit.
;;; This is the safe default that prevents memory exhaustion attacks.
(define (make-memo-table)
  (make-bounded-memo-table *default-memo-table-limit*))

;;; make-bounded-memo-table : Nat → MemoTable
;;; Create a memoization table with specified maximum entry limit.
;;; When the limit is exceeded, oldest entries (by access time) are evicted.
(define (make-bounded-memo-table limit)
  (list 'bounded-memo-table
        (make-hashtable equal-hash equal?)  ; cache: key -> (result . timestamp)
        (box 0)                              ; counter: access timestamp
        (box limit)))                        ; max-entries

;;; make-unbounded-memo-table : () → MemoTable
;;; Create an unbounded memoization table.
;;; WARNING: Only use this for trusted inputs or when you have other
;;; safeguards against memory exhaustion attacks.
(define (make-unbounded-memo-table)
  (list 'unbounded-memo-table
        (make-hashtable equal-hash equal?)))

;;; bounded-memo-table? : MemoTable → Boolean
(define (bounded-memo-table? table)
  (and (pair? table) (eq? (car table) 'bounded-memo-table)))

;;; unbounded-memo-table? : MemoTable → Boolean
(define (unbounded-memo-table? table)
  (and (pair? table) (eq? (car table) 'unbounded-memo-table)))

;;; memo-table-cache : MemoTable → Hashtable
(define (memo-table-cache table)
  (cadr table))

;;; memo-table-counter : BoundedMemoTable → Box Nat
(define (memo-table-counter table)
  (caddr table))

;;; memo-table-limit : BoundedMemoTable → Box Nat
(define (memo-table-limit table)
  (cadddr table))

;;; memo-key : Symbol × Nat → MemoKey
;;; Create a memoization key from rule name and position.
(define (memo-key name offset)
  (cons name offset))

;;; next-timestamp! : BoundedMemoTable → Nat
;;; Get and increment the access timestamp.
(define (next-timestamp! table)
  (let* ([counter (memo-table-counter table)]
         [ts (unbox counter)])
        (set-box! counter (+ ts 1))
        ts))

;;; evict-random! : BoundedMemoTable × Nat → ()
;;; Evict random entries to make room for new ones.
;;; Uses O(k) random eviction instead of O(N log N) LRU sort.
;;; Removes approximately 10% of entries to amortize eviction cost.
(define (evict-random! table count-to-evict)
  (let* ([cache (memo-table-cache table)]
         [keys (hashtable-keys cache)]
         [n (vector-length keys)]
         [to-evict (min count-to-evict n)])
        ;; Remove random entries by selecting random indices
        (let loop ([remaining to-evict] [available n])
             (when (and (> remaining 0) (> available 0))
                   ;; Pick a random index in [0, available)
                   (let ([idx (random available)])
                        ;; Delete the key at that index
                        (hashtable-delete! cache (vector-ref keys idx))
                        ;; Swap with last element to maintain valid range
                        (vector-set! keys idx (vector-ref keys (- available 1)))
                        (loop (- remaining 1) (- available 1)))))))

;;; memo-lookup : MemoTable × Symbol × Nat → (Maybe Result)
;;; Look up a cached result. Updates access time for bounded tables.
(define (memo-lookup table name offset)
  (let* ([cache (memo-table-cache table)]
         [key (memo-key name offset)]
         [entry (hashtable-ref cache key 'not-found)])
        (if (eq? entry 'not-found)
            nothing
            (if (bounded-memo-table? table)
                ;; Update timestamp for LRU tracking
                (let ([ts (next-timestamp! table)])
                     (hashtable-set! cache key
                                     (make-memo-entry (memo-entry-result entry) ts))
                     (just (memo-entry-result entry)))
                ;; Unbounded: just return the result directly
                (just entry)))))

;;; memo-store! : MemoTable × Symbol × Nat × Result → ()
;;; Store a result in the cache. For bounded tables, evicts old entries if needed.
(define (memo-store! table name offset result)
  (if (bounded-memo-table? table)
      (let* ([cache (memo-table-cache table)]
             [limit (unbox (memo-table-limit table))]
             [current-size (hashtable-size cache)]
             [key (memo-key name offset)])
            ;; Check if we need to evict
            (when (>= current-size limit)
                  ;; Evict 10% of entries to amortize eviction cost
                  (evict-random! table (max 1 (quotient limit 10))))
            ;; Store with timestamp
            (let ([ts (next-timestamp! table)])
                 (hashtable-set! cache key (make-memo-entry result ts))))
      ;; Unbounded: just store directly
      (hashtable-set! (memo-table-cache table) (memo-key name offset) result)))

;;; memo : Symbol × Parser a → MemoTable → Parser a
;;; Create a memoizing parser. The memo table is passed at parse time.
;;; This allows the same parser definition to be reused with different tables.
(define (memo name parser)
  (lambda (table)
          (make-parser
           (lambda (state)
                   (let ([offset (pos-offset (state-pos state))])
                        (let ([cached (memo-lookup table name offset)])
                             (if (just? cached)
                                 ;; Cache hit - return cached result
                                 (from-just cached)
                                 ;; Cache miss - compute and store
                                 (let ([result (run-parser parser state)])
                                      (memo-store! table name offset result)
                                      result))))))))

;;; memo-ref : (MemoTable → Parser a) × MemoTable → Parser a
;;; Resolve a memoized parser with its table.
(define (memo-ref memo-parser table)
  (memo-parser table))

;;; parse-with-memo : (MemoTable → Parser a) × String × MemoTable → Either Error Value
;;; Parse using memoization.
(define (parse-with-memo memo-parser input table)
  (let* ([parser (memo-ref memo-parser table)]
         [result (run-parser parser (initial-state input))])
        (if (right? result)
            (right (car (from-right result)))
            result)))

;;; parse-packrat : (MemoTable → Parser a) × String → Either Error Value
;;; Parse with a fresh memo table (convenience function).
(define (parse-packrat memo-parser input)
  (parse-with-memo memo-parser input (make-memo-table)))

;;; ============================================================
;;; Packrat Combinators
;;; ============================================================
;;;
;;; These combinators work with memoized parsers.

;;; memo-bind : (MemoTable → Parser a) × (a → MemoTable → Parser b) → MemoTable → Parser b
;;; Monadic bind for memoized parsers.
(define (memo-bind mp f)
  (lambda (table)
          (parser-bind (memo-ref mp table)
                       (lambda (x) (memo-ref (f x) table)))))

;;; memo-then : (MemoTable → Parser a) × (MemoTable → Parser b) → MemoTable → Parser b
;;; Sequence memoized parsers, discarding first result.
(define (memo-then mp1 mp2)
  (lambda (table)
          (parser-then (memo-ref mp1 table) (memo-ref mp2 table))))

;;; memo-or : (MemoTable → Parser a) × (MemoTable → Parser a) → MemoTable → Parser a
;;; Try memoized parsers in order.
(define (memo-or mp1 mp2)
  (lambda (table)
          (parser-or (memo-ref mp1 table) (memo-ref mp2 table))))

;;; memo-pure : a → MemoTable → Parser a
;;; Lift a value into the memoized parser context.
(define (memo-pure x)
  (lambda (table) (parser-pure x)))

;;; memo-map : (a → b) → (MemoTable → Parser a) → MemoTable → Parser b
;;; Map over a memoized parser.
(define (memo-map f mp)
  (lambda (table)
          (parser-map f (memo-ref mp table))))

;;; memo-many : (MemoTable → Parser a) → MemoTable → Parser (List a)
;;; Zero or more of a memoized parser.
(define (memo-many mp)
  (lambda (table)
          (many (memo-ref mp table))))

;;; memo-some : (MemoTable → Parser a) → MemoTable → Parser (List a)
;;; One or more of a memoized parser.
(define (memo-some mp)
  (lambda (table)
          (some (memo-ref mp table))))

;;; lift-parser : Parser a → MemoTable → Parser a
;;; Lift a regular parser to work with memo combinators.
(define (lift-parser p)
  (lambda (table) p))

;;; ============================================================
;;; Packrat Statistics (for debugging)
;;; ============================================================

;;; memo-stats : MemoTable → (entries . limit)
;;; Get statistics about memo table usage.
;;; Returns (current-entries . max-limit) for bounded tables,
;;; or (current-entries . #f) for unbounded tables.
(define (memo-stats table)
  (let ([cache (memo-table-cache table)])
       (if (bounded-memo-table? table)
           (cons (hashtable-size cache)
                 (unbox (memo-table-limit table)))
           (cons (hashtable-size cache) #f))))

;;; memo-table-size : MemoTable → Nat
;;; Get the current number of entries in the memo table.
(define (memo-table-size table)
  (hashtable-size (memo-table-cache table)))

;;; memo-table-set-limit! : BoundedMemoTable × Nat → ()
;;; Change the limit of a bounded memo table.
;;; If new limit is smaller than current size, eviction happens on next store.
(define (memo-table-set-limit! table new-limit)
  (if (bounded-memo-table? table)
      (set-box! (memo-table-limit table) new-limit)
      (error 'memo-table-set-limit! "cannot set limit on unbounded table")))
