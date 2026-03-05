;;; lattice/fp/parsing/parser.ss — Parser Combinators
;;; @module parser
;;; @requires prelude combinators
;;; @purity mixed
;;; @stability stable

(require 'prelude)
(require 'combinators)

(doc 'module 'parser)
(doc 'description "Monadic Parser Combinators — A practical parser combinator library for building DSLs and parsers. Uses the Maybe and Either types from combinators.ss for results.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'features '(position-tracking primitive-parsers sequencing alternation repetition lookahead error-handling packrat indentation-sensitive))

(doc 'section 'character-constants)

(define %newline (integer->char 10))
(doc %newline 'type 'Char)

(define %tab (integer->char 9))
(doc %tab 'type 'Char)

(define %return (integer->char 13))
(doc %return 'type 'Char)

(define %backspace (integer->char 8))
(doc %backspace 'type 'Char)

(define %page (integer->char 12))
(doc %page 'type 'Char)

(doc 'section 'parser-state)

(define (make-pos line col offset)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat Pos))
  (doc 'description "Create parser position with line, column, offset")
  (list 'pos line col offset))

(define (pos? p)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? p) (eq? (car p) 'pos)))

(define (pos-line p)
  (doc 'export #t)
  (doc 'type '(-> Pos Nat))
  (list-ref p 1))

(define (pos-col p)
  (doc 'export #t)
  (doc 'type '(-> Pos Nat))
  (list-ref p 2))

(define (pos-offset p)
  (doc 'export #t)
  (doc 'type '(-> Pos Nat))
  (list-ref p 3))

(define parser-initial-pos (make-pos 1 1 0))
(doc parser-initial-pos 'export #t)
(doc parser-initial-pos 'type 'Pos)
(doc parser-initial-pos 'description "Initial parsing position (line 1, column 1, offset 0)")

(define (advance-pos pos ch)
  (doc 'export #t)
  (doc 'type '(-> Pos Char Pos))
  (doc 'description "Advance position by one character, handling newlines")
  (if (char=? ch %newline)
      (make-pos (+ (pos-line pos) 1) 1 (+ (pos-offset pos) 1))
      (make-pos (pos-line pos) (+ (pos-col pos) 1) (+ (pos-offset pos) 1))))

(define (parser-make-state input index pos)
  (doc 'export #t)
  (doc 'type '(-> String Nat Pos State))
  (doc 'description "Create parser state with full input string, current index, and position. Uses index-based access for O(1) character access instead of O(N) substring copying.")
  (list 'state input index pos))

(define (parser-state? s)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? s) (eq? (car s) 'state)))

(define (parser-state-input s)
  (doc 'export #t)
  (doc 'type '(-> State String))
  (doc 'description "Returns the full input string (for internal use)")
  (list-ref s 1))

(define (parser-state-index s)
  (doc 'export #t)
  (doc 'type '(-> State Nat))
  (doc 'description "Returns the current parse position index")
  (list-ref s 2))

(define (parser-state-pos s)
  (doc 'export #t)
  (doc 'type '(-> State Pos))
  (list-ref s 3))

(define (parser-state-remaining s)
  (doc 'export #t)
  (doc 'type '(-> State String))
  (doc 'description "Returns the remaining unparsed input (for compatibility/debugging)")
  (doc 'note "This creates a substring copy - use sparingly!")
  (let ([input (parser-state-input s)]
        [index (parser-state-index s)])
       (substring input index (string-length input))))

(define (parser-state-at-end? s)
  (doc 'export #t)
  (doc 'type '(-> State Boolean))
  (doc 'description "Check if we've reached the end of input")
  (>= (parser-state-index s) (string-length (parser-state-input s))))

(define (parser-state-current-char s)
  (doc 'export #t)
  (doc 'type '(-> State Char))
  (doc 'description "Get the current character (assumes not at end)")
  (string-ref (parser-state-input s) (parser-state-index s)))

(define (parser-initial-state input)
  (doc 'export #t)
  (doc 'type '(-> String State))
  (parser-make-state input 0 parser-initial-pos))

(doc 'section 'parse-error)

(define (make-parse-error pos message expected)
  (doc 'export #t)
  (doc 'type '(-> Pos String (List String) Error))
  (doc 'description "Create a parse error with position, message, and expected items")
  (list 'parse-error pos message expected))

(define (parse-error? e)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? e) (eq? (car e) 'parse-error)))

(define (error-pos e)
  (doc 'export #t)
  (doc 'type '(-> Error Pos))
  (list-ref e 1))

(define (error-message e)
  (doc 'export #t)
  (doc 'type '(-> Error String))
  (list-ref e 2))

(define (error-expected e)
  (doc 'export #t)
  (doc 'type '(-> Error (List String)))
  (list-ref e 3))

(define (merge-errors e1 e2)
  (doc 'export #t)
  (doc 'type '(-> Error Error Error))
  (doc 'description "Merge two errors, keeping the one at furthest position")
  (let ([p1 (pos-offset (error-pos e1))]
        [p2 (pos-offset (error-pos e2))])
       (cond
        [(> p1 p2) e1]
        [(< p1 p2) e2]
        [else (make-parse-error
               (error-pos e1)
               (error-message e1)
               (append (error-expected e1) (error-expected e2)))])))

(define (format-error err)
  (doc 'export #t)
  (doc 'type '(-> Error String))
  (doc 'description "Format parse error as human-readable string")
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

(define (format-expected exps)
  (doc 'export #t)
  (doc 'type '(-> (List String) String))
  (doc 'description "Format expected items list with 'or' separators")
  (cond
   [(null? exps) ""]
   [(null? (cdr exps)) (car exps)]
   [else (string-append (car exps) " or " (format-expected (cdr exps)))]))

(doc 'section 'parser-type)

(define (make-parser run-fn)
  (doc 'export #t)
  (doc 'type '(-> (-> State (Either Error (Pair α State))) (Parser α)))
  (doc 'description "Create a parser from a state transformer function. Parser type: State → Either Error (Value × State)")
  (list 'parser run-fn))

(define (parser? p)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (pair? p) (eq? (car p) 'parser)))

(define (run-parser parser state)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) State (Either Error (Pair α State))))
  (doc 'description "Run parser on state, returning Either Error (value, new-state)")
  ((cadr parser) state))

(define (parser-parse parser input)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) String (Either Error α)))
  (doc 'description "Run parser on input string, extracting final value")
  (let ([result (run-parser parser (parser-initial-state input))])
       (if (right? result)
           (right (car (from-right result)))  ; Extract value
           result)))  ; Return error

(define (parse-all parser input)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) String (Either Error α)))
  (doc 'description "Run parser and require complete input consumption")
  (let ([full-parser (parser-left parser parser-eof)])
       (parser-parse full-parser input)))

(doc 'section 'primitive-parsers)

(define (parser-pure x)
  (doc 'export #t)
  (doc 'type '(-> α (Parser α)))
  (doc 'description "Parser that succeeds with value without consuming input")
  (make-parser
   (lambda (state)
           (right (cons x state)))))

(define (parser-fail message)
  (doc 'export #t)
  (doc 'type '(-> String (Parser α)))
  (doc 'description "Parser that always fails with message")
  (make-parser
   (lambda (state)
           (left (make-parse-error (parser-state-pos state) message '())))))

(define parser-eof
  (make-parser
   (lambda (state)
           (if (parser-state-at-end? state)
               (right (cons '() state))
               (left (make-parse-error
                      (parser-state-pos state)
                      "expected end of input"
                      '("end of input")))))))
(doc parser-eof 'type '(Parser Unit))
(doc parser-eof 'description "Parser that succeeds only at end of input")

(define parser-any-char
  (make-parser
   (lambda (state)
           (if (parser-state-at-end? state)
               (left (make-parse-error
                      (parser-state-pos state)
                      "unexpected end of input"
                      '("any character")))
               (let* ([ch (parser-state-current-char state)]
                      [new-pos (advance-pos (parser-state-pos state) ch)]
                      [new-state (parser-make-state (parser-state-input state)
                                             (+ (parser-state-index state) 1)
                                             new-pos)])
                     (right (cons ch new-state)))))))
(doc parser-any-char 'type '(Parser Char))
(doc parser-any-char 'description "Parser that consumes any single character. O(1) per character - uses index-based access instead of substring copying.")

(define (parser-satisfy pred description)
  (doc 'export #t)
  (doc 'type '(-> (-> Char Bool) String (Parser Char)))
  (doc 'description "Parser that consumes char parser-satisfying predicate. O(1) per character - uses index-based access instead of substring copying.")
  (make-parser
   (lambda (state)
           (if (parser-state-at-end? state)
               (left (make-parse-error
                      (parser-state-pos state)
                      "unexpected end of input"
                      (list description)))
               (let ([ch (parser-state-current-char state)])
                    (if (pred ch)
                        (let* ([new-pos (advance-pos (parser-state-pos state) ch)]
                               [new-state (parser-make-state (parser-state-input state)
                                                      (+ (parser-state-index state) 1)
                                                      new-pos)])
                              (right (cons ch new-state)))
                        (left (make-parse-error
                               (parser-state-pos state)
                               (string-append "unexpected '" (string ch) "'")
                               (list description)))))))))

(define (parser-char c)
  (doc 'export #t)
  (doc 'type '(-> Char (Parser Char)))
  (doc 'description "Parser that matches specific character")
  (parser-satisfy (lambda (ch) (char=? ch c))
           (string-append "'" (string c) "'")))

(define (parser-char-ci c)
  (doc 'export #t)
  (doc 'type '(-> Char (Parser Char)))
  (doc 'description "Case-insensitive character match")
  (parser-satisfy (lambda (ch) (char-ci=? ch c))
           (string-append "'" (string c) "' (case-insensitive)")))

(define (parser-one-of chars)
  (doc 'export #t)
  (doc 'type '(-> String (Parser Char)))
  (doc 'description "Match any character in string")
  (parser-satisfy (lambda (ch)
                   (let loop ([i 0])
                        (if (>= i (string-length chars))
                            #f
                            (or (char=? ch (string-ref chars i))
                                (loop (+ i 1))))))
           (string-append "one of '" chars "'")))

(define (parser-none-of chars)
  (doc 'export #t)
  (doc 'type '(-> String (Parser Char)))
  (doc 'description "Match any character NOT in string")
  (parser-satisfy (lambda (ch)
                   (let loop ([i 0])
                        (if (>= i (string-length chars))
                            #t
                            (and (not (char=? ch (string-ref chars i)))
                                 (loop (+ i 1))))))
           (string-append "none of '" chars "'")))

(doc 'section 'character-class-parsers)

(define parser-digit (parser-satisfy char-numeric? "digit"))
(doc parser-digit 'export #t)
(doc parser-digit 'type '(Parser Char))
(doc parser-digit 'description "Parser that matches a digit character")

(define parser-letter (parser-satisfy char-alphabetic? "letter"))
(doc parser-letter 'export #t)
(doc parser-letter 'type '(Parser Char))
(doc parser-letter 'description "Parser that matches an alphabetic character")

(define parser-alpha-num
  (parser-satisfy (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))
           "alphanumeric"))
(doc parser-alpha-num 'type '(Parser Char))
(doc parser-alpha-num 'description "Parser that matches an alphanumeric character")

(define parser-space (parser-satisfy char-whitespace? "whitespace"))
(doc parser-space 'export #t)
(doc parser-space 'type '(Parser Char))
(doc parser-space 'description "Parser that matches a whitespace character")

(define parser-lower (parser-satisfy char-lower-case? "lowercase letter"))
(doc parser-lower 'export #t)
(doc parser-lower 'type '(Parser Char))
(doc parser-lower 'description "Parser that matches a lowercase letter")

(define parser-upper (parser-satisfy char-upper-case? "uppercase letter"))
(doc parser-upper 'export #t)
(doc parser-upper 'type '(Parser Char))
(doc parser-upper 'description "Parser that matches an uppercase letter")

(define newline-char (parser-char %newline))
(doc newline-char 'export #t)
(doc newline-char 'type '(Parser Char))
(doc newline-char 'description "Parser that matches a newline character")

(define tab-char (parser-char %tab))
(doc tab-char 'export #t)
(doc tab-char 'type '(Parser Char))
(doc tab-char 'description "Parser that matches a tab character")

(doc 'section 'parser-strings)

(define (parser-string str)
  (doc 'export #t)
  (doc 'type '(-> String (Parser String)))
  (doc 'description "Match exact string. O(len) where len is the target string length - no copying of input.")
  (if (string=? str "")
      (parser-pure "")
      (make-parser
       (lambda (state)
               (let* ([input (parser-state-input state)]
                      [index (parser-state-index state)]
                      [len (string-length str)]
                      [input-len (string-length input)]
                      [remaining (- input-len index)])
                     (if (< remaining len)
                         (left (make-parse-error
                                (parser-state-pos state)
                                "unexpected end of input"
                                (list (string-append "\"" str "\""))))
                         ;; Compare character by character without copying
                         (let loop ([i 0])
                              (if (= i len)
                                  ;; All characters matched
                                  (let* ([new-pos (fold-left
                                                   (lambda (p j)
                                                           (advance-pos p (string-ref str j)))
                                                   (parser-state-pos state)
                                                   (iota len))]
                                         [new-state (parser-make-state input (+ index len) new-pos)])
                                        (right (cons str new-state)))
                                  ;; Compare next character
                                  (if (char=? (string-ref input (+ index i))
                                              (string-ref str i))
                                      (loop (+ i 1))
                                      (left (make-parse-error
                                             (parser-state-pos state)
                                             (string-append "expected \"" str "\"")
                                             (list (string-append "\"" str "\"")))))))))))))
(doc 'section 'monad-operations)

(define (parser-bind p f)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (-> α (Parser β)) (Parser β)))
  (doc 'description "Monadic bind (>>=) for parsers")
  (make-parser
   (lambda (state)
           (let ([result (run-parser p state)])
                (if (left? result)
                    result
                    (let* ([val-state (from-right result)]
                           [val (car val-state)]
                           [new-state (cdr val-state)])
                          (run-parser (f val) new-state)))))))

(define (parser-then p1 p2)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser β) (Parser β)))
  (doc 'description "Sequence, discarding first result (>>)")
  (parser-bind p1 (lambda (_) p2)))

(define (parser-left p1 p2)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser β) (Parser α)))
  (doc 'description "Sequence, discarding second result (<*)")
  (parser-bind p1 (lambda (x)
                          (parser-bind p2 (lambda (_)
                                                  (parser-pure x))))))

(define parser-right parser-then)
(doc parser-right 'export #t)
(doc parser-right 'type '(-> (Parser α) (Parser β) (Parser β)))
(doc parser-right 'description "Sequence, discarding first result (*>). Same as parser-then")

(define (parser-map f p)
  (doc 'export #t)
  (doc 'type '(-> (-> α β) (Parser α) (Parser β)))
  (doc 'description "Functor map for parsers")
  (parser-bind p (lambda (x) (parser-pure (f x)))))

(define (parser-ap pf pa)
  (doc 'export #t)
  (doc 'type '(-> (Parser (-> α β)) (Parser α) (Parser β)))
  (doc 'description "Applicative apply for parsers")
  (parser-bind pf (lambda (f)
                          (parser-bind pa (lambda (a)
                                                  (parser-pure (f a)))))))

(doc 'section 'alternation)

(define (parser-or p1 p2)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser α) (Parser α)))
  (doc 'description "Try first parser, if it fails without consuming input, try second (<|>)")
  (make-parser
   (lambda (state)
           (let ([result1 (run-parser p1 state)])
                (if (right? result1)
                    result1
                    ;; Check if any input was consumed
                    (let ([err1 (from-left result1)]
                          [start-offset (pos-offset (parser-state-pos state))]
                          [err-offset (pos-offset (error-pos (from-left result1)))])
                         (if (> err-offset start-offset)
                             ;; Input consumed, don't try alternative
                             result1
                             ;; No input consumed, try p2
                             (let ([result2 (run-parser p2 state)])
                                  (if (right? result2)
                                      result2
                                      (left (merge-errors err1 (from-left result2))))))))))))

(define (parser-choice parsers)
  (doc 'export #t)
  (doc 'type '(-> (List (Parser α)) (Parser α)))
  (doc 'description "Try parsers in order (left to right)")
  (if (null? parsers)
      (parser-fail "no alternatives")
      (fold-left parser-or (car parsers) (cdr parsers))))

(define (parser-try p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser α)))
  (doc 'description "Try parser, on failure pretend no input was consumed")
  (make-parser
   (lambda (state)
           (let ([result (run-parser p state)])
                (if (right? result)
                    result
                    ;; Reset error position to start
                    (left (make-parse-error
                           (parser-state-pos state)
                           (error-message (from-left result))
                           (error-expected (from-left result)))))))))

(define (parser-optional p default)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) α (Parser α)))
  (doc 'description "Try parser, return default on failure")
  (parser-or p (parser-pure default)))

(define (parser-option-maybe p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser (Maybe α))))
  (doc 'description "Try parser, return Just on success, Nothing on failure")
  (parser-or (parser-map just p)
             (parser-pure nothing)))

(doc 'section 'repetition)

(define (parser-many p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser (List α))))
  (doc 'description "Zero or more occurrences. Detects and breaks infinite loops when parser succeeds without consuming input.")
  (make-parser
   (lambda (state)
           (let loop ([acc '()]
                      [current-state state])
                (let ([start-offset (pos-offset (parser-state-pos current-state))]
                      [result (run-parser p current-state)])
                     (if (right? result)
                         (let* ([val-state (from-right result)]
                                [val (car val-state)]
                                [new-state (cdr val-state)]
                                [end-offset (pos-offset (parser-state-pos new-state))])
                               ;; Check if any input was consumed
                               (if (= start-offset end-offset)
                                   ;; No input consumed - break to avoid infinite loop
                                   (right (cons (reverse acc) current-state))
                                   ;; Input consumed - continue
                                   (loop (cons val acc) new-state)))
                         ;; Parser failed - return accumulated results
                         (right (cons (reverse acc) current-state))))))))

(define (parser-some p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser (List α))))
  (doc 'description "One or more occurrences. Detects and breaks infinite loops when parser succeeds without consuming input.")
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
                          (let ([rest-result (run-parser (parser-many p) new-state)])
                               (if (right? rest-result)
                                   (let* ([rest-val-state (from-right rest-result)]
                                          [rest-vals (car rest-val-state)]
                                          [final-state (cdr rest-val-state)])
                                         (right (cons (cons val rest-vals) final-state)))
                                   rest-result))))))))

(define (parser-count n p)
  (doc 'export #t)
  (doc 'type '(-> Nat (Parser α) (Parser (List α))))
  (doc 'description "Exactly n occurrences")
  (if (= n 0)
      (parser-pure '())
      (parser-bind p (lambda (x)
                             (parser-bind (parser-count (- n 1) p)
                                          (lambda (xs)
                                                  (parser-pure (cons x xs))))))))

(define (parser-between open close p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser β) (Parser γ) (Parser γ)))
  (doc 'description "Parse between delimiters")
  (parser-then open (parser-left p close)))

(define (parser-sep-by p sep)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser β) (Parser (List α))))
  (doc 'description "Zero or more, separated by separator")
  (parser-or (parser-sep-by1 p sep) (parser-pure '())))

(define (parser-sep-by1 p sep)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser β) (Parser (List α))))
  (doc 'description "One or more, separated by separator")
  (parser-bind p (lambda (x)
                         (parser-bind (parser-many (parser-then sep p))
                                      (lambda (xs)
                                              (parser-pure (cons x xs)))))))

(define (parser-end-by p sep)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser β) (Parser (List α))))
  (doc 'description "Zero or more, each followed by separator")
  (parser-many (parser-left p sep)))

(define (parser-end-by1 p sep)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser β) (Parser (List α))))
  (doc 'description "One or more, each followed by separator")
  (parser-some (parser-left p sep)))

(define (parser-many-till p end)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser β) (Parser (List α))))
  (doc 'description "Parse until end parser succeeds. Detects and breaks infinite loops when body parser succeeds without consuming input.")
  (make-parser
   (lambda (state)
           (let loop ([acc '()]
                      [current-state state])
                ;; First try end parser
                (let ([end-result (run-parser end current-state)])
                     (if (right? end-result)
                         ;; End succeeded - return accumulated results
                         (right (cons (reverse acc) (cdr (from-right end-result))))
                         ;; End failed - try body parser
                         (let ([start-offset (pos-offset (parser-state-pos current-state))]
                               [body-result (run-parser p current-state)])
                              (if (right? body-result)
                                  (let* ([val-state (from-right body-result)]
                                         [val (car val-state)]
                                         [new-state (cdr val-state)]
                                         [end-offset (pos-offset (parser-state-pos new-state))])
                                        ;; Check if any input was consumed
                                        (if (= start-offset end-offset)
                                            ;; No input consumed - infinite loop detected
                                            (left (make-parse-error
                                                   (parser-state-pos current-state)
                                                   "parser-many-till: body parser succeeded without consuming input (infinite loop)"
                                                   '()))
                                            ;; Input consumed - continue
                                            (loop (cons val acc) new-state)))
                                  ;; Body parser failed - propagate error
                                  body-result))))))))

(doc 'section 'lookahead)

(define (parser-look-ahead p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser α)))
  (doc 'description "Try parser without consuming input on success")
  (make-parser
   (lambda (state)
           (let ([result (run-parser p state)])
                (if (right? result)
                    ;; Restore original state
                    (right (cons (car (from-right result)) state))
                    result)))))

(define (parser-not-followed-by p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser Unit)))
  (doc 'description "Succeed only if parser fails")
  (make-parser
   (lambda (state)
           (let ([result (run-parser p state)])
                (if (right? result)
                    (left (make-parse-error
                           (parser-state-pos state)
                           "unexpected success"
                           '()))
                    (right (cons '() state)))))))

(doc 'section 'error-handling)

(define (parser-label p description)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) String (Parser α)))
  (doc 'description "Replace expected in error messages")
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


(doc 'section 'convenience-combinators)

(define parser-spaces (parser-map list->string (parser-many parser-space)))
(doc parser-spaces 'export #t)
(doc parser-spaces 'type '(Parser String))
(doc parser-spaces 'description "Zero or more whitespace characters")

(define parser-spaces1 (parser-map list->string (parser-some parser-space)))
(doc parser-spaces1 'export #t)
(doc parser-spaces1 'type '(Parser String))
(doc parser-spaces1 'description "One or more whitespace characters")

(define (parser-lexeme p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser α)))
  (doc 'description "Parse and consume trailing whitespace")
  (parser-left p parser-spaces))

(define (parser-symbol str)
  (doc 'export #t)
  (doc 'type '(-> String (Parser String)))
  (doc 'description "Parse string as lexeme")
  (parser-lexeme (parser-string str)))

(define (parser-parens p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser α)))
  (doc 'description "Parse between parentheses")
  (parser-between (parser-symbol "(") (parser-symbol ")") p))

(define (parser-braces p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser α)))
  (doc 'description "Parse between braces")
  (parser-between (parser-symbol "{") (parser-symbol "}") p))

(define (parser-brackets p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser α)))
  (doc 'description "Parse between brackets")
  (parser-between (parser-symbol "[") (parser-symbol "]") p))

(define (parser-angles p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser α)))
  (doc 'description "Parse between angle brackets")
  (parser-between (parser-symbol "<") (parser-symbol ">") p))

(define (comma-sep p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser (List α))))
  (doc 'description "Comma-separated values")
  (parser-sep-by p (parser-symbol ",")))

(define (semi-sep p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser (List α))))
  (doc 'description "Semicolon-separated values")
  (parser-sep-by p (parser-symbol ";")))

(doc 'section 'number-parsers)

(define parser-natural
  (parser-bind (parser-some parser-digit)
               (lambda (digits)
                       (parser-pure (string->number (list->string digits))))))
(doc parser-natural 'type '(Parser Nat))
(doc parser-natural 'description "Parse natural number")

(define parser-integer
  (parser-bind (parser-optional (parser-one-of "+-") #\+)
               (lambda (sign)
                       (parser-bind parser-natural
                                    (lambda (n)
                                            (parser-pure (if (char=? sign #\-)
                                                             (- n)
                                                             n)))))))
(doc parser-integer 'type '(Parser Int))
(doc parser-integer 'description "Parse integer (with optional sign)")

(define decimal
  (parser-bind (parser-optional (parser-one-of "+-") #\+)
               (lambda (sign)
                       (parser-bind (parser-some parser-digit)
                                    (lambda (int-part)
                                            (parser-bind (parser-optional (parser-try (parser-then (parser-char #\.)
                                                                                     (parser-some parser-digit)))
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
(doc decimal 'type '(Parser Number))
(doc decimal 'description "Parse decimal number")

(doc 'section 'identifier-parser)

(define parser-identifier
  (parser-bind (parser-or parser-letter (parser-char #\_))
               (lambda (first)
                       (parser-bind (parser-many (parser-or parser-alpha-num (parser-char #\_)))
                                    (lambda (rest)
                                            (parser-pure (list->string (cons first rest))))))))
(doc parser-identifier 'type '(Parser String))
(doc parser-identifier 'description "Parse identifier (letter followed by alphanumerics)")

(define (parser-keyword kw)
  (doc 'export #t)
  (doc 'type '(-> String (Parser String)))
  (doc 'description "Parse keyword (identifier matching specific string)")
  (parser-try (parser-bind parser-identifier
                    (lambda (id)
                            (if (string=? id kw)
                                (parser-pure kw)
                                (parser-fail (string-append "expected keyword '" kw "'")))))))

(doc 'section 'higher-order-combinators)

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
(doc chainl1 'type '(-> (Parser α) (Parser (-> α α α)) (Parser α)))
(doc chainl1 'description "Parse left-associative binary operations. Parses: p (op p)*, Associates: ((a op b) op c)")

(define (chainl p op default)
  (doc 'export #t)
  (parser-or (chainl1 p op) (parser-pure default)))
(doc chainl 'type '(-> (Parser α) (Parser (-> α α α)) α (Parser α)))
(doc chainl 'description "Like chainl1, but returns default if no matches")

(define (chainr1 p op)
  (doc 'export #t)
  (parser-bind p
               (lambda (x)
                       (parser-or
                        (parser-bind op
                                     (lambda (f)
                                             (parser-bind (chainr1 p op)
                                                          (lambda (y)
                                                                  (parser-pure (f x y))))))
                        (parser-pure x)))))
(doc chainr1 'type '(-> (Parser α) (Parser (-> α α α)) (Parser α)))
(doc chainr1 'description "Parse right-associative binary operations. Parses: p (op p)*, Associates: (a op (b op c))")

(define (chainr p op default)
  (doc 'export #t)
  (parser-or (chainr1 p op) (parser-pure default)))
(doc chainr 'type '(-> (Parser α) (Parser (-> α α α)) α (Parser α)))
(doc chainr 'description "Like chainr1, but returns default if no matches")

(define (skip-many p)
  (doc 'export #t)
  (parser-or (parser-bind p (lambda (_) (skip-many p)))
             (parser-pure '())))
(doc skip-many 'type '(-> (Parser α) (Parser Unit)))
(doc skip-many 'description "Apply parser zero or more times, discarding results")

(define (skip-some p)
  (doc 'export #t)
  (parser-bind p (lambda (_) (skip-many p))))
(doc skip-some 'type '(-> (Parser α) (Parser Unit)))
(doc skip-some 'description "Apply parser one or more times, discarding results")

;;; sep-end-by : (Parser α) × (Parser β) → (Parser (List α))
;;; Zero or more, separated and optionally ended by separator.
(define (sep-end-by p sep)
  (doc 'export #t)
  (parser-or (sep-end-by1 p sep) (parser-pure '())))

;;; sep-end-by1 : (Parser α) × (Parser β) → (Parser (List α))
;;; One or more, separated and optionally ended by separator.
(define (sep-end-by1 p sep)
  (doc 'export #t)
  (parser-bind p
               (lambda (x)
                       (parser-or
                        (parser-bind sep
                                     (lambda (_)
                                             (parser-bind (sep-end-by p sep)
                                                          (lambda (xs)
                                                                  (parser-pure (cons x xs))))))
                        (parser-pure (list x))))))

;;; many-accum : (α × β → β) × β × (Parser α) → (Parser β)
;;; Parse zero or more, accumulating with a function.
(define (many-accum f init p)
  (define (go acc)
    (parser-or
     (parser-bind p (lambda (x) (go (f x acc))))
     (parser-pure acc)))
  (go init))

;;; fold-p : (β × α → β) × β × (Parser α) → (Parser β)
;;; Left fold over parsed values.
(define (fold-p f init p)
  (doc 'export #t)
  (many-accum (lambda (x acc) (f acc x)) init p))

;;; scan-p : (β × α → β) × β × (Parser α) → (Parser (List β))
;;; Like fold-p but collects intermediate results.
(define (scan-p f init p)
  (doc 'export #t)
  (parser-map reverse
              (many-accum (lambda (x acc)
                                  (let ([new-val (f (car acc) x)])
                                       (cons new-val acc)))
                          (list init) p)))

;;; until : (Parser α) × (Parser β) → (Parser (List β))
;;; Parse until end succeeds, returning parsed values (not including end).
(define (until end p)
  (doc 'export #t)
  (parser-many-till p end))

;;; exactly : Nat × (Parser α) → (Parser (List α))
;;; Alias for count.
(define exactly parser-count)

;;; at-most : Nat × (Parser α) → (Parser (List α))
;;; Parse at most n occurrences.
(define (at-most n p)
  (doc 'export #t)
  (if (<= n 0)
      (parser-pure '())
      (parser-or
       (parser-try (parser-bind p
                         (lambda (x)
                                 (parser-bind (at-most (- n 1) p)
                                              (lambda (xs)
                                                      (parser-pure (cons x xs)))))))
       (parser-pure '()))))

;;; at-least : Nat × (Parser α) → (Parser (List α))
;;; Parse at least n occurrences.
(define (at-least n p)
  (doc 'export #t)
  (parser-bind (parser-count n p)
               (lambda (xs)
                       (parser-bind (parser-many p)
                                    (lambda (ys)
                                            (parser-pure (append xs ys)))))))

;;; range-of : Nat × Nat × (Parser α) → (Parser (List α))
;;; Parse parser-between min and max occurrences.
(define (range-of min max p)
  (doc 'export #t)
  (parser-bind (parser-count min p)
               (lambda (xs)
                       (parser-bind (at-most (- max min) p)
                                    (lambda (ys)
                                            (parser-pure (append xs ys)))))))

(doc 'section 'position-utilities)

(define get-pos
  (make-parser
   (lambda (state)
           (right (cons (parser-state-pos state) state)))))
(doc get-pos 'type '(Parser Pos))
(doc get-pos 'description "Get current position")

(define get-input
  (make-parser
   (lambda (state)
           (right (cons (parser-state-remaining state) state)))))
(doc get-input 'type '(Parser String))
(doc get-input 'description "Get remaining input (from current position to end)")
(doc get-input 'note "This creates a substring copy - use sparingly in performance-critical code")

(define (with-pos p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser (Pair α Pos))))
  (doc 'description "Attach starting position to result")
  (parser-bind get-pos
               (lambda (pos)
                       (parser-bind p
                                    (lambda (val)
                                            (parser-pure (cons val pos)))))))

(define (parser-with-span p)
  (doc 'export #t)
  (doc 'type '(-> (Parser α) (Parser (List α))))
  (doc 'description "Attach start and end positions to result (as list: value, start-pos, end-pos)")
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind p
                                    (lambda (val)
                                            (parser-bind get-pos
                                                         (lambda (end)
                                                                 (parser-pure (list val start end)))))))))

(doc 'section 'debugging-utilities)

(define (trace-parser label p)
  (doc 'export #t)
  (doc 'type '(-> String (Parser α) (Parser α)))
  (doc 'description "Print debug info when parser is invoked")
  (make-parser
   (lambda (state)
           (let ([remaining (parser-state-remaining state)])
                (display "TRACE ")
                (display label)
                (display " at ")
                (display (pos-line (parser-state-pos state)))
                (display ":")
                (display (pos-col (parser-state-pos state)))
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

(doc 'section 'indentation-sensitive-parsing)

(define get-column
  (make-parser
   (lambda (state)
           (right (cons (pos-col (parser-state-pos state)) state)))))
(doc get-column 'type '(Parser Nat))
(doc get-column 'description "Get the current column number (1-based)")

(define get-line
  (make-parser
   (lambda (state)
           (right (cons (pos-line (parser-state-pos state)) state)))))
(doc get-line 'type '(Parser Nat))
(doc get-line 'description "Get the current line number (1-based)")

(define (at-column n p)
  (doc 'export #t)
  (doc 'type '(-> Nat (Parser α) (Parser α)))
  (doc 'description "Run parser only if at exactly column n")
  (parser-bind get-column
               (lambda (col)
                       (if (= col n)
                           p
                           (parser-fail
                            (string-append "expected column "
                                           (number->string n)
                                           ", at column "
                                           (number->string col)))))))

;;; column-gt : Nat × (Parser α) → (Parser α)
;;; Run parser only if current column > reference.
(define (column-gt ref p)
  (doc 'export #t)
  (parser-bind get-column
               (lambda (col)
                       (if (> col ref)
                           p
                           (parser-fail
                            (string-append "expected column > "
                                           (number->string ref)
                                           ", at column "
                                           (number->string col)))))))

;;; column-ge : Nat × (Parser α) → (Parser α)
;;; Run parser only if current column >= reference.
(define (column-ge ref p)
  (doc 'export #t)
  (parser-bind get-column
               (lambda (col)
                       (if (>= col ref)
                           p
                           (parser-fail
                            (string-append "expected column >= "
                                           (number->string ref)
                                           ", at column "
                                           (number->string col)))))))

;;; column-eq : Nat × (Parser α) → (Parser α)
;;; Run parser only if current column = reference.
(define (column-eq ref p)
  (doc 'export #t)
  (at-column ref p))

;;; same-line : (Parser α) → (Parser α)
;;; Run parser, failing if it crosses a newline.
(define (same-line p)
  (doc 'export #t)
  (make-parser
   (lambda (state)
           (let* ([start-line (pos-line (parser-state-pos state))]
                  [result (run-parser p state)])
                 (if (right? result)
                     (let* ([pair (from-right result)]
                            [end-line (pos-line (parser-state-pos (cdr pair)))])
                           (if (= start-line end-line)
                               result
                               (left (make-parse-error
                                      (parser-state-pos state)
                                      "unexpected newline in same-line parser"
                                      '()))))
                     result)))))

;;; newline-parser : Parser Unit
;;; Parse a newline character.
(define newline-parser
  (parser-map (lambda (_) '())
              (parser-char #\newline)))

;;; indent-spaces : Parser Nat
;;; Parse zero or more spaces/tabs at start of content, return column after.
;;; Use this after a newline to get the indentation level.
(define indent-spaces
  (parser-then (parser-many (parser-one-of " \t"))
               get-column))

;;; newline-indent : Parser Nat
;;; Parse newline followed by indentation, return the new column.
(define newline-indent
  (parser-then newline-parser indent-spaces))

;;; blank-line : Parser Unit
;;; Parse a blank line (optional whitespace followed by newline).
(define blank-line
  (parser-map (lambda (_) '())
              (parser-then (parser-many (parser-one-of " \t"))
                           newline-parser)))

;;; skip-blank-lines : Parser Unit
;;; Skip zero or more blank lines.
(define skip-blank-lines
  (parser-map (lambda (_) '())
              (parser-many blank-line)))

;;; indented : (Parser α) → (Parser α)
;;; Run parser at current position; the parsed content must start at
;;; a column greater than 0 (i.e., be indented from column 1).
(define (parser-indented p)
  (doc 'export #t)
  (column-gt 1 p))

;;; indented-from : Nat × (Parser α) → (Parser α)
;;; Run parser requiring column > ref.
(define (indented-from ref p)
  (doc 'export #t)
  (column-gt ref p))

;;; aligned : Nat × (Parser α) → (Parser α)
;;; Run parser requiring column = ref.
(define (aligned ref p)
  (doc 'export #t)
  (at-column ref p))

;;; indented-block : (Parser α) → (Parser (List α))
;;; Parse an indented block: one or more items at same or greater indentation.
;;; The first item establishes the reference column. Subsequent items must be
;;; at the same column or greater (for continuation lines).
(define (indented-block item-parser)
  (doc 'export #t)
  (parser-bind get-column
               (lambda (ref-col)
                       (parser-bind item-parser
                                    (lambda (first)
                                            (parser-bind
                                             (parser-many (parser-bind newline-indent
                                                                (lambda (col)
                                                                        (if (>= col ref-col)
                                                                            item-parser
                                                                            (parser-fail "dedent")))))
                                             (lambda (rest)
                                                     (parser-pure (cons first rest)))))))))

;;; indented-block-strict : (Parser α) → (Parser (List α))
;;; Parse a block where items must be at exactly the same column.
;;; More strict than indented-block.
(define (indented-block-strict item-parser)
  (doc 'export #t)
  (parser-bind get-column
               (lambda (ref-col)
                       (parser-bind item-parser
                                    (lambda (first)
                                            (parser-bind
                                             (parser-many (parser-bind newline-indent
                                                                (lambda (col)
                                                                        (if (= col ref-col)
                                                                            item-parser
                                                                            (parser-fail "column mismatch")))))
                                             (lambda (rest)
                                                     (parser-pure (cons first rest)))))))))

;;; next-line : (Parser α) → (Parser α)
;;; Expect newline, skip blank lines, then parse at next non-blank line.
(define (next-line p)
  (doc 'export #t)
  (parser-then newline-parser
               (parser-then skip-blank-lines
                            (parser-then indent-spaces p))))

;;; offside : Nat × (Parser α) → (Parser α)
;;; Run parser, but fail if it ever reaches a column <= ref (offside rule).
;;; This is a simplified version - just checks starting column.
(define (offside ref p)
  (doc 'export #t)
  (column-gt ref p))

(doc 'section 'packrat-parsing)

(define *default-memo-table-limit* 50000)
(doc *default-memo-table-limit* 'export #t)
(doc *default-memo-table-limit* 'type 'Nat)
(doc *default-memo-table-limit* 'description "Default maximum entries in memo table (prevents DoS via memory exhaustion)")

;;; make-memo-entry : α × Nat → (Pair α Nat)
;;; Create a memo table entry with result and access timestamp.
(define (make-memo-entry result timestamp)
  (doc 'export #t)
  (cons result timestamp))

;;; memo-entry-result : (α × Nat) → α
(define (memo-entry-result entry) (car entry))

;;; memo-entry-timestamp : (α × Nat) → Nat
(define (memo-entry-timestamp entry) (cdr entry))

;;; make-memo-table : Unit → MemoTable
;;; Create a new bounded memoization table with default limit.
;;; This is the safe default that prevents memory exhaustion attacks.
(define (make-memo-table)
  (doc 'export #t)
  (make-bounded-memo-table *default-memo-table-limit*))

;;; make-bounded-memo-table : Nat → MemoTable
;;; Create a memoization table with specified maximum entry limit.
;;; When the limit is exceeded, oldest entries (by access time) are evicted.
(define (make-bounded-memo-table limit)
  (doc 'export #t)
  (list 'bounded-memo-table
        (make-hashtable equal-hash equal?)  ; cache: key -> (result . timestamp)
        (box 0)                              ; counter: access timestamp
        (box limit)))                        ; max-entries

;;; make-unbounded-memo-table : Unit → MemoTable
;;; Create an unbounded memoization table.
;;; WARNING: Only use this for trusted inputs or when you have other
;;; safeguards against memory exhaustion attacks.
(define (make-unbounded-memo-table)
  (doc 'export #t)
  (list 'unbounded-memo-table
        (make-hashtable equal-hash equal?)))

;;; bounded-memo-table? : MemoTable → Boolean
(define (bounded-memo-table? table)
  (doc 'export #t)
  (and (pair? table) (eq? (car table) 'bounded-memo-table)))

;;; unbounded-memo-table? : MemoTable → Boolean
(define (unbounded-memo-table? table)
  (doc 'export #t)
  (and (pair? table) (eq? (car table) 'unbounded-memo-table)))

;;; memo-table-cache : MemoTable → Hashtable
(define (memo-table-cache table)
  (doc 'export #t)
  (cadr table))

;;; memo-table-counter : MemoTable → (Box Nat)
(define (memo-table-counter table)
  (doc 'export #t)
  (caddr table))

;;; memo-table-limit : MemoTable → (Box Nat)
(define (memo-table-limit table)
  (doc 'export #t)
  (cadddr table))

;;; memo-key : Symbol × Nat → (Symbol × Nat)
;;; Create a memoization key from rule name and position.
(define (memo-key name offset)
  (doc 'export #t)
  (cons name offset))

;;; next-timestamp! : MemoTable → Nat
;;; Get and increment the access timestamp.
(define (next-timestamp! table)
  (doc 'export #t)
  (let* ([counter (memo-table-counter table)]
         [ts (unbox counter)])
        (set-box! counter (+ ts 1))
        ts))

;;; evict-random! : MemoTable × Nat → Unit
;;; Evict random entries to make room for new ones.
;;; Uses O(k) random eviction instead of O(N log N) LRU sort.
;;; Removes approximately 10% of entries to amortize eviction cost.
(define (evict-random! table count-to-evict)
  (doc 'export #t)
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

;;; memo-lookup : MemoTable × Symbol × Nat → (Option α)
;;; Look up a cached result. Updates access time for bounded tables.
(define (memo-lookup table name offset)
  (doc 'export #t)
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

;;; memo-store! : MemoTable × Symbol × Nat × α → Unit
;;; Store a result in the cache. For bounded tables, evicts old entries if needed.
(define (memo-store! table name offset result)
  (doc 'export #t)
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

;;; memo : Symbol × (Parser α) → (MemoTable → (Parser α))
;;; Create a memoizing parser. The memo table is passed at parse time.
;;; This allows the same parser definition to be reused with different tables.
(define (memo name parser)
  (doc 'export #t)
  (lambda (table)
          (make-parser
           (lambda (state)
                   (let ([offset (pos-offset (parser-state-pos state))])
                        (let ([cached (memo-lookup table name offset)])
                             (if (just? cached)
                                 ;; Cache hit - return cached result
                                 (from-just cached)
                                 ;; Cache miss - compute and store
                                 (let ([result (run-parser parser state)])
                                      (memo-store! table name offset result)
                                      result))))))))

;;; memo-ref : (MemoTable → (Parser α)) × MemoTable → (Parser α)
;;; Resolve a memoized parser with its table.
(define (memo-ref memo-parser table)
  (doc 'export #t)
  (memo-parser table))

;;; parse-with-memo : (MemoTable → (Parser α)) × String × MemoTable → (Either Error α)
;;; Parse using memoization.
(define (parse-with-memo memo-parser input table)
  (doc 'export #t)
  (let* ([parser (memo-ref memo-parser table)]
         [result (run-parser parser (parser-initial-state input))])
        (if (right? result)
            (right (car (from-right result)))
            result)))

;;; parse-packrat : (MemoTable → (Parser α)) × String → (Either Error α)
;;; Parse with a fresh memo table (convenience function).
(define (parse-packrat memo-parser input)
  (doc 'export #t)
  (parse-with-memo memo-parser input (make-memo-table)))

;;; ====
;;; Packrat Combinators
;;; ====
;;;
;;; These combinators work with memoized parsers.

;;; memo-bind : (MemoTable → (Parser α)) × (α → (MemoTable → (Parser β))) → (MemoTable → (Parser β))
;;; Monadic bind for memoized parsers.
(define (memo-bind mp f)
  (doc 'export #t)
  (lambda (table)
          (parser-bind (memo-ref mp table)
                       (lambda (x) (memo-ref (f x) table)))))

;;; memo-then : (MemoTable → (Parser α)) × (MemoTable → (Parser β)) → (MemoTable → (Parser β))
;;; Sequence memoized parsers, discarding first result.
(define (memo-then mp1 mp2)
  (doc 'export #t)
  (lambda (table)
          (parser-then (memo-ref mp1 table) (memo-ref mp2 table))))

;;; memo-or : (MemoTable → (Parser α)) × (MemoTable → (Parser α)) → (MemoTable → (Parser α))
;;; Try memoized parsers in order.
(define (memo-or mp1 mp2)
  (doc 'export #t)
  (lambda (table)
          (parser-or (memo-ref mp1 table) (memo-ref mp2 table))))

;;; memo-pure : α → (MemoTable → (Parser α))
;;; Lift a value into the memoized parser context.
(define (memo-pure x)
  (doc 'export #t)
  (lambda (table) (parser-pure x)))

;;; memo-map : (α → β) × (MemoTable → (Parser α)) → (MemoTable → (Parser β))
;;; Map over a memoized parser.
(define (memo-map f mp)
  (doc 'export #t)
  (lambda (table)
          (parser-map f (memo-ref mp table))))

;;; memo-many : (MemoTable → (Parser α)) → (MemoTable → (Parser (List α)))
;;; Zero or more of a memoized parser.
(define (memo-many mp)
  (doc 'export #t)
  (lambda (table)
          (parser-many (memo-ref mp table))))

;;; memo-some : (MemoTable → (Parser α)) → (MemoTable → (Parser (List α)))
;;; One or more of a memoized parser.
(define (memo-some mp)
  (doc 'export #t)
  (lambda (table)
          (parser-some (memo-ref mp table))))

;;; lift-parser : (Parser α) → (MemoTable → (Parser α))
;;; Lift a regular parser to work with memo combinators.
(define (lift-parser p)
  (doc 'export #t)
  (lambda (table) p))

;;; ====
;;; Packrat Statistics (for debugging)
;;; ====

;;; memo-stats : MemoTable → (Nat × (Option Nat))
;;; Get statistics about memo table usage.
;;; Returns (current-entries . max-limit) for bounded tables,
;;; or (current-entries . #f) for unbounded tables.
(define (memo-stats table)
  (doc 'export #t)
  (let ([cache (memo-table-cache table)])
       (if (bounded-memo-table? table)
           (cons (hashtable-size cache)
                 (unbox (memo-table-limit table)))
           (cons (hashtable-size cache) #f))))

;;; memo-table-size : MemoTable → Nat
;;; Get the current number of entries in the memo table.
(define (memo-table-size table)
  (doc 'export #t)
  (hashtable-size (memo-table-cache table)))

;;; memo-table-set-limit! : MemoTable × Nat → Unit
;;; Change the limit of a bounded memo table.
;;; If new limit is smaller than current size, eviction happens on next store.
(define (memo-table-set-limit! table new-limit)
  (doc 'export #t)
  (if (bounded-memo-table? table)
      (set-box! (memo-table-limit table) new-limit)
      (error 'memo-table-set-limit! "cannot set limit on unbounded table")))
