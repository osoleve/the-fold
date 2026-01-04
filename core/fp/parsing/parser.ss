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
;;; which copies the remainder of the string (O(N) per character = O(N² total),
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
(define (state-at-end? s)
  (>= (state-index s) (string-length (state-input s))))

;;; state-next : State → (Char × State) | #f
(define (state-next s)
  (let ([input (state-input s)]
        [index (state-index s)]
        [pos (state-pos s)])
       (if (>= index (string-length input))
           #f
           (let ([ch (string-ref input index)])
                (cons ch (make-state input (+ index 1) (advance-pos pos ch)))))))

;;; initial-state : String → State
(define (initial-state input)
  (make-state input 0 initial-pos))

;;; ============================================================
;;; Error Types
;;; ============================================================

;;; make-parse-error : Pos × String × (List String) → ParseError
(define (make-parse-error pos message expected)
  (list 'parse-error pos message expected))

;;; parse-error? : Any → Boolean
(define (parse-error? e)
  (and (pair? e) (eq? (car e) 'parse-error)))

;;; parse-error-pos : ParseError → Pos
(define (parse-error-pos e) (list-ref e 1))

;;; parse-error-message : ParseError → String
(define (parse-error-message e) (list-ref e 2))

;;; parse-error-expected : ParseError → (List String)
(define (parse-error-expected e) (list-ref e 3))

;;; format-error : ParseError → String
(define (format-error e)
  (let ([pos (parse-error-pos e)])
       (format "Parse error at line ~a, col ~a: ~a~a"
               (pos-line pos)
               (pos-col pos)
               (parse-error-message e)
               (if (null? (parse-error-expected e))
                   ""
                   (format " (expected ~a)" (parse-error-expected e))))))

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

;;; parser-run : Parser × State → Either Error (Value × State)
;;; Internal runner that applies the parser's function.
(define (parser-run parser state)
  ((cadr parser) state))

;;; run-parser : Parser × State → Either Error (Value × State)
;;; Alias for parser-run (deprecated, use parser-run to avoid conflicts).
(define run-parser parser-run)

;;; parse : Parser × String → Either Error Value
;;; Run parser on input string.
(define (parse parser input)
  (let ([result (parser-run parser (initial-state input))])
       (if (right? result)
           (right (car (from-right result)))  ; Extract value
           result)))  ; Return error

;;; parser-parse : Parser × String → Either Error Value
(define parser-parse parse)

;;; parse-all : Parser × String → Either Error Value
;;; Run parser and require complete input consumption.
(define (parse-all parser input)
  (let ([full-parser (parser-left parser eof)])
       (parse full-parser input)))

;;; parser-parse-all : Parser × String → Either Error Value
(define parser-parse-all parse-all)

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
               (left (make-parse-error (state-pos state) "expected end of input" '("EOF")))))))

;;; satisfy : (Char → Boolean) × String → Parser Char
;;; Parser that succeeds if the next character satisfies the predicate.
(define (satisfy pred label)
  (make-parser
   (lambda (state)
           (let ([next (state-next state)])
                (if (and next (pred (car next)))
                    (right (cons (car next) (cdr next)))
                    (left (make-parse-error (state-pos state)
                                            (format "expected ~a" label)
                                            (list label))))))))

;;; char : Char → Parser Char
(define (char c)
  (satisfy (lambda (x) (char=? x c)) (list->string (list c))))

;;; any-char : Parser Char
(define any-char
  (satisfy (lambda (x) #t) "any character"))

;;; one-of : String → Parser Char
(define (one-of chars)
  (satisfy (lambda (c) (string-contains? chars (list->string (list c))))
           (format "one of ~s" chars)))

;;; none-of : String → Parser Char
(define (none-of chars)
  (satisfy (lambda (c) (not (string-contains? chars (list->string (list c)))))
           (format "none of ~s" chars)))

;;; string-parser : String → Parser String
(define (string-parser s)
  (make-parser
   (lambda (state)
           (let loop ([chars (string->list s)] [current-state state])
                (cond
                 [(null? chars) (right (cons s current-state))]
                 [else
                  (let ([next (state-next current-state)])
                       (if (and next (char=? (car next) (car chars)))
                           (loop (cdr chars) (cdr next))
                           (left (make-parse-error (state-pos current-state)
                                                   (format "expected ~s" s)
                                                   (list s)))))])))))

;;; ============================================================
;;; Combinators — Sequencing
;;; ============================================================

;;; parser-bind : Parser a × (a → Parser b) → Parser b
(define (parser-bind p f)
  (make-parser
   (lambda (state)
           (let ([result (parser-run p state)])
                (if (right? result)
                    (let ([val (car (from-right result))]
                          [new-state (cdr (from-right result))])
                         (parser-run (f val) new-state))
                    result)))))

;;; parser-then : Parser a × Parser b → Parser b
(define (parser-then p1 p2)
  (parser-bind p1 (lambda (_) p2)))

;;; parser-left : Parser a × Parser b → Parser a
(define (parser-left p1 p2)
  (parser-bind p1 (lambda (x) (parser-then p2 (parser-pure x)))))

;;; parser-map : (a → b) × Parser a → Parser b
(define (parser-map f p)
  (parser-bind p (lambda (x) (parser-pure (f x)))))

;;; ============================================================
;;; Combinators — Choice
;;; ============================================================

;;; parser-or : Parser a × Parser a → Parser a
(define (parser-or p1 p2)
  (make-parser
   (lambda (state)
           (let ([result1 (parser-run p1 state)])
                (if (right? result1)
                    result1
                    ;; Optimization: only try p2 if p1 didn't consume input OR if we use 'try'
                    ;; Here we use a simple choice: always try p2 if p1 fails.
                    (parser-run p2 state))))))

;;; try : Parser a → Parser a
;;; Backtracking wrapper. In this implementation, parser-or already backtracks
;;; because it passes the same state to both parsers. This is for future compatibility.
(define (try p) p)

;;; choice : (List Parser a) → Parser a
(define (choice ps)
  (if (null? ps)
      (parser-fail "no alternative succeeded")
      (parser-or (car ps) (choice (cdr ps)))))

;;; ============================================================
;;; Combinators — Repetition
;;; ============================================================

;;; many : Parser a → Parser (List a)
;;; Parse zero or more occurrences. Guards against infinite loops when
;;; parser succeeds without consuming input.
(define (many p)
  (make-parser
   (lambda (state)
           (let loop ([current-state state] [acc '()])
                (let* ([start-index (state-index current-state)]
                       [result (parser-run p current-state)])
                      (if (right? result)
                          (let ([new-state (cdr (from-right result))])
                               ;; Guard: if parser succeeded but consumed nothing, stop
                               ;; to prevent infinite loop
                               (if (= (state-index new-state) start-index)
                                   (right (cons (reverse acc) current-state))
                                   (loop new-state (cons (car (from-right result)) acc))))
                          (right (cons (reverse acc) current-state))))))))

;;; some : Parser a → Parser (List a)
(define (some p)
  (parser-bind p (lambda (x) (parser-map (lambda (xs) (cons x xs)) (many p)))))

;;; count : Nat × Parser a → Parser (List a)
(define (count n p)
  (if (zero? n)
      (parser-pure '())
      (parser-bind p (lambda (x) (parser-map (lambda (xs) (cons x xs)) (count (- n 1) p))))))

;;; sep-by1 : Parser a × Parser sep → Parser (List a)
(define (sep-by1 p sep)
  (parser-bind p (lambda (x) (parser-map (lambda (xs) (cons x xs)) (many (parser-then sep p))))))

;;; sep-by : Parser a × Parser sep → Parser (List a)
(define (sep-by p sep)
  (parser-or (sep-by1 p sep) (parser-pure '())))

;;; end-by : Parser a × Parser sep → Parser (List a)
(define (end-by p sep)
  (many (parser-left p sep)))

;;; ============================================================
;;; Combinators — Surrounding
;;; ============================================================

;;; between : Parser open × Parser close × Parser a → Parser a
(define (between open close p)
  (parser-then open (parser-left p close)))

;;; ============================================================
;;; Error Customization
;;; ============================================================

;;; label : Parser a × String → Parser a
(define (label p msg)
  (make-parser
   (lambda (state)
           (let ([result (parser-run p state)])
                (if (left? result)
                    (let ([err (from-left result)])
                         (left (make-parse-error (parse-error-pos err) msg (list msg))))
                    result)))))

;;; ============================================================
;;; Whitespace and Lexemes
;;; ============================================================

;;; whitespace : Parser Char
(define whitespace
  (satisfy char-whitespace? "whitespace"))

;;; space : Parser Char
;;; Alias for whitespace.
(define space whitespace)

;;; newline-char : Parser Char
(define newline-char
  (satisfy (lambda (c) (char=? c %newline)) "newline"))

;;; spaces : Parser (List Char)
(define spaces (many whitespace))

;;; lexeme : Parser a → Parser a
(define (lexeme p) (parser-left p spaces))

;;; symbol : String → Parser String
(define (symbol s) (lexeme (string-parser s)))

;;; parens : Parser a → Parser a
(define (parens p) (between (symbol "(") (symbol ")") p))

;;; braces : Parser a → Parser a
(define (braces p) (between (symbol "{") (symbol "}") p))

;;; brackets : Parser a → Parser a
(define (brackets p) (between (symbol "[") (symbol "]") p))

;;; comma : Parser String
(define comma (symbol ","))

;;; comma-sep : Parser a → Parser (List a)
(define (comma-sep p) (sep-by p comma))

;;; ============================================================
;;; Standard Parsers
;;; ============================================================

;;; digit : Parser Char
(define digit (satisfy char-numeric? "digit"))

;;; letter : Parser Char
(define letter (satisfy char-alphabetic? "letter"))

;;; alphanum : Parser Char
(define alphanum (satisfy (lambda (c) (or (char-alphabetic? c) (char-numeric? c))) "alphanumeric"))

;;; decimal : Parser Number
(define decimal
  (lexeme
   (parser-bind (many digit)
                (lambda (digits)
                        (if (null? digits)
                            (parser-fail "expected digits")
                            (parser-pure (string->number (list->string digits))))))))

;;; ============================================================
;;; Expression Parsing (Chainl/Chainr)
;;; ============================================================

;;; chainl1 : Parser a × Parser (a × a → a) → Parser a
(define (chainl1 p op)
  (parser-bind p
               (lambda (init)
                       (let loop ([acc init])
                            (parser-or
                             (parser-bind op
                                          (lambda (f)
                                                  (parser-bind p
                                                               (lambda (x)
                                                                       (loop (f acc x))))))
                             (parser-pure acc))))))

;;; chainr1 : Parser a × Parser (a × a → a) → Parser a
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

;;; ============================================================
;;; Lookahead
;;; ============================================================

;;; look-ahead : Parser a → Parser a
(define (look-ahead p)
  (make-parser
   (lambda (state)
           (let ([result (parser-run p state)])
                (if (right? result)
                    (right (cons (car (from-right result)) state))
                    result)))))

;;; not-followed-by : Parser a → Parser ()
(define (not-followed-by p)
  (make-parser
   (lambda (state)
           (let ([result (parser-run p state)])
                (if (right? result)
                    (left (make-parse-error (state-pos state) "unexpected lookahead match" '()))
                    (right (cons '() state)))))))

;;; ============================================================
;;; Packrat Memoization (Optional)
;;; ============================================================

;;; make-memo-table : → MemoTable
(define (make-memo-table)
  (list 'memo-table (make-hashtable (lambda (x) (string-hash (symbol->string (car x)))) (lambda (x y) (and (eq? (car x) (car y)) (= (cdr x) (cdr y)))))))

;;; make-bounded-memo-table : Nat → MemoTable
(define (make-bounded-memo-table limit)
  (list 'memo-table (make-hashtable (lambda (x) (string-hash (symbol->string (car x)))) (lambda (x y) (and (eq? (car x) (car y)) (= (cdr x) (cdr y))))) (box limit) (box 0)))

;;; memo-table-cache : MemoTable → Hashtable
(define (memo-table-cache table) (list-ref table 1))

;;; bounded-memo-table? : MemoTable → Boolean
(define (bounded-memo-table? table) (> (length table) 2))

;;; memo-table-limit : MemoTable → Box Nat
(define (memo-table-limit table) (list-ref table 2))

;;; memo-table-timestamp : MemoTable → Box Nat
(define (memo-table-timestamp table) (list-ref table 3))

;;; make-memo-entry : Result × Nat → MemoEntry
(define (make-memo-entry result timestamp) (cons result timestamp))

;;; memo-entry-result : MemoEntry → Result
(define (memo-entry-result entry) (car entry))

;;; memo-entry-timestamp : MemoEntry → Nat
(define (memo-entry-timestamp entry) (cdr entry))

;;; next-timestamp! : MemoTable → Nat
(define (next-timestamp! table)
  (let ([b (memo-table-timestamp table)])
       (let ([ts (+ (unbox b) 1)])
            (set-box! b ts)
            ts)))

;;; memo-key : Symbol × Nat → (Symbol . Nat)
(define (memo-key name offset) (cons name offset))

;;; memo-lookup : MemoTable × Symbol × Nat → Maybe Result
(define (memo-lookup table name offset)
  (let ([entry (hashtable-ref (memo-table-cache table) (memo-key name offset) #f)])
       (if entry
           (just (memo-entry-result entry))
           nothing)))

;;; evict-random! : Hashtable × Nat → ()
(define (evict-random! table count)
  ;; Simplified eviction: just clear the table if it gets too large,
  ;; or we could implement LRU. For now, we clear.
  (hashtable-clear! table))

;;; memo-store! : MemoTable × Symbol × Nat × Result → ()
(define (memo-store! table name offset result)
  (if (bounded-memo-table? table)
      (let ([limit (unbox (memo-table-limit table))]
            [cache (memo-table-cache table)]
            [current-size (hashtable-size (memo-table-cache table))]
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
                                 (let ([result (parser-run parser state)])
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
         [result (parser-run parser (initial-state input))])
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
