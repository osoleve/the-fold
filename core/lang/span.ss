;;; core/lang/span.ss — Position-Aware Parser Combinators
;;; @module span
;;; @requires prelude parse
;;;
;;; Extends parse.ss with source location tracking for error reporting.
;;;
;;; ParseState = (state input offset line column file)
;;; SpannedResult a = (ok value remaining-state start-span)
;;;                 | (err expected state)
;;;
;;; Spans track file:line:column for precise error messages.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - parse.ss (basic combinators)

(load "core/base/prelude.ss")
(load "core/lang/parse.ss")

;;; ============================================================
;;; Source Spans
;;; ============================================================

;;; make-span : String × Nat × Nat × Nat × Nat → Span
;;; Create a source span.
(define (make-span file line column end-line end-column)
  (list 'span file line column end-line end-column))

;;; span? : Any → Boolean
(define (span? x)
  (and (pair? x) (eq? (car x) 'span)))

;;; Span accessors
(define (span-file s) (list-ref s 1))
(define (span-line s) (list-ref s 2))
(define (span-column s) (list-ref s 3))
(define (span-end-line s) (list-ref s 4))
(define (span-end-column s) (list-ref s 5))

;;; no-span : Span
;;; Placeholder span for when location is unknown.
(define no-span (make-span "<unknown>" 0 0 0 0))

;;; merge-spans : Span × Span → Span
;;; Create a span from start of first to end of second.
(define (merge-spans s1 s2)
  (make-span (span-file s1)
             (span-line s1)
             (span-column s1)
             (span-end-line s2)
             (span-end-column s2)))

;;; format-span : Span → String
;;; Format span for display.
(define (format-span s)
  (if (span? s)
      (string-append (span-file s) ":"
                     (number->string (span-line s)) ":"
                     (number->string (span-column s)))
      "<unknown>"))

;;; ============================================================
;;; Parser State with Position Tracking
;;; ============================================================

;;; State = (state input offset line column file)

(define (make-state input offset line column file)
  (list 'state input offset line column file))

(define (state? x)
  (and (pair? x) (eq? (car x) 'state)))

(define (state-input s) (list-ref s 1))
(define (state-offset s) (list-ref s 2))
(define (state-line s) (list-ref s 3))
(define (state-column s) (list-ref s 4))
(define (state-file s) (list-ref s 5))

;;; initial-state : String [× String] → State
;;; Create initial parser state from input.
(define (initial-state input . file-arg)
  (let ([file (if (null? file-arg) "<input>" (car file-arg))])
       (make-state input 0 1 1 file)))

;;; state-span : State → Span
;;; Get current position as a zero-width span.
(define (state-span s)
  (make-span (state-file s)
             (state-line s)
             (state-column s)
             (state-line s)
             (state-column s)))

;;; state-remaining : State → Nat
;;; Number of characters remaining in input from current offset.
(define (state-remaining s)
  (- (string-length (state-input s)) (state-offset s)))

;;; state-remaining-input : State → String
;;; Return the unconsumed portion of the input string.
;;; Note: This creates a new string; use sparingly (e.g., for debugging).
(define (state-remaining-input s)
  (substring (state-input s) (state-offset s) (string-length (state-input s))))

;;; state-peek : State → Char
;;; Return character at current offset (assumes not at end).
(define (state-peek s)
  (string-ref (state-input s) (state-offset s)))

;;; state-empty? : State → Boolean
;;; Check if no more input remains.
(define (state-empty? s)
  (>= (state-offset s) (string-length (state-input s))))

;;; advance-state : State × Char → State
;;; Advance state by one character, updating offset and line/column.
;;; Note: The original input string is retained; only offset changes.
(define (advance-state s char)
  (let ([input (state-input s)]
        [offset (state-offset s)]
        [line (state-line s)]
        [col (state-column s)]
        [file (state-file s)])
       (if (char=? char #\newline)
           (make-state input
                       (+ offset 1)
                       (+ line 1)
                       1
                       file)
           (make-state input
                       (+ offset 1)
                       line
                       (+ col 1)
                       file))))

;;; ============================================================
;;; Spanned Parse Results
;;; ============================================================

;;; SpannedResult a =
;;;   | (ok value state span)     ; value with ending state and covering span
;;;   | (err expected state)      ; failure with state at error position

(define (spanned-success value state span)
  (list 'ok value state span))

(define (spanned-failure expected state)
  (list 'err expected state))

(define (spanned-ok? r) (and (pair? r) (eq? (car r) 'ok)))
(define (spanned-err? r) (and (pair? r) (eq? (car r) 'err)))

(define (spanned-value r) (list-ref r 1))
(define (spanned-state r) (list-ref r 2))
(define (spanned-span r) (list-ref r 3))
(define (spanned-expected r) (list-ref r 1))
(define (spanned-error-state r) (list-ref r 2))

;;; ============================================================
;;; Spanned Parser Type
;;; ============================================================

;;; SpannedParser a = State → SpannedResult a

;;; run-spanned : SpannedParser a × String [× String] → SpannedResult a
(define (run-spanned parser input . file-arg)
  (let ([file (if (null? file-arg) "<input>" (car file-arg))])
       (parser (initial-state input file))))

;;; ============================================================
;;; Primitive Spanned Parsers
;;; ============================================================

;;; s-pure : a → SpannedParser a
;;; Always succeeds with value, zero-width span at current position.
(define (s-pure value)
  (lambda (state)
          (spanned-success value state (state-span state))))

;;; s-fail : String → SpannedParser a
;;; Always fails with message.
(define (s-fail msg)
  (lambda (state)
          (spanned-failure msg state)))

;;; s-item : SpannedParser Char
;;; Consume one character.
(define s-item
  (lambda (state)
          (if (state-empty? state)
              (spanned-failure "any character" state)
              (let* ([char (state-peek state)]
                     [start-span (state-span state)]
                     [next-state (advance-state state char)]
                     [end-span (state-span next-state)])
                    (spanned-success char next-state (merge-spans start-span end-span))))))

;;; s-eof : SpannedParser Unit
;;; Succeed only at end of input.
(define s-eof
  (lambda (state)
          (if (state-empty? state)
              (spanned-success '() state (state-span state))
              (spanned-failure "end of input" state))))

;;; s-satisfy : (Char → Bool) × String → SpannedParser Char
;;; Consume a character if predicate holds.
(define (s-satisfy pred label)
  (lambda (state)
          (if (state-empty? state)
              (spanned-failure label state)
              (let ([char (state-peek state)])
                   (if (pred char)
                       (let* ([start-span (state-span state)]
                              [next-state (advance-state state char)]
                              [end-span (state-span next-state)])
                             (spanned-success char next-state (merge-spans start-span end-span)))
                       (spanned-failure label state))))))

;;; ============================================================
;;; Character Parsers (Spanned)
;;; ============================================================

(define (s-char target)
  (s-satisfy (lambda (c) (char=? c target))
             (list->string (list target))))

(define s-digit
  (s-satisfy char-numeric? "digit"))

(define s-alpha
  (s-satisfy char-alphabetic? "letter"))

(define s-space
  (s-satisfy char-whitespace? "whitespace"))

(define s-alphanum
  (s-satisfy (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))
             "alphanumeric"))

;;; s-string : String → SpannedParser String
;;; Match an exact string.
(define (s-string target)
  (lambda (state)
          (let ([tlen (string-length target)]
                [input (state-input state)]
                [offset (state-offset state)])
               (if (< (state-remaining state) tlen)
                   (spanned-failure target state)
                   ;; Compare character-by-character without creating substring
                   (let check-loop ([i 0])
                        (if (= i tlen)
                            ;; Match succeeded, advance through all chars for line/column tracking
                            (let advance-loop ([s state] [j 0])
                                 (if (= j tlen)
                                     (spanned-success target s
                                                      (merge-spans (state-span state) (state-span s)))
                                     (advance-loop (advance-state s (string-ref input (+ offset j)))
                                                   (+ j 1))))
                            ;; Still checking
                            (if (char=? (string-ref input (+ offset i))
                                        (string-ref target i))
                                (check-loop (+ i 1))
                                (spanned-failure target state))))))))

;;; ============================================================
;;; Spanned Combinators — Core
;;; ============================================================

;;; s-bind : SpannedParser a × (a → SpannedParser b) → SpannedParser b
;;; Monadic bind with span merging.
(define (s-bind parser f)
  (lambda (state)
          (let ([result (parser state)])
               (if (spanned-ok? result)
                   (let* ([value (spanned-value result)]
                          [next-state (spanned-state result)]
                          [span1 (spanned-span result)]
                          [result2 ((f value) next-state)])
                         (if (spanned-ok? result2)
                             ;; Merge spans from first and second parse
                             (spanned-success (spanned-value result2)
                                              (spanned-state result2)
                                              (merge-spans span1 (spanned-span result2)))
                             result2))
                   result))))

;;; s-seq : SpannedParser a × SpannedParser b → SpannedParser b
(define (s-seq p1 p2)
  (s-bind p1 (lambda (_) p2)))

;;; s-seq-left : SpannedParser a × SpannedParser b → SpannedParser a
(define (s-seq-left p1 p2)
  (s-bind p1 (lambda (a)
                     (s-bind p2 (lambda (_)
                                        (s-pure a))))))

;;; s-alt : SpannedParser a × SpannedParser a → SpannedParser a
(define (s-alt p1 p2)
  (lambda (state)
          (let ([result (p1 state)])
               (if (spanned-ok? result)
                   result
                   (p2 state)))))

;;; s-choice : (List SpannedParser) → SpannedParser a
(define (s-choice parsers)
  (if (null? parsers)
      (s-fail "no alternatives")
      (if (null? (cdr parsers))
          (car parsers)
          (s-alt (car parsers) (s-choice (cdr parsers))))))

;;; s-map : (a → b) × SpannedParser a → SpannedParser b
(define (s-map f parser)
  (s-bind parser (lambda (a) (s-pure (f a)))))

;;; ============================================================
;;; Spanned Combinators — Repetition
;;; ============================================================

(define (s-many-helper parser acc state)
  (let ([result (parser state)])
       (if (spanned-ok? result)
           (s-many-helper parser
                          (cons (spanned-value result) acc)
                          (spanned-state result))
           (cons (reverse acc) state))))

;;; s-many : SpannedParser a → SpannedParser (List a)
(define (s-many parser)
  (lambda (state)
          (let* ([start-span (state-span state)]
                 [result (s-many-helper parser '() state)]
                 [values (car result)]
                 [end-state (cdr result)]
                 [end-span (state-span end-state)])
                (spanned-success values end-state (merge-spans start-span end-span)))))

;;; s-many1 : SpannedParser a → SpannedParser (List a)
(define (s-many1 parser)
  (s-bind parser (lambda (first)
                         (s-bind (s-many parser) (lambda (rest)
                                                         (s-pure (cons first rest)))))))

;;; s-optional : SpannedParser a → SpannedParser (Maybe a)
(define (s-optional parser)
  (s-alt (s-map (lambda (x) (list x)) parser)
         (s-pure '())))

;;; s-sep-by : SpannedParser a × SpannedParser sep → SpannedParser (List a)
(define (s-sep-by parser sep)
  (s-alt
   (s-bind parser (lambda (first)
                          (s-bind (s-many (s-seq sep parser)) (lambda (rest)
                                                                      (s-pure (cons first rest))))))
   (s-pure '())))

;;; ============================================================
;;; Spanned Utility Parsers
;;; ============================================================

(define s-spaces (s-many s-space))
(define s-spaces1 (s-many1 s-space))

(define (s-lexeme parser)
  (s-seq-left parser s-spaces))

(define (s-symbol str)
  (s-lexeme (s-string str)))

(define (s-between open close parser)
  (s-seq open (s-seq-left parser close)))

(define (s-parens parser)
  (s-between (s-symbol "(") (s-symbol ")") parser))

(define (s-brackets parser)
  (s-between (s-symbol "[") (s-symbol "]") parser))

;;; s-nat : SpannedParser Nat
(define s-nat
  (s-bind (s-many1 s-digit) (lambda (digits)
                                    (s-pure (string->number (list->string digits))))))

;;; s-int : SpannedParser Int
(define s-int
  (s-bind (s-optional (s-char #\-)) (lambda (sign)
                                            (s-bind s-nat (lambda (n)
                                                                  (s-pure (if (null? sign) n (- n))))))))

;;; s-identifier : SpannedParser String
(define s-identifier
  (s-bind s-alpha (lambda (first)
                          (s-bind (s-many (s-alt s-alphanum (s-char #\_))) (lambda (rest)
                                                                                   (s-pure (list->string (cons first rest))))))))

;;; ============================================================
;;; Spanned Value Wrapper
;;; ============================================================

;;; A Spanned value pairs a value with its source span.
;;; This is used for AST nodes.

(define (spanned value span)
  (list 'spanned value span))

(define (spanned-value? x)
  (and (pair? x) (eq? (car x) 'spanned)))

(define (get-spanned-value sv)
  (if (spanned-value? sv) (cadr sv) sv))

(define (get-value-span sv)
  (if (spanned-value? sv) (caddr sv) no-span))

;;; s-spanned : SpannedParser a → SpannedParser (Spanned a)
;;; Wrap a parser to return a spanned value.
(define (s-spanned parser)
  (lambda (state)
          (let ([result (parser state)])
               (if (spanned-ok? result)
                   (spanned-success (spanned (spanned-value result)
                                             (spanned-span result))
                                    (spanned-state result)
                                    (spanned-span result))
                   result))))

;;; ============================================================
;;; Error Formatting
;;; ============================================================

;;; format-parse-error : SpannedResult → String
(define (format-parse-error result)
  (if (spanned-err? result)
      (let* ([expected (spanned-expected result)]
             [state (spanned-error-state result)]
             [loc (format-span (state-span state))])
            (string-append loc ": expected " expected))
      "no error"))
