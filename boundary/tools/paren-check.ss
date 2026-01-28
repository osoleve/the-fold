(load "core/base/prelude.ss")

(doc 'module 'paren-check)
(doc 'description "Parenthesis balance checker with detailed error reporting")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "Usage: (paren-check \"path/to/file.ss\") for full report")
(doc 'note "(paren-check \"path/to/file.ss\" 100 150) for lines 100-150 only")
(doc 'note "(paren-balance \"path/to/file.ss\") for just the final balance")
(doc 'note "(paren-locate \"path/to/file.ss\") for stack-based exact error locations")
(doc 'note "(paren-errors \"path/to/file.ss\") for list of error structs")
(doc 'note "(paren-suggest \"path/to/file.ss\") for indentation-based fix suggestions")
(doc 'note "(paren-summary \"path/to/file.ss\") for quick one-line status")

(doc 'section 'core-balance)

(doc count-parens 'type '(-> String (Values Int Int Int)))
(doc count-parens 'description "Count open and close parens in a string, returns (opens closes balance-delta), handles (, ), [, ], {, } and ignores chars in strings/comments")
(define (count-parens line)
  (let loop ([i 0]
             [opens 0]
             [closes 0]
             [in-string #f]
             [in-comment #f])
       (if (>= i (string-length line))
           (values opens closes (- opens closes))
           (let ([c (string-ref line i)])
                (cond
                 ;; Already in comment - skip rest of line
                 [in-comment
                  (values opens closes (- opens closes))]
                 
                 ;; String handling
                 [(and in-string (char=? c #\\))
                  ;; Escape sequence - skip next char
                  (loop (+ i 2) opens closes in-string in-comment)]
                 [(and in-string (char=? c #\"))
                  ;; End string
                  (loop (+ i 1) opens closes #f in-comment)]
                 [in-string
                  ;; Inside string - ignore parens
                  (loop (+ i 1) opens closes in-string in-comment)]
                 [(char=? c #\")
                  ;; Start string
                  (loop (+ i 1) opens closes #t in-comment)]
                 
                 ;; Comment start
                 [(char=? c #\;)
                  (loop (+ i 1) opens closes in-string #t)]
                 
                 ;; Count parens
                 [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
                  (loop (+ i 1) (+ opens 1) closes in-string in-comment)]
                 [(or (char=? c #\)) (char=? c #\]) (char=? c #\}))
                  (loop (+ i 1) opens (+ closes 1) in-string in-comment)]
                 
                 ;; Other chars
                 [else
                  (loop (+ i 1) opens closes in-string in-comment)])))))

;;; ====
;;; File Analysis
;;; ====

;;; analyze-file : String -> (List LineInfo)
;;; Analyze a file and return line-by-line paren info.
;;; Each LineInfo is (line-num opens closes running-balance line-text)
(define (analyze-file path)
  (call-with-input-file path
                        (lambda (port)
                                (let loop ([line-num 1]
                                           [running 0]
                                           [results '()])
                                     (let ([line (get-line port)])
                                          (if (eof-object? line)
                                              (reverse results)
                                              (let-values ([(opens closes delta) (count-parens line)])
                                                          (let ([new-running (+ running delta)])
                                                               (loop (+ line-num 1)
                                                                     new-running
                                                                     (cons (list line-num opens closes new-running line)
                                                                           results))))))))))

;;; ====
;;; Reporting
;;; ====

;;; format-balance : Int -> String
;;; Format balance with indicator.
(define (format-balance n)
  (cond
   [(> n 0) (string-append "+" (number->string n))]
   [(< n 0) (number->string n)]
   [else "0"]))

;;; paren-check : String [Int Int] -> void
;;; Print paren balance report for a file.
;;; Optional start/end line numbers to focus on a range.
(define paren-check
  (case-lambda
   [(path)
    (paren-check path 1 999999)]
   [(path start-line end-line)
    (let ([results (analyze-file path)])
         (display "\n")
         (display "Parenthesis Balance Report: ")
         (display path)
         (display "\n")
         (display (make-string 60 #\─))
         (display "\n")
         (display "Line   Open Close  Balance  Preview\n")
         (display (make-string 60 #\─))
         (display "\n")
         
         (for-each
          (lambda (info)
                  (let ([line-num (car info)]
                        [opens (cadr info)]
                        [closes (caddr info)]
                        [running (cadddr info)]
                        [text (car (cddddr info))])
                       (when (and (>= line-num start-line)
                                  (<= line-num end-line)
                                  (or (not (= opens 0))
                                      (not (= closes 0))
                                      (< running 0)))  ; Always show negative balance
                             ;; Line number
                             (display (pad-left (number->string line-num) 5))
                             (display "  ")
                             ;; Opens
                             (display (pad-left (number->string opens) 4))
                             (display "  ")
                             ;; Closes
                             (display (pad-left (number->string closes) 5))
                             (display "  ")
                             ;; Running balance with indicator
                             (let ([bal-str (format-balance running)])
                                  (display (pad-left bal-str 7))
                                  (when (< running 0)
                                        (display " ⚠️")))
                             (display "  ")
                             ;; Preview (truncated)
                             (display (truncate-string (string-trim text) 30))
                             (newline))))
          results)
         
         (display (make-string 60 #\─))
         (display "\n")
         
         ;; Final summary
         (let ([final-balance (if (null? results)
                                  0
                                  (cadddr (car (reverse results))))])
              (display "Final balance: ")
              (display (format-balance final-balance))
              (cond
               [(= final-balance 0)
                (display " ✓ Balanced\n")]
               [(> final-balance 0)
                (display " ✗ ")
                (display final-balance)
                (display " unclosed open paren(s)\n")]
               [else
                (display " ✗ ")
                (display (- final-balance))
                (display " extra close paren(s)\n")])
              (display "\n")))]))

;;; paren-balance : String -> Int
;;; Return just the final balance (0 = balanced).
(define (paren-balance path)
  (let ([results (analyze-file path)])
       (if (null? results)
           0
           (cadddr (car (reverse results))))))

;;; find-imbalance : String -> (List LineInfo)
;;; Return only lines where balance goes negative.
(define (find-imbalance path)
  (filter
   (lambda (info)
           (< (cadddr info) 0))
   (analyze-file path)))

;;; ====
;;; String Utilities
;;; ====

(define (pad-left str width)
  (let ([len (string-length str)])
       (if (>= len width)
           str
           (string-append (make-string (- width len) #\space) str))))

(define (truncate-string str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

;;; NOTE: string-trim provided by core/prelude.ss

;;; ====
;;; Quick Diagnostic
;;; ====

;;; paren-diag : String Int -> void
;;; Show detailed diagnosis around a specific line.
(define (paren-diag path target-line)
  (let* ([start (max 1 (- target-line 10))]
         [end (+ target-line 10)]
         [results (analyze-file path)])
        (display "\n")
        (display "Diagnosis around line ")
        (display target-line)
        (display ":\n")
        (display (make-string 70 #\─))
        (display "\n")
        
        (for-each
         (lambda (info)
                 (let ([line-num (car info)]
                       [opens (cadr info)]
                       [closes (caddr info)]
                       [running (cadddr info)]
                       [text (car (cddddr info))])
                      (when (and (>= line-num start) (<= line-num end))
                            ;; Marker for target line
                            (if (= line-num target-line)
                                (display ">>> ")
                                (display "    "))
                            ;; Line number
                            (display (pad-left (number->string line-num) 4))
                            (display " [")
                            (display (format-balance running))
                            (display "] ")
                            ;; Full line (truncated)
                            (display (truncate-string text 55))
                            (newline))))
         results)
        
        (display (make-string 70 #\─))
        (display "\n\n")))

;;; ====
;;; Stack-Based Location Tracking
;;; ====
;;;
;;; Instead of just counting parens, this tracks a stack of openers
;;; with their exact locations. When something goes wrong, we can
;;; point to exactly where the problem is.

;;; An opener is (type line col indent form) where:
;;;   type is 'paren, 'bracket, or 'brace
;;;   indent is the indentation level of the line containing the opener
;;;   form is the detected form name (e.g., 'define, 'let, 'lambda) or #f
(define (make-opener type line col indent form)
  (list type line col indent form))
(define (opener-type o) (car o))
(define (opener-line o) (cadr o))
(define (opener-col o) (caddr o))
(define (opener-indent o) (list-ref o 3))
(define (opener-form o) (list-ref o 4))

;;; An error is (kind message line col . extra)
;;; Kinds: 'unclosed, 'extra-close, 'mismatch
(define (make-paren-error kind message line col . extra)
  (list* kind message line col extra))
(define (paren-error-kind e) (car e))
(define (paren-error-message e) (cadr e))
(define (paren-error-line e) (caddr e))
(define (paren-error-col e) (cadddr e))

;;; char->opener-type : Char -> Symbol | #f
(define (char->opener-type c)
  (cond
    [(char=? c #\() 'paren]
    [(char=? c #\[) 'bracket]
    [(char=? c #\{) 'brace]
    [else #f]))

;;; char->closer-type : Char -> Symbol | #f
(define (char->closer-type c)
  (cond
    [(char=? c #\)) 'paren]
    [(char=? c #\]) 'bracket]
    [(char=? c #\}) 'brace]
    [else #f]))

;;; type->open-char : Symbol -> Char
(define (type->open-char type)
  (case type
    [(paren) #\(]
    [(bracket) #\[]
    [(brace) #\{]))

;;; type->close-char : Symbol -> Char
(define (type->close-char type)
  (case type
    [(paren) #\)]
    [(bracket) #\]]
    [(brace) #\}]))

;;; ====
;;; Indentation Heuristics
;;; ====

;;; line-indent : String -> Int
;;; Return the indentation level (column of first non-whitespace char).
(define (line-indent line)
  (let ([len (string-length line)])
    (let loop ([i 0])
      (cond
        [(>= i len) i]
        [(char-whitespace? (string-ref line i)) (loop (+ i 1))]
        [else i]))))

;;; detect-form : String Int -> Symbol | #f
;;; Detect the Scheme form starting after the opener at col.
;;; Returns form symbol ('define, 'let, 'lambda, etc.) or #f.
(define (detect-form line col)
  (let ([len (string-length line)])
    ;; Skip whitespace after opener
    (let skip-ws ([i (+ col 1)])
      (cond
        [(>= i len) #f]
        [(char-whitespace? (string-ref line i)) (skip-ws (+ i 1))]
        [(char-alphabetic? (string-ref line i))
         ;; Extract the identifier (including / and : for namespaced forms)
         (let extract ([end i])
           (if (and (< end len)
                    (or (char-alphabetic? (string-ref line end))
                        (char-numeric? (string-ref line end))
                        (memv (string-ref line end) '(#\- #\_ #\? #\! #\* #\> #\/ #\:))))
               (extract (+ end 1))
               ;; Check if it's a known form
               (let ([sym (string->symbol (substring line i end))])
                 (if (memq sym '(define define-syntax define-record-type
                                 let let* letrec letrec* let-values let*-values
                                 lambda case-lambda
                                 if cond case when unless
                                 begin do
                                 syntax-rules syntax-case with-syntax
                                 parameterize guard
                                 call-with-values call/cc
                                 match match-let
                                 module library))
                     sym
                     #f))))]
        [else #f]))))

;;; form-expected-closers : Symbol -> Int
;;; Return expected minimum body forms for a given form type.
;;; This hints at minimum closing parens expected.
(define (form-expected-closers form)
  (case form
    [(define define-syntax) 2]        ; name + body
    [(lambda case-lambda) 2]          ; args + body
    [(let let* letrec letrec*) 2]     ; bindings + body
    [(if) 3]                          ; test + then + else
    [(cond case) 1]                   ; at least one clause
    [(begin) 1]                       ; at least one form
    [(when unless) 2]                 ; test + body
    [else 1]))

;;; Known form names for helpful messages
(define *form-names*
  '((define . "definition")
    (define-syntax . "syntax definition")
    (let . "let binding")
    (let* . "let* binding")
    (letrec . "letrec binding")
    (lambda . "lambda")
    (case-lambda . "case-lambda")
    (if . "conditional")
    (cond . "cond expression")
    (case . "case expression")
    (when . "when clause")
    (unless . "unless clause")
    (begin . "begin block")
    (do . "do loop")
    (syntax-rules . "syntax-rules")
    (syntax-case . "syntax-case")
    (match . "match expression")))

(define (form-name->string form)
  (cond
    [(assq form *form-names*) => cdr]
    [else (symbol->string form)]))

;;; paren-stack-analyze : String -> (Values Stack Errors)
;;; Analyze file with stack tracking. Returns final stack and list of errors.
(define (paren-stack-analyze path)
  (call-with-input-file path
    (lambda (port)
      (let line-loop ([line-num 1]
                      [stack '()]
                      [errors '()]
                      [in-string #f]
                      [in-block-comment 0]   ; nesting depth for #|...|#
                      [in-datum-skip 0])     ; #; state: 0=normal, -1=waiting, >0=inside at depth
        (let ([line (get-line port)])
          (if (eof-object? line)
              ;; EOF - check for unclosed items
              (let* ([unclosed-errors
                      (map (lambda (opener)
                             (let ([form (opener-form opener)]
                                   [indent (opener-indent opener)])
                               (make-paren-error
                                 'unclosed
                                 (if form
                                     (format "unclosed '~a' (~a at indent ~a) - never closed"
                                             (type->open-char (opener-type opener))
                                             (form-name->string form)
                                             indent)
                                     (format "unclosed '~a' (indent ~a) - never closed"
                                             (type->open-char (opener-type opener))
                                             indent))
                                 (opener-line opener)
                                 (opener-col opener))))
                           (reverse stack))]
                     ;; Also check for unterminated block comments
                     [block-error
                      (if (> in-block-comment 0)
                          (list (make-paren-error
                                  'unclosed
                                  "unterminated block comment #|...|#"
                                  line-num 0))  ; line-num is EOF position
                          '())]
                     ;; Check for unterminated datum comments
                     ;; in-datum-skip = -1 means waiting, >0 means inside parens
                     [datum-error
                      (if (not (= in-datum-skip 0))
                          (list (make-paren-error
                                  'unclosed
                                  "unterminated datum comment #;..."
                                  line-num 0))
                          '())]
                     ;; And unterminated strings
                     [string-error
                      (if in-string
                          (list (make-paren-error
                                  'unclosed
                                  "unterminated string literal"
                                  line-num 0))
                          '())])
                ;; Return stack for suggestion analysis, plus all errors
                (values stack (append (reverse errors) unclosed-errors block-error datum-error string-error)))
              ;; Process this line
              (let-values ([(new-stack new-errors new-in-string new-in-block new-in-datum)
                            (process-line line line-num stack errors
                                          in-string in-block-comment in-datum-skip)])
                (line-loop (+ line-num 1)
                           new-stack
                           new-errors
                           new-in-string
                           new-in-block
                           new-in-datum))))))))

;;; skip-datum : String Int Bool -> (Values Int Bool Bool)
;;; Skip a single datum (S-expression) on the current line.
;;; Returns (new-col new-in-string crossed-line-boundary).
;;; If the datum extends beyond the line, returns with crossed-line-boundary=#t.
;;;
;;; A datum is:
;;;   - A list/vector starting with (, [, {, or #(
;;;   - An atom (symbol, number, boolean, etc.)
;;;   - A string "..."
;;;   - A character literal #\x
(define (skip-datum line start-col in-string)
  (let ([len (string-length line)])
    ;; Skip leading whitespace
    (let skip-ws ([col start-col])
      (cond
        [(>= col len)
         ;; End of line - datum is on next line(s)
         (values col in-string #t)]
        [(char-whitespace? (string-ref line col))
         (skip-ws (+ col 1))]
        [else
         ;; Found start of datum
         (let ([c (string-ref line col)])
           (cond
             ;; List/vector opener - skip until matching closer
             [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
              (skip-balanced line col in-string)]
             ;; Vector #( or bytevector #vu8(
             [(char=? c #\#)
              (cond
                [(and (< (+ col 1) len)
                      (char=? (string-ref line (+ col 1)) #\())
                 ;; #( vector
                 (skip-balanced line (+ col 1) in-string)]
                [(and (< (+ col 3) len)
                      (char=? (string-ref line (+ col 1)) #\v)
                      (char=? (string-ref line (+ col 2)) #\u)
                      (char=? (string-ref line (+ col 3)) #\8))
                 ;; #vu8( bytevector - find the (
                 (let find-paren ([i (+ col 4)])
                   (if (and (< i len) (char=? (string-ref line i) #\())
                       (skip-balanced line i in-string)
                       ;; Should not happen in valid code
                       (values (min i len) in-string (>= i len))))]
                [(and (< (+ col 1) len)
                      (char=? (string-ref line (+ col 1)) #\\))
                 ;; Character literal #\x - skip it
                 (let ([next-col (+ col 2)])
                   (if (>= next-col len)
                       (values next-col in-string #t)
                       (let ([char-start (string-ref line next-col)])
                         (if (char-alphabetic? char-start)
                             ;; Named char like #\newline - scan to end
                             (let scan ([i (+ next-col 1)])
                               (if (or (>= i len)
                                       (not (char-alphabetic? (string-ref line i))))
                                   (values i in-string #f)
                                   (scan (+ i 1))))
                             ;; Simple char like #\( - just skip one more
                             (values (+ next-col 1) in-string #f)))))]
                [else
                 ;; Other # syntax (boolean #t/#f, etc.) - treat as atom
                 (skip-atom line col in-string)])]
             ;; String
             [(char=? c #\")
              (skip-string line (+ col 1))]
             ;; Quote/quasiquote/unquote prefixes - skip prefix then recurse
             [(or (char=? c #\') (char=? c #\`) (char=? c #\,))
              (let ([next-col (+ col 1)])
                ;; Check for ,@ (unquote-splicing)
                (if (and (char=? c #\,)
                         (< next-col len)
                         (char=? (string-ref line next-col) #\@))
                    (skip-datum line (+ col 2) in-string)
                    (skip-datum line next-col in-string)))]
             ;; Regular atom (symbol, number, etc.)
             [else
              (skip-atom line col in-string)]))]))))

;;; skip-balanced : String Int Bool -> (Values Int Bool Bool)
;;; Skip a balanced expression starting at an opener.
(define (skip-balanced line start-col in-string)
  (let ([len (string-length line)]
        [opener (string-ref line start-col)])
    (let loop ([col (+ start-col 1)]
               [depth 1]
               [in-str in-string])
      (cond
        [(>= col len)
         ;; End of line with unclosed parens - datum continues
         (values col in-str #t)]
        [(= depth 0)
         (values col in-str #f)]
        [else
         (let ([c (string-ref line col)])
           (cond
             ;; String handling
             [(and in-str (char=? c #\\))
              (loop (+ col 2) depth in-str)]
             [(and in-str (char=? c #\"))
              (loop (+ col 1) depth #f)]
             [in-str
              (loop (+ col 1) depth in-str)]
             [(char=? c #\")
              (loop (+ col 1) depth #t)]
             ;; Line comment - skip rest
             [(char=? c #\;)
              (values len in-str #t)]  ; Continue on next line
             ;; Nested openers
             [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
              (loop (+ col 1) (+ depth 1) in-str)]
             ;; Closers
             [(or (char=? c #\)) (char=? c #\]) (char=? c #\}))
              (loop (+ col 1) (- depth 1) in-str)]
             [else
              (loop (+ col 1) depth in-str)]))]))))

;;; skip-string : String Int -> (Values Int Bool Bool)
;;; Skip a string starting after the opening quote.
(define (skip-string line start-col)
  (let ([len (string-length line)])
    (let loop ([col start-col])
      (cond
        [(>= col len)
         ;; String continues on next line
         (values col #t #t)]
        [(char=? (string-ref line col) #\\)
         ;; Escape - skip next char
         (loop (+ col 2))]
        [(char=? (string-ref line col) #\")
         ;; End of string
         (values (+ col 1) #f #f)]
        [else
         (loop (+ col 1))]))))

;;; skip-atom : String Int Bool -> (Values Int Bool Bool)
;;; Skip an atom (symbol, number, etc.) until delimiter.
(define (skip-atom line start-col in-string)
  (let ([len (string-length line)])
    (let loop ([col start-col])
      (if (>= col len)
          (values col in-string #f)
          (let ([c (string-ref line col)])
            (if (or (char-whitespace? c)
                    (char=? c #\() (char=? c #\))
                    (char=? c #\[) (char=? c #\])
                    (char=? c #\{) (char=? c #\})
                    (char=? c #\") (char=? c #\;)
                    (char=? c #\') (char=? c #\`)
                    (char=? c #\,))
                (values col in-string #f)
                (loop (+ col 1))))))))

;;; skip-datum-with-depth : String Int Bool -> (Values Int Bool Int)
;;; Like skip-datum but returns paren depth instead of crossed-line boolean.
;;; Returns (new-col new-in-string remaining-depth)
;;; remaining-depth meanings:
;;;   0 = datum complete
;;;   -1 = waiting for datum to start (next line)
;;;   >0 = inside datum, tracking paren depth
(define (skip-datum-with-depth line start-col in-string)
  (let ([len (string-length line)])
    ;; Skip leading whitespace and comments
    (let skip-ws ([col start-col])
      (cond
        [(>= col len)
         ;; End of line before finding datum start
         ;; The datum must be on the next line - signal waiting
         (values col in-string -1)]  ; -1 = waiting for datum start
        [(char-whitespace? (string-ref line col))
         (skip-ws (+ col 1))]
        ;; Line comment - rest of line is comment, datum on next line
        [(char=? (string-ref line col) #\;)
         (values len in-string -1)]
        ;; Block comment #|...|# - skip it and continue looking
        [(and (char=? (string-ref line col) #\#)
              (< (+ col 1) len)
              (char=? (string-ref line (+ col 1)) #\|))
         ;; Skip the block comment (may span lines, but we only handle same-line for now)
         (let skip-block ([i (+ col 2)] [depth 1])
           (cond
             [(>= i len)
              ;; Block comment continues to next line - datum waiting
              (values i in-string -1)]
             [(and (< (+ i 1) len)
                   (char=? (string-ref line i) #\|)
                   (char=? (string-ref line (+ i 1)) #\#))
              (if (= depth 1)
                  ;; Block comment closed, continue skip-ws
                  (skip-ws (+ i 2))
                  ;; Nested block comment closed
                  (skip-block (+ i 2) (- depth 1)))]
             [(and (< (+ i 1) len)
                   (char=? (string-ref line i) #\#)
                   (char=? (string-ref line (+ i 1)) #\|))
              ;; Nested block comment
              (skip-block (+ i 2) (+ depth 1))]
             [else
              (skip-block (+ i 1) depth)]))]
        [else
         ;; Found start of datum
         (let ([c (string-ref line col)])
           (cond
             ;; List/vector opener - track balanced parens
             [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
              (skip-balanced-depth line (+ col 1) in-string 1)]
             ;; Vector #( or bytevector #vu8(
             [(char=? c #\#)
              (cond
                ;; #( vector
                [(and (< (+ col 1) len)
                      (char=? (string-ref line (+ col 1)) #\())
                 (skip-balanced-depth line (+ col 2) in-string 1)]
                ;; #vu8( bytevector
                [(and (< (+ col 4) len)
                      (char=? (string-ref line (+ col 1)) #\v)
                      (char=? (string-ref line (+ col 2)) #\u)
                      (char=? (string-ref line (+ col 3)) #\8)
                      (char=? (string-ref line (+ col 4)) #\())
                 (skip-balanced-depth line (+ col 5) in-string 1)]
                ;; #\ character literal - skip it
                [(and (< (+ col 1) len)
                      (char=? (string-ref line (+ col 1)) #\\))
                 (let ([char-pos (+ col 2)])
                   (if (>= char-pos len)
                       (values char-pos in-string 0)
                       (let ([char-start (string-ref line char-pos)])
                         (if (char-alphabetic? char-start)
                             ;; Named char like #\newline
                             (let scan ([i (+ char-pos 1)])
                               (if (or (>= i len)
                                       (not (char-alphabetic? (string-ref line i))))
                                   (values i in-string 0)
                                   (scan (+ i 1))))
                             ;; Simple char like #\(
                             (values (+ char-pos 1) in-string 0)))))]
                ;; #; datum comment - skip the commented datum, then find actual datum
                [(and (< (+ col 1) len)
                      (char=? (string-ref line (+ col 1)) #\;))
                 ;; Skip the #; and its target datum, then continue looking
                 (let-values ([(col1 str1 depth1)
                               (skip-datum-with-depth line (+ col 2) in-string)])
                   (if (not (= depth1 0))
                       ;; Commented datum continues to next line
                       ;; Encode nested state: -1000 + inner-depth
                       ;; -1001 = waiting for inner datum (-1)
                       ;; -999 = inside parens in inner datum (depth 1)
                       (values col1 str1 (- depth1 1000))
                       ;; Commented datum complete, now find actual datum
                       (skip-datum-with-depth line col1 str1)))]
                ;; Other # syntax (boolean #t/#f, etc.) - atom
                [else
                 (let-values ([(new-col new-str _) (skip-atom line col in-string)])
                   (values new-col new-str 0))])]
             ;; String
             [(char=? c #\")
              (let-values ([(new-col new-str crossed) (skip-string line (+ col 1))])
                (if crossed
                    (values new-col new-str 1)  ; String continues - need to finish it
                    (values new-col new-str 0)))]
             ;; Quote prefix - skip and recurse
             [(or (char=? c #\') (char=? c #\`) (char=? c #\,))
              (let ([next-col (+ col 1)])
                (if (and (char=? c #\,)
                         (< next-col len)
                         (char=? (string-ref line next-col) #\@))
                    (skip-datum-with-depth line (+ col 2) in-string)
                    (skip-datum-with-depth line next-col in-string)))]
             ;; Atom (symbol, number, etc.) - single line only
             [else
              (let-values ([(new-col new-str _) (skip-atom line col in-string)])
                (values new-col new-str 0))]))]))))

;;; skip-balanced-depth : String Int Bool Int -> (Values Int Bool Int)
;;; Skip balanced parens and return remaining depth.
(define (skip-balanced-depth line start-col in-string depth)
  (let ([len (string-length line)])
    (let loop ([col start-col]
               [d depth]
               [in-str in-string])
      (cond
        [(>= col len)
         ;; End of line - return remaining depth
         (values col in-str d)]
        [(= d 0)
         ;; Balanced - done
         (values col in-str 0)]
        [else
         (let ([c (string-ref line col)])
           (cond
             ;; String handling
             [(and in-str (char=? c #\\))
              (loop (+ col 2) d in-str)]
             [(and in-str (char=? c #\"))
              (loop (+ col 1) d #f)]
             [in-str
              (loop (+ col 1) d in-str)]
             [(char=? c #\")
              (loop (+ col 1) d #t)]
             ;; Line comment - continue on next line
             [(char=? c #\;)
              (values len in-str d)]
             ;; Nested opener
             [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
              (loop (+ col 1) (+ d 1) in-str)]
             ;; Closer
             [(or (char=? c #\)) (char=? c #\]) (char=? c #\}))
              (loop (+ col 1) (- d 1) in-str)]
             [else
              (loop (+ col 1) d in-str)]))]))))

;;; process-line : String Int Stack Errors Bool Int -> (Values Stack Errors Bool Int)
;;; Process a single line, updating stack and collecting errors.
(define (process-line line line-num stack errors in-string in-block-comment in-datum-skip)
  (let char-loop ([col 0]
                  [stack stack]
                  [errors errors]
                  [in-string in-string]
                  [in-block in-block-comment]
                  [in-datum in-datum-skip])  ; paren depth in datum being skipped
    (if (>= col (string-length line))
        (values stack errors in-string in-block in-datum)
        (let ([c (string-ref line col)]
              [next-c (if (< (+ col 1) (string-length line))
                          (string-ref line (+ col 1))
                          #f)])
          (cond
            ;; Nested #; continuation: skipping inner datum, then need actual datum
            ;; Encoding: in-datum = inner-depth - 1000
            ;; So -1001 = inner waiting (-1), -999 = inner depth 1, etc.
            [(< in-datum -900)
             (let ([inner-depth (+ in-datum 1000)])
               (cond
                 ;; Inner depth -1 means waiting for inner datum to start
                 [(= inner-depth -1)
                  (cond
                    [(char-whitespace? c)
                     (char-loop (+ col 1) stack errors in-string in-block in-datum)]
                    ;; Line comment while waiting - continue to next line
                    [(char=? c #\;)
                     (values stack errors in-string in-block in-datum)]
                    ;; Found start of inner datum - process it
                    [else
                     (let-values ([(new-col new-str new-depth)
                                   (skip-datum-with-depth line col in-string)])
                       (if (= new-depth 0)
                           ;; Inner datum complete, now find actual datum
                           (char-loop new-col stack errors new-str in-block -1)
                           ;; Inner datum continues
                           (values stack errors new-str in-block (- new-depth 1000))))])]
                 ;; Inner depth > 0 means inside parens in inner datum
                 [(> inner-depth 0)
                  (let-values ([(new-col new-str new-depth)
                                (skip-balanced-depth line col in-string inner-depth)])
                    (if (= new-depth 0)
                        ;; Inner datum complete, now find actual datum
                        (char-loop new-col stack errors new-str in-block -1)
                        ;; Inner datum continues
                        (values stack errors new-str in-block (- new-depth 1000))))]
                 [else
                  ;; Shouldn't happen
                  (char-loop (+ col 1) stack errors in-string in-block in-datum)]))]

            ;; Waiting for datum to start (saw #; but datum is on next line)
            ;; in-datum = -1 means waiting for the datum to begin
            [(= in-datum -1)
             (cond
               ;; Skip whitespace
               [(char-whitespace? c)
                (char-loop (+ col 1) stack errors in-string in-block -1)]
               ;; Opener starts a list/vector - now track depth
               [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
                (char-loop (+ col 1) stack errors in-string in-block 1)]
               ;; String starts - skip it, datum complete when string ends
               [(char=? c #\")
                (let-values ([(new-col new-str crossed) (skip-string line (+ col 1))])
                  (if crossed
                      ;; String continues to next line
                      (values stack errors #t in-block 1)  ; Use 1 to track string continuation
                      ;; String complete - datum done
                      (char-loop new-col stack errors #f in-block 0)))]
               ;; Quote prefix - skip and still waiting for actual datum
               [(or (char=? c #\') (char=? c #\`) (char=? c #\,))
                (let ([next-col (+ col 1)])
                  (if (and (char=? c #\,)
                           (< next-col (string-length line))
                           (char=? (string-ref line next-col) #\@))
                      (char-loop (+ col 2) stack errors in-string in-block -1)
                      (char-loop next-col stack errors in-string in-block -1)))]
               ;; #-prefixed forms
               [(char=? c #\#)
                (cond
                  ;; Vector #(
                  [(and next-c (char=? next-c #\())
                   (char-loop (+ col 2) stack errors in-string in-block 1)]
                  ;; Bytevector #vu8(
                  [(and (< (+ col 4) (string-length line))
                        (char=? (string-ref line (+ col 1)) #\v)
                        (char=? (string-ref line (+ col 2)) #\u)
                        (char=? (string-ref line (+ col 3)) #\8)
                        (char=? (string-ref line (+ col 4)) #\())
                   (char-loop (+ col 5) stack errors in-string in-block 1)]
                  ;; Character literal #\x - atom, datum complete
                  [(and next-c (char=? next-c #\\))
                   (let ([char-pos (+ col 2)])
                     (if (>= char-pos (string-length line))
                         (char-loop char-pos stack errors in-string in-block 0)
                         (let ([char-start (string-ref line char-pos)])
                           (if (char-alphabetic? char-start)
                               ;; Named char like #\newline
                               (let scan ([i (+ char-pos 1)])
                                 (if (or (>= i (string-length line))
                                         (not (char-alphabetic? (string-ref line i))))
                                     (char-loop i stack errors in-string in-block 0)
                                     (scan (+ i 1))))
                               ;; Simple char like #\(
                               (char-loop (+ char-pos 1) stack errors in-string in-block 0)))))]
                  ;; #; datum comment while waiting - skip its target, keep waiting
                  [(and next-c (char=? next-c #\;))
                   (let-values ([(new-col new-str depth)
                                 (skip-datum-with-depth line (+ col 2) in-string)])
                     (if (not (= depth 0))
                         ;; Commented datum continues - track it
                         (values stack errors new-str in-block (- depth 1000))
                         ;; Commented datum done, still waiting for actual datum
                         (char-loop new-col stack errors new-str in-block -1)))]
                  ;; Other # syntax (boolean #t/#f) - atom, datum complete
                  [else
                   (let-values ([(new-col new-str _) (skip-atom line col in-string)])
                     (char-loop new-col stack errors new-str in-block 0))])]
               ;; Line comment - still waiting on next line
               [(char=? c #\;)
                (values stack errors in-string in-block -1)]
               ;; Any other character - it's an atom, skip it and datum complete
               [else
                (let-values ([(new-col new-str _) (skip-atom line col in-string)])
                  (char-loop new-col stack errors new-str in-block 0))])]

            ;; Continuing datum skip from previous line (inside balanced parens)
            ;; in-datum > 0 means we're tracking paren depth inside a list/vector
            [(> in-datum 0)
             (cond
               ;; String handling within datum
               [(and in-string (char=? c #\\))
                (char-loop (+ col 2) stack errors in-string in-block in-datum)]
               [(and in-string (char=? c #\"))
                (char-loop (+ col 1) stack errors #f in-block in-datum)]
               [in-string
                (char-loop (+ col 1) stack errors in-string in-block in-datum)]
               [(char=? c #\")
                (char-loop (+ col 1) stack errors #t in-block in-datum)]
               ;; Nested openers in datum - increase depth
               [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
                (char-loop (+ col 1) stack errors in-string in-block (+ in-datum 1))]
               ;; Closers in datum - decrease depth
               [(or (char=? c #\)) (char=? c #\]) (char=? c #\}))
                (let ([new-depth (- in-datum 1)])
                  (if (= new-depth 0)
                      ;; Datum complete - continue normal processing
                      (char-loop (+ col 1) stack errors in-string in-block 0)
                      (char-loop (+ col 1) stack errors in-string in-block new-depth)))]
               ;; Line comment within datum - skip rest of line
               [(char=? c #\;)
                (values stack errors in-string in-block in-datum)]
               [else
                (char-loop (+ col 1) stack errors in-string in-block in-datum)])]

            ;; Inside block comment
            [(> in-block 0)
             (cond
               ;; End block comment
               [(and (char=? c #\|) next-c (char=? next-c #\#))
                (char-loop (+ col 2) stack errors in-string (- in-block 1) in-datum)]
               ;; Nested block comment
               [(and (char=? c #\#) next-c (char=? next-c #\|))
                (char-loop (+ col 2) stack errors in-string (+ in-block 1) in-datum)]
               [else
                (char-loop (+ col 1) stack errors in-string in-block in-datum)])]

            ;; String handling
            [(and in-string (char=? c #\\))
             ;; Escape - skip next char
             (char-loop (+ col 2) stack errors in-string in-block in-datum)]
            [(and in-string (char=? c #\"))
             ;; End string
             (char-loop (+ col 1) stack errors #f in-block in-datum)]
            [in-string
             ;; Inside string - skip
             (char-loop (+ col 1) stack errors in-string in-block in-datum)]
            [(char=? c #\")
             ;; Start string
             (char-loop (+ col 1) stack errors #t in-block in-datum)]

            ;; Datum comment #; - skip the next complete S-expression
            ;; Must check before line comment since #; starts with # not ;
            [(and (char=? c #\#) next-c (char=? next-c #\;))
             (let-values ([(new-col new-in-string datum-depth)
                           (skip-datum-with-depth line (+ col 2) in-string)])
               (if (not (= datum-depth 0))
                   ;; Datum extends beyond this line - track state for continuation
                   ;; datum-depth = -1 means waiting for datum, >0 means inside parens
                   (values stack errors new-in-string in-block datum-depth)
                   ;; Datum complete on this line
                   (char-loop new-col stack errors new-in-string in-block 0)))]

            ;; Line comment - skip rest of line
            [(char=? c #\;)
             (values stack errors in-string in-block in-datum)]

            ;; Pipe-delimited symbol |foo(bar)| - skip to closing pipe
            ;; These can contain parens that shouldn't be counted
            [(char=? c #\|)
             (let scan-pipe ([i (+ col 1)])
               (cond
                 [(>= i (string-length line))
                  ;; Unclosed pipe symbol on this line - continue to next line
                  ;; For simplicity, just skip to end of line
                  (values stack errors in-string in-block in-datum)]
                 [(char=? (string-ref line i) #\|)
                  ;; Found closing pipe
                  (char-loop (+ i 1) stack errors in-string in-block in-datum)]
                 [else
                  (scan-pipe (+ i 1))]))]

            ;; Block comment start
            [(and (char=? c #\#) next-c (char=? next-c #\|))
             (char-loop (+ col 2) stack errors in-string (+ in-block 1) in-datum)]

            ;; Character literal - #\( is not an opener
            ;; Handle both simple (#\x) and named (#\newline, #\space) forms
            [(and (char=? c #\#) next-c (char=? next-c #\\))
             (let ([char-after (if (< (+ col 2) (string-length line))
                                   (string-ref line (+ col 2))
                                   #f)])
               (if (and char-after (char-alphabetic? char-after))
                   ;; Named character literal - scan to end of name
                   (let scan-name ([end (+ col 3)])
                     (if (and (< end (string-length line))
                              (char-alphabetic? (string-ref line end)))
                         (scan-name (+ end 1))
                         (char-loop end stack errors in-string in-block in-datum)))
                   ;; Simple character literal like #\( or #\x
                   (char-loop (+ col 3) stack errors in-string in-block in-datum)))]

            ;; Openers
            [(char->opener-type c)
             => (lambda (type)
                  (let ([indent (line-indent line)]
                        [form (detect-form line col)])
                    (char-loop (+ col 1)
                               (cons (make-opener type line-num col indent form) stack)
                               errors
                               in-string
                               in-block
                               in-datum)))]

            ;; Closers
            [(char->closer-type c)
             => (lambda (close-type)
                  (cond
                    ;; Stack empty - extra closer
                    [(null? stack)
                     (char-loop (+ col 1)
                                stack
                                (cons (make-paren-error
                                        'extra-close
                                        (format "unexpected '~a' - no matching opener"
                                                (type->close-char close-type))
                                        line-num col)
                                      errors)
                                in-string
                                in-block
                                in-datum)]
                    ;; Mismatch
                    [(not (eq? (opener-type (car stack)) close-type))
                     (let ([opener (car stack)])
                       (char-loop (+ col 1)
                                  (cdr stack)  ; pop anyway to continue
                                  (cons (make-paren-error
                                          'mismatch
                                          (format "mismatched brackets: opened '~a' at line ~a col ~a, closed with '~a'"
                                                  (type->open-char (opener-type opener))
                                                  (opener-line opener)
                                                  (+ (opener-col opener) 1)  ; 1-indexed for display
                                                  (type->close-char close-type))
                                          line-num col
                                          opener)
                                        errors)
                                  in-string
                                  in-block
                                  in-datum))]
                    ;; Match - pop stack
                    [else
                     (char-loop (+ col 1)
                                (cdr stack)
                                errors
                                in-string
                                in-block
                                in-datum)]))]

            ;; Other characters
            [else
             (char-loop (+ col 1) stack errors in-string in-block in-datum)])))))

;;; paren-errors : String -> (List Error)
;;; Return list of paren errors in file.
(define (paren-errors path)
  (let-values ([(_ errors) (paren-stack-analyze path)])
    errors))

;;; paren-locate : String -> void
;;; Print detailed location info for paren errors.
(define (paren-locate path)
  (let ([errors (paren-errors path)])
    (printf "\n~a\n" (make-string 70 #\─))
    (printf "Paren Location Report: ~a\n" path)
    (printf "~a\n\n" (make-string 70 #\─))

    (if (null? errors)
        (printf "✓ All parentheses balanced!\n\n")
        (begin
          (printf "Found ~a error(s):\n\n" (length errors))
          (for-each
            (lambda (err)
              (let ([kind (paren-error-kind err)]
                    [msg (paren-error-message err)]
                    [line (paren-error-line err)]
                    [col (paren-error-col err)])
                ;; Print in compiler-friendly format
                (printf "~a:~a:~a: ~a: ~a\n"
                        path line (+ col 1)  ; 1-indexed for editors
                        (case kind
                          [(unclosed) "error"]
                          [(extra-close) "error"]
                          [(mismatch) "error"])
                        msg)))
            errors)
          (printf "\n")))

    (printf "~a\n" (make-string 70 #\─))))

;;; paren-ok? : String -> Boolean
;;; Quick check - returns #t if file has balanced parens.
(define (paren-ok? path)
  (null? (paren-errors path)))

;;; ====
;;; Indentation-Based Suggestions
;;; ====

;;; group-by-indent : (List Opener) -> (Alist Int (List Opener))
;;; Group openers by their indentation level.
(define (group-by-indent openers)
  (let loop ([ops openers] [groups '()])
    (if (null? ops)
        (reverse groups)
        (let* ([op (car ops)]
               [indent (opener-indent op)]
               [existing (assv indent groups)])
          (if existing
              (loop (cdr ops)
                    (cons (cons indent (cons op (cdr existing)))
                          (filter (lambda (g) (not (= (car g) indent))) groups)))
              (loop (cdr ops)
                    (cons (cons indent (list op)) groups)))))))

;;; analyze-indent-pattern : String -> (List (Line Indent Opens Closes))
;;; Analyze the indentation pattern of a file.
(define (analyze-indent-pattern path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([line-num 1] [results '()])
        (let ([line (get-line port)])
          (if (eof-object? line)
              (reverse results)
              (let-values ([(opens closes _) (count-parens line)])
                (let ([indent (line-indent line)])
                  (loop (+ line-num 1)
                        (cons (list line-num indent opens closes) results))))))))))

;;; suggest-missing-closers : (List Opener) Int -> String
;;; Generate suggestion text for unclosed openers based on indentation.
(define (suggest-missing-closers unclosed-stack total-lines)
  (if (null? unclosed-stack)
      ""
      (let* ([groups (group-by-indent unclosed-stack)]
             [sorted (sort (lambda (a b) (< (car a) (car b))) groups)])
        (with-output-to-string
          (lambda ()
            (printf "\n  Indentation analysis:\n")
            (for-each
              (lambda (group)
                (let ([indent (car group)]
                      [openers (cdr group)])
                  (printf "    Indent ~a: ~a unclosed opener(s) from line(s) ~a\n"
                          indent
                          (length openers)
                          (string-join
                            (map (lambda (o)
                                   (if (opener-form o)
                                       (format "~a (~a)" (opener-line o) (opener-form o))
                                       (number->string (opener-line o))))
                                 (reverse openers))
                            ", "))))
              sorted)
            (printf "\n  Suggestion: Add ~a closer(s). Look for forms starting at:\n"
                    (length unclosed-stack))
            ;; Show the most likely culprits (outermost/earliest unclosed)
            (let ([earliest (take-up-to 5 (reverse unclosed-stack))])
              (for-each
                (lambda (o)
                  (if (opener-form o)
                      (printf "    - Line ~a: ~a (~a) at column ~a\n"
                              (opener-line o)
                              (opener-form o)
                              (type->open-char (opener-type o))
                              (+ (opener-col o) 1))
                      (printf "    - Line ~a: '~a' at column ~a\n"
                              (opener-line o)
                              (type->open-char (opener-type o))
                              (+ (opener-col o) 1))))
                earliest)))))))

;;; take-up-to : Int (List a) -> (List a)
(define (take-up-to n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-up-to (- n 1) (cdr lst)))))

;; string-join is provided by prelude

;;; paren-suggest : String -> void
;;; Analyze file and provide indentation-based suggestions for fixing imbalances.
(define (paren-suggest path)
  (let-values ([(final-stack errors) (paren-stack-analyze path)])
    (printf "\n~a\n" (make-string 70 #\─))
    (printf "Paren Suggestion Report: ~a\n" path)
    (printf "~a\n" (make-string 70 #\─))

    (cond
      [(and (null? errors) (null? final-stack))
       (printf "\n✓ All parentheses balanced!\n")]

      [else
       ;; Show errors first
       (when (not (null? errors))
         (printf "\nErrors found:\n")
         (for-each
           (lambda (err)
             (printf "  ~a:~a: ~a\n"
                     (paren-error-line err)
                     (+ (paren-error-col err) 1)
                     (paren-error-message err)))
           errors))

       ;; Analyze the pattern file to find where closers might be missing
       (let ([pattern (analyze-indent-pattern path)]
             [balance (paren-balance path)])
         (cond
           [(> balance 0)
            ;; More opens than closes - suggest where to add closers
            (printf "\n~a unclosed opener(s) detected.\n" balance)
            ;; Get the remaining stack from a fresh analysis
            (let-values ([(stack _) (paren-stack-analyze path)])
              (printf "~a" (suggest-missing-closers stack (length pattern))))]

           [(< balance 0)
            ;; More closes than opens - find where extra closers are
            (printf "\n~a extra closer(s) detected.\n" (- balance))
            (printf "\n  Look for lines with unexpected ')' or ']' or '}'.\n")
            ;; Find lines where balance goes negative
            (let ([imbalanced (filter (lambda (info) (< (cadddr info) 0))
                                      (analyze-file path))])
              (when (not (null? imbalanced))
                (printf "  First imbalance at line ~a\n" (car (car imbalanced)))))]

           [else
            ;; Balance is 0 but there are mismatch errors
            (printf "\n  Bracket types are mismatched.\n")]))])

    (printf "\n~a\n" (make-string 70 #\─))))

;;; paren-summary : String -> void
;;; Quick summary with just the key info.
(define (paren-summary path)
  (let ([balance (paren-balance path)]
        [errors (paren-errors path)])
    (cond
      [(and (= balance 0) (null? errors))
       (printf "✓ ~a: balanced\n" path)]
      [(> balance 0)
       (printf "✗ ~a: ~a unclosed opener(s)\n" path balance)]
      [(< balance 0)
       (printf "✗ ~a: ~a extra closer(s)\n" path (- balance))]
      [else
       (printf "✗ ~a: ~a error(s)\n" path (length errors))])))
