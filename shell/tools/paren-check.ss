;;; shell/paren-check.ss — Parenthesis Balance Checker
;;;
;;; Analyzes Scheme files for parenthesis balance issues.
;;; Reports running balance per line and highlights imbalances.
;;;
;;; Usage:
;;;   (paren-check "path/to/file.ss")           ; Full report
;;;   (paren-check "path/to/file.ss" 100 150)   ; Lines 100-150 only
;;;   (paren-balance "path/to/file.ss")         ; Just the final balance
;;;   (paren-locate "path/to/file.ss")          ; Stack-based: exact error locations
;;;   (paren-errors "path/to/file.ss")          ; Returns list of error structs
;;;
;;; This is Shell code: file IO for analysis.
;;;
;;; NOTE: string-trim provided by core/prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Core Balance Calculation
;;; ====

;;; count-parens : String -> (Values Int Int Int)
;;; Count open and close parens in a string.
;;; Returns (opens closes balance-delta).
;;; Handles (, ), [, ], {, } and ignores chars in strings/comments.
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

;;; An opener is (type line col) where type is 'paren, 'bracket, or 'brace
(define (make-opener type line col)
  (list type line col))
(define (opener-type o) (car o))
(define (opener-line o) (cadr o))
(define (opener-col o) (caddr o))

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

;;; paren-stack-analyze : String -> (Values Stack Errors)
;;; Analyze file with stack tracking. Returns final stack and list of errors.
(define (paren-stack-analyze path)
  (call-with-input-file path
    (lambda (port)
      (let line-loop ([line-num 1]
                      [stack '()]
                      [errors '()]
                      [in-string #f]
                      [in-block-comment 0])  ; nesting depth for #|...|#
        (let ([line (get-line port)])
          (if (eof-object? line)
              ;; EOF - check for unclosed items
              (let* ([unclosed-errors
                      (map (lambda (opener)
                             (make-paren-error
                               'unclosed
                               (format "unclosed '~a' - never closed"
                                       (type->open-char (opener-type opener)))
                               (opener-line opener)
                               (opener-col opener)))
                           (reverse stack))]
                     ;; Also check for unterminated block comments
                     [block-error
                      (if (> in-block-comment 0)
                          (list (make-paren-error
                                  'unclosed
                                  "unterminated block comment #|...|#"
                                  line-num 0))  ; line-num is EOF position
                          '())]
                     ;; And unterminated strings
                     [string-error
                      (if in-string
                          (list (make-paren-error
                                  'unclosed
                                  "unterminated string literal"
                                  line-num 0))
                          '())])
                (values '() (append (reverse errors) unclosed-errors block-error string-error)))
              ;; Process this line
              (let-values ([(new-stack new-errors new-in-string new-in-block)
                            (process-line line line-num stack errors
                                          in-string in-block-comment)])
                (line-loop (+ line-num 1)
                           new-stack
                           new-errors
                           new-in-string
                           new-in-block))))))))

;;; process-line : String Int Stack Errors Bool Int -> (Values Stack Errors Bool Int)
;;; Process a single line, updating stack and collecting errors.
(define (process-line line line-num stack errors in-string in-block-comment)
  (let char-loop ([col 0]
                  [stack stack]
                  [errors errors]
                  [in-string in-string]
                  [in-block in-block-comment])
    (if (>= col (string-length line))
        (values stack errors in-string in-block)
        (let ([c (string-ref line col)]
              [next-c (if (< (+ col 1) (string-length line))
                          (string-ref line (+ col 1))
                          #f)])
          (cond
            ;; Inside block comment
            [(> in-block 0)
             (cond
               ;; End block comment
               [(and (char=? c #\|) next-c (char=? next-c #\#))
                (char-loop (+ col 2) stack errors in-string (- in-block 1))]
               ;; Nested block comment
               [(and (char=? c #\#) next-c (char=? next-c #\|))
                (char-loop (+ col 2) stack errors in-string (+ in-block 1))]
               [else
                (char-loop (+ col 1) stack errors in-string in-block)])]

            ;; String handling
            [(and in-string (char=? c #\\))
             ;; Escape - skip next char
             (char-loop (+ col 2) stack errors in-string in-block)]
            [(and in-string (char=? c #\"))
             ;; End string
             (char-loop (+ col 1) stack errors #f in-block)]
            [in-string
             ;; Inside string - skip
             (char-loop (+ col 1) stack errors in-string in-block)]
            [(char=? c #\")
             ;; Start string
             (char-loop (+ col 1) stack errors #t in-block)]

            ;; Line comment - skip rest of line
            [(char=? c #\;)
             (values stack errors in-string in-block)]

            ;; Pipe-delimited symbol |foo(bar)| - skip to closing pipe
            ;; These can contain parens that shouldn't be counted
            [(char=? c #\|)
             (let scan-pipe ([i (+ col 1)])
               (cond
                 [(>= i (string-length line))
                  ;; Unclosed pipe symbol on this line - continue to next line
                  ;; For simplicity, just skip to end of line
                  (values stack errors in-string in-block)]
                 [(char=? (string-ref line i) #\|)
                  ;; Found closing pipe
                  (char-loop (+ i 1) stack errors in-string in-block)]
                 [else
                  (scan-pipe (+ i 1))]))]

            ;; Block comment start
            [(and (char=? c #\#) next-c (char=? next-c #\|))
             (char-loop (+ col 2) stack errors in-string (+ in-block 1))]

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
                         (char-loop end stack errors in-string in-block)))
                   ;; Simple character literal like #\( or #\x
                   (char-loop (+ col 3) stack errors in-string in-block)))]

            ;; Openers
            [(char->opener-type c)
             => (lambda (type)
                  (char-loop (+ col 1)
                             (cons (make-opener type line-num col) stack)
                             errors
                             in-string
                             in-block))]

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
                                in-block)]
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
                                  in-block))]
                    ;; Match - pop stack
                    [else
                     (char-loop (+ col 1)
                                (cdr stack)
                                errors
                                in-string
                                in-block)]))]

            ;; Other characters
            [else
             (char-loop (+ col 1) stack errors in-string in-block)])))))

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
