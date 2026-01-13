;;; shell/tools/template-parser.ss — Linear Syntax Parser for Templates
;;; @module template-parser
;;; @requires shell/tools/template-session
;;;
;;; Parses the EBNF-like linear syntax into template operations.
;;;
;;; Syntax:
;;;   define $sig $body           → (ts-start '(define $sig $body))
;;;   $name := factorial          → (ts-fill '$name 'factorial)
;;;   $body := if $c $t $e        → (ts-fill '$body '(if $c $t $e))
;;;   $cond := = n 0              → (ts-fill '$cond '(= n 0))  ; implicit parens
;;;
;;; Rules:
;;;   1. Line starting with "$name :=" is a fill operation
;;;   2. Otherwise, line is a new template definition
;;;   3. Tokenize: split on whitespace, preserve parenthesized groups
;;;   4. Implicit parens: if >1 token, wrap in ()
;;;   5. Single token stays as-is
;;;
;;; This is Shell code: impure, handles IO.

(load "shell/tools/template-session.ss")

;;; ============================================================
;;; Tokenizer
;;; ============================================================

;;; tokenize : String → (List Token)
;;; Split string into tokens, preserving parenthesized groups.
;;; Tokens are either atoms (strings) or nested lists (parenthesized groups).
(define (tokenize str)
  (let ([chars (string->list str)]
        [tokens '()]
        [current '()])
    (define (flush-current!)
      (when (pair? current)
        (set! tokens (cons (list->string (reverse current)) tokens))
        (set! current '())))
    (define (read-paren chars depth acc)
      ;; Read until matching close paren
      (cond
        [(null? chars)
         (error 'tokenize "Unmatched open parenthesis")]
        [(char=? (car chars) #\()
         (read-paren (cdr chars) (+ depth 1) (cons #\( acc))]
        [(char=? (car chars) #\))
         (if (= depth 1)
             (values (cdr chars) (list->string (reverse (cons #\) acc))))
             (read-paren (cdr chars) (- depth 1) (cons #\) acc)))]
        [else
         (read-paren (cdr chars) depth (cons (car chars) acc))]))
    (let loop ([chars chars])
      (cond
        [(null? chars)
         (flush-current!)
         (reverse tokens)]
        [(char-whitespace? (car chars))
         (flush-current!)
         (loop (cdr chars))]
        [(char=? (car chars) #\()
         (flush-current!)
         (let-values ([(rest paren-str) (read-paren (cdr chars) 1 '(#\())])
           (set! tokens (cons paren-str tokens))
           (loop rest))]
        [else
         (set! current (cons (car chars) current))
         (loop (cdr chars))]))))

;;; ============================================================
;;; Token to S-expression Conversion
;;; ============================================================

;;; token->sexpr : String → Sexpr
;;; Convert a token string to an S-expression.
;;; Handles atoms, numbers, and parenthesized expressions.
(define (token->sexpr token)
  (cond
    [(string=? token "") #f]
    [(char=? (string-ref token 0) #\()
     ;; Parenthesized - parse as S-expression
     (read (open-input-string token))]
    [(string->number token)
     => (lambda (n) n)]
    [else
     ;; Bare atom - convert to symbol
     (string->symbol token)]))

;;; tokens->sexpr : (List String) → Sexpr
;;; Convert tokens to S-expression, applying implicit parens rule.
(define (tokens->sexpr tokens)
  (let ([sexprs (map token->sexpr tokens)])
    (cond
      [(null? sexprs) '()]
      [(null? (cdr sexprs)) (car sexprs)]  ; Single token
      [else sexprs])))  ; Multiple tokens → list (implicit parens)

;;; ============================================================
;;; Line Parsing
;;; ============================================================

;;; parse-assignment : String → (values Symbol Sexpr) | #f
;;; Try to parse a line as "$name := value".
;;; Returns (values hole-sym value-sexpr) or #f if not an assignment.
(define (parse-assignment line)
  (let ([tokens (tokenize line)])
    (and (>= (length tokens) 3)
         (let ([first (car tokens)]
               [second (cadr tokens)])
           (and (> (string-length first) 0)
                (char=? (string-ref first 0) #\$)
                (string=? second ":=")
                (let ([hole-sym (string->symbol first)]
                      [value-tokens (cddr tokens)])
                  (values hole-sym (tokens->sexpr value-tokens))))))))

;;; parse-template-line : String → Sexpr
;;; Parse a line as a template definition.
(define (parse-template-line line)
  (tokens->sexpr (tokenize line)))

;;; ============================================================
;;; Main Parser Interface
;;; ============================================================

;;; tp-parse : String → Unit
;;; Parse a line and execute the appropriate template operation.
(define (tp-parse line)
  (let ([trimmed (string-trim line)])
    (cond
      [(string=? trimmed "") #f]  ; Empty line
      [(string=? trimmed "!") (ts-compile)]  ; Compile shorthand
      [(string=? trimmed "?") (t?)]  ; Status shorthand
      [(string=? trimmed "undo") (ts-undo)]  ; Undo command
      [(string=? trimmed "reset") (ts-reset)]  ; Reset command
      [else
       (call-with-values
        (lambda () (parse-assignment trimmed))
        (lambda results
          (if (and (pair? results) (car results))
              ;; It's an assignment: $name := value
              (ts-fill (car results) (cadr results))
              ;; It's a template definition
              (if (ts-active?)
                  (display "Session already active. Use 'reset' to start over.\n")
                  (ts-start (parse-template-line trimmed))))))])))

;;; tp-eval : String → Sexpr | #f
;;; Parse and evaluate a line, returning the result if compile.
(define (tp-eval line)
  (let ([trimmed (string-trim line)])
    (cond
      [(string=? trimmed "!") (ts-compile)]
      [else (tp-parse line) #f])))

;;; ============================================================
;;; String Utilities
;;; ============================================================

;;; string-trim : String → String
;;; Remove leading/trailing whitespace.
(define (string-trim str)
  (let* ([chars (string->list str)]
         [trimmed-front (drop-while char-whitespace? chars)]
         [trimmed-back (reverse (drop-while char-whitespace? (reverse trimmed-front)))])
    (list->string trimmed-back)))

;;; drop-while : (α → Bool) × (List α) → (List α)
(define (drop-while pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (drop-while pred (cdr lst))]
    [else lst]))

;;; ============================================================
;;; Interactive Mode
;;; ============================================================

;;; tp-repl : → Unit
;;; Simple REPL for template construction.
(define (tp-repl)
  (display "Template REPL (type 'help' for commands, 'quit' to exit)\n")
  (let loop ()
    (display "> ")
    (let ([line (get-line (current-input-port))])
      (cond
        [(eof-object? line) (newline)]
        [(string=? (string-trim line) "quit") (display "Bye!\n")]
        [(string=? (string-trim line) "help")
         (display "Commands:\n")
         (display "  <expr>         - Start new template\n")
         (display "  $hole := val   - Fill a hole\n")
         (display "  !              - Compile\n")
         (display "  ?              - Show status\n")
         (display "  undo           - Undo last fill\n")
         (display "  reset          - Clear session\n")
         (display "  quit           - Exit REPL\n")
         (loop)]
        [else
         (guard (exn [else (display "Error: ")
                           (display (if (condition? exn)
                                        (condition-message exn)
                                        exn))
                           (newline)])
                (tp-parse line))
         (loop)]))))
