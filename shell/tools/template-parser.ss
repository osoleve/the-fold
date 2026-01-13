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
;;;   3. Uses Scheme's `read` for proper parsing of strings, booleans, etc.
;;;   4. Implicit parens: if >1 token, wrap in ()
;;;   5. Single token stays as-is
;;;
;;; This is Shell code: impure, handles IO.

(load "shell/tools/template-session.ss")

;;; ============================================================
;;; Tokenizer (Read-based)
;;; ============================================================

;;; tokenize : String → (List Sexpr)
;;; Parse string into a list of S-expressions using Scheme's reader.
;;; Handles strings, booleans, quoted forms, etc. correctly.
(define (tokenize str)
  (let ([port (open-input-string str)])
    (let loop ([acc '()])
      (let ([datum (read port)])
        (if (eof-object? datum)
            (reverse acc)
            (loop (cons datum acc)))))))

;;; ============================================================
;;; Implicit Parens
;;; ============================================================

;;; apply-implicit-parens : (List Sexpr) → Sexpr
;;; If list has >1 element, wrap in parens (make a list).
;;; Single element stays as-is.
(define (apply-implicit-parens tokens)
  (cond
    [(null? tokens) '()]
    [(null? (cdr tokens)) (car tokens)]  ; Single token
    [else tokens]))  ; Multiple tokens → list (implicit parens)

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
           (and (symbol? first)
                (hole? first)  ; Check if it's a $-prefixed symbol
                (eq? second ':=)
                (let ([hole-sym first]
                      [value-tokens (cddr tokens)])
                  (values hole-sym (apply-implicit-parens value-tokens))))))))

;;; parse-template-line : String → Sexpr
;;; Parse a line as a template definition.
(define (parse-template-line line)
  (apply-implicit-parens (tokenize line)))

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
       (guard (exn [else
                    (display "Parse error: ")
                    (display (if (condition? exn)
                                 (condition-message exn)
                                 exn))
                    (newline)])
         (call-with-values
          (lambda () (parse-assignment trimmed))
          (lambda results
            (if (and (pair? results) (car results))
                ;; It's an assignment: $name := value
                (ts-fill (car results) (cadr results))
                ;; It's a template definition
                (if (ts-active?)
                    (display "Session already active. Use 'reset' to start over.\n")
                    (ts-start (parse-template-line trimmed)))))))])))

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
