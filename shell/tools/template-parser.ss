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

;;; tp-batch : String → Expr
;;; Parse template + fills or multiple definitions, separated by ---.
;;;
;;; Two modes:
;;;   1. Hole-filling: First section is template, rest are $hole := value fills
;;;      "define $sig $body --- $sig := foo x --- $body := + x 1"
;;;      → (define (foo x) (+ x 1))
;;;
;;;   2. Complete definitions: All sections are hole-free expressions
;;;      "define (f x) (+ x 1) --- define (g x) (f x)"
;;;      → (begin (define (f x) (+ x 1)) (define (g x) (f x)))
;;;
;;; Detection: If first section has holes, use mode 1. Otherwise mode 2.
(define (tp-batch input)
  (let* ([sections (map string-trim (string-split input "---"))]
         [first-section (car sections)]
         [first-expr (apply-implicit-parens (tokenize first-section))]
         [first-holes (find-holes first-expr)])
    (if (null? first-holes)
        ;; Mode 2: Complete definitions (no holes in first section)
        (let ([exprs (map (lambda (s)
                            (apply-implicit-parens (tokenize s)))
                          sections)])
          (ts-reset)
          (ts-defs exprs))
        ;; Mode 1: Template + fills
        (begin
          (ts-reset)
          (ts-start first-expr)
          (for-each
            (lambda (section)
              (let ([tokens (tokenize section)])
                (when (and (>= (length tokens) 3)
                           (hole? (car tokens))
                           (eq? (cadr tokens) ':=))
                  (ts-fill (car tokens)
                           (apply-implicit-parens (cddr tokens))))))
            (cdr sections))
          (ts-compile)))))

;;; string-split : String × String → (List String)
;;; Split string by delimiter.
(define (string-split str delim)
  (let ([delim-len (string-length delim)]
        [str-len (string-length str)])
    (let loop ([start 0] [acc '()])
      (let ([pos (string-find str delim start)])
        (if pos
            (loop (+ pos delim-len)
                  (cons (substring str start pos) acc))
            (reverse (cons (substring str start str-len) acc)))))))

;;; ============================================================
;;; String Utilities
;;; ============================================================

;;; string-find : String × String × Nat → Nat | #f
;;; Find first occurrence of needle in haystack starting at pos.
(define (string-find haystack needle start)
  (let ([hay-len (string-length haystack)]
        [needle-len (string-length needle)])
    (let loop ([i start])
      (cond
        [(> (+ i needle-len) hay-len) #f]
        [(string=? (substring haystack i (+ i needle-len)) needle) i]
        [else (loop (+ i 1))]))))

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
