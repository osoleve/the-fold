;;; core/lsp/diagnostics.ss — Error to Diagnostic Conversion
;;; @module diagnostics
;;; @requires prelude json protocol documents error
;;;
;;; Converts The Fold's error system to LSP diagnostics:
;;;   - Parse errors
;;;   - Type inference errors
;;;   - Evaluation errors
;;;
;;; This is Core code: pure functions for error conversion.

(load "core/base/prelude.ss")
(load "boundary/lsp/json.ss")
(load "boundary/lsp/protocol.ss")
(load "boundary/lsp/documents.ss")

;;; Load the error system (always load - Chez has a built-in make-error
;;; with different arity that we need to override)
(load "core/base/error.ss")

;;; ====
;;; Error Phase to Severity Mapping
;;; ====

;;; phase->severity : Symbol → Int
;;; Convert an error phase to LSP diagnostic severity.
(define (phase->severity phase)
  (case phase
        [(parse) *severity-error*]
        [(infer) *severity-error*]
        [(eval)  *severity-error*]
        [(block) *severity-error*]
        [(warning) *severity-warning*]
        [(hint) *severity-hint*]
        [else *severity-information*]))

;;; ====
;;; Fold Error to LSP Diagnostic
;;; ====

;;; fold-error->diagnostic : Document × FoldError → JsonObject
;;; Convert a Fold error to an LSP diagnostic.
(define (fold-error->diagnostic doc err)
  (if (not (error? err))
      ;; Not a structured error, create a generic one
      (json-obj "range" (make-range (make-position 0 0) (make-position 0 1))
                "severity" *severity-error*
                "source" "fold"
                "message" (format "~a" err))
      ;; Structured error
      (let* ([phase (error-phase err)]
             [code (error-code err)]
             [ctx (error-context err)]
             [details (error-details err)]
             [message (format-diagnostic-message phase code details)]
             [range (context->range doc ctx)]
             [severity (phase->severity phase)])
            (json-obj "range" range
                      "severity" severity
                      "source" "fold"
                      "code" (symbol->string code)
                      "message" message))))

;;; context->range : Document × Context → Range
;;; Convert an error context (usually a span) to an LSP range.
(define (context->range doc ctx)
  (cond
   [(and (pair? ctx) (eq? (car ctx) 'span))
    ;; It's a span
    (span->lsp-range doc ctx)]
   [(integer? ctx)
    ;; It's a character position
    (let ([pos (offset->lsp-position doc ctx)])
         (make-range pos pos))]
   [else
    ;; Unknown context, use start of document
    (make-range (make-position 0 0) (make-position 0 1))]))

;;; format-diagnostic-message : Symbol × Symbol × (List Any) → String
;;; Format the diagnostic message from error components.
(define (format-diagnostic-message phase code details)
  (let ([base-msg (lookup-error-message* phase code)])
       (if (null? details)
           base-msg
           (case code
                 [(unbound-variable)
                  (format "~a: '~a'" base-msg (car details))]
                 [(type-mismatch)
                  (if (>= (length details) 2)
                      (format "~a\n  expected: ~a\n  actual: ~a"
                              base-msg (car details) (cadr details))
                      base-msg)]
                 [(arity-mismatch)
                  (if (>= (length details) 2)
                      (format "~a (expected ~a, got ~a)"
                              base-msg (car details) (cadr details))
                      base-msg)]
                 [(unknown-primitive)
                  (format "~a: '~a'" base-msg (car details))]
                 [else
                  (if (pair? details)
                      (format "~a: ~a" base-msg (car details))
                      base-msg)]))))

;;; lookup-error-message* : Symbol × Symbol → String
;;; Look up the base error message from error.ss tables.
(define (lookup-error-message* phase code)
  (let* ([table (case phase
                      [(parse) *parse-errors*]   ; From error.ss
                      [(infer) *infer-errors*]   ; From error.ss
                      [(eval)  *eval-errors*]    ; From error.ss
                      [(block) *block-errors*]   ; From error.ss
                      [else '()])]
         [entry (assq code table)])
        (if entry
            (cdr entry)
            (symbol->string code))))

;;; ====
;;; Document Analysis
;;; ====

;;; analyze-document-for-diagnostics : Document → (List Diagnostic)
;;; Analyze a document and return LSP diagnostics.
(define (analyze-document-for-diagnostics doc)
  (let* ([content (document-content doc)]
         [uri (document-uri doc)]
         [path (uri->path uri)]
         [errors (collect-document-errors content path)])
        (map (lambda (err) (fold-error->diagnostic doc err)) errors)))

;;; collect-document-errors : String × String → (List Error)
;;; Collect all errors from parsing and type checking.
(define (collect-document-errors content path)
  ;; Try to parse and type check
  (let ([parse-errors (try-parse content path)])
       (if (pair? parse-errors)
           parse-errors  ; Return parse errors if any
           ;; No parse errors, try type inference
           (let ([type-errors (try-typecheck content path)])
                type-errors))))

;;; try-parse : String × String → (List Error)
;;; Try to parse content, return list of errors.
(define (try-parse content path)
  ;; Placeholder: actual parsing integration goes here
  ;; For now, do basic bracket matching
  (check-balanced-parens content path))

;;; try-typecheck : String × String → (List Error)
;;; Try to type check content, return list of errors.
(define (try-typecheck content path)
  ;; Placeholder: actual type checking integration goes here
  '())

;;; ====
;;; Basic Syntax Checking
;;; ====

;;; check-balanced-parens : String × String → (List Error)
;;; Check for unbalanced parentheses.
(define (check-balanced-parens content path)
  (let ([len (string-length content)])
       (let loop ([i 0] [depth 0] [in-string #f] [escape #f]
                  [paren-stack '()] [errors '()])
            (if (>= i len)
                ;; End of input
                (if (and (not in-string) (> depth 0))
                    ;; Unclosed parens
                    (reverse (cons (make-unclosed-error path paren-stack)
                                   errors))
                    (reverse errors))
                (let ([c (string-ref content i)])
                     (cond
                      ;; Handle escape in string
                      [escape
                       (loop (+ i 1) depth in-string #f paren-stack errors)]
                      ;; String handling
                      [(and in-string (char=? c #\\))
                       (loop (+ i 1) depth in-string #t paren-stack errors)]
                      [(char=? c #\")
                       (loop (+ i 1) depth (not in-string) #f paren-stack errors)]
                      ;; Skip content inside strings
                      [in-string
                       (loop (+ i 1) depth in-string #f paren-stack errors)]
                      ;; Comment handling (skip to end of line)
                      [(char=? c #\;)
                       (loop (skip-to-newline content i) depth in-string #f paren-stack errors)]
                      ;; Opening parens
                      [(char=? c #\()
                       (loop (+ i 1) (+ depth 1) in-string #f
                             (cons (compute-line-col content i) paren-stack) errors)]
                      [(char=? c #\[)
                       (loop (+ i 1) (+ depth 1) in-string #f
                             (cons (compute-line-col content i) paren-stack) errors)]
                      ;; Closing parens
                      [(char=? c #\))
                       (if (> depth 0)
                           (loop (+ i 1) (- depth 1) in-string #f
                                 (if (pair? paren-stack) (cdr paren-stack) '()) errors)
                           (loop (+ i 1) depth in-string #f paren-stack
                                 (cons (make-extra-close-error path i content) errors)))]
                      [(char=? c #\])
                       (if (> depth 0)
                           (loop (+ i 1) (- depth 1) in-string #f
                                 (if (pair? paren-stack) (cdr paren-stack) '()) errors)
                           (loop (+ i 1) depth in-string #f paren-stack
                                 (cons (make-extra-close-error path i content) errors)))]
                      ;; Other characters
                      [else
                       (loop (+ i 1) depth in-string #f paren-stack errors)]))))))

;;; skip-to-newline : String × Int → Int
(define (skip-to-newline content i)
  (let ([len (string-length content)])
       (let loop ([j i])
            (if (or (>= j len) (char=? (string-ref content j) #\newline))
                (+ j 1)
                (loop (+ j 1))))))

;;; compute-line-col : String × Int → (line . col)
(define (compute-line-col content offset)
  (let loop ([i 0] [line 1] [col 1])
       (cond
        [(>= i offset) (cons line col)]
        [(char=? (string-ref content i) #\newline)
         (loop (+ i 1) (+ line 1) 1)]
        [else
         (loop (+ i 1) line (+ col 1))])))

;;; make-unclosed-error : String × (List (line . col)) → Error
(define (make-unclosed-error path paren-stack)
  (let ([loc (if (pair? paren-stack) (car paren-stack) '(1 . 1))])
       (make-error 'parse 'unclosed-list
                   (make-span path (car loc) (cdr loc) (car loc) (+ (cdr loc) 1)))))

;;; make-extra-close-error : String × Int × String → Error
(define (make-extra-close-error path offset content)
  (let ([loc (compute-line-col content offset)])
       (make-error 'parse 'unexpected-char
                   (make-span path (car loc) (cdr loc) (car loc) (+ (cdr loc) 1))
                   ")")))
