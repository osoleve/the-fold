(load "core/base/prelude.ss")
(load "core/base/span.ss")

(doc 'module 'error)
(doc 'description "Unified error system. Standardized error types with phase identification (where it happened), error codes (what happened), source context (location when available), and suggestions (how to fix).")
(doc 'layer 'core)
(doc 'purity 'total)
(doc 'dependencies '(prelude span))
(doc 'note "Error = (error phase code context details...)")

(doc 'section 'error-codes-by-phase)

(doc *parse-errors* 'type (List (Pair Symbol String)))
(doc *parse-errors* 'description "Parse error codes and messages.")
(define *parse-errors*
  '((unexpected-eof    . "Unexpected end of input")
    (unexpected-char   . "Unexpected character")
    (unclosed-string   . "Unclosed string literal")
    (unclosed-list     . "Unclosed list - missing )")
    (invalid-number    . "Invalid number format")
    (invalid-escape    . "Invalid escape sequence")))

(doc *infer-errors* 'type (List (Pair Symbol String)))
(doc *infer-errors* 'description "Type inference error codes and messages.")
(define *infer-errors*
  '((unbound-variable  . "Variable is not defined")
    (type-mismatch     . "Types do not match")
    (arity-mismatch    . "Wrong number of arguments")
    (not-a-function    . "Attempting to call a non-function")
    (occurs-check      . "Infinite type detected")
    (unknown-primitive . "Unknown primitive operation")
    (if-test-not-bool  . "If condition must be boolean")))

(doc *eval-errors* 'type (List (Pair Symbol String)))
(doc *eval-errors* 'description "Evaluation error codes and messages.")
(define *eval-errors*
  '((unbound-variable   . "Variable is not defined")
    (invalid-expression . "Cannot evaluate this expression")
    (not-a-closure      . "Attempting to call a non-function")
    (invalid-arguments  . "Invalid arguments to function")
    (division-by-zero   . "Division by zero")
    (out-of-bounds      . "Index out of bounds")
    (type-error         . "Runtime type error")))

(doc *block-errors* 'type (List (Pair Symbol String)))
(doc *block-errors* 'description "Block/CAS error codes and messages.")
(define *block-errors*
  '((invalid-tag       . "Invalid block tag")
    (invalid-payload   . "Invalid block payload")
    (invalid-refs      . "Invalid block references")
    (hash-mismatch     . "Content hash does not match")
    (not-found         . "Block not found in store")))

(doc 'section 'error-construction)

(define (make-error phase code context . details)
  (doc 'type (-> Symbol Symbol Any Any ... Error))
  (doc 'description "Create a structured error.")
  (doc 'export #t)
  `(error ,phase ,code ,context ,@details))

(define (error-phase err)
  (doc 'type (-> Error (Maybe Symbol)))
  (doc 'description "Extract phase from error.")
  (doc 'export #t)
  (and (error? err) (cadr err)))

(define (error-code err)
  (doc 'type (-> Error (Maybe Symbol)))
  (doc 'description "Extract code from error.")
  (doc 'export #t)
  (and (error? err) (caddr err)))

(define (error-context err)
  (doc 'type (-> Error (Maybe α)))
  (doc 'description "Extract context from error.")
  (doc 'export #t)
  (and (error? err) (cadddr err)))

(define (error-details err)
  (doc 'type (-> Error (List α)))
  (doc 'description "Extract details from error.")
  (doc 'export #t)
  (and (error? err) (cddddr err)))

(define (error? x)
  (doc 'type (-> α Bool))
  (doc 'description "Is this an error?")
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'error)
       (>= (length x) 4)))

(doc 'section 'source-context)
(doc 'note "Span operations imported from span.ss: make-span, span?, span-file, span-line, span-column, no-span.")

(doc 'section 'error-message-lookup)

(define (lookup-error-message phase code)
  (doc 'type (-> Symbol Symbol String))
  (doc 'description "Lookup error message for phase and code.")
  (doc 'export #t)
  (let* ([table (case phase
                      [(parse) *parse-errors*]
                      [(infer) *infer-errors*]
                      [(eval)  *eval-errors*]
                      [(block cas) *block-errors*]
                      [else '()])]
         [entry (assq code table)])
        (if entry
            (cdr entry)
            (symbol->string code))))

(doc 'section 'error-formatting)

(define (format-error err)
  (doc 'type (-> α String))
  (doc 'description "Format an error for display.")
  (doc 'export #t)
  (if (not (error? err))
      (format "~a" err)
      (let* ([phase (error-phase err)]
             [code (error-code err)]
             [ctx (error-context err)]
             [details (error-details err)]
             [message (lookup-error-message phase code)]
             [location (format-location ctx)])
            (string-append
             location
             (format-phase phase)
             message
             (format-details code details)
             (format-suggestion phase code details)))))

(define (format-phase phase)
  (doc 'type (-> Symbol String))
  (doc 'description "Format phase for display.")
  (format "[~a] " phase))

(define (format-location ctx)
  (doc 'type (-> α String))
  (doc 'description "Format location context for display.")
  (cond
   [(span? ctx)
    (let ([file (span-file ctx)]
          [line (span-line ctx)]
          [col (span-column ctx)])
         (if (and (> line 0) (> col 0))
             (format "~a:~a:~a: " file line col)
             ""))]
   [(string? ctx) (format "~a: " ctx)]
   [else ""]))

(define (format-details code details)
  (doc 'type (-> Symbol (List α) String))
  (doc 'description "Format error details for display.")
  (if (null? details)
      ""
      (case code
            [(unbound-variable)
             (format ": '~a'" (car details))]
            [(type-mismatch)
             (if (>= (length details) 2)
                 (format "
  expected: ~a
  actual:   ~a"
                         (car details) (cadr details))
                 "")]
            [(arity-mismatch)
             (if (>= (length details) 2)
                 (format " (expected ~a, got ~a)"
                         (car details) (cadr details))
                 "")]
            [(unknown-primitive)
             (format ": '~a'" (car details))]
            [else
             (if (pair? details)
                 (format ": ~a" (car details))
                 "")])))

(doc 'section 'suggestions)

(define (format-suggestion phase code details)
  (doc 'type (-> Symbol Symbol (List α) String))
  (doc 'description "Format suggestion for error.")
  (let ([suggestion (get-suggestion phase code details)])
       (if suggestion
           (format "
  hint: ~a" suggestion)
           "")))

(define (get-suggestion phase code details)
  (doc 'type (-> Symbol Symbol (List α) (Maybe String)))
  (doc 'description "Get suggestion text for error code.")
  (case code
        [(unbound-variable)
         (cond
          [(and (pair? details) (similar-to? (car details) 'define))
           "Did you mean 'fn' for function definition?"]
          [(and (pair? details) (similar-to? (car details) 'lambda))
           "Use 'fn' instead of 'lambda' in The Fold"]
          [else
           "Check spelling or add a binding with 'let' or 'fix'"])]
        [(type-mismatch)
         "Ensure the expression returns the expected type"]
        [(not-a-function)
         "Only closures can be called. Check that the first element is a function."]
        [(unknown-primitive)
         (if (and (pair? details)
                  (memq (car details) '(+ - * /)))
             (format "Use 'add', 'sub', 'mul', 'div' instead of ~a" (car details))
             "See (help 'primitives) for available operations")]
        [(unclosed-list)
         "Count your parentheses - every ( needs a matching )"]
        [(unclosed-string)
         "Add a closing \" to complete the string"]
        [else #f]))

(define (similar-to? s1 s2)
  (doc 'type (-> α β Bool))
  (doc 'description "Check if two symbols are similar (for typo detection).")
  (and (symbol? s1) (symbol? s2)
       (let ([str1 (symbol->string s1)]
             [str2 (symbol->string s2)])
            (<= (edit-distance str1 str2) 2))))

(doc 'section 'pretty-printing)

(define (print-error err)
  (doc 'type (-> Error Void))
  (doc 'description "Print a formatted error to current output.")
  (doc 'export #t)
  (display (format-error err))
  (newline))

(define (print-error-with-source err source)
  (doc 'type (-> Error String Void))
  (doc 'description "Print error with source code context.")
  (doc 'export #t)
  (let ([ctx (error-context err)])
       (display (format-error err))
       (newline)
       (when (and (span? ctx) (> (span-line ctx) 0))
             (display-source-context source (span-line ctx) (span-column ctx)))
       (newline)))

(define (display-source-context source line col)
  (doc 'type (-> String Nat Nat Void))
  (doc 'description "Show the relevant line with an underline pointer.")
  (let ([lines (string-split source #\newline)])
       (when (and (> line 0) (<= line (length lines)))
             (let ([src-line (list-ref lines (- line 1))])
                  (display "    ")
                  (display src-line)
                  (newline)
                  (display "    ")
                  (display (make-string (max 0 (- col 1)) #\space))
                  (display "^")
                  (newline)))))

(doc 'note "string-split is provided by prelude.ss (loaded above).")

(doc 'section 'error-helpers)
(doc 'note "Helpers for common error cases.")

(define (unbound-error var span)
  (doc 'type (-> Symbol Span Error))
  (doc 'description "Create unbound variable error.")
  (doc 'export #t)
  (make-error 'infer 'unbound-variable span var))

(define (type-error expected actual span)
  (doc 'type (-> α β Span Error))
  (doc 'description "Create type mismatch error.")
  (doc 'export #t)
  (make-error 'infer 'type-mismatch span expected actual))

(define (parse-error code expected position)
  (doc 'type (-> Symbol String Nat Error))
  (doc 'description "Create parse error.")
  (doc 'export #t)
  (make-error 'parse code (make-span "<input>" 1 position 1 position) expected))

(define (eval-error code value span)
  (doc 'type (-> Symbol α Span Error))
  (doc 'description "Create evaluation error.")
  (doc 'export #t)
  (make-error 'eval code span value))
