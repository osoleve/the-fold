;;; fabric/stitches/error.ss — Unified Error System
;;;
;;; Standardized error types with:
;;;   - Phase identification (where it happened)
;;;   - Error codes (what happened)
;;;   - Source context (location when available)
;;;   - Suggestions (how to fix)
;;;
;;; Error = (error phase code context details...)
;;;
;;; This is Core code: pure, total.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - span.ss (for source location tracking)

(load "core/base/prelude.ss")
(load "core/lang/span.ss")  ; Imports make-span, span?, span-file, span-line, etc.

;;; ============================================================
;;; Error Codes by Phase
;;; ============================================================

;;; Parse errors
(define *parse-errors*
  '((unexpected-eof    . "Unexpected end of input")
    (unexpected-char   . "Unexpected character")
    (unclosed-string   . "Unclosed string literal")
    (unclosed-list     . "Unclosed list - missing )")
    (invalid-number    . "Invalid number format")
    (invalid-escape    . "Invalid escape sequence")))

;;; Type inference errors
(define *infer-errors*
  '((unbound-variable  . "Variable is not defined")
    (type-mismatch     . "Types do not match")
    (arity-mismatch    . "Wrong number of arguments")
    (not-a-function    . "Attempting to call a non-function")
    (occurs-check      . "Infinite type detected")
    (unknown-primitive . "Unknown primitive operation")
    (if-test-not-bool  . "If condition must be boolean")))

;;; Evaluation errors
(define *eval-errors*
  '((unbound-variable   . "Variable is not defined")
    (invalid-expression . "Cannot evaluate this expression")
    (not-a-closure      . "Attempting to call a non-function")
    (invalid-arguments  . "Invalid arguments to function")
    (division-by-zero   . "Division by zero")
    (out-of-bounds      . "Index out of bounds")
    (type-error         . "Runtime type error")))

;;; Block/CAS errors
(define *block-errors*
  '((invalid-tag       . "Invalid block tag")
    (invalid-payload   . "Invalid block payload")
    (invalid-refs      . "Invalid block references")
    (hash-mismatch     . "Content hash does not match")
    (not-found         . "Block not found in store")))

;;; ============================================================
;;; Error Construction
;;; ============================================================

;;; make-error : Phase × Code × Context × Details... → Error
;;; Create a structured error.
(define (make-error phase code context . details)
  `(error ,phase ,code ,context ,@details))

;;; error-phase : Error → Symbol
(define (error-phase err)
  (and (error? err) (cadr err)))

;;; error-code : Error → Symbol
(define (error-code err)
  (and (error? err) (caddr err)))

;;; error-context : Error → Any
(define (error-context err)
  (and (error? err) (cadddr err)))

;;; error-details : Error → List
(define (error-details err)
  (and (error? err) (cddddr err)))

;;; error? : Any → Boolean
(define (error? x)
  (and (pair? x)
       (eq? (car x) 'error)
       (>= (length x) 4)))

;;; ============================================================
;;; Source Context (Spans)
;;;
;;; Span operations imported from span.ss:
;;;   make-span, span?, span-file, span-line, span-column, no-span
;;; ============================================================

;;; ============================================================
;;; Error Message Lookup
;;; ============================================================

;;; lookup-error-message : Phase × Code → String
(define (lookup-error-message phase code)
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

;;; ============================================================
;;; Error Formatting
;;; ============================================================

;;; format-error : Error → String
;;; Format an error for display.
(define (format-error err)
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

;;; format-phase : Symbol → String
(define (format-phase phase)
  (format "[~a] " phase))

;;; format-location : Context → String
(define (format-location ctx)
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

;;; format-details : Code × Details → String
(define (format-details code details)
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

;;; ============================================================
;;; Suggestions
;;; ============================================================

;;; format-suggestion : Phase × Code × Details → String
(define (format-suggestion phase code details)
  (let ([suggestion (get-suggestion phase code details)])
       (if suggestion
           (format "
  hint: ~a" suggestion)
           "")))

;;; get-suggestion : Phase × Code × Details → String | #f
(define (get-suggestion phase code details)
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

;;; similar-to? : Symbol × Symbol → Boolean
;;; Check if two symbols are similar (for typo detection).
(define (similar-to? s1 s2)
  (and (symbol? s1) (symbol? s2)
       (let ([str1 (symbol->string s1)]
             [str2 (symbol->string s2)])
            (<= (edit-distance str1 str2) 2))))

;;; edit-distance : String × String → Int
;;; Levenshtein distance between two strings.
(define (edit-distance s1 s2)
  (let* ([len1 (string-length s1)]
         [len2 (string-length s2)]
         [matrix (make-vector (* (+ len1 1) (+ len2 1)) 0)])
        ;; Initialize first row and column
        (do ([i 0 (+ i 1)]) ((> i len1))
            (vector-set! matrix (* i (+ len2 1)) i))
        (do ([j 0 (+ j 1)]) ((> j len2))
            (vector-set! matrix j j))
        ;; Fill matrix
        (do ([i 1 (+ i 1)]) ((> i len1))
            (do ([j 1 (+ j 1)]) ((> j len2))
                (let* ([cost (if (char=? (string-ref s1 (- i 1))
                                         (string-ref s2 (- j 1)))
                                 0 1)]
                       [idx (+ (* i (+ len2 1)) j)]
                       [del (+ 1 (vector-ref matrix (+ (* (- i 1) (+ len2 1)) j)))]
                       [ins (+ 1 (vector-ref matrix (+ (* i (+ len2 1)) (- j 1))))]
                       [sub (+ cost (vector-ref matrix (+ (* (- i 1) (+ len2 1)) (- j 1))))])
                      (vector-set! matrix idx (min del ins sub)))))
        ;; Return final distance
        (vector-ref matrix (+ (* len1 (+ len2 1)) len2))))

;;; ============================================================
;;; Pretty Printing
;;; ============================================================

;;; print-error : Error → void
;;; Print a formatted error to current output.
(define (print-error err)
  (display (format-error err))
  (newline))

;;; print-error-with-source : Error × String → void
;;; Print error with source code context.
(define (print-error-with-source err source)
  (let ([ctx (error-context err)])
       (display (format-error err))
       (newline)
       (when (and (span? ctx) (> (span-line ctx) 0))
             (display-source-context source (span-line ctx) (span-column ctx)))
       (newline)))

;;; display-source-context : String × Int × Int → void
;;; Show the relevant line with an underline pointer.
(define (display-source-context source line col)
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

;;; NOTE: string-split is provided by prelude.ss (loaded above)

;;; ============================================================
;;; Error Helpers for Common Cases
;;; ============================================================

;;; unbound-error : Symbol × Span → Error
(define (unbound-error var span)
  (make-error 'infer 'unbound-variable span var))

;;; type-error : Type × Type × Span → Error
(define (type-error expected actual span)
  (make-error 'infer 'type-mismatch span expected actual))

;;; parse-error : Code × String × Int → Error
(define (parse-error code expected position)
  (make-error 'parse code (make-span "<input>" 1 position 1 position) expected))

;;; eval-error : Code × Any × Span → Error
(define (eval-error code value span)
  (make-error 'eval code span value))
