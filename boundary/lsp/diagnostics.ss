(load "core/base/prelude.ss")
(load "boundary/lsp/json.ss")
(load "boundary/lsp/protocol.ss")
(load "boundary/lsp/documents.ss")

(doc 'note "Load the error system (always load - Chez has a built-in make-error with different arity that we need to override)")
(load "core/base/error.ss")

(doc 'module 'lsp/diagnostics)
(doc 'description "Converts The Fold's error system to LSP diagnostics: parse errors, type inference errors, and evaluation errors")
(doc 'layer 'boundary)
(doc 'purity 'total)
(doc 'requires '(prelude json protocol documents error))

(doc 'section 'error-phase-to-severity-mapping)

(doc phase->severity 'type '(-> Symbol Int))
(doc phase->severity 'description "Convert an error phase to LSP diagnostic severity")
(define (phase->severity phase)
  (case phase
        [(parse) *severity-error*]
        [(infer) *severity-error*]
        [(eval)  *severity-error*]
        [(block) *severity-error*]
        [(warning) *severity-warning*]
        [(hint) *severity-hint*]
        [else *severity-information*]))

(doc 'section 'fold-error-to-lsp-diagnostic)

(doc fold-error->diagnostic 'type '(-> Document FoldError JsonObject))
(doc fold-error->diagnostic 'description "Convert a Fold error to an LSP diagnostic")
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

(doc 'section 'document-analysis)

(doc analyze-document-for-diagnostics 'type '(-> Document (List Diagnostic)))
(doc analyze-document-for-diagnostics 'description "Analyze a document and return LSP diagnostics")
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

(doc 'section 'basic-syntax-checking)

(doc check-balanced-parens 'type '(-> String String (List Error)))
(doc check-balanced-parens 'description "Check for unbalanced parentheses")
(doc check-balanced-parens 'note "O(N) complexity - tracks line/col incrementally instead of rescanning")
(define (check-balanced-parens content path)
  (let ([len (string-length content)])
       (let loop ([i 0] [depth 0] [in-string #f] [escape #f]
                  [line 1] [col 1]  ;; Track line/col incrementally
                  [paren-stack '()] [errors '()])
            (if (>= i len)
                ;; End of input
                (if (and (not in-string) (> depth 0))
                    ;; Unclosed parens
                    (reverse (cons (make-unclosed-error path paren-stack)
                                   errors))
                    (reverse errors))
                (let* ([c (string-ref content i)]
                       [nl? (char=? c #\newline)]
                       [nxt-line (if nl? (+ line 1) line)]
                       [nxt-col (if nl? 1 (+ col 1))])
                      (cond
                       ;; Handle escape in string
                       [escape
                        (loop (+ i 1) depth in-string #f nxt-line nxt-col paren-stack errors)]
                       ;; String handling
                       [(and in-string (char=? c #\\))
                        (loop (+ i 1) depth in-string #t nxt-line nxt-col paren-stack errors)]
                       [(char=? c #\")
                        (loop (+ i 1) depth (not in-string) #f nxt-line nxt-col paren-stack errors)]
                       ;; Skip content inside strings
                       [in-string
                        (loop (+ i 1) depth in-string #f nxt-line nxt-col paren-stack errors)]
                       ;; Line comment handling (skip to end of line)
                       [(char=? c #\;)
                        (let ([new-i (skip-to-newline content i)])
                             (loop new-i depth in-string #f (+ line 1) 1 paren-stack errors))]
                       ;; Hash-prefixed constructs
                       [(and (char=? c #\#) (< (+ i 1) len))
                        (let ([c2 (string-ref content (+ i 1))])
                             (cond
                              ;; #| block comment - skip to matching |#
                              [(char=? c2 #\|)
                               (let-values ([(new-i new-line new-col)
                                             (skip-block-comment content (+ i 2) line (+ col 2))])
                                           (loop new-i depth in-string #f new-line new-col paren-stack errors))]
                              ;; #; datum comment - skip the next datum
                              [(char=? c2 #\;)
                               (let-values ([(new-i new-line new-col)
                                             (skip-datum content (+ i 2) line (+ col 2))])
                                           (loop new-i depth in-string #f new-line new-col paren-stack errors))]
                              ;; Other # constructs (like #t, #f, #\char, #(...)) - just continue
                              [else
                               (loop (+ i 1) depth in-string #f nxt-line nxt-col paren-stack errors)]))]
                       ;; Opening parens - track type along with position
                       ;; Stack entries are (type line . col) where type is 'paren or 'bracket
                       [(char=? c #\()
                        (loop (+ i 1) (+ depth 1) in-string #f nxt-line nxt-col
                              (cons (list 'paren line col) paren-stack) errors)]
                       [(char=? c #\[)
                        (loop (+ i 1) (+ depth 1) in-string #f nxt-line nxt-col
                              (cons (list 'bracket line col) paren-stack) errors)]
                       ;; Closing parens - verify type matches
                       [(char=? c #\))
                        (if (> depth 0)
                            (let* ([opener (if (pair? paren-stack) (car paren-stack) #f)]
                                   [opener-type (if opener (car opener) #f)]
                                   [mismatch? (and opener (eq? opener-type 'bracket))])
                                  (if mismatch?
                                      ;; Bracket opened, paren closed - error
                                      (loop (+ i 1) (- depth 1) in-string #f nxt-line nxt-col
                                            (if (pair? paren-stack) (cdr paren-stack) '())
                                            (cons (make-mismatch-error path line col 'paren opener) errors))
                                      ;; Matched correctly
                                      (loop (+ i 1) (- depth 1) in-string #f nxt-line nxt-col
                                            (if (pair? paren-stack) (cdr paren-stack) '()) errors)))
                            (loop (+ i 1) depth in-string #f nxt-line nxt-col paren-stack
                                  (cons (make-extra-close-error-fast path line col) errors)))]
                       [(char=? c #\])
                        (if (> depth 0)
                            (let* ([opener (if (pair? paren-stack) (car paren-stack) #f)]
                                   [opener-type (if opener (car opener) #f)]
                                   [mismatch? (and opener (eq? opener-type 'paren))])
                                  (if mismatch?
                                      ;; Paren opened, bracket closed - error
                                      (loop (+ i 1) (- depth 1) in-string #f nxt-line nxt-col
                                            (if (pair? paren-stack) (cdr paren-stack) '())
                                            (cons (make-mismatch-error path line col 'bracket opener) errors))
                                      ;; Matched correctly
                                      (loop (+ i 1) (- depth 1) in-string #f nxt-line nxt-col
                                            (if (pair? paren-stack) (cdr paren-stack) '()) errors)))
                            (loop (+ i 1) depth in-string #f nxt-line nxt-col paren-stack
                                  (cons (make-extra-close-error-fast path line col) errors)))]
                       ;; Other characters
                       [else
                        (loop (+ i 1) depth in-string #f nxt-line nxt-col paren-stack errors)]))))))

;;; skip-to-newline : String × Int → Int
;;; Returns position after the next newline, or len if no newline found.
(define (skip-to-newline content i)
  (let ([len (string-length content)])
       (let loop ([j i])
            (cond
             [(>= j len) len]  ; At end, return len (not len+1)
             [(char=? (string-ref content j) #\newline) (+ j 1)]
             [else (loop (+ j 1))]))))

;;; skip-block-comment : String × Int × Int × Int → (values Int Int Int)
;;; Skip a #| |# block comment, handling nesting.
;;; Returns (new-position new-line new-col).
;;; Starts at position i which is right after the opening #|.
(define (skip-block-comment content i line col)
  (let ([len (string-length content)])
       (let loop ([j i] [depth 1] [ln line] [cl col])
            (cond
             [(>= j len)
              ;; Unterminated block comment - return end position
              (values j ln cl)]
             [(>= (+ j 1) len)
              ;; Only one char left, can't match |# or #|
              (let ([c (string-ref content j)])
                   (if (char=? c #\newline)
                       (loop (+ j 1) depth (+ ln 1) 1)
                       (loop (+ j 1) depth ln (+ cl 1))))]
             [else
              (let ([c (string-ref content j)]
                    [c2 (string-ref content (+ j 1))])
                   (cond
                    ;; Nested opening #|
                    [(and (char=? c #\#) (char=? c2 #\|))
                     (loop (+ j 2) (+ depth 1) ln (+ cl 2))]
                    ;; Closing |#
                    [(and (char=? c #\|) (char=? c2 #\#))
                     (if (= depth 1)
                         (values (+ j 2) ln (+ cl 2))  ;; Done
                         (loop (+ j 2) (- depth 1) ln (+ cl 2)))]
                    ;; Newline
                    [(char=? c #\newline)
                     (loop (+ j 1) depth (+ ln 1) 1)]
                    ;; Other character
                    [else
                     (loop (+ j 1) depth ln (+ cl 1))]))]))))

;;; skip-datum : String × Int × Int × Int → (values Int Int Int)
;;; Skip a single datum for #; comments.
;;; Returns (new-position new-line new-col).
;;; Strategy: Track paren depth, handle strings, skip to end of atom.
(define (skip-datum content i line col)
  (let ([len (string-length content)])
       ;; First skip whitespace
       (let skip-ws ([j i] [ln line] [cl col])
            (cond
             [(>= j len) (values j ln cl)]
             [(char-whitespace? (string-ref content j))
              (if (char=? (string-ref content j) #\newline)
                  (skip-ws (+ j 1) (+ ln 1) 1)
                  (skip-ws (+ j 1) ln (+ cl 1)))]
             ;; Start of datum - dispatch on first char
             [else
              (let ([c (string-ref content j)])
                   (cond
                    ;; String
                    [(char=? c #\")
                     (skip-string content (+ j 1) ln (+ cl 1))]
                    ;; List/vector
                    [(or (char=? c #\() (char=? c #\[))
                     (skip-list content (+ j 1) ln (+ cl 1) 1)]
                    ;; Character literal
                    [(and (char=? c #\#) (< (+ j 1) len))
                     (let ([c2 (string-ref content (+ j 1))])
                          (cond
                           ;; #( vector
                           [(char=? c2 #\()
                            (skip-list content (+ j 2) ln (+ cl 2) 1)]
                           ;; #| block comment inside datum comment - skip it
                           [(char=? c2 #\|)
                            (let-values ([(nj nl nc) (skip-block-comment content (+ j 2) ln (+ cl 2))])
                                        (skip-datum content nj nl nc))]
                           ;; #; nested datum comment
                           [(char=? c2 #\;)
                            (let-values ([(nj nl nc) (skip-datum content (+ j 2) ln (+ cl 2))])
                                        (skip-datum content nj nl nc))]
                           ;; #\char - skip char literal
                           [(char=? c2 #\\)
                            (skip-atom content (+ j 2) ln (+ cl 2))]
                           ;; Other # construct - treat as atom
                           [else
                            (skip-atom content j ln cl)]))]
                    ;; Quote/quasiquote - skip the next datum too
                    [(or (char=? c #\') (char=? c #\`) (char=? c #\,))
                     (let ([at? (and (< (+ j 1) len) (char=? (string-ref content (+ j 1)) #\@))])
                          (if at?
                              (skip-datum content (+ j 2) ln (+ cl 2))
                              (skip-datum content (+ j 1) ln (+ cl 1))))]
                    ;; Regular atom (symbol, number)
                    [else
                     (skip-atom content j ln cl)]))]))))

;;; skip-string : String × Int × Int × Int → (values Int Int Int)
;;; Skip a string literal starting right after the opening quote.
(define (skip-string content i line col)
  (let ([len (string-length content)])
       (let loop ([j i] [ln line] [cl col] [escape #f])
            (cond
             [(>= j len) (values j ln cl)]
             [(let ([c (string-ref content j)])
                   (cond
                    [escape (loop (+ j 1) ln (+ cl 1) #f)]
                    [(char=? c #\\) (loop (+ j 1) ln (+ cl 1) #t)]
                    [(char=? c #\") (values (+ j 1) ln (+ cl 1))]
                    [(char=? c #\newline) (loop (+ j 1) (+ ln 1) 1 #f)]
                    [else (loop (+ j 1) ln (+ cl 1) #f)]))]))))

;;; skip-list : String × Int × Int × Int × Int → (values Int Int Int)
;;; Skip a list/vector starting right after the opening paren.
(define (skip-list content i line col depth)
  (let ([len (string-length content)])
       (let loop ([j i] [ln line] [cl col] [d depth] [in-str #f] [esc #f])
            (cond
             [(>= j len) (values j ln cl)]
             [else
              (let ([c (string-ref content j)])
                   (cond
                    ;; Handle string escapes
                    [esc (loop (+ j 1) ln (+ cl 1) d in-str #f)]
                    [(and in-str (char=? c #\\))
                     (loop (+ j 1) ln (+ cl 1) d in-str #t)]
                    [(char=? c #\")
                     (loop (+ j 1) ln (+ cl 1) d (not in-str) #f)]
                    [in-str
                     (if (char=? c #\newline)
                         (loop (+ j 1) (+ ln 1) 1 d in-str #f)
                         (loop (+ j 1) ln (+ cl 1) d in-str #f))]
                    ;; Handle parens outside strings
                    [(or (char=? c #\() (char=? c #\[))
                     (loop (+ j 1) ln (+ cl 1) (+ d 1) in-str #f)]
                    [(or (char=? c #\)) (char=? c #\]))
                     (if (= d 1)
                         (values (+ j 1) ln (+ cl 1))  ;; Done
                         (loop (+ j 1) ln (+ cl 1) (- d 1) in-str #f))]
                    ;; Handle ; comments inside list
                    [(char=? c #\;)
                     (let ([new-j (skip-to-newline content j)])
                          (loop new-j (+ ln 1) 1 d in-str #f))]
                    ;; Handle block comments inside list
                    [(and (char=? c #\#) (< (+ j 1) len) (char=? (string-ref content (+ j 1)) #\|))
                     (let-values ([(nj nl nc) (skip-block-comment content (+ j 2) ln (+ cl 2))])
                                 (loop nj nl nc d in-str #f))]
                    ;; Newline
                    [(char=? c #\newline)
                     (loop (+ j 1) (+ ln 1) 1 d in-str #f)]
                    ;; Other
                    [else (loop (+ j 1) ln (+ cl 1) d in-str #f)]))]))))

;;; skip-atom : String × Int × Int × Int → (values Int Int Int)
;;; Skip an atom (symbol, number, etc.).
(define (skip-atom content i line col)
  (let ([len (string-length content)])
       (let loop ([j i] [ln line] [cl col])
            (cond
             [(>= j len) (values j ln cl)]
             [else
              (let ([c (string-ref content j)])
                   (if (or (char-whitespace? c)
                           (char=? c #\() (char=? c #\))
                           (char=? c #\[) (char=? c #\])
                           (char=? c #\") (char=? c #\;))
                       (values j ln cl)
                       (if (char=? c #\newline)
                           (loop (+ j 1) (+ ln 1) 1)
                           (loop (+ j 1) ln (+ cl 1)))))]))))

;;; compute-line-col : String × Int → (line . col)
(define (compute-line-col content offset)
  (let loop ([i 0] [line 1] [col 1])
       (cond
        [(>= i offset) (cons line col)]
        [(char=? (string-ref content i) #\newline)
         (loop (+ i 1) (+ line 1) 1)]
        [else
         (loop (+ i 1) line (+ col 1))])))

;;; make-unclosed-error : String × (List (type line col)) → Error
;;; Stack entries are now (type line col) triples.
(define (make-unclosed-error path paren-stack)
  (let* ([entry (if (pair? paren-stack) (car paren-stack) #f)]
         [ln (if entry (cadr entry) 1)]
         [cl (if entry (caddr entry) 1)])
        (make-error 'parse 'unclosed-list
                    (make-span path ln cl ln (+ cl 1)))))

;;; make-mismatch-error : String × Int × Int × Symbol × (type line col) → Error
;;; Create an error for mismatched bracket types.
;;; closer-type is 'paren or 'bracket (what was used to close).
;;; opener is the stack entry (type line col) of the opener.
(define (make-mismatch-error path close-line close-col closer-type opener)
  (let* ([opener-type (car opener)]
         [opener-line (cadr opener)]
         [opener-col (caddr opener)]
         [msg (if (eq? closer-type 'paren)
                  "bracketed list terminated by parenthesis"
                  "parenthesized list terminated by bracket")])
        (make-error 'parse 'bracket-mismatch
                    (make-span path close-line close-col close-line (+ close-col 1))
                    msg)))

;;; make-extra-close-error : String × Int × String → Error
;;; Legacy version - computes line/col from offset (O(N) per call).
(define (make-extra-close-error path offset content)
  (let ([loc (compute-line-col content offset)])
       (make-error 'parse 'unexpected-char
                   (make-span path (car loc) (cdr loc) (car loc) (+ (cdr loc) 1))
                   ")")))

;;; make-extra-close-error-fast : String × Int × Int → Error
;;; O(1) version - takes line/col directly for incremental scanning.
(define (make-extra-close-error-fast path line col)
  (make-error 'parse 'unexpected-char
              (make-span path line col line (+ col 1))
              ")"))
