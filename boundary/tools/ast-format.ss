(load "core/base/prelude.ss")
(load "core/util/pretty.ss")

(doc 'module 'ast-format)
(doc 'description "AST-aware code formatter using Wadler-Lindig pretty-printing combinators. Supports style profiles (compact, verbose, diff-friendly), quote shorthand preservation, and dotted pairs.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Comment preservation is LIMITED: only file header comments are preserved. Inline comments are discarded by `read`.")

;;; ====
;;; Style Profiles
;;; ====
;;;
;;; A style profile controls formatting decisions:
;;;   - width: Target line width
;;;   - indent: Indentation step
;;;   - break-threshold: When to break multiline
;;;   - one-per-line: Force one element per line (diff-friendly)
;;;   - align-bindings: Align let/define bindings
;;;   - special-forms: Custom indentation rules

;;; make-style-profile : width indent break-threshold one-per-line align-bindings collapse-simple → StyleProfile
(define (make-style-profile width indent break-threshold
                            one-per-line align-bindings collapse-simple)
  (vector 'style-profile width indent break-threshold
          one-per-line align-bindings collapse-simple))

(define (style-profile? x)
  (and (vector? x) (eq? (vector-ref x 0) 'style-profile)))

(define (style-width p) (vector-ref p 1))
(define (style-indent p) (vector-ref p 2))
(define (style-break-threshold p) (vector-ref p 3))
(define (style-one-per-line p) (vector-ref p 4))
(define (style-align-bindings p) (vector-ref p 5))
(define (style-collapse-simple p) (vector-ref p 6))

;;; Predefined profiles

(define *profile-compact*
  (vector 'style-profile 80 2 40 #f #t #t))

(define *profile-verbose*
  (vector 'style-profile 80 2 30 #f #t #f))

(define *profile-diff-friendly*
  (vector 'style-profile 80 2 20 #t #f #f))

(define *profile-canonical*
  (vector 'style-profile 100 2 60 #f #t #t))

;;; Current profile (mutable)
(define *current-profile* *profile-compact*)

;;; Profile lookup
(define (get-profile name)
  (case name
        [(compact) *profile-compact*]
        [(verbose) *profile-verbose*]
        [(diff-friendly diff) *profile-diff-friendly*]
        [(canonical) *profile-canonical*]
        [else *profile-compact*]))

;;; ====
;;; Special Form Rules
;;; ====
;;;
;;; Rules for indenting special forms.
;;; Format: (form-name body-start-index break-style)
;;; body-start-index: index of first "body" element (indent more)
;;; break-style: 'always, 'never, 'auto

(define *special-form-rules*
  '((define 2 auto)      ; (define name value) or (define (name args) body...)
    (define-syntax 2 always)
    (lambda 1 auto)      ; (lambda (args) body...)
    (let 2 auto)         ; (let ((bindings)) body...)
    (let* 2 auto)
    (letrec 2 auto)
    (letrec* 2 auto)
    (if 1 auto)          ; (if cond then else)
    (cond 1 always)      ; (cond clause...)
    (case 2 always)      ; (case expr clause...)
    (match 2 always)     ; (match expr clause...)
    (guard 1 auto)
    (when 1 auto)
    (unless 1 auto)
    (begin 1 auto)
    (and 1 auto)
    (or 1 auto)
    (do 2 always)
    (set! 1 never)
    (quote 1 never)
    (quasiquote 1 never)
    (syntax-rules 2 always)
    (syntax-case 3 always)))

(define (get-form-rule name)
  (let ([found (assq name *special-form-rules*)])
       (if found
           (cdr found)
           '(1 auto))))  ; Default: break after first element, auto break

;;; ====
;;; AST to Document Conversion
;;; ====

;;; Convert S-expression to pretty-printer Doc
(define (sexp->doc* sexp profile)
  (cond
   [(null? sexp) (text "()")]
   [(symbol? sexp) (text (symbol->string sexp))]
   [(number? sexp) (text (number->string sexp))]
   [(string? sexp) (text (format "~s" sexp))]
   [(boolean? sexp) (text (if sexp "#t" "#f"))]
   [(char? sexp) (text (format "~s" sexp))]
   [(vector? sexp) (vector->doc sexp profile)]
   [(pair? sexp)
    (if (style-one-per-line profile)
        (list->doc-diff-friendly sexp profile)
        (list->doc sexp profile))]
   [else (text (format "~s" sexp))]))

;;; Convert vector to Doc
(define (vector->doc vec profile)
  (<> (text "#")
      (parens
       (sep (map (lambda (x) (sexp->doc* x profile))
                 (vector->list vec))))))

;;; Check if lst is a proper list (not dotted)
(define (proper-list? lst)
  (or (null? lst)
      (and (pair? lst)
           (proper-list? (cdr lst)))))

;;; Quote shorthands
(define *quote-shorthands*
  '((quote . "'")
    (quasiquote . "`")
    (unquote . ",")
    (unquote-splicing . ",@")))

;;; Convert list to Doc (normal mode)
(define (list->doc lst profile)
  (cond
   [(null? lst) (text "()")]
   ;; Handle dotted pairs: (a . b)
   [(not (proper-list? lst))
    (dotted-pair->doc lst profile)]
   ;; Handle quote shorthands: 'x -> (quote x)
   [(and (pair? lst)
         (= (length lst) 2)
         (symbol? (car lst))
         (assq (car lst) *quote-shorthands*))
    (let ([shorthand (cdr (assq (car lst) *quote-shorthands*))])
         (<> (text shorthand) (sexp->doc* (cadr lst) profile)))]
   ;; Normal list handling
   [else
    (let* ([head (car lst)]
           [tail (cdr lst)]
           [is-special? (and (symbol? head)
                             (assq head *special-form-rules*))]
           [rule (if is-special?
                     (cdr (assq head *special-form-rules*))
                     '(1 auto))]
           [body-start (car rule)]
           [break-style (cadr rule)])
          (format-form head tail body-start break-style profile))]))

;;; Convert dotted pair to Doc: (a b . c) -> "(a b . c)"
(define (dotted-pair->doc lst profile)
  (let loop ([lst lst] [acc '()])
       (cond
        [(null? lst)
         ;; Proper list - shouldn't happen but handle gracefully
         (parens (hsep (reverse acc)))]
        [(not (pair? lst))
         ;; Reached the dotted tail
         (parens
          (hsep (append (reverse acc)
                        (list (text ".") (sexp->doc* lst profile)))))]
        [else
         (loop (cdr lst) (cons (sexp->doc* (car lst) profile) acc))])))

;;; Format a form with its rule
(define (format-form head tail body-start break-style profile)
  (let* ([head-doc (sexp->doc* head profile)]
         [indent-amt (style-indent profile)]
         [all-docs (map (lambda (x) (sexp->doc* x profile)) tail)])
        (cond
         ;; Empty tail - just the head in parens
         [(null? tail)
          (parens head-doc)]
         ;; Force single line
         [(eq? break-style 'never)
          (parens (hsep (cons head-doc all-docs)))]
         ;; Force multi-line
         [(eq? break-style 'always)
          (format-multiline head-doc all-docs body-start indent-amt)]
         ;; Auto mode - let pretty printer decide
         [else
          (group
           (parens
            (<> head-doc
                (nest indent-amt
                      (<> line
                          (sep all-docs))))))])))

;;; Format multiline form
(define (format-multiline head-doc body-docs body-start indent-amt)
  (if (null? body-docs)
      (parens head-doc)
      (let* ([pre-body (take-n body-docs (- body-start 1))]
             [body (drop-n body-docs (- body-start 1))]
             [pre-docs (if (null? pre-body)
                           empty
                           (<+> (hsep pre-body) empty))])
            (parens
             (<> head-doc
                 (<> (if (null? pre-body) empty (text " "))
                     (<> pre-docs
                         (nest indent-amt
                               (if (null? body)
                                   empty
                                   (<> line (vsep body)))))))))))

;;; Convert list to Doc (diff-friendly mode - one per line)
(define (list->doc-diff-friendly lst profile)
  (cond
   [(null? lst) (text "()")]
   ;; Handle dotted pairs
   [(not (proper-list? lst))
    (dotted-pair->doc lst profile)]
   ;; Handle quote shorthands
   [(and (pair? lst)
         (= (length lst) 2)
         (symbol? (car lst))
         (assq (car lst) *quote-shorthands*))
    (let ([shorthand (cdr (assq (car lst) *quote-shorthands*))])
         (<> (text shorthand) (sexp->doc* (cadr lst) profile)))]
   ;; Normal list handling
   [else
    (let* ([docs (map (lambda (x) (sexp->doc* x profile)) lst)]
           [indent-amt (style-indent profile)])
          ;; Simple forms on one line, complex on multiple
          (if (and (style-collapse-simple profile)
                   (simple-form? lst))
              (parens (hsep docs))
              (parens
               (nest indent-amt
                     (vcat docs)))))]))

;;; Check if form is simple enough for one line
(define (simple-form? sexp)
  (cond
   [(null? sexp) #t]
   [(not (pair? sexp)) #t]
   [(> (length sexp) 4) #f]  ; Too many elements
   [else
    (andmap (lambda (x)
                    (or (symbol? x)
                        (number? x)
                        (boolean? x)
                        (string? x)
                        (and (pair? x) (< (length x) 3))))
            sexp)]))

;;; ====
;;; List Utilities
;;; ====

(define (take-n lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-n (cdr lst) (- n 1)))))

(define (drop-n lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop-n (cdr lst) (- n 1))))

;;; ====
;;; Comment Preservation
;;; ====
;;;
;;; Comments are extracted before parsing, then reinserted after.
;;; This is a simplified approach - full comment preservation would
;;; require a custom reader.

;;; Extract comments from source code
;;; Returns: (list (line-number . comment-text) ...)
(define (extract-comments source)
  (let* ([lines (string-split-lines source)]
         [indexed (zip-with-index lines)])
        (filter-map
         (lambda (pair)
                 (let ([line-num (car pair)]
                       [line (cdr pair)])
                      (let ([trimmed (string-trim line)])
                           (if (and (> (string-length trimmed) 0)
                                    (char=? (string-ref trimmed 0) #\;))
                               (cons line-num trimmed)
                               #f))))
         indexed)))

;;; Split string into lines
(define (string-split-lines str)
  (let loop ([s str] [acc '()] [current '()])
       (if (= (string-length s) 0)
           (reverse (cons (list->string (reverse current)) acc))
           (let ([c (string-ref s 0)]
                 [rest (substring s 1 (string-length s))])
                (if (char=? c #\newline)
                    (loop rest
                          (cons (list->string (reverse current)) acc)
                          '())
                    (loop rest acc (cons c current)))))))

;;; Zip list with indices
(define (zip-with-index lst)
  (let loop ([lst lst] [i 0] [acc '()])
       (if (null? lst)
           (reverse acc)
           (loop (cdr lst) (+ i 1) (cons (cons i (car lst)) acc)))))

;;; filter-map provided by prelude

;;; ====
;;; Main API
;;; ====

;;; Format a string of Scheme code
(define (ast-format-string code . args)
  (let ([profile (if (null? args)
                     *current-profile*
                     (let ([p (car args)])
                          (if (symbol? p)
                              (get-profile p)
                              p)))])
       (guard (e [else
                  ;; Return original on parse error
                  (display (format "Format error: ~a\n"
                                   (if (condition? e)
                                       (condition-message e)
                                       e)))
                  code])
              ;; Extract leading comments
              (let* ([comments (extract-comments code)]
                     [comment-block (format-comment-block comments)]
                     ;; Parse expressions from string
                     [exprs (read-all-from-string code)]
                     ;; Format each expression
                     [docs (map (lambda (e) (sexp->doc* e profile)) exprs)]
                     ;; Render with blank lines between top-level forms
                     [formatted (format-top-level docs (style-width profile))])
                    (if (string=? comment-block "")
                        formatted
                        (string-append comment-block "\n" formatted))))))

;;; Read all S-expressions from a string
(define (read-all-from-string str)
  (guard (e [else '()])
         (let ([port (open-input-string str)])
              (let loop ([acc '()])
                   (let ([expr (read port)])
                        (if (eof-object? expr)
                            (reverse acc)
                            (loop (cons expr acc))))))))

;;; Format leading comment block
(define (format-comment-block comments)
  (let ([header-comments (take-while-header comments)])
       (if (null? header-comments)
           ""
           (string-join (map cdr header-comments) "\n"))))

;;; Take comments from the header (consecutive from line 0)
(define (take-while-header comments)
  (let loop ([cs comments] [expected 0] [acc '()])
       (if (or (null? cs)
               (not (= (car (car cs)) expected)))
           (reverse acc)
           (loop (cdr cs) (+ expected 1) (cons (car cs) acc)))))

;;; Format top-level forms with blank lines between them
(define (format-top-level docs width)
  (if (null? docs)
      ""
      (let ([rendered (map (lambda (d) (pretty width d)) docs)])
           (string-join rendered "\n\n"))))

;;; Format a file
(define (ast-format-file path . args)
  (let ([profile (if (null? args) *current-profile* (car args))])
       (guard (e [else
                  (display (format "Error formatting ~a: ~a\n"
                                   path
                                   (if (condition? e)
                                       (condition-message e)
                                       e)))
                  #f])
              (let* ([original (read-file-string path)]
                     [formatted (ast-format-string original profile)])
                    (if (string=? (normalize-ws original) (normalize-ws formatted))
                        (begin
                         (display (format "~a: already formatted\n" path))
                         #f)
                        (begin
                         (write-file-string path formatted)
                         (display (format "~a: formatted\n" path))
                         #t))))))

;;; Check if file needs formatting
(define (ast-format-check path . args)
  (let ([profile (if (null? args) *current-profile* (car args))])
       (guard (e [else #t])
              (let* ([original (read-file-string path)]
                     [formatted (ast-format-string original profile)])
                    (not (string=? (normalize-ws original) (normalize-ws formatted)))))))

;;; Show formatting diff
(define (ast-format-diff path . args)
  (let ([profile (if (null? args) *current-profile* (car args))])
       (guard (e [else (format "Error: ~a\n" e)])
              (let* ([original (read-file-string path)]
                     [formatted (ast-format-string original profile)])
                    (if (string=? (normalize-ws original) (normalize-ws formatted))
                        "No changes needed.\n"
                        (simple-diff original formatted))))))

;;; Format an expression directly
(define (ast-format-expr expr . args)
  (let ([profile (if (null? args) *current-profile* (car args))])
       (pretty (style-width profile) (sexp->doc* expr profile))))

;;; ====
;;; File I/O Helpers
;;; ====

(define (read-file-string path)
  (call-with-input-file path
                        (lambda (port)
                                (let loop ([lines '()])
                                     (let ([line (get-line port)])
                                          (if (eof-object? line)
                                              (string-join (reverse lines) "\n")
                                              (loop (cons line lines))))))))

(define (write-file-string path content)
  (call-with-output-file path
                         (lambda (port)
                                 (display content port)
                                 (newline port))
                         'replace))

;;; ====
;;; String Utilities
;;; ====

;;; Normalize whitespace for comparison
(define (normalize-ws str)
  (let ([chars (string->list str)])
       (list->string
        (let loop ([cs chars] [acc '()] [prev-ws? #f])
             (if (null? cs)
                 (reverse acc)
                 (let ([c (car cs)])
                      (if (char-whitespace? c)
                          (if prev-ws?
                              (loop (cdr cs) acc #t)
                              (loop (cdr cs) (cons #\space acc) #t))
                          (loop (cdr cs) (cons c acc) #f))))))))

;;; Simple diff (line-based)
(define (simple-diff original formatted)
  (let* ([orig-lines (string-split-lines original)]
         [fmt-lines (string-split-lines formatted)]
         [diffs (compute-line-diffs orig-lines fmt-lines)])
        (string-join
         (cons "--- original"
               (cons "+++ formatted"
                     (map format-diff-line diffs)))
         "\n")))

;;; Compute simple line differences
(define (compute-line-diffs orig fmt)
  (let loop ([o orig] [f fmt] [n 1] [acc '()])
       (cond
        [(and (null? o) (null? f))
         (reverse acc)]
        [(null? o)
         (loop '() (cdr f) (+ n 1)
               (cons (list '+ n (car f)) acc))]
        [(null? f)
         (loop (cdr o) '() (+ n 1)
               (cons (list '- n (car o)) acc))]
        [(string=? (car o) (car f))
         (loop (cdr o) (cdr f) (+ n 1) acc)]
        [else
         (loop (cdr o) (cdr f) (+ n 1)
               (cons (list '! n (car o) (car f)) acc))])))

;;; Format a diff line
(define (format-diff-line d)
  (case (car d)
        [(+) (format "@@ +~a @@\n+~a" (cadr d) (caddr d))]
        [(-) (format "@@ -~a @@\n-~a" (cadr d) (caddr d))]
        [(!) (format "@@ ~a @@\n-~a\n+~a" (cadr d) (caddr d) (cadddr d))]
        [else ""]))

;;; ====
;;; Profile Configuration
;;; ====

;;; Set current profile
(define (set-format-profile! name)
  (set! *current-profile* (get-profile name))
  (display (format "Format profile set to: ~a\n" name)))

;;; Show current profile
(define (show-format-profile)
  (let ([p *current-profile*])
       (display (format "Current profile:
  width:           ~a
  indent:          ~a
  break-threshold: ~a
  one-per-line:    ~a
  align-bindings:  ~a
  collapse-simple: ~a\n"
                        (style-width p)
                        (style-indent p)
                        (style-break-threshold p)
                        (style-one-per-line p)
                        (style-align-bindings p)
                        (style-collapse-simple p)))))

;;; ====
;;; REPL Commands
;;; ====

;;; Format code from REPL
(define (fmt code . args)
  (let ([profile (if (null? args) *current-profile* (get-profile (car args)))])
       (display (ast-format-string code profile))
       (newline)))

;;; Format file from REPL
(define (fmt-file path . args)
  (let ([profile (if (null? args) 'compact (car args))])
       (ast-format-file path (get-profile profile))))

;;; Check file formatting
(define (fmt-check path . args)
  (let ([profile (if (null? args) 'compact (car args))])
       (if (ast-format-check path (get-profile profile))
           (display (format "~a: needs formatting\n" path))
           (display (format "~a: OK\n" path)))))

;;; Show diff
(define (fmt-diff path . args)
  (let ([profile (if (null? args) 'compact (car args))])
       (display (ast-format-diff path (get-profile profile)))))

;;; ====
;;; Help
;;; ====

(define (ast-format-help)
  (display "
AST-Aware Code Formatter
====

Profiles:
  'compact      - Standard style (default)
  'verbose      - More line breaks
  'diff-friendly - One item per line for clean diffs
  'canonical    - Wider lines, conservative breaks

Functions:
  (ast-format-string code [profile])  - Format code string
  (ast-format-file path [profile])    - Format file in place
  (ast-format-check path [profile])   - Check if needs formatting
  (ast-format-diff path [profile])    - Show diff
  (ast-format-expr expr [profile])    - Format expression

REPL shortcuts:
  (fmt code [profile])      - Format code string
  (fmt-file path [profile]) - Format file
  (fmt-check path)          - Check formatting
  (fmt-diff path)           - Show diff

Configuration:
  (set-format-profile! name) - Set default profile
  (show-format-profile)      - Show current settings

"))

;;; ====
;;; Load Message
;;; ====

(display "AST-aware formatter loaded. Use (ast-format-help) for help.\n")
