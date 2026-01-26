(load "core/base/prelude.ss")

(doc 'module 'exports)
(doc 'description "Quick export discovery for files without requiring manifest.sexp")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'core-extraction)

(define (exports-of filepath)
  (doc 'description "Extract all top-level definitions from a Scheme file")
  (doc 'param 'filepath "Path to .ss file to analyze")
  (doc 'returns "(List Symbol) - Sorted list of defined symbols")
  (guard (e [else
             (printf "Error reading ~a: ~a\n"
                     filepath
                     (if (message-condition? e)
                         (condition-message e)
                         e))
             '()])
    (if (not (file-exists? filepath))
        (begin (printf "File not found: ~a\n" filepath) '())
        (let ([defs (collect-file-definitions filepath)])
          (list-sort symbol<? (map car defs))))))

(define (collect-file-definitions filepath)
  (doc 'description "Collect all definitions from a file")
  (doc 'note "Warns on read errors rather than silently truncating")
  (call-with-input-file filepath
    (lambda (port)
      (let loop ([results '()])
        (let-values ([(expr err) (safe-read port)])
          (cond
            [(eof-object? expr) (reverse results)]
            [err
             ;; Read error - warn and return what we have
             (printf "Warning: Read error in ~a: ~a\n" filepath err)
             (printf "  (returning ~a definitions found before error)\n"
                     (length results))
             (reverse results)]
            [else
             (let ([defs (extract-definitions expr)])
               (loop (append defs results)))]))))))

(define (safe-read port)
  (doc 'description "Read from port with error recovery")
  (doc 'returns "(values expr #f) on success, (values #f error-msg) on error")
  (guard (e [else
             (values #f (if (message-condition? e)
                            (condition-message e)
                            (format "~a" e)))])
    (let ([expr (read port)])
      (values expr #f))))

(define (extract-definitions expr)
  (doc 'description "Extract defined names from a top-level expression")
  (doc 'returns "(List (Symbol . Symbol)) - List of (name . form-type) pairs")
  (cond
    [(not (pair? expr)) '()]
    ;; (define name value) or (define (name args...) body)
    [(eq? (car expr) 'define)
     (cond
       [(and (pair? (cdr expr)) (symbol? (cadr expr)))
        (list (cons (cadr expr) 'define))]
       [(and (pair? (cdr expr)) (pair? (cadr expr)) (symbol? (caadr expr)))
        (list (cons (caadr expr) 'define))]
       [else '()])]
    ;; (define-syntax name ...)
    [(eq? (car expr) 'define-syntax)
     (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
         (list (cons (cadr expr) 'syntax))
         '())]
    ;; (define-record-type name ...) - extract record name
    [(eq? (car expr) 'define-record-type)
     (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
         (list (cons (cadr expr) 'record))
         '())]
    ;; (define-protocol (name ...) ...) - extract protocol name
    [(eq? (car expr) 'define-protocol)
     (if (and (pair? (cdr expr)) (pair? (cadr expr)) (symbol? (caadr expr)))
         (list (cons (caadr expr) 'protocol))
         '())]
    ;; (define-protocol/default (name ...) ...) - same pattern
    [(eq? (car expr) 'define-protocol/default)
     (if (and (pair? (cdr expr)) (pair? (cadr expr)) (symbol? (caadr expr)))
         (list (cons (caadr expr) 'protocol))
         '())]
    [else '()]))

(define (symbol<? a b)
  (doc 'description "Alphabetical comparison for symbols")
  (string<? (symbol->string a) (symbol->string b)))

(doc 'section 'categorization)

(define (categorize-symbol sym)
  (doc 'description "Categorize symbols by naming convention")
  (let ([name (symbol->string sym)])
    (cond
      ;; Predicates end with ?
      [(and (> (string-length name) 1)
            (char=? (string-ref name (- (string-length name) 1)) #\?))
       'predicate]
      ;; Constructors start with make-
      [(string-prefix? "make-" name) 'constructor]
      ;; Accessors often have - in middle after type name
      [(string-contains-char? name #\-) 'accessor-or-op]
      ;; Constants/instances (no dashes, not predicates)
      [else 'value])))

(define (string-prefix? prefix str)
  (doc 'description "Check if string starts with prefix")
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

(define (string-contains-char? str ch)
  (doc 'description "Check if string contains character")
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #f]
      [(char=? (string-ref str i) ch) #t]
      [else (loop (+ i 1))])))

(define (group-exports symbols)
  (doc 'description "Group symbols by category")
  (let ([groups (make-eq-hashtable)])
    (for-each
     (lambda (sym)
       (let* ([cat (categorize-symbol sym)]
              [existing (hashtable-ref groups cat '())])
         (hashtable-set! groups cat (cons sym existing))))
     symbols)
    ;; Convert to sorted alist
    (map (lambda (cat)
           (cons cat (reverse (hashtable-ref groups cat '()))))
         '(constructor predicate accessor-or-op value))))

(doc 'section 'pretty-printing)

(define (exports-of-pretty filepath)
  (doc 'description "Pretty-print exports grouped by category")
  (let ([symbols (exports-of filepath)])
    (if (null? symbols)
        (printf "No exports found in ~a\n" filepath)
        (begin
          (printf "\nExports from ~a (~a symbols)\n" filepath (length symbols))
          (printf "~a\n\n" (make-string 60 #\─))
          (let ([groups (group-exports symbols)])
            (for-each
             (lambda (group)
               (let ([cat (car group)]
                     [syms (cdr group)])
                 (unless (null? syms)
                   (printf "~a (~a):\n"
                           (category-label cat)
                           (length syms))
                   (print-symbol-columns syms 3 25)
                   (newline))))
             groups))
          (printf "~a\n" (make-string 60 #\─))))))

(define (category-label cat)
  (doc 'description "Convert category symbol to display label")
  (case cat
    [(constructor) "Constructors"]
    [(predicate) "Predicates"]
    [(accessor-or-op) "Accessors & Operations"]
    [(value) "Values & Instances"]
    [else "Other"]))

(define (print-symbol-columns syms cols width)
  (doc 'description "Print symbols in columns")
  (let loop ([remaining syms] [col 0])
    (unless (null? remaining)
      (let ([sym (car remaining)])
        (if (= col 0) (display "  "))
        (display (pad-right (symbol->string sym) width))
        (if (= col (- cols 1))
            (begin (newline) (loop (cdr remaining) 0))
            (loop (cdr remaining) (+ col 1))))))
  ;; Final newline if we didn't end on column 0
  (let ([rem (remainder (length syms) cols)])
    (when (> rem 0) (newline))))

(define (pad-right str width)
  (doc 'description "Pad string to width with spaces on right")
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append str (make-string (- width len) #\space)))))

(doc 'section 'quick-summary)

(define (exports-of-summary filepath)
  (doc 'description "One-line summary of exports")
  (let ([symbols (exports-of filepath)])
    (if (null? symbols)
        (printf "~a: (no exports)\n" filepath)
        (let ([groups (group-exports symbols)])
          (printf "~a: ~a exports ("
                  filepath (length symbols))
          (display
           (string-join
            (filter-map
             (lambda (g)
               (let ([n (length (cdr g))])
                 (if (> n 0)
                     (format "~a ~a" n (short-category (car g)))
                     #f)))
             groups)
            ", "))
          (printf ")\n")))))

(define (short-category cat)
  (doc 'description "Convert category symbol to short label")
  (case cat
    [(constructor) "constructors"]
    [(predicate) "predicates"]
    [(accessor-or-op) "ops"]
    [(value) "values"]
    [else "other"]))

;; string-join, filter-map provided by prelude

(doc 'section 'convenience-aliases)

(define (lef filepath)
  (doc 'description "Quick alias for exports-of-pretty (lattice export file)")
  (exports-of-pretty filepath))

(doc 'section 'repl-interface)

(printf "\nexports.ss loaded — Quick export discovery\n")
(printf "  (exports-of \"file.ss\")        - List exported symbols\n")
(printf "  (exports-of-pretty \"file.ss\") - Pretty-print with grouping\n")
(printf "  (lef \"file.ss\")               - Quick alias for pretty-print\n")
(printf "  (exports-of-summary \"file.ss\") - One-line summary\n")
