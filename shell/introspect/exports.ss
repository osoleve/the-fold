;;; shell/introspect/exports.ss — Quick Export Discovery for Files
;;;
;;; Discover what symbols a file defines without needing a manifest.sexp.
;;; Useful for understanding module APIs during development.
;;;
;;; This is Shell code: performs file I/O, parses Scheme source.
;;;
;;; Usage:
;;;   (exports-of "lattice/fp/templates.ss")      ; List exported symbols
;;;   (exports-of-pretty "lattice/fp/templates.ss") ; Pretty-print with grouping
;;;   (lef "lattice/fp/templates.ss")             ; Quick alias
;;;
;;; Features:
;;;   - Extracts all top-level (define ...) and (define-syntax ...) forms
;;;   - Groups by category (predicates, constructors, operations)
;;;   - Works with any .ss file, no manifest required
;;;
;;; Dependencies:
;;;   core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Core Extraction
;;; ====

;;; exports-of : String -> (List Symbol)
;;; Extract all top-level definitions from a Scheme file.
;;; Returns list of defined symbol names, sorted alphabetically.
(define (exports-of filepath)
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

;;; collect-file-definitions : String -> (List Symbol)
;;; Collect all definitions from a file.
;;; Warns on read errors rather than silently truncating.
(define (collect-file-definitions filepath)
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

;;; safe-read : Port -> (Values S-expr (or String #f))
;;; Read from port, returning (values result #f) on success,
;;; or (values #f error-message) on error.
(define (safe-read port)
  (guard (e [else
             (values #f (if (message-condition? e)
                            (condition-message e)
                            (format "~a" e)))])
    (let ([expr (read port)])
      (values expr #f))))

;;; extract-definitions : S-expr -> (List (Symbol . Symbol))
;;; Extract defined names from a top-level expression.
;;; Returns list of (name . form-type) pairs.
(define (extract-definitions expr)
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

;;; symbol<? : Symbol x Symbol -> Boolean
;;; Alphabetical comparison for symbols.
(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

;;; ====
;;; Categorization
;;; ====

;;; Categorize symbols by naming convention
(define (categorize-symbol sym)
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

;;; string-prefix? : String x String -> Boolean
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

;;; string-contains-char? : String x Char -> Boolean
(define (string-contains-char? str ch)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #f]
      [(char=? (string-ref str i) ch) #t]
      [else (loop (+ i 1))])))

;;; group-exports : (List Symbol) -> Alist
;;; Group symbols by category.
(define (group-exports symbols)
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

;;; ====
;;; Pretty Printing
;;; ====

;;; exports-of-pretty : String -> void
;;; Pretty-print exports grouped by category.
(define (exports-of-pretty filepath)
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

;;; category-label : Symbol -> String
(define (category-label cat)
  (case cat
    [(constructor) "Constructors"]
    [(predicate) "Predicates"]
    [(accessor-or-op) "Accessors & Operations"]
    [(value) "Values & Instances"]
    [else "Other"]))

;;; print-symbol-columns : (List Symbol) x Nat x Nat -> void
;;; Print symbols in columns.
(define (print-symbol-columns syms cols width)
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

;;; pad-right : String x Nat -> String
(define (pad-right str width)
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append str (make-string (- width len) #\space)))))

;;; ====
;;; Quick Summary
;;; ====

;;; exports-of-summary : String -> void
;;; One-line summary of exports.
(define (exports-of-summary filepath)
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

;;; short-category : Symbol -> String
(define (short-category cat)
  (case cat
    [(constructor) "constructors"]
    [(predicate) "predicates"]
    [(accessor-or-op) "ops"]
    [(value) "values"]
    [else "other"]))

;;; string-join : (List String) x String -> String
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; filter-map helper
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let ([result (f (car lst))])
          (if result
              (loop (cdr lst) (cons result acc))
              (loop (cdr lst) acc))))))

;;; ====
;;; Convenience Aliases
;;; ====

;;; lef : String -> void
;;; Quick "lattice export file" - pretty-print exports
(define (lef filepath)
  (exports-of-pretty filepath))

;;; ====
;;; REPL Interface
;;; ====

(printf "\nexports.ss loaded — Quick export discovery\n")
(printf "  (exports-of \"file.ss\")        - List exported symbols\n")
(printf "  (exports-of-pretty \"file.ss\") - Pretty-print with grouping\n")
(printf "  (lef \"file.ss\")               - Quick alias for pretty-print\n")
(printf "  (exports-of-summary \"file.ss\") - One-line summary\n")
