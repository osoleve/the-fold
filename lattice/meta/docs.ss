(unless (top-level-bound? 'hamt-empty) (load "lattice/data/hamt.ss"))

(doc 'module 'docs)
(doc 'description "S-expression doc form extraction for search and inspection")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'state)

;; Respect existing *meta-quiet* if set, otherwise default to #f
(define *docs-quiet*
  (and (top-level-bound? '*meta-quiet*)
       (top-level-value '*meta-quiet*)))

;;; Doc index: list of (file line tag content target?)
(define *doc-index* '())

;;; Initialization flag: distinguishes "not yet built" from "built but empty"
(define *doc-index-built?* #f)

(doc 'section 'pure-parsing)

;;; doc-form? : SExp -> Boolean
;;; Check if expression is a doc form
(define (doc-form? sexp)
  (and (pair? sexp)
       (eq? (car sexp) 'doc)
       (pair? (cdr sexp))))

;;; parse-doc-form : SExp -> (tag content target?) | #f
;;; Parse a doc form, returning its components
;;; Contextual: (doc 'tag content...) -> ('tag content #f)
;;; Targeted:   (doc target 'tag content...) -> ('tag content target)
(define (parse-doc-form sexp)
  (if (not (doc-form? sexp))
      #f
      (let ([args (cdr sexp)])
        (cond
          ;; Empty doc
          [(null? args) #f]
          ;; Check if first arg is quoted symbol (contextual)
          [(and (pair? (car args))
                (eq? (caar args) 'quote)
                (symbol? (cadar args)))  ; Tag must be a symbol
           ;; Contextual: (doc 'tag content...)
           (list (cadar args)                    ; tag
                 (if (null? (cdr args)) '() (cdr args))  ; content
                 #f)]                            ; no target
          ;; Check if first arg is bare symbol (targeted)
          [(symbol? (car args))
           ;; Targeted: (doc target 'tag content...)
           (if (and (pair? (cdr args))
                    (pair? (cadr args))
                    (eq? (caadr args) 'quote)
                    (symbol? (cadadr args)))  ; Tag must be a symbol
               (list (cadadr args)               ; tag
                     (if (null? (cddr args)) '() (cddr args))  ; content
                     (car args))                 ; target
               #f)]
          [else #f]))))

;;; extract-docs-from-sexp : SExp × Int -> (List (line tag content target?))
;;; Recursively extract doc forms from an S-expression
(define (extract-docs-from-sexp sexp line)
  (cond
    [(not (pair? sexp)) '()]
    [(doc-form? sexp)
     (let ([parsed (parse-doc-form sexp)])
       (if parsed
           (list (cons line parsed))
           '()))]
    [(not (list? sexp)) '()]  ; Skip improper lists (dotted pairs)
    [else
     (append-map (lambda (sub) (extract-docs-from-sexp sub line))
                 sexp)]))

(doc 'section 'cache-population)

;;; populate-doc-index! : (List (file line tag content target?)) -> Void
;;; Populate the doc index from pre-parsed entries.
;;; Called from boundary orchestrator after I/O.
(define (populate-doc-index! entries)
  (set! *doc-index* entries)
  (set! *doc-index-built?* #t))

(doc 'section 'query-api)

;;; lf-docs : Symbol -> (List Doc)
;;; Find all docs with a specific tag
(define (lf-docs tag)
  (filter (lambda (doc)
            (eq? (caddr doc) tag))  ; doc = (file line tag content target?)
          *doc-index*))

;;; docs-for : Symbol -> (List Doc)
;;; Find all docs targeting a specific symbol
(define (docs-for target)
  (filter (lambda (doc)
            (eq? (list-ref doc 4) target))  ; 5th element is target
          *doc-index*))

;;; doc-stats : -> (Alist Tag Nat)
;;; Count docs by tag
(define (doc-stats)
  (let ([counts (fold-left
                  (lambda (acc doc)
                    (let* ([tag (caddr doc)]
                           [current (hamt-lookup-or tag acc 0)])
                      (hamt-assoc tag (+ current 1) acc)))
                  hamt-empty
                  *doc-index*)])
    (hamt-entries counts)))

(doc 'section 'pretty-printing)

;;; print-docs : (List Doc) -> Void
;;; Pretty print a list of docs
(define (print-docs docs)
  (for-each
   (lambda (doc)
     (let ([file (car doc)]
           [line (cadr doc)]
           [tag (caddr doc)]
           [content (cadddr doc)]
           [target (if (> (length doc) 4) (list-ref doc 4) #f)])
       (printf "~a:~a  [~a]" file line tag)
       (when target
         (printf " -> ~a" target))
       (newline)
       (unless (null? content)
         (printf "  ~s~n" content))))
   docs))

(doc 'section 'convenience-aliases)

;;; Quick search aliases
(define (lf-todo) (print-docs (lf-docs 'todo)))
(define (lf-fixme) (print-docs (lf-docs 'fixme)))
(define (lf-types) (print-docs (lf-docs 'type)))
(define (lf-deprecated) (print-docs (lf-docs 'deprecated)))
