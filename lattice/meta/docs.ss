;;; lattice/meta/docs.ss — S-expression Doc Form Extraction
;;;
;;; Extract (doc ...) forms from source files for search and inspection.
;;; Unlike ;;; comments (which are stripped by reader), doc forms are
;;; first-class S-expressions that survive in source.
;;;
;;; Syntax:
;;;   (doc 'tag content...)        ; contextual - belongs to enclosing def
;;;   (doc target 'tag content...) ; targeted - names what it documents
;;;
;;; Standard tags: 'type, 'description, 'param, 'returns, 'todo, 'fixme,
;;;                'deprecated, 'since, 'see, 'note
;;;
;;; Usage:
;;;   (lf-docs 'todo)              ; Find all todos
;;;   (lf-docs 'type)              ; Find all type annotations
;;;   (docs-for 'add)              ; Find docs for specific target
;;;   (doc-stats)                  ; Summary counts by tag
;;;
;;; This is Lattice code: impure (reads files), used during indexing.

;;; ====
;;; Dependencies
;;; ====

;; Respect existing *meta-quiet* if set, otherwise default to #f
(define *docs-quiet*
  (and (top-level-bound? '*meta-quiet*)
       (top-level-value '*meta-quiet*)))

;;; ====
;;; State
;;; ====

;;; Doc index: list of (file line tag content target?)
(define *doc-index* '())

;;; Initialization flag: distinguishes "not yet built" from "built but empty"
(define *doc-index-built?* #f)

;;; ====
;;; File Reading
;;; ====

;;; read-all-sexps : String -> (List SExp) | #f
;;; Read all S-expressions from a file
(define (read-all-sexps path)
  (guard (e [else #f])
    (call-with-input-file path
      (lambda (port)
        (let loop ([acc '()])
          (let ([sexp (read port)])
            (if (eof-object? sexp)
                (reverse acc)
                (loop (cons sexp acc)))))))))

;;; ====
;;; Doc Form Extraction
;;; ====

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
     (apply append
            (map (lambda (sub) (extract-docs-from-sexp sub line))
                 sexp))]))

;;; extract-docs-from-file : String -> (List (file line tag content target?))
;;; Extract all doc forms from a source file
(define (extract-docs-from-file path)
  (let ([sexps (read-all-sexps path)])
    (if (not sexps)
        '()
        (let loop ([sexps sexps] [line 1] [acc '()])
          (if (null? sexps)
              acc
              (let* ([sexp (car sexps)]
                     [docs (extract-docs-from-sexp sexp line)]
                     [tagged (map (lambda (d) (cons path d)) docs)])
                (loop (cdr sexps)
                      (+ line 1)  ; Approximate - real line tracking would need reader
                      (append acc tagged))))))))

;;; ====
;;; Indexing
;;; ====

;;; find-scheme-files : String -> (List String)
;;; Find all .ss files under a directory
(define (find-scheme-files root)
  (define (walk dir)
    (guard (e [else '()])
      (let ([entries (directory-list dir)])
        (apply append
               (map (lambda (entry)
                      (let ([path (string-append dir "/" entry)])
                        (cond
                          [(and (> (string-length entry) 3)
                                (string=? ".ss" (substring entry
                                                           (- (string-length entry) 3)
                                                           (string-length entry))))
                           (list path)]
                          [(and (file-directory? path)
                                (not (char=? (string-ref entry 0) #\.)))
                           (walk path)]
                          [else '()])))
                    entries)))))
  (if (file-directory? root)
      (walk root)
      (if (file-exists? root) (list root) '())))

;;; build-doc-index! : -> Void
;;; Build the doc index from the codebase
;;; Uses cons+reverse pattern to avoid O(N²) append overhead
(define (build-doc-index!)
  (let ([roots '("core" "lattice" "boundary")]
        [acc '()])
    (for-each
     (lambda (root)
       (let ([files (find-scheme-files root)])
         (for-each
          (lambda (file)
            (let ([docs (extract-docs-from-file file)])
              ;; Prepend docs (constant time) instead of append (linear time)
              (set! acc (append docs acc))))
          files)))
     roots)
    ;; Reverse once at the end to restore file order
    (set! *doc-index* (reverse acc))
    (set! *doc-index-built?* #t))
  (unless *docs-quiet*
    (printf "Doc index built: ~a entries~n" (length *doc-index*))))

;;; ====
;;; Search Functions
;;; ====

;;; ensure-doc-index! : -> Void
;;; Build index if not yet built (handles empty index correctly)
(define (ensure-doc-index!)
  (unless *doc-index-built?*
    (build-doc-index!)))

;;; lf-docs : Symbol -> (List Doc)
;;; Find all docs with a specific tag
(define (lf-docs tag)
  (ensure-doc-index!)
  (filter (lambda (doc)
            (eq? (caddr doc) tag))  ; doc = (file line tag content target?)
          *doc-index*))

;;; docs-for : Symbol -> (List Doc)
;;; Find all docs targeting a specific symbol
(define (docs-for target)
  (ensure-doc-index!)
  (filter (lambda (doc)
            (eq? (list-ref doc 4) target))  ; 5th element is target
          *doc-index*))

;;; doc-stats : -> (Alist Tag Nat)
;;; Count docs by tag
(define (doc-stats)
  (ensure-doc-index!)
  (let ([counts (make-hashtable equal-hash equal?)])  ; Use equal? for non-symbol tags
    (for-each
     (lambda (doc)
       (let* ([tag (caddr doc)]
              [current (hashtable-ref counts tag 0)])
         (hashtable-set! counts tag (+ current 1))))
     *doc-index*)
    (let-values ([(keys vals) (hashtable-entries counts)])
      (map cons (vector->list keys) (vector->list vals)))))

;;; ====
;;; Pretty Printing
;;; ====

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

;;; ====
;;; Convenience Aliases
;;; ====

;;; Quick search aliases
(define (lf-todo) (print-docs (lf-docs 'todo)))
(define (lf-fixme) (print-docs (lf-docs 'fixme)))
(define (lf-types) (print-docs (lf-docs 'type)))
(define (lf-deprecated) (print-docs (lf-docs 'deprecated)))

;;; Rebuild index (resets flag, forces fresh build)
(define (doc-reindex!)
  (set! *doc-index-built?* #f)
  (build-doc-index!))
