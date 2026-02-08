(doc 'module 'xref)
(doc 'description "Cross-reference tracking for function call relationships and impact analysis")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'state)

;;; Call graph indices
;;; callers: symbol -> (list of symbols that call it)
;;; callees: symbol -> (list of symbols it calls)
(define *xref-callers* (make-hashtable symbol-hash eq?))
(define *xref-callees* (make-hashtable symbol-hash eq?))

;;; Set of all known definitions (for filtering)
(define *xref-known-defs* (make-eq-hashtable))

(doc 'section 'symbol-extraction)

;;; extract-symbols-from-sexp : SExp -> (List Symbol)
;;; Extract all symbols from an s-expression (excluding quoted data)
(define (extract-symbols-from-sexp sexp)
  (cond
    [(symbol? sexp) (list sexp)]
    [(pair? sexp)
     (case (car sexp)
       ;; Skip quoted expressions
       [(quote quasiquote) '()]
       ;; Handle define specially - skip the name being defined
       [(define)
        (if (and (pair? (cdr sexp)) (pair? (cddr sexp)))
            (let ([name-part (cadr sexp)])
              ;; Skip the function name, extract from body
              (append-map extract-symbols-from-sexp (cddr sexp)))
            '())]
       ;; Handle let/let*/letrec - skip binding names
       ;; Also handle named let: (let loop ([x 1]) body...)
       [(let let* letrec)
        (cond
          ;; Named let: (let name ([var init] ...) body...)
          [(and (pair? (cdr sexp))
                (symbol? (cadr sexp))
                (pair? (cddr sexp))
                (pair? (caddr sexp)))
           (let ([bindings (caddr sexp)]
                 [body (cdddr sexp)])
             (append
              ;; Extract from binding values
              (append-map (lambda (b)
                            (if (and (pair? b) (pair? (cdr b)))
                                (extract-symbols-from-sexp (cadr b))
                                '()))
                          (if (list? bindings) bindings '()))
              ;; Extract from body
              (append-map extract-symbols-from-sexp body)))]
          ;; Regular let: (let ([var init] ...) body...)
          [(and (pair? (cdr sexp)) (pair? (cadr sexp)))
           (let ([bindings (cadr sexp)]
                 [body (cddr sexp)])
             (append
              ;; Extract from binding values (not names)
              (append-map (lambda (b)
                            (if (and (pair? b) (pair? (cdr b)))
                                (extract-symbols-from-sexp (cadr b))
                                '()))
                          (if (list? bindings) bindings '()))
              ;; Extract from body
              (append-map extract-symbols-from-sexp body)))]
          [else '()])]
       ;; Handle lambda - skip parameter names
       [(lambda)
        (if (pair? (cddr sexp))
            (append-map extract-symbols-from-sexp (cddr sexp))
            '())]
       ;; Default: recurse into all parts
       [else
        (append-map extract-symbols-from-sexp sexp)])]
    [else '()]))

(doc 'section 'toplevel-extraction)

;;; extract-xref-from-toplevel : SExp -> (Symbol . (List Symbol)) | #f
;;; Extract cross-reference info from a top-level form
(define (extract-xref-from-toplevel sexp)
  (cond
    ;; (define (name ...) body...)
    [(and (pair? sexp)
          (eq? (car sexp) 'define)
          (pair? (cdr sexp)))
     (let ([name (get-define-name sexp)])
       (if name
           (let* ([body (cddr sexp)]
                  [refs (append-map extract-symbols-from-sexp body)]
                  [unique-refs (remove-duplicates-sym refs)])
             (cons name unique-refs))
           #f))]
    ;; (define-syntax name ...) - skip for now
    [(and (pair? sexp) (eq? (car sexp) 'define-syntax))
     #f]
    [else #f]))

;;; get-define-name : SExp -> Symbol | #f
;;; Extract the name from a define form
(define (get-define-name sexp)
  (if (pair? (cdr sexp))
      (let ([name-part (cadr sexp)])
        (cond
          [(symbol? name-part) name-part]
          [(and (pair? name-part) (symbol? (car name-part)))
           (car name-part)]
          [else #f]))
      #f))

;;; remove-duplicates-sym : (List Symbol) -> (List Symbol)
(define (remove-duplicates-sym lst)
  (let ([seen (make-eq-hashtable)])
    (filter (lambda (sym)
              (if (hashtable-ref seen sym #f)
                  #f
                  (begin (hashtable-set! seen sym #t) #t)))
            lst)))

(doc 'section 'cache-population)

;;; populate-xref-cache! : (List (Symbol . (List Symbol))) -> Void
;;; Populate the cross-reference indices from pre-parsed xref entries.
;;; Each entry is (definer-name . callees-list).
;;; Called from boundary orchestrator after I/O.
(define (populate-xref-cache! all-xrefs)
  ;; Reset indices
  (set! *xref-callers* (make-hashtable symbol-hash eq?))
  (set! *xref-callees* (make-hashtable symbol-hash eq?))
  (set! *xref-known-defs* (make-eq-hashtable))

  ;; First pass: collect all definitions
  (for-each
   (lambda (xref)
     (hashtable-set! *xref-known-defs* (car xref) #t))
   all-xrefs)

  ;; Second pass: build caller/callee indices
  (for-each
   (lambda (xref)
     (let* ([caller (car xref)]
            [all-refs (cdr xref)]
            ;; Filter to only known definitions (not primitives)
            [callees (filter (lambda (s)
                               (and (not (eq? s caller))  ; No self-refs
                                    (hashtable-ref *xref-known-defs* s #f)))
                             all-refs)])
       ;; Store callees for this function
       (hashtable-set! *xref-callees* caller callees)
       ;; Update callers for each callee
       (for-each
        (lambda (callee)
          (let ([existing (hashtable-ref *xref-callers* callee '())])
            (unless (memq caller existing)
              (hashtable-set! *xref-callers* callee (cons caller existing)))))
        callees)))
   all-xrefs))

(doc 'section 'query-api)

;;; xref-callers : Symbol -> (List Symbol)
;;; Get functions that call the given symbol
(define (xref-callers sym)
  (hashtable-ref *xref-callers* sym '()))

;;; xref-callees : Symbol -> (List Symbol)
;;; Get functions that the given symbol calls
(define (xref-callees sym)
  (hashtable-ref *xref-callees* sym '()))

;;; xref-callers-transitive : Symbol -> (List Symbol)
;;; Get all transitive callers (functions that directly or indirectly call sym)
(define (xref-callers-transitive sym)
  (let ([seen (make-eq-hashtable)]
        [result '()])
    (let loop ([to-visit (xref-callers sym)])
      (if (null? to-visit)
          result
          (let ([current (car to-visit)])
            (if (hashtable-ref seen current #f)
                (loop (cdr to-visit))
                (begin
                  (hashtable-set! seen current #t)
                  (set! result (cons current result))
                  (loop (append (cdr to-visit) (xref-callers current))))))))))

;;; xref-callees-transitive : Symbol -> (List Symbol)
;;; Get all transitive callees (functions that sym directly or indirectly calls)
(define (xref-callees-transitive sym)
  (let ([seen (make-eq-hashtable)]
        [result '()])
    (let loop ([to-visit (xref-callees sym)])
      (if (null? to-visit)
          result
          (let ([current (car to-visit)])
            (if (hashtable-ref seen current #f)
                (loop (cdr to-visit))
                (begin
                  (hashtable-set! seen current #t)
                  (set! result (cons current result))
                  (loop (append (cdr to-visit) (xref-callees current))))))))))

(doc 'section 'pretty-printing)

;;; print-xref-callers : Symbol -> Void
(define (print-xref-callers sym)
  (let ([callers (xref-callers sym)])
    (if (null? callers)
        (printf "~a: no callers found\n" sym)
        (begin
          (printf "~a is called by (~a functions):\n" sym (length callers))
          (for-each (lambda (c) (printf "  ~a\n" c))
                    (take-n-xref 15 callers))
          (when (> (length callers) 15)
            (printf "  ... and ~a more\n" (- (length callers) 15)))))))

;;; print-xref-callees : Symbol -> Void
(define (print-xref-callees sym)
  (let ([callees (xref-callees sym)])
    (if (null? callees)
        (printf "~a: no callees found\n" sym)
        (begin
          (printf "~a calls (~a functions):\n" sym (length callees))
          (for-each (lambda (c) (printf "  ~a\n" c))
                    (take-n-xref 15 callees))
          (when (> (length callees) 15)
            (printf "  ... and ~a more\n" (- (length callees) 15)))))))

(define (take-n-xref n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n-xref (- n 1) (cdr lst)))))

(doc 'section 'convenience-functions)

;;; lxu : Symbol -> Void
;;; "Lattice Xref Uses" - quick lookup of callers
(define (lxu sym)
  (print-xref-callers sym))

;;; lxc : Symbol -> Void
;;; "Lattice Xref Calls" - quick lookup of callees
(define (lxc sym)
  (print-xref-callees sym))

(doc 'section 'statistics)

;;; xref-stats : -> Void
(define (xref-stats)
  (let ([def-count (hashtable-size *xref-known-defs*)]
        [caller-count (hashtable-size *xref-callers*)]
        [callee-count (hashtable-size *xref-callees*)])
    (printf "Cross-reference statistics:\n")
    (printf "  Known definitions: ~a\n" def-count)
    (printf "  Functions with callers: ~a\n" caller-count)
    (printf "  Functions with callees: ~a\n" callee-count)))

;;; xref-most-called : Int -> (List (Symbol . Count))
;;; Get most-called functions
(define (xref-most-called n)
  (let ([counts '()])
    (vector-for-each
      (lambda (sym)
        (let ([callers (xref-callers sym)])
          (when (pair? callers)
            (set! counts (cons (cons sym (length callers)) counts)))))
      (hashtable-keys *xref-callers*))
    (take-n-xref n
      (sort (lambda (a b) (> (cdr a) (cdr b))) counts))))

(doc 'section 'repl-interface)

(meta-printf "xref.ss loaded.\n")
(meta-printf "  (lxu 'fn)                 - What calls this function?\n")
(meta-printf "  (lxc 'fn)                 - What does this function call?\n")
(meta-printf "  (xref-callers 'fn)        - Get caller list\n")
(meta-printf "  (xref-callees 'fn)        - Get callee list\n")
(meta-printf "  (xref-stats)              - Show statistics\n")
