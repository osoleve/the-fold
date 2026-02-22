(unless (top-level-bound? 'filter-map) (load "core/base/prelude.ss"))

(doc 'module 'exports)
(doc 'description "Extract exported symbols from S-expressions using (doc 'export #t) annotations")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Pure pattern recognition on S-expressions. I/O orchestration in boundary/meta/exports-io.ss")

;;; ====
;;; Dependencies
;;; ====

;; Respect existing *meta-quiet* if set
(define *exports-quiet*
  (and (top-level-bound? '*meta-quiet*)
       (top-level-value '*meta-quiet*)))

;;; ====
;;; Pattern Recognition
;;; ====

;;; define-form? : SExp -> Boolean
;;; Check if expression is a define form
(define (define-form? sexp)
  (and (pair? sexp)
       (eq? (car sexp) 'define)
       (pair? (cdr sexp))))

;;; define-name : SExp -> Symbol | #f
;;; Extract the name from a define form
;;; Handles both (define (name args...) body) and (define name value)
(define (define-name sexp)
  (if (not (define-form? sexp))
      #f
      (let ([second (cadr sexp)])
        (cond
          ;; (define (name args...) body...)
          [(pair? second) (car second)]
          ;; (define name value)
          [(symbol? second) second]
          [else #f]))))

;;; define-body : SExp -> (List SExp)
;;; Get the body expressions of a define form
(define (define-body sexp)
  (if (not (define-form? sexp))
      '()
      (let ([second (cadr sexp)])
        (if (pair? second)
            ;; (define (name args...) body...)
            (cddr sexp)
            ;; (define name (lambda ...)) - check if it's a lambda
            (let ([value (if (pair? (cddr sexp)) (caddr sexp) #f)])
              (if (and (pair? value)
                       (eq? (car value) 'lambda)
                       (pair? (cddr value)))
                  ;; Extract lambda body
                  (cddr value)
                  ;; Not a function, return value
                  (if value (list value) '())))))))

;;; has-export-doc? : SExp -> Boolean
;;; Check if body contains (doc 'export #t)
(define (has-export-doc? body)
  (and (pair? body)
       (exists (lambda (expr)
                 (and (pair? expr)
                      (eq? (car expr) 'doc)
                      (pair? (cdr expr))
                      (pair? (cadr expr))
                      (eq? (caadr expr) 'quote)
                      (eq? (cadadr expr) 'export)
                      (pair? (cddr expr))
                      (eq? (caddr expr) #t)))
               body)))

;;; exported-define? : SExp -> Boolean
;;; Check if a define form is marked for export
(define (exported-define? sexp)
  (and (define-form? sexp)
       (has-export-doc? (define-body sexp))))

;;; ====
;;; Extraction
;;; ====

;;; standalone-export-doc? : SExp -> Boolean
;;; Check if sexp is a standalone (doc 'export #t) form
(define (standalone-export-doc? sexp)
  (and (pair? sexp)
       (eq? (car sexp) 'doc)
       (pair? (cdr sexp))
       (pair? (cadr sexp))
       (eq? (caadr sexp) 'quote)
       (eq? (cadadr sexp) 'export)
       (or (= (length sexp) 2)
           (and (= (length sexp) 3)
                (eq? (caddr sexp) #t)))))

;;; extract-exports-from-sexps : (List SExp) -> (List Symbol)
;;; Extract exported symbols, handling both inline and following doc forms
;;; Patterns handled:
;;; 1. (define (name ...) (doc 'export #t) ...)  - inline
;;; 2. (define (name ...) body) \n (doc 'export #t)  - following
;;; 3. (doc name 'export #t) \n (define name ...)  - targeted
(define (extract-exports-from-sexps sexps)
  (let loop ([sexps sexps] [acc '()])
    (cond
      [(null? sexps) (reverse acc)]
      ;; Check if this define is followed by a standalone export doc
      [(and (define-form? (car sexps))
            (pair? (cdr sexps))
            (standalone-export-doc? (cadr sexps)))
       (let ([name (define-name (car sexps))])
         (loop (cddr sexps)
               (if name (cons name acc) acc)))]
      ;; Check if this define has inline export doc
      [(exported-define? (car sexps))
       (let ([name (define-name (car sexps))])
         (loop (cdr sexps)
               (if name (cons name acc) acc)))]
      ;; Skip standalone export docs (already handled above or targeted)
      [(standalone-export-doc? (car sexps))
       (loop (cdr sexps) acc)]
      ;; Recurse into begin forms
      [(and (pair? (car sexps)) (eq? (caar sexps) 'begin))
       (loop (cdr sexps)
             (append (reverse (extract-exports-from-sexps (cdar sexps))) acc))]
      ;; Skip other forms
      [else
       (loop (cdr sexps) acc)])))

;;; ====
;;; Targeted Export Doc Detection
;;; ====

;;; find-targeted-exports : (List SExp) -> (List Symbol)
;;; Find symbols exported via targeted doc forms: (doc symbol 'export #t)
(define (find-targeted-exports sexps)
  (let ([exports '()])
    (for-each
     (lambda (sexp)
       (when (and (pair? sexp)
                  (eq? (car sexp) 'doc)
                  (>= (length sexp) 3)
                  (symbol? (cadr sexp))
                  (pair? (caddr sexp))
                  (eq? (caaddr sexp) 'quote)
                  (eq? (car (cdaddr sexp)) 'export)
                  (or (= (length sexp) 3)
                      (and (= (length sexp) 4)
                           (eq? (cadddr sexp) #t))))
         (set! exports (cons (cadr sexp) exports))))
     sexps)
    (reverse exports)))

;;; ====
;;; Helpers
;;; ====

;;; delete-duplicates : (List a) -> (List a)
(define (delete-duplicates lst)
  (let loop ([lst lst] [seen '()] [acc '()])
    (cond
      [(null? lst) (reverse acc)]
      [(member (car lst) seen) (loop (cdr lst) seen acc)]
      [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

;;; basename : String -> String
;;; Extract filename from path
(define (basename path)
  (let loop ([i (- (string-length path) 1)])
    (cond
      [(< i 0) path]
      [(char=? (string-ref path i) #\/)
       (substring path (+ i 1) (string-length path))]
      [else (loop (- i 1))])))

;;; truncate-string : String x Nat -> String
(define (truncate-string str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

;;; path->module : String -> Symbol
;;; Extract module name from path (fallback when no doc 'module)
(define (path->module path)
  (let* ([base (basename path)]
         [name (if (and (> (string-length base) 3)
                        (string=? ".ss" (substring base (- (string-length base) 3) (string-length base))))
                   (substring base 0 (- (string-length base) 3))
                   base)])
    (string->symbol name)))

;;; ====
;;; Manifest Structure Manipulation (pure — operates on S-expression data)
;;; ====

;;; update-manifest-entry : SExp x Symbol x SExp -> SExp
;;; Replace or add an entry in a manifest
(define (update-manifest-entry manifest key new-value)
  (let ([found #f])
    (let ([updated
           (map (lambda (item)
                  (if (and (pair? item) (eq? (car item) key))
                      (begin (set! found #t) new-value)
                      item))
                (cddr manifest))])
      (if found
          (cons* (car manifest) (cadr manifest) updated)
          (cons* (car manifest) (cadr manifest) (append updated (list new-value)))))))

;;; cons* : a x b x ... x (List c) -> (List a b ... c)
(define (cons* . args)
  (if (null? (cdr args))
      (car args)
      (cons (car args) (apply cons* (cdr args)))))

;;; manifest-exports-list : SExp -> (List Symbol)
;;; Extract flat list of exports from manifest
;;; Handles grouped exports: (exports (group sym1 sym2...) (ring sym3 sym4...))
;;; The first symbol in each group is the module label, not an export
(define (manifest-exports-list manifest)
  (let ([exports-entry (assq 'exports (cddr manifest))])
    (if (not exports-entry)
        '()
        (append-map (lambda (item)
                      (cond
                        ;; Top-level symbol (flat exports list)
                        [(symbol? item) (list item)]
                        ;; Grouped: (module-label sym1 sym2 ...) - skip first element
                        [(and (pair? item) (symbol? (car item)))
                         (filter symbol? (cdr item))]
                        [else '()]))
                    (cdr exports-entry)))))

;;; ====
;;; Pretty Printing (formatter — writes to caller-supplied port)
;;; ====

;;; pretty-print-manifest : SExp x Port -> Void
(define (pretty-print-manifest manifest port)
  (define (indent n) (make-string (* n 2) #\space))

  (define (pp-value val depth)
    (cond
      [(string? val)
       (fprintf port "~s" val)]
      [(symbol? val)
       (fprintf port "~a" val)]
      [(number? val)
       (fprintf port "~a" val)]
      [(null? val)
       (fprintf port "()")]
      [(pair? val)
       (fprintf port "(")
       (let loop ([items val] [first #t])
         (when (pair? items)
           (unless first (fprintf port " "))
           (pp-value (car items) (+ depth 1))
           (loop (cdr items) #f)))
       (fprintf port ")")]))

  (define (pp-entry entry depth)
    (let ([key (car entry)]
          [val (cdr entry)])
      (fprintf port "~a(~a" (indent depth) key)
      (cond
        ;; Multi-line entries
        [(memq key '(description))
         (fprintf port "~n~a ~s)" (indent (+ depth 1)) (car val))]
        ;; List entries (one per line for exports)
        [(eq? key 'exports)
         (fprintf port "~n")
         (for-each
          (lambda (group)
            (if (pair? group)
                (begin
                  (fprintf port "~a(~a" (indent (+ depth 1)) (car group))
                  (for-each (lambda (s) (fprintf port " ~a" s)) (cdr group))
                  (fprintf port ")~n"))
                (fprintf port "~a~a~n" (indent (+ depth 1)) group)))
          val)
         (fprintf port "~a)" (indent depth))]
        [(eq? key 'modules)
         (fprintf port "~n")
         (for-each
          (lambda (mod)
            (fprintf port "~a(~a ~s ~s)~n"
                     (indent (+ depth 1))
                     (car mod) (cadr mod) (caddr mod)))
          val)
         (fprintf port "~a)" (indent depth))]
        ;; Simple lists
        [(list? val)
         (fprintf port " ")
         (pp-value val depth)
         (fprintf port ")")]
        ;; Single value
        [else
         (fprintf port " ")
         (pp-value (car val) depth)
         (fprintf port ")")])))

  ;; Main body
  (fprintf port ";;; ~a/manifest.sexp~n~n"
           (let ([path-entry (assq 'path (cddr manifest))])
             (if path-entry (cadr path-entry) "skill")))
  (fprintf port "(skill ~a~n" (cadr manifest))
  (for-each
   (lambda (entry)
     (when (pair? entry)
       (pp-entry entry 1)
       (fprintf port "~n~n")))
   (cddr manifest))
  (fprintf port ")~n"))

;;; ====
;;; REPL Interface
;;; ====

(unless *exports-quiet*
  (meta-printf "exports.ss loaded (pure pattern recognition).\n"))
