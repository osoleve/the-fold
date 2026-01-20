(load "core/base/prelude.ss")

(doc 'module 'exports)
(doc 'description "Extract exported symbols from source files using (doc 'export #t) annotations")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Scans source files for definitions marked with (doc 'export #t)")

;;; ====
;;; Dependencies
;;; ====

;; Respect existing *meta-quiet* if set
(define *exports-quiet*
  (and (top-level-bound? '*meta-quiet*)
       (top-level-value '*meta-quiet*)))

;;; ====
;;; File Reading
;;; ====

;;; read-all-sexps : String -> (List SExp) | #f
;;; Read all S-expressions from a file
(define (read-all-sexps-ex path)
  (guard (e [else #f])
    (call-with-input-file path
      (lambda (port)
        (let loop ([acc '()])
          (let ([sexp (read port)])
            (if (eof-object? sexp)
                (reverse acc)
                (loop (cons sexp acc)))))))))

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

;;; extract-exports-from-sexp : SExp -> (List Symbol)
;;; Extract exported symbol names from a single S-expression
(define (extract-exports-from-sexp sexp)
  (cond
    [(not (pair? sexp)) '()]
    [(exported-define? sexp)
     (let ([name (define-name sexp)])
       (if name (list name) '()))]
    ;; Recurse into begin forms
    [(and (pair? sexp) (eq? (car sexp) 'begin))
     (apply append (map extract-exports-from-sexp (cdr sexp)))]
    [else '()]))

;;; extract-exports-from-file : String -> (List Symbol)
;;; Extract all exported symbols from a source file
(define (extract-exports-from-file path)
  (let ([sexps (read-all-sexps-ex path)])
    (if (not sexps)
        '()
        (apply append (map extract-exports-from-sexp sexps)))))

;;; ====
;;; Module Info Extraction
;;; ====

;;; extract-module-info : String -> (module description) | #f
;;; Extract module name and description from a source file
(define (extract-module-info path)
  (let ([sexps (read-all-sexps-ex path)])
    (if (not sexps)
        #f
        (let ([module-name #f]
              [description #f])
          (for-each
           (lambda (sexp)
             (when (and (pair? sexp)
                        (eq? (car sexp) 'doc)
                        (pair? (cdr sexp))
                        (pair? (cadr sexp))
                        (eq? (caadr sexp) 'quote))
               (let ([tag (cadadr sexp)])
                 (cond
                   [(and (eq? tag 'module) (pair? (cddr sexp)))
                    (set! module-name (caddr sexp))]
                   [(and (eq? tag 'description) (pair? (cddr sexp)))
                    (set! description (caddr sexp))]))))
           sexps)
          (if module-name
              (list module-name description)
              #f)))))

;;; ====
;;; Directory Scanning
;;; ====

;;; find-scheme-files : String -> (List String)
;;; Find all .ss files under a directory
(define (find-scheme-files-ex root)
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
                                                           (string-length entry)))
                                (not (string-prefix? "test-" entry)))  ; Skip test files
                           (list path)]
                          [(and (file-directory? path)
                                (not (char=? (string-ref entry 0) #\.)))
                           (walk path)]
                          [else '()])))
                    entries)))))
  (if (file-directory? root)
      (walk root)
      (if (file-exists? root) (list root) '())))

;;; string-prefix? : String × String -> Boolean
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

;;; ====
;;; Skill Export Scanning
;;; ====

;;; scan-skill-exports : String -> ((module . exports) ...)
;;; Scan a skill directory and collect exports by module
(define (scan-skill-exports skill-dir)
  (let ([files (find-scheme-files-ex skill-dir)]
        [results '()])
    (for-each
     (lambda (path)
       (let ([exports (extract-exports-from-file path)]
             [info (extract-module-info path)])
         (when (and info (pair? exports))
           (let ([module-name (car info)]
                 [desc (cadr info)])
             (set! results (cons (list module-name exports desc path) results))))))
     files)
    (reverse results)))

;;; ====
;;; Pretty Printing
;;; ====

;;; print-skill-exports : String -> Void
;;; Print exports for a skill directory
(define (print-skill-exports skill-dir)
  (let ([results (scan-skill-exports skill-dir)])
    (printf "Exports for ~a:~n" skill-dir)
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (entry)
       (let ([module (car entry)]
             [exports (cadr entry)]
             [desc (caddr entry)]
             [path (cadddr entry)])
         (printf "~n~a (~a):~n" module (basename path))
         (when desc
           (printf "  ~a~n" (truncate-string desc 70)))
         (printf "  Exports: ~a~n" (map symbol->string exports))))
     results)
    (printf "~n~a~n" (make-string 60 #\-))
    (printf "Total modules: ~a~n" (length results))
    (printf "Total exports: ~a~n"
            (apply + (map (lambda (e) (length (cadr e))) results)))))

;;; basename : String -> String
;;; Extract filename from path
(define (basename path)
  (let loop ([i (- (string-length path) 1)])
    (cond
      [(< i 0) path]
      [(char=? (string-ref path i) #\/)
       (substring path (+ i 1) (string-length path))]
      [else (loop (- i 1))])))

;;; truncate-string : String × Nat -> String
(define (truncate-string str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

;;; ====
;;; Manifest Generation
;;; ====

;;; generate-exports-sexp : String -> SExp
;;; Generate the exports section for a manifest
(define (generate-exports-sexp skill-dir)
  (let ([results (scan-skill-exports skill-dir)])
    (cons 'exports
          (apply append
                 (map (lambda (entry)
                        (let ([module (car entry)]
                              [exports (cadr entry)])
                          (cons (string->symbol (format ";; ~a" module))
                                exports)))
                      results)))))

;;; generate-modules-sexp : String -> SExp
;;; Generate the modules section for a manifest
(define (generate-modules-sexp skill-dir)
  (let ([results (scan-skill-exports skill-dir)])
    (cons 'modules
          (map (lambda (entry)
                 (let ([module (car entry)]
                       [desc (or (caddr entry) "")]
                       [path (cadddr entry)])
                   (list module (basename path) desc)))
               results))))

;;; ====
;;; Convenience Interface
;;; ====

;;; lef : String -> (List Symbol)
;;; List exports from a file (similar to le but for any file)
(define (lef path)
  (let ([exports (extract-exports-from-file path)])
    (if (null? exports)
        (printf "No exports found in ~a~n" path)
        (begin
          (printf "Exports from ~a:~n" path)
          (for-each (lambda (e) (printf "  ~a~n" e)) exports)))
    exports))

;;; le-skill : String -> Void
;;; List exports for a skill directory (alias for print-skill-exports)
(define le-skill print-skill-exports)
