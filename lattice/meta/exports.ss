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

;;; extract-exports-from-file : String -> (List Symbol)
;;; Extract all exported symbols from a source file
(define (extract-exports-from-file path)
  (let ([sexps (read-all-sexps-ex path)])
    (if (not sexps)
        '()
        (extract-exports-from-sexps sexps))))

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
        (append-map (lambda (entry)
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
                    entries))))
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
          (append-map (lambda (entry)
                        (let ([module (car entry)]
                              [exports (cadr entry)])
                          (cons (string->symbol (format ";; ~a" module))
                                exports)))
                      results))))

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

;;; extract-all-exports-from-file : String -> (List Symbol)
;;; Extract all exports (both contextual, following, and targeted) from a file
(define (extract-all-exports-from-file path)
  (let ([sexps (read-all-sexps-ex path)])
    (if (not sexps)
        '()
        (let ([from-defines (extract-exports-from-sexps sexps)]
              [targeted (find-targeted-exports sexps)])
          (delete-duplicates (append from-defines targeted))))))

;;; delete-duplicates : (List α) -> (List α)
(define (delete-duplicates lst)
  (let loop ([lst lst] [seen '()] [acc '()])
    (cond
      [(null? lst) (reverse acc)]
      [(member (car lst) seen) (loop (cdr lst) seen acc)]
      [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

;;; ====
;;; Full Manifest Generation
;;; ====

;;; scan-skill-all-exports : String -> ((module exports desc path) ...)
;;; Scan skill directory, finding both contextual and targeted exports
(define (scan-skill-all-exports skill-dir)
  (let ([files (find-scheme-files-ex skill-dir)]
        [results '()])
    (for-each
     (lambda (path)
       (let ([exports (extract-all-exports-from-file path)]
             [info (extract-module-info path)])
         (when (pair? exports)
           (let ([module-name (if info (car info) (path->module path))]
                 [desc (if info (cadr info) "")])
             (set! results (cons (list module-name exports desc path) results))))))
     files)
    (reverse results)))

;;; path->module : String -> Symbol
;;; Extract module name from path (fallback when no doc 'module)
(define (path->module path)
  (let* ([base (basename path)]
         [name (if (and (> (string-length base) 3)
                        (string=? ".ss" (substring base (- (string-length base) 3) (string-length base))))
                   (substring base 0 (- (string-length base) 3))
                   base)])
    (string->symbol name)))

;;; generate-manifest-exports : String -> SExp
;;; Generate (exports ...) section grouped by module
(define (generate-manifest-exports skill-dir)
  (let ([results (scan-skill-all-exports skill-dir)])
    (cons 'exports
          (map (lambda (entry)
                 (cons (car entry)    ; module name
                       (cadr entry))) ; exports list
               results))))

;;; generate-manifest-modules : String -> SExp
;;; Generate (modules ...) section
(define (generate-manifest-modules skill-dir)
  (let ([results (scan-skill-all-exports skill-dir)])
    (cons 'modules
          (map (lambda (entry)
                 (let ([module (car entry)]
                       [desc (or (caddr entry) "")]
                       [path (cadddr entry)])
                   (list module (basename path) desc)))
               results))))

;;; ====
;;; Manifest Update (preserves metadata)
;;; ====

;;; read-manifest-sexp : String -> SExp | #f
(define (read-manifest-sexp path)
  (guard (e [else #f])
    (call-with-input-file path read)))

;;; update-manifest-entry : SExp × Symbol × SExp -> SExp
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

;;; cons* : α × β × ... × (List γ) -> (List α β ... γ)
(define (cons* . args)
  (if (null? (cdr args))
      (car args)
      (cons (car args) (apply cons* (cdr args)))))

;;; update-manifest-from-source : String -> SExp | #f
;;; Read existing manifest and update exports/modules from source
(define (update-manifest-from-source skill-dir)
  (let ([manifest-path (string-append skill-dir "/manifest.sexp")]
        [new-exports (generate-manifest-exports skill-dir)]
        [new-modules (generate-manifest-modules skill-dir)])
    (let ([manifest (read-manifest-sexp manifest-path)])
      (if (not manifest)
          #f
          (let* ([m1 (update-manifest-entry manifest 'exports new-exports)]
                 [m2 (update-manifest-entry m1 'modules new-modules)])
            m2)))))

;;; ====
;;; Manifest Comparison
;;; ====

;;; compare-manifest-exports : String -> Void
;;; Compare manifest-declared exports vs source-defined exports
(define (compare-manifest-exports skill-dir)
  (let* ([manifest-path (string-append skill-dir "/manifest.sexp")]
         [manifest (read-manifest-sexp manifest-path)])
    (if (not manifest)
        (printf "No manifest found at ~a~n" manifest-path)
        (let* ([source-exports (scan-skill-all-exports skill-dir)]
               [source-symbols (delete-duplicates (append-map cadr source-exports))]
               [manifest-exports (manifest-exports-list manifest)]
               [in-source-not-manifest (filter (lambda (s) (not (member s manifest-exports))) source-symbols)]
               [in-manifest-not-source (filter (lambda (s) (not (member s source-symbols))) manifest-exports)])
          (printf "Export comparison for ~a:~n" skill-dir)
          (printf "  Source exports:   ~a~n" (length source-symbols))
          (printf "  Manifest exports: ~a~n" (length manifest-exports))
          (unless (null? in-source-not-manifest)
            (printf "~n  In source but not manifest (~a):~n" (length in-source-not-manifest))
            (for-each (lambda (s) (printf "    + ~a~n" s)) in-source-not-manifest))
          (unless (null? in-manifest-not-source)
            (printf "~n  In manifest but not source (~a):~n" (length in-manifest-not-source))
            (for-each (lambda (s) (printf "    - ~a~n" s)) in-manifest-not-source))
          (when (and (null? in-source-not-manifest) (null? in-manifest-not-source))
            (printf "~n  ✓ Exports are in sync~n"))))))

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
;;; Pretty Print Manifest
;;; ====

;;; pretty-print-manifest : SExp × Port -> Void
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

;;; write-manifest : String × SExp -> Void
;;; Write manifest to file with pretty formatting
(define (write-manifest path manifest)
  (call-with-output-file path
    (lambda (port)
      (pretty-print-manifest manifest port))
    'replace))

;;; sync-manifest : String -> Void
;;; Update manifest file from source exports
(define (sync-manifest skill-dir)
  (let ([manifest (update-manifest-from-source skill-dir)])
    (if (not manifest)
        (printf "Could not read manifest for ~a~n" skill-dir)
        (let ([path (string-append skill-dir "/manifest.sexp")])
          (write-manifest path manifest)
          (printf "Updated ~a~n" path)
          (compare-manifest-exports skill-dir)))))
