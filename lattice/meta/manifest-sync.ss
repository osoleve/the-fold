(load "core/base/prelude.ss")

(doc 'module 'manifest-sync)
(doc 'description "Compare manifest-declared exports with source file definitions")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Helps synchronize manifest.sexp with actual exports in source files")

;;; ====
;;; Manifest Parsing
;;; ====

;;; read-manifest : String -> SExp | #f
(define (read-manifest path)
  (doc 'type '(-> String (Maybe SExp)))
  (guard (e [else #f])
    (call-with-input-file path
      (lambda (port)
        ;; Skip comment lines
        (let loop ()
          (let ([c (peek-char port)])
            (cond
              [(eof-object? c) #f]
              [(char=? c #\;) (get-line port) (loop)]
              [(char-whitespace? c) (get-char port) (loop)]
              [else (read port)])))))))

;;; flatten-exports : (List (Symbol | List)) -> (List Symbol)
;;; Handle both flat list of symbols and grouped lists
(define (flatten-exports items)
  (apply append
         (map (lambda (item)
                (cond
                  [(symbol? item) (list item)]
                  [(pair? item) (filter symbol? item)]
                  [else '()]))
              items)))

;;; manifest-exports : SExp -> (List Symbol)
;;; Extract exports from manifest sexp (handles both formats and grouped exports)
(define (manifest-exports manifest)
  (doc 'type '(-> SExp (List Symbol)))
  (cond
    ;; Format 1: (skill name (exports ...) ...)
    [(and (pair? manifest) (eq? (car manifest) 'skill))
     (let ([exports-entry (find-entry 'exports (cddr manifest))])
       (if exports-entry
           (flatten-exports (cdr exports-entry))
           '()))]
    ;; Format 2: ((skill . name) (exports . (...)) ...)
    [(and (pair? manifest) (pair? (car manifest)) (eq? (caar manifest) 'skill))
     (let ([exports-entry (assq 'exports manifest)])
       (if exports-entry
           (flatten-exports (cdr exports-entry))
           '()))]
    [else '()]))

;;; normalize-module-entry : SExp -> (name file desc) | #f
;;; Normalize various module entry formats to (symbol string string)
(define (normalize-module-entry entry)
  (cond
    [(not (pair? entry)) #f]
    ;; Format: (symbol "file.ss" "desc") - standard
    [(and (symbol? (car entry))
          (>= (length entry) 2)
          (string? (cadr entry)))
     (list (car entry)
           (cadr entry)
           (if (>= (length entry) 3) (caddr entry) ""))]
    ;; Format: ("name" "file.ss" "desc") - string module name
    [(and (string? (car entry))
          (>= (length entry) 2)
          (string? (cadr entry)))
     (list (string->symbol (car entry))
           (cadr entry)
           (if (>= (length entry) 3) (caddr entry) ""))]
    [else #f]))

;;; manifest-modules : SExp -> (List (name file desc))
(define (manifest-modules manifest)
  (doc 'type '(-> SExp (List (Triple Symbol String String))))
  (let ([raw-modules
         (cond
           ;; Format 1: (skill name (modules ...) ...)
           [(and (pair? manifest) (eq? (car manifest) 'skill))
            (let ([modules-entry (find-entry 'modules (cddr manifest))])
              (if modules-entry (cdr modules-entry) '()))]
           ;; Format 2: ((skill . name) (modules . (...)) ...)
           [(and (pair? manifest) (pair? (car manifest)) (eq? (caar manifest) 'skill))
            (let ([modules-entry (assq 'modules manifest)])
              (if modules-entry (cdr modules-entry) '()))]
           [else '()])])
    ;; Handle extra nesting: (modules ((...))) -> flatten the inner list
    (let ([flattened
           (if (and (= (length raw-modules) 1)
                    (pair? (car raw-modules))
                    (pair? (caar raw-modules)))
               ;; Extra level of nesting - unwrap it
               (car raw-modules)
               raw-modules)])
      ;; Normalize all entries
      (filter identity (map normalize-module-entry flattened)))))

;;; find-entry : Symbol × (List SExp) -> SExp | #f
(define (find-entry key lst)
  (cond
    [(null? lst) #f]
    [(and (pair? (car lst)) (eq? (caar lst) key)) (car lst)]
    [else (find-entry key (cdr lst))]))

;;; ====
;;; Source File Analysis
;;; ====

;;; read-all-sexps : String -> (List SExp) | #f
(define (read-all-sexps path)
  (guard (e [else #f])
    (call-with-input-file path
      (lambda (port)
        (let loop ([acc '()])
          (let ([sexp (read port)])
            (if (eof-object? sexp)
                (reverse acc)
                (loop (cons sexp acc)))))))))

;;; extract-definitions : String -> (List Symbol)
;;; Extract all defined symbols from a source file
(define (extract-definitions path)
  (doc 'type '(-> String (List Symbol)))
  (let ([sexps (read-all-sexps path)])
    (if (not sexps)
        '()
        (apply append (map extract-defs-from-sexp sexps)))))

;;; extract-defs-from-sexp : SExp -> (List Symbol)
(define (extract-defs-from-sexp sexp)
  (cond
    [(not (pair? sexp)) '()]
    [(eq? (car sexp) 'define)
     (let ([second (if (pair? (cdr sexp)) (cadr sexp) #f)])
       (cond
         ;; (define (name args...) body)
         [(pair? second) (list (car second))]
         ;; (define name value)
         [(symbol? second) (list second)]
         [else '()]))]
    [(eq? (car sexp) 'define-syntax)
     (let ([second (if (pair? (cdr sexp)) (cadr sexp) #f)])
       (if (symbol? second) (list second) '()))]
    [(eq? (car sexp) 'begin)
     (apply append (map extract-defs-from-sexp (cdr sexp)))]
    [else '()]))

;;; ====
;;; Comparison
;;; ====

;;; compare-exports : String -> (missing-in-source missing-in-manifest)
;;; Compare manifest exports with source definitions
(define (compare-exports skill-dir)
  (doc 'type '(-> String (Pair (List Symbol) (List Symbol))))
  (let* ([manifest-path (string-append skill-dir "/manifest.sexp")]
         [manifest (read-manifest manifest-path)])
    (if (not manifest)
        (cons '() '())
        (let* ([declared (manifest-exports manifest)]
               [modules (manifest-modules manifest)]
               ;; Collect all definitions from source files
               [defined (apply append
                              (map (lambda (mod)
                                     (let ([file (string-append skill-dir "/" (cadr mod))])
                                       (if (file-exists? file)
                                           (extract-definitions file)
                                           '())))
                                   modules))]
               ;; Find mismatches
               [missing-source (filter (lambda (s) (not (member s defined))) declared)]
               [missing-manifest (filter (lambda (s) (not (member s declared))) defined)])
          (cons missing-source missing-manifest)))))

;;; ====
;;; Reporting
;;; ====

;;; analyze-skill : String -> Void
;;; Print analysis of a skill's export status
(define (analyze-skill skill-dir)
  (doc 'type '(-> String Void))
  (let* ([manifest-path (string-append skill-dir "/manifest.sexp")]
         [manifest (read-manifest manifest-path)])
    (if (not manifest)
        (printf "No manifest found: ~a~n" manifest-path)
        (let* ([declared (manifest-exports manifest)]
               [modules (manifest-modules manifest)]
               [comparison (compare-exports skill-dir)])
          (printf "~n=== ~a ===~n" skill-dir)
          (printf "Manifest declares ~a exports~n" (length declared))
          (printf "Modules: ~a~n" (length modules))
          (let ([missing-source (car comparison)]
                [missing-manifest (cdr comparison)])
            (when (pair? missing-source)
              (printf "~nDeclared but not found in source (~a):~n" (length missing-source))
              (for-each (lambda (s) (printf "  - ~a~n" s)) (take missing-source 10))
              (when (> (length missing-source) 10)
                (printf "  ... and ~a more~n" (- (length missing-source) 10))))
            (when (pair? missing-manifest)
              (printf "~nDefined but not exported (~a):~n" (length missing-manifest))
              (for-each (lambda (s) (printf "  - ~a~n" s)) (take missing-manifest 10))
              (when (> (length missing-manifest) 10)
                (printf "  ... and ~a more~n" (- (length missing-manifest) 10)))))))))

;;; take : (List a) × Nat -> (List a)
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;;; ====
;;; Export Annotation Generator
;;; ====

;;; generate-export-edits : String -> (List (file symbol))
;;; Generate list of (file . symbol) pairs needing export annotations
(define (generate-export-edits skill-dir)
  (doc 'type '(-> String (List (Pair String Symbol))))
  (let* ([manifest-path (string-append skill-dir "/manifest.sexp")]
         [manifest (read-manifest manifest-path)])
    (if (not manifest)
        '()
        (let* ([declared (manifest-exports manifest)]
               [modules (manifest-modules manifest)])
          (apply append
                 (map (lambda (mod)
                        (let* ([file (string-append skill-dir "/" (cadr mod))]
                               [defined (if (file-exists? file)
                                           (extract-definitions file)
                                           '())]
                               [to-annotate (filter (lambda (s) (member s declared)) defined)])
                          (map (lambda (s) (cons file s)) to-annotate)))
                      modules))))))

;;; print-export-edits : String -> Void
;;; Print the export annotations needed for a skill
(define (print-export-edits skill-dir)
  (let ([edits (generate-export-edits skill-dir)])
    (printf "Export annotations needed for ~a:~n" skill-dir)
    (for-each
     (lambda (edit)
       (printf "  ~a: ~a~n" (car edit) (cdr edit)))
     edits)
    (printf "Total: ~a annotations~n" (length edits))))

;;; ====
;;; Batch Analysis
;;; ====

;;; analyze-all-skills : -> Void
(define (analyze-all-skills)
  (let ([dirs (directory-list "lattice")])
    (for-each
     (lambda (dir)
       (let ([path (string-append "lattice/" dir "/manifest.sexp")])
         (when (file-exists? path)
           (analyze-skill (string-append "lattice/" dir)))))
     dirs)))
