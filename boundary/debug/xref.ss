(load "boundary/tools/string-utils.ss")

(source-directories (cons "shell" (source-directories)))

(load "boundary/io/fs.ss")
(load "boundary/tools/edit.ss")

(doc 'module 'xref)
(doc 'description "Find all references to definitions across the codebase")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'features
     "Find all uses of a symbol"
     "Find all definitions of a symbol"
     "Show context around each reference"
     "Support for filtering by directory"
     "Generate cross-reference reports")
(doc 'usage "(xref-find-uses fs \"symbol-name\" [\"directory\"])")
(doc 'usage "(xref-find-defs fs \"symbol-name\" [\"directory\"])")
(doc 'usage "(xref-report fs \"symbol-name\" \"output-file\" [\"directory\"])")
(doc 'usage "(xref-index fs \"directory\")")
(doc 'note "string-trim and string-split-lines provided by boundary/tools/string-utils.ss")

(doc 'section 'sexpr-walking)

(define (find-symbol-in-sexpr sym sexpr context)
  (doc 'type (-> Symbol Sexpr Nat (List Nat)))
  (doc 'description "Find all positions where symbol appears in s-expression")
  (doc 'note "Position is the nesting depth/index - used for reporting")
  (doc 'returns "List of line numbers where symbol appears (approximate)")
  (let walk ([expr sexpr]
             [results '()]
             [depth 0])
       (cond
        [(symbol? expr)
         (if (eq? expr sym)
             (cons context results)
             results)]
        [(pair? expr)
         (let ([results (walk (car expr) results (+ depth 1))])
              (walk (cdr expr) results (+ depth 1)))]
        [else results])))

(define (is-definition? expr)
  (doc 'type (-> Sexpr Boolean))
  (doc 'description "Check if s-expression is a definition form")
  (and (pair? expr)
       (symbol? (car expr))
       (memq (car expr) '(define define-syntax let let* letrec letrec*
                          lambda λ case-lambda
                          define-record-type))))

(define (extract-defined-name expr)
  (doc 'type (-> Sexpr (Maybe Symbol)))
  (doc 'description "Extract the name being defined from a definition form")
  (cond
   [(not (pair? expr)) #f]
   [(not (pair? (cdr expr))) #f]
   [else
    (let ([form (car expr)]
          [rest (cdr expr)])
         (case form
               [(define define-syntax)
                (let ([name-part (car rest)])
                     (cond
                      [(symbol? name-part) name-part]
                      [(pair? name-part) (car name-part)]  ; (define (name ...) ...)
                      [else #f]))]
               [(let let* letrec letrec*)
                ;; Named let: (let name ...)
                (if (and (not (null? rest))
                         (symbol? (car rest)))
                    (car rest)
                    #f)]
               [(lambda λ case-lambda)
                #f]  ; lambdas don't define names directly
               [(define-record-type)
                (if (and (not (null? rest))
                         (symbol? (car rest)))
                    (car rest)
                    #f)]
               [else #f]))]))

(doc 'section 'file-analysis)

(doc 'type-alias 'Reference '(Tuple String Nat String))
(doc 'note "Reference = (file-path line-number context-line) where context-line is the actual line of code")

(define (search-file-for-symbol fs file-path sym)
  (doc 'type (-> FS String Symbol (List Reference)))
  (doc 'description "Search a file for all occurrences of a symbol")
  (doc 'returns "List of references with file path, line number, and context")
  (guard (e [else '()])
         (let* ([content (read-text-file fs file-path)]
                [lines (string-split-lines content)]
                [port (open-input-string content)])
               ;; Read all s-expressions and track line numbers
               (let loop ([line-no 1]
                          [results '()])
                    (let ([expr (guard (e [else #f])
                                       (read port))])
                         (cond
                          [(eof-object? expr)
                           (reverse results)]
                          [(not expr)  ; parse error
                           (reverse results)]
                          [else
                           ;; Check if symbol appears in this expression
                           (let ([found (find-symbol-in-expr sym expr)])
                                (if found
                                    (let* ([context-line (get-context-line lines line-no)]
                                           [ref (list file-path line-no context-line)])
                                          (loop (+ line-no 1) (cons ref results)))
                                    (loop (+ line-no 1) results)))]))))))

(define (find-symbol-in-expr sym expr)
  (doc 'type (-> Symbol Sexpr Boolean))
  (doc 'description "Check if symbol appears anywhere in the expression")
  (cond
   [(symbol? expr) (eq? expr sym)]
   [(pair? expr)
    (or (find-symbol-in-expr sym (car expr))
        (find-symbol-in-expr sym (cdr expr)))]
   [else #f]))

(define (get-context-line lines line-no)
  (doc 'type (-> (List String) Nat String))
  (doc 'description "Get the line at the given line number (1-indexed)")
  (if (and (> line-no 0) (<= line-no (length lines)))
      (list-ref lines (- line-no 1))
      ""))

(doc 'section 'definition-finding)

(define (find-definitions-in-file fs file-path sym)
  (doc 'type (-> FS String Symbol (List Reference)))
  (doc 'description "Find all definitions of a symbol in a file")
  (guard (e [else '()])
         (let* ([content (read-text-file fs file-path)]
                [lines (string-split-lines content)]
                [port (open-input-string content)])
               (let loop ([line-no 1]
                          [results '()])
                    (let ([expr (guard (e [else #f])
                                       (read port))])
                         (cond
                          [(eof-object? expr)
                           (reverse results)]
                          [(not expr)
                           (reverse results)]
                          [else
                           ;; Check if this is a definition of sym
                           (let ([defined-name (extract-defined-name expr)])
                                (if (and defined-name (eq? defined-name sym))
                                    (let* ([context-line (get-context-line lines line-no)]
                                           [ref (list file-path line-no context-line)])
                                          (loop (+ line-no 1) (cons ref results)))
                                    (loop (+ line-no 1) results)))]))))))

(doc 'section 'directory-traversal)

(doc find-all-scheme-files 'type (-> FS String (List String)))
(doc find-all-scheme-files 'description "Recursively find all .ss files in a directory")
(define (find-all-scheme-files fs dir-path)
  (define (scheme-file? path)
    (and (>= (string-length path) 3)
         (string=? (substring path (- (string-length path) 3) (string-length path)) ".ss")))
  
  (define (skip-dir? name)
    (or (string=? name ".")
        (string=? name "..")
        (string=? name ".git")
        (string=? name ".fold-repl")
        (string=? name ".fold-sessions")
        (string=? name "node_modules")))
  
  (let loop ([to-process (list dir-path)]
             [result '()])
       (if (null? to-process)
           result
           (let ([current (car to-process)]
                 [rest (cdr to-process)])
                (cond
                 [(not (file-exists? current))
                  (loop rest result)]
                 [(and (file-exists? current)
                       (guard (e [else #f])
                              (let ([entries (directory-list current)])
                                   (not (null? entries))))
                       (not (scheme-file? current)))
                  ;; It's a directory - add its non-skipped contents
                  (let ([entries (guard (e [else '()])
                                        (directory-list current))])
                       (let ([full-paths (filter
                                          (lambda (name) (not (skip-dir? name)))
                                          (map (lambda (e)
                                                       (string-append current "/" e))
                                               entries))])
                            (loop (append full-paths rest) result)))]
                 [(scheme-file? current)
                  (loop rest (cons current result))]
                 [else
                  (loop rest result)])))))

(doc 'section 'public-api)

(define (xref-find-uses fs sym . opts)
  (doc 'type (-> FS String [String] Void))
  (doc 'description "Find all uses of a symbol across the codebase")
  (let* ([symbol (if (string? sym) (string->symbol sym) sym)]
         [dir (if (null? opts) "." (car opts))]
         [files (find-all-scheme-files fs dir)]
         [all-refs '()])

        ;; Collect all references
        ;; BUGFIX: Use cons instead of append to avoid O(N^2)
        (for-each
         (lambda (file)
                 (let ([refs (search-file-for-symbol fs file symbol)])
                      (set! all-refs (append refs all-refs))))
         files)
        ;; Reverse to restore original order
        (set! all-refs (reverse all-refs))
        
        ;; Display results
        (display (format "\nCross-reference for '~a' in ~a:\n" symbol dir))
        (display (format "Found ~a references in ~a files.\n\n"
                         (length all-refs)
                         (length (remove-duplicates (map car all-refs)))))
        
        (if (null? all-refs)
            (display "  (no references found)\n")
            (for-each
             (lambda (ref)
                     (let ([file (car ref)]
                           [line-no (cadr ref)]
                           [context (caddr ref)])
                          (display (format "  ~a:~a\n" file line-no))
                          (display (format "    ~a\n" (string-trim context)))))
             all-refs))
        (display "\n")))

(define (xref-find-defs fs sym . opts)
  (doc 'type (-> FS String [String] Void))
  (doc 'description "Find all definitions of a symbol")
  (let* ([symbol (if (string? sym) (string->symbol sym) sym)]
         [dir (if (null? opts) "." (car opts))]
         [files (find-all-scheme-files fs dir)]
         [all-defs '()])
        
        ;; Collect all definitions
        (for-each
         (lambda (file)
                 (let ([defs (find-definitions-in-file fs file symbol)])
                      (set! all-defs (append all-defs defs))))
         files)
        
        ;; Display results
        (display (format "\nDefinitions of '~a' in ~a:\n" symbol dir))
        (if (null? all-defs)
            (display "  (no definitions found)\n")
            (for-each
             (lambda (def)
                     (let ([file (car def)]
                           [line-no (cadr def)]
                           [context (caddr def)])
                          (display (format "  ~a:~a\n" file line-no))
                          (display (format "    ~a\n" (string-trim context)))))
             all-defs))
        (display "\n")))

(define (xref-report fs sym output-file . opts)
  (doc 'type (-> FS String String [String] Void))
  (doc 'description "Generate a comprehensive cross-reference report")
  (let* ([symbol (if (string? sym) (string->symbol sym) sym)]
         [dir (if (null? opts) "." (car opts))]
         [files (find-all-scheme-files fs dir)]
         [all-defs '()]
         [all-refs '()])
        
        ;; Collect definitions and references
        (for-each
         (lambda (file)
                 (let ([defs (find-definitions-in-file fs file symbol)]
                       [refs (search-file-for-symbol fs file symbol)])
                      (set! all-defs (append all-defs defs))
                      (set! all-refs (append all-refs refs))))
         files)
        
        ;; Write report
        (call-with-output-file output-file
                               (lambda (port)
                                       (display (format "CROSS-REFERENCE REPORT FOR '~a'\n" symbol) port)
                                       (display "====\n\n" port)
                                       (display (format "Search directory: ~a\n" dir) port)
                                       (display (format "Files scanned: ~a\n\n" (length files)) port)
                                       
                                       ;; Definitions section
                                       (display "DEFINITIONS\n" port)
                                       (display "----\n" port)
                                       (if (null? all-defs)
                                           (display "No definitions found.\n\n" port)
                                           (begin
                                            (display (format "Found ~a definitions:\n\n" (length all-defs)) port)
                                            (for-each
                                             (lambda (def)
                                                     (let ([file (car def)]
                                                           [line-no (cadr def)]
                                                           [context (caddr def)])
                                                          (display (format "~a:~a\n" file line-no) port)
                                                          (display (format "  ~a\n\n" (string-trim context)) port)))
                                             all-defs)))
                                       
                                       ;; References section
                                       (display "REFERENCES\n" port)
                                       (display "----\n" port)
                                       (if (null? all-refs)
                                           (display "No references found.\n\n" port)
                                           (begin
                                            (display (format "Found ~a references:\n\n" (length all-refs)) port)
                                            (for-each
                                             (lambda (ref)
                                                     (let ([file (car ref)]
                                                           [line-no (cadr ref)]
                                                           [context (caddr ref)])
                                                          (display (format "~a:~a\n" file line-no) port)
                                                          (display (format "  ~a\n\n" (string-trim context)) port)))
                                             all-refs)))))
        
        (display (format "Report written to ~a\n" output-file))))

(define (xref-index fs dir-path)
  (doc 'type (-> FS String Void))
  (doc 'description "Build and display an index of all definitions in a directory")
  (let* ([files (find-all-scheme-files fs dir-path)]
         [index (make-eq-hashtable)])
        
        ;; Build index: symbol -> list of (file line-no context)
        (for-each
         (lambda (file)
                 (guard (e [else (void)])
                        (let* ([content (read-text-file fs file)]
                               [lines (string-split-lines content)]
                               [port (open-input-string content)])
                              (let loop ([line-no 1])
                                   (let ([expr (guard (e [else #f])
                                                      (read port))])
                                        (cond
                                         [(eof-object? expr) (void)]
                                         [(not expr) (void)]
                                         [else
                                          (let ([defined-name (extract-defined-name expr)])
                                               (when defined-name
                                                     (let* ([context-line (get-context-line lines line-no)]
                                                            [entry (list file line-no context-line)]
                                                            [existing (hashtable-ref index defined-name '())])
                                                           (hashtable-set! index defined-name (cons entry existing)))))
                                          (loop (+ line-no 1))]))))))
         files)
        
        ;; Display index
        (display (format "\nDefinition Index for ~a:\n" dir-path))
        (display (format "Total symbols: ~a\n\n" (hashtable-size index)))
        
        (let ([symbols (list-sort
                        (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                        (vector->list (hashtable-keys index)))])
             (for-each
              (lambda (sym)
                      (let ([defs (hashtable-ref index sym '())])
                           (display (format "~a (~a definitions)\n" sym (length defs)))
                           (for-each
                            (lambda (def)
                                    (display (format "  ~a:~a\n" (car def) (cadr def))))
                            defs)))
              symbols))
        (display "\n")))

(doc 'section 'utilities)
(doc 'note "string-trim provided by core/prelude.ss")
(doc 'note "remove-duplicates provided by core/base/prelude.ss")

(display "Cross-reference tool loaded.\n")
(display "Usage:\n")
(display "  (xref-find-uses fs \"symbol-name\" [\"directory\"])\n")
(display "  (xref-find-defs fs \"symbol-name\" [\"directory\"])\n")
(display "  (xref-report fs \"symbol-name\" \"output-file\" [\"directory\"])\n")
(display "  (xref-index fs \"directory\")\n")
