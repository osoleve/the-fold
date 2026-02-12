(load "core/base/prelude.ss")
(load "core/base/string/string-core.ss")
(load "core/base/string/string-search.ss")

(doc 'module 'index)
(doc 'description "Symbol Index and Module Registry - provides discovery and navigation for The Fold codebase")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'index-data-structures)

(doc 'note "Symbol entry: ((name . symbol) (kind . define|define-syntax) (file . path) (line . number) (signature . string|#f) (docstring . string|#f) (module . path))")
(doc 'note "Module entry: ((path . string) (defines . (symbol ...)) (loads . (path ...)) (scan-time . timestamp))")

;;; Global index state
(define *symbol-index* (make-eq-hashtable))  ; symbol -> entry
(define *index-module-registry* (make-hashtable string-hash string=?))  ; path -> module-entry
(define *reverse-deps* (make-hashtable string-hash string=?))  ; path -> list of dependent paths

;;; ====
;;; File Scanning
;;; ====

;;; read-file-lines : String → (List String)
;;; Read all lines from a file.
(define (read-file-lines path)
  (guard (e [else '()])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([lines '()])
                                            (let ([line (get-line port)])
                                                 (if (eof-object? line)
                                                     (reverse lines)
                                                     (loop (cons line lines)))))))))

;;; index-extract-definitions : String × (List String) → (List Entry)
;;; Extract definition entries from file content.
;;; Accumulates consecutive ;;; comment lines for multi-line docstrings.
(define (index-extract-definitions path lines)
  (let loop ([lines lines]
             [line-num 1]
             [prev-comments '()]  ; List of comment lines (reversed)
             [results '()])
       (if (null? lines)
           (reverse results)
           (let* ([line (car lines)]
                  [trimmed (string-trim line)])
                 (cond
                  ;; Docstring comment (;;; ...) - accumulate
                  [(and (>= (string-length trimmed) 4)
                        (string-starts-with? trimmed ";;; "))
                   (loop (cdr lines) (+ line-num 1)
                         (cons trimmed prev-comments)  ; accumulate
                         results)]

                  ;; Definition forms
                  [(string-starts-with? trimmed "(define ")
                   (let ([entry (parse-define path line-num trimmed
                                              (reverse prev-comments))])
                        (loop (cdr lines) (+ line-num 1) '()
                              (if entry (cons entry results) results)))]

                  [(string-starts-with? trimmed "(define-syntax ")
                   (let ([entry (parse-define-syntax path line-num trimmed
                                                     (reverse prev-comments))])
                        (loop (cdr lines) (+ line-num 1) '()
                              (if entry (cons entry results) results)))]

                  ;; Any other line clears the docstring context
                  [(and (> (string-length trimmed) 0)
                        (not (string-starts-with? trimmed ";;")))
                   (loop (cdr lines) (+ line-num 1) '() results)]

                  [else
                   (loop (cdr lines) (+ line-num 1) prev-comments results)])))))

;;; parse-define : String × Nat × String × (Option String) → (Option Entry)
;;; Parse a define form and extract name.
(define (parse-define path line-num line prev-comment)
  (let ([rest (substring line 8 (string-length line))])  ; skip "(define "
       (cond
        ;; (define (name ...) ...)
        [(and (> (string-length rest) 0)
              (char=? (string-ref rest 0) #\())
         (let ([name-end (find-name-end rest 1)])
              (if name-end
                  (let ([name-str (substring rest 1 name-end)])
                       (make-symbol-entry
                        (string->symbol name-str)
                        'define
                        path
                        line-num
                        (extract-signature prev-comment)
                        (extract-docstring prev-comment)))
                  #f))]
        ;; (define name ...)
        [else
         (let ([name-end (find-name-end rest 0)])
              (if name-end
                  (let ([name-str (substring rest 0 name-end)])
                       (make-symbol-entry
                        (string->symbol name-str)
                        'define
                        path
                        line-num
                        (extract-signature prev-comment)
                        (extract-docstring prev-comment)))
                  #f))])))

;;; parse-define-syntax : String × Nat × String × (Option String) → (Option Entry)
;;; Parse a define-syntax form.
(define (parse-define-syntax path line-num line prev-comment)
  (let ([rest (substring line 15 (string-length line))])  ; skip "(define-syntax "
       (let ([name-end (find-name-end rest 0)])
            (if name-end
                (let ([name-str (substring rest 0 name-end)])
                     (make-symbol-entry
                      (string->symbol name-str)
                      'syntax
                      path
                      line-num
                      #f
                      (extract-docstring prev-comment)))
                #f))))

;;; find-name-end : String × Nat → (Option Nat)
;;; Find end of identifier in string.
(define (find-name-end str start)
  (let ([len (string-length str)])
       (let loop ([i start])
            (cond
             [(>= i len) (if (> i start) i #f)]
             [(char-name-constituent? (string-ref str i))
              (loop (+ i 1))]
             [(> i start) i]
             [else #f]))))

;;; char-name-constituent? : Char → Boolean
(define (char-name-constituent? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\:))))

;;; extract-signature : (List String) → (Option String)
;;; Extract type signature from first docstring comment line.
;;; Looks for ";;; name : signature" pattern.
(define (extract-signature comments)
  (and (pair? comments)
       (let* ([first-line (car comments)]
              [colon-pos (string-find-char first-line #\:)])
         (and colon-pos
              (< colon-pos (- (string-length first-line) 1))
              (string-trim (substring first-line (+ colon-pos 1) (string-length first-line)))))))

;;; extract-docstring : (List String) → (Option String)
;;; Extract description from docstring comments.
;;; Uses lines after the first (signature line), or the part after ":"
;;; on the first line if there's only one line.
(define (extract-docstring comments)
  (cond
   [(null? comments) #f]
   [(null? (cdr comments))
    ;; Single line: extract text after ";;; " prefix
    (let ([line (car comments)])
      (and (> (string-length line) 4)
           (substring line 4 (string-length line))))]
   [else
    ;; Multiple lines: join all lines after the first (signature line)
    ;; Each line has ";;; " prefix to strip
    (let ([doc-lines (map (lambda (line)
                            (if (> (string-length line) 4)
                                (substring line 4 (string-length line))
                                ""))
                          (cdr comments))])
      (string-join doc-lines "\n"))]))

;;; string-find-char : String × Char → (Option Nat)
(define (string-find-char str ch)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) ch) i]
             [else (loop (+ i 1))]))))

;;; make-symbol-entry : Symbol × Symbol × String × Nat × (Option String) × (Option String) → Entry
(define (make-symbol-entry name kind file line sig doc)
  `((name . ,name)
    (kind . ,kind)
    (file . ,file)
    (line . ,line)
    (signature . ,sig)
    (docstring . ,doc)))

;;; ====
;;; Load Statement Extraction
;;; ====

;;; extract-loads : (List String) → (List String)
;;; Extract loaded file paths from source lines.
(define (extract-loads lines)
  (let loop ([lines lines] [results '()])
       (if (null? lines)
           (reverse results)
           (let* ([line (string-trim (car lines))])
                 (cond
                  [(string-starts-with? line "(load \"")
                   (let ([end (string-find-char (substring line 7 (string-length line)) #\")])
                        (if end
                            (loop (cdr lines) (cons (substring line 7 (+ 7 end)) results))
                            (loop (cdr lines) results)))]
                  [else (loop (cdr lines) results)])))))

;;; ====
;;; Index Building
;;; ====

;;; scan-module : String → ModuleEntry
;;; Scan a single module file.
(define (scan-module path)
  (let* ([lines (read-file-lines path)]
         [defs (index-extract-definitions path lines)]
         [loads (extract-loads lines)]
         [def-names (map (lambda (e) (cdr (assq 'name e))) defs)])
        ;; Register each definition in symbol index
        (for-each
         (lambda (entry)
                 (hashtable-set! *symbol-index*
                                 (cdr (assq 'name entry))
                                 entry))
         defs)
        ;; Return module entry
        `((path . ,path)
          (defines . ,def-names)
          (loads . ,loads)
          (scan-time . ,(current-time)))))

;;; index-directory : String → Void
;;; Recursively index all .ss files in directory.
(define (index-directory dir)
  (let ([files (directory-list-recursive dir ".ss")])
       (for-each
        (lambda (path)
                (let ([entry (scan-module path)])
                     (hashtable-set! *index-module-registry* path entry)
                     ;; Update reverse dependencies
                     (for-each
                      (lambda (loaded)
                              (let ([existing (hashtable-ref *reverse-deps* loaded '())])
                                   (hashtable-set! *reverse-deps* loaded (cons path existing))))
                      (cdr (assq 'loads entry)))))
        files)))

;;; directory-list-recursive : String × String → (List String)
;;; List all files with given extension recursively.
(define (directory-list-recursive dir ext)
  (guard (e [else '()])
         (let loop ([pending (list dir)] [results '()])
              (if (null? pending)
                  results
                  (let ([current (car pending)])
                       (if (file-directory? current)
                           (let ([entries (map (lambda (e) (string-append current "/" e))
                                               (directory-list current))])
                                (loop (append (cdr pending) entries) results))
                           (if (string-ends-with? current ext)
                               (loop (cdr pending) (cons current results))
                               (loop (cdr pending) results))))))))

;;; ====
;;; Query Functions
;;; ====

;;; index-refresh! : Unit → Void
;;; Rebuild the entire index.
(define (index-refresh!)
  ;; Clear existing index
  (hashtable-clear! *symbol-index*)
  (hashtable-clear! *index-module-registry*)
  (hashtable-clear! *reverse-deps*)
  ;; Scan all key directories
  (for-each (lambda (dir)
              (when (file-exists? dir)
                (index-directory dir)))
            '("core" "lattice" "boundary" "user"))
  (display "Index complete.\n")
  (index-stats))

;;; index-refresh-file! : String → Void
;;; Incrementally refresh index for a single file.
;;; Removes old entries and re-scans the file.
(define (index-refresh-file! path)
  ;; 1. Get old module entry to find symbols to remove
  (let ([old-entry (hashtable-ref *index-module-registry* path #f)])
    (when old-entry
      ;; Remove old symbol entries for this file
      (for-each
       (lambda (sym)
         (hashtable-delete! *symbol-index* sym))
       (cdr (assq 'defines old-entry)))
      ;; Remove old reverse deps contributed by this file
      (for-each
       (lambda (loaded)
         (let ([deps (hashtable-ref *reverse-deps* loaded '())])
           (hashtable-set! *reverse-deps* loaded
                           (filter (lambda (d) (not (string=? d path))) deps))))
       (cdr (assq 'loads old-entry)))))
  ;; 2. Re-scan the file (if it still exists)
  (when (file-exists? path)
    (let ([entry (scan-module path)])
      (hashtable-set! *index-module-registry* path entry)
      ;; Update reverse dependencies
      (for-each
       (lambda (loaded)
         (let ([existing (hashtable-ref *reverse-deps* loaded '())])
           (hashtable-set! *reverse-deps* loaded (cons path existing))))
       (cdr (assq 'loads entry))))))

;;; index-find : String → (List Entry)
;;; Find symbols whose names contain pattern.
(define (index-find pattern)
  (let ([pattern-lower (string-downcase pattern)]
        [keys (hashtable-keys *symbol-index*)]
        [results '()])
       (vector-for-each
        (lambda (name)
                (when (string-contains? (string-downcase (symbol->string name)) pattern-lower)
                      (set! results (cons (hashtable-ref *symbol-index* name #f) results))))
        keys)
       (sort-entries-by-name results)))

;;; ?? : String → Void
;;; Quick search and display. Usage: (?? "variance")
(define (?? pattern)
  (let ([results (index-find pattern)])
    (if (null? results)
        (printf "  No matches for '~a'\n" pattern)
        (begin
          (printf "\n  ~a match~a for '~a':\n"
                  (length results)
                  (if (= 1 (length results)) "" "es")
                  pattern)
          (display "  ────────────────────────────────\n")
          (for-each
           (lambda (e)
             (let ([name (cdr (assq 'name e))]
                   [sig (cdr (assq 'signature e))]
                   [file (cdr (assq 'file e))]
                   [line (cdr (assq 'line e))])
               (display "  ")
               (display name)
               (when sig (display " : ") (display sig))
               (printf "\n    ~a:~a\n" file line)))
           results)
          (display "\n")))))

;;; index-lookup : Symbol → (Option Entry)
;;; Get entry for exact symbol name.
(define (index-lookup name)
  (hashtable-ref *symbol-index* name #f))

;;; index-show : Entry → Void
;;; Pretty-print a symbol entry.
(define (index-show entry)
  (if (not entry)
      (display "  (not found)\n")
      (let ([name (cdr (assq 'name entry))]
            [kind (cdr (assq 'kind entry))]
            [file (cdr (assq 'file entry))]
            [line (cdr (assq 'line entry))]
            [sig (cdr (assq 'signature entry))]
            [doc (cdr (assq 'docstring entry))])
        (display "\n")
        (display "  ") (display name)
        (when sig
          (display " : ") (display sig))
        (display "\n")
        (display "  ────────────────────────────────\n")
        (printf "  ~a at ~a:~a\n" kind file line)
        (when doc
          (display "\n  ")
          (display (string-replace doc "\n" "\n  "))
          (display "\n"))
        (display "\n"))))

;;; ? : Symbol → Void
;;; Quick lookup and display. Usage: (? 'vec-variance)
(define (? name)
  (index-show (index-lookup name)))

;;; index-module : String → (Option ModuleEntry)
;;; Get module entry by path.
(define (index-module path)
  (hashtable-ref *index-module-registry* path #f))

;;; index-exports : String → (List Symbol)
;;; Get symbols defined in module.
(define (index-exports path)
  (let ([mod (index-module path)])
       (if mod
           (cdr (assq 'defines mod))
           '())))

;;; index-deps : String → (List String)
;;; Get modules this one depends on (loads).
(define (index-deps path)
  (let ([mod (index-module path)])
       (if mod
           (cdr (assq 'loads mod))
           '())))

;;; index-dependents : String → (List String)
;;; Get modules that depend on (load) this one.
(define (index-dependents path)
  (hashtable-ref *reverse-deps* path '()))

;;; index-stats : Unit → Void
;;; Display index statistics.
(define (index-stats)
  (display "
")
  (display "  Index Statistics
")
  (display "  ────────────────────────────────
")
  (printf "  Symbols indexed: ~a
" (hashtable-size *symbol-index*))
  (printf "  Modules scanned: ~a
" (hashtable-size *index-module-registry*))
  (display "
"))

;;; sort-entries-by-name : (List Entry) → (List Entry)
(define (sort-entries-by-name entries)
  (list-sort
   (lambda (a b)
           (string<? (symbol->string (cdr (assq 'name a)))
                     (symbol->string (cdr (assq 'name b)))))
   entries))

;;; ====
;;; Display Functions
;;; ====

;;; display-entry : Entry → Void
;;; Pretty print a symbol entry.
(define (display-entry entry)
  (let ([name (cdr (assq 'name entry))]
        [kind (cdr (assq 'kind entry))]
        [file (cdr (assq 'file entry))]
        [line (cdr (assq 'line entry))]
        [sig (cdr (assq 'signature entry))]
        [doc (cdr (assq 'docstring entry))])
       (printf "  ~a" name)
       (when sig (printf " : ~a" sig))
       (display "
")
       (printf "    [~a] ~a:~a
" kind file line)
       (when doc (printf "    ~a
" doc))))

;;; display-find-results : (List Entry) → Void
;;; Display search results.
(define (display-find-results entries)
  (display "
")
  (if (null? entries)
      (display "  No matches found.
")
      (begin
       (printf "  Found ~a symbol(s):

" (length entries))
       (for-each display-entry entries)))
  (display "
"))

;;; ====
;;; REPL Integration
;;; ====

;;; find : String → Void
;;; User-friendly search command.
(define (find pattern)
  (display-find-results (index-find pattern)))

;;; describe : Symbol → Void
;;; Show detailed info about a symbol.
(define (describe name)
  (let ([entry (index-lookup name)])
       (display "
")
       (if entry
           (display-entry entry)
           (printf "  Symbol '~a' not found in index.
" name))
       (display "
")))

;;; whatis : Symbol → Void
;;; Alias for describe.
(define whatis describe)

;;; where : Symbol → Void
;;; Show where a symbol is defined.
(define (where name)
  (let ([entry (index-lookup name)])
       (if entry
           (printf "~a:~a
"
                   (cdr (assq 'file entry))
                   (cdr (assq 'line entry)))
           (printf "Symbol '~a' not found.
" name))))

;;; ====
;;; Module Exploration
;;; ====

;;; modules : String... → Void
;;; List all indexed modules, optionally filtered by pattern.
(define (modules . args)
  (let* ([pattern (if (null? args) "" (car args))]
         [pattern-lower (string-downcase pattern)]
         [keys (hashtable-keys *index-module-registry*)]
         [paths '()])
        (vector-for-each
         (lambda (path)
                 (when (string-contains? (string-downcase path) pattern-lower)
                       (set! paths (cons path paths))))
         keys)
        (display "
")
        (if (null? paths)
            (display "  No modules found.
")
            (begin
             (printf "  Found ~a module(s):

" (length paths))
             (for-each
              (lambda (path)
                      (let* ([mod (hashtable-ref *index-module-registry* path #f)]
                             [defs (cdr (assq 'defines mod))]
                             [loads (cdr (assq 'loads mod))])
                            (printf "  ~a
" path)
                            (printf "    defines: ~a symbols, loads: ~a modules
"
                                    (length defs) (length loads))))
              (list-sort string<? paths))))
        (display "
")))

;;; deps : String → Void
;;; Show dependency tree for a module.
(define (deps path)
  (let ([mod (index-module path)])
       (display "
")
       (if mod
           (let ([loads (cdr (assq 'loads mod))]
                 [dependents (index-dependents path)])
                (printf "  Module: ~a

" path)
                (printf "  Loads (~a):
" (length loads))
                (for-each (lambda (p) (printf "    <- ~a
" p)) loads)
                (printf "
  Loaded by (~a):
" (length dependents))
                (for-each (lambda (p) (printf "    -> ~a
" p)) dependents))
           (printf "  Module '~a' not found.
" path))
       (display "
")))

;;; symbols : String → Void
;;; List all symbols defined in a module.
(define (symbols path)
  (let ([mod (index-module path)])
       (display "
")
       (if mod
           (let ([defs (cdr (assq 'defines mod))])
                (printf "  Symbols in ~a (~a):

" path (length defs))
                (for-each
                 (lambda (name)
                         (let ([entry (index-lookup name)])
                              (if entry
                                  (let ([sig (cdr (assq 'signature entry))])
                                       (if sig
                                           (printf "    ~a : ~a
" name sig)
                                           (printf "    ~a
" name)))
                                  (printf "    ~a
" name))))
                 (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) defs)))
           (printf "  Module '~a' not found.
" path))
       (display "
")))

;;; browse : Unit → Void
;;; Interactive index browser.
(define (browse)
  (display "
")
  (display "  Index Browser Commands:
")
  (display "  ───────────────────────────────────────────
")
  (display "  (find \"pattern\")     - Search symbols
")
  (display "  (describe 'name)     - Symbol details
")
  (display "  (where 'name)        - Symbol location
")
  (display "  (modules)            - List all modules
")
  (display "  (modules \"pattern\") - Filter modules
")
  (display "  (symbols \"path\")     - Symbols in module
")
  (display "  (deps \"path\")        - Module dependencies
")
  (display "  (index-stats)        - Index statistics
")
  (display "  (index-refresh!)     - Rebuild index
")
  (display "
"))

(printf "Index system loaded. Use (index-refresh!) to build index.
")
