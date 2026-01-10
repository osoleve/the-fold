;;; shell/tools/manifest-gen.ss — Manifest Generation Helper
;;;
;;; Tools to scan lattice skill directories and generate manifest.sexp files.
;;; Extracts function definitions, type signatures, and module metadata.
;;;
;;; This is Shell code: reads files, handles IO.
;;;
;;; Usage:
;;;   (manifest-scan "lattice/algebra")      ; Scan dir, return analysis
;;;   (manifest-template 'algebra 0)         ; Generate template for tier 0 skill
;;;   (manifest-generate "lattice/algebra")  ; Full generation
;;;   (manifest-validate "manifest.sexp")    ; Validate existing manifest
;;;
;;; Dependencies:
;;;   core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; File Scanning
;;; ============================================================

;;; list-scheme-files : String -> (List String)
;;; List all .ss files in a directory (non-recursive)
(define (list-scheme-files dir)
  (filter (lambda (f) (string-ends-with? f ".ss"))
          (directory-list dir)))

;;; read-file-string : String -> String
;;; Read entire file contents as string
(define (read-file-string filepath)
  (call-with-input-file filepath
                        (lambda (port)
                                (get-string-all port))))

;;; string-ends-with? : String String -> Bool
(define (string-ends-with? str suffix)
  (let ([str-len (string-length str)]
        [suf-len (string-length suffix)])
       (and (>= str-len suf-len)
            (string=? (substring str (- str-len suf-len) str-len) suffix))))

;;; ============================================================
;;; Source Code Parsing
;;; ============================================================

;;; extract-defines : String -> (List Symbol)
;;; Extract all top-level (define ...) names from source code
(define (extract-defines source-code)
  (let ([port (open-input-string source-code)])
       (let loop ([defines '()])
            (let ([expr (guard (e [else #f]) (read port))])
                 (cond
                  [(eof-object? expr) (reverse defines)]
                  [(not expr) (reverse defines)]
                  [(and (pair? expr)
                        (eq? (car expr) 'define)
                        (pair? (cdr expr)))
                   (let ([name-part (cadr expr)])
                        (cond
                         ;; (define (name args...) body)
                         [(pair? name-part)
                          (loop (cons (car name-part) defines))]
                         ;; (define name value)
                         [(symbol? name-part)
                          (loop (cons name-part defines))]
                         [else (loop defines)]))]
                  [(and (pair? expr)
                        (eq? (car expr) 'define-record-type)
                        (pair? (cdr expr))
                        (symbol? (cadr expr)))
                   (loop (cons (cadr expr) defines))]
                  [else (loop defines)])))))

;;; extract-header-metadata : String -> Alist
;;; Extract @module, @requires from file header comments
(define (extract-header-metadata source-code)
  (let ([lines (string-split source-code #\newline)])
       (let loop ([lines lines]
                  [metadata '()]
                  [in-header? #t])
            (cond
             [(null? lines) (reverse metadata)]
             [(not in-header?) (reverse metadata)]
             [else
              (let ([line (car lines)])
                   (cond
                    ;; @module name
                    [(string-contains? line "@module")
                     (let ([name (extract-tag-value line "@module")])
                          (loop (cdr lines)
                                (cons (cons 'module name) metadata)
                                #t))]
                    ;; @requires dep1, dep2
                    [(string-contains? line "@requires")
                     (let ([deps (extract-tag-value line "@requires")])
                          (loop (cdr lines)
                                (cons (cons 'requires deps) metadata)
                                #t))]
                    ;; Still in comment header
                    [(string-starts-with? (string-trim-left line) ";;;")
                     (loop (cdr lines) metadata #t)]
                    ;; Empty line - still might be in header
                    [(string=? (string-trim line) "")
                     (loop (cdr lines) metadata #t)]
                    ;; Hit non-comment line - end of header
                    [else (reverse metadata)]))]))))

;;; extract-tag-value : String String -> String
;;; Extract value after a @tag in a comment line
(define (extract-tag-value line tag)
  (let* ([idx (string-contains-idx line tag)]
         [start (+ idx (string-length tag))])
        (string-trim (substring line start (string-length line)))))

;;; string-contains-idx : String String -> Nat | #f
;;; Find index of substring, or #f if not found
(define (string-contains-idx str sub)
  (let ([str-len (string-length str)]
        [sub-len (string-length sub)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string=? (substring str i (+ i sub-len)) sub) i]
             [else (loop (+ i 1))]))))

;;; string-trim-left : String -> String
(define (string-trim-left str)
  (let loop ([chars (string->list str)])
       (cond
        [(null? chars) ""]
        [(char-whitespace? (car chars)) (loop (cdr chars))]
        [else (list->string chars)])))

;;; string-trim : String -> String
(define (string-trim str)
  (string-trim-left (string-trim-right str)))

;;; string-trim-right : String -> String
(define (string-trim-right str)
  (list->string (reverse (let loop ([chars (reverse (string->list str))])
                              (cond
                               [(null? chars) '()]
                               [(char-whitespace? (car chars)) (loop (cdr chars))]
                               [else chars])))))

;;; ============================================================
;;; Type Signature Extraction
;;; ============================================================

;;; extract-type-signatures : String -> (List (Symbol . String))
;;; Extract ;;; name : type comments
(define (extract-type-signatures source-code)
  (let ([lines (string-split source-code #\newline)])
       (filter-map
        (lambda (line)
                (let ([trimmed (string-trim-left line)])
                     (if (and (string-starts-with? trimmed ";;;")
                              (string-contains? trimmed " : "))
                         (parse-type-signature trimmed)
                         #f)))
        lines)))

;;; parse-type-signature : String -> (Symbol . String) | #f
;;; Parse ";;; name : Type -> Type" into (name . "Type -> Type")
(define (parse-type-signature line)
  (let* ([content (substring line 3 (string-length line))]  ; Remove ;;;
         [content (string-trim content)]
         [colon-idx (string-contains-idx content " : ")])
        (if colon-idx
            (let ([name (string-trim (substring content 0 colon-idx))]
                  [type (string-trim (substring content (+ colon-idx 3) (string-length content)))])
                 (if (and (> (string-length name) 0)
                          (not (string-contains? name " ")))  ; Single word = function name
                     (cons (string->symbol name) type)
                     #f))
            #f)))

;;; filter-map : (a -> b | #f) (List a) -> (List b)
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; ============================================================
;;; Module Analysis
;;; ============================================================

;;; analyze-module : String -> ModuleInfo
;;; Analyze a single .ss file
(define (analyze-module filepath)
  (let* ([content (read-file-string filepath)]
         [filename (path-basename filepath)]
         [module-name (string->symbol (path-stem filename))]
         [header (extract-header-metadata content)]
         [defines (extract-defines content)]
         [signatures (extract-type-signatures content)])
        `((file . ,filename)
          (module . ,module-name)
          (header . ,header)
          (exports . ,defines)
          (signatures . ,signatures))))

;;; path-basename : String -> String
;;; Extract filename from path
(define (path-basename path)
  (let ([parts (string-split path #\/)])
       (if (null? parts)
           path
           (let ([last (car (reverse parts))])
                (if (string=? last "")
                    (if (> (length parts) 1)
                        (cadr (reverse parts))
                        "")
                    last)))))

;;; path-stem : String -> String
;;; Remove extension from filename
(define (path-stem filename)
  (let ([dot-idx (string-last-index-of filename ".")])
       (if dot-idx
           (substring filename 0 dot-idx)
           filename)))

;;; string-last-index-of : String String -> Nat | #f
(define (string-last-index-of str sub)
  (let ([str-len (string-length str)]
        [sub-len (string-length sub)])
       (let loop ([i (- str-len sub-len)])
            (cond
             [(< i 0) #f]
             [(string=? (substring str i (+ i sub-len)) sub) i]
             [else (loop (- i 1))]))))

;;; ============================================================
;;; Directory Scanning
;;; ============================================================

;;; manifest-scan : String -> SkillAnalysis
;;; Scan a skill directory and return analysis
(define (manifest-scan dir)
  (let* ([files (list-scheme-files dir)]
         [skill-name (string->symbol (path-basename dir))]
         [modules (map (lambda (f)
                               (analyze-module (string-append dir "/" f)))
                       (filter (lambda (f)
                                       (not (string-starts-with? f "test-")))
                               files))])
        `((skill . ,skill-name)
          (path . ,dir)
          (module-count . ,(length modules))
          (modules . ,modules)
          (all-exports . ,(apply append (map (lambda (m) (cdr (assq 'exports m))) modules))))))

;;; ============================================================
;;; Manifest Generation
;;; ============================================================

;;; manifest-template : Symbol Nat (List Symbol) String -> String
;;; Generate a manifest.sexp template
(define (manifest-template skill-name tier deps description)
  (format ";;; lattice/~a/manifest.sexp — ~a Skill Manifest

(skill ~a
  (version \"0.1.0\")
  (tier ~a)
  (purity total)
  (stability experimental)
  (fuel-bound \"O(?)\")

  (deps ~s)

  (description
   \"~a\")

  (keywords ())
  (aliases ())

  (exports
   ;; TODO: Group exports by module
   ())

  (modules
   ;; TODO: Fill in from manifest-scan
   ()))
"
          skill-name
          (symbol->string skill-name)
          skill-name
          tier
          deps
          description))

;;; manifest-generate : String -> String
;;; Scan directory and generate complete manifest
(define (manifest-generate dir)
  (let* ([analysis (manifest-scan dir)]
         [skill-name (cdr (assq 'skill analysis))]
         [modules (cdr (assq 'modules analysis))]
         [all-exports (cdr (assq 'all-exports analysis))])
        (format ";;; lattice/~a/manifest.sexp — ~a Skill Manifest

(skill ~a
  (version \"0.1.0\")
  (tier 0)  ; TODO: Adjust tier
  (purity total)
  (stability experimental)
  (fuel-bound \"O(?)\")  ; TODO: Document complexity

  (deps ())  ; TODO: Add dependencies

  (description
   \"TODO: Add description\")

  (keywords ())  ; TODO: Add search keywords
  (aliases ())   ; TODO: Add aliases

  (exports
~a)

  (modules
~a))
"
                skill-name
                (symbol->string skill-name)
                skill-name
                (format-exports modules)
                (format-modules modules))))

;;; format-exports : (List ModuleInfo) -> String
;;; Format exports section
(define (format-exports modules)
  (let ([groups (map (lambda (m)
                             (let ([name (cdr (assq 'module m))]
                                   [exports (cdr (assq 'exports m))])
                                  (if (null? exports)
                                      ""
                                      (format "   (~a ~a)"
                                              name
                                              (string-join (map symbol->string exports) " ")))))
                     modules)])
       (string-join (filter (lambda (s) (not (string=? s ""))) groups) "\n")))

;;; format-modules : (List ModuleInfo) -> String
;;; Format modules section
(define (format-modules modules)
  (let ([entries (map (lambda (m)
                              (let ([name (cdr (assq 'module m))]
                                    [file (cdr (assq 'file m))])
                                   (format "   (~a \"~a\" \"TODO: description\")" name file)))
                      modules)])
       (string-join entries "\n")))

;;; string-join : (List String) String -> String
(define (string-join strings sep)
  (if (null? strings)
      ""
      (let loop ([strs (cdr strings)]
                 [acc (car strings)])
           (if (null? strs)
               acc
               (loop (cdr strs) (string-append acc sep (car strs)))))))

;;; ============================================================
;;; Manifest Validation
;;; ============================================================

;;; manifest-validate : String -> (List String)
;;; Validate a manifest file, return list of issues
(define (manifest-validate filepath)
  (let* ([content (read-file-string filepath)]
         [sexp (guard (e [else #f])
                      (read (open-input-string content)))])
        (cond
         [(not sexp) '("Parse error: invalid S-expression")]
         [(not (pair? sexp)) '("Invalid format: not a list")]
         [(not (eq? (car sexp) 'skill)) '("Missing (skill ...) wrapper")]
         [else (validate-skill-fields (cdr sexp))])))

;;; validate-skill-fields : SExp -> (List String)
(define (validate-skill-fields fields)
  (let ([issues '()]
        [has-version? #f]
        [has-tier? #f]
        [has-purity? #f]
        [has-deps? #f]
        [has-description? #f]
        [has-exports? #f]
        [has-modules? #f])
       (for-each
        (lambda (field)
                (when (pair? field)
                      (case (car field)
                            [(version) (set! has-version? #t)]
                            [(tier) (set! has-tier? #t)]
                            [(purity) (set! has-purity? #t)]
                            [(deps) (set! has-deps? #t)]
                            [(description) (set! has-description? #t)]
                            [(exports) (set! has-exports? #t)]
                            [(modules) (set! has-modules? #t)])))
        fields)
       (append
        (if has-version? '() '("Missing (version ...)"))
        (if has-tier? '() '("Missing (tier ...)"))
        (if has-purity? '() '("Missing (purity ...)"))
        (if has-deps? '() '("Missing (deps ...)"))
        (if has-description? '() '("Missing (description ...)"))
        (if has-exports? '() '("Missing (exports ...)"))
        (if has-modules? '() '("Missing (modules ...)")))))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "manifest-gen loaded.\n")
(printf "  (manifest-scan \"lattice/algebra\")     - Analyze skill directory\n")
(printf "  (manifest-generate \"lattice/algebra\") - Generate manifest template\n")
(printf "  (manifest-validate \"manifest.sexp\")   - Validate manifest file\n")
