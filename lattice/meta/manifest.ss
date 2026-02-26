;;; @module manifest
(doc 'module 'manifest)
(doc 'description "Pure functions for parsing lattice skill manifests. No I/O - takes S-expression input, returns structured data.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'field-extraction)

(doc manifest-field 'type (-> SExp Symbol (Maybe Any)))
(doc manifest-field 'description "Extract a field from a manifest s-expression. A manifest has the form: (skill <name> (<field> <values>...) ...)")
(define (manifest-field manifest field-name)
  (if (and (pair? manifest)
           (eq? (car manifest) 'skill)
           (pair? (cdr manifest))      ; Has at least skill name
           (pair? (cddr manifest)))    ; Has at least one field
      (let loop ([fields (cddr manifest)])
           (cond
            [(null? fields) #f]
            [(and (pair? (car fields))
                  (eq? (caar fields) field-name))
             (cdar fields)]
            [else (loop (cdr fields))]))
      #f))

(doc 'section 'helpers)

(doc flatten-single 'type (-> (Maybe (List a)) (List a)))
(doc flatten-single 'description "If list contains single element that is itself a list, return that inner list. Handles manifest fields like (deps ((linalg algebra))) -> (linalg algebra)")
(define (flatten-single lst)
  (cond
   [(not lst) '()]
   [(null? lst) '()]
   [(and (= (length lst) 1)
         (list? (car lst)))
    (car lst)]
   [else lst]))

;;; flatten-modules : (List Any) -> (List ModuleEntry)
;;; Like flatten-single but module-aware. Unwraps the extra nesting in
;;; (modules ( entries... )) format, but NOT single-module entries like
;;; ((name "file.ss" "desc")). Disambiguates by checking whether the
;;; first element of the inner list is a symbol/string (module entry)
;;; or a list (nested subdir wrapper).
(define (flatten-modules lst)
  (cond
   [(not lst) '()]
   [(null? lst) '()]
   [(and (= (length lst) 1)
         (list? (car lst))
         (pair? (car lst))
         ;; If inner list's first element is itself a list, it's the
         ;; nested (modules ( entries... )) format — unwrap.
         ;; If it's a symbol/string, it's a single module entry — keep wrapped.
         (list? (caar lst)))
    (car lst)]
   [else lst]))

(doc car-or-default 'type (-> (Maybe (List a)) a a))
(doc car-or-default 'description "Get first element or return default.")
(define (car-or-default lst default)
  (if (and lst (pair? lst))
      (car lst)
      default))

(doc 'section 'parsing)

(doc parse-manifest 'type (-> SExp (Maybe ManifestData)))
(doc parse-manifest 'description "Parse a manifest s-expression into structured alist data. Returns #f if the input is not a valid manifest. ManifestData is an alist with keys: name, version, path, purity, stability, fuel-bound, deps, description, keywords, aliases, exports, modules, concepts. Tier is derived from DAG depth (see lattice-depth).")
(define (parse-manifest sexp)
  (if (and (pair? sexp) (eq? (car sexp) 'skill))
      (let ([name (cadr sexp)])
           `((name . ,name)
             (version . ,(car-or-default (manifest-field sexp 'version) "0.0.0"))
             (path . ,(car-or-default (manifest-field sexp 'path) ""))
             (purity . ,(car-or-default (manifest-field sexp 'purity) 'total))
             (stability . ,(car-or-default (manifest-field sexp 'stability) 'experimental))
             (fuel-bound . ,(car-or-default (manifest-field sexp 'fuel-bound) "O(?)"))
             (deps . ,(flatten-single (manifest-field sexp 'deps)))
             (description . ,(car-or-default (manifest-field sexp 'description) ""))
             (keywords . ,(flatten-single (manifest-field sexp 'keywords)))
             (aliases . ,(flatten-single (manifest-field sexp 'aliases)))
             (exports . ,(or (manifest-field sexp 'exports) '()))
             (modules . ,(flatten-modules (or (manifest-field sexp 'modules) '())))
             (concepts . ,(or (manifest-field sexp 'concepts) '()))))
      #f))

(doc 'section 'accessors)

(define (manifest-name manifest)
  (doc 'type (-> ManifestData Symbol))
  (cdr (assq 'name manifest)))

(define (manifest-path manifest)
  (doc 'type (-> ManifestData String))
  (cdr (assq 'path manifest)))

(doc manifest-modules 'type (-> ManifestData (List (List Symbol String String))))
(doc manifest-modules 'description "Get the modules list from a parsed manifest. Each module entry is (name \"file.ss\" \"description\")")
(define (manifest-modules manifest)
  (let ([mods (cdr (assq 'modules manifest))])
       (if (list? mods) mods '())))

(doc manifest-deps 'type (-> ManifestData (List Symbol)))
(doc manifest-deps 'description "Get the skill dependencies from a parsed manifest.")
(define (manifest-deps manifest)
  (let ([deps (cdr (assq 'deps manifest))])
       (if (list? deps) deps '())))

(doc manifest-exports 'type (-> ManifestData (List (List Symbol))))
(doc manifest-exports 'description "Get the exports list from a parsed manifest.")
(define (manifest-exports manifest)
  (let ([exports (cdr (assq 'exports manifest))])
       (if (list? exports) exports '())))

(doc 'section 'module-index)

(doc parse-module-entry 'type (-> SExp String (List (Pair Symbol String))))
(doc parse-module-entry 'description "Parse a single module entry and return module index entries. Handles both simple format: (name \"file.ss\" \"desc\") and nested format: ((subdir \"name\") (description \"...\") (files (\"a.ss\" \"b.ss\")))")
(define (parse-module-entry mod skill-path)
  (cond
   ;; Simple format: (name "file.ss" "description") — name can be symbol or string
   [(and (pair? mod)
         (or (symbol? (car mod)) (string? (car mod)))
         (>= (length mod) 2)
         (string? (cadr mod)))
    (let* ([mod-name (if (string? (car mod))
                         (string->symbol (car mod))
                         (car mod))]
           [mod-file (cadr mod)]
           [full-path (string-append skill-path "/" mod-file)])
          (list (cons mod-name full-path)))]

   ;; Nested format: ((subdir "name") (description "...") (files (...)))
   [(and (pair? mod)
         (pair? (car mod))
         (eq? (caar mod) 'subdir))
    (let* ([subdir (cadar mod)]
           [files-entry (assq 'files mod)]
           [files (if files-entry (cadr files-entry) '())]
           [subdir-path (if (string=? subdir "")
                            skill-path
                            (string-append skill-path "/" subdir))])
          (filter-map
           (lambda (file)
                   (if (string? file)
                       (let* ([mod-name (string->symbol (path->module-name file))]
                              [full-path (string-append subdir-path "/" file)])
                             (cons mod-name full-path))
                       #f))
           files))]

   ;; Unknown format
   [else '()]))

(doc path->module-name 'type (-> String String))
(doc path->module-name 'description "Convert \"foo.ss\" to \"foo\", \"bar.scm\" to \"bar\"")
(define (path->module-name path)
  (let ([len (string-length path)])
       (cond
        [(and (> len 3) (string=? (substring path (- len 3) len) ".ss"))
         (substring path 0 (- len 3))]
        [(and (> len 4) (string=? (substring path (- len 4) len) ".scm"))
         (substring path 0 (- len 4))]
        [else path])))

(doc manifest->module-index 'type (-> ManifestData (List (Pair Symbol String))))
(doc manifest->module-index 'description "Convert a manifest to a list of (module-name . file-path) pairs. The path is constructed from the manifest's path field. Handles both simple and nested module formats. Example: For manifest with (path \"lattice/linalg\") and module (vec \"vec.ss\" ...) Returns: ((vec . \"lattice/linalg/vec.ss\") ...)")
(define (manifest->module-index manifest)
  (let* ([skill-path (manifest-path manifest)]
         [mods (manifest-modules manifest)])
        (append-map (lambda (mod) (parse-module-entry mod skill-path))
                    mods)))

(doc manifest->namespaced-index 'type (-> ManifestData (List (Pair Symbol String))))
(doc manifest->namespaced-index 'description "Like manifest->module-index but creates namespaced module names. These are always unambiguous: 'linalg/vec, 'diffgeo/charts, etc. Example: For manifest 'linalg with module (vec \"vec.ss\" ...) Returns: ((linalg/vec . \"lattice/linalg/vec.ss\") ...)")
(define (manifest->namespaced-index manifest)
  (let* ([skill-name (manifest-name manifest)]
         [simple-entries (manifest->module-index manifest)])
        ;; Transform simple entries to namespaced entries
        (map (lambda (entry)
                     (let* ([mod-name (car entry)]
                            [path (cdr entry)]
                            [namespaced-name (string->symbol
                                              (string-append (symbol->string skill-name)
                                                             "/"
                                                             (symbol->string mod-name)))])
                           (cons namespaced-name path)))
             simple-entries)))

;;; filter-map provided by prelude

(doc 'section 'validation)

(doc valid-manifest? 'type (-> ManifestData Boolean))
(doc valid-manifest? 'description "Check if a parsed manifest has required fields.")
(define (valid-manifest? manifest)
  (and (pair? manifest)
       (assq 'name manifest)
       (assq 'path manifest)
       (let ([name (cdr (assq 'name manifest))]
             [path (cdr (assq 'path manifest))])
            (and (symbol? name)
                 (string? path)
                 (> (string-length path) 0)))))
