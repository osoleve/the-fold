(unless (top-level-bound? 'sort-by) (load "lattice/data/sort.ss"))
(unless (top-level-bound? 'hamt-empty) (load "lattice/data/hamt.ss"))
(unless (top-level-bound? 'kg-skills) (load "lattice/meta/kg.ss"))
(unless (top-level-bound? 'bm25-create) (load "lattice/meta/bm25.ss"))
(unless (top-level-bound? 'get-docstring) (load "lattice/meta/docstrings.ss"))

(doc 'module 'search)
(doc 'description "Unified search interface for the skill lattice. Integrates BM25 with the knowledge graph for ranked results.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'state)

(doc *skill-index* 'description "Mutable BM25 index for skills")
(define *skill-index* (bm25-create))

(doc *module-index* 'description "Mutable BM25 index for modules")
(define *module-index* (bm25-create))

(doc *export-index* 'description "Mutable BM25 index for exports")
(define *export-index* (bm25-create))

(doc *module-cache* 'description "Pre-computed list of all modules for O(1) lookup")
(define *module-cache* '())

(doc *export-module-map* 'description "Reverse map: export symbol → module require path (e.g., make-group → group)")
(define *export-module-map* hamt-empty)

(doc *search-ready* 'description "Flag indicating search indices are built")
(define *search-ready* #f)

(doc 'section 'index-building)

(doc lattice-index! 'type (-> Void))
(doc lattice-index! 'description "Build search indices from the knowledge graph. Requires kg-build! to have been called first")
(define (lattice-index!)
  ;; Reset indices
  (set! *skill-index* (bm25-create))
  (set! *module-index* (bm25-create))
  (set! *export-index* (bm25-create))
  (set! *module-cache* '())
  (set! *export-module-map* hamt-empty)

  ;; Build docstring cache only if not already populated from cache
  (when (zero? (hamt-size *docstrings*))
        (build-docstring-cache!))

  ;; Index all skills
  (set! *skill-index*
        (fold-left
         (lambda (idx skill-name)
           (let ([manifest-data (kg-skill-data skill-name)])
             (if manifest-data
                 (let ([terms (skill->terms manifest-data)])
                   (bm25-add-doc idx skill-name terms manifest-data))
                 idx)))
         *skill-index*
         (kg-skills)))

  ;; Index all modules and build module cache
  (let ([acc (fold-left
              (lambda (acc skill-name)
                (let ([modules (kg-modules skill-name)])
                  (fold-left
                   (lambda (acc mod-entry)
                     (let* ([mod-key (car mod-entry)]
                            [mod-data `((name . ,mod-key)
                                        (skill . ,skill-name))]
                            [key-str (symbol->string mod-key)]
                            [terms (tokenize key-str)])
                       (cons (cons mod-entry (car acc))
                             (bm25-add-doc (cdr acc) mod-key terms mod-data))))
                   acc
                   modules)))
              (cons *module-cache* *module-index*)
              (kg-skills))])
    (set! *module-cache* (car acc))
    (set! *module-index* (cdr acc)))

  ;; Build export→module reverse mapping from manifest grouped exports
  ;; Manifest exports format: ((module-name sym1 sym2 ...) ...)
  (for-each
   (lambda (skill-name)
           (let ([manifest-data (kg-skill-data skill-name)])
                (when manifest-data
                      (let ([exports-raw (let ([e (assq 'exports manifest-data)])
                                              (if e (cdr e) '()))])
                           (when (list? exports-raw)
                                 (for-each
                                  (lambda (export-group)
                                          (when (pair? export-group)
                                                (let* ([first-sym (car export-group)]
                                                       [mod-key (if (symbol? first-sym)
                                                                    (string->symbol
                                                                     (format "~a/~a" skill-name first-sym))
                                                                    #f)]
                                                       [is-grouped (and mod-key
                                                                        (assq mod-key *module-cache*))]
                                                       [module-name (if is-grouped first-sym #f)]
                                                       [export-syms (if is-grouped
                                                                        (cdr export-group)
                                                                        export-group)])
                                                      (when module-name
                                                            (for-each
                                                             (lambda (sym)
                                                                     (when (and (symbol? sym)
                                                                                (not (hamt-lookup sym *export-module-map*)))
                                                                           (set! *export-module-map*
                                                                                 (hamt-assoc sym module-name *export-module-map*))))
                                                             export-syms)))))
                                  exports-raw))))))
   (kg-skills))

  ;; Second pass: populate export→module map from *module-paths* via exports-of
  ;; This catches exports not declared in manifests. Manifest mappings win (don't overwrite).
  (when (top-level-bound? 'exports-of)
        (let ([mod-names (hashtable-keys *module-paths*)])
             (vector-for-each
              (lambda (mod-name)
                      (let ([path (hashtable-ref *module-paths* mod-name #f)])
                           (when (and path (file-exists? path))
                                 (guard (e [else (void)])  ; skip files that fail to parse
                                   (let ([syms (exports-of path)])
                                        (for-each
                                         (lambda (sym)
                                                 (when (and (symbol? sym)
                                                            (not (hamt-lookup sym *export-module-map*)))
                                                       (set! *export-module-map*
                                                             (hamt-assoc sym mod-name *export-module-map*))))
                                         syms))))))
              mod-names)))

  ;; Index all exports (with docstrings and module info)
  (set! *export-index*
        (fold-left
         (lambda (idx export-entry)
           (let* ([export-name (car export-entry)]
                  [name-terms (export->terms export-name)]
                  [doc-terms (docstring-terms export-name)]
                  [all-terms (append name-terms doc-terms)]
                  [docstring (get-docstring export-name)]
                  [module (hamt-lookup export-name *export-module-map*)]
                  [data `((name . ,export-name)
                          ,@(if module `((module . ,module)) '())
                          ,@(if docstring `((docstring . ,docstring)) '()))])
             (bm25-add-doc idx export-name all-terms data)))
         *export-index*
         (kg-exports)))

  (set! *search-ready* #t)
  (printf "Search indices built:\n")
  (printf "  Skills:  ~a\n" (cdr (assq 'documents (bm25-stats *skill-index*))))
  (printf "  Modules: ~a\n" (cdr (assq 'documents (bm25-stats *module-index*))))
  (printf "  Exports: ~a\n" (cdr (assq 'documents (bm25-stats *export-index*))))
  'ok)

(doc ensure-indexed! 'type (-> Void))
(doc ensure-indexed! 'description "Ensure search indices are ready, build if needed")
(define (ensure-indexed!)
  (unless *search-ready*
          (when (null? (kg-skills))
                (kg-build!))
          (lattice-index!)))

(doc 'section 'search-api)

(doc lattice-find 'type (-> String Int Symbol (List SearchResult)))
(doc lattice-find 'description "Full-text search across all indexed entities. Optional: k = max results (default 10), type = 'skill|'module|'export|'all")
(define (lattice-find query . options)
  (ensure-indexed!)
  (let* ([k (if (and (pair? options) (number? (car options)))
                (car options)
                10)]
         [type (if (and (pair? options) (pair? (cdr options)))
                   (cadr options)
                   'all)]
         [query-terms (tokenize query)])
        (if (null? query-terms)
            ;; Tokenization yielded nothing (query has only special chars)
            ;; Fall back to substring matching on raw names
            (take-at-most k (lattice-find-substring (string->symbol query) k))
            (case type
                  [(skill skills)
                   (search-with-context *skill-index* query-terms k 'skill)]
                  [(module modules)
                   (search-with-context *module-index* query-terms k 'module)]
                  [(export exports)
                   (search-with-context *export-index* query-terms k 'export)]
                  [else
                   ;; Search all, merge and re-sort
                   (let* ([skill-results (search-with-context *skill-index* query-terms k 'skill)]
                          [module-results (search-with-context *module-index* query-terms k 'module)]
                          [export-results (search-with-context *export-index* query-terms k 'export)]
                          [all-results (append skill-results module-results export-results)]
                          [sorted (sort-by (lambda (a b) (> (cadr a) (cadr b))) all-results)])
                         (take-at-most k sorted))]))))

;;; search-with-context : Index (List Symbol) Int Symbol -> (List SearchResult)
;;; Search and add type context to results
(define (search-with-context idx query-terms k result-type)
  (let ([results (bm25-search idx query-terms k)])
       (map (lambda (result)
                    (let ([id (car result)]
                          [score (cdr result)]
                          [data (bm25-get-data idx (car result))])
                         (list id score result-type data)))
            results)))

;;; lattice-find-exact : Symbol -> SearchResult | #f
;;; Find exact match for a symbol
(define (lattice-find-exact sym)
  (ensure-indexed!)
  (cond
   ;; Check skills first (use kg-skill-data since block may be #f from cache)
   [(kg-skill-data sym)
    => (lambda (data)
               (list sym 1.0 'skill data))]
   ;; Check exports
   [(assq sym (kg-exports))
    => (lambda (entry)
               (let ([mod (hamt-lookup sym *export-module-map*)])
                    (list sym 1.0 'export
                          `((name . ,sym)
                            ,@(if mod `((module . ,mod)) '())))))]
   ;; Check modules
   [(find-module-by-name sym)
    => (lambda (mod-entry)
               (list (car mod-entry) 1.0 'module `((name . ,(car mod-entry)))))]
   [else #f]))

;;; lattice-export-source : Symbol -> Symbol | #f
;;; Find which skill exports a given symbol. Returns the skill name.
(define (lattice-export-source sym)
  (let loop ([skills (kg-skills)])
    (if (null? skills) #f
        (let* ([skill-name (car skills)]
               [data (kg-skill-data skill-name)]
               [exports-raw (if data
                                (let ([e (assq 'exports data)])
                                  (if e (cdr e) '()))
                                '())])
          (cond
           [(not (list? exports-raw)) (loop (cdr skills))]
           [(null? exports-raw) (loop (cdr skills))]
           ;; Flat format: (sym1 sym2 ...)
           [(symbol? (car exports-raw))
            (if (memq sym exports-raw)
                skill-name
                (loop (cdr skills)))]
           ;; Grouped or nested flat: ((module sym1 sym2) ...) or ((sym1 sym2 ...))
           [else
            (let group-loop ([groups exports-raw])
              (if (null? groups)
                  (loop (cdr skills))
                  (let ([group (car groups)])
                    (if (and (pair? group) (memq sym group))
                        skill-name
                        (group-loop (cdr groups))))))])))))

;;; lattice-find-prefix : Symbol [Int] -> (List SearchResult)
;;; Find all exports/skills/modules whose names start with prefix
(define (lattice-find-prefix prefix-sym . options)
  (ensure-indexed!)
  (let* ([k (if (pair? options) (car options) 20)]
         [prefix-str (string-downcase (symbol->string prefix-sym))]
         [prefix-len (string-length prefix-str)])
        (let* ([export-matches
                (filter-map
                 (lambda (export-entry)
                         (let* ([export-name (car export-entry)]
                                [name-str (string-downcase (symbol->string export-name))])
                               (if (and (>= (string-length name-str) prefix-len)
                                        (string=? (substring name-str 0 prefix-len) prefix-str))
                                   (let ([mod (hamt-lookup export-name *export-module-map*)])
                                        (list export-name 0.9 'export
                                              `((name . ,export-name)
                                                ,@(if mod `((module . ,mod)) '()))))
                                   #f)))
                 (kg-exports))]
               [skill-matches
                (filter-map
                 (lambda (skill-name)
                         (let ([name-str (string-downcase (symbol->string skill-name))])
                              (if (and (>= (string-length name-str) prefix-len)
                                       (string=? (substring name-str 0 prefix-len) prefix-str))
                                  (list skill-name 0.95 'skill (kg-skill-data skill-name))
                                  #f)))
                 (kg-skills))]
               [all-matches (append skill-matches export-matches)]
               [sorted (sort-by (lambda (a b) (> (cadr a) (cadr b))) all-matches)])
              (take-at-most k sorted))))

;;; lattice-find-substring : Symbol [Int] -> (List SearchResult)
;;; Find all exports/skills whose names contain the substring
(define (lattice-find-substring substr-sym . options)
  (ensure-indexed!)
  (let* ([k (if (pair? options) (car options) 20)]
         [substr-str (string-downcase (symbol->string substr-sym))])
        (let* ([export-matches
                (filter-map
                 (lambda (export-entry)
                         (let* ([export-name (car export-entry)]
                                [name-str (string-downcase (symbol->string export-name))])
                               (if (string-contains? name-str substr-str)
                                   (let ([mod (hamt-lookup export-name *export-module-map*)])
                                        (list export-name 0.8 'export
                                              `((name . ,export-name)
                                                ,@(if mod `((module . ,mod)) '()))))
                                   #f)))
                 (kg-exports))]
               [skill-matches
                (filter-map
                 (lambda (skill-name)
                         (let ([name-str (string-downcase (symbol->string skill-name))])
                              (if (string-contains? name-str substr-str)
                                  (list skill-name 0.85 'skill (kg-skill-data skill-name))
                                  #f)))
                 (kg-skills))]
               [all-matches (append skill-matches export-matches)]
               [sorted (sort-by (lambda (a b) (> (cadr a) (cadr b))) all-matches)])
              (take-at-most k sorted))))

;;; string-contains? : String String -> Bool
;;; Check if haystack contains needle
(define (string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (if (> n-len h-len)
           #f
           (let loop ([i 0])
                (cond
                 [(> (+ i n-len) h-len) #f]
                 [(string=? (substring haystack i (+ i n-len)) needle) #t]
                 [else (loop (+ i 1))])))))

;;; find-module-by-name : Symbol -> (Key . Block) | #f
;;; Uses pre-computed *module-cache* for O(N) lookup instead of O(S×M)
(define (find-module-by-name name)
  (let ([name-str (symbol->string name)])
       (find (lambda (entry)
                     (let ([key-str (symbol->string (car entry))])
                          (or (string=? key-str name-str)
                              (string-ends-with? key-str (string-append "/" name-str)))))
             *module-cache*)))

;;; string-ends-with? : String String -> Bool
(define (string-ends-with? str suffix)
  (let ([str-len (string-length str)]
        [suf-len (string-length suffix)])
       (and (>= str-len suf-len)
            (string=? (substring str (- str-len suf-len) str-len) suffix))))

;;; find helper
(define (find pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) (car lst)]
   [else (find pred (cdr lst))]))

;;; ====
;;; Autocomplete
;;; ====

;;; lattice-complete : String [Int] -> (List (Symbol . Score))
;;; Autocomplete for a prefix
(define (lattice-complete prefix . options)
  (ensure-indexed!)
  (let* ([k (if (pair? options) (car options) 10)]
         [prefix-lower (string-downcase prefix)]
         [prefix-len (string-length prefix-lower)])
        ;; Find all skills/exports that start with prefix
        (let* ([skill-matches
                (filter-map
                 (lambda (skill-name)
                         (let ([name-str (string-downcase (symbol->string skill-name))])
                              (if (and (>= (string-length name-str) prefix-len)
                                       (string=? (substring name-str 0 prefix-len) prefix-lower))
                                  (cons skill-name 0.9)  ; Skills get high score
                                  #f)))
                 (kg-skills))]
               [export-matches
                (filter-map
                 (lambda (export-entry)
                         (let* ([export-name (car export-entry)]
                                [name-str (string-downcase (symbol->string export-name))])
                               (if (and (>= (string-length name-str) prefix-len)
                                        (string=? (substring name-str 0 prefix-len) prefix-lower))
                                   (cons export-name 0.5)  ; Exports get lower score
                                   #f)))
                 (kg-exports))]
               [all-matches (append skill-matches export-matches)]
               [sorted (sort-by (lambda (a b)
                                       (or (> (cdr a) (cdr b))
                                           (and (= (cdr a) (cdr b))
                                                (< (string-length (symbol->string (car a)))
                                                   (string-length (symbol->string (car b)))))))
                               all-matches)])
              (take-at-most k sorted))))

;;; ====
;;; Filtered Search
;;; ====

;;; lattice-find-by-tier : String Int [Int] -> (List SearchResult)
;;; Search skills filtered by tier
(define (lattice-find-by-tier query tier . options)
  (let* ([k (if (pair? options) (car options) 10)]
         [results (lattice-find query k 'skill)])
        (filter
         (lambda (result)
                 (let ([data (cadddr result)])
                      (and data
                           (let ([t (assq 'tier data)])
                                (and t (= (cdr t) tier))))))
         results)))

;;; lattice-find-by-purity : String Symbol [Int] -> (List SearchResult)
;;; Search skills filtered by purity (total or partial)
(define (lattice-find-by-purity query purity . options)
  (let* ([k (if (pair? options) (car options) 10)]
         [results (lattice-find query k 'skill)])
        (filter
         (lambda (result)
                 (let ([data (cadddr result)])
                      (and data
                           (let ([p (assq 'purity data)])
                                (and p (eq? (cdr p) purity))))))
         results)))

(doc 'section 'pretty-printing)

(doc *show-scores* 'description "Whether to show BM25 scores in output (default: #f)")
(define *show-scores* #f)

(doc print-results 'type (-> (List SearchResult) Void))
(doc print-results 'description "Agent-compact search results: always shows scores, require paths, brief descriptions")
(define (print-results results)
  (if (null? results)
      (printf "No results found.\n")
      (for-each
       (lambda (result)
               (let ([id (car result)]
                     [score (cadr result)]
                     [type (caddr result)]
                     [data (cadddr result)])
                    (case type
                      [(skill)
                       (printf "~a [skill] (~a)" id (round-to score 2))
                       (when data
                             (let ([desc (assq 'description data)]
                                   [mods (assq 'modules data)])
                               (when (and desc (string? (cdr desc)))
                                     (printf " — ~a" (truncate-string (cdr desc) 60)))))
                       (printf "\n")]
                      [(export)
                       (let ([mod (and data (assq 'module data))]
                             [doc (and data (assq 'docstring data))])
                         (if mod
                             (printf "~a [export] (~a)  (require '~a)\n" id (round-to score 2) (cdr mod))
                             (printf "~a [export] (~a)\n" id (round-to score 2)))
                         (when (and doc (string? (cdr doc)) (> (string-length (cdr doc)) 0))
                               (printf "  ~a\n" (truncate-string (cdr doc) 80))))]
                      [(module)
                       (let ([skill (and data (assq 'skill data))])
                         (printf "~a [module] (~a)" id (round-to score 2))
                         (when skill (printf "  part of ~a" (cdr skill)))
                         (printf "\n"))])))
       results)))

;;; print-results-pretty : (List SearchResult) -> void
;;; Verbose human-oriented search results
(define (print-results-pretty results)
  (if (null? results)
      (printf "No results found.\n")
      (for-each
       (lambda (result)
               (let ([id (car result)]
                     [score (cadr result)]
                     [type (caddr result)]
                     [data (cadddr result)])
                    (if *show-scores*
                        (printf "~a [~a] (score: ~a)\n" id type (round-to score 3))
                        (printf "~a [~a]\n" id type))
                    (print-result-detail-pretty type id data)))
       results)))

;;; print-result-detail-pretty : Symbol × Symbol × Alist -> void
;;; Verbose detail for a search result
(define (print-result-detail-pretty type id data)
  (case type
    [(skill)
     (when data
           (let ([desc (assq 'description data)])
                (when (and desc (string? (cdr desc)) (> (string-length (cdr desc)) 0))
                      (printf "  ~a\n" (truncate-string (cdr desc) 70)))))]
    [(export)
     (when data
           (let ([mod (assq 'module data)]
                 [doc (assq 'docstring data)])
                (when mod
                      (printf "  (require '~a)\n" (cdr mod)))
                (when (and doc (string? (cdr doc)) (> (string-length (cdr doc)) 0))
                      (printf "  ~a\n" (truncate-string (cdr doc) 70)))))]
    [(module)
     (when data
           (let ([skill (assq 'skill data)])
                (when skill
                      (printf "  Part of: ~a\n" (cdr skill)))))]))

;;; round-to : Number Int -> Number
(define (round-to n places)
  (let ([factor (expt 10 places)])
       (/ (round (* n factor)) factor)))

;;; truncate-string : String Int -> String
(define (truncate-string str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

(doc 'section 'convenience-functions)

(doc lf 'type (-> String Void))
(doc lf 'description "Search the lattice. Agent-compact output with scores and require paths")
(define (lf query)
  (print-results (lattice-find query 10)))

(doc lfe 'type (-> Symbol Void))
(doc lfe 'description "Exact symbol lookup. Falls back to substring search if no exact match")
(define (lfe sym)
  (let ([result (lattice-find-exact sym)])
       (if result
           (print-results (list result))
           (let ([substring-results (lattice-find-substring sym 10)])
                (if (null? substring-results)
                    (printf "Not found: ~a\n" sym)
                    (begin
                      (printf "No exact match for ~a. Showing substring matches:\n\n" sym)
                      (print-results substring-results)))))))

;;; lfp : Symbol -> void
;;; Prefix search
(define (lfp prefix)
  (let ([results (lattice-find-prefix prefix 15)])
       (if (null? results)
           (printf "No matches for prefix: ~a\n" prefix)
           (print-results results))))

;;; lfs : Symbol -> void
;;; Substring search
(define (lfs substr)
  (let ([results (lattice-find-substring substr 15)])
       (if (null? results)
           (printf "No matches containing: ~a\n" substr)
           (print-results results))))

;;; ====
;;; Human-verbose variants (pretty printers)
;;; ====

;;; lf-pretty : String -> void
;;; Verbose search with optional scores (toggle with search-scores)
(define (lf-pretty query)
  (print-results-pretty (lattice-find query 10)))

;;; lfe-pretty : Symbol -> void
;;; Verbose exact search
(define (lfe-pretty sym)
  (let ([result (lattice-find-exact sym)])
       (if result
           (print-results-pretty (list result))
           (let ([substring-results (lattice-find-substring sym 10)])
                (if (null? substring-results)
                    (printf "Not found: ~a\n" sym)
                    (begin
                      (printf "No exact match for ~a. Showing substring matches:\n\n" sym)
                      (print-results-pretty substring-results)))))))

;;; lfp-pretty : Symbol -> void
;;; Verbose prefix search
(define (lfp-pretty prefix)
  (let ([results (lattice-find-prefix prefix 15)])
       (if (null? results)
           (printf "No matches for prefix: ~a\n" prefix)
           (print-results-pretty results))))

;;; lfs-pretty : Symbol -> void
;;; Verbose substring search
(define (lfs-pretty substr)
  (let ([results (lattice-find-substring substr 15)])
       (if (null? results)
           (printf "No matches containing: ~a\n" substr)
           (print-results-pretty results))))

;;; search-scores : Bool -> void
;;; Toggle display of BM25 scores in verbose output
(define (search-scores show?)
  (set! *show-scores* show?)
  (printf "Score display: ~a\n" (if show? "on" "off")))

(doc 'section 'structured-data-api)

(doc lattice-find-data 'type (-> String (List Alist)))
(doc lattice-find-data 'description "Search returning structured alists instead of printing. For MCP/IPC consumption.")
;;; lattice-find-data : String [Nat] [Symbol] -> (List Alist)
;;; Each result is an alist: ((id . sym) (score . 0.85) (type . export) ...)
;;; with type-specific fields: module, docstring (exports); description (skills); skill (modules)
(define (lattice-find-data query . args)
  (let* ([k (if (pair? args) (car args) 10)]
         [type (if (and (pair? args) (pair? (cdr args))) (cadr args) #f)]
         [results (if type (lattice-find query k type) (lattice-find query k))])
    (map (lambda (result)
           (let ([id (car result)]
                 [score (cadr result)]
                 [type (caddr result)]
                 [data (cadddr result)])
             `((id . ,id)
               (score . ,(round-to score 3))
               (type . ,type)
               ,@(result-data->alist type data))))
         results)))

;;; result-data->alist : Symbol Alist -> Alist
;;; Extract type-specific fields from a search result's data payload
(define (result-data->alist type data)
  (if (not data)
      '()
      (case type
        [(skill)
         (let ([desc (assq 'description data)]
               [tier (assq 'tier data)]
               [purity (assq 'purity data)]
               [modules-raw (assq 'modules data)])
           `(,@(if (and desc (string? (cdr desc)))
                   `((description . ,(cdr desc)))
                   '())
             ,@(if tier `((tier . ,(cdr tier))) '())
             ,@(if purity `((purity . ,(cdr purity))) '())
             ,@(if modules-raw
                   `((module-count . ,(length (cdr modules-raw))))
                   '())))]
        [(export)
         (let ([mod (assq 'module data)]
               [doc (assq 'docstring data)])
           `(,@(if mod `((module . ,(cdr mod))
                         (require . ,(cdr mod)))
                   '())
             ,@(if (and doc (string? (cdr doc))
                        (> (string-length (cdr doc)) 0))
                   `((docstring . ,(cdr doc)))
                   '())))]
        [(module)
         (let ([skill (assq 'skill data)])
           `(,@(if skill `((skill . ,(cdr skill))) '())))]
        [else '()])))

(doc 'section 'repl-interface)

(meta-printf "search.ss loaded.\n")
(meta-printf "  (lf \"query\")                   - Search (agent-compact)\n")
(meta-printf "  (lfe 'symbol)                  - Exact lookup\n")
(meta-printf "  (lfp 'prefix)                  - Prefix search\n")
(meta-printf "  (lfs 'substr)                  - Substring search\n")
(meta-printf "  (lf-pretty \"query\")            - Search (verbose)\n")
(meta-printf "  (lattice-find \"query\" k type)  - Programmatic search\n")
(meta-printf "  (lattice-complete \"prefix\")    - Autocomplete\n")
