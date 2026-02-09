(load "lattice/data/sort.ss")
(load "lattice/meta/kg.ss")
(load "lattice/meta/bm25.ss")
(load "lattice/meta/docstrings.ss")

(doc 'module 'search)
(doc 'description "Unified search interface for the skill lattice. Integrates BM25 with the knowledge graph for ranked results.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'state)

(doc *skill-index* 'description "Mutable BM25 index for skills")
(define *skill-index* (bm25-create))

(doc *module-index* 'description "Mutable BM25 index for modules")
(define *module-index* (bm25-create))

(doc *export-index* 'description "Mutable BM25 index for exports")
(define *export-index* (bm25-create))

(doc *module-cache* 'description "Pre-computed list of all modules for O(1) lookup")
(define *module-cache* '())

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

  ;; Build docstring cache only if not already populated from cache
  (when (zero? (hashtable-size *docstrings*))
        (build-docstring-cache!))

  ;; Index all skills
  (for-each
   (lambda (skill-name)
           (let ([manifest-data (kg-skill-data skill-name)])
                (when manifest-data
                      (let ([terms (skill->terms manifest-data)])
                           (set! *skill-index*
                                 (bm25-add-doc *skill-index* skill-name terms manifest-data))))))
   (kg-skills))

  ;; Index all modules and build module cache
  (for-each
   (lambda (skill-name)
           (let ([modules (kg-modules skill-name)])
                (for-each
                 (lambda (mod-entry)
                         (let* ([mod-key (car mod-entry)]
                                [mod-data `((name . ,mod-key)
                                            (skill . ,skill-name))])
                               ;; Add to module cache for fast lookup
                               (set! *module-cache* (cons mod-entry *module-cache*))
                               ;; Extract module name from key (e.g., 'linalg/vec -> 'vec)
                               (let* ([key-str (symbol->string mod-key)]
                                      [terms (tokenize key-str)])
                                     (set! *module-index*
                                           (bm25-add-doc *module-index* mod-key terms mod-data)))))
                 modules)))
   (kg-skills))

  ;; Index all exports (with docstrings)
  (for-each
   (lambda (export-entry)
           (let* ([export-name (car export-entry)]
                  [name-terms (export->terms export-name)]
                  [doc-terms (docstring-terms export-name)]
                  [all-terms (append name-terms doc-terms)]
                  [docstring (get-docstring export-name)]
                  [data `((name . ,export-name)
                          ,@(if docstring `((docstring . ,docstring)) '()))])
                 (set! *export-index*
                       (bm25-add-doc *export-index* export-name all-terms data))))
   (kg-exports))

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
            '()
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
               (list sym 1.0 'export `((name . ,sym))))]
   ;; Check modules
   [(find-module-by-name sym)
    => (lambda (mod-entry)
               (list (car mod-entry) 1.0 'module `((name . ,(car mod-entry)))))]
   [else #f]))

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
                                   (list export-name 0.9 'export `((name . ,export-name)))
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
                                   (list export-name 0.8 'export `((name . ,export-name)))
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
(doc print-results 'description "Pretty print search results with clean formatting")
(define (print-results results)
  (if (null? results)
      (printf "No results found.\n")
      (for-each
       (lambda (result)
               (let ([id (car result)]
                     [score (cadr result)]
                     [type (caddr result)]
                     [data (cadddr result)])
                    ;; Format: name [type] (score: N.N) - only show score if enabled
                    (if *show-scores*
                        (printf "~a [~a] (score: ~a)\n" id type (round-to score 3))
                        (printf "~a [~a]\n" id type))
                    ;; Show description/docstring
                    (print-result-detail type id data)))
       results)))

;;; print-result-detail : Symbol × Symbol × Alist -> void
;;; Print details for a search result based on its type
(define (print-result-detail type id data)
  (case type
    [(skill)
     ;; For skills, show description
     (when data
           (let ([desc (assq 'description data)])
                (when (and desc (string? (cdr desc)) (> (string-length (cdr desc)) 0))
                      (printf "  ~a\n" (truncate-string (cdr desc) 70)))))]
    [(export)
     ;; For exports, show docstring (first line only)
     (when data
           (let ([doc (assq 'docstring data)])
                (when (and doc (string? (cdr doc)) (> (string-length (cdr doc)) 0))
                      (printf "  ~a\n" (truncate-string (cdr doc) 70)))))]
    [(module)
     ;; For modules, show the module info
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
(doc lf 'description "Quick search with pretty output (for REPL)")
(define (lf query)
  (print-results (lattice-find query 10)))

(doc lfe 'type (-> Symbol Void))
(doc lfe 'description "Quick exact search with pretty output. Falls back to substring search if no exact match found")
(define (lfe sym)
  (let ([result (lattice-find-exact sym)])
       (if result
           (print-results (list result))
           ;; Fallback to substring search
           (let ([substring-results (lattice-find-substring sym 10)])
                (if (null? substring-results)
                    (printf "Not found: ~a\n" sym)
                    (begin
                      (printf "No exact match for ~a. Showing substring matches:\n\n" sym)
                      (print-results substring-results)))))))

;;; lfp : Symbol -> void
;;; Quick prefix search with pretty output
(define (lfp prefix)
  (let ([results (lattice-find-prefix prefix 15)])
       (if (null? results)
           (printf "No matches for prefix: ~a\n" prefix)
           (print-results results))))

;;; lfs : Symbol -> void
;;; Quick substring search with pretty output
(define (lfs substr)
  (let ([results (lattice-find-substring substr 15)])
       (if (null? results)
           (printf "No matches containing: ~a\n" substr)
           (print-results results))))

;;; search-scores : Bool -> void
;;; Toggle display of BM25 scores in search results
(define (search-scores show?)
  (set! *show-scores* show?)
  (printf "Score display: ~a\n" (if show? "on" "off")))

(doc 'section 'repl-interface)

(meta-printf "search.ss loaded.\n")
(meta-printf "  (lattice-index!)               - Build search indices\n")
(meta-printf "  (lattice-find \"query\")         - Full-text search\n")
(meta-printf "  (lattice-find-exact 'symbol)   - Exact match\n")
(meta-printf "  (lattice-complete \"prefix\")    - Autocomplete\n")
(meta-printf "  (lf \"query\")                   - Quick search\n")
(meta-printf "  (lfe 'symbol)                  - Quick exact search\n")
(meta-printf "  (lfp 'prefix)                  - Prefix search\n")
(meta-printf "  (lfs 'substr)                  - Substring search\n")
