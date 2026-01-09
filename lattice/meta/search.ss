;;; lattice/meta/search.ss — Lattice Search API
;;;
;;; Unified search interface for the skill lattice.
;;; Integrates BM25 with the knowledge graph for ranked results.
;;;
;;; This is Lattice code: pure (mostly), uses Core primitives.
;;;
;;; Usage:
;;;   (lattice-index!)                   ; Build search index from KG
;;;   (lattice-find "query")             ; Full-text search
;;;   (lattice-find-exact 'symbol)       ; Exact symbol match
;;;   (lattice-complete "prefix")        ; Autocomplete
;;;
;;; Dependencies:
;;;   lattice/meta/kg.ss
;;;   lattice/meta/bm25.ss

(load "lattice/meta/kg.ss")
(load "lattice/meta/bm25.ss")

;;; ============================================================
;;; Search Index State
;;; ============================================================

;;; Mutable search indices (built from KG)
(define *skill-index* (bm25-create))
(define *module-index* (bm25-create))
(define *export-index* (bm25-create))
(define *module-cache* '())  ; Pre-computed list of all modules for O(1) lookup
(define *search-ready* #f)

;;; ============================================================
;;; Index Building
;;; ============================================================

;;; lattice-index! : -> void
;;; Build search indices from the knowledge graph
;;; Requires kg-build! to have been called first
(define (lattice-index!)
  (printf "Building search indices...\n")
  ;; Reset indices
  (set! *skill-index* (bm25-create))
  (set! *module-index* (bm25-create))
  (set! *export-index* (bm25-create))
  (set! *module-cache* '())
  
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
  
  ;; Index all exports
  (for-each
   (lambda (export-entry)
           (let* ([export-name (car export-entry)]
                  [terms (export->terms export-name)]
                  [data `((name . ,export-name))])
                 (set! *export-index*
                       (bm25-add-doc *export-index* export-name terms data))))
   (kg-exports))
  
  (set! *search-ready* #t)
  (printf "Search indices built:\n")
  (printf "  Skills:  ~a\n" (cdr (assq 'documents (bm25-stats *skill-index*))))
  (printf "  Modules: ~a\n" (cdr (assq 'documents (bm25-stats *module-index*))))
  (printf "  Exports: ~a\n" (cdr (assq 'documents (bm25-stats *export-index*))))
  'ok)

;;; ensure-indexed! : -> void
;;; Ensure search indices are ready, build if needed
(define (ensure-indexed!)
  (unless *search-ready*
          (when (null? (kg-skills))
                (kg-build!))
          (lattice-index!)))

;;; ============================================================
;;; Search API
;;; ============================================================

;;; lattice-find : String [Int] [Symbol] -> (List SearchResult)
;;; Full-text search across all indexed entities
;;; Optional: k = max results (default 10), type = 'skill|'module|'export|'all
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
                          [sorted (sort (lambda (a b) (> (cadr a) (cadr b))) all-results)])
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
   ;; Check skills first
   [(kg-skill sym)
    => (lambda (block)
               (list sym 1.0 'skill (kg-skill-data sym)))]
   ;; Check exports
   [(assq sym (kg-exports))
    => (lambda (entry)
               (list sym 1.0 'export `((name . ,sym))))]
   ;; Check modules
   [(find-module-by-name sym)
    => (lambda (mod-entry)
               (list (car mod-entry) 1.0 'module `((name . ,(car mod-entry)))))]
   [else #f]))

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

;;; ============================================================
;;; Autocomplete
;;; ============================================================

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
               [sorted (sort (lambda (a b)
                                     (or (> (cdr a) (cdr b))
                                         (and (= (cdr a) (cdr b))
                                              (< (string-length (symbol->string (car a)))
                                                 (string-length (symbol->string (car b)))))))
                             all-matches)])
              (take-at-most k sorted))))

;;; ============================================================
;;; Filtered Search
;;; ============================================================

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

;;; ============================================================
;;; Pretty Printing
;;; ============================================================

;;; print-results : (List SearchResult) -> void
;;; Pretty print search results
(define (print-results results)
  (if (null? results)
      (printf "No results found.\n")
      (for-each
       (lambda (result)
               (let ([id (car result)]
                     [score (cadr result)]
                     [type (caddr result)]
                     [data (cadddr result)])
                    (printf "~a [~a] (score: ~a)\n" id type (round-to score 3))
                    (when (and data (assq 'description data))
                          (let ([desc (cdr (assq 'description data))])
                               (when (and (string? desc) (> (string-length desc) 0))
                                     (printf "  ~a\n" (truncate-string desc 70)))))))
       results)))

;;; round-to : Number Int -> Number
(define (round-to n places)
  (let ([factor (expt 10 places)])
       (/ (round (* n factor)) factor)))

;;; truncate-string : String Int -> String
(define (truncate-string str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; lf : String -> void
;;; Quick search with pretty output (for REPL)
(define (lf query)
  (print-results (lattice-find query 10)))

;;; lfe : Symbol -> void
;;; Quick exact search with pretty output
(define (lfe sym)
  (let ([result (lattice-find-exact sym)])
       (if result
           (print-results (list result))
           (printf "Not found: ~a\n" sym))))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "search.ss loaded.\n")
(printf "  (lattice-index!)               - Build search indices\n")
(printf "  (lattice-find \"query\")         - Full-text search\n")
(printf "  (lattice-find-exact 'symbol)   - Exact match\n")
(printf "  (lattice-complete \"prefix\")    - Autocomplete\n")
(printf "  (lf \"query\")                   - Quick search\n")
(printf "  (lfe 'symbol)                  - Quick exact search\n")
