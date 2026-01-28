(load "core/base/prelude.ss")

(doc 'module 'bm25)
(doc 'description "Pure functional BM25 implementation for lattice search. Provides term-frequency/inverse-document-frequency ranking with document length normalization.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "BM25 Formula: score(D, Q) = Σ IDF(qi) × (f(qi, D) × (k1 + 1)) / (f(qi, D) + k1 × (1 - b + b × |D|/avgdl))")

(doc 'section 'parameters)

(define BM25-K1 1.2)
(doc BM25-K1 'description "Term frequency saturation parameter")

(define BM25-B 0.75)
(doc BM25-B 'description "Length normalization factor")

(doc 'section 'index-structure)

(doc 'note "BM25 index is an alist with: doc-count, total-length, term-doc-freq, doc-term-freq, doc-lengths, doc-data")

(doc bm25-create 'type (-> Index))
(doc bm25-create 'description "Create an empty BM25 index")
(define (bm25-create)
  '((doc-count . 0)
    (total-length . 0)
    (term-doc-freq . ())
    (doc-term-freq . ())
    (doc-lengths . ())
    (doc-data . ())))

(doc 'section 'accessors)

(define (bm25-doc-count idx)
  (doc 'type (-> Index Nat))
  (cdr (assq 'doc-count idx)))

(define (bm25-total-length idx)
  (cdr (assq 'total-length idx)))

(define (bm25-term-doc-freq idx)
  (cdr (assq 'term-doc-freq idx)))

(define (bm25-doc-term-freq idx)
  (cdr (assq 'doc-term-freq idx)))

(define (bm25-doc-lengths idx)
  (cdr (assq 'doc-lengths idx)))

(define (bm25-doc-data idx)
  (cdr (assq 'doc-data idx)))

(define (bm25-avg-doc-length idx)
  (let ([count (bm25-doc-count idx)]
        [total (bm25-total-length idx)])
       (if (= count 0)
           0
           (/ total count))))

(doc 'section 'term-operations)

(doc tokenize 'type (-> String (List Symbol)))
(doc tokenize 'description "Tokenize a string into lowercase word symbols. Hyphenated symbols are split into parts: c2d-zoh -> [c2d-zoh, c2d, zoh]")
(define (tokenize str)
  (if (not (string? str))
      '()
      (let* ([words (string-split-words (string-downcase str))]
             [base-tokens (filter-map
                           (lambda (w)
                                   (if (> (string-length w) 1)
                                       (string->symbol w)
                                       #f))
                           words)]
             ;; Also split hyphenated tokens into parts
             [split-tokens (append-map (lambda (sym)
                                               (split-hyphenated sym))
                                       base-tokens)])
            (remove-duplicates (append base-tokens split-tokens)))))

;;; split-hyphenated : Symbol -> (List Symbol)
;;; Split a hyphenated symbol into its parts
;;; c2d-zoh -> [c2d, zoh], lqr -> []
(define (split-hyphenated sym)
  (let* ([str (symbol->string sym)]
         [parts (string-split-on-char str #\-)])
        (if (> (length parts) 1)
            (filter-map
             (lambda (p)
                     (if (> (string-length p) 1)
                         (string->symbol p)
                         #f))
             parts)
            '())))

;;; string-split-on-char : String Char -> (List String)
(define (string-split-on-char str ch)
  (let loop ([chars (string->list str)]
             [current '()]
             [parts '()])
       (cond
        [(null? chars)
         (if (null? current)
             (reverse parts)
             (reverse (cons (list->string (reverse current)) parts)))]
        [(char=? (car chars) ch)
         (if (null? current)
             (loop (cdr chars) '() parts)
             (loop (cdr chars) '() (cons (list->string (reverse current)) parts)))]
        [else
         (loop (cdr chars) (cons (car chars) current) parts)])))

;; remove-duplicates is provided by prelude (aliased to unique)

;;; string-split-words : String -> (List String)
;;; Split string on whitespace and punctuation
(define (string-split-words str)
  (let loop ([chars (string->list str)]
             [current '()]
             [words '()])
       (cond
        [(null? chars)
         (if (null? current)
             (reverse words)
             (reverse (cons (list->string (reverse current)) words)))]
        [(word-char? (car chars))
         (loop (cdr chars) (cons (car chars) current) words)]
        [(null? current)
         (loop (cdr chars) '() words)]
        [else
         (loop (cdr chars) '() (cons (list->string (reverse current)) words))])))

;;; word-char? : Char -> Bool
(define (word-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (char=? c #\-)
      (char=? c #\_)))

;;; string-downcase : String -> String
(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

;;; count-terms : (List Symbol) -> ((Symbol . Count) ...)
;;; Count term frequencies in a list
(define (count-terms terms)
  (let loop ([terms terms] [counts '()])
       (if (null? terms)
           counts
           (let* ([term (car terms)]
                  [entry (assq term counts)])
                 (if entry
                     (loop (cdr terms)
                           (cons (cons term (+ 1 (cdr entry)))
                                 (remove-assq term counts)))
                     (loop (cdr terms)
                           (cons (cons term 1) counts)))))))

;;; remove-assq : Key Alist -> Alist
(define (remove-assq key alist)
  (filter (lambda (entry) (not (eq? (car entry) key))) alist))

;;; filter-map provided by prelude

(doc 'section 'index-building)

(doc bm25-add-doc 'type (-> Index DocId (List Symbol) Any Index))
(doc bm25-add-doc 'description "Add a document to the index. Returns new index (functional update)")
(define (bm25-add-doc idx doc-id terms data)
  (let* ([term-counts (count-terms terms)]
         [doc-length (length terms)]
         [unique-terms (map car term-counts)]
         ;; Update term-doc-freq (increment for each unique term)
         [old-tdf (bm25-term-doc-freq idx)]
         [new-tdf (fold-left
                   (lambda (tdf term)
                           (let ([entry (assq term tdf)])
                                (if entry
                                    (cons (cons term (+ 1 (cdr entry)))
                                          (remove-assq term tdf))
                                    (cons (cons term 1) tdf))))
                   old-tdf
                   unique-terms)]
         ;; Update doc-term-freq
         [old-dtf (bm25-doc-term-freq idx)]
         [new-dtf (cons (cons doc-id term-counts) old-dtf)]
         ;; Update doc-lengths
         [old-dl (bm25-doc-lengths idx)]
         [new-dl (cons (cons doc-id doc-length) old-dl)]
         ;; Update doc-data
         [old-dd (bm25-doc-data idx)]
         [new-dd (cons (cons doc-id data) old-dd)])
        `((doc-count . ,(+ 1 (bm25-doc-count idx)))
          (total-length . ,(+ doc-length (bm25-total-length idx)))
          (term-doc-freq . ,new-tdf)
          (doc-term-freq . ,new-dtf)
          (doc-lengths . ,new-dl)
          (doc-data . ,new-dd))))

(doc bm25-add-doc! 'type (-> Index DocId (List Symbol) Any Index))
(doc bm25-add-doc! 'description "Alias for bm25-add-doc (pure, returns new index)")
(define bm25-add-doc! bm25-add-doc)

(doc 'section 'scoring)

(doc bm25-idf 'type (-> Index Symbol Number))
(doc bm25-idf 'description "Compute inverse document frequency for a term")
(define (bm25-idf idx term)
  (let* ([N (bm25-doc-count idx)]
         [tdf (bm25-term-doc-freq idx)]
         [entry (assq term tdf)]
         [n (if entry (cdr entry) 0)])
        (log (+ 1 (/ (+ (- N n) 0.5)
                     (+ n 0.5))))))

(doc bm25-term-score 'type (-> Index DocId Symbol Number Number Number))
(doc bm25-term-score 'description "Score a single term for a document")
(define (bm25-term-score idx doc-id term doc-length avgdl)
  (let* ([dtf (bm25-doc-term-freq idx)]
         [doc-entry (assq doc-id dtf)]
         [term-freqs (if doc-entry (cdr doc-entry) '())]
         [freq-entry (assq term term-freqs)]
         [f (if freq-entry (cdr freq-entry) 0)]
         [idf (bm25-idf idx term)]
         [numerator (* f (+ BM25-K1 1))]
         [denominator (+ f (* BM25-K1
                              (+ (- 1 BM25-B)
                                 (* BM25-B (/ doc-length avgdl)))))])
        (if (= denominator 0)
            0
            (* idf (/ numerator denominator)))))

(doc bm25-score 'type (-> Index DocId (List Symbol) Number))
(doc bm25-score 'description "Compute BM25 score for a document given query terms")
(define (bm25-score idx doc-id query-terms)
  (let* ([dl (bm25-doc-lengths idx)]
         [dl-entry (assq doc-id dl)]
         [doc-length (if dl-entry (cdr dl-entry) 0)]
         [avgdl (bm25-avg-doc-length idx)])
        (if (= doc-length 0)
            0
            (fold-left
             (lambda (score term)
                     (+ score (bm25-term-score idx doc-id term doc-length avgdl)))
             0
             query-terms))))

(doc 'section 'search)

(doc bm25-search 'type (-> Index (List Symbol) Int (List (Pair DocId Score))))
(doc bm25-search 'description "Search for documents matching query terms, return top-k results")
(define (bm25-search idx query-terms k)
  (let* ([all-docs (map car (bm25-doc-lengths idx))]
         [scored (map (lambda (doc-id)
                              (cons doc-id (bm25-score idx doc-id query-terms)))
                      all-docs)]
         [filtered (filter (lambda (entry) (> (cdr entry) 0)) scored)]
         [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) filtered)])
        (take-at-most k sorted)))

(doc bm25-search-string 'type (-> Index String Int (List (Pair DocId Score))))
(doc bm25-search-string 'description "Search with a string query (tokenizes automatically)")
(define (bm25-search-string idx query k)
  (bm25-search idx (tokenize query) k))

;;; take-at-most : Int (List a) -> (List a)
(define (take-at-most n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-at-most (- n 1) (cdr lst)))))

(doc 'section 'result-helpers)

(doc bm25-get-data 'type (-> Index DocId (Maybe Any)))
(doc bm25-get-data 'description "Get associated data for a document")
(define (bm25-get-data idx doc-id)
  (let ([entry (assq doc-id (bm25-doc-data idx))])
       (if entry (cdr entry) #f)))

(doc bm25-results-with-data 'type (-> Index (List (Pair DocId Score)) (List (List DocId Score Data))))
(doc bm25-results-with-data 'description "Augment search results with associated data")
(define (bm25-results-with-data idx results)
  (map (lambda (result)
               (let ([doc-id (car result)]
                     [score (cdr result)])
                    (list doc-id score (bm25-get-data idx doc-id))))
       results))

(doc 'section 'statistics)

(doc bm25-stats 'type (-> Index Alist))
(doc bm25-stats 'description "Get index statistics")
(define (bm25-stats idx)
  `((documents . ,(bm25-doc-count idx))
    (total-terms . ,(bm25-total-length idx))
    (unique-terms . ,(length (bm25-term-doc-freq idx)))
    (avg-doc-length . ,(bm25-avg-doc-length idx))))

(doc 'section 'lattice-integration)

(doc skill->terms 'type (-> ManifestData (List Symbol)))
(doc skill->terms 'description "Extract searchable terms from skill manifest data")
(define (skill->terms manifest-data)
  (if (not manifest-data)
      '()
      (let ([name (cdr (or (assq 'name manifest-data) '(name . unknown)))]
            [description (cdr (or (assq 'description manifest-data) '(description . "")))]
            [keywords (cdr (or (assq 'keywords manifest-data) '(keywords . ())))]
            [aliases (cdr (or (assq 'aliases manifest-data) '(aliases . ())))])
           (append
            ;; Name tokens (weighted by repetition)
            (let ([name-tokens (tokenize (symbol->string name))])
                 (append name-tokens name-tokens name-tokens))  ; 3x weight
            ;; Description tokens
            (tokenize description)
            ;; Keywords (already symbols)
            (if (list? keywords) keywords '())
            ;; Aliases (already symbols)
            (if (list? aliases) aliases '())))))

(doc module->terms 'type (-> ModuleData (List Symbol)))
(doc module->terms 'description "Extract searchable terms from module data")
(define (module->terms mod-data)
  (if (not mod-data)
      '()
      (let ([name (cdr (or (assq 'name mod-data) '(name . unknown)))]
            [description (cdr (or (assq 'description mod-data) '(description . "")))])
           (append
            (tokenize (symbol->string name))
            (tokenize description)))))

(doc export->terms 'type (-> Symbol (List Symbol)))
(doc export->terms 'description "Extract searchable terms from export symbol")
(define (export->terms sym)
  (if (symbol? sym)
      (tokenize (symbol->string sym))
      '()))

(doc 'section 'repl-interface)

(meta-printf "bm25.ss loaded.\n")
(meta-printf "  (bm25-create)                  - Create empty index\n")
(meta-printf "  (bm25-add-doc idx id terms d)  - Add document\n")
(meta-printf "  (bm25-search idx terms k)      - Top-k search\n")
(meta-printf "  (bm25-search-string idx q k)   - Search with string\n")
(meta-printf "  (bm25-score idx id terms)      - Score document\n")
(meta-printf "  (bm25-stats idx)               - Index statistics\n")
