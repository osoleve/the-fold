;;; lattice/meta/kg-ingest.ss — KG Ingestion Pipeline
;;;
;;; Natural language triple extraction and KG ingestion.
;;; Parses simple fact sentences into normalized (subject predicate object)
;;; triples and upserts them into a nested alist store.

(unless (top-level-bound? 'filter-map) (load "core/base/prelude.ss"))

(doc 'module 'kg-ingest)
(doc 'description "Natural language triple extraction and KG ingestion pipeline.
Parses simple fact sentences into normalized (subject predicate object) triples
and upserts them into a nested alist store.")
(doc 'layer 'lattice)
(doc 'purity 'pure)

;;; ====
;;; Internal Helpers
;;; ====

(define (tokenize-simple x)
  (let* ([s (cond [(string? x) x]
                  [(symbol? x) (symbol->string x)]
                  [else ""])]
         [len (string-length s)])
    (let loop ([i 0] [cur '()] [acc '()])
      (if (= i len)
          (let ([acc2 (if (null? cur)
                          acc
                          (cons (list->string (reverse cur)) acc))])
            (reverse acc2))
          (let ([ch (char-downcase (string-ref s i))])
            (if (or (char-alphabetic? ch) (char-numeric? ch))
                (loop (+ i 1) (cons ch cur) acc)
                (if (null? cur)
                    (loop (+ i 1) '() acc)
                    (loop (+ i 1)
                          '()
                          (cons (list->string (reverse cur)) acc)))))))))

(define (list-prefix? prefix xs)
  (cond
    [(null? prefix) #t]
    [(null? xs) #f]
    [else
     (and (equal? (car prefix) (car xs))
          (list-prefix? (cdr prefix) (cdr xs)))]))

(define (drop n xs)
  (if (or (<= n 0) (null? xs))
      xs
      (drop (- n 1) (cdr xs))))

(define (split-on-pattern xs pat)
  (let loop ([rest xs] [prefix '()])
    (cond
      [(list-prefix? pat rest)
       (cons (reverse prefix)
             (drop (length pat) rest))]
      [(null? rest) #f]
      [else (loop (cdr rest) (cons (car rest) prefix))])))

(define (contains-equal? x xs)
  (cond
    [(null? xs) #f]
    [(equal? x (car xs)) #t]
    [else (contains-equal? x (cdr xs))]))

(define (alist-set alist key value)
  (let loop ([xs alist] [acc '()] [done #f])
    (cond
      [(null? xs)
       (let ([base (reverse acc)])
         (if done
             base
             (append base (list (cons key value)))))]
      [(and (pair? (car xs)) (eq? (caar xs) key))
       (loop (cdr xs) (cons (cons key value) acc) #t)]
      [else
       (loop (cdr xs) (cons (car xs) acc) done)])))

;;; ====
;;; Entity Normalization
;;; ====

(define (kg-normalize-entity x)
  (doc 'type (-> (U String Symbol) Symbol))
  (doc 'description "Normalize an entity name to a canonical symbol.
Lowercases, strips non-alphanumeric chars, joins tokens with hyphens.")
  (let ([tokens (tokenize-simple x)])
    (if (null? tokens)
        'unknown
        (string->symbol (string-join tokens "-")))))

(define (kg-normalize-relation rel)
  (doc 'type (-> (U String Symbol) Symbol))
  (doc 'description "Normalize a relation string to a canonical symbol.
Maps known aliases (works at, employed by, etc.) to standard relation symbols.
Unknown relations get a rel_ prefix.")
  (let ([tokens (tokenize-simple rel)])
    (cond
      [(or (equal? tokens '("works" "at"))
           (equal? tokens '("employed" "by"))
           (equal? tokens '("employee" "of")))
       'works_at]
      [(or (equal? tokens '("lives" "in"))
           (equal? tokens '("located" "in"))
           (equal? tokens '("in")))
       'located_in]
      [(or (equal? tokens '("is" "part" "of"))
           (equal? tokens '("part" "of"))
           (equal? tokens '("member" "of")))
       'part_of]
      [(equal? tokens '("founded")) 'founded]
      [(null? tokens) 'unknown_relation]
      [else
       (string->symbol
        (string-append
         "rel_"
         (symbol->string (kg-normalize-entity (string-join tokens "-")))))])))

;;; ====
;;; Triple Construction & Validation
;;; ====

(define (kg-make-triple subject relation object)
  (doc 'type (-> (U String Symbol) (U String Symbol) (U String Symbol) Triple))
  (doc 'description "Construct a normalized triple from raw subject, relation, and object.")
  (list (kg-normalize-entity subject)
        (kg-normalize-relation relation)
        (kg-normalize-entity object)))

(define (kg-valid-triple? triple)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a triple is well-formed: 3 symbols, no unknowns.")
  (and (list? triple)
       (= (length triple) 3)
       (let ([s (car triple)]
             [p (cadr triple)]
             [o (caddr triple)])
         (and (symbol? s)
              (symbol? p)
              (symbol? o)
              (not (eq? s 'unknown))
              (not (eq? o 'unknown))
              (not (eq? p 'unknown_relation))))))

;;; ====
;;; Sentence Parsing
;;; ====

(define (kg-parse-simple-fact sentence)
  (doc 'type (-> String (U Triple #f)))
  (doc 'description "Parse a simple English fact sentence into a triple.
Recognizes patterns: works at, employed by, lives in, is part of,
part of, founded. Returns #f if no pattern matches.")
  (let* ([tokens (tokenize-simple sentence)]
         [split
          (or (let ([x (split-on-pattern tokens '("works" "at"))])
                (and x (cons x "works at")))
              (let ([x (split-on-pattern tokens '("employed" "by"))])
                (and x (cons x "works at")))
              (let ([x (split-on-pattern tokens '("lives" "in"))])
                (and x (cons x "lives in")))
              (let ([x (split-on-pattern tokens '("is" "part" "of"))])
                (and x (cons x "part of")))
              (let ([x (split-on-pattern tokens '("part" "of"))])
                (and x (cons x "part of")))
              (let ([x (split-on-pattern tokens '("founded"))])
                (and x (cons x "founded"))))])
    (if (not split)
        #f
        (let* ([parts (car split)]
               [rel (cdr split)]
               [subject-tokens (car parts)]
               [object-tokens (cdr parts)])
          (if (or (null? subject-tokens)
                  (null? object-tokens))
              #f
              (kg-make-triple (string-join subject-tokens " ")
                              rel
                              (string-join object-tokens " ")))))))

;;; ====
;;; Batch Extraction
;;; ====

(define (kg-extract-triples sentences)
  (doc 'type (-> (List String) (List Triple)))
  (doc 'description "Extract deduplicated valid triples from a list of sentences.")
  (let loop ([rest sentences] [acc '()])
    (if (null? rest)
        (reverse acc)
        (let ([triple (kg-parse-simple-fact (car rest))])
          (loop (cdr rest)
                (if (and triple
                         (kg-valid-triple? triple)
                         (not (contains-equal? triple acc)))
                    (cons triple acc)
                    acc))))))

;;; ====
;;; Store Operations
;;; ====

(define (kg-upsert-triple store triple)
  (doc 'type (-> Store Triple Store))
  (doc 'description "Upsert a single triple into a nested alist store.
Store shape: ((subject (predicate obj1 obj2 ...) ...) ...).
Deduplicates objects under the same subject+predicate.")
  (if (not (kg-valid-triple? triple))
      store
      (let* ([s (car triple)]
             [p (cadr triple)]
             [o (caddr triple)]
             [subject-row (assq s store)]
             [preds (if subject-row (cdr subject-row) '())]
             [pred-row (assq p preds)]
             [objs (if pred-row (cdr pred-row) '())]
             [objs2 (if (memq o objs) objs (append objs (list o)))]
             [preds2 (alist-set preds p objs2)])
        (alist-set store s preds2))))

(define (kg-upsert-triples store triples)
  (doc 'type (-> Store (List Triple) Store))
  (doc 'description "Upsert multiple triples into a store, folding left.")
  (fold-left
   (lambda (acc triple)
     (kg-upsert-triple acc triple))
   store
   triples))
