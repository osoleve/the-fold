(load "core/base/prelude.ss")

(doc 'module 'concept-map)
(doc 'description "Extracts the ontology/concept structure from the codebase")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Analyzes definitions, types, and relationships between concepts")

(doc 'section 's-expression-file-reader)

(define (read-sexps-from-file path)
  (doc 'type "String -> (List Sexpr)")
  (doc 'description "Read all S-expressions from a file")
  (if (not (file-exists? path))
      '()
      (call-with-input-file path
                            (lambda (port)
                                    (let loop ([acc '()])
                                         (let ([expr (read port)])
                                              (if (eof-object? expr)
                                                  (reverse acc)
                                                  (loop (cons expr acc)))))))))

;;; read-file-lines : String -> (List String)
;;; Read all lines from a file.
(define (read-file-lines path)
  (if (not (file-exists? path))
      '()
      (call-with-input-file path
                            (lambda (port)
                                    (let loop ([acc '()])
                                         (let ([line (get-line port)])
                                              (if (eof-object? line)
                                                  (reverse acc)
                                                  (loop (cons line acc)))))))))

;;; ====
;;; Definition Extraction
;;; ====

;;; extract-definitions : String -> (List Definition)
;;; Extract all define/define-record-type forms from a file.
;;; Returns list of:
;;;   ((type . define|record|syntax)
;;;    (name . <symbol>)
;;;    (fields . <list>)      ; for records
;;;    (params . <list>)      ; for functions
;;;    (docstring . <string>) ; if present
;;;    (line . <number>))
(define (extract-definitions path)
  (let* ([sexps (read-sexps-from-file path)]
         [lines (read-file-lines path)])
        (let loop ([exprs sexps] [defs '()])
             (if (null? exprs)
                 (reverse defs)
                 (let ([expr (car exprs)])
                      (loop (cdr exprs)
                            (cond
                             ;; define-record-type
                             [(and (pair? expr)
                                   (eq? (car expr) 'define-record-type))
                              (cons (extract-record-def expr) defs)]
                             ;; define (function or value)
                             [(and (pair? expr) (eq? (car expr) 'define))
                              (cons (extract-define expr) defs)]
                             ;; define-syntax
                             [(and (pair? expr) (eq? (car expr) 'define-syntax))
                              (cons (extract-syntax-def expr) defs)]
                             [else defs])))))))

;;; extract-record-def : Sexpr -> Definition
;;; Extract record definition details.
(define (extract-record-def expr)
  ;; (define-record-type name (fields ...))
  (let* ([name (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
                   (cadr expr)
                   'unknown)]
         [fields-clause (find-clause 'fields (cddr expr))]
         [field-names (if fields-clause
                          (map extract-field-name (cdr fields-clause))
                          '())])
        `((type . record)
          (name . ,name)
          (fields . ,field-names))))

;;; extract-field-name : Sexpr -> Symbol
;;; Extract field name from field specification.
(define (extract-field-name field-spec)
  (cond
   [(symbol? field-spec) field-spec]
   [(and (pair? field-spec) (symbol? (car field-spec))) (car field-spec)]
   [else 'unknown-field]))

;;; find-clause : Symbol x (List Sexpr) -> Sexpr | #f
;;; Find a clause with the given head in a list.
(define (find-clause head clauses)
  (cond
   [(null? clauses) #f]
   [(and (pair? (car clauses))
         (eq? (caar clauses) head))
    (car clauses)]
   [else (find-clause head (cdr clauses))]))

;;; extract-define : Sexpr -> Definition
;;; Extract define form details.
(define (extract-define expr)
  ;; (define name value) or (define (name args...) body...)
  (let* ([form (cadr expr)]
         [name (if (pair? form) (car form) form)]
         [params (if (pair? form) (cdr form) '())]
         [is-function? (pair? form)])
        `((type . ,(if is-function? 'function 'value))
          (name . ,name)
          (params . ,params))))

;;; extract-syntax-def : Sexpr -> Definition
;;; Extract define-syntax form details.
(define (extract-syntax-def expr)
  (let ([name (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
                  (cadr expr)
                  'unknown)])
       `((type . syntax)
         (name . ,name))))

;;; ====
;;; Concept Extraction
;;; ====

;;; known-concepts : (List Symbol)
;;; Capitalized concepts we recognize as significant.
(define known-concepts
  '(Block Type Hash Bytes Bytevector Symbol String
    Nat Int Bool Char Unit Void Ref List Vector
    Capability FS Result Error OK
    Forum Channel Post Head Pin
    Core Shell Prelude Tier Session))

;;; extract-concepts : String -> (List Concept)
;;; Find concept-related patterns in a file.
;;; Returns list of:
;;;   ((name . <symbol>)
;;;    (type . record|base|compound)
;;;    (occurrences . <number>)
;;;    (contexts . <list>))
(define (extract-concepts path)
  (let* ([sexps (read-sexps-from-file path)]
         [all-symbols (collect-symbols sexps)]
         [concept-syms (filter capitalized-symbol? all-symbols)]
         [counts (count-occurrences concept-syms)])
        (map (lambda (pair)
                     (let ([sym (car pair)]
                           [count (cdr pair)])
                          `((name . ,sym)
                            (type . ,(classify-concept sym))
                            (occurrences . ,count))))
             (sort-by-count counts))))

;;; collect-symbols : Sexpr -> (List Symbol)
;;; Recursively collect all symbols from an S-expression.
(define (collect-symbols expr)
  (cond
   [(null? expr) '()]
   [(symbol? expr) (list expr)]
   [(pair? expr) (append (collect-symbols (car expr))
                         (collect-symbols (cdr expr)))]
   [else '()]))

;;; capitalized-symbol? : Any -> Boolean
;;; Is this a symbol that starts with a capital letter?
(define (capitalized-symbol? x)
  (and (symbol? x)
       (let ([str (symbol->string x)])
            (and (> (string-length str) 0)
                 (char-upper-case? (string-ref str 0))))))

;;; count-occurrences : (List Symbol) -> (List (Symbol . Nat))
;;; Count occurrences of each unique symbol.
(define (count-occurrences syms)
  (let loop ([syms syms] [counts '()])
       (if (null? syms)
           counts
           (let* ([sym (car syms)]
                  [existing (assq sym counts)])
                 (loop (cdr syms)
                       (if existing
                           (map (lambda (pair)
                                        (if (eq? (car pair) sym)
                                            (cons sym (+ 1 (cdr pair)))
                                            pair))
                                counts)
                           (cons (cons sym 1) counts)))))))

;;; sort-by-count : (List (Symbol . Nat)) -> (List (Symbol . Nat))
;;; Sort by count descending.
(define (sort-by-count pairs)
  (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs))

;;; classify-concept : Symbol -> Symbol
;;; Classify a concept as record, base, or compound.
(define (classify-concept sym)
  (let ([name (symbol->string sym)])
       (cond
        [(memq sym '(Block Type Hash Ref)) 'fundamental]
        [(memq sym '(Nat Int Bool Char Symbol String Bytes Bytevector Unit Void)) 'base]
        [(memq sym '(List Vector)) 'container]
        [(memq sym '(Capability FS)) 'capability]
        [(memq sym '(Result Error OK)) 'result]
        [(memq sym '(Forum Channel Post Head Pin)) 'forum]
        [(memq sym '(Core Shell Prelude)) 'module]
        [(memq sym '(Tier Session)) 'session]
        [else 'other])))

;;; ====
;;; Relationship Finding
;;; ====

;;; find-relationships : (List Definition) -> (List Relationship)
;;; Infer relationships between concepts based on definitions.
;;; Returns list of:
;;;   ((<from-concept> <relation> <to-concept>))
(define (find-relationships defs)
  (let loop ([defs defs] [rels '()])
       (if (null? defs)
           (reverse rels)
           (let* ([def (car defs)]
                  [new-rels (extract-definition-rels def)])
                 (loop (cdr defs) (append new-rels rels))))))

;;; extract-definition-rels : Definition -> (List Relationship)
;;; Extract relationships from a single definition.
(define (extract-definition-rels def)
  (let ([type (cdr (assq 'type def))]
        [name (cdr (assq 'name def))])
       (cond
        ;; Records have field relationships
        [(eq? type 'record)
         (let ([fields (cdr (assq 'fields def))])
              (map (lambda (field)
                           (list name 'has-field field))
                   fields))]
        ;; Functions might reference concepts in their name
        [(eq? type 'function)
         (let ([params (cdr (assq 'params def))])
              (append
               (infer-name-relationships name)
               (map (lambda (param)
                            (list name 'takes-param param))
                    params)))]
        [else '()])))

;;; infer-name-relationships : Symbol -> (List Relationship)
;;; Infer relationships from function naming patterns.
(define (infer-name-relationships name)
  (let ([str (symbol->string name)])
       (cond
        ;; type->something patterns
        [(string-contains? str "->")
         (let ([parts (string-split-at str "->")])
              (if (= (length parts) 2)
                  (let ([from (string->symbol (car parts))]
                        [to (string->symbol (cadr parts))])
                       (list (list from 'converts-to to)))
                  '()))]
        ;; something? predicates
        [(string-ends-with? str "?")
         (let ([concept (string->symbol (substring str 0 (- (string-length str) 1)))])
              (list (list concept 'has-predicate name)))]
        ;; make-something constructors
        [(string-starts-with? str "make-")
         (let ([concept (string->symbol (substring str 5 (string-length str)))])
              (list (list concept 'has-constructor name)))]
        ;; something-something accessors
        [(string-contains? str "-")
         (let ([parts (string-split str #\-)])
              (if (>= (length parts) 2)
                  (let ([concept (string->symbol (car parts))])
                       (if (capitalized-symbol? concept)
                           (list (list concept 'has-accessor name))
                           '()))
                  '()))]
        [else '()])))

;;; NOTE: string-contains? provided by core/prelude.ss

;;; string-split-at : String x String -> (List String)
;;; Split string at FIRST occurrence of delimiter.
;;; Returns 2-element list (before, after) or 1-element list if not found.
;;; NOTE: This differs from string-split-flex which splits on ALL occurrences.
(define (string-split-at str delim)
  (let ([str-len (string-length str)]
        [delim-len (string-length delim)])
       (let loop ([i 0])
            (cond
             [(> (+ i delim-len) str-len) (list str)]
             [(string=? delim (substring str i (+ i delim-len)))
              (list (substring str 0 i)
                    (substring str (+ i delim-len) str-len))]
             [else (loop (+ i 1))]))))

;;; NOTE: string-split, string-starts-with?, string-ends-with?
;;;       provided by core/prelude.ss

;;; ====
;;; Concept Graph Building
;;; ====

;;; build-concept-graph : String -> ConceptGraph
;;; Build a graph of concepts and their relationships from a file.
;;; Returns:
;;;   ((concepts . <alist of concept -> properties>)
;;;    (relationships . <list of (from relation to)>))
(define (build-concept-graph path)
  (let* ([defs (extract-definitions path)]
         [concepts (extract-concepts path)]
         [rels (find-relationships defs)]
         [concept-map (build-concept-map-from-defs defs concepts)])
        `((concepts . ,concept-map)
          (definitions . ,(length defs))
          (relationships . ,rels)
          (source . ,path))))

;;; build-concept-map-from-defs : (List Def) x (List Concept) -> Alist
;;; Build concept properties from definitions and concept occurrences.
(define (build-concept-map-from-defs defs concepts)
  (let* ([records (filter (lambda (d) (eq? (cdr (assq 'type d)) 'record)) defs)]
         [functions (filter (lambda (d) (eq? (cdr (assq 'type d)) 'function)) defs)])
        (map (lambda (concept-info)
                     (let* ([name (cdr (assq 'name concept-info))]
                            [type (cdr (assq 'type concept-info))]
                            [occurrences (cdr (assq 'occurrences concept-info))]
                            ;; Find if this concept has a record definition
                            [record-def (find-def-by-name name records)]
                            [fields (if record-def
                                        (cdr (assq 'fields record-def))
                                        '())]
                            ;; Find related functions
                            [related-fns (find-related-functions name functions)])
                           (cons name
                                 `((type . ,type)
                                   (occurrences . ,occurrences)
                                   ,@(if (null? fields) '() `((fields . ,fields)))
                                   ,@(if (null? related-fns) '() `((functions . ,related-fns)))))))
             concepts)))

;;; find-def-by-name : Symbol x (List Def) -> Def | #f
(define (find-def-by-name name defs)
  (cond
   [(null? defs) #f]
   [(eq? (cdr (assq 'name (car defs))) name) (car defs)]
   [else (find-def-by-name name (cdr defs))]))

;;; find-related-functions : Symbol x (List Def) -> (List Symbol)
;;; Find functions whose names reference this concept.
(define (find-related-functions concept-name fns)
  (let ([concept-str (string-downcase (symbol->string concept-name))])
       (filter-map
        (lambda (fn)
                (let* ([fn-name (cdr (assq 'name fn))]
                       [fn-str (string-downcase (symbol->string fn-name))])
                      (if (string-contains? fn-str concept-str)
                          fn-name
                          #f)))
        fns)))

;;; NOTE: string-downcase provided by core/prelude.ss

;;; filter-map : (a -> b | #f) x (List a) -> (List b)
;;; Map and filter out #f results.
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (loop (cdr lst)
                      (if result (cons result acc) acc))))))

;;; ====
;;; Rendering
;;; ====

;;; render-concept-map : ConceptGraph -> String
;;; Render concept graph as readable text output.
(define (render-concept-map graph)
  (let* ([concepts (cdr (assq 'concepts graph))]
         [rels (cdr (assq 'relationships graph))]
         [source (cdr (assq 'source graph))]
         [def-count (cdr (assq 'definitions graph))])
        (string-append
         (render-header source def-count (length concepts) (length rels))
         "\n"
         (render-concepts concepts)
         "\n"
         (render-relationships rels))))

;;; render-header : String x Nat x Nat x Nat -> String
(define (render-header source def-count concept-count rel-count)
  (string-append
   "====\n"
   "CONCEPT MAP\n"
   "====\n"
   "Source: " source "\n"
   "Definitions: " (number->string def-count) "\n"
   "Concepts: " (number->string concept-count) "\n"
   "Relationships: " (number->string rel-count) "\n"
   "====\n"))

;;; render-concepts : Alist -> String
(define (render-concepts concepts)
  (if (null? concepts)
      "  (no concepts found)\n"
      (apply string-append
             (map render-concept concepts))))

;;; render-concept : (Symbol . Alist) -> String
(define (render-concept entry)
  (let* ([name (car entry)]
         [props (cdr entry)]
         [type (cdr (assq 'type props))]
         [occurrences (cdr (assq 'occurrences props))]
         [fields-pair (assq 'fields props)]
         [fields (if fields-pair (cdr fields-pair) '())]
         [fns-pair (assq 'functions props)]
         [fns (if fns-pair (cdr fns-pair) '())])
        (string-append
         "\n  " (symbol->string name) " [" (symbol->string type) "] "
         "(" (number->string occurrences) " occurrences)\n"
         (if (null? fields)
             ""
             (string-append "    Fields: " (render-symbol-list fields) "\n"))
         (if (null? fns)
             ""
             (string-append "    Functions: " (render-symbol-list (take-up-to 5 fns))
                            (if (> (length fns) 5) " ..." "") "\n")))))

;;; render-symbol-list : (List Symbol) -> String
(define (render-symbol-list syms)
  (if (null? syms)
      "()"
      (string-join (map symbol->string syms) ", ")))

;;; take-up-to : Nat x (List a) -> (List a)
(define (take-up-to n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take-up-to (- n 1) (cdr lst)))))

;;; render-relationships : (List Relationship) -> String
(define (render-relationships rels)
  (if (null? rels)
      "  (no relationships found)\n"
      (string-append
       "\nRELATIONSHIPS:\n"
       (apply string-append
              (map (lambda (rel)
                           (string-append
                            "  " (symbol->string (car rel))
                            " --[" (symbol->string (cadr rel)) "]--> "
                            (symbol->string (caddr rel)) "\n"))
                   (take-up-to 20 rels)))
       (if (> (length rels) 20)
           (string-append "  ... and " (number->string (- (length rels) 20)) " more\n")
           ""))))

;;; ====
;;; Multi-file Analysis
;;; ====

;;; scan-directory-concepts : String x String -> ConceptGraph
;;; Scan a directory for all .ss files and build combined concept graph.
(define (scan-directory-concepts dir pattern)
  (let* ([files (directory-files-matching dir pattern)]
         [graphs (map build-concept-graph files)]
         [all-concepts (merge-concepts (map (lambda (g) (cdr (assq 'concepts g))) graphs))]
         [all-rels (apply append (map (lambda (g) (cdr (assq 'relationships g))) graphs))]
         [total-defs (apply + (map (lambda (g) (cdr (assq 'definitions g))) graphs))])
        `((concepts . ,all-concepts)
          (definitions . ,total-defs)
          (relationships . ,(unique-rels all-rels))
          (source . ,dir)
          (files . ,(length files)))))

;;; directory-files-matching : String x String -> (List String)
;;; List files in directory matching pattern (simple .ss check).
(define (directory-files-matching dir pattern)
  (if (not (file-exists? dir))
      '()
      (let ([entries (directory-list dir)])
           (filter-map
            (lambda (entry)
                    (let ([full-path (string-append dir "/" entry)])
                         (if (and (not (file-directory? full-path))
                                  (string-ends-with? entry ".ss"))
                             full-path
                             #f)))
            entries))))

;;; merge-concepts : (List (List (Symbol . Props))) -> (List (Symbol . Props))
;;; Merge concept lists, combining occurrences.
(define (merge-concepts concept-lists)
  (let ([all-concepts (apply append concept-lists)])
       (let loop ([concepts all-concepts] [merged '()])
            (if (null? concepts)
                merged
                (let* ([entry (car concepts)]
                       [name (car entry)]
                       [existing (assq name merged)])
                      (loop (cdr concepts)
                            (if existing
                                ;; Merge occurrences
                                (map (lambda (e)
                                             (if (eq? (car e) name)
                                                 (merge-concept-entries e entry)
                                                 e))
                                     merged)
                                (cons entry merged))))))))

;;; merge-concept-entries : Entry x Entry -> Entry
(define (merge-concept-entries e1 e2)
  (let* ([name (car e1)]
         [props1 (cdr e1)]
         [props2 (cdr e2)]
         [occ1 (cdr (assq 'occurrences props1))]
         [occ2 (cdr (assq 'occurrences props2))]
         [type (cdr (assq 'type props1))]
         [fields1 (let ([f (assq 'fields props1)]) (if f (cdr f) '()))]
         [fields2 (let ([f (assq 'fields props2)]) (if f (cdr f) '()))]
         [fns1 (let ([f (assq 'functions props1)]) (if f (cdr f) '()))]
         [fns2 (let ([f (assq 'functions props2)]) (if f (cdr f) '()))])
        (cons name
              `((type . ,type)
                (occurrences . ,(+ occ1 occ2))
                ,@(if (null? fields1) '() `((fields . ,(unique (append fields1 fields2)))))
                ,@(if (null? fns1) '() `((functions . ,(unique (append fns1 fns2)))))))))

;;; unique-rels : (List Relationship) -> (List Relationship)
;;; Remove duplicate relationships.
;;; Alias for prelude's unique (uses equal? for deep comparison).
(define unique-rels unique)

;;; ====
;;; Convenience Functions
;;; ====

;;; concept-map : String -> void
;;; Build and display concept map for a file.
(define (concept-map path)
  (let ([graph (build-concept-graph path)])
       (display (render-concept-map graph))
       graph))

;;; concept-map-dir : String -> void
;;; Build and display concept map for a directory.
(define (concept-map-dir dir)
  (let ([graph (scan-directory-concepts dir ".ss")])
       (display (render-concept-map graph))
       graph))

;;; ====
;;; Help
;;; ====

(define (concept-map-help)
  (display "
====
CONCEPT MAP TOOL
====

Extracts the ontology/concept structure from Scheme source files.

SINGLE FILE:
  (concept-map \"path/to/file.ss\")

  Returns and displays a concept graph with:
  - All defined records, functions, values
  - Capitalized concepts (Block, Type, Hash, etc.)
  - Inferred relationships between concepts

DIRECTORY:
  (concept-map-dir \"path/to/directory\")

  Scans all .ss files and merges results.

LOW-LEVEL API:
  (extract-definitions path)  ; Get all define forms
  (extract-concepts path)     ; Get capitalized symbols with counts
  (find-relationships defs)   ; Infer relationships from definitions
  (build-concept-graph path)  ; Build full graph structure
  (render-concept-map graph)  ; Render as readable text

OUTPUT FORMAT:
  ((concepts . ((Block . ((type . fundamental)
                          (occurrences . 42)
                          (fields . (tag payload refs))
                          (functions . (make-block block-tag ...))))
                ...))
   (relationships . ((Block has-field tag)
                     (block converts-to bytes)
                     ...)))

"))
