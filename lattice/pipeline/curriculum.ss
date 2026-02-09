;;; lattice/pipeline/curriculum.ss — RL Curriculum Extraction
;;; @module curriculum
;;; @requires prelude

(doc 'module 'curriculum)
(doc 'description "Pure extraction logic for RL curriculum task-answer pairs from lattice source files. Walks S-expressions, accumulates available functions, and produces task-entry records.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;; ====
;; Task Entry Type
;; ====

(doc 'section 'task-entry)

(define (make-task-entry id skill module tier position name type description notes section deps available answer)
  (doc 'type (-> String Symbol Symbol Nat Nat Symbol String String (List String) (Maybe Symbol) (List Symbol) (List Symbol) String TaskEntry))
  (doc 'description "Construct a task-entry record for RL curriculum")
  (vector 'task-entry id skill module tier position name type description notes section deps available answer))

(define (task-entry? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (eq? (vector-ref x 0) 'task-entry)))

(define (task-entry-id te)        (vector-ref te 1))
(define (task-entry-skill te)     (vector-ref te 2))
(define (task-entry-module te)    (vector-ref te 3))
(define (task-entry-tier te)      (vector-ref te 4))
(define (task-entry-position te)  (vector-ref te 5))
(define (task-entry-name te)      (vector-ref te 6))
(define (task-entry-type te)      (vector-ref te 7))
(define (task-entry-desc te)      (vector-ref te 8))
(define (task-entry-notes te)     (vector-ref te 9))
(define (task-entry-section te)   (vector-ref te 10))
(define (task-entry-deps te)      (vector-ref te 11))
(define (task-entry-available te) (vector-ref te 12))
(define (task-entry-answer te)    (vector-ref te 13))

;; ====
;; Doc Form Extraction
;; ====

(doc 'section 'doc-extraction)

(define (doc-form? sexp)
  (doc 'type (-> SExp Boolean))
  (doc 'description "Is this a (doc ...) form?")
  (and (pair? sexp)
       (eq? (car sexp) 'doc)))

(define (internal-doc-form? sexp)
  (doc 'type (-> SExp Boolean))
  (doc 'description "Is this a doc form inside a define body? Internal doc forms look like (doc 'type ...) or (doc 'description ...) where the second element is a quoted tag.")
  (and (doc-form? sexp)
       (>= (length sexp) 3)
       (pair? (cadr sexp))
       (eq? (car (cadr sexp)) 'quote)))

(define (unwrap-quote val)
  (doc 'description "Strip (quote X) wrapper if present, returning X")
  (if (and (pair? val) (eq? (car val) 'quote) (pair? (cdr val)) (null? (cddr val)))
      (cadr val)
      val))

(define (extract-doc-from-forms forms)
  (doc 'description "Extract type/desc/notes from a list of forms containing doc annotations")
  (let loop ([forms forms] [type ""] [desc ""] [notes '()])
    (if (null? forms)
        (values type desc (reverse notes))
        (let ([form (car forms)])
          (if (internal-doc-form? form)
              (let ([tag (cadr (cadr form))]
                    [val (unwrap-quote (caddr form))])
                (cond
                  [(eq? tag 'type)
                   (loop (cdr forms) (format "~a" val) desc notes)]
                  [(eq? tag 'description)
                   (loop (cdr forms) type (if (string? val) val (format "~a" val)) notes)]
                  [(or (eq? tag 'note) (eq? tag 'complexity) (eq? tag 'example))
                   (loop (cdr forms) type desc
                         (cons (if (string? val) val (format "~a" val)) notes))]
                  [else (loop (cdr forms) type desc notes)]))
              (loop (cdr forms) type desc notes))))))

(define (extract-doc-annotations body)
  (doc 'type (-> (List SExp) (Values String String (List String))))
  (doc 'description "Extract type, description, and notes from doc forms in a define body. Returns (values type-string desc-string notes-list). Also looks inside case-lambda first clause.")
  (let-values ([(type desc notes) (extract-doc-from-forms body)])
    ;; If we didn't find a type, check inside case-lambda first clause
    (if (and (string=? type "")
             (= (length body) 1)
             (pair? (car body))
             (eq? (caar body) 'case-lambda)
             (pair? (cdar body)))
        ;; First clause body: ((formals) (doc ...) ... body)
        (let ([first-clause (cadar body)])
          (if (and (pair? first-clause) (pair? (cdr first-clause)))
              (let-values ([(t2 d2 n2) (extract-doc-from-forms (cdr first-clause))])
                (values (if (string=? type "") t2 type)
                        (if (string=? desc "") d2 desc)
                        (if (null? notes) n2 notes)))
              (values type desc notes)))
        (values type desc notes))))

(define (strip-doc-forms body)
  (doc 'type (-> (List SExp) (List SExp)))
  (doc 'description "Remove (doc ...) forms from a define body, keeping only executable code")
  (filter (lambda (form) (not (internal-doc-form? form))) body))

;; ====
;; Define Recognition
;; ====

(doc 'section 'define-recognition)

(define (define-form? sexp)
  (doc 'type (-> SExp Boolean))
  (doc 'description "Is this a (define ...) form?")
  (and (pair? sexp)
       (eq? (car sexp) 'define)
       (>= (length sexp) 3)))

(define (alias-define? sexp)
  (doc 'type (-> SExp Boolean))
  (doc 'description "Is this a (define x y) alias form where y is a simple symbol?")
  (and (define-form? sexp)
       (symbol? (cadr sexp))
       (= (length sexp) 3)
       (symbol? (caddr sexp))))

(define (function-define? sexp)
  (doc 'type (-> SExp Boolean))
  (doc 'description "Is this a (define (f args...) body...) function definition?")
  (and (define-form? sexp)
       (pair? (cadr sexp))))

(define (define-name sexp)
  (doc 'type (-> SExp Symbol))
  (doc 'description "Extract the name from a define form")
  (if (pair? (cadr sexp))
      (car (cadr sexp))   ; (define (f x) ...)
      (cadr sexp)))        ; (define x ...)

;; ====
;; Section Tracking
;; ====

(doc 'section 'section-tracking)

(define (section-doc? sexp)
  (doc 'type (-> SExp Boolean))
  (doc 'description "Is this a (doc 'section ...) form?")
  (and (doc-form? sexp)
       (>= (length sexp) 3)
       (pair? (cadr sexp))
       (eq? (car (cadr sexp)) 'quote)
       (eq? (cadr (cadr sexp)) 'section)))

(define (section-name sexp)
  (doc 'type (-> SExp Symbol))
  (doc 'description "Extract section name from (doc 'section 'name)")
  (let ([val (caddr sexp)])
    (if (and (pair? val) (eq? (car val) 'quote))
        (cadr val)
        val)))

;; ====
;; External Doc Annotations (post-define pattern)
;; ====

(doc 'section 'external-docs)

(define (external-doc-for? sexp name)
  (doc 'type (-> SExp Symbol Boolean))
  (doc 'description "Is this a (doc name 'tag value) form — external doc for a named define?")
  (and (doc-form? sexp)
       (>= (length sexp) 3)
       (eq? (cadr sexp) name)))

(define (collect-external-docs sexps name)
  (doc 'type (-> (List SExp) Symbol (Values String String (List String))))
  (doc 'description "Collect type/description/notes from external doc forms following a define")
  (let loop ([forms sexps] [type ""] [desc ""] [notes '()])
    (if (or (null? forms)
            (not (external-doc-for? (car forms) name)))
        (values type desc (reverse notes))
        (let* ([form (car forms)]
               [tag-form (caddr form)]
               [tag (if (and (pair? tag-form) (eq? (car tag-form) 'quote))
                        (cadr tag-form)
                        tag-form)]
               [val (unwrap-quote (if (>= (length form) 4)
                                      (cadddr form)
                                      ""))])
          (cond
            [(eq? tag 'type)
             (loop (cdr forms) (format "~a" val) desc notes)]
            [(eq? tag 'description)
             (loop (cdr forms) type (if (string? val) val (format "~a" val)) notes)]
            [(or (eq? tag 'note) (eq? tag 'complexity) (eq? tag 'example))
             (loop (cdr forms) type desc
                   (cons (if (string? val) val (format "~a" val)) notes))]
            [else (loop (cdr forms) type desc notes)])))))

;; ====
;; Load/Require Extraction
;; ====

(doc 'section 'deps-extraction)

(define (load-form? sexp)
  (and (pair? sexp)
       (eq? (car sexp) 'load)
       (>= (length sexp) 2)
       (string? (cadr sexp))))

(define (require-form? sexp)
  (and (pair? sexp)
       (eq? (car sexp) 'require)
       (>= (length sexp) 2)))

(define (extract-deps sexps)
  (doc 'type (-> (List SExp) (List String)))
  (doc 'description "Extract load/require dependencies from top-level forms")
  (let loop ([forms sexps] [deps '()])
    (if (null? forms)
        (reverse deps)
        (let ([form (car forms)])
          (cond
            [(load-form? form)
             (loop (cdr forms) (cons (cadr form) deps))]
            [(require-form? form)
             (loop (cdr forms) (cons (format "~a" (cadr form)) deps))]
            [else (loop (cdr forms) deps)])))))

;; ====
;; S-expression → Code String
;; ====

(doc 'section 'code-formatting)

(define (sexp->code-string sexp)
  (doc 'type (-> SExp String))
  (doc 'description "Convert an S-expression to a code string. Uses simple formatting — not the full pretty-printer (which lives in core/util/pretty.ss and would be a heavy dependency).")
  (cond
    [(null? sexp) "()"]
    [(symbol? sexp) (symbol->string sexp)]
    [(number? sexp) (number->string sexp)]
    [(string? sexp) (string-append "\"" sexp "\"")]
    [(boolean? sexp) (if sexp "#t" "#f")]
    [(char? sexp) (format "#\\~a" sexp)]
    [(vector? sexp)
     (string-append "#(" (join " " (map sexp->code-string (vector->list sexp))) ")")]
    [(pair? sexp)
     (if (and (pair? sexp)
              (eq? (car sexp) 'quote)
              (pair? (cdr sexp))
              (null? (cddr sexp)))
         (string-append "'" (sexp->code-string (cadr sexp)))
         (string-append "(" (join " " (sexp->code-string-list sexp)) ")"))]
    [else (format "~a" sexp)]))

(define (sexp->code-string-list sexp)
  (doc 'description "Handle proper and improper lists")
  (cond
    [(null? sexp) '()]
    [(pair? sexp)
     (cons (sexp->code-string (car sexp))
           (sexp->code-string-list (cdr sexp)))]
    [else (list "." (sexp->code-string sexp))]))

(define (join sep strs)
  (doc 'type (-> String (List String) String))
  (if (null? strs) ""
      (let loop ([rest (cdr strs)] [acc (car strs)])
        (if (null? rest)
            acc
            (loop (cdr rest)
                  (string-append acc sep (car rest)))))))

(define (strip-docs-deep sexp)
  (doc 'description "Recursively strip doc forms, including inside case-lambda clauses")
  (cond
    [(not (pair? sexp)) sexp]
    ;; case-lambda: strip doc forms from each clause body
    [(eq? (car sexp) 'case-lambda)
     (cons 'case-lambda
           (map (lambda (clause)
                  (if (and (pair? clause) (pair? (cdr clause)))
                      (cons (car clause) (strip-doc-forms (cdr clause)))
                      clause))
                (cdr sexp)))]
    [else sexp]))

(define (strip-define-docs sexp)
  (doc 'type (-> SExp SExp))
  (doc 'description "Strip doc forms from a define body, returning the cleaned define")
  (cond
    [(function-define? sexp)
     (let* ([header (cadr sexp)]
            [body (cddr sexp)]
            [clean-body (strip-doc-forms body)])
       (cons 'define (cons header (map strip-docs-deep clean-body))))]
    [(alias-define? sexp) sexp]
    [(define-form? sexp)
     ;; (define x expr) — variable/case-lambda define
     (let* ([name (cadr sexp)]
            [body (cddr sexp)]
            [clean-body (strip-doc-forms body)])
       (cons 'define (cons name (map strip-docs-deep clean-body))))]
    [else sexp]))

;; ====
;; Main Extraction
;; ====

(doc 'section 'extraction)

(define (extract-defines sexps skill-name module-name tier)
  (doc 'type (-> (List SExp) Symbol Symbol Nat (List TaskEntry)))
  (doc 'description "Walk sexps in order, accumulating available functions. For each define form, produce a task-entry with the functions defined earlier as 'available'. Tracks current section for grouping context.")
  (let ([deps (extract-deps sexps)])
    (let loop ([forms sexps]
               [position 0]
               [section #f]
               [available '()]
               [entries '()])
      (if (null? forms)
          (reverse entries)
          (let ([form (car forms)]
                [rest (cdr forms)])
            (cond
              ;; Section marker — update current section
              [(section-doc? form)
               (loop rest position (section-name form) available entries)]

              ;; Define form — extract task entry
              [(define-form? form)
               (let* ([name (define-name form)]
                      [id (string-append (symbol->string skill-name)
                                         "/"
                                         (symbol->string module-name)
                                         "/"
                                         (symbol->string name))])
                 ;; Get doc annotations (internal for functions, external for aliases)
                 (let-values ([(type desc notes)
                               (if (function-define? form)
                                   (extract-doc-annotations (cddr form))
                                   (collect-external-docs rest name))])
                   (let* ([clean-sexp (strip-define-docs form)]
                          [answer (sexp->code-string clean-sexp)]
                          [entry (make-task-entry
                                   id skill-name module-name tier position
                                   name type desc notes section
                                   (map string->symbol deps)
                                   available answer)])
                     (loop rest
                           (+ position 1)
                           section
                           (cons name available)
                           (cons entry entries)))))]

              ;; Anything else — skip
              [else
               (loop rest position section available entries)]))))))


;; ====
;; Difficulty Scoring
;; ====

(doc 'section 'difficulty)

(define (compute-difficulty tier skill-order position)
  (doc 'type (-> Nat Nat Nat Nat))
  (doc 'description "Compute difficulty score: tier * 10000 + skill-order * 100 + position")
  (+ (* tier 10000) (* skill-order 100) position))

;; ====
;; Output Formatting
;; ====

(doc 'section 'output)

(define (task-entry->sexp te)
  (doc 'type (-> TaskEntry SExp))
  (doc 'description "Convert task-entry to S-expression for serialization")
  `(task-entry
    (id ,(task-entry-id te))
    (skill ,(task-entry-skill te))
    (module ,(task-entry-module te))
    (tier ,(task-entry-tier te))
    (difficulty ,(task-entry-position te))  ; placeholder — driver sets real difficulty
    (name ,(task-entry-name te))
    (type ,(task-entry-type te))
    (description ,(task-entry-desc te))
    (notes ,(task-entry-notes te))
    (section ,(or (task-entry-section te) 'none))
    (deps ,(task-entry-deps te))
    (available ,(reverse (task-entry-available te)))
    (answer ,(task-entry-answer te))))

(define (task-entry->sexp/difficulty te difficulty)
  (doc 'type (-> TaskEntry Nat SExp))
  (doc 'description "Convert task-entry to S-expression with computed difficulty")
  `(task-entry
    (id ,(task-entry-id te))
    (skill ,(task-entry-skill te))
    (module ,(task-entry-module te))
    (tier ,(task-entry-tier te))
    (difficulty ,difficulty)
    (name ,(task-entry-name te))
    (type ,(task-entry-type te))
    (description ,(task-entry-desc te))
    (notes ,(task-entry-notes te))
    (section ,(or (task-entry-section te) 'none))
    (deps ,(task-entry-deps te))
    (available ,(reverse (task-entry-available te)))
    (answer ,(task-entry-answer te))))
