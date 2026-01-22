;;; lattice/dataset/format/sexp.ss — Canonical S-expression Format
;;; @module sexp
;;; @requires prelude sample

(load "core/base/prelude.ss")
(load "lattice/dataset/sample.ss")

(doc 'module 'sexp)
(doc 'description "Canonical S-expression format for samples. This is the native storage format, content-addressed via the CAS.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Part 1: Sample Serialization
;;; ============================================================

(doc 'section 'serialization)

;;; sample->canonical-sexp : Sample → Sexp
;;; Convert sample to canonical S-expression (stable, hashable)
(define (sample->canonical-sexp s)
  (doc 'export #t)
  `(sample
    (version "1.0")
    (id ,(sample-id s))
    (frames ,@(sample-frames s))
    (question ,(sample-question s))
    (options
     ,@(map (lambda (opt)
             `(,(car opt) ,(cdr opt)))
           (sample-options s)))
    (answer ,(sample-answer s))
    (metadata
     ,@(map (lambda (m)
             `(,(car m) ,(cdr m)))
           (sort-alist (sample-metadata s))))))

;;; sort-alist : Alist → Alist
;;; Sort by key for canonical ordering
(define (sort-alist al)
  (list-sort (lambda (a b)
              (string<? (symbol->string (car a))
                        (symbol->string (car b))))
            al))

;;; canonical-sexp->sample : Sexp → Sample
;;; Parse sample from canonical S-expression
(define (canonical-sexp->sample sexp)
  (doc 'export #t)
  (if (and (pair? sexp) (eq? (car sexp) 'sample))
      (let* ([fields (cdr sexp)]
             [get (lambda (key)
                   (let ([pair (assq key fields)])
                     (and pair (cdr pair))))]
             [id (car (get 'id))]
             [frames (get 'frames)]
             [question (car (get 'question))]
             [opts-raw (get 'options)]
             [options (map (lambda (o)
                           (cons (car o) (cadr o)))
                         opts-raw)]
             [answer (car (get 'answer))]
             [meta-raw (get 'metadata)]
             [metadata (map (lambda (m)
                             (cons (car m) (cadr m)))
                           meta-raw)])
        (make-sample id frames question options answer metadata))
      (error 'canonical-sexp->sample "Invalid sample S-expression")))

;;; ============================================================
;;; Part 2: Dataset Serialization
;;; ============================================================

(doc 'section 'dataset)

;;; dataset->sexp : String × String × (List Sample) → Sexp
;;; Convert dataset to S-expression
(define (dataset->sexp name description samples)
  (doc 'export #t)
  `(dataset
    (name ,name)
    (description ,description)
    (version "1.0")
    (count ,(length samples))
    (samples
     ,@(map sample->canonical-sexp samples))))

;;; sexp->dataset : Sexp → (String × String × (List Sample))
;;; Parse dataset from S-expression
(define (sexp->dataset sexp)
  (doc 'export #t)
  (if (and (pair? sexp) (eq? (car sexp) 'dataset))
      (let* ([fields (cdr sexp)]
             [get (lambda (key)
                   (let ([pair (assq key fields)])
                     (and pair (cdr pair))))]
             [name (car (get 'name))]
             [description (car (get 'description))]
             [samples-raw (get 'samples)]
             [samples (map canonical-sexp->sample samples-raw)])
        (list name description samples))
      (error 'sexp->dataset "Invalid dataset S-expression")))

;;; ============================================================
;;; Part 3: Pretty Printing
;;; ============================================================

(doc 'section 'pretty)

;;; sexp->string : Sexp → String
;;; Pretty-print S-expression to string
(define (sexp->string sexp)
  (doc 'export #t)
  (format "~s" sexp))

;;; sexp->pretty-string : Sexp × Nat → String
;;; Pretty-print with indentation
(define (sexp->pretty-string sexp indent)
  (doc 'export #t)
  (pp-sexp sexp indent 0))

;;; pp-sexp : Sexp × Nat × Nat → String
(define (pp-sexp sexp indent depth)
  (let ([pad (make-string (* depth indent) #\space)])
    (cond
      [(null? sexp) "()"]
      [(not (pair? sexp))
       (cond
         [(string? sexp) (format "~s" sexp)]
         [(symbol? sexp) (symbol->string sexp)]
         [(number? sexp) (number->string sexp)]
         [else (format "~s" sexp)])]
      [(and (eq? (car sexp) 'sample)
            (> (length sexp) 3))
       ;; Format sample on multiple lines
       (string-append
        "(sample\n"
        (apply string-append
               (map (lambda (field)
                     (string-append pad "  " (pp-sexp field indent (+ depth 1)) "\n"))
                   (cdr sexp)))
        pad ")")]
      [(and (pair? sexp) (< (sexp-length sexp) 60))
       ;; Short list, inline
       (string-append "(" (join-strings (map (lambda (x) (pp-sexp x indent depth))
                                             sexp)
                                        " ")
                      ")")]
      [else
       ;; Long list, multi-line
       (string-append
        "(" (pp-sexp (car sexp) indent depth) "\n"
        (apply string-append
               (map (lambda (x)
                     (string-append pad "  " (pp-sexp x indent (+ depth 1)) "\n"))
                   (cdr sexp)))
        pad ")")])))

;;; sexp-length : Sexp → Nat
;;; Estimate printed length
(define (sexp-length sexp)
  (cond
    [(null? sexp) 2]
    [(not (pair? sexp))
     (cond
       [(string? sexp) (+ 2 (string-length sexp))]
       [(symbol? sexp) (string-length (symbol->string sexp))]
       [(number? sexp) (string-length (number->string sexp))]
       [else 10])]
    [else (+ 2 (apply + (map sexp-length sexp)))]))

;;; join-strings : (List String) × String → String
(define (join-strings strs sep)
  (if (null? strs)
      ""
      (let loop ([rest (cdr strs)] [acc (car strs)])
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc sep (car rest)))))))

;;; ============================================================
;;; Part 4: Validation
;;; ============================================================

(doc 'section 'validation)

;;; validate-sample-sexp : Sexp → (Ok Sample) | (Err String)
;;; Validate and parse sample S-expression
(define (validate-sample-sexp sexp)
  (doc 'export #t)
  (guard (e [else (err (format "Parse error: ~a" e))])
    (let ([sample (canonical-sexp->sample sexp)])
      (if (sample-valid? sample)
          (ok sample)
          (err "Invalid sample structure")))))

;;; ok : Any → Ok
(define (ok v) (list 'ok v))
(define (ok? x) (and (pair? x) (eq? (car x) 'ok)))
(define (from-ok x) (cadr x))

;;; err : String → Err
(define (err msg) (list 'err msg))
(define (err? x) (and (pair? x) (eq? (car x) 'err)))
(define (from-err x) (cadr x))
