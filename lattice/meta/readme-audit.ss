(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module meta/readme-audit
;;; @requires prelude
(require 'prelude)

(doc 'module 'meta/readme-audit)
(doc 'description "README.sexp schema canonicalization and audit. Defines the canonical
schema for directory documentation, validates existing files, and detects
inconsistencies (flat-list vs cons-pair patterns). Pure analysis functions.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Canonical Schema
;;; ====
;;;
;;; The canonical README.sexp schema uses flat-list (not cons-pair) notation:
;;;
;;;   ((name "module-name")
;;;    (purpose "One-line purpose")
;;;    (description "Multi-line description")
;;;    (layer core|lattice|boundary)
;;;    (purity total|mostly-pure|impure)
;;;    (modules ((file.ss "description") ...))
;;;    (dependencies (dep1 dep2 ...))
;;;    (see-also ("related1" "related2" ...)))
;;;
;;; Required fields: name, purpose
;;; Recommended fields: description, modules, layer
;;; Optional fields: purity, dependencies, see-also, usage, algorithm, load-order

(doc 'section 'readme-schema)

(define *readme-required-fields* '(name purpose))
(define *readme-recommended-fields* '(description modules layer))
(define *readme-optional-fields* '(purity dependencies see-also usage algorithm load-order tier-access))
(define *readme-all-known-fields*
  (append *readme-required-fields*
          *readme-recommended-fields*
          *readme-optional-fields*))

;;; ====
;;; Field Extraction (handles both styles)
;;; ====

(doc 'section 'readme-extract)

(doc 'type '(-> Alist Symbol Any))
(doc 'description "Extract a field value from a README.sexp entry.
Handles both flat-list ((name \"x\")) and cons-pair ((name . \"x\")) patterns.")
(define (readme-field entries key)
  (let loop ([entries entries])
    (cond
      [(null? entries) #f]
      [(not (pair? (car entries))) (loop (cdr entries))]
      [(eq? (caar entries) key)
       (let ([entry (car entries)])
         (cond
           ;; Cons pair: (name . "value")
           [(and (pair? (cdr entry)) (not (pair? (cadr entry)))
                 (not (null? (cdr entry)))
                 ;; Distinguish (name . "val") from (name "val")
                 ;; by checking if cddr is null — flat-list has (name "val") -> cddr = ()
                 (null? (cddr entry)))
            (cadr entry)]
           ;; Cons pair where cdr is not a list: (name . value)
           [(not (pair? (cdr entry)))
            (cdr entry)]
           ;; Flat list: (name "value") or (name (sublist))
           [else (cadr entry)]))]
      [else (loop (cdr entries))])))

;;; ====
;;; Style Detection
;;; ====

(doc 'section 'readme-style)

(doc 'type '(-> Alist Symbol))
(doc 'description "Detect the notation style of a README.sexp: 'flat-list, 'cons-pair, or 'mixed.
Flat-list: ((name \"x\") ...). Cons-pair: ((name . \"x\") ...).")
(define (readme-detect-style entries)
  (let loop ([entries entries] [flat 0] [cons-p 0])
    (cond
      [(null? entries)
       (cond
         [(and (> flat 0) (= cons-p 0)) 'flat-list]
         [(and (> cons-p 0) (= flat 0)) 'cons-pair]
         [(and (> flat 0) (> cons-p 0)) 'mixed]
         [else 'empty])]
      [(not (pair? (car entries))) (loop (cdr entries) flat cons-p)]
      [else
       (let ([entry (car entries)])
         (cond
           ;; Has a symbol key
           [(not (symbol? (car entry)))
            (loop (cdr entries) flat cons-p)]
           ;; Cons pair: (key . atom) where atom is not a list
           [(and (not (null? (cdr entry)))
                 (not (pair? (cdr entry))))
            (loop (cdr entries) flat (+ cons-p 1))]
           ;; Check for ((key . value) ...) pattern in modules
           [(eq? (car entry) 'modules)
            (loop (cdr entries) flat cons-p)]  ; skip modules — they have nested structure
           ;; Flat list: (key value) or (key (sublist))
           [else
            (loop (cdr entries) (+ flat 1) cons-p)]))])))

;;; ====
;;; Validation
;;; ====

(doc 'section 'readme-validation)

(doc 'type '(-> Alist (List (List Symbol String))))
(doc 'description "Validate a parsed README.sexp against the canonical schema.
Returns a list of (severity message) pairs where severity is 'error, 'warning, or 'info.")
(define (readme-validate entries)
  (let* ([style (readme-detect-style entries)]
         [issues '()]
         ;; Check required fields
         [issues (let loop ([fields *readme-required-fields*] [acc issues])
                   (if (null? fields) acc
                       (loop (cdr fields)
                             (if (readme-field entries (car fields))
                                 acc
                                 (cons (list 'error
                                         (format "Missing required field: ~a" (car fields)))
                                       acc)))))]
         ;; Check recommended fields
         [issues (let loop ([fields *readme-recommended-fields*] [acc issues])
                   (if (null? fields) acc
                       (loop (cdr fields)
                             (if (readme-field entries (car fields))
                                 acc
                                 (cons (list 'warning
                                         (format "Missing recommended field: ~a" (car fields)))
                                       acc)))))]
         ;; Check for unknown fields
         [issues (let loop ([entries entries] [acc issues])
                   (if (null? entries) acc
                       (loop (cdr entries)
                             (if (and (pair? (car entries))
                                      (symbol? (caar entries))
                                      (not (memq (caar entries) *readme-all-known-fields*)))
                                 (cons (list 'info
                                         (format "Unknown field: ~a" (caar entries)))
                                       acc)
                                 acc))))]
         ;; Check style consistency
         [issues (if (eq? style 'cons-pair)
                     (cons (list 'warning "Uses cons-pair style; canonical is flat-list")
                           issues)
                     issues)]
         [issues (if (eq? style 'mixed)
                     (cons (list 'error "Mixed styles (flat-list and cons-pair)")
                           issues)
                     issues)])
    (reverse issues)))

(doc 'type '(-> (List (List Symbol String)) Nat))
(doc 'description "Count issues of a given severity")
(define (readme-issue-count issues severity)
  (length (filter (lambda (i) (eq? (car i) severity)) issues)))

(doc 'type '(-> (List (List Symbol String)) Boolean))
(doc 'description "True if there are no errors (warnings/info are acceptable)")
(define (readme-valid? issues)
  (= 0 (readme-issue-count issues 'error)))

;;; ====
;;; Audit Report
;;; ====

(doc 'section 'readme-audit)

(doc 'type '(-> String Alist Alist))
(doc 'description "Produce an audit report for a single README.sexp file.
Takes the directory path and parsed S-expression content.")
(define (readme-audit-one path entries)
  (let* ([issues (readme-validate entries)]
         [style (readme-detect-style entries)]
         [name (readme-field entries 'name)]
         [errors (readme-issue-count issues 'error)]
         [warnings (readme-issue-count issues 'warning)])
    (list 'readme-audit
          (list 'path path)
          (list 'name (or name "(unnamed)"))
          (list 'style style)
          (list 'errors errors)
          (list 'warnings warnings)
          (list 'valid (readme-valid? issues))
          (list 'issues issues))))

(doc 'type '(-> Any Boolean))
(define (readme-audit? x)
  (and (pair? x) (eq? (car x) 'readme-audit)))

(doc 'type '(-> Alist Any))
(define (readme-audit-field report key)
  (let loop ([fields (cdr report)])
    (cond
      [(null? fields) #f]
      [(and (pair? (car fields)) (eq? (caar fields) key))
       (cadar fields)]
      [else (loop (cdr fields))])))

;;; ====
;;; Batch Audit Summary
;;; ====

(doc 'type '(-> (List Alist) Alist))
(doc 'description "Summarize a list of individual audit reports into aggregate stats")
(define (readme-audit-summary reports)
  (let* ([total (length reports)]
         [valid (length (filter (lambda (r) (readme-audit-field r 'valid)) reports))]
         [cons-style (length (filter (lambda (r)
                                       (eq? 'cons-pair (readme-audit-field r 'style)))
                                     reports))]
         [flat-style (length (filter (lambda (r)
                                       (eq? 'flat-list (readme-audit-field r 'style)))
                                     reports))]
         [total-errors (apply + (map (lambda (r) (readme-audit-field r 'errors)) reports))]
         [total-warnings (apply + (map (lambda (r) (readme-audit-field r 'warnings)) reports))])
    (list 'readme-audit-summary
          (list 'total total)
          (list 'valid valid)
          (list 'invalid (- total valid))
          (list 'flat-list-style flat-style)
          (list 'cons-pair-style cons-style)
          (list 'total-errors total-errors)
          (list 'total-warnings total-warnings))))
