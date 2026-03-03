;;; lattice/meta/analyzer-patterns.ss — Pattern completeness lint analyzer
;;; @module meta/analyzer-patterns
;;; @requires prelude meta/lattice-walk

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'meta/lattice-walk)

(doc 'module 'meta/analyzer-patterns)
(doc 'description "Pattern completeness lint analyzer for the lattice walker.
Scans modules for case expressions missing else clauses and duplicate case
datums. These are common sources of subtle bugs: a case without else silently
returns void (unspecified) when no datum matches.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Expression Finder
;;; ============================================================================

(doc 'section 'finder)

;;; find-case-exprs : (List SExp) -> (List SExp)
;;; Recursively find all (case ...) expressions in an sexp tree.
;;; Descends into define bodies, let bindings, lambda bodies, etc.
;;; Skips quoted and quasiquoted forms.
(doc find-case-exprs 'export #t)
(doc find-case-exprs 'type '(-> (List SExp) (List SExp)))
(define (find-case-exprs sexps)
  (let ([results '()])
    (define (walk expr)
      (when (pair? expr)
        ;; Match (case ...) forms
        (when (and (symbol? (car expr)) (eq? (car expr) 'case))
          (set! results (cons expr results)))
        ;; Recurse, but skip quoted data
        (unless (and (symbol? (car expr))
                     (memq (car expr) '(quote quasiquote)))
          (walk-children expr))))
    ;; Safe walker for improper lists: (lambda (x . rest) ...) has
    ;; dotted pairs in parameter position. for-each blows up on those.
    (define (walk-children lst)
      (when (pair? lst)
        (when (pair? (car lst)) (walk (car lst)))
        (walk-children (cdr lst))))
    (for-each walk sexps)
    (reverse results)))

;;; ============================================================================
;;; Case Analysis
;;; ============================================================================

(doc 'section 'case-analysis)

;;; case-has-else? : SExp -> Boolean
;;; Check if a case expression has an else clause as its last branch.
;;; Chez case syntax: (case expr [(datum ...) body ...] ... [else body ...])
(doc case-has-else? 'export #t)
(doc case-has-else? 'type '(-> SExp Boolean))
(define (case-has-else? case-expr)
  (let ([clauses (cddr case-expr)])  ; skip 'case and scrutinee
    (and (pair? clauses)
         (let loop ([cs clauses])
           (cond
             [(null? (cdr cs))
              ;; Last clause — check if it starts with else
              (and (pair? (car cs))
                   (eq? (caar cs) 'else))]
             [else (loop (cdr cs))])))))

;;; case-datums : SExp -> (List Datum)
;;; Extract all datums from a case expression's non-else clauses.
;;; Each clause car is a list of datums: [(d1 d2) body ...].
(doc case-datums 'export #t)
(doc case-datums 'type '(-> SExp (List Datum)))
(define (case-datums case-expr)
  (let ([clauses (cddr case-expr)])
    (apply append
           (filter-map
            (lambda (clause)
              (and (pair? clause)
                   (not (eq? (car clause) 'else))
                   (if (list? (car clause))
                       (car clause)
                       (list (car clause)))))
            clauses))))

;;; find-duplicate-datums : (List Datum) -> (List Datum)
;;; Find datums that appear more than once. Uses equal? for comparison.
(doc find-duplicate-datums 'export #t)
(doc find-duplicate-datums 'type '(-> (List Datum) (List Datum)))
(define (find-duplicate-datums datums)
  (let loop ([ds datums] [seen '()] [dups '()])
    (if (null? ds)
        (reverse dups)
        (let ([d (car ds)])
          (if (member d seen)
              (if (member d dups)
                  (loop (cdr ds) seen dups)       ; already reported
                  (loop (cdr ds) seen (cons d dups)))
              (loop (cdr ds) (cons d seen) dups))))))

;;; ============================================================================
;;; Analyzer
;;; ============================================================================

(doc 'section 'analyzer)

;;; make-pattern-analyzer : -> Analyzer
;;; Creates a pattern completeness lint analyzer.
;;;
;;; Reports modules containing:
;;;   - case expressions without else clauses
;;;   - duplicate datums in case expressions
;;;
;;; State: alist ((module-name . result-alist) ...)
;;; Only modules with findings are included.
;;;
;;; Result alist keys:
;;;   cases             — total case expressions found
;;;   cases-without-else — count missing else
;;;   duplicate-datums   — count with duplicate datums
(doc make-pattern-analyzer 'export #t)
(doc make-pattern-analyzer 'type '(-> Analyzer))
(define (make-pattern-analyzer)
  (make-analyzer
   'patterns
   (lambda () '())
   (lambda (mr acc state)
     (let* ([sexps (module-record-sexps mr)]
            [name (module-record-name mr)]
            [cases (find-case-exprs sexps)]
            [n-cases (length cases)]
            [incomplete (filter (lambda (c) (not (case-has-else? c))) cases)]
            [with-dups
             (filter-map
              (lambda (c)
                (let ([dups (find-duplicate-datums (case-datums c))])
                  (and (pair? dups) dups)))
              cases)]
            [n-issues (+ (length incomplete) (length with-dups))])
       (if (zero? n-issues)
           state
           (cons (cons name
                       `((cases . ,n-cases)
                         (cases-without-else . ,(length incomplete))
                         (duplicate-datums . ,(length with-dups))))
                 state))))))
