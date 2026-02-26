(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

;;; @module meta/unused-requires
;;; @requires prelude xref

(doc 'module 'meta/unused-requires)
(doc 'description "Detect unused (require ...) statements in Scheme source files.
Given a file's S-expressions and a function that maps module names to their
exported symbols, determines which requires are actually used, unused, or
uncertain (when exports are unknown).")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Require Extraction
;;; ====

(doc 'section 'extraction)

;;; extract-requires-from-sexps : (List SExp) -> (List Symbol)
;;; Walk top-level forms and extract module names from (require 'name) calls.
;;; Handles:
;;;   (require 'foo)              -> (foo)
;;;   (require 'foo 'bar)         -> (foo bar)   [multi-arg, unlikely but safe]
;;;   (unless ... (require ...))  -> recurse into unless/when bodies
;;; Skips requires inside (define ...) bodies — those are top-level only.
(doc extract-requires-from-sexps 'type '(-> (List SExp) (List Symbol)))
(doc extract-requires-from-sexps 'export #t)
(define (extract-requires-from-sexps sexps)
  (let loop ([forms sexps] [acc '()])
    (if (null? forms)
        (reverse acc)
        (loop (cdr forms)
              (append (reverse (extract-require-from-form (car forms)))
                      acc)))))

;;; extract-require-from-form : SExp -> (List Symbol)
;;; Extract require targets from a single top-level form.
(define (extract-require-from-form form)
  (cond
    [(not (pair? form)) '()]
    ;; Direct: (require 'foo) or (require 'foo 'bar ...)
    [(eq? (car form) 'require)
     (filter-map (lambda (arg)
                   (and (pair? arg)
                        (eq? (car arg) 'quote)
                        (pair? (cdr arg))
                        (symbol? (cadr arg))
                        (cadr arg)))
                 (cdr form))]
    ;; Guarded: (unless ... (require ...)) or (when ... (require ...))
    [(memq (car form) '(unless when))
     (append-map extract-require-from-form (cddr form))]
    ;; Begin block at top level
    [(eq? (car form) 'begin)
     (append-map extract-require-from-form (cdr form))]
    [else '()]))

;;; ====
;;; Symbol Collection (reuses xref.ss pattern)
;;; ====

(doc 'section 'symbol-collection)

;;; collect-file-symbols : (List SExp) -> (List Symbol)
;;; Collect all referenced symbols from a file's top-level forms,
;;; excluding quoted data and binding names. This is the full file
;;; version of extract-symbols-from-sexp (which operates on a single sexp).
;;;
;;; We require xref.ss for extract-symbols-from-sexp. Since this module
;;; is lattice-pure, we use a late-binding pattern: the function is
;;; expected to be bound at load time via require chain.
(doc collect-file-symbols 'type '(-> (List SExp) (List Symbol)))
(doc collect-file-symbols 'export #t)
(define (collect-file-symbols sexps)
  (let ([all-syms (append-map extract-symbols-from-sexp*-safe sexps)])
    (deduplicate-symbols all-syms)))

;;; extract-symbols-from-sexp*-safe : SExp -> (List Symbol)
;;; Guard-wrapped version that returns '() on unexpected structure.
(define (extract-symbols-from-sexp*-safe sexp)
  (guard (e [else '()])
    (extract-symbols-from-sexp* sexp)))

;;; extract-symbols-from-sexp* : SExp -> (List Symbol)
;;; Scope-aware symbol extraction. Skips quotes, binding names in
;;; let/lambda/define. Inline implementation so this module doesn't
;;; depend on xref.ss's mutable state.
;;; safe-append-map : (a -> (List b)) (List-or-Improper a) -> (List b)
;;; Like append-map but handles improper lists (dotted pairs) gracefully.
;;; (a b . c) -> process a, b, and c individually.
(define (safe-append-map f lst)
  (cond
    [(null? lst) '()]
    [(not (pair? lst)) (f lst)]
    [else
     (append (f (car lst))
             (safe-append-map f (cdr lst)))]))

(define (extract-symbols-from-sexp* sexp)
  (cond
    [(symbol? sexp) (list sexp)]
    [(not (pair? sexp)) '()]
    [(not (symbol? (car sexp)))
     ;; Non-symbol in car position — recurse safely into proper list parts
     (safe-append-map extract-symbols-from-sexp* sexp)]
    [else
     (case (car sexp)
       ;; Skip quoted expressions
       [(quote quasiquote) '()]
       ;; Handle define — skip the name being defined
       [(define define-record-type define-syntax)
        (if (and (pair? (cdr sexp)) (pair? (cddr sexp)))
            (safe-append-map extract-symbols-from-sexp* (cddr sexp))
            '())]
       ;; Handle let/let*/letrec — skip binding names
       [(let let* letrec letrec*)
        (cond
          ;; Named let: (let name ([var init] ...) body...)
          [(and (pair? (cdr sexp))
                (symbol? (cadr sexp))
                (pair? (cddr sexp))
                (list? (caddr sexp)))
           (let ([bindings (caddr sexp)]
                 [body (cdddr sexp)])
             (append
              (safe-append-map (lambda (b)
                                 (if (and (pair? b) (pair? (cdr b)))
                                     (extract-symbols-from-sexp* (cadr b))
                                     '()))
                               (if (list? bindings) bindings '()))
              (safe-append-map extract-symbols-from-sexp* body)))]
          ;; Regular let: (let ([var init] ...) body...)
          [(and (pair? (cdr sexp)) (list? (cadr sexp)))
           (let ([bindings (cadr sexp)]
                 [body (cddr sexp)])
             (append
              (safe-append-map (lambda (b)
                                 (if (and (pair? b) (pair? (cdr b)))
                                     (extract-symbols-from-sexp* (cadr b))
                                     '()))
                               (if (list? bindings) bindings '()))
              (safe-append-map extract-symbols-from-sexp* body)))]
          [else '()])]
       ;; Handle lambda/case-lambda — skip parameter names, extract from body
       [(lambda)
        (if (and (pair? (cdr sexp)) (pair? (cddr sexp)))
            (safe-append-map extract-symbols-from-sexp* (cddr sexp))
            '())]
       [(case-lambda)
        (safe-append-map (lambda (clause)
                           (if (and (pair? clause) (pair? (cdr clause)))
                               (safe-append-map extract-symbols-from-sexp* (cdr clause))
                               '()))
                         (cdr sexp))]
       ;; Handle syntax-case/syntax-rules — too complex, skip
       [(syntax-case syntax-rules) '()]
       ;; Handle doc forms — skip (doc ...) since they're metadata
       [(doc) '()]
       ;; Handle require/load — not symbol references
       [(require load library import) '()]
       ;; Default: recurse into all parts
       [else
        (safe-append-map extract-symbols-from-sexp* sexp)])]))

;;; deduplicate-symbols : (List Symbol) -> (List Symbol)
;;; Remove duplicate symbols while preserving order.
(define (deduplicate-symbols syms)
  (let loop ([remaining syms] [seen '()] [acc '()])
    (if (null? remaining)
        (reverse acc)
        (let ([sym (car remaining)])
          (if (memq sym seen)
              (loop (cdr remaining) seen acc)
              (loop (cdr remaining)
                    (cons sym seen)
                    (cons sym acc)))))))

;;; ====
;;; Core Analysis
;;; ====

(doc 'section 'analysis)

;;; analyze-file-requires : (List SExp) (Symbol -> (List Symbol)) -> RequireAnalysis
;;; Given a file's S-expressions and a function that maps module names to
;;; their exported symbols, classify each require as used, unused, or uncertain.
;;;
;;; The export-lookup function should return:
;;;   - A non-empty list of symbols if the module's exports are known
;;;   - '() if the module has no known exports (classified as 'uncertain)
;;;
;;; Returns an alist:
;;;   ((unused  . (list-of-module-names))
;;;    (used    . (list-of-module-names))
;;;    (uncertain . (list-of-module-names)))
(doc analyze-file-requires 'type '(-> (List SExp) (-> Symbol (List Symbol)) RequireAnalysis))
(doc analyze-file-requires 'export #t)
(define (analyze-file-requires sexps export-lookup)
  (let* ([requires (extract-requires-from-sexps sexps)]
         [used-syms (collect-file-symbols sexps)]
         [results (map (lambda (mod)
                         (classify-require mod (export-lookup mod) used-syms))
                       requires)])
    `((unused    . ,(filter-map (lambda (r) (and (eq? (cdr r) 'unused) (car r))) results))
      (used      . ,(filter-map (lambda (r) (and (eq? (cdr r) 'used) (car r))) results))
      (uncertain . ,(filter-map (lambda (r) (and (eq? (cdr r) 'uncertain) (car r))) results)))))

;;; classify-require : Symbol (List Symbol) (List Symbol) -> (Pair Symbol Classification)
;;; Classify a single require as used, unused, or uncertain.
;;;   mod: the required module name
;;;   mod-exports: that module's exported symbols (empty = unknown)
;;;   used-syms: all symbols referenced in the file
(define (classify-require mod mod-exports used-syms)
  (cond
    ;; No known exports — can't determine usage
    [(null? mod-exports)
     (cons mod 'uncertain)]
    ;; Check if any export from this module is referenced
    [(exists (lambda (exp) (memq exp used-syms)) mod-exports)
     (cons mod 'used)]
    ;; None of the module's exports are referenced
    [else
     (cons mod 'unused)]))

;;; ====
;;; Detail Report
;;; ====

(doc 'section 'detail)

;;; analyze-file-requires-detail : (List SExp) (Symbol -> (List Symbol)) -> DetailedAnalysis
;;; Like analyze-file-requires but also reports which specific exports are used
;;; from each module. Useful for understanding partial usage.
;;;
;;; Returns: ((module-name . ((status . used|unused|uncertain)
;;;                           (exports-used . (sym ...))
;;;                           (exports-total . N))) ...)
(doc analyze-file-requires-detail 'type '(-> (List SExp) (-> Symbol (List Symbol)) DetailedAnalysis))
(doc analyze-file-requires-detail 'export #t)
(define (analyze-file-requires-detail sexps export-lookup)
  (let* ([requires (extract-requires-from-sexps sexps)]
         [used-syms (collect-file-symbols sexps)])
    (map (lambda (mod)
           (let* ([mod-exports (export-lookup mod)]
                  [exports-used (filter (lambda (e) (memq e used-syms)) mod-exports)]
                  [status (cond
                            [(null? mod-exports) 'uncertain]
                            [(pair? exports-used) 'used]
                            [else 'unused])])
             (cons mod `((status . ,status)
                         (exports-used . ,exports-used)
                         (exports-total . ,(length mod-exports))))))
         requires)))

;;; ====
;;; Convenience Accessors
;;; ====

(doc 'section 'accessors)

(doc analysis-unused 'type '(-> RequireAnalysis (List Symbol)))
(doc analysis-unused 'export #t)
(define (analysis-unused result)
  (cdr (assq 'unused result)))

(doc analysis-used 'type '(-> RequireAnalysis (List Symbol)))
(doc analysis-used 'export #t)
(define (analysis-used result)
  (cdr (assq 'used result)))

(doc analysis-uncertain 'type '(-> RequireAnalysis (List Symbol)))
(doc analysis-uncertain 'export #t)
(define (analysis-uncertain result)
  (cdr (assq 'uncertain result)))
