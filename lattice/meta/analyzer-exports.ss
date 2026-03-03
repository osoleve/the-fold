;;; lattice/meta/analyzer-exports.ss — Export collection analyzer
;;; @module meta/analyzer-exports
;;; @requires prelude meta/lattice-walk exports

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'meta/lattice-walk)
(require 'exports)

(doc 'module 'meta/analyzer-exports)
(doc 'description "Export collection analyzer for the lattice walker.
Wraps exports.ss pattern recognition to accumulate a tri-state export DB
as modules are processed in dependency order.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Export DB
;;; ============================================================================

;;; The export DB is an alist: ((module-name . (known (sym ...))) ...)
;;; Tri-state contract:
;;;   (assq mod db) => (mod . (known (sym ...)))  -- analyzed, has exports
;;;   (assq mod db) => (mod . (known ()))          -- analyzed, no exports
;;;   (assq mod db) => #f                          -- not yet analyzed (unknown)

;;; export-db-lookup : ExportDB Symbol -> (known (List Symbol)) | unknown
;;; Look up a module's exports. Returns (known (sym ...)) or 'unknown.
(doc export-db-lookup 'export #t)
(doc export-db-lookup 'type '(-> ExportDB Symbol (Union (Pair known (List Symbol)) unknown)))
(define (export-db-lookup db mod-name)
  (let ([entry (assq mod-name db)])
    (if entry (cdr entry) 'unknown)))

;;; export-db-symbols : ExportDB Symbol -> (List Symbol)
;;; Convenience: get just the symbol list (empty if unknown or no exports).
(doc export-db-symbols 'export #t)
(define (export-db-symbols db mod-name)
  (let ([result (export-db-lookup db mod-name)])
    (if (eq? result 'unknown)
        '()
        (cadr result))))

;;; ============================================================================
;;; Top-level define extraction
;;; ============================================================================

;;; extract-top-defines : (List SExp) -> (List Symbol)
;;; Extract all top-level define names as fallback when no export annotations.
(define (extract-top-defines sexps)
  (let loop ([forms sexps] [acc '()])
    (if (null? forms)
        (reverse acc)
        (let ([form (car forms)])
          (loop (cdr forms)
                (cond
                  [(and (pair? form) (eq? (car form) 'define)
                        (pair? (cdr form)))
                   (let ([second (cadr form)])
                     (cond
                       [(pair? second) (cons (car second) acc)]
                       [(symbol? second) (cons second acc)]
                       [else acc]))]
                  [(and (pair? form) (eq? (car form) 'define-syntax)
                        (pair? (cdr form)) (symbol? (cadr form)))
                   (cons (cadr form) acc)]
                  ;; Recurse into top-level begin
                  [(and (pair? form) (eq? (car form) 'begin))
                   (append (reverse (extract-top-defines (cdr form))) acc)]
                  [else acc]))))))

;;; ============================================================================
;;; Analyzer
;;; ============================================================================

;;; make-export-analyzer : -> Analyzer
;;; Creates an analyzer that collects exports from each module.
;;; Uses doc 'export annotations when present, falls back to all top-level defines.
(doc make-export-analyzer 'export #t)
(doc make-export-analyzer 'type '(-> Analyzer))
(define (make-export-analyzer)
  (make-analyzer
   'exports
   ;; init: empty alist
   (lambda () '())
   ;; step: extract exports, cons onto alist
   (lambda (mr acc state)
     (let* ([sexps (module-record-sexps mr)]
            [name (module-record-name mr)]
            ;; Try export annotations first
            [annotated (extract-exports-from-sexps sexps)]
            [targeted (find-targeted-exports sexps)]
            [all-annotated (unique (append annotated targeted))])
       ;; Use annotated exports if any found, otherwise fall back to all defines
       (let ([exports (if (pair? all-annotated)
                          all-annotated
                          (extract-top-defines sexps))])
         (cons (cons name (list 'known exports))
               state))))))
