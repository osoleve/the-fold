;;; lattice/meta/analyzer-unused.ss — Unused-requires analyzer
;;; @module meta/analyzer-unused
;;; @requires prelude meta/lattice-walk meta/analyzer-exports meta/unused-requires

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'meta/lattice-walk)
(require 'meta/analyzer-exports)
(require 'meta/unused-requires)

(doc 'module 'meta/analyzer-unused)
(doc 'description "Unused-requires analyzer for the lattice walker.
Wraps the existing analyze-file-requires with the topsort-built export DB,
eliminating 'uncertain results that occur when exports are unknown.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Analyzer
;;; ============================================================================

;;; make-unused-requires-analyzer : -> Analyzer
;;; Creates an analyzer that detects unused requires using the accumulated export DB.
;;; Must run after the export analyzer (exports must be in AccState).
(doc make-unused-requires-analyzer 'export #t)
(doc make-unused-requires-analyzer 'type '(-> Analyzer))
(define (make-unused-requires-analyzer)
  (make-analyzer
   'unused
   ;; init: empty results alist
   (lambda () '())
   ;; step: analyze this module's requires using the accumulated export DB
   (lambda (mr acc state)
     (let* ([sexps (module-record-sexps mr)]
            [name (module-record-name mr)]
            ;; Get the export DB from the accumulator (populated by export analyzer)
            [export-db (acc-ref acc 'exports)]
            ;; Build a lookup function that bridges the tri-state export DB
            ;; to the (Symbol -> (List Symbol)) interface of analyze-file-requires
            [export-lookup
             (lambda (mod-name)
               (if (not export-db)
                   '()  ; No export analyzer ran — fall back to empty
                   (let ([entry (export-db-lookup export-db mod-name)])
                     (cond
                       ;; Module analyzed, has exports
                       [(and (pair? entry) (eq? (car entry) 'known))
                        (cadr entry)]
                       ;; Module not in our analysis set (core module, etc.)
                       ;; Return empty → classified as 'uncertain
                       [else '()]))))]
            ;; Run the existing analysis
            [result (analyze-file-requires sexps export-lookup)])
       (cons (cons name result) state)))))
