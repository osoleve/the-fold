;;; lattice/meta/analyzer-coverage.ss — Export utilization analyzer
;;; @module meta/analyzer-coverage
;;; @requires prelude meta/lattice-walk meta/analyzer-exports meta/unused-requires

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'meta/lattice-walk)
(require 'meta/analyzer-exports)
(require 'meta/unused-requires)

(doc 'module 'meta/analyzer-coverage)
(doc 'description "Export utilization analyzer for the lattice walker.
Tracks which exported symbols are referenced by downstream modules.
Identifies dead exports — symbols exported by a module but never
referenced by any consumer. The static analog of test coverage:
if no downstream module uses an export, it may be dead code.
Must run after the export analyzer.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Analyzer
;;; ============================================================================

(doc 'section 'analyzer)

;;; make-coverage-analyzer : -> Analyzer
;;; Creates an export utilization analyzer.
;;;
;;; State: eq-hashtable (module-name -> eq-hashtable (symbol -> Nat))
;;; For each module that provides exports, tracks how many downstream
;;; modules reference each symbol.
;;;
;;; Must run after the export analyzer (reads 'exports from AccState).
(doc make-coverage-analyzer 'export #t)
(doc make-coverage-analyzer 'type '(-> Analyzer))
(define (make-coverage-analyzer)
  (make-analyzer
   'coverage
   ;; init: empty outer hashtable
   (lambda () (make-eq-hashtable))
   ;; step: check which dep exports this module references
   (lambda (mr acc state)
     (let* ([sexps (module-record-sexps mr)]
            [deps (module-record-deps mr)]
            [export-db (acc-ref acc 'exports)]
            [referenced-syms (collect-file-symbols sexps)])
       (when export-db
         (for-each
          (lambda (dep)
            (let ([dep-exports (export-db-symbols export-db dep)])
              (when (pair? dep-exports)
                ;; Ensure inner hashtable exists for this dep
                (unless (hashtable-ref state dep #f)
                  (hashtable-set! state dep (make-eq-hashtable)))
                (let ([inner (hashtable-ref state dep #f)])
                  (for-each
                   (lambda (sym)
                     (when (memq sym referenced-syms)
                       (hashtable-set! inner sym
                                       (+ 1 (hashtable-ref inner sym 0)))))
                   dep-exports)))))
          deps))
       state))))

;;; ============================================================================
;;; Query Helpers
;;; ============================================================================

(doc 'section 'queries)

;;; coverage-unused-exports : Hashtable ExportDB Symbol -> (List Symbol)
;;; Get exports from a module that are never referenced by any downstream module.
(doc coverage-unused-exports 'export #t)
(doc coverage-unused-exports 'type '(-> Hashtable ExportDB Symbol (List Symbol)))
(define (coverage-unused-exports coverage-db export-db module-name)
  (let* ([all-exports (export-db-symbols export-db module-name)]
         [usage (hashtable-ref coverage-db module-name #f)])
    (if (not usage)
        all-exports  ; no downstream module references anything
        (filter (lambda (sym) (zero? (hashtable-ref usage sym 0)))
                all-exports))))

;;; coverage-module-stats : Hashtable ExportDB Symbol -> (Pair Nat Nat)
;;; Get (used-count . total-count) for a module's exports.
(doc coverage-module-stats 'export #t)
(doc coverage-module-stats 'type '(-> Hashtable ExportDB Symbol (Pair Nat Nat)))
(define (coverage-module-stats coverage-db export-db module-name)
  (let* ([all-exports (export-db-symbols export-db module-name)]
         [total (length all-exports)]
         [usage (hashtable-ref coverage-db module-name #f)]
         [used (if (not usage)
                   0
                   (length (filter (lambda (sym)
                                     (> (hashtable-ref usage sym 0) 0))
                                   all-exports)))])
    (cons used total)))
