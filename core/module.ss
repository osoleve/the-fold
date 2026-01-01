;;; fabric/stitches/module.ss — Simple Module System for The Fold
;;;
;;; Provides a module loader that:
;;;   - Tracks loaded modules to avoid reloading
;;;   - Automatically loads dependencies in order
;;;   - Records load times for performance metrics
;;;
;;; Usage:
;;;   (require 'compile)        ; Load core/compile.ss and dependencies
;;;   (require 'eval 'infer)    ; Load multiple modules
;;;   (module-stats)            ; Show load times
;;;   (module-deps 'eval)       ; Show dependencies of a module
;;;
;;; Module Manifest (at top of .ss files):
;;;   ;;; @module eval
;;;   ;;; @requires prelude block prim
;;;
;;; Dependencies:
;;;   - prelude.ss (must be loaded first manually)

(load "core/prelude.ss")

;;; ============================================================
;;; Module Registry
;;; ============================================================

;;; *module-registry* : Hashtable Symbol → (loaded? load-time-ms)
(define *module-registry* (make-eq-hashtable))

;;; *module-deps* : Hashtable Symbol → (List Symbol)
;;; Declared dependencies for each module
(define *module-deps* (make-eq-hashtable))

;;; *load-order* : List Symbol
;;; Order in which modules were loaded (for diagnostics)
(define *load-order* '(prelude))

;;; Register prelude as already loaded (we loaded it above)
(hashtable-set! *module-registry* 'prelude (cons #t 0))

;;; ============================================================
;;; Dependency Declarations
;;; ============================================================

;;; Known dependencies (hardcoded for now, could parse from files)
(begin
 ;; Layer 0: Foundation
 (hashtable-set! *module-deps* 'prelude '())
 (hashtable-set! *module-deps* 'sha256 '(prelude))
 
 ;; Layer 1: Block System
 (hashtable-set! *module-deps* 'block '(prelude))
 (hashtable-set! *module-deps* 'cas '(prelude block sha256))
 
 ;; Layer 2: Language Core
 (hashtable-set! *module-deps* 'parse '(prelude))
 (hashtable-set! *module-deps* 'span '(prelude parse))
 (hashtable-set! *module-deps* 'fold-parse '(prelude span))
 (hashtable-set! *module-deps* 'normalize '(prelude))
 (hashtable-set! *module-deps* 'expand '(prelude))
 (hashtable-set! *module-deps* 'prim '(prelude))
 
 ;; Layer 3: Type System
 (hashtable-set! *module-deps* 'types '(prelude))
 (hashtable-set! *module-deps* 'kinds '(prelude types))
 
 ;; Layer 4: Type Inference
 (hashtable-set! *module-deps* 'infer '(prelude types kinds))
 (hashtable-set! *module-deps* 'resolve '(prelude types kinds))
 (hashtable-set! *module-deps* 'annotate '(prelude types kinds infer))
 
 ;; Layer 5: Evaluation
 (hashtable-set! *module-deps* 'eval '(prelude block prim))
 
 ;; Layer 6: Compilation
 (hashtable-set! *module-deps* 'compile '(prelude parse span fold-parse normalize expand types infer eval))
 
 ;; Layer 7: Error System
 (hashtable-set! *module-deps* 'error '(prelude span)))

;;; ============================================================
;;; Module Loading
;;; ============================================================

;;; current-time-ms : → Nat
(define (current-time-ms)
  (let ([t (current-time)])
       (+ (* (time-second t) 1000)
          (quotient (time-nanosecond t) 1000000))))

;;; module-loaded? : Symbol → Boolean
(define (module-loaded? name)
  (let ([entry (hashtable-ref *module-registry* name #f)])
       (and entry (car entry))))

;;; module-load-time : Symbol → Nat | #f
(define (module-load-time name)
  (let ([entry (hashtable-ref *module-registry* name #f)])
       (and entry (cdr entry))))

;;; load-module! : Symbol → void
;;; Load a single module (without dependencies).
(define (load-module! name)
  (unless (module-loaded? name)
          (let* ([path (string-append (symbol->string name) ".ss")]
                 [start (current-time-ms)])
                (load path)
                (let ([duration (- (current-time-ms) start)])
                     (hashtable-set! *module-registry* name (cons #t duration))
                     (set! *load-order* (cons name *load-order*))))))

;;; require-one : Symbol → void
;;; Load a module and all its dependencies.
(define (require-one name)
  (unless (module-loaded? name)
          (let ([deps (hashtable-ref *module-deps* name '())])
               ;; Load dependencies first
               (for-each require-one deps)
               ;; Then load this module
               (load-module! name))))

;;; require : Symbol ... → void
;;; Load one or more modules with their dependencies.
(define (require . names)
  (for-each require-one names))

;;; ============================================================
;;; Module Information
;;; ============================================================

;;; module-deps : Symbol → (List Symbol)
;;; Get declared dependencies for a module.
(define (module-deps name)
  (hashtable-ref *module-deps* name '()))

;;; all-deps : Symbol → (List Symbol)
;;; Get all transitive dependencies for a module.
(define (all-deps name)
  (let ([direct (module-deps name)])
       (if (null? direct)
           '()
           (unique (append direct (apply append (map all-deps direct)))))))

;;; module-stats : → void
;;; Display loading statistics.
(define (module-stats)
  (display "
╔════════════════════════════════════════════════════════════╗
")
  (display "║                    MODULE STATISTICS                        ║
")
  (display "╚════════════════════════════════════════════════════════════╝

")
  
  (let* ([loaded (reverse *load-order*)]
         [total-time (fold-left + 0 (map (lambda (m) (or (module-load-time m) 0)) loaded))])
        
        (display "  Loaded modules:
")
        (for-each
         (lambda (m)
                 (let ([time (module-load-time m)])
                      (display (format "    ~a~a~ams
"
                                       m
                                       (make-string (max 1 (- 20 (string-length (symbol->string m)))) #\space)
                                       (or time 0)))))
         loaded)
        
        (newline)
        (display (format "  Total: ~a modules, ~ams

" (length loaded) total-time))))

;;; module-graph : → void
;;; Display dependency graph.
(define (module-graph)
  (display "
╔════════════════════════════════════════════════════════════╗
")
  (display "║                   DEPENDENCY GRAPH                          ║
")
  (display "╚════════════════════════════════════════════════════════════╝

")
  
  (let ([modules (vector->list (hashtable-keys *module-deps*))])
       (for-each
        (lambda (m)
                (let ([deps (module-deps m)])
                     (display (format "  ~a → ~a
" m
                                      (if (null? deps) "(none)" (apply string-append
                                                                       (map (lambda (d) (string-append (symbol->string d) " ")) deps)))))))
        (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) modules))))

;;; ============================================================
;;; Convenience
;;; ============================================================

;;; require-core : → void
;;; Load all core modules.
(define (require-core)
  (require 'compile 'error 'annotate))

;;; require-all : → void
;;; Load everything.
(define (require-all)
  (let ([all-modules (vector->list (hashtable-keys *module-deps*))])
       (for-each require-one all-modules)))
