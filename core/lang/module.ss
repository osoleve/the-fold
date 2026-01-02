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

(load "core/base/prelude.ss")

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

;;; *loading-stack* : List Symbol
;;; Stack of modules currently being loaded (for circular dependency detection)
(define *loading-stack* '())

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

;;; format-cycle : Symbol × (List Symbol) → String
;;; Format a circular dependency chain for error message.
(define (format-cycle name stack)
  (let* ([cycle-start (member name stack)]
         [cycle (if cycle-start
                    (reverse (cons name cycle-start))
                    (reverse (cons name stack)))])
        (apply string-append
               (cons (symbol->string (car cycle))
                     (map (lambda (m) (string-append " -> " (symbol->string m)))
                          (cdr cycle))))))

;;; require-one : Symbol → void
;;; Load a module and all its dependencies.
;;; Detects circular dependencies and raises an error with the cycle path.
(define (require-one name)
  (cond
   ;; Already loaded - nothing to do
   [(module-loaded? name) (void)]
   ;; Currently loading - circular dependency detected!
   [(memq name *loading-stack*)
    (error 'require-one
           (string-append "Circular dependency detected: "
                          (format-cycle name *loading-stack*)))]
   ;; Not loaded yet - load dependencies, then this module
   [else
    (let ([deps (hashtable-ref *module-deps* name '())])
         ;; Push onto loading stack
         (set! *loading-stack* (cons name *loading-stack*))
         ;; Load dependencies first (may raise circular dependency error)
         (for-each require-one deps)
         ;; Then load this module
         (load-module! name)
         ;; Pop from loading stack
         (set! *loading-stack* (cdr *loading-stack*)))]))

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

;;; loading-stack : → (List Symbol)
;;; Get the current loading stack (modules in progress).
(define (loading-stack)
  *loading-stack*)

;;; detect-cycle : Symbol → (List Symbol) | #f
;;; Check if loading a module would create a circular dependency.
;;; Returns the cycle path if a cycle exists, #f otherwise.
(define (detect-cycle name)
  (detect-cycle-helper name '()))

;;; detect-cycle-helper : Symbol × (List Symbol) → (List Symbol) | #f
(define (detect-cycle-helper name visited)
  (cond
   [(memq name visited)
    ;; Found a cycle - return the path
    (reverse (cons name (member name (reverse visited))))]
   [(module-loaded? name) #f]
   [else
    (let ([deps (module-deps name)])
         (let loop ([remaining deps])
              (cond
               [(null? remaining) #f]
               [else
                (let ([result (detect-cycle-helper (car remaining)
                                                   (cons name visited))])
                     (if result
                         result
                         (loop (cdr remaining))))])))]))

;;; validate-deps : → (List (module . cycle))
;;; Check all registered modules for circular dependencies.
;;; Returns a list of (module . cycle-path) pairs for any cycles found.
(define (validate-deps)
  (let ([modules (vector->list (hashtable-keys *module-deps*))])
       (filter (lambda (x) x)
               (map (lambda (m)
                            (let ([cycle (detect-cycle m)])
                                 (if cycle
                                     (cons m cycle)
                                     #f)))
                    modules))))

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
