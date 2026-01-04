;;; core/lang/module.ss — Module System for The Fold
;;; @module module
;;; @requires prelude
;;;
;;; Provides a module loader that:
;;;   - Tracks loaded modules to avoid reloading
;;;   - Automatically loads dependencies in order
;;;   - Parses @module/@requires annotations from file headers
;;;   - Records load times for performance metrics
;;;   - Provides discovery functions for LLMs and users
;;;
;;; Usage:
;;;   (require 'compile)        ; Load module and its dependencies
;;;   (require 'eval 'infer)    ; Load multiple modules
;;;   (modules)                 ; List all registered modules
;;;   (module-info 'eval)       ; Show module details (deps, path, status)
;;;   (module-stats)            ; Show load times
;;;   (module-deps 'eval)       ; Show dependencies of a module
;;;
;;; Module Header Format (at top of .ss files):
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
;;; Module Path Registry
;;; ============================================================

;;; *module-paths* : Hashtable Symbol → String
;;; Maps module names to file paths (includes both pre-registered and discovered)
(define *module-paths* (make-eq-hashtable))

;;; *header-cache* : Hashtable String → (name . deps) | #f
;;; Cache for parsed headers (keyed by file path)
(define *header-cache* (make-hashtable string-hash string=?))

;;; *module-search-dirs* : (List String)
;;; Directories to search when resolving module names
(define *module-search-dirs*
  '(;; Core directories
    "core/base" "core/blocks" "core/lang" "core/types" "core/data"
    "core/query" "core/util" "core/linalg" "core/numeric" "core/autodiff"
    "core/random" "core/pipeline" "core/info-theory"
    ;; FP subdirectories
    "core/fp" "core/fp/control" "core/fp/numeric" "core/fp/parsing"
    "core/fp/meta" "core/fp/data" "core/fp/game" "core/fp/symbolic"
    "core/fp/measure" "core/fp/control-systems"
    ;; Shell directories
    "shell" "shell/tests"))

;;; *header-scan-limit* : Nat
;;; Number of lines to scan for header annotations
(define *header-scan-limit* 60)

;;; register-module-path! : Symbol × String → void
;;; Register a module's file path.
(define (register-module-path! name path)
  (hashtable-set! *module-paths* name path))

;;; Initialize known module paths (core modules)
(begin
 ;; BASE layer
 (register-module-path! 'prelude "core/base/prelude.ss")
 (register-module-path! 'sha256 "core/base/sha256.ss")
 (register-module-path! 'error "core/base/error.ss")
 
 ;; Block layer
 (register-module-path! 'block "core/blocks/block.ss")
 (register-module-path! 'cas "core/blocks/cas.ss")
 (register-module-path! 'normalize "core/blocks/normalize.ss")
 (register-module-path! 'expand "core/blocks/expand.ss")
 
 ;; Lang layer
 (register-module-path! 'parse "core/lang/parse.ss")
 (register-module-path! 'span "core/lang/span.ss")
 (register-module-path! 'fold-parse "core/lang/fold-parse.ss")
 (register-module-path! 'prim "core/lang/prim.ss")
 (register-module-path! 'eval "core/lang/eval.ss")
 (register-module-path! 'compile "core/lang/compile.ss")
 (register-module-path! 'module "core/lang/module.ss")
 (register-module-path! 'nbe "core/lang/nbe.ss")
 
 ;; Types layer
 (register-module-path! 'types "core/types/types.ss")
 (register-module-path! 'kinds "core/types/kinds.ss")
 (register-module-path! 'infer "core/types/infer.ss")
 (register-module-path! 'resolve "core/types/resolve.ss")
 (register-module-path! 'annotate "core/types/annotate.ss")
 (register-module-path! 'dep-types "core/types/dep-types.ss")
 
 ;; Query layer
 (register-module-path! 'query "core/query/query.ss")
 (register-module-path! 'query-dsl "core/query/query-dsl.ss")
 
 ;; Data layer
 (register-module-path! 'data-structures "core/data/data-structures.ss")
 (register-module-path! 'collection-utils "core/data/collection-utils.ss")
 (register-module-path! 'graph-algorithms "core/data/graph-algorithms.ss")
 
 ;; Linalg layer
 (register-module-path! 'vec "core/linalg/vec.ss")
 (register-module-path! 'matrix "core/linalg/matrix.ss")
 (register-module-path! 'matrix-decomp "core/linalg/matrix-decomp.ss")
 (register-module-path! 'matrix-solvers "core/linalg/matrix-solvers.ss")
 (register-module-path! 'sparse "core/linalg/sparse.ss")
 
 ;; Numeric layer
 (register-module-path! 'complex "core/numeric/complex.ss")
 (register-module-path! 'dft "core/numeric/dft.ss")
 (register-module-path! 'convolution "core/numeric/convolution.ss")
 
 ;; FP layers
 (register-module-path! 'transcendental "core/fp/numeric/transcendental.ss")
 (register-module-path! 'monad "core/fp/control/monad.ss")
 (register-module-path! 'parser-combinators "core/fp/parsing/parser-combinators.ss"))

;;; clear-module-caches! : → void
;;; Clear header cache (useful after file modifications).
(define (clear-module-caches!)
  (hashtable-clear! *header-cache*))

;;; ============================================================
;;; Header Parsing
;;; ============================================================

;;; read-header-lines : String × Nat → (List String) | #f
;;; Read first n lines from file for header parsing. Returns #f if file doesn't exist.
(define (read-header-lines filepath n)
  (guard (exn [else #f])
         (call-with-input-file filepath
                               (lambda (port)
                                       (let loop ([i 0] [lines '()])
                                            (if (>= i n)
                                                (reverse lines)
                                                (let ([line (get-line port)])
                                                     (if (eof-object? line)
                                                         (reverse lines)
                                                         (loop (+ i 1) (cons line lines))))))))))

;;; extract-annotation : String × String → String | #f
;;; Extract value from ";;; @key value" line.
(define (extract-annotation line prefix)
  (let ([trimmed (string-trim line)])
       (and (string-starts-with? trimmed ";;;")
            (let ([after-comment (string-trim (substring trimmed 3 (string-length trimmed)))])
                 (and (string-starts-with? after-comment prefix)
                      (string-trim (substring after-comment
                                              (string-length prefix)
                                              (string-length after-comment))))))))

;;; parse-module-name : (List String) → Symbol | #f
;;; Extract module name from @module annotation.
(define (parse-module-name lines)
  (let loop ([lines lines])
       (cond
        [(null? lines) #f]
        [else
         (let ([name (extract-annotation (car lines) "@module ")])
              (if (and name (> (string-length name) 0))
                  (string->symbol name)
                  (loop (cdr lines))))])))

;;; parse-requires : (List String) → (List Symbol)
;;; Extract dependencies from ALL @requires annotations.
;;; Supports multiple @requires lines and accumulates all deps.
(define (parse-requires lines)
  (let loop ([lines lines] [acc '()])
       (cond
        [(null? lines) (reverse acc)]
        [else
         (let ([deps (extract-annotation (car lines) "@requires ")])
              (if deps
                  (let ([parsed (filter (lambda (s) (> (string-length (symbol->string s)) 0))
                                        (map string->symbol
                                             (filter (lambda (s) (> (string-length s) 0))
                                                     (string-split deps #\space))))])
                       (loop (cdr lines) (append (reverse parsed) acc)))
                  (loop (cdr lines) acc)))])))

;;; parse-module-header : String → (name . deps) | #f
;;; Parse @module/@requires from file. Returns (name . deps) or #f.
;;; Only caches successful parses (not file-not-found failures).
(define (parse-module-header filepath)
  ;; Check cache first
  (let ([cached (hashtable-ref *header-cache* filepath 'not-found)])
       (if (not (eq? cached 'not-found))
           cached
           (let* ([lines (read-header-lines filepath *header-scan-limit*)]
                  [result (and lines
                               (let ([name (parse-module-name lines)])
                                    (and name
                                         (cons name (parse-requires lines)))))])
                 ;; Only cache successful parses (don't cache failures)
                 (when result
                       (hashtable-set! *header-cache* filepath result))
                 result))))

;;; ============================================================
;;; Path Resolution
;;; ============================================================

;;; find-module-path : Symbol → String | #f
;;; Find file path for a module by searching known locations.
;;; Searches core/, shell/, and all subdirectories in *module-search-dirs*.
(define (find-module-path name)
  (let ([name-str (symbol->string name)])
       ;; Search all registered directories
       (let loop ([dirs *module-search-dirs*])
            (if (null? dirs)
                #f
                (let ([path (string-append (car dirs) "/" name-str ".ss")])
                     (if (file-exists? path)
                         path
                         (loop (cdr dirs))))))))

;;; module-name->path : Symbol → String | #f
;;; Get file path for module, using registry or searching.
;;; Discovered modules are registered in *module-paths* so they appear in (modules).
(define (module-name->path name)
  ;; Check explicit registry first
  (or (hashtable-ref *module-paths* name #f)
      ;; Then search and register if found
      (let ([found (find-module-path name)])
           (when found
                 ;; Register in *module-paths* so it appears in (modules) listing
                 (hashtable-set! *module-paths* name found))
           found)))

;;; ============================================================
;;; Auto-Registration from Headers
;;; ============================================================

;;; auto-register-module! : Symbol → void
;;; Parse module header and register dependencies if not already known.
;;; Uses the requested name (not the header's @module name) as the key.
(define (auto-register-module! name)
  (unless (hashtable-contains? *module-deps* name)
          (let ([path (module-name->path name)])
               (when path
                     (let ([header (parse-module-header path)])
                          (when header
                                ;; Register under the requested name, using deps from header
                                (hashtable-set! *module-deps* name (cdr header))))))))

;;; ============================================================
;;; Bootstrap Dependencies
;;; ============================================================

;;; prelude is the foundation - it has no dependencies
;;; All other modules now declare deps via @requires headers
(hashtable-set! *module-deps* 'prelude '())

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
          (let* ([path (or (module-name->path name)
                           (string-append (symbol->string name) ".ss"))]
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
;;; Auto-registers dependencies from file headers if not already known.
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
   ;; Not loaded yet - auto-register, load dependencies, then this module
   [else
    ;; Try to discover dependencies from header if not known
    (auto-register-module! name)
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
;;; Uses visited tracking to prevent infinite loops on circular dependencies.
(define (all-deps name)
  (all-deps-helper name '()))

;;; all-deps-helper : Symbol × (List Symbol) → (List Symbol)
;;; Helper with visited tracking for cycle prevention.
(define (all-deps-helper name visited)
  (if (memq name visited)
      '()  ; Cycle detected, stop recursion
      (let ([direct (module-deps name)])
           (if (null? direct)
               '()
               (unique (append direct
                               (apply append
                                      (map (lambda (d)
                                                   (all-deps-helper d (cons name visited)))
                                           direct))))))))

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

;;; ============================================================
;;; Module Discovery (LLM-Friendly)
;;; ============================================================

;;; extract-category : String → String
;;; Extract category from path like "core/base/foo.ss" → "BASE"
(define (extract-category path)
  (cond
   [(string-starts-with? path "core/base/") "BASE"]
   [(string-starts-with? path "core/blocks/") "BLOCKS"]
   [(string-starts-with? path "core/lang/") "LANG"]
   [(string-starts-with? path "core/types/") "TYPES"]
   [(string-starts-with? path "core/query/") "QUERY"]
   [(string-starts-with? path "core/data/") "DATA"]
   [(string-starts-with? path "core/linalg/") "LINALG"]
   [(string-starts-with? path "core/numeric/") "NUMERIC"]
   [(string-starts-with? path "core/autodiff/") "AUTODIFF"]
   [(string-starts-with? path "core/random/") "RANDOM"]
   [(string-starts-with? path "core/pipeline/") "PIPELINE"]
   [(string-starts-with? path "core/info-theory/") "INFO-THEORY"]
   [(string-starts-with? path "core/util/") "UTIL"]
   [(string-starts-with? path "core/fp/") "FP"]
   [(string-starts-with? path "shell/") "SHELL"]
   [else "OTHER"]))

;;; category-description : String → String
;;; Return description for category.
(define (category-description cat)
  (cond
   [(string=? cat "BASE") "foundation, no dependencies"]
   [(string=? cat "BLOCKS") "content-addressed storage"]
   [(string=? cat "LANG") "evaluation, compilation"]
   [(string=? cat "TYPES") "type system"]
   [(string=? cat "QUERY") "pattern matching, search"]
   [(string=? cat "DATA") "data structures"]
   [(string=? cat "LINALG") "linear algebra"]
   [(string=? cat "NUMERIC") "numerical computing"]
   [(string=? cat "AUTODIFF") "automatic differentiation"]
   [(string=? cat "RANDOM") "randomness, probability"]
   [(string=? cat "PIPELINE") "agent pipelines"]
   [(string=? cat "INFO-THEORY") "information theory"]
   [(string=? cat "UTIL") "utilities"]
   [(string=? cat "FP") "functional programming toolkit"]
   [(string=? cat "SHELL") "shell, REPL, IO"]
   [else ""]))

;;; group-modules-by-category : → Hashtable String → (List Symbol)
;;; Group all registered modules by their category.
(define (group-modules-by-category)
  (let ([groups (make-hashtable string-hash string=?)]
        [modules (vector->list (hashtable-keys *module-paths*))])
       (for-each
        (lambda (mod)
                (let* ([path (hashtable-ref *module-paths* mod "")]
                       [cat (extract-category path)]
                       [existing (hashtable-ref groups cat '())])
                      (hashtable-set! groups cat (cons mod existing))))
        modules)
       groups))

;;; modules : → void
;;; List all registered modules grouped by category.
;;; Dynamically builds listing from *module-paths* registry.
(define (modules)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                    AVAILABLE MODULES                               │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  
  (let* ([groups (group-modules-by-category)]
         [categories '("BASE" "BLOCKS" "LANG" "TYPES" "QUERY" "DATA"
                       "LINALG" "NUMERIC" "AUTODIFF" "RANDOM" "PIPELINE"
                       "INFO-THEORY" "UTIL" "FP" "SHELL" "OTHER")])
        
        (for-each
         (lambda (cat)
                 (let ([mods (hashtable-ref groups cat '())])
                      (unless (null? mods)
                              (let ([desc (category-description cat)]
                                    [sorted-mods (sort (lambda (a b)
                                                               (string<? (symbol->string a)
                                                                         (symbol->string b)))
                                                       mods)])
                                   (display (format "  ~a~a:\n"
                                                    cat
                                                    (if (string=? desc "")
                                                        ""
                                                        (string-append " (" desc ")"))))
                                   (display "    ")
                                   (display (apply string-append
                                                   (map (lambda (m)
                                                                (string-append (symbol->string m) " "))
                                                        sorted-mods)))
                                   (display "\n\n")))))
         categories)
        
        (display "  Usage: (require 'module-name) to load a module\n")
        (display "         (module-info 'module-name) for details\n")
        (display "         (module-stats) for load times\n\n")))

;;; module-info : Symbol → void
;;; Show detailed information about a module.
;;; Useful for understanding dependencies before loading.
(define (module-info name)
  (display "\n")
  (display (format "  Module: ~a\n" name))
  (display "  ────────────────────────────────────────────────────────\n")
  
  ;; Path
  (let ([path (module-name->path name)])
       (if path
           (display (format "  Path: ~a\n" path))
           (display "  Path: (not registered)\n")))
  
  ;; Load status
  (display (format "  Status: ~a\n" (if (module-loaded? name) "LOADED" "not loaded")))
  
  ;; Load time
  (let ([time (module-load-time name)])
       (when time
             (display (format "  Load time: ~ams\n" time))))
  
  ;; Dependencies
  (let ([deps (hashtable-ref *module-deps* name 'not-found)])
       (cond
        [(eq? deps 'not-found)
         ;; Try to discover from header
         (let ([path (module-name->path name)])
              (if path
                  (let ([header (parse-module-header path)])
                       (if header
                           (begin
                            (display (format "  Dependencies: ~a (from header)\n"
                                             (if (null? (cdr header)) "(none)" (cdr header)))))
                           (display "  Dependencies: (unknown - no header annotations)\n")))
                  (display "  Dependencies: (unknown - module not found)\n")))]
        [(null? deps)
         (display "  Dependencies: (none)\n")]
        [else
         (display (format "  Dependencies: ~a\n" deps))]))
  
  ;; Transitive dependencies
  (let ([all (all-deps name)])
       (unless (null? all)
               (display (format "  Transitive: ~a\n" all))))
  
  (display "\n"))

;;; list-registered-modules : → (List Symbol)
;;; Return a list of all registered module names.
(define (list-registered-modules)
  (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
        (vector->list (hashtable-keys *module-paths*))))
