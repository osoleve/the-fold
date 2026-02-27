;;; @module module-display
;;; @description Module discovery, listing, and export inspection for REPL use.
;;; Loaded by module.ss — requires module state tables to be defined first.

;;; ====
;;; Module Discovery (LLM-Friendly)
;;; ====

;;; extract-category : String → String
;;; Extract category from path like "core/base/foo.ss" → "BASE"
(define (extract-category path)
  (cond
   [(string-starts-with? path "core/base/") "BASE"]
   [(string-starts-with? path "core/blocks/") "BLOCKS"]
   [(string-starts-with? path "core/lang/") "LANG"]
   [(string-starts-with? path "core/types/") "TYPES"]
   [(string-starts-with? path "lattice/query/") "QUERY"]
   [(string-starts-with? path "lattice/data/") "DATA"]
   [(string-starts-with? path "lattice/linalg/") "LINALG"]
   [(string-starts-with? path "lattice/numeric/") "NUMERIC"]
   [(string-starts-with? path "lattice/autodiff/") "AUTODIFF"]
   [(string-starts-with? path "lattice/random/") "RANDOM"]
   [(string-starts-with? path "lattice/pipeline/") "PIPELINE"]
   [(string-starts-with? path "lattice/info/") "INFO-THEORY"]
   [(string-starts-with? path "core/util/") "UTIL"]
   [(string-starts-with? path "lattice/symbolic/") "SYMBOLIC"]
   [(string-starts-with? path "lattice/rewrite/") "REWRITE"]
   [(string-starts-with? path "lattice/category/") "CATEGORY"]
   [(string-starts-with? path "lattice/fp/") "FP"]
   [(string-starts-with? path "boundary/") "BOUNDARY"]
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
   [(string=? cat "SYMBOLIC") "symbolic computation, CAS"]
   [(string=? cat "REWRITE") "term rewriting, proof tactics"]
   [(string=? cat "CATEGORY") "category theory"]
   [(string=? cat "FP") "functional programming toolkit"]
   [(string=? cat "BOUNDARY") "boundary, REPL, IO"]
   [else ""]))

;;; group-modules-by-category : Unit → (Hashtable String (List Symbol))
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

;;; modules : Unit → Void
;;; List all registered modules grouped by category.
;;; Dynamically builds listing from *module-paths* registry.
(define (modules)
  (display "\n")
  (display "  --------------- AVAILABLE MODULES ---------------\n")
  (display "\n")
  
  (let* ([groups (group-modules-by-category)]
         [categories '("BASE" "BLOCKS" "LANG" "TYPES" "QUERY" "DATA"
                       "LINALG" "NUMERIC" "AUTODIFF" "RANDOM" "PIPELINE"
                       "INFO-THEORY" "UTIL" "SYMBOLIC" "REWRITE" "CATEGORY"
                       "FP" "BOUNDARY" "OTHER")])
        
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
        (display "         (require 'dir/module) for namespaced (avoids collisions)\n")
        (display "         (module-info 'module-name) for details\n")
        (display "         (module-stats) for load times\n")
        (display (format "\n  Full lattice discovery (~a registered modules):\n"
                         (hashtable-size *module-paths*)))
        (display "         (load \"lattice/meta/meta.ss\") then (lattice-init!)\n")
        (display "         (lf \"query\")  search functions   (li 'skill)  skill info\n")
        (display "         (le 'skill)  list exports       (lh)  health check\n\n")))

;;; module-info : Symbol → Void
;;; Show detailed information about a module.
;;; Useful for understanding dependencies before loading.
(define (module-info name)
  (display "\n")
  (display (format "  Module: ~a\n" name))
  (display "  --------------------------------------------------------\n")

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

;;; module-runtime-syms : Symbol → (List Symbol)
;;; Get all runtime symbols matching module prefix from oblist.
(define (module-runtime-syms name)
  (let* ([prefix (string-append (symbol->string name) "-")]
         [prefix-len (string-length prefix)])
    (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
          (filter (lambda (s)
                    (let ([str (symbol->string s)])
                      (and (>= (string-length str) prefix-len)
                           (string=? (substring str 0 prefix-len) prefix)
                           (top-level-bound? s))))
                  (oblist)))))

;;; module-runtime-extras : Symbol × (List Symbol) → (List Symbol)
;;; Get runtime symbols not in the source-level set.
(define (module-runtime-extras name source-names)
  (let ([runtime (module-runtime-syms name)])
    (filter (lambda (s) (not (memq s source-names))) runtime)))

;;; module-exports-runtime : Symbol → Void
;;; Show only runtime symbols for a module.
(define (module-exports-runtime name)
  (if (not (module-loaded? name))
      (display (format "  Module '~a' not loaded. Use (require '~a) first.\n" name name))
      (let ([syms (module-runtime-syms name)])
        (display (format "\n  Runtime exports of '~a' (~a symbols):\n" name (length syms)))
        (display "  --------------------------------------------------------\n")
        (for-each (lambda (s) (display (format "    ~a\n" s))) syms)
        (display "  --------------------------------------------------------\n")
        (display "\n"))))

;;; module-exports : Symbol [Symbol] → Void
;;; Show top-level definitions exported by a module.
;;; Scans source for define, define-syntax, and define-record-type forms.
;;; Optional mode argument:
;;;   'all     — include macro-generated runtime symbols
;;;   'runtime — show only runtime symbols (requires module to be loaded)
;;; Default shows source-level exports with a count of runtime symbols.
(define (module-exports name . mode-arg)
  (let ([mode (if (null? mode-arg) 'source (car mode-arg))]
        [path (module-name->path name)])
    (if (not path)
        (display (format "  Module '~a' not found.\n" name))
        (if (eq? mode 'runtime)
            ;; Runtime-only mode: just list symbols from oblist
            (module-exports-runtime name)
            (let ([port (open-input-file path)])
              (display (format "\n  Exports of '~a' (~a):\n" name path))
              (display "  --------------------------------------------------------\n")
              (let loop ([count 0] [has-macros #f] [source-names '()])
                (let ([form (guard (e [#t #!eof]) (read port))])
                  (cond
                    [(eof-object? form)
                     (close-input-port port)
                     ;; Show macro-generated symbols when mode is 'all
                     (let ([extra-count 0])
                       (when (and (eq? mode 'all) has-macros (module-loaded? name))
                         (let ([extra (module-runtime-extras name source-names)])
                           (unless (null? extra)
                             (display "  --- macro-generated ---\n")
                             (for-each (lambda (s)
                                         (display (format "    ~a\n" s)))
                                       extra)
                             (set! extra-count (length extra)))))
                       (display "  --------------------------------------------------------\n")
                       (display (format "  ~a source-level exports\n" count))
                       (when (> extra-count 0)
                         (display (format "  ~a macro-generated exports\n" extra-count))))
                     ;; Show dependencies
                     (let ([deps (hashtable-ref *module-deps* name '())])
                       (unless (null? deps)
                         (display (format "  Dependencies: ~a\n" deps))))
                     ;; Runtime symbol count hint (source mode only)
                     (when (and (eq? mode 'source) (module-loaded? name))
                       (let* ([prefix (string-append (symbol->string name) "-")]
                              [prefix-len (string-length prefix)]
                              [runtime-syms
                               (filter (lambda (s)
                                         (let ([str (symbol->string s)])
                                           (and (>= (string-length str) prefix-len)
                                                (string=? (substring str 0 prefix-len) prefix)
                                                (top-level-bound? s))))
                                       (oblist))])
                         (when (> (length runtime-syms) count)
                           (display (format "  + ~a macro-generated runtime symbols (use 'all to see all ~a* bindings)\n"
                                            (- (length runtime-syms) count) name)))))
                     (when (and (eq? mode 'source) has-macros (not (module-loaded? name)))
                       (display "  Note: load module first to see macro-generated exports\n"))
                     (display "\n")]
                ;; (define (name args ...) body)
                [(and (pair? form) (eq? (car form) 'define)
                      (pair? (cdr form)) (pair? (cadr form)))
                 (let ([fn-name (caadr form)]
                       [args (cdadr form)])
                   (display (format "    (~a ~a)\n" fn-name
                                    (let fmt ([a args])
                                      (cond
                                        [(null? a) ""]
                                        [(symbol? a) (format ". ~a" a)]
                                        [(pair? a) (string-append
                                                    (symbol->string (car a))
                                                    (if (or (null? (cdr a)) (symbol? (cdr a)))
                                                        (fmt (cdr a))
                                                        (string-append " " (fmt (cdr a)))))]
                                        [else ""])))))
                 (loop (+ count 1) has-macros (cons (caadr form) source-names))]
                ;; (define name value)
                [(and (pair? form) (eq? (car form) 'define)
                      (pair? (cdr form)) (symbol? (cadr form)))
                 (display (format "    ~a\n" (cadr form)))
                 (loop (+ count 1) has-macros (cons (cadr form) source-names))]
                ;; (define-syntax name ...)
                [(and (pair? form) (eq? (car form) 'define-syntax)
                      (pair? (cdr form)) (symbol? (cadr form)))
                 (display (format "    ~a [syntax]\n" (cadr form)))
                 (loop (+ count 1) has-macros (cons (cadr form) source-names))]
                ;; (define-record-type name ...)
                [(and (pair? form) (eq? (car form) 'define-record-type)
                      (pair? (cdr form)) (symbol? (cadr form)))
                 (display (format "    ~a [record]\n" (cadr form)))
                 (loop (+ count 1) has-macros (cons (cadr form) source-names))]
                ;; Detect macro invocations that likely generate defines
                ;; Pattern: bare (symbol args...) at top level where symbol
                ;; contains "generate" or starts with known macro prefixes
                [(and (pair? form) (symbol? (car form))
                      (let ([s (symbol->string (car form))])
                        (or (and (>= (string-length s) 8)
                                 (string=? (substring s 0 8) "generate"))
                            (and (>= (string-length s) 7)
                                 (string=? (substring s 0 7) "define-")
                                 (not (memq (car form)
                                            '(define-syntax define-record-type)))))))
                 (loop count #t source-names)]
                ;; Skip other forms
                [else (loop count has-macros source-names)]))))))))

;;; list-registered-modules : Unit → (List Symbol)
;;; Return a list of all registered module names.
(define (list-registered-modules)
  (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
        (vector->list (hashtable-keys *module-paths*))))

;;; module-collisions : Unit → Void
;;; List all module names that have multiple files (collision candidates).
;;; Useful for auditing the codebase and knowing when to use namespaced requires.
;;;
;;; Usage: (module-collisions) to see all colliding names
;;;        Use namespaced form (require 'dir/module) to avoid ambiguity
(define (module-collisions)
  (display "\n")
  (display "  --------------- MODULE COLLISIONS ---------------\n")
  (display "\n")
  (display "  Known collisions (use namespaced form to disambiguate):\n\n")

  ;; Check known collision-prone names
  (let ([collision-count 0]
        [names-to-check '("types" "state" "effects" "polynomial" "parser"
                          "distributions" "dsl" "integrators" "optimize"
                          "span" "stability" "units" "numeric-instances")])
       (for-each
        (lambda (name)
                (let ([paths (find-all-module-paths name)])
                     (when (> (length paths) 1)
                           (set! collision-count (+ collision-count 1))
                           (display (format "  ~a (~a files):~%" name (length paths)))
                           (for-each
                            (lambda (p)
                                    (display (format "    → (require '~a)~%" (path->namespace p))))
                            paths)
                           (display "\n"))))
        names-to-check)

       (if (= collision-count 0)
           (display "  No collisions found.\n\n")
           (display (format "  Total: ~a module names with collisions~%~%" collision-count)))))

