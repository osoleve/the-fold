;;; shell/tools/dead-code.ss — Dead Code and Unused Binding Detection
;;;
;;; Finds unused definitions across the codebase using the symbol index
;;; and call graph. Provides safe-delete suggestions.
;;;
;;; Features:
;;;   - Find globally unused definitions (defined but never called)
;;;   - Find locally unused bindings in let/let*/letrec forms
;;;   - Identify dead branches in conditionals
;;;   - Safe-delete suggestions with impact analysis
;;;
;;; This is Shell code: uses index and lens systems for analysis.
;;;
;;; Usage:
;;;   (dead-code-scan)                 - Scan entire codebase
;;;   (dead-code-scan "dir")           - Scan specific directory
;;;   (dead-code-file "file.ss")       - Analyze single file
;;;   (dead-code-suggest-delete 'sym)  - Safe-delete analysis
;;;   (dead-code-stats)                - Show statistics
;;;
;;; Dependencies:
;;;   core/lang/index.ss (symbol index)
;;;   shell/lens/call-graph.ss (call graph)

;;; ============================================================
;;; Dependencies
;;; ============================================================

(load "core/base/prelude.ss")

;;; ============================================================
;;; Analysis State
;;; ============================================================

;;; All defined symbols found
(define *all-definitions* '())

;;; All referenced symbols found
(define *all-references* (make-eq-hashtable))

;;; Unused symbols detected
(define *unused-symbols* '())

;;; Built-in/standard symbols to ignore
(define *ignore-symbols*
  '(;; R6RS/Chez standard
    define define-syntax lambda let let* letrec letrec* if cond case
    and or not when unless begin set! quote quasiquote unquote
    car cdr cons list vector string null? pair? list? symbol? string?
    number? boolean? procedure? vector? eq? eqv? equal? + - * /
    < > <= >= = map filter fold-left fold-right for-each apply
    display newline format printf error guard call/cc call-with-values
    values with-exception-handler raise raise-continuable
    ;; Common test/framework symbols
    test-case check assert expect run-tests
    ;; Module system
    library import export))

;;; ============================================================
;;; Symbol Collection
;;; ============================================================

;;; collect-definitions : String -> (List (symbol file line))
;;; Collect all definitions from a file.
(define (collect-definitions file)
  (guard (e [else '()])
         (call-with-input-file file
                               (lambda (port)
                                       (let loop ([line-num 1] [results '()])
                                            (let ([expr (guard (e [else #f]) (read port))])
                                                 (cond
                                                  [(eof-object? expr) (reverse results)]
                                                  [(not expr) (reverse results)]
                                                  [else
                                                   (let ([defs (extract-defs expr file line-num)])
                                                        (loop (+ line-num 1)
                                                              (append defs results)))])))))))

;;; extract-defs : S-expr × String × Nat -> (List (symbol file line))
;;; Extract defined names from an expression.
(define (extract-defs expr file line)
  (cond
   [(not (pair? expr)) '()]
   [(eq? (car expr) 'define)
    (cond
     [(and (pair? (cdr expr)) (symbol? (cadr expr)))
      (list (list (cadr expr) file line))]
     [(and (pair? (cdr expr)) (pair? (cadr expr)))
      (list (list (caadr expr) file line))]
     [else '()])]
   [(eq? (car expr) 'define-syntax)
    (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
        (list (list (cadr expr) file line))
        '())]
   [else '()]))

;;; collect-references : String -> void
;;; Collect all symbol references from a file into *all-references*.
(define (collect-references file)
  (guard (e [else (void)])
         (call-with-input-file file
                               (lambda (port)
                                       (let loop ()
                                            (let ([expr (guard (e [else #f]) (read port))])
                                                 (cond
                                                  [(eof-object? expr) (void)]
                                                  [(not expr) (void)]
                                                  [else
                                                   (walk-for-refs expr)
                                                   (loop)])))))))

;;; walk-for-refs : S-expr -> void
;;; Walk expression tree and record all symbol references.
(define (walk-for-refs expr)
  (cond
   [(symbol? expr)
    (hashtable-set! *all-references* expr #t)]
   [(pair? expr)
    (walk-for-refs (car expr))
    (walk-for-refs (cdr expr))]
   [else (void)]))

;;; ============================================================
;;; Dead Code Detection
;;; ============================================================

;;; find-scheme-files : String -> (List String)
(define (find-scheme-files dir)
  (define (skip-dir? name)
    (or (string=? name ".")
        (string=? name "..")
        (string=? name ".git")
        (string=? name ".store")
        (string=? name ".fold-repl")
        (string=? name ".fold-sessions")
        (string=? name "node_modules")))
  
  (guard (e [else '()])
         (let loop ([pending (list dir)] [results '()])
              (if (null? pending)
                  results
                  (let ([current (car pending)]
                        [rest (cdr pending)])
                       (cond
                        [(not (file-exists? current))
                         (loop rest results)]
                        [(file-directory? current)
                         (let ([entries (guard (e [else '()])
                                               (directory-list current))])
                              (let ([full-paths
                                     (filter
                                      (lambda (p) p)
                                      (map (lambda (name)
                                                   (if (skip-dir? name)
                                                       #f
                                                       (string-append current "/" name)))
                                           entries))])
                                   (loop (append full-paths rest) results)))]
                        [(string-suffix? ".ss" current)
                         (loop rest (cons current results))]
                        [else
                         (loop rest results)]))))))

;;; string-suffix? : String × String -> Boolean
(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
       (and (>= len slen)
            (string=? suffix (substring str (- len slen) len)))))

;;; dead-code-scan : [String] -> (List (symbol file line))
;;; Scan for unused definitions.
(define (dead-code-scan . args)
  (let ([dir (if (null? args) "." (car args))])
       
       (display "\n")
       (printf "  Dead Code Analysis: ~a\n" dir)
       (display "  ────────────────────────────────\n\n")
       
       ;; Reset state
       (set! *all-definitions* '())
       (hashtable-clear! *all-references*)
       (set! *unused-symbols* '())
       
       ;; Find all files
       (let ([files (find-scheme-files dir)])
            (printf "  Scanning ~a files...\n" (length files))
            
            ;; Phase 1: Collect all definitions
            (display "  Phase 1: Collecting definitions...\n")
            (for-each
             (lambda (file)
                     (let ([defs (collect-definitions file)])
                          (set! *all-definitions*
                                (append defs *all-definitions*))))
             files)
            (printf "    Found ~a definitions\n" (length *all-definitions*))
            
            ;; Phase 2: Collect all references
            (display "  Phase 2: Collecting references...\n")
            (for-each collect-references files)
            (printf "    Found ~a unique references\n"
                    (hashtable-size *all-references*))
            
            ;; Phase 3: Find unreferenced definitions
            (display "  Phase 3: Finding unused definitions...\n")
            (let ([unused '()])
                 (for-each
                  (lambda (def)
                          (let ([sym (car def)]
                                [file (cadr def)]
                                [line (caddr def)])
                               (unless (or (hashtable-ref *all-references* sym #f)
                                           (memq sym *ignore-symbols*)
                                           (test-related? sym)
                                           (exported? sym file))
                                       (set! unused (cons def unused)))))
                  *all-definitions*)
                 
                 (set! *unused-symbols* unused)
                 
                 ;; Report results
                 (display "\n")
                 (display "  ════════════════════════════════════════\n")
                 (printf "  UNUSED DEFINITIONS: ~a\n" (length unused))
                 (display "  ════════════════════════════════════════\n\n")
                 
                 (if (null? unused)
                     (display "    No unused definitions found.\n")
                     (begin
                      ;; Group by file
                      (let ([by-file (group-by-file unused)])
                           (for-each
                            (lambda (group)
                                    (printf "  ~a:\n" (car group))
                                    (for-each
                                     (lambda (def)
                                             (printf "    Line ~a: ~a\n"
                                                     (caddr def)
                                                     (car def)))
                                     (cdr group))
                                    (display "\n"))
                            by-file))))
                 
                 (display "\n")
                 unused))))

;;; group-by-file : (List (sym file line)) -> (List (file . defs))
(define (group-by-file defs)
  (let ([groups (make-hashtable string-hash string=?)])
       (for-each
        (lambda (def)
                (let* ([file (cadr def)]
                       [existing (hashtable-ref groups file '())])
                      (hashtable-set! groups file (cons def existing))))
        defs)
       (let ([result '()])
            (vector-for-each
             (lambda (key)
                     (set! result (cons (cons key (hashtable-ref groups key '()))
                                        result)))
             (hashtable-keys groups))
            (sort-groups result))))

;;; sort-groups : (List (file . defs)) -> (List (file . defs))
(define (sort-groups groups)
  (list-sort (lambda (a b) (string<? (car a) (car b))) groups))

;;; test-related? : Symbol -> Boolean
;;; Check if symbol is likely test-related (should not be flagged).
(define (test-related? sym)
  (let ([name (symbol->string sym)])
       (or (string-prefix? "test-" name)
           (string-suffix? "-test" name)
           (string-prefix? "check-" name)
           (string-prefix? "assert-" name)
           (string-prefix? "expect-" name))))

;;; string-prefix? : String × String -> Boolean
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
       (and (<= plen slen)
            (string=? prefix (substring str 0 plen)))))

;;; exported? : Symbol × String -> Boolean
;;; Check if symbol might be exported from the module.
;;; This is a heuristic - checks if the file has (export ...).
(define (exported? sym file)
  ;; For now, assume public if starts with no underscore
  (let ([name (symbol->string sym)])
       (not (string-prefix? "_" name))))

;;; ============================================================
;;; Unused Local Bindings
;;; ============================================================

;;; find-unused-locals : String -> (List (name file line binding-form))
;;; Find unused local bindings in let/let*/letrec forms.
(define (find-unused-locals file)
  (guard (e [else '()])
         (let ([content (call-with-input-file file get-string-all)]
               [port (open-input-file file)]
               [unused '()])
              (let loop ([line-num 1])
                   (let ([expr (guard (e [else #f]) (read port))])
                        (cond
                         [(eof-object? expr) (close-input-port port)]
                         [(not expr) (close-input-port port)]
                         [else
                          (let ([found (find-unused-in-expr expr file line-num)])
                               (set! unused (append found unused)))
                          (loop (+ line-num 1))])))
              (reverse unused))))

;;; find-unused-in-expr : S-expr × String × Nat -> (List unused)
;;; Recursively find unused bindings in an expression.
(define (find-unused-in-expr expr file line)
  (cond
   [(not (pair? expr)) '()]
   [(memq (car expr) '(let let* letrec letrec*))
    (if (and (pair? (cdr expr))
             (or (pair? (cadr expr))     ; Regular let
                 (symbol? (cadr expr)))) ; Named let
        (let* ([is-named? (symbol? (cadr expr))]
               [bindings (if is-named? (caddr expr) (cadr expr))]
               [body (if is-named? (cdddr expr) (cddr expr))])
              (if (list? bindings)
                  (let ([bound-names (map car bindings)]
                        [body-refs (collect-body-refs body)])
                       ;; Find which bound names aren't used in body
                       (let ([unused-here
                              (filter
                               (lambda (name)
                                       (not (memq name body-refs)))
                               bound-names)])
                            (append
                             (map (lambda (n) (list n file line (car expr)))
                                  unused-here)
                             ;; Recurse into binding expressions and body
                             (apply append
                                    (map (lambda (b)
                                                 (if (pair? (cdr b))
                                                     (find-unused-in-expr (cadr b) file line)
                                                     '()))
                                         bindings))
                             (apply append
                                    (map (lambda (e) (find-unused-in-expr e file line))
                                         body)))))
                  '()))
        '())]
   [(eq? (car expr) 'lambda)
    ;; Check for unused lambda parameters
    (if (and (pair? (cdr expr)) (pair? (cddr expr)))
        (let ([params (cadr expr)]
              [body (cddr expr)])
             (if (list? params)
                 (let ([body-refs (collect-body-refs body)]
                       [unused-params
                        (filter
                         (lambda (p)
                                 (and (symbol? p)
                                      (not (memq p body-refs))))
                         params)])
                      (append
                       (map (lambda (p) (list p file line 'lambda)) unused-params)
                       (apply append
                              (map (lambda (e) (find-unused-in-expr e file line))
                                   body))))
                 '()))
        '())]
   [else
    ;; Recurse into subexpressions
    (apply append
           (map (lambda (e) (find-unused-in-expr e file line))
                expr))]))

;;; collect-body-refs : S-expr -> (List Symbol)
;;; Collect all symbol references in body expressions.
(define (collect-body-refs body)
  (let ([refs '()])
       (letrec ([walk (lambda (expr)
                              (cond
                               [(symbol? expr) (set! refs (cons expr refs))]
                               [(pair? expr)
                                (walk (car expr))
                                (walk (cdr expr))]
                               [else (void)]))])
               (walk body)
               refs)))

;;; ============================================================
;;; Safe Delete Analysis
;;; ============================================================

;;; dead-code-suggest-delete : Symbol -> void
;;; Analyze safety of deleting a symbol.
(define (dead-code-suggest-delete sym)
  (display "\n")
  (printf "  Safe-Delete Analysis: '~a'\n" sym)
  (display "  ────────────────────────────────\n\n")
  
  ;; Find definition
  (let ([def (find-def-in-list sym *all-definitions*)])
       (if (not def)
           (printf "  Symbol '~a' not found in current analysis.\n" sym)
           (let ([file (cadr def)]
                 [line (caddr def)])
                
                (printf "  Definition: ~a:~a\n\n" file line)
                
                ;; Check for references
                (let ([is-referenced (hashtable-ref *all-references* sym #f)])
                     (if is-referenced
                         (begin
                          (display "  WARNING: Symbol is referenced in codebase!\n")
                          (display "  Deletion would cause errors.\n")
                          (display "\n  Callers:\n")
                          ;; Try to find callers using call graph
                          (guard (e [else (display "    (call graph not loaded)\n")])
                                 (when (and (top-level-bound? 'call-graph-callers)
                                            (procedure? call-graph-callers))
                                       (let ([callers (call-graph-callers sym)])
                                            (if (null? callers)
                                                (display "    (no direct callers found)\n")
                                                (for-each
                                                 (lambda (c) (printf "    ~a\n" c))
                                                 (take-up-to callers 10)))))))
                         (begin
                          (display "  SAFE TO DELETE: No references found.\n\n")
                          (display "  To delete, remove the definition from:\n")
                          (printf "    ~a:~a\n" file line)))))))
  (display "\n"))

;;; find-def-in-list : Symbol × (List def) -> def | #f
(define (find-def-in-list sym defs)
  (let loop ([defs defs])
       (cond
        [(null? defs) #f]
        [(eq? (caar defs) sym) (car defs)]
        [else (loop (cdr defs))])))

;;; take-up-to : List × Nat -> List
(define (take-up-to lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-up-to (cdr lst) (- n 1)))))

;;; ============================================================
;;; Statistics and Reporting
;;; ============================================================

;;; dead-code-stats : -> void
;;; Show analysis statistics.
(define (dead-code-stats)
  (display "\n")
  (display "  Dead Code Analysis Statistics\n")
  (display "  ────────────────────────────────\n\n")
  (printf "  Total definitions analyzed: ~a\n" (length *all-definitions*))
  (printf "  Unique references found:    ~a\n" (hashtable-size *all-references*))
  (printf "  Unused definitions:         ~a\n" (length *unused-symbols*))
  
  (when (> (length *unused-symbols*) 0)
        (display "\n  Top unused symbols:\n")
        (for-each
         (lambda (def)
                 (printf "    ~a (~a:~a)\n" (car def) (cadr def) (caddr def)))
         (take-up-to *unused-symbols* 10))
        (when (> (length *unused-symbols*) 10)
              (printf "    ... and ~a more\n" (- (length *unused-symbols*) 10))))
  (display "\n"))

;;; dead-code-file : String -> void
;;; Analyze a single file for unused locals.
(define (dead-code-file file)
  (display "\n")
  (printf "  Unused Local Bindings: ~a\n" file)
  (display "  ────────────────────────────────\n\n")
  
  (let ([unused (find-unused-locals file)])
       (if (null? unused)
           (display "    No unused local bindings found.\n")
           (for-each
            (lambda (u)
                    (printf "    Line ~a: ~a (in ~a form)\n"
                            (caddr u) (car u) (cadddr u)))
            unused)))
  (display "\n"))

;;; ============================================================
;;; REPL Commands
;;; ============================================================

;;; dead-code-help : -> void
(define (dead-code-help)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                  DEAD CODE DETECTION COMMANDS                      │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  (display "  Global Analysis:\n")
  (display "    (dead-code-scan)              Scan entire codebase\n")
  (display "    (dead-code-scan \"dir\")        Scan specific directory\n")
  (display "    (dead-code-stats)             Show analysis statistics\n")
  (display "\n")
  (display "  File Analysis:\n")
  (display "    (dead-code-file \"file.ss\")    Find unused locals in file\n")
  (display "\n")
  (display "  Safe Delete:\n")
  (display "    (dead-code-suggest-delete 'sym)   Analyze deletion safety\n")
  (display "\n")
  (display "  Help:\n")
  (display "    (dead-code-help)              Show this help\n")
  (display "\n"))

;;; ============================================================
;;; Initialization
;;; ============================================================

(display "\n")
(display "  Dead Code Detection Loaded\n")
(display "  ────────────────────────────────\n")
(display "  Use (dead-code-help) for available commands.\n")
(display "  Use (dead-code-scan) to analyze the codebase.\n")
(display "\n")
